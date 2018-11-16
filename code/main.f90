! program main
! (c) Vladislav Matus
! last edit: 22. 09. 2018
! TODO: file manipulation error handling
! TODO: check all ierr  
      
      program main
      use mydepend
      use mydepend90
      use myroutines90          
      use gvroutines 
      use auxroutines 
      use testing
      use raggedmultiarray
      use metis_interface
      use cmdlineLoader
      
      implicit none

!--------------------------------------------------------------------
! 
! constants
      
!     
! -- unit numbers for file handling            
      integer, parameter :: metunit = 1  
      integer, parameter :: graphvizunit = 2      
      integer, parameter :: metpartunit = 3
	integer, parameter :: infileunit = 4	  
! -- constants for calling METIS
      character(len=*), parameter :: metpath = "./METIS/metis.exe"
      character(len=*), parameter :: metfilename = "metgraph"               
      character(len=*), parameter :: metarguments = ">nul 2>&1"
      
      integer, parameter :: METIS_OPTION_NUMBERING = 17
      integer, parameter :: METIS_OK = 1
      integer, parameter :: metisncon = 1      
! -- constants for Graphviz file
      character(len=*), parameter :: graphvizfilename = "GVgraph"  
! -- miscelaneous      
      character(len=*), parameter :: sp = " " ! alias for space    
      integer, parameter :: partsch_max_len = 100 !length of string partsch

!--------------------------------------------------------------------      
!      
! work variables
!
! -- number of parts in partition
      integer :: parts          
! -- dimension of the matrix
      integer :: n
! -- number of edges in corresponding graph
!      integer :: ne      
! -- matrix in CSR format      
      integer, allocatable, dimension(:) :: ia, ja
      double precision, allocatable, dimension(:) :: aa
! -- obtained partitioning, vertex separator is denoted by (parts +1)
      integer, allocatable, dimension(:) :: part     
! -- multidimensional ragged arrays corresponding to partitions 
!    containing matrices in CSR format
      type(intRaggedArr) :: iap, jap
      type(dpRaggedArr) :: aap
      type(logicalRaggedArr) :: nvs
      ! Corresponding permutations from original to partitioned and back
      integer, allocatable, dimension(:) :: np, perm
      type(intRaggedArr) :: invperm      
! -- permutations from original to ordered matrix and back, describes position in new matrix, old matrix
      integer, allocatable, dimension(:) :: ordperm, invordperm
      type(intRaggedArr) :: ordpermp, invordpermp
! -- description of elimination tree
      integer, allocatable, dimension(:) :: parent, ancstr, colcnt, marker
! -- fill in Cholesky factor of matrix
      integer, allocatable, dimension(:) :: cholFill
! -- command line arguments
      character*(CMDARG_MAXLEN) :: matrixtype
      character*(CMDARG_MAXLEN) :: matrixpath
      character*(ORDERINGTYPE_MAXLEN) :: orderingType
      double precision :: mixedCoef
      integer :: vsMoves
! -- work variables for calling METIS  
      integer, allocatable, dimension(:) :: iaNoLoops, jaNoLoops
      double precision, allocatable, dimension(:) :: aaNoLoops
      integer, dimension(0:40) :: metisoptions 
      integer :: metisobjval    
      integer :: metis_call_status       
      integer :: sepsize
! -- miscelaneous 
      integer :: nfull ! one dimension of matrix, "nfull = sqrt(n)"
      integer :: i, j, k, m 
      integer :: ierr, info, statio      
      integer :: chsize ! size of the fill      
      integer, allocatable, dimension(:) :: wn01, wn02 !auxiliary vectors
      ! -- conversions of numbers to strings
      integer :: ndigits      
      !number of parts as string, use partsch(1:ndigits)
      character*(partsch_max_len) :: partsch       
      integer :: mformat ! matrix format for loading             
      integer :: testGraphNumber ! which matrix should be loaded in test mode
! -- for testing purposes only
      logical :: TESTswitch
      integer, allocatable, dimension(:) :: TESTordperm1, TESTordperm2
      integer, allocatable, dimension(:) :: TESTinvordperm1, TESTinvordperm2
      integer, allocatable, dimension(:) :: TESTia, TESTja, TESTpart
      logical, allocatable, dimension(:) :: TESTnvs
      character(len=99) :: chari

!--------------------------------------------------------------------
!
! program start
!
! -- various initializations
!            
! -- TODO load command line arguments, at the moment hardcoded:
!	  
     parts = 2
     call getCmdlineArgs(matrixpath, matrixtype, nfull, TESTswitch, testGraphNumber, orderingType, mixedCoef, vsMoves)

!
! -- matrix loading
!    TODO externalizovat
!      
      info = 0
  !loading of the matrix in RB format      
      select case (TRIM(matrixtype))
        case ('RSA')
          open(unit=infileunit, file=matrixpath, action='read', iostat=statio)        
          if(statio .ne. 0) then
            stop 'specified matrix file cannot be opened.'
          end if   
          !allocate ia, ja
          call imhb3(infileunit, m, n, mformat, ia, ja, info) ! read a matrix in a RB format
          allocate(wn01(n+1), wn02(n+1), stat=ierr)

          call symtr6(n, ia, ja, wn01, wn02)
          deallocate(wn01, wn02, stat=ierr)
          
        case ('P')
          !allocate ia, ja
          call poisson1(nfull, n, ia, ja, info)
          mformat = 11  

        case ('T')
            write(*,*) "Running in test mode"
            call loadTestGraph(ia, ja, n, testGraphNumber)

        case default
          stop 'Unrecognised matrix format!'
      end select   

      write(*,*) "----------------------------------------------------------------"
      write(*,*) TRIM(ADJUSTL(matrixpath)), ": "
      
!
! -- Calling Graph partitioner METIS embeded into program 
!     
      ! -- Preparation of fields for METIS
      call remloops(n, ia, ja, iaNoLoops, jaNoLoops, ierr)
      allocate(part(n), stat=ierr)      
      metis_call_status=METIS_SetDefaultOptions(metisoptions)
      ! -- Transform graph into C++ notation (starting from 0)    
      call shiftnumbering(-1, n, iaNoLoops, jaNoLoops)  
      ! -- Call METIS           
      metis_call_status = METIS_ComputeVertexSeparator(n, iaNoLoops, jaNoLoops, C_NULL_PTR, metisoptions, sepsize, part)
      if(metis_call_status /= METIS_OK) then
        write(*,*) "ERROR: METIS graph partitioner failed!"
        stop
      end if
      deallocate(iaNoLoops, jaNoLoops)
      ! -- Transform graph partition into Fortran notation (starting from 1)
      part = part + 1
      ! -- Check for nonempty separator
      if (sepsize == 0) then
        write(*,*) "Graph created from matrix has more components and it is well partitioned by default."
        stop 
      end if
      write(*,*) "Separator size:", sepsize

!
! -- The main program
!
      do i = 0, vsMoves
        ! -- Create subgraphs
        call createSubgraphs(ia, ja, n, part, parts, iap, jap, np, nvs, perm, invperm, ierr)


        ! TODO delete
        open(unit=graphvizunit, file=graphvizfilename)                  
        call  gvColorGraph (ia, ja, n, part, graphvizunit, ierr)
        close(graphvizunit)   
        write(unit=chari,fmt=*) i
        write(*,*) "dot -Tpdf GVgraph -o ./temp/"//TRIM(ADJUSTL(chari))//".pdf"
        CALL EXECUTE_COMMAND_LINE("dot -Tpdf GVgraph -o './temp/"//TRIM(ADJUSTL(chari))//".pdf'")

 
        ! -- Find ordering of vertices of the original graph              
        if(TRIM(ADJUSTL(orderingType)) /= 'no') then
          select case(TRIM(ADJUSTL(orderingType)))
            case ('MD')
              call orderByMD(ia, ja, n, ordperm, invordperm, ierr)
              write(*,*) "Ordering graph using MD ordering."
            case ('DIST')
              call orderByDistance(ia, ja, n, part, parts, ordperm, invordperm, ierr)  
              write(*,*) "Ordering graph by distance from separator."
            case ('MIX')
              call orderMixed(ia, ja, n, part, parts, ordperm, invordperm, ierr) 
              write(*,*) "Ordering graph using mixed ordering"
            case default
              call orderCoefMixed(ia, ja, n, part, parts, ordperm, invordperm, mixedCoef, ierr)
              write(*,*) "Ordering graph by mixed ordering with coeficients."
            end select
          ! -- Apply ordering
          call partOrdering(ordperm, invordperm, ordpermp, invordpermp, n, np, part, parts, ierr)
          do j = 1, parts
            call applyOrdering(iap%vectors(j)%elements, jap%vectors(j)%elements, np(j), &
              nvs%vectors(j)%elements, ordpermp%vectors(j)%elements, &
              invordpermp%vectors(j)%elements, ierr)
          end do
          call deallocRaggedArr(ordpermp, parts + 1, ierr)
          call deallocRaggedArr(invordpermp, parts + 1, ierr)
        end if

        ! -- Count nonzeros in Cholesky factor
        allocate(cholFill(parts), stat=ierr)
        do j = 1, parts
          allocate(parent(np(j)), ancstr(np(j)), colcnt(np(j)), marker(np(j) + 1), stat=ierr)
          call eltree2(np(j), iap%vectors(j)%elements, jap%vectors(j)%elements, parent, ancstr)
          call colcnts(np(j), iap%vectors(j)%elements, jap%vectors(j)%elements, colcnt, parent, marker)
          cholFill(j) = SUM(colcnt)
          deallocate(parent, ancstr, colcnt, marker)
        end do
        write(*,*) "Nonzeros in L", cholFill

        ! -- Move vertex separator to balace nonzeros in Cholesky factor
        if (i < vsMoves) then
          call moveVertSep(ia, ja, n, part, parts, MAXLOC(cholFill,1), sepsize)
        end if
        ! -- Deallocate all fields
        deallocate(cholFill)
        call subgraphCleanup(iap, jap, np, nvs, perm, invperm, parts, ierr)
      end do

!
! -- Write out partitioned graph in Graphviz format        
!      
      open(unit=graphvizunit, file=graphvizfilename)                  
      call  gvColorGraph (ia, ja, n, part, graphvizunit, ierr)  
      ! call  gvSetLabels (ia, ja, n, ordperm, graphvizunit, ierr)  
      ! call  gvSimpleGraph (iap%vectors(1)%elements, jap%vectors(1)%elements, np(1), &
      !  graphvizunit, ierr)  
      close(graphvizunit)   
      
!      
! -- write out matlab format for displaying this matrix
!      
      ! call ommatl4(n, ia, ja, aa, mformat)
      k = 1
      allocate(aa(iap%vectors(k)%elements(np(k)+1)-1))
      aa = 1
      call ommatl4(np(k), iap%vectors(k)%elements, jap%vectors(k)%elements, aa, 0)
      deallocate(aa)

!
! -- final tests in test mode
!            
      if(TESTswitch) then
        write(*,*) "------TEST RESULTS: --------------------------------------------------------------"
        call orderByDistance(iap%vectors(1)%elements, jap%vectors(1)%elements, np(1), &
           logical2intArr(nvs%vectors(1)%elements) + 1, 1, TESTordperm1, TESTinvordperm1, ierr)
        call orderCoefMixed(iap%vectors(1)%elements, jap%vectors(1)%elements, np(1), &
           logical2intArr(nvs%vectors(1)%elements) + 1, 1, TESTordperm2, TESTinvordperm2, REAL(1,8), ierr)
        if(ALL(TESTordperm1 == TESTordperm2)) then 
          write(*,*) "TEST 1: OK"
        else 
          write(*,*) "TEST 1: failed!"
        end if
        if(ALL(TESTinvordperm1 == TESTinvordperm2)) then 
          write(*,*) "TEST 2: OK"
        else 
          write(*,*) "TEST 2: failed!"
        end if   
        deallocate(TESTordperm1, TESTinvordperm1, TESTordperm2, TESTinvordperm2)     

        call orderByMD(iap%vectors(1)%elements, jap%vectors(1)%elements, np(1), &
        TESTordperm1, TESTinvordperm1, ierr)
        call orderCoefMixed(iap%vectors(1)%elements, jap%vectors(1)%elements, np(1), &
        logical2intArr(nvs%vectors(1)%elements) + 1, 1, TESTordperm2, TESTinvordperm2, REAL(0,8), ierr)    
        if(ALL(TESTordperm1 == TESTordperm2)) then 
          write(*,*) "TEST 3: OK"
        else 
          write(*,*) "TEST 3: failed!"
        end if
        if(ALL(TESTinvordperm1 == TESTinvordperm2)) then 
          write(*,*) "TEST 4: OK"
        else 
          write(*,*) "TEST 4: failed!"
        end if
        deallocate(TESTordperm2, TESTinvordperm2)     
        
        allocate(TESTia(n + 1), TESTja(ia(n + 1) - 1), TESTpart(n), stat=ierr)
        TESTia = ia
        TESTja = ja
        allocate(TESTnvs(n),stat=ierr)
        TESTnvs = .true.
        TESTpart = part
        call applyOrdering(TESTia, TESTja, n, TESTnvs, TESTordperm1, TESTinvordperm1, ierr, TESTpart)
        call applyOrdering(TESTia, TESTja, n, TESTnvs, TESTinvordperm1, TESTordperm1, ierr, TESTpart)
        if(ALL(TESTia == ia) .and. ALL(TESTja == ja) .and. ALL(TESTpart == part) .and. ALL(TESTpart == part)) then 
          write(*,*) "TEST 5: OK"
        else 
          write(*,*) "TEST 5: failed!"
        end if
        deallocate(TESTia, TESTja, TESTpart, TESTnvs)

        TESTswitch = .true.
        do i = 1, n
          if (TESTinvordperm1(TESTordperm1(i)) /= i) then
            TESTswitch = .false.
          end if
        end do
        if(TESTswitch) then 
          write(*,*) "TEST 6: OK"
        else 
          write(*,*) "TEST 6: failed!"
        end if

        TESTswitch = .true.
        do i = 1, parts
          do j = 1, np(i)
            if (invordpermp%vectors(i)%elements(ordpermp%vectors(i)%elements(j)) /= j) then
              TESTswitch = .false.
            end if
          end do    
        end do
        if(TESTswitch) then 
          write(*,*) "TEST 7: OK"
        else 
          write(*,*) "TEST 7: failed!"
        end if
        deallocate(TESTordperm1, TESTinvordperm1)

        j = 0
        do i = 1, parts + 1
          j = j + np(i)
        end do
        if(j == n) then 
          write(*,*) "TEST 8: OK"
        else 
          write(*,*) "TEST 8: failed!"
        end if
      
        ! TODO test separator moving



      end if
!
! -- deallocate all allocated fields
!
      !TODO check deallocations
      deallocate(ia, ja, part, ordperm, invordperm, stat=ierr)
      call subgraphCleanup(iap, jap, np, nvs, perm, invperm, parts, ierr)
!
!--------------------------------------------------------------------          
!
! program end
!      
      end program