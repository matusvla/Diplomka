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
      integer, parameter :: metisncon = 1      
! -- constants for Graphviz file
      character(len=*), parameter :: graphvizfilename = "GVgraph.txt"  
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

!--------------------------------------------------------------------
!
! program start
!
! -- various initializations
!            
! -- TODO load command line arguments, at the moment hardcoded:
!	  
     parts = 2
     call getCmdlineArgs(matrixpath, matrixtype, nfull, TESTswitch, testGraphNumber)

    !  TODO delete
    !  parts = 2
    !  TESTswitch = .true.
    !  matrixtype = 'T' !possible values: T ... Test, P ... Poisson, RSA ... from file     
    !  matrixpath = "./matrices/bcsstk01.rsa"
    !  testGraphNumber = 1
    !  nfull = 5


!
! -- matrix loading
!    TODO improve matrix loading, now it's just generating matrix using poisson1
!      
      info = 0
  !loading of the matrix in RB format      
      select case (TRIM(matrixtype))
        case ('RSA')
          open(unit=infileunit, file=matrixpath, action='read', iostat=statio)        
          if(statio .ne. 0) then
            stop 'specified matrix file cannot be opened.'
          end if   
          !allocate ia, ja, aa
          call imhb2(infileunit, m, n, mformat, ia, ja, aa, info) ! read a matrix in a RB format
          allocate(wn01(n+1), wn02(n+1), stat=ierr)
          
          !TODO konzultace, symtr7 nedela to, co bych cekal
          !write(*,*) ia
          !write(*,*) ja
          call symtr7(n, ia, ja, aa, wn01, wn02)
          deallocate(wn01, wn02, stat=ierr)
          !write(*,*) "symtr"
          !write(*,*) ia
          !write(*,*) ja
          !end konzultace          
          
        case ('P')
          !allocate ia, ja, aa
          call poisson1(nfull, n, ia, ja, aa, info)
          mformat = 11  

        case ('T')
            write(*,*) "Running in test mode"
            call loadTestGraph(ia, ja, aa, n, testGraphNumber)

        case default
          stop 'Unrecognised matrix format!'
      end select   

      write(*,*) matrixpath
      
!
! -- calling Graph partitioner METIS embeded into program
!    TODO miscelaneous error handling    
!     
    
      call remloops(n, ia, ja, iaNoLoops, jaNoLoops, ierr, aa, aaNoLoops)
      allocate(part(n), stat=ierr)      
      metis_call_status=METIS_SetDefaultOptions(metisoptions)
      call shiftnumbering(-1, n, iaNoLoops, jaNoLoops)  ! transform graph into C++ notation (starting from 0)    
            
      metis_call_status=METIS_ComputeVertexSeparator(n, iaNoLoops, jaNoLoops, C_NULL_PTR, metisoptions, sepsize, part)

      call shiftnumbering(1, n, iaNoLoops, jaNoLoops, part)  ! transform graph back into Fortran notation (starting from 1)
      if (sepsize == 0) then
        write(*,*) "Graph created from matrix has more components and it is well partitioned by default."
        stop 
      end if
      write(*,*) "sepsize",sepsize
!
! -- Create subgraphs
!
      call createSubgraphs(ia, ja, aa, n, part, parts, iap, jap, aap, np, nvs, perm, invperm, ierr)
!
! -- Find best ordering of vertices   
!            
      
      call orderByMD(ia, ja, n, ordperm, invordperm, ierr)
      ! call orderByDistance(ia, ja, n, part, parts, ordperm, invordperm, ierr)  
      ! call orderMixed(ia, ja, n, part, parts, ordperm, invordperm, ierr) 
      ! call orderCoefMixed(ia, ja, n, part, parts, ordperm, invordperm, REAL(0.5,8)/100, ierr)

      call partOrdering(ordperm, invordperm, ordpermp, invordpermp, n, np, part, parts, ierr)

      do i = 1, parts
        call applyOrdering(iap%vectors(i)%elements, jap%vectors(i)%elements, np(i), &
          nvs%vectors(i)%elements, ordpermp%vectors(i)%elements, &
          invordpermp%vectors(i)%elements, ierr)
      end do

      allocate(cholFill(parts), stat=ierr)
      do i = 1, parts
        allocate(parent(np(i)), ancstr(np(i)), colcnt(np(i)), marker(np(i) + 1), stat=ierr)
        call eltree2(np(i), iap%vectors(i)%elements, jap%vectors(i)%elements, parent, ancstr)
        call colcnts(np(i), iap%vectors(i)%elements, jap%vectors(i)%elements, colcnt, parent, marker)      
      !   allocate(parent(n), ancstr(n), colcnt(n), marker(n + 1), stat=ierr)
      !   call eltree2(n, ia, ja, parent, ancstr)
      !   call colcnts(n, ia, ja, colcnt, parent, marker)
        cholFill(i) = SUM(colcnt)
        deallocate(parent, ancstr, colcnt, marker)
      end do
      write(*,*) matrixpath(1:30), cholFill
      deallocate(cholFill)
      
      ! do i = 1, parts + 1
      !   ! call orderByDistance(iap%vectors(i)%elements, jap%vectors(i)%elements, np(i), &
      !   !   logical2intArr(nvs%vectors(i)%elements) + 1, 1, & 
      !   !   ordpermp%vectors(i)%elements, invordpermp%vectors(i)%elements, ierr)  
      !   call orderByMD(iap%vectors(i)%elements, jap%vectors(i)%elements, np(i), &
      !      ordpermp%vectors(i)%elements, invordpermp%vectors(i)%elements, ierr)   
      !   write(*,*) invordpermp%vectors(i)%elements
      ! end do


 
      !TODO deallocate deallocate(ordperm,invordperm)   

      !    write(*,'(50I3)') ordperm
      !    deallocate(ordperm,invordperm)

      ! call orderMixed(iap%vectors(1)%elements, jap%vectors(1)%elements, np(1), &
      !    logical2intArr(nvs%vectors(1)%elements) + 1, 1, ordperm, invordperm, ierr)      

      !call orderMixed(ia, ja, n, [1,1,1,2], 1, ordperm, invordperm, ierr)      
      
      ! do m = -5, 105
      !   call orderCoefMixed(iap%vectors(1)%elements, jap%vectors(1)%elements, np(1), &
      !     logical2intArr(nvs%vectors(1)%elements) + 1, 1, ordperm, invordperm, REAL(m,8)/100, ierr)       
      
      !   if(TESTswitch) then        
      !     call testUniqueness(ordperm)
      !     call testUniqueness(invordperm)
      !   end if
      !   deallocate(ordperm,invordperm)
      ! end do
!
! -- Write out partitioned graph in Graphviz format
!    TODO miscelaneous error handling          
!      
      open(unit=graphvizunit, file=graphvizfilename)                  
      call  gvColorGraph (ia, ja, n, part, graphvizunit, ierr)  
      ! call  gvSetLabels (ia, ja, n, ordperm, graphvizunit, ierr)  
      ! call  gvSimpleGraph (iap%vectors(1)%elements, jap%vectors(1)%elements, np(1), &
      !  graphvizunit, ierr)  
      close(graphvizunit)  
      
      ! open(unit=15, file="GVgraph1.txt")   
      ! call graphvizcr(iap%vectors(1)%elements, jap%vectors(1)%elements, np(1), part, 15, ierr, .true., .false., .false.,)
      ! close(15)  
      
!      
! -- write out matlab format for displaying this matrix
!      
      ! aa = 1
      ! call ommatl4(n, ia, ja, aa, mformat)

      deallocate(aa)
      k = 1
      allocate(aa(iap%vectors(k)%elements(np(k)+1)-1))
      aa = 1
      call ommatl4(np(k), iap%vectors(k)%elements, jap%vectors(k)%elements, aa, 0)


!      allocate(colcnt(nfull), stat=ierr)
!      call chfill2(nfull, ia, ja, mformat, colcnt, chsize, info)


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



      end if
!
! -- deallocate all allocated fields
!
      deallocate(ia, stat=ierr)
      deallocate(ja, stat=ierr)
      deallocate(aa, stat=ierr)
      deallocate(part, stat=ierr)	  
      call subgraphCleanup(iap, jap, aap, np, nvs, perm, invperm, parts, ierr)
!
!--------------------------------------------------------------------          
!
! program end
!      
      end program 


