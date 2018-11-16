! program main
! (c) Vladislav Matus
! last edit: 16. 11. 2018
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
        use cmdlineloader
        use matrixloader  
        use metiscaller
        implicit none

!--------------------------------------------------------------------
!        
! constants  
!     
! -- constants for Graphviz file
      integer, parameter :: GRAPHVIZ_UNIT = 2      
      character(len=*), parameter :: GRAPHVIZ_FILENAME = "GVgraph"  

!--------------------------------------------------------------------      
!      
! work variables
!
! -- number of parts in partition
      integer :: parts    
! -- dimension of the matrix
      integer :: n   
! -- matrix in CSR format      
      integer, allocatable, dimension(:) :: ia, ja
      double precision, allocatable, dimension(:) :: aa !TODO decide if keeping
! -- obtained partitioning (vertex separator is denoted by (parts +1)) and its size
      integer, allocatable, dimension(:) :: part     
      integer :: sepsize
! -- multidimensional ragged arrays corresponding to partitions 
!    containing matrices in CSR format
      type(intRaggedArr) :: iap, jap
      type(dpRaggedArr) :: aap
      type(logicalRaggedArr) :: nvs
      ! Corresponding permutations from original to partitioned and back
      integer, allocatable, dimension(:) :: np, perm
      type(intRaggedArr) :: invperm      
! -- permutations from original to ordered matrix and back
      integer, allocatable, dimension(:) :: ordperm, invordperm
      type(intRaggedArr) :: ordpermp, invordpermp
! -- description of elimination tree
      integer, allocatable, dimension(:) :: parent, ancstr, colcnt, marker
! -- fill in Cholesky factor of matrix
      integer, allocatable, dimension(:) :: cholFill
! -- command line arguments
      character*(CMDARG_MAXLEN) :: matrixtype ! 'RSA'/'P'/'T'
      character*(CMDARG_MAXLEN) :: matrixpath ! path to matrix if matrixtype == 'RSA'
      character*(ORDERINGTYPE_MAXLEN) :: orderingType ! how should the matrix be ordered
      double precision :: mixedCoef ! coeficient for mixed ordering
      integer :: vertSepMoves ! how many times the vertex separator should be moved
      integer :: testMatrixNumber ! index of test matrix if matrixtype == 'T'
      integer :: nfull ! one dimension of poisson matrix if matrixtype == 'P'
! -- miscelaneous 
      integer :: i, j
      integer :: k !TODO decide if delete
      integer :: ierr, metisierr ! error codes      
      integer :: chsize ! size of the fill      
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
! -- Command line arguments parsing
!	  
     parts = 2
     call getCmdlineArgs(matrixpath, matrixtype, nfull, TESTswitch, testMatrixNumber, orderingType, mixedCoef, vertSepMoves)
!
! -- Matrix loading
!      
      call loadMatrix(ia, ja, n, matrixtype, matrixpath, nfull, testMatrixNumber)
!
! -- Calling Graph partitioner METIS 
!     
      call metisCall(ia, ja, n, part, sepsize, metisierr)
!
! -- The main loop
!
      do i = 0, vertSepMoves
        ! -- Create subgraphs
        call createSubgraphs(ia, ja, n, part, parts, iap, jap, np, nvs, perm, invperm, ierr)


        ! TODO delete
        open(unit=GRAPHVIZ_UNIT, file=GRAPHVIZ_FILENAME)                  
        call  gvColorGraph (ia, ja, n, part, GRAPHVIZ_UNIT, ierr)
        close(GRAPHVIZ_UNIT)   
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
              write(*,*) "Ordering graph using mixed ordering with coeficients."
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
        ! -- Deallocate all fields allocated by createSubgraphs
        call subgraphCleanup(iap, jap, np, nvs, perm, invperm, parts, ierr)
        ! -- If there was no vertex separator after initial partion
        if (metisierr == METIS_NO_SEP) then
          exit
        end if
        ! -- Move vertex separator to balace nonzeros in Cholesky factor
        if (i < vertSepMoves) then
          call moveVertSep(ia, ja, n, part, parts, MAXLOC(cholFill,1), sepsize)
        end if
        deallocate(cholFill)
      end do
!
! -- Write out partitioned graph in Graphviz format        
!      
      open(unit=GRAPHVIZ_UNIT, file=GRAPHVIZ_FILENAME)                  
      call  gvColorGraph (ia, ja, n, part, GRAPHVIZ_UNIT, ierr)  
      ! call  gvSetLabels (ia, ja, n, ordperm, GRAPHVIZ_UNIT, ierr)  
      ! call  gvSimpleGraph (iap%vectors(1)%elements, jap%vectors(1)%elements, np(1), &
      !  GRAPHVIZ_UNIT, ierr)  
      close(GRAPHVIZ_UNIT)         
!      
! -- write out matlab format for displaying this matrix
!      
      ! call ommatl4(n, ia, ja, aa, mformat)
      ! k = 1
      ! allocate(aa(iap%vectors(k)%elements(np(k)+1)-1))
      ! aa = 1
      ! call ommatl4(np(k), iap%vectors(k)%elements, jap%vectors(k)%elements, aa, 0)
      ! deallocate(aa)

!
! -- Final deallocations
!
      deallocate(ia, ja, part, ordperm, invordperm, stat=ierr)

  
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
!--------------------------------------------------------------------          
!
! program end
!      
      end program