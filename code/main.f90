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
      type(logicalRaggedArr) :: nvs
      ! Corresponding permutations from original to partitioned and back
      integer, allocatable, dimension(:) :: np, perm
      type(intRaggedArr) :: invperm      
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
      logical :: hasGvOutput ! Should a Grapgviz file be created
      character*(CMDARG_MAXLEN) :: gvFilename ! name of output file
      logical :: writesProgress

! -- miscelaneous 
      integer :: i, j
      integer :: k !TODO decide if delete
      integer :: ierr, metisierr ! error codes      
      integer :: chsize ! size of the fill      
! -- for testing purposes only
      logical :: TESTswitch
      integer :: TESTno = 1
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
      call getCmdlineArgs(matrixpath, matrixtype, nfull, TESTswitch, testMatrixNumber, & 
        orderingType, mixedCoef, vertSepMoves, hasGvOutput, gvFilename, writesProgress)
      write(*,*) "Processing ", TRIM(ADJUSTL(matrixpath)), ":"
!
! -- Matrix loading
!     
      if(writesProgress) write(*,*) "Loading matrix from file..." 
      call loadMatrix(ia, ja, n, matrixtype, matrixpath, nfull, testMatrixNumber)  
!
! -- Calling Graph partitioner METIS 
!  
      if(writesProgress) write(*,*) "Running METIS graph partitioner..." 
      call metisCall(ia, ja, n, part, sepsize, metisierr)
!
! -- The main loop
!
      allocate(cholFill(parts), stat=ierr)
      do i = 0, vertSepMoves
        if(writesProgress) write(*,*) "Separator size:", sepsize
        ! -- Create subgraphs
        if(writesProgress) write(*,*) "Creating subgraphs from the original graph..." 
        call createSubgraphs(ia, ja, n, part, parts, iap, jap, np, nvs, perm, invperm, ierr)
        ! -- Find ordering of vertices of the original graph
        if (i == vertSepMoves) then
          call orderSubgraphs(orderingType, mixedCoef, ia, ja, n, part, parts, &
            iap, jap, np, nvs, writesProgress)
        end if
        ! -- Count nonzeros in Cholesky factor
        if(writesProgress) write(*,*) "Counting nonzeros in Cholesky factor..." 
        do j = 1, parts
          allocate(parent(np(j)), ancstr(np(j)), colcnt(np(j)), marker(np(j) + 1), stat=ierr)
          call eltree2(np(j), iap%vectors(j)%elements, jap%vectors(j)%elements, parent, ancstr)
          call colcnts(np(j), iap%vectors(j)%elements, jap%vectors(j)%elements, colcnt, parent, marker)
          cholFill(j) = SUM(colcnt)
          deallocate(parent, ancstr, colcnt, marker)
        end do
        if(writesProgress) write(*,*) "Nonzeros in L: ", cholFill
        ! -- Deallocate all fields allocated by createSubgraphs
        if(writesProgress) write(*,*) "Cleaning up subgraphs..." 
        call subgraphCleanup(iap, jap, np, nvs, perm, invperm, parts, ierr)
        ! -- If there was no vertex separator after initial partion
        if (metisierr == METIS_NO_SEP) exit
        ! -- Move vertex separator to balace nonzeros in Cholesky factor
        if (i < vertSepMoves) then
          if(writesProgress) write(*,*) "Moving vertex separator..."
          call moveVertSep(ia, ja, n, part, parts, MAXLOC(cholFill,1), sepsize)
        end if
      end do

!
! -- Write out partitioned graph in Graphviz format        
!      

      if(hasGvOutput) then
        if(writesProgress) write(*,*) "Outputting Graphviz file..."
        open(unit=GRAPHVIZ_UNIT, file=gvFilename)                  
        call gvColorGraph (ia, ja, n, part, GRAPHVIZ_UNIT, ierr)
        close(GRAPHVIZ_UNIT)
      end if   

      ! CALL EXECUTE_COMMAND_LINE("dot -Tpdf GVgraph -o './temp/"//TRIM(ADJUSTL(chari))//".pdf'")
      ! open(unit=GRAPHVIZ_UNIT, file=gvFilename)                  
      ! call  gvColorGraph (ia, ja, n, part, GRAPHVIZ_UNIT, ierr)  
      ! ! call  gvSetLabels (ia, ja, n, ordperm, GRAPHVIZ_UNIT, ierr)  
      ! ! call  gvSimpleGraph (iap%vectors(1)%elements, jap%vectors(1)%elements, np(1), &
      ! !  GRAPHVIZ_UNIT, ierr)  
      ! close(GRAPHVIZ_UNIT)         
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
! -- Final steps
!
      write(*,*) "Final size of separator: ", sepsize
      write(*,*) "Final count of nonzeros in L: ", cholFill
     ! deallocate(ia, ja, part, ordperm, invordperm, cholFill, stat=ierr) TODO uncomment

!
! -- final tests in test mode
!            
      if(TESTswitch) then
        write(*,*) "------TEST RESULTS: --------------------------------------------------------------"
        call createSubgraphs(ia, ja, n, part, parts, iap, jap, np, nvs, perm, invperm, ierr)
        
        if(SUM(np) == n) then
          write(*,'(A, I2, A)') "TEST ",TESTno,": OK"
        else
          write(*,'(A, I2, A)') "TEST ",TESTno,": failed!"
        end if
        TESTno = TESTno + 1

        TESTswitch = .true.
        do i = 1, parts + 1
          TESTswitch = TESTswitch .and. COUNT(part == i) == np(i)
        end do
        if(TESTswitch) then 
          write(*,'(A, I2, A)') "TEST ",TESTno,": OK"
        else 
          write(*,'(A, I2, A)') "TEST ",TESTno,": failed!"
        end if
        TESTno = TESTno + 1

        TESTswitch = .true.
        do i = 1, parts + 1
          k = 1
          do j = 1, np(i) + 1
            TESTswitch = TESTswitch .and. (iap%vectors(i)%elements(j) >= k)
            k = iap%vectors(i)%elements(j)
          end do
        end do
        if(TESTswitch) then 
          write(*,'(A, I2, A)') "TEST ",TESTno,": OK"
        else 
          write(*,'(A, I2, A)') "TEST ",TESTno,": failed!"
        end if
        TESTno = TESTno + 1

        TESTswitch = .true.
        do i = 1, parts + 1
          do j = 1, iap%vectors(i)%elements(np(i) + 1) - 1
            TESTswitch = TESTswitch .and. (jap%vectors(i)%elements(j) > 0)
            TESTswitch = TESTswitch .and. (jap%vectors(i)%elements(j) <= np(i))
          end do
        end do
        if(TESTswitch) then 
          write(*,'(A, I2, A)') "TEST ",TESTno,": OK"
        else 
          write(*,'(A, I2, A)') "TEST ",TESTno,": failed!"
        end if
        TESTno = TESTno + 1


        call orderByDistance(iap%vectors(1)%elements, jap%vectors(1)%elements, np(1), &
           logical2intArr(nvs%vectors(1)%elements) + 1, 1, TESTordperm1, TESTinvordperm1, ierr)
        call orderCoefMixed(iap%vectors(1)%elements, jap%vectors(1)%elements, np(1), &
           logical2intArr(nvs%vectors(1)%elements) + 1, 1, TESTordperm2, TESTinvordperm2, REAL(1,8), ierr)
        if(ALL(TESTordperm1 == TESTordperm2)) then 
          write(*,'(A, I2, A)') "TEST ",TESTno,": OK"
        else 
          write(*,'(A, I2, A)') "TEST ",TESTno,": failed!"
        end if
        TESTno = TESTno + 1

        if(ALL(TESTinvordperm1 == TESTinvordperm2)) then 
          write(*,'(A, I2, A)') "TEST ",TESTno,": OK"
        else 
          write(*,'(A, I2, A)') "TEST ",TESTno,": failed!"
        end if
        TESTno = TESTno + 1   
        deallocate(TESTordperm1, TESTinvordperm1, TESTordperm2, TESTinvordperm2)     

        call orderByMD(iap%vectors(1)%elements, jap%vectors(1)%elements, np(1), &
        TESTordperm1, TESTinvordperm1, ierr)
        call orderCoefMixed(iap%vectors(1)%elements, jap%vectors(1)%elements, np(1), &
        logical2intArr(nvs%vectors(1)%elements) + 1, 1, TESTordperm2, TESTinvordperm2, REAL(0,8), ierr)    
        if(ALL(TESTordperm1 == TESTordperm2)) then 
          write(*,'(A, I2, A)') "TEST ",TESTno,": OK"
        else 
          write(*,'(A, I2, A)') "TEST ",TESTno,": failed!"
        end if
        TESTno = TESTno + 1
        if(ALL(TESTinvordperm1 == TESTinvordperm2)) then 
          write(*,'(A, I2, A)') "TEST ",TESTno,": OK"
        else 
          write(*,'(A, I2, A)') "TEST ",TESTno,": failed!"
        end if
        TESTno = TESTno + 1
        deallocate(TESTordperm1, TESTinvordperm1, TESTordperm2, TESTinvordperm2)     
        
        allocate(TESTia(n + 1), TESTja(ia(n + 1) - 1), TESTpart(n), stat=ierr)
        TESTia = ia
        TESTja = ja
        allocate(TESTnvs(n),stat=ierr)
        TESTnvs = .true.
        TESTpart = part
        call orderByMD(ia, ja, n, TESTordperm1, TESTinvordperm1, ierr)
        call applyOrdering(TESTia, TESTja, n, TESTnvs, TESTordperm1, TESTinvordperm1, ierr, TESTpart)
        call applyOrdering(TESTia, TESTja, n, TESTnvs, TESTinvordperm1, TESTordperm1, ierr, TESTpart)
        if(ALL(TESTia == ia) .and. ALL(TESTja == ja) .and. ALL(TESTpart == part) .and. ALL(TESTpart == part)) then 
          write(*,'(A, I2, A)') "TEST ",TESTno,": OK"
        else 
          write(*,'(A, I2, A)') "TEST ",TESTno,": failed!"
        end if
        TESTno = TESTno + 1
        deallocate(TESTia, TESTja, TESTpart, TESTnvs)

        TESTswitch = .true.
        do i = 1, n
          if (TESTinvordperm1(TESTordperm1(i)) /= i) then
            TESTswitch = .false.
          end if
        end do
        if(TESTswitch) then 
          write(*,'(A, I2, A)') "TEST ",TESTno,": OK"
        else 
          write(*,'(A, I2, A)') "TEST ",TESTno,": failed!"
        end if
        TESTno = TESTno + 1

        TESTswitch = .true.
        do i = 1, n
          if (TESTordperm1(TESTinvordperm1(i)) /= i) then
            TESTswitch = .false.
          end if
        end do
        if(TESTswitch) then 
          write(*,'(A, I2, A)') "TEST ",TESTno,": OK"
        else 
          write(*,'(A, I2, A)') "TEST ",TESTno,": failed!"
        end if
        TESTno = TESTno + 1

        deallocate(TESTordperm1, TESTinvordperm1)

        j = 0
        do i = 1, parts + 1
          j = j + np(i)
        end do
        if(j == n) then 
          write(*,'(A, I2, A)') "TEST ",TESTno,": OK"
        else 
          write(*,'(A, I2, A)') "TEST ",TESTno,": failed!"
        end if
        TESTno = TESTno + 1
      
        ! TODO test separator moving



      end if
!--------------------------------------------------------------------          
!
! program end
!      
      end program