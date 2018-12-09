! program main
! (c) Vladislav Matus
! last edit: 16. 11. 2018
      
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
! -- number of parts in partitionF
      integer :: parts    
! -- dimension of the matrix
      integer :: n   
! -- matrix in CSR format      
      integer, allocatable, dimension(:) :: ia, ja
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
      logical :: orderEveryMove ! when should the ordering be called 
      logical :: writesProgress

! -- miscelaneous 
      integer :: i, j
      integer :: ierr, metisierr ! error codes      

!--------------------------------------------------------------------
!
! program start
!
! -- Command line arguments parsing
!	  
      parts = 2
      call getCmdlineArgs(matrixpath, matrixtype, nfull, testMatrixNumber, & 
        orderingType, mixedCoef, vertSepMoves, hasGvOutput, gvFilename, writesProgress, &
        orderEveryMove)
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
        if (i == vertSepMoves .or. orderEveryMove) then
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
        if (i < vertSepMoves) then
          if(writesProgress) write(*,*) "Cleaning up subgraphs..." 
          call subgraphCleanup(iap, jap, np, nvs, perm, invperm, parts, ierr)
        end if
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
!
! -- Final steps
!
      write(*,*) "Final size of separator: ", sepsize
      write(*,*) "Final sizes of parts: ", np(1:parts)
      write(*,*) "Final count of nonzeros in L: ", cholFill
      if(writesProgress) write(*,*) "Cleaning up subgraphs..." 
      call subgraphCleanup(iap, jap, np, nvs, perm, invperm, parts, ierr)
      deallocate(ia, ja, part, cholFill, stat=ierr)

!--------------------------------------------------------------------          
!
! program end
!      
      end program