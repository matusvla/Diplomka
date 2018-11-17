! module matrixloader
! (c) Vladislav Matus
! last edit: 16. 11. 2018     
      module matrixloader
        use mydepend
        use mydepend90
        use cmdlineloader, only: CMDARG_MAXLEN
        use testing
        implicit none
	      integer, parameter :: infileunit = 4	  
      contains
!--------------------------------------------------------------------           
! subroutine loadMatrix
! (c) Vladislav Matus
! last edit: 16. 11. 2018
!
! Purpose:
!   load appropriate matrix to ia, ja, n
! Input:
!   matrixtype ... type of matrix as loaded from command line  
!   matrixpath ... path to matrix for matrixtype 'RSA'
!   nfull ... one dimension of matrix for matrixtype 'P'
!   testGraphNumber ... index of test graph for matrixtype 'T'
! Output:
!   ia, ja ... matrix in CSR format
!   n ... size of this matrix
! Allocations: ia, ja            

      subroutine loadMatrix(ia, ja, n, matrixtype, matrixpath, nfull, testGraphNumber)
        implicit none
!
! parameters
!
      integer :: n, nfull, testGraphNumber
      integer, allocatable, dimension(:) :: ia, ja
      character*(CMDARG_MAXLEN) :: matrixtype
      character*(CMDARG_MAXLEN) :: matrixpath
!
! internals
!        
      integer :: info = 0, statio, m, mformat = 0, ierr
      integer, allocatable, dimension(:) :: wn01, wn02 !auxiliary vectors
!
! start of loadMatrix
!      
      select case (TRIM(matrixtype))
        case ('RSA')
          open(unit=infileunit, file=matrixpath, action='read', iostat=statio)        
          if(statio .ne. 0) then
            write(*,*) 'ERROR: Specified matrix file cannot be opened.'
            stop
          end if   
          ! read a matrix in a RB format
          call imhb3(infileunit, m, n, mformat, ia, ja, info) 
          allocate(wn01(n+1), wn02(n+1), stat=ierr)
          call symtr6(n, ia, ja, wn01, wn02)
          deallocate(wn01, wn02, stat=ierr)
          
        case ('P')
          call poisson1(nfull, n, ia, ja, info)

        case ('T')
            write(*,*) "Running in test mode"
            call loadTestGraph(ia, ja, n, testGraphNumber)

        case default
          write(*,*) "[matrixloader.90:loadMatrix] ERROR: Unrecognised matrix type!"
          stop
      end select   
!
! end of loadMatrix
!  
      end subroutine loadMatrix    
      
!--------------------------------------------------------------------          
      end module matrixloader