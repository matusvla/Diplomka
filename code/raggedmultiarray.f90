! module raggedmultiarray
! (c) Vladislav Matus
! last edit: 16. 11. 2018      

      module raggedmultiarray
        implicit none
!--------------------------------------------------------------------         
!
! multidimensional "ragged" array construction
! new types: intRaggedArr, dpRaggedArr
! source: https://stackoverflow.com/questions/18316592/
!         multidimensional-array-with-different-lengths (13.03.2018)
!
      type :: intVect
        integer, dimension(:), allocatable :: elements
      end type intVect      
      type :: intRaggedArr
        type(intVect), dimension(:), allocatable :: vectors
      end type intRaggedArr
      
      type :: dpVect
        double precision, dimension(:), allocatable :: elements
      end type dpVect      
      type :: dpRaggedArr
        type(dpVect), dimension(:), allocatable :: vectors
      end type dpRaggedArr

      type :: logicalVect
        logical, dimension(:), allocatable :: elements
      end type logicalVect      
      type :: logicalRaggedArr
        type(logicalVect), dimension(:), allocatable :: vectors
      end type logicalRaggedArr
      
!--------------------------------------------------------------------
      contains   
!--------------------------------------------------------------------
! subroutine deallocRaggedArr
! (c) Vladislav Matus
! last edit: 16. 11. 2018
!
! Purpose:
!   deallocate everything allocated when creating intRaggedArr
! Input:
!   ra ... ragged array to deallocate     
!   dim ... first dimension of ra
! Output:
!   ierr ... error code, nonzero if not succcessful
! Allocations: none             

      subroutine deallocRaggedArr(ra, dim, ierr)
        implicit none
!
! parameters
!
      type(intRaggedArr) :: ra
      integer :: dim, ierr
!
! internals
!        
        integer :: i, iierr
!
! start of deallocRaggedArr
!      
        iierr = 0
        do i = 1, dim
          deallocate(ra%vectors(i)%elements, stat = iierr)
          ierr = ierr + iierr
        end do
        deallocate(ra%vectors, stat = iierr)
        ierr = ierr + iierr
!
! end of deallocRaggedArr
!  
      end subroutine deallocRaggedArr    

!
! end of module
!      
      end module raggedmultiarray      
