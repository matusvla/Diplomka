! module raggedmultiarray
! (c) Vladislav Matus
! last edit: 22. 09. 2018      

      module raggedmultiarray
        implicit none
!--------------------------------------------------------------------         
!
! multidimensional "ragged" array construction
! new types: intRaggedArr, dpRaggedArr
! TODO is there a way how to not repeat myself? i.e. templates or something similar?
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
! end of module
!      
      end module raggedmultiarray      
