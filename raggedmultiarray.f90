! module raggedmultiarray
! (c) Vladislav Matus
! last edit: 04. 07. 2018      

      module raggedmultiarray
        implicit none
!--------------------------------------------------------------------         
!
! multidimensional "ragged" array construction
! new types: intraggedarr, dpraggedarr
! TODO is there a way how to not repeat myself? i.e. templates or something similar?
! source: https://stackoverflow.com/questions/18316592/
!         multidimensional-array-with-different-lengths (13.03.2018)
!
      type :: intvect
        integer, dimension(:), allocatable :: elements
      end type intvect      
      type :: intraggedarr
        type(intvect), dimension(:), allocatable :: vectors
      end type intraggedarr
      
      type :: dpvect
        double precision, dimension(:), allocatable :: elements
      end type dpvect      
      type :: dpraggedarr
        type(dpvect), dimension(:), allocatable :: vectors
      end type dpraggedarr
      
!--------------------------------------------------------------------
! end of module
!      
      end module raggedmultiarray      