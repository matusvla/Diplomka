! module metiscaller
! (c) Vladislav Matus
! last edit: 16. 11. 2018     
      module metiscaller
        use myroutines90, only: remloops, shiftnumbering
        use metis_interface

        implicit none
        private
        public :: metisCall, METIS_NO_SEP

        integer, parameter :: METIS_NO_SEP = -200
        integer, parameter :: METIS_OK = 1
        integer, parameter :: METIS_OPTION_NUMBERING = 17

      contains
!--------------------------------------------------------------------           
! subroutine metisCall
! (c) Vladislav Matus
! last edit: 16. 11. 2018
!
! Purpose:
!   Call METIS and handle exceptions
! Input:

! Output:

! Allocations: part        

      subroutine metisCall(ia, ja, n, part, sepsize, ierr)
        implicit none
!
! parameters
!
      integer :: n, ierr, sepsize
      integer, allocatable, dimension(:) :: ia, ja, part
!
! internals
!        
      integer :: metis_call_status, iierr
      integer, allocatable, dimension(:) :: iaNoLoops, jaNoLoops
      integer, dimension(0:40) :: metisoptions 
!
! start of metisCall
!      
      ierr = 0
      ! -- Preparation of fields for METIS
      call remloops(n, ia, ja, iaNoLoops, jaNoLoops, iierr)
      allocate(part(n), stat=iierr)      
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
        write(*,*) "[metiscaller.f90:metisCall] Warning: ", &
          "Graph created from matrix has more components and it is well partitioned by default."
        ierr = METIS_NO_SEP
      end if
!
! end of metisCall
!  
      end subroutine metisCall    
      
!--------------------------------------------------------------------          
      end module metiscaller