! module cmdlineloader
! (c) Vladislav Matus
! last edit: 09. 11. 2018      

      module cmdlineloader
        implicit none        
        integer, parameter :: CMDARG_MAXLEN = 100 !length of string matrixpath         
      contains

!--------------------------------------------------------------------  
! subroutine getCmdlineArgs
! (c) Vladislav Matus
! last edit: 09. 11. 2018  
!
! Purpose: 
!   Load command line arguments       
!   
! Input: none
!   
! Output:
!   matrixpath
!   matrixtype
!   nfull
!   
! Allocations: none
!

      subroutine getCmdlineArgs(matrixpath, matrixtype, nfull, testSwitch, testGraphNumber)
        implicit none
!
! parameters
!
      logical :: testSwitch
      integer :: nfull, stat, testGraphNumber
      character*(CMDARG_MAXLEN) :: value
!
! internals
!      
      integer :: i, n        
      character*(CMDARG_MAXLEN) :: matrixpath, matrixtype
!
! start of getCmdlineArgs
!	      
      !Set defaults
      matrixtype = "RSA"
      nfull = 5
      testSwitch = .false.
      testGraphNumber = 1
      !Loop over the arguments
      n = command_argument_count()
      if (n > 0) then
        i = 1
        do while (i <= n)
          call get_command_argument(i, value)
          select case(TRIM(ADJUSTL(value)))
            case("-h","-help")
              write(*,*) "Vladislav Matus's diploma thesis, 2018"
              write(*,*) "  -o [path] path to matrix file which should be opened. works only with RSA matrix type"
              write(*,*) "  -mt [matrixtype] for choosing type of matrix. Allowed types: RSA, P[number], T"
              write(*,*) "  -t for running development tests"
              write(*,*) "  -h for help"
              stop
            case("-o")
              call get_command_argument(i + 1, value)
              matrixpath = TRIM(ADJUSTL(value))
              i = i + 1
            case("-mt")
              call get_command_argument(i + 1, value)
              matrixtype = TRIM(ADJUSTL(value))
              if(matrixtype(1:1) == "P") then
                matrixpath = matrixtype
                read(matrixtype(2:),*,iostat=stat) nfull
                if ( stat /= 0 .or. nfull < 1) then
                  write(*,*) 'Invalid or unspecified number for matrix type "P"' 
                  stop
                endif
                matrixtype = "P"
              else if(matrixtype(1:1) == "T") then
                matrixpath = matrixtype
                read(matrixtype(2:),*,iostat=stat) testGraphNumber
                if ( stat /= 0 .or. testGraphNumber < 1) then
                  write(*,*) 'Invalid or unspecified number for matrix type "T"' 
                  stop
                endif
                matrixtype = "T"
                testSwitch = .true.
              end if
              i = i + 1
            case("-t")
              testSwitch = .true.
            case default
              write(*,*) 'Invalid command line argument "', TRIM(ADJUSTL(value)), '" encountered, use -h for help'
              stop
          end select
          i = i + 1
        end do
      end if
!
! end of getCmdlineArgs
!  
      end subroutine getCmdlineArgs
!--------------------------------------------------------------------    
    end module cmdlineloader
