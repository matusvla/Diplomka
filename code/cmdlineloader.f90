! module cmdlineloader
! (c) Vladislav Matus
! last edit: 11. 11. 2018      

      module cmdlineloader
        implicit none        
        integer, parameter :: CMDARG_MAXLEN = 100 
        integer, parameter :: ORDERINGTYPE_MAXLEN = 10
        integer, parameter :: MVS_MAX = 20
      contains

!--------------------------------------------------------------------  
! subroutine getCmdlineArgs
! (c) Vladislav Matus
! last edit: 16. 11. 2018  
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

      subroutine getCmdlineArgs(matrixpath, matrixtype, nfull, testSwitch, &
        testGraphNumber, orderingType, mixedCoef, vsMoves)
        implicit none
!
! parameters
!
      logical :: testSwitch
      integer :: nfull, stat, testGraphNumber, vsMoves
      character*(CMDARG_MAXLEN) :: value
      character*(CMDARG_MAXLEN) :: matrixpath, matrixtype
      character*(ORDERINGTYPE_MAXLEN) :: orderingType
      double precision :: mixedCoef
!
! internals
!      
      integer :: i, n    
      character(len=200) :: mvsch    
!
! start of getCmdlineArgs
!	      
      !Set defaults
      matrixpath = ""
      matrixtype = "RSA"
      nfull = 5
      testSwitch = .false.
      testGraphNumber = 1
      orderingType = "no"
      mixedCoef = 0.0
      vsMoves = 0
      !Loop over the arguments
      n = command_argument_count()
      if (n > 0) then
        i = 1
        do while (i <= n)
          call get_command_argument(i, value)
          select case(TRIM(ADJUSTL(value)))
            case("-h","-help")
              write(*,*) "Vladislav Matus's diploma thesis, 2018"
              write(*,*) "  -o [path] path to matrix file which should be processed. Works only with 'RSA' matrix type."
              write(*,*) "  -mt [matrixtype] for choosing type of matrix. Allowed types: 'RSA', 'P[number]', 'T[number]'."
              write(*,*) "  -ot [type] ordering type, default is no ordering. Allowed types 'MD', 'DIST', 'MIX', 'MIX[number]'."
              write(*,*) "  -mvs [number] how many times vertex separator should be moved to try to improve the partition."
              write(*,*) "  -t for running development tests."
              write(*,*) "  -h for help."
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
              end if
              i = i + 1
            case("-ot")
              call get_command_argument(i + 1, value)
              orderingType = TRIM(ADJUSTL(value))
              if ( orderingType /= "MD" .and. orderingType /= "DIST" .and. orderingType(1:3) /= "MIX") then
                write(*,*) "Warning: Invalid ordering type '", &
                  TRIM(ADJUSTL(orderingType)), "' was reset to no ordering!"
                orderingType = "no"
              else if (orderingType(1:3) == "MIX" .and. orderingType /= 'MIX') then 
                read(orderingType(4:),*,iostat=stat) mixedCoef
                if(stat /= 0) then
                  write(*,*) "Invalid ordering type '", TRIM(ADJUSTL(orderingType)), "' was reset to no ordering"
                  orderingType = "no"
                end if
              end if
              i = i + 1
            case("-mvs")
              call get_command_argument(i + 1, value)
              read(value, *, iostat=stat) vsMoves 
              if (stat /= 0 .or. vsMoves < 0 .or. vsMoves > MVS_MAX) then
                write(unit=mvsch,fmt=*) MVS_MAX
                write(*,*) "Error: Invalid value was provided for -mvs argument. Minimum is 0, maximum is "//TRIM(ADJUSTL(mvsch))
                stop
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
      else
        write(*,*) "ERROR: no matrix specified, use -h for help"
        stop
      end if
!
! end of getCmdlineArgs
!  
      end subroutine getCmdlineArgs
!--------------------------------------------------------------------    
    end module cmdlineloader
