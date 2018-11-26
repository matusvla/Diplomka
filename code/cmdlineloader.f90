! module cmdlineloader
! (c) Vladislav Matus
! last edit: 11. 11. 2018      

      module cmdlineloader
        implicit none        
        integer, parameter :: CMDARG_MAXLEN = 200 
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
        testGraphNumber, orderingType, mixedCoef, vsMoves, hasGvOutput, outputFile, &
        writesProgress, orderEveryMove)
        implicit none
!
! parameters
!
      logical :: testSwitch, hasGvOutput, writesProgress, orderEveryMove
      integer :: nfull, stat, testGraphNumber, vsMoves
      character*(CMDARG_MAXLEN) :: argValue
      character*(CMDARG_MAXLEN) :: matrixpath, matrixtype, outputFile
      character*(ORDERINGTYPE_MAXLEN) :: orderingType
      double precision :: mixedCoef
!
! internals
!      
      logical :: validArguments = .false.
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
      hasGvOutput = .false.
      outputFile = ""
      writesProgress = .false.
      orderEveryMove = .false.
      !Loop over the arguments
      n = command_argument_count()
      if (n > 0) then
        i = 1
        do while (i <= n)
          call get_command_argument(i, argValue)
          select case(TRIM(ADJUSTL(argValue)))
            case("-h","-help")
              write(*,*) "Vladislav Matus's diploma thesis, 2018"
              write(*,*) "  -mt [matrixtype] for choosing input type of matrix. Allowed types: 'RSA', 'P[number]', 'T[number]'."
              write(*,*) "  -f [path] path to matrix file which should be processed. Required when using 'RSA' matrix type."
              write(*,*) "  -ot [type] ordering type, default is no ordering. Allowed types 'MD', 'DIST', 'MIX', 'MIX[number]'."
              write(*,*) "  -mvs [number] how many times vertex separator should be moved to try to improve the partition."
              write(*,*) "  -oe vertices are ordered in every iteration and not only in the last one."
              write(*,*) "  -o [path] if specified, graph in graphviz format will be written to file on path."
              write(*,*) "  -w program outputs additional information about its progress when running."
              write(*,*) "  -h for help."
              stop
            case("-f")
              validArguments = .true.
              call get_command_argument(i + 1, argValue)
              if (argValue(1:1) /= '-' .and. i < n) then
                matrixpath = TRIM(ADJUSTL(argValue))
              else
                write(*,*) "ERROR: Input file name not specified! Use -h for help." 
                stop
              end if
              i = i + 1
            case("-o")
              call get_command_argument(i + 1, argValue)
              hasGvOutput = .true.
              if (argValue(1:1) /= '-' .and. i < n) then
                outputFile = argValue
              else
                write(*,*) "ERROR: Output file name not specified! Use -h for help." 
                stop
              end if
              i = i + 1
            case("-mt")
              call get_command_argument(i + 1, argValue)
              matrixtype = TRIM(ADJUSTL(argValue))
              if(matrixtype(1:1) == "P") then
                validArguments = .true.
                matrixpath = matrixtype
                read(matrixtype(2:),*,iostat=stat) nfull
                if ( stat /= 0 .or. nfull < 1) then
                  write(*,*) "ERROR: Invalid or unspecified number for matrix type 'P'" 
                  stop
                endif
                matrixtype = "P"
              else if(matrixtype(1:1) == "T") then
                validArguments = .true.
                matrixpath = matrixtype
                read(matrixtype(2:),*,iostat=stat) testGraphNumber
                if ( stat /= 0 .or. testGraphNumber < 1) then
                  write(*,*) 'ERROR: Invalid or unspecified number for matrix type "T"' 
                  stop
                endif
                matrixtype = "T"
              else if(matrixtype /= "RSA") then
                if(argValue(1:1) == '-' .or. i >= n) then
                  write(*,*) "ERROR: No matrix type specified! Use -h for help."
                else
                  write(*,*) "ERROR: Unrecognised matrix type '",TRIM(ADJUSTL(matrixtype)),"'! Use -h for help."
                end if
                stop
              end if
              i = i + 1
            case("-ot")
              call get_command_argument(i + 1, argValue)
              orderingType = TRIM(ADJUSTL(argValue))
              if ( orderingType /= "MD" .and. orderingType /= "DIST" .and. orderingType(1:3) /= "MIX") then
                write(*,*) "WARNING: Invalid ordering type '", &
                  TRIM(ADJUSTL(orderingType)), "' was reset to no ordering!"
                orderingType = "no"
              else if (orderingType(1:3) == "MIX" .and. orderingType /= 'MIX') then 
                read(orderingType(4:),*,iostat=stat) mixedCoef
                if(stat /= 0) then
                  write(*,*) "WARNING: Invalid ordering type '", TRIM(ADJUSTL(orderingType)), &
                    "' was reset to no ordering"
                  orderingType = "no"
                end if
              end if
              i = i + 1
            case("-mvs")
              call get_command_argument(i + 1, argValue)
              read(argValue, *, iostat=stat) vsMoves 
              if (stat /= 0 .or. vsMoves < 0 .or. vsMoves > MVS_MAX) then
                write(unit=mvsch,fmt=*) MVS_MAX
                write(*,*) "ERROR: Invalid value was provided for -mvs argument. Minimum is 0, maximum is "//TRIM(ADJUSTL(mvsch))
                stop
              end if
              i = i + 1
            case("-w")
              writesProgress = .true.
            case("-t") ! TODO remove
              testSwitch = .true.
            case("-oe") ! TODO remove
              orderEveryMove = .true.
            case default
              write(*,*) "ERROR: Invalid command line argument '", TRIM(ADJUSTL(argValue)), &
                "' encountered, use -h for help"
              stop
          end select
          i = i + 1
        end do
      else
        write(*,*) "ERROR: No input matrix specified, use -h for help"
        stop
      end if
      if (.not. validArguments) then
        write(*,*) "ERROR: No input matrix specified, use -h for help"
        stop
      end if

!
! end of getCmdlineArgs
!  
      end subroutine getCmdlineArgs
!--------------------------------------------------------------------    
    end module cmdlineloader
