! module auxroutines
! (c) Vladislav Matus
! last edit: 20. 09. 2018      

      module auxroutines            
        implicit none  
      contains
!--------------------------------------------------------------------
      subroutine insertionSort(arr)
        implicit none
        integer, dimension(:) :: arr
        integer :: i, j, val, arrSize
        arrSize = SIZE(arr)
        do i = 1, arrSize
          j = i
          do while (j > 1 .and. arr(j) < arr(j-1))
            !swap elements
            write(*,*) "swapping", arr(j), arr(j-1)
            val = arr(j-1)
            arr(j-1) = arr(j)
            arr(j) = val
            j = j - 1
          end do
        end do
      end subroutine insertionSort
!--------------------------------------------------------------------   
      subroutine uniquify(arr, uniqueArr, ierr, omitElement)   
        implicit none       

        integer, dimension(:) :: arr
        integer, allocatable, dimension(:) :: uniqueArr
        integer, optional :: omitElement
        integer :: n, i, uI, uniqueL, ierr, firstIndex     
        logical :: omit, firstFound        

        if(present(omitElement))then
          omit = .true.
        else
          omit = .false.
        endif
        ierr = 0        

        n = SIZE(arr)                
        call insertionSort(arr)                  
        uniqueL = 0
        do i = 1, n
          if (arr(i) /= arr(i - 1)) then
            if(.not. (omit .and. omitElement==arr(i))) then
              uniqueL = uniqueL + 1            
            end if          
          end if
        end do                

        if(uniqueL < 1) then
          write(*,*) "[auxroutines.f90:56] Error in uniquify, nothing to return!"
          ierr = 1
          return
        end if

        allocate(uniqueArr(uniqueL))

        firstFound = .false.        
        do i = 1, n
          if(.not. (omit .and. omitElement==arr(i))) then
            uniqueArr(1) = arr(i) 
            firstIndex = i                   
            firstFound = .true.
          end if        
          if(firstFound) then
            exit
          end if
        end do          

        uI = 2        

        do i = firstIndex + 1, n
          if (arr(i) /= arr(i - 1)) then
            if(.not. (omit .and. omitElement==arr(i))) then
              uniqueArr(uI) = arr(i)
              uI = uI + 1              
            end if            
          end if
        end do                  
      end subroutine uniquify
!--------------------------------------------------------------------         
      end module auxroutines