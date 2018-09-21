! module auxroutines
! (c) Vladislav Matus
! last edit: 20. 09. 2018      

      module auxroutines            
        implicit none  
        private  
        public :: insertionSort, uniquify, trim

      contains
!-------------------------------------------------------------------- 
! subroutine insertionSort
! (c) Vladislav Matus
! last edit: 20. 09. 2018  
!      
! Purpose:
!   Sort array 
! Input:
!   arr ... array for sorting
! Output:
!   arr ... sorted array
! Allocations: none
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
            !write(*,*) "swapping", arr(j), arr(j-1)
            val = arr(j-1)
            arr(j-1) = arr(j)
            arr(j) = val
            j = j - 1
          end do
        end do
      end subroutine insertionSort
!-------------------------------------------------------------------- 
! subroutine uniquify
! (c) Vladislav Matus
! last edit: 20. 09. 2018  
!      
! Purpose:
!   Return array with all duplicates removed
! Input:
!   arr ... array for removing
!   [omitElements] ... which elements should be removed completely
! Output:
!   uniqueArr ... array with each value only once
!   ierr ... 0 if uniqueArr is not empty, 1 otherwise      
! Allocations: uniqueArr (only if not empty)
!--------------------------------------------------------------------          
      subroutine uniquify(arr, uniqueArr, ierr, omitElements)   
        implicit none       

        integer, dimension(:) :: arr
        integer, allocatable, dimension(:) :: uniqueArr
        integer, dimension(:), optional :: omitElements
        integer :: n, i, j, uI, uniqueL, ierr, firstIndex, omitNo    
        logical :: omit, firstFound        

        if(present(omitElements))then
          omitNo = SIZE(omitElements)
        else
          omitNo = 0
        endif
        ierr = 0        

        n = SIZE(arr)                
        call insertionSort(arr)                  
        uniqueL = 0
        do i = 1, n
          if (arr(i) /= arr(i - 1)) then
            omit = .false.
            do j = 1, omitNo
              omit = omit .or. (omitElements(j)==arr(i))
            end do
            if(.not. omit) then
              uniqueL = uniqueL + 1            
            end if          
          end if
        end do                        

        if(uniqueL < 1) then
          write(*,*) "[auxroutines.f90:uniquify] Warning: Nothing to return in uniquify!"
          ierr = 1
          return
        end if

        allocate(uniqueArr(uniqueL))

        firstFound = .false.        
        do i = 1, n
          omit = .false.
          do j = 1, omitNo
            omit = omit .or. (omitElements(j)==arr(i))
          end do
          if(.not. omit) then
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
          omit = .false.
          do j = 1, omitNo
            omit = omit .or. (omitElements(j)==arr(i))
          end do
            if(.not. omit) then
              uniqueArr(uI) = arr(i)
              uI = uI + 1              
            end if            
          end if
        end do                  
      end subroutine uniquify
!-------------------------------------------------------------------- 
! subroutine trim
! (c) Vladislav Matus
! last edit: 21. 09. 2018  
!      
! Purpose:
!   Remove 
! Input:
!   arr ... array for removing trailing zeros
!   endIndex ... last element or new array, if > SIZE(arr), warning thrown
! Output:
!   arr ... resulting subarray
! Allocations: none
!--------------------------------------------------------------------          
      subroutine trim(arr, endIndex)
        implicit none
        integer, allocatable, dimension(:) :: arr, newArr
        integer :: endIndex
        if(endIndex > SIZE(arr)) then
          write(*,*) "[auxroutines.f90:trim] Warning: endIndex > SIZE(arr), not trimmed"
          return 
        end if
        allocate(newArr(endIndex))
        newArr = arr(1 : endIndex)
        deallocate(arr)
        arr = newArr
        deallocate(newArr)
      end subroutine trim
!--------------------------------------------------------------------          

      end module auxroutines