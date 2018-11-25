! module auxroutines
! (c) Vladislav Matus
! last edit: 20. 09. 2018      

      module auxroutines            
        implicit none  
        private  
        public :: insertionSort, uniquify, trimArr, shiftArr, logical2intArr

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
! last edit: 07. 10. 2018  
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
        integer, allocatable, dimension(:) :: uniqueArr, arrCopy
        integer, dimension(:), optional :: omitElements
        integer :: n, i, j, uI, uniqueL, ierr, firstIndex, omitNo    
        logical :: omit, firstFound        

        if(present(omitElements))then
          omitNo = SIZE(omitElements)
        else
          omitNo = 0
        endif
        ierr = 0     

        arrCopy = arr
        n = SIZE(arrCopy)                
        call insertionSort(arrCopy)                  
        uniqueL = 0
        omit = .false.
        do j = 1, omitNo
          omit = omit .or. (omitElements(j)==arrCopy(1))
        end do
        if(.not. omit) then
          uniqueL = uniqueL + 1            
        end if 

        do i = 2, n
          if (arrCopy(i) /= arrCopy(i - 1)) then
            omit = .false.
            do j = 1, omitNo
              omit = omit .or. (omitElements(j)==arrCopy(i))
            end do
            if(.not. omit) then
              uniqueL = uniqueL + 1            
            end if          
          end if
        end do  

        if(uniqueL < 1) then
          ierr = 1
          return
        end if

        allocate(uniqueArr(uniqueL))

        firstFound = .false.        
        do i = 1, n
          omit = .false.
          do j = 1, omitNo
            omit = omit .or. (omitElements(j)==arrCopy(i))
          end do
          if(.not. omit) then
            uniqueArr(1) = arrCopy(i) 
            firstIndex = i                   
            firstFound = .true.
          end if        
          if(firstFound) then
            exit
          end if
        end do          

        uI = 2        

        do i = firstIndex + 1, n
          if (arrCopy(i) /= arrCopy(i - 1)) then
          omit = .false.
          do j = 1, omitNo
            omit = omit .or. (omitElements(j)==arrCopy(i))
          end do
            if(.not. omit) then
              uniqueArr(uI) = arrCopy(i)
              uI = uI + 1              
            end if            
          end if
        end do  
      end subroutine uniquify
!-------------------------------------------------------------------- 
! subroutine trimArr
! (c) Vladislav Matus
! last edit: 10. 11. 2018  
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
      subroutine trimArr(arr, endIndex, ierr)
        implicit none
        integer, allocatable, dimension(:) :: arr, newArr
        integer :: endIndex, ierr
        ierr = 0
        if(endIndex > SIZE(arr)) then
          write(*,*) "[auxroutines.f90:trimArr] WARNING: endIndex > SIZE(arr), not trimmed"
          ierr = 1
          return 
        end if
        allocate(newArr(endIndex))
        newArr = arr(1 : endIndex)
        deallocate(arr)
        arr = newArr
        deallocate(newArr)
      end subroutine trimArr
!-------------------------------------------------------------------- 
! subroutine shiftArr
! (c) Vladislav Matus
! last edit: 21. 09. 2018  
!      
! Purpose:
!   Shift values larger then threshold in array by coef
! Input:
!   arr ... array for shifting
!   threshold ... only values >threshold are modified      
!   [coef] ... shift coeficient, if not specified = -1
! Output:
!   arr ... result
! Allocations: none
!--------------------------------------------------------------------           
      subroutine shiftArr(arr, threshold, coef)
        implicit none
        integer, allocatable, dimension(:) :: arr
        integer :: threshold, coefValue, i
        integer, optional ::coef

        if(present(coef)) then
          coefValue = coef
        else
          coefValue = -1
        end if            
        
        do i = 1, SIZE(arr)
          if (arr(i) > threshold) then
            arr(i) = arr(i) + coefValue
          end if          
        end do
        
      end subroutine shiftArr         
!-------------------------------------------------------------------- 
! subroutine logical2intArr
! (c) Vladislav Matus
! last edit: 22. 09. 2018  
!      
! Purpose:
!   Transforms boolean array into integer array: true = 1, false = 0
!   (Note that you need to add +1 to use resulting array as part)    
! Input:
!   arr ... array for shifting
!   threshold ... only values >threshold are modified      
!   [coef] ... shift coeficient, if not specified = -1
! Output:
!   arr ... result
! Allocations: none
!--------------------------------------------------------------------           
      function logical2intArr (logicalArr)
        implicit none
        logical, dimension(:) :: logicalArr
        integer :: logical2intArr(SIZE(logicalArr))
        logical2intArr = logicalArr                     
      end function logical2intArr         

!-------------------------------------------------------------------- 

      end module auxroutines
