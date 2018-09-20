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
      subroutine uniquify(arr, uniqueArr)   
        implicit none       
        integer, dimension(:) :: arr
        integer, allocatable, dimension(:) :: uniqueArr
        integer :: n, i, uI, uniqueL
        n = SIZE(arr)
        write(*,*) "n",n 
        call insertionSort(arr)        
        uniqueL = 0
        do i = 1, n
          if (arr(i) /= arr(i - 1)) then
            uniqueL = uniqueL + 1
          end if
        end do
        allocate(uniqueArr(uniqueL))
        uniqueArr(1) = arr(1)
        uI = 2        
        do i = 2, n
          if (arr(i) /= arr(i - 1)) then
            uniqueArr(uI) = arr(i)
            uI = uI + 1
          end if
        end do                  
      end subroutine uniquify
!--------------------------------------------------------------------         
      end module auxroutines