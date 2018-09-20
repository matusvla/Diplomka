! module auxroutines
! (c) Vladislav Matus
! last edit: 20. 09. 2018      

      module auxroutines
        implicit none        
      contains
!--------------------------------------------------------------------
      subroutine insertionSort(arr, arrSize)
            implicit none
            integer :: arrSize, arr(arrSize)
            integer :: i, j, val
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
      end module auxroutines


