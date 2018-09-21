! module testing
! (c) Vladislav Matus
! last edit: 21. 09. 2018      

      module testing
      contains
!--------------------------------------------------------------------           
      subroutine loadTestGraph(ia, ja, aa, n, graphID)
        implicit none
        integer :: n, graphID
        integer, allocatable, dimension(:) :: ia, ja
        double precision, allocatable, dimension(:) :: aa
        select case (graphID)
          case (2)
            n = 5
            ia = [1, 5, 9, 13, 17, 21]
            ja = [3, 2, 5, 4, 1, 3, 4, 5, 1, 2, 4, 5, 1, 2, 3, 5, 1, 2, 3, 4]   
          case (3)
            n = 4
            ia = [1, 2, 4, 6, 7]
            ja = [2, 1, 3, 2, 4, 3]
          case default !the same as case 1
            n = 5
            ia = [1, 4, 6, 8, 10, 13]
            ja = [2, 3, 5, 1, 4, 1, 5, 2, 5, 1, 3, 4]    
        end select
        allocate(aa(ia(n+1)-1))
        aa = 0
      end subroutine loadTestGraph
!--------------------------------------------------------------------                      
      end module testing