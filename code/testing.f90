! module testing
! (c) Vladislav Matus
! last edit: 21. 09. 2018      

      module testing
        use auxroutines
        implicit none
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
          case (4)
            n = 7
            ia = [1, 4, 8, 11, 13, 17, 20, 23]
            ja = [5,2,4,  3,1,6,7,  5,2,6,  1,7,  3,6,7,1,  3,5,2,  4,5,2]  
          case (5)
            n = 10
            ia = [1,4,7,10,13,17,20,23,28,32,37]
            ja = [1,3,8, 2,5,9, 1,3,7, 4,9,10, 2,5,9,10, 6,7,8, 3,6,7, 1,6,8,9,10, 4,5,8,9, 2,4,5,8,10]  
          case (6)
            n = 9
            ia = [1,2,4,5,7,10,13,17,20,23]
            ja = [2, 1,9, 4, 3,9, 6,7,8, 5,7,8, 5,6,8,9, 5,6,7, 2,4,7]  
          case (7)
            n = 4
            ia = [1,1,3,5,7]
            ja = [3,4,2,4,2,3]  
          case default !the same as case 1
            n = 5
            ia = [1, 4, 6, 8, 10, 13]
            ja = [2, 3, 5, 1, 4, 1, 5, 2, 5, 1, 3, 4]    
        end select
        allocate(aa(ia(n+1)-1))
        aa = 0
      end subroutine loadTestGraph
!--------------------------------------------------------------------        
      subroutine loadTestPart(part, graphID)
        implicit none
        integer :: graphID
        integer, allocatable, dimension(:) :: part        
        select case (graphID)
          case (4)
            part = [1, 1, 1, 2, 1, 1, 1]
          case default !the same as case 1
            stop "[testing.f90:loadTestPart] ERROR: Unknown graphID"
        end select
      end subroutine loadTestPart
!--------------------------------------------------------------------                         
      subroutine testUniqueness(arr)
        implicit none        
        integer, allocatable, dimension(:) :: arr        
        integer, allocatable, dimension(:) :: uniqueArr 
        integer :: ierr        
        call uniquify(arr, uniqueArr, ierr)
        if(.not. SIZE(arr) == SIZE(uniqueArr)) then
          write(*,*) "TEST: Array not unique"
          write(*,*) arr
          write(*,*) uniqueArr
          write(*,'(50I4)') arr
          stop "aborting"
        end if
      end subroutine testUniqueness
!--------------------------------------------------------------------                         
    end module testing
