! module gvroutines
! (c) Vladislav Matus
! last edit: 19. 09. 2018      

      module gvroutines
        implicit none        
      contains

!--------------------------------------------------------------------

! subroutine graphvizcr
! (c) Vladislav Matus
! last edit: 15. 07. 2018
! TODO: check contents of ia, ja
! TODO: fill ierr      
!
! Purpose:
!   creating Graphviz friendly file to look at the graph corresponding
!   to matrix desribed by ia, ja
!   can be used e.g. https://dreampuf.github.io/GraphvizOnline/ (16. 3. 2018)
! Input:
!   ia, ja ... matrix in CSR format
!   n ... size of this matrix
!   part ... vector describing the partition of corresponding graph      
!   unitn ... nuber of unit of the opened file         
!   vertsep ... boolean flag, if true, vertex separator is colored, else edge separator
! Output:
!   ierr ... error code (0 if succesful, 1 otherwise)
! Allocations: none     

      subroutine graphvizcr(ia,ja,n,part,unitn,ierr,isvertsep)
        implicit none
!
! parameters
!
      integer :: n, unitn, ierr
      integer :: ia(n+1),ja(ia(n+1)-1)
      integer :: part(n)
      logical, optional :: isvertsep
!
! internals
!        
      integer :: i,j,vertsepindex
      character(len=10) :: ich,jch 
      logical :: vertsep
!
! constants
!        
      character(len=*), parameter :: formatedge = "[color=red]"
      character(len=*), parameter :: formatvertex = "[fillcolor=yellow, style=filled]"
!
! start of graphvizcr
!

!
! -- write out the file
!   
      
      if (present(isvertsep)) then
        vertsep = isvertsep
      else
        vertsep = .false.
      end if
      
      write(unitn,*) "strict graph G {"
      
      if(vertsep) then 
        vertsepindex = maxval(part)        
        do i = 1, n
          if (part(i) == vertsepindex) then
            write(ich,'(I10)') i
            write(unitn,*) ich//formatvertex
          end if
        end do
      end if

      do i = 1, n
        do j = ia(i), ia(i+1)-1
          if (i <= ja(j)) then
            !write all of the edges
            ich = "";
            jch = "";              
            write(ich,'(I10)') i
            write(jch,'(I10)') ja(j)              
            write(unitn,'(a)',advance='no') "  "//ich//"--"//jch;
            !format the edge separator
            if (part(i) /= part(ja(j)) .and. .not. vertsep) then
              write(unitn,'(a)',advance='no') formatedge
            end if
            write(unitn,*) !new line
          end if
        end do
      end do
      write(unitn,*) "}"     
!
! end of graphvizcr
!  
      end subroutine graphvizcr  

!--------------------------------------------------------------------            

!
! end of module
!      
      end module gvroutines


