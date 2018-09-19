! module gvroutines
! (c) Vladislav Matus
! last edit: 19. 09. 2018      

      module gvroutines
        implicit none        
      contains

!--------------------------------------------------------------------

! subroutine graphvizcr
! (c) Vladislav Matus
! last edit: 19. 09. 2018
! TODO: check contents of ia, ja
! TODO: fill ierr      
!
! Purpose:
!   The most advanced routine simplified by the routines following it
!   Creates Graphviz friendly .txt file representing the graph corresponding
!   to matrix desribed by ia, ja
!   can be visualised by e.g. https://dreampuf.github.io/GraphvizOnline/ (16. 3. 2018)
! Input:
!   ia, ja ... matrix in CSR format
!   n ... size of this matrix
!   part ... vector describing the partition of corresponding graph      
!   unitn ... nuber of unit of the opened file         
!   vertsepcolor ... boolean flag, if true, part with highest number
!     (i. e. vertex separator) is colored to yellow
!   egdesepcolor ... boolean flag, if true, all edges from one part
!     to another are colored red
!   colorful ... boolean flag, each part colored another shade of gray
!   hasLabels ... boolean flag, change labels of vertices
!   labels ... if hasLabels, then labels are used             
! Output:
!   ierr ... error code (0 if succesful, 1 otherwise)
! Allocations: none     

      subroutine graphvizcr(ia, ja, n, part, unitn, ierr, &
        vertsepcolor, edgesepcolor, colorful, hasLabels, labels)
        implicit none
!
! parameters
!
      integer :: n, unitn, ierr
      integer :: ia(n+1), ja(ia(n+1)-1), labels(n)
      integer :: part(n)
      logical :: vertsepcolor, edgesepcolor, colorful, hasLabels
!
! internals
!        
      integer :: i,j,vertsepindex, colorStep
      character(len=10) :: ich, jch, grayShadow
!
! constants
!        
      character(len=*), parameter :: edgeformat = " [color=red]"
      character(len=*), parameter :: separatorformat = "[fillcolor=yellow, style=filled]"

!
! start of graphvizcr
!

!
! -- write out the file
!         
      write(unitn,*) "strict graph G {"      
      
      if(colorful) then          
        colorStep = 100 / maxval(part)
        if (colorStep == 0) then
          colorStep = 1

        end if
        do i = 1, n            
          write(grayShadow,'(I10)') &
            colorStep * part(i) - colorStep * part(i) / 100 * 100
          write(ich,'(I10)') i
          write(unitn,*) "  "//TRIM(ADJUSTL(ich))// &
            "[fillcolor=gray"//TRIM(ADJUSTL(grayShadow))//&
            "fontcolor=red, style=filled]"
        end do
      end if
      
      if(vertsepcolor) then            
        vertsepindex = maxval(part)  
        do i = 1, n
          if (part(i) == vertsepindex) then
            write(ich,'(I10)') i
            write(unitn,*) "  "//TRIM(ADJUSTL(ich))//separatorformat
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
            write(unitn,'(a)',advance='no') &
              "  "//TRIM(ADJUSTL(ich))//" -- "//TRIM(ADJUSTL(jch));
            !format the edge separator
            if (part(i) /= part(ja(j)) .and. edgesepcolor) then
              write(unitn,'(a)',advance='no') edgeformat
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
! subroutine gvSimpleGraph
! (c) Vladislav Matus
! last edit: 19. 09. 2018
! Interface for using graphvizcr for drawing a graph without any partition    
!--------------------------------------------------------------------         
    subroutine gvSimpleGraph (ia, ja, n, unitn, ierr)
        implicit none

        integer :: n, unitn, ierr
        integer :: ia(n+1), ja(ia(n+1)-1), labels(n)
        
        integer :: part(n)
        logical :: vertsepcolor = .false.
        logical :: edgesepcolor = .false.
        logical :: colorful = .false.
        logical :: hasLabels = .false.

        part = 0        
        call graphvizcr(ia, ja, n, part, unitn, ierr, vertsepcolor, &
          edgesepcolor, colorful, hasLabels, labels)

    end subroutine gvSimpleGraph
!--------------------------------------------------------------------       
! subroutine gvColorGraph
! (c) Vladislav Matus
! last edit: 19. 09. 2018
! Interface for using graphvizcr for drawing a graph with every part in different color    
!--------------------------------------------------------------------         
    subroutine gvColorGraph (ia, ja, n, part, unitn, ierr)
        implicit none

        integer :: n, unitn, ierr
        integer :: ia(n+1), ja(ia(n+1)-1), labels(n)
        
        integer :: part(n)
        logical :: vertsepcolor = .false.
        logical :: edgesepcolor = .false.
        logical :: colorful = .true.
        logical :: hasLabels = .false.

        call graphvizcr(ia, ja, n, part, unitn, ierr, vertsepcolor, &
          edgesepcolor, colorful, hasLabels, labels)

    end subroutine gvColorGraph
!--------------------------------------------------------------------       
! subroutine gvColorVertSep
! (c) Vladislav Matus
! last edit: 19. 09. 2018
! Interface for using graphvizcr for drawing a graph with every part in different color    
!--------------------------------------------------------------------         
    subroutine gvColorVertSep (ia, ja, n, part, unitn, ierr)
      implicit none

      integer :: n, unitn, ierr
      integer :: ia(n+1), ja(ia(n+1)-1), labels(n)
      
      integer :: part(n)
      logical :: vertsepcolor = .true.
      logical :: edgesepcolor = .false.
      logical :: colorful = .false.
      logical :: hasLabels = .false.


      call graphvizcr(ia, ja, n, part, unitn, ierr, vertsepcolor, &
        edgesepcolor, colorful, hasLabels, labels)

  end subroutine gvColorVertSep    
!--------------------------------------------------------------------       
! subroutine gvColorEdgeSep
! (c) Vladislav Matus
! last edit: 19. 09. 2018
! Interface for using graphvizcr for drawing a graph with every part in different color    
!--------------------------------------------------------------------         
  subroutine gvColorEdgeSep (ia, ja, n, part, unitn, ierr)
    implicit none

    integer :: n, unitn, ierr
    integer :: ia(n+1), ja(ia(n+1)-1), labels(n)
    
    integer :: part(n)
    logical :: vertsepcolor = .false.
    logical :: edgesepcolor = .true.
    logical :: colorful = .false.
    logical :: hasLabels = .true.

    call graphvizcr(ia, ja, n, part, unitn, ierr, vertsepcolor, &
      edgesepcolor, colorful, hasLabels, labels)

end subroutine gvColorEdgeSep      
!--------------------------------------------------------------------       
! subroutine gvColorEdgeSep
! (c) Vladislav Matus
! last edit: 19. 09. 2018
! Interface for using graphvizcr for drawing a graph with every part in different color    
!--------------------------------------------------------------------         
subroutine gvSetLabels (ia, ja, n, labels, unitn, ierr)
  implicit none

  integer :: n, unitn, ierr
  integer :: ia(n+1), ja(ia(n+1)-1), labels(n)
  
  integer :: part(n)
  logical :: vertsepcolor = .true.
  logical :: edgesepcolor = .false.
  logical :: colorful = .false.
  logical :: hasLabels = .true.

  call graphvizcr(ia, ja, n, part, unitn, ierr, vertsepcolor, &
    edgesepcolor, colorful, hasLabels, labels)

end subroutine gvSetLabels   
!--------------------------------------------------------------------             
!
! end of module
!      
      end module gvroutines


