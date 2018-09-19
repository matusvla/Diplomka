! module myroutines90
! (c) Vladislav Matus
! last edit: 19. 09. 2018      

      module myroutines90
        use raggedmultiarray
        use m_mrgrnk
        implicit none        
      contains

!--------------------------------------------------------------------           

! subroutine metcr
! (c) Vladislav Matus
! last edit: 22. 7. 2018
! TODO: check contents of ia, ja      
! TODO: fill ierr
!
! Purpose:
!   creating contents of a file suitable for METIS graph partitioner
! Input:
!   ia, ja ... matrix in CSR format
!   n ... size of this matrix
!   unitn ... nuber of unit of the opened file         
! Output:
!   ierr ... error code (0 if succesful, 1 otherwise)  
! Allocations: none             
! TODO TEST, changed but not tested!

      subroutine metcr(ia,ja,n,unitn,ierr)
        implicit none
!
! parameters
!
      integer :: n, unitn, ierr
      integer, allocatable, dimension(:) :: ia, ja
!
! internals
!        
        integer :: i,j,loop
!
! start of metcr
!      
! -- count number of loops in graph
!        
        loop = countloops(n,ia,ja)
!
! -- write header for METIS
!             
        write(unitn,"(I10.1)",advance="no") n    ! number of vertices
        write(unitn,"(I10)") (ia(n+1)-1-loop)/2  ! number of edges-loops
!
! -- write the rest of the file
! 
        do i = 1, n
          do j = ia(i), ia(i+1)-1
            if(i/=ja(j)) then
              write(unitn,"(I12.1)",advance="no") ja(j);
            end if
          end do
          write(unitn,*)
        end do
!
! end of metcr
!  
      end subroutine metcr    
      
!--------------------------------------------------------------------           

! subroutine loadpartition
! (c) Vladislav Matus
! last edit: 16. 03. 2018  
! TODO error handling      
! TODO fill ierr  
! TODO check if nubmer of parts is as it should be
!
! Purpose:
!   Loading of graph partition from input file into an array,
!   partitions are indexed from 1.
! Input:
!   n ... size of this matrix
!   unitn ... nuber of unit of the opened file         
! Output:
!   part ... partitioning of matrix
!   ierr ... error code (0 if succesful, 1 otherwise)  
! Allocations: part 

      subroutine loadpartition(part,n,unitn,ierr)
        implicit none
!
! parameters
!
      integer :: n, unitn, ierr
      integer, allocatable :: part(:)
!
! internals
!        
      integer :: i,j
!
! start of loadpartition
!
      allocate(part(n),stat=ierr)        
      do i = 1, n
        read(unitn,*) part(i)
        part(i) = part(i) + 1 ! indexing from 1
      end do       
!
! end of loadpartition
!  
      end subroutine loadpartition

!--------------------------------------------------------------------

! subroutine crsubgr
! (c) Vladislav Matus
! last edit: 15. 07. 2018  
! TODO error handling      
! TODO fill ierr  
!
! Purpose:
!   This routine takes a matrix in arrays ia, ja, aa and the partitioning
!   of the coresponding graph and creates multidimensional "ragged" arrays
!   iap, jap, aap (referenced by *%vectors(i)%elements(j)) describing 
!   the submatrices corresponding to subgraphs created by said partitioning.   
!   The vertex separator is stored in iap(parts+1), jap(parts+1), aap((parts+1)
! Input:
!   ia, ja, aa ... matrix in CSR format
!   n ... size of this matrix
!   part ... vector of length n containing the partitioning of the graph
!   parts ... number of subgraphs
! Output:
!   iap, jap, aap ... multidimensional ragged arrays containing submatrices in CSR format
!   np ... vector of sizes of the submatrices
!   ierr ... error code (0 if succesful, 1 otherwise)  
!   perm ... array containing the new indices of vertices after partition
!   invperm ... multidimensional array containing the original indices
! Allocations: iap%vectors,iap%vectors%elements(1..parts),
!   jap%vectors, jap%vectors%elements(1..parts),
!   aap%vectors, aap%vectors%elements(1..parts),
!   np, perm, invperm%vectors, invperm%vectors%elements(1..parts) 

      subroutine crsubgr(ia,ja,aa,n,part,parts,iap,jap,aap,np,perm,invperm,ierr)
        implicit none
!
! parameters
!
      integer :: n, ierr, parts
      integer :: ia(n+1),ja(ia(n+1)-1),part(n)
      double precision :: aa(ia(n+1)-1)
      type(intraggedarr) :: iap,jap
      type(dpraggedarr) :: aap
	type(intraggedarr) :: invperm	  	  
	integer, allocatable, dimension(:) :: np,perm
!
! internals
!              
      integer :: i, j      
      integer, allocatable, dimension(:) :: ip,jp !indices for cycling     
      integer, allocatable, dimension(:) :: nep ! (2 * edges + loops) in parts 
!
! start of crsubgr
!	
! -- zero out np, npe and count np, npe again
!           
      allocate(np(parts + 1), stat=ierr)
      allocate(nep(parts + 1), stat=ierr)      
      ! zero out
      do i = 1, parts + 1
        np(i)=0
        nep(i)=0
      end do
      ! count np
      do i = 1, n
        if (part(i) /= -1) then ! if not separator
          np(part(i)) = np(part(i)) + 1
        else
          np(parts + 1) = np(parts + 1) + 1
        end if        
      end do    
      ! count npe
      do i = 1, n
        do j = ia(i), ia(i+1) - 1          
          if(part(i) == part(ja(j)))then ! if in same partition            
              nep(part(i)) = nep(part(i)) + 1                        
          end if
        end do
      end do      
!
! -- allocate and fill in perm, invperm
!                     
      !allocate ip
      allocate(ip(parts+1),stat=ierr)      
      ! fill ip with ones
      do i = 1, parts + 1
        ip(i) = 1
      end do      
      allocate(invperm%vectors(parts+1),stat=ierr)
      do i = 1, parts + 1        
        allocate(invperm%vectors(i)%elements(np(i)), stat=ierr)        
      end do      
      allocate(perm(n),stat=ierr)
      do i = 1, n
        perm(i) = ip(part(i))
        invperm%vectors(part(i))%elements(ip(part(i))) = i
        ip(part(i)) = ip(part(i)) + 1        
      end do      
!
! -- allocate iap, jap, aap
!                
      ! allocate the first dimension of the multiarrays to number of partitions + 1
      allocate(iap%vectors(parts+1),stat=ierr)
      allocate(jap%vectors(parts+1),stat=ierr)
      allocate(aap%vectors(parts+1),stat=ierr)      
      ! allocate the second dimension of the multiarrays ia, ja, aa, invperm
	do i = 1, parts + 1
        allocate(iap%vectors(i)%elements(np(i)+1), stat=ierr)        
        allocate(jap%vectors(i)%elements(nep(i)), stat=ierr)
        allocate(aap%vectors(i)%elements(nep(i)), stat=ierr)        
      end do
!
! -- fill in the iap, jap, aap
!      
      !allocate jp
      allocate(jp(parts+1),stat=ierr)         
      !set first elements of iap
      do i = 1, parts + 1        
        iap%vectors(i)%elements(1) = 1
      end do
      ! fill ip with twosm jp with ones
      do i = 1, parts + 1
        ip(i) = 2
        jp(i) = 1
      end do      
      !fill iap      
      do i = 1, n
        !set appropriate element in iap to his predecessor
        iap%vectors(part(i))%elements(ip(part(i))) = iap%vectors(part(i))%elements(ip(part(i))-1)            
        do j = ia(i), ia(i+1) - 1
          if(part(i) == part(ja(j))) then ! if in same partition 
            !raise iap element by one          
            iap%vectors(part(i))%elements(ip(part(i))) = iap%vectors(part(i))%elements(ip(part(i))) + 1
            !write neighbour into jap and aap            
            jap%vectors(part(i))%elements(jp(part(i))) = perm(ja(j))            
            aap%vectors(part(i))%elements(jp(part(i))) = aa(j)            
            jp(part(i)) = jp(part(i)) + 1
          end if
        end do
        ip(part(i)) = ip(part(i)) + 1       
      end do      
 
!
! -- clean up
!
      deallocate(ip,stat=ierr)
      deallocate(jp,stat=ierr)
      deallocate(nep,stat=ierr)  
!
! end of crsubgr
!  
      end subroutine crsubgr
      
!--------------------------------------------------------------------

! subroutine subgrcleanup
! (c) Vladislav Matus
! last edit: 15. 07. 2018  
!
! Purpose:
!   This routine deallocates all allocated fields after using routine crsubgr
! Input:
!   iap,jap,aap,np,perm,invperm ... various fields which need deallocating
!   parts ... number of parts in partition
! Output:
!   ierr ... error code (0 if succesful, # of fails otherwise)  
! Allocations: none

      subroutine subgrcleanup(iap,jap,aap,np,perm,invperm,parts,ierr)
        implicit none
!
! parameters
!
      integer :: parts,ierr            
      type(intraggedarr) :: iap,jap
      type(dpraggedarr) :: aap
	type(intraggedarr) :: invperm	  	  
	integer, allocatable, dimension(:) :: np,perm
!
! internals
!              
      integer :: i, iierr        
!
! start of subgrcleanup
!	
! -- deallocate all fields
! 
      ierr = 0
	do i = 1, parts + 1
        deallocate(iap%vectors(i)%elements, jap%vectors(i)%elements, & 
          aap%vectors(i)%elements, invperm%vectors(i)%elements, stat=iierr)
        ierr = ierr + iierr
      end do
      deallocate(iap%vectors, jap%vectors, aap%vectors, invperm%vectors, &
        perm, np, stat=iierr)
      ierr = ierr + iierr
!
! end of subgrcleanup
!  
      end subroutine subgrcleanup
        
!--------------------------------------------------------------------        

! subroutine shiftnumbering
! (c) Vladislav Matus
! last edit: 22. 07. 2018  
!
! Purpose:
!   This routine changes the numbering of the vertices and optionaly partition
!   Standard usage is with coef -1 for changing Fortran -> C
!                       of coef +1 for changing C -> Fortran
! Input:
!   n ... integer, size of graph
!   ia,ja ... graph in CSR format
!   coef ... integer, by how much should the numbering of the vertices change
! Allocations: none

      subroutine shiftnumbering(coef,n,ia,ja,part)
        implicit none
!
! parameters
!
      integer :: n, coef
      integer, allocatable, dimension(:) :: ia, ja      
      integer, allocatable, dimension(:), optional :: part      
!
! internals
!              
      integer :: i, ne        
!
! start of shiftnumbering
!	         
      ne = ia(n+1) - ia(1)
      do i = 1, n+1
        ia(i) = ia(i) + coef
      end do
      do i = 1, ne
        ja(i) = ja(i) + coef
      end do
      if(present(part))then
        do i = 1, n
          part(i) = part(i) + coef
        end do
      end if
!
! end of shiftnumbering
!  
      end subroutine shiftnumbering
        
!-------------------------------------------------------------------- 

! function countloops
! (c) Vladislav Matus
! last edit: 22. 07. 2018  
!
! Purpose:
!   This routine counts all loops in the graph
! Input:
!   n ... integer, size of graph
!   ia,ja ... graph in CSR format
! Returns:
!   integer, number of loops
! Allocations: none

      integer function countloops(n,ia,ja)
        implicit none
!
! parameters
!
      integer :: n
      integer, allocatable, dimension(:) :: ia, ja      
!
! internals
!              
      integer :: i, j
!
! start of countloops
!	      
      countloops = 0;
      do i = 1, n
        do j = ia(i), ia(i+1)-1
          if(i == ja(j)) then
            countloops = countloops + 1
         end if
        end do
      end do
      return
!
! end of countloops
!  
      end function countloops
        
!-------------------------------------------------------------------- 
! subroutine remloops
! (c) Vladislav Matus
! last edit: 22. 07. 2018  
!
! Purpose:
!   This routine removes all loops in the graph
! Input:
!   n ... integer, number of vertices in graph
!   ia,ja,aa ... graph in CSR format
! Output:
!   iaN,jaN,aaN .. graph without loops in CSR format
! Allocations: iaN, jaN, aaN
! TODO Error handling

      subroutine remloops(n,ia,ja,aa,iaN,jaN,aaN,ierr)
        implicit none
!
! parameters
!
      integer :: n, ierr
      integer, allocatable, dimension(:) :: ia, ja, iaN, jaN
      double precision, allocatable, dimension(:) :: aa, aaN
!
! internals
!              
      integer :: i, j, loops, seenloops     
!
! start of remloops
!	      
      loops = countloops(n,ia,ja)
      allocate(iaN(n+1), jaN(ia(n+1)-loops-1), aaN(ia(n+1)-loops-1), stat=ierr)
      seenloops = 0      
      do i = 1, n
        iaN(i) = ia(i) - seenloops
        do j = ia(i), ia(i+1) - 1
          if (i == ja(j)) then
            seenloops = seenloops + 1            
          else
            jaN(j-seenloops) = ja(j)
            aaN(j-seenloops) = aa(j)
          end if          
        end do
      end do
      iaN(n+1) = ia(n+1) - seenloops;

!
! end of remloops
!  
      end subroutine remloops
!-------------------------------------------------------------------- 
! subroutine countDistance
! (c) Vladislav Matus
! last edit: 19. 09. 2018  
!
! Purpose:
!   create ordering of subgraphs according to distance from vertex separator
!   Returns array of distances from separator
!   
! Input:
!   ia, ja, aa ... graph in CSR format
!   n ... size of the corresponding matrix
!   parts ... number of parts in partition
!   part ... vector describing the partition of the graph
!   
! Output:
!   distFromSep ... int vector of length n which contains distances of vertices from separator
!   
! Allocations:  distFromSep

      subroutine countDistance(ia, ja, n, part, parts, distFromSep)
        implicit none
!
! parameters
!
        integer :: n, parts, ierr
        integer :: ia(n+1), ja(ia(n+1)-1), part(n), distFromSep(n)        
!
! internals
!             
        integer :: i, j, maxDepth, vertexNo
        integer :: currentLayer(n), currentLayerSize = 0
        integer :: oldLayer(n), oldLayerSize = 0
        logical :: isOrdered(n)        
!
! start of countDistance
!	    
! -- initialize currentLayer, currentLayerSize with separator
!    and fill isOrdered and distFromSep accordingly        
!          
        do i = 1, n          
          if (part(i) == parts + 1) then
            isOrdered(i) = .true.
            distFromSep(i) = 0
            currentLayerSize = currentLayerSize + 1
            currentLayer(currentLayerSize) = i            
          else
            isOrdered(i) = .false.
          end if
        end do
!
! -- count distance from separator for all the vertices
!                       
        maxDepth = -1        

        do while (currentLayerSize /= 0)
          maxDepth = maxDepth + 1
          oldLayer = currentLayer
          oldLayerSize = currentLayerSize
          currentLayerSize = 0
          do i = 1, oldLayerSize
            vertexNo = oldLayer(i)
            do j = ia(vertexNo), ia(vertexNo + 1) - 1
              if (.not. isOrdered(ja(j))) then                
                isOrdered(ja(j)) = .true.
                distFromSep(ja(j)) = maxDepth + 1
                currentLayerSize = currentLayerSize + 1
                currentLayer(currentLayerSize) = ja(j)                
              end if
            end do
          end do         
        end do    
!
! end of countDistance
!  
      end subroutine countDistance     
      
!-------------------------------------------------------------------- 
! subroutine ordervertices
! (c) Vladislav Matus
! last edit: 19. 09. 2018  
!
! Purpose:
!   Computes the permutation of vertices in graph to be ordered
!   in an order specified by internal order vector.
!   
! Input:
!   ia, ja, aa ... graph in CSR format
!   n ... number of vertices    
!   part ... vector of length n containing the partitioning of the graph
!   parts ... number of subgraphs        
!   
! Output:
!   iaord, jaord, aaord ... ordered graph in CSR format     
!   perm ... permutation: original ordering -> new ordering
!   invperm ... permutation: new ordering -> original ordering 
!   ierr ... error code (0 if succesful, 1 otherwise)             
!   
! Allocations:  perm, invperm

      subroutine ordervertices(ia, ja, aa, n, part, parts, perm, invperm, ierr)
        implicit none
!
! parameters
!
      integer :: ierr, n, parts
      integer :: ia(n+1), ja(ia(n+1)-1), part(n)
      double precision :: aa(ia(n+1)-1)    	  
      integer, allocatable, dimension(:) :: perm, invperm      
!
! internals
!              
      integer :: i, distFromSep(n)
      real :: ordvalue, order(n)
      double precision :: distOrd(n)
!
! start of ordervertices
!	    
      !TODO real order, now just random
      ! do i = 1, n
      ! CALL RANDOM_NUMBER(ordvalue)
      ! order(i) = ordvalue
      ! end do 
      
      call countDistance(ia, ja, n, part, parts, distFromSep) 
      write(*,'(30I3)') distFromSep   
!
! -- fill in invperm and perm using sorted order values
!                  
      allocate(perm(n),invperm(n),stat=ierr)
      call MRGRNK (distFromSep, invperm);
      do i = 1, n
        perm(invperm(i)) = i
      end do
!
! end of ordervertices
!  
      end subroutine ordervertices
        
!--------------------------------------------------------------------       

!
! end of module
!      
      end module myroutines90


