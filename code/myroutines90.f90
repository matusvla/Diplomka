! module myroutines90
! (c) Vladislav Matus
! last edit: 16. 11. 2018      

      module myroutines90
        use auxroutines
        use raggedmultiarray
        use m_mrgrnk        
        use gvroutines
        implicit none        
        integer, parameter :: MAX_INT =  2147483647
        integer, parameter :: INT_SIZE =  4
      contains

!--------------------------------------------------------------------
! subroutine createSubgraphs
! (c) Vladislav Matus
! last edit: 12. 11. 2018
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
!   nextToVertSep ... logical multidimensional ragged array containig for each vertex info
!     if it is connected to the vertex separator      
!   np ... vector of sizes of the submatrices
!   ierr ... error code (0 if succesful, 1 otherwise)  
!   perm ... array containing the new indices of vertices after partition
!   invperm ... multidimensional array containing the original indices
! Allocations: iap%vectors,iap%vectors%elements(1..parts+1),
!   jap%vectors, jap%vectors%elements(1..parts+1),
!   nextToVertSep%vectors, nextToVertSep%vectors%elements(1..parts+1),
!   np, perm, invperm%vectors, invperm%vectors%elements(1..parts+1) 
!   aap%vectors, aap%vectors%elements(1..parts+1),

      subroutine createSubgraphs(ia, ja, n, part, parts, iap, jap, np, &
        nextToVertSep, perm, invperm, ierr, aa, aap)
        implicit none
!
! parameters
!
      integer :: n, ierr, parts
      integer :: ia(n+1),ja(ia(n+1)-1),part(n)
      type(intRaggedArr) :: iap,jap
      type(logicalRaggedArr) :: nextToVertSep
      type(intRaggedArr) :: invperm
      integer, allocatable, dimension(:) :: np, perm
      type(dpRaggedArr), optional :: aap
      double precision, optional :: aa(ia(n+1)-1)
!
! internals
!              
      integer :: i, j, origInd 
      integer, allocatable, dimension(:) :: ip,jp !indices for cycling     
      integer, allocatable, dimension(:) :: nep ! (2 * edges + loops) in parts 
!
! start of createSubgraphs
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
      ip = 1    
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
      if(present(aap)) then
        allocate(aap%vectors(parts+1),stat=ierr)      
      end if
      allocate(nextToVertSep%vectors(parts + 1),stat=ierr)      
      ! allocate the second dimension of the multiarrays ia, ja, aa, invperm
      do i = 1, parts + 1
        allocate(iap%vectors(i)%elements(np(i)+1), stat=ierr)        
        allocate(jap%vectors(i)%elements(nep(i)), stat=ierr)
        if(present(aap)) then
          allocate(aap%vectors(i)%elements(nep(i)), stat=ierr)
        end if
        allocate(nextToVertSep%vectors(i)%elements(np(i)), stat=ierr)                
      end do      
!
! -- fill in the iap, jap, aap
!      
      !allocate jp
      allocate(jp(parts+1),stat=ierr)         
      !set first elements of iap and set everything in nextToVertSep to false
      do i = 1, parts + 1        
        iap%vectors(i)%elements(1) = 1
        nextToVertSep%vectors(i)%elements = .false.
      end do
      ! fill ip with twos jp with ones
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
            if(present(aap)) then
              aap%vectors(part(i))%elements(jp(part(i))) = aa(j)            
            end if
            jp(part(i)) = jp(part(i)) + 1
          end if          
        end do
        ip(part(i)) = ip(part(i)) + 1       
      end do  
      !mark vectors connected to vertex separator
      do i = 1, np(parts + 1)
        origInd = invperm%vectors(parts + 1)%elements(i)
        do j = ia(origInd), ia(origInd + 1) - 1
          nextToVertSep%vectors(part(ja(j)))%elements(perm(ja(j))) = .true.          
        end do
      end do
!
! -- clean up
!
      deallocate(ip,stat=ierr)
      deallocate(jp,stat=ierr)
      deallocate(nep,stat=ierr)  
!
! end of createSubgraphs
!  
      end subroutine createSubgraphs
      
!--------------------------------------------------------------------
! subroutine subgraphCleanup
! (c) Vladislav Matus
! last edit: 12. 11. 2018  
!
! Purpose:
!   This routine deallocates all allocated fields after using routine createSubgraphs
! Input:
!   iap, jap, aap, nvs, np, perm, invperm ... various fields which need deallocating
!   parts ... number of parts in partition
! Output:
!   ierr ... error code (0 if succesful, # of fails otherwise)  
! Allocations: none

      subroutine subgraphCleanup(iap, jap, np, nvs, perm, invperm, parts, ierr, aap)
        implicit none
!
! parameters
!
      integer :: parts,ierr            
      type(intRaggedArr) :: iap,jap
      type(intRaggedArr) :: invperm
      type(logicalRaggedArr) :: nvs
      integer, allocatable, dimension(:) :: np,perm
      type(dpRaggedArr), optional :: aap
!
! internals
!              
      integer :: i, iierr        
!
! start of subgraphCleanup
!	
! -- deallocate all fields
! 
      ierr = 0
      do i = 1, parts + 1
        deallocate(iap%vectors(i)%elements, jap%vectors(i)%elements, & 
          invperm%vectors(i)%elements, nvs%vectors(i)%elements, stat=iierr)
        ierr = ierr + iierr
        if(present(aap)) then
          deallocate(aap%vectors(i)%elements, stat=iierr)
          ierr = ierr + iierr
        end if
      end do
      deallocate(iap%vectors, jap%vectors, nvs%vectors, &
        invperm%vectors, perm, np, stat=iierr)
      ierr = ierr + iierr
      if(present(aap)) then
        deallocate(aap%vectors)
      end if
!
! end of subgraphCleanup
!  
      end subroutine subgraphCleanup
        
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
!   part ... optional, partition of the graph
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
      integer, dimension(:) :: ia, ja      
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
! last edit: 21. 09. 2018  
!
! Purpose:
!   This routine removes all loops in the graph
! Input:
!   n ... integer, number of vertices in graph
!   ia, ja, aa ... graph in CSR format
! Output:
!   iaN, jaN, aaN .. graph without loops in CSR format
! Allocations: iaN, jaN, aaN  

      subroutine remloops(n, ia, ja, iaN, jaN, ierr, aa, aaN)
        implicit none
!
! parameters
!
      integer :: n, ierr
      integer, dimension(:) :: ia, ja 
      integer, allocatable, dimension(:) :: iaN, jaN
      double precision, allocatable, dimension(:), optional :: aa, aaN
      logical :: countingAa
!
! internals
!              
      integer :: i, j, loops, seenloops     
!
! start of remloops
!	      
      if(present(aa) .and. present(aaN)) then
        countingAa = .true.
      else
        countingAa = .false.
      end if  

      loops = countloops(n,ia,ja)
      allocate(iaN(n+1), jaN(ia(n+1)-loops-1), stat=ierr)
      if (countingAa) allocate(aaN(ia(n+1)-loops-1), stat=ierr)
      seenloops = 0      
      do i = 1, n
        iaN(i) = ia(i) - seenloops
        do j = ia(i), ia(i+1) - 1
          if (i == ja(j)) then
            seenloops = seenloops + 1            
          else
            jaN(j-seenloops) = ja(j)
            if(countingAa) aaN(j-seenloops) = aa(j)
          end if          
        end do
      end do
      iaN(n+1) = ia(n+1) - seenloops;

!
! end of remloops
!  
      end subroutine remloops

!-------------------------------------------------------------------- 
! subroutine addDiagonal
! (c) Vladislav Matus
! last edit: 19. 11. 2018  
!
! Purpose:
!   This routine adds diagonal entries to the input matrix.
! Input:
!   n ... integer, size of graph
!   ia,ja ... matrix in CSR format
! Output:
!   iaNew, jaNew ... new matrix in CSR format
! Allocations:  iaNew, jaNew

      subroutine addDiagonal(ia, ja, n, iaNew, jaNew)
        implicit none
!
! parameters
!
      integer :: n
      integer, allocatable, dimension(:) :: ia, ja, iaNew, jaNew
!
! internals
!           
      integer :: i, j, before, added
      logical :: hasDiagonal
!
! start of addDiagonal
!	      
      added = 0
      iaNew(1) = ia(1)
      jaNew = 0
      do i = 1, n
        hasDiagonal = .false.
        do j = ia(i), ia(i + 1) - 1
          if (ja(j) == i) hasDiagonal = .true.
        end do
        if(.not. hasDiagonal) then
          jaNew(ia(i) + added) = i
          added = added + 1
        end if
        iaNew(i + 1) = ia(i + 1) + added 
        jaNew(ia(i) + added : ia(i + 1) + added - 1) = ja(ia(i) : ia(i + 1) - 1)
      end do
!
! end of addDiagonal
!  
      end subroutine addDiagonal

!-------------------------------------------------------------------- 
! function countDistance
! (c) Vladislav Matus
! last edit: 10. 11. 2018  
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
!   ierr ... returns 1 if graph is disconnected and therefore has vertices numbered -1, returns 0 otherwise
!   
! Allocations:  distFromSep
! 
! Returns: length of the longest path      


      integer function countDistance(ia, ja, n, part, parts, distFromSep, ierr)
        implicit none
!
! parameters
!
        integer :: n, parts, ierr
        integer :: ia(n+1), ja(ia(n+1)-1), part(n), distFromSep(n)        
!
! internals
!             
        integer :: i, j, maxDepth, vertexNo, testVertexNo = 0
        integer :: currentLayer(n), currentLayerSize = 0
        integer :: oldLayer(n), oldLayerSize = 0
        logical :: isOrdered(n)        
!
! start of countDistance
!	    
! -- initialize currentLayer, currentLayerSize with separator
!    and fill isOrdered and distFromSep accordingly        
!       
        ierr = 0
        distFromSep = n

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
        testVertexNo = currentLayerSize
        
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
          testVertexNo = testVertexNo + currentLayerSize
        end do 
        if (testVertexNo /= n) then
          ierr = 1
          write(*,*) "[myroutines90.f90:countDistance] WARNING: Partition not continuous", &
            " and therefore not all counted!"
        end if
        countDistance = maxDepth  
!
! end of countDistance
!  
      end function countDistance   
      
!-------------------------------------------------------------------- 
! function findMinimumDegreeIndex
! (c) Vladislav Matus
! last edit: 20. 09. 2018  
!
! Purpose:
!   Finds vertex in graph with minimimal degree
! Input:
!   n ... integer, size of graph
!   ia ... pointer array of graph in CSR format
! Returns:
!   integer, index of vertex with minimal degree
! Allocations: none

      integer function findMinimumDegreeIndex(ia, n)
        implicit none

        integer :: n, ia(n + 1), minimumDegree, i

        minimumDegree = MAX_INT
        do i = 1, n          
          if (minimumDegree > ia(i + 1) - ia(i)) then
            minimumDegree = ia(i + 1) -ia(i)
            findMinimumDegreeIndex = i
          end if
        end do   

      end function findMinimumDegreeIndex

!-------------------------------------------------------------------- 
! subroutine findMinimumDegreeMask
! (c) Vladislav Matus
! last edit: 23. 09. 2018  
!
! Purpose:
!   Returns logical mask where all vertices with minimal value are .true.
! Input:
!   n ... integer, size of graph
!   ia ... pointer array of graph in CSR format
! Returns:
!   logical array, mask with vertices with minimal ldegree marked as .true.      
! Allocations: none

      subroutine findMinimumDegreeMask(ia, n, resultingMask)
        implicit none

        integer :: n, ia(n + 1), minimumDegree, i
        logical :: resultingMask(n)
        minimumDegree = MAX_INT
        resultingMask = .false.
        
        do i = 1, n          
          if (minimumDegree > ia(i+1)-ia(i)) then            
            minimumDegree = ia(i+1)-ia(i)
            resultingMask = .false.
            resultingMask(i) = .true.       
          else if (minimumDegree == ia(i+1)-ia(i)) then
            resultingMask(i) = .true.    

          end if        
        end do 
      end subroutine findMinimumDegreeMask

!--------------------------------------------------------------------           
! subroutine normalizeOrdering
! (c) Vladislav Matus
! last edit: 21. 09. 2018  
!      
! Purpose:
!   Takes an array of indices removed from resulting graph in each step
!   of minimum degree algorithm and projects it onto an array with
!   the indices corresponding to original ones            
! Input:
!   ord ... input indices array
! Output:
!   ord ... minimum ordering of the graph
! Allocations: none  

      subroutine normalizeOrdering(ord)
        implicit none
        integer, dimension(:) :: ord
        integer :: n, i, j
        
        n = SIZE(ord)
        do i = n - 1, 1, -1
          do j = i + 1, n
            if (ord(i) <= ord(j)) ord(j) = ord(j) + 1
          end do
        end do
      end subroutine normalizeOrdering       

!-------------------------------------------------------------------- 
! subroutine vertexToClique
! (c) Vladislav Matus
! last edit: 21. 09. 2018  
!
! Purpose:
!   Transforms graph into a new graph where one of the original vertices
!   is removed and all its neighbours are connected into a clique
!   
! Input:
!   ia, ja ... graph in CSR format
!   n ... number of vertices   
!   replaceIndex ... index of vertex which should be removed      
!   
! Output:
!   iaNew, jaNew ... new graph in CSR format
!   nNew ... number of vertices of the new graph
!   
! Allocations:  iaNew, jaNew
!      
      subroutine vertexToClique(ia, ja, n, iaNew, jaNew, nNew, replaceIndex)
        implicit none

        integer :: n, ia(n + 1), ja(ia(n+1)-1), replaceIndex, ierr
        integer :: nNew
        integer, allocatable, dimension(:) :: iaNew, jaNew

        integer :: vertexDegree
        integer :: i, j, jaNewSize, nI, iaDiff
        integer :: neighbours(ia(replaceIndex + 1) - ia(replaceIndex))
        integer, allocatable, dimension(:) :: uniqueNeighbours        

! -- allocations
        vertexDegree = ia(replaceIndex + 1) - ia(replaceIndex)
        nNew = n - 1
        jaNewSize = ia(n + 1) - 1 - 2 * vertexDegree + vertexDegree * vertexDegree
        allocate(iaNew(nNew + 1), jaNew(jaNewSize))
        jaNew = 0
        iaNew = 0

! -- 
        neighbours = ja(ia(replaceIndex) : ia(replaceIndex + 1) - 1)  
        call insertionSort(neighbours)        
        nI = 1
        iaNew(1) = 1
        i = 0 !indexing for avoiding replaceIndex
        do j = 1, n  
          if (j == replaceIndex) cycle                   
          i = i + 1 
          if (nI <= SIZE(neighbours) .and. j == neighbours(nI)) then ! if one of the neighbours of replaceIndex     
            call uniquify([ja(ia(j) : ia(j + 1) - 1), neighbours], uniqueNeighbours, ierr, [j,replaceIndex])
            if(ierr == 0) then              
              iaNew(i + 1) = iaNew(i) + SIZE(uniqueNeighbours)            
              jaNew(iaNew(i) : iaNew(i + 1) - 1) = uniqueNeighbours                        
              deallocate(uniqueNeighbours)            
            else
              iaNew(i + 1) = iaNew(i)
            end if
            nI = nI + 1
          else ! just copy the part of ja
            iaDiff = ia(j + 1) - ia(j)
            iaNew(i + 1) = iaNew(i) + iaDiff
            jaNew(iaNew(i) : iaNew(i + 1) - 1) = ja(ia(j) : ia(j + 1) - 1)
          end if            
        end do      
        call trimArr(jaNew, iaNew(nNew + 1) - 1, ierr)
        call shiftArr(jaNew,replaceIndex)
      end subroutine vertexToClique

!-------------------------------------------------------------------- 
! subroutine minimumordering
! (c) Vladislav Matus
! last edit: 21. 09. 2018  
!
! Purpose:
!   Comupute the minimum ordering of the graph.
!   
! Input:
!   ia, ja ... graph in CSR format
!   n ... number of vertices   
!   
! Output:
!   ordering ... the final ordering of the graph             
!   
! Allocations:  none

      subroutine minimumordering(ia, ja, n, ordering)
        implicit none
!
! parameters
!
        integer :: n, i, ierr
        integer :: ia(n+1), ja(ia(n+1)-1)      
!
! internals
!  
        integer :: minDegIndex, nIn, nOut
        integer, allocatable, dimension(:) :: iaIn, jaIn, iaOut, jaOut
        integer :: ordering(n)           
!
! start of minimumordering
!	        
        call remloops(n, ia, ja, iaIn, jaIn, ierr)        
        nIn = n
        do i = 1, n - 2    
          minDegIndex = findMinimumDegreeIndex(iaIn, nIn)
          ordering(i) = minDegIndex
          call vertexToClique(iaIn, jaIn, nIn, iaOut, jaOut, nOut, minDegIndex)
          iaIn = iaOut
          jaIn = jaOut
          nIn = nOut          
          deallocate(iaOut, jaOut)               
        end do
        ordering(n - 1) = 1 !two vertex graph is symetrical
        ordering(n) = 1
        call normalizeOrdering(ordering)
        deallocate(iaIn, jaIn, stat = ierr)
        

!
! end of minimumordering
!  
      end subroutine minimumordering    

!-------------------------------------------------------------------- 
! subroutine mixedOrdering
! (c) Vladislav Matus
! last edit: 23. 09. 2018  
!
! Purpose:
!   Comupute the mixed ordering of the graph
!   
! Input:
!   ia, ja ... graph in CSR format
!   n ... number of vertices   
!   part ... vector of length n containing the partitioning of the graph
!   parts ... number of subgraphs        
!   
! Output:
!   ordering ... the final ordering of the indices of the input graph             
!   
! Allocations:  none

      subroutine mixedOrdering(ia, ja, n, part, parts, ordering)
        implicit none
!
! parameters
!
        integer :: n, i, ierr, parts
        integer :: ia(n+1), ja(ia(n+1)-1), part(n)
!
! internals
!  
        integer :: nIn, nOut, aux, distFromSep(n)
        integer, allocatable, dimension(:) :: iaIn, jaIn, iaOut, jaOut, dfs
        integer :: ordering(n)
        logical, allocatable, dimension(:) :: mask
!
! start of mixedOrdering
!	        
        call remloops(n, ia, ja, iaIn, jaIn, ierr)
        aux = countDistance(ia, ja, n, part, parts, distFromSep, ierr)
        dfs = distFromSep
        nIn = n
        do i = 1, n - 1     
          allocate(mask(n - i + 1))                   
          call findMinimumDegreeMask(iaIn, nIn, mask)
          ordering(i) = MAXLOC(dfs, 1, mask)
          dfs = [dfs(1 : ordering(i) - 1), dfs(ordering(i) + 1 : n - i + 1)] 
          call vertexToClique(iaIn, jaIn, nIn, iaOut, jaOut, nOut, ordering(i))
          iaIn = iaOut
          jaIn = jaOut
          nIn = nOut          
          deallocate(iaOut, jaOut, mask)
        end do        
        ordering(n) = 1
        call normalizeOrdering(ordering)  
        deallocate(iaIn, jaIn, stat = ierr)

!
! end of mixedOrdering
!  
      end subroutine mixedOrdering        

      
!-------------------------------------------------------------------- 
! subroutine orderByDistance
! (c) Vladislav Matus
! last edit: 12. 10. 2018  
!
! Purpose:
!   Computes the permutation of vertices in graph to be ordered.
!   The vertices furthest from the separator have the lowest numbers
!   When using for one component of graph, set parts = 1 and part has to contain
!   vector of 1s,2s where vertices adjacent to separator are denoted by 2
!   
! Input:
!   ia, ja ... graph in CSR format
!   n ... number of vertices    
!   part ... vector of length n containing the partitioning of the graph
!   parts ... number of subgraphs        
!   
! Output:
!   perm ... permutation: original ordering -> new ordering
!   invperm ... permutation: new ordering -> original ordering 
!   ierr ... error code (0 if succesful, 1 otherwise)             
!   
! Allocations:  perm, invperm

      subroutine orderByDistance(ia, ja, n, part, parts, perm, invperm, ierr)
        implicit none
!
! parameters
!
      integer :: ierr, n, parts, maxDepth
      integer :: ia(n+1), ja(ia(n+1)-1), part(n)       
      integer, allocatable, dimension(:) :: perm, invperm      
!
! internals
!              
      integer :: i, distFromSep(n)
!
! start of orderByDistance
!	    
      maxDepth = countDistance(ia, ja, n, part, parts, distFromSep, ierr)
!      
! -- fill in invperm and perm using sorted order values
!                  
      allocate(perm(n), invperm(n), stat=ierr)
      call MRGRNK ((maxDepth - distFromSep), perm);
      do i = 1, n
        invperm(perm(i)) = i
      end do  
!
! end of orderByDistance
!  
      end subroutine orderByDistance
        
!-------------------------------------------------------------------- 
! subroutine orderByMD
! (c) Vladislav Matus
! last edit: 12. 11. 2018  
!
! Purpose:
!   Computes the permutation of vertices in graph to be ordered using MD algorithm
!   
! Input:
!   ia, ja ... graph in CSR format
!   n ... number of vertices    
!   
! Output:
!   perm ... permutation: original ordering -> new ordering
!   invperm ... permutation: new ordering -> original ordering 
!   ierr ... error code (0 if succesful, 1 otherwise)             
!   
! Allocations:  perm, invperm

      subroutine orderByMD(ia, ja, n, perm, invperm, ierr)
        implicit none
!
! parameters
!
      integer :: ierr, n
      integer :: ia(n+1), ja(ia(n+1)-1)
      integer, allocatable, dimension(:) :: perm, invperm      
!
! internals
!              
      integer :: i   
!
! start of orderByMD
!	    
      allocate(perm(n), invperm(n), stat=ierr)
      call minimumOrdering(ia, ja, n, perm)     
!      
! -- fill in perm using obtained invperm
!                        
      do i = 1, n
        invperm(perm(i)) = i
      end do    
!
! end of orderByMD
!  
      end subroutine orderByMD
        
!-------------------------------------------------------------------- 
! subroutine orderMixed
! (c) Vladislav Matus
! last edit: 12. 11. 2018  
!
! Purpose:
!   Computes the permutation of vertices in graph to be ordered
!   Using enhanced MD algorithm, i. e. the vertex with minimal degree
!   is chosen and if there are more vertices with the same minimal degree,
!   the one the furthest from the separator is chosen            
!   
! Input:
!   ia, ja ... graph in CSR format
!   n ... number of vertices    
!   part ... vector of length n containing the partitioning of the graph
!   parts ... number of subgraphs        
!   
! Output:
!   perm ... permutation: original ordering -> new ordering
!   invperm ... permutation: new ordering -> original ordering 
!   ierr ... error code (0 if succesful, 1 otherwise)             
!   
! Allocations:  perm, invperm

      subroutine orderMixed(ia, ja, n, part, parts, perm, invperm, ierr)
        implicit none
!
! parameters
!
      integer :: ierr, n, parts
      integer :: ia(n+1), ja(ia(n+1)-1), part(n) 
      integer, allocatable, dimension(:) :: perm, invperm      
!
! internals
!              
      integer :: i, ordering(n)      
!
! start of orderMixed
!	    
      allocate(perm(n), invperm(n), stat=ierr)
      call mixedOrdering(ia, ja, n, part, parts, perm)      
!      
! -- fill in invperm and perm using sorted order values
!                        
      do i = 1, n
        invperm(perm(i)) = i
      end do       
!
! end of orderMixed
!  
      end subroutine orderMixed
        
!--------------------------------------------------------------------   

! subroutine orderCoefMixed
! (c) Vladislav Matus
! last edit: 07. 10. 2018  
!
! Purpose: 
!   Order vertices in the givven graph by mixed MD and distance ordering
!   with given weight for each ordering.      
!   
! Input:
!   ia, ja ... graph in CSR format
!   n ... number of vertices    
!   part ... vector of length n containing the partitioning of the graph
!   parts ... number of subgraphs 
!   distCoef ... coeficient [0,1], what weight should be used for ordering by distance           
!   
! Output:
!   perm ... permutation: original ordering -> new ordering
!   invperm ... permutation: new ordering -> original ordering 
!   ierr ... error code (0 if succesful, 1 otherwise)             
!   
! Allocations:  perm, invperm

      subroutine orderCoefMixed(ia, ja, n, part, parts, perm, invperm, distCoef, ierr)
        implicit none
!
! parameters
!
      integer :: ierr, n, parts
      integer :: ia(n+1), ja(ia(n+1)-1), part(n) 
      integer, allocatable, dimension(:) :: perm, invperm      
      double precision :: distCoef
!
! internals
!              
      integer :: i, distFromSep(n)
      integer, allocatable, dimension(:) :: permMD, invpermMD, permDist, invpermDist      
      double precision :: ordering(n)

      !
! start of orderCoefMixed
!	      
      if (distCoef > 1) then 
        write(*,*) "[orderCoefMixed] WARNING: distCoef > 1 set to 1"
        distCoef = 1
      else if (distCoef < 0) then
        write(*,*) "[orderCoefMixed] WARNING: distCoef < 0 set to 0"
        distCoef = 0
      end if 
!
! -- ordering
!                          
      call orderByMD(ia, ja, n, permMD, invpermMD, ierr)
      call orderByDistance(ia, ja, n, part, parts, permDist, invpermDist, ierr)  
      ordering = ((1 - distCoef) * permMD) + (distCoef * permDist)     
      deallocate(permMD, invpermMD, permDist, invpermDist) 
!      
! -- fill in invperm and perm using sorted order values
!                  
      allocate(perm(n), invperm(n), stat=ierr)
      call MRGRNK (ordering, invperm)      
      do i = 1, n
        perm(invperm(i)) = i
      end do       
!
! end of orderCoefMixed
!  
      end subroutine orderCoefMixed
        
!--------------------------------------------------------------------  
! subroutine partOrdering
! (c) Vladislav Matus
! last edit: 02. 11. 2018  
!
! Purpose: 
!   Take ordering of the whole graph and project it on ordering of parts.       
!   
! Input:
!   ordperm, invordperm ... ordering of the original graph
!   n ... number of vertices of the original graph  
!   np ... vector of sizes of the submatrices  
!   part ... vector of length n containing the partitioning of the graph
!   parts ... number of subgraphs 
!   
! Output:
!   ordpermp, invordperp ... orderings of the subgraphs
!   ierr ... error code (0 if succesful, 1 otherwise)   
!   
! Allocations:
!   ordpermp%vectors, ordpermp%vectors%elements(1..parts+1)
!   invordpermp%vectors, invordpermp%vectors%elements(1..parts+1)

      subroutine partOrdering(ordperm, invordperm, ordpermp, invordpermp, n, np, part, parts, ierr)
        implicit none
!
! parameters
!
      integer :: n, parts, ierr
      type(intRaggedArr) :: ordpermp, invordpermp
      integer, allocatable, dimension(:) :: ordperm, invordperm, part, np 
!
! internals
!              
      integer :: i, j, ip(parts+1)
      integer, allocatable, dimension(:) :: auxord, auxinvord

!
! start of partOrdering
!	    

      ! allocations  
      allocate(ordpermp%vectors(parts+1),stat=ierr)
      allocate(invordpermp%vectors(parts+1),stat=ierr)
      do i = 1, parts + 1
        allocate(ordpermp%vectors(i)%elements(np(i)),stat=ierr)
        allocate(invordpermp%vectors(i)%elements(np(i)),stat=ierr)
      end do

      ! code
      ip = 1
      do i = 1, n
        invordpermp%vectors(part(i))%elements(ip(part(i))) = invordperm(i)
        ip(part(i)) = ip(part(i)) + 1        
      end do

      do i = 1, parts + 1
        allocate(auxord(np(i)),stat=ierr)
        call MRGRNK(invordpermp%vectors(i)%elements, auxord) 
        do j = 1, np(i)
          invordpermp%vectors(i)%elements(auxord(j)) = j
        end do
        deallocate(auxord)
      end do   
      do i = 1, parts + 1
        do j = 1, np(i)
          ordpermp%vectors(i)%elements(invordpermp%vectors(i)%elements(j)) = j
        end do  
      end do   
!
! end of partOrdering
!  
      end subroutine partOrdering

!--------------------------------------------------------------------  
! subroutine applyOrdering
! (c) Vladislav Matus
! last edit: 10. 11. 2018  
!
! Purpose: 
!   Reorganise ia, ja and optionally part to correspond to the order of vertices given
!   
! Input:
!   ia, ja ... graph in CSR format
!   n ... number of vertices of the graph
!   nvs ... logical vector containing .true. for vertices next to separator      
!   part ... optional, partition of the graph
!   
! Output:
!   ia, ja ... newly ordred graph in CSR format    
!   nvs ... newly ordered nvs        
!   ordperm ... desired permutation of vertices
!   invordperm ... backward permutation of vertices
!   ierr ... error code (0 if succesful, 1 otherwise)   
!   part ... optional, partition of the graph
!   
! Allocations: none
!

      subroutine applyOrdering(ia, ja, n, nvs, ordperm, invordperm, ierr, part)
        implicit none
!
! parameters
!
      integer :: n, ierr
      integer, allocatable, dimension(:) :: ia, ja, ordperm, invordperm
      logical, allocatable, dimension(:) :: nvs
      integer, allocatable, dimension(:), optional :: part  
!
! internals
!              
      integer :: i, j, oldIa
      integer, allocatable, dimension(:) :: iaNew, jaNew, partNew
      logical, allocatable, dimension(:) :: nvsNew

!
! start of applyOrdering
!	      
      allocate(iaNew(n+1), jaNew(ia(n+1) - 1), nvsNew(n), stat=ierr)

      jaNew = 0

      iaNew(1) = 1
      do i = 1, n
        iaNew(i + 1) = iaNew(i) + ia(ordperm(i) + 1) - ia(ordperm(i))
        do j = 1, iaNew(i + 1) - iaNew(i)
          jaNew(iaNew(i) + j - 1) = invordperm(ja(ia(ordperm(i)) + j - 1))
        end do
      end do
      ia = iaNew
      ja = jaNew
      deallocate(iaNew, jaNew)

      do i = 1, n
        nvsNew(i) = nvs(ordperm(i))
      end do
      nvs = nvsNew
      deallocate(nvsNew)

      if(present(part))then
        allocate(partNew(n), stat=ierr)
        do i = 1, n
          partNew(i) = part(ordperm(i))
        end do
        part = partNew
        deallocate(partNew)
      end if
!
! end of applyOrdering
!  
      end subroutine applyOrdering

!--------------------------------------------------------------------   
! function countComponents
! (c) Vladislav Matus
! last edit: 10. 11. 2018  
!
! Purpose: 
!   Count number of components in graph
!   
! Input:
!   ia, ja ... graph in CSR format
!   n ... number of vertices of the graph
!   
! Returns:
!   integer :: number of components
!   
! Allocations: none
!
      
      integer function countComponents(ia, ja, n)
      implicit none
!
! parameters
!
        integer :: n
        integer, allocatable, dimension(:) :: ia, ja
!
! internals
!    
        integer :: i, j, oldLayerSize, currentLayerSize, nTouched = 0
        integer :: oldLayer(n), currentLayer(n), vertexNo
        logical :: marker(n), isOrdered(n)
!
! start of countComponents
!	 
        countComponents = 0
        isOrdered = .false.
        do while (.not. ALL(isOrdered))
          countComponents = countComponents + 1
          currentLayerSize = 1
          do i = 1, n
            if (.not. isOrdered(i)) then
                currentLayer(1) = i
                isOrdered(i) = .true.
                exit
            endif
          end do
          do while (currentLayerSize /= 0)
            oldLayer = currentLayer
            oldLayerSize = currentLayerSize
            currentLayerSize = 0
            do i = 1, oldLayerSize
              vertexNo = oldLayer(i)
              do j = ia(vertexNo), ia(vertexNo + 1) - 1
                if (.not. isOrdered(ja(j))) then                
                  isOrdered(ja(j)) = .true.
                  currentLayerSize = currentLayerSize + 1
                  currentLayer(currentLayerSize) = ja(j)                
                end if
              end do
            end do 
          end do 
        end do
!
! end of countComponents
!	 
      end function countComponents

!--------------------------------------------------------------------   
! subroutine moveVertSep
! (c) Vladislav Matus
! last edit: 15. 11. 2018  
!
! Purpose: 
!   Replaces vertex separator S by subset of adj(S) 
!   which lays in part 'direction'
!   
! Input:
!   ia, ja ... graph in CSR format
!   n ... number of vertices of the graph
!   part ... partition of the graph
!   parts ... number of parts in partition
!   direction ... index of part where vertex separator should be moving
!   
! Output:
!   part ... new partition of the graph
!   sepsize ... number of vertices of the graph
!   
! Allocations: none
!
  
      subroutine moveVertSep(ia, ja, n, part, parts, direction, sepsize)
        implicit none
!
! parameters
!
        integer, allocatable, dimension(:) :: ia, ja
        integer, allocatable, dimension(:) :: part  
        integer :: direction, parts, n, sepsize
!
! internals
!    
        integer :: i, j, p, np(parts), newPart(n)
        logical :: isConnected(parts + 1)
!
! start of moveVertSep
!	 
        sepsize = 0
        newPart = part
        do i = 1, n
          np = 0
          if (part(i) == parts + 1) then
            do j = ia(i), ia(i + 1) - 1
              if (newPart(ja(j)) /= parts + 1) then
                np(newPart(ja(j))) = np(newPart(ja(j))) + 1
              end if
              if (part(ja(j)) == direction .and. newPart(ja(j)) /= parts + 1) then
                newPart(ja(j)) = parts + 1
                sepsize = sepsize + 1
              end if
            end do
            np(direction) = -1
            if (COUNT(np > 0) == 1) then
              newPart(i) = MAXLOC(np,1)
            else
              newPart(i) = parts + 1
              sepsize = sepsize + 1
            end if
          end if
        end do
        part = newPart
        ! prune separator
        do i = 1, n
          isConnected = .false.
          if (part(i) == parts + 1) then
            isConnected(parts + 1) = .true.
            p = direction
            do j = ia(i), ia(i + 1) - 1
              isConnected(part(ja(j))) = .true.
              if(part(ja(j)) /= parts + 1) then
                p = part(ja(j))
              end if
            end do
            if (COUNT(isConnected) < 3) then
              part(i) = p
              sepsize = sepsize - 1
            end if
          end if
        end do
!
! end of moveVertSep
!	 
      end subroutine moveVertSep

!--------------------------------------------------------------------   
! subroutine orderSubgraphs
! (c) Vladislav Matus
! last edit: 17. 11. 2018  
!
! Purpose: 
!   Interface for running the appropriate ordering on original graph
!   and applying it to all the subgraphs.
!   
! Input:
!   orderingType ... 'no', 'MD', 'DIST', 'MIX', 'MIX[number]'
!   mixedCoef ... coeficient for mixed ordering      
!   ia, ja ... graph in CSR format
!   n ... number of vertices of the graph
!   part ... partition of the graph
!   parts ... number of parts in partition
!   iap, jap, ... multidimensional ragged arrays containing submatrices in CSR format
!   np ... vector of sizes of the submatrices
!   nvs ... logical multidimensional ragged array containig for each vertex info
!     if it is connected to the vertex separator      
!   
! Output:
!   iap, jap, ... multidimensional ragged arrays containing submatrices in CSR format,
!     newly ordered       
!   
! Allocations: none
!

      subroutine orderSubgraphs(orderingType, mixedCoef, ia, ja, n, part, &
        parts, iap, jap, np, nvs, hasOutput)
        implicit none
!
! parameters
!
        character(len=*) :: orderingType ! how should the matrix be ordered
        double precision :: mixedCoef ! coeficient for mixed ordering
        integer, allocatable, dimension(:) :: ia, ja, np
        integer, allocatable, dimension(:) :: part 
        type(intRaggedArr) :: iap, jap
        type(logicalRaggedArr) :: nvs
        integer :: parts, n
        logical :: hasOutput
!
! internals
!
      integer :: ierr, j
      ! -- permutations from original to ordered matrix and back
      integer, allocatable, dimension(:) :: ordperm, invordperm
      type(intRaggedArr) :: ordpermp, invordpermp

!
! start of orderSubgraphs
!
      if(TRIM(ADJUSTL(orderingType)) /= 'no') then
        select case(TRIM(ADJUSTL(orderingType)))
          case ('MD')
            if(hasOutput) write(*,*) "Ordering graph using MD ordering..."
            call orderByMD(ia, ja, n, ordperm, invordperm, ierr)
          case ('DIST')
            if(hasOutput) write(*,*) "Ordering graph by distance from separator..."
            call orderByDistance(ia, ja, n, part, parts, ordperm, invordperm, ierr)  
          case ('MIX')
            if(hasOutput) write(*,*) "Ordering graph using mixed ordering."
            call orderMixed(ia, ja, n, part, parts, ordperm, invordperm, ierr) 
          case default
            if(hasOutput) write(*,*) "Ordering graph using mixed ordering with coeficients..."
            call orderCoefMixed(ia, ja, n, part, parts, ordperm, invordperm, mixedCoef, ierr)
          end select
        ! -- Apply ordering
        call partOrdering(ordperm, invordperm, ordpermp, invordpermp, n, np, part, parts, ierr)
        do j = 1, parts
          call applyOrdering(iap%vectors(j)%elements, jap%vectors(j)%elements, np(j), &
            nvs%vectors(j)%elements, ordpermp%vectors(j)%elements, &
            invordpermp%vectors(j)%elements, ierr)
        end do
        call deallocRaggedArr(ordpermp, parts + 1, ierr)
        call deallocRaggedArr(invordpermp, parts + 1, ierr)
        deallocate(ordperm, invordperm, stat=ierr)
      end if
!
! end of orderSubgraphs
!	 
      end subroutine orderSubgraphs
 
!--------------------------------------------------------------------            
      end module myroutines90