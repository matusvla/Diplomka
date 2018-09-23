! module myroutines90
! (c) Vladislav Matus
! last edit: 21. 09. 2018      

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

! subroutine createSubgraphs
! (c) Vladislav Matus
! last edit: 22. 09. 2018
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
!   nextToVertSep ... logical multidimensional ragged array containig for each vertex info
!     if it is connected to the vertex separator      
!   np ... vector of sizes of the submatrices
!   ierr ... error code (0 if succesful, 1 otherwise)  
!   perm ... array containing the new indices of vertices after partition
!   invperm ... multidimensional array containing the original indices
! Allocations: iap%vectors,iap%vectors%elements(1..parts+1),
!   jap%vectors, jap%vectors%elements(1..parts+1),
!   aap%vectors, aap%vectors%elements(1..parts+1),
!   np, perm, invperm%vectors, invperm%vectors%elements(1..parts+1) 

      subroutine createSubgraphs(ia, ja, aa, n, part, parts, iap, jap, aap, np, &
        nextToVertSep, perm, invperm, ierr)
        implicit none
!
! parameters
!
      integer :: n, ierr, parts
      integer :: ia(n+1),ja(ia(n+1)-1),part(n)
      double precision :: aa(ia(n+1)-1)
      type(intRaggedArr) :: iap,jap
      type(dpRaggedArr) :: aap
      type(logicalRaggedArr) :: nextToVertSep
      type(intRaggedArr) :: invperm
      integer, allocatable, dimension(:) :: np, perm
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
      allocate(nextToVertSep%vectors(parts + 1),stat=ierr)      
      ! allocate the second dimension of the multiarrays ia, ja, aa, invperm
      do i = 1, parts + 1
        allocate(iap%vectors(i)%elements(np(i)+1), stat=ierr)        
        allocate(jap%vectors(i)%elements(nep(i)), stat=ierr)
        allocate(aap%vectors(i)%elements(nep(i)), stat=ierr)
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
! last edit: 22. 09. 2018  
!
! Purpose:
!   This routine deallocates all allocated fields after using routine createSubgraphs
! Input:
!   iap, jap, aap, nvs, np, perm, invperm ... various fields which need deallocating
!   parts ... number of parts in partition
! Output:
!   ierr ... error code (0 if succesful, # of fails otherwise)  
! Allocations: none

      subroutine subgraphCleanup(iap, jap, aap, np, nvs, perm, invperm, parts, ierr)
        implicit none
!
! parameters
!
      integer :: parts,ierr            
      type(intRaggedArr) :: iap,jap
      type(dpRaggedArr) :: aap
      type(intRaggedArr) :: invperm
      type(logicalRaggedArr) :: nvs
      integer, allocatable, dimension(:) :: np,perm
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
          aap%vectors(i)%elements, invperm%vectors(i)%elements, &
          nvs%vectors(i)%elements, stat=iierr)
        ierr = ierr + iierr
      end do
      deallocate(iap%vectors, jap%vectors, aap%vectors, nvs%vectors, &
        invperm%vectors, perm, np, stat=iierr)
      ierr = ierr + iierr
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
! TODO Test after change

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
!   ia,ja,aa ... graph in CSR format
! Output:
!   iaN,jaN,aaN .. graph without loops in CSR format
! Allocations: iaN, jaN, aaN
! TODO Error handling
! TODO Test after change      

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
! function countDistance
! (c) Vladislav Matus
! last edit: 22. 09. 2018  
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
! 
! Returns: length of the longest path      


      integer function countDistance(ia, ja, n, part, parts, distFromSep)
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

        integer :: n, ia(n), minimumDegree, i

        minimumDegree = MAX_INT
        do i = 1, n          
          if (minimumDegree > ia(i+1)-ia(i)) then
            minimumDegree = ia(i+1)-ia(i)
            findMinimumDegreeIndex = i
          end if
        end do        

      end function findMinimumDegreeIndex

!-------------------------------------------------------------------- 
! function findMinimumDegreeIndices
! (c) Vladislav Matus
! last edit: 23. 09. 2018  
!
! Purpose:
!   Finds all vertices in graph with minimimal degree
! Input:
!   n ... integer, size of graph
!   ia ... pointer array of graph in CSR format
! Returns:
!   integer array, indices of vertex with minimal degree
! Allocations: findMinimumDegreeIndices

      function findMinimumDegreeIndices(ia, n)
        implicit none

        integer :: n, ia(n), minimumDegree, i, fmdiIndex
        integer, allocatable, dimension(:) :: findMinimumDegreeIndices
        minimumDegree = MAX_INT
        allocate(findMinimumDegreeIndices(n))        
        do i = 1, n          
          if (minimumDegree > ia(i+1)-ia(i)) then            
            minimumDegree = ia(i+1)-ia(i)
            findMinimumDegreeIndices(1) = i
            fmdiIndex = 2
          else if (minimumDegree == ia(i+1)-ia(i)) then
            findMinimumDegreeIndices(fmdiIndex) = i
            fmdiIndex = fmdiIndex + 1
          end if        
        end do 
                 
        call trimArr(findMinimumDegreeIndices, fmdiIndex - 1)

      end function findMinimumDegreeIndices


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

        integer :: n, ia(n), ja(ia(n+1)-1), replaceIndex, ierr
        integer :: nNew
        integer, allocatable, dimension(:) :: iaNew, jaNew

        integer :: vertexDegree
        integer :: i, j, jaNewSize, nI, iaDiff
        integer :: neighbours(ia(replaceIndex + 1) - ia(replaceIndex))
        integer, allocatable, dimension(:) :: uniqueNeighbours        

! -- allocations
        vertexDegree = ia(replaceIndex + 1) - ia(replaceIndex)
        nNew = n - 1
        jaNewSize = ia(n + 1) - 1 - vertexDegree + (vertexDegree * (vertexDegree + 1))/ 2
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
          if (j == neighbours(nI)) then ! if one of the neighbours of replaceIndex     
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
        call trimArr(jaNew, iaNew(nNew + 1) - 1)    !TODO rewrite using PACK
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
!   ordering ... the final ordering of the graph             
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
        integer :: nIn, nOut, minDegIndex
        integer, allocatable, dimension(:) :: iaIn, jaIn, iaOut, jaOut, minDegIndices
        integer :: ordering(n)           
!
! start of mixedOrdering
!	        
        call remloops(n, ia, ja, iaIn, jaIn, ierr)        
        nIn = n
        do i = 1, n - 2               
          minDegIndices = findMinimumDegreeIndices(iaIn, nIn) !TODO rewrite using MIN
          minDegIndex = minDegIndices(1)
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
        

!
! end of mixedOrdering
!  
      end subroutine mixedOrdering        

      
!-------------------------------------------------------------------- 
! subroutine orderByDistance
! (c) Vladislav Matus
! last edit: 19. 09. 2018  
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
      maxDepth = countDistance(ia, ja, n, part, parts, distFromSep)
      distFromSep = maxDepth - distFromSep
!      
! -- fill in invperm and perm using sorted order values
!                  
      allocate(perm(n), invperm(n), stat=ierr)
      call MRGRNK (distFromSep, invperm);
      do i = 1, n
        perm(invperm(i)) = i
      end do
!
! end of orderByDistance
!  
      end subroutine orderByDistance
        
!-------------------------------------------------------------------- 
! subroutine orderByMD
! (c) Vladislav Matus
! last edit: 19. 09. 2018  
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
      integer :: i, minOrdering(n)      
!
! start of orderByMD
!	    
      call minimumOrdering(ia, ja, n, minOrdering)
      write(*,'(30I3)')   
!      
! -- fill in invperm and perm using sorted order values
!                  
      allocate(perm(n), invperm(n), stat=ierr)
      call MRGRNK (minOrdering, invperm);
      do i = 1, n
        perm(invperm(i)) = i
      end do      
!
! end of orderByMD
!  
      end subroutine orderByMD
        
!-------------------------------------------------------------------- 
! subroutine orderMixed
! (c) Vladislav Matus
! last edit: 19. 09. 2018  
!
! Purpose:
!   Computes the permutation of vertices in graph to be ordered
!   Using enhanced MD algorithm, i. e. the vertex with minimal degree
!   is chosen and if there ar more vertices with the same minimal degree,
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
      call mixedOrdering(ia, ja, n, part, parts, ordering)
      write(*,'(30I3)')   
!      
! -- fill in invperm and perm using sorted order values
!                  
      allocate(perm(n), invperm(n), stat=ierr)
      call MRGRNK (ordering, invperm);
      do i = 1, n
        perm(invperm(i)) = i
      end do      
!
! end of orderMixed
!  
      end subroutine orderMixed
        
!--------------------------------------------------------------------   

!
! end of module
!      
      end module myroutines90


