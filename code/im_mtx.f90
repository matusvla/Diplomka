
! (c) sparslab module name=im_mtx
!
! purpose:
!  read a sparse matrix in harwell-boeing format.
!
! history:
!   original version for sparslab - tu - 09/04/2001.
!
      subroutine im_mtx(iunit,m,n,mformat,ia,ja,aa,info)
!
! parameters
!
      implicit none
      integer iunit,n,m,mformat,info
      integer, allocatable, dimension(:) :: ia,ja
      double precision, allocatable, dimension(:) :: aa
!
! internals
!
      integer i,ierr,count
      integer, allocatable, dimension(:) :: wn01
      double precision :: droptol
!
!
!    -- rectangular coordinate matlab-like format
!
!    -- 1/ count nonzeros, find dimension
!
        count=1 ! additional space
        n=0
        m=0
        count=0
        nheaders=0
        droptol=tiny(droptol)
 6651   continue
        read(iunit,*,err=6661,end=6681) i,j,temp
        count=count+1
        m=max(m,i)
        n=max(n,j)
!        if(abs(temp).le.droptol.and.i.ne.j) count=count-1
        go to 6651
 6661   continue
        if(count.eq.0) then
          nheaders=nheaders+1
          go to 6651
        end if
 6681   continue
        if(count.eq.0) then
          info=1034
          return
        end if
        count=count-1 ! first readable line should not be counted
        nheaders=nheaders+1 ! need to add the line with dimensions
        allocate(ia(m+1),stat=ierr)
        if(ierr.ne.0) call serr(0,2,'sparslab','immat3', &
          ' allocation error',8,2)
        allocate(ja(count+m),aa(count+m),stat=ierr)
!
!    -- 2/ get column counts. no assumption about row/column
!    --    orderings.
!
        rewind(iunit)
        do i=1,m+1
          ia(i)=0
        end do
        do i=1,nheaders
          read(iunit,*)
        end do
 6631   continue
        read(iunit,*,err=6641,end=6711) i,j,temp
!
!    -- correction for the matrixmarket format
!
!        if(i.eq.n.and.j.eq.n.and.int(temp).eq.count-1) go to 6631
        ia(i+1)=ia(i+1)+1
        go to 6631
 6641   continue
        if(count.eq.0) go to 6631
 6711   continue
!
!    -- 3/ get partial sums
!
        allocate(wn01(m+1),stat=ierr)
        if(ierr.ne.0) call serr(0,2,'sparslab','immat3',' allocation error',8,2)
        ia(1)=1
        wn01(1)=1
        do i=2,m+1
          ia(i)=ia(i)+ia(i-1)
          wn01(i)=ia(i)
        end do
!
!    -- 4/ read the matrix.
!
        rewind(iunit)
        do i=1,nheaders
          read(iunit,*)
        end do
        do i=1,count
          read(iunit,*) ii,j,temp
!        if(.not.(ii.eq.n.and.j.eq.n.and.int(temp).eq.count-1)) then
!          if(abs(temp).gt.droptol.or.j.eq.ii) then
            ind=wn01(ii)
            ja(ind)=j
            aa(ind)=temp
            wn01(ii)=wn01(ii)+1
!          end if
!          end if
        end do
        mformat=11
        nja=ia(m+1)-1
        njas=ia(m+1)-1
        call srtcs1(m,ia,ja,aa)
        deallocate(wn01)
        close(iunit)
        go to 400
!        
      info=0
      return
 400  continue
      info=8
!
      end subroutine im_mtx

