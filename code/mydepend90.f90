
      module mydepend90
      
      use mydepend

      contains
! (c) sparslab module name=poisson1
!
! purpose:
!  generate a real square matrix.
!  simple 5pt discretization of 2D poisson equation.
!
! history:
!   original version for sparslab - tu - 2015/09/01.
!
      subroutine poisson1(nx,n,ia,ja,aa,info)
!
! parameters
!
      implicit none
      integer nx,n,info
      integer, allocatable, dimension(:) :: ia,ja
      double precision, allocatable, dimension(:) :: aa
!
! internals
!
      integer nja,ierr
      double precision one,four
      parameter(one=1.0d0,four=4.0d0)
!
! start of poisson1
!
!  -- laplace on 2d square grid, dirichlet bc
!
      n=nx*nx
      nja=(4*2+(nx-2)*(nx-2)*4+4*(nx-2)*3)+n   
      allocate(ia(n+1),stat=ierr)         
      allocate(ja(nja),aa(nja),stat=ierr)          
      call grid5pt(nx,n,ia,ja)
      call val5pt(n,ia,ja,aa,nx,nx,nx, &
        four,-one,four,four,0,one)
!
!  -- return
!
      return
!
! end of poisson1
!
      end subroutine poisson1





! (c) sparslab module name=ommatl4
!
! purpose:
!  output matrix into plain ascii file loadable from matlab.
!  filtrate the entries.
!
! history:
!   original version for sparslab - tu - 26/1/1998
!
! parameters:
!
      subroutine ommatl4(n,ia,ja,aa,mformat)
!
! parameters
!
      implicit none
      integer n,mformat
      integer ia(*),ja(*)
      double precision drfl
      double precision aa(*)
!
! internals
!
      integer i,j,k,jstrt,jstop
      integer matlab_unit
      logical short
      double precision temp
!
!
! start of ommatl4
!
      matlab_unit=91
      drfl=0.0d0
      short=.false.
!
!
!  -- open output file
!
      open(unit=matlab_unit,file='matrix')
!
!  -- output the matrix
!
      jstop=0
      do i=1,n
        jstrt=jstop+1
        jstop=ia(i+1)-1
        do j=jstrt,jstop
          k=ja(j)
          temp=aa(j)
          if(abs(temp).gt.drfl) then
          if(short) then
            write(matlab_unit,1) i,k,temp
          else
            write(matlab_unit,2) i,k,temp
          end if
          end if
        end do
      end do
!
!  -- add the other triangle
!  -- for symmetric format
!
      if(mformat.eq.111) then
        jstop=0
        do i=1,n
          jstrt=jstop+1
          jstop=ia(i+1)-1
          do j=jstrt,jstop
            k=ja(j)
            temp=aa(j)
            if(abs(temp).gt.drfl) then
            if(k.ne.i) then
              if(short) then
                write(matlab_unit,1) k,i,temp
              else
                write(matlab_unit,2) k,i,temp
              end if
            end if
            end if
          end do
        end do
      end if
!
!  -- close the output file
!
      close(matlab_unit)
!
! formats
 1   format(i7,i7,1x,g10.4)
 2   format(i7,i7,1x,g20.10)
!
!  -- return
!
      return
!
! end of ommatl4
!
      end subroutine ommatl4

      
      
      
! (c) sparslab module name=imhb2
!
! purpose:
!  read a sparse matrix in harwell-boeing format.
!
! history:
!   original version for sparslab - tu - 09/04/2001.
!
      subroutine imhb2(iunit,m,n,mformat,ia,ja,aa,info)
!
! parameters
!
      integer iunit,n,m,mformat,info
      integer, allocatable, dimension(:) :: ia,ja
      double precision, allocatable, dimension(:) :: aa
!
! internals
!
      character title*72,key*8,mxtype*3
      character ptrfmt*16,indfmt*16,valfmt*20,rhsfmt*20,rhstyp*3
      integer totcrd,ptrcrd,indcrd,valcrd,rhscrd
      integer neltvl,nrhs,nja,njas
      integer i,ierr
!
! read header
!
! -- read matrix dimensions m,n
! -- read also nja/nz but do not use it now
!
      read(iunit,1000)title,key,totcrd,ptrcrd,indcrd,valcrd,rhscrd, &
        mxtype,m,n,nja,neltvl,ptrfmt,indfmt,valfmt,rhsfmt
      if(mxtype(2:2).eq.'S'.or.mxtype(2:2).eq.'s') then
        mformat=111
      else
        mformat=12
      end if
!
!  -- read number of right-hand sides
!
      if(rhscrd.gt.0) read(iunit,1001) rhstyp,nrhs
!
!  -- read pointers
!
      allocate(ia(max(m,n)+1),stat=ierr)
      if(ierr.ne.0) go to 400
      read(iunit,ptrfmt) (ia(i),i=1,n+1)
      if(nja.ne.ia(n+1)-1) then
        stop ' internal error in harwell-boeing matrix'
      end if
!
!  -- read indices, values and close the matrix unit
!
      njas=nja
      if(mformat.eq.111) then
        njas=2*nja-n
      end if
      allocate(ja(njas+n),aa(njas+n),stat=ierr)
      if(ierr.ne.0) go to 400
      read(iunit,indfmt) (ja(i),i=1,nja)
      read(iunit,valfmt) (aa(i),i=1,nja)
!
!  -- close the matrix unit
!
      close(iunit)
!
!  -- return
!
      return
!
! -- formats
!
 1000 format(a72,a8 / 5i14 / a3, 11x, 4i14 / 2a16, 2a20)
 1001 format (a3,11x,2i14)
!
  400 continue
      info=8
      return
!
! end of imhb2
!
      end subroutine imhb2



!
! (c) sparslab module name=eltree2
!
! purpose:
!   construct an elimination tree / forest.
!   no permutations.
!
! history:
!   original version for sparslab - tu - 16/01/04.
!
! parameters:
!
  subroutine eltree2(n,ia,ja,parent,ancstr)
!
! parameters
!
      implicit none
      integer n
      integer parent(*),ia(*),ja(*),ancstr(*)
!
! internals
!
      integer i,jj,k,k3,jstrt,jstop
!
!  -- start of eltree2
!
      do i=1,n
        parent(i)=0
        ancstr(i)=0
      end do
!
      do i=1,n
        jstrt=ia(i)
        jstop=ia(i+1)-1
        do jj=jstrt,jstop
          k=ja(jj)
          if(k.lt.i) then
            k3=ancstr(k)
            do while (k3.gt.0.and.k3.ne.i)
! 300        if(k3.gt.0.and.k3.ne.i) then
              ancstr(k)=i
              k=k3
              k3=ancstr(k)
!              go to 300
!            end if
            end do
            if(k3.eq.0) then
              parent(k)=i
              ancstr(k)=i
            end if
          end if
        end do
      end do
!
      return
!
!  -- end of eltree2
!
      end subroutine eltree2
!     
      end module mydepend90
