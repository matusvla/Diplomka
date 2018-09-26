
      module mydepend

      contains
! (c) sparslab module name=grid5pt
!
! purpose:
!   get matrix from the 5pt stencil.
!
! history:
!   original version for sparslab - tu - 18/7/1998.
!
! parameters:
!
      subroutine grid5pt(size,n,ia,ja)
!
      integer size,n
      integer ia(*),ja(*)
!
! internals
!
      integer i,j,k,next
!
! start of grid5pt
!
      next=1
      k=1
      do i=1,size
        do j=1,size
          ia(k)=next
          if(i.ge.2) then
            ja(next)=k-size
            next=next+1
          end if
          if(j.ge.2) then
            ja(next)=k-1
            next=next+1
          end if
          if(j.lt.size) then
            ja(next)=k+1
            next=next+1
          end if
          if(i.lt.size) then
            ja(next)=k+size
            next=next+1
          end if
          k=k+1
        end do
      end do
      ia(k)=next
      n=size*size
!
!  -- return
!
      return
!
! end of grid5pt
!
      end subroutine grid5pt


! (c) sparslab module name=val5pt
!
! purpose:
!   put numerical values into the matrix with 5pt stencil.
!
! history:
!   original version for sparslab - tu - 18/7/1998.
!
! parameters:
!
      subroutine val5pt(n,ia,ja,aa,nx,ny,nz,
     *  diagval,ndiagval,cornerval,boundval,antyp,aniso)
!
! parameters
!
      integer n
      integer nx,ny,nz
      integer ia(*),ja(*)
      double precision diagval,ndiagval,cornerval,boundval
      integer antyp
      double precision aniso
      double precision aa(*)
!
! internals
!
      integer i,j,k,ind,jstrt,jstop,noffdiag,ideg
      double precision temp
!
! start of val5pt
!
      noffdiag=ia(n+1)-1
      ind=noffdiag+n
      do i=noffdiag,1,-1
        ja(ind)=ja(i)
        ind=ind-1
      end do
      temp=-aniso*ndiagval
      nx=nx
      ny=ny
      nz=nz
!
      ind=1
      jstop=n
      do i=1,n
        ia(i)=ind
        jstrt=jstop+1
        jstop=ia(i+1)-1+n
        ideg=jstop-jstrt+1
        do j=jstrt,jstop
          k=ja(j)
          if(k.lt.i) then
            ja(ind)=ja(j)
            if(antyp.eq.0) then
              aa(ind)=ndiagval
            elseif(antyp.eq.1) then
              if(k.eq.(i-1)) then
                aa(ind)=aniso*ndiagval
              else
                aa(ind)=ndiagval
              end if
            elseif(antyp.eq.2) then
              if(k.eq.(i-1)) then
                aa(ind)=ndiagval
              else
                aa(ind)=aniso*ndiagval
              end if
            end if
            ind=ind+1
          else
            jstrt=j
            exit
          end if
        end do
        ja(ind)=i
        if(antyp.eq.0) then
!
!      -- no anisotropy
!
          if(ideg.eq.2) then
            aa(ind)=cornerval
          elseif(ideg.eq.3) then
            aa(ind)=boundval
          elseif(ideg.eq.4) then
            aa(ind)=diagval
          end if
        elseif(antyp.eq.1.or.antyp.eq.2) then
!
!      -- anisotropy in x or y
!
          if(ideg.eq.2) then
            aa(ind)=cornerval+temp+ndiagval
          elseif(ideg.eq.3) then
            aa(ind)=boundval+2*(temp+ndiagval)
          elseif(ideg.eq.4) then
            aa(ind)=diagval+3*(temp+ndiagval)
          end if
        elseif(antyp.eq.3) then
        end if
        if(i.eq.1.and.(cornerval.ne.diagval.or.
     *    boundval.ne.diagval)) aa(ind)=aa(ind)-ndiagval
        if(i.eq.n) jstrt=jstop+1
        ind=ind+1
        do j=jstrt,jstop
          k=ja(j)
          ja(ind)=ja(j)
          if(antyp.eq.0) then
            aa(ind)=ndiagval
          elseif(antyp.eq.1) then
            if(k.eq.(i+1)) then
              aa(ind)=aniso*ndiagval
            else
              aa(ind)=ndiagval
            end if
          elseif(antyp.eq.2) then
            if(k.eq.(i+1)) then
              aa(ind)=ndiagval
            else
              aa(ind)=aniso*ndiagval
            end if
          end if
          ind=ind+1
        end do
      end do
      ia(n+1)=noffdiag+n+1
!
!  -- return
!
      return
!
! end of val5pt
!
      end subroutine val5pt
      
      
      
! (c) sparslab module name=symtr7
!
! purpose :
!  symmetrize a matrix in the css format.
!
! history:
!   original version for sparslab - tu - 24/8/1997
!
! parameters:
!   ii  n dimension of the uncompressed matrix.
!   ii  ncompr dimension of the compressed matrix.
!   ou  ia(n+1)/ja(ia(n+1)-1)/a(ia(n+1)-1)
!         matrix in compressed sparse format.
!         on output it is symmetrized.
!   ia  pd(n)  an auxiliary vector used to store pointers
!         to diagonal elements.
!   ia  wn01(n+1)  an auxiliary vector.
!
      subroutine symtr7(n,ia,ja,a,pd,wn01)
!
! parameters
!
      integer n
      integer ia(*),pd(*),ja(*),wn01(*)
      double precision a(*)
!
! internals
!
      integer i,j,k,ind,istrt,istop,jstrt,jstop
!
! start of symtr7
!
!  -- put (upper) triangular information to an auxiliary array.
!
      jstop=ia(1)
      do i=1,n
        jstrt=jstop
        jstop=ia(i+1)
        wn01(i)=jstop-jstrt
      end do
!
!  -- add (lower) information of the transposed triangle
!  -- to the auxiliary array.
!
      jstop=ia(1)-1
      do i=1,n
        jstrt=jstop+2
        jstop=ia(i+1)-1
        do j=jstrt,jstop
          k=ja(j)
          wn01(k)=wn01(k)+1
        end do
      end do
!
!  -- get pointers of the wide structure
!  -- wn01(i) points at the end of the row i
!
      do i=2,n
        wn01(i)=wn01(i)+wn01(i-1)
      end do
!
!  -- shift the (upper) triangular rows
!  -- set pointers to diagonal elements
!
      pd(1)=1
      jstop=ia(n+1)
      do i=n,1,-1
        ind=wn01(i)
        pd(i+1)=ind+1
        jstrt=jstop-1
        jstop=ia(i)
        do j=jstrt,jstop,-1
          ja(ind)=ja(j)
          a(ind)=a(j)
          ind=ind-1
        end do
      end do
!
!  -- form the transposed (lower) triangular part
!
      istop=ia(1)
      do i=1,n
        istrt=istop
        istop=ia(i+1)
        jstop=wn01(i)
        jstrt=jstop+istrt-istop+2
        do j=jstrt,jstop
          k=ja(j)
          ind=pd(k)
          ja(ind)=i
          a(ind)=a(j)
          pd(k)=ind+1
        end do
      end do
!
!  -- get new pointers
!
      do i=1,n
        ia(i+1)=wn01(i)+1
      end do
!
      return
!
! end of symtr7
!
      end

      end module mydepend

