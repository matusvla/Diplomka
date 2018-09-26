
! (c) sparslab module name=svrev
!
! purpose:
!   reverse order of elements in a string.
!
! history:
!   original version for sparslab - tu - 30/12/1997
!
! parameters:
!   ii  slen  length of the string.
!   cu  sx(slen) input string.
!
      subroutine svrev(slen,sx)
!
! parameters
!
      integer slen
      integer len_output_string_max
      parameter(len_output_string_max=100)
      character*(len_output_string_max) sx
!
! locals
      integer i,jend
      character ctemp
!
! start of svrev
!
      jend=slen
      do i=1,slen/2
        ctemp=sx(i:i)
        sx(i:i)=sx(jend:jend)
        sx(jend:jend)=ctemp
        jend=jend-1
      end do
!
      return
!
! end of svrev
!
      end subroutine svrev


