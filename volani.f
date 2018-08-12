integer :: nfull (dimenze matice), mformat
iata,jata: matice v CSR formatu (asi je to symetricka cast)

n=nfull
integer: perm(n),invp(n), colcnt(n), ierr, info
mformat = 111


          allocate(colcnt(nfull),stat=ierr)
          call chfill2(nfull,iata,jata,mformat,colcnt,chsize,info)
          if (debug) write(*,*) 'chsize',chsize
!
          allocate(perm(nfull),invp(nfull),ib(nfull+1),jb(2*ne),ic(nfull+1),jc(2*ne))
!
!      -- mmd
!
          call mmds(nfull,iata,jata,mformat,perm,0,info)
          call uxvsip(nfull,perm,invp)
          call copcs3(nfull,iata,jata,1,ib,jb,1)
          call spermfm6(nfull,ib,jb,ic,jc,perm,invp)
          call srtcs2(n,ib,jb)
          call chfill2(nfull,ib,jb,mformat,colcnt,chsizemmd,info)