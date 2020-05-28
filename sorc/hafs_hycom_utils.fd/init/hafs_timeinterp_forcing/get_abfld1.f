      subroutine get_abfld1(iunit,ip,field1,idm_,jdm_,
     $     label1,level1,fln_name,ihead,time)
      use mod_xc  ! HYCOM communication interface
      use mod_za  ! HYCOM io interface
      implicit none
      integer   level1,idm_,jdm_
      character(*) fln_name
      character(*) label1
      real      field1(idm_,jdm_)
      integer   ip(idm_,jdm_)
      integer   lgth,nset,i,lgth1,lgth2,iunit,level
      integer   ios,nrec
      integer   ihead,ltime
      character preambl(5)*79,cline*79
      character separator*1
      real      dum1,dum2,dum3,hmina,hmaxa,hminb,hmaxb,time
      save      separator,lgth1
      lp=6
cir   iunit=900
      if ( ihead.gt.0 ) then
        lgth = len_trim(fln_name)
        if(fln_name(lgth-1:lgth).eq.".b".or.
     $       fln_name(lgth-1:lgth).eq.".a")lgth=lgth-2
        print *,lgth,fln_name(1:lgth)

        lgth1=len_trim(label1)
        separator=label1(lgth1:lgth1)
        if(separator.eq.':') then
          write(lp,*)' restart formatted b-file.'
        elseif(separator.eq.'=') then
          write(lp,*)'archive formatted b-file.'
        else
          write(lp,*)'separator = ' ,separator,' is not supported.'
c          call flush_(lp)
          stop
        endif
c        call flush_(lp)
        call zaiopf(fln_name(1:lgth)//'.a', 'old', iunit)
        open (unit=iunit,file=fln_name(1:lgth)//'.b',
     &      status='old', action='read')
cjl         read (iunit,'(a79)') preambl
cjl         call preambl_print(preambl,lp)

c
c -- skip the rest of the heading
c
         do nrec=1,ihead
            read (iunit,'(a79)') cline      
         enddo
         ihead=0
      endif
c
c ---   find labels label1 
      nrec   = 0
      nset=0
      do                        ! infinate loop, with exit at end
        nrec   = nrec + 1
        read(iunit,'(a79)',iostat=ios)cline
        if     (ios.ne.0) then
          write(lp,*)
          write(lp,*) 'error in get_abfld - hit end of input'
          write(lp,*) 'nrec = ',nrec
          write(lp,*)
          stop '(get_abfld)'
       endif
       i = index(cline,'=')
       if ( cline(1:lgth1) .eq. label1(1:lgth1)) then
         if (cline(lgth1+1:lgth1+8).eq.' range =') then
            read(cline(i+1:),*)hminb,hmaxb
            level=0
         else
         if (separator.eq.'=') then
            read (cline(i+1:),*) dum1,dum2,level,dum3,hminb,hmaxb
         elseif(separator.eq.':') then
cjl           read (cline(i+1:),*) level,ltime,hminb,hmaxb
          read (cline(i+1:),*) time,level,hminb,hmaxb
         endif       
         endif
         if ( level.eq.level1) then
             nset=nset+1
             call zaiord(field1,ip,.false., hmina,hmaxa,
     &              iunit)
c
             if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &            abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
                write(lp,
     $             '(/ a / a,i3 / a / a,1p3e14.6 / a,1p3e14.6 /)')
     &             'error - .a and .b files not consistent:',
     &             'iunit = ',iunit,
     &             cline,
     &             '.a,.b min = ',hmina,hminb,hmina-hminb,
     &             '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
                stop '(get_abfld)'
             else
                write(lp,'(a79)')cline
                write(lp,*) field1(1,1)
             endif
          else
             call zaiosk(iunit)
          endif
       else
          call zaiosk(iunit)
       endif
       if     (nset.eq.1) then
          exit
       endif
      enddo                     ! infinate loop, with exit above
      return
      end

      subroutine preambl_print(preambl,lp)
      implicit none
c
      character preambl(5)*79
      integer lp
c
c --- print non-blank lines of preambl
c
      integer i
c
      write(lp,*)
      do i= 1,5
        if     (len_trim(preambl(i)).ne.0) then
          write(lp,'(a)') trim(preambl(i))
        endif
      enddo
      return
      end
