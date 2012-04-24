C*******************************************************************************
C
C
      PROGRAM CONVERT_SUMM
      integer*4 nevents,ievents,i,j
      character cfno*4,cline*80,h1*4,h2*4
      character*200 cevents(100)
      logical extracomment

      WRITE(*)'Flight Number'
      READ(5,'(A)')CFNO
      WRITE(*)CFNO
         CALL loadevents(cfno,cevents,nevents,ievents)
         open(unit=23,file=cfno//'_summ.txt',status='NEW',
     &        recordtype='STREAM_LF',carriagecontrol='NONE')
          write(23,'(A)')('             FLIGHT SUMMARY '
     &  )
          write(23,'(A)')'Flight No '//cfno
          write(23,'(A)')'Date:'
          write(23,'(A)')'Project:'
          write(23,'(A)')'Location:'
          write(23,'(A)')' '
          write(23,'(A)')'Start   End'
          write(23,'(A)')'Time    Time     Event               '//
     &'Height (s)        Hdg Comments'
          write(23,'(A)')'----    ----     -----               '//
     &'----------        --- --------'
          cline=' '
          cline(7:8)='  '
          cline(15:17)='   '
          do i=1,nevents
            cline(1:6)=cevents(i)(1:2)//cevents(i)(4:5)//cevents(i)(7:8)
            cline(9:14)=cevents(i)(32:33)//
     & cevents(i)(35:36)//cevents(i)(38:39)
            cline(18:37)=cevents(i)(64:83)
            h1=cevents(i)(14:17)
            h2=cevents(i)(45:48)
            cline(38:41)=h1
            if((h1.NE.h2).AND.(h2(4:4).NE.' ')) then
               cline(42:52)=' - '//h2//' kft'
            else 
               cline(42:52)=' kft       '
            endif
            cline(56:59)=cevents(i)(10:12)//' '
            cline(60:80)=cevents(i)(87:107)
            write(23,'(A)')cline
            J=166
            extracomment=.FALSE.
            DO WHILE ((J.GT.108).AND.(.NOT.extracomment))
              IF((cevents(i)(j:j).NE.char(0)).AND.
     & (cevents(i)(j:j).NE.' '))extracomment=.TRUE.
              J=J-1
            ENDDO
            IF(extracomment)write(23,'(A)')'                        '
     &//'                                   '//cevents(i)(108:J+1)
C            write(23,*)(cevents(i)(10:12))
C            write(23,*)(cevents(i)(14:17)//'kft')
C            write(23,*)(cevents(i)(19:23))
C            write(23,*)(cevents(i)(25:30))
C            write(23,*)(cevents(i)(41:43))
C            write(23,*)(cevents(i)(45:48))
C            if(cevents(i)(48:48).NE.' ')write(23,*)('kft')
C            write(23,*)(cevents(i)(50:54))
C            write(23,*)(cevents(i)(56:61))
C            write(23,*)(cevents(i)(87:166))
          enddo
          close(23)
       END
C*******************************************************************************
C
C     SUBROUTINE loadevents(CFNO,C,N,I)
C    
C     READ in the events from fltsumm_****.txt where **** is
C          the flight number (CFNO) 
C          C list of events 
C          N is the number of events
C          I is the current event
C
      SUBROUTINE loadevents(cfno,c,n,i)
         integer*4 n,i,istat,ii
         character*200 c(100)
         character*4 cfno,cdum

         open(11,file='fltsumm_'//cfno//'.txt',status='old',
     &       iostat=istat)
         n=0
         i=0
         if(istat.EQ.0) THEN
            read(11,'(a)')cdum
            read(11,'(I4.4,X,I4.4)')n,i
            do ii=1,n
              read(11,'(a)')c(ii)                                  
            enddo
         endif
         close(11)
      RETURN
      END
