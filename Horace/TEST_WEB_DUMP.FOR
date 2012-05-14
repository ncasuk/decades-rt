
      program test_web_dump

      call web_dump1('INIT')
      call web_dump1('DATA')

      END


      subroutine web_dump1(cmode)

      INTEGER*4 IPARA(512),IS,I,IOS,IPCNT,IP,ITRY
      CHARACTER CNAME(512)*30,CUNIT(512)*16,CLINE*80,CMODE*4
      INCLUDE 'HCOM_DEF.FOR'
      IF(CMODE.EQ.'INIT') THEN
        OPEN(10,FILE='PARANO.TXT',STATUS='OLD',READONLY)
        DO I=1,7
          READ(10,'(A)',IOSTAT=IOS) CLINE
        END DO
        IPCNT=0
        DO WHILE(IOS.EQ.0)
          READ(CLINE(1:3),'(I3)',IOSTAT=IOS) IP
          IP=IP-512
          IF(IP.GE.1.AND.IP.LE.512.AND.IOS.EQ.0) THEN
            IPCNT=IPCNT+1
            IPARA(IPCNT)=IP+512
            CNAME(IPCNT)=CLINE(26:53)
            CUNIT(IPCNT)=CLINE(55:72)
          END IF
        READ(10,'(A)',IOSTAT=IOS) CLINE
        END DO
        CLOSE(10)     

        IOS=-1
        ITRY=0
        DO WHILE(IOS.NE.0.AND.ITRY.LT.10) !Try to open 10 times if in use
          CALL LIB$WAIT(0.1)
          ITRY=ITRY+1
          OPEN(10,FILE='[HORACE]DERIVED_STUFF.TXT',STATUS='UNKNOWN',
     &        CARRIAGECONTROL='LIST',SHARED,IOSTAT=IOS)
        END DO
        IF(IOS.EQ.0) THEN
          DO IP=1,IPCNT
            WRITE(10,'(I4.4,A)',IOSTAT=IOS) IPARA(IP),
     &        ' '//CNAME(IP)//' '//CUNIT(IP)
          END DO
          CLOSE(10,IOSTAT=IOS)
        END IF
      END IF

      IF(CMODE.EQ.'DATA') THEN
        IS=STATUS(2)
        IF(IS.GE.1.AND.IS.LE.12800) THEN
          IOS=-1
          ITRY=0
          DO WHILE(IOS.NE.0.AND.ITRY.LT.10) !Try to open 10 times if in use
            CALL LIB$WAIT(0.1)
            ITRY=ITRY+1
            OPEN(10,FILE='[HORACE]DERIVED_STUFF.TXT',IOSTAT=IOS,
     &          STATUS='UNKNOWN',CARRIAGECONTROL='LIST',SHARED)
          END DO
          IF(IOS.EQ.0) THEN
            DO IP=1,IPCNT
              WRITE(10,'(I4.4,X,1PE12.5E2)') 
     &            IPARA(IP),DERIVE(IS,IPARA(IP)-512)
            END DO
            CLOSE(10)
          END IF
        END IF
      END IF
      RETURN
      END
