      program test_web_dump

      include 'HCOM_DEF.FOR'      

      cmode='DATA'

      IF(CMODE.EQ.'DATA') THEN
        IS=STATUS(2)
        IF(IS.GE.1.AND.IS.LE.12800) THEN
          IOS=-1
          ITRY=0
          DO WHILE(IOS.NE.0.AND.ITRY.LT.10) !Try to open 10 times if in use
            CALL LIB$WAIT(0.1)
            ITRY=ITRY+1
            OPEN(10,FILE='[HORACE.WEB]DERIVED_DATA.TXT',IOSTAT=IOS,
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
      END       
