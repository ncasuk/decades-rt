C
C HOR_HODO
C HOR_HODO1
C HOR_HODO_BACKG
C
C*******************************************************************************
      SUBROUTINE HOR_HODO
C
C Get the plotting arguments, then call hodo.
C
      IMPLICIT  NONE
      INTEGER*4 ILIST(3),IKEY,IFREQ

20    CALL GET_FREQ_HODO(IFREQ)
      IF(IFREQ.EQ.-1) RETURN
      ILIST(1)=3
      ILIST(2)=11
      ILIST(3)=IFREQ
      CALL UPDT_STATUS(ILIST)
      CALL HOR_HODO1(ILIST,IKEY)
      IF(CHAR(IKEY).EQ.'A') RETURN
      IF(CHAR(IKEY).EQ.'C') GOTO 20
      END
C*******************************************************************************
      SUBROUTINE HOR_HODO1(ILIST,IKEY)
C
C Plots U vs V on a hodograph background, at user selected frequency of  
c 3,6,9, or 12 seconds, plotting past data if required for 30,60,90,120 mins.
C
C External data:
C
C DERIVE(12800,196) R*4  Passed  HOR_CALCS    A circular buffer of 196 
C                                            derived parameters,calculated once
C                                            every third second.(see PARANO.TXT)
C
C SPECIAL(12800,10) I*2  Passed HOR_CALCS     A circular buffer of 10 parameters
C                                            (time,event,flags),calculated once
C                                            every third second.
C Subroutines called:
C
C HOR_HODO_BACKG     This file               Draws background hodograph
C
      IMPLICIT  NONE
      INTEGER*4 IUOFF,IVOFF,IUVPIX,ILIST(3),IKEY,IS,IFREQ,IPARAX,IPARAY,
     -     ISET,IOS,ISAVEX,ISAVEY,ITIM,IX,IY,IP,IC,IXSHFT,
     -     ITYCNT,IFSTXY
      INTEGER*2 IBLK1(500),IBLK2(500)
      REAL*4    UVTICVAL,UVINC,UVFACT,RRVAL,RRU,RRV
      CHARACTER CDATA*8,CUNIT1*7
      COMMON    /UVPLOT/ IPARAY,IPARAX,UVTICVAL,UVINC,UVFACT,IUOFF,
     -    IUVPIX,IVOFF,CUNIT1
      INCLUDE 'DCOM_DEF.FOR'
      INCLUDE 'HCOM_DEF.FOR'
C
C Start displaying-write heads on first and second time through only
C
      IKEY=ICHAR('A')                  !Default return
      IFREQ=ILIST(3)
      IF(STATUS(2).LT.1.OR.STATUS(2).GT.12800) RETURN
C
C Write fixed parts of the display to the access page the first time the
C routine is called.
C
      CALL BBK(IBLK1,IBLK2,500)        !start double blocking
      CALL SEL(0,IBLACK,IBLACK)        !black
      CALL RUB                         !clear display page
      CALL HEAD_SHELL
      CALL FOOT('SELECT',' ','FREQ',' ',' ','PRINT',' ',' ')
      IPARAY=55                        !U
      IPARAX=56                        !V
      UVTICVAL=-70.0                   !Bottom label
      UVINC=10.00                      !Circle spacing - m/s
      UVFACT=2.70                      !Converts m/s to pixels
      IUVPIX=27                        !Circle spacing - pixels
      IUOFF=307                        !Define centre
      IVOFF=380                        !Define centre
      CALL HOR_HODO_BACKG(IKEY)        !draw graph blank
      IF(IKEY.NE.0) GOTO 5000
      ISET=1                           !set for first points
      IF(IFREQ.EQ.5) GOTO 5010         !plot only current data
C
C Draw initial graph using past data
C
      IFSTXY=1
      ITYCNT=0
      IXSHFT=0
      CALL SEL(0,IFOREG,-1)            !cyan/additive
      CALL SCA(10,10,0,2)              !small letters for times
      IC=STATUS(3)
      IP=STATUS(2)
      IS=IP
      DO WHILE(IC.GT.0)
        CALL CHECK_KEY('AC',IKEY)
        IF(IKEY.NE.0) GOTO 5000
        DO WHILE(MOD(NINT(RSECS(IP)),IFREQ*3).NE.0.AND.IC.GT.0)
          IC=IC-1
          IP=IP-1
          IF(IP.LE.0) IP=IP+12800
        END DO
        RRU=DERIVE(IP,IPARAY)
        RRV=DERIVE(IP,IPARAX)
        RRVAL=SQRT(RRU*RRU+RRV*RRV)
        IY=IFIX(IUOFF-UVFACT*RRU)
        IX=IFIX(IVOFF+UVFACT*RRV)
        IF(RRVAL.LE.70..AND.SPECIAL(IP,9).EQ.1) CALL BIT(IX,IY) !draw dot only
        IC=IC-IFREQ
        IP=IP-IFREQ
        IF(IP.LE.0) IP=IP+12800         !recycle when required
      END DO
5010  CALL SEL(0,IFOREG,-1)
      CALL EBK                         !empty last block
      GOTO 110

5000  CALL SEL(0,IFOREG,-1)
      CALL EBK                         !empty last block
      RETURN
C
C Display the latest data
C
100   IS=STATUS(2)
      IF(IS.GT.12800) IS=IS-12800
      CALL HEAD_UPDATE
      CALL SCA(13,13,0,3)              !7*9 double
      WRITE(CDATA,'(F8.2)',IOSTAT=IOS) DERIVE(IS,IPARAY) !get data value
      CALL SEL(0,IFOREG,IBLACK)        !cyan/black
      CALL ALP(100,120,CDATA)          !print data
      CALL SEL(0,IFOREG,-1)
      WRITE(CDATA,'(F8.2)',IOSTAT=IOS) DERIVE(IS,IPARAX) !get data value
      CALL SEL(0,IFOREG,IBLACK)        !cyan/black
      CALL ALP(610,120,CDATA)          !print data
      CALL SEL(0,IFOREG,-1)
C
C Plot the data
C
      ITIM=NINT(RSECS(IS))
      IF(IFREQ.EQ.5) IFREQ=1           !no past data -plot every 3 secs
      IF(MOD(ITIM,IFREQ*3).EQ.0) THEN  !plot every 3,6,9 or 12 s
        RRU=DERIVE(IS,IPARAY)
        RRV=DERIVE(IS,IPARAX)
        RRVAL=SQRT(RRU*RRU+RRV*RRV)
        IY=IFIX(IUOFF-UVFACT*RRU)
        IX=IFIX(IVOFF+UVFACT*RRV)
        IF(ISET.EQ.1) THEN             !first point
           ISET=0                      !reset initial data flag
        ELSE
           CALL SEL(0,IFOREG,-1)       !replot last point in cyan
           CALL BIT(ISAVEX,ISAVEY) 
        END IF
        CALL SEL(0,IYELLOW,-1)         !draw new dot in yellow 
        IF(RRVAL.LE.70.) THEN
          CALL BIT(IX,IY)              !draw dot only     
          ISAVEX=IX
          ISAVEY=IY
        ELSE
          ISET=1
        END IF
      END IF
      CALL EBK                         !empty last block
C
110   DO WHILE(.TRUE.)
        CALL CHECK_KEY('AC',IKEY)
        IF(IKEY.NE.0) RETURN
        IF(STATUS(2).NE.IS) GOTO 100
        CALL LIB$WAIT(0.2)
      END DO

 13   FORMAT(2I2.2)
      END
C*******************************************************************************
      SUBROUTINE HOR_HODO_BACKG(IKEY)
C
C Draws blank graph and labels
C
      IMPLICIT  NONE
      INTEGER*4 IKEY,IX1,IY1,IX2,IY2,ISINA,ICOSA,IOS,IY,IPARAX,IPARAY
      INTEGER*4 IUOFF,IUVPIX,IVOFF
      REAL*4    UVTICVAL,UVINC,UVFACT,A,FIRSTTIC
      CHARACTER CLABEL*2,CUNIT1*7
      INCLUDE   'DCOM_DEF.FOR'
      COMMON    /UVPLOT/ IPARAY,IPARAX,UVTICVAL,UVINC,UVFACT,IUOFF,
     -    IUVPIX,IVOFF,CUNIT1
C
C Draw plotting area
C
      CALL SEL(0,IBACKG,IBLACK)        !Dk green
      CALL SLT(0)
      CALL CIF(380,307,189)            !solid circle
      CALL SEL(0,ILGREY,-1)            !light grey additive
      CALL CIR(380,307,27)             !5m/s
      CALL CIR(380,307,54)             !10m/s  
      CALL CIR(380,307,81)             !15m/s
      CALL CIR(380,307,108)            !20m/s
      CALL CIR(380,307,135)            !25m/s
      CALL CIR(380,307,162)            !30m/s
      CALL CIR(380,307,189)            !35m/s
c
c draw diameters
c
      CALL SEL(0,ILGREY,-1)            !light grey additive
      CALL VEC(380,118,380,496)        !vertical
      CALL VEC(381,118,381,496)
      CALL VEC(191,307,569,307)        !horizontal
      CALL VEC(191,308,569,308)
C      
      IX2=380
      IY2=307

      DO A=10.0,80.0,10.0
        CALL CHECK_KEY('AC',IKEY)
        IF(IKEY.NE.0) RETURN
        ISINA=NINT(189.0*SIND(A))      !calculate angles 
        ICOSA=NINT(189.0*COSD(A))
        IX1=380+ISINA                  !top right quadrant
        IY1=307-ICOSA
        IF(A.EQ.30.0.OR.A.EQ.60.0) THEN
          CALL SLT(0)
        ELSE
          CALL SLT(1)  
        END IF
        CALL VEC(IX1,IY1,IX2,IY2)
      END DO

      DO A=10.0,80.0,10.0
        CALL CHECK_KEY('AC',IKEY)
        IF(IKEY.NE.0) RETURN
        ISINA=NINT(189.0*SIND(A))      !calculate angles 
        ICOSA=NINT(189.0*COSD(A))
        IX1=380+ISINA                  !bottom right quadrant 
        IY1=307+ICOSA
        IF(A.EQ.30.0.OR.A.EQ.60.0) THEN
          CALL SLT(0)
        ELSE
          CALL SLT(1)  
        END IF
        CALL VEC(IX1,IY1,IX2,IY2)
      END DO

      DO A=10.0,80.0,10.0
        CALL CHECK_KEY('AC',IKEY)
        IF(IKEY.NE.0) RETURN
        ISINA=NINT(189.0*SIND(A))      !calculate angles 
        ICOSA=NINT(189.0*COSD(A))
        IX1=380-ISINA                  !bottom left quadrant
        IY1=307+ICOSA
        IF(A.EQ.30.0.OR.A.EQ.60.0) THEN
          CALL SLT(0)
        ELSE
          CALL SLT(1)  
        END IF
        CALL VEC(IX1,IY1,IX2,IY2)
      END DO

      DO A=10.0,80.0,10.0
        CALL CHECK_KEY('AC',IKEY)
        IF(IKEY.NE.0) RETURN
        ISINA=NINT(189.0*SIND(A))      !calculate angles 
        ICOSA=NINT(189.0*COSD(A))
        IX1=380-ISINA                  !top left quadrant
        IY1=307-ICOSA
        IF(A.EQ.30.0.OR.A.EQ.60.0) THEN
          CALL SLT(0)
        ELSE
          CALL SLT(1)  
        END IF
        CALL VEC(IX1,IY1,IX2,IY2)
      END DO
      CALL SLT(0)
C
C Put on parameter name(s) and units
C
      CALL SCA(13,13,0,2)              !large letters
      CALL SEL(0,IFOREG,-1)            !cyan/additive
      CALL ALP(30,115,' U')            !Print para name   
      CALL ALP(30,129,'m/s')           !print units   
      CALL ALP(570,115,' V')           !Print para name   
      CALL ALP(570,129,'m/s')          !print units   
C
C label u components
C
      CALL SEL(0,IFOREG,-1)            !back to cyan/additive
      CALL SCA(10,10,0,2)              !7*9 small letters
      FIRSTTIC=UVTICVAL                !save first tic label
      DO IY=492,114,-IUVPIX            !write labels
        CALL CHECK_KEY('AC',IKEY)
        IF(IKEY.NE.0) RETURN
        IF(FIRSTTIC.EQ.0.0) GOTO 20    !dont write 0 label
        WRITE(CLABEL,'(I2)',IOSTAT=IOS) ABS(NINT(FIRSTTIC))
        CALL ALP(360,IY,CLABEL)                
  20    FIRSTTIC=FIRSTTIC+UVINC        !increment label
      END DO
C
      RETURN
      END
