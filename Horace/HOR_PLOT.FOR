C
C HOR_PLOT
C HOR_PLOT1
C HOR_PLOT_BACKG
C HOR_PLOT_LABELX
C
C*******************************************************************************
      SUBROUTINE HOR_PLOT(IFUNC)
C
C Get the plotting arguments, then call plot.
C
      IMPLICIT  NONE
      INTEGER*4 IFUNC,ILIST(9),IORIG,ISCAL,IKEY,IPARA,IFREQ,IPARA1,
     -    IPARA2,IPARAX,IPARAY,IORIG1,ISCAL1,IORIG2,ISCAL2

      IF(IFUNC.EQ.1) THEN
10      CALL GET_PARA('Y ',IPARA)
        IF(IPARA.EQ.-1) RETURN
20      CALL GET_FREQ(IFREQ)
        IF(IFREQ.EQ.-1) RETURN
        IF(IFREQ.EQ.-2) GOTO 10
30      CALL GET_SCAL_PLOT('Y ',IFUNC,IPARA,IORIG,ISCAL)
        IF(IORIG.EQ.-1.OR.ISCAL.EQ.-1) RETURN
        IF(IORIG.EQ.-2.OR.ISCAL.EQ.-2) GOTO 10
        IF(IORIG.EQ.-3.OR.ISCAL.EQ.-3) GOTO 20
        ILIST(1)=6
        ILIST(2)=IFUNC
        ILIST(3)=IPARA
        ILIST(4)=IFREQ
        ILIST(5)=IORIG
        ILIST(6)=ISCAL
        CALL HOR_PLOT1(ILIST,IKEY)
        IF(CHAR(IKEY).EQ.'A') RETURN
        IF(CHAR(IKEY).EQ.'B') GOTO 10
        IF(CHAR(IKEY).EQ.'C') GOTO 20
        IF(CHAR(IKEY).EQ.'D') GOTO 30
      ELSE IF(IFUNC.EQ.2) THEN
11      CALL GET_PARA('Y1',IPARA1)
        IF(IPARA1.EQ.-1) RETURN
        CALL GET_PARA('Y2',IPARA2)
        IF(IPARA2.EQ.-1) RETURN
21      CALL GET_FREQ(IFREQ)
        IF(IFREQ.EQ.-1) RETURN
        IF(IFREQ.EQ.-2) GOTO 11
31      CALL GET_SCAL_PLOT('Y1',IFUNC,IPARA1,IORIG1,ISCAL1)
        IF(IORIG1.EQ.-1.OR.ISCAL1.EQ.-1) RETURN
        IF(IORIG1.EQ.-2.OR.ISCAL1.EQ.-2) GOTO 11
        IF(IORIG1.EQ.-3.OR.ISCAL1.EQ.-3) GOTO 21
        CALL GET_SCAL_PLOT('Y2',IFUNC,IPARA2,IORIG2,ISCAL2)
        IF(IORIG2.EQ.-1.OR.ISCAL2.EQ.-1) RETURN
        IF(IORIG2.EQ.-2.OR.ISCAL2.EQ.-2) GOTO 11
        IF(IORIG2.EQ.-3.OR.ISCAL2.EQ.-3) GOTO 21
        ILIST(1)=9
        ILIST(2)=IFUNC
        ILIST(3)=IPARA1
        ILIST(4)=IPARA2
        ILIST(5)=IFREQ
        ILIST(6)=IORIG1
        ILIST(7)=ISCAL1
        ILIST(8)=IORIG2
        ILIST(9)=ISCAL2
        CALL HOR_PLOT1(ILIST,IKEY)
        IF(CHAR(IKEY).EQ.'A') RETURN
        IF(CHAR(IKEY).EQ.'B') GOTO 11
        IF(CHAR(IKEY).EQ.'C') GOTO 21
        IF(CHAR(IKEY).EQ.'D') GOTO 31
      ELSE IF(IFUNC.EQ.3) THEN
12      CALL GET_PARA('Y ',IPARAY)
        IF(IPARAY.EQ.-1) RETURN
        CALL GET_PARA('X ',IPARAX)
        IF(IPARAX.EQ.-1) RETURN
22      CALL GET_FREQ(IFREQ)
        IF(IFREQ.EQ.-1) RETURN
        IF(IFREQ.EQ.-2) GOTO 12
32      CALL GET_SCAL_PLOT('Y ',IFUNC,IPARAY,IORIG1,ISCAL1)
        IF(IORIG1.EQ.-1.OR.ISCAL1.EQ.-1) RETURN
        IF(IORIG1.EQ.-2.OR.ISCAL1.EQ.-2) GOTO 12
        IF(IORIG1.EQ.-3.OR.ISCAL1.EQ.-3) GOTO 22
        CALL GET_SCAL_PLOT('X ',IFUNC,IPARAX,IORIG2,ISCAL2)
        IF(IORIG2.EQ.-1.OR.ISCAL2.EQ.-1) RETURN
        IF(IORIG2.EQ.-2.OR.ISCAL2.EQ.-2) GOTO 12
        IF(IORIG2.EQ.-3.OR.ISCAL2.EQ.-3) GOTO 22
        ILIST(1)=9
        ILIST(2)=IFUNC
        ILIST(3)=IPARAY
        ILIST(4)=IPARAX
        ILIST(5)=IFREQ
        ILIST(6)=IORIG1
        ILIST(7)=ISCAL1
        ILIST(8)=IORIG2
        ILIST(9)=ISCAL2
        CALL HOR_PLOT1(ILIST,IKEY)
        IF(CHAR(IKEY).EQ.'A') RETURN
        IF(CHAR(IKEY).EQ.'B') GOTO 12
        IF(CHAR(IKEY).EQ.'C') GOTO 22
        IF(CHAR(IKEY).EQ.'D') GOTO 32
      END IF
      END
C*******************************************************************************
      SUBROUTINE HOR_PLOT1(ILIST,IKEY)
C
C Plots user selected derived parameters, at user selected frequency of
c 3,6,9, or 12 seconds, plotting past data for 30,60,90,120 mins.
c User selects y vs time, y1,y2 vs time, or x vs y
C
      IMPLICIT  NONE
      INTEGER*4 ITMPY2(570),ICTLY2(570)
      INTEGER*4 ILIST(9),IKEY,IS,IFUNC,IPARA,IFREQ,IORIG,ISCAL,I,ITIM,
     -    IX,IY,ILASTY1,ILASTX,ILASTY2,IFSTXY,ITYCNT,IXC,
     -    IC,IP,IOS,IPARAY1,IPARAY2,IPARAY,IPARAX,IPARA1,IPARA2,IORIG1,
     -    IORIG2,ISCAL1,ISCAL2,IFIRSTY1,IFIRSTY2
      REAL*4    PREVAL1,PREVAL2
      CHARACTER CDATA*8,CLABEL*4
      INTEGER*4 LYOFF1,LYOFF2,LXOFF1,LXOFF2,LYPIX1,LYPIX2,LXPIX1,LXPIX2
      REAL*4    RTICVAL1,RTICVAL2,RTICINC1,RTICINC2,RYFACT1,RYFACT2,
     -    RXFACT1,RXFACT2
      COMMON /USERPLOT/ RTICVAL1,RTICVAL2,RTICINC1,RTICINC2,RYFACT1,
     -    RYFACT2,RXFACT1,RXFACT2,LYOFF1,LYOFF2,LXOFF1,LXOFF2,LYPIX1,
     -    LYPIX2,LXPIX1,LXPIX2
      INCLUDE   'DCOM_DEF.FOR'
      INCLUDE   'HCOM_DEF.FOR'
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
C HOR_PLOT_BACKG         This file    Draws a blank graph and labels
C HOR_PLOT_LABELX        This file    Labels x axis on xy plot
C HOR_PLOT_KEY           This file    Checks for immediate key presses
C HEAD_SHELL             HOR_SUBS
C FOOT                   HOR_SUBS
C UPDT_STATUS
C
C Start displaying-write heads on first and second time through only
C
      IF(STATUS(2).LT.1.OR.STATUS(2).GT.12800) RETURN
C
C Write fixed parts of the display to the access page the first time the
C routine is called.
C
      CALL UPDT_STATUS(ILIST)
      IFUNC=ILIST(2)
      IF(IFUNC.EQ.1) THEN
        IPARA=ILIST(3)
        IFREQ=ILIST(4)
        IORIG=ILIST(5)
        ISCAL=ILIST(6)
      ELSE
        IPARA1=ILIST(3)
        IPARA2=ILIST(4)
        IFREQ=ILIST(5)
        IORIG1=ILIST(6)
        ISCAL1=ILIST(7)
        IORIG2=ILIST(8)
        ISCAL2=ILIST(9)
      END IF

      CALL SEL(0,IBLACK,IBLACK)        !black
      CALL RUB                         !clear display page
      CALL HEAD_SHELL
      CALL FOOT('SELECT','PARAS','FREQ','ZOOM',' ','PRINT',' ',' ')
      IF(IFUNC.EQ.1) THEN              !single y plot
        IPARAY1=IPARA-512
        IPARAY2=IPARAY1
        RTICVAL1=TICVAL(IPARAY1) + ((IORIG-1)*TICINC(IPARAY1)) !new origin
        RTICVAL2=RTICVAL1
        RTICINC1=TICINC(IPARAY1)/ISCAL !new label increment 
        RTICINC2=RTICINC1
        LYPIX1=IYPIX(IPARAY1)          !no of pixels tween tics
        LYPIX2=LYPIX1
        LYOFF1=480+(RTICVAL1/RTICINC1*LYPIX1) !new zero value pixel offset
        RYFACT1=LYPIX1/RTICINC1        !new y scaling factor
        CALL HOR_PLOT_BACKG(IFUNC,IPARAY1,IPARAY1,IKEY) !draw graph blank
        IF(IKEY.NE.0) RETURN
      ELSE IF(IFUNC.EQ.2) THEN         !two y plots
        IPARAY1=IPARA1-512
        IPARAY2=IPARA2-512
        RTICVAL1=TICVAL(IPARAY1) + ((IORIG1-1)*TICINC(IPARAY1)) !new origin
        RTICVAL2=TICVAL(IPARAY2) + ((IORIG2-1)*TICINC(IPARAY2)) !new origin
        RTICINC1=TICINC(IPARAY1)/ISCAL1 !new label increment 
        RTICINC2=TICINC(IPARAY2)/ISCAL2 !new label increment
        LYPIX1=IYPIX(IPARAY1)          ! no of pixels tween tics
        LYPIX2=IYPIX(IPARAY2)          ! no of pixels tween tics
        LYOFF1=480+(RTICVAL1/RTICINC1*LYPIX1) !new zero value pixel offset
        LYOFF2=480+(RTICVAL2/RTICINC2*LYPIX2) !new zero value pixel offset
        RYFACT1=LYPIX1/RTICINC1        !new y scaling factor
        RYFACT2=LYPIX2/RTICINC2        !new y scaling factor
        CALL HOR_PLOT_BACKG(IFUNC,IPARAY1,IPARAY2,IKEY) !draw graph blank
        IF(IKEY.NE.0) RETURN
      ELSE IF(IFUNC.EQ.3) THEN         !x against y plot
        IPARAY=IPARA1-512
        IPARAX=IPARA2-512
        RTICVAL1=TICVAL(IPARAY) + ((IORIG1-1)*TICINC(IPARAY)) !new origin
        RTICVAL2=TICVAL(IPARAX) + ((IORIG2-1)*TICINC(IPARAX)) !new origin
        RTICINC1=TICINC(IPARAY)/ISCAL1 !new label increment 
        RTICINC2=TICINC(IPARAX)/ISCAL2 !new label increment
        LYPIX1=IYPIX(IPARAY)           ! no of pixels tween tics
        LYPIX2=LYPIX1
        LXPIX2=IXPIX(IPARAX)           ! no of pixels tween tics
        LYOFF1=480+(RTICVAL1/RTICINC1*LYPIX1) !new zero value pixel offset
        LXOFF2=100-(RTICVAL2/RTICINC2*LXPIX2) !new zero value pixel offset
        RYFACT1=LYPIX1/RTICINC1        !new y scaling factor
        RXFACT2=LXPIX2/RTICINC2        !new x scaling factor
        CALL HOR_PLOT_BACKG(IFUNC,IPARAY,IPARAY,IKEY) !draw graph blank
        IF(IKEY.NE.0) RETURN
        CALL HOR_PLOT_LABELX(IPARAX,IKEY)
        IF(IKEY.NE.0) RETURN
      END IF

      ILASTY1=-1
      ILASTY2=-1
      IF(IFREQ.EQ.5) GOTO 5000         !plot only current data
C
C Draw initial graph using past data
C
      IFSTXY=1
      ITYCNT=0
      IXC=669                          !next column for past data
      CALL SEL(0,IFOREG,-1)            !cyan/additive
      CALL SCA(10,10,0,2)              !small letters for times
      IC=STATUS(3)
      IP=STATUS(2)
      DO WHILE(MOD(NINT(RSECS(IP)),IFREQ*3).NE.0.AND.IC.GT.0)
        IC=IC-1
        IP=IP-1
        IF(IP.LE.0) IP=IP+12800
      END DO
      IS=IP                           !Latest data handled by this part

      DO WHILE(IC.GT.0.AND.IXC.GE.101)
        CALL CHECK_KEY('ABCD',IKEY)
        IF(IKEY.NE.0) RETURN

        IF(IFUNC.EQ.1.OR.IFUNC.EQ.2) THEN
          IY=IFIX(LYOFF1-RYFACT1*DERIVE(IP,IPARAY1))
          IF(IY.LT.151) IY=151
          IF(IY.GT.479) IY=479
          IX=IXC
          IF(SPECIAL(IP,9).EQ.0) THEN
            ILASTY1=-1
          ELSE IF(IX.EQ.669.OR.ILASTY1.EQ.-1.OR.
     -        IPARAY1.EQ.59.AND.ABS(DERIVE(IP,59)-PREVAL1).GE.300.) THEN
            CALL VEC(IX,IY,IX,IY)
            ILASTY1=IY
          ELSE
            CALL VEC(IX,IY)
!            CALL VEC(ilastx,ilasty1,IX,IY)
            ILASTY1=IY
          END IF
          IF(IPARAY1.EQ.59) PREVAL1=DERIVE(IP,59) !Wind direction
          ILASTX=IX
        END IF

        IF(IFUNC.EQ.2) THEN
          IY=IFIX(LYOFF2-RYFACT2*DERIVE(IP,IPARAY2))
          IF(IY.LT.151) IY=151
          IF(IY.GT.479) IY=479
          ITYCNT=ITYCNT+1
          IF(SPECIAL(IP,9).EQ.0) THEN
            ICTLY2(ITYCNT)=0
            ILASTY2=-1
          ELSE IF(IX.EQ.669.OR.ILASTY2.EQ.-1.OR.
     -        IPARAY2.EQ.59.AND.ABS(DERIVE(IP,59)-PREVAL2).GE.300.) THEN
            ICTLY2(ITYCNT)=1
            ITMPY2(ITYCNT)=IY
            ILASTY2=IY
          ELSE
            ICTLY2(ITYCNT)=2
            ITMPY2(ITYCNT)=IY
            ILASTY2=IY
          END IF
          IF(IPARAY2.EQ.59) PREVAL2=DERIVE(IP,59) !Wind direction
        END IF

        IF(IFUNC.EQ.3) THEN
          IY=IFIX(LYOFF1-RYFACT1*DERIVE(IP,IPARAY))
          IF(IY.LT.151) IY=151
          IF(IY.GT.479) IY=479
          IX=IFIX(LXOFF2+RXFACT2*DERIVE(IP,IPARAX))
          IF(IX.LT.101) IX=101
          IF(IX.GT.669) IX=669
          IF(SPECIAL(IP,9).EQ.1) CALL BIT(IX,IY)
        END IF

        IF(IFUNC.EQ.1.OR.IFUNC.EQ.2) THEN
          ITIM=NINT(RSECS(IP))
          IF(MOD(ITIM,IFREQ*60).EQ.0) THEN
            IF(MOD(ITIM,IFREQ*300).EQ.0) THEN !times every 5,10,15 or 20 s
              CALL VEC(ILASTX,480,ILASTX,489) !long tic
              WRITE(CLABEL,'(2I2.2)',IOSTAT=IOS) JHRS(IP),JMIN(IP)
              IF(ILASTX.GE.120) CALL ALP(ILASTX-20,490,CLABEL) !write time
            ELSE
              CALL VEC(ILASTX,480,ILASTX,485) !short tic
            END IF
            IF(ILASTY1.NE.-1) CALL VEC(ILASTX,ILASTY1,ILASTX,ILASTY1) !restore cursor
          END IF
        END IF

        IF(IS.EQ.IP.AND.IFUNC.NE.3) THEN !Save first point positions for later
          IFIRSTY1=ILASTY1
          IFIRSTY2=ILASTY2
        END IF

        IF(IFUNC.EQ.1.OR.IFUNC.EQ.2) IXC=IXC-1 !adjust counters and pointers
        IC=IC-IFREQ
        IP=IP-IFREQ
        IF(IP.LE.0) IP=IP+12800        !recycle when required
      END DO
C
C Draw second parameter from array
C
      IF(IFUNC.EQ.2) THEN
        CALL SEL(0,IYELLOW,-1)
        DO I=1,ITYCNT
          CALL CHECK_KEY('ABCD',IKEY)
          IF(IKEY.NE.0) RETURN
          IX=669-I+1
          IY=ITMPY2(I)
          IF(ICTLY2(I).EQ.0) THEN      !Do nothing
            ILASTY2=-1
          ELSE IF(ICTLY2(I).EQ.1) THEN !Plot bit
            CALL VEC(IX,IY,IX,IY)
            ILASTY2=IY
          ELSE IF(ICTLY2(I).EQ.2) THEN !Plot vector
            CALL VEC(IX,IY)
            ILASTY2=IY
          END IF
          ILASTX=IX
        END DO
      END IF

      ILASTY1=IFIRSTY1                 !Start new points at initial position
      ILASTY2=IFIRSTY2

5000  CALL SEL(0,IFOREG,-1)
      CALL ZOO(0,1)                    !copy display page to access page
      GOTO 110
C
C Display new data
C
100   IS=IS+1
      IF(IS.GT.12800) IS=1

      CALL SAP(1)                      !access page 1
      CALL HEAD_UPDATE
      CALL SCA(13,13,0,3)              !7*9 double

      IF (IFUNC.EQ.1.OR.IFUNC.EQ.2) THEN
        WRITE(CDATA,'(F8.2)',IOSTAT=IOS) DERIVE(IS,IPARAY1) !get data value
        CALL SEL(0,IFOREG,IBLACK)      !cyan/black
        CALL ALP(140,120,CDATA)        !print data
      ENDIF

      IF(IFUNC.EQ.2) THEN
        WRITE(CDATA,'(F8.2)',IOSTAT=IOS) DERIVE(IS,IPARAY2) !get data value
        CALL SEL(0,IYELLOW,IBLACK)     !yellow for 2ND Y parameter
        CALL ALP(380,120,CDATA)        !print data
      END IF    

      IF(IFUNC.EQ.3) THEN
        WRITE(CDATA,'(F8.2)',IOSTAT=IOS) DERIVE(IS,IPARAY) !get data value
        CALL SEL(0,IFOREG,IBLACK)      !cyan/black
        CALL ALP(140,120,CDATA)        !print data
        WRITE(CDATA,'(F8.2)',IOSTAT=IOS) DERIVE(IS,IPARAX) !get data value
        CALL SEL(0,IYELLOW,IBLACK)     !yellow for X parameter
        CALL ALP(380,120,CDATA)        !print data
      END IF    
C
C Plot the data
C
      ITIM=NINT(RSECS(IS))
      IF (IFREQ.EQ.5) IFREQ=1          !no past data -plot every 3 secs

      IF(MOD(ITIM,IFREQ*3).EQ.0) THEN  !plot every 3,6,9 or 12 s
        CALL SEL(0,IBACKG,IBLACK)      !green
        CALL VEC(669,151,669,479)      !clear right hand line
        CALL SEL(0,IFOREG,IBACKG)      !cyan/green

        IF(IFUNC.EQ.1.OR.IFUNC.EQ.2) THEN
          CALL ZOO(102,151,669,479,101,151,668,479) !copy/shift plot to access
          IY=IFIX(LYOFF1-RYFACT1*DERIVE(IS,IPARAY1))
          IF(IY.LT.151) IY=151
          IF(IY.GT.479) IY=479
          IF(SPECIAL(IS,9).EQ.0) THEN
            ILASTY1=-1
          ELSE IF(ILASTY1.EQ.-1.OR.
     -        IPARAY1.EQ.59.AND.ABS(DERIVE(IS,59)-PREVAL1).GE.300.) THEN
            CALL VEC(669,IY,669,IY)
            ILASTY1=IY
          ELSE
            CALL VEC(668,ILASTY1,669,IY)
            ILASTY1=IY
          END IF
          IF(IPARAY1.EQ.59) PREVAL1=DERIVE(IS,59) !Wind direction
          ILASTX=669
        END IF

        IF(IFUNC.EQ.2) THEN
          IY=IFIX(LYOFF2-RYFACT2*DERIVE(IS,IPARAY2))
          IF(IY.LT.151) IY=151
          IF(IY.GT.479) IY=479
          CALL SEL(0,IYELLOW,IBACKG)   !yellow/dk green
          IF(SPECIAL(IS,9).EQ.0) THEN
            ILASTY2=-1
          ELSE IF(ILASTY2.EQ.-1.OR.
     -        IPARAY2.EQ.59.AND.ABS(DERIVE(IS,59)-PREVAL2).GE.300.) THEN
            ILASTY2=IY
          ELSE
            CALL VEC(668,ILASTY2,669,IY)
            ILASTY2=IY
          END IF
          IF(IPARAY2.EQ.59) PREVAL2=DERIVE(IS,59) !Wind direction
          CALL SEL(0,IFOREG,IBACKG)    !cyan/green
        END IF

        IF(IFUNC.EQ.3) THEN
          IY=IFIX(LYOFF1-RYFACT1*DERIVE(IS,IPARAY))
          IF(IY.LT.151) IY=151
          IF(IY.GT.479) IY=479
          IX=IFIX(LXOFF2+RXFACT2*DERIVE(IS,IPARAX))
          IF(IX.LT.101) IX=101
          IF(IX.GT.669) IX=669
          IF(SPECIAL(IS,9).EQ.1) CALL BIT(IX,IY)
        END IF

        IF(IFUNC.EQ.1.OR.IFUNC.EQ.2) THEN
          CALL ZOO(102,482,700,500,101,482,699,500) !copy times and tics
          IF(MOD(ITIM,IFREQ*60).EQ.0) CALL VEC(669,480,669,485) !plot tic
          IF(MOD(ITIM,IFREQ*300).EQ.0) THEN !times every 5,10,15 or 20 s
            CALL VEC(669,480,669,489)  !long tic
            CALL SEL(0,IFOREG,-1)      !cyan/additive
            CALL SCA(10,10,0,2)        !small letters
            WRITE(CLABEL,'(2I2.2)',IOSTAT=IOS) JHRS(IS),JMIN(IS)
            CALL ALP(649,490,CLABEL)   !write time
          END IF
        END IF
      ENDIF
C
C Send the commands to the access page and display it
C
      CALL ZOO(1,0)                    !copy access page to display page
      CALL SAP(0)
C
110   DO WHILE(.TRUE.)
        CALL CHECK_KEY('ABCD',IKEY)
        IF(IKEY.NE.0) RETURN
        IF(STATUS(2).NE.IS) GOTO 100
        CALL LIB$WAIT(0.2)
      END DO

! 10   FORMAT(1X,A4,2X,A12,2X,2(I2.2,':'),I2.2,1X,I4.3,3X,F6.2,1X,F8.2)
! 11   FORMAT(2X,F5.0,1X,F6.0,1X,F6.1,1X,F6.0,2(2X,F5.1),I4.3,'/',I2)
      END
***************************************************************************
      SUBROUTINE HOR_PLOT_LABELX(IPARAX,IKEY)
C
C Draw labels on xaxis & tic marks
C
      IMPLICIT  NONE
      INTEGER*4 IPARAX,IKEY,IX,IOS,IY
      CHARACTER CLABEL*8
      REAL*4    FIRSTTIC
      INCLUDE   'DCOM_DEF.FOR'
      INTEGER*4 LYOFF1,LYOFF2,LXOFF1,LXOFF2,LYPIX1,LYPIX2,LXPIX1,LXPIX2
      REAL*4    RTICVAL1,RTICVAL2,RTICINC1,RTICINC2,RYFACT1,RYFACT2,
     -    RXFACT1,RXFACT2
      COMMON /USERPLOT/ RTICVAL1,RTICVAL2,RTICINC1,RTICINC2,RYFACT1,
     -    RYFACT2,RXFACT1,RXFACT2,LYOFF1,LYOFF2,LXOFF1,LXOFF2,LYPIX1,
     -    LYPIX2,LXPIX1,LXPIX2
C
C print x parameter name and units
C
      CALL SEL(0,IYELLOW,IBLACK)       !yellow for x parameter
      CALL SCA(13,13,0,2)              !7*9 double
      CALL ALP(270,115,CPARA(IPARAX))  !print x para name
      CALL ALP(270,129,CUNIT(IPARAX))  !print x para units
C
C do tics
C
      CALL SEL(0,IYELLOW,IBLACK)       !yellow/black
      DO IX=100,670,LXPIX2             !tics
        CALL VEC(IX,480,IX,486)
      END DO
C
C x axis labels
C
      CALL SCA(10,10,0,2)              !7*9 Small letters
      FIRSTTIC=RTICVAL2                !save first ticlabel
      DO IX=40,620,LXPIX2              !write labels
         CALL CHECK_KEY('ABCD',IKEY)
         IF(IKEY.NE.0) RETURN
         IY=490
         WRITE(CLABEL,'(F8.2)',IOSTAT=IOS) FIRSTTIC
         CALL ALP(IX,IY,CLABEL)
         FIRSTTIC=FIRSTTIC+RTICINC2    !increment label
      END DO
C
      RETURN
      END  
C*******************************************************************************
      SUBROUTINE HOR_PLOT_BACKG(IFUNC,IPARAY1,IPARAY2,IKEY)
C
C Draws blank background graph and labels
C
      IMPLICIT  NONE
      INTEGER*4 IFUNC,IPARAY1,IPARAY2,IY,IOS,IKEY
      REAL*4    FIRSTTIC
      CHARACTER CLABEL*8
      INCLUDE   'DCOM_DEF.FOR'
      INTEGER*4 LYOFF1,LYOFF2,LXOFF1,LXOFF2,LYPIX1,LYPIX2,LXPIX1,LXPIX2
      REAL*4    RTICVAL1,RTICVAL2,RTICINC1,RTICINC2,RYFACT1,RYFACT2,
     -    RXFACT1,RXFACT2
      COMMON /USERPLOT/ RTICVAL1,RTICVAL2,RTICINC1,RTICINC2,RYFACT1,
     -    RYFACT2,RXFACT1,RXFACT2,LYOFF1,LYOFF2,LXOFF1,LXOFF2,LYPIX1,
     -    LYPIX2,LXPIX1,LXPIX2
C
C Draw plotting area
C
      CALL SEL(0,IBACKG,IBLACK)        !Dk green
      CALL BLK(101,151,669,479)        !fill box
      CALL SEL(0,IFOREG,IBLACK)        !Cyan/black
      CALL BOXA(100,150,670,480)       !draw box
      CALL BOXA(99,149,671,481)
C
C Put on parameter name(s) and units
C
      CALL SCA(13,13,0,2)              !large letters
      CALL SEL(0,IFOREG,-1)            !cyan/additive
      CALL ALP(30,115,CPARA(IPARAY1))  !Print para name   
      CALL ALP(30,129,CUNIT(IPARAY1))  !print units   
      IF(IPARAY1.NE.IPARAY2) THEN      !do second parameter name
        CALL SEL(0,IYELLOW,-1)         !yellow/additive
        CALL ALP(270,115,CPARA(IPARAY2)) !Print para name   
        CALL ALP(270,129,CUNIT(IPARAY2)) !print units   
        CALL SEL(0,IFOREG,-1)          !back to cyan/additive
      END IF         
C
C Do y axis left tics
C
      DO IY=150,480,LYPIX1             !draw tics on yaxis
        CALL VEC(100,IY,94,IY)
      END DO
C
C Do y axis left labels
C
      CALL SEL(0,IFOREG,-1)            !cyan/additive
      CALL SCA(10,10,0,2)              !7*9 small letters
      FIRSTTIC=RTICVAL1                !save first tic label
      DO IY=475,145,-LYPIX1            !write labels
        CALL CHECK_KEY('ABCD',IKEY)
        IF(IKEY.NE.0) RETURN
        WRITE(CLABEL,'(F8.2)',IOSTAT=IOS) FIRSTTIC
        CALL ALP(10,IY,CLABEL)                
        FIRSTTIC=FIRSTTIC+RTICINC1     !increment label
      END DO
C
C Do y axis right tics (in yellow if two parameters)
C
      IF(IPARAY1.NE.IPARAY2) CALL SEL(0,IYELLOW,-1) !yellow/additive
      DO IY=150,480,LYPIX2
        CALL VEC(670,IY,676,IY)
      END DO
C
C Do y axis right labels
C
      CALL SCA(10,10,0,2)              !7*9 small letters
      IF(IFUNC.EQ.3)THEN
        FIRSTTIC=RTICVAL1              !x y plot
      ELSE
        FIRSTTIC=RTICVAL2              !save first tic label
      ENDIF
      DO IY=475,145,-LYPIX2            !write labels
        CALL CHECK_KEY('ABCD',IKEY)
        IF(IKEY.NE.0) RETURN
        WRITE(CLABEL,'(F8.2)',IOSTAT=IOS) FIRSTTIC
        CALL ALP(680,IY,CLABEL)                
        IF(IFUNC.EQ.3)THEN             !x y plot
          FIRSTTIC=FIRSTTIC+RTICINC1   !increment label
        ELSE    
          FIRSTTIC=FIRSTTIC+RTICINC2   !increment label
        ENDIF
      END DO
C
      RETURN
      END
