      SUBROUTINE HOR_DUMP
      IMPLICIT  NONE
      INTEGER*4 ILIST(2),IKEY

      ILIST(1)=2
      ILIST(2)=4
      CALL UPDT_STATUS(ILIST)
      CALL HOR_DUMP1(ILIST,IKEY)
      RETURN
      END
C*******************************************************************************
      SUBROUTINE HOR_DUMP1(ILIST,IKEY)
C
C Displays derived parameters values for most parameters in tabular form.  This
C implements the function of Option 4 of the main HORACE menu.  The display is
C updated every 3 seconds, when a new set of derived data becomes available.
C
C External data:
C
C DERIVE  R*4  Passed   HOR_CALCS    12800 secs of 196 derived parameters
C                                    contained in a circular buffer
C
C Subroutines called:
C     
C     DUMP_BACKG
C     UPDT_STATUS
C     ggs            GGS.OLB
C
      IMPLICIT  NONE
      INTEGER*4 IS,RZ,IWA,IWS,IOS,IKEY,ILIST(2)
      LOGICAL*4 LPRTFLG
      CHARACTER CLINE(6)*56
      REAL*4    RGALTI,RGLONG,RGLATI,RSURF,RTEMP
      INCLUDE   'DCOM_DEF.FOR'
      INCLUDE   'HCOM_DEF.FOR'
      CHARACTER CHEAD(12)*56 /
     -' IHDG    SPR   PHGT   DTAT   DEW     LCL   TAS    WIND  ',
     -'  deg    mb     ft      C      C      m     m/s  deg m/s',
     -' OZMR    VP    RHGT   NTAT  TWCDP    TWC   GLAT   GLONG ',
     -'  ppb    mb     ft      C      C    gkg-1   deg    deg  ',
     -'  Th    Th e   GHGT   ICTP   FLDP    LWC   ILAT   ILONG ',
     -'   K      K     ft      C      C    gkg-1   deg    deg  ',
     -'  HMR   FHMR    MMR    ST    SHUM    MAD    DAD   SZEN  ',
     -' gm-3   gm-3   gkg-1    C    gkg-1  kgm-3  kgm-3   deg  ',
     -' UCLR   URED    UIR   PTCH   ROLL     U      V      W   ',
     -' Wm-2   Wm-2   Wm-2    deg    deg    ms-1   ms-1   ms-1 ',
     -' LCLR   LRED    LIR    IGS    IDA    VN     VE     VZ   ', 
     -' Wm-2   Wm-2   Wm-2    ms-1   deg    ms-1   ms-1   ms-1 '/
C
C Write fixed parts of the display to the access page the first time the
C routine is called.
C
      RZ=ILIST(1)                      !Stop compiler complaining
      RZ=0.0
      IKEY=ICHAR('A')                  !default return
      CALL SEL(0,IBLACK,IBLACK)        !black
      CALL RUB                         !clear display page
      CALL SAP(1)
      CALL DUMP_BACKG(CHEAD)           !draw fixed parts of display
      CALL FOOT('SELECT',' ',' ',' ',' ','PRINT',' ',' ')
      CALL SAP(0)
      IS=STATUS(2)-1
C
100   IS=IS+1
      IF(IS.GT.12800) IS=IS-12800
      IF(IS.LT.1.OR.IS.GT.12800) RETURN
      LPRTFLG=.TRUE.
      IF(STATUS(35)) THEN              !If GPS data is valid
        RGLATI=RGLAT(IS)               !Use it
        RGLONG=RGLNG(IS)
        RGALTI=RGALT(IS)
      ELSE                             !Else use zeros
        RGLATI=0.0
        RGLONG=0.0
        RGALTI=0.0
      END IF
      IF(RST(IS).EQ.99.9) THEN         !If Heimann data invalid
        RSURF=RPRT4(IS)
        LPRTFLG=.TRUE.
      ELSE 
        RSURF=RST(IS)
        LPRTFLG=.FALSE.
      END IF
C
C Generate the formatted values into lines using internal write statements.
C
      IF(STATUS(34)) THEN              !If latest derivations are valid
        IWA=NINT(RIWA(IS))             !convert winds to integer
        IWS=NINT(RIWS(IS))
        WRITE(CLINE(1),11,IOSTAT=IOS) RIHDG(IS),RSPR(IS),RPHGTF(IS)*
     -      1000.0,RTATDC(IS),RDEW(IS),RLCLVL(IS),RTAS(IS)*0.502,IWA,IWS
        WRITE(CLINE(2),12,IOSTAT=IOS) ROZMR(IS),RVP(IS),RRHGT(IS)*3.28,
     -      RTATNC(IS),RTWCDP(IS),RTWC(IS),RGLATI,RGLONG 
        RTEMP=0.
        IF(RDAD(IS).GT.0.) RTEMP=RLWC(IS)/RDAD(IS)
        WRITE(CLINE(3),13,IOSTAT=IOS) RPOT(IS),RPOTE(IS),RGALTI*3.28,
     -      RICT(IS),RFLDP(IS),RTEMP,RILAT(IS),RILNG(IS)
        WRITE(CLINE(4),14,IOSTAT=IOS) RHMR(IS),RFHMR(IS),RMMR(IS),RSURF,
     -      RSHUM(IS),RMAD(IS),RDAD(IS),RSZEN(IS)
        WRITE(CLINE(5),15,IOSTAT=IOS) RUCLR(IS),RURED(IS),RUIR(IS),
     -      RPTCH(IS),RROLL(IS),RU(IS),RV(IS),RW(IS)
        WRITE(CLINE(6),16,IOSTAT=IOS) RLCLR(IS),RLRED(IS),RLIR(IS),
     -      RIGS(IS),RIDA(IS),RVN(IS),RVE(IS),RVZ(IS)
      ELSE                             !If derivations invalid show zeros
        WRITE(CLINE(1),20,IOSTAT=IOS) RZ,RZ,RZ,RZ,RZ,RZ,RZ,RZ,RZ
        WRITE(CLINE(2),21,IOSTAT=IOS) RZ,RZ,RZ,RZ,RZ,RZ,RGLATI,RGLONG
        WRITE(CLINE(3),22,IOSTAT=IOS) RZ,RZ,RGALTI,RZ,RZ,RZ,RZ,RZ
        WRITE(CLINE(4),21,IOSTAT=IOS) RZ,RZ,RZ,RZ,RZ,RZ,RZ,RZ
        WRITE(CLINE(5),21,IOSTAT=IOS) RZ,RZ,RZ,RZ,RZ,RZ,RZ,RZ
        WRITE(CLINE(6),21,IOSTAT=IOS) RZ,RZ,RZ,RZ,RZ,RZ,RZ,RZ
      END IF
C
C Write data to the screen.  The lines are stored on the graphics board
C and then written displaced one pixel down to thicken them.  Data is written
C to page 1 and only transferred to the display page (0) when complete.
C
      CALL SAP(1)
      CALL DUMP_UPDATE(CLINE)
C
C Put in the special condition flags for the Heimann, General Eastern and Water
C Vapour sensor in red.
C
      CALL SEL(0,IRED,-1)              !red additive
      CALL SCA(13,13,0,2)              !small letters
      IF(IDP(IS).EQ.1) CALL ALP(17+4*92,56,'C') !hygrometer control condition
      IF(IDP(IS).NE.1) CALL ALP(17+4*92,56,' ')
      IF(IPRT4(IS).EQ.1.AND.LPRTFLG) THEN
        CALL ALP(17+3*92,56+72*3,'C') !Heimann on calibrate
      ELSE IF(IPRT4(IS).EQ.2.AND.LPRTFLG) THEN
        CALL ALP(17+3*92,56+72*3,'T') !Heimann temperature
      ELSE
        CALL ALP(17+3*92,56+72*3,' ')
      END IF
      IF(.NOT.LPRTFLG) CALL ALP(17+3*92,56+72*2,'ST') !Corrected surface temperaturE
      IF(LPRTFLG) CALL ALP(17+3*92,56+72*2,'  ') !Corrected surface temperaturE
      IF(IFLDP(IS).EQ.1) CALL ALP(17+4*92,56+72*2,'C') !fluor wvs on calibrate
      IF(IFLDP(IS).NE.1) CALL ALP(17+4*92,56+72*2,' ')
      IF(SPECIAL(IS,8).GE.1) THEN      !If GPS data wrong
        CALL ALP(17+6*92,56+72*1,'W')
        CALL ALP(17+7*92,56+72*1,'W')
        CALL ALP(17+2*92,56+72*2,'W')
      ELSE
        CALL ALP(17+6*92,56+72*1,' ')
        CALL ALP(17+7*92,56+72*1,' ')
        CALL ALP(17+2*92,56+72*2,' ')
      END IF 
C
C Send the commands to the access page and display it
C
      CALL ZOO(1,0)                    !copy access page to display page
      CALL SAP(0)
C
      DO WHILE(.TRUE.)
        CALL CHECK_KEY('A',IKEY)
        IF(IKEY.NE.0) RETURN
        IF(STATUS(2).NE.IS) GOTO 100
        CALL LIB$WAIT(0.2)
      END DO
C
11    FORMAT(2X,F5.0,2(1X,F6.0),2(2X,F5.1),1X,F6.0,1X,F6.1,I4.3,'/',
     -    I2)
12    FORMAT(2(1X,F6.1),1X,F6.0,2(2X,F5.1),2(1X,F6.2),F7.2)
13    FORMAT(2(1X,F6.1),1X,F6.0,2(2X,F5.1),2(1X,F6.2),F7.2)
14    FORMAT(5(1X,F6.1),2(1X,F6.2),1X,F6.1)
15    FORMAT(3(1X,F6.0),2(2X,F5.1),3(1X,F6.0))
16    FORMAT(8(1X,F6.0)) 
20    FORMAT(7(1X,F6.2),F3.0,'/',F3.0)
21    FORMAT(8(1X,F6.2))
22    FORMAT(2(1X,F6.2),1X,F6.0,4(1X,F6.2))
      END
