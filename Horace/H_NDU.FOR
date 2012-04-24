      PROGRAM H_NDU
C
C THE INFORMATION DISPLAYED BY THIS PROGRAM ON THE NAVIGATOR'S DISPLAY UNIT IS
C INTENDED ONLY WITH HELPING TO MEET THE SCIENTIFIC REQUIREMENTS OF A MISSION.
C THE NAVIGATORS SHOULD BE IN NO DOUBT THAT THE INFORMATION DISPLAYED CAN AT
C ANY TIME BE INVALID OR INACCURATE WITHOUT ANY WARNING.
C 
C Handles the Navigator's Display Unit.  Once a second updates the information
C on the display and checks for any key press by the Navigator.  Responds to
C numbered commands from the Navigator as follows:
C
C 0 - Resets the display
C 1 - Loads the current position as air position 1
C 2 - Loads the current position as air position 2
C 3 - Loads the current position as ground position 1
C 4 - Loads the current position as ground position 2
C 5 - Stops/starts the updating of the display
C 6 - Steps through pre-defined positions and air/ground posns on line 3
C 7 - Steps through pre-defined positions and air/ground posns on line 4
C 8 - Selects INS or corrected INS usage, or loads new correction
C 9 - Turns the display backlight on/off
C
C The display uses an RS232 driven Densitron SAC/SAD 40-3-0015 controller to 
C drive an LM44 4x40 liquid crystal display.  Communication takes place via the
C output defined by NDU_CHAN - normally LTA506 port 6 on the HORNET terminal
C server.  The terminal line used for NDU_CHAN should be set to NOECHO, PASTHRU,
C EIGHTBIT, NOTTSYNC.  The display consists of 4 lines of 40 characters used 
C as follows:
C
C Line 1:  GPS nn nn.nD nnn nn.nD WD nnn.n WS nnn.n
C Line 2:  INe nn nn.nD nnn nn.nD MH nnn.n TH nnn.n
C Line 3:  cPb nn nn.nD nnn nn.nD MB nnn.n RG nnn.n
C Line 4:  cPb nn nn.nD nnn nn.nD MB nnn.n RG nnn.n
C
C Lines 3 and 4 can also take the form:
C
C          INe AT nnn.n OT snn.nn MB nnn.n RG nnn.n
C
C where WD is INS wind direction from true north, updated every 3s by H_DERIVE
C       WS is INS wind speed, updated every 3s by H_DERIVE
C       MH is INS magnetic heading
C       TH is INS true heading
C       MB is magnetic bearing to the air, ground or fixed position
C       RG is distance to the air, ground or fixed position - nautical miles
C       D  is N,S,E, or W
C       c  is A, G or F according to whether it is air, ground or fixed position
C       b  is 1 or 2 for Air and Ground positions, 0 to 9 for fixed positions
C       e  is S if uncorrected INS positions in use, or 2 if using corrected
C       s  is + if to right of required track, else -
C      AT  is the distance along the required track from the present posn (nm)
C      OT  is the distance off the required track from the present position (nm)
C
C The lowest line always displays the INS groundspeed and drift angle by
C default.  Otherwise the lower two lines are only displayed when air or ground
C positions have been loaded or a fixed position has been selected.  Invalid
C information is displayed as blanks.  If updating of the display is inhibited
C then the data is held constant, and the character information is made to
C blink.
C
C Fixed positions are specified by editing them into the HORACE file H_NDU.DAT.
C This may be changed before or during a flight.  If lines 3 or 4 are set to
C show the groundspeed and drift, then they also show the present distance 
C along and off the track to the fixed position shown on line 4 or 3, from the 
C previous fixed position in the fixed position file.
C
C The program should never exit; if it detects read/write errors it closes
C down the I/O channels and then tries every 10s to re-establish them.
C
C Build with:
C 
C  $ FORT H_NDU, with HCOM_DEF.FOR in the same directory
C  $ LINK H_NDU,HORACE.OLB/LIB,H_COMMON/OPT
C
C    where H_COMMON.OPT contains the line SYS$LIBRARY:H_COMMON/SHAREABLE
C
C V1.00  04/02/94  W.D.N.JACKSON
C V1.01  21/10/94  W.D.N.JACKSON
C Now handles fixed positions.  Now handles any communication problems without
C exiting.
C V1.02  14/11/94  W.D.N.JACKSON
C Now handles corrected INS positions using the GPS and option 8.
C V1.03  24/01/96  W.D.N.JACKSON
C Now handles up to 50 fixed points, and displays INS groundspeed and drift
C V1.04  15/05/96  W.D.N.JACKSON
C Now displays off track and along track distances between fixed points.
C Now uses GPS for bearing, range, along and off track, if INS data is not
C available.
C Now has better checking of INS data availability and quality.
C V1.05  12/07/00  W.D.N.JACKSON
C Now writes display to NDU_DISP.DAT if STATUS(59) is zero.  This can then be
C read and displayed by H_DISPLAY.  If STATUS(60) is non zero uses the ascii
C value as though it were a Nav's keypress.  This allows users of H_DISPLAY
C to change the NDU if required.
C V1.06  30/07/01  W.D.N.JACKSON
C Parameter defined to enable program to run when no NDU/terminal server
C attached.
C
      IMPLICIT  NONE
      CHARACTER CGPOS*18,CIPOS*18,CPOS1*18,CPOS2*18,CB*4,COFF1*18,
     &    COFF2*18
      CHARACTER CLINE1*40,CLINE2*40,CLINE3*40,CLINE4*40
      CHARACTER*5 CWS,CWD,CTH,CMH,CMB1,CRG1,CMB2,CRG2,CDA,CGS
      CHARACTER CTYP1(53)*4,CTYP2(53)*4
C      CHARACTER CTYPE(10)*6
      INTEGER*2 JTEMP(2),JV16,JCHAN,JOSB(4)
      INTEGER*4 ITEMP,INDAT(10),IP,IS,ISTAT,IFRZ,I,IP1,IP2,IKCNT,IOS,
     &    IRPTR,IWPTR,INERR,IVAL,IBACK,INPUT,IFRC,ICNT,IC,IASTADD,IKEY
      LOGICAL*4 LOUTPUT,LOFFTRK1,LOFFTRK2,LGDINS,LGDWIN,LGDGPS,LPP2,
     &    LSUBGPS
      REAL*4    RLAT1(53),RLNG1(53),RLAT2(53),RLNG2(53),RCORLAT,RCORLNG,
     &    RRILAT,RRILNG,RTH,RMD,RMH,RCILAT,RCILNG,RCNEXZ,RWS,RWD,RGGLAT,
     &    RRGLAT,RGGLNG,RRGLNG,RLSTOUTP,RAP1WS,RAP1WA,RWN,RWE,
     &    RAP1TIM,ROFFTRK2,RONTRK2,ROFFTRK1,RONTRK1,RRG1,RRG2,
     &    RMB1,RMB2,RP2LAT,RP2LNG,RP1LAT,RP1LNG,I8CNT,
     &    RELTIM,RRGT1,RRGT2,RMBT1,RMBT2,RVX,RVY,RGS,RAZ,RTK,RDA,RAP2WS,
     &    RAP2WA,RWN2RAT,RWE2RAT,RAP2TIM,RWE1RAT,RWN1RAT,RKTODS
      LOGICAL*4 NDU

      INCLUDE   '($SYSSRVNAM)'
      INCLUDE   '($SSDEF)'
      INCLUDE   'HCOM_DEF.FOR'
      COMMON    /NDUCOM/ INERR,IASTADD,IKEY,IKCNT,IWPTR,IRPTR,INDAT,
     &    JOSB,JCHAN
      VOLATILE  /NDUCOM/
      EQUIVALENCE (ITEMP,JTEMP)
      EQUIVALENCE (RGGLAT,IGPSMESS(127,1))
      EQUIVALENCE (RGGLNG,IGPSMESS(127,2))
      DATA NDU   /.FALSE./             !No NDU attached
      DATA CTYP1 /'FP0','FP1','FP2','FP3','FP4','FP5','FP6','FP7','FP8',
     &    'FP9','F10','F11','F12','F13','F14','F15','F16','F17','F18',
     &    'F19','F20','F21','F22','F23','F24','F25','F26','F27','F28',
     &    'F29','F30','F31','F32','F33','F34','F35','F36','F37','F38',
     &    'F39','F40','F41','F42','F43','F44','F45','F46','F47','F48',
     &    'F49','AP1','GP1','INS'/
      DATA CTYP2 /'FP0','FP1','FP2','FP3','FP4','FP5','FP6','FP7','FP8',
     &    'FP9','F10','F11','F12','F13','F14','F15','F16','F17','F18',
     &    'F19','F20','F21','F22','F23','F24','F25','F26','F27','F28',
     &    'F29','F30','F31','F32','F33','F34','F35','F36','F37','F38',
     &    'F39','F40','F41','F42','F43','F44','F45','F46','F47','F48',
     &    'F49','AP2','GP2','INS'/
C      DATA CTYPE /'Reset','AP1','AP2','GP1','GP2','Freeze','Step 3',
C     &            'Step 4','PP2','Light'/

      JV16(IP,IS)=NFDATA(NPLOC(IP)+IS-1,STATUS(1)) !Function to get DRS data
!
! Associate with the Common Event Flag cluster, and set CEF 82 in order to
! force a new read of the fixed point file.
!
      CALL LOG_MESS('H_NDU starting')
      ISTAT=SYS$ASCEFC(%VAL(64),'H_CEF_CLUSTER',,) !Associate with CEF cluster
      ISTAT=SYS$SETEF(%VAL(82))        !Set file change flag
!
! Open I/O channels to the NDU via RS232 port.  The CHANNEL subroutine waits
! in a loop trying every 30s until both input and output channels are opened.
!
      CALL ST_MESS('Trying to establish connection to NDU')
      CALL LOG_MESS('Trying to establish connection to NDU')
      IF(NDU) CALL CHANNEL('OPEN')     !Open I/O channels, waiting if necessary
      CALL ST_MESS('Sending data to Navs Display Unit')
      CALL LOG_MESS('Connection established to NDU')
!
! Open display output file in case needed
!
!      OPEN(UNIT=12,FILE='NDU_DISP.DAT',STATUS='UNKNOWN',SHARED,
!     &    ACCESS='DIRECT',RECL=4*40/4,IOSTAT=IOS)
      STATUS(59)=0
      STATUS(60)=0
      STATUS(61)=0
!
! Initialize variables for the main loop.  Also send the initial commands to
! turn on and initialise the NDU display.
!
      IFRZ=0                           !Display not frozen to start
      IBACK=0                          !Background light off to start
      LOUTPUT=.TRUE.                   !Pretend key pressed to force 1st output
      IF(NDU) THEN
        WRITE(10,10,IOSTAT=IOS) CHAR(27)//'a'//CHAR(5)//CHAR(40) !Initialize
        WRITE(10,10,IOSTAT=IOS) CHAR(27)//'j' !Turn display on
      END IF
      DO I=1,52                        !Show no positions yet defined
        RLAT1(I)=999.
        RLAT2(I)=999.
      END DO
      RLAT1(53)=0.                     !Enable drift/ground speed display
      RLAT2(53)=0.                     !Enable drift/ground speed display
      IP1=0                            !Show no positions yet
      IP2=53                           !Show ground speed and drift by default
      LPP2=.FALSE.                     !True if using corrected INS posn
      LOFFTRK1=.FALSE.                 !True to display off track distances
      LOFFTRK2=.FALSE.                 !True to display off track distances
      RCORLAT=0.                       !No INS correction yet
      RCORLNG=0.                       !No INS correction yet
      IKCNT=0                          !No of key presses in buffer
      IRPTR=1                          !Pointer to next char to read from buff
      IWPTR=1                          !Pointer to next char to write to buff
!
      DO WHILE(.TRUE.)                 !Loop continuously every 1s
!
! Check for errors on previous reads/write.  If any found then call routine
! to close I/O channels and wait until they can be opened again.
!
        IF((INERR.EQ.-1.OR.IOS.NE.0).AND.NDU) THEN
          CALL ST_MESS('Trying to re-establish connection to NDU')
          IF(INERR.EQ.-1.AND.IOS.NE.0) THEN
            CALL LOG_MESS('NDU I/O error.  Trying to'//
     &          ' re-establish connection')
          ELSE IF(INERR.EQ.-1) THEN
            CALL LOG_MESS('NDU read error.  Trying to'//
     &        ' re-establish connection')
          ELSE IF(IOS.NE.0) THEN
            CALL LOG_MESS('NDU write error.  Trying to'//
     &        ' re-establish connection')
          END IF
          CALL CHANNEL('REOPEN')       !Re-open I/O channels
          CALL ST_MESS('Sending data to Navs Display Unit')
         CALL LOG_MESS('Connection re-established, sending data to NDU')
          IOS=0
        END IF
C
C Compute INS lat, long, true and mag headings, ground speed and drift, taking
C data directly from the latest raw DRS data block in HORACE global common. 
C Compute corrected INS position.  Reject the INS data if no data is coming
C from the DRS, if the INU zeros and ones words are wrong, if the IIU status
C word shows errors, or if the INU is not in GC ALIGN or NAVIGATE.
C
        LGDINS=.TRUE.
        IF(STATUS(1).EQ.0) LGDINS=.FALSE.
        IF(LGDINS.AND.(STATUS(33).NE.1)) LGDINS=.FALSE.
        IF(LGDINS.AND.(JV16(167,1).NE.0)) LGDINS=.FALSE.
        IF(LGDINS.AND.(JV16(168,1).NE.'FFFF'X)) LGDINS=.FALSE.
        IF(LGDINS.AND.((JV16(164,1).AND.'C000'X).NE.0)) LGDINS=.FALSE.
        IF(LGDINS.AND..NOT.BTEST(JV16(163,29),16-9).AND.
     &     .NOT.BTEST(JV16(163,29),16-5)) LGDINS=.FALSE.
        IF(LGDINS) THEN
          JTEMP(2)=JV16(163,21)
          JTEMP(1)=JV16(163,22)
          RCNEXZ=ITEMP/2.**30
          IF(RCNEXZ.GE.-1.AND.RCNEXZ.LE.1.) RRILAT=ASIND(RCNEXZ)
          JTEMP(2)=JV16(163,23)
          JTEMP(1)=JV16(163,24)
          RRILNG=ITEMP*180./2.**31
          RTH=JV16(163,12)/2.**15*180.
          IF(RTH.LT.0.) RTH=RTH+360.
          RMH=JV16(163,13)/2.**15*180.
          IF(RMH.LT.0.) RMH=RMH+360.
          RMD=RTH-RMH
          IF(RMD.GT.90) RMD=RMD-360.
          IF(RMD.LT.-90) RMD=RMD+360.
          RCILAT=RRILAT+RCORLAT
          RCILNG=RRILNG+RCORLNG
        END IF
!
! Read in fixed point data file if CEF 82 is set, and set pointers to display
! FP0 and FP1 provided no APs or GPs are currently entered.
!
        IF(SYS$READEF(%VAL(82),IVAL).EQ.SS$_WASSET) THEN  !Check for key
          CALL READ_FIXED(RLAT1,RLNG1) !Read fixed points
          DO I=1,50                    !Copy positions for line 4
            RLAT2(I)=RLAT1(I)
            RLNG2(I)=RLNG1(I)
          END DO
          IF(IP1.LT.51.OR.IP1.EQ.53) IP1=0 !Don't use old fixed points
          IF(IP2.LT.51.OR.IP1.EQ.53) IP2=0
          DO I=1,53
            IF(IP1.EQ.0.AND.RLAT1(I).NE.999.) IP1=I
            IF(IP2.EQ.0.AND.RLAT2(I).NE.999.
     &            .AND.(I.NE.IP1.OR.I.GE.51)) IP2=I
          END DO
          IF(IP1.EQ.53.AND.IP2.EQ.53) IP1=0
          ISTAT=SYS$CLREF(%VAL(82))    !Clear file change flag
          STATUS(61)=1                 !Tell H_DISPLAY have new file
!
          IF(IP1.GE.1.AND.IP1.LE.50) THEN !If fixed point find track from last
            IP=IP1-1
            IF(IP.LT.1) IP=50
            DO WHILE(RLAT1(IP).EQ.999.)
              IP=IP-1
              IF(IP.LT.1) IP=50
            END DO
            RMBT1=999.
            IF(IP.NE.IP1)
     &          CALL CSTEER(RLAT1(IP),RLNG1(IP),RLAT1(IP1),RLNG1(IP1),
     &          RMD,RMBT1,RRGT1)
          END IF
!
          IF(IP2.GE.1.AND.IP2.LE.50) THEN !If fixed point find track from last
            IP=IP2-1
            IF(IP.LT.1) IP=50
            DO WHILE(RLAT2(IP).EQ.999.)
              IP=IP-1
              IF(IP.LT.1) IP=50
            END DO
            RMBT2=999.
            IF(IP.NE.IP2)
     &          CALL CSTEER(RLAT2(IP),RLNG2(IP),RLAT2(IP2),RLNG2(IP2),
     &          RMD,RMBT2,RRGT2)
          END IF
        END IF
C
C Take the current wind speed and direction from the latest H_DERIVE
C derivations.  Note that these are only updated every 3s.
C
        LGDWIN=.FALSE.
        IF(STATUS(2).GT.0.AND.LGDINS) THEN !Wind data only updates every 3s
          RWS=RIWS(STATUS(2))*1.944    !Wind speed in knots
          RWD=RIWA(STATUS(2))          !Wind direction - true
          LGDWIN=.TRUE.
        END IF
C
C Take GPS lat and long from IGPSMESS(127,1) and IGPSMESS(127,2) where they are
C put every second by H_GPS_LOG.  Will be -999s if no valid values.
C
        LGDGPS=.FALSE.
        IF(RGGLAT.GT.-999.AND.STATUS(17).NE.0.AND.STATUS(35).EQ.1) THEN
          RRGLAT=RGGLAT
          RRGLNG=RGGLNG
          LGDGPS=.TRUE.
        END IF
C
C If the INU is not working then substitute the GPS instead
C
        LSUBGPS=.FALSE.
        IF(LGDGPS.AND..NOT.LGDINS) THEN
          LSUBGPS=.TRUE.
          RRILAT=RRGLAT
          RRILNG=RRGLNG
          RCILAT=RRGLAT                !No correction needed
          RCILNG=RRGLNG
          RMD=0.                       !Don't know the magnetic deviation
        END IF
C
C Respond to any character received.
C
        INPUT=-1
        IF(IKCNT.GT.0) THEN
          INPUT=INDAT(IRPTR)
          IKCNT=IKCNT-1
          IRPTR=IRPTR+1
          IF(IRPTR.GT.10) IRPTR=1
C          CALL LOG_MESS('Nav''s key press '//CHAR(48+INPUT)//' - '//
C     &        CTYPE(INPUT+1))
          LOUTPUT=.TRUE.
          RLSTOUTP=SECNDS(0.0)
        END IF
C
C I don't believe the following four lines of code serve any purpose.
C
        IF(SECNDS(RLSTOUTP).GT.15.*60.) THEN
          LOUTPUT=.TRUE.              !Pretend key pressed
          RLSTOUTP=SECNDS(0.0)
        END IF
C
C See if Flight Leader / H_DISPLAY wants to change things.  If so it will
C override any Nav's key press.
C
        IF(STATUS(60).NE.0) THEN
          IF(STATUS(60).GE.48.AND.STATUS(60).LE.57) INPUT=STATUS(60)-48
          STATUS(60)=0
        END IF
C
C '0' - Reset display
C
        IF(INPUT.EQ.0) THEN
          IF(NDU) THEN
            IF(IOS.EQ.0) WRITE(10,10,IOSTAT=IOS) 
     &        CHAR(27)//'a'//CHAR(5)//CHAR(40) !Initialize
            IF(IOS.EQ.0) WRITE(10,10,IOSTAT=IOS) CHAR(27)//'j' !Turn display on
          END IF
C
C '1' - Save current INS position as the latest air position 1.  Also save the
C current wind for use in drifting the air position.  Note that a fairly rough
C calculation method is used, simply evaluating the current wind components
C N and E in deg/s, and applying them forwards in time.  Assumes 1 nautical
C mile corresponds to minute of arc.
C
        ELSE IF(INPUT.EQ.1.AND.LGDINS.AND.LGDWIN.AND.IFRZ.EQ.0) THEN
          IF(RRILAT.GT.-85.AND.RRILAT.LE.85) THEN
            IP1=51                     !Point to air position
            RLAT1(IP1)=RRILAT          !Initial air position
            RLNG1(IP1)=RRILNG
            RAP1WS=RWS                 !Wind speed and direction (M/S)
            RAP1WA=RWD
            RWN=RWS*COSD(RWD)          !Wind component N (kts)
            RWE=RWS*SIND(RWD)          !Wind component E (kts)
            RKTODS=1./3600./60.        !Kts to deg/s
            RWN1RAT=RWN*RKTODS         !Winds northwards latitude rate
            RWE1RAT=RWE*RKTODS/COSD(RRILAT) !Winds eastwards latitude rate
            RAP1TIM=SECNDS(0.)         !Time of the initial position
          END IF
C
C '2' - Save current INS position as the latest air position 2.  Also save the
C current wind for use in drifting the air position.  Note that a fairly rough
C calculation method is used, simply evaluating the current wind components
C N and E in deg/s, and applying them forwards in time.  Assumes 1 nautical
C mile corresponds to minute of arc.
C
        ELSE IF(INPUT.EQ.2.AND.LGDINS.AND.LGDWIN.AND.IFRZ.EQ.0) THEN
          IF(RRILAT.GT.-85.AND.RRILAT.LE.85) THEN
            IP2=51                     !Point to air position
            RLAT2(IP2)=RRILAT          !Initial air position
            RLNG2(IP2)=RRILNG
            RAP2WS=RWS                 !Wind speed and direction (M/S)
            RAP2WA=RWD
            RWN=RWS*COSD(RWD)          !Wind component N (kts)
            RWE=RWS*SIND(RWD)          !Wind component E (kts)
            RKTODS=1./3600./60.        !Kts to deg/s
            RWN2RAT=RWN*RKTODS         !Winds northwards latitude rate
            RWE2RAT=RWE*RKTODS/COSD(RRILAT) !Winds eastwards latitude rate
            RAP2TIM=SECNDS(0.)         !Time of the initial position
          END IF
C
C '3' - Save current INS position as the latest ground position 1
C
        ELSE IF(INPUT.EQ.3.AND.(LGDINS.OR.LSUBGPS).AND.IFRZ.EQ.0) THEN
          IP1=52                       !Point to ground position
          RLAT1(IP1)=RRILAT
          RLNG1(IP1)=RRILNG
C
C '4' - Save current INS position as the latest ground position 2
C
        ELSE IF(INPUT.EQ.4.AND.(LGDINS.OR.LSUBGPS).AND.IFRZ.EQ.0) THEN
          IP2=52                       !Point to ground position
          RLAT2(IP2)=RRILAT
          RLNG2(IP2)=RRILNG
C
C '5' - Toggle the freezing or updating of the display
C
        ELSE IF(INPUT.EQ.5) THEN
          IFRZ=1-IFRZ
          IFRC=0
C
C '6' - Step through any fixed positions, AP 1 and GP 1, INS
C
        ELSE IF(INPUT.EQ.6) THEN
          IP1=IP1+1
          IF(IP1.GT.53) IP1=1
          IC=0
          DO WHILE(RLAT1(IP1).EQ.999..AND.IC.LT.53)
            IP1=IP1+1
            IF(IP1.GT.53) IP1=1
            IC=IC+1
          END DO
          IF(IC.EQ.53) IP1=0
!
          IF(IP1.GE.1.AND.IP1.LE.50) THEN !If fixed point find track from last
            IP=IP1-1
            IF(IP.LT.1) IP=50
            DO WHILE(RLAT1(IP).EQ.999.)
              IP=IP-1
              IF(IP.LT.1) IP=50
            END DO
            RMBT1=999.
            IF(IP.NE.IP1)
     &          CALL CSTEER(RLAT1(IP),RLNG1(IP),RLAT1(IP1),RLNG1(IP1),
     &          RMD,RMBT1,RRGT1)
          END IF
C
C '7' - Step through any fixed positions, AP 2 and GP 2, INS
C
        ELSE IF(INPUT.EQ.7) THEN
          IP2=IP2+1
          IF(IP2.GT.53) IP2=1
          IC=0
          DO WHILE(RLAT2(IP2).EQ.999..AND.IC.LT.53)
            IP2=IP2+1
            IF(IP2.GT.53) IP2=1
            IC=IC+1
          END DO
          IF(IC.EQ.53) IP2=0
!
          IF(IP2.GE.1.AND.IP2.LE.50) THEN !If fixed point find track from last
            IP=IP2-1
            IF(IP.LT.1) IP=50
            DO WHILE(RLAT2(IP).EQ.999.)
              IP=IP-1
              IF(IP.LT.1) IP=50
            END DO
            RMBT2=999.
            IF(IP.NE.IP2)
     &          CALL CSTEER(RLAT2(IP),RLNG2(IP),RLAT2(IP2),RLNG2(IP2),
     &          RMD,RMBT2,RRGT2)
          END IF
C
C '8' - Handle options associated with the GPS correction of the INS.  There
C are two options: A single press means change the displayed INS position
C from uncorrected (INS) to corrected (IN2), a double press means use the
C current GPS position to correct INS positions used to compute range and
C bearing to fixed points.  If IN2 is displayed then the range and bearings
C to fixed (not air or ground) positions are computed using the IN2 value.
C
        ELSE IF(INPUT.EQ.8) THEN
          I8CNT=1
          ICNT=0
          DO WHILE(I8CNT.LT.2.AND.ICNT.LT.5) !Look for 2nd press within 0.5s
            IF(IKCNT.GT.0) THEN
              IF(INDAT(IRPTR).EQ.8) I8CNT=I8CNT+1
              ICNT=0
              IKCNT=IKCNT-1
              IRPTR=IRPTR+1
              IF(IRPTR.GT.10) IRPTR=1
            ELSE
              CALL LIB$WAIT(0.1)
            END IF
            ICNT=ICNT+1
          END DO
!
          IF(I8CNT.EQ.1) THEN          !Single key press
            LPP2=.NOT.LPP2             !Toggle display
          ELSE IF(I8CNT.EQ.2) THEN     !Double key press
            IF(LGDINS.AND.LGDGPS) THEN !Only use new values if have good data
              LPP2=.TRUE.              !Force display of corrected values
              RCORLAT=RRGLAT-RRILAT    !Store correction
              RCORLNG=RRGLNG-RRILNG
              IF(RCORLNG.GT.180) RCORLNG=RCORLNG-360.
              IF(RCORLNG.LT.-180) RCORLNG=RCORLNG+360.
              RCILAT=RRILAT+RCORLAT    !Do immediate update of corrected values
              RCILNG=RRILNG+RCORLNG
            END IF
          END IF
C
C '9' - Toggle background lighting on or off
C
        ELSE IF(INPUT.EQ.9) THEN
          IF(NDU) THEN
            IF(IBACK.EQ.0) THEN
              IF(IOS.EQ.0) WRITE(10,10,IOSTAT=IOS) CHAR(27)//'F' !Background on
            ELSE
              IF(IOS.EQ.0) WRITE(10,10,IOSTAT=IOS) CHAR(27)//'G' !Background off
            END IF
            IBACK=1-IBACK             
          END IF
        END IF
C
        IFRC=1-IFRC                    !Toggle freeze display indicator
        IF(IFRZ.EQ.1) GOTO 13
C
C Drift the air position using the wind saved when the air position was set.
C Compute range and bearing from the current INS position to the air and
C ground positions.
C
        IF(IP1.EQ.51) THEN
          RELTIM=SECNDS(RAP1TIM)       !Elapsed time since initial air position
          RP1LAT=RLAT1(51)-RWN1RAT*RELTIM !Compute current air position
          IF(RP1LAT.GT.90.) RP1LAT=180.-RP1LAT
          IF(RP1LAT.LT.-90.) RP1LAT=-180.+RP1LAT
          RP1LNG=RLNG1(51)-RWE1RAT*RELTIM
          IF(RP1LNG.GT.180) RP1LNG=-360.+RP1LNG
          IF(RP1LNG.LT.-180.) RP1LNG=360.+RP1LNG
        ELSE IF(IP1.GT.0.AND.IP1.NE.53) THEN
          RP1LAT=RLAT1(IP1)
          RP1LNG=RLNG1(IP1)
        END IF
        IF(IP2.EQ.51) THEN
          RELTIM=SECNDS(RAP2TIM)       !Elapsed time since initial air position
          RP2LAT=RLAT2(51)-RWN2RAT*RELTIM !Compute current air position
          IF(RP2LAT.GT.90.) RP2LAT=180.-RP2LAT
          IF(RP2LAT.LT.-90.) RP2LAT=-180.+RP2LAT
          RP2LNG=RLNG2(51)-RWE2RAT*RELTIM
          IF(RP2LNG.GT.180) RP2LNG=-360.+RP2LNG
          IF(RP2LNG.LT.-180.) RP2LNG=360.+RP2LNG
        ELSE IF(IP2.GT.0.AND.IP2.NE.53) THEN
          RP2LAT=RLAT2(IP2)
          RP2LNG=RLNG2(IP2)
        END IF
C
C Compute magnetic bearing and range to ground, air or fixed position.  For
C fixed positions use the current corrected INS positions if the corrected
C INS position is being displayed; for ground and air positions use the raw 
C INS position.
C
        IF(IP1.NE.53) THEN
          IF(IP1.GT.50.OR..NOT.LPP2) THEN
            CALL CSTEER(RRILAT,RRILNG,RP1LAT,RP1LNG,RMD,RMB1,RRG1) !Bear/range
          ELSE                           !Fixed position
            CALL CSTEER(RCILAT,RCILNG,RP1LAT,RP1LNG,RMD,RMB1,RRG1) !Bear/range
          END IF
        END IF
        IF(IP2.NE.53) THEN
          IF(IP2.GT.50.OR..NOT.LPP2) THEN
            CALL CSTEER(RRILAT,RRILNG,RP2LAT,RP2LNG,RMD,RMB2,RRG2) !Bear/range
          ELSE                           !Fixed position
            CALL CSTEER(RCILAT,RCILNG,RP2LAT,RP2LNG,RMD,RMB2,RRG2) !Bear/range
          END IF
        END IF
C
C Compute off track and along track distances in nm, if these are required.
C They are only available between fixed points, not ground or air positions.
C
        COFF1=' '
        IF(IP1.GE.1.AND.IP1.LE.50.AND.IP2.EQ.53.AND.RMBT1.NE.999.) THEN
          ROFFTRK1=SIND(RMBT1-RMB1)*RRG1
          RONTRK1=COSD(RMBT1-RMB1)*RRG1
          CALL COFFTRK(ROFFTRK1,RONTRK1,COFF1)
        END IF
        COFF2=' '
        IF(IP2.GE.1.AND.IP2.LE.50.AND.IP1.EQ.53.AND.RMBT2.NE.999.) THEN
          ROFFTRK2=SIND(RMBT2-RMB2)*RRG2
          RONTRK2=COSD(RMBT2-RMB2)*RRG2
          CALL COFFTRK(ROFFTRK2,RONTRK2,COFF2)
        END IF
C
C Compute ground speed and drift if these are being displayed.  Note that it
C is easiest to do the drift calculations in the platform, rather than true,
C frame of reference.
C
        IF((IP1.EQ.53.OR.IP2.EQ.53).AND.LGDINS) THEN
          JTEMP(2)=JV16(163,3)
          JTEMP(1)=JV16(163,4)
          RVX=ITEMP/2.**18*12*25.4/1000.
          JTEMP(2)=JV16(163,5)
          JTEMP(1)=JV16(163,6)
          RVY=ITEMP/2.**18*12*25.4/1000.
          RGS=SQRT(RVX**2+RVY**2)*1.944
          RAZ=JV16(163,9)/2.**15*180.
          RTK=0.
          IF(RVX.NE.0) RTK=ATAN2D(-RVY,RVX)
          RDA=RTK-RAZ
          IF(RDA.LT.-180.) RDA=RDA+360.
          IF(RDA.GT.180.) RDA=RDA-360.
        END IF
C
C Convert lats and longs into strings in dd mm.t format
C
        CALL CLATLNG(RRGLAT,RRGLNG,CGPOS)
        IF(LPP2) THEN                  !Display corrected INS posn
          CALL CLATLNG(RCILAT,RCILNG,CIPOS)
        ELSE
          CALL CLATLNG(RRILAT,RRILNG,CIPOS)
        END IF
        CALL CLATLNG(RP1LAT,RP1LNG,CPOS1)
        CALL CLATLNG(RP2LAT,RP2LNG,CPOS2)
C
C Convert the data to character strings
C
        CALL CODE(RWS,CWS)
        CALL CODE(RWD,CWD)
        CALL CODE(RTH,CTH)
        CALL CODE(RMH,CMH)
        IF(IP1.GT.0.AND.IP1.LT.53) CALL CODE(RMB1,CMB1)
        IF(IP1.GT.0.AND.IP1.LT.53) CALL CODE(RRG1,CRG1)
        IF(IP2.GT.0.AND.IP2.LT.53) CALL CODE(RMB2,CMB2)
        IF(IP2.GT.0.AND.IP2.LT.53) CALL CODE(RRG2,CRG2)
        IF(IP1.EQ.53.OR.IP2.EQ.53) THEN
          CALL CODE(RGS,CGS)
          CALL CODE(ABS(RDA),CDA)
          CDA(1:1)='S'
          IF(RDA.LT.0.) CDA(1:1)='P'
          IF(CDA(2:2).EQ.'0') CDA(2:2)=' '
        END IF
        CB=' MB '
        IF(LSUBGPS) CB=' TB '
C
C Quality check the data a little, and set invalid data to blanks
C
        IF(.NOT.LGDINS) THEN           !Raw data not valid
          CIPOS=' '
          CTH=' '
          CMH=' '
          IF(.NOT.LGDINS.AND..NOT.LSUBGPS) THEN
            CMB1=' '
            CMB2=' '
            CRG1=' '
            CRG2=' '
            COFF1=' '
            COFF2=' '
          END IF
          CDA=' '
          CGS=' '
        END IF
        IF(STATUS(34).NE.1.OR..NOT.LGDWIN.OR..NOT.LGDINS) THEN !Invalid winds
          CWS=' '
          CWD=' '
        END IF
        IF(.NOT.LGDGPS) CGPOS=' '      !No valid GPS
C
C Form the output strings and send them to the display
C
13      CLINE1='GPS '//CGPOS//' WD '//CWD//' WS '//CWS
        CLINE2='INS '//CIPOS//' MH '//CMH//' TH '//CTH
        IF(LPP2) CLINE2(1:3)='IN2'     !Corrected INS position
        IF(IP1.GT.0) CLINE3=CTYP1(IP1)//CPOS1//CB//CMB1//' RG '
     &      //CRG1
        IF(IP1.EQ.53) CLINE3=CTYP1(IP1)//COFF2//' DA '//
     &      CDA//' GS '//CGS
        IF(IP1.EQ.53.AND.LPP2) CLINE3(1:3)='IN2'
        IF(IP2.GT.0) CLINE4=CTYP2(IP2)//CPOS2//CB//CMB2//' RG '
     &      //CRG2
        IF(IP2.EQ.53) CLINE4=CTYP2(IP2)//COFF1//' DA '//
     &      CDA//' GS '//CGS
        IF(IP2.EQ.53.AND.LPP2) CLINE4(1:3)='IN2'
        IF(IP1.EQ.0) CLINE3=' '
        IF(IP2.EQ.0) CLINE4=' '
        IF(IFRZ.EQ.1.AND.IFRC.EQ.1) THEN !Show data is frozen
          CLINE1(1:3)=' '
          CLINE1(24:25)=' '
          CLINE1(33:34)=' '
          CLINE2(1:3)=' '
          CLINE2(24:25)=' '
          CLINE2(33:34)=' '
          CLINE3(1:3)=' '
          CLINE3(24:25)=' '
          CLINE3(33:34)=' '
          CLINE4(1:3)=' '
          CLINE4(24:25)=' '
          CLINE4(33:34)=' '
        END IF
        IOS=0
        IF(NDU) THEN
          IF(IOS.EQ.0.AND.INERR.NE.-1) WRITE(10,10,IOSTAT=IOS) 
     &        CLINE1//CHAR(27)//'v'
          IF(IOS.EQ.0.AND.INERR.NE.-1) WRITE(10,10,IOSTAT=IOS) 
     &        CLINE2//CHAR(27)//'v'
          IF(IOS.EQ.0.AND.INERR.NE.-1) WRITE(10,10,IOSTAT=IOS) 
     &        CLINE3//CHAR(27)//'v'
          IF(IOS.EQ.0.AND.INERR.NE.-1) WRITE(10,10,IOSTAT=IOS) 
     &        CLINE4//CHAR(27)//'q'
        END IF
        IF(LOUTPUT) LOUTPUT=.FALSE.
        IF(STATUS(59).EQ.1) THEN
          CNDU(1)=CLINE1
          CNDU(2)=CLINE2
          CNDU(3)=CLINE3
          CNDU(4)=CLINE4
          STATUS(59)=0
        END IF
!
! Wait for up to a second before executing loop again, checking every 1/5s
! for a key press.  If there have been any read or write errors then don't 
! wait at all.
!
        ICNT=0
        DO WHILE(IKCNT.EQ.0.AND.IOS.EQ.0.AND.ICNT.LT.5.AND.INERR.EQ.0) 
          ICNT=ICNT+1
          CALL LIB$WAIT(0.2)
        END DO
      END DO
10    FORMAT(//A)
      END   
C*******************************************************************************
      SUBROUTINE LOG_MESS(MESSAGE)
C
C Writes message to log file consisting of program name, date, time and message.
C
      IMPLICIT  NONE
      CHARACTER MESSAGE*(*)
      CHARACTER CDAT*9,CTIM*10
      CALL DATE1(CDAT)
      CALL TIME(CTIM)
      TYPE *,'H_NDU '//CDAT//' '//CTIM//MESSAGE
      RETURN
      END
C*******************************************************************************
      SUBROUTINE ST_MESS(MESSAGE)
C
C Write message to the logical H_NDU_STAT in the group logical name
C table.  GRPNAM and SYSNAM privileges are required.
C
      IMPLICIT  NONE
      CHARACTER MESSAGE*(*)
      INTEGER*4 ISTAT
      INCLUDE   '($SYSSRVNAM)'
      INCLUDE   '($LNMDEF)'
      INCLUDE   '($PSLDEF)'

      STRUCTURE /LIST/
        INTEGER*2 BUFFER_LEN
        INTEGER*2 CODE/LNM$_STRING/
        INTEGER*4 BUFFER_LOC
        INTEGER*4 RET_LEN/0/
        INTEGER*4 END_MARK/0/
      END STRUCTURE

      RECORD /LIST/ TLIST
      TLIST.BUFFER_LEN=LEN(MESSAGE)
      TLIST.BUFFER_LOC=%LOC(MESSAGE)
      ISTAT=SYS$CRELNM(,'LNM$GROUP','H_NDU_STAT',PSL$C_SUPER,TLIST)
      RETURN
      END
C*******************************************************************************
      SUBROUTINE CLATLNG(RLAT,RLNG,CPOS)
C
C Takes the supplied lat and long and converts it to a string of the form:
C
C     nn nn.nN nnn nn.nW
C
      IMPLICIT  NONE
      INTEGER*4 ILATD,ILATT,ILNGD,ILNGT,IOS,ILATM,ILNGM
      REAL*4    RLAT,RLNG,RLATM,RLNGM,RLATT,RLNGT
      CHARACTER CPOS*18,CLAT*1,CLNG*1

      IF(RLAT.LT.-90.OR.RLAT.GT.90) RETURN
      IF(RLNG.LT.-180.OR.RLNG.GT.180) RETURN
      CLAT='N'
      IF(RLAT.LT.0.) THEN
        CLAT='S'
        RLAT=-RLAT
      END IF
      ILATD=IFIX(RLAT)
      RLATM=(RLAT-ILATD)*60
      ILATM=IFIX(RLATM)
      RLATT=(RLATM-ILATM)*10
      ILATT=IFIX(RLATT)
      CLNG='E'
      IF(RLNG.LT.0.) THEN
        CLNG='W'
        RLNG=-RLNG
      END IF
      ILNGD=IFIX(RLNG)
      RLNGM=(RLNG-ILNGD)*60
      ILNGM=IFIX(RLNGM)
      RLNGT=(RLNGM-ILNGM)*10
      ILNGT=IFIX(RLNGT)
      WRITE(CPOS,10,IOSTAT=IOS) ILATD,CHAR(223),ILATM,ILATT,CLAT,ILNGD,
     -    CHAR(223),ILNGM,ILNGT,CLNG
      IF(CLAT.EQ.'S') RLAT=-RLAT
      IF(CLNG.EQ.'W') RLNG=-RLNG
      RETURN
10    FORMAT(I2.2,A,I2.2,'.',I1,A,X,I3.3,A,I2.2,'.',I1,A)
      END
C*******************************************************************************
      SUBROUTINE KEYAST
C
C Called asynchronously when a character is received from the Nav's push
C button.  A recirculating buffer is used to hold input data to ensure that
C data is not lost if the key is pressed twice in quick succession.
C The input goes into INDAT as a number in the range 0 to 9.  INERR is normally
C 0 and a value of -1 indicates that KEYAST was called after a read terminated
C with an error.  The address of this routine is passed in COMMON by IASTADD 
C so the routine can call itself.
C
C Subroutines called:
C
C   KEYAST      This routine
C
C V1.01  20/10/94  W.D.N.JACKSON
C
      IMPLICIT  NONE
      INTEGER*4 INDAT(10),INERR,ILEN,IKEY,IKCNT,IWPTR,IFNCT,ISTAT,
     &    IASTADD,IRPTR
      INTEGER*2 JOSB(4),JCHAN
      CHARACTER CMESS*54,CTIME*10
      INCLUDE   '($SYSSRVNAM)'
      INCLUDE   '($IODEF)'
      INCLUDE   '($SSDEF)'
      COMMON    /NDUCOM/ INERR,IASTADD,IKEY,IKCNT,IWPTR,IRPTR,INDAT,
     &    JOSB,JCHAN
      VOLATILE  /NDUCOM/

      IF(JOSB(1).NE.SS$_NORMAL) THEN
        INERR=-1
        CALL SYS$GETMSG(%VAL(JOSB(1)),ILEN,CMESS,%VAL(1),)
        CALL STR$UPCASE(CMESS(1:1),CMESS(1:1))
        IF(ILEN.GT.0) CALL LOG_MESS(CMESS(1:ILEN))
      ELSE
        IF(IKEY.LT.48.OR.IKEY.GT.57) THEN !Detect any non valid characters
          CALL TIME(CTIME)
          TYPE *,CTIME,IKEY
        END IF
        IKEY=IKEY.AND.'F'X             !Convert to 0 - 15
        IF(IKEY.GE.0.AND.IKEY.LE.9) THEN
          IF(IKCNT.LT.10) THEN
            INDAT(IWPTR)=IKEY
            IWPTR=IWPTR+1
            IF(IWPTR.GT.10) IWPTR=1
            IKCNT=IKCNT+1
          END IF
        END IF
        IFNCT=IO$_READVBLK.OR.IO$M_NOECHO.OR.IO$M_CVTLOW
        ISTAT=SYS$QIO(,%VAL(JCHAN),%VAL(IFNCT),JOSB,%VAL(IASTADD),,
     -      IKEY,%VAL(1),,,,)          !Read one character
        IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      END IF
      RETURN
      END     
C*******************************************************************************
      SUBROUTINE CODE(RVAL,CSTR)
C
C Converts real number into F5.1 ASCII string with leading zeros.  RVAL must
C be positive.
C
      IMPLICIT  NONE
      CHARACTER CSTR*5
      INTEGER*4 IOS,I
      REAL*4    RVAL

      WRITE(CSTR,10,IOSTAT=IOS) RVAL
      DO I=1,2
        IF(CSTR(I:I).EQ.' ') CSTR(I:I)='0'
      END DO
      RETURN
10    FORMAT(F5.1)
      END
C*******************************************************************************
      SUBROUTINE COFFTRK(ROFFTRK,RONTRK,COFF)
C
C Converts along track and off track values to an 18 character string in the
C form "AT nnn.n OT +nn.nn"
C
      IMPLICIT  NONE
      CHARACTER COFF*18
      INTEGER*4 IOS
      REAL*4    ROFFTRK,RONTRK

      COFF='AT nnn.n OT +nn.nn'
      CALL CODE(ABS(RONTRK),COFF(4:8))
      WRITE(COFF(14:18),'(F5.2)',IOSTAT=IOS) ABS(ROFFTRK)
      IF(COFF(14:14).EQ.' ') COFF(14:14)='0'
      IF(ROFFTRK.LT.0.) COFF(13:13)='-'
      RETURN
      END
C*******************************************************************************
      SUBROUTINE CSTEER(RLAT1,RLNG1,RLAT2,RLNG2,RMD,RBEAR,RDIS)
C
C Computes Great Circle range and bearing using the method used by the Litton
C LTN-3000 Navigation System. (See Litton's LTN-3000 Navigation System
C Program Description, Section 17, available from Electronic's Section).
C
C Inputs:  Lat and long of start position
C          Lat and long of final position
C          Magnetic deviation
C
C Returns: RBEAR - Magnetic bearing (0-360 deg)
C          RDIS  - Range (nmi)
C
      IMPLICIT  NONE
      REAL*4    RLAT1,RLNG1,RLAT2,RLNG2,RMD,RBEAR,RDIS,XE,YE,ZE,X,Y,Z,
     &    RGCBEAR,RGCRNGE
C
C Compute coordinates of P2 in earth axes XE, YE, ZE.
C
      XE=SIND(RLAT2)
      YE=-COSD(RLAT2)*SIND(RLNG2-RLNG1)
      ZE=COSD(RLAT2)*COSD(RLNG2-RLNG1)
C
C Rotate point through latitude of P1
C
      X=XE*COSD(RLAT1)-ZE*SIND(RLAT1)
      Y=YE
      Z=XE*SIND(RLAT1)+ZE*COSD(RLAT1)-1
C
C Compute bearing and range
C
      IF(X.NE.0.) THEN
        RGCBEAR=ATAN2D(-Y,X)           !Bearing in degrees (-180 to 180)
      ELSE
        IF(Y.LT.0.) RGCBEAR=90.
        IF(Y.GE.0.) RGCBEAR=-90.
      END IF
      IF(1.+Z.NE.0.) THEN
        RGCRNGE=ATAN2D(X*COSD(RGCBEAR)-Y*SIND(RGCBEAR),1.+Z) !Range in degrees
      ELSE
        RGCRNGE=90.
      END IF
C
C Convert bearing to magnetic bearing, adjust to the range 0-360 deg, and
C convert range from degrees to nautical miles
C
      RBEAR=RGCBEAR-RMD+360.
      IF(RBEAR.LT.0.) RBEAR=RBEAR+360.
      IF(RBEAR.GE.360.) RBEAR=RBEAR-360.
      IF(RGCRNGE.LT.0.) RGCRNGE=RGCRNGE+360.
      RDIS=RGCRNGE*60.                 !Convert degrees to nautical miles
C
      RETURN
      END
C*******************************************************************************
      SUBROUTINE READ_FIXED(RFLAT,RFLNG)
C
C Reads the H_NDU.DAT file which specifies up to 50 pre-defined fixed points for
C the NDU to display the range and bearing to.  This file may be changed at any
C time using option 12 of the Flight Leader's menu, which will set CEF 82 to 
C indicate that the file has been changed.  Prints a message each time the
C file is read with the defined fixed points.  If a line can't be read because
C it is formatted wrong, it is simply ignored.  See the file H_INU.DAT for
C details of the required format.  Lat/lngs are returned as 999. when no valid
C values are specified.
C
      IMPLICIT  NONE
      CHARACTER CLINE*80,CWORDS(12)*15
      INTEGER*4 I,IOS,ICNT,IFXNO,LAN
      LOGICAL*4 LERR
      REAL*4    RFLAT(52),RFLNG(52),RLATD,RLATM,RLAT,RLNGD,RLNGM,RLNG

      DO I=1,50
        RFLAT(I)=999.
      END DO
      OPEN(UNIT=11,FILE='H_NDU.DAT',STATUS='OLD',READONLY,IOSTAT=IOS)
      IF(IOS.NE.0) THEN
        CALL LOG_MESS('Unable to open H_NDU.DAT')
        RETURN
      END IF
      CALL LOG_MESS('Reading fixed positions from H_NDU.DAT')
      READ(11,'(A)',IOSTAT=IOS) CLINE
      DO WHILE(IOS.EQ.0)
        CALL C_GETWORDS(CLINE,CWORDS,ICNT)
        IF(ICNT.GT.0) THEN
          LERR=.FALSE.
          IF(ICNT.NE.7) LERR=.TRUE.
          READ(CWORDS(1),*,IOSTAT=IOS) IFXNO
          IF(IOS.NE.0.OR.IFXNO.LT.0.OR.IFXNO.GT.49) LERR=.TRUE.
          READ(CWORDS(2),*,IOSTAT=IOS) RLATD
          IF(IOS.NE.0.OR.RLATD.LT.0.OR.RLATD.GT.90.) LERR=.TRUE.
          READ(CWORDS(3),*,IOSTAT=IOS) RLATM
          IF(IOS.NE.0.OR.RLATM.LT.0.OR.RLATM.GT.59.999) LERR=.TRUE.
          RLAT=RLATD+RLATM/60.
          CALL STR$UPCASE(CWORDS(4),CWORDS(4))
          IF(CWORDS(4).EQ.'S') RLAT=-RLAT
          IF(CWORDS(4).NE.'N'.AND.CWORDS(4).NE.'S') LERR=.TRUE.
          READ(CWORDS(5),*,IOSTAT=IOS) RLNGD
          IF(IOS.NE.0.OR.RLNGD.LT.0.OR.RLNGD.GT.180.) LERR=.TRUE.
          READ(CWORDS(6),*,IOSTAT=IOS) RLNGM
          IF(IOS.NE.0.OR.RLNGM.LT.0.OR.RLNGM.GT.59.999) LERR=.TRUE.
          RLNG=RLNGD+RLNGM/60.
          CALL STR$UPCASE(CWORDS(7),CWORDS(7))
          IF(CWORDS(7).EQ.'W') RLNG=-RLNG
          IF(CWORDS(7).NE.'E'.AND.CWORDS(7).NE.'W') LERR=.TRUE.
          IF(LERR.AND.ICNT.GT.0) THEN
            TYPE *,CLINE(1:LAN(CLINE))//' ***Invalid format***'
          ELSE
            RFLAT(IFXNO+1)=RLAT
            RFLNG(IFXNO+1)=RLNG
            TYPE *,CLINE(1:LAN(CLINE))
          END IF
        END IF
        READ(11,'(A)',IOSTAT=IOS) CLINE
      END DO
      CLOSE(11,IOSTAT=IOS)
      RETURN
      END
C*******************************************************************************
C
C ROUTINE          C_GETWORDS SUBROUTINE FORTVAX
C
C PURPOSE          Analyses a line for up to 12 words separated by spaces
C
C DESCRIPTION      Takes a character string representing a line and analyses
C                  it for identifiable words.  Words are defined as a group
C                  of characters between separators.  Separators are spaces
C                  and the start and end of lines.  End of line is either
C                  the end of the string or an ! symbol.  Thus the ! acts
C                  as a comment introducer and all characters on a line
C                  beyond the ! are ignored.  The maximum word length is 15
C                  characters; longer words will be trucated to 15 characters.
C
C VERSION          1.00  10-1-90  N.JACKSON
C
C ARGUMENTS        CLINE      C**  IN  The line to be analysed
C                  CWORDS(12) C*15 OUT Array in which any found words are returned
C                  IWDCNT     I*4  OUT The number of words found
C
C CHANGES          
C
********************************************************************************
      SUBROUTINE C_GETWORDS(CLINE,CWORDS,IWDCNT)
CDEC$ IDENT 'V1.00'
      IMPLICIT  NONE
      CHARACTER*(*) CLINE         !The line to be analysed
      CHARACTER*15  CWORDS(12)    !Array in which any found words are returned
      INTEGER*4     IWDCNT        !The number of words found
      INTEGER*4 I,J,IWS,IWE
C
C The routine works done the line looking for separators and words
C
      DO I=1,12                        !Clear output array
        CWORDS(I)=' '
      END DO
      IWDCNT=0                         !No words yet
      J=LEN(CLINE)                     !String length
      IF(J.EQ.0) RETURN                !Nothing to do if zero length
      I=1                              !Start position on line
      DO WHILE(IWDCNT.LT.12)           !Look for up to 12 words
        DO WHILE(CLINE(I:I).EQ.' ')    !Step over spaces
          IF(I.EQ.J) RETURN            !Return if end of line
          I=I+1                        !Next character
        END DO
        IF(CLINE(I:I).EQ.'!') RETURN   !Return if found comment
        IWS=I                          !Else note start of word
        DO WHILE(CLINE(I:I).NE.' '.AND.CLINE(I:I).NE.'!') !Look for end or comm
          IF(I.EQ.J) THEN              !If end of line
            IWE=I                      !Note end of word
            IF((IWE-IWS+1).GT.15) IWE=IWS+15-1 !Truncate word length to 15
            IWDCNT=IWDCNT+1            !Bump word count
            CWORDS(IWDCNT)=CLINE(IWS:IWE) !Store word
            RETURN                     !Return because was end of line
          END IF
          I=I+1                        !Try next character
        END DO
        IWE=I-1                        !Note end of word
        IF((IWE-IWS+1).GT.15) IWE=IWS+15-1 !Truncate word length to 15
        IWDCNT=IWDCNT+1                !Bump word count
        CWORDS(IWDCNT)=CLINE(IWS:IWE)  !Store word
      END DO
      RETURN
      END
C*******************************************************************************
C
C ROUTINE          LAN FUNCTION FORTVAX
C
C PURPOSE          Returns length of a string less trailing blanks
C
C DESCRIPTION      Function returns the length of a string not including
C                  trailing blanks. If the string is all blanks, or is of zero
C                  length, the function returns a value of 1.
C
C VERSION          1.00  9-1-90  W.D.N.JACKSON
C
C ARGUMENTS        CSTRING  C**  IN   String whose length to be found
C                  LAN      I*4  OUT  String length less trailing spaces
C
C*******************************************************************************
      INTEGER*4 FUNCTION LAN(CSTRING)
      IMPLICIT  NONE
      CHARACTER CSTRING*(*)
      INTEGER*4 N

      N=LEN(CSTRING)                   !Get string length
      DO WHILE(N.GT.0.AND.CSTRING(N:N).EQ.' ') !Step back over spaces
        N=N-1
      END DO
      IF(N.EQ.0) N=1                   !Special case
      LAN=N
      RETURN
      END
C*******************************************************************************
      SUBROUTINE CHANNEL(CMODE)
!
! Open or close I/O channels, and keep trying every 30 s to open both an
! input channel from and an output channel to the NDU.
!
      IMPLICIT  NONE
      INTEGER*2 JOSB(4),JCHAN
      INTEGER*4 INDAT(10),IOS,ISTAT,IFUNCT,IPASS,ILEN,LIB$GETDVI,
     &    IASTADD,IFNCT,IKEY,INERR,IKCNT,IWPTR,IRPTR
      LOGICAL*4 LSUCCESS
      INCLUDE   '($IODEF)'
      INCLUDE   '($DVIDEF)'
      INCLUDE   '($SYSSRVNAM)'
      EXTERNAL  KEYAST
      CHARACTER CMODE*(*),CNAME*64
      COMMON    /NDUCOM/ INERR,IASTADD,IKEY,IKCNT,IWPTR,IRPTR,INDAT,
     &    JOSB,JCHAN
      VOLATILE  /NDUCOM/
!
! If mode is REOPEN then close any existing channels before starting again
! from scratch.
!
      IF(CMODE.EQ.'REOPEN') THEN
        CLOSE(UNIT=10,IOSTAT=IOS)
        ISTAT=SYS$CANCEL(%VAL(JCHAN))
        IF(CNAME(2:4).EQ.'LTA') THEN   !If terminal server
          IFUNCT=IO$_TTY_PORT.OR.IO$M_LT_DISCON !Disconnect function
          ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFUNCT),JOSB,,,,,,,,) !Disconnect
        END IF
        ISTAT=SYS$DASSGN(%VAL(JCHAN))
        ISTAT=SYS$DALLOC('NDU_CHAN',)
      END IF
!
! Try to open FORTRAN output to NDU, and direct QIO input from the NDU keys
!
      IPASS=1
      DO WHILE(.NOT.LSUCCESS.OR.IPASS.EQ.1)
        LSUCCESS=.TRUE.
        OPEN(UNIT=10,FILE='NDU_CHAN',STATUS='OLD',IOSTAT=IOS,
     -      CARRIAGECONTROL='NONE')
        IF(IOS.NE.0.AND.IPASS.EQ.1) 
     &      CALL LOG_MESS('FORTRAN open for output failed')
        IF(IOS.NE.0) LSUCCESS=.FALSE.
        ISTAT=SYS$ALLOC('NDU_CHAN',,,,) !Allocate the serial port
        IF(.NOT.ISTAT.AND.IPASS.EQ.1.AND.LSUCCESS) 
     &      CALL LOG_MESS('Unable to allocate output port')
        IF(.NOT.ISTAT) LSUCCESS=.FALSE.
        ISTAT=SYS$ASSIGN('NDU_CHAN',JCHAN,,) !Now assign a channel to it
        IF(.NOT.ISTAT.AND.IPASS.EQ.1.AND.LSUCCESS)
     &      CALL LOG_MESS('Unable to assign channel to output port')
        IF(.NOT.ISTAT) LSUCCESS=.FALSE.
        ISTAT=LIB$GETDVI(DVI$_DEVNAM,JCHAN,,,CNAME,ILEN) !Get device name
        IF(.NOT.ISTAT.AND.IPASS.EQ.1.AND.LSUCCESS)
     &      CALL LOG_MESS('Unable to get device information')
        IF(.NOT.ISTAT) LSUCCESS=.FALSE.
        IF(CNAME(2:4).EQ.'LTA') THEN   !If terminal server
          IFUNCT=IO$_TTY_PORT.OR.IO$M_LT_CONNECT !Connect function
          ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFUNCT),JOSB,,,,,,,,) !Make connectn
          IF(.NOT.ISTAT) LSUCCESS=.FALSE.
          IF(.NOT.JOSB(1).AND.IPASS.EQ.1.AND.LSUCCESS)
     &        CALL LOG_MESS('Unable to connect to terminal server')
          IF(.NOT.JOSB(1)) LSUCCESS=.FALSE.
        END IF
        IF(.NOT.LSUCCESS) THEN
          CLOSE(UNIT=10,IOSTAT=IOS)
          ISTAT=SYS$CANCEL(%VAL(JCHAN))
          IF(CNAME(2:4).EQ.'LTA') THEN !If terminal server
            IFUNCT=IO$_TTY_PORT.OR.IO$M_LT_DISCON !Disconnect function
            ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFUNCT),JOSB,,,,,,,,) !Disconnect
          END IF
          ISTAT=SYS$DASSGN(%VAL(JCHAN))
          ISTAT=SYS$DALLOC('NDU_CHAN',)
          CALL LIB$WAIT(10.0)
        END IF
        IPASS=IPASS+1
      END DO
!
! Set up first read to handle any key presses
!
      IASTADD=%LOC(KEYAST)
      IFNCT=IO$_READVBLK.OR.IO$M_NOECHO.OR.IO$M_CVTLOW.OR.IO$M_PURGE
      ISTAT=SYS$QIO(,%VAL(JCHAN),%VAL(IFNCT),JOSB,%VAL(IASTADD),,
     -    IKEY,%VAL(1),,,,)            !Read one character
      IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      INERR=0
!
      RETURN
      END
!*******************************************************************************
      SUBROUTINE DATE1(CDATE)
!
! Replaces the DATE Fortran instrinsic and allows programs to compile without
! Y2000 warnings.  Returns date as DD-MMM-YY.
!
      IMPLICIT  NONE
      INTEGER*4 IVAL(8)
      CHARACTER CMON*3(12),CDATE*9,CDATE1*8
      DATA      CMON /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     &    'SEP','OCT','NOV','DEC'/

      CALL DATE_AND_TIME(CDATE1,,,IVAL)
      WRITE(CDATE,'(A)') CDATE1(7:8)//'-'//CMON(IVAL(2))//'-'//
     &    CDATE1(3:4)
      RETURN
      END
