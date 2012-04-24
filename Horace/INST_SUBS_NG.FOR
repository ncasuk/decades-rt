!
! INST_SUBS_NG.FOR - Routines which are common to both H_JAVASERVE and
! H_DISPLAY_NG.  Note that these routines are different, though based on, the
! routines used by the old H_DISPLAY program which ran with the GGS graphics
! cards or the PV-Wave graphics card emulator.
!
! INU_MENU
! INU_DISP
! INU_ERRS
! INU_ISI_MENU
! INU_ISI_CDU
! INU_ISI_DISP(IM)
! INU_DISP_DATA_VT
! INU_DISP_ERRS_VT
! INU_TRANS_ACCL
! INU_MERGE
! NEVZ_DISP
! NEVZ_MEANPARAM
!
!*******************************************************************************
      SUBROUTINE INU_DISP
!
! Puts up the full INU display.  The routine puts up the background field, and
! then updates it once a second as new information is received from the INU
! via the DRS. This display has no provision for the
! display of past data.  All display is done in VT220 mode, rather than using
! the SBD graphics capability, and as a result may be rather small on the
! aircraft scientist's display
!
! Subroutines: INU_DISP_DATA_VT INST_SUBS.FOR
!
! V1.00  13/07/93  W.D.N.JACKSON
!
      IMPLICIT  NONE
      INTEGER*4 IPTR,INULOC,IEDGES,ILSTPTR,IKEY,IDUMMY
      INTEGER*2 JDUMMY
      LOGICAL*4 LOOP,LWAITING
      INCLUDE   'HCOM_DEF.FOR'

      CALL INU_DISP_DATA_VT(JDUMMY,IDUMMY,'BACKGROUND') !Put up background
      CALL VT_OUT(' ')
      CALL VT_OUT(' E - Exit')
      LOOP=.TRUE.
      DO WHILE(LOOP)                   !Keep looping till user wants to exit
        IPTR=STATUS(1)                 !Points to latest DRS data block
        INULOC=NPLOC(163)
        IF(IPTR.GE.1.AND.IPTR.LE.2.AND.STATUS(33).EQ.1
     _      .AND.INULOC.GT.0)          !If valid update display
     -      CALL INU_DISP_DATA_VT(NFDATA(1,IPTR),INULOC,
     -      'UPDATE')                  !Display INU data
!
! Now wait for a valid key to be pressed, or for another DRS block to be
! indicated.
!
        IEDGES=0
        ILSTPTR=IPTR
        LWAITING=.TRUE.
        DO WHILE(LWAITING)
          CALL CHECK_KEY('Ee',IKEY)
          IF(IKEY.NE.0) THEN           !If valid key
            LWAITING=.FALSE.           !Show got one
            LOOP=.FALSE.               !Enable loop exit
          END IF
          IF(STATUS(1).NE.ILSTPTR) THEN
            IEDGES=IEDGES+1
            ILSTPTR=STATUS(1)
            IF(IEDGES.GE.3) LWAITING=.FALSE. !Wait for ~3 secs before next disp
          END IF
          IF(LWAITING) CALL LIB$WAIT(0.2) !Wait 0.2s before trying again
        END DO
      END DO
      CALL VT_OUT(CHAR(0)//CHAR(27)//'[24;79H') !Put cursor at end
      RETURN
10    FORMAT(A)
      END
!*******************************************************************************
      SUBROUTINE INU_ERRS
!
! Displays full details of all INU and IIU status words, and highlights those
! bits which are set.  The routine puts up the background field, and
! then updates it once a second as new information is received from the INU
! via the DRS. This display has no provision for the
! display of past data.  All display is done in VT220 mode, rather than using
! the SBD graphics capability, and as a result may be rather small on the
! aircraft scientist's display
!
! Subroutines: INU_DISP_ERRS_VT INST_SUBS.FOR
!              ggs           GGS.OLB
!
! V1.00  13/07/93  W.D.N.JACKSON
!
      IMPLICIT  NONE
      INTEGER*4 IPTR,INULOC,IEDGES,ILSTPTR,IKEY,IDUMMY
      INTEGER*2 JDUMMY
      LOGICAL*4 LOOP,LWAITING
      INCLUDE   'HCOM_DEF.FOR'

      CALL INU_DISP_ERRS_VT(JDUMMY,IDUMMY,'BACKGROUND') !Put up background
      CALL VT_OUT(' E - Exit')
      LOOP=.TRUE.
      DO WHILE(LOOP)                   !Keep looping till user wants to exit
        IPTR=STATUS(1)                 !Points to latest DRS data block
        INULOC=NPLOC(163)
        IF(IPTR.GE.1.AND.IPTR.LE.2.AND.STATUS(33).EQ.1
     _      .AND.INULOC.GT.0)          !If valid update display
     -      CALL INU_DISP_ERRS_VT(NFDATA(1,IPTR),INULOC,
     -      'UPDATE')                  !Display INU data
!
! Now wait for a valid key to be pressed, or for another DRS block to be
! indicated.
!
        IEDGES=0
        ILSTPTR=IPTR
        LWAITING=.TRUE.
        DO WHILE(LWAITING)
          CALL CHECK_KEY('Ee',IKEY)
          IF(IKEY.NE.0) THEN           !If valid key
            LWAITING=.FALSE.           !Show got one
            LOOP=.FALSE.               !Enable loop exit
          END IF
          IF(STATUS(1).NE.ILSTPTR) THEN
            IEDGES=IEDGES+1
            ILSTPTR=STATUS(1)
            IF(IEDGES.GE.3) LWAITING=.FALSE. !Wait for ~3 secs before next disp
          END IF
          IF(LWAITING) CALL LIB$WAIT(0.2) !Wait 0.2s before trying again
        END DO
      END DO
      CALL VT_OUT(CHAR(0)//CHAR(27)//'[24;79H') !Put cursor at end
      RETURN
10    FORMAT(A)
      END
C*******************************************************************************
      SUBROUTINE NEVZ_DISP
C
C Puts up the Nevzorov display.  The routine puts up the background field, and
C then updates it once a second as new information is received from the
C Nevzorov via the DRS. This display has no provision for the
C display of past data.  All display is done in VT220 mode, rather than using
C the SBD graphics capability, and as a result may be rather small on the
C aircraft scientist's display
C
C V1.00  18/01/99  W.D.N.JACKSON
C
      IMPLICIT  NONE
      INTEGER*4 IOS,IPTR,ILWCAL,ITWCAL,NVLW,NVLR,NVLC,
     -    NVTW,NVTR,NVTC,IKEY,I,LAN,ILSTPTR
      LOGICAL*4 LOOP,LWAITING
      CHARACTER CLINE*80,CE*3,CTIME*8
      REAL*4    RLW,RLC,RLR,RTW,RTC,RTR,RNVLR,RNVLC,RNVTR,RNVTC
      REAL*4    CAL208(3),CAL211(3)
      INCLUDE   'HCOM_DEF.FOR'

      IF(NPLOC(208).EQ.0) RETURN
      CE=CHAR(0)//CHAR(27)//'['
      OPEN(12,FILE='[HORACE]HOR_CALIB.DAT',STATUS='OLD',READONLY,
     -    IOSTAT=IOS)
      READ(12,'(A)',IOSTAT=IOS) CLINE
      DO WHILE(IOS.EQ.0)
        IF(CLINE(1:6).EQ.'CAL208') READ(CLINE,'(10X,3G)',IOSTAT=IOS)
     -      CAL208
        IF(CLINE(1:6).EQ.'CAL211') READ(CLINE,'(10X,3G)',IOSTAT=IOS)
     -      CAL211
        READ(12,'(A)',IOSTAT=IOS) CLINE
      END DO
      CLOSE(12,IOSTAT=IOS)
      CALL VT_OUT(CE//'2J'//CHAR(27)//'[1;1H')
      CALL VT_OUT(CHAR(0)//'                     Nevzorov Probe'//
     -    CHAR(13))
      CALL VT_OUT(' ')
      CALL VT_OUT('                     Raw       Calibrated')
      CALL VT_OUT(' ')
      CALL VT_OUT(' Liquid Water                           gm-3')
      CALL VT_OUT(' Liquid Reference                       V')
      CALL VT_OUT(' Liquid Collector                       V')
      CALL VT_OUT(' Liquid Calibrate')
      CALL VT_OUT(' ')
      CALL VT_OUT(' Total Water                            gm-3')
      CALL VT_OUT(' Total Reference                        V')
      CALL VT_OUT(' Total Collector                        V')
      CALL VT_OUT(' Total Calibrate')
      CALL VT_OUT(' ')
      CALL VT_OUT(' DRS time')
      CALL VT_OUT(' ')
      CALL VT_OUT(' E - Exit')
      LOOP=.TRUE.
      DO WHILE(LOOP)                   !Keep looping till user wants to exit
        IPTR=STATUS(1)                 !Points to latest DRS data block
        IF(IPTR.GE.1.AND.IPTR.LE.2.AND.STATUS(33).EQ.1) THEN !If valid 
          ILWCAL=0
          ITWCAL=0
          IF(NPLOC(27).NE.0) THEN
            IF(BTEST(NFDATA(NPLOC(27),IPTR),1)) ILWCAL=1
            IF(BTEST(NFDATA(NPLOC(27),IPTR),2)) ITWCAL=1
          END IF
          CALL NEVZ_MEANPARAM(208,RLW)
          CALL NEVZ_MEANPARAM(209,RLR)
          CALL NEVZ_MEANPARAM(210,RLC)
          CALL NEVZ_MEANPARAM(211,RTW)
          CALL NEVZ_MEANPARAM(212,RTR)
          CALL NEVZ_MEANPARAM(213,RTC)
          NVLW=NINT(RLW)
          NVLR=NINT(RLR)
          NVLC=NINT(RLC)
          NVTW=NINT(RTW)
          NVTR=NINT(RTR)
          NVTC=NINT(RTC)
          RNVLR=CAL208(1)+CAL208(2)*RLR
          RNVLC=CAL208(1)+CAL208(2)*RLC
          RNVTR=CAL211(1)+CAL211(2)*RTR
          RNVTC=CAL211(1)+CAL211(2)*RTC
          CALL DRS_CTIM(NFDATA(1,STATUS(1)),CTIME)
          WRITE(CLINE,11,IOSTAT=I) CE//'5;19H',NVLW,RNVLW(STATUS(2))
          CALL VT_OUT(CLINE(1:LAN(CLINE)))
          WRITE(CLINE,12,IOSTAT=I) CE//'6;19H',NVLR,RNVLR
          CALL VT_OUT(CLINE(1:LAN(CLINE)))
          WRITE(CLINE,12,IOSTAT=I) CE//'7;19H',NVLC,RNVLC
          CALL VT_OUT(CLINE(1:LAN(CLINE)))
          WRITE(CLINE,13,IOSTAT=I) CE//'8;23H',ILWCAL
          CALL VT_OUT(CLINE(1:LAN(CLINE)))
          WRITE(CLINE,11,IOSTAT=I) CE//'10;19H',NVTW,RNVTW(STATUS(2))
          CALL VT_OUT(CLINE(1:LAN(CLINE)))
          WRITE(CLINE,12,IOSTAT=I) CE//'11;19H',NVTR,RNVTR
          CALL VT_OUT(CLINE(1:LAN(CLINE)))
          WRITE(CLINE,12,IOSTAT=I) CE//'12;19H',NVTC,RNVTC
          CALL VT_OUT(CLINE(1:LAN(CLINE)))
          WRITE(CLINE,13,IOSTAT=I) CE//'13;23H',ITWCAL
          CALL VT_OUT(CLINE(1:LAN(CLINE)))
          CALL VT_OUT(CE//'15;16H'//CTIME)
          CALL VT_OUT(CE//'24;79H') !Put cursor at end
        END IF
C
C Now wait for a valid key to be pressed, or for another DRS block to be
C indicated.
C
        ILSTPTR=IPTR
        LWAITING=.TRUE.
        DO WHILE(LWAITING)
          CALL CHECK_KEY('Ee',IKEY)
          IF(IKEY.NE.0) THEN           !If valid key
            LWAITING=.FALSE.           !Show got one
            LOOP=.FALSE.               !Enable loop exit
          END IF
          IF(STATUS(1).NE.ILSTPTR) THEN
            LWAITING=.FALSE.           !Show got one
          END IF
          IF(LWAITING) CALL LIB$WAIT(0.2) !Wait 0.2s before trying again
        END DO
      END DO
      CALL VT_OUT(CHAR(0)//CHAR(27)//'[24;79H') !Put cursor at end
      RETURN
10    FORMAT(A)
11    FORMAT(A,I5,6X,1PE8.2E1)
12    FORMAT(A,I5,6X,F5.2)
13    FORMAT(A,I1.1)
      END
C*******************************************************************************
      SUBROUTINE NEVZ_MEANPARAM(IP,RESULT)
C
C Calculates then mean value of a DRS parameter over a period of 1s.
C The result is returned as a real
C positive number.  This subroutine should only be used for parameters
C which are recorded as unsigned 12 bit binary numbers, ie not for BCD and
C not for 16 bit parameters.
C
C V1.00  01/01/89  W.D.N.JACKSON
C V1.01  01/07/93  W.D.N.JACKSON  Changed to accomodate 16 bit recording
C
      IMPLICIT NONE
      INTEGER*2 NFDATA(2048,2),NPLOC(512),NPFREQ(512),STATUS(256)
      INTEGER*2 SPECIAL(12800,10)
      INTEGER*4 IP,JSUM,JCNT,I,J
      REAL*4    DERIVE(12800,196),RESULT
      COMMON    /HCOM/ NFDATA,NPLOC,NPFREQ,STATUS,DERIVE,SPECIAL
      VOLATILE  /HCOM/
C
C External data.
C
C IP      I*4  Read    Passed         Parameter number (1 to 512)
C RESULT  R*4  Write   Passed         Mean value of parameter over 1s block
C NFDATA  I*2  Read    H_DRS_LOG      Block of compressed DRS data (see
C                                     H_DRS_LOG for format)
C NPLOC   I*2  Read    H_DERIVE       Location in NFDATA where the data for
C                                     each parameter starts. (0 if the 
C                                     parameter was not recorded, else
C                                     in range 9 to about 2000)
C NPFREQ  I*2  Read    H_DERIVE       The sampling rate of each parameter
C                                     (0 if not recorded, else in range 1 
C                                     to 64 at present)
C
C Note that the DRS only records bits 0 to 11 for most parameters, and this
C subroutine masks out the top four bits of each word.
C
      RESULT=0.0
      IF(NPLOC(IP).NE.0) THEN          !If parameter recorded
        JSUM=0
        JCNT=0
        DO I=NPLOC(IP),NPLOC(IP)+NPFREQ(IP)-1
!          J=NFDATA(I,STATUS(1))
          J=JZEXT(NFDATA(I,STATUS(1)))
!          JSUM=JSUM+(J.AND.'FFF'X)
          JSUM=JSUM+J
          JCNT=JCNT+1
        END DO
        RESULT=FLOAT(JSUM)/JCNT        !Work out mean
      END IF
      RETURN
      END
C*******************************************************************************
      SUBROUTINE INU_MENU
C
C Displays the INU menu options, and calls the appropriate routines to execute
C them.
C
C Subroutines: INU_DISP      INST_SUBS.FOR
C              INU_ERRS      INST_SUBS.FOR
C              INU_ISI_MENU  INST_SUBS.FOR
C              DRS_FOOT      INST_SUBS.FOR
C              DRS_PRINT     INST_SUBS.FOR
C              GPS_SELECT    INST_SUBS.FOR
C              ggs           GGS.OLB
C
C V1.00  13-07-93  W.D.N.JACKSON
C
      IMPLICIT  NONE
      CHARACTER CLINE(7)*50,COPTION*4
      INTEGER*4 I,IOS,IOPTION,NOPTIONS,ILEN
      LOGICAL*4 LINU,LSELECT
C
C INU menu contents
C
      DATA CLINE(1)/' '/
      DATA CLINE(2)/'                        INU Menu'/
      DATA CLINE(3)/' '/
      DATA CLINE(4)/' 0) Exit menu'/
      DATA CLINE(5)/' 1) Display INU data'/
      DATA CLINE(6)/' 2) Display INU errors and status'/
      DATA CLINE(7)/' 3) Use INU Serial Interface'/
      DATA NOPTIONS /4/
C
      LINU=.TRUE.
      DO WHILE(LINU)                   !Keep looping till leave menu
        CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H')
        CALL VT_OUT(CHAR(0)//CLINE(1)//CHAR(13))
        DO I=2,NOPTIONS+3              !Display the options
          CALL VT_OUT(' '//CLINE(I))
        END DO
        CALL VT_OUT(' ')
        LSELECT=.TRUE.
        DO WHILE(LSELECT)              !Loop until have valid entry
          CALL VT_OUT('$Enter selection [0]... ')
          CALL VT_IN(COPTION,ILEN,IOS)
          IF(IOS.EQ.-1) RETURN
          IF(ILEN.EQ.0) COPTION='0'
          READ(COPTION,'(BN,I)',IOSTAT=IOS) IOPTION
          IF(IOS.EQ.-1) RETURN
          IF(IOPTION.EQ.0) RETURN
          IF(IOPTION.GE.1.AND.IOPTION.LT.NOPTIONS) LSELECT=.FALSE.
        END DO
        CALL VT_OUT('  ')
!        WRITE(10,'(A)',IOSTAT=IOS) ' '
        IF(IOPTION.EQ.1) THEN
          CALL INPUT_AST               !Enable unsolicited input
          CALL INU_DISP                !Display INU data
          CALL INPUT_ABORT
        ELSE IF(IOPTION.EQ.2) THEN
          CALL INPUT_AST               !Enable unsolicited input
          CALL INU_ERRS                !Display INU errors
          CALL INPUT_ABORT
        ELSE IF(IOPTION.EQ.3) THEN
          CALL INU_ISI_MENU            !Use INU serial interface
        END IF
      END DO                           !Else give this menu again
      RETURN
10    FORMAT(I1)
      END
C*******************************************************************************
      SUBROUTINE INU_ISI_DISP(IM)
C
C Get and displays an INU messages every two seconds via the INU Serial Interfac
C All display is done in VT220 mode.  IM specifies the message.  The following
C messages can be handled: I01,I06,I07,I09,I10,I11,I12,I13,I14.  If IM is 99
C the miscellaneous parameters are displayed - these are always available 
C without needing to send messages to get them.
C
C Subroutines: INU_ISI_DISP_MESS_VT INST_SUBS.FOR
C              ggs               GGS.OLB
C
C V1.00  26/11/93  W.D.N.JACKSON
C
      IMPLICIT  NONE
      INTEGER*4 IM,ICNT,IKEY,ISTAT
      LOGICAL*4 LOOP,LWTDATA,LWAITING
      REAL*4    RD1,RD2
      INCLUDE   'HCOM_DEF.FOR'

      CALL INU_ISI_DISP_MESS_VT(IM,'BACKGROUND') !Put up background
      CALL VT_OUT(' ')
      CALL VT_OUT(' E - Exit')
C
C Now wait for a valid key to be pressed, or for another DRS block to be
C indicated.
C
      LOOP=.TRUE.
      DO WHILE(LOOP)                   !Keep looping once a sec till user exits
        IF(IM.NE.99) CALL INU_ISI_COMM(IM,'SEND',RD1,RD2,ISTAT) !Get message
        LWTDATA=.TRUE.                 !Show waiting for reply
        ICNT=15                        !Will wait three secs before next message
        LWAITING=.TRUE.
        DO WHILE(LWAITING)
          CALL CHECK_KEY('Ee',IKEY)
          IF(IKEY.NE.0) THEN           !If valid key pressed
            LWAITING=.FALSE.           !Show got one
            LOOP=.FALSE.               !Enable loop exit
          ELSE IF(LWTDATA) THEN        !If waiting for message to come
            IF(IM.NE.99) CALL INU_ISI_COMM(IM,'CHECK',RD1,RD2,ISTAT)
            IF(ISTAT.EQ.0.OR.IM.EQ.99) THEN
              CALL INU_ISI_DISP_MESS_VT(IM,'DATA')
              LWTDATA=.FALSE.
            ELSE IF(ISTAT.EQ.-1) THEN
              CALL INU_ISI_DISP_MESS_VT(IM,'DATAZ')
              LWTDATA=.FALSE.
            END IF
          END IF
          ICNT=ICNT-1
          IF(ICNT.LE.0) LWAITING=.FALSE.  !Break out once a second
          IF(LWAITING) CALL LIB$WAIT(0.2) !Wait 0.2s before trying again
        END DO
      END DO
      CALL VT_OUT(CHAR(0)//CHAR(27)//'[24;79H') !Put cursor at end
      RETURN
10    FORMAT(A)
      END
C*******************************************************************************
      SUBROUTINE INU_ISI_MENU
C
C Displays the INU Serial Interface menu options, and calls the appropriate 
C routines to execute them.  If the ISI is in use by another display, this
C routine returns immediately to the next level up.  As long as this, or lower,
C menus are in use the ISI cannot be used by other displays.
C
C Subroutines: INU_ISI_DISP  INST_SUBS.FOR
C              INU_ISI_CDU   INST_SUBS.FOR
C              DRS_FOOT      INST_SUBS.FOR
C              DRS_PRINT     INST_SUBS.FOR
C              GPS_SELECT    INST_SUBS.FOR
C              ggs           GGS.OLB
C
C V1.00  26-11-93  W.D.N.JACKSON
C
      IMPLICIT  NONE
      INTEGER*4 NOPTIONS,I,IOPTION,IOS,ILEN
      LOGICAL*4 LISI,LSELECT
      INCLUDE   'HCOM_DEF.FOR'
      CHARACTER CLINE(18)*50,COPTION*4
C
C INU Serial Interface menu contents
C
      DATA CLINE(1)/' '/
      DATA CLINE(2)/'                        ISI Menu'/
      DATA CLINE(3)/' '/
      DATA CLINE(4)/'This menu is not intended for use in flight.'/
      DATA CLINE(5)
     -    /'This menu should only be used by an INU expert.'/
      DATA CLINE(6)
     -    /'The ISI can only be used by one display at a time.'/
      DATA CLINE(7)/' '/
      DATA CLINE(8) /'  0) Exit menu'/
      DATA CLINE(9) /'  1) INU Message I01         6) INU Message I11'/
      DATA CLINE(10)/'  2) INU Message I06         7) INU Message I12'/
      DATA CLINE(11)/'  3) INU Message I07         8) INU Message I13'/
      DATA CLINE(12)/'  4) INU Message I09         9) INU Message I14'/
      DATA CLINE(13)/'  5) INU Message I10        10) INU Misc Paras '/
      DATA CLINE(14)/' 11) Emergency INU Control'/
      DATA NOPTIONS /11/
C
      IF(INUDATA(214).NE.0) THEN
        RETURN
      ELSE
        INUDATA(214)=1
      END IF

      LISI=.TRUE.
      DO WHILE(LISI)                   !Keep looping till leave menu
        CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H')
        CALL VT_OUT(CHAR(0)//CLINE(1)//CHAR(13))
        DO I=2,14                      !Display the options
          CALL VT_OUT(' '//CLINE(I))
        END DO
        CALL VT_OUT(' ')
        LSELECT=.TRUE.
        DO WHILE(LSELECT)              !Loop until have valid entry
          CALL VT_OUT('$Enter selection [0]... ')
          CALL VT_IN(COPTION,ILEN,IOS)
          IF(IOS.NE.0) RETURN
          IF(ILEN.EQ.0) COPTION='0'
          READ(COPTION,'(BN,I)',IOSTAT=IOS) IOPTION
          IF(IOS.NE.0) IOPTION=0
          IF(IOPTION.GE.0.AND.IOPTION.LE.NOPTIONS) LSELECT=.FALSE.
        END DO
        CALL VT_OUT('  ')
!        WRITE(10,'(A)',IOSTAT=IOS) ' '
        CALL INPUT_AST                  !Enable unsolicited input
        IF(IOPTION.EQ.0) THEN
          LISI=.FALSE.
        ELSE IF(IOPTION.EQ.1) THEN
          CALL INU_ISI_DISP(1)         !Display INU message 01
        ELSE IF(IOPTION.EQ.2) THEN
          CALL INU_ISI_DISP(6)         !Display INU message 06
        ELSE IF(IOPTION.EQ.3) THEN
          CALL INU_ISI_DISP(7)         !Display INU message 07
        ELSE IF(IOPTION.EQ.4) THEN
          CALL INU_ISI_DISP(9)         !Display INU message 09
        ELSE IF(IOPTION.EQ.5) THEN
          CALL INU_ISI_DISP(10)        !Display INU message 10
        ELSE IF(IOPTION.EQ.6) THEN
          CALL INU_ISI_DISP(11)        !Display INU message 11
        ELSE IF(IOPTION.EQ.7) THEN
          CALL INU_ISI_DISP(12)        !Display INU message 12
        ELSE IF(IOPTION.EQ.8) THEN
          CALL INU_ISI_DISP(13)        !Display INU message 13
        ELSE IF(IOPTION.EQ.9) THEN
          CALL INU_ISI_DISP(14)        !Display INU message 14
        ELSE IF(IOPTION.EQ.10) THEN
          CALL INU_ISI_DISP(99)        !Display Misc Parameters
        ELSE IF(IOPTION.EQ.11) THEN
          CALL INPUT_ABORT
          CALL INU_ISI_CDU             !Control the CDU
          CALL INPUT_AST
        END IF
        CALL INPUT_ABORT
      END DO                           !Else give this menu again
      INUDATA(214)=0                   !Show ISI free
      RETURN
10    FORMAT(I2)
      END
C*******************************************************************************
      SUBROUTINE INU_ISI_CDU
C
C Changes the INU operating mode.  Note that only the most obvious mode changes
C are provided.  Also no check is done on whether the required change is valid.
C If it is not valid, the INU will take no action.  When present position is
C required, the user is offered the current GPS position, if available, as a
C default.  Keys A to D allow return to higher level menus before the message
C is sent.  Also has option for clearing INU Maintenance History word, and
C for reading individual Miscellaneous parameters.
C
C V1.00  02/12/93  W.D.N.JACKSON
C
      IMPLICIT  NONE
      CHARACTER CLINE1*56,CLINE(12)*70,CMP*6,CN*2
      CHARACTER CSTRING*10,COPTION*4
      INTEGER*4 LAN,NOPTIONS,I,IOPTION,ISTAT,IN,ILEN,ICNT,IOS
      LOGICAL*4 LSELECT
      REAL*4    RD1,RD2,RMP,RLAT,RLNG
      INCLUDE   'HCOM_DEF.FOR'
      EQUIVALENCE (INUDATA(216),CMP)
      DATA CLINE(1)/' '/
      DATA CLINE(2)/'                 Emergency INU Control'/
      DATA CLINE(3)/' '/
      DATA CLINE(4)
     -/'Any IPC or CDU on the 2nd 1553 network must be switched'/
      DATA CLINE(5)
     -/'off before using these options.'/
      DATA CLINE(6)/' '/
      DATA CLINE(7)/'  0) Exit menu'/
      DATA CLINE(8)/'  1) Change INU mode to GC ALIGN'/
      DATA CLINE(9)/'  2) Change INU mode to NAVIGATE'/
      DATA CLINE(10)/'  3) Change INU mode to TEST'/
      DATA CLINE(11)
     -    /'  4) Read a Miscellaneous Parameter'/
      DATA CLINE(12)
     -    /'  5) Dump INU data to H_INU.LOG (allow about 1 min)'/
      DATA NOPTIONS /6/

      CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H')
      CALL VT_OUT(CHAR(0)//CLINE(1)//CHAR(13))
      DO I=2,NOPTIONS+6              !Display the options
        CALL VT_OUT(' '//CLINE(I))
      END DO
      CALL VT_OUT(' ')
      LSELECT=.TRUE.
      DO WHILE(LSELECT)              !Loop until have valid entry
        CALL VT_OUT('$Enter selection [0]... ')
        CALL VT_IN(COPTION,ILEN,IOS)
        IF(IOS.NE.0) RETURN
        IF(ILEN.EQ.0) COPTION='0'
        READ(COPTION,'(BN,I)',IOSTAT=IOS) IOPTION
        IF(IOS.NE.0) RETURN
        IF(IOPTION.EQ.0) RETURN
        IF(IOPTION.GE.1.AND.IOPTION.LT.NOPTIONS) LSELECT=.FALSE.
      END DO
        CALL VT_OUT('  ')
!        WRITE(10,'(A)',IOSTAT=IOS) ' '

      IF(IOPTION.EQ.2) THEN       !NAVIGATE
        CALL INU_ISI_COMM(21,'SEND',RLAT,RLNG,ISTAT)
      ELSE IF(IOPTION.EQ.1.OR.IOPTION.EQ.3) THEN !GC ALIGN or TEST
C
C Show the current GPS position, obtained via the HCOM global common, and see
C if the user is happy to use it to set the INU.
C
        IN=0
        IF(STATUS(35).EQ.1) THEN     !If GPS data is valid
          CALL VT_OUT(' Current GPS position is:')
          RLAT=RGLAT(STATUS(2))
          WRITE(CLINE1,12,IOSTAT=IOS) RLAT
          CALL VT_OUT(' '//CLINE1)
          RLNG=RGLNG(STATUS(2))
          WRITE(CLINE1,13,IOSTAT=IOS) RLNG
          CALL VT_OUT(' '//CLINE1)
          LSELECT=.TRUE.
          DO WHILE(LSELECT)          !Loop till have valid param 1
            CALL VT_OUT('$Use GPS position? 0-No 1-Yes [1]... ') !Get param
            CALL VT_IN(CN,ILEN,IOS)
            IF(IOS.NE.0) RETURN
            IF(ILEN.EQ.0) CN='1'
            READ(CN,'(BN,I)',IOSTAT=IOS) IN
            IF(IOS.NE.0) RETURN
            IF(IN.GE.0.AND.IN.LE.1) LSELECT=.FALSE.
          END DO
        END IF
C
C If the user doesn't want to use the current values, assume that all systems
C are not available so the aircraft is probably on the ground.  Offer the
C Boscombe Down location as the default.
C
        IF(IN.NE.1) THEN
          CALL VT_OUT(' Defaults are Woodford pan')
C
C Get latitude
C
          IOS=1
          DO WHILE(IOS.NE.0)
            CALL VT_OUT('$Latitude (deg) [53.37133] ')
            CALL VT_IN(CSTRING,ILEN,I)
            IF(I.NE.0) RETURN
            IF(ILEN.EQ.0) CSTRING='53.37133'
            READ(CSTRING(1:LAN(CSTRING)),*,IOSTAT=IOS) RLAT
            IF(RLAT.GT.90.0.OR.RLAT.LT.-90.0) IOS=1
          END DO
C
C Get longitude
C
          IOS=1
          DO WHILE(IOS.NE.0)
            CALL VT_OUT('$Longitude (deg) [-2.14883] ')
            CALL VT_IN(CSTRING,ILEN,I)
            IF(I.NE.0) RETURN
            IF(ILEN.EQ.0) CSTRING='-2.14883'
            READ(CSTRING(1:LAN(CSTRING)),*,IOSTAT=IOS) RLNG
            IF(RLNG.GT.180.0.OR.RLNG.LT.-180.0) IOS=1
          END DO
        END IF

        IF(IOPTION.EQ.1) CALL INU_ISI_COMM(20,'SEND',RLAT,RLNG,ISTAT)
        IF(IOPTION.EQ.3) CALL INU_ISI_COMM(22,'SEND',RLAT,RLNG,ISTAT)
      ELSE IF(IOPTION.EQ.4) THEN       !Read Miscellaneous Parameter
        DO WHILE(.TRUE.)
          LSELECT=.TRUE.
          DO WHILE(LSELECT)          !Loop till have valid param 1
            CALL VT_OUT('$Miscellaneous Parameter? [0] ')
            CALL VT_IN(CN,ILEN,IOS)
            IF(IOS.EQ.-1) RETURN
            IF(ILEN.EQ.0) CN='0'
            READ(CN,'(BN,I)',IOSTAT=IOS) IN
            IF(IOS.NE.0) RETURN
            IF(IN.EQ.0) RETURN
            IF(IN.GE.1.AND.IN.LT.200) LSELECT=.FALSE.
          END DO
          CLINE1=' '
          RMP=FLOAT(IN)
          ICNT=15
          CALL INU_ISI_COMM(40,'SEND',RMP,RD2,ISTAT)
          ISTAT=1
          DO WHILE(ISTAT.GT.0.AND.ICNT.GT.0)
            CALL INU_ISI_COMM(40,'CHECK',RMP,RD2,ISTAT)
            IF(ISTAT.GT.0) CALL LIB$WAIT(0.2)
            ICNT=ICNT-1
          END DO
          IF(ISTAT.EQ.0) THEN
            CLINE1='MP '
            WRITE(CLINE1(4:6),14,IOSTAT=IOS) IN           
            CLINE1(10:15)=CMP
            IF(ICHAR(CMP(1:1)).GT.127) THEN
              CLINE1(9:9)='-'
              CLINE1(10:10)=CHAR(ICHAR(CMP(1:1)).AND.'7F'X)
            END IF
          ELSE
            CLINE1=' Unable to read Misc Para'
            WRITE(CLINE1(27:29),14,IOSTAT=IOS) IN           
          END IF
          CALL VT_OUT(CLINE1(1:29))
        END DO
      ELSE IF(IOPTION.EQ.5) THEN       !Force dump of INU data to H_INU log file
        CALL INU_ISI_COMM(41,'SEND',RD1,RD2,ISTAT)
      END IF
      RETURN
11    FORMAT(I1)
12    FORMAT('Latitude ',F8.4,' deg')
13    FORMAT('Longitude ',F8.4,' deg')
14    FORMAT(I3.3)
      END
C*******************************************************************************
C
C ROUTINE          INU_DISP_DATA_VT SUBROUTINE FORTVAX
C
C PURPOSE          Displays INU data on VT320 terminals
C
C DESCRIPTION      Establishes a display of INU information and updates it
C                  as new data are received.  If called with CMODE equal to
C                  BACKGROUND, clears the screen and puts up the legend.
C                  If called with CMODE equal to UPDATE, updates the information
C                  in the corresponding part of the screen.  Mainly displays the
C                  information as recorded but also derives some additional
C                  information to make the display more useful.  Note that the
C                  logical unit must have been opened with fortran carriage
C                  control.
C
C VERSION          1.00  13-07-93  W.D.N.JACKSON
C
C ARGUMENTS        JBUFF       I*2  IN   Block of raw data
C                  INULOC      I*4  IN   Starting point of INU data in block
C                  CMODE       C**  IN   'BACKGROUND' or 'UPDATE'
C
C CHANGES          
C
C*******************************************************************************
      SUBROUTINE INU_DISP_DATA_VT(JBUFF,INULOC,CMODE)
      IMPLICIT  NONE
      INTEGER*4 IL,ILNGM,ILAT,ILNGS,I,ILATD,IMIN,I1,ILATM,INDX,I2,ILATS,
     -    INULOC,IHRS,ILNG,IOS,IINUMDE,ILNGD,ISEC
      REAL*4    RALT,RHGT,RTT,RAP,RLAT,RVE,RAU,RLNG,RVN,RAX,RMHDG,RVX,
     -    RAY,RPAZI,RVY,RAZ,RPITC,RVZ,RCNEXX,RPITR,RWA,RCNEXY,RROLL,
     -    RYAWR,RCNEXZ,RROLR,RGSPD,RTHDG,RAF,RGSPDK,RTRK
      INTEGER*4 IDIFFS(32),ITEMP,LAN
      INTEGER*2 JDATA(32,163:169),JTEMP(2),JBUFF(2048)
      INTEGER*2 JIUV,JIUZ,JIUO,JIUS,JNU141,JNU144,JNUSTS,J,JLSTTT
      CHARACTER CMODE*(*)
      CHARACTER CLINES(24)*79,CE*3,CLNG*1,CLAT*1,CDTIME*8,CL*80
      CHARACTER*30 CIIUSTS(13:16),CINUMD1(16),CINUMD2(16),
     -    CINUMDE,CINUERR,CIIUERR
      EQUIVALENCE (ITEMP,JTEMP)
C
      DATA CIIUSTS(13) /'IIU 1553 chip error'/
      DATA CIIUSTS(14) /'No baro input to IIU'/
      DATA CIIUSTS(15) /'No response from INU'/
      DATA CIIUSTS(16) /'No response from IIU'/
C
      DATA CINUMD1(1)  /'Inertial Sensor/Ref Fail'/
      DATA CINUMD1(2)  /'Any Navigation Data Fail'/
      DATA CINUMD1(3)  /'Degraded Navigation'/
      DATA CINUMD1(4)  /'Navigation Data Unavailable'/
      DATA CINUMD1(5)  /'Digital Attitude Data Fail'/
      DATA CINUMD1(6)  /'Degraded NAV Ready'/
      DATA CINUMD1(7)  /'Attitude Ready'/
      DATA CINUMD1(8)  /'Control Vector Acknowledge'/
      DATA CINUMD1(9)  /'Altitude Loop Bit'/
      DATA CINUMD1(10) /'INU in Initiated BIT'/
      DATA CINUMD1(11) /'Mechanization Flag'/
      DATA CINUMD1(12) /'In Manual Magnetic Heading'/
      DATA CINUMD1(13) /'Digital Select'/
      DATA CINUMD1(14) /'In Grid Mode'/
      DATA CINUMD1(15) /'CDU Fail'/
      DATA CINUMD1(16) /'-'/
C
      DATA CINUMD2(1)  /'OFF'/
      DATA CINUMD2(2)  /'STANDBY'/
      DATA CINUMD2(3)  /'AIR ALIGN'/
      DATA CINUMD2(4)  /'STORED HEADING ALIGN'/
      DATA CINUMD2(5)  /'GC ALIGN'/
      DATA CINUMD2(6)  /'-'/
      DATA CINUMD2(7)  /'-'/
      DATA CINUMD2(8)  /'-'/
      DATA CINUMD2(9)  /'NAVIGATE'/
      DATA CINUMD2(10) /'OVERFLY'/
      DATA CINUMD2(11) /'AUXILIARY'/
      DATA CINUMD2(12) /'ORIENT-BORESIGHT'/
      DATA CINUMD2(13) /'ATTITUDE'/
      DATA CINUMD2(14) /'TEST'/
      DATA CINUMD2(15) /'-'/
      DATA CINUMD2(16) /'-'/

      DATA CLINES(1)(1:40) /'                                        '/
      DATA CLINES(2)(1:40) /'                              Honeywell '/
      DATA CLINES(3)(1:40) /'                                        '/
      DATA CLINES(4)(1:40) /'VX         m/s  VN         m/s  LAT     '/
      DATA CLINES(5)(1:40) /'VY         m/s  VE         m/s  LNG     '/
      DATA CLINES(6)(1:40) /'VZ         m/s                  ALT     '/
      DATA CLINES(7)(1:40) /'                                        '/
      DATA CLINES(8)(1:40) /'ROLL         deg   ROLL rate         deg'/
      DATA CLINES(9)(1:40) /'PTCH         deg   PTCH rate         deg'/
      DATA CLINES(10)(1:40)/'T/HDG        deg   YAW rate          deg'/
      DATA CLINES(11)(1:40)/'M/HDG        deg                        '/
      DATA CLINES(12)(1:40)/'P/AZI        deg   Wander ang        deg'/
      DATA CLINES(13)(1:40)/'                                        '/
      DATA CLINES(14)(1:40)/'AX         m/s/s   A/forw       g       '/
      DATA CLINES(15)(1:40)/'AY         m/s/s   A/port       g       '/
      DATA CLINES(16)(1:40)/'AZ         m/s/s   A/vert       g       '/ 
      DATA CLINES(17)(1:40)/'                                        '/
      DATA CLINES(18)(1:40)/'IIU zeros / ones  / status:             '/
      DATA CLINES(19)(1:40)/'INU I14-1 / I14-4 / status:             '/
      DATA CLINES(20)(1:40)/'                                        '/
      DATA CLINES(21)(1:40)/'Time tag:          s  Diffs:            '/
      DATA CLINES(22)(1:40)/'DRS time:                               '/
      DATA CLINES(23)(1:40)/'                                        '/
      DATA CLINES(24)(1:40)/'                                        '/
      DATA CLINES(1)(41:79) /'                                       '/
      DATA CLINES(2)(41:79) /'H-423 INU                              '/
      DATA CLINES(3)(41:79) /'                                       '/
      DATA CLINES(4)(41:79) /'      d   G/SPD          m/s        kts'/
      DATA CLINES(5)(41:79) /'      d   Track          deg           '/
      DATA CLINES(6)(41:79) /'      ft  Height         km            '/
      DATA CLINES(7)(41:79) /'                                       '/
      DATA CLINES(8)(41:79) /'/s   CNEXX                             '/
      DATA CLINES(9)(41:79) /'/s   CNEXY                             '/
      DATA CLINES(10)(41:79)/'/s   CNEXZ                             '/
      DATA CLINES(11)(41:79)/'                                       '/
      DATA CLINES(12)(41:79)/'                                       '/
      DATA CLINES(13)(41:79)/'                                       '/    
      DATA CLINES(14)(41:79)/'     INU mode:                         '/
      DATA CLINES(15)(41:79)/'     IIU vers:                         '/
      DATA CLINES(16)(41:79)/'                                       '/
      DATA CLINES(17)(41:79)/'                                       '/
      DATA CLINES(18)(41:79)/'                                       '/
      DATA CLINES(19)(41:79)/'                                       '/
      DATA CLINES(20)(41:79)/'                                       '/
      DATA CLINES(21)(41:79)/'                                     ms'/
      DATA CLINES(22)(41:79)/'                                       '/
      DATA CLINES(23)(41:79)/'                                       '/
      DATA CLINES(24)(41:79)/'                                       '/

      CE=CHAR(0)//CHAR(27)//'['
      IF(CMODE.EQ.'BACKGROUND') THEN
        CALL VT_OUT(CE//'2J'//CHAR(27)//'[1;1H')
        DO IL=2,21
          CALL VT_OUT(CHAR(0)//CLINES(IL)//CHAR(13)//CHAR(10))
        END DO
        CALL VT_OUT(CHAR(0)//CLINES(22)//CHAR(13))
      END IF
      IF(CMODE.EQ.'UPDATE') THEN
        I1=JZEXT(JBUFF(10).AND.'FFF'X)    !Get hours and mins
        I2=JZEXT(JBUFF(11).AND.'FFF'X)    !Get mins and secs
        IHRS=IBITS(I1,8,4)*10+IBITS(I1,4,4) !Compute hours
        IMIN=IBITS(I1,0,4)*10+IBITS(I2,8,4) !Compute minute
        ISEC=IBITS(I2,4,4)*10+IBITS(I2,0,4) !Compute seconds
        WRITE(CDTIME,23,IOSTAT=IOS) IHRS,IMIN,ISEC
        INDX=INULOC                    !Transfer INU data to local array
        DO I=163,169
          DO J=1,32
            JDATA(J,I)=JBUFF(INDX)
            INDX=INDX+1
          END DO
        END DO
        JTEMP(2)=JDATA(3,163)
        JTEMP(1)=JDATA(4,163)
        RVX=ITEMP/2.**18*12*25.4/1000.
        JTEMP(2)=JDATA(5,163)
        JTEMP(1)=JDATA(6,163)
        RVY=ITEMP/2.**18*12*25.4/1000.
        JTEMP(2)=JDATA(7,163)
        JTEMP(1)=JDATA(8,163)
        RVZ=ITEMP/2.**18*12*25.4/1000.
        RALT=JDATA(25,163)*4.
        RGSPD=SQRT(RVX**2+RVY**2)
        RGSPDK=RGSPD*1.944
        RHGT=RALT*12.*25.4/1000./1000.
        RROLL=JDATA(10,163)/2.**15*180.
        RPITC=JDATA(11,163)/2.**15*180.
        RMHDG=JDATA(13,163)/2.**15*180.
        IF(RMHDG.LT.0.) RMHDG=RMHDG+360.
        RTHDG=JDATA(12,163)/2.**15*180.
        IF(RTHDG.LT.0.) RTHDG=RTHDG+360.
        RPAZI=JDATA(9,163)/2.**15*180.
        IF(RPAZI.LT.0.) RPAZI=RPAZI+360.
        RROLR=JDATA(30,163)/2.**13*180.
        RPITR=JDATA(31,163)/2.**13*180.
        RYAWR=JDATA(32,163)/2.**13*180.
        JTEMP(2)=JDATA(17,163)
        JTEMP(1)=JDATA(18,163)
        RCNEXX=ITEMP/2.**30
        JTEMP(2)=JDATA(19,163)
        JTEMP(1)=JDATA(20,163)
        RCNEXY=ITEMP/2.**30
        JTEMP(2)=JDATA(21,163)
        JTEMP(1)=JDATA(22,163)
        RCNEXZ=ITEMP/2.**30
        RWA=0.
        IF(RCNEXX.NE.0) RWA=ATAN2D(-RCNEXY,RCNEXX)
        RVN=COSD(RWA)*RVX-SIND(RWA)*RVY
        RVE=-SIND(RWA)*RVX-COSD(RWA)*RVY
        RTRK=0.
        IF(RVN.NE.0) RTRK=ATAN2D(RVE,RVN)
        IF(RTRK.LT.0.) RTRK=RTRK+360.
        IF(RCNEXZ.GE.-1.AND.RCNEXZ.LE.1.) RLAT=ASIND(RCNEXZ)
        CLAT='N'
        IF(RLAT.LT.0.) THEN
          CLAT='S'
          RLAT=-RLAT
        END IF
        JTEMP(2)=JDATA(23,163)
        JTEMP(1)=JDATA(24,163)
        RLNG=ITEMP*180./2.**31
        CLNG='E'
        IF(RLNG.LT.0.) THEN
          CLNG='W'
          RLNG=-RLNG
        END IF
        ILNG=RLNG*3600
        ILNGD=ILNG/3600                !Degrees
        ILNGM=MOD(ILNG/60,60)          !Minutes
        ILNGS=(MOD(ILNG,60)*100)/60    !Hundreths of minutes
        ILAT=RLAT*3600
        ILATD=ILAT/3600
        ILATM=MOD(ILAT/60,60)
        ILATS=(MOD(ILAT,60)*100)/60
        RAX=JDATA(14,163)/2.**5*12.*25.4/1000.
        RAY=JDATA(15,163)/2.**5*12.*25.4/1000.
        RAZ=JDATA(16,163)/2.**5*12.*25.4/1000.
        CALL INU_TRANS_ACCL(RAX,RAY,RAZ,RROLL,RPITC,RPAZI,RAF,RAP,RAU)
        IINUMDE=JDATA(29,163)
        CINUMDE=' '
        DO I=1,16
          J=16-I
          IF(BTEST(IINUMDE,J)) CINUMDE=CINUMD2(I)
        END DO
        JIUV=JDATA(1,164).AND.'7'X
        JIUZ=JDATA(1,167)
        JIUO=JDATA(1,168)
        JIUS=JDATA(1,164).AND.'FFF8'X
        JNU141=JDATA(1,165)
        JNU144=JDATA(1,166)
        JNUSTS=JDATA(1,163)
        JNUSTS=JNUSTS.AND..NOT.'20'X   !Mask off mechanization flag (always 1)
        CIIUERR=' '
        DO I=13,16
          IF(BTEST(JIUS,I-1)) CIIUERR='- '//CIIUSTS(I)
        END DO
        CINUERR=' '
        DO I=16,1,-1
          J=16-I
          IF(BTEST(JNUSTS,J)) CINUERR='- '//CINUMD1(I)
        END DO
        RTT=JZEXT(JDATA(2,163))*64E-6
        IDIFFS(1)=NINT((JZEXT(JDATA(2,163))-JZEXT(JLSTTT))*64E-6*1000.)
        IDIFFS(2)=NINT((JZEXT(JDATA(2,164))-JZEXT(JDATA(2,163)))
     -      *64E-6*1000.)+1000.
        DO I=3,32
          IDIFFS(I)=NINT((JZEXT(JDATA(I,164))-JZEXT(JDATA(I-1,164)))
     -        *64E-6*1000.)
        END DO
        JLSTTT=JDATA(32,164)
        IF(BTEST(JIUS,15).OR.BTEST(JIUS,14)) THEN !Zero outputs if no reponse
          RVX=0.
          RVY=0.
          RVZ=0.
          RVN=0.
          RVE=0.
          ILATD=0
          ILATM=0
          ILATS=0
          CLAT=' '
          ILNGD=0
          ILNGM=0
          ILNGS=0
          CLNG=' '
          RALT=0.
          RGSPD=0.
          RGSPDK=0.
          RTRK=0.
          RHGT=0.
          RROLL=0.
          RPITC=0.
          RTHDG=0.
          RMHDG=0.
          RPAZI=0.
          RROLR=0.
          RPITR=0.
          RYAWR=0.
          RWA=0.
          RCNEXX=0.
          RCNEXY=0.
          RCNEXZ=0.
          RAX=0.
          RAY=0.
          RAZ=0.
          RAF=0.
          RAP=0.
          RAU=0.
          CINUMDE(1:25)=' '
          CINUERR=' '
          JNUSTS=0
          JNU141=0
          JNU144=0
          IF(BTEST(JIUS,15)) JIUV=0
          RTT=0.
          DO I=1,32
            IDIFFS(I)=0
          END DO
        END IF
        WRITE(CL,11,IOSTAT=I) CE//'3;4H',RVX
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,11,IOSTAT=I) CE//'4;4H',RVY
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,11,IOSTAT=I) CE//'5;4H',RVZ
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,11,IOSTAT=I) CE//'3;20H',RVN
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,11,IOSTAT=I) CE//'4;20H',RVE
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,21,IOSTAT=I) CE//'3;37H',ILATD,ILATM,ILATS,CLAT
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,21,IOSTAT=I) CE//'4;37H',ILNGD,ILNGM,ILNGS,CLNG
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,12,IOSTAT=I) CE//'5;41H',NINT(RALT)
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,11,IOSTAT=I) CE//'3;58H',RGSPD
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,11,IOSTAT=I) CE//'3;69H',RGSPDK
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,11,IOSTAT=I) CE//'4;58H',RTRK
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,22,IOSTAT=I) CE//'5;58H',RHGT
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,14,IOSTAT=I) CE//'7;7H',RROLL
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,14,IOSTAT=I) CE//'8;7H',RPITC
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,11,IOSTAT=I) CE//'9;6H',RTHDG
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,11,IOSTAT=I) CE//'10;6H',RMHDG
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,11,IOSTAT=I) CE//'11;6H',RPAZI
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,15,IOSTAT=I) CE//'7;30H',RROLR
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,15,IOSTAT=I) CE//'8;30H',RPITR
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,15,IOSTAT=I) CE//'9;30H',RYAWR
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,14,IOSTAT=I) CE//'11;31H',RWA
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,16,IOSTAT=I) CE//'7;52H',RCNEXX
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,16,IOSTAT=I) CE//'8;52H',RCNEXY
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,16,IOSTAT=I) CE//'9;52H',RCNEXZ
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,15,IOSTAT=I) CE//'13;4H',RAX
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,15,IOSTAT=I) CE//'14;4H',RAY
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,15,IOSTAT=I) CE//'15;4H',RAZ
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,17,IOSTAT=I) CE//'13;27H',RAF
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,17,IOSTAT=I) CE//'14;27H',RAP
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,17,IOSTAT=I) CE//'15;27H',RAU
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,10,IOSTAT=I) CE//'13;56H'//CINUMDE(1:25)
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,18,IOSTAT=I) CE//'14;56H',JIUV
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,19,IOSTAT=I) CE//'17;28H',JIUZ,JIUO,JIUS
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,19,IOSTAT=I) CE//'18;28H',JNU141,JNU144,JNUSTS
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,10,IOSTAT=I) CE//'17;47H'//CIIUERR
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,10,IOSTAT=I) CE//'18;47H'//CINUERR
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,16,IOSTAT=I) CE//'20;10H',RTT
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,20,IOSTAT=I) CE//'20;29H',(IDIFFS(J),J=1,16)
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,10,IOSTAT=I) CE//'21;11H'//CDTIME
        CALL VT_OUT(CL(1:LAN(CL)))
        WRITE(CL,20,IOSTAT=I) CE//'21;29H',(IDIFFS(J),J=17,32)
        CALL VT_OUT(CL(1:LAN(CL)))
      END IF
      RETURN
10    FORMAT(A)
11    FORMAT(A,F7.2)
12    FORMAT(A,I5)
13    FORMAT(A,F6.4)
14    FORMAT(A,F6.2)
15    FORMAT(A,F7.3)
16    FORMAT(A,F9.6)
17    FORMAT(A,F5.2)
18    FORMAT(A,I1)
19    FORMAT(A,3Z6.4)
20    FORMAT(A,16I3)
21    FORMAT(A,I3,' ',I2.2,'.',I2.2,X,A)
22    FORMAT(A,F7.4)
23    FORMAT(I2.2,':',I2.2,':',I2.2)
      END
C*******************************************************************************
C
C ROUTINE          INU_DISP_ERRS_VT SUBROUTINE FORTVAX
C
C PURPOSE          Displays INU errors and status on VT320 terminals
C
C DESCRIPTION      Establishes a display of INU status words and updates it
C                  as new data are received.  If called with CMODE equal to
C                  BACKGROUND, clears the screen and puts up the legend.
C                  If called with CMODE equal to UPDATE, updates the information
C                  in the corresponding part of the screen.  If a bit is set 
C                  then the corresponding descriptor is displayed in inverse
C                  video.  Note that the logical unit must have been opened 
C                  with fortran carriage control.
C
C VERSION          1.00  13-07-93  W.D.N.JACKSON
C
C ARGUMENTS        JBUFF       I*2  IN   Block of raw data
C                  INULOC      I*4  IN   Starting point of INU data in block
C                  CMODE       C**  IN   'BACKGROUND' or 'UPDATE'
C
C CHANGES          1.01  09-09-93  W.D.N.JACKSON
C                  Now takes four arguments and displays DRS time
C
C*******************************************************************************
      SUBROUTINE INU_DISP_ERRS_VT(JBUFF,INULOC,CMODE)
      IMPLICIT NONE
      INTEGER*4 I,I1,I2,IHRS,IL,IMIN,INDX,INULOC,IOS,ISEC
      INTEGER*2 J,JLSTI011,JLSTI029,JLSTI141,JLSTI144,JLSTIIUS,JLSTONES,
     -    JLSTZERO
      INTEGER*4 M,K
      INTEGER*2 JDATA(32,163:169),JBUFF(2048)
      CHARACTER CMODE*(*),CDTIME*8
      CHARACTER CLINES(24)*79,CE*3,CZEROS*32,CONES*32,CN*4,CR*4,CROW*2

      DATA CLINES(1)(1:40) /'                              Honeywell '/
      DATA CLINES(2)(1:40) /'                                        '/
      DATA CLINES(3)(1:40) /'   I01-01             I01-29           I'/
      DATA CLINES(4)(1:40) /'                                        '/
      DATA CLINES(5)(1:40) /'1  Sensor/Ref Fail    Off              U'/
      DATA CLINES(6)(1:40) /'2  Any Nav Data fail  Standby          V'/
      DATA CLINES(7)(1:40) /'3  Degraded Nav       Air Align        W'/
      DATA CLINES(8)(1:40) /'4  Nav Data Unavlable Stored Hdg Align R'/
      DATA CLINES(9)(1:40) /'5  Dig Att Data Fail  GC Align         S'/
      DATA CLINES(10)(1:40)/'6  Degraded Nav Rdy   -                T'/
      DATA CLINES(11)(1:40)/'7  Attitude Rdy       -                H'/
      DATA CLINES(12)(1:40)/'8  Control Vect Ackn  -                I'/
      DATA CLINES(13)(1:40)/'9  Altitude Loop Bit  Navigate         N'/
      DATA CLINES(14)(1:40)/'10 INU Initiated BIT  Overfly          M'/
      DATA CLINES(15)(1:40)/'11 Mechanization Flag Auxiliary        S'/
      DATA CLINES(16)(1:40)/'12 In Manual Mag Var  Orient/Boresight -'/ 
      DATA CLINES(17)(1:40)/'13 Digital Select     Attitude         -'/
      DATA CLINES(18)(1:40)/'14 In Grid Mode       Test             -'/
      DATA CLINES(19)(1:40)/'15 CDU fail           -                -'/
      DATA CLINES(20)(1:40)/'16 -                  -                -'/
      DATA CLINES(21)(1:40)/'                                        '/
      DATA CLINES(22)(1:40)/'0''s  (0)                                '/
      DATA CLINES(23)(1:40)/'1''s  (0)                                '/
      DATA CLINES(24)(1:40)/'                                        '/

      DATA CLINES(1)(41:79) /'H-423 INU                              '/
      DATA CLINES(2)(41:79) /'                                       '/
      DATA CLINES(3)(41:79) /'14-01        I14-04         IIU        '/
      DATA CLINES(4)(41:79) /'                                       '/
      DATA CLINES(5)(41:79) /' Gyro fail   Gyro Elect  A1 Version 1  '/
      DATA CLINES(6)(41:79) /' Gyro fail   Acel Elect  A2 Version 2  '/
      DATA CLINES(7)(41:79) /' Gyro fail   A/D PAM Ass A3 Version 4  '/
      DATA CLINES(8)(41:79) /' Accel fail  Inert Proc  A4 -          '/
      DATA CLINES(9)(41:79) /' Accel fail  Nav Proc As A5 -          '/
      DATA CLINES(10)(41:79)/' Accel fail  1553B I/F   A6 -          '/
      DATA CLINES(11)(41:79)/'VPS/LVPS     Analog I/F1 A7 -          '/
      DATA CLINES(12)(41:79)/'P/MUX fail   Analog I/F2 A8 -          '/
      DATA CLINES(13)(41:79)/'P/Cal PROM   Low Vlt P/S A9 -          '/    
      DATA CLINES(14)(41:79)/'UX/Synchro   Trans/Rect A10 -          '/
      DATA CLINES(15)(41:79)/'ynchro/Disc  Sync Ref   A11 -          '/
      DATA CLINES(16)(41:79)/'             Gyro Drive A12 -          '/
      DATA CLINES(17)(41:79)/'             1553B MUX  A13 1553 err   '/
      DATA CLINES(18)(41:79)/'             Inert sens A14 No baro    '/
      DATA CLINES(19)(41:79)/'             Chassis    A15 No INU     '/
      DATA CLINES(20)(41:79)/'             -              No IIU     '/
      DATA CLINES(21)(41:79)/'                                       '/
      DATA CLINES(22)(41:79)/' (15)                                  '/
      DATA CLINES(23)(41:79)/' (15)             DRS time: hh:mm:ss   '/
      DATA CLINES(24)(41:79)/'                                       '/

      CE=CHAR(0)//CHAR(27)//'['
      CN=CHAR(27)//'[0m'               !Invoke normal
      CR=CHAR(27)//'[7m'               !Invoke reverse video
      IF(CMODE.EQ.'BACKGROUND') THEN
        CALL VT_OUT(CE//'2J'//CHAR(27)//'[1;1H')
        DO IL=1,22
          CALL VT_OUT(CHAR(0)//CLINES(IL)//CHAR(13)//CHAR(10))
        END DO
        CALL VT_OUT(CHAR(0)//CLINES(23)//CHAR(13))
        JLSTI011=0                     !Clear store of last values
        JLSTI029=0
        JLSTIIUS=0
        JLSTI141=0
        JLSTI144=0
        JLSTZERO='FFFF'X
        JLSTONES=0
      END IF
      IF(CMODE.EQ.'UPDATE') THEN
        I1=JZEXT(JBUFF(10).AND.'FFF'X)    !Get hours and mins
        I2=JZEXT(JBUFF(11).AND.'FFF'X)    !Get mins and secs
        IHRS=IBITS(I1,8,4)*10+IBITS(I1,4,4) !Compute hours
        IMIN=IBITS(I1,0,4)*10+IBITS(I2,8,4) !Compute minute
        ISEC=IBITS(I2,4,4)*10+IBITS(I2,0,4) !Compute seconds
        WRITE(CDTIME,23,IOSTAT=IOS) IHRS,IMIN,ISEC
        INDX=INULOC                    !Transfer INU data to local array
        DO I=163,169
          DO J=1,32
            JDATA(J,I)=JBUFF(INDX)
            INDX=INDX+1
          END DO
        END DO
        IF(BTEST(JDATA(1,164),14)) THEN
          JDATA(1,163)=0
          JDATA(29,163)=0
          JDATA(1,165)=0
          JDATA(1,166)=0
          JDATA(1,167)=0
          JDATA(1,168)='FFFF'X
        END IF
        IF(BTEST(JDATA(1,164),15)) JDATA(1,164)='8000'X
        DO I=0,15
          M=15-I
          K=I+5          
          WRITE(CROW,11,IOSTAT=J) K
          IF(BTEST(JLSTI011,M).AND..NOT.BTEST(JDATA(1,163),M)) THEN
            CALL VT_OUT(CE//CROW//';4H'//CN//CLINES(K)(4:21))
          ELSE IF(.NOT.BTEST(JLSTI011,M).AND.BTEST(JDATA(1,163),M)) THEN
            CALL VT_OUT(CE//CROW//';4H'//CR//CLINES(K)(4:21))
          END IF
          IF(BTEST(JLSTI029,M).AND..NOT.BTEST(JDATA(29,163),M)) THEN
            CALL VT_OUT(CE//CROW//';23H'//CN//CLINES(K)(23:38))
          ELSE IF(.NOT.BTEST(JLSTI029,M).AND.BTEST(JDATA(29,163),M))THEN
            CALL VT_OUT(CE//CROW//';23H'//CR//CLINES(K)(23:38))
          END IF
          IF(BTEST(JLSTIIUS,I).AND..NOT.BTEST(JDATA(1,164),I)) THEN
            CALL VT_OUT(CE//CROW//';69H'//CN//CLINES(K)(69:78))
          ELSE IF(.NOT.BTEST(JLSTIIUS,I).AND.BTEST(JDATA(1,164),I)) THEN
            CALL VT_OUT(CE//CROW//';69H'//CR//CLINES(K)(69:78))
          END IF
          IF(BTEST(JLSTI141,M).AND..NOT.BTEST(JDATA(1,165),M)) THEN
            CALL VT_OUT(CE//CROW//';40H'//CN//CLINES(K)(40:52))
          ELSE IF(.NOT.BTEST(JLSTI141,M).AND.BTEST(JDATA(1,165),M)) THEN
            CALL VT_OUT(CE//CROW//';40H'//CR//CLINES(K)(40:52))
          END IF
          IF(BTEST(JLSTI144,M).AND..NOT.BTEST(JDATA(1,166),M)) THEN
            CALL VT_OUT(CE//CROW//';54H'//CN//CLINES(K)(54:67))
          ELSE IF(.NOT.BTEST(JLSTI144,M).AND.BTEST(JDATA(1,166),M)) THEN
            CALL VT_OUT(CE//CROW//';54H'//CR//CLINES(K)(54:67))
          END IF
        END DO          
        CALL VT_OUT(CHAR(0)//CHAR(27)//'[0m')
        IF(JDATA(1,167).NE.JLSTZERO) THEN
          CZEROS=' 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0'
          DO I=1,16
            IF(BTEST(JDATA(1,167),I-1)) CZEROS(2*I:2*I)='1'
          END DO
          CALL VT_OUT(CE//'22;9H'//CZEROS)
        END IF
        IF(JDATA(1,168).NE.JLSTONES) THEN
          CONES= ' 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0'
          DO I=1,16
            IF(BTEST(JDATA(1,168),I-1)) CONES(2*I:2*I)='1'
          END DO
          CALL VT_OUT(CE//'23;9H'//CONES)
        END IF
        CALL VT_OUT(CE//'23;69H'//CDTIME)
        JLSTI011=JDATA(1,163)          !Save latest values
        JLSTI029=JDATA(29,163)
        JLSTIIUS=JDATA(1,164)
        JLSTI141=JDATA(1,165)
        JLSTI144=JDATA(1,166)
        JLSTZERO=JDATA(1,167)
        JLSTONES=JDATA(1,168)
      END IF
      RETURN
10    FORMAT(A)
11    FORMAT(I2.2)
23    FORMAT(I2.2,':',I2.2,':',I2.2)
      END
C*******************************************************************************
      SUBROUTINE INU_ISI_COMM(IM,CMODE,RLAT,RLNG,ISTAT)
C
C Handles requests by the H_DISPLAY program for INU messages via the INU Serial
C Interface (ISI), and for the passing of control messages to the INU from the 
C display program.  This subroutine carries out its function by communicating
C with the H_INU process via the INUDATA array of the HCOM global common area.
C It is the H_INU process which actually communicates with the ISI.
C
C IM specifies the message to be sent or solicited as follows:
C
C    1 - Request for message 01
C    6 - Request for message 06
C    7 - Request for message 07
C    9 - Request for message 09
C   10 - Request for message 10
C   11 - Request for message 11
C   12 - Request for message 12
C   13 - Request for message 13
C   14 - Request for message 14
C
C   20 - Request GC ALIGN mode
C   21 - Request NAVIGATE mode
C   22 - Request TEST mode
C
C   30 - Request clearing of INU Maintenance History Word (Misc Para 160)
C        (this is no longer implemented since Honeywell reluctant for us to
C        clear this word).
C
C   40 - Request Misc Para Read 
C   41 - Request H_INU to dump all INU data to H_INU.LOG
C
C Messages are returned in INUDATA(2:33).  Note that Miscellaneous Parameters
C can not be requested in this way by the H_DISPLAY program.  A selection of
C the 60 most useful Miscellaneous Parameters is always maintained in INUDATA
C by the H_INU process.
C
C If IM is 20 or 22, then the present latitude and longitude must also be
C specified.
C
C If IM is 40, then the required Miscellaneous Parameter number must also be
C specified, as the third argument of the subroutine and as a real*4 number.
C
C CMODE determines when the subroutine returns as follows:
C
C   'SEND'  - Takes the request and passes it to H_INU.  ISTAT returns as -1.
C   'CHECK' - Checks whether the latest send request has been met and the data 
C             is available for reading from INUDATA.  Returns with 1 if still 
C             waiting for the request to be satisfied, returns with 0 if the
C             data is now available in INUDATA, and with -1 if the H_INU
C             process was unable to process the request - either it was invalid
C             or the INU is not responding.
C
      IMPLICIT  NONE
      INTEGER*4 ISTAT,IM,IMM
      REAL*4    RLATLNG(2),RLAT,RLNG
      CHARACTER CMODE*(*)
      INCLUDE   'HCOM_DEF.FOR'
      EQUIVALENCE (INUDATA(3),RLATLNG)

      ISTAT=-1
      IF(CMODE.EQ.'SEND') THEN         !Note not checks if busy
        IF(IM.EQ.20.OR.IM.EQ.22) THEN
          RLATLNG(1)=RLAT
          RLATLNG(2)=RLNG
        ELSE IF(IM.EQ.40) THEN
          INUDATA(2)=NINT(RLAT)        !Misc Para Number
        END IF
        IMM=IM
        INUDATA(1)=IMM
      ELSE IF(CMODE.EQ.'CHECK') THEN
        IF(INUDATA(1).EQ.IMM) ISTAT=1  !H_INU not yet acknowledged request
        IF(INUDATA(1).EQ.-IMM) ISTAT=1 !H_INU waiting for INU response
        IF(INUDATA(1).EQ.-99) ISTAT=-1 !INU responded with an error
        IF(INUDATA(1).EQ.0) ISTAT=0    !H_INU successfully got message
      END IF
      RETURN
      END
C*******************************************************************************
      OPTIONS /EXTEND_SOURCE           !Some source lines are up to 132 chars
      SUBROUTINE INU_ISI_DISP_MESS_VT(IM,CMODE)
C
C Displays data from INU messages I01, I06, I07, I09, I10, I11, I12, I13, I14,
C and the Miscellaneous parameters.  IL is the LUN to which to send the data.
C IM is the message number, (99 if Misc Paras required).  CMODE determines how
C the screen is updated - 'BACKGROUND' just puts up the background information,
C 'DATA' just updates the data parts of the display, and 'FULL' writes out
C each line in full, suitable for sending to a file and printing.  If CMODE is
C specified as 'DATAZ' then the display is updated with values of zero.  The INU
C data itself is passed via the INUDATA array of the HCOM common block, and is
C is assumed to be valid and of the correct type during the time which this
C subroutine executes.  Note that all data is displayed in its basic form with
C no interpretation or expansion (except angles, which are converted from 
C pirads to degrees).  All the message decoding in this subroutine is based on
C the information in the SNU 84-1 Rev D INU specification.
C
      IMPLICIT  NONE
      INTEGER*4 ILMPC,IOSW,I,IM,IPPUE,IALT,IMDE,IPPUN,IANU,IMODE,ISFI,
     -    IAS,IMODE1,ISMC,IAT,IMODE2,ISTS,ICTL1,INWUE,ITA,ICTL3,INWUN,
     -    ITTN,IGMU,IOS,ITTS,IVEN,IXART,IXTLT,IXYPTU,IYART,IYTLT,IZART,
     -    IZPAU
      REAL*4    RCNEXZ,RMH,RSMC,RCNEYX,RMHN,RSMCS,RCNEYY,RNLAT,RTAS1,
     -    RCNEYZ,RNLNG,RTH,RCSNXX,RNX,RTT,RCSNXY,RNY,RVFC,RCSNXZ,RNZ,
     -    RVX,RCSNYX,RPA,RVY,RABU,RCSNYY,RPAAA,RVZ1,RAFC,RCSNYZ,RPATT,
     -    RWD,RAR,RDA,RPBCA,RWT,RARCT,RDGH,RPCF,RWV,RASFA,RDTN,RPDGS,
     -    RXCT,RASFU,RDTS,RPEE,RXGBU,RAW,REMH,RPEN,RXGR,RAWCT,REMV,RPGH,
     -    RXR,RAX,RETH,RPGS,RYAAA,RAY,RGCSE,RPI,RYAR,RAZ,RGSFU,RPIR,
     -    RYBCA,RBNW,RGXA,RPITT,RYCT,RBTS,RGXO,RPPLAT,RYGBU,RCCD,RGYA,
     -    RPPLNG,RYGR,RCGX,RGYO,RPTGT,RYR,RCGY,RGZA,RRAAA,RZCT,RCGZ,
     -    RGZO,RRBCA,RZGBU,RCMV,RHFC,RRO,RZGR,RCNEXX,RLONG,RROR,RZR,
     -    RCNEXY,RMGT,RROTT
      INTEGER*2 JDATA(32)
      INTEGER*4 LAN,INU_ISI_ISWAP
      CHARACTER CMODE*(*)
      CHARACTER CMISC(60)*6,CDATA(64)*1,CSUAN*10,CSUAP*10,CIL*80
      CHARACTER CAS(32)*4,CLMPC(8)*1,CMP*7,CL*2,CISMC(2)*4,CTEMP*6
      INCLUDE   'HCOM_DEF.FOR'
      EQUIVALENCE (INUDATA(2),JDATA(1)) !The INU message
      EQUIVALENCE (INUDATA(2),CDATA(1)) 
      EQUIVALENCE (INUDATA(34),CMISC(1)) !Miscellaneous parameters
      CHARACTER*79 CM01(22),CM06(22),CM07(21),CM09(22),CM10(21),CM11(20),
     -    CM12(5),CM13(20),CM14(18),CM99(22),CE*3
      DATA CISMC /'GCSM',' SCS'/
      DATA CAS /'Init','Att','A/H','8.0','7.2','6.4','5.6','4.8','4.0','3.2',
     -    '2.4','1.6','0.8','0.7','0.6','0.5','0.4','0.35','0.30','0.25','0.20',
     -    '0.15','0.10','0.05',8*' '/
      DATA CLMPC /'-','A','B','C','D','E','F',' '/

      DATA CM01 /
     -'                        Honeywell H-423 INU Message I01',
     -' ',
     -'I01-01 INU Mode Word',
     -'I01-02 Time Tag                                                 s',
     -'I01-03 Velocity X                                               fps',
     -'I01-05 Velocity Y                                               fps',
     -'I01-07 Velocity Z                                               fps',
     -'I01-09 Platform Azimuth                                         deg',
     -'I01-10 Roll/Pitch                                               deg',
     -'I01-12 Present True Heading                                     deg',
     -'I01-13 Present Magnetic Heading                                 deg',
     -'I01-14 X Acceleration                                           ft/s/s',
     -'I01-15 Y Acceleration                                           ft/s/s',
     -'I01-16 Z Acceleration                                           ft/s/s',
     -'I01-17 CNEXX',
     -'I01-19 CNEXY',
     -'I01-21 CNEXZ',
     -'I01-23 Inertial Altitude/Longitude                 ft           deg',
     -'I01-26 Great Circle Steering Error                              deg',
     -'I01-27 X-Axis/Y-Axis Residual Tilt                              arc sec',
     -'I01-29 Mode Word II',
     -'I01-30 Roll/Pitch/Yaw Rate                                      deg/s'/

      DATA CM06 /
     -'                        Honeywell H-423 INU Message I06',
     -' ',
     -'I06-01 INU Control Word 1',
     -'I06-02 Time Tag                                                      s',
     -'I06-03 Velocity X                                                    fps',
     -'I06-05 Velocity Y                                                    fps',
     -'I06-07 Velocity Z                                                    fps',
     -'I06-09 Platform Azimuth                                              deg',
     -'I06-10 Roll                                                          deg',
     -'I06-11 Pitch                                                         deg',
     -'I06-12 Present True Heading                                          deg',
     -'I06-13 Present Magnetic Heading                                      deg',
     -'I06-14 GC Steering Error/Computed Course Dev                         deg',
     -'I06-16 Time/Distance to Steerpoint                      s            nmi',
     -'I06-18 Rel Bearing to Steerpoint/nth Way/Markpnt                     deg',
     -'I06-20 Time/Distance to nth Waypoint/Markpoint          s            nmi',
     -'I06-22 INU Control Word 3',
     -'I06-23 nth Waypoint/Markpoint Lat/Long                                      deg',
     -'I06-27 Selected Mag Course to Steerpnt/Mag Course                    deg',
     -'I06-29 Magnetic Heading to nth Waypoint/Markpnt                      deg',
     -'I06-30 True Air Speed                                                kts',
     -'I06-31 Present Magnetic Ground Track/Drift Angle                     deg'/

      DATA CM07 /
     -'                        Honeywell H-423 INU Message I07',
     -' ',
     -'I07-01 nth Waypoint/Markpoint Spheroid/UTM Grid Zone/Area',
     -'I07-04 nth Waypoint/Markpoint UTM Easting/Northing                          m',
     -'I07-06 Present Position Latitude                                            deg',
     -'I07-08 Present Position Longitude                                           deg',
     -'I07-10 Present Position Spheroid/UTM Grid Zone/Area',
     -'I07-13 Present Position UTM Easting/Northing                                m',
     -'I07-15 Entered True/Magnetic Heading                                        deg',
     -'I07-17 Entered Magnetic Variation/Steering Mode                             deg',
     -'I07-18 Computed Magnetic Variation                                          deg',
     -'I07-19 Align Time and Status                                   s       ',
     -'I07-20 Wind Direction                                                       deg',
     -'I07-21 Last Mark Point Code/Wind Velocity                                   kts',
     -'I07-22 Present Ground Speed                                                 kts',
     -'I07-23 Present True Ground Track                                            deg',
     -'I07-24 Predicted Ground Speed                                               kts',
     -'I07-25 Present Convergence Factor in Use (Grid Mode)',
     -'I07-26 Present/Desired Grid Heading                                         deg',
     -'I07-28 Position Error North/East                                            nmi',
     -'I07-30 INU Miscellaneous Data'/

      DATA CM09 /
     -'                        Honeywell H-423 INU Message I09',
     -' ',
     -'I09-01 INU Mode Word',
     -'I09-02 Time Tag                                          s',
     -'I09-03 X Velocity                                        ft/s',
     -'I09-05 Y Velocity                                        ft/s',
     -'I09-07 Z Velocity                                        ft/s',
     -'I09-09 Platform Azimuth                                  deg',
     -'I09-10 Roll                                              deg',
     -'I09-11 Pitch                                             deg',
     -'I09-12 Roll Rate (p)                                     deg/s',
     -'I09-13 Pitch Rate (q)                                    deg/s',
     -'I09-14 Yaw Rate (r)                                      deg/s',
     -'I09-15 Longitudinal Acceleration (nx)                    ft/s/s',
     -'I09-16 Lateral Acceleration (ny)                         ft/s/s',
     -'I09-17 Normal Acceleration (nz)                          ft/s/s',
     -'I09-18 Platform Azimuth Time Tag                         s',
     -'I09-19 Roll Time Tag                                     s',
     -'I09-20 Pitch Time Tag                                    s',
     -'I09-21 Roll Axis Angular Acceleration                    deg/s/s',
     -'I09-22 Pitch Axis Angular Acceleration                   deg/s/s',
     -'I09-23 Yaw Axis Angular Acceleration                     deg/s/s'/

      DATA CM10 /
     -'                        Honeywell H-423 INU Message I10',
     -' ',
     -'I10-01 Time Tag                                       s',
     -'I10-02 X Axis Residual Tilt                           arc sec',
     -'I10-03 Y Axis Residual Tilt                           arc sec',
     -'I10-04 Z Axis Residual Tilt                           arc sec',
     -'I10-05 CNExx',
     -'I10-07 CNExy',
     -'I10-09 CNExz',
     -'I10-11 CNEyx',
     -'I10-13 CNEyy',
     -'I10-15 CNEyz',
     -'I10-17 CSNxx',
     -'I10-18 CSNxz',
     -'I10-19 CSNxz',
     -'I10-20 CSNyx',
     -'I10-21 CSNyy',
     -'I10-22 CSNyz',
     -'I10-23 Altitude Feedback Constant                     /s',
     -'I10-25 Velocity Feedback Constant                     /s/s',
     -'I10-27 Acceleration Feedback Constant                 /s/s/s'/

      DATA CM11 /
     -'                        Honeywell H-423 INU Message I11',
     -' ',
     -'I11-02 Accelerometer Bias Uncertainty                               mg',
     -'I11-03 (X,Y,Z) Accelerometer Scale Factor Uncertainty               %',
     -'I11-04 X Gyro Bias Uncertainty                                      deg/hr',
     -'I11-05 Y Gyro Bias Uncertainty                                      deg/hr',
     -'I11-06 Z Gyro Bias Uncertainty                                      deg/hr',
     -'I11-07 (X,Y,Z) Gyro Scale Factor Uncertainty                        %',
     -'I11-08 X Gyro Randomness                                            deg/hr',
     -'I11-09 X Gyro Correlation Time                                      min',
     -'I11-10 Y Gyro Randomness                                            deg/hr',
     -'I11-11 Y Gyro Correlation Time                                      min',
     -'I11-12 Z Gyro Randomness                                            deg/hr',
     -'I11-13 Z Gyro Correlation Time                                      min',
     -'I11-14 (X,Y,Z) Accelerometer Randomness                             mg',
     -'I11-15 (X,Y,Z) Accelerometer Randommess Correlation Time            min',
     -'I11-19 (X,Y,Z) Platform Tilt (Ground Align) Uncertainty             arc sec',
     -'I11-20 (X,Y,Z) Platform Azimuth (Ground Align) Uncertainty          arc sec',
     -'I11-21 (X,Y,Z) Accelerometer Nonorthogonality Uncertainty           arc sec',
     -'I11-24 (X,Y,Z) Gyro Misalignment Uncertainty                        arc sec'/

      DATA CM12 /
     -'                        Honeywell H-423 INU Message I12',
     -' ',
     -'I12-02 Accelerometer Scale Factor Asymmetry             ppm',
     -'I12-14 Accelerometer Warmup                             mg',
     -'I12-15 Accelerometer Warmup Correlation Time            min'/

      DATA CM13 /
     -'                        Honeywell H-423 INU Message I13',
     -' ',
     -'I13-07 Position of Specific Force Origin, Xr           inches',
     -'I13-08 Position of Specific Force Origin, Yr           inches',
     -'I13-09 Position of Specific Force Origin, Zr           inches',
     -'I13-10 Gyro Orientation about Zb, Gamma Z0             deg',
     -'I13-11 Gyro Orientation about Yb, Gamma Y0             deg',
     -'I13-12 Gyro Orientation about Xb, Gamma X0             deg',
     -'I13-13 Accl Orientation about Zb, Gamma Za             deg',
     -'I13-14 Accl Orientation about Yb, Gamma Ya             deg',
     -'I13-15 Accl Orientation about Xb, Gamma Xa             deg',
     -'I13-16 Terminal Address',
     -'I13-17 Orient Status Word',
     -'I13-18 Roll Boresight Correction Angle                 deg',
     -'I13-19 Pitch Boresight Correction Angle                deg',
     -'I13-20 Yaw Boresight Correction Angle                  deg',
     -'I13-21 Position of INU CG Along Xr                     inches',
     -'I13-22 Position of INU CG Along Yr                     inches',
     -'I13-23 Position of INU CG Along Zr                     inches',
     -'I13-24 Weight of INU                                   lbs'/

      DATA CM14 /
     -'                        Honeywell H-423 INU Message I14',
     -' ',
     -'I14-01 INU Status',
     -'I14-02 INU Vendor',
     -'I14-03 Mode Word',
     -'I14-04 SRU Failure Indicator',
     -' ',
     -'I14-11 Current LBIT Word  1             I14-22 History LBIT Word  1',
     -'I14-12 Current LBIT Word  2             I14-23 History LBIT Word  2',
     -'I14-13 Current LBIT Word  3             I14-24 History LBIT Word  3',
     -'I14-14 Current LBIT Word  4             I14-25 History LBIT Word  4',
     -'I14-15 Current LBIT Word  5             I14-26 History LBIT Word  5',
     -'I14-16 Current LBIT Word  6             I14-27 History LBIT Word  6',
     -'I14-17 Current LBIT Word  7             I14-28 History LBIT Word  7',
     -'I14-18 Current LBIT Word  8             I14-29 History LBIT Word  8',
     -'I14-19 Current LBIT Word  9             I14-30 History LBIT Word  9',
     -'I14-20 Current LBIT Word 10             I14-31 History LBIT Word 10',
     -'I14-21 Current LBIT Word 11             I14-32 History LBIT Word 11'/

      DATA CM99 /
     -'                     Honeywell H-423 INU Misc Parameters',
     -' ',
     -'000 Manuf ID  cccccc       020 Cumu CEP  cccccc nmi/h 060 Term ID   cccccc',
     -'001 BITE Hist cccccc       021 Last CEP  cccccc nmi/h 061 Baroi alt snnnnn ft',  
     -'002 BITE Stat cccccc       022 RER blat   snn nn.n d  062 Aligns    cccccc',  
     -'003 Init lati  snn nn.n d  023 RER blng  snnn nn.n d  063 Align Tim cccccc min', 
     -'004 Init long snnn nn.n d  024 RER ilat   snn nn.n d  064 Navs      cccccc', 
     -'005 Pres lati  snn nn.n d  025 RER ilng  snnn nn.n d  065 Nav Tim   cccccc min', 
     -'006 Pres long snnn nn.n d  026 RER GC1  ccc ccc nmi/h', 
     -'007 INU ID    cccccc       027 RER GC2  ccc ccc nmi/h 106 Insrt M/H snnn nn.n d',
     -'008 MUX/Time  nnnn.n min   028 RER GC3  ccc ccc nmi/h 111 Wnd Angle   snnn.nn d',
     -'009 DP N U1   snnn.n nm    029 RER GC4  ccc ccc nmi/h 112 Press Alt snnnnn.n ft',
     -'010 DP E U1   snnn.n nm    030 RER GC5  ccc ccc nmi/h 113 Real Time cccccc min',
     -'011 NTim U1   nnnn.n min   031 RER GC6  ccc ccc nmi/h 114 Pst Algns cccccc',
     -'012 DP N U2   snnn.n nm    032 RER GC7  ccc ccc nmi/h 116 Pst Navs  cccccc',
     -'013 DP E U2   snnn.n nm    033 RER GC8  ccc ccc nmi/h 118 Prv Algnt cccccc min',
     -'014 NTim U2   nnnn.n min   034 INU cycls cccccc       128 Strd HdgF cccccc',
     -'015 DP N U3   snnn.n nm    035 Last RER  cccccc       132 Malf Seve cccccc',
     -'016 DP E U3   snnn.n nm    036 Orient st cccccc       133 Batt Stat cccccc',
     -'017 NTim U3   nnnn.n min   037 Roll bore snn nn nn d  161 Auto-Cal  cccccc',
     -'018 Manl TAS  nnnn.n kts   038 Pitc bore snn nn nn d  177 Serial No cccccc',
     -'019 Miss RER ccc ccc nmi/h 039 Yaw bore  snn nn nn d  178 Config No cccccc'/

      CE=CHAR(0)//CHAR(27)//'['
      IF(CMODE.EQ.'BACKGROUND') THEN
        IF(IM.EQ.1) THEN
          CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H'//
     -        CM01(1)(1:LAN(CM01(1)))//CHAR(13))
          DO I=2,22
            CALL VT_OUT(' '//CM01(I)(1:LAN(CM01(I))))
          END DO
        ELSE IF(IM.EQ.6) THEN
          CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H'//
     -        CM06(1)(1:LAN(CM06(1)))//CHAR(13))
          DO I=2,22
            CALL VT_OUT(' '//CM06(I)(1:LAN(CM06(I))))
          END DO
        ELSE IF(IM.EQ.7) THEN
          CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H'//
     -        CM07(1)(1:LAN(CM07(1)))//CHAR(13))
          DO I=2,21
            CALL VT_OUT(' '//CM07(I)(1:LAN(CM07(I))))
          END DO
        ELSE IF(IM.EQ.9) THEN
          CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H'//
     -        CM09(1)(1:LAN(CM09(1)))//CHAR(13))
          DO I=2,22
            CALL VT_OUT(' '//CM09(I)(1:LAN(CM09(I))))
          END DO
        ELSE IF(IM.EQ.10) THEN
          CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H'//
     -        CM10(1)(1:LAN(CM10(1)))//CHAR(13))
          DO I=2,21
            CALL VT_OUT(' '//CM10(I)(1:LAN(CM10(I))))
          END DO
        ELSE IF(IM.EQ.11) THEN
          CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H'//
     -        CM11(1)(1:LAN(CM11(1)))//CHAR(13))
          DO I=2,20
            CALL VT_OUT(' '//CM11(I)(1:LAN(CM11(I))))
          END DO
        ELSE IF(IM.EQ.12) THEN
          CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H'//
     -        CM12(1)(1:LAN(CM12(1)))//CHAR(13))
          DO I=2,5
            CALL VT_OUT(' '//CM12(I)(1:LAN(CM12(I))))
          END DO
        ELSE IF(IM.EQ.13) THEN
          CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H'//
     -        CM13(1)(1:LAN(CM13(1)))//CHAR(13))
          DO I=2,20
            CALL VT_OUT(' '//CM13(I)(1:LAN(CM13(I))))
          END DO
        ELSE IF(IM.EQ.14) THEN
          CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H'//
     -        CM14(1)(1:LAN(CM14(1)))//CHAR(13))
          DO I=2,18
            CALL VT_OUT(' '//CM14(I)(1:LAN(CM14(I))))
          END DO
        ELSE IF(IM.EQ.99) THEN
          CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H'//
     -        CM99(1)(1:LAN(CM99(1)))//CHAR(13))
          DO I=2,22
            CALL VT_OUT(' '//CM99(I)(1:LAN(CM99(I))))
          END DO
        END IF
      ELSE IF(CMODE.EQ.'FULL'.OR.CMODE(1:4).EQ.'DATA') THEN
        IF(CMODE.EQ.'DATAZ') THEN
          DO I=1,32
            JDATA(I)=0
          END DO
        END IF
        IF(IM.EQ.1) THEN               !Decode message I01
          IMODE=JZEXT(JDATA(1))
          RTT=FLOAT(JZEXT(JDATA(2)))*64E-6
          RVX=FLOAT(INU_ISI_ISWAP(JDATA(3)))*3.81470E-6
          RVY=FLOAT(INU_ISI_ISWAP(JDATA(5)))*3.81470E-6
          RVZ1=FLOAT(INU_ISI_ISWAP(JDATA(7)))*3.81470E-6
          RPA=FLOAT(JDATA(9))*3.05176E-5*180.
          RRO=FLOAT(JDATA(10))*3.05176E-5*180.
          RPI=FLOAT(JDATA(11))*3.05176E-5*180.
          RTH=FLOAT(JDATA(12))*3.05176E-5*180.
          RMH=FLOAT(JDATA(13))*3.05176E-5*180.
          RAX=FLOAT(JDATA(14))*0.03125
          RAY=FLOAT(JDATA(15))*0.03125
          RAZ=FLOAT(JDATA(16))*0.03125
          RCNEXX=FLOAT(INU_ISI_ISWAP(JDATA(17)))*9.31323E-10
          RCNEXY=FLOAT(INU_ISI_ISWAP(JDATA(19)))*9.31323E-10
          RCNEXZ=FLOAT(INU_ISI_ISWAP(JDATA(21)))*9.31323E-10
          RLONG=FLOAT(INU_ISI_ISWAP(JDATA(23)))*4.65661E-10*180.
          IALT=NINT(FLOAT(JDATA(25))*4.)
          RGCSE=FLOAT(JDATA(26))*3.05175E-5*180.
          IXTLT=JDATA(27)
          IYTLT=JDATA(28)
          IMODE2=JZEXT(JDATA(29))
          RROR=FLOAT(JDATA(30))*1.22070E-4*180.
          RPIR=FLOAT(JDATA(31))*1.22070E-4*180.
          RYAR=FLOAT(JDATA(32))*1.22070E-4*180.
          IF(CMODE(1:4).EQ.'DATA') THEN
            WRITE(CIL,11,IOSTAT=IOS) CE//'3;49H',IMODE
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,12,IOSTAT=IOS) CE//'4;49H',RTT
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,13,IOSTAT=IOS) CE//'5;45H',RVX
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,13,IOSTAT=IOS) CE//'6;45H',RVY
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,13,IOSTAT=IOS) CE//'7;45H',RVZ1
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,14,IOSTAT=IOS) CE//'8;46H',RPA
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,15,IOSTAT=IOS) CE//'9;36H',RRO,RPI
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,14,IOSTAT=IOS) CE//'10;46H',RTH
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,14,IOSTAT=IOS) CE//'11;46H',RMH
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,16,IOSTAT=IOS) CE//'12;45H',RAX
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,16,IOSTAT=IOS) CE//'13;45H',RAY
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,16,IOSTAT=IOS) CE//'14;45H',RAZ
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,17,IOSTAT=IOS) CE//'15;48H',RCNEXX
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,17,IOSTAT=IOS) CE//'16;48H',RCNEXY
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,17,IOSTAT=IOS) CE//'17;48H',RCNEXZ
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,45,IOSTAT=IOS) CE//'18;36H',IALT,RLONG
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,14,IOSTAT=IOS) CE//'19;46H',RGCSE
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,19,IOSTAT=IOS) CE//'20;38H',IXTLT,IYTLT
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,11,IOSTAT=IOS) CE//'21;49H',IMODE2
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,20,IOSTAT=IOS) CE//'22;36H',RROR,RPIR,RYAR
            CALL VT_OUT(CIL(1:LAN(CIL)))
          ELSE IF(CMODE.EQ.'FULL') THEN
            WRITE(CM01(3)(48:52),11,IOSTAT=IOS) ' ',IMODE
            WRITE(CM01(4)(48:56),12,IOSTAT=IOS) ' ',RTT
            WRITE(CM01(5)(44:56),13,IOSTAT=IOS) ' ',RVX
            WRITE(CM01(6)(44:56),13,IOSTAT=IOS) ' ',RVY
            WRITE(CM01(7)(44:56),13,IOSTAT=IOS) ' ',RVZ1
            WRITE(CM01(8)(45:53),14,IOSTAT=IOS) ' ',RPA
            WRITE(CM01(9)(35:53),15,IOSTAT=IOS) ' ',RRO,RPI
            WRITE(CM01(10)(45:53),14,IOSTAT=IOS) ' ',RTH
            WRITE(CM01(11)(45:53),14,IOSTAT=IOS) ' ',RMH
            WRITE(CM01(12)(44:55),16,IOSTAT=IOS) ' ',RAX
            WRITE(CM01(13)(44:55),16,IOSTAT=IOS) ' ',RAY
            WRITE(CM01(14)(44:55),16,IOSTAT=IOS) ' ',RAZ
            WRITE(CM01(15)(47:60),17,IOSTAT=IOS) ' ',RCNEXX
            WRITE(CM01(16)(47:60),17,IOSTAT=IOS) ' ',RCNEXY
            WRITE(CM01(17)(47:60),17,IOSTAT=IOS) ' ',RCNEXZ
            WRITE(CM01(18)(35:57),45,IOSTAT=IOS) ' ',IALT,RLONG
            WRITE(CM01(19)(45:53),14,IOSTAT=IOS) ' ',RGCSE
            WRITE(CM01(20)(37:53),19,IOSTAT=IOS) ' ',IXTLT,IYTLT
            WRITE(CM01(21)(48:52),11,IOSTAT=IOS) ' ',IMODE2
            WRITE(CM01(22)(35:63),20,IOSTAT=IOS) ' ',RROR,RPIR,RYAR
            DO I=1,22
              CALL VT_OUT(' '//CM01(I)(1:LAN(CM01(I))))
            END DO
          END IF
        ELSE IF(IM.EQ.6) THEN          !Decode message I06
          ICTL1=JZEXT(JDATA(1))
          RTT=FLOAT(JZEXT(JDATA(2)))*64E-6
          RVX=FLOAT(INU_ISI_ISWAP(JDATA(3)))*3.81470E-6
          RVY=FLOAT(INU_ISI_ISWAP(JDATA(5)))*3.81470E-6
          RVZ1=FLOAT(INU_ISI_ISWAP(JDATA(7)))*3.81470E-6
          RPA=FLOAT(JDATA(9))*3.05176E-5*180.
          RRO=FLOAT(JDATA(10))*3.05176E-5*180.
          RPI=FLOAT(JDATA(11))*3.05176E-5*180.
          RTH=FLOAT(JDATA(12))*3.05176E-5*180.
          RMH=FLOAT(JDATA(13))*3.05176E-5*180.
          RGCSE=FLOAT(JDATA(14))*3.05176E-5*180.
          RCCD=FLOAT(JDATA(15))*3.05176E-5*180.
          ITTS=JZEXT(JDATA(16))
          RDTS=FLOAT(JZEXT(JDATA(17)))*0.1
          RBTS=FLOAT(JDATA(18))*3.05176E-5*180.
          RBNW=FLOAT(JDATA(19))*3.05176E-5*180.
          ITTN=JZEXT(JDATA(20))
          RDTN=FLOAT(JZEXT(JDATA(21)))*0.1
          ICTL3=JZEXT(JDATA(22))
          RNLAT=FLOAT(INU_ISI_ISWAP(JDATA(23)))*4.65661E-10*180.
          RNLNG=FLOAT(INU_ISI_ISWAP(JDATA(25)))*4.65661E-10*180.
          RSMCS=FLOAT(JDATA(27))*3.05176E-5*180.
          RSMC=FLOAT(JDATA(28))*3.05176E-5*180.
          RMHN=FLOAT(JDATA(29))*3.05176E-5*180.
          RTAS1=FLOAT(JDATA(30))*0.125
          RMGT=FLOAT(JDATA(31))*3.05176E-5*180.
          RDA=FLOAT(JDATA(32))*3.05176E-5*180.
          IF(CMODE(1:4).EQ.'DATA') THEN
            WRITE(CIL,11,IOSTAT=IOS) CE//'3;54H',ICTL1
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,12,IOSTAT=IOS) CE//'4;54H',RTT
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,13,IOSTAT=IOS) CE//'5;50H',RVX
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,13,IOSTAT=IOS) CE//'6;50H',RVY
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,13,IOSTAT=IOS) CE//'7;50H',RVZ1
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,14,IOSTAT=IOS) CE//'8;51H',RPA
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,14,IOSTAT=IOS) CE//'9;51H',RRO
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,14,IOSTAT=IOS) CE//'10;51H',RPI
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,14,IOSTAT=IOS) CE//'11;51H',RTH
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,14,IOSTAT=IOS) CE//'12;51H',RMH
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,15,IOSTAT=IOS) CE//'13;51H',RGCSE,RCCD
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,21,IOSTAT=IOS) CE//'14;50H',ITTS,RDTS
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,15,IOSTAT=IOS) CE//'15;51H',RBTS,RBNW
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,21,IOSTAT=IOS) CE//'16;50H',ITTN,RDTN
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,11,IOSTAT=IOS) CE//'17;54H',ICTL3
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,22,IOSTAT=IOS) CE//'18;50H',RNLAT,RNLNG
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,15,IOSTAT=IOS) CE//'19;51H',RSMCS,RSMC
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,14,IOSTAT=IOS) CE//'20;51H',RMHN
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,23,IOSTAT=IOS) CE//'21;50H',RTAS1
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,15,IOSTAT=IOS) CE//'22;51H',RMGT,RDA
            CALL VT_OUT(CIL(1:LAN(CIL)))
          ELSE IF(CMODE.EQ.'FULL') THEN
            WRITE(CM06(3)(53:57),11,IOSTAT=IOS) ' ',ICTL1
            WRITE(CM06(4)(53:61),12,IOSTAT=IOS) ' ',RTT
            WRITE(CM06(5)(49:61),13,IOSTAT=IOS) ' ',RVX
            WRITE(CM06(6)(49:61),13,IOSTAT=IOS) ' ',RVY
            WRITE(CM06(7)(49:61),13,IOSTAT=IOS) ' ',RVZ1
            WRITE(CM06(8)(50:58),14,IOSTAT=IOS) ' ',RPA
            WRITE(CM06(9)(50:58),14,IOSTAT=IOS) ' ',RRO
            WRITE(CM06(10)(50:58),14,IOSTAT=IOS) ' ',RPI
            WRITE(CM06(11)(50:58),14,IOSTAT=IOS) ' ',RTH
            WRITE(CM06(12)(50:58),14,IOSTAT=IOS) ' ',RMH
            WRITE(CM06(13)(50:68),15,IOSTAT=IOS) ' ',RGCSE,RCCD
            WRITE(CM06(14)(49:68),21,IOSTAT=IOS) ' ',ITTS,RDTS
            WRITE(CM06(15)(50:68),15,IOSTAT=IOS) ' ',RBTS,RBNW
            WRITE(CM06(16)(49:68),21,IOSTAT=IOS) ' ',ITTN,RDTN
            WRITE(CM06(17)(53:57),11,IOSTAT=IOS) ' ',ICTL3
            WRITE(CM06(18)(49:75),22,IOSTAT=IOS) ' ',RNLAT,RNLNG
            WRITE(CM06(19)(50:68),15,IOSTAT=IOS) ' ',RSMCS,RSMC
            WRITE(CM06(20)(50:58),14,IOSTAT=IOS) ' ',RMHN
            WRITE(CM06(21)(49:58),23,IOSTAT=IOS) ' ',RTAS1
            WRITE(CM06(22)(50:68),15,IOSTAT=IOS) ' ',RMGT,RDA
            DO I=1,22
              CALL VT_OUT(' '//CM06(I)(1:LAN(CM06(I))))
            END DO
          END IF
        ELSE IF(IM.EQ.7) THEN          !Decode message I07
          IF(JDATA(1).EQ.0) JDATA(1)='2020'X
          IF(JDATA(2).EQ.0) JDATA(2)='2020'X
          IF(JDATA(3).EQ.0) JDATA(3)='2020'X
          CSUAN=CDATA(2)//'  '//CDATA(1)//CDATA(4)//CDATA(3)//'  '//CDATA(6)//
     -        CDATA(5)
          INWUE=JZEXT(JDATA(4))*2
          INWUN=JZEXT(JDATA(5))*2
          RPPLAT=FLOAT(INU_ISI_ISWAP(JDATA(6)))*4.65661E-10*180.
          RPPLNG=FLOAT(INU_ISI_ISWAP(JDATA(8)))*4.65661E-10*180.
          IF(JDATA(10).EQ.0) JDATA(10)='2020'X
          IF(JDATA(11).EQ.0) JDATA(11)='2020'X
          IF(JDATA(12).EQ.0) JDATA(12)='2020'X
          CSUAP=CDATA(20)//'  '//CDATA(19)//CDATA(22)//CDATA(21)//'  '//
     -        CDATA(24)//CDATA(23)
          IPPUE=JZEXT(JDATA(13))*2
          IPPUN=JZEXT(JDATA(14))*2
          RETH=FLOAT(JDATA(15))*3.05176E-5*180.
          REMH=FLOAT(JDATA(16))*3.05176E-5*180.
          ISMC=(JDATA(17).AND.1)+1
          REMV=FLOAT(JDATA(17)/2)*6.10352E-5*180.
          RCMV=FLOAT(JDATA(18))*3.05176E-5*180.
          IAT=(JDATA(19).AND.'3FF'X)*6
          IAS=((JDATA(19).AND.'7FFF'X)/1024)+1
          RWD=FLOAT(JDATA(20))*3.05176E-5*180.
          RWV=FLOAT(JZEXT(JDATA(21))/8)*0.0625
          ILMPC=(JDATA(21).AND.7)+1
          RPGS=FLOAT(JZEXT(JDATA(22)))*0.0625
          RPTGT=FLOAT(JDATA(23))*3.05175E-5*180.
          RPDGS=FLOAT(JZEXT(JDATA(24)))*0.0625
          RPCF=FLOAT(JZEXT(JDATA(25)))*3.05176E-5
          RPGH=FLOAT(JDATA(26))*3.05176E-5*180.
          RDGH=FLOAT(JDATA(27))*3.05176E-5*180.
          RPEN=FLOAT(JDATA(28))*0.01
          RPEE=FLOAT(JDATA(29))*0.01
          IF(JDATA(30).EQ.0) JDATA(30)='2020'X
          IF(JDATA(31).EQ.0) JDATA(30)='2020'X
          IF(JDATA(32).EQ.0) JDATA(30)='2020'X
          CMP=' '//CDATA(60)//CDATA(59)//CDATA(62)//CDATA(61)//CDATA(64)
     -        //CDATA(63)
          IF(ICHAR(CMP(2:2)).GT.127) THEN
            CMP(1:1)='-'
            CMP(2:2)=CHAR(ICHAR(CMP(2:2))-128)
          END IF
          DO I=1,7
            IF(ICHAR(CMP(I:I)).EQ.0) CMP(I:I)=' '
          END DO
          IF(CMODE(1:4).EQ.'DATA') THEN
            WRITE(CIL,24,IOSTAT=IOS) CE//'3;59H'//CSUAN
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,25,IOSTAT=IOS) CE//'4;58H',INWUE,INWUN
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,26,IOSTAT=IOS) CE//'5;59H',RPPLAT
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,26,IOSTAT=IOS) CE//'6;59H',RPPLNG
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,24,IOSTAT=IOS) CE//'7;59H'//CSUAP
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,25,IOSTAT=IOS) CE//'8;58H',IPPUE,IPPUN
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,43,IOSTAT=IOS) CE//'9;59H',RETH,REMH
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,14,IOSTAT=IOS) CE//'10;53H'//CISMC(ISMC)//'  ',REMV
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,14,IOSTAT=IOS) CE//'11;59H',RCMV
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,27,IOSTAT=IOS) CE//'12;59H',IAT,CAS(IAS)
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,14,IOSTAT=IOS) CE//'13;59H',RWD
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,28,IOSTAT=IOS) CE//'14;56H'//CLMPC(ILMPC)//'   ',RWV
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,28,IOSTAT=IOS) CE//'15;60H',RPGS
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,14,IOSTAT=IOS) CE//'16;59H',RPTGT
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,28,IOSTAT=IOS) CE//'17;60H',RPDGS
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,29,IOSTAT=IOS) CE//'18;62H',RPCF
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,43,IOSTAT=IOS) CE//'19;59H',RPGH,RDGH
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,30,IOSTAT=IOS) CE//'20;59H',RPEN,RPEE
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,24,IOSTAT=IOS) CE//'21;59H'//CMP
            CALL VT_OUT(CIL(1:LAN(CIL)))
          ELSE IF(CMODE.EQ.'FULL') THEN
            WRITE(CM07(3)(58:68),24,IOSTAT=IOS) ' '//CSUAN
            WRITE(CM07(4)(57:75),25,IOSTAT=IOS) ' ',INWUE,INWUN
            WRITE(CM07(5)(58:70),26,IOSTAT=IOS) ' ',RPPLAT
            WRITE(CM07(6)(58:70),26,IOSTAT=IOS) ' ',RPPLNG
            WRITE(CM07(7)(58:68),24,IOSTAT=IOS) ' '//CSUAP
            WRITE(CM07(8)(57:75),25,IOSTAT=IOS) ' ',IPPUE,IPPUN
            WRITE(CM07(9)(58:75),43,IOSTAT=IOS) ' ',RETH,REMH
            WRITE(CM07(10)(52:66),14,IOSTAT=IOS) ' '//CISMC(ISMC)//'  ',REMV
            WRITE(CM07(11)(58:66),14,IOSTAT=IOS) ' ',RCMV
            WRITE(CM07(12)(58:71),27,IOSTAT=IOS) ' ',IAT,CAS(IAS)
            WRITE(CM07(13)(58:66),14,IOSTAT=IOS) ' ',RWD
            WRITE(CM07(14)(55:67),28,IOSTAT=IOS) ' '//CLMPC(ILMPC)//'   ',RWV
            WRITE(CM07(15)(59:67),28,IOSTAT=IOS) ' ',RPGS
            WRITE(CM07(16)(58:66),14,IOSTAT=IOS) ' ',RPTGT
            WRITE(CM07(17)(59:67),28,IOSTAT=IOS) ' ',RPDGS
            WRITE(CM07(18)(61:68),29,IOSTAT=IOS) ' ',RPCF
            WRITE(CM07(19)(58:75),43,IOSTAT=IOS) ' ',RPGH,RDGH
            WRITE(CM07(20)(58:74),30,IOSTAT=IOS) ' ',RPEN,RPEE
            WRITE(CM07(21)(58:65),24,IOSTAT=IOS) ' '//CMP
            DO I=1,21
              CALL VT_OUT(' '//CM07(I)(1:LAN(CM07(I))))
            END DO
          END IF
        ELSE IF(IM.EQ.9) THEN          !Decode message I09
          IMODE1=JZEXT(JDATA(1))
          RTT=FLOAT(JZEXT(JDATA(2)))*64E-6
          RVX=FLOAT(INU_ISI_ISWAP(JDATA(3)))*3.81470E-6
          RVY=FLOAT(INU_ISI_ISWAP(JDATA(5)))*3.81470E-6
          RVZ1=FLOAT(INU_ISI_ISWAP(JDATA(7)))*3.81470E-6
          RPA=FLOAT(JDATA(9))*3.05176E-5*180.
          RRO=FLOAT(JDATA(10))*3.05176E-5*180.
          RPI=FLOAT(JDATA(11))*3.05176E-5*180.
          RROR=FLOAT(JDATA(12))*1.22070E-4*180.
          RPIR=FLOAT(JDATA(13))*1.22070E-4*180.
          RYAR=FLOAT(JDATA(14))*1.22070E-4*180.
          RNX=FLOAT(JDATA(15))*0.03125
          RNY=FLOAT(JDATA(16))*0.03125
          RNZ=FLOAT(JDATA(17))*0.03125
          RPATT=FLOAT(JZEXT(JDATA(18)))*64E-6
          RROTT=FLOAT(JZEXT(JDATA(19)))*64E-6
          RPITT=FLOAT(JZEXT(JDATA(20)))*64E-6
          RRAAA=FLOAT(JDATA(21))*2.44141E-4*180.
          RPAAA=FLOAT(JDATA(22))*2.44141E-4*180.
          RYAAA=FLOAT(JDATA(23))*2.44141E-4*180.
          IF(CMODE(1:4).EQ.'DATA') THEN
            WRITE(CIL,11,IOSTAT=IOS) CE//'3;49H',IMODE1
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,12,IOSTAT=IOS) CE//'4;49H',RTT
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,13,IOSTAT=IOS) CE//'5;45H',RVX
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,13,IOSTAT=IOS) CE//'6;45H',RVY
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,13,IOSTAT=IOS) CE//'7;45H',RVZ1
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,31,IOSTAT=IOS) CE//'8;46H',RPA
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,31,IOSTAT=IOS) CE//'9;46H',RRO
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,31,IOSTAT=IOS) CE//'10;46H',RPI
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,31,IOSTAT=IOS) CE//'11;46H',RROR
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,31,IOSTAT=IOS) CE//'12;46H',RPIR
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,31,IOSTAT=IOS) CE//'13;46H',RYAR
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,32,IOSTAT=IOS) CE//'14;46H',RNX
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,32,IOSTAT=IOS) CE//'15;46H',RNY
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,32,IOSTAT=IOS) CE//'16;46H',RNZ
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,12,IOSTAT=IOS) CE//'17;49H',RPATT
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,12,IOSTAT=IOS) CE//'18;49H',RROTT
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,12,IOSTAT=IOS) CE//'19;49H',RPITT
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,31,IOSTAT=IOS) CE//'20;46H',RRAAA
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,31,IOSTAT=IOS) CE//'21;46H',RPAAA
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,31,IOSTAT=IOS) CE//'22;46H',RYAAA
            CALL VT_OUT(CIL(1:LAN(CIL)))
          ELSE IF(CMODE.EQ.'FULL') THEN
            WRITE(CM09(3)(48:52),11,IOSTAT=IOS) ' ',IMODE1
            WRITE(CM09(4)(48:56),12,IOSTAT=IOS) ' ',RTT
            WRITE(CM09(5)(44:56),13,IOSTAT=IOS) ' ',RVX
            WRITE(CM09(6)(44:56),13,IOSTAT=IOS) ' ',RVY
            WRITE(CM09(7)(44:56),13,IOSTAT=IOS) ' ',RVZ1
            WRITE(CM09(8)(45:54),31,IOSTAT=IOS) ' ',RPA
            WRITE(CM09(9)(45:54),31,IOSTAT=IOS) ' ',RRO
            WRITE(CM09(10)(45:54),31,IOSTAT=IOS) ' ',RPI
            WRITE(CM09(11)(45:54),31,IOSTAT=IOS) ' ',RROR
            WRITE(CM09(12)(45:54),31,IOSTAT=IOS) ' ',RPIR
            WRITE(CM09(13)(45:54),31,IOSTAT=IOS) ' ',RYAR
            WRITE(CM09(14)(45:55),32,IOSTAT=IOS) ' ',RNX
            WRITE(CM09(15)(45:55),32,IOSTAT=IOS) ' ',RNY
            WRITE(CM09(16)(45:55),32,IOSTAT=IOS) ' ',RNZ
            WRITE(CM09(17)(48:56),12,IOSTAT=IOS) ' ',RPATT
            WRITE(CM09(18)(48:56),12,IOSTAT=IOS) ' ',RROTT
            WRITE(CM09(19)(48:56),12,IOSTAT=IOS) ' ',RPITT
            WRITE(CM09(20)(45:54),31,IOSTAT=IOS) ' ',RRAAA
            WRITE(CM09(21)(45:54),31,IOSTAT=IOS) ' ',RPAAA
            WRITE(CM09(22)(45:54),31,IOSTAT=IOS) ' ',RYAAA
            DO I=1,22
              CALL VT_OUT(' '//CM09(I)(1:LAN(CM09(I))))
            END DO
          END IF
        ELSE IF(IM.EQ.10) THEN         !Decode message I10
          RTT=FLOAT(JZEXT(JDATA(1)))*64E-6
          IXART=JDATA(2)
          IYART=JDATA(3)
          IZART=JDATA(4)
          RCNEXX=FLOAT(INU_ISI_ISWAP(JDATA(5)))*9.31323E-10
          RCNEXY=FLOAT(INU_ISI_ISWAP(JDATA(7)))*9.31323E-10
          RCNEXZ=FLOAT(INU_ISI_ISWAP(JDATA(9)))*9.31323E-10
          RCNEYX=FLOAT(INU_ISI_ISWAP(JDATA(11)))*9.31323E-10
          RCNEYY=FLOAT(INU_ISI_ISWAP(JDATA(13)))*9.31323E-10
          RCNEYZ=FLOAT(INU_ISI_ISWAP(JDATA(15)))*9.31323E-10
          RCSNXX=FLOAT(JDATA(17))*6.10352E-5
          RCSNXY=FLOAT(JDATA(18))*6.10352E-5
          RCSNXZ=FLOAT(JDATA(19))*6.10352E-5
          RCSNYX=FLOAT(JDATA(20))*6.10352E-5
          RCSNYY=FLOAT(JDATA(21))*6.10352E-5
          RCSNYZ=FLOAT(JDATA(22))*6.10352E-5
          RHFC=FLOAT(INU_ISI_ISWAP(JDATA(23)))*2.79387E-9
          RVFC=FLOAT(INU_ISI_ISWAP(JDATA(25)))*2.79397E-9
          RAFC=FLOAT(INU_ISI_ISWAP(JDATA(27)))*9.31323E-10
          IF(CMODE(1:4).EQ.'DATA') THEN
            WRITE(CIL,12,IOSTAT=IOS) CE//'3;42H',RTT
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,33,IOSTAT=IOS) CE//'4;44H',IXART
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,33,IOSTAT=IOS) CE//'5;44H',IYART
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,33,IOSTAT=IOS) CE//'6;44H',IZART
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,17,IOSTAT=IOS) CE//'7;41H',RCNEXX
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,17,IOSTAT=IOS) CE//'8;41H',RCNEXY
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,17,IOSTAT=IOS) CE//'9;41H',RCNEXZ
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,17,IOSTAT=IOS) CE//'10;41H',RCNEYX
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,17,IOSTAT=IOS) CE//'11;41H',RCNEYY
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,17,IOSTAT=IOS) CE//'12;41H',RCNEYZ
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,34,IOSTAT=IOS) CE//'13;41H',RCSNXX
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,34,IOSTAT=IOS) CE//'14;41H',RCSNXY
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,34,IOSTAT=IOS) CE//'15;41H',RCSNXZ
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,34,IOSTAT=IOS) CE//'16;41H',RCSNYX
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,34,IOSTAT=IOS) CE//'17;41H',RCSNYY
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,34,IOSTAT=IOS) CE//'18;41H',RCSNYZ
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,17,IOSTAT=IOS) CE//'19;41H',RHFC
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,17,IOSTAT=IOS) CE//'20;41H',RVFC
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,17,IOSTAT=IOS) CE//'21;41H',RAFC
            CALL VT_OUT(CIL(1:LAN(CIL)))
          ELSE IF(CMODE.EQ.'FULL') THEN
            WRITE(CM10(3)(41:49),12,IOSTAT=IOS) ' ',RTT
            WRITE(CM10(4)(43:49),33,IOSTAT=IOS) ' ',IXART
            WRITE(CM10(5)(43:49),33,IOSTAT=IOS) ' ',IYART
            WRITE(CM10(6)(43:49),33,IOSTAT=IOS) ' ',IZART
            WRITE(CM10(7)(40:53),17,IOSTAT=IOS) ' ',RCNEXX
            WRITE(CM10(8)(40:53),17,IOSTAT=IOS) ' ',RCNEXY
            WRITE(CM10(9)(40:53),17,IOSTAT=IOS) ' ',RCNEXZ
            WRITE(CM10(10)(40:53),17,IOSTAT=IOS) ' ',RCNEYX
            WRITE(CM10(11)(40:53),17,IOSTAT=IOS) ' ',RCNEYY
            WRITE(CM10(12)(40:53),17,IOSTAT=IOS) ' ',RCNEYZ
            WRITE(CM10(13)(40:48),34,IOSTAT=IOS) ' ',RCSNXX
            WRITE(CM10(14)(40:48),34,IOSTAT=IOS) ' ',RCSNXY
            WRITE(CM10(15)(40:48),34,IOSTAT=IOS) ' ',RCSNXZ
            WRITE(CM10(16)(40:48),34,IOSTAT=IOS) ' ',RCSNYX
            WRITE(CM10(17)(40:48),34,IOSTAT=IOS) ' ',RCSNYY
            WRITE(CM10(18)(40:48),34,IOSTAT=IOS) ' ',RCSNYZ
            WRITE(CM10(19)(40:53),17,IOSTAT=IOS) ' ',RHFC
            WRITE(CM10(20)(40:53),17,IOSTAT=IOS) ' ',RVFC
            WRITE(CM10(21)(40:53),17,IOSTAT=IOS) ' ',RAFC
            DO I=1,21
              CALL VT_OUT(' '//CM10(I)(1:LAN(CM10(I))))
            END DO
          END IF
        ELSE IF(IM.EQ.11) THEN         !Decode message I11
          RABU=FLOAT(JZEXT(JDATA(2)))*0.0016
          RASFU=FLOAT(JZEXT(JDATA(3)))*1E-5
          RXGBU=FLOAT(JZEXT(JDATA(4)))*1E-5
          RYGBU=FLOAT(JZEXT(JDATA(5)))*1E-5
          RZGBU=FLOAT(JZEXT(JDATA(6)))*1E-5
          RGSFU=FLOAT(JZEXT(JDATA(7)))*1E-5
          RXGR=FLOAT(JZEXT(JDATA(8)))*1E-6
          RXCT=FLOAT(JZEXT(JDATA(9)))*0.01
          RYGR=FLOAT(JZEXT(JDATA(10)))*1E-6
          RYCT=FLOAT(JZEXT(JDATA(11)))*0.01
          RZGR=FLOAT(JZEXT(JDATA(12)))*1E-6
          RZCT=FLOAT(JZEXT(JDATA(13)))*0.01
          RAR=FLOAT(JZEXT(JDATA(14)))*1E-5
          RARCT=FLOAT(JZEXT(JDATA(15)))*0.01
          IXYPTU=JZEXT(JDATA(19))
          IZPAU=JZEXT(JDATA(20))
          IANU=JZEXT(JDATA(21))
          IGMU=JZEXT(JDATA(24))
          IF(CMODE(1:4).EQ.'DATA') THEN
            WRITE(CIL,35,IOSTAT=IOS) CE//'3;59H',RABU
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,29,IOSTAT=IOS) CE//'4;60H',RASFU
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,29,IOSTAT=IOS) CE//'5;60H',RXGBU
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,29,IOSTAT=IOS) CE//'6;60H',RYGBU
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,29,IOSTAT=IOS) CE//'7;60H',RZGBU
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,29,IOSTAT=IOS) CE//'8;60H',RGSFU
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,36,IOSTAT=IOS) CE//'9;60H',RXGR
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,37,IOSTAT=IOS) CE//'10;58H',RXCT
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,36,IOSTAT=IOS) CE//'11;60H',RYGR
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,37,IOSTAT=IOS) CE//'12;58H',RYCT
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,36,IOSTAT=IOS) CE//'13;60H',RZGR
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,37,IOSTAT=IOS) CE//'14;58H',RZCT
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,29,IOSTAT=IOS) CE//'15;60H',RAR
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,37,IOSTAT=IOS) CE//'16;58H',RARCT
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,38,IOSTAT=IOS) CE//'17;59H',IXYPTU
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,38,IOSTAT=IOS) CE//'18;59H',IZPAU
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,38,IOSTAT=IOS) CE//'19;59H',IANU
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,38,IOSTAT=IOS) CE//'20;59H',IGMU
            CALL VT_OUT(CIL(1:LAN(CIL)))
          ELSE IF(CMODE.EQ.'FULL') THEN
            WRITE(CM11(3)(58:65),35,IOSTAT=IOS) ' ',RABU
            WRITE(CM11(4)(59:66),29,IOSTAT=IOS) ' ',RASFU
            WRITE(CM11(5)(59:66),29,IOSTAT=IOS) ' ',RXGBU
            WRITE(CM11(6)(59:66),29,IOSTAT=IOS) ' ',RYGBU
            WRITE(CM11(7)(59:66),29,IOSTAT=IOS) ' ',RZGBU
            WRITE(CM11(8)(59:66),29,IOSTAT=IOS) ' ',RGSFU
            WRITE(CM11(9)(59:67),36,IOSTAT=IOS) ' ',RXGR
            WRITE(CM11(10)(57:63),37,IOSTAT=IOS) ' ',RXCT
            WRITE(CM11(11)(59:67),36,IOSTAT=IOS) ' ',RYGR
            WRITE(CM11(12)(57:63),37,IOSTAT=IOS) ' ',RYCT
            WRITE(CM11(13)(59:67),36,IOSTAT=IOS) ' ',RZGR
            WRITE(CM11(14)(57:63),37,IOSTAT=IOS) ' ',RZCT
            WRITE(CM11(15)(59:66),29,IOSTAT=IOS) ' ',RAR
            WRITE(CM11(16)(57:63),37,IOSTAT=IOS) ' ',RARCT
            WRITE(CM11(17)(58:63),38,IOSTAT=IOS) ' ',IXYPTU
            WRITE(CM11(18)(58:63),38,IOSTAT=IOS) ' ',IZPAU
            WRITE(CM11(19)(58:63),38,IOSTAT=IOS) ' ',IANU
            WRITE(CM11(20)(58:63),38,IOSTAT=IOS) ' ',IGMU
            DO I=1,20
              CALL VT_OUT(' '//CM11(I)(1:LAN(CM11(I))))
            END DO
          END IF
        ELSE IF(IM.EQ.12) THEN         !Decode message I12
          RASFA=FLOAT(JZEXT(JDATA(2)))*0.1
          RAW=FLOAT(JZEXT(JDATA(14)))*0.001
          RAWCT=FLOAT(JZEXT(JDATA(15)))*0.01
          IF(CMODE(1:4).EQ.'DATA') THEN
            WRITE(CIL,14,IOSTAT=IOS) CE//'3;48H',RASFA
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,40,IOSTAT=IOS) CE//'4;50H',RAW
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,44,IOSTAT=IOS) CE//'5;49H',RAWCT
            CALL VT_OUT(CIL(1:LAN(CIL)))
          ELSE IF(CMODE.EQ.'FULL') THEN
            WRITE(CM12(3)(47:55),14,IOSTAT=IOS) ' ',RASFA
            WRITE(CM12(4)(49:55),40,IOSTAT=IOS) ' ',RAW
            WRITE(CM12(5)(48:55),44,IOSTAT=IOS) ' ',RAWCT
            DO I=1,5
              CALL VT_OUT(' '//CM12(I)(1:LAN(CM12(I))))
            END DO
          END IF
        ELSE IF(IM.EQ.13) THEN         !Decode message I13
          RXR=FLOAT(JDATA(7))*0.001
          RYR=FLOAT(JDATA(8))*0.001
          RZR=FLOAT(JDATA(9))*0.001
          RGZO=FLOAT(JDATA(10))*3.05176E-5*180.
          RGYO=FLOAT(JDATA(11))*3.05176E-5*180.
          RGXO=FLOAT(JDATA(12))*3.05176E-5*180.
          RGZA=FLOAT(JDATA(13))*3.05176E-5*180.
          RGYA=FLOAT(JDATA(14))*3.05176E-5*180.
          RGXA=FLOAT(JDATA(15))*3.05176E-5*180.
          ITA=JZEXT(JDATA(16))
          IOSW=JZEXT(JDATA(17))
          RRBCA=FLOAT(JDATA(18))*1.52588E-5*180.
          RPBCA=FLOAT(JDATA(19))*1.52588E-5*180.
          RYBCA=FLOAT(JDATA(20))*1.52588E-5*180.
          RCGX=FLOAT(JDATA(21))*4.88281E-4
          RCGY=FLOAT(JDATA(22))*4.88281E-4
          RCGZ=FLOAT(JDATA(23))*4.88281E-4
          RWT=FLOAT(JZEXT(JDATA(24)))*9.15527E-4
          IF(CMODE(1:4).EQ.'DATA') THEN
            WRITE(CIL,14,IOSTAT=IOS) CE//'3;46H',RXR
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,14,IOSTAT=IOS) CE//'4;46H',RYR
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,14,IOSTAT=IOS) CE//'5;46H',RZR
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,14,IOSTAT=IOS) CE//'6;46H',RGZO
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,14,IOSTAT=IOS) CE//'7;46H',RGYO
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,14,IOSTAT=IOS) CE//'8;46H',RGXO
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,14,IOSTAT=IOS) CE//'9;46H',RGZA
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,14,IOSTAT=IOS) CE//'10;46H',RGYA
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,14,IOSTAT=IOS) CE//'11;46H',RGXA
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,11,IOSTAT=IOS) CE//'12;49H',ITA
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,11,IOSTAT=IOS) CE//'13;49H',IOSW
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,14,IOSTAT=IOS) CE//'14;46H',RRBCA
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,14,IOSTAT=IOS) CE//'15;46H',RPBCA
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,14,IOSTAT=IOS) CE//'16;46H',RYBCA
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,31,IOSTAT=IOS) CE//'17;46H',RCGX
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,31,IOSTAT=IOS) CE//'18;46H',RCGY
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,31,IOSTAT=IOS) CE//'19;46H',RCGZ
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,31,IOSTAT=IOS) CE//'20;46H',RWT
            CALL VT_OUT(CIL(1:LAN(CIL)))
          ELSE IF(CMODE.EQ.'FULL') THEN
            WRITE(CM13(3)(45:53),14,IOSTAT=IOS) ' ',RXR
            WRITE(CM13(4)(45:53),14,IOSTAT=IOS) ' ',RYR
            WRITE(CM13(5)(45:53),14,IOSTAT=IOS) ' ',RZR
            WRITE(CM13(6)(45:53),14,IOSTAT=IOS) ' ',RGZO
            WRITE(CM13(7)(45:53),14,IOSTAT=IOS) ' ',RGYO
            WRITE(CM13(8)(45:53),14,IOSTAT=IOS) ' ',RGXO
            WRITE(CM13(9)(45:53),14,IOSTAT=IOS) ' ',RGZA
            WRITE(CM13(10)(45:53),14,IOSTAT=IOS) ' ',RGYA
            WRITE(CM13(11)(45:53),14,IOSTAT=IOS) ' ',RGXA
            WRITE(CM13(12)(48:52),11,IOSTAT=IOS) ' ',ITA
            WRITE(CM13(13)(48:52),11,IOSTAT=IOS) ' ',IOSW
            WRITE(CM13(14)(45:53),14,IOSTAT=IOS) ' ',RRBCA
            WRITE(CM13(15)(45:53),14,IOSTAT=IOS) ' ',RPBCA
            WRITE(CM13(16)(45:53),14,IOSTAT=IOS) ' ',RYBCA
            WRITE(CM13(17)(45:54),31,IOSTAT=IOS) ' ',RCGX
            WRITE(CM13(18)(45:54),31,IOSTAT=IOS) ' ',RCGY
            WRITE(CM13(19)(45:54),31,IOSTAT=IOS) ' ',RCGZ
            WRITE(CM13(20)(45:54),31,IOSTAT=IOS) ' ',RWT
            DO I=1,20
              CALL VT_OUT(' '//CM13(I)(1:LAN(CM13(I))))
            END DO
          END IF
        ELSE IF(IM.EQ.14) THEN         !Decode message I14
          ISTS=JZEXT(JDATA(1))
          IVEN=JZEXT(JDATA(2))
          IMDE=JZEXT(JDATA(3))
          ISFI=JZEXT(JDATA(4))
          IF(CMODE(1:4).EQ.'DATA') THEN
            WRITE(CIL,11,IOSTAT=IOS) CE//'3;30H',ISTS
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,11,IOSTAT=IOS) CE//'4;30H',IVEN
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,11,IOSTAT=IOS) CE//'5;30H',IMDE
            CALL VT_OUT(CIL(1:LAN(CIL)))
            WRITE(CIL,11,IOSTAT=IOS) CE//'6;30H',ISFI
            CALL VT_OUT(CIL(1:LAN(CIL)))
            DO I=11,21
              WRITE(CIL,41,IOSTAT=IOS) CE,I-3,';30H',JDATA(I)
              CALL VT_OUT(CIL(1:LAN(CIL)))
            END DO
            DO I=22,32
              WRITE(CIL,41,IOSTAT=IOS) CE,I-14,';70H',JDATA(I)
              CALL VT_OUT(CIL(1:LAN(CIL)))
            END DO
          ELSE IF(CMODE.EQ.'FULL') THEN
            WRITE(CM14(3)(29:33),11,IOSTAT=IOS) ' ',ISTS
            WRITE(CM14(4)(29:33),11,IOSTAT=IOS) ' ',IVEN
            WRITE(CM14(5)(29:33),11,IOSTAT=IOS) ' ',IMDE
            WRITE(CM14(6)(29:33),11,IOSTAT=IOS) ' ',ISFI
            DO I=11,21
              WRITE(CM14(I-3)(29:33),11,IOSTAT=IOS) ' ',JDATA(I)
            END DO
            DO I=22,32
              WRITE(CM14(I-14)(69:73),11,IOSTAT=IOS) ' ',JDATA(I)
            END DO
            DO I=1,18
              CALL VT_OUT(' '//CM14(I)(1:LAN(CM14(I))))
            END DO
          END IF
        ELSE IF(IM.EQ.99) THEN         !Decode miscellaneous parameters
          CTEMP=CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)
          DO I=1,60
            IF(CMISC(I).EQ.CTEMP) CMISC(I)='      '
          END DO
          DO I=1,20
            CM99(2+I)(15:20)=CMISC(I)
          END DO
          DO I=21,40
            CM99(2+I-20)(42:47)=CMISC(I)
          END DO
          DO I=41,60
            CM99(2+I-40)(69:74)=CMISC(I)
          END DO
          CALL INU_ISI_CSPLT(CMISC(4),' S23 45.6',CM99(6)(15:23))
          CALL INU_ISI_CSPLT(CMISC(5),'S123 45.6',CM99(7)(15:23))
          CALL INU_ISI_CSPLT(CMISC(6),' S23 45.6',CM99(8)(15:23))
          CALL INU_ISI_CSPLT(CMISC(7),'S123 45.6',CM99(9)(15:23))
          CALL INU_ISI_CSPLT(CMISC(9),'1345.6',CM99(11)(15:20))
          CALL INU_ISI_CSPLT(CMISC(10),'S345.6',CM99(12)(15:20))
          CALL INU_ISI_CSPLT(CMISC(11),'S345.6',CM99(13)(15:20))
          CALL INU_ISI_CSPLT(CMISC(12),'2345.6',CM99(14)(15:20))
          CALL INU_ISI_CSPLT(CMISC(13),'S345.6',CM99(15)(15:20))
          CALL INU_ISI_CSPLT(CMISC(14),'S345.6',CM99(16)(15:20))
          CALL INU_ISI_CSPLT(CMISC(15),'2345.6',CM99(17)(15:20))
          CALL INU_ISI_CSPLT(CMISC(16),'S345.6',CM99(18)(15:20))
          CALL INU_ISI_CSPLT(CMISC(17),'S345.6',CM99(19)(15:20))
          CALL INU_ISI_CSPLT(CMISC(18),'2345.6',CM99(20)(15:20))
          CALL INU_ISI_CSPLT(CMISC(19),'2345.6',CM99(21)(15:20))
          CALL INU_ISI_CSPLT(CMISC(20),'123 456',CM99(22)(14:20))
          CALL INU_ISI_CSPLT(CMISC(23),' S23 45.6',CM99(5)(42:51))
          CALL INU_ISI_CSPLT(CMISC(24),'S123 45.6',CM99(6)(42:51))
          CALL INU_ISI_CSPLT(CMISC(25),' S23 45.6',CM99(7)(42:51))
          CALL INU_ISI_CSPLT(CMISC(26),'S123 45.6',CM99(8)(42:51))
          CALL INU_ISI_CSPLT(CMISC(27),'123 456',CM99(9)(41:47))
          CALL INU_ISI_CSPLT(CMISC(28),'123 456',CM99(10)(41:47))
          CALL INU_ISI_CSPLT(CMISC(29),'123 456',CM99(11)(41:47))
          CALL INU_ISI_CSPLT(CMISC(30),'123 456',CM99(12)(41:47))
          CALL INU_ISI_CSPLT(CMISC(31),'123 456',CM99(13)(41:47))
          CALL INU_ISI_CSPLT(CMISC(32),'123 456',CM99(14)(41:47))
          CALL INU_ISI_CSPLT(CMISC(33),'123 456',CM99(15)(41:47))
          CALL INU_ISI_CSPLT(CMISC(34),'123 456',CM99(16)(41:47))
          CALL INU_ISI_CSPLT(CMISC(38),'S12 34 56',CM99(20)(42:51))
          CALL INU_ISI_CSPLT(CMISC(39),'S12 34 56',CM99(21)(42:51))
          CALL INU_ISI_CSPLT(CMISC(40),'S12 34 56',CM99(22)(42:51))
          CALL INU_ISI_CSPLT(CMISC(42),'S23456',CM99(4)(69:74))
          CALL INU_ISI_CSPLT(CMISC(48),'S123 45.6',CM99(10)(69:77))
          CALL INU_ISI_CSPLT(CMISC(49),'S234.56',CM99(11)(71:77))
          CALL INU_ISI_CSPLT(CMISC(50),'S12345.6',CM99(12)(69:76))
          IF(CMODE(1:4).EQ.'DATA') THEN
            DO I=3,22
              WRITE(CL,42,IOSTAT=IOS) I
              WRITE(CIL,24,IOSTAT=IOS) CE//CL//';14H'//CM99(I)(14:23)
              CALL VT_OUT(CIL(1:LAN(CIL)))
              WRITE(CIL,24,IOSTAT=IOS) CE//CL//';41H'//CM99(I)(41:51)
              CALL VT_OUT(CIL(1:LAN(CIL)))
              WRITE(CIL,24,IOSTAT=IOS) CE//CL//';69H'//CM99(I)(69:77)
              CALL VT_OUT(CIL(1:LAN(CIL)))
            END DO
          ELSE IF(CMODE.EQ.'FULL') THEN
            DO I=1,22
              CALL VT_OUT(' '//CM99(I)(1:LAN(CM99(I))))
            END DO
          END IF
        END IF
      END IF
      RETURN
10    FORMAT(X,A)
11    FORMAT(A,Z4.4)
12    FORMAT(A,F8.6)
13    FORMAT(A,F12.6)
14    FORMAT(A,F8.3)
15    FORMAT(A,F8.3,2X,F8.3)
16    FORMAT(A,F11.5)
17    FORMAT(A,F13.10)
18    FORMAT(A,F12.7,' deg  ',I6)
19    FORMAT(A,I6,4X,I6)
20    FORMAT(A,F8.3,2X,F8.3,2X,F8.3)
21    FORMAT(A,I5,' s    ',F8.3)
22    FORMAT(A,F12.6,2X,F12.6)
23    FORMAT(A,F9.3)
24    FORMAT(A)
25    FORMAT(A,I5,4X,I5)
26    FORMAT(A,F12.7)
27    FORMAT(A,I4,' s   ',A)
28    FORMAT(A,F8.4)
29    FORMAT(A,F7.5)
30    FORMAT(A,F7.2,2X,F7.2)
31    FORMAT(A,F9.4)
32    FORMAT(A,F10.5)
33    FORMAT(A,I6)
34    FORMAT(A,F8.5)
35    FORMAT(A,F7.4)
36    FORMAT(A,F8.6)
37    FORMAT(A,F6.2)
38    FORMAT(A,I5)
39    FORMAT(A,F6.1)
40    FORMAT(A,F6.3)
41    FORMAT(A,I2.2,A,Z4.4)
42    FORMAT(I2.2)
43    FORMAT(A,F8.3,X,F8.3)
44    FORMAT(A,F7.3)
45    FORMAT(A,I6,' ft ',F12.7)
      END
C*******************************************************************************
      INTEGER*4 FUNCTION INU_ISI_ISWAP(JDATA)
C
C Returns an I*4 word from the two I*2 words in the JDATA array, but with the
C order of the two words swapped.
C
      IMPLICIT  NONE
      INTEGER*2 JDATA(2),JTEMP(2)
      INTEGER*4 ITEMP
      EQUIVALENCE (ITEMP,JTEMP)
      JTEMP(1)=JDATA(2)
      JTEMP(2)=JDATA(1)
      INU_ISI_ISWAP=ITEMP
      RETURN
      END
C*******************************************************************************
      SUBROUTINE INU_ISI_CSPLT(CMISC,CSTR,CLINE)
C
C Takes a 6 character miscellaneous parameter string and formats it according
C to the specification string given by CSTR, and puts the result in CLINE.
C
      IMPLICIT  NONE
      INTEGER*4 IL,IP,I
      CHARACTER CMISC*6,CSTR*(*),CLINE*(*),CS*1

      CS=' '
      IF(ICHAR(CMISC(1:1)).GT.127) THEN
        CS='-'
        CMISC(1:1)=CHAR(ICHAR(CMISC(1:1))-128)
      END IF
      IL=LEN(CSTR)
      IP=1
      DO I=1,IL
        IF(CSTR(I:I).EQ.'S') THEN
          CLINE(IP:IP)=CS
        ELSE IF(CSTR(I:I).EQ.'1') THEN
          CLINE(IP:IP)=CMISC(1:1)
        ELSE IF(CSTR(I:I).EQ.'2') THEN
          CLINE(IP:IP)=CMISC(2:2)
        ELSE IF(CSTR(I:I).EQ.'3') THEN
          CLINE(IP:IP)=CMISC(3:3)
        ELSE IF(CSTR(I:I).EQ.'4') THEN
          CLINE(IP:IP)=CMISC(4:4)
        ELSE IF(CSTR(I:I).EQ.'5') THEN
          CLINE(IP:IP)=CMISC(5:5)
        ELSE IF(CSTR(I:I).EQ.'6') THEN
          CLINE(IP:IP)=CMISC(6:6)
        ELSE
          CLINE(IP:IP)=CSTR(I:I)
        END IF
        IP=IP+1
      END DO
      RETURN
      END
C*******************************************************************************
      SUBROUTINE INU_TRANS_ACCL(RAX,RAY,RAZ,RROL,RPIT,RHDG,RAF,RAL,RAU)
C
C Transforms accelerations from the platform (navigation) frame to the 
C aircraft frame.  Uses the transpose of the Direction Cosine Matrix defined
C in SNU 84-1 Rev D, section 6.5.2.
C
      IMPLICIT NONE
      REAL*4   RAX,RAY,RAZ,RROL,RPIT,RHDG,RAF,RAL,RAU,SA,CA,SP,CP,SR,CR
      REAL*4   RT(3,3)                 !Full transformation matrix

      SA=SIND(RHDG)                    !Compute sines and cosines
      CA=COSD(RHDG)
      SP=SIND(RPIT)
      CP=COSD(RPIT)
      SR=SIND(RROL)
      CR=COSD(RROL)
      RT(1,1)=CA*CP                    !Load matrix
      RT(2,1)=CA*SP*SR
      RT(3,1)=CA*SP*CR
      RT(1,2)=-SA*CP
      RT(2,2)=-SA*SP*SR-CA*CR
      RT(3,2)=CA*SR-SA*SP*CR
      RT(1,3)=SP
      RT(2,3)=-CP*SR
      RT(3,3)=-CP*CR
      RAF=RT(1,1)*RAX+RT(1,2)*RAY+RT(1,3)*RAZ !Transform accelerations
      RAL=RT(2,1)*RAX+RT(2,2)*RAY+RT(2,3)*RAZ
      RAU=RT(3,1)*RAX+RT(3,2)*RAY+RT(3,3)*RAZ
      RAF=RAF/9.8                      !Convert m/s/s to g
      RAL=-RAL/9.8                     !Convert m/s/s to g and stbd to port
      RAU=-RAU/9.8                     !Convert m/s/s to g and down to up
      RETURN
      END
C*******************************************************************************
      SUBROUTINE GPS1_DISP
C
C Puts up the full GPS display for the XR5 receiver.  Note that apart from the
C background, this
C routine only updates information that is being received from the GPS 
C receiver.  Depending on the working of the H_GPS_LOG process, not all
C information may be routinely updated.  This display has no provision for
C displaying past data.  All display is done in VT220 mode, rather than using
C the SBD graphics capability, and as a result may be rather small on the
C aircraft scientist's display.  This routine is identical to the GPS_DISP
C routine except that it calls GPS1_DISP_MESS rather than GPS_DISP_MESS.
C
C Subroutines: GPS1_DISP_MESS_VT GPS1_SUBS.FOR
C
C V1.00  22/09/93  W.D.N.JACKSON
C
      IMPLICIT  NONE
      INTEGER*4 IKEY,IPTR
      LOGICAL*4 LWAITING,LOOP
      INCLUDE   'HCOM_DEF.FOR'

      CALL GPS1_DISP_MESS_VT(IGPSMESS,'BACKGROUND') !Put up background
      CALL VT_OUT(CHAR(0)//CHAR(27)//'[24;1HE - Exit')
      LOOP=.TRUE.
      DO WHILE(LOOP)                   !Keep looping till user wants to exit
        IPTR=STATUS(17)                !Points to latest GPS message
        IF(IPTR.GE.1.AND.IPTR.LE.2.AND.STATUS(35).EQ.1) !If valid update display
     -      CALL GPS1_DISP_MESS_VT(IGPSMESS(1,IPTR),'UPDATE') !Display each mess
C
C Now wait for a valid key to be pressed, or for another GPS message to be
C indicated.
C
        LWAITING=.TRUE.
        DO WHILE(LWAITING)
          CALL CHECK_KEY('Ee',IKEY)
          IF(IKEY.NE.0) THEN           !If valid key
            LWAITING=.FALSE.           !Show got one
            LOOP=.FALSE.               !Enable loop exit
          END IF
          IF(STATUS(17).NE.IPTR) LWAITING=.FALSE. !If have new message
          IF(LWAITING) CALL LIB$WAIT(0.2) !Wait 0.2s before trying again
        END DO
      END DO
      CALL VT_OUT(CHAR(0)//CHAR(27)//'[24;79H') !Put cursor at end
      RETURN
10    FORMAT(A)
      END
C*******************************************************************************
      SUBROUTINE NDU_DISP
C
C Puts up a copy of the NDU display, which is retrieved from the file
C NDU_DISP.DAT.  H_NDU updates this file when STATUS(59) is set to 1.
C
C V2.00  02/02/02  W.D.N.JACKSON
C
      IMPLICIT  NONE
      INTEGER*4 IOS,IKEY,IL,LAN,I
      LOGICAL*4 LOOP,LWAITING
      CHARACTER CE*3,CLINE*80
      INCLUDE   'HCOM_DEF.FOR'

      CE=CHAR(0)//CHAR(27)//'['
10    CALL VT_OUT(CE//'2J'//CHAR(27)//'[1;1H')
      CALL VT_OUT
     &  (CHAR(0)//'                           Navigators Display Unit')
C
      OPEN(UNIT=12,FILE='[HORACE]H_NDU.DAT',STATUS='OLD',READONLY,
     &    IOSTAT=IOS)
      READ(12,'(A)',IOSTAT=IOS) CLINE
      IL=0
      CALL VT_OUT(CE//'6;1H')
      DO WHILE(IOS.EQ.0.AND.IL.LT.16)
        IF(CLINE(1:1).NE.'!') THEN
          IL=IL+1
          CALL VT_OUT(' '//CLINE(1:LAN(CLINE)))
        END IF
        READ(12,'(A)',IOSTAT=IOS) CLINE
      END DO
      CLOSE(12,IOSTAT=IOS)         
      CALL VT_OUT(' ')
      CALL VT_OUT(' E - Exit')
C
      CALL VT_OUT(CE//'8;50H 0 - Reset NDU')
      CALL VT_OUT(CE//'9;50H 1 - Set air position 1')
      CALL VT_OUT(CE//'10;50H 2 - Set air position 2')
      CALL VT_OUT(CE//'11;50H 3 - Set ground position 1')
      CALL VT_OUT(CE//'12;50H 4 - Set ground position 2')
      CALL VT_OUT(CE//'13;50H 5 - Stop/start updating')
      CALL VT_OUT(CE//'14;50H 6 - Step line 3 positions')
      CALL VT_OUT(CE//'15;50H 7 - Step line 4 positions')
      CALL VT_OUT(CE//'16;50H 8 - INS/Corrected INS')
      CALL VT_OUT(CE//'17;50H 9 - Backlight on/off')
      STATUS(61)=0
C
      STATUS(59)=1
      LOOP=.TRUE.
      DO WHILE(LOOP)                   !Keep looping till user wants to exit
        IF(STATUS(61).EQ.1) GOTO 10
        IF(STATUS(59).EQ.0) THEN
          STATUS(59)=1
          CALL VT_OUT(CE//'2;1H')
          DO I=1,40
            IF(ICHAR(CNDU(1)(I:I)).EQ.223) CNDU(1)(I:I)=CHAR(176)
            IF(ICHAR(CNDU(2)(I:I)).EQ.223) CNDU(2)(I:I)=CHAR(176)
            IF(ICHAR(CNDU(3)(I:I)).EQ.223) CNDU(3)(I:I)=CHAR(176)
            IF(ICHAR(CNDU(4)(I:I)).EQ.223) CNDU(4)(I:I)=CHAR(176)
          END DO
          CALL VT_OUT(' '//CNDU(1))
          CALL VT_OUT(' '//CNDU(2))
          CALL VT_OUT(' '//CNDU(3))
          CALL VT_OUT(' '//CNDU(4))
          CALL VT_OUT(CE//'24;79H')    !Put cursor at end
        END IF
C
C Now wait for a valid key to be pressed, or for another DRS block to be
C indicated.
C
        LWAITING=.TRUE.
        DO WHILE(LWAITING)
          CALL CHECK_KEY('0123456789Ee',IKEY)
          IF(IKEY.GE.48.AND.IKEY.LE.57) THEN
            STATUS(60)=IKEY            !Emulate a Nav's keypress
            IKEY=0
          END IF
          IF(IKEY.NE.0) THEN           !If valid key
            LWAITING=.FALSE.           !Show got one
            LOOP=.FALSE.               !Enable loop exit
          END IF
          IF(STATUS(59).EQ.0) THEN
            LWAITING=.FALSE.           !Show got one
          END IF
          IF(LWAITING) CALL LIB$WAIT(0.2) !Wait 0.2s before trying again
        END DO
      END DO
      CALL VT_OUT(CHAR(0)//CHAR(27)//'[24;79H') !Put cursor at end
      RETURN
      END
C*******************************************************************************
      SUBROUTINE GPS_MENU
C
C Displays the GPS menu options, and calls the appropriate routines to execute
C them.
C
C Subroutines: GPS1_DISP      INST_SUBS.FOR
C              GPS1_INIT      INST_SUBS.FOR
C              DRS_FOOT      INST_SUBS.FOR
C              DRS_PRINT     INST_SUBS.FOR
C              GPS_SELECT    INST_SUBS.FOR
C              ggs           GGS.OLB
C
C V1.00  23-02-91  W.D.N.JACKSON
C V1.01  22-09-93  W.D.N.JACKSON
C        Options 3 and 4 added to handle XR5 GPS receiver
C
      IMPLICIT  NONE
      CHARACTER CLINE(7)*50,COPTION*4
      INTEGER*4 I,IOPTION,NOPTIONS,IOS,ILEN
      LOGICAL*4 LGPS,LSELECT
C
C GPS menu contents
C
      DATA CLINE(1)/' '/
      DATA CLINE(2)/'                        GPS Menu'/
      DATA CLINE(3)/' '/
      DATA CLINE(4)/' 0) Exit menu'/
      DATA CLINE(5)/' 1) DISPLAY XR5 GPS data'/
      DATA CLINE(6)/' 2) INITIALISE XR5 GPS with new or current values'/
      DATA NOPTIONS /3/
C
      LGPS=.TRUE.
      DO WHILE(LGPS)                   !Keep looping till leave menu
        CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H')
        CALL VT_OUT(CHAR(0)//CLINE(1)//CHAR(13))
        DO I=2,NOPTIONS+3              !Display the options
          CALL VT_OUT(' '//CLINE(I))
        END DO
        CALL VT_OUT(' ')
        LSELECT=.TRUE.
        DO WHILE(LSELECT)              !Loop until have valid entry
          CALL VT_OUT('$Enter selection [0]... ')
          CALL VT_IN(COPTION,ILEN,IOS)
          IF(IOS.EQ.-1) RETURN
          READ(COPTION,'(BN,I)',IOSTAT=IOS) IOPTION
          IF(IOPTION.EQ.0) RETURN
          IF(IOPTION.GE.1.AND.IOPTION.LT.NOPTIONS) LSELECT=.FALSE.
        END DO
        CALL VT_OUT('  ')
!        WRITE(10,'(A)',IOSTAT=IOS) ' '
        IF(IOPTION.EQ.1) THEN
          CALL INPUT_AST               !Enable unsolicited input
          CALL GPS1_DISP               !Display XR5 GPS data
          CALL INPUT_ABORT
        ELSE IF(IOPTION.EQ.2) THEN
          CALL GPS1_INIT               !Initialise with new values
        END IF
      END DO                           !Else give this menu again
      RETURN
      END
C*******************************************************************************
      SUBROUTINE GPS1_INIT
C
C Handles initialization of the XR5 GPS receiver by forming an initialization 
C message in response to user input, and putting it in the IGPSMESS global
C common area, from where H_GPS_LOG will send it to the GPS receiver.
C Only the initial values are changed, with the user being
C given the current values as measured by other aircraft systems as the default
C option.  Keys A to C allow return to higher level menus before the message
C is sent.  This routine is similar to the GPS_INIT routine, but matches the
C requirements of the XR5 receiver.
C
C Note that Dynamics and Baud can not be set by the INIT command on the XR5, 
C and initial velocity is not needed.  Use the CDU program on the Toshiba 
C portable PC to set the dynamics to 4 if necessary.
C
C V1.00  22/09/93  W.D.N.JACKSON
C
      IMPLICIT  NONE
      INTEGER*4 IN,IOS,I,IM,IS,ILEN,IPTR,ICNT,IRECID,IINITW
      LOGICAL*4 LSELECT
      REAL*4    RINILAT,RINILNG,RINIALT
      CHARACTER CTIME*8,CLINE*56,CDATE*9,CINIDAT*9,CINITIM*8
      CHARACTER CMON(12)*3,CSTRING*10,CDATE1*8,CVAL*1
      DATA CMON /'JAN','FEB','MAR','APR','MAY','JUN',
     -           'JUL','AUG','SEP','OCT','NOV','DEC'/
      INCLUDE   'HCOM_DEF.FOR'
      INTEGER*2 IMESS(33)              !Temporary store for message
      INTEGER*4 ISV(4),LAN
C
C Show the current values being computed by HORACE on the assumption that the
C aircraft is flying.
C
      CALL VT_OUT(' Current values are:')
      CALL DRS_CDAT(NFDATA(1,STATUS(1)),CDATE)
      CALL VT_OUT(' Date      '//CDATE//'      (DRS)')
      CALL DRS_CTIM(NFDATA(1,STATUS(1)),CTIME)
      CALL VT_OUT(' Time       '//CTIME//'      (DRS)')
      WRITE(CLINE,12,IOSTAT=IOS) RGLAT(STATUS(2))
      CALL VT_OUT(' '//CLINE)
      WRITE(CLINE,13,IOSTAT=IOS) RGLNG(STATUS(2))
      CALL VT_OUT(' '//CLINE)
      WRITE(CLINE,14,IOSTAT=IOS) RPHGT(STATUS(2))
      CALL VT_OUT(' '//CLINE)
      LSELECT=.TRUE.
      DO WHILE(LSELECT)                !Loop till have valid param 1
        CALL VT_OUT('$Use current values? 0-No 1-Yes [1]... ') !Get param
        CALL VT_IN(CVAL,ILEN,IOS)
        IF(IOS.EQ.-1) RETURN
        READ(CVAL,'(BN,I)',IOSTAT=IOS) IN
        IF(ILEN.EQ.0) IN=1
        IF(IN.GE.0.AND.IN.LE.1) LSELECT=.FALSE.
      END DO
C
C If the user wants to use the current values, simply read them from the
C HORACE stores.
C
      IF(IN.EQ.1) THEN
        CALL DRS_CDAT(NFDATA(1,STATUS(1)),CINIDAT)
        CALL DRS_CTIM(NFDATA(1,STATUS(1)),CINITIM)
        RINILAT=RGLAT(STATUS(2))
        RINILNG=RGLNG(STATUS(2))
        RINIALT=RPHGT(STATUS(2))
C
C If the user doesn't want to use the current values, assume that all systems
C are not available so the aircraft is probably on the ground.  Offer the
C Boscombe Down location and HORACE time as the default.
C
      ELSE
        CALL VT_OUT(' ')
        CALL VT_OUT(' Defaults are Woodford pan and HORACE time')
C
C Get date.  Use dd-mm-yy format since keypads don't have full alphabet
C
        IOS=1
        DO WHILE(IOS.NE.0)
          IOS=0
          CALL DATE1(CDATE)
          DO I=1,9
            IF(CDATE(I:I).EQ.' ') CDATE(I:I)='0'
          END DO
          DO I=1,12
            IF(CDATE(4:6).EQ.CMON(I)) IM=I
          END DO
          WRITE(CDATE1,11,IOSTAT=IS) CDATE(1:3),IM,CDATE(7:9)
          CALL VT_OUT('$Date      (dd-mm-yy) ['//CDATE1//'] ')
          CALL VT_IN(CSTRING,ILEN,I)
          IF(I.EQ.-1) RETURN
          IF(ILEN.EQ.0) CSTRING=CDATE1
          IF(CSTRING(1:2).LT.'01'.OR.CSTRING(1:2).GT.'31') IOS=1
          IF(CSTRING(4:5).LT.'01'.OR.CSTRING(4:5).GT.'12') IOS=1
          IF(CSTRING(7:8).LT.'00'.OR.CSTRING(7:8).GT.'99') IOS=1
          IF(IOS.EQ.0) THEN
            CINIDAT='xx-xxx-xx'
            CINIDAT(1:2)=CSTRING(1:2)
            READ(CSTRING(4:5),16,IOSTAT=IOS) IM
            CINIDAT(4:6)=CMON(IM)
            CINIDAT(8:9)=CSTRING(7:8)
          END IF
        END DO
C
C Get latitude
C
        IOS=1
        DO WHILE(IOS.NE.0)
          CALL VT_OUT('$Latitude  (deg)      [53.37133] ')
          CALL VT_IN(CSTRING,ILEN,I)
          IF(I.EQ.-1) RETURN
          IF(ILEN.EQ.0) CSTRING='53.37133'
          READ(CSTRING(1:LAN(CSTRING)),*,IOSTAT=IOS) RINILAT
          IF(RINILAT.GT.90.0.OR.RINILAT.LT.-90.0) IOS=1
        END DO
C
C Get longitude
C
        IOS=1
        DO WHILE(IOS.NE.0)
          CALL VT_OUT('$Longitude (deg)      [-2.14883] ')
          CALL VT_IN(CSTRING,ILEN,I)
          IF(I.EQ.-1) RETURN
          IF(ILEN.EQ.0) CSTRING='-2.14883'
          READ(CSTRING(1:LAN(CSTRING)),*,IOSTAT=IOS) RINILNG
          IF(RINILNG.GT.180.0.OR.RINILNG.LT.-180.0) IOS=1
        END DO
C
C Get altitude
C
        IOS=1
        DO WHILE(IOS.NE.0)
          CALL VT_OUT('$Altitude  (m)            [80.0] ')
          CALL VT_IN(CSTRING,ILEN,I)
          IF(I.EQ.-1) RETURN
          IF(ILEN.EQ.0) CSTRING='80.0'
          READ(CSTRING(1:LAN(CSTRING)),*,IOSTAT=IOS) RINIALT
          IF(RINIALT.GT.10000.0.OR.RINIALT.LT.0.0) IOS=1
        END DO
C
C Get time in hh.mm.ss format since no :'s on keypads
C
        IOS=1
        DO WHILE(IOS.NE.0)
          IOS=0
          CALL TIME(CTIME)
          CTIME(3:3)='.'
          CTIME(6:6)='.'
          CALL VT_OUT('$GMT time  (hh.mm.ss) ['//CTIME//'] ')
          CALL VT_IN(CSTRING,ILEN,I)
          IF(I.EQ.-1) RETURN
          IF(ILEN.EQ.0) CSTRING=CTIME
          CINITIM=CSTRING
          IF(CINITIM(1:2).LT.'00'.OR.CINITIM(1:2).GT.'23') IOS=1
          IF(CINITIM(4:5).LT.'00'.OR.CINITIM(4:5).GT.'59') IOS=1
          IF(CINITIM(7:8).LT.'00'.OR.CINITIM(7:8).GT.'59') IOS=1
          CINITIM(3:3)=':'
          CINITIM(6:6)=':'
        END DO
      END IF
C
C Fill in the rest of the message with standard defaults
C
      IRECID=0
      IINITW=0.OR.'0001'X.OR.'0004'X.OR.'0010'X !Init rec, use Iono, use Tropo
      ISV(1)=0
      ISV(2)=0
      ISV(3)=0
      ISV(4)=0
C
C Code the message and put into the HCOM common block when the H_GPS_LOG
C process is idle.
C
      CALL GPS1_CODE_MESS1(IRECID,IINITW,ISV,CINIDAT,CINITIM,RINILAT,
     -    RINILNG,RINIALT,IMESS)
      IPTR=STATUS(17)
      ICNT=0
      DO WHILE(IPTR.EQ.STATUS(17).AND.ICNT.NE.25)
        CALL LIB$WAIT(0.2)
        ICNT=ICNT+1
      END DO
      IF(ICNT.NE.25) THEN              !If GPS/H_GPS_LOG is responding
        IPTR=STATUS(17)
        DO I=1,33
          IGPSMESS(I,IPTR)=IMESS(I)
        END DO
        STATUS(17)=STATUS(17)+10
      END IF
      RETURN
11    FORMAT(A,I2.2,A)
12    FORMAT('Latitude   ',F8.4,' deg  (GPS)')
13    FORMAT('Longitude ',F9.4,' deg  (GPS)')
14    FORMAT('Altitude   ',F8.0,' m    (PRESSURE)')
16    FORMAT(I2)
      END
!*******************************************************************************
      SUBROUTINE NEPH_MENU
!
! PURPOSE : Displays menu of nephelometer options
!
! DESCRIPTION : writes menu to display, takes input from keyboard and
!               sends commands to H_NEPH via the varaible STATUS.
!
! INPUT / OUTPUT
!         STATUS(48)   Set to true if any display is in the nephelometer menu
!         STATUS(49)   Zero mode ; 0=manual, 1=auto, 2->24=air chop, 
!                      100=zero val, <0 no cmd
!         STATUS(50)   auto zero period (10-9999), <0 no cmd
!         STATUS(51)   backscatter mode; 0 normal, 1 backscatter, <0 no cmd
!         STATUS(52)   heater; 0 off, 1 on, <0 no cmd
!         STATUS(53)   power; 0 off, 1 on, <0 no cmd
!                      Command STATUS variables (49 thro' 53) are +1 and set
!                      negative to show last command given.
!
! REFERENCES : Nephelometer instruction manual
!              Instrument appendix ..
!              Nephelometer internal technical note
!
! AUTHOR :   V1.00 D P BRIGGS
!
! CHANGES :
!
      IMPLICIT  NONE
      CHARACTER CLINE(14)*50,CVAL*6,CN*4,CR*4,CLINE1*56,COPT*4
      INTEGER*4 I,NOPTIONS,ISTATVAL,IOS,IOPTION,IOPT,LAN,ILEN
      INTEGER*4 ISVAL(11),IBVAL(11),ITVAL(11),ICOL(11)
      LOGICAL*4 LSELECT,LHIGH
      INCLUDE   'HCOM_DEF.FOR'
!
! Nephelometer menu contents
!
      DATA CLINE(1)/'                   Nephelometer Menu'/
      DATA CLINE(2)/' '/
      DATA CLINE(3)/' 0) Exit menu'/
      DATA CLINE(4)/' 1) Zero Calibration'/
      DATA CLINE(5)/' 2) Manual Zero Cal Mode'/
      DATA CLINE(6)/' 3) Auto Zero Cal Mode'/
      DATA CLINE(7)/' 4) Air-Chop Auto Zero Cal Mode'/
      DATA CLINE(8)/' 5) Auto Zero Cal Period'/
      DATA CLINE(9)/' 6) Set Mode to Backscatter'/
      DATA CLINE(10)/' 7) Set Mode to Normal'/
      DATA CLINE(11)/' 8) Heater On'/
      DATA CLINE(12)/' 9) Heater Off'/
      DATA CLINE(13)/'10) Power Up'/
      DATA CLINE(14)/'11) Power Down'/
      DATA NOPTIONS /11/
      DATA ISVAL    / 49,49,49,49,  50,51,51,52,52,53,53/
      DATA IBVAL    /100,00,01,02,  10,01,00,01,00,01,00/
      DATA ITVAL    /124,00,01,24,9999,01,00,01,00,01,00/
      DATA ICOL     /  0, 1, 1, 1,   0, 1, 1, 1, 1, 1, 1/
!
      CN=CHAR(27)//'[0m'               !Invoke normal
      CR=CHAR(27)//'[7m'               !Invoke reverse video
!      status(48)=.false.
      IF (.NOT.STATUS(48)) THEN        !if no other display is in neph menu
        STATUS(48) = .TRUE. 
        DO WHILE(.TRUE.)               !Keep looping till leave menu
          CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H')
          CALL VT_OUT(CHAR(0)//CLINE(1)//CHAR(13))
          CALL VT_OUT(' '//CLINE(2))
          CALL VT_OUT(' '//CLINE(3))
!          
          DO I=1,NOPTIONS              !Display the options
            IF (STATUS(ISVAL(I)) .LT. 0) THEN
              ISTATVAL = - 1 - STATUS(ISVAL(I))
            ELSE 
              ISTATVAL = STATUS(ISVAL(I))
            END IF 
            IF (ICOL(I) .GT. 0 .AND. 
     &          ISTATVAL .GE. IBVAL(I) .AND.
     &          ISTATVAL .LE. ITVAL(I)) THEN
              LHIGH=.TRUE.
            ELSE
              LHIGH=.FALSE.
            END IF
            IF (I .EQ. 4) THEN
              WRITE(CVAL,'(2X,I4)') MOD(ISTATVAL,100)
            ELSE IF (I .EQ. 5) THEN
              WRITE(CVAL,'(2X,I4)') ISTATVAL
            ELSE
              CVAL = '      '
            END IF
            IF(LHIGH) THEN
              CLINE1=CLINE(I+3)(1:32)//CVAL
              CLINE1(4:4)='*'
              IOS=LAN(CLINE1)
              CALL VT_OUT(' '//CLINE1(1:4)//CR//CLINE1(5:IOS)//CN)
            ELSE
              CALL VT_OUT(' '//CLINE(I+3)(1:32)//CVAL)
            END IF
          END DO
!
          CALL VT_OUT(' ')
          LSELECT=.TRUE.
          DO WHILE(LSELECT)              !Loop until have valid entry
            CALL VT_OUT('$Enter selection [0]... ')
            CALL VT_IN(COPT,ILEN,IOS)
            IF(IOS.EQ.-1) THEN
              STATUS(48)=.FALSE.
              RETURN
            END IF
            READ(COPT,'(BN,I)',IOSTAT=IOS) IOPTION
            IF(IOS.NE.0.OR.IOPTION.EQ.0) THEN
              STATUS(48)=.FALSE.
              RETURN
            END IF
            IF(IOPTION.GE.1.AND.IOPTION.LE.NOPTIONS) LSELECT=.FALSE.
          END DO
          CALL VT_OUT('  ')
!          WRITE(10,'(A)',IOSTAT=IOS) ' '
          IF(IOPTION.EQ.1 .AND. STATUS(49) .LT. 0) THEN  !zero
            STATUS(49) = 99 - STATUS(49) 
          ELSE IF(IOPTION.EQ.2 .AND. STATUS(49) .LT. 0) THEN  !manual zero cal
            STATUS(49) = 0
          ELSE IF(IOPTION.EQ.3 .AND. STATUS(49) .LT. 0) THEN  !auto zero cal
            STATUS(49) = 1
          ELSE IF(IOPTION.EQ.4 .AND. STATUS(49) .LT. 0) THEN  !air chop zero cal
            LSELECT=.TRUE.
            DO WHILE(LSELECT)              !Loop until have valid entry
              CALL VT_OUT('$Number of cals (2-24)... ')
              CALL VT_IN(COPT,ILEN,IOS)
              READ(COPT,'(BN,I)',IOSTAT=IOS) IOPT
              IF(IOPT.GE.2.AND.IOPT.LE.24) LSELECT=.FALSE.
            END DO
            STATUS(49) = IOPT
          ELSE IF(IOPTION.EQ.5 .AND. STATUS(50) .LT. 0) THEN  !auto zero period
            LSELECT=.TRUE.
            DO WHILE(LSELECT)              !Loop until have valid entry
              CALL VT_OUT('$Auto-zero period (secs)... ')
              CALL VT_IN(COPT,ILEN,IOS)
              READ(COPT,'(BN,I)',IOSTAT=IOS) IOPT
              IF(IOPT.GE.10.AND.IOPT.LE.9999) LSELECT=.FALSE.
            END DO
            STATUS(50) = IOPT
          ELSE IF(IOPTION.EQ.6 .AND. STATUS(51) .LT. 0) THEN !set mode backscatter
            STATUS(51) = 1
          ELSE IF(IOPTION.EQ.7 .AND. STATUS(51) .LT. 0) THEN !set mode normal
            STATUS(51) = 0
          ELSE IF(IOPTION.EQ.8 .AND. STATUS(52) .LT. 0) THEN  !heater on
            STATUS(52) = 1
          ELSE IF(IOPTION.EQ.9 .AND. STATUS(52) .LT. 0) THEN  !heater off
            STATUS(52) = 0
          ELSE IF(IOPTION.EQ.10 .AND. STATUS(53) .LT. 0) THEN !power up
            STATUS(53) = 1
          ELSE IF(IOPTION.EQ.11 .AND. STATUS(53) .LT. 0) THEN !power down
            STATUS(53) = 0
          END IF
        END DO                           !Else give this menu again
      ELSE
        CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H')
        CALL VT_OUT(CHAR(0)//CLINE(1)//CHAR(13))
        CALL VT_OUT(' '//CLINE(2))
        CALL VT_OUT(' Nephelometer menu is already in use')
        CALL VT_OUT(' ')
        CALL VT_OUT(' '//CLINE(3))
        CALL VT_OUT(' ')
        CALL VT_OUT('$Enter selection [0]... ')
        CALL VT_IN(COPT,ILEN,IOS)
      END IF 
      RETURN
      END
!********************************************************************************
      SUBROUTINE SAT_MENU
!
! Displays the INU menu options, and calls the appropriate routines to execute
! them.  Note that entering 99 calls up a menu of special case options for 
! use while experimenting with the system.
!
! All communication between H_DISPLAY, of which these routines are part, and
! H_SATCOM, which carries out all communication with the Satcom system itself,
! takes place through the STATUS(47) word of the H_COMMON global common, and
! through the following shared files:
!
!   [.SATCOM]SAT_DISP.DAT
!   [.SATCOM]SAT_INP_STS.DAT
!   [.SATCOM]SAT_OUT_STS.DAT
!   [.SATCOM]SAT_INP_nnnnn.DAT
!   [.SATCOM]SAT_OUT_nnnnn.DAT
!
! The STATUS(47) bits are used as follows:
!
! Bit 0  Set when hardware info is required from the capsat
!        Cleared by H_SATCOM each time it gets new hardware information
! Bit 1  Set when GPS info is required from the capsat
!        Cleared by H_SATCOM each time it gets new GPS information
! Bit 2  Set when misc info is required from the capsat
!        Cleared by H_SATCOM each time it gets new misc information
! Bit 3  Set when mess info is required from the capsat
!        Cleared by H_SATCOM each time it gets new misc information
! Bit 4  Negated by H_SATCOM each time it gets new hardware information
! Bit 5  Negated by H_SATCOM each time it gets new GPS information
! Bit 6  Negated by H_SATCOM each time it gets new misc information
! Bit 7  Negated by H_SATCOM each time it gets new mess information
! Bit 8  Set to force H_SATCOM to scan the transmission log - SAT_OUT_STS.DAT
! Bit 9  Negated each time a change is made to SAT_INP_STS.DAT
! Bit 10 Negated each time a change is made to SAT_OUT_STS.DAT
! Bit 11 Set to disable position reporting
! Bit 12 Set for conditional ocean region forcing
! Bit 13 Set for unconditional ocean region forcing
! Bit 14,15 0 to 3 to force WAOR,EAOR,POR,IOR respectively
!
! Note that the address book, [.SATCOM]SAT_ADDR.DAT, needs to be kept up to
! date and matching that used at the base station.  It can be edited manually
! or an up to date file created on EAGLE and transferred on floppy disk.
!
! Subroutines: SAT_READ_MESS INST_SUBS.FOR
!              SAT_SEND_MESS INST_SUBS.FOR
!              SAT_LIST_MESS INST_SUBS.FOR
!              SAT_SHOW_STAT INST_SUBS.FOR
!              SAT_CHNG_MESS INST_SUBS.FOR
!              SAT_SPEC_OPTI INST_SUBS.FOR
!
! V1.00  09-03-95  W.D.N.JACKSON
!
      IMPLICIT  NONE
      CHARACTER CLINE(15)*55,COPTION*4
      INTEGER*4 I,ILINES,IOS,IOPTION,IOPTNS,ILEN
      LOGICAL*4 LSAT,LSELECT
!
! Satcom menu contents
!
      DATA CLINE(1)/' '/
      DATA CLINE(2)/'                      Satcom Menu'/
      DATA CLINE(3)/' '/
      DATA CLINE(4)/' 0) Exit menu'/
      DATA CLINE(5)/' 1) List received messages'/
      DATA CLINE(6)/' 2) Read received messages'/
      DATA CLINE(7)/' 3) List transmitted messages'/
      DATA CLINE(8)/' 4) Read transmitted messages'/
      DATA CLINE(9)/' 5) Show hardware status'/
      DATA CLINE(10)/' 6) Show GPS status'/
      DATA CLINE(11)/' 7) Show system status'/
      DATA CLINE(12)/' 8) Show system messages'/
      DATA CLINE(13)/' 9) Send a message'/
      DATA CLINE(14)/'10) Stop, resubmit, or copy a message'/
      DATA CLINE(15)/'11) Send a special message'/
!
      ILINES=15
      IOPTNS=12
      LSAT=.TRUE.
      DO WHILE(LSAT)                   !Keep looping till leave menu
        CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H')
        CALL VT_OUT(CHAR(0)//CLINE(1)//CHAR(13))
        DO I=2,ILINES                  !Display the options
          CALL VT_OUT(' '//CLINE(I))
        END DO
        CALL VT_OUT(' ')
        LSELECT=.TRUE.
        DO WHILE(LSELECT)              !Loop until have valid entry
          CALL VT_OUT('$Enter selection [0]... ')
          CALL VT_IN(COPTION,ILEN,IOS)
          IF(IOS.EQ.-1) RETURN
          READ(COPTION,'(BN,I)',IOSTAT=IOS) IOPTION
          IF(IOS.NE.0) RETURN
          IF(IOPTION.EQ.0) RETURN
          IF(IOPTION.GE.1.AND.IOPTION.LT.IOPTNS) LSELECT=.FALSE.
          IF(IOPTION.EQ.99) LSELECT=.FALSE. !Special secret option
        END DO
        CALL VT_OUT('  ')
!        WRITE(10,'(A)',IOSTAT=IOS) ' '
        IF(IOPTION.EQ.1) THEN
          CALL INPUT_AST
          CALL SAT_LIST_MESS('IN')     !List received messages
          CALL INPUT_ABORT
        ELSE IF(IOPTION.EQ.2) THEN
          CALL INPUT_AST
          CALL SAT_READ_MESS('IN')     !Read received messages
          CALL INPUT_ABORT
        ELSE IF(IOPTION.EQ.3) THEN
          CALL INPUT_AST
          CALL SAT_LIST_MESS('OUT')    !List transmitted messages
          CALL INPUT_ABORT
        ELSE IF(IOPTION.EQ.4) THEN
          CALL INPUT_AST
          CALL SAT_READ_MESS('OUT')    !Read transmitted messages
          CALL INPUT_ABORT
        ELSE IF(IOPTION.EQ.5) THEN
          CALL INPUT_AST
          CALL SAT_SHOW_STAT('HARD')   !Show hardware status
          CALL INPUT_ABORT
        ELSE IF(IOPTION.EQ.6) THEN
          CALL INPUT_AST
          CALL SAT_SHOW_STAT('GPS')    !Show GPS status
          CALL INPUT_ABORT
        ELSE IF(IOPTION.EQ.7) THEN
          CALL INPUT_AST
          CALL SAT_SHOW_STAT('MISC')   !Show misc status
          CALL INPUT_ABORT
        ELSE IF(IOPTION.EQ.8) THEN
          CALL INPUT_AST
          CALL SAT_SHOW_STAT('MESS')   !Show system messages
          CALL INPUT_ABORT
        ELSE IF(IOPTION.EQ.9) THEN
          CALL SAT_SEND_MESS('NORM')   !Send message
        ELSE IF(IOPTION.EQ.10) THEN
          CALL SAT_CHNG_MESS           !Change message status
        ELSE IF(IOPTION.EQ.11) THEN
          CALL SAT_SPEC_MESS           !Send a special message
        ELSE IF(IOPTION.EQ.99) THEN
          CALL SAT_SPEC_OPTI           !Special (secret) options
        END IF
      END DO                           !Else give this menu again
      RETURN
10    FORMAT(I2)
      END
!*******************************************************************************
      SUBROUTINE SAT_LIST_MESS(CMODE)
!
! If CMODE is IN then reads and displays the one line messages status entries
! from the SAT_INP_STS.DAT file, displaying the latest first and allowing the
! user to step back through earlier entries.  If CMODE is OUT then performs the
! same actions on the output (transmitted) message files.  Note that the
! status file is opened as a shared file so that all programs using it may
! read/write to it as they require.  If there is any change to the status file
! then the file is read and displayed afresh.
!
! Immediate options are P - PREV N - NEXT                            
!
! V1.00  09-03-95  W.D.N.JACKSON
!
      IMPLICIT  NONE
      INTEGER*4 LAN,IOS,IKEY,ILAST,ILSTVER,IFRST,LSTBIT,
     -    JOS,ICNT,IL
      LOGICAL*4 LOOP,LWAITING
      CHARACTER CLINE*80,CD*8,CE*8,CIFILE*30,CMODE*(*)
      INCLUDE   'HCOM_DEF.FOR'

      IF(CMODE.EQ.'IN') THEN
        CIFILE='[HORACE.SATCOM]SAT_INP_STS.DAT'
      ELSE IF(CMODE.EQ.'OUT') THEN
        CIFILE='[HORACE.SATCOM]SAT_OUT_STS.DAT'
      END IF
      CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H')
      LOOP=.TRUE.                      !Main loop variable
      OPEN(UNIT=11,FILE=CIFILE,STATUS='OLD',SHARED,
     &    ACCESS='DIRECT',RECL=20,IOSTAT=IOS) !Open the status file
      CLINE=' '
      READ(11,REC=1,IOSTAT=IOS) CLINE  !Read the first line to get no of mess's
      UNLOCK(11,IOSTAT=JOS)            !Release the record
      IF(IOS.NE.0.OR.CLINE(1:5).EQ.'00000') THEN !Can't open or read then tell
        CALL VT_OUT(' ')
        IF(CLINE(1:5).EQ.'00000') THEN
          CALL VT_OUT(' No messages')
        ELSE
          CALL VT_OUT(' Unable to open or read '//CIFILE)
        END IF
        CALL VT_OUT(CHAR(0)//CHAR(27)//'[24;1H'// !Put up options
     &    'E - Exit  ')
        LWAITING=.TRUE.                !Wait until user is ready to move up
        DO WHILE(LWAITING)
          CALL CHECK_KEY('Ee',IKEY)
          IF(IKEY.NE.0) THEN           !Wait for exit key
            LWAITING=.FALSE.
          ELSE
            CALL LIB$WAIT(0.2)         !Check 5 times a second
          END IF
        END DO
        LOOP=.FALSE.                   !Will leave this routine so don't loop
      ELSE                             !If status file read OK
        READ(CLINE(1:5),'(I5)',IOSTAT=IOS) ILSTVER !Get highest message vers num
        ILAST=ILSTVER                  !Display latest first
        IFRST=MAX(1,ILAST-21)          !Up to 22 lines per display 
      END IF        
!   
      DO WHILE(LOOP)                   !Keep looping till user wants to exit
        IF(CMODE.EQ.'IN') LSTBIT=BTEST(STATUS(47),9)
        IF(CMODE.EQ.'OUT') LSTBIT=BTEST(STATUS(47),10)
        READ(11,REC=IFRST+1,IOSTAT=IOS) CLINE !Read matching line from sts file
        CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)
     &      //'[1;1H'//CLINE(1:LAN(CLINE))//CHAR(13)) !Display first line
        IF(ILAST.GT.IFRST) THEN
          DO IL=IFRST+1,ILAST
            READ(11,REC=IL+1,IOSTAT=IOS) CLINE !Read matching line from sts file
            CALL VT_OUT(' '//CLINE(1:LAN(CLINE))) !Display it
          END DO
        END IF
        UNLOCK(11,IOSTAT=JOS)          !Release the last record
        CD=' '                         !Decide whether to allow steps back
        IF(IFRST.GT.1) CD='P - Prev '
        CE='N - Next'                    !If not latest version allow step forward
        IF(ILAST.GE.ILSTVER) CE=' ' 
        CALL VT_OUT(CHAR(0)//CHAR(27)//'[24;1H'// !Put up options
     &    'E - Exit  '//CD//'  '//CE)
!
! Now wait for a valid key to be pressed, or for a change to the status file
! as indicated by a change to bit 9 or 10 of STATUS(47).
!
        LWAITING=.TRUE.
        ICNT=0
        DO WHILE(LWAITING)
          CALL CHECK_KEY('ENPenp',IKEY)
          IF(CHAR(IKEY).EQ.'E'.OR.CHAR(IKEY).EQ.'e') THEN !Step up menu level(s)
            LWAITING=.FALSE.           !Show got one
            LOOP=.FALSE.               !Enable loop exit
          ELSE IF((CMODE.EQ.'IN'.AND.BTEST(STATUS(47),9).NE.LSTBIT)
     &        .OR.(CMODE.EQ.'OUT'.AND.BTEST(STATUS(47),10).NE.LSTBIT))
     &        THEN
            READ(11,REC=1,IOSTAT=IOS) CLINE !Read 1st line to get no of mess's
            UNLOCK(11,IOSTAT=JOS)      !Release the record
            READ(CLINE(1:5),'(I5)',IOSTAT=IOS) ILSTVER !Get top mess vers num
            ILAST=ILSTVER              !Display latest first
            IFRST=MAX(1,ILAST-21)      !Up to 22 lines per display 
            LWAITING=.FALSE.
          ELSE IF((CHAR(IKEY).EQ.'P'.OR.CHAR(IKEY).EQ.'p')
     &        .AND.CD.NE.' ') THEN !Previous page
            IFRST=MAX(1,IFRST-22)
            ILAST=MIN(IFRST+21,ILSTVER)
            LWAITING=.FALSE.
          ELSE IF((CHAR(IKEY).EQ.'N'.OR.CHAR(IKEY).EQ.'n')
     &        .AND.CE.NE.' ') THEN !Next page
            ILAST=MIN(ILAST+22,ILSTVER)
            IFRST=MAX(1,ILAST-21)
            LWAITING=.FALSE.
          END IF
          IF(LWAITING) CALL LIB$WAIT(0.2) !Wait 0.2s before trying again
          ICNT=ICNT+1                  !Bump wait count
        END DO
      END DO
      CLOSE(11,IOSTAT=IOS)
      CALL VT_OUT(CHAR(0)//CHAR(27)//'[24;79H') !Put cursor at end
      RETURN
10    FORMAT(A)
      END
!*******************************************************************************
      SUBROUTINE SAT_READ_MESS(CMODE)
!
! If CMODE is IN then reads and displays messages from SAT_INP_nnnn.DAT
! (received) files, displaying the latest first, and allowing user to step back
! through all files currently on hard disk.  Only displays one message at a
! time.  Also adds the status information for each file obtained from the file
! SAT_INP_STS.DAT to the top of each page.  If CMODE is OUT then performs the
! same actions using the output (transmitted) message files.  Note that the
! status file is opened as a shared file so that all programs using it may
! read/write to it as they require.  If there is any change to the status file
! then the latest file is displayed immediately.  Can display files of any
! length, but only displays 20 lines at a time; the next 20 lines are obtained
! by using the H immediate key.  Note that the first line of transmitted
! message files, which contains addressing information, is never displayed
!
! Immediate options are P - PRVMES  N - NXTMES  H - NXTSCR.
!
! V1.00  09-03-95  W.D.N.JACKSON
! V1.01  07-11-95  W.D.N.JACKSON
!        Modified to handle purged status files
!
      IMPLICIT  NONE
      INTEGER*4 LAN,IOS,IKEY,ICNT,ISTEP,IFILE,ILSTVER,I,IL,JOS,
     -    IFSTVER,LSTBIT,IREC
      LOGICAL*4 LOOP,LWAITING,LXIST
      CHARACTER CLINE*80,CVER*5,CD*8,CE*8,CMFILE*32,CIFILE*30,CMODE*(*)
      CHARACTER CS*8
      INCLUDE   'HCOM_DEF.FOR'

      IF(CMODE.EQ.'IN') THEN
        CIFILE='[HORACE.SATCOM]SAT_INP_STS.DAT'
        CMFILE='[HORACE.SATCOM]SAT_INP_nnnnn.DAT'
      ELSE IF(CMODE.EQ.'OUT') THEN
        CIFILE='[HORACE.SATCOM]SAT_OUT_STS.DAT'
        CMFILE='[HORACE.SATCOM]SAT_OUT_nnnnn.DAT'
      END IF

      CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H')
      LOOP=.TRUE.                      !Main loop variable
      OPEN(UNIT=11,FILE=CIFILE,STATUS='OLD',SHARED,
     &    ACCESS='DIRECT',RECL=20,IOSTAT=IOS) !Open the status file
      CLINE=' '
      READ(11,REC=1,IOSTAT=IOS) CLINE  !Read the first line to get no of mess's
      UNLOCK(11,IOSTAT=JOS)            !Release the record
      IF(IOS.NE.0.OR.CLINE(1:5).EQ.'00000') THEN !If can't open/read then tell
        CALL VT_OUT(' ')
        IF(CLINE(1:5).EQ.'00000') THEN
          CALL VT_OUT(' No messages')
        ELSE
          CALL VT_OUT(' Unable to open or read '//CIFILE)
        END IF
        CALL VT_OUT(CHAR(0)//CHAR(27)//'[24;1H'// !Put up options
     &    'E - Exit  ')
        LWAITING=.TRUE.                !Wait until user is ready to move up
        DO WHILE(LWAITING)
          CALL CHECK_KEY('Ee',IKEY)
          IF(IKEY.NE.0) THEN           !Wait for A, B or C to be pressed
            LWAITING=.FALSE.
          ELSE
            CALL LIB$WAIT(0.2)         !Check 5 times a second
          END IF
        END DO
        LOOP=.FALSE.                   !Will leave this routine so don't loop
      ELSE                             !If status file read OK
        READ(CLINE(1:5),'(I5)',IOSTAT=IOS) ICNT !Get record count
        READ(11,REC=2,IOSTAT=IOS) CLINE !Read the 2nd line to get first version
        UNLOCK(11,IOSTAT=IOS)
        READ(CLINE(1:5),'(I5)',IOSTAT=IOS) IFSTVER !Get record count
        ILSTVER=IFSTVER+ICNT-1
        IFILE=ILSTVER                  !Show latest version first
      END IF        
!   
      CS=' '
      ISTEP=0                          !No of lines in message to skip
      DO WHILE(LOOP)                   !Keep looping till user wants to exit
        IF(CMODE.EQ.'IN') LSTBIT=BTEST(STATUS(47),9)
        IF(CMODE.EQ.'OUT') LSTBIT=BTEST(STATUS(47),10)
        IREC=IFILE-IFSTVER+1
        READ(11,REC=IREC+1,IOSTAT=IOS) CLINE !Read matching line from sts file
        UNLOCK(11,IOSTAT=JOS)          !Release the record
        CALL VT_OUT(CHAR(0)//CLINE(1:LAN(CLINE))) !Display it
        CALL VT_OUT(' ')
        WRITE(CVER,'(I5.5)',IOSTAT=IOS) IFILE !Work out message file version
        CMFILE(24:28)=CVER
        OPEN(12,FILE=CMFILE,STATUS='OLD',READONLY,IOSTAT=IOS,
     &      CARRIAGECONTROL='NONE') !Open message file
        IF(IOS.NE.0) THEN              !If can't open it then tell user
          CALL VT_OUT(' Message file '//CMFILE//' does not exist')
        ELSE                           !Else read each line and display it
          DO I=1,ISTEP                 !Skip some lines if necessary
            READ(12,10,IOSTAT=IOS) CLINE
          END DO
          READ(12,10,IOSTAT=IOS) CLINE
          IL=0
          IF(IOS.EQ.0.AND.(CMODE.EQ.'IN'.OR.ISTEP.NE.0)) THEN
            CALL VT_OUT(' '//CLINE(1:LAN(CLINE))) !Don't write first transmit line
            IL=IL+1
          END IF
          ISTEP=ISTEP+1
          READ(12,10,IOSTAT=IOS) CLINE
          DO WHILE(IOS.EQ.0.AND.IL.LT.20)
            CALL VT_OUT(' '//CLINE(1:LAN(CLINE)))
            ISTEP=ISTEP+1
            IL=IL+1
            READ(12,10,IOSTAT=IOS) CLINE
          END DO
          IF(IOS.EQ.0) CS='H - Step'   !File not finished
          IF(IOS.NE.0) ISTEP=0         !All fits on one screen
          CLOSE(12,IOSTAT=IOS)         !Close message file
        END IF
        CD=' '                         !Decide whether to allow steps back
        IF(IFILE.GT.IFSTVER) THEN      !If not first file
          WRITE(CVER,'(I5.5)',IOSTAT=IOS) IFILE-1 !See if can open prev version
          CMFILE(24:28)=CVER
          INQUIRE(FILE=CMFILE,EXIST=LXIST)
          IF(LXIST) CD='P - Prev '        !If can open then allow step back
        END IF
        CE='N - Next '                    !If not latest version allow step forward
        IF(IFILE.GE.ILSTVER) CE=' ' 
        CALL VT_OUT(CHAR(0)//CHAR(27)//'[24;1H'// !Put up options
     &    'E - Exit  '//CD//'  '//CE//'  '//CS)
!
! Now wait for a valid key to be pressed, or for a new message to become
! available.
!
        LWAITING=.TRUE.
        ICNT=0
        DO WHILE(LWAITING)
          CALL CHECK_KEY('ENPSenps',IKEY)
          IF(CHAR(IKEY).EQ.'E'.OR.CHAR(IKEY).EQ.'e') THEN !Step up menu level(s)
            LWAITING=.FALSE.           !Show got one
            LOOP=.FALSE.               !Enable loop exit
          ELSE IF((CMODE.EQ.'IN'.AND.BTEST(STATUS(47),9).NE.LSTBIT)
     &        .OR.(CMODE.EQ.'OUT'.AND.BTEST(STATUS(47),10).NE.LSTBIT))
     &        THEN
            READ(11,REC=1,IOSTAT=IOS) CLINE !Read 1st line to get no of mess's
            UNLOCK(11,IOSTAT=JOS)      !Release the record
            READ(CLINE(1:5),'(I5)',IOSTAT=IOS) ICNT !Get top message vers num
            ILSTVER=IFSTVER+ICNT-1
            IFILE=ILSTVER              !Show latest version first
            LWAITING=.FALSE.
            ISTEP=0
            CS=' '
          ELSE IF((CHAR(IKEY).EQ.'P'.OR.CHAR(IKEY).EQ.'p')
     &        .AND.CD.NE.' ') THEN !Previous message file
            IFILE=IFILE-1
            LWAITING=.FALSE.
            ISTEP=0
            CS=' '
          ELSE IF((CHAR(IKEY).EQ.'N'.OR.CHAR(IKEY).EQ.'n')
     &        .AND.CE.NE.' ') THEN !Next message file
            IFILE=IFILE+1
            LWAITING=.FALSE.
            ISTEP=0
            CS=' '
          ELSE IF((CHAR(IKEY).EQ.'S'.OR.CHAR(IKEY).EQ.'s')
     &         .AND.CS.NE.' ') THEN !Next screen
            LWAITING=.FALSE.
          END IF
          IF(LWAITING) CALL LIB$WAIT(0.2) !Wait 0.2s before trying again
          ICNT=ICNT+1                  !Bump wait count
        END DO
        CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H')
      END DO
      CLOSE(11,IOSTAT=IOS)
      CALL VT_OUT(CHAR(0)//CHAR(27)//'[24;79H') !Put cursor at end
      RETURN
10    FORMAT(A)
      END
!*******************************************************************************
      SUBROUTINE SAT_SHOW_STAT(CMODE)
!
! Displays the hardware, GPS or miscellaneous status of the Capsat transceiver.
! The information is obtained from the SAT_DISP.DAT file which contains
! copies of the latest information for each of the three displays in three
! fixed length records.  H_SATCOM keeps this file up to date, but only updates
! it at a maximum of every 5s and only if a display process is wanting to 
! display the information.  Coordination with the H_SATCOM process is provided
! by the use of bits in the STATUS(47) word as follows:
!
! Bit 0  Set by this routine when hardware info is required from the capsat
!        Cleared by H_SATCOM each time it gets new hardware information
! Bit 1  Set by this routine when GPS info is required from the capsat
!        Cleared by H_SATCOM each time it gets new GPS information
! Bit 2  Set by this routine when misc info is required from the capsat
!        Cleared by H_SATCOM each time it gets new misc information
! Bit 3  Set by this routine when mess info is required from the capsat
!        Cleared by H_SATCOM each time it gets new misc information
! Bit 4  Negated by H_SATCOM each time it gets new hardware information
! Bit 5  Negated by H_SATCOM each time it gets new GPS information
! Bit 6  Negated by H_SATCOM each time it gets new misc information
! Bit 7  Negated by H_SATCOM each time it gets new mess information
!
! V1.00  09-03-95  W.D.N.JACKSON
!
      IMPLICIT  NONE
      INTEGER*4 IM,IOS,LSTS,ICNT,IKEY,I
      LOGICAL*4 LOOP
      CHARACTER CMODE*(*),CDISP(23)*80
      INCLUDE   'HCOM_DEF.FOR'

      IF(CMODE.EQ.'HARD') IM=0
      IF(CMODE.EQ.'GPS') IM=1
      IF(CMODE.EQ.'MISC') IM=2
      IF(CMODE.EQ.'MESS') IM=3
!
! Clear screen and set cursor to home position
!
      CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J')
      CALL VT_OUT(CHAR(0)//CHAR(27)//'[1;1H'//'Please wait...')
      CALL VT_OUT(CHAR(0)//CHAR(27)//'[24;1H'//'E - Exit')
!
! Open SAT_DISP.DAT file, tell H_SATCOM which type of data is required
!
      OPEN(UNIT=11,FILE='[HORACE.SATCOM]SAT_DISP.DAT;',STATUS='OLD',
     &    SHARED,ACCESS='DIRECT',RECL=23*80/4,IOSTAT=IOS) !Open the display file
      LSTS=BTEST(STATUS(47),IM+4)      !Current display info status
      STATUS(47)=IBSET(STATUS(47),IM)  !Tell H_SATCOM we want some info
!
! Loop waiting for either a key press, or for new status information to become
! available, or for a delay of more than 17s - indicating H_SATCOM is not
! responding for some reason.
!
      LOOP=.TRUE.                      !Main loop variable
      ICNT=0
      DO WHILE(LOOP)
        CALL CHECK_KEY('Ee',IKEY)
        IF(IKEY.NE.0) LOOP=.FALSE.     !Enable loop exit
!        IF(BTEST(STATUS(47),IM+4).NE.LSTS) THEN !New data
        if(icnt.eq.10) then
          READ(11,REC=IM+1,IOSTAT=IOS) CDISP
          UNLOCK(11,IOSTAT=IOS)        !Release the record
          CALL VT_OUT(CHAR(0)//CHAR(27)//'[1;1H')
          DO I=1,23
            CALL VT_OUT(CHAR(0)//CDISP(I)//CHAR(13)//CHAR(10))
          END DO
          CALL VT_OUT(CHAR(0)//'E - Exit')
          STATUS(47)=IBSET(STATUS(47),IM) !Tell H_SATCOM we still want info
          LSTS=BTEST(STATUS(47),IM+4)
          ICNT=0
        END IF
        IF(ICNT.EQ.85) THEN            !No response from H_SATCOM
          CALL VT_OUT(CHAR(0)//CHAR(27)
     &        //'[1;1H'//CHAR(27)//'[0K'//
     &        'No response from the H_SATCOM process.')
          ICNT=0
          STATUS(47)=IBSET(STATUS(47),IM) !Tell H_SATCOM we still want info
          LSTS=BTEST(STATUS(47),IM+4)  !Current display info status
        END IF
        IF(LOOP) CALL LIB$WAIT(0.2)    !Wait 0.2s before trying again
        ICNT=ICNT+1                    !Bump wait count
      END DO
!
! Close display file and return
!
      CLOSE(11,IOSTAT=IOS)
!
      CALL VT_OUT(CHAR(0)//CHAR(27)//'[24;79H') !Put cursor at end
      RETURN
10    FORMAT(A)
      END
!*******************************************************************************
      SUBROUTINE SAT_SEND_MESS(CMODE)
!
! Gets message type and destination details, together with the message itself
! from the flight leader, and passes it to the H_SATCOM process for onwards
! transmission via the Capsat transceiver.
!
! The message is passed to H_SATCOM via a hard disk file SAT_TEXT_MESS.TMP, and
! by the setting of bit 8 in STATUS(47).  H_SATCOM clears bit 8 when it starts
! and when it has picked up the new messages and deleted SAT_TEXT_MESS.TMP.
!
! This routine reads the file SAT_ADDR.DAT for destination codes in the
! range A to M and displays these as options to the flight leader, along with
! a few other standard destination options.  Note that destinations in the
! range A to M must match those used by the MRF base station, since H_SATCOM
! only sends the single character and does not hold any actual addresses or
! numbers.  Messages with destination codes N to Y have pre-defined meanings,
! and normally required explicit addresses to accompany them.  The handling
! of these codes by this routine is hard coded into the software and they
! should not be changed in the base station software without matching changes
! being made to this routine.
!
! CMODE can take the following values:
!
!   NORM - Send a new message
!   TEMP - Produce and send a coded temp message
!   SSTM - Produce and send an SST map
!   COPY - Send an existing message to a new destination
!
! Pre-defined destination codes currently handled by this routine:
!
! N Send to specified fax number
! O Send to specified CONDOR file name
!! P Send to specified MRFPST file name
! Q Send to specified Internet address
!! R Send to specified VMS mail address
! Z Emergency direct routing
!
! V1.00  09-03-95  W.D.N.JACKSON
! V1.01  07-11-95  W.D.N.JACKSON
!        Modified to handle purged status files
! V1.02  29-02-96  W.D.N.JACKSON
!        Modified to handle temp code files
! V1.03  11-09-96  W.D.N.JACKSON
!        Modified to use TPU editor, and handled copied files
!
      IMPLICIT  NONE
      INTEGER*4 IRETACT,LAN,IDEST,IOS,IOPT,JOS,ISTART,IEND,ICNT,
     -    INEWVER,IFSTVER,IS,ISIZ,I,ILEN
      LOGICAL*4 LMESS,LGET,LOK,LEXISTS
      CHARACTER CDCODE(23)*1,CDNAME(23)*40,CDDEFR(23)*20,CLINE*80
      CHARACTER COPT*2,CHLINE*80,COR*4,CTO*80,CFROM*80,CDATE*9,CTIME*8
      CHARACTER CDLINE*80,CCOR*50,CSLINE*80,CMODE*(*)
      INCLUDE   'HCOM_DEF.FOR'
!
! Clear screen, set cursor to home position
!
      CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H')
!
! Open SAT_ADDR.DAT file, read and store the destination options.
!
      OPEN(UNIT=11,FILE='[HORACE.SATCOM]SAT_ADDR.DAT;',STATUS='OLD')
      IDEST=0
      READ(11,10,IOSTAT=IOS) CLINE
      DO WHILE(IOS.EQ.0.AND.IDEST.LT.13)
        IF(CLINE(1:1).GE.'A'.AND.CLINE(1:1).LE.'M') THEN
          IDEST=IDEST+1
          CDCODE(IDEST)=CLINE(1:1)     !Destination code
          CDNAME(IDEST)=CLINE(3:39)    !Destination name
          CDDEFR(IDEST)=CLINE(40:59)   !Default recipient
        END IF
        READ(11,10,IOSTAT=IOS) CLINE
      END DO
      CLOSE(11,IOSTAT=IOS)
!
! Add the predefined options to the list
!
      CDCODE(IDEST+1)='O'
      CDNAME(IDEST+1)='Specified CONDOR file name'
      CDDEFR(IDEST+1)=' '
      CDCODE(IDEST+2)='Q'
      CDNAME(IDEST+2)='Specified Internet address'
      CDDEFR(IDEST+2)=' '
      CDCODE(IDEST+3)='Z'
      CDNAME(IDEST+3)='Emergency route bypassing Met Office'
      CDDEFR(IDEST+3)=' '
!      CDCODE(IDEST+1)='N'
!      CDNAME(IDEST+1)='Specified fax number'
!      CDDEFR(IDEST+1)=' '
!      CDCODE(IDEST+3)='P'
!      CDNAME(IDEST+3)='Specified MRFPST file name'
!      CDDEFR(IDEST+3)=' '
!      CDCODE(IDEST+5)='R'
!      CDNAME(IDEST+5)='Specified VMS mail address'
!      CDDEFR(IDEST+5)=' '
      IDEST=IDEST+3
!
! Display the options and get user requirement
!
      LMESS=.TRUE.
      DO WHILE(LMESS)
        CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J')
        CALL VT_OUT(CHAR(0)//CHAR(27)//'[1;1H')
        CALL VT_OUT('                             Message destinations')
        CALL VT_OUT(' ')
        CALL VT_OUT('  0) Exit this menu')
        DO I=1,IDEST
          WRITE(CLINE,11,IOSTAT=IOS) I,') '//CDNAME(I)
          CALL VT_OUT(CLINE(1:LAN(CLINE)))
        END DO
        CALL VT_OUT(' ')
        CALL VT_OUT('$Enter selection [0]: ')
        CALL VT_IN(COPT,ILEN,IOS)
        IF(COPT.EQ.' ') COPT='0'       !Use default
        IF(COPT(2:2).EQ.' ') THEN      !Right justify
          COPT(2:2)=COPT(1:1)
          COPT(1:1)=' '
        END IF
        READ(COPT,'(BN,I)',IOSTAT=JOS) IOPT !Convert string to number
        IRETACT=0                      !Don't want to finish yet
        IF(IOS.EQ.-1.OR.COPT.EQ.' 0') THEN !If CTRL/Z or 0
          IRETACT=1                    !Then finish
!
! Decode the required destination, get additional information when required
! and form the message header line
!
        ELSE IF(IOPT.GE.1.AND.IOPT.LE.IDEST.AND.JOS.EQ.0) THEN !Good option
!
! Codes A to M are sent as they are
!
          IF(CDCODE(IOPT).GE.'A'.AND.CDCODE(IOPT).LE.'M') THEN
            CHLINE=CDCODE(IOPT)        !Single destination code letter
            CDLINE=CDNAME(IOPT)(1:LAN(CDNAME(IOPT)))
!
! The following codes (N to R) require additional address information
!
          ELSE IF(CDCODE(IOPT).EQ.'N') THEN !Get required fax number
            CALL VT_OUT('$Fax number (eg 01252376588): ')
            CALL VT_IN(CLINE,ILEN,IOS)
            IF(IOS.EQ.-1) IRETACT=1
            CHLINE=CDCODE(IOPT)//' '//CLINE
            CDLINE=CDNAME(IOPT)(1:LAN(CDNAME(IOPT)))//' - '//
     &          CLINE(1:LAN(CLINE))
          ELSE IF(CDCODE(IOPT).EQ.'O') THEN !Get required file name
           CALL VT_OUT('$CONDOR file name (eg MRFA:[SATCOM]DATA.DAT): ')
            CALL VT_IN(CLINE,ILEN,IOS)
            IF(IOS.EQ.-1) IRETACT=1
            CHLINE=CDCODE(IOPT)//' '//CLINE
            CDLINE=CDNAME(IOPT)(1:LAN(CDNAME(IOPT)))//' - '//
     &          CLINE(1:LAN(CLINE))
!          ELSE IF(CDCODE(IOPT).EQ.'P') THEN !Get required file name
!            CALL VT_OUT('$MRFPST file name (eg C:\MESSAGES\DATA.DAT): ')
!            CALL VT_IN(CLINE,ILEN,IOS)
!            IF(IOS.EQ.-1) IRETACT=1
!            CHLINE=CDCODE(IOPT)//' '//CLINE
!            CDLINE=CDNAME(IOPT)(1:LAN(CDNAME(IOPT)))//' - '//
!     &          CLINE(1:LAN(CLINE))
          ELSE IF(CDCODE(IOPT).EQ.'Q') THEN !Get internet address
            CALL VT_OUT
     &          ('$Internet address (eg jo.bloggs@metoffice.com): ')
            CALL VT_IN(CLINE,ILEN,IOS)
            IF(IOS.EQ.-1) IRETACT=1
            CHLINE=CDCODE(IOPT)//' '//CLINE !Get VMS address
            CDLINE=CDNAME(IOPT)(1:LAN(CDNAME(IOPT)))//' - '//
     &          CLINE(1:LAN(CLINE))
!          ELSE IF(CDCODE(IOPT).EQ.'R') THEN
!            CALL VT_OUT('$VMS mail address (eg BLOGGS): ')
!            CALL VT_IN(CLINE,ILEN,IOS)
!            IF(IOS.EQ.-1) IRETACT=1
!            CHLINE=CDCODE(IOPT)//' '//CLINE
!            CDLINE=CDNAME(IOPT)(1:LAN(CDNAME(IOPT)))//' - '//
!     &          CLINE(1:LAN(CLINE))
!
! Code Z has its own menu which specifies the option further and gets
! additional information.  Note that the telex, X.25, X.400 and satcom options
! have been commented out because they have not been properly tested.
!
          ELSE IF(CDCODE(IOPT).EQ.'Z') THEN !Direct routing bypassing base
            LGET=.TRUE.
            DO WHILE(LGET)
              CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J')
              CALL VT_OUT(CHAR(0)//CHAR(27)//'[24;1H'//
     &            'CTRL/Z to exit this menu')
              CALL VT_OUT(CHAR(0)//CHAR(27)//'[1;1H')
              CALL VT_OUT('                       Emergency routing')
              CALL VT_OUT(' ')
              CALL VT_OUT(' 0) Return to Destination menu')
              CALL VT_OUT(' 1) Direct to fax')
              CALL VT_OUT(' 2) Direct to Satcom mobile')
              CALL VT_OUT(' 3) Direct to email')
!              CALL VT_OUT(' 4) Direct to telex')
!              CALL VT_OUT(' 5) Direct to X.25')
!              CALL VT_OUT(' 6) Direct to X.400')
!              CALL VT_OUT(' 7) Direct to PSTN modem')
              CALL VT_OUT(' ')
              CALL VT_OUT('$Enter selection [1]: ')
              CALL VT_IN(COPT(1:1),ILEN,IOS)
              IF(COPT(1:1).EQ.'0') IRETACT=-1
              IF(IOS.EQ.-1) IRETACT=1
              IF(COPT(1:1).EQ.' ') COPT(1:1)='1'
              IF(COPT(1:1).GE.'0'.AND.COPT(1:1).LE.'4') LGET=.FALSE.
            END DO
            CALL VT_OUT(' ')
            IF(IRETACT.NE.0) THEN
            ELSE IF(COPT(1:1).EQ.'1') THEN
              CLINE=' '
              IOS=0
              DO WHILE(CLINE.EQ.' '.AND.IOS.NE.-1)
                CALL VT_OUT
     &              ('$Fax no with intl code (eg 441252376588): ')
                CALL VT_IN(CLINE,ILEN,IOS)
              END DO
              IF(IOS.EQ.-1) IRETACT=1
              CHLINE='ZF '//CLINE
              CDLINE='Emergency routing direct to fax - '//
     &          CLINE(1:LAN(CLINE))
!            ELSE IF(COPT(1:1).EQ.'7') THEN
!              CALL VT_OUT( 
!     &            '$PSTN no with intl code (eg 441252395778): ')
!              CALL VT_IN(CLINE,ILEN,IOS)
!              IF(IOS.EQ.-1) IRETACT=1
!              CHLINE='ZP '//CLINE
!              CDLINE='Emergency routing direct to PSTN modem - '//
!     &          CLINE(1:LAN(CLINE))
            ELSE IF(COPT(1:1).EQ.'2') THEN
              COR=' '
              DO WHILE(COR.EQ.' '.AND.IRETACT.EQ.0)
                CALL VT_OUT( '$Mobile ocean region '//
     &              '(EAOR, WAOR, IOR, or POR) [EAOR]: ')
                CALL VT_IN(CLINE,ILEN,IOS)
                IF(IOS.EQ.-1) IRETACT=1
                IF(CLINE.EQ.' ') CLINE='EAOR'
                IF(CLINE.EQ.'EAOR') COR='1111'
                IF(CLINE.EQ.'WAOR') COR='1114'
                IF(CLINE.EQ.'IOR') COR='1113'
                IF(CLINE.EQ.'POR') COR='1112'
              END DO
              CCOR=CLINE(1:3)
              IF(IRETACT.EQ.0) THEN
                CALL VT_OUT('$Mobile number [492340941]: ')
                CALL VT_IN(CLINE,ILEN,IOS)
                IF(IOS.EQ.-1) IRETACT=1
                IF(CLINE.EQ.' ') CLINE='492340941'
              END IF
              CHLINE='ZM '//COR//CLINE
              CCOR=CCOR(1:3)//' '//CLINE(1:LAN(CLINE))
              CDLINE='Emergency routing direct to Satcom mobile - '//
     &            CCOR(1:LAN(CCOR))
            ELSE IF(COPT(1:1).EQ.'3') THEN
              CALL VT_OUT
     &            ('$Internet address (eg jo.bloggs@metoffice.com): ')
              CALL VT_IN(CLINE,ILEN,IOS)
              IF(IOS.EQ.-1) IRETACT=1
              CHLINE='ZE '//CLINE
              CDLINE='Emergency routing direct to email - '//
     &            CLINE(1:LAN(CLINE))
!            ELSE IF(COPT(1:1).EQ.'4') THEN
!              CALL VT_OUT('$Telex number (eg 44 22289): ')
!              CALL VT_IN(CLINE,ILEN,IOS)
!              IF(IOS.EQ.-1) IRETACT=1
!              CHLINE='ZT '//CLINE
!              CDLINE='Emergency routing direct to telex - '//
!     &          CLINE(1:LAN(CLINE))
!            ELSE IF(COPT(1:1).EQ.'5') THEN
!              CALL VT_OUT('$X.25 address (eg ???? ?????): ')
!              CALL VT_IN(CLINE,ILEN,IOS)
!              IF(IOS.EQ.-1) IRETACT=1
!              CHLINE='ZX '//CLINE
!              CDLINE='Emergency routing direct to X.25 - '//
!     &          CLINE(1:LAN(CLINE))
!            ELSE IF(COPT(1:1).EQ.'6') THEN
!              CALL VT_OUT( 
!     &            '$X.400 address information must go in message. '//
!     &            'OK? [Y]: ')
!              CALL VT_IN(CLINE,ILEN,IOS)
!              IF(IOS.EQ.-1) IRETACT=1
!              CHLINE='Z4 '
!              CDLINE='Emergency routing direct to X.400 '
            END IF
          END IF
!
! We now have the header line.  Next get the message From and message To
! information - this goes in the message and is not essential for message
! routing, but may be useful when the message gets to the other end.
!
          IF(IRETACT.EQ.0) THEN
            IF(CMODE.EQ.'NORM') THEN
              CALL DATE1(CDATE)
              CALL TIME(CTIME)
              IF(CDATE(4:6).EQ.'JAN') CDATE(4:5)='01'
              IF(CDATE(4:6).EQ.'FEB') CDATE(4:5)='02'
              IF(CDATE(4:6).EQ.'MAR') CDATE(4:5)='03'
              IF(CDATE(4:6).EQ.'APR') CDATE(4:5)='04'
              IF(CDATE(4:6).EQ.'MAY') CDATE(4:5)='05'
              IF(CDATE(4:6).EQ.'JUN') CDATE(4:5)='06'
              IF(CDATE(4:6).EQ.'JUL') CDATE(4:5)='07'
              IF(CDATE(4:6).EQ.'AUG') CDATE(4:5)='08'
              IF(CDATE(4:6).EQ.'SEP') CDATE(4:5)='09'
              IF(CDATE(4:6).EQ.'OCT') CDATE(4:5)='10'
              IF(CDATE(4:6).EQ.'NOV') CDATE(4:5)='11'
              IF(CDATE(4:6).EQ.'DEC') CDATE(4:5)='12'
              IF(CDATE(1:1).EQ.' ') CDATE(1:1)='0'
              CFROM='G-LUXE'
              CTO=' '
              IF(CDCODE(IOPT).LE.'M') CTO=CDDEFR(IOPT)
              CALL VT_OUT('$Message from ['//
     &            CFROM(1:LAN(CFROM))//']: ')
              CALL VT_IN(CLINE,ILEN,IOS)
              IF(IOS.EQ.-1) IRETACT=1
              IF(CLINE.EQ.' ') CLINE=CFROM
              CFROM='From: '//CLINE(1:LAN(CLINE))//' '//
     &            CDATE(1:2)//'/'//CDATE(4:5)//'/'//
     &            CDATE(8:9)//' '//CTIME(1:2)//CTIME(4:5)//'Z'
              IF(IRETACT.EQ.0) THEN
                IF(CTO.NE.' ') THEN
                  CALL VT_OUT('$Message to   ['//
     &                CTO(1:LAN(CTO))//']: ')
                ELSE
                  CALL VT_OUT('$Message to:  ')
                END IF
                CALL VT_IN(CLINE,ILEN,IOS)
                IF(IOS.EQ.-1) IRETACT=1
                IF(CLINE.EQ.' ') CLINE=CTO
                CTO='To:   '//CLINE
                OPEN(13,FILE='SAT_SEND.TMP',STATUS='NEW',
     &              IOSTAT=IOS,CARRIAGECONTROL='LIST')
                IF(CFROM.NE.' ') WRITE(13,'(A)',IOSTAT=IOS) 
     &              CFROM(1:LAN(CFROM))
                IF(CTO.NE.' ') WRITE(13,'(A)',IOSTAT=IOS) 
     &              CTO(1:LAN(CTO))
                WRITE(13,'(A)',IOSTAT=IOS) ' '
                CLOSE(13,IOSTAT=IOS)
              END IF
            ELSE IF(CMODE.EQ.'TEMP') THEN
              CALL VT_OUT( 
     &            '$Temp message start time (hh:mm:ss): ')
              CALL VT_IN(CLINE,ILEN,IOS)
              CALL CTIMSPM(CLINE(1:8),ISTART)
              IF(IOS.EQ.-1) IRETACT=1
              IF(IRETACT.EQ.0) THEN
                CALL VT_OUT( 
     &              '$Temp message end time (hh:mm:ss): ')
                CALL VT_IN(CLINE,ILEN,IOS)
                CALL CTIMSPM(CLINE(1:8),IEND)
                IF(IOS.EQ.-1) IRETACT=1
              END IF
              IF(IRETACT.EQ.0)
     &            CALL SAT_TEMP(ISTART,IEND,'SAT_SEND.TMP;')
            ELSE IF(CMODE.EQ.'SSTM') THEN
              CALL SAT_SSTMAP('SAT_SEND.TMP;')
            END IF
!
! If it is new message then invoke the TPU editor to enter it.
!
            IF(IRETACT.EQ.0) THEN
              IF(CMODE.EQ.'NORM') CALL VT_EDIT('[HORACE]SAT_SEND.TMP')
!              IF(CMODE.EQ.'NORM') CALL LIB$SPAWN
!     &            ('@H_EDIT_TEMP.COM TTT',,,2+4+8,,,,,,,,)
!
! Now display the complete message, including the forwarding line, and check
! whether the user really wants to send it.
!
              LOK=.FALSE.
              DO WHILE(.NOT.LOK)
                CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//
     &              CHAR(27)//'[1;1H')
                CALL VT_OUT(CHAR(0)//'Dest: '//
     &              CDLINE(1:LAN(CDLINE)))
                CALL VT_OUT(' ')
                OPEN(11,FILE='SAT_SEND.TMP;',STATUS='OLD',READONLY)
                READ(11,'(A)',IOSTAT=IOS) CLINE
                DO WHILE(IOS.EQ.0)
                  CALL VT_OUT(' '//CLINE(1:LAN(CLINE)))
                  READ(11,'(A)',IOSTAT=IOS) CLINE
                END DO
                CLOSE(11,IOSTAT=IOS)
                CALL VT_OUT(' ')
                CALL VT_OUT(
     &              '$Send this message (Yes,No,Edit)? [Y]: ')
                CALL VT_IN(CLINE,ILEN,IOS)
                CALL STR$UPCASE(CLINE,CLINE)
                IF(CLINE.EQ.' ') CLINE='Y'
                IF(CLINE.EQ.'Y'.OR.CLINE.EQ.'N') LOK=.TRUE.           
                IF(CLINE.EQ.'E') CALL VT_EDIT('[HORACE]SAT_SEND.TMP')
!                IF(CLINE.EQ.'E') 
!     &            CALL LIB$SPAWN('@H_EDIT_TEMP.COM TTT',,,2+4+8,
!     &                ,,,,,,,)
              END DO
!
! If it is not to be sent then delete the temporary file.
!
              IF(CLINE.EQ.'N') THEN
                OPEN(13,FILE='SAT_SEND.TMP;',STATUS='OLD',IOSTAT=IOS)
                CLOSE(13,IOSTAT=IOS,DISPOSE='DELETE')
              END IF
!
! If happy with message then write it to a file, and signal to
! H_SATCOM that there is a new message to be sent.
!
              IF(CLINE.EQ.'Y'.AND.IOS.EQ.0) THEN
                INQUIRE(FILE='[HORACE.SATCOM]SAT_OUT_STS.DAT',
     &              EXIST=LEXISTS)
                IF(.NOT.LEXISTS) THEN
                  OPEN(UNIT=11,FILE='[HORACE.SATCOM]SAT_OUT_STS.DAT',
     &                STATUS='NEW',SHARED,ACCESS='DIRECT',RECL=20,
     &                IOSTAT=IOS)      !Open the status file
                  CLINE='00000 00000'
                  WRITE(11,REC=1,IOSTAT=IOS) CLINE
                  CLOSE(11,IOSTAT=IOS)
                END IF
                OPEN(UNIT=11,FILE='[HORACE.SATCOM]SAT_OUT_STS.DAT',
     &              STATUS='OLD',SHARED,ACCESS='DIRECT',RECL=20,
     &              IOSTAT=IOS)        !Open the status file
                READ(11,REC=1,IOSTAT=IOS) CLINE  !Read 1st line for no of mess's
                READ(CLINE(1:5),'(I5)',IOSTAT=IOS) ICNT !Get mess vers num
                ICNT=ICNT+1
                WRITE(CLINE(1:5),'(I5.5)',IOSTAT=IOS) ICNT
                IF(CLINE(7:11).EQ.'00000'.OR.ICNT.EQ.1)
     &              CLINE(7:11)=CLINE(1:5)
                WRITE(11,REC=1,IOSTAT=IOS) CLINE
                IF(ICNT.EQ.1) THEN
                  INEWVER=1
                ELSE
                  READ(11,REC=2,IOSTAT=IOS) CLINE
                  READ(CLINE(1:5),'(I5)',IOSTAT=IOS) IFSTVER !Get first mess no
                  INEWVER=IFSTVER+ICNT-1
                END IF
                UNLOCK(11,IOSTAT=IOS)
                WRITE(CLINE(1:5),'(I5.5)',IOSTAT=IOS) INEWVER
                OPEN(UNIT=12,FILE='[HORACE.SATCOM]SAT_OUT_'//CLINE(1:5)
     &              //'.DAT',
     &              STATUS='NEW',IOSTAT=IOS,CARRIAGECONTROL='LIST')
                IS=LAN(CHLINE)
                WRITE(12,10,IOSTAT=IOS) CHLINE(1:IS)
                ISIZ=IS
                OPEN(13,FILE='SAT_SEND.TMP;',STATUS='OLD',IOSTAT=IOS)
                READ(13,'(A)',IOSTAT=IOS) CSLINE
                DO WHILE(IOS.EQ.0)
                  IS=LAN(CSLINE)
                  WRITE(12,10,IOSTAT=IOS) CSLINE(1:IS)
                  ISIZ=ISIZ+IS
                  READ(13,'(A)',IOSTAT=IOS) CSLINE
                END DO
                CLOSE(13,IOSTAT=IOS,DISPOSE='DELETE')
                CLOSE(12,IOSTAT=IOS)
!
! Form status line and write it out to SAT_OUT_STS.DAT.
!
                CSLINE=CLINE(1:5)
                CALL DATE1(CSLINE(8:16))
                IF(CSLINE(8:8).EQ.' ') CSLINE(8:8)='0'
                CALL TIME(CSLINE(19:26))
                WRITE(CSLINE(29:32),'(I4)',IOSTAT=IOS) ISIZ
                IF(CHLINE(1:1).GE.'A'.AND.CHLINE(1:1).LE.'M')
     &              CSLINE(35:63)=CDLINE
                IF(CHLINE(1:1).EQ.'N')
     &              CSLINE(35:63)='Fax - '//CHLINE(3:)
                IF(CHLINE(1:1).EQ.'O')
     &              CSLINE(35:63)='File - '//CHLINE(3:)
                IF(CHLINE(1:1).EQ.'P')
     &              CSLINE(35:63)='MRFPST - '//CHLINE(3:)
                IF(CHLINE(1:1).EQ.'Q')
     &              CSLINE(35:63)='Mail - '//CHLINE(3:)
                IF(CHLINE(1:1).EQ.'R')
     &              CSLINE(35:63)='Mail - '//CHLINE(3:)
                IF(CHLINE(1:2).EQ.'ZE')
     &              CSLINE(35:63)='Direct mail - '//CHLINE(4:)
                IF(CHLINE(1:2).EQ.'ZF')
     &              CSLINE(35:63)='Direct fax - '//CHLINE(4:)
                IF(CHLINE(1:2).EQ.'ZP')
     &              CSLINE(35:63)='Direct PSTN - '//CHLINE(4:)
                IF(CHLINE(1:2).EQ.'ZM') CSLINE(35:63)=
     &              'Direct mobile - '//CCOR(1:LAN(CCOR))
                CSLINE(66:77)='Queued'
                CSLINE(79:80)='FL'
                WRITE(11,REC=ICNT+1,IOSTAT=IOS) CSLINE
                CLOSE(11,IOSTAT=IOS)
                STATUS(47)=IBSET(STATUS(47),8) !Tell H_SATCOM
                STATUS(47)=STATUS(47).XOR.'400'X !Negate bit 10 for H_DISPLAY
              END IF
              IRETACT=1              
            END IF
          END IF
        END IF
        IF(IRETACT.GT.0) LMESS=.FALSE. !Move back to Satcom menu
      END DO
      RETURN
10    FORMAT(A)
11    FORMAT(X,I2,A)
      END
!*******************************************************************************
      SUBROUTINE SAT_CHNG_MESS
!
! Allows the flight leader to change the status of a message, either by
! resubmitting any message that was previously entered on the transmit queue -
! including sonde messages, turnaround and file transfer messages - or by 
! stopping a message that is currently queued on the transmit queue - doing
! this will also abort any current transmission of the message if this is 
! possible.
!
! V1.00  09-03-95  W.D.N.JACKSON
! V1.01  07-11-95  W.D.N.JACKSON
!        Modified to handle purged status files
! V1.02  07-11-95  W.D.N.JACKSON
!        Modified to handle sending a message to a different user
!
      IMPLICIT  NONE
      INTEGER*4 IRETACT,LAN,IL,JOS,IOPT,IOS,IMESNO,IREC,IFSTVER,
     &    IFSTQUE,ILEN
      LOGICAL*4 LCHANGE,LGDMES
      CHARACTER CLINE*80,COPT*1,CHLINE*80,CSLINE*80,CMESNO*5,CSTAT*12
      CHARACTER CLINE2*80
      INCLUDE   'HCOM_DEF.FOR'
!
! Clear screen, set cursor to home position
!
      CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H')
!
! Display the options and get user requirement
!
      LCHANGE=.TRUE.
      DO WHILE(LCHANGE)
        CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J')
        CALL VT_OUT(CHAR(0)//CHAR(27)//'[1;1H')
        CALL VT_OUT(
     &      '                            Message control options')
        CALL VT_OUT(' ')
        CALL VT_OUT('  0) Exit this menu')
        CALL VT_OUT('  1) Stop a message')
        CALL VT_OUT('  2) Resubmit a message')
        CALL VT_OUT('  3) Copy a message')
        CALL VT_OUT(' ')
        CALL VT_OUT('$Enter selection [0]: ')
        CALL VT_IN(COPT,IL,IOS)
        IL=LAN(COPT)
        IF(COPT.EQ.' ') COPT='0'       !Use default
        READ(COPT,'(BN,I)',IOSTAT=JOS) IOPT !Convert string to number
        IRETACT=0                      !Don't want to finish yet
        IF(IOS.EQ.-1.OR.COPT.EQ.'0') THEN !If CTRL/Z or 0
          IRETACT=1                    !Then finish
        ELSE IF(IOPT.GE.1.AND.IOPT.LE.3.AND.JOS.EQ.0.AND.IL.LE.1) THEN !OK optn
!
! Get message number
!
          LGDMES=.FALSE.
          DO WHILE(.NOT.LGDMES.AND.IRETACT.EQ.0)
            LGDMES=.TRUE.
            IF(IOPT.EQ.1) CALL VT_OUT('$Stop message number: ')
            IF(IOPT.EQ.2) CALL VT_OUT('$Resubmit message number: ')
            IF(IOPT.EQ.3) CALL VT_OUT('$Copy message number: ')
            CALL VT_IN(CLINE,ILEN,IOS)
            IF(IOS.EQ.-1) IRETACT=1
            READ(CLINE,'(BN,I)',IOSTAT=IOS) IMESNO !Convert string to number
            WRITE(CMESNO,'(I5.5)',IOSTAT=IOS) IMESNO
            IF(CLINE.EQ.' ') LGDMES=.FALSE.
            IF(IMESNO.EQ.0) LGDMES=.FALSE.
            OPEN(UNIT=11,FILE='[HORACE.SATCOM]SAT_OUT_STS.DAT',
     &          STATUS='OLD',SHARED,
     &          ACCESS='DIRECT',RECL=20,IOSTAT=IOS) !Open the status file
            IF(IOS.NE.0) LGDMES=.FALSE.
            READ(11,REC=1,IOSTAT=IOS) CHLINE !Read 1st line to get no of mess's
            IF(IOS.NE.0) LGDMES=.FALSE.
            READ(11,REC=2,IOSTAT=IOS) CLINE2 !Read the second line
            IF(IOS.NE.0) LGDMES=.FALSE.
            READ(CLINE2(1:5),'(I5)',IOSTAT=IOS) IFSTVER
            IF(IOS.NE.0) LGDMES=.FALSE.
            IREC=IMESNO-IFSTVER+1
            READ(11,REC=IREC+1,IOSTAT=IOS) CSLINE !Read status line
            IF(IOS.NE.0) LGDMES=.FALSE.
            CSTAT=CSLINE(66:77)        !Get message status
            UNLOCK(11,IOSTAT=IOS)
            OPEN(UNIT=12,FILE='[HORACE.SATCOM]SAT_OUT_'//CMESNO//'.DAT',
     &          STATUS='OLD',IOSTAT=IOS,CARRIAGECONTROL='LIST',READONLY)
            IF(IOS.NE.0) LGDMES=.FALSE.
            IF(.NOT.LGDMES.AND.IRETACT.EQ.0) THEN
              CALL VT_OUT(' Unable to find message '//CMESNO)
              IRETACT=2
            ELSE IF(LGDMES.AND.IRETACT.EQ.0) THEN
              IF(IOPT.EQ.1) THEN
                IF(CSTAT.NE.'Issued'.AND.CSTAT.NE.'Submitted'.AND.
     &              CSTAT.NE.'Queued'.AND.IRETACT.EQ.0) THEN
                  CALL VT_OUT(' Can''t stop a message '//
     &                'with "'//CSTAT(1:LAN(CSTAT))//'" status')
                  IRETACT=2
                END IF                
              ELSE IF(IOPT.EQ.2) THEN
                IF((CSTAT.EQ.'Issued'.OR.CSTAT.EQ.'Submitted'.OR.
     &              CSTAT.EQ.'Queued').AND.IRETACT.EQ.0) THEN
                  CALL VT_OUT('Can''t resubmit a message '//
     &                'with "'//CSTAT(1:LAN(CSTAT))//'" status')
                  IRETACT=2
                END IF
              END IF
            END IF
            IF(IRETACT.GE.1) THEN
              IF(IRETACT.EQ.2) THEN
                CALL VT_OUT(' ')
                CALL VT_OUT('$Press any key to continue')
                CALL VT_IN(CLINE,ILEN,IOS)
                IRETACT=1
              END IF
              CLOSE(11,IOSTAT=IOS)
              CLOSE(12,IOSTAT=IOS)
            END IF
          END DO
!
! Now display the complete message, including the forwarding line, and check
! whether the user really wants to change it.
!
          IF(IRETACT.EQ.0) THEN
            CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H')
            CALL VT_OUT(' '//CSLINE(1:LAN(CSLINE)))
            CALL VT_OUT(' ')
            READ(12,10,IOSTAT=IOS) CLINE
            IF(IOPT.EQ.3) READ(12,10,IOSTAT=IOS) CLINE !Skip destination line
            DO WHILE(IOS.EQ.0)
              CALL VT_OUT(' '//CLINE(1:LAN(CLINE)))
              READ(12,10,IOSTAT=IOS) CLINE
            END DO
            CALL VT_OUT(' ')
            IF(IOPT.EQ.1) THEN
              CALL VT_OUT('$Stop this message? [Y]: ')
            ELSE IF(IOPT.EQ.2) THEN
              CALL VT_OUT('$Resubmit this message? [Y]: ')
            ELSE IF(IOPT.EQ.3) THEN
              CALL VT_OUT('$Copy this message? [Y]: ')
            END IF
            CALL VT_IN(CLINE,ILEN,IOS)
            CALL STR$UPCASE(CLINE,CLINE)
            IF(CLINE.EQ.' ') CLINE='Y'
!
! If happy then change message status and tell H_SATCOM, or invoke copy.
!
            IF(CLINE.EQ.'Y'.AND.IOS.EQ.0) THEN
              IF(IOPT.EQ.1) THEN
                CSLINE(66:77)='Stopped'
                WRITE(11,REC=IREC+1,IOSTAT=IOS) CSLINE
                UNLOCK(11,IOSTAT=IOS)
                STATUS(47)=IBSET(STATUS(47),8) !Tell H_SATCOM
                STATUS(47)=STATUS(47).XOR.'400'X !Negate bit 10 for H_DISPLAY
              ELSE IF(IOPT.EQ.2) THEN
                CSLINE(66:77)='Queued'
                WRITE(11,REC=IREC+1,IOSTAT=IOS) CSLINE
                READ(11,REC=1,IOSTAT=IOS) CHLINE
                READ(CHLINE(7:11),'(I)',IOSTAT=IOS) IFSTQUE
                IF(IFSTQUE.EQ.0.OR.IREC.LT.IFSTQUE) THEN
                  WRITE(CHLINE(7:11),'(I5.5)',IOSTAT=IOS) IREC
                  WRITE(11,REC=1,IOSTAT=IOS) CHLINE
                END IF
                UNLOCK(11,IOSTAT=IOS)
                STATUS(47)=IBSET(STATUS(47),8) !Tell H_SATCOM
                STATUS(47)=STATUS(47).XOR.'400'X !Negate bit 10 for H_DISPLAY
              ELSE IF(IOPT.EQ.3) THEN
                OPEN(13,FILE='SAT_SEND.TMP',STATUS='NEW',
     &              IOSTAT=IOS,CARRIAGECONTROL='LIST')
                CLOSE(12,IOSTAT=IOS)
                OPEN(UNIT=12,FILE='[HORACE.SATCOM]SAT_OUT_'//CMESNO//
     &              '.DAT',
     &              STATUS='OLD',IOSTAT=IOS,CARRIAGECONTROL='LIST',
     &              READONLY)
                READ(12,10,IOSTAT=IOS) CLINE !Skip destination line
                READ(12,10,IOSTAT=IOS) CLINE
                DO WHILE(IOS.EQ.0)
                  WRITE(13,10,IOSTAT=IOS) CLINE(1:LAN(CLINE))
                  READ(12,10,IOSTAT=IOS) CLINE
                END DO
                CLOSE(13,IOSTAT=IOS)
                CALL SAT_SEND_MESS(IRETACT,'COPY')
!                CALL INPUT_ABORT
              END IF
            END IF
            IRETACT=1              
          END IF
          CLOSE(11,IOSTAT=IOS)
          CLOSE(12,IOSTAT=IOS)
        END IF
        IF(IRETACT.GT.0) LCHANGE=.FALSE. !Move back to Satcom menu
      END DO
      CALL VT_OUT(CHAR(0)//CHAR(27)//'[24;79H') !Put cursor at end
      RETURN
10    FORMAT(A)
11    FORMAT(X,I2,A)
      END
!*******************************************************************************
      SUBROUTINE SAT_SPEC_OPTI
!
! Provides some special options for the control of the Satcom system.  This
! option is not advertised and is for use while experimenting with the system.
!
! Coordination with the H_SATCOM process is provided by the use of bits in the
! STATUS(47) word as follows:
!
! Bit 11 Set to disable position reporting
! Bit 12 Set for conditional ocean region forcing
! Bit 13 Set for unconditional ocean region forcing
! Bit 14,15 0 to 3 to force WAOR,EAOR,POR,IOR respectively
!
! V1.00  09-03-95  W.D.N.JACKSON
!
      IMPLICIT  NONE
      INTEGER*4 IOCEAN,IOPT,IS,IOS,IOPT1,ISTATUS,IL,ILEN
      LOGICAL*4 LNOCEAN
      CHARACTER CPSTAT*8,CUSTAT*8,CCSTAT*8,COCEAN*13,COPT1*1
      CHARACTER CLINE*80,COPT*4
      INCLUDE   'HCOM_DEF.FOR'
      EQUIVALENCE (STATUS(47),ISTATUS)
!
! Clear screen, set cursor to home position
!
      CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H')
!
! Display the options and get user requirement
!
      DO WHILE(.TRUE.)
        CCSTAT='Disabled'
        CUSTAT='Disabled'
        CPSTAT='Enabled'
        IF(BTEST(STATUS(47),11)) CPSTAT='Disabled'
        IF(BTEST(STATUS(47),12)) CCSTAT='Enabled'
        IF(BTEST(STATUS(47),13)) CUSTAT='Enabled'
        IOCEAN=ISHFT(STATUS(47),-14)
        IF(IOCEAN.EQ.0) COCEAN='West Atlantic'
        IF(IOCEAN.EQ.1) COCEAN='East Atlantic'
        IF(IOCEAN.EQ.2) COCEAN='Pacific'
        IF(IOCEAN.EQ.3) COCEAN='Indian'
        CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J')
        CALL VT_OUT(CHAR(0)//CHAR(27)//'[1;1H')
        CALL VT_OUT(
     &      '                            Special control options')
        CALL VT_OUT(' ')
        CALL VT_OUT('  Conditional ocean forcing:   '//CCSTAT)
        CALL VT_OUT('  Unconditional ocean forcing: '//CUSTAT)
        CALL VT_OUT('  Forced ocean:                '//COCEAN)
        CALL VT_OUT('  Position reporting:          '//CPSTAT)
        CALL VT_OUT(' ')
        CALL VT_OUT('  0) Exit this menu')
        CALL VT_OUT('  1) Enable conditional ocean forcing')
        CALL VT_OUT('  2) Enable unconditional ocean forcing')
        CALL VT_OUT('  3) Disable ocean forcing')
        CALL VT_OUT('  4) Disable position reporting')
        CALL VT_OUT('  5) Enable position reporting')
        CALL VT_OUT('  6) List address book')
        CALL VT_OUT('  7) Zero received message counter')
        CALL VT_OUT('  8) Zero transmitted message counter')
        CALL VT_OUT('  9) Set Satcom GPS from XR5 GPS')
        CALL VT_OUT(' ')
        CALL VT_OUT('$Enter selection [0]: ')
        CALL VT_IN(COPT,ILEN,IS)
        IF(IS.EQ.-1.OR.COPT.EQ.' '.OR.IL.EQ.0) RETURN
        READ(COPT,'(BN,I)',IOSTAT=IS) IOPT !Get response
        IF(IS.NE.0.OR.IOPT.EQ.0) RETURN
        IF(IOPT.EQ.1.OR.IOPT.EQ.2) THEN !Get ocean region
          LNOCEAN=.TRUE.
          DO WHILE(LNOCEAN)
            CALL VT_OUT(' ')
            CALL VT_OUT('  0) West Atlantic')
            CALL VT_OUT('  1) East Atlantic')
            CALL VT_OUT('  2) Pacific')
            CALL VT_OUT('  3) Indian')
            CALL VT_OUT(' ')
            CALL VT_OUT('$Enter selection [1]: ')
            CALL VT_IN(COPT1,IL,IOS)
            IF(IOS.EQ.-1) RETURN
            IF(IL.EQ.0) COPT1='1'
            READ(COPT1,'(BN,I)',IOSTAT=IS) IOPT1 !Get response
            IF(IS.NE.0) RETURN
            IF(IOPT1.GE.0.AND.IOPT1.LE.3) THEN
              CALL MVBITS(IOPT1,0,2,ISTATUS,14)
              LNOCEAN=.FALSE.
            END IF
          END DO
        END IF
!
        IF(IOPT.EQ.1) THEN
          STATUS(47)=IBSET(STATUS(47),12) !Enable conditional ocean forcing
          STATUS(47)=IBCLR(STATUS(47),13) !Disable unconditional ocean forcing
        ELSE IF(IOPT.EQ.2) THEN
          STATUS(47)=IBSET(STATUS(47),13) !Enable unconditional ocean forcing
          STATUS(47)=IBCLR(STATUS(47),12) !Disable conditional ocean forcing
        ELSE IF(IOPT.EQ.3) THEN
          STATUS(47)=IBCLR(STATUS(47),13) !Disable unconditional ocean forcing
          STATUS(47)=IBCLR(STATUS(47),12) !Disable conditional ocean forcing
        ELSE IF(IOPT.EQ.4) THEN
          STATUS(47)=IBSET(STATUS(47),11) !Disable position reporting
        ELSE IF(IOPT.EQ.5) THEN
          STATUS(47)=IBCLR(STATUS(47),11) !Enable position reporting
        ELSE IF(IOPT.EQ.6) THEN
          CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J')
          CALL VT_OUT(CHAR(0)//CHAR(27)//'[1;1H'//
     &        'Contents of [HORACE.SATCOM]SAT_ADDR.DAT')
          CALL VT_OUT(' ')
          OPEN(UNIT=11,FILE='[HORACE.SATCOM]SAT_ADDR.DAT;',STATUS='OLD')
          READ(11,'(Q,A)',IOSTAT=IOS) IL,CLINE
          DO WHILE(IOS.EQ.0)
            IF(IL.GT.0.AND.CLINE(1:1).NE.'!') 
     &          CALL VT_OUT(' '//CLINE(1:IL))
            READ(11,'(Q,A)',IOSTAT=IOS) IL,CLINE
          END DO
          CLOSE(11,IOSTAT=IOS)
          CALL VT_OUT(' ')
          CALL VT_OUT('$Press Enter to continue...')
          CALL VT_IN(COPT1,ILEN,IS)
        ELSE IF(IOPT.EQ.7) THEN
          CLINE='00000'
          OPEN(UNIT=11,FILE='[HORACE.SATCOM]SAT_INP_STS.DAT',
     &        STATUS='NEW',SHARED,ACCESS='DIRECT',RECL=20,IOSTAT=IOS)
          WRITE(11,REC=1,IOSTAT=IOS) CLINE
          CLOSE(11,IOSTAT=IOS)
          CALL VT_OUT(' ')
          CALL VT_OUT(' New SAT_INP_STS.DAT file created')
          CALL VT_OUT(' ')
          CALL VT_OUT('$Press Enter to continue...')
          CALL VT_IN(COPT1,ILEN,IS)
        ELSE IF(IOPT.EQ.8) THEN
          CLINE='00000 00000'
          OPEN(UNIT=11,FILE='[HORACE.SATCOM]SAT_OUT_STS.DAT',
     &        STATUS='NEW',
     &        SHARED,ACCESS='DIRECT',RECL=20,IOSTAT=IOS)
          WRITE(11,REC=1,IOSTAT=IOS) CLINE
          CLOSE(11,IOSTAT=IOS)
          CALL VT_OUT(' ')
          CALL VT_OUT(' New SAT_OUT_STS.DAT file created')
          CALL VT_OUT(' ')
          CALL VT_OUT('$Press Enter to continue...')
          CALL VT_IN(COPT1,ILEN,IS)
        ELSE IF(IOPT.EQ.9) THEN
          STATUS(54)=IBSET(STATUS(54),0) !Set GPS
        END IF
      END DO
10    FORMAT(A)
11    FORMAT(X,I2,A)
      END
!*******************************************************************************
      SUBROUTINE SAT_SPEC_MESS
!
! Allows the flight leader to create and send various special messages, such
! as coded profiles or SST maps.
!
! V1.00  19-11-96  W.D.N.JACKSON
!
      IMPLICIT  NONE
      INTEGER*4 IOS,IOPT,IL
      CHARACTER COPT*2
      INCLUDE   'HCOM_DEF.FOR'
!
! Clear screen, set cursor to home position
!
      CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H')
!
! Display the options and get user requirement
!
      DO WHILE(.TRUE.)
        CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J')
        CALL VT_OUT(CHAR(0)//CHAR(27)//'[1;1H')
        CALL VT_OUT(
     &      '                            Special message options')
        CALL VT_OUT(' ')
        CALL VT_OUT('  0) Exit this menu')
        CALL VT_OUT('  1) Send a TEMP message')
        CALL VT_OUT('  2) Send an SST map')
        CALL VT_OUT(' ')
        CALL VT_OUT('$Enter selection [0]: ')
        CALL VT_IN(COPT,IL,IOS)
        IF(IOS.NE.0) RETURN
        IF(IL.EQ.0) IOPT='0'
        READ(COPT,'(BN,I)',IOSTAT=IOS) IOPT !Get response
        IF(IOS.NE.0.OR.IOPT.EQ.0) RETURN
        IF(IOPT.EQ.1) CALL SAT_SEND_MESS('TEMP') !Send TEMP message
        IF(IOPT.EQ.2) CALL SAT_SEND_MESS('SSTM') !Send SST map
      END DO
10    FORMAT(A)
11    FORMAT(X,I2,A)
      END
C*******************************************************************************
      SUBROUTINE DRS_MENU
C
C Displays the DRS menu options, and calls the appropriate routines to execute
C them.  This menu, and all the DRS options, carry a standard DRS type header
C line which is updated once a second with time and event mark information.
C This is done by frequent checking for new DRS data, and then updating anyC
C parts of the status line that have changed.
C
C Subroutines: DRS_HEADER    INST_SUBS.FOR
C              DRS_FOOT      INST_SUBS.FOR
C              DRS_PRINT     INST_SUBS.FOR
C              DRS_SELECT    INST_SUBS.FOR
C              DRS_PLOT      INST_SUBS.FOR
C              DRS_LIST      INST_SUBS.FOR
C              DRS_NAMES     INST_SUBS.FOR
C              DRS_BLOCKS    INST_SUBS.FOR
C
C V1.00  23-02-91  W.D.N.JACKSON
C
      IMPLICIT  NONE
      CHARACTER CLINE(11)*50
      CHARACTER CSNAME(512)*4,CFNAME(512)*20,COPTION*4
      INTEGER*4 NOPTIONS,IOPTION,I,IOS,ILEN
      LOGICAL*4 LSELECT
      COMMON    /DRSCOM/ CSNAME,CFNAME
C
C DRS menu contents
C
      DATA CLINE(1)/' '/
      DATA CLINE(2)/'                        DRS Menu'/
      DATA CLINE(3)/' '/
      DATA CLINE(4)/' 0) Exit menu'/
      DATA CLINE(5)/' 1)                      2)                     '/
      DATA CLINE(6)/' 3) LIST      - Hex      4) LIST      - Dec     '/
      DATA CLINE(7)/' 5) MULTILIST - Hex      6) MULTILIST - Dec     '/
      DATA CLINE(8)/' 7) BLOCKS    - Hex      8) BLOCKS    - Dec     '/
      DATA CLINE(9)/' 9) NAMES               10)                     '/
      DATA CLINE(10)/'11)                     12)                     '/ 
      DATA CLINE(11)/'13) BCDS Check List                            '/ 
      DATA NOPTIONS /14/
C
      CALL DRS_NAMES_READ              !Read in names if necessary
      DO WHILE(.TRUE.)                 !Keep looping till leave menu
        CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H')
        CALL VT_OUT(CHAR(0)//CLINE(1)//CHAR(13))
        DO I=2,11                      !Display the options
          CALL VT_OUT(' '//CLINE(I))
        END DO
        CALL VT_OUT(' ')
        LSELECT=.TRUE.
        DO WHILE(LSELECT)              !Loop until have valid entry
          CALL VT_OUT('$Enter selection [0]... ')
          CALL VT_IN(COPTION,ILEN,IOS)
          IF(IOS.NE.0) RETURN
          IF(ILEN.EQ.0) COPTION='0'
          READ(COPTION,'(BN,I)',IOSTAT=IOS) IOPTION
          IF(IOS.NE.0) RETURN
          IF(IOPTION.EQ.0) RETURN
          IF(IOPTION.GE.1.AND.IOPTION.LT.NOPTIONS) LSELECT=.FALSE.
        END DO
        CALL VT_OUT('  ')
!        WRITE(10,'(A)',IOSTAT=IOS) ' '
        IF(IOPTION.EQ.1) THEN
!          CALL DRS_PLOT('PLT1')        !Plot one parameter
        ELSE IF(IOPTION.EQ.2) THEN
!          CALL DRS_PLOT('PLT2')        !Plot two parameters
        ELSE IF(IOPTION.EQ.3) THEN
          CALL DRS_LIST('HEX')         !List parameter in hex
        ELSE IF(IOPTION.EQ.4) THEN
          CALL DRS_LIST('DEC')         !List parameter in decimal
        ELSE IF(IOPTION.EQ.5) THEN
          CALL DRS_MLIST('HEX')        !List multiple parameters in hex
        ELSE IF(IOPTION.EQ.6) THEN
          CALL DRS_MLIST('DEC')        !List multiple parameters in decimal
        ELSE IF(IOPTION.EQ.7) THEN
          CALL INPUT_AST
          CALL DRS_BLOCKS('HEX')       !List DRS blocks in hex
          CALL INPUT_ABORT
        ELSE IF(IOPTION.EQ.8) THEN
          CALL INPUT_AST
          CALL DRS_BLOCKS('DEC')       !List DRS blocks in decimal
          CALL INPUT_ABORT
        ELSE IF(IOPTION.EQ.9) THEN
          CALL INPUT_AST
          CALL DRS_NAMES               !List parameter names
          CALL INPUT_ABORT
        ELSE IF(IOPTION.EQ.10) THEN
!          CALL DRS_PLOT('BITS')        !Plot parameter in bits
        ELSE IF(IOPTION.EQ.11) THEN
!          CALL DRS_CHECK               !Run through DRS CHEK check list
        ELSE IF(IOPTION.EQ.12) THEN
!          CALL DRS_TOTW                !Run through Total Water check list
        ELSE IF(IOPTION.EQ.13) THEN
          CALL DRS_BCDS                !Run through BCDS check list
        END IF
      END DO                           !Else give this menu again
      END
C*******************************************************************************
      SUBROUTINE DRS_LIST(CMODE)
C
C Lists a DRS parameter in decimal or hex.  Up to eight values are listed on a
C line, together with the DRS time.  The listing can be started or held with
C the H key.  The P key allows the parameter being listed to be changed.
C
C V1.00  14/05/91  W.D.N.JACKSON
C
      IMPLICIT  NONE
      INTEGER*4 IOS,IPARAM,IPFREQ,IPLOC,J,I,LSTBLK,IKEY,IL
      LOGICAL*4 LSELECT,LOOP,LHOLDING
      CHARACTER CMODE*3,CSNAME(512)*4,CFNAME(512)*20,CPARA1*3
      CHARACTER CTIME*8,CLINE*57,CFREQ*2,CH*8,CL*2,CLINE1*57,CPARAM*4
      INTEGER*4 IBUFF(128)
      COMMON    /DRSCOM/ CSNAME,CFNAME
      INCLUDE   'HCOM_DEF.FOR'
      DATA      CLINE1 /' '/

      DO WHILE(.TRUE.)                 !This loop gets a new parameter  
        CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H')
        LSELECT=.TRUE.
        DO WHILE(LSELECT)              !Loop until have valid entry
          CALL VT_OUT('$Parameter number [1]... ')
          CALL VT_IN(CPARAM,IL,IOS)
          IF(IOS.NE.0) RETURN
          IF(IL.EQ.0) CPARAM='1'
          READ(CPARAM,'(BN,I)',IOSTAT=IOS) IPARAM
          IF(IOS.NE.0) RETURN
          IF(IPARAM.EQ.0) RETURN
          IF(IPARAM.GE.1.AND.IPARAM.LE.512) THEN
            IF(NPFREQ(IPARAM).NE.0) LSELECT=.FALSE.
          END IF
        END DO
!
        IPFREQ=NPFREQ(IPARAM)          !Get parameter frequency
        IPLOC=NPLOC(IPARAM)            !Get parameter data start location
        WRITE(CPARA1,10,IOSTAT=IOS) IPARAM !Convert para num to string
        WRITE(CFREQ,13,IOSTAT=IOS) IPFREQ !Convert para freq to string
        CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H')
        CALL VT_OUT(' PARA '//CPARA1//'  '//CSNAME(IPARAM)//'   '//
     -      CFNAME(IPARAM)//'  '//CFREQ//' Hz')
        CALL VT_OUT(' ')
        IL=3
        LHOLDING=.FALSE.               !Not on hold initially
        LOOP=.TRUE.                    !Keep looping once a second
        CALL INPUT_AST
        DO WHILE(LOOP)                 !Keep looping till user wants to exit
          CH='H - Hold'                !Set hold option
          IF(LHOLDING) CH='H - Resu'
          IF(.NOT.LHOLDING) THEN       !If not on hold
            CALL DRS_CTIM(NFDATA(1,STATUS(1)),CTIME) !Get latest DRS time
            J=1                        !Pointer to data for required param
            DO I=IPLOC,IPLOC+IPFREQ-1  !Transfer params data into buffer
              IBUFF(J)=JZEXT(NFDATA(I,STATUS(1)))
              J=J+1
            END DO
            LSTBLK=NFDATA(8,STATUS(1)) !Note last DRS block number
            CLINE=CTIME                !Only the first line contains the time
            DO I=1,IPFREQ,8            !Eight values per line
              CALL CHECK_KEY('EPHeph',IKEY)
              IF(IKEY.NE.0) GOTO 100   !Check keys
              IF(CMODE.EQ.'HEX') WRITE(CLINE(10:57),11,IOSTAT=IOS) !Code line
     -            (IBUFF(J),J=I,MIN(IPFREQ,I+7))
              IF(CMODE.EQ.'DEC') WRITE(CLINE(10:57),12,IOSTAT=IOS) !Code line
     -            (IBUFF(J),J=I,MIN(IPFREQ,I+7))
              WRITE(CL,'(I2.2)',IOSTAT=IOS) IL
              CALL VT_OUT(CHAR(0)//CHAR(27)//'['//CL//';1H'//CLINE//
     -            CHAR(13)//CHAR(10)//CLINE1)
              IL=IL+1
              IF(IL.GT.22) IL=3
            END DO
            CALL VT_OUT(CHAR(0)//CHAR(27)//'[24;1H')
            CALL VT_OUT(CHAR(0)//'E - Exit  P - Para'//'  '//CH)
          END IF
C
C Now wait for a valid key to be pressed, or for another block of raw data
C to become available.  Check every 0.2 seconds.  At the same time update
C the header and printer status information whenever it changes
C
          DO WHILE(.TRUE.)             !Keep looping till have reason to leave
            CALL CHECK_KEY('EPHeph',IKEY)
            IF(IKEY.NE.0) GOTO 100   !Check keys
            IF(.NOT.LHOLDING.AND.NFDATA(8,STATUS(1)).NE.LSTBLK) GOTO 100
            CALL LIB$WAIT(0.2)         !Wait for 0.2 s
          END DO
100       IF(CHAR(IKEY).EQ.'P'.OR.CHAR(IKEY).EQ.'p') LOOP=.FALSE. !Another para
          IF(CHAR(IKEY).EQ.'H'.OR.CHAR(IKEY).EQ.'h') 
     -        LHOLDING=.NOT.LHOLDING
          IF(CHAR(IKEY).EQ.'E'.OR.CHAR(IKEY).EQ.'e') LOOP=.FALSE.
        END DO
        CALL INPUT_ABORT
        IF(CHAR(IKEY).EQ.'E'.OR.CHAR(IKEY).EQ.'e') RETURN
      END DO
10    FORMAT(I3)
11    FORMAT(8Z5.4)
12    FORMAT(8I6)
13    FORMAT(I2)
      END
C*******************************************************************************
      SUBROUTINE DRS_MLIST(CMODE)
C
C Lists multiple DRS parameters in decimal or hex.  Up to eight parameters
C are listed on a line, together with the DRS time.  The listing can be started
C or held with the H key.  The P key allows the parameter being listed to be 
C changed.
C
C V1.00  14/05/91  W.D.N.JACKSON
C
      IMPLICIT  NONE
      INTEGER*4 IP,IPARAM,I,IPCNT,IMAXFRQ,J,ILOC,IFRQ,LSTBLK,
     -    IKEY,IS,IOS,IL
      LOGICAL*4 LCHOOSING,LOOP,LHOLDING,LSELECT
      CHARACTER CMODE*3,CSNAME(512)*4
      CHARACTER CTIME*8,CLINE*57,CH*8,CPARAM*4
      INTEGER*4 IBUFF(128,8)
      INTEGER*4 IPARA(9)
      DATA      IPARA /9*0/
      COMMON    /DRSCOM/ CSNAME
      INCLUDE   'HCOM_DEF.FOR'

      DO WHILE(.TRUE.)                 !This loop gets a new parameter  
        CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H')
        CALL VT_OUT(' Enter a zero to end list.')
        CALL VT_OUT(' ')
        IP=1
        LCHOOSING=.TRUE.
        DO WHILE(LCHOOSING)
          LSELECT=.TRUE.
          DO WHILE(LSELECT)              !Loop until have valid entry
            CALL VT_OUT('$Parameter number [0]... ')
            CALL VT_IN(CPARAM,IL,IOS)
            IF(IOS.NE.0) RETURN
            IF(IL.EQ.0) CPARAM='0'
            READ(CPARAM,'(BN,I)',IOSTAT=IOS) IPARAM
            IF(IOS.NE.0) RETURN
            IF(IPARAM.GE.1.AND.IPARAM.LE.512) THEN
              IF(NPFREQ(IPARAM).NE.0) THEN
                LSELECT=.FALSE.
              ELSE
                CALL VT_OUT(' Invalid parameter')
              END IF
            ELSE
              IF(IPARAM.NE.0) CALL VT_OUT(' Invalid parameter')
            END IF
            IF(IPARAM.EQ.0) LSELECT=.FALSE.
          END DO
          IF(IPARAM.NE.0) THEN
            IPARA(IP)=IPARAM
            IP=IP+1
          END IF
          IF(IP.GE.9.OR.IPARAM.EQ.0) LCHOOSING=.FALSE.
        END DO
C
        IPCNT=IP-1
        IF(IPCNT.EQ.0) RETURN
        CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H')
        IF(CMODE.EQ.'HEX') THEN
          WRITE(CLINE,15,IOSTAT=IOS) (IPARA(I),I=1,IPCNT)
        ELSE
          WRITE(CLINE,17,IOSTAT=IOS) (IPARA(I),I=1,IPCNT)
        END IF
        CALL VT_OUT(' '//CLINE)
        IF(CMODE.EQ.'HEX') THEN
          WRITE(CLINE,16,IOSTAT=IOS) (CSNAME(IPARA(I)),I=1,IPCNT)
        ELSE
          WRITE(CLINE,18,IOSTAT=IOS) (CSNAME(IPARA(I)),I=1,IPCNT)
        END IF
        CALL VT_OUT(' '//CLINE)
        CALL VT_OUT(' ')
        LHOLDING=.FALSE.               !Not on hold initially
        LOOP=.TRUE.                    !Keep looping once a second
        CALL INPUT_AST
        DO WHILE(LOOP)                 !Keep looping till user wants to exit
          CH='H - Hold'                !Set hold option
          IF(LHOLDING) CH='H - Resu'
          IF(.NOT.LHOLDING) THEN       !If not on hold
            CALL DRS_CTIM(NFDATA(1,STATUS(1)),CTIME) !Get latest DRS time
            IMAXFRQ=0
            DO IP=1,IPCNT
              ILOC=NPLOC(IPARA(IP))
              IFRQ=NPFREQ(IPARA(IP))
              IF(IFRQ.GT.IMAXFRQ) IMAXFRQ=IFRQ
              J=1
              DO I=ILOC,ILOC+IFRQ-1
                IBUFF(J,IP)=JZEXT(NFDATA(I,STATUS(1)))
                J=J+1
              END DO
            END DO
            LSTBLK=NFDATA(8,STATUS(1)) !Note last DRS block number
            CLINE=CTIME                !Only the first line contains the time
            CALL VT_OUT(CHAR(0)//CHAR(27)//'[2;1H')
            DO I=1,MIN(IMAXFRQ,20) 
              CALL CHECK_KEY('EPHeph',IKEY)
              IF(IKEY.NE.0) GOTO 100   !Check keys
              IF(CMODE.EQ.'HEX') WRITE(CLINE(10:57),11,IOSTAT=IOS) !Code line
     -            (IBUFF(I,J),J=1,IPCNT)
              IF(CMODE.EQ.'DEC') WRITE(CLINE(10:57),12,IOSTAT=IOS) !Code line
     -            (IBUFF(I,J),J=1,IPCNT)
              DO IP=1,IPCNT
                IF(I.GT.NPFREQ(IPARA(IP))) THEN
                  IS=10+(IP-1)*5
                  CLINE(IS:IS+4)=' '
                END IF
              END DO
              CALL VT_OUT(' '//CLINE)
            END DO
            CALL VT_OUT(CHAR(0)//CHAR(27)//'[24;1H')
            CALL VT_OUT(CHAR(0)//'E - Exit  P - Para'//'  '//CH)
          END IF
C
C Now wait for a valid key to be pressed, or for another block of raw data
C to become available.  Check every 0.2 seconds.  At the same time update
C the header and printer status information whenever it changes
C
          DO WHILE(.TRUE.)             !Keep looping till have reason to leave
            CALL CHECK_KEY('EPHeph',IKEY)
            IF(IKEY.NE.0) GOTO 100   !Check keys
            IF(.NOT.LHOLDING.AND.NFDATA(8,STATUS(1)).NE.LSTBLK) GOTO 100
            CALL LIB$WAIT(0.2)         !Wait for 0.2 s
          END DO
100       IF(CHAR(IKEY).EQ.'P'.OR.CHAR(IKEY).EQ.'p') LOOP=.FALSE. !Another para
          IF(CHAR(IKEY).EQ.'H'.OR.CHAR(IKEY).EQ.'h') 
     -        LHOLDING=.NOT.LHOLDING
          IF(CHAR(IKEY).EQ.'E'.OR.CHAR(IKEY).EQ.'e') LOOP=.FALSE.
        END DO
        CALL INPUT_ABORT
        IF(CHAR(IKEY).EQ.'E'.OR.CHAR(IKEY).EQ.'e') RETURN
      END DO
10    FORMAT(I3)
11    FORMAT(8Z5.4)
12    FORMAT(8I6)
13    FORMAT(I2)
14    FORMAT(I1)
15    FORMAT(9X,8I5)
16    FORMAT(9X,8(X,A))
17    FORMAT(9X,8I6)
18    FORMAT(9X,8(2X,A))
      END
C*******************************************************************************
      SUBROUTINE DRS_NAMES
C
C Displays the short and long names of the DRS parameters, together with
C their frequency and position in the DRS data block.  Displays one parameter
C per line, with 24 lines per page.  The immediate keys D and E allow
C the display of further or previous pages of parameters, and the keys A to C
C allow return to the previous menu levels.
C
C V1.00  14/05/91  W.D.N.JACKSON
C
      IMPLICIT  NONE
      INTEGER*4 IP,I,IP1,ILSTP1,IPPP,IL,IP2,IP3,IKEY,IOS
      CHARACTER CSNAME(512)*4,CFNAME(512)*20
      CHARACTER CLINE*40,CN*8,CP*8
      INTEGER*4 IPARAS(512)
      COMMON    /DRSCOM/ CSNAME,CFNAME
      INCLUDE   'HCOM_DEF.FOR'
      DATA      IPPP /22/              !No of lines per page of display

      IP=0                             !No of parameters to display
      DO I=1,512                       !Put valid parameters in a separate list
        IF(NPFREQ(I).GT.0.AND.NPFREQ(I).LE.64) THEN
          IP=IP+1
          IPARAS(IP)=I
        END IF
      END DO
      ILSTP1=0                         !Last parameter list pointer
      IP1=1                            !Pointer to next parameter at start of
      DO WHILE(.TRUE.)                 !Keep looping till user wants to exit
        IF(ILSTP1.NE.IP1) THEN         !If new page wanted
          CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H')
          CP='P - Prev'                !Define available options
          CN='N - Next'        
          IF(IP1.EQ.1) CP=' '          !Can't do previous if at start
          IF(IP1+IPPP.GT.IP) CN=' '    !Can't do next if at end
          IL=0                         !Lines listed on current page
          DO IP2=IP1,MIN(IP1+IPPP-1,IP) !For each parameter on page
            CALL CHECK_KEY('EPNepn',IKEY)
            IF(CHAR(IKEY).EQ.'E'.OR.CHAR(IKEY).EQ.'e') RETURN
            IF(CHAR(IKEY).EQ.'P'.OR.CHAR(IKEY).EQ.'p') GOTO 100
            IF(CHAR(IKEY).EQ.'N'.OR.CHAR(IKEY).EQ.'n') GOTO 100
            IP3=IPARAS(IP2)            !Parameter number
            WRITE(CLINE,10,IOSTAT=IOS) IP3,NPFREQ(IP3),NPLOC(IP3),
     -          CSNAME(IP3),CFNAME(IP3) !Write data into string
            IF(IP2.EQ.IP1) THEN
              CALL VT_OUT(CHAR(0)//CLINE//CHAR(13))
            ELSE
              CALL VT_OUT(' '//CLINE)
            END IF
            IL=IL+1                    !Bump line count
          END DO
          DO WHILE(IL.LT.IPPP+1)
            CALL VT_OUT(' ')
            IL=IL+1
          END DO
          ILSTP1=IP1                   !Save last parameter pointer
          CALL VT_OUT(' '//'E - Exit  '//CP//'  '//CN)
        END IF
C
C Now wait for a valid key to be pressed.
C
        DO WHILE(.TRUE.)
          CALL CHECK_KEY('EPNepn',IKEY)
          IF(CHAR(IKEY).EQ.'E'.OR.CHAR(IKEY).EQ.'e') RETURN
          IF(CHAR(IKEY).EQ.'P'.OR.CHAR(IKEY).EQ.'p') GOTO 100
          IF(CHAR(IKEY).EQ.'N'.OR.CHAR(IKEY).EQ.'n') GOTO 100
          CALL LIB$WAIT(0.2)           !Wait 0.2s before trying again
        END DO
100     IF(CHAR(IKEY).EQ.'P'.OR.CHAR(IKEY).EQ.'p') IP1=IP1-IPPP !Previous page
        IF(IP1.LT.1) IP1=1
        IF(CHAR(IKEY).EQ.'N'.OR.CHAR(IKEY).EQ.'n') IP1=IP1+IPPP !Next page
        IF(IP1.GT.IP) IP1=IP1-IPPP
      END DO
10    FORMAT(I3,I4,I5,2X,A,2X,A)
      END
C*******************************************************************************
      SUBROUTINE DRS_BLOCKS(CMODE)
C
C Displays DRS data blocks in decimal or hex.  When in continuous mode only the
C first 32 words of each block are displayed, in order to keep the display 
C going in real-time.  When in 'HOLD' mode all of the data in a block can be
C inspected by using the next and previous page keys.
C
C V1.00  14/05/91  W.D.N.JACKSON
C
      IMPLICIT  NONE
      INTEGER*4 I,IWPB,ILPP,ILINES,ILSTLINES,IS,ISTRT,LSTBLK,J,IL,IKEY,
     -    IOS,K
      LOGICAL*4 LHOLDING
      CHARACTER CMODE*3,CLINE*55,CP*8,CN*8,CH*8
      INCLUDE   'HCOM_DEF.FOR'
      INTEGER*4 IBUFF(2048)
      DATA      ILPP /22/

      I=512                            !Work out the no of valid words in a blk
      DO WHILE(NPLOC(I).EQ.0.AND.I.GT.1)
        I=I-1
      END DO
      IWPB=NPLOC(I)-1                  !Words per block
      type *,iwpb
      ILINES=-1
      LHOLDING=.FALSE.                 !Not currently on 'HOLD'
!
      CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J')
      DO WHILE(.TRUE.)                 !This loop executes once a second
        ILSTLINES=ILINES
        IF(.NOT.LHOLDING) THEN         !If not on 'HOLD'
          ILINES=4                     !No of lines to display when not holding
          IS=STATUS(1)                 !Transfer latest data block to local buf
          DO I=1,IWPB
            IBUFF(I)=JZEXT(NFDATA(I,IS))
          END DO
          ISTRT=1                      !Start point for data display
          LSTBLK=IBUFF(8)              !Latest DRS block number
          CP=' '                       !Define valid immediate options
          CN=' '
          CH='H - Hold'
          IF(ILSTLINES.NE.4) CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J')
        ELSE
          ILINES=ILPP                  !No of lines to display when holding
          CP='P - Prev'                !Define valid immediate options
          IF(ISTRT.LE.1) CP=' '
          CN='N - Next'
          IF(ISTRT+8*ILPP.GT.IWPB) CN=' '
          CH='H - Resu'
          CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J')
        END IF
        J=ISTRT                        !DRS block location for first word
        CALL VT_OUT(CHAR(0)//CHAR(27)//'[1;1H')
        DO IL=1,ILINES                 !For each line
          CALL CHECK_KEY('EPNHepnh',IKEY)
          IF(CHAR(IKEY).EQ.'E'.OR.CHAR(IKEY).EQ.'e') RETURN
          IF(CHAR(IKEY).EQ.'H'.OR.CHAR(IKEY).EQ.'h') GOTO 100
          IF(LHOLDING) THEN
            IF(CHAR(IKEY).EQ.'P'.OR.CHAR(IKEY).EQ.'p'.AND.CP.NE.' ')
     -          GOTO 100
            IF(CHAR(IKEY).EQ.'N'.OR.CHAR(IKEY).EQ.'n'.AND.CN.NE.' ')
     -          GOTO 100
          END IF
          IF(J.LE.IWPB) THEN
            IF(CMODE.EQ.'HEX') WRITE(CLINE,10,IOSTAT=IOS)
     -          J,(IBUFF(K),K=J,MIN(J+7,IWPB))
            IF(CMODE.EQ.'DEC') WRITE(CLINE,11,IOSTAT=IOS)
     -          J,(IBUFF(K),K=J,MIN(J+7,IWPB))
          ELSE
            CLINE=' '
          END IF
          IF(IL.EQ.1) THEN
            CALL VT_OUT(CHAR(0)//CLINE//CHAR(13))
          ELSE
            CALL VT_OUT(' '//CLINE)
          END IF
          J=J+8                        !Bump word point to next data
        END DO
        CALL VT_OUT(' ')
        CALL VT_OUT(' '//'E - Exit  '//CP//'  '//CN//'  '//CH)
        DO WHILE(.TRUE.)               !Now wait for something to happen
          CALL CHECK_KEY('EPNHepnh',IKEY)
          IF(CHAR(IKEY).EQ.'E'.OR.CHAR(IKEY).EQ.'e') RETURN
          IF(CHAR(IKEY).EQ.'H'.OR.CHAR(IKEY).EQ.'h') GOTO 100
          IF(LHOLDING) THEN
            IF(CHAR(IKEY).EQ.'P'.OR.CHAR(IKEY).EQ.'p'.AND.CP.NE.' ')
     -          GOTO 100
            IF(CHAR(IKEY).EQ.'N'.OR.CHAR(IKEY).EQ.'n'.AND.CN.NE.' ')
     -          GOTO 100
          END IF
          IF(.NOT.LHOLDING.AND.NFDATA(8,STATUS(1)).NE.LSTBLK) GOTO 100 !New data
          CALL LIB$WAIT(0.2)           !Wait 0.2s before trying again
        END DO
100     IF(LHOLDING) THEN
          IF(CHAR(IKEY).EQ.'P'.OR.CHAR(IKEY).EQ.'p') ISTRT=ISTRT-8*ILPP !Start of previous page
          IF(ISTRT.LT.1) ISTRT=1
          IF(CHAR(IKEY).EQ.'N'.OR.CHAR(IKEY).EQ.'n') ISTRT=ISTRT+8*ILPP !Start of next page
          IF(IWPB.LT.ISTRT) ISTRT=ISTRT-8*ILPP
        END IF
        IF(CHAR(IKEY).EQ.'H'.OR.CHAR(IKEY).EQ.'h')LHOLDING=.NOT.LHOLDING
      END DO
10    FORMAT(I5,2X,8Z5.4)
11    FORMAT(I5,2X,8I6)
      END
C******************************************************************************
      SUBROUTINE DRS_BCDS
C
C Displays selected DRS parameters on HORACE. Mimics the DRS BCDS program.
C Keys E starts listing the next parameter in the list, key H the previous 
C parameter.
C 
C V1.01  08/03/94  D.R.Lauchlan - based on DRS_LIST by W.D.N.Jackson
C
      IMPLICIT  NONE
      INTEGER*4 INPARAS,IOS,IPOS,ILSTP,IPARAM,I,IPFREQ,IPLOC,
     -    J,LSTBLK,IKEY,IL
      LOGICAL*4 LPARAM,LSELECT,LOOP,LHOLDING
      CHARACTER CSNAME(512)*4,CFNAME(512)*20,CPARA1*4
      CHARACTER CTIME*8,CLINE*56,CLINE1*56,CPARAM*4
      CHARACTER CH1*8,CH2*8,CMODE*3,CH*8,CL*2
      INTEGER*2 IBUFF(128),IPARAS(4)
      COMMON    /DRSCOM/ CSNAME,CFNAME
      INCLUDE   'HCOM_DEF.FOR'
      DATA      ILSTP /1/
      DATA      IPARAS /1,2,3,4/
      DATA      INPARAS /4/
C                                                 
C NB whenever a parameter is added or removed to/from the IPARAS list then
C INPARAS has to be changed accordingly and the IPARAS array has to be defined
C to be the same length as INPARAS.
C 
      CH='H - Hold'
      CH1='P - Prev'                   
      CH2='N - Next'                   
C
C Get starting parameter number
C
      CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H')
      LSELECT=.TRUE.
      DO WHILE(LSELECT)                !Loop until have valid entry
        CALL VT_OUT('$Parameter number [1]... ')
        CALL VT_IN(CPARAM,IL,IOS)
        IF(IOS.NE.0) RETURN
        IF(IL.EQ.0) CPARAM='1'
        READ(CPARAM,'(BN,I)',IOSTAT=IOS) IPARAM
        IF(IOS.NE.0) RETURN
        IF(IL.EQ.0) IPARAM=ILSTP
        IF(IPARAM.EQ.0) RETURN
        DO I=1,INPARAS
          IF(IPARAM.EQ.IPARAS(I)) THEN
            IF(NPFREQ(IPARAM).NE.0) LSELECT=.FALSE.
            IPOS=I
          END IF
        END DO
      END DO
!
      ILSTP=IPARAM
      IPFREQ=NPFREQ(IPARAM)            !Note parameter frequency
      IPLOC=NPLOC(IPARAM)              !Note where data starts
C
C LPARAM is enacted whenever a new parameter is plotted
C
      CALL INPUT_AST
      LPARAM=.TRUE.
      DO WHILE(LPARAM)
        CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H')
C        
C Now put up the parameter information 
C
        WRITE(CPARA1,10,IOSTAT=IOS) IPARAM !Form parameter string
        CLINE=' '
        WRITE(CLINE(15:56),11,IOSTAT=IOS) ' PARA '//CPARA1//' '//
     -      CSNAME(IPARAM)//' '//CFNAME(IPARAM)//' ',IPFREQ,' Hz'
        CALL VT_OUT(CHAR(0)//CLINE//CHAR(13))
        IL=3
        LHOLDING=.FALSE.               !Not on hold initially
        LOOP=.TRUE.               
        CLINE1=' '
        DO WHILE(LOOP)                 !Keep looping till user wants to exit
          CMODE='DEC'
          IF(IPARAM.LT.49.OR.IPARAM.GT.504) CMODE='HEX'
          CALL DRS_CTIM(NFDATA(1,STATUS(1)),CTIME) !Get latest DRS time
C Get data to be plotted
          J=1                          !Pointer to data for required param
          DO I=IPLOC,IPLOC+IPFREQ-1    !Transfer params data into buffer
            IBUFF(J)=NFDATA(I,STATUS(1))
            IF(IPARAM.GT.48.AND.IPARAM.LT.162)
     -          IBUFF(J)=IBUFF(J)  
            J=J+1
          END DO
!
          LSTBLK=NFDATA(8,STATUS(1))   !Note last DRS block number
          CLINE=CTIME                  !Only the first line contains the time
          DO I=1,IPFREQ,8              !Eight values per line
            CALL CHECK_KEY('EPNHepnh',IKEY)
            IF(IKEY.NE.0) GOTO 100     !Check keys
            IF(CMODE.EQ.'HEX') WRITE(CLINE(10:49),16,IOSTAT=IOS) !Code line
     -          (IBUFF(J),J=I,MIN(IPFREQ,I+7))
            IF(CMODE.EQ.'DEC') WRITE(CLINE(10:49),17,IOSTAT=IOS) !Code line
     -          (IBUFF(J),J=I,MIN(IPFREQ,I+7))
            WRITE(CL,'(I2.2)',IOSTAT=IOS) IL
            CALL VT_OUT(CHAR(0)//CHAR(27)//'['//CL//';1H'//CLINE//
     -          CHAR(13)//CHAR(10)//CLINE1)
            IL=IL+1
            IF(IL.GT.22) IL=3
          END DO
          CALL VT_OUT(CHAR(0)//CHAR(27)//'[24;1H')
          CALL VT_OUT(CHAR(0)//'E - Exit  '//CH1//'  '//CH2//'  '//CH)
C 
C Now wait for a valid key to be pressed, or for another block of raw data
C to become available.  Check 5 times a second and also update header and
C printer status information when necessary.
C
          DO WHILE(.TRUE.)
            CALL CHECK_KEY('EPNHepnh',IKEY)
            IF(IKEY.NE.0) GOTO 100     !Check keys
            IF(.NOT.LHOLDING.AND.NFDATA(8,STATUS(1)).NE.LSTBLK) 
     -          GOTO 100               !New data
            CALL LIB$WAIT(0.2)         !Wait 0.2 s
          END DO
100       IF(CHAR(IKEY).EQ.'H'.OR.CHAR(IKEY).EQ.'h') THEN
            LHOLDING=.NOT.LHOLDING
            CH='H - Hold'
            IF(LHOLDING) CH='H - Resu'
          END IF
          IF(CHAR(IKEY).EQ.'E'.OR.CHAR(IKEY).EQ.'e') THEN
            CALL INPUT_ABORT
            RETURN
          END IF
          IF (CHAR(IKEY).EQ.'P'.OR.CHAR(IKEY).EQ.'N'
     -    .OR.CHAR(IKEY).EQ.'p'.OR.CHAR(IKEY).EQ.'n') THEN
            IF(CHAR(IKEY).EQ.'P'.OR.CHAR(IKEY).EQ.'p')THEN
              IF (IPOS.EQ.1)IPOS=IPOS+INPARAS
              IPOS=IPOS-1              !decrement para number
            ELSE IF(CHAR(IKEY).EQ.'N'.OR.CHAR(IKEY).EQ.'n')THEN
              IF (IPOS.EQ.INPARAS)IPOS=IPOS-INPARAS
              IPOS=IPOS+1              !increment para number
            END IF
            IPARAM=IPARAS(IPOS)
            ILSTP=IPARAM
            IPFREQ=NPFREQ(IPARAM)      !Note parameter frequency
            IPLOC=NPLOC(IPARAM)        !Note where data starts
            LOOP=.FALSE.
          END IF
        END DO                         !end of LOOP
      END DO                           !end  of LPARAM
10    FORMAT(I3)
11    FORMAT(A,I2,A)
12    FORMAT(I2)
13    FORMAT('$',Z5.4,' (',I5.4,')')
14    FORMAT(I6)
15    FORMAT(A)
16    FORMAT(8Z5.4)
17    FORMAT(8I5.4)
      END
!*******************************************************************************
      SUBROUTINE INST_MENU
      IMPLICIT  NONE
      CHARACTER CLINE(11)*50,COPTION*4
      INTEGER*4 I,IOPTION,NOPTIONS,IOS,ILEN
      LOGICAL*4 LSELECT
!
! Instrument menu contents
!
      DATA CLINE(1)/' '/
      DATA CLINE(2)/'                    Instrument Menu'/
      DATA CLINE(3)/' '/
      DATA CLINE(4)/' 0) Exit menu    '/
      DATA CLINE(5)/' 1) DRS          '/
      DATA CLINE(6)/' 2) GPS          '/
      DATA CLINE(7)/' 3) INU          '/
      DATA CLINE(8)/' 4) SATCOM       '/
      DATA CLINE(9)/' 5) Nephelometer '/
      DATA CLINE(10)/' 6) Nevzorov     '/
      DATA CLINE(11)/' 7) NDU          '/
      DATA NOPTIONS /8/
!
      DO WHILE(.TRUE.)                 !Keep looping till leave menu
        CALL VT_OUT(CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H')
        CALL VT_OUT(CHAR(0)//CLINE(1)//CHAR(13))
        DO I=2,NOPTIONS+3              !Display the options
          CALL VT_OUT(' '//CLINE(I))
        END DO
        CALL VT_OUT(' ')
        LSELECT=.TRUE.
        DO WHILE(LSELECT)              !Loop until have valid entry
          CALL VT_OUT('$Enter selection [0]... ')
          CALL VT_IN(COPTION,ILEN,IOS)
          IF(IOS.NE.0) RETURN
          IF(ILEN.EQ.0) COPTION='0'
          READ(COPTION,'(BN,I)',IOSTAT=IOS) IOPTION
          IF(IOS.NE.0) RETURN
          IF(IOPTION.EQ.0) RETURN
          IF(IOPTION.GE.1.AND.IOPTION.LT.NOPTIONS) LSELECT=.FALSE.
        END DO
        CALL VT_OUT('  ')
!        WRITE(10,'(A)',IOSTAT=IOS) ' '
        IF(IOPTION.EQ.1) THEN
          CALL DRS_MENU                !Put up DRS menu options
        ELSE IF(IOPTION.EQ.2) THEN
          CALL GPS_MENU                !Put up GPS menu options
        ELSE IF(IOPTION.EQ.3) THEN
          CALL INU_MENU                !Put up INU menu options
        ELSE IF(IOPTION.EQ.4) THEN
          CALL SAT_MENU                !Put up Satcoms menu options
        ELSE IF(IOPTION.EQ.5) THEN
          CALL NEPH_MENU               !Put up Nephelometer options
        ELSE IF(IOPTION.EQ.6) THEN
          CALL INPUT_AST               !Enable unsolicited input
          CALL NEVZ_DISP               !Put up Nevzorov display
          CALL INPUT_ABORT
        ELSE IF(IOPTION.EQ.7) THEN
          CALL INPUT_AST               !Enable unsolicited input
          CALL NDU_DISP                !Put up NDU display
          CALL INPUT_ABORT
        END IF
      END DO                           !Else give this menu again
      END
C*******************************************************************************
      SUBROUTINE DRS_NAMES_READ
C
C Reads in the DRS parameter short and long names from the file DRS_NAMES.TXT.
C This is done whenever the DRS menu is first entered, and the names are
C thereafter accessible in the DRSCOM common block.
C
C V1.00  14/05/91  W.D.N.JACKSON
C
      IMPLICIT  NONE
      INTEGER*4 IOS,LUN,IPARAM
      CHARACTER CSNAME(512)*4,CFNAME(512)*20,CLINE*80
      COMMON    /DRSCOM/ CSNAME,CFNAME
      DATA      CSNAME/512*' '/,CFNAME/512*' '/

      IF(CSNAME(1).NE.' ') RETURN      !Don't need to do if already have a value
      CALL LIB$GET_LUN(LUN)            !Get a logical unit number
      OPEN(UNIT=LUN,FILE='[HORACE]DRS_NAMES.TXT',STATUS='OLD',READONLY,
     -    IOSTAT=IOS)
      IF(IOS.NE.0) THEN                !Return if any problems
        CALL LIB$FREE_LUN(LUN)
        RETURN
      END IF
      READ(LUN,'(A)',IOSTAT=IOS) CLINE !Read each line
      DO WHILE(IOS.EQ.0)               !Until get to end of file
        READ(CLINE(2:5),'(I4)',IOSTAT=IOS) IPARAM !Decode parameter number
        IF(IPARAM.GT.0.AND.IPARAM.LE.512) THEN !If valid
          CSNAME(IPARAM)=CLINE(14:17)  !Store short name
          CFNAME(IPARAM)=CLINE(30:49)  !Store long name
        END IF
        READ(LUN,'(A)',IOSTAT=IOS) CLINE !Read next line
      END DO
      CLOSE(UNIT=LUN,IOSTAT=IOS)       !Close file
      CALL LIB$FREE_LUN(LUN)           !Free logical unit
      RETURN
      END
C*******************************************************************************
      SUBROUTINE DRS_CTIM(NFDATA,CTIME)
C
C Produces a DRS time string in the format hh:mm:ss from a block of DRS data.
C
C V1.00  14/05/91  W.D.N.JACKSON
C
      IMPLICIT  NONE
      INTEGER*4 J1,J2,JHRS,JMIN,JSEC,IOS
      INTEGER*2 NFDATA(2048)
      CHARACTER*8 CTIME

      J1=NFDATA(10)                    !Work out DRS time
      J2=NFDATA(11)
      JHRS=IBITS(J1,8,4)*10+IBITS(J1,4,4)
      JMIN=IBITS(J1,0,4)*10+IBITS(J2,8,4)
      JSEC=IBITS(J2,4,4)*10+IBITS(J2,0,4)
      WRITE(CTIME(1:8),10,IOSTAT=IOS) JHRS,JMIN,JSEC
      RETURN
10    FORMAT(I2.2,':',I2.2,':',I2.2)
      END
C*******************************************************************************
      SUBROUTINE DRS_CDAT(NFDATA,CDATE)
C
C Produces a DRS date string in the format dd-mmm-yy from a block of DRS data.
C
C V1.00  14/05/91  W.D.N.JACKSON
C
      IMPLICIT  NONE
      INTEGER*4 J1,J2,JDAY,JMON,JYER,IOS
      INTEGER*2 NFDATA(2048)
      CHARACTER*9 CDATE
      CHARACTER*3 CMON(12) /'JAN','FEB','MAR','APR','MAY','JUN',
     -    'JUL','AUG','SEP','OCT','NOV','DEC'/
C
      CDATE='**-***-**'
      J1=NFDATA(5)                   !Work out DRS date
      J2=NFDATA(6)
      JDAY=IBITS(J1,8,4)*10+IBITS(J1,4,4)
      JMON=IBITS(J1,0,4)*10+IBITS(J2,8,4)
      JYER=IBITS(J2,4,4)*10+IBITS(J2,0,4)
      WRITE(CDATE(1:2),'(I2.2)',IOSTAT=IOS) JDAY
      IF(JMON.GE.1.AND.JMON.LE.12) CDATE(4:6)=CMON(JMON)
      WRITE(CDATE(8:9),'(I2.2)',IOSTAT=IOS) JYER
      RETURN
11    FORMAT(I2.2)
      END
!*******************************************************************************
      SUBROUTINE CTIMSPM(CTIM,ISECS)
!
! ROUTINE          CTIMSPM SUBROUTINE FORTVAX
!
! PURPOSE          Converts hh:mm:ss character string to seconds past midnight
!
! DESCRIPTION      Given an eight character time string in the form hh:mm:ss,
!                  returns the time as seconds past midnight.  Returns -1 if
!                  the character string contains an invalid time.
!
! VERSION          1.00  10-1-90  W.D.N.JACKSON
!
! ARGUMENTS        CTIM     C*8  IN   Character time as hh:mm:ss
!                  ISECS    I*4  OUT  Time in seconds past midnight
!
      IMPLICIT  NONE
      CHARACTER CTIM*(*)               !Character string to hold time
      INTEGER*4 ISECS                  !Time in secs past midnight
      INTEGER*4 IOS,JOS,KOS,JHRS,JMIN,JSEC
      CHARACTER CTMP*8

      CTMP=CTIM                        !Use temporary in case CTIM has wrong length
      READ(CTMP(1:2),'(I2)',IOSTAT=IOS) JHRS !Decode hours
      READ(CTMP(4:5),'(I2)',IOSTAT=JOS) JMIN !Decode minutes
      READ(CTMP(7:8),'(I2)',IOSTAT=KOS) JSEC !Decode seconds
      IF(IOS.NE.0.OR.JOS.NE.0.OR.KOS.NE.0.OR.JHRS.LT.0.OR.JHRS.GT.23
     -    .OR.JMIN.LT.0.OR.JMIN.GT.59.OR.JSEC.LT.0.OR.JSEC.GT.59.
     -    .OR.CTMP(3:3).NE.':'.OR.CTMP(6:6).NE.':') THEN
        ISECS=-1
      ELSE
        ISECS=3600*JHRS+60*JMIN+JSEC
      END IF
      RETURN
      END
!*******************************************************************************
      SUBROUTINE SAT_SSTMAP(CFILE)
!
! Generates a 'map' of sea surface temperature from measurements over the
! last 90 minutes, for transmission by Satcom.
!      
! CFILE specifies the file the map is to be written to.
!
! V1.00  19/11/96  W.D.N.JACKSON
!
      IMPLICIT  NONE
      CHARACTER CFILE*(*),CMAP(22)*65,CLET*52,CS*1,CLINE*80
      INTEGER*4 LAN,ISS,IS,I,IL,IX,IY,IOS,ILAT,ILNG
      REAL*4    RSST,RLAT,RLNG,RLATMIN,RLNGMIN,RSSTMIN,RSSTINT,RVAL,
     &    RCONST(4)
      INCLUDE   'HCOM_DEF.FOR'
      DATA CLET /'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'/
!
! Define bottom end of ranges.  Values for the four ranges/intervals are taken
! from the file SAT_SSTMAP.DAT if present, otherwise the default values below
! are used.
!
      RCONST(1)=35.                    !Minimum latitude, MUST be integer
      RCONST(2)=-3.                    !Minimum longitude, MUST be integer
      RCONST(3)=3.0                    !Minimum sea surface temperature
      RCONST(4)=0.1                    !Temperature interval
      OPEN(11,FILE='SAT_SSTMAP.DAT',READONLY,STATUS='OLD')
      READ(11,'(A)',IOSTAT=IOS) CLINE
      I=1
      DO WHILE(IOS.EQ.0)
        IF(CLINE(1:1).NE.'!') THEN
          READ(CLINE,*,IOSTAT=IOS) RCONST(I)
          IF(IOS.EQ.0) I=I+1
        END IF
        READ(11,'(A)',IOSTAT=IOS) CLINE
      END DO
      CLOSE(11,IOSTAT=IOS)
      RLATMIN=RCONST(1)                !Minimum latitude, MUST be integer
      RLNGMIN=RCONST(2)                !Minimum longitude, MUST be integer
      RSSTMIN=RCONST(3)                !Minimum sea surface temperature
      RSSTINT=RCONST(4)                !Temperature interval
!
! Create basic map outline
!
      CMAP(1)='G-LUXE SST dd-mmm-yy hh:mm:ss'
      CMAP(2)='3W--------2W--------1W--------0---------1E--------+39N'
      CMAP(3)='!         !         !         !         !         !'
      CMAP(4)=CMAP(3)
      CMAP(5)=CMAP(3)
      CMAP(6)=CMAP(3)
      CMAP(7)='+---------+---------+---------+---------+---------+38N'
      CMAP(8)=CMAP(3)
      CMAP(9)=CMAP(3)
      CMAP(10)=CMAP(3)
      CMAP(11)=CMAP(3)
      CMAP(12)='+---------+---------+---------+---------+---------+37N'
      CMAP(13)=CMAP(3)
      CMAP(14)=CMAP(3)
      CMAP(15)=CMAP(3)
      CMAP(16)=CMAP(3)
      CMAP(17)='+---------+---------+---------+---------+---------+36N'
      CMAP(18)=CMAP(3)
      CMAP(19)=CMAP(3)
      CMAP(20)=CMAP(3)
      CMAP(21)=CMAP(3)
      CMAP(22)='3W--------2W--------1W--------0---------1E--------+35N'
      CALL DATE1(CMAP(1)(11:19))
      CALL TIME(CMAP(1)(21:28))
!
! Put on accurate scale markings, depending on the values defined above
!
      DO IS=0,4
        ISS=2+5*IS
        ILAT=NINT(RLATMIN+4-IS)
        CS='N'
        IF(ILAT.LT.0) THEN
          CS='S'
          ILAT=-ILAT
        END IF
        WRITE(CMAP(ISS)(52:),'(I2,A)',IOSTAT=IOS) ILAT,CS
      END DO
!
      DO IS=0,4
        ISS=1+10*IS
        IL=2+5*IS
        ILNG=NINT(RLNGMIN+IS)
        CS='E'
        IF(ILNG.LT.0) THEN
          CS='W'
          ILNG=-ILNG
        END IF
        IF(ILNG.LT.10) THEN
          WRITE(CMAP(2)(ISS:ISS+1),'(I1,A)',IOSTAT=IOS) ILNG,CS
          WRITE(CMAP(22)(ISS:ISS+1),'(I1,A)',IOSTAT=IOS) ILNG,CS
        ELSE IF(ILNG.LT.100) THEN
          WRITE(CMAP(2)(ISS:ISS+2),'(I2,A)',IOSTAT=IOS) ILNG,CS
          WRITE(CMAP(22)(ISS:ISS+2),'(I2,A)',IOSTAT=IOS) ILNG,CS
        ELSE
          WRITE(CMAP(2)(ISS:ISS+3),'(I3,A)',IOSTAT=IOS) ILNG,CS
          WRITE(CMAP(22)(ISS:ISS+3),'(I3,A)',IOSTAT=IOS) ILNG,CS
        END IF
      END DO
!
      ISS=2
      DO IS=1,52,5
        RVAL=RSSTMIN+(IS-1)*RSSTINT
        WRITE(CMAP(ISS)(56:),'(A,F4.1)',IOSTAT=IOS) CLET(IS:IS)//' - ',
     &      RVAL
        ISS=ISS+1
      END DO
      WRITE(CMAP(ISS+1)(56:),'(A,F4.1)',IOSTAT=IOS) 'Int ',RSSTINT
!
! Now plot the data, provided everything is in range and the Heiman is not in
! calibrate.  Plot a maximum of 90 min of data, if available.
!     
      ISS=STATUS(2)
      DO IS=1,MIN(STATUS(3),90*20)     !Use last 90 mins of data
        RLNG=RILNG(ISS)
        IX=NINT(RLNG*10-RLNGMIN*10)+1
        RLAT=RILAT(ISS)
        IY=NINT((RLATMIN+4)*5-RLAT*5)+2
        RSST=RPRT4(ISS)
        IL=NINT(RSST/RSSTINT-RSSTMIN/RSSTINT)+1
        IF(IX.GE.1.AND.IX.LE.51.AND.IY.GE.2.AND.IY.LE.22.AND.
     &      IL.GE.1.AND.IL.LE.52.AND.SPECIAL(ISS,6).NE.1) 
     &      CMAP(IY)(IX:IX)=CLET(IL:IL)
        ISS=ISS+1
        IF(ISS.GT.12800) ISS=1
      END DO
!
! Write out the data
!
      OPEN(11,FILE=CFILE,STATUS='NEW',CARRIAGECONTROL='LIST',IOSTAT=IOS)
      DO I=1,22
        WRITE(11,'(A)',IOSTAT=IOS) CMAP(I)(1:LAN(CMAP(I)))
      END DO
      CLOSE(11,IOSTAT=IOS)
      RETURN
      END
C*******************************************************************************
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
C*******************************************************************************
      INTEGER*4 FUNCTION LAN(CSTRING)
C
C Function returns the length of a string not including trailing blanks. If 
C the string is all blanks, or is of zero length, the function returns a value
C of 1.
C
      IMPLICIT  NONE
      INTEGER*4 N,LEN
      CHARACTER CSTRING*(*)

      N=LEN(CSTRING)                   !Get string length
      DO WHILE(N.GT.0.AND.CSTRING(N:N).EQ.' ') !Step back over spaces
        N=N-1
      END DO
      IF(N.EQ.0) N=1                   !Special case
      LAN=N
      RETURN
      END
