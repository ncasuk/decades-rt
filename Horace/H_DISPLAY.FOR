      PROGRAM H_DISPLAY
C
C H_DISPLAY.FOR
C
C Provides the display on the HORACE aircraft displays.
C
C Build commands:
C
C    $ FORT H_DISPLAY
C    $ LINK H_DISPLAY,HORACE.OLB/LIB,GGSX/LIB,H_COMMON/OPT
C
C    where H_COMMON.OPT contains the line SYS$LIBRARY:H_COMMON /SHAREABLE
C
C Note that this program now only handles display to X terminals.
C
C Subroutines called:
C
C   LOGMESS   HOR_UTIL.FOR
C   GRFINI    HOR_SUBS.FOR
C   GET_LOGICAL HOR_UTIL.FOR
C   VT_OUT    HOR_UTIL.FOR
C
C V2.00  02/02/02  W.D.N.JACKSON
C
      IMPLICIT  NONE
      INTEGER*4 ISTAT,LIB$GETDVI,ILEN,IFUNC
      CHARACTER CNAME*64
      INCLUDE   '($SYSSRVNAM)'
      INCLUDE   '($SSDEF)'
      INCLUDE   '($IODEF)'
      INCLUDE   '($DVIDEF)'
      INCLUDE   'DCOM_DEF.FOR'
      INCLUDE   'HCOM_DEF.FOR'
C
C Translate the logical name DISPLAY_DEVICE to find which display is to be
C driven. Set up flags and logical name for appropriate display.
C Also put appropriate messages in the log file.
C
      CALL GET_LOGICAL('DISPLAY_DEVICE',DSPDEV) !Translate log name for display name
      IF(DSPDEV.EQ.' ') THEN
        TYPE *,'Logical name DISPLAY_DEVICE must be defined.'
        CALL EXIT
      END IF
      CALL LOGMESS('H_DISPLAY_'//DSPDEV(5:6)//' starting')
      TYPE *,' '
C
C Assign a channel to the TT: port, this
C will be used for keyboard/pad input.  If the device is a terminal server
C port then a connection to the port also has to be established.
C
      ISTAT=SYS$ASSIGN('TT:',ICHAN,,) !Now assign for output
      IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      ISTAT=LIB$GETDVI(DVI$_DEVNAM,ICHAN,,,CNAME,ILEN) !Get device name
      IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
!
! The following line is required when using a full Wave license.
!
      CALL CWAVEFOR(2,1,'print," "',9) !Start Wave before INPUT_AST
C
      CALL INPUT_AST                   !Enable keyboard input
C
      CALL UPDT_STATUS(0)              !Initialise argument lists
C
C Set up for output to VT type screens.  LUN 10 is used to do any ordinary 
C FORTRAN output to X terms.
C
      CALL VT_OUT('INITIALISE')
C
C Initialize graphics system, define the colour maps as appropriate for
C colour or monochrome displays, and also read in the plotting constants
C used by the various display routines. 
C
      CALL GRFINI(DSPDEV)
C
C Now enter the main loop where all options are handled.
C
      DO WHILE(.TRUE.)
        CALL GET_FUNC(IFUNC)
        IF(IFUNC.EQ.1) THEN
          CALL HOR_PLOT(IFUNC)
        ELSE IF(IFUNC.EQ.2) THEN
          CALL HOR_PLOT(IFUNC)
        ELSE IF(IFUNC.EQ.3) THEN
          CALL HOR_PLOT(IFUNC)
        ELSE IF(IFUNC.EQ.4) THEN
          CALL HOR_DUMP
        ELSE IF(IFUNC.EQ.5) THEN
          CALL HOR_TEPHI
        ELSE IF(IFUNC.EQ.6) THEN
!          CALL SAF_DUMP
        ELSE IF(IFUNC.EQ.7) THEN
!          CALL SAF_PLOT(IFUNC)
        ELSE IF(IFUNC.EQ.8) THEN
!          CALL SAF_PLOT(IFUNC)
        ELSE IF(IFUNC.EQ.9) THEN
        ELSE IF(IFUNC.EQ.10) THEN
        ELSE IF(IFUNC.EQ.11) THEN
          CALL HOR_HODO
        ELSE IF(IFUNC.EQ.12) THEN
          CALL HOR_TRACK
        ELSE IF(IFUNC.EQ.13) THEN
!          CALL MARSS_DUMP
        ELSE IF(IFUNC.EQ.14) THEN
!          CALL MARSS_PLOT(IFUNC)
        ELSE IF(IFUNC.EQ.15) THEN
!          CALL MARSS_PLOT(IFUNC)
        ELSE IF(IFUNC.EQ.16) THEN
!          CALL MARSS_PLOT(IFUNC)
        ELSE IF(IFUNC.EQ.17) THEN
!          CALL INST_MENU
        ELSE IF(IFUNC.EQ.18) THEN
          CALL CCN_MENU
        ELSE IF(IFUNC.EQ.19) THEN
          CALL CHEM_MENU
        ELSE IF(IFUNC.EQ.20) THEN
!          CALL DEIMOS_DUMP
        END IF
      END DO
C
      END
