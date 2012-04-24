C
C H_DERIVE.FOR
c
C Reads in calibration constants required for the derivations.
C Carries out the calculation of derived parameters using the raw DRS data,
C once every 3 seconds, and the calculation of DEIMOS, MARSS, and CCN derived 
C parameters once every second.
C MARSS parameters are stored in common block once every 3 seconds(calculated
C internally in MARSS_CALCS).
C CCN results go into the CCNCOM common block.
C Calculates solar zenith and azimuth once every 120 secs. 
C Calculates Heimann corrections once a second by filling lookup tables when
C the Heimann is on calibrate.
C This program should be run at a fairly high priority.
C This main program is only a controller; the actual calculations are done by 
C the HOR_CALCS, HEIM_CALCS, and CCN_CALCS subroutines.
C
C Build command:
C
C    $ LINK H_DERIVE,HORACE.OLB/LIB,H_COMMON/OPT
C
C V1.00  27/01/89  W.D.N.JACKSON
C
C  Changes  01   27/01/89      Original version
C           02   17/04/89      Read calibration constants into subroutine
C           04   18/04/90      HEIM_CALCS etc 
C           07   31/05/91      CCN_CALCS provision (WDNJ)
C           08   17/02/92      Store program start time in STATUS(31&32)
C           09   07/08/92      Now sets EFs for two more displays
C                              Now uses watchdog timer and STATUS(34) to show if
C                              derivations are out of date
C
      PROGRAM H_DERIVE
      IMPLICIT  NONE
      INTEGER*4 ITIMINT(2),ISTAT,I,ITRIES,INXT,J2,JSECS,ICNT,IPTR
      CHARACTER CLOG*3
      INCLUDE   'HCOM_DEF.FOR'
      INCLUDE   '($SYSSRVNAM)'
      EXTERNAL  WATCHDOG
C
C Start by associating a name with a common event flag cluster.  This
C cluster is common to all processes with the same UIC group number.
C
C The flags are used as follows:
C
C  Flag  64        Set by H_DRS_SIM or H_DRS_LOG when a new DRS data block 
C                  is in global common. Used and cleared by H_DERIVE.
C  Flag  65        Set by H_DRS_SIM or H_DRS_LOG when a new DRS data block 
C                  is in global common. Used and cleared by H_TAPE.
C  Flag  66        Set by H_DRS_SIM or H_DRS_LOG when an ISS data block 
C                  is in global common.  Used and cleared by H_ISS_GET.
C
C Status Words are used as follows:
C            Type    Value       Written by 
C  STATUS(1)  I*2    1 or 2      H_DRS_LOG   Pointer to buffer with raw DRS data
C                                H_DRS_SIM
C  STATUS(2)  I*2    1-->12800   H_DERIVE    Pointer into DERIVE for latest 
C                                            set of derivations
C  STATUS(3)  I*2    0-->12799   H_DERIVE    Number of sets of derivations 
C                                            in DERIVE array
C  STATUS(4)  I*2    1-->12800   H_DERIVE    Pointer into SAFPAR for latest 
C                                            set of derivations
C  STATUS(5)  I*2    0-->12799   H_DERIVE    Number of sets of derivations
C                                            in SAFPAR array 
C  STATUS(13) I*2    0/1/2       HEIM_CALCS  Pointer to current competed 
C                                            Heimann lookup table, 0-initially
C  STATUS(31,32) I*4             H_DERIVE    Program start time in spm
C  STATUS(34) I*2    0/1         H_DERIVE    1 if DERIVE values valid, else 0
C
      CALL GET_LOGICAL('H_DERIVE_RESET',CLOG)
      IF(CLOG(1:1).EQ.'Y') THEN
        STATUS(55)=0
        STATUS(56)=0
        STATUS(57)=0
        STATUS(58)=0
      END IF
      CALL STR_TIME(STATUS(31))        !Note program starting time
      CALL LOG_MESS('H_DERIVE starting') !Put message in log file
      CALL ST_MESS('Starting')         !Set status logical name value
      ISTAT=SYS$ASCEFC(%VAL(64),'H_CEF_CLUSTER',,) !Associate with CEF cluster
      IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      ISTAT=SYS$BINTIM('0000 00:00:04.00',ITIMINT) !Use 4s watchdog timeout
c
c Read in calibration constants.
C
      CALL READ_CONSTS                 !Standard constants from HOR_CALIB.DAT
      CALL READ_CCN_CONSTS             !CCN constants from CCN_CALIB.DAT
      CALL WEB_DUMP('INIT')            !Put derived param names on web page
C
C Get ISS block contents off disc.  Derivations can't be done until the arrays
C NPLOC and NPFREQ have been filled, so the program sits in a loop, retrying
C every time a new DRS data block becomes available
C
      I=0                              !Will be 1 when have ISS info
      CALL LOG_MESS('Trying for ISS block') !Update messages
      CALL ST_MESS('Trying for ISS block')
      ITRIES=0
      DO WHILE(I.EQ.0)
        ISTAT=SYS$CLREF(%VAL(64))
        IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
        ISTAT=SYS$WAITFR(%VAL(64))     !Wait for next DRS data block
        IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
        CALL READ_ISS(I)               !Try for ISS data
        ITRIES=ITRIES+1
        IF(ITRIES.EQ.2) CALL HM_MESS('There is no ISS file on disk.'//
     -      '  H_DERIVE is suspended')
      END DO
      CALL LOG_MESS('ISS read')        !Show we got it
      IF(ITRIES.GT.1) CALL HM_MESS('H_DERIVE now has ISS file')
C
C Do CCN calculations every 1 sec
C Calculate Heimann correction lookup tables every 1sec if PRT4 on calibrate
C Do standard derivations every 3 sec
C Calculate solar azimuth and albedo every 120 secs as part of standard derivs
C
      STATUS(34)=0                     !No valid derivations yet
      CALL LOG_MESS('Starting derivations') !Update messages
      CALL ST_MESS('Carrying out derivations')
      DO WHILE(.TRUE.)                 !Loop indefinitely
        ISTAT=SYS$CLREF(%VAL(64))      !Clear event flag
        IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
        ISTAT=SYS$WAITFR(%VAL(64))     !Wait for next DRS data block
        IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
        IPTR=STATUS(1)                 !Latest DRS buffer
        CALL UPDATE_1SPTR(IPTR,INXT,ICNT) !Update 1s pointer
        CALL CCN_CALCS                 !Do CCN calculations
        CALL HEIM_CALCS                !Fill lookup table 
        STATUS(5)=ICNT                 !Allow others to use data
        STATUS(4)=INXT                 !Allow others to use data
        J2=NFDATA(NPLOC(3),STATUS(1))  !Pointer to DRS time
        JSECS=IBITS(J2,4,4)*10+IBITS(J2,0,4) !Compute DRS seconds
        IF(MOD(JSECS,3).EQ.0) THEN     !Test for every 3rd sec
          CALL UPDATE_3SPTR(IPTR,INXT,ICNT) !Update 3s pointer
          CALL HOR_CALCS(INXT)         !Derive standard paras
          STATUS(3)=ICNT               !Allow others to use data
          STATUS(2)=INXT               !Allow others to use data
          STATUS(34)=1                 !Show derivations are up to date
          ISTAT=SYS$CANTIM(,)          !Cancel last watchdog period
          ISTAT=SYS$SETIMR(,ITIMINT,WATCHDOG,,) !Set timer for 4s
          CALL WEB_DUMP('DATA')        !Put data on web page
        END IF
      END DO                           !Go to wait for next DRS data block
      END
C*******************************************************************************
      SUBROUTINE WATCHDOG
C
C Watchdog routine, called asynchronously if the 4s watchdog timer expires.  If
C this happens, this routine sets STATUS(34) to 0 to show that the derivations
C are no longer up to date.
C
      IMPLICIT  NONE
      INCLUDE   'HCOM_DEF.FOR'

      STATUS(34)=0
      RETURN
      END
C*******************************************************************************
      SUBROUTINE READ_ISS(ISTAT)
C
C Given a block of DRS data in NFDATA, works out the DRS's ISS number and
C reads in the ISS contents from the appropiate file with a name of ISS_nn.DAT,
C where nn is the ISS number.  If the DRS data block doesn't have valid data,
C or the corresponding ISS file is not present on disk, then returns with ISTAT
C equal to 0.  If the ISS is successfully read, and its contents transferred to
C NPLOC and NPFREQ, then returns with ISTAT equal to 1.
C
C Note that this routine does not interpret DRS ISS blocks directly.  When the
C ISS is changed then the H_ISS_GET process is responsible for creating the new
C ISS file, after which this routine will read it back and use it.
C
C V1.00  01/01/89  W.D.N.JACKSON
C V1.01  13/06/91  W.D.N.JACKSON
C        Now returns with ISTAT=0 if the right ISS file is not present or it
C        fails to read it, rather than aborting the program.
C
      IMPLICIT  NONE
      INTEGER*4 ISTAT,ISS,ICALLS,IOS,J,JJ
      INTEGER*2 IDATA(768)
      CHARACTER CISSF*10
      INCLUDE   'HCOM_DEF.FOR'
      DATA      ICALLS /0/
C
C Read the ISS data and fill NPLOC and NPFREQ
C
      ISS=(NFDATA(1,STATUS(1)).AND.'FFF'X) !First word of DRS block is ISS  
      ISTAT=1                          !Default is good return (ISTAT=1)
      IF(ISS.LE.0.OR.ISS.GT.999) GOTO 20 !Make sure it is valid number
      CISSF='ISSxxx.DAT'               !Form ISS file name
      WRITE(CISSF(4:6),10) ISS
      ICALLS=ICALLS+1                  !No of times this routine called
      IF(ICALLS.EQ.1) CALL LOG_MESS('Waiting to read '//CISSF)
      OPEN(UNIT=10,FILE=CISSF,STATUS='OLD',READONLY,ACCESS='DIRECT',
     -    RECL=384,IOSTAT=IOS)         !Open it if present
      IF(IOS.NE.0) GOTO 20             !Return with error if open fails
      READ(UNIT=10,REC=1,IOSTAT=IOS) (IDATA(J),J=1,768) !Read the record
      CLOSE(UNIT=10)                   !Close the file
      IF(IOS.NE.0) GOTO 20             !Return with error if read fails
      DO J=1,512                       !Clear arrays
        NPLOC(J)=0
        NPFREQ(J)=0
      END DO
      ISTAT=1
      DO J=2,512+1                     !For each possible parameter
        IF(IDATA(J).EQ.'FFF'X) RETURN  !Return with ISTAT=1 if end of data
        IF(IDATA(J).LT.0.OR.IDATA(J).GT.3968) !Abort if data invalid
     -      GOTO 20
        IF(IDATA(J).NE.0) THEN         !If parameter recorded
          NPLOC(J-1)=IDATA(J)/2+1      !Turn byte offset to words
          JJ=J
          DO WHILE(IDATA(JJ+1).EQ.0)   !Search for next offset
            JJ=JJ+1
          END DO
          NPFREQ(J-1)=(IDATA(JJ+1)-IDATA(J))/2 !Calc sampling rate
        END IF
      END DO
   20 ISTAT=0                          !Error return if ISTAT=0
      RETURN
   10 FORMAT(I3.3)
      END
C*******************************************************************************
      SUBROUTINE LOG_MESS(MESSAGE)
C
C Writes message to log file consisting of program name, date, time and message.
C
C V1.00 01/01/89  W.D.N.JACKSON
C
      IMPLICIT NONE
      CHARACTER*(*) MESSAGE
      CHARACTER CDAT*9,CTIM*10
      CALL DATE1(CDAT)
      CALL TIME(CTIM)
      TYPE *,'H_DERIVE '//CDAT//' '//CTIM//MESSAGE
      RETURN
      END
C*******************************************************************************
      SUBROUTINE    ST_MESS(MESSAGE)
C
C Write message to the logical H_DERIVE_STAT in the group logical name
C table.  GRPNAM and SYSNAM privileges are required.
C
C V1.00 01/01/89  W.D.N.JACKSON
C
      IMPLICIT      NONE
      INTEGER*4     ISTAT
      CHARACTER*(*) MESSAGE
      INCLUDE       '($SYSSRVNAM)'
      INCLUDE       '($LNMDEF)'
      INCLUDE       '($PSLDEF)'

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
      ISTAT=SYS$CRELNM(,'LNM$GROUP','H_DERIVE_STAT',PSL$C_SUPER,TLIST)
      RETURN
      END
C*******************************************************************************
      SUBROUTINE STR_TIME(JSTORE)
C
C Stores the current time as a 32 bit integer seconds past midnight.
C
C V1.00         W.D.N.JACKSON
C
      IMPLICIT  NONE
      INTEGER*4 ITEMP
      INTEGER*2 JSTORE(2),JTEMP(2)
      EQUIVALENCE (ITEMP,JTEMP)
      ITEMP=NINT(SECNDS(0.0))
      JSTORE(1)=JTEMP(1)
      JSTORE(2)=JTEMP(2)
      RETURN
      END
C*******************************************************************************
      SUBROUTINE HM_MESS(CMESS)
C
C Writes an immediate message to the Flight Leader's display, and also stores
C it in the shared file HORACE:H_MESSAGES.LOG.  These messages should only be
C sent when there is a problem or potential problem which must be drawn to
C the Flight Leader's attention.  OPER privilege is required.  Note that this
C routine prefaces the message with the time that it was received by the
C H_MESSAGE process, so there is no need for users to time stamp their
C messages.
C
      IMPLICIT      NONE
      INTEGER*4     IFUNCT,ISTAT,MLEN
      CHARACTER*(*) CMESS
      CHARACTER     CMESSA*72,CERRMESS*132
      INTEGER*2     IOSB(4),ICHAN
      INTEGER*4     IFIRST/1/
      INCLUDE       '($SYSSRVNAM)'
      INCLUDE       '($IODEF)'

      CMESSA=CMESS
      IF(IFIRST.EQ.1) THEN
        ISTAT=SYS$ASSIGN('H_MESSAGE_MBX',ICHAN,,)
        IF(.NOT.ISTAT) THEN            !Don't abort program on error
          CALL SYS$GETMSG(%VAL(ISTAT),MLEN,CERRMESS,%VAL(1),) !Just print message
          CALL LOG_MESS(CERRMESS(1:MLEN))
          RETURN
        ELSE
          IFIRST=0
        END IF
      END IF
      IFUNCT=IO$_WRITEVBLK
      ISTAT=SYS$QIO(,%VAL(ICHAN),%VAL(IFUNCT),IOSB,,,
     -    %REF(CMESSA),%VAL(72),,,,)
      IF(.NOT.ISTAT) THEN              !Don't abort program on error
        CALL SYS$GETMSG(%VAL(ISTAT),MLEN,CERRMESS,%VAL(1),) !Just print message
        CALL LOG_MESS(CERRMESS(1:MLEN))
      END IF
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
!*******************************************************************************
      SUBROUTINE UPDATE_3SPTR(IPTR,INXT,ICNT)
!
! Update the 3s data pointer when a new DRS data block is received.  If the
! block does not follow on immediately from the previous data, missing data
! is filled with zeroes.
!
      IMPLICIT  NONE
      INTEGER*4 IYTIM,ILSTYTIM,I,J,INXT,ITMPNXT,ICNT,ITMPCNT,ITIM,IPTR
      INTEGER*4 J1,J2,JJHRS,JJMIN,JJSEC,IOS
      REAL*4    RTIM
      CHARACTER CSTIM*8,CETIM*8
      INCLUDE   'HCOM_DEF.FOR'
      EQUIVALENCE (ILSTYTIM,STATUS(55))

      CALL YEARTIME(NFDATA(1,IPTR),IYTIM)
      ITMPNXT=STATUS(2)
      ITMPCNT=STATUS(3)
      IF(IYTIM.LT.ILSTYTIM.OR.(IYTIM-ILSTYTIM).GT.12800*3.OR.
     -    ILSTYTIM.EQ.0) THEN          !Start anew if back in time
        DO I=1,196
          DO J=1,12800
            DERIVE(J,I)=0.
          END DO
        END DO
        DO I=1,10
          DO J=1,12800
            SPECIAL(J,I)=0
          END DO
        END DO
        ITMPNXT=12800                  !Latest derivation (1 to 12800)
        ITMPCNT=0                      !No of derivations in array (0 to 12799)
        IF(IYTIM.LT.ILSTYTIM) 
     -      CALL LOG_MESS('3s data buffers zeroed - time backwards')
        IF(IYTIM-ILSTYTIM.GT.12800*3) 
     -      CALL LOG_MESS('3s data buffers zeroed - more than 10 hours')
        IF(ILSTYTIM.EQ.0) 
     -      CALL LOG_MESS('3s data buffers zeroed - last time zero')
      ELSE IF(IYTIM.EQ.ILSTYTIM) THEN  !Ignore if time the same
        CALL LOG_MESS('Repeated 3s data time ignored')
      ELSE IF(IYTIM.GT.ILSTYTIM+3) THEN !Step over missing data
        CSTIM='xx:xx:xx'
        CETIM='xx:xx:xx'
        RTIM=DERIVE(ITMPNXT,3)
        DO WHILE(IYTIM.GT.ILSTYTIM+3)
          ITMPNXT=ITMPNXT+1
          IF(ITMPNXT.GT.12800) ITMPNXT=1
          ITMPCNT=ITMPCNT+1
          IF(ITMPCNT.GE.12800) ITMPCNT=12799
          DO I=1,196
            DERIVE(ITMPNXT,I)=0.
          END DO
          RTIM=RTIM+3.
          DERIVE(ITMPNXT,3)=RTIM
          DO I=1,10
            SPECIAL(ITMPNXT,I)=0
          END DO
          ITIM=NINT(RTIM)
          SPECIAL(ITMPNXT,1)=ITIM/3600
          SPECIAL(ITMPNXT,2)=MOD(ITIM/60,60)
          SPECIAL(ITMPNXT,3)=MOD(MOD(ITIM,3600),60)
          SPECIAL(ITMPNXT,9)=0         !Show data invalid
          ILSTYTIM=ILSTYTIM+3
          IF(CSTIM(1:2).EQ.'xx') THEN
            WRITE(CSTIM(1:2),'(I2.2)',IOSTAT=IOS) SPECIAL(ITMPNXT,1)
            WRITE(CSTIM(4:5),'(I2.2)',IOSTAT=IOS) SPECIAL(ITMPNXT,2)
            WRITE(CSTIM(7:8),'(I2.2)',IOSTAT=IOS) SPECIAL(ITMPNXT,3)
          END IF
          WRITE(CETIM(1:2),'(I2.2)',IOSTAT=IOS) SPECIAL(ITMPNXT,1)
          WRITE(CETIM(4:5),'(I2.2)',IOSTAT=IOS) SPECIAL(ITMPNXT,2)
          WRITE(CETIM(7:8),'(I2.2)',IOSTAT=IOS) SPECIAL(ITMPNXT,3)
        END DO
        CALL LOG_MESS('Gap filled in 3s data - '//CSTIM//' to '//CETIM)
      END IF
      ILSTYTIM=IYTIM
      INXT=ITMPNXT+1
      IF(INXT.GT.12800) INXT=1
      ICNT=ITMPCNT+1
      IF(ICNT.GE.12800) ICNT=12799
      J1=NFDATA(NPLOC(2),IPTR)
      J2=NFDATA(NPLOC(3),IPTR)
      JJHRS=IBITS(J1,8,4)*10+IBITS(J1,4,4) !DRS hours
      JJMIN=IBITS(J1,0,4)*10+IBITS(J2,8,4) !DRS minutes
      JJSEC=IBITS(J2,4,4)*10+IBITS(J2,0,4) !DRS seconds
      SPECIAL(INXT,1)=JJHRS
      SPECIAL(INXT,2)=JJMIN
      SPECIAL(INXT,3)=JJSEC
      SPECIAL(INXT,9)=1                !This data is valid
      DERIVE(INXT,3)=3600*JJHRS+60*JJMIN+JJSEC
      RETURN
      END
!*******************************************************************************
      SUBROUTINE UPDATE_1SPTR(IPTR,INXT,ICNT)
!
! Update the 1s data pointer when a new DRS data block is received.  If the
! block does not follow on immediately from the previous data, missing data
! is filled with zeroes.
!
      IMPLICIT  NONE
!      INTEGER*4 IYTIM,ILSTYTIM,I,J,INXT,ITMPNXT,ICNT,ITMPCNT,IPTR,IOS,
      INTEGER*4 IYTIM,ILSTYTIM,INXT,ITMPNXT,ICNT,ITMPCNT,IPTR,IOS,
     -    IFIL
!      REAL*4    SAFPAR(12800,100)
!      REAL*4    MARSSPAR(12800,60)
!      REAL*4    DEIMOSPAR(12800,115)
      CHARACTER CGAP*5
!      COMMON    /SAFCOM/ SAFPAR
!      COMMON    /MARSSCOM/ MARSSPAR
!      COMMON    /DEIMOSCOM/ DEIMOSPAR
      INCLUDE   'HCOM_DEF.FOR'
      EQUIVALENCE (ILSTYTIM,STATUS(57))

      CALL YEARTIME(NFDATA(1,IPTR),IYTIM)
      ITMPNXT=STATUS(4)
      ITMPCNT=STATUS(5)
      IF(IYTIM.LT.ILSTYTIM.OR.(IYTIM-ILSTYTIM).GT.12800.OR.
     -    ILSTYTIM.EQ.0) THEN          !Start anew if back in time
!        DO I=1,100
!          DO J=1,12800
!            SAFPAR(J,I)=0.
!          END DO
!        END DO
!        DO I=1,60
!          DO J=1,12800
!            MARSSPAR(J,I)=0.
!          END DO
!        END DO
!        DO I=1,115
!          DO J=1,12800
!            DEIMOSPAR(J,I)=0.
!          END DO
!        END DO
        ITMPNXT=12800                  !Latest derivation (1 to 12800)
        ITMPCNT=0                      !No of derivations in array (0 to 12799)
        IF(IYTIM.LT.ILSTYTIM) 
     -      CALL LOG_MESS('1s data buffers zeroed - time backwards')
        IF(IYTIM-ILSTYTIM.GT.12800) 
     -     CALL LOG_MESS('1s data buffers zeroed - more than 3.5 hours')
        IF(ILSTYTIM.EQ.0) 
     -      CALL LOG_MESS('1s data buffers zeroed - last time zero')
      ELSE IF(IYTIM.EQ.ILSTYTIM) THEN  !Ignore if time the same
        CALL LOG_MESS('Repeated 1s data time ignored')
      ELSE IF(IYTIM.GT.ILSTYTIM+1) THEN !Step over missing data
        IFIL=0
        DO WHILE(IYTIM.GT.ILSTYTIM+1)
          ITMPNXT=ITMPNXT+1
          IF(ITMPNXT.GT.12800) ITMPNXT=1
          ITMPCNT=ITMPCNT+1
          IF(ITMPCNT.GE.12800) ITMPCNT=12799
!          DO I=1,100
!            SAFPAR(ITMPNXT,I)=0.
!          END DO
!          DO I=1,60
!            MARSSPAR(ITMPNXT,I)=0.
!          END DO
!          DO I=1,115
!            DEIMOSPAR(ITMPNXT,I)=0.
!          END DO
          ILSTYTIM=ILSTYTIM+1
          IFIL=IFIL+1
        END DO
        WRITE(CGAP,'(I5)',IOSTAT=IOS) IFIL
        CALL LOG_MESS('1s data gap filled - '//CGAP//'s')
      END IF
      ILSTYTIM=IYTIM
      INXT=ITMPNXT+1
      IF(INXT.GT.12800) INXT=1
      ICNT=ITMPCNT+1
      IF(ICNT.GE.12800) ICNT=12799
      RETURN
      END
!*******************************************************************************
      SUBROUTINE YEARTIME(NDATA,ITIM)
C
C Given a block of DRS data, returns the time as seconds
C from the beginning of the year.  The date used is based on the HORACE date
C since the DRS date does not roll over at midnight.  The times have 1 added
C to them so that 0 can represent an invalid time.
C
      IMPLICIT   NONE
      INTEGER*2  NDATA(2048)
      INTEGER*4  J1,J2,JHRS,JMIN,JSEC,JSTAT,LIB$CVT_FROM_INTERNAL_TIME
      INTEGER*4  ITIM,IDRSSEC,IHORSEC,IDAY
      INCLUDE    '($LIBDTDEF)'

      J1=NDATA(10).AND.'FFF'X          !Work out DRS time
      J2=NDATA(11).AND.'FFF'X
      JHRS=IBITS(J1,8,4)*10+IBITS(J1,4,4)
      JMIN=IBITS(J1,0,4)*10+IBITS(J2,8,4)
      JSEC=IBITS(J2,4,4)*10+IBITS(J2,0,4)
      IDRSSEC=JHRS*3600+JMIN*60+JSEC
      JSTAT=LIB$CVT_FROM_INTERNAL_TIME(LIB$K_DAY_OF_YEAR,IDAY)
      JSTAT=LIB$CVT_FROM_INTERNAL_TIME(LIB$K_SECOND_OF_DAY,IHORSEC)
      IF((IDRSSEC-IHORSEC).GT.10) IDAY=IDAY-1 !Catch difference across mdnght
      IF((IHORSEC-IDRSSEC).GT.10) IDAY=IDAY+1
      ITIM=IDAY*24*3600+IDRSSEC+1
      RETURN
      END
C*******************************************************************************
      SUBROUTINE GET_LOGICAL(CLOGICAL,CRESULT)
C
C Translate the logical name specified by CLOGICAL and returns the result in
C CRESULT.  Returns with blank if not found.  Searches all logical name tables
C and returns the first (lowest) definition found.
C
      IMPLICIT    NONE
      CHARACTER   CLOGICAL*(*),CRESULT*(*)
      INTEGER*4   ILIST(4),NAMLEN
      INTEGER*2   JLIST(8)
      EQUIVALENCE (ILIST,JLIST)
      INCLUDE     '($LNMDEF)'
C
      CRESULT=' '                      !Will contain the translation 
      JLIST(1)=LEN(CRESULT)            !Length of string space
      JLIST(2)=LNM$_STRING             !Required function
      ILIST(2)=%LOC(CRESULT)           !String location
      ILIST(3)=%LOC(NAMLEN)            !Will have string length
      ILIST(4)=0                       !End of list
      CALL SYS$TRNLNM(,'LNM$DCL_LOGICAL',CLOGICAL,,ILIST)
      RETURN
      END
C*******************************************************************************
      SUBROUTINE WEB_DUMP(CMODE)
!
! Creates text file/web page in the web accessible [HORACE.WEB] directory.
! If CMODE is INIT then creates a DERIVED_PARA.TXT page with the parameter
! numbers, names and units.  If CMODE is DATA then creates a DERIVED_DATA.TXT
! page with the parameter numbers and values, every 3 s.  The pages are pure
! text with no attempt at formatting.  Uses [HORACE]PARANO.TXT as source of
! derived parameter definitions.
! 
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
          OPEN(10,FILE='[HORACE.WEB]DERIVED_PARA.TXT',STATUS='UNKNOWN',
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
      RETURN
      END
