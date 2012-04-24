      PROGRAM H_MESSAGE
C
C Creates and opens the H_MESSAGE_MBX mailbox and then waits for other
C processes to send messages to this mailbox.  Once a valid message is received
C it is time stamped, written to the H_MESSAGES.LOG file, which can be read by
C other processes, and also sent to the flight leader's display.
C
C Build with:
C 
C  $ FORT H_MESSAGE
C  $ LINK H_MESSAGE,H_COMMON/OPT
C
C    where H_COMMON.OPT contains the line SYS$LIBRARY:H_COMMON/SHAREABLE
C
C V1.00  13/02/92  W.D.N.JACKSON
C
      IMPLICIT NONE
      INTEGER*4 ISTAT,IOS,LUN,MLUN,IMCNT,IGCNT,I,IBAD
      REAL*4  RLSTIM
      INCLUDE 'HCOM_DEF.FOR'
      INTEGER*2 ICHAN
      CHARACTER CMESS*80,CTIM*8,CSTRING*41
      INCLUDE '($SYSSRVNAM)'

      STATUS(16)=0                     !Count of the number of messages in file
      ISTAT=SYS$ASSIGN('H_MESSAGE_MBX',ICHAN,,)
      ISTAT=SYS$DELMBX(%VAL(ICHAN))    !Close any existing mailbox
      ISTAT=SYS$DASSGN(%VAL(ICHAN))
      ISTAT=SYS$CREMBX(%VAL(1),ICHAN,,,,,'H_MESSAGE_MBX') !Creat perm mailbox
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      CALL LIB$GET_LUN(LUN)            !Connect to mailbox
      OPEN(UNIT=LUN,FILE='H_MESSAGE_MBX',STATUS='OLD',
     -    CARRIAGECONTROL='LIST',IOSTAT=IOS)
      IF(IOS.NE.0) CALL LOG_MESS('Failed to open mailbox')
      CALL LIB$GET_LUN(MLUN)           !Open message file
      OPEN(UNIT=MLUN,FILE='HORACE:H_MESSAGES.LOG;',STATUS='NEW',
     -    CARRIAGECONTROL='LIST',SHARED,IOSTAT=IOS)
      IF(IOS.NE.0) CALL LOG_MESS('Failed to open H_MESSAGES.LOG')
      CALL LOG_MESS('Started collecting messages')
      IMCNT=0                          !Total messages
      IGCNT=0                          !Good messages
      RLSTIM=0.0                       !Time of last message
      DO WHILE(.TRUE.)
        CALL ST_MESS('Collecting messages')
        READ(LUN,10,IOSTAT=IOS) CMESS
        IMCNT=IMCNT+1
        IBAD=0                         !Check for null strings
        DO I=1,10
          IF(ICHAR(CMESS(I:I)).LT.32) IBAD=IBAD+1 !Count control chars
        END DO
        IF (CMESS.NE.' '.AND.IBAD.EQ.0.AND.IOS.EQ.0) THEN
          IGCNT=IGCNT+1
          CALL TIME(CTIM)
          TYPE *,CTIM//' '//CMESS(1:70)
          WRITE(MLUN,10,IOSTAT=IOS) CTIM//' '//CMESS(1:70)
          IF(IOS.EQ.0) STATUS(16)=STATUS(16)+1
          IF(SECNDS(RLSTIM).GT.600.) THEN !List if more than 10 mins since last
            WRITE(CSTRING,11,IOSTAT=IOS) IMCNT,IGCNT,STATUS(16)
            CALL LOG_MESS(CSTRING)
            RLSTIM=SECNDS(0.0)
          END IF
          CALL FL_MESS(CHAR(10)//CHAR(13)//CTIM//' '//CMESS(1:70)
     -        //CHAR(13))
        END IF
      END DO
10    FORMAT(A)
11    FORMAT('Read',I6,', Valid',I6,', Written',I6,'.')
      END   
********************************************************************************
      SUBROUTINE LOG_MESS(MESSAGE)
C
C Writes message to log file consisting of program name, date, time and message.
C
      IMPLICIT NONE
      CHARACTER*(*) MESSAGE
      CHARACTER CDAT*9,CTIM*10
      CALL DATE1(CDAT)
      CALL TIME(CTIM)
      TYPE *,'H_MESSAGE '//CDAT//' '//CTIM//MESSAGE
      RETURN
      END
********************************************************************************
      SUBROUTINE ST_MESS(MESSAGE)
C
C Write message to the logical H_MESSAGE_STAT in the group logical name
C table.  GRPNAM and SYSNAM privileges are required.
C
      IMPLICIT NONE
      INTEGER*4 ISTAT
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
      ISTAT=SYS$CRELNM(,'LNM$GROUP','H_MESSAGE_STAT',PSL$C_SUPER,TLIST)
      RETURN
      END
********************************************************************************
      SUBROUTINE FL_MESS(MESSAGE)
C
C Sends an immediate message to the flight leader's display.  The message 
C is only sent when any existing output ends, so it should not interfere
C with graphics displays.  OPER privilege is required.
C
      IMPLICIT  NONE
      INTEGER*4 ISTAT
      CHARACTER MESSAGE*(*)
      INCLUDE   '($SYSSRVNAM)'
      INCLUDE   '($BRKDEF)'

      ISTAT=SYS$BRKTHRU(,MESSAGE,'SBD_FL',%VAL(BRK$C_DEVICE),,,,,,,)
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
