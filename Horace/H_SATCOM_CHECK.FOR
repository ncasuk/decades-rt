      PROGRAM H_SATCOM_CHECK
!
! Checks the SAT_INP_STS.DAT and SAT_OUT_STS.DAT for any inconsistencies.  If
! any inconsistencies are found then opens a new file with zero entries.
!
! V1.00  14/03/96  W.D.N.JACKSON
!
      IMPLICIT  NONE
      INTEGER*4 IOS,ICOUNT,IFIRST,I,ILAST,IQUEUE
      LOGICAL*4 LERR,LEXISTS
      CHARACTER CLINE*80,CFILE*80
!
! First check the input status file
!
      LERR=.FALSE.
      CALL LOG_MESS('Checking input file')
      OPEN(10,FILE='[.SATCOM]SAT_INP_STS.DAT;',READONLY,STATUS='OLD',
     &    ACCESS='DIRECT',RECL=20,IOSTAT=IOS) !Open input file
      IF(IOS.NE.0) THEN
        LERR=.TRUE.
        CALL LOG_MESS('Failed to open input file')
      END IF
      READ(10,REC=1,IOSTAT=IOS) CLINE  !Read first input record
      IF(IOS.NE.0) THEN
        IF(.NOT.LERR) CALL LOG_MESS('Failed to read first record')
        LERR=.TRUE.
      END IF
      READ(CLINE(1:5),'(I5)',IOSTAT=IOS) ICOUNT !Get no of records in file
      IF(IOS.NE.0) THEN
        IF(.NOT.LERR) CALL LOG_MESS('Failed to get number of records')
        LERR=.TRUE.
      END IF
      IF(ICOUNT.GT.0) THEN
        READ(10,REC=2,IOSTAT=IOS) CLINE !Get no of first input file
        IF(IOS.NE.0) THEN
          IF(.NOT.LERR) CALL LOG_MESS('Failed to get first input file')
          LERR=.TRUE.
        END IF
        READ(CLINE(1:5),'(I5)',IOSTAT=IOS) IFIRST
        IF(IOS.NE.0) THEN
          IF(.NOT.LERR) CALL LOG_MESS('Failed first input file decode')
          LERR=.TRUE.
        END IF
        ILAST=IFIRST+ICOUNT-1          !Work out last file
        CFILE='[.SATCOM]SAT_INP_nnnnn.DAT' !Template for file names
        DO I=IFIRST,ILAST              !Check each file and note first existing
          WRITE(CFILE(18:22),'(I5.5)',IOSTAT=IOS) I !Form file name
          IF(IOS.NE.0) THEN
            IF(.NOT.LERR) CALL LOG_MESS('Failed to form file name')
            LERR=.TRUE.
          END IF
          INQUIRE(FILE=CFILE,EXIST=LEXISTS,IOSTAT=IOS) !See if it exists
          IF(IOS.NE.0) THEN
            IF(.NOT.LERR) CALL LOG_MESS('Failed inquire')
            LERR=.TRUE.
          END IF
          IF(.NOT.LEXISTS) THEN
            IF(.NOT.LERR) CALL LOG_MESS('Output file missing')
            LERR=.TRUE.
          END IF
        END DO
      END IF
      CLOSE(10,IOSTAT=IOS)
      IF(IOS.NE.0) LERR=.TRUE.
      IF(LERR) THEN
        OPEN(10,FILE='[.SATCOM]SAT_INP_STS.DAT;',STATUS='NEW',
     &      ACCESS='DIRECT',RECL=20,IOSTAT=IOS) !Open input file
        CLINE='00000'
        WRITE(10,REC=1,IOSTAT=IOS) CLINE
        IF(IOS.EQ.0) THEN
          CALL LOG_MESS('New SAT_INP_STS.DAT file created')
        ELSE
          CALL LOG_MESS('Failed to create a new SAT_INP_STS.DAT file')
        END IF
        CLOSE(10,IOSTAT=IOS)
      END IF
!
! Now check the output status file
!
      LERR=.FALSE.
      CALL LOG_MESS('Checking output file')
      OPEN(10,FILE='[.SATCOM]SAT_OUT_STS.DAT;',READONLY,STATUS='OLD',
     &    ACCESS='DIRECT',RECL=20,IOSTAT=IOS) !Open output file
      IF(IOS.NE.0) THEN
        LERR=.TRUE.
        CALL LOG_MESS('Failed to open output file')
      END IF
      READ(10,REC=1,IOSTAT=IOS) CLINE  !Read first output record
      IF(IOS.NE.0) THEN
        IF(.NOT.LERR) CALL LOG_MESS('Failed to read first record')
        LERR=.TRUE.
      END IF
      READ(CLINE(1:5),'(I5)',IOSTAT=IOS) ICOUNT !Get no of records in file
      IF(IOS.NE.0) THEN
        IF(.NOT.LERR) CALL LOG_MESS('Failed to get number of records')
        LERR=.TRUE.
      END IF
      READ(CLINE(7:11),'(I5)',IOSTAT=IOS) IQUEUE !Get first queued record
      IF(IOS.NE.0) THEN
        IF(.NOT.LERR) CALL LOG_MESS('Failed to get first queued record')
        LERR=.TRUE.
      END IF
      IF(IQUEUE.LT.0.OR.IQUEUE.GT.ICOUNT) THEN
        IF(.NOT.LERR) CALL LOG_MESS('Queued record invalid')
        LERR=.TRUE.
      END IF
      IF(ICOUNT.GT.0) THEN
        READ(10,REC=2,IOSTAT=IOS) CLINE !Get no of first input file
        IF(IOS.NE.0) THEN
          IF(.NOT.LERR) CALL LOG_MESS('Failed to get first output file')
          LERR=.TRUE.
        END IF
        READ(CLINE(1:5),'(I5)',IOSTAT=IOS) IFIRST
        IF(IOS.NE.0) THEN
          IF(.NOT.LERR) CALL LOG_MESS('Failed first output file decode')
          LERR=.TRUE.
        END IF
        ILAST=IFIRST+ICOUNT-1          !Work out last file
        CFILE='[.SATCOM]SAT_OUT_nnnnn.DAT' !Template for file names
        DO I=IFIRST,ILAST              !Check each file and note first existing
          WRITE(CFILE(18:22),'(I5.5)',IOSTAT=IOS) I !Form file name
          IF(IOS.NE.0) THEN
            IF(.NOT.LERR) CALL LOG_MESS('Failed to form file name')
            LERR=.TRUE.
          END IF
          INQUIRE(FILE=CFILE,EXIST=LEXISTS,IOSTAT=IOS) !See if it exists
          IF(IOS.NE.0) THEN
            IF(.NOT.LERR) CALL LOG_MESS('Failed inquire')
            LERR=.TRUE.
          END IF
          IF(.NOT.LEXISTS) THEN
            IF(.NOT.LERR) CALL LOG_MESS('Output file missing')
            LERR=.TRUE.
          END IF
        END DO
      END IF
      CLOSE(10,IOSTAT=IOS)
      IF(IOS.NE.0) LERR=.TRUE.
      IF(LERR) THEN
        OPEN(10,FILE='[.SATCOM]SAT_OUT_STS.DAT;',STATUS='NEW',
     &      ACCESS='DIRECT',RECL=20,IOSTAT=IOS) !Open output file
        CLINE='00000 00000'
        WRITE(10,REC=1,IOSTAT=IOS) CLINE
        IF(IOS.EQ.0) THEN
          CALL LOG_MESS('New SAT_OUT_STS.DAT file created')
        ELSE
          CALL LOG_MESS('Failed to create a new SAT_OUT_STS.DAT file')
        END IF
        CLOSE(10,IOSTAT=IOS)
      END IF
!
      CALL EXIT
      END
!******************************************************************************
      SUBROUTINE LOG_MESS(MESSAGE)
!
! Writes message to log file consisting of program name, date, time and message.
!
      IMPLICIT  NONE
      CHARACTER MESSAGE*(*),CDAT*9,CTIM*10

      CALL DATE1(CDAT)
      CALL TIME(CTIM)
      TYPE *,'H_SATCOM '//CDAT//' '//CTIM//MESSAGE
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
