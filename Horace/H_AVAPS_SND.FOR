      PROGRAM H_AVAPS_SND
C
C Sends aircraft flight data to the AVAPS system every 1s.
C The data consists of an 106 character string as defined in the AVAPS
C interface control document.
C The data consists of 15 groups each followed by a space, with the
C end of the line marked by a cr/lf.  Note that the ICD calls for 999s to be
C used when data is invalid.  This program does no validity checking and
C assumes that when the data is used (immediately before launch) all the 
C aircraft data systems are working.
C
C The data are sent to the RS232/423 port with the logical name definition of
C AVAPS_SND_CHAN, so this logical must be defined before this program is run.
C
C If the link to the PC is lost, the program keeps retrying until is becomes
C available.
C
C Build with:
C 
C  $ FORT H_AVAPS_SND
C  $ LINK H_AVAPS_SND,H_COMMON/OPT
C
C    where H_COMMON.OPT contains the line SYS$LIBRARY:H_COMMON/SHAREABLE
C
C V1.00  20/01/99  W.D.N.JACKSON
C
      IMPLICIT  NONE
      INTEGER*4 IM,IOS,IPTR,I,IL,IMM,IT
      LOGICAL*4 LOPEN
      REAL*4    RLNG,RLAT,RALT
      CHARACTER CLINE*120,CDATE*9,CD*2,CY*4,CMTHS(12)*3
      DATA CMTHS /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP',
     -    'OCT','NOV','DEC'/
      INCLUDE 'HCOM_DEF.FOR'

      CALL LOG_MESS('H_AVAPS_SND starting')
      IT=0
      IPTR=STATUS(2)
C
C Loop indefinitely, checking 5 times a second for new derivations to become
C available.
C
      DO WHILE(.TRUE.)
C
        DO WHILE(.NOT.LOPEN)         !Try opening connection once a minute
          CALL LIB$WAIT(60.0)
          OPEN(UNIT=10,FILE='AVAPS_SND_CHAN',STATUS='NEW',
     -          IOSTAT=IOS,CARRIAGECONTROL='NONE')
          IF(IOS.EQ.0) THEN
            CALL LOG_MESS('AVAPS_SND_CHAN is now open')
            LOPEN=.TRUE.
            IM=0
          ELSE
            IF(IT.EQ.0) CALL LOG_MESS('Unable to open AVAPS_SND_CHAN')
            IT=IT+1
          END IF
        END DO
C
        DO WHILE(STATUS(34).EQ.0.OR.IPTR.EQ.STATUS(2)) !Wait for new derivations
          CALL LIB$WAIT(0.2)
        END DO
C
        IPTR=STATUS(2)
        CALL DATE1(CDATE)
        CD=CDATE(1:2)
        CY='19'//CDATE(8:9)
        IF(CY.LT.'1998') CY(1:2)='20'
        DO I=1,12
          IF(CDATE(4:6).EQ.CMTHS(I)) IMM=I
        END DO
        RLNG=999.
        RLAT=99.
        RALT=99999.
        IF(STATUS(35).EQ.1) THEN     !If GPS is valid
          RLNG=RGLNG(IPTR)
          RLAT=RGLAT(IPTR)
          RALT=RGALT(IPTR)             !WGS84 height
        END IF
        WRITE(CLINE,10,IOSTAT=IOS)
     -      CY,IMM,CD,
     -      (SPECIAL(IPTR,I),I=1,3),
     -      RSPR(IPTR),
     -      RTATDC(IPTR),
     -      RDEW(IPTR),
     -      RIWA(IPTR),
     -      RIWS(IPTR),
     -      RIHDG(IPTR),
     -      RTAS(IPTR)/1.944,        !kts to m/s
     -      RIHDG(IPTR)+RIDA(IPTR),  !Track
     -      RIGS(IPTR),
     -      RW(IPTR),
     -      RLNG,
     -      RLAT,
     -      RALT,                    !WGS84 height
     -      RPHGT(IPTR),             !Geopotential (pressure) height
     -      CHAR(13)//CHAR(10)
10    FORMAT(A,X,I2.2,X,A,X,3(I2.2,X),F6.1,X,F5.1,X,F6.1,X,7(F5.1,X),
     -      F9.4,X,F8.4,X,2(F7.1,X),A)
        CALL COMPRESS(CLINE,120,IL)
        WRITE(10,'(A)',IOSTAT=IOS) CLINE(1:IL)
        IF(IOS.NE.0) THEN
          CALL LOG_MESS('Unable to write to AVAPS_SND_CHAN')
          CLOSE(10,IOSTAT=IOS)
          IT=0
          LOPEN=.FALSE.
        ELSE
          IM=IM+1
          IF(IM.EQ.1) CALL LOG_MESS('Sending data to AVAPS_SND_CHAN')
        END IF
      END DO
      END   
!*******************************************************************************
      SUBROUTINE LOG_MESS(MESSAGE)
!
! Writes message to log file consisting of program name, date, time and message.
!
      IMPLICIT  NONE
      CHARACTER MESSAGE*(*),CDAT*9,CTIM*10

      CALL DATE1(CDAT)
      CALL TIME(CTIM)
      TYPE *,'H_AVAPS_SND '//CDAT//' '//CTIM//MESSAGE
      RETURN
      END
!*******************************************************************************
      SUBROUTINE COMPRESS(CLINE,IS,IL)
!
! Removes multiple spaces from a string
!
      IMPLICIT  NONE
      INTEGER*4 I,IS,IL
      CHARACTER CLINE*120,CTEMP*120

      CTEMP=CLINE
      IL=1
      DO I=2,IS
        IF(CTEMP(I:I).NE.' ') THEN
          IL=IL+1
          CLINE(IL:IL)=CTEMP(I:I)
        ELSE IF(CLINE(IL:IL).NE.' ') THEN
          IL=IL+1
          CLINE(IL:IL)=' '
        END IF
      END DO
      IF(CLINE(IL:IL).EQ.' ') IL=IL-1
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
