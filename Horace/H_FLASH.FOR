      PROGRAM H_FLASH
C
C Sends the character A to the satcom flasher indicator via the RS232 port
C with the logical name FLASH_CHAN, whenever the H_SATCOM program sets bit 0
C in STATUS(46).
C
C Build with:
C 
C  $ FORT H_FLASH
C  $ LINK H_FLASH,H_COMMON/OPT
C
C    where H_COMMON.OPT contains the line SYS$LIBRARY:H_COMMON/SHAREABLE
C
C V1.00  02/02/02  W.D.N.JACKSON
C
      IMPLICIT NONE
      INTEGER*4 IOS
      INCLUDE 'HCOM_DEF.FOR'

      OPEN(UNIT=10,FILE='FLASH_CHAN',STATUS='OLD',IOSTAT=IOS)
      IF(IOS.EQ.0) CALL LOG_MESS('FLASH_CHAN is now open')
      IF(IOS.NE.0) CALL LOG_MESS('Unable to open FLASH_CHAN')
      DO WHILE(.TRUE.)
        IF(BTEST(STATUS(46),0)) THEN
          WRITE(10,'(A)',IOSTAT=IOS) CHAR(0)//'A'
          IF(IOS.NE.0) THEN
            CALL ST_MESS('Unable to write to FLASH_CHAN')
            CLOSE(10,IOSTAT=IOS)
            OPEN(UNIT=10,FILE='FLASH_CHAN',STATUS='OLD',IOSTAT=IOS)
          ELSE
            CALL LOG_MESS('Writing to FLASH_CHAN')
            CALL ST_MESS('Writing to FLASH_CHAN')
          END IF
        END IF
        CALL LIB$WAIT(1.0)
      END DO
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
      TYPE *,'H_FLASH '//CDAT//' '//CTIM//MESSAGE
      RETURN
      END
********************************************************************************
      SUBROUTINE ST_MESS(MESSAGE)
C
C Write message to the logical H_FLASH_STAT in the group logical name
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
      ISTAT=SYS$CRELNM(,'LNM$GROUP','H_FLASH_STAT',PSL$C_SUPER,TLIST)
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
