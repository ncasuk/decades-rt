      PROGRAM H_TDL
C
C Sends true airspeed and pressure height values to the TDL
C roughly once every 10s.  The data is sent to the RS232/423 port with the
C logical name definition of TDL_CHAN, so this logical must be defined
C before this program is run.  The message consists of a 54 character string as
C follows:
C
C tttttt<sp>pppp.p<sp>ttt.t<sp>ttt.t<sp>sll.lllll<sp>slll.lllll<sp>hhhhh<cr><lf>
C
C where tttttt is time in secs past midnight, pppp.p is pressure in mb, ttt.t
C temperature in C, ttt.t is dew point in C, sll.lllll is GPS lat in 
C degrees, slll.lllll is GPS long in degrees, hhhhh is GPS height in m.
C The time between data is only
C approximate.  If there are no valid derivations available in the global
C common area, then no data are sent.  If the program can't open the output
C channel, it sends a message to the log file and keeps trying until it can.
C As well as checking that the measurements being used are current, it also
C checks that the measurements themselves are reasonable, and doesn't send
C them if they are not.
C
C Build with:
C 
C  $ FORT H_TDL
C  $ LINK H_TDL,H_COMMON/OPT
C
C    where H_COMMON.OPT contains the line SYS$LIBRARY:H_COMMON/SHAREABLE
C
C V1.00  04/02/94  W.D.N.JACKSON
C
      IMPLICIT NONE
      INTEGER*4 IOS,IM,JOS,KOS,IPTR,ITIM,IALT
      LOGICAL*4 LERROR,LOPEN
      REAL*4    RTEMP,RDEWP,RLAT,RLONG,RPRESS
      INCLUDE 'HCOM_DEF.FOR'

      LERROR=.FALSE.
      OPEN(UNIT=10,FILE='TDL_CHAN',STATUS='OLD',
     -    IOSTAT=IOS,CARRIAGECONTROL='NONE')
      IM=0
      IF(IOS.EQ.0) THEN
        CALL LOG_MESS('TDL_CHAN is now open')
        LOPEN=.TRUE.
      ELSE
        CALL LOG_MESS('Unable to open TDL_CHAN')
        LERROR=.TRUE.
        LOPEN=.FALSE.
      END IF
12    IF(STATUS(33).EQ.1.AND.LOPEN) THEN !If derivations are valid
        JOS=0
        KOS=0
        IPTR=STATUS(2)
C
        ITIM=NINT(RSECS(IPTR))         !Secs past midnight (s)
        RPRESS=RSPR(IPTR)              !RVSM static pressure (mb)
        RTEMP=RTATDC(IPTR)             !De-iced true temp (C)
        RDEWP=RDEW(IPTR)               !GE dewpoint (C)
        RLAT=RGLAT(IPTR)               !GPS lat (deg)
        RLONG=RGLNG(IPTR)              !GPS long (deg)
        IALT=NINT(RGALT(IPTR))         !GPS height (m)
C
        WRITE(10,10,IOSTAT=JOS) ITIM,RPRESS,RTEMP,RDEWP,RLAT,RLONG,IALT,
     -      CHAR(13),CHAR(10)
10      FORMAT(I6,F7.1,F6.1,F6.1,F10.5,F11.5,I6,A,A)
        IF(IOS.NE.0) THEN
          IF(.NOT.LERROR) CALL LOG_MESS
     -        ('Unable to write to TDL_CHAN')
          CALL ST_MESS('Unable to write to TDL')
          CLOSE(10,IOSTAT=IOS)
          LERROR=.TRUE.
          LOPEN=.FALSE.
          IM=0
        ELSE
          IM=IM+1
          IF(IM.EQ.1) THEN
            CALL LOG_MESS('Sending data to TDL')
            CALL ST_MESS('Sending data to TDL')
            LERROR=.FALSE.
          END IF
        END IF
      END IF
      IF(LOPEN) THEN
        CALL LIB$WAIT(10.0)
      ELSE
        DO WHILE(.NOT.LOPEN)
          CALL LIB$WAIT(60.0)
          OPEN(UNIT=10,FILE='TDL_CHAN',STATUS='OLD',
     -        IOSTAT=IOS,CARRIAGECONTROL='NONE')
          IF(IOS.EQ.0) THEN
            LOPEN=.TRUE.
          END IF
        END DO
      END IF
      GOTO 12
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
      TYPE *,'H_TDL '//CDAT//' '//CTIM//MESSAGE
      RETURN
      END
********************************************************************************
      SUBROUTINE    ST_MESS(MESSAGE)
C
C Write message to the logical H_TDL_STAT in the group logical name
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
      ISTAT=SYS$CRELNM(,'LNM$GROUP','H_TDL_STAT',PSL$C_SUPER,TLIST)
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
      CHARACTER CMON*3(12),CDATE*9,CDATE1*8,CDUMM1*10,CDUMM2*5
      DATA      CMON /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     &    'SEP','OCT','NOV','DEC'/

      CALL DATE_AND_TIME(CDATE1,CDUMM1,CDUMM2,IVAL)
      WRITE(CDATE,'(A)') CDATE1(7:8)//'-'//CMON(IVAL(2))//'-'//
     &    CDATE1(3:4)
      RETURN
      END
