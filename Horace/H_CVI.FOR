C
      PROGRAM H_CVI
C
C Supplies a datastream to the CVI through the port defined by CVI_CHAN
C (normally port 8 on the HORNET terminal server), using 9600 baud, no 
C parity, 1 stop bit, no handshaking.
C
C Datastream comprises the following 40 character block, starting with
C an S and ending with a Z, sent once every 3 seconds:
C
C                Parameter     Resolution/Units
C   S sp 
C   xxxx sp      TAS           .1 m/s
C   xxxx sp      Pressure      1 mb
C   xxxx sp      True air temp .1 K
C   xxxx sp      Latitude      .1 deg (-900 to +900)         GPS lat
C   xxxx sp      Longitude     .1 deg (0000 to 3600)         GPS long
C   xxxx sp      Altitude      1 m                           Pressure height
C   xxxxxx sp    DRS time      hhmmss
C   Z
C
C Any value is set to zero if there is no valid data available.  If the 
C program can't open the output channel, it sends a message to the log file 
C and keeps trying until it can.  Note that TAS and TAT are not available on
C the ground.
C
C Build with:
C 
C  $ FORT H_CVI
C  $ LINK H_CVI,H_COMMON/OPT
C
C    where H_COMMON.OPT contains the line SYS$LIBRARY:H_COMMON/SHAREABLE
C
C V1.00  09/01/96  W.D.N.JACKSON
C
      IMPLICIT NONE
      INTEGER*4 IOS,IM,ITAS,ITAT,ILAT,ILNG,IPRS,IALT,ISEC,IMIN,ISECS,
     &    IHRS,IPTR
      LOGICAL*4 LOPEN,LERROR
      INCLUDE 'HCOM_DEF.FOR'
      CHARACTER CLINE*40,CTEMPL*40

      CTEMPL='S 0000 0000 0000 0000 0000 0000 000000 Z'
      LERROR=.FALSE.
      OPEN(UNIT=10,FILE='CVI_CHAN',STATUS='OLD',
     -    IOSTAT=IOS,CARRIAGECONTROL='NONE')
      IM=0
      IF(IOS.EQ.0) THEN
        CALL LOG_MESS('CVI_CHAN is now open')
        LOPEN=.TRUE.
      ELSE
        CALL LOG_MESS('Unable to open CVI_CHAN')
        LERROR=.TRUE.
        LOPEN=.FALSE.
      END IF
C
      DO WHILE(.TRUE.)
        IF(LOPEN) THEN
          CLINE=CTEMPL
          IPTR=STATUS(2)
          IF(STATUS(33).EQ.1.AND.IPTR.NE.0) THEN
            IF((RGINNV(IPTR)**2+RGINEV(IPTR)**2).GT.25.*25.) THEN !If > 50 kts
              ITAS=NINT(RTAS(IPTR)*10./1.944) !True airspeed in .1 m/s
              WRITE(CLINE(3:6),'(I4.4)',IOSTAT=IOS) ITAS
              ITAT=NINT(RTATDI(IPTR)*10.) !Deiced TAT in K
              WRITE(CLINE(13:16),'(I4.4)',IOSTAT=IOS) ITAT
            END IF
            IPRS=NINT(RSPR(IPTR))      !Static pressure in mb
            WRITE(CLINE(8:11),'(I4.4)',IOSTAT=IOS) IPRS
            IF(STATUS(35).EQ.1) THEN   !If GPS is valid
              ILAT=NINT(RGLAT(IPTR)*10)  
              IF(ILAT.GE.0) THEN
                WRITE(CLINE(18:21),'(I4.4)',IOSTAT=IOS) ILAT
              ELSE
                WRITE(CLINE(18:21),'(I4.3)',IOSTAT=IOS) ILAT
              END IF
              ILNG=NINT((RGLNG(IPTR))*10.)
              IF(ILNG.LT.0) ILNG=ILNG+3600
              WRITE(CLINE(23:26),'(I4.4)',IOSTAT=IOS) ILNG
            END IF
            IALT=NINT(RPHGT(IPTR))     !Pressure height in m
            WRITE(CLINE(28:31),'(I4.4)',IOSTAT=IOS) IALT
            ISECS=NINT(RSECS(IPTR))
            IHRS=ISECS/3600
            IMIN=MOD(ISECS/60,60)
            ISEC=MOD(MOD(ISECS,3600),60)
            WRITE(CLINE(33:38),'(3I2.2)',IOSTAT=IOS) IHRS,IMIN,ISEC
          END IF
          WRITE(10,'(A)',IOSTAT=IOS) CHAR(0)//CLINE
          IF(IOS.NE.0) THEN
            IF(.NOT.LERROR) CALL LOG_MESS
     -          ('Unable to write to CVI_CHAN')
            CALL ST_MESS('Unable to write to CVI')
            CLOSE(10,IOSTAT=IOS)
            LERROR=.TRUE.
            LOPEN=.FALSE.
            IM=0
          ELSE
            IM=IM+1
            IF(IM.EQ.1) THEN
              CALL LOG_MESS('Sending data to CVI')
              CALL ST_MESS('Sending data to CVI')
              LERROR=.FALSE.
            END IF
          END IF
          DO WHILE(IPTR.EQ.STATUS(2))  !Wait for next derivation to come in
            CALL LIB$WAIT(0.2)
          END DO
        ELSE
          DO WHILE(.NOT.LOPEN)
            CALL LIB$WAIT(60.0)
            OPEN(UNIT=10,FILE='CVI_CHAN',STATUS='OLD',
     -          IOSTAT=IOS,CARRIAGECONTROL='NONE')
            IF(IOS.EQ.0) LOPEN=.TRUE.
          END DO
        END IF
      END DO
10    FORMAT(A,I3.3,A,I5.5,A,A)
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
      TYPE *,'H_CVI '//CDAT//' '//CTIM//MESSAGE
      RETURN
      END
********************************************************************************
      SUBROUTINE    ST_MESS(MESSAGE)
C
C Write message to the logical H_CVI_STAT in the group logical name
C table.  GRPNAM and SYSNAM privileges are required.
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
      ISTAT=SYS$CRELNM(,'LNM$GROUP','H_CVI_STAT',PSL$C_SUPER,TLIST)
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
