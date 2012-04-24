      PROGRAM H_AVAPS_GET
C
C Must be compiled with the file HCOM_DEF.FOR in the same directory.
C
C Link with:     $ LINK H_AVAPS_GET
C
C The program uses the logical name AVAPS_RTD_CHAN to identify the RS232 port
C from which the AVAPS data is to be read.  This port must have been set during
C system startup with the following command:
C
C $ SET TERM AVAPS_GET_CHAN /PERMANENT /EIGHTBIT /NOTTSYNC /PASTHRU /SPEED=19200
C
C Note that no handshaking or flow control takes place, and there may be times
C when data is lost, though in practice this does not seem to occur very often.
C
C The program operates continuously as follows:
C
C 1. Opens the RS232 channel to AVPAS using the logical AVAPS_GET_CHAN
C
C 2. Opens a new AVSDAT.DAT file on hard disk.
C
C 3. Opens a new AVSDAT.DAT file on optical disk.  If it fails to open
C    these files it will keep trying as it carries out other operations.
C
C 4. Sends an initial line with the flight number, date and time
C

C 5. Collects all messages sent by AVAPS.
C
C 6. Writes the messages unaltered to the disk files, every minute.
C
C V1.00  21/01/99  W.D.N.JACKSON
C V1.01  03/10/02  W.D.N.JACKSON
C        Now gets flight number from FLIGHT_NUMBER logical so flight letter is
C        correct.
C
      IMPLICIT  NONE
      CHARACTER CFLTNO*4,CDATE*9,CTIME*8,CSYSSTS*12,COPTSTS*12
      CHARACTER CLINE*150
      INTEGER*4 ISTS,IL,IOS,IOSS,IOSO,ITIMTIME,ILINCNT,IDATOVR
      INTEGER*4 ISYSOUT,IOPTOUT,IST,ITIMOTIM
      INTEGER*2 JCHAN
      LOGICAL*4 LVALDRS,LFSTTRYO,LFSTTRYS
      REAL*4    RLSTTIME,RLSTOUT
      INCLUDE   'HCOM_DEF.FOR'
      INCLUDE   '($SSDEF)'

      CALL LOG_MESS('H_AVAPS_GET starting') !Send message to log file
C
C Open channel to AVAPS and the AVSDAT.DAT files.  The 
C program aborts if it fails to open the AVAPS channel.
C
      CALL CONNECT(JCHAN,'AVAPS_GET_CHAN',ISTS)
      IF(ISTS.EQ.0) CALL EXIT
      ITIMOTIM=600
      CALL READLINE(JCHAN,ITIMOTIM,ISTS,CLINE,IL) !First read has long timeout
C
C Initialize values
C
      ITIMTIME=0
      RLSTTIME=0
      ILINCNT=0
      IDATOVR=0
      ISYSOUT=0
      IOPTOUT=0
      RLSTOUT=SECNDS(0.)
      LFSTTRYS=.TRUE.
      LFSTTRYO=.TRUE.
      IOSS=1
      IOSO=1
C
C Start main loop
C
      DO WHILE(.TRUE.)
C
C First check the status of the disks, and close the AVSDAT.DAT files if some
C other process on the system has set the relevant logical name to
C 'Unavailable'.
C
        IST=ISTS
        IF(SECNDS(RLSTTIME).GT.2..AND.(IOSS.EQ.0.OR.IOSO.EQ.0)) THEN
          CALL GET_DISK_STATUS(CSYSSTS,COPTSTS) !Check disk availability
          IF(CSYSSTS.EQ.'Unavailable') THEN !If system disk unavailable
            IF(IOSS.EQ.0) THEN
              CLOSE(11,IOSTAT=IOS)
              IOSS=1                   !Show no open file on system disk
              CALL LOG_MESS
     -          ('HORACE:AVSDAT.DAT closed because disk unavailable')
            END IF
          END IF
          IF(COPTSTS.EQ.'Unavailable') THEN !If optical disk unavailable
            IF(IOSO.EQ.0) THEN
              CLOSE(12,IOSTAT=IOS)
              IOSO=1                   !Show no open file on optical disk
              CALL LOG_MESS
     -          ('DUB0:AVSDAT.DAT closed because disk unavailable')
            END IF
          END IF
          RLSTTIME=SECNDS(0.)
        END IF
C
C Now look at the result of the previous read and decide how to handle it
C
        IF(IST.EQ.1) THEN              !Successful read
C
C If either of the output files are closed try to open them
C
          IF(IOSS.NE.0.AND.CSYSSTS.NE.'Unavailable') THEN !If can open sys file
            OPEN(11,FILE='HORACE:AVSDAT.DAT',STATUS='NEW',IOSTAT=IOSS,
     -          CARRIAGECONTROL='LIST',RECL=150)
            IF(IOSS.EQ.0) THEN         !If successful
              CALL LOG_MESS('HORACE:AVSDAT.DAT opened')
              CALL DRS_DECODE_BLK(CFLTNO,CDATE,CTIME,LVALDRS) !Get DRS info
              IF(LVALDRS) THEN         !Will be false if no DRS info in message
                WRITE(11,'(A)',IOSTAT=IOSS) CFLTNO//' '//CDATE//' '//
     -              CTIME
                IF(IOSS.NE.0) THEN
                  CLOSE(11,IOSTAT=IOS)   !Close file if write fail
                  CALL LOG_MESS
     -                ('HORACE:AVSDAT.DAT closed after write failure')
                END IF
              END IF
            END IF
          END IF
          IF(IOSS.NE.0.AND.LFSTTRYS)
     -        CALL LOG_MESS('Unable to open HORACE:AVSDAT.DAT')
          LFSTTRYS=.FALSE.
          IF(IOSO.NE.0.AND.COPTSTS.NE.'Unavailable') THEN !If can open opt file
            OPEN(12,FILE='DUB0:[HORACE]AVSDAT.DAT',STATUS='NEW',
     -          IOSTAT=IOSO,CARRIAGECONTROL='LIST',RECL=150)
            IF(IOSO.EQ.0) THEN         !If successful
              CALL LOG_MESS('DUB0:AVSDAT.DAT opened')
              CALL DRS_DECODE_BLK(CFLTNO,CDATE,CTIME,LVALDRS) !Get DRS info
              IF(LVALDRS) THEN         !Will be false if no DRS info in message
                WRITE(12,'(A)',IOSTAT=IOSO) CFLTNO//' '//CDATE//' '//
     -              CTIME
                IF(IOSO.NE.0) THEN
                  CLOSE(12,IOSTAT=IOS)   !Close file if write fail
                  CALL LOG_MESS
     -                ('DUB0:AVSDAT.DAT closed after write failure')
                END IF
              END IF
            END IF
          END IF
          IF(IOSO.NE.0.AND.LFSTTRYO) 
     -        CALL LOG_MESS('Unable to open DUB0:AVSDAT.DAT')
          LFSTTRYO=.FALSE.
C
C Print out key messages - beginning and end of data, launch, and the aircraft
C data line.
C
          ILINCNT=ILINCNT+1
        IF((IL.GE.13.AND.(CLINE(11:13).EQ.'STA'.OR.CLINE(11:13).EQ.'LAU'
     -     .OR.CLINE(11:13).EQ.'END')).OR.(IL.GE.11.AND.CLINE(11:11).EQ.
     -     'A')) CALL LOG_MESS(CLINE(1:IL))
C
C Now write the data if the output files are open
C
          IF(IOSS.EQ.0) THEN
            IF(IL.GT.0) THEN
              WRITE(11,'(A)',IOSTAT=IOSS) CLINE(1:MIN(IL,150))
            ELSE IF(IL.EQ.0) THEN
              WRITE(11,'(A)',IOSTAT=IOSS)
            END IF
            IF(IOSS.NE.0) THEN
              CLOSE(11,IOSTAT=IOS)
              CALL LOG_MESS
     -            ('HORACE:AVSDAT.DAT closed after write error')
            ELSE
              ISYSOUT=ISYSOUT+1
            END IF
          END IF
          IF(IOSO.EQ.0) THEN
            IF(IL.GT.0) THEN
              WRITE(12,'(A)',IOSTAT=IOSO) CLINE(1:MIN(IL,150))
            ELSE IF(IL.EQ.0) THEN
              WRITE(12,'(A)',IOSTAT=IOSO)
            END IF
            IF(IOSO.NE.0) THEN
              CLOSE(12,IOSTAT=IOS)
              CALL LOG_MESS
     -            ('DUB0:AVSDAT.DAT closed after write error')
            ELSE
              IOPTOUT=IOPTOUT+1
            END IF
          END IF
          ITIMTIME=0
          ITIMOTIM=2
          CALL READLINE(JCHAN,ITIMOTIM,ISTS,CLINE,IL) !Next read
        ELSE IF(IST.EQ.SS$_TIMEOUT) THEN
          ITIMTIME=ITIMTIME+2
          IF(ITIMTIME.GT.300) THEN     !5 mins since last good read
            IF(IOSS.EQ.0) THEN
              CLOSE(11,IOSTAT=IOS)
              IOSS=1                   !Show no open file on system disk
              CALL LOG_MESS
     -          ('HORACE:AVSDAT.DAT closed - no data for 5 mins')
            END IF
            IF(IOSO.EQ.0) THEN
              CLOSE(12,IOSTAT=IOS)
              IOSO=1                   !Show no open file on optical disk
              CALL LOG_MESS
     -          ('DUB0:AVSDAT.DAT closed - no data for 5 mins')
            END IF
            ITIMOTIM=600               !Read with long timeout
          ELSE
            ITIMOTIM=2
          END IF
          CALL READLINE(JCHAN,ITIMOTIM,ISTS,CLINE,IL)
        ELSE IF(IST.EQ.SS$_DATAOVERUN) THEN
          IDATOVR=IDATOVR+1
          ITIMTIME=0
          ITIMOTIM=2
          CALL READLINE(JCHAN,ITIMOTIM,ISTS,CLINE,IL)
        ELSE IF(IST.EQ.716) THEN
          CLOSE(11,IOSTAT=IOS)         !Close system file if open
          CLOSE(12,IOSTAT=IOS)         !Close optical file if open
          CALL DISCONNECT(JCHAN)
          CALL LOG_MESS('Data set hang-up')
          CALL EXIT
        ELSE
          CLOSE(11,IOSTAT=IOS)         !Close system file if open
          CLOSE(12,IOSTAT=IOS)         !Close optical file if open
          CALL DISCONNECT(JCHAN)
          CALL LOG_MESS('Aborting on unknown read error')
          TYPE *,IST
          CALL EXIT
        END IF
        IF(SECNDS(RLSTOUT).GE.600.) THEN
C          WRITE(CLINE1,'(4(A,I5))',IOSTAT=IOS) 'Read: ',ILINCNT,
C     -        ' Sys: ',ISYSOUT,' Opt: ',IOPTOUT,' Ovr: ',IDATOVR
C          CALL LOG_MESS(CLINE1(1:LAN(CLINE1)))
          RLSTOUT=SECNDS(0.)
        END IF        
      END DO
      END
********************************************************************************
      SUBROUTINE LOG_MESS(MESSAGE)
C
C Writes message to log file consisting of program name, date, time and message.
C
C V1.00  08/02/99  W.D.N.JACKSON
C
      IMPLICIT NONE
      CHARACTER*(*) MESSAGE
      CHARACTER CDAT*9,CTIM*10
      CALL DATE1(CDAT)
      CALL TIME(CTIM)
      TYPE *,'H_AVAPS_GET '//CDAT//' '//CTIM//MESSAGE
      RETURN
      END
********************************************************************************
      SUBROUTINE DRS_DECODE_BLK(CFLTNO,CDATE,CTIME,LVALID)
C
C Decodes a DRS block for flight number, date and time.  If if the block is
C invalid returns spaces and LVALID FALSE, else returns with LVALID TRUE.
C
C V1.00  08/02/99  W.D.N.JACKSON
C
      IMPLICIT  NONE
      LOGICAL*4 LVALID
      INTEGER*4 J1,J2,JFLT,JJDAY,JJMON,JJYER,IOS,JJHRS,JJMIN,JJSEC,IS
      CHARACTER CFLTNO*4,CDATE*9,CTIME*8
      INCLUDE 'HCOM_DEF.FOR'
      CHARACTER*3 CMON(12) /'JAN','FEB','MAR','APR','MAY','JUN',
     -    'JUL','AUG','SEP','OCT','NOV','DEC'/

      CFLTNO=' '
      CDATE=' '
      CTIME=' '
      LVALID=.FALSE.
      IF(STATUS(33).NE.1) RETURN       !Return if DRS data is not current
      IS=STATUS(1)
      IF(IS.LT.1.OR.IS.GT.2) RETURN
      J1=NFDATA(9,IS)                  !Work out flight number
      JFLT=IBITS(J1,8,4)*100+IBITS(J1,4,4)*10+IBITS(J1,0,4)
      CALL GET_LOGICAL('FLIGHT_NUMBER',CFLTNO)
!      CFLTNO='Axxx'
!      WRITE(CFLTNO(2:4),10,IOSTAT=IOS) JFLT
      J1=NFDATA(5,IS)                  !Work out DRS date
      J2=NFDATA(6,IS)
      JJDAY=IBITS(J1,8,4)*10+IBITS(J1,4,4)
      JJMON=IBITS(J1,0,4)*10+IBITS(J2,8,4)
      JJYER=IBITS(J2,4,4)*10+IBITS(J2,0,4)
      CDATE='xx-xxx-xx'
      IF(JJMON.LT.1.OR.JJMON.GT.12) RETURN
      WRITE(CDATE(1:2),11,IOSTAT=IOS) JJDAY
      CDATE(4:6)=CMON(JJMON)
      WRITE(CDATE(8:9),11,IOSTAT=IOS) JJYER
      J1=NFDATA(10,IS)                 !Work out DRS time
      J2=NFDATA(11,IS)
      JJHRS=IBITS(J1,8,4)*10+IBITS(J1,4,4)
      JJMIN=IBITS(J1,0,4)*10+IBITS(J2,8,4)
      JJSEC=IBITS(J2,4,4)*10+IBITS(J2,0,4)
      CTIME='xx:xx:xx'
      WRITE(CTIME(1:2),11,IOSTAT=IOS) JJHRS
      WRITE(CTIME(4:5),11,IOSTAT=IOS) JJMIN
      WRITE(CTIME(7:8),11,IOSTAT=IOS) JJSEC
      LVALID=.TRUE.
      RETURN
10    FORMAT(I3.3)
11    FORMAT(I2.2)
      END
********************************************************************************
      SUBROUTINE GET_DISK_STATUS(CSYSSTS,COPTSTS)
C
C Reads the group logical names OPTI_STATUS and SYST_STATUS
C
C V1.00  08/02/99  W.D.N.JACKSON
C
      IMPLICIT NONE
      CHARACTER*(*) CSYSSTS,COPTSTS
      CHARACTER*20 LOGNAME
      INTEGER*4 ILIST(4),ISTAT,NAMLEN
      INTEGER*2 JLIST(8)
      EQUIVALENCE (ILIST,JLIST)
      INCLUDE '($SYSSRVNAM)'
      INCLUDE '($LNMDEF)'

      CSYSSTS=' '
      COPTSTS=' '
      JLIST(1)=20
      JLIST(2)=LNM$_STRING
      ILIST(2)=%LOC(LOGNAME)
      ILIST(3)=%LOC(NAMLEN)
      ILIST(4)=0
      ISTAT=SYS$TRNLNM(,'LNM$GROUP','SYST_STATUS',,ILIST)
      IF(ISTAT) CSYSSTS=LOGNAME(1:MAX(NAMLEN,1))
      ISTAT=SYS$TRNLNM(,'LNM$GROUP','OPTI_STATUS',,ILIST)
      IF(ISTAT) COPTSTS=LOGNAME(1:MAX(NAMLEN,1))
      RETURN
10    FORMAT(3I)
      END
!*******************************************************************************
!
! ROUTINE          CONNECT SUBROUTINE FORTVAX
!
! PURPOSE          Opens a VMS channel for getting messages from AVAPS system.
!
! DESCRIPTION      Establishes a VMS channel to the AVAPS system
!                  via an RS232 port on the system.  Returns with ISTS zero if
!                  fails to allocate the port, or cannot assign a channel. 
!                  Else 1.  Note that the terminal line used must be
!                  allocatable, and must have its characteristics set with the
!                  following:
!
!                  /EIGHTBIT /NOTTSYNC /PASTHRU /SPEED=(9600,9600)
!
!                  Note also that any terminal server must be set for remote
!                  access, ie CHANGE PORT n ACCESS REMOTE
!
! VERSION          1.00  08-2-99  W.D.N.JACKSON
!
! ARGUMENTS        JCHAN       I*4  OUT  The assigned channel number
!                  CNAME       C**  IN   Name of RS232 port to AVAPS
!                  ISTS        I*4  OUT  1 if successful, else 0
!
!*******************************************************************************
      SUBROUTINE CONNECT(JCHAN,CNAME,ISTS)
      IMPLICIT  NONE
      CHARACTER CNAME*(*),CNAME1*80
      INTEGER*2 JCHAN,JOSB(4)
      INTEGER*4 ISTAT,IFUNCT,ISTS,ILEN,LIB$GETDVI,LAN
      INCLUDE   '($IODEF)'
      INCLUDE   '($DVIDEF)'
      INCLUDE   '($SYSSRVNAM)'
!
! Try to open channel to the processor.
!
      ISTAT=SYS$ALLOC(CNAME,,,,)     !Allocate the serial port
      IF(.NOT.ISTAT) CALL LOG_MESS('Unable to allocate port '//
     &      CNAME(1:LAN(CNAME)))
      IF(ISTAT) THEN
        ISTAT=SYS$ASSIGN(CNAME,JCHAN,,) !Now assign a channel to it
        IF(.NOT.ISTAT) CALL LOG_MESS
     &      ('Unable to assign channel to port '//
     &      CNAME(1:LAN(CNAME)))
      END IF
      IF(ISTAT) THEN
        ISTAT=LIB$GETDVI(DVI$_DEVNAM,JCHAN,,,CNAME1,ILEN) !Get device name
        IF(.NOT.ISTAT) CALL LOG_MESS
     &        ('Unable to get device information '//
     &        CNAME1(1:LAN(CNAME1)))
      END IF
      IF(CNAME1(2:4).EQ.'LTA'.AND.ISTAT) THEN   !If terminal server
        IFUNCT=IO$_TTY_PORT.OR.IO$M_LT_CONNECT !Connect function
        ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFUNCT),JOSB(1)
     &        ,,,,,,,,)                !Connect
        IF(.NOT.JOSB(1).OR..NOT.ISTAT) THEN
          CALL LOG_MESS
     &        ('Unable to connect to terminal server '//
     &        CNAME1(1:LAN(CNAME1)))
          ISTAT=.FALSE.
        END IF
      END IF
      IF(.NOT.ISTAT) THEN
        ISTAT=SYS$CANCEL(%VAL(JCHAN))
        IF(CNAME1(2:4).EQ.'LTA') THEN !If terminal server
          IFUNCT=IO$_TTY_PORT.OR.IO$M_LT_DISCON !Disconnect function
          ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFUNCT),JOSB(1)
     &          ,,,,,,,,)              !Disconnect
        END IF
        ISTAT=SYS$DASSGN(%VAL(JCHAN))
        ISTAT=SYS$DALLOC(CNAME1,)
        ISTS=0
      ELSE
        CALL LOG_MESS('Output port opened '//CNAME(1:LAN(CNAME)))
        ISTS=1
      END IF
!
      RETURN
      END
!*******************************************************************************
      SUBROUTINE DISCONNECT(JCHAN)
!
! Cancels any outstanding I/O on the AVAPS channel and then frees the channel.
!
      IMPLICIT  NONE
      INTEGER*2 JCHAN
      INTEGER*4 ISTAT
      INCLUDE   '($SYSSRVNAM)'
   
      ISTAT=SYS$CANCEL(%VAL(JCHAN))
      ISTAT=SYS$DASSGN(%VAL(JCHAN))
      RETURN
      END
!*******************************************************************************
!
! ROUTINE          LAN FUNCTION FORTVAX
!
! PURPOSE          Returns length of a string less trailing blanks
!
! DESCRIPTION      Function returns the length of a string not including
!                  trailing blanks. If the string is all blanks, or is of zero
!                  length, the function returns a value of 1.
!
! VERSION          1.00  9-1-90  W.D.N.JACKSON
!
! ARGUMENTS        CSTRING  C**  IN   String whose length to be found
!                  LAN      I*4  OUT  String length less trailing spaces
!
!*******************************************************************************
      INTEGER*4 FUNCTION LAN(CSTRING)
      IMPLICIT  NONE
      CHARACTER CSTRING*(*)
      INTEGER*4 I

      I=LEN(CSTRING)                   !Get string length
      DO WHILE(I.GT.0.AND.CSTRING(I:I).EQ.' ') !Step back over spaces
        I=I-1
      END DO
      IF(I.EQ.0) I=1                   !Special case
      LAN=I
      RETURN
      END
!*******************************************************************************
      SUBROUTINE READLINE(JCHAN,ITIMEOUT,ISTS,CLINE,IL)
C
C Reads a line from the input port, with timeout defined by ITIMEOUT, and
C return the line in CLINE, with its length in IL.  ISTS contains the resulting
C read status which may be 1 if OK, or any other error value.  Assumes line
C is terminated by CR/LF combination
C
      IMPLICIT NONE
      CHARACTER CLINE*(*)
      INTEGER*4 MASK(2)/2*0/,ISTS,IL,SYS$QIOW,ITIMEOUT,IFUNC
      INTEGER*2 IOSB(4),JCHAN
      INCLUDE '($IODEF)'

      MASK(2)=IBSET(0,10)              !Terminate read on LF only
      IL=0
      ISTS=1
      IFUNC=IO$_READVBLK.OR.IO$M_TIMED.OR.IO$M_NOECHO
     -    .OR.IO$M_NOFILTR
      ISTS=SYS$QIOW(,%VAL(JCHAN),%VAL(IFUNC),IOSB,,,
     -    %REF(CLINE),%VAL(150),%VAL(ITIMEOUT),%VAL(%LOC(MASK)),,)
      IF(.NOT.ISTS) GOTO 10
      ISTS=IOSB(1)
      IF(ISTS.NE.1) GOTO 10
      IL=IOSB(2)
      IF(IL.GE.1.AND.CLINE(IL:IL).EQ.CHAR(13)) IL=IL-1 !Strip CR
10    RETURN
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
      SUBROUTINE GET_LOGICAL(CLOGICAL,CRESULT)
!
! Translate the logical name specified by CLOGICAL and returns the result in
! CRESULT.  Returns with blank if not found.  Searches all logical name tables
! and returns the first (lowest) definition found.
!
      IMPLICIT    NONE
      CHARACTER   CLOGICAL*(*),CRESULT*(*)
      INTEGER*4   ILIST(4),NAMLEN
      INTEGER*2   JLIST(8)
      EQUIVALENCE (ILIST,JLIST)
      INCLUDE     '($LNMDEF)'
!
      CRESULT=' '                      !Will contain the translation 
      JLIST(1)=LEN(CRESULT)            !Length of string space
      JLIST(2)=LNM$_STRING             !Required function
      ILIST(2)=%LOC(CRESULT)           !String location
      ILIST(3)=%LOC(NAMLEN)            !Will have string length
      ILIST(4)=0                       !End of list
      CALL SYS$TRNLNM(,'LNM$DCL_LOGICAL',CLOGICAL,,ILIST)
      RETURN
      END
