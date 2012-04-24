      PROGRAM H_CNC
!
! Must be compiled with the file DRS_GDEF.FOR in the same directory.
!
! Link with:     $ LINK H_CNC,DRS_COM/OPT
!
! The program uses the logical name CNC_CHAN to identify the RS232 port
! through which to communicate with the CNC.  This port must have been set 
! during system startup with the following command:
!
! $ SET TERM CNC_CHAN /PERMANENT /EIGHTBIT /NOTTSYNC /PASTHRU /SPEED=9600
!
! The program operates continuously as follows:
!
! 1. Opens the RS232 channel to the CNC using the logical CNC_CHAN
!
! 2. Waits for the DRS program to start a new data block
!
! 3. Sends RDcr to the CNC to solicit a response
!
! 4. Waits up to 0.5s for a response
!
! 5. Decodes the response into a single 16 bit word and writes it directly to 
!    the DRS current data block in the correct position for parameter 50, as 
!    the following 4 char BCD string:
!
!    AAAA - No response from CNC
!    EEEE - CNC sent 'ERROR'
!    klmn - CNC count decodes as (k+.1l+.01m)*10**n
!
! V1.00  26/09/02  W.D.N.JACKSON
! V1.01  23/05/05  W.D.N.JACKSON
!        Now puts latest value into DRS block immediately it is opened in 
!        case the block is closed before a new value is obtained.
!
      IMPLICIT  NONE
      INTEGER*4 ISTS,IC,IW,IFUNC
      INTEGER*2 JCHAN,JOSB(4),JVAL,JOSFB(4)
      CHARACTER CLINE*80,CFLINE*80
      INCLUDE   'DRS_GDEF.FOR'
      INCLUDE   '($SSDEF)'
      INCLUDE   '($IODEF)'
      INCLUDE   '($SYSSRVNAM)'
      DATA      JVAL /'FFFF'X/

      CALL LOG_MESS('H_CNC starting') !Send message to log file
!
! Open channel to AVAPS and the AVSDAT.DAT files.  The 
! program aborts if it fails to open the AVAPS channel.
!
      CALL CONNECT(JCHAN,'CNC_CHAN',ISTS)
      IF(ISTS.EQ.0) CALL EXIT

      OPEN(UNIT=285,FILE='[HORACE]Bxxx_CNCFLOW.TXT',STATUS='NEW',
     &   CARRIAGECONTROL='LIST',RECL=1024)

!
! Start main loop
!
      IC=ICURTIM
      DO WHILE(.TRUE.)
        DO WHILE(IC.EQ.ICURTIM)
          CALL LIB$WAIT(0.05)
        END DO
        JDRSBLK(JISSBLK(50+1)/2+1)=JVAL !Use latest value to start with
        IC=ICURTIM
        CLINE=' '
        IFUNC=IO$_WRITEVBLK
        ISTS=SYS$QIOW(,%VAL(JCHAN),%VAL(IFUNC),JOSB,,,
     -      %REF('RD'//CHAR(13)),%VAL(3),,,,)
        IFUNC=IO$_READVBLK.OR.IO$M_NOECHO.OR.IO$M_PURGE.OR.IO$M_TIMED
        ISTS=SYS$QIO(,%VAL(JCHAN),%VAL(IFUNC),JOSB,,,
     -      %REF(CLINE),%VAL(80),%VAL(2),,,) !2s timeout is backup catchall
        IW=0
        DO WHILE(IW.LT.5.AND.JOSB(1).EQ.0)
          IW=IW+1
          CALL LIB$WAIT(0.1)
        END DO
        IF(JOSB(2).EQ.4) CLINE(6:6)='0'
        JVAL='EEEE'X
        IF(JOSB(1).EQ.0) THEN          !Timeout
          ISTS=SYS$CANCEL(%VAL(JCHAN))
          JVAL='AAAA'X
        ELSE IF(CLINE(1:5).EQ.'ERROR') THEN
          JVAL='EEEE'X
        ELSE IF(CLINE(1:1).GE.'0'.AND.CLINE(1:1).LE.'9'.AND.
     &      CLINE(3:4).GE.'00'.AND.CLINE(3:4).LE.'99'.AND.
     &      CLINE(6:6).GE.'0'.AND.CLINE(6:6).LE.'9') THEN
          CALL MVBITS(ICHAR(CLINE(1:1))-48,0,4,JVAL,12)
          CALL MVBITS(ICHAR(CLINE(3:3))-48,0,4,JVAL,8)
          CALL MVBITS(ICHAR(CLINE(4:4))-48,0,4,JVAL,4)
          CALL MVBITS(ICHAR(CLINE(6:6))-48,0,4,JVAL,0)
        END IF
        JDRSBLK(JISSBLK(50+1)/2+1)=JVAL
!
!     Added for AMS benefit
! 
!
        CFLINE=' '
        IFUNC=IO$_WRITEVBLK
        ISTS=SYS$QIOW(,%VAL(JCHAN),%VAL(IFUNC),JOSFB,,,
     -      %REF('R4'//CHAR(13)),%VAL(3),,,,)
        IFUNC=IO$_READVBLK.OR.IO$M_NOECHO.OR.IO$M_PURGE.OR.IO$M_TIMED
        ISTS=SYS$QIO(,%VAL(JCHAN),%VAL(IFUNC),JOSFB,,,
     -      %REF(CFLINE),%VAL(80),%VAL(2),,,) !2s timeout is backup catchall
        IW=0
        DO WHILE(IW.LT.2.AND.JOSFB(1).EQ.0)
          IW=IW+1
          CALL LIB$WAIT(0.1)
        END DO

        IF(JOSFB(1).EQ.0) THEN          !Timeout
          ISTS=SYS$CANCEL(%VAL(JCHAN))
          CFLINE='MISSED'
          JOSFB(2)=6
        ENDIF

        WRITE(285,'(I6.6,A)')
     &     ICURTIM,','//CLINE(1:JOSB(2))//','//CFLINE(1:JOSFB(2))

      END DO
      
      END
!*******************************************************************************
      SUBROUTINE LOG_MESS(MESSAGE)
!
! Writes message to log file consisting of program name, date, time and message.
!
! V1.00  08/02/99  W.D.N.JACKSON
!
      IMPLICIT NONE
      CHARACTER*(*) MESSAGE
      CHARACTER CDAT*9,CTIM*10
      CALL DATE1(CDAT)
      CALL TIME(CTIM)
      TYPE *,'H_CNC '//CDAT//' '//CTIM//MESSAGE
      RETURN
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
