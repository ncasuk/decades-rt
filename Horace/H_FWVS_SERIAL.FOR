      PROGRAM H_FWVS
!
! Must be compiled with the file DRS_GDEF.FOR in the same directory.
!
! Link with:     $ LINK H_FWVS,H_COMMON/OPT,DRS_COM/OPT
!
! The program uses the logical name FWVS_CHAN to identify the RS232 port
! through which to communicate with the FWVS.  This port must have been set 
! during system startup with the following command:
!
! $ SET TERM FWVS_CHAN /PERMANENT /EIGHTBIT /NOTTSYNC /PASTHRU /SPEED=9600
!
! The program operates continuously as follows:
!
! 1. Opens the RS232 channel to the FWVS using the logical FWVS_CHAN
!
! 2. Waits for the DRS program to start a new data block
!
! 3. Sends FDDDTTT to the FWVS to solicit a response, where DDD is GE dewpoint
!    and TTT is the de-iced temperature, both in Kelvin.
!
! 4. Waits up to 0.5s for a response of 64 hex ascii characters
!
! 5. Decodes the hex chars into 16 16 bit words and stores in DRS data block
!
! Note that if no data or incomplete data is returned then nothing is put in 
! the DRS block for that second and the parameter has the default value of
! FFFF.
!
! V1.00  03/11/04  W.D.N.JACKSON
! V1.01  16/11/04  W.D.N.JACKSON
!        Diagnostic messages improved.  Sets up read before sending the 'F'.
! V1.02  23/05/05  W.D.N.JACKSON
!        Now puts in latest values immediately new DRS blk is opened in case new
!        values are not obtained before the block is closed.
! V1.03  26/08/05  W.D.N.JACKSON
!        Now outputs GE dewpoint and de-iced temp to FWVS.
!
!
! v1.04  27 apr 09 Jim Crawford
!        test for ge flag commented out to force ge data to go to fwvs
!        even if ge not in control - allows fwvs to function in profiles
!
!
!
      IMPLICIT  NONE
      INTEGER*4 ISTS,IC,IW,IFUNC,I,IFWVS,ISTAT,IDP1,ITEMP,IOS
      INTEGER*2 JCHAN,JOSB(4),JVAL(16),JOSB1(4)
      CHARACTER CLINE*64,COUT*7
      INCLUDE   'HCOM_DEF.FOR'
      INCLUDE   'DRS_GDEF.FOR'
      INCLUDE   '($SSDEF)'
      INCLUDE   '($IODEF)'
      INCLUDE   '($SYSSRVNAM)'
      DATA      IFWVS /230/            !Multiplexed FWVS parameter number
      DATA      JVAL /16*'FFFF'X/
  
      CALL LOG_MESS('H_FWVS starting') !Send message to log file
!
! Open channel to FWVS.  The program aborts if it fails to open the FWVS 
! channel.
!
      CALL CONNECT(JCHAN,'FWVS_CHAN',ISTS)
      IF(ISTS.EQ.0) CALL EXIT
!
! Start main loop
!
      ISTAT=0
      IC=ICURTIM
      DO WHILE(.TRUE.)
        DO WHILE(IC.EQ.ICURTIM)
          CALL LIB$WAIT(0.05)
        END DO
        IC=ICURTIM
        DO I=1,16
          JDRSBLK(JISSBLK(IFWVS+1)/2+I)=JVAL(I)
        END DO        
        CLINE=' '
        IF(ISTAT.EQ.0) THEN
          CALL LOG_MESS('Sending first F')
          ISTAT=1
        END IF
        IFUNC=IO$_READVBLK.OR.IO$M_NOECHO.OR.IO$M_PURGE.OR.IO$M_TIMED
        ISTS=SYS$QIO(,%VAL(JCHAN),%VAL(IFUNC),JOSB,,,
     -      %REF(CLINE),%VAL(64),%VAL(2),,,) !2s timeout is backup catchall
        COUT='F000000'
        IF(STATUS(2).GE.1.AND.STATUS(2).LE.12800.AND.STATUS(34).EQ.1) 
     -      THEN
! the next if is commented out JC
!         IF(SPECIAL(STATUS(2),5).EQ.0) THEN !GE in control
            IDP1=NINT(DERIVE(STATUS(2),17)+273.16) !GE dewpoint - K
            IF(IDP1.GE.0.AND.IDP1.LE.999) 
     -          WRITE(COUT(2:4),'(I3.3)',IOSTAT=IOS) IDP1
! and its associated endif
!         END IF
          ITEMP=NINT(DERIVE(STATUS(2),8)) !True de-iced temp - K
          IF(ITEMP.GE.0.AND.ITEMP.LE.999) 
     -        WRITE(COUT(5:7),'(I3.3)',IOSTAT=IOS) ITEMP
        END IF
        IFUNC=IO$_WRITEVBLK
        ISTS=SYS$QIOW(,%VAL(JCHAN),%VAL(IFUNC),JOSB1,,,
     -      %REF(COUT),%VAL(7),,,,)
        IW=0
        DO WHILE(IW.LT.5.AND.JOSB(1).EQ.0)
          IW=IW+1
          CALL LIB$WAIT(0.1)
        END DO
        IF(JOSB(1).EQ.0.OR.JOSB(2).LT.64) THEN !Timeout or incomplete data
          IF(ISTAT.EQ.2.OR.ISTAT.EQ.1) 
     -        CALL LOG_MESS('Timeout or incomplete data')
          ISTAT=3
          ISTS=SYS$CANCEL(%VAL(JCHAN))
        ELSE
          IF(ISTAT.EQ.1.OR.ISTAT.EQ.3) 
     -        CALL LOG_MESS('First 64 characters received')
          READ(CLINE,'(16Z4.4)',IOSTAT=ISTS) JVAL
          IF(ISTS.EQ.0) THEN
            DO I=1,16
              JDRSBLK(JISSBLK(IFWVS+1)/2+I)=JVAL(I)
            END DO        
          ELSE
            IF(ISTAT.EQ.1.OR.ISTAT.EQ.3) 
     -        CALL LOG_MESS('Can''t decode data')
          END IF
          ISTAT=2
        END IF
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
      TYPE *,'H_FWVS '//CDAT//' '//CTIM//MESSAGE
      RETURN
      END
!*******************************************************************************
!
! ROUTINE          CONNECT SUBROUTINE FORTVAX
!
! PURPOSE          Opens a VMS channel for getting messages from FWVS system.
!
! DESCRIPTION      Establishes a VMS channel to the FWVS system
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
!                  CNAME       C**  IN   Name of RS232 port to FWVS
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
! Cancels any outstanding I/O on the FWVS channel and then frees the channel.
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
      CHARACTER CMON*3(12),CDATE*9,CDATE1*8,CDUM*15
      DATA      CMON /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     &    'SEP','OCT','NOV','DEC'/

      CALL DATE_AND_TIME(CDATE1,CDUM,CDUM,IVAL)
      WRITE(CDATE,'(A)') CDATE1(7:8)//'-'//CMON(IVAL(2))//'-'//
     &    CDATE1(3:4)
      RETURN
      END
