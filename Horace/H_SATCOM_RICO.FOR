!
! Version for BAE 146 makes the following changes:
!
! 1. XV208 references changed to G-LUXE
! 2. meto.gov.uk references changed to metoffice.com
! 3. Looks for STRATOS CSAT rather than BT CSAT in email header lines
! 4. Only direct mail, fax, PSTN, and mobile now supported
! 5. Doesn't send text pos reps when on ground
!
! Version for MAP makes the following changes:
!
! 1. Reinstates Indian and Pacific options
! 2. Uses BT's LESs for Indian and Pacific
! 3. Sends by email if TRANSMISSION_MODE logical is set to EMAIL
! 4. Handles messages uploaded by Satmail
! 5. Handles request for list of messages sent/received
! 6. Compacting of sonde messages disabled, pending full testing
! 7. |G is now available to force email, pstn, or default transmission mode
! 8. Sends immediate message to Flight Leader when new message received
!
      PROGRAM H_SATCOM
!
! Handles all communication with the Capsat satcom system, via the RS232/423
! port on HORNET defined by the SATCOM_CHAN logical.  Passes messages between
! the satcom system and the H_DISPLAY and H_NAVAID processes, and gets status 
! information from the system for display by H_DISPLAY when required.
!
! The SATCOM_CHAN port uses hardware handshaking and should be set to
! 9600 baud, 8D, 1S, NP, software handshaking.  The Capsat Transceiver Software
! Interface Reference Manual fully defines the commands and responses which
! can be sent/received over the link to the transceiver.
!
! All messages received via the Satcom transceiver are passed to the Flight
! Leader with the following exception: messages beginning with a | (ASCII 124)
! character are intercepted and interpreted by H_SATCOM, without notifying
! the flight leader, as follows:
!
! |A           The following lines are treated as a message to send, just as if
!              they had been passed from the flight leader.  This provides
!              automatic turnaround for testing.
! |B filename  The following lines are to be written to the specified file on
!              HORACE.  
! |C filename  The specified HORACE file is to be sent to the destination
!              specified by the following line.
! |D n         Controls conditional NCS forcing.  0 to disable, 1 for WAOR,
!              2 for EAOR, 3 for POR, 4 for IOR.
! |E           Force immediate position report.
! |F n         Request system status report.  The following line must be an
!              address line which will be use to route the returned message.
!              0 - hardware, 1 - GPS, 2 - Misc, 3 - messages.
! |G c         Sets future transmission mode.  E - email, P - PSTN, D - Default
! |P c         Sets position reporting mode.  B - data, M - message, D - Default
!
! In each case the line beginning with the | must be terminated by a CR/LF pair.
! This program:
! 
! 1. Opens a connection to the satcom transceiver.
! 2. Routinely checks for:
!    a. Sonde messages to be sent to the transceiver.
!    b. Text messages from the flight leader to be sent to the transceiver.
!    c. New messages received by the transceiver for the flight leader.
!    d. New messages received by the transceiver requiring special action.
!    e. Transceiver status summary.
!    f. Detailed transceiver status when required by H_DISPLAY processes.
!    g. Error and info messages from the transceiver.
!    h. Poll requests requiring no action.
! 3. Most information passing through H_SATCOM is logged in H_SATCOM.LOG.
! 4. When a new message is received triggers the Flight Leaders' Alert System.
!
! The program uses the STATUS(46) word as follows:
!
! Bit 0 Set for at least 1.1s when a new message has been received, then
!       cleared.  This is long enough for H_DRS_LOG to trigger the Flight
!       Leader's Alert System (FLASH) ie the red flashing light to the left of
!       FL's console.
!
! The program uses the STATUS(47) word as follows:
!
! Bit 0 Set by H_DISPLAY programs when they want Satcom hardware status.
!       Cleared by H_SATCOM when new hardware status has been written to file.
! Bit 1 Set by H_DISPLAY programs when they want Satcom GPS status.
!       Cleared by H_SATCOM when new GPS status has been written to file.
! Bit 2 Set by H_DISPLAY programs when they want Satcom miscellaneous status.
!       Cleared by H_SATCOM when new miscellaneous status has been written to
!       file.
! Bit 3 Set by H_DISPLAY programs when they want Satcom message information.
!       Cleared by H_SATCOM when new messages have been written to file.
! Bit 4 Negated when H_SATCOM has written new hardware status to file.
! Bit 5 Negated when H_SATCOM has written new GPS status to file.
! Bit 6 Negated when H_SATCOM has written new miscellaneous status to file.
! Bit 7 Negated when H_SATCOM has written new messages to file.
! Bit 8 Set when there is a change to SAT_OUT_STS.DAT.
! Bit 9 Negated whenever SAT_INP_STS.DAT is changed.
! Bit 10 Negated whenever SAT_OUT_STS.DAT is changed.
! Bit 11 Set by H_DISPLAY to disable position reporting.
! Bit 12 Set by H_DISPLAY for conditional ocean region forcing.
! Bit 13 Set by H_DISPLAY for unconditional ocean region forcing.
! Bit 14 With bit 15 specifies the required ocean region.
! Bit 15 With bit 14 specifies the required ocean region.
!
! The program uses the STATUS(54) word as follows:
!
! Bit 0 Set for by H_DISPLAY when it is required to set the Satcom GPS from
!       the XR5 GPS.
!
! The program uses shared files in [HORACE.SATCOM] to communicate with the 
! H_DISPLAY and H_NAVAID processes as follows:
!
! SAT_INP_STS.DAT    Status file of all incoming messages
!                    Written and maintained by H_SATCOM.  Read by H_DISPLAY
! SAT_INP_nnnn.DAT   Incoming messages.  Written and maintained by H_SATCOM.
!                    Read by H_DISPLAY
! SAT_OUT_STS.DAT    Status file of all outgoing messages.  Written and read by
!                    all processes.
! SAT_OUT_nnnn.DAT   Outgoing messages, including sonde messages.
! SAT_DISP.DAT       Shared file of the four Satcom system status displays
!                    Written by H_SATCOM, read by H_DISPLAY programs
! SAT_ADDR.DAT       Destination code definition and description
!                    Hand edited or transferred from EAGLE, read by
!                    SAT_SEND_MESS routine of H_DISPLAY.
!
! SAT_INP_STS.DAT as a minimum contains 00000 and typically looks like:
!
! 00003                                                                         
! 00196  03-NOV-95  16:10:37   339  From: Nick Jackson  3/11/95 1604L           
! 00197  07-NOV-95  12:19:02   324  From: AIR_MAN  7/11/95 1211L                
! 00198  09-NOV-95  14:53:07   315  From: AIR_MAN  9/11/95 1435L                
!
! SAT_OUT_STS.DAT as a minimum contains 00000 00000 and typically looks like:
!
! 00003 00002                                                                   
! 00399  10-NOV-95  09:39:01    54  Mail - JACKSON               Send failed  TA
! 00400  10-NOV-95  10:35:07   422  Synoptic data bank           Queued       NV
! 00401  10-NOV-95  10:36:45   232  MRF fax at Farnborough       Successful   FL
!
! Build with:
! 
!  $ FORT H_SATCOM                  requires HCOM_DEF.FOR in same directory
!  $ LINK H_SATCOM,H_COMMON/OPT
!
!    where H_COMMON.OPT contains the line SYS$LIBRARY:H_COMMON/SHAREABLE
!
! The program uses the following subroutines which are included with this file:
!
! AZIMELEV   Computes azimuth and elevation of a satellite
! BESTSATS   Computes list of available satellites, nearest first
! COMPACT    Consolidates files of drop sonde messages
! COMPRESS   Replaces multiple spaces with single ones in a string
! CONNECT    Establishes RS232 port connection to Satcom system
! ERRCHK     Checks for errors in Satcom command responses
! GETLES     Works out which LES to use for a message
! GET_NXT_OUT Works out the next output file version and its record position
! INSGS      Gets INS groundspeed
! LAN        Works out string length less trailing spaces
! LOG_MESS   Sends message to log file
! NCSCHK     Checks NCS in use, and logins in to new NCS if necessary
! Q_CLEAN    Cleans up the transmit queue when program starts
! Q_SCAN     Scans transmit queue for next queued entry
! SATCOM     Sends command to Satcom system
! SATFIL     Gets a new message from the Satcom system
! SATINF     Gets full status information from Satcom system and writes to log
! SATINIT    Initialises Satcom system
! SNDMES     Sends message from aircraft
! SNDPOS     Sends position from aircraft
! ST_MESS    Sets status message
! STSLIN     Creates a log file status line for created files
! STSUPD     Creates status displays for H_DISPLAY and received status requests
! UPDTSTS    Updates transmit status file
!
! V1.00  30/11/94  W.D.N.JACKSON
! V1.01  16/04/95  W.D.N.JACKSON
!        No functionality changes.
!        Bug fixes in light of problems arising on first set of flights.
!        Improved error detection, reporting and diagnostics.
! V1.02  24/10/95  W.D.N.JACKSON
!        |D option implemented to allowed ocean region forcing from the ground.
!        Now sets the Flight Number each time it sends a position report, in
!        case the Satcom transceiver has cycled on/off and lost its setting.
! V1.03  16/02/96  W.D.N.JACKSON
!        Now prints |D messages to log
!        Now handles purged SAT_INP_STS files
!        Now prints buffer size in SATCOM command read error 1
!        Now correctly prints LES change times from GETLES, and on one line
!        Now has diagnostic statements to check NCSCHK's speed computation
!        Now handles purged SAT_OUT_STS files
!        Now handles input messages > 50 lines
! V1.04  21/03/96  W.D.N.Jackson
!        Diagnostic statements removed
!        Now uses INS ground speed if available
!        Now uses | instead of tilde for special message requests
!        |D 0 option now forces re-selection of best satellite
!        |E option added for immediate position reports
!        |F option added for status reports
!        Provision for use of H_SATCOM.TST file
!        New routine for handling NCS changes
!        New init routine to enable takeoff/landing reports
!        Now routes all messages to memory
! V1.05  17/02/97  W.D.N.Jackson
!        Bug fixed so doesn't crash on commands that produce more than 50 lines.
!        REPORT files routinely deleted now, in case building up.
! V1.06  16/09/99  W.D.N.Jackson
!        Option via bit 0, STATUS(54) to set GPS position.
! V1.07  07/11/00  W.D.N.Jackson
!        Strips header from messages received by email.  Sends with sc 67.
!
      IMPLICIT  NONE
      BYTE      BBUFF(15000)
      CHARACTER CLINE*80,CDISP(23)*80,CBUFF(50)*80,CSLINE*80,CFILE*80,
     &          CLSTFNUM*4,CMESS(23)*80,CFILE1*12,CINDIR(5)*80,
     &          CLINE1*80,CFILNO*5,CFILE2*12,CTMPNO*5,CNXTMES*5,CMODE*5,
     &          CPOSN*4
      INTEGER*2 JCHAN
      INTEGER*4 STR$POSITION,ILSTPOS,ISNDPOS,IFSTMES,IOS,IL,
     &          IMCNT,IMSRT,IMNXT,IP1,IP2,IFILNO,IJ,ITIME,ITDIF,ILR,
     &          INCNT,IF,IS,ICNT,IOS1,IOS2,IOS3,IOS4,IOS5,IOS6,IOS7,
     &          IOS8,IR,ISIZ,INXTREC,LAN,ISTS,ILST,I,IP,IOPEN,IFIRST,
     &          ISTAT
      LOGICAL*4 LCHKRX,LCHKTX,LFLSET,LSATOK,LSTSUPD,LSATINIT,LSTSDONE,
     &          LNOTR,LOK,LWAITING,LTSTFIL
      REAL*4    RLSTUPD0,RLSTUPD1,RLSTUPD2,RLSTUPD3,RLSTMES,RLSTSCN,
     &          RFLSET,RLSTPOS,RLSTTRY
      INCLUDE   'HCOM_DEF.FOR'
      COMMON    JCHAN,BBUFF,CBUFF
      COMMON    /MESSCOM/ IMCNT,IMSRT,IMNXT,CMESS
!
! Initialize variables
!
      IMSRT=1                          !Where system messages start in buffer
      IMCNT=0                          !No of messages in buffer
      IMNXT=1                          !Location for next message
      CLSTFNUM=CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0) !Last fno stored by Satcom
      ILSTPOS=NINT(SECNDS(0.0))        !Time when last posn report sent
      ISNDPOS=-1                       !Don't send posn report yet, show first
      LCHKRX=.TRUE.                    !Check for any received messages
      LCHKTX=.TRUE.                    !Check for any transmitted messages
      LFLSET=.FALSE.                   !FLASH system not set
      LSATOK=.TRUE.                    !Assume can contact Satcom system
      CFILNO='00000'                   !No file currently being transmitted
      RLSTUPD0=0.                      !Last hardware status update
      RLSTUPD1=0.                      !Last GPS status update
      RLSTUPD2=0.                      !Last misc status update
      RLSTUPD3=0.                      !Last message status update
      LSTSUPD=.TRUE.                   !Force initial status update
      LSATINIT=.FALSE.                 !Satcom system not yet initialised
      LSTSDONE=.FALSE.                 !Initial status output not done yet
      IFSTMES=0                        !Number of the 1st mess in SAT_OUT_STS
      RLSTTRY=0.0
!
! Clear the global status bits
!
      CALL LOG_MESS('H_SATCOM starting')
      STATUS(46)=IBCLR(STATUS(46),0)
      STATUS(47)=0
      CALL GET_LOGICAL('POSITION_REPORTS',CPOSN) !Get posn report requirement
      CALL LOG_MESS('Position report status - '//CPOSN)
      IF(CPOSN.EQ.'OFF ') STATUS(47)=IBSET(STATUS(47),11) 
      STATUS(54)=IBCLR(STATUS(54),0)
!
! Open shared data files.  Note that if an _STS.DAT file is missing, a new
! one will be started with zero associated files.  Clear the display file
! to reduce chance of confusion with old data.
!
      OPEN(UNIT=10,FILE='[.SATCOM]SAT_INP_STS.DAT',STATUS='UNKNOWN',
     &    SHARED,ACCESS='DIRECT',RECL=20,IOSTAT=IOS)
      OPEN(UNIT=11,FILE='[.SATCOM]SAT_OUT_STS.DAT',STATUS='UNKNOWN',
     &    SHARED,ACCESS='DIRECT',RECL=20,IOSTAT=IOS)
      OPEN(UNIT=12,FILE='[.SATCOM]SAT_DISP.DAT;',STATUS='UNKNOWN',
     &    SHARED,ACCESS='DIRECT',RECL=23*80/4,IOSTAT=IOS)
      DO I=1,23
        CDISP(I)=' '
      END DO
      WRITE(12,REC=1,IOSTAT=IOS) CDISP
      WRITE(12,REC=2,IOSTAT=IOS) CDISP
      WRITE(12,REC=3,IOSTAT=IOS) CDISP
      WRITE(12,REC=4,IOSTAT=IOS) CDISP
      UNLOCK(12,IOSTAT=IOS)
!
! Open the address book and list its contents to the log file
!
      OPEN(UNIT=13,FILE='[.SATCOM]SAT_ADDR.DAT',STATUS='OLD',
     &    READONLY,IOSTAT=IOS)
      IF(IOS.EQ.0) THEN
        CALL LOG_MESS('Contents of address book:')
        WRITE(6,'(X,A)',IOSTAT=IOS) ' '
        READ(13,'(A)',IOSTAT=IOS) CLINE
        DO WHILE(IOS.EQ.0)
          IF(CLINE(1:1).NE.'!') 
     &        WRITE(6,'(X,A)',IOSTAT=IOS) CLINE(1:LAN(CLINE))
          READ(13,'(A)',IOSTAT=IOS) CLINE
        END DO
        WRITE(6,'(X,A)',IOSTAT=IOS) ' '
      ELSE
        CALL LOG_MESS('Unable to open or find address book')
      END IF
      CLOSE(13,IOSTAT=IOS)     
!
! Go through the transmit log file and stop any messages in the queue or
! being transmitted.
!
      CALL Q_CLEAN(IFSTMES)
!
! Open I/O channel to the transceiver via the SATCOM_CHAN RS232 port.  The 
! CONNECT subroutine waits in a loop trying every 10s until a connection has
! been established.
!
      CALL ST_MESS('Trying to establish connection to Satcom port')
      CALL LOG_MESS('Trying to establish connection to Satcom port')
      CALL CONNECT(JCHAN)              !Open I/O channels, waiting if necessary
      CALL LOG_MESS('Connection established to Satcom port')
!
      CALL GET_LOGICAL('TRANSMISSION_MODE',CMODE) !Get transmission mode
!
! Now loop once every 8s checking for changes and new events.
!
      CALL LOG_MESS('Looping every 8s')
      CALL ST_MESS('Checking for events every 8s')
      DO WHILE(.TRUE.)
!
! Check that the Satcom system is responding, in case it has been switched off
! or the connection through HORNET has been lost.
!
        IL=0
        IF(.NOT.LSATOK) IL=101         !Inhibit timeout messages
        CALL SATCOM(CHAR(3),CBUFF,IL)  !Send CTRL/C
        IF(IL.LT.0.AND.LSATOK) THEN    !Just lost contact
          CALL ST_MESS('No response from Satcom system')
          CALL ERRCHK('Satcom not responding',0,IL)
          LSATOK=.FALSE.
        ELSE IF(IL.GE.0.AND..NOT.LSATOK) THEN !Just made contact
          CALL LOG_MESS('Response received from Satcom')
          CALL ST_MESS('Checking for events every 8s')
          LSATOK=.TRUE.
        END IF
!
! If not done, initialise the Satcom system, and put a copy of most of the
! system status information into the H_SATCOM log file.
!
        IF(LSATOK.AND..NOT.LSATINIT) CALL SATINIT(LSATINIT,CLSTFNUM)
        IF(LSATOK.AND.LSATINIT.AND..NOT.LSTSDONE) THEN
          CALL SATINF(LSTSDONE)
          CALL ST_MESS('Checking for events every 8s')
        END IF
!
! Check for any new messages from the transceiver.  These are always copied to
! the log file and to the message display buffer.  A few messages will require
! suitable action to be taken.
!
        IF(LSATOK) THEN
          CALL SATCOM('st -l',CBUFF,IL)  !Get status messages
          CALL ERRCHK('Bad status collection',0,IL)
          IF(IL.GE.2) LSTSUPD=.TRUE.     !Show have got new status message(s)
          DO I=2,IL
            CALL TIME(CMESS(IMNXT)(1:8))
            CMESS(IMNXT)(9:9)=' '
            CMESS(IMNXT)(10:)=CBUFF(I)
            WRITE(6,'(X,A)',IOSTAT=IOS) CMESS(IMNXT)
     &          (1:LAN(CMESS(IMNXT)))
            IMNXT=IMNXT+1
            IF(IMNXT.GT.23) IMNXT=1
            IMCNT=MIN(IMCNT+1,23)
            IF(IMCNT.EQ.23) IMSRT=IMNXT
!
            IF(CBUFF(I)(1:9).EQ.'INFO  480') THEN !Position report failure
              ISNDPOS=1                !Send it again
            ELSE IF(CBUFF(I)(1:9).EQ.'INFO  481') THEN !Position report success
              ISNDPOS=0                !No longer need to report
              ILSTPOS=NINT(SECNDS(0.0)) !Note time of latest report
            ELSE IF(CBUFF(I)(1:9).EQ.'INFO  403') THEN !RXLOG updated
              LCHKRX=.TRUE.            !Need to check for received messages
            ELSE IF(CBUFF(I)(1:9).EQ.'INFO   82') THEN !Transmission issued
              CALL UPDTSTS(IFSTMES,CFILNO,'Issued')
            ELSE IF(CBUFF(I)(1:9).EQ.'INFO   80') THEN !Transmission failed
              IF(CFILNO.GT.'00000') THEN
                CALL UPDTSTS(IFSTMES,CFILNO,'Send failed')
                CALL SATCOM('de TA'//CFILNO//'.TXT',CBUFF,100)
                CALL SATCOM('de OUT*.*',CBUFF,100)
                STATUS(47)=IBSET(STATUS(47),8) !Tell H_SATCOM
                CFILNO='00000'         !No current file
              END IF
            ELSE IF(CBUFF(I)(1:9).EQ.'INFO   81') THEN !Transmission successful
              IF(CFILNO.GT.'00000') THEN
                CALL UPDTSTS(IFSTMES,CFILNO,'Successful')
                STATUS(47)=IBSET(STATUS(47),8) !Tell H_SATCOM
                CFILNO='00000'         !No current file
              END IF
            ELSE IF(CBUFF(I)(1:9).EQ.'ERROR 253') THEN !REPORT file renamed
              CALL SATCOM('de REPORT.*',CBUFF,100)
            END IF
          END DO
        END IF
!
! See if a new flight number is available, and if so store in the Satcom
! transceiver for use as the Flight ID.  This is to cover the case that the
! DRS does not start to send data to HORACE until after this program starts.
!
        IF(CFNUM.NE.CLSTFNUM.AND.LSATOK) THEN !Use latest flt num for Flight ID
          IF(CFNUM(2:4).GE.'000'.AND.CFNUM(2:4).LE.'999') THEN !If valid
            CALL SATCOM('se -z FLIGHTID='//CFNUM,CBUFF,IL) !Set flight id
            IF(IL.GE.0) THEN
              CLSTFNUM=CFNUM
              CALL LOG_MESS('Flight number '//CFNUM//' sent to Satcom')
            END IF
          END IF
        END IF
!
! See if the NCS which is being used needs to be changed.  Although this
! routine is called every 8 s, it will not actually do any checks more often
! than once every minute.
!
        IF(LSATOK) CALL NCSCHK
!
! If more than ten mins since transmit issued check it is still active.  This
! is a safety check and shouldn't be necessary under normal circumstances.
!
        IF(CFILNO.NE.'00000'.AND.SECNDS(RLSTMES).GT.600.
     &      .AND.LSATOK) THEN
          LNOTR=.TRUE.                 !Assume no transmissions active
          CALL SATCOM('st -q',CBUFF,IL) !Get queued commands
          CALL ERRCHK('Bad queued commands',2,IL)
          DO I=2,IL                    !Return if any transmissions active
            IP1=STR$POSITION(CBUFF(I),'Transmission',0)
            IP2=STR$POSITION(CBUFF(I),'Transmit',0)
            IF(IP1.NE.0.OR.IP2.NE.0) LNOTR=.FALSE. !A transm is active or queued
          END DO
          IF(LNOTR) THEN               !If no transmissions found
            STATUS(47)=IBSET(STATUS(47),8) !Force transmit queue check
            CALL UPDTSTS(IFSTMES,CFILNO,'Unknown') !Don't know what happened
            CALL LOG_MESS('Unknown transmit status for '//CFILNO)
            CFILNO='00000'
          END IF
          RLSTMES=SECNDS(0.0)
        END IF
!
! If more than five mins since scanned queue do it again.  This is a safety
! check and shouldn't be necessary under normal circumstances.
!
        IF(CFILNO.EQ.'00000'.AND.SECNDS(RLSTSCN).GT.300.
     &      .AND..NOT.BTEST(STATUS(47),8)) THEN
          CALL Q_SCAN(IFSTMES,CFILNO)
          IF(CFILNO.NE.'00000') THEN
            STATUS(47)=IBSET(STATUS(47),8) !Force transmit queue check
            CALL LOG_MESS('5 min q check found '//CFILNO//' waiting')
            CFILNO='00000'
          END IF
          RLSTSCN=SECNDS(0.0)
        END IF
!
! Check the transmit queue when requested and request transmission if any
! files are waiting to be transmitted.  Also check if the flight leader wants
! to stop the current transmission.
!
        IF(BTEST(STATUS(47),8).AND.LSATOK) THEN
          STATUS(47)=IBCLR(STATUS(47),8) !Need to check transmit queue
          IF(CFILNO.EQ.'00000') THEN   !Only scan if no transmission active
            CALL Q_SCAN(IFSTMES,CFILNO) !Get next highest priority queued entry
            RLSTSCN=SECNDS(0.0)        !Note scan time
            CTMPNO=CFILNO
            IF(CFILNO.GT.'00000') THEN !If there is a message
              CALL SNDMES(IFSTMES,CFILNO,CMODE) !Try to send it
              IF(CFILNO.NE.'00000') THEN
                RLSTMES=SECNDS(0.0) !Note time if succeed
              ELSE
                STATUS(47)=IBSET(STATUS(47),8) !Force another queue scan
              END IF
            END IF
          ELSE                         !Check active message status
            READ(CFILNO,'(BN,I)',IOSTAT=IOS1) IFILNO
            READ(11,REC=IFILNO-IFSTMES+2,IOSTAT=IOS2) CLINE
            UNLOCK(11,IOSTAT=IOS3)
            IF((IOS1.OR.IOS2.OR.IOS3).NE.0) THEN
              CALL LOG_MESS('Message check failure:')
              TYPE *,IOS1,IOS2,IOS3,' '//CFILNO,IFILNO
            END IF
            IF(CLINE(66:77).EQ.'Stopped') THEN !Abort current or pending TX
              CALL LOG_MESS('Aborting transmission of message '//CFILNO)
              CALL SATCOM('di OUT*.*',CBUFF,IL)
              CALL ERRCHK('Abort dir err',2,IL)
              IF(IL.GT.2) THEN
                DO I=1,IL-2
                  IF(CBUFF(I)(14:14).EQ.CHAR(35)) THEN !If hash
                    CALL SATCOM('cl',CBUFF,IL) !Stop current transmission
                    CALL ERRCHK('Clear error',0,IL)
                  ELSE                 !Delete OUT file
                    CBUFF(I)(9:9)='.'  !Get OUT file name, removing spaces
                    CFILE2=' '
                    IP=1
                    DO IJ=1,12
                      IF(CBUFF(I)(IJ:IJ).NE.' ') THEN
                        CFILE2(IP:IP)=CBUFF(I)(IJ:IJ)
                        IP=IP+1
                      END IF
                    END DO
                    CALL SATCOM('cl -f '//CFILE2,CBUFF,IL) !Take file off queue
                    CALL ERRCHK('File clear err '//CFILE2,0,IL)
                  END IF
                END DO
              END IF
              CALL SATCOM('de TA*.TXT',CBUFF,IL) !Delete any tx files
              CALL ERRCHK('Del TA err',0,IL)
              CALL SATCOM('de OUT*.*',CBUFF,IL)  !Delete any pending files
              CALL ERRCHK('Del OUT err',0,IL)
              STATUS(47)=IBSET(STATUS(47),8) !Force transmit queue check
              CFILNO='00000'         !No current file
            END IF
          END IF
        END IF
!
! Check if time for a new position report (every 15 min on the quarter if at
! least 5 mins since last, and at least every 15 mins.
! This forces a check in case the program has missed the success message
! for some reason.
!
        ITIME=NINT(SECNDS(0.0))
        ITDIF=ITIME-ILSTPOS
        IF(ITDIF.LT.-20000) ITDIF=ITDIF+24*3600 !Midnight crossing
        IF((ITIME/300.NE.ILSTPOS/300)
     &      .AND.(ITDIF.GT.200.OR.ISNDPOS.EQ.-1)
     &      .AND.ISNDPOS.LE.0) ISNDPOS=1
        IF(ISNDPOS.EQ.2.AND.SECNDS(RLSTPOS).GT.900.) ISNDPOS=1
!
! Check for any H_SATCOM.TST file in the current directory.  If found this
! is used as though it were a message received from the satcom transceiver.  It
! is deleted after being used.
!
        INQUIRE(FILE='H_SATCOM.TST',EXIST=LTSTFIL,IOSTAT=IOS)
!
! Check for new received messages.  This is only done if there has been a 
! change to the RXLOG or the first time this loop is executed.  Note that
! only ASCII (7 bit, language 0) messages are handled at present.  Some
! change could be required to read binary messages.  This routine will retrieve
! files received by the transceiver while HORACE is off.
!
        IF((LCHKRX.AND.LSATOK).OR.LTSTFIL) THEN
          IF(LTSTFIL) THEN             !Will work with a pseudo message
            INCNT=1
          ELSE                         !Normal message from transceiver
            CALL SATCOM('di IN.*',CBUFF,ILR) !Get directory of received files
            CALL ERRCHK('CHKRX dir err',2,ILR)
            INCNT=MIN(ILR-2,5)         !Can only handle 5 file each pass
            IF(ILR.LE.7) LCHKRX=.FALSE. !If no more IN files in directory
            IOPEN=0                    !No of open (unusable) files
            DO I=1,INCNT
              IF(CBUFF(I)(14:14).NE.CHAR(35)) THEN !If no # then is usable
                CINDIR(I)=CBUFF(I)     !Save directory entries
              ELSE
                IOPEN=IOPEN+1
              END IF
            END DO
            INCNT=INCNT-IOPEN          !Remove any open files
          END IF
          DO IF=1,INCNT                !Will only execute this loop if >=1 mes
            IF(LTSTFIL) THEN
              CALL LOG_MESS('Using test message from H_SATCOM.TST')
              IL=0
              OPEN(13,FILE='H_SATCOM.TST;',STATUS='OLD',IOSTAT=IOS)
              READ(13,'(A)',IOSTAT=IOS) CBUFF(IL+1)
              DO WHILE(IOS.EQ.0)
                IL=IL+1
                READ(13,'(A)',IOSTAT=IOS) CBUFF(IL+1)
              END DO
              CLOSE(13,DISPOSE='DELETE',IOSTAT=IOS)
            ELSE
              CFILE1='IN.'//CINDIR(IF)(10:12)
              CALL SATFIL(CFILE1(1:LAN(CFILE1)),CBUFF,IL) !Get the message
              CALL ERRCHK('File read err '//CFILE1(1:LAN(CFILE1)),1,IL)
            END IF
            IF(IL.GT.0.AND.CBUFF(2)(1:9).NE.'ERROR 331') THEN
!
! Strip any leading blank lines or BT or ST (Singapore Telecom) headers from
! the message.  It is not clear why we have these headers, but they are
! normally there.
!
              IS=1
              DO WHILE(CBUFF(IS).EQ.' '.OR.CBUFF(IS)(1:12).EQ.
     &            'STRATOS CSAT'
     &            .OR.CBUFF(IS)(1:9).EQ.'FROM PSTN'.OR.CBUFF(IS)(1:10)
     &            .EQ.'Text item:')
                IS=IS+1
              END DO
              IF(CBUFF(IS)(1:6).EQ.'Msg#: ') IS=IS+8 !Remove any BT email header
!
! Strip trailing blanks, which are found at the end of messages sent by email
!
              IF(IL.LE.50) THEN
                DO WHILE(CBUFF(IL).EQ.' '.AND.IL.GT.IS)
                  IL=IL-1
                END DO
              END IF
!
! If received message is for the flight leader, then write it out to a file
! SAT_INP_nnnnn.DAT, and also to the log.  Update SAT_INP_STS.DAT with a new
! entry.
!
              IF(CBUFF(IS)(1:1).NE.'|') THEN
                READ(10,REC=1,IOSTAT=IOS1) CLINE !Read first line for latest ver
                READ(CLINE(1:5),'(I5)',IOSTAT=IOS2) ICNT !Get record count
                ICNT=ICNT+1
                WRITE(CLINE(1:5),'(I5.5)',IOSTAT=IOS3) ICNT
                WRITE(10,REC=1,IOSTAT=IOS4) CLINE
                IF(ICNT.NE.1) THEN     !If not new file
                  READ(10,REC=2,IOSTAT=IOS6) CLINE
                  READ(CLINE(1:5),'(I5)',IOSTAT=IOS7) IFIRST
                ELSE
                  IOS6=0
                  IOS7=0
                  IFIRST=1
                END IF
                UNLOCK(10,IOSTAT=IOS)
                WRITE(CLINE(1:5),'(I5.5)',IOSTAT=IOS8) IFIRST+ICNT-1
                OPEN(13,FILE='[.SATCOM]SAT_INP_'//CLINE(1:5)//'.DAT;',
     &              STATUS='NEW',CARRIAGECONTROL='LIST',IOSTAT=IOS5)
                IF((IOS1.OR.IOS2.OR.IOS3.OR.IOS4.OR.IOS5.OR.IOS6
     &              .OR.IOS7.OR.IOS8).NE.0) THEN
                  CALL LOG_MESS('RX log file err:')
                  TYPE *,IOS1,IOS2,IOS3,IOS4,IOS5,IOS6,IOS7,IOS8,
     &                ' '//CLINE(1:5)
                END IF
                CALL LOG_MESS('Message received '//CLINE(1:5)//':')
                DO I=1,IS-1
                  WRITE(6,'(X,A)',IOSTAT=IOS) CBUFF(I)(1:LAN(CBUFF(I)))
                END DO
                ISIZ=0
                DO I=IS,MIN(IL,50)
                  IR=LAN(CBUFF(I))
                  WRITE(13,'(A)',IOSTAT=IOS) CBUFF(I)(1:IR)
                  IF(IOS.NE.0) THEN
                    CALL LOG_MESS('RX file write err '//CLINE(1:5))
                    TYPE *,IOS
                  END IF
                  WRITE(6,'(X,A)',IOSTAT=IOS) CBUFF(I)(1:IR)
                  ISIZ=ISIZ+IR
                END DO
                IF(IL.GT.50) THEN
                  OPEN(15,FILE='[.SATCOM]OVERFLOW.DAT;',STATUS='OLD',
     &                IOSTAT=IOS)
                  READ(15,'(A)',IOSTAT=IOS) CSLINE
                  DO WHILE(IOS.EQ.0)
                    IR=LAN(CSLINE)
                    WRITE(13,'(A)',IOSTAT=IOS) CSLINE(1:IR)
                    IF(IOS.NE.0) THEN
                      CALL LOG_MESS('RX file write err '//CLINE(1:5))
                      TYPE *,IOS
                    END IF
                    WRITE(6,'(X,A)',IOSTAT=IOS) CSLINE(1:IR)
                    ISIZ=ISIZ+IR
                    READ(15,'(A)',IOSTAT=IOS) CSLINE
                  END DO
                  CLOSE(15,IOSTAT=IOS,DISPOSE='DELETE')
                END IF
                CLOSE(13,IOSTAT=IOS)
!
                CSLINE(1:)=CLINE(1:5) !Put in file number
                CALL DATE1(CSLINE(8:16)) !Put in date
                IF(CSLINE(8:8).EQ.' ') CSLINE(8:8)='0'
                CALL TIME(CSLINE(19:26)) !Put in time
                WRITE(CSLINE(29:32),'(I4)',IOSTAT=IOS) ISIZ !Message size
                CSLINE(35:)=CBUFF(IS)(1:) !Put in part of first line
                WRITE(10,REC=ICNT+1,IOSTAT=IOS) CSLINE
                IF(IOS.NE.0) THEN
                  CALL LOG_MESS('RX status file write error:')
                  TYPE *,IOS
                END IF
                UNLOCK(10,IOSTAT=IOS)
                STATUS(46)=IBSET(STATUS(46),0) !Tell the FLASH system
                CALL HM_MESS('New Satcom message received')
                STATUS(47)=STATUS(47).XOR.'200'X !Negate bit 9 for H_DISPLAY
                RFLSET=SECNDS(0.0)
                LFLSET=.TRUE.
!
! If received message begins with | then needs special handling by H_SATCOM
!
              ELSE
!
! |A = Turnaround message.  Write it out to file and then transmit it
! back again to whatever destination is defined in its second line.
!
                IF(CBUFF(IS)(2:2).EQ.'A') THEN !Turnaround message
                  CALL GET_NXT_OUT(CNXTMES,INXTREC)
                  OPEN(UNIT=13,FILE='[.SATCOM]SAT_OUT_'//CNXTMES//
     &                '.DAT',STATUS='NEW',CARRIAGECONTROL='LIST',
     &                IOSTAT=IOS)
                  CALL LOG_MESS('Turnaround message received '//
     &                CNXTMES//':')
                  DO I=1,IS
                    WRITE(6,'(X,A)',IOSTAT=IOS)CBUFF(I)(1:LAN(CBUFF(I)))
                  END DO
                  ISIZ=0
                  DO I=IS+1,MIN(IL,50)
                    IR=LAN(CBUFF(I))
                    WRITE(6,'(X,A)',IOSTAT=IOS) CBUFF(I)(1:IR)
                    WRITE(13,'(A)',IOSTAT=IOS) CBUFF(I)(1:IR)
                    ISIZ=ISIZ+IR
                  END DO
                  IF(IL.GT.50) THEN
                    OPEN(15,FILE='[.SATCOM]OVERFLOW.DAT;',STATUS='OLD',
     &                  IOSTAT=IOS)
                    READ(15,'(A)',IOSTAT=IOS) CSLINE
                    DO WHILE(IOS.EQ.0)
                      IR=LAN(CSLINE)
                      WRITE(6,'(X,A)',IOSTAT=IOS) CSLINE(1:IR)
                      WRITE(13,'(A)',IOSTAT=IOS) CSLINE(1:IR)
                      ISIZ=ISIZ+IR
                      READ(15,'(A)',IOSTAT=IOS) CSLINE
                    END DO
                    CLOSE(15,IOSTAT=IOS,DISPOSE='DELETE')
                  END IF
                  CLOSE(13,IOSTAT=IOS)
!
                  CALL STSLIN(CBUFF(IS+1),CNXTMES,ISIZ,CSLINE)
                  CSLINE(79:80)='TA'   !Show turnaround message
                  WRITE(11,REC=INXTREC,IOSTAT=IOS) CSLINE
                  UNLOCK(11,IOSTAT=IOS)
                  STATUS(47)=IBSET(STATUS(47),8) !Force transmit queue check
                  STATUS(47)=STATUS(47).XOR.'400'X !Negate bit 10 for H_DISPLAY
!
! |B = File for HORACE.  Extract the filename and write the file out.
!
                ELSE IF(CBUFF(IS)(2:2).EQ.'B') THEN !New HORACE file arrived
                  CFILE=CBUFF(IS)(4:)
                  LOK=.TRUE.
                  OPEN(13,FILE=CFILE,STATUS='NEW',
     &                CARRIAGECONTROL='LIST',IOSTAT=IOS)
                  IF(IOS.NE.0) LOK=.FALSE.
                  CALL LOG_MESS('File received - '//
     &                CFILE(1:LAN(CFILE)))
                  DO I=1,IS
                    WRITE(6,'(X,A)',IOSTAT=IOS) 
     &                  CBUFF(I)(1:LAN(CBUFF(I)))
                  END DO
                  DO I=IS+1,MIN(IL,50)
                    IR=LAN(CBUFF(I))
                    WRITE(6,'(X,A)',IOSTAT=IOS) CBUFF(I)(1:IR)
                    WRITE(13,'(A)',IOSTAT=IOS) CBUFF(I)(1:IR)
                    IF(IOS.NE.0) LOK=.FALSE.
                  END DO
                  IF(IL.GT.50) THEN
                    OPEN(15,FILE='[.SATCOM]OVERFLOW.DAT;',STATUS='OLD',
     &                  IOSTAT=IOS)
                    READ(15,'(A)',IOSTAT=IOS) CSLINE
                    DO WHILE(IOS.EQ.0)
                      IR=LAN(CSLINE)
                      WRITE(6,'(X,A)',IOSTAT=IOS) CSLINE(1:IR)
                      WRITE(13,'(A)',IOSTAT=IOS) CSLINE(1:IR)
                      IF(IOS.NE.0) LOK=.FALSE.
                      READ(15,'(A)',IOSTAT=IOS) CSLINE
                    END DO
                    CLOSE(15,IOSTAT=IOS,DISPOSE='DELETE')
                  END IF
                  CLOSE(13,IOSTAT=IOS)
                  IF(IOS.NE.0) LOK=.FALSE.
                  IF(.NOT.LOK) CALL LOG_MESS('Failed to write file')
!
! |C = Transmit the specified HORACE file.
!
                ELSE IF(CBUFF(IS)(2:2).EQ.'C') THEN !HORACE file wanted
                  CFILE=CBUFF(IS)(4:)  !Get file name
                  CALL LOG_MESS('File requested - '
     &                //CFILE(1:LAN(CFILE)))
                  DO I=1,IS
                    WRITE(6,'(X,A)',IOSTAT=IOS)CBUFF(I)(1:LAN(CBUFF(I)))
                  END DO
                  CALL GET_NXT_OUT(CNXTMES,INXTREC)
                  OPEN(UNIT=13,FILE='[.SATCOM]SAT_OUT_'//CNXTMES//
     &                '.DAT',STATUS='NEW',IOSTAT=IOS,CARRIAGECONTROL=
     &                'LIST')
                  LOK=.TRUE.
                  OPEN(14,FILE=CFILE,STATUS='OLD',READONLY,IOSTAT=IOS)
                  IF(IOS.NE.0) LOK=.FALSE.
                  IR=LAN(CBUFF(IS+1))
                  WRITE(13,'(A)',IOSTAT=IOS) CBUFF(IS+1)(1:IR)
                  WRITE(6,'(X,A)',IOSTAT=IOS) CBUFF(IS+1)(1:IR)
                  ISIZ=IR
                  READ(14,'(A)',IOSTAT=IOS) CLINE1
                  IF(IOS.NE.0) LOK=.FALSE.
                  DO WHILE(IOS.EQ.0)
                    IR=LAN(CLINE1)
                    WRITE(13,'(A)',IOSTAT=IOS) CLINE1(1:IR)
                    ISIZ=ISIZ+IR
                    READ(14,'(A)',IOSTAT=IOS) CLINE1
                  END DO
                  IF(IOS.GT.0) LOK=.FALSE.
                  CLOSE(13,IOSTAT=IOS)
                  CLOSE(14,IOSTAT=IOS)
                  IF(IOS.NE.0) LOK=.FALSE.
                  IF(.NOT.LOK) CALL LOG_MESS('Failed to read file')
!
                  CALL STSLIN(CBUFF(IS+1),CNXTMES,ISIZ,CSLINE)
                  IF(.NOT.LOK) CSLINE(66:77)='Not found'
                  CSLINE(79:80)='FT'   !Show file transfer message
                  WRITE(11,REC=INXTREC,IOSTAT=IOS) CSLINE
                  UNLOCK(11,IOSTAT=IOS)
                  STATUS(47)=STATUS(47).XOR.'400'X !Negate bit 10 for H_DISPLAY
                  IF(LOK) STATUS(47)=IBSET(STATUS(47),8) !Force queue check
!
! |D = Set or change conditional forcing.  If the login fails, the system will
! stay with its current NCS.  It will not try again unless the required OR is 
! changed or conditional bit is cleared and then set.  When the conditional
! forcing is cleared the system will revert to the nearest NCS at that time.
! The successful conditional forcing of an NCS will also end if the aircraft
! takes off, or if the signal from the NCS is lost.
!
                ELSE IF(CBUFF(IS)(2:2).EQ.'D') THEN !Conditional forcing
                  CALL LOG_MESS
     &                ('Conditional forcing request - '//CBUFF(IS)(4:4))
                  DO I=1,MIN(IL,50)
                    WRITE(6,'(X,A)',IOSTAT=IOS)CBUFF(I)(1:LAN(CBUFF(I)))
                  END DO
                  IF(CBUFF(IS)(4:4).EQ.'0') THEN !Clear all forcing
                    CALL MVBITS(0,0,2,STATUS(47),12)
                  ELSE IF(CBUFF(IS)(4:4).EQ.'1') THEN !Force WAOR
                    CALL MVBITS(1,0,4,STATUS(47),12)
                  ELSE IF(CBUFF(IS)(4:4).EQ.'2') THEN !Force EAOR
                    CALL MVBITS(5,0,4,STATUS(47),12)
                  ELSE IF(CBUFF(IS)(4:4).EQ.'3') THEN !Force POR
                    CALL MVBITS(9,0,4,STATUS(47),12)
                  ELSE IF(CBUFF(IS)(4:4).EQ.'4') THEN !Force IOR
                    CALL MVBITS(13,0,4,STATUS(47),12)
                  END IF
!
! |E = Force immediate position report.  Note the following:
!
! a. If a position report is currently taking place the request is ignored.
! b. If the report completes within 5 mins of the next quarter, then the next
!    routine position report will not take place.
! c. There is no 5 min threshold on immediate reports and they will be
!    sent each time requested, provided any previous position report is
!    complete.
! d. The request will be ignored if position reports have been inhibited
!    by H_DISPLAY (secret option 99).
!
                ELSE IF(CBUFF(IS)(2:2).EQ.'E') THEN !Force position report
                  CALL LOG_MESS
     &                ('Immediate position report request')
                  DO I=1,MIN(IL,50)
                    WRITE(6,'(X,A)',IOSTAT=IOS)CBUFF(I)(1:LAN(CBUFF(I)))
                  END DO
                  ISNDPOS=1
                ELSE IF(CBUFF(IS)(2:2).EQ.'F') THEN !Force status report
!
! |F 0 = Send hardware status
! |F 1 = Send GPS status
! |F 2 = Send miscellaneous status
! |F 3 = Send messages status
!
                  CALL LOG_MESS
     &                ('Status report request - '//CBUFF(IS)(4:4))
                  DO I=1,MIN(IL,50)
                    WRITE(6,'(X,A)',IOSTAT=IOS)CBUFF(I)(1:LAN(CBUFF(I)))
                  END DO
                  READ(CBUFF(IS)(4:4),'(I1)',IOSTAT=IOS) ISTS
                  IF(ISTS.GE.0.AND.ISTS.LE.3.AND.IOS.EQ.0) THEN
                    CALL GET_NXT_OUT(CNXTMES,INXTREC)
                    OPEN(UNIT=13,FILE='[.SATCOM]SAT_OUT_'//CNXTMES//
     &                  '.DAT',STATUS='NEW',CARRIAGECONTROL='LIST',
     &                  IOSTAT=IOS)
                    ISIZ=0
                    IR=LAN(CBUFF(IS+1))
                    WRITE(13,'(A)',IOSTAT=IOS) CBUFF(IS+1)(1:IR)
                    ISIZ=ISIZ+IR
                    IF(ISTS.EQ.2) LSTSUPD=.TRUE. !Force full status update
                    CALL STSUPD(ISTS,CDISP,LSATOK,CBUFF,LSTSUPD)
                    DO I=1,23
                      IF(CDISP(I).NE.' ') ILST=I
                    END DO
                    DO I=1,ILST
                      IR=LAN(CDISP(I))
                      WRITE(13,'(A)',IOSTAT=IOS) CDISP(I)(1:IR)
                      ISIZ=ISIZ+IR
                    END DO
                    CLOSE(13,IOSTAT=IOS)
                    CALL STSLIN(CBUFF(IS+1),CNXTMES,ISIZ,CSLINE)
                    CSLINE(79:80)='TA'   !Show turnaround message
                    IF(ISTS.EQ.0) CSLINE(35:63)='Send hardware status'
                    IF(ISTS.EQ.1) CSLINE(35:63)='Send GPS status'
                    IF(ISTS.EQ.2) CSLINE(35:63)='Send misc status'
                    IF(ISTS.EQ.3) CSLINE(35:63)='Send messages status'
                    WRITE(11,REC=INXTREC,IOSTAT=IOS) CSLINE
                    UNLOCK(11,IOSTAT=IOS)
                    STATUS(47)=IBSET(STATUS(47),8) !Force transmit queue check
                    STATUS(47)=STATUS(47).XOR.'400'X !Negat bit 10 for H_DISPLAY
                  END IF
                ELSE IF(CBUFF(IS)(2:2).EQ.'G') THEN !Force transmission mode
!
! |G E = Use email
! |G P = Use pstn
! |G D = Use default
!
                  CALL LOG_MESS
     &                ('Transmission mode request - '//CBUFF(IS)(4:4))
                  DO I=1,MIN(IL,50)
                    WRITE(6,'(X,A)',IOSTAT=IOS)CBUFF(I)(1:LAN(CBUFF(I)))
                  END DO
                  IF(CBUFF(IS)(4:4).EQ.'E') CMODE='EMAIL'
                  IF(CBUFF(IS)(4:4).EQ.'P') CMODE='PSTN'
                  IF(CBUFF(IS)(4:4).EQ.'D') 
     &                CALL GET_LOGICAL('TRANSMISSION_MODE',CMODE)
                ELSE IF(CBUFF(IS)(2:2).EQ.'P') THEN !Control posn reports
!
! |P B = Use data reports (binary)
! |P M = Use text messages
! |P D = Use default
!
                  CALL LOG_MESS
     &                ('Position report request - '//CBUFF(IS)(4:4))
                  DO I=1,MIN(IL,50)
                    WRITE(6,'(X,A)',IOSTAT=IOS)CBUFF(I)(1:LAN(CBUFF(I)))
                  END DO
                  IF(CBUFF(IS)(4:4).EQ.'B') CPOSN='DATA'
                  IF(CBUFF(IS)(4:4).EQ.'M') CPOSN='MESS'
                  IF(CBUFF(IS)(4:4).EQ.'D') 
     &                CALL GET_LOGICAL('TRANSMISSION_MODE',CPOSN)
                END IF
              END IF
!
!      CALL SATCOM('di',CBUFF,IL)       !Get directory
!      WRITE(6,'(X,A)',IOSTAT=IOS) ' '
!      DO I=1,IL-1
!        WRITE(6,'(X,A)',IOSTAT=IOS) CBUFF(I)(1:LAN(CBUFF(I)))
!      END DO

              IF(.NOT.LTSTFIL) THEN
                CALL SATCOM('de '//CFILE1(1:LAN(CFILE1)),CBUFF,IL) !Del message
                CALL ERRCHK('RX del fil err '//CFILE1(1:LAN(CFILE1)),0,
     &              IL)
              END IF
            END IF        
          END DO
        END IF
!
! Check if the FLASH indicator has been set.  If so it needs clearing after
! a 1.1 s or more, by which time H_DRS_LOG should have passed it on.
!
        IF(LFLSET) THEN
          IF(SECNDS(RFLSET).GT.1.1) THEN
            STATUS(46)=IBCLR(STATUS(46),0)
            LFLSET=.FALSE.
          END IF
        END IF
!
! Time to send position report, if required, and sending has not been inhibited
! by special request.
!
        IF(ISNDPOS.EQ.1.AND.LSATOK.AND..NOT.BTEST(STATUS(47),11)) THEN
          IF(CPOSN.EQ.'DATA') CALL SNDPOS(ISNDPOS,CFNUM)
          IF(CPOSN.EQ.'MESS'.AND.SECNDS(RLSTTRY).GT.120.) THEN
            CBUFF(1)='T'
            CALL MAKE_POSREP(CBUFF(2),LSATOK)
            IF(CBUFF(2).NE.' ') THEN
              RLSTTRY=SECNDS(0.0)
              CALL GET_NXT_OUT(CNXTMES,INXTREC)
              OPEN(UNIT=13,FILE='[.SATCOM]SAT_OUT_'//CNXTMES//
     &            '.DAT',STATUS='NEW',CARRIAGECONTROL='LIST',
     &            IOSTAT=IOS)
!              CALL LOG_MESS('Writing position report')
              ISIZ=0
              DO I=1,2
                IR=LAN(CBUFF(I))
!                WRITE(6,'(X,A)',IOSTAT=IOS) CBUFF(I)(1:IR)
                WRITE(13,'(A)',IOSTAT=IOS) CBUFF(I)(1:IR)
                ISIZ=ISIZ+IR
              END DO
              CLOSE(13,IOSTAT=IOS)
!
              CALL STSLIN(CBUFF(1),CNXTMES,ISIZ,CSLINE)
              CSLINE(79:80)='PR'         !Show position report
              WRITE(11,REC=INXTREC,IOSTAT=IOS) CSLINE
              UNLOCK(11,IOSTAT=IOS)
              STATUS(47)=IBSET(STATUS(47),8) !Force transmit queue check
              STATUS(47)=STATUS(47).XOR.'400'X !Negate bit 10 for H_DISPLAY
              ISNDPOS=0
              ILSTPOS=NINT(SECNDS(0.0)) !Note time of latest report
            END IF
          END IF
          IF(ISNDPOS.EQ.2) RLSTPOS=SECNDS(0.0)
        END IF
!
! Hardware status update required.
!
        IF(BTEST(STATUS(47),0)) THEN   !Update hardware status display
          RLSTUPD0=SECNDS(0.0)         !Note update time
          CALL STSUPD(0,CDISP,LSATOK,CBUFF,LSTSUPD)
          WRITE(12,REC=1,IOSTAT=IOS) CDISP
          UNLOCK(12,IOSTAT=IOS)
          STATUS(47)=IBCLR(STATUS(47),0)
          STATUS(47)=STATUS(47).XOR.'10'X !Negate indicator
        END IF
!
! GPS status update required
!
        IF(BTEST(STATUS(47),1)) THEN   !Update GPS status display
          RLSTUPD1=SECNDS(0.0)         !Note update time
          CALL STSUPD(1,CDISP,LSATOK,CBUFF,LSTSUPD)
          WRITE(12,REC=2,IOSTAT=IOS) CDISP
          UNLOCK(12,IOSTAT=IOS)
          STATUS(47)=IBCLR(STATUS(47),1)
          STATUS(47)=STATUS(47).XOR.'20'X !Negate indicator
        END IF
!
! Misc status update required.  The display consists of a status line, and
! various other information which doesn't change too fast.  To reduce processor
! usage the slowly varying information is only got from the transceiver
! every 5 mins or when it may have changed.
!
        IF(BTEST(STATUS(47),2)) THEN   !Update misc status display
          RLSTUPD2=SECNDS(0.0)         !Note update time
          CALL STSUPD(2,CDISP,LSATOK,CBUFF,LSTSUPD)
          WRITE(12,REC=3,IOSTAT=IOS) CDISP
          UNLOCK(12,IOSTAT=IOS)
          STATUS(47)=IBCLR(STATUS(47),2)
          STATUS(47)=STATUS(47).XOR.'40'X !Negate indicator
        END IF
!
! Messages update required.
!
        IF(BTEST(STATUS(47),3)) THEN   !Update messages 
          RLSTUPD3=SECNDS(0.0)         !Note update time
          CALL STSUPD(3,CDISP,LSATOK,CBUFF,LSTSUPD)
          WRITE(12,REC=4,IOSTAT=IOS) CDISP
          UNLOCK(12,IOSTAT=IOS)
          STATUS(47)=IBCLR(STATUS(47),3)
          STATUS(47)=STATUS(47).XOR.'80'X !Negate indicator
        END IF
!
! GPS system position setting required
!
        IF(BTEST(STATUS(54),0)) THEN   !Update messages 
          CALL SETGPS(ISTAT)
! Until we know it works only try once, not until successful
          STATUS(54)=IBCLR(STATUS(54),0)
!          IF(ISTAT.EQ.1) STATUS(54)=IBCLR(STATUS(54),0)
        END IF
!
! Wait 8 seconds before executing loop again.  There is no point in waiting if
! there is no response from the Satcom transceiver since a 10s timeout has
! taken place anyhow.  While waiting check for any requests for display
! updates and respond immediately if these are new requests - this gives a
! quicker response when a new display is called up.
!
        IF(LSATOK) THEN
          ICNT=0
          LWAITING=.TRUE.
          DO WHILE(LWAITING)
            IF(LFLSET.AND.SECNDS(RFLSET).GT.1.1) LWAITING=.FALSE.
            IF(BTEST(STATUS(47),0).AND.SECNDS(RLSTUPD0).GT.8.)
     &          LWAITING=.FALSE.
            IF(BTEST(STATUS(47),1).AND.SECNDS(RLSTUPD1).GT.8.)
     &          LWAITING=.FALSE.
            IF(BTEST(STATUS(47),2).AND.SECNDS(RLSTUPD2).GT.8.)
     &          LWAITING=.FALSE.
            IF(BTEST(STATUS(47),3).AND.SECNDS(RLSTUPD3).GT.8.)
     &          LWAITING=.FALSE.
            ICNT=ICNT+1
            IF(ICNT.GT.16) LWAITING=.FALSE.
            IF(LWAITING) CALL LIB$WAIT(0.5)
          END DO
        END IF
      END DO
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
!******************************************************************************
      SUBROUTINE ST_MESS(MESSAGE)
!
! Write message to the logical H_SATCOM_STAT in the group logical name
! table.  GRPNAM and SYSNAM privileges are required.
!
      IMPLICIT  NONE
      CHARACTER MESSAGE*(*)
      INTEGER*4 ISTAT
      INCLUDE   '($SYSSRVNAM)'
      INCLUDE   '($LNMDEF)'
      INCLUDE   '($PSLDEF)'

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
      ISTAT=SYS$CRELNM(,'LNM$GROUP','H_SATCOM_STAT',PSL$C_SUPER,TLIST)
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
      SUBROUTINE CONNECT(JCHAN)
!
! Open or close I/O channels, and keep trying every 10 s to open the channel
! to the Satcom transceiver.
!
      IMPLICIT  NONE
      CHARACTER CNAME*64
      INTEGER*2 JOSB(4),JCHAN
      INTEGER*4 ISTAT,IFUNCT,IPASS,ILEN,LIB$GETDVI
      LOGICAL*4 LSUCCESS
      INCLUDE   '($IODEF)'
      INCLUDE   '($DVIDEF)'
      INCLUDE   '($SYSSRVNAM)'
!
! Try to open channel to the transceiver.
!
      IPASS=1
      DO WHILE(.NOT.LSUCCESS.OR.IPASS.EQ.1)
        LSUCCESS=.TRUE.
        ISTAT=SYS$ALLOC('SATCOM_CHAN',,,,) !Allocate the serial port
        IF(.NOT.ISTAT.AND.IPASS.EQ.1) 
     &      CALL LOG_MESS('Unable to allocate output port')
        IF(.NOT.ISTAT) LSUCCESS=.FALSE.
        ISTAT=SYS$ASSIGN('SATCOM_CHAN',JCHAN,,) !Now assign a channel to it
        IF(.NOT.ISTAT.AND.IPASS.EQ.1.AND.LSUCCESS)
     &      CALL LOG_MESS('Unable to assign channel to output port')
        IF(.NOT.ISTAT) LSUCCESS=.FALSE.
        ISTAT=LIB$GETDVI(DVI$_DEVNAM,JCHAN,,,CNAME,ILEN) !Get device name
        IF(.NOT.ISTAT.AND.IPASS.EQ.1.AND.LSUCCESS)
     &      CALL LOG_MESS('Unable to get device information')
        IF(.NOT.ISTAT) LSUCCESS=.FALSE.
        IF(CNAME(2:4).EQ.'LTA') THEN   !If terminal server
          IFUNCT=IO$_TTY_PORT.OR.IO$M_LT_CONNECT !Connect function
          ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFUNCT),JOSB,,,,,,,,) !Make connectn
          IF(.NOT.ISTAT) LSUCCESS=.FALSE.
          IF(.NOT.JOSB(1).AND.IPASS.EQ.1.AND.LSUCCESS)
     &        CALL LOG_MESS('Unable to connect to terminal server')
          IF(.NOT.JOSB(1)) LSUCCESS=.FALSE.
        END IF
        IF(.NOT.LSUCCESS) THEN
          ISTAT=SYS$CANCEL(%VAL(JCHAN))
          IF(CNAME(2:4).EQ.'LTA') THEN !If terminal server
            IFUNCT=IO$_TTY_PORT.OR.IO$M_LT_DISCON !Disconnect function
            ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFUNCT),JOSB,,,,,,,,) !Disconnect
          END IF
          ISTAT=SYS$DASSGN(%VAL(JCHAN))
          ISTAT=SYS$DALLOC('SATCOM_CHAN',)
          CALL LIB$WAIT(10.0)
        END IF
        IPASS=IPASS+1
      END DO
!
      RETURN
      END
!*******************************************************************************
      SUBROUTINE SNDPOS(ISNDPOS,CFNUM)
!
! Sends a position report from the aircraft.  Position reports are sent every
! 15 minutes on the quarter, provided it is at least five minutes from the time
! the previous report was actually sent.  The main program checks this and
! calls this routine when a new report is required.
!
! This subroutine is called when ISNDPOS is 1, indicating that a new report
! needs to be sent.  If it fails to issue a report it returns with ISNDPOS
! still equal to 1.  If there is already a report in action, or it succeeds
! in issuing a report it returns with ISNDPOS equal to 2.
!
! It decides which LES to use on the basis of current LES list.
!
! The report is sent as a data message in ADS format containing the following
! information spread over 20 bytes:
! Bytes  1 to  8 Basic ADS report
! Bytes  9 to 10 ADS response (00e0)
! Bytes 11 to 16 Flight id (48 bits)
! Bytes 17 to 18 Track/heading (12 bits)  true deg
! Bytes 18 to 19 Speed (12 bits)          kt
! Byte  20       Vertical rate (8 bits)   ft/min
! 
! Full details of the coding of the report and its contents may be found in the
! 'Software requirements document ADS report implementation in TT-3024A V2.10'
!
! The following command is used to send the message:
!
! da -o -a 00e1 nnn,10082,1,0
! 
! where nnn is the appropriate LES id, eg 102 for Goonhilly EAOR, 00e1
! specifies the required extended ADS components, and 10082 is the DNID we have
! setup at the Goonhilly and Sentosa LESs.  This command sends the position as 
! a data report, and will always work provided the DNID is not disabled.  No
! delivery confirmation is sought.
!
! Note that no use is made of the PU and PA reporting initiated (polled) from
! the ground, and this capability is disabled as far as possible.
!
! The position reports are transmitted in binary by the Goonhilly LES to the
! modem number associated with the DNID immediately they are received.
!
! The Satcom transceiver variable FLIGHTID is defined to contain the flight
! number when this is available.
!
! The routine will not attempt to issue new position report requests more often
! than every 2 minutes.
!
! The reporting of a successful position report simply means that it has been
! received by the LES.  The LES will try for some time to deliver the
! message without reporting any error.
!
      IMPLICIT  NONE
      BYTE      BBUFF(15000)
      CHARACTER CLINE*80,CBUFF(50)*80,CLES*3,CMESS(23)*80,CFNUM*4
      INTEGER*2 JCHAN
      INTEGER*4 STR$POSITION,ISNDPOS,I,IL,IMCNT,IMSRT,IMNXT,LAN,IOS
      REAL*4    RLSTCALL
      COMMON    JCHAN,BBUFF,CBUFF
      COMMON    /MESSCOM/ IMCNT,IMSRT,IMNXT,CMESS
      DATA      RLSTCALL /0.0/

      IF(SECNDS(RLSTCALL).LT.120.) RETURN !Return if too soon
      RLSTCALL=SECNDS(0.0)             !Note time of subroutine call
      CALL SATCOM('st -q',CBUFF,IL)    !Get queued commands
      CALL ERRCHK('SNDPOS - Queue command error',2,IL)
      IF(IL.LT.2) RETURN               !No response or error
      DO I=2,IL                        !Return if any position reports active
        IF(STR$POSITION(CBUFF(I),'Position report',0).NE.0) THEN
          ISNDPOS=2                    !No need to send another report
          CALL LOG_MESS('SNDPOS - Position report already active')
          RETURN
        END IF
      END DO
      DO I=2,IL                        !Return if a login is active
        IF(STR$POSITION(CBUFF(I),'ogin',0).NE.0) THEN
          CALL LOG_MESS('SNDPOS - Login active')
          RETURN
        END IF
      END DO
      CALL GETLES(CLES)                !Get LES to use
      IF(CLES.EQ.'000') THEN
        CALL LOG_MESS('SNDPOS - No LES available')
        RETURN
      END IF
!
! Now send the report and wait for report issued message.  Before sending the
! report set the flight number.
!
      CALL SATCOM('st -d z',CBUFF,IL)  !Disable PU/PA reporting if enabled
      CALL ERRCHK('SNDPOS - PU/PA disable err',1,IL)
      IF(IL.LT.1) RETURN               !No response or error
      IF(CFNUM(2:4).GE.'000'.AND.CFNUM(2:4).LE.'999')
     &    CALL SATCOM('se -z FLIGHTID='//CFNUM,CBUFF,IL) !Send flight id
      CLINE='da -o -a 00e1 '//CLES//',10082,1,0' !Form report command
      CALL SATCOM(CLINE(1:LAN(CLINE)),CBUFF,IL) !Send report command
      CALL ERRCHK('SNDPOS - Pos report err',1,IL)
      IF(IL.LT.1) RETURN               !No response or error
      CALL LOG_MESS('Position report requested - '//CLES)
      IF(IL.GE.2) THEN                 !Returned with error
        DO I=2,IL
          CMESS(IMNXT)(10:)=CBUFF(I)
          CALL TIME(CMESS(IMNXT)(1:9))
          WRITE(6,'(X,A)',IOSTAT=IOS) 
     &        CMESS(IMNXT)(1:LAN(CMESS(IMNXT))) !Write error to log
          IMNXT=IMNXT+1
          IF(IMNXT.GT.23) IMNXT=1
          IMCNT=MIN(IMCNT+1,23)
          IF(IMCNT.EQ.23) IMSRT=IMNXT
        END DO
        RETURN
      END IF
      ISNDPOS=2                        !Show report sent
      RETURN
      END
!*******************************************************************************
      SUBROUTINE SNDPOSM(ISNDPOS,CFNUM)
!
! Sends a position report from the aircraft.  Position reports are sent every
! 15 minutes on the quarter, provided it is at least five minutes from the time
! the previous report was actually sent.  The main program checks this and
! calls this routine when a new report is required.
!
! This subroutine is called when ISNDPOS is 1, indicating that a new report
! needs to be sent.  If it fails to issue a report it returns with ISNDPOS
! still equal to 1.  If there is already a report in action, or it succeeds
! in issuing a report it returns with ISNDPOS equal to 2.
!
! It decides which LES to use on the basis of current LES list.
!
! The report is sent as a data message in ADS format containing the following
! information spread over 20 bytes:
! Bytes  1 to  8 Basic ADS report
! Bytes  9 to 10 ADS response (00e0)
! Bytes 11 to 16 Flight id (48 bits)
! Bytes 17 to 18 Track/heading (12 bits)  true deg
! Bytes 18 to 19 Speed (12 bits)          kt
! Byte  20       Vertical rate (8 bits)   ft/min
! 
! The routine will not attempt to issue new position report requests more often
! than every 2 minutes.
!
! The reporting of a successful position report simply means that it has been
! received by the LES.  The LES will try for some time to deliver the
! message without reporting any error.
!
      IMPLICIT  NONE
      BYTE      BBUFF(15000)
      CHARACTER CLINE*80,CBUFF(50)*80,CLES*3,CMESS(23)*80,CFNUM*4
      INTEGER*2 JCHAN
      INTEGER*4 STR$POSITION,ISNDPOS,I,IL,IMCNT,IMSRT,IMNXT,LAN,IOS
      REAL*4    RLSTCALL
      COMMON    JCHAN,BBUFF,CBUFF
      COMMON    /MESSCOM/ IMCNT,IMSRT,IMNXT,CMESS
      DATA      RLSTCALL /0.0/

      IF(SECNDS(RLSTCALL).LT.120.) RETURN !Return if too soon
      RLSTCALL=SECNDS(0.0)             !Note time of subroutine call
      CALL SATCOM('st -q',CBUFF,IL)    !Get queued commands
      CALL ERRCHK('SNDPOS - Queue command error',2,IL)
      IF(IL.LT.2) RETURN               !No response or error
!      DO I=2,IL                        !Return if any position reports active
!        IF(STR$POSITION(CBUFF(I),'Position report',0).NE.0) THEN
!          ISNDPOS=2                    !No need to send another report
!          CALL LOG_MESS('SNDPOS - Position report already active')
!          RETURN
!        END IF
!      END DO
      DO I=2,IL                        !Return if a login is active
        IF(STR$POSITION(CBUFF(I),'ogin',0).NE.0) THEN
          CALL LOG_MESS('SNDPOS - Login active')
          RETURN
        END IF
      END DO
      CALL GETLES(CLES)                !Get LES to use
      IF(CLES.EQ.'000') THEN
        CALL LOG_MESS('SNDPOS - No LES available')
        RETURN
      END IF
!
! Now send the report and wait for report issued message.  Before sending the
! report set the flight number.
!
      CALL SATCOM('st -d z',CBUFF,IL)  !Disable PU/PA reporting if enabled
      CALL ERRCHK('SNDPOS - PU/PA disable err',1,IL)
      IF(IL.LT.1) RETURN               !No response or error
      IF(CFNUM(2:4).GE.'000'.AND.CFNUM(2:4).LE.'999')
     &    CALL SATCOM('se -z FLIGHTID='//CFNUM,CBUFF,IL) !Send flight id
      CLINE='da -o -a 00e1 '//CLES//',10082,1,0' !Form report command
      CALL SATCOM(CLINE(1:LAN(CLINE)),CBUFF,IL) !Send report command
      CALL ERRCHK('SNDPOS - Pos report err',1,IL)
      IF(IL.LT.1) RETURN               !No response or error
      CALL LOG_MESS('Position report requested - '//CLES)
      IF(IL.GE.2) THEN                 !Returned with error
        DO I=2,IL
          CMESS(IMNXT)(10:)=CBUFF(I)
          CALL TIME(CMESS(IMNXT)(1:9))
          WRITE(6,'(X,A)',IOSTAT=IOS) 
     &        CMESS(IMNXT)(1:LAN(CMESS(IMNXT))) !Write error to log
          IMNXT=IMNXT+1
          IF(IMNXT.GT.23) IMNXT=1
          IMCNT=MIN(IMCNT+1,23)
          IF(IMCNT.EQ.23) IMSRT=IMNXT
        END DO
        RETURN
      END IF
      ISNDPOS=2                        !Show report sent
      RETURN
      END
!*******************************************************************************
      SUBROUTINE GETLES(CLES)
!
! Get the LES to use by searching the LES list for the current ocean region
! for one of 2,102,202,302.  Returns with 0 if unable to find an LES.
!
      IMPLICIT  NONE
      BYTE      BBUFF(15000)
      CHARACTER CLES*3,CLINE*80,CRES*3,CLSTLES*3,CTIM*8,CLSTTIM*8
      INTEGER*2 JCHAN,JOSB(4),JTERM(4)
      INTEGER*4 IMASK(2),IFNCT,ISTAT,ICNT,IPTR,ILFS,IP,I
      INCLUDE   '($IODEF)'
      INCLUDE   '($SSDEF)'
      INCLUDE   '($SYSSRVNAM)'
      COMMON    JCHAN,BBUFF
      DATA      CLSTLES /'xxx'/, CLSTTIM /'00:00:00'/

      CLES='000'                       !Default return
!
! Form the LES land station network command, add a carriage return and send it
! to the transceiver
!
      IFNCT=IO$_READLBLK.OR.IO$M_PURGE !Purge input buffer
      ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFNCT),JOSB,,,
     &    BBUFF(1),%VAL(0),,,,)
      IF(.NOT.ISTAT.OR..NOT.JOSB(1)) THEN
        CALL LOG_MESS('GETLES - purge error:')
        TYPE *,ISTAT,JOSB(1)
      END IF
      CLINE='st -n'//CHAR(13)
      IFNCT=IO$_WRITELBLK.OR.IO$M_NOFORMAT
      ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFNCT),JOSB,,,
     &    %REF(CLINE),%VAL(6),,,,,)
      IF(.NOT.ISTAT.OR..NOT.(JOSB(1)).OR.JOSB(2).NE.6) THEN
        CALL LOG_MESS('GETLES - command write error:')
        TYPE *,ISTAT,JOSB(1),JOSB(2)
        RETURN
      END IF
!
! Now read until a > is returned.  There is a 10s timeout on reads.
!
      IMASK(1)=8                       !Eight bytes in terminator mask
      IMASK(2)=%LOC(JTERM)             !Address of terminator mask
      JTERM(1)=0
      JTERM(2)=0
      JTERM(3)=0
      JTERM(4)=IBSET(0,14)             !Set bit for >
      IFNCT=IO$_READLBLK.OR.IO$M_TIMED
      ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFNCT),JOSB,,,
     &    BBUFF(1),%VAL(4000),%VAL(10),%VAL(%LOC(IMASK)),,)
      IF(.NOT.ISTAT.OR..NOT.JOSB(1).OR.JOSB(3).NE.62) THEN
        CALL LOG_MESS('GETLES - command response error:')
        TYPE *,ISTAT,JOSB(1),JOSB(2),JOSB(3)
      END IF
      IF(JOSB(1).EQ.SS$_TIMEOUT) RETURN !Timeout
      IF(JOSB(3).NE.62) RETURN         !Buffer overflow - no >
      IF(.NOT.ISTAT.OR..NOT.JOSB(1)) RETURN  !Miscellaneous read error
!
! Sort the data to get result we want
!
      ICNT=JOSB(2)
      IPTR=1
      ILFS=0                           !Line feed count
      DO WHILE(ILFS.LT.5.AND.IPTR.LE.ICNT)
        IF(BBUFF(IPTR).EQ.10) ILFS=ILFS+1
        IPTR=IPTR+1
      END DO
      IF(ILFS.LT.5) THEN
        CALL LOG_MESS('GETLES - line feed error:')
        TYPE *,ILFS
        RETURN
      END IF
      IP=1
      DO WHILE(BBUFF(IPTR).NE.3.AND.IPTR.LE.ICNT.AND.IP.LE.80) !ETX ends list
        CLINE(IP:IP)=CHAR(BBUFF(IPTR)) !Convert line to characters
        IP=IP+1
        IPTR=IPTR+1
      END DO
      DO I=0,19
        CALL STR$ELEMENT(CRES,I,' ',CLINE) !Extract LESs from list
        IF(CRES.EQ.'2')   CLES='  2'
        IF(CRES.EQ.'102') CLES='102'
        IF(CRES.EQ.'202') CLES='202'
        IF(CRES.EQ.'302') CLES='302'
      END DO
      IF(CLES.EQ.'000') THEN
        CALL LOG_MESS('GETLES - No LES found:')
        TYPE *,ICNT,ILFS
      END IF
!
! Send a single carriage return to complete command
!
      CLINE=CHAR(13)
      IFNCT=IO$_WRITELBLK.OR.IO$M_NOFORMAT
      ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFNCT),JOSB,,,
     &    %REF(CLINE),%VAL(1),,,,,)
      IF(.NOT.ISTAT.OR..NOT.(JOSB(1)).OR.JOSB(2).NE.1) THEN
        CALL LOG_MESS('GETLES - CR write error:')
        TYPE *,ISTAT,JOSB(1),JOSB(2),JOSB(3)
        RETURN
      END IF
!
! Wait for terminating colon
!
      JTERM(4)=0
      JTERM(4)=IBSET(0,10)             !Set bit for :
      IFNCT=IO$_READLBLK.OR.IO$M_TIMED
      ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFNCT),JOSB,,,
     &    BBUFF(1),%VAL(4000),%VAL(10),%VAL(%LOC(IMASK)),,)
      IF(.NOT.ISTAT.OR..NOT.(JOSB(1)).OR.JOSB(3).NE.58) THEN
        CALL LOG_MESS('GETLES - colon wait error:')
        TYPE *,ISTAT,JOSB(1),JOSB(2),JOSB(3)
      END IF
!
! Read the final space that terminates the command prompt
!
      JTERM(3)=1                       !Set bit for space
      JTERM(4)=0                       !Clear colon bit
      ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFNCT),JOSB,,,
     &    BBUFF(1),%VAL(1),%VAL(10),%VAL(%LOC(IMASK)),,)
      IF(.NOT.ISTAT.OR..NOT.(JOSB(1)).OR.JOSB(3).NE.32) THEN
        CALL LOG_MESS('GETLES - space wait error:')
        TYPE *,ISTAT,JOSB(1),JOSB(2),JOSB(3)
      END IF
!
! Make a note of any changes and return with result
!
      IF(CLES.NE.'000'.AND.CLES.NE.CLSTLES) THEN
        CALL TIME(CTIM)
        IF(CLSTLES.NE.'xxx') CALL LOG_MESS('GETLES - LES from '
     &      //CLSTLES//' ('//CLSTTIM//') to '//CLES//' ('//CTIM//')')
        CLSTLES=CLES
        CLSTTIM=CTIM
      END IF
      RETURN
      END
!*******************************************************************************
      SUBROUTINE SNDMES(IFSTMES,CFILNO,CMODE)
!
! Takes the file specified by CFILNO, looks at the first line which is decoded
! to find the message destination, and then sends the message to the 
! transceiver for onward transmission.  If the first line of the message
! begins with a Z then the rest of the line is removed and decoded to find
! the required direct destination.  Note that each line message has a cr/lf
! added.  Each message sent is also written to SYS$OUTPUT.
!
! In the case of sonde messages from H_NAVAID, this routine checks if there
! is more than one message waiting to be sent, and if there is it compacts all
! the messages into a single file so that it can be sent with a single 
! transmission.
!
! If the routine succeeds in submitting the transmission request it simply
! returns with no change to the argument.  If it fails to submit the
! transmission it sets CFILNO to 0, puts a suitable status in the status file,
! and sets bit 8 in STATUS(47) so that the queue file is checked again.
!
! This routine always sends data as eight bits using the following transmit
! command, except for special case direct faxes which are slightly different.
!
! tx -c les -e V22B -l 7 -s 0 -t 1 -n
!
! If the logical TRANSMISSION_MODE is defined as EMAIL then sends data using
! special code 67 with the following command.
!
! tx -c les -e 67 -l 7 -s 0 -t 6 -n
!
      IMPLICIT  NONE
      BYTE      BBUFF(15000),BEQUIV
      CHARACTER CFILNO*5,CLSTBUSY*5,CLSTNLES*5,CLINE*80,CBUFF(50)*80,
     &          CBASE*20,CLES*3,CEQUIV*1,CDEST*80,CTRANSM*50,
     &          CTRANSF*50,CMESS(23)*80,CMODE*(*)
      INTEGER*2 JCHAN,JOSB(4),JTERM(4)
      INTEGER*4 STR$POSITION,LAN
      INTEGER*4 IMASK(2),IL,IP1,IP2,IFSTMES,ISIZ,IOS1,IOS2,IOS,I,IFNCT,
     &          ISTAT,IP,IMNXT,IMSRT,IMCNT,ITEMP
      INCLUDE   '($IODEF)'
      INCLUDE   '($SYSSRVNAM)'
      EQUIVALENCE (BEQUIV,CEQUIV)
      COMMON    JCHAN,BBUFF,CBUFF
      COMMON    /MESSCOM/ IMCNT,IMSRT,IMNXT,CMESS
      DATA      CBASE /'441252395778'/ !MRF base station phone number at FNH
      DATA      CLSTBUSY /'00000'/,CLSTNLES /'00000'/

      CALL SATCOM('st -q',CBUFF,IL)    !Get queued commands
      CALL ERRCHK('SNDMES - q command error',2,IL)
      IF(IL.LT.2) THEN
        CFILNO='00000'                 !No current file
        RETURN                         !No response or error
      END IF
      DO I=2,IL                        !Return if any transmissions active
        IP1=STR$POSITION(CBUFF(I),'Transmission',0)
        IP2=STR$POSITION(CBUFF(I),'Transmit',0)
        IF(IP1.NE.0.OR.IP2.NE.0) THEN
          IF(CFILNO.NE.CLSTBUSY) CALL LOG_MESS
     &        ('SNDMES - '//CFILNO//' not sent.  Transmit in progress.')
          CLSTBUSY=CFILNO
          CFILNO='00000'               !No current file
          RETURN
        END IF
      END DO
      DO I=2,IL                        !Return if any logins active
        IF(STR$POSITION(CBUFF(I),'ogin',0).NE.0) THEN
          IF(CFILNO.NE.CLSTNLES) CALL LOG_MESS
     &        ('SNDMES - '//CFILNO//' not sent.  Login in progress.')
          CLSTNLES=CFILNO
          CFILNO='00000'               !No current file
          RETURN
        END IF
      END DO
      CALL GETLES(CLES)                !Get LES to use
      IF(CLES.EQ.'000') THEN
        CALL LOG_MESS('SNDMES - No LES available')
        CFILNO='00000'                 !No current file
        RETURN
      END IF
!
! Check if the message is a sonde message and if it is compact all waiting
! sonde messages into a single file, in which case CFILNO will point to the
! new file.
!
! disabled pending full test      CALL COMPACT(IFSTMES,CFILNO)
!
! Open the specified message file and read the first line to get the 
! destination and addressing requirements.  Write suitable addressing
! information at the start of the message to be sent to the transceiver, and
! also write the first line of the message, providing it doesn't begin with
! a Z, into the store for use at the MRF base station.  Also use the
! addressing information to generate the transmit command which will be 
! required by the transceiver.
!
      ISIZ=0                           !No of bytes in message store
      OPEN(13,FILE='[.SATCOM]SAT_OUT_'//CFILNO//'.DAT',STATUS='OLD',
     &    READONLY,IOSTAT=IOS1)
      READ(13,'(Q,A)',IOSTAT=IOS2) IL,CLINE !Read the first line
      IF((IOS1.OR.IOS2).NE.0) THEN
        CALL LOG_MESS('SNDMES - Unable to read file '//CFILNO)
        TYPE *,IOS1,IOS2
        CLOSE(13,IOSTAT=IOS)
        CALL UPDTSTS(IFSTMES,CFILNO,'Submit failed')
        CFILNO='00000'
        RETURN
      END IF
      CALL LOG_MESS('Sending message '//CFILNO//' - '//CLES)
      WRITE(6,'(X,A)',IOSTAT=IOS) CLINE(1:IL)
      IF(CLINE(1:1).GE.'A'.AND.CLINE(1:1).LE.'Y') THEN
        IF(CMODE.NE.'EMAIL') THEN
          CDEST=CBASE(1:LAN(CBASE))//'+'//CHAR(13)//CHAR(10)//CHAR(13)
     &        //CHAR(10)               !MRF base station
        ELSE
          IF(CLINE(1:1).NE.'T') THEN
            CDEST='to+snoopy@metoffice.com'//CHAR(13)//CHAR(10)//
     &        'subject+Message from G-LUXE '//CFILNO//CHAR(13)//
     &        CHAR(10)//CHAR(13)//CHAR(10)
          ELSE                         !No subject on posn reports
            CDEST='to+snoopy@metoffice.com'//CHAR(13)//
     &        CHAR(10)//CHAR(13)//CHAR(10)
          END IF
        END IF
        DO I=1,LAN(CDEST)              !Write addressing info to store
          ISIZ=ISIZ+1
          CEQUIV=CDEST(I:I)
          BBUFF(ISIZ)=BEQUIV
        END DO
        DO I=1,IL                      !Write the first line to store
          ISIZ=ISIZ+1
          CEQUIV=CLINE(I:I)
          BBUFF(ISIZ)=BEQUIV
        END DO
        ISIZ=ISIZ+1
        BBUFF(ISIZ)=13
        ISIZ=ISIZ+1
        BBUFF(ISIZ)=10
        IF(CMODE.NE.'EMAIL') THEN
          CTRANSM='tx -c '//CLES//' -e V22B -l 7 -s 0 -t 1 -n ' !Transmit command
        ELSE
          CTRANSM='tx -c '//CLES//' -e 67 -l 7 -s 0 -t 6 -n ' !Transmit command
        END IF
      ELSE IF(CLINE(1:1).EQ.'Z') THEN
        IF(CLINE(2:2).NE.'E') THEN
!        IF(CMODE.NE.'EMAIL') THEN
          CDEST=CLINE(4:IL)//'+'//CHAR(13)//CHAR(10)//CHAR(13)
     &        //CHAR(10)
        ELSE
          CDEST='to+'//CLINE(4:IL)//CHAR(13)//CHAR(10)//
     &        'subject+Message from G-LUXE '//CFILNO//CHAR(13)//
     &        CHAR(10)//CHAR(13)//CHAR(10)
        END IF
        DO I=1,LAN(CDEST)
          ISIZ=ISIZ+1
          CEQUIV=CDEST(I:I)
          BBUFF(ISIZ)=BEQUIV
        END DO
        IF(CLINE(2:2).EQ.'F') THEN     !Direct fax destination
          CTRANSM='tx -c '//CLES//' -e T30 -l 7 -s 0 -t 1 -n ' !Transmit command
!        ELSE IF(CLINE(2:2).EQ.'P') THEN !Direct PSTN destination
!          CTRANSM='tx -c '//CLES//' -e V22B -l 7 -s 0 -t 1 -n ' !Transmt command
        ELSE IF(CLINE(2:2).EQ.'M') THEN !Direct mobile destination
          CTRANSM='tx -c '//CLES//' -l 7 -s 0 -t 3 -n ' !Transmit command
        ELSE IF(CLINE(2:2).EQ.'E') THEN !Direct email
          CTRANSM='tx -c '//CLES//' -e 67 -l 7 -s 0 -t 6 -n ' !Transmit command
        END IF
      END IF
!
! Store remainder of file in memory.
!
      READ(13,'(Q,A)',IOSTAT=IOS) IL,CLINE
      DO WHILE(IOS.EQ.0.AND.ISIZ.LT.15000)
        IF(IL.GT.0) THEN
          DO I=1,IL
            ISIZ=ISIZ+1
            IF(ISIZ.LE.15000) THEN
              CEQUIV=CLINE(I:I)
              BBUFF(ISIZ)=BEQUIV
            END IF
          END DO
          WRITE(6,'(X,A)',IOSTAT=IOS) CLINE(1:IL)
        ELSE
          WRITE(6,'(X,A)',IOSTAT=IOS)
        END IF
        ISIZ=ISIZ+1
        IF(ISIZ.LE.15000) BBUFF(ISIZ)=13
        ISIZ=ISIZ+1
        IF(ISIZ.LE.15000) BBUFF(ISIZ)=10
        READ(13,'(Q,A)',IOSTAT=IOS) IL,CLINE
      END DO
      CLOSE(13,IOSTAT=IOS)
!
! Form and send transfer command as a binary transfer with a specified byte
! count, then send the data.  The file is given the name TRnnnnn.TXT on the
! transceiver, where nnnnn corresponds to the file number in the SAT_OUT_STS
! file.  Wait for acknowledgement in the form of a colon or a 10 s timeout.
!
      CTRANSF='tr TAnnnnn.TXT -b nnnnn'//CHAR(13)
      CTRANSF(6:10)=CFILNO
      WRITE(CTRANSF(19:23),'(I5.5)',IOSTAT=IOS) ISIZ
      IFNCT=IO$_READLBLK.OR.IO$M_PURGE !Purge input buffer
      ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFNCT),JOSB,,,
     &    BBUFF(1),%VAL(0),,,,)
      IF(.NOT.ISTAT.OR..NOT.JOSB(1)) THEN
        CALL LOG_MESS('SNDMES - purge error:')
        TYPE *,ISTAT,JOSB(1)
      END IF
      IFNCT=IO$_WRITELBLK.OR.IO$M_NOFORMAT
      ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFNCT),JOSB,,, !Send the command
     &    %REF(CTRANSF),%VAL(24),,,,,)
      IF(.NOT.ISTAT.OR..NOT.JOSB(1).OR.JOSB(2).NE.24) THEN
        CALL LOG_MESS('SNDMES - ASCII command send error:')
        TYPE *,ISTAT,JOSB(1),JOSB(2)
        CALL UPDTSTS(IFSTMES,CFILNO,'Submit failed')
        CFILNO='00000'
        RETURN
      END IF
      CALL LIB$WAIT(0.2)               !Wait for switch to binary
      ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFNCT),JOSB,,, !Send the data
     &    %REF(BBUFF),%VAL(ISIZ),,,,,)
      IF(.NOT.ISTAT.OR..NOT.JOSB(1).OR.JOSB(2).NE.ISIZ) THEN
        CALL LOG_MESS('SNDMES - Binary command send error:')
        TYPE *,ISTAT,JOSB(1),JOSB(2),ISIZ
        CALL UPDTSTS(IFSTMES,CFILNO,'Submit failed')
        CFILNO='00000'
        RETURN
      END IF
!
      IMASK(1)=8                       !Eight bytes in terminator mask
      IMASK(2)=%LOC(JTERM)             !Address of terminator mask
      JTERM(1)=0
      JTERM(2)=0
      JTERM(3)=0
      JTERM(4)=IBSET(0,10)             !Set bit for colon
      IFNCT=IO$_READLBLK.OR.IO$M_TIMED
      ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFNCT),JOSB,,,
     &    BBUFF(1),%VAL(4000),%VAL(10),%VAL(%LOC(IMASK)),,)
      IF(.NOT.ISTAT.OR..NOT.JOSB(1).OR.JOSB(3).NE.58) THEN
        CALL LOG_MESS('SNDMES - command read error:')
        TYPE *,ISTAT,JOSB(1),JOSB(2),JOSB(3)
        CALL UPDTSTS(IFSTMES,CFILNO,'Transfer failed')
        CFILNO='00000'
        RETURN
      END IF
      IF(JOSB(2).EQ.36) THEN           !Satcom has returned an error
        CALL LOG_MESS('SNDMES - Transfer of '//CFILNO//' error:')
        BBUFF(37)=':'
        ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFNCT),JOSB,,, !Read rest of message
     &      BBUFF(38),%VAL(4000-38),%VAL(10),%VAL(%LOC(IMASK)),,)
        CLINE=' '
        CALL TIME(CLINE)
        IP=10
        DO I=28,35+JOSB(2)
          IF(IP.LE.80) CLINE(IP:IP)=CHAR(BBUFF(I))
          IP=IP+1
        END DO
        WRITE(6,'(X,A)',IOSTAT=IOS) CLINE(1:LAN(CLINE))
        CMESS(IMNXT)=CLINE
        IMNXT=IMNXT+1
        IF(IMNXT.GT.23) IMNXT=1
        IMCNT=MIN(IMCNT+1,23)
        IF(IMCNT.EQ.23) IMSRT=IMNXT
!
! Read the final space that terminates the command prompt
!
        JTERM(3)=1                     !Set bit for space
        JTERM(4)=0                     !Clear colon bit
        ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFNCT),JOSB,,,
     &      ITEMP,%VAL(1),%VAL(10),%VAL(%LOC(IMASK)),,)
        CALL UPDTSTS(IFSTMES,CFILNO,'Transfer failed')
        CFILNO='00000'                 !No current file
        RETURN
      END IF
!
! Read the final space that terminates the command prompt
!
      JTERM(3)=1                       !Set bit for space
      JTERM(4)=0                       !Clear colon bit
      ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFNCT),JOSB,,,
     &    ITEMP,%VAL(1),%VAL(10),%VAL(%LOC(IMASK)),,)
      IF(.NOT.ISTAT.OR..NOT.JOSB(1)) THEN
        CALL LOG_MESS('SNDMES - space read error:')
        TYPE *,ISTAT,JOSB(1),JOSB(2),JOSB(3)
        CALL UPDTSTS(IFSTMES,CFILNO,'Transfer failed')
        CFILNO='00000'
        RETURN
      END IF
!
! Now send the transmit command using the correct filename.  The rest of the
! command was formed earlier.
!
!      type *,ctransm(1:lan(ctransm))
!      type *,ctransf(4:14)
      CALL SATCOM(CTRANSM(1:LAN(CTRANSM))//' '//CTRANSF(4:14),
     &    CBUFF,IL)
      IF(IL.LT.1) THEN
        CALL ERRCHK('SNDMES - transmit command err',1,IL)
      ELSE IF(IL.GT.1) THEN            !Returned with error
        CALL LOG_MESS('SNDMES - Transmit failure:')
        CALL TIME(CMESS(IMNXT)(1:9))
        DO I=2,IL
          CMESS(IMNXT)(10:)=CBUFF(I)
          WRITE(6,'(X,A)',IOSTAT=IOS) 
     &        CMESS(IMNXT)(1:LAN(CMESS(IMNXT))) !Write error to log
          IMNXT=IMNXT+1
          IF(IMNXT.GT.23) IMNXT=1
          IMCNT=MIN(IMCNT+1,23)
          IF(IMCNT.EQ.23) IMSRT=IMNXT
        END DO
      END IF
      IF(IL.NE.1) THEN
        CALL UPDTSTS(IFSTMES,CFILNO,'Issue failed')
        CFILNO='00000'                 !No current file
        RETURN
      END IF
!
! Update SAT_OUT_STS.DAT by updating entry for the new message.
!
      CALL UPDTSTS(IFSTMES,CFILNO,'Submitted')

!      CALL SATCOM('di',CBUFF,IL)       !Get directory
!      WRITE(6,'(X,A)',IOSTAT=IOS) ' '
!      DO I=1,IL-1
!        WRITE(6,'(X,A)',IOSTAT=IOS) CBUFF(I)(1:LAN(CBUFF(I)))
!      END DO
!
      RETURN
      END
!*******************************************************************************
      SUBROUTINE Q_CLEAN(IFSTMES)
!
! Cleans up the SAT_OUT_STS.DAT output log file and queue, when H_SATCOM
! starts.  The various status entries are altered as follows:
!
! Queued        to Stopped
! Submitted     to Unknown
! Issued        to Unknown
! Anything else is unchanged
!
! Note that this routine goes through the whole file, however many entries,
! and also sets the first queued entry indicator to zero since when it exits
! there are no queued entries.  Also adjusts record count in there are fewer
! records than the count indicates.
!
      IMPLICIT  NONE
      CHARACTER CLINE*80,CLINE1*80
      INTEGER*4 IFSTMES,IOS1,IOS2,IRECCNT,ICNT,IRCNT,IOS,LAN,I
      INCLUDE   'HCOM_DEF.FOR'

      IFSTMES=0
      READ(11,REC=1,IOSTAT=IOS1) CLINE1
      READ(CLINE1(1:5),'(I5)',IOSTAT=IOS2) IRECCNT
      IF((IOS1.OR.IOS2).NE.0) THEN
        CALL LOG_MESS('Q_CLEAN - log file read error:')
        TYPE *,IOS1,IOS2,IRECCNT,CLINE1(1:LAN(CLINE1))
        RETURN
      END IF
      ICNT=0
      IRCNT=0
      DO I=1,IRECCNT
        READ(11,REC=I+1,IOSTAT=IOS1) CLINE
        IF(IOS1.EQ.0) THEN
          IRCNT=IRCNT+1
          IF(I.EQ.1) READ(CLINE(1:5),'(I5)',IOSTAT=IOS) IFSTMES
          IF(CLINE(66:77).EQ.'Queued') THEN
            ICNT=ICNT+1
            IF(ICNT.EQ.1) WRITE(6,'(/X,A)',IOSTAT=IOS) 
     &          'Transmit queue entries changed to Stopped or Unknown:'
            CLINE(66:77)='Stopped'
            WRITE(11,REC=I+1,IOSTAT=IOS2) CLINE
            WRITE(6,'(X,A)',IOSTAT=IOS) CLINE(1:LAN(CLINE))
          ELSE IF(CLINE(66:77).EQ.'Submitted'.OR.CLINE(66:77).EQ.
     &        'Issued') THEN
            ICNT=ICNT+1
            IF(ICNT.EQ.1) WRITE(6,'(/X,A)',IOSTAT=IOS) 
     &          'Transmit queue entries changed to Stopped or Unknown:'
            CLINE(66:77)='Unknown'
            WRITE(11,REC=I+1,IOSTAT=IOS2) CLINE
            WRITE(6,'(X,A)',IOSTAT=IOS) CLINE(1:LAN(CLINE))
          END IF
          IF(IOS2.NE.0) THEN
            CALL LOG_MESS('Q_CLEAN - write error on log file:')
            TYPE *,IOS2,I
            RETURN
          END IF
        ELSE
          CALL LOG_MESS('Q_CLEAN - read error on log file.  '//
     &        'Will correct.')
          TYPE *,IOS1,I
        END IF
      END DO
      IF(ICNT.GT.0) THEN
        WRITE(6,'(X,A)',IOSTAT=IOS) ' '
        STATUS(47)=STATUS(47).XOR.'400'X !Negate bit 10 for H_DISPLAY
      END IF
      WRITE(CLINE1(1:5),'(I5.5)',IOSTAT=IOS) IRCNT
      CLINE1(7:11)='00000'             !Show no queued entries
      WRITE(11,REC=1,IOSTAT=IOS1) CLINE1
      UNLOCK(11,IOSTAT=IOS2)
      IF((IOS1.OR.IOS2).NE.0) THEN
        CALL LOG_MESS('Q_CLEAN - write error on log file:')
        TYPE *,IOS1,IOS2,' '//CLINE(1:11)
      END IF
      RETURN
      END
!*******************************************************************************
      SUBROUTINE Q_SCAN(IFSTMES,CNEXT)
!
! Scans the transmission queue for the queued entry with the highest priority
! and the earliest date/time.  Only entries with a status of 'Queued' are
! used.  Uses the first queued entry indicator to decide where to start
! scanning.  Returns with CNEXT set to '00000' if there are no queued entries.
! Priorities are as follows (highest first):
!
! FL  Message from flight leader
! NC  Compacted file of NAVAID messages
! NV  Message from NAVAID (sonde) system
! TA  Turnaround messages
! PR  Position report - text
! FT  File transfer messages
!
      IMPLICIT  NONE
      CHARACTER CNEXT*5,CLINE*80
      INTEGER*4 IFSTMES,IOS1,IOS2,IRECCNT,IFIRST,IOS3,IOS4,I,IOS,INXT,
     &          INEXT
      
      CNEXT='00000'
      READ(11,REC=1,IOSTAT=IOS1) CLINE
      UNLOCK(11,IOSTAT=IOS2)
      READ(CLINE(1:5),'(I5)',IOSTAT=IOS3) IRECCNT
      READ(CLINE(7:11),'(I5)',IOSTAT=IOS4) IFIRST
      IF((IOS1.OR.IOS2.OR.IOS3.OR.IOS4).NE.0) THEN
        CALL LOG_MESS('Q_SCAN - log file read error:')
        TYPE *,IOS1,IOS2,IOS3,IOS4,IFIRST,IRECCNT,' '//CLINE(1:11)
        RETURN
      END IF
      IF(IRECCNT.EQ.0) RETURN          !No entries
      IF(IFIRST.EQ.0) RETURN           !No queued entries
      INEXT=0
      DO I=IFIRST,IRECCNT
        READ(11,REC=I+1,IOSTAT=IOS) CLINE
        IF(IOS.NE.0) THEN
          CALL LOG_MESS('Q_SCAN - log file read error:')
          TYPE *,IOS
          UNLOCK(11,IOSTAT=IOS)
          RETURN
        END IF
        IF(CLINE(66:77).EQ.'Queued') THEN
          IF(CLINE(79:80).EQ.'FL') INXT=600000+I
          IF(CLINE(79:80).EQ.'NC') INXT=500000+I
          IF(CLINE(79:80).EQ.'NV') INXT=400000+I
          IF(CLINE(79:80).EQ.'TA') INXT=300000+I
          IF(CLINE(79:80).EQ.'PR') INXT=200000+I
          IF(CLINE(79:80).EQ.'FT') INXT=100000+I
          IF(INXT.GT.INEXT) INEXT=INXT
        END IF
      END DO
      UNLOCK(11,IOSTAT=IOS)
      IF(INEXT.NE.0) INEXT=MOD(INEXT,100000)+IFSTMES-1
      WRITE(CNEXT,'(I5.5)',IOSTAT=IOS) INEXT
      RETURN
      END
!*******************************************************************************
      SUBROUTINE UPDTSTS(IFSTMES,CFILNO,CSTS)
!
! Updates the status information in the transmit log file
!       
      IMPLICIT  NONE
      CHARACTER CSTS*(*),CLINE*80,CFILNO*5
      INTEGER*4 IFSTMES,IOS1,IOS2,IFILNO,IOS3,IOS,I
      INCLUDE   'HCOM_DEF.FOR'

      READ(CFILNO,'(BN,I)',IOSTAT=IOS1) IFILNO
      IF(IFILNO.GT.0) THEN
        I=0
        IOS2=1
        DO WHILE(IOS2.NE.0.AND.I.LT.5) !Try five times in case locked
          READ(11,REC=IFILNO-IFSTMES+2,IOSTAT=IOS2) CLINE
          IF(IOS2.NE.0) CALL LIB$WAIT(0.1)
          I=I+1
        END DO
        CLINE(66:77)=CSTS
        IF(IOS2.EQ.0) WRITE(11,REC=IFILNO-IFSTMES+2,IOSTAT=IOS3) CLINE
        UNLOCK(11,IOSTAT=IOS)
        STATUS(47)=STATUS(47).XOR.'400'X !Negate bit 10 for H_DISPLAY
      END IF
      IF((IOS1.OR.IOS2.OR.IOS3).NE.0) THEN
        CALL LOG_MESS('UPDTSTS - read or write error:')
        TYPE *,IOS1,IOS2,IOS3,' '//CFILNO//' '//CSTS,IFILNO
      END IF
      RETURN
      END
!*******************************************************************************
      SUBROUTINE AZIMELEV(ILAT,ILNG,CNCSS,CAZIM,CELEV)
!
! Given a current position and NCS, computes the azimuth and elevation of
! the satellite and returns them as strings to the nearest degree.
!
! Based on the method used for great circle steering in H_NDU.FOR
!
      IMPLICIT  NONE
      CHARACTER CNCSS*3,CAZIM*3,CELEV*2,CNCS(4)*3
      INTEGER*4 ILAT,ILNG,IOS,I
      REAL*4    RSLNG(4),RSATLNG,RLNG,RLAT,YE,ZE,X,Y,Z,RGCBEAR,RGCRNGE,
     &          RBEAR,RDIST,RELEV

      DATA RSLNG /-55.0,-15.5,64.5,178.0/  !Sat longitudes
      DATA CNCS  /'44 ','144','344','244'/ !Corresponding ocean region NCS's

      RSATLNG=0.
      DO I=1,4
        IF(CNCSS.EQ.CNCS(I)) RSATLNG=RSLNG(I)
      END DO
      IF(RSATLNG.EQ.0.) THEN
        CALL LOG_MESS('AZIMELEV - Unrecognized NCS - '//CNCSS)
        CAZIM='000'
        CELEV='00'
        RETURN
      END IF
      RLNG=FLOAT(ILNG)
      RLAT=FLOAT(ILAT)
!
! Compute coordinates of satellite position on earth in earth axes XE, YE, ZE.
!
      YE=-SIND(RSATLNG-RLNG)
      ZE=COSD(RSATLNG-RLNG)
!
! Rotate point through latitude of P1
!
      X=-ZE*SIND(RLAT)
      Y=YE
      Z=ZE*COSD(RLAT)-1
!
! Compute bearing and range
!
      IF(X.NE.0.) THEN
        RGCBEAR=ATAN2D(-Y,X)           !Bearing in degrees (-180 to 180)
      ELSE
        IF(Y.LT.0.) RGCBEAR=90.
        IF(Y.GE.0.) RGCBEAR=-90.
      END IF
      IF(1.+Z.NE.0.) THEN
        RGCRNGE=ATAN2D(X*COSD(RGCBEAR)-Y*SIND(RGCBEAR),1.+Z) !Range in degrees
      ELSE
        RGCRNGE=90.
      END IF
!
! Adjust bearing and range to 0-360 deg.
!
      RBEAR=RGCBEAR+360.
      IF(RBEAR.LT.0.) RBEAR=RBEAR+360.
      IF(RBEAR.GE.360.) RBEAR=RBEAR-360.
      IF(RGCRNGE.LT.0.) RGCRNGE=RGCRNGE+360.
!
! Now compute the elevation of the satellite assuming the present position is
! 6000 km from the centre of the earth and the satellite is 41700 km from the
! centre of the earth.
!
      RDIST=SQRT(6**2+41.7**2-2*6*41.7*COSD(RGCRNGE))
      RELEV=ACOSD((6**2+RDIST**2-41.7**2)/(2*6*RDIST))-90.
!
! Code the output.
!
      WRITE(CAZIM,'(I3.3)',IOSTAT=IOS) NINT(RBEAR)
      WRITE(CELEV,'(I2.2)',IOSTAT=IOS) NINT(RELEV)
!
      RETURN
      END
!*******************************************************************************
      SUBROUTINE BESTSATS(RLAT,RLNG,RTHRESH,ICNT,CNCSLST)
!
! Given the current latitude and longitude, computes the elevation of each of
! the four Inmarsat satellites, and returns a list of those satellites above
! the specified threshold elevation.  The list contains NCS ids (ie, 44, 144,
! 244 and 344) with the highest elevation satellites first.
!
! Computes Great Circle range and bearing using the method used by the Litton
! LTN-3000 Navigation System. (See Litton's LTN-3000 Navigation System
! Program Description, Section 17, available from Electronic's Section).  The
! the Great Circle range specifies in degrees the angle subtended between the
! current position on earth and the satellite.  From this the elevation angle
! can be calculated.
! 
! Inputs:  Lat and long of current position
!          Threshold elevation (deg)  - typically 0 or 5
!
! Returns: ICNT - The number of satellites exceeding the threshold
!          CNCSLST - List of the satellites above the threshold.
!
      IMPLICIT  NONE
      CHARACTER CNCSLST(4)*3,CNCS(4)*3
      INTEGER*4 IS(4),ISAT,ITEMP,ICNT,I,IJ
      REAL*4    RSLNG(4),RELEV(4),RSATLNG,YE,ZE,X,Y,Z,RGCBEAR,RANGLE,
     &          RDIST,RTEMP,RTHRESH,RLAT,RLNG
      DATA RSLNG /-55.0,-15.5,64.5,178.0/  !Sat longitudes
      DATA CNCS  /' 44','144','344','244'/ !Corresponding ocean region NCS's

      DO ISAT=1,4
        RSATLNG=RSLNG(ISAT)
!
! Compute coordinates of satellite position on earth in earth axes XE, YE, ZE.
!
        YE=-SIND(RSATLNG-RLNG)
        ZE=COSD(RSATLNG-RLNG)
!
! Rotate point through latitude of P1
!
        X=-ZE*SIND(RLAT)
        Y=YE
        Z=ZE*COSD(RLAT)-1
!
! Compute the angle subtended between lines from the centre of the earth to
! the present position and the satellite.
!
        IF(X.NE.0.) THEN
          RGCBEAR=ATAN2D(-Y,X)         !Bearing in degrees (-180 to 180)
        ELSE
          IF(Y.LT.0.) RGCBEAR=90.
          IF(Y.GE.0.) RGCBEAR=-90.
        END IF
        IF(1.+Z.NE.0.) THEN
          RANGLE=ATAN2D(X*COSD(RGCBEAR)-Y*SIND(RGCBEAR),1.+Z) !Range in degrees
        ELSE
          RANGLE=90.
        END IF
!
! Now compute the elevation of the satellite assuming the present position is
! 6000 km from the centre of the earth and the satellite is 41700 km from the
! centre of the earth.
!
        RDIST=SQRT(6**2+41.7**2-2*6*41.7*COSD(RANGLE))
        RELEV(ISAT)=ACOSD((6**2+RDIST**2-41.7**2)/(2*6*RDIST))-90.
        IS(ISAT)=ISAT
! The following two lines are temporary fixes to stop the system trying to
! use the indian and pacific satellites while in sweden and austria, pending
! getting their DNIDs loaded, and getting some confidence back that the
! indian satellite is usable.  These lines should not prevent conditional
! and unconditional forcing of the satellites.
         if(isat.eq.3) relev(isat)=-5.
         if(isat.eq.4) relev(isat)=-5.
      END DO
!
! Now sort the data, keeping satellites with elevations above the threshold,
! and arranging them in order with highest first.
!
      DO IJ=1,3                        !Sort into highest first
        DO I=1,3
          IF(RELEV(I).LT.RELEV(I+1)) THEN
            RTEMP=RELEV(I)
            RELEV(I)=RELEV(I+1)
            RELEV(I+1)=RTEMP
            ITEMP=IS(I)
            IS(I)=IS(I+1)
            IS(I+1)=ITEMP
          END IF
        END DO
      END DO
      ICNT=0                           !Now select those above threshold
      DO I=1,4
        IF(RELEV(I).GE.RTHRESH) THEN
          ICNT=ICNT+1
          CNCSLST(ICNT)=CNCS(IS(I))
        END IF
      END DO
!
      RETURN
      END
!*******************************************************************************
      SUBROUTINE SATCOM(COMMAND,CBUFF,IL)
!
! Sends a command to the Satcom system and returns with the reply.  COMMAND
! is the command to send (less cr), each CBUFF element contains a returned
! line, and IL is the number of lines returned, up to 50.  IL is 0 if the
! transceiver only returned the prompt, but no text lines.  -ve values of IL
! indicate errors as follows:
!
! -1  Write failure to Satcom port
! -2  Read failure from Satcom port
! -3  Read timeout (10s) from Satcom
! -4  More than 4000 characters or 50 lines returned
!
! If the routine is called with IL set to 100 it simply sends the command and
! waits for a response, but does not process the returned data, so that all 
! the arguments are returned unchanged.
!
! If the routine is called with IL set to 101 it doesn't print out the
! SATCOM command read error message.  This is to stop a continuous stream of
! error messages when the SATCOM transceiver is not switched on.
!
      IMPLICIT  NONE
      BYTE      BBUFF(15000)
      CHARACTER COMMAND*(*),CBUFF(50)*80,CLINE*80
      INTEGER*2 JCHAN,JOSB(4),JTERM(4)
      INTEGER*4 LAN,IMASK(2),ICHRS,IL,IFNCT,ISTAT,ILL,IPTR,ICNT,I,
     &          IC,ITEMP
      LOGICAL*4 LDONE
      INCLUDE   '($IODEF)'
      INCLUDE   '($SSDEF)'
      INCLUDE   '($SYSSRVNAM)'
      COMMON    JCHAN,BBUFF
!
! Get the command, add a carriage return and send it to the transceiver
!
      CLINE=COMMAND
      ICHRS=LAN(CLINE)
      IF(ICHRS.GT.79) THEN
        CALL LOG_MESS('SATCOM - Command too long:')
        TYPE *,CLINE
        IL=-4
        RETURN
      END IF
      CLINE=CLINE(1:ICHRS)//CHAR(13)  !Add carriage return
      IF(ICHAR(CLINE(1:1)).NE.3) ICHRS=ICHRS+1 !Don't use cr with CTRL/C
      IFNCT=IO$_READLBLK.OR.IO$M_PURGE !Purge anything in input buffer
      ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFNCT),JOSB,,,
     &    BBUFF(1),%VAL(0),,,,)
      IF(.NOT.ISTAT.OR..NOT.JOSB(1)) THEN
        CALL LOG_MESS('SATCOM - Purge error:')
        TYPE *,ISTAT,JOSB(1),JOSB(2),JOSB(3)
      END IF
      IFNCT=IO$_WRITELBLK.OR.IO$M_NOFORMAT
      ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFNCT),JOSB,,,
     &    %REF(CLINE),%VAL(ICHRS),,,,,)
      IF(.NOT.ISTAT.OR..NOT.(JOSB(1)).OR.JOSB(2).NE.ICHRS) THEN
        CALL LOG_MESS('SATCOM - Command write error:')
        TYPE *,CLINE(1:MAX(1,ICHRS-1))
        TYPE *,ISTAT,JOSB(1),JOSB(2),ICHRS
        IF(IL.NE.100) IL=-1
        RETURN
      END IF
!
! Now keep executing reads, using a colon as a terminating character,
! until the sequence cr/lf/: is returned, indicating the end of the transceiver
! response.  There is a 10s timeout on the reads.
!
      ILL=0                            !Error indicator
      IPTR=1                           !Pointer for storing next data
      LDONE=.FALSE.                    !Will be true when get cr/lf/:
      IMASK(1)=8                       !Eight bytes in terminator mask
      IMASK(2)=%LOC(JTERM)             !Address of terminator mask
      JTERM(1)=0
      JTERM(2)=0
      JTERM(3)=0
      JTERM(4)=IBSET(0,10)             !Set bit for colon
      IFNCT=IO$_READLBLK.OR.IO$M_TIMED
      ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFNCT),JOSB,,,
     &    BBUFF(1),%VAL(4100),%VAL(10),%VAL(%LOC(IMASK)),,)
      IF(.NOT.ISTAT.OR..NOT.JOSB(1).OR.JOSB(3).NE.58) THEN
        IF(IL.NE.101) THEN
          CALL LOG_MESS('SATCOM - Command read error:')
          TYPE *,ISTAT,JOSB(1),JOSB(2),JOSB(3)
        END IF
      END IF
      IF(JOSB(1).EQ.SS$_TIMEOUT) THEN
        ILL=-3                         !Timeout
      ELSE IF(JOSB(3).NE.58) THEN
        ILL=-4                         !Buffer overflow (no colon)
      ELSE IF(.NOT.ISTAT.OR..NOT.JOSB(1)) THEN
        ILL=-2                         !Miscellaneous read error
      END IF
      IF(ILL.LT.0) THEN
        IF(IL.NE.100) IL=ILL
        RETURN
      END IF
      DO WHILE(.NOT.LDONE)
        IPTR=IPTR+JOSB(2)
        BBUFF(IPTR)=58                 !Put in the terminating colon
        IPTR=IPTR+1
        IF(IPTR.GT.4100) THEN
          CALL LOG_MESS('SATCOM - Input exceeds 4100 chars:')
          TYPE *,IPTR
          IF(IL.NE.100) IL=-4
          RETURN
        END IF
        IF(IPTR.LE.3) THEN
          LDONE=.TRUE.
        ELSE IF(JOSB(2).GE.2.AND.BBUFF(IPTR-2).EQ.10.AND. !Look for cmd prompt
     &      BBUFF(IPTR-3).EQ.13) THEN
          LDONE=.TRUE.                 !True if found
        ELSE                           !Else read some more
          ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFNCT),JOSB,,,
     &        BBUFF(IPTR),%VAL(4101-IPTR),%VAL(10),%VAL(%LOC(IMASK)),,)
          IF(.NOT.ISTAT.OR..NOT.JOSB(1).OR.JOSB(3).NE.58) THEN
            CALL LOG_MESS('SATCOM - Command read error 1:')
            TYPE *,ISTAT,JOSB(1),JOSB(2),JOSB(3),(4101-IPTR)
          END IF
          IF(JOSB(1).EQ.SS$_TIMEOUT) THEN
            ILL=-3                     !Timeout
          ELSE IF(JOSB(3).NE.58) THEN
            ILL=-4                     !Buffer overflow (no colon)
          ELSE IF(.NOT.JOSB(1)) THEN
            ILL=-2                     !Miscellaneous read error
          END IF
          IF(ILL.LT.0) THEN
            IF(IL.NE.100) IL=ILL
            RETURN
          END IF
        END IF
      END DO
!
! Read the final space that terminates the command prompt
!
      JTERM(3)=1                       !Set bit for space
      JTERM(4)=0                       !Clear colon bit
      ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFNCT),JOSB,,,
     &    ITEMP,%VAL(1),%VAL(10),%VAL(%LOC(IMASK)),,)
      IF(.NOT.ISTAT.OR..NOT.JOSB(1).OR.JOSB(3).NE.32) THEN
        CALL LOG_MESS('SATCOM - Command read error 2:')
        TYPE *,ISTAT,JOSB(1),JOSB(2),JOSB(3)
      END IF
      IF(JOSB(1).EQ.SS$_TIMEOUT) THEN
        ILL=-3                         !Timeout
      ELSE IF(JOSB(3).NE.32) THEN
        ILL=-4                         !No space
      ELSE IF(.NOT.JOSB(1)) THEN
        ILL=-2                         !Miscellaneous read error
      END IF
      IF(ILL.LT.0.AND.IL.NE.100) THEN
        IL=ILL
        RETURN
      END IF
      IF(IL.EQ.100) RETURN
!
! Now sort the data into lines, using line feed characters to delimit lines.
! Carriage returns and any other control characters are ignored.
!
      ICNT=IPTR-2                      !Get data counter right
      IF(ICNT.LT.0) ICNT=0             !Just in case
      I=1
      DO WHILE(BBUFF(I).NE.10.AND.I.LT.ICNT) !Step over command echo to lf
        I=I+1
      END DO
      IF(BBUFF(I).NE.10) THEN          !Error if line feed not found
        IL=-4
        RETURN
      END IF
      IL=0                             !Count of complete lines
      IC=1                             !Character pointer
      CLINE=' '
      DO WHILE(I.LT.ICNT)
        I=I+1
        IF(BBUFF(I).EQ.9) BBUFF(I)=32  !Convert tabs to spaces
        IF(BBUFF(I).GE.32) THEN
          IF(IC.LE.80) CLINE(IC:IC)=CHAR(BBUFF(I)) !Text character
          IC=IC+1
        END IF
        IF(BBUFF(I).EQ.10) THEN        !Line feed means new line
          IL=IL+1                      !Another complete line
          IF(IL.LE.50) CBUFF(IL)=CLINE
          IC=1
          CLINE=' '
        END IF
      END DO
      IF(IL.GT.50) THEN                !Only 50 lines allowed
        CALL LOG_MESS('SATCOM - More than 50 lines:')
        TYPE *,IL
        IL=-4
      END IF
      RETURN
      END
!*******************************************************************************
      SUBROUTINE COMPACT(IFSTMES,CFILNO)
!
! Compacts multiple sonde messages into a single file so that they can be sent
! with a single transmission.  This routine is only called when H_SATCOM is
! about to transmit a sonde message.  It checks to see whether there are any
! later sonde messages marked as 'Queued', and if so compacts them into a
! single message file, marks it as 'Queued', and returns with CFILNO set to the
! new file so that this is the one that is sent.  All messages which are moved
! into the new file are marked as 'Compacted'.  The new file can have a maximum
! size of 15000 bytes; any messages which do not fit into this space are left
! unchanged.  The single file starts with an 'S' to indicate it is for the 
! Synoptic Data Bank.  Each message within the file can be identified if
! necessary by it starting with an ZCZC and ending with an NNNN.
!
      IMPLICIT  NONE
      CHARACTER CFILNO*5,CNXTMES*5,CLINE*80,CFIL*5,CLINE1*80
      INTEGER*4 IFSTMES,IOS1,IOS2,IOS3,IFILNO,IRECCNT,IOS,I,INVS,
     &          INXTREC,IL,ISIZ,IMSIZ,IOS0

      READ(CFILNO,'(BN,I)',IOSTAT=IOS1) IFILNO !Decode file number
      READ(11,REC=IFILNO-IFSTMES+2,IOSTAT=IOS2) CLINE  !Read file status line
      UNLOCK(11,IOSTAT=IOS3)
      IF((IOS1.OR.IOS2.OR.IOS3).NE.0) THEN
        CALL LOG_MESS('COMPACT - log file read error:')
        TYPE *,IOS1,IOS2,IOS3,IFILNO,' '//CFILNO
        RETURN
      END IF
      IF(CLINE(79:80).NE.'NV') RETURN !Return if not sonde message
      READ(11,REC=1,IOSTAT=IOS1) CLINE
      READ(CLINE(1:5),'(BN,I)',IOSTAT=IOS2) IRECCNT
      IF((IOS1.OR.IOS2).NE.0) THEN
        CALL LOG_MESS('COMPACT - log file read error 1:')
        TYPE *,IOS1,IOS2,IFILNO,' '//CFILNO,IRECCNT,' '//CLINE(1:5)
        UNLOCK(11,IOSTAT=IOS)
        RETURN
      END IF
      I=IFILNO-IFSTMES+2
      INVS=1                           !Number of queued sonde messages
      DO WHILE(I.LE.IRECCNT.AND.INVS.LE.1)
        READ(11,REC=I+1,IOSTAT=IOS) CLINE
        IF(IOS.NE.0) THEN
          CALL LOG_MESS('COMPACT - log file read error 2:')
          TYPE *,IOS,I,IRECCNT,INVS
          UNLOCK(11,IOSTAT=IOS)
          RETURN
        END IF
        I=I+1
        IF(CLINE(79:80).EQ.'NV'.AND.CLINE(66:77).EQ.'Queued')INVS=INVS+1
      END DO
      UNLOCK(11,IOSTAT=IOS)
      IF(INVS.LE.1) RETURN             !Only one sonde message so no compaction
!
! We now know that there are at least two sonde files to be compacted, so go
! through each queued file and add it to a new, compacted file.
!
      CALL GET_NXT_OUT(CNXTMES,INXTREC)
      OPEN(13,FILE='[.SATCOM]SAT_OUT_'//CNXTMES//'.DAT',STATUS='NEW',
     &    CARRIAGECONTROL='LIST',IOSTAT=IOS1)
      WRITE(13,'(A)',IOSTAT=IOS2) 'S'
      IF((IOS1.OR.IOS2).NE.0) THEN
        CALL LOG_MESS('COMPACT - new file open/write error:')
        TYPE *,IOS1,IOS2,CLINE(1:5)
        CLOSE(13,IOSTAT=IOS)
        RETURN
      END IF
      CALL LOG_MESS('Compacting sonde messages')
      ISIZ=3
      DO I=2,INXTREC-1
        READ(11,REC=I,IOSTAT=IOS) CLINE
        IF(IOS.NE.0) THEN
          CALL LOG_MESS('COMPACT - status read error:')
          TYPE *,IOS,I
        END IF
        UNLOCK(11,IOSTAT=IOS)
        IF(CLINE(66:77).EQ.'Queued'.AND.CLINE(79:80).EQ.'NV'.AND.
     &      IOS.EQ.0) THEN
          READ(CLINE(29:32),'(I4)',IOSTAT=IOS) IMSIZ
          IF(ISIZ+IMSIZ.LE.15000) THEN
            WRITE(CFIL,'(I5.5)',IOSTAT=IOS) I+IFSTMES-2
            OPEN(14,FILE='[.SATCOM]SAT_OUT_'//CFIL//'.DAT',STATUS='OLD',
     &          READONLY,IOSTAT=IOS0)
            READ(14,'(Q,A)',IOSTAT=IOS1) IL,CLINE1 !Skip 'S' line
            READ(14,'(Q,A)',IOSTAT=IOS) IL,CLINE1
            DO WHILE(IOS.EQ.0)
              IF(IL.GT.0) THEN
                WRITE(13,'(A)',IOSTAT=IOS3) CLINE1(1:IL)
              ELSE
                WRITE(13,'(A)',IOSTAT=IOS3)
              END IF
              ISIZ=ISIZ+IL+2           !Include space for a cr/lf
              READ(14,'(Q,A)',IOSTAT=IOS) IL,CLINE1
            END DO
            CLOSE(14,IOSTAT=IOS2)
            IF((IOS0.OR.IOS1.OR.IOS2.OR.IOS3).NE.0) THEN
              CALL LOG_MESS('COMPACT - data open/read/write error:')
              TYPE *,IOS0,IOS1,IOS2,IOS3,' '//CFIL
            END IF
            CLINE(66:77)='Compacted'
            WRITE(11,REC=I,IOSTAT=IOS) CLINE
            IF(IOS.NE.0) THEN
              CALL LOG_MESS('COMPACT - log write error:')
              TYPE *,IOS,I
            END IF
            UNLOCK(11,IOSTAT=IOS)
          END IF
        END IF
      END DO
      CLOSE(13,IOSTAT=IOS)
!
! Form status line and write it out to SAT_OUT_STS.DAT.
!
      CLINE=CNXTMES
      CALL DATE1(CLINE(8:16))
      IF(CLINE(8:8).EQ.' ') CLINE(8:8)='0'
      CALL TIME(CLINE(19:26))
      WRITE(CLINE(28:32),'(I5)',IOSTAT=IOS1) ISIZ
      CLINE(35:63)='Synoptic data bank'
      CLINE(66:77)='Queued'
      CLINE(79:80)='NC'
      WRITE(11,REC=INXTREC,IOSTAT=IOS2) CLINE
      IF((IOS1.OR.IOS2).NE.0) THEN
        CALL LOG_MESS('COMPACT - status write error 2:')
        TYPE *,IOS1,IOS2,IRECCNT,ISIZ,' '//CLINE(28:32)
      END IF
      UNLOCK(11,IOSTAT=IOS)
      CFILNO=CNXTMES
      RETURN
      END
!*******************************************************************************
      SUBROUTINE SATINIT(LSATINIT,CLSTFNUM)
!
! Sends initialisation commands to the Satcom system, and sets LSATINIT to
! true if all the commands are executed successfully.
!
      IMPLICIT  NONE
      BYTE      BBUFF(15000)
      CHARACTER CBUFF(50)*80,CLSTFNUM*4
      INTEGER*2 JCHAN
      INTEGER*4 IL1,IL2,IL3,IL4,IL5,IL6,IL7,IL8,IL9,IL10,IL11,IL12
      LOGICAL*4 LSATINIT
      INCLUDE   'HCOM_DEF.FOR'
      COMMON    JCHAN,BBUFF,CBUFF

      CALL ST_MESS('Initialising Satcom system')
      CALL SETREP                       !Setup automatic takeoff/landing repts
      CALL SATCOM('ro 10 1',CBUFF,IL1)  !Route incoming messages to memory
      CALL SATCOM('ro 30 1',CBUFF,IL2)  !Route incoming messages to memory
      CALL SATCOM('ro 31 1',CBUFF,IL3)  !Route incoming messages to memory
      CALL SATCOM('ro 32 1',CBUFF,IL4)  !Route incoming messages to memory
      CALL SATCOM('ro 33 1',CBUFF,IL5)  !Route incoming messages to memory
      CALL SATCOM('se -z PRINTER=OFF',CBUFF,IL6) !Don't try to use printer
      CALL SATCOM('se -a 0',CBUFF,IL7)  !Disable EGC system message reception
      CALL SATCOM('se -i 0',CBUFF,IL8)  !Disable EGC FleetNET message reception
      CALL SATCOM('se -w 0',CBUFF,IL9)  !Disable EGC SafetyNET message reception
      IF(CFNUM(2:4).GE.'000'.AND.CFNUM(2:4).LE.'999') THEN !Set flight ID
        CALL SATCOM('se -z FLIGHTID='//CFNUM,CBUFF,IL10)
        CLSTFNUM=CFNUM
      ELSE
        CALL SATCOM('se -z FLIGHTID=',CBUFF,IL10)
      END IF
      CALL SATCOM('de TA*.TXT',CBUFF,IL11) !Delete any transmit files
      CALL SATCOM('de OUT*.*',CBUFF,IL12) !Delete any transmitted files
      IF(IL1.GE.0.AND.IL2.GE.0.AND.IL3.GE.0.AND.IL4.GE.0.AND.IL5.GE.0
     &    .AND.IL6.GE.0.AND.IL7.GE.0.AND.IL8.GE.0.AND.IL9.GE.0
     &    .AND.IL10.GE.0.AND.IL11.GE.0.AND.IL12.GE.0) THEN
        LSATINIT=.TRUE.
        CALL LOG_MESS('Satcom system initialised')
      ELSE
        CALL LOG_MESS('Satcom initialisation failed:')
        TYPE *,IL1,IL2,IL3,IL4,IL5,IL6,IL7,IL8,IL9,IL10,IL11,IL12
      END IF
      RETURN
      END
!*******************************************************************************
      SUBROUTINE SATINF(LSTSDONE)
!
! Get basic Satcom information and store it in the log.  Set LSTSDONE when
! finished.  Note that there is no error checking in this routine.
!
      IMPLICIT  NONE
      BYTE      BBUFF(15000)
      CHARACTER CBUFF(50)*80,CMESS(23)*80
      INTEGER*2 JCHAN
      INTEGER*4 IL,IOS,IMCNT,IMSRT,IMNXT,I,LAN
      LOGICAL*4 LSTSDONE
      COMMON    JCHAN,BBUFF,CBUFF
      COMMON    /MESSCOM/ IMCNT,IMSRT,IMNXT,CMESS

      CALL ST_MESS('Reading status information')
      CALL LOG_MESS('Satcom status information:')
      CALL SATCOM('st -v',CBUFF,IL)    !Get software version
      DO I=6,10
        WRITE(6,'(X,A)',IOSTAT=IOS) CBUFF(I)(1:LAN(CBUFF(I)))
      END DO
      DO I=13,14
        WRITE(6,'(X,A)',IOSTAT=IOS) CBUFF(I)(1:LAN(CBUFF(I)))
      END DO
      CALL SATCOM('st -i',CBUFF,IL)    !Get transceiver ID
      DO I=1,5
        WRITE(6,'(X,A)',IOSTAT=IOS) CBUFF(I)(1:LAN(CBUFF(I)))
      END DO
      CALL SATCOM('ti',CBUFF,IL)       !Get current time
      DO I=1,2
        WRITE(6,'(X,A)',IOSTAT=IOS) CBUFF(I)(1:LAN(CBUFF(I)))
      END DO
      CALL SATCOM('st -o',CBUFF,IL)    !Get power on statistics
      DO I=1,IL
        WRITE(6,'(X,A)',IOSTAT=IOS) CBUFF(I)(1:LAN(CBUFF(I)))
      END DO
      CALL SATCOM('se -c',CBUFF,IL)    !Get serial port parameters
      DO I=1,il
        WRITE(6,'(X,A)',IOSTAT=IOS) CBUFF(I)(1:LAN(CBUFF(I)))
      END DO
      CALL SATCOM('se -r',CBUFF,IL)    !Get silent status
      DO I=1,2
        WRITE(6,'(X,A)',IOSTAT=IOS) CBUFF(I)(1:LAN(CBUFF(I)))
      END DO
      CALL SATCOM('se -z',CBUFF,IL)    !Get all environmental variables
      DO I=1,IL
        WRITE(6,'(X,A)',IOSTAT=IOS) CBUFF(I)(1:LAN(CBUFF(I)))
      END DO
      CALL SATCOM('st -d',CBUFF,IL)    !Get DNID information
      DO I=1,IL-1
        WRITE(6,'(X,A)',IOSTAT=IOS) CBUFF(I)(1:LAN(CBUFF(I)))
      END DO
      CALL SATCOM('se -l',CBUFF,IL)    !Get NCS list
      WRITE(6,'(X,A)',IOSTAT=IOS) ' '
      DO I=1,IL
        WRITE(6,'(X,A)',IOSTAT=IOS) CBUFF(I)(1:LAN(CBUFF(I)))
      END DO
      CALL SATCOM('st -r',CBUFF,IL)    !Get receive log
      DO I=1,IL-1
        WRITE(6,'(X,A)',IOSTAT=IOS) CBUFF(I)(1:LAN(CBUFF(I)))
      END DO
      CALL SATCOM('st -t',CBUFF,IL)    !Get transmit log
      DO I=1,IL-1
        WRITE(6,'(X,A)',IOSTAT=IOS) CBUFF(I)(1:LAN(CBUFF(I)))
      END DO
      CALL SATCOM('di',CBUFF,IL)       !Get directory
      WRITE(6,'(X,A)',IOSTAT=IOS) ' '
      DO I=1,IL-1
        WRITE(6,'(X,A)',IOSTAT=IOS) CBUFF(I)(1:LAN(CBUFF(I)))
      END DO
      CALL SATCOM('st -l',CBUFF,IL)    !Get status messages
      IF(IL.GT.1) THEN
        DO I=1,IL
          WRITE(6,'(X,A)',IOSTAT=IOS) CBUFF(I)(1:LAN(CBUFF(I)))
          CMESS(IMNXT)(1:9)=' '        !No time tags on old messages
          CMESS(IMNXT)(10:)=CBUFF(I)
          IMNXT=IMNXT+1                ! since don't know when arrived
          IF(IMNXT.GT.23) IMNXT=1
          IMCNT=MIN(IMCNT+1,23)
          IF(IMCNT.EQ.23) IMSRT=IMNXT
        END DO
      END IF
      CALL SATCOM('st -q',CBUFF,IL)    !Get queue entries
      DO I=1,IL
        WRITE(6,'(X,A)',IOSTAT=IOS) CBUFF(I)(1:LAN(CBUFF(I)))
      END DO
      CALL SATCOM('st -s',CBUFF,IL)    !Get signal strength
      DO I=1,2
        WRITE(6,'(X,A)',IOSTAT=IOS) CBUFF(I)(1:LAN(CBUFF(I)))
      END DO
      CALL SATCOM('se -o',CBUFF,IL)    !Get NCS status
      DO I=1,4
        WRITE(6,'(X,A)',IOSTAT=IOS) CBUFF(I)(1:LAN(CBUFF(I)))
      END DO
      CALL SATCOM('st -c',CBUFF,IL)    !Get last login/logout
      WRITE(6,'(X,A)',IOSTAT=IOS) ' '
      WRITE(6,'(X,A)',IOSTAT=IOS) CBUFF(13)(1:LAN(CBUFF(13)))
      CALL SATCOM('ru -l',CBUFF,IL)    !Get GPS status
      WRITE(6,'(X,A)',IOSTAT=IOS) ' '
      DO I=3,4
        WRITE(6,'(X,A)',IOSTAT=IOS) CBUFF(I)(1:LAN(CBUFF(I)))
      END DO
      CALL SATCOM('st -f',CBUFF,IL)    !Get doppler information
      DO I=1,IL
        WRITE(6,'(X,A)',IOSTAT=IOS) CBUFF(I)(1:LAN(CBUFF(I)))
      END DO
      CALL SATCOM('se -p',CBUFF,IL)    !Get position
      DO I=1,6
        WRITE(6,'(X,A)',IOSTAT=IOS) CBUFF(I)(1:LAN(CBUFF(I)))
      END DO
      WRITE(6,'(X,A)',IOSTAT=IOS) ' '
!      
      LSTSDONE=.TRUE.
      RETURN
      END
!*******************************************************************************
      SUBROUTINE ERRCHK(CMESS,IS,IL)
!
! Checks if the supplied value of IL is greater than or equal to IS.  If it is
! does nothing, else writes out CMESS followed by the value of IL.
!
      IMPLICIT  NONE
      CHARACTER CMESS*(*),CLINE*70,CNUM*2
      INTEGER*4 IS,IL,IOS,LAN,I

      IF(IL.LT.IS) THEN
        CLINE=CMESS
        WRITE(CNUM,'(I2)',IOSTAT=IOS) IL
        I=LAN(CLINE)
        I=MIN(66,I+1)
        CLINE(I:)=' - '//CNUM
        CALL LOG_MESS(CLINE(1:I+4))
      END IF
      RETURN
      END
!*******************************************************************************
      SUBROUTINE SATFIL(CFILE,CBUFF,IL)
!
! Reads the file specified by CFILE from the Satcom transceiver.  Puts the
! first 50 lines into CBUFF and any extra lines in [.SATCOM]OVERFLOW.DAT.
! IL returns with the number of lines read (0 to n).  Processes received data
! as follows:
!
!   a. Any control or 8 bit characters are ignored.
!   b. Tabs are converted to spaces.
!   c. Only a max of 80 chars are stored from each line.
!   d. No lines must start with a colon (this signifies the end of message)
!   e. The lines returned do not contain cr/lf's.
!
! IL is 0 if the transceiver only returned the prompt, but no text lines.  -ve
! values of IL indicate errors as follows:
!
! -1  Write failure to Satcom port
! -2  Read failure from Satcom port
! -3  Read timeout (10s) from Satcom
! -4  More than 15000 characters read
!
      IMPLICIT  NONE
      BYTE      BBUFF(15000)
      CHARACTER CFILE*(*),CBUFF(50)*80,CLINE*80
      INTEGER*2 JCHAN,JOSB(4),JTERM(4)
      INTEGER*4 IMASK(2),IL,LAN,ICHRS,IFNCT,ISTAT,IPTR,IOS,ITEMP,I
      LOGICAL*4 LDONE,LOVERF
      INCLUDE   '($IODEF)'
      INCLUDE   '($SSDEF)'
      INCLUDE   '($SYSSRVNAM)'
      COMMON    JCHAN,BBUFF
!
! Form the command and send it to the transceiver
!
      CLINE=CFILE
      ICHRS=LAN(CLINE)
      CLINE='ty '//CLINE(1:ICHRS)//CHAR(13)  !Form command
      ICHRS=ICHRS+4                    !Command length
      IFNCT=IO$_READLBLK.OR.IO$M_PURGE !Purge anything in input buffer
      ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFNCT),JOSB,,,
     &    BBUFF(1),%VAL(0),,,,)
      IF(.NOT.ISTAT.OR..NOT.JOSB(1)) THEN
        CALL LOG_MESS('SATFIL - Purge error:')
        TYPE *,ISTAT,JOSB(1),JOSB(2),JOSB(3)
      END IF
      IFNCT=IO$_WRITELBLK.OR.IO$M_NOFORMAT !Send the type command
      ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFNCT),JOSB,,,
     &    %REF(CLINE),%VAL(ICHRS),,,,,)
      IF(.NOT.ISTAT.OR..NOT.(JOSB(1)).OR.JOSB(2).NE.ICHRS) THEN
        CALL LOG_MESS('SATFIL - Command write error:')
        TYPE *,CLINE(1:MAX(1,ICHRS-1))
        TYPE *,ISTAT,JOSB(1),JOSB(2),ICHRS
        IL=-1
        RETURN
      END IF
!
! Now keep executing reads, using a colon or line feed as a terminating 
! characters, until the sequence lf/: is returned, indicating the end of the 
! transceiver response.  There is a 10s timeout on the reads.
!
      LOVERF=.FALSE.                   !No overflow file open yet
      IL=0                             !No lines yet
      IPTR=1                           !Pointer for storing next data
      LDONE=.FALSE.                    !Will be true when get cr/lf/:
      IMASK(1)=8                       !Eight bytes in terminator mask
      IMASK(2)=%LOC(JTERM)             !Address of terminator mask
      JTERM(1)=IBSET(0,10)             !Set bit for line feed
      JTERM(2)=0
      JTERM(3)=0
      JTERM(4)=IBSET(0,10)             !Set bit for colon
      IFNCT=IO$_READLBLK.OR.IO$M_TIMED
      ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFNCT),JOSB,,,
     &    BBUFF(1),%VAL(256),%VAL(10),%VAL(%LOC(IMASK)),,)
      IF(JOSB(1).EQ.SS$_TIMEOUT) THEN
        IL=-3                          !Timeout
      ELSE IF(JOSB(3).NE.58.AND.JOSB(3).NE.10) THEN
        IL=-4                          !Buffer overflow (no colon/lf)
      ELSE IF(.NOT.ISTAT.OR..NOT.JOSB(1)) THEN
        IL=-2                          !Miscellaneous read error
      END IF
      IF(IL.LT.0) THEN
        CALL LOG_MESS('SATFIL - Data read error:')
        TYPE *,ISTAT,JOSB(1),JOSB(2),JOSB(3)
        RETURN
      END IF
      DO WHILE(.NOT.LDONE)             !Read till get to lf/colon
        IPTR=IPTR+JOSB(2)
        IF(JOSB(3).EQ.58) THEN
          BBUFF(IPTR)=58               !Put in the terminating colon
          IPTR=IPTR+1
        ELSE                           !Line feed means end of line
          CLINE=' '
          DO I=1,MIN(IPTR-1,80)        !Take max of 80 characters
            IF(BBUFF(I).GE.32.AND.BBUFF(I).LE.127) !Transfer valid characters
     &          CLINE(I:I)=CHAR(BBUFF(I))
            IF(BBUFF(I).EQ.9) CLINE(I:I)=' ' !Convert tab to space
          END DO
          IL=IL+1                      !Bump line count
          IF(IL.GT.1.AND.IL.LE.51) THEN !If one of first 50 lines
            CBUFF(IL-1)=CLINE          !Store in array
          ELSE IF(IL.EQ.52) THEN       !First line for overflow file
            OPEN(20,FILE='[.SATCOM]OVERFLOW.DAT;',STATUS='NEW',
     &          CARRIAGECONTROL='LIST',IOSTAT=IOS)
            LOVERF=.TRUE.              !Show overflow file open
            WRITE(20,'(A)',IOSTAT=IOS) CLINE
          ELSE IF(IL.GT.52) THEN       !Another line for overflow file
            WRITE(20,'(A)',IOSTAT=IOS) CLINE
          ELSE                         !Line 1 is ignored
          END IF
          IPTR=1                       !Point to beginning of buffer
        END IF
        IF(IPTR.EQ.2.AND.BBUFF(1).EQ.58) THEN !Look for final lf/colon
          LDONE=.TRUE.                 !True if found
        ELSE                           !Else read some more
          ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFNCT),JOSB,,,
     &        BBUFF(IPTR),%VAL(257-IPTR),%VAL(10),%VAL(%LOC(IMASK)),,)
          IF(JOSB(1).EQ.SS$_TIMEOUT) THEN
            IL=-3                      !Timeout
          ELSE IF(JOSB(3).NE.58.AND.JOSB(3).NE.10) THEN
            IL=-4                      !Buffer overflow (no colon/lf)
          ELSE IF(.NOT.ISTAT.OR..NOT.JOSB(1)) THEN
            IL=-2                      !Miscellaneous read error
          END IF
          IF(IL.LT.0) THEN
            CALL LOG_MESS('SATFIL - Data read error 1:')
            TYPE *,ISTAT,JOSB(1),JOSB(2),JOSB(3)
            IF(LOVERF) CLOSE(20,IOSTAT=IOS) !Close overflow if necessary
            RETURN
          END IF
        END IF
      END DO
      IF(LOVERF) CLOSE(20,IOSTAT=IOS)  !Close overflow file if necessary
      IL=IL-1                          !Adjust line count to miss line 1
!
! Read the final space that terminates the command prompt
!
      JTERM(1)=0                       !Clear line feed bit
      JTERM(2)=0
      JTERM(3)=1                       !Set bit for space
      JTERM(4)=0                       !Clear colon bit
      ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFNCT),JOSB,,,
     &    ITEMP,%VAL(1),%VAL(10),%VAL(%LOC(IMASK)),,)
      IF(JOSB(1).EQ.SS$_TIMEOUT) THEN
        IL=-3                          !Timeout
      ELSE IF(JOSB(3).NE.32) THEN
        IL=-4                          !No space
      ELSE IF(.NOT.ISTAT.OR..NOT.JOSB(1)) THEN
        IL=-2                          !Miscellaneous read error
      END IF
      IF(IL.LT.0) THEN
        CALL LOG_MESS('SATFIL - Data read error 2:')
        TYPE *,ISTAT,JOSB(1),JOSB(2),JOSB(3)
      END IF
      RETURN
      END
!*******************************************************************************
      SUBROUTINE GET_NXT_OUT(CNXTMES,INXTREC)
!
! Looks in the SAT_OUT_STS.DAT file and works out the version number of the
! next output message (CNXTMES) and its position in the file (INXTREC).  Also
! adjusts the first line by adding 1 to the message count, and setting the
! first queued message number to the new message if it was zero.
!
      IMPLICIT  NONE
      CHARACTER CNXTMES*5,CLINE*80,CLINE1*80
      INTEGER*4 INXTREC,IOS,ICOUNT,IFSTQD,IFSTMES

      READ(11,REC=1,IOSTAT=IOS) CLINE
      IF(IOS.NE.0) CLINE='00000 00000'
      READ(CLINE(1:5),'(I5)',IOSTAT=IOS) ICOUNT
      IF(ICOUNT.GT.0) READ(11,REC=2,IOSTAT=IOS) CLINE1
      WRITE(CLINE(1:5),'(I5.5)',IOSTAT=IOS) ICOUNT+1
      READ(CLINE(7:11),'(I5)',IOSTAT=IOS) IFSTQD
      IF(IFSTQD.EQ.0) CLINE(7:11)=CLINE(1:5)
      CLINE(12:80)=' '
      WRITE(11,REC=1,IOSTAT=IOS) CLINE
      UNLOCK(11,IOSTAT=IOS)
      IF(ICOUNT.GT.0) READ(CLINE1(1:5),'(I5)',IOSTAT=IOS) IFSTMES
      IF(ICOUNT.EQ.0) IFSTMES=1
      WRITE(CNXTMES,'(I5.5)',IOSTAT=IOS) IFSTMES+ICOUNT
      INXTREC=ICOUNT+2
      RETURN
      END
!*******************************************************************************
      SUBROUTINE COMPRESS(CSTRING)
!
! Collapses multiple spaces in a string to single spaces
!
      IMPLICIT  NONE
      CHARACTER CSTRING*(*)
      INTEGER*4 IJ,IL,LAN,I

      IJ=1
      IL=LAN(CSTRING)
      DO I=2,IL
        IF(CSTRING(IJ:IJ).NE.' '.OR.CSTRING(I:I).NE.' ') THEN
          IJ=IJ+1
          CSTRING(IJ:IJ)=CSTRING(I:I)
        END IF
      END DO
      IF(IJ.LT.IL) THEN
        DO I=IJ+1,IL
          CSTRING(I:I)=' '
        END DO
      END IF
      RETURN
      END
!*******************************************************************************
      SUBROUTINE INSGS(CSPD)
!
! Returns the INS ground speed in knots as a 3 digit character string.  Returns
! spaces if not available or invalid.
!
      IMPLICIT  NONE
      CHARACTER CSPD*3
      INTEGER*4 IP,IS,IOS
      INTEGER*2 IV16
      INCLUDE   'HCOM_DEF.FOR'

      IV16(IP,IS)=NFDATA(NPLOC(IP)+IS-1,STATUS(1))

      CSPD=' '
      IF(STATUS(1).LT.1.OR.STATUS(1).GT.2) RETURN !Check DRS data available
      IF(STATUS(2).LT.1.OR.STATUS(1).GT.12800) RETURN !Check derived data availa
      IF(STATUS(33).NE.1) RETURN       !Check DRS data valid
      IF(STATUS(34).NE.1) RETURN       !Check derived data valid
      IF(IV16(163,1).NE.'0020'X) RETURN !Check for mechanization flag
      IF(IV16(163,29).NE.'0080'X) RETURN !Check INU in navigate
      IF(IV16(164,1).AND.'F000'X.NE.'0000'X) RETURN !Check no IIU problems
      IF(IV16(167,1).NE.'0000'X) RETURN !Check Millie bits
      IF(IV16(168,1).NE.'FFFF'X) RETURN !Check Millie bits
      WRITE(CSPD(1:3),'(I3.3)',IOSTAT=IOS) NINT(RIGS(STATUS(2))*1.944) !G/S kts
      RETURN
      END
!*******************************************************************************
      SUBROUTINE STSLIN(CLINE,CNXTMES,ISIZ,CSLINE)
!
! Forms a status line for the transmit status file for some types of message
! that start with a |.
!
      IMPLICIT  NONE
      CHARACTER CLINE*80,CNXTMES*5,CSLINE*80,CLINE1*80
      INTEGER*4 ISIZ,IOS

      CSLINE=CNXTMES
      CALL DATE1(CSLINE(8:16))
      IF(CSLINE(8:8).EQ.' ') CSLINE(8:8)='0'
      CALL TIME(CSLINE(19:26))
      WRITE(CSLINE(29:32),'(I4)',IOSTAT=IOS) ISIZ
      IF(CLINE(1:1).GE.'A'.AND.CLINE(1:1).LE.'M') THEN
        OPEN(UNIT=13,FILE='[.SATCOM]SAT_ADDR.DAT',STATUS='OLD',
     &      READONLY,IOSTAT=IOS)
        READ(13,'(A)',IOSTAT=IOS) CLINE1
        DO WHILE(CSLINE(35:63).EQ.' '.AND.IOS.EQ.0)
          IF(CLINE1(1:1).EQ.CLINE(1:1)) CSLINE(35:63)=CLINE1(3:)
          READ(13,'(A)',IOSTAT=IOS) CLINE1
        END DO
        CLOSE(13,IOSTAT=IOS)
      ELSE IF(CLINE(1:1).EQ.'N') THEN
        CSLINE(35:63)='Fax - '//CLINE(3:)
      ELSE IF(CLINE(1:1).EQ.'O') THEN
        CSLINE(35:63)='File - '//CLINE(3:)
!      ELSE IF(CLINE(1:1).EQ.'P') THEN
!        CSLINE(35:63)='MRFPST - '//CLINE(3:)
      ELSE IF(CLINE(1:1).EQ.'Q') THEN
        CSLINE(35:63)='Mail - '//CLINE(3:)
!      ELSE IF(CLINE(1:1).EQ.'R') THEN
!        CSLINE(35:63)='Mail - '//CLINE(3:)
      ELSE IF(CLINE(1:1).EQ.'T') THEN
        CSLINE(35:63)='Position report to Met Office'
      ELSE IF(CLINE(1:2).EQ.'ZE') THEN
        CSLINE(35:63)='Direct mail - '//CLINE(4:)
      ELSE IF(CLINE(1:2).EQ.'ZF') THEN
        CSLINE(35:63)='Direct fax - '//CLINE(4:)
      ELSE IF(CLINE(1:2).EQ.'ZM') THEN
        CSLINE(35:63)='Direct mobile - '//CLINE(4:)
      ELSE IF(CLINE(1:2).EQ.'ZP') THEN
        CSLINE(35:63)='Direct PSTN - '//CLINE(4:)
      END IF
      CSLINE(66:77)='Queued'
      RETURN
      END
!*******************************************************************************
      SUBROUTINE STSUPD(ISTS,CDISP,LSATOK,CBUFF,LSTSUPD)
!
! Produces the status screens required by H_DISPLAY and by received status
! request messages.
!
      IMPLICIT  NONE
      CHARACTER CDISP(23)*80,CBUFF(50)*80,CSATCNT*1,CSTOR(21)*80,
     &          CPDOP*3,CAHLD*1,CVAL*3,CNCS*3,CMESS(23)*80
      INTEGER*4 I,ISTS,IMCNT,IMSRT,IMNXT,IL,IM,ILAT,ILNG,IP,ISTL,IOS,LAN
      LOGICAL*4 LSATOK,LSTSUPD,LOGN
      REAL*4    RLSTUPD

      COMMON    /MESSCOM/ IMCNT,IMSRT,IMNXT,CMESS
      DATA      RLSTUPD /0./

      DO I=1,23
        CDISP(I)=' '
      END DO
!
! Hardware status
!
      IF(ISTS.EQ.0) THEN
        CDISP(1)='Satcom hardware status unavailable'
        IF(.NOT.LSATOK) RETURN
        CALL SATCOM('st -w',CBUFF,IL)  !Show hardware status screen
        CALL ERRCHK('Hardware stat err',18,IL)
        DO I=1,MIN(IL,23)
          CDISP(I)=CBUFF(I)
        END DO
!
! GPS status
!
      ELSE IF(ISTS.EQ.1) THEN
        CDISP(1)='Satcom GPS status unavailable' !Write default
        IF(.NOT.LSATOK) RETURN
        CALL SATCOM('ru -g',CBUFF,IL)  !Show hardware status screen
        CALL ERRCHK('Satcom GPS status err',18,IL)
        DO I=1,MIN(IL,23)
          CDISP(I+I/13)=CBUFF(I)       !Make line 13 a blank
        END DO
!
! Miscellaneous status
!
      ELSE IF(ISTS.EQ.2) THEN
        CDISP(1)='Satcom miscellaneous status unavailable'
        IF(.NOT.LSATOK) RETURN
        CDISP(1)='                             Sync:      Sig:'//
     &      '    GPS:      Azim:      Elev:   '
!
        CALL SATCOM('st -q',CBUFF,IL)
        CALL ERRCHK('Misc stat q err',2,IL)
        IF(IL.GT.1) CDISP(1)(1:12)=CBUFF(2)(20:)
        LOGN=.TRUE.                    !True if logged in
        IF(CDISP(1)(1:12).EQ.'Scanning'.OR.
     &     CDISP(1)(1:12).EQ.'Login'.OR.
     &     CDISP(1)(1:12).EQ.'Logout'.OR.
     &     CDISP(1)(1:12).EQ.'Link Test') LOGN=.FALSE.
!
        CALL SATCOM('se -o',CBUFF,IL)
        CALL ERRCHK('Misc stat NCS err',4,IL)
        CNCS=CBUFF(3)(19:21)           !Get NCS
        IF(LOGN) CDISP(1)(15:27)=CBUFF(3)(24:) !Get ocean
        IF(LOGN.AND.CNCS.EQ.'44 ') CDISP(1)(15:27)=CBUFF(3)(23:)
        CDISP(1)(36:38)=CBUFF(4)(19:)  !Get sync
!
        CALL SATCOM('st -s',CBUFF,IL)
        CALL ERRCHK('Misc stat sig err',2,IL)
        IF(IL.EQ.2) CDISP(1)(46:46)=CBUFF(2)(19:19) !Signal strength
!
        CALL SATCOM('ru -l',CBUFF,IL)  !Get GPS info
        CALL ERRCHK('Misc stat GPS err',4,IL)
        IM=0
        DO I=1,IL
          IF(CBUFF(I)(2:5).EQ.'BGPS') IM=I
        END DO
        IF(IM.GT.0) THEN
          CDISP(1)(54:56)='Acq'
          CSATCNT=CBUFF(IM)(16:16)
          CPDOP=CBUFF(IM)(22:24)
          CAHLD=CBUFF(IM)(31:31)
          IF(CPDOP.GT.'286'.OR.CSATCNT.LT.'3') THEN
            CDISP(1)(54:56)='Acq'
          ELSE IF(CPDOP.LT.'010'.AND.CSATCNT.GE.'4') THEN
            CDISP(1)(54:56)='3-D'
          ELSE IF(CAHLD.EQ.'1'.AND.CSATCNT.EQ.'3') THEN
            CDISP(1)(54:56)='2-D'
          ELSE IF(CPDOP.GT.'013'.AND.CPDOP.LT.'143'
     &        .AND.CSATCNT.EQ.'4') THEN
            CDISP(1)(54:56)='2-D'
          END IF
        END IF
!
        IF((CDISP(1)(54:56).EQ.'3-D'.OR.
     &      CDISP(1)(54:56).EQ.'2-D').AND.LOGN) THEN
          CALL SATCOM('se -p',CBUFF,IL) !Show position
          CALL ERRCHK('Misc stat pos err',4,IL)
          IF(IL.GE.6) THEN
            CALL STR$ELEMENT(CVAL(1:2),2,' ',CBUFF(2)) !Get lat degrees
            READ(CVAL(1:2),'(I2)',IOSTAT=IOS) ILAT
            CALL STR$ELEMENT(CVAL(1:1),5,' ',CBUFF(2)) !Get N/S
            IF(CVAL(1:1).EQ.'S') ILAT=-ILAT
            CALL STR$ELEMENT(CVAL(1:3),6,' ',CBUFF(2)) !Get long degrees
            READ(CVAL(1:3),'(I3)',IOSTAT=IOS) ILNG
            CALL STR$ELEMENT(CVAL(1:1),9,' ',CBUFF(2)) !Get E/W
            IF(CVAL(1:1).EQ.'W') ILNG=-ILNG
            CALL AZIMELEV(ILAT,ILNG,CNCS,CDISP(1)(65:67),
     &          CDISP(1)(76:77))
          END IF
        END IF
        IP=2
!         
        IF(LSTSUPD.OR.SECNDS(RLSTUPD).GT.300.) THEN
          LSTSUPD=.FALSE.
          RLSTUPD=SECNDS(0.0)
          CALL SATCOM('st -r',CBUFF,IL) !Get receive log
          CALL ERRCHK('Misc stat rx log err',2,IL)
          IF(IL.GE.2) THEN
            IP=IP+1
            CDISP(IP)=CBUFF(3)
            CDISP(IP)(75:80)='RX Log'
            IP=IP+1
            DO I=MAX(5,IL-6),IL-2
              IF(IP.LE.23) CDISP(IP)=CBUFF(I)
              IP=IP+1
            END DO
          END IF
!
          CALL SATCOM('st -t',CBUFF,IL) !Get transmit log
          CALL ERRCHK('Misc stat tx log err',2,IL)
          IF(IL.GE.2) THEN
            IP=IP+1
            IF(IP.LE.23) THEN
              CDISP(IP)=CBUFF(3)
              CDISP(IP)(75:80)='TX Log'
            END IF
            IP=IP+1
            DO I=MAX(5,IL-6),IL-2
              IF(IP.LE.23) CDISP(IP)=CBUFF(I)
              IP=IP+1
            END DO
          END IF
!
          IP=IP+1
          CALL SATCOM('st -c',CBUFF,IL) !Get last login/logout
          CALL ERRCHK('Misc stat login err',14,IL)
          IF(IL.GE.13) THEN
            CBUFF(13)(75:80)='Login '
            IF(IP.LE.23) CDISP(IP)=CBUFF(13)
            IP=IP+2
          ELSE IF(IL.GT.0) THEN
            DO I=1,IL
              TYPE *,CBUFF(I)(1:LAN(CBUFF(I)))
            END DO
          END IF
!
          CALL SATCOM('st -d',CBUFF,IL) !Get DNID table
          CALL ERRCHK('Misc stat dnid err',2,IL)
          IF(IL.GE.4) THEN
            DO I=2,IL-2
              IF(CBUFF(I)(1:8).EQ.'Provider') 
     &            CBUFF(I)(75:80)='DNIDs '
              IF(IP.LE.23) CDISP(IP)=CBUFF(I)
              IP=IP+1
            END DO
          END IF
!
          ISTL=MIN(IP-1,23)            !Store lines 3 to n for later use
          DO I=3,ISTL
            CSTOR(I-2)=CDISP(I)
          END DO
        ELSE                           !Just use the values in store
          DO I=3,ISTL
            IP=IP+1
            CDISP(IP)=CSTOR(I-2)
          END DO
          IP=IP+1
        END IF
!
        CALL SATCOM('di',CBUFF,IL)     !Get directory
        CALL ERRCHK('Misc stat dir err',4,IL)
        IF(IL.GE.5) THEN
          IP=IP+1
          DO I=1,IL-2
            IF(I.EQ.1) CBUFF(I)(75:80)='Dir   '
            IF(IP.LE.23) CDISP(IP)=CBUFF(I)
            IP=IP+1
          END DO
        END IF
!
! Message status
!
      ELSE IF(ISTS.EQ.3) THEN
        CDISP(1)='No Satcom messages stored at present'
        IP=IMSRT
        DO I=1,IMCNT
          CDISP(I)=CMESS(IP)
          IP=IP+1
          IF(IP.GT.23) IP=1
        END DO
      END IF
      RETURN
      END
!*******************************************************************************
      SUBROUTINE NCSCHK
!
! Manages the use of ocean regions and network coordinating stations.  The
! basic strategy when flying is to log into the nearest satellite and not to
! change it unless the error rate is greater than 50% and there is another
! satellite closer.  On the ground the system tries to use the nearest 
! satellite but if that fails it tries any other ocean regions which may be
! visible.
!
! However frequently this routine is called it will not make any checks more
! frequently than once a minute.  A login takes about a minute to carry out,
! but this routine returns as soon as the login request has been issued.
! The routine returns immediately if no GPS information is available, since it
! then neither knows whether the aircraft is flying or not, nor what its 
! position is.
!
! Note that the transceiver will automatically try retuning when it has 
! received no signal for about 12 minutes.
!
! The following rules are used:
!
! If flying when first called, log into nearest satellite.
! If flying and using nearest satellite then make no changes.
! If flying and not using nearest satellite and signal strength is <3 and
!   there have been less than 3 frames in the last minute, then change to
!   closest.
! If stationary when first called, try in turn the nearest 3 satellites.
! If stationary signal strength is <3 and there have been less than 3 frames in
!   the last minute (out of a possible 7), then try alternative satellites.
! If stationary and start flying then log into nearest satellite when airspeed
!   exceeds 50 kts.
!
! Bits 12,13,14 and 15 of STATUS(47) can be used to force the system to use
! a particular ocean region regardless of availability (bit 13 set) or to use
! a particular ocean region if possible (bit 12 set).  Bits 14 and 15 indicate
! which ocean region to use.
!
      IMPLICIT  NONE
      BYTE      BBUFF(15000)
      CHARACTER CBUFF(50)*80,CVAL*5,CSPD*3,CNCS(4)*3,CSAT*3,CLSTSPD*3,
     &          CFNCS*3,CISS*1,CBBERR*3
      INTEGER*2 JCHAN,JSAT,JSTS47
      INTEGER*4 STR$POSITION,IL,ISS,IFCNT,ICNT,I,IOS,ILAT,ILNG,LAN,INXT,
     &          ILSTFCNT,IPOS,IFND
      LOGICAL*4 LFIRST,LNAVGD,LTAKEO,LSIGERR,LUNCLR,LCOCLR,LCOSET,
     &          LFND,LCOCHG,LUNSET
      REAL*4    RLSTCALL,RLSTFTIM,RLAT,RLNG,REXPF,RBBERR
      INCLUDE   'HCOM_DEF.FOR'
      COMMON    JCHAN,BBUFF,CBUFF

      DATA LFIRST /.TRUE./,RLSTCALL /0./,CLSTSPD /'000'/,RLSTFTIM /-1./
      DATA JSTS47 /0/,INXT /0/
!
! Return if less than 60s since last call
!
      IF(SECNDS(RLSTCALL).LT.60.) RETURN !Less than a minute since last call
!
! Return if a login is already active
!
      RLSTCALL=SECNDS(0.0)
      CALL SATCOM('st -q',CBUFF,IL)    !Get queued commands
      CALL ERRCHK('NCSCHK - Queue command error',2,IL)
      IF(IL.LT.2) RETURN               !No response or error
      DO I=2,IL                        !Return if any logins active
        IF(STR$POSITION(CBUFF(I),'ogin',0).NE.0) THEN
          CALL LOG_MESS('NCSCHK - Login already active')
          RETURN
        END IF
      END DO
!
! Get current NCS, if any.  Note that the current NCS, as supplied by the 
! se -o command is normally the logged in NCS, but may not be if trying to 
! login into another NCS.
!
      CALL SATCOM('se -o',CBUFF,IL)
      CALL ERRCHK('NCSCHK - NCS error',4,IL)
      IF(IL.LT.4) RETURN               !No response or error
      CSAT=CBUFF(3)(19:21)             !Get current NCS
      IF(CSAT.EQ.'44 ') CSAT=' 44'
      IF(CSAT(2:3).NE.'44') CSAT='000' !None or invalid
!
! Get current latitude, longitude, and speed. Return if there is no nav data
!
      CALL SATCOM('ru -l',CBUFF,IL)    !Get GPS info
      CALL ERRCHK('NCSCHK - GPS command error',3,IL)
      IF(IL.LT.3) RETURN               !No response or error
      LNAVGD=.FALSE.
      DO I=1,IL                        !Check for good Nav status
        IF(CBUFF(I)(2:5).EQ.'BGPS') THEN
          IF(CBUFF(I)(9:9).NE.'1') THEN
            CALL LOG_MESS('NCSCHK - GPS not in navigate')
            RETURN
          ELSE
            LNAVGD=.TRUE.
          END IF
        END IF
      END DO
      IF(.NOT.LNAVGD) THEN
        CALL LOG_MESS('NCSCHK - GPS data not found')
        RETURN
      END IF
!
! Get position
!
      CALL SATCOM('se -p',CBUFF,IL)    !Show position
      CALL ERRCHK('NCSCHK - Position command error',6,IL)
      IF(IL.LT.4) RETURN               !No response or error
      CALL STR$ELEMENT(CVAL(1:2),2,' ',CBUFF(2)) !Get lat degrees
      READ(CVAL(1:2),'(I2)',IOSTAT=IOS) ILAT
      CALL STR$ELEMENT(CVAL(1:1),5,' ',CBUFF(2)) !Get N/S
      IF(CVAL(1:1).EQ.'S') ILAT=-ILAT
      CALL STR$ELEMENT(CVAL(1:3),6,' ',CBUFF(2)) !Get long degrees
      READ(CVAL(1:3),'(I3)',IOSTAT=IOS) ILNG
      CALL STR$ELEMENT(CVAL(1:1),9,' ',CBUFF(2)) !Get E/W
      IF(CVAL(1:1).EQ.'W') ILNG=-ILNG
      RLAT=FLOAT(ILAT)
      RLNG=FLOAT(ILNG)
!
! Get speed
!
      CALL INSGS(CSPD)                 !Get INS ground speed if available
      IF(CSPD.EQ.' ') THEN             !If no INS then use Satcom GS
        CALL COMPRESS(CBUFF(4))
        CALL STR$ELEMENT(CSPD,2,' ',CBUFF(4)) !Get speed in kts
      END IF
      LTAKEO=.FALSE.
      IF(CLSTSPD.LE.'050'.AND.CSPD.GT.'050') LTAKEO=.TRUE. !Detect take offs
      CLSTSPD=CSPD
!
! Get signal strength and bulletin board error rate.
!
      CALL SATCOM('st -w',CBUFF,IL)    !Get status screen
      CALL ERRCHK('NCSCHK - status command error',18,IL)
      IF(IL.LT.18) RETURN              !No response or error
      READ(CBUFF(7)(64:64),'(I1)',IOSTAT=IOS) ISS !Get signal strength
      CVAL=CBUFF(6)(66:70)
      READ(CVAL(1:LAN(CVAL)),'(I)',IOSTAT=IOS) IFCNT !Get frame count
      RBBERR=0.                        !Assume err rate is zero if can't compute
      IF(RLSTFTIM.NE.-1.) THEN
        REXPF=SECNDS(RLSTFTIM)/8.64    !Expected no of frames since last noted
        IF(REXPF.GT.0.) RBBERR=(1.-FLOAT(IFCNT-ILSTFCNT)/REXPF)*100
      END IF
      ILSTFCNT=IFCNT
      RLSTFTIM=SECNDS(0.0)
      LSIGERR=.FALSE.
      IF(ISS.LT.3.AND.RBBERR.GT.50.) LSIGERR=.TRUE.
      WRITE(CISS,'(I1)',IOSTAT=IOS) ISS
      WRITE(CBBERR,'(I3.3)',IOSTAT=IOS) NINT(RBBERR)
!
! Get list of available satellites, best first, with elevations greater than 0.
! Only do this if not in the middle of a cycle of trying all satellites in
! turn.
!
      IF(INXT.EQ.0) THEN
        CALL BESTSATS(RLAT,RLNG,0.0,ICNT,CNCS)
        IF(ICNT.LT.1) THEN
          CALL LOG_MESS('NCSCHK - No sats returned by bestsats')
          RETURN
        END IF
        INXT=1                         !Start at beginning of cycle
      END IF
!
! Check for changes in conditional and unconditional forcing bits.
!
      LCOSET=.FALSE.                   !True when conditional bit set
      IF(BTEST(STATUS(47),12).AND..NOT.BTEST(JSTS47,12)) LCOSET=.TRUE.
      LCOCLR=.FALSE.                   !True when conditional bit cleared
      IF(.NOT.BTEST(STATUS(47),12).AND.BTEST(JSTS47,12)) LCOCLR=.TRUE.
      LUNSET=.FALSE.                   !True if unconditional bit set
      IF(BTEST(STATUS(47),13)) LUNSET=.TRUE.
      LUNCLR=.FALSE.                   !True when unconditional bit cleared
      IF(.NOT.BTEST(STATUS(47),13).AND.BTEST(JSTS47,13)) LUNCLR=.TRUE.
      LCOCHG=.FALSE.                   !True when conditional sat changed
      IF(BTEST(STATUS(47),12).AND..NOT.LCOSET.AND.((STATUS(47).AND.
     &    'C000'X).NE.(JSTS47.AND.'C000'X))) LCOCHG=.TRUE.
      JSTS47=STATUS(47)                !Store latest value for next time
!
! If conditional forcing has just been set, or the conditional forcing
! satellite has just been changed then put the required satellite at the top
! of the satellite list, and move the others down
!
      IF(LCOSET.OR.LCOCHG.OR.LUNSET) THEN !Conditional or unconditional forcing
        JSAT=0
        CFNCS=' 44'
        CALL MVBITS(STATUS(47),14,2,JSAT,0)
        IF(JSAT.NE.0) WRITE(CFNCS(1:1),'(I1)',IOSTAT=IOS) JSAT
        LFND=.FALSE.
        DO I=1,ICNT                    !See if region is already in list
          IF(CNCS(I).EQ.CFNCS) IFND=.TRUE.
        END DO
        IF(.NOT.LFND) THEN             !If not in list add to bottom of list
          ICNT=ICNT+1
          CNCS(ICNT)=CFNCS
        END IF
        DO I=1,ICNT                    !Find position of required ocean in list
          IF(CNCS(I).EQ.CFNCS) IPOS=I
        END DO
        IF(IPOS.NE.1) THEN             !If not at top of list, move it there
          DO I=IPOS,2,-1
            CNCS(I)=CNCS(I-1)
          END DO
          CNCS(1)=CFNCS
        END IF
      END IF
!
! If flying then do login into nearest if not already logged into nearest and:
!
!    This is first pass
! or Have just taken off
! or The error rate and signal thresholds have been exceeded
! or The unconditional forcing bit has just been cleared
! or The conditional forcing bit has just been cleared
! or Not currently logged into any NCS
!
! If flying and conditional forcing bit is set or the conditional forcing 
! satellite is changed then log into required NCS if not already using it.
! Note that if it fails to log into the conditional forcing satellite, it will
! remain logged into whatever NCS it was using before.
!
! If not flying then proceed as follows:
!
!   If first pass, or unconditional bit just cleared, or conditional bit just
!   cleared, then log into nearest if not already logged into it.
!   Else if conditional bit just set then log into requested NCS if not
!   already using it.
!   Else if not logged in anywhere, or error rate too high log into next NCS 
!   in list if not already using it.
!   Else do nothing
!
      IF(LFIRST.OR.LUNCLR.OR.LCOCLR.OR.LCOSET.OR.LCOCHG.OR.LSIGERR
     &    .OR.CSAT.EQ.'000'.OR.LTAKEO.OR.LUNSET) THEN
        IF(LFIRST.OR.LUNCLR.OR.LCOCLR.OR.LCOCLR.OR.LCOSET.OR.LCOCHG.OR.
     &      LTAKEO.OR.CSPD.GT.'050') INXT=1 !Use nearest/forced
        IF(LUNSET.OR.CSPD.GT.'050') ICNT=1 !Only try one NCS
        IF(CSAT.NE.CNCS(INXT)) THEN    !If not already logged in
          IF(LTAKEO) CALL LOG_MESS
     &        ('NCSCHK - Trying login for takeoff')
          IF(LFIRST) CALL LOG_MESS
     &        ('NCSCHK - Trying login for first pass')
          IF(LUNCLR) CALL LOG_MESS
     &        ('NCSCHK - Trying login for uncond bit cleared')
          IF(LUNSET) CALL LOG_MESS
     &        ('NCSCHK - Trying login for uncond bit set')
          IF(LCOCLR) CALL LOG_MESS
     &        ('NCSCHK - Trying login for cond bit cleared')
          IF(LCOSET) CALL LOG_MESS
     &        ('NCSCHK - Trying login for cond bit set')
          IF(LCOCHG) CALL LOG_MESS
     &        ('NCSCHK - Trying login for cond sat changed')
          IF(LSIGERR) CALL LOG_MESS
     &        ('NCSCHK - Trying login for poor signal')
          IF(CSAT.EQ.'000') CALL LOG_MESS
     &        ('NCSCHK - Trying login for no current NCS')
          CALL LOG_MESS('Changing from '//CSAT//' to '//CNCS(INXT)//' ('
     &        //CSPD//' kts, '//CISS//' sig, '//CBBERR//' err)')
          CALL SATCOM('li '//CNCS(INXT),CBUFF,IL) !Try to login
          CALL ERRCHK('NCSCHK - Login command error',0,IL)
        END IF
        INXT=INXT+1                    !Step to next NCS
        IF(INXT.GT.ICNT) INXT=0        !End cycle if no more
      END IF
      LFIRST=.FALSE.
      RETURN
      END
!*******************************************************************************
      SUBROUTINE SETREP
!
! Set up the DNIDs to enable automatic takeoff and landing reports.  This
! doesn't normally need to be done except when a new DNID is loaded, but is 
! done here, as part of the program initialization, for all four DNIDs we use 
! (one in each ocean region), so that we don't have to do it manually when
! a new DNID is loaded.  Automatic takeoff and landing reports are enabled when
! the "st -d u" is used to program unreserved data reporting.  The "st -d z"
! command is then sent, which stops any automatic position reporting, but
! leaves takeoff and landing reporting enabled (after which it can never be
! stopped except by deleting the DNIDs).  This has to be done for each DNID.
! Note that the Satcom system must be seeing a satellite for this to work,
! otherwise it fails because it doesn't know what the current frame is.
! This program has no error return.
!
      IMPLICIT  NONE
      BYTE      BBUFF(15000)
      CHARACTER CBUFF(50)*80
      INTEGER*2 JCHAN
      CHARACTER CDINX(4)*1,CS(4)*6
      INTEGER*4 I,IL,IDNUM,LAN,IOS,I1
      COMMON    JCHAN,BBUFF,CBUFF
      DATA      CDINX /'0','1','2','3'/

      CALL SATCOM('st -d',CBUFF,IL)    !Get DNID list
      CALL ERRCHK(CBUFF,3,IL)
      IF(IL.LT.5) RETURN               !No DNIDs
      IDNUM=IL-4                       !DNID count
      DO I=1,IDNUM
        CS(I)=CBUFF(I+2)(28:33)
      END DO
      DO I=1,IDNUM
        IF(CS(I)(1:2).NE.'49') THEN
          CALL SATCOM('st -c',CBUFF,IL) !Get framecount
          CALL ERRCHK(CBUFF,11,IL)
          IF(IL.LT.11) RETURN
          IF(CBUFF(11)(22:22).NE.'-') THEN !If framecount is valid
            CALL LOG_MESS('SETREP - Changing reporting for '//CS(I))
            CALL STRSND('st -d u')     !Send program PU command
            CALL STRGET('>')           !Wait for response
            CALL STRGET(' ')           !Wait for response
            CALL STRSND(CDINX(I))      !Required DNID index
            CALL STRGET('>')           !Wait for response
            CALL STRGET(' ')           !Wait for response
            CALL STRSND('10')          !Interval (not used)
            CALL STRGET('>')           !Wait for response
            CALL STRGET(' ')           !Wait for response
            CALL STRSND('10')          !Start (not used) - don't use 0
            CALL STRGET('>')           !Wait for response
            CALL STRGET(' ')           !Wait for response
            CALL STRSND('N')           !Include position (not used)
            CALL STRGET('>')           !Wait for response
            CALL STRGET(' ')           !Wait for response
            CALL STRSND('0000')        !ADS field activation (not used)
            CALL STRGET(':')           !Wait for response
            CALL STRGET(' ')           !Wait for response
            CALL STRSND('st -d z')     !Stop PU programming
            CALL STRGET(':')           !Wait for response
            CALL STRGET(' ')           !Wait for response
            CALL SATCOM('st -d',CBUFF,IL)
            DO I1=3,IL-2
              WRITE(6,'(X,A)',IOSTAT=IOS) CBUFF(I1)(1:LAN(CBUFF(I1)))
            END DO
          END IF
        END IF
      END DO
      RETURN
      END
!*******************************************************************************
      SUBROUTINE STRSND(CSTRING)
!
! Sends the command specified in CLINE to the transceiver, followed by a CTRL/C.
! Also purges the input buffer before sending the string
!
      IMPLICIT  NONE
      CHARACTER CLINE*80,CSTRING*(*)
      INTEGER*2 JOSB(4),JCHAN
      INTEGER*4 ILEN,LAN,IFNCT,ITEMP,ISTAT
      INCLUDE   '($IODEF)'
      INCLUDE   '($SSDEF)'
      INCLUDE   '($SYSSRVNAM)'
      COMMON    JCHAN

      ILEN=LAN(CSTRING)
      CLINE=CSTRING(1:ILEN)//CHAR(13)
      IFNCT=IO$_READLBLK.OR.IO$M_PURGE !Purge input buffer
      ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFNCT),JOSB,,,
     &    ITEMP,%VAL(0),,,,)
      IF(.NOT.ISTAT.OR..NOT.JOSB(1)) THEN
        CALL LOG_MESS('SETREP - purge error:')
        TYPE *,ISTAT,JOSB(1)
      END IF
      CLINE=CLINE(1:ILEN)//CHAR(13)
      IFNCT=IO$_WRITELBLK.OR.IO$M_NOFORMAT
      ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFNCT),JOSB,,,
     &    %REF(CLINE),%VAL(ILEN+1),,,,,)
      IF(.NOT.ISTAT.OR..NOT.(JOSB(1)).OR.JOSB(2).NE.ILEN+1) THEN
        CALL LOG_MESS('SETREP - command write error:')
        TYPE *,ISTAT,JOSB(1),JOSB(2)
      END IF
      RETURN
      END
!*******************************************************************************
      SUBROUTINE STRGET(CSTRING)
!
! Reads characters from the transceiver until the required character is
! obtained or a colon in returned.  Doesn't do anything with the received data.
! Returns with the terminating character in C.  There is a 30s timeout on all 
! reads, which is needed if not currently in sync.
!
      IMPLICIT  NONE
      BYTE      BBUFF(15000)
      CHARACTER CSTRING*(*)
      INTEGER*2 JCHAN,JOSB(4),JTERM(4)
      INTEGER*4 IMASK(2),IFNCT,ISTAT,ICHR
      INCLUDE   '($IODEF)'
      INCLUDE   '($SSDEF)'
      INCLUDE   '($SYSSRVNAM)'
      COMMON    JCHAN,BBUFF

      ICHR=ICHAR(CSTRING(1:1))
      IMASK(1)=8                       !Eight bytes in terminator mask
      IMASK(2)=%LOC(JTERM)             !Address of terminator mask

      JTERM(1)=0
      JTERM(2)=0
      JTERM(3)=0
      JTERM(4)=0
      JTERM(ICHR/16+1)=IBSET(JTERM(ICHR/16+1),MOD(ICHR,16))

      IFNCT=IO$_READLBLK.OR.IO$M_TIMED
      ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFNCT),JOSB,,,
     &    BBUFF(1),%VAL(4000),%VAL(30),%VAL(%LOC(IMASK)),,)
      IF(.NOT.ISTAT.OR..NOT.JOSB(1).OR.JOSB(3).NE.ICHR) THEN
        CALL LOG_MESS('SETREP - command response error:')
        TYPE *,ISTAT,JOSB(1),JOSB(2),JOSB(3)
      END IF
      RETURN
      END
!*******************************************************************************
      SUBROUTINE SETGPS(ISTAT)
!
! Sets the GPS with the XR5 position.  This is requested from the H_DISPLAY
! program by the setting of bit 0 in STATUS(54).  If there is no GPS data
! available this program returns without taking any action, and ISTAT set to 0.
!
      IMPLICIT  NONE
      INTEGER*4 ISTAT,IS,ILATD,ILATM,ILATH,ILNGD,ILNGM,ILNGH,IHDG,ISPD,
     &    IL,IOS,LAN
      REAL*4    RLAT,RLNG,RLATM,RLATH,RLNGM,RLNGH
      CHARACTER CLAT*1,CLNG*1,CLINE*80,CSPD*3
      BYTE      BBUFF(15000)
      CHARACTER CBUFF(50)*80
      INTEGER*2 JCHAN
      COMMON    JCHAN,BBUFF,CBUFF
      INCLUDE   'HCOM_DEF.FOR'

      ISTAT=0
      IS=STATUS(2)
      IF(IS.EQ.0) RETURN               !No derivations
      IF(STATUS(35).NE.1) RETURN       !No GPS
      RLAT=RGLAT(IS)
      RLNG=RGLNG(IS)
      IF(RLAT.LT.-90.OR.RLAT.GT.90) RETURN
      IF(RLNG.LT.-180.OR.RLNG.GT.180) RETURN
      CLAT='N'
      IF(RLAT.LT.0.) THEN
        CLAT='S'
        RLAT=-RLAT
      END IF
      ILATD=IFIX(RLAT)
      RLATM=(RLAT-ILATD)*60
      ILATM=IFIX(RLATM)
      RLATH=(RLATM-ILATM)*100
      ILATH=IFIX(RLATH)
      CLNG='E'
      IF(RLNG.LT.0.) THEN
        CLNG='W'
        RLNG=-RLNG
      END IF
      ILNGD=IFIX(RLNG)
      RLNGM=(RLNG-ILNGD)*60
      ILNGM=IFIX(RLNGM)
      RLNGH=(RLNGM-ILNGM)*100
      ILNGH=IFIX(RLNGH)
      IHDG=NINT(RIHDG(IS))
      CALL INSGS(CSPD)
      IF(CSPD.EQ.'   ') THEN
        IHDG=0
        ISPD=0
      ELSE
        READ(CSPD,'(I3)',IOSTAT=IOS) ISPD
      END IF
      WRITE(CLINE,10,IOSTAT=IOS) 'se -p ',ILATD,',',ILATM,',',ILATH,
     &    ','//CLAT//',',ILNGD,',',ILNGM,',',ILNGH,','//CLNG//',',IHDG,
     &    ',',ISPD
      IF(IOS.NE.0) RETURN
      CALL LOG_MESS('Setting GPS position - '//CLINE(1:LAN(CLINE)))
      CALL SATCOM(CLINE(1:LAN(CLINE)),CBUFF,IL)
      IF(IL.LT.0) THEN
        CALL ERRCHK('Satcom not responding',0,IL)
        RETURN
      END IF
      ISTAT=1
      RETURN
10    FORMAT(9(A,I3.3))
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
      SUBROUTINE HM_MESS(CMESS)
C
C Writes an immediate message to the Flight Leader's display, and also stores
C it in the shared file HORACE:H_MESSAGES.LOG.
C These messages should only be sent when there is a
C problem or potential problem which must be drawn to the Flight Leader's
C attention.  OPER privilege is required.  Note that this routine prefaces
C the message with the time that it was received by the H_MESSAGE process, so
C there is not need for users to time stamp their messages.
C
      IMPLICIT      NONE
      INTEGER*4     ISTAT,IFUNCT,MLEN
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
          CALL SYS$GETMSG(%VAL(ISTAT),MLEN,CERRMESS,%VAL(1),) !Just print messge
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
      SUBROUTINE MAKE_POSREP(CLINE,LSATOK)
!
! Generates a position report by interrogating the Satcom's GPS system and
! using the values returned to code up a single line report
!
      INTEGER*4 IVALS(8),I,I1,I2,I3,IOS,IL
      LOGICAL*4 LSATOK
      CHARACTER CLINE*80,CDATE*8,CTIME*10,CZONE*5,CFNUM*4,CBUFF(17)*80,
     &    C1*2,C2*2,C3*2,C4*1,C5*3,C6*6
     
      CLINE=' '
      IF(.NOT.LSATOK) RETURN
      CALL SATCOM('ru -g',CBUFF,IL)
      CALL ERRCHK('Satcom GPS status err',18,IL)
!
      DO I=1,17                        !Remove multiple spaces
        CALL STR_COMPRESS(CBUFF(I))
      END DO
      CALL GET_LOGICAL('FLIGHT_NUMBER',CFNUM)
      IF(CFNUM(2:4).GE.'000'.AND.CFNUM(2:4).LE.'999') CLINE(2:5)=CFNUM
      CALL DATE_AND_TIME(CDATE,CTIME,CZONE,IVALS)
      CLINE(11:18)=CDATE(7:8)//'/'//CDATE(5:6)//'/'//CDATE(3:4)
      CALL STR$ELEMENT(CTIME,11,' ',CBUFF(13))
      CLINE(20:24)=CTIME(1:2)//CTIME(4:5)//'Z'
!
      CALL STR$ELEMENT(C1,2,' ',CBUFF(13)) !Do lat
      CALL STR$ELEMENT(C2,3,' ',CBUFF(13))
      CALL STR$ELEMENT(C3,4,' ',CBUFF(13))
      CALL STR$ELEMENT(C4,5,' ',CBUFF(13))
      READ(C1,'(BN,I)',IOSTAT=IOS) I1
      READ(C2,'(BN,I)',IOSTAT=IOS) I2
      READ(C3,'(BN,I)',IOSTAT=IOS) I3
      WRITE(CLINE(26:27),'(I2)',IOSTAT=IOS) I1
      CLINE(28:28)='o'
      WRITE(CLINE(29:30),'(I2)',IOSTAT=IOS) I2
      CLINE(31:31)='.'
      WRITE(CLINE(32:32),'(I1)',IOSTAT=IOS) I3/10
      CLINE(33:33)=C4
!
      CALL STR$ELEMENT(C5,6,' ',CBUFF(13)) !Do long
      CALL STR$ELEMENT(C2,7,' ',CBUFF(13))
      CALL STR$ELEMENT(C3,8,' ',CBUFF(13))
      CALL STR$ELEMENT(C4,9,' ',CBUFF(13))
      READ(C5,'(BN,I)',IOSTAT=IOS) I1
      READ(C2,'(BN,I)',IOSTAT=IOS) I2
      READ(C3,'(BN,I)',IOSTAT=IOS) I3
      WRITE(CLINE(34:36),'(I3)',IOSTAT=IOS) I1
      CLINE(37:37)='o'
      WRITE(CLINE(38:39),'(I2)',IOSTAT=IOS) I2
      CLINE(40:40)='.'
      WRITE(CLINE(41:41),'(I1)',IOSTAT=IOS) I3/10
      CLINE(42:42)=C4
!
      CALL STR$ELEMENT(C6,2,' ',CBUFF(17)) !Height
      READ(C6,'(BN,I)',IOSTAT=IOS) I1
      WRITE(CLINE(44:51),'(I5,A)',IOSTAT=IOS) I1,' ft'
      CALL STR$ELEMENT(C6,2,' ',CBUFF(15)) !Speed
      READ(C6,'(BN,I)',IOSTAT=IOS) I1
      WRITE(CLINE(52:58),'(I4,A)',IOSTAT=IOS) I1,' kt'
      IF(I1.LT.50) THEN                    !No message if speed < 50 kts
        CLINE=' '
        RETURN
      END IF
      CALL STR$ELEMENT(C6,2,' ',CBUFF(14)) !Heading
      READ(C6,'(BN,I)',IOSTAT=IOS) I1
      WRITE(CLINE(59:66),'(I4,A)',IOSTAT=IOS) I1,' deg'
      CALL STR$ELEMENT(C6,3,' ',CBUFF(16)) !Ascent rate
      READ(C6,'(BN,I)',IOSTAT=IOS) I1
      WRITE(CLINE(67:77),'(I7,A)',IOSTAT=IOS) I1,' f/m'
!
      RETURN
      END
!*******************************************************************************
      SUBROUTINE STR_COMPRESS(CLINE)
!
! Removes multiple spaces from an 80 char string, and converts tabs to spaces
! Also removes leading spaces
!
      IMPLICIT  NONE
      INTEGER*4 I,IL,LAN
      CHARACTER CLINE*80,CTEMP*80

      I=1
      DO WHILE(CLINE(I:I).EQ.' '.AND.I.LT.79)
        I=I+1
      END DO
      CTEMP=CLINE(I:)                  !Remove leading spaces
      CLINE=CTEMP
      DO I=1,LAN(CTEMP)
        IF(ICHAR(CTEMP(I:I)).EQ.9) CTEMP(I:I)=' ' !Convert tabs to spaces
        IF(ICHAR(CLINE(I:I)).EQ.9) CLINE(I:I)=' ' !Convert tabs to spaces
      END DO
      IL=1
      DO I=2,LAN(CTEMP)
        IF(CTEMP(I:I).NE.' ') THEN
          IL=IL+1
          CLINE(IL:IL)=CTEMP(I:I)
        ELSE IF(CLINE(IL:IL).NE.' ') THEN
          IL=IL+1
          CLINE(IL:IL)=' '
        END IF
      END DO
      IF(CLINE(IL:IL).EQ.' ') IL=IL-1
      IF(IL.LT.80) CLINE(IL+1:)=' '    !Blank fill remainder of string
      RETURN
      END
