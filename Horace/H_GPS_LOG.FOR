      PROGRAM H_GPS_LOG
C
C H_GPS_LOG.FOR
C
C This version of H_GPS_LOG has been heavily modified to work with the NAVSTAR 
C XR5M GPS receiver.  It will not work with the XR3.
C
C Solicits data from the GPS receiver, writes the messages to hard and optical
C disks, and puts the coded and decoded message information into the HCOM
C common block.
C
C Must be compiled with the file HCOM_DEF.FOR in the same directory.
C Link with:     $ LINK H_GPS_LOG,HORACE.OLB/LIB,H_COMMON/OPT
C                the contents of GPS1_SUBS must in in HORACE.OLB, or it must
C                be linked in separately.
C
C The program uses STATUS(17) as a pointer to the part of the IGPSMESS array
C containing the latest message.  The program uses the IGPSMESS array in HCOM
C to contain the last two messages to be sent to the display programs.
C
C The program uses the logical name GPS_CHAN to identify the RS232 port to 
C which the GPS receiver is connected.  This port must have been set during
C system startup with the following command:
C
C $ SET TERM GPS_CHAN /PERMANENT /EIGHTBIT /NOTTSYNC /PASTHRU /SPEED=(9600,9600)
C
C Note that no handshaking or flow control takes place, and there may be times
C when data is lost, though in practice this does not seem to occur very often.
C
C This program should not be called before H_DRS_LOG is running, so that
C the various STATUS values will have been initialized, and it is preferable
C that data is actually being logged by H_DRS_LOG so that items such as
C flight number, date and DRS time can be extracted to go in the message file.
C
C The program operates continuously as follows:
C
C 1. Opens the RS232 channel to the GPS receiver, using the logical GPS_CHAN
C
C 2. Opens a new GPSDAT.DAT file on hard disk.
C
C 3. Opens a new GPSDAT.DAT file on optical disk.  If it fails to open
C    these files it will keep trying as it carries out other operations.
C
C 4. Sends an initial message 0 with the flight number, date and time
C
C 5. Once a second, timed with HORACE's clock, solicits GPS messages 18,20,24
C    from the receiver
C
C 6. Writes the messages to the disk files.  The messages are not validated
C    before writing, and may therefore contain invalid messages, due to 
C    transmission errors and other reasons.
C
C 7. Puts each of the three messages into the IGPSMESS array in the HCOM
C    common block in turn over a three second cycle.
C
C 8. Decodes message 24 for lat, long and altitude every 3 secs and puts the
C    decoded values in the DERIVE array.
C
C 9. Every ten minutes writes out a message 0 to the message file, using the
C    latest DRS time.
C
C V1.00  15/05/91  W.D.N.JACKSON
C V1.01  13/02/92  W.D.N.JACKSON 
C        Now has better checking and logging of errors.  Also checks for 
C        the OPTI_STATUS logical to indicate whether the optical disk is
C        available or not.  In the event of any problem the file on the optical
C        disk is closed, and a new one opened when things have sorted
C        themselves out.  Also now writes time that latest data received from 
C        GPS in STATUS(29&30), time of last write to hard disk in STATUS(23&24)
C        and time of last write to optical disk in STATUS(25,26).  These times
C        apply to message 24, the main GPS navigation message.
C V1.02  10/08/92  W.D.N.JACKSON 
Cc        Now sends message 15 and solicits message 31 to get DOPS and azimelvs.
C        Timing changed so that always tries to interrogate for message 24 at
C        the start of each second.  Messages 18,20 and 31 are only sought if
C        there is at least 0.4s left in the second to handle them.
C        Now also checks the value of SYST_STATUS to decide whether to write
C        to the system disk.
C        Now only write messages to disk if the GPS system is something like
C        working - ie not sats not all the same - and the GPS time is similar
C        to the DRS time if it is available. Still writes all messages to
C        H_DISPLAY.
C        Now sends message to flight leader if no sense from GPS for 5 minutes.
C        Now stores the GPS system figure of merit in SPECIAL(n,8).
C        Now writes DRS flight number, date and time, every 5 mins if available
C        Now only solicits M18,M20 and M31 if have had a sensible M24 message.
C        Now solicits M24 once a second, and M18,M20 and M31 on a 3s rolling
C        cycle.
C        Now sends messages to H_DISPLAY in the following 6s cycle: M24,M18,M24,
C        M20,M24,M31
C V1.03  16/09/93  W.D.N.JACKSON 
C        Converted to use GPS1_SUBS routines and work with XR5 GPS messages.
C        All output data uses the same naming conventions as before.  There is
C        no way of telling if a message file contains XR3 or XR5 messages
C        without looking at them.  They each need their own software to
C        process.  Note that for the moment sending of message 15's (5's) and
C        interrogation of message 31 for DOPS and AZIMELEV is not handled.
C V1.04  29/10/93  W.D.N.JACKSON
C        Now correctly resets H_GPS_LOG_STAT after a period with no data.
C V1.05  22/04/94  W.D.N.JACKSON
C        Now loads passes GPS latitude and longitude to H_NDU process in
C        IGPSMESS; use values of -999. if present data is invalid.
C V1.06  28/08/96  W.D.N.JACKSON
C        Now also loads GPS height, time, and UTC offset into IGPSMESS for use
C        by the H_NAVAID_NAV process IGPSMESS; uses values of -999. if present
C        data is invalid.
C V1.07  02/10/97  W.D.N.JACKSON
C        Now passes data to the NDU even when the nav degraded bit is set.
C        Now opens new files if the flight number is changed.
C V1.08  26/02/98  W.D.N.JACKSON
C        Now handles V4.1 firmware
C V1.09  25/06/98  W.D.N.JACKSON
C        Now passes data to NDU even if receiver fault bit set
C V1.10  03/10/02  W.D.N.JACKSON
C        Now gets flight number from FLIGHT_NUMBER logical so can obtain flight
C        letter as well.
C
      IMPLICIT  NONE
      INTEGER*4 ISTATS,ISTATO,ICYCLM,ICYCLD,M24TO,M18TO,M20TO,M31TO,
     &    ILOOPCNT,ISTAT,IOS,ILOOP,J1,J2,JJHRS,JJMIN,JJSEC,IDTIM,IGTIM,
     &    IDIFF,IFOM,IALTHLD,IDSTS1,IDSTS2,IDSTS3,IUTC,IMESSNUM,IRECID,
     &    IDATUM,I,IT1,IT2
      LOGICAL*4 LHDROKS,LHDROKO,LVALDRS,LSNDM15,LNDUOK,LSTART,LM24OK,
     &    LMESSNT,LGPSOK
      INTEGER*2 IMESS(103),JWEEKS,JUTCOF
      CHARACTER CFLTNO*4,CDATE*9,CTIME*8,CLINE*58,CSYSSTS*12,COPTSTS*12
      CHARACTER CGTIME*11,CLSTFLTNO*4
      REAL*4    RRV(3),RERRSRT,RGSECS,RGGLAT,RGGLNG,RGGREF,RVEPE,RHEPE,
     &    RRLAT,RRLNG,RRALTMSL,RRALTREF,RERRTIM
      INTEGER*4 ISV(4),ITRKSTS(4),GPS1_ILONGWR
      INCLUDE   'HCOM_DEF.FOR'
      EQUIVALENCE (RGGLAT,IGPSMESS(127,1))
      EQUIVALENCE (RGGLNG,IGPSMESS(127,2))
      EQUIVALENCE (RGGREF,IGPSMESS(125,1))
      EQUIVALENCE (RGSECS,IGPSMESS(125,2))
      EQUIVALENCE (JWEEKS,IGPSMESS(124,1))
      EQUIVALENCE (JUTCOF,IGPSMESS(124,2))

      STATUS(35)=0                     !A one indicates GPS data is valid
      CALL LOG_MESS('H_GPS_LOG starting') !Send message to log file
      CALL ST_MESS('Starting')         !Set status logical
C
C Open channel to the receiver, and the GPSDAT.DAT files.  Note that the 
C program aborts if it fails to open the GPS receiver channel, but will
C continue if it fails to open the GPSDAT.DAT files.
C
      CALL GPS1_MESS_OPEN('GPS_CHAN')   !Open connection to GPS receiver
      CALL GET_DISK_STATUS(CSYSSTS,COPTSTS) !Check disk availability
      ISTATS=1                         !0 if GPSDAT file open on system disk
      IF(CSYSSTS.NE.'Unavailable') THEN !If system disk available
        CALL GPS1_MESSF_CREATE('HORACE:GPSDAT.DAT',1,ISTATS) !Open file
        IF(ISTATS.EQ.0) CALL LOG_MESS('HORACE:GPSDAT.DAT opened')
      END IF
      ISTATO=1                         !0 if GPSDAT file open on DUB0:
      IF(COPTSTS.NE.'Unavailable') THEN !If optical disk available
        CALL GPS1_MESSF_CREATE('DUB0:[HORACE]GPSDAT.DAT',2,ISTATO) !Open file
        IF(ISTATO.EQ.0) CALL LOG_MESS('DUB0:GPSDAT.DAT opened')
      END IF
C
C Get the flight number, date, and time from the DRS, code it as message 0
C and write it as the first message in each file.  If no data is available
C from the DRS at this time a message 0 is still written using blanks.  This
C ensures that the message files are still processable by the TARDIS programs
C but not by the TARDIS routine procedures.  When the DRS data becomes 
C the current GPS files will be closed and new ones started with the DRS
C data at the beginning of them.
C
      CALL DRS_DECODE_BLK(CFLTNO,CDATE,CTIME,LVALDRS) !Get info from DRS block
      CALL GPS1_CODE_MESS0(CFLTNO,CDATE,CTIME,IMESS) !Code into message 0
      LHDROKS=LVALDRS                  !False till good mess 0 written to sys
      LHDROKO=LVALDRS                  !False till good mess 0 written to opt
      CLSTFLTNO=CFLTNO
      IF(ISTATS.EQ.0) THEN             !If file open on system disk
        CALL GPS1_MESSF_WRITE(1,IMESS,ISTATS) !Write message 0
        IF(ISTATS.NE.0) THEN
          CALL GPS1_MESSF_CLOSE(1,ISTAT) !Close file if write fail
          CALL LOG_MESS('HORACE:GPSDAT.DAT closed after write failure')
        END IF
      END IF
      IF(ISTATO.EQ.0) THEN             !If file open on optical disk
        CALL GPS1_MESSF_WRITE(2,IMESS,ISTATO) !Write message 0
        IF(ISTATO.NE.0) THEN
          CALL GPS1_MESSF_CLOSE(2,ISTAT) !Close file if write fail
          CALL LOG_MESS('DUB0:GPSDAT.DAT closed after write failure')
        END IF
      END IF
C
C Initialize some variables
C
      ICYCLM=1                         !Cycles 1-3 to get M18,20,31 in turn
      ICYCLD=1                         !1-6 for M24,18,24,20,24,31 to H_DISPLAY
      STATUS(17)=1                     !Pointer for latest message to H_DISPLAY
      STATUS(23)=0                     !Last write to hard disk
      STATUS(24)=0                     !Last write to hard disk
      STATUS(25)=0                     !Last write to optical disk
      STATUS(26)=0                     !Last write to optical disk
      STATUS(29)=0                     !Last write to data from GPS
      STATUS(30)=0                     !Last write to data from GPS
      M24TO=0                          !Message 24 timeouts
      M18TO=0                          !Message 18 timeouts
      M20TO=0                          !Message 20 timeouts
      M31TO=0                          !Message 31 timeouts
      LSNDM15=.TRUE.                   !True if message 15 needs to be sent
      RERRSRT=-1.                      !Time when GPS system stopped working
      LMESSNT=.FALSE.                  !True if GPS warning sent to flight lead
      LSTART=.TRUE.                    !True first time try for message
      ILOOPCNT=0                       !No of passes through main loop
      CALL ST_MESS('Collecting GPS messages') !Set status logical
C
C Now loop continuously, executing the loop once a second, as timed by the
C HORACE clock, unless the loop overuns, in which case the next loop is 
C executed at the start of the next second after the overun.  Note that if the
C GPS box does not respond the timeouts involved mean that the loop is only
C executed about once every 4 seconds
C
      DO WHILE(.TRUE.)                 !Execute loop once a second
C
C First check the status of the disks, and close the GPSDAT.DAT files if some
C other process on the system has set the relevant logical name to
C 'Unavailable'.
C
        CALL GET_DISK_STATUS(CSYSSTS,COPTSTS) !Check disk availability
        IF(CSYSSTS.EQ.'Unavailable') THEN !If system disk unavailable
          IF(ISTATS.EQ.0) THEN
            CALL GPS1_MESSF_CLOSE(1,ISTAT) !Close file
            ISTATS=1                     !Show no open file on system disk
            CALL LOG_MESS
     -          ('HORACE:GPSDAT.DAT closed because disk unavailable')
          END IF
        END IF
        IF(COPTSTS.EQ.'Unavailable') THEN !If optical disk unavailable
          IF(ISTATO.EQ.0) THEN
            CALL GPS1_MESSF_CLOSE(2,ISTAT) !Close file
            ISTATO=1                     !Show no open file on optical disk
            CALL LOG_MESS
     -          ('DUB0:GPSDAT.DAT closed because disk unavailable')
          END IF
        END IF
C
C Now check if H_DISPLAY has provided an initialization message which needs
C to be sent to the GPS receiver.  H_DISPLAY indicates this by adding 10 to
C the value in STATUS(17).  STATUS(17) points to the part of the buffer 
C containing the message.  The initialization message is also copied to the
C message files.  H_DISPLAY MUST store the message and change STATUS(17) right
C after this program changes STATUS(17) as part of its normal operating cycle
C and when it is waiting for the next second.  This will normally be alright
C provided H_GPS_LOG is running at a higher priority than H_DISPLAY.
C       
        IF(STATUS(17).GE.11.AND.STATUS(17).LE.12) THEN !If need to initialize
          STATUS(17)=STATUS(17)-10     !Strip init indicator
          CALL GPS1_MESS_SEND(IGPSMESS(1,STATUS(17)),ISTAT) !Send init message
          CALL LOG_MESS('GPS initialization message sent')
          IF(ISTATS.EQ.0) CALL GPS1_MESSF_WRITE(1,IGPSMESS(1,STATUS(17)) 
     -        ,ISTATS)                 !If file is open on system disk
          IF(ISTATS.NE.0) THEN
            CALL GPS1_MESSF_CLOSE(1,ISTAT) !Close on error
            CALL LOG_MESS('HORACE:GPSDAT.DAT closed after write error')
          END IF
          IF(ISTATO.EQ.0) CALL GPS1_MESSF_WRITE(2,IGPSMESS(1,STATUS(17))
     -        ,ISTATO)                 !If file is open on optical disk
          IF(ISTATO.NE.0) THEN
            CALL GPS1_MESSF_CLOSE(2,ISTAT) !Close on error
            CALL LOG_MESS('DUB0:GPSDAT.DAT closed after write error')
          END IF
          LSNDM15=.TRUE.               !Always send message 15 after init
        END IF
C
C Every 60s check whether either of the disk files are not currently open but
C the disks are have their logicals set to show they are available.  In this
C case try to start a new GPSDAT.DAT file, read the DRS data, and write a
C message 0 to the start of the file, even if it is only spaces.
C
        ILOOP=ILOOP+1                  !Bump loop count
        IF(MOD(ILOOP,60).EQ.0) THEN    !Every 60s
          IF(ISTATS.NE.0.AND.CSYSSTS.NE.'Unavailable') THEN !If can open sys file
            CALL GPS1_MESSF_CREATE('HORACE:GPSDAT.DAT',1,ISTATS) !Open it
            IF(ISTATS.EQ.0) THEN       !If successful
              CALL LOG_MESS('HORACE:GPSDAT.DAT opened')
              CALL DRS_DECODE_BLK(CFLTNO,CDATE,CTIME,LVALDRS) !Get DRS info
              CALL GPS1_CODE_MESS0(CFLTNO,CDATE,CTIME,IMESS) !Form message 0
              LHDROKS=LVALDRS          !Will be false if no DRS info in message
              CALL GPS1_MESSF_WRITE(1,IMESS,ISTATS) !Write message to file
              IF(ISTATS.NE.0) THEN
                CALL GPS1_MESSF_CLOSE(1,ISTAT) !Close file on erro
                CALL LOG_MESS
     -              ('HORACE:GPSDAT.DAT closed after write error')
              END IF
            END IF
          END IF
          IF(ISTATO.NE.0.AND.COPTSTS.NE.'Unavailable') THEN !If can open opt file
            CALL GPS1_MESSF_CREATE('DUB0:[HORACE]GPSDAT.DAT',2,ISTATO) !Open it
            IF(ISTATO.EQ.0) THEN       !If successful
              CALL LOG_MESS('DUB0:GPSDAT.DAT opened')
              CALL DRS_DECODE_BLK(CFLTNO,CDATE,CTIME,LVALDRS) !Get DRS info
              CALL GPS1_CODE_MESS0(CFLTNO,CDATE,CTIME,IMESS) !Form message 0
              LHDROKO=LVALDRS          !Will be false if no DRS info in message
              CALL GPS1_MESSF_WRITE(2,IMESS,ISTATO) !Write message to file
              IF(ISTATO.NE.0) THEN
                CALL GPS1_MESSF_CLOSE(2,ISTAT) !Close file on error
                CALL LOG_MESS
     -              ('DUB0:GPSDAT.DAT closed after write error')
              END IF
            END IF
          END IF
        END IF
C
C Every 5 mins, provided the DRS is running, write a message 0 with the DRS
C flight number, date and time to the GPSDAT.DAT files.  Also opens new files
C if the Flight Number changes.
C
        IF(MOD(ILOOP,300).EQ.0) THEN   !Every five minutes
          LSNDM15=.TRUE.               !Force message 15 to be sent
          CALL DRS_DECODE_BLK(CFLTNO,CDATE,CTIME,LVALDRS) !Get DRS info
          IF(LVALDRS) THEN             !If DRS info is valid
            IF(CFLTNO.NE.CLSTFLTNO) THEN
              LHDROKS=.FALSE.
              LHDROKO=.FALSE.
              CLSTFLTNO=CFLTNO
            END IF
            CALL GPS1_CODE_MESS0(CFLTNO,CDATE,CTIME,IMESS) !Code up message 0
            IF(.NOT.LHDROKS.AND.ISTATS.EQ.0) THEN !If sys file had no DRS heade
              CALL GPS1_MESSF_CLOSE(1,ISTAT) !Close it
              CALL GPS1_MESSF_CREATE('HORACE:GPSDAT.DAT',1,ISTATS) !Open new one
            END IF
            IF(ISTATS.EQ.0) THEN
              CALL GPS1_MESSF_WRITE(1,IMESS,ISTATS) !Send to sys
              IF(ISTATS.NE.0) THEN
                CALL GPS1_MESSF_CLOSE(1,ISTAT) !Close on error
                CALL LOG_MESS
     -              ('HORACE:GPSDAT.DAT closed after write error')
              ELSE IF(.NOT.LHDROKS) THEN
                CALL LOG_MESS
     -          ('New HORACE:GPSDAT.DAT opened with valid DRS header')
                LHDROKS=.TRUE.
              END IF
            END IF
            IF(.NOT.LHDROKO.AND.ISTATO.EQ.0) THEN !If opt file had no DRS heade
              CALL GPS1_MESSF_CLOSE(2,ISTAT) !Close it
              CALL GPS1_MESSF_CREATE('DUB0:[HORACE]GPSDAT.DAT',2,ISTATO) !Open ne
            END IF
            IF(ISTATO.EQ.0) THEN 
              CALL GPS1_MESSF_WRITE(2,IMESS,ISTATO) !Send to opt
              IF(ISTATO.NE.0) THEN
                CALL GPS1_MESSF_CLOSE(2,ISTAT) !Close on error
                CALL LOG_MESS
     -              ('DUB0:GPSDAT.DAT closed after write error')
              ELSE IF(.NOT.LHDROKO) THEN
                CALL LOG_MESS
     -              ('New DUB0:GPSDAT.DAT opened with valid DRS header')
                LHDROKO=.TRUE.
              END IF
            END IF
          END IF
        END IF
C
C Every 10 mins write the number of timeouts for each type of message to the
C log file.
C
        IF(MOD(ILOOP,600).EQ.0) THEN   !Every 10 minutes
          WRITE(CLINE,11,IOSTAT=IOS) M24TO,M18TO,M20TO,M31TO !Format timeouts
          CALL LOG_MESS(CLINE(1:46))   !Write timeout counts to log file
        END IF
C
C Now start getting and processing data from the GPS receiver.  This is done
C on a cyclic basis so that every second, message 24 and one of messages 18,
C 20, and 31 are solicited from the receiver.  Messages 18, 20 and 31 are
C only solicited if there is sufficient time to get them without causing the
C loop to overun its basic 1s timing cycle. In this way maximum priority is
C given to always getting the basic GPS navigation message, message 24.
C
        LNDUOK=.FALSE.
        CALL GPS1_MESS_GET(24,ISTAT,IMESS)
        IF(ISTAT.EQ.1) CALL GPS1_ANALYS_MESS(IMESS,IMESSNUM) !Analyse mess type
        IF(ISTAT.EQ.1.AND.IMESSNUM.EQ.24) THEN !If got good message
          CALL STR_TIME(STATUS(29))    !Note time got latest M24 data from GPS
          CALL GPS1_DECODE_MESS24(IMESS,CGTIME,IRECID,IALTHLD,IDATUM,
     -        ISV,ITRKSTS,IFOM,RVEPE,RHEPE,RRLAT,RRLNG,RRALTMSL,
     -        RRALTREF,RRV)
          LGPSOK=.TRUE.                !Will be false if GPS has not got going
          IF(ISV(1).EQ.ISV(2).AND.ISV(1).EQ.ISV(3).AND. !Check if sv's same
     -       ISV(1).EQ.ISV(4)) LGPSOK=.FALSE.
          IF(ISV(1).EQ.1.AND.ISV(2).EQ.2.AND.ISV(3).EQ.3.AND. !Check for 1234
     -       ISV(4).EQ.4) LGPSOK=.FALSE.
          IF(STATUS(33).EQ.1) THEN     !If have good DRS data check times
            J1=NFDATA(10,STATUS(1))    !Decode DRS time into secs past midnight
            J2=NFDATA(11,STATUS(1))
            JJHRS=IBITS(J1,8,4)*10+IBITS(J1,4,4)
            JJMIN=IBITS(J1,0,4)*10+IBITS(J2,8,4)
            JJSEC=IBITS(J2,4,4)*10+IBITS(J2,0,4)
            IDTIM=JJHRS*3600+JJMIN*60+JJSEC !DRS seconds past midnight
            IGTIM=GPS1_ILONGWR(IMESS(4))    !Get GPS seconds
            IGTIM=MOD(IGTIM,24*60*60)  !GPS time in spm
            IDIFF=IDTIM-IGTIM          !Difference in times
            IF(ABS(IDIFF).GT.75.AND.   !Allow for UTC corr plus some more
     -          ABS(IDIFF).LT.24*3600-75) LGPSOK=.FALSE. !Allow for crossmdnt
          END IF
          IF(LGPSOK.AND.(IFOM.LE.1.OR.IFOM.GE.8)) LNDUOK=.TRUE. !Data good for H_NDU
c          IF(LGPSOK) THEN              !Only write message if basically OK
          if(.true.) then              !Write all messages for TOGA
            IF(ISTATS.EQ.0) THEN
              CALL GPS1_MESSF_WRITE(1,IMESS,ISTATS) !Write to sys
              IF(ISTATS.NE.0) THEN
                CALL GPS1_MESSF_CLOSE(1,ISTAT) !Close on error
                CALL LOG_MESS
     -              ('HORACE:GPSDAT.DAT closed after write error')
              ELSE
                CALL STR_TIME(STATUS(23)) !Latest write to sys disk
              END IF
            END IF
            IF(ISTATO.EQ.0) THEN
              CALL GPS1_MESSF_WRITE(2,IMESS,ISTATO) !Write to opt
              IF(ISTATO.NE.0) THEN
                CALL GPS1_MESSF_CLOSE(2,ISTAT) !Close on error
                CALL LOG_MESS
     -              ('DUB0:GPSDAT.DAT closed after write error')
              ELSE
                CALL STR_TIME(STATUS(25)) !Latest write to optical
              END IF
            END IF
          END IF
          IF(ICYCLD.EQ.1.OR.ICYCLD.EQ.3.OR.ICYCLD.EQ.5) THEN !If time for displ
            DO I=1,27                  !Copy message to display buffer
              IGPSMESS(I,3-STATUS(17))=IMESS(I)
            END DO
          END IF
          LM24OK=.TRUE.                !Show had valid message 24
          STATUS(35)=1                 !Show have recent message
        ELSE                           !If failed to get message 24
          M24TO=M24TO+1                !Bump timeout counter
          LM24OK=.FALSE.               !Show failed to get valid message 24
          STATUS(35)=0                 !Show no recent message
        END IF
C
C If everything working well then pass data to H_NDU process, else send -999s
C
        IF(LNDUOK) THEN
          RGGLAT=RRLAT
          RGGLNG=RRLNG
          RGGREF=RRALTREF
          IT1=GPS1_ILONGWR(IMESS(4))    !Integer seconds of week
          IT2=GPS1_ILONGWR(IMESS(6))    !Each bit is 2**-31 seconds
          RGSECS=FLOAT(IT1)+FLOAT(IT2)/2**16/2**15
        ELSE
          RGGLAT=-999.0
          RGGLNG=-999.0
          RGGREF=-999.0
          RGSECS=-999.0
        END IF
C
C Send error message if system not working.  Also adjust the data passed to
C the display program so that it shows as 0's.
C
        IF(.NOT.LGPSOK.OR..NOT.LM24OK) THEN !If system completely not working
          IF(RERRSRT.EQ.-1) RERRSRT=SECNDS(0.0) !Note time problem starts
          RERRTIM=SECNDS(0.0)-RERRSRT !Time this problem has lasted
          IF(RERRTIM.LT.0.0) RERRTIM=RERRTIM+24*3600 !Midnight crossover
          IF(RERRTIM.GT.5*60.AND..NOT.LMESSNT) THEN !After 5 mins
            CALL HM_MESS('GPS has not worked for last 5 mins.  '//
     -          'Initialization may be required') !Send message to FL
            CALL LOG_MESS('No valid GPS data for 5 minutes')
            CALL ST_MESS('Not receiving GPS data')
            LMESSNT=.TRUE.           !Show message sent
          END IF
          IF(LSTART) THEN            !If first try
            CALL LOG_MESS('No GPS message received')
            CALL ST_MESS('Not receiving GPS data')
            LMESSNT=.TRUE.           !Show message sent
            LSTART=.FALSE.
          END IF
          RRLAT=0.0                  !No useful values available
          RRLNG=0.0
          RRALTMSL=0.0               !Show altitude held/unavailable
          IFOM=3                     !Show no data
        ELSE                         !Data looks good
          RERRSRT=-1.                !Show now longer have problem
          IF(LMESSNT.OR.LSTART) THEN
            CALL LOG_MESS('Valid GPS message received')
            CALL ST_MESS('Collecting GPS messages') !Set status logical
            LSTART=.FALSE.
            LMESSNT=.FALSE.          !Reset for next time
          END IF
          IF(IALTHLD.EQ.1) RRALTMSL=0.0 !Show altitude held/unavailable
          IFOM=IFOM.AND.'1'X         !Use nav degraded bit
        END IF
C
C Pass the data to the display program.  Note that two locations ahead are also
C filled (6s) so that if looping very slowly because of timeouts, old data is
C still overwritten.
C
        IDSTS1=STATUS(2)             !Get current derivation pointer
        IF(IDSTS1.GT.0.AND.IDSTS1.LE.12800) THEN !If valid pointer
          IDSTS2=IDSTS1+1            !Note next location also
          IF(IDSTS2.GT.12800) IDSTS2=1 !Wrap around if necessary
          IDSTS3=IDSTS2+1            !Note next location also
          IF(IDSTS3.GT.12800) IDSTS3=1 !Wrap around if necessary
          RGLAT(IDSTS1)=RRLAT        !Transfer lat
          RGLNG(IDSTS1)=RRLNG        !Transfer lng
          RGALT(IDSTS1)=RRALTMSL     !Transfer alt
          RGLAT(IDSTS2)=RRLAT        !Also put lat in next location
          RGLNG(IDSTS2)=RRLNG        !Also put lng in next location
          RGALT(IDSTS2)=RRALTMSL     !Also put alt in next location
          RGLAT(IDSTS3)=RRLAT        !Also put lat in next location
          RGLNG(IDSTS3)=RRLNG        !Also put lng in next location
          RGALT(IDSTS3)=RRALTMSL     !Also put alt in next location
          SPECIAL(IDSTS1,8)=IFOM     !Store figure of merit
          SPECIAL(IDSTS2,8)=IFOM     !Store figure of merit
          SPECIAL(IDSTS3,8)=IFOM     !Store figure of merit
        END IF
C
C Get message 18 every 3s and write it to disk files.  Also transfer it to
C common if it is the right part of the cycle (every 6s).
C
        IF(MOD(SECNDS(0.0),1.0).LT.0.6.AND.ICYCLM.EQ.1) THEN
!          CALL GPS1_MESS_GET(18,ISTAT,IMESS) !Try for message 18
!          IF(ISTAT) THEN               !If got it
!            IF(LGPSOK.AND.LM24OK) THEN !If system is basically working
!              IF(ISTATS.EQ.0) THEN
!                CALL GPS1_MESSF_WRITE(1,IMESS,ISTATS) !Write to sys
!                IF(ISTATS.NE.0) THEN
!                  CALL GPS1_MESSF_CLOSE(1,ISTAT) !Close on error
!                  CALL LOG_MESS
!     -                ('HORACE:GPSDAT.DAT closed after write error')
!                END IF
!              END IF
!              IF(ISTATO.EQ.0) THEN
!                CALL GPS1_MESSF_WRITE(2,IMESS,ISTATO) !Write to opt
!                IF(ISTATO.NE.0) THEN
!                  CALL GPS1_MESSF_CLOSE(2,ISTAT) !Close on error
!                  CALL LOG_MESS
!     -                ('DUB0:GPSDAT.DAT closed after write error')
!                END IF
!              END IF
!            END IF
!            IF(ICYCLD.EQ.4) THEN       !If time to display
!              DO I=1,46                !Transfer message to display buffer
!                IGPSMESS(I,3-STATUS(17))=IMESS(I)
!              END DO
!            END IF
!          ELSE                         !If failed to get message 18
!            M18TO=M18TO+1              !Bump timeout count
!          END IF
        END IF
C
C Get message 20 every 3s and write it to disk files.  Also transfer it to
C common if it is the right part of the cycle (every 6s).
C
        IF(MOD(SECNDS(0.0),1.0).LT.0.6.AND.ICYCLM.EQ.2) THEN
          CALL GPS1_MESS_GET(20,ISTAT,IMESS) !Try for message 20
          IF(ISTAT) THEN               !If got it
            IUTC=GPS1_ILONGWR(IMESS(9))
            JUTCOF=NINT(FLOAT(IUTC)/2**26)
            JWEEKS=IMESS(25)
            IF(LGPSOK.AND.LM24OK) THEN !If system is basically OK
              IF(ISTATS.EQ.0) THEN
                CALL GPS1_MESSF_WRITE(1,IMESS,ISTATS) !Write to sys
                IF(ISTATS.NE.0) THEN
                  CALL GPS1_MESSF_CLOSE(1,ISTAT) !Close on error
                  CALL LOG_MESS
     -                ('HORACE:GPSDAT.DAT closed after write error')
                END IF
              END IF
              IF(ISTATO.EQ.0) THEN
                CALL GPS1_MESSF_WRITE(2,IMESS,ISTATO) !Write to opt
                IF(ISTATO.NE.0) THEN
                  CALL GPS1_MESSF_CLOSE(2,ISTAT) !Close on error
                  CALL LOG_MESS
     -                ('DUB0:DRSDAT.DAT closed after write error')
                END IF
              END IF
            END IF
            IF(ICYCLD.EQ.2) THEN       !If time to display
              DO I=1,36                !Transfer message to display buffer
                IGPSMESS(I,3-STATUS(17))=IMESS(I)
              END DO
            END IF
          ELSE                         !If failed to get message 20
            M20TO=M20TO+1              !Bump timeout count
          END IF
        END IF
C
C Get message 31 every 3s and write it to disk files.  Also transfer it to
C common if it is the right part of the cycle (every 6s).  Before the GPS
C receiver will send a valid message 31 it needs to have been sent a message 15
C to define the content and structure of message 31.  This doesn't need to be
C sent all the time, but we send it here if the system has been initialized, if
C it has not been sent since this program started, and every 5 minutes in case
C the receiver has been switched off and on for any reason.
C
        IF(MOD(SECNDS(0.0),1.0).LT.0.6.AND.ICYCLM.EQ.3) THEN
          IF(LSNDM15) THEN             !If need to define message 31 format
c            CALL GPS1_CODE_MESS15(IMESS) !Code definition into message 15
c            CALL GPS1_MESS_SEND(IMESS,ISTAT) !Send message 15
            LSNDM15=.FALSE.            !Show done
          END IF
c          CALL GPS1_MESS_GET(31,ISTAT,IMESS) !Try for message 31
          istat=.false. !temporary exclude m31 interrogation
          IF(ISTAT) THEN               !If got it
            IF(LGPSOK.AND.LM24OK) THEN !If system is basically OK
              IF(ISTATS.EQ.0) THEN
                CALL GPS1_MESSF_WRITE(1,IMESS,ISTATS) !Write to sys
                IF(ISTATS.NE.0) THEN
                  CALL GPS1_MESSF_CLOSE(1,ISTAT) !Close on error
                  CALL LOG_MESS
     -                ('HORACE:GPSDAT.DAT closed after write error')
                END IF
              END IF
              IF(ISTATO.EQ.0) THEN
                CALL GPS1_MESSF_WRITE(2,IMESS,ISTATO) !Write to opt
                IF(ISTATO.NE.0) THEN
                  CALL GPS1_MESSF_CLOSE(2,ISTAT) !Close on error
                  CALL LOG_MESS
     -                ('DUB0:GPSDAT.DAT closed after write error')
                END IF
              END IF
            END IF
            IF(ICYCLD.EQ.6) THEN       !If time to display
c              DO I=1,50??                !Transfer message to display buffer
c                IGPSMESS(I,3-STATUS(17))=IMESS(I)
c              END DO
            END IF
          ELSE                         !If failed to get message 31
            M31TO=M31TO+1              !Bump timeout count
          END IF
        END IF
C
C Adjust pointers and counters, and then wait for the next second
C
        ICYCLM=ICYCLM+1                !Bump message cycling count
        IF(ICYCLM.GT.3) ICYCLM=1       !Wrap at 3
        ICYCLD=ICYCLD+1                !Bump display cycling count
        IF(ICYCLD.GT.6) ICYCLD=1       !Wrap at 6
        STATUS(17)=3-STATUS(17)        !Swap GPS message buffer pointers
        CALL LIB$WAIT(1.0-MOD(SECNDS(0.0),1.0)) !Wait for next second
      END DO
11    FORMAT('M24,M18,M20,M31 timeouts: ',4I5)
      END
********************************************************************************
      SUBROUTINE LOG_MESS(MESSAGE)
C
C Writes message to log file consisting of program name, date, time and message.
C
C V1.00  15/05/91  W.D.N.JACKSON
C
      IMPLICIT NONE
      CHARACTER*(*) MESSAGE
      CHARACTER CDAT*9,CTIM*10
      CALL DATE1(CDAT)
      CALL TIME(CTIM)
      TYPE *,'H_GPS_LOG '//CDAT//' '//CTIM//MESSAGE
      RETURN
      END
********************************************************************************
      SUBROUTINE    ST_MESS(MESSAGE)
C
C Write message to the logical H_DRS_LOG_STAT in the group logical name
C table.  GRPNAM and SYSNAM privileges are required.
C
C V1.00  15/05/91  W.D.N.JACKSON
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
      ISTAT=SYS$CRELNM(,'LNM$GROUP','H_GPS_LOG_STAT',PSL$C_SUPER,TLIST)
      IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      RETURN
      END
********************************************************************************
      SUBROUTINE DRS_DECODE_BLK(CFLTNO,CDATE,CTIME,LVALID)
C
C Decodes a DRS block for flight number, date and time.  If if the block is
C invalid returns spaces and LVALID FALSE, else returns with LVALID TRUE.
C
C V1.00  15/05/91  W.D.N.JACKSON
C V1.01  10/08/92  W.D.N.JACKSON
C        Now returns with LVALID false if no valid DRS data
C
      IMPLICIT NONE
      INTEGER*4 IS,J1,JFLT,J2,JJDAY,JJMON,JJYER,JJHRS,JJMIN,JJSEC,IOS
      LOGICAL*4 LVALID
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
C V1.00            W.D.N.JACKSON
C V1.01  10/08/92  W.D.N.JACKSON
C        Now also read SYST_STATUS
C
      IMPLICIT NONE
      INTEGER*4 ISTAT,NAMLEN
      CHARACTER*(*) CSYSSTS,COPTSTS
      CHARACTER*20 LOGNAME
      INTEGER*4 ILIST(4)
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
C*******************************************************************************
      SUBROUTINE STR_TIME(JSTORE)
C
C Stores the current time as a 32 bit integer seconds past midnight.
C
C V1.00            W.D.N.JACKSON
C
      IMPLICIT NONE
      INTEGER*4 ITEMP
      INTEGER*2 JSTORE(2),JTEMP(2)
      EQUIVALENCE (ITEMP,JTEMP)
      ITEMP=NINT(SECNDS(0.0))
      JSTORE(1)=JTEMP(1)
      JSTORE(2)=JTEMP(2)
      RETURN
      END
C**************************************************************************
      SUBROUTINE HM_MESS(CMESS)
C
C Writes an immediate message to the Flight Leader's display, and also stores
C it in the shared file HORACE:H_MESSAGES.LOG.  These messages should only be
C sent when there is a problem or potential problem which must be drawn to
C the Flight Leader's attention.  OPER privilege is required.  Note that this
C routine prefaces the message with the time that it was received by the
C H_MESSAGE process, so there is no need for users to time stamp their
C messages.
C
C V1.00            W.D.N.JACKSON
C
      IMPLICIT      NONE
      CHARACTER*(*) CMESS
      CHARACTER     CMESSA*72,CERRMESS*132
      INTEGER*2     IOSB(4),ICHAN
      INTEGER*4     IFIRST/1/,ISTAT,IFUNCT,MLEN
      INCLUDE       '($SYSSRVNAM)'
      INCLUDE       '($IODEF)'

      CMESSA=CMESS
      IF(IFIRST.EQ.1) THEN
        ISTAT=SYS$ASSIGN('H_MESSAGE_MBX',ICHAN,,)
        IF(.NOT.ISTAT) THEN            !Don't abort program on error
          CALL SYS$GETMSG(%VAL(ISTAT),MLEN,CERRMESS,%VAL(1),) !Just print message
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
