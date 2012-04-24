      PROGRAM  H_NEPH
!
! PURPOSE : Controls and monitors the TSI Model 3563 Integrating Nephelometer
!
! DESCRIPTION : The nephelometer is controlled via a RS232 connection to a
!               terminal server port given the logical name NEPH_CHAN.
!               When a valid response is gained from the instrument a power up
!               command series is issued. The file H_NEPH.DAT is then read,
!               interpreted and the commands defined in it issued. H_NEPH then
!               sits in a monitor state reading status information from the
!               instrument every minute. The global array STATUS is monitored
!               every second for commands which are interpreted and issued.
!
! INPUT : NEPH_CHAN    Logical pointing to RS232 connection to nephelometer
!         H_NEPH.DAT   File containing set up control commands
!         STATUS(48)   Set to true if any display is in the nephelometer menu
!         STATUS(49)   Zero mode ; 0=manual, 1=auto, 2->24=air chop,
!                      100=zero val, <0 no cmd
!         STATUS(50)   auto zero period (10-9999), <0 no cmd
!         STATUS(51)   backscatter mode; 0 normal, 1 backscatter, <0 no cmd
!         STATUS(52)   heater; 0 off, 1 on, <0 no cmd
!         STATUS(53)   power; 0 off, 1 on, <0 no cmd
!                      Command STATUS variables (49 thro' 53) are +1 and set
!                      negative to show last command given.
!
! OUTPUT : NEPH_CHAN    as above
!          H_NEPH_STAT  logical set to status of instrument
!          Log file
!
! REFERENCES : Nephelometer instruction manual
!              Instrument appendix ..
!              Nephelometer internal technical note
!
! AUTHOR :   V1.00 D P BRIGGS
!
! CHANGES :  V1.01 D P Briggs
!            Updated and improved status information
!
      IMPLICIT NONE
      INCLUDE 'HCOM_DEF.FOR'
      LOGICAL*4  ISTATOK       !set .FALSE. if connection failure
      INTEGER*4  INEPHCHAN,    !assigned channel no.
     &           ITIME,        !time in seconds since last status report
     &           IC            !counter
      CHARACTER*90 CCOMM
      COMMON /NEPHCOM/ INEPHCHAN
!
      STATUS(48) = .FALSE.
      !open channel to nephelometer
      CALL H_NEPH_OPEN_CHAN(ISTATOK)
      !wait for valid response
      CALL H_NEPH_RESP(ISTATOK)
      !submit power up commands
      CALL H_NEPH_PU(ISTATOK)
      STATUS(53) = -2     !instrument powered up
      !read command file and submit to nephelometer
      CALL H_NEPH_RCF(ISTATOK)
      !interrogate nephelometer and submit to log file
      CALL H_NEPH_FSTAT(ISTATOK,STATUS)
      CALL H_NEPH_STAT(ISTATOK,STATUS)

      ITIME = 0           !initialise timer
      DO WHILE (.TRUE.)
        !wait 1 second
        CALL LIB$WAIT(1.0)
        ITIME = ITIME + 1
        !check connection not lost
        IF (ISTATOK) THEN
          !check for commands
          IF (STATUS(49) .GE. 100) THEN  !perform zero cal
            CCOMM = 'Z'
            CALL H_NEPH_COM(ISTATOK,CCOMM,1,'OK')
            STATUS(49) = -1 - MOD(STATUS(49),100)
          ELSE IF (STATUS(49) .GE. 0) THEN  !set zero mode
            IC = 1
            IF (STATUS(49) .GE. 10) IC = 2 
            WRITE(CCOMM,'(''SMZ'',I<IC>)') STATUS(49)
            CALL H_NEPH_COM(ISTATOK,CCOMM,3+IC,'OK')
            STATUS(49) = -1 - STATUS(49)  
          ENDIF
          IF (STATUS(50) .GE. 10) THEN  !calibration period
            IC = 1 + INT(ALOG10(FLOAT(STATUS(50))))
            WRITE(CCOMM,'(''STP'',I<IC>)') STATUS(50)
            CALL H_NEPH_COM(ISTATOK,CCOMM,3+IC,'OK')
            STATUS(50) = -1 - STATUS(50)  
          ENDIF
          IF (STATUS(51) .GE. 0) THEN  !backscatter mode
            WRITE(CCOMM,'(''SMB'',I1)') STATUS(51)
            CALL H_NEPH_COM(ISTATOK,CCOMM,4,'OK')
            STATUS(51) = -1 - STATUS(51)  
          ENDIF
          IF (STATUS(52) .GE. 0) THEN  !heater
            WRITE(CCOMM,'(''H'',I1)') STATUS(52)
            CALL H_NEPH_COM(ISTATOK,CCOMM,2,'OK')
            STATUS(52) = -1 - STATUS(52)  
          ENDIF
          IF (STATUS(53) .GE. 0) THEN  !power
            IF (STATUS(53) .EQ. 0) THEN
              CCOMM = 'L0'              !switch lamp off  
              CALL H_NEPH_COM(ISTATOK,CCOMM,2,'OK')
              CCOMM = 'PD'              !power down 
              CALL H_NEPH_COM(ISTATOK,CCOMM,2,'OK')
            ELSE
              CALL H_NEPH_PU(ISTATOK)
              CALL H_NEPH_STAT(ISTATOK,STATUS)
            ENDIF 
            STATUS(53) = -1 - STATUS(53)  
          ENDIF
          !if 5 minutes is up
          IF (ITIME .GT. 900) THEN
            !get and report nephelometer status
            CALL H_NEPH_STAT(ISTATOK,STATUS)
            ITIME = 0       !initialise timer
          ENDIF
        ELSE  
          !report lost connection
          CALL LOG_MESS('Lost connection to Nephelometer')
          CALL ST_MESS('Trying to connect to Nephelometer') 
          !open channel to nephelometer
          CALL H_NEPH_OPEN_CHAN(ISTATOK)
          !wait for valid response
          CALL H_NEPH_RESP(ISTATOK)
          !submit power up commands
          CALL H_NEPH_PU(ISTATOK)
          STATUS(53) = -2
        ENDIF 
      END DO
      END
********************************************************************************
      SUBROUTINE LOG_MESS(MESSAGE)
!
! Writes message to log file consisting of program name, date, time and message.
!
      IMPLICIT NONE
      CHARACTER*(*) MESSAGE
      CHARACTER CDAT*9,CTIM*10
      CALL DATE1(CDAT)
      CALL TIME(CTIM)
      TYPE *,'H_NEPH '//CDAT//' '//CTIM//MESSAGE
      RETURN
      END
********************************************************************************
      SUBROUTINE    ST_MESS(MESSAGE)
!
! Write message to the logical H_NEPH_STAT in the group logical name
! table.  GRPNAM and SYSNAM privileges are required.
!
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
      ISTAT=SYS$CRELNM(,'LNM$GROUP','H_NEPH_STAT',PSL$C_SUPER,TLIST)
      IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      RETURN
      END
********************************************************************************
      SUBROUTINE  H_NEPH_OPEN_CHAN(ISTATOK)
!
! Attempts to open an I/O unit, defined by NEPH_CHAN, to nephelometer.
!
      IMPLICIT NONE
      LOGICAL*4 ISTATOK        !set .FALSE. if connection failure
      INTEGER*4 INEPHCHAN     !assigned channel no.
      INTEGER*4 ISTAT,IM,ILEN,LIB$GETDVI,IFUNCT
      INTEGER*2  IOSB(4)
      CHARACTER*64 CNAME
      COMMON /NEPHCOM/ INEPHCHAN
      INCLUDE   '($DVIDEF)'
      INCLUDE   '($IODEF)'
      INCLUDE   '($SSDEF)'
      INCLUDE   '($SYSSRVNAM)'
!
      ISTATOK = .TRUE.
      IM = 0
      ISTAT = SYS$ALLOC('NEPH_CHAN',,,,)
      IF (ISTAT) THEN
        ISTAT = SYS$ASSIGN('NEPH_CHAN',INEPHCHAN,,)
        ISTAT = LIB$GETDVI(DVI$_DEVNAM,INEPHCHAN,,,CNAME,ILEN) !Get device name
        IF(CNAME(2:4).EQ.'LTA') THEN   !If terminal server
          IFUNCT=IO$_TTY_PORT.OR.IO$M_LT_CONNECT !Connect function
          ISTAT=SYS$QIOW(,%VAL(INEPHCHAN),%VAL(IFUNCT),IOSB,,,,,,,,) !Make cnctn
        END IF
      ENDIF
      DO WHILE(.NOT. ISTAT)
        ISTAT=SYS$DALLOC('NEPH_CHAN',)
        ISTAT=SYS$DASSGN(%VAL(INEPHCHAN))
        IF(IM.EQ.0) CALL LOG_MESS('Unable to open NEPH_CHAN')
        IF(IM.EQ.0) CALL ST_MESS('Trying to connect to Nephelometer')
        IM=IM+1
        CALL LIB$WAIT(10.0)
        ISTAT = SYS$ALLOC('NEPH_CHAN',,,,)
        IF (ISTAT) THEN
          ISTAT = SYS$ASSIGN('NEPH_CHAN',INEPHCHAN,,)
          ISTAT=LIB$GETDVI(DVI$_DEVNAM,INEPHCHAN,,,CNAME,ILEN) !Get device name
          IF(CNAME(2:4).EQ.'LTA') THEN   !If terminal server
            IFUNCT=IO$_TTY_PORT.OR.IO$M_LT_CONNECT      !Connect function
            ISTAT=SYS$QIOW(,%VAL(INEPHCHAN),%VAL(IFUNCT),IOSB,,,,,,,,) 
          ENDIF
        ENDIF
      ENDDO
      CALL LOG_MESS('NEPH_CHAN is now open')
      CALL ST_MESS('Waiting for Nephelometer control/status')
      RETURN
      END
********************************************************************************
      SUBROUTINE  H_NEPH_RESP(ISTATOK)
!
! Submits Read Status Flags ('RF') commands to nephelometer, via INEPHCHAN,
! until a valid response is gained (status < 200 hex).
!
      IMPLICIT NONE
      LOGICAL*4  ISTATOK      !set .FALSE. if connection failure
      CHARACTER*90 CCOMM
!
      IF (ISTATOK) THEN
        CCOMM = 'RF'
        CALL H_NEPH_COM(ISTATOK,CCOMM,2,'  ')
        DO WHILE (CCOMM(1:2) .EQ. 'RF')
          CALL LIB$WAIT(5.0)
          CCOMM = 'RF'
          CALL H_NEPH_COM(ISTATOK,CCOMM,2,'  ')
        END DO
      ENDIF
      RETURN
      END
********************************************************************************
      SUBROUTINE  H_NEPH_PU(ISTATOK)
!
! submit the commands to power up the nephelometer
!
      IMPLICIT NONE
      LOGICAL*4 ISTATOK
      CHARACTER*90 CCOMM
      CHARACTER*8  CTIME
      INTEGER*4    IM,ID,IY 
!
      IF (ISTATOK) THEN
        CCOMM = 'PU'                   !power up
        CALL H_NEPH_COM(ISTATOK,CCOMM,2,'OK')
        CCOMM = 'F1'                   !lamp cooling fan on
        CALL H_NEPH_COM(ISTATOK,CCOMM,2,'OK')
        CCOMM = 'L1'                   !lamp on
        CALL H_NEPH_COM(ISTATOK,CCOMM,2,'OK')
        CCOMM = 'H1'                   !heater on
        CALL H_NEPH_COM(ISTATOK,CCOMM,2,'OK')
        CALL IDATE1 (IM,ID,IY)          !set date-time
        IY = IY + 1900
        IF (IY .LT. 1990) IY = IY + 100 !year 2000 + 
        CALL TIME (CTIME)
        WRITE(CCOMM,
     &'(''STT'',I4.4,'','',I2.2,'','',I2.2,'','',A2,'','',A2,'','',A2)')
     &IY,IM,ID,CTIME(1:2),CTIME(4:5),CTIME(7:8)
        CALL H_NEPH_COM(ISTATOK,CCOMM,21,'OK')
        CALL LIB$WAIT(15.0)   !wait 15 seconds to ensure power up
      ENDIF
      RETURN
      END
********************************************************************************
      SUBROUTINE  H_NEPH_RCF(ISTATOK)
!
! reads commands in control file H_NEPH.DAT and submits them to the 
! nephelometer. All text on a line after a '!' is ignored. All spaces
! are ignored.
!
      IMPLICIT NONE
      LOGICAL*4 ISTATOK
      CHARACTER*90 CCOMM
      CHARACTER*80 CFNAME,CLINE
      INTEGER*4      IIP,IOP,IOS
      DATA CFNAME / 'H_NEPH.DAT' /
! 
      IF (ISTATOK) THEN
        !open command file for read
        OPEN(UNIT=10,FILE=CFNAME,STATUS='OLD',IOSTAT=IOS)
        !read line
        READ (10,'(A)',IOSTAT=IOS) CLINE
        DO WHILE (IOS .EQ. 0)
          !remove white space and anything after a '!'
          IIP = 1
          IOP = 0
          DO WHILE (CLINE(IIP:IIP) .NE. '!' .AND. IIP .LT. 80)
            IF (CLINE(IIP:IIP) .NE. ' ') THEN
              IOP = IOP + 1
              CCOMM(IOP:IOP) = CLINE(IIP:IIP)
            ENDIF
            IIP = IIP + 1
          END DO
          IF (IOP .GT. 1) THEN  
            !submit command
            CALL H_NEPH_COM(ISTATOK,CCOMM,IOP,'OK')
          END IF
          !read line
          READ (10,'(A)',IOSTAT=IOS) CLINE
        END DO
        !close file
        CLOSE (10)
      ENDIF
      RETURN
      END
********************************************************************************
      SUBROUTINE  H_NEPH_FSTAT(ISTATOK,STATUS)
!
! Finds the value to the major nephelometer variables and reports them to the
! log file.
!
      IMPLICIT     NONE
      LOGICAL*4    ISTATOK
      CHARACTER*3  CMND(30)
      CHARACTER*25 CMESS(30)
      CHARACTER*90 CCOMM
      INTEGER*4 ICMDNO,IC
      INTEGER*2    STATUS(256)
      DATA (CMND(IC),IC=1,26)
     &          /'SAR','SAP','SAT','SAS',
     &           'SB0','SB1','SB2','SB3','SB4',
     &           'SB5','SB6','SB7','SB8',
     &           'SCI','SCP','SCP','SCS',
     &           'SKB','SKG','SKR',
     &           'STB','STP','STZ',
     &           'SVB','SVG','SVR'/
      DATA (CMESS(IC),IC=1,26)
     &      /'RANGE: Relative humidity ',
     &       'RANGE: Pressure          ',
     &       'RANGE: Temperature       ',
     &       'RANGE: Scattering        ',
     &       'Output channel 0         ',
     &       'Output channel 1         ',
     &       'Output channel 2         ',
     &       'Output channel 3         ',
     &       'Output channel 4         ',
     &       'Output channel 5         ',
     &       'Output channel 6         ',
     &       'Output channel 7         ',
     &       'Output channel 8         ',
     &       'CAL: Inlet temperature   ',
     &       'CAL: Pressure            ',
     &       'CAL: Relative humidity   ',
     &       'CAL: Sample temperature  ',
     &       'K CAL: Blue              ',
     &       'K CAL: Green             ',
     &       'K CAL: Red               ',
     &       'Blanking time            ',
     &       'Autozero period          ',
     &       'Zero duration            ',
     &       'PMT voltage blue         ',
     &       'PMT voltage green        ',
     &       'PMT voltage red          '/
      DATA ICMDNO / 26  /
!
      IF (ISTATOK) THEN
        CALL LOG_MESS('Nephelometer constant summary')
        DO IC = 1,ICMDNO
          CCOMM = CMND(IC)
          CALL H_NEPH_COM(ISTATOK,CCOMM,3,'  ')
          WRITE(6,'(1X,A)') CMESS(IC)//CCOMM(1:54)
        END DO
        STATUS(48) = .FALSE.
      END IF
      RETURN
      END
********************************************************************************
      SUBROUTINE  H_NEPH_STAT(ISTATOK,STATUS)
!
! Periodic nephelometer status information.
!
      IMPLICIT NONE
      LOGICAL*4 ISTATOK
      CHARACTER*90 CCOMM
!      CHARACTER*80 CLINE
      INTEGER*4    ISTAT
      INTEGER*2    STATUS(256)
!
      CALL LOG_MESS('Nephelometer status report')
      !read and interpret status flags
      CCOMM = 'RF'
      CALL H_NEPH_COM(ISTATOK,CCOMM,2,'  ')
      IF (ISTATOK) THEN
        READ(CCOMM,'(BN,Z4)') ISTAT 
        IF (BTEST(ISTAT,0)) THEN     !bit 0 lamp 10% outside of setting
          CALL LOG_MESS('Lamp ±10% outside of setting')
        END IF
        IF (BTEST(ISTAT,1)) THEN     !bit 1 valve fault
          CALL LOG_MESS('Valve fault')
        END IF
        IF (BTEST(ISTAT,2)) THEN     !bit 2 chopper fault
          CALL LOG_MESS('Chopper fault / parked')
        END IF
        IF (BTEST(ISTAT,3)) THEN     !bit 3 shutter fault
          CALL LOG_MESS('Shutter fault')
        END IF
        IF (BTEST(ISTAT,4)) THEN     !bit 4 heater not stabilized
          CALL LOG_MESS('Heater not stable')
        END IF
        IF (BTEST(ISTAT,5)) THEN     !bit 5 pressure out of range
          CALL LOG_MESS('Pressure out of range')
        END IF
        IF (BTEST(ISTAT,6)) THEN     !bit 6 samp temp out of range
          CALL LOG_MESS('Sample temperature out of range')
        END IF
        IF (BTEST(ISTAT,7)) THEN     !bit 7 Inlet temp out of range
          CALL LOG_MESS('Inlet temperature out of range')
        END IF
        IF (BTEST(ISTAT,8)) THEN     !bit 8 RH out of range
          CALL LOG_MESS('RH out of range')
        END IF
        !lamp voltage, current and power
!        CCOMM = 'RL'
!        CALL H_NEPH_COM(ISTATOK,CCOMM,2,'  ')
!        CLINE = 'Lamp voltage, current / power : '//CCOMM
!        CCOMM = 'SP'
!        CALL H_NEPH_COM(ISTATOK,CCOMM,2,'  ')
!        CLINE(44:49) = '/ '//CCOMM(1:3)
!        WRITE(6,*) CLINE
        !time
        CCOMM = 'RT'
        CALL H_NEPH_COM(ISTATOK,CCOMM,2,'  ')
        WRITE(6,*) 'Nephelometer time : '//CCOMM(1:22)
        !accumulated on time
        CCOMM = 'RO'
        CALL H_NEPH_COM(ISTATOK,CCOMM,2,'  ')
        WRITE(6,*) 'Nephelometer on time : '//CCOMM(1:8)
        !noise level
!        CCOMM = 'RN'
!        CALL H_NEPH_COM(ISTATOK,CCOMM,2,'  ')
!        WRITE(6,*) 'Nephelometer noise levels :-'
!        WRITE(6,*) CCOMM(1:70)
        !Auxillary status info
        CCOMM = 'RY'
        CALL H_NEPH_COM(ISTATOK,CCOMM,2,'  ')
        WRITE(6,*) 'Nephelometer status :-'
        WRITE(6,*) CCOMM(1:80)
        !data
        CCOMM = 'RD'
        CALL H_NEPH_COM(ISTATOK,CCOMM,2,'  ')
        WRITE(6,*) 'Nephelometer data values :-'
        WRITE(6,*) CCOMM(1:80)
        !background zero data
        CCOMM = 'RZ'
        CALL H_NEPH_COM(ISTATOK,CCOMM,2,'  ')
        WRITE(6,*) 'Nephelometer zero data :-'
        WRITE(6,*) CCOMM
!
! read information for menu
        CCOMM = 'SMZ'
        CALL H_NEPH_COM(ISTATOK,CCOMM,3,'  ')
        READ(CCOMM,'(BN,I2)') STATUS(49)
        STATUS(49) = -1 - STATUS(49)
        CCOMM = 'STP'
        CALL H_NEPH_COM(ISTATOK,CCOMM,3,'  ')
        READ(CCOMM,'(BN,I4)') STATUS(50)
        STATUS(50) = -1 - STATUS(50)
        CCOMM = 'SMB'
        CALL H_NEPH_COM(ISTATOK,CCOMM,3,'  ')
        READ(CCOMM,'(BN,I4)') STATUS(51)
        STATUS(51) = -1 - STATUS(51)
        CCOMM = 'H'
        CALL H_NEPH_COM(ISTATOK,CCOMM,1,'  ')
        READ(CCOMM,'(BN,I4)') STATUS(52)
        STATUS(52) = -1 - STATUS(52)
      ELSE
        CALL LOG_MESS('No response from Nephelometer')
        CALL ST_MESS('No response from Nephelometer')
      ENDIF
      RETURN
      END
********************************************************************************
      SUBROUTINE  H_NEPH_COM(ISTATOK,CCOMM,ICLEN,CRESP)
!
! submits command CCOMM to nephelometer and returns response in CCOMM.
! If an invalid response or time out occurs ISTATOK is set .FALSE.
!
      IMPLICIT       NONE
      CHARACTER*(*)  CCOMM
      CHARACTER*90   CLINE,CERR
      CHARACTER*2    CRESP
      LOGICAL*4      ISTATOK
      INTEGER*4      INEPHCHAN,IEC,IC1,IC2,ICLEN,IOLDSTAT,IFUNCT,ISTAT
      INTEGER*2      IOSB(4)  
      COMMON /NEPHCOM/ INEPHCHAN
      INCLUDE   '($DVIDEF)'
      INCLUDE   '($IODEF)'
      INCLUDE   '($SSDEF)'
      INCLUDE   '($SYSSRVNAM)'
!
      IOLDSTAT = ISTATOK
      ISTATOK =.TRUE.
      IEC = 0
      CLINE = CCOMM(1:ICLEN)//CHAR(13)
      CERR = CCOMM(1:ICLEN)//CHAR(13)
      IFUNCT = IO$_WRITEVBLK         !write message to nephelometer
      ISTAT = SYS$QIO (,%VAL(INEPHCHAN),%VAL(IFUNCT),IOSB,,,
     &                 %VAL(%LOC(CLINE)),%VAL(ICLEN+1),,,,)
      ! wait 2 sec for returned message
      IFUNCT = IO$_READVBLK.OR.IO$M_TIMED.OR.IO$M_PURGE
      IF (CRESP(1:2) .EQ. '  ') THEN    !no need to wait for valid response
        ISTAT = SYS$QIOW (,%VAL(INEPHCHAN),%VAL(IFUNCT),IOSB,,,
     &                    %VAL(%LOC(CLINE)),%VAL(90),%VAL(2),,,)
      ELSE
        DO WHILE(CLINE(1:2) .NE. CRESP(1:2) .AND.
     &           CLINE(1:5) .NE. 'ERROR' .AND.
     &           IEC .LT. 30)           !only accept valid response or error
          ISTAT = SYS$QIOW (,%VAL(INEPHCHAN),%VAL(IFUNCT),IOSB,,,
     &                      %VAL(%LOC(CLINE)),%VAL(90),%VAL(2),,,)
          IEC = IEC + 1
        ENDDO
      ENDIF
! remove control characters
      WRITE(CCOMM,'(20A)') '     '
      IC2 = 1
      IC1 = 1
      DO WHILE(IC1 .LT. 90 .AND. ICHAR(CLINE(IC1:IC1)) .GE. 20) 
        IF (ICHAR(CLINE(IC1:IC1)) .GE. 20) THEN
          CCOMM(IC2:IC2) = CLINE(IC1:IC1)
          IC2=IC2+1
        ENDIF 
        IC1 = IC1 + 1
      ENDDO
      IF (CCOMM(1:5) .EQ. 'ERROR') THEN
        CALL LOG_MESS('Nephelometer command error '//CERR(1:ICLEN))
      ELSE IF (IEC .GE. 30) THEN 
        CALL LOG_MESS('Nephelometer response error '//CERR(1:ICLEN))
        ISTATOK = .FALSE.
      ELSE IF (IOSB(1) .EQ. SS$_TIMEOUT) THEN
        IF (IOLDSTAT) THEN
          CALL LOG_MESS('No response in time limit')
        ENDIF
        ISTATOK = .FALSE.
      ENDIF
!
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
      SUBROUTINE IDATE1(IM,ID,IY)
!
! Replaces the DATE Fortran instrinsic and allows programs to compile without
! Y2000 warnings.  Returns date as three integers.
!
      IMPLICIT  NONE
      INTEGER*4 IVAL(8),IM,ID,IY
      CALL DATE_AND_TIME(,,,IVAL)
      IM=IVAL(2)
      ID=IVAL(3)
      IY=MOD(IVAL(1),100)
      RETURN
      END
