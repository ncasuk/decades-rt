!
! DRS_CONTROL.FOR - Controls the Ethernet packet based Data Recording System on 
! HORACE.
!
! Build and run with:-
!
!     $ FORT DRS_CONTROL /WARN=NOTRUNC
!     $ LINK DRS_CONTROL,DRS_SUBS/LIB,DRS_COM/OPT
!     $ RUN DRS_CONTROL
!
! Notes:
!
! 1. All input/output is from/to an ordinary terminal window.
! 2. All communication to the DRS process is throught the DRS_COM global
!    common.
! 3. Only one person can run this program at at time.
! 4. Requires the HORACE logical name to be defined as dir with DRS files.
! 5. Requires DRS_CONTROL_PRINTER logical name to be defined to a print queue.
!
      PROGRAM DRS_CONTROL
      IMPLICIT NONE
      INTEGER*4 ICHOICE,IOS,ISTAT,LIB$SPAWN,IL,I,ICHOICE7,INPUT,IOSB(2),
     &    ICHAN
      CHARACTER CLINE*80,CLOG1*80,CLOG2*80
      INCLUDE '($SYSSRVNAM)'
      INCLUDE '($SSDEF)'
      INCLUDE 'DRS_GDEF.FOR'
      COMMON  INPUT,IOSB,ICHAN
!
! Associate with common event flag cluster 
!
      ISTAT=SYS$ASCEFC(%VAL(64),'H_CEF_CLUSTER',,) !Associate with common EFs
      IF(ISTAT.NE.SS$_NORMAL) CALL LIB$SIGNAL(%VAL(ISTAT))
      ISTAT=SYS$ASSIGN('TT:',ICHAN,,) !Now assign for output
      IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
!
! Check required logical names are defined, and program not in use.
!
      CALL GET_LOGICAL('HORACE',CLOG1)
      IF(CLOG1.EQ.' ') 
     &    WRITE(6,'(A)') ' Logical name HORACE is not defined'
      CALL GET_LOGICAL('DRS_CONTROL_PRINTER',CLOG2)
      IF(CLOG2.EQ.' ') WRITE(6,'(A)') 
     &    ' Logical name DRS_CONTROL_PRINTER is not defined'
      IF(CLOG1.EQ.' '.OR.CLOG2.EQ.' ') CALL EXIT
      OPEN(10,FILE='HORACE:DRS_CONTROL.CHK',STATUS='OLD',
     &    IOSTAT=IOS)
      IF(IOS.NE.0) THEN
        WRITE(6,'(A)') ' DRS_CONTROL is in use by another user'
        CALL EXIT
      END IF
!
10    ITSTDRS=1
      ISTAT=SYS$SETEF(%VAL(83))
      WRITE(6,'(A)') CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H'
      WRITE(6,'(A)') ' DRS Control Options'
      WRITE(6,'(A)') ' '
      WRITE(6,'(A)') '    0  Exit'
      WRITE(6,'(A)') '    1  Stop DRS process'
      WRITE(6,'(A)') '    2  Start DRS process'
      WRITE(6,'(A)') '    3  Set flight number'
      WRITE(6,'(A)') '    4  Stop data recording'
      WRITE(6,'(A)') '    5  Start data recording'
      WRITE(6,'(A)') '    6  Show DRS status'
      WRITE(6,'(A)') '    7  Show DLU stats'
      WRITE(6,'(A)') '    8  Show DRS log file'
      WRITE(6,'(A)') '    9  Master Time Generator options'
      WRITE(6,'(A)') '   10  DLU configuration options'
      WRITE(6,'(A)') '   11  DRS data'
      WRITE(6,'(A)') '   12  Restart DRS process'
!      WRITE(6,'(A)') '   13  DLU time status'
      WRITE(6,'(A)') '  '
      CALL LIB$WAIT(0.1)
      IF(ITSTDRS.NE.0) THEN
        WRITE(6,'(A)') ' DRS:           Stopped'
      ELSE
        WRITE(6,'(A)') ' DRS:           Running'
        IF(CFLTNUM.EQ.' ') WRITE(6,'(A)') ' Flight number: None'
        IF(CFLTNUM.NE.' ') WRITE(6,'(A)') ' Flight number: '//CFLTNUM
        IF(IDATHLD.EQ.1) WRITE(6,'(A)') ' Data:          Hold'
        IF(IDATHLD.EQ.0) WRITE(6,'(A)') ' Data:          Recording'
      END IF
      WRITE(6,'(A)') '  '
      WRITE(6,'(A)') '$Enter choice [0]: '
      ICHOICE=-1
      DO WHILE(ICHOICE.LT.0.OR.ICHOICE.GT.13)
        READ(5,'(A)',IOSTAT=IOS) CLINE
        IF(IOS.LT.0) CALL EXIT
        IF(CLINE.EQ.' ') CLINE='0'
        READ(CLINE,'(BN,I)',IOSTAT=IOS) ICHOICE
      END DO
!
      IF(ICHOICE.EQ.0) THEN            !Exit
        CALL EXIT
      ELSE IF(ICHOICE.EQ.1) THEN       !Stop DRS
        IF(ITSTDRS.EQ.1) THEN
          WRITE(6,'(A)') ' '
          WRITE(6,'(A)') 
     &        ' DRS is not responding, but will attempt to stop anyhow'
        END IF
        ISTAT=LIB$SPAWN('@HORACE:DRS.COM STOP',,,128,,,,,,,,)
        IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
        WRITE(6,'(A)') ' '
        WRITE(6,'(A)') ' DRS stopped'
      ELSE IF(ICHOICE.EQ.2) THEN       !Start DRS
        WRITE(6,'(A)') '  '
        IF(ITSTDRS.EQ.0) THEN
          WRITE(6,'(A)') ' DRS is already running'
        ELSE
          ISTAT=LIB$SPAWN('@HORACE:DRS.COM START',,,128,,,,,,,,)
          IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
        END IF
      ELSE IF(ICHOICE.EQ.3) THEN       !Set flight number
        IF(ITSTDRS.EQ.1) THEN
          WRITE(6,'(A)') '  '
          WRITE(6,'(A)') 
     &        ' DRS must be running before setting flight number'
        ELSE
          IF(CFLTNUM.NE.' ') THEN
            WRITE(6,'(A)') '  '
            WRITE(6,'(A)') ' Current flight number is '//CFLTNUM
          END IF
          WRITE(6,'(A)') '  '
          ISTAT=0
          DO WHILE(ISTAT.NE.1)
            WRITE(6,'(A)') '$New flight number: '
            READ(5,'(Q,A)',IOSTAT=IOS) IL,CNEWFNO
            IF(IOS.NE.0) CALL EXIT
            CALL STR$UPCASE(CNEWFNO,CNEWFNO)
            ISTAT=1
            IF(IL.NE.4) ISTAT=0
            IF(CNEWFNO(1:1).LT.'A'.OR.CNEWFNO(1:1).GT.'Z') ISTAT=0
            DO I=2,4
              IF(CNEWFNO(I:I).LT.'0'.OR.CNEWFNO(I:I).GT.'9') ISTAT=0
            END DO
            IF(ISTAT.EQ.1.AND.CNEWFNO.NE.CFLTNUM) THEN
              ICHGFNO=1
              ISTAT=SYS$SETEF(%VAL(83))
            END IF
          END DO
        END IF
      ELSE IF(ICHOICE.EQ.4) THEN       !Stop logging
        IF(ITSTDRS.EQ.1) THEN
          WRITE(6,'(A)') '  '
          WRITE(6,'(A)') 
     &        ' DRS is not running'
        ELSE
          IF(IDATHLD.EQ.1) THEN
            WRITE(6,'(A)') '  '
            WRITE(6,'(A)') ' DRS is not logging'
          ELSE
            ICHGDAT=1
            ISTAT=SYS$SETEF(%VAL(83))
            WRITE(6,'(A)') '  '
            WRITE(6,'(A)') ' DRS logging stopped'
          END IF
        END IF
      ELSE IF(ICHOICE.EQ.5) THEN       !Start logging
        IF(ITSTDRS.EQ.1) THEN
          WRITE(6,'(A)') '  '
          WRITE(6,'(A)') 
     &        ' DRS is not running'
        ELSE
          IF(IDATHLD.EQ.0) THEN
            WRITE(6,'(A)') '  '
            WRITE(6,'(A)') ' DRS is already logging'
          ELSE
            IF(CFLTNUM.EQ.' ') THEN
              WRITE(6,'(A)') '  '
              WRITE(6,'(A)') ' Flight number must be set'//
     &            ' before starting logging'
            ELSE
              ICHGDAT=1
              ISTAT=SYS$SETEF(%VAL(83))
              WRITE(6,'(A)') '  '
              WRITE(6,'(A)') ' DRS logging started'
            END IF
          END IF
        END IF
      ELSE IF(ICHOICE.EQ.6) THEN       !Status
        CALL DRS_STATUS
        GOTO 10
      ELSE IF(ICHOICE.EQ.7) THEN       !Stats
        CALL DRS_STATS
        GOTO 10
      ELSE IF(ICHOICE.EQ.8) THEN       !Show DRS log file
        WRITE(6,'(A)') CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H'
        ISTAT=LIB$SPAWN('TYPE [HORACE]DRS.LOG;',,,128,,,,,,,,)
        IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      ELSE IF(ICHOICE.EQ.9) THEN       !AMTG
107     WRITE(6,'(A)') CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H'
        WRITE(6,'(A)') ' AMTG Control Options'
        WRITE(6,'(A)') ' '
        WRITE(6,'(A)') '    0  Exit'
        WRITE(6,'(A)') '    1  Show AMTG status'
        WRITE(6,'(A)') '    2  Initialize AMTG with battery time'
        WRITE(6,'(A)') '    3  Initialize AMTG with specified time'
        WRITE(6,'(A)') '  '
        WRITE(6,'(A)') '$Enter choice [0]: '
        ICHOICE7=-1
        DO WHILE(ICHOICE7.LT.0.OR.ICHOICE7.GT.3)
          READ(5,'(A)',IOSTAT=IOS) CLINE
          IF(IOS.LT.0) CALL EXIT
          IF(CLINE.EQ.' ') CLINE='0'
          READ(CLINE,'(BN,I)',IOSTAT=IOS) ICHOICE7
        END DO
        IF(ICHOICE7.EQ.0) THEN
          GOTO 10
        ELSE IF(ICHOICE7.EQ.1) THEN
          CALL AMTG_STATUS
          GOTO 107
        ELSE IF(ICHOICE7.EQ.2) THEN
          CALL INIT_TIME(1)
        ELSE IF(ICHOICE7.EQ.3) THEN
          CALL INIT_TIME(2)
        END IF
        WRITE(6,'(A)') ' '
        WRITE(6,'(A)') '$Press any key to continue'
        READ(5,'(A)',IOSTAT=IOS) CLINE
        IF(IOS.LT.0) CALL EXIT
        GOTO 107
      ELSE IF(ICHOICE.EQ.10) THEN      !Change DLU config
        CALL DRS_CONFIGURATION
        GOTO 10
      ELSE IF(ICHOICE.EQ.11) THEN      !Look at output data
        CALL DRS_DATA
        GOTO 10
      ELSE IF(ICHOICE.EQ.12) THEN      !Restart DRS
        ISTAT=LIB$SPAWN('@HORACE:DRS.COM RESTART',,,128,,,,,,,,)
        IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
        WRITE(6,'(A)') ' '
        WRITE(6,'(A)') ' DRS restarted'
        GOTO 10
      ELSE IF(ICHOICE.EQ.13) THEN      !DLU time status
        CALL DLU_TIME_STATUS
        GOTO 10
      END IF
      
      WRITE(6,'(A)') ' '
      WRITE(6,'(A)') '$Press any key to continue'
      READ(5,'(A)',IOSTAT=IOS) CLINE
      IF(IOS.EQ.0) GOTO 10
      CALL EXIT
      END
!*******************************************************************************
      SUBROUTINE AMTG_STATUS
!
! Displays an AMTG time packet (type 3)
!
      IMPLICIT  NONE
      INTEGER*4 ITEMP,J,ISRTPKT,ICNT,INPUT,IOSB(2),ICHAN
      INTEGER*2 JTEMP(2),JTIMEP(11)
      CHARACTER CSTRING*17,CNAM*6,CDATE*9,CTIME*8,CLINE*80
      CHARACTER CSOURCE(32)*6,CNAME(32)*6
      EQUIVALENCE (ITEMP,JTEMP)
      INCLUDE 'DRS_GDEF.FOR'
      COMMON    INPUT,IOSB,ICHAN
!
      CALL SCREEN('CLEAR','DUMMY',ICNT)
      CALL SCREEN('LINE','Master Time Generator Status',1)
      CALL SCREEN('LINE','Source name',3)
      CALL SCREEN('LINE','Source address',4)
      CALL SCREEN('LINE','Status',5)
      CALL SCREEN('LINE','Clock secs',6)
      CALL SCREEN('LINE','Clock day',7)
      CALL SCREEN('LINE','Clock month',8)
      CALL SCREEN('LINE','Clock year',9)
      CALL SCREEN('LINE','GPS flag',10)
      CALL SCREEN('LINE','Time slip',11)
      CALL SCREEN('LINE','Up time',12)
      CALL SCREEN('LINE','Clock date',13)
      CALL SCREEN('LINE','Clock time',14)
      CALL SCREEN('LINE','P - Print, E - Exit',16)
      CALL SCREEN('UPDATE','DUMMY',ICNT)
!
      ISRTPKT=ITIMPKT
      CALL GET_NAMES(CSOURCE,CNAME)
      CALL INPUT_AST
10    ICNT=0
      INPUT=0
      DO WHILE(ISRTPKT.EQ.ITIMPKT.AND.INPUT.EQ.0)
        CALL LIB$WAIT(0.2)
        ICNT=ICNT+1
        IF(ICNT.EQ.8) THEN
          CALL SCREEN('LINE','No AMTG packets being received',18)
          CALL SCREEN('UPDATE','DUMMY',ICNT)
        END IF
      END DO
      IF(INPUT.NE.0) THEN
        IF(INPUT.EQ.80.OR.INPUT.EQ.112) THEN
          CALL SCREEN('PRINT','DUMMY',ICNT)
          GOTO 10
        ELSE
          CALL INPUT_ABORT
          RETURN
        END IF
      END IF
      ISRTPKT=ITIMPKT
!
      DO J=1,11
        JTIMEP(J)=JTIMPKT(J)
      END DO
      CALL SWAP_ENDS16(JTIMEP(1),2)
      CALL SWAP_ENDS32(JTIMEP(3),1)
      CALL SWAP_ENDS16(JTIMEP(5),5)
      CALL SWAP_ENDS32(JTIMEP(10),1)
!
      CALL SCREEN('LINE','Master Time Generator Status',1)
      CALL ADDR_STRING(CTIMPKT,CSTRING)
      CNAM=' '
      DO J=1,32
        IF(CSOURCE(J).EQ.CTIMPKT) CNAM=CNAME(J)
      END DO
      WRITE(CLINE,'(A,T37,A)') 'Source name',CNAM
      CALL SCREEN('LINE',CLINE,3)
      WRITE(CLINE,'(A,T26,A)') 'Source address',CSTRING
      CALL SCREEN('LINE',CLINE,4)
      IF(JTIMEP(2).EQ.0) THEN
        WRITE(CLINE,'(A,T35,I8,A)')'Status',JTIMEP(2),' Not initialised'
        CALL SCREEN('LINE',CLINE,5)
        JTEMP(1)=JTIMEP(3)
        JTEMP(2)=JTIMEP(4)
        WRITE(CLINE,'(A,T35,I8,A)') 'Battery clock secs',ITEMP+1,
     &      ' Secs past midnight'
        CALL SCREEN('LINE',CLINE,6)
        WRITE(CLINE,'(A,T35,I8)') 'Battery clock day',JTIMEP(5)
        CALL SCREEN('LINE',CLINE,7)
        WRITE(CLINE,'(A,T35,I8)') 'Battery clock month',JTIMEP(6)
        CALL SCREEN('LINE',CLINE,8)
        WRITE(CLINE,'(A,T35,I8)') 'Battery clock year',JTIMEP(7)
        CALL SCREEN('LINE',CLINE,9)
        IF(JTIMEP(8).EQ.1) 
     &      WRITE(CLINE,'(A,T35,I8,A)') 'GPS flag',JTIMEP(8),' Valid'
        IF(JTIMEP(8).NE.1) 
     &      WRITE(CLINE,'(A,T35,I8,A)') 'GPS flag',JTIMEP(8),' Invalid'
        CALL SCREEN('LINE',CLINE,10)
        WRITE(CLINE,'(A,T35,I8,A)') 'Time slip',
     &      NINT((JTIMEP(9)*1000.)/2048.),' ms (-ve is AMTG slow)'
        CALL SCREEN('LINE',CLINE,11)
        JTEMP(1)=JTIMEP(10)
        JTEMP(2)=JTIMEP(11)
        WRITE(CLINE,'(A,T35,I8,A)') 
     &      'Up time',ITEMP,' Secs since switch on'
        CALL SCREEN('LINE',CLINE,12)
        CALL AMTG_TIME(JTIMEP,CDATE,CTIME)
        WRITE(CLINE,'(A,T35,A)') 'Battery clock date',CDATE
        CALL SCREEN('LINE',CLINE,13)
        WRITE(CLINE,'(A,T35,A)') 'Battery clock time',CTIME
        CALL SCREEN('LINE',CLINE,14)
      ELSE
        IF(JTIMEP(2).EQ.1) THEN
          WRITE(CLINE,'(A,T35,I8,A)') 
     &        'Status',JTIMEP(2),' Initialised from GPS'
        ELSE IF(JTIMEP(2).EQ.2) THEN
          WRITE(CLINE,'(A,T35,I8,A)')
     &        'Status',JTIMEP(2),' Initialised from battery clock'
        ELSE IF(JTIMEP(2).EQ.3) THEN
          WRITE(CLINE,'(A,T35,I8,A)') 
     &        'Status',JTIMEP(2),' Initialised from supplied value'
        END IF
        CALL SCREEN('LINE',CLINE,5)
        JTEMP(1)=JTIMEP(3)
        JTEMP(2)=JTIMEP(4)
        WRITE(CLINE,'(A,T35,I8,A)') 
     &      'ATMG clock secs',ITEMP+1,' Secs past midnight'
        CALL SCREEN('LINE',CLINE,6)
        WRITE(CLINE,'(A,T35,I8)') 'ATMG clock day',JTIMEP(5)
        CALL SCREEN('LINE',CLINE,7)
        WRITE(CLINE,'(A,T35,I8)') 'ATMG clock month',JTIMEP(6)
        CALL SCREEN('LINE',CLINE,8)
        WRITE(CLINE,'(A,T35,I8)') 'ATMG clock year',JTIMEP(7)
        CALL SCREEN('LINE',CLINE,9)
        IF(JTIMEP(8).EQ.1) WRITE(CLINE,'(A,T35,I8,A)') 
     &      'GPS flag',JTIMEP(8),' Valid'
        IF(JTIMEP(8).NE.1) WRITE(CLINE,'(A,T35,I8,A)') 
     &      'GPS flag',JTIMEP(8),' Invalid'
        CALL SCREEN('LINE',CLINE,10)
        WRITE(CLINE,'(A,T35,I8,A)') 'Time slip',
     &      NINT((JTIMEP(9)*1000.)/2048.),' ms (-ve is AMTG slow)'
        CALL SCREEN('LINE',CLINE,11)
        JTEMP(1)=JTIMEP(10)
        JTEMP(2)=JTIMEP(11)
        WRITE(CLINE,'(A,T35,I8,A)') 
     &      'Up time',ITEMP,' Secs since initialisation'
        CALL SCREEN('LINE',CLINE,12)
        CALL AMTG_TIME(JTIMEP,CDATE,CTIME)
        WRITE(CLINE,'(A,T34,A)') 'ATMG clock date',CDATE
        CALL SCREEN('LINE',CLINE,13)
        WRITE(CLINE,'(A,T35,A)') 'ATMG clock time',CTIME
        CALL SCREEN('LINE',CLINE,14)
      END IF
      CALL SCREEN('LINE','P - Print, E - Exit',16)
      CALL SCREEN('LINE',' ',18)
      CALL SCREEN('UPDATE','DUMMY',ICNT)
!
      GOTO 10
      END
!*******************************************************************************
      SUBROUTINE GET_NAMES(CSOURCE,CNAMES)
!
! Gets equivalence list between 6 byte ethernet addresses and actual names
! 
      IMPLICIT  NONE
      INTEGER*4 I,IC,ISTS
      CHARACTER CSOURCE(32)*6,CNAMES(32)*6,CEADD(32)*17

      DO I=1,32
        CSOURCE(I)=' '
        CNAMES(I)=' '
      END DO
      CALL DRS_ADDRESSES_READ(CEADD,CNAMES,IC)
      DO I=1,IC
        CALL STRING_ADDR(CEADD(I),%REF(CSOURCE(I)),ISTS)
      END DO
      RETURN
      END
!*******************************************************************************
      SUBROUTINE DRS_STATUS
      IMPLICIT  NONE
      INTEGER*4 IL,IOS,LAN,SYS$SETEF,ISTAT,I,IADDR,J,INPUT,IOSB(2),
     &    ICHAN,ICNT,IL1
      INTEGER*2 JSTS
      CHARACTER CLINE*80,CEADD(32)*17,CNAME(32)*6,CEADDR*17,CSTS*20
      INCLUDE  'DRS_GDEF.FOR'
      COMMON   INPUT,IOSB,ICHAN

      CALL INPUT_AST
      INPUT=0
      CALL SCREEN('CLEAR','DUMMY',IL)
11    ITSTDRS=1
      ISTAT=SYS$SETEF(%VAL(83))
      IL=1
      CALL SCREEN('LINE','DRS status',IL)
      IL=IL+2
      ICNT=0
      DO WHILE(ITSTDRS.EQ.1.AND.ICNT.LT.10)
        CALL LIB$WAIT(0.1)
        ICNT=ICNT+1
      END DO
      IF(ITSTDRS.EQ.1) THEN
        CLINE='DRS stopped.'
      ELSE
        CLINE='DRS running. '
        IL1=LAN(CLINE)
        IF(CFLTNUM.EQ.' ') CLINE(IL1+2:)=' Flight number not defined. '
        IF(CFLTNUM.NE.' ') CLINE(IL1+2:)=' Flight number '//CFLTNUM
     &      //'. '
        IL1=LAN(CLINE)
        IF(IDATHLD.EQ.1) CLINE(IL1+2:)=' Data on hold. '
        IF(IDATHLD.EQ.0) CLINE(IL1+2:)=' Data recording. '
      END IF
      CALL STR_COMPRESS(CLINE)
      CALL SCREEN('LINE',CLINE,IL)
      IL=IL+2     
!
      IF(IDLUSTS(33).EQ.0) THEN
        CLINE='AMTG not sending data. '
      ELSE
        CLINE='AMTG sending data. '
        JSTS=JTIMPKT(2)
        CALL SWAP_ENDS16(JSTS,1)
        IL1=LAN(CLINE)
        IF(JSTS.EQ.0) CLINE(IL1+2:)=' Not initialized.'
        IF(JSTS.EQ.1) CLINE(IL1+2:)=' Initialized from GPS.'
        IF(JSTS.EQ.2) CLINE(IL1+2:)=' Initialized from battery clock.'
        IF(JSTS.EQ.3) CLINE(IL1+2:)=' Initialized manually.'
      END IF
      CALL STR_COMPRESS(CLINE)
      CALL SCREEN('LINE',CLINE,IL)
      IL=IL+2     
!
      IF(ITSTDRS.EQ.0) THEN
        WRITE(CLINE,'(X,A,I4,A,4(I5,A))',IOSTAT=IOS) 'ISS',IISSNUM,
     &      ':',ICNFCNT,' paras, ',ICNFSIZ,' words configured. ',
     &      IPARCNT,' paras, ',IISSSIZ,' words required.'
        CALL STR_COMPRESS(CLINE)
        CALL SCREEN('LINE',CLINE,IL)
        IL=IL+2
        CALL DRS_ADDRESSES_READ(CEADD,CNAME,IADDR)
        DO I=1,IDLUCNT
          CEADDR=' '
          DO J=1,IADDR
            IF(CNAME(J).EQ.CDLUNAM(I)) CEADDR=CEADD(J)
          END DO
          CSTS=' '
          IF(IDLUSTS(I).EQ.0) CSTS='No packets'
          IF(IDLUSTS(I).EQ.1) CSTS='Waiting init load'
          IF(IDLUSTS(I).EQ.2) CSTS='Init load failed'
          IF(IDLUSTS(I).EQ.3) CSTS='No data packets'
          IF(IDLUSTS(I).EQ.4) CSTS='Clock not init'
          IF(IDLUSTS(I).EQ.5) CSTS='Sending data'
          IF(IDLUSTS(I).EQ.9) CSTS='Unknown status'
          CLINE=CDLUNAM(I)//'  '//CEADDR//'  '//CSTS
          CALL SCREEN('LINE',CLINE,IL)
          IL=IL+1
        END DO
      ELSE
        CLINE=' '
        CALL SCREEN('LINE',CLINE,IL)
        IL=IL+2
        DO I=1,IDLUCNT
          CALL SCREEN('LINE',CLINE,IL)
          IL=IL+1
        END DO
      END IF
      CALL SCREEN('LINE','P - Print, E - Exit',IL+1)
      CALL SCREEN('UPDATE','DUMMY',IL)
!
      ICNT=0
      DO WHILE(INPUT.EQ.0.AND.ICNT.LT.5)
        CALL LIB$WAIT(0.2)
        ICNT=ICNT+1
      END DO
      IF(INPUT.NE.0) THEN
        IF(INPUT.EQ.80.OR.INPUT.EQ.112) THEN
          CALL SCREEN('PRINT','DUMMY',IL)
          INPUT=0
        ELSE
          CALL INPUT_ABORT
          RETURN
        END IF
      END IF
      GOTO 11
      END
!*******************************************************************************
      SUBROUTINE DRS_STATS
      INTEGER*4 I,J,IOS,IOSB(2),ICHAN,INPUT,ICNT,IL,ISTAT,IADDR,IID,II
      CHARACTER CSOURCE(32)*6,CNAMES(32)*6,CLINE*80,CLIN*2,CNAM*6,
     &    CSTRING*17,CID*2
      INCLUDE 'DRS_GDEF.FOR'
      INCLUDE '($SYSSRVNAM)'
      COMMON   INPUT,IOSB,ICHAN

      CALL INPUT_AST
      INPUT=0
      IADDR=0
      CID=' '
      IID=0
      CALL SCREEN('CLEAR','DUMMY',ICNT)
11    CALL SCREEN('LINE','DRS statistics',1)
      IF(IADDR.EQ.0) THEN
        CLINE='ID   Name  Typ 3 Typ 4 Typ 5 Typ 6 Typ 7 Inval Incmp   '
     &      //'T1   T2   T3   T4   T5'
      ELSE
        CLINE='ID Ethernet address    Name  Typ 3 Typ 4 Typ 5 Typ 6 '
     &      //'Typ 7 Inval Incmp'
      END IF
      CALL SCREEN('LINE',CLINE,3)
      CALL GET_NAMES(CSOURCE,CNAMES)
      IL=4
      DO I=1,ISOURCE
        CALL ADDR_STRING(CSRCLST(I),CSTRING)
        CNAM=' '
        II=0
        DO J=1,32
          IF(CSRCLST(I).EQ.CSOURCE(J)) THEN
            CNAM=CNAMES(J)
          END IF
          IF(CSRCLST(I).EQ.CDLUADD(J)) THEN
            II=J
          END IF
        END DO
        WRITE(CLIN,'(I2.2)',IOSTAT=IOS) I
        IF(IADDR.EQ.0) THEN
          IF(II.NE.0) THEN
            WRITE(CLINE,'(A,7I6,5I5)',IOSTAT=IOS) CLIN//' '//
     &        CNAM//' ',(IPKTCNT(I,J),J=3,7),IPKTCNT(I,33),IMESERR(II),
     &        (ITIMSTS(I,J),J=1,5)
          ELSE
            WRITE(CLINE,'(A,7I6,5I5)',IOSTAT=IOS) CLIN//' '//
     &        CNAM//' ',(IPKTCNT(I,J),J=3,7),IPKTCNT(I,33),0,
     &        (ITIMSTS(I,J),J=1,5)
          END IF
        ELSE
          IF(II.NE.0) THEN
            WRITE(CLINE,'(A,7I6)',IOSTAT=IOS) CLIN//' '//CSTRING//' '
     &        //CNAM//' ',(IPKTCNT(I,J),J=3,7),IPKTCNT(I,33),IMESERR(II)
          ELSE
            WRITE(CLINE,'(A,7I6)',IOSTAT=IOS) CLIN//' '//CSTRING//' '
     &        //CNAM//' ',(IPKTCNT(I,J),J=3,7),IPKTCNT(I,33),0
          END IF
        END IF
        IF(CLINE.NE.' ') THEN
          IL=IL+1
          CALL SCREEN('LINE',CLINE,IL)
        END IF
      END DO
      IF(IREADER.NE.0) THEN
        WRITE(CLINE,'(I,A)') IREADER,' read errors.'
        CALL STR_COMPRESS(CLINE)
        IL=IL+1
        CALL SCREEN('LINE',CLINE,IL)
      END IF
      IF(ICNTERR.NE.0) THEN
        WRITE(CLINE,'(I,A)') ICNTERR,' controller errors.'
        CALL STR_COMPRESS(CLINE)
        IL=IL+1
        CALL SCREEN('LINE',CLINE,IL)
      END IF
      IF(ITIMOUT.NE.0) THEN
        WRITE(CLINE,'(I,A)') ITIMOUT,' timeouts.'
        CALL STR_COMPRESS(CLINE)
        IL=IL+1
        CALL SCREEN('LINE',CLINE,IL)
      END IF
      IF(IBUFERR.NE.0) THEN
        WRITE(CLINE,'(I,A)') IBUFERR,' out of buffers.'
        CALL STR_COMPRESS(CLINE)
        IL=IL+1
        CALL SCREEN('LINE',CLINE,IL)
      END IF
      IF(INULLIN.NE.0) THEN
        WRITE(CLINE,'(I,A)') INULLIN,' empty messages.'
        CALL STR_COMPRESS(CLINE)
        IL=IL+1
        CALL SCREEN('LINE',CLINE,IL)
      END IF
      IL=IL+2
      CALL SCREEN('LINE','P - Print, Z - Zero, A - Addresses, nn - ID,'
     &    //' E - Exit',IL)
      CALL SCREEN('UPDATE','DUMMY',IL)
!
      ICNT=0
      DO WHILE(INPUT.EQ.0.AND.ICNT.LT.5)
        CALL LIB$WAIT(0.2)
        ICNT=ICNT+1
      END DO
      IF(INPUT.NE.0) THEN
        IF(INPUT.EQ.80.OR.INPUT.EQ.112) THEN !P or p
          CALL SCREEN('PRINT','DUMMY',ICNT)
          INPUT=0
        ELSE IF(INPUT.EQ.90.OR.INPUT.EQ.122) THEN !Z or z
          IZERCNT=1
          ISTAT=SYS$SETEF(%VAL(83))
          INPUT=0
        ELSE IF(INPUT.EQ.65.OR.INPUT.EQ.97) THEN !A or a
          IADDR=1-IADDR
          INPUT=0
        ELSE IF(INPUT.EQ.69.OR.INPUT.EQ.101) THEN !E or e
          CALL INPUT_ABORT
          RETURN
        ELSE IF(INPUT.GE.48.AND.INPUT.LE.57) THEN !0-9
          IID=IID+1
          IF(IID.GT.2) THEN
            IID=0
          ELSE
            CID(IID:IID)=CHAR(INPUT)
          END IF
          IF(IID.EQ.2) THEN
            IF(CID.GE.'01'.AND.CID.LE.'32') CALL DLU_STATS(CID)
            IID=0
          END IF
          INPUT=0
        END IF
      END IF
      GOTO 11
      END
!*******************************************************************************
      SUBROUTINE AMTG_TIME(JPKT,CDATE,CTIME)
!
! Computes ASCII time (hh:mm:ss) and date (dd-Mmm-yy) from time packet in JPKT.
!
      IMPLICIT  NONE
      INTEGER*4 ITEMP,IHRS,IMIN,ISEC,IOS
      INTEGER*2 JPKT(11),JTEMP(2)
      CHARACTER CDATE*9,CTIME*8,CYEAR*4,CMON(12)*3,CMONTH*3
      EQUIVALENCE (ITEMP,JTEMP)
      DATA CMON /'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep',
     &    'Oct','Nov','Dec'/

      JTEMP(1)=JPKT(3)
      JTEMP(2)=JPKT(4)
      ITEMP=ITEMP+1                    !Add one because packet time is UTC-1
      ISEC=MOD(ITEMP,60)
      IMIN=MOD(ITEMP/60,60)
      IHRS=ITEMP/3600
      WRITE(CYEAR,'(I4.4)',IOSTAT=IOS) JPKT(7)
      CMONTH='***'
      IF(JPKT(6).GE.1.AND.JPKT(6).LE.12) CMONTH=CMON(JPKT(6))
      WRITE(CDATE,'(I2.2,A)',IOSTAT=IOS) JPKT(5),'-'//CMONTH
     &    //'-'//CYEAR(3:4)
      WRITE(CTIME,'(2(I2.2,A),I2.2)',IOSTAT=IOS) IHRS,':',IMIN,':',ISEC
      RETURN
      END
!*******************************************************************************
      SUBROUTINE INIT_TIME(IMODE)
!
! Requests DRS to send an AMTG initialise time packet (type 2).  If IMODE is 1
! then just send request to use AMTG battery backed time, else get the time to
! use and send it.
!
      IMPLICIT  NONE
      INTEGER*4 IMODE,I,IOS,IOS1,IOS2,IOS3,ITEMP,ID,IM,IY,IHR,IMN,ISC,
     &    ISTAT
      INTEGER*2 JTEMP(2),JSENDP(7)
      CHARACTER CDATE*9,CTIME*8,CDATE1*9,CTIME1*9,CMON(12)*3
      INCLUDE   'DRS_GDEF.FOR'
      INCLUDE   '($SYSSRVNAM)'
      EQUIVALENCE (ITEMP,JTEMP)
      DATA      CMON /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     &    'SEP','OCT','NOV','DEC'/
      
      ISNDSIZ=7*2
      JSENDP(1)=2
      WRITE(6,'(A)') ' '
      IF(IMODE.EQ.1) THEN
        JSENDP(2)=1
        DO I=3,7
          JSENDP(I)=0
        END DO
      ELSE
        JSENDP(2)=2
        CALL DATE1(CDATE)
        CALL TIME(CTIME)
        IOS=1
        DO WHILE(IOS.NE.0)
          WRITE(6,'(A)') '$Initialisation date ['//CDATE//']: '
          READ(5,'(A)',IOSTAT=IOS1) CDATE1
          IF(IOS1.NE.0) CALL EXIT
          IF(CDATE1.EQ.' ') CDATE1=CDATE
          CALL STR$UPCASE(CDATE1,CDATE1)
          READ(CDATE1(1:2),'(I2.2)',IOSTAT=IOS1) ID
          IM=0
          DO I=1,12
            IF(CDATE1(4:6).EQ.CMON(I)) IM=I
          END DO
          IF(IM.NE.0) IOS2=0
          IF(IM.EQ.0) IOS2=1
          READ(CDATE1(8:9),'(I2.2)',IOSTAT=IOS3) IY
          IY=2000+IY
          IOS=IOS1+IOS2+IOS3
        END DO
        JSENDP(5)=ID
        JSENDP(6)=IM
        JSENDP(7)=IY
        IOS=1
        DO WHILE(IOS.NE.0)
          WRITE(6,'(A)') '$Initialisation time ['//CTIME//']: '
          READ(5,'(A)',IOSTAT=IOS1) CTIME1
          IF(IOS1.NE.0) CALL EXIT
          IF(CTIME1.EQ.' ') CTIME1=CTIME
          READ(CTIME1(1:2),'(I2.2)',IOSTAT=IOS1) IHR
          READ(CTIME1(4:5),'(I2.2)',IOSTAT=IOS2) IMN
          READ(CTIME1(7:8),'(I2.2)',IOSTAT=IOS3) ISC
          IOS=IOS1+IOS2+IOS3
        END DO
        ITEMP=IHR*3600+IMN*60+ISC
        JSENDP(3)=JTEMP(1)
        JSENDP(4)=JTEMP(2)
      END IF
      CALL SWAP_ENDS16(JSENDP(1),2)    !Reorder bytes
      CALL SWAP_ENDS32(JSENDP(3),1)
      CALL SWAP_ENDS16(JSENDP(5),3)
      DO I=1,7                         !Transfer message
        JSNDPKT(I)=JSENDP(I)
      END DO
      CSNDPKT=CTIMPKT                  !Initiate the send
      ISTAT=SYS$SETEF(%VAL(83))
      WRITE(6,'(A)') ' Initialization command sent to AMTG'
      RETURN
      END
!*******************************************************************************
      SUBROUTINE DRS_CONFIGURATION
!
! Handles the various configuration menus by getting user requirements and
! calling the appropriate routines.
!
      IMPLICIT  NONE
      INTEGER*4 ICHOICE,IOS
      CHARACTER CLINE*80

10    ICHOICE=-1
      DO WHILE(ICHOICE.LT.0.OR.ICHOICE.GT.4)
        WRITE(6,'(A)') CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H'
        WRITE(6,'(A)') ' DRS Configuration Options'
        WRITE(6,'(A)') ' '
        WRITE(6,'(A)') '    0  Exit'
        WRITE(6,'(A)') '    1  DRS parameters'
        WRITE(6,'(A)') '    2  Recorded parameters (ISS)'
        WRITE(6,'(A)') '    3  Ethernet addresses'
        WRITE(6,'(A)') '    4  DLU configuration'
        WRITE(6,'(A)') '  '
        WRITE(6,'(A)') '$Enter choice [0]: '
        READ(5,'(A)',IOSTAT=IOS) CLINE
        IF(IOS.NE.0) CALL EXIT
        IF(CLINE.EQ.' ') CLINE='0'
        READ(CLINE,'(BN,I)',IOSTAT=IOS) ICHOICE
        IF(IOS.NE.0) ICHOICE=-1
      END DO
!
      IF(ICHOICE.EQ.0) THEN            !Exit
        RETURN
      ELSE IF(ICHOICE.EQ.1) THEN
14      ICHOICE=-1
        DO WHILE(ICHOICE.LT.0.OR.ICHOICE.GT.3)
          WRITE(6,'(A)') CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H'
          WRITE(6,'(A)') ' DRS Parameter Options'
          WRITE(6,'(A)') ' '
          WRITE(6,'(A)') '    0  Exit'
          WRITE(6,'(A)') '    1  List DRS parameters'
          WRITE(6,'(A)') '    2  Add DRS parameter'
          WRITE(6,'(A)') '    3  Print DRS parameters'
          WRITE(6,'(A)') '  '
          WRITE(6,'(A)') '$Enter choice [0]: '
          READ(5,'(A)',IOSTAT=IOS) CLINE
          IF(IOS.NE.0) CALL EXIT
          IF(CLINE.EQ.' ') CLINE='0'
          READ(CLINE,'(BN,I)',IOSTAT=IOS) ICHOICE
          IF(IOS.NE.0) ICHOICE=-1
        END DO
        IF(ICHOICE.EQ.1) THEN
          CALL DRS_PARAMETERS_SHOW
        ELSE IF(ICHOICE.EQ.2) THEN
          CALL DRS_PARAMETERS_ADD
        ELSE IF(ICHOICE.EQ.3) THEN
          CALL DRS_PARAMETERS_PRINT
        END IF
        IF(ICHOICE.NE.0) GOTO 14
      ELSE IF(ICHOICE.EQ.2) THEN
13      ICHOICE=-1
          DO WHILE(ICHOICE.LT.0.OR.ICHOICE.GT.3)
          WRITE(6,'(A)') CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H'
          WRITE(6,'(A)') ' Recorded Parameters (ISS) Options'
          WRITE(6,'(A)') ' '
          WRITE(6,'(A)') '    0  Exit'
          WRITE(6,'(A)') '    1  List recorded parameters'
          WRITE(6,'(A)') '    2  Change recorded parameters'
          WRITE(6,'(A)') '    3  Print recorded parameters'
          WRITE(6,'(A)') '  '
          WRITE(6,'(A)') '$Enter choice [0]: '
          READ(5,'(A)',IOSTAT=IOS) CLINE
          IF(IOS.NE.0) CALL EXIT
          IF(CLINE.EQ.' ') CLINE='0'
          READ(CLINE,'(BN,I)',IOSTAT=IOS) ICHOICE
          IF(IOS.NE.0) ICHOICE=-1
        END DO
        IF(ICHOICE.EQ.1) THEN
          CALL DRS_ISS_SHOW(-1)
        ELSE IF(ICHOICE.EQ.2) THEN
          CALL DRS_ISS_CHANGE
        ELSE IF(ICHOICE.EQ.3) THEN
          CALL DRS_ISS_PRINT
        END IF
        IF(ICHOICE.NE.0) GOTO 13
      ELSE IF(ICHOICE.EQ.3) THEN
11      ICHOICE=-1
        DO WHILE(ICHOICE.LT.0.OR.ICHOICE.GT.3)
          WRITE(6,'(A)') CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H'
          WRITE(6,'(A)') ' Ethernet Address Options'
          WRITE(6,'(A)') ' '
          WRITE(6,'(A)') '    0  Exit'
          WRITE(6,'(A)') '    1  List Ethernet addresses'
          WRITE(6,'(A)') '    2  Change Ethernet addresses'
          WRITE(6,'(A)') '    3  Print Ethernet addresses'
          WRITE(6,'(A)') '  '
          WRITE(6,'(A)') '$Enter choice [0]: '
          READ(5,'(A)',IOSTAT=IOS) CLINE
          IF(IOS.NE.0) CALL EXIT
          IF(CLINE.EQ.' ') CLINE='0'
          READ(CLINE,'(BN,I)',IOSTAT=IOS) ICHOICE
          IF(IOS.NE.0) ICHOICE=-1
        END DO
        IF(ICHOICE.EQ.1) THEN
          CALL DRS_ADDRESSES_SHOW
        ELSE IF(ICHOICE.EQ.2) THEN
          CALL DRS_ADDRESSES_CHANGE
        ELSE IF(ICHOICE.EQ.3) THEN
          CALL DRS_ADDRESSES_PRINT
        END IF
        IF(ICHOICE.NE.0) GOTO 11
      ELSE IF(ICHOICE.EQ.4) THEN
12      ICHOICE=-1
        DO WHILE(ICHOICE.LT.0.OR.ICHOICE.GT.3)
          WRITE(6,'(A)') CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H'
          WRITE(6,'(A)') ' DLU Configuration Options'
          WRITE(6,'(A)') ' '
          WRITE(6,'(A)') '    0  Exit'
          WRITE(6,'(A)') '    1  List DLU configuration'
          WRITE(6,'(A)') '    2  Change DLU configuration'
          WRITE(6,'(A)') '    3  Print DLU configuration'
          WRITE(6,'(A)') '  '
          WRITE(6,'(A)') '$Enter choice [0]: '
          READ(5,'(A)',IOSTAT=IOS) CLINE
          IF(IOS.NE.0) CALL EXIT
          IF(CLINE.EQ.' ') CLINE='0'
          READ(CLINE,'(BN,I)',IOSTAT=IOS) ICHOICE
          IF(IOS.NE.0) ICHOICE=-1
        END DO
        IF(ICHOICE.EQ.1) THEN
          CALL DRS_CONFIG_SHOW
        ELSE IF(ICHOICE.EQ.2) THEN
          CALL DRS_CONFIG_CHANGE
        ELSE IF(ICHOICE.EQ.3) THEN
          CALL DRS_CONFIG_PRINT
        END IF
        IF(ICHOICE.NE.0) GOTO 12
      END IF
      GOTO 10
      END
C*******************************************************************************
      SUBROUTINE INPUT_ABORT
C
C Aborts unsolicted keyboard input, (so can take input through FORTRAN reads).
C Note that this causes an immediate AST which must be handled.
C
      IMPLICIT  NONE
      INTEGER*4 ISTAT,INPUT,IOSB(2),ICHAN
      INCLUDE   '($SYSSRVNAM)'
      COMMON    INPUT,IOSB,ICHAN

      ISTAT=SYS$CANCEL(%VAL(ICHAN))
      IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      RETURN
      END
C*******************************************************************************
      SUBROUTINE INPUT_AST
C
C This routine is used because FORTRAN routines can't call themselves
C
      CALL INPUT_AST1
      RETURN
      END
C*******************************************************************************
      SUBROUTINE INPUT_AST1
C
C Called asynchronously when a character is received from the keyboard:
C
C 1.  Character is returned in INPUT
C 2.  Sets up the next asynchronous trap
C
C Subroutines called:
C
C   INPUT_AST This file
C
C Note:
C
C 1.  All characters are converted to upper case
C 2.  The typeahead buffer is purged beforehand
C 3.  CTRL/Y is trapped before this routine is called
C 4.  The program aborts on any read errors
C
C V1.00  13/09/99  W.D.N.JACKSON
C
      IMPLICIT  NONE
      INTEGER*4 IFNCT,ISTAT,IOSB(2),INPUT,ICHAN
      INCLUDE   '($SYSSRVNAM)'
      INCLUDE   '($IODEF)'
      INCLUDE   '($SSDEF)'
      EXTERNAL  INPUT_AST
      COMMON    INPUT,IOSB,ICHAN

      IF(IOSB(1).EQ.SS$_ABORT) THEN    !Do nothing if was SYS$CANCEL
        IOSB(1)=0
        RETURN
      END IF
      IFNCT=IO$_READVBLK.OR.IO$M_NOECHO.OR.IO$M_CVTLOW
     -    .OR.IO$M_PURGE.OR.IO$M_NOFILTR
      ISTAT=SYS$QIO(%VAL(10),
     -    %VAL(ICHAN),%VAL(IFNCT),IOSB,%REF(INPUT_AST),,
     -    %REF(INPUT),%VAL(1),,,,)     !Read one character
      IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      RETURN
      END     
!*******************************************************************************
      SUBROUTINE SCREEN(CMODE,CLINE,ILINE)
!
! Contains a VT300 screen image, so can update just the changed parts, and so
! has a copy ready for printing as required.  Takes the following values for
! CMODE.
!
! CLEAR - Clears the store and the screen
! PRINT - Prints the store contents to DRS_CONTROL_PRINTER
! LINE -  Changes line ILINE in the store to CLINE
! UPDATE - Sends any changes since last update to the screen
! PASS - Sends ILINE to the screen directly, bypassing store
!
      IMPLICIT  NONE
      INTEGER*4 I,IL,ILINE,LAN,LIB$SPAWN,ISTAT
      CHARACTER CLAST(30)*80,CCURR(30)*80,CLINE*(*),CL*80,CMODE*(*),
     &    CLINE1*80

      IF(CMODE.EQ.'CLEAR') THEN
        DO I=1,30
          CLAST(I)=' '
          CCURR(I)=' '
        END DO
        WRITE(6,'(A)') CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H'
      ELSE IF(CMODE.EQ.'PRINT') THEN
        OPEN(12,FILE='HORACE:DRS_CONTROL.TMP',STATUS='NEW',
     &      CARRIAGECONTROL='LIST')
        DO I=1,30
          IF(CLAST(I).NE.' ') IL=I
        END DO
        DO I=1,IL
          WRITE(12,'(A)') CLAST(I)(1:LAN(CLAST(I)))
        END DO
        CALL DATE1(CL(1:9))
        CALL TIME(CL(12:19))
        WRITE(12,'(A)') ' '
        WRITE(12,'(A)') CL(1:LAN(CL))
        CLOSE(12)
        CLINE1='PRIN/QUEU=DRS_CONTROL_PRINTER/DELE '//
     &    'HORACE:DRS_CONTROL.TMP;'
        ISTAT=LIB$SPAWN(CLINE1,,,,,,,,,,,)
        IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      ELSE IF(CMODE.EQ.'UPDATE') THEN
        DO I=1,30
          IF(CCURR(I).NE.CLAST(I)) THEN
            WRITE(CL,'(I2.2)') I
            WRITE(6,'(A)') CHAR(0)//CHAR(27)//'['//CL(1:2)//';1H'
     &          //CCURR(I)
            CLAST(I)=CCURR(I)
          END IF
        END DO
      ELSE IF(CMODE.EQ.'LINE') THEN
        IF(ILINE.GE.1.AND.ILINE.LE.30) CCURR(ILINE)=CLINE
      ELSE IF(CMODE.EQ.'PASS') THEN
        IF(ILINE.EQ.1) THEN
          WRITE(6,'(A)') CHAR(0)//CLINE(1:LAN(CLINE))//CHAR(13)
        ELSE
          WRITE(6,'(X,A)') CLINE(1:LAN(CLINE))
        END IF
      END IF
      RETURN
      END
!*******************************************************************************
      SUBROUTINE DLU_STATS(CID)
!
! Note that the internal SCREEN store mechanism is not used for some of the
! options in this routine since they may produce more than 24 lines of output.
! Instead the output is written directly to the screen, but there is then no
! print option.
!
      INTEGER*4 I,J,IOS,IOSB(2),ICHAN,INPUT,ICNT,IL,ISTAT,IADDR,IID,
     &    IGETS,ICYCLE,LAN,ITMP,K,JJ,IFRQ(512),IPARAS,I8,
     &    IPARA(128),IFREQ(128),IRSCONF,IPORTS,IPORT,IRGET,IGNOR
      LOGICAL*4 LPRINT
      INTEGER*2 JPKT(750),JTMP(2)
      BYTE      BGETPKT(1500)
      CHARACTER CSOURCE(32)*6,CNAMES(32)*6,CLINE*80,CNAM*6,
     &    CID*2,CSRC*6,CTEMP*4,CTEMP1*6,CPNAM(512)*4,CPNAME(128)*4
      INTEGER*4 IBBRSIG(8),IBBRZER(8),IB,IBBRFRQ
      DATA IBBRSIG /81,82,83,224,91,92,93,227/
      DATA IBBRZER /84,85,86,225,94,95,96,228/
      DATA IBBRFRQ /2/
      INCLUDE 'DRS_GDEF.FOR'
      INCLUDE '($SYSSRVNAM)'
      EQUIVALENCE (JGETPKT,BGETPKT),(JTMP,ITMP)
      COMMON   INPUT,IOSB,ICHAN

      INPUT=0
      IADDR=0
      READ(CID,'(I2.2)',IOSTAT=IOS) IID
      IF(IID.LT.1.OR.IID.GT.32) RETURN
      CALL SCREEN('CLEAR','DUMMY',ICNT)
      CALL GET_NAMES(CSOURCE,CNAMES)
      CNAM=' '
      CSRC=' '
      DO J=1,32
        IF(CSRCLST(IID).EQ.CSOURCE(J)) THEN
          CNAM=CNAMES(J)
          CSRC=CSOURCE(J)
        END IF
      END DO
      ICYCLE=1
11    LPRINT=.TRUE.
      IF(ICYCLE.EQ.1) THEN             !Display status
        CALL SCREEN('LINE','DLU status from '//CNAM,1)
        CALL SCREEN('LINE',' ',2)
        IF(CSRC.EQ.' ') THEN
          ICNT=10
        ELSE
          IGETS=IGETCNT                !Set up read of status packet
          IGETPKT=6
          CGETPKT=CSRC
          JPKT(1)=11
          CALL SWAP_ENDS16(JPKT(1),1)
          ISNDSIZ=2
          JSNDPKT(1)=JPKT(1)
          CSNDPKT=CSRC
          ISTAT=SYS$SETEF(%VAL(83))
          ICNT=0
            DO WHILE(IGETS.EQ.IGETCNT.AND.ICNT.LT.10)
            CALL LIB$WAIT(0.2)
            ICNT=ICNT+1
          END DO
        END IF
        IF(ICNT.EQ.10) THEN
          CLINE='DLU not returning status message'
          CALL SCREEN('LINE',CLINE,3)
          IL=3
        ELSE
          CALL SWAP_ENDS16(JGETPKT(1),3)
          IF(JGETPKT(2).EQ.0) CLINE='Receiving 1Hz AMTG pulses'
          IF(JGETPKT(2).EQ.1) CLINE='Not receiving 1Hz AMTG pulses'
          CALL SCREEN('LINE',CLINE,3)
          IF(JGETPKT(3).EQ.0) CLINE='Receiving AMTG time packets'
          IF(JGETPKT(3).EQ.1) CLINE='Not receiving AMTG time packets'
          CALL SCREEN('LINE',CLINE,4)
          JTMP(1)=JGETPKT(4)
          JTMP(2)=JGETPKT(5)
          CALL SWAP_ENDS32(ITMP,1)
          IF(ITMP.EQ.0) THEN
            CLINE='DLU time not initialised'
          ELSE
            CLINE='            seconds since DLU time initialised'
            WRITE(CLINE(1:11),'(I11)',IOSTAT=IOS) ITMP
            CALL STR_COMPRESS(CLINE)
          END IF
          CALL SCREEN('LINE',CLINE,5)
          CALL SWAP_ENDS16(JGETPKT(6),1)
          IF(JGETPKT(6).EQ.0) CLINE='DLU time not initialised'
          IF(JGETPKT(6).EQ.1)
     &        CLINE='DLU time initialised with GPS AMTG value'
          IF(JGETPKT(6).EQ.2)
     &        CLINE='DLU time initialised with battery AMTG value'
          IF(JGETPKT(6).EQ.3)
     &        CLINE='DLU time initialised with supplied AMTG value'
          CALL SCREEN('LINE',CLINE,6)
          JTMP(1)=JGETPKT(7)
          JTMP(2)=JGETPKT(8)
          CALL SWAP_ENDS32(ITMP,1)
          IF(ITMP.EQ.0) THEN
            CLINE='DLU configuration not initialised'
          ELSE
            CLINE='             seconds since DLU configuration'//
     &          ' initialised'
            WRITE(CLINE(1:11),'(I11)',IOSTAT=IOS) ITMP
            CALL STR_COMPRESS(CLINE)
          END IF
          CALL SCREEN('LINE',CLINE,7)
          CALL SWAP_ENDS16(JGETPKT(9),3)
          IF(JGETPKT(9).EQ.0) THEN
            CLINE='No serial ports initialised'
          ELSE
            CLINE='Serial ports                  initialised'
            J=14
            DO I=0,7
              IF(BTEST(JGETPKT(9),I)) THEN
                WRITE(CLINE(J:J+1),'(I1,A)',IOSTAT=IOS) I,','
                J=J+2
              END IF
            END DO
            CLINE(J-1:J-1)=' '
            CALL STR_COMPRESS(CLINE)
          END IF
          CALL SCREEN('LINE',CLINE,8)
          WRITE(CLINE,'(A,2(X,Z4.4))',IOSTAT=IOS) 'Diagnostic words ',
     &        JGETPKT(10),JGETPKT(11)
          CALL STR_COMPRESS(CLINE)
          CALL SCREEN('LINE',CLINE,9)
          CALL SWAP_ENDS16(JGETPKT(12),1)
          WRITE(CLINE,'(A,I5)',IOSTAT=IOS) 'DLU data byte count is ',
     &        JGETPKT(12)
          CALL STR_COMPRESS(CLINE)
          CALL SCREEN('LINE',CLINE,10)
          CALL SWAP_ENDS32(JGETPKT(13),2)
          JTMP(1)=JGETPKT(13)
          JTMP(2)=JGETPKT(14)
          CLINE='             seconds since DLU powered up'
          WRITE(CLINE(1:11),'(I11)',IOSTAT=IOS) ITMP
          CALL STR_COMPRESS(CLINE)
          CALL SCREEN('LINE',CLINE,11)
          JTMP(1)=JGETPKT(15)
          JTMP(2)=JGETPKT(16)
          WRITE(CLINE,'(A,I11)',IOSTAT=IOS) 'DLU time is ',ITMP
          CALL STR_COMPRESS(CLINE)
          CALL SCREEN('LINE',CLINE,12)
          IL=12
        END IF
      ELSE IF(ICYCLE.EQ.2) THEN        !Get configuration from DLU
        LPRINT=.FALSE.
        CALL SCREEN('PASS','DLU configuration from '//CNAM,1)
        CALL SCREEN('PASS',' ',0)
        IGETS=IGETCNT                  !Set up read of configuration packet
        IGETPKT=7
        CGETPKT=CSRC
        JPKT(1)=12
        CALL SWAP_ENDS16(JPKT(1),1)
        ISNDSIZ=2
        JSNDPKT(1)=JPKT(1)
        CSNDPKT=CSRC
        ISTAT=SYS$SETEF(%VAL(83))
        ICNT=0
        DO WHILE(IGETS.EQ.IGETCNT.AND.ICNT.LT.10)
          CALL LIB$WAIT(0.2)
          ICNT=ICNT+1
        END DO
        IF(ICNT.EQ.10) THEN
          CLINE='DLU not returning configuration message'
          CALL SCREEN('PASS',CLINE,0)
        ELSE
          CALL SWAP_ENDS16(JGETPKT(1),3)
          IPORTS=JGETPKT(2)
          IRSCONF=JGETPKT(3)
          WRITE(CLINE,'(3I6)',IOSTAT=IOS) (JGETPKT(I),I=1,3)
          CALL STR_COMPRESS(CLINE)
          CALL SCREEN('PASS',CLINE,0)
          CALL SWAP_ENDS16(JGETPKT(4),IPORTS*2)
          J=1
          DO I=1,IPORTS*2,2
            WRITE(CTEMP1,'(Z2.2,I4)',IOSTAT=IOS) JGETPKT(I+3),
     &          JGETPKT(I+4)
            CLINE(J:)=CTEMP1
            CALL STR_COMPRESS(CLINE)
            J=LAN(CLINE)+2
            IF(J.GT.75) THEN
              CALL SCREEN('PASS',CLINE,0)
              J=1
            END IF
          END DO
          IF(J.NE.1) CALL SCREEN('PASS',CLINE,0)
        END IF
      ELSE IF(ICYCLE.EQ.3) THEN        !Get latest data packet
        LPRINT=.FALSE.
        CALL SCREEN('PASS','Latest data packet from '//CNAM,1)
        CALL SCREEN('PASS',' ',0)
        IGETS=IGETCNT                  !Set up read of data packet
        IGETPKT=4
        CGETPKT=CSRC
        ICNT=0
        DO WHILE(IGETS.EQ.IGETCNT.AND.ICNT.LT.10)
          CALL LIB$WAIT(0.2)
          ICNT=ICNT+1
        END DO
        IF(ICNT.EQ.10) THEN
          CLINE='DLU not sending data packets'
          CALL SCREEN('PASS',CLINE,0)
          IL=3
        ELSE
          CALL SWAP_ENDS16(JGETPKT(1),2)
          CALL SWAP_ENDS32(JGETPKT(3),1)
          CALL SWAP_ENDS16(JGETPKT(5),4)
          CLINE='Message part number '
          WRITE(CLINE(25:),'(I10)',IOSTAT=IOS) JGETPKT(2)
          CALL STR_COMPRESS(CLINE)
          CALL SCREEN('PASS',CLINE,0)
          CLINE='DRS time '
          JTMP(1)=JGETPKT(3)
          JTMP(2)=JGETPKT(4)
          WRITE(CLINE(17:),'(I6,A)',IOSTAT=IOS) ITMP,'  secs'
          CALL STR_COMPRESS(CLINE)
          CALL SCREEN('PASS',CLINE,0)
          IF(JGETPKT(5).EQ.0) 
     &        CLINE='Receiving 1 Hz pulses and 1 Hz packets'
          IF(JGETPKT(5).EQ.1)
     &        CLINE='Not receiving 1 Hz pulses'
          IF(JGETPKT(5).EQ.2)
     &        CLINE='Not receiving 1 Hz packets'
          IF(JGETPKT(5).EQ.3)
     &        CLINE='Not receiving 1 Hz pulses nor 1 Hz packets'
          IF(JGETPKT(5).GT.3)
     &        CLINE='DLU clock not initialised'
          CALL STR_COMPRESS(CLINE)
          CALL SCREEN('PASS',CLINE,0)
          CLINE='Slippage is            1/2048s units'
          WRITE(CLINE(13:18),'(I6)',IOSTAT=IOS) JGETPKT(6)
          CALL STR_COMPRESS(CLINE)
          CALL SCREEN('PASS',CLINE,0)
          CLINE='Message length is 8 +             words '
          WRITE(CLINE(24:27),'(I4)',IOSTAT=IOS) IGETSIZ-8
          CALL STR_COMPRESS(CLINE)
          CALL SCREEN('PASS',CLINE,0)
          WRITE(CLINE,'(A,2(X,Z4.4))',IOSTAT=IOS) 'Diagnostic words ',
     &        (JGETPKT(I),I=7,8)
          CALL STR_COMPRESS(CLINE)
          CALL SCREEN('PASS',CLINE,0)
          CALL SCREEN('PASS',' ',0)
          CALL SWAP_ENDS16(JGETPKT(9),IGETSIZ-8)
          J=1
          DO I=9,IGETSIZ
            I8=JZEXT(JGETPKT(I))
            WRITE(CTEMP1,'(I6)',IOSTAT=IOS) I8
            CLINE(J:)=CTEMP1
            CALL STR_COMPRESS(CLINE)
            J=LAN(CLINE)+2
            IF(J.GT.75) THEN
              CALL SCREEN('PASS',CLINE,0)
              J=1
            END IF
          END DO
          IF(J.NE.1) CALL SCREEN('PASS',CLINE,0)
        END IF
      ELSE IF(ICYCLE.EQ.4) THEN        !Get latest serial packet
        LPRINT=.FALSE.
        CALL SCREEN('PASS','Latest serial packets from '//CNAM,1)
        IRGET=-1
12      CALL SCREEN('PASS',' ',0)
        IGETS=IGETCNT                  !Set up read of serial packet
        IGETPKT=5
        CGETPKT=CSRC
        IF(IRGET.EQ.-1) THEN
          IGETPRT=0                    !Any port to start
        ELSE
          DO I=0,7
            IF(BTEST(IRGET,I)) THEN    !Specify a remaining port
              IGETPRT=130+2*I
              IF(IGETPRT.GT.136) IGETPRT=IGETPRT+8
            END IF
          END DO
        END IF
        ICNT=0
        DO WHILE(IGETS.EQ.IGETCNT.AND.ICNT.LT.10)
          CALL LIB$WAIT(0.2)
          ICNT=ICNT+1
        END DO
        IF(ICNT.EQ.10) THEN
          CLINE='DLU not sending serial packets'
          CALL SCREEN('PASS',CLINE,0)
          IL=3
          IRGET=0
        ELSE
          CALL SWAP_ENDS16(JGETPKT(1),1)
          CALL SWAP_ENDS32(JGETPKT(2),1)
          CALL SWAP_ENDS16(JGETPKT(4),5)
          IF(JGETPKT(6).EQ.0) THEN
            CLINE='No serial ports initialised'
            IRGET=0
          ELSE
            CLINE='Serial ports                  initialised'
            J=14
            IF(IRGET.EQ.-1) IRGET=JGETPKT(6)
            DO I=0,7
              IF(BTEST(JGETPKT(6),I)) THEN
                WRITE(CLINE(J:J+1),'(I1,A)',IOSTAT=IOS) I,','
                J=J+2
              END IF
            END DO
            CLINE(J-1:J-1)=' '
            CALL STR_COMPRESS(CLINE)
          END IF
          CALL SCREEN('PASS',CLINE,0)
          CLINE='DRS time is '
          JTMP(1)=JGETPKT(2)
          JTMP(2)=JGETPKT(3)
          WRITE(CLINE(20:),'(I6,A)',IOSTAT=IOS) ITMP,'  secs'
          CALL STR_COMPRESS(CLINE)
          CALL SCREEN('PASS',CLINE,0)
          IF(JGETPKT(4).EQ.0) 
     &        CLINE='Receiving 1 Hz pulses and 1 Hz packets'
          IF(JGETPKT(4).EQ.1)
     &        CLINE='Not receiving 1 Hz pulses'
          IF(JGETPKT(4).EQ.2)
     &        CLINE='Not receiving 1 Hz packets'
          IF(JGETPKT(4).EQ.3)
     &        CLINE='Not receiving 1 Hz pulses nor 1 Hz packets'
          IF(JGETPKT(4).GT.3)
     &        CLINE='DLU clock not initialised'
          CALL STR_COMPRESS(CLINE)
          CALL SCREEN('PASS',CLINE,0)
          CLINE='Slippage is            1/2048s units'
          WRITE(CLINE(13:18),'(I6)',IOSTAT=IOS) JGETPKT(5)
          CALL STR_COMPRESS(CLINE)
          CALL SCREEN('PASS',CLINE,0)
          CLINE='Message length is 8 words +             bytes '
          WRITE(CLINE(30:33),'(I4)',IOSTAT=IOS) JGETPKT(8)
          CALL STR_COMPRESS(CLINE)
          CALL SCREEN('PASS',CLINE,0)
          IPORT=JGETPKT(7)/2-64
          IF(IPORT.GT.3) IPORT=IPORT-4
          DO I=1,8
            IF(BTEST(IPORT,I-1)) THEN
              IRGET=IBCLR(IRGET,I-1)   !Show got this port
              JJ=1
              CALL SCREEN('PASS',' ',0)
              WRITE(CLINE,'(A,I1,A,I3,A)',IOSTAT=IOS)'Serial port ',I-1,
     &            ' - ',JGETPKT(8),' bytes'
              CALL STR_COMPRESS(CLINE)
              CALL SCREEN('PASS',CLINE,0)
              CLINE=' '
              DO K=1,JGETPKT(8)
                WRITE(CTEMP,'(Z2.2)',IOSTAT=IOS) BGETPKT(K+16)
                CLINE(JJ:JJ+1)=CTEMP(1:2)
                JJ=JJ+3
                CALL STR_COMPRESS(CLINE)
                IF(JJ.GT.76) THEN
                  CALL SCREEN('PASS',CLINE,0)
                  JJ=1
                  CLINE=' '
                END IF
              END DO
              IF(JJ.NE.1) CALL SCREEN('PASS',CLINE,0)
            END IF
          END DO
        END IF
        IF(IRGET.NE.0) GOTO 12
      ELSE IF(ICYCLE.EQ.5) THEN        !Show DLU port usage
        LPRINT=.FALSE.
        CALL SCREEN('PASS',CNAM//' port usage',1)
        CALL SCREEN('PASS',' ',2)
        CLINE='    DLU   Card  Card type  Port no  Port addr'//
     &    '  Para no   Para freq   Para name'
        CALL SCREEN('PASS',CLINE,3)
        CALL SCREEN('PASS',' ',4)
        IL=4
        OPEN(11,FILE='HORACE:DRS_CONFIG.DAT',STATUS='OLD',READONLY)
        READ(11,'(A)',IOSTAT=IOS) CLINE
        DO WHILE(IOS.EQ.0)
          IF(CLINE(1:6).EQ.CNAM) THEN
            IL=IL+1
            CALL SCREEN('PASS',CLINE,IL)
          END IF
          READ(11,'(A)',IOSTAT=IOS) CLINE
        END DO
        CLOSE(11,IOSTAT=IOS)       
      ELSE IF(ICYCLE.EQ.6) THEN        !Display data by parameter
        LPRINT=.FALSE.
        CALL SCREEN('PASS','Latest data from '//CNAM,1)
        IGETS=IGETCNT                  !Set up read of data packet
        IGETPKT=4
        CGETPKT=CSRC
        ICNT=0
        DO WHILE(IGETS.EQ.IGETCNT.AND.ICNT.LT.10)
          CALL LIB$WAIT(0.2)
          ICNT=ICNT+1
        END DO
        IF(ICNT.EQ.10) THEN
          CLINE='DLU not sending data packets'
          CALL SCREEN('PASS',CLINE,0)
        ELSE
          CALL SWAP_ENDS16(JGETPKT(7),IGETSIZ-6)
          CALL DRS_PARAMETERS_READ(IFRQ,CPNAM)
          IPARAS=0
          OPEN(11,FILE='HORACE:DRS_CONFIG.DAT',STATUS='OLD',READONLY)
          READ(11,'(A)',IOSTAT=IOS) CLINE
          DO WHILE(IOS.EQ.0)
            IF(CLINE(1:6).EQ.CNAM) THEN
              CALL STR_COMPRESS(CLINE)
              CALL STR$ELEMENT(CTEMP,2,' ',CLINE)
              IF(CTEMP(1:1).NE.'S') THEN
                CALL STR$ELEMENT(CTEMP,5,' ',CLINE)
                READ(CTEMP,'(BN,I)') I
                IGNOR=0
                DO IB=1,8
                  IF(I.EQ.IBBRZER(IB)) IGNOR=1
                END DO
                IF(IGNOR.EQ.0) THEN
                  IPARAS=IPARAS+1
                  IPARA(IPARAS)=I
                  CPNAME(IPARAS)=CPNAM(I)
                  CALL STR$ELEMENT(CTEMP,6,' ',CLINE)
                  READ(CTEMP,'(BN,I)') IFREQ(IPARAS)
                  DO IB=1,8
                    IF(I.EQ.IBBRSIG(IB)) IFREQ(IPARAS)=IBBRFRQ
                  END DO
                END IF
              END IF
            END IF
            READ(11,'(A)',IOSTAT=IOS) CLINE
          END DO
          CLOSE(11,IOSTAT=IOS)       
          K=8
          DO I=1,IPARAS
            CALL SCREEN('PASS',' ',0)
            WRITE(CLINE,'(A,I4,A,I4,A)',IOSTAT=IOS)
     &          'Parameter ',IPARA(I),' '//CPNAME(I)//' at ',
     &          IFREQ(I),' Hz'
            CALL STR_COMPRESS(CLINE)
            CALL SCREEN('PASS',CLINE,0)
            J=1
            DO JJ=1,IFREQ(I)
              K=K+1
              I8=JZEXT(JGETPKT(K))
              WRITE(CTEMP1,'(I6)',IOSTAT=IOS) I8
              CLINE(J:)=CTEMP1
              CALL STR_COMPRESS(CLINE)
              J=LAN(CLINE)+2
              IF(J.GT.75) THEN
                CALL SCREEN('PASS',CLINE,0)
                J=1
              END IF
            END DO
            IF(J.NE.1) CALL SCREEN('PASS',CLINE,0)
          END DO
        END IF
      ELSE IF(ICYCLE.EQ.7) THEN        !Display serial data by parameter
        LPRINT=.FALSE.
        CALL SCREEN('PASS','Latest serial data from '//CNAM,1)
        IRGET=-1
13      IF(IRGET.EQ.-1) THEN
          IGETPRT=0                    !Any port to start
        ELSE
          DO I=0,7
            IF(BTEST(IRGET,I)) THEN    !Specify a remaining port
              IGETPRT=130+2*I
              IF(IGETPRT.GT.136) IGETPRT=IGETPRT+8
            END IF
          END DO
        END IF
        IGETS=IGETCNT                  !Set up read of data packet
        IGETPKT=5
        CGETPKT=CSRC
        ICNT=0
        DO WHILE(IGETS.EQ.IGETCNT.AND.ICNT.LT.10)
          CALL LIB$WAIT(0.2)
          ICNT=ICNT+1
        END DO
        IF(ICNT.EQ.10) THEN
          CALL SCREEN('PASS',' ',0)
          CLINE='DLU not sending serial packets'
          CALL SCREEN('PASS',CLINE,0)
          IRGET=0
        ELSE
          CALL SWAP_ENDS16(JGETPKT(7),1) !Port address
          IPORT=JGETPKT(7)/2-64
          IF(IPORT.GT.3) IPORT=IPORT-4
          CALL SWAP_ENDS16(JGETPKT(6),1)
          IF(IRGET.EQ.-1) IRGET=JGETPKT(6)
          DO I=1,8
            IF(BTEST(IPORT,I-1)) IRGET=IBCLR(IRGET,I-1)
          END DO
          CALL DRS_PARAMETERS_READ(IFRQ,CPNAM)
          IPARAS=0
          OPEN(11,FILE='HORACE:DRS_CONFIG.DAT',STATUS='OLD',READONLY)
          READ(11,'(A)',IOSTAT=IOS) CLINE
          DO WHILE(IOS.EQ.0)
            IF(CLINE(1:6).EQ.CNAM) THEN
              CALL STR_COMPRESS(CLINE)
              CALL STR$ELEMENT(CTEMP,2,' ',CLINE)
              CALL STR$ELEMENT(CTEMP1,4,' ',CLINE)
              READ(CTEMP1,'(BN,Z)') IPORT
              IF(CTEMP(1:1).EQ.'S'.AND.IPORT.EQ.JGETPKT(7)) THEN
                CALL STR$ELEMENT(CTEMP,5,' ',CLINE) !Para no
                READ(CTEMP,'(BN,I)') I
              END IF
            END IF
            READ(11,'(A)',IOSTAT=IOS) CLINE
          END DO
          CLOSE(11,IOSTAT=IOS)       

          CALL SCREEN('PASS',' ',0)
          WRITE(CLINE,'(A,I4,A,I4,A)',IOSTAT=IOS)
     &        'Parameter ',I,' '//CPNAM(I)//' at ',IFRQ(I),' Hz'
          CALL STR_COMPRESS(CLINE)
          CALL SCREEN('PASS',CLINE,0)
          JJ=1
          DO K=9,9+IFRQ(I)-1
            WRITE(CTEMP1,'(Z6.4)',IOSTAT=IOS) JGETPKT(K)
            CLINE(JJ:)=CTEMP1
            CALL STR_COMPRESS(CLINE)
            JJ=LAN(CLINE)+2
            IF(JJ.GT.75) THEN
              CALL SCREEN('PASS',CLINE,0)
              JJ=1
            END IF
          END DO
          IF(JJ.NE.1) CALL SCREEN('PASS',CLINE,0)
        END IF
        IF(IRGET.NE.0) GOTO 13
      END IF
!
      IF(LPRINT) THEN
        CALL SCREEN('LINE',' ',IL+1)
        CALL SCREEN('LINE','B - Back, N - Next, R - Repeat, P - Print,'
     &      //' E - Exit',IL+2)
        CALL SCREEN('UPDATE','DUMMY',IL)
      ELSE
        CALL SCREEN('PASS',' ',0)
        CALL SCREEN('PASS','B - Back, N - Next, R - Repeat, E - Exit',
     &      0)
      END IF 
!
      DO WHILE(INPUT.EQ.0)
        CALL LIB$WAIT(0.2)
      END DO
      IF(INPUT.NE.0) THEN
        IF((INPUT.EQ.80.OR.INPUT.EQ.112).AND.LPRINT) THEN !P or p
          CALL SCREEN('PRINT','DUMMY',ICNT)
          INPUT=0
        ELSE IF(INPUT.EQ.78.OR.INPUT.EQ.110) THEN !N or n
          INPUT=0
          ICYCLE=ICYCLE+1
          IF(ICYCLE.GT.7) ICYCLE=1
          CALL SCREEN('CLEAR','DUMMY',ICNT)
        ELSE IF(INPUT.EQ.66.OR.INPUT.EQ.98) THEN !B or b
          INPUT=0
          ICYCLE=ICYCLE-1
          IF(ICYCLE.LT.1) ICYCLE=7
          CALL SCREEN('CLEAR','DUMMY',ICNT)
        ELSE IF(INPUT.EQ.69.OR.INPUT.EQ.101) THEN !E or e
          INPUT=0
          CALL SCREEN('CLEAR','DUMMY',ICNT)
          RETURN
        ELSE
          INPUT=0
          CALL SCREEN('CLEAR','DUMMY',ICNT)
        END IF
      END IF
      GOTO 11
      END
!*******************************************************************************
      SUBROUTINE DRS_DATA
!
! Note that the internal SCREEN store mechanism is not used for some of the
! options in this routine since they may produce more than 24 lines of output.
! Instead the output is written directly to the screen, but there is then no
! print option.
!
      INTEGER*4 I,J,IOS,IOSB(2),ICHAN,INPUT,ICNT,IL,LAN,
     &    ICYCLE,K,IFRQ(512),IFNO,ISEC,IBLK,I8
      LOGICAL*4 LPRINT
      INTEGER*2 IPLOC(256),IPFREQ(256),IPLIST(256)
      CHARACTER CLINE*80,CFDATE*9,CTEMP1*6,CISS*3,CFILE*10,CPNAM(512)*4
      INCLUDE 'DRS_GDEF.FOR'
      INCLUDE '($SYSSRVNAM)'
      COMMON   INPUT,IOSB,ICHAN

      CALL INPUT_AST
      INPUT=0
      ICYCLE=1
      CALL SCREEN('CLEAR','DUMMY',ICNT)
11    LPRINT=.FALSE.
      IF(ICYCLE.EQ.1) THEN             !Display ISS
        WRITE(CISS,'(I3.3)',IOSTAT=IOS) IISSNUM
        CFILE='ISS'//CISS//'.DAT'
        CLINE='Contents of '//CFILE
        CALL SCREEN('PASS',CLINE,1)
        CALL SCREEN('PASS',' ',0)
        K=1
        J=1
        I=0
        DO WHILE(I.NE.4095)
          WRITE(CTEMP1,'(I5)',IOSTAT=IOS) JISSBLK(K)
          CLINE(J:)=CTEMP1
          J=J+5
          IF(J.GT.76) THEN
            CALL SCREEN('PASS',CLINE,0)
            J=1
          END IF
          I=JISSBLK(K)
          K=K+1
        END DO
        IF(J.NE.1) CALL SCREEN('PASS',CLINE,0)
      ELSE IF(ICYCLE.EQ.2) THEN        !Display raw DRS data block in decimal
        CALL SCREEN('PASS','Latest DRS data block in decimal',1)
        CALL SCREEN('PASS',' ',0)
        J=1
        DO K=1,IISSSIZ
          I8=JZEXT(JDATBLK(K))
          WRITE(CTEMP1,'(I6)',IOSTAT=IOS) I8
          CLINE(J:)=CTEMP1
          J=J+6
          IF(J.GT.75) THEN
            CALL SCREEN('PASS',CLINE,0)
            J=1
          END IF
        END DO
        IF(J.NE.1) CALL SCREEN('PASS',CLINE,0)
      ELSE IF(ICYCLE.EQ.3) THEN        !Display raw DRS data block in hex
        CALL SCREEN('PASS','Latest DRS data block in hex',1)
        CALL SCREEN('PASS',' ',0)
        J=1
        DO K=1,IISSSIZ
          WRITE(CTEMP1,'(Z5.4)',IOSTAT=IOS) JDATBLK(K)
          CLINE(J:)=CTEMP1
          J=J+5
          IF(J.GT.76) THEN
            CALL SCREEN('PASS',CLINE,0)
            J=1
          END IF
        END DO
        IF(J.NE.1) CALL SCREEN('PASS',CLINE,0)
      ELSE IF(ICYCLE.EQ.4.OR.ICYCLE.EQ.5) THEN !Get parameter data in DRS block
        IF(ICYCLE.EQ.4) 
     &      CALL SCREEN('PASS','Latest DRS data block in decimal',1)
        IF(ICYCLE.EQ.5) 
     &      CALL SCREEN('PASS','Latest DRS data block in hex',1)
        CALL SCREEN('PASS',' ',0)
        CALL DRS_PARAMETERS_READ(IFRQ,CPNAM)
        WRITE(CISS,'(I2.2)',IOSTAT=IOS) IISSNUM
        CALL READ_ISS(JISSBLK,IPLIST,IPLOC,IPFREQ)
        CALL DRSDAT(JDATBLK,CFDATE)
        CALL BLKSEC(JDATBLK,IFNO,IBLK,ISEC)
        CLINE='ISS: xx, DBF: xx, IC: xx, Block: nnnnn, '//
     &      'Date: dd-mmm-yy, Tim: dd:hh:mm, Flt: xxx'
        WRITE(CLINE(6:7),'(I2)') JDATBLK(1).AND.'FFF'X
        WRITE(CLINE(15:16),'(I2)') JDATBLK(2)
        WRITE(CLINE(23:24),'(I2)') JDATBLK(3)
        WRITE(CLINE(34:38),'(I5)') IBLK
        CLINE(47:55)=CFDATE
        CALL SPMCTIM(ISEC,CLINE(63:70))
        WRITE(CLINE(78:80),'(I3.3)') IFNO
        CALL STR_COMPRESS(CLINE)
        CALL SCREEN('PASS',CLINE,0)

        DO I=1,256
          IF(IPLIST(I).NE.0) THEN
            CALL SCREEN('PASS',' ',0)
            WRITE(CLINE,'(A,I5,A,I5,A)') 'Parameter ',IPLIST(I),
     &          ' '//CPNAM(IPLIST(I))//' ',IPFREQ(I),' Hz'
            CALL STR_COMPRESS(CLINE)
            CALL SCREEN('PASS',CLINE,0)
            J=1
            DO K=IPLOC(I),IPLOC(I)+IPFREQ(I)-1
              IF(ICYCLE.EQ.4) THEN
                I8=JZEXT(JDATBLK(K))
                WRITE(CTEMP1,'(I6)',IOSTAT=IOS) I8
                CLINE(J:)=CTEMP1
                CALL STR_COMPRESS(CLINE)
                J=LAN(CLINE)+2
                IF(J.GT.75) THEN
                  CALL SCREEN('PASS',CLINE,0)
                  J=1
                END IF
              ELSE
                WRITE(CTEMP1,'(Z5.4)',IOSTAT=IOS) JDATBLK(K)
                CLINE(J:)=CTEMP1
                CALL STR_COMPRESS(CLINE)
                J=LAN(CLINE)+2
                IF(J.GT.76) THEN
                  CALL SCREEN('PASS',CLINE,0)
                  J=1
                END IF
              END IF
            END DO
            IF(J.NE.1) CALL SCREEN('PASS',CLINE,0)
          END IF
        END DO          
      END IF
!
      IF(LPRINT) THEN
        CALL SCREEN('LINE',' ',IL+1)
        CALL SCREEN('LINE','B - Back, N - Next, R - Repeat, P - Print,'
     &      //' E - Exit',IL+2)
        CALL SCREEN('UPDATE','DUMMY',IL)
      ELSE
        CALL SCREEN('PASS',' ',0)
        CALL SCREEN('PASS','B - Back, N - Next, R - Repeat, E - Exit',
     &      0)
      END IF 
!
      DO WHILE(INPUT.EQ.0)
        CALL LIB$WAIT(0.2)
      END DO
      IF(INPUT.NE.0) THEN
        IF((INPUT.EQ.80.OR.INPUT.EQ.112).AND.LPRINT) THEN !P or p
          CALL SCREEN('PRINT','DUMMY',ICNT)
          INPUT=0
        ELSE IF(INPUT.EQ.78.OR.INPUT.EQ.110) THEN !N or n
          INPUT=0
          ICYCLE=ICYCLE+1
          IF(ICYCLE.GT.5) ICYCLE=1
          CALL SCREEN('CLEAR','DUMMY',ICNT)
        ELSE IF(INPUT.EQ.66.OR.INPUT.EQ.98) THEN !B or b
          INPUT=0
          ICYCLE=ICYCLE-1
          IF(ICYCLE.LT.1) ICYCLE=5
          CALL SCREEN('CLEAR','DUMMY',ICNT)
        ELSE IF(INPUT.EQ.69.OR.INPUT.EQ.101) THEN !E or e
          INPUT=0
          CALL SCREEN('CLEAR','DUMMY',ICNT)
          CALL INPUT_ABORT
          RETURN
        ELSE
          INPUT=0
          CALL SCREEN('CLEAR','DUMMY',ICNT)
        END IF
      END IF
      GOTO 11
      END
C******************************************************************************
      SUBROUTINE READ_ISS(IDATA,IPLIST,IPLOC,IPFREQ)
      IMPLICIT   NONE
      INTEGER*4  J,K,JJ
      INTEGER*2  IPLIST(256),IPLOC(256),IPFREQ(256),IDATA(768)
C
C Read the ISS data and fill parameter list, parameter offsets, and parameter
C frequencies.
C
      DO J=1,256                                   !Clear arrays
        IPLIST(J)=0
        IPLOC(J)=0
        IPFREQ(J)=0
      END DO
      K=0
      DO J=2,512+1                                 !For each possible parameter
        IF(IDATA(J).EQ.'FFF'X) RETURN              !Return if end of data
        IF(IDATA(J).NE.0.AND.IDATA(J+1).NE.'FFF'X) THEN !If parameter recorded
          K=K+1
          IF(K.GT.256) THEN
            WRITE(6,*) 'More than 256 parameters.  Aborting'
            CALL EXIT
          END IF
          IPLIST(K)=J-1
          IPLOC(K)=IDATA(J)/2+1                    !Turn byte offset to words
          JJ=J
          DO WHILE(IDATA(JJ+1).EQ.0)               !Search for next offset
            JJ=JJ+1
          END DO
          IPFREQ(K)=(IDATA(JJ+1)-IDATA(J))/2       !Calc sampling rate
        END IF
      END DO
      RETURN
      END
C******************************************************************************
      SUBROUTINE DRSDAT(IDATA,CFDATE)
CDEC$ IDENT 'V1.00'
C
C Takes the flight date from a block of DRS data and returns it as a string
C of the form  dd-mmm-yy.
C
C Input:  IDATA  I*2  2048 array containing a block of raw DRS data
C Output: CFDATE C*9  The returned date string
C
C The flight date is held in words 5 and 6 of each DRS block as follows:
C
C Word 5 bits  0 to  3  Month units
C Word 5 bits  4 to  7  Day tens
C Word 5 bits  8 to 11  Day units
C Word 6 bits  0 to  3  Year tens
C Word 6 bits  4 to  7  Year units
C Word 6 bits  8 to 11  Month tens
C
      IMPLICIT    NONE
      INTEGER*4   IMINDX
      CHARACTER   CFDATE*9,CMONTHS*36
      INTEGER*2   IDATA(2048),J1,J2,JDAY,JMON,JYEAR
      DATA CMONTHS /'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'/
 
      CFDATE='dd-mmm-yy'
      J1=JZEXT(IDATA(5))
      J2=JZEXT(IDATA(6))
      JDAY=IBITS(J1,8,4)*10+IBITS(J1,4,4)
      JMON=IBITS(J1,0,4)*10+IBITS(J2,8,4)
      JYEAR=IBITS(J2,4,4)*10+IBITS(J2,0,4)
      WRITE(CFDATE(1:2),10) JDAY
      IMINDX=(JMON-1)*3 + 1
      IF(IMINDX.GE.1.AND.IMINDX.LE.34) 
     &    CFDATE(4:6)=CMONTHS(IMINDX:IMINDX+2)
      WRITE(CFDATE(8:9),10) JYEAR
      RETURN
10    FORMAT(I2.2)
      END
C******************************************************************************
      SUBROUTINE BLKSEC(IDATA,IFNO,IBLK,ISEC)
CDEC$ IDENT 'V1.01'
C
C Given a 1s block of raw DRS data, this subroutine returns the DRS block
C number and the DRS time as seconds past midnight.  DRS block numbers start
C at 10 and count upwards so long as the DRS is recording.  This routine 
C returns seconds past midnight on the day the flight started and for cross
C midnight flights ISEC will exceed 24*60*60. 
C
C Input:  IDATA  I*2  2048 array containing a block of raw DRS data
C         IFNO   I*4  Required flight number for valid block
C Output: IBLK   I*4  DRS block number returned
C         ISEC   I*4  DRS time converted to seconds past midnight
C
C The DRS block number is held in words 7 and 8 of each DRS block as follows:
C
C Word 7 bits  0 to 11  DRS block number highest 12 bits
C Word 8 bits  0 to 11  DRS block number lowest 12 bits
C
C The DRS time is held in words 10 and 11 of each DRS block as follows:
C
C Word 10 bits  0 to  3  Minute units
C Word 10 bits  4 to  7  Hour tens
C Word 10 bits  8 to 11  Hour units
C Word 11 bits  0 to  3  Second tens
C Word 11 bits  4 to  7  Second units
C Word 11 bits  8 to 11  Minute tens
C
      IMPLICIT    NONE
      INTEGER*4   ISEC,IFNO,IBLK
      INTEGER*2   IDATA(2048),J1,J2,JHRS,JMIN,JSEC
C
!      J1=JZEXT(IDATA(9))
      J1=IDATA(9)
      IFNO=IBITS(J1,0,4)+IBITS(J1,4,4)*10+IBITS(J1,8,4)*100
      IBLK=4096*JZEXT(IDATA(7))+JZEXT(IDATA(8))
      IF(IBLK.LT.0.OR.IBLK.GT.86400) RETURN !Check for reasonable block no
!      J1=JZEXT(IDATA(10))
!      J2=JZEXT(IDATA(11))
      J1=IDATA(10)
      J2=IDATA(11)
      JHRS=IBITS(J1,8,4)*10+IBITS(J1,4,4)
      JMIN=IBITS(J1,0,4)*10+IBITS(J2,8,4)
      JSEC=IBITS(J2,4,4)*10+IBITS(J2,0,4)
      IF(JHRS.LT.0.OR.JHRS.GT.23.OR.JMIN.LT.0.OR.JMIN.GT.59.OR.
     -    JSEC.LT.0.OR.JSEC.GT.59) RETURN !Check for reasonable time
      ISEC=JHRS*3600+JMIN*60+JSEC
      RETURN
      END
C*******************************************************************************
C
C ROUTINE          SPMCTIM SUBROUTINE FORTVAX
C
C PURPOSE          Converts time as seconds past midnight to hh:mm:ss string
C
C DESCRIPTION      Takes the time in seconds past midnight, reduces it to
C                  less that a day if necessary and converts it to a string
C                  in the form hh:mm:ss
C
C VERSION          1.00  10-1-90  W.D.N.JACKSON
C
C ARGUMENTS        ISECS    I*4  IN   Time in seconds past midnight
C                  CTIM     C*8  OUT  Character time as hh:mm:ss
C
C*******************************************************************************
      SUBROUTINE SPMCTIM(ISECS,CTIM)
      IMPLICIT NONE
      INTEGER*4 ISECS             !Time in secs past midnight
      CHARACTER CTIM*(*)          !Character string to hold time
      INTEGER*4 ITIM,IH,IM,IS,IOS

      ITIM=MOD(ISECS,24*60*60)    !Reduce to less than a day
      IH=ITIM/3600                !Get hours
      IM=MOD(ITIM/60,60)          !Get minutes
      IS=ITIM-3600*IH-60*IM       !Get secs
      CTIM='xx:xx:xx'             !Blank time string
      WRITE(CTIM(1:2),10,IOSTAT=IOS) IH !Fill string
      WRITE(CTIM(4:5),10,IOSTAT=IOS) IM
      WRITE(CTIM(7:8),10,IOSTAT=IOS) IS
      RETURN
10    FORMAT(I2.2)
      END
!*******************************************************************************
      SUBROUTINE DLU_TIME_STATUS
!
! Displays the time slip of the AMTG and each DLU.
!
      INTEGER*4 I,J,IOS,IOSB(2),ICHAN,INPUT,ICNT,IL,ISTAT,
     &    IGETS,ITMP,IUPTIM,ISRTPKT
      INTEGER*2 JPKT(750),JTMP(2),JTIMEP(11)
      LOGICAL*4 LGPS,LINIT
      REAL*4    RDSLIP,RSLIPR,RASLIP,RTSLIP,RASLIPR
      CHARACTER CSOURCE(32)*6,CNAMES(32)*6,CLINE*80,CNAM*6
      INCLUDE 'DRS_GDEF.FOR'
      INCLUDE '($SYSSRVNAM)'
      EQUIVALENCE (JTMP,ITMP)
      COMMON   INPUT,IOSB,ICHAN
!
      CALL GET_NAMES(CSOURCE,CNAMES)
      CALL INPUT_AST
      INPUT=0
      CALL SCREEN('CLEAR','DUMMY',ICNT)
!
11    CALL SCREEN('LINE','DLU time status',1)
      CALL SCREEN('LINE',' ',2)
      WRITE(CLINE,'(A,T16,A,T32,A,T48,A)',IOSTAT=IOS) 
     &    'DLU','Slip wrt AMTG','Slip wrt UTC',
     &    'Slip rate wrt UTC'
      CALL SCREEN('LINE',CLINE,3)
      WRITE(CLINE,'(T16,A,T32,A,T48,A)',IOSTAT=IOS) 
     &    '(ms)','(ms)','(ms/h)            +ve is fast'
      CALL SCREEN('LINE',CLINE,4)
      CALL SCREEN('LINE',' ',5)
!
! First do the AMTG
!
      ICNT=0
      ISRTPKT=ITIMPKT
      DO WHILE(ISRTPKT.EQ.ITIMPKT.AND.ICNT.LT.8)
        CALL LIB$WAIT(0.2)
        ICNT=ICNT+1
      END DO
      IF(ICNT.EQ.8) THEN
        CLINE='No AMTG packets being received'
      ELSE          
        DO J=1,11
          JTIMEP(J)=JTIMPKT(J)
        END DO
        CALL SWAP_ENDS16(JTIMEP(1),2)
        CALL SWAP_ENDS32(JTIMEP(3),1)
        CALL SWAP_ENDS16(JTIMEP(5),5)
        CALL SWAP_ENDS32(JTIMEP(10),1)
        CNAM=' '
        DO J=1,32
          IF(CSOURCE(J).EQ.CTIMPKT) CNAM=CNAMES(J)
        END DO
        CLINE(1:6)=CNAM
        LINIT=.FALSE.
        IF(JTIMEP(2).GT.0) LINIT=.TRUE.
        LGPS=.FALSE.
        IF(JTIMEP(8).EQ.1) LGPS=.TRUE.
        JTMP(1)=JTIMEP(10)
        JTMP(2)=JTIMEP(11)
        IUPTIM=ITMP
        IF(.NOT.LINIT.OR..NOT.LGPS) THEN
          RASLIP=0
        ELSE
          RASLIP=JTIMEP(9)*1000./2048. !ms (-ve is AMTG slow)'
        END IF
        IF(IUPTIM.EQ.0.OR..NOT.LGPS.OR..NOT.LINIT) THEN
          RASLIPR=0
        ELSE
          RASLIPR=RASLIP*3600./IUPTIM     !Slip rate ms/h
        END IF
        WRITE(CLINE,'(A,T16,F7.1,T32,F7.1,T48,F7.1)',IOSTAT=IOS) CNAM,
     &      0.,RASLIP,RASLIPR
      END IF
      CALL SCREEN('LINE',CLINE,6)
      IL=6
!
! Now do each DLU
!
      DO I=1,IDLUCNT
        CNAM=CDLUNAM(I)
        IF(IDLUSTS(I).NE.5) THEN
          CLINE=CNAM
        ELSE       
          RDSLIP=IDLUSLP(I)*1000./2048.
          IGETS=IGETCNT                !Set up read of status packet
          IGETPKT=6
          CGETPKT=CDLUADD(I)
          JPKT(1)=11
          CALL SWAP_ENDS16(JPKT(1),1)
          ISNDSIZ=2
          JSNDPKT(1)=JPKT(1)
          CSNDPKT=CDLUADD(I)
          ISTAT=SYS$SETEF(%VAL(83))
          ICNT=0
          DO WHILE(IGETS.EQ.IGETCNT.AND.ICNT.LT.10)
            CALL LIB$WAIT(0.2)
            ICNT=ICNT+1
          END DO
          IF(ICNT.EQ.10) THEN
            CLINE=CNAM//' DLU not returning status message'
          ELSE
            JTMP(1)=JGETPKT(4)
            JTMP(2)=JGETPKT(5)
            CALL SWAP_ENDS32(ITMP,1)
            IF(ITMP.EQ.0) THEN
              CLINE=CNAM//' DLU time slip not valid'
            ELSE
              IF(LINIT.AND.LGPS) THEN
                RTSLIP=RDSLIP-RASLIP
                IUPTIM=ITMP
                IF(IUPTIM.EQ.0) THEN
                  RSLIPR=0.
                ELSE
                  RSLIPR=RTSLIP*3600./IUPTIM     !Slip rate ms/h
                END IF
                WRITE(CLINE,'(A,T16,F7.1,T32,F7.1,T48,F7.1)',IOSTAT=IOS)
     &              CNAM,RTSLIP,RDSLIP,RSLIPR+RASLIPR
              ELSE
                WRITE(CLINE,'(A)',IOSTAT=IOS) CNAM
              END IF
            END IF
          END IF
        END IF
        IL=IL+1
        CALL SCREEN('LINE',CLINE,IL)
      END DO
!
      CALL SCREEN('LINE',' ',IL+1)
      CALL SCREEN('LINE','R - Repeat, P - Print,'
     &    //' E - Exit',IL+2)
      CALL SCREEN('UPDATE','DUMMY',IL)
!
      DO WHILE(INPUT.EQ.0)
        CALL LIB$WAIT(0.2)
      END DO
      IF(INPUT.NE.0) THEN
        IF((INPUT.EQ.80.OR.INPUT.EQ.112)) THEN !P or p
          CALL SCREEN('PRINT','DUMMY',ICNT)
          INPUT=0
        ELSE IF(INPUT.EQ.69.OR.INPUT.EQ.101) THEN !E or e
          INPUT=0
          CALL SCREEN('CLEAR','DUMMY',ICNT)
          CALL INPUT_ABORT
          RETURN
        ELSE
          INPUT=0
          CALL SCREEN('CLEAR','DUMMY',ICNT)
        END IF
      END IF
      GOTO 11
      END
