      PROGRAM DRS
!
! Note that OPER privilege is required when running this program (to set the
! system clock).  If there is no data for a parameter it is returned with a
! value of 65535 (FFFF).
!
! Collects and processes all Ethernet packets sent to HORACE using protocol
! 33-33 and multicast address AB-00-04-00-00-00.  Also sends out packets
! passed to it through the DRS_COM global common.  Logical name ETH_PORT must be
! defined as the physical Ethernet interface.  Logical name HORACE must be
! defined as the directory with the DRS config files.
!
! Assumes:
!
!     32 valid packet types (1-32)
!     32 max DLUs/senders
!     64 max non-serial ports per DLU
!      8 max RS232 ports per DLU
!     2048 max words total per second to be recorded (incl block header)
!
! Functions:
!
! 1. Zeros counters if IZERCNT set to 1 and CEF 83 set.  Then clears IZERCNT.
!
! 2. Puts all AMTG time packets received (type 3) into JTIMPKT, puts the AMTG
!    Ethernet address into CTIMPKT, increments ITIMPKT, and sets IDLUSTS(33).
!
! 3. If a packet is received whose source address is that specified in CGETPKT
!    and its packet type is that specified in IGETPKT, then the packet is put
!    into JGETPKT, and IGETCNT is incremented.  Note that this is the only way
!    for an external program (DRS_CONTROL) to obtain a packet, and any packet
!    may be obtained but only one at a time.
!
! 4. If CSNDPKT is non blank and CEF 83 is set, then outputs the packet
!    in JSNDPKT to the address in CSNDPKT, using the length specified in
!    ISNDSIZ (bytes), increments the packet count for that address and type,
!    and clears CSNDPKT to show packet sent.
!
! 5. For each packet received, increments the counter for that source and 
!    packet type.
!
! Once initialisation has taken place the program is entirely event driven as
! follows:
!
! 1. Every second a watchdog runs which:
!    a. Checks that the DLUs are still sending packets
!    b. Checks the AMTG is sending packets
!    c. Updates system status in IDLUSTS
!
! 2. When event flag 83 sets executes commands sent by DRS_CONTROL:
!    a. Send a supplied packet to a DLU
!    b. Zero the program counters
!    c. Check the DRS program is responding
!    d. Set a new flight number
!    e. Change mode between record and hold
!    f. Increment the event mark
!
! 3. Respond to arriving ethernet packets by:
!    a. Check for transmission errors
!    b. Identify source and add to list
!    c. Increment packet counts
!    d. Pass packet to DRS_CONTROL via common if required
!    e. Process packet by type depending on current program status:
!
! Status                 pkt4       pkt5       pkt6              pkt7
!
! 0 Waiting for 1st pkt  Req config Req config Snd configs       Ignore
!                                              Req config
!
! 1 Waiting for config   Ignore     Ignore     Snd configs       Chk config
!                                              Req config        
!
! 2 Config check failed  Ignore     Ignore     Snd configs       Ignore
!                                              Req config
!
! 3 Configs OK, logging  Log        Log        Ignore if solic   Ignore
!                                              Snd configs
!                                              Req config
! Build with:
!
! $ FORT DRS
! $ LINK DRS,DRS_SUBS/LIB,DRS_COM/OPT,H_COMMON/OPT
!
      IMPLICIT  NONE
      INTEGER*4 ISTS
      CHARACTER CVAL*4
      INCLUDE   'DRS_LDEF.FOR'
      INCLUDE   'DRS_GDEF.FOR'

      IDIAG=0                          !1 to list each packet's contents, else 0
      CALL GET_LOGICAL('DRS_TESTMBX',CVAL)
      IF(CVAL.NE.' ') LMBX=.TRUE.      !Send packets to/from mailboxes for test
      CALL LOG_MESS('Program starting')
      IF(LMBX) CALL LOG_MESS('Program running in mailbox test mode')
      CALL LOG_MESS('Checking Ethernet addresses')
      CALL DRS_ADDRESSES_CHECK(ISTS)   !Confirm no errors in address database
      IF(ISTS.NE.1) THEN
        CALL LOG_MESS('Exiting due to error in DRS_ADDRESSES.DAT')
        CALL EXIT
      END IF
      CALL LOG_MESS('Checking configuration')
      CALL DRS_CONFIG_CHECK(ISTS)      !Confirm no errors in config database
      IF(ISTS.NE.1) THEN
        CALL LOG_MESS('Exiting due to error in DRS_CONFIG.DAT')
        CALL EXIT
      END IF
!
      CALL INIT                        !Initialize variables
      CALL DRS_ISS_BLOCK(JISSBLK,IISSNUM) !Create an ISS block
      CALL DRS_BLANK_BLOCK(IISSNUM,JEMPBLK)!Create an empty DRS block
      CALL DRS_INI_PACKETS(JISSBLK,JINIPKT,JLOOKUP,IDLUCNT,IINILEN,
     &    CDLUADD,IRSPORT,IRSLOOK,IRSFREQ,IISSSIZ,IPARCNT,
     &    CDLUNAM,IXMCNT,ICNFSIZ,ICNFCNT,IPARTS,IPRTLEN,JDRSTAT,
     &    IXMTOT)                      !Create init packets for DLUs
      IF(LMBX) THEN
        CALL MBX_CONN
      ELSE
        CALL ETH_CONN                  !Establish Ethernet input port
      END IF
      CALL WATCHDOG                    !Start the 1s watchdog
      CALL ETH_AST                     !Start first and continuing input reads
      CALL BLOCK_TIMER                 !Start block timing
      CALL WAIT_COMMANDS               !Wait for any output packets or DRS cmds
      END
!*******************************************************************************
      SUBROUTINE DRS_BLANK_BLOCK(ISS,JBLOCK)
!
! Builds a blank DRS which will be the template for all DRS blocks.  Note that
! the date is filled in the block using the HORACE date, which is always
! available but may not always match the AMTG initialization date.
!
! The following elements are filled: ISS,IC,DBF,Date,data block terminator.
! Unfilled areas are filled with FFFFs.
! The following to be filled each sec by the DRS program: block number, 
! parameters 1-5, and any other data supplied directly by HORACE processes.
!
      IMPLICIT  NONE
      INTEGER*4 ISS,I,IVALS(8),IY10,IY1,IM10,IM1,ID10,ID1
      INTEGER*2 JBLOCK(2048)
      CHARACTER CDATE*8,CTIME*10,CZONE*5

! Clear the block
      DO I=1,2048
        JBLOCK(I)='FFFF'X
      END DO
! Put in ISS with bit 13 set, DBF, and IC code
      JBLOCK(1)=ISS
      JBLOCK(1)=IBSET(JBLOCK(1),13)
      JBLOCK(2)=ISS
      JBLOCK(3)=2
      JBLOCK(4)=0
! Put in date from HORACE clock, in binary coded decimal (BCD)
      CALL DATE_AND_TIME(CDATE,CTIME,CZONE,IVALS)
      ID10=IVALS(3)/10
      ID1=MOD(IVALS(3),10)
      IM10=IVALS(2)/10
      IM1=MOD(IVALS(2),10)
      IVALS(1)=IVALS(1)-2000
      IY10=IVALS(1)/10
      IY1=MOD(IVALS(1),10)
      JBLOCK(5)=(ID10*256).OR.(ID1*16).OR.IM10
      JBLOCK(6)=(IM1*256).OR.(IY10*16).OR.IY1
! Use 0 for block number until filled in
      JBLOCK(7)=0
      JBLOCK(8)=0
! Mark end of data block
      JBLOCK(2048)=2047
      RETURN
      END
!*******************************************************************************
      SUBROUTINE DRS_INI_PACKETS(JISSBLK,JINIPKT,JLOOKUP,IDLU,IINILEN,
     &    CDLUADD,IRSPORT,IRSLOOK,IRSFREQ,IISSSIZ,IPARCNT,
     &    CDLUNAM,IXMCNT,ICNFSIZ,ICNFCNT,IPARTS,IPRTLEN,JDRSTAT,IXMTOT)
!
! Returns with initialization strings for each DLU in JINIPKT, and a lookup
! table in JLOOKUP for each data word returned from a DLU to where it goes in
! the DRS block.
!
! Input arguments - 
!
! JISSBLK - standard ISS block
!
! Output arguments - 
!
! JINIPKT - ISS ini packet for each DLU, first word byte swapped, but not rest
!           ISS definition will always be contained in one packet
! IINILEN - Length of init packet in words, incl first word
! IDLU    - Number of DLUs (init packets)
! JLOOKUP - Position in DRS data block to put each data word from DLU
! CDLUADD - 6 byte Ethernet addresses of each DLU
! IRSPORT - RS232 port definitions - up to 16 bits set in each word
! IRSFREQ - Frequency of each RS232 parameter
! IRSLOOK - Position in DRS block to put first sample of each RS232 parameter
! IISSSIZ - No of data words in ISS
! IPARCNT - No parameters in ISS
! CDLUNAM - Name of each DLU
! IXMCNT  - Expected packets per sec for each DLU
! IXMTOT  - Expected total packets per sec from all DLUs
! ICNFSIZ - No of configured data words
! ICNFCNT - No of configured parameters
! IPARTS  - No of packet type 4 parts
! IPRTLEN - Data quantity, in words, in each packet type 4 part
! JDRSTAT - 600 word DRS status array
!
      IMPLICIT  NONE
      INTEGER*4 I,IDLU,IFRQ(512),IINILEN(32),IADDR,
     &    ITEMP(3,64),IPS,IPTR,II,IJ,IOS,IRSPORT(32),
     &    IRSLOOK(8,32),IRSFREQ(8,32),IRS,IRTMP(3,8),IISSSIZ,IPARCNT,
     &    IXMCNT(32),ICNFSIZ,ICNFCNT,IPRTLEN(6,32),IPARTS(32),J,IW,IC,
     &    IP,LAN,IXMTOT,IBBRSIG(8),IBBRZER(8),IB,IBBRFRQ
      INTEGER*2 JINIPKT(131,32),JLOOKUP(742,6,32),
     &    JISSBLK(2048),JLSTOFF,JDRSTAT(600)
      CHARACTER CEADD(32)*17,CNAME(32)*6,CLINE*80,CARDTYP*3,CNAM(512)*4,
     &    CPARA*3,CPORTAD*3,CDLUADD(32)*6,CDLUNAM(32)*6
      DATA IBBRSIG /81,82,83,224,91,92,93,227/
      DATA IBBRZER /84,85,86,225,94,95,96,228/
      DATA IBBRFRQ /2/
!
! Compute ISS size and parameter count
      I=2
      DO WHILE(JISSBLK(I).NE.4095)
        IF(JISSBLK(I).NE.0) THEN
          IPARCNT=IPARCNT+1
          IF(IPARCNT.NE.1) IISSSIZ=IISSSIZ+(JISSBLK(I)-JLSTOFF)/2
          JLSTOFF=JISSBLK(I)
        END IF
        I=I+1
      END DO
      IPARCNT=IPARCNT-1      
      JDRSTAT(547)=JISSBLK(1)
      JDRSTAT(549)=IPARCNT
      JDRSTAT(550)=IISSSIZ
!
      ICNFSIZ=0
      ICNFCNT=0
! Get DLU names and numbers
      CALL DRS_ADDRESSES_READ(CEADD,CNAME,IADDR)
! Get parameter frequencies
      CALL DRS_PARAMETERS_READ(IFRQ,CNAM)
! Clear returned counts of ini message lengths, and total words from each DLU
      IDLU=0
      IXMTOT=0
      DO I=1,32
        IINILEN(I)=0
        IRSPORT(I)=0
        IXMCNT(I)=0
        IPARTS(I)=0
        DO II=1,6
          IPRTLEN(II,I)=0
        END DO
        DO II=1,131
          JINIPKT(II,I)=0
        END DO
        DO II=1,742
          DO J=1,6
            JLOOKUP(II,J,I)=0
          END DO
        END DO
        DO II=1,8
          IRSLOOK(II,I)=0
          IRSFREQ(II,I)=0
        END DO
        CDLUADD(I)=' '
      END DO
! Scan the config file for each DLU, identifying ports in use and frequencies
      DO I=1,IADDR                     !For each defined ethernet address
        IPS=0                          !No of parameters on this DLU
        IRS=0                          !No of RS232 parameters on this DLU
        OPEN(11,FILE='HORACE:DRS_CONFIG.DAT',STATUS='OLD',READONLY)
        READ(11,'(A)',IOSTAT=IOS) CLINE
! For each line
        DO WHILE(IOS.EQ.0)
          IF(CLINE(1:6).EQ.CNAME(I)) THEN
            CALL STR_COMPRESS(CLINE)
            CALL STR$ELEMENT(CARDTYP,2,' ',CLINE)
! If matches DLU name, and not a serial type, extract address, parno, freq
            IF(CARDTYP(1:1).NE.'S') THEN
              IPS=IPS+1
              IF(IPS.GT.64) THEN
                TYPE *,'More than 64 ports specified for '//CNAME(I)
                CALL EXIT
              END IF
              CALL STR$ELEMENT(CPORTAD,4,' ',CLINE)
              READ(CPORTAD,'(BN,Z)') ITEMP(1,IPS) !Port address
              CALL STR$ELEMENT(CPARA,5,' ',CLINE)
              READ(CPARA,'(BN,I)') ITEMP(2,IPS) !Parameter number
              ITEMP(3,IPS)=IFRQ(ITEMP(2,IPS)) !Parameter frequency
              DO IB=1,8                !Request BBR signal data at 2 Hz
                IF(ITEMP(2,IPS).EQ.IBBRSIG(IB)) ITEMP(3,IPS)=IBBRFRQ
              END DO
              ICNFCNT=ICNFCNT+1
              ICNFSIZ=ICNFSIZ+IFRQ(ITEMP(2,IPS))
            ELSE
              IRS=IRS+1
              IF(IRS.GT.8) THEN
                TYPE *,'More than 8 RS232 ports specified for '//
     &              CNAME(I)
                CALL EXIT
              END IF
              CALL STR$ELEMENT(CPORTAD,4,' ',CLINE)
              READ(CPORTAD,'(BN,Z)') IRTMP(1,IRS) !Port address
              IRTMP(1,IRS)=IRTMP(1,IRS)/2-65
              IF(IRTMP(1,IRS).GT.3) IRTMP(1,IRS)=IRTMP(1,IRS)-4 !Port num 0-7
              CALL STR$ELEMENT(CPARA,5,' ',CLINE)
              READ(CPARA,'(BN,I)') IRTMP(2,IRS) !Parameter number
              IRTMP(3,IRS)=IFRQ(IRTMP(2,IRS)) !Parameter frequency
              ICNFCNT=ICNFCNT+1
              ICNFSIZ=ICNFSIZ+IRTMP(3,IRS)
            END IF
          END IF
          READ(11,'(A)',IOSTAT=IOS) CLINE
        END DO
        CLOSE(11)
        IF(IPS.GT.0.OR.IRS.GT.0) THEN
          IDLU=IDLU+1
          IF(IDLU.GT.32) THEN
            TYPE *,'More than 32 DLUs specified in DRS_CONFIG.DAT'
            CALL EXIT
          END IF
          CALL STRING_ADDR(CEADD(I),%REF(CDLUADD(IDLU)),II)
          CDLUNAM(IDLU)=CNAME(I)
        END IF
! Handle RS232 ports
        IF(IRS.GT.0) THEN
          IRSPORT(IDLU)=0
          DO II=1,IRS
            IRSPORT(IDLU)=IBSET(IRSPORT(IDLU),IRTMP(1,II))
            IRSFREQ(IRTMP(1,II)+1,IDLU)=IRTMP(3,II)
            IRSLOOK(IRTMP(1,II)+1,IDLU)=JISSBLK(IRTMP(2,II)+1)/2+1
            JDRSTAT(IRTMP(2,II))=IDLU+256*IRTMP(3,II)
            IXMCNT(IDLU)=IXMCNT(IDLU)+1
            IXMTOT=IXMTOT+1
          END DO
        END IF
! Handle normal DLU ports.  Note that expect a packet even if zero ports.
        IF(IPS.GT.0.OR.IRS.GT.0) THEN  !If the DLU is active
! Compute number of packet 4s needed to contain DLU data
          IW=0
          DO II=1,IPS
            IW=IW+ITEMP(3,II)
          END DO
          IPARTS(IDLU)=(IW-1)/742+1    !No of packet 4 parts
! Compute the length of each message part
          DO II=1,IPARTS(IDLU)
            IW=IW-742
            IF(IW.GE.0) IPRTLEN(II,IDLU)=742
            IF(IW.LT.0) IPRTLEN(II,IDLU)=742+IW
          END DO
! Form the configuration packet for the DLU
          JINIPKT(1,IDLU)=10
          JINIPKT(2,IDLU)=IPS
          JINIPKT(3,IDLU)=IRSPORT(IDLU)
          IPTR=4
          DO II=1,IPS
            JINIPKT(IPTR,IDLU)=ITEMP(1,II)
            JINIPKT(IPTR+1,IDLU)=ITEMP(3,II)
            IPTR=IPTR+2
            JDRSTAT(ITEMP(2,II))=IDLU+256*ITEMP(3,II)
          END DO
! Compute config packet length in bytes
          IINILEN(IDLU)=IPTR-1
! Swap the bytes in the config packet
          CALL SWAP_ENDS16(JINIPKT(1,IDLU),IPTR-1)
! Update the expected packet count for the DLU
          IXMCNT(IDLU)=IXMCNT(IDLU)+IPARTS(IDLU)
          IXMTOT=IXMTOT+IPARTS(IDLU)
! Compute posn in DRS block for each word in each packet type 4
          IC=0
          IP=1
          DO II=1,IPS
            DO IJ=1,ITEMP(3,II)
              IC=IC+1
              IF(IC.GT.742) THEN
                IP=IP+1
                IC=1
              END IF
              JLOOKUP(IC,IP,IDLU)=JISSBLK(ITEMP(2,II)+1)/2+IJ
              DO IB=1,8
                IF(ITEMP(2,II).EQ.IBBRSIG(IB)) THEN !De-MUX BBR sig/zero data
                  IF(IBBRFRQ.EQ.2) THEN
                    IF(IJ.EQ.1) JLOOKUP(IC,IP,IDLU)=
     -                  JISSBLK(IBBRZER(IB)+1)/2+1
                    IF(IJ.EQ.2) JLOOKUP(IC,IP,IDLU)=
     -                  JISSBLK(IBBRSIG(IB)+1)/2+1
                  ELSE IF(IBBRFRQ.EQ.4) THEN
                    IF(IJ.EQ.1) JLOOKUP(IC,IP,IDLU)=-1
                    IF(IJ.EQ.2) JLOOKUP(IC,IP,IDLU)=
     -                  JISSBLK(IBBRZER(IB)+1)/2+1
                    IF(IJ.EQ.3) JLOOKUP(IC,IP,IDLU)=-1
                    IF(IJ.EQ.4) JLOOKUP(IC,IP,IDLU)=
     -                  JISSBLK(IBBRSIG(IB)+1)/2+1
                  END IF                   
                END IF
                IF(ITEMP(2,II).EQ.IBBRZER(IB)) JLOOKUP(IC,IP,IDLU)=-1
              END DO
            END DO
          END DO
        END IF
      END DO
      JDRSTAT(548)=IDLU
! Handle the non DLU parameters
      OPEN(11,FILE='HORACE:DRS_CONFIG.DAT',STATUS='OLD',READONLY)
      READ(11,'(A)',IOSTAT=IOS) CLINE
      DO WHILE(IOS.EQ.0)
        IF(CLINE(1:6).EQ.'NONDLU') THEN
          CALL STR_COMPRESS(CLINE)
          CALL STR$ELEMENT(CPARA,5,' ',CLINE)
          READ(CPARA,'(BN,I)') IP      !Parameter number
          ICNFCNT=ICNFCNT+1
          ICNFSIZ=ICNFSIZ+IFRQ(IP)
        END IF
        READ(11,'(A)',IOSTAT=IOS) CLINE
      END DO
      CLOSE(11,IOSTAT=IOS)
!
      IF(ICNFSIZ.GT.2048-9) THEN
        TYPE *,'More that 2014 data words specified for recording'
        CALL EXIT
      END IF
      IF(ICNFSIZ.NE.IISSSIZ) THEN
        WRITE(CLINE,'(A,I4,A,I4)',IOSTAT=IOS) 
     &      'Warning - configured size ',ICNFSIZ,
     &      ' does not match ISS size ',IISSSIZ
        CALL STR_COMPRESS(CLINE)
        TYPE *,CLINE(1:LAN(CLINE))
      END IF
      IF(ICNFCNT.NE.IPARCNT) THEN
        WRITE(CLINE,'(A,I4,A,I4)',IOSTAT=IOS) 
     &      'Warning - configured parameters ',ICNFCNT,
     &      ' do not match ISS parameters ',IPARCNT
        CALL STR_COMPRESS(CLINE)
        TYPE *,CLINE(1:LAN(CLINE))
      END IF
      RETURN
      END
!*******************************************************************************
      SUBROUTINE ETH_CONN
!
! Opens an Ethernet port on the specified protocol.  Logical ETH_PORT must have
! been defined.
! Note that buffered Ethernet packets are stored in system memory and only a
! limited amount of space is available.  If the product of IPSIZ and IBUFS is
! more than about 60000 attempts to establish a connection will fail with
! 'bad parameter value'.
!
      IMPLICIT  NONE
      CHARACTER CEDEV*8,CEADD*17,CEPRO*5,CTEMP*4,CNAME*5,CEADD1*17
      INTEGER*4 IFUNCT,ISTAT,IBUFS,ITEMP,ISTS
      INTEGER*4 NMA$C_PCLI_PTY,NMA$C_PCLI_PAD,NMA$C_STATE_OFF
      INTEGER*4 NMA$C_PCLI_BFN,NMA$C_PCLI_BUS,NMA$C_PCLI_MCA
      INTEGER*4 NMA$C_LINMC_SET
      INTEGER*2 JTEMP(2)
      INTEGER*4 ISETPARMDSC(2)
      INTEGER*2 JSETPARM(44)
      EQUIVALENCE (ITEMP,JTEMP)
      INCLUDE   '($IODEF)'
      INCLUDE   '($SYSSRVNAM)'
      INCLUDE   'DRS_LDEF.FOR'
!
! The following settings define how the port is to be used, and can be
! changed in the future if necessary.
!
      CEADD='AB-00-04-00-00-00'        !Multicast address used by all DLUs
      CEADD1='AB-00-04-00-00-01'       !Multicast address used by time generator
      CEPRO='33-33'                    !Protocol used by all senders
      CEDEV='ETH_PORT'                 !Network interface to use (logical name)
      IPSIZ=1500                       !Max packet size to buffer (bytes)
      IBUFS=40                         !Max system packet buffers
!
! The following parameters are defined in SYS$LIBRARY:LIB.MLB, but are not
! available in the FORTRAN system definition files, and have therefore had to
! be defined explicitly.
!
      NMA$C_PCLI_BFN=1105              !Preallocated receive buffers
      NMA$C_PCLI_BUS=2801              !Max received message length
      NMA$C_PCLI_MCA=2831              !Multicast address
      NMA$C_PCLI_PAD=2842              !Pad field parameter
      NMA$C_PCLI_PTY=2830              !Protocol type parameter
      NMA$C_STATE_OFF=1                !No padding
      NMA$C_LINMC_SET=1                !Set multicast address
!
! First assign a channel to the Ethernet controller.
!
      CALL GET_LOGICAL(CEDEV,CNAME)
      ISTAT=SYS$ASSIGN(CEDEV,ICHAN,,)
      IF(.NOT.ISTAT) THEN
        CALL LOG_MESS('Unable to assign device '//CNAME)
        CALL EXIT
      END IF
!
! Now define the port characteristics and start the port
!
      ISETPARMDSC(1)=42                !Length of characteristics list
      ISETPARMDSC(2)=%LOC(JSETPARM)    !Address of characteristics list
!
      JSETPARM(1)=NMA$C_PCLI_PTY       !Protocol type
      CTEMP=CEPRO(4:5)//CEPRO(1:2)     !Reverse hex pairs
      READ(CTEMP,'(Z4)') JSETPARM(2)   !Decode protocol into list
      JSETPARM(3)=0                    !High word is zero
!
      JSETPARM(4)=NMA$C_PCLI_PAD       !Pad parameter
      ITEMP=NMA$C_STATE_OFF
      JSETPARM(5)=JTEMP(1)
      JSETPARM(6)=JTEMP(2)
!
      JSETPARM(7)=NMA$C_PCLI_BFN       !Preallocate receive buffers
      ITEMP=IBUFS
      JSETPARM(8)=JTEMP(1)
      JSETPARM(9)=JTEMP(2)
!
      JSETPARM(10)=NMA$C_PCLI_BUS      !Set max buffer (message) size
      ITEMP=IPSIZ
      JSETPARM(11)=JTEMP(1)
      JSETPARM(12)=JTEMP(2)
!
      JSETPARM(13)=NMA$C_PCLI_MCA      !Set allowed multicast address
!
      JSETPARM(14)=14                  !Counted length
      JSETPARM(15)=NMA$C_LINMC_SET     !Allow address
      CALL STRING_ADDR(CEADD,JSETPARM(16),ISTS) !Convert and load DLU address
      CALL STRING_ADDR(CEADD1,JSETPARM(19),ISTS) !Convert and load AMTG addres
!
      IFUNCT=IO$_SETMODE.OR.IO$M_CTRL.OR.IO$M_STARTUP
      ISTAT=SYS$QIOW(,%VAL(ICHAN),%VAL(IFUNCT),JOSB,,,
     -    ,%VAL(%LOC(ISETPARMDSC)),,,,)
      IF(.NOT.ISTAT) THEN
        CALL LOG_MESS('Ethernet startup call failed')
      ELSE
        ISTAT=JOSB(1)
        IF(.NOT.ISTAT) THEN
          CALL LOG_MESS('Ethernet startup on '//CNAME//' failed')
        END IF
      END IF
!
      IF(.NOT.ISTAT) THEN
        ITEMP=SYS$CANCEL(%VAL(ICHAN))
        ITEMP=SYS$DASSGN(%VAL(ICHAN))
        CALL LIB$STOP(%VAL(ISTAT))
      END IF
!
      CALL LOG_MESS('Ethernet port opened on '//CNAME)
      CALL ST_MESS('Ethernet port opened on '//CNAME)
      JOSB(1)=0
      RETURN
      END
!*******************************************************************************
      SUBROUTINE ETH_AST
!
! This routine is used because FORTRAN routines do/did not call themselves
!
      CALL ETH_AST1
      RETURN
      END
!*******************************************************************************
      SUBROUTINE ETH_AST1
!
! Reads and processes all ethernet packets received.
! 
      IMPLICIT  NONE
      INTEGER*4 IFNCT,ISTAT,IOSB(2),I,ISRC,IERR
      INTEGER*2 JPKT(750),JPTYPE,JTIMTYP,JMBX(757),JTMP
      CHARACTER CHEADER*14,CSOURCE*6
      INCLUDE   '($SYSSRVNAM)'
      INCLUDE   '($IODEF)'
      INCLUDE   '($SSDEF)'
      INCLUDE   '($XMDEF)'
      INCLUDE   'DRS_GDEF.FOR'
      INCLUDE   'DRS_LDEF.FOR'
      EQUIVALENCE (IOSB,JOSB)
      EQUIVALENCE (JMBX,CHEADER)
      EXTERNAL  ETH_AST

      IF(JOSB(1).EQ.0) THEN            !First call, so nothing to process
        IFNCT=IO$_READVBLK
        JTIMTYP=3
        CALL SWAP_ENDS16(JTIMTYP,1)
        GOTO 10 
      END IF
!
! Check for errors
!
      IF(.NOT.LMBX) THEN
        IERR=1
        IF(.NOT.JOSB(1)) THEN
          IREADER=IREADER+1
          CALL LOG_MESS('Read error')
        ELSE IF(IOSB(2).AND.XM$M_ERR_FATAL.NE.0) THEN
          ICNTERR=ICNTERR+1
          CALL LOG_MESS('Controller error')
        ELSE IF(IOSB(2).AND.XM$M_STS_TIMO.NE.0) THEN
          ITIMOUT=ITIMOUT+1
          CALL LOG_MESS('Timeout error')
        ELSE IF(IOSB(2).AND.XM$M_STS_BUFFAIL.NE.0) THEN
          IBUFERR=IBUFERR+1
          CALL LOG_MESS('Buffer error')
        ELSE IF(JOSB(2).EQ.0) THEN
          INULLIN=INULLIN+1
          CALL LOG_MESS('Null packet')
        ELSE
          IERR=0
        END IF
        IF(IERR.EQ.1) GOTO 10
      END IF
!
! If getting 'packets' from the mailbox then split the header and data
!
      IF(LMBX) THEN
        DO I=8,(JOSB(2)-1)/2+1
          JPKT(I-7)=JMBX(I)
        END DO
        JOSB(2)=JOSB(2)-14
      END IF
!
! If in diagnostic mode then write out packet contents
!
      IF(IDIAG.GT.0) WRITE(6,'(X,16Z5.4)',IOSTAT=ISTAT) 
     &    (JPKT(I),I=1,JOSB(2)/2)
!
! Check for valid AMTG time packets and if so check HORACE clock and set up 
! AST to close current data collection period in 0.5 s time.
!
      IF(JPKT(1).EQ.JTIMTYP.AND.JPKT(2).GT.0) THEN
        CALL CLOCKSET(JPKT)
        CALL BLOCK_TIMER(JPKT)
      END IF
!
! Identify the source, adding it to the source list if necessary
!
      CSOURCE=CHEADER(7:12)
      ISRC=0
      DO I=1,ISOURCE
        IF(CSOURCE.EQ.CSRCLST(I)) ISRC=I
      END DO
      IF(ISRC.EQ.0.AND.ISOURCE.LT.32) THEN
        ISOURCE=ISOURCE+1
        CSRCLST(ISOURCE)=CSOURCE
        ISRC=ISOURCE
        DO I=1,32
          IF(CSOURCE.EQ.CDLUADD(I)) THEN
            CALL LOG_MESS('First packet received from '//CDLUNAM(I))
            JDRSTAT(513+I)=ICHAR(CSOURCE(6:6)) !Store DLU address
            CALL SWAP_ENDS16(JDRSTAT(513+I),1)
          END IF
        END DO
        IF(JPKT(1).EQ.JTIMTYP) THEN
          JDRSTAT(513)=ICHAR(CSOURCE(6:6))     !Store AMTG address
          CALL SWAP_ENDS16(JDRSTAT(513),1)
        END IF
      END IF

!
!      do i=1,32
!        if(csource.eq.cdluadd(i)) then
!          if(cdlunam(i).eq.'UPPBBR'.and.jpkt(1).eq.'0400'x) then
!          WRITE(6,'(X,16Z5.4)',IOSTAT=ISTAT) 
!     &    (JPKT(ierr),ierr=1,JOSB(2)/2)
!            jpkt(9)=1
!            jpkt(10)=2
!            jpkt(11)=3
!            jpkt(12)=4
!            jpkt(13)=5
!            jpkt(14)=6
!            jpkt(15)=7
!            jpkt(16)=8
!            jpkt(17)=9
!            jpkt(18)=10
!            jpkt(19)=11
!            jpkt(20)=12
!            jpkt(21)=13
!            jpkt(22)=14
!            jpkt(23)=15
!            jpkt(24)=16
!            call swap_ends16(jpkt(9),16)
!          end if
!        end if
!      end do        
!
! Identify the packet type and increment its count
!
      JPTYPE=JPKT(1)
      CALL SWAP_ENDS16(JPTYPE,1)
      IF(JPTYPE.GE.1.AND.JPTYPE.LE.32.AND.ISRC.GT.0) THEN
        IPKTCNT(ISRC,JPTYPE)=IPKTCNT(ISRC,JPTYPE)+1
      ELSE IF(ISRC.GT.0) THEN
        IPKTCNT(ISRC,33)=IPKTCNT(ISRC,33)+1
      END IF
!
! Pass the packet to common if it is currently requested
!
      JTMP=0
      IF(JPTYPE.EQ.5) THEN             !For RS232 check message is from req port
        JTMP=JPKT(7)
        CALL SWAP_ENDS16(JTMP,1)
        IF(JTMP.NE.IGETPRT.AND.IGETPRT.NE.0) JTMP=1
        IF(JTMP.NE.1) JTMP=0
      END IF
      IF(CSOURCE.EQ.CGETPKT.AND.JPTYPE.EQ.IGETPKT.AND.JTMP.EQ.0) THEN
        IGETSIZ=MIN((JOSB(2)-1)/2+1,750)
        DO I=1,IGETSIZ
          JGETPKT(I)=JPKT(I)           !Transfer data to common area
        END DO
        CGETPKT=' '
        IGETCNT=IGETCNT+1
      END IF
!
! Now process each packet type
!
      IF(JPTYPE.EQ.3) THEN             !AMTG time packet
        CALL DO_PKT3(JPKT,CSOURCE,ISRC)
      ELSE IF(JPTYPE.EQ.4) THEN        !DLU data packet
        CALL DO_PKT4(JPKT,CSOURCE,ISRC)
      ELSE IF(JPTYPE.EQ.5) THEN        !DLU RS232 packet
        CALL DO_PKT5(JPKT,CSOURCE,ISRC)
      ELSE IF(JPTYPE.EQ.6) THEN        !DLU status packet
        CALL DO_PKT6(JPKT,CSOURCE)
      ELSE IF(JPTYPE.EQ.7) THEN        !DLU ISS packet
        CALL DO_PKT7(JPKT,CSOURCE)
      END IF
!
! If got all the data packets for the second then output them even though
! haven't got to end of collection period.
!
!      ixmtot=1
      IF(IMTOT.EQ.IXMTOT) CALL NEXT_BLOCK('MID')
!
! Set up the next read
!
10    IF(LMBX) THEN
        ISTAT=SYS$QIO(,%VAL(ICHAN),%VAL(IFNCT),JOSB,%REF(ETH_AST),,
     -      JMBX,%VAL(1514),,,,)
        IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      ELSE
        ISTAT=SYS$QIO(,%VAL(ICHAN),%VAL(IFNCT),JOSB,%REF(ETH_AST),,
     -      %REF(JPKT),%VAL(IPSIZ),,,%REF(CHEADER),) !Read a packet
        IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      END IF
      RETURN
      END     
!*******************************************************************************
      SUBROUTINE WAIT_COMMANDS
!
! Waits for event flag 83 to be set to show that there is a packet to output, or
! the counters need zeroing, or some other action is required. Output the 
! packet, clear the event flag, and wait for some more.  Note that the output 
! packet is simply put on the IO queue.  There is no waiting for completion or 
! checking of result.
!
      IMPLICIT  NONE
      INTEGER*4 ISTAT,I
      INTEGER*2 JTYP
      CHARACTER CTMPFNO*6
      INCLUDE   '($IODEF)'
      INCLUDE   '($SSDEF)'
      INCLUDE   '($SYSSRVNAM)'
      INCLUDE   'DRS_GDEF.FOR'
      INCLUDE   'DRS_LDEF.FOR'

      ISTAT=SYS$ASCEFC(%VAL(64),'H_CEF_CLUSTER',,) !Associate with common EFs
      IF(ISTAT.NE.SS$_NORMAL) CALL LIB$SIGNAL(%VAL(ISTAT))
      ISTAT=SYS$CLREF(%VAL(83))        !Clear event flag
10    ISTAT=SYS$WAITFR(%VAL(83))       !Wait for event flag to set
!
      IF(CSNDPKT.NE.' ') THEN          !Packet from DRS_CONTROL to output
        JTYP=JSNDPKT(1)
        CALL SWAP_ENDS16(JTYP,1)
        IF(JTYP.EQ.11) THEN            !Solicited packet 6 request
          DO I=1,32
            IF(CDLUADD(I).EQ.CSNDPKT) IREQSTS(I)=1 !Show soliciting
          END DO
        END IF
        CALL SNDPKT(JSNDPKT,ISNDSIZ,CSNDPKT)
        CSNDPKT=' '
      END IF
!
      IF(IZERCNT.EQ.1) THEN
        CALL ZERO_COUNTERS
        IZERCNT=0
        CALL LOG_MESS('Counters zeroed by DRS_CONTROL')
      END IF
!
      IF(ITSTDRS.EQ.1) ITSTDRS=0       !Response test by DRS_CONTROL
!
      IF(ICHGFNO.EQ.1) THEN            !New flight number
        CTMPFNO=CNEWFNO
        CALL STR$UPCASE(CTMPFNO,CTMPFNO)      
        IF(CTMPFNO(2:4).GE.'000'.AND.CTMPFNO(2:4).LE.'999') THEN
          CFLTNUM=CTMPFNO
          CALL PUT_CHARSTR(%REF(CFLTNUM),JDRSTAT(546))
          CALL SET_FN_LOG(CFLTNUM)     !Define FLIGHT_NUMBER logical name
        END IF
        ICHGFNO=0
        CALL LOG_MESS('Flight number set to '//CFLTNUM)
      END IF
!
      IF(ICHGDAT.EQ.1) THEN            !Change recording mode
        IF(IDATHLD.EQ.0) THEN
          IDATHLD=1
          CALL LOG_MESS('Data set to hold by DRS_CONTROL')
        ELSE
          IDATHLD=0
          CALL LOG_MESS('Data set to record by DRS_CONTROL')
        END IF
        ICHGDAT=0
      END IF
!
      IF(INXTEVM.EQ.1) THEN            !Event mark request
        IEVENT=IEVENT+1
        IF(SECNDS(RBLKTIM).LT.0.5) IEVM(1)=IEVENT
        IEVM(2)=IEVENT
        INXTEVM=0
      END IF
!
      ISTAT=SYS$CLREF(%VAL(83))        !Clear event flag
      GOTO 10
      END
!*******************************************************************************
      SUBROUTINE DO_PKT3(JPKT,CSOURCE,ISRC)
!
! Handle received packet type 3 (AMTG time packet) by storing each packet and 
! updating the AMTG address and packet count.  Anything interesting is noted in 
! the log or various status words.
!
      IMPLICIT  NONE
      INTEGER*4 ILSTTIM,ITIM,I,IOS,ISRC
      INTEGER*2 JPKT(750),JTIM(2),JTMP
      CHARACTER CSOURCE*6,CTIM*5,CLINE*80,CTIME*8,CADDR*17
      INCLUDE   'DRS_LDEF.FOR'
      INCLUDE   'DRS_GDEF.FOR'
      EQUIVALENCE (ITIM,JTIM)
      DATA      ILSTTIM /-1/

      DO I=1,13
        JTIMPKT(I)=JPKT(I)             !Store latest time packet as received
      END DO
      JTMP=JPKT(2)
      CALL SWAP_ENDS16(JTMP,1)
      CALL MVBITS(0,0,8,JDRSTAT(513),0)
      JDRSTAT(513)=IBSET(JDRSTAT(513),4) !Sending packets
      IF(JTMP.GT.0) JDRSTAT(513)=IBSET(JDRSTAT(513),JTMP-1) !Set init source
      IF(JPKT(8).NE.0) JDRSTAT(513)=IBSET(JDRSTAT(513),3) !Set if have GPS
      IF(ILSTTIM.EQ.-1.OR.CTIMPKT.NE.CSOURCE) THEN !New/changed AMTG
        CALL ADDR_STRING(CSOURCE,CADDR)
        CALL LOG_MESS('AMTG address is '//CADDR)
      END IF
      CTIMPKT=CSOURCE                  !AMTG address
      ITIMPKT=ITIMPKT+1                !Bump packet count
      IWATCHD(33)=2
      IF(JPKT(2).EQ.0.AND.ISRC.GT.0) THEN
        ITIMSTS(ISRC,1)=ITIMSTS(ISRC,1)+1  !Not intialised
        IF(ITIMSTS(ISRC,1).EQ.1) CALL LOG_MESS('AMTG not initialised')
      ELSE
        JTIM(1)=JPKT(3)
        JTIM(2)=JPKT(4)
        CALL SWAP_ENDS32(ITIM,1)
!        type *,'pkt3',itim
        IF(ILSTTIM.NE.-1) THEN
          IF(ISRC.GT.0) THEN
            IF(ITIM.EQ.ILSTTIM) THEN
              ITIMSTS(ISRC,2)=ITIMSTS(ISRC,2)+1 !Same time as before
            ELSE IF(ITIM.EQ.ILSTTIM+1) THEN
              ITIMSTS(ISRC,3)=ITIMSTS(ISRC,3)+1 !Good - 1s since last
            ELSE IF(ITIM.LT.ILSTTIM) THEN
              ITIMSTS(ISRC,4)=ITIMSTS(ISRC,4)+1 !Time moved backwards
            ELSE IF(ITIM.GT.ILSTTIM+1) THEN
              ITIMSTS(ISRC,5)=ITIMSTS(ISRC,5)+1 !Time advanced more than 1s
            END IF
          END IF
          IF(ITIM-ILSTTIM.NE.1) THEN   !There has been a break
            WRITE(CTIM,'(I5)',IOSTAT=IOS) ITIM-ILSTTIM-1
           CALL LOG_MESS('Time packets resumed after '//CTIM//'s break')
          END IF
        ELSE                           !First valid time
          JTMP=JPKT(2)
          CALL SWAP_ENDS16(JTMP,1)
          IF(JTMP.EQ.1) THEN
            CALL LOG_MESS('AMTG initialised with GPS time')
          ELSE IF(JTMP.EQ.2) THEN
            CALL LOG_MESS('AMTG initialised with battery time')
          ELSE IF(JTMP.EQ.3) THEN
            CALL LOG_MESS('AMTG initialised with supplied time')
          END IF
        END IF
        ILSTTIM=ITIM
        IF(JPKT(8).NE.0) THEN          !Record slip if GPS valid
          JAMTSLP=JPKT(9)
          CALL SWAP_ENDS16(JAMTSLP,1)
        END IF
      END IF
      IF(MOD(NINT(SECNDS(0.0)),600).EQ.0) THEN !Log message every 10 mins
        CLINE='hh:mm:ss AMTG Sts:n GPS:n Slp:nnnn Spm:nnnnnn '//
     &      'U:nnnnnn D:xxxxxxxx <nnn =nnn >nnn'
        CALL TIME(CTIME)
        CLINE(1:8)=CTIME(1:8)
        JTMP=JPKT(2)
        CALL SWAP_ENDS16(JTMP,1)
        WRITE(CLINE(19:19),'(I1)',IOSTAT=IOS) JTMP        
        JTMP=JPKT(8)
        CALL SWAP_ENDS16(JTMP,1)
        WRITE(CLINE(25:25),'(I1)',IOSTAT=IOS) JTMP        
        JTMP=JPKT(9)
        CALL SWAP_ENDS16(JTMP,1)
        WRITE(CLINE(31:34),'(I4)',IOSTAT=IOS) JTMP        
        JTIM(1)=JPKT(3)
        JTIM(2)=JPKT(4)
        CALL SWAP_ENDS32(ITIM,1)
        WRITE(CLINE(40:45),'(I6)',IOSTAT=IOS) ITIM
        JTIM(1)=JPKT(10)
        JTIM(2)=JPKT(11)
        CALL SWAP_ENDS32(ITIM,1)
        WRITE(CLINE(49:54),'(I6)',IOSTAT=IOS) ITIM
        JTIM(1)=JPKT(12)
        JTIM(2)=JPKT(13)
        CALL SWAP_ENDS32(ITIM,1)
        WRITE(CLINE(58:65),'(Z8.8)',IOSTAT=IOS) ITIM
        WRITE(CLINE(68:70),'(I3.3)',IOSTAT=IOS) ITIMSTS(ISRC,4)
        WRITE(CLINE(73:75),'(I3.3)',IOSTAT=IOS) ITIMSTS(ISRC,2)
        WRITE(CLINE(78:80),'(I3.3)',IOSTAT=IOS) ITIMSTS(ISRC,5)
        TYPE *,CLINE
      END IF
      RETURN
      END
!*******************************************************************************
      SUBROUTINE DO_PKT4(JPKT,CSOURCE,ISRC)
!
! Handle received packet type 4 (DLU data packet) as follows:
!
! 1. If waiting for first packet from DLU (ie the DRS program has just
!    started, then sends a request for the DLU's configuration.
!
! 2. If all config checks have been passed and is already logging then,
!    logs the data in the packet.
!
! 3. Under all other circumstances this packet is ignored.
!
      IMPLICIT  NONE
      INTEGER*4 I,ISTS,II,IP,ISRC
      INTEGER*2 JPKT(750),JTMP(2)
      CHARACTER CSOURCE*6
      INCLUDE   'DRS_GDEF.FOR'
      INCLUDE   'DRS_LDEF.FOR'

      DO I=1,IDLUCNT
        IF(CDLUADD(I).EQ.CSOURCE) THEN
          CALL MVBITS(0,0,3,JDRSTAT(513+I),0)
          JTMP(1)=JPKT(5)              !Time status
          CALL SWAP_ENDS16(JTMP(1),1)
          IF(JTMP(1).LE.3) JDRSTAT(513+I)=IBSET(JDRSTAT(513+I),0)
          IF(JTMP(1).EQ.0) THEN
            JDRSTAT(513+I)=IBSET(JDRSTAT(513+I),1)
            JDRSTAT(513+I)=IBSET(JDRSTAT(513+I),2)
          ELSE IF(JTMP(1).EQ.1) THEN
            JDRSTAT(513+I)=IBSET(JDRSTAT(513+I),2)
          ELSE IF(JTMP(1).EQ.2) THEN
            JDRSTAT(513+I)=IBSET(JDRSTAT(513+I),1)
          END IF
          JTMP(1)=JPKT(6)
          CALL SWAP_ENDS16(JTMP(1),1)
          IDLUSLP(I)=JTMP(1)           !Record the slip
          IF(JPKT(5).NE.0) IDLUSLP(I)=32767 !Don't use unless everything OK
          IWATCHD(I)=2
          IF(IDLUSEQ(I).EQ.0) THEN     !Need to know configuration
            JTMP(1)=12
            CALL SWAP_ENDS16(JTMP(1),1)
            CALL LOG_MESS('Requesting configuration from '//CDLUNAM(I))
            IDLUSEQ(I)=1               !Show waiting for config packet 7
            CALL SNDPKT(JTMP,2,CSOURCE)
          ELSE IF(IDLUSEQ(I).EQ.3) THEN !Config has been confirmed
            CALL CHECK_TIME(JPKT(1),ISRC,ISTS)
            JTMP(1)=JPKT(2)
            CALL SWAP_ENDS16(JTMP(1),1)
            IP=JTMP(1)               !Message part
            IF(IP.GE.1.AND.IP.LE.6.AND.IPRTLEN(IP,I).GT.0) THEN
              CALL SWAP_ENDS16(JPKT(9),IPRTLEN(IP,I))
              DO II=1,IPRTLEN(IP,I)
                IF(JLOOKUP(II,IP,I).NE.-1)
     -              JDRSBLK(JLOOKUP(II,IP,I))=JPKT(8+II)
              END DO
            END IF
            IMCNT(I)=IMCNT(I)+1
            IMTOT=IMTOT+1
            ILSTPK4(I)=1               !Receiving packets
          END IF
        END IF
      END DO
      RETURN
      END
!*******************************************************************************
      SUBROUTINE DO_PKT5(JPKT,CSOURCE,ISRC)
!
! Handle received packet type 5 (DLU RS232 packet) as follows:
!
! 1. If waiting for first packet from DLU (ie the DRS program has just
!    started, then sends a request for the DLU's configuration.
!
! 2. If all config checks have been passed and is already logging then,
!    logs the data in the packet.
!
! 3. Under all other circumstances this packet is ignored.
!
      IMPLICIT  NONE
      INTEGER*4 I,ISTS,ICNT,K,J,IP,IL,ISRC
!      INTEGER*4 IPP
      INTEGER*2 JPKT(136),JTMP
!      INTEGER*2 JPRTADD(8)
      BYTE      BDRSBLK(4096)
      CHARACTER CSOURCE*6
      INCLUDE   'DRS_GDEF.FOR'
      INCLUDE   'DRS_LDEF.FOR'
      EQUIVALENCE (BDRSBLK,JDRSBLK)
!      DATA    JPRTADD /'82'X,'84'X,'86'X,'88'X,'92'X,'94'X,'96'X,'98'X/

      DO I=1,IDLUCNT
        IF(CDLUADD(I).EQ.CSOURCE) THEN
          IWATCHD(I)=2
          IF(IDLUSEQ(I).EQ.0) THEN     !Need to know configuration
            JTMP=12
            CALL SWAP_ENDS16(JTMP,1)
            CALL LOG_MESS('Requesting configuration from '//CDLUNAM(I))
            IDLUSEQ(I)=1               !Show waiting for config packet 7
            CALL SNDPKT(JTMP,2,CSOURCE)
          ELSE IF(IDLUSEQ(I).EQ.3) THEN !Config has been confirmed
            CALL CHECK_TIME(JPKT(1),ISRC,ISTS)
            CALL SWAP_ENDS16(JPKT(7),2)
!            IPP=JPKT(7)              !Port no (0-7)
!            DO J=0,7
!              IF(IPP.EQ.JPRTADD(J+1)) IP=J
!            END DO
            IP=JPKT(7)               !Port no (0-7)
            IL=JPKT(8)               !Message length (bytes) (0-256)
            IF(IP.GE.0.AND.IP.LE.7) THEN
              J=9
              ICNT=MIN(IRSFREQ(IP+1,I),(IL+1)/2)
!              CALL SWAP_ENDS16(JPKT(J),ICNT) !No need to swap bytes
              DO K=1,ICNT
                JDRSBLK(IRSLOOK(IP+1,I)+K-1)=JPKT(J+K-1)
              END DO
              IF(IL.LT.2*IRSFREQ(IP+1,I)) THEN !Fill missing with Fs
                DO J=2*IRSLOOK(IP+1,I)-1+IL,
     &               2*IRSLOOK(IP+1,I)-1+2*IRSFREQ(IP+1,I)-1
                  BDRSBLK(J)='FF'X
                END DO
              END IF
            END IF
            IMCNT(I)=IMCNT(I)+1
            IMTOT=IMTOT+1
            ILSTPK4(I)=1               !Receiving packets
          END IF
        END IF
      END DO
      RETURN
      END
!*******************************************************************************
      SUBROUTINE DO_PKT6(JPKT,CSOURCE)
!
! Handle received packet type 6 (DLU status packet) as follows:
!
! 1. If this packet is in response to a solicited request from DRS_CONTROL then
!    no further action is taken.
!
! 2. In all other circumstances the packet type 6 is taken to indicate that
!    the DLU has been restarted, so the configuration
!    packet is sent, and the configuration is requested back from the DLU.
!
! 3. Ignore these packets if within 10s of sending a configuration packet.
!
      IMPLICIT  NONE
      INTEGER*4 I
      INTEGER*2 JPKT(750),JTMP(2)
      CHARACTER CSOURCE*6
      INCLUDE   'DRS_GDEF.FOR'
      INCLUDE   'DRS_LDEF.FOR'

      DO I=1,IDLUCNT
        IF(CDLUADD(I).EQ.CSOURCE) THEN
          IWATCHD(I)=2
          IF(IDLUSEQ(I).EQ.1.AND.IIGNSTS(I).LT.10) THEN
            IIGNSTS(I)=IIGNSTS(I)+1
          ELSE IF(IDLUSEQ(I).EQ.0.OR.
     &       IDLUSEQ(I).EQ.1.OR.
     &       IDLUSEQ(I).EQ.2.OR.
     &       (IDLUSEQ(I).EQ.3.AND.IREQSTS(I).EQ.0)) THEN
            IF(IDLUSEQ(I).EQ.3) IDLUSRT(I)=IDLUSRT(I)+1 !Restart
            CALL LOG_MESS('Sending configuration to '//CDLUNAM(I))
            CALL SNDPKT(JINIPKT(1,I),IINILEN(I)*2,CSOURCE) !Send ini packet
            CALL LIB$WAIT(0.5)
            JTMP(1)=12
            CALL SWAP_ENDS16(JTMP(1),1)
            IDLUSEQ(I)=1               !Waiting for config from DLU (pkt 7)
            IIGNSTS(I)=0
            CALL SNDPKT(JTMP,2,CSOURCE)!Request config from DLU (pkt 7)
          ELSE IF(IREQSTS(I).EQ.1) THEN
            IREQSTS(I)=0
          END IF
          JTMP(1)=JPKT(1)              !Stop compiler warnings until use JPKT
        END IF
      END DO
      RETURN
      END
!*******************************************************************************
      SUBROUTINE DO_PKT7(JPKT,CSOURCE)
!
! Handle received packet type 7 (DLU config packet) as follows:
!
! 1. If waiting for the DLU config to be returned then checks the config.  If
!    correct then shows this in status word.
!    If incorrect indicates this in the status word, so that the DLU
!    will have to be restarted to make any further progress.
!
! 2. Under all other circumstances this packet is ignored.
!
! 3. Also includes special specific code to force the core console DLU to send
!    a 'D' to the CNC every second, via RS232 port 0.
!
      IMPLICIT  NONE
      INTEGER*4 I,J,IOS
      INTEGER*2 JPKT(142),JTMP(4)
      BYTE      BTMP(8)
      CHARACTER CSOURCE*6,CSTRING*8
      EQUIVALENCE (JTMP,BTMP)
      INCLUDE   'DRS_GDEF.FOR'
      INCLUDE   'DRS_LDEF.FOR'
!
      DO I=1,IDLUCNT
        IF(CDLUADD(I).EQ.CSOURCE) THEN
          IWATCHD(I)=2
          IF(IDLUSEQ(I).EQ.1) THEN
            IDLUSEQ(I)=3               !Assume check succeeds
            DO J=2,IINILEN(I)
              IF(JPKT(J).NE.JINIPKT(J,I)) IDLUSEQ(I)=2 !Config check failed
      if(jpkt(j).eq.'8a00'x.and.jinipkt(j,i).eq.'c000'x) idluseq(i)=3  !for present accept 8a for c0 (digital io)
            END DO          
            IF(IDLUSEQ(I).EQ.3) THEN
              CALL LOG_MESS('Received configuration from '//CDLUNAM(I)//
     &            ' checked OK')
              IF(CDLUNAM(I).EQ.'CORCON') THEN !Special code to trigger CNC
                JTMP(1)=13             !RS232 output packet
                JTMP(2)=0              !CNC must be on serial port 0 (82X)
                JTMP(3)=-1             !Data to be stored in DLU for o/p every s
                CALL SWAP_ENDS16(JTMP(1),3)
                BTMP(7)='D'            !CNC requires a D sent to it every sec
                BTMP(8)=0              !Not used
                CALL SNDPKT(JTMP,8,CSOURCE)!Send RS232 regular output data
              END IF
            ELSE
              CALL LOG_MESS('Received configuration from '//CDLUNAM(I)//
     &            ' failed to check')
              TYPE *,'Sent Rcvd'
              DO J=2,IINILEN(I)
                WRITE(CSTRING,'(2Z4.4)',IOSTAT=IOS) JINIPKT(J,I),JPKT(J)
                TYPE *,CSTRING(3:4)//CSTRING(1:2)//' '//CSTRING(7:8)//
     &              CSTRING(5:6)
              END DO          
            END IF           
          END IF
        END IF
      END DO
      RETURN
      END
!*******************************************************************************
      SUBROUTINE LOG_MESS(MESSAGE)
!
! Writes message to log file consisting of program name, date, time and message.
!
      IMPLICIT  NONE
      CHARACTER MESSAGE*(*),CDAT*9,CTIM*8
!
      CALL DATE1(CDAT)
      CALL TIME(CTIM)
      TYPE *,'DRS '//CDAT//' '//CTIM//'  '//MESSAGE
      RETURN
      END
!*******************************************************************************
      SUBROUTINE ST_MESS(MESSAGE)
!
! Write message to the logical DRS_STAT in the group logical name
! table.  GRPNAM and SYSNAM privileges are required.
!
      IMPLICIT  NONE
      INTEGER*4 ISTAT
      CHARACTER MESSAGE*(*)
      INCLUDE   '($SYSSRVNAM)'
      INCLUDE   '($LNMDEF)'
      INCLUDE   '($PSLDEF)'
!
      STRUCTURE /LIST/
        INTEGER*2 BUFFER_LEN
        INTEGER*2 CODE/LNM$_STRING/
        INTEGER*4 BUFFER_LOC
        INTEGER*4 RET_LEN/0/
        INTEGER*4 END_MARK/0/
      END STRUCTURE
!
      RECORD /LIST/ TLIST
      TLIST.BUFFER_LEN=LEN(MESSAGE)
      TLIST.BUFFER_LOC=%LOC(MESSAGE)
      ISTAT=SYS$CRELNM(,'LNM$GROUP','DRS_STAT',PSL$C_SUPER,TLIST)
      IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      RETURN
      END
!*******************************************************************************
      SUBROUTINE SNDPKT(JPKT,IPKTSIZ,CDEST)
!
! Sends a packet to the specified Ethernet address.  Note that the packet is
! simply put on the I/O queue and there is no checking for completion or
! success.  The packet count is also updated.  The packet is sent as passed in
! JPKT, so any byte swapping must already have been done.  IPKTSIZ is in bytes.
!
      IMPLICIT  NONE
      INTEGER*4 IPKTSIZ,ISTAT,IFUNC,I
      INTEGER*2 JPKT(*),JPTYPE,JMBX(757),JDEST(3)
      CHARACTER CDEST*6,CTEMP*6
      INCLUDE   '($IODEF)'
      INCLUDE   '($SYSSRVNAM)'
      INCLUDE   'DRS_GDEF.FOR'
      INCLUDE   'DRS_LDEF.FOR'
      EQUIVALENCE (CTEMP,JDEST)

      IF(IDIAG.GT.0) WRITE(6,'(X,16Z5.4)',IOSTAT=ISTAT) 
     &    (JPKT(I),I=1,IPKTSIZ/2)
      IFUNC=IO$_WRITEVBLK
      IF(LMBX) THEN
        CTEMP=CDEST
        DO I=1,3
          JMBX(I)=JDEST(I)
        END DO
        DO I=1,(IPKTSIZ-1)/2+1
          JMBX(I+7)=JPKT(I)
        END DO
        ISTAT=SYS$QIO(,%VAL(ICHAN1),%VAL(IFUNC),,,,
     -      JMBX,%VAL(MIN(IPKTSIZ+14,1500)),,,,)
        IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      ELSE
        ISTAT=SYS$QIO(,%VAL(ICHAN),%VAL(IFUNC),,,, !Write the packet, no wait
     &      JPKT,%VAL(MIN(IPKTSIZ,1500)),,,%REF(CDEST),)
        IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      END IF
      JPTYPE=JPKT(1)
      CALL SWAP_ENDS16(JPTYPE,1)
      IF(JPTYPE.GE.1.AND.JPTYPE.LE.32) THEN
        DO I=1,ISOURCE               !Update send count if know address
          IF(CSRCLST(I).EQ.CDEST) 
     &        IPKTCNT(I,JPTYPE)=IPKTCNT(I,JPTYPE)+1
        END DO
      END IF
      RETURN
      END
!*******************************************************************************
      SUBROUTINE INIT
!
! Initializes variables on startup
!
      IMPLICIT  NONE
      INTEGER*4 I
      CHARACTER CVAL*4
      INCLUDE   'DRS_GDEF.FOR'
      INCLUDE   'DRS_LDEF.FOR'

      DO I=1,32
        CSRCLST(I)=' '
        IDLUSRT(I)=0
        IMCNT(I)=0
        IREQSTS(I)=0
        IDLUSEQ(I)=0
        JTSLIPS(I)='7FFF'X
        ILASTT(I)=-1
      END DO
      DO I=1,33
        IWATCHD(I)=0
      END DO
      ICURTIM=-1
      ITSTDRS=0
      ICHGFNO=0
      ICHGDAT=0
      IBLKCNT=-1
      ISOURCE=0
      IZERCNT=0
      IISSNUM=0
      IISSSIZ=0
      IPARCNT=0
      INXTEVM=0
      IMTOT=0
      IEVM(1)=0
      IEVM(2)=0
      CGETPKT=' '              
      CSNDPKT=' '              
      CTIMPKT=' '
      CNEWFNO=' '
      JAMTSLP='7FFF'X
      DO I=1,600
        JDRSTAT(I)=0
      END DO
!
! Only init the following if the logical DRS_RESTART is not defined.  Otherwise
! leave them unchanged in DRS_COM so that after an abort the program will
! restart recording without any operator intervention.
!
      CALL GET_LOGICAL('DRS_RESTART',CVAL)
      IF(CVAL.EQ.' ') THEN
        IDATHLD=1
        IEVENT=0
        CFLTNUM=' '              
        CALL PUT_CHARSTR(%REF('    '),JDRSTAT(546))
      END IF
      CALL SET_FN_LOG(CFLTNUM)
      CALL ZERO_COUNTERS
      RETURN
      END
!*******************************************************************************
      SUBROUTINE ZERO_COUNTERS
!
! Zeros the counters. These are zeroed when DRS starts and when requested by
! DRS_CONTROL.
!
      IMPLICIT  NONE
      INTEGER*4 I,J
      INCLUDE   'DRS_GDEF.FOR'

      DO I=1,32                        !For each DLU
        DO J=1,33                      !For each packet type
          IPKTCNT(I,J)=0
        END DO
        DO J=1,5                       !For each type of timing error
          ITIMSTS(I,J)=0
        END DO
        IMESERR(I)=0                   !Count of wrong no of mess from DLU
      END DO
      IREADER=0                        !Ethernet read errors
      ICNTERR=0
      ITIMOUT=0
      IBUFERR=0
      INULLIN=0
      IGETCNT=0                        !No of specially retrieved packets
      IGETPRT=0
      ITIMPKT=0                        !No of time packets
      RETURN
      END
!*******************************************************************************
      SUBROUTINE CHECK_TIME(JPKT,ISRC,ISTS)
!
! For packet types 4 and 5 from the DLU, checks that the DLU time has been
! initialized, and then start a new DRS block if this is the first data for
! a new second.  Note that all slippage times are ignored at present. ISTS is
! returned as follows:
!
! 1 - DLU has not had its time initialised
! 2 - Time matches current block time
! 4 - Time is less than current block time
! 5 - Time is more than current block time
!
      IMPLICIT  NONE
      INTEGER*4 ISTS,ITIM,ISRC,IBTIM
      INTEGER*2 JPKT(750),JTIM(2),JTYP,JSTS
      EQUIVALENCE (ITIM,JTIM)
      INCLUDE   'DRS_LDEF.FOR'
      INCLUDE   'DRS_GDEF.FOR'
!
      JTYP=JPKT(1)
      CALL SWAP_ENDS16(JTYP,1)
      IF(JTYP.EQ.4) THEN               !Get time status
        JSTS=JPKT(5)
      ELSE IF(JTYP.EQ.5) THEN
        JSTS=JPKT(4)
      END IF
      CALL SWAP_ENDS16(JSTS,1)
      IF(JSTS.GT.3) THEN
        ISTS=1                         !DLU clock not initialized
      ELSE
        IF(JTYP.EQ.4) THEN
          JTIM(1)=JPKT(3)            
          JTIM(2)=JPKT(4)            
        ELSE IF(JTYP.EQ.5) THEN
          JTIM(1)=JPKT(2)            
          JTIM(2)=JPKT(3)            
        END IF
        CALL SWAP_ENDS32(ITIM,1)
!        type *,'pkt4/5',itim,iblktim
        ITIM=MOD(ITIM,24*3600)
        IBTIM=MOD(IBLKTIM,24*3600)
        IF(ITIM.EQ.IBTIM) THEN
          ISTS=2
        ELSE IF(ITIM.LT.IBTIM) THEN
          ISTS=4
        ELSE IF(ITIM.GT.IBTIM) THEN
          ISTS=5
        END IF
      END IF
      IF(ISRC.GT.0) ITIMSTS(ISRC,ISTS)=ITIMSTS(ISRC,ISTS)+1
      RETURN
      END
!*******************************************************************************
      SUBROUTINE NEXT_BLOCK(CMODE)
!
! Outputs the current second's DRS block and starts a new block
!
! Note the following:
!
! DRS always starts with data on hold, except after a restart, and this has to 
! be changed manually by running DRS_CONTROL.
!
! The flight number must have been set to a valid number between c000 and c999
! before data can be changed from hold to record.  The first, alphabetic,
! character of the flight number is not used by DRS, but is always converted to
! uppercase.
!
! The flight number can be changed by DRS_CONTROL without DRS stopping, but any
! change to an invalid number will stop the DRS recording (ie put it on
! hold).  The H_DRS_LOG program will however need to open new files if the 
! flight number changes.
!
! The first 10 data blocks output after DRS starts (ie the blocks that would 
! have block numbers 0 to 9) and data is on record are
! replaced by copies of the ISS block.
!
! The ISS cannot be changed without restarting the DRS program.
!
! Note that all slippage information is currently ignored
!
      IMPLICIT  NONE
      INTEGER*4 IHRS,IMIN,ISEC,I,ISTAT,IOS,ILSTTIM,IDIFF,LAN
      LOGICAL*4 LBLKOPN
      INTEGER*2 JBLK
      CHARACTER CTIM*6,CLINE*80,CMODE*3
      INCLUDE   'DRS_LDEF.FOR'
      INCLUDE   'DRS_GDEF.FOR'
      INCLUDE   'HCOM_DEF.FOR'
      INCLUDE   '($SYSSRVNAM)'
      DATA      LBLKOPN /.FALSE./
!
      IF(CFLTNUM.EQ.' ') IDATHLD=1     !Must have valid fno to record data
!
      IF(LBLKOPN) THEN
        IHRS=IBLKTIM/3600
        ISEC=MOD(IBLKTIM,60)
        IMIN=(IBLKTIM-3600*IHRS)/60
        WRITE(CTIM,'(3I2.2)') IHRS,IMIN,ISEC
        JDRSBLK(10)=0
        JDRSBLK(11)=0
        CALL CVT_BCD(CTIM(1:3),JDRSBLK(10))
        CALL CVT_BCD(CTIM(4:6),JDRSBLK(11))
        JDRSBLK(7)=0
        JDRSBLK(8)=0
        IF(IBLKCNT.NE.-1) THEN         !If started counting blocks
          CALL MVBITS(IBLKCNT,0,12,JDRSBLK(8),0) !Fill in block number
          CALL MVBITS(IBLKCNT,12,12,JDRSBLK(7),0)
        END IF
        JDRSBLK(9)=0
        IF(CFLTNUM.NE.' ') THEN
          CALL CVT_BCD(CFLTNUM(2:4),JDRSBLK(9)) !Fill in flight number
        ELSE
          CALL CVT_BCD('000',JDRSBLK(9)) !Use dummy for diagnostics if on hold
        END IF
        WRITE(CTIM,'(I6.3)',IOSTAT=IOS) IEVM(1) !Fill in event marks
        CALL CVT_BCD(CTIM(4:6),JDRSBLK(12))
        WRITE(CTIM,'(I6.3)',IOSTAT=IOS) IEVM(2)
        CALL CVT_BCD(CTIM(4:6),JDRSBLK(13))
! Fill DRS status words
        JDRSBLK(14)=0
        IF(CTIMTYP.EQ.'H') JDRSBLK(14)=IBSET(JDRSBLK(14),0)
        JDRSBLK(15)=JAMTSLP            !AMTG time slip
        DO I=16,28                     !DLU time slips, may need to mux them
          JDRSBLK(I)=0
        END DO
        IF(IDLUCNT.GT.26.AND.MOD(IBLKTIM-1,3).EQ.2) THEN
          DO I=1,13
            IF(I+26.LE.IDLUCNT) THEN
              JDRSBLK(15+I)=JTSLIPS(26+I)
              JTSLIPS(26+I)='7FFF'X
            END IF
          END DO
        ELSE IF(IDLUCNT.GT.13.AND.MOD(IBLKTIM-1,2).EQ.1) THEN
          DO I=1,13
            IF(I+13.LE.IDLUCNT) THEN
              JDRSBLK(15+I)=JTSLIPS(13+I)
              JTSLIPS(13+I)='7FFF'X
            END IF
          END DO
        ELSE
          DO I=1,13
            IF(I.LE.IDLUCNT) THEN
              JDRSBLK(15+I)=JTSLIPS(I)
              JTSLIPS(I)='7FFF'X
            END IF
          END DO
        END IF
        DO I=1,IDLUCNT                 !Note any missing packets
          JDRSTAT(513+I)=IBSET(JDRSTAT(513+I),4)
          IF(IMCNT(I).NE.IXMCNT(I)) THEN
            IMESERR(I)=IMESERR(I)+1
            JDRSTAT(513+I)=IBCLR(JDRSTAT(513+I),4)
          END IF
        END DO
        JDRSBLK(29)=JDRSTAT(MOD(IBLKTIM,600)+1) !Muxed DRS status info
!
        DO I=1,2048
          JDATBLK(I)=JDRSBLK(I)        !Transfer data to common for DRS_CONTROL
        END DO
!
! Now output the data provided not on hold, provided at least some data 
! packets came in the second, and provided the block time is not less than or
! equal to the previous time by only a small amount (5s).  If there is a large
! negative time there is clearly a problem so just write out the data and work
! out what to do with it later.
!
        IDIFF=IBLKTIM-ILSTTIM
        IF(IDIFF.LT.-43200) IDIFF=IDIFF+24*3600 !Poss midnight crossing
        IF(IDATHLD.NE.1.AND.IMTOT.NE.0.AND.(IDIFF.GT.0.OR.IDIFF.LT.-5))
     &       THEN          !If not on hold write data or small back change
          IF(IBLKCNT.EQ.-1) IBLKCNT=0  !Start counting blocks
          IF(IBLKCNT.GE.0.AND.IBLKCNT.LE.9) THEN
            IF(STATUS(62).EQ.1.OR.STATUS(62).EQ.2) THEN
              DO I=1,2048
                NFDATA(I,STATUS(62))=JISSBLK(I) !Transfer ISS to H_DRS_LOG
              END DO
            END IF
          ELSE
            IF(STATUS(62).EQ.1.OR.STATUS(62).EQ.2) THEN
              DO I=1,2048
                NFDATA(I,STATUS(62))=JDATBLK(I) !Transfer data to H_DRS_LOG
              END DO
            END IF
          END IF
          ISTAT=SYS$SETEF(%VAL(84))      !Set EF to tell H_DRS_LOG
          IF(MOD(IBLKCNT,600).EQ.10) THEN
            JBLK=JZEXT(JDATBLK(8))+4096*JZEXT(JDATBLK(7))
            CLINE=' '
            WRITE(CLINE,'(A,I6,A,I6)',IOSTAT=IOS) 'Blocks sent:',
     &          IBLKCNT,'  Latest block:',JBLK
            CALL LOG_MESS(CLINE(1:LAN(CLINE)))
          END IF
!          type *,ctimtyp,iblktim,iblkcnt,imtot,secnds(0.0)
          IF(IBLKCNT.NE.-1) IBLKCNT=IBLKCNT+1
        END IF
        ILSTTIM=IBLKTIM
        LBLKOPN=.FALSE.
      END IF
!
      IF(CMODE.EQ.'END') THEN          !Open new block at end of each sec
        ICURTIM=IBLKTIM
        RBLKTIM=SECNDS(0.0)            !Used for event marks only
        IEVM(1)=IEVENT
        IEVM(2)=IEVENT
        JAMTSLP='7FFF'X
        DO I=1,2048
          JDRSBLK(I)=JEMPBLK(I)
        END DO
        DO I=1,32                      !Reset packet count
          IMCNT(I)=0
        END DO
        IMTOT=0
        LBLKOPN=.TRUE.
      END IF
      RETURN
      END
!*******************************************************************************
      SUBROUTINE CVT_BCD(CSTR,JWRD)
!
! Converts the specified 3 digit string to a BCD number in the bottom 12
! bits of an I*2.  Used to convert flight number to DRS format.
!
      IMPLICIT  NONE
      INTEGER*2 JWRD
      CHARACTER CSTR*3
!
      JWRD=0
      CALL MVBITS(ICHAR(CSTR(1:1))-48,0,4,JWRD,8)
      CALL MVBITS(ICHAR(CSTR(2:2))-48,0,4,JWRD,4)
      CALL MVBITS(ICHAR(CSTR(3:3))-48,0,4,JWRD,0)
      RETURN
      END 
!*******************************************************************************
      SUBROUTINE WATCHDOG
!
! This routine is used because FORTRAN routines do/did not call themselves
!
      CALL WATCHDOG1
      RETURN
      END
!*******************************************************************************
      SUBROUTINE PUT_CHARSTR(JSTRING,JWORD)
      INTEGER*2 JSTRING,JWORD
      JWORD=JSTRING
      RETURN
      END
!*******************************************************************************
      SUBROUTINE WATCHDOG1
!
! Watchdog routine, called asynchronously every second.  Increments IDRSACT to
! show that the DRS program is running.
!
      IMPLICIT  NONE
      INTEGER*4 ISTAT,ITIMINT(2),I,IFIRST
      INCLUDE   'DRS_GDEF.FOR'
      INCLUDE   'DRS_LDEF.FOR'
      INCLUDE   '($SYSSRVNAM)'
      EXTERNAL  WATCHDOG
      DATA      IFIRST /0/

      IF(IFIRST.EQ.0) THEN
        ISTAT=SYS$BINTIM('0000 00:00:01.00',ITIMINT) !Use 1s watchdog timeout
        IFIRST=1
      ELSE
        DO I=1,33
          IWATCHD(I)=IWATCHD(I)-1
        END DO
      END IF
      ISTAT=SYS$SETIMR(,ITIMINT,WATCHDOG,,) !Set timer for 1s
      DO I=1,32
        IF(IDLUSEQ(I).EQ.1) THEN
          IDLUSTS(I)=1                 !Waiting init load
        ELSE IF(IDLUSEQ(I).EQ.2) THEN
          IDLUSTS(I)=2                 !Init load failed
        ELSE IF(IWATCHD(I).LE.0.OR.IDLUSEQ(I).EQ.0) THEN
          IDLUSTS(I)=0                 !No packets
        ELSE IF(IDLUSEQ(I).EQ.3) THEN
          IF(ILSTPK4(I).EQ.0) THEN
            IDLUSTS(I)=3               !No data packets
            JDRSTAT(513+I)=IBCLR(JDRSTAT(513+I),3)
          ELSE IF (ILSTPK4(I).EQ.1) THEN
            IDLUSTS(I)=5               !Sending data
            JDRSTAT(513+I)=IBSET(JDRSTAT(513+I),3)
          ELSE IF(ILSTPK4(I).EQ.2) THEN
            IDLUSTS(I)=4 !Clock not init
          END IF
        ELSE
          IDLUSTS(I)=9                 !Unknown status
        END IF
        ILSTPK4(I)=0
      END DO
      IF(IWATCHD(33).LE.0) IDLUSTS(33)=0 !AMTG not sending packets
      IF(IWATCHD(33).GT.0) IDLUSTS(33)=1 !AMTG sending packets
      IF(IWATCHD(33).LE.0) CALL MVBITS(0,0,8,JDRSTAT(513),0)
      IF(CFLTNUM.EQ.' ') THEN
        CALL ST_MESS('Waiting for flight number from operator')
      ELSE IF(IDATHLD.EQ.1) THEN
        CALL ST_MESS('Data is on HOLD')
      ELSE
        CALL ST_MESS('Running')
      END IF
      RETURN
      END
!*******************************************************************************
      SUBROUTINE CLOCKSET(JPKT)
!
! Set HORACE clock to match AMTG time, if necessary.  This routine is called
! every time an AMTG time packet is received by HORACE.  Note that VMS clock
! can be set to 100ns and counts 100ns intervals since 17/11/1858.
!
! 1. Doesn't start NTP server until have set HORACE from AMTG.
! 2. Computes diff between HORACE and AMTG.
! 3. Takes the smallest diff in any 20s period, and uses this to adjust
!    HORACE clock if more than 1.5 ms.
! 4. Can add arbitrary time offset based on practical tests.
! 5. Start NTP if necessary after HORACE time is first set.
!
! Note that OPER privilege is required to set the clock.
! Note that this routine needs to run at high priority for best results.
! Note that this routine keeps HORACE aligned with AMTG time not GPS.
! Note that the AMTG packet is sent out at the beginning of each second, and
!  the information it contains is for the previous second.  In particular the
!  time the packet is received will be at least 1s later than the time in the 
!  packet.
!
! Note that HORACE time is updated by a 1024 Hz clock, in 9765 increments.
!
! Note that HORACE clock only has a resolution of about 1.25 ms, so no point
! in trying to keep it accurate to better than this.  In practice it is 
! probably reasonable to expect this routine to keep the clock accurate to
! +-2 ms.
!
! Note that HORACE/AMTG differences arise from:
!
! 1. AMTG drifts (small)
! 2. HORACE drifts (up to 50 ppm or 1 ms in 20s)
! 3. Variable software and transmission delays before this routine is reached
! 4. HORACE clock missing counts because of higher level interrupts
! 5. Leap ticks, because the HORACE clock runs at 1024 Hz not 1000 Hz,
!    about once every 20s.
!
! Choosing the smallest difference in every 20s period provides the most
! consistent measure of the difference.
!
      INTEGER*8  IHTIM,IATIM,ITEMP,IMINVAL,IMIN,ILSTMIN
      INTEGER*4  ISTAT,IOS,ISPM,IOFFSET,INTP,LIB$SPAWN
      INTEGER*4  SYS$GETTIM,SYS$SETIME,SYS$BINTIM,IC
      INTEGER*2  JPKT(750),JDATE(3),JSPM(2)
      CHARACTER  CMON(12)*3,CTIME*23,CDIFF*12,CVAL*4
      EQUIVALENCE (ISPM,JSPM)
      DATA CMON /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP',
     &    'OCT','NOV','DEC'/
      DATA IOFFSET /0/                 !100ns time correction based on tests
      DATA INTP /0/                    !0 = NTP not started, 1 = NTP started
      DATA IC /1/
      DATA ILSTMIN /0/
      DATA IMIN /99999999999/
!
! Check time is valid
!
      IF(JPKT(2).EQ.0) RETURN          !Not init
!
! Get HORACE time
!
      ISTAT=SYS$GETTIM(IHTIM)          !Get HORACE time
      IF(.NOT.ISTAT) RETURN
!
! Convert AMTG time to VMS binary time (100ns past 17/11/1858)
!
      JSPM(1)=JPKT(3)
      JSPM(2)=JPKT(4)
      CALL SWAP_ENDS32(ISPM,1)
      JDATE(1)=JPKT(5)                 !Day
      JDATE(2)=JPKT(6)                 !Month
      JDATE(3)=JPKT(7)                 !Year
      CALL SWAP_ENDS16(JDATE,3)
      IF(JDATE(1).LT.1.OR.JDATE(1).GT.31) RETURN
      IF(JDATE(2).LT.1.OR.JDATE(2).GT.12) RETURN
      IF(JDATE(3).LT.2001.OR.JDATE(3).GT.2099) RETURN
      CTIME='dd-mmm-yyyy 00:00:00.00'
      WRITE(CTIME(1:11),'(I2.2,A,I4.4)',IOSTAT=IOS) JDATE(1),
     &    '-'//CMON(JDATE(2))//'-',JDATE(3)
      ISTAT=SYS$BINTIM(CTIME,IATIM)
      IF(.NOT.ISTAT) THEN
        CALL LOG_MESS('SYS$BINTIM failed on '//CTIME)
        RETURN
      END IF
      ITEMP=ISPM+1                     !Time in packet is for prev sec, so add 1
      ITEMP=ITEMP*10000000
      IATIM=IATIM+ITEMP
!
! Add any empirical offset
!
      IATIM=IATIM+IOFFSET
!
! Work out the smallest difference between HORACE and AMTG time in each 20s
! period and use this to decide when to adjust HORACE time
!
      IF(ABS(IHTIM-IATIM).LT.IMIN) THEN
        IMIN=ABS(IHTIM-IATIM)         
        IMINVAL=IHTIM-IATIM
      END IF
!
      IC=IC+1
      IF(IC.GT.20) THEN
!        TYPE *,IMIN,IMINVAL,IMIN-ILSTMIN
        IF(IMIN.GT.15000) THEN         !Adjust time if 1.5 ms diff
          ISTAT=SYS$GETTIM(IHTIM)
          ISTAT=SYS$SETIME(IHTIM-IMINVAL-10000) !Arbitrary corr for proc time
          WRITE(CDIFF,'(F12.6)',IOSTAT=IOS) FLOAT(IMINVAL)/1E7
!          CALL LOG_MESS('HORACE clock adjusted by '//CDIFF//' s')
          IF(INTP.EQ.0) THEN
            CALL GET_LOGICAL('DRS_NTPSTART',CVAL) 
            IF(CVAL.NE.' ') THEN       !If NTP startup required
              ISTAT=LIB$SPAWN(,'SYS$STARTUP:TCPIP$NTP_STARTUP.COM',,
     &          1+2+4+8,,,,,,,,)     !Spawn with no wait
              IF(.NOT.ISTAT) THEN
                CALL LOG_MESS('NTP spawn failed')
              ELSE
                INTP=1
                CALL LOG_MESS('NTP service started')
              END IF
            ELSE
              INTP=1                   !Don't try again
            END IF
          END IF
        END IF
        IC=1
        ILSTMIN=IMIN
        IMIN=99999999999
      END IF
!
      RETURN
      END
!*******************************************************************************
      SUBROUTINE MBX_CONN
!
! Opens an mailbox connection on DRSTEST_MBX.
!
      INTEGER*4 ISTAT
      INCLUDE   '($SYSSRVNAM)'
      INCLUDE   'DRS_LDEF.FOR'

      ISTAT=SYS$CREMBX(%VAL(1),ICHAN,%VAL(1514),%VAL(32000),,,
     &    'DRSTEST_MBX',,)
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      ISTAT=SYS$CREMBX(%VAL(1),ICHAN1,%VAL(1514),%VAL(32000),,,
     &    'DRSTEST_MBX1',,)
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      CALL LOG_MESS('Mailbox opened on DRSTEST_MBX')
      CALL ST_MESS('Mailbox opened on DRSTEST_MBX')
      JOSB(1)=0
      RETURN
      END
!*******************************************************************************
      SUBROUTINE SET_FN_LOG(CFN)
!
! Sets the logical FLIGHT_NUMBER in the group logical name table with the
! current flight number.  GRPNAM and SYSNAM privileges are required.
!
      CHARACTER CFN*(*)
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
      TLIST.BUFFER_LEN=LEN(CFN)
      TLIST.BUFFER_LOC=%LOC(CFN)
      ISTAT=SYS$CRELNM(,'LNM$GROUP','FLIGHT_NUMBER',PSL$C_SUPER,TLIST)
      RETURN
      END
!*******************************************************************************
      SUBROUTINE BLOCK_TIMER(JPKT)
!
! Called when an AMTG packet has arrived.  Notes the current time and
! schedules the start of a new block in 0.5s time.  Also cancels any new blocks
! that have been scheduled by HORACE in case the AMTG was not running.  Also
! called when program starts in which case starts a 1s timer based on HORACE
! clock (the AMTG will then cancel this request and use its own if it is
! running).
!
      INTEGER*8 INOWTIM,IHAFSEC
      INTEGER*4 ISTAT,SYS$CANTIM,SYS$GETTIM,SYS$BINTIM,LIB$ADD_TIMES,
     &    SYS$SETIMR,IFIRST,ITIM,SYS$ASCTIM,IOS,IH,IM,IS,ISEC
      INTEGER*2 JPKT(750),JTIM(2)
      CHARACTER CDATTIM*23
      INCLUDE   'DRS_LDEF.FOR'
      EQUIVALENCE (ITIM,JTIM)
      EXTERNAL  BLOCK_PROCESS
      DATA      IFIRST /0/

      ISTAT=SYS$CANTIM(%VAL(100),)     !Cancel any AMTG timer just in case
      ISTAT=SYS$CANTIM(%VAL(101),)     !Cancel HORACE default timer
      IF(IFIRST.EQ.0) THEN             !Set end block time on 0.5s boundary
        ISTAT=SYS$ASCTIM(,CDATTIM,,)
        IBLKTIM=SECNDS(0.0)            !Which will be 1.5s before boundary
        ISEC=IBLKTIM+1
        IS=MOD(ISEC,60)
        IM=MOD((ISEC/60),60)
        IH=ISEC/3600
        WRITE(CDATTIM(13:),'(3(I2.2,A))',IOSTAT=IOS)
     &      IH,':',IM,':',IS,'.50'
        ISTAT=SYS$BINTIM(CDATTIM,ICHGTIM)
        ISTAT=SYS$SETIMR(,ICHGTIM,BLOCK_PROCESS,%VAL(101),) !Set AST to nxt .5s
        CTIMTYP='H'
        IFIRST=1
      ELSE
        JTIM(1)=JPKT(3)
        JTIM(2)=JPKT(4)
        CALL SWAP_ENDS32(ITIM,1)
        IF(ITIM.GE.0.AND.ITIM.LT.36*3600) THEN !Very simple quality check
          ISTAT=SYS$GETTIM(INOWTIM)      
          ISTAT=SYS$BINTIM('0000 00:00:00.50',IHAFSEC)
          ISTAT=LIB$ADD_TIMES(INOWTIM,IHAFSEC,ICHGTIM)
          ISTAT=SYS$SETIMR(,ICHGTIM,BLOCK_PROCESS,%VAL(100),) !Set AST for 0.5s
          CTIMTYP='A'
          IBLKTIM=ITIM                 !DRS time of current block
        END IF
      END IF
      RETURN
      END
!*******************************************************************************
      SUBROUTINE BLOCK_PROCESS
      CALL BLOCK_PROCESS1
      RETURN
      END
!*******************************************************************************
      SUBROUTINE BLOCK_PROCESS1
!
! Called half a HORACE second after an AMTG pulse or one HORACE second after a
! previous block_process call.  At the time this routine is called ICHGTIM
! contains the time used to schedule the call.  This ensures that the next
! call can be exactly one HORACE second after the previous one regardless of
! processing times and delays.
!
      INTEGER*8 IONESEC,INXTIM
      INTEGER*4 ISTAT,SYS$BINTIM,LIB$ADD_TIMES,SYS$SETIMR
      INCLUDE   'DRS_LDEF.FOR'
      EXTERNAL  BLOCK_PROCESS

      CALL NEXT_BLOCK('END')
      ISTAT=SYS$BINTIM('0000 00:00:01.00',IONESEC)
      ISTAT=LIB$ADD_TIMES(ICHGTIM,IONESEC,INXTIM)
      ICHGTIM=INXTIM
      ISTAT=SYS$SETIMR(,ICHGTIM,BLOCK_PROCESS,%VAL(101),) !Set AST in 1s
      CTIMTYP='H'
      IBLKTIM=IBLKTIM+1
      RETURN
      END
