!
! DRS_SUBS.FOR - Utility subroutines used by DRS and DRS_CONTROL, for handling
! DRS functions and configuration files.
!
! DRS_ADDRESSES_CHANGE Spawns editor to change contents of DRS_ADDRESSES.DAT
! DRS_ADDRESSES_CHECK  Checks contents of DRS_ADDRESSES.DAT are valid
! DRS_ADDRESSES_PRINT  Prints contents of DRS_ADDRESSES.DAT
! DRS_ADDRESSES_READ   Reads contents of DRS_ADDRESSES.DAT into arrays
! DRS_ADDRESSES_SHOW   Displays contents of DRS_ADDRESSES.DAT
!
! DRS_PARAMETERS_ADD   Adds new parameter to DRS_PARAMETERS.DAT
! DRS_PARAMETERS_PRINT Prints conents of DRS_PARAMETERS.DAT
! DRS_PARAMETERS_READ  Reads contents of DRS_PARAMETERS.DAT into arraygs
! DRS_PARAMETERS_SHOW  Displays contents of DRS_PARAMETERS.DAT
! DRS_PARAMETERS_SORT  Sorts and tidies DRS_PARAMETERS.DAT
!
! DRS_ISS_ADD          Marks a parameter for addition to ISS
! DRS_ISS_APPLY        Adds new ISS with the marked changes
! DRS_ISS_BLOCK        Builds a DRS ISS block
! DRS_ISS_CHANGE       Handles all options to change an ISS
! DRS_ISS_CREATE       Creates a DRS_ISS.DAT file from HORACE ISS_nn.DAT files
! DRS_ISS_PRINT        Prints contents of latest ISS
! DRS_ISS_READ         Reads latest ISS into arrays
! DRS_ISS_REMOVE       Marks a parameter for removal from ISS
! DRS_ISS_SHOW         Displays contents of latest ISS
!
! DRS_CONFIG_CHANGE    Spawns editor to change contents of DRS_CONFIG.DAT
! DRS_CONFIG_CHECK     Checks consistency of DRS_CONFIG.DAT
! DRS_CONFIG_PRINT     Prints contents of DRS_CONFIG.DAT
! DRS_CONFIG_SHOW      Displays contents of DRS_CONFIG.DAT
! DRS_CONFIG_SORT      Sorts and tidies DRS_CONFIG.DAT
!
! ADDR_STRING          Converts 6 byte Ethernet address to 17 character string
! DATE1                Returns date as dd-mmm-yy
! GET_LOGICAL          Returns translated logical name
! LAN                  Returns length of string less trailing blanks
! STR_COMPRESS         Removes leading, multiple spaces and tabs on a line
! STRING_ADDR          Converts 17 character Ethernet string to 6 byte address
! SWAP_ENDS16          Reverses byte order of 16 bit words
! SWAP_ENDS32          Reverses byte order of 32 bit words
!
! Dependencies:
!
! 1. Logical name DRS_CONTROL_PRINTER must be defined as the required printer
!    name.
! 2. Logical name HORACE must be defined as the directory containing the DRS
!    configuration files.
! 3. LUN 6 is used for all output.
! 4. Contraints:
!       Max 32 DLUs
!       Max 512 parameters
!       Max 16 ports per card
!       Max 16 cards per DLU
!       Card addresses 8 to 253
!
! V1.00  12/12/00  W D N JACKSON
!
!*******************************************************************************
      SUBROUTINE DRS_ADDRESSES_CHANGE
!
! Runs the editor to make changes to the DRS_ADDRESSES.DAT file, and checks
! that the new file is valid.
!
      IMPLICIT  NONE
      INTEGER*4 ISTAT,LIB$SPAWN,ISTS,IOS
      CHARACTER CLINE*80

      WRITE(6,'(A)') ' '
      WRITE(6,'(A)') ' Please wait...'
      OPEN(11,FILE='HORACE:DRS_CONTROL.TMP',STATUS='NEW',
     &    CARRIAGECONTROL='LIST')
      WRITE(11,'(A)') '$ DEFINE /USER SYS$INPUT SYS$COMMAND'
      WRITE(11,'(A)') '$ EDIT /TPU /INIT=HORACE:EVEINI.TPU '//
     &    'HORACE:DRS_ADDRESSES.DAT'
      CLOSE(11)
      CLINE='@HORACE:DRS_CONTROL.TMP'
      ISTAT=LIB$SPAWN(CLINE,,,,,,,,,,,)
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      WRITE(6,'(A)') ' '
      OPEN(11,FILE='HORACE:DRS_CONTROL.TMP',STATUS='OLD')
      CLOSE(11,DISPOSE='DELETE')
      CALL DRS_ADDRESSES_CHECK(ISTS)
      IF(ISTS.NE.1) THEN
        WRITE(6,'(A)') ' '
        WRITE(6,'(A)') '$Press any key to continue'
        READ(5,'(A)',IOSTAT=IOS) CLINE
      END IF
      RETURN
      END
!*******************************************************************************
      SUBROUTINE DRS_ADDRESSES_CHECK(ISTS)
!
! Checks that Ethernet addresses in the DRS_ADDRESSES.DAT are correctly 
! specified.  Returns 1 if no problems, else 0.  No duplicate names or
! addresses, max 32 addresses, no blank lines, all addresses start
! AA-00-04-00-00-.
!
      IMPLICIT  NONE
      INTEGER*4 IOS,IL,LAN,IC,I,ISTS,ISTS1
      INTEGER*2 JSOURCE(3)
      CHARACTER CLINE*80,CLINE1*80,CNAME*6,CEADD*17
      CHARACTER CEADDR(32)*17,CNAMES(32)*6,CFILE*80

      ISTS=1
      CFILE='HORACE:DRS_ADDRESSES.DAT'
      OPEN(11,FILE=CFILE,STATUS='OLD',READONLY,IOSTAT=IOS)
      IF(IOS.NE.0) THEN
        WRITE(6,'(A)') ' Unable to open '//CFILE(1:LAN(CFILE))
        ISTS=0
        RETURN
      END IF
      IL=1
      IC=0
      READ(11,'(A)',IOSTAT=IOS) CLINE
      DO WHILE(IOS.EQ.0)
        IF(CLINE(1:1).EQ.'!') THEN
        ELSE IF(CLINE.NE.' ') THEN
          CALL STR$UPCASE(CLINE1,CLINE)
          CALL STR_COMPRESS(CLINE1)
          CALL STR$ELEMENT(CEADD,0,' ',CLINE1)
          CALL STR$ELEMENT(CNAME,1,' ',CLINE1)
          CALL STRING_ADDR(CEADD,JSOURCE,ISTS1)
!          IF(CEADD(1:15).NE.'AA-00-04-00-00-'.OR.CLINE1(16:17).LT.'00'
          IF(CLINE1(16:17).LT.'00'
     &        .OR.CLINE1(16:17).GT.'FF'.OR.ISTS1.NE.1) THEN
            WRITE(6,'(A,I3,A)') 
     &          ' LINE ',IL,' - Invalid Ethernet address'
            WRITE(6,'(A)') ' '//CLINE(1:LAN(CLINE))
            WRITE(6,'(A)') ' '
            ISTS=0
          END IF
          IF(CNAME.EQ.' ') THEN
            WRITE(6,'(A,I3,A)') ' LINE ',IL,' - DLU name missing'
            WRITE(6,'(A)') ' '//CLINE(1:LAN(CLINE))
            WRITE(6,'(A)') ' '
            ISTS=0
          END IF
          IF(IC.EQ.32) THEN            !Too many addresses/DLUs
            WRITE(6,'(A,I3,A)') ' LINE ',IL,' - More than 32 addresses'
            WRITE(6,'(A)') ' '//CLINE(1:LAN(CLINE))
            WRITE(6,'(A)') ' '
            ISTS=0
          END IF
          IF(IC.GE.1.AND.IC.LT.32) THEN
            DO I=1,IC
              IF(CEADD.EQ.CEADDR(I)) THEN !Duplicate address
                WRITE(6,'(A,I3,A)') ' LINE ',IL,' - Duplicate address'
                WRITE(6,'(A)') ' '//CLINE(1:LAN(CLINE))
                WRITE(6,'(A)') ' '
                ISTS=0
              END IF
              IF(CNAME.EQ.CNAMES(I)) THEN !Duplicate name
                WRITE(6,'(A,I3,A)') ' LINE ',IL,' - Duplicate name'
                WRITE(6,'(A)') ' '//CLINE(1:LAN(CLINE))
                WRITE(6,'(A)') ' '
                ISTS=0
              END IF
            END DO
          END IF
          IF(IC.LT.32) THEN
            IC=IC+1
            CEADDR(IC)=CEADD
            CNAMES(IC)=CNAME
          END IF
        END IF
        READ(11,'(A)',IOSTAT=IOS) CLINE
        IL=IL+1
      END DO
      CLOSE(11,IOSTAT=IOS)
      IF(IC.EQ.0) THEN                 !No addresses
        WRITE(6,'(A)') ' No Ethernet addresses specified'
        ISTS=0
      END IF
      RETURN
      END
!*******************************************************************************
      SUBROUTINE DRS_ADDRESSES_PRINT
!
! Prints all non-comment lines in the DRS_ADDRESSES.DAT file to the
! printer defined by the logical DRS_CONTROL_PRINTER.
!
      IMPLICIT  NONE
      INTEGER*4 IOS,LAN,LIB$SPAWN,ISTAT
      CHARACTER CLINE*80

      WRITE(6,'(A)') ' '
      CALL GET_LOGICAL('DRS_CONTROL_PRINTER',CLINE)
      IF(CLINE.EQ.' ') THEN
        WRITE(6,'(A)')
     &      ' Logical name DRS_CONTROL_PRINTER must define printer'
        WRITE(6,'(A)') ' '
        WRITE(6,'(A)') '$Press any key to continue'
        READ(5,'(A)',IOSTAT=IOS) CLINE
        RETURN
      END IF
      WRITE(6,'(A)') ' Please wait...'
      OPEN(12,FILE='HORACE:DRS_CONTROL.TMP',STATUS='NEW',
     &    CARRIAGECONTROL='LIST')
      WRITE(12,'(A)') 'DLU Ethernet addresses'
      WRITE(12,'(A)') ' '
      OPEN(11,FILE='HORACE:DRS_ADDRESSES.DAT',STATUS='OLD',READONLY)
      READ(11,'(A)',IOSTAT=IOS) CLINE
      DO WHILE(IOS.EQ.0)
        IF(CLINE(1:1).NE.'!') WRITE(12,'(A)') CLINE(1:LAN(CLINE))
        READ(11,'(A)',IOSTAT=IOS) CLINE
      END DO
      CLOSE(11)
      WRITE(12,'(A)') ' '
      CLINE=' '
      CALL DATE1(CLINE(1:9))
      CALL TIME(CLINE(12:19))
      WRITE(12,'(A)') CLINE(1:LAN(CLINE))
      CLOSE(12)
      WRITE(6,'(A)') ' '
      CLINE='PRIN/QUEU=DRS_CONTROL_PRINTER/DELE '//
     &    'HORACE:DRS_CONTROL.TMP;'
      ISTAT=LIB$SPAWN(CLINE,,,,,,,,,,,)
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      WRITE(6,'(A)') ' '
      WRITE(6,'(A)') '$Press any key to continue'
      READ(5,'(A)',IOSTAT=IOS) CLINE
      RETURN
      END
!*******************************************************************************
      SUBROUTINE DRS_ADDRESSES_READ(CEADD,CNAME,IC)
!
! Reads the DRS_ADDRESSES.DAT file and returns its contents.  CEADD is array of
! 17 char addresses, CNAME is array of 6 char names, IC is no of addresses.
!
      IMPLICIT  NONE
      INTEGER*4 IOS,IC
      CHARACTER CEADD(32)*17,CNAME(32)*6,CLINE*80

      DO IC=1,32
        CEADD(IC)=' '
        CNAME(IC)=' '
      END DO
      IC=0
      OPEN(11,FILE='HORACE:DRS_ADDRESSES.DAT',STATUS='OLD',READONLY)
      READ(11,'(A)',IOSTAT=IOS) CLINE
      DO WHILE(IOS.EQ.0)
        IF(CLINE(1:1).NE.'!') THEN
          IC=IC+1
          IF(IC.GT.32) THEN
            WRITE(6,'(A)') 
     &          ' DRS_ADDRESSES.DAT contains more than 32 addresses'
            CLOSE(11)
            CALL EXIT
          END IF
          CEADD(IC)=CLINE(1:17)
          CNAME(IC)=CLINE(19:24)
        END IF
        READ(11,'(A)',IOSTAT=IOS) CLINE
      END DO
      CLOSE(11,IOSTAT=IOS)
      RETURN
      END
!*******************************************************************************
      SUBROUTINE DRS_ADDRESSES_SHOW
!
! Read the DRS_ADDRESSES.DAT file and displays all non-comment lines on the
! screen.
!
      IMPLICIT  NONE
      INTEGER*4 IOS,LAN,ISTS
      CHARACTER CLINE*80

      WRITE(6,'(A)') CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H'
      WRITE(6,'(A)') ' DLU Ethernet addresses'
      WRITE(6,'(A)') ' '
      OPEN(11,FILE='HORACE:DRS_ADDRESSES.DAT',STATUS='OLD',READONLY)
      READ(11,'(A)',IOSTAT=IOS) CLINE
      DO WHILE(IOS.EQ.0)
        IF(CLINE(1:1).NE.'!') WRITE(6,'(X,A)') CLINE(1:LAN(CLINE))
        READ(11,'(A)',IOSTAT=IOS) CLINE
      END DO
      CLOSE(11,IOSTAT=IOS)
      WRITE(6,'(A)') ' '
      CALL DRS_ADDRESSES_CHECK(ISTS)
      IF(ISTS.NE.1) WRITE(6,'(A)') ' '
      WRITE(6,'(A)') '$Press any key to continue'
      READ(5,'(A)',IOSTAT=IOS) CLINE
      RETURN
      END
!*******************************************************************************
      SUBROUTINE DRS_PARAMETERS_ADD
!
! Adds a new parameter to the parameter file.  Note that if a parameter's
! frequency is changed, then a new parameter will need to be defined.
!
      IMPLICIT  NONE
      INTEGER*4 IP,IF,IFREQ(512),IOS,I,ISTAT,LAN
      CHARACTER CNAME(512)*4,CLINE*80,CTEMP*4

      CALL DRS_PARAMETERS_READ(IFREQ,CNAME)
!
      ISTAT=0
      WRITE(6,'(A)') ' '
      DO WHILE(ISTAT.EQ.0)
        ISTAT=1
        WRITE(6,'(A)') '$New parameter number: '
        READ(5,'(A)',IOSTAT=IOS) CLINE
        IF(IOS.LT.0) RETURN
        READ(CLINE,'(I)',IOSTAT=IOS) IP
        IF(IOS.NE.0) ISTAT=0
        IF(IP.LT.1.OR.IP.GT.512) ISTAT=0
        IF(ISTAT.EQ.1) THEN
          IF(IFREQ(IP).NE.0) THEN
            WRITE(6,'(A)') ' Parameter '//CLINE(1:LAN(CLINE))//
     &          ' already exists'
            ISTAT=0
          END IF
        END IF
      END DO
!
      ISTAT=0
      DO WHILE(ISTAT.EQ.0)
        ISTAT=1
        WRITE(6,'(A)') '$New parameter frequency: '
        READ(5,'(A)',IOSTAT=IOS) CLINE
        IF(IOS.LT.0) RETURN
        READ(CLINE,'(I)',IOSTAT=IOS) IF
        IF(IOS.NE.0) ISTAT=0
        IF(ISTAT.EQ.1) THEN            !Check power of 2 between 1 and 256
          ISTAT=0
          DO I=1,9
            IF(IF.EQ.2**(I-1)) ISTAT=1
          END DO
          IF(ISTAT.EQ.0) WRITE(6,'(A)') 
     &       ' Parameter frequency must be power of 2 between 1 and 256'
        END IF
      END DO
!
      ISTAT=0
      DO WHILE(ISTAT.EQ.0)
        ISTAT=1
        WRITE(6,'(A)') '$New parameter name: '
        READ(5,'(A)',IOSTAT=IOS) CLINE
        IF(IOS.LT.0) RETURN
        CALL STR$UPCASE(CLINE,CLINE)
        CALL STR_COMPRESS(CLINE)
        IF(CLINE(1:4).EQ.' ') THEN
          WRITE(6,'(A)') ' Parameter name is blank'
          ISTAT=0
        ELSE
          DO I=1,512
            IF(CNAME(I).EQ.CLINE(1:4)) THEN
              WRITE(6,'(A)') ' Parameter name '//CLINE(1:4)//
     &            ' already exists'
              ISTAT=0
            END IF
          END DO
        END IF
        CTEMP=CLINE(1:4)
      END DO
!
      OPEN(11,FILE='HORACE:DRS_PARAMETERS.DAT',STATUS='OLD',
     &    ACCESS='APPEND')
      WRITE(11,'(I4,I4,X,A)') IP,IF,CTEMP
      CLOSE(11)
      CALL DRS_PARAMETERS_SORT         !Tidy up and sort file
!
      WRITE(6,'(A)') ' '
      WRITE(CLINE,'(A,I3,A,I3,A)') 
     &    'Parameter ',IP,' '//CTEMP//' added at ',IF,' Hz'
      CALL STR_COMPRESS(CLINE)
      WRITE(6,'(X,A)') CLINE
      WRITE(6,'(A)') ' '
      WRITE(6,'(A)') '$Press any key to continue'
      READ(5,'(A)',IOSTAT=IOS) CLINE
      RETURN
      END
!*******************************************************************************
      SUBROUTINE DRS_PARAMETERS_PRINT
!
! Prints the contents of the DRS_PARAMETERS.DAT file
!
      IMPLICIT  NONE
      INTEGER*4 IFREQ(512),I,IPNUM,IPLIST(512),NPLINES,NFLINES,IL,IOFF,
     &    IOS,NLINES,IPFREQ(512),ISTAT,LIB$SPAWN,LAN
      CHARACTER CNAME(512)*4,CLINE*80,CPNAME(512)*4

      WRITE(6,'(A)') ' '
      CALL GET_LOGICAL('DRS_CONTROL_PRINTER',CLINE)
      IF(CLINE.EQ.' ') THEN
        WRITE(6,'(A)')
     &      ' Logical name DRS_CONTROL_PRINTER must define printer'
        WRITE(6,'(A)') ' '
        WRITE(6,'(A)') '$Press any key to continue'
        READ(5,'(A)',IOSTAT=IOS) CLINE
        RETURN
      END IF
      WRITE(6,'(A)') ' Please wait...'
      OPEN(12,FILE='HORACE:DRS_CONTROL.TMP',STATUS='NEW')
      CALL DRS_PARAMETERS_READ(IFREQ,CNAME)
      IPNUM=0
      DO I=1,512
        IF(IFREQ(I).NE.0) THEN
          IPNUM=IPNUM+1
          IPLIST(IPNUM)=I
          IPFREQ(IPNUM)=IFREQ(I)
          CPNAME(IPNUM)=CNAME(I)
        END IF
      END DO
      WRITE(12,'(A)') ' '
      WRITE(12,'(A)') ' Defined DRS parameters'
      WRITE(12,'(A)') ' '
      NLINES=(IPNUM-1)/4+1           !No of lines to be written
      NPLINES=NLINES*4-IPNUM         !No of part lines
      NFLINES=NLINES-NPLINES         !No of full lines
      IOFF=NLINES
      DO IL=1,NFLINES                !For each full line
        WRITE(12,11) ((IPLIST(I),IPFREQ(I),CPNAME(I)),
     &      I=IL,IL+3*IOFF,IOFF)
      END DO
      IF(NPLINES.NE.0) THEN
        DO IL=NFLINES+1,NLINES       !For each part line
          WRITE(12,11) ((IPLIST(I),IPFREQ(I),CPNAME(I)),
     &      I=IL,IL+2*IOFF,IOFF)
        END DO
      END IF
      WRITE(12,'(A)') ' '
      CLINE=' '
      CALL DATE1(CLINE(1:9))
      CALL TIME(CLINE(12:19))
      WRITE(12,'(X,A)') CLINE(1:LAN(CLINE))
      CLOSE(12)
      WRITE(6,'(A)') ' '
      CLINE='PRIN/QUEU=DRS_CONTROL_PRINTER/DELE '//
     &    'HORACE:DRS_CONTROL.TMP;'
      ISTAT=LIB$SPAWN(CLINE,,,,,,,,,,,)
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      WRITE(6,'(A)') ' '
      WRITE(6,'(A)') '$Press any key to continue'
      READ(5,'(A)',IOSTAT=IOS) CLINE
      RETURN
11    FORMAT(X,4(I3,X,I3,X,A,10X))
      END
!*******************************************************************************
      SUBROUTINE DRS_PARAMETERS_READ(IFREQ,CNAME)
!
! Reads data from the DRS_PARAMETERS.DAT file and returns the data in 512
! element arrays.  If a parameter does not exist returns 0 for its frequency
! and blanks for its name.  Assumes the data file only contains valid entries.
!
      IMPLICIT  NONE
      INTEGER*4 IFREQ(512),IOS,IP,IF
      CHARACTER CNAME(512)*4,CNAM*4

      DO IP=1,512
        IFREQ(IP)=0
        CNAME(IP)='    '
      END DO
      OPEN(11,FILE='HORACE:DRS_PARAMETERS.DAT',STATUS='OLD')
      READ(11,'(I4,I4,X,A)',IOSTAT=IOS) IP,IF,CNAM
      DO WHILE(IOS.EQ.0)
        IFREQ(IP)=IF
        CNAME(IP)=CNAM
        READ(11,'(I4,I4,X,A)',IOSTAT=IOS) IP,IF,CNAM
      END DO
      CLOSE(11)
      RETURN
      END
!*******************************************************************************
      SUBROUTINE DRS_PARAMETERS_SHOW
!
! Lists the contents of the DRS_PARAMETERS.DAT file
!
      IMPLICIT  NONE
      INTEGER*4 IFREQ(512),I,IPNUM,IPLIST(512),NPLINES,NFLINES,IL,IOFF,
     &    IOS,NLINES,IPFREQ(512)
      CHARACTER CNAME(512)*4,CLINE*80,CPNAME(512)*4

      CALL DRS_PARAMETERS_READ(IFREQ,CNAME)
      IPNUM=0
      DO I=1,512
        IF(IFREQ(I).NE.0) THEN
          IPNUM=IPNUM+1
          IPLIST(IPNUM)=I
          IPFREQ(IPNUM)=IFREQ(I)
          CPNAME(IPNUM)=CNAME(I)
        END IF
      END DO
      WRITE(6,'(A)') ' '
      WRITE(6,'(A)') ' Defined DRS parameters'
      WRITE(6,'(A)') ' '
      NLINES=(IPNUM-1)/4+1           !No of lines to be written
      NPLINES=NLINES*4-IPNUM         !No of part lines
      NFLINES=NLINES-NPLINES         !No of full lines
      IOFF=NLINES
      DO IL=1,NFLINES                !For each full line
        WRITE(6,11) ((IPLIST(I),IPFREQ(I),CPNAME(I)),
     &      I=IL,IL+3*IOFF,IOFF)
      END DO
      IF(NPLINES.NE.0) THEN
        DO IL=NFLINES+1,NLINES       !For each part line
          WRITE(6,11) ((IPLIST(I),IPFREQ(I),CPNAME(I)),
     &      I=IL,IL+2*IOFF,IOFF)
        END DO
      END IF
      WRITE(6,'(A)') ' '
      WRITE(6,'(A)') '$Press any key to continue'
      READ(5,'(A)',IOSTAT=IOS) CLINE
      RETURN
11    FORMAT(X,4(I3,X,I3,X,A,10X))
      END
!*******************************************************************************
      SUBROUTINE DRS_PARAMETERS_SORT
!
! Tidies up and sorts into parameter order the DRS_PARAMETERS.DAT file.
! Assumes all entries are valid.  Writes message if contents invalid.
!
      IMPLICIT  NONE
      INTEGER*4 I,IOS,IFREQ(512),IPARA,IOS1,IOS2
      CHARACTER CLINE*80,CPARA*4,CFREQ*4,CNAME(512)*4

      OPEN(11,FILE='HORACE:DRS_PARAMETERS.DAT',STATUS='OLD')
      READ(11,'(A)',IOSTAT=IOS) CLINE
      DO I=1,512
        IFREQ(I)=0
      END DO
      DO WHILE(IOS.EQ.0)
        CALL STR$UPCASE(CLINE,CLINE)
        CALL STR_COMPRESS(CLINE)
        CALL STR$ELEMENT(CPARA,0,' ',CLINE)
        CALL STR$ELEMENT(CFREQ,1,' ',CLINE)
        READ(CPARA,'(BN,I)',IOSTAT=IOS1) IPARA               
        READ(CFREQ,'(BN,I)',IOSTAT=IOS2) IFREQ(IPARA)
        IF(IOS1.NE.0.OR.IOS2.NE.0) THEN
          WRITE(6,'(A)') ' Contents of DRS_PARAMETERS.DAT are invalid'
          CLOSE(11)
          CALL EXIT
        END IF
        CALL STR$ELEMENT(CNAME(IPARA),2,' ',CLINE)
        READ(11,'(A)',IOSTAT=IOS) CLINE
      END DO
      REWIND(11)
      DO I=1,512
        IF(IFREQ(I).GT.0) THEN
          WRITE(11,'(I4,I4,X,A)') I,IFREQ(I),CNAME(I)
        END IF
      END DO
      CLOSE(11,IOSTAT=IOS)
      RETURN
      END
!*******************************************************************************
      SUBROUTINE DRS_ISS_ADD(BLIST)
!
! Flags the addition of an ISS parameter
!
      IMPLICIT  NONE
      INTEGER*4 IFREQ(512),ISS,LAN,IP,IOS,I,ICNT
      BYTE      BLIST(512),BBUFF(512)
      CHARACTER CNAME(512)*4,CLINE*80

      CALL DRS_ISS_READ(BBUFF,ISS)
      CALL DRS_PARAMETERS_READ(IFREQ,CNAME)
10    WRITE(6,'(A)') ' '
      WRITE(CLINE,'(A,I3,A)') 
     &    'Parameter to add to ISS ',ISS,' (CTRL/Z when done): '
      CALL STR_COMPRESS(CLINE)
      WRITE(6,'(A)') '$'//CLINE(1:LAN(CLINE))//' '
      READ(5,'(A)',IOSTAT=IOS) CLINE
      IF(IOS.LT.0) RETURN
      WRITE(6,'(A)') ' '
      READ(CLINE,'(I)',IOSTAT=IOS) IP
      IF(IOS.NE.0) THEN
        WRITE(6,'(A)') ' Invalid entry'
      ELSE IF(IP.LT.1.OR.IP.GT.512) THEN
        WRITE(6,'(A)') ' Invalid parameter number'
      ELSE IF(BBUFF(IP).EQ.1) THEN
        WRITE(6,'(A)') ' Parameter is already present in current ISS'
      ELSE IF(BLIST(IP).EQ.1) THEN
        WRITE(6,'(A)') ' Parameter is already flagged for adding'
      ELSE IF(IFREQ(IP).EQ.0) THEN
        WRITE(6,'(A)') ' Parameter is not defined in parameter list'
      ELSE
        ICNT=0
        DO I=1,512
          IF(BBUFF(I).EQ.1) ICNT=ICNT+IFREQ(I)
          IF(BLIST(I).EQ.-1) ICNT=ICNT-IFREQ(I)
          IF(BLIST(I).EQ.1) ICNT=ICNT+IFREQ(I)
        END DO
        ICNT=ICNT+IFREQ(IP)
        IF(ICNT.GT.2040) THEN
          WRITE(6,'(A)') ' ISS would exceed 2048 words'
        ELSE
          BLIST(IP)=1
          WRITE(CLINE,'(A,I4,A,I4)') 
     &      ' Parameter',IP,' has been flagged for addition to ISS',ISS
          CALL STR_COMPRESS(CLINE)
          WRITE(6,'(X,A)') CLINE(1:LAN(CLINE))
        END IF
      END IF  
      GOTO 10
      END
!*******************************************************************************
      SUBROUTINE DRS_ISS_APPLY(BLIST)
!
! Creates a new ISS with the required changes.
!
      IMPLICIT  NONE
      INTEGER*4 IFREQ(512),ISS,LAN,I,IP,IF,IOS,ICNT
      BYTE      BLIST(512),BBUFF(512)
      CHARACTER CNAME(512)*4,CLINE*80,CTEMP*4

      WRITE(6,'(A)') ' '
      CALL DRS_ISS_READ(BBUFF,ISS)
      ICNT=0
      DO I=1,512
        IF(BLIST(I).NE.0) ICNT=ICNT+1
      END DO
      IF(ICNT.EQ.0) THEN
        WRITE(CLINE,'(A,I3)') 
     &      'No changes made to ISS ',ISS
        CALL STR_COMPRESS(CLINE)
        WRITE(6,'(X,A)') CLINE(1:LAN(CLINE))
        WRITE(6,'(A)') ' '
        WRITE(6,'(A)') '$Press any key to continue'
        READ(5,'(A)',IOSTAT=IOS) CLINE
        RETURN
      END IF
      CALL DRS_PARAMETERS_READ(IFREQ,CNAME)
      WRITE(CLINE,'(A,I3)') 
     &    'The following changes will be made to ISS ',ISS
      CALL STR_COMPRESS(CLINE)
      WRITE(6,'(X,A)') CLINE(1:LAN(CLINE))
      WRITE(6,'(A)') ' '
      DO I=1,512
        IF(BLIST(I).EQ.-1) THEN
          IP=I
          IF=IFREQ(I)
          CTEMP=CNAME(I)
          WRITE(CLINE,'(A,I3,A,I3,A)') 
     &        'Remove parameter ',IP,' '//CTEMP//'  at ',IF,' Hz'
          CALL STR_COMPRESS(CLINE)
          WRITE(6,'(X,A)') CLINE(1:LAN(CLINE))
        END IF
      END DO
      DO I=1,512
        IF(BLIST(I).EQ.1) THEN
          IP=I
          IF=IFREQ(I)
          CTEMP=CNAME(I)
          WRITE(CLINE,'(A,I3,A,I3,A)') 
     &        'Add parameter ',IP,' '//CTEMP//'  at ',IF,' Hz'
          CALL STR_COMPRESS(CLINE)
          WRITE(6,'(X,A)') CLINE(1:LAN(CLINE))
        END IF
      END DO
      WRITE(6,'(A)') ' '
      WRITE(6,'(A)') '$Proceed? [N]: '
      READ(5,'(A)',IOSTAT=IOS) CLINE
      IF(IOS.LT.0) RETURN
      IF(CLINE.EQ.' ') CLINE='N'
      CALL STR$UPCASE(CLINE,CLINE)
      IF(CLINE(1:1).NE.'Y') RETURN

      DO I=1,512
        IF(BLIST(I).EQ.1) BBUFF(I)=1
        IF(BLIST(I).EQ.-1) BBUFF(I)=0
      END DO
      OPEN(11,FILE='HORACE:DRS_ISS.DAT;',STATUS='OLD',RECL=128,
     &    ACCESS='DIRECT')
      ISS=ISS+1
      WRITE(11,REC=ISS) BBUFF
      CLOSE(11)
      WRITE(6,'(A)') ' '
      WRITE(6,'(A,I3)') ' New ISS created with number ',ISS
      WRITE(6,'(A)') ' '
      WRITE(6,'(A)') '$Press any key to continue'
      READ(5,'(A)',IOSTAT=IOS) CLINE
      RETURN
      END
!*******************************************************************************
      SUBROUTINE DRS_ISS_BLOCK(JBLOCK,ISS)
!
! Builds a DRS ISS block for sending as the first 10 blocks after startup.  The
! ISS is built from the latest ISS definition in the DRS_ISS.DAT file.
!
      IMPLICIT  NONE
      INTEGER*4 I,IFREQ(512),IOFF,IPMAX,ISS
      INTEGER*2 JBLOCK(2048)
      BYTE      BBUFF(512)
      CHARACTER CNAME(512)*4
! Read the latest ISS
      CALL DRS_ISS_READ(BBUFF,ISS)
! Read the parameter list
      CALL DRS_PARAMETERS_READ(IFREQ,CNAME)
! Compute the highest parameter number used
      DO I=1,512
        IF(BBUFF(I).EQ.1) IPMAX=I
      END DO
! Clear the block
      DO I=1,2048
        JBLOCK(I)=0
      END DO
! Put in the ISS and set the start of block bit
      JBLOCK(1)=ISS
!      JBLOCK(1)=IBSET(JBLOCK(1),13) !ISS blocks don't have bit 13 set!!!
! Compute the starting position for each parameter's data
      IOFF=16
      DO I=1,IPMAX
        IF(BBUFF(I).NE.0) THEN
          JBLOCK(I+1)=IOFF
          IOFF=IOFF+2*IFREQ(I)
        END IF
      END DO
! Put in position of final (non-existent) parameter so can compute last freq
      JBLOCK(IPMAX+2)=IOFF
! Mark end of data
      JBLOCK(IPMAX+3)=4095
      RETURN
      END
!*******************************************************************************
      SUBROUTINE DRS_ISS_CHANGE
!
! Handles ISS changes relative to the latest ISS by
!
! a. Asking which parameters to be deleted
! b. Asking which parameters to be added
! c. Checking that new parameters have been defined
! d. Confirming the required changes
! e. Actually making the change and adding the new ISS to the DRS_ISS.DAT file
!
      IMPLICIT  NONE
      INTEGER*4 I
      BYTE      BLIST(512)

      DO I=1,512
        BLIST(I)=0
      END DO
      CALL DRS_ISS_REMOVE(BLIST)
      CALL DRS_ISS_ADD(BLIST)
      CALL DRS_ISS_APPLY(BLIST)
      CALL DRS_ISS_SHOW(-1)
      RETURN
      END
!*******************************************************************************
      SUBROUTINE DRS_ISS_CREATE
!
! Creates from scratch the DRS_ISS.DAT, which is a direct access file with one
! block for each ISS, starting at 1.  Each block contains 512 bytes, set to 1
! if the parameter is recorded and 0 if it is not.  The last block in the file
! contains the latest/current ISS.  Data are taken from ISS_nn.DAT files in the
! HORACE: directory, up to ISS_37.DAT.
!
      IMPLICIT  NONE
      INTEGER*4 II,I,IOS,IP
      CHARACTER CFILE*80
      BYTE      BBUFF(512)
      INTEGER*2 JDATA(768)

      OPEN(11,FILE='HORACE:DRS_ISS.DAT;',STATUS='NEW',RECL=128,
     &    ACCESS='DIRECT')
      DO I=1,37
        DO II=1,512
          BBUFF(II)=0
        END DO
        CFILE='HORACE:ISS_xx.DAT'
        WRITE(CFILE(12:13),'(I2.2)') I
        OPEN(12,FILE=CFILE,STATUS='OLD',READONLY,IOSTAT=IOS,RECL=384,
     &      ACCESS='DIRECT')
        IF(IOS.EQ.0) THEN
          READ(UNIT=12,REC=1) JDATA
          IP=1
          DO WHILE(JDATA(IP+2).NE.4095)
            IF(JDATA(IP+1).NE.0) BBUFF(IP)=1
            IP=IP+1
          END DO
        END IF
        CLOSE(12,IOSTAT=IOS)           !Close the file
        WRITE(11,REC=I) BBUFF
      END DO
      CLOSE(11)
      RETURN
      END
!*******************************************************************************
      SUBROUTINE DRS_ISS_PRINT
!
! Prints the contents of the latest ISS in three columns.
!
      IMPLICIT  NONE
      INTEGER*4 I,IOS,IFREQ(512),IPNUM,IPLIST(512),IPFREQ(512)
      INTEGER*4 NLINES,NPLINES,NFLINES,IOFF,IL,ISS,IWORDS,LAN,IM
      INTEGER*4 ISTAT,LIB$SPAWN
      CHARACTER CNAME(512)*4,CPNAME(512)*4,CLINE*80
      BYTE      BBUFF(512)

      WRITE(6,'(A)') ' '
      CALL GET_LOGICAL('DRS_CONTROL_PRINTER',CLINE)
      IF(CLINE.EQ.' ') THEN
        WRITE(6,'(A)')
     &      ' Logical name DRS_CONTROL_PRINTER must define printer'
        WRITE(6,'(A)') ' '
        WRITE(6,'(A)') '$Press any key to continue'
        READ(5,'(A)',IOSTAT=IOS) CLINE
        RETURN
      END IF
      WRITE(6,'(A)') ' Please wait...'
      OPEN(12,FILE='HORACE:DRS_CONTROL.TMP',STATUS='NEW')
      OPEN(11,FILE='HORACE:DRS_ISS.DAT;',STATUS='OLD',RECL=128,
     &    ACCESS='DIRECT',READONLY)
      IM=0
      READ(11,REC=IM+1,IOSTAT=IOS) BBUFF
      DO WHILE(IOS.EQ.0)
        IM=IM+1
        READ(11,REC=IM+1,IOSTAT=IOS) BBUFF
      END DO
      ISS=IM
      READ(11,REC=ISS) BBUFF
      CLOSE(11)
      CALL DRS_PARAMETERS_READ(IFREQ,CNAME)
      IPNUM=0
      IWORDS=0
      DO I=1,512
        IF(BBUFF(I).EQ.1) THEN
          IPNUM=IPNUM+1
          IPLIST(IPNUM)=I
          IPFREQ(IPNUM)=IFREQ(I)
          CPNAME(IPNUM)=CNAME(I)
          IWORDS=IWORDS+IFREQ(I)
        END IF
      END DO
      WRITE(12,'(A)') ' '
      WRITE(CLINE,'(A,I4,A)') 'ISS',ISS,' contents'
      CALL STR_COMPRESS(CLINE)
      WRITE(12,'(X,A)') CLINE(1:LAN(CLINE))
      WRITE(12,'(A)') ' '
      IF(IPNUM.EQ.0) THEN
        WRITE(CLINE,'(A,I4)') 'No information available for ISS ',ISS
        CALL STR_COMPRESS(CLINE)
        WRITE(12,'(X,A)') CLINE(1:LAN(CLINE))
      ELSE
        NLINES=(IPNUM-1)/4+1           !No of lines to be written
        NPLINES=NLINES*4-IPNUM         !No of part lines
        NFLINES=NLINES-NPLINES         !No of full lines
        IOFF=NLINES
        DO IL=1,NFLINES                !For each full line
          WRITE(12,11) ((IPLIST(I),IPFREQ(I),CPNAME(I)),
     &        I=IL,IL+3*IOFF,IOFF)
        END DO
        IF(NPLINES.NE.0) THEN
          DO IL=NFLINES+1,NLINES       !For each part line
            WRITE(12,11) ((IPLIST(I),IPFREQ(I),CPNAME(I)),
     &        I=IL,IL+2*IOFF,IOFF)
          END DO
        END IF
        WRITE(12,'(A)') ' '
        WRITE(CLINE,'(X,I3,A,I4,A)') IPNUM,' parameters and ',IWORDS,
     &      ' data words.'
        CALL STR_COMPRESS(CLINE)
        WRITE(12,'(X,A)') CLINE(1:LAN(CLINE))
        WRITE(12,'(A)') ' '
        DO I=1,IPNUM
          IF(IPFREQ(I).EQ.0) WRITE(12,'(A,I3,A)')
     &        ' WARNING - Parameter ',
     &        IPLIST(I),' is not defined.'
        END DO
      END IF
      WRITE(12,'(A)') ' '
      CLINE=' '
      CALL DATE1(CLINE(1:9))
      CALL TIME(CLINE(12:19))
      WRITE(12,'(X,A)') CLINE(1:LAN(CLINE))
      CLOSE(12)
      WRITE(6,'(A)') ' '
      CLINE='PRIN/QUEU=DRS_CONTROL_PRINTER/DELE '//
     &    'HORACE:DRS_CONTROL.TMP;'
      ISTAT=LIB$SPAWN(CLINE,,,,,,,,,,,)
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      WRITE(6,'(A)') ' '
      WRITE(6,'(A)') '$Press any key to continue'
      READ(5,'(A)',IOSTAT=IOS) CLINE
      RETURN
11    FORMAT(X,4(I3,X,I3,X,A,10X))
      END
!*******************************************************************************
      SUBROUTINE DRS_ISS_READ(BBUFF,ISS)
!
! Reads the latest ISS into the 512 element byte array, where each byte is set
! to 1 if the parameter is in the ISS, else 0.  Also returns the latest ISS
! number.
! 
      IMPLICIT  NONE
      INTEGER*4 IM,IOS,ISS
      BYTE      BBUFF(512)

      OPEN(11,FILE='HORACE:DRS_ISS.DAT;',STATUS='OLD',RECL=128,
     &    ACCESS='DIRECT',READONLY)
      IM=1
      READ(11,REC=IM,IOSTAT=IOS) BBUFF
      DO WHILE(IOS.EQ.0)
        IM=IM+1
        READ(11,REC=IM,IOSTAT=IOS) BBUFF
      END DO
      ISS=IM-1
      READ(11,REC=ISS) BBUFF
      CLOSE(11)
      RETURN
      END
!*******************************************************************************
      SUBROUTINE DRS_ISS_REMOVE(BLIST)
!
! Flags the removal of an ISS parameter
!
      IMPLICIT  NONE
      INTEGER*4 ISS,LAN,IP,IOS
      BYTE      BLIST(512),BBUFF(512)
      CHARACTER CLINE*80

      CALL DRS_ISS_READ(BBUFF,ISS)
10    WRITE(6,'(A)') ' '
      WRITE(CLINE,'(A,I3,A)') 
     &    'Parameter to remove from ISS ',ISS,' (CTRL/Z when done): '
      CALL STR_COMPRESS(CLINE)
      WRITE(6,'(A)') '$'//CLINE(1:LAN(CLINE))//' '
      READ(5,'(A)',IOSTAT=IOS) CLINE
      IF(IOS.LT.0) RETURN
      WRITE(6,'(A)') ' '
      READ(CLINE,'(I)',IOSTAT=IOS) IP
      IF(IOS.NE.0) THEN
        WRITE(6,'(A)') ' Invalid entry'
      ELSE IF(IP.LT.1.OR.IP.GT.512) THEN
        WRITE(6,'(A)') ' Invalid parameter number'
      ELSE IF(BLIST(IP).EQ.-1) THEN
        WRITE(6,'(A)') ' Parameter is already flagged for removal'
      ELSE IF(BBUFF(IP).NE.1) THEN
        WRITE(6,'(A)') ' Parameter is not present in current ISS'
      ELSE
        BLIST(IP)=-1
        WRITE(CLINE,'(A,I4,A,I4)') 
     &      ' Parameter',IP,' has been flagged for removal from ISS',ISS
        CALL STR_COMPRESS(CLINE)
        WRITE(6,'(X,A)') CLINE(1:LAN(CLINE))
      END IF  
      GOTO 10
      END
!*******************************************************************************
      SUBROUTINE DRS_ISS_SHOW(IMODE)
!
! Asks the user which ISS to display (the latest is the default) and displays
! it in three columns.
!
! If IMODE is -1 displays the latest ISS, if 0 it asks, if +ve it displays
! the number given.
!
      IMPLICIT  NONE
      INTEGER*4 I,IOS,IFREQ(512),IPNUM,IPLIST(512),IPFREQ(512)
      INTEGER*4 NLINES,NPLINES,NFLINES,IOFF,IL,ISS,IWORDS,LAN,IM,IMODE
      CHARACTER CNAME(512)*4,CPNAME(512)*4,CLINE*80,CISS*4,CM*4
      BYTE      BBUFF(512)

      OPEN(11,FILE='HORACE:DRS_ISS.DAT;',STATUS='OLD',RECL=128,
     &    ACCESS='DIRECT',READONLY)
      IM=0
      READ(11,REC=IM+1,IOSTAT=IOS) BBUFF
      DO WHILE(IOS.EQ.0)
        IM=IM+1
        READ(11,REC=IM+1,IOSTAT=IOS) BBUFF
      END DO
      ISS=0
      IF(IMODE.EQ.-1) ISS=IM
      IF(IMODE.GT.0.AND.IMODE.LE.IM) ISS=IMODE
      DO WHILE(ISS.LE.0.OR.ISS.GT.IM)
        WRITE(CM,'(I4)') IM
        I=1
        DO WHILE(CM(I:I).EQ.' ')
          I=I+1
        END DO
        WRITE(6,'(A)') '$ISS number ['//CM(I:)//']: '
        READ(5,'(A)',IOSTAT=IOS) CISS
        IF(IOS.LT.0) RETURN
        READ(CISS,'(I)',IOSTAT=IOS) ISS
        IF(CISS.EQ.' ') ISS=IM
        IF(IOS.NE.0) ISS=0
      END DO 
      READ(11,REC=ISS) BBUFF
      CLOSE(11)
      CALL DRS_PARAMETERS_READ(IFREQ,CNAME)
      IPNUM=0
      IWORDS=0
      DO I=1,512
        IF(BBUFF(I).EQ.1) THEN
          IPNUM=IPNUM+1
          IPLIST(IPNUM)=I
          IPFREQ(IPNUM)=IFREQ(I)
          CPNAME(IPNUM)=CNAME(I)
          IWORDS=IWORDS+IFREQ(I)
        END IF
      END DO
      WRITE(6,'(A)') ' '
      WRITE(CLINE,'(A,I4,A)') 'ISS',ISS,' contents'
      CALL STR_COMPRESS(CLINE)
      WRITE(6,'(X,A)') CLINE(1:LAN(CLINE))
      WRITE(6,'(A)') ' '
      IF(IPNUM.EQ.0) THEN
        WRITE(CLINE,'(A,I4)') 'No information available for ISS ',ISS
        CALL STR_COMPRESS(CLINE)
        WRITE(6,'(X,A)') CLINE(1:LAN(CLINE))
      ELSE
        NLINES=(IPNUM-1)/4+1           !No of lines to be written
        NPLINES=NLINES*4-IPNUM         !No of part lines
        NFLINES=NLINES-NPLINES         !No of full lines
        IOFF=NLINES
        DO IL=1,NFLINES                !For each full line
          WRITE(6,11) ((IPLIST(I),IPFREQ(I),CPNAME(I)),
     &        I=IL,IL+3*IOFF,IOFF)
        END DO
        IF(NPLINES.NE.0) THEN
          DO IL=NFLINES+1,NLINES       !For each part line
            WRITE(6,11) ((IPLIST(I),IPFREQ(I),CPNAME(I)),
     &        I=IL,IL+2*IOFF,IOFF)
          END DO
        END IF
        WRITE(6,'(A)') ' '
        WRITE(CLINE,'(X,I3,A,I4,A)') IPNUM,' parameters and ',IWORDS,
     &      ' data words.'
        CALL STR_COMPRESS(CLINE)
        WRITE(6,'(X,A)') CLINE(1:LAN(CLINE))
        WRITE(6,'(A)') ' '
        DO I=1,IPNUM
          IF(IPFREQ(I).EQ.0) WRITE(6,'(A,I3,A)')' WARNING - Parameter ',
     &        IPLIST(I),' is not defined.'
        END DO
      END IF
      WRITE(6,'(A)') ' '
      WRITE(6,'(A)') '$Press any key to continue'
      READ(5,'(A)',IOSTAT=IOS) CLINE
      RETURN
11    FORMAT(X,4(I3,X,I3,X,A,10X))
      END
!*******************************************************************************
      SUBROUTINE DRS_CONFIG_CHANGE
!
! Runs the editor to make changes to the DRS_CONFIG.DAT file, and checks
! that the new file is valid.
!
      IMPLICIT  NONE
      INTEGER*4 ISTAT,LIB$SPAWN,ISTS
      CHARACTER CLINE*80

      WRITE(6,'(A)') ' '
      WRITE(6,'(A)') ' Please wait...'
      OPEN(11,FILE='HORACE:DRS_CONTROL.TMP',STATUS='NEW',
     &    CARRIAGECONTROL='LIST')
      WRITE(11,'(A)') '$ DEFINE /USER SYS$INPUT SYS$COMMAND'
      WRITE(11,'(A)') '$ EDIT /TPU /INIT=HORACE:EVEINI.TPU '//
     &    'HORACE:DRS_CONFIG.DAT'
      CLOSE(11)
      CLINE='@HORACE:DRS_CONTROL.TMP'
      ISTAT=LIB$SPAWN(CLINE,,,,,,,,,,,)
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      OPEN(11,FILE='HORACE:DRS_CONTROL.TMP',STATUS='OLD')
      CLOSE(11,DISPOSE='DELETE')
      WRITE(6,'(A)') ' '
      CALL DRS_CONFIG_CHECK(ISTS)
      IF(ISTS.EQ.1) CALL DRS_CONFIG_SORT
      RETURN
      END
!*******************************************************************************
      SUBROUTINE DRS_CONFIG_CHECK(ISTS)
!
! Checks that the information in the DRS_CONFIG.DAT file are correctly 
! specified.  Returns 1 if no problems, else 0.
!
! 02/08/04  WDNJ  Valid port address for M type boards reduced from C0 to A0
! to allow TWC to work on aft port DLU.
!
      IMPLICIT  NONE
      INTEGER*4 IOS,IL,LAN,IC,I,ISTS,IFREQ(512),ISS,ICNT,IP,IL1
      CHARACTER CLINE*80,CLINE1*80,CLIN*80
      CHARACTER CFILE*80,CNAME(512)*4,CTEMP*11,CTEMP1*9
      CHARACTER CEADDR(32)*17,CADDNAM(32)*6
      CHARACTER CDLU*6,CARD*2,CARDTYP*3,CPORTNO*2,CPORTAD*3,
     &    CPARA*3,CPFRQ*3,CPNAM*4,CDTP(256)*11,CDA(256)*9,CTYP*1
      BYTE      BISS(512),BPAR(512)

      ISTS=1
      DO I=1,512
        BPAR(I)=0
      END DO
      CALL DRS_ISS_READ(BISS,ISS)
      CALL DRS_ADDRESSES_READ(CEADDR,CADDNAM,ICNT)
      CALL DRS_PARAMETERS_READ(IFREQ,CNAME)
      CFILE='HORACE:DRS_CONFIG.DAT'
      OPEN(11,FILE=CFILE,STATUS='OLD',READONLY)
      READ(11,'(A)',IOSTAT=IOS) CLINE
      IL=1
      IL1=0
      DO WHILE(IOS.EQ.0)
        IF(CLINE(1:1).EQ.'!') THEN
        ELSE IF(CLINE.NE.' ') THEN
          IL1=IL1+1
          CALL STR$UPCASE(CLINE1,CLINE)
          CALL STR_COMPRESS(CLINE1)
!
          CALL STR$ELEMENT(CDLU,0,' ',CLINE1)
          IC=0
          DO I=1,ICNT
            IF(CDLU.EQ.CADDNAM(I)) IC=IC+1
          END DO
          IF(IC.EQ.0.AND.CDLU.NE.'NONDLU') THEN
            WRITE(CLIN,'(A,I3,A)') 
     &          'LINE ',IL,' - DLU name has no Ethernet address defined'
            CALL STR_COMPRESS(CLIN)
            WRITE(6,'(X,A)') CLIN(1:LAN(CLIN))
            WRITE(6,'(X,A)') CLINE(1:LAN(CLINE))
            WRITE(6,'(A)') ' '
            ISTS=0
          END IF
!
          CALL STR$ELEMENT(CARD,1,' ',CLINE1)
          READ(CARD,'(BN,I)',IOSTAT=IOS) IC
          IF((IC.LT.1.OR.IC.GT.14.OR.IOS.NE.0).AND.CDLU.NE.'NONDLU') 
     &      THEN
            WRITE(CLIN,'(A,I3,A)') 
     &          'LINE ',IL,' - Invalid card number'
            CALL STR_COMPRESS(CLIN)
            WRITE(6,'(X,A)') CLIN(1:LAN(CLIN))
            WRITE(6,'(X,A)') CLINE(1:LAN(CLINE))
            WRITE(6,'(A)') ' '
            ISTS=0
          END IF
!
          CALL STR$ELEMENT(CARDTYP,2,' ',CLINE1)
          CTYP=CARDTYP(1:1)
          IF(CTYP.NE.'A'.AND.CTYP.NE.'S'.AND.
     &          CTYP.NE.'M'.AND.CTYP.NE.'D'.AND.CTYP.NE.'N') THEN
            WRITE(CLIN,'(A,I3,A)') 
     &          'LINE ',IL,' - Invalid card type'
            CALL STR_COMPRESS(CLIN)
            WRITE(6,'(X,A)') CLIN(1:LAN(CLIN))
            WRITE(6,'(X,A)') CLINE(1:LAN(CLINE))
            WRITE(6,'(A)') ' '
            ISTS=0
          END IF
!
          CALL STR$ELEMENT(CPORTNO,3,' ',CLINE1)
          READ(CPORTNO,'(BN,I)',IOSTAT=IOS) IC
          IF((IC.LT.1.OR.IC.GT.5.OR.
     &        IOS.NE.0).AND.CDLU.NE.'NONDLU') THEN
            WRITE(CLIN,'(A,I3,A)') 
     &          'LINE ',IL,' - Invalid port number'
            CALL STR_COMPRESS(CLIN)
            WRITE(6,'(X,A)') CLIN(1:LAN(CLIN))
            WRITE(6,'(X,A)') CLINE(1:LAN(CLINE))
            WRITE(6,'(A)') ' '
            ISTS=0
          ELSE IF(((CTYP.NE.'D'.AND.
     &        IC.GT.4).OR.CTYP.EQ.'D'.AND.IC.NE.5)
     &        .AND.CDLU.NE.'NONDLU') THEN
            WRITE(CLIN,'(A,I3,A)') 
     &          'LINE ',IL,' - Port number doesn''t match card type'
            CALL STR_COMPRESS(CLIN)
            WRITE(6,'(X,A)') CLIN(1:LAN(CLIN))
            WRITE(6,'(X,A)') CLINE(1:LAN(CLINE))
            WRITE(6,'(A)') ' '
            ISTS=0
          END IF
!
          CTEMP=CDLU(1:LAN(CDLU))//CARD(1:LAN(CARD))//
     &        CPORTNO(1:LAN(CPORTNO))
          IF(IL1.GT.1) THEN
            DO I=1,IL1-1
              IF(CDTP(I).EQ.CTEMP.AND.CTYP.NE.'M'.AND.CDLU.NE.'NONDLU')
     &          THEN
                WRITE(CLIN,'(A,I3,A)') 
     &              'LINE ',IL,' - Port '//CPORTNO(1:LAN(CPORTNO))//
     &              ' on card '//CARD(1:LAN(CARD))//
     &              ' has already been used'
                CALL STR_COMPRESS(CLIN)
                WRITE(6,'(X,A)') CLIN(1:LAN(CLIN))
                WRITE(6,'(X,A)') CLINE(1:LAN(CLINE))
                WRITE(6,'(A)') ' '
                ISTS=0
              END IF
            END DO
          END IF
          CDTP(IL1)=CTEMP
!
          CALL STR$ELEMENT(CPORTAD,4,' ',CLINE1)
          READ(CPORTAD,'(BN,Z)',IOSTAT=IOS) IC
          IF(IOS.NE.0.OR.
     &      (CTYP.EQ.'A'.AND.(IC.LT.'10'X.OR.IC.GT.'7F'X)).OR.
     &      (CTYP.EQ.'S'.AND.(IC/2.LT.65.OR.IC/2.GT.76)).OR.
     &      (CTYP.EQ.'D'.AND.(IC.NE.'8A'X.AND.IC.NE.'9A'X.AND.
     &      IC.NE.'C0'X.AND.IC.NE.'D0'X.AND.IC.NE.'E0'X.AND.
     &      IC.NE.'A0'X)).OR.
     &      (CTYP.EQ.'M'.AND.(IC.LT.'A0'X.OR.IC.GT.'EF'X))) THEN
            WRITE(CLIN,'(A,I3,A)') 
     &          'LINE ',IL,' - Invalid port address'
            CALL STR_COMPRESS(CLIN)
            WRITE(6,'(X,A)') CLIN(1:LAN(CLIN))
            WRITE(6,'(X,A)') CLINE(1:LAN(CLINE))
            WRITE(6,'(A)') ' '
            ISTS=0
          END IF
!
          CTEMP1=CDLU(1:LAN(CDLU))//CPORTAD(1:LAN(CPORTAD))
          IF(IL1.GT.1) THEN
            DO I=1,IL1-1
              IF(CDA(I).EQ.CTEMP1.AND.CDLU.NE.'NONDLU') THEN
                WRITE(CLIN,'(A,I3,A)') 
     &              'LINE ',IL,' - Port address '//
     &              CPORTAD(1:LAN(CPORTAD))//' on '//CDLU(1:LAN(CDLU))
     &              //' has already been used'
                CALL STR_COMPRESS(CLIN)
                WRITE(6,'(X,A)') CLIN(1:LAN(CLIN))
                WRITE(6,'(X,A)') CLINE(1:LAN(CLINE))
                WRITE(6,'(A)') ' '
                ISTS=0
              END IF
            END DO
          END IF
          CDA(IL1)=CTEMP1
!
          CALL STR$ELEMENT(CPARA,5,' ',CLINE1)
          READ(CPARA,'(BN,I)',IOSTAT=IOS) IC
          IP=IC
          IF(IC.LT.1.OR.IC.GT.512.OR.IOS.NE.0) THEN
            WRITE(CLIN,'(A,I3,A)') 
     &          'LINE ',IL,' - Invalid parameter number'
            CALL STR_COMPRESS(CLIN)
            WRITE(6,'(X,A)') CLIN(1:LAN(CLIN))
            WRITE(6,'(X,A)') CLINE(1:LAN(CLINE))
            WRITE(6,'(A)') ' '
            ISTS=0
            IP=IC
          ELSE IF(IFREQ(IC).EQ.0) THEN
            WRITE(CLIN,'(A,I3,A,I3,A)') 
     &          'LINE ',IL,' - Parameter ',IC,' is not defined'
            CALL STR_COMPRESS(CLIN)
            WRITE(6,'(X,A)') CLIN(1:LAN(CLIN))
            WRITE(6,'(X,A)') CLINE(1:LAN(CLINE))
            WRITE(6,'(A)') ' '
            ISTS=0
          ELSE IF(BPAR(IC).NE.0) THEN
            WRITE(CLIN,'(A,I3,A,I3,A)') 'LINE ',IL,
     &          ' - Parameter ',IC,' has already been configured'
            CALL STR_COMPRESS(CLIN)
            WRITE(6,'(X,A)') CLIN(1:LAN(CLIN))
            WRITE(6,'(X,A)') CLINE(1:LAN(CLINE))
            WRITE(6,'(A)') ' '
            ISTS=0
          ELSE IF(BISS(IC).NE.1) THEN
            WRITE(CLIN,'(A,I3,A,I3,A)') 'LINE ',IL,
     &          ' - Parameter ',IC,' is not recorded in the current ISS'
            CALL STR_COMPRESS(CLIN)
            WRITE(6,'(X,A)') CLIN(1:LAN(CLIN))
            WRITE(6,'(X,A)') CLINE(1:LAN(CLINE))
            WRITE(6,'(A)') ' '
            ISTS=0
          END IF
          IF(IC.GT.0.AND.IC.LE.255.AND.IOS.EQ.0) BPAR(IC)=1
!
          CALL STR$ELEMENT(CPFRQ,6,' ',CLINE1)
          IF(CPFRQ.NE.' ') THEN
            READ(CPFRQ,'(BN,I)',IOSTAT=IOS) IC
            IF(IP.NE.0) THEN
              IF(IOS.NE.0.OR.IC.NE.IFREQ(IP).AND.ISTS.EQ.1) THEN
                WRITE(CLIN,'(A,I3,A,I3)') 'LINE ',IL,' - Parameter'//
     &    ' frequency is invalid.  Will use defined value of ',IFREQ(IP)
                CALL STR_COMPRESS(CLIN)
                WRITE(6,'(X,A)') CLIN(1:LAN(CLIN))
                WRITE(6,'(X,A)') CLINE(1:LAN(CLINE))
                WRITE(6,'(A)') ' '
              END IF
            END IF
          END IF
!
          CALL STR$ELEMENT(CPNAM,7,' ',CLINE1)
          IF(CPNAM.NE.' ') THEN
            IF(IP.NE.0) THEN
              IF(IOS.NE.0.OR.CPNAM.NE.CNAME(IP).AND.ISTS.EQ.1) THEN
                WRITE(CLIN,'(A,I3,A,A)') 'LINE ',IL,' - Parameter'//
     &         ' name is invalid.  Will use defined value of ',CNAME(IP)
                CALL STR_COMPRESS(CLIN)
                WRITE(6,'(X,A)') CLIN(1:LAN(CLIN))
                WRITE(6,'(X,A)') CLINE(1:LAN(CLINE))
                WRITE(6,'(A)') ' '
              END IF
            END IF
          END IF
!          
        END IF
        READ(11,'(A)',IOSTAT=IOS) CLINE
        IL=IL+1
      END DO
      CLOSE(11,IOSTAT=IOS)
      IF(ISTS.NE.1) THEN
        WRITE(6,'(A)') ' '
        WRITE(6,'(A)') '$Press any key to continue'
        READ(5,'(A)',IOSTAT=IOS) CLINE
      END IF
      RETURN
      END
!*******************************************************************************
      SUBROUTINE DRS_CONFIG_PRINT
!
! Prints the DRS_CONFIG.DAT file and displays all non-comment lines to the
! printer defined by the logical DRS_CONTROL_PRINTER.
!
      IMPLICIT  NONE
      INTEGER*4 IOS,LAN,LIB$SPAWN,ISTAT
      CHARACTER CLINE*80,CLSTDLU*6

      WRITE(6,'(A)') ' '
      CALL GET_LOGICAL('DRS_CONTROL_PRINTER',CLINE)
      IF(CLINE.EQ.' ') THEN
        WRITE(6,'(A)')
     &      ' Logical name DRS_CONTROL_PRINTER must define printer'
        WRITE(6,'(A)') ' '
        WRITE(6,'(A)') '$Press any key to continue'
        READ(5,'(A)',IOSTAT=IOS) CLINE
        RETURN
      END IF
      WRITE(6,'(A)') ' Please wait...'
      OPEN(12,FILE='HORACE:DRS_CONTROL.TMP',STATUS='NEW')
      WRITE(12,'(A)') ' DLU configuration'
      WRITE(12,'(A)') ' '
      WRITE(12,'(A)') '    DLU   Card  Card type  Port no  Port addr'//
     &    '  Para no   Para freq   Para name'
      OPEN(11,FILE='HORACE:DRS_CONFIG.DAT',STATUS='OLD',READONLY)
      READ(11,'(A)',IOSTAT=IOS) CLINE
      CLSTDLU=' '
      DO WHILE(IOS.EQ.0)
        IF(CLINE(1:1).NE.'!') THEN
          IF(CLINE(1:6).NE.CLSTDLU) WRITE(12,'(A)') ' '
          WRITE(12,'(X,A)') CLINE(1:LAN(CLINE))
          CLSTDLU=CLINE(1:6)
        END IF
        READ(11,'(A)',IOSTAT=IOS) CLINE
      END DO
      CLOSE(11)
      WRITE(12,'(A)') ' '
      CLINE=' '
      CALL DATE1(CLINE(1:9))
      CALL TIME(CLINE(12:19))
      WRITE(12,'(X,A)') CLINE(1:LAN(CLINE))
      CLOSE(12)
      WRITE(6,'(A)') ' '
      CLINE='PRIN/QUEU=DRS_CONTROL_PRINTER/DELE '//
     &    'HORACE:DRS_CONTROL.TMP;'
      ISTAT=LIB$SPAWN(CLINE,,,,,,,,,,,)
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      WRITE(6,'(A)') ' '
      WRITE(6,'(A)') '$Press any key to continue'
      READ(5,'(A)',IOSTAT=IOS) CLINE
      RETURN
      END
!*******************************************************************************
      SUBROUTINE DRS_CONFIG_SHOW
!
! Read the DRS_CONFIG.DAT file and displays all non-comment lines on the
! screen. Then checks the file and displays any problems.
!
      IMPLICIT  NONE
      INTEGER*4 IOS,LAN,ISTS
      CHARACTER CLINE*80,CLSTDLU*6

      WRITE(6,'(A)') CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H'
      WRITE(6,'(A)') ' DLU configuration'
      WRITE(6,'(A)') ' '
      WRITE(6,'(A)') '    DLU   Card  Card type  Port no  Port addr'//
     &    '  Para no   Para freq   Para name'
      OPEN(11,FILE='HORACE:DRS_CONFIG.DAT',STATUS='OLD',READONLY)
      READ(11,'(A)',IOSTAT=IOS) CLINE
      CLSTDLU=' '
      DO WHILE(IOS.EQ.0)
        IF(CLINE(1:1).NE.'!') THEN
          IF(CLINE(1:6).NE.CLSTDLU) WRITE(6,'(A)') ' '
          WRITE(6,'(X,A)') CLINE(1:LAN(CLINE))
          CLSTDLU=CLINE(1:6)
        END IF
        READ(11,'(A)',IOSTAT=IOS) CLINE
      END DO
      CLOSE(11)
      WRITE(6,'(A)') ' '
      CALL DRS_CONFIG_CHECK(ISTS)
      IF(ISTS.EQ.1) THEN
        WRITE(6,'(A)') '$Press any key to continue'
        READ(5,'(A)',IOSTAT=IOS) CLINE
      END IF
      RETURN
      END
!*******************************************************************************
      SUBROUTINE DRS_CONFIG_SORT
!
! Tidies up the DRS_CONFIG.DAT file, putting in correct parameter freqs and
! names where necessary, formatting into lined up columns and sorting by DLU,
! card and port.
!
      IMPLICIT  NONE
      INTEGER*4 IOS,ICARD,IPNUM,IPADD,IPARA,LAN,IFREQ(0:512),ISTAT,ICNT,
     &    I
      CHARACTER CDLUN*6,CCARD*2,CCTYP*3,CPNUM*2,CPADD*3,CPARA*3,
     &    CLINE*80,CLIN*80,CNAME(0:512)*4,CLAST*80,CEADDR(32)*17,
     &    CDLUNAM(32)*6
      LOGICAL*4 LCOMMENT
      INTEGER*2 KEYBUF(21)
      INTEGER*4 SOR$PASS_FILES,SOR$BEGIN_SORT,SOR$SORT_MERGE,
     &    SOR$END_SORT 
      EXTERNAL DSC$K_DTYPE_T 

      CALL DRS_PARAMETERS_READ(IFREQ(1),CNAME(1))
      IFREQ(0)=0
      CNAME(0)=' '
      CALL DRS_ADDRESSES_READ(CEADDR,CDLUNAM,ICNT)
!
! Write out to temporary file, neatly formatted, all lines which start with
! a DLU name, or have a valid DLU name prefixed by a !.  In the latter case
! move the ! to column 28, and then treat the line like other DLU lines.
!
      OPEN(11,FILE='HORACE:DRS_CONFIG.DAT',STATUS='OLD',READONLY)
      OPEN(12,FILE='HORACE:DRS_CONFIG_SORT.TMP',STATUS='NEW',
     &    CARRIAGECONTROL='LIST')
      READ(11,'(A)',IOSTAT=IOS) CLINE
      DO WHILE(IOS.EQ.0)
        LCOMMENT=.FALSE.
        IF(CLINE(1:1).EQ.'!') THEN
          DO I=1,ICNT
            IF(CLINE(2:7).EQ.CDLUNAM(I)) LCOMMENT=.TRUE.
          END DO
          IF(LCOMMENT) CLINE=CLINE(2:)
        END IF
        IF(CLINE(1:1).EQ.'!') THEN
        ELSE IF(CLINE.NE.' ') THEN
          CALL STR$UPCASE(CLINE,CLINE)
          CALL STR_COMPRESS(CLINE)
          CALL STR$ELEMENT(CDLUN,0,' ',CLINE)
          CALL STR$ELEMENT(CCARD,1,' ',CLINE)
          CALL STR$ELEMENT(CCTYP,2,' ',CLINE)
          CALL STR$ELEMENT(CPNUM,3,' ',CLINE)
          CALL STR$ELEMENT(CPADD,4,' ',CLINE)
          CALL STR$ELEMENT(CPARA,5,' ',CLINE)
          READ(CCARD,'(BN,I)',IOSTAT=IOS) ICARD
          READ(CPNUM,'(BN,I)',IOSTAT=IOS) IPNUM
          READ(CPADD,'(BN,Z)',IOSTAT=IOS) IPADD
          READ(CPARA,'(BN,I)',IOSTAT=IOS) IPARA
          WRITE(CLIN,'(A,I7,9X,A,T29,I5,Z11,I9,I12,8X,A)',IOSTAT=IOS)
     &    CDLUN,ICARD,CCTYP,IPNUM,IPADD,IPARA,IFREQ(IPARA),CNAME(IPARA)
          IF(LCOMMENT) CLIN(28:28)='!'
          WRITE(12,'(A)') CLIN(1:LAN(CLIN))
        END IF
        READ(11,'(A)',IOSTAT=IOS) CLINE
      END DO
      CLOSE(11,IOSTAT=IOS)
      CLOSE(12,IOSTAT=IOS)
! Now sort the temp file into a new temp file.
      KEYBUF(1)=5
      KEYBUF(2)=%LOC(DSC$K_DTYPE_T) !Primary key DLU name
      KEYBUF(3)=0 
      KEYBUF(4)=0 
      KEYBUF(5)=6 
      KEYBUF(6)=%LOC(DSC$K_DTYPE_T) !Secondary key Card num
      KEYBUF(7)=0
      KEYBUF(8)=11 
      KEYBUF(9)=2 
      KEYBUF(10)=%LOC(DSC$K_DTYPE_T) !Tertiary key Port num
      KEYBUF(11)=0 
      KEYBUF(12)=31 
      KEYBUF(13)=2 
      KEYBUF(14)=%LOC(DSC$K_DTYPE_T) !Quadrenary key Comment
      KEYBUF(15)=0 
      KEYBUF(16)=27 
      KEYBUF(17)=1 
      KEYBUF(18)=%LOC(DSC$K_DTYPE_T) !Quintenary key Port addr
      KEYBUF(19)=0 
      KEYBUF(20)=42 
      KEYBUF(21)=2 
      ISTAT=SOR$PASS_FILES('HORACE:DRS_CONFIG_SORT.TMP',
     &    'HORACE:DRS_CONFIG_SORT1.TMP') 
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      ISTAT=SOR$BEGIN_SORT(KEYBUF,,,,,,,) 
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      ISTAT=SOR$SORT_MERGE() 
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      ISTAT=SOR$END_SORT() 
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      OPEN(11,FILE='HORACE:DRS_CONFIG_SORT.TMP',STATUS='OLD')
      CLOSE(11,DISPOSE='DELETE')
!
! Now take the comment lines from the start of the original file, and then add
! the sorted DLU lines, putting back the ! in the case of DLU lines which were
! originally commented out.  Note that only comments at the start of the file
! or at the end of a line are retained.  Others are lost.
!
      OPEN(11,FILE='HORACE:DRS_CONFIG.DAT;',STATUS='OLD')
      OPEN(12,FILE='HORACE:DRS_CONFIG.DAT;',STATUS='NEW',
     &    CARRIAGECONTROL='LIST',IOSTAT=IOS)
      READ(11,'(A)') CLINE
      DO WHILE(CLINE(1:1).EQ.'!')
        LCOMMENT=.FALSE.
        DO I=1,ICNT
          IF(CLINE(2:7).EQ.CDLUNAM(I)) LCOMMENT=.TRUE.
        END DO
        IF(.NOT.LCOMMENT) WRITE(12,'(A)') CLINE(1:LAN(CLINE))
        READ(11,'(A)') CLINE
      END DO
      CLOSE(11)
      OPEN(11,FILE='HORACE:DRS_CONFIG_SORT1.TMP',STATUS='OLD')
      READ(11,'(A)',IOSTAT=IOS) CLINE
      CLAST=CLINE
      DO WHILE(IOS.EQ.0)
        IF(CLINE(1:6).NE.CLAST(1:6)) WRITE(12,'(A)') '!'
        CLAST=CLINE
        IF(CLINE(28:28).EQ.'!') THEN
          CLINE='!'//CLINE
          CLINE(29:29)=' '
        END IF      
        WRITE(12,'(A)') CLINE(1:LAN(CLINE))
        READ(11,'(A)',IOSTAT=IOS) CLINE
      END DO
      CLOSE(11,DISPOSE='DELETE')
      CLOSE(12)
      RETURN
      END
!*******************************************************************************
      SUBROUTINE ADDR_STRING(CSOURCE,CSTRING)
!
! Converts 6 byte string ethernet address to standard 17 char display string
!
      IMPLICIT  NONE
      INTEGER*4 I,IOS,IP,IVAL
      CHARACTER CSOURCE*6,CSTRING*17

      CSTRING='xx-xx-xx-xx-xx-xx'
      DO I=1,6
        IVAL=ICHAR(CSOURCE(I:I))
        IP=(I-1)*3+1
        WRITE(CSTRING(IP:IP+1),'(Z2.2)',IOSTAT=IOS) IVAL
      END DO
      RETURN
      END
!*******************************************************************************
      SUBROUTINE DATE1(CDATE)
!
! Replaces the DATE Fortran instrinsic and allows programs to compile without
! Y2000 warnings.  Returns current date as DD-MMM-YY.
!
      IMPLICIT  NONE
      INTEGER*4 IVAL(8)
      CHARACTER CMON*3(12),CDATE*9,CDATE1*8,CTIME*10,CZONE*5
      DATA      CMON /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     &    'SEP','OCT','NOV','DEC'/

      CALL DATE_AND_TIME(CDATE1,CTIME,CZONE,IVAL)
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
!*******************************************************************************
      INTEGER*4 FUNCTION LAN(CSTRING)
!
! Returns length of a string less trailing blanks.  Returns 1 if blank string.
!
      IMPLICIT  NONE
      INTEGER*4 N
      CHARACTER CSTRING*(*)

      N=LEN(CSTRING)                   !Get string length
      DO WHILE(N.GT.0.AND.CSTRING(N:N).EQ.' ') !Step back over spaces
        N=N-1
      END DO
      IF(N.EQ.0) N=1                   !Special case
      LAN=N
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
!*******************************************************************************
      SUBROUTINE STRING_ADDR(CEADD,JSOURCE,ISTS)
!
! Converts standard 17 char ethernet address string to 6 byte string in JSOURCE
! Returns 1 is STS if decodes OK, else 0
!
      IMPLICIT  NONE
      INTEGER*4 ISTS,IOS1,IOS2,IOS3
      INTEGER*2 JSOURCE(3)
      CHARACTER CEADD*17,CTEMP*4

      DO ISTS=1,3
        JSOURCE(ISTS)=0
      END DO
      CTEMP=CEADD(4:5)//CEADD(1:2)     !Reverse hex pairs
      READ(CTEMP,'(Z4)',IOSTAT=IOS1) JSOURCE(1) !Decode address into list
      CTEMP=CEADD(10:11)//CEADD(7:8)   !Reverse hex pairs
      READ(CTEMP,'(Z4)',IOSTAT=IOS2) JSOURCE(2) !Decode address into list
      CTEMP=CEADD(16:17)//CEADD(13:14) !Reverse hex pairs
      READ(CTEMP,'(Z4)',IOSTAT=IOS3) JSOURCE(3) !Decode address into list
      ISTS=1
      IF(IOS1.NE.0.OR.IOS2.NE.0.OR.IOS3.NE.0) ISTS=0
      RETURN
      END
!*******************************************************************************
      SUBROUTINE SWAP_ENDS16(JBUF,N)
!
! Swaps the two bytes in a sixteen bit word, or array of 16 bit words
! JBUF contains the 16 bit words, and N is the number of words in JBUF
!
      IMPLICIT  NONE
      INTEGER*2 JBUF(*),JTEMP
      INTEGER*4 N,I
      BYTE      B,BTEMP(2)
      EQUIVALENCE (JTEMP,BTEMP)

      DO I=1,N
        JTEMP=JBUF(I)
        B=BTEMP(1)
        BTEMP(1)=BTEMP(2)
        BTEMP(2)=B
        JBUF(I)=JTEMP
      END DO
      RETURN
      END
!*******************************************************************************
      SUBROUTINE SWAP_ENDS32(IBUF,N)
!
! Swaps bytes and words in a thirty two bit word, or array of 32 bit words
! JBUF contains the 32 bit words, and N is the number of words in IBUF
!
      IMPLICIT  NONE
      INTEGER*4 IBUF(*),I,ITEMP
      INTEGER*4 N
      BYTE      BBUF(4),B
      EQUIVALENCE (ITEMP,BBUF)

      DO I=1,N
        ITEMP=IBUF(I)
        B=BBUF(1)
        BBUF(1)=BBUF(4)
        BBUF(4)=B
        B=BBUF(2)
        BBUF(2)=BBUF(3)
        BBUF(3)=B
        IBUF(I)=ITEMP   
      END DO
      RETURN
      END
