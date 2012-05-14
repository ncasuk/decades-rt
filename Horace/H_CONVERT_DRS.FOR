C
C ROUTINE          H_CONVERT_DRS PROGRAM FORTVAX
C
C PURPOSE          Reads HORACE optical disks and creates a raw dataset
C
C DESCRIPTION      Reads HORACE optical disks with DRSDAT.DAT files, and
C                  produces raw datasets on ICARUS disk.
C                  Either the whole tape, or a specified time interval can be
C                  read.
C
C                  The optical disk must be mounted and accessible and the
C                  files specified as input must include device specifications.
C
C                  Note that the program will also create raw datasets from
C                  DRSDAT.DAT files on ICARUS provided the correct files are
C                  specified in chronological order, and the appropriate
C                  ISS file is present in the default directory.
C
C                  Includes code to generate dummy datasets, for testing the
C                  CALIBRATE program.
C
C VERSION          1.00  27-09-90  W.D.N.JACKSON
C
C ARGUMENTS        The programs takes the following from SYS$INPUT:
C
C                     Flight letter  (A-Z)
C                     Processing start time (hh:mm:ss or null)
C                     Processing end time (hh:mm:ss or null)
C                     The names of the raw data files is chronological order
C
C CHANGES          
C
C                  Now allows and requires specification of start and end times.
C
C                  V1.01  28-04-92  W.D.N.JACKSON
C                  Now correctly handles the time when a DRS reboot occurs
C                  after a midnight crossover.  Also rejects any times which
C                  would be earlier than times already processed.  Most of this
C                  change has been made in the BLKSEC routine.
C
C                  V1.02  16-03-93  D.P.BRIGGS
C                  Now checks that each data block is sequential in time,
C                  ie 1 second older than the last data block.
C                  Previously, the check was against block number.
C
C                  V1.03  18-10-93  W.D.N.JACKSON
C                  Opening text changed to show that optical disks are not
C                  required.  A blank line can now be used to terminate the
C                  file list.  No longer issues reminder to remove optical disk.
C                  Handles CTRL/Z for all inputs.  Now continues, with a
C                  message, after a data read error, rather than stopping.
C                  Error when producing dummy data fixed.
C
C
C		   V1.04 22-01-96  D Lauchlan
C 		   Unused variable defitions deleted
C
C
********************************************************************************
      PROGRAM H_CONVERT_DRS
CDEC$ IDENT 'V1.04'
C
C Build with   $FORT H_CONVERT_DRS
C              $LINK H_CONVERT_DRS,BIO
C
      INTEGER*2 IBUFF(20480,2)  !Declare first for page alignment in $LOCAL
C
      CHARACTER*4 CFNO
      CHARACTER*8 CFDATE,CFSRTT,CFENDT,
     -            CTDATE,CTTIME,
     -            CEDATE,CETIME,CESRTT,CEENDT
      INTEGER*4   ISS,IC,IDSTYP,IDUMM(106),IMAXBLK,ISRTTIM,IOS
      INTEGER*4   IOS1,IHRS,IMIN,ISEC,IENDTIM,IB,IRECLB,ISTATUS
      INTEGER*4   IFNO,JSTART,JCOUNT,J,IP,IBLK,I,IPARAM,IS,ISAMP
      INTEGER*4   IPOS,IVAL,LASTSEC,LASTBLK,IRET,IOS2,IOS3,IBYTES
      INTEGER*4   ITIME,IREC
C
      INTEGER*4   ISECTN,
     -            ISSRTT(40),
     -            ISENDT(40),
     -            ISRECD(40),
     -            IRECLW,
     -            IDUMMY(6)
C
      INTEGER*2   IPLIST(256),
     -            IPLOC(256),
     -            IPFREQ(256)

      LOGICAL LDUMMY,IFIRSTB,IFIRST,IEND,ISRT
C
      CHARACTER CISSFNAM*10,CFLETT*1,CVDAT*9
      CHARACTER CSTART*8,CEND*8,CLINE*132
      DATA IMAXBLK /20480/, IDSTYP /2/

      LDUMMY=.FALSE.                   !Set true if want a dummy dataset
      WRITE(6,15) ' '
      WRITE(6,15) 'H_CONVERT_DRS - creates raw datasets from HORACE '//
     -    'data files'
      WRITE(6,15) ' '
      CALL GETLETT(CFLETT)
      ISRTTIM=-1
      DO WHILE(ISRTTIM.EQ.-1)
        WRITE(6,17) '$Start time (hh:mm:ss or RETURN): '
        READ(5,17,IOSTAT=IOS) CLINE
        IF(IOS.EQ.-1) CALL EXIT
        IF (CLINE.EQ.' ') THEN
          ISRTTIM=0
        ELSE
          READ(CLINE(1:2),18,IOSTAT=IOS1) IHRS
          READ(CLINE(4:5),18,IOSTAT=IOS2) IMIN
          READ(CLINE(7:8),18,IOSTAT=IOS3) ISEC
          IF(IOS1.EQ.0.AND.IOS2.EQ.0.AND.IOS3.EQ.0.AND.
     -        IHRS.GE.0.AND.IHRS.LE.47.AND.IMIN.GE.0.AND.IMIN.LE.59.AND.
     -        ISEC.GE.0.AND.ISEC.LE.59) ISRTTIM=IHRS*3600+IMIN*60+ISEC
        END IF
      END DO
      IENDTIM=-1
      DO WHILE(IENDTIM.EQ.-1)
        WRITE(6,17) '$End time (hh:mm:ss or RETURN): '
        READ(5,17,IOSTAT=IOS) CLINE
        IF(IOS.EQ.-1) CALL EXIT
        IF (CLINE.EQ.' ') THEN
          IENDTIM=200000
        ELSE
          READ(CLINE(1:2),18,IOSTAT=IOS1) IHRS
          READ(CLINE(4:5),18,IOSTAT=IOS2) IMIN
          READ(CLINE(7:8),18,IOSTAT=IOS3) ISEC
          IF(IOS1.EQ.0.AND.IOS2.EQ.0.AND.IOS3.EQ.0.AND.
     -        IHRS.GE.0.AND.IHRS.LE.47.AND.IMIN.GE.0.AND.IMIN.LE.59.AND.
     -        ISEC.GE.0.AND.ISEC.LE.59) IENDTIM=IHRS*3600+IMIN*60+ISEC
        END IF
      END DO
      IFIRSTB=.TRUE.
      IFIRST=.TRUE.
      IEND=.FALSE.
      ISRT=.FALSE.
      IB=1
      CALL GET_NEXT_FILE(IRECLB,ISTATUS)
      IF(.NOT.ISTATUS) CALL EXIT
      CALL BIO_READ(12,IBUFF(1,IB),IMAXBLK)
      CALL BIO_WAITR(12,IBYTES)
      DO WHILE (ISTATUS.AND..NOT.IEND)
        CALL BIO_READ(12,IBUFF(1,3-IB),IMAXBLK)!Start next tape block read
        IF(IFIRSTB) THEN
          ISS=JZEXT(IBUFF(1,IB).AND.'FFF'X)
          CISSFNAM='ISS000.DAT'
          WRITE(CISSFNAM(4:6),19) ISS
          IFNO=IBUFF(9,IB)
          IFIRSTB=.FALSE.
        END IF          
        JSTART=1                        !Start point in IBUFF for disc data
        JCOUNT=0                        !No of bytes to be written to disc
        DO J=1,IBYTES/IRECLB            !For each DRS block in the data block
          IP=(J-1)*IRECLB/2+1           !Pointer to each DRS block
          CALL BLKSEC(IBUFF(IP,IB),IFNO,IBLK,ISEC)
          IF(ISEC.GT.-1) THEN           !For each good DRS blk
            IF(ISEC.GE.ISRTTIM) ISRT=.TRUE.
            IF(LDUMMY) THEN             !Convert data to dummies
              DO I=1,256
                IF(IPLIST(I).GT.3) THEN
                  IPARAM=IPLIST(I).AND.'F'X
                  ITIME=(ISEC*16.AND.'F0'X)
                  DO IS=1,IPFREQ(IPLIST(I))
                    ISAMP=IS*16*16.AND.'F00'X
                    IPOS=IP+IPLOC(I)+IS-2
                    IVAL=IBUFF(IPOS,IB)
                    IVAL=IVAL.AND.'F000'X
                    IVAL=IVAL.OR.ISAMP.OR.ITIME.OR.IPARAM
                    IBUFF(IPOS,IB)=IVAL
                  END DO
                END IF
              END DO
            END IF
            IF(IFIRST.AND.ISRT) THEN
              WRITE(CFNO,11) CFLETT,IBUFF(9,IB) !Flight no is in 9th word as BCD
              CALL BIO_CREATE(11,CFNO//'_RAW_DATA.DAT',IRECLB)
              IREC=0
              ISECTN=1
              ISSRTT(ISECTN)=ISEC
              ISRECD(ISECTN)=1
              ISS=JZEXT(IBUFF(1,IB)).AND.'FFF'X
              IC=JZEXT(IBUFF(3,IB))
              CALL DRSDAT(IBUFF(1,IB),CFDATE) !Get flight date
              CALL TIME(CTTIME)               !Get current time
              CETIME=CTTIME
              CALL DATE_AND_TIME(CVDAT)                !Get current date
              CTDATE=CVDAT(1:2)//CVDAT(4:6)//''''//CVDAT(8:9)
              CEDATE=CTDATE
              CALL SPMCTIM(ISEC,CFSRTT)
              CESRTT=CFSRTT
              IRECLW=IRECLB/2
              IFIRST=.FALSE.
            END IF   
            IF(ISRT) THEN
              JCOUNT=JCOUNT+IRECLB        !Bump byte count
              IREC=IREC+1
            END IF
            IF(ISEC.NE.LASTSEC+1.AND.IREC.GT.1) THEN
              ISENDT(ISECTN)=LASTSEC
              ISECTN=ISECTN+1
              IF(ISECTN.GT.40) THEN
        WRITE(6,*)'H_CONVERT_DRS-W-Stopping after 40 sections'
                IEND=.TRUE.
              ELSE
                ISSRTT(ISECTN)=ISEC
                ISRECD(ISECTN)=IREC
              END IF
            END IF
            LASTSEC=ISEC
            LASTBLK=IBLK
            IF(ISEC.GE.IENDTIM) IEND=.TRUE.
          ELSE                          !For each bad block
            IF(JCOUNT.NE.0) THEN        !Write out any waiting data to disc
              CALL BIO_WRITE(11,IBUFF(JSTART,IB),JCOUNT)
              JSTART=JSTART+JCOUNT/2+IRECLB/2 !Update pointer past bad block
              CALL BIO_WAITW(11,IRET)   !Wait for write to complete
              IF(IRET.NE.JCOUNT) THEN
        WRITE(6,*)'H_CONVERT_DRS-F-Disc write error.  Aborting.'
                IEND=.TRUE.
              END IF
              JCOUNT=0
            ELSE
              JSTART=JSTART+IRECLB/2    !Update pointer past bad block
            END IF
          END IF
        END DO
        IF(JCOUNT.NE.0) THEN     !Write remaining (normally all) data
          CALL BIO_WRITE(11,IBUFF(JSTART,IB),JCOUNT)
          CALL BIO_WAITW(11,IRET) !Wait for disc write to complete
          IF(IRET.NE.JCOUNT) THEN
        WRITE(6,*)' H_CONVERT_DRS-F-Stopping after disc write error.'
            IEND=.TRUE.
          END IF
        END IF
        IBYTES=-9999                   !Tell BIO_WAITR to return on error
        CALL BIO_WAITR(12,IBYTES)
        DO WHILE(IBYTES.EQ.-1)         !Until no read error
        WRITE(6,*) ' H_CONVERT_DRS-W-DRS data read error.  Continuing.'
          CALL BIO_READ(12,IBUFF(1,3-IB),IMAXBLK)
          IBYTES=-9999
          CALL BIO_WAITR(12,IBYTES)
        END DO
        IF(IBYTES.EQ.0) THEN           !End of file
          CALL BIO_CLOSE(12)
          CALL GET_NEXT_FILE(IRECLB,ISTATUS)
          IF(ISTATUS) THEN
            CALL BIO_READ(12,IBUFF(1,3-IB),IMAXBLK)
            CALL BIO_WAITR(12,IBYTES)
          END IF
        END IF
        IB=3-IB                        !Swap buffer pointers
      END DO      
      CALL BIO_CLOSE(12)
      CALL BIO_CLOSE(11)
      ISENDT(ISECTN)=LASTSEC
      CALL SPMCTIM(LASTSEC,CFENDT)
      CEENDT=CFENDT
C
C Convert ISS file information to parameter start and frequency information.
C
      CALL READ_ISS(CISSFNAM,IPLIST,IPLOC,IPFREQ)
C
C Form and write header and index blocks information
C
      OPEN(UNIT=10,FILE=CFNO//'_RAW_HDDR.DAT',STATUS='NEW',
     -    ACCESS='DIRECT',RECL=128)
      WRITE(UNIT=10,REC=1) CFNO,CFDATE,CFSRTT,CFENDT,CTDATE,CTTIME,
     -    CEDATE,CETIME,CESRTT,CEENDT,
     -    ISS,IC,IDSTYP,IDUMM
      WRITE(UNIT=10,REC=2) ISECTN,ISSRTT,ISENDT,ISRECD,IRECLW,IDUMMY
      WRITE(UNIT=10,REC=3) IPLIST
      WRITE(UNIT=10,REC=4) IPLOC
      WRITE(UNIT=10,REC=5) IPFREQ
      CLOSE(UNIT=10)
C
C Put brief synopsis on screen
C
      WRITE(6,*) ' '
      WRITE(6,*) 'Flight '//CFNO//'   '//CFDATE
      WRITE(6,*) ' '
      WRITE(6,*) 'Files written '//CFNO//'_RAW_HDDR.DAT'
      WRITE(6,*) '              '//CFNO//'_RAW_DATA.DAT'
      WRITE(6,*) ' '
      WRITE(6,14) ISECTN,' sections',IRECLW,' words per record'
      WRITE(6,*) ' '
      WRITE(6,*) 'Section   Start      End  '
      DO I=1,ISECTN
        CALL SPMCTIM(ISSRTT(I),CSTART)
        CALL SPMCTIM(ISENDT(I),CEND)
        WRITE(6,13) I,CSTART,CEND
      END DO
      WRITE(6,*) ' '
C
C Exit
C
      CALL EXIT
10    FORMAT(I2.2)
11    FORMAT(A,Z3.3)
12    FORMAT(I5.5)
13    FORMAT(1X,I3,3X,2(2X,A))
14    FORMAT(1X,2(I6,A))
15    FORMAT(X,A)
17    FORMAT(A)
18    FORMAT(I2)
19    FORMAT(I3.3)
      END
*******************************************************************************
      SUBROUTINE GETLETT(CFLETT)
C
C Asks user for the flight letter.  Pressing [RETURN] uses the default letter
C H.  Otherwise only alphabetic characters are accepted, and lower case are
C converted to upper case.
C
      CHARACTER*(*) CFLETT
      CHARACTER  CREPLY*40
      integer ios
      CFLETT=' '
      DO WHILE(CFLETT.EQ.' ')
        WRITE(6,10) '$Enter flight letter [B]: '
        READ(5,10,IOSTAT=IOS) CREPLY
        IF(IOS.EQ.-1) CALL EXIT
        CALL STR$UPCASE(CREPLY,CREPLY)
        IF(CREPLY.EQ.' ') THEN
          CFLETT='B'
        ELSE IF(CREPLY(1:1).GE.'A'.AND.CREPLY(1:1).LE.'Z') THEN
          CFLETT=CREPLY(1:1)
        END IF
      END DO
      RETURN
10    FORMAT(A)
      END
*******************************************************************************
      SUBROUTINE READ_ISS(CIFNAM,IPLIST,IPLOC,IPFREQ)
      INTEGER*2 IPLIST(256),IPLOC(256),IPFREQ(256),IDATA(768)
      CHARACTER*(*) CIFNAM
      integer ios,j,k,jj
C
C Read the ISS data and fill parameter list, parameter offsets, and parameter
C frequencies.
C
      OPEN(UNIT=10,FILE=CIFNAM,STATUS='OLD',
     -     READONLY,ACCESS='DIRECT',RECL=384,IOSTAT=IOS)
      IF(IOS.NE.0) THEN
        WRITE(6,*) ' H_CONVERT_DRS-F-Unable to open '//CIFNAM
        CALL EXIT
      END IF
      READ(UNIT=10,REC=1) (IDATA(J),J=1,768)       !Read the record
      CLOSE(UNIT=10)                               !Close the file
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
*******************************************************************************
      SUBROUTINE DRSDAT(IDATA,CFDATE)
C
C Takes the flight date from a block of DRS data and returns it as a string
C of the form  ddmmm'yy.
C
C Input:  IDATA  I*2  2048 array containing a block of raw DRS data
C Output: CFDATE C*8  The returned date string
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
      CHARACTER   CFDATE*8,CMONTHS*36
      INTEGER*2   IDATA(2048)
      integer j1,j2,jday,jmon,jyear,imindx
      DATA CMONTHS /'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'/
 
      J1=JZEXT(IDATA(5))
      J2=JZEXT(IDATA(6))
      JDAY=IBITS(J1,8,4)*10+IBITS(J1,4,4)
      JMON=IBITS(J1,0,4)*10+IBITS(J2,8,4)
      JYEAR=IBITS(J2,4,4)*10+IBITS(J2,0,4)
      WRITE(CFDATE(1:2),10) JDAY
      IMINDX=(JMON-1)*3 + 1
      CFDATE(3:6)=CMONTHS(IMINDX:IMINDX+2)//''''
      WRITE(CFDATE(7:8),10) JYEAR
      RETURN
10    FORMAT(I2.2)
      END
*******************************************************************************
      SUBROUTINE BLKSEC(IDATA,IFNO,IBLK,ISEC)
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
C Detects midnight crossings whether logging at the time or not.  But rejects
C any errors in the DRS clock which might cause it to lose time.  If the
C computed time (including any correction for midnight crossover) is earlier or
C the same as any time already processed, returns a value of ISEC of -1.  Also
C returns a time of -1 if the flight number or block number is invalid.
C
      INTEGER*2   IDATA(2048)
      integer isec,iblk,j1,j2,jhrs,jmin,jsec,ilstdrssec,ilstvalsec
      integer ilstblk,IFNO
      logical LFIRST,LXMN
      DATA        LFIRST /.TRUE./   !True for first call
      DATA        LXMN   /.FALSE./  !True when have crossed midnight
C
      ISEC=-1
      IF(IDATA(9).NE.IFNO) RETURN   !Check for reasonable flight no
      IBLK=4096*JZEXT(IDATA(7))+JZEXT(IDATA(8))
      IF(IBLK.LT.0.OR.IBLK.GT.86400) RETURN !Check for reasonable block no
      J1=JZEXT(IDATA(10))
      J2=JZEXT(IDATA(11))
      JHRS=IBITS(J1,8,4)*10+IBITS(J1,4,4)
      JMIN=IBITS(J1,0,4)*10+IBITS(J2,8,4)
      JSEC=IBITS(J2,4,4)*10+IBITS(J2,0,4)
      IF(JHRS.LT.0.OR.JHRS.GT.47.OR.JMIN.LT.0.OR.JMIN.GT.59.OR.
     -    JSEC.LT.0.OR.JSEC.GT.59) RETURN !Check for reasonable time
      ISEC=JHRS*3600+JMIN*60+JSEC
      IF(LFIRST) THEN
        ILSTDRSSEC=ISEC                 !Last DRS time processed
        ILSTVALSEC=-1                   !Last valid time
        ILSTBLK=IBLK
        LFIRST=.FALSE.
      END IF
      IF(ISEC.EQ.0.AND.ILSTDRSSEC.EQ.86399.AND.IBLK.EQ.ILSTBLK+1) THEN
        LXMN=.TRUE.                     !Logging across midnight
      ELSE IF(IBLK.NE.ILSTBLK+1.AND.ISEC.LT.ILSTDRSSEC) THEN
        LXMN=.TRUE.                     !Not logging but probably crossed m'nght
      END IF
      ILSTDRSSEC=ISEC
      IF(LXMN) ISEC=ISEC+24*60*60       !Adjust if cross midnight
      ILSTBLK=IBLK
      IF(ISEC.LE.ILSTVALSEC) THEN       !Check for valid time
        ISEC=-1                         !Time decrease is an error 
      ELSE
        ILSTVALSEC=ISEC                 !Store latest valid time returned
      END IF
      RETURN
      END
*******************************************************************************
      SUBROUTINE SPMCTIM(ISECS,CTIM)
C
C Converts time in seconds past midnight to a character string of the form
C hh:mm:ss
C
      integer itim,isecs,ih,im,is
      CHARACTER CTIM*8
      ITIM=MOD(ISECS,24*60*60)    !Reduce to less than a day
      IH=ITIM/3600
      IM=MOD(ITIM/60,60)
      IS=ITIM-3600*IH-60*IM
      CTIM='xx:xx:xx'
      WRITE(CTIM(1:2),10) IH
      WRITE(CTIM(4:5),10) IM
      WRITE(CTIM(7:8),10) IS
      RETURN
10    FORMAT(I2.2)
      END
*******************************************************************************
      SUBROUTINE GET_NEXT_FILE(IRECLB,ISTATUS)
C
C Asks user for the name of the next data file.  The file is checked for
C existence and readability and then opened for asynchronous I/O.
C
C Returns with IRECLB indicating the record length, and ISTATUS true if the
C file was opened, and false if no more files to open. (User entered CTRL/Z
C or command file input ended)
C
C V1.01  18-10-93  Now takes blank line as list terminator
C
      CHARACTER  CREPLY*80
      integer ios,ireclb
      logical istatus,itexists
      ISTATUS = .TRUE.
      ITEXISTS = .FALSE.      
      DO WHILE(.NOT. ITEXISTS)
        WRITE(6,10) '$Full name of next data file (or CTRL/Z): '
        READ(5,10,IOSTAT=IOS) CREPLY
        IF(IOS.EQ.-1.OR.CREPLY.EQ.' ') THEN
          ISTATUS=.FALSE.
          ITEXISTS=.TRUE.
        ELSE
          INQUIRE(FILE=CREPLY,EXIST=ITEXISTS)
          IF(.NOT.ITEXISTS) WRITE(6,10) ' File not found'
        END IF
      END DO
      IF(ISTATUS) CALL BIO_OPEN(12,CREPLY,IRECLB)
      RETURN
10    FORMAT(A)
      END
