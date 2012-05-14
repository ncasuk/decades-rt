      PROGRAM H_DRS_LOG
C
C H_DRS_LOG.FOR
C
C Reads data from the DRS, one block at a time into global common memory.
C Read completion is signalled to other processes by the setting of common
C event flags.  The reception of ISS data blocks (the first 10 from the DRS)
C is also signalled.  This program should be run at a high priority.
C
C Build command:
C
C    $ FORT H_DRS_LOG
C    $ LINK H_DRS_LOG,H_COMMON/OPT
C
C    where H_COMMON.OPT contains the following line:
C
C     SYS$LIBRARY:H_COMMON/SHAREABLE
C
C V1.00  01/01/89  W.D.N.JACKSON
C
C V1.02  17/10/90  W.D.N.JACKSON
C        Now calls DRS_BLK_SIZ to autocompute the record length to be used in
C        the DRSDAT.DAT files, and stores the value in STATUS(6).
C
C V1.07  17/02/92  W.D.N.JACKSON
C        Now notes time that good data are received in STATUS(27&28)
C
C V1.08  03/08/92  W.D.N.JACKSON
C        Now maintains STATUS(33) with 1 if there is valid data in NFDATA, else
C        0.  Valid means actual DRS data has been received within the last 3s.
C        Now stores latest flight number and date.
C
C V1.13  01/09/04  W.D.N.JACKSON
C        Now indicates correct status if DRS restarts during logging. All code
C        for old C-130 DRS removed.
C 
      IMPLICIT  NONE
      INTEGER*4 ISTAT,IBLKS,IBUFC,IBUFL,IBLKSTS,IOS,I,ISTS,IMASK,
     -    TIMINT(2)
      REAL*4    RTIME1,RTIMDIF,RLSTTIM
      LOGICAL*4 LWAITING,LTIMO
      INTEGER*2 LASTEVM /-1/, LST5 /0/, LST6 /0/ ,LST9
      CHARACTER CMESS*256
      INCLUDE   'HCOM_DEF.FOR'
      INCLUDE   '($SSDEF)'
      INCLUDE   '($SYSSRVNAM)'
C
C Note the following:
C 
C 1. The DRS only sends data to HORACE when it is in DATA mode.
C 
C 2. The DRS data consists of 2048 16 bit words every 1s.
C 
C 3. The first 10 DRS data blocks after the DRS starts recording are 
C    identical and contain the Instrument Sampling Sequence (ISS).
C
C The flags are used as follows:
C
C  Flag  64        Set by H_DRS_SIM or H_DRS_LOG when a new DRS data block 
C                  is in global common, but not an ISS block.  Used and 
C                  cleared by H_DERIVE.
C  Flag  65        Set by H_DRS_SIM or H_DRS_LOG when a new DRS data block 
C                  is in global common, but not an ISS block.  Used and
C                  cleared by H_TAPE.
C  Flag  66        Set by H_DRS_SIM or H_DRS_LOG when an ISS data block 
C                  is in global common.  Used and cleared by H_ISS_GET.
C  Flag  75        Set by H_DRS_SIM or H_DRS_LOG when a new DRS data block 
C                  is in global common, but not an ISS block.  Used and
C                  cleared by H_DISC.
C
C H_DRS_LOG takes data from the new DRS process. The DRS process puts the data 
C directly into the NFDATA array
C using STATUS(62) as a pointer.  It then sets common event flag 84 to indicate
C that there is a new block of data available.  Also uses CEF 85 internally to
C operated a 2s read timeout.
C
C Start by associating a name with a common event flag cluster.  This
C cluster is common to all processes with the same UIC group number.
C Assign common event flag cluster, starting at EF 64
C
      CALL LOG_MESS('H_DRS_LOG starting') !Send message to log file
      CALL ST_MESS('Starting')            !Send message to logical name
      CALL LOG_MESS('Taking data from new DRS') !Send message to log file
      ISTAT=SYS$ASCEFC(%VAL(64),'H_CEF_CLUSTER',,) !Associate with CEF cluster
      IF(ISTAT.NE.SS$_NORMAL) CALL LIB$SIGNAL(%VAL(ISTAT))
      ISTAT=SYS$BINTIM('0000 00:00:02.00',TIMINT) !Conv 2s to 64 bit delta time
      IMASK=0                          !Form mask for required event flags
      IMASK=IBSET(IMASK,84-64)         !EF 76 will be set by user
      IMASK=IBSET(IMASK,85-64)         !EF 77 will be set by 1s timer
      STATUS(62)=1
      ISTAT=SYS$CLREF(%VAL(84))
      IBLKS=0                          !No blocks recvd
      IBUFC=1                          !Swinging buffer counter 1 or 2
      CALL DRS_BLK_SIZ('GET')          !Read in DRSBLK_SIZ.DAT file
      STATUS(27)=0                     !Time last data received
      STATUS(28)=0                     !Time last data received
      STATUS(46)=0                     !DRV11 function bits (0-7)
      LWAITING=.TRUE.                  !Waiting for valid data block
      CFDAT=' '                        !Latest flight date
      CFNUM=' '                        !Latest flight number
      DO WHILE(.TRUE.)                 !Loop continuously
        IBUFL=IBUFC                    !Swap last and current buffer pointers
        IBUFC=3-IBUFC
        ISTAT=SYS$SETIMR(%VAL(85),TIMINT,,,) !Wait up to two seconds
        ISTAT=SYS$WFLOR(%VAL(64),%VAL(IMASK)) !Wait for EF 84 or 85 to set
        ISTAT=SYS$CLREF(%VAL(84))      !Clear EF 84
        LTIMO=.FALSE.                  !Assume not a timeout
        IF(SYS$READEF(%VAL(85),ISTAT).EQ.SS$_WASSET) LTIMO=.TRUE.
        ISTAT=SYS$CANTIM(,)            !Clear any outstanding timer request
        STATUS(62)=3-STATUS(62)        !Point next input to new buffer
        RTIME1=SECNDS(0.0)
        IF(IBLKS.EQ.0) RLSTTIM=RTIME1  !Special case
        RTIMDIF=RTIME1-RLSTTIM         !Time since last read completed
        RLSTTIM=RTIME1                 !Save for next time
        IF(LTIMO) THEN                 !Catch timeouts
          IF(.NOT.LWAITING) THEN
            CALL LOG_MESS('DRS data reception timeout')
            CALL ST_MESS('NOT receiving DRS data')
            LWAITING=.TRUE.
            STATUS(33)=0               !Show data is old/invalid
          END IF
        ELSE
          CALL CHECK_BLK(NFDATA(1,IBUFL),IBLKSTS)
          IF(IBLKSTS.EQ.0) THEN        !Invalid block
            CALL LOGMESS(NFDATA(1,IBUFC),IBLKS)
            CALL ST_MESS('NOT receiving DRS data')
            CALL LOG_MESS('Invalid DRS block')
            LWAITING=.TRUE.
            STATUS(33)=0
            WRITE(6,1000) (NFDATA(I,IBUFL),I=1,8),' Words 1 to 8'
            WRITE(6,1000) (NFDATA(I,IBUFL),I=9,16),' Words 9 to 16'
          ELSE                         !Have good DRS or ISS block
            IF(RTIMDIF.GT.1.2) THEN
              WRITE(CMESS,1001,IOSTAT=IOS) RTIMDIF
              CALL LOG_MESS('Block read difference of '//CMESS(1:5)
     -            //' s')
            END IF
            STATUS(1)=IBUFL            !Update buffer pointer
            IF(IBLKSTS.EQ.1) THEN      !If ISS block
              IF(LWAITING) CALL LOG_MESS
     -            ('Valid ISS block received from DRS')
              LWAITING=.FALSE.
              STATUS(33)=0
              CALL DRS_BLK_SIZ('ISS')  !Calculate recording block size
              ISTAT=SYS$SETEF(%VAL(66))!Tell H_DRS_GET
            ELSE                       !Good DRS data block
              IF(LWAITING) CALL LOG_MESS
     -            ('Valid data block received from DRS')
              IF(STATUS(33).EQ.0) CALL ST_MESS('Receiving DRS data')
              LWAITING=.FALSE.
              STATUS(33)=1             !Show that data in NFDATA is valid
              IF(LASTEVM.NE.NFDATA(13,STATUS(1))) THEN !Event mark change
                STATUS(14)=NFDATA(10,STATUS(1)) !Store DRS HHM time
                STATUS(15)=NFDATA(11,STATUS(1)) !Store DRS MSS time
                LASTEVM=NFDATA(13,STATUS(1))    !Note new event mark
              END IF     
              CALL DRS_BLK_SIZ('DRS')  !Calculate rec block size if necessary
              ISTAT=SYS$SETEF(%VAL(64))!Tell H_DERIVE
              ISTAT=SYS$SETEF(%VAL(65))!Tell H_OPTIC
              ISTAT=SYS$SETEF(%VAL(75))!Tell H_DISC
              ISTS=STATUS(1)
              IF(NFDATA(5,ISTS).NE.LST5.OR.NFDATA(6,ISTS).NE.LST6.OR.
     -            NFDATA(9,ISTS).NE.LST9.OR.CFNUM.EQ.' '.OR.CFDAT
     -            .EQ.' ') THEN
                CALL UPD_FDAT(NFDATA(1,ISTS),CFNUM,CFDAT)
                LST5=NFDATA(5,ISTS)
                LST6=NFDATA(6,ISTS)
                LST9=NFDATA(9,ISTS)
              END IF
            END IF
            CALL STR_TIME(STATUS(27))  !Store latest time
            IBLKS=IBLKS+1              !Bump block count
            IF(MOD(IBLKS,600).EQ.1) CALL LOGMESS(NFDATA(1,IBUFL),IBLKS)
          END IF
        END IF
      END DO                           !Loop continuously
1000  FORMAT(1X,8Z5.4,A)
1001  FORMAT(F5.2)
1002  FORMAT(X,A)
1003  FORMAT(X,A,Z6.4)
1004  FORMAT(X,A,I6)
1005  FORMAT(X,'Status (H) ',Z4.4,'  Bytes (D) ',I4.4,'  CSR (H) ',Z4.4,
     -    '  Interval (s) ',F6.2)
      END
********************************************************************************
      SUBROUTINE LOG_MESS(MESSAGE)
C
C Writes message to log file consisting of program name, date, time and message.
C
C V1.00  01/01/89  W.D.N.JACKSON
C
      IMPLICIT NONE
      CHARACTER*(*) MESSAGE
      CHARACTER CDAT*9,CTIM*10
      CALL DATE1(CDAT)
      CALL TIME(CTIM)
      TYPE *,'H_DRS_LOG '//CDAT//' '//CTIM//MESSAGE
      RETURN
      END
********************************************************************************
      SUBROUTINE    ST_MESS(MESSAGE)
C
C Write message to the logical H_DRS_LOG_STAT in the group logical name
C table.  GRPNAM and SYSNAM privileges are required.
C
C V1.00  01/01/89  W.D.N.JACKSON
C
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
      ISTAT=SYS$CRELNM(,'LNM$GROUP','H_DRS_LOG_STAT',PSL$C_SUPER,TLIST)
      IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      RETURN
      END
********************************************************************************
      SUBROUTINE DRS_BLK_SIZ(CMODE)
C
C Works out the size of each one second data block to write to disk.  These
C are an integral number of VMS blocks (ie multiples of 256 words); only the
C minimum number are written to save time and space, and in recognition of the
C fact that currently not all the possible 2048 DRS data words are used.
C The value to use cannot be ascertained unambiguously from a DRS data block -
C either the ISS block must be available or the information must be stored on 
C disk.  This procedure tries to cover as many eventualities as possible using
C the following scheme:
C
C  1. If the file DRSBLK_SIZ.DAT is present and contains a value for the
C     ISS in use then this value is used and rounded up to the next 256 word
C     boundary.
C
C  2. If 1 fails then a new value is computed if this routine is called when
C     an ISS block is present in the data buffer.  In this case a new version
C     of DRSBLK_SIZ.DAT is also written.
C
C  3. If neither 1 or 2 have occurred and a block of DRS data has been
C     received without any knowledge of what size data blocks to write then
C     an emergency procedure is followed to ensure that logging can still
C     take place.  In this case the amount of useful data is taken to be the
C     larger of the highest non-zero value in the data block and the largest
C     value for any old ISS plus 128 words.  This is then rounded up to a block
C     boundary.
C
C The subroutine takes 3 values for CMODE:
C
C  'GET'    Reads in DRSBLK_SIZ.DAT if present
C  'ISS'    Processes ISS data block
C  'DRS'    Processes DRS data block
C
C The subroutine output is set in STATUS(6), as multiples of 256.  It also
C produces new versions of DRSBLK_SIZ.DAT if there is a change.
C
C V1.00            W.D.N.JACKSON
C
C V1.01  24/05/91  W.D.N.JACKSON
C        Bug fixed in emergency part of code to compute ISS properly
C
C V1.02  05/08/02  W.D.N.JACKSON
C        Now handles ISS up to 999
C
      IMPLICIT  NONE
      CHARACTER*(*) CMODE
      CHARACTER CLINE*80
      INTEGER*4 IBLKSIZ(999),I,IOS,JOS,KOS,ISS,IBS,MAXSIZ,MAXWRD
      INCLUDE   'HCOM_DEF.FOR'
C
C Note that NPLOC and NPFREQ are not defined at this stage. (Done by H_DERIVE
C using ISS file).
C
      IF(CMODE.EQ.'GET') THEN          !Initialization
        STATUS(6)=0
        DO I=1,999                     !Clear array of DRS data sizes
          IBLKSIZ(I)=0
        END DO
        OPEN(UNIT=10,FILE='HORACE:DRSBLK_SIZ.DAT',STATUS='OLD', !Try to read
     -      READONLY,CARRIAGECONTROL='LIST',IOSTAT=IOS)
        IF(IOS.EQ.0) THEN              !If file opened
          READ(10,10,IOSTAT=IOS) CLINE !Read first line
          DO WHILE(IOS.GE.0)           !Exit loop on end of file
            READ(CLINE(1:3),11,IOSTAT=JOS) ISS !Interpret each line
            READ(CLINE(5:8),12,IOSTAT=KOS) IBS ! may be a comment line
            IF(JOS.EQ.0.AND.KOS.EQ.0.AND.ISS.GE.1.AND.ISS.LE.999
     -        .AND.IBS.GE.700.AND.IBS.LE.2048) IBLKSIZ(ISS)=IBS
            READ(10,10,IOSTAT=IOS) CLINE !Read next lines
          END DO
        END IF
        CLOSE(UNIT=10,IOSTAT=IOS)      !Close file
      ELSE IF(CMODE.EQ.'ISS'.AND.STATUS(6).EQ.0) THEN !ISS block is present
        ISS=NFDATA(1,STATUS(1)).AND.'FFF'X !Get ISS number
        IF(IBLKSIZ(ISS).EQ.0) THEN     !If we don't already have a size
          I=2048                       !Search for end of ISS data mark (4095)
          DO WHILE(NFDATA(I,STATUS(1)).NE.4095.AND.I.GT.2)
            I=I-1
          END DO
          IF(NFDATA(I,STATUS(1)).EQ.4095) THEN !If mark found
            IBLKSIZ(ISS)=NFDATA(I-1,STATUS(1))/2 !Use final offset (in bytes)
            OPEN(UNIT=10,FILE='HORACE:DRSBLK_SIZ.DAT',STATUS='NEW',  
     -          CARRIAGECONTROL='LIST',IOSTAT=IOS) !Write new file
            WRITE(10,10,IOSTAT=IOS) '!' !Put on a header
            WRITE(10,10,IOSTAT=IOS) '! DRSBLK_SIZ.DAT - Number of '//
     -          'DRS words for each ISS value'
            WRITE(10,10,IOSTAT=IOS) '!'
            DO I=1,999                  !Put in all known values
              IF(IBLKSIZ(I).NE.0) WRITE(10,13,IOSTAT=IOS) I,IBLKSIZ(I)
            END DO
            CLOSE(UNIT=10,IOSTAT=IOS)  !Close output file
          END IF
        END IF
        STATUS(6)=((IBLKSIZ(ISS)-1)/256+1)*256 !Round sizes up to nearest blk
        WRITE(CLINE,14,IOSTAT=IOS) 'ISS size ',IBLKSIZ(ISS),
     -      ' words.  Block size ',STATUS(6),' words.'
        CALL LOG_MESS(CLINE(1:50))
      ELSE IF(CMODE.EQ.'DRS'.AND.STATUS(6).EQ.0) THEN !DRS block is present
        ISS=NFDATA(1,STATUS(1)).AND.'FFF'X !Get ISS value
        IF(ISS.GE.1.AND.ISS.LE.999) THEN !Must be valid
          IF(IBLKSIZ(ISS).EQ.0) THEN !If haven't got a value
            MAXSIZ=0                 !Calc present highest value
            DO I=1,999
              IF(IBLKSIZ(I).GT.MAXSIZ) MAXSIZ=IBLKSIZ(I)
            END DO    
            MAXWRD=2048              !Calc highest non-zero data
            DO WHILE(NFDATA(MAXWRD,STATUS(1)).EQ.0.AND.MAXWRD.GT.2)
              MAXWRD=MAXWRD-1
            END DO
            IBLKSIZ(ISS)=MAX(MAXWRD,MAXSIZ+128) !Take the largest
          END IF
          STATUS(6)=((IBLKSIZ(ISS)-1)/256+1)*256 !Round up to next block
          WRITE(CLINE,14,IOSTAT=IOS) 'ISS size ',IBLKSIZ(ISS),
     -        ' words.  Block size ',STATUS(6),' words.'
          CALL LOG_MESS(CLINE(1:50))
        END IF
      END IF
10    FORMAT(A)
11    FORMAT(I3)
12    FORMAT(I4)
13    FORMAT(I3.3,I5.4)
14    FORMAT(A,I4,A,I4,A)
      RETURN
      END
C*******************************************************************************
      SUBROUTINE STR_TIME(JSTORE)
C
C Stores the current time as a 32 bit integer seconds past midnight.
C
      IMPLICIT  NONE
      INTEGER*4 ITEMP
      INTEGER*2 JSTORE(2),JTEMP(2)
      EQUIVALENCE (ITEMP,JTEMP)
      ITEMP=NINT(SECNDS(0.0))
      JSTORE(1)=JTEMP(1)
      JSTORE(2)=JTEMP(2)
      RETURN
      END
********************************************************************************
      SUBROUTINE UPD_FDAT(NDATA,CFLT,CDAT)
C
C Given a block of DRS data, returns the flight number and date as character
C strings.  If either is invalid it is returned as an empty string.
C
      IMPLICIT  NONE
      INTEGER*4 J1,IHUN,ITEN,IUNI,IOS,J2,JDAY,JMON,JYER
      LOGICAL*4 LVFNO,LVDAT
      INTEGER*2 NDATA(2048)
      CHARACTER CFLT*4,CDAT*9,CFN*4
      CHARACTER CMON(12)*3 /'JAN','FEB','MAR','APR','MAY','JUN',
     -                      'JUL','AUG','SEP','OCT','NOV','DEC'/

      LVFNO=.TRUE.
      J1=NDATA(9)                      !Flight number
      IHUN=IBITS(J1,8,4)
      ITEN=IBITS(J1,4,4)
      IUNI=IBITS(J1,0,4)
      IF(IHUN.GT.9.OR.IHUN.LT.0) LVFNO=.FALSE.
      IF(ITEN.GT.9.OR.ITEN.LT.0) LVFNO=.FALSE.
      IF(IUNI.GT.9.OR.IUNI.LT.0) LVFNO=.FALSE.
      IF(LVFNO) THEN
        WRITE(CFLT,10,IOSTAT=IOS) IHUN,ITEN,IUNI
        CALL GET_LOGICAL('FLIGHT_NUMBER',CFN)
        IF(CFN.NE.' ') CFLT(1:1)=CFN(1:1)
      ELSE
        CFLT=' '
      END IF

      LVDAT=.TRUE.
      J1=NDATA(5)                      !Work out DRS date
      J2=NDATA(6)
      JDAY=IBITS(J1,8,4)*10+IBITS(J1,4,4)
      JMON=IBITS(J1,0,4)*10+IBITS(J2,8,4)
      JYER=IBITS(J2,4,4)*10+IBITS(J2,0,4)
      IF(JDAY.LT.1.OR.JDAY.GT.31) LVDAT=.FALSE.
      IF(JMON.LT.1.OR.JMON.GT.12) LVDAT=.FALSE.
      IF(JYER.LT.0.OR.JYER.GT.99) LVDAT=.FALSE.
      IF(LVDAT) THEN
        WRITE(CDAT,11,IOSTAT=IOS) JDAY,CMON(JMON),JYER        
      ELSE
        CDAT=' '
      END IF

      RETURN
10    FORMAT('B',3I1)
11    FORMAT(I2.2,'-',A,'-',I2.2)
      END
********************************************************************************
      SUBROUTINE CHECK_BLK(IBUFF,IBLKSTS)
C
C Does simple checks on a block of DRS data to see if it is valid and if it is
C an ISS block.  Returns 0 if invalid, 1 if ISS block, and 2 if a standard DRS
C block.
C
C DRS blocks must have bit 13 of the first word set, word 4 must be 0, and 
C words 1, 2 and 3 must be non-zero.
C
C ISS blocks must have words 2 to 6 with values of 16, 18, 20, 22, 26.
C
      IMPLICIT  NONE
      INTEGER*4 IBLKSTS
      INTEGER*2 IBUFF(2048)

      IBLKSTS=2                        !Assume it is a good DRS block
      IF(.NOT.BITEST(IBUFF(1),13)) IBLKSTS=0 !First word must have bit 13
!      IF(IBUFF(4).NE.0) IBLKSTS=0      !Do very simple checks
      IF(IBUFF(1).EQ.0.OR.IBUFF(2).EQ.0.OR.IBUFF(3).EQ.0) IBLKSTS=0
      IF(IBUFF(2).EQ.16.AND.IBUFF(3).EQ.18.AND.IBUFF(4).EQ.20.
     -  AND.IBUFF(5).EQ.22.AND.IBUFF(6).EQ.26) IBLKSTS=1 !ISS block
      RETURN
      END
********************************************************************************
      SUBROUTINE LOGMESS(NBUFF,IBLKS)
      IMPLICIT  NONE
      INTEGER*4 JBLK,IOS,IBLKS
      INTEGER*2 NBUFF(2048)
      CHARACTER*10 CLINE*50
          
      JBLK=JZEXT(NBUFF(8).AND.'FFF'X)+4096*JZEXT(NBUFF(7).AND.'FFF'X)
      IF(JBLK.GT.72*3600) JBLK=0       !Likely ISS block if more than 72 hours
      WRITE(CLINE,10,IOSTAT=IOS) 'Blocks read:',IBLKS,
     -    '  Latest DRS block:',JBLK
      CALL LOG_MESS(CLINE)
      RETURN
10    FORMAT(A,I6,A,I6)
      END
!*******************************************************************************
      SUBROUTINE DATE1(CDATE)
!
! Replaces the DATE Fortran instrinsic and allows programs to compile without
! Y2000 warnings.  Returns date as DD-MMM-YY.
!
      IMPLICIT  NONE
      INTEGER*4 IVAL(8)
      CHARACTER CMON*3(12),CDATE*9,CDATE1*8,CDUMMY*10
      DATA      CMON /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     &    'SEP','OCT','NOV','DEC'/

      CALL DATE_AND_TIME(CDATE1,CDUMMY,CDUMMY,IVAL)
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
