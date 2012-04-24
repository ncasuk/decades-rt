      PROGRAM H_DRS_SIM
C
C H_DRS_SIM.FOR
C
C Simulates the input of data from the DRS by reading data from a disk file.
C This program should be run at a high priority.
C
C This program requires four logical names to be defined in the PROCESS
C logical name table, before being run:
C
C H_DRS_SIM_FILE     The name of the data file to be used
C H_DRS_SIM_RATE     The data replay speed up rate (1-5)
C H_DRS_SIM_START    The data start time (hh:mm:ss or B)
C H_DRS_SIM_END      The data end time (hh:mm:ss or E)
C
C The file name must be defined, the rest will default to 1,B,E respectively if
C not defined.  Note that although the program will read data back at up to
C five times real-time, there is no guarantee that other processes will
C correctly handle data read at other than real-time (1Hz).  This option
C is included to help users and they should inspect the results before
C deciding to use H_DRS_SIM in speeded up mode.
C
C Build command:
C
C    $ LINK H_DRS_SIM,H_COMMON/OPT 
C
C    where H_COMMON.OPT contains the line SYS$LIBRARY:H_COMMON/SHAREABLE
C
C V1.00  01/01/89  W.D.N.JACKSON
C
C V1.01  17/10/90  W.D.N.JACKSON
C        Now stores time of latest event mark change in the STATUS array.
C        Now autocomputes the DRSDAT.DAT record size and puts in STATUS(6).
C
C V1.02  05/03/91  W.D.N.JACKSON
C        Now works between specified Sstart and end times, and will operate with
C        integer speed up factors between 1 and 5.
C
C V1.03  17/02/92  W.D.N.JACKSON
C        Now stores latest time in STATUS(27&28)
C
C V1.04  11/08/92  W.D.N.JACKSON
C        Now maintains STATUS(33) with 1 if there is valid data in NFDATA, else
C        0.  Valid means actual DRS data has been received within the last sec.
C        Now stores latest time in STATUS(27&28)
C
C V1.04  03/05/99  W.D.N.JACKSON
C        Now keep running across midnight.
C
      IMPLICIT  NONE
      INTEGER*4 ISTAT,IRECL,NAMSTRT,I,IERR,IREC,ISRTREC,LAST_JBLK,
     &    IBADBLKS,IBUFF,LAN,JBLK,IOS,J,IENDREC
      REAL*4    TIME1,TIME2,DELAY,TIMINT
      INTEGER*2 LASTEVM /-1/
      CHARACTER CFILE*80,CSTRING*6
      INCLUDE   'HCOM_DEF.FOR'
      INCLUDE   '($SYSSRVNAM)'
      INCLUDE   '($FORIOSDEF)'

C
C Start by associating a name with a common event flag cluster.  This
C cluster is common to all processes with the same UIC group number.
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
      CALL ST_MESS('Program starting')
      CALL LOG_MESS('H_DRS_SIM starting')
      ISTAT=SYS$ASCEFC(%VAL(64),'H_CEF_CLUSTER',,)
      IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      STATUS(33)=0
      CFDAT=' '                        !Latest flight date
      CFNUM=' '                        !Latest flight number
C
C Read flight data at a rate of one DRS block per second
C
      INQUIRE(FILE='H_DRS_SIM_FILE',NAME=CFILE,RECL=IRECL)  !Gets rec len in BYTES
      NAMSTRT=1
      DO I=1,80
        IF(CFILE(I:I).EQ.']')NAMSTRT=I+1
      END DO
      OPEN(UNIT=11,FILE='H_DRS_SIM_FILE',STATUS='OLD',IOSTAT=IERR,
     -     READONLY,ACCESS='DIRECT',RECL=(IRECL/4))
      IF(IERR.NE.0) THEN
        CALL LOG_MESS('Aborting due to file open error on '
     -      //CFILE(NAMSTRT:LAN(CFILE)))
        CALL FL_MESS('H_DRS_SIM aborted, failed to open input file')
        CALL ST_MESS('Aborting due to file open error on '
     -      //CFILE(NAMSTRT:LAN(CFILE)))
        CALL EXIT
      END IF
      CALL DRS_BLK_SIZ('GET')
      CALL RATETIMES(TIMINT,ISRTREC,IENDREC)
      IREC=ISRTREC
      LAST_JBLK=-1
      IBADBLKS=0
      IBUFF=1
      TIME1=SECNDS(0.0)+TIMINT
      DO WHILE (.TRUE.)
        READ(11,REC=IREC,IOSTAT=IERR) (NFDATA(J,IBUFF),J=1,IRECL/2)
        IF(IERR.EQ.0) THEN
          CALL ST_MESS('Reading data from '//CFILE(NAMSTRT:LAN(CFILE)))
          IF(NFDATA(2,IBUFF).EQ.16.AND.
     -       NFDATA(3,IBUFF).EQ.18.AND.
     -       NFDATA(4,IBUFF).EQ.20.AND.
     -       NFDATA(5,IBUFF).EQ.22.AND.
     -       NFDATA(6,IBUFF).EQ.26) THEN  !ISS block
            STATUS(1)=IBUFF
            CALL DRS_BLK_SIZ('ISS')
            ISTAT=SYS$SETEF(%VAL(66))
            CALL LOG_MESS('ISS block read')
            IBUFF=3-IBUFF
            IREC=IREC+1
            TIME2=SECNDS(0.0)
            DELAY=TIME1-TIME2
            IF(DELAY.GT.1000) THEN
              TIME1=TIME1-24*3600
              DELAY=DELAY-24*3600
            END IF
            CALL LIB$WAIT(DELAY)
            TIME1=TIME1+TIMINT
            STATUS(33)=0
          ELSE IF (NFDATA(4,IBUFF).EQ.0.AND.
     -      NFDATA(2,IBUFF).GE.0.AND.NFDATA(2,IBUFF).LT.100.AND.
     -      NFDATA(3,IBUFF).GE.0.AND.NFDATA(3,IBUFF).LT.100.AND.
     -      NFDATA(7,IBUFF).GE.0.AND.NFDATA(7,IBUFF).LE.'FFF'X.AND.
     -      NFDATA(8,IBUFF).GE.0.AND.NFDATA(8,IBUFF).LE.'FFF'X) THEN
            JBLK=JZEXT(NFDATA(8,IBUFF))+4096*JZEXT(NFDATA(7,IBUFF))
            STATUS(1)=IBUFF
            CALL DRS_BLK_SIZ('DRS')
            IF(LASTEVM.NE.NFDATA(13,STATUS(1))) THEN !Check for event mark change
              STATUS(14)=NFDATA(10,STATUS(1)) !Store DRS HHM time
              STATUS(15)=NFDATA(11,STATUS(1)) !Store DRS MSS time
              LASTEVM=NFDATA(13,STATUS(1))    !Note new event mark
            END IF     
            ISTAT=SYS$SETEF(%VAL(64))        
            ISTAT=SYS$SETEF(%VAL(65))        
            ISTAT=SYS$SETEF(%VAL(75))        
            STATUS(33)=1
            CALL UPD_FDAT(NFDATA(1,STATUS(1)),CFNUM,CFDAT)
            IF(JBLK.NE.LAST_JBLK+1) THEN
              IF(LAST_JBLK.NE.-1) THEN
                WRITE(CSTRING,10,IOSTAT=IOS) LAST_JBLK
                CALL LOG_MESS('End of section.    DRS block '
     -              //CSTRING(1:6))
              END IF
              IF(IBADBLKS.NE.0) THEN
                WRITE(CSTRING,10,IOSTAT=IOS) IBADBLKS
                CALL LOG_MESS(CSTRING(1:6)//' bad blocks skipped')
                IBADBLKS=0
              ENDIF
              WRITE(CSTRING,10,IOSTAT=IOS) JBLK
              CALL LOG_MESS('Start of section.  DRS block '
     -            //CSTRING(1:6))
            END IF
            LAST_JBLK=JBLK
            IBUFF=3-IBUFF
            IREC=IREC+1
            TIME2=SECNDS(0.0)
            DELAY=TIME1-TIME2
            IF(DELAY.GT.1000) THEN
              TIME1=TIME1-24*3600
              DELAY=DELAY-24*3600
            END IF
            CALL LIB$WAIT(DELAY)
            TIME1=TIME1+TIMINT
          ELSE
            IBADBLKS=IBADBLKS+1
            IREC=IREC+1
            STATUS(33)=0
          ENDIF                
          CALL STR_TIME(STATUS(27))    !Note latest time
        ELSE IF (IERR.EQ.FOR$IOS_ATTACCNON) THEN !Attempt to access non-
          IREC=ISRTREC                 !existent record at file end
        ELSE
          CALL LOG_MESS('Aborting due to file read error')
          CALL FL_MESS('H_DRS_SIM aborted on read error')
          CALL ST_MESS('Aborted due to file read error')
          CLOSE(UNIT=11)
          STATUS(33)=0
          CALL EXIT
        ENDIF
        IF(IREC.GT.IENDREC) IREC=ISRTREC
      END DO
10    FORMAT(I6)
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
      TYPE *,'H_DRS_SIM '//CDAT//' '//CTIM//MESSAGE
      RETURN
      END
********************************************************************************
      SUBROUTINE    ST_MESS(MESSAGE)
C
C Write message to the logical H_DRS_SIM_STAT in the group logical name
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
      ISTAT=SYS$CRELNM(,'LNM$GROUP','H_DRS_SIM_STAT',PSL$C_SUPER,TLIST)
      RETURN
      END
********************************************************************************
      SUBROUTINE    FL_MESS(MESSAGE)
C
C Sends an immediate message to the flight leader's display.  The message 
C is only sent when any existing output ends, so it should not interfere
C with graphics displays.  OPER privilege is required.
C
C V1.00  01/01/89  W.D.N.JACKSON
C
      IMPLICIT NONE
      INTEGER*4 ISTAT
      CHARACTER*(*) MESSAGE
      INCLUDE       '($SYSSRVNAM)'
      INCLUDE       '($BRKDEF)'

      ISTAT=SYS$BRKTHRU(,MESSAGE,'SBD_FL',%VAL(BRK$C_DEVICE),,,,,,,)
      RETURN
      END
********************************************************************************
      INTEGER*4 FUNCTION LAN(CSTRING)
C
C Returns the length of a string less any trailing spaces.  If the string is
C all spaces, a value of 1 is returned.
C
C V1.00  01/01/89  W.D.N.JACKSON
C
      IMPLICIT NONE
      INTEGER*4 N
      CHARACTER*(*) CSTRING

      N=LEN(CSTRING)                   !Get string length
      DO WHILE(N.GT.0.AND.CSTRING(N:N).EQ.' ') !Step back over spaces
        N=N-1
      END DO
      IF(N.EQ.0) N=1                   !Special case
      LAN=N
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
C V1.01  23/05/91  W.D.N.JACKSON
C        Bugs fixed to ensure correct operation when no info present
C
C V1.02  05/09/02  W.D.N.JACKSON
C        Now handles ISS up to 999
C
      IMPLICIT NONE
      INTEGER*4 I,IOS,JOS,KOS,IBS,ISS,MAXSIZ,MAXWRD
      CHARACTER*(*) CMODE
      CHARACTER CLINE*80,CSTRING*10
      INTEGER*4 IBLKSIZ(999)
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
      ELSE IF(CMODE.EQ.'ISS'.AND.STATUS(6).EQ.0) THEN     !ISS block is present
        ISS=NFDATA(1,STATUS(1)).AND.'FFF'X !Get ISS number
        IF(ISS.GE.1.AND.ISS.LE.999) THEN !Must be valid
          IF(IBLKSIZ(ISS).EQ.0) THEN    !If we don't already have a size
            I=2048                      !Search for end of ISS data mark (4095)
            DO WHILE(NFDATA(I,STATUS(1)).NE.4095.AND.I.GT.2)
              I=I-1
            END DO
            IF(NFDATA(I,STATUS(1)).EQ.4095) THEN !If mark found
              IBLKSIZ(ISS)=NFDATA(I-1,STATUS(1))/2 !Use final offset (in bytes)
              OPEN(UNIT=10,FILE='HORACE:DRSBLK_SIZ.DAT',STATUS='NEW',  
     -            CARRIAGECONTROL='LIST',IOSTAT=IOS) !Write new file
              WRITE(10,10,IOSTAT=IOS) '!' !Put on a header
              WRITE(10,10,IOSTAT=IOS) '! DRSBLK_SIZ.DAT - Number of '//
     -            'DRS words for each ISS value'
              WRITE(10,10,IOSTAT=IOS) '!'
              DO I=1,999                !Put in all known values
                IF(IBLKSIZ(I).NE.0) WRITE(10,13,IOSTAT=IOS) I,IBLKSIZ(I)
              END DO
              CLOSE(UNIT=10,IOSTAT=IOS) !Close output file
            END IF
          END IF
          STATUS(6)=((IBLKSIZ(ISS)-1)/256+1)*256 !Round sizes up to nearest blk
          WRITE(CSTRING,14,IOSTAT=IOS) IBLKSIZ(ISS),STATUS(6)
          CALL LOG_MESS('Using ISS and block sizes of '//CSTRING)
        END IF
      ELSE IF(CMODE.EQ.'DRS'.AND.STATUS(6).EQ.0) THEN     !DRS block is present
        ISS=NFDATA(1,STATUS(1)).AND.'FFF'X !Get ISS value
        IF(ISS.GE.1.AND.ISS.LE.999) THEN !Must be valid
          IF(IBLKSIZ(ISS).EQ.0) THEN   !If haven't got a value
            MAXSIZ=0                   !Calc present highest value
            DO I=1,999
              IF(IBLKSIZ(I).GT.MAXSIZ) MAXSIZ=IBLKSIZ(I)
            END DO    
            MAXWRD=2048                !Calc highest non-zero data
            DO WHILE(NFDATA(MAXWRD,STATUS(1)).EQ.0.AND.MAXWRD.GT.2)
              MAXWRD=MAXWRD-1
            END DO
            IBLKSIZ(ISS)=MAX(MAXWRD,MAXSIZ+128) !Take the largest
          END IF
          STATUS(6)=((IBLKSIZ(ISS)-1)/256+1)*256 !Round up to next block
          WRITE(CSTRING,14,IOSTAT=IOS) IBLKSIZ(ISS),STATUS(6)
          CALL LOG_MESS('Using ISS and block sizes of '//CSTRING)
        END IF
      END IF
10    FORMAT(A)
11    FORMAT(I3)
12    FORMAT(I4)
13    FORMAT(I3.3,I5.4)
14    FORMAT(2I5)
      RETURN
      END
********************************************************************************
      SUBROUTINE RATETIMES(TIMINT,ISRTREC,IENDREC)
C
C Supplies values for the speedup rate (1-5) and start and end records
C for H_DRS_SIM, based on definitions of the process logicals H_DRS_SIM_RATE,
C H_DRS_SIM_START, H_DRS_SIM_END.  If these are not specified, uses default
C values of 1, beginning and end.
C
C Note that unit 11 is used to access the raw data.
C
C V1.00  05/03/91  W.D.N.JACKSON
C
      IMPLICIT NONE
      INTEGER*4 NAMLEN,ISTAT,IRATE,IOS,ISRTTIM,JHRS,JMIN,JSEC,JOS,KOS,
     &    IENDTIM,IDRSTIM,ISRTREC,IENDREC,J1,J2
      REAL*4    TIMINT
      INTEGER*2 IBUFF(256)
      CHARACTER CTRAN*128
      INTEGER*4 ILIST(4)
      INTEGER*2 JLIST(8)
      EQUIVALENCE (ILIST,JLIST)
      INCLUDE '($SYSSRVNAM)'
      INCLUDE '($LNMDEF)'

      NAMLEN=0
      JLIST(1)=128
      JLIST(2)=LNM$_STRING
      ILIST(2)=%LOC(CTRAN)
      ILIST(3)=%LOC(NAMLEN)
      ILIST(4)=0
C
C Get the required reading rate, as an integer between 1 and 5, and turn
C into a time interval.  Use a default of 1Hz if any errors show.
C
      ISTAT=SYS$TRNLNM(,'LNM$PROCESS','H_DRS_SIM_RATE',,ILIST)
      IF(NAMLEN.GT.0) READ(CTRAN(1:1),*,IOSTAT=IOS) IRATE
      IF(.NOT.ISTAT.OR.NAMLEN.EQ.0.OR.IRATE.LT.1.OR.IRATE.GT.5
     -    .OR.IOS.NE.0) IRATE=1
      TIMINT=1.0/FLOAT(IRATE)
C
C Get start time - must be specified as hh:mm:ss.  If no value or an invalid
C value or B is specified a value of -1 is used to indicate to start at the
C start of the data.
C
      ISRTTIM=-1
      NAMLEN=0
      ISTAT=SYS$TRNLNM(,'LNM$PROCESS','H_DRS_SIM_START',,ILIST)
      IF(NAMLEN.GE.8.AND.CTRAN(3:3).EQ.':'.AND.CTRAN(6:6).EQ.':') THEN
        READ(CTRAN(1:2),*,IOSTAT=IOS) JHRS
        READ(CTRAN(4:5),*,IOSTAT=JOS) JMIN
        READ(CTRAN(7:8),*,IOSTAT=KOS) JSEC
        IF(IOS.EQ.0.AND.JOS.EQ.0.AND.KOS.EQ.0.AND.JHRS.LE.23.AND.
     -      JHRS.GE.0.AND.JMIN.LE.59.AND.JMIN.GE.0.AND.JSEC.LE.59.
     -      AND.JSEC.GE.0) ISRTTIM=3600*JHRS+60*JMIN+JSEC
      END IF
C
C Get end time - must be specified as hh:mm:ss.  If no value or an invalid
C value or E is specified a value of -1 is used to indicate to go to the
C end of the data.
C
      IENDTIM=-1
      NAMLEN=0
      ISTAT=SYS$TRNLNM(,'LNM$PROCESS','H_DRS_SIM_END',,ILIST)
      IF(NAMLEN.GE.8.AND.CTRAN(3:3).EQ.':'.AND.CTRAN(6:6).EQ.':') THEN
        READ(CTRAN(1:2),*,IOSTAT=IOS) JHRS
        READ(CTRAN(4:5),*,IOSTAT=JOS) JMIN
        READ(CTRAN(7:8),*,IOSTAT=KOS) JSEC
        IF(IOS.EQ.0.AND.JOS.EQ.0.AND.KOS.EQ.0.AND.JHRS.LE.23.AND.
     -      JHRS.GE.0.AND.JMIN.LE.59.AND.JMIN.GE.0.AND.JSEC.LE.59.
     -      AND.JSEC.GE.0) IENDTIM=3600*JHRS+60*JMIN+JSEC
      END IF
C
C Now work out the records in the data file which contain the start and end
C times.  Use a fairly simple approach, and if anything looks wrong then 
C simply use the start and end of the file.  Note that problems and 
C and inaccuracies may arise if there are breaks in the data, but these will
C not stop the program working.
C
      READ(UNIT=11,REC=1,IOSTAT=IOS) IBUFF
      J1=JZEXT(IBUFF(10))              !Get hours and mins
      J2=JZEXT(IBUFF(11))              !Get mins and secs
      JHRS=IBITS(J1,8,4)*10+IBITS(J1,4,4) !Compute hours
      JMIN=IBITS(J1,0,4)*10+IBITS(J2,8,4) !Compute minute
      JSEC=IBITS(J2,4,4)*10+IBITS(J2,0,4) !Compute seconds
      IDRSTIM=JHRS*3600+JMIN*60+JSEC   !Compute full time
      IF(IDRSTIM.GE.ISRTTIM) THEN
        ISRTREC=1
      ELSE
        ISRTREC=ISRTTIM-IDRSTIM+1
        READ(UNIT=11,REC=ISRTREC,IOSTAT=IOS) IBUFF
        IF(IOS.NE.0) ISRTREC=1
      END IF
      IF(IENDTIM.EQ.-1) THEN
        IENDREC=1000000
      ELSE
        IENDREC=IENDTIM-IDRSTIM+1
        READ(UNIT=11,REC=IENDREC,IOSTAT=IOS) IBUFF
        IF(IOS.NE.0) IENDREC=1000000
      END IF
      IF(IENDREC.LE.ISRTREC) THEN
        ISRTREC=1
        IENDREC=1000000
      END IF
      RETURN
      END
C*******************************************************************************
      SUBROUTINE STR_TIME(JSTORE)
C
C Stores the current time as a 32 bit integer seconds past midnight.
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
********************************************************************************
      SUBROUTINE UPD_FDAT(NDATA,CFLT,CDAT)
C
C Given a block of DRS data, returns the flight number and date as character
C strings.  If either is invalid it is returned as an empty string.
C
      IMPLICIT NONE
      INTEGER*4 J1,IHUN,ITEN,IUNI,J2,JDAY,JMON,JYER,IOS
      LOGICAL*4 LVFNO,LVDAT
      INTEGER*2 NDATA(2048)
      CHARACTER CFLT*4,CDAT*9
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
10    FORMAT('A',3I1)
11    FORMAT(I2.2,'-',A,'-',I2.2)
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
