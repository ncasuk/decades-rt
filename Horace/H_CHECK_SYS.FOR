      PROGRAM H_CHECK_SYS
C
C Carries out system checks on the DRS data stream and on the operation of the
C major HORACE software components.  Doesn't try to interpret the DRS data.
C
C Build as follows:
C
C    $ FORT H_CHECK_SYS
C    $ LINK H_CHECK_SYS,H_COMMON/OPT
C
C    where H_COMMON.OPT contains the line SYS$LIBRARY:H_COMMON/SHAREABLE
C
C V1.01  01/07/93  W.D.N.JACKSON - some changes to ensure continues to work
C with 16 bit data.  However does not check top 4 bits are changing at present.
C
C V1.02  11/04/95  W.D.N.JACKSON - now waits rather than aborts if H_CHECK_SYS.
C TXT is in use by another program.
C
C V1.03  03/01/96  W.D.N.JACKSON - parameter lists updated to reflect recent
C changes.  ISS alignment threshold changed from 1000 to 1500.
C
C V1.04  11/09/02  W.D.N.JACKSON - Integer*8s declared explicitly.
C
      IMPLICIT NONE
      CHARACTER CMESS(6)*20,CDDATE*9,CHDATE*9,CDTIME*8,CHTIME*8,CISS*26
      CHARACTER CMON(12)*3 /'JAN','FEB','MAR','APR','MAY','JUN',
     -                      'JUL','AUG','SEP','OCT','NOV','DEC'/
      CHARACTER CLINE*132,CPROT*50,CLINE1*80
      REAL*4 RDATRATE,RDATTIMA,RDATTIMB,RTIMLFTA,RTIMLFTB  
      INTEGER*4 IEVM,INSERPAR,JSEC1,IF,IOS,JYER,IFBA,IP,LDRS,IFBB,IPOS,
     -    LEARDRSA,IFLT,IRECL,LEARDRSB,IGPSDATA,IS,LEARGPSA,IGPSDATB,
     -    LEARGPSB,IHIGH,ISTAT,LMILLON,I,IHTIM,ISUM,LOK,IALLOC,IHUN,
     -    ITEN,LSAME,IBLK,ILOW,ITIM,LVDATE,IC,IMAXIP,ITOTDATA,LVEVM,
     -    ICTIM,IMAXP,ITOTDATB,LVFLT,ID,IMBA,IUNI,LVPROT,IDAT,IMBB,IVAL,
     -    LVTIME,IDATRATE,IMTA,LXISTS,IDATSPACEA,IMTB,IDATSPACEB,
     -    INDRSA,J2,IDBF,INDRSB,JDAY,IDRSDATA,INGPSA,JHRS1,IDRSDATB,
     -    INGPSB,JMIN1,IDTIM,INISSPAR,JMON,IERR,INMILPAR,JOS,J,J1,ISS,
     -    LIB$GETDVI
      INTEGER*2 ISS1(768),ISS2(768),NFDATA1(2048)
      INTEGER*4 IISSPAR(30),ISERPAR(80),IMILPAR(30)
      INCLUDE   'HCOM_DEF.FOR'
      INCLUDE   '($DVIDEF)'
C The following parameters are recorded through SIMON
      DATA ISERPAR /48,49,60,61,70,71,72,73,74,
     -    75,76,77,78,81,82,83,84,85,86,87,88,89,91,92,93,94,95,96,97,
     -    98,99,126,127,128,129,130,131,132,
     -    133,134,135,136,137,138,139,140,160,161,170,171,172,173,174,
     -    191,192,193,24*0/
      DATA INSERPAR /56/
C The following parameters are recorded through MILLIE
      DATA IMILPAR /5,7,8,9,10,11,14,18,19,23,24,37,41,42,47,58,
     -    59,144,145,146,147,148,149,152,153,5*0/
      DATA INMILPAR /25/
C The following parameters are used for checking ISS alignment.  They must be
C parameters that have an even frequency, and give a continuous output with
C no step changes or rollovers.
      DATA IISSPAR /8,9,18,19,48,70,126,23*0/
      DATA INISSPAR /7/

      CALL OPEN_MESS                   !Convert and open message file
C Check H_DRS_LOG is receiving data from the DRS
      ICTIM=NINT(SECNDS(0.0))          !Get current spm
      CALL GET_TIME(STATUS(27),ITIM)   !Get stored time
      LDRS=.FALSE.                     !False if not receiving DRS data
      IF(ITIM.EQ.0) THEN               !No data received yet
        CALL OUTMESS(6,9,0,CMESS)
      ELSE IF(ICTIM-ITIM.GT.2) THEN    !Not receiving data at present
        CALL SPMCTIM(ITIM,CMESS(1))
        CALL OUTMESS(6,10,1,CMESS)
      ELSE
        LDRS=.TRUE.                    !Receiving DRS data
      END IF
C Check H_GPS_LOG is receiving data from the GPS
      ICTIM=NINT(SECNDS(0.0))          !Get current spm
      CALL GET_TIME(STATUS(29),ITIM)   !Get stored time
      IF(ITIM.EQ.0) THEN               !No data received yet
        CALL OUTMESS(6,11,0,CMESS)
      ELSE IF(ICTIM-ITIM.GT.2) THEN    !Not receiving data at present
        CALL SPMCTIM(ITIM,CMESS(1))
        CALL OUTMESS(6,12,1,CMESS)
      END IF
C Check H_DISC is writing DRS data to hard disk
      ICTIM=NINT(SECNDS(0.0))          !Get current spm
      CALL GET_TIME(STATUS(19),ITIM)   !Get stored time
      IF(ITIM.EQ.0.) THEN              !No data written yet
        CALL OUTMESS(6,1,0,CMESS)
      ELSE IF(ICTIM-ITIM.GT.11) THEN   !Not writing data at present
        CALL SPMCTIM(ITIM,CMESS(1))
        CALL OUTMESS(6,2,1,CMESS)
      END IF
C Check H_OPTIC is writing DRS data to optical disk
      ICTIM=NINT(SECNDS(0.0))          !Get current spm
      CALL GET_TIME(STATUS(21),ITIM)   !Get stored time
      IF(ITIM.EQ.0) THEN               !No data written yet
        CALL OUTMESS(6,3,0,CMESS)
      ELSE IF(ICTIM-ITIM.GT.11) THEN   !Not writing data at present
        CALL SPMCTIM(ITIM,CMESS(1))
        CALL OUTMESS(6,4,1,CMESS)
      END IF
C Check H_GPS_LOG is writing GPS data to hard disk
      ICTIM=NINT(SECNDS(0.0))          !Get current spm
      CALL GET_TIME(STATUS(23),ITIM)   !Get stored time
      IF(ITIM.EQ.0) THEN               !No data written yet
        CALL OUTMESS(6,5,0,CMESS)
      ELSE IF(ICTIM-ITIM.GT.5) THEN    !Not writing data at present
        CALL SPMCTIM(ITIM,CMESS(1))
        CALL OUTMESS(6,6,1,CMESS)
      END IF
C Check H_GPS_LOG is writing GPS data to optical disk
      ICTIM=NINT(SECNDS(0.0))          !Get current spm
      CALL GET_TIME(STATUS(25),ITIM)   !Get stored time
      IF(ITIM.EQ.0) THEN               !No data written yet
        CALL OUTMESS(6,7,0,CMESS)
      ELSE IF(ICTIM-ITIM.GT.5) THEN    !Not writing data at present
        CALL SPMCTIM(ITIM,CMESS(1))
        CALL OUTMESS(6,8,1,CMESS)
      END IF
C Check H_DERIVE is not crashing
      ICTIM=NINT(SECNDS(0.0))          !Get current spm
      CALL GET_TIME(STATUS(31),ITIM)   !Get stored time
      IF(ABS(ICTIM-ITIM).LT.10) THEN   !If started within last 10s is crashing
        CALL OUTMESS(6,13,0,CMESS)
      END IF
C See if the hard and optical disks are mounted
      ISTAT=LIB$GETDVI(DVI$_MNT,,'DUA0:',IMTA,,)
      ISTAT=LIB$GETDVI(DVI$_MNT,,'DUB0:',IMTB,,)
C DRS checks
      IF(LDRS) THEN
C Check bit 13 set in first word of block
        IF(.NOT.BTEST(NFDATA(1,STATUS(1)),13)) 
     -      CALL OUTMESS(6,14,0,CMESS)
C Check ISS is reasonable (<1000)
        ISS=NFDATA(1,STATUS(1)).AND.'FFF'X
        IF(ISS.GE.999) THEN
          WRITE(CMESS(1),'(I4)') ISS
          CALL OUTMESS(6,15,1,CMESS)
        END IF
C Check ISS file exists on hard disk
        IF(ISS.LT.1000.AND.IMTA.EQ.1) THEN
          WRITE(CMESS(1),'(I3.3)') ISS
          CISS='DUA0:[HORACE]ISSxxx.DAT;  '
          WRITE(CISS(17:19),10,IOSTAT=IOS) ISS
          INQUIRE(FILE=CISS,EXIST=LXISTS)
          IF(.NOT.LXISTS) THEN
            CALL OUTMESS(6,27,1,CMESS)
          ELSE                
C Check ISS file on hard disk matches ISS values in memory
            DO I=1,768
              ISS2(I)=0
            END DO
            ISS2(1)=ISS
            IP=2
            DO I=1,512
              IF(NPFREQ(I).EQ.0) THEN
                ISS2(IP)=0
              ELSE
                ISS2(IP)=NPLOC(I)*2-2
                IMAXP=I
                IMAXIP=IP
              END IF
              IP=IP+1
            END DO
C            ISS2(IMAXIP+1)=ISS2(IMAXIP)+NPFREQ(IMAXP)*2
            ISS2(IMAXIP+1)=4095
C            ISS2(IMAXIP+2)=4095
C Read in the ISS file off hard disk
            OPEN(11,FILE=CISS,STATUS='OLD',READONLY,ACCESS='DIRECT',
     -          RECL=1536/4)
            READ(11,REC=1,IOSTAT=IOS) ISS1
            CLOSE(11)
C Compare them
            LSAME=.TRUE.
            DO I=1,768
              IF(ISS1(I).NE.ISS2(I)) LSAME=.FALSE.
            END DO
            IF(.NOT.LSAME) CALL OUTMESS(6,49,1,CMESS)
C Compare with any earlier one on hard disk
            CISS(19:20)='-1'
            OPEN(11,FILE=CISS,STATUS='OLD',READONLY,ACCESS='DIRECT',
     -          RECL=1536/4,IOSTAT=IOS)
            IF(IOS.EQ.0) THEN
              READ(11,REC=1,IOSTAT=IOS) ISS2
              CLOSE(11)
              LSAME=.TRUE.
              DO I=1,768
                IF(ISS1(I).NE.ISS2(I)) LSAME=.FALSE.
              END DO
              IF(.NOT.LSAME) CALL OUTMESS(6,28,1,CMESS)
            END IF
          END IF
        END IF
C Check ISS file exists on optical disk
        IF(ISS.LT.1000.AND.IMTB.EQ.1) THEN
          WRITE(CMESS(1),'(I3.3)') ISS
          CISS='DUB0:[HORACE]ISSxxx.DAT;  '
          WRITE(CISS(17:19),10,IOSTAT=IOS) ISS
          INQUIRE(FILE=CISS,EXIST=LXISTS)
          IF(.NOT.LXISTS) THEN
            CALL OUTMESS(6,47,1,CMESS)
          ELSE                
C Check ISS file on optical disk matches ISS values in memory
            DO I=1,768
              ISS2(I)=0
            END DO
            ISS2(1)=ISS
            IP=2
            DO I=1,512
              IF(NPFREQ(I).EQ.0) THEN
                ISS2(IP)=0
              ELSE
                ISS2(IP)=NPLOC(I)*2-2
                IMAXP=I
                IMAXIP=IP
              END IF
              IP=IP+1
            END DO
C            ISS2(IMAXIP+1)=ISS2(IMAXIP)+NPFREQ(IMAXP)*2
            ISS2(IMAXIP+1)=4095
C            ISS2(IMAXIP+2)=4095
C Read in the ISS file off optical disk
            OPEN(11,FILE=CISS,STATUS='OLD',READONLY,ACCESS='DIRECT',
     -          RECL=1536/4)
            READ(11,REC=1,IOSTAT=IOS) ISS1
            CLOSE(11)
C Compare them
            LSAME=.TRUE.
            DO I=1,768
              IF(ISS1(I).NE.ISS2(I)) LSAME=.FALSE.
            END DO
            IF(.NOT.LSAME) CALL OUTMESS(6,50,1,CMESS)
C Compare with any earlier one on optical disk
            CISS(19:20)='-1'
            OPEN(11,FILE=CISS,STATUS='OLD',READONLY,ACCESS='DIRECT',
     -          RECL=1536/4,IOSTAT=IOS)
            IF(IOS.EQ.0) THEN
              READ(11,REC=1,IOSTAT=IOS) ISS2
              CLOSE(11)
              LSAME=.TRUE.
              DO I=1,768
                IF(ISS1(I).NE.ISS2(I)) LSAME=.FALSE.
              END DO
              IF(.NOT.LSAME) CALL OUTMESS(6,48,1,CMESS)
            END IF
          END IF
        END IF
C Check as far as possible that ISS is aligned with DRS data.  Can only do this
C for parameters greater that 1 Hz which should always be more or less the same
C for each value in the one second block.
        ISUM=0
        DO I=1,INISSPAR
          IP=IISSPAR(I)
          IF(NPFREQ(IP).GT.1) THEN
            DO J=1,NPFREQ(IP)
              ISUM=ISUM+(NFDATA(NPLOC(IP)+J-1,STATUS(1)).AND.'FFF'X)*
     -            (2*MOD(J,2)-1)
            END DO
          END IF
        END DO
        IF(ABS(ISUM).GT.1500) THEN
          WRITE(CMESS(1),'(I3.3)') ISS
          CALL OUTMESS(6,29,1,CMESS)
        END IF
C Check all DRS bits go both high and low
        IHIGH='FFFFFFFF'X
        ILOW='00000000'X
        DO I=1,2048
          IHIGH=IHIGH.AND.NFDATA(I,STATUS(1))
          ILOW=ILOW.OR.NFDATA(I,STATUS(1))
        END DO
        IF((IHIGH.AND.'FFF'X).NE.0.OR.(ILOW.AND.'FFF'X).NE.'FFF'X) THEN
          CMESS(1)='------------'
          DO I=0,11
            IPOS=11-I+1
            IF(BTEST(IHIGH,I)) CMESS(1)(IPOS:IPOS)='1'
            IF(.NOT.BTEST(ILOW,I)) CMESS(1)(IPOS:IPOS)='0'
          END DO
          CALL OUTMESS(6,24,1,CMESS)
        END IF
C Check all bits in serial parameters go both high and low
        IHIGH='FFFFFFFF'X
        ILOW='00000000'X
        DO I=1,INSERPAR
          IP=ISERPAR(I)
          DO IS=1,NPFREQ(IP)
            IVAL=NFDATA(NPLOC(IP)+IS-1,STATUS(1)).AND.'FFF'X
            IHIGH=IHIGH.AND.IVAL
            ILOW=ILOW.OR.IVAL
          END DO
        END DO
        IF((IHIGH.AND.'FFF'X).NE.0.OR.(ILOW.AND.'FFF'X).NE.'FFF'X) THEN
          CMESS(1)='------------'
          DO I=0,11
            IPOS=11-I+1
            IF(BTEST(IHIGH,I)) CMESS(1)(IPOS:IPOS)='1'
            IF(.NOT.BTEST(ILOW,I)) CMESS(1)(IPOS:IPOS)='0'
          END DO
          CALL OUTMESS(6,30,1,CMESS)
        END IF
C Check all bits in MILLIE parameters go both high and low
        IHIGH='FFFFFFFF'X
        ILOW='00000000'X
        LMILLON=.FALSE.
        DO I=1,INMILPAR
          IP=IMILPAR(I)
          DO IS=1,NPFREQ(IP)
            IVAL=NFDATA(NPLOC(IP)+IS-1,STATUS(1)).AND.'FFF'X
            IF(IVAL.LT.4095) LMILLON=.TRUE.
            IHIGH=IHIGH.AND.IVAL
            ILOW=ILOW.OR.IVAL
          END DO
        END DO
        IF(.NOT.LMILLON) THEN
          CALL OUTMESS(6,51,0,CMESS)
        ELSE
          IF((IHIGH.AND.'FFF'X).NE.0.OR.(ILOW.AND.'FFF'X).NE.'FFF'X)THEN
            CMESS(1)='------------'
            DO I=0,11
              IPOS=11-I+1
              IF(BTEST(IHIGH,I)) CMESS(1)(IPOS:IPOS)='1'
              IF(.NOT.BTEST(ILOW,I)) CMESS(1)(IPOS:IPOS)='0'
            END DO
            CALL OUTMESS(6,31,1,CMESS)
          END IF
        END IF
C Check DBF is same as ISS
        IDBF=NFDATA(2,STATUS(1)).AND.'FFF'X
        IF(IDBF.NE.ISS) THEN
          WRITE(CMESS(1),'(I4)') ISS
          WRITE(CMESS(2),'(I4)') IDBF
          CALL OUTMESS(6,16,2,CMESS)
        END IF
C Check that IC code is 2
        IC=NFDATA(3,STATUS(1)).AND.'FFF'X
        IF(IC.NE.2) THEN
          WRITE(CMESS(1),'(I4)') IC
          CALL OUTMESS(6,17,1,CMESS)
        END IF
C Check DRS date is valid and the same as HORACE date
        LVDATE=.TRUE.                  !Valid date
        CDDATE='xx-xxx-xx'
        J1=NFDATA(5,STATUS(1)).AND.'FFF'X !Work out DRS date
        J2=NFDATA(6,STATUS(1)).AND.'FFF'X
        JDAY=IBITS(J1,8,4)*10+IBITS(J1,4,4)
        JMON=IBITS(J1,0,4)*10+IBITS(J2,8,4)
        JYER=IBITS(J2,4,4)*10+IBITS(J2,0,4)
        IF(JDAY.LT.1.OR.JDAY.GT.31) LVDATE=.FALSE.
        IF(JMON.LT.1.OR.JMON.GT.12) LVDATE=.FALSE.
        IF(JYER.LT.0.OR.JYER.GT.99) LVDATE=.FALSE.
        WRITE(CDDATE(1:2),11,IOSTAT=IOS) JDAY
        IF(JMON.GE.1.AND.JMON.LE.12) CDDATE(4:6)=CMON(JMON)
        WRITE(CDDATE(8:9),11,IOSTAT=IOS) JYER
        CALL DATE1(CHDATE)              !Get current HORACE date
        IF(CHDATE(1:1).EQ.' ') CHDATE(1:1)='0'
        IF(.NOT.LVDATE) THEN
          CALL OUTMESS(6,18,0,CMESS)
        ELSE IF(CDDATE.NE.CHDATE) THEN
          CMESS(1)=CDDATE
          CMESS(2)=CHDATE
          CALL OUTMESS(6,19,2,CMESS)
        END IF
C Check DRS block number is reasonable (ie < 24hours)
        J1=NFDATA(8,STATUS(1)).AND.'FFF'X
        J2=NFDATA(7,STATUS(1)).AND.'FFF'X
        IBLK=J1+4096*J2
        IF(IBLK.LT.0.OR.IBLK.GT.86400) THEN
          CALL OUTMESS(6,20,0,CMESS)
        END IF
C Check Flight number is valid
        J1=NFDATA(9,STATUS(1)).AND.'FFF'X !Work out flight number
        IHUN=IBITS(J1,8,4)
        ITEN=IBITS(J1,4,4)
        IUNI=IBITS(J1,0,4)
        LVFLT=.TRUE.
        IF(IHUN.GT.9.OR.IHUN.LT.0.OR.ITEN.GT.9.OR.ITEN.LT.0.OR.
     -      IUNI.GT.9.OR.IUNI.LT.0) LVFLT=.FALSE.
        IFLT=IHUN*100+ITEN*10+IUNI
        IF(IFLT.GT.999.OR.IFLT.LT.0) LVFLT=.FALSE.
        IF(.NOT.LVFLT) CALL OUTMESS(6,21,0,CMESS)
C Check DRS time is valid and within 5s of HORACE time
        LVTIME=.TRUE.                  !Valid time
        CDTIME='hh:mm:ss'
        J1=NFDATA(10,STATUS(1)).AND.'FFF'X !Work out DRS time
        J2=NFDATA(11,STATUS(1)).AND.'FFF'X
        JHRS1=IBITS(J1,8,4)*10+IBITS(J1,4,4)
        JMIN1=IBITS(J1,0,4)*10+IBITS(J2,8,4)
        JSEC1=IBITS(J2,4,4)*10+IBITS(J2,0,4)
        IF(JHRS1.LT.0.OR.JHRS1.GT.23) LVTIME=.FALSE.
        IF(JMIN1.LT.0.OR.JMIN1.GT.59) LVTIME=.FALSE.
        IF(JSEC1.LT.0.OR.JSEC1.GT.59) LVTIME=.FALSE.
        WRITE(CDTIME(1:2),11,IOSTAT=IOS) JHRS1
        WRITE(CDTIME(4:5),11,IOSTAT=IOS) JMIN1
        WRITE(CDTIME(7:8),11,IOSTAT=IOS) JSEC1
        CALL TIME(CHTIME)              !Get current HORACE time
        CALL CTIMSPM(CDTIME,IDTIM)
        CALL CTIMSPM(CHTIME,IHTIM)
        IF(.NOT.LVTIME) THEN
          CALL OUTMESS(6,22,0,CMESS)
        ELSE IF(ABS(IDTIM-IHTIM).GT.5) THEN
          CMESS(1)=CDTIME
          CMESS(2)=CHTIME
          CALL OUTMESS(6,23,2,CMESS)
        END IF
C Check event mark valid
        LVEVM=.TRUE.
        DO I=12,13
          J1=NFDATA(I,STATUS(1)).AND.'FFF'X
          IHUN=IBITS(J1,8,4)
          ITEN=IBITS(J1,4,4)
          IUNI=IBITS(J1,0,4)
          IF(IHUN.GT.9.OR.IHUN.LT.0.OR.ITEN.GT.9.OR.ITEN.LT.0.OR.
     -        IUNI.GT.9.OR.IUNI.LT.0) LVEVM=.FALSE.
          IEVM=IHUN*100+ITEN*10+IUNI
          IF(IEVM.GT.999.OR.IEVM.LT.0) LVEVM=.FALSE.
        END DO
        IF(.NOT.LVEVM) CALL OUTMESS(6,32,0,CMESS)
C Check deiced scale within range
        LOK=.TRUE.
        DO I=1,NPFREQ(11)
          IVAL=NFDATA(NPLOC(11)+I-1,STATUS(1)).AND.'FFF'X
          IF(IVAL.GT.7) LOK=.FALSE.
        END DO
        IF(.NOT.LOK) CALL OUTMESS(6,33,0,CMESS)
C Check non-deiced scale within range
        LOK=.TRUE.
        DO I=1,NPFREQ(24)
          IVAL=NFDATA(NPLOC(24)+I-1,STATUS(1)).AND.'FFF'X
          IF(IVAL.GT.7) LOK=.FALSE.
        END DO
        IF(.NOT.LOK) CALL OUTMESS(6,34,0,CMESS)
      END IF
C Check for non-zero error count on hard and optical disks
      IF(IMTA.EQ.1) THEN
        ISTAT=LIB$GETDVI(DVI$_ERRCNT,,'DUA0:',IERR,,)
        IF(ISTAT.AND.IERR.GT.0) THEN
          WRITE(CMESS(1)(1:4),'(I4)',IOSTAT=IOS) IERR
          CALL OUTMESS(6,25,1,CMESS)
        END IF
      END IF
      IF(IMTB.EQ.1) THEN
        ISTAT=LIB$GETDVI(DVI$_ERRCNT,,'DUB0:',IERR,,)
        IF(ISTAT.AND.IERR.GT.0) THEN
          WRITE(CMESS(1)(1:4),'(I4)',IOSTAT=IOS) IERR
          CALL OUTMESS(6,26,1,CMESS)
        END IF
      END IF
      if(ievm.ne.-99999) goto 999      !The rest is too slow and not needed
C Get some information about the disks
      IF(IMTA.EQ.1) THEN
        ISTAT=LIB$GETDVI(DVI$_MAXBLOCK,,'DUA0:',IMBA,,)
        ISTAT=LIB$GETDVI(DVI$_FREEBLOCKS,,'DUA0:',IFBA,,)
      END IF
      IF(IMTB.EQ.1) THEN
        ISTAT=LIB$GETDVI(DVI$_MAXBLOCK,,'DUB0:',IMBB,,)
        ISTAT=LIB$GETDVI(DVI$_FREEBLOCKS,,'DUB0:',IFBB,,)
        ISTAT=LIB$GETDVI(DVI$_VPROT,,'DUB0:',,CPROT,)
        IF(INDEX(CPROT,'WORLD=RW').EQ.0) CALL OUTMESS(6,36,0,CMESS)
      END IF
C Compute bytes being generated per second
      IDATRATE=STATUS(6)               !DRS data
      IDATRATE=IDATRATE+75             !Add GPS messages
      RDATRATE=REAL(IDATRATE*2)        !Convert words to bytes
C Spawn job to create a list of DRS and GPS files on hard and optical disks
      CALL LIB$SPAWN(,'H_CHECK_SYS.COM',,2+4+8,,,,,,,,)
C Open the file and process each entry
      OPEN(UNIT=11,FILE='H_CHECK_SYS.TMP',STATUS='OLD',DISPOSE='DELETE')
      ITOTDATA=0                       !Total allocated DRS and GPS data on DUA
      ITOTDATB=0                       !Total allocated DRS and GPS data on DUB
      LEARDRSA=.FALSE.                 !True if earlier files for same flight
      LEARGPSA=.FALSE.                 !True if earlier files for same flight
      LEARDRSB=.FALSE.                 !True if earlier files for same flight
      LEARGPSB=.FALSE.                 !True if earlier files for same flight
      IDRSDATA=0                       !DRS Space used for current flight on DUA
      IGPSDATA=0                       !GPS Space used for current flight on DUA
      IDRSDATB=0                       !DRS Space used for current flight on DUB
      IGPSDATB=0                       !GPS Space used for current flight on DUB
      INDRSA=0                         !No of DRSDAT file done so far on DUA0
      INGPSA=0                         !No of GPSDAT file done so far on DUA0
      INDRSB=0                         !No of DRSDAT file done so far on DUB0
      INGPSB=0                         !No of GPSDAT file done so far on DUB0
      LVPROT=.TRUE.                    !True if optical disk Vol prot is OK
      CALL FLTDATDRS(NFDATA(1,STATUS(1)),IFLT,IDAT) !Get current flight and date
      READ(11,12,IOSTAT=IOS) CLINE     !Read first line
      DO WHILE(IOS.EQ.0)
C Process files on DUA0
        IF(INDEX(CLINE,'DUA0').NE.0) THEN
          IF(INDEX(CLINE,'.DAT').NE.0) THEN
            READ(CLINE(33:42),13) IALLOC
            ITOTDATA=ITOTDATA+IALLOC
          END IF
C Process DRS files on DUA0
          IF(INDEX(CLINE,'DRSDAT.DAT').NE.0) THEN
            INDRSA=INDRSA+1
            INQUIRE(FILE=CLINE(1:30),RECL=IRECL)
            OPEN(UNIT=12,FILE=CLINE(1:30),STATUS='OLD',ACCESS='DIRECT',
     -          RECL=IRECL/4,IOSTAT=IOS)
            READ(12,REC=1,IOSTAT=IOS) (NFDATA1(I),I=1,IRECL/2)
            CLOSE(12,IOSTAT=JOS)
            IF(IOS.EQ.0) THEN
              CALL FLTDATDRS(NFDATA1,IF,ID)
              IF(ID.NE.-1.AND.IDAT.NE.-1) THEN
                IF(IF.EQ.IFLT) THEN
                  IF(IDAT-ID.GT.15*3600) THEN
                    LEARDRSA=.TRUE.
                  ELSE
                    IDRSDATA=IDRSDATA+IALLOC
                  END IF
                END IF
              END IF
            ELSE
              IF(INDRSA.EQ.1) IDRSDATA=IDRSDATA+IALLOC
            END IF
          END IF
C Process GPS files on DUA0
          IF(INDEX(CLINE,'GPSDAT.DAT').NE.0) THEN
            INGPSA=INGPSA+1
            OPEN(UNIT=12,FILE=CLINE(1:30),STATUS='OLD',IOSTAT=IOS)
            READ(12,'(A)',IOSTAT=IOS) CLINE1
            CLOSE(12,IOSTAT=JOS)
            IF(IOS.EQ.0) THEN
              CALL FLTDATGPS(CLINE1,IF,ID)
              IF(ID.NE.-1.AND.IDAT.NE.-1) THEN
                IF(IF.EQ.IFLT) THEN
                  IF(IDAT-ID.GT.15*3600) THEN
                    LEARGPSA=.TRUE.
                  ELSE
                    IGPSDATA=IGPSDATA+IALLOC
                  END IF
                END IF
              END IF
            ELSE
              IF(INGPSA.EQ.1) IGPSDATA=IGPSDATA+IALLOC
            END IF
          END IF
        END IF
C Now process files on DUB0
        IF(INDEX(CLINE,'DUB0').NE.0) THEN
          IF(INDEX(CLINE,'[000000]HORACE.DIR').NE.0) THEN
            I=INDEX(CLINE,',')
            I=INDEX(CLINE(I+1:),',')
            I=INDEX(CLINE(I+1:),',')
            IF(INDEX(CLINE(I+1:),'RW').EQ.0) CALL OUTMESS(6,37,0,CMESS)
          END IF
          IF(INDEX(CLINE,',RWED)').EQ.0) LVPROT=.FALSE.
          IF(INDEX(CLINE,'.DAT').NE.0) THEN
            READ(CLINE(33:42),13) IALLOC
            ITOTDATB=ITOTDATB+IALLOC
          END IF
          IF(INDEX(CLINE,'.DAT').NE.0) THEN
            READ(CLINE(33:42),13) IALLOC
            ITOTDATA=ITOTDATA+IALLOC
          END IF
C Process DRS files on DUB0
          IF(INDEX(CLINE,'DRSDAT.DAT').NE.0) THEN
            INDRSB=INDRSB+1
            INQUIRE(FILE=CLINE(1:30),RECL=IRECL)
            OPEN(UNIT=12,FILE=CLINE(1:30),STATUS='OLD',ACCESS='DIRECT',
     -          RECL=IRECL/4,IOSTAT=IOS)
            READ(12,REC=1,IOSTAT=IOS) (NFDATA1(I),I=1,IRECL/2)
            CLOSE(12,IOSTAT=JOS)
            IF(IOS.EQ.0) THEN
              CALL FLTDATDRS(NFDATA1,IF,ID)
              IF(ID.NE.-1.AND.IDAT.NE.-1) THEN
                IF(IF.EQ.IFLT) THEN
                  IF(IDAT-ID.GT.15*3600) THEN
                    LEARDRSB=.TRUE.
                  ELSE
                    IDRSDATB=IDRSDATB+IALLOC
                  END IF
                END IF
              END IF
            ELSE
              IF(INDRSB.EQ.1) IDRSDATB=IDRSDATB+IALLOC
            END IF
          END IF
C Process GPS files on DUB0
          IF(INDEX(CLINE,'GPSDAT.DAT').NE.0) THEN
            INGPSB=INGPSB+1
            OPEN(UNIT=12,FILE=CLINE(1:30),STATUS='OLD',IOSTAT=IOS)
            READ(12,'(A)',IOSTAT=IOS) CLINE1
            CLOSE(12,IOSTAT=JOS)
            IF(IOS.EQ.0) THEN
              CALL FLTDATGPS(CLINE1,IF,ID)
              IF(ID.NE.-1.AND.IDAT.NE.-1) THEN
                IF(IF.EQ.IFLT) THEN
                  IF(IDAT-ID.GT.15*3600) THEN
                    LEARGPSB=.TRUE.
                  ELSE
                    IGPSDATB=IGPSDATB+IALLOC
                  END IF
                END IF
              END IF
            ELSE
              IF(INGPSB.EQ.1) IGPSDATB=IGPSDATB+IALLOC
            END IF
          END IF
        END IF
        READ(11,12,IOSTAT=IOS) CLINE   !Read next line
      END DO
      CLOSE(11)
      IF(.NOT.LVPROT) CALL OUTMESS(6,38,0,CMESS)
C Check reasonable data space on hard disk
      IF(IMTA.EQ.1) THEN
        IDATSPACEA=ITOTDATA+IFBA
        RDATTIMA=REAL(IDATSPACEA*512)/RDATRATE/3600.
        IF(RDATTIMA.LT.10.0) THEN
          WRITE(CMESS(1)(1:4),'(F4.1)') RDATTIMA
          CALL OUTMESS(6,39,1,CMESS)
        END IF
      END IF
C Check reasonable data space on optical disk
      IF(IMTB.EQ.1) THEN
        IDATSPACEB=ITOTDATB+IFBB
        RDATTIMB=REAL(IDATSPACEB*512)/RDATRATE/3600.
        IF(RDATTIMB.LT.10.0) THEN
          WRITE(CMESS(1)(1:4),'(F4.1)') RDATTIMB
          CALL OUTMESS(6,40,1,CMESS)
        END IF
      END IF
C Check space left for this flight on hard disk
      IF(IMTA.EQ.1) THEN
        RTIMLFTA=REAL((IDATSPACEA-IDRSDATA-IGPSDATA)*512)/RDATRATE/3600.
        WRITE(CMESS(1),'(F4.1)',IOSTAT=IOS) RTIMLFTA
        IF(RTIMLFTA.LT.1.0) CALL OUTMESS(6,41,1,CMESS)
      END IF
C Check space left for this flight on optical disk
      IF(IMTB.EQ.1) THEN
        RTIMLFTB=REAL((IDATSPACEB-IDRSDATB-IGPSDATB)*512)/RDATRATE/3600.
        WRITE(CMESS(1),'(F4.1)',IOSTAT=IOS) RTIMLFTB
        IF(RTIMLFTB.LT.1.0) CALL OUTMESS(6,42,1,CMESS)
      END IF
C Message if earlier DRS data file for this flight on hard disk
      IF(LEARDRSA) CALL OUTMESS(6,43,0,CMESS)
C Message if earlier GPS data file for this flight on hard disk
      IF(LEARGPSA) CALL OUTMESS(6,44,0,CMESS)
C Message if earlier DRS data file for this flight on optical disk
      IF(LEARDRSB) CALL OUTMESS(6,45,0,CMESS)
C Message if earlier GPS data file for this flight on optical disk
      IF(LEARDRSB) CALL OUTMESS(6,46,0,CMESS)
C Give final success message if no other messages
999   CALL OUTMESS(6,35,0,CMESS)
      CLOSE(10)
      CALL EXIT
10    FORMAT(I3.3)
11    FORMAT(I2.2)
12    FORMAT(A)
13    FORMAT(I10)
      END
********************************************************************************
      SUBROUTINE OPEN_MESS
C
C Opens the error message file H_CHECK_SYS.TXT.  If it is in variable length
C format it has probably recently been edited, and it is converted into the
C fixed length 80 byte record format required by this program for direct
C access of the error messsages.
C
      IMPLICIT NONE
      INTEGER*4 I,IOS,ICNT
      CHARACTER CLINE*80,CTYPE*20

      INQUIRE(FILE='H_CHECK_SYS.TXT',RECORDTYPE=CTYPE)
      IF(CTYPE.EQ.'VARIABLE') THEN
        OPEN(10,FILE='H_CHECK_SYS.TXT',STATUS='OLD')
        OPEN(11,FILE='H_CHECK_SYS.TXT',STATUS='NEW',ACCESS='DIRECT',
     -      RECL=20)
        READ(10,10,IOSTAT=IOS) CLINE
        I=1
        DO WHILE(IOS.EQ.0)
          WRITE(11,REC=I) CLINE
          I=I+1
          READ(10,10,IOSTAT=IOS) CLINE
        END DO
        CLOSE(10)
        CLOSE(11)
      END IF
      OPEN(UNIT=10,FILE='H_CHECK_SYS.TXT',STATUS='OLD',
     -    ACCESS='DIRECT',RECL=20,IOSTAT=IOS,READONLY)
      ICNT=0
      IF(IOS.NE.0) THEN
        IF(ICNT.EQ.0) THEN
          CALL LOG_MESS('Waiting to open H_CHECK_SYS.TXT')
          ICNT=1
        END IF
        CALL LIB$WAIT(10.0)
        OPEN(UNIT=10,FILE='H_CHECK_SYS.TXT',STATUS='OLD',
     -      ACCESS='DIRECT',RECL=20,IOSTAT=IOS,READONLY)
      END IF
      IF(ICNT.NE.0) CALL LOG_MESS('H_CHECK_SYS.TXT opened')
      RETURN
10    FORMAT(A)
      END
********************************************************************************
      SUBROUTINE OUTMESS(LUN,IMNUM,ICNUM,CMES)
C
C Gets the specified message from the message file and outputs it to the
C specified unit.  Any words in the message consisting of asterisks are
C replaced with the words specified by the CMESS array.
C
      IMPLICIT NONE
      INTEGER*4 LUN,IMNUM,ICNUM,ICNT,I,J,LAN,IW,K,L,IOS
      CHARACTER  CMESS*80,CM(6)*20,CMES(6)*20
      DATA       ICNT /0/

      IF(IMNUM.EQ.35.AND.ICNT.NE.0) RETURN
      ICNT=ICNT+1

      DO I=1,6
        IF(ICNUM.GE.I) THEN
          CM(I)=CMES(I)
        ELSE
          CM(I)=' '
        END IF
      END DO

      READ(10,REC=IMNUM) CMESS
      J=LAN(CMESS)
      IW=1                              !Asterisked word in message
      K=1                               !Character in asterisked word
      DO L=1,J
        IF(CMESS(L:L).EQ.'*') THEN
          CMESS(L:L)=CM(IW)(K:K)
          K=K+1
          IF(L.LE.79.AND.CMESS(L+1:L+1).NE.'*') THEN
            IW=IW+1
            K=1
          END IF
        END IF
      END DO
      WRITE(LUN,10,IOSTAT=IOS) CMESS(6:LAN(CMESS))
      RETURN
10    FORMAT(A)
      END
********************************************************************************
C
C ROUTINE          LAN FUNCTION FORTVAX
C
C PURPOSE          Returns length of a string less trailing blanks
C
C DESCRIPTION      Function returns the length of a string not including
C                  trailing blanks. If the string is all blanks, or is of zero
C                  length, the function returns a value of 1.
C
C VERSION          1.00  9-1-90  W.D.N.JACKSON
C
C ARGUMENTS        CSTRING  C**  IN   String whose length to be found
C                  LAN      I*4  OUT  String length less trailing spaces
C
C*******************************************************************************
      INTEGER*4 FUNCTION LAN(CSTRING)
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
C*******************************************************************************
      SUBROUTINE GET_TIME(JSTORE,ITIM)
C
C Gets I*4 number from I*2 array.
C
      IMPLICIT NONE
      INTEGER*4 ITIM,ITEMP
      INTEGER*2 JSTORE(2),JTEMP(2)
      EQUIVALENCE (JTEMP,ITEMP)
      JTEMP(1)=JSTORE(1)
      JTEMP(2)=JSTORE(2)
      ITIM=ITEMP
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
      INTEGER*4     ISECS         !Time in secs past midnight
      CHARACTER*(*) CTIM          !Character string to hold time
      INTEGER*4     IH,IM,IS,IOS,ITIM

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
C*******************************************************************************
C
C ROUTINE          CTIMSPM SUBROUTINE FORTVAX
C
C PURPOSE          Converts hh:mm:ss character string to seconds past midnight
C
C DESCRIPTION      Given an eight character time string in the form hh:mm:ss,
C                  returns the time as seconds past midnight.  Returns -1 if
C                  the character string contains an invalid time.
C
C VERSION          1.00  10-1-90  W.D.N.JACKSON
C
C ARGUMENTS        CTIM     C*8  IN   Character time as hh:mm:ss
C                  ISECS    I*4  OUT  Time in seconds past midnight
C
C*******************************************************************************
      SUBROUTINE CTIMSPM(CTIM,ISECS)
      IMPLICIT NONE
      CHARACTER*(*) CTIM          !Character string to hold time
      INTEGER*4     ISECS         !Time in secs past midnight
      INTEGER*4     IOS,JOS,KOS,JHRS,JMIN,JSEC
      CHARACTER     CTMP*8

      CTMP=CTIM                   !Use temporary in case CTIM has wrong length
      READ(CTMP(1:2),10,IOSTAT=IOS) JHRS !Decode hours
      READ(CTMP(4:5),10,IOSTAT=JOS) JMIN !Decode minutes
      READ(CTMP(7:8),10,IOSTAT=KOS) JSEC !Decode seconds
      IF(IOS.NE.0.OR.JOS.NE.0.OR.KOS.NE.0.OR.JHRS.LT.0.OR.JHRS.GT.23
     -    .OR.JMIN.LT.0.OR.JMIN.GT.59.OR.JSEC.LT.0.OR.JSEC.GT.59.
     -    .OR.CTMP(3:3).NE.':'.OR.CTMP(6:6).NE.':') THEN
        ISECS=-1
      ELSE
        ISECS=3600*JHRS+60*JMIN+JSEC
      END IF
      RETURN
10    FORMAT(I2)
      END
********************************************************************************
      SUBROUTINE FLTDATDRS(NDATA,IFLT,ITIM)
C
C Given a block of DRS data, returns the flight number and time as seconds
C from the beginning of the year.  If the values can't be ascertained they
C are returned as -1s.
C
      IMPLICIT NONE
      INTEGER*2  NDATA(2048),JTIM(7)
      INTEGER*4  IFLT,J1,IHUN,ITEN,IUNI,J2,JDAY,JMON,JYER,JHRS,
     -   JMIN,JSEC,ISTAT,JSTAT,LIB$CVT_VECTIM,ITIM,
     -   LIB$CVT_FROM_INTERNAL_TIME
      INTEGER*8  IQTIM
      LOGICAL*4 LVTIM
      INCLUDE    '($LIBDTDEF)'

      J1=NDATA(9).AND.'FFF'X           !Flight number
      IHUN=IBITS(J1,8,4)
      ITEN=IBITS(J1,4,4)
      IUNI=IBITS(J1,0,4)
      IF(IHUN.GT.9.OR.IHUN.LT.0.OR.ITEN.GT.9.OR.ITEN.LT.0.OR.
     -    IUNI.GT.9.OR.IUNI.LT.0) THEN
        IFLT=-1
      ELSE
        IFLT=IHUN*100+ITEN*10+IUNI
      END IF

      LVTIM=.TRUE.

      J1=NDATA(5).AND.'FFF'X           !Work out DRS date
      J2=NDATA(6).AND.'FFF'X
      JDAY=IBITS(J1,8,4)*10+IBITS(J1,4,4)
      JMON=IBITS(J1,0,4)*10+IBITS(J2,8,4)
      JYER=IBITS(J2,4,4)*10+IBITS(J2,0,4)
      IF(JDAY.LT.1.OR.JDAY.GT.31) LVTIM=.FALSE.
      IF(JMON.LT.1.OR.JMON.GT.12) LVTIM=.FALSE.
      IF(JYER.LT.0.OR.JYER.GT.99) LVTIM=.FALSE.

      J1=NDATA(10).AND.'FFF'X          !Work out DRS time
      J2=NDATA(11).AND.'FFF'X
      JHRS=IBITS(J1,8,4)*10+IBITS(J1,4,4)
      JMIN=IBITS(J1,0,4)*10+IBITS(J2,8,4)
      JSEC=IBITS(J2,4,4)*10+IBITS(J2,0,4)
      IF(JHRS.LT.0.OR.JHRS.GT.23) LVTIM=.FALSE.
      IF(JMIN.LT.0.OR.JMIN.GT.59) LVTIM=.FALSE.
      IF(JSEC.LT.0.OR.JSEC.GT.59) LVTIM=.FALSE.

      JTIM(1)=JYER+1900
      IF(JYER.LT.90) JTIM(1)=JTIM(1)+100
      JTIM(2)=JMON
      JTIM(3)=JDAY
      JTIM(4)=JHRS
      JTIM(5)=JMIN
      JTIM(6)=JSEC
      JTIM(7)=0

      ISTAT=LIB$CVT_VECTIM(JTIM,IQTIM) !Convert to internal 64 fit format
      JSTAT=LIB$CVT_FROM_INTERNAL_TIME(LIB$K_SECOND_OF_YEAR,ITIM,IQTIM) !Secs
      IF(.NOT.LVTIM.OR..NOT.ISTAT.OR..NOT.JSTAT) ITIM=-1
      RETURN
      END
********************************************************************************
      SUBROUTINE FLTDATGPS(CMESS,IFLT,ITIM)
C
C Given a GPS message 0, returns the flight number and time as seconds
C from the beginning of the year.  If the values can't be ascertained they
C are returned as -1s.
C
      IMPLICIT NONE
      CHARACTER CMESS*80
      INTEGER*4 IFLT,ITIM,SYS$BINTIM,IOS,ISTAT,
     -    LIB$CVT_FROM_INTERNAL_TIME,JSTAT
      INTEGER*8 IQTIM
      INCLUDE '($LIBDTDEF)'

      READ(CMESS(12:14),'(I3)',IOSTAT=IOS) IFLT
      IF(IOS.NE.0.OR.IFLT.LT.0.OR.IFLT.GT.999) IFLT=-1
      IF(CMESS(24:25).LT.'90') THEN
        ISTAT=SYS$BINTIM(CMESS(17:23)//'19'//CMESS(24:26)//CMESS(28:35)
     -      //'.00',IQTIM)
      ELSE
        ISTAT=SYS$BINTIM(CMESS(17:23)//'20'//CMESS(24:26)//CMESS(28:35)
     -      //'.00',IQTIM)
      END IF
      JSTAT=LIB$CVT_FROM_INTERNAL_TIME(LIB$K_SECOND_OF_YEAR,ITIM,IQTIM) !Secs
      IF(.NOT.ISTAT.OR..NOT.JSTAT) ITIM=-1
      RETURN
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
      TYPE *,'H_CHECK_SYS '//CDAT//' '//CTIM//MESSAGE
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
      CHARACTER CMON(12)*3,CDATE*9,CDATE1*8,CDUMM1*10,CDUMM2*5
      DATA      CMON /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     &    'SEP','OCT','NOV','DEC'/

      CALL DATE_AND_TIME(CDATE1,CDUMM1,CDUMM2,IVAL)
      WRITE(CDATE,'(A)') CDATE1(7:8)//'-'//CMON(IVAL(2))//'-'//
     &    CDATE1(3:4)
      RETURN
      END
