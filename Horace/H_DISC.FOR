      PROGRAM H_DISC
C
C H_DISC.FOR
C
C Handles the logging to disc of DRS data. 
C This program should be run at a high priority, less than H_DRS_LOG but
C more than other processes.
C
C The program:
C
C    a.  Collects 10s of data from the DRS.  Only part
C        of each block is collected at present.  IDRSBLK has values up to 2048
C        in steps of 256 to determine the amount of each DRS block stored.
C
C    b.  A single block of 10 DRS blocks is then written to tape - large blocks
C        enable more efficient disc transfers to take place.
C
C    c.  While the data block is being written to disc the program 
C        collects further 10 DRS block blocks in an alternative buffer.
C
C    d.  The buffering and writing continues indefinitely.
C
C    e.  The program will only stop if aborted.
C
C    f.  The program will also stop if it fails to write a block.
C
C    g.  Note that some data may not be recorded when the program
C        is terminated.
C
C Build command:
C
C    $ LINK H_DISC,HCOM/OPT
C
C    where HCOM.OPT contains the following line:
C
C     SYS$LIBRARY:HCOM/SHAREABLE
C
C V1.00            W.D.N.JACKSON
C V1.01  20/09/90  W.D.N.JACKSON
C        Modified to write DRSDAT files to HORACE: rather than DU:[HORACE]
C V1.02  17/02/92  W.D.N.JACKSON
C        Now notes time of each successful write in STATUS(19) and STATUS(20),
C        for use by checking routines.
C V1.03  03/08/92  W.D.N.JACKSON
C        Now uses the H_MESSAGE facility to deliver messages to the flight
C        leader.  Improved messages to log file.
C V1.04  03/01/96  W.D.N.JACKSON
C        Now starts a new file if the flight number changes.
C
      IMPLICIT  NONE
      INTEGER*4 IDRSBLK,ISTAT,J,JFNO,JBLK,IOS,ICHECKD,ILUN,IBLKS,INDEX,
     &    OUTDEX,FREE_BLKS,LAST_JBLK,LAST_JFNO,IERR,LIB$SPAWN,I
      LOGICAL*4 LWAITING,FILE_OPEN,LTRYING,WRITING
      INTEGER*2 HBUF(40960)
      CHARACTER CTEXT1*6,CTEXT2*6
      INCLUDE   'HCOM_DEF.FOR'
      INCLUDE   '($SYSSRVNAM)'
      INCLUDE   '($SSDEF)'
      INCLUDE   '($IODEF)'
      INCLUDE   '($CLIDEF)'
      INCLUDE   '($RMSDEF)'
      DATA ILUN /10/           !FORTRAN logical unit for disc

      LWAITING=.FALSE.
      STATUS(19)=0
      STATUS(20)=0
      DO WHILE (STATUS(6).EQ.0) !Wait for H_DRS_LOG or H_DRS_SIM to start
        IF(.NOT.LWAITING) THEN
          CALL ST_MESS('Waiting for DRS block size to be set')
          CALL LOG_MESS('Waiting for DRS block size to be set')
          LWAITING=.TRUE.
        END IF
        CALL LIB$WAIT(10.0)
      END DO
      IDRSBLK=STATUS(6)        !No of DRS words to log, in 256 word steps
C
C Start by associating a name with a common event flag cluster.  This
C cluster is common to all processes with the same UIC group number.
C
C The flags are used as follows:
C
C  Flag  75        Set by H_DRS_SIM or H_DRS_LOG when a new DRS data block 
C                  is in global common. Used and cleared by H_DISC.
C
      CALL LOG_MESS('Starting')
      CALL ST_MESS('Starting')
      ISTAT=SYS$ASCEFC(%VAL(64),'H_CEF_CLUSTER',,)
      IF(ISTAT.NE.SS$_NORMAL) CALL LIB$SIGNAL(%VAL(ISTAT))
C
C Spawn the H_DISC_1 subprocess to continuously monitor disc space and to
C delete the oldest version of DRSDAT.DAT when the disc is nearly full.
C
      ISTAT=LIB$SPAWN(,'H_DISC_1.COM','H_DISC_1.LOG',CLI$M_NOWAIT,
     -                 'H_DISC_1',,,,,,,)
      IF(ISTAT.NE.SS$_NORMAL) THEN
        CALL LOG_MESS('Failed to spawn H_DISC_1.  Continuing')
        CALL HM_MESS
     -  ('H_DISC_1 not spawned - Free disc space manually if necessary')
      ELSE
        CALL LOG_MESS('H_DISC_1 spawned to manage disc space')
      ENDIF
C
C Open DRSDAT.DAT giving it the next highest version number.
C
      FILE_OPEN=.FALSE.
      LTRYING=.FALSE.
      DO WHILE(.NOT.FILE_OPEN)
        CALL OPEND(IDRSBLK,ILUN,IERR)
        IF(IERR.EQ.0) THEN
          IF(LTRYING) THEN
            CALL LOG_MESS('DRSDAT.DAT now open')
            CALL HM_MESS('Have now opened DRSDAT.DAT on system disk')
          END IF
          FILE_OPEN=.TRUE.
        ELSE
          IF(.NOT.LTRYING) THEN
            CALL LOG_MESS('Failed to open DRSDAT.DAT')
            CALL ST_MESS('Trying to open DRSDAT.DAT')
            CALL HM_MESS('Unable to open DRSDAT.DAT on system disk.  '
     -          //'Will keep trying')
            LTRYING=.TRUE.
          END IF
          CALL LIB$WAIT(60.0)
        END IF
      END DO        
C
C Start collecting DRS data
C
      CALL LOG_MESS('Starting DRS data collection')
      CALL ST_MESS('Transferring DRS data to disc')
      WRITING=.FALSE.     !True if have disc write waiting to complete
      IBLKS=0             !No of DRS blocks written to disc
      INDEX=1             !Pointer to where next DRS word is to be put
      OUTDEX=1            !Pointer to where next disc write is to be from
      FREE_BLKS=20        !No of DRS block HBUF still has space for
      LAST_JBLK=-1        !DRS block number of last DRS block received
      LAST_JFNO=-1        !Flight number in the last DRS block received
C
C 10 DRS blocks are written to disc at a time as a single disc block, 
C although they will appear as individual records when read back.
C Collect 10 DRS blocks, each time clearing Common Event Flag 75 and then
C waiting for it to be set by H_DRS_LOG or H_DRS_SIM.  After each block check
C for completion of any previous disc write.  Then write any further disc
C blocks awaiting writing.  Up to 2 disc blocks can be stored before DRS data
C is lost.
C
      DO WHILE(.TRUE.)                 !Loop indefinitely
        ISTAT=SYS$CLREF(%VAL(75))      !Wait for next DRS block
        ISTAT=SYS$WAITFR(%VAL(75))
        IF(FREE_BLKS.GT.0) THEN        !If still some buffer space
          J=STATUS(1)
          JBLK=JZEXT(NFDATA(8,J))+4096*JZEXT(NFDATA(7,J))
          JFNO=NFDATA(9,J)
          IF(JFNO.NE.LAST_JFNO.AND.LAST_JFNO.NE.-1) THEN
            WRITE(CTEXT1,'(Z6.3)',IOSTAT=IOS) JFNO
            WRITE(CTEXT2,'(Z6.3)',IOSTAT=IOS) LAST_JFNO
            CALL LOG_MESS('Flight number changed from '//CTEXT2(4:6)
     -          //' to '//CTEXT1(4:6))
            CALL NEW_FILE(ILUN,WRITING,IBLKS,FREE_BLKS,OUTDEX,IDRSBLK,
     -          HBUF,INDEX)
          END IF
          DO I=1,IDRSBLK               !Get DRS data
            HBUF(INDEX)=NFDATA(I,J)
            INDEX=INDEX+1
          END DO
          FREE_BLKS=FREE_BLKS-1
          IF(INDEX.GT.IDRSBLK*20) INDEX=1
          IF(JBLK.NE.LAST_JBLK+1) THEN
            IF(LAST_JBLK.NE.-1) THEN
              WRITE(CTEXT1,1000,IOSTAT=IOS) LAST_JBLK
              CALL LOG_MESS('End of section.   DRS block'//CTEXT1)
            END IF
            WRITE(CTEXT1,1000,IOSTAT=IOS) JBLK
            CALL LOG_MESS('Start of section. DRS block'//CTEXT1)
          END IF
          LAST_JBLK=JBLK
          LAST_JFNO=JFNO
        ELSE
          CALL LOG_MESS('Buffer overflow')
        END IF
C
C If previous disc write is in operation:
C 1.  Check if finished (ICHECKD=0 until finished)
C 2.  Check for errors
C 3.  If no errors then submit next disc block for writing if there are any
C     waiting.
C
        IF(WRITING) THEN !If previous write in operation
          ISTAT=ICHECKD(%VAL(FOR$RAB(ILUN)))
          IF(ISTAT.NE.0) THEN !If write has completed
            WRITING=.FALSE.
            IF(ISTAT.NE.RMS$_NORMAL) THEN !If errors
              CALL LOG_MESS('Stopping after disc write failure')
              CALL ST_MESS('Stopped after disc write failure')
              CALL FL_MESS
     -            ('H_DISC stopping after disc write failure')
              CALL LIB$STOP(%VAL(ISTAT))
            ELSE                   !If write was good
              CALL STR_TIME(STATUS(19)) !Note time of latest write
              IBLKS=IBLKS+10       !Bump block count
              IF(MOD(IBLKS,600).EQ.10) THEN !Message every 600 blocks written
                WRITE(CTEXT1,1000,IOSTAT=IOS) JBLK
                WRITE(CTEXT2,1000,IOSTAT=IOS) IBLKS
                CALL LOG_MESS('Latest DRS block: '//CTEXT1//
     -          '    Blocks written: '//CTEXT2)
              END IF
              FREE_BLKS=FREE_BLKS+10
              OUTDEX=OUTDEX+IDRSBLK*10
              IF(OUTDEX.GT.IDRSBLK*20) OUTDEX=1
            END IF
          END IF
        END IF
C
C Write next block to disc if disc not busy and a block is ready
C
        IF(FREE_BLKS.LE.10.AND..NOT.WRITING) THEN
          CALL WRITED(%VAL(FOR$RAB(ILUN)),HBUF(OUTDEX),IDRSBLK*10)
          WRITING=.TRUE.
        END IF
      END DO

1000  FORMAT(I6)

      END       
C**************************************************************************
      SUBROUTINE LOG_MESS(MESSAGE)
C
C Writes message to log file consisting of program name, date, time and message.
C
      IMPLICIT      NONE
      CHARACTER*(*) MESSAGE
      CHARACTER CDAT*9,CTIM*10
      CALL DATE1(CDAT)
      CALL TIME(CTIM)
      TYPE *,'H_DISC '//CDAT//' '//CTIM//MESSAGE
      RETURN
      END
C**************************************************************************
      SUBROUTINE    ST_MESS(MESSAGE)
C
C Write message to the logical H_DISC_STAT in the group logical name
C table.  GRPNAM and SYSNAM privileges are required.
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
      ISTAT=SYS$CRELNM(,'LNM$GROUP','H_DISC_STAT',PSL$C_SUPER,TLIST)
      RETURN
      END
C**************************************************************************
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
C**************************************************************************
      SUBROUTINE OPEND(IDRSBLK,ILUN,ISTAT)
C
C Opens the disk file.  Note that the file is opened with
C the USEROPEN keyword in order to use asynchronous block writes to the 
C disk (see FOPEN routine).  The file is opened with a basic initial
C space allocation (on a best try contiguous basis) and is then extended if
C required in fairly large steps.  This saves the need to repeatedly extend 
C the file.  Any usused space is deallocated when the file closes.
C 
C IDRSBLK is required DRS block size in words.
C
      IMPLICIT   NONE
      INTEGER*4  ILUN,ISTAT,IERR,IDRSBLK,FOPEN
      EXTERNAL   FOPEN
      INCLUDE    '($SYSSRVNAM)'
      INCLUDE    '($SSDEF)'

      OPEN(UNIT=ILUN,
     -     FILE='HORACE:DRSDAT.DAT',
     -     STATUS='NEW',
     -     EXTENDSIZE=(60*IDRSBLK*2/512),         !VMS blocks = 60s of data
     -     FORM='UNFORMATTED',
     -     ACCESS='SEQUENTIAL',
     -     ORGANIZATION='SEQUENTIAL',
     -     RECL=IDRSBLK/2,                    !Longwords
     -     RECORDTYPE='FIXED',
     -     IOSTAT=IERR,
     -     USEROPEN=FOPEN)
      ISTAT=IERR
      RETURN  !IERR is 0 for success, otherwise FORTRAN not VMS error codes
      END
C**************************************************************************
      INTEGER*4 FUNCTION FOPEN(FFAB,FRAB,JLUN)
C
C Part of the file opening procedure, this function gives access to VMS
C Record Management Services not available in standard FORTRAN.  This is
C done so that block writes can be used (whereby data is transferred 
C directly from user arrays to disk, without using intermediate FORTRAN 
C buffers.  It also allows the writes to disk to be done asynchronously, so 
C that the FORTRAN program can continue to execute while the write is taking 
C place.  The function returns the result of the create and connect services.
C
      IMPLICIT  NONE
      INTEGER*4 JSTAT,JLUN
      INCLUDE   '($SYSSRVNAM)'
      INCLUDE   '($FABDEF)'              !File access block definitions
      INCLUDE   '($RABDEF)'              !Record access block definitions
      INCLUDE   '($RMSDEF)'              !RMS status definitions
      RECORD    /FABDEF/ FFAB            !Declare the File Access Block
      RECORD    /RABDEF/ FRAB            !Declare the Record Access Block
C
C External data
C
C FOPEN   I*4  write   Returned       Returned status of create or connect
C FFAB    REC  read    Passed         FAB name for FORTRAN
C FRAB    REC  read    Passed         RAB name for FORTRAN
C JLUN    I*4  read    Passed         LUN number from FORTRAN
C
      FFAB.FAB$B_FAC=FFAB.FAB$B_FAC.OR.FAB$M_BIO !Use block I/O
      FFAB.FAB$L_FOP=FFAB.FAB$L_FOP.OR.FAB$M_TEF !Truncate file on close
      JSTAT=JLUN                                 !Stop compiler worrying
      JSTAT=SYS$CREATE(FFAB)                     !Create file
      IF(JSTAT.NE.RMS$_NORMAL) CALL LIB$STOP(%VAL(JSTAT))
      JSTAT=SYS$CONNECT(FRAB)                    !Connect to it
      IF(JSTAT.NE.RMS$_NORMAL) CALL LIB$STOP(%VAL(JSTAT))
      FOPEN=JSTAT  !FORTRAN converts this RMS error code to a FORTRAN code
      RETURN
      END
C**************************************************************************
      SUBROUTINE WRITED(FRAB,IBUF,IWORDS)
C
C Performs asynchronous block write to disk.
C
      IMPLICIT   NONE
      INTEGER*4  IWORDS,JSTAT
      INTEGER*2  IBUF
      INCLUDE    '($SYSSRVNAM)'
      INCLUDE    '($RABDEF)'
      INCLUDE    '($RMSDEF)'
      RECORD     /RABDEF/ FRAB
C
C External data
C
C FRAB    REC  Read    Passed         The name of the record access block
C
      FRAB.RAB$W_RSZ=IWORDS*2                    !Load block size
      FRAB.RAB$L_RBF=%LOC(IBUF)                  !Load block location
      FRAB.RAB$L_BKT=0
      FRAB.RAB$L_ROP=FRAB.RAB$L_ROP.OR.RAB$M_ASY !Set for asynchronous
      JSTAT=SYS$WRITE(FRAB)                      !Write
      IF(JSTAT.NE.RMS$_NORMAL.AND.JSTAT.NE.RMS$_PENDING) 
     -         CALL LIB$STOP(%VAL(JSTAT))
      RETURN
      END
C**************************************************************************
      INTEGER*4 FUNCTION ICHECKD(FRAB)
C
C Reads the record access block status to see whether the asynchronous 
C write has completed.  The status word is returned.  This is zero if
C the write has not completed.
C
      IMPLICIT   NONE
      INCLUDE    '($RABDEF)'
      RECORD     /RABDEF/ FRAB
C
C External data
C
C FRAB    REC  Read    Passed         The name of the record access block
C
      ICHECKD=FRAB.RAB$L_STS
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
C*******************************************************************************
      SUBROUTINE    FL_MESS(MESSAGE)
C
C Sends an immediate message to the flight leader's display.  The message 
C is only sent when any existing output ends, so it should not interfere
C with graphics displays.  OPER privilege is required.
C
      IMPLICIT      NONE
      INTEGER*4     ISTAT
      CHARACTER*(*) MESSAGE
      INCLUDE       '($SYSSRVNAM)'
      INCLUDE       '($BRKDEF)'

      ISTAT=SYS$BRKTHRU(,MESSAGE,'SBD_FL',%VAL(BRK$C_DEVICE),,,,,,,)
      RETURN
      END
C*******************************************************************************
      SUBROUTINE NEW_FILE(ILUN,WRITING,IBLKS,FREE_BLKS,OUTDEX,IDRSBLK,
     -    HBUF,INDEX)
C
C Writes any outstanding output to the current file, closes it, and then opens
C a new output file.
C
      IMPLICIT  NONE
      INTEGER*4 ILUN,IBLKS,FREE_BLKS,OUTDEX,IDRSBLK,INDEX
      INTEGER*4 ISTAT,IOS,IERR,ICHECKD
      LOGICAL*4 WRITING,FILE_OPEN,LTRYING
      INTEGER*2 HBUF(40960)
      INCLUDE   'HCOM_DEF.FOR'
      INCLUDE   '($RMSDEF)'
C
C If previous disc write is in operation:
C 1.  Check if finished (ICHECKD=0 until finished)
C 2.  Check for errors
C 3.  If no errors then submit next disc block for writing if there are any
C     waiting.
C
      IF(WRITING) THEN                 !If previous write in operation
        ISTAT=ICHECKD(%VAL(FOR$RAB(ILUN)))
        DO WHILE(ISTAT.EQ.0)
          CALL LIB$WAIT(0.1)
          ISTAT=ICHECKD(%VAL(FOR$RAB(ILUN)))
        END DO
        IF(ISTAT.NE.RMS$_NORMAL) THEN  !If errors
          CALL LOG_MESS('Stopping after disc write failure')
          CALL ST_MESS('Stopped after disc write failure')
          CALL FL_MESS
     -        ('H_DISC stopping after disc write failure')
          CALL LIB$STOP(%VAL(ISTAT))
        ELSE                           !If write was good
          CALL STR_TIME(STATUS(19))    !Note time of latest write
          FREE_BLKS=FREE_BLKS+10
          OUTDEX=OUTDEX+IDRSBLK*10
          IF(OUTDEX.GT.IDRSBLK*20) OUTDEX=1
        END IF
      END IF
C
C Write any remaining blocks to disc.
C
      IF(FREE_BLKS.LT.20) THEN
        CALL WRITED(%VAL(FOR$RAB(ILUN)),HBUF(OUTDEX),
     -      IDRSBLK*(20-FREE_BLKS))
        ISTAT=ICHECKD(%VAL(FOR$RAB(ILUN)))
        DO WHILE(ISTAT.EQ.0)
          CALL LIB$WAIT(0.1)
          ISTAT=ICHECKD(%VAL(FOR$RAB(ILUN)))
        END DO
        IF(ISTAT.NE.RMS$_NORMAL) THEN  !If errors
          CALL LOG_MESS('Stopping after disc write failure')
          CALL ST_MESS('Stopped after disc write failure')
          CALL FL_MESS
     -        ('H_DISC stopping after disc write failure')
          CALL LIB$STOP(%VAL(ISTAT))
        ELSE                           !If write was good
          CALL STR_TIME(STATUS(19))    !Note time of latest write
        END IF
      END IF
C
C Close current DRSDAT.DAT file.
C
      CLOSE(ILUN,IOSTAT=IOS)
      IF(IOS.NE.0) CALL LOG_MESS('Data file close error')
C
C Open new DRSDAT.DAT giving it the next highest version number.
C
      FILE_OPEN=.FALSE.
      LTRYING=.FALSE.
      DO WHILE(.NOT.FILE_OPEN)
        CALL OPEND(IDRSBLK,ILUN,IERR)
        IF(IERR.EQ.0) THEN
          CALL LOG_MESS('New DRSDAT.DAT opened')
          FILE_OPEN=.TRUE.
        ELSE
          IF(.NOT.LTRYING) THEN
            CALL LOG_MESS('Failed to open new DRSDAT.DAT')
            CALL ST_MESS('Trying to open new DRSDAT.DAT')
            CALL HM_MESS('Unable to open new DRSDAT.DAT on system disk.'
     -         //'  Will keep trying')
            LTRYING=.TRUE.
          END IF
          CALL LIB$WAIT(60.0)
        END IF
      END DO        
C
C Adjust the pointers
C
      INDEX=1
      OUTDEX=1
      FREE_BLKS=20
      WRITING=.FALSE.
      IBLKS=0
C
      RETURN
1000  FORMAT(I6)
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
