      PROGRAM H_ISS_GET
C
C H_ISS_GET.FOR
C
C If the DRS sends to HORACE an ISS block which is not currently held on
C disc, this program writes out a new ISS file to disc.  Note that the
C DRS only sends out the ISS blocks when it starts logging data, so that
C HORACE must be up and running before the DRS starts logging if the ISS
C is to be captured.
C
C Build command:
C
C    $ LINK H_ISS_GET,HCOM/OPT
C
C    where HCOM.OPT contains the following line:
C
C     SYS$LIBRARY:HCOM/SHAREABLE
C
C The program operates as follows:
C 
C 1.  It waits for H_DRS_LOG to signal an ISS block.
C
C 2.  It collects the data and checks that it is a valid ISS block.
C
C 3.  If it is a valid ISS block it writes it to disc as a file with a name
C     of ISSxxx.DAT if such a file does not already exist.
C
C 4.  If the file already exists on disc, it checks whether it matches the ISS
C     and writes a new file if it doesn't.
C
C 5.  The program then waits for any more ISS blocks.
C
C
C Note:
C	The ISS block is stored in a disk file with the name ISSxxx.DAT,
C       where xxx is the ISS number, eg ISS008.DAT.
C
C       It is not necessary to copy the ISS blocks from every flight, only
C	when a new ISS is used, or the disk files have been lost.
C
C DRS data stream:
C
C	The first ten blocks sent by the DRS when it starts recording
C	contain the Instrument Sampling Sequence (ISS) in the format shown
C	below.  Note that these blocks are only sent to the first tape of
C	a flight and are not sent to subsequent tapes.
C
C	If n is the maximum DRS parameter number being recorded then the format
C	of the DRS ISS block is as follows:
C
C	Word 1		The ISS number in binary
C	Word 2		The offset in bytes of DRS parameter 1 in the DRS data
C			blocks
C	Word 3		Parameter 2 offset, as for Word 2
C	  .
C	  .
C	Word n+1	Parameter n offset, as for Word 2
C	Word n+2	Offset for parameter n+1, if it were present.  This word
C			is used to indicate the sampling rate of parameter n.
C	Word n+3	Binary 4095, which indicates the end of the ISS data.
C
C	If a parameter is not recorded it has an offset of zero.
C	
C	Note that although a DRS data block contains 2048 words the maximum
C	usage of the DRS ISS blocks is 515 words (the ISS number, 512 
C	parameter offsets, the offset for 'parameter 513', and the ISS data
C	terminator.
C
C ISS file format:
C
C	The ISS file is named ISSxxx.DAT, where xxx is the ISS number.
C	The ISS 
C	contains n+3 useful data words, where n is the highest parameter number
C	recorded (ie the ISS number, 512 parameter offsets, the offset for 
C	'parameter 513', and the ISS data terminator).
C	This data is stored as a single fixed length 768 (ie 3*512 byte) word 
C	record in the file.
C	The data is checked for consistency as far as possible so that
C	it should not have any obvious errors.
C
C	If n is the maximum DRS parameter number being recorded then the format
C	of the ISSxxx.DAT file is as follows (all words are 16-bit):
C
C	Word 1		The ISS number in binary
C	Word 2		The offset in bytes of DRS parameter 1 in the DRS data
C			blocks.
C	Word 3		Parameter 2 offset, as for Word 2
C	  .
C	  .
C	Word n+1	Parameter n offset, as for Word 2
C	Word n+2	Offset for parameter n+1, if it were present.  This word
C			is used to indicate the sampling rate of parameter n.
C	Word n+3	Binary 4095, which indicates the end of the ISS data.
C
C	If a parameter is not recorded it has an offset of zero.
C
C V1.00            W.D.N.JACKSON
C V1.01  03/08/92  W.D.N.JACKSON
C        Now checks whether ISS matches any existing file, and writes a new
C        file if it differs.
C V2.00  02/02/02  W.D.N.JACKSON
C        ISS filename changed to ISSxxx.DAT.
C
      IMPLICIT  NONE
      INTEGER*4 ISTAT,J,ISSVAL,NLAST,N,ISS,IOS,I,JOS,JEOD,K
      LOGICAL*4 LFILCHKD,LSAME,ISS_EXISTS
      INTEGER*2 ISSBLK(768),ISSFIL(768)
      CHARACTER CISSFNAME*10
      INCLUDE   'HCOM_DEF.FOR'
      INCLUDE   '($SYSSRVNAM)'
      INCLUDE   '($SSDEF)'
C
C Start by associating a name with a common event flag cluster.  This
C cluster is common to all processes with the same UIC group number.
C
C The flags are used as follows:
C
C  Flag  66        Set by H_DRS_SIM or H_DRS_LOG when a new DRS data block 
C                  is in global common, with a block number between 1 and 
C                  10. Used and cleared by H_ISS_GET.
C
      LFILCHKD=.FALSE.                 !Not checked ISS file yet
      CALL LOG_MESS('H_ISS_GET starting')
      ISTAT=SYS$ASCEFC(%VAL(64),'H_CEF_CLUSTER',,)
      IF(ISTAT.NE.SS$_NORMAL) CALL LIB$SIGNAL(%VAL(ISTAT))
      DO WHILE (.TRUE.)                !Loop indefinitely
        ISTAT=SYS$CLREF(%VAL(66))
        CALL LOG_MESS('Waiting for ISS block')
        CALL ST_MESS('Waiting for ISS block')
        ISTAT=SYS$WAITFR(%VAL(66))
        IF(ISTAT.NE.SS$_NORMAL) CALL LIB$SIGNAL(%VAL(ISTAT))
        DO J=1,768
          ISSBLK(J)=NFDATA(J,STATUS(1))
        END DO
        CALL LOG_MESS('Validating ISS block')
C
C Validate ISS block as far as possible
C
        ISSVAL=1				!0 for valid, 1 for invalid
        J=1
        DO WHILE(ISSVAL.EQ.1.AND.J.LE.515)	!515 = 512 + 3
          IF(ISSBLK(J).EQ.4095) ISSVAL=0	!Look for end of ISS marker
          J=J+1
        END DO
        JEOD=J-1				!Posn of end of ISS marker
        IF(ISSVAL.EQ.0) THEN
          IF(ISSBLK(2).NE.16) ISSVAL=1		!First offset must be 16
          NLAST=14
          DO K=2,JEOD-1
            N=ISSBLK(K)
            IF(N.LT.0.OR.N.GT.3968) ISSVAL=1	!3968 = 4096 * 31/32
            IF(MOD(N,2).NE.0) ISSVAL=1		!Offsets in bytes, must be even
            IF(N.NE.0) THEN
              IF(N.LE.NLAST) ISSVAL=1		!Offsets must increase
              NLAST=N
            END IF
          END DO
        END IF
C
C Validation is complete. ISSVAL is 1 if invalid, 0 if valid.
C
        IF(ISSVAL.EQ.1) THEN           !If block was not valid
          CALL LOG_MESS('ISS block is invalid')
        ELSE                           !Seems like a real ISS block
          ISS=(ISSBLK(1).AND.'0FFF'X)  !Extract ISS number
          CISSFNAME='ISSxxx.DAT'       !Generate ISS file name
          WRITE(CISSFNAME(4:6),1000) ISS !Insert the ISS number
          INQUIRE(FILE=CISSFNAME,EXIST=ISS_EXISTS) !See if already exists
          IF(ISS_EXISTS) THEN          !If it already exists
            IF(.NOT.LFILCHKD) THEN     !If haven't yet checked disk file
              OPEN(UNIT=10,FILE=CISSFNAME,STATUS='OLD',ACCESS='DIRECT',
     -            RECL=384,IOSTAT=IOS) !Open file
              READ(10,REC=1,IOSTAT=IOS) ISSFIL !Read file contents
              CLOSE(UNIT=10,IOSTAT=JOS)
              LSAME=.TRUE.             !Will stay true if contents the same
              DO I=1,384               !Check contents against actual ISS
                IF(ISSBLK(I).NE.ISSFIL(I)) LSAME=.FALSE.
              END DO
              IF(.NOT.LSAME.OR.IOS.NE.0) THEN !If couldn't read file or differe
                CALL LOG_MESS('ISS file '//CISSFNAME//' differs'//
     -              ' from actual ISS')
                CALL HM_MESS('ISS contents differ from disk file.  '//
     -              'A new ISS file will be written')
                ISS_EXISTS=.FALSE.     !Force writing of a new file
              ELSE                     !If the same
                CALL LOG_MESS('ISS file '//CISSFNAME//' is already'//
     -              ' on disk')
              END IF
              LFILCHKD=.TRUE.          !Show have now checked the disk file
            END IF
          END IF
          IF(.NOT.ISS_EXISTS) THEN     !If need to write a new ISS file
            OPEN(UNIT=10,FILE=CISSFNAME,STATUS='NEW',ACCESS='DIRECT',
     -          RECL=384,IOSTAT=IOS)   !Open new file
            WRITE(10,REC=1,IOSTAT=IOS) ISSBLK !Write the ISS block
            CLOSE(UNIT=10,IOSTAT=JOS)  !Close the file
            IF(IOS.EQ.0) THEN          !If no open or write errors
              CALL LOG_MESS('ISS file '//CISSFNAME//' written to disk.')
            ELSE                       !If open or write errrors
              CALL LOG_MESS('Failed to write '//CISSFNAME//' to disk.')
              CALL HM_MESS('Unable to write new ISS file to disk')
            END IF
          END IF
        END IF
      END DO                           !Wait for some more ISS blocks
1000  FORMAT(I3.3)
      END
C*******************************************************************************
      SUBROUTINE LOG_MESS(MESSAGE)
C
C Writes message to log file consisting of program name, date, time and message.
C
      IMPLICIT NONE
      CHARACTER*(*) MESSAGE
      CHARACTER CDAT*9,CTIM*10
      CALL DATE1(CDAT)
      CALL TIME(CTIM)
      TYPE *,'H_ISS_GET '//CDAT//' '//CTIM//MESSAGE
      RETURN
      END
C*******************************************************************************
      SUBROUTINE    ST_MESS(MESSAGE)
C
C Write message to the logical H_DRS_SIM_STAT in the group logical name
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
      ISTAT=SYS$CRELNM(,'LNM$GROUP','H_ISS_GET_STAT',PSL$C_SUPER,TLIST)
      RETURN
      END
C*******************************************************************************
      SUBROUTINE HM_MESS(CMESS)
C
C Writes an immediate message to the Flight Leader's display, and also stores
C it in the shared file HORACE:H_MESSAGES.LOG for recall by the messages option
C of the INSTRUMENT menu.  These messages should only be sent when there is a
C problem or potential problem which must be drawn to the Flight Leader's
C attention.  OPER privilege is required.  Note that this routine prefaces
C the message with the time that it was received by the H_MESSAGE process, so
C there is not need for users to time stamp their messages.
C
      IMPLICIT      NONE
      INTEGER*4     IFUNCT,ISTAT,MLEN
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
