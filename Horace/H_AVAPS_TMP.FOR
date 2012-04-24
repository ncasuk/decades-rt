      PROGRAM H_AVAPS_TMP
!
! Handles the processing of TEMP message files from the AVAPS PC for
! onward transmission via the Satcom system.
!
! This program:
! 
! 1. Is run each time a new TEMP file is detected.  It then copies
!    it into the [.SATCOM] directory and queues it for transmission as 
!    NAVAID files (which can be compacted).  Note that each file will only
!    contain 1 temp message, and no quality control or checking is done on
!    their contents, they simply have the SDB destination header added.
!
! The file to be processed must be define by the logical name H_AVAPS_TMP_FILE
!
! Build with:
! 
!  $ FORT H_AVAPS_TMP
!  $ LINK H_AVAPS_TMP,H_COMMON/OPT
!
!    where H_COMMON.OPT contains the line SYS$LIBRARY:H_COMMON/SHAREABLE
!
! V1.00  24/05/99  W.D.N.JACKSON
! V1.01  30/06/99  W.D.N.JACKSON
!        Now converts NOAA format messages to MRF/Met Office format
! V1.02  26/08/99  W.D.N.JACKSON
!        Now handles NOAH AVAPS editor V2.0.8 by removing =
! V1.03  26/07/00  W.D.N.JACKSON
!        Now keeps the ='s and makes no changes to the message
!
      IMPLICIT  NONE
      CHARACTER CLINE*80,CSLINE*80,CFILE*80,CNXTMES*5
      INTEGER*4 IOS,INXTREC,ISIZ,IL,LAN,IOS1,IOS2,I
      INCLUDE   'HCOM_DEF.FOR'

      CALL GETFILE(CFILE)
      I=1
      DO WHILE(CFILE(I:I).NE.']')
        I=I+1
      END DO
      I=I+1
      OPEN(UNIT=13,FILE='H_AVAPS_TMP_FILE',STATUS='OLD',READONLY,
     &    SHARED,IOSTAT=IOS1)
      IF(IOS1.NE.0) THEN
        CALL LOG_MESS('TEMP message open failure '//CFILE(I:LAN(CFILE)))
        TYPE *,IOS1
        CLOSE(13,IOSTAT=IOS)
        CALL EXIT
      END IF
      CALL LOG_MESS('Processing TEMP message '//CFILE(I:LAN(CFILE)))
      OPEN(UNIT=11,FILE='[.SATCOM]SAT_OUT_STS.DAT',STATUS='UNKNOWN',
     &    SHARED,ACCESS='DIRECT',RECL=20,IOSTAT=IOS) 
      CALL GET_NXT_OUT(CNXTMES,INXTREC)
      OPEN(UNIT=12,FILE='[.SATCOM]SAT_OUT_'//CNXTMES//'.DAT',
     &    STATUS='NEW',IOSTAT=IOS,CARRIAGECONTROL='LIST')
      WRITE(12,'(A)',IOSTAT=IOS) 'S' !Message for synoptic data bank
      ISIZ=0
      READ(13,'(A)',IOSTAT=IOS2) CLINE
      DO WHILE(IOS2.EQ.0)
!        IF(CLINE(1:2).EQ.'UZ') CLINE(1:11)='UZUK80 EGRR'
!        IF(CLINE(1:2).EQ.'UZ') CLINE(20:22)=' '
!        IF(CLINE(1:5).EQ.'61616') CLINE(7:11)='XV208'
!        IF(CLINE(1:5).EQ.'61616') CLINE(38:41)='EGRR'
!        IF(CLINE(1:5).EQ.'62626') THEN
!          IL=LAN(CLINE)
!          IF(CLINE(IL:IL).EQ.'=') CLINE(IL:IL)=' '
!        END IF
        IL=LAN(CLINE)
        WRITE(12,'(A)',IOSTAT=IOS) CLINE(1:IL)
        TYPE *,CLINE(1:IL)
        ISIZ=ISIZ+IL
        READ(13,'(A)',IOSTAT=IOS2) CLINE
      END DO
      CLOSE(12,IOSTAT=IOS)
      CLOSE(13,IOSTAT=IOS)
!
! Form status line and write it out to SAT_OUT_STS.DAT.
!
      CSLINE=CNXTMES
      CALL DATE1(CSLINE(8:16))
      IF(CSLINE(8:8).EQ.' ') CSLINE(8:8)='0'
      CALL TIME(CSLINE(19:26))
      WRITE(CSLINE(29:32),'(I4)',IOSTAT=IOS) ISIZ
      CSLINE(35:63)='Synoptic data bank'
      CSLINE(66:77)='Queued'
      CSLINE(79:80)='NV'
      WRITE(11,REC=INXTREC,IOSTAT=IOS) CSLINE
      CLOSE(11)
      STATUS(47)=IBSET(STATUS(47),8) !Tell H_SATCOM
      STATUS(47)=STATUS(47).XOR.'400'X !Negate bit 10 for H_DISPLAY
      CALL EXIT
      END
!*******************************************************************************
      SUBROUTINE LOG_MESS(MESSAGE)
!
! Writes message to log file consisting of program name, date, time and message.
!
      IMPLICIT  NONE
      CHARACTER MESSAGE*(*),CDAT*9,CTIM*10
      CALL DATE1(CDAT)
      CALL TIME(CTIM)
      TYPE *,'H_AVAPS_TMP '//CDAT//' '//CTIM//MESSAGE
      RETURN
      END
!*******************************************************************************
      SUBROUTINE GET_NXT_OUT(CNXTMES,INXTREC)
!
! Looks in the SAT_OUT_STS.DAT file and works out the version number of the
! next output message (CNXTMES) and its position in the file (INXTREC).  Also
! adjusts the first line by adding 1 to the message count, and setting the
! first queued message number to the new message if it was zero.
!
      IMPLICIT  NONE
      CHARACTER CNXTMES*5,CLINE*80,CLINE1*80
      INTEGER*4 IOS,ICOUNT,IFSTQD,IFSTMES,INXTREC

      READ(11,REC=1,IOSTAT=IOS) CLINE
      IF(IOS.NE.0) CLINE='00000 00000'
      READ(CLINE(1:5),'(I5)',IOSTAT=IOS) ICOUNT
      IF(ICOUNT.GT.0) READ(11,REC=2,IOSTAT=IOS) CLINE1
      WRITE(CLINE(1:5),'(I5.5)',IOSTAT=IOS) ICOUNT+1
      READ(CLINE(7:11),'(I5)',IOSTAT=IOS) IFSTQD
      IF(IFSTQD.EQ.0) CLINE(7:11)=CLINE(1:5)
      WRITE(11,REC=1,IOSTAT=IOS) CLINE
      UNLOCK(11,IOSTAT=IOS)
      IF(ICOUNT.GT.0) READ(CLINE1(1:5),'(I5)',IOSTAT=IOS) IFSTMES
      IF(ICOUNT.EQ.0) IFSTMES=1
      WRITE(CNXTMES,'(I5.5)',IOSTAT=IOS) IFSTMES+ICOUNT
      INXTREC=ICOUNT+2
      RETURN
      END
!*******************************************************************************
!
! ROUTINE          LAN FUNCTION FORTVAX
!
! PURPOSE          Returns length of a string less trailing blanks
!
! DESCRIPTION      Function returns the length of a string not including
!                  trailing blanks. If the string is all blanks, or is of zero
!                  length, the function returns a value of 1.
!
! VERSION          1.00  9-1-90  W.D.N.JACKSON
!
! ARGUMENTS        CSTRING  C**  IN   String whose length to be found
!                  LAN      I*4  OUT  String length less trailing spaces
!
!*******************************************************************************
      INTEGER*4 FUNCTION LAN(CSTRING)
      IMPLICIT  NONE
      CHARACTER CSTRING*(*)
      INTEGER*4 I

      I=LEN(CSTRING)                   !Get string length
      DO WHILE(I.GT.0.AND.CSTRING(I:I).EQ.' ') !Step back over spaces
        I=I-1
      END DO
      IF(I.EQ.0) I=1                   !Special case
      LAN=I
      RETURN
      END
!***************************************************************************
      SUBROUTINE GETFILE(CFILE)
!
! Translates the logical name H_AVAPS_TMP_FILE. Returns a blank string if the 
! logical name cannot be translated.  Note that the logical name must be 
! defined in the PROCESS table.
!
! V1.00  24/05/99  W.D.N.JACKSON
!
      IMPLICIT     NONE
      CHARACTER*80 CFILE
      INTEGER*4    ILIST(4),NAMLEN,ISTAT
      INTEGER*2    JLIST(8)
      EQUIVALENCE  (ILIST,JLIST)
      INCLUDE '($SYSSRVNAM)'
      INCLUDE '($LNMDEF)'
!
      CFILE=' '                        !Will contain the translation 
      JLIST(1)=80                      !Length of string space
      JLIST(2)=LNM$_STRING             !Required function
      ILIST(2)=%LOC(CFILE)             !String location
      ILIST(3)=%LOC(NAMLEN)            !Will have string length
      ILIST(4)=0                       !End of list
      ISTAT=SYS$TRNLNM(,'LNM$PROCESS','H_AVAPS_TMP_FILE',,ILIST)
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
