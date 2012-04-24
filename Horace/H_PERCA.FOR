      PROGRAM H_PERCA
!
! Currently a demonstration of reading user supplied values into the DERIVE
! array, and writing out DERIVE data at regular intervals, using PERCA_CHAN.
! Will need some tailoring to meet exact user requirements when known.  Note
! that may be necessary to write into adjacent DERIVE array locations, or for
! uses to update data once a second, to prevent gaps, zeros or glitches in the
! DERIVE time series.
!
! Can by used as a model for similar applications, or just the relevant parts
! if only doing input or output.
!
! Note that H_STOP, H_START, H_BUILD, H_SYSTARTUP, H_PERCA.COM also need to be
! created or modified, as well as allocating suitable parameter numbers in
! DERIVE for input data.
!
! Build: FORT H_PERCA
!        LINK H_PERCA,H_COMMON/OPT
!        RUN H_PERCA
!
! V1.00  02/02/02  W.D.N.JACKSON
!
      INTEGER*4 ITIMINT,IFUNCR,IFUNCW,ISTAT,ILEN,IFUNCT,
     &    LIB$GETDVI,LAN,IP1,IP2,IP3,IP4,IOS,STR$ELEMENT
      INTEGER*2 JOSB(4),JOSB1(4),JCHAN
      CHARACTER CNAME*64,CHANNEL*10,CLINE*80,CLINE1*80,CP1*10,CP2*10,
     &    CP3*10,CP4*10
      REAL*4    RLSTIME
      INCLUDE   '($DVIDEF)'
      INCLUDE   '($IODEF)'
      INCLUDE   '($SYSSRVNAM)'
      INCLUDE   'HCOM_DEF.FOR'
!
! Define requirements for this port
!
      DATA      ITIMINT /10/              !Output data rate in secs
!
! Establish connection to channel.
!
      CHANNEL='PERCA_CHAN'
      ISTAT=SYS$ALLOC(CHANNEL,,,,)     !Allocate the serial port
      IF(.NOT.ISTAT) THEN
        TYPE *,' Unable to allocate '//CHANNEL
        CALL EXIT
      END IF
      ISTAT=SYS$ASSIGN(CHANNEL,JCHAN,,) !Now assign a channel to it
      IF(.NOT.ISTAT) THEN
        TYPE *,' Unable to assign channel to '//CHANNEL
        ISTAT=SYS$DALLOC(CHANNEL,)
        CALL EXIT
      END IF
      ISTAT=LIB$GETDVI(DVI$_DEVNAM,JCHAN,,,CNAME,ILEN) !Get device name
      IF(CNAME(2:4).EQ.'LTA') THEN   !If terminal server
        IFUNCT=IO$_TTY_PORT.OR.IO$M_LT_CONNECT !Connect function
        ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFUNCT),JOSB,,,,,,,,) !Make connectn
        IF(.NOT.ISTAT.OR..NOT.JOSB(1)) THEN
          TYPE *,' Unable to connect to '//CHANNEL
          ISTAT=SYS$DASSGN(%VAL(JCHAN))
          ISTAT=SYS$DALLOC(CHANNEL,)
          CALL EXIT
        END IF
      END IF
!
! Now wait in loop until time to send data, or new data are received.
!
      IFUNCR=IO$_READVBLK.OR.IO$M_NOECHO
      IFUNCW=IO$_WRITEVBLK
      CLINE=' '
      RLSTIME=SECNDS(0.0)
      ISTAT=SYS$QIO(,%VAL(JCHAN),%VAL(IFUNCR),JOSB,,,
     &    %REF(CLINE),%VAL(80),,,,)    !Read line from user
      DO WHILE(.TRUE.)
        IF(JOSB(1).NE.0) THEN
          CLINE(JOSB(2)+1:)=' '       !Remove terminator(s)
          CALL STR_COMPRESS(CLINE)    !Remove multiple spaces
          ISTAT=STR$ELEMENT(CP1,0,' ',CLINE) !Assumes spaces delimit values
          ISTAT=STR$ELEMENT(CP2,1,' ',CLINE)
          ISTAT=STR$ELEMENT(CP3,2,' ',CLINE)
          ISTAT=STR$ELEMENT(CP4,3,' ',CLINE)
          READ(CP1,'(BN,I)',IOSTAT=IOS) IP1 !Convert to integers
          READ(CP2,'(BN,I)',IOSTAT=IOS) IP2
          READ(CP3,'(BN,I)',IOSTAT=IOS) IP3
          READ(CP4,'(BN,I)',IOSTAT=IOS) IP4
! Note that can add/subt to the STATUS(2) value if want to put data ahead or
! behind current data, provided whatever value used is between 1 and 12800.
          IF(STATUS(2).GT.0) THEN
            DERIVE(STATUS(2),519-512)=FLOAT(IP1)*10. !Scale values as required
            DERIVE(STATUS(2),520-512)=FLOAT(IP2)*10.
            DERIVE(STATUS(2),529-512)=FLOAT(IP3)*10.
            DERIVE(STATUS(2),563-512)=FLOAT(IP4)*10.
          END IF
!          TYPE *,IP1,IP2,IP3,IP4
          CLINE=' '                    !Clear for next read
          ISTAT=SYS$QIO(,%VAL(JCHAN),%VAL(IFUNCR),JOSB,,,
     &      %REF(CLINE),%VAL(80),,,,)  !Read next line (80 chars or cr or lf)
        END IF
        IF(SECNDS(RLSTIME).GE.FLOAT(ITIMINT)) THEN
          IF(STATUS(2).GT.0) THEN
            IP1=NINT(DERIVE(STATUS(2),519-512)*10) !519 IATDI
            IP2=NINT(DERIVE(STATUS(2),520-512)*10) !520 TATDI
            IP3=NINT(DERIVE(STATUS(2),529-512)*10) !529 RDEW
            IP4=NINT(DERIVE(STATUS(2),563-512)*10) !563 INS G/S
            WRITE(CLINE1,'(4(I5,X),A)',IOSTAT=IOS) 
     &          IP1,IP2,IP3,IP4,CHAR(13)//CHAR(10)
!            TYPE *,CLINE1(1:LAN(CLINE1))
          ISTAT=SYS$QIO(,%VAL(JCHAN),%VAL(IFUNCW),JOSB1,,,
     &        %REF(CLINE1),%VAL(LAN(CLINE1)),,,,) !Send data to user
          END IF
          RLSTIME=SECNDS(0.0)
        END IF
        CALL LIB$WAIT(0.1)
      END DO
!
      END
C*******************************************************************************
      INTEGER*4 FUNCTION LAN(CSTRING)
CDEC$ IDENT 'V1.00'
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
