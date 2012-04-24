      PROGRAM H_CHECK_LAT
C
C Given that the logical name LAT_DEVICE is defined as LTA50x:, then tries
C to establish a connection to the terminal server port.  If the server is
C not available, returns after a few seconds with the symbol $SEVERITY set to
C a number greater than 1, usually 4.  If the device is available returns
C almost immediately with $SEVERITY set to 1.  Note that this program does an
C implicit disconnection and deallocation of the port when it exits.
C
      IMPLICIT NONE
      INTEGER*4 IPL,IHL,LAN,ISTAT,IFUNCT
      INCLUDE '($SYSSRVNAM)'
      INCLUDE '($IODEF)'
      INCLUDE '($SSDEF)'
      INCLUDE '($JPIDEF)'
      INTEGER*2 ICHAN,IOSB(4)
      CHARACTER CPORT*7,CPNAME*15,CDATE*9,CTIME*8,CHEAD*50

      CALL LIB$GETJPI(JPI$_PRCNAM,,,,CPNAME,IPL)
      CALL TIME(CTIME)
      CALL DATE1(CDATE)
      CALL GETDEV(CPORT)               !Translate logical name for port
      CHEAD=CPNAME(1:IPL)//' '//CDATE//' '//CTIME//'  '//CPORT
      IHL=LAN(CHEAD)
      ISTAT=SYS$ALLOC(CPORT,,,,)       !Allocate the serial port
      IF(.NOT.ISTAT) THEN
        TYPE *,CHEAD(1:IHL)//' cannot be allocated'
        CALL EXIT(ISTAT)
      END IF
      ISTAT=SYS$ASSIGN(CPORT,ICHAN,,)  !Now assign a channel to it
      IF(.NOT.ISTAT) THEN
        TYPE *,CHEAD(1:IHL)//' cannot be assigned'
        CALL EXIT(ISTAT)
      END IF
      IFUNCT=IO$_TTY_PORT.OR.IO$M_LT_CONNECT !LAT Connect function
      ISTAT=SYS$QIOW(,%VAL(ICHAN),%VAL(IFUNCT),IOSB,,,,,,,,) !Make connection
      IF(.NOT.ISTAT) THEN
        IF(ISTAT.EQ.SS$_ILLIOFUNC) THEN
          TYPE *,CHEAD(1:IHL)//' is not an applications port'
        ELSE IF(ISTAT.EQ.SS$_DEVACTIVE) THEN
          TYPE *,CHEAD(1:IHL)//' is already in use'
        ELSE
          TYPE *,CHEAD(1:IHL)//' unknown connect error'
        END IF
        CALL EXIT(ISTAT)
      ELSE
        IF(IOSB(1).EQ.SS$_NORMAL) THEN
          TYPE *,CHEAD(1:IHL)//' successful connection'
          CALL EXIT(ISTAT)
        ELSE IF(IOSB(1).EQ.SS$_TIMEOUT) THEN
          TYPE *,CHEAD(1:IHL)//' connection attempt timeout'
          CALL EXIT(IOSB(1))
        ELSE IF(IOSB(1).EQ.SS$_ABORT) THEN
          IF(IOSB(2).EQ.SS$_BADPARAM) THEN
            TYPE *,CHEAD(1:IHL)//' connection failure - bad parameter'
            CALL EXIT(IOSB(2))
          ELSE IF(IOSB(2).EQ.SS$_ACCVIO) THEN
            TYPE *,CHEAD(1:IHL)
     -          //' connection failure - access violation'
            CALL EXIT(IOSB(2))
          ELSE IF(IOSB(2).EQ.SS$_NOPRIV) THEN
            TYPE *,CHEAD(1:IHL)//' connection failure - no privilege'
            CALL EXIT(IOSB(2))
          END IF
        END IF
      END IF
      CALL EXIT(1)
      END
C*******************************************************************************
      SUBROUTINE GETDEV(CDSPDEV)
C
C Translates the logical name LAT_DEVICE which defines the terminal server
C port which is to be tested. Returns a blank string if the logical
C name cannot be translated.  Note that the logical name must be defined in
C the PROCESS table.
C
C V1.00  04/02/94  W.D.N.JACKSON
C
      IMPLICIT     NONE
      CHARACTER*7  CDSPDEV
      INTEGER*4    ILIST(4),NAMLEN,ISTAT
      INTEGER*2    JLIST(8)
      EQUIVALENCE  (ILIST,JLIST)
      INCLUDE '($SYSSRVNAM)'
      INCLUDE '($LNMDEF)'
C
      CDSPDEV=' '                      !Will contain the translation 
      JLIST(1)=7                       !Length of string space
      JLIST(2)=LNM$_STRING             !Required function
      ILIST(2)=%LOC(CDSPDEV)           !String location
      ILIST(3)=%LOC(NAMLEN)            !Will have string length
      ILIST(4)=0                       !End of list
      ISTAT=SYS$TRNLNM(,'LNM$PROCESS','LAT_DEVICE',,ILIST)
      RETURN
      END
C*******************************************************************************
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
