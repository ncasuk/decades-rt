      PROGRAM H_CHECK_NDU
C
C Reads the H_NDU.DAT file, checks that all lats and longs are correctly 
C specified, and sets Common Event Flag 82 to tell H_NDU that the file has been
C changed.  Note that even if not all the lats and longs can be read, H_NDU
C is still told of, and uses, the new file.  This program is normally run as
C part of the Flight Leader's menu option 12, but can be run by hand if
C H_NDU.DAT is edited separately and the new values are to be used by H_NDU 
C without restarting the process.
C
C V1.00  14/10/94  W.D.N.JACKSON
C V1.01  22/02/96  W.D.N.JACKSON
C        Now allows 50 fixed points.
C
      IMPLICIT  NONE
      INTEGER*4 IOS,ICNT,IFXNO,LAN,ISTAT
      LOGICAL*4 LERR
      REAL*4    RLATD,RLATM,RLNGD,RLNGM
      INCLUDE   '($SYSSRVNAM)'
      CHARACTER CLINE*80,CWORDS(12)*15

      OPEN(UNIT=11,FILE='H_NDU.DAT',STATUS='OLD',READONLY,IOSTAT=IOS)
      IF(IOS.NE.0) THEN
        TYPE *,'Unable to open H_NDU.DAT'
        CALL EXIT
      END IF
      READ(11,'(A)',IOSTAT=IOS) CLINE  !Read first line
      DO WHILE(IOS.EQ.0)               !So long as no read errors
        CALL C_GETWORDS(CLINE,CWORDS,ICNT) !Split line into words
        IF(ICNT.GT.0) THEN             !If not a comment line
          LERR=.FALSE.
          IF(ICNT.NE.7) LERR=.TRUE.    !Must be seven words
          READ(CWORDS(1),*,IOSTAT=IOS) IFXNO !Check position nos 0 to 49
          IF(IOS.NE.0.OR.IFXNO.LT.0.OR.IFXNO.GT.49) LERR=.TRUE.
          READ(CWORDS(2),*,IOSTAT=IOS) RLATD !Check lat degrees
          IF(IOS.NE.0.OR.RLATD.LT.0.OR.RLATD.GT.90.) LERR=.TRUE.
          READ(CWORDS(3),*,IOSTAT=IOS) RLATM !Check lat minutes
          IF(IOS.NE.0.OR.RLATM.LT.0.OR.RLATM.GE.60.) LERR=.TRUE.
          CALL STR$UPCASE(CWORDS(4),CWORDS(4)) !Check N or S
          IF(CWORDS(4).NE.'N'.AND.CWORDS(4).NE.'S') LERR=.TRUE.
          READ(CWORDS(5),*,IOSTAT=IOS) RLNGD !Check long degrees
          IF(IOS.NE.0.OR.RLNGD.LT.0.OR.RLNGD.GT.180.) LERR=.TRUE.
          READ(CWORDS(6),*,IOSTAT=IOS) RLNGM !Check long minutes
          IF(IOS.NE.0.OR.RLNGM.LT.0.OR.RLNGM.GE.60.) LERR=.TRUE.
          CALL STR$UPCASE(CWORDS(7),CWORDS(7)) !Check E or W
          IF(CWORDS(7).NE.'E'.AND.CWORDS(7).NE.'W') LERR=.TRUE.
          IF(LERR.AND.ICNT.GT.0) THEN !If invalid give message and actual line
            TYPE *,
     &'Invalid lat/long specification.  Use ''n dd mm.t h ddd mm.t h''.'
            TYPE *,CLINE(1:LAN(CLINE))
          END IF
        END IF
        READ(11,'(A)',IOSTAT=IOS) CLINE !Read next line
      END DO
      CLOSE(11,IOSTAT=IOS)             !Close file
      ISTAT=SYS$ASCEFC(%VAL(82),'H_CEF_CLUSTER',,) !Associate common EF cluster
      ISTAT=SYS$SETEF(%VAL(82))        !Set event flag 82 to show new file
      CALL EXIT
      END
C*******************************************************************************
C
C ROUTINE          C_GETWORDS SUBROUTINE FORTVAX
C
C PURPOSE          Analyses a line for up to 12 words separated by spaces
C
C DESCRIPTION      Takes a character string representing a line and analyses
C                  it for identifiable words.  Words are defined as a group
C                  of characters between separators.  Separators are spaces
C                  and the start and end of lines.  End of line is either
C                  the end of the string or an ! symbol.  Thus the ! acts
C                  as a comment introducer and all characters on a line
C                  beyond the ! are ignored.  The maximum word length is 15
C                  characters; longer words will be trucated to 15 characters.
C
C VERSION          1.00  10-1-90  N.JACKSON
C
C ARGUMENTS        CLINE      C**  IN  The line to be analysed
C                  CWORDS(12) C*15 OUT Array in which any found words are returned
C                  IWDCNT     I*4  OUT The number of words found
C
C CHANGES          
C
********************************************************************************
      SUBROUTINE C_GETWORDS(CLINE,CWORDS,IWDCNT)
CDEC$ IDENT 'V1.00'
      IMPLICIT      NONE
      CHARACTER*(*) CLINE         !The line to be analysed
      CHARACTER*15  CWORDS(12)    !Array in which any found words are returned
      INTEGER*4     IWDCNT        !The number of words found
      INTEGER*4     I,J,LEN,IWE,IWS
C
C The routine works done the line looking for separators and words
C
      DO I=1,12                        !Clear output array
        CWORDS(I)=' '
      END DO
      IWDCNT=0                         !No words yet
      J=LEN(CLINE)                     !String length
      IF(J.EQ.0) RETURN                !Nothing to do if zero length
      I=1                              !Start position on line
      DO WHILE(IWDCNT.LT.12)           !Look for up to 12 words
        DO WHILE(CLINE(I:I).EQ.' ')    !Step over spaces
          IF(I.EQ.J) RETURN            !Return if end of line
          I=I+1                        !Next character
        END DO
        IF(CLINE(I:I).EQ.'!') RETURN   !Return if found comment
        IWS=I                          !Else note start of word
        DO WHILE(CLINE(I:I).NE.' '.AND.CLINE(I:I).NE.'!') !Look for end or comm
          IF(I.EQ.J) THEN              !If end of line
            IWE=I                      !Note end of word
            IF((IWE-IWS+1).GT.15) IWE=IWS+15-1 !Truncate word length to 15
            IWDCNT=IWDCNT+1            !Bump word count
            CWORDS(IWDCNT)=CLINE(IWS:IWE) !Store word
            RETURN                     !Return because was end of line
          END IF
          I=I+1                        !Try next character
        END DO
        IWE=I-1                        !Note end of word
        IF((IWE-IWS+1).GT.15) IWE=IWS+15-1 !Truncate word length to 15
        IWDCNT=IWDCNT+1                !Bump word count
        CWORDS(IWDCNT)=CLINE(IWS:IWE)  !Store word
      END DO
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
