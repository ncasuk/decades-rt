C
C HOR_UTIL.FOR - Utility routines for use by the H_DISPLAY program.
C
C This file contains the following routines:
C
C   SELECT
C   LOGMESS
C   GET_LOGICAL
C   VT_OUT
C   VT_PAGE
C   INPUT_ABORT
C   INPUT_AST
C   INPUT_AST1
C   DATE1
C   CHECK_KEY
C   WAIT_KD
C   UPDT_STATUS
C
C*******************************************************************************
      SUBROUTINE SELECT(IX,IY,IS)
C
C Gets a user response to a menu or parameter number request on the display.
C The letters A to H are passed back immediately as IS values in the
C range -1 to -8.  IX and IY specify the X and Y coordinates on the display
C where the number is to be displayed by calls to ALP.
C
C Otherwise a number of up to 4 numerals,or a minus and 3 numerals terminated 
C by a carriage return is required.  Numbers can be edited with the delete 
C key before CR is pressed.  Note that negative numbers are returned in IS with
C -1000 added to them in order to distinguish them from the immediate keys,
C which have values between -1 and -8.
C A value of zero is returned if return is pressed with no entry.
C
C V1.00  01/01/89  W.D.N.JACKSON
C V1.01  14/12/95  W.D.N.JACKSON
C        Now resets video switch if CTRL/W entered.
C V2.00  13/09/99  W.D.N.JACKSON
C        Now uses asynchronous input via INPUT
C
      IMPLICIT  NONE
      INTEGER*4 ICNT,IEND,I,IS,IX,IY,IOS
      CHARACTER LINE*4
      INCLUDE   '($SYSSRVNAM)'
      INCLUDE   '($IODEF)'
      INCLUDE   'DCOM_DEF.FOR'

      ICNT=0                           !Number of character input so far
      IEND=1                           !Will be 0 when have full input
      LINE='    '                      !Start with blank line
      DO WHILE(IEND.NE.0)              !Loop till have valid entry
        INPUT=0
        DO WHILE(INPUT.EQ.0)
          CALL LIB$WAIT(0.2)
        END DO
        I=INPUT                        !Get ascii value     
        IF(I.GE.65.AND.I.LE.72) THEN   !A to H require immediate action
          IF(CHAR(I).EQ.'F') THEN      !Print requests are
            CALL VIDPNT                ! handled straight away without return
          ELSE                         !Valid immediate key
            IS=-(I-64)
            IEND=0                     !Ready to return
          END IF
        ELSE IF(I.EQ.127) THEN         !DELETE key
          IF(ICNT.GT.0) THEN           !If some chars on line
            LINE(ICNT:ICNT)=' '        !Over print with space
            CALL ALP(IX,IY,LINE)
            ICNT=ICNT-1                !Decrement character count
          END IF
        ELSE IF((I.GE.48.AND.I.LE.57).OR.I.EQ.45) THEN !Valid numeral or -
          IF(ICNT.LT.4) THEN           !Can only handle 4 characters on input
            LINE(ICNT+1:ICNT+1)=CHAR(INPUT) !Write out new digit
            CALL ALP(IX,IY,LINE)
            ICNT=ICNT+1                !Bump character count
          END IF
        ELSE IF(I.EQ.13.AND.ICNT.NE.0) THEN !Carriage return
          READ(LINE,10,IOSTAT=IOS) IS  !Decode string into number
10        FORMAT(I<ICNT>)
          IF(IS.LE.0) IS=IS-1000       !Negative numbers have -1000 added
          IF(IOS.EQ.0) THEN
            IEND=0
          ELSE                         !Invalid string for some reason
            ICNT=0                     !So clear line and start afresh
            LINE=' '
            CALL ALP(IX,IY,LINE)
          END IF
        ELSE IF(I.EQ.13.AND.ICNT.EQ.0) THEN !Carriage return
          IS=0                         !Null entry
          IEND=0                       !Ready to return
        ELSE IF(I.EQ.23) THEN          !CTRL/W
          CALL VIDRST                  !Reset video switch
        END IF
      END DO
      RETURN     
      END
C*******************************************************************************
      SUBROUTINE LOGMESS(CTEXT)
C
C Writes a message to the log file containing the process name, date, time,
C and message text.
C
C V1.00  01/01/89  W.D.N.JACKSON
C
      IMPLICIT  NONE
      CHARACTER CTEXT*(*)
      CHARACTER CDAT*9,CTIM*10
      INCLUDE   'DCOM_DEF.FOR'

      CALL DATE1(CDAT)
      CALL TIME(CTIM)
      TYPE *,'H_DISPLAY_'//DSPDEV(5:6)//' '//CDAT//' '//CTIM//CTEXT
      RETURN
      END
C*******************************************************************************
      SUBROUTINE GET_LOGICAL(CLOGICAL,CRESULT)
C
C Translate the logical name specified by CLOGICAL and returns the result in
C CRESULT.  Returns with blank if not found.  Searches all logical name tables
C and returns the first (lowest) definition found.
C
      IMPLICIT    NONE
      CHARACTER   CLOGICAL*(*),CRESULT*(*)
      INTEGER*4   ILIST(4),NAMLEN
      INTEGER*2   JLIST(8)
      EQUIVALENCE (ILIST,JLIST)
      INCLUDE     '($LNMDEF)'
C
      CRESULT=' '                      !Will contain the translation 
      JLIST(1)=LEN(CRESULT)            !Length of string space
      JLIST(2)=LNM$_STRING             !Required function
      ILIST(2)=%LOC(CRESULT)           !String location
      ILIST(3)=%LOC(NAMLEN)            !Will have string length
      ILIST(4)=0                       !End of list
      CALL SYS$TRNLNM(,'LNM$DCL_LOGICAL',CLOGICAL,,ILIST)
      RETURN
      END
********************************************************************************
      SUBROUTINE VT_OUT(CSTRING)
C
C Provides a common handling point for all output directed to VT type terminal
C windows.  The first character of each CSTRING must be a FORTRAN carriage
C control character.  Strings for serial graphics cards or for X term windows
C are passed on directly.  Strings for DMA or Ethernet graphics cards are
C send using the TXT command, first replacing any null carriage control 
C characters with ampersands.  Whatever the output device, the string is
C also used to maintain an up to date copy of the VT display in memory.
C
      IMPLICIT  NONE
      CHARACTER CSTRING*(*),CTEMP*132
      INTEGER*4 IOS,ITYPE,I,STR$POSITION,IL
      INCLUDE   'DCOM_DEF.FOR'
C
      IF(CSTRING.EQ.'INITIALISE') THEN
        OPEN(UNIT=10,FILE='TT:',STATUS='NEW')
        ITYPE=4                        !PV-Wave X term
        CALL VT_PAGE('INITIALISE')
        RETURN
      END IF
C
      CTEMP=' '
      I=STR$POSITION(CSTRING,'G-VIDEO')
      IF(I.NE.0) THEN
        CTEMP=CSTRING
        CTEMP(I:I+6)='G-     '         !No video switching on X terms
        IL=LEN(CSTRING)
      END IF
      IF(CTEMP.EQ.' ') WRITE(10,'(A)',IOSTAT=IOS) CSTRING
      IF(CTEMP.NE.' ') WRITE(10,'(A)',IOSTAT=IOS) CTEMP(1:IL)
      CALL VT_PAGE(CSTRING)
      RETURN
      END
C*******************************************************************************
      SUBROUTINE VT_PAGE(CSTRING)
C
C Maintains a copy of the VT screen image in memory.
C The first character of each CSTRING must be a FORTRAN carriage control char.
C Does not wrap or scroll characters
C Only handles and expects the following VT escape sequences:
C
C esc[2J          Clear screen
C esc[r;cH        Move cursor
C esc[0m          Normal video          - ignored
C esc[7m          Reverse video         - ignored
C esc[0K          Erase from cursor to end of line
C
C Only handles the following control characters:
C
C Carriage return (13)
C Line feed (10)
C
C For printing of the page to work the logical H_DISPLAY_TEXT_PRINT_COMMAND
C must have been defined.  Something along the following lines is recommended:
C
C DEFINE H_DISPLAY_TEXT_PRINT_COMMAND -
C  "PRINT/QUEU=LPS17/PARA=DATA=ANSI/DELE/FORM=MARGIN/NOIDEN"
C
C Note that logical names can't be used in the print command
C
      IMPLICIT  NONE
      CHARACTER CSTRING*(*),CVDU(24)*80,CC*2,CR*2,COMMAND*80,CFILE*80
      INTEGER*4 IC,IR,IOS,INXT,IDONE,I,LEN,ILUN,LAN
      INCLUDE   'DCOM_DEF.FOR'
       
C Handle special cases
      IF(CSTRING.EQ.'INITIALISE') THEN
        DO I=1,24
          CVDU(I)=' '
        END DO
        RETURN
      END IF    
C
      IF(CSTRING.EQ.'PRINT') THEN
        CALL GET_LOGICAL('H_DISPLAY_TEXT_PRINT_COMMAND',COMMAND)
        IF(COMMAND.EQ.' ') RETURN
        CFILE='H_DISPLAY_'//DSPDEV(5:6)//'.TXT;'
        CALL LIB$GET_LUN(ILUN)
        OPEN(ILUN,FILE=CFILE,STATUS='NEW',CARRIAGECONTROL='LIST',
     &      IOSTAT=IOS)
          DO I=1,24
            WRITE(ILUN,'(A)',IOSTAT=IOS) CVDU(I)
          END DO
        CLOSE(ILUN,IOSTAT=IOS)
        CALL LIB$FREE_LUN(ILUN)
        CALL LIB$SPAWN(COMMAND(1:LAN(COMMAND))//' '//
     &       CFILE(1:LAN(CFILE)),,,1+2+4+8,,,,,,,,)
        RETURN
      END IF    
C Do start of line carriage control
      IF(CSTRING(1:1).EQ.' ') IR=IR+1
      IF(CSTRING(1:1).EQ.'0') IR=IR+2
      IF(CSTRING(1:1).EQ.'$') IR=IR+1
      IF(CSTRING(1:1).EQ.'1') THEN
        DO I=1,24
          CVDU(I)=' '
        END DO
        IR=1
        IC=1
      END IF
C Interpret the line
      IDONE=1                          !Characters processed
      DO WHILE(IDONE.LT.LEN(CSTRING))
        INXT=IDONE+1
        IF(CSTRING(INXT:INXT).EQ.CHAR(13)) THEN         !Carriage return
          IC=1
          IDONE=IDONE+1
        ELSE IF(CSTRING(INXT:INXT).EQ.CHAR(10)) THEN    !Line feed
          IR=IR+1
          IDONE=IDONE+1
        ELSE IF(CSTRING(INXT:INXT).EQ.CHAR(27)) THEN    !Escape sequence
          IF(CSTRING(INXT+1:INXT+3).EQ.'[0m') THEN      !Ignore
            IDONE=IDONE+4
          ELSE IF(CSTRING(INXT+1:INXT+3).EQ.'[7m') THEN !Ignore
            IDONE=IDONE+4
          ELSE IF(CSTRING(INXT+1:INXT+3).EQ.'[0K') THEN !Erase to end of line
            IF(IR.LE.24.AND.IC.LE.80) CVDU(IR)(IC:)=' '
            IDONE=IDONE+4
          ELSE IF(CSTRING(INXT+1:INXT+3).EQ.'[2J') THEN !Clear screen
            DO I=1,24
              CVDU(I)=' '
            END DO
            IDONE=IDONE+4
          ELSE                                          !Must be move cursor
            DO WHILE(CSTRING(INXT:INXT).NE.';')
              INXT=INXT+1
            END DO
            CR=CSTRING(INXT-2:INXT-1)
            IF(CR(1:1).EQ.'[') CR(1:1)='0'
            DO WHILE(CSTRING(INXT:INXT).NE.'H')
              INXT=INXT+1
            END DO
            CC=CSTRING(INXT-2:INXT-1)
            IF(CC(1:1).EQ.';') CC(1:1)='0'
            READ(CR,'(I2)',IOSTAT=IOS) IR
            READ(CC,'(I2)',IOSTAT=IOS) IC
            IDONE=INXT
          END IF
        ELSE
          IF(IR.LE.24.AND.IC.LE.80) CVDU(IR)(IC:IC)=CSTRING(INXT:INXT)
          IC=IC+1
          IDONE=IDONE+1
        END IF
      END DO
C Do end of line carriage control
      IF(CSTRING(1:1).EQ.' ') IC=1
      IF(CSTRING(1:1).EQ.'0') IC=1
      IF(CSTRING(1:1).EQ.'1') IC=1
      IF(CSTRING(1:1).EQ.'+') IC=1
      RETURN
      END
C*******************************************************************************
      SUBROUTINE INPUT_ABORT
C
C Aborts unsolicted keyboard input, (so can take input through FORTRAN reads).
C Note that this causes an immediate AST which must be handled.
C
      IMPLICIT  NONE
      INTEGER*4 ISTAT
      INCLUDE   '($SYSSRVNAM)'
      INCLUDE   'DCOM_DEF.FOR'

      ISTAT=SYS$CANCEL(%VAL(ICHAN))
      IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      RETURN
      END
C*******************************************************************************
      SUBROUTINE INPUT_AST
C
C This routine is used because FORTRAN routines can't call themselves
C
      CALL INPUT_AST1
      RETURN
      END
C*******************************************************************************
      SUBROUTINE INPUT_AST1
C
C Called asynchronously when a character is received from the keyboard:
C
C 1.  Character is returned in INPUT
C 2.  Sets up the next asynchronous trap
C
C Subroutines called:
C
C   INPUT_AST This file
C
C Note:
C
C 1.  All characters are converted to upper case
C 2.  The typeahead buffer is purged beforehand
C 3.  CTRL/Y is trapped before this routine is called
C 4.  The program aborts on any read errors
C
C V1.00  13/09/99  W.D.N.JACKSON
C
      IMPLICIT  NONE
      INTEGER*4 IFNCT,ISTAT,IOSB(2)
      INCLUDE   '($SYSSRVNAM)'
      INCLUDE   '($IODEF)'
      INCLUDE   '($SSDEF)'
      INCLUDE   'DCOM_DEF.FOR'
      EXTERNAL  INPUT_AST
      VOLATILE  IOSB

      IF(IOSB(1).EQ.SS$_ABORT) THEN    !Do nothing if was SYS$CANCEL
        IOSB(1)=0
        RETURN
      END IF
      IFNCT=IO$_READVBLK.OR.IO$M_NOECHO.OR.IO$M_CVTLOW
     &    .OR.IO$M_NOFILTR
      ISTAT=SYS$QIO(%VAL(10),
     -    %VAL(ICHAN),%VAL(IFNCT),IOSB,%REF(INPUT_AST),,
     -    %REF(INPUT),%VAL(1),,,,)     !Read one character
      IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      RETURN
      END     
C*******************************************************************************
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
C*******************************************************************************
      SUBROUTINE CHECK_KEY(CSTR,IKEY)
C
C Checks for valid immediate key presses, and either processes them on the spot
C or returns with key value if they are to bring the plot to an end.  Returns
C 0 if no valid key press.  CSTR is a string containing valid key press
C characters.
C
      IMPLICIT  NONE
      INTEGER*4 IKEY,I,LEN
      CHARACTER CSTR*(*)
      INCLUDE   'DCOM_DEF.FOR'
C
      IKEY=0
      IF(INPUT.EQ.0) RETURN   
      IF(CHAR(INPUT).EQ.'F') THEN
        CALL VIDPNT
        INPUT=0
        RETURN
      END IF
      IF(INPUT.EQ.23) THEN             !CTRL/W
        CALL VIDRST                    !Reset video switch
        INPUT=0
        RETURN
      END IF
      DO I=1,LEN(CSTR)
        IF(INPUT.EQ.ICHAR(CSTR(I:I))) IKEY=INPUT
      END DO
      INPUT=0                          !Clear any invalid input
      RETURN
      END
C*******************************************************************************
      SUBROUTINE WAIT_KD(IKEY)
C
C Waits for the next set of derivations to become available (every 3s) or for
C a key with a value of A to H to be pressed.  IK has a value of 0 on return
C if no key was pressed, a value of 1 to 8 if keys A to H (or a to h) were
C pressed, and 13 if the return key was pressed.  Note that the key is not
C echoed when pressed.
C
C This subroutine works by checking for changes five times a second.
C
      IMPLICIT  NONE
      INTEGER*4 IKEY,IDERNO
      INCLUDE   'DCOM_DEF.FOR'
      INCLUDE   'HCOM_DEF.FOR'

      IKEY=0
      INPUT=0
      IDERNO=STATUS(2)                 !Pointer to latest derivations
      DO WHILE(.TRUE.)
        IF(STATUS(2).NE.IDERNO) RETURN !Check for new derivations
        IF(INPUT.NE.0) THEN
          IF(INPUT.EQ.13) THEN         !CR
            IKEY=13
            RETURN
          ELSE IF(INPUT.GE.65.AND.INPUT.LE.72) THEN !A to H
            IKEY=INPUT-64
            RETURN
          ELSE                         !Invalid character
            INPUT=0
          END IF
        END IF
        CALL LIB$WAIT(0.2)
      END DO
      END
C*******************************************************************************
      SUBROUTINE UPDT_STATUS(ILIST)
C
C Writes a timed message to the log file each time a new plotting/display
C routine is called so can see what is being called and when.  Will eventually
C be used to keep a track of all current displays, so can copy/print them.
C
      IMPLICIT  NONE
      INTEGER*4 ILIST(64),IOS,LAN,J,JJ
      CHARACTER CTIM*8,CLINE*80
      INCLUDE   'DCOM_DEF.FOR'

      IF(ILIST(1).EQ.0) THEN           !Initialize argument list
        DO J=1,16
          IARGLST(1,J)=0
        END DO
        RETURN
      END IF
C 
      WRITE(CLINE,'(16I5)',IOSTAT=IOS) (ILIST(J),J=2,ILIST(1))
      CALL TIME(CTIM)
      TYPE *,CTIM//' '//CLINE(1:LAN(CLINE))
C
      DO J=16,2,-1                     !Shuffle each existing list down one
        DO JJ=1,IARGLST(1,J-1)
          IARGLST(JJ,J)=IARGLST(JJ,J-1)
        END DO
      END DO
      DO JJ=1,ILIST(1)                 !Put the latest list at the top
        IARGLST(JJ,1)=ILIST(JJ)
      END DO
      RETURN
      END
