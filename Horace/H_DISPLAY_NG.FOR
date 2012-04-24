      PROGRAM   H_DISPLAY_NG
!
! Note:
! HORACE must be default directory
! Two lines to remove just below
!
      IMPLICIT  NONE
      INTEGER*4 ISTAT,INPUT
      INTEGER*2 JCHAN,JOSB(4)
      INCLUDE   '($SYSSRVNAM)'
      COMMON    INPUT,JOSB,JCHAN
      INCLUDE   'HCOM_DEF.FOR'
!
! Assign the device
!
      ISTAT=SYS$ASSIGN('TT:',JCHAN,,)
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      JOSB(1)=1
      OPEN(UNIT=10,FILE='TT:',STATUS='OLD',IOSTAT=ISTAT)
      IF(ISTAT.NE.0) CALL EXIT
      CALL INST_MENU
      CLOSE(UNIT=10,IOSTAT=ISTAT)
      ISTAT=SYS$DASSGN(%VAL(JCHAN))
      CALL EXIT
      END
!*******************************************************************************
      SUBROUTINE VT_OUT(CSTRING)
!
! Simple line out routine, for backwards compatibility with previous more
! complicated requirements.
!
      IMPLICIT  NONE
      INTEGER*4 IOS
      CHARACTER CSTRING*(*)

      WRITE(10,'(A)',IOSTAT=IOS) CSTRING
      RETURN
      END
!*******************************************************************************
      SUBROUTINE VT_IN(CSTRING,ILEN,IOS)
!
! Reads a line from the keyboard.  Note that AST input handling must be
! disabled.
!
      IMPLICIT  NONE
      INTEGER*4 ILEN,IOS
      CHARACTER CSTRING*(*)

      READ(10,'(Q,A)',IOSTAT=IOS) ILEN,CSTRING
      RETURN
      END
!*******************************************************************************
      SUBROUTINE INPUT_AST
!
! This routine is used because FORTRAN routines can't call themselves
!
      CALL INPUT_AST1
      RETURN
      END
!*******************************************************************************
      SUBROUTINE INPUT_AST1
!
! Called asynchronously when a character is received from the keyboard:
!
! 1.  Character is returned in INPUT
! 2.  Sets up the next asynchronous trap
!
! Note that JOSB(1) must be 1 the first time this routine is called.
!
! Subroutines called:
!
!   INPUT_AST This file
!
! The program aborts on any read errors
!
! V1.00  13/09/99  W.D.N.JACKSON
!
      IMPLICIT  NONE
      INTEGER*4 IFNCT,INPUT,ISTAT
      INTEGER*2 JOSB(4),JCHAN
      INCLUDE   '($SYSSRVNAM)'
      INCLUDE   '($IODEF)'
      INCLUDE   '($SSDEF)'
      EXTERNAL  INPUT_AST
      COMMON    INPUT,JOSB,JCHAN

      IF(JOSB(1).NE.SS$_NORMAL) THEN   !Do nothing if was an error
        JOSB(1)=0
        RETURN
      END IF
     
      IFNCT=IO$_READVBLK.OR.IO$M_NOECHO
      ISTAT=SYS$QIO(,%VAL(JCHAN),%VAL(IFNCT),JOSB,%REF(INPUT_AST),,
     -    INPUT,%VAL(1),,,,)           !Read one character
      IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      RETURN
      END     
!*******************************************************************************
      SUBROUTINE INPUT_ABORT
!
! Aborts unsolicted keyboard input, (so can take input through FORTRAN reads).
! Note that this causes an immediate AST which must be handled.
!
      IMPLICIT  NONE
      INTEGER*4 ISTAT,INPUT
      INTEGER*2 JOSB(4),JCHAN
      INCLUDE   '($SYSSRVNAM)'
      COMMON    INPUT,JOSB,JCHAN

      ISTAT=SYS$CANCEL(%VAL(JCHAN))
      IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      JOSB(1)=1                        !Make sure can start again
      RETURN
      END
!*******************************************************************************
      SUBROUTINE CHECK_KEY(CSTR,IKEY)
!
! Checks for valid immediate key presses, and either processes them on the spot
! or returns with key value if they are to bring the plot to an end.  Returns
! 0 if no valid key press.  CSTR is a string containing valid key press
! characters.
!
      IMPLICIT  NONE
      INTEGER*4 IKEY,I,LEN,INPUT
      INTEGER*2 JOSB(4),JCHAN
      CHARACTER CSTR*(*)
      COMMON    INPUT,JOSB,JCHAN
!
      IKEY=0
      IF(INPUT.EQ.0) RETURN   
      DO I=1,LEN(CSTR)
        IF(INPUT.EQ.ICHAR(CSTR(I:I))) IKEY=INPUT
      END DO
      INPUT=0                          !Clear any invalid input
      RETURN
      END
!*******************************************************************************
      SUBROUTINE VT_EDIT(CSTRING)
!
!
!
      CHARACTER  CSTRING*(*),CS*80

      CS=CSTRING
      CALL LIB$SPAWN('@H_EDIT_TEMP.COM TTT',,,2+4+8,,,,,,,,)
      RETURN
      END
