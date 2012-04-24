C
C MESSAGE_TEM.FOR
C
C This is a template file for use in other procedures.
C
********************************************************************************
      SUBROUTINE LOG_MESS(MESSAGE)
C
C Writes message to log file (SYS$OUTPUT) consisting of program name, date,
C time and message.  These messages should be used to record any noteworthy
C events, as an aid to analysis of events after a flight.
C
      CHARACTER*(*) MESSAGE
      CHARACTER CDAT*9,CTIM*10
      CALL DATE(CDAT)
      CALL TIME(CTIM)
      TYPE *,'H_*** '//CDAT//' '//CTIM//MESSAGE
      RETURN
      END
********************************************************************************
      SUBROUTINE ST_MESS(MESSAGE)
C
C Write message to the logical H_***_STAT in the group logical name
C table.  GRPNAM and SYSNAM privileges are required.  These messages should
C be used to indicate the current status of a program and its processing.
C
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
      ISTAT=SYS$CRELNM(,'LNM$GROUP','H_***_STAT',PSL$C_SUPER,TLIST)
      RETURN
      END
********************************************************************************
      SUBROUTINE FL_MESS(MESSAGE)
C
C Sends an immediate message to the flight leader's display.  The message 
C is only sent when any existing output ends, so it should not interfere
C with graphics displays.  OPER privilege is required.  These messages should
C only be sent when there is a problem or potential problem which must be drawn
C to the Flight Leader's attention.  The use of this routine has been
C superseded in most cases by HM_MESS, which also writes the message to file
C for later recall.
C
      CHARACTER*(*) MESSAGE
      INCLUDE       '($SYSSRVNAM)'
      INCLUDE       '($BRKDEF)'

      ISTAT=SYS$BRKTHRU(,MESSAGE,'SBD_FL',%VAL(BRK$C_DEVICE),,,,,,,)
      RETURN
      END
********************************************************************************
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
      CHARACTER*(*) CMESS
      CHARACTER     CMESSA*72,CERRMESS*132
      BYTE          BMESSA(72)
      EQUIVALENCE   (CMESSA,BMESSA)
      INTEGER*2     IOSB(4)
      INTEGER*4     IFIRST/1/
      INCLUDE       '($SYSSRVNAM)'
      INCLUDE       '($IODEF)'

      CMESSA=CMESS
      IF(IFIRST.EQ.1) THEN
        ISTAT=SYS$ASSIGN('H_MESSAGE_MBX',ICHAN,,)
        IF(.NOT.ISTAT) THEN            !Don't abort program on error
          CALL SYS$GETMSG(%VAL(ISTAT),MLEN,CERRMESS,%VAL(1),) !Just print message
          CALL LOG_MESS(CERRMESS(1:MLEN))
        END IF
        IFIRST=0
      END IF
      IFUNCT=IO$_WRITEVBLK
      ISTAT=SYS$QIO(,%VAL(ICHAN),%VAL(IFUNCT),IOSB,,,
     -    BMESSA,%VAL(72),,,,)
      IF(.NOT.ISTAT) THEN              !Don't abort program on error
        CALL SYS$GETMSG(%VAL(ISTAT),MLEN,CERRMESS,%VAL(1),) !Just print message
        CALL LOG_MESS(CERRMESS(1:MLEN))
      END IF
      RETURN
      END
