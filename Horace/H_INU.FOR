      PROGRAM H_INU
C
C Handles all communication between HORACE and the INU Serial Interface (ISI).
C All data is sent via the RS232/423 port with the logical name definition of 
C INU_CHAN, so this logical must be defined before this program is run.  
C If the program can't open the output channel, it sends a message to the log 
C file and keeps trying until it can.
C
C The main purpose of this program is to handle requests from the H_DISPLAY
C program for INU functions requiring the ISI.  It also gets a limited amount
C of information automatically for recording in the H_INU.LOG file.  The
C H_DISPLAY program requests an operation by putting a positive number in 
C INUDATA(1), which is in the HORACE global common H_COMMON.  The following 
C numbers are handled:
C
C  1    H_DISPLAY requests INU message 01 via the ISI
C  6    H_DISPLAY requests INU message 06 via the ISI
C  7    H_DISPLAY requests INU message 07 via the ISI
C  9    H_DISPLAY requests INU message 09 via the ISI
C 10    H_DISPLAY requests INU message 10 via the ISI
C 11    H_DISPLAY requests INU message 11 via the ISI
C 12    H_DISPLAY requests INU message 12 via the ISI
C 13    H_DISPLAY requests INU message 13 via the ISI
C 14    H_DISPLAY requests INU message 14 via the ISI
C 20    H_DISPLAY requests INU mode change to GC ALIGN via the ISI
C 21    H_DISPLAY requests INU mode change to NAVIGATE via the ISI
C 22    H_DISPLAY requests INU mode change to TEST via the ISI
C 30    H_DISPLAY requests INU BIT/History words to be cleared via the ISI
C 40    H_DISPLAY requests INU miscellaneous parameter via the ISI
C 41    H_DISPLAY requests this program to dump INU data to H_INU.LOG
C
C In the case of items 1 to 14 the message is returned in INUDATA(2) to (33)
C In the case of items 20 and 22 the present position (lat/lng) is passed to
C this program by H_DISPLAY in INUDATA(3) to (6).  In the case of item 40
C the required miscellaneous parameter number is passed to this program by 
C H_DISPLAY in INUDATA(2).
C
C This program uses INUDATA(1) to indicate back to H_DISPLAY the status of its
C request as follows:
C
C   INUDATA(1) = 0   Request has been successfully carried out
C              = -m  Request for action m is still being carried out
C              = -99 Failed to carry out item - normally after a 2s timeout
C
C Items 20,21,22 return with 0 when the commands have been sent to the INU
C successfully, but don't check that the mode has actually changed.
C
C As well as carrying out the above tasks this program also interrogates the
C INU every 2 minutes for a set of 60 Miscellaneous Parameters, and loads them
C into INUDATA(34) to (213) for use by H_DISPLAY.  It only does this when the
C H_DISPLAY program is using the ISI menu.  When this program first detects
C that the INU is in NAVIGATE it interrogates the INU for all messages and
C Misc Paras and writes them all out to the H_INU.LOG file.  It also does this
C when item 41 is requested by H_DISPLAY.
C
C Note that reading of miscellaneous parameters is relatively slow and it is
C likely to take between 15 s and a minute to read the full set of 60
C parameters.
C
C Note that items 1 to 14 will work even if the IPC or a CDU is using the other
C 1553 network.  Items 20 to 41 will not work properly if the other network is
C being actively used, although the Misc Paras can normally be retrieved rather
C slowly if one keeps trying.
C
C Build with:
C 
C  $ FORT H_INU
C  $ LINK H_INU,H_COMMON/OPT
C
C    where H_COMMON.OPT contains the line SYS$LIBRARY:H_COMMON/SHAREABLE
C
C V1.00  25/11/93  W.D.N.JACKSON
C
      IMPLICIT NONE
      INCLUDE 'HCOM_DEF.FOR'
      INCLUDE '($SYSSRVNAM)'
      INCLUDE '($IODEF)'
      INCLUDE '($DVIDEF)'
      INTEGER*4 IMISCP(60),ISTAT,ILEN,I,ILL,ILC,IM,IMP,LIB$GETDVI,
     &    IFUNCT,IOS
      LOGICAL*4 LDUMP,LNAV,LFORCEDMP
      CHARACTER CMISCP(60)*6,CMP*6
      INTEGER*2 ICHAN,IOSB(4),IMESS(32),IDUM
      CHARACTER CNAME*64,CMODE*8,CPP*20
      REAL*4 RLAT,RLNG
      EQUIVALENCE (RLAT,INUDATA(3)),(RLNG,INUDATA(5))
      EQUIVALENCE (CMISCP,INUDATA(34))
      EQUIVALENCE (CMP,INUDATA(216))
      DATA IMISCP /0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
     -    20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,
     -    60,61,62,63,64,65,-1,106,111,112,113,114,116,118,128,132,133,
     -    161,177,178/

      ISTAT=SYS$ALLOC('INU_CHAN',,,,)  !Allocate the serial port
      IF(.NOT.ISTAT) THEN
        CALL LOG_MESS('Failed INU_CHAN allocation. Stopping process')
        CALL EXIT
      END IF
      ISTAT=SYS$ASSIGN('INU_CHAN',ICHAN,,) !Now assign a channel to it
      IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      ISTAT=LIB$GETDVI(DVI$_DEVNAM,ICHAN,,,CNAME,ILEN) !Get device name
      IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      IF(CNAME(2:4).EQ.'LTA') THEN     !If terminal server
        IFUNCT=IO$_TTY_PORT.OR.IO$M_LT_CONNECT !Connect function
        ISTAT=SYS$QIOW(,%VAL(ICHAN),%VAL(IFUNCT),IOSB,,,,,,,,) !Make connection
        IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT)) !Abort if times out
        IF(.NOT.IOSB(1)) THEN
          TYPE *,'LAT rejection code ',IOSB(2)
          CALL LIB$SIGNAL(%VAL(IOSB(2)))
          CALL LIB$SIGNAL(%VAL(IOSB(1)))
        END IF
      END IF

      DO I=1,33                        !Zero control word and data area
        INUDATA(I)=0
      END DO
      DO I=34,213                      !Set misc para area to spaces
        INUDATA(I)='2020'X
      END DO
      INUDATA(214)=0                   !Show H_DISPLAY ISI menu not in use
      LDUMP=.FALSE.
      LFORCEDMP=.FALSE.
      LNAV=.FALSE.
      ILL=0
      ILC=0
      CALL SND_MESS(ICHAN,0,IDUM,ISTAT) !Send FFFF to clear ISI buffer
      DO WHILE(.TRUE.)
        ILC=ILC+1                     !Loop counter
        IF(INUDATA(1).GE.1.AND.INUDATA(1).LE.14) THEN
          IM=INUDATA(1)
          INUDATA(1)=-INUDATA(1)
          CALL SND_MESS(ICHAN,IM,INUDATA(2),ISTAT)
          IF(ISTAT.GE.1) INUDATA(1)=0
          IF(ISTAT.LT.1) INUDATA(1)=-99
        ELSE IF(INUDATA(1).GE.20.AND.INUDATA(1).LE.22) THEN
          IM=INUDATA(1)
          INUDATA(1)=-INUDATA(1)
          IF(IM.EQ.20) CMODE='GC ALIGN'
          IF(IM.EQ.21) CMODE='NAVIGATE'
          IF(IM.EQ.22) CMODE='TEST'
          CALL MDE_CHG(ICHAN,CMODE,ISTAT)
          IF(ISTAT.GE.1) CALL LOG_MESS('INU mode change to '//CMODE)
          IF((IM.EQ.20.OR.IM.EQ.22).AND.ISTAT.EQ.1) THEN
            CALL SND_PP(ICHAN,RLAT,RLNG,ISTAT)
            WRITE(CPP,10,IOSTAT=IOS) RLAT,RLNG
            IF(ISTAT.GE.1) CALL LOG_MESS('INU PP sent - '//CPP)
          END IF
          IF(ISTAT.GE.1) INUDATA(1)=0
          IF(ISTAT.LT.1) INUDATA(1)=-99
        ELSE IF(INUDATA(1).EQ.30) THEN
          INUDATA(1)=-INUDATA(1)
          CALL CLR_MH(ICHAN,ISTAT)
          IF(ISTAT.GE.1) 
     -        CALL LOG_MESS('INU Maintenance History Word cleared')
          IF(ISTAT.GE.1) INUDATA(1)=0
          IF(ISTAT.LT.1) INUDATA(1)=-99
        ELSE IF(INUDATA(1).EQ.40) THEN
          IMP=INUDATA(2)
          INUDATA(1)=-INUDATA(1)
          CALL GET_MP(ICHAN,IMP,CMP,ISTAT)
          IF(ISTAT.GE.1) INUDATA(1)=0
          IF(ISTAT.LT.1) INUDATA(1)=-99
        ELSE IF(INUDATA(1).EQ.41) THEN
          LFORCEDMP=.TRUE.
          ILL=1
          INUDATA(1)=0
        END IF
        CALL LIB$WAIT(0.2)

        IF(.NOT.LNAV.AND.MOD(ILC,600).EQ.1) THEN !Check for NAV every 2 mins
          CALL SND_MESS(ICHAN,1,IMESS,ISTAT)
          IF(IMESS(29).EQ.'0080'X) THEN  !If in NAVIGATE
            LNAV=.TRUE.
            ILL=1
          END IF
        END IF

        IF(MOD(ILC,600).EQ.1.AND.INUDATA(214).NE.0) ILL=1
        IF(ILL.GE.1.AND.ILL.LE.60) THEN
          IF(IMISCP(ILL).GT.-1) THEN
            CALL GET_MP(ICHAN,IMISCP(ILL),CMISCP(ILL),ISTAT)
            IF(ISTAT.EQ.1) ILL=ILL+1
          ELSE
            ILL=ILL+1
          END IF
        END IF

        IF(((.NOT.LDUMP.AND.LNAV).OR.LFORCEDMP).AND.ILL.GT.60) THEN
          CALL LOG_MESS('INU Message and Parameter Dump')
          CALL SND_MESS(ICHAN,1,INUDATA(2),ISTAT)
          WRITE(6,11)
          CALL INU_ISI_DISP_MESS(6,1,'FULL')
          CALL SND_MESS(ICHAN,6,INUDATA(2),ISTAT)
          WRITE(6,11)
          CALL INU_ISI_DISP_MESS(6,6,'FULL')
          CALL SND_MESS(ICHAN,7,INUDATA(2),ISTAT)
          WRITE(6,11)
          CALL INU_ISI_DISP_MESS(6,7,'FULL')
          CALL SND_MESS(ICHAN,9,INUDATA(2),ISTAT)
          WRITE(6,11)
          CALL INU_ISI_DISP_MESS(6,9,'FULL')
          CALL SND_MESS(ICHAN,10,INUDATA(2),ISTAT)
          WRITE(6,11)
          CALL INU_ISI_DISP_MESS(6,10,'FULL')
          CALL SND_MESS(ICHAN,11,INUDATA(2),ISTAT)
          WRITE(6,11)
          CALL INU_ISI_DISP_MESS(6,11,'FULL')
          CALL SND_MESS(ICHAN,12,INUDATA(2),ISTAT)
          WRITE(6,11)
          CALL INU_ISI_DISP_MESS(6,12,'FULL')
          CALL SND_MESS(ICHAN,13,INUDATA(2),ISTAT)
          WRITE(6,11)
          CALL INU_ISI_DISP_MESS(6,13,'FULL')
          CALL SND_MESS(ICHAN,14,INUDATA(2),ISTAT)
          WRITE(6,11)
          CALL INU_ISI_DISP_MESS(6,14,'FULL')
          WRITE(6,11)
          CALL INU_ISI_DISP_MESS(6,99,'FULL')
          WRITE(6,11)
          IF(.NOT.LFORCEDMP) LDUMP=.TRUE.
          LFORCEDMP=.FALSE.
        END IF
      END DO
10    FORMAT(2(F8.3,2X))
11    FORMAT(X)
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
      TYPE *,'H_INU '//CDAT//' '//CTIM//MESSAGE
      RETURN
      END
C*******************************************************************************
      SUBROUTINE ST_MESS(MESSAGE)
C
C Write message to the logical H_INU_STAT in the group logical name
C table.  GRPNAM and SYSNAM privileges are required.
C
      IMPLICIT NONE
      INTEGER*4 ISTAT
      CHARACTER*(*) MESSAGE
      INCLUDE '($SYSSRVNAM)'
      INCLUDE '($LNMDEF)'
      INCLUDE '($PSLDEF)'

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
      ISTAT=SYS$CRELNM(,'LNM$GROUP','H_INU_STAT',PSL$C_SUPER,TLIST)
      RETURN
      END
C*******************************************************************************
      SUBROUTINE SND_PP(ICHAN,RLAT,RLNG,ISTAT)
C
C Sends present lat/lng to the INU, as required during GC ALIGN and TEST modes.
C RLAT and RLNG should be REAL*4 numbers specifying lat and lng in degrees.
C This routine codes the necessary D01 message and sends it to the INU, waits
C for the INU to acknowledge the message through the I06 message, sends a D01
C message clearing the present position data, and waits for the INU to
C acknowledge through the I06 message that it has accepted the new positions.
C
C Returns with ISTAT=1 if new position successfully loaded, else with 0 or -1
C
      IMPLICIT  NONE
      INTEGER*2 ICHAN,D01(7),I06(32),JLAT(2),JLNG(2)
      REAL*4 RLAT,RLNG
      INTEGER*4 ISTAT,ILAT,ILNG,ISTS,I
      EQUIVALENCE (ILAT,JLAT),(ILNG,JLNG)

      ISTAT=-1
      IF(RLAT.LT.-90..OR.RLAT.GT.90.) RETURN
      IF(RLNG.LT.-180..OR.RLNG.GT.180.) RETURN
      ILAT=NINT(RLAT/180.*2.**31.)
      ILNG=NINT(RLNG/180.*2.**31.)
      D01(1)='2400'X                   !Use Test mode, since normally invalid
      D01(2)='0800'X                   !Present Posn lat/lng
      D01(3)=JLAT(2)
      D01(4)=JLAT(1)
      D01(5)=JLNG(2)
      D01(6)=JLNG(1)
      D01(7)=0
      CALL SND_MESS(ICHAN,-1,D01,ISTS) !Send D01 with PP data
      IF(ISTS.NE.1) RETURN             !Return on message error
c      I06(22)=0                        !Initialize loop
c      ICNT=10                          !Wait up to 2 s for INU to ack PP data
c      DO WHILE(.NOT.BTEST(I06(22),11).AND.ICNT.GT.0) !Check data rdy flg ackn
        CALL SND_MESS(ICHAN,6,I06,ISTS)
        IF(ISTS.NE.1) RETURN
c        IF(BTEST(I06(22),2)) RETURN    !INU can't process D01 message
c        CALL LIB$WAIT(0.2)
c        ICNT=ICNT-1
c      END DO
c      IF(ICNT.EQ.0) RETURN             !Return if message error
      DO I=1,7                         !Clear the data in D01
        D01(I)=0
      END DO
      CALL SND_MESS(ICHAN,-1,D01,ISTS) !Send cleared D01
      IF(ISTS.NE.1) RETURN             !Return on message error
c      ICNT=10                          !Wait up to 2 s for INU to process PP
c      DO WHILE(BTEST(I06(22),11).AND.ICNT.GT.0) !Chk for data rdy flg ackn clr
        CALL SND_MESS(ICHAN,6,I06,ISTS)
        IF(ISTS.NE.1) RETURN
c        IF(BTEST(I06(22),2)) RETURN    !INU can't process D01 message
c        CALL LIB$WAIT(0.2)
c        ICNT=ICNT-1
c      END DO
c      IF(ICNT.GT.0) ISTAT=1
      ISTAT=1
      RETURN
      END
C*******************************************************************************
      SUBROUTINE CLR_MH(ICHAN,ISTAT)
C
C Sends a Miscellaneous Parameter Insert command to clear the Maintenance
C History word in the INU.  This becomes set after the INU BIT detects possible
C errors, and stays set until cleared.  This word should only be cleared by
C Electronics section, after making a note of the error word and the possible
C faulty boards.
C
C This routine codes the necessary D01 message and sends it to the INU, waits
C for the INU to acknowledge the message through the I06 message, sends a D01
C message clearing the present position data, and waits for the INU to
C acknowledge through the I06 message that it has accepted the new positions.
C
C Returns with ISTAT=1 if Miscellaneous parameter 160 successfully cleared,
C else with 0 or -1
C
      IMPLICIT  NONE
      INTEGER*2 ICHAN,D01(7),I06(32)
      INTEGER*4 ISTAT,ISTS,I
      CHARACTER CMP*6
      EQUIVALENCE (CMP,D01(4))

      ISTAT=-1
      D01(1)='2400'X                   !Use Test mode, since normally invalid
      D01(2)='0A40'X                   !Insert Misc Para mode
      D01(3)=160                       !BIT/History clear
      CMP=' 00000'                     !Misc Para Ascii string
      D01(7)=0
      CALL SND_MESS(ICHAN,-1,D01,ISTS) !Send D01 with Misc Para Insert
      IF(ISTS.NE.1) RETURN             !Return on message error
      CALL SND_MESS(ICHAN,6,I06,ISTS)
      IF(ISTS.NE.1) RETURN
      DO I=1,7                         !Clear the data in D01
        D01(I)=0
      END DO
      CALL SND_MESS(ICHAN,-1,D01,ISTS) !Send cleared D01
      IF(ISTS.NE.1) RETURN             !Return on message error
      CALL SND_MESS(ICHAN,6,I06,ISTS)
      IF(ISTS.NE.1) RETURN
      ISTAT=1
      RETURN
      END
C*******************************************************************************
      SUBROUTINE GET_MP(ICHAN,IMP,CMPAR,ISTAT)
C
C Gets a miscellaneous parameter, specified by IMP, from the INU and returns it
C as a six character string in CMPAR.  Send Misc Para read request, waits for
C acknowlegment, clears Misc Para read request, waits for acknowlegment
C that the data is ready, and gets data.
C
C Returns with ISTAT=1 if misc para successfully got, else with 0 or -1
C
C Note that the method used is less than rigorous since it does not check for
C the proper setting and clearing of the Data Ready Flag Acknowlege bit of
C I06-22.  However the right number and sequence of messages are sent, and
C the results seem to be correct.  Furthermore it overcomes the problem that
C the Illegal Command bit of I06-22 sometimes seems to be set (because asking
C for TEST mode?) instead of the Acknowlege bit.
C
      IMPLICIT  NONE
      CHARACTER CMPAR*6
      INTEGER*2 ICHAN,D01(7),I06(32),I07(32)
      INTEGER*4 ISTAT,IMP,I,ISTS
      BYTE      BMPAR(6)
      EQUIVALENCE (I07(30),BMPAR)

      ISTAT=-1
      IF(IMP.LT.0.OR.IMP.GT.185) RETURN
      D01(1)='2400'X                   !Use Test mode, since normally invalid
      D01(2)='0A00'X                   !Read Misc Para mode
      D01(3)=IMP                       !Misc Para to read
      D01(4)=0
      D01(5)=0
      D01(6)=0
      D01(7)=0
      CALL SND_MESS(ICHAN,-1,D01,ISTS) !Send D01 with read misc para data
      IF(ISTS.NE.1) RETURN             !Return on message error
      CALL SND_MESS(ICHAN,6,I06,ISTS)
      IF(ISTS.NE.1) RETURN
      DO I=1,7                         !Clear the data in D01
        D01(I)=0
      END DO
      CALL SND_MESS(ICHAN,-1,D01,ISTS) !Send cleared D01
      IF(ISTS.NE.1) RETURN             !Return on message error
      CALL SND_MESS(ICHAN,6,I06,ISTS)
      IF(ISTS.NE.1) RETURN
      CALL SND_MESS(ICHAN,7,I07,ISTS)
      IF(ISTS.LT.1) RETURN
      CMPAR(1:1)=CHAR(BMPAR(2))
      CMPAR(2:2)=CHAR(BMPAR(1))
      CMPAR(3:3)=CHAR(BMPAR(4))
      CMPAR(4:4)=CHAR(BMPAR(3))
      CMPAR(5:5)=CHAR(BMPAR(6))
      CMPAR(6:6)=CHAR(BMPAR(5))
      ISTAT=1
      RETURN
      END
C*******************************************************************************
      SUBROUTINE MDE_CHG(ICHAN,CMODE,ISTAT)
C
C Changes the operating mode of the INU by sending a suitable D01 message for
C at least a second.  The only modes supported are GC ALIGN, NAVIGATE and TEST.
C Simply sends the necessary D01 message 6 times at 0.2 s intervals.  No check
C is made that the INU actually changes mode (which it may not if the required
C mode is invalid for the INU's current state).  Returns with ISTAT=1 if the 
C six messages were successfully sent, else with 0 or -1.
C
      IMPLICIT NONE
      CHARACTER*(*) CMODE
      INTEGER*2 ICHAN,D01(7)
      INTEGER*4 ISTAT,I,ICNT

      IF(CMODE.EQ.'GC ALIGN') THEN
        D01(1)='0800'X
      ELSE IF(CMODE.EQ.'NAVIGATE') THEN
        D01(1)='1000'X
      ELSE IF(CMODE.EQ.'TEST') THEN
        D01(1)='2400'X
      ELSE
        ISTAT=-1
        RETURN
      END IF

      DO I=2,7
        D01(I)=0
      END DO
      ICNT=6
      DO WHILE(ICNT.GT.0)
        CALL SND_MESS(ICHAN,-1,D01,ISTAT)
        IF(ISTAT.NE.1) ICNT=0
        IF(ICNT.GT.1) CALL LIB$WAIT(0.2)
        ICNT=ICNT-1
      END DO
      RETURN
      END
C*******************************************************************************
      SUBROUTINE SND_MESS(ICHAN,IM,IDATA,ISTAT)
C
C Sends a message to, or gets a message from the Honeywell H423 INU, via the 
C Serial Interface.   Only the following message requests, specified by
C matching values of IM, are handled: I01,I06,I07,I09,I10,I11,I12,I13,I14.  
C Only the following message to the INU is handled, specified by an IM value of
C -1: D01.  If IM is specified as 0 then two FF bytes are sent to the ISI to 
C clear its input buffer. For D01 messages the seven words of the message must
C be supplied in IDATA. For the I messages the message is returned in IDATA.
C 
C If the IIU does not respond within 2 seconds with the required message or
C confirmation, ISTAT is returned as 0, if the IIU returns any error or invalid
C message ISTAT is returned as -1, else ISTAT is returned with a 1.
C 
C This routines adds the correct command word to the message, and checks for
C the correct reply.
C
C ICHAN is a VMS channel already opened to the serial port driving the Serial
C Interface.
C
C The command words are made up as follows (see SNU 84-1 para 60.4.2.2.2):
C
C     bits 15-11  The RT address of the H423 (00001)
C     bit     10  0 for receive, 1 for transmit
C     bits   9-5  the required message subaddress
C     bits   4-0  the required no of words - note that 32 is coded as 00000
C
      IMPLICIT NONE
      INCLUDE '($IODEF)'
      INCLUDE '($SSDEF)'
      INTEGER*2 ICHAN,IDATA(32),IOSB(4),ICWORD(10),IMESS(33)
      INTEGER*4 IM,ISTAT,ICOUNT(10),SYS$QIOW,ICLRWRD,ISENDC(10)
      INTEGER*4 ISTS,IFUNC,IN,I
      INTEGER*4 MASK(2) /2*0/
      DATA ICWORD /'0E00'X,'0F20'X,'0F60'X,'0E77'X,'0E5C'X,'0EDA'X,
     -    '0EEF'X,'0F18'X,'0E80'X,'0B47'X/
      DATA ISENDC /1,1,1,1,1,1,1,1,1,8/
      DATA ICOUNT /33,33,33,24,29,27,16,25,33,1/
      DATA ICLRWRD /'FFFF'X/

      ISTAT=-1
      IF(IM.EQ.0) THEN                 !Clear IIU input buffer
        IFUNC=IO$_WRITEVBLK.OR.IO$M_NOFORMAT.OR.IO$M_CANCTRLO
        ISTS=SYS$QIOW(,%VAL(ICHAN),%VAL(IFUNC),IOSB,,,
     -      ICLRWRD,%VAL(2),,%VAL(0),,)
        IF(.NOT.ISTS) CALL LIB$SIGNAL(%VAL(ISTS))
        IF(.NOT.IOSB(1)) CALL LIB$SIGNAL(%VAL(IOSB(1)))
        IF(IOSB(1).AND.ISTS) ISTAT=1
        RETURN
      END IF

      IN=0
      IF(IM.EQ.1) IN=1                 !I01
      IF(IM.EQ.6) IN=2                 !I06
      IF(IM.EQ.7) IN=3                 !I07
      IF(IM.EQ.9) IN=4                 !I09
      IF(IM.EQ.10) IN=5                !I10
      IF(IM.EQ.11) IN=6                !I11
      IF(IM.EQ.12) IN=7                !I12
      IF(IM.EQ.13) IN=8                !I13
      IF(IM.EQ.14) IN=9                !I14
      IF(IM.EQ.-1) IN=10               !D01
      IF(IN.EQ.0) RETURN               !Invalid message requested
C Send the request for the message
      IFUNC=IO$_WRITEVBLK.OR.IO$M_NOFORMAT.OR.IO$M_CANCTRLO
      IMESS(1)=ICWORD(IN)
      IF(ISENDC(IN).GT.1) THEN
        DO I=2,ISENDC(IN)
          IMESS(I)=IDATA(I-1)
        END DO
      END IF
      DO I=1,ISENDC(IN)
        CALL REV_BYTE(IMESS(I))        !Reverse bytes before sending
      END DO
!      write(6,'(x,a,33z5.4)',iostat=ists) 'To ISI ',
!     -    (imess(i),i=1,isendc(in))
      ISTS=SYS$QIOW(,%VAL(ICHAN),%VAL(IFUNC),IOSB,,,
     -    IMESS,%VAL(ISENDC(IN)*2),,%VAL(0),,)
      IF(.NOT.ISTS) CALL LIB$SIGNAL(%VAL(ISTS))
      IF(.NOT.IOSB(1)) CALL LIB$SIGNAL(%VAL(IOSB(1)))
      IF(.NOT.ISTS.OR..NOT.IOSB(1)) RETURN
C Now get the returned message, with a 2 second timeout and buffer purge
      IFUNC=IO$_READVBLK.OR.IO$M_TIMED.OR.IO$M_NOECHO.OR.IO$M_PURGE
     -    .OR.IO$M_NOFILTR
      ISTS=SYS$QIOW(,%VAL(ICHAN),%VAL(IFUNC),IOSB,,,
     -    IMESS,%VAL(ICOUNT(IN)*2),%VAL(2),%VAL(%LOC(MASK)),,)
      IF(.NOT.ISTS) CALL LIB$SIGNAL(%VAL(ISTS))
      IF(.NOT.IOSB(1).AND.(IOSB(1).NE.SS$_TIMEOUT)) 
     -    CALL LIB$SIGNAL(%VAL(IOSB(1)))
!      IF(IOSB(1).eq.SS$_TIMEOUT) write(6,'(a)') ' No response timeout' 
!      write(6,'(x,2i6)') iosb(1),iosb(2)      
!      if(iosb(2).gt.1) write(6,'(3(x,16z5.4/))') 
!     -    (imess(i),i=1,iosb(2)/2)
      IF(ISTS) THEN
        IF(IOSB(2).GE.2.AND.IOSB(2).LE.ICOUNT(IN)*2) THEN
          DO I=1,IOSB(2)/2
            CALL REV_BYTE(IMESS(I))    !Reverse bytes before using
          END DO
        END IF
        IF(IOSB(1).EQ.SS$_TIMEOUT) THEN
          ISTAT=0
        ELSE IF(.NOT.IOSB(1).OR.(IOSB(2).NE.ICOUNT(IN)*2)
     -      .OR.(IMESS(1).NE.ICWORD(IN))) THEN
          ISTAT=-1
        ELSE
          ISTAT=1                      !Show success
          IF(ICOUNT(IN).GT.1) THEN     !For I messages transfer data
            DO I=1,ICOUNT(IN)-1
              IDATA(I)=IMESS(I+1)      !Transfer message but not command word
            END DO
          END IF
        END IF
      ELSE
        ISTAT=-1
      END IF
      RETURN
      END
C*******************************************************************************
      SUBROUTINE REV_BYTE(IWORD)
C
C Swaps the two bytes in a 16 bit word
C
      IMPLICIT NONE
      INTEGER*2 IWORD,IW
      BYTE BW(2),BTEMP
      EQUIVALENCE (IW,BW)

      IW=IWORD
      BTEMP=BW(1)
      BW(1)=BW(2)
      BW(2)=BTEMP
      IWORD=IW
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
!*******************************************************************************
      OPTIONS /EXTEND_SOURCE           !Some source lines are up to 132 chars
      SUBROUTINE INU_ISI_DISP_MESS(IL,IM,CMODE)
C
C Displays data from INU messages I01, I06, I07, I09, I10, I11, I12, I13, I14,
C and the Miscellaneous parameters.  IL is the LUN to which to send the data.
C IM is the message number, (99 if Misc Paras required).  CMODE determines how
C the screen is updated - 'BACKGROUND' just puts up the background information,
C 'DATA' just updates the data parts of the display, and 'FULL' writes out
C each line in full, suitable for sending to a file and printing.  If CMODE is
C specified as 'DATAZ' then the display is updated with values of zero.  The INU
C data itself is passed via the INUDATA array of the HCOM common block, and is
C is assumed to be valid and of the correct type during the time which this
C subroutine executes.  Note that all data is displayed in its basic form with
C no interpretation or expansion (except angles, which are converted from 
C pirads to degrees).  All the message decoding in this subroutine is based on
C the information in the SNU 84-1 Rev D INU specification.
C
      IMPLICIT  NONE
      INTEGER*4 ILMPC,IOSW,I,IM,IPPUE,IALT,IMDE,IPPUN,IANU,IMODE,ISFI,
     -    IAS,IMODE1,ISMC,IAT,IMODE2,ISTS,ICTL1,INWUE,ITA,ICTL3,INWUN,
     -    ITTN,IGMU,IOS,ITTS,IVEN,IXART,IXTLT,IXYPTU,IYART,IYTLT,IZART,
     -    IZPAU
      REAL*4    RCNEXZ,RMH,RSMC,RCNEYX,RMHN,RSMCS,RCNEYY,RNLAT,RTAS1,
     -    RCNEYZ,RNLNG,RTH,RCSNXX,RNX,RTT,RCSNXY,RNY,RVFC,RCSNXZ,RNZ,
     -    RVX,RCSNYX,RPA,RVY,RABU,RCSNYY,RPAAA,RVZ1,RAFC,RCSNYZ,RPATT,
     -    RWD,RAR,RDA,RPBCA,RWT,RARCT,RDGH,RPCF,RWV,RASFA,RDTN,RPDGS,
     -    RXCT,RASFU,RDTS,RPEE,RXGBU,RAW,REMH,RPEN,RXGR,RAWCT,REMV,RPGH,
     -    RXR,RAX,RETH,RPGS,RYAAA,RAY,RGCSE,RPI,RYAR,RAZ,RGSFU,RPIR,
     -    RYBCA,RBNW,RGXA,RPITT,RYCT,RBTS,RGXO,RPPLAT,RYGBU,RCCD,RGYA,
     -    RPPLNG,RYGR,RCGX,RGYO,RPTGT,RYR,RCGY,RGZA,RRAAA,RZCT,RCGZ,
     -    RGZO,RRBCA,RZGBU,RCMV,RHFC,RRO,RZGR,RCNEXX,RLONG,RROR,RZR,
     -    RCNEXY,RMGT,RROTT
      INTEGER*2 JDATA(32)
      INTEGER*4 LAN,INU_ISI_ISWAP,IL
      CHARACTER CMODE*(*)
      CHARACTER CMISC(60)*6,CDATA(64)*1,CSUAN*10,CSUAP*10
      CHARACTER CAS(32)*4,CLMPC(8)*1,CMP*7,CISMC(2)*4,CTEMP*6
      INCLUDE   'HCOM_DEF.FOR'
      EQUIVALENCE (INUDATA(2),JDATA(1)) !The INU message
      EQUIVALENCE (INUDATA(2),CDATA(1)) 
      EQUIVALENCE (INUDATA(34),CMISC(1)) !Miscellaneous parameters
      CHARACTER*79 CM01(22),CM06(22),CM07(21),CM09(22),CM10(21),CM11(20),
     -    CM12(5),CM13(20),CM14(18),CM99(22),CE*3
      DATA CISMC /'GCSM',' SCS'/
      DATA CAS /'Init','Att','A/H','8.0','7.2','6.4','5.6','4.8','4.0','3.2',
     -    '2.4','1.6','0.8','0.7','0.6','0.5','0.4','0.35','0.30','0.25','0.20',
     -    '0.15','0.10','0.05',8*' '/
      DATA CLMPC /'-','A','B','C','D','E','F',' '/

      DATA CM01 /
     -'                        Honeywell H-423 INU Message I01',
     -' ',
     -'I01-01 INU Mode Word',
     -'I01-02 Time Tag                                                 s',
     -'I01-03 Velocity X                                               fps',
     -'I01-05 Velocity Y                                               fps',
     -'I01-07 Velocity Z                                               fps',
     -'I01-09 Platform Azimuth                                         deg',
     -'I01-10 Roll/Pitch                                               deg',
     -'I01-12 Present True Heading                                     deg',
     -'I01-13 Present Magnetic Heading                                 deg',
     -'I01-14 X Acceleration                                           ft/s/s',
     -'I01-15 Y Acceleration                                           ft/s/s',
     -'I01-16 Z Acceleration                                           ft/s/s',
     -'I01-17 CNEXX',
     -'I01-19 CNEXY',
     -'I01-21 CNEXZ',
     -'I01-23 Inertial Altitude/Longitude                 ft           deg',
     -'I01-26 Great Circle Steering Error                              deg',
     -'I01-27 X-Axis/Y-Axis Residual Tilt                              arc sec',
     -'I01-29 Mode Word II',
     -'I01-30 Roll/Pitch/Yaw Rate                                      deg/s'/

      DATA CM06 /
     -'                        Honeywell H-423 INU Message I06',
     -' ',
     -'I06-01 INU Control Word 1',
     -'I06-02 Time Tag                                                      s',
     -'I06-03 Velocity X                                                    fps',
     -'I06-05 Velocity Y                                                    fps',
     -'I06-07 Velocity Z                                                    fps',
     -'I06-09 Platform Azimuth                                              deg',
     -'I06-10 Roll                                                          deg',
     -'I06-11 Pitch                                                         deg',
     -'I06-12 Present True Heading                                          deg',
     -'I06-13 Present Magnetic Heading                                      deg',
     -'I06-14 GC Steering Error/Computed Course Dev                         deg',
     -'I06-16 Time/Distance to Steerpoint                      s            nmi',
     -'I06-18 Rel Bearing to Steerpoint/nth Way/Markpnt                     deg',
     -'I06-20 Time/Distance to nth Waypoint/Markpoint          s            nmi',
     -'I06-22 INU Control Word 3',
     -'I06-23 nth Waypoint/Markpoint Lat/Long                                      deg',
     -'I06-27 Selected Mag Course to Steerpnt/Mag Course                    deg',
     -'I06-29 Magnetic Heading to nth Waypoint/Markpnt                      deg',
     -'I06-30 True Air Speed                                                kts',
     -'I06-31 Present Magnetic Ground Track/Drift Angle                     deg'/

      DATA CM07 /
     -'                        Honeywell H-423 INU Message I07',
     -' ',
     -'I07-01 nth Waypoint/Markpoint Spheroid/UTM Grid Zone/Area',
     -'I07-04 nth Waypoint/Markpoint UTM Easting/Northing                          m',
     -'I07-06 Present Position Latitude                                            deg',
     -'I07-08 Present Position Longitude                                           deg',
     -'I07-10 Present Position Spheroid/UTM Grid Zone/Area',
     -'I07-13 Present Position UTM Easting/Northing                                m',
     -'I07-15 Entered True/Magnetic Heading                                        deg',
     -'I07-17 Entered Magnetic Variation/Steering Mode                             deg',
     -'I07-18 Computed Magnetic Variation                                          deg',
     -'I07-19 Align Time and Status                                   s       ',
     -'I07-20 Wind Direction                                                       deg',
     -'I07-21 Last Mark Point Code/Wind Velocity                                   kts',
     -'I07-22 Present Ground Speed                                                 kts',
     -'I07-23 Present True Ground Track                                            deg',
     -'I07-24 Predicted Ground Speed                                               kts',
     -'I07-25 Present Convergence Factor in Use (Grid Mode)',
     -'I07-26 Present/Desired Grid Heading                                         deg',
     -'I07-28 Position Error North/East                                            nmi',
     -'I07-30 INU Miscellaneous Data'/

      DATA CM09 /
     -'                        Honeywell H-423 INU Message I09',
     -' ',
     -'I09-01 INU Mode Word',
     -'I09-02 Time Tag                                          s',
     -'I09-03 X Velocity                                        ft/s',
     -'I09-05 Y Velocity                                        ft/s',
     -'I09-07 Z Velocity                                        ft/s',
     -'I09-09 Platform Azimuth                                  deg',
     -'I09-10 Roll                                              deg',
     -'I09-11 Pitch                                             deg',
     -'I09-12 Roll Rate (p)                                     deg/s',
     -'I09-13 Pitch Rate (q)                                    deg/s',
     -'I09-14 Yaw Rate (r)                                      deg/s',
     -'I09-15 Longitudinal Acceleration (nx)                    ft/s/s',
     -'I09-16 Lateral Acceleration (ny)                         ft/s/s',
     -'I09-17 Normal Acceleration (nz)                          ft/s/s',
     -'I09-18 Platform Azimuth Time Tag                         s',
     -'I09-19 Roll Time Tag                                     s',
     -'I09-20 Pitch Time Tag                                    s',
     -'I09-21 Roll Axis Angular Acceleration                    deg/s/s',
     -'I09-22 Pitch Axis Angular Acceleration                   deg/s/s',
     -'I09-23 Yaw Axis Angular Acceleration                     deg/s/s'/

      DATA CM10 /
     -'                        Honeywell H-423 INU Message I10',
     -' ',
     -'I10-01 Time Tag                                       s',
     -'I10-02 X Axis Residual Tilt                           arc sec',
     -'I10-03 Y Axis Residual Tilt                           arc sec',
     -'I10-04 Z Axis Residual Tilt                           arc sec',
     -'I10-05 CNExx',
     -'I10-07 CNExy',
     -'I10-09 CNExz',
     -'I10-11 CNEyx',
     -'I10-13 CNEyy',
     -'I10-15 CNEyz',
     -'I10-17 CSNxx',
     -'I10-18 CSNxz',
     -'I10-19 CSNxz',
     -'I10-20 CSNyx',
     -'I10-21 CSNyy',
     -'I10-22 CSNyz',
     -'I10-23 Altitude Feedback Constant                     /s',
     -'I10-25 Velocity Feedback Constant                     /s/s',
     -'I10-27 Acceleration Feedback Constant                 /s/s/s'/

      DATA CM11 /
     -'                        Honeywell H-423 INU Message I11',
     -' ',
     -'I11-02 Accelerometer Bias Uncertainty                               mg',
     -'I11-03 (X,Y,Z) Accelerometer Scale Factor Uncertainty               %',
     -'I11-04 X Gyro Bias Uncertainty                                      deg/hr',
     -'I11-05 Y Gyro Bias Uncertainty                                      deg/hr',
     -'I11-06 Z Gyro Bias Uncertainty                                      deg/hr',
     -'I11-07 (X,Y,Z) Gyro Scale Factor Uncertainty                        %',
     -'I11-08 X Gyro Randomness                                            deg/hr',
     -'I11-09 X Gyro Correlation Time                                      min',
     -'I11-10 Y Gyro Randomness                                            deg/hr',
     -'I11-11 Y Gyro Correlation Time                                      min',
     -'I11-12 Z Gyro Randomness                                            deg/hr',
     -'I11-13 Z Gyro Correlation Time                                      min',
     -'I11-14 (X,Y,Z) Accelerometer Randomness                             mg',
     -'I11-15 (X,Y,Z) Accelerometer Randommess Correlation Time            min',
     -'I11-19 (X,Y,Z) Platform Tilt (Ground Align) Uncertainty             arc sec',
     -'I11-20 (X,Y,Z) Platform Azimuth (Ground Align) Uncertainty          arc sec',
     -'I11-21 (X,Y,Z) Accelerometer Nonorthogonality Uncertainty           arc sec',
     -'I11-24 (X,Y,Z) Gyro Misalignment Uncertainty                        arc sec'/

      DATA CM12 /
     -'                        Honeywell H-423 INU Message I12',
     -' ',
     -'I12-02 Accelerometer Scale Factor Asymmetry             ppm',
     -'I12-14 Accelerometer Warmup                             mg',
     -'I12-15 Accelerometer Warmup Correlation Time            min'/

      DATA CM13 /
     -'                        Honeywell H-423 INU Message I13',
     -' ',
     -'I13-07 Position of Specific Force Origin, Xr           inches',
     -'I13-08 Position of Specific Force Origin, Yr           inches',
     -'I13-09 Position of Specific Force Origin, Zr           inches',
     -'I13-10 Gyro Orientation about Zb, Gamma Z0             deg',
     -'I13-11 Gyro Orientation about Yb, Gamma Y0             deg',
     -'I13-12 Gyro Orientation about Xb, Gamma X0             deg',
     -'I13-13 Accl Orientation about Zb, Gamma Za             deg',
     -'I13-14 Accl Orientation about Yb, Gamma Ya             deg',
     -'I13-15 Accl Orientation about Xb, Gamma Xa             deg',
     -'I13-16 Terminal Address',
     -'I13-17 Orient Status Word',
     -'I13-18 Roll Boresight Correction Angle                 deg',
     -'I13-19 Pitch Boresight Correction Angle                deg',
     -'I13-20 Yaw Boresight Correction Angle                  deg',
     -'I13-21 Position of INU CG Along Xr                     inches',
     -'I13-22 Position of INU CG Along Yr                     inches',
     -'I13-23 Position of INU CG Along Zr                     inches',
     -'I13-24 Weight of INU                                   lbs'/

      DATA CM14 /
     -'                        Honeywell H-423 INU Message I14',
     -' ',
     -'I14-01 INU Status',
     -'I14-02 INU Vendor',
     -'I14-03 Mode Word',
     -'I14-04 SRU Failure Indicator',
     -' ',
     -'I14-11 Current LBIT Word  1             I14-22 History LBIT Word  1',
     -'I14-12 Current LBIT Word  2             I14-23 History LBIT Word  2',
     -'I14-13 Current LBIT Word  3             I14-24 History LBIT Word  3',
     -'I14-14 Current LBIT Word  4             I14-25 History LBIT Word  4',
     -'I14-15 Current LBIT Word  5             I14-26 History LBIT Word  5',
     -'I14-16 Current LBIT Word  6             I14-27 History LBIT Word  6',
     -'I14-17 Current LBIT Word  7             I14-28 History LBIT Word  7',
     -'I14-18 Current LBIT Word  8             I14-29 History LBIT Word  8',
     -'I14-19 Current LBIT Word  9             I14-30 History LBIT Word  9',
     -'I14-20 Current LBIT Word 10             I14-31 History LBIT Word 10',
     -'I14-21 Current LBIT Word 11             I14-32 History LBIT Word 11'/

      DATA CM99 /
     -'                     Honeywell H-423 INU Misc Parameters',
     -' ',
     -'000 Manuf ID  cccccc       020 Cumu CEP  cccccc nmi/h 060 Term ID   cccccc',
     -'001 BITE Hist cccccc       021 Last CEP  cccccc nmi/h 061 Baroi alt snnnnn ft',  
     -'002 BITE Stat cccccc       022 RER blat   snn nn.n d  062 Aligns    cccccc',  
     -'003 Init lati  snn nn.n d  023 RER blng  snnn nn.n d  063 Align Tim cccccc min', 
     -'004 Init long snnn nn.n d  024 RER ilat   snn nn.n d  064 Navs      cccccc', 
     -'005 Pres lati  snn nn.n d  025 RER ilng  snnn nn.n d  065 Nav Tim   cccccc min', 
     -'006 Pres long snnn nn.n d  026 RER GC1  ccc ccc nmi/h', 
     -'007 INU ID    cccccc       027 RER GC2  ccc ccc nmi/h 106 Insrt M/H snnn nn.n d',
     -'008 MUX/Time  nnnn.n min   028 RER GC3  ccc ccc nmi/h 111 Wnd Angle   snnn.nn d',
     -'009 DP N U1   snnn.n nm    029 RER GC4  ccc ccc nmi/h 112 Press Alt snnnnn.n ft',
     -'010 DP E U1   snnn.n nm    030 RER GC5  ccc ccc nmi/h 113 Real Time cccccc min',
     -'011 NTim U1   nnnn.n min   031 RER GC6  ccc ccc nmi/h 114 Pst Algns cccccc',
     -'012 DP N U2   snnn.n nm    032 RER GC7  ccc ccc nmi/h 116 Pst Navs  cccccc',
     -'013 DP E U2   snnn.n nm    033 RER GC8  ccc ccc nmi/h 118 Prv Algnt cccccc min',
     -'014 NTim U2   nnnn.n min   034 INU cycls cccccc       128 Strd HdgF cccccc',
     -'015 DP N U3   snnn.n nm    035 Last RER  cccccc       132 Malf Seve cccccc',
     -'016 DP E U3   snnn.n nm    036 Orient st cccccc       133 Batt Stat cccccc',
     -'017 NTim U3   nnnn.n min   037 Roll bore snn nn nn d  161 Auto-Cal  cccccc',
     -'018 Manl TAS  nnnn.n kts   038 Pitc bore snn nn nn d  177 Serial No cccccc',
     -'019 Miss RER ccc ccc nmi/h 039 Yaw bore  snn nn nn d  178 Config No cccccc'/

      CE=CHAR(0)//CHAR(27)//'['
      IF(CMODE.EQ.'FULL'.OR.CMODE(1:4).EQ.'DATA') THEN
        IF(CMODE.EQ.'DATAZ') THEN
          DO I=1,32
            JDATA(I)=0
          END DO
        END IF
        IF(IM.EQ.1) THEN               !Decode message I01
          IMODE=JZEXT(JDATA(1))
          RTT=FLOAT(JZEXT(JDATA(2)))*64E-6
          RVX=FLOAT(INU_ISI_ISWAP(JDATA(3)))*3.81470E-6
          RVY=FLOAT(INU_ISI_ISWAP(JDATA(5)))*3.81470E-6
          RVZ1=FLOAT(INU_ISI_ISWAP(JDATA(7)))*3.81470E-6
          RPA=FLOAT(JDATA(9))*3.05176E-5*180.
          RRO=FLOAT(JDATA(10))*3.05176E-5*180.
          RPI=FLOAT(JDATA(11))*3.05176E-5*180.
          RTH=FLOAT(JDATA(12))*3.05176E-5*180.
          RMH=FLOAT(JDATA(13))*3.05176E-5*180.
          RAX=FLOAT(JDATA(14))*0.03125
          RAY=FLOAT(JDATA(15))*0.03125
          RAZ=FLOAT(JDATA(16))*0.03125
          RCNEXX=FLOAT(INU_ISI_ISWAP(JDATA(17)))*9.31323E-10
          RCNEXY=FLOAT(INU_ISI_ISWAP(JDATA(19)))*9.31323E-10
          RCNEXZ=FLOAT(INU_ISI_ISWAP(JDATA(21)))*9.31323E-10
          RLONG=FLOAT(INU_ISI_ISWAP(JDATA(23)))*4.65661E-10*180.
          IALT=NINT(FLOAT(JDATA(25))*4.)
          RGCSE=FLOAT(JDATA(26))*3.05175E-5*180.
          IXTLT=JDATA(27)
          IYTLT=JDATA(28)
          IMODE2=JZEXT(JDATA(29))
          RROR=FLOAT(JDATA(30))*1.22070E-4*180.
          RPIR=FLOAT(JDATA(31))*1.22070E-4*180.
          RYAR=FLOAT(JDATA(32))*1.22070E-4*180.
          IF(CMODE.EQ.'FULL') THEN
            WRITE(CM01(3)(48:52),11,IOSTAT=IOS) ' ',IMODE
            WRITE(CM01(4)(48:56),12,IOSTAT=IOS) ' ',RTT
            WRITE(CM01(5)(44:56),13,IOSTAT=IOS) ' ',RVX
            WRITE(CM01(6)(44:56),13,IOSTAT=IOS) ' ',RVY
            WRITE(CM01(7)(44:56),13,IOSTAT=IOS) ' ',RVZ1
            WRITE(CM01(8)(45:53),14,IOSTAT=IOS) ' ',RPA
            WRITE(CM01(9)(35:53),15,IOSTAT=IOS) ' ',RRO,RPI
            WRITE(CM01(10)(45:53),14,IOSTAT=IOS) ' ',RTH
            WRITE(CM01(11)(45:53),14,IOSTAT=IOS) ' ',RMH
            WRITE(CM01(12)(44:55),16,IOSTAT=IOS) ' ',RAX
            WRITE(CM01(13)(44:55),16,IOSTAT=IOS) ' ',RAY
            WRITE(CM01(14)(44:55),16,IOSTAT=IOS) ' ',RAZ
            WRITE(CM01(15)(47:60),17,IOSTAT=IOS) ' ',RCNEXX
            WRITE(CM01(16)(47:60),17,IOSTAT=IOS) ' ',RCNEXY
            WRITE(CM01(17)(47:60),17,IOSTAT=IOS) ' ',RCNEXZ
            WRITE(CM01(18)(35:57),45,IOSTAT=IOS) ' ',IALT,RLONG
            WRITE(CM01(19)(45:53),14,IOSTAT=IOS) ' ',RGCSE
            WRITE(CM01(20)(37:53),19,IOSTAT=IOS) ' ',IXTLT,IYTLT
            WRITE(CM01(21)(48:52),11,IOSTAT=IOS) ' ',IMODE2
            WRITE(CM01(22)(35:63),20,IOSTAT=IOS) ' ',RROR,RPIR,RYAR
            DO I=1,22
              WRITE(IL,10,IOSTAT=IOS) CM01(I)(1:LAN(CM01(I)))
            END DO
          END IF
        ELSE IF(IM.EQ.6) THEN          !Decode message I06
          ICTL1=JZEXT(JDATA(1))
          RTT=FLOAT(JZEXT(JDATA(2)))*64E-6
          RVX=FLOAT(INU_ISI_ISWAP(JDATA(3)))*3.81470E-6
          RVY=FLOAT(INU_ISI_ISWAP(JDATA(5)))*3.81470E-6
          RVZ1=FLOAT(INU_ISI_ISWAP(JDATA(7)))*3.81470E-6
          RPA=FLOAT(JDATA(9))*3.05176E-5*180.
          RRO=FLOAT(JDATA(10))*3.05176E-5*180.
          RPI=FLOAT(JDATA(11))*3.05176E-5*180.
          RTH=FLOAT(JDATA(12))*3.05176E-5*180.
          RMH=FLOAT(JDATA(13))*3.05176E-5*180.
          RGCSE=FLOAT(JDATA(14))*3.05176E-5*180.
          RCCD=FLOAT(JDATA(15))*3.05176E-5*180.
          ITTS=JZEXT(JDATA(16))
          RDTS=FLOAT(JZEXT(JDATA(17)))*0.1
          RBTS=FLOAT(JDATA(18))*3.05176E-5*180.
          RBNW=FLOAT(JDATA(19))*3.05176E-5*180.
          ITTN=JZEXT(JDATA(20))
          RDTN=FLOAT(JZEXT(JDATA(21)))*0.1
          ICTL3=JZEXT(JDATA(22))
          RNLAT=FLOAT(INU_ISI_ISWAP(JDATA(23)))*4.65661E-10*180.
          RNLNG=FLOAT(INU_ISI_ISWAP(JDATA(25)))*4.65661E-10*180.
          RSMCS=FLOAT(JDATA(27))*3.05176E-5*180.
          RSMC=FLOAT(JDATA(28))*3.05176E-5*180.
          RMHN=FLOAT(JDATA(29))*3.05176E-5*180.
          RTAS1=FLOAT(JDATA(30))*0.125
          RMGT=FLOAT(JDATA(31))*3.05176E-5*180.
          RDA=FLOAT(JDATA(32))*3.05176E-5*180.
          IF(CMODE.EQ.'FULL') THEN
            WRITE(CM06(3)(53:57),11,IOSTAT=IOS) ' ',ICTL1
            WRITE(CM06(4)(53:61),12,IOSTAT=IOS) ' ',RTT
            WRITE(CM06(5)(49:61),13,IOSTAT=IOS) ' ',RVX
            WRITE(CM06(6)(49:61),13,IOSTAT=IOS) ' ',RVY
            WRITE(CM06(7)(49:61),13,IOSTAT=IOS) ' ',RVZ1
            WRITE(CM06(8)(50:58),14,IOSTAT=IOS) ' ',RPA
            WRITE(CM06(9)(50:58),14,IOSTAT=IOS) ' ',RRO
            WRITE(CM06(10)(50:58),14,IOSTAT=IOS) ' ',RPI
            WRITE(CM06(11)(50:58),14,IOSTAT=IOS) ' ',RTH
            WRITE(CM06(12)(50:58),14,IOSTAT=IOS) ' ',RMH
            WRITE(CM06(13)(50:68),15,IOSTAT=IOS) ' ',RGCSE,RCCD
            WRITE(CM06(14)(49:68),21,IOSTAT=IOS) ' ',ITTS,RDTS
            WRITE(CM06(15)(50:68),15,IOSTAT=IOS) ' ',RBTS,RBNW
            WRITE(CM06(16)(49:68),21,IOSTAT=IOS) ' ',ITTN,RDTN
            WRITE(CM06(17)(53:57),11,IOSTAT=IOS) ' ',ICTL3
            WRITE(CM06(18)(49:75),22,IOSTAT=IOS) ' ',RNLAT,RNLNG
            WRITE(CM06(19)(50:68),15,IOSTAT=IOS) ' ',RSMCS,RSMC
            WRITE(CM06(20)(50:58),14,IOSTAT=IOS) ' ',RMHN
            WRITE(CM06(21)(49:58),23,IOSTAT=IOS) ' ',RTAS1
            WRITE(CM06(22)(50:68),15,IOSTAT=IOS) ' ',RMGT,RDA
            DO I=1,22
              WRITE(IL,10,IOSTAT=IOS) CM06(I)(1:LAN(CM06(I)))
            END DO
          END IF
        ELSE IF(IM.EQ.7) THEN          !Decode message I07
          IF(JDATA(1).EQ.0) JDATA(1)='2020'X
          IF(JDATA(2).EQ.0) JDATA(2)='2020'X
          IF(JDATA(3).EQ.0) JDATA(3)='2020'X
          CSUAN=CDATA(2)//'  '//CDATA(1)//CDATA(4)//CDATA(3)//'  '//CDATA(6)//
     -        CDATA(5)
          INWUE=JZEXT(JDATA(4))*2
          INWUN=JZEXT(JDATA(5))*2
          RPPLAT=FLOAT(INU_ISI_ISWAP(JDATA(6)))*4.65661E-10*180.
          RPPLNG=FLOAT(INU_ISI_ISWAP(JDATA(8)))*4.65661E-10*180.
          IF(JDATA(10).EQ.0) JDATA(10)='2020'X
          IF(JDATA(11).EQ.0) JDATA(11)='2020'X
          IF(JDATA(12).EQ.0) JDATA(12)='2020'X
          CSUAP=CDATA(20)//'  '//CDATA(19)//CDATA(22)//CDATA(21)//'  '//
     -        CDATA(24)//CDATA(23)
          IPPUE=JZEXT(JDATA(13))*2
          IPPUN=JZEXT(JDATA(14))*2
          RETH=FLOAT(JDATA(15))*3.05176E-5*180.
          REMH=FLOAT(JDATA(16))*3.05176E-5*180.
          ISMC=(JDATA(17).AND.1)+1
          REMV=FLOAT(JDATA(17)/2)*6.10352E-5*180.
          RCMV=FLOAT(JDATA(18))*3.05176E-5*180.
          IAT=(JDATA(19).AND.'3FF'X)*6
          IAS=((JDATA(19).AND.'7FFF'X)/1024)+1
          RWD=FLOAT(JDATA(20))*3.05176E-5*180.
          RWV=FLOAT(JZEXT(JDATA(21))/8)*0.0625
          ILMPC=(JDATA(21).AND.7)+1
          RPGS=FLOAT(JZEXT(JDATA(22)))*0.0625
          RPTGT=FLOAT(JDATA(23))*3.05175E-5*180.
          RPDGS=FLOAT(JZEXT(JDATA(24)))*0.0625
          RPCF=FLOAT(JZEXT(JDATA(25)))*3.05176E-5
          RPGH=FLOAT(JDATA(26))*3.05176E-5*180.
          RDGH=FLOAT(JDATA(27))*3.05176E-5*180.
          RPEN=FLOAT(JDATA(28))*0.01
          RPEE=FLOAT(JDATA(29))*0.01
          IF(JDATA(30).EQ.0) JDATA(30)='2020'X
          IF(JDATA(31).EQ.0) JDATA(30)='2020'X
          IF(JDATA(32).EQ.0) JDATA(30)='2020'X
          CMP=' '//CDATA(60)//CDATA(59)//CDATA(62)//CDATA(61)//CDATA(64)
     -        //CDATA(63)
          IF(ICHAR(CMP(2:2)).GT.127) THEN
            CMP(1:1)='-'
            CMP(2:2)=CHAR(ICHAR(CMP(2:2))-128)
          END IF
          DO I=1,7
            IF(ICHAR(CMP(I:I)).EQ.0) CMP(I:I)=' '
          END DO
          IF(CMODE.EQ.'FULL') THEN
            WRITE(CM07(3)(58:68),24,IOSTAT=IOS) ' '//CSUAN
            WRITE(CM07(4)(57:75),25,IOSTAT=IOS) ' ',INWUE,INWUN
            WRITE(CM07(5)(58:70),26,IOSTAT=IOS) ' ',RPPLAT
            WRITE(CM07(6)(58:70),26,IOSTAT=IOS) ' ',RPPLNG
            WRITE(CM07(7)(58:68),24,IOSTAT=IOS) ' '//CSUAP
            WRITE(CM07(8)(57:75),25,IOSTAT=IOS) ' ',IPPUE,IPPUN
            WRITE(CM07(9)(58:75),43,IOSTAT=IOS) ' ',RETH,REMH
            WRITE(CM07(10)(52:66),14,IOSTAT=IOS) ' '//CISMC(ISMC)//'  ',REMV
            WRITE(CM07(11)(58:66),14,IOSTAT=IOS) ' ',RCMV
            WRITE(CM07(12)(58:71),27,IOSTAT=IOS) ' ',IAT,CAS(IAS)
            WRITE(CM07(13)(58:66),14,IOSTAT=IOS) ' ',RWD
            WRITE(CM07(14)(55:67),28,IOSTAT=IOS) ' '//CLMPC(ILMPC)//'   ',RWV
            WRITE(CM07(15)(59:67),28,IOSTAT=IOS) ' ',RPGS
            WRITE(CM07(16)(58:66),14,IOSTAT=IOS) ' ',RPTGT
            WRITE(CM07(17)(59:67),28,IOSTAT=IOS) ' ',RPDGS
            WRITE(CM07(18)(61:68),29,IOSTAT=IOS) ' ',RPCF
            WRITE(CM07(19)(58:75),43,IOSTAT=IOS) ' ',RPGH,RDGH
            WRITE(CM07(20)(58:74),30,IOSTAT=IOS) ' ',RPEN,RPEE
            WRITE(CM07(21)(58:65),24,IOSTAT=IOS) ' '//CMP
            DO I=1,21
              WRITE(IL,10,IOSTAT=IOS) CM07(I)(1:LAN(CM07(I)))
            END DO
          END IF
        ELSE IF(IM.EQ.9) THEN          !Decode message I09
          IMODE1=JZEXT(JDATA(1))
          RTT=FLOAT(JZEXT(JDATA(2)))*64E-6
          RVX=FLOAT(INU_ISI_ISWAP(JDATA(3)))*3.81470E-6
          RVY=FLOAT(INU_ISI_ISWAP(JDATA(5)))*3.81470E-6
          RVZ1=FLOAT(INU_ISI_ISWAP(JDATA(7)))*3.81470E-6
          RPA=FLOAT(JDATA(9))*3.05176E-5*180.
          RRO=FLOAT(JDATA(10))*3.05176E-5*180.
          RPI=FLOAT(JDATA(11))*3.05176E-5*180.
          RROR=FLOAT(JDATA(12))*1.22070E-4*180.
          RPIR=FLOAT(JDATA(13))*1.22070E-4*180.
          RYAR=FLOAT(JDATA(14))*1.22070E-4*180.
          RNX=FLOAT(JDATA(15))*0.03125
          RNY=FLOAT(JDATA(16))*0.03125
          RNZ=FLOAT(JDATA(17))*0.03125
          RPATT=FLOAT(JZEXT(JDATA(18)))*64E-6
          RROTT=FLOAT(JZEXT(JDATA(19)))*64E-6
          RPITT=FLOAT(JZEXT(JDATA(20)))*64E-6
          RRAAA=FLOAT(JDATA(21))*2.44141E-4*180.
          RPAAA=FLOAT(JDATA(22))*2.44141E-4*180.
          RYAAA=FLOAT(JDATA(23))*2.44141E-4*180.
          IF(CMODE.EQ.'FULL') THEN
            WRITE(CM09(3)(48:52),11,IOSTAT=IOS) ' ',IMODE1
            WRITE(CM09(4)(48:56),12,IOSTAT=IOS) ' ',RTT
            WRITE(CM09(5)(44:56),13,IOSTAT=IOS) ' ',RVX
            WRITE(CM09(6)(44:56),13,IOSTAT=IOS) ' ',RVY
            WRITE(CM09(7)(44:56),13,IOSTAT=IOS) ' ',RVZ1
            WRITE(CM09(8)(45:54),31,IOSTAT=IOS) ' ',RPA
            WRITE(CM09(9)(45:54),31,IOSTAT=IOS) ' ',RRO
            WRITE(CM09(10)(45:54),31,IOSTAT=IOS) ' ',RPI
            WRITE(CM09(11)(45:54),31,IOSTAT=IOS) ' ',RROR
            WRITE(CM09(12)(45:54),31,IOSTAT=IOS) ' ',RPIR
            WRITE(CM09(13)(45:54),31,IOSTAT=IOS) ' ',RYAR
            WRITE(CM09(14)(45:55),32,IOSTAT=IOS) ' ',RNX
            WRITE(CM09(15)(45:55),32,IOSTAT=IOS) ' ',RNY
            WRITE(CM09(16)(45:55),32,IOSTAT=IOS) ' ',RNZ
            WRITE(CM09(17)(48:56),12,IOSTAT=IOS) ' ',RPATT
            WRITE(CM09(18)(48:56),12,IOSTAT=IOS) ' ',RROTT
            WRITE(CM09(19)(48:56),12,IOSTAT=IOS) ' ',RPITT
            WRITE(CM09(20)(45:54),31,IOSTAT=IOS) ' ',RRAAA
            WRITE(CM09(21)(45:54),31,IOSTAT=IOS) ' ',RPAAA
            WRITE(CM09(22)(45:54),31,IOSTAT=IOS) ' ',RYAAA
            DO I=1,22
              WRITE(IL,10,IOSTAT=IOS) CM09(I)(1:LAN(CM09(I)))
            END DO
          END IF
        ELSE IF(IM.EQ.10) THEN         !Decode message I10
          RTT=FLOAT(JZEXT(JDATA(1)))*64E-6
          IXART=JDATA(2)
          IYART=JDATA(3)
          IZART=JDATA(4)
          RCNEXX=FLOAT(INU_ISI_ISWAP(JDATA(5)))*9.31323E-10
          RCNEXY=FLOAT(INU_ISI_ISWAP(JDATA(7)))*9.31323E-10
          RCNEXZ=FLOAT(INU_ISI_ISWAP(JDATA(9)))*9.31323E-10
          RCNEYX=FLOAT(INU_ISI_ISWAP(JDATA(11)))*9.31323E-10
          RCNEYY=FLOAT(INU_ISI_ISWAP(JDATA(13)))*9.31323E-10
          RCNEYZ=FLOAT(INU_ISI_ISWAP(JDATA(15)))*9.31323E-10
          RCSNXX=FLOAT(JDATA(17))*6.10352E-5
          RCSNXY=FLOAT(JDATA(18))*6.10352E-5
          RCSNXZ=FLOAT(JDATA(19))*6.10352E-5
          RCSNYX=FLOAT(JDATA(20))*6.10352E-5
          RCSNYY=FLOAT(JDATA(21))*6.10352E-5
          RCSNYZ=FLOAT(JDATA(22))*6.10352E-5
          RHFC=FLOAT(INU_ISI_ISWAP(JDATA(23)))*2.79387E-9
          RVFC=FLOAT(INU_ISI_ISWAP(JDATA(25)))*2.79397E-9
          RAFC=FLOAT(INU_ISI_ISWAP(JDATA(27)))*9.31323E-10
          IF(CMODE.EQ.'FULL') THEN
            WRITE(CM10(3)(41:49),12,IOSTAT=IOS) ' ',RTT
            WRITE(CM10(4)(43:49),33,IOSTAT=IOS) ' ',IXART
            WRITE(CM10(5)(43:49),33,IOSTAT=IOS) ' ',IYART
            WRITE(CM10(6)(43:49),33,IOSTAT=IOS) ' ',IZART
            WRITE(CM10(7)(40:53),17,IOSTAT=IOS) ' ',RCNEXX
            WRITE(CM10(8)(40:53),17,IOSTAT=IOS) ' ',RCNEXY
            WRITE(CM10(9)(40:53),17,IOSTAT=IOS) ' ',RCNEXZ
            WRITE(CM10(10)(40:53),17,IOSTAT=IOS) ' ',RCNEYX
            WRITE(CM10(11)(40:53),17,IOSTAT=IOS) ' ',RCNEYY
            WRITE(CM10(12)(40:53),17,IOSTAT=IOS) ' ',RCNEYZ
            WRITE(CM10(13)(40:48),34,IOSTAT=IOS) ' ',RCSNXX
            WRITE(CM10(14)(40:48),34,IOSTAT=IOS) ' ',RCSNXY
            WRITE(CM10(15)(40:48),34,IOSTAT=IOS) ' ',RCSNXZ
            WRITE(CM10(16)(40:48),34,IOSTAT=IOS) ' ',RCSNYX
            WRITE(CM10(17)(40:48),34,IOSTAT=IOS) ' ',RCSNYY
            WRITE(CM10(18)(40:48),34,IOSTAT=IOS) ' ',RCSNYZ
            WRITE(CM10(19)(40:53),17,IOSTAT=IOS) ' ',RHFC
            WRITE(CM10(20)(40:53),17,IOSTAT=IOS) ' ',RVFC
            WRITE(CM10(21)(40:53),17,IOSTAT=IOS) ' ',RAFC
            DO I=1,21
              WRITE(IL,10,IOSTAT=IOS) CM10(I)(1:LAN(CM10(I)))
            END DO
          END IF
        ELSE IF(IM.EQ.11) THEN         !Decode message I11
          RABU=FLOAT(JZEXT(JDATA(2)))*0.0016
          RASFU=FLOAT(JZEXT(JDATA(3)))*1E-5
          RXGBU=FLOAT(JZEXT(JDATA(4)))*1E-5
          RYGBU=FLOAT(JZEXT(JDATA(5)))*1E-5
          RZGBU=FLOAT(JZEXT(JDATA(6)))*1E-5
          RGSFU=FLOAT(JZEXT(JDATA(7)))*1E-5
          RXGR=FLOAT(JZEXT(JDATA(8)))*1E-6
          RXCT=FLOAT(JZEXT(JDATA(9)))*0.01
          RYGR=FLOAT(JZEXT(JDATA(10)))*1E-6
          RYCT=FLOAT(JZEXT(JDATA(11)))*0.01
          RZGR=FLOAT(JZEXT(JDATA(12)))*1E-6
          RZCT=FLOAT(JZEXT(JDATA(13)))*0.01
          RAR=FLOAT(JZEXT(JDATA(14)))*1E-5
          RARCT=FLOAT(JZEXT(JDATA(15)))*0.01
          IXYPTU=JZEXT(JDATA(19))
          IZPAU=JZEXT(JDATA(20))
          IANU=JZEXT(JDATA(21))
          IGMU=JZEXT(JDATA(24))
          IF(CMODE.EQ.'FULL') THEN
            WRITE(CM11(3)(58:65),35,IOSTAT=IOS) ' ',RABU
            WRITE(CM11(4)(59:66),29,IOSTAT=IOS) ' ',RASFU
            WRITE(CM11(5)(59:66),29,IOSTAT=IOS) ' ',RXGBU
            WRITE(CM11(6)(59:66),29,IOSTAT=IOS) ' ',RYGBU
            WRITE(CM11(7)(59:66),29,IOSTAT=IOS) ' ',RZGBU
            WRITE(CM11(8)(59:66),29,IOSTAT=IOS) ' ',RGSFU
            WRITE(CM11(9)(59:67),36,IOSTAT=IOS) ' ',RXGR
            WRITE(CM11(10)(57:63),37,IOSTAT=IOS) ' ',RXCT
            WRITE(CM11(11)(59:67),36,IOSTAT=IOS) ' ',RYGR
            WRITE(CM11(12)(57:63),37,IOSTAT=IOS) ' ',RYCT
            WRITE(CM11(13)(59:67),36,IOSTAT=IOS) ' ',RZGR
            WRITE(CM11(14)(57:63),37,IOSTAT=IOS) ' ',RZCT
            WRITE(CM11(15)(59:66),29,IOSTAT=IOS) ' ',RAR
            WRITE(CM11(16)(57:63),37,IOSTAT=IOS) ' ',RARCT
            WRITE(CM11(17)(58:63),38,IOSTAT=IOS) ' ',IXYPTU
            WRITE(CM11(18)(58:63),38,IOSTAT=IOS) ' ',IZPAU
            WRITE(CM11(19)(58:63),38,IOSTAT=IOS) ' ',IANU
            WRITE(CM11(20)(58:63),38,IOSTAT=IOS) ' ',IGMU
            DO I=1,20
              WRITE(IL,10,IOSTAT=IOS) CM11(I)(1:LAN(CM11(I)))
            END DO
          END IF
        ELSE IF(IM.EQ.12) THEN         !Decode message I12
          RASFA=FLOAT(JZEXT(JDATA(2)))*0.1
          RAW=FLOAT(JZEXT(JDATA(14)))*0.001
          RAWCT=FLOAT(JZEXT(JDATA(15)))*0.01
          IF(CMODE.EQ.'FULL') THEN
            WRITE(CM12(3)(47:55),14,IOSTAT=IOS) ' ',RASFA
            WRITE(CM12(4)(49:55),40,IOSTAT=IOS) ' ',RAW
            WRITE(CM12(5)(48:55),44,IOSTAT=IOS) ' ',RAWCT
            DO I=1,5
              WRITE(IL,10,IOSTAT=IOS) CM12(I)(1:LAN(CM12(I)))
            END DO
          END IF
        ELSE IF(IM.EQ.13) THEN         !Decode message I13
          RXR=FLOAT(JDATA(7))*0.001
          RYR=FLOAT(JDATA(8))*0.001
          RZR=FLOAT(JDATA(9))*0.001
          RGZO=FLOAT(JDATA(10))*3.05176E-5*180.
          RGYO=FLOAT(JDATA(11))*3.05176E-5*180.
          RGXO=FLOAT(JDATA(12))*3.05176E-5*180.
          RGZA=FLOAT(JDATA(13))*3.05176E-5*180.
          RGYA=FLOAT(JDATA(14))*3.05176E-5*180.
          RGXA=FLOAT(JDATA(15))*3.05176E-5*180.
          ITA=JZEXT(JDATA(16))
          IOSW=JZEXT(JDATA(17))
          RRBCA=FLOAT(JDATA(18))*1.52588E-5*180.
          RPBCA=FLOAT(JDATA(19))*1.52588E-5*180.
          RYBCA=FLOAT(JDATA(20))*1.52588E-5*180.
          RCGX=FLOAT(JDATA(21))*4.88281E-4
          RCGY=FLOAT(JDATA(22))*4.88281E-4
          RCGZ=FLOAT(JDATA(23))*4.88281E-4
          RWT=FLOAT(JZEXT(JDATA(24)))*9.15527E-4
          IF(CMODE.EQ.'FULL') THEN
            WRITE(CM13(3)(45:53),14,IOSTAT=IOS) ' ',RXR
            WRITE(CM13(4)(45:53),14,IOSTAT=IOS) ' ',RYR
            WRITE(CM13(5)(45:53),14,IOSTAT=IOS) ' ',RZR
            WRITE(CM13(6)(45:53),14,IOSTAT=IOS) ' ',RGZO
            WRITE(CM13(7)(45:53),14,IOSTAT=IOS) ' ',RGYO
            WRITE(CM13(8)(45:53),14,IOSTAT=IOS) ' ',RGXO
            WRITE(CM13(9)(45:53),14,IOSTAT=IOS) ' ',RGZA
            WRITE(CM13(10)(45:53),14,IOSTAT=IOS) ' ',RGYA
            WRITE(CM13(11)(45:53),14,IOSTAT=IOS) ' ',RGXA
            WRITE(CM13(12)(48:52),11,IOSTAT=IOS) ' ',ITA
            WRITE(CM13(13)(48:52),11,IOSTAT=IOS) ' ',IOSW
            WRITE(CM13(14)(45:53),14,IOSTAT=IOS) ' ',RRBCA
            WRITE(CM13(15)(45:53),14,IOSTAT=IOS) ' ',RPBCA
            WRITE(CM13(16)(45:53),14,IOSTAT=IOS) ' ',RYBCA
            WRITE(CM13(17)(45:54),31,IOSTAT=IOS) ' ',RCGX
            WRITE(CM13(18)(45:54),31,IOSTAT=IOS) ' ',RCGY
            WRITE(CM13(19)(45:54),31,IOSTAT=IOS) ' ',RCGZ
            WRITE(CM13(20)(45:54),31,IOSTAT=IOS) ' ',RWT
            DO I=1,20
              WRITE(IL,10,IOSTAT=IOS) CM13(I)(1:LAN(CM13(I)))
            END DO
          END IF
        ELSE IF(IM.EQ.14) THEN         !Decode message I14
          ISTS=JZEXT(JDATA(1))
          IVEN=JZEXT(JDATA(2))
          IMDE=JZEXT(JDATA(3))
          ISFI=JZEXT(JDATA(4))
          IF(CMODE.EQ.'FULL') THEN
            WRITE(CM14(3)(29:33),11,IOSTAT=IOS) ' ',ISTS
            WRITE(CM14(4)(29:33),11,IOSTAT=IOS) ' ',IVEN
            WRITE(CM14(5)(29:33),11,IOSTAT=IOS) ' ',IMDE
            WRITE(CM14(6)(29:33),11,IOSTAT=IOS) ' ',ISFI
            DO I=11,21
              WRITE(CM14(I-3)(29:33),11,IOSTAT=IOS) ' ',JDATA(I)
            END DO
            DO I=22,32
              WRITE(CM14(I-14)(69:73),11,IOSTAT=IOS) ' ',JDATA(I)
            END DO
            DO I=1,18
              WRITE(IL,10,IOSTAT=IOS) CM14(I)(1:LAN(CM14(I)))
            END DO
          END IF
        ELSE IF(IM.EQ.99) THEN         !Decode miscellaneous parameters
          CTEMP=CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)
          DO I=1,60
            IF(CMISC(I).EQ.CTEMP) CMISC(I)='      '
          END DO
          DO I=1,20
            CM99(2+I)(15:20)=CMISC(I)
          END DO
          DO I=21,40
            CM99(2+I-20)(42:47)=CMISC(I)
          END DO
          DO I=41,60
            CM99(2+I-40)(69:74)=CMISC(I)
          END DO
          CALL INU_ISI_CSPLT(CMISC(4),' S23 45.6',CM99(6)(15:23))
          CALL INU_ISI_CSPLT(CMISC(5),'S123 45.6',CM99(7)(15:23))
          CALL INU_ISI_CSPLT(CMISC(6),' S23 45.6',CM99(8)(15:23))
          CALL INU_ISI_CSPLT(CMISC(7),'S123 45.6',CM99(9)(15:23))
          CALL INU_ISI_CSPLT(CMISC(9),'1345.6',CM99(11)(15:20))
          CALL INU_ISI_CSPLT(CMISC(10),'S345.6',CM99(12)(15:20))
          CALL INU_ISI_CSPLT(CMISC(11),'S345.6',CM99(13)(15:20))
          CALL INU_ISI_CSPLT(CMISC(12),'2345.6',CM99(14)(15:20))
          CALL INU_ISI_CSPLT(CMISC(13),'S345.6',CM99(15)(15:20))
          CALL INU_ISI_CSPLT(CMISC(14),'S345.6',CM99(16)(15:20))
          CALL INU_ISI_CSPLT(CMISC(15),'2345.6',CM99(17)(15:20))
          CALL INU_ISI_CSPLT(CMISC(16),'S345.6',CM99(18)(15:20))
          CALL INU_ISI_CSPLT(CMISC(17),'S345.6',CM99(19)(15:20))
          CALL INU_ISI_CSPLT(CMISC(18),'2345.6',CM99(20)(15:20))
          CALL INU_ISI_CSPLT(CMISC(19),'2345.6',CM99(21)(15:20))
          CALL INU_ISI_CSPLT(CMISC(20),'123 456',CM99(22)(14:20))
          CALL INU_ISI_CSPLT(CMISC(23),' S23 45.6',CM99(5)(42:51))
          CALL INU_ISI_CSPLT(CMISC(24),'S123 45.6',CM99(6)(42:51))
          CALL INU_ISI_CSPLT(CMISC(25),' S23 45.6',CM99(7)(42:51))
          CALL INU_ISI_CSPLT(CMISC(26),'S123 45.6',CM99(8)(42:51))
          CALL INU_ISI_CSPLT(CMISC(27),'123 456',CM99(9)(41:47))
          CALL INU_ISI_CSPLT(CMISC(28),'123 456',CM99(10)(41:47))
          CALL INU_ISI_CSPLT(CMISC(29),'123 456',CM99(11)(41:47))
          CALL INU_ISI_CSPLT(CMISC(30),'123 456',CM99(12)(41:47))
          CALL INU_ISI_CSPLT(CMISC(31),'123 456',CM99(13)(41:47))
          CALL INU_ISI_CSPLT(CMISC(32),'123 456',CM99(14)(41:47))
          CALL INU_ISI_CSPLT(CMISC(33),'123 456',CM99(15)(41:47))
          CALL INU_ISI_CSPLT(CMISC(34),'123 456',CM99(16)(41:47))
          CALL INU_ISI_CSPLT(CMISC(38),'S12 34 56',CM99(20)(42:51))
          CALL INU_ISI_CSPLT(CMISC(39),'S12 34 56',CM99(21)(42:51))
          CALL INU_ISI_CSPLT(CMISC(40),'S12 34 56',CM99(22)(42:51))
          CALL INU_ISI_CSPLT(CMISC(42),'S23456',CM99(4)(69:74))
          CALL INU_ISI_CSPLT(CMISC(48),'S123 45.6',CM99(10)(69:77))
          CALL INU_ISI_CSPLT(CMISC(49),'S234.56',CM99(11)(71:77))
          CALL INU_ISI_CSPLT(CMISC(50),'S12345.6',CM99(12)(69:76))
          IF(CMODE.EQ.'FULL') THEN
            DO I=1,22
              WRITE(IL,10,IOSTAT=IOS) CM99(I)(1:LAN(CM99(I)))
            END DO
          END IF
        END IF
      END IF
      RETURN
10    FORMAT(X,A)
11    FORMAT(A,Z4.4)
12    FORMAT(A,F8.6)
13    FORMAT(A,F12.6)
14    FORMAT(A,F8.3)
15    FORMAT(A,F8.3,2X,F8.3)
16    FORMAT(A,F11.5)
17    FORMAT(A,F13.10)
18    FORMAT(A,F12.7,' deg  ',I6)
19    FORMAT(A,I6,4X,I6)
20    FORMAT(A,F8.3,2X,F8.3,2X,F8.3)
21    FORMAT(A,I5,' s    ',F8.3)
22    FORMAT(A,F12.6,2X,F12.6)
23    FORMAT(A,F9.3)
24    FORMAT(A)
25    FORMAT(A,I5,4X,I5)
26    FORMAT(A,F12.7)
27    FORMAT(A,I4,' s   ',A)
28    FORMAT(A,F8.4)
29    FORMAT(A,F7.5)
30    FORMAT(A,F7.2,2X,F7.2)
31    FORMAT(A,F9.4)
32    FORMAT(A,F10.5)
33    FORMAT(A,I6)
34    FORMAT(A,F8.5)
35    FORMAT(A,F7.4)
36    FORMAT(A,F8.6)
37    FORMAT(A,F6.2)
38    FORMAT(A,I5)
39    FORMAT(A,F6.1)
40    FORMAT(A,F6.3)
41    FORMAT(A,I2.2,A,Z4.4)
42    FORMAT(I2.2)
43    FORMAT(A,F8.3,X,F8.3)
44    FORMAT(A,F7.3)
45    FORMAT(A,I6,' ft ',F12.7)
      END
C*******************************************************************************
      INTEGER*4 FUNCTION LAN(CSTRING)
C
C Returns the length of a string less any trailing spaces.  If the string is
C all spaces, a value of 1 is returned.
C
C V1.00  01/01/89  W.D.N.JACKSON
C
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
      INTEGER*4 FUNCTION INU_ISI_ISWAP(JDATA)
C
C Returns an I*4 word from the two I*2 words in the JDATA array, but with the
C order of the two words swapped.
C
      IMPLICIT  NONE
      INTEGER*2 JDATA(2),JTEMP(2)
      INTEGER*4 ITEMP
      EQUIVALENCE (ITEMP,JTEMP)
      JTEMP(1)=JDATA(2)
      JTEMP(2)=JDATA(1)
      INU_ISI_ISWAP=ITEMP
      RETURN
      END
C*******************************************************************************
      SUBROUTINE INU_ISI_CSPLT(CMISC,CSTR,CLINE)
C
C Takes a 6 character miscellaneous parameter string and formats it according
C to the specification string given by CSTR, and puts the result in CLINE.
C
      IMPLICIT  NONE
      INTEGER*4 IL,IP,I
      CHARACTER CMISC*6,CSTR*(*),CLINE*(*),CS*1

      CS=' '
      IF(ICHAR(CMISC(1:1)).GT.127) THEN
        CS='-'
        CMISC(1:1)=CHAR(ICHAR(CMISC(1:1))-128)
      END IF
      IL=LEN(CSTR)
      IP=1
      DO I=1,IL
        IF(CSTR(I:I).EQ.'S') THEN
          CLINE(IP:IP)=CS
        ELSE IF(CSTR(I:I).EQ.'1') THEN
          CLINE(IP:IP)=CMISC(1:1)
        ELSE IF(CSTR(I:I).EQ.'2') THEN
          CLINE(IP:IP)=CMISC(2:2)
        ELSE IF(CSTR(I:I).EQ.'3') THEN
          CLINE(IP:IP)=CMISC(3:3)
        ELSE IF(CSTR(I:I).EQ.'4') THEN
          CLINE(IP:IP)=CMISC(4:4)
        ELSE IF(CSTR(I:I).EQ.'5') THEN
          CLINE(IP:IP)=CMISC(5:5)
        ELSE IF(CSTR(I:I).EQ.'6') THEN
          CLINE(IP:IP)=CMISC(6:6)
        ELSE
          CLINE(IP:IP)=CSTR(I:I)
        END IF
        IP=IP+1
      END DO
      RETURN
      END
