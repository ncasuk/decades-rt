C
C ROUTINE          H_CONVERT_GPS PROGRAM FORTVAX
C
C PURPOSE          Reads HORACE optical disks and creates a GPS message file
C
C DESCRIPTION      Reads HORACE optical disks with GPSDAT.DAT files, and
C                  produces GPS message files on ICARUS.  Works with message
C                  files produced by the NAVSTAR XR5 receiver, rather than the
C                  NAVSTAR XR3.
C
C                  The optical disk must be mounted and accessible and the
C                  files specified as input must include device specifications.
C
C                  Note that the program will also create GPS message files
C                  from GPSDAT.DAT files on ICARUS provided the correct files
C                  are specified in chronological order.
C
C                  This program does no checking and works in a very simple
C                  way.  Therefore note the following:
C
C                  a.  The flight number which is required to form the output
C                      message file name is taken from the message 0 message
C                      in the message file.  If the first message in the first
C                      file is not a valid message 0, then a flight number of
C                      XXXX is used.
C
C                  b.  No checking of message times is done, so that if the
C                      input files are specified in the wrong order, or contain
C                      over lapping times, or are from different flights, then
C                      this is not spotted and the output file is simply a
C                      concatenation of the input files.
C
C                  c.  The program assumes that the end of message file is
C                      reached when a FORTRAN read error occurs.  If this
C                      should arise for a reason other than an end of file then
C                      the remaining data in the file will not be transferred.
C
C VERSION          1.00  13-09-93  W.D.N.JACKSON
C
C ARGUMENTS        The programs takes the following from SYS$INPUT:
C
C                     The names of the GPS message files is chronological order.
C
C CHANGES          1.01  10-06-94  D.P.BRIGGS
C                     Code for redundant subroutine GETLETT removed.
C
C*******************************************************************************
      PROGRAM H_CONVERT_GPS
CDEC$ IDENT 'V1.01'
C
C Build with   $FORT H_CONVERT_GPS
C              $LINK H_CONVERT_GPS,GPS1_SUBS
C
      INTEGER*2 IMESS(106)
      CHARACTER CFLTNO*4,CDATE*9,CTIME*8
      LOGICAL ISTATUS
      INTEGER ISTAT,IMESSNUM 
C
      WRITE(6,15) ' '
      WRITE(6,15) 'H_CONVERT_GPS - creates XR5 GPS message files '//
     -    'from HORACE optical disks'
      CFLTNO='XXXX'                    !This is used if no flight no is found
      CALL OPEN_NEXT_FILE(ISTATUS)     !Open first input GPSDAT.DAT file
      IF(ISTATUS) THEN                 !If open successful
        CALL GPS1_MESSF_READ(2,IMESS,ISTAT) !Read first message
        CALL GPS1_ANALYS_MESS(IMESS,IMESSNUM) !Find out type number
        IF(IMESSNUM.EQ.0) CALL GPS1_DECODE_MESS0(IMESS,CFLTNO,CDATE,
     -      CTIME)                     !If message 0 then decode for flight no
      END IF          
      CALL GPS1_MESSF_CREATE(CFLTNO//'_GPS_MESS.DAT;',1,ISTAT) !Create O/P file
      IF(ISTAT.NE.0) THEN              !Can't continue if create fails
        WRITE(6,15) 'Failed to open '//CFLTNO//'_GPS_MESS.DAT;'
        CALL GPS1_MESSF_CLOSE(2,ISTAT)
        CALL EXIT
      END IF
      DO WHILE(ISTATUS)                !For each GPSDAT.DAT file
        DO WHILE(ISTAT.EQ.0)           !For each message in the file
          CALL GPS1_MESSF_WRITE(1,IMESS,ISTAT) !Write it to the new file
          CALL GPS1_MESSF_READ(2,IMESS,ISTAT)  !Read the next message
        END DO
        CALL GPS1_MESSF_CLOSE(2,ISTAT) !Close current file
        CALL OPEN_NEXT_FILE(ISTATUS)   !Open next file (if any)
      END DO
      CALL GPS1_MESSF_CLOSE(1,ISTAT)   !Close new file
C
C Exit
C
      CALL EXIT
15    FORMAT(X,A)
      END
*******************************************************************************
      SUBROUTINE OPEN_NEXT_FILE(ISTATUS)
CDEC$ IDENT 'V1.00'
C
C Asks user for the name of the next data file.  The file is checked for
C existence and readability and then opened with GPS1_MESSF_OPEN.
C
C Returns with ISTATUS true if the file was opened, and false if no more files
C to open or failed to open the specified file. (User entered CTRL/Z or
C command file input ended)
C
      CHARACTER  CREPLY*80
      INTEGER IOS,ISTAT
      LOGICAL ISTATUS,ITEXISTS
      ISTATUS = .TRUE.
      ITEXISTS = .FALSE.      
      DO WHILE(.NOT. ITEXISTS)
        WRITE(6,10) '$Full name of next data file :  '
        READ(5,10,IOSTAT=IOS) CREPLY
        IF(IOS.EQ.-1.OR.CREPLY.EQ.' ') THEN
          ISTATUS=.FALSE.
          ITEXISTS=.TRUE.
        ELSE
          INQUIRE(FILE=CREPLY,EXIST=ITEXISTS)
          IF(.NOT.ITEXISTS) THEN
            WRITE(6,10) ' File not found'
          ELSE
            CALL GPS1_MESSF_OPEN(CREPLY,2,ISTAT)
            IF(ISTAT.NE.0) ISTATUS=.FALSE.
          END IF
        END IF
      END DO
      RETURN
10    FORMAT(A)
      END
