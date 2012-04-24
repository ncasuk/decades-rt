c
c Note that the handling of messages 15 (5) and 31 has not been implemented yet
c pending a version of the XR5 software which supports the interrogation of
c DOPS.
C
C GPS1_SUBS.FOR
C
C This file contains subroutines for working with the NAVSTAR XR5 GPS receiver,
C and all routines use the name GPS1 to distinguish them from the GPS_*
C routines which handle the NAVSTAR XR3 GPS receiver.  Not all routines have
C required code changes, but their names have still been changed to provide
C a consistent and self contained set of routines which work with the XR5.
C
C W.D.N.JACKSON  21/09/93
C
C Changes:
C
C 11/04/94  W.D.N.JACKSON  Now uses Boscombe pan as default
C 18/06/02  W.D.N.JACKSON  Now uses Woodford pan as default
C
C This file contains the following subroutines:
C
C GPS1_ANALYS_MESS        Checks a message for errors and returns message number
C GPS1_CODE_MESS0         Forms the DRS message
C GPS1_CODE_MESS1         Forms the initialization message
C! GPS1_CODE_MESS5         Forms the Test Instrumentation Request message
C GPS1_DECODE_MESS0       Decodes the DRS pseudo-message
C GPS1_DECODE_MESS1       Decodes an initialization message
C! GPS1_DECODE_MESS5       Decodes the Test Instrumentation Request message
C GPS1_DECODE_MESS24      Decodes the GPS Nav State lat/lng message
C GPS1_DECODE_MESS24_BRIEFDecodes the GPS Nav lat/lng message for lat/long/alt
C GPS1_DECODE_MESS18      Decodes the GPS Receiver message
C GPS1_DECODE_MESS20      Decodes the GPS Status message
C! GPS1_DECODE_MESS31      Decodes the GPS Test Instrumentation message
C GPS1_DISP_MESS          Displays GPS messages on VT320
C GPS1_DISP_MESS_VT       Displays GPS messages on VT320
C GPS1_DUMP_MESS          Produces a one line dump of a GPS message
C GPS1_MESS_OPEN          Gets a VMS channel for sending messages to the GPS
C GPS1_MESS_GET           Gets a message from the GPS receiver
C GPS1_MESS_SEND          Sends a message to the GPS receiver
C GPS1_MESS_CLOSE         Frees the GPS VMS channel
C GPS1_MESSF_OPEN         Opens a file containing GPS messages
C GPS1_MESSF_CREATE       Creates a file to hold GPS messages
C GPS1_MESSF_CLOSE        Closes a GPS message file
C GPS1_MESSF_READ         Reads the next message from a GPS message file
C GPS1_MESSF_WRITE        Writes the next message to a GPS message file
C GPS1_DECODE_REAL        Decodes a 4 byte REAL GPS number
C GPS1_DECODE_TIME        Decodes a 8 byte time in GPS format
C GPS1_INITIME_CODE       Codes the GPS initialization time
C GPS1_INITIME_DECODE     Decodes the GPS initialization time
C GPS1_DISTEN             Computes distances east and north from a reference
C GPS1_ILONGWR            Reverses two 16 bit words into one 32 bit word
C GPS1_MVBITS_WL          Moves bits between I*2 and I*4 words
C GPS1_MVBITS_LW          Moves bits between I*4 and I*2 words
C
C*******************************************************************************
C
C ROUTINE          GPS1_ANALYS_MESS SUBROUTINE FORTVAX
C
C PURPOSE          Analyses GPS receiver message for errors and message number
C
C DESCRIPTION      Analyses standard GPS receiver messages, by checking for
C                  consistency in the header words and checking the checksums.
C                  The actual data is not checked.  If there are no errors,
C                  also returns the message number.  The message can be as
C                  received from the receiver, as prepared to be sent to the
C                  receiver, or as read from a GPS message file.  Works with
C                  any GPS message.  Note that the maximum message length is
C                  103 words, ie a 2 word header, 100 data words, and a 1 word
C                  data checksum.  Note that uplink messages differ from 
C                  downlink by not having a sync word, and using the checksum
C                  differently.
C
C                  This subroutine is also used to define the message lengths
C                  which are required by this and other routines.
C
C VERSION          1.00  13-09-93  W.D.N.JACKSON
C
C ARGUMENTS        IMESS(103)  I*2  IN   GPS message, incl header and sync word
C                  IMESSNUM    I*4  OUT  Message number if positive. Else:
C                                          0 DRS header message
C                                         -1 No sync word
C                                         -2 Message no outside range 0-31
C                                         -5 Invalid data checksum
C
C REFERENCES       See NAVSTAR XR5 GPS manual for GPS message format.
C                  DRS message format is in the same format as a downlink GPS 
C                  message but has a message ident of 0, the message length is
C                  13 words and the message consists of 
C                  'cnnn  dd-mmm-yy  hh:mm:ss '.
C
C*******************************************************************************
      SUBROUTINE GPS1_ANALYS_MESS(IMESS,IMESSNUM)
      IMPLICIT  NONE
      INTEGER*2 IMESS(103)             !GPS message     
      INTEGER*4 IMESSNUM               !Returned message number
      INTEGER*4 ILUN(3),ICHAN,IMESSIZ(0:31),IMID,IS,IE,IW,ISUM
      COMMON /GPS1_COM/ ILUN,ICHAN,IMESSIZ
      DATA IMESSIZ /13,                                      !DRS message length
     -              22,27,13,18,-1,0,22,0,5,3,0,20,10,0,0,0, !Uplink messages
     -              30,43,0,33,22,18,28,23,0,0,0,0,97,66,-1/ !Downlink messages

      CALL GPS1_MVBITS_WL(IMESS(1),4,5,IMID,0) !Message number
      IF(IMID.LT.0.OR.IMID.GT.31) THEN !Must be in range 0 to 31
        IMESSNUM=-2
        RETURN
      END IF
      IF(IMID.EQ.0.OR.IMID.GE.17) THEN !Downlink message
        IF(IMESS(2).NE.'81FF'X) THEN
          IMESSNUM=-1
          RETURN
        END IF
        IS=3                           !Start of data in checksum
        IE=IMESSIZ(IMID)+2             !End of data in checksum
      ELSE                             !Uplink message
        IS=1                           !Start of data in checksum
        IE=IMESSIZ(IMID)+1             !End of data in checksum
      END IF
      ISUM=0                           !Compute data checksum
      DO IW=IS,IE
        ISUM=ISUM+JZEXT(IMESS(IW))
      END DO
      ISUM=-ISUM
      ISUM=ISUM.AND.'FFFF'X
      IF(ISUM.NE.JZEXT(IMESS(IE+1))) THEN !Check checksum
        IMESSNUM=-5
        RETURN
      END IF
      IMESSNUM=IMID                    !Return message number
      RETURN
      END
C*******************************************************************************
C
C ROUTINE          GPS1_CODE_MESS0 SUBROUTINE FORTVAX
C
C PURPOSE          Codes the DRS pseudo-message
C
C DESCRIPTION      Codes the DRS pseudo-message.  This message is inserted in
C                  GPS message files to supply information such as flight
C                  number, date and DRS time.  It is in the same format as
C                  other GPS downlink messages.
C
C VERSION          1.00  13-09-93  W.D.N.JACKSON
C
C ARGUMENTS        CFLTNO      C*4  IN   Flight number - cnnn
C                  CDATE       C*9  IN   Flight date - dd-mmm-yy
C                  CTIME       C*8  IN   DRS time of message - hh:mm:ss
C                  IMESS(16)   I*2  OUT  DRS message 0
C
C*******************************************************************************
      SUBROUTINE GPS1_CODE_MESS0(CFLTNO,CDATE,CTIME,IMESS)
      IMPLICIT  NONE
      INTEGER*4 I,IW,ISUM
      INTEGER*2 IMESS(-1:14),ITMESS(13)
      BYTE      BMESS(26)
      CHARACTER CFLTNO*4,CDATE*9,CTIME*8
      EQUIVALENCE (ITMESS,BMESS)

      IMESS(-1)='0000'X                !Message number 0
      IMESS(0)='81FF'X                 !Sync word           
      DO I=1,4                         !Transfer flight no
        BMESS(I)=ICHAR(CFLTNO(I:I))
      END DO
      BMESS(5)=ICHAR(' ')              !Insert two spaces
      BMESS(6)=ICHAR(' ')
      DO I=1,9                         !Transfer flight date
        BMESS(6+I)=ICHAR(CDATE(I:I))
      END DO
      BMESS(16)=ICHAR(' ')             !Insert two spaces
      BMESS(17)=ICHAR(' ')
      DO I=1,8                         !Transfer flight time
        BMESS(17+I)=ICHAR(CTIME(I:I))
      END DO
      BMESS(26)=ICHAR(' ')             !Insert space
      DO I=1,13                        !Transfer to passed array
        IMESS(I)=ITMESS(I)
      END DO
      ISUM=0                           !Compute data checksum
      DO IW=1,13
        ISUM=ISUM+JZEXT(IMESS(IW))
      END DO
      ISUM=-ISUM
      CALL GPS1_MVBITS_LW(ISUM,0,16,IMESS(14),0) !Data checksum 
      RETURN
      END
C*******************************************************************************
C
C ROUTINE          GPS1_CODE_MESS1 SUBROUTINE FORTVAX
C
C PURPOSE          Codes GPS message 1 (Initialization)
C
C DESCRIPTION      Codes the GPS initialization message and returns
C                  the coded uplink message as shown below.  This message
C                  contains mainly optional information which can be sent to 
C                  the receiver to control its operation.  Note that the
C                  initial values of position etc are not used, although they
C                  are stored, unless the 'initialize receiver' bit in the
C                  initialization word is set, and the commanded satellite list
C                  is not used unless the 'use commanded SVs' bit in the
C                  initialization word is set.
C
C VERSION          1.00  13-09-93  W.D.N.JACKSON
C
C ARGUMENTS        IRECID      I*4  IN   Receiver ID
C                  IINIT       I*4  IN   Initialization word. Bit values:
C                                         1 Initialize receiver
C                                         2 Start self tests
C                                         4 Use ionospheric model
C                                        16 Use tropospheric model
C                                        64 Use differential correction
C                                       512 Use commanded SVs
C                                     32768 Use max of 4 trackers
C                  ISV(4)      I*4  IN   Commanded SV list
C                  CINIDAT     C*9  IN   Initial GPS date (dd-mmm-yy)
C                  CINITIM     C*8  IN   Initial GPS time (hh:mm:ss)
C                  RINILAT     R*4  IN   Initial latitude (-90 to +90 deg)
C                  RINILNG     R*4  IN   Initial longitude (-180 to +180 deg)
C                  RINIALT     R*4  IN   Initial altitude (m)
C                  IMESS(24)   I*2  OUT  GPS message 1, incl header words
C
C*******************************************************************************
      SUBROUTINE GPS1_CODE_MESS1(IRECID,IINIT,ISV,CINIDAT,CINITIM,
     -    RINILAT,RINILNG,RINIALT,IMESS)
      IMPLICIT  NONE
      INTEGER*2 IMESS(0:23)
      CHARACTER CINITIM*8,CINIDAT*9
      REAL*4    RINILAT,RINILNG,RINIALT
      INTEGER*4 IRECID,IINIT,ISV(4)
      INTEGER*4 ILAT,ILNG,I,II,IWEEK,ITIME,ISUM,IW
               
      IMESS(1)=IRECID.AND.'0FFF'X      !Stop compiler complaining
      DO I=0,22
        IMESS(I)=0
      END DO
      CALL GPS1_MVBITS_LW(1,0,5,IMESS(0),4) !Message number 1

      II=IINIT.AND.'8257'X
      CALL GPS1_MVBITS_LW(II,0,12,IMESS(2),0) !Store initialization word

      IF(IBITS(IINIT,9,1).NE.0) THEN   !If commanded satellites required
        CALL GPS1_MVBITS_LW(ISV(1),0,8,IMESS(3),0)
        CALL GPS1_MVBITS_LW(ISV(2),0,8,IMESS(3),8)
        CALL GPS1_MVBITS_LW(ISV(3),0,8,IMESS(4),0)
        CALL GPS1_MVBITS_LW(ISV(4),0,8,IMESS(4),8)
      END IF

      CALL GPS1_INITIME_CODE(CINIDAT,CINITIM,IWEEK,ITIME) !Convert to GPS format
      CALL GPS1_MVBITS_LW(IWEEK,0,16,IMESS(8),0) !Initial week number
      CALL GPS1_MVBITS_LW(ITIME,0,16,IMESS(10),0) !Initial GPS time
      CALL GPS1_MVBITS_LW(ITIME,16,16,IMESS(9),0)
      ILAT=NINT(2**23*RINILAT)         !Initial latitude
      CALL GPS1_MVBITS_LW(ILAT,0,16,IMESS(14),0)
      CALL GPS1_MVBITS_LW(ILAT,16,16,IMESS(13),0)
      ILNG=NINT(2**23*RINILNG)         !Initial longitude
      CALL GPS1_MVBITS_LW(ILNG,0,16,IMESS(16),0)
      CALL GPS1_MVBITS_LW(ILNG,16,16,IMESS(15),0)
      IMESS(17)=NINT(RINIALT)          !Initial altitude
      ISUM=0                           !Compute data checksum
      DO IW=0,22
        ISUM=ISUM+JZEXT(IMESS(IW))
      END DO
      ISUM=-ISUM
      CALL GPS1_MVBITS_LW(ISUM,0,16,IMESS(23),0) !Data checksum 
      RETURN
      END
C*******************************************************************************
C
C ROUTINE          GPS1_DECODE_MESS0 SUBROUTINE FORTVAX
C
C PURPOSE          Decodes the DRS pseudo-message
C
C DESCRIPTION      Decodes the DRS pseudo-message.  This message is inserted in
C                  GPS message files to supply information such as flight
C                  number, date and DRS time.  It follows the same format as
C                  GPS downlink messages.
C
C VERSION          1.00  13-09-93  W.D.N.JACKSON
C
C ARGUMENTS        IMESS(16)   I*2  IN   DRS message 0
C                  CFLTNO      C*4  OUT  Flight number - cnnn
C                  CDATE       C*9  OUT  Flight date - dd-mmm-yy
C                  CTIME       C*8  OUT  DRS time of message - hh:mm:ss
C
C*******************************************************************************
      SUBROUTINE GPS1_DECODE_MESS0(IMESS,CFLTNO,CDATE,CTIME)
      IMPLICIT  NONE
      INTEGER*4 I
      INTEGER*2 IMESS(-1:14),ITMESS(13)
      CHARACTER CFLTNO*4,CDATE*9,CTIME*8,CMESS*28
      EQUIVALENCE (ITMESS,CMESS)

      DO I=1,13                        !Transfer to local array
        ITMESS(I)=IMESS(I)
      END DO
      CFLTNO=CMESS(1:4)
      CDATE=CMESS(7:15)
      CTIME=CMESS(18:25)
      RETURN
      END
C*******************************************************************************
C
C ROUTINE          GPS1_DECODE_MESS1 SUBROUTINE FORTVAX
C
C PURPOSE          Decodes GPS message 1 (Initialization)
C
C DESCRIPTION      Decodes the GPS initialization message and returns
C                  the decoded information as shown below.  This message
C                  contains mainly optional information which can be sent to 
C                  the receiver to control its operation.
C
C VERSION          1.00  13-09-93  W.D.N.JACKSON
C
C ARGUMENTS        IMESS(24)   I*2  IN   GPS message 1, incl header word
C                  IRECID      I*4  OUT  Receiver ID
C                  IINIT       I*4  OUT  Initialization word. Bit values:
C                                         1 Initialize receiver
C                                         2 Start self tests
C                                         4 Use ionospheric model
C                                        16 Use tropospheric model
C                                        64 Use differential correction
C                                       512 Use commanded SVs
C                                     32768 Use max of 4 trackers
C                  ISV(4)      I*4  OUT  Commanded SV list
C                  CINIDAT     C*9  IN   Initial GPS date (dd-mmm-yy)
C                  CINITIM     C*8  IN   Initial GPS time (hh:mm:ss)
C                  RINILAT     R*4  OUT  Initial latitude (-90 to +90 deg)
C                  RINILNG     R*4  OUT  Initial longitude (-180 to +180 deg)
C                  RINIALT     R*4  OUT  Initial altitude (m)
C
C*******************************************************************************
      SUBROUTINE GPS1_DECODE_MESS1(IMESS,IRECID,IINIT,ISV,CINIDAT,
     -    CINITIM,RINILAT,RINILNG,RINIALT)
      IMPLICIT  NONE
      INTEGER*2 IMESS(0:23)
      CHARACTER CINITIM*8,CINIDAT*9
      REAL*4    RINILAT,RINILNG,RINIALT
      INTEGER*4 IRECID,IINIT,ISV(4)
      INTEGER*4 IT,ILAT,ILNG,GPS1_ILONGWR,IWEEK,ITIME

      IT=GPS1_ILONGWR(IMESS(9))
      ILAT=GPS1_ILONGWR(IMESS(13))
      ILNG=GPS1_ILONGWR(IMESS(15))
               
      IRECID=IBITS(JZEXT(IMESS(1)),4,12) !Extract receiver id   
      IINIT=IMESS(4).AND.'8257'X       !Get required bits from init word
      ISV(1)=IBITS(JZEXT(IMESS(3)),0,8) !Get command satellite list
      ISV(2)=IBITS(JZEXT(IMESS(3)),8,8)
      ISV(3)=IBITS(JZEXT(IMESS(4)),0,8)
      ISV(4)=IBITS(JZEXT(IMESS(4)),8,8)
      IWEEK=IMESS(8)                   !Decode week and time to date and time
      ITIME=IT
      CALL GPS1_INITIME_DECODE(IWEEK,ITIME,CINIDAT,CINITIM)
      RINILAT=FLOAT(ILAT)/2**23        !Get initial latitude
      RINILNG=FLOAT(ILNG)/2**23        !Get initial longitude
      RINIALT=FLOAT(IMESS(17))         !Get initial altitude
      RETURN
      END
C*******************************************************************************
C
C ROUTINE          GPS1_DECODE_MESS24 SUBROUTINE FORTVAX
C
C PURPOSE          Decodes GPS message 24 (Nav state lat/lng)
C
C DESCRIPTION      Decodes the GPS Navigation state lat/lng message and returns
C                  the decoded information as shown below.  This message
C                  contains position information as lats and longs, together
C                  with other information and status data.
C
C VERSION          1.00  13-09-93  W.D.N.JACKSON
C
C ARGUMENTS        IMESS(26)   I*2  IN   GPS message 24, incl header word
C                  CTIME       C*11 OUT  GPS measurement time - hh:mm:ss.dd
C                  IRECID      I*4  OUT  Receiver ID
C                  IALTHLD     I*4  OUT  Altitude hold mode = 1, else 0
C                  IDATUM      I*4  OUT  0-42, 1 = WGS-84
C                  ISV(4)      I*4  OUT  Space Vehicle IDs
C                  ITRKSTS(4)  I*4  OUT  Tracking status (0-7). Bit values:
C                                         1 Non-coherent track
C                                         2 Diff correction applied
C                                         4 Coherent track
C                  IFOM        I*4  OUT  Figure of merit (0-9). Bit values:
C                                         1 Navigation degraded
C                                         8 Receiver fault (needs repair)
C                  RVEPE       R*4  OUT  Vertical estimated posn. error (m)
C                  RHEPE       R*4  OUT  Horizontal estimated posn. error (m)
C                  RLAT        R*4  OUT  Latitude (-90 to +90 deg)
C                  RLONG       R*4  OUT  Longitude (-180 to +180 deg)
C                  RALTMSL     R*4  OUT  Height above MSL (m)
C                  RALTREF     R*4  OUT  Height above ref elipsoid (m)
C                  RV(3)       R*4  OUT  Velocity East, North, Up (m/s)
C
C*******************************************************************************
      SUBROUTINE GPS1_DECODE_MESS24(IMESS,CTIME,IRECID,IALTHLD,IDATUM,
     -    ISV,ITRKSTS,IFOM,RVEPE,RHEPE,RLAT,RLONG,RALTMSL,RALTREF,
     -    RV)
      IMPLICIT  NONE
      INTEGER*2 IMESS(-1:24)
      CHARACTER CTIME*11
      REAL*4    RVEPE,RHEPE,RLAT,RLONG,RALTMSL,RALTREF,RV(3),RGS,RHDG
      INTEGER*4 IRECID,IALTHLD,IDATUM,ISV(4),ITRKSTS(4),IFOM
      INTEGER*4 GPS1_ILONGWR,ILAT,ILONG,IAMSL,IGS,IVZ,IAREF,IHR,IMN,ISC,
     -    IHU,ISPM,IDAY

      ILAT=GPS1_ILONGWR(IMESS(10))
      ILONG=GPS1_ILONGWR(IMESS(12))
      IAMSL=GPS1_ILONGWR(IMESS(14))
      IAREF=GPS1_ILONGWR(IMESS(16))
      IGS=GPS1_ILONGWR(IMESS(18))
      IVZ=GPS1_ILONGWR(IMESS(21))
      
      CALL GPS1_DECODE_TIME(IMESS(2),IHR,IMN,ISC,IHU,ISPM,IDAY,CTIME)
      IRECID=IBITS(JZEXT(IMESS(6)),4,12)
      IALTHLD=IBITS(JZEXT(IMESS(6)),3,1)
      ISV(1)=IBITS(JZEXT(IMESS(7)),0,5)
      ISV(2)=IBITS(JZEXT(IMESS(7)),8,5)
      ISV(3)=IBITS(JZEXT(IMESS(8)),0,5)
      ISV(4)=IBITS(JZEXT(IMESS(8)),8,5)
      ITRKSTS(1)=IBITS(JZEXT(IMESS(7)),5,3)
      ITRKSTS(2)=IBITS(JZEXT(IMESS(7)),13,3)
      ITRKSTS(3)=IBITS(JZEXT(IMESS(8)),5,3)
      ITRKSTS(4)=IBITS(JZEXT(IMESS(8)),13,3)
      IFOM=0
      IFOM=IBITS(IMESS(9),8,1)
      IF(BTEST(IMESS(9),15)) IFOM=IFOM+8
      RVEPE=0.25*(2**IBITS(JZEXT(IMESS(9)),4,4))      
      RHEPE=0.25*(2**IBITS(JZEXT(IMESS(9)),0,4))      
      RLAT=FLOAT(ILAT)/2**23
      RLONG=FLOAT(ILONG)/2**23
      RALTMSL=FLOAT(IAMSL)/2**4
      RALTREF=FLOAT(IAREF)/2**4
      RGS=FLOAT(IGS)/2**8
      RHDG=(FLOAT(IMESS(20))/2**15)*180.0
      RV(1)=RGS*SIND(RHDG)
      RV(2)=RGS*COSD(RHDG)
      RV(3)=FLOAT(IVZ)/2**8
      IDATUM=IMESS(23)
      RETURN
      END
C*******************************************************************************
C
C ROUTINE          GPS1_DECODE_MESS24_BRIEF SUBROUTINE FORTVAX
C
C PURPOSE          Decodes GPS message 24 for lat/long/height only(Nav lat/lng)
C
C DESCRIPTION      Decodes the GPS Navigation state lat/lng message and returns
C                  the decoded information as shown below.  This message
C                  contains position information as lats and longs, and height
C                  only.  This is aimed at use on HORACE to save time.
C
C VERSION          1.00  03-03-92  W.D.N.JACKSON
C
C ARGUMENTS        IMESS(26)   I*2  IN   GPS message 24, incl header words
C                  RLAT        R*4  OUT  Latitude (-90 to +90 deg)
C                  RLONG       R*4  OUT  Longitude (-180 to +180 deg)
C                  RALTMSL     R*4  OUT  Height above MSL (m)
C
C*******************************************************************************
      SUBROUTINE GPS1_DECODE_MESS24_BRIEF(IMESS,RLAT,RLONG,RALTMSL)
      IMPLICIT  NONE
      INTEGER*2 IMESS(-1:24)
      REAL*4    RLAT,RLONG,RALTMSL
      INTEGER*4 GPS1_ILONGWR,ILAT,ILONG,IAMSL

      ILAT=GPS1_ILONGWR(IMESS(10))
      ILONG=GPS1_ILONGWR(IMESS(12))
      IAMSL=GPS1_ILONGWR(IMESS(14))
      RLAT=FLOAT(ILAT)/2**23
      RLONG=FLOAT(ILONG)/2**23
      RALTMSL=FLOAT(IAMSL)/2**4
      RETURN
      END
C*******************************************************************************
C
C ROUTINE          GPS1_DECODE_MESS18 SUBROUTINE FORTVAX
C
C PURPOSE          Decodes GPS message 18 (Receiver measurement)
C
C DESCRIPTION      Decodes the GPS receiver measurement message and returns
C                  the decoded information as shown below.  This message
C                  contains status information for each of the four tracking
C                  channels in the GPS receiver.
C
C VERSION          1.00  13-09-93  W.D.N.JACKSON
C
C ARGUMENTS        IMESS(46)   I*2  IN   GPS message 18, incl header words
C                  CTIME       C*11 OUT  GPS measurement time - hh:mm:ss.dd
C                  IRECID      I*4  OUT  Receiver ID
C                  RTIME       R*4  OUT  User measurement time (s)
C                  RPSR(4)     R*4  OUT  Pseudo ranges (m)
C                  RDR(4)      R*4  OUT  Delta ranges (m)
C                  ICHSTS(4)   I*4  OUT  Channel status:
C                                         0 No measurement
C                                         1 Initial search in progress
C                                         2 Pull-in in progress
C                                         3 Not used
C                                         4 AFC/code (noncoherent)
C                                         5 Costas track (coherent)
C                                         6 Not used
C                                         7 Reacquisition search in progress
C                  ISV(4)      I*4  OUT  Space vehicle PRN number
C                  IEPHEM(4)   I*4  OUT  1 if ephemeris available, else 0
C
C*******************************************************************************
      SUBROUTINE GPS1_DECODE_MESS18(IMESS,CTIME,IRECID,RTIME,RPSR,RDR,
     -    ICHSTS,ISV,IEPHEM)
      IMPLICIT  NONE
      INTEGER*2 IMESS(-1:44),IRDRM(6)
      BYTE BRDRM(12)
      CHARACTER CTIME*11
      REAL*4    RTIME,RPSR(4),RDR(4)
      REAL*8    DTEMP
      INTEGER*4 IRECID,ICHSTS(4),ISV(4),IEPHEM(4)
      INTEGER*4 GPS1_ILONGWR,IHR,IMN,ISC,IHU,ISPM,IDAY,I,IDR1,IDR2,IDR3,
     -    IDR4
      EQUIVALENCE (IRDRM,BRDRM)
      
      CALL GPS1_DECODE_TIME(IMESS(2),IHR,IMN,ISC,IHU,ISPM,IDAY,CTIME)
      IRECID=IBITS(JZEXT(IMESS(6)),4,12)
      RTIME=FLOAT(JZEXT(IMESS(7)))*0.001
      DO I=1,4
        DTEMP=DFLOAT(GPS1_ILONGWR(IMESS(6+2*I)))
        DTEMP=DTEMP/2**5
        RPSR(I)=REAL(DTEMP)
        ICHSTS(I)=IBITS(JZEXT(IMESS(21+I)),9,3)
        ISV(I)=IBITS(JZEXT(IMESS(21+I)),0,5)
        IEPHEM(I)=IBITS(JZEXT(IMESS(21+I)),5,1)
      END DO
      IDR1=0
      CALL GPS1_MVBITS_WL(IMESS(16),8,8,IDR1,16)
      CALL GPS1_MVBITS_WL(IMESS(16),0,8,IDR1,8)
      CALL GPS1_MVBITS_WL(IMESS(17),8,8,IDR1,0)
      IF(BTEST(IDR1,23)) IDR1=IDR1.OR.'FF000000'X
      RDR(1)=FLOAT(IDR1)/2**12
      IDR2=0
      CALL GPS1_MVBITS_WL(IMESS(17),0,8,IDR2,16)
      CALL GPS1_MVBITS_WL(IMESS(18),8,8,IDR2,8)
      CALL GPS1_MVBITS_WL(IMESS(18),0,8,IDR2,0)
      IF(BTEST(IDR2,23)) IDR2=IDR2.OR.'FF000000'X
      RDR(2)=FLOAT(IDR2)/2**12
      IDR3=0
      CALL GPS1_MVBITS_WL(IMESS(19),8,8,IDR3,16)
      CALL GPS1_MVBITS_WL(IMESS(19),0,8,IDR3,8)
      CALL GPS1_MVBITS_WL(IMESS(20),8,8,IDR3,0)
      IF(BTEST(IDR3,23)) IDR3=IDR3.OR.'FF000000'X
      RDR(3)=FLOAT(IDR3)/2**12
      IDR4=0
      CALL GPS1_MVBITS_WL(IMESS(20),0,8,IDR4,16)
      CALL GPS1_MVBITS_WL(IMESS(21),8,8,IDR4,8)
      CALL GPS1_MVBITS_WL(IMESS(21),0,8,IDR4,0)
      IF(BTEST(IDR4,23)) IDR4=IDR4.OR.'FF000000'X
      RDR(4)=FLOAT(IDR4)/2**12
      RETURN
      END
C*******************************************************************************
C
C ROUTINE          GPS1_DECODE_MESS20 SUBROUTINE FORTVAX
C
C PURPOSE          Decodes GPS message 20 (Receiver Status)
C
C DESCRIPTION      Decodes the GPS receiver status message and returns
C                  the decoded information as shown below.  This message
C                  contains status information about the receiver and the
C                  signals being measured.
C
C VERSION          1.00  13-09-93  W.D.N.JACKSON
C
C ARGUMENTS        IMESS(36)   I*2  IN   GPS message 20, incl header word
C                  CTIME       C*11 OUT  GPS time
C                  IRECID      I*4  OUT  Receiver ID
C                  IAOLD       I*4  OUT  1=old almanac, else 0
C                  RUTCCOR     R*4  OUT  UTC offset from GPS (s)
C                  ISVIDS(4)   I*4  OUT  Assigned satellite list
C                  ICNO(4)     I*4  OUT  Signal strengths (db-Hz)
C                  RTROPCOR(4) R*4  OUT  Tropospheric correction (m)
C                  RIONOCOR(4) R*4  OUT  Ionospheric correction (m)
C                  IODE(4)     I*4  OUT  Issue of data ephemeris counter
C                  IWKNUM      I*4  OUT  GPS week number
C                  RCLKBIAS    R*4  OUT  Estimated receiver clock bias (m)
C                  RCLKDRFT    R*4  OUT  Estimated receiver clock drift (m/s)
C                  IMAINTSUM   I*4  OUT  Maintenance and diagnostic bits
C                  IDYNAM      I*4  OUT  Platform dynamics:
C                                         1 Stationary
C                                         3 Ground vehicle
C                                         4 4g platform
C                  RGRPDEL1    R*4  OUT  Antenna 1 path delay (m)
C                  ITRKRS      I*4  OUT  No of trackers in use
C                  IRECTYP     I*4  OUT  Receiver type - 4
C                  ICDU        I*4  OUT  1 if CDU present
C                  CSOFT       C*4  OUT  Software version and revision v.rr
C                  CUTC        C*23 OUT  UTC time - hh:mm:ss.dd dd-mmm-yyyy
C*******************************************************************************
      SUBROUTINE GPS1_DECODE_MESS20(IMESS,CTIME,IRECID,IAOLD,
     -    RUTCCOR,ISVIDS,ICNO,RTROPCOR,RIONOCOR,IODE,IWKNUM,RCLKBIAS,
     -    RCLKDRFT,IMAINTSUM,IDYNAM,RGRPDEL1,ITRKRS,IRECTYP,ICDU,CSOFT,
     _    CUTC)
      IMPLICIT  NONE
      INTEGER*2 IMESS(-1:34)
      CHARACTER CTIME*11,CSOFT*4,CUTC*23,CUTCCOR*5
      REAL*4    RUTCCOR,RTROPCOR(4),RIONOCOR(4),RCLKBIAS,
     -          RCLKDRFT,RGRPDEL1
      INTEGER*4 IRECID,IAOLD,ISVIDS(4),ICNO(4),IODE(4),IWKNUM,
     -          IMAINTSUM,IDYNAM
      INTEGER*4 SYS$BINTIM,SYS$ASCTIM,LIB$CVT_TO_INTERNAL_TIME,
     -    LIB$ADD_TIMES,LIB$SUB_TIMES,IRECTYP,ICDU,ITRKRS,IOS
      INTEGER*4 GPS1_ILONGWR,IHR,IMN,ISC,IHU,ISPM,IDAY
      INTEGER*4 IUTC,ICB,ICD,IMA,I,IVER,IREV,IW,ID,ISTAT
      REAL*8    DTIME,DTEMP
      CHARACTER CREFDATE*23
      DATA CREFDATE /'06-JAN-1980 00:00:00.00'/ !GPS reference date
      INCLUDE '($LIBDTDEF)'

      IUTC=GPS1_ILONGWR(IMESS(7))
      ICB=GPS1_ILONGWR(IMESS(24))
      ICD=GPS1_ILONGWR(IMESS(26))
      IMA=GPS1_ILONGWR(IMESS(28))
      
      CALL GPS1_DECODE_TIME(IMESS(2),IHR,IMN,ISC,IHU,ISPM,IDAY,CTIME)
      IRECID=IBITS(JZEXT(IMESS(6)),4,12)
      IAOLD=IBITS(JZEXT(IMESS(6)),0,1)
      RUTCCOR=FLOAT(IUTC)/2**26
      ISVIDS(1)=IBITS(JZEXT(IMESS(9)),0,8)
      ISVIDS(2)=IBITS(JZEXT(IMESS(9)),8,8)
      ISVIDS(3)=IBITS(JZEXT(IMESS(10)),0,8)
      ISVIDS(4)=IBITS(JZEXT(IMESS(10)),8,8)
      ICNO(1)=IBITS(JZEXT(IMESS(11)),0,8)
      ICNO(2)=IBITS(JZEXT(IMESS(11)),8,8)
      ICNO(3)=IBITS(JZEXT(IMESS(12)),0,8)
      ICNO(4)=IBITS(JZEXT(IMESS(12)),8,8)
      DO I=1,4
        RTROPCOR(I)=FLOAT(IMESS(12+I))/2**4
        RIONOCOR(I)=FLOAT(IMESS(16+I))/2**4
      END DO
      IODE(1)=IBITS(JZEXT(IMESS(21)),0,8)
      IODE(2)=IBITS(JZEXT(IMESS(21)),8,8)
      IODE(3)=IBITS(JZEXT(IMESS(22)),0,8)
      IODE(4)=IBITS(JZEXT(IMESS(22)),8,8)
      IWKNUM=IMESS(23)
      RCLKBIAS=FLOAT(ICB)/2**4
      RCLKDRFT=FLOAT(ICD)/2**7
      IMAINTSUM=IMA
      IRECTYP=IBITS(JZEXT(IMESS(30)),7,3)
      ICDU=IBITS(JZEXT(IMESS(30)),11,1)
      IVER=IBITS(JZEXT(IMESS(30)),4,3)
      IREV=IBITS(JZEXT(IMESS(30)),0,4)
      WRITE(CSOFT,10,IOSTAT=IOS) IVER,IREV
      IDYNAM=IBITS(JZEXT(IMESS(31)),13,3)
      ITRKRS=IBITS(JZEXT(IMESS(31)),8,3)
      RGRPDEL1=FLOAT(IMESS(32))/2**6
C
C Compute UTC from GPS time, noting that the GPS week starts on sunday and the
C VMS week starts on monday, as follows:
C
C 1.  Convert GPS reference date to internal absolute time
C 2.  Convert GPS week number to internal delta time and add to GPS time
C 3.  Convert GPS day of week to internal delta time and add to GPS time
C 4.  Convert GPS time of day to internal delta time and add to GPS time
C 5.  Convert UTC correction to internal delta time and subtract from GPS time
C 6.  Convert internal time to 23 character ASCII string
C
      IW=IWKNUM
      ID=IDAY
      IF(ID.EQ.0) THEN                 !Convert GPS sundays to VMS sundays
        IW=IW-1
        ID=ID+7
      END IF
      ISTAT=SYS$BINTIM(CREFDATE,DTIME) !Cvt GPS ref date to abs int time
      ISTAT=LIB$CVT_TO_INTERNAL_TIME(LIB$K_DELTA_WEEKS,IW,DTEMP)
      ISTAT=LIB$ADD_TIMES(DTIME,DTEMP,DTIME)
      ISTAT=LIB$CVT_TO_INTERNAL_TIME(LIB$K_DELTA_DAYS,ID,DTEMP)
      ISTAT=LIB$ADD_TIMES(DTIME,DTEMP,DTIME)
      ISTAT=SYS$BINTIM('0 '//CTIME,DTEMP)
      ISTAT=LIB$ADD_TIMES(DTIME,DTEMP,DTIME)
      WRITE(CUTCCOR,11,IOSTAT=IOS) RUTCCOR
      ISTAT=SYS$BINTIM('0 ::'//CUTCCOR,DTEMP)
      ISTAT=LIB$SUB_TIMES(DTIME,DTEMP,DTIME)
      ISTAT=SYS$ASCTIM(,CUTC,DTIME,)
      RETURN
10    FORMAT(I1,'.',I2.2)
11    FORMAT(F5.2)
      END
C*******************************************************************************
C
C ROUTINE          GPS1_MESS_OPEN SUBROUTINE FORTVAX
C
C PURPOSE          Opens a VMS channel for sending messages to the GPS receiver
C
C DESCRIPTION      Establishes a VMS channel to the GPS receiver via an RS232
C                  port on the system.  Terminates the program with a message
C                  if fails to allocate the port, or cannot assign a channel.
C                  Note that the terminal line used must be allocatable, and
C                  must have its characteristics set with the following:
C
C                  /EIGHTBIT /NOTTSYNC /PASTHRU /SPEED=(9600,9600)
C
C VERSION          1.00  13-09-93  W.D.N.JACKSON
C
C ARGUMENTS        CNAME       C**  IN   Name of RS232 port to GPS system
C
C*******************************************************************************
      SUBROUTINE GPS1_MESS_OPEN(CNAME)
      IMPLICIT  NONE
      CHARACTER CNAME*(*)
      INTEGER*4 ILUN(3),ICHAN,IMESSIZ(0:31),ISTAT,SYS$ASSIGN
      COMMON /GPS1_COM/ ILUN,ICHAN,IMESSIZ

      ISTAT=SYS$ASSIGN(CNAME,ICHAN,,)
      IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      RETURN
      END
C*******************************************************************************
C
C ROUTINE          GPS1_MESS_GET SUBROUTINE FORTVAX
C
C PURPOSE          Solicits a message from the GPS receiver and returns it
C
C DESCRIPTION      Solicits a downlink message from the GPS receiver, waits
C                  for it to be sent, and returns with it.  If a valid message
C                  is not received within 2 seconds, returns with ISTAT = 0.
C                  The returned message includes the two word header, and the
C                  final one word data checksum.
C
C VERSION          1.00  13-09-93  W.D.N.JACKSON
C
C ARGUMENTS        IMESSNO     I*4  IN   Required message number (16 up)
C                  ISTAT       I*4  OUT  0 if error, else 1
C                  IMESS(103)  I*2  OUT  The received message
C
C*******************************************************************************
      SUBROUTINE GPS1_MESS_GET(IMESSNO,ISTAT,IMESS)
      IMPLICIT NONE
      INTEGER*4 MASK(2)/2*0/
      INTEGER*2 IMESS(103),JMESS(2),IOSB(4)
      INCLUDE '($IODEF)'
      INTEGER*4 ILUN(3),ICHAN,IMESSIZ(0:31),ISTAT,ISUM,IFUNC,ISTS,
     -    IBYTES,IMESSNO,SYS$QIOW
      COMMON /GPS1_COM/ ILUN,ICHAN,IMESSIZ

      ISTAT=0                          !Default is error return   
      IF(IMESSNO.LT.17.OR.IMESSNO.GT.31) RETURN
      IF(IMESSIZ(IMESSNO).LE.0) RETURN
      JMESS(1)=0
      CALL GPS1_MVBITS_LW(IMESSNO,0,5,JMESS(1),11) !Form request block
      ISUM=-JZEXT(JMESS(1))
      CALL GPS1_MVBITS_LW(ISUM,0,16,JMESS(2),0)
C First send the request for the message
      IFUNC=IO$_WRITEVBLK.OR.IO$M_NOFORMAT.OR.IO$M_CANCTRLO
      ISTS=SYS$QIOW(,%VAL(ICHAN),%VAL(IFUNC),IOSB,,,
     -    JMESS,%VAL(4),,%VAL(0),,)
      IF(.NOT.ISTS.OR..NOT.IOSB(1)) RETURN
C Now get the returned message
      IFUNC=IO$_READVBLK.OR.IO$M_TIMED.OR.IO$M_NOECHO.OR.IO$M_PURGE
     -    .OR.IO$M_NOFILTR
      IBYTES=(IMESSIZ(IMESSNO)+3)*2
      ISTS=SYS$QIOW(,%VAL(ICHAN),%VAL(IFUNC),IOSB,,,
     -    IMESS(1),%VAL(IBYTES),%VAL(2),%VAL(%LOC(MASK)),,)
      IF(.NOT.ISTS.OR..NOT.IOSB(1)) RETURN
      IF(IOSB(2).NE.IBYTES) RETURN
      ISTAT=1
      RETURN
      END
C*******************************************************************************
C
C ROUTINE          GPS1_MESS_SEND SUBROUTINE FORTVAX
C
C PURPOSE          Sends a message to the GPS receiver
C
C DESCRIPTION      Sends a GPS message to the GPS receiver.  Normally this will
C                  only be the initialization message (message 1), but any
C                  message can be handled.  Note that the message should
C                  include the header, and the data checksum when it is passed
C                  to this routine.  This routine only returns when the message
C                  has been sent, but it does not check for confirmation
C                  from the GPS receiver that the message has been received.
C
C VERSION          1.00  13-09-93  W.D.N.JACKSON
C
C ARGUMENT         IMESS(103)  I*2  IN   The message to be sent
C                  ISTAT       I*4  OUT  0 if error, else 1
C
C*******************************************************************************
      SUBROUTINE GPS1_MESS_SEND(IMESS,ISTAT)
      IMPLICIT NONE
      INTEGER*2 IMESS(103),IOSB(4)
      INCLUDE '($IODEF)'
      INTEGER*4 ILUN(3),ICHAN,IMESSIZ(0:31),ISTAT,IMESSNO,ILEN,IFUNC,
     -    ISTS,SYS$QIOW
      COMMON /GPS1_COM/ ILUN,ICHAN,IMESSIZ

      ISTAT=1
      IMESSNO=0
      CALL GPS1_MVBITS_WL(IMESS(1),4,5,IMESSNO,0) !Get message length
      IF(IMESSNO.LT.1.OR.IMESSNO.GT.16) RETURN
      IF(IMESSIZ(IMESSNO).LE.0) RETURN
      ILEN=(IMESSIZ(IMESSNO)+2)*2
      IFUNC=IO$_WRITEVBLK.OR.IO$M_NOFORMAT.OR.IO$M_CANCTRLO
      ISTS=SYS$QIOW(,%VAL(ICHAN),%VAL(IFUNC),IOSB,,,
     -    IMESS,%VAL(ILEN),,%VAL(0),,)
      IF(.NOT.ISTS.OR..NOT.IOSB(1)) ISTAT=0
      RETURN
      END
C*******************************************************************************
C
C ROUTINE          GPS1_MESS_CLOSE SUBROUTINE FORTVAX
C
C PURPOSE          Frees the GPS VMS channel
C
C DESCRIPTION      Cancels any outstanding I/O on the GPS channel and then
C                  frees the channel.
C
C VERSION          1.00  13-09-93  W.D.N.JACKSON
C
C ARGUMENTS        none
C
C*******************************************************************************
      SUBROUTINE GPS1_MESS_CLOSE
      IMPLICIT NONE
      INTEGER*4 ILUN(3),ICHAN,IMESSIZ(0:31)
      COMMON /GPS1_COM/ ILUN,ICHAN,IMESSIZ

      CALL SYS$CANCEL(%VAL(ICHAN))
      CALL SYS$DASSGN(%VAL(ICHAN))
      RETURN
      END
C*******************************************************************************
C
C ROUTINE          GPS1_MESSF_OPEN SUBROUTINE FORTVAX
C
C PURPOSE          Opens an existing GPS message file
C
C DESCRIPTION      Opens a GPS message file and assigns it the specified
C                  stream number.  Returns with ISTAT=-1 if an invalid stream
C                  was specified, else with the FORTRAN IO status code.
C
C VERSION          1.00  13-09-93  W.D.N.JACKSON
C
C ARGUMENTS        CNAME       C**  IN   GPS message file name
C                  ISTR        I*4  IN   Stream number for file (1-3)
C                  ISTAT       I*4  OUT  0 for success, else FORTRAN error code
C
C*******************************************************************************
      SUBROUTINE GPS1_MESSF_OPEN(CNAME,ISTR,ISTAT)
      IMPLICIT NONE
      CHARACTER CNAME*(*)
      INTEGER*4 ISTR,ISTAT
      INTEGER*4 ILUN(3),ICHAN,IMESSIZ(0:31)
      COMMON /GPS1_COM/ ILUN,ICHAN,IMESSIZ
      DATA ILUN /3*1000/
  
      ISTAT=-1                          !Default error return
      IF(ISTR.LT.1.OR.ISTR.GT.3) RETURN !Invalid stream
      IF(ILUN(ISTR).NE.1000) RETURN     !Stream already in use
      CALL LIB$GET_LUN(ILUN(ISTR))      !Get LUN
      OPEN(UNIT=ILUN(ISTR),FILE=CNAME,STATUS='OLD',READONLY, !Open file
     -    DEFAULTFILE='.DAT',IOSTAT=ISTAT)
      IF(ISTAT.NE.0) THEN               !If open failed
        CALL LIB$FREE_LUN(ILUN(ISTR))   !Release LUN
        ILUN(ISTR)=1000                 !Show stream free
      END IF
      RETURN
      END
C*******************************************************************************
C
C ROUTINE          GPS1_MESSF_CREATE SUBROUTINE FORTVAX
C
C PURPOSE          Creates a new GPS message file
C
C DESCRIPTION      Creates a new GPS message file and assigns it the specified
C                  stream number.  Returns with ISTAT=-1 if an invalid stream
C                  was specified, else with the FORTRAN IO status code.
C
C VERSION          1.00  13-09-93  W.D.N.JACKSON
C
C ARGUMENTS        CNAME       C**  IN   GPS message file name
C                  ISTR        I*4  IN   Stream number for file (1-3)
C                  ISTAT       I*4  OUT  0 for success, else FORTRAN error code
C
C*******************************************************************************
      SUBROUTINE GPS1_MESSF_CREATE(CNAME,ISTR,ISTAT)
      IMPLICIT NONE
      CHARACTER CNAME*(*)
      INTEGER*4 ISTR,ISTAT
      INTEGER*4 ILUN(3),ICHAN,IMESSIZ(0:31)
      COMMON /GPS1_COM/ ILUN,ICHAN,IMESSIZ
      DATA ILUN /3*1000/
  
      ISTAT=-1                          !Default error return               
      IF(ISTR.LT.1.OR.ISTR.GT.3) RETURN !Invalid stream
      IF(ILUN(ISTR).NE.1000) RETURN     !Stream already open
      CALL LIB$GET_LUN(ILUN(ISTR))      !Get LUN
      OPEN(UNIT=ILUN(ISTR),FILE=CNAME,STATUS='NEW',IOSTAT=ISTAT, !Create file
     -    DEFAULTFILE='.DAT',CARRIAGECONTROL='LIST')
      IF(ISTAT.NE.0) THEN               !If create failed
        CALL LIB$FREE_LUN(ILUN(ISTR))   !Release LUN
        ILUN(ISTR)=1000                 !Show stream free
      END IF
      RETURN
      END
C*******************************************************************************
C
C ROUTINE          GPS1_MESSF_CLOSE SUBROUTINE FORTVAX
C
C PURPOSE          Closes a GPS message file
C
C DESCRIPTION      Closes the GPS message file assigned the specified
C                  stream number.  Returns with ISTAT=-1 if an invalid stream
C                  was specified, else with the FORTRAN IO status code.
C
C VERSION          1.00  13-09-93  W.D.N.JACKSON
C
C ARGUMENTS        ISTR        I*4  IN   Stream number for file (1-3)
C                  ISTAT       I*4  OUT  0 for success, else FORTRAN error code
C
C*******************************************************************************
      SUBROUTINE GPS1_MESSF_CLOSE(ISTR,ISTAT)
      IMPLICIT NONE
      INTEGER*4 ISTR,ISTAT
      INTEGER*4 ILUN(3),ICHAN,IMESSIZ(0:31)
      COMMON /GPS1_COM/ ILUN,ICHAN,IMESSIZ
      DATA ILUN /3*1000/

      ISTAT=-1                          !Default error return
      IF(ISTR.LT.1.OR.ISTR.GT.3) RETURN !Invalid stream
      IF(ILUN(ISTR).EQ.1000) RETURN     !Stream not open
      CLOSE(UNIT=ILUN(ISTR),IOSTAT=ISTAT) !Close file
      CALL LIB$FREE_LUN(ILUN(ISTR))     !Free LUN
      ILUN(ISTR)=1000                   !Show stream free
      RETURN
      END
C*******************************************************************************
C
C ROUTINE          GPS1_MESSF_READ SUBROUTINE FORTVAX
C
C PURPOSE          Reads a GPS message from a GPS message file
C
C DESCRIPTION      Reads a GPS message from the GPS message file specified by
C                  the stream number.  Returns with ISTAT=-1 if an invalid
C                  stream was specified, else with the FORTRAN IO status code.
C
C VERSION          1.00  13-09-93  W.D.N.JACKSON
C
C ARGUMENTS        ISTR        I*4  IN   Stream number for file (1-3)
C                  IMESS(103)  I*2  OUT  GPS message, incl header words
C                  ISTAT       I*4  OUT  0 for success, else FORTRAN error code
C
C*******************************************************************************
      SUBROUTINE GPS1_MESSF_READ(ISTR,BMESS,ISTAT)
      IMPLICIT  NONE
      BYTE      BMESS(212)
      INTEGER*4 ISTR,ISTAT,I,IN
      INTEGER*4 ILUN(3),ICHAN,IMESSIZ(0:31)
      COMMON /GPS1_COM/ ILUN,ICHAN,IMESSIZ
      DATA ILUN /3*1000/

      ISTAT=-1                          !Default error return
      IF(ISTR.LT.1.OR.ISTR.GT.3) RETURN !Invalid stream
      IF(ILUN(ISTR).EQ.1000) RETURN     !Stream not open
      DO I=1,6                          !Overwrite message area, in case fails
        BMESS(I)=0                      ! fails to read any data
      END DO
      READ(ILUN(ISTR),10,IOSTAT=ISTAT) 
     -    IN,(BMESS(I),I=1,MIN(MAX(IN,1),212)) !Read message
      RETURN
10    FORMAT(Q,212A1)
      END
C*******************************************************************************
C
C ROUTINE          GPS1_MESSF_WRITE SUBROUTINE FORTVAX
C
C PURPOSE          Writes a GPS message to a GPS message file
C
C DESCRIPTION      Writes a GPS message to the GPS message file specified by
C                  the stream number.  Returns with ISTAT=-1 if an invalid
C                  stream was specified, else with the FORTRAN IO status code.
C
C VERSION          1.00  13-09-93  W.D.N.JACKSON
C
C ARGUMENTS        ISTR        I*4  IN   Stream number for file (1-3)
C                  IMESS(103)  I*2  IN   GPS message, incl header words
C                  ISTAT       I*4  OUT  0 for success, else FORTRAN error code
C
C*******************************************************************************
      SUBROUTINE GPS1_MESSF_WRITE(ISTR,BMESS,ISTAT)
      IMPLICIT  NONE
      BYTE      BMESS(206),BM(2)
      INTEGER*2 IMH
      INTEGER*4 ISTR,ISTAT,IMESSNO,IL,I
      EQUIVALENCE (IMH,BM(1))
      INTEGER*4 ILUN(3),ICHAN,IMESSIZ(0:31)
      COMMON /GPS1_COM/ ILUN,ICHAN,IMESSIZ
      DATA ILUN /3*1000/

      ISTAT=-1                          !Default error return
      IF(ISTR.LT.1.OR.ISTR.GT.3) RETURN !Invalid stream
      IF(ILUN(ISTR).EQ.1000) RETURN     !Stream not open
      IMESSNO=0
      BM(1)=BMESS(1)
      BM(2)=BMESS(2)
      CALL GPS1_MVBITS_WL(IMH,4,5,IMESSNO,0)
      IF(IMESSNO.LT.0.OR.IMESSNO.GT.31) RETURN
      IF(IMESSIZ(IMESSNO).LE.0) RETURN
      IF(IMESSNO.EQ.0) THEN
        IL=32
      ELSE
        IL=IMESSIZ(IMESSNO)+2           !Message length incl header and chksum
        IF(IMESSNO.GE.17) IL=IL+1       !Add sync word if downlink message
        IL=IL*2                         !Message length (bytes)
      END IF
      WRITE(ILUN(ISTR),10,IOSTAT=ISTAT) (BMESS(I),I=1,IL) !Write message
      RETURN
10    FORMAT(206A1)
      END
C*******************************************************************************
C
C ROUTINE          GPS1_DECODE_TIME SUBROUTINE FORTVAX
C
C PURPOSE          Decodes GPS time word into hh:mm:ss.dd
C
C DESCRIPTION      Takes the GPS eight byte time word and decodes it into hours,
C                  minutes,seconds, and hundreths of seconds.  It also produces
C                  the time as a character string with the format hh:mm:ss.dd.
C                  The day of week information is returned separately.
C
C VERSION          1.00  13-09-93  W.D.N.JACKSON
C
C ARGUMENTS        JGPSTIME(4) I*2  IN   GPS time as two words
C                  IHR         I*4  OUT  GPS hours
C                  IMN         I*4  OUT  GPS minutes
C                  ISC         I*4  OUT  GPS seconds
C                  IHU         I*4  OUT  GPS hundreths of seconds
C                  ISPM        I*4  OUT  GPS seconds past midnight
C                  IDAY        I*4  OUT  GPS day of week - 1 = Sunday?
C                  CTIME       C*11 OUT  GPS time char string - hh:mm:ss.dd
C
C CHANGES          1.01  01-09-98  W.D.N.JACKSON
C                  Now uses exact hundredths rather than nearest.
C
C*******************************************************************************
      SUBROUTINE GPS1_DECODE_TIME(JGPSTIME,IHR,IMN,ISC,IHU,ISPM,IDAY,
     -    CTIME)
      IMPLICIT  NONE
      INTEGER*4 IGPSTIME(2),IHR,IMN,ISC,IHU,ISPM,IDAY,ITEMP,IOS
      INTEGER*2 JTEMP(2),JGPSTIME(4),JTEMP1(4)
      EQUIVALENCE (ITEMP,JTEMP)
      EQUIVALENCE (IGPSTIME,JTEMP1)
      CHARACTER*11 CTIME
      INTEGER*4 GPS1_ILONGWR,I,IT1,IT2

      DO I=1,4
        JTEMP1(I)=JGPSTIME(I)
      END DO
      ITEMP=IGPSTIME(1)
      IT1=GPS1_ILONGWR(JTEMP)          !Integer seconds of week
      ITEMP=IGPSTIME(2)
      IT2=GPS1_ILONGWR(JTEMP)          !Each bit is 2**-31 seconds
      IDAY=IT1/(24*60*60)
      IT1=MOD(IT1,24*60*60)
      ISC=MOD(IT1,60)
      IHR=IT1/3600
      IMN=(IT1-IHR*3600)/60
      IHU=MOD(INT((FLOAT(IT2)/2**16/2**15)*100.0),100)
      ISPM=IT1
      WRITE(CTIME,10,IOSTAT=IOS) IHR,':',IMN,':',ISC,'.',IHU
      RETURN
10    FORMAT(3(I2.2,A),I2.2)
      END
C*******************************************************************************
C
C ROUTINE          GPS1_DISTEN SUBROUTINE FORTVAX
C
C PURPOSE          Calculates distance east and north of a reference point
C
C DESCRIPTION      The distance east of the reference point is given by:
C
C                       Dist East = F * (Long - Ref Long) * PI/180
C
C                  where F is the radius of the current latitude circle given by
C
C                      F = (A -|Lat * B|) * cos(Lat * PI/180)
C
C                  and A = the radius of the Earth at the equator
C                      B = the correction factor (Km/deg) to be applied at a
C                          given latitude to the radius of the Earth assuming 
C                          the Earth is an oblate spheriod.
C
C                  The distance north of the reference point is given by:
C
C                       Dist North = R * (Lat - Ref Lat) * PI/180
C
C                  where R is the mean Earth radius between the current
C                  latitude and the reference latitude. This is calculated using
C
C                           R = A - (|Lat + Ref Lat| * B)/2
C
C VERSION          1.00  13-09-93  W.D.N.JACKSON
C
C ARGUMENTS        REFLNG      R*4  IN   Reference longitude 
C                  REFLAT      R*4  IN   Reference latitude
C                  RLNG        R*4  IN   Current longitude
C                  RLAT        R*4  IN   Current latitude
C                  REAST       R*4  OUT  Distance east from reference (km)
C                  RNRTH       R*4  OUT  Distance north from reference (km)
C
C REFERENCES       S_POSTN subroutine in TARDIS (by D.W.Johnson)
C
C*******************************************************************************
      SUBROUTINE GPS1_DISTEN(REFLNG,REFLAT,RLNG,RLAT,REAST,RNRTH)
      IMPLICIT NONE
      REAL*4 REFLNG,REFLAT,RLNG,RLAT,REAST,RNRTH
      REAL*8 DPI,DRADEQ,D2R,DOBLAT_COR,DLAT,DLNG,DREFLAT,DREFLNG,
     -    DRADML,DRADLC
C
C Set up constants
C
      DPI=3.141592654D00                !Double precision pi      
      DRADEQ=6.378388D03                !Radius of earth at equator
      D2R=DPI/180.D0                    !Degrees to radians
      DOBLAT_COR=2.38656D-01            !Radius correction factor (km/deg)
C
C Convert lats & longs to double precision
C
      DLAT=DBLE(RLAT)
      DLNG=DBLE(RLNG)
      DREFLAT=DBLE(REFLAT)
      DREFLNG=DBLE(REFLNG)
C
C Calculate radius of circle at mean latitude of current latitude and
C reference latitude.
C
      DRADML=DRADEQ-DABS(DLAT+DREFLAT)*0.5D0*DOBLAT_COR
C
C Calculate length of arc of longitude circle between current latitude 
C and reference latitude.
C
      RNRTH=D2R*DRADML*(DLAT-DREFLAT)
C
C Calculate radius of latitude circle
C
      DRADML=DRADEQ-DABS(DLAT*DOBLAT_COR)
      DRADLC=DRADML*DCOS(DABS(DLAT*D2R))
C
C Calculate length of arc of current latitude circle between current longitude
C and reference longitude
C
      REAST=D2R*DRADLC*(DLNG-DREFLNG)
      RETURN
      END
C*******************************************************************************
C
C ROUTINE          GPS1_INITIME_CODE SUBROUTINE FORTVAX
C
C PURPOSE          Converts date and time to GPS week and time
C
C DESCRIPTION      Takes date and time character strings and converts them to
C                  GPS week number and GPS time.
C
C VERSION          1.00  13-09-93  W.D.N.JACKSON
C
C ARGUMENTS        CDATE       C*9  IN   Date (dd-mmm-yy)
C                  CTIME       C*8  IN   Time (hh:mm:ss)
C                  IWEEK       I*4  OUT  GPS week number
C                  ITIME       I*4  OUT  GPS time
C
C REFERENCES       GPS week numbers count from 01-JAN-80.  GPS time is seconds
C                  from midnight saturday/sunday.
C
C*******************************************************************************
      SUBROUTINE GPS1_INITIME_CODE(CDATE,CTIME,IWEEK,ITIME)
      IMPLICIT  NONE
      INTEGER*4 IWEEK,ITIME,SYS$BINTIM,ISTAT,LIB$CVT_FROM_INTERNAL_TIME,
     -    LIB$SUB_TIMES,IRTIME
      REAL*8    DREFTIME,DTIME,DDIFTIME
      CHARACTER CDATE*9,CTIME*8,CREFDATE*23,CDAT*23
      DATA CREFDATE /'06-JAN-1980 00:00:00.00'/ !GPS reference date
      INCLUDE '($LIBDTDEF)'
C
C Calculations are done using VMS delta and absolute 64 bit internal times.
C Note that the GPS week starts on sunday and the VMS week starts on monday.
C
C 1.  Convert GPS reference date to internal absolute time
C 2.  Convert the supplied time to internal absolute time, inserting the '19'
C     part of the year.
C 3.  Subtract the two times to get a delta time
C 4.  Convert the delta time to the number of elapsed weeks
C 5.  Convert the delta time to the number of seconds into the week
C 6.  Adjust this number of seconds for the GPS week
C
      ISTAT=SYS$BINTIM(CREFDATE,DREFTIME)
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      ISTAT=LIB$CVT_FROM_INTERNAL_TIME !Check for which system
     -    (LIB$K_SECOND_OF_WEEK,IRTIME,DREFTIME) !Should be 518400 seconds
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      CDAT=CDATE(1:7)//'19'//CDATE(8:9)//' '//CTIME//'.00'
      IF(CDATE(8:9).LT.'80') CDAT(8:9)='20'
      CALL STR$UPCASE(CDAT,CDAT)
      CALL SYS$BINTIM(CDAT,DTIME)
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      ISTAT=LIB$SUB_TIMES(DTIME,DREFTIME,DDIFTIME)
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      ISTAT=LIB$CVT_FROM_INTERNAL_TIME
     -    (LIB$K_DELTA_WEEKS,IWEEK,DDIFTIME)
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      CALL LIB$CVT_FROM_INTERNAL_TIME  !This routine is system dependent
     -    (LIB$K_SECOND_OF_WEEK,ITIME,DTIME)
      IF(IRTIME.EQ.604800) ITIME=ITIME-24*60*60 !Fix for HORACE and QUILTS
      ITIME=ITIME+24*60*60-1           !GPS times start sunday not monday  
      IF(ITIME.GT.7*24*60*60) ITIME=ITIME-7*24*60*60
      RETURN
      END
C*******************************************************************************
C
C ROUTINE          GPS1_INITIME_DECODE SUBROUTINE FORTVAX
C
C PURPOSE          Converts GPS week and time to date and time strings
C
C DESCRIPTION      Takes GPS week number and GPS time and converts them to
C                  date and time character strings.
C
C VERSION          1.00  13-09-93  W.D.N.JACKSON
C
C ARGUMENTS        IWEEK       I*4  IN   GPS week number
C                  ITIME       I*4  IN   GPS time
C                  CDATE       C*9  OUT  Date (dd-mmm-yy)
C                  CTIME       C*8  OUT  Time (hh:mm:ss)
C
C REFERENCES       GPS week numbers count from 01-JAN-80.  GPS time is seconds
C                  from midnight saturday/sunday.
C
C*******************************************************************************
      SUBROUTINE GPS1_INITIME_DECODE(IWEEK,ITIME,CDATE,CTIME)
      IMPLICIT  NONE
      INTEGER*4 IWEEK,ITIME,ITIM,ILEN
      INTEGER*4 JREFTIME(2),JTIME(2),JDIFTIME(2)
      CHARACTER CDATE*9,CTIME*8,CREFDATE*23,CSTRING*23
      DATA CREFDATE /'06-JAN-1980 00:00:00.00'/
      INCLUDE '($LIBDTDEF)'
C
C Calculations are done using VMS delta and absolute 64 bit internal times.
C Note that the GPS week starts on sunday and the VMS week starts on monday.
C
C 1.  Convert GPS reference date to internal absolute time
C 2.  Convert the supplied week number to an internal delta time
C 3.  Add the absolute and delta times to get absolute time of week start
C 4.  Convert the supplied time in seconds to an internal delta time
C 5.  Add this delta time to the time of the week start
C 6.  Convert the internal time to an ASCII string and extract date and time
C
      CALL SYS$BINTIM(CREFDATE,JREFTIME(1))
      CALL LIB$CVT_TO_INTERNAL_TIME(LIB$K_DELTA_WEEKS,IWEEK,JDIFTIME(1))
      CALL LIB$ADD_TIMES(JREFTIME,JDIFTIME,JTIME)
      ITIM=ITIME+1
      CALL LIB$CVT_TO_INTERNAL_TIME
     -    (LIB$K_DELTA_SECONDS,ITIM,JDIFTIME(1))
      CALL LIB$ADD_TIMES(JTIME(1),JDIFTIME(1),JTIME(1))
      CALL LIB$SYS_ASCTIM(ILEN,CSTRING,JTIME(1),0)
      CDATE=CSTRING(1:7)//CSTRING(10:11)
      CTIME=CSTRING(13:20)
      RETURN
      END
C*******************************************************************************
C
C ROUTINE          GPS1_ILONGWR INTEGER FUNCTION FORTVAX
C
C PURPOSE          Converts two I*2 words into one I*4 word
C
C DESCRIPTION      Converts two consequetive integer*2 words in an array into
C                  one integer*4 word.  Reverses the order of the words by
C                  treating the first word as the most significant.
C
C VERSION          1.00  13-09-93  W.D.N.JACKSON
C
C ARGUMENTS        IWORD(2)     I*2  IN   Array of two integer*2 words
C                  GPS1_ILONGWR I*4  OUT  The resultant integer*4 word
C
C*******************************************************************************
      INTEGER*4 FUNCTION GPS1_ILONGWR(IWORD)
      IMPLICIT  NONE
      INTEGER*2 IWORD(2)
      CALL GPS1_MVBITS_WL(IWORD(1),0,16,GPS1_ILONGWR,16)
      CALL GPS1_MVBITS_WL(IWORD(2),0,16,GPS1_ILONGWR,0)
      RETURN
      END
C*******************************************************************************
C
C ROUTINE          GPS1_MVBITS_WL SUBROUTINE FORTVAX
C
C PURPOSE          Moves bits between integer words
C
C DESCRIPTION      Converts two consequetive integer*2 words in an array into
C                  one integer*4 word.  Reverses the order of the words by
C                  treating the first word as the most significant.  Unlike
C                  the FORTRAN routine MVBITS this routine will move bits
C                  between I*2 and I*4 words.  However if you try to move more
C                  than 16 bits into an I*2 word, you will corrupt some other
C                  storage location.
C
C VERSION          1.00  13-09-93  W.D.N.JACKSON
C
C ARGUMENTS        JSOURCE(2)  I*    IN     Words with bits to be moved
C                  ISOFF       I*4   IN     Offset in word where bits start (0-
C                  ISBITS      I*4   IN     Number of bits to be moved
C                  IDESTIN     I*4   IN/OUT Word into which bits are to go
C                  IDOFF       I*4   IN     Offset in word where bits are to go
C
C*******************************************************************************
      SUBROUTINE GPS1_MVBITS_WL(JSOURCE,ISOFF,ISBITS,IDESTIN,IDOFF)
      IMPLICIT NONE
      INTEGER*2 JSOURCE(2),JTEMP(2)
      INTEGER*4 ISOURCE,ISOFF,ISBITS,IDESTIN,IDOFF
      EQUIVALENCE (JTEMP,ISOURCE)

      JTEMP(1)=JSOURCE(1)
      JTEMP(2)=JSOURCE(2)
      CALL MVBITS(ISOURCE,ISOFF,ISBITS,IDESTIN,IDOFF)
      RETURN
      END
C*******************************************************************************
C
C ROUTINE          GPS1_MVBITS_LW SUBROUTINE FORTVAX
C
C PURPOSE          Moves bits between integer words
C
C DESCRIPTION      Forms two consequetive integer*2 words in an array from
C                  one integer*4 word.  Reverses the order of the words by
C                  treating the first word as the most significant.  Unlike
C                  the FORTRAN routine MVBITS this routine will move bits
C                  between I*2 and I*4 words.  However if you try to move more
C                  than 16 bits into an I*2 word, you will corrupt some other
C                  storage location.
C
C VERSION          1.00  13-09-93  W.D.N.JACKSON
C
C ARGUMENTS        ISOURCE     I*4   IN     Word with bits to be moved
C                  ISOFF       I*4   IN     Offset in word where bits start (0-
C                  ISBITS      I*4   IN     Number of bits to be moved
C                  JDESTIN(2)  I*2   IN/OUT Words into which bits are to go
C                  IDOFF       I*4   IN     Offset in word where bits are to go
C
C*******************************************************************************
      SUBROUTINE GPS1_MVBITS_LW(ISOURCE,ISOFF,ISBITS,JDESTIN,IDOFF)
      IMPLICIT NONE
      INTEGER*2 JDESTIN(2),JTEMP(2)
      INTEGER*4 ISOURCE,ISOFF,ISBITS,ITEMP,IDOFF
      EQUIVALENCE (JTEMP,ITEMP)

      JTEMP(1)=JDESTIN(1)
      JTEMP(2)=JDESTIN(2)
      CALL MVBITS(ISOURCE,ISOFF,ISBITS,ITEMP,IDOFF)
      JDESTIN(1)=JTEMP(1)
      JDESTIN(2)=JTEMP(2)
      RETURN
      END
C*******************************************************************************
C
C ROUTINE          GPS1_DECODE_REAL SUBROUTINE FORTVAX
C
C PURPOSE          Decodes a REAL GPS number
C
C DESCRIPTION      Takes a four byte real GPS word and decodes it into a
C                  REAL*4 FORTRAN number.  This is done by reversing the two 16
C                  bit parts of the 32 bit word, and then treating the word as
C                  a FORTRAN REAL*4 F_floating number.  The final result needs
C                  to be multiplied by 4 to get it right.
C
C VERSION          1.00  12-05-92  W.D.N.JACKSON
C
C ARGUMENTS        JGPSWORD(2) I*2  IN   GPS real word (4 bytes)
C                  RNUMBER     R*4  OUT  Decoded GPS number
C
C*******************************************************************************
      SUBROUTINE GPS1_DECODE_REAL(JGPSWORD,RNUMBER)
      IMPLICIT  NONE
      INTEGER*4 IGPSWORD,ITEMP
      REAL*4    RNUMBER,RTEMP
      INTEGER*4 GPS1_ILONGWR,ITEMP1
      INTEGER*2 JTEMP(2),JGPSWORD(2),JTEMP1(2)
      EQUIVALENCE (ITEMP,RTEMP)
      EQUIVALENCE (JTEMP,ITEMP1)
      EQUIVALENCE (JTEMP1,IGPSWORD)

      JTEMP1(1)=JGPSWORD(1)
      JTEMP1(2)=JGPSWORD(2)
      ITEMP1=IGPSWORD
      ITEMP=GPS1_ILONGWR(JTEMP)        !Reverse the word
      RNUMBER=RTEMP*4.0                !Treat it a REAL and multiply by 4
      RETURN
      END
C*******************************************************************************
C
C ROUTINE          GPS1_CODE_MESS5 SUBROUTINE FORTVAX
C
C PURPOSE          Codes GPS message 5 (Test instrumentation request)
C
C DESCRIPTION      Codes the GPS test instrumentation request message and 
C                  returns the coded message as shown below.  This message
C                  defines what should be sent by the GPS system in the 
C                  Test Instrumentation.  At present the DOPs and the 
C                  azimuth and elevation are asked for.  If this message has
C                  to be changed in future, it should be done by asking for
C                  further packets at the end of the message, so that existing
C                  Test Instrumentation messages can still be decoded.  Note
C                  that the TARDIS/HORACE GPS processing software makes no
C                  provision for handling Test Instrumentation messages other
C                  than the one defined by this subroutine.
C
C                  Note that the memory addresses and word counts are obtained
C                  by running the DSW program on the PC, entering ALT/= to get
C                  into the system, entering $X to put the system into debug
C                  mode, and then entering DOPS? or AZIMELEV?.  This will show
C                  the codes actually being sent to the GPS to extract the
C                  required information.
C
C                  This message only needs to be sent once.  Thereafter all
C                  message 31 request will be in this format.
C
C VERSION          1.00  12-05-92  W.D.N.JACKSON
C
C ARGUMENTS        IMESS(33)   I*2  OUT  GPS message 5, incl header words
C
C*******************************************************************************
      SUBROUTINE GPS1_CODE_MESS5(IMESS)
      IMPLICIT  NONE
      INTEGER*4 ISUM,I,IW
      INTEGER*2 IMESS(-4:15)

      IMESS(-4)='81FF'X                !Sync word           
      IMESS(-3)=4096+15                !Message number 15
      IMESS(-2)=14                     !Message word count
      IMESS(-1)='8000'X                !Special flag word
      ISUM=0
      DO I=-4,-1
        ISUM=ISUM+JZEXT(IMESS(I))
      END DO
      ISUM=-ISUM
      CALL GPS1_MVBITS_LW(ISUM,0,16,IMESS(0),0) !Header checksum 

      IMESS(1)=0                       !Test Control Word
      IMESS(2)=6                       !Packet count

      IMESS(3)='D544'X                 !HDOP low 2 bytes of address
      IMESS(4)='0280'X                 !HDOP top byte of address and word count
      IMESS(5)='D540'X                 !PDOP low 2 bytes of address
      IMESS(6)='0280'X                 !PDOP top byte of address and word count
      IMESS(7)='1828'X                 !OPT low 2 bytes of address
      IMESS(8)='0480'X                 !OPT top byte of address and word count
      IMESS(9)='1820'X                 !ALT low 2 bytes of address
      IMESS(10)='0480'X                !ALT top byte of address and word count
      IMESS(11)='1800'X                !ELEV low 2 bytes of address
      IMESS(12)='1080'X                !ELEV top byte of address and word count
      IMESS(13)='17E0'X                !AZIM low 2 bytes of address
      IMESS(14)='1080'X                !AZIM top byte of address and word count

      ISUM=0                           !Compute data checksum
      DO IW=1,14
        ISUM=ISUM+JZEXT(IMESS(IW))
      END DO
      ISUM=-ISUM
      CALL GPS1_MVBITS_LW(ISUM,0,16,IMESS(15),0) !Data checksum 
      RETURN
      END
C*******************************************************************************
C
C ROUTINE          GPS1_DECODE_MESS5 SUBROUTINE FORTVAX
C
C PURPOSE          Decodes GPS message 5 (Test Instrumentation Request)
C
C DESCRIPTION      Decodes the GPS test instrumentation request message and
C                  returns the decoded information as shown below.  This message
C                  defines what the GPS system sends when asked for message 31,
C                  and its contents are hard coded in the GPS_CODE_MESS5 
C                  subroutine.  At present 6 packets of information are
C                  requested: HDOP, PDOP, OPT, ALT, ELEV and AZIM.  This
C                  routine returns the no of packets, and arrays with their
C                  memory addresses and word counts.
C
C VERSION          1.00  12-05-92  W.D.N.JACKSON
C
C ARGUMENTS        IMESS(33)   I*2  IN   GPS message 5, incl header words
C                  IPACKET     I*4  OUT  No of packets requested
C                  IADDR(4)    I*4  OUT  Memory address of each packet
C                  IWCNT(4)    I*4  OUT  Word count for each packet
C
C*******************************************************************************
      SUBROUTINE GPS1_DECODE_MESS5(IMESS,IPACKET,IADDR,IWCNT)
      IMPLICIT  NONE
      INTEGER*2 IMESS(-4:15)
      INTEGER*4 IADDR(6),IWCNT(6),I,IPACKET

      IPACKET=IMESS(2)                 !Get number of packets
      DO I=1,6
        IADDR(I)=0
        CALL GPS1_MVBITS_WL(IMESS(1+2*I),0,16,IADDR(I),0) !Get lower two bytes of addr
        CALL GPS1_MVBITS_WL(IMESS(2+2*I),0,8,IADDR(I),16) !Get top byte of address
        IWCNT(I)=IBITS(IMESS(2+2*I),8,8)          !Get word count
      END DO
      RETURN
      END
C*******************************************************************************
C
C ROUTINE          GPS1_DECODE_MESS31 SUBROUTINE FORTVAX
C
C PURPOSE          Decodes GPS message 31 (Test Instrumentation)
C
C DESCRIPTION      Decodes the GPS Test Instrumentation message and returns
C                  the decoded information as shown below.  This message decode
C                  assumes that the Test Instrumentation message contains the 
C                  data defined in the GPS_CODE_MESS5 subroutine, namely
C                  HDOP, PDOP, OPT, ALT, ELEV and AZIM.  Note that message 5
C                  must have been sent at least once to the GPS system before
C                  asking for and decoding message 31.
C
C                  Note that the order of the SVs returned in message 31 may
C                  not be the same as that in other messages.  Also note that
C                  the conversions into degrees for elevation and azimuth are
C                  approximate, since the exact conversion algorithm has not
C                  been established.
C
C VERSION          1.00  12-05-92  W.D.N.JACKSON
C
C ARGUMENTS        IMESS(50)   I*2  IN   GPS message 31, incl header words
C                  RHDOP       R*4  OUT  Horizontal dilution of precision
C                  RVDOP       R*4  OUT  Vertical dilution of precision
C                  RPDOP       R*4  OUT  Position dilution of precision
C                  ISV(8)      I*4  OUT  Satellite list - first the four
C                                        optimum SVs, then up to 4 alternate SVs
C                  RELEV(8)    R*4  OUT  The elevation of each SV
C                  RAZIM(8)    R*4  OUT  The azimuth of each SV
C
C*******************************************************************************
      SUBROUTINE GPS1_DECODE_MESS31(IMESS,RHDOP,RVDOP,RPDOP,ISV,RELEV,
     -    RAZIM)
      IMPLICIT  NONE
      INTEGER*2 IMESS(-4:45)
      REAL*4    RHDOP,RVDOP,RPDOP,RELEV(8),RAZIM(8)
      INTEGER*4 ISV(8),I

      CALL GPS1_DECODE_REAL(IMESS(1),RHDOP) !Get HDOP
      CALL GPS1_DECODE_REAL(IMESS(3),RPDOP) !Get PDOP
      RVDOP=0.
      IF(RPDOP.GT.RHDOP) RVDOP=SQRT(RPDOP**2-RHDOP**2) !Compute VDOP
      DO I=1,8
        ISV(I)=IMESS(4+I)                            !Get satellite numbers
        CALL GPS1_DECODE_REAL(IMESS(11+2*I),RELEV(I)) !Get elevations
        RELEV(I)=ASIND(RELEV(I))                     !Convert to degrees
        CALL GPS1_DECODE_REAL(IMESS(27+2*I),RAZIM(I)) !Get azimuths
        RAZIM(I)=RAZIM(I)*180.0/3.141592654          !Convert from radians
        IF(RAZIM(I).LT.0.) RAZIM(I)=RAZIM(I)+360.    !Make positive
      END DO
      RETURN
      END
C*******************************************************************************
C
C ROUTINE          GPS1_DUMP_MESS SUBROUTINE FORTVAX
C
C PURPOSE          Produces a one line dump of a GPS message
C
C DESCRIPTION      Decodes a GPS message and produces a one line listing of its
C                  contents on SYS$OUTPUT. It uses a full 132 column width
C                  and displays as much of the message information as possible.
C                  If called with CMODE equal to 'HEADER' it puts up a header
C                  line, otherwise it displays the data.  Only works with
C                  message type 0(DRS),1,24,18,20 and 31.  Note that the logical
C                  unit must have been opened with fortran carriage control.
C
C VERSION          1.00  13-09-93  W.D.N.JACKSON
C
C ARGUMENTS        LUN         I*4  IN   Logical unit for output to VT320
C                  IMESS(103)  I*2  IN   Full GPS message 0,1,24,18,20 or 31
C                  CMODE       C**  IN   'HEADER' or 'DATA'
C
C*******************************************************************************
      SUBROUTINE GPS1_DUMP_MESS(LUN,IMESS,CMODE)
      IMPLICIT  NONE
      INTEGER*4 IRECTYP,ITRKRS,I,IALTHLD,IAOLD,ICDU,IDATUM,IDYNAM,IFOM,
     -    IINIT,IMAINTSUM,IOS,IRECID,ITYPE,IWKNUM,LUN
      REAL*4    RINILAT,RINILNG,RALTMSL,RALTREF,RCLKBIAS,RCLKDRFT,RHDOP,
     -    RHEPE,RINIALT,RLAT,RLONG,RPDOP,RTIME,RUTCCOR,RVDOP,RVEPE,
     -    RGRPDEL1
      INTEGER*2 IMESS(103)
      CHARACTER CMODE*(*)
      CHARACTER CTIME*11,CINIDAT*9,CINITIM*8,CFLTNO*4,CDATE*9,CTIME1*8
      CHARACTER CSOFT*4,CUTC*23
      CHARACTER*131 CHEAD0,CHEAD1,CHEAD24,CHEAD18,CHEAD20,CHEAD31
      INTEGER*4 ISV(4),ITRKSTS(4),ICHSTS(4),ISVIDS(4),ICNO(4),IODE(4)
      INTEGER*4 ISATS(8),IEPHEM(4)
      REAL*4    RV(3),RPSR(4),RDR(4),RTROPCOR(4),
     -          RIONOCOR(4),RELEV(8),RAZIM(8)
      DATA CHEAD0/'00 FNUM   FLTDATE    FLTTIME'/
      DATA CHEAD1(1:40) /'01 Recid Ini S1 S2 S3 S4 Ini date  Ini t'/
      DATA CHEAD1(41:80) /'ime Ini latit Ini longi Inial'/
      DATA CHEAD24(1:40) /'24    Time     D D S1 S2 S3 S4 T T T T F'/
      DATA CHEAD24(41:80) /'m   VEPE   HEPE  Latitude  Longitud  Alt'/
      DATA CHEAD24(81:120) /'msl  Altref     VE     VN     VU'/
      DATA CHEAD18(1:40) /'18    Time     Recid   Utime       Psr1 '/
      DATA CHEAD18(41:80) /'      Psr2       Psr3       Psr4   Delr1'/
      DATA CHEAD18(81:120) /' Delr2 Delr3 Delr4 T T T T S1 S2 S3 S4 E'/
      DATA CHEAD18(121:131) /'1 E2 E3 E4'/
      DATA CHEAD20(1:40) /'20    Time     A Utc S1 S2 S3 S4 C1 C2 C'/
      DATA CHEAD20(41:80) /'3 C4 Trop1 Trop2 Trop3 Trop4 Iono1 Iono2'/
      DATA CHEAD20(81:120) /' Iono3 Iono4  A1  A2  A3  A4  Wkn  Bias '/
      DATA CHEAD20(121:131) /' Drift  T'/
      DATA CHEAD31(1:40) /'31  HDOP VDOP PDOP  SV Elev Azim  SV Ele'/
      DATA CHEAD31(41:80) /'v Azim  SV Elev Azim  SV Elev Azim  SV E'/
      DATA CHEAD31(81:120) /'lev Azim  SV Elev Azim  SV Elev Azim'/
     
      ITYPE=0
      CALL GPS1_MVBITS_WL(IMESS(1),4,5,ITYPE,0)
      IF(CMODE.EQ.'HEADER') THEN
        IF(ITYPE.EQ.0) WRITE(LUN,10,IOSTAT=IOS) CHEAD0
        IF(ITYPE.EQ.1) WRITE(LUN,10,IOSTAT=IOS) CHEAD1
        IF(ITYPE.EQ.24) WRITE(LUN,10,IOSTAT=IOS) CHEAD24
        IF(ITYPE.EQ.18) WRITE(LUN,10,IOSTAT=IOS) CHEAD18
        IF(ITYPE.EQ.20) WRITE(LUN,10,IOSTAT=IOS) CHEAD20
        IF(ITYPE.EQ.31) WRITE(LUN,10,IOSTAT=IOS) CHEAD31
      ELSE IF(CMODE.EQ.'DATA') THEN
        IF(ITYPE.EQ.0) THEN
          CALL GPS1_DECODE_MESS0(IMESS,CFLTNO,CDATE,CTIME1)
          WRITE(LUN,10,IOSTAT=IOS) '00 '//CFLTNO//'  '//CDATE//'  '
     -        //CTIME1
        ELSE IF(ITYPE.EQ.1) THEN
          CALL GPS1_DECODE_MESS1(IMESS,IRECID,IINIT,ISV,CINIDAT,CINITIM,
     -        RINILAT,RINILNG,RINIALT)
          WRITE(LUN,1,IOSTAT=IOS) 1,IRECID,IINIT,(ISV(I),I=1,4),CINIDAT,
     -        CINITIM,RINILAT,RINILNG,RINIALT
        ELSE IF(ITYPE.EQ.24) THEN
          CALL GPS1_DECODE_MESS24(IMESS,CTIME,IRECID,IALTHLD,IDATUM,ISV,
     -        ITRKSTS,IFOM,RVEPE,RHEPE,RLAT,RLONG,RALTMSL,RALTREF,RV)
          WRITE(LUN,24,IOSTAT=IOS) 24,CTIME,IALTHLD,IDATUM,
     -        (ISV(I),I=1,4),(ITRKSTS(I),I=1,4),IFOM,RVEPE,RHEPE,RLAT,
     -        RLONG,RALTMSL,RALTREF,(RV(I),I=1,3)
        ELSE IF(ITYPE.EQ.18) THEN
          CALL GPS1_DECODE_MESS18(IMESS,CTIME,IRECID,RTIME,RPSR,RDR,
     -        ICHSTS,ISV,IEPHEM)
          WRITE(LUN,18,IOSTAT=IOS) 18,CTIME,IRECID,RTIME,
     -        (RPSR(I),I=1,4),(RDR(I),I=1,4),(ICHSTS(I),I=1,4),
     -        (ISV(I),I=1,4),(IEPHEM(I),I=1,4)
        ELSE IF(ITYPE.EQ.20) THEN
          CALL GPS1_DECODE_MESS20(IMESS,CTIME,IRECID,IAOLD,
     -        RUTCCOR,ISVIDS,ICNO,RTROPCOR,RIONOCOR,IODE,IWKNUM,
     -        RCLKBIAS,RCLKDRFT,IMAINTSUM,IDYNAM,RGRPDEL1,ITRKRS,
     -        IRECTYP,ICDU,CSOFT,CUTC)
          WRITE(LUN,20,IOSTAT=IOS) 20,CTIME,IAOLD,NINT(RUTCCOR),
     -        (ISVIDS(I),I=1,4),(ICNO(I),I=1,4),(RTROPCOR(I),I=1,4),
     -        (RIONOCOR(I),I=1,4),(IODE(I),I=1,4),IWKNUM,
     -        RCLKBIAS/1000,RCLKDRFT,ITRKRS
        ELSE IF(ITYPE.EQ.31) THEN
          CALL GPS1_DECODE_MESS31(IMESS,RHDOP,RVDOP,RPDOP,ISATS,RELEV,
     -        RAZIM)
          WRITE(LUN,31,IOSTAT=IOS) 31,RHDOP,RVDOP,RPDOP,
     -        ((ISATS(I),RELEV(I),RAZIM(I)),I=1,7)
        END IF
      END IF
      RETURN
10    FORMAT(X,A)
1     FORMAT(X,I2.2,X,I5.5,I4.3,4I3.2,2(X,A),2F10.4,I6.5)
24    FORMAT(X,I2.2,X,A,X,I1,X,I1,X,4(I2.2,X),4(I1,X),I2,X,2(F6.1,X),
     -    2(F9.4,X),2(F7.1,X),2(F6.1,X),F6.2)
18    FORMAT(X,I2.2,X,A,X,I5.5,X,F7.3,X,4(F10.0,X),2X,4(F5.0,X),
     -    4(I1,X),4(I2.2,X),4(I2.1,X))
20    FORMAT(X,I2.2,X,A,X,I1,X,I3,X,8(I2.2,X),8(F5.1,X),4(I3.3,X),
     -    I4.3,2(F6.1,X),I2)
31    FORMAT(X,I2.2,X,3F5.1,8(I4.2,F5.1,F6.1))
      END
C*******************************************************************************
C
C ROUTINE          GPS1_DISP_MESS SUBROUTINE FORTVAX
C
C PURPOSE          Displays GPS messages on VT320 terminals
C
C DESCRIPTION      Establishes a display of GPS information and updates it
C                  as new messages are received.  If called with CMODE equal to
C                  BACKGROUND, clears the screen and puts up the legend. The
C                  screen display is split into 3 roughly equal sections,
C                  containing the data corresponding to messagess 24,18,20 & 31.
C                  If called with CMODE equal to UPDATE, and a GPS message with
C                  a number of 0, 24, 18, 20 or 31, then updates the information
C                  in the corresponding part of the screen.  Only displays the 
C                  information returned in the messages, except that it also 
C                  computes and displays the ground speed and heading, and the
C                  distance east and north of Boscombe Down.  Note that the
C                  logical unit must have been opened with fortran carriage
C                  control.
C
C VERSION          1.00  13-09-93  W.D.N.JACKSON
C
C ARGUMENTS        L           I*4  IN   Logical unit for output to VT320
C                  IMESS(103)  I*2  IN   Full GPS message 0,24,18,20 or 31
C                  CMODE       C**  IN   'BACKGROUND' or 'UPDATE'
C
C CHANGES          1.01  11-04-94  W.D.N.JACKSON
C                  Now uses Boscombe Down as a reference
C                  1.02  19-06-02  W.D.N.JACKSON
C                  Now uses Woodford as a reference
C
C*******************************************************************************
      SUBROUTINE GPS1_DISP_MESS(L,IMESS,CMODE)
      IMPLICIT  NONE
      INTEGER*4 IDATUM,IDYNAM,IFOM,IL,ILAT,ILATD,ILATM,ILATS,ILNG,ILNGD,
     -    I,IALTHLD,IAOLD,ICDU,IRECID,IRECTYP,ITRKRS,IWKNUM,J,L,ILNGM,
     -    ILNGS,IMAINTSUM,IMESSNO
      REAL*4   RALTMSL,RALTREF,RCLKBIAS,RCLKDRFT,RGS,RHDG,RHDOP,RHEPE,
     -    RLAT,RLONG,RNORTH,RPDOP,RTIME,RUTCCOR,REAST,RFNHLAT,RFNHLNG,
     -    RGRPDEL1,RVDOP,RVEPE
      INTEGER*2 IMESS(103)
      CHARACTER CMODE*(*),CLAT*1,CLNG*1
      CHARACTER CTIME*11,CE*3,CDATE*9,CTIME1*8,CFLTNO*4,CSOFT*4,CUTC*23
      INTEGER*4 ISV(4),ITRKSTS(4),ICHSTS(4),ISVIDS(4),ICNO(4),IODE(4)
      INTEGER*4 ISATS(8),IEPHEM(4)
      REAL*4    RV(3),RPSR(4),RDR(4),RTROPCOR(4),
     -          RIONOCOR(4),RELEV(8),RAZIM(8),RETMP(4),RATMP(4)
      CHARACTER CLINES(24)*79
      DATA CLINES(1)(1:40)/'                                        '/
      DATA CLINES(2)(1:40)/'M24 GPS time                            '/
      DATA CLINES(3)(1:40)/'    Lat/Long                            '/
      DATA CLINES(4)(1:40)/'    MSL/Ref                         m   '/
      DATA CLINES(5)(1:40)/'    Vel ENU                         m/s '/
      DATA CLINES(6)(1:40)/'    HEPE/VEPE                       m   '/
      DATA CLINES(7)(1:40)/'    GS/Hdg                          m/s,'/
      DATA CLINES(8)(1:40)/'    E/N BS                          km  '/
      DATA CLINES(9)(1:40)/'                                        '/
      DATA CLINES(10)(1:40)/'M18 SV                                  '/
      DATA CLINES(11)(1:40)/'    Pseud rng                           '/
      DATA CLINES(12)(1:40)/'    Delta rng                           '/
      DATA CLINES(13)(1:40)/'    Ephem/Sts                           '/
      DATA CLINES(14)(1:40)/'                                        '/
      DATA CLINES(15)(1:40)/'M20 SV                                  '/ 
      DATA CLINES(16)(1:40)/'    Carr/noi                            '/
      DATA CLINES(17)(1:40)/'    Trop Cor                            '/
      DATA CLINES(18)(1:40)/'    Iono Cor                            '/
      DATA CLINES(19)(1:40)/'    IODE Cnt                            '/
      DATA CLINES(20)(1:40)/'                                        '/
      DATA CLINES(21)(1:40)/'M31 Elevation                           '/
      DATA CLINES(22)(1:40)/'    Azimuth                             '/
      DATA CLINES(23)(1:40)/'                                        '/
      DATA CLINES(1)(41:79)/'                                       '/
      DATA CLINES(2)(41:79)/' SV                    Nav degraded    '/
      DATA CLINES(3)(41:79)/' Noncoher              Altitude held   '/
      DATA CLINES(4)(41:79)/' Diff cor              Recvr fault     '/
      DATA CLINES(5)(41:79)/' Coherent              Recvr ID        '/
      DATA CLINES(6)(41:79)/'          HDOP         Recvr type  XR5M'/
      DATA CLINES(7)(41:79)/'deg       VDOP         Geo datum       '/
      DATA CLINES(8)(41:79)/'          PDOP                         '/
      DATA CLINES(9)(41:79)/'                                       '/
      DATA CLINES(10)(41:79)/'                                       '/
      DATA CLINES(11)(41:79)/'              m                        '/
      DATA CLINES(12)(41:79)/'              m                        '/    
      DATA CLINES(13)(41:79)/'                 GPS date              '/
      DATA CLINES(14)(41:79)/'                 UTC time              '/
      DATA CLINES(15)(41:79)/'                 Trkrs/Dynam           '/
      DATA CLINES(16)(41:79)/'     db-Hz       Old almanac           '/
      DATA CLINES(17)(41:79)/'         m       UTC offset         s  '/
      DATA CLINES(18)(41:79)/'         m       Week number           '/
      DATA CLINES(19)(41:79)/'                 Clock bias         km '/
      DATA CLINES(20)(41:79)/'                 Clock drift        m/s'/
      DATA CLINES(21)(41:79)/'     deg         Softw/Rec             '/
      DATA CLINES(22)(41:79)/'     deg         Maint/CDU             '/
      DATA CLINES(23)(41:79)/'                                       '/
      DATA RFNHLAT /53.37133/ !Woodford pan
      DATA RFNHLNG /-2.14883/

      CE=CHAR(0)//CHAR(27)//'['
      IF(CMODE.EQ.'BACKGROUND') THEN
        WRITE(L,10,IOSTAT=I) CE//'2J'//CHAR(27)//'[1;1H'
        DO IL=1,23
          WRITE(L,10,IOSTAT=I) CHAR(0)//CLINES(IL)//CHAR(13)//CHAR(10)
        END DO
      END IF
      IF(CMODE.EQ.'UPDATE') THEN
        CALL GPS1_ANALYS_MESS(IMESS,IMESSNO)
        IF(IMESSNO.EQ.0) THEN
          CALL GPS1_DECODE_MESS0(IMESS,CFLTNO,CDATE,CTIME1)
          WRITE(L,10,IOSTAT=I) CE//'1;1H'//CFLTNO//'  '//CDATE//'  '//
     -        CTIME1
        ELSE IF(IMESSNO.EQ.24) THEN
          CALL GPS1_DECODE_MESS24(IMESS,CTIME,IRECID,IALTHLD,IDATUM,ISV,
     -        ITRKSTS,IFOM,RVEPE,RHEPE,RLAT,RLONG,RALTMSL,RALTREF,RV)
          RGS=SQRT(RV(1)**2+RV(2)**2)
          RHDG=0.0
          IF(RV(1).NE.0..OR.RV(2).NE.0.) THEN
            RHDG=ATAN2D(RV(1),RV(2))
            IF(RHDG.LT.0.) RHDG=RHDG+360.0
          END IF
          CALL GPS1_DISTEN(RFNHLNG,RFNHLAT,RLONG,RLAT,REAST,RNORTH)
          CLAT='N'
          IF(RLAT.LT.0.) THEN
            CLAT='S'
            RLAT=-RLAT
          END IF
          CLNG='E'
          IF(RLONG.LT.0.) THEN
            CLNG='W'
            RLONG=-RLONG
          END IF
          ILNG=RLONG*3600
          ILNGD=ILNG/3600                !Degrees
          ILNGM=MOD(ILNG/60,60)          !Minutes
          ILNGS=(MOD(ILNG,60)*100)/60    !Hundreths of minutes
          ILAT=RLAT*3600
          ILATD=ILAT/3600
          ILATM=MOD(ILAT/60,60)
          ILATS=(MOD(ILAT,60)*100)/60
          WRITE(L,20,IOSTAT=I) CE//'2;15H',CTIME
          WRITE(L,23,IOSTAT=I) CE//'3;14H',ILATD,ILATM,ILATS,CLAT
          WRITE(L,23,IOSTAT=I) CE//'3;27H',ILNGD,ILNGM,ILNGS,CLNG
          WRITE(L,26,IOSTAT=I) CE//'4;15H',RALTMSL,RALTREF
          WRITE(L,29,IOSTAT=I) CE//'5;16H',(RV(J),J=1,3)
          WRITE(L,34,IOSTAT=I) CE//'6;16H',RHEPE,RVEPE
          WRITE(L,36,IOSTAT=I) CE//'7;16H',RGS,RHDG
          WRITE(L,37,IOSTAT=I) CE//'8;15H',REAST,RNORTH
          WRITE(L,21,IOSTAT=I) CE//'2;50H',(ISV(J),J=1,4)
          WRITE(L,24,IOSTAT=I) CE//'3;50H',(IBITS(ITRKSTS(J),0,1),J=1,4)
          WRITE(L,27,IOSTAT=I) CE//'4;50H',(IBITS(ITRKSTS(J),1,1),J=1,4)
          WRITE(L,30,IOSTAT=I) CE//'5;50H',(IBITS(ITRKSTS(J),2,1),J=1,4)
          WRITE(L,22,IOSTAT=I) CE//'2;79H',IBITS(IFOM,0,1)
          WRITE(L,35,IOSTAT=I) CE//'3;79H',IALTHLD
          WRITE(L,22,IOSTAT=I) CE//'4;79H',IBITS(IFOM,3,1)
          WRITE(L,31,IOSTAT=I) CE//'5;75H',IRECID
          WRITE(L,33,IOSTAT=I) CE//'7;78H',IDATUM
        ELSE IF(IMESSNO.EQ.18) THEN
          CALL GPS1_DECODE_MESS18(IMESS,CTIME,IRECID,RTIME,RPSR,RDR,
     -        ICHSTS,ISV,IEPHEM)
          WRITE(L,42,IOSTAT=I) CE//'10;19H',(ISV(J),J=1,4)
          WRITE(L,39,IOSTAT=I) CE//'11;15H',(NINT(RPSR(J)),J=1,4)
          WRITE(L,40,IOSTAT=I) CE//'12;15H',(RDR(J),J=1,4)
          WRITE(L,41,IOSTAT=I) CE//'13;15H',
     -        (((IEPHEM(J)),ICHSTS(J)),J=1,4)
        ELSE IF(IMESSNO.EQ.20) THEN
          CALL GPS1_DECODE_MESS20(IMESS,CTIME,IRECID,IAOLD,
     -        RUTCCOR,ISVIDS,ICNO,RTROPCOR,RIONOCOR,IODE,IWKNUM,
     -        RCLKBIAS,RCLKDRFT,IMAINTSUM,IDYNAM,RGRPDEL1,ITRKRS,
     -        IRECTYP,ICDU,CSOFT,CUTC)
          WRITE(L,44,IOSTAT=I) CE//'15;15H',(ISVIDS(J),J=1,4)
          WRITE(L,46,IOSTAT=I) CE//'16;15H',(ICNO(J),J=1,4)
          WRITE(L,48,IOSTAT=I) CE//'17;15H',(RTROPCOR(J),J=1,4)
          WRITE(L,50,IOSTAT=I) CE//'18;15H',(RIONOCOR(J),J=1,4)
          WRITE(L,52,IOSTAT=I) CE//'19;15H',(IODE(J),J=1,4)
          WRITE(L,56,IOSTAT=I) CE//'13;67H',CUTC(1:7)//CUTC(10:11)
          WRITE(L,57,IOSTAT=I) CE//'14;68H',CUTC(13:20)
          WRITE(L,43,IOSTAT=I) CE//'15;73H',ITRKRS,IDYNAM
          WRITE(L,45,IOSTAT=I) CE//'16;71H',IAOLD
          WRITE(L,47,IOSTAT=I) CE//'17;71H',NINT(RUTCCOR)
          WRITE(L,49,IOSTAT=I) CE//'18;71H',IWKNUM
          WRITE(L,51,IOSTAT=I) CE//'19;69H',RCLKBIAS/1000.
          WRITE(L,53,IOSTAT=I) CE//'20;71H',RCLKDRFT
          WRITE(L,54,IOSTAT=I) CE//'21;70H',CSOFT,IRECTYP
          WRITE(L,55,IOSTAT=I) CE//'22;70H',IMAINTSUM,ICDU
        ELSE IF(IMESSNO.EQ.31) THEN
          CALL GPS1_DECODE_MESS31(IMESS,RHDOP,RVDOP,RPDOP,ISATS,RELEV,
     -        RAZIM)
          WRITE(L,51,IOSTAT=I) CE//'6;56H',RHDOP
          WRITE(L,51,IOSTAT=I) CE//'7;56H',RVDOP
          WRITE(L,51,IOSTAT=I) CE//'8;56H',RPDOP
          DO I=1,4
            RETMP(I)=0.
            RATMP(I)=0.
          END DO
          DO I=1,4                       !Sort into same sat order as for M21
            DO J=1,4
              IF(ISATS(J).EQ.ISVIDS(I)) THEN
                RETMP(I)=RELEV(J)
                RATMP(I)=RAZIM(J)
              END IF
            END DO
          END DO
          WRITE(L,48,IOSTAT=I) CE//'21;15H',(RETMP(J),J=1,4)
          WRITE(L,48,IOSTAT=I) CE//'22;15H',(RATMP(J),J=1,4)
        END IF
      END IF
      RETURN
10    FORMAT(A)
20    FORMAT(A,A)
21    FORMAT(A,I2,3(X,I2))
22    FORMAT(A,I1)
23    FORMAT(A,I3,'''',I2.2,'.',I2.2,X,A)
24    FORMAT(A,I2,3I3)
25    FORMAT(A,I1)
26    FORMAT(A,F7.1,3X,F7.1)
27    FORMAT(A,I2,3I3)
28    FORMAT(A,I1)
29    FORMAT(A,F6.1,X,F6.1,X,F6.2)
30    FORMAT(A,I2,3I3)
31    FORMAT(A,I5)
32    FORMAT(A,I10,3(2X,I10))
33    FORMAT(A,I2)
34    FORMAT(A,F6.1,4X,F6.1)
35    FORMAT(A,I1)
36    FORMAT(A,F6.1,4X,F6.1)
37    FORMAT(A,F9.3,X,F9.3)
38    FORMAT(A,F10.1,3(X,F10.1))
39    FORMAT(A,I9,3(X,I9))
40    FORMAT(A,F9.2,3(1X,F9.2))
41    FORMAT(A,I5,' /',I2,3(I6,' /',I2))
42    FORMAT(A,I5,3(X,I9))
43    FORMAT(A,I1,'/',I1)
44    FORMAT(A,I6,3(2X,I6))
45    FORMAT(A,I5)
46    FORMAT(A,I6,3(2X,I6))
47    FORMAT(A,I5)
48    FORMAT(A,F6.1,3(2X,F6.1))
49    FORMAT(A,I5)
50    FORMAT(A,F6.1,3(2X,F6.1))
51    FORMAT(A,F7.1)
52    FORMAT(A,I6,3(2X,I6))
53    FORMAT(A,F5.1)
54    FORMAT(A,A,'/',I1)
55    FORMAT(A,Z4.4,'/',I1)
56    FORMAT(A,A)
57    FORMAT(A,A)
      END
C*******************************************************************************
C
C ROUTINE          GPS1_DISP_MESS_VT SUBROUTINE FORTVAX
C
C PURPOSE          Displays GPS messages on VT320 terminals
C
C DESCRIPTION      Establishes a display of GPS information and updates it
C                  as new messages are received.  If called with CMODE equal to
C                  BACKGROUND, clears the screen and puts up the legend. The
C                  screen display is split into 3 roughly equal sections,
C                  containing the data corresponding to messagess 24,18,20 & 31.
C                  If called with CMODE equal to UPDATE, and a GPS message with
C                  a number of 0, 24, 18, 20 or 31, then updates the information
C                  in the corresponding part of the screen.  Only displays the 
C                  information returned in the messages, except that it also 
C                  computes and displays the ground speed and heading, and the
C                  distance east and north of Boscombe Down.  Note that the
C                  logical unit must have been opened with fortran carriage
C                  control.
C
C VERSION          1.00  13-09-93  W.D.N.JACKSON
C
C ARGUMENTS        IMESS(103)  I*2  IN   Full GPS message 0,24,18,20 or 31
C                  CMODE       C**  IN   'BACKGROUND' or 'UPDATE'
C
C CHANGES          1.01  11-04-94  W.D.N.JACKSON
C                  Now uses Boscombe Down as a reference
C                  1.02  19-06-02  W.D.N.JACKSON
C                  Now uses Woodford as a reference
C
C*******************************************************************************
      SUBROUTINE GPS1_DISP_MESS_VT(IMESS,CMODE)
      IMPLICIT  NONE
      INTEGER*4 IDATUM,IDYNAM,IFOM,IL,ILAT,ILATD,ILATM,ILATS,ILNG,ILNGD,
     -    I,IALTHLD,IAOLD,ICDU,IRECID,IRECTYP,ITRKRS,IWKNUM,J,ILNGM,
     -    ILNGS,IMAINTSUM,IMESSNO,LAN
      REAL*4   RALTMSL,RALTREF,RCLKBIAS,RCLKDRFT,RGS,RHDG,RHDOP,RHEPE,
     -    RLAT,RLONG,RNORTH,RPDOP,RTIME,RUTCCOR,REAST,RFNHLAT,RFNHLNG,
     -    RGRPDEL1,RVDOP,RVEPE
      INTEGER*2 IMESS(103)
      CHARACTER CMODE*(*),CLAT*1,CLNG*1
      CHARACTER CTIME*11,CE*3,CDATE*9,CTIME1*8,CFLTNO*4,CSOFT*4,CUTC*23
      INTEGER*4 ISV(4),ITRKSTS(4),ICHSTS(4),ISVIDS(4),ICNO(4),IODE(4)
      INTEGER*4 ISATS(8),IEPHEM(4)
      REAL*4    RV(3),RPSR(4),RDR(4),RTROPCOR(4),
     -          RIONOCOR(4),RELEV(8),RAZIM(8),RETMP(4),RATMP(4)
      CHARACTER CLINES(24)*79,CL*80
      DATA CLINES(1)(1:40)/'                                        '/
      DATA CLINES(2)(1:40)/'M24 GPS time                            '/
      DATA CLINES(3)(1:40)/'    Lat/Long                            '/
      DATA CLINES(4)(1:40)/'    MSL/Ref                         m   '/
      DATA CLINES(5)(1:40)/'    Vel ENU                         m/s '/
      DATA CLINES(6)(1:40)/'    HEPE/VEPE                       m   '/
      DATA CLINES(7)(1:40)/'    GS/Hdg                          m/s,'/
      DATA CLINES(8)(1:40)/'    E/N BS                          km  '/
      DATA CLINES(9)(1:40)/'                                        '/
      DATA CLINES(10)(1:40)/'M18 SV                                  '/
      DATA CLINES(11)(1:40)/'    Pseud rng                           '/
      DATA CLINES(12)(1:40)/'    Delta rng                           '/
      DATA CLINES(13)(1:40)/'    Ephem/Sts                           '/
      DATA CLINES(14)(1:40)/'                                        '/
      DATA CLINES(15)(1:40)/'M20 SV                                  '/ 
      DATA CLINES(16)(1:40)/'    Carr/noi                            '/
      DATA CLINES(17)(1:40)/'    Trop Cor                            '/
      DATA CLINES(18)(1:40)/'    Iono Cor                            '/
      DATA CLINES(19)(1:40)/'    IODE Cnt                            '/
      DATA CLINES(20)(1:40)/'                                        '/
      DATA CLINES(21)(1:40)/'M31 Elevation                           '/
      DATA CLINES(22)(1:40)/'    Azimuth                             '/
      DATA CLINES(23)(1:40)/'                                        '/
      DATA CLINES(1)(41:79)/'                                       '/
      DATA CLINES(2)(41:79)/' SV                    Nav degraded    '/
      DATA CLINES(3)(41:79)/' Noncoher              Altitude held   '/
      DATA CLINES(4)(41:79)/' Diff cor              Recvr fault     '/
      DATA CLINES(5)(41:79)/' Coherent              Recvr ID        '/
      DATA CLINES(6)(41:79)/'          HDOP         Recvr type  XR5M'/
      DATA CLINES(7)(41:79)/'deg       VDOP         Geo datum       '/
      DATA CLINES(8)(41:79)/'          PDOP                         '/
      DATA CLINES(9)(41:79)/'                                       '/
      DATA CLINES(10)(41:79)/'                                       '/
      DATA CLINES(11)(41:79)/'              m                        '/
      DATA CLINES(12)(41:79)/'              m                        '/    
      DATA CLINES(13)(41:79)/'                 GPS date              '/
      DATA CLINES(14)(41:79)/'                 UTC time              '/
      DATA CLINES(15)(41:79)/'                 Trkrs/Dynam           '/
      DATA CLINES(16)(41:79)/'     db-Hz       Old almanac           '/
      DATA CLINES(17)(41:79)/'         m       UTC offset         s  '/
      DATA CLINES(18)(41:79)/'         m       Week number           '/
      DATA CLINES(19)(41:79)/'                 Clock bias         km '/
      DATA CLINES(20)(41:79)/'                 Clock drift        m/s'/
      DATA CLINES(21)(41:79)/'     deg         Softw/Rec             '/
      DATA CLINES(22)(41:79)/'     deg         Maint/CDU             '/
      DATA CLINES(23)(41:79)/'                                       '/
      DATA RFNHLAT /53.37133/ !Woodford pan
      DATA RFNHLNG /-2.14883/

      CE=CHAR(0)//CHAR(27)//'['
      IF(CMODE.EQ.'BACKGROUND') THEN
        CALL VT_OUT(CE//'2J'//CHAR(27)//'[1;1H')
        DO IL=1,23
          CALL VT_OUT(CHAR(0)//CLINES(IL)//CHAR(13)//CHAR(10))
        END DO
      END IF
      IF(CMODE.EQ.'UPDATE') THEN
        CALL GPS1_ANALYS_MESS(IMESS,IMESSNO)
        IF(IMESSNO.EQ.0) THEN
          CALL GPS1_DECODE_MESS0(IMESS,CFLTNO,CDATE,CTIME1)
          CALL VT_OUT(CE//'1;1H'//CFLTNO//'  '//CDATE//'  '//CTIME1)
        ELSE IF(IMESSNO.EQ.24) THEN
          CALL GPS1_DECODE_MESS24(IMESS,CTIME,IRECID,IALTHLD,IDATUM,ISV,
     -        ITRKSTS,IFOM,RVEPE,RHEPE,RLAT,RLONG,RALTMSL,RALTREF,RV)
          RGS=SQRT(RV(1)**2+RV(2)**2)
          RHDG=0.0
          IF(RV(1).NE.0..OR.RV(2).NE.0.) THEN
            RHDG=ATAN2D(RV(1),RV(2))
            IF(RHDG.LT.0.) RHDG=RHDG+360.0
          END IF
          CALL GPS1_DISTEN(RFNHLNG,RFNHLAT,RLONG,RLAT,REAST,RNORTH)
          CLAT='N'
          IF(RLAT.LT.0.) THEN
            CLAT='S'
            RLAT=-RLAT
          END IF
          CLNG='E'
          IF(RLONG.LT.0.) THEN
            CLNG='W'
            RLONG=-RLONG
          END IF
          ILNG=RLONG*3600
          ILNGD=ILNG/3600                !Degrees
          ILNGM=MOD(ILNG/60,60)          !Minutes
          ILNGS=(MOD(ILNG,60)*100)/60    !Hundreths of minutes
          ILAT=RLAT*3600
          ILATD=ILAT/3600
          ILATM=MOD(ILAT/60,60)
          ILATS=(MOD(ILAT,60)*100)/60
          WRITE(CL,20,IOSTAT=I) CE//'2;15H',CTIME
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,23,IOSTAT=I) CE//'3;14H',ILATD,ILATM,ILATS,CLAT
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,23,IOSTAT=I) CE//'3;27H',ILNGD,ILNGM,ILNGS,CLNG
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,26,IOSTAT=I) CE//'4;15H',RALTMSL,RALTREF
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,29,IOSTAT=I) CE//'5;16H',(RV(J),J=1,3)
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,34,IOSTAT=I) CE//'6;16H',RHEPE,RVEPE
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,36,IOSTAT=I) CE//'7;16H',RGS,RHDG
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,37,IOSTAT=I) CE//'8;15H',REAST,RNORTH
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,21,IOSTAT=I) CE//'2;50H',(ISV(J),J=1,4)
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,24,IOSTAT=I) CE//'3;50H',
     -        (IBITS(ITRKSTS(J),0,1),J=1,4)
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,27,IOSTAT=I) CE//'4;50H',
     -        (IBITS(ITRKSTS(J),1,1),J=1,4)
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,30,IOSTAT=I) CE//'5;50H',
     -        (IBITS(ITRKSTS(J),2,1),J=1,4)
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,22,IOSTAT=I) CE//'2;79H',IBITS(IFOM,0,1)
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,35,IOSTAT=I) CE//'3;79H',IALTHLD
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,22,IOSTAT=I) CE//'4;79H',IBITS(IFOM,3,1)
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,31,IOSTAT=I) CE//'5;75H',IRECID
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,33,IOSTAT=I) CE//'7;78H',IDATUM
          CALL VT_OUT(CL(1:LAN(CL)))
        ELSE IF(IMESSNO.EQ.18) THEN
          CALL GPS1_DECODE_MESS18(IMESS,CTIME,IRECID,RTIME,RPSR,RDR,
     -        ICHSTS,ISV,IEPHEM)
          WRITE(CL,42,IOSTAT=I) CE//'10;19H',(ISV(J),J=1,4)
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,39,IOSTAT=I) CE//'11;15H',(NINT(RPSR(J)),J=1,4)
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,40,IOSTAT=I) CE//'12;15H',(RDR(J),J=1,4)
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,41,IOSTAT=I) CE//'13;15H',
     -        (((IEPHEM(J)),ICHSTS(J)),J=1,4)
          CALL VT_OUT(CL(1:LAN(CL)))
        ELSE IF(IMESSNO.EQ.20) THEN
          CALL GPS1_DECODE_MESS20(IMESS,CTIME,IRECID,IAOLD,
     -        RUTCCOR,ISVIDS,ICNO,RTROPCOR,RIONOCOR,IODE,IWKNUM,
     -        RCLKBIAS,RCLKDRFT,IMAINTSUM,IDYNAM,RGRPDEL1,ITRKRS,
     -        IRECTYP,ICDU,CSOFT,CUTC)
          WRITE(CL,44,IOSTAT=I) CE//'15;15H',(ISVIDS(J),J=1,4)
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,46,IOSTAT=I) CE//'16;15H',(ICNO(J),J=1,4)
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,48,IOSTAT=I) CE//'17;15H',(RTROPCOR(J),J=1,4)
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,50,IOSTAT=I) CE//'18;15H',(RIONOCOR(J),J=1,4)
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,52,IOSTAT=I) CE//'19;15H',(IODE(J),J=1,4)
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,56,IOSTAT=I) CE//'13;67H',CUTC(1:7)//CUTC(10:11)
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,57,IOSTAT=I) CE//'14;68H',CUTC(13:20)
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,43,IOSTAT=I) CE//'15;73H',ITRKRS,IDYNAM
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,45,IOSTAT=I) CE//'16;71H',IAOLD
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,47,IOSTAT=I) CE//'17;71H',NINT(RUTCCOR)
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,49,IOSTAT=I) CE//'18;71H',IWKNUM
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,51,IOSTAT=I) CE//'19;69H',RCLKBIAS/1000.
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,53,IOSTAT=I) CE//'20;71H',RCLKDRFT
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,54,IOSTAT=I) CE//'21;70H',CSOFT,IRECTYP
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,55,IOSTAT=I) CE//'22;70H',IMAINTSUM,ICDU
          CALL VT_OUT(CL(1:LAN(CL)))
        ELSE IF(IMESSNO.EQ.31) THEN
          CALL GPS1_DECODE_MESS31(IMESS,RHDOP,RVDOP,RPDOP,ISATS,RELEV,
     -        RAZIM)
          WRITE(CL,51,IOSTAT=I) CE//'6;56H',RHDOP
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,51,IOSTAT=I) CE//'7;56H',RVDOP
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,51,IOSTAT=I) CE//'8;56H',RPDOP
          CALL VT_OUT(CL(1:LAN(CL)))
          DO I=1,4
            RETMP(I)=0.
            RATMP(I)=0.
          END DO
          DO I=1,4                       !Sort into same sat order as for M21
            DO J=1,4
              IF(ISATS(J).EQ.ISVIDS(I)) THEN
                RETMP(I)=RELEV(J)
                RATMP(I)=RAZIM(J)
              END IF
            END DO
          END DO
          WRITE(CL,48,IOSTAT=I) CE//'21;15H',(RETMP(J),J=1,4)
          CALL VT_OUT(CL(1:LAN(CL)))
          WRITE(CL,48,IOSTAT=I) CE//'22;15H',(RATMP(J),J=1,4)
          CALL VT_OUT(CL(1:LAN(CL)))
        END IF
      END IF
      RETURN
10    FORMAT(A)
20    FORMAT(A,A)
21    FORMAT(A,I2,3(X,I2))
22    FORMAT(A,I1)
23    FORMAT(A,I3,'''',I2.2,'.',I2.2,X,A)
24    FORMAT(A,I2,3I3)
25    FORMAT(A,I1)
26    FORMAT(A,F7.1,3X,F7.1)
27    FORMAT(A,I2,3I3)
28    FORMAT(A,I1)
29    FORMAT(A,F6.1,X,F6.1,X,F6.2)
30    FORMAT(A,I2,3I3)
31    FORMAT(A,I5)
32    FORMAT(A,I10,3(2X,I10))
33    FORMAT(A,I2)
34    FORMAT(A,F6.1,4X,F6.1)
35    FORMAT(A,I1)
36    FORMAT(A,F6.1,4X,F6.1)
37    FORMAT(A,F9.3,X,F9.3)
38    FORMAT(A,F10.1,3(X,F10.1))
39    FORMAT(A,I9,3(X,I9))
40    FORMAT(A,F9.2,3(1X,F9.2))
41    FORMAT(A,I5,' /',I2,3(I6,' /',I2))
42    FORMAT(A,I5,3(X,I9))
43    FORMAT(A,I1,'/',I1)
44    FORMAT(A,I6,3(2X,I6))
45    FORMAT(A,I5)
46    FORMAT(A,I6,3(2X,I6))
47    FORMAT(A,I5)
48    FORMAT(A,F6.1,3(2X,F6.1))
49    FORMAT(A,I5)
50    FORMAT(A,F6.1,3(2X,F6.1))
51    FORMAT(A,F7.1)
52    FORMAT(A,I6,3(2X,I6))
53    FORMAT(A,F5.1)
54    FORMAT(A,A,'/',I1)
55    FORMAT(A,Z4.4,'/',I1)
56    FORMAT(A,A)
57    FORMAT(A,A)
      END
