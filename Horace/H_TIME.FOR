!
! H_TIME.FOR
!
! Multicasts a data packet from port 1501 whenever a time packet is received
! from the AMTG.  This packet contains an accurate time together with the 
! latest key measurements, including event mark.
!
! Also monitors port 1501 for UDP datagrams coming from any source and
! containing the letter 'E', in which case the DRS event mark is incremented
! and data packet multicast out immediately in response.
!
! V1.00  02/09/02  W.D.N.JACKSON
!
! Note that may need to $TCPIP SET PROTO UDP /BROADCAST
! and check with $TCPIP SHOW PROTO UDP /PARA
!
      PROGRAM   H_TIME
      INTEGER*4 ISTAT,LAN
      INTEGER*2 JLSTWRD,JCHAN
      CHARACTER CSTRING*80
      INCLUDE   'DRS_GDEF.FOR'
      INCLUDE   '($SYSSRVNAM)'
      INCLUDE   '($SSDEF)'
      COMMON    JCHAN

      ISTAT=SYS$ASCEFC(%VAL(64),'H_CEF_CLUSTER',,) !Associate with common EFs
      IF(ISTAT.NE.SS$_NORMAL) CALL LIB$SIGNAL(%VAL(ISTAT))
      CALL UDP_CREATE                  !Create UDP broadcast socket
      CALL UDP_GET_AST                 !Set up AST to handle event mark reqs
      JLSTWRD=0
      DO WHILE(.TRUE.)
        DO WHILE(JLSTWRD.EQ.JTIMPKT(4))
          CALL LIB$WAIT(0.1)
        END DO
        JLSTWRD=JTIMPKT(4)
        CALL FORM_STRING(CSTRING)
        CALL UDP_SEND(CSTRING(1:LAN(CSTRING))) 
      END DO
      END
!*******************************************************************************
      SUBROUTINE EVENT_MARK
!
! Requests the DRS program to increment the DRS event mark, by setting INXTEVM
! to one, and then setting common event flag 83.
!
      INTEGER*4 ISTAT
      INCLUDE   'DRS_GDEF.FOR'
      INCLUDE   '($SYSSRVNAM)'
      INCLUDE   '($SSDEF)'

      INXTEVM=1
      ISTAT=SYS$SETEF(%VAL(83))
      RETURN
      END
!*******************************************************************************
      SUBROUTINE UDP_CREATE
!
! Creates a UDP broadcast socket on port 1501.
!
      IMPLICIT  NONE
      INTEGER*4 ISTAT
      INTEGER*2 JCHAN,JOSB(4),JBROAD
      INCLUDE   'SYS$LIBRARY:TCPIP$INETDEF.FOR'
      INCLUDE   '($IODEF)'
      INCLUDE   '($SYSSRVNAM)'
      COMMON    JCHAN

      STRUCTURE /ITEMLST_2/
        INTEGER*2 LENGTH
        INTEGER*2 PARAM
        INTEGER*4 ADDRESS
      END STRUCTURE

      STRUCTURE /SOCKCHAR/
        INTEGER*2 PROTO
        BYTE      TYPE
        BYTE      AF
      END STRUCTURE

      RECORD /ITEMLST_2/ SO            !Socket options
      RECORD /ITEMLST_2/ BR            !Broadcast option
      RECORD /SOCKCHAR/ SC             !Socket characteristics
!
! Assign a channel to the TCPIP device
!
      ISTAT = SYS$ASSIGN('TCPIP$DEVICE:',JCHAN,,)
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
!
! Create the socket, using the Broadcast socket option
!
      SC.PROTO = TCPIP$C_UDP           !Define socket characteristics
      SC.TYPE  = TCPIP$C_DGRAM
      SC.AF    = TCPIP$C_AF_INET
!
      SO.LENGTH  = 8                   !Define socket options item list
      SO.PARAM   = TCPIP$C_SOCKOPT
      SO.ADDRESS = %LOC(BR)
!
      BR.LENGTH  = 8
      BR.PARAM   = TCPIP$C_BROADCAST
      BR.ADDRESS = %LOC(JBROAD)
      JBROAD     = 1                   !Enable broadcasting

      ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IO$_SETMODE),JOSB,,,
     &    SC,,,,%REF(SO),) !Socket characteristics buffer and socket options
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      IF(.NOT.JOSB(1)) CALL LIB$STOP(%VAL(JOSB(1)))
!
      RETURN
      END
!*******************************************************************************
      SUBROUTINE UDP_SEND(CSTRING)

      INTEGER*2 JOSB(4),JPORT,JCHAN
      INTEGER*4 LAN,ISTAT,IBUFLEN
      CHARACTER CSTRING*(*)
      CHARACTER CPORT*4
      BYTE      BPORT(2),BTEMP
      EQUIVALENCE (BPORT,JPORT)
      INCLUDE   'SYS$LIBRARY:TCPIP$INETDEF.FOR'
      INCLUDE   '($IODEF)'
      INCLUDE   '($SYSSRVNAM)'
      COMMON    JCHAN

      STRUCTURE /ITEMLST_2/
        INTEGER*2 LENGTH
        INTEGER*2 PARAM
        INTEGER*4 ADDRESS
      END STRUCTURE

      STRUCTURE /SOCKADD/
        INTEGER*2 FAMILY
        INTEGER*2 PORT
        INTEGER*4 ADDRESS
        INTEGER*8 UNUSED
      END STRUCTURE

      RECORD /ITEMLST_2/ RH            !Remote host
      RECORD /SOCKADD/ SA              !Remote socket name
!
! Write I/O buffer to any broadcast address
!
      EQUIVALENCE (BPORT,JPORT)

      CPORT='1501'
      READ(CPORT(1:LAN(CPORT)),'(BN,I)') JPORT
      BTEMP=BPORT(1)
      BPORT(1)=BPORT(2)
      BPORT(2)=BTEMP

      RH.LENGTH  = 16              !Define item list
      RH.PARAM   = TCPIP$C_SOCK_NAME
      RH.ADDRESS = %LOC(SA)
!
      SA.FAMILY  = TCPIP$C_AF_INET !Define remote socket name
      SA.PORT    = JPORT
      SA.ADDRESS = TCPIP$C_INADDR_BROADCAST
      SA.UNUSED  = 0
!
! Send the string
!
      IBUFLEN = LAN(CSTRING)
      ISTAT = SYS$QIOW(,%VAL(JCHAN),%VAL(IO$_WRITEVBLK),JOSB,,,
     &    %REF(CSTRING),%VAL(IBUFLEN),%REF(RH),,,)
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      IF(.NOT.JOSB(1)) CALL LIB$STOP(%VAL(JOSB(1)))
!
      RETURN
      END
!*******************************************************************************
      SUBROUTINE UDP_GET_AST
!
! Because Fortran routines can't call themselves.
!
      CALL UDP_GET_AST1
      RETURN
      END
!*******************************************************************************
      SUBROUTINE UDP_GET_AST1
!
! Collects asynchronous input on port 1501, particular the character 'E' sent
! by any system, indicating a request to increment the DRS event mark.
!
      INTEGER*4 ISTAT,IFIRST
      INTEGER*2 JOSB(4),JCHAN
      CHARACTER CBUFF*80
      DATA      IFIRST /0/
      INCLUDE   '($IODEF)'
      INCLUDE   '($SYSSRVNAM)'
      EXTERNAL  UDP_GET_AST
      COMMON    JCHAN

      IF(IFIRST.NE.0) THEN
        IF(.NOT.JOSB(1)) CALL LIB$STOP(%VAL(JOSB(1)))
        IF(CBUFF(1:1).EQ.'E') CALL EVENT_MARK
      END IF
      IFIRST=1
      ISTAT = SYS$QIO(,%VAL(JCHAN),%VAL(IO$_READVBLK),JOSB,
     &    %REF(UDP_GET_AST),,
     &    %REF(CBUFF),%VAL(80),,,,) !Note remote port details not required
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      RETURN
      END
!*******************************************************************************
      INTEGER*4 FUNCTION LAN(CSTRING)
!
! Returns the length of a string without trailing spaces.
!
      IMPLICIT  NONE
      CHARACTER CSTRING*(*)
      INTEGER*4 N

      N=LEN(CSTRING)                   !Get string length
      DO WHILE(N.GT.0.AND.CSTRING(N:N).EQ.' ') !Step back over spaces
        N=N-1
      END DO
      IF(N.EQ.0) N=1                   !Special case
      LAN=N
      RETURN
      END
!*******************************************************************************
      SUBROUTINE FORM_STRING(CSTR)
      IMPLICIT  NONE
      INTEGER*4 IS,IOS,I,IFLTEVM
      CHARACTER CSTR*(*),CEVMTIM*10,CTIM*6,CFLTNO*4,CFLTDAT*9,
     &    CFLTTIM*8,CFLTEVM*4
      REAL*4    RFLTLAT,RFLTLNG
      INCLUDE   'HCOM_DEF.FOR'
      INCLUDE   'DRS_GDEF.FOR'

      CSTR=' '
      CALL GET_LOGICAL('FLIGHT_NUMBER',CFLTNO)
      CALL AMTG_TIME(JTIMPKT,CFLTDAT,CFLTTIM)
      IFLTEVM=IEVENT
      WRITE(CFLTEVM,'(I4.4)',IOSTAT=IOS) IFLTEVM
      WRITE(CTIM,'(2(Z3.3))',IOSTAT=IOS) (STATUS(I),I=14,15) !Last EVM time
      CEVMTIM='('//CTIM(1:2)//':'//CTIM(3:4)//':'//CTIM(5:6)//')'
      IS=STATUS(2)
      IF(STATUS(34).AND.IS.GE.1.AND.IS.LE.12800) THEN
        RFLTLAT=RGLAT(IS)      
        RFLTLNG=RGLNG(IS)      
      ELSE
        RFLTLAT=0.
        RFLTLNG=0.
      END IF
      WRITE(CSTR,10,IOSTAT=IOS) CFLTNO,CFLTDAT,CFLTTIM,CFLTEVM(2:4),
     &    CEVMTIM,RGLAT(IS),RGLNG(IS)
      RETURN
10    FORMAT(A,2X,A,2X,A,2X,A,2X,A,2X,F6.2,2X,F7.2)
      END
!*******************************************************************************
      SUBROUTINE AMTG_TIME(JTIMPKT,CDATE,CTIME)
!
! Computes ASCII time (hh:mm:ss) and date (dd-Mmm-yy) from time packet in JPKT.
!
      IMPLICIT  NONE
      INTEGER*4 ITEMP,IHRS,IMIN,ISEC,IOS,I
      INTEGER*2 JTIMPKT(11),JTEMP(2),JPKT(11)
      CHARACTER CDATE*9,CTIME*8,CYEAR*4,CMON(12)*3,CMONTH*3
      EQUIVALENCE (ITEMP,JTEMP)
      DATA CMON /'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep',
     &    'Oct','Nov','Dec'/

      DO I=1,11
        JPKT(I)=JTIMPKT(I)
      END DO
      CALL SWAP_ENDS32(JPKT(3),1)
      CALL SWAP_ENDS16(JPKT(5),3)
      JTEMP(1)=JPKT(3)
      JTEMP(2)=JPKT(4)
      ITEMP=ITEMP+1                    !Add one because packet time is UTC-1
      ISEC=MOD(ITEMP,60)
      IMIN=MOD(ITEMP/60,60)
      IHRS=ITEMP/3600
      WRITE(CYEAR,'(I4.4)',IOSTAT=IOS) JPKT(7)
      CMONTH='***'
      IF(JPKT(6).GE.1.AND.JPKT(6).LE.12) CMONTH=CMON(JPKT(6))
      WRITE(CDATE,'(I2.2,A)',IOSTAT=IOS) JPKT(5),'-'//CMONTH
     &    //'-'//CYEAR(3:4)
      WRITE(CTIME,'(2(I2.2,A),I2.2)',IOSTAT=IOS) IHRS,':',IMIN,':',ISEC
      RETURN
      END
!*******************************************************************************
      SUBROUTINE GET_LOGICAL(CLOGICAL,CRESULT)
!
! Translate the logical name specified by CLOGICAL and returns the result in
! CRESULT.  Returns with blank if not found.  Searches all logical name tables
! and returns the first (lowest) definition found.
!
      IMPLICIT    NONE
      CHARACTER   CLOGICAL*(*),CRESULT*(*)
      INTEGER*4   ILIST(4),NAMLEN
      INTEGER*2   JLIST(8)
      EQUIVALENCE (ILIST,JLIST)
      INCLUDE     '($LNMDEF)'
!
      CRESULT=' '                      !Will contain the translation 
      JLIST(1)=LEN(CRESULT)            !Length of string space
      JLIST(2)=LNM$_STRING             !Required function
      ILIST(2)=%LOC(CRESULT)           !String location
      ILIST(3)=%LOC(NAMLEN)            !Will have string length
      ILIST(4)=0                       !End of list
      CALL SYS$TRNLNM(,'LNM$DCL_LOGICAL',CLOGICAL,,ILIST)
      RETURN
      END
!*******************************************************************************
      SUBROUTINE SWAP_ENDS32(IBUF,N)
!
! Swaps bytes and words in a thirty two bit word, or array of 32 bit words
! JBUF contains the 32 bit words, and N is the number of words in IBUF
!
      IMPLICIT  NONE
      INTEGER*4 IBUF(*),I,ITEMP
      INTEGER*4 N
      BYTE      BBUF(4),B
      EQUIVALENCE (ITEMP,BBUF)

      DO I=1,N
        ITEMP=IBUF(I)
        B=BBUF(1)
        BBUF(1)=BBUF(4)
        BBUF(4)=B
        B=BBUF(2)
        BBUF(2)=BBUF(3)
        BBUF(3)=B
        IBUF(I)=ITEMP   
      END DO
      RETURN
      END
!*******************************************************************************
      SUBROUTINE SWAP_ENDS16(JBUF,N)
!
! Swaps the two bytes in a sixteen bit word, or array of 16 bit words
! JBUF contains the 16 bit words, and N is the number of words in JBUF
!
      IMPLICIT  NONE
      INTEGER*2 JBUF(*),JTEMP
      INTEGER*4 N,I
      BYTE      B,BTEMP(2)
      EQUIVALENCE (JTEMP,BTEMP)

      DO I=1,N
        JTEMP=JBUF(I)
        B=BTEMP(1)
        BTEMP(1)=BTEMP(2)
        BTEMP(2)=B
        JBUF(I)=JTEMP
      END DO
      RETURN
      END
