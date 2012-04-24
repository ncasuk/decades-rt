        PROGRAM h_gin_log
 
C       To build:
C           $ FORTRAN h_gin_log
C           $ LINK    h_gin_log
C       Then run this program to connect to the server:
C           $ RUN h_gin_log
        IMPLICIT NONE
C       ** include useful system definitions
        INCLUDE '($CVTDEF)'
        INCLUDE 'HCOM_DEF.FOR'
C       ** declare variables
        INTEGER*4 I,IDSTS1
        INTEGER*4 J1,J2,JJHRS,JJMIN,JJSEC,IDTIM,OLDTIME
        INTEGER*2   checksum
        BYTE GRPMESS(132)
        BYTE bytval(8,8)

        REAL*8 DBLVAL(8)
        REAL*4 LAT,LON,ALT
        CHARACTER grp_start*4
        INTEGER*2 grp_ID,byte_count
        INTEGER*2 sgrpmess(66)

        EQUIVALENCE(bytval,dblval)
	EQUIVALENCE(sgrpmess,grpmess)
C       ** function declarations
 
C ---------------------------------------------------------------
 
!        CALL opensocket(192,168,102,21,5602)
        CALL opensocket(151,170,96,124,5602)
!       Open data file
!
        OLDTIME=-9999
        DO WHILE(.TRUE.)
          CALL readsocket(4,%R
EF(grp_start))
          CALL readsocket(2,%REF(grp_ID))
!          print *,'Read Group ID ',grp_ID
          CALL readsocket(2,%REF(byte_count))
!          print *,'Read byte count ',byte_count
          CALL readsocket(byte_count,%REF(grpmess))
! Calculate CHECKSUM
         checksum=0
         checksum=(checksum+21063).AND.'FFFF'X   ! GR as integer  Add GRP$ group start to checksum 
         checksum=(checksum+9296).AND.'FFFF'X    ! P$ as integer
         checksum=(checksum+grp_ID).AND.'FFFF'X  ! Add grou
p ID to checksum
         checksum=(checksum+byte_count).AND.'FFFF'X  ! Add byte count to checksum
         DO I=1,byte_count/2
           checksum=(checksum+sgrpmess(I)).AND.'FFFF'X   !Add rest of message to checksum
         ENDDO
! Only if CHECKSUM=0 do any more processing
         IF(checksum.EQ.0)THEN
           IDTIM=-1
           IF(STATUS(33).EQ.1) THEN     !If have good DRS data check times
             J1=NFDATA(10,STATUS(1))    !Decode DRS time into secs past midnight
             J2=NFDATA(11,ST
ATUS(1))
             JJHRS=IBITS(J1,8,4)*10+IBITS(J1,4,4)
             JJMIN=IBITS(J1,0,4)*10+IBITS(J2,8,4)
             JJSEC=IBITS(J2,4,4)*10+IBITS(J2,0,4)
             IDTIM=JJHRS*3600+JJMIN*60+JJSEC !DRS seconds past midnight
           ENDIF
	   CALL WRITE_GIN(GRP_ID,GRPMESS)
           IF((IDTIM.GE.(OLDTIME+1)).OR.(IDTIM.LT.OLDTIME))THEN
           DO I=1,8
             bytval(I,1)=grpmess(I+26)
             bytval(I,2)=grpmess(I+34)
             bytval(I,3)=grpmess(I+42)
!             bytval(I,4)=gr
pmess(I)
           ENDDO
           CALL CVT$FTOF(dblval(1),%VAL(CVT$K_IEEE_T),
     &               lat,%VAL(CVT$K_VAX_F),)
           CALL CVT$FTOF(dblval(2),%VAL(CVT$K_IEEE_T),
     &               lon,%VAL(CVT$K_VAX_F),)
           CALL CVT$FTOF(dblval(3),%VAL(CVT$K_IEEE_T),
     &               alt,%VAL(CVT$K_VAX_F),)
!           CALL CVT$FTOF(dblval(4),%VAL(CVT$K_IEEE_T),
!     &               newtime,%VAL(CVT$K_VAX_F),)
             OLDTIME=IDTIM
             IDSTS1=STATUS(2)
             if(MOD(IDT
IM,300).EQ.0)print *,
     &  '%H_GIN_LOG Running DRS time=',
     &  IDTIM,'Lat=',lat,'Lon=',lon
             IF(IDSTS1.GT.0.AND.IDSTS1.LE.12800) THEN
               RGINLAT(IDSTS1)=LAT
               RGINLON(IDSTS1)=LON
               RGINALT(IDSTS1)=ALT
             ENDIF
           ENDIF
	 ELSE
	   print *,'Problem with Check Sum = ',checksum
	 ENDIF
       ENDDO
       END
 
C --------------------------------------------------------------------------
 
        INTEGER*2 FUNCTION htons( port )
        I
NTEGER*2 port
 
C       ** swap the two bytes of 'port' around
 
        htons=0
        call mvbits(port,0,8,htons,8)
        htons=htons.AND.'FF00'X
        call mvbits(port,8,8,htons,0)
 
        END
 
********************************************************************************
      SUBROUTINE CLOSEANDEXIT(ISTAT)
      INTEGER*4 ISTAT
      print *,'Close and exit'
      CALL LIB$STOP(ISTAT)
      RETURN
      END


********************************************************************************
   
   SUBROUTINE GET_DISK_STATUS(CSYSSTS,COPTSTS)
C
C Reads the group logical names OPTI_STATUS and SYST_STATUS
C
C V1.00            W.D.N.JACKSON
C V1.01  10/08/92  W.D.N.JACKSON
C        Now also read SYST_STATUS
C
      IMPLICIT NONE
      INTEGER*4 ISTAT,NAMLEN
      CHARACTER*(*) CSYSSTS,COPTSTS
      CHARACTER*20 LOGNAME
      INTEGER*4 ILIST(4)
      INTEGER*2 JLIST(8)
      EQUIVALENCE (ILIST,JLIST)
      INCLUDE '($SYSSRVNAM)'
      INCLUDE '($LNMDEF)'

      CSYSSTS=' '
      COPTSTS=' '
      JLIST(
1)=20
      JLIST(2)=LNM$_STRING
      ILIST(2)=%LOC(LOGNAME)
      ILIST(3)=%LOC(NAMLEN)
      ILIST(4)=0
      ISTAT=SYS$TRNLNM(,'LNM$GROUP','SYST_STATUS',,ILIST)
      IF(ISTAT) CSYSSTS=LOGNAME(1:MAX(NAMLEN,1))
      ISTAT=SYS$TRNLNM(,'LNM$GROUP','OPTI_STATUS',,ILIST)
      IF(ISTAT) COPTSTS=LOGNAME(1:MAX(NAMLEN,1))
      RETURN
10    FORMAT(3I)
      END
C*******************************************************************************
!*********************************************************************
**********
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
      CRESULT=' '                      !Will contain
 the translation 
      JLIST(1)=LEN(CRESULT)            !Length of string space
      JLIST(2)=LNM$_STRING             !Required function
      ILIST(2)=%LOC(CRESULT)           !String location
      ILIST(3)=%LOC(NAMLEN)            !Will have string length
      ILIST(4)=0                       !End of list
      CALL SYS$TRNLNM(,'LNM$DCL_LOGICAL',CLOGICAL,,ILIST)
      RETURN
      END

!*****************************************************************************
      SUBROUTINE DRSMSG(LUN,PACKET_NO)


      BYTE GRPMESS(132)
      CHARACTER CMESS*132
      CHARACTER CFLTNO*4
      INTEGER*2 grp_id,byte_count
      INTEGER*4 packet_no,LUN
      INCLUDE 'HCOM_DEF.FOR'
      EQUIVALENCE (CMESS,GRPMESS)
      print *,'DRSMSG ',LUN

      CALL GET_LOGICAL('FLIGHT_NUMBER',CFLTNO)
      grp_id=0
      byte_count=132
      CMESS(1:4)=CFLTNO  
      CALL GINMSG(LUN,PACKET_NO,grp_id,grpmess)    
      RETURN
      END
!*****************************************************************************
      SUBROUTINE G
INMSG(LUN,packet_no,grp_id,gmess)

      BYTE GRPMESS(132),GMESS(132)
      CHARACTER*132 CMESS
      INTEGER*4 IMESS(33),ISTAT,I
      INTEGER*2 grp_id,byte_count
      INTEGER*4 packet_no,LUN,J1,J2,JJHRS,JJMIN,JJSEC,IDTIM
      INCLUDE 'HCOM_DEF.FOR'
      EQUIVALENCE (CMESS,GRPMESS)
      EQUIVALENCE (IMESS,GRPMESS)

      byte_count=132
      IDTIM=-1
      DO I=1,132
        GRPMESS(I)=GMESS(I)
      ENDDO
      IF(STATUS(33).EQ.1) THEN     !If have good DRS data check times
            J1=NFDATA(10,ST
ATUS(1))    !Decode DRS time into secs past midnight
            J2=NFDATA(11,STATUS(1))
            JJHRS=IBITS(J1,8,4)*10+IBITS(J1,4,4)
            JJMIN=IBITS(J1,0,4)*10+IBITS(J2,8,4)
            JJSEC=IBITS(J2,4,4)*10+IBITS(J2,0,4)
            IDTIM=JJHRS*3600+JJMIN*60+JJSEC !DRS seconds past midnight
            IMESS(33)=IDTIM
      ENDIF
      WRITE(LUN,REC=packet_no,IOSTAT=ISTAT)grp_ID,byte_count,
     &               (grpmess(I),I=1,byte_count)
      IF(ISTAT.EQ.0)PACKET_NO=PACKET_NO+1
      RETURN

      END
!************************************************************************

      SUBROUTINE OPEN_GIN
      CHARACTER*12 CSYSSTS,COPTSTS
      INTEGER*4 DLUN,OLUN,D_REC,O_REC,ISTAT
      COMMON /GIN/DLUN,OLUN,D_REC,O_REC

      CALL GET_DISK_STATUS(CSYSSTS,COPTSTS)
      DLUN=1000
      OLUN=1000
      print *,'OPEN_GIN'
      IF(CSYSSTS.NE.'Unavailable') THEN
        CALL LIB$GET_LUN(DLUN)
        OPEN(UNIT=DLUN,FILE='[HORACE]GINDAT.DAT',STATUS='NEW',
     &      IOSTAT=ISTAT,
     &      ACCESS=
'DIRECT',FORM='UNFORMATTED',RECL=34)
        IF(ISTAT.NE.0) THEN               !If create failed
          CALL LIB$FREE_LUN(DLUN)   !Release LUN
          DLUN=1000                 !Show stream free
	ELSE
	  print *,'[HORACE]GINDAT.DAT open',DLUN 
	  D_REC=1
	  CALL DRSMSG(DLUN,D_REC)
        END IF
      ENDIF
      IF(COPTSTS.NE.'Unavailable') THEN
        CALL LIB$GET_LUN(OLUN)
        OPEN(UNIT=OLUN,FILE='DUB0:[HORACE]GINDAT.DAT',STATUS='NEW',
     &      IOSTAT=ISTAT,
     &      ACCESS='DIRECT',FORM=
'UNFORMATTED',RECL=34)
        IF(ISTAT.NE.0) THEN               !If create failed
          CALL LIB$FREE_LUN(OLUN)   !Release LUN
          OLUN=1000                 !Show stream free
	ELSE
	  print *,'DUB0:[HORACE]GINDAT.DAT open',OLUN 
	  O_REC=1
          CALL DRSMSG(OLUN,O_REC)
        END IF
      ENDIF
      RETURN
      END
!************************************************************************

      SUBROUTINE WRITE_GIN(GRP_ID,GRPMESS)
      CHARACTER*4 CFLTNO,LAST_CFLTNO
      CHARACTER*12 CSY
SSTS,COPTSTS
      INTEGER*4 DLUN,OLUN,D_REC,O_REC
      INTEGER*2 GRP_ID
      BYTE GRPMESS(132)
      COMMON /GIN/DLUN,OLUN,D_REC,O_REC
      DATA LAST_CFLTNO/'xxxx'/
      SAVE LAST_CFLTNO
      
      CALL GET_DISK_STATUS(CSYSSTS,COPTSTS)
      CALL GET_LOGICAL('FLIGHT_NUMBER',CFLTNO)
      IF(CFLTNO.NE.LAST_CFLTNO)THEN
        CALL CLOSE_GIN
	CALL OPEN_GIN
	LAST_CFLTNO=CFLTNO
      ENDIF
      IF(CSYSSTS.NE.'Unavailable')THEN
         IF(DLUN.NE.1000)THEN
	   CALL GINMSG(DLUN,D_REC,GRP_ID,GRPMESS)  
  
       ENDIF
      ENDIF
      IF(COPTSTS.NE.'Unavailable')THEN
         IF(OLUN.NE.1000)THEN
	   CALL GINMSG(OLUN,O_REC,GRP_ID,GRPMESS)  
         ENDIF
      ENDIF
      
      RETURN
      END
!************************************************************************

      SUBROUTINE CLOSE_GIN
      INTEGER*4 DLUN,OLUN,D_REC,O_REC
      COMMON /GIN/DLUN,OLUN,D_REC,O_REC
        IF(DLUN.NE.1000)CLOSE(DLUN)
	IF(OLUN.NE.1000)CLOSE(OLUN)
      RETURN
      END
!***********************************************
********************************
      SUBROUTINE opensocket(a,b,c,d,port)

      IMPLICIT  NONE
      INCLUDE   'SYS$LIBRARY:TCPIP$INETDEF.FOR'
      INCLUDE   '($IODEF)'
      INCLUDE   '($SYSSRVNAM)'
      INCLUDE   '(lib$routines)'
      INTEGER*4 EVNF
      INTEGER*2 JOSB(4),JCHAN,SCKPARM(2),port
      INTEGER*4   a, b, c, d,f,IFUNC,ISTAT
      INTEGER*2   htons
      RECORD / sockaddrin / remote_host
      STRUCTURE / struct_il2 /
          INTEGER*4 il2_length
          INTEGER*4 il2_address       ! 
address
      END STRUCTURE
      RECORD /struct_il2/ rhst_adrs
      COMMON /SOCK/EVNF,JOSB,JCHAN
      ISTAT = lib$get_ef( EVNF )
      IF (.NOT. ISTAT) CALL lib$stop( %VAL( ISTAT ) )
      IF (EVNF .EQ. -1) CALL lib$stop( %VAL( 0 ) )
      rhst_adrs.il2_length  = sizeof( remote_host )
      rhst_adrs.il2_address = %LOC( remote_host )
      remote_host.SIN$W_FAMILY = TCPIP$C_AF_INET    ! INET family
 
      PRINT *, 'Server address = ', a, '.', b, '.', c, '.', d
      PRINT *, 'Server port    = ', port
C 
      ** sin$l_adr needs to store an IP address of form a.b.c.d
C       ** in the byte order d:c:b:a
        
      call mvbits(d,0,8,f,24)
      call mvbits(c,0,8,f,16)
      call mvbits(b,0,8,f,8)
      call mvbits(a,0,8,f,0)
      remote_host.sin$l_addr = f
      remote_host.sin$w_port = htons( port )
 
      PRINT *, 'Server host address as an integer = ',
     &              remote_host.sin$l_addr
      PRINT *, 'Server port in network format     = ',
     &              remote_host.sin$w_port
!
! Assi
gn the device
! 
      ISTAT=SYS$ASSIGN('TCPIP$DEVICE:',JCHAN,,)
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
!
!  Create the socket
!
      IFUNC=IO$_SETMODE
      SCKPARM(1)=TCPIP$C_TCP
      SCKPARM(2)=INET_PROTYP$C_STREAM
      ISTAT=SYS$QIOW(%VAL(EVNF),%VAL(JCHAN),%VAL(IFUNC),JOSB,,,
     &    SCKPARM,,,,,)
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      IF(.NOT.JOSB(1)) CALL LIB$STOP(%VAL(JOSB(1)))
!
!  Connect to specified host and port number
!
      IFUNC=IO$_ACCESS
      ISTAT=SYS$QIOW(%V
AL(EVNF),%VAL(JCHAN),%VAL(IFUNC),JOSB,,,
     &    ,,%REF(rhst_adrs),,,)
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      IF(.NOT.JOSB(1)) CALL LIB$STOP(%VAL(JOSB(1)))
      
      RETURN
      END
!*******************************************************************************
      SUBROUTINE readsocket(ILCIN,IREF)
!
      IMPLICIT  NONE

      INCLUDE   'SYS$LIBRARY:TCPIP$INETDEF.FOR'
      INCLUDE   '($IODEF)'
      INCLUDE   '($SYSSRVNAM)'
      INTEGER*4 ILCIN,IREF
      INTEGER*4 ISTAT,IFUNC,EVN
F
      INTEGER*2 JOSB(4),JCHAN
      COMMON /SOCK/EVNF,JOSB,JCHAN
!
! Read some data
!
      IFUNC=IO$_READVBLK
      ISTAT=SYS$QIOW(%VAL(EVNF),%VAL(JCHAN),%VAL(IFUNC),JOSB,,,
     & IREF,
     &%VAL(ILCIN),,,,)
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      IF(.NOT.JOSB(1)) CALL closesocket()
      RETURN
      END
C*******************************************************************************
C*******************************************************************************
      SUBROUTINE closeso
cket()

      IMPLICIT  NONE
      INCLUDE   'SYS$LIBRARY:TCPIP$INETDEF.FOR'
      INCLUDE   '($IODEF)'
      INCLUDE   '($SYSSRVNAM)'
      INCLUDE '(lib$routines)'
      INTEGER*4 ISTAT,IFUNC,EVNF
      INTEGER*2 JOSB(4),JCHAN
      COMMON /SOCK/EVNF,JOSB,JCHAN
!
! Shutdown the socket
!
      IFUNC=IO$_DEACCESS.OR.IO$M_SHUTDOWN
      ISTAT=SYS$QIOW(%VAL(EVNF),%VAL(JCHAN),%VAL(IFUNC),JOSB,,,
     &    ,,,%VAL(TCPIP$C_DSC_ALL),,)
!      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
!      IF(.NOT.JOSB(1)) CALL 
LIB$STOP(%VAL(JOSB(1)))

! Close the socket

      IFUNC=IO$_DEACCESS
      ISTAT=SYS$QIOW(%VAL(EVNF),%VAL(JCHAN),%VAL(IFUNC),JOSB,,,
     &    ,,,,,)
!      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
!      IF(.NOT.JOSB(1)) CALL LIB$STOP(%VAL(JOSB(1)))

! Deassign the device

      ISTAT=SYS$DASSGN(%VAL(JCHAN))
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
!
!       ** free the event flag
 
      ISTAT = lib$free_ef( %REF( EVNF ) )
      IF (.NOT. ISTAT) CALL lib$stop( %VAL(ISTAT ) )
C       ** free the e
vent flag
 
 
C       -------------------------------------------------------
C       ** Inform user that program ran to completion:
 
C       ** if you don't see the following msg, then run the program
C       ** under the debugger to see where it calls lib$stop()
 
 
      PRINT *, 'Socket closed and exiting'
      CALL EXIT
      RETURN
      END
C*******************************************************************************
                                                                               
