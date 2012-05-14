      PROGRAM H_FWVS
       
      REAL*4 FWVSVAL
      INTEGER*4 IC,ISTS,IFWVS
      INTEGER*2 JVAL(16)
      CHARACTER FWVSLINE*64
      COMMON /FWVSCOM/FWVSVAL(20)

      INTEGER*4 I,I1,IX

      STRUCTURE /socket/
       INTEGER*2 CHANNEL
       INTEGER*4  EVNF
       INTEGER*2 JOSB(4)
      END STRUCTURE

      RECORD / SOCKET / FWVS

      COMMON /CHANS/FWVS
      INCLUDE   'HCOM_DEF.FOR'
      INCLUDE   'DRS_GDEF.FOR'
      DATA      IFWVS /230/            !FWVS parameter number

      print *,'***  H_FWVS  ***'
      
      print *,'opening sockets'
!      CALL OPENSOCKET(FWVS,151,170,96,6,6340)
!      CALL OPENSOCKET(FWVS,151.170.96,124,6340)
      CALL OPENSOCKET(FWVS,192,168,101,165,6340)
      print *,'opened sockets'
      DO WHILE(.TRUE.)
      I=1 
      I1=1     
      IX=1 
      IC=ICURTIM
      DO WHILE(IC.EQ.ICURTIM)
        CALL LIB$WAIT(0.05)
      ENDDO
!      CALL LIB$WAIT(1.0)
      IC=ICURTIM
      
      DO I=1,16
        JDRSBLK(JISSBLK(IFWVS+1)/2+I)=JVAL(I)
      ENDDO    
      CALL writesocket(FWVS,'F')
      CALL readsocket(FWVS,64,%REF(FWVSLINE))
!      print *,FWVSLINE
      READ(FWVSLINE,'(16Z4.4)',IOSTAT=ISTS) JVAL
      IF(ISTS.EQ.0) THEN
        DO I=1,16
          JDRSBLK(JISSBLK(IFWVS+1)/2+I)=JVAL(I)
        ENDDO    
      ENDIF
      ENDDO
!      CALL FWVS_STOPS(DLUN)
      END
! --------------------------------------------------------------------------
!*******************************************************************************

      

!************************************************************************

      SUBROUTINE OPEN_FWVS(DLUN)
      CHARACTER*12 CSYSSTS,COPTSTS
      CHARACTER*132 CFLTNO
      INTEGER*4 ISTAT,DLUN

      CALL GET_DISK_STATUS(CSYSSTS,COPTSTS)
      DLUN=1000
      print *,'OPEN_FWVS'
      IF(CSYSSTS.NE.'Unavailable') THEN
        CALL LIB$GET_LUN(DLUN)
        OPEN(UNIT=DLUN,FILE='[HORACE]BUKDAT.DAT',STATUS='NEW',       
     &      IOSTAT=ISTAT,recl=140)
        IF(ISTAT.NE.0) THEN               !If create failed
          CALL LIB$FREE_LUN(DLUN)   !Release LUN
          DLUN=1000                 !Show stream free
         ELSE
          print *,'[HORACE]BUKDAT.DAT open',DLUN 
         CALL GET_LOGICAL('FLIGHT_NUMBER',CFLTNO)
         CALL FWVSMSG(DLUN,CFLTNO)   
        ENDIF
      ENDIF
      RETURN
      END

!*****************************************************************************
      SUBROUTINE FWVSMSG(DLUN,CLINE)

      CHARACTER CLINE*132
      INTEGER*4 IMESS(33),DLUN
      INTEGER*4 J1,J2,JJHRS,JJMIN,JJSEC,IDTIM

      INTEGER*2 NFDATA(2048,2),NPLOC(512),NPFREQ(512),STATUS(256), 
     &         SPECIAL(12800,10)
      REAL*4    DERIVE(12800,196)
      COMMON    /HCOM/ NFDATA,NPLOC,NPFREQ,STATUS,DERIVE,SPECIAL
      VOLATILE /HCOM/

      IF(STATUS(33).EQ.1) THEN     !If have good DRS data check times
            J1=NFDATA(10,STATUS(1))    !Decode DRS time into secs past midnight
            J2=NFDATA(11,STATUS(1))
            JJHRS=IBITS(J1,8,4)*10+IBITS(J1,4,4)
            JJMIN=IBITS(J1,0,4)*10+IBITS(J2,8,4)
            JJSEC=IBITS(J2,4,4)*10+IBITS(J2,0,4)
            IDTIM=JJHRS*3600+JJMIN*60+JJSEC !DRS seconds past midnight
            IMESS(33)=IDTIM
      ENDIF
      WRITE(DLUN,'(I6.6,X,A)')IDTIM,CLINE
!      WRITE(DLUN,'(I6.6,A1,A)')IDTIM,',',TRIM(CLINE)
      RETURN
      END
!************************************************************************


      SUBROUTINE CLOSE_FWVS(DLUN)
        INTEGER*4 DLUN
        IF(DLUN.NE.1000)CLOSE(DLUN)
        print *,'CLOSE_FWVS'
      RETURN
      END
!*******************************************************************************

      SUBROUTINE GET_DISK_STATUS(CSYSSTS,COPTSTS)
!
! Reads the group logical names OPTI_STATUS and SYST_STATUS
!
! V1.00            W.D.N.JACKSON
! V1.01  10/08/92  W.D.N.JACKSON
!        Now also read SYST_STATUS
!
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
      JLIST(1)=20
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
!*******************************************************************************
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

!*****************************************************************************
 
        FUNCTION htons( port )
        INTEGER*2 port,HTONS
 
!       ** swap the two bytes of 'port' around
 
        htons=0
        call mvbits(port,0,8,htons,8)
        htons=htons.AND.'FF00'X
        call mvbits(port,8,8,htons,0)
 
        END
 
!*******************************************************************************
      SUBROUTINE opensocket(jchan,a,b,c,d,port)

      IMPLICIT  NONE
      INCLUDE   'SYS$LIBRARY:TCPIP$INETDEF.FOR'
      INCLUDE   '($IODEF)'
      INCLUDE   '($SYSSRVNAM)'
      INCLUDE   '(lib$routines)'
      STRUCTURE /socket/

       INTEGER*2 CHANNEL
       INTEGER*4  EVNF
       INTEGER*2 JOSB(4)

      END STRUCTURE
      RECORD /SOCKET/ jchan
      INTEGER*2 SCKPARM(2),port
      INTEGER*4   a, b, c, d,f,IFUNC,ISTAT
      INTEGER*2   htons
      RECORD / sockaddrin / remote_host
      STRUCTURE / struct_il2 /
          INTEGER*4 il2_length
          INTEGER*4 il2_address       ! address
      END STRUCTURE
      RECORD /struct_il2/ rhst_adrs
      ISTAT = lib$get_ef( jchan.EVNF )
      IF (.NOT. ISTAT) CALL lib$stop( %VAL( ISTAT ) )
      IF (jchan.EVNF .EQ. -1) CALL lib$stop( %VAL( 0 ) )
      rhst_adrs.il2_length  = sizeof( remote_host )
      rhst_adrs.il2_address = %LOC( remote_host )
      remote_host.SIN$W_FAMILY = TCPIP$C_AF_INET    ! INET family
 
!      PRINT *, 'Server address = ', a, '.', b, '.', c, '.', d
!      PRINT *, 'Server port    = ', port
!       ** sin$l_adr needs to store an IP address of form a.b.c.d
!       ** in the byte order d:c:b:a
        
      call mvbits(d,0,8,f,24)
      call mvbits(c,0,8,f,16)
      call mvbits(b,0,8,f,8)
      call mvbits(a,0,8,f,0)
      remote_host.sin$l_addr = f
      remote_host.sin$w_port = htons( port )
 
!      PRINT *, 'Server host address as an integer = ',      &
!                   remote_host.sin$l_addr
!      PRINT *, 'Server port in network format     = ',      &
!                   remote_host.sin$w_port
!
! Assign the device
! 
      ISTAT=SYS$ASSIGN('TCPIP$DEVICE:',JCHAN.channel,,)
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
!
!  Create the socket
!
      IFUNC=IO$_SETMODE
      SCKPARM(1)=TCPIP$C_TCP
      SCKPARM(2)=INET_PROTYP$C_STREAM
      ISTAT=SYS$QIOW(%VAL(jchan.EVNF),%VAL(JCHAN.channel), 
     &   %VAL(IFUNC),jchan.JOSB,,,   
     &   SCKPARM,,,,,)
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      IF(.NOT.jchan.JOSB(1)) CALL LIB$STOP(%VAL(jchan.JOSB(1)))
!
!  Connect to specified host and port number
!
      IFUNC=IO$_ACCESS
      ISTAT=SYS$QIOW(%VAL(jchan.EVNF),%VAL(JCHAN.channel), 
     &   %VAL(IFUNC),jchan.JOSB,,,   
     &   ,,%REF(rhst_adrs),,,)
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      IF(.NOT.jchan.JOSB(1)) CALL LIB$STOP(%VAL(jchan.JOSB(1)))
      RETURN
      END
!*******************************************************************************
      SUBROUTINE readsocket(JCHAN,ILCIN,IREF)
!
      IMPLICIT  NONE

      INCLUDE   'SYS$LIBRARY:TCPIP$INETDEF.FOR'
      INCLUDE   '($IODEF)'
      INCLUDE   '($SYSSRVNAM)'
      INTEGER*4 ILCIN,IREF
      INTEGER*4 IFUNC,ISTAT
      STRUCTURE /socket/

       INTEGER*2 CHANNEL
       INTEGER*4  EVNF
       INTEGER*2 JOSB(4)

      END STRUCTURE
      RECORD/SOCKET/JCHAN
!
! Read some data
!
!      print *,'readsocket'
      IFUNC=IO$_READVBLK
      ISTAT=SYS$QIOW(%VAL(jchan.EVNF),%VAL(JCHAN.channel), 
     &       %VAL(IFUNC),jchan.JOSB,,,   
     &       IREF,%VAL(ILCIN),,,,)
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      IF(.NOT.jchan.JOSB(1)) CALL FWVS_STOPS
!      print *,IREF
      RETURN
      END

C*******************************************************************************
      SUBROUTINE writesocket(JCHAN,COUT)
!
      IMPLICIT  NONE
      INTEGER*4 ILCOUT
      CHARACTER COUT*(*)
      INCLUDE   'SYS$LIBRARY:TCPIP$INETDEF.FOR'
      INCLUDE   '($IODEF)'
      INCLUDE   '($SYSSRVNAM)'
      INTEGER*4 IFUNC,ISTAT
      STRUCTURE /socket/
       INTEGER*2 CHANNEL
       INTEGER*4  EVNF
       INTEGER*2 JOSB(4)
      END STRUCTURE
      RECORD/SOCKET/JCHAN
!
! Write some data
!
!      print *,'writesocket ',cout
      ILCOUT=LEN(COUT)
      IFUNC=IO$_WRITEVBLK
      ISTAT=SYS$QIOW(%VAL(jchan.EVNF),%VAL(JCHAN.channel), 
     &       %VAL(IFUNC),jchan.JOSB,,,   
     & %REF(COUT),
     &%VAL(ILCOUT),,,,)
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      IF(.NOT.jchan.JOSB(1)) CALL FWVS_STOPS
      RETURN
      END
C*******************************************************************************
!
      SUBROUTINE FWVS_STOPS(DLUN)
       INTEGER*4 DLUN

      STRUCTURE /socket/
       INTEGER*2 CHANNEL
       INTEGER*4  EVNF
       INTEGER*2 JOSB(4)
      END STRUCTURE

      RECORD / SOCKET / FWVS
      COMMON /CHANS/FWVS
        CALL CLOSE_FWVS(DLUN)
       CALL CLOSESOCKET(FWVS)
       CALL EXIT
      RETURN
      END
     

!*******************************************************************************
!*******************************************************************************
      SUBROUTINE closesocket(JCHAN)

      IMPLICIT  NONE
      INCLUDE   'SYS$LIBRARY:TCPIP$INETDEF.FOR'
      INCLUDE   '($IODEF)'
      INCLUDE   '($SYSSRVNAM)'
      INCLUDE '(lib$routines)'
      INTEGER*4 ISTAT,IFUNC
      STRUCTURE /socket/

       INTEGER*2 CHANNEL
       INTEGER*4  EVNF
       INTEGER*2 JOSB(4)

      END STRUCTURE
      RECORD/SOCKET/JCHAN
!
! Shutdown the socket
!
      IFUNC=IO$_DEACCESS.OR.IO$M_SHUTDOWN
      ISTAT=SYS$QIOW(%VAL(jchan.EVNF),%VAL(JCHAN.channel), 
     &  %VAL(IFUNC),jchan.JOSB,,,   
     &  ,,,%VAL(TCPIP$C_DSC_ALL),,)
!      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
!      IF(.NOT.JOSB(1)) CALL LIB$STOP(%VAL(JOSB(1)))

! Close the socket

      IFUNC=IO$_DEACCESS
      ISTAT=SYS$QIOW(%VAL(jchan.EVNF),%VAL(JCHAN.channel), 
     & %VAL(IFUNC),jchan.JOSB,,,
     & ,,,,,)
!      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
!      IF(.NOT.jchan.JOSB(1)) CALL LIB$STOP(%VAL(jchan.JOSB(1)))

! Deassign the device

      ISTAT=SYS$DASSGN(%VAL(JCHAN.channel))
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
!
!       ** free the event flag
 
      ISTAT = lib$free_ef( %REF( jchan.EVNF ) )
      IF (.NOT. ISTAT) CALL lib$stop( %VAL(ISTAT ) )
!       ** free the event flag
 
 
!       -------------------------------------------------------
!       ** Inform user that program ran to completion:
 
!       ** if yo
!       ** under the debugger to see where it calls lib$stop()
 
 
      PRINT *, 'Socket closed and exiting'
!      CALL EXIT
      RETURN
      END
!*******************************************************************************
! --------------------------------------------------------------------------
      

