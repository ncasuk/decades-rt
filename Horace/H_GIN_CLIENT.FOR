 
        PROGRAM h_gin_client
 
 
C       To build:
C           $ FORTRAN h_gin_client
C           $ LINK    h_gin_client
 
C       Then run this program to connect to the server:
 
C           $ RUN h_gin_client
 
 
 
        IMPLICIT NONE
 
C       ** include useful system definitions
        INCLUDE '(lib$routines)'
        INCLUDE '($syssrvnam)'
        INCLUDE '($iodef)'
        INCLUDE 'sys$library:tcpip$inetdef.for'
        INCLUDE '($CVTDEF)'
 
C       ** declare variables
        INTEGER*4   buflen
        INTEGER*4   status
        INTEGER*4   a, b, c, d, e,f
        INTEGER*4   flags
        INTEGER*4   efn     ! ** event flag number for SYS$QIO
        INTEGER*4 packet_no,ISTAT,I
 

        INTEGER*2   port
        INTEGER*2   channel
        INTEGER*2   sck_parm(2)
 
C       ** there are various ways to define the I/O status block:
C       ** you could declare a structure, but this program treats
C       ** it as an array of 16-bit values
 
        INTEGER*2   iosb(4)
 
        CHARACTER*512 buf / 'Hello There' /
        BYTE GRPMESS(512)
        BYTE latbyte(8),lonbyte(8)

        REAL*8 latdbl,londbl
        REAL*8 latdbl1,londbl1
        CHARACTER grp_start*4
        INTEGER*2 grp_ID,byte_count
 
        RECORD / sockaddrin / remote_host
 
        STRUCTURE / struct_il2 /
            INTEGER*4 il2_length
            INTEGER*4 il2_address       ! address
        END STRUCTURE
        RECORD /struct_il2/ rhst_adrs
 
 
C       ** function declarations
 
        INTEGER*2   htons
        EQUIVALENCE(latbyte,latdbl)
        EQUIVALENCE(lonbyte,londbl)
     
 
C ---------------------------------------------------------------
 
        buflen = sizeof( buf )
 
C       ** get an available event flag
        status = lib$get_ef( efn )
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
        IF (efn .EQ. -1) CALL lib$stop( %VAL( 0 ) )
 
 
        rhst_adrs.il2_length  = sizeof( remote_host )
        rhst_adrs.il2_address = %LOC( remote_host )
 
 
        sck_parm(1) = TCPIP$C_TCP             ! TCP/IP protocol
        sck_parm(2) = INET_PROTYP$C_STREAM  ! stream type of socket
 
 
        remote_host.SIN$W_FAMILY = TCPIP$C_AF_INET    ! INET family
 
 
C        GOTO 110    ! skip prompting user for IP server details during testing
 
        TYPE *, 'Enter IP address and port number for GIN server:'
        TYPE *, 'use comma "," not . to separate IP address parts'
        TYPE *, 'E.g.   "151,170,108,35,5602"  for 151.170.108.35 5602'
 
!        READ (5,501) a, b, c, d, port
 501    FORMAT( I, I, I, I, I )
!        GOTO 120
 
 
 110    CONTINUE    ! can ignore compile-time warnings about this line
 
C       ** hard-code IP address of server host during testing (a.b.c.d)
        a = 192
        b = 168
        c = 102
        d = 21
 
        port = 5602
 
 120    CONTINUE
 
        PRINT *, 'Server address = ', a, '.', b, '.', c, '.', d
        PRINT *, 'Server port    = ', port
 
C       ** sin$l_adr needs to store an IP address of form a.b.c.d
C       ** in the byte order d:c:b:a

        
        call mvbits(d,0,8,f,24)
        call mvbits(c,0,8,f,16)
        call mvbits(b,0,8,f,8)
        call mvbits(a,0,8,f,0)
        remote_host.sin$l_addr = f
        
 
!        remote_host.sin$l_addr = (d * 256 * 256 * 256) + 
!     &                           (c * 256 * 256) +
!     &                           (b * 256) +
!     &                           (a)
 
        remote_host.sin$w_port = htons( port )
 
        PRINT *, 'Server host address as an integer = ',
     &              remote_host.sin$l_addr
        PRINT *, 'Server port in network format     = ',
     &              remote_host.sin$w_port
 
 
C       -------------------------------------
C       ** assign a channel to the TCPIP device
 
        print *, 'Assigning a channel to the TCPIP device'
 
C       ** the string 'tcpip$device' is passed by descriptor by default

        status = sys$assign( 'TCPIP$DEVICE:', channel, ,  )
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
 
 
C       --------------------
C       ** Create the socket
 
        PRINT *, 'Creating the socket'
 
        status = sys$qiow( %VAL( efn ),         ! Event flag
     &                     %VAL( channel ),     ! Channel number
     &                     %VAL( IO$_SETMODE ), ! I/O function
     &                     %REF( iosb ),        ! I/O status block
     &                     ,                    ! AST rtn address
     &                     ,                    ! AST parameter
     &                     %REF( sck_parm ),    ! p1 : Socket creation parameter
     &                     ,                    ! p2
     &                     ,                    ! p3
     &                     ,                    ! p4
     &                     ,                    ! p5
     &                     )                    ! p6
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
        status = iosb(1)
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
 
 
C       --------------------------------------------
C       ** Connect to specified host and port number
 
        PRINT *, 'Connecting to specified host and port number'
 
        status = sys$qiow( %VAL( efn ),         ! Event flag
     &                     %VAL( channel ),     ! Channel number
     &                     %VAL( IO$_ACCESS ),  ! I/O function
     &                     %REF( iosb ),        ! I/O status block
     &                     ,                    ! AST rtn address
     &                     ,                    ! AST parameter
     &                     ,                    ! p1
     &                     ,                    ! p2
     &                     %REF( rhst_adrs ),   ! p3 : remote IP address
     &                     ,                    ! p4
     &                     ,                    ! p5
     &                     )                    ! p6
        if (STATUS) print *,'It is Connected'
        print *,'status=',status
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
        status = iosb(1)
        if (STATUS) print *,'It really is Connected'
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
 
 
C       --------------------------------
C       ** Get message to send to server
 
 300    CONTINUE
!        TYPE *, 'Enter message to send to server'
!        TYPE *, '(or ''exit'' or ''quit'')'
 
!        READ (5,511) buf
! 511    FORMAT( A )
 
!        IF (buf .EQ. 'quit') GOTO 400
 
 

C       -------------------
C       ** Read packets
        OPEN(UNIT=123,FILE='GINDAT.DAT',STATUS='NEW',IOSTAT=ISTAT,
     &      ACCESS='SEQUENTIAL',FORM='UNFORMATTED')

        DO packet_no=1,100
 
        PRINT *, 'Reading from socket '
 
        status = sys$qiow( %VAL( efn ),		       ! Event flag
     &                     %VAL( channel ),	       ! Channel number
     &                     %VAL( IO$_READVBLK ),  ! I/O function
     &                     %REF( iosb ),           ! I/O status block
     &                     ,                       ! AST rtn address
     &                     ,                       ! AST parameter
     &                     %REF( grp_start ),            ! p1 : buffer address
     &                     %VAL( 4 ),         ! p2 : buffer length
     &                     ,                       ! p3
     &                     ,                       ! p4
     &                     ,                       ! p5
     &                     )                       ! p6
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
        status = iosb(1)
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
        print *,'Read Group Start '//grp_start
        status = sys$qiow( %VAL( efn ),		       ! Event flag
     &                     %VAL( channel ),	       ! Channel number
     &                     %VAL( IO$_READVBLK ),  ! I/O function
     &                     %REF( iosb ),           ! I/O status block
     &                     ,                       ! AST rtn address
     &                     ,                       ! AST parameter
     &                     %REF( grp_ID ),            ! p1 : buffer address
     &                     %VAL( 2 ),         ! p2 : buffer length
     &                     ,                       ! p3
     &                     ,                       ! p4
     &                     ,                       ! p5
     &                     )                       ! p6

        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
        status = iosb(1)
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
        print *,'Read Group ID ',grp_ID
        status = sys$qiow( %VAL( efn ),		       ! Event flag
     &                     %VAL( channel ),	       ! Channel number
     &                     %VAL( IO$_READVBLK ),  ! I/O function
     &                     %REF( iosb ),           ! I/O status block
     &                     ,                       ! AST rtn address
     &                     ,                       ! AST parameter
     &                     %REF( byte_count ),            ! p1 : buffer address
     &                     %VAL( 2 ),         ! p2 : buffer length
     &                     ,                       ! p3
     &                     ,                       ! p4
     &                     ,                       ! p5
     &                     )                       ! p6

        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
        status = iosb(1)
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
        print *,'Read byte count ',byte_count
        status = sys$qiow( %VAL( efn ),           ! Event flag
     &                     %VAL( channel ),       ! Channel number
     &                     %VAL( IO$_READVBLK ),  ! I/O function
     &                     %REF( iosb ),           ! I/O status block
     &                     ,                       ! AST rtn address
     &                     ,                       ! AST parameter
     &                     %REF( grpmess ),            ! p1 : buffer address
     &                     %VAL( byte_count ),         ! p2 : buffer length
     &                     ,                       ! p3
     &                     ,                       ! p4
     &                     ,                       ! p5
     &                     )                       ! p6

       WRITE(123)grp_ID,byte_count,(grpmess(I),I=1,byte_count)
       DO I=1,8
         Latbyte(I)=grpmess(I+26)
         Lonbyte(I)=grpmess(I+34)
       ENDDO
       CALL CVT$FTOF(latdbl,%VAL(CVT$K__BIG_ENDIAN_IEEE_T),
     &               latdbl1,%VAL(CVT$K_VAX_D))
       print *,latdbl,latdbl1
       CALL CVT$FTOF(londbl,%VAL(CVT$K_IEEE_T),
     &               londbl1,%VAL(CVT$K_VAX_D))
       print *,londbl,londbl1
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
        status = iosb(1)
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
 
        print *,'Read packet'
        ENDDO
        CLOSE(123)

!        IF (buf .EQ. 'exit') GOTO 400
!        GOTO 300
 
C       ----------------------------------
C       ** Shut down the socket (optional)
 
 400    continue
 
        flags = io$_deaccess .OR. io$m_shutdown
 
        PRINT *, 'Shutting down the socket'
 
        status = sys$qiow( %VAL( efn ),		      ! Event flag
     &                     %VAL( channel ),	      ! Channel number
     &                     %VAL( flags ),         ! I/O function
     &                     %REF( iosb ),          ! I/O status block
     &                     ,                      ! AST rtn address
     &                     ,                      ! AST parameter
     &                     ,                      ! p1
     &                     ,                      ! p2
     &                     ,                      ! p3
     &                     %VAL( tcpip$c_dsc_all ), ! p4 : Discard all packets
     &                     ,                      ! p5
     &                     )                      ! p6
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
        status = iosb(1)
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
 
 
C       ------------------------------
C       ** Close the socket (optional)
 
        PRINT *, 'Closing the socket'
 
        status = sys$qiow( %VAL( efn ),		      ! Event flag
     &                     %VAL( channel ),	      ! Channel number
     &                     %VAL( IO$_DEACCESS ),  ! I/O function
     &                     %REF( iosb ),          ! I/O status block
     &                     ,                      ! AST rtn address
     &                     ,                      ! AST parameter
     &                     ,                      ! p1
     &                     ,                      ! p2
     &                     ,                      ! p3
     &                     ,                      ! p4
     &                     ,                      ! p5
     &                     )                      ! p6
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
        status = iosb(1)
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
 
 
C       ----------------------------------
C       ** Deassign the TCPIP device channel
 
        print *, 'Deassigning the TCPIP device channel'
 
        status = sys$dassgn( %VAL( channel ) )
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
 
 
C       ** free the event flag
        status = lib$free_ef( %REF( efn ) )
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
 
 
C       -------------------------------------------------------
C       ** Inform user that program ran to completion:
 
C       ** if you don't see the following msg, then run the program
C       ** under the debugger to see where it calls lib$stop()
 
 
        PRINT *, 'All is well that ends well'
 
        END
 
C --------------------------------------------------------------------------
 
        INTEGER*2 FUNCTION htons( port )
        INTEGER*2 port
        INTEGER*2 high, low
 
C       ** swap the two bytes of 'port' around
 
        htons=0
        call mvbits(port,0,8,htons,8)
        htons=htons.AND.'FF00'X
        call mvbits(port,8,8,htons,0)
 
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
C*******************************************************************************
