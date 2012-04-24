 
        PROGRAM hgin_client
 
C       ** Modified version of tcpip_tcp_client_qio_for.for
C       which makes a socket connection to a server. It
C       prompts the user to enter a message which it sends
C       to the server. If the message is 'exit', the client
C       sends 'exit' to the server and then finishes. If the
C       message is 'quit', no message is sent to the server:
C       this program just finishes.
 
C       ** After sending any message other than 'exit', this program
C       loops; prompting the user for another message to be sent.
 
 
C       This is a FORTRAN version of the TCP/IP Services for OpenVMS
C       example program TCPIP$EXAMPLES:TCPIP$TCP_CLIENT_QIO.C
 
C       The best reference for SYS$QIO programming for socket usage is
C       the "TCP/IP Services for OpenVMS: System Services and C Socket
C       Programming" manual, chapter 3.
 
C       To build:
C           $ FORTRAN tcpip_tcp_client_qio_for
C           $ LINK    tcpip_tcp_client_qio_for
 
C       To run, first build & run ones of the following server programs:
C           TCPIP$EXAMPLES:TCPIP$TCP_SERVER_QIO.C
C           TCPIP$EXAMPLES:TCPIP$TCP_SERVER_IPC.C
C                        TCPIP_TCP_SERVER_QIO_FOR.FOR
 
C       Then run this program to connect to the server:
 
C           $ RUN tcpip_tcp_client_qio_for
 
C       This client sends a simple message ('hello there') to the server.
 
 
C       John Wood    Compaq Computer (UK) Ltd    January 1999
 
 
        IMPLICIT NONE
 
C       ** include useful system definitions
        INCLUDE '(lib$routines)'
        INCLUDE '($syssrvnam)'
        INCLUDE '($iodef)'
        INCLUDE 'sys$library:tcpip$inetdef.for'
 
 
C       ** declare variables
        INTEGER*4   buflen
        INTEGER*4	status
        INTEGER*4   a, b, c, d, e
        INTEGER*4   flags
        INTEGER*4   efn     ! ** event flag number for SYS$QIO
 
        INTEGER*2   port
        INTEGER*2   channel
        INTEGER*2   sck_parm(2)
 
C       ** there are various ways to define the I/O status block:
C       ** you could declare a structure, but this program treats
C       ** it as an array of 16-bit values
 
        INTEGER*2   iosb(4)
 
        CHARACTER*512 buf / 'Hello There' /
 
        RECORD / sockaddrin / remote_host
 
        STRUCTURE / struct_il2 /
            INTEGER*4 il2_length
            INTEGER*4 il2_address       ! address
        END STRUCTURE
        RECORD /struct_il2/ rhst_adrs
 
 
C       ** function declarations
 
        INTEGER*2   htons
 
 
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
 
        TYPE *, 'Enter IP address and port number for server:'
        TYPE *, 'use comma "," not . to separate IP address parts'
        TYPE *, 'E.g.   "151,170,108,35,4747"  for 151.170.108.35 4747'
        TYPE *, 'E.g.   "192,168, , ,5602"  for GIN 5602'
 
        READ (5,501) a, b, c, d, port
 501    FORMAT( I, I, I, I, I )
        GOTO 120
 
 
 110    CONTINUE    ! can ignore compile-time warnings about this line
 
C       ** hard-code IP address of server host during testing (a.b.c.d)
        a = 16
        b = 37
        c = 144
        d = 139
 
        port = 4747
 
 120    CONTINUE
 
        PRINT *, 'Server address = ', a, '.', b, '.', c, '.', d
        PRINT *, 'Server port    = ', port
 
C       ** sin$l_adr needs to store an IP address of form a.b.c.d
C       ** in the byte order d:c:b:a
 
        remote_host.sin$l_addr = (d * 256 * 256 * 256) +
     &                           (c * 256 * 256) +
     &                           (b * 256) +
     &                           (a)
 
        remote_host.sin$w_port = htons( port )
 
        PRINT *, 'Server host address as an integer = ',
     &              remote_host.sin$l_addr
        PRINT *, 'Server port in network format     = ',
     &              remote_host.sin$w_port
 
 
C       -------------------------------------
C       ** assign a channel to the TCPIP device
 
        print *, 'Assigning a channel to the TCPIP device'
 
C       ** the string 'tcpip$device' is passed by descriptor by default
 
        status = sys$assign( 'tcpip$device', channel, ,  )
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
 
 
C       --------------------
C       ** Create the socket
 
        PRINT *, 'Creating the socket'
 
        status = sys$qiow( %VAL( efn ),		    ! Event flag
     &                     %VAL( channel ),	    ! Channel number
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
 
        status = sys$qiow( %VAL( efn ),		    ! Event flag
     &                     %VAL( channel ),	    ! Channel number
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
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
        status = iosb(1)
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
 
 
C       --------------------------------
C       ** Get message to send to server
 
 300    CONTINUE
        TYPE *, 'Enter message to send to server'
        TYPE *, '(or ''exit'' or ''quit'')'
 
        READ (5,511) buf
 511    FORMAT( A )
 
        IF (buf .EQ. 'quit') GOTO 400
 
C       -------------------
C       ** Write I/O buffer
 
        PRINT *, 'Writing I/O buffer ', buf
 
        status = sys$qiow( %VAL( efn ),		       ! Event flag
     &                     %VAL( channel ),	       ! Channel number
     &                     %VAL( IO$_WRITEVBLK ),  ! I/O function
     &                     %REF( iosb ),           ! I/O status block
     &                     ,                       ! AST rtn address
     &                     ,                       ! AST parameter
     &                     %REF( buf ),            ! p1 : buffer address
     &                     %VAL( buflen ),         ! p2 : buffer length
     &                     ,                       ! p3
     &                     ,                       ! p4
     &                     ,                       ! p5
     &                     )                       ! p6
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
        status = iosb(1)
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
 
        IF (buf .EQ. 'exit') GOTO 400
        GOTO 300
 
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
 
        low = IMOD( port, 256 )
        high = port / 256
 
        print *,'High=',high,' Low=',low
        call mvbits(low,0,8,htons,8)
        print *,'Htons=',htons
        htons=htons.AND.'FF00'X
        print *,'Htons=',htons
        htons = htons +high
C        htons = (256*low) + high
 
        END
 
