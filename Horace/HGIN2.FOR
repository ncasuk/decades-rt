       PROGRAM ucx_tcp_server_qio_for2
 
C       ** Modified version of ucx_tcp_server_qio_for.for
C       which sits waiting for a client connection. When
C       a cient connects, it waits for the client to send
C       messages, and displays them. If the message is 'exit',
C       this program finishes.
 
C       ** If the client terminates, this sets a SS$_LINKDISCON
C       status in the $QIOW IOSB. This program then deassigns
C       the channel; assigns a new one and awaits for a new
C       client to connect.
 
 
 
C       This is a FORTRAN version of the TCP/IP Services for OpenVMS
C       example program UCX$EXAMPLES:UCX$TCP_SERVER_QIO.C
 
C       The best reference for SYS$QIO programming for socket usage is
C       the "TCP/IP Services for OpenVMS: System Services and C Socket
C       Programming" manual, chapter 3.
 
C       To build:
C           $ FORTRAN ucx_tcp_server_qio_for
C           $ LINK    ucx_tcp_server_qio_for
 
C       To run:
C           $ RUN     ucx_tcp_server_qio_for
 
C       Build and run a client program such as
C       UCX$EXAMPLES:UCX$TCP_CLIENT_QIO.C, UCX$EXAMPLES:UCX$TCP_CLIENT_IPC.C
C       or UCX_TCP_CLIENT_QIO_FOR.FOR which connects to the server and sends
C       a message to the server. The server simply displays the message and
C       then exits.
 
 
C       John Wood    Compaq Computer (UK) Ltd   January 1999
 
 
        IMPLICIT NONE
 
C       ** include useful system definitions
        INCLUDE '(lib$routines)'
        INCLUDE '($syssrvnam)'
        INCLUDE '($iodef)'
        INCLUDE '($ssdef)'
        INCLUDE 'sys$library:ucx$inetdef.for'
 
 
C       ** declare variables
        INTEGER*4   buflen
        INTEGER*4   retval
        INTEGER*4	status
        INTEGER*4   r_retlen
        INTEGER*4   one
        INTEGER*4   a, b, c, d
        INTEGER*4   flags
        INTEGER*4   efn     ! ** event flag number for SYS$QIO
 
        INTEGER*2   port, port_1
        INTEGER*2   channel, channel_1, channel_2
        INTEGER*2   sck_parm(2)
 
        INTEGER*4   client_addr
        INTEGER*1   bytes(4)
        EQUIVALENCE (client_addr, bytes)
 
 
C       ** there are various ways to define the I/O status block:
CCC        INTEGER*2   iosb(4)
 
        STRUCTURE / iosb_struct /
            INTEGER*2 status
            INTEGER*2 transfer_size
            INTEGER*4 address
        END STRUCTURE
        RECORD /iosb_struct/ iosb
 
        CHARACTER*512 buf
 
 
        RECORD / sockaddrin / local_host, remote_host
 
        STRUCTURE / struct_il2 /
            INTEGER*4 il2_length
            INTEGER*4 il2_address       ! address
        END STRUCTURE
        RECORD /struct_il2/ lhst_adrs
 
        STRUCTURE / struct_il3 /
            INTEGER*4 il3_length
            INTEGER*4 il3_address       ! address
            INTEGER*4 il3_retlen        ! address
        END STRUCTURE
        RECORD /struct_il3/ rhst_adrs
 
        STRUCTURE / struct_ssp /
            INTEGER*2 len
            INTEGER*2 param
            INTEGER*4 ptr               ! address
        END STRUCTURE
        RECORD / struct_ssp / options, item_list(1)
 
 
C       ** function declarations
 
        INTEGER*2   htons
 
 
C *************************************************
 
        buflen = SIZEOF( buf )
        one = 1
 
C       ** get an available event flag
        status = lib$get_ef( efn )      ! efn by %REF by default
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
        IF (efn .EQ. -1) CALL lib$stop( %VAL( 0 ) )
 
 
        item_list(1).len   = SIZEOF( one )
        item_list(1).param = ucx$c_reuseaddr
        item_list(1).ptr   = %LOC( one )
 
        options.len   = SIZEOF( item_list )
        options.param = ucx$c_sockopt
        options.ptr   = %LOC( item_list )
 
 
        local_host.SIN$W_FAMILY = UCX$C_AF_INET     ! INET family
        local_host.SIN$L_ADDR = UCX$C_INADDR_ANY    ! Any address
 
        lhst_adrs.il2_length  = SIZEOF( local_host )
        lhst_adrs.il2_address = %LOC( local_host )
 
 
        rhst_adrs.il3_length  = SIZEOF( remote_host )
        rhst_adrs.il3_address = %LOC( remote_host )
        rhst_adrs.il3_retlen  = %LOC( r_retlen )
 
 
 
C        GOTO 110        ! ** for testing, use hard-coded port number
 
        WRITE (6, *) ' Enter port number '
        READ (5,*) port
        GOTO 120
 
 110    CONTINUE    ! don't worry about compile-time warnings for this line
C       ** for testing, skip prompting user for port
        port = 4747
 
 
 120    CONTINUE
 
        local_host.sin$w_port = htons( port )
 
        WRITE (6,*) 'Server will use port ', port
        WRITE (6,*) 'Server port in network format = ',
     &              local_host.sin$w_port
 
 
C       ----------------------------------------
C	    ** Assign two channels to the UCX device
C       ** (These calls will fail if UCX is not started on the system...)
 
C       ** the string 'ucx$device' is passed by descriptor by default
 
        status = sys$assign( 'ucx$device', channel, ,  )
        if (.not. status) call lib$stop( %val(status) )
 
 
        status = sys$assign( 'ucx$device', channel_2, ,  )
        if (.not. status) call lib$stop( %val(status) )
 
 
C       -------------------------------------------------
C	    ** Create the socket and set the REUSEADDR option
 
        sck_parm(1) = UCX$C_TCP             ! TCP/IP protocol
        sck_parm(2) = INET_PROTYP$C_STREAM  ! stream type of socket
 
        status = sys$qiow( %VAL( efn ),
     &                     %VAL( channel ),
     &                     %VAL( io$_setmode ),
     &                     %REF( iosb ),
     &                     ,
     &                     ,
     &                     %REF( sck_parm ),    ! p1
     &                     ,                    ! p2
     &                     ,                    ! p3
     &                     ,                    ! p4
     &                     options,             ! p5
     &                     )                    ! p6
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
        status = iosb.status
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
 
 
C       ------------------------------------------------------------
C	    ** Bind to chosen port number (after REUSEADDR is set above)
 
        status = sys$qiow( %VAL( efn ),
     &                     %VAL( channel ),
     &                     %VAL( io$_setmode ),
     &                     %REF( iosb ),
     &                     ,
     &                     ,
     &                     ,            ! p1
     &                     ,            ! p2
     &                     lhst_adrs,   ! p3 : local socket name
     &                     %VAL( 3 ),   ! p4 : Connection backlog
     &                     ,            ! p5
     &                     )            ! p6
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
        status = iosb.status
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
 
        GOTO 222
 
C       -----------------------------------------------
C       ** If here, a client has closed the connection.
C       ** So we de-assign the channel, ready to re-assign it below.
 
 200    CONTINUE
 
        status = sys$dassgn( %VAL(channel_1) )
        IF (.NOT. status) call lib$stop( %VAL(status) )
        TYPE *, 'Lost connection with client; awaiting new connection'
 
 
C       ------------------------------------
C       ** Accept a connection from a client
 
 222    CONTINUE
        status = sys$assign( 'ucx$device', channel_1, ,  )
        IF (.NOT. status) CALL lib$stop( %VAL(status) )
 
        status = sys$qiow( %VAL( efn ),
     &                     %VAL( channel ),
     &                     %VAL( io$_access .OR. io$m_accept ),
     &                     %REF( iosb ),
     &                     ,
     &                     ,
     &                     ,            ! p1
     &                     ,            ! p2
     &                     rhst_adrs,   ! p3 : Remote IP address
     &                     channel_1,   ! p4 : Channel for new socket
     &                     ,            ! p5
     &                     )            ! p6
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
        status = iosb.status
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
 
 
        port_1 = htons( remote_host.sin$w_port )
 
C       ** extract & display IP address from remote host
 
C       ** copy client address into equivalenced datum
        client_addr = remote_host.sin$l_addr
 
C       ** rem: 16.37.144.139 = 0x10 0x25 0x90 0x8B
C       ** in remote_host.sin$l_addr format, = 0x8B 90 25 10 = d.c.b.a
 
C       ** rem: INTEGER*1 bytes(4) is equivalenced to client_addr
        a = bytes(1)
        b = bytes(2)
        c = bytes(3)
        d = bytes(4)
        if (a .lt. 0) a = a + 256
        if (b .lt. 0) b = b + 256
        if (c .lt. 0) c = c + 256
        if (d .lt. 0) d = d + 256
 
 
        WRITE (6,601) client_addr, a, b, c, d, port_1
 601    FORMAT ( ' Connection from client (', I, '): ', I3, '.',
     &           I3, '.', I3, '.', I3, '; port ', I5 )
 
 
C       ------------------
C	    ** Read I/O buffer
 
 300    continue
 
        status = sys$qiow( %VAL( efn ),
     &                     %VAL( channel_1 ),
     &                     %VAL( io$_readvblk ),
     &                     %REF( iosb ),
     &                     ,
     &                     ,
     &                     %REF( buf ),     ! p1 : buffer
     &                     %VAL( buflen ),  ! p2 : buffer length
     &                     ,                ! p3
     &                     ,                ! p4
     &                     ,                ! p5
     &                     )                ! p6
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
        status = iosb.status
C       ** check that client hasn't closed the connection; if so, go to 200
        IF (status .eq. SS$_LINKDISCON) goto 200
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
 
 
C       ** If all is well, print message (all 512 bytes!)
        TYPE *, 'Received ', iosb.transfer_size, ' bytes; text: ', buf
 
        IF (buf .EQ. 'exit') GOTO 400
 
        goto 300
 
C       ----------------------------------
C	    ** Shut down the socket (optional)
 
 400    continue
 
        status = sys$qiow( %VAL( efn ),
     &                     %VAL( channel_1 ),
     &                     %VAL( io$_deaccess .or. io$m_shutdown ),
     &                     %REF( iosb ),
     &                     ,
     &                     ,
     &                     ,                       ! p1
     &                     ,                       ! p2
     &                     ,                       ! p3
     &                     ,                       ! p4
     &                     ,                       ! p5
     &                     )                       ! p6
        IF (.NOT. status) CALL lib$stop( %val(status) )
        status = iosb.status
        IF (.NOT. status) CALL lib$stop( %val(status) )
 
 
C       -------------------------------------------------------
C       ** Close the sockets -- accepted and listner (optional)
 
        status = sys$qiow( %VAL( efn ),
     &                     %VAL( channel_1 ),
     &                     %VAL( io$_deaccess ),
     &                     %REF( iosb ),
     &                     ,
     &                     ,
     &                     ,                ! p1
     &                     ,                ! p2
     &                     ,                ! p3
     &                     ,                ! p4
     &                     ,                ! p5
     &                     )                ! p6
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
        status = iosb.status
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
 
 
        status = sys$qiow( %VAL( efn ),
     &                     %VAL( channel ),
     &                     %VAL( io$_deaccess ),
     &                     %REF( iosb ),
     &                     ,
     &                     ,
     &                     ,                ! p1
     &                     ,                ! p2
     &                     ,                ! p3
     &                     ,                ! p4
     &                     ,                ! p5
     &                     )                ! p6
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
        status = iosb.status
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
 
 
C       -------------------------------------------------------
C       ** Deassign the UCX device channels
 
        status = sys$dassgn( %VAL( channel_1 ) )
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
 
        status = sys$dassgn( %VAL( channel ) )
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
 
 
C       ** free the event flag
        status = lib$free_ef( efn )     ! passed by %REF by default
        IF (.NOT. status) CALL lib$stop( %VAL( status ) )
 
 
C       -------------------------------------------------------
C       ** Inform user that program ran to completion:
 
C       ** if you don't see the following msg, then run the program
C       ** under the debugger to see where it calls lib$stop()
 
        PRINT *, 'All is well that ends well'
 
        END
 
C -----------------
 
        INTEGER*2 FUNCTION htons( port )
        INTEGER*2 port
        INTEGER*2 high, low
 
C       ** swap the two bytes of 'port' around
 
        low = IMOD( port, 256 )
        high = port / 256
 
        htons = (256*low) + high
 
        END
 
 
--------------------------------------------------------------------------------
 
        PROGRAM ucx_tcp_client_qio_for2
 
C       ** Modified version of ucx_tcp_client_qio_for.for
C       which makes a socket connection to a server. It
C       prompts the user to enter a message which it sends
C       to the server. If the message is 'exit', the client
C       sends 'exit' to the server and then finishes. If the
C       message is 'quit', no message is sent to the server:
C       this program just finishes.
 
C       ** After sending any message other than 'exit', this program
C       loops; prompting the user for another message to be sent.
 
 
C       This is a FORTRAN version of the TCP/IP Services for OpenVMS
C       example program UCX$EXAMPLES:UCX$TCP_CLIENT_QIO.C
 
C       The best reference for SYS$QIO programming for socket usage is
C       the "TCP/IP Services for OpenVMS: System Services and C Socket
C       Programming" manual, chapter 3.
 
C       To build:
C           $ FORTRAN ucx_tcp_client_qio_for
C           $ LINK    ucx_tcp_client_qio_for
 
C       To run, first build & run ones of the following server programs:
C           UCX$EXAMPLES:UCX$TCP_SERVER_QIO.C
C           UCX$EXAMPLES:UCX$TCP_SERVER_IPC.C
C                        UCX_TCP_SERVER_QIO_FOR.FOR
 
C       Then run this program to connect to the server:
 
C           $ RUN ucx_tcp_client_qio_for
 
C       This client sends a simple message ('hello there') to the server.
 
 
C       John Wood    Compaq Computer (UK) Ltd    January 1999
 
 
        IMPLICIT NONE
 
C       ** include useful system definitions
        INCLUDE '(lib$routines)'
        INCLUDE '($syssrvnam)'
        INCLUDE '($iodef)'
        INCLUDE 'sys$library:ucx$inetdef.for'
 
 
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
 
 
        sck_parm(1) = UCX$C_TCP             ! TCP/IP protocol
        sck_parm(2) = INET_PROTYP$C_STREAM  ! stream type of socket
 
 
        remote_host.SIN$W_FAMILY = UCX$C_AF_INET    ! INET family
 
 
C        GOTO 110    ! skip prompting user for IP server details during testing
 
        TYPE *, 'Enter IP address and port number for server:'
        TYPE *, 'use comma "," not . to separate IP address parts'
        TYPE *, 'E.g.   "16,37,144,139,  4747"  for 16.37.144.139 4747'
 
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
C       ** assign a channel to the UCX device
 
        print *, 'Assigning a channel to the UCX device'
 
C       ** the string 'ucx$device' is passed by descriptor by default
 
        status = sys$assign( 'ucx$device', channel, ,  )
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
     &                     %REF( sck_parm ),    ! p1 : Socket creation
parameter
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
     &                     %VAL( ucx$c_dsc_all ), ! p4 : Discard all packets
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
C       ** Deassign the UCX device channel
 
        print *, 'Deassigning the UCX device channel'
 
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
 
        htons = (256*low) + high
 
        END
 
