      PROGRAM HGIN

      INCLUDE   'SYS$LIBRARY:TCPIP$INETDEF.FOR'
      INCLUDE   '($IODEF)'
      INCLUDE   '($SYSSRVNAM)'
!
! Assign the device
!
      ISTAT=SYS$ASSIGN('EWA0:',JCHAN,,)
      print *,ISTAT
      IF(.NOT.ISTAT) print *,'Stopping - failed to assign device'
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      END

