      PROGRAM ADDCRLF
!
! Reads each line from SYS$INPUT, adds a cr/lf, and outputs the line to 
! SYS$OUTPUT.  Needed when sending DCL output to SYS$NET when using the 
! auxiliary server.  Best used with the PIPE command.
!
! Used by the H_JAVASERVE.FOR program when spawning DCL commands whose output
! is to be sent back over the network connection.
!
! V1.00  08/08/02  W.D.N.JACKSON
!
      CHARACTER CLINE*80
      INTEGER*4 IL,IOS

      OPEN(10,FILE='SYS$INPUT',STATUS='OLD')
      OPEN(11,FILE='SYS$OUTPUT',STATUS='NEW',CARRIAGECONTROL='NONE')
      READ(10,'(Q,A)',IOSTAT=IOS) IL,CLINE
      DO WHILE(IOS.EQ.0)
        IF(IL.GT.0) THEN
          WRITE(11,'(A)') CLINE(1:MIN(80,IL))//CHAR(10)//CHAR(13)
        ELSE
          WRITE(11,'(A)') CHAR(10)//CHAR(13)
        END IF
        READ(10,'(Q,A)',IOSTAT=IOS) IL,CLINE
      END DO
      CLOSE(10)
      CLOSE(11)
      CALL EXIT
      END
