!
! CREATE_DRS_NAMES.FOR
!
! Creates the file DRS_NAMES.TXT in [HORACE.WEB.PLOT] for use by the JAVA
! DRS plotting routine.
!
! Only puts in parameters in the current ISS which is got from DRS_ISS.DAT.
! Parameter short names are got from DRS_PARAMETERS.DAT.  Parameter long names
! are got from [HORACE]DRS_NAMES.TXT.
!
! Build with FORT CREATE_DRS_NAMES and LINK CREATE_DRS_NAMES,DRS_SUBS/LIB
!
      INTEGER*4 I,ISS,IFREQ(512)
      BYTE      BBUFF(512)
      CHARACTER CNAME(512)*4,CLONG(512)*30

      CALL DRS_ISS_READ(BBUFF,ISS)
      CALL DRS_PARAMETERS_READ(IFREQ,CNAME)
      CALL DRS_LONG_READ(CLONG)
      OPEN(10,FILE='[HORACE.WEB.PLOT]DRS_NAMES.TXT',STATUS='NEW',
     &    CARRIAGECONTROL='LIST')
      WRITE(10,'(A)') '[HORACE.WEB.PLOT]DRS_NAMES.TXT '
      WRITE(10,'(A)') ' '
      WRITE(10,'(A)') 'Created by CREATE_DRS_NAMES'
      WRITE(10,'(A)') ' '
      DO I=1,512
        IF(BBUFF(I).EQ.1) 
     &      WRITE(10,'(I5,8X,A,2X,A,7X,A)') I,CNAME(I),'DRS',CLONG(I)
      END DO
      CLOSE(10)
      CALL EXIT
      END
!*******************************************************************************
      SUBROUTINE DRS_LONG_READ(CLONG)
      INTEGER*4 IOS,IP
      CHARACTER CLONG(512)*30,CLINE*80

      OPEN(10,FILE='[HORACE]DRS_NAMES.TXT',STATUS='OLD',READONLY)
      READ(10,'(A)') CLINE
      READ(10,'(A)') CLINE
      READ(10,'(A)') CLINE
      READ(10,'(A)') CLINE
      READ(10,'(A)',IOSTAT=IOS) CLINE
      DO WHILE(IOS.EQ.0)
        READ(CLINE(1:5),'(I5)',IOSTAT=IOS) IP
        IF(IOS.EQ.0.AND.IP.GE.1.AND.IP.LE.512) CLONG(IP)=CLINE(30:59)
        READ(10,'(A)',IOSTAT=IOS) CLINE
      END DO
      CLOSE(10)
      RETURN
      END       
