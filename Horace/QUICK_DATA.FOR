!
! ROUTINE          QUICK_DATA PROGRAM FORTVAX
!
! PURPOSE          Writes derived HORACE parameters to file
!
! DESCRIPTION      Writes data from HORACE's global common block of derived
!                  parameter values to file.  Start and end times must be
!                  specified, together with required parameters.
!
!                  Note that HORACE keeps derived data in its global common
!                  for about 15 hours, although it may occasionally delete 
!                  all data, if for instance the DRS data time goes backwards.
!
!                  This program can be run at any time without impacting on
!                  any of the processes normally running on HORACE.
!
!                  Values are written for each 3s interval, all values being
!                  fixed format and written on a single line.  Up to about 70
!                  parameters can be written.
!
!                  The program must be run in the HORACE directory.
!
! VERSION          1.00  09-06-04  W.D.N.JACKSON
!
! ARGUMENTS        The programs takes the following from SYS$INPUT:
!
!                     Processing start time (hh:mm:ss)
!                     Processing end time (hh:mm:ss)
!                     The name of the required output file
!                     Required parameter numbers, one on each line and
!                      terminated with a -1
!
! CHANGES          
!
********************************************************************************
      PROGRAM QUICK_DATA
!
! Build with   $ FORT QUICK_DATA
!              $ LINK QUICK_DATA,H_COMMON/OPT
!              $ RUN QUICK_DATA
!
      INTEGER*4 ISRTTIM,IENDTIM,IOS,IOS1,IOS2,IOS3,IPC,IPARA,IP(196),
     &    IHRS,IMIN,ISEC,IC,I,IPTR,II
      CHARACTER CLINE*80,CREPLY*80,CPARA(196)*80,CWDAT*9,CWTIM*8
      INCLUDE 'HCOM_DEF.FOR'

      WRITE(6,'(A)') ' '
      WRITE(6,'(A)') 
     &    ' QUICK_DATA - writes derived HORACE data to file'
      WRITE(6,'(A)') ' '
!
! Get data start and end times
!
      ISRTTIM=-1
      DO WHILE(ISRTTIM.EQ.-1)
        WRITE(6,'(A)') '$Start time (hh:mm:ss): '
        READ(5,'(A)',IOSTAT=IOS) CLINE
        IF(IOS.EQ.-1) CALL EXIT
        IF (CLINE.NE.' ') THEN
          READ(CLINE(1:2),'(I2.2)',IOSTAT=IOS1) IHRS
          READ(CLINE(4:5),'(I2.2)',IOSTAT=IOS2) IMIN
          READ(CLINE(7:8),'(I2.2)',IOSTAT=IOS3) ISEC
          IF(IOS1.EQ.0.AND.IOS2.EQ.0.AND.IOS3.EQ.0.AND.
     -        IHRS.GE.0.AND.IHRS.LE.23.AND.IMIN.GE.0.AND.IMIN.LE.59.AND.
     -        ISEC.GE.0.AND.ISEC.LE.59) ISRTTIM=IHRS*3600+IMIN*60+ISEC
        END IF
      END DO
!
      IENDTIM=-1
      DO WHILE(IENDTIM.EQ.-1)
        WRITE(6,'(A)') '$End time (hh:mm:ss): '
        READ(5,'(A)',IOSTAT=IOS) CLINE
        IF(IOS.EQ.-1) CALL EXIT
        IF (CLINE.NE.' ') THEN
          READ(CLINE(1:2),'(I2.2)',IOSTAT=IOS1) IHRS
          READ(CLINE(4:5),'(I2.2)',IOSTAT=IOS2) IMIN
          READ(CLINE(7:8),'(I2.2)',IOSTAT=IOS3) ISEC
          IF(IOS1.EQ.0.AND.IOS2.EQ.0.AND.IOS3.EQ.0.AND.
     -        IHRS.GE.0.AND.IHRS.LE.23.AND.IMIN.GE.0.AND.IMIN.LE.59.AND.
     -        ISEC.GE.0.AND.ISEC.LE.59) IENDTIM=IHRS*3600+IMIN*60+ISEC
        END IF
      END DO
!
! Get required output file name
!
      WRITE(6,'(A)') '$Output file [[ANONYMOUS]QUICK_DATA.TXT]: '
      READ(5,'(A)',IOSTAT=IOS) CREPLY
      IF(IOS.EQ.-1) CALL EXIT
      IF(CREPLY.EQ.' ') CREPLY='[ANONYMOUS]QUICK_DATA.TXT'
      OPEN(20,FILE=CREPLY,STATUS='NEW',CARRIAGECONTROL='LIST',
     &    RECL=1024)
      CALL DATE(CWDAT)
      CALL TIME(CWTIM)
      WRITE(20,'(A)') '  FAAM  '//CFNUM//'  '//CFDAT//
     &    '  Quick look real-time data written at '//CWTIM//' '
     &    //CWDAT
!
! Get list of required parameters
!      
      IPC=0
      DO WHILE(CREPLY.NE.'-1')
        WRITE(6,'(A)') '$HORACE parameter number (-1 to end list): '
        READ(5,'(A)',IOSTAT=IOS) CREPLY
        IF(IOS.EQ.-1) CALL EXIT
        READ(CREPLY,'(I)',IOSTAT=IOS) IPARA
        IF(IPARA.GE.512+1.AND.IPARA.LE.512+196) THEN
          IPC=IPC+1
          IP(IPC)=IPARA-512
        ELSE
          IF(IPARA.NE.-1) WRITE(6,'(A)') ' Illegal parameter'
        END IF
      END DO
!
! Write headers
!
      OPEN(21,FILE='PARANO.TXT',STATUS='OLD',READONLY)
      READ(21,'(A)',IOSTAT=IOS) CLINE
      DO WHILE(IOS.EQ.0)
        READ(CLINE(1:3),'(I3)',IOSTAT=IOS1) II
        IF(IOS1.EQ.0.AND.II.GE.512+1.AND.II.LE.512+196) 
     &      CPARA(II-512)=CLINE
        READ(21,'(A)',IOSTAT=IOS) CLINE
      END DO
      CLOSE(21)
      WRITE(20,'(2X,100A)') 
     &    (CPARA(IP(I))(1:4)//CPARA(IP(I))(14:23),I=1,IPC)
      WRITE(20,'(2X,100A)') (CPARA(IP(I))(55:68),I=1,IPC)
!
! Search for start of data
!
      IPTR=STATUS(2)
      IC=0
      DO WHILE(RSECS(IPTR).GT.ISRTTIM.AND.RSECS(IPTR).NE.0.0.
     &    AND.IC.LT.STATUS(3))
        IPTR=IPTR-1
        IF(IPTR.LE.0) IPTR=IPTR+12800
        IC=IC+1
      END DO
!
! Output the required data
!
      DO WHILE(RSECS(IPTR).LT.IENDTIM.AND.RSECS(IPTR).NE.0.0)
        WRITE(20,'(100(1PE14.6E2))') (DERIVE(IPTR,IP(I)),I=1,IPC)
        IPTR=IPTR+1
        IF(IPTR.GT.12800) IPTR=1
      END DO
!
! Exit
!
      CLOSE(20)
      CALL EXIT
      END
