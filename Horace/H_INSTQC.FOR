      PROGRAM H_INSTQC
!
! Purpose : To help flight leaders spot possible instrument problems.
!
! Method : Instruments are checked for problems by using a set numer of
!          methods:
!
!            DRS parameters
!               statistics based on mean of 5 seconds of data
!               statistics based on linear fit to 5 seconds of data
!               
!            Derived parameters (defined in HCOM_DEF.FOR)
!               max/min limits
!               correlation to related parameters
!               tollerances compared to related paramters
!
!          Some instruments are delt with as special cases.
!     
! instruments checked :
!     INU, GPS, OMEGA, radalt, airspeed, AoA, AoSS, static pressure,
!     cabin pressure, de-iced temperature, non de-iced temperature,
!     ICTP, LWC, TWC, General Eastern, BBRs, Heimann,
!     DRS is checked for flight number, time, reference volts
!     and signal register.
!
! author   D.P.Briggs   May 1996
!
! V1.01 09/03/00  WDNJ Array sizes increased from 2560 to 12800
!                 Uses waits rather than event flags for new DRS data
!
! subroutines
!
      IMPLICIT NONE
! DRS checks
      CALL H_DRS_CHECK
!
! Instrument checks 
      CALL H_INST_CHECK
!
      CALL EXIT
      STOP 'H_INSTQC ends'
      END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE H_DRS_CHECK
!
! performs basic averaging and standard deviation checks on raw DRS data
!
      IMPLICIT NONE
      STRUCTURE /PARSTAT/  !parameter statistics
         REAL*4      RVAL(320)   !individual values
         REAL*4      TVAL,TC     !total value, total count
      ENDSTRUCTURE
!
      RECORD  /PARSTAT/
     &   DRST,REFP,REFN,SREG,
     &   DITTF,NITTF,ICTP,ICTHK,PHGT,RHGT,AIRS,AOA,AOSS,
     &   HYGR,HYCC,LWC,TWC,UP1T,UP2T,UIRT,LP1T,LP2T,LIRT,UP1Z,
     &   UP2Z,UIRZ,LP1Z,LP2Z,LIRZ,UP1S,UP2S,UIRS,LP1S,LP2S,
     &   LIRS,PRTT,PRTC
      LOGICAL*4   LRAD,LPRT,LTMP,LTWC
      INTEGER*4   IBBRLF,IBBRUF,ISTAT
      REAL*4      RTEMP
      CHARACTER   CDAT*9,CTIM*8
!
      INCLUDE     'HCOM_DEF.FOR'
      INCLUDE     '($SYSSRVNAM)'
!
      INTEGER*4 ICT,IST,I,IT1,IT2,IHH,IMM,ISS,IS
      REAL*4 DMAX,DMIN,DSD     !default values
      DMAX = 4000.0
      DMIN = 100.0
      DSD = 1.0
!
!      ISTAT = SYS$ASCEFC(%VAL(64),'H_CEF_CLUSTER',,)
!
      CALL DATE1(CDAT)
      CALL TIME(CTIM)
      WRITE(6,*) ' ' 
      WRITE(6,*) CDAT//' '//CTIM//' DRS checks'
      IST = STATUS(1)
      IF (IST.LT.1.OR.IST.GT.2) THEN
        WRITE(6,*) '*** No DRS data available ***'
        CALL EXIT
      ENDIF
      ICT = 1
      ! DRS time
      CALL READ_DRS(DRST,2,IST,ICT)
      IT1 = INT(DRST.RVAL(1))
      CALL READ_DRS(DRST,3,IST,ICT)
      IT2 = INT(DRST.RVAL(1))
      IHH = IBITS(IT1,8,4)*10 + IBITS(IT1,4,4)
      IMM = IBITS(IT1,0,4)*10 + IBITS(IT2,8,4)
      ISS = IBITS(IT2,4,4)*10 + IBITS(IT2,0,4)
      WRITE(6,'(1X,A,I2.2,'':'',I2.2,'':'',I2.2)') 
     &        'DRS time : ',IHH,IMM,ISS
!
! check for case of no data coming from DRS 
      IT1 = NFDATA(NPLOC(3),STATUS(1))
      CALL LIB$WAIT(3.0)
      IT2 = NFDATA(NPLOC(3),STATUS(1))
      IF (IT1 .EQ. IT2) THEN
        WRITE(6,*) '*** No DRS data available ***'
        CALL EXIT
      ENDIF
!
!      ISTAT = SYS$WAITFR(%VAL(64))      ! wait for next DRS data block
      IS=STATUS(1)
      DO WHILE(IS.EQ.STATUS(1))
        CALL LIB$WAIT(0.1)
      END DO
      ! Flight number
      CALL READ_DRS(DRST,1,IST,ICT)
      IT1 = INT(DRST.RVAL(1))
      I = IBITS(IT1,8,4)*100 + IBITS(IT1,4,4)*10 + IBITS(IT1,0,4)
      WRITE(6,'(1X,A,I3.3)') 'Flight number A',I 
      ! clear fault flags and counters
      LRAD = .FALSE. 
      LPRT = .FALSE.
      LTMP = .FALSE.
      LTWC = .FALSE.
      IBBRLF = 0
      IBBRUF = 0
!
      DO ICT = 1,5                        ! read in 5 seconds of current data
!        ISTAT = SYS$WAITFR(%VAL(64))      ! wait for next DRS data block
        IS=STATUS(1)
        DO WHILE(IS.EQ.STATUS(1))
          CALL LIB$WAIT(0.1)
        END DO
        IST = STATUS(1)
        ! reference voltages
        CALL READ_DRS(REFP,5,IST,ICT)
        CALL READ_DRS(REFN,7,IST,ICT)
        ! status register
        CALL READ_DRS(SREG,27,IST,ICT)
        DO I = 1,2
          ISTAT = INT(SREG.RVAL(I))
          IF (BTEST(ISTAT,0)) LPRT = .TRUE. !prt in cal mode
          IF (BTEST(ISTAT,6)) LRAD = .TRUE. !radalt >5000'
          IF (BTEST(ISTAT,5)) LTMP = .TRUE. !dit heater on
        ENDDO
        ! deiced true air temp
        CALL READ_DRS_16(DITTF,10,IST,ICT)
        ! non deiced true air temp
        CALL READ_DRS_16(NITTF,23,IST,ICT)
        ! ICTP
        CALL READ_DRS(ICTP,48,IST,ICT)
        CALL READ_DRS(ICTHK,49,IST,ICT)
        ! static pressure
        CALL READ_DRS(PHGT,8,IST,ICT)
        ! Radalt
        IF (.NOT.LRAD) THEN !if sig reg bit 6 set >5000ft
          CALL READ_DRS(RHGT,37,IST,ICT)
        ENDIF
        ! Airspeed
        CALL READ_DRS(AIRS,9,IST,ICT)
        ! AoA
        CALL READ_DRS(AOA,18,IST,ICT)
        ! AoSS
        CALL READ_DRS(AOSS,19,IST,ICT)
        ! General Eastern Hygrometer and control
        CALL READ_DRS(HYGR,58,IST,ICT)
        CALL READ_DRS(HYCC,59,IST,ICT)
        ! LWC
        CALL READ_DRS(LWC,42,IST,ICT)
        ! TWC
        CALL READ_DRS(TWC,78,IST,ICT)
        IF (TWC.RVAL(ICT).LT.4095.0) LTWC = .TRUE.
        ! UPPER BBR signal
        CALL READ_DRS(UP1S,81,IST,ICT)
        IF (UP1S.RVAL(ICT).GT.4094.0) IBBRUF = IBBRUF + 1
        CALL READ_DRS(UP2S,82,IST,ICT)
        IF (UP2S.RVAL(ICT).GT.4094.0) IBBRUF = IBBRUF + 1
        CALL READ_DRS(UIRS,83,IST,ICT)
        IF (UIRS.RVAL(ICT).GT.4094.0) IBBRUF = IBBRUF + 1
        ! UPPER BBR zero
        CALL READ_DRS(UP1Z,84,IST,ICT)
        IF (UP1Z.RVAL(ICT).GT.4094.0) IBBRUF = IBBRUF + 1
        CALL READ_DRS(UP2Z,85,IST,ICT)
        IF (UP2Z.RVAL(ICT).GT.4094.0) IBBRUF = IBBRUF + 1
        CALL READ_DRS(UIRZ,86,IST,ICT)
        IF (UIRZ.RVAL(ICT).GT.4094.0) IBBRUF = IBBRUF + 1
        ! UPPER BBR temps
        CALL READ_DRS(UP1T,87,IST,ICT)
        IF (UP1T.RVAL(ICT).GT.4094.0) IBBRUF = IBBRUF + 1
        CALL READ_DRS(UP2T,88,IST,ICT)
        IF (UP2T.RVAL(ICT).GT.4094.0) IBBRUF = IBBRUF + 1
        CALL READ_DRS(UIRT,89,IST,ICT)
        IF (UIRT.RVAL(ICT).GT.4094.0) IBBRUF = IBBRUF + 1
        ! LOWER BBR signal
        CALL READ_DRS(LP1S,91,IST,ICT)
        IF (LP1S.RVAL(ICT).GT.4094.0) IBBRLF = IBBRLF + 1
        CALL READ_DRS(LP2S,92,IST,ICT)
        IF (LP2S.RVAL(ICT).GT.4094.0) IBBRLF = IBBRLF + 1
        CALL READ_DRS(LIRS,93,IST,ICT)
        IF (LIRS.RVAL(ICT).GT.4094.0) IBBRLF = IBBRLF + 1
        ! LOWER BBR zero
        CALL READ_DRS(LP1Z,94,IST,ICT)
        IF (LP1Z.RVAL(ICT).GT.4094.0) IBBRLF = IBBRLF + 1
        CALL READ_DRS(LP2Z,95,IST,ICT)
        IF (LP2Z.RVAL(ICT).GT.4094.0) IBBRLF = IBBRLF + 1
        CALL READ_DRS(LIRZ,96,IST,ICT)
        IF (LIRZ.RVAL(ICT).GT.4094.0) IBBRLF = IBBRLF + 1
        ! LOWER BBR temps
        CALL READ_DRS(LP1T,97,IST,ICT)
        IF (LP1T.RVAL(ICT).GT.4094.0) IBBRLF = IBBRLF + 1
        CALL READ_DRS(LP2T,98,IST,ICT)
        IF (LP2T.RVAL(ICT).GT.4094.0) IBBRLF = IBBRLF + 1
        CALL READ_DRS(LIRT,99,IST,ICT)
        IF (LIRT.RVAL(ICT).GT.4094.0) IBBRLF = IBBRLF + 1
        ! Heimann
        CALL READ_DRS(PRTT,141,IST,ICT)
        CALL READ_DRS(PRTC,142,IST,ICT)
      ENDDO
!    
      ! reference voltages
      CALL STAT_DRS(REFP,567.0,569.0,0.0,0.55,'Ref volts +ve')
      CALL STAT_DRS(REFN,2858.0,2861.0,0.0,0.56,'Ref volts -ve')
      ! deiced true air temp
      IF (.NOT.LTMP) WRITE(6,*) '*** Deiced heater on'
      CALL STAT_DRS_L(DITTF,100.0,28665.0,0.5,22.3,0.0,7.0,
     &              'Deiced')
      ! non deiced true air temp
      CALL STAT_DRS_L(NITTF,100.0,28665.0,0.5,22.3,0.0,7.0,
     &              'Non Deiced')
      ! ICTP house keeping
      IF (ICTP.TVAL/ICTP.TC .LT. 4095.0 .AND.
     &    ICTHK.TVAL/ICTHK.TC .LT. 4095.0) THEN
        IF (ICTHK.RVAL(2) .GT. 3000.0) THEN
          IT1 = 2        !VREF found first  
          IT2 = 10
        ELSE
          IT2 = 2        !VREF found second
          IT1 = 10
        ENDIF
        IF (ICTHK.RVAL(IT1) .LT. 3840.0 .OR.    
     &      ICTHK.RVAL(IT1) .GT. 3860.0)
     &     WRITE(6,*) '*** ICTP reference voltage fault.'
        IF (ICTHK.RVAL(IT2) .LT. 1200.0 .OR.    
     &      ICTHK.RVAL(IT2) .GT. 2500.0)
     &     WRITE(6,*) '*** ICTP chopper motor fault.'
        IF (ICTHK.RVAL(6) .LT. 3584.0 .OR.
     &      ICTHK.RVAL(6) .GT. 4000.0)
     &     WRITE(6,*) '*** ICTP detector temperature problem.'
        IF (ICTHK.RVAL(8) .LT. 165.0 .OR.
     &      ICTHK.RVAL(8) .GT. 235.0)
     &     WRITE(6,'(1X,A,F6.1)') '*** ICTP ICE detector:',ICTHK.RVAL(8)
        ! ICTP
        CALL STAT_DRS(ICTP,Dmin,Dmax,0.0,8.54,'ICTP')
      ELSE
        WRITE(6,*) '*** ICTP not on.'
      ENDIF
      ! static pressure
      CALL STAT_DRS_L(PHGT,1200.0,4000.0,0.1,1.24,0.0,0.22,
     &              'static pressure')
      ! Radalt
      IF (LRAD) THEN
        WRITE(6,*) '*** Radalt off; >5000''?'
      ELSE
        CALL STAT_DRS_L(RHGT,0.0,4096.0,0.0,4.0,0.0,6.9,'Radalt')
      ENDIF
      ! Airspeed
      CALL STAT_DRS(AIRS,800.0,4000.0,0.5,28.4,'pitot static')
      ! AoA
      CALL STAT_DRS(AOA,700.0,3000.0,1.0,100.0,'AoA')
      ! AoSS
      CALL STAT_DRS(AOSS,700.0,3000.0,1.0,100.0,'AoSS')
      ! General Eastern Hygrometer and control
      CALL STAT_DRS_L(HYGR,100.0,3900.0,0.1,6.5,0.0,10.1,
     &              'GE dew pt')
      CALL STAT_DRS(HYCC,695.0,900.0,0.1,100.0,'GE control')
      ! LWC
      CALL STAT_DRS_L(LWC,700.0,4000.0,0.22,30.0,0.0,3.5,'LWC')
      ! TWC
      IF (LTWC) THEN 
        WRITE(6,*) '*** TWC error:'
        ISTAT = INT(TWC.RVAL(1))
        IF (.NOT.BTEST(ISTAT,0)) WRITE(6,*) '  Low UV'
        IF (.NOT.BTEST(ISTAT,1)) WRITE(6,*) '  Nose temp <> limits'
        IF (.NOT.BTEST(ISTAT,2)) WRITE(6,*) '  Sample temp <> limits'
        IF (.NOT.BTEST(ISTAT,3)) WRITE(6,*) '  Ambient temp'
        IF (.NOT.BTEST(ISTAT,4)) WRITE(6,*) '  Source temp <> limits'
        IF (.NOT.BTEST(ISTAT,5)) WRITE(6,*) '  Evap heater 1'
        IF (.NOT.BTEST(ISTAT,6)) WRITE(6,*) '  Evap heater 2'
        IF (.NOT.BTEST(ISTAT,7)) WRITE(6,*) '  Source current <> limits'
      ENDIF
      !estimate BBR temperature from de-iced
      RTEMP =  4570.0 - 0.082*DITTF.TVAL/DITTF.TC
      !UPPER BBRs ...
      IF (IBBRUF .GT. 1) THEN
        WRITE (6,*) '*** UPPER BBRs not working.'
      ELSE
        ! UPPER BBR signal      
        CALL STAT_DRS_L(UP1S,178.0,Dmax,0.0,81.0,0.0,150.0,'Upper P1S')
        CALL STAT_DRS_L(UP2S,135.0,Dmax,0.0,84.4,0.0,150.0,'Upper P2S')
        CALL STAT_DRS_L(UIRS,1000.0,Dmax,0.0,3.2,0.0,4.6,'Upper IRS')
        ! UPPER BBR zero        
        CALL STAT_DRS(UP1Z,120.0,180.0,0.0,4.0,'Upper P1Z')
        CALL STAT_DRS(UP2Z,100.0,200.0,0.0,4.0,'Upper P2Z')
        CALL STAT_DRS(UIRZ,1950.0,2150.0,0.0,4.0,'Upper IRZ')
        ! UPPER BBR temps
        CALL STAT_DRS(UP1T,RTEMP-145.0,RTEMP+145.0,0.0,2.9,
     &                'Upper P1T')
        CALL STAT_DRS(UP2T,RTEMP-145.0,RTEMP+145.0,0.0,2.9,
     &                'Upper P2T')
        CALL STAT_DRS(UIRT,RTEMP-145.0,RTEMP+145.0,0.0,2.9,
     &                'Upper IRT')
      ENDIF
      !LOWER BBRs ...
      IF (IBBRLF .GT. 1) THEN
        WRITE (6,*) '*** LOWER BBRs not working.'
      ELSE
        ! LOWER BBR signal
        CALL STAT_DRS_L(LP1S,166.0,Dmax,0.0,16.0,0.0,10.5,'Lower P1S')
        CALL STAT_DRS_L(LP2S,113.0,Dmax,0.0,16.0,0.0,8.3,'Lower P2S')
        CALL STAT_DRS_L(LIRS,1000.0,Dmax,0.0,6.5,0.0,2.8,'Lower IRS')
        ! LOWER BBR zero
        CALL STAT_DRS(LP1Z,120.0,180.0,0.0,4.0,'Lower P1Z')
        CALL STAT_DRS(LP2Z,100.0,200.0,0.0,4.0,'Lower P2Z')
        CALL STAT_DRS(LIRZ,1950.0,2150.0,0.0,4.0,'Lower IRZ')
        ! LOWER BBR temps
        CALL STAT_DRS(LP1T,RTEMP-145.0,RTEMP+145.0,0.0,2.9,
     &                'Lower P1T')
        CALL STAT_DRS(LP2T,RTEMP-145.0,RTEMP+145.0,0.0,2.9,
     &                'Lower P2T')
        CALL STAT_DRS(LIRT,RTEMP-145.0,RTEMP+145.0,0.0,2.9,
     &                'Lower IRT')
      ENDIF
      ! Heimann
      IF (LPRT) THEN
        WRITE(6,*) '*** Radtherm set to calibrate.'
      ELSE
        CALL STAT_DRS_L(PRTT,Dmin,Dmax,0.0,31.4,0.0,8.0,'PRT Temp')
        CALL STAT_DRS(PRTC,2000.0,2500.0,0.0,4.03,'PRT Control')
      ENDIF
!
      RETURN
      END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE READ_DRS(PARAM,IPN,IST,ICT)
!
! reads DRS data
!
      IMPLICIT NONE
      STRUCTURE /PARSTAT/  !parameter statistics
         REAL*4      RVAL(320)   !individual values
         REAL*4      TVAL,TC     !total value, total count
      ENDSTRUCTURE
      RECORD /PARSTAT/ PARAM
      INTEGER*4     IPN,IST,ICT,IFRQ,ILOC,I
      INTEGER*2     IBUFF
      INCLUDE     'HCOM_DEF.FOR'
!
      IF (ICT .EQ. 1) THEN
        PARAM.TVAL = 0.0
        PARAM.TC = 0.0
      ENDIF
      IFRQ = NPFREQ(IPN)
      ILOC = NPLOC(IPN)-1
      DO I=1,IFRQ
        IBUFF = NFDATA(ILOC+I,IST)
        PARAM.RVAL((ICT-1)*IFRQ+I) = FLOAT(IBUFF.AND.'FFF'X)
        PARAM.TVAL = PARAM.TVAL + FLOAT(IBUFF.AND.'FFF'X)
        PARAM.TC = PARAM.TC + 1.0
      ENDDO 
!
      RETURN
      END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE READ_DRS_16(PARAM,IPN,IST,ICT)
!
! reads DRS 16 bit data
!
      IMPLICIT NONE
      STRUCTURE /PARSTAT/  !parameter statistics
         REAL*4      RVAL(320)   !individual values
         REAL*4      TVAL,TC     !total value, total count
      ENDSTRUCTURE
      RECORD /PARSTAT/ PARAM
      INTEGER*4     IPN,IST,ICT,IFRQ,ILOC,I
      INTEGER*2     IBUFF
      INCLUDE     'HCOM_DEF.FOR'
!
      IF (ICT .EQ. 1) THEN
        PARAM.TVAL = 0.0
        PARAM.TC = 0.0
      ENDIF
      IFRQ = NPFREQ(IPN)
      ILOC = NPLOC(IPN)-1
      DO I=1,IFRQ
        IBUFF = NFDATA(ILOC+I,IST)
        PARAM.RVAL((ICT-1)*IFRQ+I) = FLOAT(IBUFF)
        PARAM.TVAL = PARAM.TVAL + FLOAT(IBUFF)
        PARAM.TC = PARAM.TC + 1.0
      ENDDO 
!
      RETURN
      END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE STAT_DRS(PARAM,RMIN,RMAX,RSDM,RSDX,CMESS)
!
! calculates statistics on parameter and outputs message.
!
      IMPLICIT NONE
      STRUCTURE /PARSTAT/  !parameter statistics
         REAL*4      RVAL(320)   !individual values
         REAL*4      TVAL,TC     !total value, total count
      ENDSTRUCTURE
      RECORD /PARSTAT/ PARAM
      REAL*4           RMAX,RMIN,RSD,RSDM,RSDX
      REAL*4           RMN,STD_DEV_M
      CHARACTER*(*)  CMESS
!
      IF (PARAM.TC .LT. 1.0) THEN
        WRITE(6,'(1X,A)') '*** '//CMESS//' No data.'
      ELSE
        RMN = PARAM.TVAL/PARAM.TC
        CALL W_MESS(RMN,RMIN,RMAX,CMESS//' too low.',
     &                                 CMESS//' too high.')
        RSD = STD_DEV_M(PARAM.RVAL,RMN,INT(PARAM.TC))
        CALL W_MESS(RSD,RSDM,RSDX,CMESS//' too quiet.',
     &                                 CMESS//' too noisy.')
      ENDIF
!
      RETURN
      END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE STAT_DRS_L(PARAM,RMIN,RMAX,RSDM,RSDX,ROCM,ROCX,CMESS)
!
! calculates statistics on parameter and outputs message.
!
      IMPLICIT NONE
      STRUCTURE /PARSTAT/  !parameter statistics
         REAL*4      RVAL(320)   !individual values
         REAL*4      TVAL,TC     !total value, total count
      ENDSTRUCTURE
      RECORD /PARSTAT/ PARAM
      REAL*4           RMAX,RMIN,RSDM,RSDX,ROCM,ROCX
      REAL*4           RMN,RSD,ROC,RIT
      CHARACTER*(*)  CMESS
!
      IF (PARAM.TC .LT. 1.0) THEN
        WRITE(6,'(1X,A)') '***'//CMESS//' No data.'
      ELSE
        RMN = PARAM.TVAL/PARAM.TC  !mean
        CALL W_MESS(RMN,RMIN,RMAX,CMESS//' too low.',
     &                                 CMESS//' too high.')
        CALL L_FIT(PARAM.RVAL,INT(PARAM.TC),ROC,RIT,RSD)
        CALL W_MESS(ABS(ROC),ROCM,ROCX,CMESS//' RoC too slow.',
     &                                 CMESS//' RoC too fast.')
        CALL W_MESS(RSD,RSDM,RSDX,CMESS//' too quiet.',
     &                                 CMESS//' too noisy.')
      ENDIF
!
      RETURN
      END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE H_INST_CHECK
!
! performs limit and correlation checks on DERIVED data
!
      IMPLICIT NONE
      CHARACTER CDAT*9,CTIM*8
      REAL*4    RT(12800)
      INTEGER*4 ICT
      LOGICAL*4 D_CHECK
      INCLUDE  'HCOM_DEF.FOR'
!
      CALL DATE1(CDAT)
      CALL TIME(CTIM)
      WRITE(6,*) ' '
      WRITE(6,*) CDAT//' '//CTIM//' Derived Parameter checks'
!
! thermodynamic
      IF (D_CHECK(RTATDI,203.0,343.0,'Deiced')) THEN  !valid deiced temperatures
        !de iced vs non de iced
        CALL DERV_CHECK(RTATND,RTATDI,203.0,343.0,0.4,0.980,1.0,
     &   'Non Deiced','Deiced')
        !ICTP vs non de iced
        CALL DERV_CHECK(RICT,RIATDI,-70.0,+70.0,2.1,0.9,1.0,
     &   'ICTP','Deiced')
        CALL DERV_CHECK(RSPR,RTATDI,300.0,1030.0,-1.0,0.7,0.99,
     &   'Static pressure','Deiced')
       ENDIF
! kinetic
      IF (D_CHECK(RGLAT,-90.0,+90.0,'GPS latitude') .AND.
     &    D_CHECK(RGLNG,-180.0,+180.0,'GPS longitude') ) THEN
        CALL DERV_CHECK(RILAT,RGLAT,-90.0,+90.0,0.1,0.950,1.00,
     &   'INU lat','GPS')
        CALL DERV_CHECK(RILNG,RGLNG,-180.0,+180.0,0.1,0.950,1.00,
     &   'INU long','GPS')
C        CALL DERV_CHECK(ROLAT,RGLAT,-90.0,+90.0,0.1,0.950,1.0,
C     &   'OMEGA lat','GPS')
C        CALL DERV_CHECK(ROLONG,RGLNG,-180.0,+180.0,0.1,0.950,1.0,
C     &   'OMEGA long','GPS')
      ENDIF
      IF (D_CHECK(RIGS,0.0,200.0,'INU ground speed')) THEN
        ! rtas knots: gs m/s ; m/s = knots/1.944
        DO ICT = 1,12800
          RT(ICT) = RTAS(ICT)/1.944
        ENDDO
        CALL DERV_CHECK(RT,RIGS,0.0,200.0,25.0,0.8,1.00,
     &   'Air speed','Ground Speed')
      ENDIF
! hygrometric
      IF (D_CHECK(RDEW,-70.0,+30.0,'GE hygrometer')) THEN
        CALL DERV_CHECK(RTWCDP,RDEW,-70.0,+30.0,1.0,0.94,1.0,
     &   'TWC','GE dew point')
      ENDIF
! photometric
      IF (D_CHECK(RUCLR,-10.0,2000.0,'Upper BBR clear')) THEN
        CALL DERV_CHECK(RURED,RUCLR,-10.0,1000.0,-1.0,0.950,1.0,
     &   'Upper BBR red','BBR clear')
      ENDIF 
      IF (D_CHECK(RLCLR,-10.0,2000.0,'Lower BBR clear')) THEN
        CALL DERV_CHECK(RLRED,RLCLR,-10.0,1000.0,-1.0,0.950,1.0,
     &   'Lower BBR red','BBR clear')
      ENDIF 
!
      RETURN
      END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE DERV_CHECK(P1,P2,RMIN,RMAX,RDIFF,RCMIN,RCMAX,CP1,CP2)
!
! performs checks on derived data
!
      IMPLICIT NONE
      REAL*4  P1(12800),P2(12800),
     &        RMIN,RMAX,RDIFF,RCMIN,RCMAX,RCDIFF
      CHARACTER*(*)  CP1,CP2
      INTEGER*4 IC,I
      LOGICAL*4 D_CHECK,C_MESS
      INCLUDE 'HCOM_DEF.FOR'
!
      IF (D_CHECK(P1,RMIN,RMAX,CP1)) THEN
        IF (RDIFF .GT. 0.0 .AND.
     &     C_MESS(P1,P2,RCMIN,RCMAX,CP1//' vs '//CP2)) THEN
          RCDIFF = 0.0
          IC = STATUS(2)
          DO I = 1,2560
            RCDIFF = RCDIFF + P1(IC) - P2(IC)
            IC = IC - 1
            IF (IC .LE. 0) IC = 12800
          ENDDO
          RCDIFF = RCDIFF / 2560.0
          IF (ABS(RCDIFF).GT.RDIFF) WRITE(6,'(1X,A,1X,F7.2)')
     &            CP1//' - '//CP2//' abs mean difference high',rcdiff
        ENDIF
      ENDIF
      RETURN
      END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      LOGICAL*4 FUNCTION D_CHECK(RVAR,RMIN,RMAX,CMESS)
!
! returns true if all data is within max and min else false
!
      IMPLICIT NONE
      CHARACTER*(*)  CMESS
      REAL*4  RVAR(12800),RMIN,RMAX
      INTEGER*4 IC,I
      INCLUDE 'HCOM_DEF.FOR'
!
      D_CHECK = .TRUE.
      IC = STATUS(2)
      DO I=1,2560                      !Last two hours
        IF (RVAR(IC) .LT. RMIN .OR. RVAR(IC) .GT. RMAX) 
     &      D_CHECK = .FALSE.
        IC = IC - 1
        IF (IC .LE. 0) IC = 12800
      ENDDO
      IF (.NOT. D_CHECK) 
     &   WRITE(6,'(1X,A,F8.2)') '*** '//CMESS//' bad data found.',
     &                           RVAR(IC-1)                       
      RETURN
      END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      REAL*4 FUNCTION MEAN(X,N)
!
! purpose : calculates mean of input array
!
      IMPLICIT NONE
      REAL*4 X(3600),SX
      INTEGER*4 N,I
!
      SX = 0.0
      DO I=1,N
        SX = SX + X(I)
      ENDDO
      MEAN = SX/FLOAT(N)
!
      RETURN
      END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      REAL*4 FUNCTION STD_DEV_M(X,M,N)
!
! purpose : calculates standard deviation of sample from mean
!
      IMPLICIT NONE
      REAL*4 X(3600),M,DXM,FN
      INTEGER*4 I,N
!
      FN = FLOAT(N) 
      DXM = 0.0
      DO I=1,N
        DXM = DXM + (X(I)-M)**2/(FN-1)
      ENDDO
      STD_DEV_M = SQRT(DXM)
!      
      RETURN
      END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      REAL*4 FUNCTION CORRELATE(X,Y)
!
! purpose : caluculates the correlation coefficient for two input arrays
!
      IMPLICIT NONE
      REAL*4 X(2560),Y(2560),MX,MY,ST,SB1,SB2,MEAN,FN
      INTEGER*4 N,I
!
      N = 2560
      FN = FLOAT(N)
      MX = MEAN(X,N)
      MY = MEAN(Y,N)
      ST = 0.0
      SB1 = 0.0
      SB2 = 0.0
      DO I=1,N
        ST = ST + (X(I)-MX)*(Y(I)-MY)
        SB1 = SB1 + (X(I)-MX)**2
        SB2 = SB2 + (Y(I)-MY)**2
      ENDDO
!
      IF (SB1 .GT. 0.0 .AND. SB2 .GT. 0.0) THEN
        CORRELATE = ST/SQRT(SB1*SB2)
      ELSE 
        CORRELATE = 0.0
      ENDIF
!
      RETURN
      END  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE L_FIT(X,N,A,B,SD)
!
! purpose : fits line to data using least squares fit so that X~A*t+b
!           then calculates standard deviation from line.
!
      IMPLICIT NONE
      REAL*4  X(320),A,B,SD,MX,MY,ST,SB,DXM,MEAN
      INTEGER*4 N,I
!
      MX = MEAN(X,N)
      MY = FLOAT(N)/2.0
!
      ST = 0.0
      SB = 0.0
      DO I=1,N
        ST = ST + (X(I)-MX)*(FLOAT(I)-MY)
        SB = SB + (FLOAT(I)-MY)**2
      ENDDO
      A = ST/SB
      B = MX - A*MY
!
      DXM = 0.0
      DO I=1,N
        DXM = DXM + (X(I)-(A*FLOAT(I)+B))**2/(FLOAT(N-1))
      ENDDO
      SD = SQRT(DXM)
!
      RETURN
      END  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE W_MESS (RVAR,RMIN,RMAX,CMIN,CMAX)
!
! purpose : outputs message according to ranges
!
      IMPLICIT NONE
      REAL*4 RVAR,RMIN,RMAX
      CHARACTER*(*) CMIN,CMAX
!
      IF (RVAR .LT. RMIN) THEN
          WRITE(6,'(1X,A,1X,F12.4)') CMIN,RVAR
      ELSE IF (RVAR .GT. RMAX) THEN
          WRITE(6,'(1X,A,1X,F12.4)') CMAX,RVAR
      ENDIF
      RETURN
      END  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      LOGICAL*4 FUNCTION C_MESS (P1,P2,RMIN,RMAX,CMESS)
!
! purpose : outputs message according to ranges
!
      IMPLICIT NONE
      REAL*4   P1(2560),P2(2560),RVAR,RMIN,RMAX,CORRELATE
      CHARACTER*(*) CMESS
!
      C_MESS = .TRUE.
      RVAR = CORRELATE(P1,P2)
      IF (RVAR .LT. 0.3) THEN
        WRITE(6,'(1X,A)') CMESS//' correlation < 0.3.'
        C_MESS = .FALSE.
      ELSE IF (RVAR .LT. RMIN) THEN
        WRITE(6,'(1X,A,1X,F5.3)') CMESS//' correlation low : ',RVAR
      ELSE IF (RVAR .GT. RMAX) THEN
        WRITE(6,'(1X,A,1X,F5.3)') CMESS//' correlation high : ',RVAR
      ENDIF
      RETURN
      END  
!*******************************************************************************
      SUBROUTINE DATE1(CDATE)
!
! Replaces the DATE Fortran instrinsic and allows programs to compile without
! Y2000 warnings.  Returns date as DD-MMM-YY.
!
      IMPLICIT  NONE
      INTEGER*4 IVAL(8)
      CHARACTER CMON*3(12),CDATE*9,CDATE1*8
      DATA      CMON /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     &    'SEP','OCT','NOV','DEC'/

      CALL DATE_AND_TIME(CDATE1,,,IVAL)
      WRITE(CDATE,'(A)') CDATE1(7:8)//'-'//CMON(IVAL(2))//'-'//
     &    CDATE1(3:4)
      RETURN
      END
