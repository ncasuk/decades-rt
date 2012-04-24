         PROGRAM H_CHECK_PARA
C
C This is the main program for running the parameter checking program.
C It reads in the number of error messages and passes them to the parameter 
C checking program. The program is done this way as it makes it easier to read
C the error messages in.
C
        IMPLICIT  NONE
        INTEGER*4 ILIM
        LOGICAL*4 OMESS_CALLED
        COMMON    /CHECK_COMMON/ ILIM,OMESS_CALLED
C
        CALL H_ALL_CHECK
        END  
C*******************************************************************************
C       SUBROUTINE  H_ALL_CHECK PROGRAM FORTVAX
C
C       PURPOSE  In flight monitoring of DRS raw data.
C
C       DESCRIPTION  This program monitors the raw data from selected
C scientific instruments. It checks the data for errors in the range,rate
C of change, and standard deviation. Any errors found are written to the 
C screen/message file. 
C                     The instruments checked are those which cover the basic
C scientific parameters.(The instrument fit for a particular flight is specified
C in the H_CHECK_PARA_FIT.DAT file. 
C This fit can be changed by running the FITMOD 
C program.) The program goes through each of the instruments in turn and checks
C that the data are not changing at too great a rate, the standard deviation is
C within certain limits and that all the data points lie within certain ranges.
C The ranges are in the H_CHECK_PARA_RANGES.DAT file and can be altered using
C the RANGEMOD program.
C                     The program cycles though these checks for 60 seconds. 
C It then performs intercomparisons of those instruments which measure the same
C variables.
C
C
C N.B. The DRS must be on DATA and the H_DRS_LOG and H_DERIVE processes must be
C running fot this program to work.
C      If H_DERIVE is crashing the program will still run correctly.
C
C
C  TO COMPILE:
C             $ FORT H_CHECK_PARA
C      $ LINK H_CHECK_PARA,[HORACE]H_COMMON/OPT,[HORACE]HORACE/LIB
C   
C  TO RUN :
C      $ RUN H_CHECK_PARA
C
C  All output goes to .....
C
C
C VERSION 1.01     by D.R.Lauchlan  02.08.1993
C
C CHANGES
C
C******************************************************************************
        SUBROUTINE H_ALL_CHECK
C
C
C This program performs limits and range checks on the various DRS
C       parameters.
C       Build as follows:
C       
C     FORTRAN H_CHECK_PARA
C     LINK H_CHECK_PARA,H_COMMON/OPT,[HORACE]HORACE.OLB/LIB
C
C     H_COMMON/OPT contains the information needed for HORACE to find the 
C     DRS data and the STATUS data.
C     
C     By David Lauchlan, 24th August 1992.
C
      IMPLICIT NONE
      INTEGER*4 IHRS,IPOS1,IX,ILSTIM,ISECON,J,I,IMINS,I2,IN,IT1,
     &    IC,INEWCO,IT2,ICOUNT,INOMESS,ITEMP,IFIRST,INPARAS,ITIM1,IFLNO,
     &    IPOS,ITIM2  
      REAL*4 R,R1PSP,R2,RPAZI,RTDI,RWA,RES1,RPHT,
     &    RYAWR_1,RPITR_1,RTNDI,X,RIHDG_1,RPRESS,Y,RINUVEL,
     &    RPTCH_1,RVEL,RIP,RROLL_1,RVX,RSTDEV,RVY,RMACH1,
     &    RVZ_1
      CHARACTER*60 CMESS1(25)
      INTEGER*4 ILIM(512,9)
      INTEGER*4 IPARAS(29)
      LOGICAL*4 OCONT,OFITTED(512),OSTATUS,ISTAT,OST,OMESS_CALLED
      INTEGER*2 IVAL_16(32),JTEMP(2)
      EQUIVALENCE (JTEMP,ITEMP)
      REAL*4 PREVAL(512)        
      DATA IPARAS/8,9,18,19,37,42,48,58,59,70,126,81,82,83,
     -    84,85,86,87,88,89,91,92,93,94,95,96,97,98,99/
      REAL*4 RTERR,RGAMMA,RLAMDA,RUPW,RTASK,
     -        CAL8(3),CAL9(2),CAL10(3),CAL14(3),CAL18(2),CAL19(2),
     -        CAL23(3),CAL37(3),CAL40(2),CAL42(2),CAL43(3),CAL47(2),
     -        CAL58(2),CAL70(2),CAL71(2),CAL72(2),CAL73(2),CAL74(2),
     -        CAL75(2),CAL76(2),CAL77(2),CAL81(2),CAL82(2),CAL83(2),
     -        CAL87(2),CAL91(2),CAL92(2),CAL93(2),CAL100(14),CAL101(5),
     -        CAL103(4),CAL106(6),CAL144(2),CAL145(2),CAL146(2),
     -        CAL147(2),CAL148(2),CAL149(2)
      COMMON  /CALS/ RTERR,RGAMMA,RLAMDA,RUPW,RTASK,
     -        CAL8,CAL9,CAL10,CAL14,CAL18,CAL19,
     -        CAL23,CAL37,CAL40,CAL42,CAL43,CAL47,
     -        CAL58,CAL70,CAL71,CAL72,CAL73,CAL74,
     -        CAL75,CAL76,CAL77,CAL81,CAL82,CAL83,
     -        CAL87,CAL91,CAL92,CAL93,CAL100,CAL101,
     -        CAL103,CAL106,CAL144,CAL145,CAL146,
     -        CAL147,CAL148,CAL149
      COMMON /CHECK_COMMON/ILIM,OMESS_CALLED
      INCLUDE '($SYSSRVNAM)'
      INCLUDE 'HCOM_DEF.FOR'

      OPEN(UNIT=40,FILE='[HORACE]H_CHECK_PARA_MESS.DAT',
     -    STATUS='OLD',READONLY)
      READ(40,*)INOMESS    
      DO I=1,INOMESS
        READ(40,40)CMESS1(I)
      END DO
      CLOSE(40)
C Call the subroutine to read in the calibration constants for the parameters
      CALL READ_CONSTS
C Set the number of normal parameters to be checked
      INPARAS=29
C Set the number of INS parameters to be checked
c     INS_NUM=6  
C Set the first run through counter to equal zero
      IFIRST=0 
C This sets up ISTAT which is a logical flag set when a new set of data is
C received by the DRS.
      ISTAT=SYS$ASCEFC(%VAL(64),'H_CEF_CLUSTER',,) !Associate with CEF cluster
      J=NFDATA(NPLOC(1),1)
      IFLNO=IBITS(J,8,4)*100+IBITS(J,4,4)*10+IBITS(J,0,4)           
c Read in the error messages used by the program
      OPEN(UNIT=40,FILE='[HORACE]H_CHECK_PARA_RANGES.DAT',
     -    STATUS='OLD',READONLY)
      DO I=1,512
        READ(40,*)IX,(ILIM(I,IPOS),IPOS=1,9)
      END DO
      CLOSE(40)
      ILSTIM=10000
      OPEN(UNIT=41,FILE='[HORACE]H_CHECK_PARA_FIT.DAT',
     -    STATUS='OLD',READONLY)
      DO I=1,512
        READ (41,*)IX,OFITTED(I)
      END DO
      CLOSE (41)
C
C ILIM array contains the limits required to do the parameter tests for 
C range checks, noise levels, and rates of change.
C ILIM(n,1) : upper limit for parameter value
C ILIM(n,2) : lower limit for parameter value
C ILIM(n,3) : upper standard deviation limit
C ILIM(n,4) : lower standard deviation limit
C ILIM(n,5) : maximum rate of change
C ILIM(n,6) : message 1
C ILIM(n,7 to 9) : also other messages
C
      IT1=NFDATA(NPLOC(2),STATUS(1))
      IT2=NFDATA(NPLOC(3),STATUS(1))
      IHRS=IBITS(IT1,8,4)*10+IBITS(IT1,4,4)
      IMINS=IBITS(IT1,0,4)*10+IBITS(IT2,8,4)
      ISECON=IBITS(IT2,4,4)*10+IBITS(IT2,0,4)
c display time at start of check
      WRITE(6,50)'Time:',IHRS,IMINS,ISECON
      ICOUNT=0
      IN=29
C need to check for case of no data coming from DRS - i.e. DRS is on HOLD
      ITIM1=NFDATA(NPLOC(3),STATUS(1))
      CALL LIB$WAIT(3.0)
      ITIM2=NFDATA(NPLOC(3),STATUS(1))
      IF(ITIM1.EQ.ITIM2)THEN
        WRITE(6,*)' No DRS data available '
        CALL EXIT
      END IF
      DO WHILE(ICOUNT.LT.IN)                 !Loop for IN seconds
C This section waits until a flag is set which marks when HORACE receives a 
c block of data from the DRS. Until that flag is set the rest of the program is
c not executed. This means that the data checks are performed once a second.
C                           
        INEWCO=0      
        OCONT=.FALSE.                       
        ISTAT=SYS$WAITFR(%VAL(64))     !Wait for next DRS data block
        ICOUNT=ICOUNT+1                    
        IT1=NFDATA(NPLOC(2),STATUS(1))
        IT2=NFDATA(NPLOC(3),STATUS(1))
        IHRS=IBITS(IT1,8,4)*10+IBITS(IT1,4,4)
        IMINS=IBITS(IT1,0,4)*10+IBITS(IT2,8,4)
        ISECON=IBITS(IT2,4,4)*10+IBITS(IT2,0,4)
C Check normal DRS parameters         
        DO IC=1,INPARAS
          IF(OFITTED(IPARAS(IC))) THEN 
            CALL MEANPARAM(IPARAS(IC),R) 
            CALL PARA_STATS(IPARAS(IC),OSTATUS,RSTDEV)
            CALL RANGE_CHECK(IPARAS(IC),R,CMESS1)
            CALL NOISE_CHECK(IPARAS(IC),OSTATUS,RSTDEV,CMESS1)
            CALL RATE_OF_CHANGE(IPARAS(IC),R,
     -          PREVAL(IPARAS(IC)),CMESS1,IFIRST)
            PREVAL(IPARAS(IC))=R
          END IF
        END DO                     


C The INS checks coding was below here.  
C Now do the INS checks.
C The statistical calculations are done seperately as the INS parameters are
C made up of fine and coarse parameters which have to be merged. Then three
C checks are performed . They are : 1. range checks
C                                   2. noise checks
C                                   3. rate of change checks
C
c         DO J=1,INS_NUM
c
c           IPAR_FINE=INS_PARA_FINE(J)
c           IPAR_COARSE=INS_PARA_COARSE(J)
c           IF(OFITTED(IPAR_FINE)) THEN 
c             CALL INS_STATS(IPAR_COARSE,IPAR_FINE,R,OSTATUS,RSTDEV)
c             CALL RANGE_CHECK(IPAR_FINE,R,CMESS1)
c             CALL NOISE_CHECK(IPAR_FINE,OSTATUS,RSTDEV,CMESS1)
c             CALL RATE_OF_CHANGE(IPAR_FINE,R,PREVAL(IPAR_FINE),CMESS1
c     -       ,IFIRST)
c             PREVAL(IPAR_FINE)=R
c           END IF
c         END DO 
C Check new INS paras
C        IV16(IP,IS)=NFDATA(NPLOC(IP)+IS-1,STATUS(1))

        IF(NPLOC(163).GT.0) THEN              !H-423 recorded 
          DO IPOS1=1,32                                  
            IVAL_16(IPOS1)=NFDATA(NPLOC(163)+IPOS1-1,STATUS(1))
          END DO
          RIP=15.49                           !Vanes to INU distance (m)
C INU X velocity (m s-1 +ve northish)
          JTEMP(2)=IVAL_16(3)
          JTEMP(1)=IVAL_16(4)
          RVX=ITEMP/2.**18*12*25.4/1000.      !INU X velocity (m s-1)
C INU Y velocity (m s-1 +ve westish)
          JTEMP(2)=IVAL_16(5)
          JTEMP(1)=IVAL_16(6)
          RVY=ITEMP/2.**18*12*25.4/1000.      !INU Y velocity (m s-1)
C VZ    - INU vertical velocity (m s-1 +ve up)
          JTEMP(2)=IVAL_16(7)
          JTEMP(1)=IVAL_16(8)
          RVZ_1=ITEMP/2.**18*12*25.4/1000.      !INU vertical velocity (m s-1)
          R=RVZ_1
          CALL RANGE_CHECK(202,R,CMESS1)
          CALL RATE_OF_CHANGE(202,R,PREVAL(202),CMESS1,IFIRST)
          PREVAL(202)=R
C ROLL  - INU roll (-180 to +180 deg stbd roll is +ve)
          RROLL_1=IVAL_16(10)/2.**15*180.       !INU roll (deg)
          R=RROLL_1   
          CALL RANGE_CHECK(203,R,CMESS1)
          CALL RATE_OF_CHANGE(203,R,PREVAL(203),CMESS1,IFIRST)
          PREVAL(203)=R
C PTCH  - INU pitch (-90 to +90 nose up is +ve)
          RPTCH_1=IVAL_16(11)/2.**15*180.       !INU pitch (deg)
          R=RPTCH_1
          CALL RANGE_CHECK(204,R,CMESS1)
          CALL RATE_OF_CHANGE(204,R,PREVAL(204),CMESS1,IFIRST)
          PREVAL(204)=R
C IHDG  - INU azimuth (0 to 360 deg clockwise from above is +ve)
          RIHDG_1=IVAL_16(12)/2.**15*180.       !INU azimuth (deg)
          IF(RIHDG_1.LT.0.) RIHDG_1=RIHDG_1+360.
          R=RIHDG_1
          CALL RANGE_CHECK(205,R,CMESS1)
          CALL RATE_OF_CHANGE(205,R,PREVAL(205),CMESS1,IFIRST)
          PREVAL(205)=R
C Platform azimuth and wander angle
          RPAZI=IVAL_16(9)/2.**15*180.
          IF(RPAZI.LT.0.) RPAZI=RPAZI+360.
          RWA=RPAZI-RIHDG_1
C VN    - INU north velocity (m s-1 +ve north)
          R=COSD(RWA)*RVX+SIND(RWA)*RVY      !INU north velocity (m s-1)
          CALL RANGE_CHECK(200,R,CMESS1)
          CALL RATE_OF_CHANGE(200,R,PREVAL(200),CMESS1,IFIRST)
          PREVAL(200)=R
C VE    - INU east velocity (m s-1 +ve east)
          R=SIND(RWA)*RVX-COSD(RWA)*RVY     !INU east velocity (m s-1)
          CALL RANGE_CHECK(201,R,CMESS1)
          CALL RATE_OF_CHANGE(201,R,PREVAL(201),CMESS1,IFIRST)
          PREVAL(201)=R
C PITR  - INU pitch rate (deg s-1)
          RPITR_1=IVAL_16(31)/2.**13*180.    !INU pitch rate (deg s-1)
C YAWR  - INU yaw rate (deg s-1)
          RYAWR_1=IVAL_16(32)/2.**13*1      !INU yaw rate (deg s-1)
        END IF
        IFIRST=IFIRST+1
      END DO
C      CLOSE(42)
      IT1=NFDATA(NPLOC(2),STATUS(1))
      IT2=NFDATA(NPLOC(3),STATUS(1))
      IHRS=IBITS(IT1,8,4)*10+IBITS(IT1,4,4)
      IMINS=IBITS(IT1,0,4)*10+IBITS(IT2,8,4)
      ISECON=IBITS(IT2,4,4)*10+IBITS(IT2,0,4)
      WRITE(6,50)'Time:',IHRS,IMINS,ISECON
C************** Intercomparison of parameters  *******************
C First compare the pressure height with the radar height.
C Check the radar is locked on
C       IF(.NOT.BTEST(NFDATA(NPLOC(27),1),6)) THEN
C If it isn't locked on then do comparison.
      CALL MEANPARAM(37,R)
      RES1=CAL37(3)*R**2+CAL37(2)*R+CAL37(1)
      CALL MEANPARAM(8,R)
      RPRESS=CAL8(3)*R**2+CAL8(2)*R+CAL8(1)
      RPHT=44330.77*(1.0-(RPRESS/1013.25)**(1.0/5.256)) !Press height (m)
c          WRITE(6,*) 'PRESSURE HEIGHT=',RPHT
c          WRITE(6,*) 'RADAR HEIGHT=',RES1
      X=(ABS(RES1-RPHT))    
      IF(ABS(RES1-RPHT).GT.200) THEN
      WRITE(6,50)'Time:',IHRS,IMINS,ISECON
      WRITE(6,*)' Difference between radar and pressure height'//
     -    ' greater than normal!'
      WRITE(6,60)' Difference= ',X,' metres'
      WRITE(6,*)' This could be due to the altitude being'//
     -    ' greater than 5000 feet.'
           END IF
C         END IF
      IF(RPRESS.LT.5.0)RPRESS=1.0+RPRESS


C  Pitot static airspeed compared with INS airspeed?
        IF(OFITTED(9).AND.OFITTED(201)) THEN
          CALL MEANPARAM(9,R)
          R1PSP=CAL9(2)*R+CAL9(1)
          RMACH1=5.0*((1.0+R1PSP/RPRESS)**(2.0/7.0)-1)
          RMACH1=SQRT(ABS(RMACH1))
          RVEL=0.0
          RVEL=340.294*RMACH1*SQRT(RPRESS/1013.25) !Ind air speed (m/s)
      

c          CALL INS_STATS(121,21,RVALN,OSTATUS,R)
c          CALL INS_STATS(122,22,RVALE,OSTATUS,R)

          RINUVEL=SQRT(PREVAL(200)**2+PREVAL(201)**2)
          Y=(ABS(RINUVEL-RVEL))
          IF(ABS(RINUVEL-RVEL).GT. 30.0) THEN
            WRITE(6,50)'Time:',IHRS,IMINS,ISECON
            WRITE(6,*)'Absolute difference between INS and pitot'//
     -' static velocities greater than usual '
            WRITE(6,*)'pitot velocity= ',RVEL
            WRITE(6,*)'INU velocity  = ',RINUVEL
            WRITE(6,*)'Absolute difference (m/s)in velocities= ',Y
          END IF  
        END IF
C
C Temperature probe comparison
C
C For flights after A217 the two Rosemount temperature sensors data are
C recorded  and processed in the same way.
      IF(OFITTED(10) .AND. OFITTED(23)) THEN 
C
C ACTION OF PROCESSING AND CHECKING THE DATA.
        CALL DEICED_TEMP_STATS(10,RTDI,I2,R2,OST) 
        CALL DEICED_TEMP_STATS(23,RTNDI,I2,R2,OST)
        RTDI=CAL10(3)*RTDI**2+CAL10(2)*RTDI+CAL10(1)
        RTNDI=CAL23(3)*RTNDI**2+CAL23(2)*RTNDI+CAL23(1)
        IF(ABS(RTDI-RTNDI).GT.1.0) THEN
          X=ABS(RTDI-RTNDI)
          WRITE(6,70)'Temperature difference= ',X,' Kelvin.'
          WRITE(6,*)'Difference between Deiced and Non Deiced'//
     -        ' temperatures greater than normal(>1K).'          
        END IF      
      END IF
      RETURN

C                 
10    FORMAT(2X,3(I2.2))           
11    FORMAT(2X,I2,2X,3(I2.2))           
15    FORMAT(2X,I3,3(I2),F8.2,1X,F8.2)    
30    FORMAT(1X,A9,1X,I3,1X,A60)
35    FORMAT(1X,A19,A60)
40    FORMAT(A60)
50    FORMAT(3X,A5,1X,3(I2.2))
60    FORMAT(1X,A12,F12.2,A7)
70    FORMAT(1X,A24,F9.2,A8)
      END
C*******************************************************************************
      SUBROUTINE MEANPARAM(IP,RESULT)
C
C Calculates then mean value of a DRS parameter over a period of 1s.
C The result is returned as a real
C positive number.  This subroutine should only be used for parameters
C which are recorded as unsigned 12 bit binary numbers, ie not for BCD and
C not for 16 bit parameters.
C
C V1.00  01/01/89  W.D.N.JACKSON
C V1.01  01/07/93  W.D.N.JACKSON  Changed to accomodate 16 bit recording
C
      IMPLICIT  NONE
      INTEGER*4 IP,I,J,JSUM,JCNT
      REAL*4    RESULT,R
      INCLUDE  'HCOM_DEF.FOR'
C
C External data.
C
C IP      I*4  Read    Passed         Parameter number (1 to 512)
C RESULT  R*4  Write   Passed         Mean value of parameter over 1s block
C NFDATA  I*2  Read    H_DRS_LOG      Block of compressed DRS data (see
C                                     H_DRS_LOG for format)
C NPLOC   I*2  Read    H_DERIVE       Location in NFDATA where the data for
C                                     each parameter starts. (0 if the 
C                                     parameter was not recorded, else
C                                     in range 9 to about 2000)
C NPFREQ  I*2  Read    H_DERIVE       The sampling rate of each parameter
C                                     (0 if not recorded, else in range 1 
C                                     to 64 at present)
C
C Note that the DRS only records bits 0 to 11 for most parameters, and this
C subroutine masks out the top four bits of each word.
C
      RESULT=0.0
      IF(NPLOC(IP).NE.0) THEN          !If parameter recorded
        JSUM=0
        JCNT=0
                   
        DO I=NPLOC(IP),NPLOC(IP)+NPFREQ(IP)-1
          J=NFDATA(I,STATUS(1))
          IF(IP .EQ. 39) THEN
            R=(IBITS(J,8,2)*100+IBITS(J,4,4)*10+IBITS(J,0,4))*0.1
            IF(BTEST(J,10))R=-R
            JSUM=JSUM+R
          ELSE
            JSUM=JSUM+(J.AND.'FFF'X)
          END IF
          JCNT=JCNT+1
        END DO
        RESULT=FLOAT(JSUM)/JCNT        !Work out mean
      END IF

      RETURN
      END
C*******************************************************************************
      SUBROUTINE PARA_STATS(IP,OSTATUS,RSTDEV)
      IMPLICIT NONE
      INTEGER*4 ISTATS(512,4),JSUM,JSUMSQ,JNUM(512),J,J1,JSQ
      INTEGER*4 IPASS(512),ITLAST(512),IP,I,IT1,IT2,ITIM,IDIFF
      LOGICAL*4 OSTATUS
      INCLUDE   'HCOM_DEF.FOR'
      REAL*4    RSTDEV,RMEAN,RVAR,RRAT,R,RSUM,RSUMSQ
C Find sum and sum of squares of present set of numbers for parameter IP.
C
      OSTATUS=.FALSE.
      JSUM=0
      JSUMSQ=0
      J=0
      J1=0
      JSQ=0
      ITIM=NFDATA(NPLOC(3),STATUS(1))
      IF(NPLOC(IP) .NE. 0) THEN
        DO I=NPLOC(IP),NPLOC(IP)+NPFREQ(IP)-1
          J=NFDATA(I,STATUS(1))
          IF(.NOT. BTEST(J,12)) THEN
C Check that the parameter is not the de_iced scale
            IF(IP .NE. 11 .AND. IP .NE. 39) THEN
C If it isn't then mask out the top bits
              JSUM=JSUM+(J.AND.'FFF'X)
              J1=J.AND.'FFF'X 
            ELSE IF(IP.EQ.11) THEN
C If it is then just do a straight addition. As parameter 11 is BCD then no 
C conversion/alteration is necessary.
C
              JSUM=JSUM+J
              J1=J
            ELSE IF(IP .EQ. 39) THEN
              R=(IBITS(J,8,2)*100+IBITS(J,4,4)*10+IBITS(J,0,4))*0.1
              IF(BTEST(J,10))R=-R
              J=NINT(R)
c                WRITE(6,*)J,R
              JSUM=JSUM+J 
              J1=J   
            END IF
C              
            JSQ=J1**2
            JSUMSQ=JSUMSQ+JSQ
            JNUM(IP)=JNUM(IP)+1
          END IF
        END DO
        IF(JNUM(IP) .EQ. 0) THEN
          IF(IP .NE. 11) THEN
C If it isn't then mask out the top bits
             JSUM=J .AND. 'FFF'X
             J1=J.AND.'FFF'X   
          ELSE
C If it is then just do a straight addition. As parameter 11 is BCD then no 
C conversion/alteration is necessary.
C
            JSUM=JSUM+J
            J1=J
          END IF
          JSUMSQ =  J1**2           
          JNUM(IP)=1
        END IF  
C Udate array with totals of the parameters stored.
C                                           
        ISTATS(IP,1)=ISTATS(IP,1)+JSUM
        ISTATS(IP,2)=ISTATS(IP,2)+JSUMSQ
        ISTATS(IP,3)=ISTATS(IP,3)+JNUM(IP)
        IPASS(IP)=IPASS(IP)+1
C If the program has been run through enough times then calculate standard
C deviation.
        IF(ISTATS(IP,3).GE.90) THEN
          IT1=NFDATA(NPLOC(2),STATUS(1))
          IT2=NFDATA(NPLOC(3),STATUS(1))
          ITIM=(IBITS(IT1,8,4)*10+IBITS(IT1,4,4))*3600+
     -        (IBITS(IT1,0,4)*10+IBITS(IT2,8,4))*60+
     -        (IBITS(IT2,4,4)*10+IBITS(IT2,0,4))
          IDIFF=ITIM-ITLAST(IP)
          RRAT= REAL(IPASS(IP))/REAL(IDIFF)
C          IF(IP.EQ.8)WRITE(6,*)RRAT,IPASS(IP),IDIFF
          ITLAST(IP)=ITIM
          IF(IP.EQ.39) THEN       
            RSUM=REAL(ISTATS(IP,1))*0.1
            RSUMSQ=REAL(ISTATS(IP,2))*0.01
            RMEAN = RSUM/REAL(ISTATS(IP,3))
c             WRITE (6,*)RMEAN
            RVAR=ABS(RSUMSQ/REAL(ISTATS(IP,3))-(RMEAN**2))
            RSTDEV=SQRT(RVAR)
            OSTATUS=.TRUE.              
          ELSE
            RMEAN = REAL(ISTATS(IP,1))/REAL(ISTATS(IP,3))
            RVAR=ABS((REAL(ISTATS(IP,2))/REAL(ISTATS(IP,3)))-(RMEAN**2))
            RSTDEV=SQRT(RVAR)  
          END IF   
          IPASS(IP)=0         
          ISTATS(IP,1)=0
          ISTATS(IP,2)=0
          ISTATS(IP,3)=0
          OSTATUS=.TRUE.
          RSTDEV=RSTDEV*RRAT                 
        END IF     
      END IF
      JNUM(IP)=0
      RETURN
      END
C******************************************************************************
        SUBROUTINE INS_STATS(IC,IF,RESULT,OSTATUS,RSTDEV)
C
C Merges the coarse and fine INS data to reconstitute a full 16 bit
C INS word.  This is returned as a 32 bit word with the sign extended to
C 32 bits if bit 15 of the 16 bit word is set.
C
C V1.00  01/01/89  W.D.N.JACKSON
C
      IMPLICIT  NONE
      INTEGER*4 IVAL(512,64),IC,IF,IFS,J,ICS,I,JT,JD
      INCLUDE   'HCOM_DEF.FOR'
      REAL*4    RSTATS(26,4),RMEAN,RSTDEV,RVAR,RSUM,RSUMSQ,R,RESULT,
     &    RCNT
      LOGICAL*4 OSTATUS
C External data.
C
C IC      I*4  Read    Passed         The coarse parameter number
C                                     (54, 115-117, 120-122)
C ICS     I*4  Read    Passed         The coarse parameter sample number
C                                     (1-4max depending on parameter)
C IF      I*4  Read    Passed         The fine parameter number
C                                     (15-17, 20-22, 53)
C IFS     I*4  Read    Passed         The fime parameter sample number
C                                     (1-32, except for param 53)
C J       I*4  Write   Passed         The merged INS value
C NFDATA  I*2  Read    H_DRS_LOG      Block of compressed DRS data (see
C                                     H_DRS_LOG for format)
C NPLOC   I*2  Read    H_DERIVE       Location in NFDATA where the data for
C                                     each parameter starts. (0 if the 
C                                     parameter was not recorded, else
C                                     in range 9 to about 2000)
      IFS=1
      ICS=1  
      RESULT=0.0
      RSUM=0
      RCNT=0
      RSUMSQ=0
      OSTATUS=.FALSE.
      RSTDEV=0.0
      RVAR=0.0
      R=0.0
      DO I=NPLOC(IF),NPLOC(IF)+NPFREQ(IF)-1
        IVAL(IF,IFS)=NFDATA(I,STATUS(1)).AND.'FFF'X
        IVAL(IC,ICS)=NFDATA(NPLOC(IC)+ICS-1,STATUS(1)).AND.'FFF'X
        JT=IBITS(IVAL(IC,ICS),8,4)       !Get four most coarse bits
        JD=IBITS(IVAL(IC,ICS),6,2)-IBITS(IVAL(IF,IFS),10,2) !Look for change
        JT=(JT+JD/3).AND.'F'X            !Correct top 4 bits if necessary
        J=IVAL(IF,IFS)+ISHFT(JT,12)      !Add top 4 bits to bottom 12 bits
        IF(BTEST(J,15)) J=J.OR.'FFFF0000'X !Sign extend to 32 bits if -ve
        IF(IF.EQ.15 .OR. IF.EQ.17) THEN
          R=(J/4)*0.02197
        END IF        
        IF(IF.EQ.16) THEN
          R=(J/8)*0.02197
        END IF        
        IF(IF.EQ.20) THEN
          R=(J/2)*0.045721
        END IF        
        IF(IF.EQ.21 .OR. IF.EQ.22) THEN
          R=J*0.045721
        END IF       
        IF(IF.EQ.10 .OR. IF.EQ.23) THEN
          R=J
        ENDIF        
        RSUM=RSUM+R 
        RCNT=RCNT+1.0
        RSUMSQ=RSUMSQ+(R**2)
        IFS=IFS+1
        IF(IFS .GE. 17)ICS=2
      END DO
      RSTATS(IF,1)=RSTATS(IF,1)+RSUM
      RSTATS(IF,2)=RSTATS(IF,2)+RSUMSQ
      RSTATS(IF,3)=RSTATS(IF,3)+RCNT
C If the program has been run through enough times then calculate standard
C deviation.
      IF(RSTATS(IF,3).GE.90) THEN
        RMEAN =RSTATS(IF,1)/RSTATS(IF,3)
        RVAR=ABS((RSTATS(IF,2)/RSTATS(IF,3))-(RMEAN**2))
        RSTDEV=SQRT(RVAR)                      
        RSTATS(IF,1)=0
        RSTATS(IF,2)=0
        RSTATS(IF,3)=0
        OSTATUS=.TRUE.
      END IF     
      RESULT=RSUM/RCNT        !Work out mean
      RETURN
      END
C***************************************************************************
C
C Note that the DRS only records bits 0 to 11.  Bit 15 is only set for the
C first word of a block, and anything else is rubbish.  The top 4 bits are
C therefore masked out.  No checking is done for parity errors, and no
C averaging takes place.
C
C The function below extracts from the DRS data block the specified sample
C of the specified parameter, masking off the top four bits, and returns it
C as a 32 bit word with no sign extension.
C
C   IP is the parameter number (1-512)
C   IS is the sample number (1-64) depending on the parameter sampling rate.
C
C                            
C The INS data words are 16 bits.  The lower 12 bits are recorded as fine
C parameters on the DRS, and the upper 12 bits are recorded as coarse
C parameters on the DRS.  There is therefore an overlap of 8 bits between
C the coarse and fine values.  The fine values are recorded at different
C rates to the coarse, and at slightly different times.  The possibility
C therefore exists that the a coarse value is read that does not match the
C fine value.  The code below looks for a difference in the top two of the
C eight overlapping bits of the coarse and fine values (JD).  Because of
C limits to maximum rates of change this
C difference will be +-3 since in one case the bits may be 00 and 11, and
C in the other 11 and 00.  +-1 is therefore added, as appropriate to the
C top 4 coarse bits to correct them, and any overflow (overall change of
C sign is masked off).
C 
C Note that not all 16 INS bits are necessarily significant, and it is left
C to the calling program to strip out (by division) any lower non-
C significant bits.
C  
C***************************************************************************
      SUBROUTINE DEICED_TEMP_STATS(IPARF,RMEAN,IDIFF,RSTDEV,OSTATUS)
C This subroutine merges the two old deiced temperature parameters and
C calculates the mean and standard deviation of a set of data
C  
      IMPLICIT  NONE
      LOGICAL*4 OSTATUS
      INTEGER*4 IR,IPARC,IPARF,IX,I,ILOWT,IHIGHT,ILSTEMP,IT1,IT2,
     &    ITIM,IDIFF
      REAL*4    RSUM,RSUMSQ,RCNT,RSTORE(2,4),RMEAN,RM,RVAR,RSTDEV
      INCLUDE   'HCOM_DEF.FOR'

      OSTATUS=.FALSE.
      RSUM=0.0
      RSUMSQ=0.0
      RCNT=0.0
      IPARC=IPARF+1
      IF(IPARF.EQ.10)IX=1
      IF(IPARF.EQ.23)IX=2
      DO I=1,32
C MERGE COARSE AND FINE
        ILOWT=NFDATA(NPLOC(IPARF)+I-1,STATUS(1)).AND.'FFF'X
        IHIGHT=NFDATA(NPLOC(IPARC)+((I-1)/16),STATUS(1)).AND.'F'X
        IR=ILOWT+IHIGHT*4096
C ADJUST DATA TO COPE WITH SCALE CHANGES
        IF(I.GT.1) THEN
          IF(IR-ILSTEMP.GT.3500)IR=IR-4096
          IF(IR-ILSTEMP.LT.-3500)IR=IR+4096
        END IF
C UPDATE THE RUNNING TOTALS
        ILSTEMP=IR
        RSUM=RSUM+REAL(IR)
        RSUMSQ=RSUMSQ+(REAL(IR)**2)
        RCNT=RCNT+1.0
      END DO
C ADD RUNNING TOTALS TO STATISTICS ARRAY
      RSTORE(IX,1)=RSTORE(IX,1)+RSUM
      RSTORE(IX,2)=RSTORE(IX,2)+RSUMSQ
      RSTORE(IX,3)=RSTORE(IX,3)+RCNT
C CALCULATE MEAN OF SECONDS WORTH OF DATA
      RMEAN=RSUM/RCNT
C CALCULATE STANDARD DEVIATION
      IF(RSTORE(IX,3).GT.89.9) THEN
        RM=RSTORE(IX,1)/RSTORE(IX,3)
        RVAR=ABS((RSTORE(IX,2)/RSTORE(IX,3))-(RM**2))
        RSTDEV=SQRT(RVAR)
        OSTATUS=.TRUE.
        RSTORE(IX,1)=0.0
        RSTORE(IX,2)=0.0
        RSTORE(IX,3)=0.0
      END IF        
      IT1=NFDATA(NPLOC(2),STATUS(1))
      IT2=NFDATA(NPLOC(3),STATUS(1))
      ITIM=(IBITS(IT1,8,4)*10+IBITS(IT1,4,4))*3600+
     -    (IBITS(IT1,0,4)*10+IBITS(IT2,8,4))*60+
     -    (IBITS(IT2,4,4)*10+IBITS(IT2,0,4))
      IDIFF=ITIM-NINT(RSTORE(IX,4))
      RSTORE(IX,4)=REAL(ITIM)
      RETURN
      END
C*******************************************************************************
      SUBROUTINE READ_CONSTS
C
C Reads calibration constants from the file HOR_CALIB.DAT.  Contents of the
C file must match the requirements of this routine exactly.  The constants are
C read into the CALS common block from where they are used by the main
C HOR_CALCS routine.  Note that HOR_CALIB only has data for the basic
C parameters calculated by HOR_CALCS; there are other files read in by 
C different routines for MARSS_CALCS, etc.
C
C V1.00  17/04/89  J.HARMER
C V1.01  06/08/92  W.D.N.JACKSON
C        Now keeps going if fails to read a constant, but issues a message.
C        Cabin pressure constants added
C        Non de-iced constants added
C        De-iced constants now a single quadratic
C        Refractometer constants removed
C        Constants reordered
C
      IMPLICIT  NONE
      INTEGER*4 IERR(100),I,IOS
      CHARACTER CI*2
      REAL*4 RTERR,RGAMMA,RLAMDA,RUPW,RTASK,
     -        CAL8(3),CAL9(2),CAL10(3),CAL14(3),CAL18(2),CAL19(2),
     -        CAL23(3),CAL37(3),CAL40(2),CAL42(2),CAL43(3),CAL47(2),
     -        CAL58(2),CAL70(2),CAL71(2),CAL72(2),CAL73(2),CAL74(2),
     -        CAL75(2),CAL76(2),CAL77(2),CAL81(2),CAL82(2),CAL83(2),
     -        CAL87(2),CAL91(2),CAL92(2),CAL93(2),CAL100(14),CAL101(5),
     -        CAL103(4),CAL106(6),CAL144(2),CAL145(2),CAL146(2),
     -        CAL147(2),CAL148(2),CAL149(2)
      COMMON  /CALS/ RTERR,RGAMMA,RLAMDA,RUPW,RTASK,
     -        CAL8,CAL9,CAL10,CAL14,CAL18,CAL19,
     -        CAL23,CAL37,CAL40,CAL42,CAL43,CAL47,
     -        CAL58,CAL70,CAL71,CAL72,CAL73,CAL74,
     -        CAL75,CAL76,CAL77,CAL81,CAL82,CAL83,
     -        CAL87,CAL91,CAL92,CAL93,CAL100,CAL101,
     -        CAL103,CAL106,CAL144,CAL145,CAL146,
     -        CAL147,CAL148,CAL149

      DO I=1,100
        IERR(I)=0
      END DO
      OPEN(UNIT=10,FILE='HOR_CALIB.DAT',STATUS='OLD',READONLY,
     -    IOSTAT=IOS)
      IF(IOS.NE.0) THEN
        CALL EXIT
      END IF
      READ(10,1000,IOSTAT=IERR(1))
      READ(10,1001,IOSTAT=IERR(6)) RTERR   !Deiced heating correction (deg C)
      READ(10,1001,IOSTAT=IERR(7)) RGAMMA  !Heading correction factor
      READ(10,1001,IOSTAT=IERR(8)) RLAMDA  !Temperature recovery factor
      READ(10,1001,IOSTAT=IERR(9)) RUPW    !Upwash factor
      READ(10,1001,IOSTAT=IERR(10)) RTASK   !TAS correction factor
      READ(10,1003,IOSTAT=IERR(11)) CAL8   !Static Pressure (mb)
      READ(10,1002,IOSTAT=IERR(12)) CAL9   !Pitot static pressure (mb)
      READ(10,1003,IOSTAT=IERR(13)) CAL10  !De-iced temperature (C)
      READ(10,1003,IOSTAT=IERR(14)) CAL14  !Cabin pressure (mb)
      READ(10,1002,IOSTAT=IERR(15)) CAL18  !Angle of attack (rad)
      READ(10,1002,IOSTAT=IERR(16)) CAL19  !Angle of sideslip (rad)
      READ(10,1003,IOSTAT=IERR(17)) CAL23  !Non de-iced temperature (C)
      READ(10,1003,IOSTAT=IERR(18)) CAL37  !Radar height (m)
      READ(10,1002,IOSTAT=IERR(19)) CAL40  !RAL receiver output (volts)
      READ(10,1002,IOSTAT=IERR(20)) CAL42  !Johnson Williams  (g m-3)
      READ(10,1003,IOSTAT=IERR(21)) CAL43  !Manual Hygrometer            (C)
      READ(10,1002,IOSTAT=IERR(22)) CAL47  !Nephelometer (nephels)
      READ(10,1002,IOSTAT=IERR(23)) CAL58  !General Eastern Hygrometer   (C) 
      READ(10,1002,IOSTAT=IERR(24)) CAL70  !Total water detector (gkg-1)
      READ(10,1002,IOSTAT=IERR(25)) CAL71  !TWC nose temp (C)
      READ(10,1002,IOSTAT=IERR(26)) CAL72  !TWC sample temp (C)
      READ(10,1002,IOSTAT=IERR(27)) CAL73  !TWC ambient temp (C)
      READ(10,1002,IOSTAT=IERR(28)) CAL74  !TWC source temp (C)
      READ(10,1002,IOSTAT=IERR(29)) CAL75  !TWC evap heater 1 (A)
      READ(10,1002,IOSTAT=IERR(30)) CAL76  !TWC evap heater 2 (A)
      READ(10,1002,IOSTAT=IERR(31)) CAL77  !TWC source current (mA)
      READ(10,1002,IOSTAT=IERR(32)) CAL81  !Upper pyran clear sig & zero (W m-2)
      READ(10,1002,IOSTAT=IERR(33)) CAL82  !Upper pyran red   sig & zero (W m-2)
      READ(10,1002,IOSTAT=IERR(34)) CAL83  !Upper pyrge       sig & zero (W m-2)
      READ(10,1002,IOSTAT=IERR(35)) CAL87  !All temps (deg C)
      READ(10,1002,IOSTAT=IERR(36)) CAL91  !Lower pyran clear sig & zero (W m-2)
      READ(10,1002,IOSTAT=IERR(37)) CAL92  !Lower pyran red   sig & zero (W m-2)
      READ(10,1002,IOSTAT=IERR(38)) CAL93  !Lower pyrge       sig & zero (W m-2)
      READ(10,1007,IOSTAT=IERR(39)) (CAL100(I),I=1,7) !Ozone range bins
      READ(10,1007,IOSTAT=IERR(40)) (CAL100(I),I=8,14) !Ozone ranges
      READ(10,1005,IOSTAT=IERR(41)) CAL101 !SO2 Mixing ratio             (ppb)
      READ(10,1004,IOSTAT=IERR(42)) CAL103 !NOx and NO Mixing ratio      (ppb)
      READ(10,1006,IOSTAT=IERR(43)) CAL106 !Ozone pressure
      READ(10,1002,IOSTAT=IERR(44)) CAL144 !KFA NO pressure (mb)
      READ(10,1002,IOSTAT=IERR(45)) CAL145 !KFA NOY pressure (mb)
      READ(10,1002,IOSTAT=IERR(46)) CAL146 !KFA NO counts per sec
      READ(10,1002,IOSTAT=IERR(47)) CAL147 !KFA NOY counts per sec
      READ(10,1002,IOSTAT=IERR(48)) CAL148 !KFA NO min mean signal (ppb)
      READ(10,1002,IOSTAT=IERR(49)) CAL149 !KFA NOY min mean signal (ppb)
      CLOSE(UNIT=10,IOSTAT=IOS)
      DO I=1,100
        IF(IERR(I).NE.0) THEN
          WRITE(CI,1010,IOSTAT=IOS) I
        END IF
      END DO
      RETURN
1000  FORMAT(////)
1001  FORMAT(10X,1G)
1002  FORMAT(10X,2G)
1003  FORMAT(10X,3G)
1004  FORMAT(10X,4G)
1005  FORMAT(10X,5G)
1006  FORMAT(10X,6G)
1007  FORMAT(10X,7G)
1010  FORMAT(A)
      END
C*******************************************************************************
      SUBROUTINE RANGE_CHECK(IPAR,R,CMESS1)
      IMPLICIT NONE
      INTEGER*4 ILIM(512,9),IPAR
      REAL*4 R
      CHARACTER*60 CMESS1(25)
      LOGICAL*4 OMESS_CALLED(512,5)
      COMMON /CHECK_COMMON/ILIM,OMESS_CALLED                 
        
      IF(R.GT.REAL(ILIM(IPAR,1)) .OR. R .LT. REAL(ILIM(IPAR,2)))THEN
        IF(.NOT.OMESS_CALLED(IPAR,1)) THEN
          WRITE(6,1000)'Parameter',IPAR,CMESS1(ILIM(IPAR,6)) 
          OMESS_CALLED(IPAR,1)=.TRUE.
        END IF
      END IF   
      RETURN          
1000  FORMAT(1X,A9,1X,I3,1X,A60)  
1100  FORMAT(3X,A5,1X,3(I2.2))
      END  
C*******************************************************************************
      SUBROUTINE NOISE_CHECK(IPAR,OSTATUS,RSTDEV,CMESS1)
      IMPLICIT NONE
      INTEGER*4 ILIM(512,9),IPAR
      REAL*4 RSTDEV
      CHARACTER*60 CMESS1(25)
      LOGICAL*4 OMESS_CALLED(512,5),OSTATUS
      COMMON /CHECK_COMMON/ILIM,OMESS_CALLED
C Noise check
      IF(OSTATUS) THEN            
        IF(RSTDEV .GT. REAL(ILIM(IPAR,3))) THEN  
          IF(.NOT.OMESS_CALLED(IPAR,2)) THEN
            WRITE(6,1000)'Parameter',IPAR,CMESS1(ILIM(IPAR,7))
            OMESS_CALLED(IPAR,2)=.TRUE.
          END IF
        END IF
      END IF                        
      RETURN
1000  FORMAT(1X,A9,1X,I3,1X,A60)  
1100  FORMAT(3X,A5,1X,3(I2.2))
      END                            
C*******************************************************************************
      SUBROUTINE RATE_OF_CHANGE(IPAR,RVAL,RPREV_VAL,CMESS1,IFIRST)
      IMPLICIT NONE
      INTEGER*4 ILIM(512,9),IPAR,IFIRST
      REAL*4 RVAL,RPREV_VAL
      CHARACTER*60 CMESS1(25)
      LOGICAL*4 OMESS_CALLED(512,5)
      COMMON /CHECK_COMMON/ILIM,OMESS_CALLED

      IF(ABS(RVAL-RPREV_VAL) .GT. REAL(ILIM(IPAR,5))) THEN
        IF(.NOT.OMESS_CALLED(IPAR,3).AND.IFIRST.GT.0) THEN
          WRITE(6,1000)'Parameter',IPAR,CMESS1(ILIM(IPAR,8))
          OMESS_CALLED(IPAR,3)=.TRUE.
        END IF
      END IF                        
      RETURN
1000  FORMAT(1X,A9,1X,I3,1X,A60)
1100  FORMAT(3X,A5,1X,3(I2.2))
      END                            
