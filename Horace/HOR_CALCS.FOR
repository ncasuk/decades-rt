
      SUBROUTINE HOR_CALCS(ISEC)
C
C This routine computes all the standard HORACE derived parameters using data
C in the DRS data block, and calibration constants read in from HOR_CALIB.DAT.
C H_DERIVE only calls this routine once every three seconds, so that new
C parameters are only produced at this interval.  The derived parameters go
C into the DERIVE array in the HCOM common block, which contains a circular
C buffer of 12800 values, for each of the possible 196 derived parameters.
C The value of ISEC passed to this routine indicates where in the circular 
C buffer the new derivations are to go.  See HOR_CALCS.TXT for a description
C of how each parameter is derived.  Note that GPS_LOG and some other processes
C put data directly into the DERIVE array without passing through this program.
C The valid parameters in the DERIVE array are specified in PARANO.TXT.
C
C
C External data:
C
C NFDATA  I*2  Read    H_DRS_LOG      Block of compressed DRS data (see
C                                     H_DRS_LOG for format)
C NPLOC   I*2  Read    H_DERIVE       Location in NFDATA where the data for
C                                     each parameter starts. (0 if the 
C                                     parameter was not recorded, else
C                                     in range 9 to about 2000)
C ISEC    I*2  Passed  H_DERIVE       Latest sec to fill circular buffer   
C RTABLE  R*4  Passed  HEIM_CALCS     (640,2) 2 lookup tables of correction 
C                                     values for the Heimann to 
C                                     automatically calibrate to surface 
C                                     temperature.
C                                     STATUS(13) used to point to current 
C                                     completed table, 0(initially)/1/2
C
C Note that the DRS records 16 bit unsigned numbers.  Not all bits may have
C a meaning, particular for older digital parameters.
C
C Subroutines called:
C
C MEANPARAM    This file      Gets mean DRS parameter over 1s
C
C Arrays used:
C
C DERIVE(12800,196)  A circular buffer of 196 (real) derived parameters, 
C                    calculated  once every third second.(see PARANO.TXT
C                    for order).
C SPECIAL(12800,10)  A circular buffer of 10 (integer) parameters,(time,
C                    event,and flags), calculated once every third second,in 
C                    the following order: JHRS,JMIN,JSEC,JEVM,IDP,IHEIM,IFLDP
C
C V2.00  06/08/02  W.D.N.JACKSON
C
      IMPLICIT   NONE
      INTEGER*2  NFDATA(2048,2),NPLOC(512),NPFREQ(512),STATUS(256),
     -    SPECIAL(12800,10),JTEMP(2),JVAL,J,JEVM
      INTEGER*4  ITEMP,IVAL,IP,IS,ISEC,ICPU,I1,IV12,IFNO,
     -    I2,I3,I4,N,INDEX
      EQUIVALENCE (JTEMP,ITEMP)
      REAL*4     DERIVE(12800,196),CAL(512,6),RTABLE(640,2),TDRS
!      INTEGER*4  ISRTMOD,IENDMOD,I
!      REAL*4     PERMOD(21)

      COMMON     /HCOM/ NFDATA,NPLOC,NPFREQ,STATUS,DERIVE,SPECIAL
      COMMON     /HEIM_LOOK/ RTABLE
      COMMON     /SUNPOS/ SAZI,SZEN
      COMMON     /CALS/ CAL
      VOLATILE   /HCOM/
      REAL*4  RSPR,RPSP,RPHGT,RPHGTF,RMACH,RIATDI,
     -    RTATDI,RTATDC,RIATND,RIAS,RTAS,RPOT,RDEW,RRH,RVP,
     -    RMAD,RSHUM,RMMR,RHMR,R,RD,RT,RJW,RLWC,RHEIM,RUCLR,RURED,RUIR,
     -    RLCLR,RLRED,RLIR,RAOA,RAOSS,RVX,RVY,RVZ,RROLL,RPTCH,
     -    RIHDG,RWA,RPAZI,RVE,RPITR,RYAWR,RIGS,RIDA,RILAT,RILNG,RTA,
     -    RTS,RSR,RCR,RSP,RCP,RSH,RV1,RV2,RV3,RV4,RV5,RIP,RU,RV,RW,
     -    RIWS,RIWA,RSZEN,RSAZI,RCORR,RSHDG,RCAL,RHO,
     -    RHO2,RPT,RPT2,RTWC,RF,RFVP,RFMAD,RFSHUM,
     -    RFHMR,R1,R2,R3,ROZMR,RPOTE,RTATND,RTATNC,RDAD,RHYCC,RESTOP,
     -    RESBOT,RS,RVN,RCNEXZ,RCH,SAZI,SZEN,RTSAMPC,ROXYCOR,RL,RSALB,
     -    RNALB,RLVIS,RUVIS,RVALB,RNETIR,RUNIRS,RLNIRS,RST,RTWCDP,RHGT,
     -    R10MWS,VK,EPS,USTAR,US,Z0,R_TATDI,REFRACT,REF_INDEX,RREFR,
     -    RREFRM,RLCLVL,RNVL,RNVT
      INCLUDE '($JPIDEF)'
C
C The following functions extract from the DRS data block the specified sample
C of the specified parameter and puts it into 16 or 32 bits word.  There is a
C function for just keeping 12 bits, for use with old instruments.  Note that
C the vast majority of DRS data values are unsigned 16 bit values.
C
C   IP is the parameter number (1-512)
C   IS is the sample number (1-64) depending on the parameter sampling rate.
C
      JVAL(IP,IS)=NFDATA(NPLOC(IP)+IS-1,STATUS(1))
      IV12(IP,IS)=NFDATA(NPLOC(IP)+IS-1,STATUS(1)).AND.'FFF'X
      IVAL(IP,IS)=JZEXT(NFDATA(NPLOC(IP)+IS-1,STATUS(1))) !16 bit unsigned
C
      DATA ICPU /10/                   !Start with max CPU for H2O2 model
C RFLNO - Flight number
      J=IV12(1,1)
      IFNO=IBITS(J,8,4)*100+IBITS(J,4,4)*10+IBITS(J,0,4) !Flight no
      DERIVE(ISEC,1)=FLOAT(IFNO)
C EVM   - DRS event mark
      J=JVAL(4,1)
      JEVM=IBITS(J,8,4)*100+IBITS(J,4,4)*10+IBITS(J,0,4) !Event mark
      SPECIAL(ISEC,4)=JEVM
!C SPR   - Static pressure (mb)
      RSPR=900.
      DERIVE(ISEC,64)=RSPR
!C PSP   - Pitot static pressure difference (mb)
      RPSP=80.
      DERIVE(ISEC,65)=RPSP
C MACH  - Mach No
      RMACH=0.0
      IF(RPSP.GT.0.AND.RSPR.GT.0) 
     -    R=5.0*((1.0+RPSP/RSPR)**(2.0/7.0)-1.0)
      IF(R.GT.0) RMACH=SQRT(R)                   !Mach No
      DERIVE(ISEC,6)=RMACH
C AOA   - Angle of attack (deg)  (+ve vane pointing down)
      RAOA=0.
      DERIVE(ISEC,36)=RAOA
C AOSS  - Angle of sideslip (deg)  (+ve vane pointing port)
      RAOSS=0.
      DERIVE(ISEC,37)=RAOSS
C IAS   - Indicated air speed (m/s)
!      RIAS=0.0
      IF(RSPR.GT.0) RIAS=340.294*RMACH*SQRT(RSPR/1013.25) !Ind air speed (m/s)
      DERIVE(ISEC,4)=RIAS
C PHGT  - Pressure height (m)
      IF(RSPR.GT.0.0) THEN
        RPHGT=44330.77*(1.0-(RSPR/1013.25)**(1.0/5.256)) !Press height (m)
      ELSE
        RPHGT=0.0
      END IF
      DERIVE(ISEC,66)=RPHGT
C PHGTF - Pressure height (Kft)
      RPHGTF=RPHGT * 0.003281
      DERIVE(ISEC,67)=RPHGTF                     !Press height  (Kft)  
C IATDI - De-iced indicated air temperature (deg C)
      CALL MEANPARAM(10,R)
      RIATDI=CAL(10,1)+CAL(10,2)*R+CAL(10,3)*R*R
      IF(.NOT.BTEST(IVAL(27,1),5)) RIATDI=RIATDI-CAL(1,1) !Ind di air tmp (deg C)
      DERIVE(ISEC,7)=RIATDI
C TATDI - True de-iced air temperature (K)
C TATDC - True de-iced air temperature (deg C)
      RTATDI=(RIATDI+273.16)/(1.0+(0.2*RMACH**2*0.956)) !True di air temp (K)
      RTATDC=RTATDI-273.16                       !True di air temp (deg C)
      DERIVE(ISEC,8)=RTATDI
      DERIVE(ISEC,9)=RTATDC
C IATND - Non de-iced indicated air temperature (deg C)
      RIATND=0.0
      CALL MEANPARAM(23,R)
      RIATND=CAL(23,1)+CAL(23,2)*R+CAL(23,3)*R*R
      DERIVE(ISEC,10)=RIATND                     !Non deiced indicated temp (C)
C TATND - True non de-iced air temperature (K)
C TATNC - True non de-iced air temperature (deg C)
      RTATND=(RIATND+273.16)/(1.0+(0.2*RMACH**2*0.985)) !True ndi air temp (K)
      RTATNC=RTATND-273.16                       !True ndi air temp (deg C)
      DERIVE(ISEC,11)=RTATND
      DERIVE(ISEC,12)=RTATNC
C TAS   - True air speed (knots)
      RTAS=0.0
      IF(RSPR.GT.0.AND.RTATDI.GT.0)
     -    RTAS=CAL(4,1)*(RIAS*SQRT((1013.25/RSPR)*(RTATDI/288.15))) !True a/s (m/s)
      DERIVE(ISEC,5)=RTAS*1.944                  !True a/s  (knots)
C POT   - Potential temperature (K)
      RPOT=0.0
      IF(RSPR.GT.0) RPOT=RTATDI*(1000.0/RSPR)**(2.0/7.0) !Potential temp (K)
      DERIVE(ISEC,15)=RPOT
C DAD   - Dry air density (kg m-3)
      RDAD=0.0
      IF(RTATDI.NE.0)RDAD=0.34838*RSPR/RTATDI    !Dry air dens (kg m-3)
      DERIVE(ISEC,16)=RDAD
C DEW   - Dew point (deg C) from General Eastern Hygrometer
      RDEW=0.0
      CALL MEANPARAM(58,R)
      RDEW=CAL(58,2)*R + CAL(58,1)               !Dew point (deg C)
      CALL MEANPARAM(59,RHYCC) 
      SPECIAL(ISEC,5)=0
!      IF(RHYCC.GT.901*16.OR.RHYCC.LT.696*16) SPECIAL(ISEC,5)=1 !C if control lost
      IF(RHYCC.GT.18076.OR.RHYCC.LT.15451) SPECIAL(ISEC,5)=1 !C if control lost
      DERIVE(ISEC,17)=RDEW
C RH    - Relative humidity (%)
      RD=RDEW+273.16
      RT=RTATDI
      IF(RD.GT.RT) THEN
        RRH=100.
      ELSE
        RESTOP=6.112*EXP((17.67*RD)/(243.5+RD))
        RESBOT=6.112*EXP((17.67*RT)/(243.5+RT))
        RRH=0.0
        IF(RESBOT.NE.0) RRH=RESTOP/RESBOT*100.
      END IF
      DERIVE(ISEC,24)=RRH
C VP    - Vapour pressure (mb)
      R=1000.0/(RDEW+273.16)
      RVP=10.0**(8.42926609-(1.82717843+(0.07120871*R))*R) !Vap press (mb)
      DERIVE(ISEC,18)=RVP
C MAD   - Moist air density (kg m-3)
      RMAD=0.0
      IF(RTATDI.NE.0) RMAD=0.34838*(RSPR-0.378*RVP)/RTATDI !Mst a dens (kg m-3)
      DERIVE(ISEC,19)=RMAD
C SHUM  - Specific humidity (g kg-1)
      RSHUM=0.0
      IF(RSPR.NE.0..OR.RVP.NE.0.) THEN
        RSHUM=622.0*RVP/(RSPR-0.378*RVP)         !Spec humidity (g kg-1)
      END IF
      DERIVE(ISEC,20)=RSHUM  
C MMR   - Mass mixing ratio (g kg-1)
      RMMR=0.0
      IF((RSPR-RVP).NE.0.0) THEN
        RMMR=622.0*RVP/(RSPR-RVP)                !Mass mix ratio (g kg-1)
      END IF
      DERIVE(ISEC,21)=RMMR
C HMR   - Humidity mixing ratio (g m-3)
      RHMR=RSHUM*RMAD                            !Hum mix ratio (g kg-1)
      DERIVE(ISEC,22)=RHMR
C POTE  - equivalent potential temperature  (K)
      RL=2.834E6 - 259.5*(RTATDC)
      RPOTE=0.
      IF(RTATDI.GT.0..AND.RMMR.GT.0.)
     -     RPOTE=RPOT*EXP(RL*RMMR/(1000*1005*RTATDI))
      DERIVE(ISEC,13)=RPOTE                      !Equiv pot temp(K)
C JW    - Johnson Williams liquid water (g m-3)
      CALL MEANPARAM(42,R)
      RJW=CAL(42,2)*R+CAL(42,1)                  !Johnson Williams (g m-3)
C LWC   - Corrected J-W liquid water (g m-3)
      RLWC=0.0
      IF(RTAS.NE.0.0) RLWC=RJW*77.2/RTAS         !Corrected JW (g m-3)
      DERIVE(ISEC,23)=RLWC
C CNC parameter
      I1=IBITS(IVAL(50,1),12,4)
      I2=IBITS(IVAL(50,1),8,4)
      I3=IBITS(IVAL(50,1),4,4)
      I4=IBITS(IVAL(50,1),0,4)
      DERIVE(ISEC,33)=0.
      IF(I1.GE.0.AND.I1.LE.9.AND.I2.GE.0.AND.I2.LE.9.AND.
     -    I3.GE.0.AND.I3.LE.9.AND.I4.GE.0.AND.I4.LE.9) 
     -    DERIVE(ISEC,33)=(I1+I2*0.1+I3*0.01)*10.**I4
C TWC   - total water content (g kg-1)
      IF (IV12(74,1).NE.4095) THEN
        TDRS=FLOAT(IV12(72,1))                     
        RTSAMPC=CAL(72,1)+TDRS*CAL(72,2)+TDRS**2*CAL(72,3)
     -       +TDRS**3*CAL(72,4)+TDRS**4*CAL(72,5)
     -       +TDRS**5*CAL(72,6)                  !Sample temp(K)
        RCAL=0.
        IF(RSPR.NE.0.) RCAL=RTSAMPC/(0.34838*RSPR) !Convert g/m3 to g/kg
        CALL MEANPARAM(70,R)                     !TWC detector less 
        IF(CAL(70,2).NE.0.) RHO=(R-CAL(70,1))/CAL(70,2) !offset for window degrade
        RHO2=RHO*RHO                             !calculate oxygen correction
        IF(RTSAMPC.NE.0.) RPT=RSPR/RTSAMPC       !RHO in g m-3
        RPT2=RPT*RPT                             !ROXYCOR in g m-3
        ROXYCOR=- 5.250E-4 + 6.047E-4 * RHO - 2.00E-5 * RHO2
     -          +(6.269E-2 - 3.440E-3 * RHO + 2.38E-4 * RHO2) * RPT
     -          +(5.130E-3 - 2.047E-4 * RHO + 1.76E-5 * RHO2) * RPT2
        RTWC=(RHO+ROXYCOR)*RCAL                  !RTWC in g kg-1
      ELSE 
        RTWC=0.0                                 !Test for fitted and working
      ENDIF
      DERIVE(ISEC,60)=RTWC
C RHGT  - Radar height (m)
      CALL MEANPARAM(37,RV)
      RV=RV*0.25/3.281                           !Radar height (m)
      DERIVE(ISEC,63)=RV
C INU X velocity (m s-1 +ve northish)
      JTEMP(2)=JVAL(163,3)
      JTEMP(1)=JVAL(163,4)
      RVX=ITEMP/2.**18*12*25.4/1000.             !INU X velocity (m s-1)
C INU Y velocity (m s-1 +ve westish)
      JTEMP(2)=JVAL(163,5)
      JTEMP(1)=JVAL(163,6)
      RVY=ITEMP/2.**18*12*25.4/1000.             !INU Y velocity (m s-1)
C VZ    - INU vertical velocity (m s-1 +ve up)
      JTEMP(2)=JVAL(163,7)
      JTEMP(1)=JVAL(163,8)
      RVZ=ITEMP/2.**18*12*25.4/1000.             !INU vertical velocity (m s-1)
      DERIVE(ISEC,45)=RVZ
C ROLL  - INU roll (-180 to +180 deg stbd roll is +ve)
      RROLL=JVAL(163,10)/2.**15*180.             !INU roll (deg)
      DERIVE(ISEC,48)=RROLL
C PTCH  - INU pitch (-90 to +90 nose up is +ve)
      RPTCH=JVAL(163,11)/2.**15*180.             !INU pitch (deg)
      DERIVE(ISEC,49)=RPTCH
C IHDG  - INU azimuth (0 to 360 deg clockwise from above is +ve)
      RIHDG=JVAL(163,12)/2.**15*180.             !INU azimuth (deg)
      IF(RIHDG.LT.0.) RIHDG=RIHDG+360.
      DERIVE(ISEC,50)=RIHDG
C Platform azimuth and wander angle
      RPAZI=JVAL(163,9)/2.**15*180.
      IF(RPAZI.LT.0.) RPAZI=RPAZI+360.
      RWA=RPAZI-RIHDG
C VN    - INU north velocity (m s-1 +ve north)
      RVN=COSD(RWA)*RVX-SIND(RWA)*RVY            !INU north velocity (m s-1)
      DERIVE(ISEC,46)=RVN
C VE    - INU east velocity (m s-1 +ve east)
      RVE=-SIND(RWA)*RVX-COSD(RWA)*RVY           !INU east velocity (m s-1)
      DERIVE(ISEC,47)=RVE
C PITR  - INU pitch rate (deg s-1)
      RPITR=JVAL(163,31)/2.**13*180.             !INU pitch rate (deg s-1)
      DERIVE(ISEC,53)=RPITR
C YAWR  - INU yaw rate (deg s-1)
      RYAWR=JVAL(163,32)/2.**13*180.             !INU yaw rate (deg s-1)
      DERIVE(ISEC,54)=RYAWR 
C IGS   - INU ground speed (m s-1)
      RIGS=SQRT(RVN**2+RVE**2)                   !INU ground speed (m s-1)
      DERIVE(ISEC,51)=RIGS
C IDA   - INU drift angle (deg)
      RIDA=0.0
      IF(RVN.NE.0.OR.RVE.NE.0) THEN
        R=ATAN2D(RVE,RVN)
        IF(R.LT.0) R=R+360.
        RIDA=R-RIHDG                             !INU drift angle (deg)
        IF(RIDA.LT.-180.0) RIDA=RIDA+360.0
      END IF
      DERIVE(ISEC,52)=RIDA
C ILAT  - INU latitude (deg)
      JTEMP(2)=JVAL(163,21)
      JTEMP(1)=JVAL(163,22)
      RCNEXZ=ITEMP/2.**30
      RILAT=0.                                   !INU latitude (deg)
      IF(RCNEXZ.GE.-1.AND.RCNEXZ.LE.1.) RILAT=ASIND(RCNEXZ)
      DERIVE(ISEC,93)=RILAT
C ILNG  - INU longitude (deg)
      JTEMP(2)=JVAL(163,23)
      JTEMP(1)=JVAL(163,24)
      RILNG=ITEMP*180./2.**31                    !INU longitude (deg)
      DERIVE(ISEC,94)=RILNG
C Start wind derivations - note that roll rate corrections are not applied to
C across track wind components.
      RTA=TAND(RAOA)
      RTS=TAND(RAOSS)
      RSR=SIND(RROLL)
      RCR=COSD(RROLL)
      RSP=SIND(RPTCH)
      RCP=COSD(RPTCH)
      RSH=SIND(RIHDG)
      RCH=COSD(RIHDG)
      RV1=RTA*RCR-RTS*RSR
      RV2=RCP+RSP*RV1
      RV3=RTA*RSR+RTS*RCR
      RV4=RCP*RYAWR*3.14159/180.0
      RV5=RSP*RPITR*3.14159/180.0
      RIP=15.49                                  !Vanes to INU distance (m)
C U     - Northwards wind component (m s-1)
      RU=RVN-RTAS*(RSH*RV3+RCH*RV2)-RIP*(RSH*RV4+RCH*RV5) !N wind (m s-1)
      DERIVE(ISEC,55)=RU
C V     - Eastwards wind component (m s-1)
      RV=RVE+RTAS*(RCH*RV3-RSH*RV2)+RIP*(RCH*RV4-RSH*RV5) !E wind (m s-1)
      DERIVE(ISEC,56)=RV
C W     - Vertical wind component (m s-1)
      RW=RVZ+RCP*(RTAS*(RV1-TAND(RPTCH))+RIP*RPITR*3.14159/180.0)!V wind(m s-1)
      DERIVE(ISEC,57)=RW
C IWS   - INU derived wind speed (m s-1)
      RIWS=SQRT(RU**2+RV**2)                     !INU wind speed (m s-1)
      DERIVE(ISEC,58)=RIWS
C IWA   - INU derived wind angle (deg)
      IF(RU.EQ.0.0.AND.RV.EQ.0.0)  THEN
        RIWA=0.0
      ELSE
        RIWA=ATAN2D(RV,RU)+180.0       
      ENDIF
      DERIVE(ISEC,59)=RIWA
C UCLR  - Upper pyranometer (clear) radiance (W m-2)
      RUCLR=(IVAL(81,1)-IVAL(84,1))*CAL(81,2)    !Up pyr clr rad (W m-2)
      DERIVE(ISEC,26)=RUCLR
C URED  - Upper pyranometer (red)   radiance (W m-2)
      RURED=(IVAL(82,1)-IVAL(85,1))*CAL(82,2)    !Up pyr red rad (W m-2)
      DERIVE(ISEC,27)=RURED
C UIR   - Upper pyrgeometer         radiance (W m-2)
      RT=IVAL(89,1)*CAL(89,2)+CAL(89,1)
      RS=(IVAL(83,1)-IVAL(86,1))*CAL(83,2)
      RUIR=5.899E-8*(RT+273.16)**4+RS            !Up prg rad (W m-2)
      DERIVE(ISEC,28)=RUIR
C LCLR  - Lower pyranometer (clear) radiance (W m-2)
      RLCLR=(IVAL(91,1)-IVAL(94,1))*CAL(91,2)    !Lo pyr clr rad (W m-2)
      DERIVE(ISEC,29)=RLCLR
C LRED  - Lower pyranometer (red)   radiance (W m-2)
      RLRED=(IVAL(92,1)-IVAL(95,1))*CAL(92,2)    !Lo pyr red rad (W m-2)
      DERIVE(ISEC,30)=RLRED
C LIR   - Lower pyrgeometer         radiance (W m-2)
      RT=IVAL(99,1)*CAL(99,2)+CAL(99,1)
      RS=(IVAL(93,1)-IVAL(96,1))*CAL(93,2)
      RLIR=5.899E-8*(RT+273.16)**4+RS            !Lo prg rad (W m-2)
      DERIVE(ISEC,31)=RLIR
C SZEN  - solar position, roughly every 120s     !Degrees
      IF(MOD(ISEC,40).EQ.1) CALL ALB_CALC(ISEC,SAZI,SZEN)
      RSZEN=SZEN
      DERIVE(ISEC,40)=RSZEN
      RSAZI=SAZI
      DERIVE(ISEC,41)=RSAZI
C Upper pyranometer corrections
      RCORR=1.0
      IF (RIGS.GT.1000.0) THEN
C BBR corrections only done when INS fitted
        RSHDG=RIHDG-SAZI                         !!!Get quadrants right ?
        R=SIND(SZEN)*SIND(RSHDG)*SIND(RROLL) -
     -  COSD(RSHDG)*SIND(RPTCH)*COSD(RROLL)*SIND(SZEN) +
     -  COSD(SZEN)*COSD(RPTCH)*SIND(RROLL)
        IF(R.NE.0.) RCORR=COSD(SZEN)/R
      END IF
      DERIVE(ISEC,26)=DERIVE(ISEC,26)*RCORR      !Clear
      DERIVE(ISEC,27)=DERIVE(ISEC,27)*RCORR      !Red
C SALB  - solar albedo
      RSALB=0.0
      IF(RUCLR.NE.0.0) RSALB=RLCLR/RUCLR
      DERIVE(ISEC,68)=RSALB                      !Lower clear/upper clear
C NALB  - nIR albedo
      RNALB=0.0
      IF(RURED.NE.0.0) RNALB=RLRED/RURED
      DERIVE(ISEC,69)=RNALB                      !Lower red/upper red
C LVIS  - lower visible 
      RLVIS=RLCLR-RLRED
      DERIVE(ISEC,71)=RLVIS                      !Lower clear-lower red
C UVIS  - upper visible
      RUVIS=RUCLR-RURED
      DERIVE(ISEC,72)=RUVIS                      !Upper clear-upper red
C VALB  - visible albedo
      RVALB=0.0                 
      IF(RUVIS.NE.0.0) RVALB=RLVIS/RUVIS
      DERIVE(ISEC,70)=RVALB                      !Lclear-Lred/Uclear-Ured
C NETIR - net ir
      RNETIR=RLIR-RUIR
      DERIVE(ISEC,73)=RNETIR                     !Lower IR-Upper IR
C UNIRS - upper nir/solar
      RUNIRS=0.0
      IF(RUCLR.NE.0.0) RUNIRS=RURED/RUCLR
      DERIVE(ISEC,74)=RUNIRS                     !Upper red/Upper clear
C LNIRS - lower nir/solar
      RLNIRS=0.0
      IF(RLCLR.NE.0.0) RLNIRS=RLRED/RLCLR
      DERIVE(ISEC,75)=RLNIRS                     !Lower red/Lower clear 
C FLDP  - dewpt from fluorescence water vapour sensor (C)
!      SPECIAL(ISEC,7)=0  
!      IF (BTEST(IVAL(139,1),3)) THEN
!        SPECIAL(ISEC,7)=1
!      ELSE
!        RV=(IVAL(138,1)/20.0) - 100.0            !FWVS dewpoint
!      ENDIF      
!      DERIVE(ISEC,61)=RV        
C FVP   - Fluorescence derived Vapour pressure (mb)
      RV=0.
      RF=1000.0/(RV+273.16)
      RFVP=10.0**(8.42926609-(1.82717843+(0.07120871*RF))*RF) !Vap press (mb)
C FMAD  - FWVS derived moist air density (kg m-3)
      RFMAD=0.0
      IF(RTATDI.NE.0) RFMAD=0.34838*(RSPR-0.378*RFVP)/RTATDI 
C FSHUM - FWVS derived specific humidity (g kg-1)
      RFSHUM=0.0
      IF(RSPR.NE.0..OR.RFVP.NE.0.) THEN
        RFSHUM=622.0*RFVP/(RSPR-0.378*RFVP)      !Spec humidity (g kg-1)
      END IF
C FHMR  - FWVS derived humidity mixing ratio (g m-3)
      RFHMR=RFSHUM*RFMAD                         !FWVS Hum mix ratio (g kg-1)
      DERIVE(ISEC,35)=RFHMR
C TWCDP - Dewpoint from Total Water Content  (deg C)
      RTWCDP=0.0
      IF((RSPR*RTWC).GT.0.0)THEN
        RTWCDP=5.42E3 / LOG(1.57366E12/(RSPR*RTWC)) !(K)
        RTWCDP=RTWCDP-273.16                     !Dewpoint (C) 
      ENDIF
      DERIVE(ISEC,79)=RTWCDP
C 10MWS - 10m NEUTRAL STABILITY WINDS (m s-1)
      RIWS=DERIVE(ISEC,58)                       !INS wind speed (m s-1)
      RHGT=DERIVE(ISEC,66)                       !Pressure height (m)
      R10MWS=0.0
      IF(RIWS.GT.0.AND.RHGT.GT.1.0) THEN
        VK=0.40                                  !Von Karman's constant
        EPS=0.005                                !Required fit
        USTAR=0.3                                !Surface friction velocity m/s
        US=0.0
        N=0                                      !Iteration count
        DO WHILE(ABS(USTAR-US).GT.EPS.AND.N.LT.30)
          US=USTAR
          N=N+1
          Z0=0.3905E-4/USTAR+1.604E-3*USTAR*USTAR-0.017465E-2 !Pierson model
          USTAR=VK*RIWS/ALOG(RHGT/Z0)
        END DO
        IF(RHGT.NE.0..AND.VK.NE.0.) 
     -      R10MWS=RIWS+USTAR*ALOG(10.0/RHGT)/VK
      END IF
      DERIVE(ISEC,98)=R10MWS                     !10m neutral stab wind spd m/s
C Refractive index n / Refractivity (N)
C N=(77.6/RTAtDI)*(RSPR+(4810*RVP/RTATDI))
C N=(n-1)*1E6
      R_TATDI=RTATDI
      IF(R_TATDI.EQ.0.) R_TATDI=1.0 
      REFRACT=(77.6/R_TATDI)*(RSPR+(4810*RVP/R_TATDI))
      IF(REFRACT.EQ.0.)THEN
        REF_INDEX=1
      ELSE
        REF_INDEX=(REFRACT/1000000.0)+1
      END IF         
      RREFR=REF_INDEX
      DERIVE(ISEC,14)=RREFR
      RREFRM=REFRACT
      DERIVE(ISEC,77)=RREFRM
C LCLVL - Lifting condensation level (in metres)
      RLCLVL=RPHGT+((RTATDI-(RDEW+273.16))*125.0)
      DERIVE(ISEC,34)=RLCLVL
C Theta W , from a 3rd order least squares fit with theta E
      DERIVE(ISEC,143)=-917.7114+RPOTE*10.119819-
     &    RPOTE*RPOTE*2.89312109e-02+RPOTE*RPOTE*RPOTE*2.83998353e-5
C CABP  - Cabin pressure (mb)
      R=FLOAT(IVAL(14,1))
      RV=CAL(14,3)*R**2+CAL(14,2)*R+CAL(14,1)    !Cabin pressure (mb)
      DERIVE(ISEC,107)=RV
C CABT  - Cabin temperature (C)
      R=FLOAT(IVAL(207,1))
      RV=CAL(207,1)+CAL(207,2)*R                 !Cabin temperature (C)
      DERIVE(ISEC,39)=RV
C HEIM  - Heimann surface temperature (deg C)
      CALL MEANPARAM(141,R)
      RHEIM=CAL(141,1)+CAL(141,2)*R              !Heimann value
      SPECIAL(ISEC,6)=0
      IF(BTEST(IVAL(27,1),0)) SPECIAL(ISEC,6)=1  !calibrate
      DERIVE(ISEC,25)=RHEIM
C ST    - Corrected Surface Temperature   (deg C)
      INDEX=NINT((RHEIM+22.0)*10.0 +3.0)         !Index into lookup table
      RST=99.9                                   !Flagged 
      IF(STATUS(13).NE.0) THEN
         IF(INDEX.GE.3.AND.INDEX.LE.640) THEN
            RCORR=RTABLE(INDEX,STATUS(13))       !Current lookup table
            IF(RCORR.LT.1000.0.AND.SPECIAL(ISEC,6).EQ.0) THEN !not in cal/ref
                 RST=RHEIM + RCORR               !Add valid correction
            ENDIF
         ENDIF
      ENDIF
      DERIVE(ISEC,78)=RST                        !Corr. surface temp (C)
C Nevzorov
      RNVL=0.0
      RNVT=0.0
      CALL MEANPARAM(208,RL)
      RL=CAL(208,1)+CAL(208,2)*RL
      IF(RTAS.GT.0..AND.CAL(208,3).NE.0.) 
     -    RNVL=RL**2/RTAS/2589/CAL(208,3)
      CALL MEANPARAM(211,RT)
      RT=CAL(211,1)+CAL(211,2)*RT
      IF(RTAS.GT.0..AND.CAL(211,3).NE.0.) 
     -     RNVT=RT**2/RTAS/2589/CAL(211,3)
      DERIVE(ISEC,148)=RNVL                      !Nevzorov liquid water g/m3
      DERIVE(ISEC,149)=RNVT                      !Nevzorov total water g/m3
C Nephelometer parameters
      DERIVE(ISEC,108)=CAL(175,1)+CAL(175,2)*IVAL(175,1) !Neph pressure
      DERIVE(ISEC,109)=CAL(176,1)+CAL(176,2)*IVAL(176,1) !Neph temperature
      RV=CAL(177,1)+CAL(177,2)*IVAL(177,1)
      DERIVE(ISEC,110)=10**((RV/CAL(177,4))-CAL(177,3))-CAL(177,5) !Blue sp
      RV=CAL(178,1)+CAL(178,2)*IVAL(178,1)
      DERIVE(ISEC,111)=10**((RV/CAL(178,4))-CAL(178,3))-CAL(178,5) !Green sp
      RV=CAL(179,1)+CAL(179,2)*IVAL(179,1)
      DERIVE(ISEC,112)=10**((RV/CAL(179,4))-CAL(179,3))-CAL(179,5) !Red sp
      RV=CAL(180,1)+CAL(180,2)*IVAL(180,1)
      DERIVE(ISEC,113)=10**((RV/CAL(180,4))-CAL(180,3))-CAL(180,5) !Blue bsp
      RV=CAL(182,1)+CAL(182,2)*IVAL(182,1)
      DERIVE(ISEC,114)=10**((RV/CAL(182,4))-CAL(182,3))-CAL(182,5) !Green bsp
      RV=CAL(181,1)+CAL(181,2)*IVAL(181,1)
      DERIVE(ISEC,115)=10**((RV/CAL(181,4))-CAL(181,3))-CAL(181,5) !Red bsp
      DERIVE(ISEC,110)=DERIVE(ISEC,110)*1E6 !Scale by 10**6 in order to plot
      DERIVE(ISEC,111)=DERIVE(ISEC,111)*1E6
      DERIVE(ISEC,112)=DERIVE(ISEC,112)*1E6
      DERIVE(ISEC,113)=DERIVE(ISEC,113)*1E6
      DERIVE(ISEC,114)=DERIVE(ISEC,114)*1E6
      DERIVE(ISEC,115)=DERIVE(ISEC,115)*1E6
      DERIVE(ISEC,116)=CAL(183,1)+CAL(183,2)*IVAL(183,1) !Neph humidity   
      DERIVE(ISEC,117)=CAL(184,1)+CAL(184,2)*IVAL(184,1) !Neph status     
C PSAP parameters
      DERIVE(ISEC,42)=CAL(185,1)+CAL(185,2)*IVAL(185,1) !Lin abs coeff
      DERIVE(ISEC,43)=CAL(186,1)+CAL(186,2)*IVAL(186,1) !Log abs coeff
      DERIVE(ISEC,44)=CAL(187,1)+CAL(187,2)*IVAL(187,1) !Filter transmittance
C Teco 49 Ozone
      R1=CAL(100,1)+CAL(100,2)*IVAL(100,1)       !Ozone signal
      R2=CAL(106,1)+CAL(106,2)*IVAL(106,1)       !Ozone pressure
      R3=CAL(113,1)+CAL(113,2)*IVAL(113,1)       !Ozone temperature
      IF(R2.NE.0.) ROZMR=R1*(1013.0/R2)*(R3/273.16)
      DERIVE(ISEC,62)=ROZMR                      !Ozone mixing ratio ppb  
C TECO NOx
      DERIVE(ISEC,145)=CAL(203,1)+CAL(203,2)*IVAL(203,1) !TECO NO ppb
      DERIVE(ISEC,146)=CAL(204,1)+CAL(204,2)*IVAL(204,1) !TECO NO2 ppb
      DERIVE(ISEC,147)=CAL(205,1)+CAL(205,2)*IVAL(205,1) !TECO NOx ppb
C TECO SO2
      DERIVE(ISEC,99)=CAL(214,1)+CAL(214,2)*IVAL(214,1) !TECO SO2 ppb
C CO mixing ratio
      DERIVE(ISEC,76)=CAL(154,1)+CAL(154,2)*IVAL(154,1) !CO m/r
C NOXY parameters
      DERIVE(ISEC,118)=((CAL(199,1)+CAL(199,2)*IVAL(199,1))*0.4)-0.05 !NOXY NO
      DERIVE(ISEC,119)=(CAL(200,1)+CAL(200,2)*IVAL(200,1))*1.0 !NOXY NO2
      DERIVE(ISEC,120)=(CAL(201,1)+CAL(201,2)*IVAL(201,1))*2.0 !NOXY NOY1
      DERIVE(ISEC,121)=(CAL(202,1)+CAL(202,2)*IVAL(202,1))*2.0 !NOXY NOY2
C HCHO  - Formaldehyde mixing ratio
      DERIVE(ISEC,80)=CAL(150,1)+CAL(150,2)*IVAL(150,1) !HCHO m/r
C H2O2  - Peroxide mixing ratio
      DERIVE(ISEC,101)=CAL(152,1)+CAL(152,2)*IVAL(152,1) !H2O2 m/r
C ORGP  - Organic peroxide mixing ratio
      DERIVE(ISEC,102)=CAL(151,1)+CAL(151,2)*IVAL(151,1) !Org H2O2 m/r
C JNO2 - derived from BBR parameters
!      DERIVE(ISEC,100)=(IVAL(93,1)*43.9+IVAL(83,1)*44.38)/4095.
C JO1D - derived from BBR parameters
!      DERIVE(ISEC,144)=(IVAL(81,1)+IVAL(91,1))/409.5
C  Modelled peroxide parameters
!  Modelling code commented out until needed again
!      CALL LIB$GETJPI(JPI$_CPUTIM,,,ISRTMOD,,)
!      CALL H2O2MODEL(RTATDI,RVP,ROZMR,DERIVE(ISEC,95),RPHGT,RSPR,
!     &    DERIVE(ISEC,118),RMMR,DERIVE(ISEC,76),DERIVE(ISEC,144),
!     &    ICPU,PERMOD)
!      CALL LIB$GETJPI(JPI$_CPUTIM,,,IENDMOD,,)
!      IF(IENDMOD-ISRTMOD.GT.2) THEN     !No more than 20ms
!        ICPU=MAX(0,ICPU-1)
!        TYPE *,'H2O2 model cpu value reduced to ',ICPU
!      END IF
!      DO I=1,21
!        DERIVE(ISEC,121+I)=PERMOD(I)
!      ENDDO
C
      RETURN
      END
C*******************************************************************************
      SUBROUTINE MEANPARAM(IP,RESULT)
C
C Calculates the mean value of a DRS parameter over a period of 1s.
C The result is returned as a real positive number.  This subroutine should 
C only be used for parameters which are recorded as unsigned 16 bit binary 
C numbers, ie not for BCD
C
C V2.00  19/06/02  W.D.N.JACKSON  Handles 16 bit unsigned recording
C
      IMPLICIT  NONE
      INTEGER*2 NFDATA(2048,2),NPLOC(512),NPFREQ(512),STATUS(256)
      INTEGER*2 SPECIAL(12800,10)
      INTEGER*4 IP,JSUM,JCNT,I,J
      REAL*4    DERIVE(12800,196),RESULT
      COMMON    /HCOM/ NFDATA,NPLOC,NPFREQ,STATUS,DERIVE,SPECIAL
      VOLATILE  /HCOM/
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
C                                     to 128 at present)
C
      RESULT=0.0
      IF(NPLOC(IP).NE.0) THEN          !If parameter recorded
        JSUM=0
        JCNT=0
        DO I=NPLOC(IP),NPLOC(IP)+NPFREQ(IP)-1
          J=JZEXT(NFDATA(I,STATUS(1)))
          JSUM=JSUM+J
          JCNT=JCNT+1
        END DO
        RESULT=FLOAT(JSUM)/JCNT        !Work out mean
      END IF
      RETURN
      END
!*******************************************************************************
      SUBROUTINE READ_CONSTS
!
! Reads calibration constants from the file HOR_CALIB.DAT.  Takes all constants
! specified in the file and puts them in the appropriate parameter slot in the
! CAL array.  Outputs messages for invalid lines but keeps going.  This routine
! is called by H_DERIVE.
!
! V2.00  06/08/02  W.D.N.JACKSON
!
      IMPLICIT  NONE
      INTEGER*4 IOS,IOS1,IPAR,J,ICNT
      CHARACTER CLINE*80
      REAL*4    CAL(512,6)
      COMMON    /CALS/ CAL

      DO IPAR=1,512
        DO ICNT=1,6
          CAL(IPAR,ICNT)=0.
        END DO
      END DO
!
      OPEN(UNIT=10,FILE='HOR_CALIB.DAT',STATUS='OLD',READONLY,
     -    IOSTAT=IOS)
      IF(IOS.NE.0) THEN
        CALL LOG_MESS('Failed to open HOR_CALIB.DAT')
        CALL HM_MESS('Unable to open HOR_CALIB calibration file')
        CALL EXIT
      END IF
      READ(10,'(A)',IOSTAT=IOS) CLINE
      DO WHILE(IOS.EQ.0)
        IF(CLINE(1:3).EQ.'CAL') THEN
          IPAR=0
          READ(CLINE(4:6),'(I3)',IOSTAT=IOS1) IPAR
          IF(IPAR.GE.1.AND.IPAR.LE.512) THEN
            READ(CLINE(8:8),'(I1)',IOSTAT=IOS1) ICNT
            IF(ICNT.GE.1.AND.ICNT.LE.6) THEN
              READ(CLINE,'(8X,<ICNT>G)',IOSTAT=IOS1) 
     &            (CAL(IPAR,J),J=1,ICNT)
              IF(IOS1.NE.0) THEN
                TYPE *,'Invalid line in HOR_CALIB.DAT'
                TYPE *,CLINE
              END IF
            ELSE
              TYPE *,'Invalid line in HOR_CALIB.DAT'
              TYPE *,CLINE
            END IF
          END IF
        END IF
        READ(10,'(A)',IOSTAT=IOS) CLINE
      END DO
      CLOSE(UNIT=10,IOSTAT=IOS)
      RETURN
      END
C*******************************************************************************
      SUBROUTINE ALB_CALC(ISEC,AZ,ZEN)
C   
C   ROUTINE TO CALCULATE THE SOLAR ZENITH/AZIMUTH ANGLES GIVEN
C   A DATE/TIME AND LAT/LONG. THE PROGRAM USES THE ROUTINE INSOL
C   WHICH WAS DEVELOPED AS A QUICK METHOD OF CALCULATING SOLAR
C   POSITION. A MORE ACCURATE VERSION IS THE MRF SUNPOS ROUTINE.
C   THE SOLAR ANGLES COMPUTED ARE OUTPUT INTO THE SAFPAR ARRAY.
C
      IMPLICIT  NONE
      CHARACTER CDATE*9,CMON(12)*3
      INTEGER*4 ISEC,IHR,IMIN,IDAY,N,IMON,INOLEAP(12),IDAYNO
      REAL*4    AZ,ZEN,ALAT,ALON
      INCLUDE   'HCOM_DEF.FOR'
      DATA INOLEAP/0,31,59,90,120,151,181,212,243,273,304,334/
      DATA CMON/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     1          'SEP','OCT','NOV','DEC'/
C
      ALAT = RGLAT(ISEC)                      ! A/C latitude
      ALON = RGLNG(ISEC)                      ! A/C longitude
      IHR = JHRS(ISEC)                        ! Hours
      IMIN= JMIN(ISEC)                        ! minutes
      CALL DATE1(CDATE)
      READ(CDATE(1:2),'(I2)')IDAY             ! Day in month (1-31)    
      N = 0
      DO IMON = 1 , 12
        N = N + 1
        IF ( CMON(IMON) .EQ. CDATE(4:6))GO TO 10
      ENDDO
      GO TO 99
 10   IMON = N                                 ! Month in year (1-12)    
      IDAYNO = INOLEAP(IMON) + IDAY
C
      CALL INSOL(ALAT,ALON,IHR,IMIN,IDAYNO,ZEN,AZ)
C
 99   RETURN
      END
C*******************************************************************************
      SUBROUTINE INSOL(LAT,LON,HRS,MINS,DAYNO,ZENITH,AZIMUT)
C
C PROGRAM TO CALCULATE THE TOTAL AVAILABLE SOLAR ENERGY AT ANY
C AT ANY POINT ABOVE THE ATMOSPHERE AND THE SOLAR ZENITH AND
C AZIMUTH  ANGLES.
C
      IMPLICIT  NONE
      REAL*4    LAT,LON,AMINS,TIME,DAYFRA,DIFF(90),ZENITH,AZIMUT,PI,
     1    DAY1,DECL,AK,C,RDIFF,TIME1,HRANG,ADEC,ALAT,HALOC,AHA,COSZ,AZ
      INTEGER*4 DAYNO,DAY(90),HRS,MINS,I
C
C SET UP LOOK UP TABLE FOR EQUATION OF TIME (FROM NORTON'S STAR ATLAS)
C
      DATA DIFF/-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-14.33,-14,-13.
     15,-13,-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,3.5,3.75,3.5,
     23,2,1,0,-1,-2,-3,-4,-5,-5.5,-6,-6.33,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5
     3,6,7,8,9,10,11,12,13,14,15,16,16.33,16,15,14,13,12,11,10,9,8,7,6,5
     4,4,3,2,1,0,-1,-2/
      DATA DAY/1,3,5,7,9,12,15,18,21,24,29,35,43,51,55,58,63,67,71,75,79
     1,82,86,88,92,95,98,102,106,110,115,122,127,135,143,148,155,161,166
     2,170,175,180,185,191,195,200,208,217,225,230,234,238,241,245,248,2
     351,254,257,260,262,265,268,271,274,277,280,284,288,293,300,308,315
     4,322,326,330,333,335,338,341,343,345,347,350,352,354,356,358,360,3
     562,364/
C
C SOLAR CONSTANT FROM LEE ET AL 6TH CONF ON ATMOS RAD WILLIAMSBURG 1986
C
      DATA PI/3.14159/
C
C WORK OUT SOLAR DECLINATION FOR GIVEN TIME
C
      DAYFRA=FLOAT(HRS)/24.
      DAY1=FLOAT(DAYNO)+DAYFRA
      DECL=23.44*COS((DAY1-172)*PI/186.0)
      AMINS=FLOAT(MINS)/60.
      TIME=FLOAT(HRS)+AMINS
C
C WORK OUT SOLAR HOUR ANGLE
C
      DO 20 I=1,90
      IF(DAY(I).GE.DAYNO)GO TO 30
 20   CONTINUE
C
C FIND EQUATION OF TIME CORRECTION
C
 30   AK=(DIFF(I)-DIFF(I-1))/FLOAT(DAY(I)-DAY(I-1))
      C=DIFF(I)-AK*FLOAT(DAY(I))
      RDIFF=AK*FLOAT(DAYNO)+C
      TIME1=TIME+RDIFF/60
      HRANG=(TIME1*360/24)+180.
      IF(HRANG.GE.360)HRANG=HRANG-360.
C
C CALCULATE SOLAR ZENITH ANGLE
C
      ADEC=DECL*PI/180.
      ALAT=LAT*PI/180.
      HALOC=HRANG+LON
      IF(HALOC.GT.360)HALOC=HALOC-360.
      IF(HALOC.LT.0.)HALOC=HALOC+360.
      AHA=HALOC*PI/180.
      COSZ=SIN(ADEC)*SIN(ALAT)+COS(ADEC)*COS(ALAT)*COS(AHA)
      ZENITH=ACOS(COSZ)*180./PI
C
C CALCULATE SOLAR AZIMUTH ANGLE
C
      AZ=(COS(ADEC)*SIN(AHA))/(COS(ADEC)*COS(AHA)*SIN(ALAT)
     + - SIN(ADEC)*COS(ALAT))
      AZIMUT=ATAN(AZ)*180./PI
      IF((AHA.LT.PI).AND.(AZ.LT.0.0))AZIMUT=180.+AZIMUT
      IF((AHA.GT.PI).AND.(AZ.GT.0.0))AZIMUT=AZIMUT-180.
      AZIMUT=180.+AZIMUT
C
      RETURN
      END
C
C*******************************************************************************
C
C Peroxide subroutine supplied by C.Reeves@uea.ac.uk, 02 July 1999 15:42
C
C Note that this routine uses HORACE date, and not DRS data - OK for flying
C but not when simulating.
C Print statements commented out.
C Doesn't currently compute OHINST from JO1D, although commented out lines
C have been altered to compile if required.
C Following repeated crashing of H_DERIVE now returns immediately if detects 
C upcoming arithmetic errors.
C No longer outputs underflow messages
C
      OPTIONS /CHECK=(ALL,NOUNDERFLOW)
      SUBROUTINE H2O2MODEL(RTATDI,RVP,ROZMR,ROLAT,ALTM,PMBAR,RNOMRAV,
     +                     RMMR,RCOMR,RJO1D,ICPU,PERMOD)

      IMPLICIT NONE

      REAL*4 OLDOH,OLDHO2,OLDCH3O2,OLDH2O2
      REAL*4 OLDCH3OOH,OLDHCHO,OLDNO2,OLDCH3OH
      REAL*4 OH,HO2,CH3O2,H2O2,CH3OOH
      REAL*4 HCHO,CH3OH,CH4,NO,NO2,O1D
      REAL*4 ROZMR,RCOMR,ALTM,ALTCM,ROLAT,LAT,RNOMRAV
      REAL*4 RJO1D
      REAL*4 PMBAR,RTATDI,PBLM,TOL,M,O3,CO
      REAL*4 RVP,RMMR,RHOAIR,Q,H2O
      REAL*4 K2,K3,J4,K5,J6,VD7,K7,K8,K10,K13,K14,K15,J16
      REAL*4 VD17,K17,K18,K19,K20,K21,K22,K23,K24,J25,J26
      REAL*4 K0,KINF,K0M,K27A,K27B,K27C,K27,K28,J29,K30
      REAL*4 AA2,BB2,CC2,ARG2,DX,D
      REAL*4 HO2MR,CH3O2MR,H2O2MR,CH3OOHMR
      REAL*4 HCHOMR,NO2MR
      REAL*4 PERMOD(21)
      REAL*4 J4X(24,12),J6X(24,12),J16X(24,12),J25X(24,12)
      REAL*4 J26X(24,12),J29X(24,12),LATS(24)
      real*4 rdummy
 
      INTEGER*2 J,MONTH,I,IPBLM,NNOX,NRUN,NRUN10,NPBLM,NNNOX
      INTEGER*2 ICPU

      CHARACTER*9  CDATE
      CHARACTER*3  MON(12)

C Look up tables of J values

      DATA ((J4X(I,J),I=1,24),J=1,3)/
     +7.11E-06,9.19E-06,1.14E-05,1.40E-05,1.67E-05,1.91E-05,
     +2.08E-05,2.13E-05,2.10E-05,2.01E-05,1.90E-05,1.79E-05,
     +1.63E-05,1.43E-05,1.22E-05,9.59E-06,7.04E-06,4.80E-06,
     +2.86E-06,1.37E-06,4.85E-07,1.06E-07,5.95E-09,1.00E-14,
     +3.09E-06,6.75E-06,9.04E-06,1.18E-05,1.46E-05,1.75E-05,
     +1.94E-05,2.05E-05,2.07E-05,2.02E-05,1.96E-05,1.89E-05,
     +1.78E-05,1.63E-05,1.42E-05,1.16E-05,9.03E-06,6.57E-06,
     +4.29E-06,2.44E-06,1.06E-06,3.57E-07,6.89E-08,1.00E-14,
     +7.87E-07,3.31E-06,5.58E-06,8.66E-06,1.20E-05,1.54E-05,
     +1.82E-05,2.02E-05,2.10E-05,2.13E-05,2.11E-05,2.07E-05,
     +2.04E-05,1.93E-05,1.75E-05,1.50E-05,1.21E-05,9.29E-06,
     +6.77E-06,4.42E-06,2.32E-06,1.09E-06,4.73E-07,6.13E-09/
      DATA ((J4X(I,J),I=1,24),J=4,6)/
     +1.54E-08,7.97E-07,2.19E-06,4.48E-06,7.36E-06,1.04E-05,
     +1.34E-05,1.60E-05,1.79E-05,1.90E-05,1.99E-05,2.05E-05,
     +2.08E-05,2.09E-05,2.00E-05,1.77E-05,1.52E-05,1.24E-05,
     +9.89E-06,7.21E-06,4.62E-06,2.72E-06,1.81E-06,3.16E-07,
     +1.00E-14,8.44E-08,5.63E-07,1.64E-06,3.47E-06,5.88E-06,
     +8.68E-06,1.14E-05,1.37E-05,1.58E-05,1.75E-05,1.87E-05,
     +1.96E-05,2.02E-05,2.00E-05,1.89E-05,1.71E-05,1.51E-05,
     +1.27E-05,1.02E-05,7.40E-06,4.80E-06,3.42E-06,1.56E-06,
     +1.00E-14,6.50E-09,1.55E-07,7.03E-07,1.78E-06,3.56E-06,
     +5.84E-06,8.29E-06,1.08E-05,1.33E-05,1.52E-05,1.69E-05,
     +1.80E-05,1.87E-05,1.91E-05,1.89E-05,1.81E-05,1.70E-05,
     +1.51E-05,1.28E-05,9.97E-06,7.05E-06,5.25E-06,4.56E-06/
      DATA ((J4X(I,J),I=1,24),J=7,9)/
     +1.00E-14,6.10E-09,1.46E-07,6.52E-07,1.64E-06,3.29E-06,
     +5.45E-06,7.81E-06,1.03E-05,1.26E-05,1.46E-05,1.64E-05,
     +1.76E-05,1.85E-05,1.93E-05,1.97E-05,1.94E-05,1.85E-05,
     +1.66E-05,1.42E-05,1.15E-05,8.59E-06,6.34E-06,4.73E-06,
     +1.02E-14,7.45E-08,4.71E-07,1.33E-06,2.83E-06,4.88E-06,
     +7.34E-06,9.82E-06,1.21E-05,1.44E-05,1.63E-05,1.78E-05,
     +1.88E-05,1.94E-05,1.98E-05,2.02E-05,1.96E-05,1.84E-05,
     +1.66E-05,1.39E-05,1.09E-05,7.88E-06,5.63E-06,2.42E-06,
     +1.39E-08,6.60E-07,1.72E-06,3.42E-06,5.85E-06,8.72E-06,
     +1.13E-05,1.36E-05,1.55E-05,1.69E-05,1.82E-05,1.92E-05,
     +1.97E-05,1.98E-05,1.98E-05,1.91E-05,1.80E-05,1.64E-05,
     +1.41E-05,1.12E-05,8.20E-06,5.28E-06,2.92E-06,6.16E-07/
      DATA ((J4X(I,J),I=1,24),J=10,12)/
     +6.38E-07,2.67E-06,4.48E-06,7.37E-06,1.06E-05,1.40E-05,
     +1.60E-05,1.73E-05,1.81E-05,1.90E-05,1.95E-05,1.96E-05,
     +1.95E-05,1.88E-05,1.78E-05,1.66E-05,1.48E-05,1.25E-05,
     +1.00E-05,7.17E-06,4.25E-06,1.94E-06,6.68E-07,1.26E-08,
     +2.87E-06,6.39E-06,8.63E-06,1.20E-05,1.53E-05,1.81E-05,
     +1.94E-05,2.03E-05,2.06E-05,2.02E-05,1.96E-05,1.88E-05,
     +1.78E-05,1.65E-05,1.50E-05,1.30E-05,1.06E-05,8.17E-06,
     +5.76E-06,3.56E-06,1.62E-06,4.70E-07,6.09E-08,1.00E-14,
     +7.70E-06,9.54E-06,1.13E-05,1.50E-05,1.81E-05,2.00E-05,
     +2.16E-05,2.24E-05,2.25E-05,2.16E-05,2.00E-05,1.83E-05,
     +1.67E-05,1.49E-05,1.29E-05,1.04E-05,7.76E-06,5.47E-06,
     +3.45E-06,1.78E-06,6.65E-07,1.21E-07,4.45E-09,1.00E-14/

      DATA ((J6X(I,J),I=1,24),J=1,3)/
     +5.19E-06,4.10E-06,4.15E-06,4.33E-06,4.51E-06,4.61E-06,
     +4.63E-06,4.61E-06,4.48E-06,4.35E-06,4.19E-06,4.00E-06,
     +3.79E-06,3.53E-06,3.21E-06,2.87E-06,2.49E-06,2.07E-06,
     +1.63E-06,1.18E-06,7.30E-07,3.32E-07,6.14E-08,1.00E-14,
     +3.41E-06,3.30E-06,3.56E-06,3.84E-06,4.10E-06,4.27E-06,
     +4.38E-06,4.41E-06,4.38E-06,4.34E-06,4.23E-06,4.10E-06,
     +3.94E-06,3.73E-06,3.48E-06,3.17E-06,2.83E-06,2.45E-06,
     +2.04E-06,1.60E-06,1.13E-06,6.68E-07,2.94E-07,1.06E-14,
     +1.25E-06,2.11E-06,2.59E-06,3.04E-06,3.44E-06,3.73E-06,
     +3.98E-06,4.14E-06,4.23E-06,4.23E-06,4.23E-06,4.20E-06,
     +4.12E-06,4.02E-06,3.87E-06,3.62E-06,3.35E-06,3.04E-06,
     +2.67E-06,2.27E-06,1.81E-06,1.35E-06,1.04E-06,8.84E-08/
      DATA ((J6X(I,J),I=1,24),J=4,6)/
     +9.67E-08,9.26E-07,1.51E-06,2.04E-06,2.51E-06,2.92E-06,
     +3.27E-06,3.53E-06,3.73E-06,3.90E-06,4.00E-06,4.07E-06,
     +4.15E-06,4.16E-06,4.11E-06,4.00E-06,3.81E-06,3.57E-06,
     +3.30E-06,3.01E-06,2.62E-06,2.22E-06,2.28E-06,1.08E-06,
     +1.00E-14,2.40E-07,6.94E-07,1.18E-06,1.66E-06,2.11E-06,
     +2.52E-06,2.87E-06,3.18E-06,3.44E-06,3.66E-06,3.84E-06,
     +3.99E-06,4.11E-06,4.17E-06,4.13E-06,4.07E-06,3.96E-06,
     +3.80E-06,3.62E-06,3.33E-06,3.02E-06,2.84E-06,2.97E-06,
     +1.00E-14,4.71E-08,3.35E-07,7.41E-07,1.18E-06,1.62E-06,
     +2.05E-06,2.45E-06,2.79E-06,3.11E-06,3.40E-06,3.63E-06,
     +3.82E-06,3.97E-06,4.09E-06,4.16E-06,4.21E-06,4.19E-06,
     +4.13E-06,4.01E-06,3.80E-06,3.58E-06,3.47E-06,4.66E-06/
      DATA ((J6X(I,J),I=1,24),J=7,9)/
     +1.00E-14,4.66E-08,3.31E-07,7.31E-07,1.16E-06,1.59E-06,
     +2.02E-06,2.41E-06,2.76E-06,3.07E-06,3.36E-06,3.60E-06,
     +3.80E-06,3.96E-06,4.10E-06,4.20E-06,4.29E-06,4.28E-06,
     +4.24E-06,4.12E-06,3.92E-06,3.73E-06,3.61E-06,4.17E-06,
     +1.15E-14,2.36E-07,6.74E-07,1.14E-06,1.59E-06,2.02E-06,
     +2.42E-06,2.77E-06,3.08E-06,3.35E-06,3.59E-06,3.78E-06,
     +3.94E-06,4.06E-06,4.17E-06,4.20E-06,4.22E-06,4.18E-06,
     +4.07E-06,3.91E-06,3.64E-06,3.35E-06,3.07E-06,2.89E-06,
     +9.57E-08,8.98E-07,1.44E-06,1.93E-06,2.38E-06,2.78E-06,
     +3.12E-06,3.38E-06,3.59E-06,3.77E-06,3.90E-06,4.00E-06,
     +4.09E-06,4.11E-06,4.11E-06,4.08E-06,3.99E-06,3.84E-06,
     +3.63E-06,3.36E-06,2.98E-06,2.55E-06,2.03E-06,1.08E-06/
      DATA ((J6X(I,J),I=1,24),J=10,12)/
     +1.21E-06,2.03E-06,2.48E-06,2.92E-06,3.32E-06,3.61E-06,
     +3.82E-06,3.96E-06,4.06E-06,4.10E-06,4.14E-06,4.13E-06,
     +4.07E-06,4.00E-06,3.90E-06,3.73E-06,3.53E-06,3.28E-06,
     +2.95E-06,2.54E-06,2.05E-06,1.50E-06,9.13E-07,8.54E-08,
     +3.38E-06,3.27E-06,3.52E-06,3.83E-06,4.11E-06,4.27E-06,
     +4.34E-06,4.38E-06,4.36E-06,4.32E-06,4.22E-06,4.09E-06,
     +3.93E-06,3.75E-06,3.53E-06,3.26E-06,2.95E-06,2.59E-06,
     +2.19E-06,1.73E-06,1.22E-06,7.04E-07,2.37E-07,1.00E-14,
     +5.26E-06,4.12E-06,4.13E-06,4.37E-06,4.56E-06,4.63E-06,
     +4.64E-06,4.63E-06,4.53E-06,4.41E-06,4.23E-06,4.02E-06,
     +3.81E-06,3.56E-06,3.26E-06,2.92E-06,2.55E-06,2.13E-06,
     +1.70E-06,1.24E-06,7.73E-07,3.41E-07,4.99E-08,1.00E-14/

      DATA ((J16X(I,J),I=1,24),J=1,3)/
     +5.13E-06,4.01E-06,4.00E-06,4.10E-06,4.20E-06,4.24E-06,
     +4.23E-06,4.18E-06,4.06E-06,3.93E-06,3.78E-06,3.61E-06,
     +3.42E-06,3.18E-06,2.90E-06,2.60E-06,2.27E-06,1.90E-06,
     +1.52E-06,1.11E-06,6.94E-07,3.17E-07,5.98E-08,1.00E-14,
     +3.34E-06,3.22E-06,3.42E-06,3.64E-06,3.82E-06,3.93E-06,
     +4.00E-06,4.01E-06,3.97E-06,3.92E-06,3.82E-06,3.70E-06,
     +3.55E-06,3.37E-06,3.15E-06,2.88E-06,2.58E-06,2.26E-06,
     +1.91E-06,1.51E-06,1.08E-06,6.44E-07,2.89E-07,1.06E-14,
     +1.21E-06,2.05E-06,2.49E-06,2.87E-06,3.19E-06,3.43E-06,
     +3.62E-06,3.76E-06,3.82E-06,3.82E-06,3.81E-06,3.79E-06,
     +3.72E-06,3.63E-06,3.50E-06,3.29E-06,3.06E-06,2.81E-06,
     +2.50E-06,2.15E-06,1.74E-06,1.31E-06,1.03E-06,8.96E-08/
      DATA ((J16X(I,J),I=1,24),J=4,6)/
     +9.36E-08,8.94E-07,1.44E-06,1.92E-06,2.33E-06,2.68E-06,
     +2.98E-06,3.20E-06,3.37E-06,3.52E-06,3.60E-06,3.67E-06,
     +3.74E-06,3.75E-06,3.71E-06,3.63E-06,3.48E-06,3.30E-06,
     +3.09E-06,2.85E-06,2.52E-06,2.17E-06,2.27E-06,1.11E-06,
     +1.00E-14,2.29E-07,6.59E-07,1.11E-06,1.54E-06,1.94E-06,
     +2.30E-06,2.61E-06,2.87E-06,3.10E-06,3.30E-06,3.46E-06,
     +3.59E-06,3.70E-06,3.76E-06,3.74E-06,3.71E-06,3.64E-06,
     +3.54E-06,3.41E-06,3.18E-06,2.92E-06,2.81E-06,3.02E-06,
     +1.00E-14,4.45E-08,3.15E-07,6.95E-07,1.10E-06,1.50E-06,
     +1.88E-06,2.23E-06,2.53E-06,2.81E-06,3.06E-06,3.27E-06,
     +3.44E-06,3.57E-06,3.68E-06,3.76E-06,3.82E-06,3.83E-06,
     +3.82E-06,3.75E-06,3.61E-06,3.43E-06,3.37E-06,4.63E-06/
      DATA ((J16X(I,J),I=1,24),J=7,9)/
     +1.00E-14,4.43E-08,3.12E-07,6.88E-07,1.08E-06,1.48E-06,
     +1.85E-06,2.20E-06,2.51E-06,2.78E-06,3.04E-06,3.25E-06,
     +3.42E-06,3.56E-06,3.69E-06,3.79E-06,3.88E-06,3.89E-06,
     +3.89E-06,3.82E-06,3.69E-06,3.55E-06,3.46E-06,4.05E-06,
     +1.15E-14,2.26E-07,6.44E-07,1.08E-06,1.50E-06,1.89E-06,
     +2.24E-06,2.54E-06,2.81E-06,3.04E-06,3.25E-06,3.42E-06,
     +3.56E-06,3.67E-06,3.76E-06,3.79E-06,3.81E-06,3.78E-06,
     +3.71E-06,3.60E-06,3.40E-06,3.16E-06,2.91E-06,2.76E-06,
     +9.36E-08,8.77E-07,1.39E-06,1.84E-06,2.25E-06,2.61E-06,
     +2.90E-06,3.11E-06,3.28E-06,3.43E-06,3.54E-06,3.62E-06,
     +3.69E-06,3.71E-06,3.70E-06,3.68E-06,3.60E-06,3.47E-06,
     +3.29E-06,3.07E-06,2.76E-06,2.39E-06,1.92E-06,1.03E-06/
      DATA ((J16X(I,J),I=1,24),J=10,12)/
     +1.19E-06,1.99E-06,2.41E-06,2.80E-06,3.14E-06,3.38E-06,
     +3.54E-06,3.64E-06,3.71E-06,3.73E-06,3.75E-06,3.74E-06,
     +3.68E-06,3.61E-06,3.51E-06,3.35E-06,3.18E-06,2.96E-06,
     +2.67E-06,2.33E-06,1.90E-06,1.41E-06,8.62E-07,8.10E-08,
     +3.33E-06,3.21E-06,3.41E-06,3.66E-06,3.88E-06,3.98E-06,
     +4.02E-06,4.02E-06,3.97E-06,3.92E-06,3.82E-06,3.70E-06,
     +3.55E-06,3.38E-06,3.18E-06,2.94E-06,2.66E-06,2.35E-06,
     +2.00E-06,1.60E-06,1.14E-06,6.61E-07,2.23E-07,1.00E-14,
     +5.22E-06,4.05E-06,4.00E-06,4.17E-06,4.29E-06,4.30E-06,
     +4.28E-06,4.24E-06,4.13E-06,4.00E-06,3.82E-06,3.63E-06,
     +3.44E-06,3.21E-06,2.93E-06,2.63E-06,2.30E-06,1.94E-06,
     +1.56E-06,1.15E-06,7.24E-07,3.20E-07,4.75E-08,1.00E-14/

      DATA ((J25X(I,J),I=1,24),J=1,3)/
     +1.85E-05,1.53E-05,1.57E-05,1.64E-05,1.70E-05,1.74E-05,
     +1.75E-05,1.74E-05,1.69E-05,1.63E-05,1.57E-05,1.50E-05,
     +1.41E-05,1.31E-05,1.18E-05,1.05E-05,8.98E-06,7.37E-06,
     +5.69E-06,3.95E-06,2.30E-06,9.50E-07,1.54E-07,1.00E-14,
     +1.15E-05,1.22E-05,1.33E-05,1.45E-05,1.54E-05,1.61E-05,
     +1.65E-05,1.66E-05,1.65E-05,1.63E-05,1.59E-05,1.54E-05,
     +1.47E-05,1.39E-05,1.29E-05,1.17E-05,1.04E-05,8.91E-06,
     +7.30E-06,5.57E-06,3.75E-06,2.08E-06,8.35E-07,1.14E-14,
     +3.97E-06,7.52E-06,9.49E-06,1.13E-05,1.29E-05,1.40E-05,
     +1.50E-05,1.56E-05,1.59E-05,1.60E-05,1.59E-05,1.58E-05,
     +1.55E-05,1.51E-05,1.45E-05,1.35E-05,1.24E-05,1.12E-05,
     +9.80E-06,8.19E-06,6.31E-06,4.50E-06,3.28E-06,2.22E-07/
      DATA ((J25X(I,J),I=1,24),J=4,6)/
     +2.57E-07,3.05E-06,5.25E-06,7.36E-06,9.23E-06,1.08E-05,
     +1.22E-05,1.33E-05,1.40E-05,1.47E-05,1.51E-05,1.53E-05,
     +1.56E-05,1.57E-05,1.55E-05,1.50E-05,1.43E-05,1.34E-05,
     +1.23E-05,1.11E-05,9.49E-06,7.82E-06,7.76E-06,3.31E-06,
     +1.00E-14,6.95E-07,2.23E-06,4.03E-06,5.88E-06,7.65E-06,
     +9.26E-06,1.06E-05,1.18E-05,1.29E-05,1.37E-05,1.44E-05,
     +1.50E-05,1.54E-05,1.57E-05,1.56E-05,1.53E-05,1.49E-05,
     +1.43E-05,1.35E-05,1.23E-05,1.09E-05,1.01E-05,9.74E-06,
     +1.00E-14,1.20E-07,9.92E-07,2.41E-06,4.02E-06,5.74E-06,
     +7.41E-06,8.95E-06,1.03E-05,1.16E-05,1.27E-05,1.36E-05,
     +1.43E-05,1.49E-05,1.53E-05,1.56E-05,1.58E-05,1.58E-05,
     +1.55E-05,1.51E-05,1.42E-05,1.31E-05,1.25E-05,1.61E-05/
      DATA ((J25X(I,J),I=1,24),J=7,9)/
     +1.00E-14,1.19E-07,9.78E-07,2.36E-06,3.95E-06,5.63E-06,
     +7.28E-06,8.81E-06,1.02E-05,1.14E-05,1.25E-05,1.35E-05,
     +1.42E-05,1.48E-05,1.54E-05,1.58E-05,1.61E-05,1.61E-05,
     +1.59E-05,1.54E-05,1.47E-05,1.38E-05,1.30E-05,1.44E-05,
     +1.40E-14,6.77E-07,2.15E-06,3.84E-06,5.61E-06,7.32E-06,
     +8.88E-06,1.03E-05,1.15E-05,1.25E-05,1.35E-05,1.42E-05,
     +1.48E-05,1.53E-05,1.57E-05,1.58E-05,1.59E-05,1.57E-05,
     +1.53E-05,1.46E-05,1.35E-05,1.23E-05,1.10E-05,9.40E-06,
     +2.55E-07,2.94E-06,4.98E-06,6.91E-06,8.73E-06,1.04E-05,
     +1.17E-05,1.27E-05,1.35E-05,1.42E-05,1.47E-05,1.51E-05,
     +1.54E-05,1.55E-05,1.54E-05,1.53E-05,1.49E-05,1.43E-05,
     +1.35E-05,1.24E-05,1.09E-05,9.14E-06,7.03E-06,3.32E-06/
      DATA ((J25X(I,J),I=1,24),J=10,12)/
     +3.83E-06,7.19E-06,9.02E-06,1.09E-05,1.25E-05,1.37E-05,
     +1.45E-05,1.50E-05,1.53E-05,1.55E-05,1.56E-05,1.56E-05,
     +1.53E-05,1.50E-05,1.46E-05,1.39E-05,1.31E-05,1.21E-05,
     +1.08E-05,9.21E-06,7.28E-06,5.10E-06,2.90E-06,2.19E-07,
     +1.14E-05,1.21E-05,1.32E-05,1.45E-05,1.57E-05,1.63E-05,
     +1.66E-05,1.66E-05,1.65E-05,1.63E-05,1.59E-05,1.54E-05,
     +1.47E-05,1.40E-05,1.31E-05,1.20E-05,1.08E-05,9.40E-06,
     +7.85E-06,6.09E-06,4.14E-06,2.20E-06,6.48E-07,1.00E-14,
     +1.90E-05,1.55E-05,1.56E-05,1.67E-05,1.74E-05,1.77E-05,
     +1.77E-05,1.76E-05,1.72E-05,1.67E-05,1.59E-05,1.51E-05,
     +1.42E-05,1.32E-05,1.20E-05,1.07E-05,9.20E-06,7.61E-06,
     +5.95E-06,4.21E-06,2.48E-06,9.74E-07,1.19E-07,1.00E-14/

      DATA ((J26X(I,J),I=1,24),J=1,3)/
     +4.25E-05,3.12E-05,3.04E-05,3.07E-05,3.09E-05,3.08E-05,
     +3.04E-05,2.99E-05,2.90E-05,2.82E-05,2.72E-05,2.61E-05,
     +2.49E-05,2.35E-05,2.17E-05,1.98E-05,1.77E-05,1.53E-05,
     +1.26E-05,9.60E-06,6.32E-06,3.05E-06,6.11E-07,1.00E-14,
     +2.91E-05,2.55E-05,2.64E-05,2.74E-05,2.84E-05,2.86E-05,
     +2.88E-05,2.87E-05,2.83E-05,2.81E-05,2.74E-05,2.66E-05,
     +2.56E-05,2.45E-05,2.32E-05,2.16E-05,1.98E-05,1.77E-05,
     +1.54E-05,1.26E-05,9.46E-06,5.93E-06,2.79E-06,1.58E-14,
     +1.10E-05,1.68E-05,1.97E-05,2.20E-05,2.39E-05,2.50E-05,
     +2.60E-05,2.66E-05,2.70E-05,2.70E-05,2.70E-05,2.68E-05,
     +2.63E-05,2.59E-05,2.53E-05,2.41E-05,2.29E-05,2.15E-05,
     +1.96E-05,1.74E-05,1.47E-05,1.15E-05,9.46E-06,9.06E-07/
      DATA ((J26X(I,J),I=1,24),J=4,6)/
     +9.36E-07,7.87E-06,1.20E-05,1.53E-05,1.80E-05,2.01E-05,
     +2.19E-05,2.31E-05,2.40E-05,2.50E-05,2.55E-05,2.59E-05,
     +2.64E-05,2.65E-05,2.64E-05,2.62E-05,2.55E-05,2.47E-05,
     +2.36E-05,2.23E-05,2.04E-05,1.82E-05,1.98E-05,1.03E-05,
     +1.00E-14,2.19E-06,5.89E-06,9.44E-06,1.25E-05,1.52E-05,
     +1.74E-05,1.93E-05,2.09E-05,2.23E-05,2.36E-05,2.46E-05,
     +2.55E-05,2.63E-05,2.68E-05,2.69E-05,2.70E-05,2.69E-05,
     +2.65E-05,2.61E-05,2.51E-05,2.39E-05,2.35E-05,2.69E-05,
     +1.00E-14,4.54E-07,2.97E-06,6.16E-06,9.28E-06,1.21E-05,
     +1.46E-05,1.69E-05,1.88E-05,2.05E-05,2.22E-05,2.35E-05,
     +2.46E-05,2.56E-05,2.64E-05,2.71E-05,2.78E-05,2.80E-05,
     +2.83E-05,2.83E-05,2.78E-05,2.75E-05,2.77E-05,3.96E-05/
      DATA ((J26X(I,J),I=1,24),J=7,9)/
     +1.00E-14,4.52E-07,2.95E-06,6.13E-06,9.22E-06,1.20E-05,
     +1.45E-05,1.68E-05,1.87E-05,2.04E-05,2.21E-05,2.34E-05,
     +2.46E-05,2.56E-05,2.65E-05,2.72E-05,2.79E-05,2.82E-05,
     +2.86E-05,2.86E-05,2.82E-05,2.79E-05,2.82E-05,3.46E-05,
     +2.48E-14,2.17E-06,5.82E-06,9.28E-06,1.23E-05,1.49E-05,
     +1.71E-05,1.90E-05,2.07E-05,2.22E-05,2.34E-05,2.45E-05,
     +2.54E-05,2.62E-05,2.68E-05,2.70E-05,2.73E-05,2.74E-05,
     +2.72E-05,2.69E-05,2.59E-05,2.49E-05,2.37E-05,2.46E-05,
     +9.35E-07,7.78E-06,1.18E-05,1.50E-05,1.76E-05,1.97E-05,
     +2.15E-05,2.27E-05,2.37E-05,2.47E-05,2.53E-05,2.57E-05,
     +2.63E-05,2.64E-05,2.64E-05,2.64E-05,2.60E-05,2.53E-05,
     +2.44E-05,2.33E-05,2.15E-05,1.93E-05,1.62E-05,9.51E-06/
      DATA ((J26X(I,J),I=1,24),J=10,12)/
     +1.08E-05,1.66E-05,1.94E-05,2.17E-05,2.36E-05,2.47E-05,
     +2.56E-05,2.62E-05,2.67E-05,2.67E-05,2.68E-05,2.67E-05,
     +2.62E-05,2.58E-05,2.53E-05,2.43E-05,2.33E-05,2.21E-05,
     +2.03E-05,1.82E-05,1.54E-05,1.21E-05,7.82E-06,8.24E-07,
     +2.90E-05,2.54E-05,2.63E-05,2.74E-05,2.84E-05,2.86E-05,
     +2.87E-05,2.86E-05,2.83E-05,2.80E-05,2.74E-05,2.66E-05,
     +2.56E-05,2.45E-05,2.33E-05,2.18E-05,2.01E-05,1.81E-05,
     +1.58E-05,1.30E-05,9.80E-06,6.06E-06,2.20E-06,1.00E-14,
     +4.27E-05,3.13E-05,3.04E-05,3.08E-05,3.11E-05,3.08E-05,
     +3.04E-05,3.00E-05,2.91E-05,2.83E-05,2.73E-05,2.62E-05,
     +2.50E-05,2.36E-05,2.18E-05,1.99E-05,1.79E-05,1.54E-05,
     +1.28E-05,9.82E-06,6.48E-06,3.09E-06,4.93E-07,1.00E-14/

      DATA ((J29X(I,J),I=1,24),J=1,3)/
     +9.09E-03,6.07E-03,5.63E-03,5.49E-03,5.41E-03,5.29E-03,
     +5.14E-03,5.05E-03,4.87E-03,4.75E-03,4.61E-03,4.45E-03,
     +4.28E-03,4.09E-03,3.82E-03,3.58E-03,3.31E-03,2.97E-03,
     +2.58E-03,2.13E-03,1.56E-03,8.81E-04,1.92E-04,1.00E-14,
     +6.81E-03,5.05E-03,4.97E-03,4.97E-03,5.01E-03,4.93E-03,
     +4.89E-03,4.83E-03,4.75E-03,4.72E-03,4.61E-03,4.49E-03,
     +4.35E-03,4.19E-03,4.02E-03,3.81E-03,3.58E-03,3.32E-03,
     +3.02E-03,2.63E-03,2.15E-03,1.51E-03,8.31E-04,1.87E-12,
     +2.83E-03,3.55E-03,3.87E-03,4.10E-03,4.28E-03,4.35E-03,
     +4.43E-03,4.50E-03,4.53E-03,4.50E-03,4.50E-03,4.49E-03,
     +4.40E-03,4.35E-03,4.29E-03,4.14E-03,4.02E-03,3.88E-03,
     +3.65E-03,3.38E-03,3.04E-03,2.59E-03,2.38E-03,2.91E-04/
      DATA ((J29X(I,J),I=1,24),J=4,6)/
     +2.80E-04,1.91E-03,2.59E-03,3.04E-03,3.38E-03,3.63E-03,
     +3.85E-03,3.98E-03,4.08E-03,4.22E-03,4.28E-03,4.32E-03,
     +4.41E-03,4.42E-03,4.42E-03,4.44E-03,4.39E-03,4.32E-03,
     +4.22E-03,4.14E-03,3.95E-03,3.73E-03,4.41E-03,2.83E-03,
     +1.00E-14,6.47E-04,1.46E-03,2.08E-03,2.54E-03,2.91E-03,
     +3.20E-03,3.44E-03,3.65E-03,3.84E-03,4.00E-03,4.15E-03,
     +4.28E-03,4.40E-03,4.50E-03,4.52E-03,4.58E-03,4.62E-03,
     +4.65E-03,4.70E-03,4.65E-03,4.65E-03,4.84E-03,6.61E-03,
     +1.00E-14,1.43E-04,8.32E-04,1.48E-03,2.02E-03,2.44E-03,
     +2.80E-03,3.12E-03,3.37E-03,3.59E-03,3.83E-03,4.01E-03,
     +4.17E-03,4.32E-03,4.45E-03,4.55E-03,4.70E-03,4.78E-03,
     +4.90E-03,5.00E-03,5.05E-03,5.18E-03,5.50E-03,8.68E-03/
      DATA ((J29X(I,J),I=1,24),J=7,9)/
     +1.00E-14,1.43E-04,8.31E-04,1.48E-03,2.02E-03,2.44E-03,
     +2.80E-03,3.12E-03,3.37E-03,3.59E-03,3.83E-03,4.01E-03,
     +4.17E-03,4.31E-03,4.45E-03,4.55E-03,4.71E-03,4.78E-03,
     +4.90E-03,5.00E-03,5.05E-03,5.19E-03,5.50E-03,7.47E-03,
     +4.45E-12,6.47E-04,1.46E-03,2.08E-03,2.54E-03,2.91E-03,
     +3.19E-03,3.43E-03,3.64E-03,3.83E-03,4.00E-03,4.15E-03,
     +4.28E-03,4.40E-03,4.50E-03,4.52E-03,4.58E-03,4.63E-03,
     +4.66E-03,4.71E-03,4.66E-03,4.67E-03,4.67E-03,5.78E-03,
     +2.81E-04,1.91E-03,2.59E-03,3.04E-03,3.37E-03,3.63E-03,
     +3.84E-03,3.97E-03,4.08E-03,4.22E-03,4.27E-03,4.32E-03,
     +4.41E-03,4.42E-03,4.41E-03,4.44E-03,4.39E-03,4.32E-03,
     +4.23E-03,4.15E-03,3.96E-03,3.75E-03,3.39E-03,2.49E-03/
      DATA ((J29X(I,J),I=1,24),J=10,12)/
     +2.83E-03,3.55E-03,3.87E-03,4.10E-03,4.28E-03,4.35E-03,
     +4.43E-03,4.49E-03,4.53E-03,4.49E-03,4.50E-03,4.48E-03,
     +4.40E-03,4.35E-03,4.29E-03,4.14E-03,4.03E-03,3.88E-03,
     +3.66E-03,3.39E-03,3.05E-03,2.60E-03,1.90E-03,2.54E-04,
     +6.81E-03,5.05E-03,4.98E-03,4.98E-03,5.02E-03,4.94E-03,
     +4.89E-03,4.83E-03,4.75E-03,4.73E-03,4.61E-03,4.49E-03,
     +4.35E-03,4.19E-03,4.02E-03,3.81E-03,3.59E-03,3.33E-03,
     +3.03E-03,2.63E-03,2.15E-03,1.51E-03,6.67E-04,1.00E-14,
     +9.11E-03,6.08E-03,5.63E-03,5.50E-03,5.42E-03,5.30E-03,
     +5.15E-03,5.05E-03,4.88E-03,4.76E-03,4.61E-03,4.45E-03,
     +4.28E-03,4.09E-03,3.83E-03,3.58E-03,3.31E-03,2.97E-03,
     +2.58E-03,2.13E-03,1.56E-03,8.81E-04,1.61E-04,1.00E-14/

      DATA LATS/-78,-60.5,-52,-45,-38.5,-32.5,-27,-22,-17,-12,-7,-2,
     +2,7,12,17,22,27,32.5,38.5,45,52,60.5,78/
      DATA MON/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG'
     +,'SEP','OCT','NOV','DEC'/

      RDUMMY=RJO1D                    !Stop the compiler complained about unused
C
C ICPU is an integer used to reduce the amount of CPU required
C for the subroutine to run by omitting certain parts of the 
C calculations.
C
C NNNOX - with or without NOX
C NPBLM - Different PBL heights
C TOL - Tolerance set for accuracy of solution for iterations
C

      IF (ICPU.EQ.10) THEN
       NNNOX=2
       NPBLM=5
       TOL=1.0E-3
      ENDIF
      IF (ICPU.EQ.9) THEN
       NNNOX=2
       NPBLM=5
       TOL=1.0E-2
      ENDIF
      IF (ICPU.EQ.8) THEN
       NNNOX=2
       NPBLM=3
       TOL=1.0E-2
      ENDIF
      IF (ICPU.EQ.7) THEN
       NNNOX=2
       NPBLM=1
       TOL=1.0E-2
      ENDIF
      IF (ICPU.EQ.6) THEN
       NNNOX=2
       NPBLM=1
       TOL=5.0E-2
      ENDIF
      IF (ICPU.EQ.5) THEN
       NNNOX=1
       NPBLM=1
       TOL=5.0E-2
      ENDIF
      IF (ICPU.EQ.4) THEN
       NNNOX=1
       NPBLM=1
       TOL=6.0E-2
      ENDIF
      IF (ICPU.EQ.3) THEN
       NNNOX=1
       NPBLM=1
       TOL=7.0E-2
      ENDIF
      IF (ICPU.EQ.2) THEN
       NNNOX=1
       NPBLM=1
       TOL=8.0E-2
      ENDIF
      IF (ICPU.EQ.1) THEN
       NNNOX=1
       NPBLM=1
       TOL=1.0E-1
      ENDIF
      IF (ICPU.EQ.0) THEN
       NNNOX=0
       NPBLM=0
       TOL=1.0E-1
      ENDIF

C The values to be output for plotting are initialised to zero.
C The program is set to calculate 10 values of H2O2 and CH3OOH.
C 2 scenarios are for NOX: 1) no NO; 2) using measured NO
C 5 scenarios are for different BL heights: 0, 500, 1000, 1500, 2000m
C The program also calculates 1 value for OH.

      DO I=1,21
       PERMOD(I)=0.0
      ENDDO

C Look up J values for correct date and latitude

      CALL DATE1(CDATE)
      MONTH=1
      DO WHILE (MON(MONTH).NE.CDATE(4:6))
       MONTH=MONTH+1
       IF (MONTH.EQ.13) RETURN
      ENDDO
      LAT=ROLAT
      IF (LAT.GT.78) LAT=78
      IF (LAT.LT.-78) LAT=-78
      I=1
      DO WHILE ((LATS(I).GT.LAT).OR.(LATS(I+1).LT.LAT))
       I=I+1
       IF (I.EQ.24) RETURN
      ENDDO
      DX=(LATS(I+1)-LATS(I))
      IF(DX.EQ.0.) RETURN
      D=(LAT-LATS(I))/DX
      J4=J4X(I,MONTH)+D*(J4X(I+1,MONTH)-J4X(I,MONTH))
      J6=J6X(I,MONTH)+D*(J6X(I+1,MONTH)-J6X(I,MONTH))
      J16=J16X(I,MONTH)+D*(J16X(I+1,MONTH)-J16X(I,MONTH))
      J25=J25X(I,MONTH)+D*(J25X(I+1,MONTH)-J25X(I,MONTH))
      J26=J26X(I,MONTH)+D*(J26X(I+1,MONTH)-J26X(I,MONTH))
      J29=J29X(I,MONTH)+D*(J29X(I+1,MONTH)-J29X(I,MONTH))

C Below I set J values to check the model.

C      J4=1.2E-5
C      J6=3.6E-6
C      J16=3.3E-6
C      J25=1.3E-5
C      J26=2.5E-5
C      J29=4.3E-3

C Air density (molecules/cm3)
      IF(RTATDI.EQ.0.) RETURN
      M=(PMBAR*6.02205E23*0.1)/(8314.0*RTATDI)

C convert mixing ratios (ppb) to molecular densities
      O3=ROZMR*1E-9*M
      CO=RCOMR*1E-9*M

C Calculate water vapour in molecules/cm3 from
C RVP and RMMR
C moist air density (kg/m3)
      RHOAIR=100*(PMBAR-0.3774*RVP)/(287*RTATDI)
C VMR (g/m3)
      Q=RMMR*RHOAIR
C water vapour (molecules/cm3)
      H2O=Q*6.02205E23*1E-6/18.016

C temp or pressure dependent rate constants
      IF ((RTATDI.LT.100).OR.(RTATDI.GT.400)) RETURN
      K2=1.5E-13*(1+0.6*(PMBAR/1000))
      K3=2.2E-10
      K5=2.1E-11*EXP(100/RTATDI)
      VD7=1.0
      K8=2.9E-12*EXP(-160/RTATDI)
      K10=(2.3E-13*EXP(600/RTATDI)+1.7E-33*M*EXP(1000/RTATDI))*
     +    (1+1.4E-21*H2O*EXP(2200/RTATDI))
      K13=2.45E-12*EXP(-1775/RTATDI)
      K14=2.5E-13*EXP(190/RTATDI)
      K15=3.8E-13*EXP(800/RTATDI)
      VD17=0.5
      K18=3.8E-12*EXP(200/RTATDI)
      K19=1.6E-12*EXP(-940/RTATDI)
      K20=1.1E-14*EXP(-500/RTATDI)
      K21=3.5E-12*EXP(250/RTATDI)
      K22=3.0E-12*EXP(280/RTATDI)
      K23=4.8E-11*EXP(250/RTATDI)
      K24=1.0E-11
      K0=2.5E-30*(RTATDI/300)**(-4.4)
      KINF=1.6E-11*(RTATDI/300)**(-1.7)
      K0M=K0*M
      IF(KINF.EQ.0) RETURN
      IF((1+K0*M/KINF).EQ.0.) RETURN
      K27A=(K0*M/(1+K0*M/KINF))
      IF(M/KINF.LE.0.) RETURN
      K27B=(ALOG10(K0*M/KINF))**2
      K27C=(1+K27B)**(-1)
      K27=(K27A*0.6**K27C)
      K28=2.0E-12*EXP(-1400/RTATDI)
      K30=6.7E-12*EXP(-600/RTATDI)

C also calculate peroxide for with or without NOx

      DO NNOX=1,NNNOX
       IF(NNOX.EQ.1)THEN
        NO=0.0
       ELSE
C NO converted to molecules/cm3 from ppb
        NO=RNOMRAV*1E-9*M
       ENDIF

C calculate peroxide for 5 different bl heights

       DO IPBLM=1,NPBLM
        PBLM=FLOAT(IPBLM-1)*500.0
        ALTCM=100.0*ALTM
        IF(ALTCM.EQ.0.) RETURN
        IF((ALTM.LT.PBLM).AND.(PBLM.NE.0))THEN
         IF (ALTCM.LE.1.0) THEN
          K7=VD7
          K17=VD17
         ELSE
          K7=VD7/(ALTCM)
          K17=VD17/(ALTCM)
         ENDIF 
        ELSE
         K7=0.0
         K17=0.0
        ENDIF         

C initialise for integration
        OH=1E5
        HO2=10.0*1E-12*M
        CH3O2=10.0*1E-12*M
        H2O2=1.0*1E-9*M
        CH3OOH=0.5*1E-9*M
        HCHO=100.0*1E-12*M
        CH3OH=10.0*1E-12*M
        CH4=1800.0*1E-9*M
        NO2=NO*4

C calculate O1D
        IF((M*K5+H2O*K3).EQ.0.) RETURN
        O1D=(O3*J4)/(M*K5+H2O*K3)

C start interation
        J=0

C check interation
   10   IF (J.GE.50) THEN
         GO TO 99
        ENDIF
        J=J+1
C reset with old values for test
        OLDOH=OH
        OLDHO2=HO2
        OLDCH3O2=CH3O2
        OLDH2O2=H2O2
        OLDCH3OOH=CH3OOH
        OLDHCHO=HCHO
        OLDNO2=NO2
        OLDCH3OH=CH3OH

C Calculate OH, HO2, CH3O2, H2O2, CH3OOH, HCHO,
C NO2, CH3OH using iterative method

C solve for OH
        IF((CO*K2+H2O2*K8+O3*K19+CH4*K13+HO2*K23+
     +      CH3OOH*K18*0.7+HCHO*K24+NO2*K27+CH3OH*K30).EQ.0) RETURN
        OH=(2*O1D*H2O*K3+2*H2O2*J6+O3*HO2*K20+
     +      NO*HO2*K21+CH3OOH*J16)/
     +     (CO*K2+H2O2*K8+O3*K19+CH4*K13+HO2*K23+
     +      CH3OOH*K18*0.7+HCHO*K24+NO2*K27+CH3OH*K30)

C solve for HO2
C self-reaction rate
        AA2=2*K10
C other losses
        BB2=O3*K20+CH3O2*K15+NO*K21+OH*K23
C production
        CC2=-CO*OH*K2-H2O2*OH*K8-O3*OH*K19-
     +       CH3O2*CH3O2*K14*0.6-CH3OOH*J16-
     +       CH3O2*NO*K22-HCHO*OH*K24-HCHO*J25*2-OH*CH3OH*K30
C solve
        ARG2=BB2*BB2 - 4.0*AA2*CC2
        IF(ARG2.LT.0.) RETURN
        IF(AA2.EQ.0) RETURN
        HO2=(-BB2+SQRT(ARG2))/(2.0*AA2)

C solve for CH3O2
C self-reaction rate
        AA2=2*K14
C other losses
        BB2=HO2*K15+NO*K22
C production
        CC2=-CH4*OH*K13-0.7*CH3OOH*OH*K18
C solve
        ARG2=BB2*BB2 - 4.0*AA2*CC2
        IF(ARG2.LT.0.) RETURN
        IF(AA2.EQ.0) RETURN
        CH3O2=(-BB2+SQRT(ARG2))/(2.0*AA2)

C solve for H2O2
        IF((J6+K7+OH*K8).EQ.0.) RETURN
        H2O2=(HO2*HO2*K10)/(J6+K7+OH*K8)

C solve for CH3OOH
        IF((J16+K17+OH*K18).EQ.0.) RETURN
        CH3OOH=(CH3O2*HO2*K15*0.6)/(J16+K17+OH*K18)

C solve for HCHO
        IF((OH*K24+J25+J26).EQ.0.) RETURN
        HCHO=(CH3O2*CH3O2*K14*1.3+CH3O2*NO*K22+
     +        CH3OOH*OH*K18*0.3+CH3OOH*J16+
     +        CH3O2*HO2*K15*0.4+CH3OH*OH*K30)/
     +       (OH*K24+J25+J26)

C solve for NO2
        IF((J29+OH*K27).EQ.0) RETURN
        NO2=(O3*NO*K28+HO2*NO*K21+CH3O2*NO*K22)/(J29+OH*K27)

C solve for CH3OH
        IF((OH*K30).EQ.0.) RETURN
        CH3OH=(CH3O2*CH3O2*K14*0.7)/(OH*K30)

C Test solution
        IF ((ABS(OH-OLDOH).GT.ABS(OH*TOL)).OR.
     +      (ABS(HO2-OLDHO2).GT.ABS(HO2*TOL)).OR.
     +      (ABS(CH3O2-OLDCH3O2).GT.ABS(CH3O2*TOL)).OR.
     +      (ABS(H2O2-OLDH2O2).GT.ABS(H2O2*TOL)).OR.
     +      (ABS(CH3OOH-OLDCH3OOH).GT.ABS(CH3OOH*TOL)).OR.
     +      (ABS(HCHO-OLDHCHO).GT.ABS(HCHO*TOL)).OR.
     +      (ABS(NO2-OLDNO2).GT.ABS(NO2*TOL)).OR.
     +      (ABS(CH3OH-OLDCH3OH).GT.ABS(CH3OH*TOL))) THEN
         GO TO 10
        ENDIF

C Convert H2O2 and CH3OOH to mixing ratio (ppb) for output
   99   IF(M.EQ.0.) RETURN
        H2O2MR=H2O2/M*1E9
        NRUN=IPBLM+5*(NNOX-1)
        NRUN10=NRUN+10
        PERMOD(NRUN)=H2O2MR
        CH3OOHMR=CH3OOH/M*1E9
        PERMOD(NRUN10)=CH3OOHMR

C Check results
        HO2MR=HO2/M*1E12
        HCHOMR=HCHO/M*1E12
        CH3O2MR=CH3O2/M*1E12
        NO2MR=NO2/M*1E12
C        PRINT *, "OH, HO2, CH3O2, H2O2, CH3OOH, HCHO, NO2"
C        PRINT *, OH, HO2MR, CH3O2MR, H2O2MR, CH3OOHMR,
C     +   HCHOMR, NO2MR
C        PRINT *, "NRUN, MODH2O2, NRUN10, MODORGP"
C        PRINT *, NRUN, PERMOD(NRUN), NRUN10, PERMOD(NRUN10)

C ENDDO for different bl heights
       ENDDO

C ENDDO  for NOX
      ENDDO

C instantaneous OH in 1e6 molecules/cm3
C MODIFICATION MAY BE REQUIRED HERE !
C JO1D is the measured value of JO1D.
C It needs to be in /second.
C I think the value of JO1D displayed on HORACE is multiplied
C by 1E5.  It may therefore need to be *1E-5
C      RJO1D=1E-5
C      IF(K5*M.EQ.0) RETURN
C      IF((CO*K2+CH4*K13).EQ.0) RETURN
C      PERMOD(21)=(RJO1D*O3*H2O*K3/(K5*M)/
C     +                (CO*K2+CH4*K13))*2*1E-6
      RETURN
      END
