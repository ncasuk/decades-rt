
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
     -    RREFRM,RLCLVL,RNVL,RNVT,RTASD,RTASW,RTPSP,CONOW,COTOTAL,
     -    CO(30),ROLDV,ROLDU,RPLANV,RVV
      INTEGER*4 I_CO,I_CON,I
      DATA I_CO/1/
      DATA I_CON/1/
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
C SPR   - Static pressure and altitude from RVSM system
      CALL MEANPARAMNOFS(222,R)
      RPHGT=R*4.*12.*25.4/1000.        !Altitude (m)
      DERIVE(ISEC,66)=RPHGT
      RPHGTF=RPHGT * 3.281
      DERIVE(ISEC,67)=RPHGTF/1000.0    !Press height (Kft)  
      CALL ALT2PRESS(RPHGTF,RSPR)      !Static pressure (mb)
      DERIVE(ISEC,64)=RSPR
C PSP   - Pitot static pressure difference from RVSM system (mb)
      CALL MEANPARAMNOFS(223,R)
      RIAS=R/32.*0.514444              !Computed airspeed (m s-1)
      DERIVE(ISEC,4)=RIAS
      CALL S_PSP(RIAS,RSPR,RPSP)
      DERIVE(ISEC,65)=RPSP
C MACH  - Mach No
      RMACH=0.0
      IF(RPSP.GT.0.AND.RSPR.GT.0) 
     -    R=5.0*((1.0+RPSP/RSPR)**(2.0/7.0)-1.0)
      IF(R.GT.0) RMACH=SQRT(R)         !Mach No
      DERIVE(ISEC,6)=RMACH
C S9SP  - S9 static pressure (mb)
      CALL MEANPARAM(221,R)
      R=CAL(221,1)+R*CAL(221,2)+R*R*CAL(221,3)
      DERIVE(ISEC,86)=R                !S9 static pressure (mb)
C Turbulence probe pressures (mb)
      CALL MEANPARAM(215,R)
      R=CAL(215,1)+R*CAL(215,2)+R*R*CAL(215,3)
      DERIVE(ISEC,81)=R                !TBP0 (mb)
      CALL MEANPARAM(216,R)
      R=CAL(216,1)+R*CAL(216,2)+R*R*CAL(216,3)
      DERIVE(ISEC,82)=R                !TBPA (mb)
      CALL MEANPARAM(217,R)
      R=CAL(217,1)+R*CAL(217,2)+R*R*CAL(217,3)
      DERIVE(ISEC,83)=R                !TBPB (mb)
      CALL MEANPARAM(218,R)
      R=CAL(218,1)+R*CAL(218,2)+R*R*CAL(218,3)
      DERIVE(ISEC,84)=R                !TBPD (mb)
      CALL MEANPARAM(219,R)
      R=CAL(219,1)+R*CAL(219,2)+R*R*CAL(219,3)
      DERIVE(ISEC,85)=R                !TBPD (mb)
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
C TAS   - True RVSM airspeed (knots)
      RTAS=0.0
      IF(RSPR.GT.0.AND.RTATDI.GT.0)
     -    RTAS=CAL(4,1)*(RIAS*SQRT((1013.25/RSPR)*(RTATDI/288.15))) !True a/s (m/s)
      DERIVE(ISEC,5)=RTAS*1.944                  !True a/s  (knots)
C Attack, sideslip, turb probe airspeeds and pitot-static
C AOA   - Angle of attack (deg)  (+ve vane pointing down)
C AOSS  - Angle of sideslip (deg)  (+ve vane pointing port)
C TASD  - Turbulence probe dry airspeed (ms-1)
C TASW  - Turbulence probe wet airspeed (ms-1)
C TPSP  - Turbulence probe pitot-static pressure (mb)
      CALL TURB(ISEC,RAOA,RAOSS,RTASD,RTASW,RTPSP)
      DERIVE(ISEC,36)=RAOA
      DERIVE(ISEC,37)=RAOSS
      DERIVE(ISEC,87)=RTASD
      DERIVE(ISEC,88)=RTASW
      DERIVE(ISEC,89)=RTPSP
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
C RHGT  - Radar height (ft)
      CALL MEANPARAMNOFS(37,RV)
      RV=RV*0.25                                 !Radar height (ft)
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
C      RSR=SIND(RROLL)
C      RCR=COSD(RROLL)
C      RSP=SIND(RPTCH)
C      RCP=COSD(RPTCH)
C      RSH=SIND(RIHDG)
C      RCH=COSD(RIHDG)
      RSR=SIND(DERIVE(ISEC,156))
      RCR=COSD(DERIVE(ISEC,156))
      RSP=SIND(DERIVE(ISEC,157))
      RCP=COSD(DERIVE(ISEC,157))
      RSH=SIND(DERIVE(ISEC,158))
      RCH=COSD(DERIVE(ISEC,158))
      RV1=RTA*RCR-RTS*RSR
      RV2=RCP+RSP*RV1
      RV3=RTA*RSR+RTS*RCR
C      RV4=RCP*RYAWR*3.14159/180.0
C      RV5=RSP*RPITR*3.14159/180.0
      RV4=RCP*DERIVE(ISEC,164)*3.14159/180.0
      RV5=RSP*DERIVE(ISEC,163)*3.14159/180.0
      RIP=15.49                                  !Vanes to INU distance (m)
C V     - Northwards wind component (m s-1)
C      RV=RVN-RTAS*(RSH*RV3+RCH*RV2)-RIP*(RSH*RV4+RCH*RV5) !N wind (m s-1)
      RV=RVN-RTASD*(RSH*RV3+RCH*RV2)-RIP*(RSH*RV4+RCH*RV5) !N wind (m s-1)
      DERIVE(ISEC,55)=RV
C U     - Eastwards wind component (m s-1)
C      RU=RVE+RTAS*(RCH*RV3-RSH*RV2)+RIP*(RCH*RV4-RSH*RV5) !E wind (m s-1)
      RU=RVE+RTASD*(RCH*RV3-RSH*RV2)+RIP*(RCH*RV4-RSH*RV5) !E wind (m s-1)
      DERIVE(ISEC,56)=RU
C W     - Vertical wind component (m s-1)
C      RW=RVZ+RCP*(RTAS*(RV1-TAND(RPTCH))+RIP*RPITR*3.14159/180.0)!V wind(m s-1)
      RW=RVZ+RCP*(RTAS*(RV1-TAND(DERIVE(ISEC,157)))
     &  +RIP*DERIVE(ISEC,163)*3.14159/180.0)!V wind(m s-1)
      DERIVE(ISEC,57)=RW
C IWS   - INU derived wind speed (m s-1)
      RIWS=SQRT(RU**2+RV**2)                     !INU wind speed (m s-1)
      DERIVE(ISEC,58)=RIWS
C IWA   - INU derived wind angle (deg)
      IF(RU.EQ.0.0.AND.RV.EQ.0.0)  THEN
        RIWA=0.0
      ELSE
        RIWA=ATAN2D(RU,RV)+180.0       
      ENDIF
      DERIVE(ISEC,59)=RIWA
! VERVORT - vertical vorticity
! Planetary vorticity 2*omega*sin(lat) omega=2pi/24hrs
! (2*2*pi/24*60*60)*sin(lat) = pi*sin(lat)/4*60*60
      RPLANV=3.14159*SIND(DERIVE(ISEC,150))/21600.0
! Vertical vorticity = dv/dx-du/dy+f
! dv/dx  = v1-v0/(dx/dt)*dt
! du/dy  = u1-u0/(dy/dt)*dt
! ((v1-v0)/(dx/dt)-(u1-u0)/(dy/dt))/dt + f  ! dt = 3s
      RVV=((RV-ROLDV)/DERIVE(ISEC,154)-
     &         (RU-ROLDU)/DERIVE(ISEC,153))/3.0+RPLANV
      ROLDU=RU
      ROLDV=RV
      DERIVE(ISEC,168)=RVV
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
        RV=(IVAL(230,9)/10.0) - 273.16           !FWVS dewpoint (C)
!      ENDIF      
      DERIVE(ISEC,61)=RV        
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
      CONOW=CAL(154,1)+CAL(154,2)*IVAL(154,1) !CO m/r
      CO(I_CO)=CONOW
      I_CO=I_CO+1
      IF(I_CON.LT.10)I_CON=I_CON+1
      IF(I_CO.GT.10)THEN
        I_CO=1
      ENDIF
      COTOTAL=0
      DO I=1,I_CON 
        COTOTAL=COTOTAL+CO(I)
      ENDDO
      DERIVE(ISEC,76)=COTOTAL/REAL(I_CON)
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
C*******************************************************************************
      SUBROUTINE MEANPARAMNOFS(IP,RESULT)
C
C Calculates the mean value of a DRS parameter over a period of 1s.
C The result is returned as a real positive number.  This subroutine should 
C be used for ARINC-429 parameters which are recorded as signed 16 bit binary 
C numbers.  Does not include FFFE values in the mean.
C
C V2.00  16/09/02  W.D.N.JACKSON  Excludes FFFE's from ARINC-429 systems.
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
          IF(NFDATA(I,STATUS(1)).NE.'FFFE'X) THEN
            J=NFDATA(I,STATUS(1))
            JSUM=JSUM+J
            JCNT=JCNT+1
          END IF
        END DO
        IF(JCNT.GT.0) RESULT=FLOAT(JSUM)/JCNT !Work out mean
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
!*******************************************************************************
      SUBROUTINE S_PSP(RCAS,RSTP,RPSP)
!
! Computes pitot-static pressure from indicated (computed) airspeed and static 
! pressure from the following equations (see S_MACH and C_AIRSPD modules):
!
! IAS = 340.294 * Mach * SQRT(Static/1013.25)
! Mach= SQRT(5*((1+Pitot/Static)**(2/7)-1))
!
! where 340.294 is the speed of sound in m/s.
!
! RCAS - Corrected airspeed (m/s)
! RSTP - Static pressure (mb)
! RPSP - Pitot-static pressure (mb)
!
      REAL*4 RCAS,RSTP,RPSP,RMACH

      RPSP=0.
      IF(RSTP.EQ.0.) RETURN
      RMACH=RCAS/340.294/SQRT(RSTP/1013.25)
      RPSP=RSTP*((((RMACH**2.)/5.+1.)**3.5)-1.)
      RETURN
      END
!*******************************************************************************
      SUBROUTINE ALT2PRESS(RALT,RPRESS)
!
! Converts altitudes in feet to pressures in mb using the tables provided in
! NASA Technical Note D-822 (Tables of airspeed, altitude and mach number based 
! on latest international values for atmospheric properties and physical
! constants. Sadie P Livingston and William Gracey. August 1961).  If altitude
! is outside the range -1000 to 50000 ft then returns a pressure of 0 mb.
!
! This routine is provided to convert the altitudes provided by the 146 RVSM
! system (Innovative Solutions & Support Inc Air Data Display Unit) into 
! static pressures, using the same standard tables as are used by the RVSM 
! system to convert pressure into altitude.
!
! Pressure values in the NASA tables are given in lb/sq ft.  These have been
! converted to mb using 1 lb/sq in = 68.9476258 mb and 144 sq in = 1 sq ft.
!
! Only simple linear interpolation is used between the tabulated values. This
! will give maximum error of 0.005 mb which is well below the recorded
! resolution, let alone the system accuracy.
!
! V1.00  14/05/03  W.D.N.Jackson
!
      REAL*4 RALT,RPRESS,RTABLE(2,442)
      INTEGER*4 IL,IP,IH

      DATA RTABLE /                    !Heights (ft) and pressures (mb)
     & -1000.0,1050.408,
     &  -900.0,1046.644,
     &  -800.0,1042.890,
     &  -700.0,1039.146,
     &  -600.0,1035.416,
     &  -500.0,1031.691,
     &  -400.0,1027.985,
     &  -300.0,1024.284,
     &  -200.0,1020.597,
     &  -100.0,1016.915,
     &     0.0,1013.252,
     &   100.0,1009.594,
     &   200.0,1005.951,
     &   300.0,1002.312,
     &   400.0,998.6872,
     &   500.0,995.0770,
     &   600.0,991.4716,
     &   700.0,987.8806,
     &   800.0,984.2991,
     &   900.0,980.7273,
     &  1000.0,977.1649,
     &  1100.0,973.6171,
     &  1200.0,970.0739,
     &  1300.0,966.5452,
     &  1400.0,963.0259,
     &  1500.0,959.5163,
     &  1600.0,956.0211,
     &  1700.0,952.5306,
     &  1800.0,949.0544,
     &  1900.0,945.5880,
     &  2000.0,942.1310,
     &  2100.0,938.6836,
     &  2200.0,935.2458,
     &  2300.0,931.8176,
     &  2400.0,928.4037,
     &  2500.0,924.9947,
     &  2600.0,921.5999,
     &  2700.0,918.2147,
     &  2800.0,914.8392,
     &  2900.0,911.4732,
     &  3000.0,908.1168,
     &  3100.0,904.7700,
     &  3200.0,901.4328,
     &  3300.0,898.1098,
     &  3400.0,894.7917,
     &  3500.0,891.4880,
     &  3600.0,888.1891,
     &  3700.0,884.9045,
     &  3800.0,881.6294,
     &  3900.0,878.3640,
     &  4000.0,875.1033,
     &  4100.0,871.8571,
     &  4200.0,868.6204,
     &  4300.0,865.3932,
     &  4400.0,862.1757,
     &  4500.0,858.9677,
     &  4600.0,855.7693,
     &  4700.0,852.5804,
     &  4800.0,849.4012,
     &  4900.0,846.2316,
     &  5000.0,843.0715,
     &  5100.0,839.9209,
     &  5200.0,836.7800,
     &  5300.0,833.6486,
     &  5400.0,830.5268,
     &  5500.0,827.4146,
     &  5600.0,824.3120,
     &  5700.0,821.2189,
     &  5800.0,818.1354,
     &  5900.0,815.0615,
     &  6000.0,811.9971,
     &  6100.0,808.9376,
     &  6200.0,805.8924,
     &  6300.0,802.8568,
     &  6400.0,799.8259,
     &  6500.0,796.8095,
     &  6600.0,793.7979,
     &  6700.0,790.8005,
     &  6800.0,787.8080,
     &  6900.0,784.8251,
     &  7000.0,781.8517,
     &  7100.0,778.8879,
     &  7200.0,775.9337,
     &  7300.0,772.9891,
     &  7400.0,770.0540,
     &  7500.0,767.1238,
     &  7600.0,764.2078,
     &  7700.0,761.2966,
     &  7800.0,758.3951,
     &  7900.0,755.5032,
     &  8000.0,752.6208,
     &  8100.0,749.7479,
     &  8200.0,746.8847,
     &  8300.0,744.0263,
     &  8400.0,741.1822,
     &  8500.0,738.3429,
     &  8600.0,735.5132,
     &  8700.0,732.6930,
     &  8800.0,729.8824,
     &  8900.0,727.0767,
     &  9000.0,724.2852,
     &  9100.0,721.4986,
     &  9200.0,718.7215,
     &  9300.0,715.9540,
     &  9400.0,713.1913,
     &  9500.0,710.4431,
     &  9600.0,707.6995,
     &  9700.0,704.9655,
     &  9800.0,702.2411,
     &  9900.0,699.5215,
     & 10000.0,696.8162,
     & 10100.0,694.1158,
     & 10200.0,691.4250,
     & 10300.0,688.7437,
     & 10400.0,686.0671,
     & 10500.0,683.4003,
     & 10600.0,680.7429,
     & 10700.0,678.0951,
     & 10800.0,675.4521,
     & 10900.0,672.8187,
     & 11000.0,670.1948,
     & 11100.0,667.5806,
     & 11200.0,664.9711,
     & 11300.0,662.3712,
     & 11400.0,659.7809,
     & 11500.0,657.2001,
     & 11600.0,654.6241,
     & 11700.0,652.0578,
     & 11800.0,649.5010,
     & 11900.0,646.9490,
     & 12000.0,644.4065,
     & 12100.0,641.8737,
     & 12200.0,639.3456,
     & 12300.0,636.8271,
     & 12400.0,634.3181,
     & 12500.0,631.8188,
     & 12600.0,629.3242,
     & 12700.0,626.8392,
     & 12800.0,624.3591,
     & 12900.0,621.8884,
     & 13000.0,619.4274,
     & 13100.0,616.9711,
     & 13200.0,614.5244,
     & 13300.0,612.0873,
     & 13400.0,609.6599,
     & 13500.0,607.2322,
     & 13600.0,604.8191,
     & 13700.0,602.4108,
     & 13800.0,600.0120,
     & 13900.0,597.6227,
     & 14000.0,595.2383,
     & 14100.0,592.8586,
     & 14200.0,590.4933,
     & 14300.0,588.1328,
     & 14400.0,585.7771,
     & 14500.0,583.4310,
     & 14600.0,581.0944,
     & 14700.0,578.7626,
     & 14800.0,576.4405,
     & 14900.0,574.1278,
     & 15000.0,571.8200,
     & 15100.0,569.5169,
     & 15200.0,567.2235,
     & 15300.0,564.9396,
     & 15400.0,562.6605,
     & 15500.0,560.3910,
     & 15600.0,558.1262,
     & 15700.0,555.8710,
     & 15800.0,553.6255,
     & 15900.0,551.3846,
     & 16000.0,549.1487,
     & 16100.0,546.9222,
     & 16200.0,544.7054,
     & 16300.0,542.4933,
     & 16400.0,540.2908,
     & 16500.0,538.0931,
     & 16600.0,535.9050,
     & 16700.0,533.7216,
     & 16800.0,531.5431,
     & 16900.0,529.3789,
     & 17000.0,527.2147,
     & 17100.0,525.0601,
     & 17200.0,522.9150,
     & 17300.0,520.7748,
     & 17400.0,518.6441,
     & 17500.0,516.5183,
     & 17600.0,514.4019,
     & 17700.0,512.2904,
     & 17800.0,510.1837,
     & 17900.0,508.0865,
     & 18000.0,505.9990,
     & 18100.0,503.9113,
     & 18200.0,501.8382,
     & 18300.0,499.7697,
     & 18400.0,497.7061,
     & 18500.0,495.6472,
     & 18600.0,493.6028,
     & 18700.0,491.5583,
     & 18800.0,489.5233,
     & 18900.0,487.4980,
     & 19000.0,485.4727,
     & 19100.0,483.4617,
     & 19200.0,481.4507,
     & 19300.0,479.4493,
     & 19400.0,477.4565,
     & 19500.0,475.4686,
     & 19600.0,473.4877,
     & 19700.0,471.5137,
     & 19800.0,469.5462,
     & 19900.0,467.5851,
     & 20000.0,465.6311,
     & 20100.0,463.6833,
     & 20200.0,461.7422,
     & 20300.0,459.8079,
     & 20400.0,457.8802,
     & 20500.0,455.9588,
     & 20600.0,454.0440,
     & 20700.0,452.1356,
     & 20800.0,450.2337,
     & 20900.0,448.3387,
     & 21000.0,446.4498,
     & 21100.0,444.5671,
     & 21200.0,442.6911,
     & 21300.0,440.8219,
     & 21400.0,438.9584,
     & 21500.0,437.1021,
     & 21600.0,435.2515,
     & 21700.0,433.4076,
     & 21800.0,431.5695,
     & 21900.0,429.7386,
     & 22000.0,427.9134,
     & 22100.0,426.0944,
     & 22200.0,424.2816,
     & 22300.0,422.4756,
     & 22400.0,420.6753,
     & 22500.0,418.8817,
     & 22600.0,417.0939,
     & 22700.0,415.3127,
     & 22800.0,413.5373,
     & 22900.0,411.7682,
     & 23000.0,410.0052,
     & 23100.0,408.2480,
     & 23200.0,406.4970,
     & 23300.0,404.7527,
     & 23400.0,403.0137,
     & 23500.0,401.2814,
     & 23600.0,399.5548,
     & 23700.0,397.8340,
     & 23800.0,396.1194,
     & 23900.0,394.4110,
     & 24000.0,392.7084,
     & 24100.0,391.0121,
     & 24200.0,389.3214,
     & 24300.0,387.6364,
     & 24400.0,385.9578,
     & 24500.0,384.2848,
     & 24600.0,382.6176,
     & 24700.0,380.9562,
     & 24800.0,379.3010,
     & 24900.0,377.6515,
     & 25000.0,376.0078,
     & 25100.0,374.3698,
     & 25200.0,372.7376,
     & 25300.0,371.1111,
     & 25400.0,369.4908,
     & 25500.0,367.8758,
     & 25600.0,366.2665,
     & 25700.0,364.6630,
     & 25800.0,363.0652,
     & 25900.0,361.4733,
     & 26000.0,359.8865,
     & 26100.0,358.3059,
     & 26200.0,356.7307,
     & 26300.0,355.1612,
     & 26400.0,353.5970,
     & 26500.0,352.0389,
     & 26600.0,350.4862,
     & 26700.0,348.9387,
     & 26800.0,347.3969,
     & 26900.0,345.8609,
     & 27000.0,344.3302,
     & 27100.0,342.8047,
     & 27200.0,341.2850,
     & 27300.0,339.7705,
     & 27400.0,338.2618,
     & 27500.0,336.7584,
     & 27600.0,335.2607,
     & 27700.0,333.7678,
     & 27800.0,332.2806,
     & 27900.0,330.7987,
     & 28000.0,329.3221,
     & 28100.0,327.8512,
     & 28200.0,326.3856,
     & 28300.0,324.9248,
     & 28400.0,323.4697,
     & 28500.0,322.0199,
     & 28600.0,320.5753,
     & 28700.0,319.1356,
     & 28800.0,317.7015,
     & 28900.0,316.2723,
     & 29000.0,314.8488,
     & 29100.0,313.4301,
     & 29200.0,312.0167,
     & 29300.0,310.6086,
     & 29400.0,309.2057,
     & 29500.0,307.8076,
     & 29600.0,306.4147,
     & 29700.0,305.0272,
     & 29800.0,303.6449,
     & 29900.0,302.2673,
     & 30000.0,300.8947,
     & 30100.0,299.5272,
     & 30200.0,298.1649,
     & 30300.0,296.8076,
     & 30400.0,295.4554,
     & 30500.0,294.1081,
     & 30600.0,292.7655,
     & 30700.0,291.4282,
     & 30800.0,290.0957,
     & 30900.0,288.7684,
     & 31000.0,287.4455,
     & 31100.0,286.1279,
     & 31200.0,284.8155,
     & 31300.0,283.5074,
     & 31400.0,282.2045,
     & 31500.0,280.9060,
     & 31600.0,279.6128,
     & 31700.0,278.3243,
     & 31800.0,277.0406,
     & 31900.0,275.7618,
     & 32000.0,274.4877,
     & 32100.0,273.2184,
     & 32200.0,271.9539,
     & 32300.0,270.6936,
     & 32400.0,269.4387,
     & 32500.0,268.1885,
     & 32600.0,266.9427,
     & 32700.0,265.7017,
     & 32800.0,264.4654,
     & 32900.0,263.2339,
     & 33000.0,262.0067,
     & 33100.0,260.7843,
     & 33200.0,259.5663,
     & 33300.0,258.3534,
     & 33400.0,257.1449,
     & 33500.0,255.9408,
     & 33600.0,254.7414,
     & 33700.0,253.5468,
     & 33800.0,252.3564,
     & 33900.0,251.1705,
     & 34000.0,249.9892,
     & 34100.0,248.8124,
     & 34200.0,247.6402,
     & 34300.0,246.4724,
     & 34400.0,245.3089,
     & 34500.0,244.1502,
     & 34600.0,242.9958,
     & 34700.0,241.8458,
     & 34800.0,240.7000,
     & 34900.0,239.5590,
     & 35000.0,238.4223,
     & 35100.0,237.2895,
     & 35200.0,236.1614,
     & 35300.0,235.0381,
     & 35400.0,233.9187,
     & 35500.0,232.8036,
     & 35600.0,231.6927,
     & 35700.0,230.5862,
     & 35800.0,229.4840,
     & 35900.0,228.3861,
     & 36000.0,227.2925,
     & 36100.0,226.2028,
     & 36200.0,225.1183,
     & 36400.0,222.9646,
     & 36600.0,220.8316,
     & 36800.0,218.7191,
     & 37000.0,216.6267,
     & 37200.0,214.5545,
     & 37400.0,212.5018,
     & 37600.0,210.4688,
     & 37800.0,208.4555,
     & 38000.0,206.4613,
     & 38200.0,204.4857,
     & 38400.0,202.5298,
     & 38600.0,200.5921,
     & 38800.0,198.6731,
     & 39000.0,196.7727,
     & 39200.0,194.8900,
     & 39400.0,193.0256,
     & 39600.0,191.1793,
     & 39800.0,189.3503,
     & 40000.0,187.5385,
     & 40200.0,185.7444,
     & 40400.0,183.9676,
     & 40600.0,182.2075,
     & 40800.0,180.4647,
     & 41000.0,178.7381,
     & 41200.0,177.0283,
     & 41400.0,175.3348,
     & 41600.0,173.6570,
     & 41800.0,171.9961,
     & 42000.0,170.3504,
     & 42200.0,168.7211,
     & 42400.0,167.1070,
     & 42600.0,165.5083,
     & 42800.0,163.9249,
     & 43000.0,162.3563,
     & 43200.0,160.8036,
     & 43400.0,159.2652,
     & 43600.0,157.7416,
     & 43800.0,156.2325,
     & 44000.0,154.7376,
     & 44200.0,153.2572,
     & 44400.0,151.7911,
     & 44600.0,150.3393,
     & 44800.0,148.9010,
     & 45000.0,147.4766,
     & 45200.0,146.0655,
     & 45400.0,144.6679,
     & 45600.0,143.2842,
     & 45800.0,141.9134,
     & 46000.0,140.5560,
     & 46200.0,139.2110,
     & 46400.0,137.8795,
     & 46600.0,136.5603,
     & 46800.0,135.2542,
     & 47000.0,133.9600,
     & 47200.0,132.6787,
     & 47400.0,131.4094,
     & 47600.0,130.1520,
     & 47800.0,128.9072,
     & 48000.0,127.6738,
     & 48200.0,126.4523,
     & 48400.0,125.2424,
     & 48600.0,124.0444,
     & 48800.0,122.8580,
     & 49000.0,121.6825,
     & 49200.0,120.5185,
     & 49400.0,119.3656,
     & 49600.0,118.2236,
     & 49800.0,117.0922,
     & 50000.0,115.9723/

      RPRESS=0.
      IF(RALT.LT.-1000..OR.RALT.GE.50000.) RETURN
      IL=1                             !Find nearest two altitudes
      IH=442
      DO WHILE(IL+1.NE.IH)
        IP=(IH+IL)/2
        IF(RALT.LT.RTABLE(1,IP)) IH=IP
        IF(RALT.GE.RTABLE(1,IP)) IL=IP
      END DO
      RPRESS=RTABLE(2,IL)+(RTABLE(2,IH)-RTABLE(2,IL)) !Linear interpolation
     &    *(RALT-RTABLE(1,IL))/(RTABLE(1,IH)-RTABLE(1,IL))
      RETURN
      END
!*******************************************************************************
      SUBROUTINE TURB(ISEC,RAOA,RAOSS,RTASD,RTASW,RTPSP)
!
! Computes air incidence and speed from the turbulence probe.
! Currently only a dummy routine.  Takes its input data from the DERIVE array.
!
      IMPLICIT   NONE
      INTEGER*2  NFDATA(2048,2),NPLOC(512),NPFREQ(512),STATUS(256),
     -    SPECIAL(12800,10)
      INTEGER*4  ISEC
      REAL*4     DERIVE(12800,196),RAOA,RAOSS,RTASD,RTASW,RTPSP
      REAL*4     AIAS,TTDI,TTND,SPR,PSP,PHGT,TP0,TPA,TPB,CA,CB,AMACH
      REAL*4     A0,A1,B0,B1,DCPA,DCPB,R
      REAL*4     RCONST(18)
      COMMON     /HCOM/ NFDATA,NPLOC,NPFREQ,STATUS,DERIVE,SPECIAL
      VOLATILE   /HCOM/
      DATA       RCONST/3.35361E-01,2.78277E-01,-5.73689E-01,
     & -6.1619E-02,-5.2595E-02,1.0300E-01,-2.1887E-02,
     & 0.0000E-00,0.0000E0,5.7967E-02,-1.7229E-02,0.0000E0,
     & 2.0000E-03,0.9984E0,-0.4126E+0,1.0776E+0,0.0050E+0,0.9505E+0/
      IF(ISEC.GE.1.AND.ISEC.LE.12800) THEN
        AIAS=DERIVE(ISEC,4)
        TTDI=DERIVE(ISEC,8)
        TTND=DERIVE(ISEC,11)
        SPR=DERIVE(ISEC,64)
        PSP=DERIVE(ISEC,65)
        PHGT=DERIVE(ISEC,66)
        TP0=DERIVE(ISEC,81)
        TPA=DERIVE(ISEC,82)
        TPB=DERIVE(ISEC,83)
        CA=DERIVE(ISEC,84)
        CB=DERIVE(ISEC,85)
        AMACH=DERIVE(ISEC,6)
        RAOA=0.
        RAOSS=0.
        RTASD=0.
        RTASW=0.
        RTPSP=0.
! First guess AOA and AOSS
        A0 = RCONST(1)+AMACH*(RCONST(2)+AMACH*RCONST(3))
        A1 = RCONST(4)+AMACH*(RCONST(5)+AMACH*RCONST(6))
        B0 = RCONST(7)+AMACH*(RCONST(8)+AMACH*RCONST(9))
        B1 = RCONST(10)+AMACH*(RCONST(11)+AMACH*RCONST(12))
        RAOA  = (TPA/PSP - A0)/A1
        RAOSS = (TPB/PSP - B0)/B1
! Calculate and apply flow angle corrections to derive true pitot pressure
! from centre-port measurement.
        DCPA = 0.0273+ RAOA*(-0.0141+ RAOA*(0.00193- RAOA*5.2E-5))
        DCPB = 0.0   +RAOSS*(0.0    + RAOSS*7.6172E-4)

! Apply corrections to measured differential pressure
        RTPSP = TP0+(DCPA+DCPB)*PSP
        R=0.0
        IF((SPR.GT.0).AND.(RTPSP.GT.0))
     &       R=5.0*((1.0+RTPSP/SPR)**(2.0/7.0)-1.0)
        IF(R.GT.0) AMACH=SQRT(R)         !Mach No
        RTASD = RCONST(14) * 340.294 * AMACH * SQRT(TTDI/288.15)
        RTASW = RCONST(14) * 340.294 * AMACH * SQRT(TTDI/288.15)
! Apply linear corrections to calculated AoA and AoSS, derived from Al Rodi
! analysis - minimization of residual vertical wind during yawing orbits.
        RAOA = RAOA*RCONST(16) + RCONST(15)
        RAOSS= RAOSS*RCONST(18) + RCONST(17)
        
      END IF
      RETURN
      END
