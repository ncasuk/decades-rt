import numpy as np
import rt_data
reload(rt_data)

class derived(rt_data.rt_data):
    """ A collection of the processing routines for realtime in flight data """
    def pressure_height_feet(self,data):
        rvsm_alt=self.getdata('rvsm_alt',data)
        return rvsm_alt*4
    def pressure_height_kft(self,data):
        feet=self.getdata('pressure_height_feet',data) 
        return feet/1000.0
    def pressure_height_meters(self,data):
        feet=self.getdata('pressure_height_feet',data) 
        return feet*0.3048
    def static_pressure(self,data):
        feet=self.getdata('pressure_height_feet',data)
        return 1013.25*(1-6.87535e-6*feet)**5.2561
    def indicated_air_speed(self,data):
        rvsm_ias=self.getdata('rvsm_ias',data)
        return rvsm_ias/32*0.514444
    def pitot_static_pressure(self,data):
        spr=self.getdata('static_pressure',data)
        ias=self.getdata('indicated_air_speed',data)
        psp=np.zeros(len(spr))
        ind=np.where(spr>0)
        rmach=ias[ind]/340.294/((spr[ind]/1013.25)**0.5)
        psp[ind]=spr[ind]*((((rmach**2.0)/5.0+1.0)**3.5)-1.)
        return psp
    def mach_no(self,data):
        psp=self.getdata('pitot_static_pressure',data)
        spr=self.getdata('static_pressure',data)
        rmach=np.zeros(len(spr))
        ind=np.where((psp>0) & (spr>0))
        rmach[ind]=5.0*((1.0+psp[ind]/spr[ind])**(2.0/7.0)-1.0)
        ind=np.where(rmach>0)
        rmach[ind]=rmach[ind]**0.5
        return rmach
    def s9_static_pressure(self,data):
        raw=self.getdata('s9_press',data)
        #c=self.cals['CAL221']
        c=self.getdata('CAL221',data)
        return c[0]+c[1]*raw+c[2]*raw**2
    def turb_probe_pitot_static(self,data):
        c=self.cals['CAL215']
        raw=self.getdata('tp_p0_s10',data)
        return c[0]+c[1]*raw+c[2]*raw**2
    def turb_probe_attack_diff(self,data):
        c=self.cals['CAL216']
        raw=self.getdata('tp_up_down',data)
        return c[0]+c[1]*raw+c[2]*raw**2
    def turb_probe_sideslip_diff(self,data):
        c=self.cals['CAL217']
        raw=self.getdata('tp_left_right',data)
        return c[0]+c[1]*raw+c[2]*raw**2
    def turb_probe_attack_check(self,data):
        c=self.cals['CAL218']
        raw=self.getdata('tp_top_s10',data)
        return c[0]+c[1]*raw+c[2]*raw**2
    def turb_probe_sideslip_check(self,data):
        c=self.cals['CAL219']
        raw=self.getdata('tp_right_s10',data)
        return c[0]+c[1]*raw+c[2]*raw**2
    def deiced_indicated_air_temp_c(self,data):
        c=self.cals['CAL010']
        raw=self.getdata('di_temp',data)
        sig_reg=self.getdata('sig_register',data)
        ans=c[0]+c[1]*raw+c[2]*raw**2
        di=np.where(np.array(sig_reg,dtype='i2') & int('00100000',2))
        ans[di]-=self.cals['CAL001'][0]
        return ans
    def deiced_true_air_temp_k(self,data):
        iatdi_C=self.getdata('deiced_indicated_air_temp_c',data)
        mach=self.getdata('mach_no',data)
        return (iatdi_C+273.16)/(1.0+(0.2*mach**2*0.956))
    def deiced_true_air_temp_c(self,data):
        tatdi_K=self.getdata('deiced_true_air_temp_k',data)
        return tatdi_K-273.16
    def nondeiced_indicated_air_temp_c(self,data):
        raw=self.getdata('ndi_temp',data)
        c=self.cals['CAL023']
        return c[0]+c[1]*raw+c[2]*raw**2
    def nondeiced_true_air_temp_k(self,data):
        iatndi_C=self.getdata('nondeiced_indicated_air_temp_c',data)
        mach=self.getdata('mach_no',data)    
        return (iatndi_C+273.16)/(1.0+(0.2*mach**2*0.985))
    def nondeiced_true_air_temp_c(self,data):
        tatndi_K=self.getdata('nondeiced_true_air_temp_k',data)
        return tatndi_K-273.16
    def true_air_speed(self,data): 
        spr=self.getdata('static_pressure',data)
        tatdi_K=self.getdata('deiced_true_air_temp_k',data)
        ias=self.getdata('raw_ias',data) 
        tas=np.zeros(len(ias))
        good=np.where((spr>0.0) & (tatdi_K>0.0))
        tas[good]=self.cals['CAL004'][0]*(ias[good]*((1013.25/spr[good])*(tatdi_K[good]/288.15))**0.5)
        return tas*1.944
    def angle_of_attack(self,data):
        mach=self.getdata('mach_no',data)
        tpad=self.getdata('turb_probe_attack_diff',data)
        psp=self.getdata('pitot_static_pressure',data)
        #c=self.cals['CALAOA']
        c=[3.35361E-01,2.78277E-01,-5.73689E-01,-6.1619E-02,-5.2595E-02,1.0300E-01,1.0776E+0,-0.4126E+0]
        AOA=np.empty(len(mach))
        AOA[:]=6.0
        A0 = c[0]+mach*(c[1]+mach*c[2])
        A1 = c[3]+mach*(c[4]+mach*c[5])
        ind=np.where((A1!=0) & (psp!=0))
        print A1,psp
        AOA[ind]=(tpad[ind]/psp[ind]-A0[ind])/A1[ind]
        AOA = AOA*c[6] + c[7]
        return AOA
        
    def angle_of_sideslip(self,data):
        mach=self.getdata('mach_no',data)
        tpsd=self.getdata('turb_probe_sideslip_diff',data)
        psp=self.getdata('pitot_static_pressure',data)
        #c=self.cals['CALAOSS']
        c=[-2.1887E-02,0.0000E-00,0.0000E0,5.7967E-02,-1.7229E-02,0.0000E0,0.9505E+0,0.0050E+0]
        AOSS=np.zeros(len(mach))
        B0 = c[0]+mach*(c[1]+mach*c[2])
        B1 = c[3]+mach*(c[4]+mach*c[5])
        ind=np.where((B1!=0) & (psp!=0))
        print B1,psp
        AOSS[ind]=(tpsd[ind]/psp[ind]-B0[ind])/B1[ind]
        AOSS = AOSS*c[6] + c[7]
        return AOSS

    def turb_probe_cor_pitot_static(self,data):
        # Calculate and apply flow angle corrections to derive true pitot pressure
        # from centre-port measurement.
        RAOA=self.getdata('angle_of_attack',data)
        RAOSS=self.getdata('angle_of_sideslip',data)
        PSP=self.getdata('pitot_static_pressure',data)
        TP0=self.getdata('turb_probe_pitot_static',data)
        DCPA = 0.0273+ RAOA*(-0.0141+ RAOA*(0.00193- RAOA*5.2E-5))
        DCPB = 0.0   +RAOSS*(0.0    + RAOSS*7.6172E-4)
        # Apply corrections to measured differential pressure
        RTPSP = TP0+(DCPA+DCPB)*PSP
        return RTPSP
    def turb_probe_tas(self,data):
        AMACH=self.getdata('mach_no',data)
        SPR=self.getdata('static_pressure',data)
        RTPSP=self.getdata('turb_probe_cor_pitot_static',data)
        TTDI=self.getdata('deiced_true_air_temp_k',data)
        #c=self.cals['CALTAS']
        c=[0.9984E0]
        R=np.zeros(len(SPR))
        ind=np.where((SPR>0) & (RTPSP>0))
        R[ind]=5.0*((1.0+RTPSP[ind]/SPR[ind])**(2.0/7.0)-1.0)
        ind=np.where(R>0)
        AMACH[ind]=(R[ind]**0.5)         #!Mach No
        return AMACH
        return c[0] * 340.294 * AMACH * (TTDI/288.15)**0.5
        
    def potential_temperature(self,data):
        RSPR=self.getdata('static_pressure',data)
        RTATDI=self.getdata('deiced_true_air_temp_k',data)
        RPOT=np.zeros(len(RSPR))
        ind=np.where(RSPR>0)
        RPOT[ind]=RTATDI[ind]*(1000.0/RSPR[ind])**(2.0/7.0) #!Potential temp (K)
        return RPOT

    def dry_air_density(self,data):
        RSPR=self.getdata('static_pressure',data)
        RTATDI=self.getdata('deiced_true_air_temp_k',data)
        RDAD=np.zeros(len(RSPR))
        ind=np.where(RTATDI!=0)
        RDAD[ind]=0.34838*RSPR[ind]/RTATDI[ind] # !Dry air dens (kg m-3)
        return RDAD
        
    def dew_point(self,data):
        """Dew point (deg C) from General Eastern Hygrometer"""
        raw=self.getdata('ge_dew',data)
        c=self.cals['CAL058']
        hycc=self.getdata('ge_cont',data)
        cc=np.where((hycc>18076) | (hycc<15451)) # not sure what to do with this info ( control lost )
        return raw*c[1]+c[0]

    def relative_humidity(self,data):
        """Relative humidity (%)"""
        rd=self.getdata('dew_point')+273.16
        rt=self.getdata('deiced_true_air_temp_k')
        rh=np.zeros(len(rd))
        sat=np.where(rd>=rt)
        rh[sat]=100.0
        unsat=np.where(rd<rt)
        esbot=6.112*np.exp((17.67*rt[unsat])/(243.5+rt[unsat]))
        ind=unsat[np.where(esbot!=0)]
        rh[ind]=(6.112*np.exp((17.67*rd[ind])/(243.5+rd[ind])))/esbot
        return rh

    def vapour_pressure(self,data):
        """Vapour pressure (mb)"""
        r=1000.0/(self.getdata('dew_point')+273.16)
        vp=10.0**(8.42926609-(1.82717843+(0.07120871*r))*r) #Vap press (mb)
        return vp

"""
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
      DERIVE(ISEC,94)=RILNG"""


