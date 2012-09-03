import numpy as np
import rt_data
from datetime import datetime
import time

class derived(rt_data.rt_data):
    """ A collection of the processing routines for realtime in flight data """
    def pressure_height_feet(self,data):
        rvsm_alt=self.getdata('prtaft01_pressure_alt',data)
        return rvsm_alt*4
    def pressure_height_kft(self,data):
        feet=self.getdata('pressure_height_feet',data) 
        return feet/1000.0
    def pressure_height_m(self,data):
        feet=self.getdata('pressure_height_feet',data) 
        return feet*0.3048
    def static_pressure(self,data):
        feet=self.getdata('pressure_height_feet',data)
        return 1013.25*(1-6.87535e-6*feet)**5.2561
    def indicated_air_speed_knts(self,data):
        rvsm_ias=self.getdata('prtaft01_ind_air_speed',data) 
        return rvsm_ias/32
    def indicated_air_speed(self,data):
        ias=self.getdata('indicated_air_speed_knts',data) 
        return ias*0.514444 # m s-1 ( should it be knots ? )
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
        raw=self.getdata('corcon01_s9_press',data)
        #c=self.cals['CAL221']
        c=self.getdata('CAL221',data)
        return c[0]+c[1]*raw+c[2]*raw**2
    def turb_probe_pitot_static(self,data):
        c=self.cals['CAL215']
        raw=self.getdata('corcon01_tp_p0_s10',data)
        return c[0]+c[1]*raw+c[2]*raw**2
    def turb_probe_attack_diff(self,data):
        c=self.cals['CAL216']
        raw=self.getdata('corcon01_tp_up_down',data)
        return c[0]+c[1]*raw+c[2]*raw**2
    def turb_probe_sideslip_diff(self,data):
        c=self.cals['CAL217']
        raw=self.getdata('corcon01_tp_left_right',data)
        return c[0]+c[1]*raw+c[2]*raw**2
    def turb_probe_attack_check(self,data):
        c=self.cals['CAL218']
        raw=self.getdata('corcon01_tp_top_s10',data)
        return c[0]+c[1]*raw+c[2]*raw**2
    def turb_probe_sideslip_check(self,data):
        c=self.cals['CAL219']
        raw=self.getdata('corcon01_tp_right_s10',data)
        return c[0]+c[1]*raw+c[2]*raw**2
    def deiced_indicated_air_temp_c(self,data):
        c=self.cals['CAL010']
        raw=self.getdata('corcon01_di_temp',data)
        di=self.getdata('prtaft01_deiced_temp_flag',data)
        #sig_reg=self.getdata('sig_register',data)
        #di=np.where(np.array(sig_reg,dtype='i2') & int('00100000',2))
        ans=c[0]+c[1]*raw+c[2]*raw**2
        #ans[di]-=self.cals['CAL001'][0]
        ans = ans - self.cals['CAL001'][0]*di
        return ans
    def deiced_true_air_temp_k(self,data):
        iatdi_C=self.getdata('deiced_indicated_air_temp_c',data)
        mach=self.getdata('mach_no',data)
        return (iatdi_C+273.16)/(1.0+(0.2*mach**2*0.956))
    def deiced_true_air_temp_c(self,data):
        tatdi_K=self.getdata('deiced_true_air_temp_k',data)
        return tatdi_K-273.16
    def nondeiced_indicated_air_temp_c(self,data):
        raw=self.getdata('corcon01_ndi_temp',data)
        c=self.cals['CAL023']
        return c[0]+c[1]*raw+c[2]*raw**2
    def nondeiced_true_air_temp_k(self,data):
        iatndi_C=self.getdata('nondeiced_indicated_air_temp_c',data)
        mach=self.getdata('mach_no',data)    
        return (iatndi_C+273.16)/(1.0+(0.2*mach**2*0.985))
    def nondeiced_true_air_temp_c(self,data):
        tatndi_K=self.getdata('nondeiced_true_air_temp_k',data)
        return tatndi_K-273.16
    def true_air_speed_ms(self,data): 
        spr=self.getdata('static_pressure',data)
        tatdi_K=self.getdata('deiced_true_air_temp_k',data)
        ias=self.getdata('indicated_air_speed',data) 
        tas=np.zeros(len(ias))
        good=np.where((spr>0.0) & (tatdi_K>0.0))
        tas[good]=self.cals['CAL004'][0]*(ias[good]*((1013.25/spr[good])*(tatdi_K[good]/288.15))**0.5)
        return tas
        
    def true_air_speed(self,data):
        tas=self.getdata('true_air_speed_ms',data) 
        return tas*1.944 # knots (how strange )

    def angle_of_attack(self,data):
        mach=self.getdata('mach_no',data)
        tpad=self.getdata('turb_probe_attack_diff',data)
        psp=self.getdata('pitot_static_pressure',data)
        c=self.cals['CALAOA']
        AOA=np.empty(len(mach))
        AOA[:]=6.0
        A0 = c[0]+mach*(c[1]+mach*c[2])
        A1 = c[3]+mach*(c[4]+mach*c[5])
        ind=np.where((A1!=0) & (psp!=0))
        #print A1,psp
        AOA[ind]=(tpad[ind]/psp[ind]-A0[ind])/A1[ind]
        AOA = AOA*c[6] + c[7]
        return AOA
        
    def angle_of_sideslip(self,data):
        mach=self.getdata('mach_no',data)
        tpsd=self.getdata('turb_probe_sideslip_diff',data)
        psp=self.getdata('pitot_static_pressure',data)
        c=self.cals['CALAOSS']
        AOSS=np.zeros(len(mach))
        B0 = c[0]+mach*(c[1]+mach*c[2])
        B1 = c[3]+mach*(c[4]+mach*c[5])
        ind=np.where((B1!=0) & (psp!=0))
        #print B1,psp
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
        c=self.cals['CALTAS']
        R=np.zeros(len(SPR))
        ind=np.where((SPR>0) & (RTPSP>0))
        R[ind]=5.0*((1.0+RTPSP[ind]/SPR[ind])**(2.0/7.0)-1.0)
        ind=np.where(R>0)
        AMACH[ind]=(R[ind]**0.5)         #!Mach No
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
        raw=self.getdata('corcon01_ge_dew',data)
        c=self.cals['CAL058']
        hycc=self.getdata('corcon01_ge_cont',data)
        cc=np.where((hycc>18076) | (hycc<15451)) # not sure what to do with this info ( control lost )
        return raw*c[1]+c[0]

    '''def relative_humidity(self,data):
        """Relative humidity (%)"""
        rd=self.getdata('dew_point',data)+273.16
        rt=self.getdata('deiced_true_air_temp_k',data)
        rh=np.zeros(len(rd))
        sat=np.where(rd>=rt)
        rh[sat]=100.0
        unsat=np.where(rd<rt)
        esbot=6.112*np.exp((17.67*rt[unsat])/(243.5+rt[unsat]))
        ind=unsat[np.where(esbot!=0)]
        rh[ind]=(6.112*np.exp((17.67*rd[ind])/(243.5+rd[ind])))/esbot
        return rh'''
    def relative_humidity(self,data):
        """Relative humidity (%)"""
        Td=self.getdata('dew_point',data)
        T=self.getdata('deiced_true_air_temp_c',data)
        RH=np.zeros(len(Td))
        RH[Td>=T]=100.0     # saturated temp<dew point
        unsat=Td<T          # calculate for unsaturated section
        # NOAA approximation http://www.srh.noaa.gov/images/epz/wxcalc/rhTdFromWetBulb.pdf
        # es = 6.112*exp(17.67*T/(T+243.5))
        # e  = 6.112*exp(17.67*Td/(Td+243.5))
        # RH = 100*e/es 
 
        RH[unsat]=100.0*np.exp(17.67*(Td[unsat]/(243.5+Td[unsat])-T[unsat]/(243.5+T[unsat])))
        return RH

    def vapour_pressure(self,data):
        """Vapour pressure (mb)"""
        r=1000.0/(self.getdata('dew_point',data)+273.16)
        vp=10.0**(8.42926609-(1.82717843+(0.07120871*r))*r) #Vap press (mb)
        return vp

    def moist_air_density(self,data):
        """Moist air density (kg m-3)"""
        spr=self.getdata('static_pressure',data)
        vp=self.getdata('vapour_pressure',data)
        tat=self.getdata('deiced_true_air_temp_k',data)
        mad=np.zeros(len(spr))
        ind=np.where(tat>0)
        mad[ind]=0.34838*(spr[ind]-0.378*vp[ind])/tat[ind] #Mst a dens (kg m-3)
        return mad
        
    def specific_humidity(self,data):
        """Specific humidity (g kg-1)"""
        vp=self.getdata('vapour_pressure',data)
        spr=self.getdata('static_pressure',data)
        shum=np.zeros(len(spr))
        ind=np.where((spr!=0) | (vp!=0))
        shum[ind]=622.0*vp[ind]/(spr[ind]-0.378*vp[ind])         #Spec humidity (g kg-1)
        return shum

    def mass_mixing_ratio(self,data):
        """Mass mixing ratio (g kg-1)"""
        vp=self.getdata('vapour_pressure',data)
        spr=self.getdata('static_pressure',data)
        mmr=np.zeros(len(spr))
        ind=np.where((spr-vp)!=0)
        mmr[ind]=622.0*vp[ind]/(spr[ind]-vp[ind])         #Mass mix ratio (g kg-1)
        return mmr
        
    def humidity_mixing_ratio(self,data):
        """Humidity mixing ratio (g m-3)"""
        shum=self.getdata('specific_humidity',data)
        mad=self.getdata('moist_air_density',data)
        return shum*mad
        
    def equivalent_potential_temp(self,data):
        """ Equivalent Potential Temperature K"""
        tatc=self.getdata('deiced_true_air_temp_c',data)
        tatk=self.getdata('deiced_true_air_temp_k',data)
        mmr=self.getdata('mass_mixing_ratio',data)
        pot=self.getdata('potential_temperature',data)
        pote=np.zeros(len(tatc))
        rl=2.834E6 - 259.5*tatc
        ind=np.where((tatk>0) & (mmr>0))
        pote[ind]=pot*np.exp(rl*mmr/(1000*1005*tatk))
        return pote

    def jw_liquid_water_content(self,data):
        """Johnson Williams liquid water (g m-3)"""
        c=self.cals['CAL042']
        raw=self.getdata('corcon01_jw_lwc',data)
        tas=self.getdata('true_air_speed',data)
        jw=c[1]*raw+c[0]
        lwc=np.zeros(len(raw))
        ind=np.where(tas!=0)
        lwc[ind]=jw[ind]*77.2/tas[ind]
        return lwc


    def total_water_content(self,data):    
        """ TWC   - total water content (g kg-1)
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
      DERIVE(ISEC,60)=RTWC"""
        P0=1013.25
        P1=self.getdata('static_pressure',data)
        cT=self.cals['CAL072']
        cS=self.cals['CAL070']
        Traw=self.getdata('.twc_temp',data)     # Which cRIO ?
        Sraw=self.getdata('.twc_det',data)
        T2=cT[0]+Traw*cT[1]+Traw**2*cT[2]+Traw**3*cT[3]+Traw**4*cT[4]+Traw**5*cT[5]
        F=0.93  # Ratio of internal/external pressures
        P2=F*P1
        KO2=0.304+0.351*P2/P0
        Kv=427.0
        uO2=0.2095
        oxycor=(KO2*uO2*P1)/Kv
        S=Sraw*cS[1]+cS[0]
        vp=T2*S-oxycor
        vmr=vp/(P2-vp)
        mmr=622.0*vmr
        return mmr
        
    def dew_point_total_water(self,data):
        #TWCDP - Dewpoint from Total Water Content  (deg C)
        spr=self.getdata('static_pressure',data)
        twc_mmr=self.getdata('total_water_content',data)
        DP=np.zeroes(len(spr))
        ind=np.where(twc_mmr*spr>0)
        DP[ind]=5.42E3 / LOG(1.57366E12/(spr[ind]*twc_mmr[ind])) -273.16 # Dewpoint (C) 
        return DP
        
    def radar_height(self,data):
        """Radar height (ft)"""
        return self.getdata('prtaft01_rad_alt',data)/4.0

    def vertical_vorticity(self,data):
        """
! VERVORT - vertical vorticity
! Planetary vorticity 2*omega*sin(lat) omega=2pi/24hrs
! (2*2*pi/24*60*60)*sin(lat) = pi*sin(lat)/4*60*60
      RVV=3.14159*SIND(DERIVE(ISEC,150))/21600.0
! Vertical vorticity = dv/dx-du/dy+f
! dv/dx  = v1-v0/(dx/dt)*dt
! du/dy  = u1-u0/(dy/dt)*dt
! ((v1-v0)/(dx/dt)-(u1-u0)/(dy/dt))/dt + f  ! dt = 3s
      IF(ABS(DERIVE(ISEC,154)).GT.0.0) 
     &  RVV=RVV+(RV-ROLDV)/(DERIVE(ISEC,154)*3.0)
      IF(ABS(DERIVE(ISEC,153)).GT.0.0)
     &  RVV=RVV-(RU-ROLDU)/(DERIVE(ISEC,153)*3.0)
      ROLDU=RU
      ROLDV=RV
      DERIVE(ISEC,168)=RVV
"""
        return None
        
    def upper_pyranometer_clear_flux(self,data):
        ''' not sure why only CAL081[1] is used and not CAL081[0], but his is what HOR_CALCS did '''
        c=self.cals['CAL081'] 
        corr=self.getdata('pyranometer_correction',data)
        return (self.getdata('uppbbr01_radiometer_1_sig',data)-self.getdata('uppbbr01_radiometer_1_zero',data))*c[1]*corr
    def upper_pyranometer_red_flux(self,data):
        c=self.cals['CAL082']
        corr=self.getdata('pyranometer_correction',data)
        #return (self.getdata('uppbbr01_radiometer_2_sig',data)-self.getdata('uppbbr01_radiometer_2_zero',data))*c*corr
        return (self.getdata('uppbbr01_radiometer_2_sig',data) - self.getdata('uppbbr01_radiometer_2_zero',data))*c[1]*corr

    def upper_pyrgeometer_flux(self,data):
        c=self.cals['CAL083']
        ct=self.cals['CAL089']
        rt=self.getdata('uppbbr01_radiometer_3_temp',data)*ct[1]+ct[0]
        rs=(self.getdata('uppbbr01_radiometer_3_sig',data)-self.getdata('uppbbr01_radiometer_3_zero',data))*c[1]
        uir=5.899E-8*(rt+273.16)**4+rs
        return uir

    def lower_pyranometer_clear_flux(self,data):
        c=self.cals['CAL091']
        return (self.getdata('lowbbr01_radiometer_1_sig',data)-self.getdata('lowbbr01_radiometer_1_zero',data))*c[1]
    def lower_pyranometer_red_flux(self,data):
        c=self.cals['CAL092']
        return (self.getdata('lowbbr01_radiometer_2_sig',data)-self.getdata('lowbbr01_radiometer_2_zero',data))*c[1]
    def lower_pyrgeometer_flux(self,data):
        c=self.cals['CAL093']
        ct=self.cals['CAL099']
        t=self.getdata('lowbbr01_radiometer_3_temp',data)*ct[1]+ct[0]
        s=(self.getdata('lowbbr01_radiometer_3_sig',data)-self.getdata('lowbbr01_radiometer_3_zero',data))*c[1]
        uir=5.899E-8*(rt+273.16)**4+rs
        return uir

    def gin_latitude(self,data):
        return self.getdata('gindat01_latitude_gin',data)
    def gin_longitude(self,data):
        return self.getdata('gindat01_longitude_gin',data)
    def gin_altitude(self,data):
        return self.getdata('gindat01_altitude_gin',data)
    def gin_n_velocity(self,data):
        return self.getdata('gindat01_velocity_north_gin',data)
    def gin_e_velocity(self,data):
        return self.getdata('gindat01_velocity_east_gin',data)
    def gin_d_velocity(self,data):
        return self.getdata('gindat01_velocity_down_gin',data)
    def gin_roll(self,data):
        return self.getdata('gindat01_roll_gin',data)
    def gin_pitch(self,data):
        return self.getdata('gindat01_pitch_gin',data)
    def gin_heading(self,data):
        return self.getdata('gindat01_heading_gin',data)
    def gin_track_angle(self,data):
        return self.getdata('gindat01_track_gin',data)
    def gin_speed(self,data):
        return self.getdata('gindat01_speed_gin',data)
    def gin_rate_about_long(self,data):
        return self.getdata('gindat01_rate_about_long_gin',data)
    def gin_rate_about_trans(self,data):
        return self.getdata('gindat01_rate_about_trans_gin',data)
    def gin_rate_about_down(self,data):
        return self.getdata('gindat01_rate_about_down_gin',data)
    def gin_acc_long(self,data):
        return self.getdata('gindat01_long_accel_gin',data)
    def gin_acc_trans(self,data):
        return self.getdata('gindat01_trans_accel_gin',data)
    def gin_acc_down(self,data):
        return self.getdata('gindat01_down_accel_gin',data)

    """
        Calculation of Equation of Tome and Solar declination per second..
    
    def angle_from_solstice(self,data):
        # Calculate angle from the solstice for equation of time and declination, per second is it necessary ?
        import datetime
        Dfrac=self.getdata('Time',data)/86400.0  
        D = datetime.datetime.now().timetuple().tm_yday - 1 +Dfrac  
        W=360.0/365.24 # Orbital velocity degrees per day
        # tilt 23.44 deg = 0.4091 rad
        tilt=np.deg2rad(23.44) # obliquity (tilt) of the Earth's axis in degrees
        A=W*(D+10)    # Add approximate days from Solstice to Jan 1 ( 10 ) 
        # 2 is the number of days from January 1 to the date of the Earth's perihelion
        # Earth's orbital eccentricity, 0.0167
        # B=A+(360/np.pi)*0.0167*np.sin(np.deg2rad(W*(D-2))) simplifies to..
        B=np.deg2rad(A+1.914*np.sin(np.deg2rad(W*(D-2)))) # 
        return B 

    def equation_of_time(self,data):
        # Calculate equation of time per second ...
        B=self.getdata('angle_from_solstice',data)
        C=(A-np.rad2deg(np.arctan(np.tan(B)/np.cos(tilt))))/180.0
        return (720*(C-np.around(C)))/4.0 # Convert from minutes(time) to degrees(angle) 4 degrees per minute
        
    def solar_declination(self,data):
        # Calculate equation of time per second ...
        B=self.getdata('angle_from_solstice',data)
        return -np.arcsin(np.sin(tilt)*np.cos(B)) # In radians """

    def solar_zenith_angle(self,data):
        Decl=self.getdata('Solar_declination',data) # radians
        Tcorr=self.getdata('Equation_of_time',data) # degrees
        Latrad=np.deg2rad(self.getdata('gin_latitude',data))  # radians
        Londeg=self.getdata('gin_longitude',data) # degrees
        Timedeg=self.getdata('time_since_midnight',data)/240.0  # 86400 secs = 24 hrs = 360 degrees
        Angrad=np.deg2rad((Timedeg+Tcorr+180.+Londeg) % 360)  #  radians
        # CALCULATE SOLAR ZENITH ANGLE
        Zen=np.rad2deg(np.arccos(np.sin(Decl)*np.sin(Latrad)+np.cos(Decl)*np.cos(Latrad)*np.cos(Angrad)))
        return Zen

    def solar_azimuth_angle(self,data):
        Decl=self.getdata('Solar_declination',data) # radians
        Tcorr=self.getdata('Equation_of_time',data) # degrees
        Latrad=np.deg2rad(self.getdata('gin_latitude',data))  # radians
        Londeg=self.getdata('gin_longitude',data) # degrees
        Timedeg=self.getdata('time_since_midnight',data)/240.0  #  86400 secs = 24 hrs = 360 degrees
        Angrad=np.deg2rad((Timedeg+Tcorr+180.+Londeg) % 360)  #  radians
        # CALCULATE SOLAR AZIMUTH ANGLE
        Azim=180.+np.rad2deg(np.arctan2((np.cos(Decl)*np.sin(Angrad)),
                                        (np.cos(Decl)*np.cos(Angrad)*np.sin(Latrad) - np.sin(Decl)*np.cos(Latrad))
                            )          )
        return Azim

    def gin_northwards_wind_component(self,data):
        TAS=self.getdata('true_air_speed_ms',data)
        head=np.deg2rad(self.getdata('gin_heading',data)-self.cals['ginhead_corr'])
        north=self.getdata('gin_n_velocity',data)
        pitch=np.deg2rad(self.getdata('gin_pitch',data))
        return north-TAS*np.cos(head)/np.cos(pitch)
        
    def gin_eastwards_wind_component(self,data):
        TAS=self.getdata('true_air_speed_ms',data)
        head=np.deg2rad(self.getdata('gin_heading',data)-self.cals['ginhead_corr'])
        east=self.getdata('gin_e_velocity',data)
        pitch=np.deg2rad(self.getdata('gin_pitch',data))
        return east-TAS*np.sin(head)/np.cos(pitch)
        
    def gin_wind_angle(self,data):
        north=self.getdata('gin_northwards_wind_component',data)
        east=self.getdata('gin_eastwards_wind_component',data)
        return np.rad2deg(np.arctan2(-east,-north)) % 360
        
    def gin_wind_speed(self,data): # m/s
        north=self.getdata('gin_northwards_wind_component',data)
        east=self.getdata('gin_eastwards_wind_component',data)
        return (north*north+east*east)**0.5
 
    def n_wind(self,data):
        TAS=self.getdata('turb_probe_tas',data)
        head=np.deg2rad(self.getdata('gin_heading',data))
        north=self.getdata('gin_n_velocity',data)
        AOA=np.deg2rad(self.getdata('angle_of_attack',data))
        AOSS=np.deg2rad(self.getdata('angle_of_sideslip',data))
        roll=np.deg2rad(self.getdata('gin_roll',data))
        pitch=np.deg2rad(self.getdata('gin_pitch',data))
        grad=np.deg2rad(self.getdata('gin_rate_about_down',data))
        grat=np.deg2rad(self.getdata('gin_rate_about_trans',data))
        RTA=np.tan(AOA)
        RTS=np.tan(AOSS)
        RSR=np.sin(roll)
        RCR=np.cos(roll)
        RSP=np.sin(pitch)
        RCP=np.cos(pitch)
        RSH=np.sin(head)
        RCH=np.cos(head)
        RV1=RTA*RCR-RTS*RSR
        RV2=RCP+RSP*RV1
        RV3=RTA*RSR+RTS*RCR
        RV4=RCP*grad
        RV5=RSP*grat
        RIP=15.49                                  #Vanes to INU distance (m)
        # V     - Northwards wind component (m s-1)
        RV=north-TAS*(RSH*RV3+RCH*RV2)-RIP*(RSH*RV4+RCH*RV5) #N wind (m s-1)        
        return RV
        
    def e_wind(self,data):
        TAS=self.getdata('turb_probe_tas',data)
        head=np.deg2rad(self.getdata('gin_heading',data))
        east=self.getdata('gin_e_velocity',data)
        AOA=np.deg2rad(self.getdata('angle_of_attack',data))
        AOSS=np.deg2rad(self.getdata('angle_of_sideslip',data))
        roll=np.deg2rad(self.getdata('gin_roll',data))
        pitch=np.deg2rad(self.getdata('gin_pitch',data))
        grad=np.deg2rad(self.getdata('gin_rate_about_down',data))
        grat=np.deg2rad(self.getdata('gin_rate_about_trans',data))
        RTA=np.tan(AOA)
        RTS=np.tan(AOSS)
        RSR=np.sin(roll)
        RCR=np.cos(roll)
        RSP=np.sin(pitch)
        RCP=np.cos(pitch)
        RSH=np.sin(head)
        RCH=np.cos(head)
        RV1=RTA*RCR-RTS*RSR
        RV2=RCP+RSP*RV1
        RV3=RTA*RSR+RTS*RCR
        RV4=RCP*grad
        RV5=RSP*grat
        RIP=15.49                                  #Vanes to INU distance (m)
        # U     - Eastwards wind component (m s-1)
        RU=east+TAS*(RCH*RV3-RSH*RV2)+RIP*(RCH*RV4-RSH*RV5) #E wind (m s-1)
        return RU

    def v_wind(self,data):
        TAS=self.getdata('turb_probe_tas',data)
        down=self.getdata('gin_d_velocity',data)
        AOA=np.deg2rad(self.getdata('angle_of_attack',data))
        AOSS=np.deg2rad(self.getdata('angle_of_sideslip',data))
        roll=np.deg2rad(self.getdata('gin_roll',data))
        pitch=np.deg2rad(self.getdata('gin_pitch',data))
        grat=np.deg2rad(self.getdata('gin_rate_about_trans',data))
        RTA=np.tan(AOA)
        RCR=np.cos(roll)
        RTS=np.tan(AOSS)
        RSR=np.sin(roll)
        RTP=np.tan(pitch)
        RV1=RTA*RCR-RTS*RSR
        RIP=15.49                                  #Vanes to INU distance (m)
        RCP=np.cos(pitch)
        RW=-down+RCP*(TAS*(RV1-RTP)+RIP*grat)  # V wind(m s-1)
        return RW

    def wind_angle(self,data):
        north=self.getdata('n_wind',data)
        east=self.getdata('e_wind',data)
        return np.rad2deg(np.arctan2(-east,-north)) % 360
        
    def wind_speed(self,data): # m/s
        north=self.getdata('n_wind',data)
        east=self.getdata('e_wind',data)
        return (north*north+east*east)**0.5
 
    def pyranometer_correction(self,data):
        #Upper pyranometer corrections
        SZEN=np.deg2rad(self.getdata('solar_zenith_angle',data))          
        SAZI=np.deg2rad(self.getdata('solar_azimuth_angle',data))
        head=np.deg2rad(self.getdata('gin_heading',data))
        SHDG=head-SAZI                         #!!!Get quadrants right ?
        ROLL=np.deg2rad(self.getdata('gin_roll',data))
        PTCH=np.deg2rad(self.getdata('gin_pitch',data))
        R=(np.sin(SZEN)*np.sin(SHDG)*np.sin(ROLL) -
           np.cos(SHDG)*np.sin(PTCH)*np.cos(ROLL)*np.sin(SZEN) +
           np.cos(SZEN)*np.cos(PTCH)*np.sin(ROLL))
        CORR=np.cos(SZEN)/R
        return CORR


    def solar_albedo(self,data):
        low=self.getdata('lower_pyranometer_clear_flux',data)
        upp=self.getdata('upper_pyranometer_clear_flux',data)
        return low/upp

    def near_infrared_albedo(self,data):
        low=self.getdata('lower_pyranometer_red_flux',data)
        upp=self.getdata('upper_pyranometer_red_flux',data)
        return low/upp

    def lower_visible_flux(self,data):
        clr=self.getdata('lower_pyranometer_clear_flux',data)
        red=self.getdata('lower_pyranometer_red_flux',data)
        return clr-red

    def upper_visible_flux(self,data):
        clr=self.getdata('upper_pyranometer_clear_flux',data)
        red=self.getdata('upper_pyranometer_red_flux',data)
        return clr-red

    def visible_albedo(self,data):
        low=self.getdata('lower_visible_flux',data)
        upp=self.getdata('upper_visible_flux',data)
        return low/upp

    def net_infra_red_flux(self,data):
        low=self.getdata('lower_pyrgeometer_flux',data)
        upp=self.getdata('upper_pyrgeometer_flux',data)
        return low-upp

    def upper_near_infra_red_fraction(self,data):
        red=self.getdata('upper_pyranometer_red_flux',data)
        clr=self.getdata('upper_pyranometer_clear_flux',data)
        return red/clr

    def lower_near_infra_red_fraction(self,data):
        red=self.getdata('lower_pyranometer_red_flux',data)
        clr=self.getdata('lower_pyranometer_clear_flux',data)
        return red/clr


    def ten_m_wind_speed(self,data):
        """ 10MWS - 10m NEUTRAL STABILITY WINDS (m s-1)
            This is iterative so maybe a bad idea !"""
        ws=self.getdata('wind_speed',data)
        phgt=self.getdata('pressure_height_m',data)
        tenmws=np.zeros(len(ws))
        i1=np.where((phgt>1)&(ws>0))
        VK=0.40                                  #Von Karman's constant
        EPS=0.005                                #Required fit
        USTAR=np.zero(len(ws))
        USTAR[i1]=0.3                             #Surface friction velocity m/s
        US=0.0
        MAXIT=30                                 #Max iterations
        for i in range(MAXIT):
            ind=np.where(USTAR>US)               
            US=USTAR
            Z0=0.3905E-4/USTAR[ind]+1.604E-3*USTAR[ind]*USTAR[ind]-0.017465E-2 #Pierson model
            USTAR[ind]=VK*wa[ind]/np.log(phgt[ind]/Z0)
        tenmws[i1]=ws[i1]+USTAR[i1]*np.log(10.0/phgt[i1])/VK
        return tenmws
    
    
    def refractivity(self,data):
        tatdi=self.getdata('deiced_true_air_temp_k',data)
        tatdi[tatdi==0]=1.0
        spr=self.getdata('static_pressure',data)
        vp=self.getdata('vapour_pressure',data)
        N=((77.6/tatdi)*(spr+(4810*vp/tatdi)))
        return N

    def refractive_index(self,data):
        ri=(self.getdata('refractivity',data))/1e6+1
        ri[ri==0]=1.0
        return ri
  
    def lifting_condensation_level(self,data):
        phgt=self.getdata('pressure_height_m',data)  
        tatdi=self.getdata('deiced_true_air_temp_k',data)  
        dp=self.getdata('dew_point',data)+273.16
        return phgt+((tatdi-dp)*125.0)  
 
    def theta_w(self,data):
        """Theta W , from a 3rd order least squares fit with theta E"""
        pote=self.getdata('potential_temperature',data)   
        return -917.7114+pote*10.119819-pote*pote*2.89312109e-02+pote*pote*pote*2.83998353e-5

    def cabin_pressure(self,data):
        """Cabin pressure (mb)"""
        c=self.cals['CAL014']
        raw=self.getdata('corcon01_cabin_p',data)
        return c[2]*raw**2+c[1]*raw+c[0]

    def cabin_temperature(self,data):
        """Cabin pressure (mb)"""
        c=self.cals['CAL207']
        raw=self.getdata('corcon01_cabin_t',data)
        return c[1]*raw+c[0]

    def heimann_surface_temp(self,data):
        """HEIM  - Heimann surface temperature (deg C)"""
        raw=self.getdata('corcon01_heim_t',data)
        c=self.cals['CAL141']
        return c[0]+c[1]*raw
       
    def corrected_surface_temp(self,data):
        heim=self.getdata('heimann_surface_temp',data)
        """
        What should we do - this is the fortran CODE...
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
      DERIVE(ISEC,78)=RST                        !Corr. surface temp (C)"""
        return heim

    def nevzorov_liquid_water(self,data):
        icol=self.getdata('corcon01_nv_lwc_icol',data)    
        vcol=self.getdata('corcon01_nv_lwc_vref',data)    
        iref=self.getdata('corcon01_nv_lwc_icol',data)    
        vref=self.getdata('corcon01_nv_lwc_vref',data)
        tas=self.getdata('true_air_speed_ms',data)
        c=self.cals['CAL208']   
        power=(icol-iref)*(vcol-vref)
        nvlwc=power/tas/2589/c
        """IF(TAS.GT.0..AND.CAL(208,3).NE.0.) 
                NVLWC=RL**2/TAS/2589/CAL(208,3)"""
        return nvlwc
        
    def nevzorov_toal_water(self,data):
        icol=self.getdata('corcon01_nv_twc_icol',data)    
        vcol=self.getdata('corcon01_nv_twc_vref',data)    
        iref=self.getdata('corcon01_nv_twc_icol',data)    
        vref=self.getdata('corcon01_nv_twc_vref',data)
        tas=self.getdata('true_air_speed_ms',data)
        c=self.cals['CAL211']
        power=(icol-iref)*(vcol-vref)
        nvtwc=power/tas/2589/c
        """IF(TAS.GT.0..AND.CAL(211,3).NE.0.) 
                NVTWC=RT**2/TAS/2589/CAL(208,3)"""
        return nvtwc
        
    def neph_pressure(self,data):
        raw=self.getdata('aerack01_csv:neph_pressure',data)
        c=self.cals['CAL175']
        return c[0]+c[1]*raw 

    def neph_temperature(self,data):
        raw=self.getdata('aerack01_csv:neph_temperature',data)
        c=self.cals['CAL176']
        return c[0]+c[1]*raw 

    def neph_blue_sp(self,data):
        raw=self.getdata('aerack01_csv:neph_total_blue',data)
        c=self.cals['CAL177']
        rv=c[0]+c[1]*raw
        return (10**((rv/c[3])-c[2])-c[4])*1E6

    def neph_green_sp(self,data):
        raw=self.getdata('aerack01_csv:neph_total_green',data)
        c=self.cals['CAL178']
        rv=c[0]+c[1]*raw
        return (10**((rv/c[3])-c[2])-c[4])*1E6

    def neph_red_sp(self,data):
        raw=self.getdata('aerack01_csv:neph_total_red',data)
        c=self.cals['CAL179']
        rv=c[0]+c[1]*raw
        return (10**((rv/c[3])-c[2])-c[4])*1E6

    def neph_blue_bsp(self,data):
        raw=self.getdata('aerack01_csv:neph_backscatter_blue',data)
        c=self.cals['CAL180']
        rv=c[0]+c[1]*raw
        return (10**((rv/c[3])-c[2])-c[4])*1E6

    def neph_green_bsp(self,data):
        raw=self.getdata('aerack01_csv:neph_backscatter_green',data)
        c=self.cals['CAL181']
        rv=c[0]+c[1]*raw
        return (10**((rv/c[3])-c[2])-c[4])*1E6

    def neph_red_bsp(self,data):
        raw=self.getdata('aerack01_neph_backscatter_red',data)
        c=self.cals['CAL182']
        rv=c[0]+c[1]*raw
        return (10**((rv/c[3])-c[2])-c[4])*1E6

    def neph_pressure(self,data):
        raw=self.getdata('aerack01_csv:neph_humidity',data)
        c=self.cals['CAL183']
        return c[0]+c[1]*raw 

    def neph_humidity(self,data):
        raw=self.getdata('aerack01_csv:neph_status',data)
        c=self.cals['CAL184']
        return c[0]+c[1]*raw 

    def psap_lin_abs_coeff(self,data):
        raw=self.getdata('aerack01_csv:psap_lin',data)
        c=self.cals['CAL185']
        return c[0]+c[1]*raw 

    def psap_log_abs_coeff(self,data):
        raw=self.getdata('aerack01_csv:psap_log',data)
        c=self.cals['CAL186']
        return c[0]+c[1]*raw 

    def psap_transmittance(self,data):
        raw=self.getdata('aerack01_csv:psap_transmission',data)
        c=self.cals['CAL187']
        return c[0]+c[1]*raw 

    def teco_ozone_mixing_ratio(self,data):
        raw=self.getdata('CHEM:teco_ozone',data)  # What raw signal ?
        c=self.cals['CAL100']
        return c[0]+c[1]*raw 
        
    def aqd_no(self,data):
        raw=self.getdata('CHEM:aqdno',data)  # What raw signal ?
        c=self.cals['CAL203']
        return c[0]+c[1]*raw 

    def aqd_no2(self,data):
        raw=self.getdata('CHEM:aqdno2',data)  # What raw signal ?
        c=self.cals['CAL204']
        return c[0]+c[1]*raw 

    def aqd_nox(self,data):
        raw=self.getdata('CHEM:aqdnox',data)  # What raw signal ?
        c=self.cals['CAL205']
        return c[0]+c[1]*raw 

    def teco_so2(self,data):
        raw=self.getdata('CHEM:teco_so2',data)  # What raw signal ?
        c=self.cals['CAL214']
        return c[0]+c[1]*raw 
        
    def co_mixing_ratio(self,data):
        raw=self.getdata('CHEM:co',data)  # What raw signal ?
        c=self.cals['CAL154']
        return c[0]+c[1]*raw 

    def time_since_midnight(self,data):
        """ Is this the best place to get time - is there not time in a master time rather than ubber bbr time ? """
        raw=self.getdata('corcon01_utc_time',data) #unixtimestamp
        unixtime_at_midnight = time.mktime(datetime.now().timetuple()[0:3]+(0,0,0,0,0,0))
        #raw is an array, so subtracting an integer appears to be valid
        return raw - unixtime_at_midnight
        
    def derindex(self,data):
      return self.getdata('id',data)
        
