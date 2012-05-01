import numpy as np
import rt_data

class derived(rt_data.rt_data):
    """ A collection of the processing routines for realtime in flight data """
    def pressure_height_feet(self,data):
        rvsm_alt=self.getdata('PRTAFT01.pressure_alt',data)
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
    def indicated_air_speed(self,data):
        rvsm_ias=self.getdata('PRTAFT01.ind_air_speed',data)
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
        raw=self.getdata('CORCON01.s9_press',data)
        #c=self.cals['CAL221']
        c=self.getdata('CAL221',data)
        return c[0]+c[1]*raw+c[2]*raw**2
    def turb_probe_pitot_static(self,data):
        c=self.cals['CAL215']
        raw=self.getdata('CORCON01.tp_p0_s10',data)
        return c[0]+c[1]*raw+c[2]*raw**2
    def turb_probe_attack_diff(self,data):
        c=self.cals['CAL216']
        raw=self.getdata('CORCON01.tp_up_down',data)
        return c[0]+c[1]*raw+c[2]*raw**2
    def turb_probe_sideslip_diff(self,data):
        c=self.cals['CAL217']
        raw=self.getdata('CORCON01.tp_left_right',data)
        return c[0]+c[1]*raw+c[2]*raw**2
    def turb_probe_attack_check(self,data):
        c=self.cals['CAL218']
        raw=self.getdata('CORCON01.tp_top_s10',data)
        return c[0]+c[1]*raw+c[2]*raw**2
    def turb_probe_sideslip_check(self,data):
        c=self.cals['CAL219']
        raw=self.getdata('CORCON01.tp_right_s10',data)
        return c[0]+c[1]*raw+c[2]*raw**2
    def deiced_indicated_air_temp_c(self,data):
        c=self.cals['CAL010']
        raw=self.getdata('CORCON01.di_temp',data)
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
        raw=self.getdata('CORCON01.ndi_temp',data)
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
        ias=self.getdata('indicated_air_speed',data) 
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
        raw=self.getdata('CORCON01.ge_dew',data)
        c=self.cals['CAL058']
        hycc=self.getdata('CORCON01.ge_cont',data)
        cc=np.where((hycc>18076) | (hycc<15451)) # not sure what to do with this info ( control lost )
        return raw*c[1]+c[0]

    def relative_humidity(self,data):
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
        return rh

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
        """ Equivalent Portntial Temperature K"""
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
        raw=self.getdata('CORCON01.jw_lwc',data)
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
        
    def radar_height(self,data):
        """Radar height (ft)"""
        return self.getdata('PRTAFT01.rad_alt',data)/4.0

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
        c=self.cals['CAL081']
        return (self.getdata('UPPBBR01.radiometer_1_sig',data)-self.getdata('UPPBBR01.radiometer_1_zero',data))*c
    def upper_pyranometer_red_flux(self,data):
        c=self.cals['CAL082']
        return (self.getdata('UPPBBR01.radiometer_2_sig',data)-self.getdata('UPPBBR01.radiometer_2_zero',data))*c
    def upper_pyrgeometer_flux(self,data):
        c=self.cals['CAL083']
        return (self.getdata('UPPBBR01.radiometer_3_sig',data)-self.getdata('UPPBBR01.radiometer_3_zero',data))*c

    def lower_pyranometer_clear_flux(self,data):
        c=self.cals['CAL091']
        return (self.getdata('LOWBBR01.radiometer_1_sig',data)-self.getdata('LOWBBR01.radiometer_1_zero',data))*c
    def lower_pyranometer_red_flux(self,data):
        c=self.cals['CAL092']
        return (self.getdata('LOWBBR01.radiometer_2_sig',data)-self.getdata('LOWBBR01.radiometer_2_zero',data))*c
    def lower_pyrgeometer_flux(self,data):
        c=self.cals['CAL093']
        return (self.getdata('LOWBBR01.radiometer_3_sig',data)-self.getdata('LOWBBR01.radiometer_3_zero',data))*c

    def gin_latitude(self,data):
        return self.getdata('PRTAFT01.gin_lat',data)
    def gin_longitude(self,data):
        return self.getdata('PRTAFT01.gin_long',data)
    def gin_altitude(self,data):
        return self.getdata('PRTAFT01.gin_alt',data)
    def gin_n_velocity(self,data):
        return self.getdata('PRTAFT01.gin_north_vel',data)
    def gin_e_velocity(self,data):
        return self.getdata('PRTAFT01.gin_east_vel',data)
    def gin_d_velocity(self,data):
        return self.getdata('PRTAFT01.gin_down_vel',data)
    def gin_roll(self,data):
        return self.getdata('PRTAFT01.gin_roll',data)
    def gin_pitch(self,data):
        return self.getdata('PRTAFT01.gin_pitch',data)
    def gin_heading(self,data):
        return self.getdata('PRTAFT01.gin_heading',data)
    def gin_track_angle(self,data):
        return self.getdata('PRTAFT01.gin_track',data)
    def gin_speed(self,data):
        return self.getdata('PRTAFT01.gin_speed',data)
    def gin_rate_about_long(self,data):
        return self.getdata('PRTAFT01.gin_roll_rate',data)
    def gin_rate_about_trans(self,data):
        return self.getdata('PRTAFT01.gin_pitch_rate',data)
    def gin_rate_about_down(self,data):
        return self.getdata('PRTAFT01.gin_heading_rate',data)
    def gin_acc_long(self,data):
        return self.getdata('PRTAFT01.gin_accel_fwd',data)
    def gin_acc_trans(self,data):
        return self.getdata('PRTAFT01.gin_accel_strbrd',data)
    def gin_acc_down(self,data):
        return self.getdata('PRTAFT01.gin_accel_dwn',data)

    def solar_zenith(self,data):
        Decl=self.cals['Solar_declination'] # radians
        Tcorr=self.cals['Equation_of_time'] # degrees
        Latrad=np.deg2rad(self.getdata('gin_latitude',data))  # radians
        Londeg=self.getdata('gin_longitude',data) # degrees
        Timedeg=self.getdata('Time',data)/240.0  # 86400 secs = 24 hrs = 360 degrees
        Angrad=np.deg2rad((Timedeg+Tcorr+180.+Londeg) % 360)  #  radians
        # CALCULATE SOLAR ZENITH ANGLE
        Zen=np.rad2deg(np.arccos(np.sin(Decl)*np.sin(Latrad)+np.cos(Decl)*np.cos(Latrad)*np.cos(Angrad)))
        return Zen

    def solar_azimuth(self,data):
        Decl=self.cals['Solar_declination'] # radians
        Tcorr=self.cals['Equation_of_time'] # degrees
        Latrad=np.deg2rad(self.getdata('gin_latitude',data))  # radians
        Londeg=self.getdata('gin_longitude',data) # degrees
        Timedeg=self.getdata('Time',data)/240.0  #  86400 secs = 24 hrs = 360 degrees
        Angrad=np.deg2rad((Timedeg+Tcorr+180.+Londeg) % 360)  #  radians
        # CALCULATE SOLAR AZIMUTH ANGLE
        Azim=180.+np.rad2deg(np.arctan2((np.cos(Decl)*np.sin(Angrad)),
                                        (np.cos(Decl)*np.cos(Angrad)*np.sin(Latrad) - np.sin(Decl)*np.cos(Latrad))
                            )          )
        return Azim
    
"""

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
"""
