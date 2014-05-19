import numpy as np
from twisted.python import log
import time

class rt_data(object):
    """ Class to read extract data from database and perform calibrations for display
        all the actual algorithms are in rt_derive.py"""
    def __init__(self,database,calfile='HOR_CALIB.DAT'):
        """ Initialise with database and reading in calibration constants""" 
        der=[]
        for d in dir(self):
            if d not in dir(rt_data):
                der.append(d)
        self.derived=der   # list of derivations, empty unless subclassed
        self.database=database #python Cursor class (Named Tuple version)
        self.database=database
        self.read_cal_const(calfile)
        
    def derive_data(self,names,selection,order=" ORDER BY id",rawdata=None):
        """Read in data and process in one go, using repeated database queries
           ( must be sure that the selection doesn't vary )"""
        ans={}
        if(rawdata==None):
            rawdata={}
        for name in names:
            ans[name]=self.getdata(name,(rawdata,(selection,order)))
            #ans[name]=self.getdata(name,rawdata)
        return ans

    def derive_data_alt(self,names,selection,order=" ORDER BY id"):
        """Alternative read in data and process.  Separated so that only one database query
           ( Goes through the process twice - first with empty data array, then reads all raw data
             before the second pass)"""
        rawset=self.get_raw_required(names)
        rawdata=self.getbunchofdata_fromdatabase(rawset,selection, order)
        return self.derive_data(names,selection,rawdata=rawdata)

    def get_raw_required(self,names):
        """ Work out which raw data needed by trying to process with empty data array"""
        rawset=set()
        for name in names:
            self.getdata(name,(rawset,))
        for r in list(rawset):
            if((r in self.derived) or (r in self.cals)):
                rawset.remove(r)
        return rawset

    def getdata(self,name,data):
        """ Main routine for extracting data - decides whether it is 
            . Already available
            . Is a calibration constant
            . Needs processing
            . Needs to be extracted from database"""
        try:
            if(data[0].has_key(name)):
                return data[0][name]
            elif(self.cals.has_key(name)):
                return self.cals[name]
            else:    
                if(name in self.derived):
                    try:
                        data[0][name]=self.__getattribute__(name)(data)
                    except IndexError:  # probably mismatched arrays
                        data[0][name]=np.array([])
                else:
                    data[0][name]=self.getdata_fromdatabase(name,*data[1]) 
                return data[0][name]               
        except AttributeError:
            # This is for the dummy run which puts the needed raw data into a set
            if(name in data[0]):
                return np.array([])
            elif(name in self.cals):
                return self.cals[name]
            elif(name in self.derived):
                data[0].update([name])
                return self.__getattribute__(name)(data)
            else:
                data[0].update([name])
                return np.array([])
 
    def getdata_fromdatabase(self,name,selection,order=' ORDER BY id'):
        """ Reads one parameter from database"""
        fieldname_part = 'SELECT %s ' % name 
        #instrument = (filter(lambda a: a[9:] != 'flight_num',
        #             filter(lambda b: b[9:] != 'utc_time',
        #             filter(lambda c: c != 'utc_time',
        #             filter(lambda d: d != 'id',[name])))))
        #not_null_part = ' AND %s IS NOT NULL' % name
        #if(instrument):
        #    not_null_part = ' AND %s_utc_time IS NOT NULL' % instrument[0][0:8]
        self.database.execute('%s FROM mergeddata WHERE id %s %s' % (fieldname_part, selection , order) )
        data=np.reshape(np.array(self.database.fetchall()),-1)
        if(data.dtype=='O'):
            try:
                data=data.astype('float')
            except ValueError:
                data=data.astype('str')
        return data
            
    def getbunchofdata_fromdatabase(self,names,selection,order=' ORDER BY id'):
        """ Reads several parameters from database"""
        t1=time.time()
        fieldname_part = 'SELECT %s ' % ', '.join(names)
        #gets a set of the "names" list's entry's first 8 characters
        #sets are unique so removes duplicates
        instruments = set([s[0:8] for s in 
                              filter(lambda a: a[9:] != 'flight_num',
                              filter(lambda b: b[9:] != 'utc_time',
                              filter(lambda c: c != 'utc_time',names)))]) 
        instruments.discard('id') # don't need that one, it's not an instrument
        #if the instrument is returning data <instrumentname>_utc_time will not be null
        not_null_part = ''
        if len(instruments) >0:
            not_null_part = ' AND %s' % '_utc_time IS NOT NULL AND '.join(instruments) + '_utc_time IS NOT NULL'
        self.database.execute(fieldname_part + ('FROM mergeddata WHERE id %s %s %s')% (selection, not_null_part, order) )
        ans={}
        dty=[]
        fetched=self.database.fetchall()
        if(fetched):
            for i,n in zip(fetched[0],names):
                dt=np.array(i).dtype
                if(dt=='O'):dt='f'
                dty.append((n,dt))
            ansarr=np.array(fetched,dty)
            for name in names:
                ans[name]=ansarr[name]
        else:
            for name in names:
                ans[name]=np.array([])
               
        return ans
                    
    def constants_not_in_file(self):
        # Calculate equation of time and declination, as if constant for this day
        import datetime
        D = datetime.datetime.now().timetuple().tm_yday - 1
        W=360.0/365.24 # Orbital velocity degrees per day
        # tilt 23.44 deg = 0.4091 rad
        tilt=np.deg2rad(23.44) # obliquity (tilt) of the Earth's axis in degrees
        A=W*(D+10)    # Add approximate days from Solstice to Jan 1 ( 10 ) 
        # 2 is the number of days from January 1 to the date of the Earth's perihelion
        # Earth's orbital eccentricity, 0.0167
        # B=A+(360/np.pi)*0.0167*np.sin(np.deg2rad(W*(D-2))) simplifies to..
        B=np.deg2rad(A+1.914*np.sin(np.deg2rad(W*(D-2)))) # 
        C=(A-np.rad2deg(np.arctan(np.tan(B)/np.cos(tilt))))/180.0
        self.cals['Equation_of_time']=(720*(C-np.around(C)))/4.0 # Convert from minutes(time) to degrees(angle) 4 degrees per minute
        self.cals['Solar_declination']=-np.arcsin(np.sin(tilt)*np.cos(B)) # In radians
        self.cals['ginhead_corr']=0.45
        self.cals['CALAOSS']=[-2.1887E-02,0.0000E-00,0.0000E0,5.7967E-02,-1.7229E-02,0.0000E0,0.9505E+0,0.0050E+0]
        self.cals['CALAOA']=[3.35361E-01,2.78277E-01,-5.73689E-01,-6.1619E-02,-5.2595E-02,1.0300E-01,1.0776E+0,-0.4126E+0]
        self.cals['CALTAS']=[0.9984E0]
        return

    def read_cal_const(self,filename):
        """ Reads in the constants file
            All calibrations must be named in 6 characters starting CAL
            The current file format then has a single digit which tells how many numbers there are (n)
            Followed by n comma seperated constants and an optional comment."""
        f=open(filename)
        self.cals={}
        self.constants_not_in_file()
        for line in f:
            if line.startswith('CAL'):
                ncal=int(line[7])
                s=line[8:].split(',')
                self.cals[line[:6]]=tuple(float(c) for c in s[:ncal])
        


class rt_status(dict):
    def __init__(self):
        dict.__init__(self,{})
        self.paras=['derindex','utc_time','time_since_midnight',
               'pressure_height_kft','static_pressure',
               'gin_heading','gin_latitude','gin_longitude',
               'true_air_speed','deiced_true_air_temp_c',
               'gin_wind_speed','wind_angle','dew_point','flight_number']
        for p in self.paras:
            self[p]=float('NaN')
        self['flight_number']='####'
        self['derindex']=0
        self.struct_fmt = ">bii11f4s"
        self.output_format = "{0:.2f}" #Format string for those output variables that are displayed unmodified in STAT lines 2 d.p at present
        self.derindex=0
        self.prttime=0
        self.gintime=0
        self.cortime=0

    def packed(self):
        import struct
        return struct.pack(self.struct_fmt,
               1,
               self['derindex'],
               self['derindex'],
	       self['time_since_midnight'],
	       self['gin_heading'],
	       self['static_pressure'],
	       self['pressure_height_kft'],
	       self['true_air_speed'],
	       float(self.output_format.format(self['deiced_true_air_temp_c'])),
	       float(self.output_format.format(self['dew_point'])),
	       self['gin_wind_speed'],
	       self['wind_angle'],
	       self['gin_latitude'],
	       self['gin_longitude'],
	       self['flight_number'])

    def checkStatus(self,rtlib,oldestdata=10):
        """ Updates once there is a new index (id ) """
        derind=rtlib.getdata_fromdatabase('id','>%i' % self['derindex'],'ORDER BY id DESC LIMIT 1')
        if(derind):
            rawdata={}
            if(not(hasattr(self,'rawset'))):
                self.rawset=rtlib.get_raw_required(self.paras)
                self.rawset.remove('utc_time')
            r='utc_time'
            rawdata[r]=rtlib.getdata_fromdatabase(r,'=id AND %s IS NOT NULL' % r,'ORDER BY id DESC LIMIT 1')
            t=rawdata[r][0]-oldestdata # Oldest data to display 10 secs ago.
            for r in self.rawset:
                rawdata[r]=rtlib.getdata_fromdatabase(r,'=id AND %s IS NOT NULL AND utc_time>%i' % (r,t),'ORDER BY id DESC LIMIT 1')
            newdata=rtlib.derive_data(self.paras,'=id','ORDER BY id DESC LIMIT 1',rawdata=rawdata)
            for k in self.paras:
                try:
                    self[k]=float(newdata[k][0])
                except ValueError:
                    self[k]=newdata[k][0]
                except IndexError:
                    if(k=='flight_number'):
                        self[k]='####'
                    elif(k!='derindex'):
                        self[k]=float('NaN')
        return self
 