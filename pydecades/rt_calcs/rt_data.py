import numpy as np
from twisted.python import log

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
        
    def derive_data(self,names,selection,rawdata=None):
        """Read in data and process in one go, using repeated database queries
           ( must be sure that the selection doesn't vary )"""
        ans={}
        if(rawdata==None):
            rawdata={}
        for name in names:
            #ans[name]=self.getdata(name,(rawdata,selection))
            ans[name]=self.getdata(name,rawdata)
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
        return rawset

    def getdata(self,name,data):
        """ Main routine for extracting data - decides whether it is 
            . Already available
            . Is a calibration constant
            . Needs processing
            . Needs to be extracted from database"""
        try:
            if(data.has_key(name)):
                return data[name]
            elif(self.cals.has_key(name)):
                return self.cals[name]
            else:    
                if(name in self.derived):
                    data[name]=eval('self.'+name+'(data)')
                    #print 'Derive '+name
                else:
                    data[name]=self.getdata_fromdatabase(name,data[1]) 
                return data[name]               
        except AttributeError:
            # This is for the dummy run which puts the needed raw data into a set
            if(name in self.derived):
                return eval('self.'+name+'(data)')
            elif(name in self.cals):
                return self.cals[name]
            else:
                #data[0].update([name])
                data[0].update({name:[]})
                return np.array([])
 
    def getdata_fromdatabase(self,name,selection):
        """ Reads one parameter from database"""
        fieldname_part = 'SELECT %s ' % name 
        self.database.execute(fieldname_part + 'FROM mergeddata WHERE id %s AND %s IS NOT NULL ORDER BY id' % (selection, name) )
        #print self.database.query
        data[name] = []
        for record in self.database: #iterates over results 
            data[name].append(getattr(record,name))
        return np.array(data[name],dtype='float')
            
    def getbunchofdata_fromdatabase(self,names,selection,order=' ORDER BY id'):
        """ Reads several parameters from database"""
        fieldname_part = 'SELECT %s ' % ', '.join(names)
        #gets a set of the "names" list's entry's first 8 characters
        #sets are unique so removes duplicates
        instruments = set([s[0:8] for s in filter(lambda a: a[9:] != 'flight_num',filter(lambda b: b[9:] != 'utc_time',filter(lambda c: c != 'utc_time',names)))]) 
        instruments.discard('id') # don't need that one, it's not an instrument
        #if the instrument is returning data <instrumentname>_utc_time will not be null
        not_null_part = ''
        if len(instruments) >0:
            not_null_part = ' AND %s' % '_utc_time IS NOT NULL AND '.join(instruments) + '_utc_time IS NOT NULL'
        #self.database.execute(fieldname_part + ('FROM mergeddata WHERE id %s AND ' + ' IS NOT NULL AND '.join(names) + ' IS NOT NULL ')% selection, )
        #log.msg(fieldname_part + ('FROM mergeddata WHERE id %s %s %s')% (selection, not_null_part, order) )
        self.database.execute(fieldname_part + ('FROM mergeddata WHERE id %s %s %s')% (selection, not_null_part, order) )
        ans={}
        data={}
        for name in names:
            data[name] = []
        for record in self.database: #iterates over results 
            for name in names:
               data[name].append(getattr(record,name))
        for name in names:
            try:
               ans[name]=np.array(data[name],dtype='float')
            except ValueError:
               #can't cast to float, presumably string
               ans[name] = data[name]
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
        
