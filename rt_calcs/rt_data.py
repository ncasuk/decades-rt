import numpy as np

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
        self.database=database
        self.read_cal_const(calfile)
        
    def derive_data(self,names,selection,rawdata=None):
        """Read in data and process in one go, using repeated database queries
           ( must be sure that the selection doesn't vary )"""
        ans={}
        if(rawdata==None):
            rawdata={}
        for name in names:
            ans[name]=self.getdata(name,(rawdata,selection))
        return ans

    def derive_data_alt(self,names,selection):
        """Alternative read in data and process.  Separated so that only one database query
           ( Goes through the process twice - first with empty data array, then reads all raw data
             before the second pass)"""
        rawset=self.get_raw_required(names)
        rawdata=self.getbunchofdata_fromdatabase(rawset,selection)
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
            if(data[0].has_key(name)):
                return data[0][name]
            elif(self.cals.has_key(name)):
                return self.cals[name]
            else:    
                if(name in self.derived):
                    data[0][name]=eval('self.'+name+'(data)')
                    print 'Derive '+name
                else:
                    data[0][name]=self.getdata_fromdatabase(name,data[1]) 
                return data[0][name]               
        except AttributeError:
            # This is for the dummy run which puts the needed raw data into a set
            if(name in self.derived):
                return eval('self.'+name+'(data)')
            elif(name in self.cals):
                return self.cals[name]
            else:
                data[0].update([name])
                return np.array([])
 
    def getdata_fromdatabase(self,name,selection):
        """ Dummy routine to read one parameter from database"""
        print 'SELECT ' + name + ' FROM scratchdata WHERE id '+selection
        return np.array(self.database,dtype='float')
            
    def getbunchofdata_fromdatabase(self,names,selection):
        """ Dummy routine to read several parameters from database"""
        print 'SELECT ' + ', '.join(names) + ' FROM scratchdata WHERE id '+selection
        ans={}
        for name in names:
            ans[name]=np.array(self.database,dtype='float')
        return ans
                    
    def read_cal_const(self,filename):
        """ Reads in the constants file
            All calibrations must be named in 6 characters starting CAL
            The current file format then has a single digit which tells how many numbers there are (n)
            Followed by n comma seperated constants and an optional comment."""
        f=open(filename)
        self.cals={}
        # Calculater equation of time and declination, as if constant for this day
        import datetime
        D = datetime.datetime.now().timetuple().tm_yday - 1
        W=360.0/365.24
        # tilt 23.44 deg = 0.4091 rad
        tilt=0.4091
        A=W*(D+10)
        B=np.deg2rad(A+1.914*np.sin(np.deg2rad(W*(D-2)))) # B=A+(360/np.pi)*0.0167*np.sin(np.deg2rad(W*(D-2))) 
        C=(A-np.rad2deg(np.arctan(np.tan(B)/np.cos(tilt))))/180.0
        self.cals['Equation_of_time']=(720*(C-np.around(C)))/4.0 # Convert from minutes(time) to degrees(angle) 4 degrees per minute
        self.cals['Solar_declination']=-np.arcsin(np.sin(tilt*np.cos(B))) # In radians
        for line in f:
            if line.startswith('CAL'):
                ncal=int(line[7])
                s=line[8:].split(',')
                self.cals[line[:6]]=tuple(float(c) for c in s[:ncal])
        