# vim: tabstop=8 expandtab shiftwidth=3 softtabstop=3
import numpy as np
from twisted.python import log
from pydecades.configparser import DecadesConfigParser
import time
data_types_numpy = {'boolean':'bool', 'integer':'int', 'real':'float', 'character varying':'U13'} # postgres "types" to Numpy array types

class rt_data(object):
    """ Class to read extract data from database and perform calibrations for display
        all the actual algorithms are in :class:`pydecades.rt_calcs.rt_derive.derived`"""
    def __init__(self,database,calfile='HOR_CALIB.DAT', config=DecadesConfigParser()):
        """ Initialise with database and reading in calibration constants""" 
        der=[]
        for d in dir(self):
            if d not in dir(rt_data):
                der.append(d)

        #Get config details
        self.config = config
        self.derived=der   # list of derivations, empty unless subclassed
        self.database=database #python Cursor class (Named Tuple version)
        self.database.execute("SELECT column_name,data_type FROM information_schema.columns WHERE table_name='mergeddata'")
        self.columns={}
        for name,dt in self.database:
            self.columns[name]=data_types_numpy[dt]
        self.read_cal_const(calfile)
        self.status=rt_status()
        
    def get_status(self):
        return self.status.checkStatus(self)
    
    def get_json_status(self):
        return self.get_status().json()

    def get_packed_status(self):
        return self.get_status().packed()
    
    def get_available(self):
        ans=[]
        for para in self.derived:
            rawset,rawmissing=self.get_raw_required([para])
            if(len(rawmissing)==0):
                ans.append(para)
        return ans

    def get_paranos(self):
        #get derived params
        avail=self.get_available()
        lines={}
        for p in avail:
            lines[p]=dict(zip(['ParameterIdentifier','DisplayText','DisplayUnits','GroupId']
                              ,self.__getattribute__(p).__doc__.split(",")))

        #get params from Display Parameters file unless already specified
        params = self.config.items('Parameters')
        for k,v in params:
           if not v in lines:
              lines[v] = {'ParameterIdentifier': k}

        return lines

    def get_raw_paranos(self):
        lines={}
        for i,p in enumerate(self.columns):
            lines[p]=dict(zip(['ParameterIdentifier','DisplayText','DisplayUnits','GroupId']
                              ,[i,' '.join(p.upper().split('_')),'raw','raw']))
        return lines


    def derive_data(self,names,where='',order='',rawdata=None):
        """Read in data and process in one go, using repeated database queries
           ( must be sure that the selection doesn't vary )
         :param names: list(-like) of strings
         :param where: str, SQL ``WHERE`` clause
         :param order: str, SQL ``ORDER BY`` clause, not including ``ORDER BY utc_time`` so should be somthing of the form ``DESC LIMIT 1``

         :returns: data values.
         :rtype: Dictionary with keys being names parameter
         """
        ans={}
        if(rawdata==None):
            rawdata={}
        for name in names:
            ans[name]=self.getdata(name,(rawdata,(where,order)))
        return ans

    def derive_data_alt(self,names,where='',order=''):
        """Alternative read in data and process.  Separated so that only one database query
           ( Goes through the process twice - first with empty data array, then reads all raw data before the second pass)"""
        rawset,rawmissing=self.get_raw_required(names)
        rawdata={}
        for name in rawmissing:
            rawdata[name]=np.array([])
        rawdata.update(self.getbunchofdata_fromdatabase(rawset,where, order))
        return self.derive_data(names,where,rawdata=rawdata)

    def get_raw_required(self,names):
        """ Work out which raw data needed by trying to process with empty data array"""
        rawset=set()
        rawmissing=set()
        for name in names:
            self.getdata(name,(rawset,))
        for r in list(rawset):
            if((r in self.derived) or (r in self.cals)):
                rawset.remove(r)
            elif(r not in self.columns):
                rawset.remove(r)
                rawmissing.add(r)
        return rawset,rawmissing

    def getdata(self,name,data):
        """ Main routine for extracting data - decides whether it is 

            * Already available
            * Is a calibration constant
            * Needs processing
            * Needs to be extracted from database
            
            """
        try:
            #use defined types. if known
            empty_array = np.array([], dtype=self.columns[name])
        except KeyError:
            #defaults to float64
            empty_array = np.array([])

        try:
            if(data[0].has_key(name)):
                return data[0][name]
            elif(self.cals.has_key(name)):
                return self.cals[name]
            else:    
                if(name in self.derived):
                    try:
                        data[0][name]=self.__getattribute__(name)(data)
                    except:  # probably mismatched arrays
                        data[0][name]=empty_array
                else:
                    if(name in self.columns):
                        data[0][name]=self.getdata_fromdatabase(name,*data[1]) 
                    else:
                        data[0][name]=empty_array
                return data[0][name]               
        except AttributeError:
            # This is for the dummy run which puts the needed raw data into a set
            if(name in data[0]):
                return empty_array
            elif(name in self.cals):
                return self.cals[name]
            elif(name in self.derived):
                data[0].update([name])
                try:
                    return self.__getattribute__(name)(data)
                except:
                    log.err()
                    pass #some basic error
            else:
                data[0].update([name])
                return empty_array
 
    def getdata_fromdatabase(self,name,where='',order='',table='mergeddata'):
        """ Reads one parameter from database"""
        fieldname_part = 'SELECT %s ' % name 
        ord='ORDER BY utc_time '+order
        if(where):
            selection='WHERE '+where
        else:
            selection=''
        self.database.execute('%s FROM %s %s %s' % (fieldname_part, table, selection , ord) )
        data=np.array(self.database.fetchall())
        try:
            data=data.astype(self.columns[name])
        except TypeError:
            try:
                data=data.astype('float')
            except TypeError:
                data=data.astype('str')
        data=np.reshape(data,-1)
        return data
            
    def getbunchofdata_fromdatabase(self,names,where='',order='',table='mergeddata'):
        """ Reads several parameters from database

        :param names: list of strings of required fields
        :param where: SQL ``WHERE`` clause (not including ``WHERE`` keyword)
        :param order: SQL clause to follow ``ORDER BY utc_time``, i.e. ``DESC`` or ``ASC``
        :param table: SQL table to query. Defaults to ``mergeddata``, and unlikely to need to be otherwise"""
        t1=time.time()
        log.msg('Updating data for ' + repr(names))
        fieldname_part = 'SELECT %s ' % ', '.join(names)
        ord='ORDER BY utc_time '+order
        if(where):
            selection='WHERE '+where
        else:
            selection=''        
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
        self.database.execute(fieldname_part + ('FROM %s %s %s')% (table, selection , ord) )
        ans={}
        dty=[]
        fetched=self.database.fetchall()
        if(fetched):
            dt=[]
            for name in names:
                dt.append((name,self.columns[name]))            
            try:
                data=np.array(fetched,dtype=dt)
                log.msg('Data ' + repr(data))
            except TypeError:
                try:
                    for i,d in enumerate(dt):
                        if(d[1]=='int'):
                            dt[i]=(d[0],'float')
                    data=np.array(fetched,dtype=dt)
                except TypeError:
                    for i,d in enumerate(dt):
                        if(d[1]=='bool'):
                            dt[i]=(d[0],'float')
                    data=np.array(fetched,dtype=dt)

            for name in names:
                ans[name]=data[name]
        else:
            for name in names:
                ans[name]=np.array([])
               
        log.msg('Fetched data ' + repr(fetched))
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
        try:
            self.database.execute("SELECT utc_time FROM mergeddata ORDER BY utc_time ASC LIMIT 1")
            utc=self.database.fetchone()
            self.cals['MIDNIGHT']=86400*(utc.utc_time/86400)
        except Exception as e:
            self.cals['MIDNIGHT']=time.mktime(datetime.datetime.utcnow().timetuple()[0:3]+(0,0,0,0,0,0))
        return

    def read_cal_const(self,filename):
        """ Reads in the constants file.

            All calibrations must be named in 6 characters starting CAL
            The current file format then has a single digit which tells how many numbers there are (*n*)
            Followed by *n* comma seperated constants and an optional comment."""
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
        paras=['utc_time','time_since_midnight',
               'pressure_height_kft','static_pressure',
               'gin_heading','gin_latitude','gin_longitude',
               'true_air_speed','deiced_true_air_temp_c',
               'gin_wind_speed','wind_angle','dew_point','flight_number']
        for p in paras:
            self[p]=float('NaN')
        self['flight_number']='####'
        self['utc_time']=0
        self.struct_fmt = ">bii11f4s"
        self.output_format = "{0:.2f}" #Format string for those output variables that are displayed unmodified in STAT lines 2 d.p at present

    def packed(self):
        import struct
        return struct.pack(self.struct_fmt,
               1,
               self['utc_time'],
               self['utc_time'],
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

    def json(self):
        import json
        return json.dumps(self)

    def checkStatus(self,rtlib,oldestdata=10):
        """ Updates once there is a new index (id ) """
        derind=rtlib.getdata_fromdatabase('utc_time','utc_time > %i' % self['utc_time'],'DESC LIMIT 1')
        if(derind):
            rawdata={}
            if(not(hasattr(self,'rawset'))):
                self.rawset,self.rawmissing=rtlib.get_raw_required(self.keys())
                self.rawset.remove('utc_time')
            r='utc_time'
            rawdata[r]=rtlib.getdata_fromdatabase(r,'%s IS NOT NULL' % r,'DESC LIMIT 1')
            t=rawdata[r][0]-oldestdata # Oldest data to display 10 secs ago.
            for r in self.rawset:
                rawdata[r]=rtlib.getdata_fromdatabase(r,'%s IS NOT NULL AND utc_time>%i' % (r,t),'DESC LIMIT 1')
            newdata=rtlib.derive_data(self.keys(),'','DESC LIMIT 1',rawdata=rawdata)
            for k in self.keys():
                try:
                    self[k]=float(newdata[k][0])
                except ValueError:
                    self[k]=newdata[k][0]
                except IndexError:
                    if(k=='flight_number'):
                        self[k]='####'
                    elif(k!='utc_time'):
                        self[k]=float('NaN')
        return self
