#!/usr/bin/python
#Class to read CSV protocol descriptions and present them to Python
# vim: tabstop=8 expandtab shiftwidth=3 softtabstop=3
import os, glob, csv
from twisted.python import log 
import time
import psycopg2
import re

class DecadesDataProtocols():
   location = "/opt/decades/dataformats/"
   protocols = {} #Dictionary of protocols
   tables = {} #list of protocol_name to current tablename
   protocol_versions = {} #Dictionary of protocol:version pairs. Version is mtime at present
   field_types_map = {'boolean':'boolean', 'signed_int':'integer', 'single_float':'real', 'double_float':'real','float':'real', 'text':'varchar', 'unsigned_int':'integer'} # maps CSV protocol file "types" to PostgreSQL field types (postgreSQL does not have unsigned values)
   new_table_count = 0
   
   def __init__(self):
      dirList=glob.glob(os.path.join(self.location,'*.csv'))
      for proto_path_name in dirList:
         protocol_file_name = os.path.basename(proto_path_name)
         self.protocols[protocol_file_name[0:-4]] = [] #[0:-4] strips the '.csv. off the end
         full_path = os.path.join(self.location,protocol_file_name)
         self.protocol_versions[protocol_file_name[0:-4]] = str(os.stat(full_path)[9]) #we're just after the integer - dots are not allowed in psql tablenames
         with open(full_path, 'rb') as protocol_file:
            protocolReader = csv.DictReader(open(full_path, 'rb'))
            for row in protocolReader:
               self.protocols[protocol_file_name[0:-4]].append({k:element.strip() for k, element in row.iteritems()})

   def available(self):
      return self.protocols.keys()

   def create_table(self, protocol_name, cursor, suffix='_test'):
      #returns a suitable (Postgres)SQL CREATE TABLE command for a named protocol
      tablename = (protocol_name.lower() + suffix)
      s = 'CREATE TABLE %s (' % tablename
      self.tables[protocol_name] = tablename
      for field in self.protocols[protocol_name]:
         #created postgres field spec. Strips leading $ from field name as it won't work
         s = s + " ".join([field['field'].lstrip('$'),self.field_types_map[field['type'].lstrip('<>')],',']) #strips the endianness indicator off, if it is present.

      s = s.rstrip(',') + ")"
      create_index_query = "CREATE INDEX %s_time_index ON %s (utc_time)" % (tablename, tablename)
      #check if table exists (can't use IF NOT EXISTS until postgres 9.1)
      cursor.execute("select exists(select * from information_schema.tables where table_name=%s)", (tablename,))
      if cursor.fetchone()[0]:
         #exists
         log.msg('Table %s exists' % tablename)
         return True
      else:
         #doesn't exist, create table
         log.msg('Creating table %s' % tablename)
         log.msg(cursor.mogrify(s))
         cursor.execute(s)
         self.new_table_count += 1 #increment new table
         log.msg('Creating index %s_time_index' % tablename)
         cursor.execute(create_index_query)
         return cursor.connection.commit()

   def reuse_table(self,cursor):
      now=int(time.time())
      try:
          cursor.execute("SELECT utc_time FROM mergeddata ORDER BY utc_time DESC LIMIT 1")
          lasttime=cursor.fetchone()[0]
          if((now-lasttime)>3600):
              self.clear_table(cursor)
          else:
              log.msg('Reusing existing mergeddata table')
      except (TypeError):
          #cursor.connection.rollback()
          self.clear_table(cursor)

   def get_comment(self,cursor):
      cursor.execute("SELECT description FROM pg_description WHERE objoid='mergeddata'::regclass;")
      return cursor.fetchall()[0][0]

   def add_comment(self,cursor,comment):
      self.tstart=int(time.time())
      cursor.execute("COMMENT ON TABLE mergeddata IS '%s'" % comment)

   def clear_table(self,cursor):
      log.msg('Truncating mergeddata table')
      cursor.execute("TRUNCATE mergeddata")

   def empty_table(self,cursor):
      log.msg('Deleting contents of mergeddata table')
      cursor.execute("DELETE FROM mergeddata *")


   def create_maintable(self, cursor):
      '''creates a SQL table of all the data from the 
      protocol files. '''
      fields = []
      all_fields = self.all_fields()
      for each in all_fields.keys():
         fields.append(each + ' ' + all_fields[each])
      squirrel = "CREATE TABLE mergeddata ( "+ ', '.join(fields) +')' 
      log.msg(cursor.mogrify(squirrel)) 
      cursor.execute('DROP TABLE IF EXISTS mergeddata')
      cursor.execute(squirrel)
      cursor.execute('ALTER TABLE mergeddata ADD COLUMN utc_time INT PRIMARY KEY')
      #cursor.execute('ALTER TABLE mergeddata ADD COLUMN id INT UNIQUE')
      #cursor.execute('CREATE index ID ON mergeddata (utc_time)')
      cursor.execute('''CREATE OR REPLACE FUNCTION data_merge(sql_update TEXT, sql_insert TEXT) RETURNS VOID AS
$$
DECLARE r INTEGER;
BEGIN
   -- first try to insert and after to update. Note : insert has pk and update not...
  LOOP
    EXECUTE sql_update; 
    GET DIAGNOSTICS r = ROW_COUNT;
    IF r = 1 THEN 
        raise notice '%', r;
        RETURN; 
    END IF;
    BEGIN
      EXECUTE sql_insert;
      RETURN;
      EXCEPTION WHEN unique_violation THEN
         --do nothing and loop
    END;
  END LOOP;
END;
$$
LANGUAGE plpgsql;''')


   def add_data(self, cursor, data, instrument):
      '''adds incoming data to the database. data is a Python 
         dictionary of fieldname=> value pairs'''

      sql_u = []
      sql_i = [[],[]]
      for each in data.iteritems():
         if each[1]=='':
            sql_u.append(cursor.mogrify(instrument+'_'+each[0]+'=%s',(None,))) 
            sql_i[0].append(instrument+'_'+each[0])
            sql_i[1].append(None)
         else:
            sql_u.append(cursor.mogrify(instrument+'_'+each[0]+'=%s',(each[1],))) 
            sql_i[0].append(instrument+'_'+each[0])
            sql_i[1].append(each[1])

      if (instrument=='corcon01'):
          cursor.execute("SELECT corcon01_flight_num FROM mergeddata WHERE corcon01_utc_time IS NOT NULL ORDER BY utc_time DESC LIMIT 1")
          flight_num=cursor.fetchone()
          if(flight_num):
              if (data['flight_num']!=flight_num[0]) & (re.match('^[A-Za-z]\d{3}$',data['flight_num'])!=None):
                  log.msg("SHOULD MAKE A NEW TABLE !!!!")
                  self.empty_table(cursor)
      sql_update = ('UPDATE mergeddata SET ' + ", ".join(sql_u) + ' WHERE utc_time=' + cursor.mogrify('%s', (data['utc_time'],)))
      #sql_insert = (cursor.mogrify('INSERT INTO mergeddata (' + ", ".join(sql_i[0]) + ',id, utc_time) VALUES (' + ('%s,' * len(sql_i[1])) + '%s,%s)',sql_i[1] +[data['utc_time'],data['utc_time']]))
      sql_insert = (cursor.mogrify('INSERT INTO mergeddata (' + ", ".join(sql_i[0]) + ', utc_time) VALUES (' + ('%s,' * len(sql_i[1])) + '%s)',sql_i[1] +[data['utc_time']]))
      cursor.execute('SELECT data_merge($$' + sql_update + '$$, $$'+ sql_insert + '$$)')
   

   def fields(self, protocol_name):
      '''returns a List of field names for a given protocol'''
      r = []
      for field in self.protocols[protocol_name]:
         r.append(field['field'].lstrip('$').strip())

      return r

   def all_fields(self):
      '''returns a dictionary of all currently-defined fields'''
      fields= {}
      for proto in self.protocols:
         protoname = self.protocols[proto][0]['field'].lstrip('$')
         for field in self.protocols[proto]: 
            value = protoname.lower()+'_'+field['field'].lstrip('$').lower().strip()
            field_type = self.field_types_map[field['type'].lstrip('<>')]
            fields[value] =  field_type

      return fields
