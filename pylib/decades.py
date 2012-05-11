#!/usr/bin/python
#Class to read CSV protocol descriptions and present them to Python
import os, glob, csv

class DecadesDataProtocols():
   location = "/opt/decades/dataformats/"
   protocols = {} #Dictionary of protocols
   tables = {} #list of protocol_name to current tablename
   protocol_versions = {} #Dictionary of protocol:version pairs. Version is mtime at present
   field_types_map = {'boolean':'boolean', 'signed_int':'integer', 'single_float':'real', 'double_float':'real', 'text':'varchar', 'unsigned_int':'int'} # maps CSV protocol file "types" to PostgreSQL field types (postgreSQL does not have unsigned values
   
   def __init__(self):
      dirList=glob.glob(os.path.join(self.location,'*.csv'))
      for proto_path_name in dirList:
         protocol_file_name = os.path.basename(proto_path_name)
         self.protocols[protocol_file_name[0:-4]] = [] #[0:-4] strips the '.csv. off the end
         full_path = os.path.join(self.location,protocol_file_name)
         self.protocol_versions[protocol_file_name[0:-4]] = str(os.stat(full_path)[9]) #we're just after the integer - dots are not allowed in psql tablenames
         protocolReader = csv.DictReader(open(full_path, 'rb'))
         for row in protocolReader:
            self.protocols[protocol_file_name[0:-4]].append(row)

   def available(self):
      return self.protocols.keys()

   def create_table(self, protocol_name, cursor, suffix='test_'):
      #returns a suitable (Postgres)SQL CREATE TABLE command for a named protocol
      s = 'CREATE TABLE %s (' % (protocol_name + suffix)
      self.tables[protocol_name] = protocol_name.lower() + suffix
      for field in self.protocols[protocol_name]:
         #created postgres field spec. Strips leading $ from field name as it won't work
         s = s + " ".join([field['field'].lstrip('$'),self.field_types_map[field['type']],','])

      s = s.rstrip(',') + ")"
      #check if table exists (can't use IF NOT EXISTS until postgres 9.1)
      cursor.execute("select exists(select * from information_schema.tables where table_name=%s)", (protocol_name.lower() + suffix,))
      if cursor.fetchone()[0]:
         #exists
         print 'Table %s exists' % (protocol_name.lower() + suffix,)
         return True
      else:
         #doesn't exist, create table
         print 'Creating table %s' % (protocol_name.lower() + suffix,)
         cursor.execute(s)
         return cursor.connection.commit()

   def create_view(self):
      '''creates a SQL VIEW (pseudo-table) of all the data from the 
      protocol files. uses LEFT JOIN so data can be missing and it
      will still work (should have NULL for missing data)'''
      squirrel = "CREATE VIEW scratchdata (" 
      select_fields = []
      for proto in self.protocols:
         protoname = self.protocols[proto][0]['field'].lstrip('$')
         fields = []
         for field in self.protocols[proto][1:]: #skip 1st one, instrument name
            fields.append('"'+protoname+'.'+field['field'].lstrip('$')+'" ')
            select_fields.append(self.tables[protoname]+'.'+field['field'].lstrip('$'))
         squirrel = squirrel + ', '.join(fields) + ','
      squirrel = squirrel.rstrip(',') + ') AS SELECT ' + ", ".join(select_fields)+' FROM '
      table_list = self.tables.items()
      join_clause = table_list[0][1] + ' '
      for table in table_list[1:]: #joins all to the first table
         join_clause = join_clause + " ".join([' LEFT JOIN',table[1],'ON (',table_list[0][1]+'.utc_time=',table[1] + '.utc_time)'])

      print squirrel + join_clause
   

   def fields(self, protocol_name):
      #returns a List of field names
      r = []
      for field in self.protocols[protocol_name]:
         r.append(field['field'].lstrip('$'))

      return r
