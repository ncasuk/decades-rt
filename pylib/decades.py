#!/usr/bin/python
#Class to read CSV protocol descriptions and present them to Python
import os, glob, csv

class DecadesDataProtocols():
   location = "/opt/decades/dataformats/"
   protocols = {} #Dictionary of protocols
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
      for field in self.protocols[protocol_name]:
         #created postgres field spec. Strips leading $ from field name as it won't work
         s = s + " ".join([field['field'].lstrip('$'),self.field_types_map[field['type']],','])

      s = s.rstrip(',') + ")"
      cursor.execute(s)
      return cursor.connection.commit()

   def fields(self, protocol_name):
      #returns a List of field names
      r = []
      for field in self.protocols[protocol_name]:
         r.append(field['field'].lstrip('$'))

      return r
