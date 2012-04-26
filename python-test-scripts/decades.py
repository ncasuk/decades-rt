#!/usr/bin/python
#Class to read CSV protocol descriptions and present them to Python
import os, csv

class DecadesDataProtocols():
   location = "/opt/decades/"
   protocols = {} #Dictionary of protocols
   field_types_map = {'boolean':'boolean', 'signed_int':'integer', 'single_float':'real', 'double_float':'real', 'text':'varchar', 'unsigned_int':'int'} # maps CSV protocol file "types" to PostgreSQL field types (postgreSQL does not have unsigned values
   
   def __init__(self):
      dirList=os.listdir(self.location)
      for protocol_name in dirList:
         print protocol_name
         self.protocols[protocol_name[0:-4]] = [] #[0:-4] strips the '.csv. off the end
         protocolReader = csv.DictReader(open(os.path.join(self.location,protocol_name), 'rb'))
         for row in protocolReader:
            self.protocols[protocol_name[0:-4]].append(row)

   def available(self):
      return self.protocols.keys()

   def create_table(self, protocol_name, cursor, prefix='test_'):
      #returns a suitable (Postgres)SQL CREATE TABLE command for a named protocol
      s = 'CREATE TABLE %s (' % (prefix + protocol_name)
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
