#!/usr/bin/env python
# vim: tabstop=8 expandtab shiftwidth=4 softtabstop=4
'''deals with the DECADES config files'''

from ConfigParser import SafeConfigParser
import csv

class DecadesConfigParser(SafeConfigParser):
    def __init__(self, files=['/etc/decades/decades.ini','decades.ini']):
        SafeConfigParser.__init__(self)
        self.read(files)
        #get the Parameters
        parameters_file = self.get('Config','parameters_file')
        with open(parameters_file, 'r') as csvfile:
            self.add_section('Parameters')
            parameters = csv.DictReader(csvfile)    #uses first line as fieldnames
            for line in parameters:
                self.set('Parameters',line['ParameterIdentifier'],line['ParameterName'].strip())
