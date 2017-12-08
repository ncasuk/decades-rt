#!/usr/bin/env python
'''Setup script for the pyDecades python package, required for DECADES'''
from setuptools import setup
import os
#read current version number
versionfile = open(os.path.join('VERSION'))
version= versionfile.read().strip()

setup(name='pydecades',
      version=version,
      description='Python DECADES functions',
      author='Dan Walker & Dave Tiddeman',
      author_email='daniel.walker@ncas.ac.uk',
      packages=['pydecades', 'pydecades.rt_calcs'],
     )
