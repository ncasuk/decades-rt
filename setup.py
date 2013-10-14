#!/usr/bin/env python
'''Setup script for the pyDecades python package, required for DECADES'''
from setuptools import setup

setup(name='pydecades',
      version='0.7.3',
      description='Python DECADES functions',
      author='Dan Walker & Dave Tiddeman',
      author_email='daniel.walker@ncas.ac.uk',
      packages=['pydecades', 'pydecades.rt_calcs'],
     )
