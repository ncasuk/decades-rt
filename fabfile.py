#!/usr/bin/env python
# -*- coding: utf-8 -*-
# fabfile for Django:
# http://morethanseven.net/2009/07/27/fabric-django-git-apache-mod_wsgi-virtualenv-and-p/
# modified for fabric 0.9/1.0
#from __future__ import with_statement # needed for python 2.5
from fabric.api import *
import time, os, glob

# globals
env.prj_name = 'decades' # no spaces!
env.sudoers_group = 'wheel'
env.webserver = 'apache2' # nginx or apache2 (directory name below /etc!)
env.dbserver = 'postgresql' # mysql or postgresql
env.timestamp = time.strftime('%Y%m%d%H%M%S')
env.dchopts = '--snapshot'
if os.environ.has_key('RELEASE') and os.environ['RELEASE']:
   env.dchopts = '--release'

@runs_once
def list_hosts():
   print env.hosts

def _annotate_hosts_with_ssh_config_info():
    '''deals with hosts specified in ~/.ssh/config - shouldn't be needed
      after fabric 1.4 '''
    from os.path import expanduser
    try: 
      from ssh.config import SSHConfig
    except ImportError: 
      from paramiko.config import SSHConfig

    def hostinfo(host, config):
        hive = config.lookup(host)
        if 'hostname' in hive:
            host = hive['hostname']
        if 'user' in hive:
            host = '%s@%s' % (hive['user'], host)
        if 'port' in hive:
            host = '%s:%s' % (host, hive['port'])
        return host

    try:
        config_file = file(expanduser('~/.ssh/config'))
    except IOError:
        pass
    else:
        config = SSHConfig()
        config.parse(config_file)
        keys = [config.lookup(host).get('identityfile', None)
            for host in env.hosts]
        env.key_filename = [expanduser(key) for key in keys if key is not None]
        env.hosts = [hostinfo(host, config) for host in env.hosts]

        for role, rolehosts in env.roledefs.items():
            env.roledefs[role] = [hostinfo(host, config) for host in rolehosts]

_annotate_hosts_with_ssh_config_info()

def setup():
   '''ab initio setup'''
   sudo('apt-get -y install aptitude')
   sudo('aptitude -y install postgresql') 
   with settings(warn_only=True): #already-exists errors ignored
      sudo('psql -c "CREATE ROLE inflight UNENCRYPTED PASSWORD \'wibble\' SUPERUSER CREATEDB CREATEROLE INHERIT LOGIN;"',user="postgres")
      sudo('createdb -O inflight inflightdata', user="postgres")
      sudo('createlang plpgsql inflightdata',user="postgres")
   
@runs_once
def create_deb():
   #local('tar zcv --transform=\'s$pylib$/opt/decades/pylib$\' -f %(prj_name)s-%(timestamp)s.orig.tar.gz pylib' % env)
   #local('tar zcv --transform=\'s$Horace$/opt/decades/Horace$\' -f %(prj_name)s-%(timestamp)s.orig.tar.gz Horace' % env)
   local('mkdir %(prj_name)s-%(timestamp)s' % env)
   local('git checkout-index --prefix=%(prj_name)s-%(timestamp)s/ -a' % env)
   local('git-dch %(dchopts)s --auto --git-author' % env) #adds latest commit details to a snapshot version
   local('cp -rp debian %(prj_name)s-%(timestamp)s/' % env)
   with lcd('%(prj_name)s-%(timestamp)s' % env):
      #debuild_out = local('git-buildpackage --git-upstream-branch=master --git-debian-branch=master --git-export=INDEX --git-ignore-new' % env, capture=True)
      debuild_out = local('debuild -us -uc' % env, capture=True)
      print debuild_out
      debuild_outlist = debuild_out.splitlines()
      for line in debuild_outlist:
         if "dpkg-deb: building package " in line:
            packagename = line.split('/')[1][:-2] #extract the .deb filename
   #local('rm -rf %(prj_name)s-%(timestamp)s.orig.tar.gz %(prj_name)s-%(timestamp)s' % env )
   print packagename
   return packagename
   
def deploy_deb(debname=False):
   if debname:
      put(debname)
      #installs all dependencies
      sudo('aptitude -y install `dpkg --info %s | grep Depends | awk -F ":" \'{print $2}\' | sed -e "s/,/ /g"`' % debname)
      sudo('dpkg -i %s' % debname) 
   else:
      print('No deb filename specified')

def test():
   '''runs all the unit tests'''
   local('trial pylib')

def unit_test_parameter(paramname):
   '''runs a unit test for a single parameter, e.g vertical_vorticity. 
   Usage: fab unit_test_parameter:<parametername>'''
   local('trial pylib.test.test_decades_server.DecadesProtocolTestCase.test_%s' % paramname)

def deploy():   
   Plot_jar()
   debname=create_deb()
   deploy_deb(debname=debname)
   #sudo('a2enmod speling')
   sudo('service apache2 restart')

def clean():
   local('find . -maxdepth 1 -name \*.tar.gz -exec rm {} \;')
   local('find . -maxdepth 1 -name \*.deb -exec rm {} \;')
   local('find . -maxdepth 1 -name \*.dsc -exec rm {} \;')
   local('find . -maxdepth 1 -name \*.build -exec rm {} \;')
   local('find . -maxdepth 1 -name \*.changes -exec rm {} \;')
   local(' rm -rf decades-20[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]')

def Plot_jar():
   '''Creates the JAR file for the display applicaton'''
   with lcd('Horace/web/plot/plot'):
      local('make jar && cp Plot.jar ..') 
