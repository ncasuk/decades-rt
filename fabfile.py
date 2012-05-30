#!/usr/bin/env python
# -*- coding: utf-8 -*-
# fabfile for Django:
# http://morethanseven.net/2009/07/27/fabric-django-git-apache-mod_wsgi-virtualenv-and-p/
# modified for fabric 0.9/1.0
#from __future__ import with_statement # needed for python 2.5
from fabric.api import *
import time, os

# globals
env.prj_name = 'decades' # no spaces!
env.sudoers_group = 'wheel'
env.webserver = 'apache2' # nginx or apache2 (directory name below /etc!)
env.dbserver = 'postgresql' # mysql or postgresql
if os.environ.has_key('HOSTS'):
   env.hosts = os.environ['HOSTS'].split(',')
else:
   env.hosts = ['septic','fish']

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
   sudo('psql -c "CREATE ROLE inflight UNENCRYPTED PASSWORD \'wibble\' SUPERUSER CREATEDB CREATEROLE INHERIT LOGIN;"',user="postgres")
   sudo('createdb -O inflight inflightdata', user="postgres")
   

def create_deb():
   env.release = time.strftime('%Y%m%d%H%M%S')
   local('tar zcv --transform=\'s$pylib$/opt/decades/pylib$\' -f %(prj_name)s-%(release)s.orig.tar.gz pylib' % env)
   local('mkdir %(prj_name)s-%(release)s' % env)
   local('git checkout-index --prefix=%(prj_name)s-%(release)s/ -a' % env)
   local('git-dch -S --auto --git-author') #adds latest commit details to a snapshot version
   local('cp -rp debian %(prj_name)s-%(release)s/' % env)
   with lcd('%(prj_name)s-%(release)s' % env):
      local('debuild -us -uc' % env)
   local('rm -rf %(prj_name)s-%(release)s.orig.tar.gz %(prj_name)s-%(release)s' % env )
   
def deploy_deb(debname=False):
   if debname:
      put(debname)
      sudo('aptitude -y install `dpkg --info %s | grep Depends | awk -F ":" \'{print $2}\' | sed -e "s/,/ /g"`' % debname)
      sudo('dpkg -i %s' % debname) 
   else:
      print('No deb filename specified')
     
