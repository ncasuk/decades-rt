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

@hosts('')
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
   
@hosts('')
def create_deb():
   #only create deb if it's the first time this run it's been called
   current=local('head -n1 debian/changelog',capture=True)
   env.release=current.split('(')[1].split(')')[0] #version number of deb
   debs = glob.glob('%(prj_name)s_%(release)s*.deb' % env)
   if len(debs) < 1:
      local('tar zcv --transform=\'s$pylib$/opt/decades/pylib$\' -f %(prj_name)s-%(timestamp)s.orig.tar.gz pylib' % env)
      local('mkdir %(prj_name)s-%(timestamp)s' % env)
      local('git checkout-index --prefix=%(prj_name)s-%(timestamp)s/ -a' % env)
      local('git-dch %(dchopts)s --auto --git-author' % env) #adds latest commit details to a snapshot version
      local('cp -rp debian %(prj_name)s-%(timestamp)s/' % env)
      with lcd('%(prj_name)s-%(timestamp)s' % env):
         debuild_out = local('git-buildpackage --git-upstream-branch=master --git-debian-branch=master --git-export=INDEX --git-ignore-new' % env, capture=True)
         debuild_outlist = debuild_out.splitlines()
         for line in debuild_outlist:
            if "dpkg-deb: building package " in line:
               packagename = line.split('/')[1][:-2] #extract the .deb filename
      local('rm -rf %(prj_name)s-%(timestamp)s.orig.tar.gz %(prj_name)s-%(timestamp)s' % env )
      return packagename
   else:
      print debs[0]
      return debs[0]
   
def deploy_deb(debname=False):
   if debname:
      put(debname)
      #installs all dependencies
      sudo('aptitude -y install `dpkg --info %s | grep Depends | awk -F ":" \'{print $2}\' | sed -e "s/,/ /g"`' % debname)
      sudo('dpkg -i %s' % debname) 
   else:
      print('No deb filename specified')

def deploy():   
   debname = execute(create_deb())
     
