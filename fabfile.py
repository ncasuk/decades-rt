#!/usr/bin/env python
# vim: tabstop=8 expandtab shiftwidth=3 softtabstop=3
# -*- coding: utf-8 -*-
# fabfile for Django:
# http://morethanseven.net/2009/07/27/fabric-django-git-apache-mod_wsgi-virtualenv-and-p/
# modified for fabric 0.9/1.0
#from __future__ import with_statement # needed for python 2.5
from fabric.api import *
from fabric.utils import warn
from fabric.contrib import console
import time, os, glob, csv
from cStringIO import StringIO

from pydecades.configparser import DecadesConfigParser

# globals
env.prj_name = 'decades' # no spaces!
env.sudoers_group = 'wheel'
env.webserver = 'apache2' # nginx or apache2 (directory name below /etc!)
env.dbserver = 'postgresql' # mysql or postgresql
env.timestamp = time.strftime('%Y%m%d%H%M%S')
env.dchopts = '--snapshot'
env.parser = DecadesConfigParser()
if os.environ.has_key('RELEASE') and os.environ['RELEASE']:
   env.dchopts = '--release'

@runs_once
@task
def list_hosts():
   '''Lists the hosts commands will act on. (Default is none)'''
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

@task
def local_database_setup(suffix=''):   
   '''Creates inflight database and summary table (does nothing if it already 
   exists'''
   with settings(warn_only=True): #already-exists errors ignored
      subs = {'suffix' : suffix}
      for each in env.parser.items('Database'):
         subs[each[0]] = each[1]
      local('sudo -u postgres psql -c "CREATE ROLE %(user)s%(suffix)s UNENCRYPTED PASSWORD \'%(password)s\' SUPERUSER CREATEDB CREATEROLE INHERIT LOGIN;"'% subs)
      local('sudo -u postgres createdb -O %(user)s%(suffix)s %(database)s%(suffix)s' % subs)
      local('sudo -u postgres createlang plpgsql %(database)s%(suffix)s' % subs)
   local('sudo -u postgres psql -c "CREATE TABLE IF NOT EXISTS summary ( id serial primary key, flight_number char(4) NOT NULL, event text, start timestamp default now(), start_heading int, start_height float, start_latitude float, start_longitude float, stop timestamp, stop_heading int, stop_height float, stop_latitude float, stop_longitude float, comment text, finished boolean default \'t\', ongoing boolean default \'t\', exclusive boolean default \'f\');" %(database)s%(suffix)s'  % subs)

@task
def local_database_delete(suffix='', ask=True):
   '''DROPs all instrument tables and the mergeddata 
      table. Leaves summary intact.'''
   if not ask or console.confirm("This will delete all live data. Do want to continue?", default=False):
      subs = {'suffix' : suffix}
      for each in env.parser.items('Database'):
         subs[each[0]] = each[1]
      tablelist = StringIO(local('sudo -u postgres psql -P "footer=off" -F "," -Ac "\dt" %(database)s%(suffix)s' % subs, capture=True))
      #skip first row
      tablelist.next()
      tables = csv.DictReader(tablelist)
      for each in tables:
         if each['Name'] not in ('summary'):
            local('sudo -u postgres psql -c "DROP TABLE ' + each['Name'] + ';" %(database)s%(suffix)s' % subs)
   else:
      print "Not doing anything!" 
   

@task
def setup_local_dev_environment():
   '''Sets up a development environment on a Ubuntu install'''
   local('sudo apt-get -y install aptitude')
   #stuff to *run* the software (you will need to first "apt-get install fabric")
   local('sudo aptitude -y install apache2 libapache2-mod-wsgi python-webpy postgresql python-setuptools python-numpy python-tz python-jinja2 python-twisted python-psycopg2 python-sphinx python-testresources python-pbr')
   local('sudo a2enmod wsgi')
   local_database_setup()
   local('sudo ln -nfs ${PWD}/config/apache-config /etc/apache2/sites-available/%(prj_name)s.conf' % env)
   local('sudo a2ensite %(prj_name)s' % env)
   #link apache files to dev versions
   local('sudo mkdir -p /var/www/%(prj_name)s/plot' % env)
   local('sudo ln -nfs ${PWD}/web/css /var/www/%(prj_name)s/' % env)
   local('sudo ln -nfs ${PWD}/web/js /var/www/%(prj_name)s/' % env)
   local('sudo ln -nfs ${PWD}/doc/_build/html /var/www/%(prj_name)s/docs' % env)
   local('sudo ln -nfs ${PWD}/Horace/web/plot/map_data.dat.gz /var/www/%(prj_name)s/plot' % env)
   local('sudo ln -nfs ${PWD}/Horace/web/plot/overlay.txt /var/www/%(prj_name)s/plot' % env)
   local('sudo ln -nfs ${PWD}/Horace/web/plot/Parano_old.txt /var/www/%(prj_name)s/plot' % env)
   local('sudo ln -nfs ${PWD}/Horace/web/plot/plot.html /var/www/%(prj_name)s/plot' % env)
   local('sudo ln -nfs ${PWD}/Horace/web/plot/Plot.jar /var/www/%(prj_name)s/plot' % env)
   local('sudo ln -nfs ${PWD}/live /var/www/%(prj_name)s-live' % env)
   local('sudo service apache2 reload' % env)
   #python module
   local('sudo python setup.py build')
   local('sudo python setup.py install')
   #runtime ini files
   local('sudo mkdir -p /etc/%(prj_name)s' % env)
   local('sudo ln -nfs ${PWD}/config/%(prj_name)s.ini /etc/%(prj_name)s/' % env)
   local('sudo ln -nfs ${PWD}/config/Display_Parameters*.csv /etc/%(prj_name)s/' % env)
   local('sudo ln -nfs ${PWD}/config/HOR_CALIB.DAT /etc/%(prj_name)s/' % env)
   local('sudo ln -nfs ${PWD}/fabfile.py /etc/%(prj_name)s/' % env)
   #dataformats
   local('sudo mkdir -p /opt/%(prj_name)s/' % env)
   local('sudo ln -nfs ${PWD}/dataformats /opt/%(prj_name)s/' % env)
   #make prj_name-dev work resolve to localhost
   #Could easily be changed; will only do it if
   # it cannot already ping it
   local('ping -c1 %(prj_name)s-dev || echo "127.0.0.1 %(prj_name)s-dev" | sudo tee -a /etc/hosts' % env)

   print('''run the decades-server app:
     DECADESPORT=1500 twistd -ny decades-server.tac
   and maybe the DB simulator:
     pydecades/database-simulator.py
   and browse to:
     http://decades-dev/''')

   print('You will need to install java. http://www.ubuntugeek.com/how-to-install-oracle-java-7-in-ubuntu-12-04.html')
   #requirements to deploy
   local('sudo aptitude install -y fastjar git-buildpackage debhelper')

   
@runs_once
@task
def package():
   '''Creates a .deb package of the current branch'''
   env.branch = local('git rev-parse --abbrev-ref HEAD', capture=True).strip()

   env.packageprefix = ('%(prj_name)s-%(timestamp)s-%(branch)s' % env)
   local('mkdir %(packageprefix)s' % env)
   local('git checkout-index --prefix=%(packageprefix)s/ -a' % env)
   local('git submodule init')
   local('git submodule update')
   local('git submodule foreach "cp -r * \$toplevel/%(packageprefix)s/\$path/"' %env)
   local('gbp dch %(dchopts)s --debian-branch=%(branch)s --auto --git-author' % env) #adds latest commit details to a snapshot version
   local('cp -rp debian %(packageprefix)s/' % env)
   #generate documentation
   docs('../%(packageprefix)s/web' % env)
   local('mv %(packageprefix)s/web/html  %(packageprefix)s/web/docs' % env)
   local('cp Horace/web/plot/Plot.jar %(packageprefix)s/Horace/web/plot/' % env)
   with lcd(env.packageprefix):
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
   
@task
def deploy_deb(debname=False):
   '''deploys a given .deb file to all hosts, including install dependencies'''
   if debname:
      put(debname)
      #installs all dependencies
      #sudo('aptitude -y install `dpkg --info %s | grep Depends | awk -F ":" \'{print $2}\' | sed -e "s/,/ /g"`' % debname)
      sudo('dpkg -i %s || apt-get -fy install' % debname) 
   else:
      print('No deb filename specified')

@task
def test():
   '''runs all the unit tests'''
   local_database_setup('_test')
   local('trial pydecades')
   local_database_delete(suffix='_test', ask=False)

@task
def unit_test_parameter(paramname):
   '''runs a unit test for a single parameter, e.g vertical_vorticity. 
   Usage: fab unit_test_parameter:<parametername>'''
   local('trial pydecades.test.test_decades_server.DecadesProtocolTestCase.test_%s' % paramname)

@task
def deploy():
   '''Creates and deploys a full package, including generating a .jar file
   for the Horace display client, and restarting apache'''
   Plot_jar()
   debname=package()
   deploy_deb(debname=debname)
   sudo('a2enmod wsgi')
   sudo('service apache2 restart')
   pg_timezone = sudo('psql -tc "SHOW TIME ZONE" | head -n1',user="postgres").strip()
   if(pg_timezone != 'UTC'):
      #raise WARNING that postgres is not correctly configured 
      warn('Postgresql timezone is ' + pg_timezone + '. Set it to UTC in postgresql.conf')
      

@task
def clean():
   '''Cleans a development tree of all build files etc.'''
   local('find . -maxdepth 1 -name \*.tar.gz -exec rm {} \;')
   local('find . -maxdepth 1 -name \*.deb -exec rm {} \;')
   local('find . -maxdepth 1 -name \*.dsc -exec rm {} \;')
   local('find . -maxdepth 1 -name \*.build -exec rm {} \;')
   local('find . -maxdepth 1 -name \*.changes -exec rm {} \;')
   local(' rm -rf decades-20[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]')
   with lcd('Horace/web/plot/plot'):
      local('make clean') #cleans compiled Java files


@task
def docs(builddir=False):
   '''Create HTML documentation'''
   with lcd('doc'):
      if builddir:
        local('BUILDDIR='+builddir+' make html')
      else:
        local('make html' % env)

@task
def pdfdocs():
   '''creates PDF documentation'''
   with lcd('doc'):
      local('make latex' % env)
   with lcd('doc/_build/latex'):
      local('make all-pdf' % env)

@runs_once
@task
def Plot_jar():
   '''Creates the JAR file for the display applicaton'''
   with lcd('Horace/web/plot/plot'):
      local('make jar')
      #sign jar if and only if it isn't signed
      local('jarsigner -verify -strict Plot.jar || jarsigner Plot.jar septic')
      local('cp Plot.jar ..') 
