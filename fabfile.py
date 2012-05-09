#!/usr/bin/env python
# -*- coding: utf-8 -*-
# fabfile for Django:
# http://morethanseven.net/2009/07/27/fabric-django-git-apache-mod_wsgi-virtualenv-and-p/
# modified for fabric 0.9/1.0
#from __future__ import with_statement # needed for python 2.5
from fabric.api import *
import time

# globals
env.prj_name = 'decades-listener' # no spaces!
env.sudoers_group = 'wheel'
env.webserver = 'apache2' # nginx or apache2 (directory name below /etc!)
env.dbserver = 'postgresql' # mysql or postgresql

# environments

def localhost():
    "Use the local virtual server"
    env.hosts = ['localhost']
    env.user = 'hraban' # You must create and sudo-enable the user first!
    env.path = '/Users/%(user)s/workspace/%(prj_name)s' % env # User home on OSX, TODO: check local OS
    env.scriptpath = '/usr/local/bin/%(prj_name)s' % env
    env.virtualhost_path = env.path
    env.pysp = '%(virtualhost_path)s/lib/python2.6/site-packages' % env

def webserver():
    "Use the actual webserver"
    env.hosts = ['fish','septic'] # Change to your server name!
    env.user = env.prj_name
    env.scriptpath = '/usr/local/bin/%(prj_name)s' % env
    env.path = '/var/www/%(prj_name)s' % env
    env.virtualhost_path = env.path
    env.pysp = '%(virtualhost_path)s/lib/python2.6/site-packages' % env
   
# tasks

def test():
    "Run the test suite and bail out if it fails"
    local("cd %(path)s; python manage.py test" % env) #, fail="abort")
    
    
def setup():
    require('hosts', provided_by=[localhost,webserver])
    require('path')
    # install Python environment
    sudo('apt-get install -y build-essential python-dev python-setuptools')
    # install some version control systems, since we need Django modules in development
    sudo('apt-get install -y git-core') # subversion git-core mercurial
        
    # install more Python stuff
    # Don't install setuptools or virtualenv on Ubuntu with easy_install or pip! Only Ubuntu packages work!
    sudo('easy_install pip')

    # install webserver and database server
    sudo('apt-get remove -y apache2 apache2-mpm-prefork apache2-utils') # is mostly pre-installed
    if env.webserver=='nginx':
        sudo('apt-get install -y nginx')
    else:
        sudo('apt-get install -y apache2-mpm-worker apache2-utils') # apache2-threaded
        sudo('apt-get install -y libapache2-mod-wsgi') # outdated on hardy!
    if env.dbserver=='mysql':
        sudo('apt-get install -y mysql-server python-mysqldb')
    elif env.dbserver=='postgresql':
        sudo('apt-get install -y postgresql python-psycopg2')
        
    # new project setup
    sudo('mkdir -p %(path)s; chown %(user)s:%(user)s %(path)s;' % env, pty=True)
    sudo('mkdir -p %(tmppath)s; chown %(user)s:%(user)s %(tmppath)s;' % env, pty=True)
    with settings(warn_only=True):
        run('cd ~; ln -s %(path)s www;' % env, pty=True) # symlink web dir in home
    with cd(env.path):
        run('virtualenv .') # activate with 'source ~/www/bin/activate'
        with settings(warn_only=True):
            run('mkdir -m a+w logs; mkdir releases; mkdir shared; mkdir packages; mkdir backup;', pty=True)
            if env.use_photologue:
                run('mkdir photologue', pty=True)
                #run('pip install -E . -U django-photologue' % env, pty=True)
            if env.use_medialibrary:
                run('mkdir medialibrary', pty=True)
            run('cd releases; ln -s . current; ln -s . previous;', pty=True)
    setup_user()
    deploy('first')
    
def setup_user():
    require('hosts', provided_by=[webserver])
    sudo('adduser "%(prj_name)s"' % env, pty=True)
    sudo('adduser "%(prj_name)s" %(sudoers_group)s' % env, pty=True)
    
def deploy(param=''):
    """
    Deploy the latest version of the site to the servers, install any
    required third party modules, install the virtual host and 
    then restart the webserver
    """
    require('hosts', provided_by=[localhost,webserver])
    require('path')
    env.release = time.strftime('%Y%m%d%H%M%S')
    install_requirements()
    install_site()
    symlink_current_release()
    migrate(param)
    restart_webserver()
    
def deploy_version(version):
    "Specify a specific version to be made live"
    require('hosts', provided_by=[localhost,webserver])
    require('path')
    env.version = version
    with cd(env.path):
        run('rm -rf releases/previous; mv releases/current releases/previous;', pty=True)
        run('ln -s %(version)s releases/current' % env, pty=True)
    restart_webserver()
    
def rollback():
    """
    Limited rollback capability. Simply loads the previously current
    version of the code. Rolling back again will swap between the two.
    """
    require('hosts', provided_by=[localhost,webserver])
    require('path')
    with cd(env.path):
        run('mv releases/current releases/_previous;', pty=True)
        run('mv releases/previous releases/current;', pty=True)
        run('mv releases/_previous releases/previous;', pty=True)
        # TODO: use South to migrate back
    restart_webserver()    
    
# Helpers. These are called by other functions rather than directly

def install_site():
    "Add the virtualhost config file to the webserver's config, activate logrotate"
    require('release', provided_by=[deploy, setup])
    with cd('%(path)s/releases/%(release)s' % env):
        sudo('cp %(webserver)s.conf /etc/%(webserver)s/sites-available/%(prj_name)s' % env, pty=True)
        if env.use_daemontools: # activate new service runner
            sudo('cp service-run.sh /etc/service/%(prj_name)s/run; chmod a+x /etc/service/%(prj_name)s/run;' % env, pty=True)
        else: # delete old service dir
            sudo('echo; if [ -d /etc/service/%(prj_name)s ]; then rm -rf /etc/service/%(prj_name)s; fi' % env, pty=True)
        # try logrotate
        with settings(warn_only=True):        
            sudo('cp logrotate.conf /etc/logrotate.d/website-%(prj_name)s' % env, pty=True)
    with settings(warn_only=True):        
        sudo('cd /etc/%(webserver)s/sites-enabled/; ln -s ../sites-available/%(prj_name)s %(prj_name)s' % env, pty=True)
    
def install_requirements():
    "Install the required packages from the requirements file using pip"
    require('release', provided_by=[deploy, setup])
    run('cd %(path)s; pip install -E . -r ./releases/%(release)s/requirements.txt' % env, pty=True)
    
def create_deb():
   env.release = time.strftime('%Y%m%d%H%M%S')
   local('tar zcv --transform=\'s$pylib$/opt/decades/pylib$\' -f %(prj_name)s-%(release)s.orig.tar.gz pylib' % env)
   local('mkdir %(prj_name)s-%(release)s' % env)
   local('dch --changelog debian/changelog --newversion %(release)s "new package created"' % env)
   local('cp -rp debian %(prj_name)s-%(release)s/' % env)
   with lcd('%(prj_name)s-%(release)s' % env):
      local('debuild -us -uc' % env)
   
   
def symlink_current_release():
    "Symlink our current release"
    require('release', provided_by=[deploy, setup])
    with cd(env.path):
        run('rm releases/previous; mv releases/current releases/previous;', pty=True)
        run('ln -s %(release)s releases/current' % env, pty=True)
        # copy South migrations from previous release, if there are any
        run('cd releases/previous/%(prj_name)s; if [ -d migrations ]; then cp -r migrations ../../current/%(prj_name)s/; fi' % env, pty=True)
        # collect static files
        with cd('releases/current/%(prj_name)s' % env):
            run('%(path)s/bin/python manage.py collectstatic -v0 --noinput' % env, pty=True)
            if env.use_photologue:
                run('cd static; rm -rf photologue; ln -s %(path)s/photologue photologue;' % env, pty=True)
    
def migrate(param=''):
    "Update the database"
    require('prj_name')
    require('path')
    env.southparam = '--auto'
    if param=='first':
        run('cd %(path)s/releases/current/%(prj_name)s; %(path)s/bin/python manage.py syncdb --noinput' % env, pty=True)
        env.southparam = '--initial'
    #with cd('%(path)s/releases/current/%(prj_name)s' % env):
    #    run('%(path)s/bin/python manage.py schemamigration %(prj_name)s %(southparam)s && %(path)s/bin/python manage.py migrate %(prj_name)s' % env)
    #    # TODO: should also migrate other apps! get migrations from previous releases
    
def restart_webserver():
    "Restart the web server"
    require('webserver')
    env.port = '8'+run('id -u', pty=True)[1:]
    with settings(warn_only=True):
        if env.webserver=='nginx':
            require('path')
            if env.use_daemontools:
                sudo('kill `cat %(path)s/logs/django.pid`' % env, pty=True) # kill process, daemontools will start it again, see service-run.sh
            if env.use_supervisor:
                sudo('supervisorctl restart %(prj_name)s:appserver' % env, pty=True)
                if env.use_celery:
                    sudo('supervisorctl restart %(prj_name)s:celery' % env, pty=True)
            #require('prj_name')
            #run('cd %(path)s; bin/python releases/current/%(prj_name)s/manage.py runfcgi method=threaded maxchildren=6 maxspare=4 minspare=2 host=127.0.0.1 port=%(port)s pidfile=./logs/django.pid' % env)
        sudo('/etc/init.d/%(webserver)s reload' % env, pty=True)
