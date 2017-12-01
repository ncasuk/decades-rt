Developing DECADES
==================

*Developing and deploying the DECADES system*

Abstract
--------

Setting up your development environment
---------------------------------------

Preparation
~~~~~~~~~~~

You will need access to the DECADES git revision control repository. You
will also need a Linux development box. The deployment tanks are running
the Ubuntu LTS distribution, but any Debian-derived distribution will
probably work.

Checkout the source code
^^^^^^^^^^^^^^^^^^^^^^^^

You will need to contact FAAM with your public key to allow access. Once
that is done: ``git clone git@213.171.204.22:/decades-rt``

will create a working copy of the repository in the current directory.

Unit Testing
------------

Before committing a change or deploying a package, you should run the unit tests:

::

    fab test

This is not currently fully complete, but does test that ``STAT`` and ``PARA`` 
requests
work as expected and all paramaters in the Display Parameters file return 
values. You can test a single parameter using a command of the form:

::

    fab unit_test_parameter:corcon01_fast_temp


Versioning
----------

To increment the version number you need to increment the number in the file ``VERSION`` in the repository root.


It is a good idea to "tag" the Git repository with the new version number:

::

   git tag -a $(cat VERSION) -m "Version $(cat VERSION)"
   git push --tags

Make the release and deploy:

``RELEASE=yes fab -H fish,septic deploy``

Alternately, you can create a package using:

``fab package``

copy the resulting ``.deb`` file to the tank(s) and then manually install the 
package with ``dpkg -i <name-of-deb-file> || apt-get -fy install`` (``fab deploy_deb:<name-of-deb-file>`` will do this automatically)

PostgreSQL
----------

Accessing the Postgres Database directly
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``psql -h 127.0.0.1 --port=5432 --password --user inflight inflightdata``

Monitoring the number of clients
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This can be useful to check if some process is leaving clients idle but unclosed. There is a ``max_connections`` parameter in the
Postgres config file (i.e. ``/etc/postgresql/X.X/main/postgresql.conf`` where X.X is the Postgres version you're running) after which 
it will refuse further queries. This defaults to 100. It has been set to 1000 on Drunk, but it is only 115 on Fish and Septic as they have
a lower max Shared Memory size. 

``SELECT SUM(numbackends) FROM pg_stat_database;``

If Postgres is refusing queries due to reaching ``max_connections`` you can restart it to close any idle ones:

::

   sudo service postgresql restart
   sudo service decades restart


