Testing the DECADES server
==========================

*An approach to testing the DECADES server in cases where no live data
are available*

Abstract
--------

An approach is described to allow the testing of the Server element of
the application is isolation using a Python script which simulates
incoming data to replace the Listener element.

Procedure
---------

Stop the Listener element
~~~~~~~~~~~~~~~~~~~~~~~~~

Locate the decades-listener process, and kill it:

``$ sudo kill $(sudo cat /var/run/decades-listener.pid)``

Start the database simulator
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Change directory to the decades dir:

``$ cd /usr/local/lib/decades``

Start the database simulator:

``$ ./pydecades/database-simulator.py``

This replaces the UDP Listener element with simulated data. You can also
limit the number of DLUs simulated. Run:

``$ ./pydecades/database-simulator.py --help``

for information.

Run clients
~~~~~~~~~~~

On a machine connected to same network as the tanks, go to

``http://fish/plot/plot.html``

or

``http://septic/plot/plot.html``

as appropriate, and start various applets monitoring incoming data. (Not
all values are simulated; for a full list, see
``/usr/local/lib/decades/pydecades/database-simulator.py``, line 25)

Monitoring
----------

As you increase the number of clients, you can monitor the effect on the
tank machine by running the command ``top`` and looking for processes
called ``twistd``.
