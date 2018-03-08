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
^^^^^^^^^^^^^^^^^^^^^^^^^

Locate the decades-listener process, and kill it:

``$ sudo kill $(sudo cat /var/run/decades-listener.pid)``

.. _simulator-usage:

Start the database simulator
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Start the database simulator:

``$ fab local_start_simulator``

or, on a system with a DECADES package installed:

``$ decades-mgr local_start_simulator``

If you have a ``.csv`` of genuine data from a flight, you can tell the simulator 
to use that as a source:

``$ fab local_start_simulator:<path-to-csv-file>``

e.g.

``$ fab local_start_simulator:pydecades/test/mergeddata-2018-02-08-1400-flight.csv``

You can also limit the number of DLUs simulated. Run:

``$ ./pydecades/database-simulator.py --help``

or 

``$ /usr/lib/python2.7/dist-packages/pydecades/decades-simulator.py --help``

for information.

Run clients
^^^^^^^^^^^

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

Remote UDP Monitoring
---------------------

It is possible to forward the incoming UDP packets on a tank to 
your local machine over SSH. This should probably not be attempted 
in-flight.

Listening to the UDP multicast port using ``socat`` on a tank:

.. code-block:: bash

    socat UDP4-RECV:50001,ip-add-membership=239.1.4.6:eth0 -

Suggested .ssh/config stanza:

::

    Host septic
    Hostname 192.171.146.62
    DynamicForward 12590
    LocalForward 15111 localhost:15111
    Port 2222

The LocalForward line sets up the TCP channel for the below.

Forwarding UDP:50001 to tcp:15111 (e.g. so it can be forwarded over ssh) (run on server, e.g. septic):

.. code-block:: bash

    socat tcp4-listen:15111 UDP4-RECV:50001,ip-add-membership=239.1.4.6:eth0

connecting tcp:15111  to be available to a listener on UDP:50001 (run on local machine)

.. code-block:: bash

    socat -T15 udp4-sendto:localhost:50001,reuseaddr tcp:localhost:15111

The combination of the ssh tunnel and the Socat commands makes the UDP inputting to septic appear locally. You can then run listen_udp_twisted.py directly to test the code or cd to work dir and:

.. code-block:: bash

    DECADESPORT=1500 twistd --pidfile=listen.pid  -ny pylib/decades-listener.tac

to run it in the same way the daemonisation scripts run.

To run the decades server (which talks to the Java applet) you need to run ``pydecades/decades-server.py``

You need to edit your hosts file to fool plot.html into thinking your laptop is horace. Just make horace an alias to 127.0.0.1

Running a local copy of decades-server
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
In work directory:

.. code-block:: bash

    DECADESPORT=1500 twistd --pidfile=server.pid -ny pylib/decades-server.tac

Forwarding incoming TCP packets
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: bash

    ssh -R 192.168.101.110:3502:129.11.85.226:3502 fish
