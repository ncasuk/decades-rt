SEA probe UDP listener
======================

The (experimental) code for the SEA probe was based on the documentation
provided by the manufacturer, namely

* ``wcm2000 data output format.docx``
* ``wcm2000 data input format.docx``
* User guide from ``http://www.scieng.com/pdf/WCM2000User.pdf``

Usage
-----

The system is not yet integrated into the DECADES system fully, so needs
to be started by SSHing into Septic (``192.168.101.108``) and running:

.. code-block:: bash

    sudo twistd -ny /etc/decades/sea_udp_listener.tac

It will need to be run in the background (e.g. with Ctrl-Z and ``bg``) or in 
GNU ``screen`` or it will close if your SSH session ends.

On running, after a few seconds it will display the initial status. It will look
something like this:

.. code-block:: shell-session

    Removing stale pidfile /home/eardkdw/twistd.pid
    2017-07-27 15:00:34+0000 [-] Log opened.
    2017-07-27 15:00:34+0000 [-] twistd 11.1.0 (/usr/bin/python 2.7.3) starting up.
    2017-07-27 15:00:34+0000 [-] reactor class: twisted.internet.pollreactor.PollReactor.
    2017-07-27 15:00:34+0000 [-] SeaUDP starting on 2100
    2017-07-27 15:00:34+0000 [-] Starting protocol <pydecades.sea_udp_listener.SeaUDP instance at 0x390c098>
    2017-07-27 15:00:34+0000 [-] Started Listening to SEA probe
    2017-07-27 15:00:34+0000 [SeaUDP (UDP)] Creating output file /opt/decades/output/2017/07/27/seaprobe_20170727_150034_XXXX

You can do ``tail -f <outputfilename>`` to observe incoming data directly.
If the flight number changes it should start a new file.

To display the TWC, IWC, and LWC values in the Java display app, you also need
a later version of ``pydecades/rt_calcs/rt_derive.py``, and ``/etc/decades/Display_Parameters_ver1.3.csv`` which include the raw (TWC, LWC83, LWC21) and 
calculated params (IWC83, IWC21). These are already on Septic.

All these files are in the ``dan-test`` branch of the DECADES git repository.

Code Docs
---------
.. automodule:: pydecades.sea_udp_listener
    :members:
