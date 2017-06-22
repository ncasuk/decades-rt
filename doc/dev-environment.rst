How to setup a DECADES development environment
==============================================

You will need:

* A working install of Ubuntu to which you have sudo access
* Access to the DECADES Git repository
* A working java install ( the `official Java package <http://www.ubuntugeek.com/how-to-install-oracle-java-7-in-ubuntu-12-04.html>`_ is best as the app is not yet compatible with the open-source versions)
* A Java signing certificate. ( ``keytool -genkey -keyalg RSA -alias septic -keystore ~/.keystore -storepass decades -validity 360 -keysize 2048`` `should work <https://www.sslshopper.com/article-how-to-create-a-self-signed-certificate-using-java-keytool.html>`_)

Install git and fabric
----------------------

``apt-get install git fabric``

Clone the Git repository
------------------------

``git clone git@77.68.61.13:/decades-rt``

Change into the repository directory
------------------------------------

``cd decades-rt``

Create the local dev environment
--------------------------------

``fab setup_local_dev_environment``

Create SSH stanzas referring to fish and septic
-----------------------------------------------

edit ``~/.ssh/config`` to include:

::

    Host fish
    Hostname 192.171.146.62
    Port 2122

    Host septic
    Hostname 192.171.146.62
    Port 2222

Run the Twisted decades-server and the database simulator
---------------------------------------------------------

``DECADESPORT=1500 twistd -ny pydecades/decades-server.tac``

and in a different terminal or `screen <http://www.gnu.org/software/screen/>`_:

``pydecades/database-simulator.py``

Then browse to:

``http://decades-test/``

If you are running on a “headless” (i.e. without X) linux box and you
need to run firefox on it and display it on a local machine, you can use
the ``-no-remote`` option to stop it interfering with your local
firefoxes:

``firefox -no-remote http://decades-test/``
