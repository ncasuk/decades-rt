Installing a Tank From a Debian package
---------------------------------------

Starting from a standard Ubuntu LTS install (use server name ``decades-test`` if you have no reason to use another one)

#. Open Terminal with Internet access
#. Update the base OS with ``sudo apt update && sudo apt dist-upgrade``
#. Install required packages with ``sudo apt install openssh-server vim screen tmux``
#. Copy latest ``.deb`` file from your development machine using SSH.
#. install it and all dependencies with ``sudo dpkg -i decades-<VERSION DETAILS>.deb || sudo apt -fy install`` 
#. Check it is running with ``ps auxw | grep decades``
#. Browse to http://<servername>, e.g. http://header/. You will need to add a ``ServerAlias`` line with the server name to ``/etc/apache2/sites-enabled/decades.conf`` if it is not there already. http://127.0.0.1 should also work.
#. Start the decades simulator with ``decades-mgr local_start_simulator`` if you need to test the Live Data display.
