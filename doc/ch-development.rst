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
that is done: ``git clone git@77.68.61.13:/decades-rt``

will create a working copy of the repository in the current directory.

Versioning
----------

To increment the version number you need to:

Edit the python module version number in: setup.py

Tag the Git repository with the new version number:

::

   git tag -a 0.9.0 -m ’Version 0.9.0’ 
   git push --tags

Make the release and deploy:

``RELEASE=yes fab -H fish,septic deploy``

You will be offered the chance to edit the change log. Change the number
on the top line to match the release number.
