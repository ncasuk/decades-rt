Adding Extra Parameters
=======================

*Adding new parameters for the Plot application*

Abstract
--------

Procedure
---------

Pre-calibrated input data
~~~~~~~~~~~~~~~~~~~~~~~~~

Use this procedure if the input data are already calibrated for display.

Console data CSV file
^^^^^^^^^^^^^^^^^^^^^

You need to create a data description Comma-separated variable (``.csv``)
file of the same format as  ``/opt/decades/dataformats/CORCON01.csv``
describing the incoming data, and place it in  ``/opt/decades/dataformats``
on all tanks. (if the instrument is a long-term addition, it should be
added to the source-code repository.)

Display Parameters file
^^^^^^^^^^^^^^^^^^^^^^^

You must also add the desired display parameters to you Display
Parameters file; this defaults to
 ``/etc/decades/Display_Parameters_ver1.1.csv`` but can be overridden in
the  ``/etc/decades/decades.ini`` file’s  ``[Config]`` section.

In the case of precalibrated data, the Display Parameters file
ParameterName field should include the console name in lowercase, as
that is the database fieldname e.g.:

 ``900,twcdat01_twc_detector,TWC DETECTOR,(arb)``
