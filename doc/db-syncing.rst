Synchronising the DECADES database
==================================

*Synchronising user-editable DECADES tables*

Abstract
--------

Although the primary live data table mergeddata on the Postgres database
is updated automatically and independantly on the Tank servers, the
Flight Summary (summary) table is input by users (primarily the Flight
Manager) during the flight and needs to be synchronised between the
tanks.

Requirements
------------

Edits on one tank are immediately copied to the other
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We are using Bucardo for this. It has a bi-directional sync mode so
edits can be made on either tank. The limitation here is that only two
tanks may be used, although this is being worked on.

Installing
~~~~~~~~~~

The bucardo daemon only needs installing on one of the tanks, however
both require the custom functions. These can be found in:

``/usr/share/bucardo/bucardo.schema``

after the Ubuntu bucardo package is installed. Both tanks require a
superuser “bucardo” creating, although the schema will do that. You then
need to create the two databases in bucardo’s config, using the
``bucardo_ctl`` control script.

Most significantly, as Postgres is using md5 authentication, the tank
running bucardo (septic) must have the Postgres bucardo user’s password
inserting in the clear into the bucardo.db table on the record referring
to the other tank’s database. This seems to be a bug in the
``bucardo_ctl`` program, and is not ideal from a security
perspective, although we are not in a high-security environment.
