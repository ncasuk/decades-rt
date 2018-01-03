.. _tanks-proposal:

Tanks on the Aircraft (proposed)
--------------------------------

I think the last few weeks have proved that we really need to have a third tank 
on the aircraft, for testing purposes. Jamie and I tried to install the 
``dan-test`` branch of DECADES onto his laptop, without much success, although 
after some work I have now successfully installed the package *ab initio* on 
a vanilla Ubuntu 16.04LTS VM [1]_. I intend to do some more work on that before 
VANAHEIM.

For testing purposes, obviously an actual tank would be ideal, but as they are 
fairly standard machines (it’s the special heatsink case that’s the expensive 
bit) a “good enough” test can be a laptop installed with Ubuntu and DECADES; 
although they won’t share the Flight Summary data, the live data stuff should 
work fine. My suggestion would be, in the style of Debian Linux’s 
stable/unstable/testing arrangement:

Fish
    Stable, no non-emergency live changes (referred to as "Flying Fish"; see below)
Septic
    Stable, live changes if required (i.e. master branch with changes limited 
    to non-interfering ones (e.g. SEA Probe stuff that’s currently flying))
Header
    Unstable. Probably the next release of the master branch. “Should work”, 
    but hasn’t flown previously.
Drunk
    Testing. “bleeding edge”, probably a particular developer’s test branch (e.g 
    ``dan-test``, ``dave-test``). Probably the developer in question would also 
    be flying.

Header and Drunk would be repurposed laptops. They do not need to be 
particularly special; an out-of-warranty one that’s a few years old would be 
fine.

In the lab, there is also a tank for testing. It also is called Fish and has the same IP address as the aircraft one. It should be referred to as "Blenny" [2]_ to
reduce ambiguity.

Users could use Header and Drunk to see live data if they wished, with the 
proviso that they aren’t to rely on it.

.. [1] Septic & Fish are currently running Ubuntu 12.04LTS, which has end-of-lifed now unless we want to pay for Extended Security Support, so that’s something else that needs dealing with.

.. [2] https://en.wikipedia.org/wiki/Pacific_leaping_blenny "They are able to dwell on land for several hours at a time" :)
