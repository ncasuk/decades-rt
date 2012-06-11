#!/bin/sh
### BEGIN INIT INFO
# Provides:          decades
# Required-Start:    $local_fs $remote_fs $network $syslog
# Required-Stop:     $local_fs $remote_fs $network $syslog
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: Start decades at boot time
# Description:       Enable service provided by decades.
### END INIT INFO

PATH=/sbin:/bin:/usr/sbin:/usr/bin

#don't change these here; change them in /etc/default/decades
listenerpidfile=/var/run/decades-listener.pid rundir=/usr/local/lib/decades/pylib/ listenerfile=/etc/decades/decades-listener.tac listenerlogfile=/var/log/decades/decades-listener.log
serverpidfile=/var/run/decades-server.pid rundir=/var/lib/decades/ serverfile=/etc/decades/decades-server.tac serverlogfile=/var/log/decades/decades-server.log

[ -r /etc/default/decades ] && . /etc/default/decades

test -x /usr/bin/twistd || exit 0
test -r $listenerfile || exit 0
#test -r /usr/share/decades/package-installed || exit 0


case "$1" in
    start)
        echo -n "Starting decades-listener: twistd"
        start-stop-daemon --start --quiet --exec /usr/bin/twistd -- \
               --pidfile=$listenerpidfile \
               --rundir=$rundir \
               --logfile=$listenerlogfile \
               --python=$listenerfile
        echo "."	
        echo -n "Starting decades-server: twistd"
        start-stop-daemon --start --quiet --exec /usr/bin/twistd -- \
               --pidfile=$serverpidfile \
               --rundir=$rundir \
               --logfile=$serverlogfile \
               --python=$serverfile
        echo "."	
   
    ;;

    stop)
        echo -n "Stopping decades-listener: twistd"
        start-stop-daemon --stop --quiet              --pidfile $listenerpidfile
        echo "."	
        echo -n "Stopping decades-server: twistd"
        start-stop-daemon --stop --quiet              --pidfile $serverpidfile
        echo "."	
    ;;

    restart)
        $0 stop
        $0 start
    ;;

    force-reload)
        $0 restart
    ;;

    *)
        echo "Usage: /etc/init.d/decades {start|stop|restart|force-reload}" >&2
        exit 1
    ;;
esac

exit 0
