#!/bin/sh

PATH=/sbin:/bin:/usr/sbin:/usr/bin

pidfile=/var/run/decades-listener.pid rundir=/opt/decades/pylib/ file=/etc/decades-listener/decades-listener.tac logfile=/var/log/decades-listener.log

[ -r /etc/default/decades-listener ] && . /etc/default/decades-listener

test -x /usr/bin/twistd || exit 0
test -r $file || exit 0
#test -r /usr/share/decades-listener/package-installed || exit 0


case "$1" in
    start)
        echo -n "Starting decades-listener: twistd"
        start-stop-daemon --start --quiet --exec /usr/bin/twistd -- \
               --pidfile=$pidfile \
               --rundir=$rundir \
               --logfile=$logfile \
               --python=$file
        echo "."	
    ;;

    stop)
        echo -n "Stopping decades-listener: twistd"
        start-stop-daemon --stop --quiet              --pidfile $pidfile
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
        echo "Usage: /etc/init.d/decades-listener {start|stop|restart|force-reload}" >&2
        exit 1
    ;;
esac

exit 0
