#!/bin/sh

PATH=/sbin:/bin:/usr/sbin:/usr/bin

pidfile=/var/run/twisted-decades-listener.pid rundir=/var/lib/twisted-decades-listener/ file=/etc/decades-listener.tac logfile=/var/log/twisted-decades-listener.log

[ -r /etc/default/twisted-decades-listener ] && . /etc/default/twisted-decades-listener

test -x /usr/bin/twistd || exit 0
test -r $file || exit 0
test -r /usr/share/twisted-decades-listener/package-installed || exit 0


case "$1" in
    start)
        echo -n "Starting twisted-decades-listener: twistd"
        start-stop-daemon --start --quiet --exec /usr/bin/twistd --                           --pidfile=$pidfile                           --rundir=$rundir                           --file=$file                           --logfile=$logfile
        echo "."	
    ;;

    stop)
        echo -n "Stopping twisted-decades-listener: twistd"
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
        echo "Usage: /etc/init.d/twisted-decades-listener {start|stop|restart|force-reload}" >&2
        exit 1
    ;;
esac

exit 0
