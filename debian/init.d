#!/bin/bash
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
listenerpidfile=/var/run/decades-listener.pid rundir=/usr/local/lib/decades/pydecades/ listenerfile=/etc/decades/decades-listener.tac listenerlogfile=/var/log/decades/decades-listener.log
serverpidfile=/var/run/decades-server.pid rundir=/var/lib/decades/ serverfile=/etc/decades/decades-server.tac serverlogfile=/var/log/decades/decades-server.log
tcplistenerpidfile=/var/run/decades-tcplistener.pid tcplistenerfile=/etc/decades/decades-tcp-listener.tac tcplistenerlogfile=/var/log/decades/decades-tcplistener.log
serverbalancerpidfile=/var/run/decades-serverbalancer.pid serverbalancerfile=/etc/decades/decades-server-balancer.tac serverbalancerlogfile=/var/log/decades/decades-serverbalancer.log
ginpidfile=/var/run/decades-gin.pid ginfile=/etc/decades/decades-gin.tac ginlogfile=/var/log/decades/decades-gin.log

umask=022

[ -r /etc/default/decades ] && . /etc/default/decades

#Load config file
. /usr/bin/cfg_parser.sh
cfg.parser '/etc/decades/decades.ini' DECADES_
cfg.section.Servers
SLAVEPORTS=`seq $DECADES_slave_base_port $(($DECADES_slave_base_port+$DECADES_slaves-1))`

test -x /usr/bin/twistd || exit 0
test -r $listenerfile || exit 0
#test -r /usr/share/decades/package-installed || exit 0


case "$1" in
    start)
        echo -n "Starting decades-tcp-listener: twistd"
        start-stop-daemon --start --quiet --exec /usr/bin/twistd -- \
               --pidfile=$tcplistenerpidfile \
               --rundir=$rundir \
               --umask=$umask \
               --logfile=$tcplistenerlogfile \
               --python=$tcplistenerfile
        echo "."	
        echo -n "Starting decades-listener: twistd"
        start-stop-daemon --start --quiet --exec /usr/bin/twistd -- \
               --pidfile=$listenerpidfile \
               --rundir=$rundir \
               --umask=$umask \
               --logfile=$listenerlogfile \
               --python=$listenerfile
        echo "."	
        for DECADESPORT in $SLAVEPORTS
        do
            echo -n "Starting decades-server [port $DECADESPORT]: twistd"
            DECADESPORT=$DECADESPORT start-stop-daemon --start --quiet --exec /usr/bin/twistd -- \
               --pidfile=$serverpidfile${DECADESPORT} \
               --rundir=$rundir \
               --umask=$umask \
               --logfile=$serverlogfile${DECADESPORT} \
               --python=$serverfile
            echo "."	
        done
        echo -n "Starting decades-server-balancer: twistd"
        start-stop-daemon --start --quiet --exec /usr/bin/twistd -- \
               --pidfile=$serverbalancerpidfile \
               --rundir=$rundir \
               --umask=$umask \
               --logfile=$serverbalancerlogfile \
               --python=$serverbalancerfile
        echo "."	
   
        echo -n "Starting decades-gin: twistd"
        start-stop-daemon --start --quiet --exec /usr/bin/twistd -- \
               --pidfile=$ginpidfile \
               --rundir=$rundir \
               --umask=$umask \
               --logfile=$ginlogfile \
               --python=$ginfile
        echo "."	
   
    ;;

    stop)
        echo -n "Stopping decades-listener: twistd"
        start-stop-daemon --stop --quiet              --pidfile $listenerpidfile
        echo "."	
        echo -n "Stopping decades-server-balancer: twistd"
        start-stop-daemon --stop --quiet              --pidfile $serverbalancerpidfile
        echo "."	
        echo -n "Stopping decades-gin: twistd"
        start-stop-daemon --stop --quiet              --pidfile $ginpidfile
        echo "."	
        for DECADESPORT in $SLAVEPORTS
        do
            echo -n "Stopping decades-server [port $DECADESPORT]: twistd"
            DECADESPORT=$DECADESPORT start-stop-daemon --stop --quiet   --pidfile $serverpidfile${DECADESPORT}
            echo "."	
        done
        echo -n "Stopping decades-tcp-listener: twistd"
        start-stop-daemon --stop --quiet              --pidfile $tcplistenerpidfile
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
