C                 PROGRAM H_JAVASERVE 
C                 Called by H_JAVASERVE.COM which is in turn
C                 called by the auxilliary server on port 1500
C
C    Opens a socket to the client and reads strings which it
C    interprets as commands and calls the appropriate subroutine.             
C
C    COMMANDS  
C
C    PARA  -  Outputs derived data from common block
C
C    DRS -    Outputs DRS data from common block
C
C    STAT -   Outputs Status data ( time position etc )
C
C    POST -   Outputs H
TML version of flight log
C
C    GET -    Same as POST
C
C    EVENT -  Modify events text file ( flight log )
C
C    QUIT -   Close socket and end program
C
      implicit none 

      integer*4 nn,ipara(61),itime,inum,i,ii,getint
      integer*4 iu,ito,itime2,nevents,ievents,i2,i3,LAN,istat
      character*256 c,readsocket
      character*200 cevents(100),cblank,c1
      character comm*80,cnam*20,cone*31
      character cfloat*4,crfloat*4,cint*2,cfno*4,cbutt*25
      integer*2 jstat2,jstat1,jnow
      real
*4 ff
      INTEGER*4 J1,J2,JJHRS,JJMIN,JJSEC,JTIME
      character*64 cnx
      character*53 cstatj
      character*80 cn
      include 'HCOM_DEF.FOR'

      CALL get_logical('FLIGHT_NUMBER',cfno)
      if(cfno.eq.' ') cfno='xxxx'
      CALL opensocket()

 3     c=readsocket(256)
      INQUIRE(FILE='[HORACE.WEB.PLOT]MAP_DATA.DAT',NAME=cn,
     &               iostat=istat)
      IF(istat.NE.0)GOTO 10
      iu=INDEX(cn,';')
      cnx=cn(iu+1:LEN(cn)-1)
      read(cnx(1:LAN(cnx)),'(I)')iu
      cstatj(1:1)=C
HAR(iu)                !MAP
      jstat2=status(2)
      cstatj(2:3)=cint(jstat2)    !Derive buffer pointer
      cstatj(4:5)=cint(status(3))
      IF((NPLOC(2).EQ.0).OR.(NPLOC(3).EQ.0))GOTO 10
      J1=NFDATA(NPLOC(2),STATUS(1))
      J2=NFDATA(NPLOC(3),STATUS(1))
      JJHRS=IBITS(J1,8,4)*10+IBITS(J1,4,4) !DRS hours
      JJMIN=IBITS(J1,0,4)*10+IBITS(J2,8,4) !DRS minutes
      JJSEC=IBITS(J2,4,4)*10+IBITS(J2,0,4) !DRS seconds
      JTIME=3600*JJHRS+60*JJMIN+JJSEC
      cstatj(6:9)=cfloat(REAL(JTIME))     
!Time
      cstatj(10:13)=cfloat(derive(jstat2,158))  !GINHDG
      cstatj(14:17)=cfloat(derive(jstat2,64))  !SPR
      cstatj(18:21)=crfloat(derive(jstat2,67),1)  !PHGTF
      cstatj(22:25)=cfloat(derive(jstat2,5))   !TAS
      cstatj(26:29)=crfloat(derive(jstat2,9),2)   !TATDC
      cstatj(30:33)=crfloat(derive(jstat2,17),2)  !DEW
      cstatj(34:37)=cfloat(derive(jstat2,58))  !IWS
      cstatj(38:41)=cfloat(derive(jstat2,59))  !IWA
      cstatj(42:45)=crfloat(derive(jstat2,150),1)  !GINLAT
      cstatj(4
6:49)=crfloat(derive(jstat2,151),1)  !GINLONG
!      cstatj(50:53)=cfloat(derive(jstat2,1))   !FLNO
      cstatj(50:53)=cfno
       if(c(1:4).EQ.'QUIT')GOTO 10
       if(c(1:4).EQ.'STAT')THEN
         call writesocket(cstatj)
       endif  
       if(c(1:4).EQ.'PARA')THEN
         itime=getint(c,1)
         itime2=getint(c,2)
         nn=getint(c,3)
         do i=1,nn
           ipara(i)=getint(c,i+3)
         enddo
         jnow=jstat2
         if(itime.LT.0)THEN
           itime=jstat2
           jnow=-1

         endif
         ito=jstat2
         if((itime2.GT.-1).AND.(itime2.LE.jstat2))ito=itime2
         inum=(ito-itime)+1
         call writesocket(cstatj)
         if(jnow.GE.0)THEN
           call writeshort(0)
         else
           call writeshort(-1)
         endif
         call writeshort(jnow)
         call writeint(inum)
         do i=1,nn
           do ii=itime,ito
             if(ii.GT.0)THEN
               ff=derive(MOD(ii,12800),ipara(i)-512)
             else 
               ff=0
          
   endif
             call writefloat(ff)
           enddo
         enddo
       endif
       if(c(1:3).EQ.'DRS')THEN
         nn=getint(c,1)
         do i=1,nn
           ipara(i)=getint(c,i+1)
         enddo
         do i=1,nn
           call writeshort(NPFREQ(ipara(i)))
         enddo
         jstat1=-1
         c1='C'
         do while (c1.EQ.'C')
         if(jstat1.EQ.status(1))THEN
           CALL LIB$WAIT(0.1)
         ELSE
           jstat1=status(1)
           J1=NFDATA(NPLOC(2),jstat1)
           
J2=NFDATA(NPLOC(3),jstat1)
           JJHRS=IBITS(J1,8,4)*10+IBITS(J1,4,4) !DRS hours
           JJMIN=IBITS(J1,0,4)*10+IBITS(J2,8,4) !DRS minutes
           JJSEC=IBITS(J2,4,4)*10+IBITS(J2,0,4) !DRS seconds
           JTIME=3600*JJHRS+60*JJMIN+JJSEC
           call writeint(jtime)
           do i=1,nn
             do ii=1,NPFREQ(ipara(i))
                call writeshort(NFDATA(NPLOC(ipara(i))+ii-1,jstat1))
             enddo
           enddo
           c1=readsocket(1)
         ENDIF
         ENDDO
       
  goto 10
       endif
       if(c(1:3).EQ.'GET')THEN
          call writehtml(cfno)
          goto 10
       endif  
       if(c(1:4).EQ.'POST')THEN
          call writehtml(cfno)
          goto 10
       endif
       if(c(1:4).EQ.'TEXT')THEN
         CALL vtaux(ICHAR(c(5:5)))
         goto 10
       endif 
       if(c(1:5).EQ.'EVENT')THEN
         cnam=c(7:26)
         comm=c(27:106)
         CALL loadevents(cfno,cevents,nevents,ievents)
         ii=nevents+1
         i2=1         
         if(c(6:6).EQ.'
S')THEN
           if(ievents.EQ.0)THEN
             ievents=ii
             cevents(ii)=cblank(cnam,comm)  
             nevents=ii
             cbutt='Stop '//cnam
           ELSE
             i2=32
             ii=ievents
             ievents=0
             if((comm(1:1).NE.' ').AND.(comm(1:1).NE.CHAR(0)))THEN
               i3=166
               do while((i3.GE.86).AND.((cevents(ii)(i3:i3).EQ.' ')
     & .OR.(cevents(ii)(i3:i3).EQ.' ')))
                 i3=i3-1
               enddo
               if(i3
.NE.86)THEN
                 cevents(ii)(i3+1:i3+1)=' '
                 i3=i3+1
               endif
               if(i3.LT.166)cevents(ii)(i3+1:166)=comm(1:166-i3)
             endif
             cbutt='Start'
           ENDIF
         ELSE
           if(ievents.EQ.0)THEN
             cbutt='Start'
           ELSE
             cbutt='Stop '//cevents(ievents)(64:83)
           ENDIF
         ENDIF
         IF(c(6:6).EQ.'E')THEN
            cevents(ii)=cblank(cnam,comm)  
            nevents=ii
         EN
DIF
         call writesocket(cbutt)
         IF(c(6:6).NE.'B')THEN
           write(cevents(ii)(i2:i2+30),'(a)')
     &         cone(jtime,derive(jstat2,158),derive(jstat2,67)! GINHDG
     &         ,derive(jstat2,150),derive(jstat2,151)) !GINLAT, GINLONG
           CALL savevents(cfno,cevents,nevents,ievents)
         ENDIF 
       ENDIF
       goto 3
10     CONTINUE
       print *,'closing link'
       call closesocket()
       
       end
C****************************************************************
***************
C
C
C  FUNCTION GETINT(C,I)
C
C     RETURNS integer value from the character string C at index I
C             and swaps the bytes for VMS use.
C
C

      integer*4 function getint(c,i)

      character*256 c,cn
      integer i,ii,ic
      equivalence(cn,ic)
      ii=i*4
      cn=c(ii+4:ii+4)//c(ii+3:ii+3)//c(ii+2:ii+2)//c(ii+1:ii+1)
      getint=ic
      return
      end
C*******************************************************************************
C
C  SUBROUTINE WRITEFLOAT(F)
C
C     WR
ITES a floating point number F to the socket after converting
C            to java (ieee bigendian) format.
C
C
      SUBROUTINE writefloat(f)

      real*4 f,ff

      INCLUDE '($CVTDEF)'
      character*4 c
      equivalence(c,ff)

      CALL CVT$FTOF(f,%VAL(CVT$K_VAX_F),ff,
     &                %VAL(CVT$K_BIG_ENDIAN_IEEE_S),)
      call writesocket(c)
      RETURN
      END
C*******************************************************************************
C
C
C  FUNCTION CFLOAT(F)
C
C     RETURNS a string
 from converting a floating point number F 
C            to java (ieee bigendian) format.
C
C
      CHARACTER*4 FUNCTION cfloat(f)

      real*4 f,ff

      INCLUDE '($CVTDEF)'
      character*4 c
      equivalence(c,ff)

      CALL CVT$FTOF(f,%VAL(CVT$K_VAX_F),ff,
     &                %VAL(CVT$K_BIG_ENDIAN_IEEE_S),)
      cfloat=c
      RETURN
      END
C*******************************************************************************
C
C
C
C  FUNCTION CRFLOAT(F,N)
C
C     RETURNS a string from converting a
 floating point number F 
C            to java (ieee bigendian) format, after rounding to
C            N decimal places.
C
C
      CHARACTER*4 FUNCTION crfloat(f,n)

      real*4 f,ff
      integer*2 n
      character*4 cfloat

      ff=f*(10**n)
      ff=ifix(ff)
      ff=ff/(10**n)
      crfloat=cfloat(ff)

      return
      end
C*******************************************************************************
C
C
C  FUNCTION CINT(I)
C
C     RETURNS character string that represents integer I 
C            
 after swapping the bytes for VMS use.
C
C
      CHARACTER*2 FUNCTION cint(i)

      INTEGER*2 ii,i
      CHARACTER*2 c
      EQUIVALENCE(c,ii)
      ii=i
      cint=c(2:2)//c(1:1)
      RETURN
      END
C*******************************************************************************
C
C
C  SUBROUTINE WRITEINT(I)
C
C     WRITES integer I to socket after swapping bytes.
C
C
C
      SUBROUTINE writeint(i)

      integer*4 i,ii
      character*4 c
      equivalence(c,ii)

      ii=i
      call writesocket(c(4:
4)//c(3:3)//c(2:2)//c(1:1))
      RETURN
      END
C*******************************************************************************
C
C     SUBROUTINE WRITESHORT(I)
C
C
C     WRITES integer*2 I to socket after swapping bytes.
C
      SUBROUTINE writeshort(i)

      integer*2 i,ii
      character*2 c
      equivalence(c,ii)

      ii=i
      call writesocket(c(2:2)//c(1:1))
      RETURN
      END
C*******************************************************************************
C
C     FUNCTION GMT(SECS)
C
C  
   RETURNS string in HH:MM:SS format from integer SECS in seconds
C             past midnight
C
C
      CHARACTER*8 function gmt(secs)

      integer*4 hr,min,sec,secs,s

      hr=secs/3600
      s=secs-hr*3600
      min=s/60
      sec=s-min*60
      write(gmt,'(I2.2,A1,I2.2,A1,I2.2)')hr,':',min,':',sec
      RETURN
      END
C*******************************************************************************
C
C    FUNCTION CBLANK(B,C)
C 
C     CREATES a blank line for the events list.      
C

      CHARACTER
*200 function cblank(n,c)

        character n*20,c*80

        cblank=CHAR(0)
        cblank(1:31)='        ,   ,    ,     ,      ,'
        cblank(32:62)='        ,   ,    ,     ,      ,'
        cblank(63:86)='"                    ","'
        cblank(87:126)='                                        '
        cblank(127:167)='                                        "'
        cblank(64:83)=n
        cblank(87:166)=c
      return
      end
C******************************************************************
*************
C
C   function cone(time,hdg,hgt,lat,lon)
C
C    RETURNS one line of the events list.
C
C
      CHARACTER*31 function cone(time,hdg,hgt,lat,lon)

      real*4 hdg,hgt,lat,lon
      integer*4 time
      character*8 gmt
      
         write(cone(1:9),'(a9)')gmt(time)//','
         write(cone(10:13),'(I3.3,a1)')ifix(hdg),','
         if(hgt.GT.1)THEN
           write(cone(14:18),'(F4.1,a1)')hgt,','
         ELSE
           write(cone(14:18),'(F4.2,a1)')hgt,','
         ENDIF
         write(cone(
19:22),'(F4.1)')abs(lat)
         write(cone(25:29),'(F5.1)')abs(lon)
         if(lat.LT.0)THEN
           write(cone(23:24),'(a)')'S,'
         ELSE
           write(cone(23:24),'(a)')'N,'
         ENDIF
         if(lon.LT.0)THEN
           write(cone(30:31),'(a)')'W,'
         ELSE
           write(cone(30:31),'(a)')'E,'
         ENDIF

      return
      end
C*******************************************************************************
C
C     SUBROUTINE savevents(CFNO,C,N,I)
C    
C     WRITE out the 
events from fltsumm_****.txt where **** is
C          the flight number (CFNO) 
C          C list of events 
C          N is the number of events
C          I is the current event
C
      SUBROUTINE savevents(cfno,c,n,i)
        integer*4 ii,n,i
!        real*4 fno
        character*200 c(100)
        character*4 cfno

!        write(cfno,'(A1,I3.3)')'B',IFIX(fno)
        open(11,file='fltsumm_'//cfno//'.txt',status='unknown',recl=256,
     &      carriagecontrol='list',access='sequential')
        write(11
,'(a)')cfno
        write(11,'(I4.4,A,I4.4)')n,',',i
        do ii=1,n
          write(11,'(a)')c(ii)
        enddo
        close(11)
      RETURN
      END
C*******************************************************************************
C
C     SUBROUTINE loadevents(CFNO,C,N,I)
C    
C     READ in the events from fltsumm_****.txt where **** is
C          the flight number (CFNO) 
C          C list of events 
C          N is the number of events
C          I is the current event
C
      SUBROUTINE loadevent
s(cfno,c,n,i)
         integer*4 n,i,istat,ii
         character*200 c(100)
         character*4 cfno,cdum

         open(11,file='fltsumm_'//cfno//'.txt',status='old',
     &       iostat=istat)
         n=0
         i=0
         if(istat.EQ.0) THEN
            read(11,'(a)')cdum
            read(11,'(I4.4,X,I4.4)')n,i
            do ii=1,n
              read(11,'(a)')c(ii)                                  
            enddo
         endif
         close(11)
      RETURN
      END
C************************
*******************************************************
C
C    SUBROUTINE WRITEHTML(CFNO)
C
C    WRITE to socket the flight log in HTML format (FNO=flight number)
C
      SUBROUTINE writehtml(cfno)
      integer*4 nevents,ievents,i
      character cfno*4
      character*200 cevents(100)

         CALL loadevents(cfno,cevents,nevents,ievents)
C          call writesocket('HTTP/1.1 200 OK'//char(10))
C          call writesocket(
C     & 'Server: Horace/1.0'//char(10))
C           call writesocket(
C     & 'Dat
e: '//char(10))
C          call writesocket(
C     & 'Content-type: text/html'//char(10))
C          call writesocket(
C     & 'Last-modified: '//char(10))
C          call writesocket(
C     & 'Connection: close'//char(10))
C          call writesocket(
C     & '<!doctype html public '-//w3c/dtd html 4.0 '
C     & //'transitional//en">'//char(10)//char(13))
          call writesocket('<html><head>'//char(10)//char(13))
          call writesocket('<META HTTP-EQUIV="Expires" ')
          call writesocket('CONT
ENT="Friday,19 Mar 1998 ')
          call writesocket('13:00:00 CST">'//char(10)//char(13))
          call writesocket('<META HTTP-EQUIV="Pragma" ')
          call writesocket('CONTENT="no-cache;">')
          call writesocket(char(10)//char(13))
C          call writesocket('<META HTTP-EQUIV="Refresh" CONTENT="1">')
          call writesocket(char(10)//char(13))
          call writesocket('</head><body>'//char(10)//char(13))
          call writesocket('<H1>Flight Summary '//cfno)
          call writesocket(
'</H1>'//char(10)//char(13))
          call writesocket
     & ('<p><table border=1>'//char(10)//char(13))
          call writesocket('<TR><TH>Event')
          call writesocket
     & ('<TH>Start<TH>Hdg<TH>Hgt<TH>Lat<TH>Long<TH>')
          call writesocket
     & ('<TH>Stop<TH>Hdg<TH>Hgt<TH>Lat<TH>Long')
          call writesocket
     & ('<TH>Comment</TR>'//char(10)//char(13))
          do i=1,nevents
            call writesocket('<TR><TD>'//cevents(i)(64:83))
            call writesocket('<TD>'//cevents
(i)(1:8))
            call writesocket('<TD>'//cevents(i)(10:12))
            call writesocket('<TD>'//cevents(i)(14:17)//'kft')
            call writesocket('<TD>'//cevents(i)(19:23))
            call writesocket('<TD>'//cevents(i)(25:30))
            call writesocket('<TD>')
            call writesocket('<TD>'//cevents(i)(32:39))
            call writesocket('<TD>'//cevents(i)(41:43))
            call writesocket('<TD>'//cevents(i)(45:48))
            if(cevents(i)(48:48).NE.' ')call writesocket('kft')
  
          call writesocket('<TD>'//cevents(i)(50:54))
            call writesocket('<TD>'//cevents(i)(56:61))
            call writesocket('<TD>'//cevents(i)(87:166))
            call writesocket('</TR>'//char(10)//char(13))
          enddo
          call writesocket('</table>'//char(10)//char(13))
          call writesocket('<a name="bottom">')
          call writesocket('&nbsp</a>'//char(10)//char(13))
          call writesocket('</body>'//char(10)//char(13))
          call writesocket('<script language="
javascript">')
          call writesocket(char(10)//char(13))
          call writesocket('window.location.href="#bottom"')
          call writesocket(char(10)//char(13))
          call writesocket('</script>')
          call writesocket(char(10)//char(13))
          call writesocket('</html>'//char(10)//char(13))
        RETURN
       END
C*******************************************************************************
      SUBROUTINE opensocket()

      IMPLICIT  NONE
      INTEGER*4 ISTAT,IFUNC,INPUT
    
  INTEGER*2 JOSB(4),JCHAN,JSOCKCHAR(2)
      BYTE      BSOCKCHAR(4)
      EQUIVALENCE (JSOCKCHAR,BSOCKCHAR)
      COMMON INPUT,JOSB,JCHAN
      INCLUDE   'SYS$LIBRARY:TCPIP$INETDEF.FOR'
      INCLUDE   '($IODEF)'
      INCLUDE   '($SYSSRVNAM)'
!
! Assign the device
!
      ISTAT=SYS$ASSIGN('SYS$NET:',JCHAN,,)
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
!
! Take over the socket
!
      IFUNC=IO$_SETMODE
      JSOCKCHAR(1)=TCPIP$C_TCP
      BSOCKCHAR(3)=TCPIP$C_STREAM
      BSOCKCHAR(4)=TCPIP$C_AUXS
     
 ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFUNC),JOSB,,,
     &    JSOCKCHAR,,,,,)
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      IF(.NOT.JOSB(1)) CALL LIB$STOP(%VAL(JOSB(1)))

      RETURN
      END
!*******************************************************************************
      CHARACTER*(*) FUNCTION readsocket(ILCIN)
!
      IMPLICIT  NONE
      INTEGER*4 ISTAT,IFUNC,INPUT,ILCIN
      INTEGER*2 JOSB(4),JCHAN,JSOCKCHAR(2)
      CHARACTER*256 CIN
      BYTE      BSOCKCHAR(4)
      COMMON INPUT,JOSB,JC
HAN
      EQUIVALENCE (JSOCKCHAR,BSOCKCHAR)
      INCLUDE   'SYS$LIBRARY:TCPIP$INETDEF.FOR'
      INCLUDE   '($IODEF)'
      INCLUDE   '($SYSSRVNAM)'
!
! Read some data
!
      JSOCKCHAR(1)=TCPIP$C_TCP
      BSOCKCHAR(3)=TCPIP$C_STREAM
      BSOCKCHAR(4)=TCPIP$C_AUXS
      IFUNC=IO$_READVBLK
      ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFUNC),JOSB,,,
     & %REF(CIN),
     &%VAL(ILCIN),,,,)
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      IF(.NOT.JOSB(1)) CALL closesocket()
      readsocket=cin(1:ILCIN)
    
  RETURN
      END
C*******************************************************************************
      CHARACTER*256 FUNCTION readlnsocket(I,ISTAT)
!
      IMPLICIT  NONE
      INTEGER*4 ISTAT,IFUNC,INPUT,I
      INTEGER*1 INP
      INTEGER*2 JOSB(4),JCHAN,JSOCKCHAR(2)
      CHARACTER CSTRING*256
      BYTE      BSOCKCHAR(4)
      COMMON INPUT,JOSB,JCHAN
      EQUIVALENCE (JSOCKCHAR,BSOCKCHAR)
      INCLUDE   'SYS$LIBRARY:TCPIP$INETDEF.FOR'
      INCLUDE   '($IODEF)'
      INCLUDE   '($SYSSRVNAM)'
!
! Re
ad some data
!
      JSOCKCHAR(1)=TCPIP$C_TCP
      BSOCKCHAR(3)=TCPIP$C_STREAM
      BSOCKCHAR(4)=TCPIP$C_AUXS
      IFUNC=IO$_READVBLK
      I=0
    9 CONTINUE
      INPUT=0
      DO WHILE(INPUT.NE.13.AND.INPUT.NE.10.AND.
     &         INPUT.NE.26.AND.I.LT.256)
        ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFUNC),JOSB,,,
     &      %REF(INP),%VAL(1),,,,)
        INPUT=INP
        IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
        IF(.NOT.JOSB(1)) CALL closesocket()
        IF(INPUT.NE.10.AND.INPUT.NE.13.AND.I
NPUT.NE.26)THEN
          I=I+1
          CSTRING(I:I)=CHAR(INPUT)
        END IF
      END DO
      ISTAT=0
      IF((INPUT.EQ.10).AND.(I.EQ.0))GOTO 9
      IF(INPUT.EQ.26) ISTAT=-1

      readlnsocket=CSTRING
      RETURN
      END
C*******************************************************************************
      SUBROUTINE writesocket(COUT)
!
      IMPLICIT  NONE
      INTEGER*4 ISTAT,IFUNC,INPUT,ILCOUT
      INTEGER*2 JOSB(4),JCHAN,JSOCKCHAR(2)
      CHARACTER COUT*(*)
      BYTE      BSOCKCHAR(4)
 
     COMMON INPUT,JOSB,JCHAN
      EQUIVALENCE (JSOCKCHAR,BSOCKCHAR)
      INCLUDE   'SYS$LIBRARY:TCPIP$INETDEF.FOR'
      INCLUDE   '($IODEF)'
      INCLUDE   '($SYSSRVNAM)'
!
! Write some data
!
      ILCOUT=LEN(COUT)
      JSOCKCHAR(1)=TCPIP$C_TCP
      BSOCKCHAR(3)=TCPIP$C_STREAM
      BSOCKCHAR(4)=TCPIP$C_AUXS
      IFUNC=IO$_WRITEVBLK
      ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFUNC),JOSB,,,
     & %REF(COUT),
     &%VAL(ILCOUT),,,,)
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      IF(.NOT.JOSB(1)) C
ALL closesocket()
      RETURN
      END
C*******************************************************************************
      SUBROUTINE closesocket()

      IMPLICIT  NONE
      INTEGER*4 ISTAT,IFUNC,INPUT
      INTEGER*2 JOSB(4),JCHAN,JSOCKCHAR(2)
      BYTE      BSOCKCHAR(4)
      COMMON INPUT,JOSB,JCHAN
      EQUIVALENCE (JSOCKCHAR,BSOCKCHAR)
      INCLUDE   'SYS$LIBRARY:TCPIP$INETDEF.FOR'
      INCLUDE   '($IODEF)'
      INCLUDE   '($SYSSRVNAM)'
!
! Kill the socket
!
      JSOCKCHAR(1)=TCPIP$C_TCP
  
    BSOCKCHAR(3)=TCPIP$C_STREAM
      BSOCKCHAR(4)=TCPIP$C_AUXS
      IFUNC=IO$_DEACCESS
      ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFUNC),JOSB,,,
     &    ,,,%VAL(TCPIP$C_DSC_ALL),,)
!
! Deassign the device
!
      ISTAT=SYS$DASSGN(%VAL(JCHAN))
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
!
      CALL EXIT
      RETURN
      END
C*******************************************************************************
      SUBROUTINE VTAUX(I1)

      INTEGER*4 INPUT,IOS,ILEN
      INTEGER*2 JCHAN,JOSB(4)
      CHAR
ACTER CLINE*80
      INTEGER*2 I1
      COMMON    INPUT,JOSB,JCHAN
!
! Write some data
!
      INPUT=0
      SELECT CASE(I1)
        CASE(1) 
          CALL SAT_MENU
        CASE(2) 
          CALL INPUT_AST
          CALL GPS1_DISP
          CALL INPUT_ABORT
          CALL GPS_MENU
        CASE(3) 
          CALL INPUT_AST
          CALL NDU_DISP
          CALL INPUT_ABORT
        CASE(4) 
          CALL INPUT_AST
          CALL NEVZ_DISP
          CALL INPUT_ABORT
        CASE(5) 
          CALL INPUT_AST

          CALL INU_DISP
          CALL INPUT_ABORT
          CALL INU_MENU
        CASE(6) 
          CALL DCL('@[HORACE]H_STAT')
          CALL VT_IN(CLINE,ILEN,IOS)            !Wait for cr or ctrl/z
        CASE(7)
          CALL DRS_MENU 
        CASE(8)
          CALL NEPH_MENU
      END SELECT
      CALL INST_MENU
      RETURN
      END
C*******************************************************************************
      SUBROUTINE QWRITE(CSTRING)
!
! Writes a string to the SYS$NET output device.
!
 
     INTEGER*4 IFUNC,ISTAT,ILEN,LEN,INPUT
      INTEGER*2 JCHAN,JOSB(4)
      CHARACTER CSTRING*(*)
      COMMON    INPUT,JOSB,JCHAN
      INCLUDE   '($IODEF)'
      INCLUDE   '($SYSSRVNAM)'

      IFUNC=IO$_WRITEVBLK
      ILEN=LEN(CSTRING)
      ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFUNC),JOSB,,,
     &    %REF(CSTRING),%VAL(ILEN),,,,)
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      IF(.NOT.JOSB(1)) CALL VTAUX_EXIT
      RETURN
      END
!*****************************************************************
**************
      SUBROUTINE INPUT_AST
!
! This routine is used because FORTRAN routines can't call themselves
!
      CALL INPUT_AST1
      RETURN
      END
!*******************************************************************************
      SUBROUTINE INPUT_AST1
!
! Called asynchronously when a character is received from the keyboard:
!
! 1.  Character is returned in INPUT
! 2.  Sets up the next asynchronous trap
!
! Subroutines called:
!
!   INPUT_AST This file
!
! The program aborts on any read err
ors
!
! V1.00  13/09/99  W.D.N.JACKSON
!
      IMPLICIT  NONE
      INTEGER*4 IFNCT,INPUT,ISTAT
      INTEGER*2 JOSB(4),JCHAN
      INCLUDE   '($SYSSRVNAM)'
      INCLUDE   '($IODEF)'
      INCLUDE   '($SSDEF)'
      EXTERNAL  INPUT_AST
      COMMON    INPUT,JOSB,JCHAN

      IF(JOSB(1).NE.SS$_NORMAL) THEN   !Do nothing if was an error
        INPUT=1000
        JOSB(1)=0
        RETURN
      END IF
     
      IFNCT=IO$_READVBLK
      ISTAT=SYS$QIO(,%VAL(JCHAN),%VAL(IFNCT),JOSB,%REF(INPUT_AST),,
     -    
INPUT,%VAL(1),,,,)           !Read one character
      IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      RETURN
      END     
!*******************************************************************************
      SUBROUTINE INPUT_ABORT
!
! Aborts unsolicted keyboard input, (so can take input through FORTRAN reads).
! Note that this causes an immediate AST which must be handled.
!
      IMPLICIT  NONE
      INTEGER*4 ISTAT,INPUT
      INTEGER*2 JOSB(4),JCHAN
      INCLUDE   '($SYSSRVNAM)'
      COMMON    IN
PUT,JOSB,JCHAN

      ISTAT=SYS$CANCEL(%VAL(JCHAN))
      IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      RETURN
      END
!*******************************************************************************
      SUBROUTINE VT_IN(CSTRING,ILEN,ISTAT)
!
! Reads a string from the SYS$NET output device.
!
      INTEGER*4 IFUNC,ISTAT,ILEN,LEN,INPUT,I
      INTEGER*2 JCHAN,JOSB(4)
      CHARACTER CSTRING*(*)
      COMMON    INPUT,JOSB,JCHAN
      INCLUDE   '($IODEF)'
      INCLUDE   '($SYSSRVNAM)'

      IFUNC=IO$
_READVBLK
      I=0
      INPUT=0
      ILEN=LEN(CSTRING)
      CSTRING=' '
      DO WHILE(INPUT.NE.13.AND.INPUT.NE.26)
        ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFUNC),JOSB,,,
     &      %REF(INPUT),%VAL(1),,,,)
        IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
        IF(.NOT.JOSB(1)) CALL VTAUX_EXIT
        IF(INPUT.NE.13.AND.INPUT.NE.26) THEN
          IF(INPUT.EQ.127)THEN
            IF(I.GT.0)THEN
              CSTRING(I:I)=CHAR(32)
              I=I-1
              CALL VT_OUT(CHAR(0)//CHAR(INPUT))  
 !Echo the character
            ENDIF
          ELSE
            IF(I.LE.ILEN-1) THEN
              I=I+1
              CSTRING(I:I)=CHAR(INPUT)
              CALL VT_OUT(CHAR(0)//CHAR(INPUT))   !Echo the character
            END IF
          END IF
        ELSE IF(INPUT.EQ.13) THEN
          CALL VT_OUT(CHAR(0)//CHAR(INPUT))   !Echo the character
        END IF
      END DO
      ILEN=I
      ISTAT=0
      IF(INPUT.EQ.26) ISTAT=-1
      RETURN
      END
!**************************************************
*****************************
      SUBROUTINE VT_OUT(CSTRING)
!
!  Outputs CSTRING using QWRITE
!
      INTEGER*4  ILEN,LEN
      CHARACTER  CSTRING*(*)

      ILEN=LEN(CSTRING)
      IF(CSTRING(1:1).EQ.CHAR(0)) THEN
        CALL QWRITE(CSTRING(2:ILEN))
      ELSE IF(CSTRING(1:1).EQ.' ') THEN
        IF(ILEN.GT.1) CALL QWRITE(CHAR(10)//CSTRING(2:ILEN)//CHAR(13))
        IF(ILEN.EQ.1) CALL QWRITE(CHAR(10)//CHAR(13))
      ELSE IF(CSTRING(1:1).EQ.'$') THEN
        IF(ILEN.GT.1) CALL QWRITE(CHAR(10)//CSTRING(
2:ILEN))
      END IF
      RETURN
      END
!*******************************************************************************
      SUBROUTINE VT_EDIT(CSTRING)
!
!
!
      CHARACTER  CSTRING*(*),CLINE*256,readlnsocket*256
      INTEGER*4 IOS,ILEN

      CALL VT_OUT
     &    (CHAR(0)//CHAR(27)//'[2J'//CHAR(27)//'[1;1H')
      CALL DCL('TYPE '//CSTRING)
      CALL VT_OUT(CHAR(0)//CHAR(27)//'E')
      OPEN(13,FILE=CSTRING,STATUS='NEW',
     &    IOSTAT=IOS,CARRIAGECONTROL='LIST')
      DO WHILE(IOS.NE.-1)
   
     CLINE=readlnsocket(ILEN,IOS)          !Wait for cr or ctrl/z
        IF(ILEN.GT.0)THEN
          WRITE(13,'(A)')CLINE(1:ILEN)
        ELSE
          WRITE(13,'(A)')' '
        ENDIF 
      END DO
      CLOSE(13)
      RETURN
      END
!*******************************************************************************
      SUBROUTINE VTAUX_EXIT
!
! Cleans up and releases socket
!
      IMPLICIT  NONE
      INTEGER*4 ISTAT,INPUT,IFUNC
      INTEGER*2 JOSB(4),JCHAN
      INCLUDE   '($SYSSRVNAM)'
      INCLU
DE   'SYS$LIBRARY:TCPIP$INETDEF.FOR'
      INCLUDE   '($IODEF)'
      COMMON    INPUT,JOSB,JCHAN
!
! Abort the outstanding asynchronous read
!
      CALL INPUT_ABORT
!
! Kill the socket
!
      IFUNC=IO$_DEACCESS
      ISTAT=SYS$QIOW(,%VAL(JCHAN),%VAL(IFUNC),JOSB,,,
     &    ,,,%VAL(TCPIP$C_DSC_ALL),,)
!
! Deassign the device
!
      ISTAT=SYS$DASSGN(%VAL(JCHAN))
!
      CALL EXIT
      RETURN
      END
!*******************************************************************************
      SUBROUTINE CHECK_
KEY(CSTR,IKEY)
!
! Checks for valid immediate key presses, and either processes them on the spot
! or returns with key value if they are to bring the plot to an end.  Returns
! 0 if no valid key press.  CSTR is a string containing valid key press
! characters.
!
      IMPLICIT  NONE
      INTEGER*4 IKEY,I,LEN,INPUT
      INTEGER*2 JOSB(4),JCHAN
      CHARACTER CSTR*(*)
      COMMON    INPUT,JOSB,JCHAN
!
      IKEY=0
      if(INPUT.EQ.1000) CALL VTAUX_EXIT
      IF(INPUT.EQ.0) RETURN   
      DO I=1,LEN(CSTR
)
        IF(INPUT.EQ.ICHAR(CSTR(I:I))) IKEY=INPUT
      END DO
      INPUT=0                          !Clear any invalid input
      RETURN
      END
!*******************************************************************************
      SUBROUTINE DCL(COMMAND)
!
! Executes a DCL command by spawning a sub-process and returning the results
! directly to SYS$NET after first adding a cr/lf to each line.  Note that it
! is not possible to execute programs and procedures which require input,
! because sys$net pr
ovides input one character at a time.
!
! This subroutine returns immediately the output has been sent.
!
      INTEGER*4 ISTAT,LIB$SPAWN
      CHARACTER COMMAND*(*),COMMAND1*80

      CALL VT_OUT(CHAR(0)//CHAR(27)//'2J'//CHAR(27)//'[1;1H')
      COMMAND1=
     &    'PIPE '//COMMAND(1:LEN(COMMAND))//
     &    '|RUN [HORACE]ADDCRLF'
      ISTAT=LIB$SPAWN(COMMAND1,,'SYS$NET',,,,,,,,,)
      IF(.NOT.ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      RETURN
      END
!*****************************************************
**************************
      SUBROUTINE GET_LOGICAL(CLOGICAL,CRESULT)
!
! Translate the logical name specified by CLOGICAL and returns the result in
! CRESULT.  Returns with blank if not found.  Searches all logical name tables
! and returns the first (lowest) definition found.
!
      IMPLICIT    NONE
      CHARACTER   CLOGICAL*(*),CRESULT*(*)
      INTEGER*4   ILIST(4),NAMLEN
      INTEGER*2   JLIST(8)
      EQUIVALENCE (ILIST,JLIST)
      INCLUDE     '($LNMDEF)'
!
      CRESULT=' '                   
   !Will contain the translation 
      JLIST(1)=LEN(CRESULT)            !Length of string space
      JLIST(2)=LNM$_STRING             !Required function
      ILIST(2)=%LOC(CRESULT)           !String location
      ILIST(3)=%LOC(NAMLEN)            !Will have string length
      ILIST(4)=0                       !End of list
      CALL SYS$TRNLNM(,'LNM$DCL_LOGICAL',CLOGICAL,,ILIST)
      RETURN
      END
                                                                                                        
