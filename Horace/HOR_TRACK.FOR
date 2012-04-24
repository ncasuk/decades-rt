C
C HOR_TRACK
C HOR_TRACK1
C COAST_LINE
C LATLONG
C GET_PARA_TRACK
C GET_FREQ_TRACK
C GET_SCAL_TRACK
C
C*******************************************************************************
      SUBROUTINE HOR_TRACK
C
C Get the plotting arguments, then call track.
C
      IMPLICIT  NONE
      INTEGER*4 ILIST(7),IDEV,IVEC,IKEY,IFREQ,ILAT,ILNG

10    CALL GET_PARA_TRACK(IDEV,IVEC)
      IF(IDEV.EQ.-1) RETURN
20    CALL GET_FREQ_TRACK(IFREQ)
      IF(IFREQ.EQ.-1) RETURN
      IF(IFREQ.EQ.-2) GOTO 10
30    CALL GET_SCAL_TRACK(IDEV,ILAT,ILNG)
      IF(ILAT.EQ.-1.OR.ILNG.EQ.-1) RETURN
      IF(ILAT.EQ.-2.OR.ILNG.EQ.-2) GOTO 10
      IF(ILAT.EQ.-3.OR.ILNG.EQ.-3) GOTO 20
      ILIST(1)=7
      ILIST(2)=12
      ILIST(3)=IDEV
      ILIST(4)=IVEC
      ILIST(5)=IFREQ
      ILIST(6)=ILAT
      ILIST(7)=ILNG
      CALL UPDT_STATUS(ILIST)
      CALL HOR_TRACK1(ILIST,IKEY)
      IF(CHAR(IKEY).EQ.'A') RETURN
      IF(CHAR(IKEY).EQ.'B') GOTO 10
      IF(CHAR(IKEY).EQ.'C') GOTO 20
      IF(CHAR(IKEY).EQ.'D') GOTO 30
      END
C*******************************************************************************
      SUBROUTINE HOR_TRACK1(ILIST,IKEY)
C
C Plots track on plotted coastline, plotting past data for 30,60,90,120
C mins or with no past data.  Uses the coastline file HIRES_EUROPE_BIN.DAT.
C The sea is filled blue with the standard coastline, but not with the 
C magnified coastline.
C
C Note that all plotting of coastlines is done using lats and longs multiplied
C by 100.  All plotting of aircraft track is done using lats and longs
C multiplied by 400.  The first call to this routine simply
C plots the coast line and past data.  Subsequent calls plot new data as it
C becomes available.
C
C Subroutines called:
C
C COAST_LINE         This file               Reads coastline dataset and draws
C LATLONG            This file               Labels map
C HEAD_SHELL         HOR_SUBS                Draws the header shell
C HEAD_UPDATE        HOR_SUBS                Updates the header
C FOOT               HOR_SUBS                Draws the options boxes
C
C V1.00  01/01/89  J.HARMER
C V1.01  17/10/90  W.D.N.JACKSON
C        Now displays printer status
C V1.02  20/05/91  W.D.N.JACKSON
C        Fully reworked.
C        Now uses direct access full European dataset and only plots the
C        relevant coasts.  Now plots coast at lower resolution to speed
C        plotting.  The magnified track can be plotted in any region between
C        90S to 90N, and 180W to 180E, although the coastline is only available
C        between 30N to 70N, and 30W to 40E.
C V1.03  18/01/96  W.D.N.JACKSON
C        Now handles multiple nav devices and immediate key presses during
C        startup
C V1.04  22/11/96  W.D.N.JACKSON
C        Now plots wind vectors every minute if requested
C
      IMPLICIT  NONE
      INTEGER*4 IPARAY(3),IPARAX(3),ILSTX(3),ILSTY(3),ILIST(7),IVEC,IC,
     -    IDEV,IFREQ,ILATL,ILNGL,IS,IX,IY,MINLAT,MINLNG,MAXLAT,MAXLNG,
     -    I,ID,MAXVALAT,MAXVALNG,MAG,IP,IL,IIY,IWX,IWY,IKEY,MAXVAL,IC1,
     -    ITIM
      INTEGER*2 IBLK1(500),IBLK2(500)
      INCLUDE   'DCOM_DEF.FOR'
      INCLUDE   'HCOM_DEF.FOR'
C
      IKEY=ICHAR('A')                  !Default return
      IF(STATUS(2).LT.1.OR.STATUS(2).GT.12800) RETURN
      IDEV=ILIST(3)
      IVEC=ILIST(4)
      IFREQ=ILIST(5)
      ILATL=ILIST(6)
      ILNGL=ILIST(7)
      IF(IDEV.EQ.1) THEN
        IPARAY(1)=95                   !GPS latitude
        IPARAX(1)=96                   !GPS longitude
        ID=1
      ELSE IF(IDEV.EQ.2) THEN
        IPARAY(1)=93                   !INU latitude
        IPARAX(1)=94                   !INU longitude
        ID=1
      ELSE
        IPARAY(1)=95                   !GPS latitude
        IPARAX(1)=96                   !GPS longitude
        IPARAY(2)=93                   !INU latitude
        IPARAX(2)=94                   !INU longitude
        ID=2
      END IF
C
C Write fixed parts of the display to the access page the first time the
C routine is called.
C
      CALL BBK(IBLK1,IBLK2,500)        !Start double blocking
      CALL SEL(0,0,IBLACK)
      CALL RUB                         !Clear the page
      CALL HEAD_SHELL                  !Put up header shell
      CALL SEL(0,IBACKG,IBLACK)        !Green background
      CALL BLK(44,151,611,479)         !Draw green block
      CALL FOOT('SELECT','DEVICE','FREQ','ZOOM',' ','PRINT',' ',' ')
      CALL SEL(0,ILGREY,-1)            !Grey
      CALL BOXA(44,151,611,479)        !Draw grey frame
      CALL BOXA(43,150,612,480)
      CALL VPT(44,151,611,479)         !Define viewport
      IF(ILATL.EQ.0) THEN              !Default - whole map
        IS=STATUS(2)
        IY=NINT(DERIVE(IS,IPARAY(1)))  !Get current position
        IX=NINT(DERIVE(IS,IPARAX(1)))
        IF(IX.EQ.0.AND.IY.EQ.0) THEN
          IX=-1
          IY=52
        END IF
        MINLAT=(IY-8)*100
        MAXLAT=(IY+8)*100
        MINLNG=(IX-20)*100
        MAXLNG=(IX+20)*100
        CALL WIN(MINLNG,MINLAT,MAXLNG,MAXLAT)
        CALL COAST_LINE(MINLAT,MINLNG,MAXLAT,MAXLNG,IKEY)
      ELSE
        IF(ILATL.LE.-1000) ILATL=ILATL+1000
        IF(ILNGL.LE.-1000) ILNGL=ILNGL+1000
        MINLAT=(ILATL-1)*100           !Enlarged map
        MAXLAT=(ILATL+1)*100
        MINLNG=(ILNGL-2)*100
        MAXLNG=(ILNGL+2)*100
        CALL WIN(MINLNG,MINLAT,MAXLNG,MAXLAT)
        CALL COAST_LINE(MINLAT,MINLNG,MAXLAT,MAXLNG,IKEY)
      ENDIF
      IF(IKEY.NE.0) GOTO 5010
      CALL SEL(0,IFOREG,-1)
      CALL SCA(13,13,0,2)
      CALL LATLONG(MINLAT,MINLNG,MAXLAT,MAXLNG,IKEY)
      IF(IKEY.NE.0) GOTO 5000
      DO I=1,ID
        ILSTX(I)=9999                  !Last plotted values
        ILSTY(I)=9999
      END DO
      MAXVALAT=MAX(ABS(MINLAT),ABS(MAXLAT))
      MAXVALNG=MAX(ABS(MINLNG),ABS(MAXLNG))
      MAXVAL=MAX(MAXVALAT,MAXVALNG)
      MAG=4                            !Adjust scaling to suit data range
      IF(MAXVAL.GT.80) MAG=3           ! require scaled lats and longs in
      IF(MAXVAL.GT.105) MAG=2          ! range -32767 to 32767
      IF(MAXVAL.GT.150) MAG=1
      MINLAT=MINLAT*MAG                !Increase scaling for plotting
      MAXLAT=MAXLAT*MAG
      MINLNG=MINLNG*MAG
      MAXLNG=MAXLNG*MAG
      IF(IVEC.EQ.1) THEN
        CALL VEC(630,120,640,120)      !10 m/s line
        CALL ALP(650,115,'10 m/s')
      END IF
C
C Draw initial track using past data
C
      IF(IFREQ.EQ.5) GOTO 5020
      CALL CHECK_KEY('ABCD',IKEY)
      IF(IKEY.NE.0) GOTO 5000          !Valid keypress
      CALL WIN(MINLNG,MINLAT,MAXLNG,MAXLAT)
      CALL VPT(44,151,611,479)
      IC=STATUS(3)
      IP=STATUS(2)
      IS=IP-IC+1
      IF(IS.LE.0) IS=IS+12800
      DO I=1,ID                        !For each nav device
        IL=1
        IC1=IC
        DO WHILE(IC1.NE.0)
          CALL CHECK_KEY('ABCD',IKEY)
          IF(IKEY.NE.0) GOTO 5010      !Valid keypress
          IY=IFIX(DERIVE(IS,IPARAY(I))*MAG*100.)
          IX=IFIX(DERIVE(IS,IPARAX(I))*MAG*100.)
          ITIM=NINT(RSECS(IS))
          IF(IX.NE.ILSTX(I).OR.IY.NE.ILSTY(I)) THEN
            IF(IX.GE.MINLNG.AND.IX.LE.MAXLNG.AND.
     -        IY.GE.MINLAT.AND.IY.LE.MAXLAT.AND.SPECIAL(IS,9).EQ.1) THEN
              IIY=MAXLAT+MINLAT-IY       !Invert latitude
              IF(IL.EQ.1) THEN
                CALL VEC(IX,IIY,IX,IIY)  !Draw dot only on xy plot    
              ELSE
                CALL VEC(IX,IIY)         !Draw dot only on xy plot    
              END IF
              IL=IL+1
              IF(MOD(ITIM,60).EQ.0.AND.ID.EQ.1.AND.IVEC.EQ.1) THEN
                IWX=NINT(IX+RIWS(IS)*SIND(RIWA(IS)))
                IWY=NINT(IIY-RIWS(IS)*COSD(RIWA(IS)))
                CALL VEC(IWX,IWY,IX,IIY)
              END IF
              ILSTX(I)=IX
              ILSTY(I)=IY
            ELSE
              IL=1
            END IF
          END IF
          IS=IS+1
          IF(IS.GT.12800) IS=1
          IC1=IC1-1
        END DO
      END DO
      IS=IP
      CALL WIN
      CALL VPT
5020  CALL EBK
      GOTO 110

5010  CALL WIN 
      CALL VPT
5000  CALL EBK                         !Empty last block
      RETURN
C
C If not doing initial track, then the subroutine effectively starts here.
C
100   CALL HEAD_UPDATE                 !Update the header information
      IS=IS+1
      IF(IS.GT.12800) IS=IS-12800
      DO I=1,ID
        IY=IFIX(DERIVE(IS,IPARAY(I))*MAG*100.) !Get new position
        IX=IFIX(DERIVE(IS,IPARAX(I))*MAG*100.)
        ITIM=NINT(RSECS(IS))
        IF((IX.NE.ILSTX(I).OR.IY.NE.ILSTY(I)).AND.
     -      IX.GE.MINLNG.AND.IX.LE.MAXLNG.AND.
     -      IY.GE.MINLAT.AND.IY.LE.MAXLAT.AND.SPECIAL(IS,9).EQ.1) THEN
          CALL SEL(0,IFOREG,IBACKG)    !Cyan/green
          CALL WIN(MINLNG,MINLAT,MAXLNG,MAXLAT)
          CALL VPT(44,151,611,479) 
          IIY=MAXLAT+MINLAT-IY         !Invert latitude
          CALL VEC(ILSTX(I),MAXLAT+MINLAT-ILSTY(I),IX,IIY) !Draw new point
          IF(IVEC.EQ.1.AND.MOD(ITIM,60).EQ.0) THEN
            IWX=NINT(IX+RIWS(IS)*SIND(RIWA(IS)))
            IWY=NINT(IIY-RIWS(IS)*COSD(RIWA(IS)))
            CALL VEC(IWX,IWY,IX,IIY)
          END IF
          ILSTX(I)=IX
          ILSTY(I)=IY
          CALL WIN
          CALL VPT
        END IF
      END DO
C
110   DO WHILE(.TRUE.)
        CALL CHECK_KEY('ABCD',IKEY)
        IF(IKEY.NE.0) RETURN
        IF(STATUS(2).NE.IS) GOTO 100
        CALL LIB$WAIT(0.2)
      END DO
C
      END
C*******************************************************************************
      SUBROUTINE LATLONG(MINLAT,MINLNG,MAXLAT,MAXLNG,IKEY)
C
C Puts the tic intervals around the plotting area, and labels them.  The
C tics are done with the window mapped to the viewport as determined by the
C current size and scaling of the plot.  The labels are done with unity
C mapping.
C
C V1.00  01/01/89  J.HARMER
C V1.01  20/05/91  W.D.N.JACKSON
C        Handles both scales of plot with the same routine and puts lat and
C        long labels outside the plotting area.
C
      IMPLICIT  NONE
      INTEGER*4 MINLAT,MINLNG,MAXLAT,MAXLNG,LATINT,LNGINT,LVS,LVL,LHS,
     -    LHL,MAXLATI,MINLATI,ILNG,ILAT,IKEY,IX,IY,IILNG,ILATI
      CHARACTER CLABEL*3
      INCLUDE  'DCOM_DEF.FOR'
C
      IF((MAXLAT-MINLAT).GT.300) THEN  !Large are track
        LATINT=100
        LNGINT=100
      ELSE                             !Magnified track
        LATINT=10
        LNGINT=10
      END IF
      LVS=(MAXLAT-MINLAT)*5/328        !Short vertical tic length
      LVL=LVS*2                        !Long vertical tic length
      LHS=(MAXLNG-MINLNG)*5/567        !Short horiz tic length
      LHL=LHS*2                        !Long horiz tic length
      MAXLATI=MINLAT                   !Inverted latitudes
      MINLATI=MAXLAT
C
C Do top and bottom tics
C
      DO ILNG=MINLNG,MAXLNG,LNGINT
        CALL CHECK_KEY('ABCD',IKEY)
        IF(IKEY.NE.0) GOTO 510
        IF(MOD(ILNG,LNGINT*10).EQ.0) THEN
          CALL VEC(ILNG,MAXLATI,ILNG,MAXLATI+LVL)
          CALL VEC(ILNG,MINLATI,ILNG,MINLATI-LVL)
        ELSE
          CALL VEC(ILNG,MAXLATI,ILNG,MAXLATI+LVS)
          CALL VEC(ILNG,MINLATI,ILNG,MINLATI-LVS)
        END IF 
      END DO
C
C Do side tics
C
      DO ILAT=MINLAT,MAXLAT,LATINT
        CALL CHECK_KEY('ABCD',IKEY)
        IF(IKEY.NE.0) GOTO 510
        ILATI=MAXLAT+MINLAT-ILAT       !Invert latitude
        IF(MOD(ILAT,LATINT*10).EQ.0) THEN
          CALL VEC(MAXLNG,ILATI,MAXLNG-LHL,ILATI)
          CALL VEC(MINLNG,ILATI,MINLNG+LHL,ILATI)
        ELSE
          CALL VEC(MAXLNG,ILATI,MAXLNG-LHS,ILATI)
          CALL VEC(MINLNG,ILATI,MINLNG+LHS,ILATI)
        END IF 
      END DO
C
C Now revert to unity mapping to do the labels
C
      CALL WIN
      CALL VPT
      DO ILNG=MINLNG,MAXLNG,LNGINT     !Top and bottom labels
        CALL CHECK_KEY('ABCD',IKEY)
        IF(IKEY.NE.0) RETURN
        IF(MOD(ILNG,LNGINT*10).EQ.0) THEN
          IX=44+(ILNG-MINLNG)*(611-44)/(MAXLNG-MINLNG)-13*2
          IILNG=ILNG
          IF(IILNG.LT.-9900) IILNG=-IILNG !If more than 99W don't plot sign
          WRITE(CLABEL,'(I3)') IILNG/100
          CALL ALP(IX,151-15,CLABEL)
          CALL ALP(IX,479+11,CLABEL)
        END IF 
      END DO
      DO ILAT=MINLAT,MAXLAT,LATINT     !Side labels
        CALL CHECK_KEY('ABCD',IKEY)
        IF(IKEY.NE.0) RETURN
        IF(MOD(ILAT,LATINT*10).EQ.0) THEN
          WRITE(CLABEL,'(I3)') ILAT/100
          IY=479-(ILAT-MINLAT)*(479-151)/(MAXLAT-MINLAT)-13/2
          CALL ALP(44-45,IY,CLABEL)
          CALL ALP(611-2,IY,CLABEL)
        END IF 
      END DO
500   RETURN
510   CALL WIN
      CALL VPT
      RETURN
      END 
C*******************************************************************************
      SUBROUTINE COAST_LINE(IMINLAT,IMINLNG,IMAXLAT,IMAXLNG,IKEY)
C
C Draws coastline, selecting only those parts required by the size and scaling
C of the plot.  Not all points are plotted;  the distance between points must
C exceed a certain amount before it is plotted.
C
C Note that doesn't work across +-180 deg long.
C
C V1.00  20/05/91  W.D.N.JACKSON
C V2.00  21/08/98  W.D.N.JACKSON - Uses world dataset
C
      IMPLICIT  NONE
      INCLUDE   'DCOM_DEF.FOR'
      INTEGER*2 MINLAT,MINLNG,MAXLAT,MAXLNG,LATLNG(2,128),IPOINTS
      INTEGER*4 IMINLAT,IMINLNG,IMAXLAT,IMAXLNG,IKEY,LUNDAT,LUNNDX,
     -    ISEGS,ISEGSTART,ICURBLK,IS,LSTLAT,LSTLNG,IBLK,IWRD,
     -    IOS,LNG,LAT,IP,ISTART,LATI
      REAL*4    RLATINT,RLNGINT
C
C Read in the coastline index blocks.  The dataset will contain the coastlines
C split up into upto 256 sections.
C
      CALL LIB$GET_LUN(LUNDAT)
      CALL LIB$GET_LUN(LUNNDX)
      OPEN(UNIT=LUNDAT,FILE='COAST_LINE.DAT',STATUS='OLD',READONLY,
     -    ACCESS='DIRECT',RECL=128)
      OPEN(UNIT=LUNNDX,FILE='COAST_LINE.NDX',STATUS='OLD',READONLY,
     -    FORM='UNFORMATTED')
      READ(LUNNDX) ISEGS
C
C Draw coastlines.  Minimum intervals are set so that a change of at least one
C pixel must have taken place to make it worth sending another point to the 
C graphics display.
C
      RLATINT=REAL(ABS(IMAXLAT-IMINLAT)*4)/328. !Compute minimum intervals
      RLNGINT=REAL(ABS(IMAXLNG-IMINLNG)*4)/567.
      ISEGSTART=1
      IPOINTS=0
      ICURBLK=-1                       !Datafile block currently in LATLNG
      DO IS=1,ISEGS                    !For each coastline section
        CALL CHECK_KEY('ABCD',IKEY)
        IF(IKEY.NE.0) GOTO 100         !If valid keypress
        ISEGSTART=ISEGSTART+IPOINTS  !Next segment index into datafile
        READ(LUNNDX) IPOINTS,MINLAT,MAXLAT,MINLNG,MAXLNG
        IF(IMINLAT.LE.MAXLAT.AND.IMAXLAT.GE.MINLAT.AND.
     -      IMINLNG.LE.MAXLNG.AND.IMAXLNG.GE.MINLNG) THEN
          ISTART=1                   !New section
          LSTLAT=9999
          LSTLNG=9999
          IBLK=(ISEGSTART-1)/128+1
          IWRD=MOD(ISEGSTART-1,128)+1
          DO IP=1,IPOINTS
            CALL CHECK_KEY('ABCD',IKEY)
            IF(IKEY.NE.0) GOTO 100     !If valid keypress
            IF(IBLK.NE.ICURBLK) THEN
              READ(LUNDAT,REC=IBLK,IOSTAT=IOS) LATLNG
              ICURBLK=IBLK
            END IF
            LNG=LATLNG(1,IWRD)
            LAT=LATLNG(2,IWRD)
            IF(LAT.LT.IMINLAT.OR.LAT.GT.IMAXLAT.OR.LNG.LT.IMINLNG
     -            .OR.LNG.GT.IMAXLNG) THEN
              ISTART=1           !Gone off edge so end vector   
            ELSE
              IF(ABS(LSTLAT-LAT).GE.RLATINT.OR.ABS(LSTLNG-LNG).GE.
     -              RLNGINT.OR.ISTART.EQ.1.OR.IP.EQ.IPOINTS) THEN
                LATI=IMAXLAT+IMINLAT-LAT !Invert latitude
                IF (ISTART.EQ.1) THEN !Draw as dot first time
                  CALL VEC(LNG,LATI,LNG,LATI)
                  ISTART=0       !Reset new section indicator
                ELSE             !Draw line from previous pt
                  CALL VEC(LNG,LATI)
                ENDIF
                LSTLAT=LAT
                LSTLNG=LNG
              END IF
            END IF
            IWRD=IWRD+1
            IF(IWRD.GT.128) THEN
              IBLK=IBLK+1
              IWRD=1
            END IF
          END DO         
        END IF
      END DO
100   CLOSE(UNIT=LUNDAT,IOSTAT=IOS)
      CLOSE(UNIT=LUNNDX,IOSTAT=IOS)
      CALL LIB$FREE_LUN(LUNDAT)
      CALL LIB$FREE_LUN(LUNNDX)
      RETURN
      END
C*******************************************************************************
      SUBROUTINE GET_FREQ_TRACK(IFREQ)
C
C Subroutines used:
C
C   FOOT         HOR_SUBS
C   SELECT       HOR_UTIL
C   ggs          GGS.OLB
C
      IMPLICIT  NONE
      INTEGER*4 IFREQ,I,IY,LAN
      CHARACTER CLINE(10)*60
      INCLUDE   'DCOM_DEF.FOR'
C
C   menu 3 contents
C
      DATA CLINE(1)/' 1) Plot every  3 seconds (last 30 mins)'/
      DATA CLINE(2)/' 2) Plot every  6 seconds (last 60 mins)'/
      DATA CLINE(3)/' 3) Plot every  9 seconds (last 90 mins)'/
      DATA CLINE(4)/' 4) Plot every 12 seconds (last 120 mins)'/
      DATA CLINE(5)/' 5) Plot current data only, every 3 seconds'/
      DATA CLINE(6)/' 6) Plot last 3 hours'/
      DATA CLINE(7)/' 7) Plot last 4 hours'/
      DATA CLINE(8)/' 8) Plot last 6 hours'/
      DATA CLINE(9)/' 9) Plot last 8 hours'/
      DATA CLINE(10)/' 10) Plot last 10 hours'/
C
9     CALL SEL(0,0,IBLACK)
      CALL RUB                         !clear display page
      CALL SEL(0,IBACKG,0)             !D green
      CALL BLK(0,0,767,509)            !Fill block with background
      CALL SEL(0,IFOREG,IBACKG)        !cyan/d green
      CALL SCA(13,13,0,3)              !cell width, height, orientation, font
      CALL ALP(200,110,' MENU C - FREQUENCY')            
      IY=150
      DO I=1,10
        CALL ALP(10,IY,CLINE(I)(1:LAN(CLINE(I))))
        IY=IY+30
      END DO
      CALL FOOT('SELECT','DEVICE',' ',' ',' ','PRINT',' ',' ')
10    CALL ALP(30,450,'Enter selection (default =1)...          ')
      CALL SELECT(446,450,IFREQ)
      IF(IFREQ.EQ.0) IFREQ=1
!      IF(IFREQ.EQ.-8) THEN
!        CALL HELP
!        GOTO 9
!      END IF
      IF(IFREQ.LT.-2.OR.IFREQ.GT.10) GOTO 10
      IF(IFREQ.EQ.10) IFREQ=20
      IF(IFREQ.EQ.9) IFREQ=16
      IF(IFREQ.EQ.8) IFREQ=12
      IF(IFREQ.EQ.7) IFREQ=8
      IF(IFREQ.EQ.6) IFREQ=6
      RETURN
      END
C*******************************************************************************
      SUBROUTINE GET_PARA_TRACK(IDEV,IVEC)
C
C Subroutines used:
C
C   FOOT         HOR_SUBS
C   SELECT       HOR_UTIL
C   ggs          GGS.OLB
C
      IMPLICIT  NONE
      INTEGER*4 IDEV,IVEC,I,IY,LAN
      CHARACTER CLINE(5)*60
      INCLUDE   'DCOM_DEF.FOR'
C
C Contents of menu 2 for track plot
C
      DATA CLINE(1) /'            MENU B - NAVIGATION DEVICE         '/
      DATA CLINE(2) /'                                               '/
      DATA CLINE(3) /'1) GPS                                         '/
      DATA CLINE(4) /'2) INU                                         '/ 
      DATA CLINE(5) /'3) All devices                                 '/ 
C
9     CALL SEL(0,0,IBLACK)
      CALL RUB                         !clear display page
      CALL SEL(0,IBACKG,0)             !D green
      CALL BLK(0,0,767,509)            !Fill block with background
      CALL SEL(0,IFOREG,IBACKG)        !cyan/d green
      CALL SCA(13,13,0,3)              !cell width, height, orientation, font
      IY=150
      DO I=1,5
        CALL ALP(30,IY,CLINE(I)(1:LAN(CLINE(I)))) !show 5 lines
        IY=IY+30
      END DO
      CALL FOOT('SELECT',' ',' ',' ',' ','PRINT',' ',' ')
10    CALL ALP(30,IY+30,'Enter selection (1-3) [1]               ')
      CALL SELECT(368,IY+30,IDEV)      !get user input
      IF(IDEV.EQ.0) IDEV=1
!      IF(IDEV.EQ.-8) THEN
!        CALL HELP
!        GOTO 9
!      END IF
      IF(IDEV.LT.-1) GOTO 10
      IF(IDEV.GT.3) GOTO 10      
      IF(IDEV.EQ.-1) RETURN
11    CALL ALP(30,IY+90,'Wind vectors? (0=No 1=Yes) [0]          ')
      CALL SELECT(433,IY+90,IVEC)      !get user input
      IF(IVEC.EQ.-1000) IVEC=0         !User entered explicit 0
!      IF(IVEC.EQ.-8) THEN
!        CALL HELP
!        GOTO 9
!      END IF
      IF(IVEC.LT.-1) GOTO 11
      IF(IVEC.GT.1) GOTO 11     
      IF(IVEC.EQ.-1) IDEV=-1
      RETURN
      END
C*******************************************************************************
      SUBROUTINE GET_SCAL_TRACK(IDEV,ILAT,ILNG)
C
C Subroutines used:
C
C   FOOT         HOR_SUBS
C   SELECT       HOR_UTIL
C   ggs          GGS.OLB
C   
      IMPLICIT  NONE
      INTEGER*4 IDEV,ILAT,ILNG,I,IY,IS,LAN
      CHARACTER CLINE(40)*48
      INCLUDE   'DCOM_DEF.FOR'
      INCLUDE   'HCOM_DEF.FOR'
C
C   contents of menu 4 for aircraft track zoom
C
      DATA CLINE(1) /' ENTER CENTRE POINT OF ENLARGED MAP IN DEGREES '/
      DATA CLINE(2) /' :- eg LAT = 52           ( -90 -->  90N)      '/  
      DATA CLINE(3) /'       LONG= 5            (-180 --> 180E)      '/
      DATA CLINE(4) /'                                               '/
      DATA CLINE(5) /' OR (ENTER) FOR WHOLE MAP                      '/

      IS=STATUS(2)
      IF(IS.LT.1.OR.IS.GT.12800) RETURN
C
      IF(IDEV.EQ.1) THEN
        WRITE(CLINE(6),100) RGLAT(IS)
        WRITE(CLINE(7),110) RGLNG(IS)
      ELSE IF(IDEV.EQ.2) THEN
        WRITE(CLINE(6),100) RILAT(IS)
        WRITE(CLINE(7),110) RILNG(IS)
      ELSE
        WRITE(CLINE(6),100) RGLAT(IS)
        WRITE(CLINE(7),110) RGLNG(IS)
      END IF
C
9     CALL SEL(0,0,IBLACK)
      CALL RUB                         !clear display page
      CALL SEL(0,IBACKG,0)             !D green
      CALL BLK(0,0,767,509)            !Fill block with background
      CALL SEL(0,IFOREG,IBACKG)        !cyan/d green
      CALL SCA(13,13,0,3)              !cell width, height, orientation, font
      CALL ALP(200,110,' MENU D - AIRCRAFT TRACK ZOOM')            
      IY=150
      DO I=1,7
        CALL ALP(10,IY,CLINE(I)(1:LAN(CLINE(I)))) !show 5 lines
        IY=IY+30
      END DO
      CALL FOOT('SELECT','DEVICE','FREQ',' ',' ','PRINT',' ',' ')
10    CALL ALP(10,410,'Enter LAT   (default=whole map) ...      ')
      CALL SELECT(478,410,ILAT)        !Get user input for LAT (whole degrees)
      IF(ILAT.LE.0.AND.ILAT.GE.-3) RETURN !A, B or C pressed, or default
!      IF(ILAT.EQ.-8) THEN
!        CALL HELP
!        GOTO 9
!      END IF
      IF(ILAT.GT.90.OR.ILAT.LT.-1090 .OR.(ILAT.LT.-8.AND.
     -    ILAT.GT.-1000)) GOTO 10
20    CALL ALP(10,450,'Enter LONG (West=-ve) ...               ')
      CALL SELECT(478,450,ILNG)        !Get user input for LNG (whole degrees)
      IF(ILNG.LE.0.AND.ILNG.GE.-3) THEN !A, B or C pressed, or default
        ILAT=ILNG               
        RETURN                         
      END IF
!      IF(ILNG.EQ.-8) THEN
!        CALL HELP
!        GOTO 9
!      END IF
      IF(ILNG.LT.-1180.OR.ILNG.GT.180.OR.(ILNG.LT.-8.AND.
     -    ILNG.GT.-1000)) GOTO 20
      RETURN

100   FORMAT(' CURRENT LATITUDE  =',F7.2)
110   FORMAT(' CURRENT LONGITUDE =',F7.2)
      END
