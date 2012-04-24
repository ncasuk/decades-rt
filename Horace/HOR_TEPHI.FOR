C
C HOR_TEPHI
C HOR_TEPHI1
C HOR_TEPHI_BACKG
C HOR_TEPHI_MENU
C
C*******************************************************************************
      SUBROUTINE HOR_TEPHI
C
C Get the plotting arguments, then call tephi.
C
      IMPLICIT  NONE
      INTEGER*4 ILIST(5),IKEY,IPARA,IFREQ,ISCAL

10    CALL GET_PARA_TEPHI(IPARA)
      IF(IPARA.EQ.-1) RETURN
20    CALL GET_FREQ(IFREQ)
      IF(IFREQ.EQ.-1) RETURN
      IF(IFREQ.EQ.-2) GOTO 10
30    CALL GET_SCAL_TEPHI(ISCAL)
      IF(ISCAL.EQ.-1) RETURN
      IF(ISCAL.EQ.-2) GOTO 10
      IF(ISCAL.EQ.-3) GOTO 20
      ILIST(1)=5
      ILIST(2)=5
      ILIST(3)=IPARA
      ILIST(4)=IFREQ
      ILIST(5)=ISCAL
      CALL UPDT_STATUS(ILIST)
      CALL HOR_TEPHI1(ILIST,IKEY)
      IF(CHAR(IKEY).EQ.'A') RETURN
      IF(CHAR(IKEY).EQ.'B') GOTO 10
      IF(CHAR(IKEY).EQ.'C') GOTO 20
      IF(CHAR(IKEY).EQ.'D') GOTO 30
      END
C*******************************************************************************
      SUBROUTINE HOR_TEPHI1(ILIST,IKEY)
C
C  Plots temperature and dewpoint(user selected) on a tephigram constructed
C  in a large virtual window, the area to be displayed is user selected and
C  then enlarged into the viewport area on the screen, plots past data 
C  30,60,90,120 mins then updates at 3,6,9,12 secs(user selected)   
C
C External data:
C
C DERIVE(12800,196) R*4  Passed  HOR_CALCS    A circular buffer of 196 
C                                            derived parameters,calculated once
C                                            every third second.(see PARANO.TXT)
C
C SPECIAL(12800,10) I*2  Passed HOR_CALCS     A circular buffer of 10 parameters
C                                            (time,event,flags),calculated once
C                                            every third second.
C Subroutines called:
C
C HOR_TEPHI_BACKG  This file                Constructs background tephigram
C HOR_TEPHI_KEY    This file
c TFPTC            This file                Calculates x & y from temp & press 
c TFCON            This file                Converts real x,y to screen ints
C      
      IMPLICIT  NONE
      INTEGER*4 IWIN(4,5)
      DATA IWIN /  60, 150,5165,3650,
     -            792,2199,3293,3650,
     -           1464,2199,3906,3650,
     -           2272,2199,4739,3650,
     -            911, 150,4739,2284/
      INTEGER*4 ILIST(5),IPARA,IFREQ,ISCAL,IS,IKEY,ITCNT,IXC,IC,IP,
     -    IX,IY,IX1,IY1,IX2,IY2,IXM,IYM,ITIM,LSTTX,LSTTY,LSTDX,
     -    LSTDY,LSTTWX,LSTTWY,LSTFDX,LSTFDY,I,ILSTCTL
      LOGICAL*4 LBIT
      INTEGER*2 IBLK1(500),IBLK2(500)
      INTEGER*4 ITMPTX(12800),ITMPTY(12800),ITMPDX(12800),ITMPDY(12800)
      INTEGER*4 ITMPFDX(12800),ITMPFDY(12800),ITMPTWX(12800),
     -    ITMPTWY(12800),ICTL(12800)
      REAL*4    SCFACT,XSFACT,YSFACT,X,Y
      COMMON    /SCALE/ SCFACT,XSFACT,YSFACT,IWIN
      INCLUDE   'DCOM_DEF.FOR'
      INCLUDE   'HCOM_DEF.FOR'
C 
C Start displaying-write heads on first and second time through only
C
      IPARA=ILIST(3)
      IFREQ=ILIST(4)
      ISCAL=ILIST(5)
      IKEY=ICHAR('A')                  !Default return
      IF(STATUS(2).LT.1.OR.STATUS(2).GT.12800) RETURN
C
C Write fixed parts of the display to the access page the first time the
C routine is called.
C
      CALL BBK(IBLK1,IBLK2,500)        !start double blocking
      CALL SEL(0,IBLACK,IBLACK)        !black
      CALL RUB                         !clear display page
      CALL HEAD_SHELL       
      CALL FOOT('SELECT','PARAS','FREQ','ZOOM',' ','PRINT',' ',' ')
      CALL HOR_TEPHI_BACKG(ISCAL,IKEY) !draw tephi background 
      IF(IKEY.NE.0) GOTO 5000
        
      IF(IPARA.EQ.1.OR.IPARA.EQ.4.OR.IPARA.EQ.5) THEN
        CALL SEL(0,IYELLOW,-1)
        CALL ALP(600,153,'GE DP')
      END IF

      IF(IPARA.EQ.2.OR.IPARA.EQ.4) THEN
        CALL SEL(0,IGREEN,-1)
        CALL ALP(600,230,'TWC DP')
      END IF

      IF(IPARA.EQ.5.OR.IPARA.EQ.3) THEN
        CALL SEL(0,IGREEN,-1)
        CALL ALP(600,230,'FWVS DP')
      END IF
C     
      DO I=1,12800                     !clear temp arrays initially
        ITMPTX(I)=0
        ITMPTY(I)=0
        ITMPDX(I)=0
        ITMPDY(I)=0
        ITMPTWX(I)=0
        ITMPTWY(I)=0
        ITMPFDX(I)=0
        ITMPFDY(I)=0
        ICTL(I)=0
      END DO

      IX1=IWIN(1,ISCAL)                !set up correct window
      IY1=IWIN(2,ISCAL)                !for required input
      IX2=IWIN(3,ISCAL)
      IY2=IWIN(4,ISCAL)
      IXM=NINT(ABS(REAL(IX1-IX2)/REAL(571-60))*50)
      IYM=NINT(ABS(REAL(IY1-IY2)/REAL(500-150))*50)

C
      IF(IFREQ.EQ.5) GOTO 5020         !current data only
C
C  Get x,y positions for past data and store in arrays
C
      ITCNT=0                          !initial value array stores
      IXC=669                          !counter for data loop
      IC=STATUS(3)                     !pointer for circular buffer
      IP=STATUS(2)                     !pointer to past data  
      IS=STATUS(2)                     !current data pointer
      DO WHILE(MOD(NINT(RSECS(IP)),IFREQ*3).NE.0.AND.IC.GT.0) !Align data
        IC=IC-1                        !decr pointer 
        IP=IP-1                        !decr pointer
        IF(IP.LE.0) IP=IP+12800
      END DO
C  Work back through data every 3rd sec or as input frequency
      DO WHILE(IC.GT.0.AND.IXC.GE.101)        
        CALL CHECK_KEY('ABCD',IKEY)
        IF(IKEY.NE.0) GOTO 5000
C  Calculate x,y positions
        ITCNT=ITCNT+1                  !increment array count
        ICTL(ITCNT)=SPECIAL(IP,9)
        IF(ICTL(ITCNT).EQ.0) GOTO 5030
        CALL TFPTC(RSPR(IP),RTATDC(IP),X,Y) !calc x,y position of temp
        CALL TFCON(X,Y,IX,IY)          !convert to screen coords
        ITMPTX(ITCNT)=IX               !store positions 
        ITMPTY(ITCNT)=IY   
        IF(ITCNT.EQ.1) LSTTX=IX
        IF(ITCNT.EQ.1) LSTTY=IY
C
        IF(IPARA.EQ.1.OR.IPARA.EQ.4.OR.IPARA.EQ.5) THEN !auto hygrometer
          CALL TFPTC(RSPR(IP),RDEW(IP),X,Y) !calc x,y position of dewpt
          CALL TFCON(X,Y,IX,IY)        !convert to screen coords
          ITMPDX(ITCNT)=IX             !store positions    
          ITMPDY(ITCNT)=IY   
          IF(ITCNT.EQ.1) LSTDX=IX
          IF(ITCNT.EQ.1) LSTDY=IY
        END IF
C
        IF(IPARA.EQ.2.OR.IPARA.EQ.4) THEN !TWC dewpoint
          IF(RTWC(IP).NE.0.0) THEN     !if available 
            CALL TFPTC(RSPR(IP),RTWCDP(IP),X,Y) !calc x,y position of dewpt
            CALL TFCON(X,Y,IX,IY)      !convert to screen coords
            ITMPTWX(ITCNT)=IX          !store positions
            ITMPTWY(ITCNT)=IY
            IF(ITCNT.EQ.1) LSTTWX=IX
            IF(ITCNT.EQ.1) LSTTWY=IY
          ELSE
            IF(ITCNT.EQ.1) LSTTWX=999
            IF(ITCNT.EQ.1) LSTTWY=999
          END IF
        END IF  
C
        IF(IPARA.EQ.3.OR.IPARA.EQ.5) THEN !Fluorescence dewpoint
          IF(IFLDP(IP).EQ.0) THEN      !if available 
            CALL TFPTC(RSPR(IP),RFLDP(IP),X,Y) !calc x,y position of dewpt
            CALL TFCON(X,Y,IX,IY)      !convert to screen coords
            ITMPFDX(ITCNT)=IX          !store positions
            ITMPFDY(ITCNT)=IY
            IF(ITCNT.EQ.1) LSTFDX=IX
            IF(ITCNT.EQ.1) LSTFDY=IY
          ELSE
            IF(ITCNT.EQ.1) LSTFDX=999
            IF(ITCNT.EQ.1) LSTFDY=999
          END IF
        END IF  
C     
5030    IXC=IXC-1                      !adjust counters & pointers
        IC=IC-IFREQ                             
        IP=IP-IFREQ
        IF(IP.LE.0) IP=IP+12800        !recycle when required
      END DO        
C
      CALL WIN(IX1,IY1,IX2,IY2)               
      CALL VPT(60,150,571,500)         !set up viewport    
C
      CALL SEL(0,IFOREG,-1)            !cyan for temp
      IF(ICTL(1).NE.0) CALL VEC(ITMPTX(1),ITMPTY(1),ITMPTX(1),ITMPTY(1)) !first point
      DO I=2,ITCNT                     !plot lines for rest
        CALL CHECK_KEY('ABCD',IKEY)
        IF(IKEY.NE.0) GOTO 5010
        IF(ICTL(I).NE.0) THEN
          IX=ITMPTX(I)                 !of array
          IY=ITMPTY(I)
          LBIT=.FALSE.
          IF(ABS(IX-ITMPTX(I-1)).GT.IXM) LBIT=.TRUE.
          IF(ABS(IY-ITMPTY(I-1)).GT.IYM) LBIT=.TRUE.
          IF(ICTL(I-1).EQ.0) LBIT=.TRUE.
          IF(LBIT) CALL VEC(IX,IY,IX,IY)
          IF(.NOT.LBIT) CALL VEC(IX,IY)
        END IF
      END DO
C
      IF(IPARA.EQ.1.OR.IPARA.EQ.4.OR.IPARA.EQ.5) THEN !auto hygrometer
        CALL SEL(0,IYELLOW,-1)         !yellow for auto dewpt
        IF(ICTL(1).NE.0) 
     -      CALL VEC(ITMPDX(1),ITMPDY(1),ITMPDX(1),ITMPDY(1)) !first point
        DO I=2,ITCNT                   !plot lines for rest
          CALL CHECK_KEY('ABCD',IKEY)
          IF(IKEY.NE.0) GOTO 5010
          IF(ICTL(I).NE.0) THEN
            IX=ITMPDX(I)               !of array
            IY=ITMPDY(I)
            LBIT=.FALSE.
            IF(ABS(IX-ITMPDX(I-1)).GT.IXM) LBIT=.TRUE.
            IF(ABS(IY-ITMPDY(I-1)).GT.IYM) LBIT=.TRUE.
            IF(ICTL(I-1).EQ.0) LBIT=.TRUE.
            IF(LBIT) CALL VEC(IX,IY,IX,IY)
            IF(.NOT.LBIT) CALL VEC(IX,IY)
          END IF
        END DO
      END IF
C
      IF(IPARA.EQ.2.OR.IPARA.EQ.4) THEN !TWC dew pt
        CALL SEL(0,IGREEN,-1)          !green for TWC dew pt 
        IF(ICTL(1).NE.0) 
     -      CALL VEC(ITMPTWX(1),ITMPTWY(1),ITMPTWX(1),ITMPTWY(1)) !first point
        DO I=2,ITCNT                   !plot lines for rest
          CALL CHECK_KEY('ABCD',IKEY)
          IF(IKEY.NE.0) GOTO 5010
          IF(ICTL(I).NE.0) THEN
            IX=ITMPTWX(I)              !of array
            IY=ITMPTWY(I)
            LBIT=.FALSE.
            IF(ABS(IX-ITMPTWX(I-1)).GT.IXM) LBIT=.TRUE.
            IF(ABS(IY-ITMPTWY(I-1)).GT.IYM) LBIT=.TRUE.
            IF(ICTL(I-1).EQ.0) LBIT=.TRUE.
            IF(LBIT) CALL VEC(IX,IY,IX,IY)
            IF(.NOT.LBIT) CALL VEC(IX,IY)
          END IF
        END DO
      END IF
C
      IF(IPARA.EQ.3.OR.IPARA.EQ.5) THEN !flwvs dew pt
        CALL SEL(0,IGREEN,-1)          !green for fl dew pt 
        IF(ICTL(1).NE.0) 
     -      CALL VEC(ITMPFDX(1),ITMPFDY(1),ITMPFDX(1),ITMPFDY(1)) !first point
        DO I=2,ITCNT                   !plot lines for rest
          CALL CHECK_KEY('ABCD',IKEY)
          IF(IKEY.NE.0) GOTO 5010
          IF(ICTL(I).NE.0) THEN
            IX=ITMPFDX(I)              !of array
            IY=ITMPFDY(I)
            LBIT=.FALSE.
            IF(ABS(IX-ITMPFDX(I-1)).GT.IXM) LBIT=.TRUE.
            IF(ABS(IY-ITMPFDY(I-1)).GT.IYM) LBIT=.TRUE.
            IF(ICTL(I-1).EQ.0) LBIT=.TRUE.
            IF(LBIT) CALL VEC(IX,IY,IX,IY)
            IF(.NOT.LBIT) CALL VEC(IX,IY)
          END IF
        END DO
      END IF
C     
      CALL WIN
      CALL VPT
5020  CALL EBK
      ILSTCTL=0
      GOTO 110

5010  CALL WIN                         !reset window to normal
      CALL VPT                         !reset viewport to normal
5000  CALL EBK                         !empty last block 
      RETURN
C
C Plot the data
C
100   IS=IS+1
      IF(IS.GT.12800) IS=IS-12800
      CALL HEAD_UPDATE
      ITIM=NINT(RSECS(IS))
      IF(IFREQ.EQ.5) IFREQ=1           !no past data -plot every 3
      IF(MOD(ITIM,IFREQ*3).EQ.0) THEN  !plot every 3,6,9,or12 secs
        CALL WIN(IX1,IY1,IX2,IY2)
        CALL VPT(60,150,571,500)
        IF(SPECIAL(IS,9).EQ.0) GOTO 120 !No data
C
        CALL TFPTC(RSPR(IS),RTATDC(IS),X,Y) !calc x,y position of temp
        CALL TFCON(X,Y,IX,IY)          !convert to screen coords
        CALL SEL(0,IFOREG,-1)          !Cyan
        LBIT=.FALSE.
        IF(ABS(IX-LSTTX).GT.IXM) LBIT=.TRUE.
        IF(ABS(IY-LSTTY).GT.IYM) LBIT=.TRUE.
        IF(ILSTCTL.EQ.0) LBIT=.TRUE.
        IF(LBIT) THEN                  !Test for first data
          CALL VEC(IX,IY,IX,IY)        !plot temp           
        ELSE
          CALL VEC(LSTTX,LSTTY,IX,IY)  !draw line from last temp
        END IF
        LSTTX=IX                       !save temp plot position
        LSTTY=IY
C
        IF(IPARA.EQ.1.OR.IPARA.EQ.4.OR.IPARA.EQ.5) THEN !auto hygrometer
          CALL TFPTC(RSPR(IS),RDEW(IS),X,Y) !calc x,y position of dewpt
          CALL TFCON(X,Y,IX,IY)        !convert to screen coords
          CALL SEL(0,IYELLOW,-1)       !else yellow
          LBIT=.FALSE.
          IF(ABS(IX-LSTDX).GT.IXM) LBIT=.TRUE.
          IF(ABS(IY-LSTDY).GT.IYM) LBIT=.TRUE.
          IF(ILSTCTL.EQ.0) LBIT=.TRUE.
          IF(LBIT) THEN                !Test for first data
            CALL VEC(IX,IY,IX,IY)      !plot temp           
          ELSE
            CALL VEC(LSTDX,LSTDY,IX,IY) !draw line from last dewpt
          END IF
          LSTDX=IX                     !save dewpt plot position
          LSTDY=IY
        END IF
C
        IF(IPARA.EQ.2.OR.IPARA.EQ.4) THEN !TWC dewpoint
          IF(RTWC(IS).NE.0.0) THEN     !if available   
            CALL TFPTC(RSPR(IS),RTWCDP(IS),X,Y) !calc x,y position of dewpt
            CALL TFCON(X,Y,IX,IY)      !convert to screen coords
            CALL SEL(0,IGREEN,-1)      !plot in green
            LBIT=.FALSE.
            IF(ABS(IX-LSTTWX).GT.IXM) LBIT=.TRUE.
            IF(ABS(IY-LSTTWY).GT.IYM) LBIT=.TRUE.
            IF(ILSTCTL.EQ.0) LBIT=.TRUE.
            IF(LBIT) THEN              !Test for first data
              CALL VEC(IX,IY,IX,IY)    !plot temp           
            ELSE
              CALL VEC(LSTTWX,LSTTWY,IX,IY) !draw line from last dewpt
            END IF
            LSTTWX=IX                  !save dewpt plot position
            LSTTWY=IY
          ELSE
            LSTTWX=999
            LSTTWY=999
          END IF
        END IF  
C
        IF(IPARA.EQ.3.OR.IPARA.EQ.5) THEN !Fluorescence dewpoint
          IF(IFLDP(IS).EQ.0) THEN      !if available   
            CALL TFPTC(RSPR(IS),RFLDP(IS),X,Y) !calc x,y position of dewpt
            CALL TFCON(X,Y,IX,IY)      !convert to screen coords
            CALL SEL(0,IGREEN,-1)      !plot in green
            LBIT=.FALSE.
            IF(ABS(IX-LSTFDX).GT.IXM) LBIT=.TRUE.
            IF(ABS(IY-LSTFDY).GT.IYM) LBIT=.TRUE.
            IF(ILSTCTL.EQ.0) LBIT=.TRUE.
            IF(LBIT) THEN              !Test for first data
              CALL VEC(IX,IY,IX,IY)    !plot temp           
            ELSE
              CALL VEC(LSTFDX,LSTFDY,IX,IY) !draw line from last dewpt
            END IF
            LSTFDX=IX                  !save dewpt plot position
            LSTFDY=IY
          ELSE
            LSTFDX=999
            LSTFDY=999
          END IF
        END IF  
C     
120     ILSTCTL=SPECIAL(IS,9)
        CALL WIN                       !Reset default screen size
        CALL VPT                       !reset default viewport 
      END IF
      CALL EBK                         !empty last block
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
      SUBROUTINE HOR_TEPHI_BACKG(ISCAL,IKEY)
C
C Draw the tephi background
C
      IMPLICIT  NONE
      INTEGER*4 IWIN(4,5),ISCAL,IKEY,IX1,IY1,IX2,IY2
      REAL*4    SCFACT,XSFACT,YSFACT
      COMMON    /SCALE/ SCFACT,XSFACT,YSFACT,IWIN
      INCLUDE   'DCOM_DEF.FOR'
C
      SCFACT=29.78                     !set up scaling factors
      XSFACT=1744.57
      YSFACT=1278.27
C     
      IX1=IWIN(1,ISCAL)                !Set up window coords
      IY1=IWIN(2,ISCAL)                !from user input   
      IX2=IWIN(3,ISCAL)
      IY2=IWIN(4,ISCAL)
      IF(ISCAL.NE.1) THEN
        CALL SEL(0,ILGREY,IBACKG)      !L grey/d green 
        CALL BOXA(60,150,571,500)
      END IF
      CALL WIN(IX1,IY1,IX2,IY2)
      CALL VPT(60,150,571,500)         !define screen area 
      CALL TPHIEXT                     !calculate exterior
      CALL SEL(0,IBACKG,ILGREY)        !LIGHT grey boundary
      IF(ISCAL.EQ.5) THEN
        CALL FLO(1000,1000)
      ELSE
        CALL FLO(3000,3000)            !Fill shape with d green
      END IF
      CALL SEL(0,ILGREY,IBACKG)        !L grey/d green 
      CALL CHECK_KEY('ABCD',IKEY)
      IF(IKEY.NE.0) GOTO 10
      CALL TFHMR                       !construct hum mixing ratios
      CALL CHECK_KEY('ABCD',IKEY)
      IF(IKEY.NE.0) GOTO 10
      CALL TFDALR                      !construct dry adiabats
      CALL CHECK_KEY('ABCD',IKEY)
      IF(IKEY.NE.0) GOTO 10
      CALL TFSALR                      !construct sat. adiabats
      CALL CHECK_KEY('ABCD',IKEY)
      IF(IKEY.NE.0) GOTO 10
      CALL TPHITEMP(ISCAL)             !construct isotherms
      CALL CHECK_KEY('ABCD',IKEY)
      IF(IKEY.NE.0) GOTO 10
      CALL TPHIBAR(ISCAL)              !construct isobars      
10    CALL WIN                         !reset normal screen coords  
      CALL VPT                         !reset normal screen coords
      RETURN
      END
C*******************************************************************************
      SUBROUTINE TPHIEXT
C
C Calculates exterior coords and constructs sides
C
      IMPLICIT  NONE
      INTEGER*4 IX0,IY0,IX1,IY1,IX2,IY2,IX3,IY3,IXS,IYS,IXF,IYF
      REAL*4    X,Y,TRYTEMP,T
      INCLUDE   'DCOM_DEF.FOR'
C
      CALL SEL(0,ILGREY,IBLACK)        !L Grey
      CALL TFPTC(1050.0,-40.0,X,Y)     !tephi origin bottom left
      CALL TFCON(X,Y,IX0,IY0)          !convert to screen coords
      TRYTEMP=-89.0                    !first guess at temp 
      CALL TFPXC(250.0,X,TRYTEMP,Y,T)  !top left corner             
      CALL TFCON(X,Y,IX1,IY1)          !convert to screen coords
      CALL VEC(IX0,IY0,IX1,IY1)        !draw line bott to top left
C
      CALL TFPTC(1050.0,45.0,X,Y)      !tephi origin bottom right
      CALL TFCON(X,Y,IX3,IY3)          !convert to screen coords
      TRYTEMP=-14.0                    !first guess at temp 
      CALL TFPXC(250.0,X,TRYTEMP,Y,T)  !top right corner             
      CALL TFCON(X,Y,IX2,IY2)          !convert to screen coords
      CALL VEC(IX2,IY2,IX3,IY3)        !draw line bott to top right
C
      IXS=IX0                          !1050mb isobar
      IYS=IY0                          !initial point left side
      DO T=-35.0,45.0,5.0              !plot isobar at 5 degree Xsect
        CALL TFPTC(1050.0,T,X,Y)
        CALL TFCON(X,Y,IXF,IYF)               
        CALL VEC(IXS,IYS,IXF,IYF)      !draw section
        IXS=IXF                               
        IYS=IYF
      END DO
C
      IXS=IX1                          !250mb isobar
      IYS=IY1                          !initial point left side
      DO T=-85.0,-15.0,5.0             !plot isobar at 5 degree Xsect
        CALL TFPTC(250.0,T,X,Y)        !to nearly end 
        CALL TFCON(X,Y,IXF,IYF)
        CALL VEC(IXS,IYS,IXF,IYF)      !draw section
        IXS=IXF
        IYS=IYF
      END DO
      IXF=IX2                          !top right
      IYF=IY2
      CALL VEC(IXS,IYS,IXF,IYF)        !plot last section
      RETURN
      END
C*******************************************************************************
      SUBROUTINE TPHIBAR(ISCAL)
C
C Constructs isobars plotting every 5 degree isotherm intersections
C
      IMPLICIT  NONE
      CHARACTER CBAR*4,CFL*5
      INTEGER*4 IWIN(4,5),ISCAL,IXS,IYS,IXF,IYF,IP,IXLAB,IFL,IYFL,IYLAB,
     -    IOS
      REAL*4    TRYTEMP,TRYTEMP2,Y,T,TS,TF,P,RINC,X,SCFACT,XSFACT,
     -    YSFACT,TEMP
      COMMON    /SCALE/ SCFACT,XSFACT,YSFACT,IWIN
      INCLUDE   'DCOM_DEF.FOR'

      TRYTEMP=-85.0                    !first lhstemp for firstlevel
      TRYTEMP2=-10.0                   !first rhs temp for first
      RINC=9.0
      CALL SCA(8,10,0,2)
      DO P=300.0,1000.0,100.0          !loop through press levels
        CALL TFPXC(P,0.0,TRYTEMP,Y,T)  !Find y left side 
        CALL TFCON(0.0,Y,IXS,IYS)      !convert to screen coords
        IYLAB=IYS                      !Save y position for label    
        TS=T                           !use temp for loop
        TF=TS+75.0
        IYFL=IYS
        DO TEMP=TS,TF,5.0              !plot isobar at 5 degree Xsect
          CALL TFPTC(P,TEMP,X,Y)       !to nearly end 
          CALL TFCON(X,Y,IXF,IYF)      !convert to screen coords
          CALL SEL(0,ILGREY,IBACKG)    !L grey/d green
          CALL VEC(IXS,IYS,IXF,IYF)    !draw section
          IF(IXS.LE.IWIN(3,ISCAL).AND.IXF.GE.IWIN(3,ISCAL)) 
     -        IYFL=IYS
          IXS=IXF
          IYS=IYF
        END DO
        CALL TFPXC(P,5105.0,TRYTEMP2,Y,T) !find y right side
        CALL TFCON(5105.0,Y,IXF,IYF)   !convert to screen coords
        CALL VEC(IXS,IYS,IXF,IYF)      !plot last section
        IF(IXS.LE.IWIN(3,ISCAL).AND.IXF.GE.IWIN(3,ISCAL)) 
     -        IYFL=IYS
        CALL SEL(0,IFOREG,IBLACK)      !cyan/black label 
        IP=NINT(P)
        WRITE(CBAR,'(I4)',IOSTAT=IOS) IP !label isobar
        IXLAB=IWIN(1,ISCAL)            !get x pos for label  
        IF(ISCAL.EQ.1) CALL ALP(IXLAB+10,IYLAB-80,CBAR)
        IF(ISCAL.EQ.2) CALL ALP(IXLAB+10,IYLAB-100,CBAR)
        IF(ISCAL.EQ.3) CALL ALP(IXLAB+10,IYLAB-150,CBAR)
        IF(ISCAL.EQ.4) CALL ALP(IXLAB+10,IYLAB-180,CBAR)
        IF(ISCAL.EQ.5) CALL ALP(IXLAB+10,IYLAB-150,CBAR)
        IFL=NINT(44330.77*(1.0-(P/1013.25)**(1.0/5.256))*0.03281)
        WRITE(CFL,'(A,I3.3)',IOSTAT=IOS) 'FL',IFL !Flight level
        IXLAB=IWIN(1,ISCAL)+(IWIN(3,ISCAL)-IWIN(1,ISCAL))*87/100
        IF(ISCAL.EQ.1) CALL ALP(IXLAB,IYFL-52,CFL)
        IF(ISCAL.EQ.2) CALL ALP(IXLAB,IYFL-22,CFL)
        IF(ISCAL.EQ.3) CALL ALP(IXLAB,IYFL-22,CFL)
        IF(ISCAL.EQ.4) CALL ALP(IXLAB,IYFL-22,CFL)
        IF(ISCAL.EQ.5) CALL ALP(IXLAB,IYFL-32,CFL)
        TRYTEMP2=TRYTEMP2+RINC         !incr guesstemps for new level
        TRYTEMP=TRYTEMP+RINC
        RINC=RINC-1.0
      END DO
      RETURN
      END 
C*******************************************************************************
      SUBROUTINE TPHITEMP(ISCAL)
C
C  Constructs isotherms
C
      IMPLICIT  NONE
      INTEGER*4 IWIN(4,5),ISCAL,IXS,IYS,IXF,IYF,IT,IOS
      REAL*4    SCFACT,XSFACT,YSFACT,TRYP,T,XS,YS,XF,YF,Y,P,X
      CHARACTER CTEMP*3
      COMMON    /SCALE/ SCFACT,XSFACT,YSFACT,IWIN
      INCLUDE   'DCOM_DEF.FOR'
C
C  First section where top and bottom of isotherms are 250,1050mbs on large
C  tephi -40----> -20   
C
      TRYP=250.0                       !first guess at pressure
      DO T= -40.0,-20.0,10.0           !Top and bottom points known
        CALL TFPTC(1050.0,T,XS,YS)     !bottom of isotherm
        CALL TFCON(XS,YS,IXS,IYS)      !convert to screen coords
        CALL TFPTC(250.0,T,XF,YF)      !top of isotherm
        CALL TFCON(XF,YF,IXF,IYF)      !convert to screen coords
        CALL SEL(0,ILGREY,IBACKG)      !L grey/dgreen
        CALL VEC(IXS,IYS,IXF,IYF)      !draw isotherm
        CALL SEL(0,IFOREG,IBLACK)      !cyan/black label
        CALL SCA(8,10,0,2)
        IT=NINT(T)
        WRITE(CTEMP,'(I3)',IOSTAT=IOS) IT !label isotherms
        IF(ISCAL.GT.1.AND.ISCAL.LT.5) THEN   !put labels at 600mbs 
          CALL TFPTC(600.0,T,XF,YF)    !on expanded sections
          CALL TFCON(XF,YF,IXF,IYF)    !convert to screen coords
          CALL ALP(IXF-150,IYF+40,CTEMP) !write label
        ELSE
          CALL ALP(IXF-300,IYF+90,CTEMP) !label at 250mbs on 1 & 5 
        END IF
c        IF (ISCAL.EQ.5) THEN          !label isotherms on right  
c          IF (T.EQ.-10.0) THEN        !on high level section
c            CALL TFTXC(T,4739.0,TRYP-150.0,Y,P) !find coord for xsect
c            CALL TFCON(4739.0,Y,IXS,IYS) !convert to screen coords
c            CALL ALP(IXS-200,IYS+50,CTEMP) !write label 
c          END IF
c        END IF
        TRYP=TRYP+100.0                !next pressure
      END DO
C
C  Second section where isotherms have unknown left axis position on
C  whole tephi   -80----> -50
C
      TRYP=300.0                       !first guess at pressure
      DO T= -80.0,-50.0,10.0           !isotherms Xsect left axis
        CALL TFTXC(T,0.0,TRYP,Y,P)     !left axis
        CALL TFCON(0.0,Y,IXS,IYS)      !convert to screen coords
        CALL TFPTC(250.0,T,X,Y)        !top of isotherm 
        CALL TFCON(X,Y,IXF,IYF)        !convert to screen coords
        CALL SEL(0,ILGREY,IBACKG)      !L grey/d green
        CALL VEC(IXS,IYS,IXF,IYF)      !draw isotherm 
        CALL SEL(0,IFOREG,IBLACK)      !cyan/black label 
        IT=NINT(T)
        WRITE(CTEMP,'(I3)',IOSTAT=IOS) IT !label isotherm
        IF(ISCAL.GT.1.AND.ISCAL.LT.5) THEN !put labels at 600mbs 
          CALL TFPTC(600.0,T,XF,YF)    !on expanded sections
          CALL TFCON(XF,YF,IXF,IYF)    !convert to screen coords
        END IF
        CALL ALP(IXF-150,IYF+40,CTEMP) !write label 
        TRYP=TRYP+100.0                !increment first guess 
      END DO
C
C  Third section where isotherms have unknown right axis position on
C  whole tephi   -10----> +40
C
      TRYP=250.0                       !first guess at pressure
      DO T= -10.0,40.0,10.0            !isotherms Xsect right axis
        CALL TFTXC(T,5110.0,TRYP,Y,P)  !right axis
        CALL TFCON(5110.0,Y,IXS,IYS)   !convert to screen coords
        CALL TFPTC(1050.0,T,X,Y)       !bottom of isotherm 
        CALL TFCON(X,Y,IXF,IYF)        !convert to screen coords
        IF(T.EQ.0.0) THEN              !draw dashed line for 0C
          CALL SLT(7)
        ELSE
          CALL SLT(0)
        END IF
        CALL SEL(0,ILGREY,IBACKG)      !L grey/d green  
        CALL VEC(IXS,IYS,IXF,IYF)      !draw isotherm 
        CALL SEL(0,IFOREG,IBLACK)      !cyan/black labels
        IT=NINT(T)
        WRITE(CTEMP,'(I3)',IOSTAT=IOS) IT !label isotherms
        IF(ISCAL.EQ.1) THEN            !put labels at 250mbs 
          CALL ALP(IXS-350,IYS+50,CTEMP) !on whole tephi
        ELSE IF(ISCAL.EQ.2) THEN      !Zoom 2
          IF(T.EQ.-10.0) THEN               
            CALL TFPTC(600.0,T,XF,YF)  !600mbs on expanded sections
            CALL TFCON(XF,YF,IXF,IYF)  !convert to screen coords
            CALL ALP(IXF-150,IYF+40,CTEMP) !write label
          ELSE  IF(T.LE.10.0) THEN             
            CALL TFTXC(T,3293.0,TRYP+400.0,Y,P) !or right axis
            CALL TFCON(3293.0,Y,IXS,IYS) !convert to screen coords
            CALL ALP(IXS-200,IYS+50,CTEMP) !write label
          END IF 
        ELSE IF(ISCAL.EQ.3) THEN       !Zoom 3
          IF(T.LE.0.0) THEN 
            CALL TFPTC(600.0,T,XF,YF)  !600mbs on expanded sections
            CALL TFCON(XF,YF,IXF,IYF)  !convert to screen coords
            CALL ALP(IXF-150,IYF+40,CTEMP) !write label 
          ELSE IF(T.LE.20.0) THEN
            CALL TFTXC(T,3906.0,TRYP+300.0,Y,P) !or right axis
            CALL TFCON(3906.0,Y,IXS,IYS) !convert to screen coords 
            CALL ALP(IXS-200,IYS+50,CTEMP) !write label
          END IF
        ELSE IF(ISCAL.EQ.4) THEN       !Zoom 4   
          IF(T.LE.10.0) THEN                
            CALL TFPTC(600.0,T,XF,YF)  !600mbs on expanded sections
            CALL TFCON(XF,YF,IXF,IYF)  !convert to screen coords
            CALL ALP(IXF-150,IYF+40,CTEMP) !write label 
          ELSE  
            CALL TFTXC(T,4739.0,TRYP+100.0,Y,P) !or right axis
            CALL TFCON(4739.0,Y,IXS,IYS) !convert to screen coords
            CALL ALP(IXS-200,IYS+50,CTEMP) !write label
          END IF
        ELSE IF(ISCAL.EQ.5) THEN       !Zoom 5
          IF(T.LE.10.0) THEN
            CALL TFTXC(T,4739.0,TRYP+50.0,Y,P) !right axis
            CALL TFCON(4739.0,Y,IXS,IYS) !convert to screen coords
            CALL ALP(IXS-230,IYS+50,CTEMP) !write label
          END IF 
        END IF
        TRYP=TRYP+100.0                !increment first guess 
      END DO
      RETURN
      END           
C*******************************************************************************
      SUBROUTINE TFDALR
C
C Constructs dry adiabats
C
      IMPLICIT  NONE
      INTEGER*4 IXS,IYS,IXF,IYF
      REAL*4    X,Y,T,S,YC,TDALR,TOPT
      INCLUDE   'DCOM_DEF.FOR'

      DO TDALR=0.0,40.0,10.0           !draw 0->40 adiabats
        CALL TFPTC(1000.0,TDALR,X,Y)   !find x and y at 1000mb  
        YC=X+Y 
        DO T=TDALR,TDALR+20.0,0.5      !loop to find 1050mb position
           CALL TFPTC(1050.0,T,X,Y)    !find x and y at bottom
           S=X+Y-YC
           IF(S.GE.0.0) GOTO 10
        END DO
10      CALL TFCON(X,Y,IXS,IYS)        !convert bottom to screen 
        T=-90.0 + TDALR/2.0            !first guess at temp top
        DO TOPT=T,T+20.0,0.5           !loop to find 250mb position
           CALL TFPTC(250.0,TOPT,X,Y)  !find x and y at top
           S=X+Y
           IF((S-YC).GE.0.0) GOTO 20
        END DO
20      CALL TFCON(X,Y,IXF,IYF)        !convert to screen coords
        CALL VEC(IXS,IYS,IXF,IYF)      !draw adiabat
      END DO
C
      DO TDALR=-40.0,-10.0,10.0        !draw -40->-10 adiabats
        CALL TFPTC(1000.0,TDALR,X,Y)   !find x and y at 1000mb  
        YC=X+Y 
        DO T=TDALR,TDALR+20.0,0.5      !loop to find 1050mb position
           CALL TFPTC(1050.0,T,X,Y)    !find x and y at bottom
           S=X+Y-YC
           IF(S.GE.0.0) GOTO 30
        END DO
30      CALL TFCON(X,Y,IXS,IYS)        !convert bott to screen coords
        CALL TFCON(0.0,YC,IXF,IYF)     !convert left to screen coords
        CALL VEC(IXS,IYS,IXF,IYF)      !draw adiabat
      END DO
C
      DO TDALR=50.0,110.0,10.0         !draw 50->110 adiabats
        CALL TFPTC(1000.0,TDALR,X,Y)   !find x and y at 1000mb  
        YC=X+Y 
        CALL TFCON(5105.0,YC-5105.0,IXS,IYS) !convert bottom to screen 
        T=-95.0+TDALR/2.0              !first guess at temp top
        DO TOPT=T,T+25.0,0.5           !loop to find 250mb position
           CALL TFPTC(250.0,TOPT,X,Y)  !find x and y at top
           S=X+Y
           IF((S-YC).GE.0.0) GOTO 40
        END DO
40      CALL TFCON(X,Y,IXF,IYF)        !convert to screen coords
        CALL VEC(IXS,IYS,IXF,IYF)      !draw adiabat
      END DO
      RETURN
      END
C*******************************************************************************
      SUBROUTINE TFSALR
C
C  Contructs saturated adiabatic curves every 10 degrees from a 
C  series of points calculated from temperature and pressure at 50 mb ints.
C
      IMPLICIT  NONE
      INTEGER*4 NOPTS(8),I,J,IXS,IYS,IXF,IYF
      REAL*4    SATDAT(17,8),PLAST(8),X,Y,P
      INCLUDE   'DCOM_DEF.FOR'
      DATA SATDAT /-31.9,-35.0,-38.4,-41.9,-45.5,-50.0,-50.0,-50.0,
     -       -50.0,-50.0,-50.0,-50.0,-50.0,-50.0,-50.0,-50.0,-50.0,
     -             -21.8,-25.0,-28.3,-31.8,-35.5,-39.5,-43.7,-50.0,
     -       -50.0,-50.0,-50.0,-50.0,-50.0,-50.0,-50.0,-50.0,-50.0,
     -             -12.0,-15.0,-18.3,-21.7,-25.3,-29.1,-33.2,-37.7,
     -       -42.4,-47.4,-50.0,-50.0,-50.0,-50.0,-50.0,-50.0,-50.0,
     -             -2.4,-5.0,-7.8,-11.0,-14.3,-18.0,-21.9,-26.2,
     -       -30.9,-36.0,-41.5,-47.5,-50.0,-50.0,-50.0,-50.0,-50.0,
     -             7.3,5.0,2.6,-0.2,-3.1,-6.2,-9.7,-13.6,
     -       -17.8,-22.6,-27.9,-33.8,-40.4,-50.0,-50.0,-50.0,-50.0,
     -             16.9,15.0,12.9,10.7,8.3,5.6,2.8,-0.3,
     -       -3.8,-7.8,-12.1,-17.4,-23.3,-30.3,-38.5,-48.0,-50.0,
     -             26.6,25.0,23.3,21.4,19.4,17.2,14.9,12.4,
     -       9.7,6.8,3.3,-0.6,-5.0,-10.2,-16.7,-24.8,-35.0,
     -             36.5,35.0,33.5,31.9,30.1,28.4,26.5,24.3,
     -       22.1,19.8,17.1,14.0,11.0,7.2,3.0,-2.2,-8.8/
      DATA PLAST /792.0,678.0,575.0,480.0,387.0,290.0,250.0,500.0/
      DATA NOPTS /6,8,11,13,14,17,17,12/      
C
      DO I=1,8                         !loop round 8 sat adiabats 
        CALL TFPTC(1050.0,SATDAT(1,I),X,Y) !find x and y pos first pt
        CALL TFCON(X,Y,IXS,IYS)        !convert to screen coords
        P=1000.0                       !first pressure in loop
        DO J=2,NOPTS(I)                !loop til end of line
          IF(J.EQ.NOPTS(I)) P=PLAST(I) !if last use special press
          CALL TFPTC(P,SATDAT(J,I),X,Y)!find x and y 
          CALL TFCON(X,Y,IXF,IYF)      !convert to screen coords 
          CALL VEC(IXS,IYS,IXF,IYF)    !draw segment
          IXS=IXF
          IYS=IYF
          P=P-50.0                     !next pressure level
        END DO
      END DO
      RETURN
      END
C*******************************************************************************
      SUBROUTINE TFHMR
C
C  Constructs humidity mixing ratio lines 1,5,20g/kg from end points
C
      IMPLICIT  NONE
      INTEGER*4 IXS,IYS,IXF,IYF
      REAL*4    X,Y
      INCLUDE   'DCOM_DEF.FOR'

      CALL SLT(1)                      !Dotted line
C
      CALL TFPTC(1050.0,-16.6,X,Y)     !1 g/kg hmr line
      CALL TFCON(X,Y,IXS,IYS)          !Convert to screen coords
      CALL TFPTC(250.0,-32.4,X,Y)      !top point
      CALL TFCON(X,Y,IXF,IYF)          !convert to screen coords
      CALL VEC(IXS,IYS,IXF,IYF)        !draw hmr
C
      CALL TFPTC(1050.0,4.5,X,Y)       !5 g/kg hmr line
      CALL TFCON(X,Y,IXS,IYS)          !convert to screen coords
      CALL TFPTC(250.0,-14.5,X,Y)      !top point
      CALL TFCON(X,Y,IXF,IYF)          !convert to screen coords
      CALL VEC(IXS,IYS,IXF,IYF)        !draw hmr
C
      CALL TFPTC(1050.0,25.5,X,Y)      !20 g/kg hmr line
      CALL TFCON(X,Y,IXS,IYS)          !convert to screen coords
      CALL TFPTC(490.0,13.2,X,Y)       !top point right side 
      CALL TFCON(X,Y,IXF,IYF)          !convert to screen coords 
      CALL VEC(IXS,IYS,IXF,IYF)        !draw hmr
      CALL SLT(0)
      RETURN
      END
C*******************************************************************************
      SUBROUTINE TFPTC(P,T,X,Y)
C
C  Calculates x and y relative to bottom left of tephi(1050,-40)from a
C  temperature and pressure.
C
      IMPLICIT  NONE
      INTEGER*4 IWIN(4,5)
      REAL*4    P,T,X,Y,SCFACT,XSFACT,YSFACT,TEMP,P1000,S
      COMMON /SCALE/ SCFACT,XSFACT,YSFACT,IWIN
      TEMP=T+273.16
      P1000=1000/P
      S=((0.286*LOG(P1000))+LOG(TEMP))*277.95
      X=SCFACT*(S+TEMP-XSFACT)
      Y=SCFACT*(S-TEMP-YSFACT)
      RETURN
      END
C*******************************************************************************
      SUBROUTINE TFPXC(P,XCOORD,TRYTMP,Y,T) 
C
C  Calculates y and temperature from a known x, pressure and first guess
C  temperature.
C
      IMPLICIT  NONE
      INTEGER*4 IWIN(4,5)
      REAL*4    P,XCOORD,TRYTMP,Y,T,SCFACT,XSFACT,YSFACT,TRYTEMP,S,X,
     &    P1000,TEMP
      COMMON    /SCALE/ SCFACT,XSFACT,YSFACT,IWIN
      TRYTEMP=TRYTMP+273.16
      P1000=1000/P
      DO TEMP=TRYTEMP,TRYTEMP+100,0.1
        S=((0.286*LOG(P1000))+LOG(TEMP))*277.95
        X=SCFACT*(S+TEMP-XSFACT)  
        IF((X-XCOORD).GE.0.0) THEN
          Y=SCFACT*(S-TEMP-YSFACT)
          T=TEMP-273.16
          RETURN  
        END IF 
      END DO
      RETURN
      END
C*******************************************************************************
      SUBROUTINE TFTXC(T,XCOORD,TRYPRESS,Y,PRESS)
C
C Calculates y and press from a known x, temperature and first guess 
C pressure 
C
      IMPLICIT  NONE
      INTEGER*4 IWIN(4,5)
      REAL*4    SCFACT,XSFACT,YSFACT,T,XCOORD,TRYPRESS,Y,PRESS,TEMP,
     &    P1000,S,X
      COMMON    /SCALE/ SCFACT,XSFACT,YSFACT,IWIN

      TEMP=T+273.16
      DO PRESS=TRYPRESS,TRYPRESS+200,1.0
        P1000=1000/PRESS
        S=((0.286*LOG(P1000))+LOG(TEMP))*277.95
        X=SCFACT*(S+TEMP-XSFACT)  
        IF((X-XCOORD).LE.0.0) THEN
          Y=SCFACT*(S-TEMP-YSFACT)
          RETURN  
        END IF 
      END DO
      RETURN
      END
C*******************************************************************************
      SUBROUTINE TFXTC(X,T,Y)
C
C  Calculates y from a known x and temperature
C
      IMPLICIT  NONE
      INTEGER*4 IWIN(4,5)
      REAL*4    SCFACT,XSFACT,YSFACT,T,Y,X

      COMMON /SCALE/ SCFACT,XSFACT,YSFACT,IWIN
      T=T+273.16
      Y=X-SCFACT*(2*T-XSFACT+YSFACT)
      RETURN
      END
C*******************************************************************************
      SUBROUTINE TFCON(X,Y,IX,IY)
C
C Converts real x and y coords relative to tephi origin bott left to
C integer ix and iy coords relative to screen origin top left
C
      IMPLICIT  NONE
      INTEGER*4 IX,IY
      REAL*4    X,Y

      IX=NINT(X)+60
      IY=3650-NINT(Y) 
      RETURN 
      END
C*******************************************************************************
      SUBROUTINE HOR_TEPHI_MENU
C
C Subroutines called:
C TEPHI1       This file
c TPHIEXT      This file
C TFHMR        This file
C TFDALR       This file 
C TFSALR       This file
C TPHIBAR      This file
C TPHITEMP     This file
C TFCON1       This file
c TFPTC        HOR_TEPHI
C TFPXC        HOR_TEPHI
C TFTXC        HOR_TEPHI
C TFXTC        HOR_TEPHI
C
      IMPLICIT  NONE
      INTEGER*2 IBLK1(500),IBLK2(500)
      INCLUDE   'DCOM_DEF.FOR'
C 
C Write fixed parts of the display to the access page the first time the
C routine is called.
C
      CALL BBK(IBLK1,IBLK2,500)        !start double blocking
      CALL TEPHI1                      !draw tephi background 
      CALL SCA(13,13,0,3)
C
      CALL SEL(0,IFOREG,-1)            !cyan box 
      CALL BOXA(60,148,571,507)        !whole tephi 1050-250mb
      CALL BOXA(59,149,572,508)
      CALL SEL(0,IFOREG,IBLACK)
      CALL ALP(300,300,'1')            !input selection
C
      CALL SEL(0,IGREEN,-1)            !green box
      CALL BOXA(133,352,384,499)       !1050-600mb,-30-+10 
      CALL BOXA(132,351,385,500)
      CALL SEL(0,IGREEN,IBLACK)
      CALL ALP(180,400,'2')            !input selection
C
      CALL SEL(0,IRED,-1)              !red box 
      CALL BOXA(200,355,445,502)       !1050-600mb,-20-+20
      CALL BOXA(199,354,446,503)
      CALL SEL(0,IRED,IBLACK)
      CALL ALP(320,400,'3')            !input selection
C
      CALL SEL(0,IFOREG,-1)            !Cyan box
      CALL BOXA(281,358,523,504)       !1050-600mb,-5-+35 
      CALL BOXA(280,357,524,505)
      CALL SEL(0,IFOREG,IBLACK)
      CALL ALP(460,400,'4')            !input selection
C
      CALL SEL(0,IMAG,-1)              !magenta box
      CALL BOXA(145,151,521,350)       !600-250mb,-47-0
      CALL BOXA(144,152,522,349)
      CALL SEL(0,IMAG,IBLACK)   
      CALL ALP(300,220,'5')            !input selection
      CALL EBK                         !empty last block 
      RETURN
      END
C*******************************************************************************
      SUBROUTINE TEPHI1
      IMPLICIT  NONE
      REAL*4    SCFACT,XSFACT,YSFACT
      COMMON    /SCALE/ SCFACT,XSFACT,YSFACT
      INCLUDE   'DCOM_DEF.FOR'
C
      SCFACT=2.978                     !1050-->250mb
      XSFACT=1744.57
      YSFACT=1278.27
C     
      CALL TPHIEXT_MENU                !calculate exterior
      CALL SEL(0,IBACKG,ILGREY)        !LIGHT grey boundary
      CALL FLO(300,300)                !Fill shape with d green
      CALL SEL(0,ILGREY,IBACKG)        !L grey/d green 
      CALL TPHIBAR_MENU                !construct isobars      
      CALL TPHITEMP_MENU               !construct isotherms
      RETURN
      END
C*******************************************************************************
      SUBROUTINE TPHIEXT_MENU
C
C  calculates exterior coords and constructs sides
C
      IMPLICIT  NONE
      INTEGER*4 IX1,IY1,IX2,IY2,IX3,IY3,IXS,IYS,IXF,IYF,IX0,IY0
      REAL*4    X,Y,TRYTEMP,T      
      INCLUDE   'DCOM_DEF.FOR'
C
      CALL SEL(0,ILGREY,IBLACK)        !L Grey
      CALL TFPTC(1050.0,-40.0,X,Y)     !tephi origin bottom left
      CALL TFCON1(X,Y,IX0,IY0)         !convert to screen coords
      TRYTEMP=-89.0                    !first guess at temp 
      CALL TFPXC(250.0,X,TRYTEMP,Y,T)  !top left corner             
      CALL TFCON1(X,Y,IX1,IY1)         !convert to screen coords
      CALL VEC(IX0,IY0,IX1,IY1)        !draw line bott to top left
C
      CALL TFPTC(1050.0,45.0,X,Y)      !tephi origin bottom right
      CALL TFCON1(X,Y,IX3,IY3)         !convert to screen coords
      TRYTEMP=-14.0                    !first guess at temp 
      CALL TFPXC(250.0,X,TRYTEMP,Y,T)  !top right corner             
      CALL TFCON1(X,Y,IX2,IY2)         !convert to screen coords
      CALL VEC(IX2,IY2,IX3,IY3)        !draw line bott to top right
C
      IXS=IX0                          !1050mb isobar
      IYS=IY0                          !initial point left side
      DO T=-35.0,45.0,5.0              !plot isobar at 5 degree Xsect
        CALL TFPTC(1050.0,T,X,Y)
        CALL TFCON1(X,Y,IXF,IYF)               
        CALL VEC(IXS,IYS,IXF,IYF)      !draw section
        IXS=IXF                               
        IYS=IYF
      END DO
C
      IXS=IX1                          !250mb isobar
      IYS=IY1                          !initial point left side
      DO T=-85.0,-15.0,5.0             !plot isobar at 5 degree Xsect
        CALL TFPTC(250.0,T,X,Y)        !to nearly end 
        CALL TFCON1(X,Y,IXF,IYF)
        CALL VEC(IXS,IYS,IXF,IYF)      !draw section
        IXS=IXF
        IYS=IYF
      END DO
      IXF=IX2                          !top right
      IYF=IY2
      CALL VEC(IXS,IYS,IXF,IYF)        !plot last section
      RETURN
      END
C*******************************************************************************
      SUBROUTINE TPHIBAR_MENU
C
C Constructs isobars plotting every 5 degree isotherm intersections
C
      IMPLICIT  NONE
      CHARACTER CBAR*4
      INTEGER*4 IXS,IYS,IXLAB,IYLAB,IXF,IYF,IP,IOS
      REAL*4    TRYTEMP,TRYTEMP2,P,Y,T,TS,TF,X,RINC,TEMP
      INCLUDE   'DCOM_DEF.FOR'

      CALL SCA(13,13,0,2)              !small letters
      TRYTEMP=-85.0                    !first lhstemp for firstlevel
      TRYTEMP2=-10.0                   !first rhs temp for first
      RINC=9.0
      DO P=300.0,1000.0,100.0          !loop through press levels
         CALL TFPXC(P,0.0,TRYTEMP,Y,T) !Find y left side 
         CALL TFCON1(0.0,Y,IXS,IYS)    !convert to screen coords
         IXLAB=IXS                     !save x,y pos for mb labels
         IYLAB=IYS
         TS=T                          !use temp for loop
         TF=TS+75.0
         DO TEMP=TS,TF,5.0             !plot isobar at 5 degree Xsect
           CALL TFPTC(P,TEMP,X,Y)      !to nearly end 
           CALL TFCON1(X,Y,IXF,IYF)    !convert to screen coords
           CALL SEL(0,ILGREY,IBACKG)   !L grey/d green
           CALL VEC(IXS,IYS,IXF,IYF)   !draw section
           IXS=IXF
           IYS=IYF
         END DO
         CALL TFPXC(P,511.0,TRYTEMP2,Y,T) !find y right side
         CALL TFCON1(511.0,Y,IXF,IYF)  !convert to screen coords
         CALL VEC(IXS,IYS,IXF,IYF)     !plot last section
         CALL SEL(0,IFOREG,IBLACK)     !cyan/black label 
         IP=NINT(P)
         WRITE(CBAR,'(I4)',IOSTAT=IOS) IP !label isobar
         CALL ALP(IXLAB+1,IYLAB-8,CBAR)
         TRYTEMP2=TRYTEMP2+RINC        !incr guesstemps for new level
         TRYTEMP=TRYTEMP+RINC
         RINC=RINC-1.0
      END DO
      RETURN
      END 
C*******************************************************************************
      SUBROUTINE TPHITEMP_MENU
C
C  Constructs isotherms
C
      IMPLICIT  NONE
      INTEGER*4 IXS,IYS,IXF,IYF,IT,IOS
      REAL*4    T,XS,YS,XF,YF,P,TRYP,X,Y
      CHARACTER CTEMP*3
      INCLUDE   'DCOM_DEF.FOR'

      DO T=-40.0,-20.0,10.0            !Top and bottom points known
        CALL TFPTC(1050.0,T,XS,YS)     !bottom of isotherm
        CALL TFCON1(XS,YS,IXS,IYS)     !convert to screen coords
        CALL TFPTC(250.0,T,XF,YF)      !top of isotherm
        CALL TFCON1(XF,YF,IXF,IYF)     !convert to screen coords
        CALL SEL(0,ILGREY,IBACKG)      !L grey/dgreen
        CALL VEC(IXS,IYS,IXF,IYF)      !draw isotherm
        CALL SEL(0,IFOREG,IBLACK)      !cyan/black label
        CALL SCA(8,10,0,2)
        IT=NINT(T)
        WRITE(CTEMP,'(I3)',IOSTAT=IOS) IT !label isotherm
        CALL ALP(IXF-30,IYF+9,CTEMP)
      END DO
      TRYP=300.0                       !first guess at pressure
      DO T=-80.0,-50.0,10.0            !isotherms Xsect left axis
        CALL TFTXC(T,0.0,TRYP,Y,P)     !left axis
        CALL TFCON1(0.0,Y,IXS,IYS)     !convert to screen coords
        CALL TFPTC(250.0,T,X,Y)        !top of isotherm 
        CALL TFCON1(X,Y,IXF,IYF)       !convert to screen coords
        CALL SEL(0,ILGREY,IBACKG)      !L grey/d green
        CALL VEC(IXS,IYS,IXF,IYF)      !draw isotherm 
        CALL SEL(0,IFOREG,IBLACK)      !cyan/black label 
        IT=NINT(T)
        WRITE(CTEMP,'(I3)',IOSTAT=IOS) IT !label isotherm
        CALL ALP(IXF-30,IYF+9,CTEMP)
        TRYP=TRYP+100.0                !increment first guess 
      END DO
C
      TRYP=250.0                       !first guess at pressure
      DO T=-10.0,40.0,10.0             !isotherms Xsect right axis
         CALL TFTXC(T,511.0,TRYP,Y,P)  !right axis
         CALL TFCON1(511.0,Y,IXS,IYS)  !convert to screen coords
         CALL TFPTC(1050.0,T,X,Y)      !bottom of isotherm 
         CALL TFCON1(X,Y,IXF,IYF)      !convert to screen coords
         IF(T .EQ. 0.0) THEN
           CALL SLT(7)
         ELSE
           CALL SLT(0)
         END IF
         CALL SEL(0,ILGREY,IBACKG)     !L grey/d green  
         CALL VEC(IXS,IYS,IXF,IYF)     !draw isotherm 
         CALL SEL(0,IFOREG,IBLACK)     !cyan/black labels
         IT=NINT(T)
         WRITE(CTEMP,'(I3)',IOSTAT=IOS) IT !label isotherm
         CALL ALP(IXS-35,IYS+5,CTEMP)
         TRYP=TRYP+100.0               !increment first guess 
      END DO
      RETURN
      END           
C*******************************************************************************
      SUBROUTINE TFDALR_MENU
C
C Constructs dry adiabats
C
      IMPLICIT  NONE
      INTEGER*4 IXS,IYS,IXF,IYF
      REAL*4    T,X,Y,S,YC,TOPT,TDALR
      INCLUDE   'DCOM_DEF.FOR'

      DO TDALR=0.0,40.0,10.0           !draw 0->40 adiabats
        CALL TFPTC(1000.0,TDALR,X,Y)   !find x and y at 1000mb  
        YC=X+Y 
        DO T=TDALR,TDALR+20.0,0.5      !loop to find 1050mb position
           CALL TFPTC(1050.0,T,X,Y)    !find x and y at bottom
           S=X+Y-YC
           IF(S.GE.0.0) GOTO 10
        END DO
10      CALL TFCON1(X,Y,IXS,IYS)       !convert bottom to screen 
        T=-90.0+TDALR/2.0              !first guess at temp top
        DO TOPT=T,T+20.0,0.5           !loop to find 250mb position
           CALL TFPTC(250.0,TOPT,X,Y)  !find x and y at top
           S=X+Y
           IF((S-YC).GE.0.0) GOTO 20
        END DO
20      CALL TFCON1(X,Y,IXF,IYF)       !convert to screen coords
        CALL VEC(IXS,IYS,IXF,IYF)      !draw adiabat
      END DO
C
      DO TDALR=-40.0,-10.0,10.0        !draw -40->-10 adiabats
        CALL TFPTC(1000.0,TDALR,X,Y)   !find x and y at 1000mb  
        YC=X+Y 
        DO T=TDALR,TDALR+20.0,0.5      !loop to find 1050mb position
           CALL TFPTC(1050.0,T,X,Y)    !find x and y at bottom
           S=X+Y-YC
           IF(S.GE.0.0) GOTO 30
        END DO
30        CALL TFCON1(X,Y,IXS,IYS)     !convert bott to screen coords
        CALL TFCON1(0.0,YC,IXF,IYF)    !convert left to screen coords
        CALL VEC(IXS,IYS,IXF,IYF)      !draw adiabat
      END DO
C
      DO TDALR=50.0,110.0,10.0         !draw 50->110 adiabats
        CALL TFPTC(1000.0,TDALR,X,Y)   !find x and y at 1000mb  
        YC=X+Y 
        CALL TFCON1(511.0,YC-511.0,IXS,IYS) !convert bottom to screen 
        T=-95.0+TDALR/2.0              !first guess at temp top
        DO TOPT=T,T+25.0,0.5           !loop to find 250mb position
           CALL TFPTC(250.0,TOPT,X,Y)  !find x and y at top
           S=X+Y
           IF((S-YC).GE.0.0) GOTO 40
        END DO
40      CALL TFCON1(X,Y,IXF,IYF)       !convert to screen coords
        CALL VEC(IXS,IYS,IXF,IYF)      !draw adiabat
      END DO
      RETURN
      END
C*******************************************************************************
      SUBROUTINE TFSALR_MENU
C
C  Contructs saturated adiabatic curves every 10 degrees from a 
C  series of points calculated from temperature and pressure at 50 mb ints.
C
      IMPLICIT  NONE
      INTEGER*4 NOPTS(8),I,IXS,IYS,IXF,IYF,J
      REAL*4    SATDAT(17,8),PLAST(8),X,Y,P
      INCLUDE   'DCOM_DEF.FOR'
      DATA SATDAT /-31.9,-35.0,-38.4,-41.9,-45.5,-50.0,-50.0,-50.0,
     -       -50.0,-50.0,-50.0,-50.0,-50.0,-50.0,-50.0,-50.0,-50.0,
     -             -21.8,-25.0,-28.3,-31.8,-35.5,-39.5,-43.7,-50.0,
     -       -50.0,-50.0,-50.0,-50.0,-50.0,-50.0,-50.0,-50.0,-50.0,
     -             -12.0,-15.0,-18.3,-21.7,-25.3,-29.1,-33.2,-37.7,
     -       -42.4,-47.4,-50.0,-50.0,-50.0,-50.0,-50.0,-50.0,-50.0,
     -             -2.4,-5.0,-7.8,-11.0,-14.3,-18.0,-21.9,-26.2,
     -       -30.9,-36.0,-41.5,-47.5,-50.0,-50.0,-50.0,-50.0,-50.0,
     -             7.3,5.0,2.6,-0.2,-3.1,-6.2,-9.7,-13.6,
     -       -17.8,-22.6,-27.9,-33.8,-40.4,-50.0,-50.0,-50.0,-50.0,
     -             16.9,15.0,12.9,10.7,8.3,5.6,2.8,-0.3,
     -       -3.8,-7.8,-12.1,-17.4,-23.3,-30.3,-38.5,-48.0,-50.0,
     -             26.6,25.0,23.3,21.4,19.4,17.2,14.9,12.4,
     -       9.7,6.8,3.3,-0.6,-5.0,-10.2,-16.7,-24.8,-35.0,
     -             36.5,35.0,33.5,31.9,30.1,28.4,26.5,24.3,
     -       22.1,19.8,17.1,14.0,11.0,7.2,3.0,-2.2,-8.8/
      DATA PLAST /792.0,678.0,575.0,480.0,387.0,290.0,250.0,500.0/
      DATA NOPTS /6,8,11,13,14,17,17,12/      
C
      DO I=1,8                         !loop round 8 sat adiabats 
        CALL TFPTC(1050.0,SATDAT(1,I),X,Y) !find x and y pos first pt
        CALL TFCON1(X,Y,IXS,IYS)       !convert to screen coords
        P=1000.0                       !first pressure in loop
        DO J=2,NOPTS(I)                !loop til end of line
          IF(J.EQ.NOPTS(I)) P=PLAST(I) !if last use special press
          CALL TFPTC(P,SATDAT(J,I),X,Y) !find x and y 
          CALL TFCON1(X,Y,IXF,IYF)     !convert to screen coords 
          CALL VEC(IXS,IYS,IXF,IYF)    !draw segment
          IXS=IXF
          IYS=IYF
          P=P-50.0                     !next pressure level
        END DO
      END DO
      RETURN
      END
C*******************************************************************************
      SUBROUTINE TFHMR_MENU
C
C  Constructs humidity mixing ratio lines 1,5,20g/kg from end points
C
      IMPLICIT  NONE
      INTEGER*4 IXS,IYS,IXF,IYF
      REAL*4    X,Y
      INCLUDE   'DCOM_DEF.FOR'

      CALL SLT(1)
      CALL TFPTC(1050.0,-16.6,X,Y)     !1 g/kg hmr line
      CALL TFCON1(X,Y,IXS,IYS)         !Convert to screen coords
      CALL TFPTC(250.0,-32.4,X,Y)      !top point
      CALL TFCON1(X,Y,IXF,IYF)         !convert to screen coords
      CALL VEC(IXS,IYS,IXF,IYF)        !draw hmr
C
      CALL TFPTC(1050.0,4.5,X,Y)       !5 g/kg hmr line
      CALL TFCON1(X,Y,IXS,IYS)         !convert to screen coords
      CALL TFPTC(250.0,-14.5,X,Y)      !top point
      CALL TFCON1(X,Y,IXF,IYF)         !convert to screen coords
      CALL VEC(IXS,IYS,IXF,IYF)        !draw hmr
C
      CALL TFPTC(1050.0,25.5,X,Y)      !20 g/kg hmr line
      CALL TFCON1(X,Y,IXS,IYS)         !convert to screen coords
      CALL TFPTC(490.0,13.2,X,Y)       !top point right side 
      CALL TFCON1(X,Y,IXF,IYF)         !convert to screen coords 
      CALL VEC(IXS,IYS,IXF,IYF)        !draw hmr
      CALL SLT(0)
      RETURN
      END
C*******************************************************************************
      SUBROUTINE TFCON1(X,Y,IX,IY)
C
C Converts real x and y coords relative to tephi origin bott left to
C integer ix and iy coords relative to screen origin top left,with
C offsets (60,500)
C
      IMPLICIT  NONE
      INTEGER*4 IX,IY
      REAL*4    X,Y

      IX=NINT(X)+60
      IY=500-NINT(Y)
      RETURN 
      END
