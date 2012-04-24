C Changes:
C
C 1      25/02/94  Mag compass heading removed (WDNJ)
C 2      02/09/94  SO2, NO, NOx options (paras 592, 605, 606) removed (WDNJ)
C 3      13/09/94  Redundant variables removed (WDNJ)
C 4      03/08/98  Remaining write statements changed so as not to produce 
C                  output conversion error messages (WDNJ)
C 5      08/09/99  Now records plotting status (WDNJ)
C 6      30/09/99  Restructured for use on Alpha (WDNJ)
C-------------------------------------------------------------------------------
C ROUTINE CHEM_MENU
C
C Displays CHEMISTRY menu on Horace display and calls relevant routines
C
C Subroutines used:
C
C   CHEM_PROF         This file
C   CHEM_TLAG         This file
C   CHEM_ECGC         This file
C   CHEM_DUMP         This file
C   SELECT
C-------------------------------------------------------------------------------
      SUBROUTINE CHEM_MENU
      IMPLICIT  NONE
      INTEGER*4 IFUNC,IKEY
      INCLUDE   'DCOM_DEF.FOR'
C
9     CALL SEL(0,0,IBLACK)             !Use black
      CALL RUB                         !Clear display page
      CALL SEL(0,IBACKG,0)             !Dk green
      CALL BLK(0,0,767,509)            !Fill block with background
      CALL SEL(0,IFOREG,IBACKG)        !Cyan on dk green
      CALL SCA(13,13,0,3)              !Char cell width, height, orient, font
      CALL ALP(190,110,' MENU B - CHEMISTRY OPTIONS')
      CALL ALP(10,150,' 1) PLOT OF THREE GCs')
      CALL ALP(10,180,' 2) PROFILE PLOT')
      CALL ALP(10,210,' 3) PLOT WITH TIME LAG')
      CALL ALP(10,240,' 4) DUMP OF CHEMISTRY PARAS')
      CALL FOOT('SELECT',' ',' ',' ',' ','PRINT',' ',' ')
10    CALL ALP(30,465,'Enter selection (default=1) ...              ')
      CALL SELECT(446,465,IFUNC)       !Get user requirement
      IF(IFUNC.EQ.0) IFUNC=1
      IF(IFUNC.EQ.-1) RETURN
      IF(IFUNC.LT.-1.OR.IFUNC.GT.4) GOTO 10 !Invalid input
      IF(IFUNC.EQ.1) CALL CHEM_ECGC(IKEY) 
      IF(IFUNC.EQ.2) CALL CHEM_PROF(IKEY)
      IF(IFUNC.EQ.3) CALL CHEM_TLAG(IKEY) 
      IF(IFUNC.EQ.4) CALL CHEM_DUMP(IKEY) 
      IF(CHAR(IKEY).NE.'A') GOTO 9
      RETURN
      END
C*******************************************************************************
      SUBROUTINE CHEM_TLAG(IKEY)
C
C Get the arguments, then call chem tlag.
C
C Subroutines used:
C
C   CHEM_TLAG_PARA
C   CHEM_GET_SCAL
C   CHEM_TLAG1
C
      IMPLICIT  NONE
      INTEGER*4 ILIST(12),IKEY,ITIMP,IX,ILGX,IY,ILGY,I4A,I4B,I4C,I4D

10    CALL CHEM_TLAG_PARA(ITIMP,IX,ILGX,IY,ILGY,IKEY)
      IF(CHAR(IKEY).EQ.'A') RETURN
      IF(CHAR(IKEY).EQ.'B') RETURN
20    CALL CHEM_GET_SCAL('X ',3,IX,I4A,I4B,IKEY)
      IF(CHAR(IKEY).EQ.'A') RETURN
      IF(CHAR(IKEY).EQ.'B') RETURN
      IF(CHAR(IKEY).EQ.'C') GOTO 10
      CALL CHEM_GET_SCAL('Y ',3,IY,I4C,I4D,IKEY)
      IF(CHAR(IKEY).EQ.'A') RETURN
      IF(CHAR(IKEY).EQ.'B') RETURN
      IF(CHAR(IKEY).EQ.'C') GOTO 10
      ILIST(1)=12
      ILIST(2)=19
      ILIST(3)=3
      ILIST(4)=ITIMP
      ILIST(5)=IX
      ILIST(6)=ILGX
      ILIST(7)=IY
      ILIST(8)=ILGY
      ILIST(9)=I4A
      ILIST(10)=I4B
      ILIST(11)=I4C
      ILIST(12)=I4D
      CALL CHEM_TLAG1(ILIST,IKEY)
      IF(CHAR(IKEY).EQ.'A') RETURN
      IF(CHAR(IKEY).EQ.'B') RETURN
      IF(CHAR(IKEY).EQ.'C') GOTO 10
      IF(CHAR(IKEY).EQ.'D') GOTO 20
      END
C-------------------------------------------------------------------------------
C ROUTINE CHEM_TLAG1
C
C Create an X Y plot with lags on one or both paras.
C
C Subroutines used:
C
C   UPDT_STATUS
C   CHEM_TLAG_BACKG
C   HEAD_UPDATE
C   CHECK_KEY
C
C AUGUST 1992 Dave T
C-------------------------------------------------------------------------------
      SUBROUTINE CHEM_TLAG1(ILIST,IKEY)
      IMPLICIT  NONE
      INTEGER*4 I4A,I4B,I4C,I4D,ILIST(12),ILAT,IPAS,IPX,IPY,IY,IX,IST,
     +    IPARX,IPARY,ILGX,ILGY,ITIMP,IKEY,INUM
      CHARACTER CIX*8
      INCLUDE   'DCOM_DEF.FOR'
      INCLUDE   'HCOM_DEF.FOR'
      REAL*4    RTICVAL1,RTICVAL2,RTICINC1,RTICINC2,RYFACT2,RYFACT1,
     +    RXFACT1,RXFACT2
      INTEGER*4 LYOFF1,LYOFF2,LXOFF1,LXOFF2,LYPIX1,LYPIX2,LXPIX1,LXPIX2
      COMMON    /USERPLOT/ RTICVAL1,RTICVAL2,RTICINC1,RTICINC2,RYFACT1,
     +    RYFACT2,RXFACT1,RXFACT2,LYOFF1,LYOFF2,LXOFF1,LXOFF2,LYPIX1,
     +    LYPIX2,LXPIX1,LXPIX2

      CALL UPDT_STATUS(ILIST)
      ITIMP=ILIST(4)
      IPARX=ILIST(5)-512
      ILGX=ILIST(6)
      IPARY=ILIST(7)-512
      ILGY=ILIST(8)
      I4A=ILIST(9)
      I4B=ILIST(10)
      I4C=ILIST(11)
      I4D=ILIST(12)
      ILGX=ILGX/3
      ILGY=ILGY/3
      INUM=0
      RTICVAL1=TICVAL(IPARY)+((I4C-1)*TICINC(IPARY)) !new origin
      RTICINC1=TICINC(IPARY)/I4D       !new label increment 
      LYPIX1=IYPIX(IPARY)              ! no of pixels tween tics
      LYPIX2=LYPIX1
      LYOFF1=480+(RTICVAL1/RTICINC1*LYPIX1) !new zero value pixel offset
      RYFACT1=LYPIX1/RTICINC1          !new y scaling factor
C
      CALL CHEM_TLAG_BACKG(IPARX,IPARY,ILGX,ILGY,I4A,I4B,IKEY)
      IF(IKEY.NE.0) RETURN
C
C Wait until there is some information to plot
C
      DO WHILE(INUM.LT.1)
        CALL LIB$WAIT(0.2)
        CALL CHECK_KEY('ABCD',IKEY)
        IF(IKEY.NE.0) RETURN
        ILAT=STATUS(2)
        INUM=STATUS(3)
        IF(ILGX.GT.ILGY) THEN 
          INUM=INUM-ILGX 
        ELSE
          INUM=INUM-ILGY
        ENDIF 
        IF(INUM.GT.ITIMP) INUM=ITIMP
      END DO
C
C Plot data up to the most recent
C
20    CALL SCA(13,13,0,3)             
      DO IPAS=(1-INUM),0
        CALL CHECK_KEY('ABCD',IKEY)
        IF(IKEY.NE.0) RETURN
        IPX=ILAT+IPAS-ILGX
        IPX=1+(MOD((IPX-1),12800))
        IF(IPX.LT.1) IPX=IPX+12800
        IPY=ILAT+IPAS-ILGY
        IPY=1+(MOD((IPY-1),12800))
        IF(IPY.LT.1) IPY=IPY+12800
        IY=IFIX(LYOFF1-RYFACT1*DERIVE(IPY,IPARY))
        IF(IY.GT.479) IY=479       !Test for outside graf area
        IF(IY.LT.151) IY=151
        IX=IFIX(LXOFF1+RXFACT1*DERIVE(IPx,IPARX))
        IF(IX.GT.669) IX=669       !Test for outside graf area
        IF(IX.LT.101) IX=101
        CALL SEL(0,IYELLOW,IBLACK) !Colour for x parameter
        WRITE(CIX,'(F8.2)',IOSTAT=IST) DERIVE(IPX,IPARX)
        IF(IPAS.EQ.0) CALL ALP(340,105,CIX) !print x para
        CALL SEL(0,IFOREG,IBLACK)  !Colour for y parameter
        WRITE(CIX,'(F8.2)',IOSTAT=IST) DERIVE(IPY,IPARY)
        IF(IPAS.EQ.0) CALL ALP(86,105,CIX)  !print y para
        CALL SEL(0,IWHITE,IBLACK)  !Colour for x y parameter
        CALL BIT(IX,IY)            !draw dot only on x y plot    
      END DO
      INUM=0
C
C Wait until the is some more data
C
      DO WHILE(INUM.EQ.0)
        CALL LIB$WAIT(0.2)
        CALL CHECK_KEY('ABCD',IKEY)
        IF(IKEY.NE.0) RETURN
        INUM=STATUS(2)-ILAT
        INUM=(MOD((INUM),12800))
        IF(INUM.LT.0) INUM=INUM+12800
        ILAT=STATUS(2)
      END DO
      CALL HEAD_UPDATE
      GOTO 20
      END
C-------------------------------------------------------------------------------
C ROUTINE CHEM_TLAG_PARA
C
C Set up paras for profiles
C
C Subroutines used:
C
C FOOT
C SELECT
C CHEM_GET_PARA
C
C AUGUST 1992 Dave T
C-------------------------------------------------------------------------------
      SUBROUTINE CHEM_TLAG_PARA(ITIMP,IX,ILGX,IY,ILGY,IKEY) 
      IMPLICIT  NONE
      INTEGER*4 IKEY,IX,IY,ITIMP,ILGX,ILGY
      INCLUDE   'DCOM_DEF.FOR'

      IKEY=0
      CALL SEL(0,0,IBLACK)
      CALL RUB                         !clear display page
      CALL SEL(0,IBACKG,0)             !D green
      CALL BLK(0,0,767,509)            !Fill block with background
      CALL FOOT('SELECT','CHEM',' ',' ',' ','PRINT',' ',' ')
C
      CALL ALP(30,450,
     +    'How far back in minutes (max=128, default=20)? ')
      ITIMP=-10
      DO WHILE(ITIMP.LT.-2.OR.ITIMP.GT.128)
        CALL SELECT(641,450,ITIMP)
      END DO
      IF(ITIMP.LT.0) THEN
        IKEY=64-ITIMP
        RETURN
      END IF
      IF(ITIMP.EQ.0) ITIMP=20          !Default value
C
      CALL CHEM_GET_PARA(IX,'X ',IKEY)
      IF(IKEY.NE.0) RETURN        
      CALL ALP(10,400,
     +    'Lag on X parameter in secs (max=900, default=0)? ')
      ILGX=-10
      DO WHILE(ILGX.LT.-2.OR.ILGX.GT.900)
        CALL SELECT(647,400,ILGX)
      END DO
      IF(ILGX.LT.0) THEN
        IKEY=64-ILGX
        RETURN
      END IF
C
      CALL CHEM_GET_PARA(IY,'Y ',IKEY)
      IF(IKEY.NE.0) RETURN        
      CALL ALP(10,400,
     +    'Lag on Y parameter in secs (max=900, default=0)? ')
      ILGY=-10
      DO WHILE(ILGY.LT.-2.OR.ILGY.GT.900)
        CALL SELECT(647,400,ILGY)
      END DO
      IF(ILGY.LT.0) THEN
        IKEY=64-ILGY
        RETURN
      END IF
      RETURN
      END
C-------------------------------------------------------------------------------
C ROUTINE CHEM_GET_SCAL
C
C Scaling factors menu
C
C Subroutines used:
C
C   FOOT
C   SELECT
C
C AUGUST 1992 Dave T
C-------------------------------------------------------------------------------
      SUBROUTINE CHEM_GET_SCAL(CC,IFUNC,IPAR,I4A,I4B,IKEY)
      IMPLICIT  NONE
      INTEGER*4 IKEY,IPAR,I4A,I4B,IX,IY,IST,IOPTION,LAN,I,IFUNC,IMIN
      REAL*4    RMINA,RMAXA1,RMAXA2,RMAXA4,RMAXA8,FIRSTTIC
      CHARACTER CC*2
      CHARACTER CLINE(6)*60
      CHARACTER COPT*1
      CHARACTER CLABEL*8
      INCLUDE   'DCOM_DEF.FOR'
C
C   contents of menu D for plotting scale enlargements
C
      DATA CLINE(1) 
     -    /' ENTER NEW ORIGIN  - from one of those shown below'/
      DATA CLINE(2) /' SELECT FACTOR FOR SCALE ENLARGEMENT           '/  
      DATA CLINE(3) /'    1)               -->                       '/
      DATA CLINE(4) /'    2)               -->                       '/
      DATA CLINE(5) /'    3)               -->                       '/
      DATA CLINE(6) /'    4)               -->                       '/

      IKEY=0
      CALL SEL(0,0,IBLACK)
      CALL RUB                         !clear display page
      CALL SEL(0,IBACKG,0)             !D green
      CALL BLK(0,0,767,509)            !Fill block with background
      CALL SEL(0,IFOREG,IBACKG)        !cyan/d green
      CALL SCA(13,13,0,3)              !character cell width, height, orientation, font
      CALL ALP(170,110,'MENU D - PLOTTING SCALE ENLARGEMENT')
      IF(IFUNC.EQ.1) THEN
        CALL FOOT('SELECT','CHEM',' ',' ',' ','PRINT',' ',' ')
      ELSE
        CALL FOOT('SELECT','CHEM','PARAS',' ',' ','PRINT',' ',' ')
      END IF
      CALL ALP(250,177,CPARA(IPAR-512)) !write in parameter name
      CALL ALP(400,177,CUNIT(IPAR-512)) !write in units
      CALL VEC(100,200,670,200)        !draw x axis for example
      DO IX=100,670,IXPIX(IPAR-512)     
        CALL VEC(IX,200,IX,206)        !draw tic marks
      END DO
      CALL SCA(10,10,0,2)              !small letters
      CALL ALP(5,235,'OPTION = ') 
      FIRSTTIC=TICVAL(IPAR-512)
      IOPTION=1
      DO IX=40,620,IXPIX(IPAR-512)
        IY=210
        WRITE(CLABEL,11,IOSTAT=IST) FIRSTTIC   
        CALL ALP(IX,IY,CLABEL)         !write labels
        WRITE(COPT,12,IOSTAT=IST) IOPTION
        CALL ALP(IX+50,235,COPT)       !write option nos
        FIRSTTIC=FIRSTTIC+TICINC(IPAR-512)
        IOPTION=IOPTION+1
      END DO
      CALL SCA(13,13,0,3)              !cell width, height, orientation, font
      CALL ALP(10,250,
     -    'Select new    origin option no (default=1) ...')
      CALL ALP(150,250,CC)             !put in y,y1,y2,x
      I4A=99
      IMIN=-3
      IF(IFUNC.EQ.1) IMIN=-2
      DO WHILE((I4A.LT.IMIN).OR.(I4A.GT.7))
        CALL SELECT(615,250,I4A)       !get user input
      END DO
      IF(I4A.LT.0) THEN 
        IKEY=64-I4A
        RETURN
      ENDIF
      IF(I4A.EQ.0) I4A=1
      RMINA=TICVAL(IPAR-512)+((I4A-1)*TICINC(IPAR-512))   
     -                                 !new origin
      RMAXA1=(330.0/IYPIX(IPAR-512)*TICINC(IPAR-512))/1.0 
     -                             +RMINA   !Max scaling factor= 1
      RMAXA2=(330.0/IYPIX(IPAR-512)*TICINC(IPAR-512))/2.0 
     -                             +RMINA   !Max Scaling factor= 2
      RMAXA4=(330.0/IYPIX(IPAR-512)*TICINC(IPAR-512))/4.0 
     -                             +RMINA   !Max Scaling factor= 4
      RMAXA8=(330.0/IYPIX(IPAR-512)*TICINC(IPAR-512))/8.0 
     -                             +RMINA   !Max Scaling factor= 8
      WRITE(CLABEL,11,IOSTAT=IST) RMINA
      CALL ALP(10,300,CLINE(2)(1:LAN(CLINE(2))))                    
      CALL SCA(10,10,0,2)
      IY=330
      DO I=3,6
        CALL ALP(10,IY,CLINE(I)(1:LAN(CLINE(I)))) !show 4 lines
        CALL ALP(70,IY,CLABEL)         !write in the minimum
        IY=IY+30
      END DO
      WRITE(CLABEL,11,IOSTAT=IST) RMAXA1    
      CALL ALP(250,330,CLABEL)         !write in the maximums
      WRITE(CLABEL,11,IOSTAT=IST) RMAXA2
      CALL ALP(250,360,CLABEL)
      WRITE(CLABEL,11,IOSTAT=IST) RMAXA4
      CALL ALP(250,390,CLABEL)
      WRITE(CLABEL,11,IOSTAT=IST) RMAXA8
      CALL ALP(250,420,CLABEL)
      CALL SCA(13,13,0,3)              !character cell width, height, orientation, font
      IF(IPAR.NE.574) THEN             !ozone is a special case - takes a default of 3
        CALL ALP(10,450,'Select Scaling Factor (default=1) ...')
      ELSE
        CALL ALP(10,450,'Select Scaling Factor (default=3) ...')
      END IF
      I4B=99
      DO WHILE((I4B.LT.IMIN).OR.(I4B.GT.7))
        CALL SELECT(570,450,I4B)       !get user input
      END DO
      IF(I4B.LT.0) IKEY=64-I4B
      IF(I4B.EQ.0) THEN
        IF(IPAR.EQ.574) THEN 
          I4B=3 
        ELSE 
          I4B=1
        END IF
      END IF
      RETURN
   11 FORMAT(F8.2)
   12 FORMAT(I1)
      END
C-------------------------------------------------------------------------------
C ROUTINE CHEM_GET_PARA
C
C Parameters menu
C
C Subroutines used:
C
C   FOOT
C   SELECT
C
C AUGUST 1992 Dave T
C-------------------------------------------------------------------------------
      SUBROUTINE CHEM_GET_PARA(IPAR,CNA,IKEY)
      IMPLICIT  NONE
      INTEGER*4 IPAR,IKEY,JJ,J,IY,I,LAN
      CHARACTER CNA*2
      CHARACTER CLINE(72)*48
      INCLUDE   'DCOM_DEF.FOR'
C
C   contents of menu 2
C
      DATA CLINE(1) /'520) DEICED TAT K       521) DEICED TAT C       '/
      DATA CLINE(2) /'523) NONDEICED TAT K    524) NONDEICED TAT C    '/ 
      DATA CLINE(3) /'544) IN CLOUD TEMP      529) DEW POINT          '/
      DATA CLINE(4) /'591) DEW POINT - TWC    573) DEW POINT - WVS    '/
      DATA CLINE(5) /'527) POTENTIAL TEMP     525) EQUIV POT TEMP     '/
      DATA CLINE(6) /'526) REFRACTIVE INDEX   589) REFRACTIVITY       '/
      DATA CLINE(7) /'528) DRY AIR DENSITY    531) MOIST AIR DENSITY  '/
      DATA CLINE(8) /'530) VAPOUR PRESSURE    532) SPECIFIC HUMIDITY  '/
      DATA CLINE(9) /'533) MASS MIXING RATIO  534) HUM MIXING RATIO   '/
      DATA CLINE(10)/'547) FWVS HMR (WVD)     546) LIFTING COND. LEVEL'/
      DATA CLINE(11)/'535) J/W LIQUID WATER   572) TOTAL WATER CONTENT'/ 
      DATA CLINE(12)/'537) HEIM UNCORRECTED   590) SURFACE TEMP       '/
      DATA CLINE(13)/'570) WIND SPEED         571) WIND ANGLE         '/
      DATA CLINE(14)/'610) 10M WIND SPEED     576) STATIC PRESSURE    '/ 
      DATA CLINE(15)/'575) RADAR HEIGHT       579) PRESSURE HT Kft    '/ 
      DATA CLINE(16)/'574) OZONE MIXING RATIO 545) CNC CONCENTRATION  '/ 
      DATA CLINE(17)/'538) UPPER CLEAR FLUX   539) UPPER RED FLUX     '/
      DATA CLINE(18)/'540) UPPER IR FLUX      541) LOWER CLEAR FLUX   '/
      DATA CLINE(19)/'542) LOWER RED FLUX     543) LOWER IR FLUX      '/
      DATA CLINE(20)/'580) SOLAR ALBEDO       581) NEAR IR ALBEDO     '/
      DATA CLINE(21)/'582) VISIBLE ALBEDO     583) LOWER VISIBLE      '/
      DATA CLINE(22)/'584) UPPER VISIBLE      585) NET IR             '/
      DATA CLINE(23)/'586) UPPER NIR/SOLAR    587) LOWER NIR/SOLAR    '/ 
      DATA CLINE(24)/'552) SOLAR ZENITH ANGLE 553) SOLAR AZIMUTH ANGLE'/
      DATA CLINE(25)/'548) ANGLE OF ATTACK    549) ANGLE OF SIDESLIP  '/
      DATA CLINE(26)/'517) TRUE AIR SPEED     557) INS VZ             '/
      DATA CLINE(27)/'558) INS VN             559) INS VE             '/
      DATA CLINE(28)/'560) INS ROLL           561) INS PITCH          '/
      DATA CLINE(29)/'562) INS HEADING        563) INS GROUNDSPEED    '/
      DATA CLINE(30)/'564) INS DRIFT ANGLE    565) INS PITCH RATE     '/
      DATA CLINE(31)/'566) INS YAW RATE       567) U - WIND NORTH     '/
      DATA CLINE(32)/'568) V - WIND EAST      569) W - WIND VERTICAL  '/
      DATA CLINE(33)/'536) RELATIVE HUMIDITY  551)                    '/
      DATA CLINE(34)/'607) GPS LATITUDE       608) GPS LONGITUDE      '/
      DATA CLINE(35)/'605) INU LATITUDE       606) INU LONGITUDE      '/
      DATA CLINE(36)/'578) PRESSURE HT  m     609) GPS ALTITUDE       '/
      DATA CLINE(37)/'554) PSAP LIN ABS COEFF 555) PSAP LOG ABS COEFF '/
      DATA CLINE(38)/'556) PSAP TRANSMITTANCE 619) CABIN PRESSURE     '/
      DATA CLINE(39)/'630) NOXY NO            631) NOXY NO2           '/
      DATA CLINE(40)/'632) NOXY NOY1          633) NOXY NOY2          '/
      DATA CLINE(41)/'620) NEPH PRESSURE      621) NEPH TEMPERATURE   '/
      DATA CLINE(42)/'622) NEPH BLUE SP       623) NEPH GREEN SP      '/
      DATA CLINE(43)/'624) NEPH RED SP        625) NEPH BLUE BSP      '/
      DATA CLINE(44)/'626) NEPH GREEN BSP     627) NEPH RED BSP       '/
      DATA CLINE(45)/'628) NEPH HUMIDITY      629) NEPH STATUS        '/      
      DATA CLINE(46)/'574) OZONE MIXING RATIO 588) CO MIXING RATIO    '/
      DATA CLINE(47)/'592) HCHO MIXING RATIO  612) 4PI JNO2           '/
      DATA CLINE(48)/'613) H2O2 MIXING RATIO  614) ORG H2O2 MIX RATIO '/
      DATA CLINE(49)/'593) ECGC SIGNAL 1      594) ECGC SIGNAL 2      '/
      DATA CLINE(50)/'595) ECGC SIGNAL 3      596) ECGC TEMP 1        '/
      DATA CLINE(51)/'597) ECGC TEMP 2        598) ECGC TEMP 3        '/
      DATA CLINE(52)/'599) ECGC PRESSURE 1    600) ECGC PRESSURE 2    '/
      DATA CLINE(53)/'601) ECGC PRESSURE 3    602) ECGC PEAK HGT 1    '/
      DATA CLINE(54)/'634) MOD H2O2 No PBL    639) MOD H2O2NOX No PBL '/
      DATA CLINE(55)/'635) MOD H2O2 0.5km PBL 640) MOD H2O2NOX 0.5km  '/
      DATA CLINE(56)/'636) MOD H202 1.0km PBL 641) MOD H2O2NOX 1.0km  '/
      DATA CLINE(57)/'637) MOD H2O2 1.5km PBL 642) MOD H2O2NOX 1.5km  '/
      DATA CLINE(58)/'638) MOD H2O2 2.0km PBL 643) MOD H2O2NOX 2.0km  '/
      DATA CLINE(59)/'644) MD2 H2O2 No PBL    649) MD2 H2O2NOX No PBL '/
      DATA CLINE(60)/'645) MD2 H2O2 0.5km PBL 650) MD2 H2O2NOX 0.5km  '/
      DATA CLINE(61)/'646) MD2 H202 1.0km PBL 651) MD2 H2O2NOX 1.0km  '/
      DATA CLINE(62)/'647) MD2 H2O2 1.5km PBL 652) MD2 H2O2NOX 1.5km  '/
      DATA CLINE(63)/'648) MD2 H2O2 2.0km PBL 653) MD2 H2O2NOX 2.0km  '/
      DATA CLINE(64)/'654) MOD OH  ppb        655) Theta W  K         '/
      DATA CLINE(65)/'656) JO1D   V                                   '/
      DATA CLINE(66)/'660) NEVZ LIQ WATER     661) NEVZ TOT WATER     '/
      DATA CLINE(67)/'657) TECO NO            658) TECO NO2           '/
      DATA CLINE(68)/'659) TECO NOx           611) TECO SO2           '/
      DATA CLINE(69)/'                                                '/
      DATA CLINE(70)/'                                                '/
      DATA CLINE(71)/'                                                '/
      DATA CLINE(72)/'                                                '/

      JJ=1
      IPAR=0
      CALL SEL(0,0,IBLACK)
      CALL RUB                         !clear display page
      CALL SEL(0,IBACKG,0)             !D green
      CALL BLK(0,0,767,509)            !Fill block with background
      CALL SEL(0,IFOREG,IBACKG)        !cyan/d green
      CALL SCA(13,13,0,3)              !character cell width, height, orientation, font
      CALL ALP(190,110,' MENU C - CHEMISTRY PARAMETERS')
      CALL FOOT('SELECT','CHEM',' ',' ',' ','PRINT',' ',' ')
      DO WHILE(IPAR.EQ.0)
        J=MOD(JJ,72)
        IY=150
        CALL SEL(0,IBACKG,0)           !D green
        CALL BLK(0,150,767,509)        !Fill block with background
        CALL SEL(0,IFOREG,IBACKG)      !cyan/d green
        DO I=J,J+7
          CALL ALP(10,IY,CLINE(I)(1:LAN(CLINE(I)))) !show next 8 lines
          IY=IY+30
        END DO
        CALL ALP(10,450,'Select '//CNA//
     +      ' (ENTER for more paras) ...      ')
        IPAR=-3
        DO WHILE((IPAR.LT.-2).OR.(IPAR.GT.661).OR.
     +      ((IPAR.GT.0).AND.(IPAR.LT.517)))
          CALL SELECT(490,450,IPAR)
        END DO
        JJ=JJ+8
      END DO
      IKEY=0
      IF(IPAR.LT.0) IKEY=64-IPAR
      RETURN
      END
C-------------------------------------------------------------------------------
C ROUTINE CHEM_TLAG_ZOOM
C
C Set scaling and set up for time lag plot
C
C Subroutines used:
C
C CHEM_GET_SCAL
C CHEM_LABELXS      
C HOR_PLOT_BACKG
C
C AUGUST 1992 Dave T
C-------------------------------------------------------------------------------
      SUBROUTINE CHEM_TLAG_ZOOM(IPARX,IPARY,ILGX,ILGY,I4A,I4B,I4C,I4D,
     +    IKEY,IFUNC)
      IMPLICIT  NONE
      CHARACTER CLAGS*11
      INTEGER*4 IQQ,IPARX,IPARY,I4A,I4B,I4C,I4D,IK,IP,ILGX,ILGY,
     +    IST,IFUNC,IKEY
      INCLUDE   'DCOM_DEF.FOR'
      INCLUDE   'HCOM_DEF.FOR'
      REAL*4    RTICVAL1,RTICVAL2,RTICINC1,RTICINC2,RYFACT2,RYFACT1,
     +    RXFACT1,RXFACT2
      INTEGER*4 LYOFF1,LYOFF2,LXOFF1,LXOFF2,LYPIX1,LYPIX2,LXPIX1,LXPIX2
      COMMON    /USERPLOT/ RTICVAL1,RTICVAL2,RTICINC1,RTICINC2,RYFACT1,
     +    RYFACT2,RXFACT1,RXFACT2,LYOFF1,LYOFF2,LXOFF1,LXOFF2,LYPIX1,
     +    LYPIX2,LXPIX1,LXPIX2

      CALL CHEM_GET_SCAL('X ',IFUNC,IPARX+512,I4A,I4B,IK)
      IF(IK.LT.0) RETURN
      CALL CHEM_GET_SCAL('Y ',IFUNC,IPARY+512,I4C,I4D,IK)
      IF(IK.LT.0) RETURN

      RTICVAL1=TICVAL(IPARY)+((I4C-1)*TICINC(IPARY)) !new origin
      RTICINC1=TICINC(IPARY)/I4D       !new label increment 
      LYPIX1=IYPIX(IPARY)              ! no of pixels tween tics
      LYPIX2=LYPIX1
      LYOFF1=480+(RTICVAL1/RTICINC1*LYPIX1) !new zero value pixel offset
      RYFACT1=LYPIX1/RTICINC1          !new y scaling factor

      CALL SEL(0,IBLACK,IBLACK)        !black
      CALL RUB                         !clear display page
      IFUNC=3
      CALL HOR_PLOT_BACKG(IFUNC,IPARY,IPARY,IKEY) !draw graph blank
      CALL SEL(0,IFOREG,IBLACK)        !Colour for y parameter
      CALL SCA(13,13,0,3)             
      CALL ALP(6,115,'        ')       !print blank
      CALL SCA(10,10,0,2)             
      CALL ALP(6,105,CPARA(IPARY))     !print y para name
      CALL ALP(190,105,CUNIT(IPARY))   !print y para units
      IP=1 
      CALL CHEM_LABELXS(IPARX,IP,I4A,I4B)
      IP=2
      IQQ=IWHITE
      IWHITE=IYELLOW
      CALL CHEM_LABELXS(IPARX,IP,I4A,I4B)
      IWHITE=IQQ
      CALL SCA(12,12,0,2)           
      CALL SEL(0,IFOREG,IBLACK)        !Colour for y parameter
      WRITE(CLAGS,'(A6,I3,A2)',IOSTAT=IST) 'Y Lag=',(ILGY*3),'s ' 
      CALL ALP(514,105,CLAGS)          !print y lag
      CALL SEL(0,IYELLOW,IBLACK)       !Colour for x parameter
      WRITE(CLAGS,'(A6,I3,A2)',IOSTAT=IST) 'X Lag=',(ILGX*3),'s ' 
      CALL ALP(641,105,CLAGS)          !print x lag
      LXOFF1=LXOFF2
      RXFACT1=RXFACT2
      IP=2
      CALL HEAD_SHELL
      CALL FOOT('SELECT','CHEM','PARAS','ZOOM',' ','PRINT',' ',' ')
      RETURN
      END
C-------------------------------------------------------------------------------
      SUBROUTINE CHEM_TLAG_BACKG(IPARX,IPARY,ILGX,ILGY,I4A,I4B,IKEY)
      IMPLICIT  NONE
      CHARACTER CLAGS*11
      INTEGER*4 IQQ,IPARX,IPARY,I4A,I4B,IP,ILGX,ILGY,
     +    IST,IFUNC,IKEY
      INCLUDE   'DCOM_DEF.FOR'
      INCLUDE   'HCOM_DEF.FOR'
      REAL*4    RTICVAL1,RTICVAL2,RTICINC1,RTICINC2,RYFACT2,RYFACT1,
     +    RXFACT1,RXFACT2
      INTEGER*4 LYOFF1,LYOFF2,LXOFF1,LXOFF2,LYPIX1,LYPIX2,LXPIX1,LXPIX2
      COMMON    /USERPLOT/ RTICVAL1,RTICVAL2,RTICINC1,RTICINC2,RYFACT1,
     +    RYFACT2,RXFACT1,RXFACT2,LYOFF1,LYOFF2,LXOFF1,LXOFF2,LYPIX1,
     +    LYPIX2,LXPIX1,LXPIX2

      CALL SEL(0,IBLACK,IBLACK)        !black
      CALL RUB                         !clear display page
      IFUNC=3
      CALL HOR_PLOT_BACKG(IFUNC,IPARY,IPARY,IKEY) !draw graph blank
      IF(IKEY.NE.0) RETURN
      CALL SEL(0,IFOREG,IBLACK)        !Colour for y parameter
      CALL SCA(13,13,0,3)             
      CALL ALP(6,115,'        ')       !print blank
      CALL SCA(10,10,0,2)             
      CALL ALP(6,105,CPARA(IPARY))     !print y para name
      CALL ALP(190,105,CUNIT(IPARY))   !print y para units
      IP=1 
      CALL CHEM_LABELXS(IPARX,IP,I4A,I4B)
      IP=2
      IQQ=IWHITE
      IWHITE=IYELLOW
      CALL CHEM_LABELXS(IPARX,IP,I4A,I4B)
      IWHITE=IQQ
      CALL SCA(12,12,0,2)           
      CALL SEL(0,IFOREG,IBLACK)        !Colour for y parameter
      WRITE(CLAGS,'(A6,I3,A2)',IOSTAT=IST) 'Y Lag=',(ILGY*3),'s ' 
      CALL ALP(514,105,CLAGS)          !print y lag
      CALL SEL(0,IYELLOW,IBLACK)       !Colour for x parameter
      WRITE(CLAGS,'(A6,I3,A2)',IOSTAT=IST) 'X Lag=',(ILGX*3),'s ' 
      CALL ALP(641,105,CLAGS)          !print x lag
      LXOFF1=LXOFF2
      RXFACT1=RXFACT2
      CALL HEAD_SHELL
      CALL FOOT('SELECT','CHEM','PARAS','ZOOM',' ','PRINT',' ',' ')
      RETURN
      END
C*******************************************************************************
      SUBROUTINE CHEM_PROF(IKEY)
C
C Get the arguments, then call chem profile.
C
      IMPLICIT  NONE
      INTEGER*4 ILIST(12),IKEY,ITIMP,IX1,IX2,I4A(2),I4B(2),I4C,I4D

10    CALL CHEM_PROF_PARA(ITIMP,IX1,IX2,IKEY)
      IF(CHAR(IKEY).EQ.'A') RETURN
      IF(CHAR(IKEY).EQ.'B') RETURN
20    CALL CHEM_GET_SCAL('X1',2,IX1,I4A(1),I4B(1),IKEY)
      IF(CHAR(IKEY).EQ.'A') RETURN
      IF(CHAR(IKEY).EQ.'B') RETURN
      IF(CHAR(IKEY).EQ.'C') GOTO 10
      CALL CHEM_GET_SCAL('X2',2,IX2,I4A(2),I4B(2),IKEY)
      IF(CHAR(IKEY).EQ.'A') RETURN
      IF(CHAR(IKEY).EQ.'B') RETURN
      IF(CHAR(IKEY).EQ.'C') GOTO 10
      CALL CHEM_GET_SCAL('HT',2,579,I4C,I4D,IKEY)
      IF(CHAR(IKEY).EQ.'A') RETURN
      IF(CHAR(IKEY).EQ.'B') RETURN
      IF(CHAR(IKEY).EQ.'C') GOTO 10
      ILIST(1)=12
      ILIST(2)=19
      ILIST(3)=2
      ILIST(4)=ITIMP
      ILIST(5)=IX1
      ILIST(6)=IX2
      ILIST(7)=I4A(1)
      ILIST(8)=I4B(1)
      ILIST(9)=I4A(2)
      ILIST(10)=I4B(2)
      ILIST(11)=I4C
      ILIST(12)=I4D
      CALL CHEM_PROF1(ILIST,IKEY)
      IF(CHAR(IKEY).EQ.'A') RETURN
      IF(CHAR(IKEY).EQ.'B') RETURN
      IF(CHAR(IKEY).EQ.'C') GOTO 10
      IF(CHAR(IKEY).EQ.'D') GOTO 20
      END
C-------------------------------------------------------------------------------
C ROUTINE CHEM_PROF1
C
C Two parameter profiles
C
C Subroutines used:
C
C CHEM_PROF_SETUP    This file
C CHEM_GET_SCAL            This file
C CHEM_LABELXS            This file
C CHEM_CHECK_KEY     This file
C FOOT               HOR_SUBS
C HEAD_SHELL         HOR_SUBS
C HEAD_UPDATE        HOR_SUBS
C
C AUGUST 1992 Dave T
C-------------------------------------------------------------------------------
      SUBROUTINE CHEM_PROF1(ILIST,IKEY)
      IMPLICIT  NONE
      INTEGER*4 IKEY,IP,IPARAY,IPARAX1,IPARAX2,ILAT,INUM,IPAS,IY,IX1,
     +    IX2,IST,I4C,I4D,IFUNC
!      CHARACTER*2 PNUM
      CHARACTER*8 CIX
      INTEGER*4 PARA(4),I4A(4),I4B(4),ITIMP,ILIST(12),II4A,II4B
      INCLUDE 'DCOM_DEF.FOR'
      INCLUDE 'HCOM_DEF.FOR'
      REAL*4    RTICVAL1,RTICVAL2,RTICINC1,RTICINC2,RYFACT2,RYFACT1,
     +    RXFACT1,RXFACT2
      INTEGER*4 LYOFF1,LYOFF2,LXOFF1,LXOFF2,LYPIX1,LYPIX2,LXPIX1,LXPIX2
      COMMON    /USERPLOT/ RTICVAL1,RTICVAL2,RTICINC1,RTICINC2,RYFACT1,
     +    RYFACT2,RXFACT1,RXFACT2,LYOFF1,LYOFF2,LXOFF1,LXOFF2,LYPIX1,
     +    LYPIX2,LXPIX1,LXPIX2

      CALL UPDT_STATUS(ILIST)
      IFUNC=ILIST(3)
      ITIMP=ILIST(4)
      PARA(1)=ILIST(5)
      PARA(2)=ILIST(6)
      I4A(1)=ILIST(7)
      I4B(1)=ILIST(8)
      I4A(2)=ILIST(9)
      I4B(2)=ILIST(10)
      I4C=ILIST(11)
      I4D=ILIST(12)
      IPARAY=67                      !Pressure height  kft (28-jun-1997) m to kft
      RTICVAL1=TICVAL(IPARAY)+((I4C-1)*TICINC(IPARAY)) !new origin
      RTICINC1=TICINC(IPARAY)/I4D    !new label increment 
      LYPIX1=IYPIX(IPARAY)           ! no of pixels tween tics
      LYPIX2=LYPIX1
      LYOFF1=480+(RTICVAL1/RTICINC1*LYPIX1) !new zero value pixel offset
      RYFACT1=LYPIX1/RTICINC1        !new y scaling factor
      CALL SEL(0,IBLACK,IBLACK)      !black
      CALL RUB                       !clear display page
      CALL CHEM_PROF_BACKG(IPARAY,IPARAY) !draw graph blank
      CALL SEL(0,IFOREG,IBLACK)      !Colour for y parameter
      CALL SCA(13,13,0,3)             
      CALL ALP(6,115,'        ')     !print blank
      CALL SCA(10,10,0,2)             
      CALL ALP(6,105,CPARA(IPARAY))  !print y para name
      CALL ALP(190,105,CUNIT(IPARAY)) !print y para units
      IP=1
      IPARAX1=PARA(IP)-512
      II4A=I4A(IP)
      II4B=I4B(IP)
      CALL CHEM_LABELXS(IPARAX1,IP,II4A,II4B)
      LXOFF1=LXOFF2
      RXFACT1=RXFACT2
      IP=2
      IPARAX2=PARA(IP)-512
      II4A=I4A(IP)
      II4B=I4B(IP)
      CALL CHEM_LABELXS(IPARAX2,IP,II4A,II4B)
      CALL HEAD_SHELL
      CALL FOOT('SELECT','CHEM','PARAS','ZOOM',' ','PRINT',' ',' ')
      ILAT=STATUS(2)
      INUM=STATUS(3)
      IF(INUM.GT.ITIMP) INUM=ITIMP
20    CALL SCA(13,13,0,3)             
      DO IPAS=(1-INUM),0
        CALL CHECK_KEY('ABCD',IKEY)
        IF(IKEY.NE.0) RETURN
        IP=ILAT+IPAS
        IP=1+(MOD((IP-1),12800))
        IF(IP.LT.1) IP=IP+12800
        IY=IFIX(LYOFF1-RYFACT1*DERIVE(IP,IPARAY))
        IF(IY.GT.479) IY=479       !Test for outside graf area
        IF(IY.LT.151) IY=151
        IX1=IFIX(LXOFF1+RXFACT1*DERIVE(IP,IPARAX1))
        IF(IX1.GT.669) IX1=669     !Test for outside graf area
        IF(IX1.LT.101) IX1=101
        WRITE(CIX,'(F8.2)',IOSTAT=IST) DERIVE(IP,IPARAX1)
        CALL SEL(0,IYELLOW,IBLACK) !Colour for x1  parameter
        IF(IPAS.EQ.0) CALL ALP(340,105,CIX) !print x1 para
        CALL BIT(IX1,IY)           !draw dot only on x1y plot    
        IX2=IFIX(LXOFF2+RXFACT2*DERIVE(IP,IPARAX2))
        IF(IX2.GT.669) IX2=669     !Test for outside graf area
        IF(IX2.LT.101) IX2=101
        WRITE(CIX,'(F8.2)',IOSTAT=IST) DERIVE(IP,IPARAX2)
        CALL SEL(0,IWHITE,IBLACK)  !Colour for x2 parameter
        IF(IPAS.EQ.0) CALL ALP(594,105,CIX) !print x2 para
        CALL BIT(IX2,IY)           !draw dot only on x2y plot    
        CALL SEL(0,IFOREG,IBLACK)  !Colour for y parameter
        WRITE(CIX,'(F8.2)',IOSTAT=IST) DERIVE(IP,IPARAY)
        IF(IPAS.EQ.0) CALL ALP(86,105,CIX) !print y para
      END DO
      INUM=0
      DO WHILE(INUM.EQ.0)
        CALL LIB$WAIT(0.2)
        CALL CHECK_KEY('ABCD',IKEY)
        IF(IKEY.NE.0) RETURN
        INUM=STATUS(2)-ILAT
        INUM=(MOD((INUM),12800))
        IF(INUM.LT.0) INUM=INUM+12800
        ILAT=STATUS(2)
      END DO
      CALL HEAD_UPDATE
      GOTO 20
      END
C-------------------------------------------------------------------------------
C ROUTINE CHEM_PROF_PARA
C
C Set up paras for profiles
C
C Subroutines used:
C
C CHEM_GET_PARA            This file
C
C AUGUST 1992 Dave T
C-------------------------------------------------------------------------------
      SUBROUTINE CHEM_PROF_PARA(ITIMP,IX1,IX2,IKEY) 
      IMPLICIT  NONE
      INTEGER*4 IKEY,IX1,IX2,ITIMP
      INCLUDE   'DCOM_DEF.FOR'

      IKEY=0
      CALL SEL(0,0,IBLACK)
      CALL RUB                         !clear display page
      CALL SEL(0,IBACKG,0)             !D green
      CALL BLK(0,0,767,509)            !Fill block with background
      CALL FOOT('SELECT','CHEM',' ',' ',' ','PRINT',' ',' ')
      CALL ALP(30,450,
     +    'How far back in minutes (max=128, default=20)? ')
      ITIMP=-10
      DO WHILE(ITIMP.LT.-2.OR.ITIMP.GT.128)
        CALL SELECT(641,450,ITIMP)
      END DO
      IF(ITIMP.LT.0) THEN
        IKEY=64-ITIMP
        RETURN
      END IF
      IF(ITIMP.EQ.0) ITIMP=20          !Default value
      CALL CHEM_GET_PARA(IX1,'X1',IKEY)
      IF(IKEY.NE.0) RETURN        
      CALL CHEM_GET_PARA(IX2,'X2',IKEY)
      RETURN
      END
C*******************************************************************************
      SUBROUTINE CHEM_PROF_BACKG(IPARAY1,IPARAY2)
C
C Draws blank graph and labels
C
      IMPLICIT  NONE
      INTEGER*4 IPARAY1,IPARAY2,IY,IP,IST
      REAL*4    PRSS(8),HGTS(8),FIRSTTIC
      CHARACTER CLABEL*8
      REAL*4    RTICVAL1,RTICVAL2,RTICINC1,RTICINC2,RYFACT2,RYFACT1,
     +    RXFACT1,RXFACT2
      INTEGER*4 LYOFF1,LYOFF2,LXOFF1,LXOFF2,LYPIX1,LYPIX2,LXPIX1,LXPIX2
      COMMON    /USERPLOT/ RTICVAL1,RTICVAL2,RTICINC1,RTICINC2,RYFACT1,
     +    RYFACT2,RXFACT1,RXFACT2,LYOFF1,LYOFF2,LXOFF1,LXOFF2,LYPIX1,
     +    LYPIX2,LXPIX1,LXPIX2
      INCLUDE 'DCOM_DEF.FOR'
C Pressures in mb
      DATA PRSS/1000.0,900.0 ,800.0 ,700.0 ,600.0 
     &                                  ,500.0  ,400.0 ,300.0 /
C Heights in kft
      DATA HGTS/0.3638,3.2432,6.3945,9.8828,13.801
     &                                 ,18.2894,23.575,30.066/
C
C Draw plotting area
C
      CALL SEL(0,IBACKG,IBLACK)        !Dk green
      CALL BLK(101,151,669,479)        !fill box
      CALL SEL(0,IFOREG,IBLACK)        !Cyan/black
      CALL BOXA(100,150,670,480)       !draw box
      CALL BOXA(99,149,671,481)
C
C Put on parameter name(s) and units
C
      CALL SCA(13,13,0,2)              !large letters
      CALL SEL(0,IFOREG,-1)            !cyan/additive
      CALL ALP(30,115,CPARA(IPARAY1))  !Print para name   
      CALL ALP(30,129,CUNIT(IPARAY1))  !print units   
      IF(IPARAY1.NE.IPARAY2) THEN      !do second parameter name
        CALL SEL(0,IYELLOW,-1)         !yellow/additive
        CALL ALP(270,115,CPARA(IPARAY2)) !Print para name   
        CALL ALP(270,129,CUNIT(IPARAY2)) !print units   
        CALL SEL(0,IFOREG,-1)          !back to cyan/additive
      END IF         
C
C Do y axis left tics
C
      DO IY=150,480,LYPIX1             !draw tics on yaxis
        CALL VEC(100,IY,94,IY)
      END DO
C
C Do y axis left labels
C
      CALL SEL(0,IFOREG,-1)            !cyan/additive
      CALL SCA(10,10,0,2)              !7*9 small letters
      FIRSTTIC=RTICVAL1                !save first tic label
      DO IY=475,145,-LYPIX1            !write labels
        WRITE(CLABEL,11,IOSTAT=IST) FIRSTTIC
        CALL ALP(10,IY,CLABEL)                
        FIRSTTIC=FIRSTTIC+RTICINC1     !increment label
      END DO
C
C Do y axis right tics (in yellow if two parameters)
C
      IF(IPARAY1.NE.IPARAY2) CALL SEL(0,IYELLOW,-1) !yellow/additive
      CALL SCA(10,10,0,2)              !7*9 small letters
      IF((IPARAY1.EQ.IPARAY2).AND.(IPARAY1.EQ.67)) THEN
C
C       Do height and pressure axis
C
        DO IP=1,8 
          IY=IFIX(LYOFF1-RYFACT1*HGTS(IP))
          IF((IY.LE.479).AND.(IY.GE.151)) THEN
            CALL VEC(670,IY,676,IY)
            WRITE(CLABEL,11,IOSTAT=IST) PRSS(IP)
            CALL ALP(680,IY,CLABEL)
          END IF
        END DO
      ELSE
        DO IY=150,480,LYPIX2
          CALL VEC(670,IY,676,IY)
        END DO
C
C Do y axis right labels
C
!        IF(IFUNC.EQ.3)THEN
          FIRSTTIC=RTICVAL1              !x y plot
!        ELSE
!          FIRSTTIC=RTICVAL2              !save first tic label
!        END IF
        DO IY=475,145,-LYPIX2            !write labels
          WRITE(CLABEL,11,IOSTAT=IST) FIRSTTIC
          CALL ALP(680,IY,CLABEL)                
!          IF(IFUNC.EQ.3)THEN             !x y plot
            FIRSTTIC=FIRSTTIC+RTICINC1   !increment label
!          ELSE    
!            FIRSTTIC=FIRSTTIC+RTICINC2   !increment label
!          END IF
        END DO
      END IF
C
      RETURN
  11  FORMAT(F8.2)
      END
C-------------------------------------------------------------------------------
C ROUTINE CHEM_LABELXS
C
C Label top or bottom X axis
C
C Subroutines used:
C
C AUGUST 1992 Dave T
C-------------------------------------------------------------------------------
      SUBROUTINE CHEM_LABELXS(IPARAX,IP,I4A,I4B)
C  labels xaxis & tic marks
      IMPLICIT  NONE
      INTEGER*4 IPARAX,IP,I4A,I4B,IY,IYTICK,IXTIT,ICOL,IX,IST
      REAL*4    FIRSTTIC
      CHARACTER CLABEL*8
      REAL*4    RTICVAL1,RTICVAL2,RTICINC1,RTICINC2,RYFACT2,RYFACT1,
     +    RXFACT1,RXFACT2
      INTEGER*4 LYOFF1,LYOFF2,LXOFF1,LXOFF2,LYPIX1,LYPIX2,LXPIX1,LXPIX2
      COMMON    /USERPLOT/ RTICVAL1,RTICVAL2,RTICINC1,RTICINC2,RYFACT1,
     +    RYFACT2,RXFACT1,RXFACT2,LYOFF1,LYOFF2,LXOFF1,LXOFF2,LYPIX1,
     +    LYPIX2,LXPIX1,LXPIX2
      INCLUDE 'DCOM_DEF.FOR'
C
C print x parameter name and units
C
      RTICVAL2=TICVAL(IPARAX)+((I4A-1)*TICINC(IPARAX)) !new origin
      RTICINC2=TICINC(IPARAX)/I4B      !new label increment
      LXPIX2=IXPIX(IPARAX)             ! no of pixels tween tics
      LXOFF2=100-(RTICVAL2/RTICINC2*LXPIX2) !new zero value pixel offset
      RXFACT2=LXPIX2/RTICINC2          !new x scaling factor
      IP=IP-1
      IY=130+(360*IP)
      IYTICK=145+(335*IP)
      IXTIT=260+(254*IP)
      ICOL=IWHITE
      IF(IP.EQ.0) ICOL=IYELLOW 
      CALL SEL(0,ICOL,IBLACK)          !Colour for x parameter
      CALL SCA(10,10,0,2)             
      CALL ALP(IXTIT,105,CPARA(IPARAX)) !print x para name
      CALL ALP(IXTIT+184,105,CUNIT(IPARAX)) !print x para units
C
C do tics
C
      CALL SCA(10,10,0,2)             
      DO IX=100,670,LXPIX2             !tics
        CALL VEC(IX,IYTICK,IX,IYTICK+5)
      END DO    
C
C x axis labels
C
      FIRSTTIC=RTICVAL2                !save first ticlabel
      DO IX=60,630,LXPIX2              !write labels
        WRITE(CLABEL,11,IOSTAT=IST) FIRSTTIC
        CALL ALP(IX,IY,CLABEL)
        FIRSTTIC=FIRSTTIC+RTICINC2     !increment label
      END DO
   11 FORMAT(F8.2)
      RETURN
      END  
C*******************************************************************************
      SUBROUTINE CHEM_ECGC(IKEY)
C
C Get the arguments, then call chem dump.
C
      IMPLICIT  NONE
      INTEGER*4 ILIST(5),IORIG,ISCAL,IKEY

10    CALL CHEM_ECGC_SCAL(IORIG,ISCAL,IKEY)
      IF(CHAR(IKEY).EQ.'A') RETURN
      IF(CHAR(IKEY).EQ.'B') RETURN
      ILIST(1)=5
      ILIST(2)=19
      ILIST(3)=1
      ILIST(4)=IORIG
      ILIST(5)=ISCAL
      CALL CHEM_ECGC1(ILIST,IKEY)
      IF(CHAR(IKEY).EQ.'A') RETURN
      IF(CHAR(IKEY).EQ.'B') RETURN
      IF(CHAR(IKEY).EQ.'C') GOTO 10
      END
C-------------------------------------------------------------------------------
C ROUTINE CHEM_ECGC1
C
C Produce three plots for the GCs
C
C Subroutines used:
C
C CHEM_ECGC_BACKG    This file
C CHEM_ECGC_SCAL     This file
C CHEM_CHECK_KEY     This file
C HEAD_UPDATE        HOR_SUBS
C
C AUGUST 1992 Dave T
C-------------------------------------------------------------------------------
      SUBROUTINE CHEM_ECGC1(ILIST,IKEY)
      IMPLICIT  NONE
      INTEGER*4 IKEY,ILATEDER,INUMDER,ITICK,IPAS,IDER1,IDER2,IGC,IX,IY,
     +    I4A,I4B,IOY
      REAL*4    RVAL,ROVAL
      INTEGER*4 ILIST(5)
      CHARACTER TEMP*15,PRESS*15
      INCLUDE   'DCOM_DEF.FOR'
      INCLUDE   'HCOM_DEF.FOR'
      REAL*4    RTICVAL1,RTICVAL2,RTICINC1,RTICINC2,RYFACT2,RYFACT1,
     +    RXFACT1,RXFACT2
      INTEGER*4 LYOFF1,LYOFF2,LXOFF1,LXOFF2,LYPIX1,LYPIX2,LXPIX1,LXPIX2
      COMMON    /USERPLOT/ RTICVAL1,RTICVAL2,RTICINC1,RTICINC2,RYFACT1,
     +    RYFACT2,RXFACT1,RXFACT2,LYOFF1,LYOFF2,LXOFF1,LXOFF2,LYPIX1,
     +    LYPIX2,LXPIX1,LXPIX2

      IKEY=ICHAR('B')
      IF(NPLOC(41).EQ.0) RETURN        !ECGCs not recorded
      CALL UPDT_STATUS(ILIST)
      I4A=ILIST(4)
      I4B=ILIST(5)
      CALL CHEM_ECGC_BACKG
      ILATEDER=STATUS(2)
      INUMDER=STATUS(3)
      ITICK=20
20    IF(INUMDER.GT.119) INUMDER=119
      CALL SEL(0,IWHITE,IBLACK)
      DO IPAS=-INUMDER,-1
        CALL CHECK_KEY('ABC',IKEY)
        IF(IKEY.NE.0) RETURN
        IDER1=ILATEDER+IPAS
        IDER2=IDER1+1
        IDER1=1+(MOD((IDER1-1),12800))
        IF(IDER1.LT.1) IDER1=IDER1+12800
        IDER2=1+(MOD((IDER2-1),12800))
        IF(IDER2.LT.1) IDER2=IDER2+12800
        IX=(726+(5*IPAS))
        DO IGC=0,2
          RVAL=(DERIVE(IDER2,81+IGC))
          ROVAL=(DERIVE(IDER1,81+IGC))
          IOY=IFIX(LYOFF1-RYFACT1*ROVAL)
          IF(IOY.GT.0) IOY=0           !Test for outside graf area
          IF(IOY.LT.-100) IOY=-100
          IOY=IOY+230+(IGC*120)
          IY=IFIX(LYOFF1-RYFACT1*RVAL)
          IF(IY.GT.0) IY=0             !Test for outside graf area
          IF(IY.LT.-100) IY=-100
          IY=IY+230+(IGC*120)
          CALL VEC(IX,IOY,IX+5,IY)
        ENDDO
      ENDDO
      CALL SCA(10,10,0,2)              !Normal characters
      CALL SEL(0,IYELLOW,IBACKG)
      DO IGC=0,2
        IY=IGC*120+114
        WRITE(TEMP,'(A5,F8.2)') 'TEMP ',(DERIVE(ILATEDER,84+IGC))
        WRITE(PRESS,'(A6,F8.2)') 'PRESS ',(DERIVE(ILATEDER,87+IGC))
        CALL ALP(16,IY,TEMP)
        CALL ALP(156,IY,PRESS)
      END DO
      INUMDER=0
      DO WHILE(INUMDER.EQ.0)
        CALL LIB$WAIT(0.2)
        CALL CHECK_KEY('ABC',IKEY)
        IF(IKEY.NE.0) RETURN
        INUMDER=STATUS(2)-ILATEDER
      END DO
      INUMDER=(MOD((INUMDER),12800))
      IF(INUMDER.LT.0) INUMDER=INUMDER+12800
      ILATEDER=STATUS(2)
      CALL ZOO(126+INUMDER*5,130,726,230,126,130,726-INUMDER*5,230)
      CALL ZOO(126+INUMDER*5,250,726,350,126,250,726-INUMDER*5,350) 
      CALL ZOO(126+INUMDER*5,370,726,470,126,370,726-INUMDER*5,470) 
      CALL SEL(0,IBLACK,IWHITE)
      CALL BLK(727-INUMDER*5,130,726,230)
      CALL BLK(727-INUMDER*5,250,726,350)
      CALL BLK(727-INUMDER*5,370,726,470)
      ITICK=ITICK-INUMDER
      IF(ITICK.LE.0) THEN 
        CALL SEL(0,IWHITE,IBACKG)                
        CALL VEC(726+ITICK*5,227,726+ITICK*5,231)
        CALL VEC(726+ITICK*5,347,726+ITICK*5,351)
        CALL VEC(726+ITICK*5,467,726+ITICK*5,471)
        ITICK=ITICK+20
      END IF
      CALL SEL(0,IWHITE,IBACKG)                
      CALL HEAD_UPDATE
      GOTO 20
      END
C-------------------------------------------------------------------------------
C ROUTINE CHEM_ECGC_BACKG
C
C Set up screen with 3 graph axes and labels for plotting:
C
C Subroutines used:
C
C FOOT               HOR_SUBS
C HEAD_SHELL         HOR_SUBS
C
C AUGUST 1992 Dave T
C-------------------------------------------------------------------------------
      SUBROUTINE CHEM_ECGC_BACKG
      IMPLICIT  NONE
      INTEGER*4 IT
      CHARACTER CX_LABEL*10 /'TIME MINS'/
      CHARACTER CMX*4,CMN*4
      INCLUDE   'DCOM_DEF.FOR'
      REAL*4    RTICVAL1,RTICVAL2,RTICINC1,RTICINC2,RYFACT2,RYFACT1,
     +    RXFACT1,RXFACT2
      INTEGER*4 LYOFF1,LYOFF2,LXOFF1,LXOFF2,LYPIX1,LYPIX2,LXPIX1,LXPIX2
      COMMON    /USERPLOT/ RTICVAL1,RTICVAL2,RTICINC1,RTICINC2,RYFACT1,
     +    RYFACT2,RXFACT1,RXFACT2,LYOFF1,LYOFF2,LXOFF1,LXOFF2,LYPIX1,
     +    LYPIX2,LXPIX1,LXPIX2

C Set up screen
      CALL SEL(0,0,IBLACK)
      CALL RUB
      CALL SEL(0,IBACKG,0)             !Dark green
      CALL BLK(15,102,751,509)         !Fill block with background
      CALL SEL(0,IBLACK,IWHITE)
      CALL BLK(66,130,726,230)
      CALL BLK(66,250,726,350)
      CALL BLK(66,370,726,470)
C Tick marks and labels:
      WRITE(CMN,'(F4.1)') RTICVAL1
      WRITE(CMX,'(F4.1)') RTICVAL1+(6*RTICINC1)
      CALL SEL(0,IWHITE,IBACKG)
      CALL BOXA(65,129,727,231)                          
      CALL BOXA(65,249,727,351)                          
      CALL BOXA(65,369,727,471)                          
      CALL SCA(8,10,2,2)               !Vertical small characters
      CALL ALP(51,155,'GC 1')
      CALL ALP(51,275,'GC 2')
      CALL ALP(51,395,'GC 3')
      CALL SCA(10,10,0,2)              !Normal small characters
      CALL ALP(16,130,CMX)
      CALL ALP(16,220,CMN)
      CALL ALP(16,250,CMX)
      CALL ALP(16,340,CMN)
      CALL ALP(16,370,CMX)
      CALL ALP(16,460,CMN)
      CALL ALP(406,232,CX_LABEL)
      CALL ALP(406,352,CX_LABEL)
      CALL ALP(406,472,CX_LABEL)
      DO IT=126,726,100
        CALL VEC(IT,227,IT,231)
        CALL VEC(IT,347,IT,351)
        CALL VEC(IT,467,IT,471)
      ENDDO
      CALL HEAD_SHELL
      CALL FOOT('SELECT','CHEM','ZOOM',' ',' ','PRINT',' ',' ')
      RETURN
      END
C-------------------------------------------------------------------------------
C ROUTINE CHEM_ECGC_SCAL
C
C Set up scales for GC plot
C
C Subroutines used:
C
C CHEM_GET_SCAL            This file
C
C AUGUST 1992 Dave T
C-------------------------------------------------------------------------------
      SUBROUTINE CHEM_ECGC_SCAL(I4A,I4B,IKEY)
      IMPLICIT  NONE
      INTEGER*4 I4A,I4B,IKEY,IPARAY,IFUNC
      REAL*4    RTICVAL1,RTICVAL2,RTICINC1,RTICINC2,RYFACT2,RYFACT1,
     +    RXFACT1,RXFACT2
      INTEGER*4 LYOFF1,LYOFF2,LXOFF1,LXOFF2,LYPIX1,LYPIX2,LXPIX1,LXPIX2
      COMMON    /USERPLOT/ RTICVAL1,RTICVAL2,RTICINC1,RTICINC2,RYFACT1,
     +    RYFACT2,RXFACT1,RXFACT2,LYOFF1,LYOFF2,LXOFF1,LXOFF2,LYPIX1,
     +    LYPIX2,LXPIX1,LXPIX2
      INCLUDE 'DCOM_DEF.FOR'

      IFUNC=1
      CALL CHEM_GET_SCAL('GC',IFUNC,593,I4A,I4B,IKEY)
      IF(IKEY.NE.0) RETURN
      IPARAY=81                        !ECGC1
      RTICVAL1=TICVAL(IPARAY)+((I4A-1)*TICINC(IPARAY)) !new origin
      RTICINC1=TICINC(IPARAY)/i4B      !new label increment 
      LYPIX1=IYPIX(IPARAY)             ! no of pixels tween tics
      LYOFF1=(RTICVAL1/RTICINC1*LYPIX1)/3.3 !new zero value pixel offset
      RYFACT1=(LYPIX1/RTICINC1)/3.3    !new y scaling factor
      RETURN
      END
C-------------------------------------------------------------------------------
C
C ROUTINE CHEM_DUMP
C
C Displays derived parameters values for chemistry parameters in tabular form.
C The display is updated every 3 seconds, when a new set of derived data 
C becomes available.
C
C External data:
C
C DERIVE  R*4  Passed   HOR_CALCS    12800 secs of 196 derived parameters
C                                    contained in a circular buffer
C
C Subroutines called:
C     
C     DUMP_BACKG
C     DUMP_UPDATE
C     FOOT
C
C-------------------------------------------------------------------------------
      SUBROUTINE CHEM_DUMP(IKEY)
      IMPLICIT  NONE
      INTEGER*4 IS,IWA,IWS,IST,IKEY,ILIST(3)
      CHARACTER CLINE(6)*56
      INCLUDE   'DCOM_DEF.FOR'
      INCLUDE   'HCOM_DEF.FOR'
      INTEGER*2 IDOTS2(16)
      DATA IDOTS2 /960,1056,2064,4104,4104,6168,6120,4104,4104,4104,
     -    4104,4104,4104,2064,1056,960/
      CHARACTER CHEAD(12)*56 /
     -'   HDG    SPR    PHGT   TAS    TAT    WIND       ',
     -'  degT     mb    kft   knots    C  deg m/s       ',
     -'   DEW   FLDP                    e               ',
     -'    C      C             K       K               ',
     -'  TWCDP                              J*100       ',
     -'    C                                  Hz        ',
     -'  ECGC1 GCTMP1  GCPR1 GCPkH1  H2O2    CO    OZMR ',
     -'    V     K      mb     V       ppB   ppB    ppB ',
     -'  ECGC2 GCTMP2  GCPR2 GCPkH2          NO1   NO2  ',
     -'    V     K      mb     V             ppB   ppB  ',
     -'  ECGC3 GCTMP3  GCPR3 GCPkH3         NOY1  NOY2  ',
     -'    V     K      mb     V             ppB   ppB  '/

      IF(STATUS(2).LT.1.OR.STATUS(2).GT.12800) RETURN
      ILIST(1)=3
      ILIST(2)=19
      ILIST(3)=4
      CALL UPDT_STATUS(ILIST)
C
C Write fixed parts of the display to the access page the first time the
C routine is called.
C
      CALL SEL(0,IBLACK,IBLACK)        !black
      CALL RUB                         !clear display page
      CALL SAP(1)
      CALL DUMP_BACKG(CHEAD)           !draw fixed parts of display
C Write THETA symbols on the display
      CALL SCA(13,13,0,3)              !set 7x9 double
      CALL SSA(16,16,0)                !Define theta size
      CALL SYM2(IDOTS2,'T')            !define theta symbol
      CALL SYM(325,110,'T')            !display theta  
      CALL SYM(325,111,'T')            !intensify
      CALL SYM(416,110,'T')            !display theta  
      CALL SYM(416,111,'T')            !intensify
      CALL FOOT('SELECT','CHEM',' ',' ',' ','PRINT',' ',' ')
      CALL SAP(0)
C
C Generate the formatted values into lines using internal write statements.
C
100   IS=STATUS(2)
      IWA=NINT(RIWA(IS))               !convert winds to integer
      IWS=NINT(RIWS(IS))
      WRITE(CLINE(1),11,IOSTAT=IST) RIHDG(IS),RSPR(IS),RPHGTF(IS),
     -    RTAS(IS),RTATDC(IS),IWA,IWS
      WRITE(CLINE(2),12,IOSTAT=IST) RDEW(IS),RFLDP(IS),RPOT(IS),
     -    RPOTE(IS)
      WRITE(CLINE(3),13,IOSTAT=IST) RTWCDP(IS),SPARE(IS)
      WRITE(CLINE(4),14,IOSTAT=IST) RECGC1(IS),RGCTEMP1(IS),
     -    RGCPRES1(IS),RGCPKHT1(IS),SPARE(IS),SPARE(IS),
     -    ROZMR(IS)
      WRITE(CLINE(5),15,IOSTAT=IST) RECGC2(IS),RGCTEMP2(IS),
     -    RGCPRES2(IS),RGCPKHT2(IS),SPARE(IS),SPARE(IS)
      WRITE(CLINE(6),16,IOSTAT=IST) RECGC3(IS),RGCTEMP3(IS),
     -    RGCPRES3(IS),RGCPKHT3(IS),SPARE(IS),SPARE(IS)
C
C Write data to the screen.  The lines are stored on the graphics board
C and then written displaced one pixel down to thicken them.
C
      CALL SAP(1)
      CALL DUMP_UPDATE(CLINE)
      CALL ZOO(1,0)                    !copy access page to display page
      CALL SAP(0)
C
      DO WHILE(.TRUE.)
        CALL CHECK_KEY('AB',IKEY)
        IF(IKEY.NE.0) RETURN
        IF(STATUS(2).NE.IS) GOTO 100
        CALL LIB$WAIT(0.2)
      END DO
C
   11 FORMAT(1X,F6.1,1X,F6.1,1X,F6.1,1X,F6.0,2X,F5.1,I4.3,'/',I2)
   12 FORMAT((1X,F6.2),1X,F6.1,7X,2(1X,F6.1))
   13 FORMAT((1X,F6.2),28X,1X,F6.2)
   14 FORMAT(1X,F6.3,1X,F6.1,1X,F6.1,1X,F6.3,4(1X,F6.1))
   15 FORMAT(1X,F6.3,1X,F6.1,1X,F6.1,1X,F6.3,7X,3(1X,F6.1))
   16 FORMAT(1X,F6.3,1X,F6.1,1X,F6.1,1X,F6.3,7X,3(1X,F6.1))
      END
