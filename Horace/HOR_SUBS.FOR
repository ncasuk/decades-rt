C
C HOR_SUBS.FOR
C
C HOR_SUBS contains the following subroutines which are used by the H_DISPLAY
C program.  These are normally stored in the HORACE.OLB object library:-
C
C   GET_FUNC               Draws SELECT menu
C   GET_PARA               Draws PARAMETER menu for standard parameter plots
C   GET_PARA_TEPHI         Draws PARAMETER menu for Tephi 
C   GET_PARA_MARSSPLOT     Draws PARAMETER menu for MARSS plots
C   GET_FREQ               Draws FREQUENCY menu for standard and marss plots
C   GET_FREQ_HODO          Draws FREQUENCY menu for HODO plots
C   GET_SCAL_TEPHI         Draws ZOOM menu for Tephi
C   GET_SCAL_PLOT          Draws ZOOM menu for all plots
C   GRFINI                 Initializes graphics,Reads plotting constant files
C   GETSET                 Sets up correct resolution for each display
C   VIDPNT                 Prints current video channel
C   VIDRST                 Resets and refreshes video switch
C   HELP                   Draws a help page
C   HEAD_SHELL             Draws standard header background
C   HEAD_UPDATE            Updates standard header background
C   FOOT                   Draws standard bottom boxes
C   DUMP_BACKG
C   DUMP_UPDATE
C   TOP_LINE
C
C   Changes  01    27/01/89   Tephi zoom facility
C            02    17/04/89   Restrict tephi zoom menu to fewer lines
C                             MCR menus
C            03    16/05/89   MCR scroll
C            04    18/05/89   Plot double pixels headers and tails
C            05    08/06/89   Hodograph
C            06    17/08/89   New paras - RAL,REFRM
C            07    04/09/89   Aircraft Track
C            08    20/11/89   Video switching (commented out)
C                             Plotting zoom menus    
C            09    18/04/90   New paras - ST,TWCDP,SO2
C                             TWCDP on Tephi
C            10    02/05/90   Video switching, Marss menus
C            11    26/07/90   New paras - ECGC
C   WDNJ     12    11/10/90   Instruments option added
C                             Video and print options now displayed
C   WDNJ     13    17/10/90   PRINTER_FLAG subroutine added
C   WDNJ     14    14/01/91   Uses default of 3 for ozone scale factor
C   WDNJ     15    06/03/91   Parameter 605 - NOXMR added
C   WDNJ     16    07/03/91   Parameter 606 - NOMR added
C   WDNJ     17    14/05/91   GPS parameters 607 to 609 added
C                             10m wind speed parameter 610 added
C                             HCOMCOM renamed HCOM_DEF
C                             Manual hygro parameter 611 added
C                             Track plotting range with high mag is now global,
C                              full European coast, new track plotting method
C   WDNJ     18    20/06/91   KFA parameters 612 to 617 added
C   WDNJ     19    03/07/91   Scintrex NO2 parameter 618 added
C   WDNJ     20    23/04/92   Chemistry option added
C                             New HOR_PLOT_CONSTS format used
C                             Now handles initialization of Ethernet cards
C                             Obsolete parameters removed from para list
C   DRL      21    03/11/92   Lifting condensation level added as para 546
C   DRL      22    05/11/92   FWVS humidity mixing ratio add as para 547, also
C                             solar zenith angle added as parameter 552.
C   DRL      23    16/11/92   Displays current lat. and long. on zoom screen
C                             for track plots.
C   DRL      24    18/03/93   Updated paras 612-617
C   DRL      25    01/03/94   SAFIRE plotting options added
C   WDNJ     26    02/09/94   SO2, NO & NOx options (paras 592,605,606) removed
C   DRL      27    01/09/94   DEIMOS plotting option added
C   WDNJ     28    07/12/95   MCR no longer handled.  VIDRST added.
C
C V2.00  24/09/99  W.D.N.JACKSON
C
C*******************************************************************************
      SUBROUTINE GET_FUNC(IFUNC)
C
C Displays the main HORACE menu, and gets the user's choice of option.  If the
C HELP key is pressed, the help text is displayed and the menu redisplayed when
C the user has finished with the help.  The only other immediate keys which
C work are the VIDEO and PRINT keys, which are handled immediately if they are
C pressed while waiting for user input.  As a special way of leaving the
C display program, 99 is accepted as an option, which causes the program to
C terminate after putting the display in VDU mode, and asking the user to
C wait for the next operation.  When exits on 99 $STATUS is set to 99 to the
C special exit can be distinguished from a program crash.
C
C On return IFUNC contains the number of the option requested, between 1 and
C the max option number.
C
C Subroutines used:
C
C   FOOT         This file
C   SELECT       HOR_UTIL
C   ggs          GGSX.OLB
C
      IMPLICIT  NONE
      INTEGER*4 LAN,NOPTION,I,IY,IFUNC,IOS
      CHARACTER CLINE(12)*60
      INCLUDE   'DCOM_DEF.FOR'
      INCLUDE   'HCOM_DEF.FOR'
      DATA CLINE(1)/' 1) Y vs TIME            2) Y1,Y2 vs TIME      '/
      DATA CLINE(2)/' 3) X vs Y               4) NUMERIC DUMP       '/
      DATA CLINE(3)/' 5) TEPHIGRAM            6)                    '/
      DATA CLINE(4)/' 7)                      8)                    '/
!     DATA CLINE(3)/' 5) TEPHIGRAM            6) SAFIRE DUMP        '/
!     DATA CLINE(4)/' 7) SAFIRE Y vs TIME     8) SAFIRE Y1,Y2 vs T  '/
      DATA CLINE(5)/' 9)                     10)                    '/
      DATA CLINE(6)/'11) U vs V HODOGRAPH    12) AIRCRAFT TRACK     '/
!      DATA CLINE(7)/'13) MARSS DUMP          14) MARSS Y1 vs TIME   '/
!      DATA CLINE(8)/'15) MARSS Y1,Y2 vs TIME 16) MARSS X vs Y       '/
      DATA CLINE(7)/'13)                     14)                    '/
      DATA CLINE(8)/'15)                     16)                    '/
!      DATA CLINE(9)/'17) INSTRUMENTS         18) CCN                '/
      DATA CLINE(9)/'17)                     18) CCN                '/
!     DATA CLINE(10)/'19) CHEMISTRY           20) DEIMOS             '/
      DATA CLINE(10)/'19) CHEMISTRY           20)                    '/

      DATA NOPTION /20/                !Number of options
C     
9     CALL SEL(0,0,IBLACK)             !Use black
      CALL RUB                         !Clear display page
      CALL SEL(0,IBACKG,0)             !Dk green
      CALL BLK(0,0,767,509)            !Fill block with background
      CALL SEL(0,IFOREG,IBACKG)        !Cyan on dk green
      CALL SCA(13,13,0,3)              !Char cell width, height, orient, font
      CALL ALP(200,110,' MENU A - SELECT ('//DSPDEV(5:6)//')')
      IY=150
      DO I=1,(NOPTION+1)/2             !List the options
        CALL ALP(10,IY,CLINE(I)(1:LAN(CLINE(I))))
        IY=IY+30
      END DO
      CALL FOOT(' ',' ',' ',' ',' ','PRINT',' ',' ')
10    CALL PIX                         !Make sure displaying graphics
      CALL ALP(30,465,'Enter selection (default =4)...              ')
      CALL SELECT(446,465,IFUNC)       !Get user requirement
      IF(IFUNC.EQ.0) IFUNC=4
      CALL PIX                         !Make sure displaying graphics
      IF(IFUNC.EQ.99) THEN             !99 is special case to exit program
        CLOSE(10,IOSTAT=IOS)
        CALL FIN                       !Stop graphics
        CALL EXIT(99)
      END IF
      IF(IFUNC.LT.0.OR.IFUNC.GT.NOPTION.OR.(IFUNC.GE.6.AND.
     -    IFUNC.LE.10).OR.(IFUNC.GE.13.AND.IFUNC.LE.17).OR.IFUNC.EQ.20) 
     _    GOTO 10                       !Invalid input
      RETURN
      END
C*******************************************************************************
      SUBROUTINE GET_PARA(CLAB,IPARA)
C
C Subroutines used:
C
C   SELECT       H_DISPLAY.FOR
C   ggs          GGSX.OLB
C
C V1.00  01/01/89  W.D.N.JACKSON
C
      IMPLICIT  NONE
      INTEGER*4 IPARA,JJ,J,IY,I,LAN
      CHARACTER CLAB*2,CLINE(64)*48
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
C
      DATA CLINE(9) /'533) MASS MIXING RATIO  534) HUM MIXING RATIO   '/
      DATA CLINE(10)/'547) FWVS HMR (WVD)     546) LIFTING COND. LEVEL'/
      DATA CLINE(11)/'535) J/W LIQUID WATER   572) TOTAL WATER CONTENT'/ 
      DATA CLINE(12)/'537) HEIM UNCORRECTED   590) SURFACE TEMP       '/
      DATA CLINE(13)/'570) WIND SPEED         571) WIND ANGLE         '/
      DATA CLINE(14)/'610) 10M WIND SPEED     576) STATIC PRESSURE    '/ 
      DATA CLINE(15)/'575) RADAR HEIGHT       579) PRESSURE HT Kft    '/ 
      DATA CLINE(16)/'574) OZONE MIXING RATIO 545) CNC CONCENTRATION  '/ 
C
      DATA CLINE(17)/'538) UPPER CLEAR FLUX   539) UPPER RED FLUX     '/
      DATA CLINE(18)/'540) UPPER IR FLUX      541) LOWER CLEAR FLUX   '/
      DATA CLINE(19)/'542) LOWER RED FLUX     543) LOWER IR FLUX      '/
      DATA CLINE(20)/'580) SOLAR ALBEDO       581) NEAR IR ALBEDO     '/
      DATA CLINE(21)/'582) VISIBLE ALBEDO     583) LOWER VISIBLE      '/
      DATA CLINE(22)/'584) UPPER VISIBLE      585) NET IR             '/
      DATA CLINE(23)/'586) UPPER NIR/SOLAR    587) LOWER NIR/SOLAR    '/ 
      DATA CLINE(24)/'552) SOLAR ZENITH ANGLE 553) SOLAR AZIMUTH ANGLE'/
C
      DATA CLINE(25)/'548) ANGLE OF ATTACK    549) ANGLE OF SIDESLIP  '/
      DATA CLINE(26)/'517) TRUE AIR SPEED     557) INS VZ             '/
      DATA CLINE(27)/'558) INS VN             559) INS VE             '/
      DATA CLINE(28)/'560) INS ROLL           561) INS PITCH          '/
      DATA CLINE(29)/'562) INS HEADING        563) INS GROUNDSPEED    '/
      DATA CLINE(30)/'564) INS DRIFT ANGLE    565) INS PITCH RATE     '/
      DATA CLINE(31)/'566) INS YAW RATE       567) U - WIND NORTH     '/
      DATA CLINE(32)/'568) V - WIND EAST      569) W - WIND VERTICAL  '/
C
      DATA CLINE(33)/'536) RELATIVE HUMIDITY  551)                    '/
      DATA CLINE(34)/'607) GPS LATITUDE       608) GPS LONGITUDE      '/
      DATA CLINE(35)/'605) INU LATITUDE       606) INU LONGITUDE      '/
      DATA CLINE(36)/'578) PRESSURE HT  m     609) GPS ALTITUDE       '/
      DATA CLINE(37)/'554) PSAP LIN ABS COEFF 555) PSAP LOG ABS COEFF '/
      DATA CLINE(38)/'556) PSAP TRANSMITTANCE 619) CABIN PRESSURE     '/
      DATA CLINE(39)/'630) NOXY NO            631) NOXY NO2           '/
      DATA CLINE(40)/'632) NOXY NOY1          633) NOXY NOY2          '/
C
      DATA CLINE(41)/'620) NEPH PRESSURE      621) NEPH TEMPERATURE   '/
      DATA CLINE(42)/'622) NEPH BLUE SP       623) NEPH GREEN SP      '/
      DATA CLINE(43)/'624) NEPH RED SP        625) NEPH BLUE BSP      '/
      DATA CLINE(44)/'626) NEPH GREEN BSP     627) NEPH RED BSP       '/
      DATA CLINE(45)/'628) NEPH HUMIDITY      629) NEPH STATUS        '/      
      DATA CLINE(46)/'574) OZONE MIXING RATIO 588) CO MIXING RATIO    '/
      DATA CLINE(47)/'592) HCHO MIXING RATIO  612) 4PI JNO2           '/
      DATA CLINE(48)/'613) H2O2 MIXING RATIO  614) ORG H2O2 MIX RATIO '/
C
      DATA CLINE(49)/'593) ECGC SIGNAL 1      594) ECGC SIGNAL 2      '/
      DATA CLINE(50)/'595) ECGC SIGNAL 3      596) ECGC TEMP 1        '/
      DATA CLINE(51)/'597) ECGC TEMP 2        598) ECGC TEMP 3        '/
      DATA CLINE(52)/'599) ECGC PRESSURE 1    600) ECGC PRESSURE 2    '/
      DATA CLINE(53)/'601) ECGC PRESSURE 3    602) ECGC PEAK HGT 1    '/
      DATA CLINE(54)/'603) ECGC PEAK HGT 2    604) ECGC PEAK HGT 3    '/
      DATA CLINE(55)/'615) MODELLED H2O2      616) DIFF. H2O2         '/
      DATA CLINE(56)/'617) CHEM 3             618) CHEM 4             '/
C
      DATA CLINE(57)/'660) NEVZ LIQ WATER     661) NEVZ TOT WATER     '/
      DATA CLINE(58)/'657) TECO NO            658) TECO NO2           '/
      DATA CLINE(59)/'659) TECO NOX           611) TECO SO2           '/
      DATA CLINE(60)/'                                                '/
      DATA CLINE(61)/'                                                '/
      DATA CLINE(62)/'                                                '/
      DATA CLINE(63)/'                                                '/
      DATA CLINE(64)/'                                                '/
C
9     CALL SEL(0,0,IBLACK)
      CALL RUB                         !clear display page
      CALL SEL(0,IBACKG,0)             !D green
      CALL BLK(0,0,767,509)            !Fill block with background
      CALL FOOT('SELECT',' ',' ',' ',' ','PRINT',' ',' ')
      CALL ALP(200,110,' MENU B - PARAMETERS')            
      IPARA=0
      JJ=1
      DO WHILE(IPARA.EQ.0)
        J=MOD(JJ,64)
        IY=150
        CALL SEL(0,IBACKG,0)           !D green
        CALL BLK(0,150,767,509)
        CALL SEL(0,IFOREG,IBACKG)      !cyan/d green
        DO I=J,J+7
          CALL ALP(10,IY,CLINE(I)(1:LAN(CLINE(I)))) !show next 8 lines
          IY=IY+30
        END DO
10      CALL ALP(10,450,'Select '//CLAB//
     -      ' (ENTER for more paras) ...      ')
        CALL SELECT(491,450,IPARA)     !get user input
!        IF(IPARA.EQ.-8) THEN
!          CALL HELP
!          GOTO 9
!        END IF
        IF(IPARA.GT.661) GOTO 10
        IF(IPARA.LT.-1) GOTO 10
        IF(IPARA.GT.0.AND.IPARA.LT.516) GOTO 10      
        JJ=JJ+8
      END DO
      RETURN
      END
C*******************************************************************************
      SUBROUTINE GET_PARA_TEPHI(IOPT)
C
C Subroutines used:
C
C   SELECT       H_DISPLAY.FOR
C   ggs          GGSX.OLB
C
C V1.00  01/01/89  J.HARMER
C
      IMPLICIT  NONE
      INTEGER*4 IOPT,I,IY,LAN
      CHARACTER CLINE(32)*60
      INCLUDE   'DCOM_DEF.FOR'
C
C   contents of menu 2 for tephigram
C
      DATA CLINE(1) /'       DEW POINT OPTIONS                       '/
      DATA CLINE(2) /'1) AUTOMATIC HYGROMETER     -  (yellow)        '/
      DATA CLINE(3) /'2) TOTAL WATER CONTENT      -  (green)         '/
      DATA CLINE(4) /'3) FL WATER VAPOUR SENSOR   -  (green)         '/ 
      DATA CLINE(5) /'4) AUTOMATIC + TOTAL WATER  -  (yellow + green)'/
      DATA CLINE(6) /'5) AUTOMATIC + WATER VAPOUR -  (yellow + green)'/
C
9     CALL SEL(0,0,IBLACK)
      CALL RUB                         !clear display page
      CALL SEL(0,IBACKG,0)             !D green
      CALL BLK(0,0,767,509)            !Fill block with background
      CALL FOOT('SELECT',' ',' ',' ',' ','PRINT',' ',' ')
      CALL ALP(200,110,' MENU B - PARAMETERS')            
      IY=150
      DO I=1,6
        CALL ALP(30,IY,CLINE(I)(1:LAN(CLINE(I)))) !show 6 lines
        IY=IY+30
      END DO
10    CALL ALP(30,450,'Enter selection (default =1)...         ')
      CALL SELECT(446,450,IOPT)         !get user input
      IF(IOPT.EQ.0) IOPT=1
!      IF(IOPT.EQ.-8) THEN
!        CALL HELP
!        GOTO 9
!      END IF
      IF(IOPT.LT.-1) GOTO 10
      IF(IOPT.GT.5) GOTO 10      
      RETURN
      END
C*******************************************************************************
      SUBROUTINE GET_FREQ(IFREQ)
C
C Subroutines used:
C
C   FOOT         This file
C   SELECT       HOR_UTIL
C   ggs          GGSX.OLB
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
      CALL SCA(13,13,0,3)              !character cell width, height, orientation, font
      CALL ALP(200,110,' MENU C - FREQUENCY')            
      IY=150
      DO I=1,10
        CALL ALP(10,IY,CLINE(I)(1:LAN(CLINE(I)))) !list choices
        IY=IY+30
      END DO
      CALL FOOT('SELECT','PARAS',' ',' ',' ','PRINT',' ',' ')
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
      SUBROUTINE GET_FREQ_HODO(IFREQ)
C
C Subroutines used:
C
C   FOOT         This file
C   SELECT       HOR_UTIL
C   ggs          GGSX.OLB
C
      IMPLICIT  NONE
      INTEGER*4 IFREQ,I,IY,LAN
      CHARACTER CLINE(10)*60
      INCLUDE   'DCOM_DEF.FOR'
C
C Menu 3 contents
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
      CALL SCA(13,13,0,3)              !character cell width, height, orientation, font
      CALL ALP(200,110,' MENU C - FREQUENCY')            
      IY=150
      DO I=1,10
        CALL ALP(10,IY,CLINE(I)(1:LAN(CLINE(I)))) !list choices
        IY=IY+30
      END DO
      CALL FOOT('SELECT',' ',' ',' ',' ','PRINT',' ',' ')
10    CALL ALP(30,450,'Enter selection (default =1)...          ')
      CALL SELECT(446,450,IFREQ)
      IF(IFREQ.EQ.0) IFREQ=1
!      IF(IFREQ.EQ.-8) THEN
!        CALL HELP
!        GOTO 9
!      END IF
      IF(IFREQ.LT.-1.OR.IFREQ.GT.10) GOTO 10
      IF(IFREQ.EQ.10) IFREQ=20
      IF(IFREQ.EQ.9) IFREQ=16
      IF(IFREQ.EQ.8) IFREQ=12
      IF(IFREQ.EQ.7) IFREQ=8
      IF(IFREQ.EQ.6) IFREQ=6
      RETURN
      END
C*******************************************************************************
      SUBROUTINE GET_SCAL_TEPHI(IAREA)
C
C Subroutines used:
C
C   SELECT       H_DISPLAY.FOR
C   ggs          GGSX.OLB
C
C
C   contents of menu 4 for tephi zoom
C
C V1.00  01/01/89  J.HARMER
C
      IMPLICIT  NONE
      INTEGER*4 IAREA
      INCLUDE   'DCOM_DEF.FOR'

9     CALL SEL(0,0,IBLACK)
      CALL RUB                                  !clear display page
      CALL SEL(0,IFOREG,IBLACK)                 !cyan/black
      CALL SCA(13,13,0,3)                       !large letters 
      CALL ALP(200,15,' MENU D - TEPHI ZOOM')                 
      CALL HOR_TEPHI_MENU
      CALL FOOT('SELECT','PARAS','FREQ',' ',' ','PRINT',' ',' ')
      CALL SEL(0,IFOREG,IBLACK)                 !cyan/black
      CALL SCA(13,13,0,3)                       !large letters 
10    CALL ALP(30,70,'Enter selection (default =1)...          ')
      CALL SELECT(446,70,IAREA)
      IF(IAREA.EQ.0) IAREA=1
!      IF(IAREA.EQ.-8) THEN
!        CALL HELP
!        GOTO 9
!      END IF
      IF(IAREA.LT.-3.OR.IAREA.GT.5) GOTO 10 !CHANGE THIS FOR MORE OPTIONS
      RETURN
      END
C*******************************************************************************
      SUBROUTINE GET_SCAL_PLOT(CLAB,IFUNC,IPARA,IORI,IRNG)
C
C Subroutines used:
C
C   SELECT       H_DISPLAY.FOR
C   ggs          GGSX.OLB
C
C V1.00  01/01/89  J.HARMER
C
      IMPLICIT  NONE
      INTEGER*4 IFUNC,IPARA,IORI,IRNG,IX,IOPTION,IRDEF,IY,LAN,IST,I
      REAL*4    FIRSTTIC,RMINA,RMAXA1,RMAXA2,RMAXA4,RMAXA8
      CHARACTER CLINE(6)*60,COPT*1,CLAB*2,CLABEL*8
      INCLUDE   'DCOM_DEF.FOR'
C
C   contents of menu 4 for plotting scale enlargements
C
      DATA CLINE(1) 
     -           /' ENTER NEW ORIGIN  - from one of those shown below'/
      DATA CLINE(2) /' SELECT FACTOR FOR SCALE ENLARGEMENT           '/  
      DATA CLINE(3) /'    1)               -->                       '/
      DATA CLINE(4) /'    2)               -->                       '/
      DATA CLINE(5) /'    3)               -->                       '/
      DATA CLINE(6) /'    4)               -->                       '/
C
9     CALL SEL(0,0,IBLACK)
      CALL RUB                                  !clear display page
      CALL SEL(0,IBACKG,0)                      !D green
      CALL BLK(0,0,767,509)                     !Fill block with background
      CALL SEL(0,IFOREG,IBACKG)                 !cyan/d green
      CALL SCA(13,13,0,3)      !character cell width, height, orientation, font
      CALL ALP(200,110,' MENU D - PLOTTING SCALE ENLARGEMENT')            
      CALL FOOT('SELECT','PARAS','FREQ',' ',' ','PRINT',' ',' ')
      CALL ALP(10,150,CLINE(1)(1:LAN(CLINE(1)))) !print first line
c
      IF(IFUNC.GE.1.AND.IFUNC.LE.3.AND.IPARA.GE.513) THEN
!     -    .OR.(IFUNC.GE.7.AND.IFUNC.LE.9) 
!     -    .OR.(IFUNC.GE.14.AND.IFUNC.LE.16) 
!     -    .AND.(IPARA.GE.513))             THEN   !standard parameter
        CALL ALP(250,177,CPARA(IPARA-512))        !write in parameter name
        CALL ALP(400,177,CUNIT(IPARA-512))        !write in units
        CALL VEC(100,200,670,200)                 !draw x axis for example
        DO IX=100,670,IXPIX(IPARA-512)     
          CALL VEC(IX,200,IX,206)                 !draw tic marks
        END DO
        CALL SCA(10,10,0,2)                       !small letters
        CALL ALP(5,235,'OPTION = ') 
        FIRSTTIC=TICVAL(IPARA-512)
        IOPTION=1
        DO IX=40,620,IXPIX(IPARA-512)
          IY=210
          WRITE(CLABEL,11,IOSTAT=IST) FIRSTTIC   
          CALL ALP(IX,IY,CLABEL)                  !write labels
          WRITE(COPT,12,IOSTAT=IST)IOPTION
          CALL ALP(IX+50,235,COPT)                !write option nos
          FIRSTTIC=FIRSTTIC+TICINC(IPARA-512)
          IOPTION=IOPTION+1
        END DO
c
!      ELSE IF(((IFUNC.GE.7.AND.IFUNC.LE.9) 
!     -   .OR.(IFUNC.GE.14.AND.IFUNC.LE.16)) 
!     -   .AND.(IPARA.LE.100))           THEN       !saf plots
!        CALL ALP(250,177,CPARAM(IPARA))           !write in parameter name
!        CALL ALP(400,177,CUNITM(IPARA))           !write in units
!        CALL VEC(100,200,670,200)                 !draw x axis for example
!        DO IX=100,670,JXPIX(IPARA)     
!          CALL VEC(IX,200,IX,206)                 !draw tic marks
!        END DO
!        CALL SCA(10,10,0,2)                       !small letters
!        CALL ALP(5,235,'OPTION = ') 
!        FIRSTTIC=TICVALM(IPARA)
!        IOPTION=1
!        DO IX=40,620,JXPIX(IPARA)
!          IY=210
!          WRITE(CLABEL,11,IOSTAT=IST) FIRSTTIC   
!          CALL ALP(IX,IY,CLABEL)                  !write labels
!          WRITE(COPT,12,IOSTAT=IST)IOPTION
!          CALL ALP(IX+50,235,COPT)                !write option nos
!          FIRSTTIC=FIRSTTIC+TICINCM(IPARA)
!          IOPTION=IOPTION+1
!        END DO
!      ELSE IF((IFUNC.GE.14.AND.IFUNC.LE.16) 
!     -   .AND.(IPARA.GE.101.AND.IPARA.LE.160))  THEN    !marss plots
!        CALL ALP(250,177,CPARAR(IPARA-100))       !write in parameter name
!        CALL ALP(400,177,CUNITR(IPARA-100))       !write in units
!        CALL VEC(100,200,670,200)                 !draw x axis for example
!        DO IX=100,670,LXPIX(IPARA-100)     
!          CALL VEC(IX,200,IX,206)                 !draw tic marks
!        END DO
!        CALL SCA(10,10,0,2)                       !small letters
!        CALL ALP(5,235,'OPTION = ') 
!        FIRSTTIC=TICVALR(IPARA-100)
!        IOPTION=1
!        DO IX=40,620,LXPIX(IPARA-100)
!          IY=210
!          WRITE(CLABEL,11,IOSTAT=IST) FIRSTTIC   
!          CALL ALP(IX,IY,CLABEL)                  !write labels
!          WRITE(COPT,12,IOSTAT=IST)IOPTION
!          CALL ALP(IX+50,235,COPT)                !write option nos
!          FIRSTTIC=FIRSTTIC+TICINCR(IPARA-100)
!          IOPTION=IOPTION+1
!        END DO
!      ELSE IF((IFUNC.GE.14.AND.IFUNC.LE.16) 
!     -   .AND.(IPARA.GE.201.AND.IPARA.LE.315))  THEN    !deimos plots
!        CALL ALP(250,177,CPARAD(IPARA-200))       !write in parameter name
!        CALL ALP(400,177,CUNITD(IPARA-200))       !write in units
!        CALL VEC(100,200,670,200)                 !draw x axis for example
!        DO IX=100,670,KXPIX(IPARA-200)     
!          CALL VEC(IX,200,IX,206)                 !draw tic marks
!        END DO
!        CALL SCA(10,10,0,2)                       !small letters
!        CALL ALP(5,235,'OPTION = ') 
!        FIRSTTIC=TICVALD(IPARA-200)
!        IOPTION=1
!        DO IX=40,620,KXPIX(IPARA-200)
!          IY=210
!          WRITE(CLABEL,11,IOSTAT=IST) FIRSTTIC   
!          CALL ALP(IX,IY,CLABEL)                  !write labels
!          WRITE(COPT,12,IOSTAT=IST)IOPTION
!          CALL ALP(IX+50,235,COPT)                !write option nos
!          FIRSTTIC=FIRSTTIC+TICINCD(IPARA-200)
!          IOPTION=IOPTION+1
!        END DO
      ENDIF
      CALL SCA(13,13,0,3)      !character cell width, height, orientation, font
10    IRDEF=1                  !default selection
      CALL ALP(10,250,
     -    'Select new '//CLAB//' origin option no (default=1) ... ')
      CALL SELECT(621,250,IORI)         !get user input
!      IF(IORI.EQ.-8) THEN
!        CALL HELP
!         GOTO 9
!      END IF
      IF(IORI.EQ.0) IORI=IRDEF       !default used to calc range
      IF(IORI.LT.0) RETURN
      IF(IORI.GT.7) GOTO 10
c Calculate choice of ranges using user specified origin
      IF(IFUNC.GE.1.AND.IFUNC.LE.3.AND.IPARA.GE.513) THEN
!      IF(((IFUNC.GE.1.AND.IFUNC.LE.3)
!     -    .OR.(IFUNC.GE.7.AND.IFUNC.LE.9) 
!     -    .OR.(IFUNC.GE.14.AND.IFUNC.LE.16))
!     -    .AND.(IPARA.GE.513))             THEN       !standard parameter
        RMINA=TICVAL(IPARA-512) + ((IORI-1)*TICINC(IPARA-512))   
     -                                                       !new origin
        RMAXA1=(330.0/IYPIX(IPARA-512)*TICINC(IPARA-512))/1.0 
     -                              + RMINA   !Max scaling factor= 1
        RMAXA2=(330.0/IYPIX(IPARA-512)*TICINC(IPARA-512))/2.0 
     -                              + RMINA   !Max Scaling factor= 2
        RMAXA4=(330.0/IYPIX(IPARA-512)*TICINC(IPARA-512))/4.0 
     -                              + RMINA   !Max Scaling factor= 4
        RMAXA8=(330.0/IYPIX(IPARA-512)*TICINC(IPARA-512))/8.0 
     -                              + RMINA   !Max Scaling factor= 8
!      ELSE IF(((IFUNC.GE.7.AND.IFUNC.LE.9) 
!     -   .OR.(IFUNC.GE.14.AND.IFUNC.LE.16))
!     -   .AND.(IPARA.LE.100))           THEN       !saf plots
!        RMINA=TICVALM(IPARA) + ((IORI-1)*TICINCM(IPARA))   !new origin
!        RMAXA1=(330.0/JYPIX(IPARA)*TICINCM(IPARA))/1.0 + RMINA !Max
!        RMAXA2=(330.0/JYPIX(IPARA)*TICINCM(IPARA))/2.0 + RMINA !Max
!        RMAXA4=(330.0/JYPIX(IPARA)*TICINCM(IPARA))/4.0 + RMINA !Max
!        RMAXA8=(330.0/JYPIX(IPARA)*TICINCM(IPARA))/8.0 + RMINA !Max
!      ELSE IF((IFUNC.GE.14.AND.IFUNC.LE.16) 
!     -   .AND.(IPARA.GE.101.AND.IPARA.LE.160))  THEN    !marss plots
!        RMINA=TICVALR(IPARA-100) + ((IORI-1)*TICINCR(IPARA-100))   
!     -                                                       !new origin
!        RMAXA1=(330.0/LYPIX(IPARA-100)*TICINCR(IPARA-100))/1.0 
!     -                              + RMINA   !Max scaling factor= 1
!        RMAXA2=(330.0/LYPIX(IPARA-100)*TICINCR(IPARA-100))/2.0 
!     -                              + RMINA   !Max Scaling factor= 2
!        RMAXA4=(330.0/LYPIX(IPARA-100)*TICINCR(IPARA-100))/4.0 
!     -                              + RMINA   !Max Scaling factor= 4
!        RMAXA8=(330.0/LYPIX(IPARA-100)*TICINCR(IPARA-100))/8.0 
!     -                              + RMINA   !Max Scaling factor= 8
!      ELSE IF((IFUNC.GE.14.AND.IFUNC.LE.16) 
!     -   .AND.(IPARA.GE.201.AND.IPARA.LE.315))  THEN    !diemos plots
!        RMINA=TICVALD(IPARA-200) + ((IORI-1)*TICINCD(IPARA-200))   
!     -                                                       !new origin
!        RMAXA1=(330.0/KYPIX(IPARA-200)*TICINCD(IPARA-200))/1.0 
!     -                              + RMINA   !Max scaling factor= 1
!        RMAXA2=(330.0/KYPIX(IPARA-200)*TICINCD(IPARA-200))/2.0 
!     -                              + RMINA   !Max Scaling factor= 2
!        RMAXA4=(330.0/KYPIX(IPARA-200)*TICINCD(IPARA-200))/4.0 
!     -                              + RMINA   !Max Scaling factor= 4
!        RMAXA8=(330.0/KYPIX(IPARA-200)*TICINCD(IPARA-200))/8.0 
!     -                              + RMINA   !Max Scaling factor= 8
      ENDIF
      WRITE(CLABEL,11,IOSTAT=IST) RMINA
      CALL ALP(10,300,CLINE(2)(1:LAN(CLINE(2))))                    
      CALL SCA(10,10,0,2)
      IY=330
      DO I=3,6
        CALL ALP(10,IY,CLINE(I)(1:LAN(CLINE(I)))) !show 4 lines
        CALL ALP(70,IY,CLABEL)            !write in the minimum
        IY=IY+30
      END DO
      WRITE(CLABEL,11,IOSTAT=IST) RMAXA1    
      CALL ALP(250,330,CLABEL)              !write in the maximums
      WRITE(CLABEL,11,IOSTAT=IST) RMAXA2
      CALL ALP(250,360,CLABEL)
      WRITE(CLABEL,11,IOSTAT=IST) RMAXA4
      CALL ALP(250,390,CLABEL)
      WRITE(CLABEL,11,IOSTAT=IST) RMAXA8
      CALL ALP(250,420,CLABEL)
      CALL SCA(13,13,0,3)      !character cell width, height, orientation, font
20    IF(IPARA.NE.574) THEN   !ozone is a special case - takes a default of 3
        CALL ALP(10,450,'Select Scaling Factor  (default=1) ... ')
      ELSE
        CALL ALP(10,450,'Select Scaling Factor  (default=3) ... ')
      END IF
      CALL SELECT(517,450,IRNG)        !get user input
      IF(IRNG.EQ.0) THEN
        IF(IPARA.EQ.574) IRNG=3        !Special ozone case
        IF(IPARA.NE.574) IRNG=1
      END IF
!      IF(IRNG.EQ.-8) THEN
!        CALL HELP
!        GOTO 9
!      END IF
      IF(IRNG.LT.0) RETURN
      IF(IRNG.GT.4) GOTO 20            !test for invalid
      IF(IRNG.EQ.4) IRNG=8           
      IF(IRNG.EQ.3) IRNG=4             !convert input to scaling factor  
   11 FORMAT(F8.2)
   12 FORMAT(I1)
      RETURN
      END
C*******************************************************************************
      SUBROUTINE GRFINI(CDSPDEV)
C
C Initializes the graphics system and also reads
C in data from the various plotting constants files.
C When the initial actions are complete menu 1 is
C displayed to get the users initial requirement.
C
C Reads the following files:
C
C     [HORACE]HOR_PLOT_CONSTS.DAT
!C     [HORACE]SAF_PLOT_CONSTS.DAT
!C     [HORACE]MARSS_PLOT_CONSTS.DAT
!C     [HORACE]DEIMOS_PLOT_CONSTS.DAT
C
C Subroutines used:
C
C   MENU         This file
C   KHANDL       This file
C   ggs          GGSX.OLB
C
C V1.00  01/01/89  W.D.N.JACKSON
C V1.01  01/10/92  W.D.N.JACKSON
C        The HOR_PLOT_CONSTS.DAT file has been reformatted with just the 
C        required max, min, and no of intervals to go on the plot.  From these
C        the values as previously entered are calculated.
C        Now handles Ethernet cards
C V1.02  04/03/96  W.D.N.JACKSON
C        Now resets VT100 terminal emulator on graphics cards
C V1.03  14/04/98  W.D.N.JACKSON
C        If driving an X terminal doesn't try to read logicals
C V1.04  15/01/99  W.D.N.JACKSON
C        TECO NOx and Nevzorov parameters added
C
      IMPLICIT  NONE
      CHARACTER CDSPDEV*6
      INTEGER*4 IMODE,IRES,ISOM,I,INCS
      REAL*4    YMIN,YMAX
      INCLUDE   'DCOM_DEF.FOR'
      INCLUDE   'HCOM_DEF.FOR'
C
C Set up the graphics system.
C
      IBLACK=0                         !Black 
      IRED=1                           !Red 
      IGREEN=2                         !Green 
      IYELLOW=3                        !Yellow 
      IBLUE=4                          !Blue 
      IMAG=5                           !Magenta   
      IFOREG=6                         !Foreground colour (cyan)
      IWHITE=7                         !White 
      IDGREY=8                         !Dark grey   
      IBACKG=9                         !Background colour (dk green)
      IORANGE=10                       !Orange 
      ILBLUE=11                        !Light blue   
      IDBLUE=12                        !Dark blue 
      IDBROWN=13                       !Brown 
      ILGREY=14                        !Light grey    
      IPURPLE=15                       !White 
!
! The following change the colour mapping to mainly Black on White display.
! Note however that corresponding changes are required to the colour
! translations in the GGS_PRINT routine, and the CCN and Chemistry routines
! don't display properly with these mappings.
!
!      IF(CDSPDEV(1:3).EQ.'XTM') THEN   !Better colours for laptops?
!        IBLACK=7
!        IBACKG=7
!        IFOREG=0
!        IYELLOW=4
!      END IF
!
      CALL INI(CDSPDEV//'_CHAN',IMODE)
      CDSPTYP='X'
      CALL SET(IRES,2)                 !Set resolution and no of pages
      CALL SOM(ISOM)                   !Select video output mode
      CALL PIX                         !Show whole display to start
      CALL SEL(0,IBLACK,IBLACK)        !Select black background
      CALL PAG(0)                      !Display page 0
      CALL RUB                         !Erase display page to black
C
C Initialise the VT100 emulator on the graphics board, in case has become
C corrupted and the graphics card has not been reset.
C
      CALL VT_OUT(' ')
C
C Get the user to enter something to get the input focus on the right window
C
      CALL VDU
      CALL VT_OUT('  ')
      CALL VT_OUT(' Click on this window to get the input focus.')
      CALL VT_OUT(' Then press Enter or CTRL/W to continue.')
      CALL VT_OUT(' ')
      INPUT=0
      DO WHILE(INPUT.EQ.0)
        CALL LIB$WAIT(0.2)
      END DO
      CALL PIX
C
C Read in constants to define the scaling and range for plotting of the
C various standard parameters.
C
      OPEN(UNIT=20,FILE='[HORACE]HOR_PLOT_CONSTS.DAT',STATUS='OLD',
     -    READONLY)
      READ(20,1000)
      DO I=1,149
        READ(20,1100) CPARA(I),CUNIT(I),YMIN,YMAX,INCS
        IF(INCS.GT.0) THEN
          TICVAL(I)=YMIN
          IYPIX(I)=330/INCS
          TICINC(I)=(YMAX-YMIN)/INCS
          YFACT(I)=IYPIX(I)/TICINC(I)
          XFACT(I)=(YFACT(I)*570.0)/330.0
          IXPIX(I)=(JZEXT(IYPIX(I))*570.0)/330.0
          IYOFF(I)=480+NINT((TICVAL(I)/TICINC(I))*REAL(IYPIX(I)))
          IXOFF(I)=100-NINT((TICVAL(I)/TICINC(I))*REAL(IXPIX(I)))
        END IF
      END DO
      CLOSE(UNIT=20)
!C
!C Read in constants to define the scaling and range for plotting of the
!C various SAFIRE parameters.
!C
!      OPEN(UNIT=21,FILE='[HORACE]SAF_PLOT_CONSTS.DAT',STATUS='OLD',
!     -    READONLY)
!      READ(21,1000)
!      DO J=1,60
!        READ(21,1100) CPARAM(J),CUNITM(J),YMIN,YMAX,INCS
!        IF(INCS.GT.0) THEN
!          TICVALM(J)=YMIN
!          JYPIX(J)=330/INCS
!          TICINCM(J)=(YMAX-YMIN)/INCS
!          YFACTM(J)=JYPIX(J)/TICINCM(J)
!          XFACTM(J)=(YFACTM(J)*570.0)/330.0
!          JXPIX(J)=(JZEXT(JYPIX(J))*570.0)/330.0
!          JYOFF(J)=480+NINT((TICVALM(J)/TICINCM(J))*REAL(JYPIX(J)))
!          JXOFF(J)=100-NINT((TICVALM(J)/TICINCM(J))*REAL(JXPIX(J)))
!        END IF
!      END DO
!      CLOSE(UNIT=21)
!C
!C CODE FOR OLD METHOD OF READING IN OFFSETS
!C        READ(21,1200) ((CPARAM(J),CUNITM(J),TICVALM(J),TICINCM(J),
!C     -    YFACTM(J),XFACTM(J),JYOFF(J),JYPIX(J),JXOFF(J),JXPIX(J)),
!C     -    J=1,60)                         
!C
!C Read in constants to define the scaling and range for plotting of the
!C various MARSS parameters.
!C
!      OPEN(UNIT=22,FILE='[HORACE]MARSS_PLOT_CONSTS.DAT',STATUS='OLD',
!     -    READONLY)
!      READ(22,1300) ((CPARAR(L),CUNITR(L),TICVALR(L),TICINCR(L),
!     -    YFACTR(L),XFACTR(L),LYOFF(L),LYPIX(L),LXOFF(L),LXPIX(L)),
!     -    L=1,50)
!      CLOSE(UNIT=22)
!C
!C Read in constants to define the scaling and range for plotting of the
!C various DEIMOS parameters.
!C
!      OPEN(UNIT=22,FILE='[HORACE]DEIMOS_PLOT_CONSTS.DAT',STATUS='OLD',
!     -    READONLY)
!      READ(22,1400,IOSTAT=I)
!     -    ((CPARAD(L),CUNITD(L),TICVALD(L),TICINCD(L),
!     -    YFACTD(L),XFACTD(L),KYOFF(L),KYPIX(L),KXOFF(L),KXPIX(L)),
!     -    L=1,115)
!      CLOSE(UNIT=22)
C
      RETURN
C
10    FORMAT(X,A,4Z6.4)
 1000 FORMAT(////)
 1100 FORMAT(9X,A8,1X,A7,2F10.2,I6)
!C 1200 FORMAT(/////91(4X,A8,1X,A7,4F8.2,4I5/))
! 1300 FORMAT(/////50(4X,A8,1X,A7,4F8.2,4I5/))
! 1400 FORMAT(/////115(4X,A8,1X,A7,4F8.2,4I5/))
      END
C*******************************************************************************
      SUBROUTINE VIDPNT
C
C Handles all print requests:
C
C If a text page is being viewed, simply takes a copy of the text page buffer
C and sends it to the text printer queue.
C
C If a graphics page is being viewed on an X terminal, uploads the X-term
C buffer, converts it to postscript, and sends it to the postscript printer
C queue.
C
C V2.01  02/02/02  W.D.N.JACKSON
C
      IMPLICIT  NONE
      INTEGER*4 IVDU,ICNT,IOS
      CHARACTER CCNT*3
      INCLUDE   'DCOM_DEF.FOR'
      COMMON    /GGSCOM/ IVDU
      DATA      ICNT /0/
C
      IF(IVDU.EQ.1) THEN               !Displaying text page
        CALL VT_PAGE('PRINT')
      ELSE                             !Displaying graphics page
        WRITE(CCNT,'(I3.3)',IOSTAT=IOS) ICNT
        ICNT=ICNT+1
        CALL GGS_PRINT('H_DISPLAY_'//DSPDEV(5:6)//'_'//CCNT//'.PS;') !Print X
      END IF
      RETURN
      END
C*******************************************************************************
      SUBROUTINE VIDRST
C
C Special request to PV-Wave, normally triggered by CTRL/W, to refresh screen.
C
C V2.00  02/02/02  W.D.N.JACKSON
C
      IMPLICIT  NONE
      INCLUDE  'DCOM_DEF.FOR'
      INCLUDE  '($SYSSRVNAM)'
C
      IF(CDSPTYP.EQ.'X') CALL PIX
      RETURN
      END
C*******************************************************************************
      SUBROUTINE HEAD_SHELL
C
C Writes the background shell for the standard header used on most HORACE
C plots.  This should only need to be written once; HEAD_UPDATE is used for
C updating the information.  Note that all writing is to the current access
C page and only the top of the display is affected in any way.
C
C Subroutines: ggs           GGSX.OLB
C
      IMPLICIT  NONE
      INTEGER*4 IX,I,IS,LAN
      CHARACTER CHEAD(8,2)*7
      INCLUDE   'DCOM_DEF.FOR'
      INCLUDE   'HCOM_DEF.FOR'
      DATA CHEAD /'HDG','SPR','PHGT','TAS','TAT','DEW','WIND',' ',
     -            'degT','mb','kft','knots','C','C','deg m/s',' '/
C
C Draw top box and banner box
C
      CALL SEL(0,IBACKG,0)             !dk green
      CALL BLK(15,0,751,101)           !draw top block
      CALL SEL(0,IFOREG,-1)            !cyan additive
      CALL TOP_LINE('I')               !Show top line all blanks
C
C Write captions on each box.  Captions are then intensified by re-writing
C one pixel down.
C
      CALL SCA(13,13,0,3)
      DO I=1,8
        IS=16+(I-1)*92+(92-LAN(CHEAD(I,1))*13)/2
        CALL ALP(IS,32,CHEAD(I,1))     !write parameter headings 
      END DO
      CALL SCA(13,13,0,2)              !set to small characters
      DO I=1,8
        IS=16+(I-1)*92+(92-LAN(CHEAD(I,2))*13)/2
        CALL ALP(IS,56,CHEAD(I,2))     !write measurement units
      END DO
C
C Draw  horizontal lines
C
      CALL VEC(15,28,751,28)
      CALL VEC(15,101,751,101)
C
C Draw the vertical separation lines
C
      DO IX=15,751,92
        CALL VEC(IX,28,IX,101)         !draw vertical separator
      END DO
      RETURN
      END
C*******************************************************************************
      SUBROUTINE HEAD_UPDATE
C
C Updates the information in the standard header used on most HORACE displays.
C HEAD_SHELL should be used to write the initial framework for the display.
C Note that this routine writes to the current access page.
C
C Subroutines used:
C
C  GGSX
C
      IMPLICIT  NONE
      INTEGER*4 IS,IWA,IWS,IOS,I,IL,LAN,ILSTCNT(8),IP
      CHARACTER CLINE1(7)*7
      INCLUDE   'DCOM_DEF.FOR'
      INCLUDE   'HCOM_DEF.FOR'
      DATA      ILSTCNT /8*0/
C
C Update the very top line
C
      CALL TOP_LINE('U')
C
C Generate the formatted values into lines using internal write statements.
C
      IS=STATUS(2)
      IWA=NINT(RIWA(IS))               !convert winds to integer
      IWS=NINT(RIWS(IS))
      WRITE(CLINE1(1),'(I3)',IOSTAT=IOS) NINT(RIHDG(IS))
      WRITE(CLINE1(2),'(I4)',IOSTAT=IOS) NINT(RSPR(IS))
      WRITE(CLINE1(3),'(F5.1)',IOSTAT=IOS) RPHGTF(IS)
      WRITE(CLINE1(4),'(I3)',IOSTAT=IOS) NINT(RTAS(IS))
      WRITE(CLINE1(5),'(F5.1)',IOSTAT=IOS) RTATDC(IS)
      WRITE(CLINE1(6),'(F5.1)',IOSTAT=IOS) RDEW(IS)
      WRITE(CLINE1(7),'(I4.2,A,I2)',IOSTAT=IOS) IWA,'/',IWS
C
C Write data to the screen.  The lines are written displaced one pixel
C to the right to thicken them.
C
      CALL SEL(0,IFOREG,IBACKG)        !cyan/dark green
      CALL SCA(13,13,0,3)              !7*9 double
      DO I=1,7
        IF(CLINE1(I).NE.' ') THEN
          DO WHILE(CLINE1(I)(1:1).EQ.' ')
            CLINE1(I)(1:)=CLINE1(I)(2:)
          END DO
        END IF
        IP=16+(I-1)*92
        IL=LAN(CLINE1(I))
        IF(IL.LT.ILSTCNT(I)) CALL ALP(IP,71,'       ') !Overwrite previous with spaces
        ILSTCNT(I)=IL
        IS=16+(I-1)*92+(92-IL*13)/2
        CALL ALP(IS,71,CLINE1(I)(1:IL)) !write data line
      END DO
C
C Put in the special condition flags for the General Eastern in red.
C
      IF(IDP(IS).EQ.1) THEN            !hygrometer control condition
        CALL SEL(0,IRED,-1)            !red additive
        CALL SCA(13,13,0,2)            !small letters
        CALL ALP(473,80,'C')
      END IF
C
      RETURN
      END
C*******************************************************************************
      SUBROUTINE FOOT(C1,C2,C3,C4,C5,C6,C7,C8)
C
C Draws the standard bottom boxes on the display indicating the immediate key 
C options.  Also put the options in the boxes.  Note that C1 to C8 must be
C not greater than six characters when passed.
C
C Subroutines used:
C
C   GGSX
C
      IMPLICIT  NONE
      INTEGER*4 I,IX,IL,LAN,IM
!      INTEGER*4 STR$POSITION
      CHARACTER*(*) C1,C2,C3,C4,C5,C6,C7,C8
      CHARACTER CKEY*8,COPT(8)*6
      INCLUDE   'DCOM_DEF.FOR'
      DATA CKEY /'ABCDEFGH'/
      
      COPT(1)=C1
      COPT(2)=C2
      COPT(3)=C3
      COPT(4)=C4
      COPT(5)=C5
      COPT(6)=C6
      COPT(7)=C7
      COPT(8)=C8
!      IF(CDSPTYP.EQ.'X') THEN
!        I=STR$POSITION(C7,'VIDEO')
!        IF(I.NE.0) COPT(7)=' '         !No video switching on X terms
!        I=STR$POSITION(C8,'HELP')
!        IF(I.NE.0) COPT(8)=' '         !No HELP on X terms
!      END IF
      DO I=1,8
        IF(COPT(I).NE.' ') THEN
          DO WHILE(COPT(I)(1:1).EQ.' ')
            COPT(I)(1:)=COPT(I)(2:)
          END DO
        END IF
      END DO
      CALL SCA(13,13,0,3)              !Large letters
      CALL SEL(0,IBACKG,0)             !dk green
      CALL BLK(15,510,751,573)         !fill in box
      CALL SEL(0,IFOREG,-1)            !cyan additive
      I=1
      DO IX=16,660,92
        IL=LAN(COPT(I))
        IM=IX+(92-IL*13)/2
        CALL ALP(IX+39,516,CKEY(I:I))  !write key names
        IF(COPT(I).NE.' ') CALL ALP(IM,543,COPT(I)(1:IL)) !write options
        I=I+1
      END DO
      CALL VEC(15,510,751,510)       !top line to box
      CALL VEC(15,573,751,573)       !bottom line to box
      DO IX=15,751,92                  !draw vertical lines
        CALL VEC(IX,510,IX,573)
      END DO
      CALL SEL(0,IFOREG,IBACKG)        !cyan/green
      RETURN
      END
C*******************************************************************************
!      SUBROUTINE HELP
!C
!C Displays the video switching sequences for each display, as recorded in 
!C the H_VIDEO.LST file, when the help key is pressed.
!C Help displays the text as long as the user requires, and then
!C returns to doing whatever was being done when the help key was pressed.
!C
!C Subroutines used:
!C
!C   SELECT
!C   GGSX
!C
!      IMPLICIT  NONE
!      CHARACTER CLINE*80
!      INTEGER*4 LAN,IOS,IY,I
!      INCLUDE   'DCOM_DEF.FOR'
!C        
!      IF(CDSPTYP.EQ.'X') RETURN        !No HELP on X terms
!      CALL PIX                         !whole screen
!      CALL SEL(0,IBLACK,IBLACK)        !clearscreen
!      CALL RUB
!      CALL SEL(0,IFOREG,IBLACK)        !cyan/black
!      CALL SCA(13,13,0,3)
!C
!      OPEN(20,FILE='H_VIDEO.LST',STATUS='OLD',READONLY,IOSTAT=IOS)
!      READ(20,'(A)',IOSTAT=IOS) CLINE
!      IY=5
!      DO WHILE(IOS.EQ.0)
!        CALL ALP(10,IY,CLINE(1:LAN(CLINE)))
!        IY=IY+30
!        READ(20,'(A)',IOSTAT=IOS) CLINE
!      END DO
!      CLOSE(20,IOSTAT=IOS)
!C
!10    CALL ALP(10,480,'Press ENTER to continue...    ')
!      CALL SELECT(361,480,I)
!      IF(I.NE.0) GOTO 10
!      RETURN
!      END
C*******************************************************************************
      INTEGER*4 FUNCTION LAN(CSTRING)
C
C Function returns the length of a string not including trailing blanks. If 
C the string is all blanks, or is of zero length, the function returns a value
C of 1.
C
      IMPLICIT  NONE
      INTEGER*4 N,LEN
      CHARACTER CSTRING*(*)

      N=LEN(CSTRING)                   !Get string length
      DO WHILE(N.GT.0.AND.CSTRING(N:N).EQ.' ') !Step back over spaces
        N=N-1
      END DO
      IF(N.EQ.0) N=1                   !Special case
      LAN=N
      RETURN
      END
C*******************************************************************************
      SUBROUTINE DUMP_BACKG(CHEAD)
C
C Draws the outline for various dump displays, all of which follow a 6 by 8
C matrix, with a header line, with each box able to hold 7 characters.
C
      IMPLICIT  NONE
      INTEGER*4 I,IY,J,IL,IS,IX,LAN
      CHARACTER CHEAD(12)*56,CTXT*7
      INCLUDE  'DCOM_DEF.FOR'
C
      CALL SEL(0,IBLACK,IBLACK)        !black
      CALL RUB(0,0,767,573)            !clear access page
      CALL TOP_LINE('I')               !show blank line
C
C Fill written area with background
C
      CALL SEL(0,IBACKG,IBLACK)        !dk green
      CALL BLK(15,0,751,460)           !draw top block
C
C Draw 7 horizontal lines
C
      CALL SEL(0,IFOREG,-1)            !cyan additive
      DO IY=28,460,72
        CALL VEC(15,IY,751,IY)
      END DO
C
C Draw 9 vertical lines
C
      DO IX=15,751,92
        CALL VEC(IX,28,IX,461)         !draw vertical separator
      END DO
C
C Write captions in each box.  Captions are archived on the graphics
C board and then intensified by re-writing one pixel down.
C
      CALL SCA(13,13,0,3)              !set 7x9 double
      IY=32
      DO I=1,11,2
        DO J=1,8
          CTXT=CHEAD(I)((J-1)*7+1:(J-1)*7+7)
          IF(CTXT.NE.' ') THEN
            DO WHILE(CTXT(1:1).EQ.' ')
              CTXT(1:)=CTXT(2:)
            END DO
          END IF
          IL=LAN(CTXT)
          IS=16+(J-1)*92+(92-IL*13)/2
          CALL ALP(IS,IY,CTXT(1:IL))   !write parameter headings 
        END DO
        IY=IY+72
      END DO
      CALL SCA(13,13,0,2)              !set to small characters
      IY=32+24
      DO I=2,12,2
        DO J=1,8
          CTXT=CHEAD(I)((J-1)*7+1:(J-1)*7+7)
          IF(CTXT.NE.' ') THEN
            DO WHILE(CTXT(1:1).EQ.' ')
              CTXT(1:)=CTXT(2:)
            END DO
          END IF
          IL=LAN(CTXT)
          IS=16+(J-1)*92+(92-IL*13)/2
          CALL ALP(IS,IY,CTXT(1:IL))   !write parameter headings 
        END DO
        IY=IY+72
      END DO
C
      RETURN
      END
C*******************************************************************************
      SUBROUTINE DUMP_UPDATE(CLINE)
C
C Updates a dump display by updating the contents of the 48 boxes with the
C contents of the 6 lines passed in CLINE.
C
      IMPLICIT  NONE
      INTEGER*4 I,IY,J,LAN,IS,IL,IP,ILSTCNT(8,6)
      CHARACTER CLINE(6)*56,CTXT*7
      INCLUDE  'DCOM_DEF.FOR'
      DATA     ILSTCNT /48*6/
C
C Update the very top line
C
      CALL TOP_LINE('U')
C
C Write data to the screen.  The lines are written displaced one pixel
C to the right to thicken them.
C
      CALL SEL(0,IFOREG,IBACKG)        !cyan/dark green
      CALL SCA(13,13,0,3)              !7*9 double
      IY=71
      DO J=1,6
        DO I=1,8
          CTXT=CLINE(J)((I-1)*7+1:(I-1)*7+7)
          IF(CTXT.NE.' ') THEN
            DO WHILE(CTXT(1:1).EQ.' ')
              CTXT(1:)=CTXT(2:)
            END DO
          END IF
          IP=16+(I-1)*92
          IL=LAN(CTXT)
          IF(IL.LT.ILSTCNT(I,J)) CALL ALP(IP,IY,'       ') !Overwrite previous with spaces
          ILSTCNT(I,J)=IL
          IS=16+(I-1)*92+(92-IL*13)/2
          CALL ALP(IS,IY,CTXT(1:IL))   !write data
        END DO
        IY=IY+72
      END DO
C
      RETURN
      END
C*******************************************************************************
      SUBROUTINE TOP_LINE(CMODE)
C
C Updates the top line on the screen
C
      IMPLICIT  NONE
      INTEGER*4 IS,IOS,I
      CHARACTER CSTR*56,CLSTSTR*56,CMODE*1,CEVMTIM*10,CTIM*6
      INCLUDE   'DCOM_DEF.FOR'
      INCLUDE   'HCOM_DEF.FOR'
      DATA      CLSTSTR /' '/

      IF(CMODE.EQ.'I') THEN
        CLSTSTR=' '
        RETURN
      END IF
      WRITE(CTIM,'(2(Z3.3))',IOSTAT=IOS) (STATUS(I),I=14,15) !Last EVM time
      CEVMTIM='('//CTIM(1:2)//':'//CTIM(3:4)//':'//CTIM(5:6)//')'
      IS=STATUS(2)
      IF(STATUS(34)) THEN              !If latest derivations are valid
        WRITE(CSTR,10,IOSTAT=IOS) CFNUM,CFDAT,JHRS(IS),JMIN(IS),
     -      JSEC(IS),JEVM(IS),CEVMTIM,RGLAT(IS),RGLNG(IS),DSPDEV(5:6)
      ELSE
        WRITE(CSTR,10,IOSTAT=IOS) CFNUM,CFDAT,0,0,0,0,'          ',
     -      0.,0.,DSPDEV(5:6)
      END IF
      CALL SCA(13,13,0,3)              !7*9 double
      DO I=1,56                        !Write changed chars only so no blinking
        IF(CSTR(I:I).NE.CLSTSTR(I:I)) THEN
          CALL SEL(0,IFOREG,IBACKG)    !cyan/dark green
          CALL ALP(16+13*(I-1),0,CSTR(I:I)) !write header line
        END IF
      END DO
      CLSTSTR=CSTR
      RETURN
10    FORMAT(A4,X,A9,X,2(I2.2,':'),I2.2,X,I3.3,X,A,X,F6.2,X,F7.2,X,A)
      END
