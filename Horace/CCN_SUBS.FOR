C V1.01  28/06/94  W.D.N.JACKSON
C        Redundant variables removed
C V1.02  06/09/99  W.D.N.JACKSON
C        Now indicates what it being plotted to UPDT_STATUS
C-------------------------------------------------------------------------------
      SUBROUTINE CCN_MENU
C
C ROUTINE  CCN_MENU
C
C Displays CCN menu on Horace display and calls relevant routines
C
C Subroutines used:
C
C   SELECT       H_DISPLAY_xx.FOR
C   ggs          GGS.OLB
C
C 30-MAY-91  N.R.JERRAM
C-------------------------------------------------------------------------------
      IMPLICIT  NONE
      INTEGER*4 INP,IRETACT,IY,I
      LOGICAL*4 OCYCLE
      CHARACTER CCNOPT(6)*40
      INCLUDE   'DCOM_DEF.FOR'
      DATA CCNOPT(1) /' 1) NUMERIC DUMP OF STATIC SPECTRA'/
      DATA CCNOPT(2) /' 2) PLOT OF STATIC SPECTRA        '/
      DATA CCNOPT(3) /' 3) PLOT OF DYNAMIC SPECTRA       '/
      DATA CCNOPT(4) /' 4) PLOT OF LAST SAMPLE           '/
C
C Set up screen:
C
      OCYCLE=.TRUE.
      DO WHILE(OCYCLE)
        CALL SEL(0,0,IBLACK)
        CALL RUB                       !clear display page
        CALL SEL(0,IBACKG,0)           !D green
        CALL BLK(9,102,737,509)        !Fill block with background
        CALL SEL(0,IFOREG,IBACKG)      !cyan/d green
        CALL SCA(13,13,0,3)            !character cell width, height, orientation, font
        CALL ALP(190,110,' MENU B - CCN OPTIONS')
C
C Write out CCN menu options:
C
        IY=150
        DO I=1,4
          CALL ALP(10, IY, CCNOPT(I))
          IY=IY+30
        END DO
C
C Bottom menu options:
C
        CALL FOOT('SELECT',' ',' ',' ',' ','PRINT',' ',' ')
C
C Enter selection:
C
        INP=-10
        DO WHILE(INP.LT.-1.OR.INP.GT.4)
          CALL SEL(0,IFOREG,IBACKG)                
          CALL ALP(30,450,
     -        'Enter selection (default =1)...              ')
          CALL SELECT(490,450,INP)
          IF(INP.EQ.0) INP=1           !Default value
        END DO
        IF(INP.EQ.-1) RETURN
        IF(INP.EQ.1) CALL CCN_DUMP(IRETACT)
        IF(INP.EQ.2) CALL CCN_PLOT_OPTION(IRETACT,1)
        IF(INP.EQ.3) CALL CCN_PLOT_OPTION(IRETACT,2)
        IF(INP.EQ.4) CALL CCN_PLOT_LAST(IRETACT)
        IF(IRETACT.EQ.1) THEN          !Finished with CCN menu?
          OCYCLE=.FALSE.
        ELSE
          OCYCLE=.TRUE.
        END IF
      END DO                           !While OCYCLE
      RETURN
      END
C-------------------------------------------------------------------------------
      SUBROUTINE CCN_DUMP(IKEY)
C
C ROUTINE  CCN_DUMP
C
C Displays CCN data in numeric form accessed from CCNCOM
C
C Subroutines used:
C
C   FOOT         HOR_SUBS.FOR
C   SELECT       H_DISPLAY_xx.FOR
C   HEAD_SHELL   HOR_SUBS.FOR
C   HEAD_UPDATE  HOR_SUBS.FOR
C   ggs          GGS.OLB
C
C 4-JUNE-91  N.R.JERRAM
C
      IMPLICIT  NONE
      INTEGER*4 IKEY,IS
      LOGICAL*4 OPT3,OPT4,ODISPLAY,OFIRST,OCLEAR
      INTEGER*4 ICOUNT,ILIST(3)
      CHARACTER CFOOT3*6,CFOOT4*6,CF3*6,CF4*6
C       
      ODISPLAY=.TRUE.
      CF3='......'
      CF4='......'
      OFIRST=.TRUE.
      OCLEAR=.FALSE.
C
C Output data:
C
      IS=0
      ILIST(1)=3
      ILIST(2)=18
      ILIST(3)=1
      CALL UPDT_STATUS(ILIST)
      DO WHILE(ODISPLAY)
        CALL CCN_GET_SAMPLE(IS,OFIRST,ICOUNT,OCLEAR)
        OFIRST=.FALSE.
        OCLEAR=.FALSE.
C
C Set up options:
C
        IF(ICOUNT.LT.2) THEN
          CFOOT3='      '
          CFOOT4='      '
        ELSE 
          IF(IS.LT.ICOUNT) THEN
           CFOOT4=' NEXT '
           OPT4=.TRUE.
          ELSE
           CFOOT4='      '
           OPT4=.FALSE.
          END IF
          IF(IS.GT.1) THEN
            CFOOT3=' PREV '
            OPT3=.TRUE.
          ELSE
            CFOOT3='      '
            OPT3=.FALSE.
          END IF
        END IF
        CALL SCA(13,13,0,3)
        CALL HEAD_UPDATE
C
C Update foot menu:
C
        IF(CF3.NE.CFOOT3.OR.CF4.NE.CFOOT4) THEN
          CALL FOOT('SELECT','CCNMEN',CFOOT3 ,CFOOT4 ,' ','PRINT',
     -        ' ',' ')
          CF3=CFOOT3
          CF4=CFOOT4
        END IF
C
C User option:
C
        CALL WAIT_KD(IKEY)
        IF(IKEY.EQ.3.AND.OPT3) THEN
          IS=IS-1
          OCLEAR=.TRUE.
        END IF
        IF(IKEY.EQ.4.AND.OPT4) THEN
          IS=IS+1
          OCLEAR=.TRUE.
        END IF
        IF(IKEY.EQ.2.OR.IKEY.EQ.1) ODISPLAY=.FALSE.
      END DO
      RETURN
      END
C-------------------------------------------------------------------------------
      SUBROUTINE CCN_GET_SAMPLE(IS,OFIRST,ICNT,OCLEAR)
C
C Called from CCN_DUMP in this file.
C
C Inputs: IS       : Spectrum number (1-5)
C         OFIRST   : TRUE when called initially. Forces output even if no
C                    changes have taken place.
C Outputs:ICOUNT   : Number of spectra available
C
      IMPLICIT  NONE
      INTEGER*4 IS,ICNT,ICOUNT,ILINES,I,ICCN_REF,ICCN_PEAK
      LOGICAL*4 ONEW,OFIRST,OCLEAR
      CHARACTER CLIN3(10,10)*7
      CHARACTER CLIN1*30
      INTEGER*4 IOS /-1/
      STRUCTURE /AIR_SAMPLE/
        REAL*4 CCN_TI(10),CCN_TT(10),CCN_T1(10),CCN_T2(10),
     -      CCN_T3(10),CCN_T4(10),CCN_REF(10),
     -      CCN_PEAK(10),CCN_SSAT_ST(10),CCN_COUNT_ST(10),
     -      CCN_SSAT_DY(300),CCN_DET_DY(300)
        INTEGER*4 CCN_TIME 
        INTEGER*4 IDTEMPS
        INTEGER*4 ISTEP
      END STRUCTURE
      RECORD /AIR_SAMPLE/ OUTSAMPLE(5)
      RECORD /AIR_SAMPLE/ TST_SAMPLE
      COMMON /CCNCOM/ OUTSAMPLE,ICOUNT
      VOLATILE /CCNCOM/
C
C If IS=0, output the latest sample:
C
      ICNT=ICOUNT
      IF(IS.EQ.0) THEN
        IS=ICOUNT
        IF(ICOUNT.EQ.0) THEN
          CLIN1= '***** NO DATA AVAILABLE ***** '
          ILINES=0
          ONEW=.FALSE.
        END IF
      END IF
C
C Has the data changed?

      IF(IS.GT.0) THEN
        CALL CCN_DIFFERENCE(ONEW,OUTSAMPLE(IS),TST_SAMPLE)
C
C Plot rotation could have caused a change, so reduce IS to 
C keep the same plot output: (only if IS is the same as last call!)
C
        IF(OUTSAMPLE(IS).CCN_TIME.NE.TST_SAMPLE.CCN_TIME .AND.
     -      IOS.EQ.IS) THEN
          IF(IS.GT.1) THEN
            IS=IS-1                    !Previous plot
          ELSE                         ! if available, else..
            OCLEAR=.TRUE.              !clear data off screen
          END IF
        END IF
        IOS=IS
      END IF
C
C Put data into character strings for output:
C
      IF(ONEW) THEN
        TST_SAMPLE=OUTSAMPLE(IS)
        WRITE(CLIN1,8) OUTSAMPLE(IS).CCN_TIME
        DO I=1,OUTSAMPLE(IS).IDTEMPS
          ICCN_REF=OUTSAMPLE(IS).CCN_REF(I)
          ICCN_PEAK= OUTSAMPLE(IS).CCN_PEAK(I)
          WRITE(CLIN3(I,1),'(F5.2)') OUTSAMPLE(IS).CCN_TI(I)
          WRITE(CLIN3(I,2),'(F5.2)') OUTSAMPLE(IS).CCN_TT(I)
          WRITE(CLIN3(I,3),'(F5.2)') OUTSAMPLE(IS).CCN_T1(I)
          WRITE(CLIN3(I,4),'(F5.2)') OUTSAMPLE(IS).CCN_T2(I)
          WRITE(CLIN3(I,5),'(F5.2)') OUTSAMPLE(IS).CCN_T3(I)
          WRITE(CLIN3(I,6),'(F5.2)') OUTSAMPLE(IS).CCN_T4(I)
          WRITE(CLIN3(I,7),'(I4)') ICCN_REF
          WRITE(CLIN3(I,8),'(I4)') ICCN_PEAK
          WRITE(CLIN3(I,9),'(F6.4)') OUTSAMPLE(IS).CCN_SSAT_ST(I)
          WRITE(CLIN3(I,10),'(F7.1)') OUTSAMPLE(IS).CCN_COUNT_ST(I)
        END DO
        ILINES=OUTSAMPLE(IS).IDTEMPS
      END IF
      IF(OFIRST.OR.ONEW) CALL CCN_DUMP_OUTPUT(OFIRST,OCLEAR,CLIN1,CLIN3,
     -    ILINES)
      RETURN
    8 FORMAT('STATIC SPECTRUM AT TIME ',I6.6)
      END
C-------------------------------------------------------------------------------
      SUBROUTINE CCN_DUMP_OUTPUT(OFIRST,OCLEAR,CLIN1,CLIN3,ILINES)
C
C Routine to output the numeric data to screen.
C The data is held in the variables CLIN1, CLIN3.

      IMPLICIT  NONE
      INTEGER*4 ILINES,I,J
      CHARACTER CLIN3(10,10)*7
      CHARACTER CLIN1*30
      LOGICAL*4 OFIRST,OCLEAR
      INTEGER*4 IX(10),JX(10)
      INCLUDE   'DCOM_DEF.FOR'
      DATA IX /145,205,265,325,385,445,495,545,615,720/
      DATA JX / 90,150,210,270,330,390,450,500,550,620/
C
C Clear old data if required:
C
      IF(OCLEAR) THEN
        CALL SEL(0,IBLACK,IWHITE)
        CALL BLK(61,161,719,209)
        DO I=1,10
          CALL BLK(JX(I),231,IX(I)-1,459)
        END DO
      END IF 
C
C Set up screen:
C
      IF(OFIRST) THEN
        CALL SEL(0,0,IBLACK)
        CALL RUB
        CALL SEL(0,IBACKG,0)           !Dark green
        CALL BLK(9,102,737,509)        !Fill block with background
        CALL HEAD_SHELL
C
C Draw table:
C
        CALL SEL(0,IBLACK,IWHITE)      !Draw 2 black blocks
        CALL BLK(60,160,720,210)
        CALL BLK(60,230,720,460)
        CALL SEL(0,IWHITE,IBACKG)      !Draw white lines
        CALL BOXA(60,160,720,460)
        CALL VEC(60,210,720,210)
        CALL VEC(60,230,720,230)
        DO I=1,9
          CALL VEC(IX(I),210,IX(I),460)
        END DO
        CALL SEL(0,IWHITE,IBACKG)      !Draw labels
        CALL SCA(6,8,0,0)
        CALL ALP(100,215,'TI') 
        CALL ALP(170,215,'TT') 
        CALL ALP(230,215,'T1') 
        CALL ALP(290,215,'T2') 
        CALL ALP(350,215,'T3') 
        CALL ALP(410,215,'T4') 
        CALL ALP(460,215,'REF') 
        CALL ALP(510,215,'PEAK') 
        CALL SEL(0,IYELLOW,IBACKG)
        CALL ALP(560,215,'SSAT %') 
        CALL ALP(630,215,'CCN COUNT') 
      END IF
C
C Insert data:
C
      CALL SCA(10,10,0,3)              !Heading
      CALL SEL(0,IWHITE,IBLACK)
      CALL ALP(240,180,CLIN1)
      IF(ILINES.GT.0) THEN
        DO I=1,ILINES
          DO J=1,10
            CALL SEL(0,IWHITE,-1)
            IF(J.EQ.9.OR.J.EQ.10)CALL SEL(0,IYELLOW,-1)
            CALL ALP(JX(J),220+(I*20),CLIN3(I,J))
          END DO
        END DO
      END IF
      CALL SEL(0,IWHITE,IBLACK)
      DO I=1,9
        CALL VEC(IX(I),210,IX(I),460)
      END DO
      RETURN
      END
C-------------------------------------------------------------------------------
      SUBROUTINE CCN_PLOT_OPTION(IRETACT,IMODE)
C
C INPUT: IMODE: Static or dynamic. IMODE=1 for static, 2 for dynamic.
C
C This routine is responsible for all the user interaction during plotting
C and the calling of any plotting routines.
 
      IMPLICIT  NONE
      INTEGER*4 IRETACT,IMODE,II,ICOUNT,ISELECT,IPLOT,ILASTPLOT,IRET
      LOGICAL*4 OHOLD,OSINGLE,ODISPLAY,OFIRST
      CHARACTER CFOOT(5)*6
      INTEGER*4 OPT(5),ILIST(3)
      STRUCTURE /AIR_SAMPLE/
        REAL*4 CCN_TI(10),CCN_TT(10),CCN_T1(10),CCN_T2(10),
     -      CCN_T3(10),CCN_T4(10),CCN_REF(10),
     -      CCN_PEAK(10),CCN_SSAT_ST(10),CCN_COUNT_ST(10),
     -      CCN_SSAT_DY(300),CCN_DET_DY(300)
        INTEGER*4 CCN_TIME 
        INTEGER*4 IDTEMPS
        INTEGER*4 ISTEP
      END STRUCTURE
      RECORD /AIR_SAMPLE/ OUTSAMPLE(5)
      COMMON /CCNCOM/ OUTSAMPLE,ICOUNT
      VOLATILE /CCNCOM/
      INCLUDE 'DCOM_DEF.FOR'
C
      ILIST(1)=3
      ILIST(2)=18  
      IF(IMODE.EQ.1) ILIST(3)=2
      IF(IMODE.EQ.2) ILIST(3)=3
      CALL UPDT_STATUS(ILIST)
      ODISPLAY=.TRUE.
      OFIRST=.TRUE.
      OHOLD=.FALSE.
      OSINGLE=.FALSE.
      CALL SEL(0,IFOREG,IBACKG)
      CALL CCN_PLOT_SETUP(IMODE)       !Set up screen with axes,labels
      CALL HEAD_SHELL
      CALL SEL (0,IFOREG,IBACKG)
      CALL FOOT('SELECT','CCNMEN','HOLD','SINGLE',' ','PRINT',' ',
     -    ' ')
C
C This loop is executed continually while in a plotting option.
C ODISPLAY is always true, except when the user wishes to leave the option.
C The routine CCN_PLOT_CONTROL decides wether to plot.
C Any key presses are processed. 
C
      DO WHILE(ODISPLAY)
        IF(.NOT.OHOLD) CALL CCN_PLOT_CONTROL(OSINGLE,IPLOT,IMODE,.TRUE.,
     -      OFIRST)
        OFIRST=.FALSE.
        CALL SCA(13,13,0,3)
        IF(OSINGLE) OHOLD=.TRUE.
        CALL WAIT_KD(IRETACT)          !Key press: IRETACT=1 for A, 2:B etc.
C
C Process any key presses:
C If the program is in SINGLE mode, key presses mean something different
C Is this single or normal mode?

        IF(.NOT.OSINGLE) THEN
C
C If in normal mode:
C
          IF(IRETACT.EQ.1.OR.IRETACT.EQ.2) ODISPLAY=.FALSE. !A or B: exit.
          IF(IRETACT.EQ.3) THEN        !C:Hold/update
            OHOLD=(.NOT.OHOLD)
            IF(OHOLD) THEN
              CALL FOOT('SELECT','CCNMEN','UPDATE','SINGLE', !Change menu
     -                ' ','PRINT',' ',' ') ! accordingly
            ELSE
              CALL FOOT('SELECT','CCNMEN','HOLD','SINGLE',
     -                '      ','PRINT',' ',' ')
            END IF
          END IF
C
C If IRETACT=4 , or D was pressed, this is the SINGLE option, whereby the user
C can output a single plot. In single mode, the bottom menu changes to let
C the user choose any one of the five plots.

          IF(IRETACT.EQ.4) THEN        !D:Enter single mode
            DO II=1,5                                    
              IF(ICOUNT.GE.II) THEN    !Build single plot
                WRITE(CFOOT(II),'(''PLOT '',I1)') II ! menu.
                OPT(II)=1
              ELSE                        
                CFOOT(II)='      '
                OPT(II)=0
              END IF
            END DO
            IF(ICOUNT.LT.5) THEN       !Exit option goes
              CFOOT(5)='SELECT'        ! into spare box.
              ISELECT=50
            ELSE
              ISELECT=0
            END IF
            CALL FOOT(CFOOT(1),CFOOT(2),CFOOT(3),CFOOT(4), !Output new menu.
     -                CFOOT(5),'PRINT',' ','NORMAL')
            OSINGLE=.TRUE.
            OHOLD=.TRUE.
          END IF
        ELSE
C
C Process key presses if the user has already selected single mode.
C A=Plot 1, B=Plot 2 etc. The option is removed for any plot currently
C on screen, and is replaced be the SELECT option. (Exit CCN)
C F and G (PRINT and VIDEO) are unaffected.
C H returns to normal mode.

          IF(IRETACT.GE.1.AND.IRETACT.LE.5 ) THEN !Is a plot selected?
            IF(OPT(IRETACT).EQ.1) THEN !********Is it available?
              IPLOT=IRETACT            !Plot no. to be passed 
              OHOLD=.FALSE.            ! to plotting routine
              OPT(IPLOT)=0             !Make plot unavailable
              IF(ISELECT.NE.50) THEN
                CFOOT(IPLOT)='SELECT'  !Put SELECT option
                ISELECT=IPLOT          ! into spare box.
              ELSE
                CFOOT(IPLOT)='      '
              END IF
              IF(ILASTPLOT.GE.1.AND.ILASTPLOT.LE.ICOUNT) THEN
                WRITE(CFOOT(ILASTPLOT),'(''PLOT '',I1)') ILASTPLOT
                OPT(ILASTPLOT)=1       !Prv. plot now available
              END IF
              ILASTPLOT=IPLOT  
              CALL FOOT(CFOOT(1),CFOOT(2),CFOOT(3),CFOOT(4), !Output menu
     -            CFOOT(5),'PRINT',' ','NORMAL')
            ELSE                       !********If plot unavailable:
              IRET=IRETACT
              IF(IRET.EQ.ISELECT.OR.
     -            (IRET.EQ.5.AND.ISELECT.EQ.50)) THEN
                ODISPLAY=.FALSE.       !SELECT chosen.
                IRETACT=1
              END IF
            END IF
          END IF
C
C If H is pressed (IRETACT=8) then normal mode was reselected:

          IF(IRETACT.EQ.8) THEN
            OSINGLE=.FALSE.
            OHOLD=.FALSE.
            OFIRST=.TRUE.
            CALL FOOT('SELECT','CCNMEN','HOLD','SINGLE',' ','PRINT',
     -          ' ',' ')
          END IF
        END IF        
        CALL HEAD_UPDATE
      END DO
      RETURN
      END
C-------------------------------------------------------------------------------
      SUBROUTINE CCN_PLOT_LAST(IKEY)       
C
C Routine to plot single air-sample spectrum onto the screen.
C 
C Both the static and dynamic modes are plotted.
C
      IMPLICIT  NONE
      INTEGER*4 IKEY,ICOUNT,IMODE
      INTEGER*4 ILIST(3)
      LOGICAL*4 ODISPLAY
      LOGICAL*4 OFIRST
      LOGICAL*4 OHOLD
      LOGICAL*4 OCLEAR
      STRUCTURE /AIR_SAMPLE/
        REAL*4 CCN_TI(10),CCN_TT(10),CCN_T1(10),CCN_T2(10),
     -      CCN_T3(10),CCN_T4(10),CCN_REF(10),
     -      CCN_PEAK(10),CCN_SSAT_ST(10),CCN_COUNT_ST(10),
     -      CCN_SSAT_DY(300),CCN_DET_DY(300)
        INTEGER*4 CCN_TIME 
        INTEGER*4 IDTEMPS
        INTEGER*4 ISTEP
      END STRUCTURE
      RECORD /AIR_SAMPLE/ OUTSAMPLE(5)
      COMMON /CCNCOM/ OUTSAMPLE,ICOUNT
      VOLATILE /CCNCOM/
      INCLUDE  'DCOM_DEF.FOR'
C
      ILIST(1)=3
      ILIST(2)=18
      ILIST(3)=4
      CALL UPDT_STATUS(ILIST)
      OFIRST=.TRUE.
      ODISPLAY=.TRUE.
      OHOLD=.FALSE.
      OCLEAR=.TRUE.
      CALL SEL(0,IFOREG,IBACKG)
      CALL CCN_PLOT_SETUP(IMODE)       !Set up screen with axes,labels
      CALL HEAD_SHELL
      CALL FOOT('SELECT','CCNMEN','HOLD',' ',' ','PRINT',' ',' ')
      DO WHILE(ODISPLAY)
        IF(.NOT.OHOLD) CALL CCN_PLOT_CONTROL(.TRUE.,0,1,OCLEAR,OFIRST)
        OCLEAR=.FALSE.
        IF(.NOT.OHOLD) CALL CCN_PLOT_CONTROL(.TRUE.,0,2,OCLEAR,OFIRST)
        OFIRST=.FALSE.
        CALL WAIT_KD(IKEY)
        IF(IKEY.EQ.1.OR.IKEY.EQ.2) ODISPLAY=.FALSE.
        IF(IKEY.EQ.3) THEN
          IF(OHOLD) THEN
            OHOLD=.FALSE.
            CALL FOOT('SELECT','CCNMEN','HOLD',' ',' ','PRINT',' ',
     -          ' ')
          ELSE
            OHOLD=.TRUE.
            CALL FOOT('SELECT','CCNMEN','UPDATE',' ',' ','PRINT',
     -          ' ',' ')
          END IF
        END IF
        CALL HEAD_UPDATE
      END DO
      RETURN
      END
C-------------------------------------------------------------------------------
      SUBROUTINE CCN_PLOT_SETUP(IMODE)
C
C Set up screen with graph axes and labels for plotting:

      IMPLICIT  NONE
      INTEGER*4 X_POS,Y_POS,X,Y,IMODE,I
      REAL*4    R
      CHARACTER CR*4
      CHARACTER*19 CY_LABEL /'DETECTOR (DRS bits)'/
      CHARACTER*19 CX_LABEL /'Supersaturation (%)'/
      INCLUDE 'DCOM_DEF.FOR'
C
C Set up screen
C
      IF(IMODE.EQ.1) CY_LABEL='CCN Count (per cc) '
      CALL SEL(0,0,IBLACK)
      CALL RUB
      CALL SEL(0,IBACKG,0)                      !Dark green
      CALL BLK(9,102,737,509)                   !Fill block with background
      CALL SEL(0,IBLACK,IWHITE)
      CALL BLK(60,160,720,460)
C
C Draw tick marks
C
C Y axis: (logarithmic scale from 10 to 10000 on ordinates 160 to 460)
C Tick marks and labels:
C
      CALL SEL(0,IWHITE,IBACKG)
      CALL BOXA(59,159,721,461)                          
      CALL SCA(6,8,0,0)                                     !Small characters
      DO I=10,100,10
        CALL CCN_SCREEN(0.0,REAL(I),X,Y_POS)
        CALL VEC(55,Y_POS,60,Y_POS)      
        CALL VEC(55,Y_POS-100,60,Y_POS-100)  ! 100 - 1000
        CALL VEC(55,Y_POS-200,60,Y_POS-200)  !1000 - 10000
        CALL VEC(720,Y_POS,725,Y_POS)        ! Right hand side. 
        CALL VEC(720,Y_POS-100,725,Y_POS-100) 
        CALL VEC(720,Y_POS-200,725,Y_POS-200) 
      END DO                  
      CALL ALP(20,160,'10000')
      CALL ALP(20,260,' 1000')
      CALL ALP(20,360,'  100')
      CALL ALP(20,460,'   10')
C
C X axis: (linear scale from 0.0 to 1.2 on ordinates 60 to 720)
C
      DO R=0.0,1.2,0.05
        CALL CCN_SCREEN(R,1.0,X_POS,Y)
        CALL VEC(X_POS,462,X_POS,460)
        CALL VEC(X_POS,158,X_POS,160)
      END DO
      DO R=0.0,1.2,0.1
        CALL CCN_SCREEN(R,1.0,X_POS,Y)
        CALL VEC(X_POS,465,X_POS,460)
        CALL VEC(X_POS,155,X_POS,160)
        WRITE(CR,'(X,F3.1)') R
        CALL ALP(X_POS-12,470,CR)
      END DO
C
C Axis labels: "CCN count [per cc]" or "Log detector" and "Supersaturation (%)"
C
      CALL SCA(8,10,2,2)
      CALL ALP(10,260,CY_LABEL)
      CALL SCA(8,13,0,2)
      CALL ALP(200,490,CX_LABEL) 
      RETURN
      END
C-------------------------------------------------------------------------------
      SUBROUTINE CCN_PLOT_CONTROL(OSINGLE,IPLT,IMODE,OCLEAR,OFIRST)
C
C This routine controls plotting.
C In the main plotting routine CCN_PLOT_OPTION, this routine is called
C once for each execution of the loop if the HOLD function is off. 
C The routine first compares the spectra in storage now with those in storage
C last time it was called. If not changed since
C the last call,control returns to CCN_PLOT_OPTION, unless OFIRST is true,
C in which case plotting is forced.
C 
C
C OSINGLE =LOGICAL  If TRUE, just one spuctrum is plotted.
C IPLOT   =INTEGER  The spectrum number, (1-5), to be plotted (if OSINGLE)
C             (If OSINGLE and IPLOT=0 then plot the latest spectrum)
C 
      IMPLICIT  NONE
      INTEGER*4 IPLOT,IMODE,II,IX,ICNT,IPLOTS,ICOUNT,IPLT
      INTEGER*4 ICOL(5),IDASH(5),IOCNT /0/,DYN_START/1/
      LOGICAL*4 OCLEAR,OCLR,OSINGLE,OFIRST,OCHANGE,ONEW(5),SETPLOT
      LOGICAL*4 OSTART /.TRUE./
      STRUCTURE /AIR_SAMPLE/
        REAL*4 CCN_TI(10),CCN_TT(10),CCN_T1(10),CCN_T2(10),
     -      CCN_T3(10),CCN_T4(10),CCN_REF(10),
     -      CCN_PEAK(10),CCN_SSAT_ST(10),CCN_COUNT_ST(10),
     -      CCN_SSAT_DY(300),CCN_DET_DY(300)
        INTEGER*4 CCN_TIME 
        INTEGER*4 IDTEMPS
        INTEGER*4 ISTEP
      END STRUCTURE
      RECORD /AIR_SAMPLE/ OUTSAMPLE(5)
      RECORD /AIR_SAMPLE/ PLT_SAMPLE(5)
      COMMON /CCNCOM/ OUTSAMPLE,ICOUNT
      VOLATILE /CCNCOM/
      INCLUDE 'DCOM_DEF.FOR'
C
C The five plots are done with the following colours/line types:
C      
      IF(OSTART) THEN
        ICOL(1)=IRED
        ICOL(2)=IYELLOW
        ICOL(3)=IFOREG   !Cyan
        ICOL(4)=IGREEN
        ICOL(5)=IWHITE
        DO II=1,5                      !Plot 1: |----------------|
          IDASH(II)=II-1               !Plot 2: |- - - - - - - - |
        END DO                         !Plot 3: |----- - ----- - |
                                       !Plot 4: |-----   -----   |
        OSTART=.FALSE.                 !Plot 5: |------------    |
      END IF
C
C Has data changed?
C Each plot (1-5) is individually checked, and the logical array ONEW() holds
C a value .TRUE. (for new data) or .FALSE. (for no change) for each plot.
C Set IPLOT and IPLOTS to only replot those with new data, unless OFIRST is
C true, in which case all are plots are drawn anyway, or OSINGLE is true, when 
C the required plot is drawn anyway.
C
      OCHANGE=.FALSE.
      DO IX=1,5
        ONEW(IX)=.FALSE.
        ICNT=ICOUNT
        CALL CCN_DIFFERENCE(ONEW(IX),OUTSAMPLE(IX),PLT_SAMPLE(IX))
        PLT_SAMPLE(IX)=OUTSAMPLE(IX)
        IF(ONEW(IX))OCHANGE=.TRUE.
      END DO
C
C Set IPLOT and IPLOTS:
C IPLOT contains the first plot number to be drawn. 
C IPLOTS contains the number of plots to be drawn.
C If OSINGLE is true, these will have been specified already.
C If not, then they must be calculated now:
C
C If data has changed, calculate according to which plots have updated:
C
      IF(OCHANGE) THEN
        IF(.NOT.(OFIRST.OR.OSINGLE)) THEN
          IPLOTS=0
          IPLOT=0
          DO IX=1,5
            IF(ONEW(IX)) THEN
              IF(IPLOTS.EQ.0) IPLOT=IX
              SETPLOT=.FALSE.
              IPLOTS=IPLOTS+1
            END IF
          END DO
       END IF
      END IF
C
C If OFIRST is true but OSINGLE is false, all plots should be drawn:
C
      IF(OFIRST.AND..NOT.OSINGLE) THEN
        IPLOT=1
        IPLOTS=ICOUNT
      END IF
C
C If OSINGLE is true, just one plot is required:
C
      IF(OSINGLE) THEN                 !Single plot required...
        IF(IPLT.EQ.0) THEN             !Plot number not specified
          IPLOT=ICOUNT                 ! ...then latest plot
        ELSE
          IPLOT=IPLT                   !Plot number specified
        END IF
        IPLOTS=1                       !Number of plots=1
      END IF
C
C If any data changes have taken place the following code is run.
C
      IF(OCHANGE) THEN                            !Is there new data?
C
C Clear screen before plotting only if
C 1: It is required (initial value of OCLEAR)     AND
C 2: The data buffer has rotated with the arrival of a new plot,  OR
C 3: OSINGLE is true OR
C 4: OFIRST is true.

        IF((OCLEAR.AND.ICNT.NE.1.AND.ONEW(1)).OR.(OCLEAR.AND.OSINGLE)
     -      .OR.(OCLEAR.AND.OFIRST)) THEN
          OCLR=.TRUE.
        ELSE
          OCLR=.FALSE.
        END IF
        CALL CCN_PLOTTING(IMODE,IPLOT,IPLOTS,ICOL,IDASH,OCLR,PLT_SAMPLE) 
      ELSE
C
C If no data changes have taken place, plotting occurs anyway if OFIRST
C or OSINGLE is true.
 
        DYN_START=1
        IF((OFIRST.OR.OSINGLE).AND.ICNT.GT.0.AND.OCLEAR) THEN
          CALL CCN_PLOTTING(IMODE,IPLOT,IPLOTS,ICOL,IDASH,OCLEAR,
     -        PLT_SAMPLE) 
        END IF 
      END IF
      IF(.NOT.OSINGLE) IOCNT=ICOUNT
      RETURN
      END
C-------------------------------------------------------------------------------
      SUBROUTINE CCN_PLOTTING(IMODE,ISTART,ISPECS,ICOL,IDASH,OCLEAR,
     -    PLT_SAMPLE)
C
C Final CCN plotting routine, suitable for all CCN plotting
C This routine really DOES do plotting.
C
C IMODE=1 for Static mode, 2 for Dynamic mode
C ISTART=First spectrum number
C ISPECS=Number of spectra to plot
C ICOL=What colours to plot them
C IDASH =What line types to use
C OCLEAR=Clear screen or not?
C PLT_SAMPLE=Plot data

      IMPLICIT  NONE
      INTEGER*4 ICOL(5),IDASH(5)
      INTEGER*4 XPOS,YPOS,XV,YV,IMODE,ISTART,ISPECS,I,ICOLOUR,ILINE,JP,J
      LOGICAL*4 OCLEAR
      REAL*4    RSAT,RCCN
      CHARACTER C_KEYTEXT*13
      STRUCTURE /AIR_SAMPLE/
        REAL*4 CCN_TI(10),CCN_TT(10),CCN_T1(10),CCN_T2(10),
     -      CCN_T3(10),CCN_T4(10),CCN_REF(10),
     -      CCN_PEAK(10),CCN_SSAT_ST(10),CCN_COUNT_ST(10),
     -      CCN_SSAT_DY(300),CCN_DET_DY(300)
        INTEGER*4 CCN_TIME 
        INTEGER*4 IDTEMPS
        INTEGER*4 ISTEP
      END STRUCTURE
      RECORD /AIR_SAMPLE/ PLT_SAMPLE(5)
      INCLUDE 'DCOM_DEF.FOR'
C
C Clear graph area:
C
      IF(OCLEAR) THEN
        CALL CCN_CLEAR_PLOT
      END IF
      DO I=ISTART,ISTART+ISPECS-1      !Number of spectra to plot
        IF(I.LE.5.AND.I.GT.0) THEN
          ICOLOUR=ICOL(PLT_SAMPLE(I).ISTEP)
          ILINE=IDASH(PLT_SAMPLE(I).ISTEP)
          CALL SCA(8,10,0,2)                         
          WRITE(C_KEYTEXT,9) I,PLT_SAMPLE(I).CCN_TIME  !Plot legend
          IF(I.LT.4) THEN              !
            YV=115
            XV=50+(250*(I-1))          !Position of plot legend
          ELSE
            YV=130
            XV=100+(250*(I-4))
          END IF 
          CALL SEL(0,IWHITE,IBACKG)    !Legend in white writing
          CALL ALP(XV+50,YV,C_KEYTEXT) !Write to screen
          CALL SEL(0,ICOLOUR,IBLACK)   !Colour of plot  
          CALL SLT(ILINE)              !Line type of plot
          CALL VEC(XV,YV+5,XV+40,YV+5) !Plot line for legend
          CALL SLT(0)
          IF(IMODE.EQ.1) THEN          !Plot static spectrum
            JP=0
            DO J=1,PLT_SAMPLE(I).IDTEMPS
              RCCN=PLT_SAMPLE(I).CCN_COUNT_ST(J)
              RSAT=PLT_SAMPLE(I).CCN_SSAT_ST(J)
              IF(RCCN.GT.10.0.AND.RCCN.LT.10000 .AND.
     -            RSAT.GT.0.0.AND.RSAT.LT.1.2) THEN
                JP=JP+1
                CALL SLT(ILINE)                
                CALL CCN_SCREEN(RSAT,RCCN,XPOS,YPOS)
                IF(JP.GT.1) CALL VEC(XPOS,YPOS)            
                CALL ALP(XPOS-4,YPOS-5,'X')             
                CALL BIT(XPOS,YPOS)                
                CALL SLT(0)
              END IF                               
            END DO                                     
          ELSE                         !Plot dynamic spectrum
            JP=0
            DO J=1,300
              RCCN=PLT_SAMPLE(I).CCN_DET_DY(J)  
              RSAT=PLT_SAMPLE(I).CCN_SSAT_DY(J)  
              IF(RCCN.GT.0.0.AND.RCCN.LT.10000 .AND.
     -            RSAT.GT.0.0.AND.RSAT.LT.1.2) THEN
                JP=JP+1
                CALL CCN_SCREEN(RSAT,RCCN,XPOS,YPOS)
                IF(JP.EQ.1) THEN
                  CALL BIT(XPOS,YPOS)
                ELSE
                  CALL VEC(XPOS,YPOS)
                END IF
              END IF
            END DO
          END IF
        END IF
      END DO
      RETURN
    9 FORMAT('PLOT ',I1,1X,I6.6)
      END
C-------------------------------------------------------------------------------
      SUBROUTINE CCN_SCREEN(RSAT,RCCN,XPOS,YPOS)
      IMPLICIT  NONE
      REAL*4    RSAT,RCCN,X_POS,Y_POS
      INTEGER*4 XPOS,YPOS
C
C Given the data values supersaturation and CCN count, this routine oututs the
C screen coordinates for plotting the point.
      
      Y_POS=ALOG10(RCCN)-1.0
      Y_POS=460.0-(Y_POS*100.0)         
      X_POS=(660.0/1.2)*RSAT
      X_POS=X_POS+60.0
      XPOS=X_POS                       !Needs to be returned as
      YPOS=Y_POS                       ! integer for plotting
      RETURN
      END       
C-------------------------------------------------------------------------------
      SUBROUTINE CCN_CLEAR_PLOT
      IMPLICIT  NONE
      INTEGER*4 X,Y,DUM
      INTEGER*4 ILINE /34952/          ! Binary 1000 1000 1000 1000
      REAL*4    DUMMY,R
      INCLUDE   'DCOM_DEF.FOR'

      DUMMY=1.0 
      CALL DLT(ILINE)
      CALL SEL(0,IBLACK,0) 
      CALL BLK(60,160,720,460)
      CALL SEL(0,IWHITE,IBLACK) 
      CALL CCN_SCREEN(DUMMY,50.0,DUM,Y)
      CALL VEC(60,Y,720,Y)
      CALL CCN_SCREEN(DUMMY,500.0,DUM,Y)
      CALL VEC(60,Y,720,Y)
      CALL CCN_SCREEN(DUMMY,5000.0,DUM,Y)
      CALL VEC(60,Y,720,Y)
      CALL VEC(60,260,720,260)
      CALL VEC(60,360,720,360)
      DO R=0.1,1.1,0.1
        CALL CCN_SCREEN(R,DUMMY,X,DUM)
        CALL VEC(X,460,X,160)
      END DO
      CALL SLT(0)
      RETURN
      END
C-------------------------------------------------------------------------------
      SUBROUTINE CCN_DIFFERENCE(ONEW,SAMPLE1,SAMPLE2)
C
C Routine to check for differences between SAMPLE1 and SAMPLE2,
C both of which are of the AIR_SAMPLE structure.
C Returns ONEW=.TRUE. if differences were found.

      IMPLICIT  NONE
      INTEGER*4 I
      LOGICAL*4 ONEW 
      STRUCTURE /AIR_SAMPLE/
        REAL*4 CCN_TI(10),CCN_TT(10),CCN_T1(10),CCN_T2(10),
     -      CCN_T3(10),CCN_T4(10),CCN_REF(10),
     -      CCN_PEAK(10),CCN_SSAT_ST(10),CCN_COUNT_ST(10),
     -      CCN_SSAT_DY(300),CCN_DET_DY(300)
        INTEGER*4 CCN_TIME 
        INTEGER*4 IDTEMPS
        INTEGER*4 ISTEP
      END STRUCTURE
      RECORD /AIR_SAMPLE/ SAMPLE1,SAMPLE2
C 
      ONEW=.FALSE.
      DO I=1,10
        IF(SAMPLE1.CCN_TI(I).NE.SAMPLE2.CCN_TI(I)) ONEW=.TRUE.
        IF(SAMPLE1.CCN_TT(I).NE.SAMPLE2.CCN_TT(I)) ONEW=.TRUE.
        IF(SAMPLE1.CCN_T1(I).NE.SAMPLE2.CCN_T1(I)) ONEW=.TRUE.
        IF(SAMPLE1.CCN_T2(I).NE.SAMPLE2.CCN_T2(I)) ONEW=.TRUE.
        IF(SAMPLE1.CCN_T3(I).NE.SAMPLE2.CCN_T3(I)) ONEW=.TRUE.
        IF(SAMPLE1.CCN_T4(I).NE.SAMPLE2.CCN_T4(I)) ONEW=.TRUE.
        IF(SAMPLE1.CCN_REF(I).NE.SAMPLE2.CCN_REF(I)) ONEW=.TRUE.
        IF(SAMPLE1.CCN_PEAK(I).NE.SAMPLE2.CCN_PEAK(I)) ONEW=.TRUE.
        IF(SAMPLE1.CCN_SSAT_ST(I).NE.SAMPLE2.CCN_SSAT_ST(I)) ONEW=.TRUE.
        IF(SAMPLE1.CCN_COUNT_ST(I).NE.SAMPLE2.CCN_COUNT_ST(I))
     -      ONEW=.TRUE.
      END DO
      DO I=1,300
        IF(SAMPLE1.CCN_SSAT_DY(I).NE.SAMPLE2.CCN_SSAT_DY(I)) ONEW=.TRUE.
        IF(SAMPLE1.CCN_DET_DY(I).NE.SAMPLE2.CCN_DET_DY(I)) ONEW=.TRUE.
      END DO
      RETURN
      END
