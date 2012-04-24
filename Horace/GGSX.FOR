!*******************************************************************************
!
! GGSX.FOR - Routines to emulate the old GGS commands by 
! using PV-Wave.  This enables the old H_DISPLAY software to be used with
! X terminals until no longer required.
!
! To build:
!
! $ FORT /SEPARATE GGSX        !Note that /SEPARATE is essential on Alphas
! $ LINK program,GGSX,SYS$INPUT/OPTION
! WAVE_IMAGE/SHAREABLE
!
! Note that not all the functionality of the GGS library, emulations are only
! provided for the functions actually used by the HORACE display routines.
!
! With X terminals note the following:
!
! Note that it works with a fixed display size of 768 x 574 pixels to match the
! GGS graphics cards.  The routines are coded in FORTRAN but call PV-Wave 
! routines to carry out the graphics functions in an X-Window.  Note that
! the GGS system has the y axis coming down from the top of the screen, whereas
! the WAVE system comes up from the bottom.
!
! Note that the WAVE polyfill command requires the RH edge coordinate to be
! specified 1 greater than that actually required, and the bottom edge
! coordinate to be specified one less than that actually required (which means
! it will sometimes have a value of -1).
!
! Note the the WAVE device,copy command requires the bottom edges of the copied
! areas to be specified one less than actually required.
!
! Note that WAVE uses the symbol BOX as an enternal reference, and therefore
! the GGS BOX subroutine has had to be renamed BOXA.
!
! The following GGS commands, used by the H_DISPLAY program, are handled with
! the restrictions described:
!
! ALP Yes
! BBK Ignored
! BGO Ignored
! BIT Yes
! BLK Ignored
! BOX No - because WAVE uses BOX as an external symbol this is now called BOXA
! CIF Only handles full circles
! CIR Only handles full circles
! DLT Yes
! EBK Ignored
! FIN Yes
! FLO NOT SUPPORTED - filling will not be carried out on TRACK and TEPHI
! INI Yes, arguments ignored
! MES Ignored
! MIX Ignored
! PAG Ignored since only page 0 is ever used as display page
! PIX Yes, restores graphics page to top
! RUB Yes
! SAP Yes
! SCA Yes
! SEL Yes
! SET Ignored
! SLT Yes
! SOM Ignored
! SSA Ignored
! SYM Ignored
! TXT Ignored
! VDU Yes, uses terminal page
! VEC Yes
! VPT Yes, only BIT and VEC will handle viewporting
! WIN Yes, only BIT and VEC will handle viewporting
! ZOO Yes
!
! GGS_COMPRESS       Removes multiple spaces from strings
! GGS_ALPX           Writes text on X terminal display
! GGS_PRINT          Prints X terminal display
!
! V2.00  02/02/02  W.D.N.JACKSON
!
!*******************************************************************************
      SUBROUTINE INI
      ENTRY INI7
      IMPLICIT NONE
      INTEGER*4 IVDU
      INTEGER*4 I
      COMMON /GGSCOM/ IVDU
      CHARACTER CMDS(4)*65
      INTEGER*4 IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,IDISPAG,
     &    IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT
      COMMON /GGSXCOM/ IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,
     &    IDISPAG,IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT

      IFORCOL=6                        !Foreground colour
      IBCKCOL=0                        !Background colour
      ICURPOSX=0                       !Current X position
      ICURPOSY=0                       !Current Y position
      ICURLIN=0                        !Current line type
      IDISPAG=0                        !Current display page
      IACCPAG=0                        !Current access page
      IX1W=0                           !Virtual window definition
      IX2W=767
      IY1W=0
      IY2W=573
      IX1V=0                           !Viewport definition
      IX2V=767
      IY1V=0
      IY2V=573
      CALL CWAVFOR(2,1,'set_plot,''X''',11)
      CMDS(1)='window,0,xpos=0,xsize=768,ysize=574,'//
     &    'title=''HORACE Display System'',colors=16'
      CMDS(2)='window,1,/pixmap,xsize=768,ysize=574' !Create page 1 as pixmap
      CMDS(3)='wset,0'                 !Make page 0 access page
      CALL CWAVFOR(2,3,CMDS,65)
! Note that in specifying a colour table that matches the GGS table, it has 
! been necessary to separate the rgb values for the two greys, and the top
! white by at least 5.  Otherwise the TVRD function erroneously returns
! negative or otherwise incorrect values when the data are retrieved from
! the X display for printing.
      CMDS(1)=
     & 'r=[0,255,0,255,0,255,172,255,80,50,255,136,0,166,205,245]'
      CMDS(2)=
     & 'g=[0,0,255,255,0,0,255,255,85,206,128,206,0,42,210,250]'
      CMDS(3)=
     & 'b=[0,0,0,0,255,255,255,255,90,50,0,236,206,42,215,255]'
      CMDS(4)='tvlct,r,g,b'
      CALL CWAVFOR(2,4,CMDS,65)        !Load colour table to match GGS
      CMDS(1)='!p.color=6'             !Make foreground colour the default
      CMDS(2)='!p.linestyle=0'         !Solid lines by default
      CMDS(3)='!P.Clip=[0,0,767,573,0,0]' !Define clipping area
      CALL CWAVFOR(2,3,CMDS,65)
      DO I=0,15                        !Save colour table mapping in cmap
        CALL SEL(0,I,0)
        CALL BLK(I,573,I,573)
      END DO
      CMDS(1)='cmap=tvrd(0,0,16,1)'    !For use when printing
      CALL CWAVFOR(2,1,CMDS,50)
      IVDU=0                           !Start in graphics mode
      RETURN
      END
!*******************************************************************************
      SUBROUTINE FIN
      IMPLICIT NONE
      CHARACTER CDUMMY*4

      CALL CWAVFOR(3,0,CDUMMY,0)
      RETURN
      END
!*******************************************************************************
      SUBROUTINE BLK(IARG1,IARG2,IARG3,IARG4)
      IMPLICIT NONE
      INTEGER*4 IARG1,IARG2,IARG3,IARG4,IOS
      INTEGER*4 IXS,IYS,IXE,IYE
      CHARACTER CMDS(3)*30,CXS*3,CXE*3,CYS*3,CYE*3
      INTEGER*4 IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,IDISPAG,
     &    IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT
      COMMON /GGSXCOM/ IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,
     &    IDISPAG,IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT

      IF(IARGCOUNT().EQ.2) THEN
        IXS=ICURPOSX
        IYS=ICURPOSY
        IXE=IARG1
        IYE=IARG2
      ELSE
        IXS=IARG1
        IYS=IARG2
        IXE=IARG3
        IYE=IARG4
      END IF
      IF(IXS.LT.0) IXS=0
      IF(IYS.LT.0) IYS=0
      IF(IXE.GT.767) IXE=767
      IF(IYE.GT.573) IYE=573
      WRITE(CXS,'(I3)',IOSTAT=IOS) IXS
      WRITE(CXE,'(I3)',IOSTAT=IOS) IXE+1     !These extra pixels are needed
      WRITE(CYS,'(I3)',IOSTAT=IOS) 573-IYE-1 !for Wave to get it right
      WRITE(CYE,'(I3)',IOSTAT=IOS) 573-IYS
      CMDS(1)='x=['//CXS//','//CXE//','//CXE//','//CXS//','//CXS//']'
      CMDS(2)='y=['//CYS//','//CYS//','//CYE//','//CYE//','//CYS//']'
      CMDS(3)='polyfill,x,y,/device'   !Drawbox
      CALL CWAVFOR(2,3,CMDS,30)
      ICURPOSX=IXE
      ICURPOSY=IYE
      RETURN
      END
!*******************************************************************************
      SUBROUTINE BIT(IARG1,IARG2)
      IMPLICIT NONE
      INTEGER*4 IARG1,IARG2,IOS,IXE,IYE
      CHARACTER CMDS(2)*67,CXE*3,CYE*3
      INTEGER*4 IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,IDISPAG,
     &    IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT
      COMMON /GGSXCOM/ IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,
     &    IDISPAG,IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT

      IF(LVPT.OR.LWPT) THEN
        WRITE(CXE,'(I3.3)',IOSTAT=IOS) 
     &    NINT(REAL(IX2V-IX1V)/REAL(IX2W-IX1W)*REAL(IARG1-IX1W)+IX1V)
        WRITE(CYE,'(I3.3)',IOSTAT=IOS) 573-
     &   (NINT(REAL(IY2V-IY1V)/REAL(IY2W-IY1W)*REAL(IARG2-IY1W)+IY1V))
      ELSE
        IXE=IARG1
        IYE=IARG2
        WRITE(CXE,'(I3.3)',IOSTAT=IOS) IXE
        WRITE(CYE,'(I3.3)',IOSTAT=IOS) 573-IYE
      END IF
      IF(CXE.NE.'***'.AND.CYE.NE.'***') THEN
        CMDS(1)='x='//CXE//'&y='//CYE
        CMDS(2)='plots,x,y,/device,psym=6,symsize=0.2,/pclip' !Draw point
        CALL CWAVFOR(2,2,CMDS,67)
      END IF
      ICURPOSX=IARG1
      ICURPOSY=IARG2
      RETURN
      END
!*******************************************************************************
      SUBROUTINE BOXA(IARG1,IARG2,IARG3,IARG4)
      IMPLICIT NONE
      INTEGER*4 IARG1,IARG2,IARG3,IARG4,IOS
      INTEGER*4 IXS,IYS,IXE,IYE
      CHARACTER CMDS(3)*37,CXS*3,CXE*3,CYS*3,CYE*3
      INTEGER*4 IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,IDISPAG,
     &    IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT
      COMMON /GGSXCOM/ IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,
     &    IDISPAG,IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT

      IF(IARGCOUNT().EQ.2) THEN
        IXS=ICURPOSX
        IYS=ICURPOSY
        IXE=IARG1
        IYE=IARG2
      ELSE
        IXS=IARG1
        IYS=IARG2
        IXE=IARG3
        IYE=IARG4
      END IF
      IF(IXS.LT.0) IXS=0
      IF(IXS.GT.767) IXS=767
      IF(IXE.LT.0) IXE=0
      IF(IXE.GT.767) IXE=767
      IF(IYS.LT.0) IYS=0
      IF(IYS.GT.573) IYS=573
      IF(IYE.LT.0) IYE=0
      IF(IYE.GT.573) IYE=573
      WRITE(CXS,'(I3.3)',IOSTAT=IOS) IXS
      WRITE(CXE,'(I3.3)',IOSTAT=IOS) IXE
      WRITE(CYS,'(I3.3)',IOSTAT=IOS) 573-IYE
      WRITE(CYE,'(I3.3)',IOSTAT=IOS) 573-IYS
      CMDS(1)='x=['//CXS//','//CXE//','//CXE//','//CXS//','//CXS//']'
      CMDS(2)='y=['//CYS//','//CYS//','//CYE//','//CYE//','//CYS//']'
      CMDS(3)='plots,x,y,/device,/pclip'      !Drawbox
      CALL CWAVFOR(2,3,CMDS,37)
      ICURPOSX=IXE
      ICURPOSY=IYE
      RETURN
      END
!*******************************************************************************
      SUBROUTINE VEC(IARG1,IARG2,IARG3,IARG4)
      IMPLICIT NONE
      INTEGER*4 IARG1,IARG2,IARG3,IARG4,IOS
      INTEGER*4 IXS,IYS,IXE,IYE
      LOGICAL*4 LGDLAST
      CHARACTER CMDS(3)*57,CXS*4,CXE*4,CYS*4,CYE*4
      INTEGER*4 IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,IDISPAG,
     &    IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT
      COMMON /GGSXCOM/ IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,
     &    IDISPAG,IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT

      LGDLAST=.TRUE.
      IF(LVPT.OR.LWPT) THEN
        IF(IARGCOUNT().EQ.2) THEN
          IXS=NINT(REAL(IX2V-IX1V)/REAL(IX2W-IX1W)*REAL(ICURPOSX-
     &        IX1W)+IX1V)
          IYS=NINT(REAL(IY2V-IY1V)/REAL(IY2W-IY1W)*REAL(ICURPOSY-
     &        IY1W))+IY1V
          IXE=NINT(REAL(IX2V-IX1V)/REAL(IX2W-IX1W)*REAL(IARG1-IX1W)
     &        +IX1V)
          IYE=NINT(REAL(IY2V-IY1V)/REAL(IY2W-IY1W)*REAL(IARG2-IY1W))
     &        +IY1V
          ICURPOSX=IARG1
          ICURPOSY=IARG2
        ELSE
          IXS=NINT(REAL(IX2V-IX1V)/REAL(IX2W-IX1W)*REAL(IARG1-IX1W)
     &        +IX1V)
          IYS=NINT(REAL(IY2V-IY1V)/REAL(IY2W-IY1W)*REAL(IARG2-IY1W))
     &        +IY1V
          IXE=NINT(REAL(IX2V-IX1V)/REAL(IX2W-IX1W)*REAL(IARG3-IX1W)
     &        +IX1V)
          IYE=NINT(REAL(IY2V-IY1V)/REAL(IY2W-IY1W)*REAL(IARG4-IY1W))
     &        +IY1V
          ICURPOSX=IARG3
          ICURPOSY=IARG4
        END IF
      ELSE
        IF(IARGCOUNT().EQ.2) THEN
          IF(ICURPOSX.LT.0.OR.ICURPOSX.GT.767.OR.
     &       ICURPOSY.LT.0.OR.ICURPOSY.GT.573) LGDLAST=.FALSE.
          IXS=ICURPOSX
          IYS=ICURPOSY
          IXE=IARG1
          IYE=IARG2
        ELSE
          IXS=IARG1
          IYS=IARG2
          IXE=IARG3
          IYE=IARG4
        END IF
        ICURPOSX=IXE
        ICURPOSY=IYE
      END IF
      WRITE(CXS,'(I4.3)',IOSTAT=IOS) IXS
      WRITE(CXE,'(I4.3)',IOSTAT=IOS) IXE
      WRITE(CYS,'(I4.3)',IOSTAT=IOS) 573-IYS
      WRITE(CYE,'(I4.3)',IOSTAT=IOS) 573-IYE
      IF(CXS.NE.'****'.AND.CXE.NE.'****'.AND.CYS.NE.'****'.AND.
     &    CYE.NE.'****'.AND.LGDLAST) THEN
        CMDS(1)='x=['//CXS//','//CXE//']'
        CMDS(2)='y=['//CYS//','//CYE//']'
        CMDS(3)='plots,x,y,/device,/pclip'      !Draw line
        CALL CWAVFOR(2,3,CMDS,57)
      ELSE IF(CXE.NE.'****'.AND.CYE.NE.'****') THEN
        CMDS(1)='x='//CXE//'&y='//CYE
        CMDS(2)='plots,x,y,/device,psym=6,symsize=0.2,/pclip' !Draw point
        CALL CWAVFOR(2,2,CMDS,57)
      END IF
      RETURN
      END
!*******************************************************************************
      SUBROUTINE RUB(IARG1,IARG2,IARG3,IARG4)
      IMPLICIT NONE
      INTEGER*4 IARG1,IARG2,IARG3,IARG4,IOS,ICS
      CHARACTER CMDS(3)*30,CXS*3,CXE*3,CYS*3,CYE*3,COL*2
      INTEGER*4 IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,IDISPAG,
     &    IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT
      COMMON /GGSXCOM/ IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,
     &    IDISPAG,IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT

      IF(IBCKCOL.LT.0) RETURN          !Nothing to do in additive mode
      WRITE(COL,'(I2.2)',IOSTAT=IOS) IBCKCOL
      IF(IARGCOUNT().EQ.0) THEN
        IF(IACCPAG.NE.0) THEN
          CMDS(1)='wset,0'
          CMDS(2)='erase,'//COL
          CMDS(3)='wset,1'
          ICS=3
        ELSE
          CMDS(1)='erase,'//COL
          ICS=1
        END IF
      ELSE
        WRITE(CXS,'(I3)',IOSTAT=IOS) (IARG1/32)*32
        WRITE(CYS,'(I3)',IOSTAT=IOS) 573-IARG4-1
        WRITE(CXE,'(I3)',IOSTAT=IOS) ((IARG3+1)/32)*32
        WRITE(CYE,'(I3)',IOSTAT=IOS) 573-IARG2
        CMDS(1)='x=['//CXS//','//CXE//','//CXE//','//CXS//','//CXS
     &      //']'
        CMDS(2)='y=['//CYS//','//CYS//','//CYE//','//CYE//','//CYS
     &      //']'
        CMDS(3)='polyfill,x,y,/device,color='//COL
        ICS=3
      END IF
      CALL CWAVFOR(2,ICS,CMDS,30)
      RETURN
      END
!*******************************************************************************
      SUBROUTINE CIF(IARG1,IARG2,IARG3)
      IMPLICIT NONE
      INTEGER*4 IARG1,IARG2,IARG3,IOS
      CHARACTER CMDS(2)*80,CXE*3,CYE*3,CRD*3
      INTEGER*4 IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,IDISPAG,
     &    IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT
      COMMON /GGSXCOM/ IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,
     &    IDISPAG,IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT

      WRITE(CXE,'(I3.3)',IOSTAT=IOS) IARG1
      WRITE(CYE,'(I3.3)',IOSTAT=IOS) 573-IARG2
      WRITE(CRD,'(I3.3)',IOSTAT=IOS) IARG3
      CMDS(1)='a=findgen(360)'
      CMDS(2)='polyfill,'//CXE//'+'//CRD//'*cos(a*!pi/180),'//CYE//'+'
     &    //CRD//'*sin(a*!pi/180),/device'
      CALL CWAVFOR(2,2,CMDS,80)
      RETURN
      END
!*******************************************************************************
      SUBROUTINE CIR(IARG1,IARG2,IARG3)
      IMPLICIT NONE
      INTEGER*4 IARG1,IARG2,IARG3,IOS
      CHARACTER CMDS(2)*87,CXE*3,CYE*3,CRD*3
      INTEGER*4 IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,IDISPAG,
     &    IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT
      COMMON /GGSXCOM/ IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,
     &    IDISPAG,IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT

      WRITE(CXE,'(I3.3)',IOSTAT=IOS) IARG1
      WRITE(CYE,'(I3.3)',IOSTAT=IOS) 573-IARG2
      WRITE(CRD,'(I3.3)',IOSTAT=IOS) IARG3
      CMDS(1)='a=findgen(360)'
      CMDS(2)='plots,'//CXE//'+'//CRD//'*cos(a*!pi/180),'//CYE//'+'//
     &    CRD//'*sin(a*!pi/180),/device,/pclip'
      CALL CWAVFOR(2,2,CMDS,87)
      RETURN
      END
!*******************************************************************************
      SUBROUTINE BGO
      RETURN
      END
!*******************************************************************************
      SUBROUTINE BBK
      RETURN
      END
!*******************************************************************************
      SUBROUTINE EBK
      RETURN
      END
!*******************************************************************************
      SUBROUTINE SET
      RETURN
      END
!*******************************************************************************
      SUBROUTINE SOM
      RETURN
      END
!*******************************************************************************
      SUBROUTINE TXT
      RETURN
      END
!*******************************************************************************
      SUBROUTINE VDU
      IMPLICIT NONE
      INTEGER*4 IVDU
      COMMON /GGSCOM/ IVDU
      CHARACTER CMDS*20
      INTEGER*4 IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,IDISPAG,
     &    IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT
      COMMON /GGSXCOM/ IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,
     &    IDISPAG,IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT

      CMDS='wshow,0,0'
      CALL CWAVFOR(2,1,CMDS,20)
      IVDU=1
      RETURN
      END
!*******************************************************************************
      SUBROUTINE MIX
      RETURN
      END
!*******************************************************************************
      SUBROUTINE PIX
      IMPLICIT NONE
      INTEGER*4 IVDU
      COMMON /GGSCOM/ IVDU
      CHARACTER CMDS*20
      INTEGER*4 IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,IDISPAG,
     &    IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT
      COMMON /GGSXCOM/ IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,
     &    IDISPAG,IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT

      CMDS='wshow,0,1'
      CALL CWAVFOR(2,1,CMDS,20)
      IVDU=0
      RETURN
      END
!*******************************************************************************
      SUBROUTINE PAG
      RETURN
      END
!*******************************************************************************
      SUBROUTINE FLO
      RETURN
      END
!*******************************************************************************
      SUBROUTINE SAP(IARG1)
      IMPLICIT NONE
      INTEGER*4 IARG1
      CHARACTER CMDS*6
      INTEGER*4 IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,IDISPAG,
     &    IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT
      COMMON /GGSXCOM/ IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,
     &    IDISPAG,IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT

      IF(IARG1.EQ.0) THEN
        CMDS='wset,0'
        IACCPAG=0
      ELSE
        CMDS='wset,1'
        IACCPAG=1
      END IF
      CALL CWAVFOR(2,1,CMDS,6)
      RETURN
      END
!*******************************************************************************
      SUBROUTINE SEL(IARG1,IARG2,IARG3)
      IMPLICIT NONE
      INTEGER*4 IARG1,IARG2,IARG3,IOS,IDUMMY
      CHARACTER CMDS*15,CFOR*2
      INTEGER*4 IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,IDISPAG,
     &    IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT
      COMMON /GGSXCOM/ IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,
     &    IDISPAG,IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT

      IDUMMY=IARG1
      WRITE(CFOR,'(I2.2)',IOSTAT=IOS) IARG2
      CMDS='!p.color='//CFOR
      CALL CWAVFOR(2,1,CMDS,15)
      IFORCOL=IARG2
      IBCKCOL=IARG3     
      RETURN
      END
!*******************************************************************************
      SUBROUTINE DLT(IARG1)
      IMPLICIT NONE
      INTEGER*4 IARG1,IOS
      CHARACTER CMDS*15,CLIN*1
      INTEGER*4 IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,IDISPAG,
     &    IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT
      COMMON /GGSXCOM/ IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,
     &    IDISPAG,IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT

      ICURLIN=0
      IF(IARG1.EQ.'5555'X) ICURLIN=1
      IF(IARG1.EQ.'8888'X) ICURLIN=5
      WRITE(CLIN,'(I1.1)',IOSTAT=IOS) ICURLIN
      CMDS='!p.linestyle='//CLIN
      CALL CWAVFOR(2,1,CMDS,15)
      RETURN
      END
!*******************************************************************************
      SUBROUTINE SLT(IARG1)
      IMPLICIT NONE
      INTEGER*4 IARG1,IOS
      CHARACTER CMDS*15,CLIN*1
      INTEGER*4 IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,IDISPAG,
     &    IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT
      COMMON /GGSXCOM/ IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,
     &    IDISPAG,IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT

      ICURLIN=IARG1
      IF(IARG1.GT.5) ICURLIN=5
      WRITE(CLIN,'(I1.1)',IOSTAT=IOS) ICURLIN
      CMDS='!p.linestyle='//CLIN
      CALL CWAVFOR(2,1,CMDS,15)
      RETURN
      END
!*******************************************************************************
      SUBROUTINE ZOO(IARG1,IARG2,IARG3,IARG4,IARG5,IARG6,IARG7,IARG8)
      IMPLICIT NONE
      INTEGER*4 IARG1,IARG2,IARG3,IARG4,IARG5,IARG6,IARG7,IARG8,
     -    IOS,IDUMMY
      CHARACTER CMDS(3)*42,CXS*3,CYS*4,CNX*3,CNY*3,CXD*3,CYD*4,CW*1
      INTEGER*4 IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,IDISPAG,
     &    IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT
      COMMON /GGSXCOM/ IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,
     &    IDISPAG,IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT

      IF(IARGCOUNT().EQ.2) THEN      !Page copy
        IF(IARG2.EQ.0) CMDS(1)='wset,0'
        IF(IARG2.EQ.1) CMDS(1)='wset,1'
        IF(IARG1.EQ.0) CMDS(2)='device,copy=[0,-1,768,574,0,-1,0]'
        IF(IARG1.EQ.1) CMDS(2)='device,copy=[0,-1,768,574,0,-1,1]'
        IF(IACCPAG.EQ.0) CMDS(3)='wset,0'
        IF(IACCPAG.EQ.1) CMDS(3)='wset,1'
        CALL CWAVFOR(2,3,CMDS,40)
      ELSE                           !Block move with 8 args from display to
        IDUMMY=IARG6
        IDUMMY=IARG7
        WRITE(CXS,'(I3.3)',IOSTAT=IOS) IARG1 ! access page
        WRITE(CYS,'(I4.3)',IOSTAT=IOS) 573-IARG4-1
        WRITE(CNX,'(I3.3)',IOSTAT=IOS) IARG3-IARG1+1
        WRITE(CNY,'(I3.3)',IOSTAT=IOS) IARG4-IARG2+1
        WRITE(CXD,'(I3.3)',IOSTAT=IOS) IARG5
        WRITE(CYD,'(I4.3)',IOSTAT=IOS) 573-IARG8-1
        WRITE(CW,'(I1.1)',IOSTAT=IOS) IDISPAG
        CMDS(1)='device,copy=['//CXS//','//CYS//','//CNX//','//CNY//
     &      ','//CXD//','//CYD//','//CW//']'
        CALL CWAVFOR(2,1,CMDS,42)
      END IF
      RETURN
      END
!*******************************************************************************
      SUBROUTINE WIN(IARG1,IARG2,IARG3,IARG4)
      IMPLICIT NONE
      INTEGER*4 IARG1,IARG2,IARG3,IARG4
      INTEGER*4 IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,IDISPAG,
     &    IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT
      COMMON /GGSXCOM/ IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,
     &    IDISPAG,IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT

      IF(IARGCOUNT().EQ.0) THEN
        IX1W=0                         !Virtual window definition
        IX2W=767
        IY1W=0
        IY2W=573
        LWPT=.FALSE.
      ELSE
        IX1W=IARG1                     !Virtual window definition
        IX2W=IARG3
        IY1W=IARG2
        IY2W=IARG4
        LWPT=.TRUE.
      END IF
      RETURN
      END
!*******************************************************************************
      SUBROUTINE VPT(IARG1,IARG2,IARG3,IARG4)
      IMPLICIT NONE
      INTEGER*4 IARG1,IARG2,IARG3,IARG4,IOS
      CHARACTER CMDS*30
      INTEGER*4 IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,IDISPAG,
     &    IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT
      COMMON /GGSXCOM/ IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,
     &    IDISPAG,IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT

      IF(IARGCOUNT().EQ.0) THEN
        IX1V=0                         !Viewport definition
        IX2V=767
        IY1V=0
        IY2V=573
        LVPT=.FALSE.
        CMDS='!P.Clip=[0,0,767,573,0,0]'
      ELSE
        IX1V=IARG1                     !Viewport definition
        IX2V=IARG3
        IY1V=IARG2
        IY2V=IARG4
        LVPT=.TRUE.
        IF(IARG1.LT.0) IX1V=0
        IF(IARG2.LT.0) IY1V=0
        IF(IARG3.GT.767) IX2V=767
        IF(IARG4.GT.573) IY2V=573
        CMDS='!P.Clip=[xxx,xxx,xxx,xxx,0,0]'
        WRITE(CMDS(10:12),'(I3.3)',IOSTAT=IOS) IX1V
        WRITE(CMDS(14:16),'(I3.3)',IOSTAT=IOS) 573-IY2V
        WRITE(CMDS(18:20),'(I3.3)',IOSTAT=IOS) IX2V
        WRITE(CMDS(22:24),'(I3.3)',IOSTAT=IOS) 573-IY1V
      END IF
      CALL CWAVFOR(2,1,CMDS,30)
      RETURN
      END
!*******************************************************************************
      SUBROUTINE ALP(IARG1,IARG2,CARG1)
      IMPLICIT NONE
      INTEGER*4 IARG1,IARG2
      CHARACTER CARG1*(*)

      CALL GGS_ALPX(IARG1,IARG2,CARG1)
      RETURN
      END
!*******************************************************************************
      SUBROUTINE SCA(IARG1,IARG2,IARG3,IARG4)
      IMPLICIT NONE
      INTEGER*4 IARG1,IARG2,IARG3,IARG4
      INTEGER*4 IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,IDISPAG,
     &    IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT
      COMMON /GGSXCOM/ IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,
     &    IDISPAG,IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT

      ICWID=IARG1
      ICHGT=IARG2
      ICORI=IARG3
      ICFON=IARG4
      RETURN
      END
!*******************************************************************************
      SUBROUTINE SSA
      RETURN
      END
!*******************************************************************************
      SUBROUTINE SYM
      ENTRY      SYM2
      RETURN
      END
!*******************************************************************************
      SUBROUTINE MES
      RETURN
      END
!*******************************************************************************
      SUBROUTINE GGS_COMPRESS(CLINE,IS,IL)
      IMPLICIT NONE
      INTEGER*4 IS,IL,I
      CHARACTER CLINE*130,CTEMP*130

      CTEMP=CLINE
      IL=1
      DO I=2,IS
        IF(CTEMP(I:I).NE.' ') THEN
          IL=IL+1
          CLINE(IL:IL)=CTEMP(I:I)
        ELSE IF(CLINE(IL:IL).NE.' ') THEN
          IL=IL+1
          CLINE(IL:IL)=' '
        END IF
      END DO
      IF(CLINE(IL:IL).EQ.' ') IL=IL-1
      RETURN
      END
!*******************************************************************************
      SUBROUTINE GGS_ALPX(IIARG1,IIARG2,CARG3)
!
! Writes text string specified by CARG3 starting at x,y position specified by
! IARG1 and IARG2.  Because WAVE writes proportional text, and GGS uses fixed
! space character cells, each character is written separately starting at its
! correct position.
!
      IMPLICIT  NONE
      INTEGER*4 IIARG1,IIARG2,IARG1,IARG2,INUMB,IOS
      INTEGER*4 IXS,IYS,IXE,IYE,IUP
      INTEGER*4 IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,IDISPAG,
     &    IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT
      COMMON /GGSXCOM/ IFORCOL,IBCKCOL,ICURPOSX,ICURPOSY,ICURLIN,
     &    IDISPAG,IACCPAG,IX1W,IX2W,IY1W,IY2W,IX1V,IX2V,IY1V,IY2V,
     &    ICWID,ICHGT,ICORI,ICFON,LWPT,LVPT
      CHARACTER CARG3*(*),CXS*3,CYS*3,CMDS(3)*132,COL*2,CWD*2,CSZ*3,
     &    CXE*3,CYE*3

      IF(LVPT.OR.LWPT) THEN
        IARG1=NINT(REAL(IX2V-IX1V)/REAL(IX2W-IX1W)*REAL(IIARG1-IX1W)+
     -      IX1V)
        IARG2=NINT(REAL(IY2V-IY1V)/REAL(IY2W-IY1W)*REAL(IIARG2-IY1W)+
     -      IY1V)
      ELSE
        IARG1=IIARG1
        IARG2=IIARG2
        IF(IARG1.LT.0.OR.IARG1.GT.767) RETURN
        IF(IARG2.LT.0.OR.IARG2.GT.573) RETURN
      END IF
      IF(IBCKCOL.GE.0) THEN            !Not additive so write background area
        INUMB=LEN(CARG3)
        IF(ICORI.EQ.0) THEN            !Horizontal text
          IXS=IARG1
          IYE=573-IARG2
          IXE=IXS+INUMB*ICWID-1
          IF(MOD(ICFON,2).EQ.0) THEN   !Single height fonts
            IYS=IYE-ICHGT+1
          ELSE
            IYS=IYE-2*ICHGT+1          !Double height fonts
          END IF
        ELSE IF(ICORI.EQ.2) THEN       !Vertical text
          IXS=IARG1
          IYE=573-IARG2
          IF(MOD(ICFON,2).EQ.0) THEN   !Single height fonts
            IXE=IXS+ICHGT-1
          ELSE
            IXE=IXS+2*ICHGT-1          !Double height fonts
          END IF
          IYS=IYE-INUMB*ICWID+1
        END IF
        WRITE(COL,'(I2.2)',IOSTAT=IOS) IBCKCOL
        WRITE(CXS,'(I3)',IOSTAT=IOS) IXS
        WRITE(CXE,'(I3)',IOSTAT=IOS) IXE+1
        WRITE(CYS,'(I3)',IOSTAT=IOS) IYS-1
        WRITE(CYE,'(I3)',IOSTAT=IOS) IYE
        IF(CXS.NE.'***'.AND.CYS.NE.'***'.AND.CXE.NE.'***'.AND.
     &      CYE.NE.'***') THEN
         CMDS(1)='x=['//CXS//','//CXE//','//CXE//','//CXS//','//CXS//']'
         CMDS(2)='y=['//CYS//','//CYS//','//CYE//','//CYE//','//CYS//']'
         CMDS(3)='polyfill,x,y,/device,/pclip,color='//COL
         CALL CWAVFOR(2,3,CMDS,117)
        END IF
      END IF
!
! Now write the text
!
      IF(ICORI.EQ.0) THEN
        IF(MOD(ICFON,2).EQ.0) THEN   !Single height fonts
          IF(ICFON.EQ.0) IUP=(ICHGT-5)/2
          IF(ICFON.EQ.2) IUP=(ICHGT-7)/2
        ELSE
          IF(ICFON.EQ.1) IUP=ICHGT-5
          IF(ICFON.EQ.3) IUP=ICHGT-7
        END IF
        WRITE(CXS,'(I3.3)',IOSTAT=IOS) IARG1
        IF(MOD(ICFON,2).EQ.0) WRITE(CYS,'(I3.3)',IOSTAT=IOS)
     &      573-IARG2-ICHGT+IUP+1
        IF(MOD(ICFON,2).NE.0) WRITE(CYS,'(I3.3)',IOSTAT=IOS)
     &      573-IARG2-2*ICHGT+IUP+1
        WRITE(CWD,'(I2.2)',IOSTAT=IOS) ICWID
        IF(ICFON.EQ.0) CSZ='0.9'
        IF(ICFON.EQ.1) CSZ='1.9'
        IF(ICFON.EQ.2) CSZ='1.1'
        IF(ICFON.EQ.3) CSZ='2.2'
        IF(CXS.NE.'***'.AND.CYS.NE.'***') THEN
!
! The following commented out code shows how to use hardware fonts rather than
! the normal Wave soft fonts, which use very much more CPU.  However there is
! no fixed spacing font readily available with a pitch of 13 pixels, which is
! required to match the GGS usage, so this approach has been abandoned.
!          CMDS(1)='device,font=''*-courier-medium-r-normal--24-*''' !Too big
!          CMDS(1)='device,font=''12x24''' !Physical name, too small
!          CALL CWAVFOR(2,1,CMDS,117)
!          CMDS(1)='text='''//CARG3//''''
!          CMDS(2)='x='//CXS//'&y='//CYS//'&w='//CWD
!          CMDS(3)='xyouts,x,y,text,/device,font=0,/pclip'
!          CALL CWAVFOR(2,3,CMDS,117)
!
          CMDS(1)='text='''//CARG3//''''
          CMDS(2)='x='//CXS//'&y='//CYS//'&w='//CWD
          CMDS(3)='for i=0,strlen(text)-1 do '//
     &        'xyouts,x+i*w+w/2,y,strmid(text,i,1),/device'//
     &        ',size='//CSZ//',alignment=0.5,/pclip'
          CALL CWAVFOR(2,3,CMDS,117)
        END IF
        ICURPOSY=IARG2+ICHGT
      ELSE IF(ICORI.EQ.2) THEN
        IF(MOD(ICFON,2).EQ.0) THEN   !Single height fonts
          IF(ICFON.EQ.0) IUP=(ICHGT-5)/2
          IF(ICFON.EQ.2) IUP=(ICHGT-7)/2
        ELSE
          IF(ICFON.EQ.1) IUP=ICHGT-5
          IF(ICFON.EQ.3) IUP=ICHGT-7
        END IF
        IF(MOD(ICFON,2).EQ.0) 
     &      WRITE(CXS,'(I3.3)',IOSTAT=IOS) IARG1+ICHGT-IUP
        IF(MOD(ICFON,2).NE.0) 
     &      WRITE(CXS,'(I3.3)',IOSTAT=IOS) IARG1+2*ICHGT-IUP
        WRITE(CYS,'(I3.3)',IOSTAT=IOS)
     &      573-(IARG2+LEN(CARG3)*ICWID)
        WRITE(CWD,'(I2.2)',IOSTAT=IOS) ICWID
        IF(ICFON.EQ.0) CSZ='0.9'
        IF(ICFON.EQ.1) CSZ='1.9'
        IF(ICFON.EQ.2) CSZ='1.1'
        IF(ICFON.EQ.3) CSZ='2.2'
        IF(CXS.NE.'***'.AND.CYS.NE.'***') THEN
          CMDS(1)='text='''//CARG3//''''
          CMDS(2)='x='//CXS//'&y='//CYS//'&w='//CWD
          CMDS(3)='for i=0,strlen(text)-1 do '//
     &        'xyouts,x,y+i*w+w/2,strmid(text,i,1),/device'//
     &        ',size='//CSZ//',alignment=0.5,orientation=90,/pclip'
          CALL CWAVFOR(2,3,CMDS,132)
        END IF
        ICURPOSY=IARG2+ICHGT
      END IF
      RETURN
      END
!*******************************************************************************
      SUBROUTINE GGS_PRINT(CFILE)
!
! Reads current display from the X terminal, converts the colour indices
! to those specified for monochrome GGS, inverts the image to give black on
! white, writes it to postscript file, and puts file on the printer queue, 
! specified by the logical name H_DISPLAY_POST_PRINT_COMMAND logical.
! 
! The explicit mapping/checking of each colour index may no longer be 
! necessary, but may be needed if other programs are being run on the PC which
! are reallocating colour map entries.
!
! NOTE that the displays will not print correctly if they are impinged upon by
! any other windows, particularly the icon panel to the right, which must be
! kept small.
!
      IMPLICIT  NONE
      INTEGER*4 LAN
      CHARACTER CMDS(39)*50,CFILE*(*),COMMAND*80

      CALL GET_LOGICAL('H_DISPLAY_POST_PRINT_COMMAND',COMMAND)
      IF(COMMAND.EQ.' ') RETURN

      CMDS(1)='a=tvrd(0,0,768,574)'
      CMDS(2)='set_plot,''PS'''
      CMDS(3)='device,filename="'//CFILE//'"'
      CMDS(4)='index=where(a eq cmap(0),icount)'
      CMDS(5)='if icount gt 0 then a(index)=0b'
      CMDS(6)='index=where(a eq cmap(15),icount)'
      CMDS(7)='if icount gt 0 then a(index)=15b'
      CMDS(8)='index=where(a eq cmap(14),icount)'
      CMDS(9)='if icount gt 0 then a(index)=14b'
      CMDS(10)='index=where(a eq cmap(13),icount)'
      CMDS(11)='if icount gt 0 then a(index)=15b'
      CMDS(12)='index=where(a eq cmap(11),icount)'
      CMDS(13)='if icount gt 0 then a(index)=15b'
      CMDS(14)='index=where(a eq cmap(10),icount)'
      CMDS(15)='if icount gt 0 then a(index)=15b'
      CMDS(16)='index=where(a eq cmap(4),icount)'
      CMDS(17)='if icount gt 0 then a(index)=15b'
      CMDS(18)='index=where(a eq cmap(5),icount)'
      CMDS(19)='if icount gt 0 then a(index)=15b'
      CMDS(20)='index=where(a eq cmap(6),icount)'
      CMDS(21)='if icount gt 0 then a(index)=15b'
      CMDS(22)='index=where(a eq cmap(7),icount)'
      CMDS(23)='if icount gt 0 then a(index)=15b'
      CMDS(24)='index=where(a eq cmap(8),icount)'
      CMDS(25)='if icount gt 0 then a(index)=15b'
      CMDS(26)='index=where(a eq cmap(1),icount)'
      CMDS(27)='if icount gt 0 then a(index)=15b'
      CMDS(28)='index=where(a eq cmap(2),icount)'
      CMDS(29)='if icount gt 0 then a(index)=15b'
      CMDS(30)='index=where(a eq cmap(12),icount)'
      CMDS(31)='if icount gt 0 then a(index)=0b'
      CMDS(32)='index=where(a eq cmap(9),icount)'
      CMDS(33)='if icount gt 0 then a(index)=0b'
      CMDS(34)='index=where(a eq cmap(3),icount)'
      CMDS(35)='if icount gt 0 then a(index)=8b'
      CMDS(36)='a=15-a'
      CMDS(37)='tvscl,a'
      CMDS(38)='device,/close_file'
      CMDS(39)='set_plot,''X'''
      CALL CWAVFOR(2,39,CMDS,50)

      CALL LIB$SPAWN(COMMAND(1:LAN(COMMAND))//' '//
     &    CFILE(1:LAN(CFILE)),,,1+2+4+8,,,,,,,,)
      RETURN
      END
!*******************************************************************************
      SUBROUTINE CWAVFOR(I1,I2,C3,I4)
!
! This routine is simply here so that the linker gives only one message instead
! of many if these routines are linked without WAVE.
!
      IMPLICIT NONE
      INTEGER*4 I1,I2,I4
      CHARACTER C3*(*)
      CALL CWAVEFOR(I1,I2,C3,I4)
      RETURN
      END
