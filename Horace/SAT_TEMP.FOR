!*******************************************************************************
      SUBROUTINE SAT_TEMP(ISTART,IEND,CFILE)
!
! Generates a temp message file using the start and end times specified by
! ISTART and IEND.
!      
      IMPLICIT NONE
      CHARACTER*(*) CFILE
      CHARACTER*240 CUUAA
      CHARACTER*960 CUUBB
      CHARACTER*80 CLINE
      CHARACTER*9  CFLTDAT
      CHARACTER*5 YYGGI,QC,PPHHH,TTTDD,DDFFF,MARSDEN
      INTEGER*4 F,IP(100),NS,KP,MP,I,NP,LTD,SIGI(8),LEN
      INTEGER*4 ISEC,ISTART,IEND,TM,I1,I2,IM,DI,GG,REGI,ID
      INTEGER*4 YY,NA,NB,MM,DPI1,TI,LP,DPDMXI,DPDMNI
      INTEGER*4 IOS,IOS1
      REAL*4 TT,DD,HH,LAT,LON,DH,DQ,TO,DO,PO,REGP,REGD,LGDP,PI
      REAL*4 A,B,TTEST,DTEST,SIGP(8),SIGD(8),SIGH(8),PKP,PTI
      REAL*4 R,G,DEWP,DPK,DPDMX,DPDMN,DPD,SGD,RGD,P1,P2,PDPI
      INCLUDE 'HCOM_DEF.FOR'
C
C
      OPEN(11,FILE=CFILE,STATUS='NEW',CARRIAGECONTROL='LIST')
C
C  SET UP CONSTANTS
C
C   TEMP AND DEWPOINT MAX VARIATIONS ( FOR CALCULATING SIG POINTS )
      TT=1.0
      DD=2.0
C
C   NUMBER OF POINTS IN DERIVE
      NP=STATUS(3)+1
      IF (NP.LT.30) THEN 
        WRITE(11,'(A)',IOSTAT=IOS) 'Not enough data in derive'
        GOTO 37
      ENDIF
C
C   PHYSICAL CONSTANTS
      R=287.0
      G=9.80665
C
C   STANDARD PRESSURE LEVELS
      SIGP(1)=1000
      SIGP(2)=850
      SIGP(3)=700
      SIGP(4)=500
      SIGP(5)=400
      SIGP(6)=300
C
C   REGIONAL WIND LEVELS
      SIGP(7)=800
      SIGP(8)=600
      REGP=900
C
C   MAX DEVIATIONS FROM STANDARD AND REGIONAL LEVELS
      RGD=10.0
      SGD=3.0
      DO F=1,8
        SIGD(F)=SGD
      ENDDO
      REGD=RGD
C
C   OPEN OUTPUT FILE ***
C

C                       Profile 1  A329
C      ISTART=42150   
C      IEND=43430
C                       Profile 2  A329
C      ISTART=47478
C      IEND=48865
C                       Profile 1 A368
C       ISTART=22238
C       IEND=22896
C                       Sawtooth 1 A368
C       ISTART=23242
C       IEND=23671

      OPEN(13,FILE='SAT_TEMP.TXT',STATUS='OLD',CARRIAGECONTROL='LIST',
     +READONLY,IOSTAT=IOS1)
      IF (IOS1.EQ.0) THEN
   14 CONTINUE
      READ(13,'(A)',END=15) CLINE
      WRITE(11,'(A)',IOSTAT=IOS) CLINE(1:LEN(CLINE))
      GOTO 14
   15 CONTINUE
      ENDIF
      CLOSE(13)
      IF(ISTART.LT.0) THEN 
        WRITE(11,'(A)',IOSTAT=IOS) 'Invalid start time'
        GOTO 37
      ENDIF
      IF(IEND.LT.0) THEN
        WRITE(11,'(A)',IOSTAT=IOS) 'Invalid end time'
        GOTO 37
      ENDIF
C
C
C   IF START AFTER END ASSUME ACROSS MIDNIGHT
      IF (IEND.LT.ISTART) IEND=IEND+86400
C
C   FIND THE ORDER AND THE START AND END POSITIONS
      ISEC=STATUS(2)
      IF ((ISEC.LT.1).OR.(ISEC.GT.12800)) THEN
        WRITE(11,'(A)',IOSTAT=IOS)'ISTATUS(2) out of range'
        GOTO 37
      ENDIF
      IF (RSECS(ISEC).LT.ISTART) GOTO 37
      IF (RSECS(ISEC).LT.IEND) THEN
        WRITE(11,'(A)',IOSTAT=IOS)
     +'** Unable to code whole run - not yet reached end time'
        WRITE(11,'(A)',IOSTAT=IOS)
     +' truncating run'
        IEND=RSECS(ISEC)
      ENDIF
      TM=(IEND+ISTART)/2
      DI=1
C   END POINT
      I2=ISEC-1
      DO WHILE (RSECS(I2+1).GT.IEND)
        I2=MOD(I2-1+NP,NP)
        IF (I2.EQ.ISEC) GOTO 37
      ENDDO
C   MID POINT
      IM=I2
      DO WHILE (RSECS(IM+1).GT.TM)
        IM=MOD(IM-1+NP,NP)
        IF (IM.EQ.ISEC) GOTO 37
      ENDDO
      LAT=RGLAT(IM+1)
      LON=RGLNG(IM+1)
      GG=MOD(TM/3600,24)
      MM=MOD(TM/60,60)
      IF (TM.GT.86400) THEN
        CALL DATE1(CFLTDAT)
      WRITE(11,'(A)',IOSTAT=IOS)
     +'Mid profile past midnight check date '//CFLTDAT
      ELSE
        IF ((STATUS(1).EQ.1).OR.(STATUS(1).EQ.2)) THEN
          CALL DRS_CDAT(NFDATA(1,STATUS(1)),CFLTDAT)
        ELSE
          CALL DATE1(CFLTDAT)
        ENDIF
      ENDIF
      WRITE(11,'(A25,I2.2,A1,I2.2)',IOSTAT=IOS) 
     +'Mid Profile '//CFLTDAT//' ',GG,':',MM
      IF (MM.GT.30) GG=MOD(GG+1,24)
      READ(CFLTDAT(1:2),'(I2)')YY
      YY=YY+50
C   START POINT
      I1=IM
      DO WHILE (RSECS(I1+1).GT.ISTART)
        I1=MOD(I1-1+NP,NP)
        IF (I1.EQ.ISEC) GOTO 37
      ENDDO
C
C   PUT IN ASCENDING ORDER
      IF (RSPR(I1+1).LT.RSPR(I2+1)) THEN
        IM=I1
        I1=I2
        I2=IM
        DI=-1
      ENDIF
      P1=RSPR(I1+1)
      P2=RSPR(I2+1)
      IF ((P1.LT.50).OR.(P2.LT.50).OR.(P1.GT.1300).OR.(P2.GT.1300) 
     +.OR.((P1-P2).LT.30)) THEN
        WRITE(11,'(A)',IOSTAT=IOS)'Pressures out of range'
        GOTO 37
      ENDIF
      HH=RRHGT(I1+1)
      IF(HH.EQ.0)HH=RPHGT(I1+1)
      PO=RSPR(I1+1)*EXP((G*HH)/(R*RTATDI(I1+1)))
      TO=500
      DO=500
      IF (HH.LT.30.5) THEN
        TO=RTATDC(I1+1)
        DO=DEWP(I1+1)
      ENDIF
C
C *****************************************************************
C
C   *******    STANDARD ( AND REGIONAL ) LEVELS    ********
C
      NS=MOD(I1-DI+NP,NP)
      KP=NS
      IF (PO.LT.1000.0) THEN
         SIGH(1)=(R*(RTATDI(I1+1))*LOG(PO/1000.0))/G
         SIGD(1)=0.0
         SIGI(1)=I1
      ENDIF
      DO WHILE (NS.NE.I2)
        NS=MOD(NS+DI+NP,NP)
        PTI=RSPR(NS+1)
        PKP=RSPR(KP+1)
        IF ((NS.NE.I1).AND.(PTI.LT.PKP).AND.(PTI.GE.P2)) THEN
          DH=(R/(G*2.0))*(RTATDI(NS+1)+RTATDI(KP+1))
     +*LOG(PKP/PTI)
          HH=HH+DH
          KP=NS
        ENDIF
        DO F=1,8
          DQ=ABS(PTI-SIGP(F))
          IF (DQ.LT.SIGD(F)) THEN
            SIGD(F)=DQ
            SIGI(F)=NS
            IF (F.LT.7) SIGH(F)=HH
          ENDIF
        ENDDO
        DQ=ABS(RRHGT(NS+1)-REGP)
        IF (DQ.LT.REGD) THEN
          REGD=DQ
          REGI=NS
        ENDIF
       ENDDO
C************************************************************************
C
C
C     ****** SIGNIFICANT POINTS ******
C
C
C     Start with the base point
C
C      WRITE(11,'(A)',IOSTAT=IOS) 'MP,TI,DPI1,LP,KP,DPDMXI,DPDMNI'
      NS=1
      IP(NS)=I1
      DPI1=-1
      TI=I1
      LGDP=DEWP(I1+1)
      PDPI=RSPR(I1+1)
      IF (LGDP.LT.400) DPI1=I1
C
C     Until top of profile
C
      DO WHILE (NINT(P2).NE.NINT(RSPR(1+IP(NS))))
C
C     Try the highest point first (KP)
C
        KP=I2
        LP=-1
        MP=0
        DO WHILE((MP.NE.2).AND.(KP.NE.TI))

C
C     Can you draw a straight line for the temperature (+-TT) ?
C
          PKP=RSPR(KP+1)
          PTI=RSPR(TI+1)
          IF ((PKP.GE.P2).AND.(PKP.LT.PTI)) THEN
          A=(RTATDC(KP+1)-RTATDC(1+TI))/
     +LOG(PKP/PTI)
          I=TI
          MP=1
          DO WHILE(I.NE.KP)
            I=MOD(I+DI+NP,NP)
            PI=RSPR(I+1)
            IF ((PI.LT.PTI).AND.(PI.GE.PKP)) THEN
              TTEST=RTATDC(1+TI)+A*LOG(PI/PTI) 
              IF (ABS(RTATDC(I+1)-TTEST).GT.TT) THEN
                      MP=0
                    ENDIF
            ENDIF
          ENDDO
C
C     Can you draw a straight line for the dewpoint (+-DD) ?
C
          B=DEWP(KP+1)
          DPDMXI=KP
          DPDMNI=KP
          IF ((B.LT.400).AND.(MP.EQ.1)) THEN
            DPDMX=RTATDC(KP+1)-B
            DPDMN=DPDMX
            IF ((DPI1.NE.-1).AND.(PDPI.GT.PKP))
     +B=(B-LGDP)/LOG(PKP/PDPI)
            MP=2
            I=TI
            DO WHILE(I.NE.KP)
              DPK=DEWP(I+1)
              IF (DPK.LT.400) THEN
                IF (DPI1.EQ.-1) THEN
C
C    If it is the first good dewpoint then use it !
C
                  MP=2
                  KP=I
                  GOTO 17
                ELSE
                  PI=RSPR(I+1)
                  IF ((PI.LE.PDPI).AND.(PI.GE.PKP)) THEN
                    DTEST=LGDP+B*LOG(PI/PDPI) 
                    IF (ABS(DPK-DTEST).GT.DD) THEN
                      MP=0
                    ENDIF
                    DPD=(RTATDC(I+1)-DPK) 
                    IF (DPD.LE.DPDMN) THEN
                      DPDMN=DPD
                      DPDMNI=I
                    ENDIF
                    IF (DPD.GE.DPDMX) THEN
                      DPDMX=DPD
                      DPDMXI=I
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
              I=MOD(I+DI+NP,NP)
            ENDDO
          ENDIF
          IF ((MP.EQ.1).AND.(LP.EQ.-1)) LP=KP
C      WRITE(11,'(3I6)',IOSTAT=IOS) KP,DPDMNI,DPDMXI
          IF ((NINT(RSPR(DPDMXI+1)).GT.NINT(RSPR(KP+1))).AND.
     +(DPDMXI.NE.TI)) THEN
            IF (((DPDMX-DPDMN)*5.).GE.DPDMN) THEN
              MP=0
              KP=MOD(DPDMXI+DI+NP,NP)
            ENDIF
          ENDIF
          IF ((NINT(RSPR(DPDMNI+1)).GT.NINT(RSPR(KP+1))).AND.
     +(DPDMNI.NE.TI)) THEN
            IF (((DPDMX-DPDMN)*5.).GE.DPDMN) THEN
              MP=0
              KP=MOD(DPDMNI+DI+NP,NP)
            ENDIF
          ENDIF
          ENDIF
          KP=MOD(KP-DI+NP,NP)
        ENDDO
        IF (TI.EQ.KP) THEN
          MP=0
          TI=MOD(TI+DI+NP,NP)
          IF (DEWP(1+TI).LT.400) THEN
            DPI1=TI
            PDPI=RSPR(1+TI)
            LGDP=DEWP(1+TI)
          ENDIF                     
        ENDIF
        KP=MOD(KP+DI+NP,NP)
   17 CONTINUE
C
C     MP=2  Good temp and dewpoint
C     LP=where MP first =1  Good temp, but no dewpoint
C     MP=0  No good points !!
C
        IF (LP.GT.-1) TI=LP
        IF (MP.GT.1) THEN
          DPI1=KP
          PDPI=RSPR(DPI1+1)
          LGDP=DEWP(DPI1+1)
          TI=KP
          IF (NINT(RSPR(TI+1)).EQ.NINT(RSPR(IP(NS)+1))) NS=NS-1
        ENDIF
        NS=NS+1
        IP(NS)=TI
      ENDDO
C
C
C
C
C    ******* CODE IT UP, AND WRITE IT OUT  ********************
C
C    ===== PART A =====
C    
C    ---- SECTION 1 ----
C
      I=ID(SIGD,SGD)
      WRITE(CUUAA(1:23),'(A8,I3.3,A)',IOSTAT=IOS) 
     +YYGGI(YY,GG,I,SIGP)//' 99',
     +NINT(ABS(LAT*10.0)),' '//QC(LAT,LON)//' '//MARSDEN(LAT,LON)
C
C    ---- SECTION 2 ---
C
      WRITE(CUUAA(24:42),'(A3,I3.3,A)',IOSTAT=IOS)' 99',LTD(PO),' '//
     +TTTDD(TO,DO)//' '//DDFFF(RIWA(I1),RIWS(I1))//' ' 
      NA=43
      DO F=1,I
        IF (SIGD(F).LT.SGD) THEN
          WRITE(CUUAA(NA:NA+17),'(A)',IOSTAT=IOS)
     +PPHHH(SIGP(F),SIGH(F))//
     +' '//TTTDD(RTATDC(SIGI(F)),DEWP(SIGI(F)))//' '//
     +DDFFF(RIWA(SIGI(F)),RIWS(SIGI(F)))//' '       
        ELSE
          KP=ltd(SIGP(F))/10
          WRITE(CUUAA(NA:NA+17),'(I2.2,A)',IOSTAT=IOS)
     +KP,'/// ///// ///// '
        ENDIF   
      NA=NA+18
      ENDDO
C
C    ---- SECTIONS 3 AND 4 ----
C
      WRITE(CUUAA(NA:NA+12),'(A)',IOSTAT=IOS) '88999 77999='
      NA=NA+12
C
C    ++++ NOW WRITE IT OUT 65 CHARACTERS PER LINE ++++
C      
C      WRITE(11,'(A)',IOSTAT=IOS) 'No 41414 Clouds coded'
      KP=60
      IF (KP.GT.NA) KP=NA
      WRITE(11,'(A)',IOSTAT=IOS) 'UUAA '//CUUAA(1:KP)
      DO WHILE (KP.LT.NA)
        MP=KP+1
        KP=KP+60
        IF (KP.GT.NA) KP=NA
        WRITE(11,'(A)',IOSTAT=IOS) '     '//CUUAA(MP:KP)
      ENDDO
C
C   ===== PART B =====
C
C   ---- SECTION 1 ----
C

      WRITE(CUUBB(1:29),'(A8,I3.3,A)',IOSTAT=IOS) 
     +YYGGI(YY,GG,-1)//' 99',
     +NINT(ABS(LAT*10.0)),' '//QC(LAT,LON)//' '//MARSDEN(LAT,LON)
C
C   ---- SECTION 5 ----
C
      NB=24
      DO F=1,NS
        KP=MOD((F-1),9)
        IF ((F.NE.1).AND.(KP.EQ.0)) KP=9
        KP=10000*KP+1000*KP+LTD(RSPR(1+IP(F)))
        WRITE(CUUBB(NB:NB+11),'(A1,I5.5,A)',IOSTAT=IOS)
     +' ',KP,' '//TTTDD(RTATDC(1+IP(F)),DEWP(1+IP(F)))
        NB=NB+12
      ENDDO
C
C   ---- SECTION 8 ----
C
      WRITE(CUUBB(NB:NB+11),'(A)',IOSTAT=IOS)' 41414 /////'
      NB=NB+12
C
C   ---- SECTION 9 ----
C
      IF (REGD.LT.RGD) THEN
        WRITE(CUUBB(NB:NB+17),'(A9,I3.3,A)',IOSTAT=IOS)
     +' 51515 11',LTD(RSPR(1+REGI)),' '//DDFFF(RIWA(REGI),RIWS(REGI))
        NB=NB+18
        IF (SIGD(7).LT.SGD) THEN
          WRITE(CUUBB(NB:NB+11),'(A)',IOSTAT=IOS)' 22800 '//
     +DDFFF(RIWA(SIGI(7)),RIWS(SIGI(7)))
          NB=NB+12
        ENDIF
        IF (SIGD(8).LT.SGD) THEN
          WRITE(CUUBB(NB:NB+11),'(A)',IOSTAT=IOS)' 33600 '//
     +DDFFF(RIWA(SIGI(8)),RIWS(SIGI(8)))
          NB=NB+12
        ENDIF
      ENDIF
      WRITE(CUUBB(NB:NB),'(A)',IOSTAT=IOS)'='

C
C    ++++ NOW WRITE IT OUT 65 CHARACTERS PER LINE ++++
C
      KP=60
      IF (KP.GT.NB) KP=NB
      WRITE(11,'(A)',IOSTAT=IOS) 'UUBB '//CUUBB(1:KP)
      DO WHILE (KP.LT.NB)
        MP=KP+1
        KP=KP+60
        IF (KP.GT.NB) KP=NB
        WRITE(11,'(A)',IOSTAT=IOS)'     '//CUUBB(MP:KP)
      ENDDO
      GOTO 49
   37 CONTINUE
      WRITE(11,'(A)',IOSTAT=IOS) ' Invalid Times '
   49 CONTINUE
      CLOSE(11,IOSTAT=IOS)
      RETURN
      END
C*******************************************************************************
      REAL*4 FUNCTION DEWP(IS)
C
C GENERAL EASTERN DEWPOINT, IF CONTROL CONDITION RETURN 500
C IF .GT. TEMP RETURN TEMP
C
      IMPLICIT  NONE
      INTEGER*4 IS
      INCLUDE 'HCOM_DEF.FOR'

      DEWP=500.0
      IF ((IS.GE.1).AND.(IS.LE.12800)) THEN
        IF (SPECIAL(IS,5).EQ.0) THEN 
          DEWP=RDEW(IS)
          IF (DEWP.GT.RTATDC(IS)) DEWP=RTATDC(IS)
        ENDIF
      ENDIF
      RETURN
      END
C*******************************************************************************
      CHARACTER*5 FUNCTION DDFFF(D,F)
C
C CALCULATE DIRECTION AND FORCE 5 FIGURE CODE FROM D AND F WIND
C COMPONENTS
C
      IMPLICIT NONE
      REAL*4 D,F
      INTEGER*4 DD,FF,IOS

      DD=500*NINT(D/5.0)
C    TO KNOTS ?
      FF=NINT(F*1.96)
      WRITE(DDFFF,'(I5.5)',IOSTAT=IOS) DD+FF
      RETURN
      END
C*******************************************************************************
      CHARACTER*5 FUNCTION TTTDD(T,D)
C
C TEMPERATURE AND DEWPOINT 5 FIGURE CODE FROM TEMP AND DEWPOINT
C     
      IMPLICIT NONE
      REAL*4 T,D,DP
      INTEGER*4 MOD,TTT,DD,IOS

      IF ((T.LT.-80).OR.(T.GT.60)) THEN
        TTTDD='/////'
      ELSE
        TTT=NINT((10.0*T))
        IF (TTT.LT.0) THEN 
          TTT=-TTT
          IF ((MOD(TTT,2)).NE.1) TTT=TTT+1
        ELSE
          IF ((MOD(TTT,2)).NE.0) TTT=TTT+1
        ENDIF
        IF (D.GT.400) THEN
          WRITE(TTTDD,'(I3.3,A2)',IOSTAT=IOS) TTT,'//'
        ELSE
          DP=T-D
          IF (DP.LT.0) DP=0
          IF (DP.LE.5) THEN 
            DD=NINT(DP*10.0)
          ELSE
            IF (DP.LT.49.5) THEN
              DD=NINT(DP+50)
            ELSE
              DD=99
            ENDIF
          ENDIF
          WRITE(TTTDD,'(I5.5)',IOSTAT=IOS) TTT*100+DD
        ENDIF
      ENDIF
      RETURN
      END
C*******************************************************************************
      INTEGER*4 FUNCTION LTD(P)
C
C GET LAST THREE DIGITS OF A NUMBER
C
      IMPLICIT NONE
      INTEGER*4 MOD
      REAL*4 P

      LTD=MOD(NINT(P),1000)
      RETURN
      END
C*******************************************************************************
      CHARACTER*5 FUNCTION PPHHH(P,H)
C
C CREATE STANDARD LEVEL 5 FIGURE CODES FROM PRESSURE AND HEIGHT
C
      IMPLICIT NONE
      INTEGER*4 PP,HHH,LTD,IOS
      REAL*4 P,H

      PP=LTD(P)/10
      IF (P.GT.600) THEN
        IF (H.LT.0) THEN
          HHH=LTD(500-H) 
        ELSE 
          HHH=LTD(H)
        ENDIF
      ELSE
        HHH=LTD(H/10)
      ENDIF
      WRITE(PPHHH,'(I5.5)',IOSTAT=IOS)PP*1000+HHH
      RETURN
      END
C*******************************************************************************
      CHARACTER*5 FUNCTION QC(LAT,LON)
C
C GLOBE QUANDRANT AND LONGITUDE
C
      IMPLICIT NONE
      REAL*4 LAT,LON
      INTEGER*4 Q,IOS

      Q=1
      IF (LON.LT.0.0) THEN
        Q=7
        IF (LAT.LT.0.0) Q=5
      ELSE
        IF (LAT.LT.0.0) Q=3
      ENDIF      
      WRITE(QC,'(I1.1,I4.4)',IOSTAT=IOS) Q,NINT(ABS(LON)*10.0)
      RETURN
      END
C*******************************************************************************
      CHARACTER*5 FUNCTION MARSDEN(LAT,LON)
C
C THE MARSDEN SQUARE WITH LAT AND LON UNITS
C
      IMPLICIT NONE
      REAL*4 LAT,LON,LNG
      INTEGER*4 M,IOS

      LNG=-LON
      M=301
      IF (LAT.GT.0.0) M=1
      IF (LNG.LT.0) LNG=LNG+360.0
      M=M+INT(LNG/10)+36*INT(ABS(LAT)/10)
      WRITE(MARSDEN,'(I3.3,I1.1,I1.1)',IOSTAT=IOS)M,
     +MOD(INT((NINT(ABS(LAT*10)))/10),10),
     +MOD(INT((NINT(ABS(LON*10)))/10),10)
      RETURN
      END
C*******************************************************************************
      CHARACTER*5 FUNCTION YYGGI(Y,G,I,SIGP)
C
C     THE DATE AND TIME GROUP
C
      IMPLICIT NONE
      INTEGER*4 Y,G,I,IOS
      REAL*4 SIGP(8)

      IF (I.GE.0) THEN
        WRITE(YYGGI,'(I2.2,I2.2,I1.1)',IOSTAT=IOS) 
     +Y,G,MOD(NINT((SIGP(I)-25)/100),10) 
      ELSE
        WRITE(YYGGI,'(I2.2,I2.2,A)',IOSTAT=IOS) Y,G,'/'
      ENDIF
      RETURN
      END
C*******************************************************************************
      INTEGER*4 FUNCTION ID(SIGD,SGD)
C
C Find the highest significant point
C
      IMPLICIT NONE
      REAL*4 SIGD(8),SGD
      INTEGER*4 KP,F

      KP=0
      DO F=1,6
        IF (SIGD(F).LT.SGD) KP=F
      ENDDO
      IF (KP.EQ.0) THEN
        ID=-1
      ELSE
        ID=KP
      ENDIF
      RETURN
      END
