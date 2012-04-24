C      PROGRAM H_JAVAMAP
C
C     Loops to check the aircraft position every 3 seconds.  
C     If it is outside the MAP bounds call CLINE.FOR to 
C     recreate MAP.
C
C
C
      PROGRAM H_JAVAMAP

      implicit none
      REAL*4 range(4),olat,olon,dlat,dlon
      integer*4 stat2,I
      logical notchange
      INCLUDE 'HCOM_DEF.FOR'

      stat2=status(2)
      do while(stat2.lt.1.or.stat2.gt.2)
        call lib$wait(3.0)
        stat2=status(2)
      enddo
      range(1)=derive(stat2,94)
      range(2)=derive(stat2,93)
      range(3)=derive(stat2,94)
      range(4)=derive(stat2,93)
      olon=range(1)
      olat=range(2)
      DO I=stat2,1,-1
          dlon=olon-derive(I,94)
          dlat=olat-derive(I,93)
        if(((dlat.LT.0.2).AND.(dlat.GT.-0.2)).AND.
     &     (((dlon.LT.0.2).AND.(dlon.GT.-0.2)).OR.
     &     ((dlon.GT.179.8).AND.(dlon.LT.180.2)).OR.
     &     ((dlon.GT.179.8).AND.(dlon.LT.180.2)).OR.
     &     ((dlon.GT.359.8).AND.(dlon.LT.360.2)).OR.
     &     ((dlon.LT.-359.8).AND.(dlon.GT.-360.2))))THEN
          olon=derive(I,94)
          olat=derive(I,93)
          if(range(1).GT.derive(I,94))range(1)=derive(I,94)
          if(range(2).GT.derive(I,93))range(2)=derive(I,93)
          if(range(3).LT.derive(I,94))range(3)=derive(I,94)
          if(range(4).LT.derive(I,93))range(4)=derive(I,93)
        ENDIF
      ENDDO
      DO WHILE(.true.)
      range(1)=range(1)-15.0
      range(2)=range(2)-15.0
      range(3)=range(3)+15.0
      range(4)=range(4)+15.0
!      print *,range
      CALL cline(range)
      notchange=.TRUE.
      I=status(2)
      olon=derive(I,94)
      olat=derive(I,93)
      DO WHILE(notchange)
        I=status(2)
        dlat=olat-derive(I,94)
        dlon=olon-derive(I,93)
        if(((dlat.LT.0.2).AND.(dlat.GT.-0.2)).AND.
     &     (((dlon.LT.0.2).AND.(dlon.GT.-0.2)).OR.
     &     ((dlon.GT.179.8).AND.(dlon.LT.180.2)).OR.
     &     ((dlon.GT.179.8).AND.(dlon.LT.180.2)).OR.
     &     ((dlon.GT.359.8).AND.(dlon.LT.360.2)).OR.
     &     ((dlon.LT.-359.8).AND.(dlon.GT.-360.2))))THEN
          olat=derive(I,94)
          olon=derive(I,93)
          if(range(1).GT.derive(I,94))THEN
            range(1)=derive(I,94)
            notchange=.FALSE.
          ENDIF
          if(range(2).GT.derive(I,93))THEN
            range(2)=derive(I,93)
            notchange=.FALSE.
          ENDIF
          if(range(3).LT.derive(I,94))THEN
            range(3)=derive(I,94)
            notchange=.FALSE.
          ENDIF
          if(range(4).LT.derive(I,93))THEN
            range(4)=derive(I,93)
            notchange=.FALSE.
          ENDIF
        ENDIF
        IF(notchange)CALL LIB$WAIT(3.0)
      ENDDO
      ENDDO
      END  
C*************************************************
C      SUBROUTINE CLINE(RANGE)
C
C     Creates a new MAP_DATA.DAT by extracting just
C     data in the RANGE from MO2.XDR
C
C
C
C
C
      SUBROUTINE CLINE(RANGE)

      IMPLICIT none
      INTEGER*4 NSEG,TINDX(3,5000),NPTS,ISTAT,I,POINT
      INTEGER*4 RDINT,I2,N
      REAL*4 DATA(2,500000)
      REAL*4 RANGES(4,5000),RANGE(4),RDFLT
      CHARACTER*512 CBUFF

      OPEN(11,FILE='H_JAVAMAP.XDR',STATUS='OLD',
     &    FORM='UNFORMATTED',IOSTAT=ISTAT,
     &    RECORDTYPE='FIXED',ACCESS='SEQUENTIAL',
     &    ORGANIZATION='SEQUENTIAL',CARRIAGECONTROL='NONE')


C      RANGE(1)=-10.0
C      RANGE(2)=45.0
C      RANGE(3)=10.0
C      RANGE(4)=65.0
      IF(ABS(RANGE(1)-RANGE(3)).GT.300.0)THEN
        IF(RANGE(1).LT.0.0)RANGE(1)=RANGE(1)+360.0
        IF(RANGE(3).LT.0.0)RANGE(3)=RANGE(3)+360.0
      ENDIF
      POINT=1
      READ(11)CBUFF
      NSEG=RDINT(11,CBUFF,POINT)
      DO I=1,NSEG
        TINDX(1,I)=RDINT(11,CBUFF,POINT)
        TINDX(2,I)=RDINT(11,CBUFF,POINT)
        TINDX(3,I)=RDINT(11,CBUFF,POINT)
C        print *,TINDX(1,I),TINDX(2,I),TINDX(3,I)
      ENDDO
      NPTS=RDINT(11,CBUFF,POINT)
!      print *,npts
      N=0
      DO I=1,NPTS
        data(1,I)=RDFLT(11,CBUFF,POINT)
        data(2,I)=RDFLT(11,CBUFF,POINT)                                       
        IF(I.EQ.(TINDX(1,N+1)+1))THEN
          IF(I.GT.1)THEN
            IF((RANGES(1,N).LT.-150.0).AND.
     &  (RANGES(3,N).GT.150.0))RANGES(1,N)=RANGES(1,N)+360.0
          ENDIF
          N=N+1
          RANGES(1,N)=data(1,I)
          RANGES(2,N)=data(2,I)
          RANGES(3,N)=data(1,I)
          RANGES(4,N)=data(2,I)
        ELSE
          IF(RANGES(1,N).GT.data(1,I))RANGES(1,N)=data(1,I)
          IF(RANGES(2,N).GT.data(2,I))RANGES(2,N)=data(2,I)
          IF(RANGES(3,N).LT.data(1,I))RANGES(3,N)=data(1,I)
          IF(RANGES(4,N).LT.data(2,I))RANGES(4,N)=data(2,I)
        ENDIF
      ENDDO
      CLOSE(11)
      
      OPEN(11,FILE='SYS$SYSDEVICE:[HORACE.WEB.PLOT]MAP_DATA.DAT',
     &    STATUS='NEW',
     &    RECORDTYPE='VARIABLE',FORM='UNFORMATTED',IOSTAT=ISTAT,
     &    ORGANIZATION='SEQUENTIAL',CARRIAGECONTROL='NONE')
      CALL WRITEINT(11,IFIX(RANGE(1)*100.0))
      CALL WRITEINT(11,IFIX(RANGE(2)*100.0))
      CALL WRITEINT(11,IFIX(RANGE(3)*100.0))
      CALL WRITEINT(11,IFIX(RANGE(4)*100.0))
      DO I=1,NSEG
        if((ranges(1,I).LE.range(3)).AND.
     &     (ranges(2,I).LE.range(4)).AND.
     &     (ranges(3,I).GE.range(1)).AND.
     &     (ranges(4,I).GE.range(2)))THEN
!          print *,ranges(1,I),ranges(2,I),ranges(3,I),ranges(4,I)
          N=TINDX(2,I)-TINDX(1,I)+1
          CALL WRITEINT(11,N)
          DO I2=TINDX(1,I)+1,TINDX(2,I)+1
            CALL WRITEINT(11,IFIX(data(1,I2)*100.0))
            CALL WRITEINT(11,IFIX(data(2,I2)*100.0))
          ENDDO
        ENDIF
      ENDDO

      CLOSE(11)
      END
C********************************************************        
C
C     FUNCTION RDINT(IUNIT,CBUFF,POINT)
C
C     Reads an integer with swapped bytes from a string CBUFF
C
C
      INTEGER*4 FUNCTION RDINT(IUNIT,CBUFF,POINT)

        CHARACTER*512 CBUFF
        CHARACTER*4 C
        INTEGER*4 I,POINT,IUNIT
        EQUIVALENCE(I,C)

        C=CBUFF(POINT+3:POINT+3)//CBUFF(POINT+2:POINT+2)//
     &    CBUFF(POINT+1:POINT+1)//CBUFF(POINT:POINT)
        POINT=POINT+4
        IF(POINT.GT.512)THEN
          POINT=1
          READ(IUNIT)CBUFF
        ENDIF
        RDINT=I
      RETURN
      END

C********************************************************        
C
C     FUNCTION RDINT(IUNIT,CBUFF,POINT)
C
C     Reads a floating point number with swapped bytes from a string CBUFF
C
C

      REAL*4 FUNCTION rdflt(iunit,cbuff,point)

        CHARACTER*512 CBUFF
        CHARACTER*4 C
        REAL*4 R,R2
        INTEGER*4 POINT,IUNIT
        EQUIVALENCE(R,C)
      INCLUDE '($CVTDEF)'

        C=CBUFF(POINT:POINT+3)
        POINT=POINT+4
        IF(POINT.GT.512)THEN
          POINT=1
          READ(IUNIT)CBUFF
        ENDIF
        
      CALL CVT$FTOF(R,%VAL(CVT$K_BIG_ENDIAN_IEEE_S),R2,
     &                %VAL(CVT$K_VAX_F),)
      RDFLT=R2
      RETURN
      END

C********************************************************        
C
C     SUBROUTINE WRITEINT(IUNIT,I)
C
C     Writes an integer with swapped bytes to unit IUNIT
C
C
      SUBROUTINE WRITEINT(IUNIT,I)

        INTEGER*4 I,IUNIT
        INTEGER*2 I2
        CHARACTER*2 C2
        EQUIVALENCE (I2,C2)
        I2=I
C        PRINT *,I2
C        WRITE(IUNIT)I2
        WRITE(IUNIT)C2(2:2)//C2(1:1)
      RETURN
      END


