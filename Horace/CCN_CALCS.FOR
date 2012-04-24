C V1.01  28/06/94  W.D.N.JACKSON
C        Redundant variables removed
C-------------------------------------------------------------------------------
C ROUTINE          FORTVAX            READ_CCN_CONSTS
C
C Reads from the file CCN_CALIB.DAT the calibration constants used to calculate
C the CCN count.
C
C They are placed in common block CCN_CONSTS_COM
C
C N R Jerram May'91
C-------------------------------------------------------------------------------
      SUBROUTINE READ_CCN_CONSTS
      IMPLICIT NONE
      INTEGER*4 IOS
      REAL*4 Y_CAL, X_CAL, XX_CAL, VOL

      COMMON /CCN_CONSTS_COM/ Y_CAL, X_CAL, XX_CAL, VOL 
C
C Open file for reading:

      OPEN (UNIT=20, FILE='CCN_CALIB.DAT', STATUS='OLD',
     -      ERR=10, READONLY)
C
C Ignore first four lines:

      READ(20,'(////)',ERR=10)
C
C Read constants:

      READ(20,'(10X,F8.5)',ERR=10)Y_CAL
      READ(20,'(10X,F8.5)',ERR=10)X_CAL
      READ(20,'(10X,F8.5)',ERR=10)XX_CAL
      READ(20,'(10X,F8.5)',ERR=10)VOL
      CLOSE(UNIT=20,ERR=10)
      RETURN
C
C Default constants in case of file read error:

   10 CLOSE(UNIT=20,IOSTAT=IOS)
      Y_CAL = 0.0
      X_CAL = 76.52348
      XX_CAL= 22.86818
      VOL   = 0.04556
      RETURN
      END


C-------------------------------------------------------------------------------
C ROUTINE          FORTVAX            CCN_CALCS 
C
C Handles raw CCN data, places the computed results in the global common block
C CCNCOM.
C This routine is called by H_DERIVE once a second.
C
C ARGUMENTS: Output: INPUT_SECOND  INTEGER*2 (32)  Contains raw CCN data for
C                                                  one second.
C
C N.R.Jerram May'91      
C-------------------------------------------------------------------------------
      SUBROUTINE CCN_CALCS
 
      IMPLICIT NONE
      INTEGER*4 I,J,IS,ICOUNT
      INTEGER*2  INPUT_SECOND(32)
      LOGICAL*4  START /.TRUE./, LDATA

      STRUCTURE /AIR_SAMPLE/
       REAL*4 CCN_TI(10), CCN_TT(10), CCN_T1(10), CCN_T2(10),
     -        CCN_T3(10), CCN_T4(10), CCN_REF(10),
     -        CCN_PEAK(10), CCN_SSAT_ST(10), CCN_COUNT_ST(10), 
     -        CCN_SSAT_DY(300), CCN_DET_DY(300)
       INTEGER*4 CCN_TIME 
       INTEGER*4 IDTEMPS
       INTEGER*4 ISTEP
      END STRUCTURE

      RECORD /AIR_SAMPLE/ SAMPLE(5)

      COMMON /CCNCOM/ SAMPLE, ICOUNT
      VOLATILE /CCNCOM/
      INCLUDE 'HCOM_DEF.FOR'
C
C If at start of processing, reset CCN common block to zeroes:

      IF(START)THEN
       DO J=1,5
        DO I=1,10
         SAMPLE(J).CCN_TI(I)  = 0.0
         SAMPLE(J).CCN_TT(I)  = 0.0
         SAMPLE(J).CCN_T1(I)  = 0.0
         SAMPLE(J).CCN_T2(I)  = 0.0
         SAMPLE(J).CCN_T3(I)  = 0.0
         SAMPLE(J).CCN_T4(I)  = 0.0
         SAMPLE(J).CCN_REF(I) = 0.0
         SAMPLE(J).CCN_PEAK(I)= 0.0
         SAMPLE(J).CCN_SSAT_ST(I)  = 0.0
         SAMPLE(J).CCN_COUNT_ST(I) = 0.0
        END DO

        DO I=1,300
         SAMPLE(J).CCN_SSAT_DY(I) = 0.0
         SAMPLE(J).CCN_DET_DY(I)  = 0.0
        END DO

        SAMPLE(J).CCN_TIME = 0
        SAMPLE(J).IDTEMPS  = 0
   
       END DO

       ICOUNT = 0
       START  = .FALSE.
      END IF
       
      LDATA=.FALSE.
      DO IS = 1, 32
       INPUT_SECOND(IS)=NFDATA(NPLOC(140)+IS-1,STATUS(1)).AND.'FFF'X
       IF(INPUT_SECOND(IS).LT.4095) LDATA=.TRUE.
      END DO

      IF(LDATA) CALL CCN_DEM_SECOND (INPUT_SECOND)

      RETURN
      END

C-----------------------------------------------------------------------------
C ROUTINE          CCN_DEM_SECOND
C
C PURPOSE          DEMULTIPLEX ONE SECOND OF CCN DATA
C
C METHOD           The data is sorted into packets. Each packet contains a
C                  number of different CCN parameters, and is preceded by
C                  the flag value 4093.
C
C ARGUMENTS        INPUT:  INPUT_SECOND     Integer*2 (32)
C                          Containing one seconds' worth of data.
C                  OUTPUT: DATA_PACKET      Integer*2 (11)
C
C SUBPROGRAMS      READ_PACKET
C
C N R JERRAM 22-5-91
C-----------------------------------------------------------------------------
      SUBROUTINE CCN_DEM_SECOND (INPUT_SECOND)
      IMPLICIT NONE
      REAL*4 Y_CAL, X_CAL, XX_CAL, VOL
      INTEGER*2 INPUT_SECOND(32), DATA_PACKET(11)
      INTEGER*4 IPOS /0/,I,IPTYPE,ILEN,J
      LOGICAL*4 OSTARTED /.FALSE./
      COMMON /CCN_CONSTS_COM/ Y_CAL, X_CAL, XX_CAL, VOL 
C
C The loop executes 32 times a second, once for each input datum.

      DO I=1,32

       IF(INPUT_SECOND(I).GE.4093)THEN
                         
        IPTYPE = DATA_PACKET(2)/100
        ILEN = 3                
        IF(IPTYPE.EQ.0 .OR.
     -     IPTYPE.EQ.1 .OR.
     -     IPTYPE.EQ.9 .OR.
     -     IPTYPE.EQ.18) ILEN = 11
        IF(IPTYPE.EQ.8 .OR.
     -     IPTYPE.EQ.19) ILEN = 4
        IF(IPTYPE.EQ.10) ILEN = 9        

        IF(IPOS .EQ. ILEN)THEN    !Packet is not cut short

         IF(IPOS.LT.11)THEN                 !If not a full lenth packet, 
          DO J=IPOS+1,11                    ! fill the remaining space with 0's.
           DATA_PACKET(J) = 0
          END DO
         END IF

         IF(OSTARTED)THEN
          CALL CCN_READ_PACKET(DATA_PACKET)
          OSTARTED = .FALSE.
         END IF

        END IF

        IF(INPUT_SECOND(I).EQ.4093)THEN
         IPOS = 1
         DATA_PACKET(IPOS) = INPUT_SECOND(I)
         OSTARTED = .TRUE.
        END IF

       ELSE IF(INPUT_SECOND(I).LT.4093)THEN

        IPOS = IPOS+1
        IF (IPOS.LT.12) DATA_PACKET(IPOS) = INPUT_SECOND(I)

       END IF

      END DO
      RETURN
      END

C-----------------------------------------------------------------------------
C ROUTINE          READ_PACKET
C
C PURPOSE          CARRY OUT CCN PROCESSING DEPENDING ON PACKET TYPE
C
C METHOD           The time and packet type are extracted. Various values
C                  are stored depending on the data and the packet type.
C
C ARGUMENTS        INPUT: DATA_PACKET      Integer*2 (11)   
C                  OUTPUT: 
C                   Peak detection (0):
C                     CCN_TI        Inlet temperature (packet with highest peak)
C                     CCN_TT        Top plate temperature                   "
C                     CCN_T1        Bottom plate temperature thermistor 1   "
C                     CCN_T2           "  " 2                               "
C                     CCN_T3           "  " 3                               "
C                     CCN_T4           "  " 4                               "
C                     CCN_REF       Reference                               "
C                     CCN_PEAK      Highest detector value in Peak Detection
C                     CCN_SSAT_ST   Super saturation calculated from above vars.
C                     CCN_COUNT_ST  CCN Count
C                   Alleviator off (17)
C                     CCN_TIME      Sample time
C                   Dynamic mode (18)
C                     CCN_SSAT_DY  Supersaturation for graph plotting 
C                     CCN_DET_DY   Detector reading for graph plotting
C
C                   All the above output variables are sent to the routine 
C                   LOAD_COMMON 
C
C SUBPROGRAMS      LOAD_COMMON called at the end of each sample, on reaching
C                  packet type 15.
C
C DESCRIPTION      Data from the CCN, raw parameter 140; is represented in
C                  groups (packets) of 3, 4, 9 or 11 values.
C
C                   The size of a packet depends on what type of packet it is,
C                  and that depends on the part of the CCN operating procedure
C                  currently under progress.
C
C                  All 11 elements exist only in types 0, 1 ,9 or 18.
C
C                  There are currently 19 types of data packet, these are
C                  outlined below:
C
C                  0: Peak detection                11: Valve filter error
C                  1: Baselining                    12: Valve purge error
C                  2: Valve to filter               13: Valve closed error
C                  3: Valve to purge                14: Valve unknown error
C                  4: Valve closed                  15: New air sample
C                  5: Pump off                      16: Alleviator on
C                  6: Pump slow                     17: Alleviator off
C                  7: Pump fast                     18: Dynamic mode
C                  8: Delta temperature set         19: Temperature calibration
C                  9: Camera fired
C                 10: Thermistor channel substitution
C
C                 Data packet structure:
C                   
C                 All packets    :4093 ppss hhmm (TI TT T1 T2 T3 T4 Ref Det)
C                 Except :-
C                 Packet type  8 :4093 08ss hhmm Td
C                 Packet type 10 :4093 10ss hhmm Tc0 Tc1 Tc2 Tc3 Tc4 Tc5
C                 Packet type 19 :4093 19ss hhmm { 0000 = Temps calibrated    }
C                                                { 0255 = Temps not calibrated}
C
C                 A lot the the above packets are ignored since they will make
C                 no difference to Horace CCN processing. These are used only
C                 in the ground processing program (written by yours truly)
C
C                 Apart from the packets mentioned in the ARGUMENTS heading
C                 above, the only ones used here are:
C                  1: Baselining : An average of detector values is taken
C                                  during this phase and the resultant variable
C                                  RBASELINE is used in the calculation of the
C                                  CCN count.
C                 10: Thermistors: Used when a value from one thermistor is
C                                  suspect and so substitutes another. Used
C                                  in the calculation of supersaturation SSAT.
C                 15: New air :    LOAD_COMMON is called.
C N R JERRAM 22-5-91
C-----------------------------------------------------------------------------
      SUBROUTINE CCN_READ_PACKET (DATA_PACKET)
      IMPLICIT  NONE
      INTEGER*4 ICYCLES/0/, ICT/0/,IPTYPE,ISS,IHH,IMM,IDCYCLES,I,ITOTAL,
     &    ICOUNT
      INTEGER*2 DATA_PACKET(11)
      LOGICAL*4 OERROR
      LOGICAL*4 PEAK_END /.FALSE./
      REAL*4    RTEMPS(4), AVTEMP, SCALED_DET
      REAL*4    TCCN_TT,TCCN_TI,TCCN_T1,TCCN_T2,TCCN_T3,TCCN_T4,
     &    TCCN_REF,TCCN_PEAK,TCCN_SSAT_ST,TCCN_COUNT_ST,REFERENCE,
     &    SUPERSAT,BASELINE,MREF,STEP,BASE
C Packet contents:

      REAL*4    TI, TT, T1, T2, T3, T4, REF, DET, PEAK
      INTEGER*4 IT(-1:4) 

C
C CCN Calibration constants:

      REAL*4 Y_CAL, X_CAL, XX_CAL, VOL
C
C Air sample data structure:

      STRUCTURE /AIR_SAMPLE/
       REAL*4 CCN_TI(10), CCN_TT(10), CCN_T1(10), CCN_T2(10),
     -        CCN_T3(10), CCN_T4(10), CCN_REF(10),
     -        CCN_PEAK(10), CCN_SSAT_ST(10), CCN_COUNT_ST(10), 
     -        CCN_SSAT_DY(300), CCN_DET_DY(300)
       INTEGER*4 CCN_TIME
       INTEGER*4 IDTEMPS
       INTEGER*4 ISTEP
      END STRUCTURE

      RECORD /AIR_SAMPLE/ OUTSAMPLE(5)
      COMMON /CCNCOM/ OUTSAMPLE, ICOUNT
      VOLATILE /CCNCOM/
      COMMON /CCN_CONSTS_COM/ Y_CAL, X_CAL, XX_CAL, VOL 
C
C Start of processing: Set ICOUNT to 0

      IF(ICT.EQ.0)THEN
       ICOUNT = 0
       ICT    = 1
      END IF
C
C Start of packet: Label, type, time:


      IF(DATA_PACKET(1).NE.4093)THEN
       OERROR = .TRUE.
      ELSE
       OERROR = .FALSE.
      END IF

      IPTYPE = DATA_PACKET(2)/100
      ISS    = DATA_PACKET(2)-(IPTYPE*100)
      IHH    = DATA_PACKET(3)/100
      IMM    = DATA_PACKET(3)-(IHH*100)

      IF(IPTYPE.GT.19 .OR. ISS.GT.59 .OR. IMM.GT.59 .OR. IHH.GT.23)
     -   OERROR = .TRUE.

      IF(.NOT.OERROR)THEN
C===============================================================================
 
       IF(IPTYPE.EQ.0 .OR. IPTYPE.EQ.1 .OR. IPTYPE.EQ.18)THEN !Full packet
        TI  = REAL(DATA_PACKET(4))/100.0
        TT  = REAL(DATA_PACKET(5))/100.0
        T1  = REAL(DATA_PACKET(6))/100.0
        T2  = REAL(DATA_PACKET(7))/100.0
        T3  = REAL(DATA_PACKET(8))/100.0
        T4  = REAL(DATA_PACKET(9))/100.0
        REF = REAL(DATA_PACKET(10))
        DET = REAL(DATA_PACKET(11))
       END IF

C
C----------------------------------------------------------------------------
C                         PEAK DETECTION
C Store the packet containing the detector peak in arrays, and calculate
C CCN count and supersaturation.
C----------------------------------------------------------------------------
       IF(IPTYPE.NE.0)THEN

        PEAK = 0.0
        IF(PEAK_END)THEN
C mod to remove chance of program crashing due to subscript out of range errors
C David Lauchlan 21.3.94
          IF (ICOUNT.GT.0.AND. ICOUNT.LE.5
     -      .AND.ICYCLES.GT.0.AND.ICYCLES.LE.10)THEN
            OUTSAMPLE(ICOUNT).CCN_TI(ICYCLES) = TCCN_TI
            OUTSAMPLE(ICOUNT).CCN_TT(ICYCLES) = TCCN_TT
            OUTSAMPLE(ICOUNT).CCN_T1(ICYCLES) = TCCN_T1
            OUTSAMPLE(ICOUNT).CCN_T2(ICYCLES) = TCCN_T2
            OUTSAMPLE(ICOUNT).CCN_T3(ICYCLES) = TCCN_T3
            OUTSAMPLE(ICOUNT).CCN_T4(ICYCLES) = TCCN_T4
            OUTSAMPLE(ICOUNT).CCN_REF(ICYCLES) = TCCN_REF
            OUTSAMPLE(ICOUNT).CCN_PEAK(ICYCLES) = TCCN_PEAK
            OUTSAMPLE(ICOUNT).CCN_SSAT_ST(ICYCLES) = TCCN_SSAT_ST
            OUTSAMPLE(ICOUNT).CCN_COUNT_ST(ICYCLES)  = TCCN_COUNT_ST
            OUTSAMPLE(ICOUNT).IDTEMPS = ICYCLES
          END IF
          PEAK_END = .FALSE.
        END IF
 
       ELSE 

        IF(REFERENCE.NE.0.0 .AND. ICOUNT.GT.0)THEN

         IF(DET.GT.PEAK .AND. ICYCLES.LT.10)THEN        
          IF(PEAK.EQ.0.0)ICYCLES = ICYCLES+1
          TCCN_TI                = TI
          TCCN_TT                = TT
          TCCN_T1                = T1
          RTEMPS(1)              = T1
          TCCN_T2                = T2
          RTEMPS(2)              = T2
          TCCN_T3                = T3
          RTEMPS(3)              = T3
          TCCN_T4                = T4
          RTEMPS(4)              = T4
          TCCN_REF               = REF
          TCCN_PEAK              = DET
          PEAK                   = DET
C
C Calculate supersaturation:
C (The array IT contains thermistor substitutions.
C  The usual contents is: IT(1)=1, IT(2)=2, etc., but in case of a
C  substitution, one will contain a different value)

          AVTEMP = 0.0

          DO I=1,4                              !Bottom plate thermistors being
           IF (IT(I).LT.2 .OR. IT(I).GT.5) IT(I)=I+1  ! zero is NOT a good idea.
           AVTEMP = AVTEMP+RTEMPS(IT(I)-1)
          END DO

        
          AVTEMP = AVTEMP/4.0
          TCCN_SSAT_ST = SUPERSAT(AVTEMP, TT)
C
C Calculate CCN count:

          SCALED_DET = (TCCN_PEAK - BASELINE)
     -                 /REFERENCE
          IF(VOL.NE.0.0)THEN
           TCCN_COUNT_ST = (Y_CAL + (X_CAL*SCALED_DET) + 
     -                     (XX_CAL*(SCALED_DET**2)))/VOL
          END IF

         END IF ! detector > peak 
         PEAK_END = .TRUE.
        END IF !REFERENCE <> 0  AND ICOUNT > 0
       END IF  ! Packet type=0 and OBASE
C-----------------------------------------------------------------------
C                        BASELINING
C Calculate the mean detector and reference values. These are used in
C calculating CCN count and supersaturation during Peak Detection.
C-----------------------------------------------------------------------
       IF (IPTYPE.NE.1) THEN

        BASE   = 0.0
        MREF   = 0.0
        ITOTAL = 0

       ELSE

        ITOTAL = ITOTAL+1
        BASE   = BASE+DET
        BASELINE = BASE/REAL(ITOTAL)
        MREF   = MREF+REF
        REFERENCE = MREF/REAL(ITOTAL)

       END IF
C-----------------------------------------------------------------------
C                       THERMISTORS
C  Get values for the array IT from the data packet 
C-----------------------------------------------------------------------
       IF(IPTYPE.EQ.10)THEN

        DO I=-1, 4
         IT(I) = DATA_PACKET(I+5)
        END DO
        
       END IF
C-----------------------------------------------------------------------
C                   NEW AIR SAMPLE
C  Call LOAD_COMMON, reset storage arrays and store time.
C-----------------------------------------------------------------------
       IF(IPTYPE.EQ.17)THEN
C
C Increment air sample count:

        ICOUNT = ICOUNT+1                !Count of number of spactra in storage
        STEP   = STEP+1
        IF(STEP.EQ.6)STEP = 1

C Prepare arrays/variables for next sample:

        IF(ICOUNT.GT.5)THEN
         ICOUNT       = 5
         OUTSAMPLE(1) = OUTSAMPLE(2)
         OUTSAMPLE(2) = OUTSAMPLE(3)
         OUTSAMPLE(3) = OUTSAMPLE(4)
         OUTSAMPLE(4) = OUTSAMPLE(5)
        END IF

        DO I=1,10
         OUTSAMPLE(ICOUNT).CCN_TI(I)  = 0.0
         OUTSAMPLE(ICOUNT).CCN_TT(I)  = 0.0
         OUTSAMPLE(ICOUNT).CCN_T1(I)  = 0.0
         OUTSAMPLE(ICOUNT).CCN_T2(I)  = 0.0
         OUTSAMPLE(ICOUNT).CCN_T3(I)  = 0.0
         OUTSAMPLE(ICOUNT).CCN_T4(I)  = 0.0
         OUTSAMPLE(ICOUNT).CCN_REF(I) = 0.0
         OUTSAMPLE(ICOUNT).CCN_PEAK(I)= 0.0
         OUTSAMPLE(ICOUNT).CCN_SSAT_ST(I)  = 0.0
         OUTSAMPLE(ICOUNT).CCN_COUNT_ST(I) = 0.0
        END DO

        DO I=1,300
         OUTSAMPLE(ICOUNT).CCN_SSAT_DY(I) = 0.0
         OUTSAMPLE(ICOUNT).CCN_DET_DY(I)  = 0.0
        END DO

        OUTSAMPLE(ICOUNT).CCN_TIME = (IHH*10000) + (IMM*100) + ISS
        OUTSAMPLE(ICOUNT).ISTEP    = STEP
        ICYCLES = 0

       END IF
C----------------------------------------------------------------------------
C                      DYNAMIC MODE
C For every packet: calculate and store Supersaturation, store Detector value
C in SAMPLE.CCN arrays.
C---------------------------------------------------------------------------- 
       IF(IPTYPE.NE.18)THEN

        IDCYCLES = 0

       ELSE

        IF(ICOUNT.GT.0)THEN
         IDCYCLES = IDCYCLES+1
C
C Calculate supersaturation:
C (The array IT contains thermistor substitutions.
C  The usual contents is: IT(1)=1, IT(2)=2, etc., but in case of a
C  substitution, one will contain a different value)

         RTEMPS(1) = T1
         RTEMPS(2) = T2
         RTEMPS(3) = T3
         RTEMPS(4) = T4

         AVTEMP = 0.0
         DO I=1,4
          IF (IT(I).LT.2 .OR. IT(I).GT.5) IT(I)=I+1 
          AVTEMP = AVTEMP+RTEMPS(IT(I)-1)
         END DO
         AVTEMP = AVTEMP/4.0
         IF(IDCYCLES.LE.300)OUTSAMPLE(ICOUNT).CCN_SSAT_DY(IDCYCLES) = 
     -                                              SUPERSAT(AVTEMP, TT)
         IF(IDCYCLES.LE.300)OUTSAMPLE(ICOUNT).CCN_DET_DY(IDCYCLES) = DET

        END IF !ICOUNT>0       
       END IF   !Packet type = 18
C===============================================================================
      END IF    !Not error.

      RETURN
      END
C*******************************************************************************
      REAL*4 FUNCTION SUPERSAT (R_AVTEMP,R_TT)
C
C PURPOSE  : calclulate super-saturation level
C
C ARGUMENTS: R_AVTEMP : real*4 : Average bottom plate temperature
C                R_TT : real*4 : Top plate temperature
C
      IMPLICIT NONE
      REAL*4 R_DELT,R_THETA,R_T2,R_KK,SSAT,R_AVTEMP,R_TT

      R_DELT   = R_TT - R_AVTEMP
      R_THETA  = (273.2 + (R_AVTEMP+R_TT+273.2))/2.0
      R_T2     = R_THETA**2
      R_KK     = 6717.23 - 4.93 * R_THETA

      SSAT     = (R_DELT**2)*
     -           ((R_KK**2)/( 2.0*(R_T2**2) )+(1.0/R_T2)-
     -           (R_KK/(R_T2*R_THETA)))
      SUPERSAT = SSAT*25.0

      RETURN
      END
