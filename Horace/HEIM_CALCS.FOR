C-----------------------------------------------------------------------------
C
C ROUTINE          HEIM_CALCS   SUBROUTINE FORTVAX       HEIM_CALCS.FOR
C
C PURPOSE          Calculate calibration correction tables for the
C                  Heimann
C
C DESCRIPTION      The routine converts the raw Heimann data to 
C                  temperatures and searches for calibration data. 
C                  When a calibration is started, bit 0 of parameter 27 is set
C                  to 1. The temperature control unit then changes the blackbody
C                  temperature to a range of temperatures depending on the
C                  calibration program chosen by the user. The blackbody
C                  temperature is sent to the DRS as parameter 142.
C                  The blackbody and Heimann
C                  temps are measured simultaneously so the program looks for 
C                  when para 142 is stable and unstable for when it is to 
C                  calculate the Heimann look up table. 
C 
C                  It then finds the most frequently occurring REF and
C                  Heimann temperatures and calculates the correction 
C                  value for the Heimann temperature. These correction values 
C                  are placed into a lookup table RTABLE(x,y),
C
C                    where x - index based on Heimann temperature
C                              1 - 2   are dummy elements
C                              3 - 640 relates to the temperature range of
C                                      -22.0 to +41.7 Deg C.
C                      and y - only two tables possible, current and previous
C                              depending on the value of STATUS(13)
C
C                  At the end of each section of calibration data the lookup
C                  table is interpolated between measured correction values and
C                  all empty elements are filled. Any correction factor with an
C                  absolute value greater than 4.0 deg C is replaced by the
C                  last known good point.
C      
C SUBPROGRAMS      IBITS      -  Extracts selected bits from input    
C                  BTEST      -  Tests value of selected single bit  
C                  SORT_HISTO -  Finds most frequently occurring temperature 
C                  FILL_TABLE -  Fills look-up table array by interpolation
C
C REFERENCES       
C
C VERSION          0.01   14-03-90   D.N. Jeffery
C                  1.02   14-02-94   D.R.Lauchlan
C                  1.03   29-08-02   W.D.N.Jackson
C
C CHANGES          1.02    Changes made for the introduction of the
C                          new temperature control unit.        
C                  1.03    All references to and code for BARNES removed.
C                          Now takes reference cal values from HOR_CALIB.DAT.
C                          Now handles 16 bit data.
C
C To compile and link : H_BUILD HEIM_CALCS
C                       H_BUILD H_DERIVE
C----------------------------------------------------------------------------
      SUBROUTINE HEIM_CALCS
      IMPLICIT   NONE
      INTEGER*2  ICAL,IGNORE,IPOS,IVALUE_RAD,ISECS,INDEX2,IHISTO(800,2),
     -           ICHK,INP,ICNT,IP,IS,IVALUE_REF
      INTEGER*4  I_RAD,IVAL,IREF,ICOUNT
      REAL*4     R_RAD,RTABLE(640,2),REF_TEMP,
     - REF_DIFF,RTEMPS(2),RLAST_REF_TEMP1,RLAST_REF_TEMP2,
     - RLAST_REF_TEMP3,RLAST_REF_TEMP4
      INCLUDE    'HCOM_DEF.FOR'
      COMMON     /HEIM_LOOK/ RTABLE
      REAL*4     CAL(512,6)
      COMMON     /CALS/ CAL
      DATA       ICAL/0/, ISECS/0/, INDEX2/2/, IPOS/0/,
     -           ICHK/0/, IGNORE/0/, RTABLE/1280*9999.9/
C
      IVAL(IP,IS)=JZEXT(NFDATA(NPLOC(IP)+IS-1,STATUS(1)))
C
C    IHISTO(IVALUE,2)=Heimann temp
C    IHISTO(IVALUE,1)=reference temp
C 
C The DO loop is repeated every second 
C
      DO INP = 1, NPFREQ(141)
C Calculate Heimann surface temp
         R_RAD = 0.0
         I_RAD=IVAL(141,1)
         R_RAD=CAL(141,1)+CAL(141,2)*I_RAD
         RTEMPS(1)=R_RAD
C
C Find out the raw REF temp value and convert this into the actual reference or
C blackbody temperature
C
         REF_TEMP=0.0
         IREF =IVAL(142,INP)
         REF_TEMP=CAL(142,1)+CAL(142,2)*IREF
         RTEMPS(2)=REF_TEMP
C         
C Check for the Heimann being set to CALIBRATE i.e. bit 0 of para 27 set to 1
C                                                                           
         IF (status(256).eq.1) THEN !Temp code in absence of signal register
!         IF (BTEST(IVAL(27,1),0)) THEN                !Check sig reg for cal
            IF (ICAL.EQ.0) THEN                       !First point of cal ?
               ICHK = 1                                 
               IF (STATUS(13).EQ.0) THEN              !First calibration ?
                  INDEX2 = 1                          !Yes so INDEX2 = 1
               ELSE 
                  INDEX2 = 3 - INDEX2                 !No so swap INDEX2
               END IF
               DO ICNT = 1,640
                  RTABLE(ICNT,INDEX2) = 9999.9        !Reset new table
               END DO
            END IF
            ICAL = 1                                  !On CALIBRATE
C
C Work out the difference between the reference blackbody temp and the
C reference blackbody temp of 1 second previously.  Update the list of 
C previous reference blackbody temperatures.
C
            REF_DIFF=SQRT((REF_TEMP-RLAST_REF_TEMP4)**2)
            RLAST_REF_TEMP4=RLAST_REF_TEMP3
            RLAST_REF_TEMP3=RLAST_REF_TEMP2
            RLAST_REF_TEMP2=RLAST_REF_TEMP1
            RLAST_REF_TEMP1=REF_TEMP
C
C The blackbody is assumed to be stable at a stable temp. when the difference 
C between the blackbody temp and that of one second ago is less than 0.185.
C
            IF (REF_DIFF.LT.0.185) THEN
               IVALUE_RAD= NINT((R_RAD + 40.) * 10.)   !Work out IHISTO indices
               IVALUE_REF=NINT((REF_TEMP+40.0)*10.)
C Only store good temps
               IF (IVALUE_REF.GE.1.AND.IVALUE_REF.LE.800) THEN     
                  IHISTO(IVALUE_REF,1) = IHISTO(IVALUE_REF,1) + 1 !Inc. counters
               END IF
               IF (IVALUE_RAD.GE.1.AND.IVALUE_RAD.LE.800) THEN     
                  IHISTO(IVALUE_RAD,1) = IHISTO(IVALUE_RAD,1) + 1 !Inc. counters
               END IF
               ICOUNT=ICOUNT+1
            ELSE 
               ICOUNT=0
            END IF               
C
C Once the program has 20 consecutive (5 seconds) of stable temperatures then
C it finds the most common occuring temperature for updating the look up table,
C which is done by calling the SORT_HISTO subroutine.
C
            IF (ICOUNT.GE.20)THEN
              CALL SORT_HISTO(INDEX2,IHISTO,RTABLE,ICHK,RTEMPS)
              ICOUNT=0
            END IF
C 
         ELSE IF (ICAL.EQ.1) THEN                     !Just finished cal ?
C
C   End of calibration so tidy up the last data and then fill the lookup
C   table with data interpolated between calculated correction values
C                      
            CALL SORT_HISTO(INDEX2,IHISTO,RTABLE,ICHK,RTEMPS)
C
C     If IGNORE = 1 then Heimann on CAL 
C                 for more than 10 minutes so ignore the calibration
C  Also if ICHK = 0 then no differences (REF - Heimann) have been calculated
C                   so ignore the calibration
C
            IF (IGNORE.EQ.1.OR.ICHK.LE.1) THEN
               DO ICNT = 1, 640                           !Yes so just reset
                  RTABLE(ICNT,INDEX2) = 9999.9            !table to 9999.9 to
               END DO                                     !avoid confusion
               INDEX2 = 3 - INDEX2                        !Reset index counter
C
C    A good calibration has been completed so now pad out the lookup table
C
            ELSE                                          !Good calibration data
               CALL FILL_TABLE(RTABLE(1,INDEX2))          !Interpolate table
               STATUS(13) = INDEX2                        !Point to new table
            END IF
            ICAL = 0                                      !Reset cal flag
            ISECS = 0                                     !Reset seconds count
            IPOS = 0                                      !Reset position flag
            ICHK = 0                                      !Reset good pair flag
            IGNORE = 0                                    !Reset 10 mins. flag
         END IF
      END DO
C
C   Heimann on CALIBRATE so check that the instrument has not been set to
C   either Heimann or REF for more than 10 minutes, if it has flag the
C   calibration as being invalid
C
      IF (ICAL.EQ.1) THEN                                
         ISECS = ISECS + 1                                !Increment secs count
         IF (ISECS.GT.600) THEN                           !On CAL for 10 mins ?
            IGNORE = 1                                    !Set 10 mins flag
         END IF
      END IF
      RETURN
C
      END
C-----------------------------------------------------------------------------
C
C ROUTINE          SORT_HISTO   SUBROUTINE FORTVAX         HEIM_CALCS.FOR
C
C PURPOSE          To calculate the Heimann calibration correction factor 
C
C DESCRIPTION      The array IHISTO contains 'Histogram' data indicating how
C                  many occurences there are of any temperature value. At the
C                  end of each pair of REF/Heimann data the difference is worked
C                  out and placed into the array RTABLE according to the Heimann
C                  temperature and calibration number.
C
C ARGUMENTS        INDEX2      I*2 - The calibration number (1 or 2)
C                  IHISTO(p,q) I*2 - Array of counts of occurences of REF and 
C                                    Heimann temperatures, indexed by temperature
C                                    and position, where :
C                                    p - Temperature in 0.1 deg steps from -39.9
C                                        (p=1) to +40.0 (p=800)
C                                    q - REF (q=1) or Heimann (q=2) position
C                             
C                  RTABLE(x,y) R*4 - Array of calculated correction factors, 
C                                    indexed by calibration number and temp.
C                    where x - index based on Heimann temperature
C                              1 - 2   are dummy elements
C                              3 - 640 relates to the temperature range of
C                                      -22.0 to +41.7 Deg C.
C                      and y - only two tables possible, current and previous
C                              depending on the value of STATUS(13)
C
C                  ICHK        I*2 - This flag is used to indicate the state of
C                                    the calibration :
C                                    = 0 - On CAL, good REF, no Heimann yet
C                                       The routine can calculate the difference
C                                       when the next Heimann is found. If still
C                                       0 at end of CAL ignore whole calibration
C                                       data.
C                                    = 1 - Start of CAL, no REF/Heimann pairs yet
C                                       Ignore all Heimann values until a good
C                                       REF value is found. If still 1 at end 
C                                       of CAL ignore whole calibration data.
C                                    = 2 - On CAL, good REF/Heimann pair found
C                                       Complete calibration run as normal
C                                 
C
C SUBPROGRAMS      ABS      -  Returns the absolute value of a number
C
C REFERENCES       
C
C VERSION          0.01   28-03-90   D.N. Jeffery
C                  1.02   14-02-94   D.R. Lauchlan 
C
C CHANGES          1.02  Works out the pair of temperatures instead of doing
C                        them alternatly.
C
C----------------------------------------------------------------------------
      SUBROUTINE SORT_HISTO(INDEX2,IHISTO,RTABLE,ICHK,RTEMP)  
      IMPLICIT   NONE
      INTEGER*2  ICHK,INDEX1,INDEX2,IHISTO(800,2),JJ,
     -  IVALUE_RAD,IVALUE_REF,IMAX_REF,IMAX_HEIM
      REAL*4     RDIFF,RTEMP(2),RTABLE(640,2),RMAX
      DATA       RMAX/4.0/   !Max. temp. correction allowed (subject to 
                             !change for a new instrument)  
C
      DO JJ = 1,800                              !Loop thru IHISTO
         IF (IHISTO(JJ,1).GT.IMAX_REF) THEN      !find most frequently
            IMAX_REF= IHISTO(JJ,1)               !occurring ref value
            IVALUE_REF = JJ
         END IF
         IHISTO(JJ,1) = 0                        !Zero ref part of IHISTO

         IF (IHISTO(JJ,2).GT.IMAX_HEIM) THEN     !find most frequently
            IMAX_HEIM= IHISTO(JJ,2)              !occurring Heimann value
            IVALUE_RAD = JJ
         END IF
         IHISTO(JJ,2) = 0                         !Zero IHISTO
      END DO
C
C Calculate the correction factor
C    RTEMP(1)= Heimann temperature
C    RTEMP(2)= Reference blackbody temperature.
C
      RDIFF = RTEMP(2) - RTEMP(1)                 !Calculate correction
      ICHK=2      
      IF (ABS(RDIFF).LT.RMAX) THEN                !Is diff. < max diff.
        INDEX1 = NINT((RTEMP(2) + 22.0)*10.0 + 3.0)  !Work out pointer
        IF (INDEX1.GE.3.AND.INDEX1.LE.640) THEN   !Is pointer in range
          RTABLE(INDEX1,INDEX2) = RDIFF           !Insert correction
        END IF          
      END IF                   
      RETURN
C
      END
C-----------------------------------------------------------------------------
C                    
C ROUTINE          FILL_TABLE   SUBROUTINE FORTVAX         HEIM_CALCS.FOR
C
C PURPOSE          Interpolates between correction factors in RTABLE
C
C DESCRIPTION      The array RTABLE initially contains calculated correction
C                  factors. At the end of a calibration sequence the remaining
C                  empty elements are filled by linear interpolation between
C                  calculated correction factors. Only calculated correction
C                  factors between +4.0 and -4.0 deg C are used.
C
C ARGUMENTS        RTABLE  R*4 -  Array of calibration correction factors
C
C SUBPROGRAMS      ABS      -  Returns the absolute value of a number
C
C REFERENCES       
C
C VERSION          0.01   28-03-90   D.N. Jeffery
C
C CHANGES          
C
C----------------------------------------------------------------------------
      SUBROUTINE FILL_TABLE(RTABLE)
      IMPLICIT   NONE
      INTEGER*2  INEWIND,IOLDIND,IFIRST,II,JJ,IINDINC,KK,IJ
      REAL*4     RTABLE(640),RDIFF,ROLD,RNEW,RINC
C
      INEWIND = 3                                 !Initialize pointer
      IFIRST = 0                                  !Zero flag
C
C   Loop through the complete lookup table filling in between calculated
C   correction factors
C
      DO II = 3, 640                                        
         IF (RTABLE(II).LT.1000) THEN             !Is it a correction value ?
            IF (IFIRST.EQ.0) THEN                 !Is it first point in table ?
               IFIRST = 1                           
               DO JJ = 3, II-1                    !Fill array upto first
                  RTABLE(JJ) = RTABLE(II)         !point with first value
               END DO
               RNEW = RTABLE(II)
               INEWIND = II
            ELSE
               ROLD = RNEW
               RNEW = RTABLE(II)
               IOLDIND = INEWIND
               INEWIND = II
               IINDINC = (INEWIND - IOLDIND)      !No. of incremental points
C
C   Only fill table if old and new points are not successive
C
               IF (IINDINC.GT.1) THEN             
                  RDIFF = ROLD - RNEW             !Diff. between 2 corrections
                  RINC = RDIFF / IINDINC          !Incremental value
                  DO KK = IOLDIND+1, INEWIND-1    !Fill section of array using
                     RTABLE(KK) = RTABLE(KK-1) - RINC !incremental value RINC
                  END DO
               END IF
            END IF
         END IF
      END DO
C
C   If there is any unfilled lookup table elements at the end of the table
C   then fill them with the last calculated correction factor
C
      IF (INEWIND.LT.640) THEN                    
         DO IJ = INEWIND+1, 640                   !Fill last section of array
            RTABLE(IJ) = RTABLE(INEWIND)          !with last correction value
         END DO
      END IF
      RETURN
      END
