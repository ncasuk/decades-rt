! adjust rsdlim in do_cal to 32 once new DRS is working
C
C ROUTINE          INST_CAL PROGRAM FORTVAX
C
C PURPOSE          Handles on-aircraft instrument calibration
C
C DESCRIPTION      Carries out instrument calibrations on the aircraft by using
C                  HORACE to collect, average, and fit data measured by the
C                  DRS.  This program works in conjunction with the file
C                  INST_CAL.DAT which defines for each calibratable parameter
C                  the calibration values which must be set up on the DRS
C                  inputs.
C
C                  The program offers a menu with all the parameters in
C                  INST_CAL.DAT as options. The default entry is always the
C                  next parameter on the menu.
C
C                  Once a parameter has been selected the program asks the user
C                  to set up each of the required inputs and press RETURN when
C                  ready.  The program then takes data until the standard
C                  deviation from the mean is less than 32 DRS bit, or for a
C                  maximum of one minute, or for a minimum of 10 readings. 
C                  When all values have been input the coefficients of the
C                  polynomial are calculated and displayed, together with the
C                  measured values.  Note that if measured values are close to
C                  0 or 4095*16 they are not used.
C
C                  Note that the DRS must be on DATA, and the H_DRS_LOG and
C                  H_DERIVE processes must be running for this program to work.
C
C                  When building note that the file HCOM_DEF.FOR must be in 
C                  the same directory as this source file.  Build with
C
C                       $ FORT INST_CAL
C                       $ LINK INST_CAL,H_COMMON/OPT
C                       $ RUN INST_CAL
C
C                  or build with H_BUILD INST_CAL
C
C                  The program logs all output, except requests to the
C                  operator, to files named INST_CAL.LOG on hard (and optical
C                  if mounted) disk.
C
C                  After doing the basic DRS calibration, which is the same
C                  for all parameters, the option is provided to include the
C                  sensor calibration to provide a set of overall calibration
C                  coefficients.  Most parameters are handled separately here,
C                  and the deiced parameter is handled by working directly with
C                  the 0 and 100 C resistances.
C
C VERSION          1.00  17-07-91  W.D.N.JACKSON
C                  1.02  09-02-94  D.R.Lauchlan 
C
C CHANGES          25/11/92  The program was altered to take into account
C                  parameters 14,42,47 and 59 all going into the same interface
C                  port.
C                  1.02 24/03/94  Options to cal 59,42, and 47 removed
C                  1.03 30/05/02  Now uses 16 bit DRS data and handles general
C                                 new DRS analogue channels using 999 (WDNJ)
C                  1.04 04/10/02  Calibration of multiple analogue channels 
C                                 added using 997 (WDNJ)
C
C*******************************************************************************
      PROGRAM INST_CAL
CDEC$ IDENT 'V1.04'
      IMPLICIT  NONE
      INTEGER*4 IPARA(32),INVAL(32),IORD(32),IPASS,IOS,INEXT,IL,I,JOS,
     -    IPAR,IPCNT,IPLST(50),ISTAT,STR$ELEMENT,LAN,IPERIOD
      REAL*4    RVAL(32,32),RCOEF(4,32),RTCOEF(4)
      CHARACTER CUNITS(32)*8,CTEXT(32)*40,CINP*3,CDATE*9,CTIME*8,
     -    CTEMP*40,CINP1*20,CLINE*200,CINP2*20,CLINE1*80,CDLU*6
      INCLUDE   'HCOM_DEF.FOR'
      CHARACTER CSNAME(512)*4,CFNAME(512)*20
      DATA      IPARA/32*0/,RCOEF/128*0./
C
C Clear screen and try to open logging files on hard and optical disk.  If this
C succeeds then tell the user, otherwise send output to the null device
C instead.
C
      WRITE(6,10) CHAR(27)//'[2J'//CHAR(27)//'[1;1H' !Clear screen
C Open the files on hard disc
      OPEN(UNIT=20,FILE='DUA0:[HORACE]INST_CAL.LOG',STATUS='NEW',
     -    IOSTAT=IOS)
      IF(IOS.NE.0) OPEN(UNIT=20,FILE='NL:',STATUS='NEW')
      IF(IOS.EQ.0) WRITE(6,*) 'Output will be logged to '//
     -    'DUA0:[HORACE]INST_CAL.LOG'
C Open the files on optical disc
      OPEN(UNIT=21,FILE='DUB0:[HORACE]INST_CAL.LOG',STATUS='NEW',
     -    IOSTAT=IOS)
      IF(IOS.NE.0) OPEN(UNIT=21,FILE='NL:',STATUS='NEW')
      IF(IOS.EQ.0) WRITE(6,*) 'Output will be logged to '//
     -    'DUB0:[HORACE]INST_CAL.LOG'
C Read in the data file
      CALL DRS_NAMES_READ(CSNAME,CFNAME)
      CALL READ_DATA(IPARA,IORD,INVAL,RVAL,CUNITS,CTEXT) !Read input data
      INEXT=1                          !Next default parameter pointer
      IL=0
      IPASS=1
      DO WHILE(.TRUE.)                 !Loop indefinitely
        WRITE(6,10)
        WRITE(6,10) 'Choose a parameter from the following list:'
        WRITE(6,10) 
        DO I=1,32                      !List possible parameters
          IF(IPARA(I).NE.0) WRITE(6,11) IPARA(I),CTEXT(I)
        END DO
        WRITE(6,10)                    !Get requirement
        WRITE(6,14) '$Parameter? (CTRL/Z to exit) [999] '
        READ(5,14,IOSTAT=IOS) CINP     !Get response
        IF(IOS.EQ.-1) CALL EXIT        !Exit on CTRL/Z
        IF(CINP.EQ.' ') CINP='999'     !Default is 999
        READ(CINP,13,IOSTAT=JOS) IPAR  !Decode parameter number
        INEXT=0                        !Parameter position in list
        DO I=1,32
          IF(IPARA(I).EQ.IPAR) INEXT=I !Look for parameter in list, 0 if absent
        END DO
C Find valid para and add to list
        IF(IPARA(INEXT).NE.0.AND.IOS.EQ.0.AND.JOS.EQ.0) THEN !If valid
          CALL DATE1(CDATE)            !Write date and time to logging files
          CALL TIME(CTIME)
          IF(IL.EQ.0) THEN
            WRITE(20,14,IOSTAT=IOS) ' INST_CAL  '//CDATE//'  '
     -        //CTIME
            WRITE(21,14,IOSTAT=IOS) ' INST_CAL  '//CDATE//'  '
     -        //CTIME
            IL=1
          ELSE                         !Do new page
            WRITE(20,14,IOSTAT=IOS) '1INST_CAL  '//CDATE//'  '
     -        //CTIME
            WRITE(21,14,IOSTAT=IOS) '1INST_CAL  '//CDATE//'  '
     -        //CTIME
          END IF
C Calibrate parameter.
          IF(IPARA(INEXT).EQ.999) THEN
100         WRITE(6,14) '$New DRS parameter number? (CTRL/Z to exit) '
            READ(5,14,IOSTAT=IOS) CINP !Get response
            IF(IOS.EQ.-1) CALL EXIT    !Exit on CTRL/Z
            IF(IOS.NE.0) GOTO 100
            READ(CINP,13,IOSTAT=JOS) IPAR  !Decode parameter number
            IF(JOS.NE.0) GOTO 100
            IF(IPAR.LT.1.OR.IPAR.GT.512) GOTO 100
            IF(NPFREQ(IPAR).LT.1) GOTO 100
            IPARA(INEXT)=IPAR
            CTEMP=CTEXT(INEXT)
            CTEXT(INEXT)=CFNAME(IPAR)
            CALL DO_CAL(INEXT,IPARA,IORD,INVAL,RVAL,CUNITS,CTEXT,RCOEF) 
            IPARA(INEXT)=999
            CTEXT(INEXT)=CTEMP
          ELSE IF(IPARA(INEXT).EQ.998) THEN
            WRITE(6,10)
            WRITE(6,10)
     -      'Enter DRS calibration coeffs as floating point numbers.'
            WRITE(6,10) 'Use 0.0 if absent.'
            WRITE(6,10)
            DO I=1,4                   !Get three sensor calibration coeffs
              IOS=-1
              DO WHILE(IOS.NE.0)       !Loop till have valid input
                WRITE(6,'(A,I1,A)') '$DRS  X**',I-1,' coefficient?  '
                READ(5,14,IOSTAT=IOS) CINP1
                IF(IOS.EQ.-1) CALL EXIT !Return immediately on CTRL/Z
                READ(CINP1,'(BN,G)',IOSTAT=IOS) RCOEF(I,INEXT) !Decode input
              END DO
            END DO      
          ELSE IF(IPARA(INEXT).EQ.996) THEN
            CALL RT_POLY(RTCOEF)
          ELSE IF(IPARA(INEXT).EQ.997) THEN
102         CLINE=' '
            WRITE(6,14) '$Enter measurement period is secs'//
     -          ' (CTRL/Z to exit) [15]: '
            READ(5,14,IOSTAT=IOS) CLINE !Get response
            IF(IOS.EQ.-1) CALL EXIT    !Exit on CTRL/Z
            IF(IOS.NE.0) GOTO 102
            IF(CLINE.EQ.' ') CLINE='15'
            READ(CLINE,'(BN,I)',IOSTAT=IOS) IPERIOD
            IF(IOS.NE.0) GOTO 102
101         CLINE=' '
            CDLU=' '
            WRITE(6,14) ' Enter DLU name or up to 50 parameters'//
     -          ' separated  by commas (CTRL/Z to exit): '
            READ(5,14,IOSTAT=IOS) CLINE !Get response
            IF(IOS.EQ.-1) CALL EXIT    !Exit on CTRL/Z
            IF(IOS.NE.0) GOTO 101
            IPCNT=0
            CALL STR$UPCASE(CLINE,CLINE)
            OPEN(99,FILE='HORACE:DRS_CONFIG.DAT',STATUS='OLD',
     -          READONLY,IOSTAT=IOS)
            READ(99,'(A)',IOSTAT=IOS) CLINE1
            DO WHILE(IOS.EQ.0)
              IF(CLINE1(1:6).EQ.CLINE(1:6)) THEN
                CALL STR_COMPRESS(CLINE1)
                ISTAT=STR$ELEMENT(CINP2,2,' ',CLINE1)
                IF(CINP2.EQ.'A') THEN
                  ISTAT=STR$ELEMENT(CINP2,5,' ',CLINE1)
                  IPCNT=IPCNT+1
                  READ(CINP2,'(BN,I)',IOSTAT=IOS) IPLST(IPCNT)
                  CDLU=CLINE(1:6)
                END IF
              END IF
              READ(99,'(A)',IOSTAT=IOS) CLINE1
            END DO
            CLOSE(99,IOSTAT=IOS)
            IF(IPCNT.EQ.0) THEN
              ISTAT=1
              ISTAT=STR$ELEMENT(CINP2,IPCNT,',',CLINE(1:LAN(CLINE)))
              DO WHILE(ISTAT)
                READ(CINP2,13,IOSTAT=JOS) IPAR  !Decode parameter number
                IF(JOS.NE.0) GOTO 101
                IF(IPAR.LT.1.OR.IPAR.GT.512) THEN
                  TYPE *,'Invalid parameter number ',IPAR
                  GOTO 101
                ELSE IF(NPFREQ(IPAR).LT.1) THEN
                  TYPE *,'Unrecorded parameter number ',IPAR
                  GOTO 101
                ELSE IF(IPCNT.GE.50) THEN
                  TYPE *,'More than 50 parameters'
                  GOTO 101
                END IF
                IPCNT=IPCNT+1
                IPLST(IPCNT)=IPAR
                ISTAT=STR$ELEMENT(CINP2,IPCNT,',',CLINE(1:LAN(CLINE)))
              END DO
            END IF
            IORD(INEXT)=1              !Only do first order polynomials
            CALL DO_MULCAL(INEXT,IORD,INVAL,RVAL,CUNITS,IPCNT,
     -           IPLST,CSNAME,CFNAME,IPERIOD,CDLU) 
          ELSE
            CALL DO_CAL(INEXT,IPARA,IORD,INVAL,RVAL,CUNITS,CTEXT,RCOEF) 
          END IF
          
          WRITE(6,10)                  !Option to add sensor calibration
          IF(IPARA(INEXT).EQ.997.OR.IPARA(INEXT).EQ.996) THEN !Do nothing
          ELSE
            IF(IPARA(INEXT).NE.998) THEN
              WRITE(6,16) 'Do you want to add sensor calibration? [N] '
              READ(5,14,IOSTAT=IOS) CINP !Get response
              IF(IOS.EQ.-1) CALL EXIT    !Exit on CTRL/Z
            ELSE
              CINP(1:1)='Y'
            END IF
            IF(CINP(1:1).EQ.'Y'.OR.CINP(1:1).EQ.'y') CALL
     -          DO_CUBIC(INEXT,IPARA,RCOEF) !Multiply polynomials if required
          END IF
          WRITE(6,14) '$Press [RETURN] for menu ' !Wait before clearing screen
          READ(5,14,IOSTAT=IOS) CINP
          IF(IOS.EQ.-1) CALL EXIT      !Exit if CTRL/Z
        END IF
        WRITE(6,10) CHAR(27)//'[2J'//CHAR(27)//'[1;1H' !Clear screen
      END DO                           !Loop indefinitely      
10    FORMAT(1X,A)
11    FORMAT(1X,'   ',I3,'    ',A)
12    FORMAT(X,A,I3,A,$)
13    FORMAT(BN,I)
14    FORMAT(A)
15    FORMAT(I3)
16    FORMAT(X,A,$)
      END
********************************************************************************
      SUBROUTINE READ_DATA(IPARA,IORD,INVAL,RVAL,CUNITS,CTEXT)
C
C Reads in data from the file INST_CAL.DAT which define the parameters and the
C values to be input to the DRS while calibrating them.  See the INST_CAL.DAT
C file for format details.
C
      IMPLICIT  NONE
      INTEGER*4 IPARA(32),IORD(32),INVAL(32),IN,IL,IOS,J,I,K
      REAL*4    RVAL(32,32)
      CHARACTER CUNITS(32)*8,CTEXT(32)*40,CLINE*80

      OPEN(UNIT=10,FILE='INST_CAL.DAT',STATUS='OLD',READONLY) !Open file
      READ(10,10,IOSTAT=IOS) CLINE     !First three lines are comments
      READ(10,10,IOSTAT=IOS) CLINE
      READ(10,10,IOSTAT=IOS) CLINE
      IN=1                             !Pointer to next location in arrays
      READ(10,10,IOSTAT=IOS) CLINE     !Read first proper line
      IL=4                             !Line count
C
C Keep reading until get a read error, more than 32 parameters are specified,
C or encounter a blank line.  Note that input values are placed eight to a line
C on the line(s) following the parameter description line.
C
      DO WHILE(IOS.EQ.0.AND.IN.LE.32.AND.CLINE.NE.' ') 
        READ(CLINE,11,IOSTAT=IOS) IPARA(IN),IORD(IN),INVAL(IN)
        IF(IOS.NE.0) THEN              !If can't decode line
          WRITE(6,*) 'Can''t decode INST_CAL.DAT line ',IL
        ELSE
          CUNITS(IN)=CLINE(10:17)      !Transfer units
          CTEXT(IN)=CLINE(19:58)       !Transfer description
          J=1                          !Read the input data values, in lines of
          DO I=1,INVAL(IN),8           ! up to eight values.
            READ(10,12,IOSTAT=IOS) (RVAL(K,IN),K=J,MIN(J+7,INVAL(IN)))
            IL=IL+1                    !Bump line count
            IF(IOS.NE.0) THEN          !If can't decode line
              WRITE(6,*) 'Can''t decode INST_CAL.DAT line ',IL
            ELSE 
              J=J+8                    !Bump value count
            END IF
          END DO
          IF(J.GT.1.OR.INVAL(IN).EQ.0) IN=IN+1 !Bump pointer if read some valid values
        END IF
        READ(10,10,IOSTAT=IOS) CLINE   !Read next line
        IL=IL+1                        !Bump line count
      END DO
      CLOSE(UNIT=10,IOSTAT=IOS)        !Close input file
      RETURN        

10    FORMAT(A)
11    FORMAT(I3,X,I1,X,I2)
12    FORMAT(9X,8G)
      END
********************************************************************************
      SUBROUTINE DO_CAL(IP,IPARA,IORD,INVAL,RVAL,CUNITS,CTEXT,RCOEF)
C
C Handles calibration of each parameter by asking the user to set up each input
C and then reading enough data to get a good mean value.  When measurements
C have been taken for all values a polynomial to the required order is fitted.
C
      IMPLICIT  NONE
      INTEGER*4 IP,IPARA(32),IORD(32),INVAL(32),IDATA(9600),I,IOS,IPF,
     -    IPL,IPL1,IV,ID,ISUM,IBLK,IS,IVAL,J,NP,LAN
      REAL*4    RVAL(32,32),RCOEF(4,32),RMEAN(32),RSD(32)
      REAL*4    RCOEFS(5),RINDEP(32),RDEPEND(32),RSDLIM,RSUMDIF,RMAX,
     -    RCORR
      CHARACTER CUNITS(32)*8,CTEXT(32)*40,CINP,CLINE*80
      INCLUDE   'HCOM_DEF.FOR'

      DO I=1,5                         !Initially set all coefficients to zero
        RCOEFS(I)=0.0
      END DO
      WRITE(6,12) IPARA(IP),CTEXT(IP)  !Remind user which parameter doing
      IPF=NPFREQ(IPARA(IP))            !Get parameter frequency
      IPL=NPLOC(IPARA(IP))             !Get params location in DRS block
      IF(IPARA(IP).EQ.10) IPL1=NPLOC(11) !Get deiced scale location
      IF(IPARA(IP).EQ.23) IPL1=NPLOC(24) !Get deiced scale location
      DO IV=1,INVAL(IP)                !For each calibration value
        WRITE(CLINE,10) 7,RVAL(IV,IP),CUNITS(IP) !Ask user to set up value on DRS inp
        CALL STR_COMPRESS(CLINE)
        WRITE(6,11) '$'//CLINE(1:LAN(CLINE))
        READ(5,11,IOSTAT=IOS) CINP     !Wait till ready
        IF(IOS.EQ.-1) RETURN           !Abort this calibration on CTRL/Z
        ID=0                           !Number of DRS data values read so far
        ISUM=0                         !Sum of DRS data values read so far
        RSD(IV)=999999.0                 !Latest evaluated standard deviation
!        RSDLIM=2.0*16                  !Adjust for 16 bit recording
        rsdlim=10000.  !Temp value while have DRS glitches
C
C Keep reading data until the standard deviation is less than 2.0 DRS units, or
C for a maximum of one minute.  Also always read at least 10 values.
C For parameters 10 and 11 treat them as a single 15 bit word by combining
C the two words.
C
        DO WHILE(ID.LT.10.OR.(RSD(IV).GT.RSDLIM.AND.ID.LE.IPF*60*1))
          IBLK=NFDATA(8,STATUS(1))    !Get current DRS block number
          DO WHILE (IBLK.EQ.NFDATA(8,STATUS(1))) !Wait for it to change
            CALL LIB$WAIT(0.2)
          END DO
          IS=STATUS(1)                 !Pointer to latest DRS block
          DO I=1,IPF                   !For each parameter sample
            IVAL=JZEXT(NFDATA(IPL+I-1,IS))  !Get 16 bit value
            ID=ID+1                    !Bump array pointer
            IDATA(ID)=IVAL             !Store it in data array
            ISUM=ISUM+IVAL             !Add it to current sum
          END DO
          RMEAN(IV)=FLOAT(ISUM)/ID     !Compute latest mean
          RSUMDIF=0.0                  !Compute sum of differences squared
          DO J=1,ID
            RSUMDIF=RSUMDIF+(FLOAT(IDATA(J))-RMEAN(IV))**2
          END DO
          RSD(IV)=SQRT(RSUMDIF/ID)     !Compute latest standard deviation
          TYPE *,IV,ID,RSD(IV),RMEAN(IV)
        END DO
      END DO
C
C Now fit polynomial to the data, first excluding any data points that are
C too near 0 or 4095 (or 32764 for param 10 and 23) to be sure if they are 
C valid.
C
      WRITE(6,14)
      WRITE(6,12) IPARA(IP),CTEXT(IP)  !Remind user which parameter doing
      WRITE(6,14)
      WRITE(20,12,IOSTAT=IOS) IPARA(IP),CTEXT(IP)
      WRITE(21,12,IOSTAT=IOS) IPARA(IP),CTEXT(IP)
      NP=0                             !Number of valid points for fitting
      RMAX=4086.*16.                   !Max usable DRS values, adjust for 16 bit
      DO I=1,INVAL(IP)                 !Check each calibration value
        IF(RMEAN(I).GT.10.0.AND.RMEAN(I).LT.RMAX) THEN !If not near ends
          NP=NP+1                      !Bump count
          RINDEP(NP)=RMEAN(I)          !DRS values are independent variables
          RDEPEND(NP)=RVAL(I,IP)       !Input values are dependent variables
        ELSE                           !Tell user about unused points
          WRITE(6,13,IOSTAT=IOS) 'Warning - Value at ',RVAL(I,IP),' '
     -    //CUNITS(IP)//' of ',RMEAN(I),
     -    ' DRS bits, not used for fitting'
          WRITE(20,13,IOSTAT=IOS) 'Warning - Value at ',RVAL(I,IP),' '
     -    //CUNITS(IP)//' of ',RMEAN(I),
     -    ' DRS bits, not used for fitting'
          WRITE(21,13,IOSTAT=IOS) 'Warning - Value at ',RVAL(I,IP),' '
     -    //CUNITS(IP)//' of ',RMEAN(I),
     -    ' DRS bits, not used for fitting'
        END IF
      END DO
      IF(NP.GT.IORD(IP)+1) THEN        !Check have enough points for fit
C
C Note that multiple polyfits are done here solely as means of writing the
C data and coefficients to each of the three possible output units.
C
        CALL POLY_FIT(6,IORD(IP),NP,RINDEP,RDEPEND,RCOEFS,RCORR) !Do fit
        CALL POLY_FIT(20,IORD(IP),NP,RINDEP,RDEPEND,RCOEFS,RCORR) !Do fit
        CALL POLY_FIT(21,IORD(IP),NP,RINDEP,RDEPEND,RCOEFS,RCORR) !Do fit
      ELSE                             !Else give message
        WRITE(6,14) 'Warning - Not enough points for fitting polynomial'
      END IF
C
C Transfer coefficients and return
C
      RCOEF(1,IP)=RCOEFS(1)            !Transfer coeffs to passed array
      RCOEF(2,IP)=RCOEFS(2)
      RCOEF(3,IP)=RCOEFS(3)
      RCOEF(4,IP)=RCOEFS(4)            !Coeffs will be 0 if not used
      RETURN                           !Return to calling routine

10    FORMAT(A1,'Set input to ',F6.2,X,A,
     -    ' and press RETURN when ready')
11    FORMAT(A)
12    FORMAT(/X,'Parameter ',I3,2X,A)
13    FORMAT(X,A,F7.2,A,F7.1,A)
14    FORMAT(X,A)
      END
********************************************************************************
      SUBROUTINE DO_MULCAL(IP,IORD,INVAL,RVAL,CUNITS,IPCNT,IPLST,
     -    CSNAME,CFNAME,IPERIOD,CDLU)
C
C Handles calibration of multiple DRS analogue parameters by working out the 
C means and SDs of many parameters at the same time and then fitting 
C polnomial to them.
C
      IMPLICIT  NONE
      INTEGER*4 IP,IORD(32),INVAL(32),IDATA(9600,50),I,IOS,
     -    IPF(50),IL,IPERIOD,
     -    IPL(50),IV,ID(50),ISUM(50),IBLK,IS,IVAL,J,NP,LAN,IPCNT,
     -    IPLST(50),IQ
      REAL*4    RVAL(32,32),RMEAN(32,50),RSD(32,50)
      REAL*4    RCOEFS(5,50),RINDEP(32,50),RDEPEND(32,50),RSUMDIF,
     -    RCORR(50)
      CHARACTER CUNITS(32)*8,CINP,CLINE*80,CSNAME(512)*4,CDLU*6,
     -    CFNAME(512)*20
      INCLUDE   'HCOM_DEF.FOR'
!
      DO IQ=1,IPCNT
        DO I=1,5                       !Initially set all coefficients to zero
          RCOEFS(I,IQ)=0.0
        END DO
        IPF(IQ)=NPFREQ(IPLST(IQ))      !Get parameter frequency
        IPL(IQ)=NPLOC(IPLST(IQ))       !Get params location in DRS block
      END DO
      DO IV=1,INVAL(IP)                !For each calibration value
        WRITE(CLINE,10) 7,RVAL(IV,IP),CUNITS(IP) !Ask user to set up value on DRS inp
        CALL STR_COMPRESS(CLINE)
        WRITE(6,11) '$'//CLINE(1:LAN(CLINE))
        READ(5,11,IOSTAT=IOS) CINP     !Wait till ready
        IF(IOS.EQ.-1) RETURN           !Abort this calibration on CTRL/Z
        DO IQ=1,IPCNT
          ID(IQ)=0                     !Number of DRS data values read so far
          ISUM(IQ)=0                   !Sum of DRS data values read so far
          RSD(IV,IQ)=999999.0          !Latest evaluated standard deviation
        END DO
C
C Keep reading data for the required period of time.
C
        IL=0                          !Loop counter
        DO WHILE(IL.LT.IPERIOD)
          IBLK=NFDATA(8,STATUS(1))    !Get current DRS block number
          DO WHILE (IBLK.EQ.NFDATA(8,STATUS(1))) !Wait for it to change
            CALL LIB$WAIT(0.2)
          END DO
          IS=STATUS(1)                 !Pointer to latest DRS block
          DO IQ=1,IPCNT
            DO I=1,IPF(IQ)             !For each parameter sample
              IVAL=JZEXT(NFDATA(IPL(IQ)+I-1,IS))  !Get 16 bit value
              ID(IQ)=ID(IQ)+1          !Bump array pointer
              IDATA(ID(IQ),IQ)=IVAL       !Store it in data array
              ISUM(IQ)=ISUM(IQ)+IVAL   !Add it to current sum
            END DO
            RMEAN(IV,IQ)=FLOAT(ISUM(IQ))/ID(IQ) !Compute latest mean
            RSUMDIF=0.0                !Compute sum of differences squared
            DO J=1,ID(IQ)
              RSUMDIF=RSUMDIF+(FLOAT(IDATA(J,IQ))-RMEAN(IV,IQ))**2
            END DO
            RSD(IV,IQ)=SQRT(RSUMDIF/ID(IQ)) !Compute latest standard deviation
          END DO
        IL=IL+1
        END DO
        DO IQ=1,IPCNT
          WRITE(6,'(X,I3,X,A,I3,F8.1,F8.2)',IOSTAT=IOS)
     _       IPLST(IQ),CSNAME(IPLST(IQ)),IPF(IQ),RMEAN(IV,IQ),RSD(IV,IQ)
        END DO
      END DO
!
! Summarize all the means and SDs
!
      WRITE(6,'(/X,A,T13,15(F6.2))',IOSTAT=IOS) 
     -   CUNITS(IP)(1:LAN(CUNITS(IP)))//':',(RVAL(IV,IP),IV=1,INVAL(IP))
      WRITE(20,'(/X,A,T13,15(F6.2))',IOSTAT=IOS) 
     -   CUNITS(IP)(1:LAN(CUNITS(IP)))//':',(RVAL(IV,IP),IV=1,INVAL(IP))
      WRITE(21,'(/X,A,T13,15(F6.2))',IOSTAT=IOS) 
     -   CUNITS(IP)(1:LAN(CUNITS(IP)))//':',(RVAL(IV,IP),IV=1,INVAL(IP))
      WRITE(CLINE,'(A,I3,A)',IOSTAT=IOS) 'Means (',IPERIOD,'s):'
      IF(CLINE(9:9).EQ.' ') CLINE=CLINE(1:7)//CLINE(10:)
      IF(CLINE(8:8).EQ.' ') CLINE=CLINE(1:7)//CLINE(9:)
      WRITE(6,'(X,A)',IOSTAT=IOS) CLINE(1:LAN(CLINE))
      WRITE(20,'(X,A)',IOSTAT=IOS) CLINE(1:LAN(CLINE))
      WRITE(21,'(X,A)',IOSTAT=IOS) CLINE(1:LAN(CLINE))
      DO IQ=1,IPCNT
        WRITE(6,'(X,I3,X,A,I3,15(I6))',IOSTAT=IOS) IPLST(IQ),
     -     CSNAME(IPLST(IQ)),IPF(IQ),(NINT(RMEAN(IV,IQ)),IV=1,INVAL(IP))
        WRITE(20,'(X,I3,X,A,I3,15(I6))',IOSTAT=IOS) IPLST(IQ),
     -     CSNAME(IPLST(IQ)),IPF(IQ),(NINT(RMEAN(IV,IQ)),IV=1,INVAL(IP))
        WRITE(21,'(X,I3,X,A,I3,15(I6))',IOSTAT=IOS) IPLST(IQ),
     -     CSNAME(IPLST(IQ)),IPF(IQ),(NINT(RMEAN(IV,IQ)),IV=1,INVAL(IP))
      END DO
!
      WRITE(CLINE,'(A,I3,A)',IOSTAT=IOS) 'SDs (',IPERIOD,'s):'
      IF(CLINE(7:7).EQ.' ') CLINE=CLINE(1:5)//CLINE(8:)
      IF(CLINE(6:6).EQ.' ') CLINE=CLINE(1:5)//CLINE(7:)
      WRITE(6,'(X,A)',IOSTAT=IOS) CLINE(1:LAN(CLINE))
      WRITE(20,'(X,A)',IOSTAT=IOS) CLINE(1:LAN(CLINE))
      WRITE(21,'(X,A)',IOSTAT=IOS) CLINE(1:LAN(CLINE))
      DO IQ=1,IPCNT
        WRITE(6,'(X,I3,X,A,I3,15(F6.2))',IOSTAT=IOS) IPLST(IQ),
     -      CSNAME(IPLST(IQ)),IPF(IQ),(RSD(IV,IQ),IV=1,INVAL(IP))
        WRITE(20,'(X,I3,X,A,I3,15(F6.2))',IOSTAT=IOS) IPLST(IQ),
     -      CSNAME(IPLST(IQ)),IPF(IQ),(RSD(IV,IQ),IV=1,INVAL(IP))
        WRITE(21,'(X,I3,X,A,I3,15(F6.2))',IOSTAT=IOS) IPLST(IQ),
     -      CSNAME(IPLST(IQ)),IPF(IQ),(RSD(IV,IQ),IV=1,INVAL(IP))
      END DO
C
C Now fit polynomial to the data.
C
      DO IV=1,INVAL(IP)                 !Check each calibration value
        DO IQ=1,IPCNT
          RINDEP(IV,IQ)=RMEAN(IV,IQ)    !DRS values are independent variables
          RDEPEND(IV,IQ)=RVAL(IV,IP)    !Input values are dependent variables
        END DO
      END DO
      NP=INVAL(IP)
C
C Note that multiple polyfits are done here solely as means of writing the
C data and coefficients to each of the three possible output units.
C
      DO IQ=1,IPCNT
        I=IPLST(IQ)
        WRITE(6,'(//X,I3,2X,A)',IOSTAT=IOS) I,CSNAME(I)//'  '//CFNAME(I)
        CALL POLY_FIT(6,IORD(IP),NP,RINDEP(1,IQ),RDEPEND(1,IQ), !Do fit
     -      RCOEFS(1,IQ),RCORR(IQ))
        WRITE(20,'(//X,I3,2X,A)',IOSTAT=IOS)I,CSNAME(I)//'  '//CFNAME(I)
        CALL POLY_FIT(20,IORD(IP),NP,RINDEP,RDEPEND,RCOEFS,RCORR)
        WRITE(21,'(//X,I3,2X,A)',IOSTAT=IOS)I,CSNAME(I)//'  '//CFNAME(I)
        CALL POLY_FIT(21,IORD(IP),NP,RINDEP,RDEPEND,RCOEFS,RCORR)
      END DO
C
C Write coefficients and return
C
      WRITE(6,'(/X,A,T19,A)',IOSTAT=IOS) CDLU,
     -    ' X**0          X**1      Correl'
      WRITE(20,'(/X,A,T19,A)',IOSTAT=IOS) CDLU,
     -    ' X**0          X**1      Correl'
      WRITE(21,'(/X,A,T19,A)',IOSTAT=IOS) CDLU,
     -    ' X**0          X**1      Correl'
      DO IQ=1,IPCNT
        WRITE(6,'(X,I3,X,A,I3,X,2(X,1PE13.6E2),0PF8.4)',IOSTAT=IOS)
     -      IPLST(IQ),CSNAME(IPLST(IQ)),IPF(IQ),
     -      RCOEFS(1,IQ),RCOEFS(2,IQ),RCORR(IQ)
        WRITE(20,'(X,I3,X,A,I3,X,2(X,1PE13.6E2),0PF8.4)',IOSTAT=IOS)
     -      IPLST(IQ),CSNAME(IPLST(IQ)),IPF(IQ),
     -      RCOEFS(1,IQ),RCOEFS(2,IQ),RCORR(IQ)
        WRITE(21,'(X,I3,X,A,I3,X,2(X,1PE13.6E2),0PF8.4)',IOSTAT=IOS)
     -      IPLST(IQ),CSNAME(IPLST(IQ)),IPF(IQ),
     -      RCOEFS(1,IQ),RCOEFS(2,IQ),RCORR(IQ)
      END DO
!
      RETURN                           !Return to calling routine
!
10    FORMAT(A1,'Set input to ',F6.2,X,A,' and press RETURN when ready')
11    FORMAT(A)
14    FORMAT(X,A)
      END
********************************************************************************
      SUBROUTINE DO_CUBIC(IP,IPARA,RCOEF)
C
C Multiplies together two polynomials to produce a set of coefficients for the
C resulting polynomial.  The input polynomials can be up to third order, 
C although if it is less, unused coefficients should be set to zero.  Computes
C up to ten coefficients for the resulting polynomial, although only four are
C printed out.
C
C IP is an index into the RCOEF array pointing to the DRS calibration
C coefficients.  Unused coefficients in this array must be set to zero.
C
C Sensors are handled as follows:
C
C General Eastern signal: do nothing as Sensor calibration is hard wired into
C th program
C                          
C V1.01  10/08/99  W D N JACKSON
C        Temperature sensors now have no special handling
C
      IMPLICIT   NONE
      CHARACTER  CINP*20
      INTEGER*4  IPARA(32),IOS,I,IP
      REAL*4     RCOEF(4,32),RSCOF(4),RDCOF(4),ROCOF(4),RAD1,A0,A1,A2,
     -    A3,B0,B1,B2,B3,C0,C1,C2,C3,C4,C5,C6,C7,C8,C9,A,B,C,D,E,F,G

      RAD1=57.3                        !Deg/rad - DO NOT USE MORE ACCURATE VALUE
      DO I=1,4                         !Clear sensor coefficient array
        RSCOF(I)=0.0
      END DO
      WRITE(6,10)                      !Do some blank lines
      WRITE(20,10,IOSTAT=IOS)
      WRITE(21,10,IOSTAT=IOS)
      IF(IPARA(IP).EQ.58) THEN         !General Eastern
        RSCOF(1)=-2.548372E+2
        RSCOF(2)=2.550347
        RSCOF(3)=0.0
      ELSE
        WRITE(6,10)
        WRITE(6,10)
     -  'Enter sensor calibration coeffs as floating point numbers.'
        WRITE(6,10) 'Use 0.0 if absent.'
        WRITE(6,10)
        DO I=1,3                       !Get three sensor calibration coeffs
          IOS=-1
          DO WHILE(IOS.NE.0)           !Loop till have valid input
            WRITE(6,12) 'Sensor  X**',I-1,' coefficient?   '
            READ(5,11,IOSTAT=IOS) CINP
            IF(IOS.EQ.-1) RETURN       !Return immediately on CTRL/Z
            READ(CINP,13,IOSTAT=IOS) RSCOF(I) !Decode input
          END DO
        END DO      
      END IF
C
      DO I=1,4
        RDCOF(I)=RCOEF(I,IP)           !Transfer corresponding DRS coefficients
      END DO
      A0=RDCOF(1)                      !Use short names for coefficients
      A1=RDCOF(2)
      A2=RDCOF(3)
      A3=RDCOF(4)
      B0=RSCOF(1)
      B1=RSCOF(2)
      B2=RSCOF(3)
      B3=RSCOF(4)
      A=A0*A0                          !Compute constants
      B=2*A0*A1
      C=A0*A2+A1*A1+A2*A0
      D=A0*A3+2*A1*A2+A3*A0
      E=2*A1*A3+A2*A2
      F=2*A2*A3
      G=A3*A3
      C0=B0+B1*A0+B2*A+B3*(A0*A)       !Compute resultant coefficients
      C1=   B1*A1+B2*B+B3*(A0*B+A1*A)
      C2=   B1*A2+B2*C+B3*(A0*C+A1*B+A2*A)
      C3=   B1*A3+B2*D+B3*(A0*D+A1*C+A2*B+A3*A)
      C4=         B2*E+B3*(A0*E+A1*D+A2*C+A3*B)
      C5=         B2*F+B3*(A0*F+A1*E+A2*D+A3*C)
      C6=         B2*G+B3*(A0*G+A1*F+A2*E+A3*D)
      C7=              B3*(     A1*G+A2*F+A3*E) 
      C8=              B3*(          A2*G+A3*F)
      C9=              B3*(               A3*G)
      ROCOF(1)=C0                      !Transfer first 4 coeffs to return array
      ROCOF(2)=C1
      ROCOF(3)=C2
      ROCOF(4)=C3
      WRITE(6,10)                      !List results
      WRITE(6,10) '              DRS coeffs    Sensor Coeffs   '//
     -    'Overall Coeffs'
      WRITE(20,10,IOSTAT=IOS) '              DRS coeffs    Sensor '//
     -    'Coeffs   Overall Coeffs'//CHAR(11)
      WRITE(21,10,IOSTAT=IOS) '              DRS coeffs    Sensor '//
     -    'Coeffs   Overall Coeffs'//CHAR(11)
      WRITE(6,10)
      DO I=1,4
        WRITE(6,14) 'X**',I-1,RDCOF(I),RSCOF(I),ROCOF(I)
        WRITE(20,14,IOSTAT=IOS) 'X**',I-1,RDCOF(I),RSCOF(I),ROCOF(I)
        WRITE(21,14,IOSTAT=IOS) 'X**',I-1,RDCOF(I),RSCOF(I),ROCOF(I)
      END DO
      WRITE(6,10)
C
      RETURN                           !Return to calling program

10    FORMAT(X,A)
11    FORMAT(A)
12    FORMAT(X,A,I1,A,$)
13    FORMAT(BN,G)
14    FORMAT(X,A,I1,3X,3(4X,1PE13.6))
15    FORMAT(/X,'Deiced temperature calibration by scale:'//,
     -    10(X,'Scale ',I1,4X,1PE13.6,4X,1PE13.6/))
      END
********************************************************************************
C
C Computes coefficients of a polynomial for calculating PRT temperature from
C resistance.  Requires resistance at 0C and 100C and then uses the
C Callender-Van Deusen equation to calculated resistances for temperatures in 
C the range -80 to +40.  A second order order polynomial is then fitted to
C these values.
C
C V1.01  19/07/91  W D N JACKSON
C
      SUBROUTINE RT_POLY(RSCOF)
      IMPLICIT  NONE
      INTEGER*4 I,IOS,J
      REAL*4    RTEMPS(64),RRESIS(64),RCOEFS(5),RSCOF(4),RA,RD,RT,RR,RB,
     -    R0,R100,RCORR
      DO I=1,4                         !Set default coeffs to 0
       RSCOF(I)=0.0
      END DO
      RSCOF(2)=1.0                     !Default is 1 - unity transformation
      WRITE(6,10) 'Sensor resistance at   0 C ?  '
      READ(5,*,IOSTAT=IOS) R0          !Resistance at 0 C
      IF(IOS.NE.0) RETURN
      WRITE(6,10) 'Sensor resistance at 100 C ?  '
      READ(5,*,IOSTAT=IOS) R100        !Resistance at 100C
      IF(IOS.NE.0) RETURN
      RA=(R100/R0-1.)/100.             !Compute alpha
      RD=1.45                          !Use standard value for delta
      J=0                              !Observation count
      DO I=-80,40,2                    !For full range of temperatures
        IF(I.LT.0) RB=0.1              !Use standard values for beta
        IF(I.GE.0) RB=0.0
        RT=FLOAT(I)                    !Temperature
        RR=1 + RA*RT - RA*RD*RT/100.*(RT/100.-1) !Apply Callender-Van Deusen
     -               - RA*RB*(RT/100.)**3*(RT/100.-1)
        RR=R0*RR
        J=J+1                          !Bump pointer
        RTEMPS(J)=RT                   !Store temp - dependent variable
        RRESIS(J)=RR                   !Store resistance - independent variable
      END DO
      CALL POLY_FIT(6,2,J,RRESIS,RTEMPS,RCOEFS,RCORR) !Fit quadratic
      CALL POLY_FIT(20,2,J,RRESIS,RTEMPS,RCOEFS,RCORR) !Fit quadratic
      CALL POLY_FIT(21,2,J,RRESIS,RTEMPS,RCOEFS,RCORR) !Fit quadratic

      WRITE(6,620,IOSTAT=IOS)
      WRITE(6,630,IOSTAT=IOS) RCOEFS(1)
      WRITE(6,640,IOSTAT=IOS) 1,RCOEFS(2)
      WRITE(6,640,IOSTAT=IOS) 2,RCOEFS(3)

      WRITE(20,620,IOSTAT=IOS)
      WRITE(20,630,IOSTAT=IOS) RCOEFS(1)
      WRITE(20,640,IOSTAT=IOS) 1,RCOEFS(2)
      WRITE(20,640,IOSTAT=IOS) 2,RCOEFS(3)

      WRITE(21,620,IOSTAT=IOS)
      WRITE(21,630,IOSTAT=IOS) RCOEFS(1)
      WRITE(21,640,IOSTAT=IOS) 1,RCOEFS(2)
      WRITE(21,640,IOSTAT=IOS) 2,RCOEFS(3)

      DO I=1,4                         !Transfer coefficients to sensor coeffs
        RSCOF(I)=RCOEFS(I)
      END DO
      RETURN
10    FORMAT(X,A,$)
620   FORMAT(/1H ,'REGRESSION COEFFICIENTS',/)
630   FORMAT(1H ,'X**0      ',1PE13.6)
640   FORMAT(1H ,'X**',I1,6X,1PE13.6)
      END
********************************************************************************
C
C ROUTINE          POLY_FIT SUBROUTINE FORTVAX
C
C PURPOSE          Fits polynomial to set of data points
C
C DESCRIPTION      Carries out a polynomial regression on a set of data points
C                  and returns the polynomial coefficients and estimate of fit.
C
C                  Up to 64 observations can be handled, and polynomials up to
C                  fourth order fitted.  The number of observations must exceed
C                  the polynomial order by at least 2.
C
C                  This routine is based on Doug Johnson's SPOLRG routine,
C                  which is used as  part of his spreadsheet program.  Also
C                  uses scientific subroutines from the RT-11 Scientific
C                  Subroutine Package.  These are included at the end of this 
C                  file.
C
C                  If LUN is specified with a non zero values, then a listing
C                  is done to the specified LUN of the coefficients and the
C                  residual of the fit to each observation.  Use a value of 6
C                  to get the listing on the screen.
C
C VERSION          1.00  16-07-91  W.D.N.JACKSON
C
C ARGUMENTS        LUN        I*4  IN  Logical unit for listing results
C                  IORDER     I*4  IN  Required order of polynomial (1-4)
C                  IPOINTS    I*4  IN  No of observations to be fitted (3-64)
C                  RINDEP(64) R*4  IN  Array of independent variables
C                  RDEPEND(64)R*4  IN  Array of dependent variables
C                  RCOEFS(5)  R*4 OUT  Array of up to 5 of calculated coeffs
C                  RCORR      R*4 OUT  Computed correlation coeff (max 1.0)
C
C CHANGES          
C
********************************************************************************
      SUBROUTINE POLY_FIT(LUN,IORDER,IPOINTS,RINDEP,RDEPEND,RCOEFS,
     -    RCORR)
C
      IMPLICIT NONE
      INTEGER*4 I,IORDER,IOS,IPOINTS,J,L,LA,LUN,M,MM,N,N2,NI,NP2,NP3,NT
      REAL*4 DET,RCORR,RESID,SUM
      REAL*4 RINDEP(64),RDEPEND(64),RCOEFS(5)
      REAL*4 P(192)                    !Dimension 3*N
      REAL*4 X(256)                    !Dimension N*(M+1)
      REAL*4 DI(16)                    !Dimension M*M
      REAL*4 D(15)                     !Dimension (M+2)*(M+1)/2
      REAL*4 B(4),SB(4),T(4),E(4)      !Dimension M
      REAL*4 XBAR(5),STD(5),COE(5),SUMSQ(5) !Dimension (M+1)
      REAL*4 ANS(10)                   !Dimension >=10
      INTEGER*4 ISAVE(5)               !Dimension (M+1)
      INTEGER*4 LR(4),MR(4)
      EQUIVALENCE (LR,B),(MR,T)
C
C Check inputs
C
      M=IORDER
      N=IPOINTS
      IF(N.LT.2.OR.N.GT.64.OR.M.LT.1.OR.M.GT.4.OR.M.GE.N-1) THEN
        WRITE(6,*) 'Invalid input arguments to POLY_FIT routine'
        CALL EXIT
      END IF
C
C Transfer data to arrays
C
      L=M*N
      DO I=1,N
        X(I)=RINDEP(I)
        X(L+I)=RDEPEND(I)
      END DO
C
C Calculate required statistics for polynomial regression.
C
      CALL GDATA(N,M,X,XBAR,STD,D,SUMSQ)
      MM=M+1
      SUM=0.0
      NT=N-1
      DO I=1,M
        ISAVE(I)=I
C
C Form subset of correlation coefficient matrix
C
        CALL ORDER(MM,D,MM,I,ISAVE,DI,E)
C
C Invert the submatrix of correlation coefficients
C
        CALL MINV(DI,I,DET,LR,MR)
        CALL MULTR(N,I,XBAR,STD,SUMSQ,DI,E,ISAVE,B,SB,T,ANS)
        SUM=ANS(4)
        NI=ANS(8)
C
C Save coefficients for calculation of Y estimates
C
        COE(1)=ANS(1)
        DO J=1,I
          COE(J+1)=B(J)
        END DO
        LA=I
      END DO
C
C Return results to calling routine and return if no print out required
C
      RCOEFS(1)=ANS(1)
      DO J=1,M
        RCOEFS(J+1)=B(J)
      END DO
      RCORR=ANS(2)
      IF(LUN.EQ.0) RETURN
C
C Send the results of calculation to output
C
      WRITE(LUN,600,IOSTAT=IOS) M,N
      WRITE(LUN,620,IOSTAT=IOS)
      WRITE(LUN,630,IOSTAT=IOS) ANS(1)
      DO J=1,M
        WRITE(LUN,640,IOSTAT=IOS) J,B(J)
      END DO
      WRITE(LUN,650,IOSTAT=IOS) ANS(2)
C
C Calculate estimates
C
      NP3=N+N
      DO I=1,N
        NP3=NP3+1
        P(NP3)=COE(1)
        L=I
        DO J=1,LA
          P(NP3)=P(NP3)+X(L)*COE(J+1)
          L=L+N
        END DO
      END DO
C
C Copy observed data
C
      N2=N
      L=N*M
      DO I=1,N
        P(I)=X(I)
        N2=N2+1
        L=L+1
        P(N2)=X(L)
      END DO
C
C Print table of residuals
C
      WRITE(LUN,660,IOSTAT=IOS)
      NP2=N
      NP3=N+N
      DO I=1,N
        NP2=NP2+1
        NP3=NP3+1
        RESID=P(NP2)-P(NP3)
        WRITE(LUN,670,IOSTAT=IOS) I,P(I),P(NP2),P(NP3),RESID
      END DO
      RETURN
C
C Format statements
C
600   FORMAT(//1H ,'POLYNOMIAL REGRESSION OF DEGREE ',I1,' WITH ',I2,
     1' DATA POINTS')
620   FORMAT(/1H ,'REGRESSION COEFFICIENTS',/)
630   FORMAT(1H ,'X**0      ',1PE13.6)
640   FORMAT(1H ,'X**',I1,6X,1PE13.6)
650   FORMAT(/,' MULTIPLE CORRELATION COEFFICIENT ',F6.4)
660   FORMAT(/27X,'TABLE OF RESIDUALS',/,/,
     1'   SAMPLE NO.   ',4X,'X VALUE',7X,'Y VALUE',7X,
     2'Y ESTIMATE',7X,'RESIDUAL',/)
670   FORMAT(3X,I6,F18.5,F14.5,F17.5,F15.5)
      END
********************************************************************************
C
C     ..................................................................
C
C        SUBROUTINE GDATA
C
C        PURPOSE
C           GENERATE INDEPENDENT VARIABLES UP TO THE M-TH POWER (THE
C           HIGHEST DEGREE POLYNOMIAL SPECIFIED) AND COMPUTE MEANS,
C           STANDARD DEVIATIONS, AND CORRELATION COEFFICIENTS.  THIS
C           SUBROUTINE IS NORMALLY CALLED BEFORE SUBROUTINES ORDER,
C           MINV AND MULTR IN THE PERFORMANCE OF A POLYNOMIAL
C           REGRESSION.
C
C        USAGE
C           CALL GDATA (N,M,X,XBAR,STD,D,SUMSQ)
C
C        DESCRIPTION OF PARAMETERS
C           N     - NUMBER OF OBSERVATIONS.
C           M     - THE HIGHEST DEGREE POLYNOMIAL TO BE FITTED.
C           X     - INPUT MATRIX (N BY M+1) .  WHEN THE SUBROUTINE IS
C                   CALLED, DATA FOR THE INDEPENDENT VARIABLE ARE
C                   STORED IN THE FIRST COLUMN OF MATRIX X, AND DATA FOR
C                   THE DEPENDENT VARIABLE ARE STORED IN THE LAST
C                   COLUMN OF THE MATRIX.  UPON RETURNING TO THE
C                   CALLING ROUTINE, GENERATED POWERS OF THE INDEPENDENT
C                   VARIABLE ARE STORED IN COLUMNS 2 THROUGH M.
C           XBAR  - OUTPUT VECTOR OF LENGTH M+1 CONTAINING MEANS OF
C                   INDEPENDENT AND DEPENDENT VARIABLES.
C           STD   - OUTPUT VECTOR OF LENGTH M+1 CONTAINING STANDARD
C                   DEVIATIONS OF INDEPENDENT AND DEPENDENT VARIABLES.
C           D     - OUTPUT MATRIX (ONLY UPPER TRIANGULAR PORTION OF THE
C                   SYMMETRIC MATRIX OF M+1 BY M+1) CONTAINING CORRELA-
C                   TION COEFFICIENTS.  (STORAGE MODE OF 1)
C           SUMSQ - OUTPUT VECTOR OF LENGTH M+1 CONTAINING SUMS OF
C                   PRODUCTS OF DEVIATIONS FROM MEANS  OF INDEPENDENT
C                   AND DEPENDENT VARIABLES.
C
C        REMARKS
C           N MUST BE GREATER THAN M+1.
C           IF M IS EQUAL TO 5 OR GREATER, SINGLE PRECISION MAY NOT BE
C           SUFFICIENT TO GIVE SATISFACTORY COMPUTATIONAL RESULTS.
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           REFER TO B. OSTLE, 'STATISTICS IN RESEARCH', THE IOWA STATE
C           COLLEGE PRESS, 1954, CHAPTER 6.
C
C     ..................................................................
C
      SUBROUTINE GDATA (N,M,X,XBAR,STD,D,SUMSQ)
      IMPLICIT NONE
      REAL*4 X(*),XBAR(*),STD(*),D(*),SUMSQ(*)
      INTEGER*4 I,J,K,L,L1,L2,M,MM,N
      REAL*4 T1,T2,DF
C
C        ...............................................................
C
C        IF A DOUBLE PRECISION VERSION OF THIS ROUTINE IS DESIRED, THE
C        C IN COLUMN 1 SHOULD BE REMOVED FROM THE DOUBLE PRECISION
C        STATEMENT WHICH FOLLOWS.
C
C     DOUBLE PRECISION X,XBAR,STD,D,SUMSQ,T1,T2
C
C        THE C MUST ALSO BE REMOVED FROM DOUBLE PRECISION STATEMENTS
C        APPEARING IN OTHER ROUTINES USED IN CONJUNCTION WITH THIS
C        ROUTINE.
C
C        THE DOUBLE PRECISION VERSION OF THIS SUBROUTINE MUST ALSO
C        CONTAIN DOUBLE PRECISION FORTRAN FUNCTIONS.  SQRT AND ABS IN
C        STATEMENT 180 MUST BE CHANGED TO DSQRT AND DABS.
C
C        ...............................................................
C
C     GENERATE INDEPENDENT VARIABLES
C
      IF(M-1) 105, 105, 90
   90 L1=0
      DO 100 I=2,M
      L1=L1+N
      DO 100 J=1,N
      L=L1+J
      K=L-N
  100 X(L)=X(K)*X(J)
C
C     CALCULATE MEANS
C
  105 MM=M+1
      DF=N
      L=0
      DO 115 I=1,MM
      XBAR(I)=0.0
      DO 110 J=1,N
      L=L+1
  110 XBAR(I)=XBAR(I)+X(L)
  115 XBAR(I)=XBAR(I)/DF
C
      DO 130 I=1,MM
  130 STD(I)=0.0
C
C     CALCULATE SUMS OF CROSS-PRODUCTS OF DEVIATIONS
C
      L=((MM+1)*MM)/2
      DO 150 I=1,L
  150 D(I)=0.0
      DO 170 K=1,N
      L=0
      DO 170 J=1,MM
      L2=N*(J-1)+K
      T2=X(L2)-XBAR(J)
      STD(J)=STD(J)+T2
      DO 170 I=1,J
      L1=N*(I-1)+K
      T1=X(L1)-XBAR(I)
      L=L+1
  170 D(L)=D(L)+T1*T2
      L=0
      DO 175 J=1,MM
      DO 175 I=1,J
      L=L+1
  175 D(L)=D(L)-STD(I)*STD(J)/DF
      L=0
      DO 180 I=1,MM
      L=L+I
      SUMSQ(I)=D(L)
  180 STD(I)= SQRT( ABS(D(L)))
C
C     CALCULATE CORRELATION COEFFICIENTS
C
      L=0
      DO 190 J=1,MM
      DO 190 I=1,J
      L=L+1
  190 D(L)=D(L)/(STD(I)*STD(J))
C
C     CALCULATE STANDARD DEVIATIONS
C
      DF=SQRT(DF-1.0)
      DO 200 I=1,MM
  200 STD(I)=STD(I)/DF
      RETURN
      END

********************************************************************************
C
C     ..................................................................
C
C        SUBROUTINE ORDER
C
C        PURPOSE
C           CONSTRUCT FROM A LARGER MATRIX OF CORRELATION COEFFICIENTS
C           A SUBSET MATRIX OF INTERCORRELATIONS AMONG INDEPENDENT
C           VARIABLES AND A VECTOR OF INTERCORRELATIONS OF INDEPENDENT
C           VARIABLES WITH DEPENDENT VARIABLE.  THIS SUBROUTINE IS
C           NORMALLY USED IN THE PERFORMANCE OF MULTIPLE AND POLYNOMIAL
C           REGRESSION ANALYSES.
C
C        USAGE
C           CALL ORDER (M,R,NDEP,K,ISAVE,RX,RY)
C
C        DESCRIPTION OF PARAMETERS
C           M     - NUMBER OF VARIABLES AND ORDER OF MATRIX R.
C           R     - INPUT MATRIX CONTAINING CORRELATION COEFFICIENTS.
C                   THIS SUBROUTINE EXPECTS ONLY UPPER TRIANGULAR
C                   PORTION OF THE SYMMETRIC MATRIX TO BE STORED (BY
C                   COLUMN) IN R.  (STORAGE MODE OF 1)
C           NDEP  - THE SUBSCRIPT NUMBER OF THE DEPENDENT VARIABLE.
C           K     - NUMBER OF INDEPENDENT VARIABLES TO BE INCLUDED
C                  IN THE FORTHCOMING REGRESSION. K MUST BE GREATER
C                  THAN OR EQUAL TO 1.
C           ISAVE - INPUT VECTOR OF LENGTH K+1 CONTAINING, IN ASCENDING
C                   ORDER, THE SUBSCRIPT NUMBERS OF K INDEPENDENT
C                   VARIABLES TO BE INCLUDED IN THE FORTHCOMING REGRES-
C                   SION.
C                   UPON RETURNING TO THE CALLING ROUTINE, THIS VECTOR
C                   CONTAINS, IN ADDITION, THE SUBSCRIPT NUMBER OF
C                   THE DEPENDENT VARIABLE IN K+1 POSITION.
C           RX    - OUTPUT MATRIX (K X K) CONTAINING INTERCORRELATIONS
C                   AMONG INDEPENDENT VARIABLES TO BE USED IN FORTH-
C                   COMING REGRESSION.
C           RY    - OUTPUT VECTOR OF LENGTH K CONTAINING INTERCORRELA-
C                   TIONS OF INDEPENDENT VARIABLES WITH DEPENDENT
C                   VARIABLES.
C
C        REMARKS
C           NONE
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           FROM THE SUBSCRIPT NUMBERS OF THE VARIABLES TO BE INCLUDED
C           IN THE FORTHCOMING REGRESSION, THE SUBROUTINE CONSTRUCTS THE
C           MATRIX RX AND THE VECTOR RY.
C
C     ..................................................................
C
      SUBROUTINE ORDER (M,R,NDEP,K,ISAVE,RX,RY)
      IMPLICIT NONE
      REAL*4 R(*),RX(*),RY(*)
      INTEGER*4 I,J,K,L,L1,L2,M,MM,NDEP,ISAVE(*)
C
C        ...............................................................
C
C        IF A DOUBLE PRECISION VERSION OF THIS ROUTINE IS DESIRED, THE
C        C IN COLUMN 1 SHOULD BE REMOVED FROM THE DOUBLE PRECISION
C        STATEMENT WHICH FOLLOWS.
C
C     DOUBLE PRECISION R,RX,RY
C
C        THE C MUST ALSO BE REMOVED FROM DOUBLE PRECISION STATEMENTS
C        APPEARING IN OTHER ROUTINES USED IN CONJUNCTION WITH THIS
C        ROUTINE.
C
C        ...............................................................
C
C     COPY INTERCORRELATIONS OF INDEPENDENT VARIABLES
C     WITH DEPENDENT VARIABLE
C
      M=M                              !Keep compiler happy
      MM=0
      DO 130 J=1,K
      L2=ISAVE(J)
      IF(NDEP-L2) 122, 123, 123
  122 L=NDEP+(L2*L2-L2)/2
      GO TO 125
  123 L=L2+(NDEP*NDEP-NDEP)/2
  125 RY(J)=R(L)
C
C     COPY A SUBSET MATRIX OF INTERCORRELATIONS AMONG
C     INDEPENDENT VARIABLES
C
      DO 130 I=1,K
      L1=ISAVE(I)
      IF(L1-L2) 127, 128, 128
  127 L=L1+(L2*L2-L2)/2
      GO TO 129
  128 L=L2+(L1*L1-L1)/2
  129 MM=MM+1
  130 RX(MM)=R(L)
C
C     PLACE THE SUBSCRIPT NUMBER OF THE DEPENDENT
C     VARIABLE IN ISAVE(K+1)
C
      ISAVE(K+1)=NDEP
      RETURN
      END

********************************************************************************
C
C     ..................................................................
C
C        SUBROUTINE MINV
C
C        PURPOSE
C           INVERT A MATRIX
C
C        USAGE
C           CALL MINV(A,N,D,L,M)
C
C        DESCRIPTION OF PARAMETERS
C           A - INPUT MATRIX, DESTROYED IN COMPUTATION AND REPLACED BY
C               RESULTANT INVERSE.
C           N - ORDER OF MATRIX A
C           D - RESULTANT DETERMINANT
C           L - WORK VECTOR OF LENGTH N
C           M - WORK VECTOR OF LENGTH N
C
C        REMARKS
C           MATRIX A MUST BE A GENERAL MATRIX
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           THE STANDARD GAUSS-JORDAN METHOD IS USED. THE DETERMINANT
C           IS ALSO CALCULATED. A DETERMINANT OF ZERO INDICATES THAT
C           THE MATRIX IS SINGULAR.
C
C     ..................................................................
C
      SUBROUTINE MINV(A,N,D,L,M)
      IMPLICIT NONE
      INTEGER*4 I,IJ,IK,IZ,J,JI,JK,JP,JQ,JR,K,KI,KJ,KK,N,NK
      REAL*4 BIGA,D,HOLD,A(*)
      INTEGER*4 L(*),M(*)
C
C        ...............................................................
C
C        IF A DOUBLE PRECISION VERSION OF THIS ROUTINE IS DESIRED, THE
C        C IN COLUMN 1 SHOULD BE REMOVED FROM THE DOUBLE PRECISION
C        STATEMENT WHICH FOLLOWS.
C
C     DOUBLE PRECISION A,D,BIGA,HOLD,DABS
C
C        THE C MUST ALSO BE REMOVED FROM DOUBLE PRECISION STATEMENTS
C        APPEARING IN OTHER ROUTINES USED IN CONJUNCTION WITH THIS
C        ROUTINE.
C
C        THE DOUBLE PRECISION VERSION OF THIS SUBROUTINE MUST ALSO
C        CONTAIN DOUBLE PRECISION FORTRAN FUNCTIONS.  ABS IN STATEMENT
C        10 MUST BE CHANGED TO DABS.
C
C        ...............................................................
C
C        SEARCH FOR LARGEST ELEMENT
C
      D=1.0
      NK=-N
      DO 80 K=1,N
      NK=NK+N
      L(K)=K
      M(K)=K
      KK=NK+K
      BIGA=A(KK)
      DO 20 J=K,N
      IZ=N*(J-1)
      DO 20 I=K,N
      IJ=IZ+I
   10 IF( ABS(BIGA)- ABS(A(IJ))) 15,20,20
   15 BIGA=A(IJ)
      L(K)=I
      M(K)=J
   20 CONTINUE
C
C        INTERCHANGE ROWS
C
      J=L(K)
      IF(J-K) 35,35,25
   25 KI=K-N
      DO 30 I=1,N
      KI=KI+N
      HOLD=-A(KI)
      JI=KI-K+J
      A(KI)=A(JI)
   30 A(JI) =HOLD
C
C        INTERCHANGE COLUMNS
C
   35 I=M(K)
      IF(I-K) 45,45,38
   38 JP=N*(I-1)
      DO 40 J=1,N
      JK=NK+J
      JI=JP+J
      HOLD=-A(JK)
      A(JK)=A(JI)
   40 A(JI) =HOLD
C
C        DIVIDE COLUMN BY MINUS PIVOT (VALUE OF PIVOT ELEMENT IS
C        CONTAINED IN BIGA)
C
   45 IF(BIGA) 48,46,48
   46 D=0.0
      RETURN
   48 DO 55 I=1,N
      IF(I-K) 50,55,50
   50 IK=NK+I
      A(IK)=A(IK)/(-BIGA)
   55 CONTINUE
C
C        REDUCE MATRIX
C
      DO 65 I=1,N
      IK=NK+I
      HOLD=A(IK)
      IJ=I-N
      DO 65 J=1,N
      IJ=IJ+N
      IF(I-K) 60,65,60
   60 IF(J-K) 62,65,62
   62 KJ=IJ-I+K
      A(IJ)=HOLD*A(KJ)+A(IJ)
   65 CONTINUE
C
C        DIVIDE ROW BY PIVOT
C
      KJ=K-N
      DO 75 J=1,N
      KJ=KJ+N
      IF(J-K) 70,75,70
   70 A(KJ)=A(KJ)/BIGA
   75 CONTINUE
C
C        PRODUCT OF PIVOTS
C
      D=D*BIGA
C
C        REPLACE PIVOT BY RECIPROCAL
C
      A(KK)=1.0/BIGA
   80 CONTINUE
C
C        FINAL ROW AND COLUMN INTERCHANGE
C
      K=N
  100 K=(K-1)
      IF(K) 150,150,105
  105 I=L(K)
      IF(I-K) 120,120,108
  108 JQ=N*(K-1)
      JR=N*(I-1)
      DO 110 J=1,N
      JK=JQ+J
      HOLD=A(JK)
      JI=JR+J
      A(JK)=-A(JI)
  110 A(JI) =HOLD
  120 J=M(K)
      IF(J-K) 100,100,125
  125 KI=K-N
      DO 130 I=1,N
      KI=KI+N
      HOLD=A(KI)
      JI=KI-K+J
      A(KI)=-A(JI)
  130 A(JI) =HOLD
      GO TO 100
  150 RETURN
      END
********************************************************************************
C
C     ..................................................................
C
C        SUBROUTINE MULTR
C
C        PURPOSE
C           PERFORM A MULTIPLE LINEAR REGRESSION ANALYSIS FOR A
C           DEPENDENT VARIABLE AND A SET OF INDEPENDENT VARIABLES.  THIS
C           SUBROUTINE IS NORMALLY USED IN THE PERFORMANCE OF MULTIPLE
C           AND POLYNOMIAL REGRESSION ANALYSES.
C
C        USAGE
C           CALL MULTR (N,K,XBAR,STD,D,RX,RY,ISAVE,B,SB,T,ANS)
C
C        DESCRIPTION OF PARAMETERS
C           N     - NUMBER OF OBSERVATIONS.
C           K     - NUMBER OF INDEPENDENT VARIABLES IN THIS REGRESSION.
C           XBAR  - INPUT VECTOR OF LENGTH M CONTAINING MEANS OF ALL
C                   VARIABLES. M IS NUMBER OF VARIABLES IN OBSERVATIONS.
C           STD   - INPUT VECTOR OF LENGTH M CONTAINING STANDARD DEVI-
C                   ATIONS OF ALL VARIABLES.
C           D     - INPUT VECTOR OF LENGTH M CONTAINING THE DIAGONAL OF
C                   THE MATRIX OF SUMS OF CROSS-PRODUCTS OF DEVIATIONS
C                   FROM MEANS FOR ALL VARIABLES.
C           RX    - INPUT MATRIX (K X K) CONTAINING THE INVERSE OF
C                   INTERCORRELATIONS AMONG INDEPENDENT VARIABLES.
C           RY    - INPUT VECTOR OF LENGTH K CONTAINING INTERCORRELA-
C                   TIONS OF INDEPENDENT VARIABLES WITH DEPENDENT
C                   VARIABLE.
C           ISAVE - INPUT VECTOR OF LENGTH K+1 CONTAINING SUBSCRIPTS OF
C                   INDEPENDENT VARIABLES IN ASCENDING ORDER.  THE
C                   SUBSCRIPT OF THE DEPENDENT VARIABLE IS STORED IN
C                   THE LAST, K+1, POSITION.
C           B     - OUTPUT VECTOR OF LENGTH K CONTAINING REGRESSION
C                   COEFFICIENTS.
C           SB    - OUTPUT VECTOR OF LENGTH K CONTAINING STANDARD
C                   DEVIATIONS OF REGRESSION COEFFICIENTS.
C           T     - OUTPUT VECTOR OF LENGTH K CONTAINING T-VALUES.
C           ANS   - OUTPUT VECTOR OF LENGTH 10 CONTAINING THE FOLLOWING
C                   INFORMATION..
C                   ANS(1)  INTERCEPT
C                   ANS(2)  MULTIPLE CORRELATION COEFFICIENT
C                   ANS(3)  STANDARD ERROR OF ESTIMATE
C                   ANS(4)  SUM OF SQUARES ATTRIBUTABLE TO REGRES-
C                           SION (SSAR)
C                   ANS(5)  DEGREES OF FREEDOM ASSOCIATED WITH SSAR
C                   ANS(6)  MEAN SQUARE OF SSAR
C                   ANS(7)  SUM OF SQUARES OF DEVIATIONS FROM REGRES-
C                           SION (SSDR)
C                   ANS(8)  DEGREES OF FREEDOM ASSOCIATED WITH SSDR
C                   ANS(9)  MEAN SQUARE OF SSDR
C                   ANS(10) F-VALUE
C
C        REMARKS
C           N MUST BE GREATER THAN K+1.
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           THE GAUSS-JORDAN METHOD IS USED IN THE SOLUTION OF THE
C           NORMAL EQUATIONS.  REFER TO W. W. COOLEY AND P. R. LOHNES,
C           'MULTIVARIATE PROCEDURES FOR THE BEHAVIORAL SCIENCES',
C           JOHN WILEY AND SONS, 1962, CHAPTER 3, AND B. OSTLE,
C           'STATISTICS IN RESEARCH', THE IOWA STATE COLLEGE PRESS,
C           1954, CHAPTER 8.
C
C     ..................................................................
C
      SUBROUTINE MULTR (N,K,XBAR,STD,D,RX,RY,ISAVE,B,SB,T,ANS)
      IMPLICIT NONE
      INTEGER*4 I,J,K,L,L1,MM,N,ISAVE(*)
      REAL*4 BO,F,FK,FN,RM,SSAR,SSARM,SSDR,SSDRM,SY
      REAL*4 XBAR(*),STD(*),D(*),RX(*),RY(*),B(*),SB(*),
     1          T(*),ANS(*)
C
C        ...............................................................
C
C        IF A DOUBLE PRECISION VERSION OF THIS ROUTINE IS DESIRED, THE
C        C IN COLUMN 1 SHOULD BE REMOVED FROM THE DOUBLE PRECISION
C        STATEMENT WHICH FOLLOWS.
C
C     DOUBLE PRECISION XBAR,STD,D,RX,RY,B,SB,T,ANS,RM,BO,SSAR,SSDR,SY,
C    1                 FN,FK,SSARM,SSDRM,F,DSQRT,DABS
C
C        THE C MUST ALSO BE REMOVED FROM DOUBLE PRECISION STATEMENTS
C        APPEARING IN OTHER ROUTINES USED IN CONJUNCTION WITH THIS
C        ROUTINE.
C
C        THE DOUBLE PRECISION VERSION OF THIS SUBROUTINE MUST ALSO
C        CONTAIN DOUBLE PRECISION FORTRAN FUNCTIONS.  SQRT AND ABS IN
C        STATEMENTS 122, 125, AND 135 MUST BE CHANGED TO DSQRT AND DABS.
C
C        ...............................................................
C
      MM=K+1
C
C        BETA WEIGHTS
C
      DO 100 J=1,K
  100 B(J)=0.0
      DO 110 J=1,K
      L1=K*(J-1)
      DO 110 I=1,K
      L=L1+I
  110 B(J)=B(J)+RY(I)*RX(L)
      RM=0.0
      BO=0.0
      L1=ISAVE(MM)
C
C        COEFFICIENT OF DETERMINATION
C
      DO 120 I=1,K
      RM=RM+B(I)*RY(I)
C
C        REGRESSION COEFFICIENTS
C
      L=ISAVE(I)
      B(I)=B(I)*(STD(L1)/STD(L))
C
C        INTERCEPT
C
  120 BO=BO+B(I)*XBAR(L)
      BO=XBAR(L1)-BO
C
C        SUM OF SQUARES ATTRIBUTABLE TO REGRESSION
C
      SSAR=RM*D(L1)
C
C        MULTIPLE CORRELATION COEFFICIENT
C
  122 RM= SQRT( ABS(RM))
C
C        SUM OF SQUARES OF DEVIATIONS FROM REGRESSION
C
      SSDR=D(L1)-SSAR
C
C        VARIANCE OF ESTIMATE
C
      FN=N-K-1
      SY=SSDR/FN
C
C        STANDARD DEVIATIONS OF REGRESSION COEFFICIENTS
C
      DO 130 J=1,K
      L1=K*(J-1)+J
      L=ISAVE(J)
  125 SB(J)= SQRT( ABS((RX(L1)/D(L))*SY))
C
C        COMPUTED T-VALUES
C
	IF(SB(J).NE.0.)T(J)=B(J)/SB(J)
 130	CONTINUE
C
C        STANDARD ERROR OF ESTIMATE
C
  135 SY= SQRT( ABS(SY))
C
C        F VALUE
C
      FK=K
      SSARM=SSAR/FK
	IF(FN.NE.0.)SSDRM=SSDR/FN
	IF(SSDRM.NE.0)F=SSARM/SSDRM
C
      ANS(1)=BO
      ANS(2)=RM
      ANS(3)=SY
      ANS(4)=SSAR
      ANS(5)=FK
      ANS(6)=SSARM
      ANS(7)=SSDR
      ANS(8)=FN
      ANS(9)=SSDRM
      ANS(10)=F
      RETURN
      END
!*******************************************************************************
      SUBROUTINE DATE1(CDATE)
!
! Replaces the DATE Fortran instrinsic and allows programs to compile without
! Y2000 warnings.  Returns date as DD-MMM-YY.
!
      IMPLICIT  NONE
      INTEGER*4 IVAL(8)
      CHARACTER CMON*3(12),CDATE*9,CDATE1*8
      DATA      CMON /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     &    'SEP','OCT','NOV','DEC'/

      CALL DATE_AND_TIME(CDATE1,,,IVAL)
      WRITE(CDATE,'(A)') CDATE1(7:8)//'-'//CMON(IVAL(2))//'-'//
     &    CDATE1(3:4)
      RETURN
      END
!*******************************************************************************
      SUBROUTINE STR_COMPRESS(CLINE)
!
! Removes multiple spaces from an 80 char string, and converts tabs to spaces
! Also removes leading spaces
!
      IMPLICIT  NONE
      INTEGER*4 I,IL,LAN
      CHARACTER CLINE*80,CTEMP*80

      I=1
      DO WHILE(CLINE(I:I).EQ.' '.AND.I.LT.79)
        I=I+1
      END DO
      CTEMP=CLINE(I:)                  !Remove leading spaces
      CLINE=CTEMP
      DO I=1,LAN(CTEMP)
        IF(ICHAR(CTEMP(I:I)).EQ.9) CTEMP(I:I)=' ' !Convert tabs to spaces
        IF(ICHAR(CLINE(I:I)).EQ.9) CLINE(I:I)=' ' !Convert tabs to spaces
      END DO
      IL=1
      DO I=2,LAN(CTEMP)
        IF(CTEMP(I:I).NE.' ') THEN
          IL=IL+1
          CLINE(IL:IL)=CTEMP(I:I)
        ELSE IF(CLINE(IL:IL).NE.' ') THEN
          IL=IL+1
          CLINE(IL:IL)=' '
        END IF
      END DO
      IF(CLINE(IL:IL).EQ.' ') IL=IL-1
      IF(IL.LT.80) CLINE(IL+1:)=' '    !Blank fill remainder of string
      RETURN
      END
C*******************************************************************************
      SUBROUTINE DRS_NAMES_READ(CSNAME,CFNAME)
C
C Reads in the DRS parameter short and long names from the file DRS_NAMES.TXT.
C
C V1.01  31/05/02  W.D.N.JACKSON
C
      IMPLICIT  NONE
      INTEGER*4 IOS,LUN,IPARAM
      CHARACTER CSNAME(512)*4,CFNAME(512)*20,CLINE*80

      DO IPARAM=1,512
        CSNAME(IPARAM)=' '
        CFNAME(IPARAM)=' '
      END DO
      CALL LIB$GET_LUN(LUN)            !Get a logical unit number
      OPEN(UNIT=LUN,FILE='[HORACE]DRS_NAMES.TXT',STATUS='OLD',READONLY,
     -    IOSTAT=IOS)
      IF(IOS.NE.0) THEN                !Return if any problems
        CALL LIB$FREE_LUN(LUN)
        RETURN
      END IF
      READ(LUN,'(A)',IOSTAT=IOS) CLINE !Read each line
      DO WHILE(IOS.EQ.0)               !Until get to end of file
        READ(CLINE(2:5),'(I4)',IOSTAT=IOS) IPARAM !Decode parameter number
        IF(IPARAM.GT.0.AND.IPARAM.LE.512) THEN !If valid
          CSNAME(IPARAM)=CLINE(14:17)  !Store short name
          CFNAME(IPARAM)=CLINE(30:49)  !Store long name
        END IF
        READ(LUN,'(A)',IOSTAT=IOS) CLINE !Read next line
      END DO
      CLOSE(UNIT=LUN,IOSTAT=IOS)       !Close file
      CALL LIB$FREE_LUN(LUN)           !Free logical unit
      RETURN
      END
C*******************************************************************************
      INTEGER*4 FUNCTION LAN(CSTRING)
      IMPLICIT  NONE
      CHARACTER CSTRING*(*)
      INTEGER*4 N

      N=LEN(CSTRING)                   !Get string length
      DO WHILE(N.GT.0.AND.CSTRING(N:N).EQ.' ') !Step back over spaces
        N=N-1
      END DO
      IF(N.EQ.0) N=1                   !Special case
      LAN=N
      RETURN
      END
