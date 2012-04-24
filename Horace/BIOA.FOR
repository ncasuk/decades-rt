! Alpha version
C
C ROUTINE          BIO SUBROUTINES FORTVAX
C
C PURPOSE          A set of routines for async block I/O, used on raw data
C
C DESCRIPTION      A set of routines for performing block IO under FORTRAN,
C                  with the ability to execute IO asynchronously.  Block IO is
C                  faster than FORTRAN IO since data is transferred directly
C                  into/out of the user's buffer.  Asynchronous operations also
C                  allows processing to take place while data is being read or
C                  written.
C                  
C                  Note that block IO consists essentially of reading/writing
C                  512 byte blocks to/from disk, without any interpretation of
C                  the record or data structure. To produce files capable of
C                  being used with FORTRAN read/write statements these routines
C                  are designed to work only with files with the following
C                  characteristics:
C                  
C                     Sequential
C                     Fixed length records - multiples of 512 bytes
C                     Unformatted
C                  
C                  All access is sequential.  It is possible to handle files
C                  with other characteristics using these routines, but this
C                  requires a deeper  understanding of what is happening and
C                  the methods are not explained here.
C                  
C                  The following routines are available and are documented in
C                  detail in each routine:
C                  
C                  BIO_CLOSE(lun)                 Close file opened for block IO
C                  BIO_CREATE(lun,filnam,ireclb)  Creates a new file for block IO
C                  BIO_OPEN(lun,filnam,ireclb)    Opens an existing file for block IO
C                  BIO_READ(lun,ibuff,ibytes)     Reads block(s) asynchronously
C                  BIO_SPACE(lun,iblks)           Reposition to required block
C                  BIO_WAITR(lun,ibytes)          Wait for read to complete
C                  BIO_WAITW(lun,ibytes)          Wait for write to complete
C                  BIO_WRITE(lun,ibuff,ibytes)    Write block(s) asynchronously
C                  
C                  Best performance will be achieved by using double buffering
C                  and reading or writing many blocks at a time.
C
C VERSION          1.00  21-12-89  W.D.N.JACKSON
C
C ARGUMENTS        
C
C CHANGES          1.01  28-10-93  W.D.N.JACKSON
C                  BIO_WAITR modified to allow it to return with an error
C                  status, rather than stopping, on a read error.  This is
C                  for use by the READ_OPTIC program.
C
*******************************************************************************
C
C BIO_CLOSE
C
C Closes a file opened for block IO with the BIO_OPEN or BIO_CREATE
C subroutines.  If the file was opened with BIO_CREATE then any allocated
C but unused space at the end of the file will be truncated.  The routine
C is synchronous and only returns after the file is successfully closed.
C Any error arising on closure is ignored.
C
C CALL BIO_CLOSE(lun)
C
C where lun is the FORTRAN logical unit associated with the file when it
C was opened.
C
C Standard FORTRAN statements are used to invoke the RMS close routine and to
C close the file.
C
      SUBROUTINE BIO_CLOSE(lun)
      INTEGER lun,IOS
C
      CLOSE(UNIT=lun,IOSTAT=IOS)  !Ignore any error
      RETURN
      END
*******************************************************************************
C
C BIO_CREATE
C
C Creates a file for writing with block IO.  To enable the file to be used
C later by ordinary FORTRAN programs the file is created with unformatted
C fixed length records, using a user specified record length, and as a
C sequential file.
C
C CALL BIO_CREATE(lun,filnam,ireclb)
C
C where lun    is the FORTRAN logical unit to be associated with the file.
C
C       filnam is a character variable containing the file name.
C
C       ireclb is the record length in bytes to be specified with the file.
C              This value is not used during block IO processing so any value
C              could be used.  However later processing will be easier if it
C              is a multiple of 512 bytes and corresponds to some logical
C              quantity of data.
C
C Standard FORTRAN OPEN statement is used for simplicity and clarity, using
C the USEROPEN feature to create the file with block IO enabled.  For 
C efficiency the file is created with a fairly large allocation and therafter
C extended in fairly large steps.  When the file is closed all allocated but
C unused space is trunated.
C
      SUBROUTINE BIO_CREATE(lun,filnam,ireclb)
      integer lun,ireclb
C
      CHARACTER*(*) filnam
      EXTERNAL RMSBLK_CREATE
      OPEN(UNIT=lun,FILE=filnam,STATUS='NEW',USEROPEN=RMSBLK_CREATE,
     -    ORGANIZATION='SEQUENTIAL',ACCESS='SEQUENTIAL',
     -    FORM='UNFORMATTED',RECORDTYPE='FIXED',RECL=ireclb/4,
     -    INITIALSIZE=1000,EXTENDSIZE=1000)
      RETURN
      END
*******************************************************************************
C
C BIO_OPEN
C
C Opens a file for reading with block IO.  If the file does not exist the
C program terminates.  It is easiest to use file with unformatted data in
C fixed length records, although this routine will work with any type of file.
C
C CALL BIO_OPEN(lun,filnam,ireclb)
C
C where lun    is the FORTRAN logical unit to be associated with the file.
C
C       filnam is a character variable containing the file name.
C
C       ireclb is returned with the largest number of bytes in a record in
C              the file.  For fixed length record files this will be the
C              size of each record in bytes.
C
C Standard FORTRAN OPEN statement is used for simplicity and clarity, using
C the USEROPEN feature to open the file with block IO enabled.
C
      SUBROUTINE BIO_OPEN(lun,filnam,ireclb)
      integer lun,ireclb
C
      CHARACTER*(*) filnam
      EXTERNAL RMSBLK_OPEN
      OPEN(UNIT=lun,FILE=filnam,STATUS='OLD',READONLY,
     -    USEROPEN=RMSBLK_OPEN)
      INQUIRE(UNIT=lun,RECL=ireclb)
      RETURN
      END
*******************************************************************************
C
C BIO_READ
C
C Initiates a block read operation on a specified FORTRAN logical unit.  The
C routine returns without waiting for the operation to complete.  Reading
C starts at the current location in the file.
C
C CALL BIO_READ(lun,ibuff,ibytes)
C
C where lun    is the FORTRAN logical unit associated with the file when it
C              was opened.
C
C       ibuff  is the buffer into which the read data is to be placed.  This
C              should be large enough to hold ibytes of data.  For best
C              performance this buffer should be page aligned.
C
C       ibytes is the number of bytes to be read.  This should be a multiple
C              of the file block size, ie 512 bytes for disk files and
C              the physical block size for tape files.
C
C SYS$READ is used to carry out the read operation.  Reading starts at the
C current location in the file.
C
      SUBROUTINE BIO_READ(lun,ibuff,ibytes)

C
      INCLUDE    '($SYSSRVNAM)'
      INCLUDE    '($RMSDEF)'
      INTEGER*8  RABADD
      integer lun,ibuff,ibytes,jstat
C
      RABADD=FOR$RAB(lun)
      CALL RMSBLK_READ(%VAL(RABADD),ibuff,ibytes)
      JSTAT=SYS$READ(%VAL(RABADD))                          !Read
      IF(JSTAT.NE.RMS$_NORMAL.AND.JSTAT.NE.RMS$_PENDING) 
     -    CALL LIB$STOP(%VAL(JSTAT))
      RETURN
      END
*******************************************************************************
C
C BIO_SPACE
C
C Skips forwards or backwards a specified number of blocks in a sequential
C file.  Note that block size for disks is 512 bytes, whereas it is dependant
C on the tape block size for tapes.  The routine is synchronous and only
C returns when the blocks have been successfully skipped.  Any error arising
C during the skip operation will lead to a message and termination of the
C program.
C
C CALL BIO_SPACE(lun,iblks)
C
C where lun   is the FORTRAN logical unit associated with the file when it
C             was opened.
C
C       iblks is the number of blocks to skip +ve forwards, -ve backwards
C
C SYS$SPACE is used to do the skipping.  For disk files all that needs to be
C done is to adjust the current block pointer.  For tapes the tape is actually
C repositioned.
C
      SUBROUTINE BIO_SPACE(lun,iblks)
CDEC$ IDENT 'V1.00'
C
      INCLUDE    '($SYSSRVNAM)'
      INCLUDE    '($RMSDEF)'
      integer lun,iblks,jstat
      INTEGER*8  RABADD
C
      RABADD=FOR$RAB(lun)
      CALL RMSBLK_SPACE(%VAL(RABADD),iblks)
      JSTAT=SYS$SPACE(%VAL(RABADD))                          !Space
      IF(JSTAT.NE.RMS$_NORMAL) CALL LIB$STOP(%VAL(JSTAT))
      RETURN
      END
*******************************************************************************
C
C BIO_WAITR
C
C Waits for an asynchronous read on a specified FORTRAN logical unit
C to complete.  If there is no operation in progress the routine returns
C immediately.  If the operation resulted in any error except end of file
C the program is terminated.  If an end of file was detected during a read
C operation the routine returns the number of bytes actually read into the
C user buffer.  Users can detect the end of file by noting that the number of
C bytes returned is less than the number of bytes requested.
C
C CALL BIO_WAITR(lun,ibytes)
C
C where lun    is the FORTRAN logical unit associated with the file when it
C              was opened.
C
C       ibytes is the number of bytes actually read in a read operation or
C              the number of bytes specified for a write operation.
C
C SYS$WAIT is used to await completion of the IO operation on the specified
C RAB.  The status value returned by SYS$WAIT is that of the completed 
C operation.  Any error except end of file causes the program to be terminated
C with a message.  Otherwise the number of bytes transferred is retrieved from
C the RAB and the routine returns.
C
C V1.01  28-10-93  W.D.N.JACKSON
C        If this routine is called with a value of IBYTES equal to -9999 then
C        it will return with an IBYTES value of -1 rather than stop.
C
      SUBROUTINE BIO_WAITR(lun,ibytes)
C
      INCLUDE    '($SYSSRVNAM)'
      INCLUDE    '($RMSDEF)'
      INTEGER*8  RABADD
      integer lun,ibytes,jstat
C
      RABADD=FOR$RAB(lun)
      JSTAT=SYS$WAIT(%VAL(RABADD))     !Wait
      IF(JSTAT.NE.RMS$_NORMAL.AND. JSTAT.NE.RMS$_EOF) THEN
        IF(ibytes.NE.-9999) THEN
          CALL LIB$STOP(%VAL(JSTAT))
        ELSE
          ibytes=-1
        END IF
      ELSE
        CALL RMSBLK_WAITR(%VAL(RABADD),ibytes)
      END IF
      RETURN
      END
*******************************************************************************
C
C BIO_WAITW
C
C Waits for an asynchronous write on a specified FORTRAN logical unit
C to complete.  If there is no operation in progress the routine returns
C immediately.  If the operation resulted in any error except device full
C the program is terminated.  If device full was detected 
C the routine returns the number of bytes actually written from the
C user buffer.  Users can detect the end of file by noting that the number of
C bytes returned is less than the number of bytes requested.
C
C CALL BIO_WAITW(lun,ibytes)
C
C where lun    is the FORTRAN logical unit associated with the file when it
C              was opened.
C
C       ibytes is the number of bytes actually written.
C
C SYS$WAIT is used to await completion of the IO operation on the specified
C RAB.  The status value returned by SYS$WAIT is that of the completed 
C operation.  Any error except end of file causes the program to be terminated
C with a message.  Otherwise the number of bytes transferred is retrieved from
C the RAB and the routine returns.
C
      SUBROUTINE BIO_WAITW(lun,ibytes)
C
      INCLUDE    '($SYSSRVNAM)'
      INCLUDE    '($RMSDEF)'
      INTEGER*8  RABADD
      integer lun,ibytes,jstat
C
      RABADD=FOR$RAB(lun)
      JSTAT=SYS$WAIT(%VAL(RABADD))                          !Wait
      IF(JSTAT.NE.RMS$_NORMAL.AND. JSTAT.NE.RMS$_EOF) 
     -    CALL LIB$STOP(%VAL(JSTAT))
      CALL RMSBLK_WAITW(%VAL(RABADD),ibytes)
      RETURN
      END
*******************************************************************************
C
C BIO_WRITE
C
C Initiates a block write operation on a specified FORTRAN logical unit.  The
C routine returns without waiting for the operation to complete.  Writing
C starts at the current location in the file.  The file is automatically
C extended as required.
C
C CALL BIO_WRITE(lun,ibuff,ibytes)
C
C where lun    is the FORTRAN logical unit associated with the file when it
C              was opened.
C
C       ibuff  is the buffer from which the data is to be written.  For best
C              performance this buffer should be page aligned.
C
C       ibytes is the number of bytes to be written.  This should be a multiple
C              of the file block size, ie 512 bytes for disk files and
C              the physical block size for tape files.
C
C SYS$WRITE is used to carry out the write operation.  Writing starts at the
C current location in the file.
C
      SUBROUTINE BIO_WRITE(lun,ibuff,ibytes)
C
      INCLUDE    '($SYSSRVNAM)'
      INCLUDE    '($RMSDEF)'
      INTEGER*8  RABADD
      integer lun,ibuff,ibytes,jstat
C
      RABADD=FOR$RAB(lun)
      CALL RMSBLK_WRITE(%VAL(RABADD),ibuff,ibytes)
      JSTAT=SYS$WRITE(%VAL(RABADD))                          !Write
      IF(JSTAT.NE.RMS$_NORMAL.AND.JSTAT.NE.RMS$_PENDING) 
     -    CALL LIB$STOP(%VAL(JSTAT))
      RETURN
      END
**************************************************************************
      INTEGER FUNCTION RMSBLK_CREATE(FABNAM,RABNAM,LUN)
C
C This function is called by FORTRAN as part of its OPEN
C procedure.  It allows block IO to be specified for the opened file.
C The function returns the result of the create and connect services, however
C FORTRAN converts the result into FORTRAN error codes which are not the same.
C
      INCLUDE   '($SYSSRVNAM)'
      INCLUDE   '($FABDEF)'              !File access block definitions
      INCLUDE   '($RABDEF)'              !Record access block definitions
      INCLUDE   '($RMSDEF)'              !RMS status definitions
      integer lun,idummy,jstat
      RECORD    /FABDEF/ FABNAM          !Declare the File Access Block
      RECORD    /RAB64DEF/ RABNAM        !Declare the Record Access Block
C
C External data
C
C RMSBLK_CREATE I*4  write   Returned       Returned status of create or connect
C FABNAM        REC  read    Passed         FAB name from FORTRAN
C RABNAM        REC  read    Passed         RAB name from FORTRAN
C LUN           I*4  read    Passed         LUN number from FORTRAN
C
      IDUMMY=LUN                       !Keep compiler happy
      FABNAM.FAB$B_FAC=FABNAM.FAB$B_FAC.OR.FAB$M_BIO !Use block I/O
      FABNAM.FAB$L_FOP=FABNAM.FAB$L_FOP.OR.FAB$M_TEF !Truncate file on close
      JSTAT=SYS$CREATE(FABNAM)                       !Create file
      IF(JSTAT.NE.RMS$_NORMAL) CALL LIB$STOP(%VAL(JSTAT))
      JSTAT=SYS$CONNECT(RABNAM)                      !Connect to it
      IF(JSTAT.NE.RMS$_NORMAL) CALL LIB$STOP(%VAL(JSTAT))
      RMSBLK_CREATE=JSTAT
      RETURN
      END
**************************************************************************
      INTEGER FUNCTION RMSBLK_OPEN(FABNAM,RABNAM,LUN)
CDEC$ IDENT 'V1.00'
C
C This function is called by FORTRAN as part of its OPEN
C procedure.  It allows block IO to be specified for the opened file.
C The function returns the result of the open and connect services, however
C FORTRAN converts the result into FORTRAN error codes which are not the same.
C
      INCLUDE   '($SYSSRVNAM)'
      INCLUDE   '($FABDEF)'              !File access block definitions
      INCLUDE   '($RABDEF)'              !Record access block definitions
      INCLUDE   '($RMSDEF)'              !RMS status definitions
      integer lun,idummy,jstat
      RECORD    /FABDEF/ FABNAM          !Declare the File Access Block
      RECORD    /RAB64DEF/ RABNAM        !Declare the Record Access Block
C
C External data
C
C RMSBLK_OPEN I*4  write   Returned       Returned status of create or connect
C FABNAM      REC  read    Passed         FAB name from FORTRAN
C RABNAM      REC  read    Passed         RAB name from FORTRAN
C LUN         I*4  read    Passed         LUN number from FORTRAN
C
      IDUMMY=LUN                       !Keep compiler happy
      FABNAM.FAB$B_FAC=FABNAM.FAB$B_FAC.OR.FAB$M_BIO.OR.FAB$M_GET !Use block I/O
      JSTAT=SYS$OPEN(FABNAM)                     !Open file
      IF(JSTAT.NE.RMS$_NORMAL) CALL LIB$STOP(%VAL(JSTAT))
      JSTAT=SYS$CONNECT(RABNAM)                  !Connect to it
      IF(JSTAT.NE.RMS$_NORMAL) CALL LIB$STOP(%VAL(JSTAT))
      RMSBLK_OPEN=JSTAT
      RETURN
      END
*******************************************************************************
      SUBROUTINE RMSBLK_READ(RABNAM,ibuff,ibytes)
C
C Modifies the specified Record Access Block (RAB) to provide the necessary
C information for the SYS$READ RMS service.
C Loads the byte count.
C Loads the user buffer address.
C Sets the bucket count to 0 so that current block pointer is used.
C Sets the asynchronous operation bit.
C
      INCLUDE    '($RABDEF)'
      integer ibytes,ibuff
      RECORD     /RAB64DEF/ RABNAM
      RABNAM.RAB64$W_USZ=ibytes                     !Load block size
      RABNAM.RAB64$L_UBF=%LOC(ibuff)                !Load block location
      RABNAM.RAB64$L_BKT=0
      RABNAM.RAB64$L_ROP=RABNAM.RAB64$L_ROP.OR.RAB64$M_ASY !Set for asynchronous
      RETURN
      END
*******************************************************************************
      SUBROUTINE RMSBLK_SPACE(RABNAM,iblks)
C
C Modifies the specified Record Access Block (RAB) to provide the necessary
C information for the SYS$SPACE RMS service.
C Sets the bucket count to the number of blocks to be spaced.
C Clears the asynchronous operation bit.
C
      INCLUDE    '($RABDEF)'
      integer iblks
      RECORD     /RAB64DEF/ RABNAM
      RABNAM.RAB64$L_BKT=iblks
      RABNAM.RAB64$L_ROP=RABNAM.RAB64$L_ROP.AND..NOT.RAB64$M_ASY !Clear async bit
      RETURN
      END
*******************************************************************************
      SUBROUTINE RMSBLK_WAITR(RABNAM,ibytes)
C
C Examines the specified Record Access Block (RAB) to retrieve the number
C of bytes transferred in the last operation.
C
      INCLUDE    '($RABDEF)'
      integer ibytes
      RECORD     /RAB64DEF/ RABNAM
      ibytes=RABNAM.RAB64$W_RSZ
      RETURN
      END
*******************************************************************************
      SUBROUTINE RMSBLK_WAITW(RABNAM,ibytes)
C
C Examines the specified Record Access Block (RAB) to retrieve the number
C of bytes transferred in the last operation.
C
      INCLUDE    '($RABDEF)'
      INCLUDE    '($RMSDEF)'
      integer ibytes
      RECORD     /RAB64DEF/ RABNAM

      IF(RABNAM.RAB64$L_STS.EQ.RMS$_NORMAL) THEN
        ibytes=RABNAM.RAB64$W_RSZ     !Asked for if no error
      ELSE
        ibytes=RABNAM.RAB64$L_STV     !Actual if error
      END IF
      RETURN
      END
*******************************************************************************
      SUBROUTINE RMSBLK_WRITE(RABNAM,ibuff,ibytes)
C
C Modifies the specified Record Access Block (RAB) to provide the necessary
C information for the SYS$WRITE RMS service.
C Loads the byte count.
C Loads the user buffer address.
C Sets the bucket count to 0 so that current block pointer is used.
C Sets the asynchronous operation bit.
C
      INCLUDE    '($RABDEF)'
      integer ibuff,ibytes
      RECORD     /RAB64DEF/ RABNAM
      RABNAM.RAB64$W_RSZ=ibytes                     !Load block size
      RABNAM.RAB64$L_RBF=%LOC(ibuff)                !Load block location
      RABNAM.RAB64$L_BKT=0
      RABNAM.RAB64$L_ROP=RABNAM.RAB64$L_ROP.OR.RAB64$M_ASY !Set for asynchronous
      RETURN
      END
