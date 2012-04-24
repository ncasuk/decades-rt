      PROGRAM H_SATCOM_PURGE
!
! Cleans up the input and output status files, removing entries for files that
! no longer exist and adjusting the pointers accordingly.  Note that the first
! record of each file contains the number of status records in the file 
! (excluding the first record of the file), and there must be a continuous
! uninterrupted sequence of record numbers between the first and the last.
! The procedure that calls this program will have deleted/purged all files 
! more than the required age, so this program simply checks and removes all
! records from the status files until a record is found for a file that still
! exists.  This record and all subsequent records are then copied to a new
! file - SAT_ccc_STS.NEW.  May return with no records, except the first which
! will then have a value of 00000.  The program is fairly cautious and if
! anything goes wrong then it exits without creating a new file.  When 
! processing the SAT_OUT_STS file this program sets the next queued entry to
! 00000, since the program is only run before H_SATCOM is run, and H_SATCOM
! always starts by stopping all queued entries.
!
! V1.00  03/11/95  W.D.N.JACKSON
! V1.01  11/03/96  W.D.N.JACKSON
!        Now creates new files if status files are missing.
!
      IMPLICIT NONE
      INTEGER*4 IOS,ICOUNT,IFIRST,ILAST,IFIRSTGD,IOUTCNT,JOS,I
      LOGICAL*4 LEXISTS
      CHARACTER CLINE*80
!
! First clean up the input status file
!
      OPEN(10,FILE='[.SATCOM]SAT_INP_STS.TMP;',READONLY,STATUS='OLD',
     &    ACCESS='DIRECT',RECL=20,IOSTAT=IOS) !Open input file
      IF(IOS.EQ.29) THEN               !File not found then try to create one
        OPEN(10,FILE='[.SATCOM]SAT_INP_STS.DAT;',STATUS='NEW',
     &    ACCESS='DIRECT',RECL=20,IOSTAT=IOS) !Open input file
        CLINE='00000'
        WRITE(10,REC=1,IOSTAT=IOS) CLINE
        IF(IOS.EQ.0) THEN
          TYPE *,'New SAT_INP_STS.DAT file created'
        ELSE
          TYPE *,'Failed to create a new SAT_INP_STS.DAT file'
        END IF
        CLOSE(10,IOSTAT=IOS)
        GOTO 100
      END IF
      IF(IOS.NE.0) GOTO 100
      READ(10,REC=1,IOSTAT=IOS) CLINE  !Read first input record
      IF(IOS.NE.0) GOTO 100
      READ(CLINE(1:5),'(I5)',IOSTAT=IOS) ICOUNT !Get no of records in file
      IF(IOS.NE.0.OR.ICOUNT.EQ.0) GOTO 100
      READ(10,REC=2,IOSTAT=IOS) CLINE  !Get no of first input file
      IF(IOS.NE.0) GOTO 100
      READ(CLINE(1:5),'(I5)',IOSTAT=IOS) IFIRST
      IF(IOS.NE.0) GOTO 100
      ILAST=IFIRST+ICOUNT-1            !Work out last file
      CLINE='[.SATCOM]SAT_INP_nnnnn.DAT' !Template for file names
      IFIRSTGD=-1                      !Don't yet know first existing file
      DO I=IFIRST,ILAST                !Check each file and note first existing
        WRITE(CLINE(18:22),'(I5.5)',IOSTAT=IOS) I !Form file name
        IF(IOS.NE.0) GOTO 100
        INQUIRE(FILE=CLINE,EXIST=LEXISTS,IOSTAT=IOS) !See if it exists
        IF(IOS.NE.0) GOTO 100
        IF(LEXISTS.AND.IFIRSTGD.EQ.-1) IFIRSTGD=I !Note it if first
      END DO
      IF(IFIRSTGD.EQ.IFIRST) GOTO 100
      OPEN(11,FILE='[.SATCOM]SAT_INP_STS.NEW;',STATUS='NEW',
     &    ACCESS='DIRECT',RECL=20,IOSTAT=IOS) !Open output file
      IF(IOS.NE.0) GOTO 100
      IOUTCNT=0                        !No of output records so far
      IF(IFIRSTGD.NE.-1) THEN
        DO I=IFIRSTGD,ILAST            !For each file we want
          READ(10,REC=2+I-IFIRST,IOSTAT=IOS) CLINE !Read in its status record
          IOUTCNT=IOUTCNT+1            !Bump output count
          WRITE(11,REC=IOUTCNT+1,IOSTAT=JOS) CLINE !Write to new file
          IF(IOS.NE.0.OR.JOS.NE.0) THEN
            CLOSE(10,IOSTAT=IOS)
            CLOSE(11,IOSTAT=IOS,DISPOSE='DELETE')
            GOTO 100
          END IF
        END DO
      END IF
      WRITE(CLINE(1:),'(I5.5)',IOSTAT=IOS) IOUTCNT !Write out count
      WRITE(11,REC=1,IOSTAT=JOS) CLINE
      IF(IOS.NE.0.OR.JOS.NE.0) CLOSE(11,IOSTAT=IOS,DISPOSE='DELETE')
100   CLOSE(10,IOSTAT=IOS)             !Close input file if open
      CLOSE(11,IOSTAT=IOS)             !Close output file if open
!
! Now clean up output status file
!
      OPEN(10,FILE='[.SATCOM]SAT_OUT_STS.TMP;',READONLY,STATUS='OLD',
     &    ACCESS='DIRECT',RECL=20,IOSTAT=IOS) !Open input file
      IF(IOS.EQ.29) THEN               !File not found then try to create one
        OPEN(10,FILE='[.SATCOM]SAT_OUT_STS.DAT;',STATUS='NEW',
     &    ACCESS='DIRECT',RECL=20,IOSTAT=IOS) !Open input file
        CLINE='00000 00000'
        WRITE(10,REC=1,IOSTAT=IOS) CLINE
        IF(IOS.EQ.0) THEN
          TYPE *,'New SAT_OUT_STS.DAT file created'
        ELSE
          TYPE *,'Failed to create a new SAT_OUT_STS.DAT file'
        END IF
        CLOSE(10,IOSTAT=IOS)
        GOTO 200
      END IF
      IF(IOS.NE.0) GOTO 200
      READ(10,REC=1,IOSTAT=IOS) CLINE  !Read first input record
      IF(IOS.NE.0) GOTO 200
      READ(CLINE(1:5),'(I5)',IOSTAT=IOS) ICOUNT !Get no of records in file
      IF(IOS.NE.0.OR.ICOUNT.EQ.0) GOTO 200
      READ(10,REC=2,IOSTAT=IOS) CLINE  !Get no of first input file
      IF(IOS.NE.0) GOTO 200
      READ(CLINE(1:5),'(I5)',IOSTAT=IOS) IFIRST
      IF(IOS.NE.0) GOTO 200
      ILAST=IFIRST+ICOUNT-1            !Work out last file
      CLINE='[.SATCOM]SAT_OUT_nnnnn.DAT' !Template for file names
      IFIRSTGD=-1                      !Don't yet know first existing file
      DO I=IFIRST,ILAST                !Check each file and note first existing
        WRITE(CLINE(18:22),'(I5.5)',IOSTAT=IOS) I !Form file name
        IF(IOS.NE.0) GOTO 200
        INQUIRE(FILE=CLINE,EXIST=LEXISTS,IOSTAT=IOS) !See if it exists
        IF(IOS.NE.0) GOTO 200
        IF(LEXISTS.AND.IFIRSTGD.EQ.-1) IFIRSTGD=I !Note it if first
      END DO
      IF(IFIRSTGD.EQ.IFIRST) GOTO 200
      OPEN(11,FILE='[.SATCOM]SAT_OUT_STS.NEW;',STATUS='NEW',
     &    ACCESS='DIRECT',RECL=20,IOSTAT=IOS) !Open output file
      IF(IOS.NE.0) GOTO 200
      IOUTCNT=0                        !No of output records so far
      IF(IFIRSTGD.NE.-1) THEN
        DO I=IFIRSTGD,ILAST            !For each file we want
          READ(10,REC=2+I-IFIRST,IOSTAT=IOS) CLINE !Read in its status record
          IOUTCNT=IOUTCNT+1            !Bump output count
          WRITE(11,REC=IOUTCNT+1,IOSTAT=JOS) CLINE !Write to new file
          IF(IOS.NE.0.OR.JOS.NE.0) THEN
            CLOSE(10,IOSTAT=IOS)
            CLOSE(11,IOSTAT=IOS,DISPOSE='DELETE')
            GOTO 200
          END IF
        END DO
      END IF
      WRITE(CLINE(1:),'(I5.5)',IOSTAT=IOS) IOUTCNT !Write out count
      CLINE(7:11)='00000'              !Set queued indicator to 0
      WRITE(11,REC=1,IOSTAT=JOS) CLINE
      IF(IOS.NE.0.OR.JOS.NE.0) CLOSE(11,IOSTAT=IOS,DISPOSE='DELETE')
200   CLOSE(10,IOSTAT=IOS)             !Close input file if open
      CLOSE(11,IOSTAT=IOS)             !Close output file if open
      END
