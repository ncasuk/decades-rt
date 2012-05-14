      PROGRAM FLASH1
C
C Sends the character A to the satcom flasher indicator via the RS232 port
C with the logical name FLASH_CHAN, whenever the H_SATCOM program sets bit 0
C in STATUS(46).
C
C Build with:
C 
C  $ FORT H_FLASH
C  $ LINK H_FLASH,H_COMMON/OPT
C
C    where H_COMMON.OPT contains the line SYS$LIBRARY:H_COMMON/SHAREABLE
C
C V1.00  02/02/02  W.D.N.JACKSON
C
      IMPLICIT NONE
      INTEGER*4 IOS
      INCLUDE 'HCOM_DEF.FOR'

      OPEN(UNIT=10,FILE='FLASH_CHAN',STATUS='OLD',IOSTAT=IOS)
          WRITE(10,'(A)',IOSTAT=IOS) CHAR(0)//'A'
      END   
