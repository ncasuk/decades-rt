      PROGRAM H_COMMON
C
C Declares the global common blocks used by the various HORACE programs, and
C stored as a common shareable image in SYS$LIBRARY.
C
C Use this program as follows:
C
C    $ FORT H_COMMON
C    $ LINK/SHARE H_COMMON,H_COMMONB/OPT
C    $ RENAME H_COMMON.EXE SYS$LIBRARY:H_COMMON.EXE
C    $ SET PROTECTION=W:RWE SYS$LIBRARY:H_COMMON.EXE
C    $ INSTALL
C      SYS$LIBRARY:H_COMMON.EXE/WRITEABLE/SHARED
C
C Thereafter make sure that the installation is done as part of the 
C system startup procedure.
C
C Note that the GBLPAGES parameter may need setting by SYSGEN to increase the
C available global pages by at least the space taken by this module (ie
C the total size of all common blocks declared below.  Do this by editing
C SYS$SYSTEM:MODPARAMS.DAT to use the ADD_GBLPAGES keyword with a size at
C least that of H_COMMON.EXE.  Then AUTOGEN the system.
C
C Programs using H_COMMON should declare the same common blocks as below, and 
C then link as follows:
C
C    $ LINK progname,H_COMMON/OPT
C
C    where H_COMMON.OPT contains the line SYS$LIBRARY:H_COMMON/SHAREABLE
C
C Note that all programs using common blocks in H_COMMON must be linked with
C the latest version of H_COMMON in SYS$LIBRARY.  If H_COMMON is relinked then
C all programs using its common blocks must also be relinked with the new 
C version.
C
C V2.00  02/02/02  W.D.N.JACKSON
C
      IMPLICIT  NONE
      INTEGER*2 NFDATA(2048,2),NPLOC(512),NPFREQ(512),STATUS(256),
     -          SPECIAL(12800,10),IGPSMESS(256),INUDATA(512)
      REAL*4    DERIVE(12800,196)
      CHARACTER CNDU(4)*80
      COMMON    /HCOM/ NFDATA,NPLOC,NPFREQ,STATUS,DERIVE,SPECIAL,
     -          IGPSMESS,INUDATA,CNDU
C
!      REAL*4    SAFPAR(12800,100)
!      COMMON    /SAFCOM/ SAFPAR
C
!      REAL*4    MARSSPAR(12800,60)
!      COMMON    /MARSSCOM/ MARSSPAR
C
      REAL*4    CCNPAR(128,112)
      COMMON    /CCNCOM/ CCNPAR
C
!      REAL*4    DEIMOSPAR(12800,115)
!      COMMON    /DEIMOSCOM/ DEIMOSPAR
C
      REAL*8 GINDVAL
      REAL*4 GINFVAL
      COMMON /GINCOM/GINDVAL(8),GINFVAL(11)
      END
