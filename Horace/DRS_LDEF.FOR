!
! DRS_LDEF.FOR - Defines the DRS_LCOM local common block used by the DRS
! program.
!
! The 32 word arrays flagged with !! are DLUs ordered by their appearance in
! DRS_ADDRESSES.DAT and counted by IDLUCNT.
!
      INTEGER*8 ICHGTIM                !Time of next interrupt
      INTEGER*4 ICHAN,                 !Channel to Ethernet port
     &          ICHAN1,                !Used by second mailbox in mailbox mode
     &          IPSIZ,                 !Max Ethernet packet siz
     &          IBLKTIM,               !Validity time of currently filling block
     &          IEVM(2),               !Temporary event marks within second
     &          ISSBLKS,               !No of ISS blocks sent out
     &          ILSTPK4(32),          !!Status of last pkt 4, 1=ok, 2=timenoinit
     &          IINILEN(32),          !!Init string length for each DLU
     &          IRSPORT(32),          !!RS232 port init word for each DLU
     &          IRSLOOK(8,32),        !!RS232 data position in DRS block
     &          IRSFREQ(8,32),        !!RS232 data frequency for each port
     &          IWATCHD(33),          !!Watchdog status for each DLU, gt0=ok
     &          IMCNT(32),            !!Actual packets in a sec from a DLU
     &          IXMCNT(32),           !!Expected packets per sec from a DLU
     &          IMTOT,                 !Total packets so far in this sec
     &          IXMTOT,                !Expected total packets per sec
     &          IPRTLEN(6,32),       !!Data in each type 4 packet
     &          IPARTS(32),           !!No of type 4 packet parts for a DLU
     &          IREQSTS(32),          !!1 if status packet 6 has been solicited
     &          ILASTT(32),           !!Time of latest packet 4/5, -1 if unknown
     &          IIGNSTS(32),          !!Ignore status packets if >0 and <10
     &          IDIAG                  !>0 if want diagnostics
      LOGICAL*4 LMBX,                  !True if getting 'packets' from mailbox
     &          LAMTGSYNC              !True if AMTG pulse define DRS blocks
      INTEGER*2 JOSB(4),               !Status block for Ethernet I/O
     &          JEMPBLK(2048),         !Empty shell DRS block
     &          JDRSTAT(600),          !Parmeter 5, sample 16 mux data
     &          JTSLIPS(32),          !!Time slip for each packet
     &          JINIPKT(131,32),      !!Init string for each DLU
     &          JLOOKUP(742,6,32),   !!Data position in DRS block of each word
     &          JAMTSLP                !Latest AMTG time slip
      REAL*4    RBLKTIM                !Time latest block started
      CHARACTER CTIMTYP*1              !A or H to indicate current time source
      COMMON    /DRS_LCOM/ ICHGTIM,ICHAN,IPSIZ,IINILEN,ILSTPK4,IRSPORT,
     &          IRSLOOK,IRSFREQ,IWATCHD,IREQSTS,ICHAN1,
     &          IMCNT,IXMCNT,ISSBLKS,IPRTLEN,IPARTS,IEVM,IDIAG,LMBX,
     &          IIGNSTS,ILASTT,LAMTGSYNC,IBLKTIM,IXMTOT,IMTOT,
     &          RBLKTIM,
     &          JOSB,JEMPBLK,JINIPKT,JLOOKUP,JTSLIPS,JDRSTAT,
     &          JAMTSLP,CTIMTYP
      VOLATILE  /DRS_LCOM/
