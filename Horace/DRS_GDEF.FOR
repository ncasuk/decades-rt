!
! DRS_GDEF.FOR - Defines the DRS_COM global common block used for Ethernet 
! input and output.
!
! The 32 word arrays flagged with !! are DLUs ordered by their appearance in
! DRS_ADDRESSES.DAT and counted by IDLUCNT.
!
! The 32 word arrays flagged with !!! are ethernet sources ordered by their 
! arrival on HORACE and counted by ISOURCE.
!
      INTEGER*4 IPKTCNT(32,33),      !!!Running packet count from/to each source
     &          ISOURCE,             !!!No of sources since counters zeroed
     &          ISNDSIZ,               !Size of packet to be sent to sender
     &          IZERCNT,               !Set to 1 to clear counters
     &          IREADER,               !No of read errors
     &          ICNTERR,               !No of controller errors
     &          ITIMOUT,               !No of timeouts
     &          IBUFERR,               !No of system buffer errors
     &          INULLIN,               !No of empty messages
     &          IGETPKT,               !Packet type to be got
     &          IGETPRT,               !Packet part to be got
     &          IGETCNT,               !No of packets got for passing
     &          IGETSIZ,               !Size in words of received packet
     &          ITIMPKT,               !No of time packets since counters zeroed
     &          ICHGFNO,               !1 to change flight number, else 0
     &          ICHGDAT,               !1 to change recording mode, else 0
     &          IDLUCNT,              !!No of DLUs being used
     &          IISSSIZ,               !No of words in ISS
     &          IISSNUM,               !ISS number
     &          IPARCNT,               !No of parameters in ISS
     &          ICNFSIZ,               !No of configured words
     &          ICNFCNT,               !No of configured parameters
     &          INXTEVM,               !1 to increment event mark, else 0
     &          IBLKCNT,               !Block no of next DRS block to be output
     &          IDATHLD,               !1 if DRS data is on hold, else 0
     &          IEVENT,                !Event mark
     &          ISECOFY,               !Second of year of last packet
     &          ICURTIM,               !Time of current DRS block
     &          ITSTDRS,               !1 to get response from DRS program
     &          IDLUSTS(33),          !!.GT.0 if DLU sending packets, else .LE.0
     &          IDLUSEQ(32),          !!1-6 depending on state of init sequence
     &          IDLUSRT(32),          !!No of times each DLU started/restarted
     &          IDLUSLP(32),          !!Latest DLU slip, 32767 if invalid
     &          IMESERR(32),          !!No of times wrong no of mesgs from a DLU
     &          ITIMSTS(32,5)        !!!No of pkts with same, bef or aft prev
      INTEGER*2 JGETPKT(750),          !Packet received from sender
     &          JSNDPKT(750),          !Packet to be sent to sender
     &          JDATBLK(2048),         !Latest DRS data block
     &          JDRSBLK(2048),         !Current DRS block
     &          JISSBLK(2048),         !DRS ISS (Instrument Sampling Sequence)
     &          JTIMPKT(13)            !Latest time packet received
      CHARACTER CSRCLST(32)*6,       !!!Ethernet address of each sender
     &          CGETPKT*6,             !Ethernet address to get packet
     &          CSNDPKT*6,             !Ethernet address to send packet
     &          CTIMPKT*6,             !Ethernet address of time packet
     &          CFLTNUM*4,             !Current flight number
     &          CNEWFNO*4,             !New flight number
     &          CDLUADD(32)*6,        !!6 byte Ethernet addr of each DLU
     &          CDLUNAM(32)*6         !!Name of each DLU
      COMMON    /DRS_GCOM/ 
     &          IPKTCNT,ISOURCE,ISNDSIZ,IZERCNT,IREADER,ICNTERR,
     &          ITIMOUT,IBUFERR,INULLIN,IGETPKT,IGETCNT,ITIMPKT,
     &          ICHGFNO,IDLUSRT,IDLUCNT,IISSSIZ,IBLKCNT,IMESERR,
     &          IISSNUM,IPARCNT,INXTEVM,IDATHLD,ITSTDRS,IDLUSTS,
     &          ICHGDAT,ITIMSTS,IGETSIZ,ICNFSIZ,ICNFCNT,IDLUSEQ,
     &          IEVENT ,IGETPRT,IDLUSLP,ICURTIM,ISECOFY,
     &          JGETPKT,JSNDPKT,JTIMPKT,JDATBLK,JISSBLK,JDRSBLK,
     &          CSRCLST,CGETPKT,CSNDPKT,CTIMPKT,CFLTNUM,CNEWFNO,CDLUNAM,
     &          CDLUADD
      VOLATILE  /DRS_GCOM/
