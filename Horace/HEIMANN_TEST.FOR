!
! temp program to tell heim_calcs if heimann calibrating, in absence of signal
! register.
!
! fort heimann_test, link heimann_test,h_common/opt
!
      integer*4 i
      include 'hcom_def.for'
      status(256)=0
10    type *,'enter 1 when enter calibrate, 0 when leave'
      accept *,i
      if(i.eq.1) status(256)=1
      if(i.ne.1) status(256)=0
      goto 10
      end
