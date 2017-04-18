      subroutine allocate_KSP(dy_muc)
      
	use muc_mod

!     Added by Xuesong and Jason July 1 2003
	integer dy_muc

      integer error,ksptime

!	print *, 'before allocate_KSP iteration =  ',iteration
!	pause
      
!Modified by Xuesong and Hayssam Jan 22 2004 0.930.9
!	if(total_hov.gt.0.01) then
	if(Veh_Type(3).eq.1) then
		
! Modified by Xuesong and Sevgi Feb 3 2004 0.930.9
!		if(link_hov.gt.0) then
		if(link_hov.gt.0.or.link_hot.gt.0) then
!			no_link_type = 2 ! if we have HOV links
!     Modified by Jing and Kuilin Mar. 2 2006
			no_link_type = 1 ! if we have HOV links
          else
			no_link_type = 1 ! default
          endif


       no_occupancy_level = 2 ! if we have HOV vehicles
	endif

!	if(dy_muc.eq.0) then !called from dynasmart
!	 iti_nu = 1
!	 ksptime = iti_nu
!	else
!	 iti_nu = nint((stagelength/tii)/ftr)
!	 ksptime = iti_nu	 !called from muc
!	endif

!     Modified by Xuesong and Jason July 1 2003

! Modified by MTI team jan 24 2004
!	if(dy_muc.eq.0) then !called from dynasmart
	if(dy_muc.eq.0) then !called from dynasmart
		 
	 iti_nu = 1
	 ksptime = iti_nu
	 noof_master_destinations = noof_master_destinations_original

       
!Added May 26 2005
	if(iSequentialLoad.eq.1) then
	 iti_nu = 1 
	 ksptime = iti_nu	 !called from muc
	 noof_master_destinations = 1
	 real_SuperzoneIndex = 1
	endif
!End

	else

!!      Modified by MTI team Jan 21 
!	 iti_nu = nint((stagelength/tii)/ftr)
	 iti_nu = nint((stagelength/tii)/ftr) +1 

	 ksptime = iti_nu	 !called from muc
	 noof_master_destinations = 1

	endif
!    End of change


	 !Added by Zihan for AMS, predictive info 20160616 

	 
	  if (i_predictiveinfo.eq.1.AND.dy_muc.eq.0) then
	    iti_nu = horizon 
	    ksptime = iti_nu
!	          
!          allocate(TravelTime_pred(noofarcs,ksptime),stat=error)
!	    if(error.ne.0) then
!	    write(911,*) "allocate TravelTime_pred error - insufficient memory"
!	    stop
!	    endif
!	    TravelTime_pred(:,:) = 0
!          
!       allocate(TravelPenalty_pred(noofarcs,ksptime,MaxMove),stat=error)
!	   if(error.ne.0) then
!	 write(911,*) "allocate TravelPenalty_pred error - insufficient memory"
!	   stop
!	   endif
!	    TravelPenalty_pred(:,:,:) = 99999  
	endif  
      ! End Addition






c     in dynasmart, only need to define 1 time interval for TTime and TTpenalty
c      common/TTime/TTime(noofarcs,Iti_nu)
c      common/TTpenalty/TTpenalty(noofarcs,Iti_nu,nu_mv)
      
	allocate(ttmarginal(ksptime,noofarcs,MaxMove),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate ttmarginal error - insufficient memory"
	  stop
	endif
	ttmarginal(:,:,:) = 0

	allocate(TTime(noofarcs,ksptime),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate TTime error - insufficient memory"
	  stop
	endif
	TTime(:,:) = 0
	
      allocate(TTpenalty(noofarcs,ksptime,MaxMove),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate TTpenalty error - insufficient memory"
	  stop
	endif
	TTpenalty(:,:,:) = 99999

	
	
	

c	real LabelOut(no_link_type,no_occupancy_level,
c     * nu_des,no_nu,Iti_nu,kay,nu_mv)

	
      allocate(LabelOut(no_link_type,no_occupancy_level,
     * noof_master_destinations,noofnodes,ksptime,kay,MaxMove),
     *stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate LabelOut error - insufficient memory"
	  stop
	endif
	LabelOut(:,:,:,:,:,:,:) = 0


	allocate(LabelOutCost(no_link_type,no_occupancy_level,
     * noof_master_destinations,noofnodes,ksptime,kay,MaxMove),
     *stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate LabelOutCost error - insufficient memory"
	  stop
	endif
	LabelOutCost(:,:,:,:,:,:,:) = 0
	
c      dimension labelforods(no_link_type,no_occupancy_level,      
c     *                  nu_des,nu_mv,2)

    	allocate(labelforods(no_link_type,no_occupancy_level,      
     *               noof_master_destinations,MaxMove,2),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate labelforods error - insufficient memory"
	  stop
	endif
	labelforods(:,:,:,:,:) = 0

c	Integer PathPointerOut1(no_link_type,no_occupancy_level,
c     *nu_des,no_nu,Iti_nu,kay,nu_mv)
      

	allocate(PathPointerOut1(no_link_type,no_occupancy_level,
     *noof_master_destinations,noofnodes,ksptime,kay,MaxMove),
     *stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate PathPointerOut1 error - 
     +  insufficient memory"
	  stop
	endif
	PathPointerOut1(:,:,:,:,:,:,:) = 0

c	Integer PathPointerOut2(no_link_type,no_occupancy_level,
c     *nu_des,no_nu,Iti_nu,kay,nu_mv)

      allocate(PathPointerOut2(no_link_type,no_occupancy_level,
     *noof_master_destinations,noofnodes,ksptime,kay,MaxMove),
     *stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate PathPointerOut2 error - 
     +  insufficient memory"
	  stop
	endif
	PathPointerOut2(:,:,:,:,:,:,:) = 0

c      Integer PathPointerOut3(no_link_type,no_occupancy_level,
c     *nu_des,no_nu,Iti_nu,kay,nu_mv)


      allocate(PathPointerOut3(no_link_type,no_occupancy_level,
     *noof_master_destinations,noofnodes,ksptime,kay,MaxMove),
     *stat=error)
	if(error.ne.0) then 
	  write(911,*) "allocate PathPointerOut3 error - 
     +  insufficient memory"
	  stop
	endif
	PathPointerOut3(:,:,:,:,:,:,:) = 0

c      Integer PathPointerOut4(no_link_type,no_occupancy_level,
c     *nu_des,no_nu,Iti_nu,kay,nu_mv)


      allocate(PathPointerOut4(no_link_type,no_occupancy_level,
     *noof_master_destinations,noofnodes,ksptime,kay,MaxMove),
     *stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate PathPointerOut4 error - 
     +  insufficient memory"
	  stop
	endif
	PathPointerOut4(:,:,:,:,:,:,:) = 0

c      Integer LabelPointerOut(no_link_type,no_occupancy_level,
c     *nu_des,no_nu,Iti_nu,kay,nu_mv)


      allocate(LabelPointerOut(no_link_type,no_occupancy_level,
     *noof_master_destinations,noofnodes,ksptime,kay,MaxMove),
     *stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate LabelPointerOut error - 
     +  insufficient memory"
	  stop
	endif
	LabelPointerOut(:,:,:,:,:,:,:) = 0

!Added Sep 12 2005
      if(iSequentialLoad.eq.0) then

c	integer totalpriority(no_link_type,no_occupancy_level,no_nu)
      allocate(totalpriority(no_link_type,no_occupancy_level,
     +noofnodes)
     +,stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate totalpriority error - 
     +  insufficient memory"
	  stop
	endif
	totalpriority(:,:,:) = 0

c     Integer Priority(no_link_type,no_occupancy_level,
c     * nu_des,no_nu,Iti_nu,kay,nu_mv)
      allocate(Priority(no_link_type,no_occupancy_level,
     *noof_master_destinations,noofnodes,ksptime,kay,MaxMove),
     *stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate priority error - insufficient memory"
	  stop
	endif
	Priority(:,:,:,:,:,:,:) = 0

c  -- calculate kno_nu
      kno_nu = noofnodes*MaxMove*kay*iti_nu

c      integer pp(no_link_type,no_occupancy_level,
c     * nu_des,kno_nu,4),track(kno_nu,4)

      allocate(pp(no_link_type,no_occupancy_level,
     * noof_master_destinations,kno_nu,4),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate pp error - insufficient memory"
	  stop
	endif
	pp(:,:,:,:,:) = 0

      endif ! End if(iSequentialLoad.eq.0) then 

c      Real Label(no_nu,Iti_nu,kay,nu_mv)
      allocate(Label(noofnodes,ksptime,kay,MaxMove),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate Label error - insufficient memory"
	  stop
	endif
	Label(:,:,:,:) = 0

	allocate(LabelCost(noofnodes,
     +ksptime,kay,MaxMove),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate LabelCost error - insufficient memory"
	  stop
	endif
	LabelCost(:,:,:,:) = 0

c      Integer FirstLabel(no_nu,Iti_nu,nu_mv)
	allocate(FirstLabel(noofnodes,ksptime,MaxMove),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate FirstLabel error - insufficient memory"
	  stop
	endif
	FirstLabel(:,:,:) = 0

c      Integer LabelPointer(no_nu,Iti_nu,kay,nu_mv)
      allocate(LabelPointer(noofnodes,
     +ksptime,kay,MaxMove),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate LabelPointer error - insufficient memory"
	  stop
	endif
	LabelPointer(:,:,:,:) = 0

c      Integer FirstGoodLabel(no_nu,Iti_nu,nu_mv)
      allocate(FirstGoodLabel(noofnodes,ksptime,MaxMove),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate FirstGoodLabel error - 
     +  insufficient memory"
	  stop
	endif
	FirstGoodLabel(:,:,:) = 0

c      integer PathPointer(no_nu,Iti_nu,kay,4,nu_mv)
      allocate(PathPointer(noofnodes,
     +ksptime,kay,4,MaxMove),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate PathPointer error - insufficient memory"
	  stop
	endif
	PathPointer(:,:,:,:,:) = 0
c	iyy=nu_mv

c      Real DequeLabel1(no_nu,Iti_nu,kay,nu_mv)
      allocate(DequeLabel1(noofnodes,ksptime,kay,MaxMove),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate DequeLabel1 error - insufficient memory"
	  stop
	endif
	DequeLabel1(:,:,:,:) = 0

c      integer DequeLabel2(no_nu,Iti_nu,kay,nu_mv)
	allocate(DequeLabel2(noofnodes,ksptime,kay,MaxMove),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate DequeLabel2 error - insufficient memory"
	  stop
	endif
	DequeLabel2(:,:,:,:) = 0 

      allocate(DequeLabel1Cost(noofnodes,ksptime,kay,MaxMove),
     *stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate DequeLabel1Cost error - 
     +  insufficient memory"
	  stop
	endif
      DequeLabel1Cost(:,:,:,:) = 0

c     integer StatusInDeque(no_nu)
      allocate(StatusInDeque(noofnodes),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate StatusInDeque error - 
     +  insufficient memory"
	  stop
	endif
	StatusInDeque(:) = 0

c --   Integer DequeLabelCounter(no_nu,Iti_nu,nu_mv)
      allocate(DequeLabelCounter(noofnodes,ksptime,MaxMove),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate DequeLabelCounter error - 
     +  insufficient memory"
	  stop
	endif
	DequeLabelCounter(:,:,:) = 0

c      Common/UpCounter/UpCounter(Iti_nu)
      allocate(UpCounter(ksptime),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate UpCounter error - insufficient memory"
	  stop
    	endif
	UpCounter(:) = 0

c      Logical Update(nu_mv,iti_nu,kay+1)
      allocate(Update(MaxMove,ksptime,kay+1),stat=error)
c      Update(:,:,:) = .false. 
	if(error.ne.0) then
	  write(911,*) "allocate Update error - insufficient memory"
	  stop
	endif

c     integer track(kno_nu,4)
      allocate(track(kno_nu,4),stat=error)
      if(error.ne.0) then
        write(911,*) "allocate track error - insufficient memory"
        stop
      endif
	track(:,:) = 0

c     cost for HOV, HOT
c     common/hot5/cost(noofarcs,no_link_type,no_occupancy_level)

!Added May 23 2005 TD link toll
!      allocate(cost(noofarcs,no_link_type,no_occupancy_level),
!     +stat=error)
	allocate(cost(noofarcs,no_link_type,no_occupancy_level,ksptime),
     +stat=error)
      if(error.ne.0) then
	  write(911,*) "allocate cost error - insufficient memory"
	  stop
      endif
	!cost(:,:,:,:) = 0
	cost(:,:,:,:) = 0.0
! End

!Added for weather VMS, June 8 2009
      if(i_risk.eq.1)then
        allocate(risk(noofarcs,no_link_type,no_occupancy_level,ksptime),
     +   stat=error)
        if(error.ne.0) then
	    write(911,*) "allocate risk error - insufficient memory"
	    stop
        endif
	  risk(:,:,:,:) = 0.0
	endif
! End

!	print *, 'after allocate_KSP iteration =  ',iteration
!	pause

            return
      end
