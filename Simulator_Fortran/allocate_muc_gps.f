      subroutine allocate_muc_gps()

	use vector_mod
	use muc_mod
 	integer error
	
!  Modification by Xuesong and Jason July 1 2003

! replace noof_master_destinations by noof_master_destinations_original

!      type (mucpolicy) temp
!	type (MUCMEMBER) temp1
!      type (PathAtt)   temp2
!      print *, 'size of mucpolicy', sizeof(temp)
!	print *, 'size of MUCMEMBER', sizeof(temp1)
!      print *, 'size of PathAtt', sizeof(temp2)

c  -- determine the size for TravelTime and TravelPenalty
c  -- by stagelength (in terms of min)
      
	allocate(TravelTime(noofarcs,aggint),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate TravelTime error - insufficient memory"
	  stop
	endif
	TravelTime(:,:) = 0 

	allocate(TravelPenalty(noofarcs,aggint,nu_mv),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate TravelPenalty error - 
     +  insufficient memory"
	  stop
	endif
	TravelPenalty(:,:,:) = 0
	   
	allocate(TravelLET(noofarcs,aggint),stat=error)
	TravelLET(:,:) = 0
 	if(error.ne.0) then
	  write(911,*) "allocate TravelLET error - insufficient memory"
	  stop
	endif

c --  number of vehicles making left or other movement, somarginal
	allocate(moveturnMG(noofarcs,aggint,2),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate moveturnMG error - insufficient memory"
	  stop
	endif
	moveturnMG(:,:,:) = 0

c --  average forward penalty (openalty) for each link
	allocate(openaltyMG(noofarcs,aggint,nu_mv),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate openaltyMG error - insufficient memory"
	  stop
	endif
	openaltyMG(:,:,:)= 0

c --  Ave # of Veh ready to move into link i
	allocate(DiffMG(noofarcs,aggint),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate DiffMG error - insufficient memory"
	  stop
	endif
	DiffMG(:,:)= 0
      
	allocate(penaltyMG(aggint,noofarcs,nu_mv),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate penaltyMG error - insufficient memory"
	  stop
	endif
	penaltyMG(:,:,:) = 0

	allocate(PenaltyEntry(noofarcs,aggint),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate PenaltyEntry error - insufficient memory"
	  stop
	endif
	PenaltyEntry(:,:) = 0

	allocate(PenaltyEntryMG(noofarcs,aggint),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate PenaltyEntryMG error - 
     +  insufficient memory"
	  stop
	endif
	PenaltyEntryMG(:,:) = 0

! Added by DTA team Nov 18 2004
	do i=1, noofarcs
        ip=ForToBackLink(i)  ! backward link index
        do iagg=1, aggint
	    ! free flow travel time, forward *
	    TravelTime(i, iagg) = s(i)/((SpeedLimit(i)+Vfadjust(i))/60.0)
		do j=1,nu_mv	      
      if(SignalPreventFor(i,j).ne.0.or.GeoPreventFor(i,j).ne.0)
     +	  then	      
              openaltyMG(i,iagg,j) = PenForPreventMove !forward* 
	      endif
      if(SignalPreventBack(ip,j).ne.0.or.GeoPreventBack(ip,j).ne.0)
     +	  then	      
              TravelPenalty(ip,iagg,j) = PenForPreventMove !backward* 
	      endif	      
	    enddo
        enddo
	enddo
! End

c ------- UE	 ------------------------------------------
      if(iue_ok.eq.1.and.itedex.gt.0) then

! Modified by MTI team Jan 17 2004 0.930.7D
! change the first dim from noofnodes to nzones (centroid)      
	allocate(uepath_lov
     *(nzones,noof_master_destinations_original,soint,itedex+1),
     + stat=error)
      if(error.ne.0) then
	  write(911,*) "uepath_lov error - insufficient memory"
	  stop
	endif
	uepath_lov(:,:,:,:) = 0

	allocate(uepolicy_lov(nzones,noof_master_destinations_original,
     +soint,itedex+1),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate uepolicy_lov error - insufficient memory"
	  stop
	endif
	uepolicy_lov(:,:,:,:)%nodesum = 0
	uepolicy_lov(:,:,:,:)%prob = 0.0
	uepolicy_lov(:,:,:,:)%NumOfVehicle = 0.0

      

      allocate(uenxz_lov
     *(nzones,noof_master_destinations_original,soint),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate uenxz_lov error - insufficient memory"
	  stop
	endif
	uenxz_lov(:,:,:) = 0

      allocate(NumUePath_lov(nzones,noof_master_destinations_original
     +,soint),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate NumUePath_lov error - 
     +  insufficient memory"
	  stop
	endif
	NumUePath_lov(:,:,:) = 1

!      print *, 'before ueaccuprob_lov'
!	pause 

      allocate(ueaccuprob_lov
     *(nzones,noof_master_destinations_original,soint,itedex+1)
     *,stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate ueaccuprob_lov error - 
     +  insufficient memory"
	  stop
	endif
      ueaccuprob_lov(:,:,:,:) = 0.0
	
      endif
c  ---------end of UE ------------------------










c -----SO
! Modified by MTI team Jan 17 2004 0.930.7D
! change the first dim from noofnodes to nzones (centroid) 

      if(iso_ok.eq.1) then

! Added by Jason Feb 3 2004 DYNASMART-P 0.930.9
c --  number of vehicles making left or other movement, somarginal
	allocate(moveturnMG_sim(noofarcs,numof_siminterval,2),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate moveturnMG_sim error-insufficient memory"
	  stop
	endif
	moveturnMG_sim(:,:,:) = 0

      allocate(TravelTime_sim(noofarcs,numof_siminterval),stat=error)
	if(error.ne.0) then
	write(911,*) "allocate TravelTime_sim error - insufficient memory"
	stop
	endif
	TravelTime_sim(:,:) = 0

	allocate(openaltyMG_sim
     *(noofarcs,numof_siminterval,nu_mv),stat=error)
	if(error.ne.0) then
	write(911,*) "allocate openaltyMG_sim error - insufficient memory"
	stop
	endif
	openaltyMG_sim(:,:,:)= 0

	allocate(DiffMG_sim(noofarcs,numof_siminterval),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate DiffMG_sim error - insufficient memory"
	  stop
	endif
	DiffMG_sim(:,:)= 0

	allocate(PenaltyEntry_sim(noofarcs,numof_siminterval),stat=error)
	if(error.ne.0) then
	  write(911,*) 
     + "allocate PenaltyEntry_sim error - insufficient memory"
	  stop
	endif
	PenaltyEntry_sim(:,:) = 0
! End

! Added by DTA team Nov 18 2004
      do i=1, noofarcs
        do iagg=1, numof_siminterval
	    ! free flow travel time, forward *
	 TravelTime_sim(i, iagg) = s(i)/((SpeedLimit(i)+Vfadjust(i))/60.0)
		do j=1,nu_mv	      
      if(SignalPreventFor(i,j).ne.0.or.GeoPreventFor(i,j).ne.0)
     +	  then	      
              openaltyMG_sim(i,iagg,j) = PenForPreventMove !forward* 
	      endif
	    enddo
        enddo
	enddo
! End

      allocate(sopath_lov
     *(nzones,noof_master_destinations_original,soint,itedex+1),
     + stat=error)
      if(error.ne.0) then
	  write(911,*) "allocate sopath error - insufficient memory"
	  stop
	endif
	sopath_lov(:,:,:,:) = 0

	allocate(sopolicy_lov(nzones,noof_master_destinations_original,
     +soint,itedex+1),stat=error)		
      if(error.ne.0) then
	  write(911,*) "allocate sopolicy_lov error - insufficient memory"
	  stop
	endif
	sopolicy_lov(:,:,:,:)%nodesum = 0
 	sopolicy_lov(:,:,:,:)%prob = 0.0
	sopolicy_lov(:,:,:,:)%NumOfVehicle = 0.0

      allocate(sonxz_lov
     *(nzones,noof_master_destinations_original,soint),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate sonxz_lov error - insufficient memory"
	  stop
	endif
	sonxz_lov(:,:,:) = 0

      allocate(NumsoPath_lov(nzones,noof_master_destinations_original
     +	,soint),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate NumsoPath_lov error - 
     +  insufficient memory"
	  stop
	endif
	NumsoPath_lov(:,:,:) = 1

      allocate(soaccuprob_lov
     *(nzones,noof_master_destinations_original,soint,itedex+1)
     *,stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate soaccuprob_lov error - 
     +  insufficient memory"
	  stop
	endif
      soaccuprob_lov(:,:,:,:) = 0.0
	
      endif
c ------------end of SO -------------------------------------------

c -----grand path set ---------------------------------------------

! Modified by MTI team Jan 17 2004 0.930.7D
! change the first dim from noofnodes to nzones (centroid) 

      if(iso_ok.eq.1.or.iue_ok.eq.1) then

!	allocate(mucpath_lov
!     *(noofnodes,noof_master_destinations_original,muc_path_total),
!     +stat=error)
!	if(error.ne.0) then
!	  write(911,*) "allocate mucpath error - insufficient memory"
!	  stop
!	endif

!      print *, 'before mucpath_lov_array'
!	pause 

! Added by Xuesong and Jason June 17 2003
	allocate(mucpath_lov_array
     *(nzones,noof_master_destinations_original,muc_path_total_lov),
     +stat=error)
	if(error.ne.0) then
	 write(911,*) "allocate mucpath_array error - insufficient memory"
	  stop
	endif
! End of modification

!      print *, 'before MucPathAtt_lov'
!	pause

      allocate(MucPathAtt_lov
     *(nzones,noof_master_destinations_original,muc_path_total_lov)
     +,stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate MucPathAtt error - insufficient memory"
	  stop
	endif
	MucPathAtt_lov(:,:,:)%node_sum = 0
 	MucPathAtt_lov(:,:,:)%node_number = 0

      allocate(NumMucPath_lov
     * (nzones,noof_master_destinations_original),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate NumMucPath error - insufficient memory"
	  stop
	endif
	NumMucPath_lov(:,:) = 0

	endif
c ---------end of Grand Path Set ------------------------------------


c --  If HOV/HOT vehicles exist then allocate memory for 
c --  relevant arrays
c ---------------------------------------- 

!Modified by Xuesong and Hayssam Jan 22 2004 0.930.9
!	if(total_hov.gt.0.001.and.iue_ok.eq.1) then
	if(Veh_Type(3).gt.0.001.and.iue_ok.eq.1) then
c     ----UE-------------

! Modified by MTI team Jan 17 2004 0.930.7D
! change the first dim from noofnodes to nzones (centroid) 

      allocate(uepath_hov
     *(nzones,noof_master_destinations_original,soint,itedex+1),
     + stat=error)
      if(error.ne.0) then
        write(911,*) "allocate uepath_hov error - insufficient memory"
        stop
      endif
	uepath_hov(:,:,:,:) = 0

      allocate(uepolicy_hov(nzones,noof_master_destinations_original,
     +soint,itedex+1),stat=error)
      if(error.ne.0) then
        write(911,*) "allocate uepolicy_hov error - insufficient memory"
        stop
      endif
	uepolicy_hov(:,:,:,:)%nodesum = 0
      uepolicy_hov(:,:,:,:)%prob = 0.0
	uepolicy_hov(:,:,:,:)%NumOfVehicle = 0.0

      allocate(ueaccuprob_hov
     *(nzones,noof_master_destinations_original,soint,itedex+1)
     *,stat=error)
	if(error.ne.0) then
	  write(911,*) "ueaccuprob_hov error - insufficient memory"
	  stop
	endif
      ueaccuprob_hov(:,:,:,:) = 0.0

      allocate(uenxz_hov
     * (nzones,noof_master_destinations_original,soint),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate uenxz_hov error - insufficient memory"
	  stop
	endif
	uenxz_hov(:,:,:) = 0

      allocate(NumUePath_hov(nzones,noof_master_destinations_original
     +,soint),
     +stat=error)
      if(error.ne.0) then
	  write(911,*) "allocate NumUePath_hov error - 
     +  insufficient memory"
	  stop
	endif
      NumUePath_hov(:,:,:) = 1

	endif
c     ----SO-------------

! Modified by MTI team Jan 17 2004 0.930.7D
! change the first dim from noofnodes to nzones (centroid) 

!      if(total_hov.gt.0.001.and.iso_ok.eq.1) then
      if(Veh_Type(3).eq.1.and.iso_ok.eq.1) then
      
	allocate(sopath_hov
     * (nzones,noof_master_destinations_original,soint,itedex+1),
     + stat=error)
      if(error.ne.0) then
        write(911,*) "allocate sopath error - insufficient memory"
        stop
      endif
	sopath_hov(:,:,:,:) = 0

      allocate(sopolicy_hov(nzones,noof_master_destinations_original,
     +soint,itedex+1),stat=error)
      if(error.ne.0) then
        write(911,*) "allocate sopolicy_hov error - insufficient memory"
        stop
      endif
	sopolicy_hov(:,:,:,:)%nodesum = 0
      sopolicy_hov(:,:,:,:)%prob = 0.0
  	sopolicy_hov(:,:,:,:)%NumOfVehicle = 0.0

      allocate(sonxz_hov
     *	(nzones,noof_master_destinations_original,soint),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate sonxz_hov error - insufficient memory"
	  stop
	endif
	sonxz_hov(:,:,:) = 0

      allocate(NumsoPath_hov(nzones,noof_master_destinations_original
     +	,soint),
     +stat=error)
      if(error.ne.0) then
	  write(911,*) "allocate NumsoPath_hov error - 
     +  insufficient memory"
	  stop
	endif
      NumsoPath_hov(:,:,:) = 1

      allocate(soaccuprob_hov
     *(nzones,noof_master_destinations_original,soint,itedex+1)
     *,stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate soaccuprob_hov error - 
     +  insufficient memory"
	  stop
	endif
      soaccuprob_hov(:,:,:,:) = 0.0

	endif
c     ----Grand Path Set-------------

! Modified by MTI team Jan 17 2004 0.930.7D
! change the first dim from noofnodes to nzones (centroid) 

!      if((iso_ok.eq.1.or.iue_ok.eq.1).and.total_hov.gt.0.001) then
      if((iso_ok.eq.1.or.iue_ok.eq.1).and.Veh_Type(3).eq.1) then
	
!      allocate(Mucpath_hov
!     *	(noofnodes,noof_master_destinations_original,muc_path_total),
!     +stat=error)
!	if(error.ne.0) then
!	  write(911,*) "allocate mucpath error - insufficient memory"
!	  stop
!	endif

!      print *, 'before mucpath_hov_array'
!      pause
 
! Added by Xuesong and Jason June 17 2003
	allocate(mucpath_hov_array
     *(nzones,noof_master_destinations_original,muc_path_total_hov),
     +stat=error)
	if(error.ne.0) then
	 write(911,*) "allocate mucpath_array error - insufficient memory"
	  stop
	endif
! End of modification

!      print *, 'before MucPathAtt_hov'
!      pause

	allocate(MucPathAtt_hov
     *	 (nzones,noof_master_destinations_original,muc_path_total_hov)
     +,stat=error)	
	if(error.ne.0) then
	  write(911,*) "allocate MucPathAtt error - insufficient memory"
	  stop
	endif
	MucPathAtt_hov(:,:,:)%node_sum = 0
 	MucPathAtt_hov(:,:,:)%node_number = 0

	allocate(NumMucPath_hov
     *	 (nzones,noof_master_destinations_original),stat=error)
	if(error.ne.0) then
	  write(911,*) "allocate NumMucPath error - insufficient memory"
	  stop
	endif
	NumMucPath_hov(:,:) = 0

	endif

c ------end of HOV/HOT -----------------------------------------------

	return
	end
