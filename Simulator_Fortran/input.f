!      subroutine input
! Add by MTI team (Jason, Xiao, Xiang, Jing) Mar 15, 2004
      subroutine input (maxintervals,network,signal,scenario,demand,
     +	              kspmuc,ksp, allsys,vehicle,runtime,vehpath,hov,
     +				  tchain, bus)
c --
c -- This subroutine reads all the input files.
c --
c -- This subroutine is called from main 
c -- This subroutine calls read_signals and prepare_network 
c --
c -- INPUT :
c --   All input data  files for DYNASMART (fort.41 -fort.50)
c --
c -- OUTPUT :
c -- no specific output 
c --
      use muc_mod
      use vector_mod
!      real demandsum,demandsumT
	!added my MTI Jan 22, 2004 for 930.9
	!Modification 05092013
      !real demandsum,demandsumT,demandsumH
	real (kind = 16) demandsum,demandsumT,demandsumH
	!Modification 05092013
! Add by MTI team (Jason, Xiao, Xiang, Jing) Mar 15, 2004
! Those variables are only used in P_Dyna as parameters for dynasmart subroutine.
! They are in fact no meaning in Dynasmart-P and Rt_dyna.
        integer maxintervals
  	  integer(4) network
        integer(4) signal
	  integer(4) scenario
	  integer(4) demand
	  integer(4) kspmuc
	  integer(4) ksp
	  integer(4) allsys
	  integer(4) vehicle
	  integer(4) runtime
	  integer(4) vehpath
	  integer(4) hov
	  integer(4) tchain
	  integer(4) bus
! +++++End of the addition

	integer OrigZone
      integer::i11 = 0
	
	integer::iseed(1) = 0 ! One seed

	integer::ifsize=0

! Modified by Jason and Xuesong July 9 2003
!	integer, allocatable :: iseed(:)
! End

! added by hayssam july 23, 2003 for 930.7	
	real::OrigVehFrac(6) = 0  !original vehicle type fractions as entered in scenario.dat


      integer::buspathtmp(1000)=0
      integer::busstoptmp(1000)=0
      integer::busvalue
      logical::CState
      integer vmaxtp, sattp, mfrtp
      INTEGER j
	integer ConZoneTmp(100)
      integer error
      real LWTmp
      real, allocatable::demtmp(:)
      integer :: FSize = 0
      character *1 reply
      logical::Fexist=.False.
      logical::Lenpnt=.False.
	logical::Grdpnt=.False.    
      integer FNodetmp, TNodetmp
! comma out by hayssam april 24 2004 DYNA 1.0
! global variables 
!	integer YLevel2N,YMove2N
      character *20 ErString
	!added by Hayssam on May 30, 2003
      integer, allocatable::mmzonetmp(:)

! Added by MTI team Jan 17 2004 0.930.7D
      integer, allocatable::ConnectorToOriginFlag(:)
	real MUC_Frac_Tmp(5),MUC_Frac_Sum,Dem_Frac_Sum

	integer Veh_Type_Tmp,Dem_Mode_Tmp,MUC_Mode_Tmp
	
! Added by Xuesong April 18 2004 1.0.0
	logical bCombinedDemandMode

! Added by MTI team April 4 2004 1.0
! a tmp array storing the indicators for each superzone
! 0: means no original zones were mapped to the superzone
      integer, allocatable::tmp_superzone(:)
! end




c -- fort.41 (network.dat) network data
c --
c -- nzones : number of zones in the network.
c -- noofnodes : number of nodes in the network.
c -- noofarcs : number of links in the network.
c --
      if(EOF(41)) then
	 ErString = "network.dat"
	 call ErReadEOF(ErString)
	endif
      read(41,*,iostat=error) nzones,noofnodes,noofarcs,kay,
     *                        SuperZoneSwitch

!Added by Alex 04042016 for snow accumulation
      inquire(file='SnowAccuFactor.dat',exist=lexist_SnowAccu)
      if(lexist_SnowAccu) then 
        read(611,*) SAFactorNum,PlowEffect
      endif
!End of snow accumulation

! Added by Hayssam Sbayti April 15, 2003 to check for the case where
! Kay is zero in network.dat
 
      if(kay.eq.0) then
	 write(911,*) 'INPUT ERROR: '
	 write(911,*) 'The number of shortest paths to be solved must be'
	 write(911,*) 'at least 1. Check the first record, fourth field'
	 write(911,*) 'in network.dat input file'
	 stop
      endif


! Added by Hayssam Sbayti April 15, 2003 to check for the case where
! Zone Aggregation Flag is neither zero or one in network.dat
 
      if(SuperZoneSwitch.lt.0.or.SuperZoneSwitch.gt.1) then
	 write(911,*) 'INPUT ERROR: '
	 write(911,*) 'The Super Zone Aggregation Flag must be either'
	 write(911,*) '0 or 1. Check the first record, fifth field'
	 write(911,*) 'in network.dat input file'
	 stop
      endif


      if(EOF(42)) then
	 ErString = "demand.dat"
	 call ErReadEOF(ErString)
	endif
      read(42,*,iostat=error) nints,multi

      if(EOF(54)) then
	 ErString = "demand_truck.dat"
	 call ErReadEOF(ErString)
	endif
      read(54,*,iostat=error) nintsT, multiT


! Added by Xuesong and Hayssam Jan 22 2004 0.930.9
      if(EOF(61)) then
	 ErString = "demand_HOV.dat"
	 call ErReadEOF(ErString)
	endif
      read(61,*,iostat=error) nintsH, multiH


      if(EOF(44)) then
	 ErString = "control.dat"
	 call ErReadEOF(ErString)
	endif
	read(44,*,iostat=error) isig

      if(EOF(45)) then
	 ErString = "ramp.dat"
	 call ErReadEOF(ErString)
	endif
	!Modified July 24 2007 
	read(45,*,iostat=error) dec_num, nrate
!	read(45,*,iostat=error) dec_num, nrate, lookup_num
!     End of modification
      if(EOF(46)) then
	 ErString = "incident.dat"
	 call ErReadEOF(ErString)
	endif
      read(46,*,iostat=error) inci_num

      if(EOF(49)) then
	 ErString = "vms.dat"
	 call ErReadEOF(ErString)
	endif
	read(49,*,iostat=error) vms_num

	! Added for weather VMS, June 8 2009
      inquire(file='vsl.dat',exist=lexist)
	if(lexist) then
        if(EOF(449)) then
	    ErString = "vsl.dat"
	    call ErReadEOF(ErString)
	  endif
	  read(449,*) num_vsl_table
	endif

      if(EOF(50)) then
	 ErString = "bus.dat"
	 call ErReadEOF(ErString)
	endif
	read(50,*,iostat=error) nubus

      if(EOF(55)) then
	 ErString = "TrafficFlowModel.dat"
	 call ErReadEOF(ErString)
	endif
      read(55,*,iostat=error) NoOfFlowModel

      if(EOF(56)) then
	 ErString = "StopCap4Way.dat"
	 call ErReadEOF(ErString)
	endif
      read(56,*,iostat=error) NLevel, NMove

      if(EOF(57)) then
	 ErString = "StopCap2Way.dat"
	 call ErReadEOF(ErString)
	endif
	read(57,*,iostat=error) Level2N,Move2N

      if(EOF(58)) then
	 ErString = "WorkZone.dat"
	 call ErReadEOF(ErString)
	endif
	read(58,*,iostat=error) WorkZoneNum

      if(EOF(59)) then
	 ErString = "GradeLenghPCE.dat"
	 call ErReadEOF(ErString)
	endif
      read(59,*,iostat=error) GradeNum,LenNum,TruckNum

      if(EOF(60)) then
	 ErString = "YieldCap.dat"
	 call ErReadEOF(ErString)
	endif
	read(60,*,iostat=error) YLevel2N,YMove2N




!added by Hayssam on June 13,2003 for DYNA 930.7
!*******************************************************************
       !modified bya hayssam August 19, 2005 partial loading implementation
	 !if(realdm.eq.2) then
	 if(realdm.eq.2 .or. realdm.eq.3) then
        
       call GETFSIZE("path.dat",ifsize)
       IF (ifsize.le.0) then
        
	  
	  
!	  if(EOF(550)) then
  	  write(911,*) 'The file path.dat is missing or empty'
        write(911,*) 'Provide the file or rename output_path.dat'
        write(911,*) 'to path.dat if using a previous run'
	  stop
	  endif
	 endif
!*******************************************************************


      if(realdm.ne.1) then
        call GETFSIZE("vehicle.dat",ifsize)
        IF (ifsize.le.0) then

	  !if(EOF(500)) then


	  write(911,*) 'The file vehicle.dat is missing or empty'
        write(911,*) 'Provide the file or rename output_vehicle.dat'
        write(911,*) 'to vehicle.dat if using a previous run'
	  stop
	  endif
	  !************************************************************

	  
	  read(500,*,iostat=error) MaxVehicles, noofstops
	  read(500,*,iostat=error) !skip a line
	  !modified by Hayssam  on August 19 2005 - partial loading implementation
	  !if(noofstops.gt.1.and.realdm.eq.2) then
	  if(noofstops.gt.1.and. (realdm.eq.2 .or.realdm.eq.3)) then
          write(911,*) 'This version doesnt allow loading trip' 
          write(911,*) 'chain with path. Please change setting'
	    stop
	  endif

	   LoadTripChain = 0
      endif
	
      if((realdm.eq.1.and.multi.eq.0.and.nubus.eq.0).or.
     * (realdm.ne.1.and.MaxVehicles.lt.1.and.nubus.lt.1)) then
	 write(911,*) 'INPUT ERROR : '
	 write(911,*) 'Total number of vehicles to be loaded is zero'
	 write(911,*) 'Please check the following files depending on'
	 write(911,*) 'the demand generation mode'
	 write(911,*) 'demand.dat'
	 write(911,*) 'vehicle.dat'
	 write(911,*) 'bus.dat'
	 stop
      endif

	nu_switch = 1000

	!********************************************************
       !modified by hayssam august 19 2005 - partial loading implementation
!	 if(realdm.eq.2) then 
	 if(realdm.eq.2 .or. realdm.eq.3) then 
	 read(550,*) xxx
	 rewind(550)
	
	if((EOF(550)).and.xxx.eq.0) then
	write(911,*) 'no paths are specified in path.dat'
	   elseif(xxx.eq.0) then
      write(911,*) 'Error in reading path.dat'
	write(911,*) 'Check format of path.dat'
	stop
	endif
	endif
	!*****************************************************

c-- Added by Archak and Zihan 20160325
c-- Read ReversibleLane.dat with ramp, link and schedule information.
   
      if (RevLane_flag.EQ.1) then      
          read(79,21,iostat=error) noofarcs_Inbound
          read(79,21,iostat=error) noofarcs_Inbound_ramp
          read(79,21,iostat=error) noofarcs_Outbound
          read(79,21,iostat=error) noofarcs_Outbound_ramp
                  
          allocate(links_for_Inbound(1:noofarcs_Inbound),stat=error)
	    if(error.ne.0) then
	      write(911,*) "links_for_Inbound error - insufficient memory"
	      stop
	    end if
	    links_for_Inbound(:)= 0
!	    
!	    allocate(links_for_Inbound_onramp(1:noofarcs_Inbound_onramp,3)
!     * ,stat=error)
!	    if(error.ne.0) then
!	      write(911,*) "links_for_Inbound_onramp error - insufficient memory"
!	      stop
!	    endif
!	    links_for_Inbound_onramp(:,:)= 0
!	    
!	    allocate(links_for_Inbound_offramp(1:noofarcs_Inbound_offramp,3)
!     * ,stat=error)
!	    if(error.ne.0) then
!	      write(911,*) "links_for_Inbound_offramp error - insufficient memory"
!	      stop
!	    endif
!	    links_for_Inbound_offramp(:,:)= 0	   
	    
	    
          allocate(links_for_Outbound(1:noofarcs_Outbound),stat=error)
	    if(error.ne.0) then
	      write(911,*) "links_for_Outbound error - insufficient memory"
	      stop
	    end if
	    links_for_Outbound(:)= 0
	    
!	    allocate(links_for_Outbound_onramp(1:noofarcs_Outbound_onramp,3)
!     * ,stat=error)
!	    if(error.ne.0) then
!	      write(911,*) "links_for_Outbound_onramp error - insufficient memory"
!	      stop
!	    endif
!	    links_for_Outbound_onramp(:,:)= 0
!	    
!	    allocate(links_for_Outbound_offramp(1:noofarcs_Outbound_offramp,3)
!     * ,stat=error)
!	    if(error.ne.0) then
!	      write(911,*) "links_for_Outbound_offramp error - insufficient memory"
!	      stop
!	    endif
!    links_for_Outbound_offramp(:,:)= 0	 	     
      end if	
      
     
21    format(i6)          
c-- End of addition	

c-- Added by Zihan 20160427
c-- Read shoulder.dat with multiple start and end time for shoulder lane.
      if (ShlLane_flag.EQ.1) then      
          read(80,*,iostat=error) noofarcs_ShlLane, noofplans_ShlLane
          allocate(SE_ShLane(noofplans_ShlLane,2),stat=error)
          do i = 1,noofplans_ShlLane
          	    read(80,*) SE_ShLane(i,1),SE_ShLane(i,2)
          enddo
      endif
c-- End addition by Zihan 20160427


c-- Added by Zihan 20160427
c-- Read predictiveinfo.dat with roll and horizon for predictive info update.
      if (i_predictiveinfo.EQ.1) then      
          read(703,*) roll
          read(703,*) horizon
          read(703,*) latency
      endif

! End Addition

c-- End addition by Zihan 20160427











	!*****************************************************

c --  call allocate_dyna to allocate memory for all dynasmart arrays
       

      if(SuperZoneSwitch.eq.0) then
	  noof_master_destinations = nzones

!      Added by Xuesong and Jason July 1 2003
	  noof_master_destinations_original = nzones


	else
	 open(file='SuperZone.dat',unit=913,status='old')
       if(EOF(913)) then
	  ErString = "SuperZone.dat"
	  call ErReadEOF(ErString)
	 endif
       read(913,*,iostat=error) noof_master_destinations

! Added by MTI team April 4 2004 1.0
	allocate(tmp_superzone(noof_master_destinations),stat=error)
	if(error.ne.0) then
	write(911,*) 'allocate tmp_superzone error -
     +	   insufficient memory'
	stop
	endif
      tmp_superzone(:)=0
! end 

!      Added by Xuesong and Jason July 1 2003
	  noof_master_destinations_original = noof_master_destinations


      endif
      noofarcs_org=noofarcs
	noofnodes_org=noofnodes
      call allocate_dyna



	
14    format(15i5)
c --
c -- Read the master destination (zone centroid) for each zone
	if(SuperZoneSwitch.eq.1) then
       if(EOF(913)) then
	  ErString = "SuperZone.dat"
	  call ErReadEOF(ErString)
	 endif
       read(913,*,iostat=error)
	 read(913,14) (OrigZone,i=1,nzones) ! this reading just for skipping
       read(913,*,iostat=error)
	 read(913,14,iostat=error) (MasterDest(i),i=1,nzones)

! Added by MTI team April 4 2004 1.0
! modified by hayssam
!      do kkk=1, noof_master_destinations
       do kkk=1, nzones
	  tmp_superzone(MasterDest(kkk)) = 1
	enddo

	do kkk=1, noof_master_destinations
	  if(tmp_superzone(kkk).eq.0) then
	    write(911, *) 'Inconsistency in superzone.dat'
		write(911, *) 'There is no original zones mapping 
     + to superzone ', kkk
	    stop
	  endif
	enddo
! end

	 close(913)
	else
       do i = 1, nzones
	  MasterDest(i) = i
       enddo
	endif
      do mm=1,nzones
   	if(MasterDest(mm).gt.noof_master_destinations)then
	 write(911,*)"Error in network.dat"
	 write(911,*)"Check the destination settings for zone",mm   
	 stop
      endif
	enddo


 	destination(:)=0

c --
c -- fort.43 (scenario.dat) the scenario data information
c --
c -- ribfa : relative indifference band (percent improvement at whcih 
c --         a user will switch his/her path).
c -- bound : threshold bound for path switching (user will not switch path
c --         unless the time savings are greater than the bound)
c -- ipinit : an index for initial path assignment
c --  if =0, randomly select from the k shortest path
c --  if =1, assign the generated vehicles to the best path (out of k) 
c -- 
      if(EOF(43)) then
	  ErString = "scenario.dat"
	  call ErReadEOF(ErString)
	endif
      read(43,*,iostat=error) ribfa,bound, istrm, ipinit, InfoPM

! Added by Xuesong and Jing March 28 2004 1.0
! Initialize istrm2 from istrm
	istrm2 = istrm+1

! Added by Jason and Jing April 11 2004 1.0
! Initialize istrm3-6 from istrm
	istrm3 = istrm+2
	istrm4 = istrm+3
	istrm5 = istrm+4
	istrm6 = istrm+5
	  
      if(ipinit.lt.0.or.ipinit.gt.1) then
        write(911,*) 'INPUT ERROR : scenario data file'
        write(911,*) 'Path index is out of the possible range'
        write(911,*) 'the value should be either 0 or 1'
        stop
      endif

      if(InfoPM.lt.0.or.InfoPM.gt.1) then
        write(911,*) 'INPUT ERROR : scenario data file'
        write(911,*) 'VMS preemption mode is out of the possible range'
        write(911,*) 'the value should be either 0 or 1'
        stop
      endif



!	CALL RANDOM_SEED (SIZE = Isize)

! Added by Jason and Xuesong July 9 2003
!	allocate(iseed(Isize)) ! To be deallocated later
! End

!	if(istrm.ne.0) then
!	 iseed(1) = istrm
!	else
!	 istrm = 930
!	 iseed(1) = istrm
!	endif
! Added by Jason and Xuesong July 9 2003
!	 do iz=2, Isize
!	  iseed(iz) = istrm + 10000
!	 end do 
! End
!	 call random_seed(PUT = iseed)

! one-seed implementation
	if(istrm.ne.0) then
	 iseed(1) = istrm
	 call random_seed(PUT = iseed)
	else
	 call random_seed()
	endif



c --
c -- com_frac : fraction of compliant vehicles
c --
      read(43,*,iostat=error) com_frac
c --
c -- Check for input errors
c --  
      if(com_frac.lt.0.or.com_frac.gt.1) then
        write(911,*) 'INPUT ERROR : scenario data file'
        write(911,*) 'com_frac is out of the possible range'
        write(911,*) 'the value should be between 0 and 1'
        stop
      endif
c --

c -- tii : the length of each simulation interval (minutes).
c -- ntt : the maximum number of simulation intervals.
c --

      read(43,*,iostat=error) itii
      tii=itii/60.0
4319  format(2I5)
c --
c -- kspstep : time interval for calculating the k shortest paths 
c --           (number of simulation intervals). 
c -- kupstep : time interval for updating the k shortest paths.
c --           (number of simulation intervals). 
c --
      read(43,*,iostat=error) kspstep,kupstep
4343  format(2i5)
c -- 
c -- Check for input errors
c --
      if(kupstep.ge.kspstep) then
        write(911,*) 'INPUT ERROR : scenario.dat '
!        write(911,*) 'kupstep is greater or equal to kspstep'
!        write(911,*) 'kupstep should be < kspstep'
        write(911,*) 'The number of simulation intervals for'
	  write(911,*) 'calculating the K-shortest paths is less than'
	  write(911,*) 'that for updating then K-shortest paths.'
        write(911,*) 'The calculation interval must be greater than'
	  write(911,*) 'the updating interval'
        stop
      endif
c -- 
c -- starttm : statistics will be collected for vehicles generated 
c --           after this time.
c -- starttm : statistics will be collected for vehicles generated 
c --           before this time.
      read(43,*,iostat=error) starttm, endtm
      
	if(starttm.ge.stagelength) then
	write(911,*) 'Simulation period is shorter than start time'
	write(911,*) 'For collecting statistics'
	write(911,*) 'Please correct'
	stop
	endif

4342  format(2f7.3)

c -- 
c -- Check for input errors
c --
      if(starttm.gt.stagelength) then
        write(911,*) 'INPUT ERROR : scenario data file'
        write(911,*) 'Warmup time is >= planning horizon'
        stop
      endif

      if(starttm.ge.endtm) then
        write(911,*) 'INPUT ERROR : scenario data file'
        write(911,*) 'Warmup time is >= end of stats collection time'
        stop
      endif


!**************Added by hayssam and Xuesong to read the new scenario.dat**
!**************** each vehicle type will have its own MUC distribution****
!************Jan, 2004 for DYNA 930.9***********************************
	
	MUC_Frac(:,:) = 0
	Dem_Frac(:) = 0
	Veh_Type(:) =0
	Dem_Mode(:) = 0
	MUC_Mode(:) = 0

! Commented out by MTI DYNASMART-P 1.0 March 19 2004
! Only initialize once for the MUC
!	Numof_Veh_Type(:) = 0
!	Numof_Veh_Class(:) = 0


	read(43,*) No_Veh_Types !number of vehicle types to be used in network  
	
	! need to check if zero vehicle types are used
	if(No_Veh_Types.lt.1) then
	   write(911,*) 'Input Error: scenario.dat' 
	   write(911,*) 'The number of vehicles to be used in the network'
	   write(911,*) 'must be at least 1'
	   write(911,*) No_Veh_Types, '   vehicle types is specified'
	   stop
	endif 


	! start reading the vehicle type attributes
	! Veh_Type(i) stores the vehicle type
	! i = 1 PC		
	! i = 2 Truck
	! i = 3 HOV
	!if Veh_Type(i) = 0 it means that vehicles of type i does not 
	!                   exist in network

	! Dem_Mode(i) = Demand mode for vehicle type i
	! 0: using demand.dat
	! 1: using individual demand table

	! Dem_Frac(i) = fraction of demand.dat to be vehicle type i
	! must have Dem_Mode(i) = 0 for this case

	! MUC_Mode(i) = MUC mode for vehicle type i
	! 0: using default MUC distribution (as specified for vehicle type 1)
	! 1: using individual MUC distribution

	!MUC_Frac(i,j)
      ! stores the MUC percentage for MUC class j for vehicle type i
	! must have MUC_Mode(i) = 1 for vehicle types other than PC
      ! i = 1 PC		
	! i = 2 Truck
	! i = 3 HOV
	! j = 1 fraction of MUC class Unresponsive  
	! j = 2 fraction of MUC class SO 
	! j = 3 fraction of MUC class UE 
	! j = 4 fraction of MUC class Enroute 
	! j = 5 fraction of MUC class VMS Responsive 

3333	format(2i5,f6.3,i3,5f6.3)

	do k=1,No_Veh_Types  !A-Level DO-LOOP
	Veh_Type_Tmp =0
	Dem_Frac_Temp = 0
	MUC_Mode_Tmp = 0
	Dem_Mode_Tmp = 0
	MUC_Frac_Tmp(:) = 0

	
      read(43,3333,iostat=error) Veh_Type_Tmp,Dem_Mode_Tmp,Dem_Frac_Tmp,
     +	MUC_Mode_Tmp, (MUC_Frac_Tmp(j),j=1,5) 
	if(error.ne.0) then
	  write(911,*) 'error in reading scenario.dat'
	  stop
	endif

		   !Veh_Type_Att_Tmp(1) returns the vehicle type

	   !Copy these values into respective arrays and check for input errors
	
		Veh_Type(Veh_Type_Tmp)    = 1
		
		
		Dem_Mode(Veh_Type_Tmp)   = Dem_Mode_Tmp
         	!the demand mode must be either 0 or 1
		 if(Dem_Mode(Veh_Type_Tmp).gt.0.and.
     +		Dem_Mode(Veh_Type_Tmp).lt.1) then 
			write(911,*)'Error! scenario.dat'
              write(911,*)'The demand mode for vehicle type',
     +Veh_Type_Tmp, 'must be either 0 or 1'
			stop
		  endif


		
		Dem_Frac(Veh_Type_Tmp)   = Dem_Frac_Tmp
      	!the demand.dat fractions must be between 0 and 1.0
		 if(Dem_Mode(Veh_Type_Tmp).eq.0) then !using demand.dat
		    if(Dem_Frac(Veh_Type_Tmp).gt.1.0.and.
     +		   Dem_Frac(Veh_Type_Tmp).lt.0) then
			   write(911,*)'Error! scenario.dat'
                 write(911,*)'The demand.dat fraction for vehicle type',
     +Veh_Type_Tmp, ' must be between 0 and 1'
			   stop
			 endif
		  endif
								
		MUC_Mode(Veh_Type_Tmp)   = MUC_Mode_Tmp
      	!the MUC mode must be either 0 or 1
		 if(MUC_Mode(Veh_Type_Tmp).gt.0.and.
     +		MUC_Mode(Veh_Type_Tmp).lt.1) then 
			   write(911,*)'Error! scenario.dat'
                 write(911,*)'The MUC mode for vehicle type',
     +Veh_Type_Tmp, ' must be either 0 or 1'
			   stop
		  endif
		  		

		MUC_Frac_Sum =0 
		do j=1,5
	    MUC_Frac(Veh_Type_Tmp,j) = MUC_Frac_Tmp(j)
		
		!check if MUC fractions are between 0 and 1
		  if(MUC_Mode(Veh_Type_Tmp).eq.1.or.Veh_Type_Tmp.eq.1)then 
		    if(MUC_Frac(Veh_Type_Tmp,j).gt.1.0.or.
     +		   MUC_Frac(Veh_Type_Tmp,j).lt.0) then
			   write(911,*)'Error! scenario.dat'
                 write(911,*)'The MUC fractions for vehicle type',
     +Veh_Type_Tmp, ' must be between 0 and 1'
			   stop
			 endif
           !check if MUC fractions sum up to 1.0
	     MUC_Frac_Sum = MUC_Frac_Sum + MUC_Frac(Veh_Type_Tmp,j)
      	   endif
          enddo
          
		!check if MUC fractions sum up to 1.0
          if(MUC_Mode(Veh_Type_Tmp).eq.1.or.Veh_Type_Tmp.eq.1)then 
      if(MUC_Frac_Sum.lt.0.9999999.or.MUC_Frac_Sum.gt.1.0000001) then
! Nov 28 2005 do not check this error when loading from vehicle file
c      IF(realdm.eq.1) then !loading from demand table
			   write(911,*)'Error! scenario.dat'
                 write(911,*)'The MUC Fractions for vehicle type',
     +Veh_Type_Tmp, ' do not sum up to 1.0'
		stop
c	ENDIF
! ENDIF
			endif
		endif



	Enddo !A-Level DO-LOOP

       !Copy default MUC Proportions to all vehicle types having MUC mode = 0 
	 do i=2,Max_No_Veh
		if(Veh_Type(i).ne.0.and.MUC_Mode(i).eq.0) Then !using default MUC distribution
			do j =1, 5
			MUC_Frac(i,j) = MUC_Frac(1,j)
	        enddo
          endif
       enddo

! Added by Xuesong April 18 2004 1.0.0
	bCombinedDemandMode = .false.


       ! check if Dem_Frac sum up to 1.0
	  Dem_Frac_Sum = 0
	 do i=1,Max_No_Veh
		if(Veh_Type(i).ne.0.and.Dem_Mode(i).eq.0) Then !using default demand table
			Dem_Frac_Sum = Dem_Frac_Sum + Dem_Frac(i)
! Added by Xuesong April 18 2004 1.0.0
			bCombinedDemandMode = .true.


		endif
	enddo

! Modified by Xuesong April 18 2004 1.0.0
!	if(Dem_Frac_Sum.lt.0.9999.or.Dem_Frac_Sum.gt.1.0001) then
	if(bCombinedDemandMode.and.(Dem_Frac_Sum.lt.0.9999. 
     +	or.Dem_Frac_Sum.gt.1.0001)) then
! Nov 28 2005 do not check this error when loading from vehicle file
c      IF(realdm.eq.1) then  
	   write(911,*)'Error! scenario.dat'
         write(911,*)'The specified fractions for demand.dat do not sum
     + up to 1.0'
         write(911,*)'Check the demand generation flag and corresponding 
     +   demand fractions'
		stop
c	ENDIF
! END
	endif


!*********************************************************
!*********************************************************
!*********************************************************


c --
c -- no_class : number of vehicle classes in the network.  Currently, there
c --            are 4 classes.
c --            1. vehicles with prespecified path.
c --            2. vehicles following System Optimal path
c --            3. vehicles following User Equilibrium path
c --            4. vehicles reaceiving en-route information (boundedly rational)
c --
c      read(43,*) no_class
c  --
c  -- iso_ok and iue_ok are indicators to know if we have SO or
c  -- UE vehicles in the network.  So, we should produce the
c  -- required output files for each procedure. 
c  --

! Commented out by Xuesong and Xiao April 08 2004 1.0.0 We already intialized these two variables in init.f
!       iso_ok=0
!       iue_ok=0

! Added by MTI team jan 24 2004
!	 ienroute_ok=0

!      read(43,*,iostat=error) (classpro(i),i=1,nu_classes)
! Modified by MTI Feb 2 2004
!	do i=1,No_Veh_Types
	do i=1,Max_No_Veh
		
	if(realdm.eq.1.and.MUC_Frac(i,2).gt.0.001) iso_ok=1
	if(realdm.eq.1.and.MUC_Frac(i,3).gt.0.001) iue_ok=1
	
! Added by MTI team jan 24 2004
	if(realdm.eq.1.and.MUC_Frac(i,4).gt.0.001) ienroute_ok=1

!      if(realdm.eq.1.and.classpro(2).gt.0.001) iso_ok=1
!      if(realdm.eq.1.and.classpro(3).gt.0.001) iue_ok=1

!4315  format(i5)
!4316  format(10f5.3)

!Added May 26 2005
      if(iSequentialLoad.eq.1) then
        !Only allow sequential loading mode when OD demand is used
	  if(realdm.ne.1) then
	write(911,*) 'In this version, sequential loading mode is not allowed' 
      write(911,*) 'if vehicle and/or path files are used!'
	    stop
	  endif

	  !Do not allow enroute info and VMS classes in the sequential loading mode
	  if((MUC_Frac(i,4).gt.0.001).or.(MUC_Frac(i,5).gt.0.001).
     +	 or.vms_num.gt.0) then
	write(911,*) 'Sequential loading mode is not allowed when'
	write(911,*) 'there are enroute info and VMS responsive vehicles'
	write(911,*) 'or VMS signs in network'
	    stop 
	  endif
	endif
!End

!      fracinf=classpro(4)
      do j=2,nu_classes
         MUC_Frac(i,j)=MUC_Frac(i,j)+MUC_Frac(i,j-1) ! we are adding up MUC_Frac to make a random number draw
       end do


	enddo
c --
c -- Check for input errors
c --
!       if(abs(classpro(nu_classes)-1.0).gt.0.0005) then
!        write(911,*) 'INPUT ERROR : scenario data file'
!        write(911,*) 'sum of percentage of vehicle classes' 
!        write(911,*) 'is not equal to 1.00'
!        stop
!	  endif
c --
c --  nu_types : number of vehicle types in the simulation. Currently, there
c --              are 6 types.
c --              1. unequipped passenger car units (pcu). 
c --              2. unequipped trucks.                    
c --              3. unequipped High Occupancy Vehilces (HOV). 
c --              4. equipped passenger car units (pcu). 
c --              5. equipped trucks.                    
c --              6. equipped High Occupancy Vehilces (HOV). 
c --
c      read(43,*) no_class2
!      read(43,*,iostat=error) (classpro2(i),i=1,nu_types-1) ! only read up to class 6. Keep in mind that class 7 is assigned to BUS internally
      
!added by hayssam to store original vehicle type fractions on july 23, 2003 for 930.7      
**************
!	do h=1,6
!	OrigVehFrac(h)=classpro2(h)
!	enddo
****************
	
!	fracinf1=classpro2(4)+classpro2(5)+classpro2(6)
!      fracnoinf1=classpro2(1)+classpro2(2)+classpro2(3)
      
!	if(realdm.eq.1) total_hov=classpro2(3)+classpro2(6)


c -- if total_hov> 0, then link_hot > 0


c --
c -- Check for input errors
c --
! changed by hayssam july 23 2003 for 930.7 too restrictive
!      if(abs(fracinf1-fracinf).gt.0.0001) then
!Commented out by Xuesong and Hayssam Jan 22 2004 0.930.9
!      if(abs(fracinf1-fracinf).gt.0.1) then
!       write(911,*) 'ERROR : Input file needs correction'
!       write(911,*) 'Scenario file : fort.43, the ratio' 
!       write(911,*) 'of boundedly rational'
!       write(911,*) 'vehicles with information is not consistant with' 
!       write(911,*) 'the sume of vehicle types 4,5 and 6'
!       Stop
!      endif
c --
c --
c --
!Commented out by Xuesong and Hayssam Jan 22 2004 0.930.9
!      if(fracnoinf1.gt.0.0) then
!      classpro2(1)=classpro2(1)/fracnoinf1
!      do i=2,nu_types/2
!         classpro2(i)=classpro2(i)/fracnoinf1+classpro2(i-1)
!      end do
!      endif
c --
!      if(fracinf1.gt.0.0) then
!      classpro2(4)=classpro2(4)/fracinf1
!      do i=5,6
!         classpro2(i)=classpro2(i)/fracinf1+classpro2(i-1)
!      end do
!      endif
c --
c -- Check for input errors
c --
!modified by hayssam on july 23, 2003 for 930.7 because too restrictive
!       if(abs((fracinf1+fracnoinf1)-1.0).gt.0.0005) then
!       if(abs((fracinf1+fracnoinf1)-1.0).gt.0.1000) then
!        write(911,*) 'INPUT ERROR : scenario data file'
!        write(911,*) 'sum of percentage of vehicle types' 
!        write(911,*) 'is not equal to 1.00'
!        stop
!       endif
c --
c -- Soda2 is an indicator for the vehicle generation.
c --  soda2=0  means that the program will generate vehicles according to
c --            the time-dependent OD matrix
c --       =1  the program will read vehicle and path files
c --       =2  read initial condition and generate vehicles according to
c --            the time-dependent OD matrix
c --       =3  read initial condition and vehicle and path files
c --
c -- NOTE : soda2 =2 or 3 are used for the Rolling Horizon procedure.
c --
c        soda2=0

         if(iteration.eq.0.and.stagest.eq.0) soda2=0
         if(iteration.gt.0.and.stagest.eq.0) soda2=1
         if(iteration.eq.0.and.stagest.gt.0) soda2=2
         if(iteration.gt.0.and.stagest.gt.0) soda2=3      
          
c      endif
c --
c -- end of reading scenario file

!allocate memory based on total number of nodes (including centriods)	

! Modifed by MTI team Jan 17 2004 0.930.7D
!	noofnodes=noofnodes+noof_master_destinations 

	if(iso_ok.eq.1.or.iue_ok.eq.1) then
	noofnodes=noofnodes+noof_master_destinations+nzones
	else
	noofnodes=noofnodes+noof_master_destinations 
	endif 


	call allocate_dyna_network_node

      do i=1,noofnodes_org
       if(EOF(41)) then
	  ErString = "network.dat"
	  call ErReadEOF(ErString)
	 endif
        read(41,*,iostat=error) nodenum(i), izone(i) !%%%
	  idnum(nodenum(i)) = i
	if(error.ne.0) then
       write(911,*) 'error in reading nodes in network.dat', idnum(i)
	 stop
	endif

      enddo
13    format(2i5)


! Added by MTI team Jan 17 2004 0.930.7D
	if(iso_ok.eq.1.or.iue_ok.eq.1) then
	do i=1,nzones
      origin(i) = noofnodes_org + i
	nodenum(origin(i))= 800000 + i 
	! give origins external numbers starting from 800000
	idnum(nodenum(origin(i)))=noofnodes_org+i !G
      izone(origin(i))=i
	enddo
	endif


c --  April 2001, Centroid Implementation
	do i=1,noof_master_destinations

! modified by MTI team Jan 17 2004 0.930.7D
! nzones takes care of the number of origins for all the zones


	if(iso_ok.eq.1.or.iue_ok.eq.1) then
	destination(i) = noofnodes_org + nzones+i
	else
      destination(i) = noofnodes_org + i
	endif


	nodenum(destination(i))= 900000 + i 
	! give centroids external numbers starting from 900000

! modified by MTI team Jan 17 2004 0.930.7D
!      destination(i) = noofnodes_org + i
!	idnum(nodenum(destination(i)))=noofnodes_org+i !G

! nzones takes care of the number of origins for all the zones
	if(iso_ok.eq.1.or.iue_ok.eq.1) then
	idnum(nodenum(destination(i)))=noofnodes_org+nzones+i !G
	else
	idnum(nodenum(destination(i)))=noofnodes_org+i !G
	endif



      izone(destination(i))=i
	enddo

c --  End of Centroid Implementation


	allocate(mmzonetmp(nzones))
c --  Going through the destination.dat to count how many artificial connecting links will be needed
      do i = 1, nzones
       if(EOF(53)) then
	!  ErString = "destination.dat"
	! Added by Hayssam Sbayti May 29, 2003 to correctly indicate 
	!which file needs to be reviewed
        write(911,*) 'Error when reading destination.dat'
	  write(911,*) 'More zones are specified in network.dat than in'
        write(911,*) 'destination.dat'
	  stop
	  !call ErReadEOF(ErString)
	 endif
	read(53,'(2i5)',iostat=error) kzonetmp, NoofConsPerZoneTmp
	   noofarcs = noofarcs + NoofConsPerZonetmp
	   mmzonetmp(i) = kzonetmp
      
! Added by MTI team DYNASMART-P 0.930.9 Mar 16 2004
	if(NoofConsPerZoneTmp.lt.1) then
        write(911,*) 'Error in reading destination.dat'
	  write(911,*) 'Each zone needs to have at least one dest'
	  write(911,*) 'Please check zone', i

        stop
	endif


	   enddo    
      close(53)
	

! Added by MTI team Jan 17 2004 0.930.7D
! Determine the number of additional connectors for origin zones


      if(iso_ok.eq.1.or.iue_ok.eq.1) then

      allocate(ConnectorToOriginFlag(noofnodes_org),stat=error)
	if(error.ne.0) then
	  write(911,*) 'allocate ConnectorToOriginFlag error -
     +	   insufficient memory'
	  stop
	endif


      do i = 1, nzones
	ConnectorToOriginFlag(:) = 0
	SumLoadWeight = 0.0
	read(52,*,iostat=error) izonetmp, NoofGenLinksPerZone(i), IDGen
	if(error.ne.0) then
         write(911,*) 'Error when reading origin.dat'
	   stop
	endif

	 do j = 1, NoofGenLinksPerZone(i)
         read(52,*,iostat=error) IUpNode,IDnNode, LWTmp !LWTmp is a temp var for LWTmp

 	   if(error.ne.0) then
           write(911,*) 'Error when reading origin.dat'
	     stop
	   endif

	if(ConnectorToOriginFlag(idnum(IUpNode)).eq.0)	then
	   ConnectorToOriginFlag(idnum(IUpNode))=1 !Used by node IUpNode
	   noofarcs = noofarcs + 1
	endif
	enddo
	enddo
	rewind(52) !to reset pointer

	endif
! End of modification by MTI team Jan 17 2004 0.930.7D



	!Added by Hayssam on May 29, 2003 to check if similar zone numbers are 
	!specified in destination.dat



  	 
	do ia=1, nzones
		do MP=ia+1,nzones
	if (mmzonetmp(ia).eq.mmzonetmp(MP)) then
	write(911,*) 'Error when reading destination.dat'
	write(911,*) 'Same zone number is specified twice'
	write(911,*) 'on lines',ia,' and', MP 
	write(911,*) 'Check destination.dat for duplication'
	write(911,*) 'of zone numbers'
	stop
	endif
		enddo
	enddo

	deallocate(mmzonetmp)

      call allocate_dyna_network_arc

! --  start reading GradeLengthPCE.dat
      if(EOF(59)) then
	 ErString = "GradeLengthPCE.dat"
	 call ErReadEOF(ErString)
	endif
      read(59,59591,iostat=error) (TruckBPnt(i),i=1,TruckNum)
      do i = 1, GradeNum
      read(59,59591,iostat=error) GradeBPnt(i)
	 do j = 1, LenNum
	 read(59,5959,iostat=error) LengthBPnt(i,j),
     * (PCE(i,j,k),k=1,TruckNum)
       enddo
	enddo
59591 format(10i4)
5959  format(f12.5,10f5.1)


c --
c -- Read link charateristics.
c --
c -- i3 : the link length in feet. s(i) : length of link i (in miles)
c -- i4 : a flag for vehicle generation from the current link.
c --              I4.eq.0 = not generation links
c --              I4.eq.1 = the demand generated on this link belongs to
c --                        the demadn generated from the zone which contains
c --                        the upstream node. 
c --              I4.eq.2 = the demand generated on this link belongs to
c --                        the demand generated from the zone which contains
c --                        the downstream node. 
c --
      Longest_link=0

	do 221 i=1,noofarcs_org
       if(EOF(41)) then
	  ErString = "network.dat"
	  call ErReadEOF(ErString)
	 endif

! Modified by Hayssam on june 18, 2003 for 0.930.7 to read right turn 
!bays from network.dat
************** Start of Modification ***********************************
!      read(41,11,iostat=error)iu,id,MTbay,i3,nlanes(i),FlowModelNum(i)
!     *    ,Vfadjust(i),SpeedLimit(i),mfrtp,sattp,link_iden(i),LGrade(i)

      read(41,11,iostat=error)iu,id,MTbay,MTbayR,i3,nlanes(i),
     * FlowModelNum(i),Vfadjust(i),SpeedLimit(i),mfrtp,sattp,
     * link_iden(i), LGrade(i)
************** End of Modification ***********************************

          

 
      if(error.ne.0) then
	  write(911,*) 'error in reading link No. ',i, ':from',iu,'to',id
	  stop
	endif
! Added August 12, 2009 for link-specific maximum density implementation
c --   Read the MaxDensity.dat, if available
      if(i_maxden.eq.1)then
        read(551, *) iu_den, id_den, maxden_l(i)
        if(iu.ne.iu_den.or.id.ne.id_den)then
          write(911,*) 'Error in reading MaxDensity.dat'
          write(911,*) 'Link: %d, Node %d -> Node %d',i,iu_den,id_den
          write(911,*) 'does not match network.dat'
          stop
        endif
        if(maxden_l(i).gt.maxden) maxden = maxden_l(i)
      endif
! End of Added August 12, 2009

! May 25 2006
      if(nlanes(i).le.0) then
        write(911,*) 'Error in network.dat'
	  write(911,*) 'Number of lanes of link ', iu, id, 'is 0'
	endif

! Added by MTI team Jan 30 2004	
	if(SpeedLimit(i).lt.1) then
      write(911,*) 'Error in specifying speed limit'
	write(911,*) 'for link:',		i 
	write(911,*) 'Upstream Node:',	iu
	write(911,*) 'Downstream Node:',id
	SpeedLimit(i)= 45 ! need to be changed later on
!      stop
	endif



! Modified by Hayssam Sbayti on April 15,2003 to check for cases where the user
! might input a traffic flow model number that is greater than the total number
! of traffic models specified in TrafficModel.dat



	if((FlowModelNum(i)).gt.NoOfFlowModel) then 
      write(911,*) 'Error in specifying the traffic flow model number'
	write(911,*) 'for link:',		i 
	write(911,*) 'Upstream Node:',	nodenum(iunod(i))
	write(911,*) 'Downstream Node:',nodenum(idnod(i))
	write(911,*) 'The associated traffic model number in network.dat' 
	write(911,*) 'for the above link is:', FlowModelNum(i) 
	write(911,*) 'It cannot be greater than the total number of'
	write(911,*) 'traffic models specified in TrafficFlowModel.dat'
	write(911,*) 'which is:',		NoOfFlowModel
      stop
	endif


	OriginLinkIndex(i) = i

	
	if(error.ne.0) then
       write(911,*) 'error in reading network.dat at up/down node',iu,id
	 stop
	endif


!      if(MTbay.gt.0) bay(i)=.True.
! Modified by Xuesong and Hayssam to allow multiple left bays on April 15, 2003
      if(MTbay.gt.0) bay(i)= MTbay
	 
	!added by Hayssam june 18, 2003 for 0.930.7 to allow for right turn bays
	if(MTbayR.gt.0) bayR(i) = MTbayR


	!added by Hayssam on April 22 2004 for DYNA 1.0
	! to keep original flow rates intact for incidents
	 Input_Flow_Rate_Orig(i) =  float(mfrtp)/3600.0*nlanes(i)
		
	MaxFlowRateOrig(i) = float(mfrtp)/3600.0*nlanes(i)
	MaxFlowRate(i) = float(mfrtp)/3600.0*nlanes(i)
	SatFlowRate(i) = float(sattp)/3600.0*nlanes(i)

! both will time nlanes in next few blocks

!     if link is too short, adjust it according to VMAX and write out warning messages
	 if(i3 < (SpeedLimit(i)+Vfadjust(i))/60.0*528.0) then
!      INQUIRE(UNIT = 511, OPENED = Fexist)
!	if(.not. Fexist) then
!	open(file='Warning.dat',unit=511,status='unknown',iostat=error)
!	if(error.ne.0) then
!         write(911,*) 'Error when opening Warning.dat'
!	   stop
!	endif
!      endif
	write
     *(511,'("shortL",i7,"-> ",i7," LinkL",i6," VMAX",f4.1,"Min L",i6)')
     *   iu,id,i3,float(SpeedLimit(i)+Vfadjust(i)),
     *   ifix((SpeedLimit(i)+Vfadjust(i))/60.0*528.0)
	   i3 = (SpeedLimit(i)+Vfadjust(i))/60.0*528.0
	endif


	if(MaxFlowRate(i).le.0.0001) then
        write(911,'("Check Saturation Flow for link #",i4)')i
	  stop
	endif
	if(nlanes(i).lt.1) then
        write(911,'("Check Number of Lanes for link #",i4)')i
	  stop
	endif


	iunod(i) = idnum(iu) !G
	idnod(i) = idnum(id) !G

!modified by Hayssam on june 19, 2003 for 930.7 to read right turn bays
!11    format(2i7,i5,i7,2i2,2i4,2i6,i2,i4)
!11    format(2i7,2i5,i7,2i2,2i4,2i6,i2,i4)

!11    format(2i7,2i5,i7,i3,i7,2i4,2i6,i2,i4)

11    format(2i7,2i5,i7,i3,i7,2i4,2i6,i3,i4)

!    198    177    0    960 1 2  +3  40  1800 5
!Modified by hayssam and xuesong to include hov/hot links on freeways
! Nov 19, 2003 for DYNA930.8
! link_type 9 = hot on a freeway
! link_type 10 = hov on a freeway
!      if(link_iden(i).eq.6) then ! HOT links
      if(link_iden(i).eq.6.or.link_iden(i).eq.9) then ! HOT links
        link_hot=link_hot+1
      endif

!Modified by hayssam and xuesong to include hov/hot links on freeways
! Nov 19, 2003 for DYNA930.8
! link_type 9 = hot on a freeway
! link_type 10 = hov on a freeway
!      if(link_iden(i).eq.8) then ! HOV links
       if(link_iden(i).eq.8.or.link_iden(i).eq.10) then ! HOV links
        link_hov=link_hov+1
      endif

	if(i3*nlanes(i)/5280.0.gt.longest_link) 
     *longest_link=i3*nlanes(i)/5280.0
c --
       





!Modified by Hayssam and Xuesong to include a maximum of 4 left 
! turn bays on April 15, 2003

         if(MTbay.lt.0.or.MTbay.gt.4) then
         write(911,*) 'INPUT ERROR : network.dat file'
         write(911,*) 'check the bay index for link number',i
         write(911,*) 'the value should be between 0 and 4'
	   write(911,*) 'the value entered is', MTbay
         stop
         endif
c --
c -- Check for input errors
c -- 

! modified by Hayssam Sbayti to check for cases where the link identification
! is out of range (1 - 10) nov 19,2003 for 930.8

!if(link_iden(i).lt.1.or.link_iden(i).gt.8) then
	if(link_iden(i).lt.1.or.link_iden(i).gt.10) then
	write(911,*) ''
      write(911,*) 'INPUT ERROR in network.dat'
      write(911,*) 'check the link identification for link number',i
	write(911,*) 'upstream node:',nodenum(iunod(i))
	write(911,*) 'downstream node:',nodenum(idnod(i))
!     write(911,*) 'the value must be between 1 and 8'
	write(911,*) 'the value must be between 1 and 10'
	write(911,*) ''
      stop
      endif


        s(i)=float(i3)/5280.0

c --  determine GRDInd and LENInd
      Lenpnt=.False.
	Grdpnt=.False.           
        do ik = 1, GradeNum
       
	 ! Modified by Hayssam, inconsistent with length and 
	 !truck percentages breakpoints
	 ! DYNA 1.0 April 5 2004
	 !   if(LGrade(i).le.GradeBPnt(ik)) then
	 ! GRDInd(i) = ik
       

	    if(LGrade(i).gt.0 .and. LGrade(i).lt.GradeBPnt(ik)) then
	      GRDInd(i) = ik - 1

         ! Added by Hayssam 
	   ! DYNA 1.0 April 5 2004
		if(ik.eq.1) then
		GRDInd(i) = 1
		endif	


	      Grdpnt=.True.
	      exit
		endif
	  enddo
	  if(.not.Grdpnt) GRDInd(i) = GradeNum
        
	  do ik = 1, LenNum
	  if(s(i).lt.LengthBPnt(GRDInd(i),ik)) then
	! Modified by Hayssam, inconsistent with length and 
	 !truck percentages breakpoints
	 ! DYNA 1.0 April 5 2004
		!   LENInd(i) = ik
            LENInd(i) = ik - 1

	       Lenpnt=.True.
		   exit
		endif
	  enddo
        if(.not.Lenpnt) LENInd(i) = LenNum

c --
c -- Initialize the entry_service for the current link.
c --
       do ijk=1,nu_de
         entry_service(i,ijk)=entrymx*nlanes(i)*tii/60.0
       enddo

221   continue ! end of reading link loop



c -- Check for input errors
c -- 


!Modified by Xuesong and Hayssam Jan 22 2004 0.930.9
!        if(total_hov.gt.0.and.((link_hov+link_hot).lt.1)) then
        if(Veh_Type(3).gt.0.and.((link_hov+link_hot).lt.1)) then
         write(911,*) 'INPUT ERROR : Found scenario.dat with HOV'
	   write(911,*) 'vehicles, but no HOV/HOT lanes are specified in'
	   write(911,*) 'network.dat'
         write(911,*) 'check the link identification for all links'
         write(911,*) 'the ID for HOT lanes is 6 or 9'
         write(911,*) 'the ID for HOV lanes is 8 or 10'
         

         stop
         endif


      call allocate_dyna_network_maxlinkveh


c --  Start to read destinations and 
c --  Start to create the connectors for destinations and centriods
	open(file='destination.dat',unit=53,status='old')      
      icount = 0
      do ia = 1, nzones
	ConZoneTmp(:) = 0
       if(EOF(53)) then
	  ErString = "destination.dat"
	  call ErReadEOF(ErString)
	 endif

! February 23, 2005
! DYNASMART-P 1.0.0.1 (Internal version number is 1.0.0.1)
! (Release version number is 1.0)
!Modified by Hayssam to read up to 100 destination nodes

!	read(53,'(2i5,30i7)',iostat=error) mzonetmp, NoofConsPerZone(ia),
!     *           (ConZoneTmp(MP),MP=1,NoofConsPerZone(ia))

	read(53,'(2i5,100i7)',iostat=error) mzonetmp, NoofConsPerZone(ia),
     *           (ConZoneTmp(MP),MP=1,NoofConsPerZone(ia))
       

	!Xuesong and Hayssam added this error checking on April 15, 2003
	!Check for non-existing destination node in file destination.dat 
	do MP=1,NoofConsPerZone(ia)
	if(idnum(ConZoneTmp(MP)).eq.0) then
	write(911,*) 'Error when reading destination.dat'
	write(911,*) 'The destination node:',ConZoneTmp(MP)
	write(911,*) 'for zone:',ia,' does not exist'
	write(911,*) 'Check network.dat for the list of existing nodes'
	stop
	endif
	enddo






	if(error.ne.0) then
         write(911,*) 'Error when reading destination.dat'
	   stop
	endif
      if(NoofConsPerZone(ia).lt.1) then
         write(911,'("Error in destination.dat")') 
	   write(911,'("Found zone",i4," contains no destination")')ia
	   stop
	endif
      do MM = 1, NoofConsPerZone(ia)
       if(ConZoneTmp(MM).lt.1) then
         write(911,*) 'Error in destination.dat, zone',ia
	   write(911,*) 'check number of zones and zone numbers'
	   stop
	 endif
	enddo




      if(NoofConsPerZone(ia).gt.0) then	  
	 do j = 1, NoofConsPerZone(ia)

         ConNoInZone(ia,j) = ConZoneTmp(j)
         icount = icount + 1

	   iline = noofarcs_org + icount


         iunod(iline)= idnum(ConNoInZone(ia,j))

         idnod(iline)= destination(MasterDest(mzonetmp))
         SpeedLimit(iline) = 100
	   FlowModelNum(iline) = NoOfFlowModel+1 ! the third type is for connector only
         Vfadjust(iline) = 0

!         bay(iline)=.False.
! Modified by Xuesong and Hayssam to allow multiple left bays on April 15, 2003
         bay(iline)= 0

         nlanes(iline)= 10
         link_iden(iline)= 99   
         MaxFlowRate(iline)= 100.0
         SatFlowRate(iline)= 100.0
         s(iline)= 0.05
         entry_service(iline,1:nu_de)=1
         LoadWeight(iline) = 0.0
         LGrade(iline) = 0.0
         GRDInd(iline) = 1
	   LENInd(iline) = 1
	   OriginLinkIndex(iline) = 0
         ! Added August 12, 2009 for link-specific maximum density implementation
         maxden_l(iline) = 250
		 enddo
	endif
	enddo   
	
C-- Added by Archak and Zihan on 20151111
C-- Determine the link number where speed limit is to be imposed 
C-- Upstream locaction is "SH_distance" miles upstream 

c-- Added by Archak and Zihan 20151111
c-- Read Linkforobs_SH.dat with number of links and link numbers for a segement where Speed Harmonization is to be imposed.
                                                                                        
      if (SH_flag) then      
        read(778,*) updatetime_speedHarm  !buffer time
        read(778,*) SH_distance
        read(778,*) noofarcs_SH
        
        allocate(links_for_SH(noofarcs_SH,2),stat=error)
        if(error.ne.0) then
            write(911,*) "links_for_SH error - insufficient memory"
            stop
        endif
        links_for_SH(:,:)= 0    

212    format(i3)

        do ii_SH=1,noofarcs_SH
            read(778,*) links_for_SH(ii_SH,1) ,links_for_SH(ii_SH,2)
        end do 
             
      endif
      
c----End Addition	 

c --  updates the iConzone: which super zone that destination i connects to
      do i = 1, noofnodes
        do j = 1, nzones
	    do k = 1, NoofConsPerZone(j)

	

            if(i.eq.idnum(ConNoInZone(j,k))) then
              iConZone(i,1)=iConZone(i,1)+1
	        if(iConZone(i,1).gt.2) then
	          !write(911,*) "Only max 2 centroids that a connector"
	          !write(911,*) "Can connect to"
                !write(911,*) "Review zone",j," node", i 
			  !write(911,*) "in your destination.dat"

!	Modified by Hayssam on June 2, 20003 to provide a clear message
          write(911,*) "A max of 2 zones may share a destination node"
                write(911,*) "Destination node",ConNoInZone(j,k) 
        	      write(911,*) "Has been used more than twice"
	          write(911,*) "Check zone",j,"    in destination.dat"

	          stop
	        endif
	        iConZone(i,iConZone(i,1)+1) = j
	
      
	      endif
	    enddo
	  enddo
	enddo
   

c --
c -- end of reading network file (fort.41)

c --
c --


c -- read demand data (fort.42).
c -- if soda2 = 1 or 3, then vehicle and path files are provided and
c -- there is no need to read the demand file.
c --
c -- nints : number of demand intervals
c -- multi : multiplication factor for the demand (i.e. each value in the
c --         provided OD matrix will be multiplied by this factor to specify
c --         the number of vehicle generated from each zone to each destination.
c --
       if(EOF(42)) then
	  ErString = "demand.dat"
	  call ErReadEOF(ErString)
	 
	 endif
! May 18 2006 change to free format
c      read(42,231,iostat=error) (begint(i),i=1,nints+1)
	read(42,*,iostat=error) (begint(i),i=1,nints+1)

! Modified by Xuesong and Hayssam April 2004 0.930.9
!      if(nintsT.gt.0) read(54,231) (begintT(i),i=1,nints+1)
c      if(nintsT.gt.0) read(54,231) (begintT(i),i=1,nintsT+1)
	if(nintsT.gt.0) read(54,*) (begintT(i),i=1,nintsT+1)

! Added by Xuesong and Hayssam Jan 22 2004 0.930.9
c      if(nintsH.gt.0) read(61,231) (begintH(i),i=1,nintsH+1)
      if(nintsH.gt.0) read(61,*) (begintH(i),i=1,nintsH+1)


239   format(2i5)
231   format(121f6.1)
c -- 
c -- Check for input errors
c --
        do k=2,nints+1
         if(begint(k-1).ge.begint(k)) then
         write(911,*) 'INPUT ERROR : demand data file'
         write(911,*) 'check the start of the', k-1,'th demand interval'
         write(911,*) 'and the', k, 'th interval.'
         stop
         endif
       enddo
c --
c -- Initialize the counter for the demand intervals (int) 
c --
c --
c --
c -- read the zonal time-dependent demand data matrix
c -- 	
      allocate (demtmp(nzones))

      demandsum = 0
      do 223 int=1,nints
	if(begint(int).ge.stagelength) exit 
	! only count vehicle those demand matrices within stagelength
	read(42,*,iostat=error)
      DO 223 iz=1,nzones
	demtmp(:) = 0
      read(42,224,iostat=error) (demtmp(izz),izz=1,nzones)

	if(error.ne.0) then
         write(911,*) 'Error when reading demand.dat'
      ! added by Hayssam sbayti on May 29, 2003 
	   write(911,*) 'Fewer zones are specified in network.dat than'
	   write(911,*) 'Provided for in demand.dat'
	   stop
	endif
	  do ioz = 1, nzones
	  demandsum = demandsum + demtmp(ioz)*multi
	  enddo
223   continue
224   format(6f10.4)
c --
      if(realdm.eq.1) then
	   MaxVehicles = nint(demandsum*1.02)
	endif	  

      rewind(42)
	read(42,*)
	read(42,*)

! --  For truck demand
      
	demandsumT = 0.0
	if(nintsT.gt.0) then
 
      do 2233 int=1,nintsT
       if(EOF(54)) then
	  ErString = "demand_truck.dat"
	  call ErReadEOF(ErString)
	 endif
	read(54,*,iostat=error)
      DO 2233 iz=1,nzones
      read(54,2244,iostat=error) (demtmp(izz),izz=1,nzones)
      
	if(error.ne.0) then
         write(911,*) 'Error in input when reading demand_truck.dat'
	   stop
	endif
	  do ioz = 1, nzones
	  demandsumT = demandsumT + demtmp(ioz)*multiT
	  enddo
2233  continue
2244  format(6f10.4)

      rewind(54) 
	!skip the first 2 records
	read(54,*,iostat=error)
	read(54,*,iostat=error)





! -- if demand_truck.dat exist update classpro2

!********** added by Hayssam on july 23,2003 for 930.7 *************
	if (demandsumT.gt.0) then !if demand_truck.dat is used
	! then update the original fraction of vehicle types
	! keeping in mind that vehicle type fractions in scenario.dat apply only to 
	! demand.dat 
	
	do i=1,6
		if(i.ne.2.and.i.ne.5) then !excluding trcuks w/o info and w/ info 
		classpro2(i)=OrigVehFrac(i)*demandsum/(demandsum+demandsumT)
		elseif (i.eq.2) then ! if truck type w/o info
	classpro2(i)=(OrigVehFrac(i)*demandsum + (1-fracinf)*demandsumT) 
		classpro2(i)=classpro2(i)/(demandsum+demandsumT)
		elseif (i.eq.5) then ! if truck type w/ info
		classpro2(i)=(OrigVehFrac(i)*demandsum + fracinf*demandsumT) 
		classpro2(i)=classpro2(i)/(demandsum+demandsumT)
		endif
	enddo

	total_hov = classpro2(3)+classpro2(6)

	do i=1,3 !update cummulative probability of vehicle types w/o info
		if (i.eq.1) then  
	    classpro2(i) = classpro2(i)/(classpro2(i)+classpro2(i+1)+ !initialzing 
     *                                 classpro2(i+2))		
		else
		classpro2(i)=(classpro2(i)/(1-fracinf))+classpro2(i-1)
		endif
	enddo 
	
	do i=4,6 !update cummulative probability of vehicle types w/o info
		if (i.eq.4) then  
	    classpro2(i) = classpro2(i)/(classpro2(i)+classpro2(i+1)+ !initialzing and normalizing
     *                                 classpro2(i+2))		
		else 
		classpro2(i)=(classpro2(i)/(fracinf))+classpro2(i-1)
		endif
	enddo 

	endif	

!********** End of addition *******************
	
		
! comma out by hayssam on july 23, 2003 for 930.7			
!********************************************************************
!        classpro2(1)=demandsum/(demandsum+demandsumT)
!	   classpro2(2)=classpro2(1)+demandsumT/(demandsum+demandsumT)
!	   classpro2(3:nu_types) = 1.0
***********************************************************************
      endif

!*************************demand_HOV***************
! added by MTI to count the number of HOV vehicles 
! --  For HOV demand
      
	demandsumH = 0.0
	if(nintsH.gt.0) then
 
      do int=1,nintsH
       if(EOF(61)) then
	  ErString = "demand_HOV.dat"
	  call ErReadEOF(ErString)
	 endif
	read(61,*,iostat=error)
      
	DO iz=1,nzones
      read(61,2244,iostat=error) (demtmp(izz),izz=1,nzones)
	if(error.ne.0) then
         write(911,*) 'Error in input when reading demand_HOV.dat'
	   stop
	endif
	  do ioz = 1, nzones
	  demandsumH = demandsumH + demtmp(ioz)*multiH
	  enddo

	enddo
	enddo

      rewind(61)
	!skip the first 2 records
	read(61,*,iostat=error)
	read(61,*,iostat=error)
	endif
!******************************************************************

      if(realdm.eq.1) then
! modified by hayssam and xuesong jan 22, 2004 for 930.9
! need to add HOV vehicles from demand_HOV

!	   MaxVehicles = MaxVehicles + nint((demandsumT)*1.05) 
     

	   MaxVehicles = MaxVehicles*1.2 + nint((demandsumT)*1.2) 
     +   + nint((demandsumH)*1.2)
	endif	  

	call allocate_dyna_vehicle
c --

c --
c -- end of reading the demand data (fort.42)
c --
c --
c -- Read the signal control data file.  This file reads only the
c -- first line of the file, then it calls read_signals at the end.  
c --
c -- isig : number of signal setting plans.
c -- startsig : start time for each signal plan.
c -- isigcount : a counter to keep track of the signal plan to 
c -- be activated when the clock time is equal to its start.
c --
       if(EOF(44)) then
	  ErString = "control.dat"
	  call ErReadEOF(ErString)
	 endif
	 !Hooram Modification: Reading Format
	!read(44,1223,iostat=error) (strtsig(i),i=1,isig)
	 read(44,*,iostat=error) (strtsig(i),i=1,isig)
	 !Hooram-End of Modification: Reading Format
	 strtsig(isig+1)=2*stagelength
1223    format(50f8.2)   
!modified by Zihan 20160427, change50f6.2 to 50f8.2 to run 24 hours simulation

	isigcount=1
c --
c -- start reading the ramp metering data.
c --
c -- array definition :  detector(i,7)
c -- (i,1): detector number
c --    2 : from node
c --    3 : to node
c --    from and to nodes define the downstream link for the metered ramp.
c --    4 : position of the first detector on the downstream link (in feet). 
c --    5 : position of the second detector on the downstream link (in feet).
c --    both distances in 4 and 5 are measured from the downstream node. 
c --    6 : upstream  node of the metered ramp
c --    7 : downstream  node of the metered ramp
c --
c --    ramp_par(i,3)
c --       1 : cons1, used in RATE=RATEP+cons1(cons2-OCC)
c --       2 : cons2
c --      According to simulation experiments, the default values of 
c --      cons1 and cons2 are 0.32 and 0.2 respectively and these values 
c --      may be calibrated using actual data.
c --       3 : ramp rate (sturation flow rate on the ramp veh/sec/lane)
c --
c -- nrate : the time interval for checking the ramp metering (in minutes)
c --
    	if(dec_num.gt.0) then
      do 455 i=1,dec_num
       if(EOF(45)) then
	  ErString = "ramp.dat"
	  call ErReadEOF(ErString)
	 endif
!      modified by Hayssam and Jason to free format 
!      read(45,451,iostat=error)(detector(i,j),j=1,7),
!     *(ramp_par(i,j),j=1,3)

	!Modified July 24 2007
      read(45,*,iostat=error)(detector(i,j),j=1,7),(ramp_par(i,j),j=1,3)
!		read(45,*,iostat=error) detector(i,1), ramp_type(i), 
!     +		(detector(i,j),j=2,7),(ramp_par(i,j),j=1,3)
	!End
		if(error.ne.0) then
		   write(911,*) 'Error when reading ramp.dat'
		   stop
		endif

! **********************************************
!added by hayssam and xuesong for 930.8 nov 17
	if(idnum(detector(i,2)).gt.0) then
		detector(i,2) = idnum(detector(i,2))!G  //copy back internal node number
	else
	write(911,*) 'INPUT ERROR: RAMP.DAT' 
	write(911,*) 'node', detector(i,2),   
     + '   does not exist in network.dat'
	stop
	endif

	if(idnum(detector(i,3)).gt.0) then
		detector(i,3) = idnum(detector(i,3))!G//copy back internal node number
	else
	write(911,*) 'INPUT ERROR: RAMP.DAT '
	write(911,*) 'node', detector(i,3),   
     + '   does not exist in network.dat'
	stop
	endif


	if(idnum(detector(i,6)).gt.0) then
		detector(i,6) = idnum(detector(i,6))!G//copy back internal node number
	else
	write(911,*) 'INPUT ERROR: RAMP.DAT' 
	write(911,*) 'node', detector(i,6),   
     + '   does not exist in network.dat'
	stop
	endif

	if(idnum(detector(i,7)).gt.0) then
		detector(i,7) = idnum(detector(i,7))!G//copy back internal node number
	else
	write(911,*) 'INPUT ERROR: RAMP.DAT' 
	write(911,*) 'node', detector(i,7),   
     + '   does not exist in network.dat'
	stop
	endif
	
!********************************************************


      detector_length(i)=detector(i,4)-detector(i,5)
c --
c -- Check for input errors
c --
       if(detector_length(i).le.0) then
         write(911,*) 'INPUT ERROR : Ramp metering data'
         Write(911,*) 'detector length is less than zero'
	   write(911,*) 'The detector location for edge 1 must be'  
	   write(911,*) 'greater than that for edge 2'
	   write(911,*) 'Please check the input file'
         stop
       endif


c --
      read(45,453,iostat=error) ramp_start(i),ramp_end(i)
	! Added July 24 2007
	! take care of the start up cycle for type 1 and 2
!	if(ramp_type(i).le.2) then
!		ramp_start(i) = ramp_start(i) + ramp_par(i,1)/60.0 ! in min
!	endif
	! End
455   continue
      endif

451   format(7i6,f7.3,2f6.2)
452   format(2i6)
453   format(2f8.2)
c --
c --
c --
c -- link_dectector(i) : defines the detector number which exist on link i.
c -- det_link(j) : defines the link number for detector j.
c -- detector_ramp(j) : the ramp controlled by detector j.
c --
c -- set link_detector and det_link
c -- 
      if(dec_num.gt.0) then
      do i=1,dec_num
         do j=1,noofarcs
            if(detector(i,2).eq.iunod(j).and.
     +		detector(i,3).eq.idnod(j)) then
               link_detector(j)=i
            endif
!potential inefficiency hayssam nov 15 930.8
            if(detector(i,2).eq.iunod(j).and.
     +      detector(i,3).eq.idnod(j)) then
               det_link(i)=j
            endif

            if(detector(i,6).eq.iunod(j).and.
     +      detector(i,7).eq.idnod(j)) then
               detector_ramp(i)=j
            endif
          enddo
c --
c -- Check for input errors
c --
         if(det_link(i).eq.0) then 
			write(911,*) 'INPUT ERROR!: ramp.dat input file'
			write(911,*) 'Check the detector link 
     +					for metered ramp number',i
	!Added by hayssam on Nov 13, 2003 for DYNA 930.8
			write(911,*) 'Link',nodenum(detector(i,2)),'   -->',  
     +		nodenum(detector(i,3)),'    does not exist in network.dat'	 
			stop
         endif
c --
!*********************************************************************
! added by hayssam to check if detector link and detector ramp are the same
!Nov 12, 2003 for 930.8
		if(detector_ramp(i).eq.det_link(i)) then
			write(911,*) 'INPUT ERROR!: RAMP.DAT' 
			write(911,*)'The freeway detector link and 
     +			ramp link are the same'
			stop
		endif



! added by hayssam to check for location of detectors
!Nov 12, 2003 for 930.8
       if( (detector(i,4).gt.5280*s(det_link(i))).or.
     +  (detector(i,5).gt.5280*s(det_link(i)))) then
         write(911,*) 'INPUT ERROR: Ramp metering data'
         write(911,*) 'Location of detectors are out of range'
	write(911,*)'The value entered is larger than the length of the'
      write(911,*)'freeway link', nodenum(detector(i,2)),'   -->',
     + nodenum(detector(i,3))
	write(911,*)'Which is',s(det_link(i))*5280,   'feet'
         stop
       endif


! added by hayssam to check for if ramp link exists in network
!Nov 12, 2003 for 930.8
      if(detector_ramp(i).eq.0) then 
      write(911,*) 'INPUT ERROR!: ramp.dat input file'
      write(911,*) 'Check the ramp link for metered ramp number',i
	write(911,*) 'Ramp',detector(i,6),'   -->',  
     +detector(i,7),'    does not exist in network.dat'	 
	  stop
         endif
!**************************************************************************

      enddo

	endif

	!Added July 24 2007
	! read the metering rate lookup table
!	if(lookup_num.gt.0) then
!	do i = 1,lookup_num
!		read(45, *,iostat=error)  itmp ! lookup table number
!		if(i.eq.itmp) then
!			read(45,*,iostat=error) (ramp_lookup(i,1,j),j=1,8) ! metering rate
!			read(45,*,iostat=error) (ramp_lookup(i,2,j),j=1,8) ! occupancy
!			read(45,*,iostat=error) (ramp_lookup(i,3,j),j=1,8) ! volume
!			read(45,*,iostat=error) (ramp_lookup(i,4,j),j=1,8) ! speed
!			do j=1,8
!				ramp_lookup(i,1,j) = ramp_lookup(i,1,j)/3600.0 ! metering rate in veh/s
!				ramp_lookup(i,2,j) = ramp_lookup(i,2,j)/100.0 ! occupancy is (0,1)
!				ramp_lookup(i,3,j) = ramp_lookup(i,3,j)/3600.0 ! volume is in veh/s
!				ramp_lookup(i,4,j) = ramp_lookup(i,4,j)/60 ! speed in mile/min
!			enddo
!		else
!			write(911,*) 'The lookup table number does not match'
!			write(911,*) 'Please check ramp.dat'
!			stop
!		endif
!	enddo
!	endif
	! End July 24 2007

c --
c -- end read ramp metering data
c --

! re-duplicated from reading generation.dat
! hayssam and xuesong for 930.7 on aug 8 2003
!********************************************************************
!********************************************************************
c --  Starting Reading origin.dat
! --  NoofGenLinksPerZone is read from origin.dat:izlins
! --  LinkNoInZone() keeps track of the link number:izone
! --  total link length for zones are stored in TotalLinkLenPerZone():totlmz

! Added by MTI team Jan 17 2004 0.930.9
! This part just takes care of adding connectors for origin zones   

      do i = 1, nzones
	ConnectorToOriginFlag(:) = 0
	SumLoadWeight = 0.0
	read(52,*,iostat=error) izonetmp, NoofGenLinksPerZone(i), IDGen

	if(error.ne.0) then
         write(911,*) 'Error when reading origin.dat'
	   stop
	endif

! added by Hayssam to check for error
! april 7 2004 for DYNA 1.0
	if(izonetmp.ne.i) then
	  if(i.gt.2) then
	   write(911,*) 'Error in origin.dat'
	   write(911,*) 'Incorrect number of generation links for'
	   write(911,*) 'zone', i-2 ,'   or zone', i-1
	   stop
	  else
         write(911,*) 'Error in origin.dat'
	   write(911,*) 'Incorrect number of generation links for'
	   write(911,*) 'zone', i-1
	   stop
	  endif
	endif

	 do j = 1, NoofGenLinksPerZone(i)
         read(52,*,iostat=error) IUpNode,IDnNode, LWTmp !LWTmp is a temp var for LWTmp
	 if(iso_ok.eq.1.or.iue_ok.eq.1) then
	if(ConnectorToOriginFlag(idnum(IUpNode)).eq.0.)then

         icount = icount + 1 ! icount has been used in reading destination.dat

	   iline = noofarcs_org + icount

         iunod(iline)= origin(izonetmp)
         idnod(iline)= idnum(IUpNode)
	   ConnectorToOriginFlag(idnum(IUpNode))=1 !Used by node IUpNode

         SpeedLimit(iline) = 100
	   FlowModelNum(iline) = NoOfFlowModel+1 ! the third type is for connector only
         Vfadjust(iline) = 0

!         bay(iline)=.False.
! Modified by Xuesong and Hayssam to allow multiple left bays on April 15, 2003
         bay(iline)= 0

         nlanes(iline)= 3
         link_iden(iline)= 100   
         MaxFlowRate(iline)= 100.0
         SatFlowRate(iline)= 100.0
         s(iline)= 1.00
         entry_service(iline,1:nu_de)=entrymx*nlanes(i)*tii/60.0
         LoadWeight(iline) = 0.0
         LGrade(iline) = 0.0
         GRDInd(iline) = 1
	   LENInd(iline) = 1
	   OriginLinkIndex(iline) = 0
         ! Added August 12, 2009 for link-specific maximum density implementation
         maxden_l(iline) = 250
		endif
       endif
! End of modification by MTI team Jan 17 2004 0.930.7D




	enddo
	enddo
	rewind(52) !to reset pointer


! --  sort all the links to be forward *, only need to sort up and downstream nodes 
!     since all other arrays are identical for the two 
! --  sort upstream nodes first, then downstream nodes



      do i = 1, noofarcs - 1
	  do j = i + 1, noofarcs
	    if(iunod(j).lt.iunod(i)) then
	      call SwapIntArray2B(iunod(i),iunod(j))
            call SwapIntArray2B(idnod(i),idnod(j))

!	      call SwapLogArray(bay(i),bay(j))
! Modified by Xuesong and Hayssam to allow multiple left bays on April 15, 2003
	      call SwapIntArray1B(bay(i),bay(j))

!added by Hayssam on June 19,2003 for 0.930.7
		     call SwapIntArray1B(bayR(i),bayR(j))

            call SwapIntArray1B(nlanes(i),nlanes(j))
            ! Added August 12, 2009 for link-specific maximum density implementation
            call SwapIntArray2B(maxden_l(i),maxden_l(j))
            
            call SwapIntArray2B(link_iden(i),link_iden(j))
            call SwapRealArray(MaxFlowRate(i),MaxFlowRate(j))
            call SwapRealArray(MaxFlowRateOrig(i),MaxFlowRateOrig(j))

		!added by hayssam april 22 2004 for DYNA 1.0
		! multiple incidents
      call SwapRealArray(Input_Flow_Rate_Orig(i),
     +	Input_Flow_Rate_Orig(j))

            call SwapRealArray(SatFlowRate(i),SatFlowRate(j))
            call SwapRealArray(s(i),s(j))
	      call SwapIntArray1B(FlowModelNum(i),FlowModelNum(j))
            call SwapIntArray2B(Vfadjust(i),Vfadjust(j))
            call SwapIntArray2B(SpeedLimit(i),SpeedLimit(j))
            call SwapIntArray1B(LGrade(i),LGrade(j))
            call SwapIntArray2B(GRDInd(i),GRDInd(j))
            call SwapIntArray2B(LENInd(i),LENInd(j))
            call SwapIntArray2B(OriginLinkIndex(i),OriginLinkIndex(j))
	    endif
	  enddo
	enddo 
!  -- Sort downstream node given the same upstream nodes
      do i = 1, noofarcs - 1
        do j = i + 1, noofarcs
            if (iunod(j).eq.iunod(i)) then
		    if (idnod(j).lt.idnod(i)) then
 	         call SwapIntArray2B(iunod(i),iunod(j))
               call SwapIntArray2B(idnod(i),idnod(j))

!	         call SwapLogArray(bay(i),bay(j))
! Modified by Xuesong and Hayssam to allow multiple left bays on April 15, 2003
	         call SwapIntArray1B(bay(i),bay(j))

		!added by Hayssam on June 19,2003 for 0.930.7 for right turn bays
		  call SwapIntArray1B(bayR(i),bayR(j))
            ! Added August 12, 2009 for link-specific maximum density implementation
          call SwapIntArray2B(maxden_l(i),maxden_l(j))
          
          call SwapIntArray1B(nlanes(i),nlanes(j))
          call SwapIntArray2B(link_iden(i),link_iden(j))
      
	!added by hayssam on Jan 17 2006
	!for incidents
	call SwapRealArray(Input_Flow_Rate_Orig(i),
     +	Input_Flow_Rate_Orig(j))


               call SwapRealArray(MaxFlowRate(i),MaxFlowRate(j))
               call SwapRealArray(MaxFlowRateOrig(i),MaxFlowRateOrig(j))
               call SwapRealArray(SatFlowRate(i),SatFlowRate(j))
               call SwapRealArray(s(i),s(j))
	         call SwapIntArray1B(FlowModelNum(i),FlowModelNum(j))
               call SwapIntArray2B(Vfadjust(i),Vfadjust(j))
               call SwapIntArray2B(SpeedLimit(i),SpeedLimit(j))
               call SwapIntArray1B(LGrade(i),LGrade(j))
               call SwapIntArray2B(GRDInd(i),GRDInd(j))
               call SwapIntArray2B(LENInd(i),LENInd(j))
              call SwapIntArray2B(OriginLinkIndex(i),OriginLinkIndex(j))
              endif
	      else
	        exit
            endif
	  enddo
	enddo


! added by MTI team Jan 17 2004 0.930.7D
! Prepare network after adding connectors
      call prepare_network()

! Moved by MTI team Jan 17 2004 0.930.7D
! Assign link numbers to array LinkNoInZone

      do i = 1, nzones
	SumLoadWeight = 0.0
	read(52,*,iostat=error) izonetmp, NoofGenLinksPerZone(i), IDGen
	if(error.ne.0) then
         write(911,*) 'Error when reading origin.dat'
	   stop
	endif

	 do j = 1, NoofGenLinksPerZone(i)
         read(52,*,iostat=error) IUpNode,IDnNode, LWTmp !LWTmp is a temp var for LWTmp

 	   if(error.ne.0) then
           write(911,*) 'Error when reading origin.dat'
	     stop
	   endif


	  ! added by xuesong on Jan 2004 for 930.9
    	   if(idnum(IUpNode).gt.0.and.idnum(IDnNode).gt.0) then
	   LinkNo = GetFLinkFromNode(idnum(IUpNode),idnum(IDnNode))

!****************************
!moved upward by hayssam on Nov 23/2003 for 930.8
  	   	   if(LinkNo.lt.1) then
	      write(911,*) 'Error in origin.dat'
          write(911,*) 'Link', IUpNode,'  ->', IDnNode,'   does not exit
     + for zone', i	 
            write(911,*) 'If the link does in fact exist in network.dat, 
     + then check if the number of'
      write(911,*)'generation links is mis-specified for 
     +any of the zones'   
		  stop 
	   endif
!**********************************************************

      else
	    write(911,*) 'Error in origin.dat'
          write(911,*) 'Link', IUpNode,'  ->', IDnNode,'   does not exit
     + for zone', i	 
            write(911,*) 'If the link does in fact exist in network.dat, 
     + then check if the number of'
      write(911,*)'generation links is mis-specified for 
     +any of the zones'   
		  stop 

      endif



	   !modified by hayssam on Nov 3 2003 for 930.8
	    !if(link_iden(LinkNo).eq.6) then
	!**************************
		if( (link_iden(LinkNo).eq.6).or.(link_iden(LinkNo).eq.9).or.
     +	(link_iden(LinkNo).eq.8).or.(link_iden(LinkNo).eq.10) )then
		 write(511,'("Check origin.dat and netowrk.dat")')
	     write(511,'("link ",i5," -->",i5," is an HOT/HOV link")') 
     *                   IUpNode,IDnNode
	     write(511,'("It cannot be a generation link in zone",i3)') i 
	    endif			
!*****************************************************************

		if(link_iden(LinkNo).eq.1) then
           write(511,'("link ",i5," -->",i5," is highway/freeway that 
     +	 is specified as a generation link")') IUpNode,IDnNode
	     write(511,*)'Freeways are not recommended to be specified as  
     +     generation links unless they are on the boundaries'
           write(511,*) 'Check origin.dat for zone', i 
	   endif



!	   if(LinkNo.lt.1) then
!	      write(911,*) 'Error in origin.dat'
!            write(911,*) 'Link doesnt exit'
!            write(911,*) 'zone, ud, nd', i, IUpNode, IDnNode
!	      stop 
!	   endif


         LinkNoInZone(i,j) = LinkNo

! Added by MTI team Feb 7 2004 0.930.6
         LGenerationFlag(LinkNo) = i
	enddo
	enddo
	rewind(52) !to reset pointer



c --
c -- start read movement data
c --
c --
       do i=1,noofarcs
          move(i,nu_mv+1)=llink(i,nu_mv+1)
          movein(i,nu_mv+1)=inlink(i,nu_mv+1)
       end do
c --
c --
cccccc we still read movement.dat based on the 
cccccc  the old number of links
      do 100 i=1,noofarcs_org
       ifrom=0
       ito=0
       mlink=0
       mleft=0
       mst=0
       mright=0
       mother1=0
       mother2=0
       mother3=0
       mother4=0
       muturn=0
!***********added by hayssam july 28 2003 for 930.7
	iuturn_flag=0

10    format(8i7)

!10    format(10i7)

       if(EOF(47)) then
	  ErString = "movement.dat"
	  call ErReadEOF(ErString)
	 endif
      read(47,10,iostat=error) 
!	modified by hayssam on july 28, 2004 for 930.7 to provide a Uturn flag
!   uturn_flag = 1, then uturn is allowed
!   uturn_flag = 0, then uturn is not allowed
!   *     ifrom,ito,ileft,ist,iright,iother1,iother2
     *   ifrom,ito,ileft,ist,iright,iother1,iother2,iuturn_flag
	

	if(error.ne.0) then
         write(911,*) 'Error when reading movement.dat'
	   stop
	endif

c --   check if ito is connecting to centriods.  
!      Max 2 conections as represented in i1 and i2
       mflag = 0
	 iother3 = 0
	 iother4 = 0
       do mp = 1, nzones
         do mn = 1, NoofConsPerZone(mp)
            if(ito.eq.ConNoInZone(mp,mn)) then
	         mflag = mflag + 1
	         if(mflag.eq.1) then
	           iother3 = nodenum(destination(MasterDest(mp)))
	         elseif(mflag.eq.2) then
	           iother4 = nodenum(destination(MasterDest(mp)))
               elseif(mflag.gt.2) then
                 write(911,*) "DYNASMART-P only allows max 2 centriods"
                 write(911,*) "That each connector could connect to"
	           write(911,*) "Please check your destination.dat"
                 stop
	         endif
	      endif
         enddo
	 enddo

	! --  determine the movement type
      ! moved up by xuesong and hayssam

      if(ifrom.ne.0.and.ito.ne.0)
     *   mlink =   GetFLinkFromNode(idnum(ifrom),idnum(ito))
      if(ito.ne.0.and.ileft.ne.0)
     *   mleft =   GetFLinkFromNode(idnum(ito),idnum(ileft))
      if(ito.ne.0.and.ist.ne.0)
     *   mst =     GetFLinkFromNode(idnum(ito),idnum(ist))
      if(ito.ne.0.and.iright.ne.0)
     *   mright =  GetFLinkFromNode(idnum(ito),idnum(iright))
      if(ito.ne.0.and.iother1.ne.0)       
     *   mother1 = GetFLinkFromNode(idnum(ito),idnum(iother1))
      if(ito.ne.0.and.iother2.ne.0)
     *   mother2 = GetFLinkFromNode(idnum(ito),idnum(iother2))
      if(ito.ne.0.and.iother3.ne.0)
     *   mother3 = GetFLinkFromNode(idnum(ito),idnum(iother3))
      if(ito.ne.0.and.iother4.ne.0)
     *   mother4 = GetFLinkFromNode(idnum(ito),idnum(iother4))

      jfind = 0
c --  jfind = 1 means an U turn physically exists from ifrom->ito
      KK = GetBLinkFromNode(idnum(ito),idnum(ifrom))
 	if(KK.gt.0) then
 !       if(idnum(ito).eq.(KK)) then
 !		if(idnod(mlink).eq.idnod(kk)) then
         jfind = 1
!	  endif
              


! added by hayssam to store and check uturn prohibitions
! july 28, 2003 for DYNA 930.7
	UturnFlag(mlink)=iuturn_flag  !copy from movement.dat 

	if(jfind.gt.0) then


!************************
!Modified by Hayssam and Xuesong to implement HOV/HOT links on freeway
!Nov 19,2003 for 930.8
!      if(link_iden(GetFLinkFromNode(idnum(ifrom),idnum(ito))).ne.1.or.
!     *   link_iden(GetFLinkFromNode(idnum(ifrom),idnum(ito))).ne.2) 

      if(link_iden(GetFLinkFromNode(idnum(ifrom),idnum(ito))).ne.1.or.
     *   link_iden(GetFLinkFromNode(idnum(ifrom),idnum(ito))).ne.2.or.
     *   link_iden(GetFLinkFromNode(idnum(ifrom),idnum(ito))).ne.9.or.
     *   link_iden(GetFLinkFromNode(idnum(ifrom),idnum(ito))).ne.10) 
!***********************************************

     *   muturn = GetFLinkFromNode(idnum(ito),idnum(ifrom)) !Forward * link # of ifrom->ito is not freeway
	!if not a freeway, then muturn is positive
	endif
!	endif

! added by hayssam to store and check uturn prohibitions
! july 28, 2003 for DYNA 930.7
!********************
	
	
!	if((muturn.gt.0.or.jfind.gt.0).and.iuturn_flag.eq.1) then
! above condition changed by Hayssam on Sep 4, 2003
	if((muturn.eq.0).and.(jfind.gt.0).and.(iuturn_flag.eq.1)) then
! If a freeway link with an allowed uturn and if a physical u-turn exists
! then prohibit that uturn 
	UturnFlag(mlink)=0
	write(511,*)
	write(511,*) 'A u-turn has been specified on a freeway'
	write(511,*) 'link', ifrom,  '   -->',ito
	write(511,*) 'The u-turn has been internally prohibited'
	write(511,*)
	endif

	kkk = mlink !to temporary store mlink
	 !if user prohibits u-turns on a generation link, internally 
	 !allow u-turns on that links
	if(jfind.gt.0.and.iuturn_flag.eq.0) then 
	!physical u-turn exists, but prohibited by user
		do iii = 1, nzones
			do jjj = 1, NoofGenLinksPerZone(iii)
				if (kkk.eq.LinkNoInZone(iii,jjj)) then
      !line below added by Hayssam on Sep 4,2003
	UturnFlag(kkk)=1
	write(511,*)
	write(511,*) 'The user prohibited u-turn on a generation link'
	write(511,*) 'link', ifrom,'   -->',ito
	write(511,*) 'The u-turn has been internally allowed'
	write(511,*)
				endif
			enddo
		enddo
	endif

!*********************
	else
! No physical U turn exists

!line below was comma out by hayssam on sep 4 2003
!	if(jfind.eq.0.or.iuturn_flag.eq.1) then

!line below was added by hayssam on sep 4 2003
	if((jfind.eq.0).and.(iuturn_flag.eq.1)) then
	UturnFlag(mlink)=0
	write(511,*)
	write(511,*) 'A u-turn has been specified on a link where'
	write(511,*) 'a u-turn does not physically exist'
      write(511,*) 'Link',ifrom,  '   -->',ito
	write(511,*) 'The u-turn has been internally prohibited'
	write(511,*)
	endif
	
	endif

      
c --
c -- Check for input errors
c --
         if(mlink.eq.0) then 
          write(911,*) 'INPUT ERROR : movement data file'
          write(911,*) 'check the upstream and down stream' 
          write(911,*) 'nodes for line number',i
	    write(911,*) iunod(i),idnod(i)
          stop
         endif
c --
c -- Check for input errors
c --
         if(mleft.eq.0.and.ileft.ne.0) then 
          write(911,*) 'INPUT ERROR : movement data file'
          write(911,*) 'check the left turning movement' 
          write(911,*) 'for line number',i
          write(911,*) mleft,ileft
		stop
         endif
c --
c --
c --
c --
c -- Check for input errors
c --
         if(mst.eq.0.and.ist.ne.0) then 
          write(911,*) 'INPUT ERROR : movement data file'
          write(911,*) 'check the through movement' 
          write(911,*) 'for line number',i
          write(911,*) mst,ist
		stop
         endif
c --c --
c --
c --
c -- Check for input errors
c --
         if(mright.eq.0.and.iright.ne.0) then 
          write(911,*) 'INPUT ERROR : movement data file'
          write(911,*) 'check the right turning movement' 
          write(911,*) 'for line number',i
          write(911,*) mright,iright
		stop
         endif
c --
c --
c --
c --
c -- Check for input errors
c --
         if(mother1.eq.0.and.iother1.ne.0) then 
          write(911,*) 'INPUT ERROR : movement data file'
          write(911,*) 'check the other movement' 
          write(911,*) 'for line number',i
	    write(911,*) ifrom,ito, mother1,iother1 
          stop
         endif
c --
c --
c --
c -- Check for input errors
c --
         if(mother2.eq.0.and.iother2.ne.0) then 
          write(911,*) 'INPUT ERROR : movement data file'
          write(911,*) 'check the other_i1 movement' 
          write(911,*) 'for line number',i
	    write(911,*) ifrom,ito, mother2, iother2
          stop
         endif
c --
c --
c --
c -- define the movement numebr
c -- left turn =1
c -- straight movement =2
c -- right movement =3
c -- other movement1 =4
c -- other movement2 =5
c -- U-turn =6
c -- other movement3 =7
c -- other movement3 =8
c --
              j=mlink
              do k=1,llink(j,nu_mv+1)
		
		        if(llink(j,k).eq.mleft) then
			     move(j,k)=1

! Added by Xuesong and Jason August 28 2003 DYNASMART-P 0.930.7
! To take into account prevented movements defined in movement.dat

!           MVPF = MoveNoForLink(j,llink(j,k))
!           GeoPreventFor(llink(j,k),MVPF) = 0 ! allowed
             
		  GeoPreventFor(j,k) = 0 ! allowed
            MVPB = MoveNoBackLink(j,llink(j,k))
            GeoPreventBack(ForToBackLink(llink(j,k)),MVPB) = 0 ! allowed
                   

                elseif(llink(j,k).eq.mst) then
			     move(j,k)=2

! Added by Xuesong and Jason August 28 2003 DYNASMART-P 0.930.7
! To take into account prevented movements defined in movement.dat
!           MVPF = MoveNoForLink(j,llink(j,k))
!           GeoPreventFor(llink(j,k),MVPF) = 0 ! allowed
             
		  GeoPreventFor(j,k) = 0 ! allowed
            MVPB = MoveNoBackLink(j,llink(j,k))
            GeoPreventBack(ForToBackLink(llink(j,k)),MVPB) = 0 ! allowed
                   


                elseif(llink(j,k).eq.mright) then
			     move(j,k)=3

! Added by Xuesong and Jason August 28 2003 DYNASMART-P 0.930.7
! To take into account prevented movements defined in movement.dat
!           MVPF = MoveNoForLink(j,llink(j,k))
!           GeoPreventFor(llink(j,k),MVPF) = 0 ! allowed
             
		  GeoPreventFor(j,k) = 0 ! allowed
            MVPB = MoveNoBackLink(j,llink(j,k))
            GeoPreventBack(ForToBackLink(llink(j,k)),MVPB) = 0 ! allowed
                   


                elseif(llink(j,k).eq.mother1) then
			     move(j,k)=4

! Added by Xuesong and Jason August 28 2003 DYNASMART-P 0.930.7
! To take into account prevented movements defined in movement.dat
!           MVPF = MoveNoForLink(j,llink(j,k))
!           GeoPreventFor(llink(j,k),MVPF) = 0 ! allowed
             
		  GeoPreventFor(j,k) = 0 ! allowed
            MVPB = MoveNoBackLink(j,llink(j,k))
            GeoPreventBack(ForToBackLink(llink(j,k)),MVPB) = 0 ! allowed
                   


                elseif(llink(j,k).eq.mother2) then
			     move(j,k)=5

! Added by Xuesong and Jason August 28 2003 DYNASMART-P 0.930.7
! To take into account prevented movements defined in movement.dat
!           MVPF = MoveNoForLink(j,llink(j,k))
!           GeoPreventFor(llink(j,k),MVPF) = 0 ! allowed
             
		  GeoPreventFor(j,k) = 0 ! allowed
            MVPB = MoveNoBackLink(j,llink(j,k))
            GeoPreventBack(ForToBackLink(llink(j,k)),MVPB) = 0 ! allowed
                   



!                elseif(llink(j,k).eq.muturn) then
!			     move(j,k)=6
! Added by Hayssam and Xuesong Aug 5 0.930.7
      elseif(llink(j,k).eq.muturn.and.UturnFlag(j).eq.1) then
			     move(j,k)=6

! Added by Xuesong and Jason August 28 2003 DYNASMART-P 0.930.7
! To take into account prevented movements defined in movement.dat
!           MVPF = MoveNoForLink(j,llink(j,k))
!           GeoPreventFor(llink(j,k),MVPF) = 0 ! allowed
             
		  GeoPreventFor(j,k) = 0 ! allowed
            MVPB = MoveNoBackLink(j,llink(j,k))
            GeoPreventBack(ForToBackLink(llink(j,k)),MVPB) = 0 ! allowed
                   



	          elseif(llink(j,k).eq.mother3) then
			     move(j,k)=7

! Added by Xuesong and Jason August 28 2003 DYNASMART-P 0.930.7
! To take into account prevented movements defined in movement.dat
!           MVPF = MoveNoForLink(j,llink(j,k))
!           GeoPreventFor(llink(j,k),MVPF) = 0 ! allowed
             
		  GeoPreventFor(j,k) = 0 ! allowed
            MVPB = MoveNoBackLink(j,llink(j,k))
            GeoPreventBack(ForToBackLink(llink(j,k)),MVPB) = 0 ! allowed
                   

	          elseif(llink(j,k).eq.mother4) then
			     move(j,k)=8

! Added by Xuesong and Jason August 28 2003 DYNASMART-P 0.930.7
! To take into account prevented movements defined in movement.dat
!           MVPF = MoveNoForLink(j,llink(j,k))
!           GeoPreventFor(llink(j,k),MVPF) = 0 ! allowed
             
		  GeoPreventFor(j,k) = 0 ! allowed
            MVPB = MoveNoBackLink(j,llink(j,k))
            GeoPreventBack(ForToBackLink(llink(j,k)),MVPB) = 0 ! allowed
                   


			  endif
c --  based on the setting in movement.dat, if a movement is prohibited, 
c --  update the GeopreventFor(forward*)



             enddo

c --
            if(mleft.gt.0) then
             j=mleft
              do k=1,inlink(j,nu_mv+1)
                if(inlink(j,k).eq.mlink) then
                  movein(j,k)=1
	            exit
                endif
              end do
            endif
c --
c --
c --
            if(mst.gt.0) then
              j=mst
              do k=1,inlink(j,nu_mv+1)
                if(inlink(j,k).eq.mlink) then
                   movein(j,k)=2
	             exit
                endif
              end do
            endif
c --
c --
c --
            if(mright.gt.0) then    
              j=mright
              do k=1,inlink(j,nu_mv+1)
                if(inlink(j,k).eq.mlink) then
                   movein(j,k)=3
                   exit
                endif
              end do
           endif
c --
c --
c --
           if(mother1.gt.0) then
              j=mother1
              do k=1,inlink(j,nu_mv+1)
                if(inlink(j,k).eq.mlink) then
                  movein(j,k)=4
                  exit
                endif
              enddo
           endif
c --
c --
c --
           if(mother2.gt.0) then
              j=mother2
              do k=1,inlink(j,nu_mv+1)
                if(inlink(j,k).eq.mlink) then
                  movein(j,k)=5
                  exit
                endif
              enddo
           endif
c --
c -- 
c --
            if(muturn.gt.0) then
              j=muturn
              do k=1,inlink(j,nu_mv+1)
                if(inlink(j,k).eq.mlink) then
                  movein(j,k)=6
                  exit
                endif
              end do
            endif
c --
c --
c --
           if(mother3.gt.0) then
              j=mother3
              do k=1,inlink(j,nu_mv+1)
                if(inlink(j,k).eq.mlink) then
                  movein(j,k)=7
                  exit
                endif
              enddo
           endif
c --
c --
c --
           if(mother4.gt.0) then
              j=mother4
              do k=1,inlink(j,nu_mv+1)
                if(inlink(j,k).eq.mlink) then
                  movein(j,k)=8
                  exit
                endif
              enddo
           endif
c --

100   continue



!     Added by MTI team Jan 21 2004
      do i=1,noofarcs

	 if(link_iden(i).eq.100) then ! only for origin connectors
	  do j=1,llink(i,nu_mv+1)
          move(i,j) = 2 ! Through
        end do
	 endif
	enddo


c --
c -- end read movement data
c --
c --
c -- start read the left-turn capacity
c --  
       if(EOF(48)) then
	  ErString = "leftcap.dat"
	  call ErReadEOF(ErString)
	 endif 
      read(48,*,iostat=error)
      do 400 k=1,5
        read(48,311,iostat=error) fgcratio
311    format(4x,f3.1)
        if(fgcratio.eq.0.3) igc=1
        if(fgcratio.eq.0.4) igc=2
        if(fgcratio.eq.0.5) igc=3
        if(fgcratio.eq.0.6) igc=4
        if(fgcratio.eq.0.7) igc=5
      do i=1,3
        read(48,262,iostat=error) itmp,(leftcapWb(igc,itmp,j),j=1,7)
      enddo
262    format(i1,3x,7i5)
400   continue
      read(48,*,iostat=error)
      do i=1,5
        read(48,311,iostat=error) fgcratio
        if(fgcratio.eq.0.3) igc=1
        if(fgcratio.eq.0.4) igc=2
        if(fgcratio.eq.0.5) igc=3
        if(fgcratio.eq.0.6) igc=4
        if(fgcratio.eq.0.7) igc=5
! Modified by Xuesong Feb 08 2004 0.930.9
!	  do k=1,ifix((fgcratio)*10)*3
	  irows=ifix((fgcratio+0.001)*10)*3
	  do k=1,irows
        read(48,313,iostat=error) ivolume,itmp,
     *      (leftcapWOb(igc,itmp,ivolume,j),j=1,7)
	 
        enddo


! Added by MTI Feb 13 2004 0.930.9
! Set the default values for iivolume> ivolume
	  do iivolume= ivolume+1, 7
	   do iu=1,3
          leftcapWOb(igc,iu,iivolume,:) = leftcapWOb(igc,iu,ivolume,:) 
	   enddo
	 enddo


!313    format(i1,i4,i4,6i5)
       !Modified by Hayssam Sbayti on June 2, 2003 to have a uniform field length
313    format(i1,i4,7i5)
      enddo
c --
c -- end of read left turn capacity
c --
c -- start read 4 way stop sign capacity
      do i = 1, NLevel
       if(EOF(56)) then
	  ErString = "StopCap4Way.dat"
	  call ErReadEOF(ErString)
	 endif
       read(56,5656,iostat=error) (stopcap4w(i,j),j=1,NMove)
      enddo
5656  format(12x,5i7) 
c --  end of reading 4 way stop sign capacity

c -- start read 2 way stop sign capacity

      do i = 1, Level2N
       if(EOF(57)) then
	  ErString = "StopCap2Way.dat"
	  call ErReadEOF(ErString)
	 endif
       read(57,*,iostat=error) stopcap2wIND(i),
     *     (stopcap2w(i,j),j=1,Move2N)
      enddo
5657  format(4i5) 
c --  end of reading 2 way stop sign capacity


c -- start read Yield Sign capacity
       ! modified by hayssam april 24 2004 for DYNA 1.0
      !do i = 1, Level2N
	do i = 1, YLevel2N
       if(EOF(60)) then
	  ErString = "YieldCap.dat"
	  call ErReadEOF(ErString)
	 endif
       ! modified by hayssam april 24 2004 for DYNA 1.0
	 !read(60,*,iostat=error) YieldCapIND(i),(YieldCap(i,j),j=1,Move2N)
	read(60,*,iostat=error) YieldCapIND(i),(YieldCap(i,j),j=1,YMove2N)
      enddo

c --  end of reading 2 way stop sign capacity








c -- read the vms data
c --
c -- vms_num : number of vms
c --
c -- i1 and i2 are the upstream and downstream nodes for the link
c -- on which the VMS exists.
c --
  
C-- Add by Zihan and Archak, 20160325, for reversible lane    

		if(vms_num_reversible.gt.0) then  
            do i=vms_num+1,vms_num+vms_num_reversible
                read(499,*,iostat=error) vmstype(i),i1,i2,vms(i,2),
     *      vms(i,3),vms_start(i),vms_end(i)

	          read(499,*,iostat=error)
     *          (vmstypetwopath(i,k)%node,k=1,vms(i,3))
	           
	           if(vmstypetwopath(i,1)%node.ne.i2) then
		             write(911,*) 'error in',i,'th  VMS'
		             write(911,*) 'error in Type 2 VMS subpath specification'
		             stop
	           endif
	           
	           vms(i,2) = 100.0
	           do mmp = 1, vms(i,3)-1
		         vmstypetwopath(i,mmp)%link=
     *		 GetFLinkFromNode(idnum(vmstypetwopath(i,mmp)%node),
     *                      idnum(vmstypetwopath(i,mmp+1)%node))     
	           enddo
            enddo
        end if         
 
C-- end of addition

  
      if(vms_num.gt.0) then    
      
	do i=1,vms_num
!Modified by MTI on March 20 for DYNA 1.0 to read Type 4 optional detour VMS
!      read(49,*,iostat=error) vmstype(i),i1,i2,vms(i,2),vms(i,3),
!     + vms_start(i),vms_end(i)

      read(49,*,iostat=error) vmstype(i),i1,i2,vms(i,2),vms(i,3),
     + vms_start(i),vms_end(i)


! read subpath for type 2 
!      if(vmstype(i).eq.2) then
!	   read(49,*,iostat=error) (vmstypetwopath(i,k)%node,k=1,vms(i,3))      
!        if(vmstypetwopath(i,1)%node.ne.i2) then
!         write(911,*) 'error in',i,'th  VMS'
!         write(911,*) 'error in Type 2 VMS subpath specification'
!         stop
!        endif
!        vms(i,2) = 100.0
!        do mmp = 1, vms(i,3)-1
!          vmstypetwopath(i,mmp)%link=
! !  *     GetFLinkFromNode(vmstypetwopath(i,mmp)%node,
! !  *                      vmstypetwopath(i,mmp+1)%node)

! modified by hayssam on nov 12 2003 for DYNA-P 930.8     
! to convert to internal node numbers

!    *     GetFLinkFromNode(idnum(vmstypetwopath(i,mmp)%node),
!    *                      idnum(vmstypetwopath(i,mmp+1)%node))
    
!        enddo
!      endif  

!*******************************************************
! Modified by MTI for March 20 2004, DYNA 1.0
! need to read subpath for vms types 2 and 4
!     read subpath for types 2 and 4
      if(vmstype(i).eq.2. or. vmstype(i).eq.4) then
	   read(49,*,iostat=error) (vmstypetwopath(i,k)%node,k=1,vms(i,3))
	   if(vmstypetwopath(i,1)%node.ne.i2) then
           write(911,*) 'error in',i,'th  VMS'
           write(911,*) 'error in Type 2 VMS subpath specification'
           stop
	   endif
         vms(i,2) = 100.0
         do mmp = 1, vms(i,3)-1
           vmstypetwopath(i,mmp)%link=
     *     GetFLinkFromNode(idnum(vmstypetwopath(i,mmp)%node),
     *                      idnum(vmstypetwopath(i,mmp+1)%node))     
         enddo
      endif  

! read the detoured (closed down) links for type 4
!      if(vmstype(i).eq.4) then
!	   read(49,*,iostat=error) 
!    +	 vms(i,4),(Detoured_Nodes(i,k),k=1,2*vms(i,4)) 
!     !vms(i,4) = number of detoured links
!	   if(Detoured_Nodes(i,1).ne.i2) then
!          write(911,*) 'error in',i,'th  VMS'
!          write(911,*) 'error in Type 4 VMS subpath specification'
!          stop
!	   endif
!      vms(i,2) = 100.0 ! response rate
 
!     endif 
!*********************************************

c --  the kth path specified in vms.dat for type 3 vms
c --  needs to be smaller or equal to the kay as specified in network.dat
	 if(vmstype(i).eq.3) then
	  if(vms(i,2).gt.100.or.vms(i,2).lt.0) then
	   write(911,*) "Error in vms.dat type 3"
	   write(911,*) "Check vms(i,2) for response percentage",i
	   stop
	  endif
	  if(vms(i,3).ne.0.and.vms(i,3).ne.1) then
	   write(911,*) "Error in vms.dat type 3"
         write(911,*) "Diversion Mode shoule be either 1 or 0"
	   stop
	  endif
	 endif
!	 if(vmstype(i).eq.2) then
!         if(vms(i,2).gt.kay.or.vms(i,2).lt.0) then
!           write(911,*) "Error in vms.dat type 2"
!           write(911,*) "Need to specify a valid path number"
!	     write(911,*) "The vaild path number is > 0 and < kay"
!	     stop
!	   endif
!	 endif
	 !Added for weather VMS, June 8 2009
	 if(vmstype(i).eq.6) then
         if(vms(i,2).lt.0) then
	     write(911,*) "Error in vms.dat type 6"
	     write(911,*) "Check vms(i,2) for weather VMS type",i
	     stop
	   endif
	   i_risk = 1
       endif
       
       if(vmstype(i).eq.7) then
         if(num_vsl_table.lt.1) then
	     write(911,*) "Error in vms.dat type 7"
	     write(911,*) "Please specify look-up table in vsl.dat"
	     stop
	   endif      
         
         if(vms(i,3).gt.num_vsl_table) then
	     write(911,*) "Error in vms.dat type 7"
	     write(911,*) "Check vms(i,2) for look-up table number",i
	     stop
	   endif      
       endif      
       ! End of June 8 2009
       
      vms(i,1)=GetFLinkFromNode(idnum(i1),idnum(i2))

!       do k=1,noofarcs
!         if(iunod(k).eq.idnum(i1).and.idnod(k).eq.idnum(i2)) then
!	    vms(i,1)=k !G
!	    exit
!	   endif
!       end do


       if(vms(i,1).eq.0) then 
         write(911,*) 'INPUT ERROR : VMS data file'
         write(911,*) 'check the VMS link' 
         write(911,*) 'for VMS number',i
         stop
       endif



c --
c --
        enddo   
      
	endif ! vms_num.gt.0

c --
c --  end of reading VMS data
c --
	! Added for weather VMS, June 8 2009
c -- read VSL look-up table
      if(num_vsl_table.gt.0) then
        do i = 1, num_vsl_table
          read(449,*,iostat=error) table_num,vsl_num_line(i)
          do j = 1, vsl_num_line(i)
            if(vsl_num_line(i).gt.10) then
              write(911,*) 'INPUT ERROR: VSL look-up table'
              write(911,*) '# of lines for one table exceeds 10' 
              stop
            endif
            read(449,*,iostat=error) vsl_v_u(i,j),vsl_v_l(i,j),
     +       vsl_r_l(i,j),vsl_r_u(i,j),vsl_s_l(i,j),vsl_s_u(i,j),
     +       vsl_speed(i,j)
          enddo
        enddo
      endif

c -- read bus data
c --
c --nubus : number of buses in the network.
c -- i1 and i2 are the upstream and downstream nodes for the starting link.
c --
c -- in this section, we read the bus attributes and path one by one
c -- and insert them into BusAtt_Array by calling BusAtt_Insert subroutine
      if(nubus.gt.0) then
	  tlatest_bus=0.0
        do i=1,nubus
      read(50,*,iostat=error) i1,i2,busstart(i),busdwell(i),NoBusNode(i)
          read(50,*,iostat=error) (buspathtmp(k),k=1,NoBusNode(i))
          read(50,*,iostat=error) (busstoptmp(k),k=1,NoBusNode(i))
          do kk = 1, NoBusNode(i)
!            call BusAtt_Insert(i,kk,1,idnumbuspathtmp(kk))
! Modified by Xuesong June 06 2003 to convert the input node numbers to internal node numbers            
			internalnodenumber = idnum(buspathtmp(kk))
         if(internalnodenumber.eq.0) then 
          write(911,*) 'INPUT ERROR : bus data file'
          write(911,*) 'node',buspathtmp(kk), ' doesnot exist'
          write(911,*) 'for bus number',i
          stop
         endif

		  call BusAtt_Insert(i,kk,1,internalnodenumber)

            call BusAtt_Insert(i,kk,2,busstoptmp(kk))
	    enddo
		
      do kk = 2, NoBusNode(i)
	LinkNo=GetFLinkFromNode(idnum(buspathtmp(kk-1)),
     +idnum(buspathtmp(kk)))
	
	if(LinkNo.eq.0) then
          write(911,*) 'INPUT ERROR : bus data file'
          write(911,*) 'link ',buspathtmp(kk-1), '->',buspathtmp(kk),
     +	 ' doesnot exist'
          write(911,*) ' for bus number',i
          stop
	endif
	enddo

		iflag1 = 0
	    ifinalnode = buspathtmp(NoBusNode(i))

!*********************************************
!		ibuszone = izone(idnum(ifinalnode))

!	do j=1,NoofConsPerZone(ibuszone)
!         if(ifinalnode.eq.ConNoInZone(ibuszone,j)) iflag1 =1
!	 enddo
!       if(iflag1.ne.1) then 
!         write(911,*) 'INPUT ERROR : bus data file'
!          write(911,*) 'final node ',ifinalnode, 'for bus number',i,
!     +' is not a destination node for zone ',ibuszone
!          stop
!       endif

! Modified by Xuesong and Hayssam Nov 26 2003 0.930.8
! We only need to check if the final node is a destination.

		iflag1 = iConZone(idnum(ifinalnode),1)

       if(iflag1.eq.0) then 
          write(911,*) 'INPUT ERROR : bus data file'
          write(911,*) 'final node ',ifinalnode, 'for bus number',i,
     +' is not a destination node'
          stop
       endif

!*************************************************

		
		!          itmp=destination(MasterDest
!     +         (izone(buspathtmp(NoBusNode(i)))))
! Modified by Xuesong June 06 2003 to convert the input node numbers to internal node numbers            

!************************************************
!          itmp=destination(MasterDest
!     +         (izone(idnum(buspathtmp(NoBusNode(i))))))
! Modified by Xuesong and Hayssam Nov 26 2003 0.930.8
! We only need to know the centriod number
          itmp=destination(MasterDest(iConZone(idnum(ifinalnode),2)))

!*************************************************   
       
	   	    
			
			
			Index1D = NoBusNode(i) + 1
          call BusAtt_Insert(i,Index1D,1,itmp)
          call BusAtt_Insert(i,Index1D,2,0)

          if(busstart(i).gt.tlatest_bus) tlatest_bus=busstart(i)
          do j=1,noofarcs
            if(iunod(j).eq.idnum(i1).and.idnod(j).eq.idnum(i2)) then
               buslink(i)=j !G
	         exit
	      endif
          end do
c --
c -- Check for input errors
c --
         if(buslink(i).eq.0) then 
          write(911,*) 'INPUT ERROR : bus data file'
          write(911,*) 'check the starting link' 
          write(911,*) 'for bus number',i
          stop
         endif

        enddo ! end of read bus loop 
	 endif
150   format(20i5)
c --
c -- end of reading bus data
c --
c --
c -- read incident data
c --
c -- inici_num : number of incidents
c -- 
c -- i1 and i2 are the upstream and downstream nodes for the link on which
c -- the incident occures.
c --
c --
      if(inci_num.gt.0) then
        do i=1,inci_num
         read(46,*) i1,i2,inci(i,1),inci(i,2),inci(i,3)
         do k=1,noofarcs
          if(iunod(k).eq.idnum(i1).and.idnod(k).eq.idnum(i2)) incil(i)=k !G
         end do
c --
c -- Check for input errors
c --
         if(incil(i).eq.0) then 
          write(911,*) 'INPUT ERROR : incident data file'
          write(911,*) 'check the incident link' 
          write(911,*) 'for incident number',i
          stop
         endif
       end do
      endif
c --
c -- determine the opposing link and its number of lanes for all links.
c --
c --
      do i=1,noofarcs
        do j=1,llink(i,nu_mv+1)
           if(move(i,j).eq.2) then
              il=llink(i,j)
             do ii=1,noofarcs
               if(idnod(il).eq.iunod(ii).and.
     +            iunod(il).eq.idnod(ii)) then
                  ill=ii
	            exit
               endif
             end do
	        if(ill.gt.0.and.ill.le.noofarcs) then
                opp_linkP(i)=ill
                opp_lane(i)=nlanes(ill)
	        endif
           endif
        enddo  
      enddo   

c --
c --  start reading TraffFlowModel.dat

      do i = 1, NoOfFlowModel
       read(55,*) MG, FlowModelType(i)
	 if(FlowModelType(i).gt.2.or.FlowModelType(i).lt.1) then
	  write(911,*) "Error in Traffic Flow Model Type"
	  write(911,*) "Currenlty, only two types are supported"  
	  write(911,*) "Please see user's manual for details"
	  stop
	 endif

	 Select Case (FlowModelType(i))
	  Case (1)
	    read(55,*) MGreenS(i)%Kcut, MGreenS(i)%Vf2,MGreenS(i)%V02,
     *            MGreenS(i)%Kjam2,MGreenS(i)%alpha2
	  Case (2)
	    read(55,*) MT1, MT2, MGreenS(i)%V02,
     *               MGreenS(i)%Kjam2,MGreenS(i)%alpha2

          MGreenS(i)%Kcut = 0	    
	 End Select 
	enddo
c --  assign flow model to connectors, this default model is to make vehicle take infimisial time to centroid
      

      FlowModelType(NoOfFlowModel+1) = 1
      MGreenS(NoOfFlowModel+1)%Kcut = 300
	MGreenS(NoOfFlowModel+1)%Vf2 = 100
	MGreenS(NoOfFlowModel+1)%V02 = 100
	MGreenS(NoOfFlowModel+1)%Kjam2 = 300
	MGreenS(NoOfFlowModel+1)%alpha2 = 1	      

c --
c -- Set the initial values for the link performance (speed, density, ...) 
c --
      do 76 i=1,noofarcs

         xl(i)=nlanes(i)*s(I)
! Added by MTI team Feb 04 2004 0.930.9
	   original_xl(i) = xl(i)

         v(i)=(SpeedLimit(i)+Vfadjust(i))/60.0
         statmpt(i)=s(i)/v(i)
         TTimeOfBackLink(ForToBackLink(i))=statmpt(i)

         IH = FlowModelnum(i)
         cmax(i) = MGreenS(IH)%Kjam2
! Added for VSL, June 8 2009
         SpeedLimit_org(i) = SpeedLimit(i)! Store the original speed limit
76    continue    
c --

       call read_signals()

       
c --
c --   read the pricing scenario
          read(51,*)
     +    price_regular_c, price_hot_lov_c,
     +    price_hot_hov_c, time_value

! Dec 6 2005 by Jason not allow negative tolls in 1.2
      if(price_regular_c.lt.0.0.or.price_hot_lov_c.lt.0.0.or.
     *   price_hot_hov_c.lt.0.0.or.time_value.lt.0.0) then
        write(911,*) 'Error in reading pricing.dat'
	  write(911,*) 'Negative toll cost or value of time' 
	  write(911,*) 'is not allowed in this version.'
        stop
	endif
! End Dec 6 2005

          read(51,*) fwy_bias, num_of_tolllinks
! added frw_bias by Hayssam and Jason Sep 23 2005
! for freeway bias implementation
	if(fwy_bias.gt.1.0 .or. fwy_bias.lt.0.0) then
	  write(911,*) 'Error in reading pricing.dat'
	  write(911,*) 'Freeway bias is greater than 1 or less than zero'
	  write(911,*) 'Freeway bias must be between  0 and 1'
	  stop
	endif

! Added by MTI team March 31 2004 1.0
      if(time_value.eq.0) then
	  time_value = 1  ! give an initial value anyway
	if(price_hot_lov_c.gt.0 .or. price_hot_hov_c.gt.0) then
	  write(911,*) "Error in Pricing.dat"
	  write(911,*) "The time value cannot be zero!"  
	  stop
	endif
	endif
! End

c -- convert the cost into time values
       
C       price_regular = price_regular_c / time_value  
C       price_hot_lov = price_hot_lov_c / time_value  
C       price_hot_hov = price_hot_hov_c / time_value  

c	Modified by Hayssam and Xuesong June 1 2003       
	 price_regular = price_regular_c*60 / time_value  
       price_hot_lov = price_hot_lov_c*60 / time_value  
       price_hot_hov = price_hot_hov_c*60 / time_value  
c --
c --   end reading the pricing scenario         
c --

!Added May 23 2005 TD link toll
!     read(51,*) num_of_tolllinks
      if(num_of_tolllinks.gt.0) then
	  iAdvance_pricing = 1
	else
	  iAdvance_pricing = 0
	endif

	if(iAdvance_pricing.eq.1) then        
	  if(num_of_tolllinks.gt.0) then
	    if(.not.allocated(LinkTolls)) then
            allocate(LinkTolls(num_of_tolllinks + 1),stat=error)
	      if(error.ne.0) then
	       write(911,*) 'allocate LinkTolls error-insufficient memory'
	       stop
	      endif
	    endif
	    LinkTolls(:)%FNode = 0
	    LinkTolls(:)%TNode = 0
	    LinkTolls(:)%num_tolls_per_link = 0

! Jason: need to add deallocate(LinkTolls) later! VOT pricing Feb 3 2005

          do ilt=1, num_of_tolllinks
            read(51,*) ilinktoll, FNodetmp, TNodetmp, num_tolls
	      if(num_tolls.gt.0.and.num_tolls.le.24) then
	        
			LinkTolls(ilt)%num_tolls_per_link = num_tolls
	        ! convert external to internal node numbers
	        LinkTolls(ilt)%FNode = idnum(FNodeTmp) 
              LinkTolls(ilt)%TNode = idnum(TNodetmp) 
              
			LinkTolls(ilt)%Tolls(:)%ST = 0.0
			LinkTolls(ilt)%Tolls(:)%ET = 0.0
			LinkTolls(ilt)%Tolls(:)%tolltype = 0
			LinkTolls(ilt)%Tolls(:)%tollcost = 0.0						  
	        
			do it=1, LinkTolls(ilt)%num_tolls_per_link
                read(51,*) rt1, rt2, it3, rt4
! Added Oct 8 2005 
      if(rt1.gt.rt2) then
	  write(911,*) 'End time of a link toll is earlier than its 
     +start time'
	  write(911,*) 'Please check link:', FNodetmp, TNodetmp
	  stop
	endif

! Added Dec 6 2005
      if(rt4.lt.0.0) then
        write(911,*) 'Error in reading pricing.dat'
	  write(911,*) 'Negative toll cost is not allowed.' 
        stop
	endif
	
     

! Added May 30 2006
      iforlink = GetFLinkFromNode(idnum(FNodeTmp), idnum(TNodetmp))
      
      


           	  LinkTolls(ilt)%Tolls(it)%ST       = rt1
                LinkTolls(ilt)%Tolls(it)%ET       = rt2
			  LinkTolls(ilt)%Tolls(it)%tolltype = it3
                LinkTolls(ilt)%Tolls(it)%tollcost = rt4
	        enddo

! Oct 12 2005 check for overlapping of toll periods on the same toll link
	        do i1 = 1, LinkTolls(ilt)%num_tolls_per_link	       
			  do i2 = i1+1, LinkTolls(ilt)%num_tolls_per_link

! Feb 25 2010: HOV & HOT Toll
 ! [ST,ET) when we read the time intervals
	if((LinkTolls(ilt)%Tolls(i1)%ST.ge.LinkTolls(ilt)%Tolls(i2)%ST
     *.and.LinkTolls(ilt)%Tolls(i1)%ST.lt.LinkTolls(ilt)%Tolls(i2)%ET
     *.and.NOT(LinkTolls(ilt)%Tolls(i1)%tolltype.eq.1
     *.AND.LinkTolls(ilt)%Tolls(i2)%tolltype.eq.3)
     *.and.NOT(LinkTolls(ilt)%Tolls(i1)%tolltype.eq.3
     *.AND.LinkTolls(ilt)%Tolls(i2)%tolltype.eq.1)
     *.and.NOT(LinkTolls(ilt)%Tolls(i1)%tolltype.eq.2
     *.AND.LinkTolls(ilt)%Tolls(i2)%tolltype.eq.4)
     *.and.NOT(LinkTolls(ilt)%Tolls(i1)%tolltype.eq.4
     *.AND.LinkTolls(ilt)%Tolls(i2)%tolltype.eq.2))
     *.or.(LinkTolls(ilt)%Tolls(i2)%ST.ge.LinkTolls(ilt)%Tolls(i1)%ST
     *.and.LinkTolls(ilt)%Tolls(i2)%ST.lt.LinkTolls(ilt)%Tolls(i1)%ET
     *.and.NOT(LinkTolls(ilt)%Tolls(i1)%tolltype.eq.1
     *.AND.LinkTolls(ilt)%Tolls(i2)%tolltype.eq.3)
     *.and.NOT(LinkTolls(ilt)%Tolls(i1)%tolltype.eq.3
     *.AND.LinkTolls(ilt)%Tolls(i2)%tolltype.eq.1)
     *.and.NOT(LinkTolls(ilt)%Tolls(i1)%tolltype.eq.2
     *.AND.LinkTolls(ilt)%Tolls(i2)%tolltype.eq.4)
     *.and.NOT(LinkTolls(ilt)%Tolls(i1)%tolltype.eq.4
     *.AND.LinkTolls(ilt)%Tolls(i2)%tolltype.eq.2))) then

     
     
	  write(911,*) 'Overlapping toll periods are found in link'
!	  write(911,*) LinkTolls(ilt)%FNode, '->', LinkTolls(ilt)%TNode
	  write(911,22310) nodenum(LinkTolls(ilt)%FNode), 
     +	  nodenum(LinkTolls(ilt)%TNode)
22310 format(I8,'->', I8)    
	  
        stop
      endif
			  enddo
			enddo
! End
            else
	        write(511,*) 'input error: array bound exceeds in pricing'
		  endif                 
	    enddo           
	  endif
	endif
	     
                        

! End May 23 2005 TD link toll

!********************************************************************
!Added by Alex on 20160404 for snow accumulation
      if (lexist_SnowAccu) then 
          allocate(SAFactor(SAFactorNum,3),stat=error)
          if(error.ne.0) then 
	      write(911,*) 'allocate SAFactor error - insufficient memory'
	      stop
	    endif
    	
	    allocate(AccuStime(noofarcs),stat=error)
          if(error.ne.0) then 
	      write(911,*) 'allocate AccuStime error - insufficient memory'
	      stop
	    endif
	    AccuStime(:)=0
    	
	    allocate(SnowDepth(noofarcs,10),stat=error)
          if(error.ne.0) then 
	      write(911,*) 'allocate SnowDepth error - insufficient memory'
	      stop
	    endif
	    SnowDepth(:,:)=0
    	
	    allocate(speedreduction(noofarcs),stat=error)
	    if (error.ne.0) then 
	      write(911,*) 'allocate speedreduction - insufficient memory'
	      stop
	    endif
	    speedreduction(:)=1
    	
	    allocate(capacityreduction(noofarcs),stat=error)
	    if (error.ne.0) then 
	      write(911,*) 'allocate capacityreduction - insufficient memory'
	      stop
	    endif
	    !Add by alex for snow plow routing 
	    inquire(file='SnowplowRoutes.dat',exist=lexist_SnowPlow)
          if(lexist_SnowPlow) then 
	          read(612,*) noofsnowplows,longestroute
	          read(613,*) noofsnowplowinterval,nooflsininterval 
	          !total number of simulation intervals and number of link served in one interval
	    endif
	    allocate(sp_routes(noofsnowplows,longestroute),stat=error)
	    if (error.ne.0) then 
	      write(911,*) 'allocate sp_routes - insufficient memory'
	      stop
	    endif
	    
	    if (lexist_SnowPlow) then 
	          do i=1,noofsnowplows
	              !read(612,*) sp_routes(i,:) 
	          enddo
	    endif
	    
	    allocate(LinkTobeServed(noofsnowplowinterval,
     *	   nooflsininterval),stat=error)
          if(error.ne.0) then 
            write(911,*)'allocate link to be served-insufficient memory'
            stop
          endif
          
          allocate(LinkServedTime(noofarcs,10),stat=error)
          if(error.ne.0) then 
            write(911,*)'allocate link served time- insufficient memory'
            stop
          endif
          LinkServedTime(:,:)=0
	    if (lexist_SnowPlow) then 
	          do i=1,noofsnowplowinterval
	              read(613,*) LinkTobeServed(i,:) 
	          enddo
	    endif
	    capacityreduction(:)=1
          do i=1, SAFactorNum
            read(611,*) SAFactor(i,:)
          enddo
      endif
    !End snow accumulation
      
    !Added Feb 10 2009 Weather Impact
      if(i_weather.eq.1) then
    ! Start reading WAF.dat
        if(.not.allocated(WAF)) then
            allocate(WAF(18,6),stat=error)
	      if(error.ne.0) then
	        write(911,*) 'allocate WAF error-insufficient memory'
	        stop
	      endif
	  endif
	  do i = 1,18
	    read(400,*) index,(WAF(i,j),j=1,6)
	    if(i.ne.index) then
	      write(911,*) 'read WAF.dat error!'
	      stop
	    endif
	  enddo
	  ! Start reading weather.dat
	  read(401,*) network_weather
	  
!Weather June 1 2011	  
!	  if(network_weather.gt.0) then ! across-the-board weather condition is specified
!	      read(401,*) network_visibility,network_rain,network_snow,
!     +        network_st,network_et

        ! first allocate the weather related arrays
      IF (network_weather.eq.1) then
      ! for single period network wide weather effects <-- SAME as OLD weather.dat
          read (401,*) network_visibility_scalar,network_rain_scalar,
     +           network_snow_scalar,network_st_scalar,network_et_scalar
      ELSE IF (network_weather.ge.2) then
      ! for multiple time dependent network wide weather effects  
          if(.not.allocated(network_visibility)) then
            allocate (network_visibility(network_weather),
     +                 stat=error)
            if(error.ne.0) then
	        write(911,*) 'allocate weather.dat error-insufficient memory'
	        stop
	      endif
	    endif
	    
	    if(.not.allocated(network_rain)) then
            allocate (network_rain(network_weather),
     +                 stat=error)
            if(error.ne.0) then
	        write(911,*) 'allocate weather.dat error-insufficient memory'
	        stop
	      endif
	    endif
	    
	    if(.not.allocated(network_snow)) then
            allocate (network_snow(network_weather),
     +                 stat=error)
            if(error.ne.0) then
	        write(911,*) 'allocate weather.dat error-insufficient memory'
	        stop
	      endif
	    endif

	    if(.not.allocated(network_st)) then
            allocate (network_st(network_weather),
     +                 stat=error)
            if(error.ne.0) then
	        write(911,*) 'allocate weather.dat error-insufficient memory'
	        stop
	      endif
	    endif

	    if(.not.allocated(network_et)) then
            allocate (network_et(network_weather),
     +                 stat=error)
            if(error.ne.0) then
	        write(911,*) 'allocate weather.dat error-insufficient memory'
	        stop
	      endif
	    endif

         !start to read network weather arrays
         do y=1,network_weather
            read(401,*) network_visibility(y),network_rain(y),
     +       network_snow(y),network_st(y),network_et(y)
         enddo
	  ENDIF !if(network_weather)
!end Weather June 1 2011
	  read(401,*) num_of_weatherlinks
	  if(num_of_weatherlinks.lt.1.and.network_weather.eq.0) then
	      i_weather = 0
	  endif
	  if(num_of_weatherlinks.ge.1) then
	    if(.not.allocated(LinkWeathers)) then
            allocate(LinkWeathers(num_of_weatherlinks + 1),stat=error)
	      if(error.ne.0) then
	        write(911,*) 'allocate LinkWeathers error-insufficient memory'
	        stop
	      endif
	    endif
	    LinkWeathers(:)%FNode = 0
	    LinkWeathers(:)%TNode = 0
	    LinkWeathers(:)%num_weather_per_link = 0

          do ilt=1, num_of_weatherlinks
            read(401,*) ilinkweather, FNodetmp, TNodetmp, num_weathers
	      if(num_weathers.gt.0.and.num_weathers.le.24) then
	        LinkWeathers(ilt)%num_weather_per_link = num_weathers
	        ! convert external to internal node numbers
	        LinkWeathers(ilt)%FNode = idnum(FNodeTmp) 
              LinkWeathers(ilt)%TNode = idnum(TNodetmp) 
              
			  LinkWeathers(ilt)%Weathers(:)%ST = 0.0
			  LinkWeathers(ilt)%Weathers(:)%ET = 0.0
			  LinkWeathers(ilt)%Weathers(:)%visibility = 10.0
			  LinkWeathers(ilt)%Weathers(:)%rain = 0.0
			  LinkWeathers(ilt)%Weathers(:)%snow = 0.0
	        
			  do it=1, LinkWeathers(ilt)%num_weather_per_link
                read(401,*) rt1, rt2, rt3, rt4, rt5
                if(rt1.gt.rt2) then
	            write(911,*) 'End time of a link weather is earlier than  
     +            its start time'
	            write(911,*) 'Please check link:', FNodetmp, TNodetmp
	            stop
	          endif
	          
	          LinkWeathers(ilt)%Weathers(it)%ST = rt1
	          LinkWeathers(ilt)%Weathers(it)%ET = rt2
	          LinkWeathers(ilt)%Weathers(it)%visibility = rt3
	          LinkWeathers(ilt)%Weathers(it)%rain = rt4
	          LinkWeathers(ilt)%Weathers(it)%snow = rt5
	        enddo
                !check for overlapping of weather periods on the same link
	        do i1 = 1, LinkWeathers(ilt)%num_weather_per_link	       
			    do i2 = i1+1, LinkWeathers(ilt)%num_weather_per_link
 !Weather June 1 2011		
 ! [ST,ET) when we read the time intervals
!	            if((LinkWeathers(ilt)%Weathers(i1)%ST.ge.
!     *                LinkWeathers(ilt)%Weathers(i2)%ST.and.
!     *                LinkWeathers(ilt)%Weathers(i1)%ST.le.
!     *                LinkWeathers(ilt)%Weathers(i2)%ET).or.
!     *               (LinkWeathers(ilt)%Weathers(i2)%ST.ge.
!     *                LinkWeathers(ilt)%Weathers(i1)%ST.and.
!     *                LinkWeathers(ilt)%Weathers(i2)%ST.le.
!     *                LinkWeathers(ilt)%Weathers(i1)%ET)) then

	            if((LinkWeathers(ilt)%Weathers(i1)%ST.ge.
     *                LinkWeathers(ilt)%Weathers(i2)%ST.and.
     *                LinkWeathers(ilt)%Weathers(i1)%ST.lt.
     *                LinkWeathers(ilt)%Weathers(i2)%ET).or.
     *               (LinkWeathers(ilt)%Weathers(i2)%ST.ge.
     *                LinkWeathers(ilt)%Weathers(i1)%ST.and.
     *                LinkWeathers(ilt)%Weathers(i2)%ST.lt.
     *                LinkWeathers(ilt)%Weathers(i1)%ET)) then


!end Weather June 1 2011     
	                write(911,*) 'Overlapping weather periods are found'
	                write(911,*) LinkWeathers(ilt)%FNode, '->',
     *                  LinkWeathers(ilt)%TNode
                      stop
                  endif
                enddo
			  enddo          
	      endif
          enddo 	      
	  endif
      endif
!********************************************************************
!********************************************************************
c --  Starting Reading origin.dat
! --  NoofGenLinksPerZone is read from origin.dat:izlins
! --  LinkNoInZone() keeps track of the link number:izone
! --  total link length for zones are stored in TotalLinkLenPerZone():totlmz
      do i = 1, nzones
	SumLoadWeight = 0.0
	read(52,*,iostat=error) izonetmp, NoofGenLinksPerZone(i), IDGen
	If(IDGen.gt.0) LoadWeightID(i)=.True.
	if(error.ne.0) then
         write(911,*) 'Error when reading origin.dat'
	   stop
	endif

!	if(NoofGenLinksPerZone(i).eq.0) then
!         write(911,*) 'Error when reading origin.dat'
!         write(911,*) 'Each zone needs to have at least one gen link'
!         write(911,*) 'Please check zone', i
!	   stop
!	endif


	 do j = 1, NoofGenLinksPerZone(i)
         read(52,*,iostat=error) IUpNode,IDnNode, LWTmp !LWTmp is a temp var for LWTmp

	   

	   if(error.ne.0) then
           write(911,*) 'Error when reading origin.dat'
	     stop
	   endif

	   if(izone(idnum(IUpNode)).ne.i.and.izone(idnum(IDnNode)).ne.i)
     *                                                            then
!         INQUIRE(UNIT = 511, OPENED = Fexist)
!	   if(.not. Fexist) then
!	   open(file='Warning.dat',unit=511,status='unknown',iostat=error)
!	    if(error.ne.0) then
!            write(911,*) 'Error when opening Warning.dat'
!	      stop
!	    endif
!         endif

         write(511,'("Link",i7,"  -->",i7," receives demand from zone",
     *     i6," not a physical zone for both nodes")')IUpNode,IDnNode,i
         endif


	   LinkNo = GetFLinkFromNode(idnum(IUpNode),idnum(IDnNode))

! Modified by Xuesong  and Hayssam Nov 3 2003
! Remark a boundary freeway link can be a generation link
	   if(link_iden(LinkNo).eq.1) then
           write(511,'("Check origin.dat")')
	     write(511,'("link ",i5," -->",i5," is highway/freeway")') 
     *                   IUpNode,IDnNode
	     write(511,'("It cannot be a generation link in zone",i3)') i 
!	     stop
	   endif
	   if(LinkNo.lt.1) then
	      write(911,*) 'Error in origin.dat'
            write(911,*) 'Link doesnt exit'
            write(911,*) 'zone, ud, nd', i, IUpNode, IDnNode
	      stop 
	   endif
         LinkNoInZone(i,j) = LinkNo
	   if(LoadWeightID(i)) then !User-specified Loading weight is used
           SumLoadWeight = SumLoadWeight + LWTmp
	     LoadWeight(LinkNo)=LWTmp
	     TotalLinkLenPerZone(i)=TotalLinkLenPerZone(i)+
     *                            LoadWeight(LinkNo)
         else
	     TotalLinkLenPerZone(i)=TotalLinkLenPerZone(i)+xl(LinkNo)

	   endif

	 enddo
	   if(LoadWeightID(i)) then
          if(abs(SumLoadWeight-1.0).gt.0.0001) then
	     write(911,*) 'Error in origin.dat'
	     write(911,*) 'The sum of loading weights in zone',i,
     *                  'is not 1.0'
           stop
	    endif	 
	   endif
	enddo    

c --  updates the iGenZone: which super zone that link i receives demand from
      do i = 1, noofarcs
        do j = 1, nzones
	    do k = 1, NoofGenLinksPerZone(j)
            if(i.eq.LinkNoInZone(j,k)) then
              iGenZone(i,1)=iGenZone(i,1) + 1
	        iGenZone(i,iGenZone(i,1)+1)= j !iGenZone should keep the original zone number
	      endif
	    enddo
	  enddo
	enddo

c --  Check if iGenZone is correct

      do i = 1, noofarcs
	if(iGenZone(i,1).gt.0.and.idnod(i).gt.noofnodes_org) then
         write(911,*) 'errors: iGenZone contains connectors'
      endif
	enddo
!********************************************************************
!********************************************************************

c --
c --
c --  Read the optional output information
c -- 

      read(101,*) i30,i30_t
      read(101,*) i31,i31_t
      read(101,*) i32,i32_t
      read(101,*) i33,i33_t
      read(101,*) i34,i34_t
      read(101,*) i35,i35_t
      read(101,*) i36,i36_t
      read(101,*) i37,i37_t
      read(101,*) i38,i38_t
      read(101,*) i39,i39_t
	read(101,*) idemand_info
!      read(101,*) i40,i40_t
      i40 = 1
	i40_t = 10 ! hard-wire them for now

	i18 = 1 !Fix i18 = 1 for now since we need to output vehicle trajectory as the default

!Added by Jing & Xuesong Mar 16 2004 Dynasmart 1.0
	i180_t = 10 !Fix i180_t = 10 for every minute
	i180 = 0 !Fix i180 = 10 for every minute to output vehicle location as the default


	if(i30.gt.0) open(file='OutLinkGen.dat',unit=30,status='unknown')
	if(i31.gt.0) open(file='OutLinkVeh.dat',unit=31,status='unknown')
	if(i32.gt.0) open(file='OutLinkQue.dat',unit=32,status='unknown')
	if(i33.gt.0) open(file='OutLinkSpeedAll.dat',unit=33,
     +status='unknown')
	if(i34.gt.0) open(file='OutLinkDent.dat',unit=34,status='unknown')
	if(i35.gt.0) open(file='OutLinkSpeedFree.dat',unit=35,
     +status='unknown')
	if(i36.gt.0) open(file='OutLinkDentFree.dat',unit=36,
     +status='unknown')
	if(i37.gt.0) open(file='OutLeftFlow.dat',unit=37,status='unknown')
	if(i38.gt.0) open(file='OutGreen.dat',unit=38,status='unknown')
	if(i39.gt.0) open(file='OutFlow.dat',unit=39,status='unknown')
	if(i40.gt.0) open(file='OutAccuVol.dat',unit=40,status='unknown')

	open(file='LinkVolume.dat',unit=29,status='unknown')


! -- start reading Work Zone data

      if(WorkZoneNum.gt.0) then
      do i = 1, WorkZoneNum
        read(58,*) FNodetmp,TNodetmp,WorkZone(i)%ST,
     *             WorkZone(i)%ET,WorkZone(i)%CapRed,
     *             WorkZone(i)%SpeedLmt,WorkZone(i)%Discharge

       WorkZone(i)%FNode=idnum(FNodeTmp)
       WorkZone(i)%TNode=idnum(TNodetmp)
       enddo
	endif


	if(WorkZoneNum.gt.1) then
	  do i=1,WorkZoneNum
	    do j=2,WorkZoneNum
	if(i.ne.j) then
		  if(WorkZone(i)%FNode.eq.WorkZone(j)%FNode .and.
     *	    WorkZone(i)%TNode.eq.WorkZone(j)%TNode) then
	 	    
	        if(WorkZone(i)%ST.eq.WorkZone(j)%ST.and.
     +            WorkZone(i)%ET.eq.WorkZone(j)%ET) then

	write(911,*)'Multiple work zones were found active on same link'
	write(911,*)'Please review workzone.dat and adjust start and end' 
	write(911,*)'times for the workzones specified on same link'
	stop      
			
					
			
			elseif( (WorkZone(i)%ST.lt.WorkZone(j)%ET.and.
     +            WorkZone(i)%ST.gt.WorkZone(j)%ST)  .or.      
     +             (WorkZone(j)%ST.lt.WorkZone(i)%ET.and.
     +            WorkZone(j)%ST.gt.WorkZone(i)%ST) ) then
     
	write(911,*)'Multiple work zones were found active on same link'
	write(911,*)'Please review workzone.dat and adjust start and end' 
	write(911,*)'times for the workzones specified on same link'
	stop
			endif
		  endif
	endif
		enddo
	  enddo
	endif  	              	





!added by hayssam july 10 2003 to check for links that have both 
!work zone and an incident at the same time
	
	k =0
	if(WorkZoneNum.gt.0.and.inci_num.gt.0) then
		do i=1,inci_num
			do j=1,WorkZoneNum
				if(iunod(incil(i)).eq.WorkZone(j)%FNode.and.
     *			idnod(incil(i)).eq.WorkZone(j)%TNode) then
				k=incil(i)

	write(511,*)
	write(511,*)'Link',nodenum(iunod(k)),'       ->',nodenum(idnod(k))
	write(511,*)'Has a work zone and an incident specified'
	write(511,*)'simultaneously. The work zone will override the' 
	write(511,*)'incident when both are active'
	write(511,*)

				endif
		end do
	end do
	endif

!**********************************************************************************
!added by hayssam April 17 2004 to check for links that have more than one incident 
! at the same time 
! DYNA 1.0

	k=0
	if(inci_num.gt.1) then
		do i=1,inci_num
			do j=i+1,inci_num
	            if(incil(i).eq.incil(j)) then
				
	            if(k.ne.incil(i)) then
				k=incil(i)

	write(511,*)
	write(511,*)'Link',nodenum(iunod(k)),'       ->',nodenum(idnod(k))
	write(511,*)'Has multiple incidents specified on it'
	write(511,*)'The incident with the highest severity will govern' 
	write(511,*)'when more than one incident is active'
	write(511,*)

				endif
	endif
	       end do
	end do
	endif


!added by hayssam April 17 2004 to check for links that have more than one VMS 
! at the same time 
! DYNA 1.0

	k=0
	if(vms_num.gt.1) then
		do i=1,vms_num
			do j=i+1,vms_num
	            if(vms(i,1).eq.vms(j,1)) then
				
	            if(k.ne.vms(i,1)) then
				k=vms(i,1)

	write(511,*)
	write(511,*)'Link',nodenum(iunod(k)),'       ->',nodenum(idnod(k))
	write(511,*)'Has multiple VMS signs specified on it'
	write(511,*)'The mandatory detour VMS will always govern, followed' 
	write(511,*)'by the optional detour, and congestion warning'
	write(511,*)'when more than one VMS is active'
	write(511,*)'Speed advisory VMS can coexist with any other VMS'
	write(511,*)
				endif
				endif
	       end do
	end do
	endif

        
	 ! check if multiple ramp meters exist  
	  
	k=0
	if(dec_num.gt.1) then
	   do i=1,dec_num
	     do j=i+1,dec_num
              if (detector_ramp(i).eq. detector_ramp(j)) then
!added by hayssam June 28 2005
	          if( ramp_start(i).eq.ramp_start(j).and.
     +			  ramp_end(i).eq.ramp_end(j)) then
     
	write(911,*)
!	write(911,*)'Multiple ramp meters were found on same link'
	write(911,*)'Multiple ramp meters were found active on same link'
	write(911,*)'DYNASMART-P does not support multiple ramp meters' 
	write(911,*)'Please review ramp.dat and delete multiple ramps'
	write(911,*)

	stop       
			  
			  elseif( (ramp_start(i).lt.ramp_end(j).and.
     +            ramp_start(i).gt.ramp_start(j))  .or. 
     +            (ramp_start(j).lt.ramp_end(i).and.
     +            ramp_start(j).gt.ramp_start(i)))     then           
				
				k = detector_ramp(i)

	write(911,*)
!	write(911,*)'Multiple ramp meters were found on same link'
	write(911,*)'Multiple ramp meters were found active on same link'
	write(911,*)'DYNASMART-P does not support multiple ramp meters' 
	write(911,*)'Please review ramp.dat and delete multiple ramps'
	write(911,*)

	stop
					endif
					endif
	       end do
	end do
	endif

		 ! check if ramp meters exist with Incidents 
	  
	
 





c -- how many simulation interval  do we average the travel time 
c -- for the time dependant KSP.
      interval_avg_for_tdksp=ftr
      timeinterval=interval_avg_for_tdksp*tii

!--  check warning.dat's file size

      if(iteration.eq.0) then

!      INQUIRE (UNIT = 511, OPENED = Fexist)
       call GETFSIZE("warning.dat",ifsize)
       IF (ifsize.gt.0) then

!       IF (Fexist) THEN
       write(6,'("There are warning messages recorded in warning.dat")') 
       write(6,'("")') 
 	write(6,'("Type <Y> or <y> followed by <enter> to exit program")')
 	write(6,'("Open the warning.dat file to see warning messages")') 
 	write(6,'("Otherwise, type any other key followed by <enter>")') 
 	write(6,'("to continue executing the program")') 
 	write(6,'("")') 		 
	  read(*,*) Reply
        if(iachar(reply).eq.89.or.iachar(reply).eq.121) then ! Y or y
	    stop
	  endif
!	  IP = system("cls")
      END IF

      endif


	deallocate(demtmp)

! Added by MTI team April 4 2004 1.0	
	if(Superzoneswitch.eq.1) then
	deallocate(tmp_superzone)
	endif
! end
      
	return 
      end

      SUBROUTINE GETFSIZE(FID,SIZE)
      USE DFLIB
      !===Get file size of file with FILE=FID 
      !  Code is set up for Compaq CVF
      CHARACTER*(*) FID
      INTEGER SIZE
      ! UNITNO is not used for compilers implemented here.
      
      !---Compaq Fortran; same as MS Fortran requires following 10 lines.
      !     USE DFLIB (must be just after SUBROUTINE statement).
      INTEGER*4 iresult
      TYPE (FILE$INFO) info
      integer (int_ptr_kind()) :: handle
      handle = FILE$FIRST
      iresult = GETFILEINFOQQ(FID,info,handle)
      if( iresult.NE.0 ) THEN
         SIZE=info.length
      ELSE
         SIZE=0
      END IF
      RETURN
      END    
