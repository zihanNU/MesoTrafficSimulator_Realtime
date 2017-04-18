    subroutine openfile()
      
	use muc_mod 
    integer error
    integer :: error_SH	
	
	open(file='ErrorLog.dat',unit=911,status='unknown',iostat=error)
	if(error.ne.0) then
         write(911,*) 'Error when opening ErrorLog.dat'
	   stop
	endif

! Added by MTI team April 7 2004 1.0.0
	open(file='Warning.dat',unit=511,status='unknown',iostat=error)
	if(error.ne.0) then
         write(911,*) 'Error when opening Warning.dat'
	   stop
	endif


	open(file='network.dat',unit=41,status='old',iostat=error) 
	if(error.ne.0) then
         write(911,*) 'Error when opening network.dat'
	   stop
	endif

! Added August 12, 2009 for link-specific maximum density implementation
! open the MaxDensity.dat, if available
	inquire(file='MaxDensity.dat',exist=lexist)
	if(lexist) then
	  i_maxden = 1
	  open(file='MaxDensity.dat',unit=551,status='old',iostat=error) 
	  if(error.ne.0) then
          write(911,*) 'Error when opening MaxDensity.dat'
	    stop
	  endif
	else
	  i_maxden = 0
	endif      

	open(file='demand.dat',unit=42,status='old',iostat=error) 
	if(error.ne.0) then
         write(911,*) 'Error when opening demand.dat'
	   stop
	endif

	open(file='scenario.dat',unit=43,status='old',iostat=error) 
	if(error.ne.0) then
         write(911,*) 'Error when opening scenario.dat'
	   stop
	endif

	open(file='control.dat',unit=44,status='old',iostat=error) 
	if(error.ne.0) then
         write(911,*) 'Error when opening control.dat'
	   stop
	endif

	open(file='ramp.dat',unit=45,status='old',iostat=error) 
	if(error.ne.0) then
         write(911,*) 'Error when opening ramp.dat'
	   stop
	endif

	open(file='incident.dat',unit=46,status='old',iostat=error) 
	if(error.ne.0) then
         write(911,*) 'Error when opening incident.dat'
	   stop
	endif

	open(file='movement.dat',unit=47,status='old',iostat=error) 
	if(error.ne.0) then
         write(911,*) 'Error when opening movement.dat'
	   stop
	endif

	open(file='leftcap.dat',unit=48,status='old',iostat=error) 
	if(error.ne.0) then
         write(911,*) 'Error when opening leftcap.dat'
	   stop
	endif

	open(file='vms.dat',unit=49,status='old',iostat=error) 
	if(error.ne.0) then
         write(911,*) 'Error when opening vms.dat'
	   stop
	endif

	! Added for weather VMS, June 8 2009
	inquire(file='vsl.dat',exist=lexist)
	if(lexist) open(file='vsl.dat',unit=449,status='old',iostat=error) 
	if(error.ne.0) then
         write(911,*) 'Error when opening vsl.dat'
	   stop
	endif

	open(file='bus.dat',unit=50,status='old',iostat=error) 

	if(error.ne.0) then
         write(911,*) 'Error when opening bus.dat'
	   stop
	endif

	open(file='pricing.dat',unit=51,status='old',iostat=error) 
	if(error.ne.0) then
         write(911,*) 'Error when opening pricing.dat'
	   stop
	endif

	open(file='origin.dat',unit=52,status='old',iostat=error)
	if(error.ne.0) then
         write(911,*) 'Error when opening origin.dat'
	   stop
	endif

	open(file='destination.dat',unit=53,status='old',iostat=error)
	if(error.ne.0) then
         write(911,*) 'Error when opening destination.dat'
	   stop
	endif


	open(file='demand_truck.dat',unit=54,status='old',iostat=error) 
	if(error.ne.0) then
         write(911,*) 'Error when opening demand_truck.dat'
	   stop
	endif


! Added by Xuesong and Hayssam Jan 22 2004 0.930.9
	open(file='demand_HOV.dat',unit=61,status='old',iostat=error) 
	if(error.ne.0) then
         write(911,*) 'Error when opening demand_HOV.dat'
	   stop
	endif


	open(file='TrafficFlowModel.dat',unit=55,status='old',iostat=error) 
	if(error.ne.0) then
         write(911,*) 'Error when opening TrfficFlowModel.dat'
	   stop
	endif

	open(file='StopCap4Way.dat',unit=56,status='old',iostat=error) 
	if(error.ne.0) then
         write(911,*) 'Error when opening StopCap4Way.dat'
	   stop
	endif

	open(file='StopCap2Way.dat',unit=57,status='old',iostat=error) 
	if(error.ne.0) then
         write(911,*) 'Error when opening StopCap2Way.dat'
	   stop
	endif

	open(file='YieldCap.dat',unit=60,status='old',iostat=error) 
	if(error.ne.0) then
         write(911,*) 'Error when opening YieldCap.dat'
	   stop
	endif


	open(file='WorkZone.dat',unit=58,status='old',iostat=error) 
	if(error.ne.0) then
         write(911,*) 'Error when opening WorkZone.dat'
	   stop
	endif

    open(file='GradeLengthPCE.dat',unit=59,status='old',iostat=error) 
	if(error.ne.0) then
         write(911,*) 'Error when opening GradeLengthPCE.dat'
	   stop
	endif


	open(file='system.dat',unit=95,status='old',iostat=error)
	if(error.ne.0) then
         write(911,*) 'Error when opening system.dat'
	   stop
	endif

	open(file='output_option.dat',unit=101,status='old',iostat=error)
	if(error.ne.0) then
         write(911,*) 'Error when opening output_option.dat'
	   stop
	endif

      open(file='vehicle.dat',unit=500,status='unknown',iostat=error)
	if(error.ne.0) then
         write(911,*) 'Error when opening vehicle.dat'
	   stop
	endif

	open(file='path.dat',unit=550,status='unknown',iostat=error)
	if(error.ne.0) then
         write(911,*) 'Error when opening path.dat'
	   stop
	endif

	open(file='Executing',unit=912,status='unknown',iostat=error)
	if(error.ne.0) then
         write(911,*) 'Error when opening executing.dat'
	   stop
	endif

	write(912,*) 'DYNASPART-P IS RUNNING....' 
	close(912)
	open(file='VehTrajectory.dat',unit=18,status='unknown',iostat=error)
	if(error.ne.0) then
         write(911,*) 'Error when opening VehTrajectory.dat'
	   stop
	endif

! Added by Xuesong ImpactedVehicles.dat June 28 2005
! September 1 2005
	open(file='VehImpact.dat',unit=19,status='unknown',iostat=error)
!	open(file='VehImpact.dat',unit=19,status='REPLACE',iostat=error)
	if(error.ne.0) then
         write(911,*) 'Error when opening VehImpact.dat'
	   stop
	endif


	open(file='BusTrajectory.dat',unit=188,status='unknown',iostat=error)
	if(error.ne.0) then
         write(911,*) 'Error when opening BusTrajectory.dat'
	   stop
	endif
    !Modification: 10182013
    if(iterativeSummary) then
        if(iteration.eq.0) then
	       open(file='SummaryStat.dat',unit=666,status='unknown',iostat=error)
!	    else
!	       open(file='SummaryStat.dat',unit=666,status='old',iostat=error) 
	    endif
	else    
        open(file='SummaryStat.dat',unit=666,status='unknown',iostat=error)
	    if(error.ne.0) then
             write(911,*) 'Error when opening SummaryStat.dat'
	       stop
	    endif
    endif

    !Modification: 10182013
	open(file='OutMUC.dat',unit=180,status='unknown',iostat=error)
	if(error.ne.0) then
         write(911,*) 'Error when opening OutMUC.dat'
	   stop
	endif

 	open(file='fort.600',unit=600,status='unknown',iostat=error)
	if(error.ne.0) then
         write(911,*) 'Error when opening fort.600'
	   stop
	endif

	open(file='fort.700',unit=700,status='unknown',iostat=error)
	if(error.ne.0) then
         write(911,*) 'Error when opening fort.700'
	   stop
	endif

	open(file='fort.800',unit=800,status='unknown',iostat=error)
	if(error.ne.0) then
         write(911,*) 'Error when opening fort.800'
	   stop
	endif

	open(file='fort.900',unit=900,status='unknown',iostat=error)
	if(error.ne.0) then
         write(911,*) 'Error when opening fort.900'
	   stop
	endif


    !Added Feb 10 2009 Weather Impact
    inquire(file='WAF.dat',exist=lexist1)
    inquire(file='weather.dat',exist=lexist2)
    if(lexist1.and.lexist2) then
        i_weather = 1
    	open(file='WAF.dat',unit=400,status='unknown',iostat=error)
	    if(error.ne.0) then
            write(911,*) 'Error when opening WAF.dat'
	        stop
	    endif
    	open(file='weather.dat',unit=401,status='unknown',iostat=error)
	    if(error.ne.0) then
            write(911,*) 'Error when opening weather.dat'
	        stop
	    endif
	elseif(lexist2.and.(.not.lexist1)) then
        write(911,*) 'Error: Please specify WAF.dat'
        write(911,*) 'to use the weather feature'
        stop    
	endif
    ! End    
    
    

	
	inquire(file='ReversibleLane.dat',exist=lexist_RevLane)
	inquire(file='vms_reversible.dat',exist=lexist_vmsRevLane)
	
    if(lexist_RevLane.AND.lexist_vmsRevLane) then
        RevLane_flag = 1
    	open(file='ReversibleLane.dat',unit=79,status='unknown',iostat=error)
    	open(file='vms_reversible.dat',unit=499,status='unknown',iostat=error)
	else if (lexist_RevLane.OR.lexist_vmsRevLane) then
	    write(911,*) 'Missing ReversibleLane.dat or vms_reversible.dat'
	else 
	    RevLane_flag = 0	    
	endif
    ! End   
 
 ! c--    Modified by Zihan on 20160427 for AMS
	
	inquire(file='shoulder.dat',exist=lexist_ShlLane)
	
    if(lexist_ShlLane) then
        ShlLane_flag = 1
    	open(file='shoulder.dat',unit=80,status='unknown',iostat=error)
	else 
	    ShlLane_flag = 0	    
	endif
    ! End     
    
    
    
 ! c--    Add by Zihan on 20160606 for AMS
	
	inquire(file='predict.dat',exist=lexist_predict)
	
    if(lexist_predict) then
        i_predictiveinfo = 1
    	open(file='predict.dat',unit=703,status='unknown',iostat=error)
	else 
	    i_predictiveinfo = 0	    
	endif
    ! End Addition 
    
    
! c--    Modified by Archak on 20160325 for AMS 
	inquire(file='Linkforobs_SH.dat',exist=lexist_SH)
	
    if(lexist_SH) then
        SH_flag = 1
    	open(file='Linkforobs_SH.dat',unit=778,status='unknown',iostat=error)
	else 
	    SH_flag = 0	    
	endif
    ! End Addition 
    

!c-- Modified by Alex on 20160404 for snow accumulation
    inquire(file='SnowAccuFactor.dat',exist=lexist_SnowAccu)
    if (lexist_SnowAccu) then 
        open(file='SnowAccuFactor.dat',unit=611,status='unknown',iostat=error)
    else
        write(911,*) 'Do not consider snow accumulation'
    endif    
    inquire(file='SnowplowRoutes.dat',exist=lexist_SnowPlow)
    if(lexist_SnowPlow) then 
        open(file='SnowplowRoutes.dat',unit=612,status='unknown',iostat=error)
        open(file='LinkToBeServed.dat',unit=613,status='unknown',iostat=error)
    else
        write(911,*) 'Do not consider snowplow'
    endif
end subroutine