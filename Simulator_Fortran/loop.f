      subroutine loop(starttime,endtime,maxintervals)
c --
c -- This is the main time loop for DYNASMART.  It loops for every simulation
c -- interval.
c --
c -- This subroutine is called from the main (in the off-line case) or
c -- from CORBA (in the real-time case)
c --
c -- This subroutine calls the following subroutines.
c -- 1. gui_stat
c -- 2. display_results (this is a C code to activate the graphics)
c --	3. read_signals
c --	4. penalty_calculation
c --	5. ksp_main
c -- 	6. inci_check
c --    7. intersection_control
c --    8. get_link_capacity
c --    9. ksp_update
c --   10. demand_generation
c -    11. vehicle_simulation
c --   12. vms_main
c --   13. ramp_metering
c --   14. final_statistics
c --   15. link_pricing 
c --   
c -- INPUT :
c -- starttime : the staring simulation interval for the loop.
c -- endtime : the ending simulation interval for the loop.
c --
c -- OUTPUT :
c -- fort.4 which includes the following statistics
c -- l : the current simulation interval number.
c -- jj : total number of vehicels loaded till now.
c -- numcars : number of targe vehicles (between STATTM and ENDTM) still in the network.
c -- nout_nontag : number of non-tagged vehicles which reached their i
c --               destination.
c -- nout_tag : number of tagged vehicles which reached their destination.
c --
c -- fort.666 : moved to fort.6 after the end of the simulation.
c --            It outputs statistics about the vehicle generation process 
c --            and the termination reason.
c --
      use muc_mod
	use vector_mod
	integer starttime,endtime,maxintervals,soindex
      integer load_veh,dy_muc,genelink
	Logical LPFileExists	
	integer iti_nuall	,error
	
	integer shift      !Add by Zihan for tdsp to update TT(1,iti_nu)
	
        character(LEN=99) :: text_time ! Add by Archak 07032016 to store current time in minutes	
        character(LEN=450) :: filename_SH ! Add by Archak 07032016 to store the file name for spd-harm
        integer :: SH_file_check = 0 
        character(LEN=350)  :: path, subpath
	
      dy_muc = 0	
!      write(*,*) "Start Time= " , starttime, "and End Time= " ,endtime
c --
      do 12 l=starttime, endtime
c -- 
c -- t_start : the start of the current simulation interval (in minutes)
c -- time_now : the end of the current simulation interval (in seconds)
c --
      t_start=(l-1)*tii
      time_now=time_now+tii*60
      
      iti_nuall=nint((stagelength/tii)/ftr) +1 
      
      ! Add for AMS Project 20160606 by Zihan 
		call check_control_update
		!call check_VMS_update
	  call check_network_update
	  !call check_vsl_update
	if  ((mod(int(time_now-latency*60),(roll*60))).eq.0) then
        if ((time_now-latency*60).ge.(roll*60)) then
	          call check_predictiveinfo	 
	      !endif     
	  endif
	
	
!	elseif (mod(int(time_now),60).eq.0) then   !every minute
!	      shift= time_now/60                 !shift first time_now/60 minute
!            do itt=1,iti_nu-shift
!	          do ilink=1,noofarcs
!                 TTime(ilink,itt)=TTime(ilink,itt+shift)
!                  do movee=1,nu_mv
!            TTpenalty(ilink,itt,movee)= TTpenalty(ilink,itt+shift,movee)
!                  enddo
!	          enddo
!	      enddo
      endif    
! End add

! Add by Archak 07052016
! Check if the shockwave files are present are present and if not, wait.

        if (SH_flag.AND.(l.eq.1.or.mod(l,idemand_info).eq.0)) then    
            if(l.eq.1) then
                write(text_time,'(i4)') int(0)
            else 
                write(text_time,'(i4)') nint(l*tii)
            endif

            CALL getcwd(path)
            subpath='\predictiveinfo\'
           
            filename_SH=trim(adjustl(path))//trim(adjustl(subpath))// 
     *  'shockwave_SH_'// trim(adjustl(text_time)) // '.dat'
        
!      filename_SH ='C:\\Users\\Administrator\\Desktop\\Archak\\AMS\\
!     *OC1-7shot_0.7\\shockwave_SH_'//trim(adjustl(text_time))//'.dat'
     
!        write(*,*) "Time now is " , l*tii
            inquire (file=filename_SH, exist=SH_file_check)
!        write(*,*) SH_file_check
            do while (SH_file_check.EQ.0)
                inquire (file=filename_SH, exist=SH_file_check)
                write(*,*) "waiting for ",filename_SH
                call sleep(2)
            end do            

            open(file=filename_SH,unit=766, status = "old",iostat=error)
	        if(error.ne.0) then
                write(911,*) 'Error when opening shockwave_SH'
	            stop
	        endif	    
          
            read(766, *) shockwave_count
!            write(*,*) "Shockwave count is " , shockwave_count
            read(766,*) !skip a line
            
            IF (ALLOCATED (link_SH)) DEALLOCATE (link_SH)
            IF (ALLOCATED (time_SH)) DEALLOCATE (time_SH)
            IF (ALLOCATED (flowmodel_SH)) DEALLOCATE (flowmodel_SH)
            
            allocate(link_SH(shockwave_count),stat=error)
            if(error.ne.0) then
                write(911,*) "link_SH error - insufficient memory"
                stop
            endif
            link_SH(:)= 0 

            allocate(time_SH(shockwave_count),stat=error)
            if(error.ne.0) then
                write(911,*) "time_SH error - insufficient memory"
                stop
            endif
            time_SH(:)= 0 

            allocate(flowmodel_SH(shockwave_count),stat=error)
            if(error.ne.0) then
                write(911,*) "flowmodel_SH error - insufficient memory"
                stop
            endif
            flowmodel_SH(:)= 0 

            do line_SH = 1, shockwave_count        
                read (766,*) time_SH(line_SH),  
     *    link_SH(line_SH), flowmodel_SH(line_SH)  
!              write (*,*) "shockwave file read ", time_SH(line_SH),  
!     *    link_SH(line_SH), flowmodel_SH(line_SH)  
            enddo                   
         endif
      close(766)
! End of addition      


!Added Feb 10 2009 Weather Impact
      if(i_weather.eq.1)then
        linkWAF(:,:) = 1 !initial value of WAF for all links
!Weather June 1 2011
!        if(network_visibility.gt.0.and.t_start.ge.network_st
!     *    .and.t_start.lt.network_et) then ! across-the-board weather 
!          do i = 1, noofarcs
!            linkWAF(i,:)= WAF(:,1)+WAF(:,2)*network_visibility
!     *       +WAF(:,3)*network_rain+WAF(:,4)*network_snow
!     *       +WAF(:,5)*network_visibility*network_rain
!     *       +WAF(:,6)*network_visibility*network_snow
!            do j = 1, 18
!              if(linkWAF(i,j).gt.1.0) linkWAF(i,j) = 1.0    !max WAF is 1.0              
!              if(linkWAF(i,j).lt.0.1) linkWAF(i,j) = 0.1    !min WAF is 0.1
!            enddo  
!          enddo  
!        endif

   ! for single period network wide weather effects <-- SAME as OLD weather.dat
        IF (network_weather.eq.1) then
            if(network_visibility_scalar.gt.0.and.t_start.ge.
     *       network_st_scalar.and.t_start.lt.network_et_scalar) then 
  
            do i = 1, noofarcs
            
!Weather June 10 2011
!we don't assing linkWAF on origin and destination connectors
!               linkWAF(i,:)= WAF(:,1)+WAF(:,2)*network_visibility_scalar
!     *       +WAF(:,3)*network_rain_scalar+WAF(:,4)*network_snow_scalar
!     *       +WAF(:,5)*network_visibility_scalar*network_rain_scalar
!     *       +WAF(:,6)*network_visibility_scalar*network_snow_scalar
               IF(link_iden(i).ne.99.and.link_iden(i).ne.100) then  
               !Modification 11052012  
               !     linkWAF(i,:)= WAF(:,1)+WAF(:,2)*network_visibility_scalar
              linkWAF(i,:)= WAF(:,1)+WAF(:,2)*network_visibility_scalar     
               !Modification 11052012 End
     *       +WAF(:,3)*network_rain_scalar+WAF(:,4)*network_snow_scalar
     *       +WAF(:,5)*network_visibility_scalar*network_rain_scalar
     *       +WAF(:,6)*network_visibility_scalar*network_snow_scalar
               ENDIF
!end Weather June 10 2011
               
     
                do j = 1, 18
                  if(linkWAF(i,j).gt.1.0) linkWAF(i,j) = 1.0    !max WAF is 1.0              
                  if(linkWAF(i,j).lt.0.1) linkWAF(i,j) = 0.1    !min WAF is 0.1
                enddo  
              enddo  
            endif
        ENDIF
    ! end single period    
    
    ! for multiple time periods network wide weather effects <-- NEW weather.dat
      IF (network_weather.ge.2) then 
        do y=1,network_weather
          if(network_visibility(y).gt.0.and.t_start.ge.network_st(y)
     *    .and.t_start.lt.network_et(y)) then ! across-the-board weather 
          do i = 1, noofarcs
          
!Weather June 10 2011
!we don't assing linkWAF on origin and destination connectors
!            linkWAF(i,:)= WAF(:,1)+WAF(:,2)*network_visibility(y)
!     *       +WAF(:,3)*network_rain(y)+WAF(:,4)*network_snow(y)
!     *       +WAF(:,5)*network_visibility(y)*network_rain(y)
!     *       +WAF(:,6)*network_visibility(y)*network_snow(y)
           IF(link_iden(i).ne.99.and.link_iden(i).ne.100) then   
            linkWAF(i,:)= WAF(:,1)+WAF(:,2)*network_visibility(y)
     *       +WAF(:,3)*network_rain(y)+WAF(:,4)*network_snow(y)
     *       +WAF(:,5)*network_visibility(y)*network_rain(y)
     *       +WAF(:,6)*network_visibility(y)*network_snow(y)
           ENDIF
!end Weather June 10 2011
           
            do j = 1, 18
              if(linkWAF(i,j).gt.1.0) linkWAF(i,j) = 1.0    !max WAF is 1.0              
              if(linkWAF(i,j).lt.0.1) linkWAF(i,j) = 0.1    !min WAF is 0.1
            enddo  
          enddo  
        endif
       enddo
      ENDIF
    ! end multiple
!end Weather June 1 2011


        if(num_of_weatherlinks.ge.1) then ! link-specific weather
         do il=1, num_of_weatherlinks 
          iFNode = LinkWeathers(il)%FNode
	    iTNode = LinkWeathers(il)%TNode
	    iForArc = GetFLinkFromNode(iFNode, iTNode)
          do ilt=1, LinkWeathers(il)%num_weather_per_link
	      if(t_start.ge.LinkWeathers(il)%Weathers(ilt)%ST.and.
     *         t_start.lt.LinkWeathers(il)%Weathers(ilt)%ET) then
               linkWAF(iForArc,:)= WAF(:,1)+WAF(:,2)*LinkWeathers(il)
     *          %Weathers(ilt)%visibility+WAF(:,3)*LinkWeathers(il)
     *          %Weathers(ilt)%rain+WAF(:,4)*LinkWeathers(il)
     *          %Weathers(ilt)%snow+WAF(:,5)*LinkWeathers(il)
     *          %Weathers(ilt)%visibility*LinkWeathers(il)
     *          %Weathers(ilt)%rain+WAF(:,6)*LinkWeathers(il)
     *          %Weathers(ilt)%visibility*LinkWeathers(il)
     *          %Weathers(ilt)%snow    
            endif
          enddo
          
         enddo
         !Weather June 7 2011
         do i = 1, noofarcs
            do j = 1, 18
              if(linkWAF(i,j).gt.1.0) linkWAF(i,j) = 1.0    !max WAF is 1.0              
              if(linkWAF(i,j).lt.0.1) linkWAF(i,j) = 0.1    !min WAF is 0.1
            enddo
         enddo 
         !end Weather June 7 2011
        endif
      endif
      
      
      
!End
c --
c -- int_d : display interval (input from the GUI).  It is the number of
c --         simulation intervals for each GUI referesh.
c --
c -- ireal : a flag received from the GUI to indicate the execution mode.
c --        If ireal =0, run off-line , if ireal=1 run real-time
c --
      if(mod((l-1),int_d).eq.0.and.l.gt.1) then
	  call gui_stat(l)
	endif
      
c -- update the signal timing plan counter
c --
      if(isig.gt.1) then
      !Hooram
	! do mg = 2, isig
	 do mg = isigcount+1, isig
	!Hooram 
	   ! modified by MTI team for 930.9 on Feb 9 2004
	   !if((l*tii-strtsig(mg)).lt.0.1) then
	    if((l*tii-strtsig(mg)).gt.0.1) then

	    isigcount=isigcount+1
	    ! added by MTI team for 930.9 on Feb 9 2004
	     !*******************
		 if (isigcount.gt.isig) then
			isigcount = isig
		 endif
		 !*********************	   
		  exit
	   endif
	 enddo
      endif

c --
c -- check if the start of a new signal timing plan has been reached.  If yes
c -- read the new signals and update the penalties and the shortest path.
c --
! potential error or inefficiency Hayssam Nov 3 2003

	   ! modified by MTI team for 930.9 on Feb 9 2004
        !if(isig.gt.1.and.((l-1)*tii.eq.strtsig(isigcount))) then
	!*************************************
      if((isig.gt.1).and.(isigcount.gt.1)) then
	if((l-1)*tii.gt.(strtsig(isigcount)-0.01).and.
     + ((l-1)*tii.lt.(strtsig(isigcount)+0.01))) then
	  SignCount = 0
      !*******************************************************

           call read_signals()
           if(iteration.eq.0) then       
             call penalty_calculation(l)
          ! Added for weather VMS, June 8 2009
             if(i_risk.eq.1) call link_risk
             call link_pricing
             call kspcost_main(dy_muc)
           endif
        endif
	   ! added by MTI team for 930.9 on Feb 9 2004
		endif

c --
c -- If there are some incidents in the network, call the inci_check
c --
       if(inci_num.gt.0) call inci_check(l*tii) 


c --
c -- If there are some work zones in the network, call the wz_check
c --
       if(WorkZoneNum.gt.0) call wz_check(l*tii) 

      !Added by Alex on 20160405 for snow accumulation
      if (lexist_SnowAccu) then 
        if(sum(network_snow).ne.0) call SnowAccu_check(l*tii)
      endif

c --
c -- for the first simulation interval, calculate the penalty
c --
      if(l.eq.1) then
        call penalty_calculation(l)
      endif

c --
      call intersection_control(l)

c --
      call get_link_capacity()

c --
c -- path calculation  
c -- kspstep : number of simulation intervals for K-shortest path calculation.
c -- kupstep : number of simulation intervals for K-shortest path update.
c --

!potential inefficiency Hayssam Nov 3 2003


c  -- shortest path calculation or update   

! Modified by MTI team jan 24 2004
!      if(iteration.eq.0.and.(realdm.ne.2.or.noofstops.gt.1.or.
!     *classpro(4)-classpro(3).gt.0.001.or.vms_num.gt.0)) then

! Modified by MTI team April 14 2004 DYNA-P 1.0
! need to calculate ksp no matter what loading mode it is used
!     if((iteration.eq.0.and.(realdm.ne.2.or.noofstops.gt.1))
	if((iteration.eq.0).or.(iteration.eq.0.and.noofstops.gt.1)
     *	.or.ienroute_ok.eq.1.or.vms_num.gt.0) then

 ! Modified by Zihan, 20160617 for AMS-ADM strategy, predictive info
          
           if(mod(l,kspstep).eq.0.) then
        ! Added for weather VMS, June 8 2009
           if(i_risk.eq.1) call link_risk
           call link_pricing
           call kspcost_main(dy_muc)
         else if(mod(L,kupstep).eq.0) then
                

!
!          if(mod(l,kspstep).eq.0.) then
!             if(i_risk.eq.1) then 
!                 call link_risk
!             endif
!           if((i_predictiveinfo.eq.0).or.(int(time_now/60).lt.roll))then
!                 call link_pricing
!                 call kspcost_main(dy_muc)
!             endif
!          else if(mod(L,kupstep).eq.0) then
                

!Added Sep 12 2005
           if(iSequentialLoad.eq.0) then

           do itmp=1,noof_master_destinations
             if(destination(itmp).ne.0) then
               do ltype=1,no_link_type
                 do ioccup=1,no_occupancy_level
                ! Added for weather VMS, June 8 2009
                  if(i_risk.eq.1) call link_risk
                  call link_pricing
                  call kspcost_update(itmp)
                 end do
               end do
	       endif
           end do

           endif !if(iSequentialLoad.eq.0) then

         endif
      endif


      ! Add by Zihan 20160606 according to DTAX to update control file
      if (i_new_control.eq.1 .and. isigcount.eq.1) then
         if((l-1)*tii.gt.(strtsig(isigcount)-0.01).and.
     +   ((l-1)*tii.lt.(strtsig(isigcount)+0.01))) then
     
            SignCount = 0
            call read_signals()
            i_new_control = 0
            
            if(iteration.eq.0) then       
                call penalty_calculation(l)
                if(i_risk.eq.1) call link_risk
                call link_pricing
                call kspcost_main(dy_muc)
            endif           
         endif
      endif
      
!      if (i_new_tt.eq.1 .or. i_new_tp.eq.1) then 
!      ! if predictive info updated
!           call kspcost_main(dy_muc)
!           i_new_tt=0
!           i_new_tp=0
!      endif
      !end addition




c --

c -- vehicle generation process : via OD matrix or path and vehicle files.
c --
c -- jtotal : total number of vehicles in the path and vehicle files.
c -- jrestore : number of vehicles already loaded + 1.
c -- 

! Modified by MTI team Jan 29 2004 0.930.9
       vlg(:)=0.0

      if(realdm.eq.1.and.iteration.eq.0) then
! demand is obtained from file for dynasmart-p or rt_dyna only.
	  if(isystemrunningmode .ne. 2) then
          call demand_generation(l)
	  endif
!Added by MTI team Mar 8 2004
!To call a seperate subroutine which gets demand from ODP module in -X.
        if(isystemrunningmode .ne. 0) then
	
		call demand_generation_from_ODP(l)

        endif

!       call vehicle_att_generation()
!Added May 26 2005
!       call vehicle_att_generation()
      if(iSequentialLoad.eq.1) then ! Jan 26 2006
	  if(l.eq.1.or. 
     *(mod(l,kspstep).eq.0.and.(time_now/60.0).lt.begint(nints+1))) then 
	    call vehicle_att_generation()  
	  endif
	else
        call vehicle_att_generation()
      endif
!End

      elseif(realdm.ne.1.and.iteration.eq.0) then
! Commented out by MTI team Jan 29 2004 0.930.9
!	  vlg(:)=0
	  call read_vehicles(t_start)
	elseif(iteration.gt.0) then
! Commented out by MTI team Jan 29 2004 0.930.9
!	  vlg(:)=0
	  load_veh = 0
        do ji=jrestore,jtotal
		
          if(abs(stime(ji)-t_start).gt.0.01) goto 1920

! Modified by MTI team Jan 17 2004 0.930.7D
			jj_MUC = jj_MUC+1
           if(ioc(ji).eq.1) then
	          if(vehclass(ji).eq.3) then
!	            call get_uepath_hov(ji,i,icurrnt(ji))

!Modified by Jason Mar 27 2005 for MUC memory efficient implementation
c                 call get_genelink_from_uepath_lov(ji,genelink)
				if(igps.eq.1) then
				  call get_genelink_from_uepath_lov(ji,genelink) 
	            endif

	          elseif(vehclass(ji).eq.2) then
!	            call get_sopath_lov(ji,i,icurrnt(ji))

!Modified by Jason Mar 27 2005 for MUC memory efficient implementation
c                 call get_genelink_from_sopath_lov(ji,genelink) 
                  if(igps.eq.1) then
				  call get_genelink_from_sopath_lov(ji,genelink)
	            endif

	          endif
           else
	          if(vehclass(ji).eq.3) then
!                 call get_uepath_hov(ji,i,icurrnt(ji))

!Modified by Jason Mar 27 2005 for MUC memory efficient implementation
c                 call get_genelink_from_uepath_hov(ji,genelink) 
	            if(igps.eq.1) then
				  call get_genelink_from_uepath_hov(ji,genelink) 
	            endif

	          elseif(vehclass(ji).eq.2) then
!	            call get_sopath_hov(ji,i,icurrnt(ji))

!Modified by Jason Mar 27 2005 for MUC memory efficient implementation
c                 call get_genelink_from_sopath_hov(ji,genelink) 
	            if(igps.eq.1) then
				  call get_genelink_from_sopath_hov(ji,genelink) 
                  endif

	          endif
           endif


!          vlg(isec(ji))=vlg(isec(ji))+1

! Modified by MTI team jan 24 2004
		if(vehclass(ji).eq.2.or.vehclass(ji).eq.3) then ! We change generation links for SO or UE vehicles
!Modified by Jason Mar 27 2005 for MUC memory efficient implementation			
			!isec(ji) = genelink
	        if(igps.eq.1) then
			  isec(ji) = genelink

! need to reset the position of the vehicles to start at mid of generation links 
! to be consistent with 1-shot
! Hayssam and Jason for 1.0 April 2, 2004
			  xpar(ji) = s(isec(ji))/2 
	        endif
	    endif

! Added by Jason Mar 4 2005, 
! determine xpar of so and ue vehicles in ueassign or soassign
! while xpar of vehicles of the other classes is determined here
          if(vehclass(ji).ne.2.and.vehclass(ji).ne.3) then
	      xpar(ji) = s(isec(ji))/2
		endif
! End

		vlg(isec(ji))=vlg(isec(ji))+1
		vlg_vhcID(isec(ji),vlg(isec(ji))) = ji
	
          icurrnt(ji)=1
          if(stime(ji).ge.starttm.and.stime(ji).lt.endtm) then
            itag(ji)=1
            load_veh = load_veh + 1
	    else 
	      itag(ji)=0
	    endif

!**********************************************************************
!**********************************************************************
! added by Hayssam and Jason Jan 17 2006
! To re-define the impact status of each vehicle in subsequent iterations
		  if(inci_num.gt.0) then
	        ImpactType(ji)%InciMode(:) = 0
			do MA = 1, inci_num
			  icnt = 0
			  inode1 = nint(VhcAtt_Value(ji,icnt+1,1))
			  inode2 = nint(VhcAtt_Value(ji,icnt+2,1))
			  Nlnk = GetFLinkFromNode(inode1,inode2)
                
             do while(idnod(Nlnk).ne.destination(MasterDest(jdest(ji))))
	            icnt = icnt + 1 
			    inode1 = nint(VhcAtt_Value(ji,icnt+1,1))
			    inode2 = nint(VhcAtt_Value(ji,icnt+2,1))
	            Nlnk = GetFLinkFromNode(inode1,inode2)
                  if(Nlnk.eq.incil(MA)) then ! still on incident link	                
		          ImpactType(ji)%InciMode(MA) = 2
		          ImpactType(ji)%InciMode(inci_num+1) = 
     +		      ImpactType(ji)%InciMode(inci_num+1) +1
                  endif
	          enddo
	        enddo
	      endif

	      if(WorkZoneNum.gt.0) then
              ImpactType(ji)%WZMode(:) = 0
	        do MA = 1, WorkZoneNum
			  icnt = 0
	NWZLink = GetFLinkFromNode(WorkZone(MA)%FNode,WorkZone(MA)%TNode)
			  inode1 = nint(VhcAtt_Value(ji,icnt+1,1))
			  inode2 = nint(VhcAtt_Value(ji,icnt+2,1))
		      Nlnk = GetFLinkFromNode(inode1,inode2)
             do while(idnod(Nlnk).ne.destination(MasterDest(jdest(ji))))
	            icnt = icnt + 1 
			    inode1 = nint(VhcAtt_Value(ji,icnt+1,1))
			    inode2 = nint(VhcAtt_Value(ji,icnt+2,1))
	            Nlnk = GetFLinkFromNode(inode1,inode2)
                  if(Nlnk.eq.NWZLink) then ! still on incident link
				  ImpactType(ji)%WZMode(MA) = 2
				  ImpactType(ji)%WZMode(WorkZoneNum+1) = 
     +			  ImpactType(ji)%WZMode(WorkZoneNum+1) +1	
                  endif
	          enddo
	        enddo
	      endif
!**********************************************************************
!**********************************************************************

        end do

1920    jrestore=ji
        numcars = numcars + load_veh 
      endif

      if(l.eq.starttime) then
        if(itedex.gt.0.and.iteration.eq.0.and.
     +               (iso_ok.eq.1.or.iue_ok.eq.1)) then
!Modified by Jason Mar 27 2005 for MUC memory efficient implementation
c         call allocate_muc
		if(igps.eq.1) then
		  call allocate_muc_gps		  
	    else
	      call allocate_muc
		endif
!End
	  endif
	endif

c  -- output initial paths for muc every soint interval
      if(iteration.eq.0) then
!Potential inefficiency NOV 5 2003
      if((iso_ok.eq.1.or.iue_ok.eq.1).and.
     +   (itedex.gt.0.and.iteration.eq.0)) then
         if(mod((l-1),tad).eq.0) then
            soindex = nint(float(L)/tad)+1
	      if(soindex.le.soint) then

!Modified by Jason Mar 27 2005 for MUC memory efficient implementation
c             call build_mucpath_lov(soindex) !build muc path set
              if(igps.eq.1) call build_mucpath_lov(soindex) !build muc path set

!Modified by Xuesong and Hayssam Jan 22 2004 0.930.9
!  	        if(total_hov.gt.0.00001) then
  	        if(Veh_Type(3).gt.0.00001) then

!Modified by Jason Mar 27 2005 for MUC memory efficient implementation
c               call build_mucpath_hov(soindex) !build muc path set
                if(igps.eq.1) call build_mucpath_hov(soindex)!build muc path set

              endif
	      endif
	   endif
      endif
      endif

c -- vms_main is a subroutine for vms operation
c -- vms_num : is the number of vms.
c --
      if(vms_num.gt.0) call vms_main(t_start)

      call vehicle_simulation(l,t_start)
	 
!Added by Jing & Xuesong Mar 16 2004 Dynasmart 1.0
      call vehicle_location(l)

c -- Output the following statistics         
c -- l : the current simulation interval number.
c -- jj : total umber of vehicels loaded till now.
c -- numcars : number of vehicles still in the network.
c -- nout_nontag : number of non-tagged vehicles which reached their i
c --               destination.
c -- nout_tag : number of tagged vehicles which reached their destination. 

c -- STOP conditions: 1) if there are no vehicles in the network
c --                  2) The end planning horizone
c --                  3) there are no vehicles exits the network in 5 min
       
      if(realdm.eq.1.and.numcars.eq.0.and.l.gt.tlatest_bus/tii
     *    .and.l.gt.(starttm/tii+10)) then ! loading OD demand
c     *	 iread_veh_count.ge.MaxVehicles) then
      write(6,*) '**************************************************'
      write(6,*) 'The program reached the end of simulation because:'
      write(6,*) 'all target vehicles have reached their destinations'
      write(6,*) '**************************************************'
      write(666,*) '**************************************************'
      write(666,*) 'The program reached the end of simulation because:'
      write(666,*) 'all target vehicles have reached their destinations'
      write(666,*) '**************************************************'
	   	
      go to 433

! Added by MTI team April 6 2004 1.0.0
	else ! Loading vehicle.dat
! Modified Dec 8 2005
        if((iread_veh_count.ge.MaxVehicles.and.numcars.eq.0).or.
     *     (t_start.ge.endtm.and.numcars.eq.0)) then
      write(6,*) '**************************************************'
      write(6,*) 'The program reached the end of simulation because:'
      write(6,*) 'all target vehicles have reached their destinations'
      write(6,*) '**************************************************'
      write(666,*) '**************************************************'
      write(666,*) 'The program reached the end of simulation because:'
      write(666,*) 'all target vehicles have reached their destinations'
      write(666,*) '**************************************************'

      go to 433
	  endif
! end of modification		

      endif
c --
c -- if there are some so or ue vehicels, print out some files for their
c -- procedure(s).
c --
      if((iso_ok.eq.1.or.iue_ok.eq.1).and.itedex.gt.0) then
	   call calavg(L)
      endif

c -- ramp_metering is a subroutine to calculate 
c -- on-ramp rate on ramps.
c -- intime : the number of simulation intervals in 1 minute.
c --
       if(dec_num.gt.0) then
         intime=nint(1/tii) 
         if(mod(l,intime*nrate).eq.0.and.l.gt.5) then
           call ramp_metering(t_start)
         endif
       endif

c -- count the simulation intervals in which there are no vehicles going
c -- out of the network.
c -- If ther are some vehicles out during the current simulation interval, then
c -- reinitialize the counter.
c --
      if(numcars.eq.oldnumcars) icount_stop=icount_stop+1
      if(numcars.ne.oldnumcars) icount_stop=0
      oldnumcars=numcars
c --
c -- STOP check : if there are no vehicles getting out of the network for
c -- 50 simulation intervals and the end of the demand generation time
c -- has been reached, then stop.
c --
      if(icount_stop.eq.100.and.t_start.ge.begint(nints+1))	then
      write(6,*) '**************************************************'
      write(6,*) 'The program reached the end of simulation because:'
      write(6,*) 'there are no target vehicles getting out of network'
      write(6,*) 'for',100*tii, 'minutes' 
      write(6,*) '**************************************************'
      write(666,*) '**************************************************'
      write(666,*) 'The program reached the end of simulation because:'
      write(666,*) 'there are no target vehicles getting out of network'
      write(666,*) 'for',100*tii, 'minutes' 
      write(666,*) '**************************************************'
       
	go to 433
      endif

c --
c -- print out some loading information
c -- for every 50 simulation intervals.
c --
c -- jj_i : total number of vehicles generated in the netwrok since beginning of the simulation
c  --       till the end of the previous simulation interval.
c --
c -- all veriables with _i at the end are for the previous simulation interval.
c --

! Modified Jan 27 2006 output for the first simulation interval
c      if(mod(l,idemand_info).eq.0) then
	if(l.eq.1.or.mod(l,idemand_info).eq.0) then
         
! Modified by MTI team Jan 28 0.930.9
!	   num_gen=jj-jj_i
	  if(iteration.eq.0) then	
	   num_gen=jj-jj_i
	  else
	   num_gen=jj_MUC
	   jj_MUC = 0
	  endif

         nout_nontag_i=nout_nontag-nout_nontag_i
         nout_tag_i=nout_tag-nout_tag_i

! Modified by MTI team Jan 28 0.930.9
	IF(iteration.eq.0) THEN
! Modified Jan 27 2006 output vehicle loading information for the first sim interval      
	if(l.eq.1) then
	write(6,3411) 0.0,jj,
     +               num_gen,nout_nontag_i,nout_tag_i,numcars
	write(666,3411) 0.0,jj,
     +               num_gen,nout_nontag_i,nout_tag_i,numcars	
      else
	write(6,3411) l*tii,jj,
     +               num_gen,nout_nontag_i,nout_tag_i,numcars
	write(666,3411) l*tii,jj,
     +               num_gen,nout_nontag_i,nout_tag_i,numcars
      endif

	ELSE

      if(l.eq.1) then
	write(6,3411) 0.0,jrestore-1,
     +               num_gen,nout_nontag_i,nout_tag_i,numcars
	write(666,3411) 0.0,jrestore-1,
     +               num_gen,nout_nontag_i,nout_tag_i,numcars  	
      else	 
      write(6,3411) l*tii,jrestore-1,
     +               num_gen,nout_nontag_i,nout_tag_i,numcars
	write(666,3411) l*tii,jrestore-1,
     +               num_gen,nout_nontag_i,nout_tag_i,numcars
	endif

      ENDIF

!3411  format(' T: ',f5.1,' Tot Veh: ',I6,' Gen: ',i6
!     +              ,' Out_n: ',I6,' Out_t:',I6,' In_v:',I6)
! Modified by Xuesong June 1 2004 dynasmart 1.0
!3411  format(' T: ',f7.1,' Tot Veh: ',I7,' Gen: ',i6
!     +              ,' Out_n: ',I6,' Out_t:',I6,' In_v:',I7)
! Modified by Archak July 8 2016     
3411  format(' T:',f6.1,' Tot Veh: ',I7,' Gen: ',i6
     +              ,' Out_n: ',I6,' Out_t:',I6,' In_v:',I7)
         jj_i=jj
         nout_nontag_i=nout_nontag
         nout_tag_i=nout_tag
      endif


12    continue

c -- In the real time case, the CORBA code calls this subroutine every display
c -- interval, therefore, the gui_stat has to be called at the end of the loop.

433   continue

! Nov 28 2005 output a file for early termination
       open(file='sim_terminate.dat', unit=901, status='REPLACE',
     *      action='write')
       write(901, *) stagelength, L
	 close(901)
! End

! added by DTA team Nov 18 2004
! fill in the remaining intervals of penalty and travel time arrays 
! with the values of current interval           
      if(itedex.gt.1.and.L.lt.endtime) then
	  i_no_agg = nint((stagelength/tii)/ftr) +1	! num of agg intervals
	  do i=1,noofarcs
	    ip = ForToBackLink(i)
	    do j=1,nu_mv
	      curr_travel_penalty = TravelPenalty(ip,curr_agg_interval,j)
	      curr_openaltyMG = openaltyMG(i,curr_agg_interval,j)
	      do iagg = curr_agg_interval+1, i_no_agg
	        TravelPenalty(ip,iagg,j) = curr_travel_penalty
	        openaltyMG(i,iagg,j) = curr_openaltyMG
	      enddo
          enddo

	    if(iso_ok.eq.1) then ! so marginals calculation
	      do j=1, nu_mv
	        curr_openaltyMG_sim = openaltyMG_sim(i,L,j)
	        do it= L+1, numof_siminterval
	          openaltyMG_sim(i,it,j) = curr_openaltyMG_sim
	        enddo
	      enddo
	    endif

        enddo
      endif
! end

      MaxIntervals = min(L,endtime)

	call gui_stat(l)

      INQUIRE(FILE='LinkFlowPOutput.Txt', EXIST=LPFileExists)
	if(LPFileExists) then
	!Modification: 10182013
      !call PrintLinkProportions(starttime,endtime,0)
          if(varyBlockSize) then
          !Allow grouping origins during the link proportion calculation --> New subroutine modified by Omer
            if(iteration.eq.itedex) then
             call PrintLinkProportionsVaryBlockSize(starttime,endtime,0)
            endif
          else
          !default setting --> original linkProportion subroutine
            call PrintLinkProportions(starttime,endtime,0)
          endif
      !Modification: 10182013
	endif
    
      INQUIRE(FILE='LinkDensityPOutput.Txt', EXIST=LPFileExists)
	if(LPFileExists) then
	!Modification: 10182013
      !call PrintLinkProportions(starttime,endtime,1)
          if(varyBlockSize) then
          !Allow grouping origins during the link proportion calculation --> New subroutine modified by Omer
            if(iteration.eq.itedex) then
             call PrintLinkProportionsVaryBlockSize(starttime,endtime,1)
            endif
          else
          !default setting --> original linkProportion subroutine
             call PrintLinkProportions(starttime,endtime,1)
          endif
      !Modification: 10182013
	endif

      end subroutine


	
