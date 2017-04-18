      subroutine link_performance(l,t,tend)      
c --
c -- This subroutine prints out the link performance information every 
c -- several simulation intervals (defined by the user for each file)
c -- and calculates the link entry time for each link.
c -- It also prints out the information for the GUI updates
c --
c -- This subroutine is called from vehicle_simulation.
c -- This subroutine does not call any other subroutines
c --
c -- INPUT :
c --   l : current simulation interval
c --   t : starting clock time of the current simulation interval
c --   tend : ending clock time of the current simulation interval.
c --
c -- OUPTPUT :
c -- Fort.30 through Fort.39 (optional output files)
c -- and fort.600 fort.700 fort.800 fort.900
c --
      use muc_mod
	use vector_mod
      use LinkList_mod
c     the following temporary arrays is used for the purpose of storing statistics
c     for the actual (physical and not virtual) links
c     we had to introduce these temporary arrays to be able to write the statistics
c     using the format we had earlier.  Otherwise, each link statistic will be
c     written on a separate line.


	real, allocatable :: tmparr2(:)
	integer tmpindex
	integer error
      type(linkstruct),pointer  :: pass
      allocate(pass)

	allocate (tmparr2(noofarcs),stat=error)
 	if(error.ne.0) then
	  write(911,*) 'allocate tmparr2 error - insufficient memory'
	  stop
	endif
	tmparr2(:)=0
	

c	tmpindex is used to represent the actual number of physical links
c     that is, we don't count the virtual links
 
	tmpindex = 0

c --
c -- calculate the link entry time (average delay on the entry link).
c --
      do i=1,noofarcs
        if(entryrate(i).gt.0) then
! fixed by hayssam june 28 2005
!         link_entry_time(i)=link_entry_queue(i)/entryrate(i)
	   link_entry_time(i)=ntryq(i)/entryrate(i)
	  else
         link_entry_time(i)=99
        endif 
      end do
c --
c -- for the purpose of post data analysis
c -- some link information will be stored in
c -- different files (optional according to the user's choice in
c -- the DYNASMARTf Optional Output files screen).
c --         
c -- fort.30 : generation volume
c -- fort.31 : volume on links
c -- fort.32 : vehicle_queue
c -- fort.33 : v(i)
c -- fort.34 : c(i)
c -- fort.35 : vtmp(i)
c -- fort.36 : ctmp(i)
c -- fort.37 : outleft(i)
c -- fort.38 : green(i)
c -- fort.39 : outflow(i)
c -- fort.40 : AccuVol(i)

!	if(i30.gt.0) open(file='OutLinkGen.dat',unit=30,status='unknown')
!	if(i31.gt.0) open(file='OutLinkVeh.dat',unit=31,status='unknown')
!	if(i32.gt.0) open(file='OutLinkQue.dat',unit=32,status='unknown')
!	if(i33.gt.0) open(file='OutLinkSpeedAll.dat',unit=33,status='unknown')
!	if(i34.gt.0) open(file='OutLinkDent.dat',unit=34,status='unknown')
!	if(i35.gt.0) open(file='OutLinkSpeedFree.dat',unit=35,status='unknown')
!	if(i36.gt.0) open(file='OutLinkDentFree.dat',unit=36,status='unknown')
!	if(i37.gt.0) open(file='OutLeftFlow.dat',unit=37,status='unknown')
!	if(i38.gt.0) open(file='OutGreen.dat',unit=38,status='unknown')
!	if(i39.gt.0) open(file='OutFlow.dat',unit=39,status='unknown')
!	if(i40.gt.0) open(file='AccuVolume.dat',unit=40,status='unknown')




c -- Fort.30
c --
      tmparr2(:) = 0
      if(i30.eq.1) then
        do i=1,noofarcs
          tmp30(i)=tmp30(i)+vlg(i)
        enddo
c --
!	  if(l.gt.1) then
         if(mod(l,i30_t).eq.0) then
	    tmpindex = 0
	    do i = 1, noofarcs
	      if (link_iden(i) < 99) then

!	        tmpindex = tmpindex + 1
! Changed by Xuesong August 28, 2003 DYNASMART-P 0.930.7
			tmpindex = OriginLinkIndex(i)

	        tmparr2(tmpindex) = tmp30(i)
	      endif
	    enddo

          write(30,*)
          write(30,330) l*tii
          write(30,3101) (tmparr2(i)/i30_t,i=1,noofarcs_org) ! May 25 2006
	    write(30,*)
          tmp30(:)=0
         endif
!        else
!	   tmpindex = 0
!	   do i = 1, noofarcs
!	     if (link_iden(i) < 99) then
!	       tmpindex = tmpindex + 1
!	       tmparr2(tmpindex) = tmp30(i)
!	     endif
!	   enddo
!
!	   write(30,*)
!         write(30,330) T
!	   write(30,3101) (tmparr2(i),i=1,tmpindex)
!	   write(30,*)
!         tmp30(:)=0
!        endif
      endif
c --
c -- Fort.31
c --
      tmparr2(:) = 0
!      if(i31.eq.1) then
        do i=1,noofarcs
          tmp31(i)=tmp31(i)+volume(i)
        enddo
        
	  if(l.gt.1) then
        if(mod(l,i31_t).eq.0) then
	    tmpindex = 0
	    do i = 1, noofarcs
	      if (link_iden(i) < 99) then
!	        tmpindex = tmpindex + 1
! Changed by Xuesong August 28, 2003 DYNASMART-P 0.930.7
			tmpindex = OriginLinkIndex(i)

	        tmparr2(tmpindex) = tmp31(i)
	      endif
	    enddo

          write(31,*)
          write(31,330) l*tii
          write(31,3101) (tmparr2(i)/i31_t,i=1,noofarcs_org) ! May 25 2006
	    write(31,*)
          tmp31(:)=0
        endif
!        else
!	    tmpindex = 0
!	    do i = 1, noofarcs
!	      if (link_iden(i) < 99) then
!	        tmpindex = tmpindex + 1
!	        tmparr2(tmpindex) = tmp31(i)
!	      endif
!	    enddo
!
!          write(31,*)
!          write(31,330) T
!	    write(31,3101) (tmparr2(i),i=1,tmpindex)
!	    write(31,*)
!          tmp31(:)=0
!        endif
      endif
3101  format(10f8.2)
c --
c -- Fort.32 
c --
      tmparr2(:) = 0
      if(i32.eq.1) then
        do i=1,noofarcs
	    tmp32(i)=tmp32(i)+vehicle_queue(i)
        enddo

!        if(l.gt.1) then
        if(mod(l,i32_t).eq.0) then
          tmpindex = 0
	    do i = 1, noofarcs
	      if (link_iden(i) < 99) then
!	        tmpindex = tmpindex + 1
! Changed by Xuesong August 28, 2003 DYNASMART-P 0.930.7
			tmpindex = OriginLinkIndex(i)

	        tmparr2(tmpindex) = tmp32(i)
	      endif
	    enddo

          write(32,*)
          write(32,330) l*tii
	    write(32,3201) (tmparr2(i)/i32_t,i=1,noofarcs_org) ! May 25 2006
          write(32,*)
          tmp32(:)=0
        endif
!        else
!          tmpindex = 0
!	    do i = 1, noofarcs
!	      if (link_iden(i) < 99) then
!	        tmpindex = tmpindex + 1
!	        tmparr2(tmpindex) = tmp32(i)
!	      endif
!	    enddo
!
!          write(32,*)
!          write(32,330) T
!          write(32,3201) (tmparr2(i),i=1,tmpindex)
!          write(32,*)
!          tmp32(:)=0
!        endif
      endif
3201  format(10f8.2)
c --
c -- Fort.33 
c --
      tmparr2(:) = 0
      if(i33.eq.1) then
        do i=1,noofarcs
	    tmp33(i)=tmp33(i)+v(i)
        enddo
      
!	  if(l.gt.1) then
        if(mod(l,i33_t).eq.0) then
          tmpindex = 0
	    do i = 1, noofarcs
	      if (link_iden(i) < 99) then
!	        tmpindex = tmpindex + 1
! Changed by Xuesong August 28, 2003 DYNASMART-P 0.930.7
			tmpindex = OriginLinkIndex(i)

	        tmparr2(tmpindex) = tmp33(i)
	      endif
	    enddo

	    write(33,*)
          write(33,330) l*tii
	    write(33,3301) (tmparr2(i)/i33_t*60.0,i=1,noofarcs_org) ! May 25 2006
          write(33,*)
          tmp33(:)=0
        endif
!        else
!      	tmpindex = 0
!	    do i = 1, noofarcs
!	      if (link_iden(i) < 99) then
!	        tmpindex = tmpindex + 1
!	        tmparr2(tmpindex) = tmp33(i)
!	      endif
!	    enddo
!
!          write(33,*)
!          write(33,330) T
!	    write(33,3301) (tmparr2(i)*60.0,i=1,tmpindex)
! 		write(33,*)
!          tmp33(:)=0
!        endif
      endif    
3301  format(10f8.2)
330   format(f8.1)
c --
c -- Fort.34 
c --
      tmparr2(:) = 0
      if(i34.eq.1) then
        do i=1,noofarcs
         tmp34(i)=tmp34(i)+c(i)
        enddo

!        if(l.gt.1) then
        if(mod(l,i34_t).eq.0) then
	    tmpindex = 0
	    do i = 1, noofarcs
	      if (link_iden(i) < 99) then
!	        tmpindex = tmpindex + 1
! Changed by Xuesong August 28, 2003 DYNASMART-P 0.930.7
			tmpindex = OriginLinkIndex(i)

	        tmparr2(tmpindex) = tmp34(i)
	      endif
	    enddo

	    write(34,*)
		write(34,330) l*tii
	    write(34,3401) (tmparr2(i)/i34_t,i=1,noofarcs_org) ! May 25 2006
		write(34,*)
          tmp34(:)=0
        endif
!        else
!	    tmpindex = 0
!	    do i = 1, noofarcs
!	      if (link_iden(i) < 99) then
!	        tmpindex = tmpindex + 1
!	        tmparr2(tmpindex) = tmp34(i)
!	      endif
!	    enddo
!
!	    write(34,*)
!          write(34,330) T
!	    write(34,3401) (tmparr2(i),i=1,tmpindex)
!      	write(34,*)
!          tmp34(:)=0
!        endif
      endif
3401  format(10f8.2)
c --
c -- Fort.35 
c --
      tmparr2(:) = 0
      if(i35.eq.1) then
        do i=1,noofarcs
          tmp35(i)=tmp35(i)+vtmp(i)
        enddo
       
!	  if(l.gt.1) then
        if(mod(l,i35_t).eq.0) then
	    tmpindex = 0
	    do i = 1, noofarcs
	      if (link_iden(i) < 99) then
!	        tmpindex = tmpindex + 1
! Changed by Xuesong August 28, 2003 DYNASMART-P 0.930.7
			tmpindex = OriginLinkIndex(i)

	        tmparr2(tmpindex) = tmp35(i)
	      endif
	    enddo
	 
	    write(35,*)
          write(35,330) l*tii
    	    write(35,3501) (tmparr2(i)/i35_t*60.0,i=1,noofarcs_org) ! May 25 2006
	    write(35,*)
          tmp35(:)=0
        endif
!        else
!	    tmpindex = 0
!	    do i = 1, noofarcs
!	      if (link_iden(i) < 99) then
!	        tmpindex = tmpindex + 1
!	        tmparr2(tmpindex) = tmp35(i)
!	      endif
!	    enddo
!        
!	    write(35,*)
!	    write(35,330) T
!	    write(35,3501) (tmparr2(i)*60.0,i=1,tmpindex)
!	    write(35,*)
!	    tmp35(:)=0
!	  endif
      endif
3501  format(10f8.2)
c -- 
c -- Fort.36 
c --
      tmparr2(:) = 0
      if(i36.eq.1) then
        do i=1,noofarcs
          tmp36(i)=tmp36(i)+ctmp(i)
        enddo

!        if(l.gt.1) then
        if(mod(l,i36_t).eq.0) then
	    tmpindex = 0
	    do i = 1, noofarcs
	      if (link_iden(i) < 99) then
!	        tmpindex = tmpindex + 1
! Changed by Xuesong August 28, 2003 DYNASMART-P 0.930.7
			tmpindex = OriginLinkIndex(i)
	        tmparr2(tmpindex) = tmp36(i)
	      endif
	    enddo

	    write(36,*)
          write(36,330) l*tii
		write(36,3601) (tmparr2(i)/i36_t,i=1,noofarcs_org) ! May 25 2006
          write(36,*)
          tmp36(:)=0
        endif
!        else
!	    tmpindex = 0
!	    do i = 1, noofarcs
!	      if (link_iden(i) < 99) then
!	        tmpindex = tmpindex + 1
!	        tmparr2(tmpindex) = tmp36(i)
!	      endif
!	    enddo
!
!	    write(36,*)
!          write(36,330) T
!          write(36,3601) (tmparr2(i),i=1,tmpindex)
!          write(36,*)
!          tmp36(:)=0
!        endif
      endif
3601  format(10f8.2)
c --
c -- Fort.37
c --
      tmparr2(:) = 0
      if(i37.eq.1) then
        do i=1,noofarcs
          tmp37(i)=tmp37(i)+outleft(i)
        enddo

!        if(l.gt.1) then
        if(mod(l,i37_t).eq.0) then
	    tmpindex = 0
	    do i = 1, noofarcs
	      if (link_iden(i) < 99) then
!	        tmpindex = tmpindex + 1
! Changed by Xuesong August 28, 2003 DYNASMART-P 0.930.7
			tmpindex = OriginLinkIndex(i)
	        tmparr2(tmpindex) = tmp37(i)
	      endif
	    enddo

	    write(37,*)
          write(37,330) l*tii
          write(37,3701) (tmparr2(i)/i37_t,i=1,noofarcs_org) ! May 25 2006
		write(37,*)
          tmp37(:)=0
        endif
!        else
!	    tmpindex = 0
!	    do i = 1, noofarcs
!	      if (link_iden(i) < 99) then
!	        tmpindex = tmpindex + 1
!	        tmparr2(tmpindex) = tmp37(i)
!	      endif
!	    enddo
!
!	    write(37,*)
!          write(37,330) T
!          write(37,3701) (tmparr2(i),i=1,tmpindex)
!		write(37,*)
!          tmp37(:)=0
!        endif
      endif
3701  format(10f8.2)
c --
c -- Fort.38 : green time allocation
c --
      tmparr2(:) = 0
      if(i38.eq.1) then
        do i=1,noofarcs
          tmp38(i)=tmp38(i)+green(i,2)
        enddo

        if(mod(l,i38_t).eq.0) then
	    tmpindex = 0
	    do i = 1, noofarcs
	      if (link_iden(i) < 99) then
!	        tmpindex = tmpindex + 1
! Changed by Xuesong August 28, 2003 DYNASMART-P 0.930.7
			tmpindex = OriginLinkIndex(i)
	        tmparr2(tmpindex) = tmp38(i)
	      endif
	    enddo

	    write(38,*)
          write(38,330) l*tii
	    write(38,3801) (tmparr2(i),i=1,noofarcs_org) ! May 25 2006
          write(38,*)
          tmp38(:)=0
        endif
      endif
3801  format(10f8.2)
c --
c -- Fort.39
c --
      tmparr2(:) = 0
      if(i39.eq.1) then
        do i=1,noofarcs
          tmp39(i)=tmp39(i)+outflow(i)
        enddo

!        if(l.gt.1) then
        if(mod(l,i39_t).eq.0) then
	    tmpindex = 0
	    do i = 1, noofarcs
	      if (link_iden(i) < 99) then
!	        tmpindex = tmpindex + 1
! Changed by Xuesong August 28, 2003 DYNASMART-P 0.930.7
			tmpindex = OriginLinkIndex(i)
	        tmparr2(tmpindex) = tmp39(i)
	      endif
	    enddo

      	write(39,*)
		write(39,330) l*tii
  		write(39,3901) (tmparr2(i)/i39_t,i=1,noofarcs_org) ! May 25 2006
		write(39,*)
          tmp39(:)=0
        endif
!        else
!	    tmpindex = 0
!	    do i = 1, noofarcs
!	      if (link_iden(i) < 99) then
!	        tmpindex = tmpindex + 1
!	        tmparr2(tmpindex) = tmp39(i)
!	      endif
!	    enddo
!
!	    write(39,*)
!          write(39,330) T
!          write(39,3901) (tmparr2(i),i=1,tmpindex)
!          write(39,*)
!          tmp39(:)=0
!        endif
      endif
3901  format(10f8.2)


c --
c -- Fort.40
c --
      tmparr2(:) = 0
      if(i40.eq.1) then

!        if(l.gt.1) then
! Modified by Xuesong and Hayssam on March 16 2004 for DYNA 1.0
! Need not read i39_t for OutAccuVol.dat
!        if(mod(l,i39_t).eq.0) then
         if(mod(l,i40_t).eq.0) then

	    tmpindex = 0
	    do i = 1, noofarcs
	      if (link_iden(i) < 99) then
!	        tmpindex = tmpindex + 1
! Changed by Xuesong August 28, 2003 DYNASMART-P 0.930.7
			tmpindex = OriginLinkIndex(i)
	        tmparr2(tmpindex) = AccuVol(i)
	      endif
	    enddo
      	write(40,*)
		write(40,330) l*tii
  		write(40,4001) (tmparr2(i),i=1,noofarcs_org)
		write(40,*)
        endif
!        else
!	    tmpindex = 0
!	    do i = 1, noofarcs
!	      if (link_iden(i) < 99) then
!	        tmpindex = tmpindex + 1
!	        tmparr2(tmpindex) = tmp39(i)
!	      endif
!	    enddo
!
!	    write(39,*)
!          write(39,330) T
!          write(39,3901) (tmparr2(i),i=1,tmpindex)
!          write(39,*)
!          tmp39(:)=0
!        endif
      endif
4001  format(10f10.2)

c -- Fort.29 LinkVolume
c --
      tmparr2(:) = 0
        if(mod(l,i39_t).eq.0) then
	    tmpindex = 0
	    do i = 1, noofarcs
	      if (link_iden(i) < 99) then
!	        tmpindex = tmpindex + 1
! Changed by Xuesong August 28, 2003 DYNASMART-P 0.930.7
			tmpindex = OriginLinkIndex(i)
	        tmparr2(tmpindex) = LinkVolume(i)
	      endif
	    enddo
      	write(29,*)
		write(29,330) l*tii
  		write(29,4001) (tmparr2(i),i=1,noofarcs_org)
		write(29,*)
	LinkVolume(:) =0

        endif


c --
c --  produce the information for the gui every int_d simaulation intervals. 
c --
      if(mod(l,int_d).eq.0) then
c --
c --
c -- fort.900,600,700 are used to display the network condition at
c --  the end of the simulation (post execution GUI). These files include
c --  all the data for the whole simulation horizon (every int_d).
c --  
c -- Fort.600 : the vehicle queue as a proportion of the link length every int_d
c --            for the whole simulation horizon.
c -- 
c -- 
	tmpindex = 0 
	do i = 1, noofarcs
	  if (link_iden(i) < 99) then
!	        tmpindex = tmpindex + 1
! Changed by Xuesong August 28, 2003 DYNASMART-P 0.930.7
			tmpindex = OriginLinkIndex(i)
	 
!	 tmparr2(tmpindex)=(vehicle_queue(i)*Vehicle_length/5280.0)/xl(i)

	    tmparr2(tmpindex)=(vehicle_queue_PCE(i)*Vehicle_length/5280.0)
     +	/xl(i)     	

! Added Jan 30 2006 if there is a blocked link, link queue may exceed 100%
          if(tmparr2(tmpindex).ge.1.0) then
	      tmparr2(tmpindex) = 1.0
		endif

	  endif
      enddo

      write(600,*)
      write(600,330) l*tii
	write(600,3401) (tmparr2(i),i=1,noofarcs_org)
      write(600,*)
c --
c -- Fort.700 : the link density every int_d for the whole simulation horizon.
c --
      tmpindex = 0
	do i = 1, noofarcs
	  if (link_iden(i) < 99) then
!	        tmpindex = tmpindex + 1
! Changed by Xuesong August 28, 2003 DYNASMART-P 0.930.7
			tmpindex = OriginLinkIndex(i)

		! modified by Hayssam and Jason April 19, 2004 DYNA-P 1.0
		! if the lane miles is 0 (or severity is greater than 90%)

	    !tmparr2(tmpindex) = npar(i)/xl(i)

	 if(xl(i)/(nlanes(i)*s(i)).le.0.1) then
	    tmparr2(tmpindex) = 0
	 else 
         tmparr2(tmpindex) = partotal(i)/xl(i)
!added by Hayssam on Oct 5 2005
!need to restrict the max density for display on GUI to 250
         if(tmparr2(tmpindex).gt.250) tmparr2(tmpindex) = 250
	 endif
 
	  endif
	enddo

      write(700,*)
      write(700,330) l*tii
	write(700,3401) (tmparr2(i),i=1,noofarcs_org)
      write(700,*)
c --
c -- Fort.900 : the link speed every int_d for the whole simulation horizon.
c --

! Jan 11 2006 if type 1 VMS in effect (speed_flag=0) then output link speed in vms_speed
      speed_flag = 1
	if(vms_num.gt.0) then  
        do i=1,vms_num
          if(t.ge.vms_start(i).and.
     +      t.lt.vms_end(i)) then
            if(vmstype(i).eq.1) then 
	        speed_flag = 0
	      endif
	    endif
	  enddo
	endif
		  		  
      if(speed_flag.eq.1) then

	tmpindex = 0
	do i = 1, noofarcs
	  if (link_iden(i) < 99) then
	
!	        tmpindex = tmpindex + 1
! Changed by Xuesong August 28, 2003 DYNASMART-P 0.930.7
			tmpindex = OriginLinkIndex(i)
	    tmparr2(tmpindex) = v(i)
	  endif
	enddo

      write(900,*)
      write(900,330) l*tii
	write(900,3301) (tmparr2(i)*60.0,i=1,noofarcs_org)
     	write(900,*)
	
	endif
c --
c -- end of the gui data
c --
      endif
c --



!Add by Zihan 20160617, for test only
      !if (((time_now/60-int(time_now/60).eq.0.1)
    ! * .AND.(time_now.ge.300)).AND.(mod(time_now,300.0).eq.0))  then	
     
!      if ((time_now/60-int(time_now/60).eq.0.1)
!     * .AND.(time_now.ge.300))  then
!     	
!		!if (time_AMS.gt.10 .AND. time_AMS.lt.1200) then
!!		    call output_td_time_AMS(time_AMS)
!
!           ! write(SkimText,'(i4)') (nint(time_now/60))
!    
!!  
!	      OPEN(7011,FILE='new_travel_time.dat',STATUS='UNKNOWN')
!	      OPEN(7021,FILE='new_travel_penalty.dat',STATUS='UNKNOWN')
!            itt=int(time_now/60)
!	      do i=1, noofarcs
!	          if (link_iden(i).lt.99) then
!                    ib = ForToBackLink(i)
!                    !write(7011,*) ib,int(time_now/60), TTimeOfBackLink(ib)
!
!    	             write(7011,*) i,int(time_now/60), TTime(ib,1)  
!    	          endif 
!    	   
!    	          do iinbound=1, inlink(i,nu_mv+1)
!				    inboundlink = inlink(i,iinbound)
!				    if (link_iden(inboundlink).lt.99) then
!				    !write(7021,*) ib,itt,iinbound, TTPenalty(ib,1,iinbound)
!				    !write(7021,*) ib,int(time_now/60),iinbound, penalty(ib,iinbound)
!				 write(7021,*) i,int(time_now/60),iinbound, TTPenalty(ib,1,iinbound)
!                    endif 
!                enddo
!            enddo
!        !write (7011,*) 
!        !write (7021,*) 
!!
!        !close(7011)
!	 ! close(7021)
!
!!            endif
!        endif
!		
!End Addition



c -- reinitialize outflow and outleft arrays
c --
      outflow(:)=0
      outleft(:)=0
	
c --
c --
      deallocate(tmparr2)

      deallocate(pass)
      return
      end 

