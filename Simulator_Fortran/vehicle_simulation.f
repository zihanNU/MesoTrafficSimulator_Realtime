      subroutine vehicle_simulation(l,t)
c --
c -- This subroutine is the main subroutine for the vehicle simulation part
c -- of DYNASMART.
c --
c -- This subroutine is called from loop every simulation interval.
c -- This subroutine calls the following subroutines
c --	a. vehicle_loading
c --  b. vehicle_moving
c --	c. vehicle_transfer
c --	d. link_performance
c --
c -- INPUT :
c --  l : the current simualtion interval number.
c --  t: current clock time
c --
      use muc_mod
 
c --
      tend=t+tii
c  --
c  -- tend: the end of this interval
c  --
c -- vehicle loading
c --

c  -- Add by ZIhan and Archak on 20160325 for reversible lane
        if (RevLane_flag) then
            call reversible_lane (l)
        endif
c -- end of addition   




! Added May 26 2005
c      call vehicle_loading(t)
      if(iSequentialLoad.eq.1.and.iteration.eq.0) then
	  if(l.eq.1.or.(mod(l,kspstep).eq.0.and.tend.lt.begint(nints+1)))
     +  then ! Jan 26 2006
          call vehicle_loading_seq(t)
	  endif
      else
        call vehicle_loading(t)     	
	endif
! End
        
c  --
c  -- vehicle moving
c  --
	call vehicle_moving(t)

c  -- vehicle transfer
c  --
      call vehicle_transfer(l,t,tend)

c --  update the speed in traffic flow model
      call flow_model_update (t)
		
c --
c --  call penalty_calculation to update the penlaties.
c --
      call penalty_calculation(l)
c  --
c  -- output link information
c  --
      call link_performance(l,t,tend)
c  --
      return
      end



