      subroutine vms_main(t_start)
c --
c -- This is the main subroutine for VMS operations.
c --
c -- This subroutine is called from loop every simulation interval
c -- This subroutine calls the following subroutines :
c -- 	a. vms_speed
c --	b. vms_path  
c --  c. vms_divert
c -- 
c -- INPUT : 
c -- the current clock time (through common blocks)
c --
c -- OUTPUT : 
c -- No specific output (it just manages the VMS files)
c --
      use muc_mod
c --
c --
c -- There are three different types of VMS :
c -- 1. speed reduction
c -- 2. assign a specific path
c -- 3. divert vehciles to other paths
c -- assumption :
c -- 1. the position of vms is known
c -- 2. the time of operation is known
c --
      if(vms_num.gt.0) then 
     
c--    Modified by Zihan on 20160427 for AMS shoulder lane
        if (ShlLane_flag) then
            do m=1,noofplans_ShlLane-1
                if(t_start.eq.SE_ShLane(m,2)) then
                    do i=1,noofarcs_ShlLane
                        vms_start(i)=SE_ShLane(m+1,1)
                        vms_end(i)=SE_ShLane(m+1,2)
                    enddo
                endif
            enddo
        endif
! c--   End of Modification
 
 
      
      
      
       
        do i=1,vms_num
          if(t_start.ge.vms_start(i).and.
     +       t_start.lt.vms_end(i)) then
            if(vmstype(i).eq.1) then
		      call vms_speed(i)
            elseif(vmstype(i).eq.5) then
              call vms_weather_speed(i)
	      elseif(vmstype(i).eq.7) then
              call vsl(i,t_start)
            endif
           elseif(t_start.ge.vms_end(i).and.vmstype(i).eq.7) then ! need to restore original speed limit after VSL is deactivated
             SpeedLimit(vms(i,1)) = SpeedLimit_org(vms(i,1))
          endif
        end do
      endif
c --
      return
      end
