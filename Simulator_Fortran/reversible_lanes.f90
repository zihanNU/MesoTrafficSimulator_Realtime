    Subroutine reversible_lane (starttime)
! Reversible Lane Module 
! Authors: Archak Mittal, Zihan Hong
! Created on Feb 28, 2016

use muc_mod

real:: clearance_time = 0
integer:: clearance_vehicle

!! 		read(49,*,iostat=error) vmstype(i),i1,i2,vms(i,2),vms(i,3),vms_start(i),vms_end(i)

if (direction_flag.NE.0) then
     
        if (direction_flag.EQ.1) then
                
           clearance_time = 0     
           do ii = 1, noofarcs_Inbound 
                clearance_time =  clearance_time + 1.1 * (TTime(links_for_Inbound(ii),starttime)) ! calculate clearance time
           end do
            clearance_time = int(clearance_time/tii)
            
             if ((schedule_Outbound(1).LE.(starttime + clearance_time)).AND.(schedule_Inbound(2).GT.(starttime + clearance_time))) then      
                 
                 !!  close Inbound                
                 !!  VMS of type 2 for all direction 1 on-ramps and off-ramps in direction 1 
                 
                 do kk = 1, (noofarcs_Inbound_ramp)
                    vms_start(kk+vms_num) = starttime
                 end do
                                  
                 direction_flag = 0        
             end if

        endif
        
        if (direction_flag.EQ.2) then
    
            clearance_time = 0 
           do ii = 1, noofarcs_Outbound 
                clearance_time =  clearance_time + 1.1 * (TTime(links_for_Outbound(ii),starttime)) ! calculate clearance time
           end do
            clearance_time = int(clearance_time/tii)
            
                if ((schedule_Outbound(1).GT.(starttime + clearance_time)).OR.(schedule_Inbound(2).LE.(starttime + clearance_time))) then
                
                !! Close Outbound
                !!  VMS of type 2 for all outbound on-ramps and off-ramps (Flush vehicles)
                do kk = 1, (noofarcs_Outbound_ramp)
                    vms_start(kk+vms_num+noofarcs_Inbound_ramp) = starttime
                 end do
                direction_flag = 0
             endif

        endif
endif

if (direction_flag.EQ.0) then

    if ((schedule_Outbound(1).GT.starttime).OR.(schedule_Inbound(2).LE.starttime)) then
         !! this is the time to open inbound
         !! VMS of type 2 for all outbound on-ramps and off-ramps (Flush vehicles)
                  
         clearance_vehicle = 0
         do ii = 1,noofarcs_Outbound
            clearance_vehicle = clearance_vehicle + volume(links_for_Inbound(ii))
         end do
        ! check total number of direction 2 bound vehicles
              
        if (clearance_vehicle.EQ.0) then
            direction_flag = 1
            do j = 1, noofarcs_Inbound_ramp 
              !   update VMS to Open inbound on-ramps
              vms_end(vms_num+j) =    min (starttime, vms_end(vms_num+j))                      
            end do
            do j = 1, noofarcs_Outbound_ramp
              !   update VMS to Open outbound on-ramps
                vms_start(vms_num+j+noofarcs_Inbound_ramp)= min(starttime, vms_start(vms_num+j+noofarcs_Inbound_ramp))
                vms_end(vms_num+j+noofarcs_Inbound_ramp)= starttime+1440
            end do            
        end if

    endif
    
    if ((schedule_Outbound(1).LE.starttime).AND.(schedule_Inbound(2).GT.starttime)) then

         !! this is the time to open outbound
         !!  VMS of type 2 for all inbound on-ramps and off-ramps (Flush vehicles)
         
         clearance_vehicle = 0
         do ii = 1,noofarcs_Inbound
            clearance_vehicle = clearance_vehicle + volume(links_for_Outbound(ii))
         end do
        ! check total number of Inbound vehicles
              
        if (clearance_vehicle.EQ.0) then
            direction_flag = 2
            do j = 1, noofarcs_Outbound_ramp
              !   update VMS to Open outbound on-ramps
                vms_end(vms_num+j+noofarcs_Inbound_ramp)= min(starttime, vms_end(vms_num+j+noofarcs_Inbound_ramp))
            end do
            do j = 1, noofarcs_Inbound_ramp
              !   update VMS to Open outbound on-ramps
                vms_start(vms_num+j)= min(starttime, vms_start(vms_num+j))
                vms_end(vms_num+j)= starttime+1440
            end do
        end if
        
    endif
        
endif

end subroutine