    Subroutine flow_model_update (t_start)

! =========================================================  
! --  $$$  Last update                                   ==
! --  Dates:   July 31                                   ==
! --  Authors: Yi-Chang Chiu                             == 
! --  Tasks:   Add Modified Greenshield data structure   ==
! --           Apply different Modified Greenshield      ==
! --           to different links                        ==
! =========================================================
! Modified Feb 10 2009 Weather Impacc, LinkWAF(:,:)
	  use muc_mod
      real tlength
      integer IQ, WZID, iflagWZ

      do 40 i=1,noofarcs

        ctmp(i)=0.0
        vtmp(i)=0.0

		! modified by Hayssam and Jason April 19, 2004 DYNA-P 1.0
		! if the lane miles is 0 (or severity is greater than 90%)
        !c(i)=(partotal(i))/xl(i)

      if(xl(i)/(nlanes(i)*s(i)).le.0.1) then
	    c(i) = 0
	  else 
        c(i)=(partotal(i))/xl(i)
	  endif


  
			
! --
! -- tlength is the queue-free lane length of the link.
! --
         tlength=xl(i)-(vehicle_queue(i)*vehicle_length/(5280.0))
! --

!modified by Hayssam on May 3 to output density in pc/mile/lane
! DYNA-P 1.0
!		 ctmp(i)=(volume(i)-vehicle_queue(i))/tlength
		 
		 ctmp(i)=(partotal(i)-vehicle_queue_PCE(i))/tlength

				 

! -- 
! -- prepare for the concentration in two ways
! -- c(i) : the one will be used in the simulation
! -- ctmp(i) : for short term prediction
! --
         c(i)   =min(c(i),cmax(i))
         ctmp(i)=min(ctmp(i),cmax(i))

! -- calculatc v the average speed

         IQ = FlowModelType(FlowModelnum(i))
		 IH = FlowModelnum(i)
! The speed updating mechanism is different for type 1 link and type 2
         if(c(i).le.MGreenS(IH)%KCut*linkWAF(i,3)) then !free-flow regime
         !Weather June 7 2011
         ! v(i)= (SpeedLimit(i)+Vfadjust(i)*linkWAF(i,8))/60.0
   		    v(i)= ((SpeedLimit(i)+Vfadjust(i))*linkWAF(i,8))/60.0
         ! end Weather June 7 2011
         elseif((c(i).gt.MGreenS(IH)%KCut*linkWAF(i,3)).and.(c(i).le.MGreenS(IH)%Kjam2*linkWAF(i,4))) then ! Modified GreenShield Regime

! Added Oct 4 2005 to account for change in speed-density relationship due to workzone effect	        
			iflagWZ = 0
			
			do iwz = 1, WorkZoneNum
              ilink=GetFLinkFromNode(WorkZone(iwz)%FNode,WorkZone(iwz)%TNode)
			  if(ilink.eq.i.and.wzstartflag(iwz).eq..True.) then
			    iflagWZ = 1
				WZID = iwz
			  endif
			enddo

			if(IQ.eq.1) then ! with flat part
! Added Oct 4 2005 
			  if(iflagWZ.eq.1) then
			  !Modification: 10182013
			  !  v(i)= (MGreenS(IH)%Vf2*linkWAF(i,1)-WorkZone(WZID)%delta_speed+Vfadjust(i)*linkWAF(i,8)-MGreenS(IH)%V02*linkWAF(i,2))/60.0*((1-c(i)/(MGreenS(IH)%Kjam2*linkWAF(i,4))))**(MGreenS(IH)%alpha2*linkWAF(i,5))+MGreenS(IH)%V02*linkWAF(i,2)/60.0
		      	v(i)= (MGreenS(IH)%Vf2*linkWAF(i,1)-WorkZone(WZID)%delta_speed-MGreenS(IH)%V02*linkWAF(i,2))/60.0*((1-c(i)/(MGreenS(IH)%Kjam2*linkWAF(i,4))))**(MGreenS(IH)%alpha2*linkWAF(i,5))+MGreenS(IH)%V02*linkWAF(i,2)/60.0
		      !Modification: 10182013
		      else
		      !Modification: 10182013
			  !  v(i)= (MGreenS(IH)%Vf2*linkWAF(i,1)+Vfadjust(i)*linkWAF(i,8)-MGreenS(IH)%V02*linkWAF(i,2))/60.0*((1-c(i)/(MGreenS(IH)%Kjam2*linkWAF(i,4))))**(MGreenS(IH)%alpha2*linkWAF(i,5))+MGreenS(IH)%V02*linkWAF(i,2)/60.0
			    v(i)= (MGreenS(IH)%Vf2*linkWAF(i,1)-MGreenS(IH)%V02*linkWAF(i,2))/60.0*((1-c(i)/(MGreenS(IH)%Kjam2*linkWAF(i,4))))**(MGreenS(IH)%alpha2*linkWAF(i,5))+MGreenS(IH)%V02*linkWAF(i,2)/60.0
			  !Modification: 10182013
			  endif
! End
	        elseif (IQ.eq.2) then ! without flat part
            !Weather June 7 2011
            !  v(i)= (SpeedLimit(i)+Vfadjust(i)*linkWAF(i,8)-MGreenS(IH)%V02*linkWAF(i,2))/60.0*((1-c(i)/(MGreenS(IH)%Kjam2*linkWAF(i,4))))**(MGreenS(IH)%alpha2*linkWAF(i,5))+MGreenS(IH)%V02*linkWAF(i,2)/60.0
              v(i)= ((SpeedLimit(i)+Vfadjust(i))*linkWAF(i,8)-MGreenS(IH)%V02*linkWAF(i,2))/60.0*((1-c(i)/(MGreenS(IH)%Kjam2*linkWAF(i,4))))**(MGreenS(IH)%alpha2*linkWAF(i,5))+MGreenS(IH)%V02*linkWAF(i,2)/60.0
            !end Weather June 7 2011
           
            else
			  write(911,*) 'error in flow_model_update'
	        endif
		 else ! minimal speed regime
		   v(i)= MGreenS(IH)%V02*linkWAF(i,2)/60.0
		 endif

!Weather June 7 2011
! added by Hayssam August 10 2005 
!if(v(i).gt.	((SpeedLimit(i) + Vfadjust(i)*linkWAF(i,8))/60.0)) then
!v(i) = (SpeedLimit(i)+Vfadjust(i)*linkWAF(i,8))/60.0
!endif

if(v(i).gt.	(((SpeedLimit(i) + Vfadjust(i))*linkWAF(i,8))/60.0)) then
v(i) = ((SpeedLimit(i)+Vfadjust(i))*linkWAF(i,8))/60.0
endif
!end Weather June 7 2011

! -- calculatc vtmp, the queue-free speed
        if(ctmp(i).le.MGreenS(IH)%KCut*linkWAF(i,3)) then !free-flow regime
        !Weather June 7 2011
		!   vtmp(i)= (SpeedLimit(i)+Vfadjust(i)*linkWAF(i,8))/60.0
		   vtmp(i)= ((SpeedLimit(i)+Vfadjust(i))*linkWAF(i,8))/60.0
        !end Weather June 7 2011
        elseif((ctmp(i).gt.MGreenS(IH)%KCut*linkWAF(i,3)).and.(ctmp(i).le.MGreenS(IH)%Kjam2*linkWAF(i,4))) then ! Modified GreenShield Regime
	       if(IQ.eq.1) then ! with flat part
	       !Modification: 10182013
		   !   vtmp(i)= (MGreenS(IH)%Vf2*linkWAF(i,1)+Vfadjust(i)*linkWAF(i,8)-MGreenS(IH)%V02*linkWAF(i,2))/60.0*((1-ctmp(i)/(MGreenS(IH)%Kjam2*linkWAF(i,4))))**(MGreenS(IH)%alpha2*linkWAF(i,5))+MGreenS(IH)%V02*linkWAF(i,2)/60.0
   		      vtmp(i)= (MGreenS(IH)%Vf2*linkWAF(i,1)-MGreenS(IH)%V02*linkWAF(i,2))/60.0*((1-ctmp(i)/(MGreenS(IH)%Kjam2*linkWAF(i,4))))**(MGreenS(IH)%alpha2*linkWAF(i,5))+MGreenS(IH)%V02*linkWAF(i,2)/60.0
	       !Modification: 10182013
	       elseif (IQ.eq.2) then ! without flat part
        !Weather June 7 2011
	    !     vtmp(i)= (SpeedLimit(i)+Vfadjust(i)*linkWAF(i,8)-MGreenS(IH)%V02*linkWAF(i,2))/60.0*((1-ctmp(i)/(MGreenS(IH)%Kjam2*linkWAF(i,4))))**(MGreenS(IH)%alpha2*linkWAF(i,5))+MGreenS(IH)%V02*linkWAF(i,2)/60.0
              vtmp(i)= ((SpeedLimit(i)+Vfadjust(i))*linkWAF(i,8)-MGreenS(IH)%V02*linkWAF(i,2))/60.0*((1-ctmp(i)/(MGreenS(IH)%Kjam2*linkWAF(i,4))))**(MGreenS(IH)%alpha2*linkWAF(i,5))+MGreenS(IH)%V02*linkWAF(i,2)/60.0
        !end Weather June 7 2011
           else
			  write(911,*) 'error in flow_model_update'
	       endif
		else ! minimal speed regime
		   vtmp(i)= MGreenS(IH)%V02*linkWAF(i,2)/60.0
		endif

!Weather June 7 2011
! added by Hayssam August 10 2005 
!if(vtmp(i).gt.((SpeedLimit(i) + Vfadjust(i)*linkWAF(i,8))/60.0)) then
!vtmp(i) = (SpeedLimit(i)+Vfadjust(i)*linkWAF(i,8))/60.0
!endif

if(vtmp(i).gt.(((SpeedLimit(i) + Vfadjust(i))*linkWAF(i,8))/60.0)) then
vtmp(i) = ((SpeedLimit(i)+Vfadjust(i))*linkWAF(i,8))/60.0
endif

!end Weather June 7 2011

!Add by Alex for snow accumulation
if (lexist_SnowAccu )then 
    v(i)=v(i)*speedreduction(i)
    vtmp(i)=vtmp(i)*speedreduction(i)
endif

! --
! -- preapre the travel time along link to TTimeOfBackLink(link)
! --
          TTimeOfBackLink(ForToBackLink(i))=tlength/(nlanes(i)*vtmp(i))
          statmpt(i)=tlength/(nlanes(i)*vtmp(i))

!added by hayssam on Jan 4 2006
!to reflect the cost for blocked links
        if(xl(i)/(nlanes(i)*s(i)).le.0.1) then
	    TTimeOfBackLink(ForToBackLink(i)) = PenForPreventMove*10
		endif


! added by Hayssam and Jason August 18, 2005
! for freeway bias implementation
      if(link_iden(i).eq.1 .or. link_iden(i).eq.2 ) then
	  TTimeOfBackLink(ForToBackLink(i))= TTimeOfBackLink(ForToBackLink(i))*(1-fwy_bias)
      statmpt(i)=statmpt(i)*(1-fwy_bias)
	  endif
	  
	  
! added by Archak and Zihan on 20151111
! for speed harmonization
! Set flag = 1 to implement speed harmonization 
   
   if (SH_flag) then
        do jj_SH=1,shockwave_count  
            if(((time_SH(jj_SH)-t_start).lt.(updatetime_speedHarm)).and.(link_SH(jj_SH).eq.i)) then
                call speed_harmonization (i,flowmodel_SH(jj_SH))
            endif
        end do 
    endif

40    continue      
end subroutine
