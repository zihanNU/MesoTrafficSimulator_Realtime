           subroutine kspcost_main(dy_muc)
c --  
c -- This subroutine is the main subroutine for the shortest path
c -- calculations. 
c --
c -- This subroutine is called from main and loop
c -- This subroutine call the following subroutines:
c -- 1. ksp_init
c -- 2. ksp_calculate
c -- 3. ksp_integrate
c -- 4. ksp_priorities
c --
c -- INPUT: 
c --  no specific input
c -- OUTPUT:
c --  no specific output
c --   
c       include 'common.inc'
      use muc_mod
c -- 
c --  dy_muc is the indicator to let ksp_main know if it is called
c --  by dynasmart or muc.  
c --  If dy_muc = 0, called from dynasmart, them do not use TD feature
c --  If dy_muc = 1, called from muc, use TD feature
	integer dy_muc

	     
c --
c --	Travel time
c --  Iti_nu has changed to be a variable = nint(stagelength)
      kpaths=kay
!	if(dy_muc.eq.0) then  !dynasmart
!	  do ilink=1,noofarcs
!         do itt=1,Iti_nu
!          TTime(ilink,itt)=TTimeOfBackLink(ilink)
!         enddo
!	  enddo
!      else
!	  do ilink=1,noofarcs
!         do itt=1,Iti_nu
!          TTime(ForToBackLink(ilink),itt)=TravelTime(ilink,itt)
!         enddo
!        enddo
!      endif
!
!c --  Penalty
!      if(dy_muc.eq.0) then
!        do ilink=1,noofarcs
!         do itt=1,Iti_nu
!          do movee=1,nu_mv
!	    TTPenalty(ilink,itt,movee)=penalty(ilink,movee)
!          enddo
!         enddo
!        enddo
!    	else
!        do ilink=1,noofarcs
!         do itt=1,Iti_nu
!          do movee=1,nu_mv
!          TTPenalty(ilink,itt,movee)=TravelPenalty(ilink,itt,movee)
!          enddo
!         enddo
!        enddo
!	endif 


!Added by Zihan for AMS project, calculate tdsp with predictive travel time
  



      IF((i_predictiveinfo.eq.0).or.(int(time_now/60).lt.roll))then
	    if(dy_muc.eq.0) then  !dynasmart
	      do ilink=1,noofarcs
             do itt=1,Iti_nu
              TTime(ilink,itt)=TTimeOfBackLink(ilink)
             enddo
	      enddo
          else
	      do ilink=1,noofarcs
             do itt=1,Iti_nu
              TTime(ForToBackLink(ilink),itt)=TravelTime(ilink,itt)
             enddo
            enddo
          endif

c --  Penalty
          if(dy_muc.eq.0) then
            do ilink=1,noofarcs
             do itt=1,Iti_nu
              do movee=1,nu_mv
	        TTPenalty(ilink,itt,movee)=penalty(ilink,movee)
              enddo
             enddo
            enddo
    	    else
            do ilink=1,noofarcs
             do itt=1,Iti_nu
              do movee=1,nu_mv
              TTPenalty(ilink,itt,movee)=TravelPenalty(ilink,itt,movee)
              enddo
             enddo
            enddo
	    endif 
	 ENDIF
	 
	 

      !else if ((dy_muc.eq.0).AND.(i_new_tt.eq.1.OR.i_new_tp.eq.1)) then
!	 if (i_new_tt.eq.1.OR.i_new_tp.eq.1)then
!	      !Iti_nu_pred=horizon
!	       do ilink=1,noofarcs
!                do itt=1,Iti_nu
!                  TTime(ilink,itt)=Traveltime_pred(ilink,itt)
!                  do mo=1,nu_mv
!                TTPenalty(ilink,itt,mo)=TravelPenalty_pred(ilink,itt,mo)
!                  enddo
!                enddo
!	      enddo
!	      write(911,*) 'predictiveinfo checked'
!	      i_new_tt=0
!	      i_new_tp=0
!
!	 endif    
	    
!	else if (int(time_now/60).lt.roll) then
!	      do ilink=1,noofarcs
!             do itt=1,Iti_nu
!              TTime(ilink,itt)=TTimeOfBackLink(ilink)
!             enddo
!	      enddo
!
!          do ilink=1,noofarcs
!             do itt=1,Iti_nu
!              do movee=1,nu_mv
!	        TTPenalty(ilink,itt,movee)=penalty(ilink,movee)
!              enddo
!             enddo
!            enddo
	!ENDIF
! End Addition by Zihan

! Modified by MTI Team Feb 07 2004 0.930.9
!    	if(dy_muc.gt.0) then
!        do ilink = 1, noofarcs
!         ilink1 = ForToBackLink(ilink)
c        mtm = backindex(ilink)-backpointr(idnod(ilink))+1
!         mtm = movein(ilink,nu_mv+1)
!         TTPenalty(ilink1,:,mtm+1)=PenaltyEntry(ilink,:)
!        enddo
!      endif

	if(iso_ok.eq.1.or.iue_ok.eq.1) then
! Initialize TTPenalty from any connector to outbound links as infinity
      do ilink = 1, noofarcs
	if(link_iden(ilink).eq.100) then ! connector 

	icentroid = iunod(ilink) ! Centroid node ID for connector
		
	do iaa=1,llink(ilink,nu_mv+1) ! for each outbound link of connector
         ioutboundlink = llink(ilink,iaa)

	iupstreamnode = iunod(ioutboundlink) ! Upstream node ID for outbound link

	Movements=BackPointr(iupstreamnode+1)-BackPointr(iupstreamnode) ! number of incoming movements wrt node iupstreamnode
	imovement = -1
       do m =1, Movements
	   NTransient=BackPointr(iupstreamnode)+m-1
         Nodee=UNodeOfBackLink(NTransient) ! upstream node of link NTransient (in backward star)
	
	  if(icentroid.eq.Nodee) then
	  imovement = m 
	  endif
	enddo
      
	if(imovement.eq.  -1) then
	write(*,*), "cannot find corresponding movement from a connector
     + to current outbound link"
	stop
	endif

      ioutboundlinkback = ForToBackLink(ioutboundlink)

      TTPenalty(ioutboundlinkback,:,imovement)= infinity
	
	enddo

	endif

	enddo


! Put entry queue time as TTPenalty from a connector to outbound generation links
	  	
      do iz=1,nzones
      do il=1,NoofGenLinksPerZone(iz)
	igenelink = LinkNoInZone(iz,il)

	icentroid = origin(iz) ! Centroid node ID for origin zone iz
	iupstreamnode = iunod(igenelink) ! Upstream node ID for generation link igenelink

	Movements=BackPointr(iupstreamnode+1)-BackPointr(iupstreamnode) ! number of incoming movements wrt node iupstreamnode
	imovement = -1
       do m =1, Movements
	   NTransient=BackPointr(iupstreamnode)+m-1
         Nodee=UNodeOfBackLink(NTransient) ! upstream node of link NTransient (in backward star)
	
	  if(icentroid.eq.Nodee) then
	  imovement = m 
	  endif
	enddo
      
	if(imovement.eq.  -1) then
	write(*,*), "cannot find corresponding movement from a connector
     + to current generation link"
	stop
	endif

      igenelinkback = ForToBackLink(igenelink)

	if (ALLOCATED(PenaltyEntry)) then

      TTPenalty(igenelinkback,:,imovement)= PenaltyEntry(igenelink,:)
	else
      TTPenalty(igenelinkback,:,imovement)= 0
	endif

	enddo
	enddo
	
	endif

!Added Sep 12 2005
!	for one shot case
!	if(itedex.eq.0) then
!		call output_td_time()
!	endif

!	for muc case
	if(itedex.gt.0.and.iteration.gt.0.and.
     *   ioutput_tdtime_flag.eq.0) then
		call output_td_time()
		ioutput_tdtime_flag = 1
	endif
!End 

c --  Construct final cost
c --  
       do ilink=1,noofarcs
        do itt=1,iti_nu
         do movee=1,nu_mv
	    if(dy_muc.eq.2) then ! SO case
           TTmarginal(itt,ilink,movee)=TTime(ilink,itt)+
     *     TTPenalty(ilink,itt,movee)+PenaltyMG(itt,ilink,movee)   
          else ! UE or static case
           TTmarginal(itt,ilink,movee)=TTime(ilink,itt)+
     *     TTPenalty(ilink,itt,movee)
	    endif
          enddo
        enddo
       enddo


c --  PenaltyEntryMG is the marginal link entry queue
c         PenaltyEntryMG(:,:)=PenaltyEntry(:,:)

         do  ides=1,noof_master_destinations
          do  ltype=1,no_link_type
           do  ioccup=1,no_occupancy_level
!             destin=destination(ides)

! Modified by Xuesong and Jason July 1 2003
!	if(dy_muc.eq.0) then  !dynasmart
	if(dy_muc.eq.0.and.iSequentialLoad.eq.0) then  !dynasmart
!Added May 26 2005 
			 destin=destination(ides)
      else
			 destin=destination(real_SuperzoneIndex)
      endif	
! End of change
	       if(destin.ne.0)then
               call kspcost_init
               call kspcost_calculate
               ! Addedd by Zihan for ksp_AMS 20160616
!               
!               if (i_predictiveinfo.eq.1) then
!	                call  kspcost_calculate_AMS
!	         else                 
!                      call kspcost_calculate
!               end if
!               
               ! END addition
               
               call kspcost_integrate
			 if(dy_muc.eq.0) then
                 if(time_now.le.1) call network_check(ides)
                   !call kspcost_priorities
! Modified Sep 12 2005
                 if(iSequentialLoad.eq.0) then                 
			     call kspcost_priorities	           
			   endif

			 endif
	       endif
      	 enddo
          enddo
         enddo
! Commented out by MTI team Jan 27 2004 0.930.9
!	   if(time_now.le.1) call CheckGenLinkOnConnectivity()


        return
        end
