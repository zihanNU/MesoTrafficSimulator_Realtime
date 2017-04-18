

subroutine check_traffic_events

	use muc_mod
	character(256) ErString
	integer(4) error
	! This subroutine checks for incidents and vms's currently in the network


! -- read incident data
! --
! -- inici_num : number of incidents
! -- 
! -- i1 and i2 are the upstream and downstream nodes for the link on which
! -- the incident occures.
! --
! --
	open(file='incident.dat',unit=46,status='old',iostat=error) 
	if(error.ne.0) then
         write(911,*) 'Error when opening incident.dat'
	   stop
	endif

	if(EOF(46)) then
	 ErString = "incident.dat"
	 call ErReadEOF(ErString)
	endif
    read(46,*) inci_num


! Allocate/deallocate memory
	if(allocated(inci)) then
		!first, deallocate evferything
		deallocate(incistartflag)
		deallocate(inci)
		deallocate(incil)
		deallocate(incilist)
		deallocate(itp)
	endif
	listtotal = 0

	if(inci_num.gt.0) then

     	allocate(incistartflag(inci_num),stat=error)
	if(error.ne.0) then
	  write(911,*) 'allocate incistartflag error - insufficient memory'
	  stop
	endif
 	incistartflag(:) = .False.
      
      allocate(inci(inci_num,3),stat=error)
	if(error.ne.0) then
	  write(911,*) 'allocate inci error - insufficient memory'
	  stop
	endif
	inci(:,:)=0
	
      allocate(incil(inci_num),stat=error)
	if(error.ne.0) then
	  write(911,*) 'allocate incil error - insufficient memory'
	  stop
	endif
	incil(:)=0
	
      allocate(incilist(inci_num),stat=error)
	if(error.ne.0) then
	  write(911,*) 'allocate incilist error - insufficient memory'
	  stop
	endif
	incilist(:)=0
	
      allocate(itp(inci_num),stat=error)
	if(error.ne.0) then
	  write(911,*) 'allocate itp error - insufficient memory'
	  stop
	endif
	itp(:)=0
	
      else

	allocate(incistartflag(1),stat=error)
	if(error.ne.0) then
	  write(911,*) 'allocate incistartflag error - insufficient memory'
	  stop
	endif
 	incistartflag(:) = .False.
      
	allocate(inci(1,3),stat=error)
	if(error.ne.0) then
	  write(911,*) 'allocate inci error - insufficient memory'
	  stop
	endif
	inci(:,:)=0
	
      allocate(incil(1),stat=error)
	if(error.ne.0) then
	  write(911,*) 'allocate incil error - insufficient memory'
	  stop
	endif
	incil(:)=0
	
      allocate(incilist(1),stat=error)
	if(error.ne.0) then
	  write(911,*) 'allocate incilist error - insufficient memory'
	  stop
	endif
	incilist(:)=0
	
      allocate(itp(1),stat=error)
	if(error.ne.0) then
	  write(911,*) 'allocate itp error - insufficient memory'
	  stop
	endif
	itp(:)=0
 	
    endif

      if(inci_num.gt.0) then
        do i=1,inci_num
         read(46,*) i1,i2,inci(i,1),inci(i,2),inci(i,3)
         do k=1,noofarcs
          if(iunod(k).eq.idnum(i1).and.idnod(k).eq.idnum(i2)) incil(i)=k !G
         end do
! --
! -- Check for input errors
! --
         if(incil(i).eq.0) then 
          write(911,*) 'INPUT ERROR : incident data file'
          write(911,*) 'check the incident link' 
          write(911,*) 'for incident number',i
          stop
         endif
       end do
      endif

	close(46) 

! --
! --  end of reading Incident data
! --
! --

! -- read the vms data
! --
! -- vms_num : number of vms
! --
! -- i1 and i2 are the upstream and downstream nodes for the link
! -- on which the VMS exists.
! --
	open(file='vms.dat',unit=49,status='old',iostat=error) 
	if(error.ne.0) then
         write(911,*) 'Error when opening vms.dat'
	   stop
	endif

    if(EOF(49)) then
	 ErString = "vms.dat"
	 call ErReadEOF(ErString)
	endif
	read(49,*) vms_num

! Allocate/deallocate memory
     if( allocated(vms) ) then
		! first, deallocate everything
		if (allocated(vmstypetwopath)) deallocate(vmstypetwopath)
		deallocate(vmstype)
		deallocate(vms)
		deallocate(vms_start)
		deallocate(vms_end)
	 endif

   if(vms_num.gt.0) then

      allocate(vmstypetwopath(vms_num,100),stat=error)
	if(error.ne.0) then 
	  write(911,*) 'allocate vmstype error - insufficient memory'
	  stop
	endif
	vmstypetwopath(:,:)%node = 0 
	vmstypetwopath(:,:)%link = 0

	
      allocate(vmstype(vms_num),stat=error)
	if(error.ne.0) then 
	  write(911,*) 'allocate vmstype error - insufficient memory'
	  stop
	endif
	vmstype(:) = 0 
	
      allocate(vms(vms_num,4),stat=error)
	if(error.ne.0) then
	  write(911,*) 'allocate vms error - insufficient memory'
	  stop
	endif
	vms(:,:) = 0
	
      allocate(vms_start(vms_num),stat=error)
	if(error.ne.0) then
	  write(911,*) 'allocate vms_start error - insufficient memory'
	  stop
	endif
	vms_start(:) = 0
	
      allocate(vms_end(vms_num),stat=error)
	if(error.ne.0) then
	  write(911,*) 'allocate vms_end error - insufficient memory'
	  stop
	endif
	vms_end(:) = 0
      
	else
	
      allocate(vmstype(1),stat=error)
 	if(error.ne.0) then
	  write(911,*) 'allocate vmstype error - insufficient memory'
	  stop
	endif
	vmstype(:) = 0 
	
      allocate(vms(1,4),stat=error)
	if(error.ne.0) then
	  write(911,*) 'allocate vms error - insufficient memory'
	  stop
	endif
	vms(:,:) = 0
	
      allocate(vms_start(1),stat=error)
	if(error.ne.0) then
	  write(911,*) 'allocate vms_start error - insufficient memory'
	  stop
	endif
	vms_start(:) = 0
	
      allocate(vms_end(1),stat=error)
	if(error.ne.0) then
	  write(911,*) 'allocate vms_end error - insufficient memory'
	  stop
	endif
	vms_end(:) = 0

	endif


      
      if(vms_num.gt.0) then    
      
	do i=1,vms_num
      read(49,*) vmstype(i),i1,i2,vms(i,2),vms(i,3), vms_start(i),vms_end(i)

! read subpath for type 2
      if(vmstype(i).eq.2) then
	  read(49,*) (vmstypetwopath(i,k)%node,k=1,vms(i,3))      
        if(vmstypetwopath(i,1)%node.ne.i2) then
         write(911,*) 'error in',i,'th  VMS'
         write(911,*) 'error in Type 2 VMS subpath specification'
         stop
	  endif
        vms(i,2) = 100.0
        do mmp = 1, vms(i,3)-1
          vmstypetwopath(i,mmp)%link=GetFLinkFromNode(vmstypetwopath(i,mmp)%node,vmstypetwopath(i,mmp+1)%node)
        enddo
      endif  


! --  the kth path specified in vms.dat for type 3 vms
! --  needs to be smaller or equal to the kay as specified in network.dat
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


      vms(i,1)=GetFLinkFromNode(idnum(i1),idnum(i2))


       if(vms(i,1).eq.0) then 
         write(911,*) 'INPUT ERROR : VMS data file'
         write(911,*) 'check the VMS link' 
         write(911,*) 'for VMS number',i
         stop
       endif

! --
! --
        enddo   
      
	endif ! vms_num.gt.0
	close(49) 

! --
! --  end of reading VMS data
! --
! --



end subroutine



! Add  for AMS Project 20160606 by Zihan, Modified according to DTAX: four subroutine added: chech_control, check_network, check_predictiveinfo
! This subroutine was further modified by Tian Hou 20140214 to read more than one new signal plans from DTAX
subroutine check_control_update

    use muc_mod
    character(256) ErString
    integer(4) error
    integer::ifsize=0
    
    !added by Tian Hou 20140214
    character (256):: orgFile
    character (256):: newFile
    orgFile = 'control.dat'
    newFile = 'new_control.dat'
    !end add
    
    !comment out by Tian Hou 20140214
    !real control_plan_start_time
    !end comment out
    call GETFSIZE("new_control.dat",ifsize)

    if(ifsize.gt.0) then  ! updated control.dat exists
        !Lan: DTAX&SM 02192014
       ! call COPY_FILE(newFile, orgFile)   
        !Lan: DTAX&SM 02192014
        i_new_control = 1
        
        ! copy new_control.dat to control.dat, and then delete new_control.dat

        !call REPLACE_FILE (orgFile,newFile)
        
        ! open control.dat and read the first two lines to isig and strtsig(:)
        open(file='new_control.dat',unit=444,iostat=error)
        if(error.ne.0) then
            write(911,*) 'Error when opening control.dat'
            stop
        endif
        read(444,*,iostat=error) isig        
        if(allocated(strtsig)) then
            deallocate(strtsig, stat = error)
            if(error.ne.0) then
		        write(911,*) 'deallocate strtsig error - insufficient memory'
		        stop
		    endif
        endif
        allocate(strtsig(isig+1),stat = error)
        if(error.ne.0) then
            write(911,*) 'allocate strtsig error - insufficient memory'
            stop
        endif
        strtsig(:) = 0
        read(444,*,iostat=error) (strtsig(i), i=1,isig)
        strtsig(isig+1) = 2*stagelength
        
        !reset counter as 0, this is very important
        isigcount = 0
        close(444,status='delete')
    endif

end subroutine




subroutine check_network_update
!Lan: Aug 22 2011
!new_network.dat is the only new_*.f which IS NOT deleted after update. The format is remained UNKNOWN though.
!Lan guess first line is a time-dependent setting for this link the second line is the original information.
!
!NOTE: ONE Error here is time_now is in seconds, and Lan guess iStartTime1(2) and iEndTime1(2) are specified in min.
!      Modifications are made accordingly.
!Lan: Aug 22 2011

    use muc_mod
	character(256) ErString
	integer(4) error, ilink
	integer mfrtp1,sattp1,mfrtp2,sattp2
    integer::ifsize=0

	call GETFSIZE("new_network.dat",ifsize)
	if(ifsize.gt.0) then  ! updated control.dat exists
       open(file='new_network.dat',unit=656,status='old',iostat=error) 
  	   if(error.ne.0) then
         write(911,*) 'Error when opening new_network.dat'
  	     stop
	   endif

	   if(EOF(656)) then
	      ErString = "incident.dat"
	      call ErReadEOF(ErString)
	   endif
	   read(656,*) ilink_num
   
       do i=1,ilink_num
          read(656,*,iostat=error) iu1,id1,iStartTime1,iEndTime1,iLinkLength1,iLanes1,iFlowModel1,mfrtp1,sattp1
		  read(656,*,iostat=error) iu2,id2,iStartTime2,iEndTime2,iLinkLength2,iLanes2,iFlowModel2,mfrtp2,sattp2
          ilink = GetFLinkFromNode(idnum(iu1),idnum(id1))
		
		  !Lan: Aug 22 2011
		  iStartTime1=iStartTime1*60
		  iEndTime1=iEndTime1*60
		  iStartTime2=iStartTime2*60
		  iEndTime2=iEndTime2*60
		  !Lan: Aug 22 2011

		  if(time_now .ge. iStartTime1 .and. time_now .le. iEndTime1) then             
			 s(ilink)= float(iLinkLength1)/5280.0
			 nlanes(ilink) = iLanes1
			 FlowModelNum(ilink) = iFlowModel1
			 MaxFlowRateOrig(ilink) = float(mfrtp1)/3600.0 * nlanes(ilink)
			 MaxFlowRate(ilink) = float(mfrtp1)/3600.0 * nlanes(ilink)
			 SatFlowRate(ilink) = float(sattp1)/3600.0 * nlanes(ilink)
			 xl(ilink) = nlanes(ilink)*s(ilink)			 
		  else
		     s(ilink)= float(iLinkLength2)/5280.0
			 nlanes(ilink) = iLanes2
			 FlowModelNum(ilink) = iFlowModel2
             MaxFlowRateOrig(ilink) = float(mfrtp2)/3600.0*nlanes(ilink)
			 MaxFlowRate(ilink) = float(mfrtp2)/3600.0*nlanes(ilink)
			 SatFlowRate(ilink) = float(sattp2)/3600.0*nlanes(ilink)
			 xl(ilink) = nlanes(ilink)*s(ilink)
		  endif

       enddo
	close(656)
!Lan: Aug 24 2011
!    Note: new_network.dat is not Deleted in RT anyway in the original design

	endif 

end subroutine


! Add by Zihan for predictive ADM 
subroutine check_predictiveinfo


    use muc_mod
	use vector_mod
    use LinkList_mod
    character(256) ErString
    character(LEN=99) :: SkimText
    character(LEN=99) :: SkimText_time
    character(LEN=99) :: SkimText_penalty
    integer(4) error
    integer::ifsize=0
    real:: time_pred
    integer:: agg_pred
    integer::linki
    integer::tfinal
    integer::timetmp
    integer::penaltytemp
    integer::shift
    character(LEN=250)  :: path 
    character(LEN=99)::subpath
    real::tt,tp
            

    
    if (i_predictiveinfo.eq.1) then
	  ! give initial updated number to avoid 0 turn penalty 
 
             do itt=1,iti_nu
              do ilink=1,noofarcs
                 TTime(ilink,itt)=TTimeOfBackLink(ilink)
                  do movee=1,nu_mv
                 TTpenalty(ilink,itt,movee)= penalty(ilink,movee)
                  enddo
               enddo
              enddo
	           
        
        !if (mod(int(time_now/60),roll).eq.0) then
        if  ((mod(int(time_now-latency*60),(roll*60))).eq.0) then
        !if (mod(int(time_now),roll*60).eq.0) then
        !iti_nuall=nint((stagelength/tii)/ftr) +1 
            CALL getcwd(path)
            subpath='\predictiveinfo\'
            write(SkimText,'(i4)') (int(time_now/60)-latency)
        
            !SkimText_time='Z:\\2014 - AMS Project\\Tests\\predictiveinfo\\new_travel_time_' // trim(adjustl(SkimText)) // '.dat'
            !SkimText_penalty='Z:\\2014 - AMS Project\\Tests\\predictiveinfo\\new_travel_penalty_' // trim(adjustl(SkimText)) // '.dat'
  
            !SkimText_time='"'//trim(adjustl(path))//'predictiveinfo\\new_travel_time_' // trim(adjustl(SkimText)) // '.dat'"'
            SkimText_time=trim(adjustl(path))//trim(adjustl(subpath))//'new_travel_time_' // trim(adjustl(SkimText)) // '.dat'
            SkimText_penalty=trim(adjustl(path))//trim(adjustl(subpath))//'new_travel_penalty_' // trim(adjustl(SkimText)) // '.dat'
  
            !SkimText_time='new_travel_time_pre.dat'
            !SkimText_penalty='new_travel_penalty_pre.dat'
                  
                  
            
            !SkimText_time=trim(path)//predictiveinfo\\new_travel_time_' // trim(adjustl(SkimText)) // '.dat'
            !SkimText_penalty=trim(path)//predictiveinfo\\new_travel_penalty_' // trim(adjustl(SkimText)) // '.dat'
    
            
            
            call GETFSIZE(SkimText_time,ifsize)

            if(ifsize.gt.0) then  ! updated travel_time.dat exists
               i_new_tt = 1

               open(file=SkimText_time,unit=701,iostat=error) 
  	           if(error.ne.0) then
                 write(911,*) 'Error when opening new_travel_time.dat'
  	             stop
	           endif
	          
	          !iti_nu=int(time_now/60)+horizon 
	           
	       
	           
	          ! give initial number for the predicted horizon 
	 
!	         do itt=1,iti_nu
!	          do ilink=1,noofarcs
!                 Traveltime_pred(ilink,itt)=TTimeOfBackLink(ilink)
!                  do movee=1,nu_mv
!                 TravelPenalty_pred(ilink,itt,movee)= penalty(ilink,movee)
!                  enddo
!	           enddo
!	          enddo
	           

	           
	           shift=int(time_now/60)
	           
	           itt=1
	           linki=1
	           

!	           do while ((itt.lt.(int(time_now/60)+horizon)).AND.(itt.lt.iti_nu))	
!                     
!                    do while ((linki.lt.noofarcs).AND.(NOT(EOF(701))))
! 
!    	                read(701,*) linki,itt, TravelTime_pred(ForToBackLink(linki),itt)
!                    enddo
!                    if (EOF(701))Exit
!               enddo  
              
              !updated the number of predicted horizon from input files       
              do while (((linki.le.noofarcs).AND.(NOT(EOF(701)))).AND.((itt-shift+1).le.iti_nu))
				  !read(701,*) linki,itt, TravelTime_pred(ForToBackLink(linki),itt+latency) 
				  !read(701,*) linki,itt, TravelTime_pred(linki,itt+latency) 
				  read(701,*) linki,itt,tt
				  !if (itt-latency-shift.gt.0)       then
				  !  TTime(linki,itt-shift+1) =tt      
				  !endif    
				  if ((itt-shift.ge.0).AND.((itt-shift+1).le.iti_nu))      then
				   !ForToBackLink(linki)
				    TTime(ForToBackLink(linki),itt-shift+1) = tt     
				  endif      
                 IF (EOF(701)) Exit
              enddo

!               tfinal=int((time_now/60)+horizon)
!               do ilink=1,noofarcs
!                  
!                   do itt=tfinal+1,Iti_nu
!                   Traveltime_pred(ilink,itt)=Traveltime_pred(ilink,tfinal)
!                   !Traveltime_pred(ilink,itt)=TTimeOfBackLink(ilink)
!                    ! Traveltime_pred(ilink,itt)=amax1(Traveltime_pred(ilink,tfinal),TTimeOfBackLink(ilink))
!                   enddo
!	           enddo
!                              
            end if
            
            
            call GETFSIZE(SkimText_penalty,ifsize)

            if(ifsize.gt.0) then  ! updated travel_time.dat exists
               i_new_tp = 1
               open(file=SkimText_penalty,unit=702,iostat=error) 
  	           if(error.ne.0) then
                 write(911,*) 'Error when opening new_travel_penalty.dat'
  	             stop
	           endif
	           

	           itt=1
	           linki=1 
	         
	         
	         !update   
              do while (((linki.le.noofarcs).AND.(NOT(EOF(702)))).AND.((itt-shift+1).le.iti_nu))
				  !read(702,*) linki,itt,iinbound,TravelPenalty_pred(ForToBackLink(linki),itt+latency,iinbound)    
				  
				  !read(702,*) linki,itt,iinbound,TravelPenalty_pred(linki,itt+latency,iinbound)
				    read(702,*) linki,itt,iinbound, tp
				    !if (itt-latency-shift.gt.0)       then
				    !    TTPenalty(linki,itt-shift+1,iinbound)=tp      
				    !endif   
				    if ((itt-shift.ge.0).AND.((itt-shift+1).le.iti_nu))     then
				    !ForToBackLink(linki)
				        TTPenalty(ForToBackLink(linki),itt-shift+1,iinbound)=tp    
				    endif                
                 IF (EOF(702)) Exit
              enddo


! After tfinal data
!                tfinal=int((time_now/60)+horizon)
!	            do ilink=1,noofarcs
!	                
!                    do itt=tfinal+1,Iti_nu
!                         !Traveltime_pred(ilink,itt)=Traveltime_pred(ilink,tfinal)
!                         do mo=1,nu_mv
!                            TravelPenalty_pred(ilink,itt,mo)=TravelPenalty_pred(ilink,tfinal,mo)
!                            !TravelPenalty_pred(ilink,itt,mo)=penalty(ilink,mo)
!                         enddo
!                    enddo
!	            enddo    
             end if
                      
	         
        end if
    end if   
    	  close (701)  !reset the flag and delete the file, 910 for traveltime and 911 for penalty
	      close (702)
    
end subroutine






! End Addition by Zihan