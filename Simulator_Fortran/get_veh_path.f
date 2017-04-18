
       subroutine get_veh_path(j,i,iselect,CurNode)

        
c --
c --   This subroutine assigns a path to the vehicle j.
c --   There are two cases for the path assignment :
c --   1. if iselect =0, then this is the initial path for the vehicle
c --   2. if iselsect >0, then this is a path switch
c --
c -- This subroutine is called from the following subroutines.
c -- 1. vehicle_moving : to assign an initial path to the vehicle
c -- 2. getlink : to assign the vehicle to the best path when the
c --              vehicle is switching paths.
c -- 3. vms_path : to assign a new path to the vehicles following the 
c --               VMS sign. 
c -- 4. vms_divert : to assign a new path to the vehicles following the 
c --               VMS sign. 
c --
c -- This subroutine does not call any other subroutines.
c --
c -- INPUT
c --    j : vehicle ID
c --    i : current link
c -- iselect : see the description above
c --
c -- OUTPUT
c --    path for vehicle j
c --  
      use muc_mod
	use vector_mod
	integer::pathttmp(1000)=0   
      integer Index1D,CurNode
    
      !real value 
	real value
c --
c -- If the vehicle is assigned to a specific path number (iselect), then
c -- the subroutine will assign the vehicle to that specific path.
c --
c -- If the vehicle will be assigned an initial path, then this will depend
c -- on the variable "ipinit": if ipinit=0 assign to a randomly selected 
c --                           path out of the "kay" paths
c --  iselect could take 3 values
c --  1: best path
c --  0: random assign
c --  others: specific path

      pathttmp(:) = 0
      if(iselect.eq.1) then
         ibest=iuserpath(j)
      elseif(iselect.eq.0) then


! Modified by Xuesong and Jing March 28 2004 1.0
!         call DYNA_random_number(r1,7)
		r1=ran1(istrm2)
	 
	   ibest=nint(r1*kay)
         if(ibest.gt.kay) ibest=kay
      else
         ibest=iselect
      endif
c  -- 
c  -- define destination and origin
c  --
          icu1=i
          ifrom=idnod(i)

!       ito=MasterDest(jdest(j))
!Added May 26 2005
	if(iSequentialLoad.eq.1) then
        ito = 1
	  itto = real_SuperzoneIndex
	else
        ito = MasterDest(jdest(j))
	  itto = ito
	endif
!End

          ict = 1

c  --
c  -- follow the shortest path code
c  --

            mov=ForToBackLink(icu1)-backpointr(ifrom)+1
c           know=labelpointerout(lt(j),ioc(j),ito,ifrom,ict,ibest,mov)
            know=ibest
            k=CurNode

!May 26 2005
c       do 20 while(ifrom.ne.destination(ito))
        do 20 while(ifrom.ne.destination(itto))
             if(know.eq.0) then
               know=1
             endif
             Index1D = k
             value = float(ifrom)
             call VhcAtt_Insert(j,Index1D,1,value)
		     pathttmp(k) = ifrom
             k=k+1
             ifromtmp=ifrom
             ktemp=know
             movetemp=mov
             icttemp=ict
         ict = 1
         mov=  pathpointerout3(lt(j),ioc(j),ito,
     *                     ifromtmp,icttemp,ktemp,movetemp)
         know= pathpointerout2(lt(j),ioc(j),ito,
     *                      ifromtmp,icttemp,ktemp,movetemp)
         ifrom=pathpointerout1(lt(j),ioc(j),ito,
     *                      ifromtmp,icttemp,ktemp,movetemp)


          if(mov.lt.1.or.know.lt.1.or.ifrom.lt.1)then

! Added Sep 8 2005 if cannot find a path then skip this vehicle (for sequential loading)
            if(iSequentialLoad.eq.1) then
	        nnpath(j) = -1
			return
            endif
! End

! added by hayssam nov 12 for 930.8
      write(911,*) 'Error!'
	write(911,*) 
	write(911,*) 'No path exists from:'
	!write(911,*) 'Node',ifromtmp,'    to destination node'
	!modified by xuesong for 930.8B  Jan 2004
	write(911,*) 'Node',nodenum(ifromtmp),'    to destination node'
     +,destination(MasterDest(jdest(j))),
     + '   in zone',jdest(j)

	!write(911,*) 'error in get_veh_path'
      write(911,*) 'For vehicle number',j
	write(911,*) 'Generation link ',nodenum(iunod(isec(j))),
     +'    ->',nodenum(idnod(isec(j)))

      write(911,*) 
	write(911,*) 'Possible reasons:'
	write(911,*) 'No physical path exists, or'
	write(911,*) 'There is a prevented movement in movement.dat, or'
	write(911,*) 'There is a prevented movement due to signal setting'
	write(911,*)
	write(911,*) 'If this error resulted from a detour VMS, then	
     + check the sequence of nodes'
      write(911,*) 'along the detour path'

           stop
	     exit
          endif 

20      continue


      Index1D = k
      value = float(destination(MasterDest(jdest(j))))
      call VhcAtt_Insert(j,Index1D,1,value)
	call VhcAtt_Clear(j,Index1D+1)

	pathttmp(k) = destination(MasterDest(jdest(j)))

      nnpath(j)=k

!commented out by hayssam
!august 17, 2005
!      if(CurNode.eq.1) then
!          call CheckImpact(pathttmp,j,CurNode-1) ! pass 0 as the last arg
!	endif

      return
      end  


!**************************************************************************************
!**************************************************************************************
! Added by MTI to store the path cost DYNASMART-P 1.0 March 19
        subroutine get_vms_path_cost(j,i,kvms,iselect,CurNode,Path_Cost,
     +  Feasibility_Flag)
c --
c --   This subroutine gets the path cost to the vehicle j.

c --
c -- INPUT
c --    j : vehicle ID
c --    i : current link
c -- iselect : see the description above
c --
c -- OUTPUT
c --    path for vehicle j
c --  
      use muc_mod
	use vector_mod
	integer::pathttmp(1000)=0
      integer Index1D,CurNode,Feasibility_Flag
      integer kvms

	real value, Path_Cost

c  -- define destination and origin
c  --
          icu1=i
          ifrom=idnod(i)
          ito=MasterDest(jdest(j))
          ict = 1

c  --
c  -- follow the shortest path code
c  --

            mov=ForToBackLink(icu1)-backpointr(ifrom)+1
	      know = 1
            k=CurNode
c --
       Path_Cost = LabelOut(lt(j),ioc(j),ito,ifrom,ict,know,mov)


	! check feasibility of this path
      pathttmp(:) = 0

c --
        do 20 while(ifrom.ne.destination(ito))
             if(know.eq.0) then
               know=1
             endif
             Index1D = k
             value = float(ifrom)
!             call VhcAtt_Insert(j,Index1D,1,value)
		   pathttmp(k) = ifrom
             k=k+1
             ifromtmp=ifrom
             ktemp=know
             movetemp=mov
             icttemp=ict
         ict = 1
         mov=  pathpointerout3(lt(j),ioc(j),ito,
     *                     ifromtmp,icttemp,ktemp,movetemp)
         know= pathpointerout2(lt(j),ioc(j),ito,
     *                      ifromtmp,icttemp,ktemp,movetemp)
         ifrom=pathpointerout1(lt(j),ioc(j),ito,
     *                      ifromtmp,icttemp,ktemp,movetemp)


          if(mov.lt.1.or.know.lt.1.or.ifrom.lt.1)then
! added by hayssam nov 12 for 930.8
      write(911,*) 'Error!'
	write(911,*) 
	write(911,*) 'No path exists from:'
	!write(911,*) 'Node',ifromtmp,'    to destination node'
	!modified by xuesong for 930.8B  Jan 2004
	write(911,*) 'Node',nodenum(ifromtmp),'    to destination node'
     +,destination(MasterDest(jdest(j))),
     + '   in zone',jdest(j)


	!write(911,*) 'error in get_veh_path'
      write(911,*) 'For vehicle number',j
	write(911,*) 'Generation link ',nodenum(iunod(isec(j))),
     +'    ->',nodenum(idnod(isec(j)))

      write(911,*) 
	write(911,*) 'Possible reasons:'
	write(911,*) 'No physical path exists, or'
	write(911,*) 'There is a prevented movement in movement.dat, or'
	write(911,*) 'There is a prevented movement due to signal setting'
	write(911,*)
	write(911,*) 'If this error resulted from a detour VMS, then	
     + check the sequence of nodes'
      write(911,*) 'along the detour path'

! Modified by Jason June 13 2003           
!	 write(911,*) 'check generation link ',nodenum(iunod(isec(j))),
!     +'->', nodenum(idnod(isec(j)))
!       write(911,*) 'destination zone',jdest(j)
!       write(911,*) 'destination node',
!     +  destination(MasterDest(jdest(j)))
!	     write(911,*) 'pathttmp(k)', pathttmp
           stop
	     exit
          endif 

20      continue

!        Feasibility_Flag = 1

!          do mm=1, vms(kvms,4)
		
!	Detour_Link = GetFLinkFromNode(idnum(Detoured_Nodes(kvms,2*mm-1)),
!     +	idnum(Detoured_Nodes(kvms,2*mm)))
	    !		do ii=CurNode, k-1  
!	  if(GetFLinkFromNode(pathttmp(ii),pathttmp(ii+1)).eq.Detour_Link)
!     +	 then
!          Feasibility_Flag = 0  !infeasible
!		exit !path is infeasible
!	   endif
!	    enddo
!	  enddo




      return
      end  


