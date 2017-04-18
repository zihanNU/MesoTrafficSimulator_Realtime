
         subroutine kspcost_calculate
c         include 'common.inc'
      use muc_mod
c -- modified by Xuesong Zhou 10 /26 /2002 for the k=1 case
c --
c -- This subroutie is for the main calculations of the k shortest paths.
c --
c -- This subroutine is called from ksp_main.
c -- This subroutine does not call any other subroutines.
c --
c -- INPUT:
c -- no specific input.
c -- OUTPUT:
c -- k shortest paths.
c -- 
c --
c --    for improper destinations
	NCountEquality=0

       if (BackPointr(Destin+1)-BackPointr(Destin).eq.0) then
         write (911,*) 'Unacceptable isolated destination', destin
         stop
       endif

       NGenericCounter=0
	
      iyy=iti_nu

C->DO B.1
	Do 201 While (FirstDeque .NE. INFINITY)

 
C        Take off the first NODE from the head of Deque
         CurrentNode=FirstDeque
         FirstDeque=StatusInDeque(CurrentNode)
         BackPointrCurrent=BackPointr(CurrentNode)
         NoOfArcsLeaving=BackPointr(CurrentNode+1)-BackPointrCurrent-1
         StatusInDeque(CurrentNode)=-1
 	   NgenericCounter=NgenericCounter+1


C--->DO B.2
         Do 202 I2=0,NoOfArcsLeaving
	   NTransient=BackPointr(CurrentNode)+I2
         Nodee=UNodeOfBackLink(NTransient)
	   Arc=BackPointrCurrent+i2
	   Movements=BackPointr(Nodee+1)-BackPointr(Nodee)
 	   IM=I2+1



!   Added by Xuesong and Jason June 11 2003
	    if( IM .gt.MaxMove) then
		IM = 1 
		! Reason 1: Only centriod will satisfy this condition:IM .gt.MaxMove  
		! Reason 2: Labels on the centriod are zero (the same) for all the movements
		! Reason 3: we do not have chances to rescan the centriod.
	    endif
! End of modification
		if(Movements .gt.MaxMove_current) then
		MaxMove_current = Movements;
		endif


	do itime=1,Iti_nu
		UpCounter(ITime)=DequeLabelCounter(CurrentNode,iTime,IM)
        enddo



	  IF (StatusInDeque(Nodee).NE.0) Then
	   do 333 iTime=1,Iti_nu
	     Do 2031 M=1,Movements
	      NextPenalty=TTPenalty(arc,ITime,M)

              NPenaltyArrivalIndex=NextPenalty/TimeInterval
	      ArrIndex=ITime+NPenaltyArrivalIndex
	      NextCost=ttmarginal(iTime,arc,M) 
              If(ArrIndex .gt. Iti_nu) ArrIndex=Iti_nu
              NextDistance=TTime(Arc,ArrIndex)
	      NArrivalTime=((NextDistance+NextPenalty)/
     *		TimeInterval)+ITime+1
	      If(NArrivalTime .gt. Iti_nu) NArrivalTime=Iti_nu
	    IDCounter=DequeLabelCounter(CurrentNode,NArrivalTime,IM)



             Do 203 I3=1,IDCounter
	      KPrevious=DequeLabel2(CurrentNode,NArrivalTime,I3,IM) 
              NewLabel=NextDistance+NextPenalty+
     *	       Label(CurrentNode,NArrivalTime,KPrevious,IM)
   
          ! Modified for weather VMS, June 8 2009
	    if(i_risk.eq.0) then
             NewLabelCost=NextCost+
     *       LabelCost(CurrentNode,NArrivalTime,KPrevious,IM) 
!    *          + cost(Arc,ltype,ioccup)
     +          + cost(Arc,ltype,ioccup,ArrIndex)
          else
             NewLabelCost=NextCost*(1+risk(Arc,ltype,ioccup,ArrIndex))+
     *       LabelCost(CurrentNode,NArrivalTime,KPrevious,IM) 
     +          + cost(Arc,ltype,ioccup,ArrIndex)
          endif     
! May 23 2005 TD link toll





C--->IF B.2S
              if (FirstGoodLabel(Nodee,ITime,M).GE.KPaths) Then
                MaxLabelCost=LabelCost(Nodee,ITime,FirstLabel
     *			(Nodee,ITime,M),M)
	        MaxLabel=Label(Nodee,ITime,FirstLabel
     *			(Nodee,ITime,M),M)
C--->IF B.3S
       If (NewLabelCost.LT. MaxLabelCost) Then

        Found=.FALSE.
        Update(M,ITime,I3)=.TRUE.
C---->IF B.3.1S
	 If(kay .EQ.1) then
          PathPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),1,M)=
     *		CurrentNode
          PathPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),2,M)=
     *		KPrevious
          PathPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),3,M)=
     *		IM
      	  PathPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),4,M)=
     *		NArrivalTime

          LabelCost(Nodee,ITime,FirstLabel(Nodee,ITime,M),M)=
     *	NewLabelCost
          Label(Nodee,ITime,FirstLabel(Nodee,ITime,M),M)=
     *	NewLabel

! Added by MTI team Feb 10 2004 0.930.9
        EmptyLabel=1


	else
			 	
        SecondLabel=LabelPointer(Nodee,iTime,
     *       FirstLabel(Nodee,ITime,M),M)
        EmptyLabel=FirstLabel(Nodee,Itime,M)

C---->IF B.4S       
        If (NewLabelCost.Ge.
     *    LabelCost(Nodee,itime,SecondLabel,M)) Then
          PathPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),1,M)=
     *		CurrentNode
          PathPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),2,M)=
     *		KPrevious
          PathPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),3,M)=
     *		IM
      	  PathPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),4,M)=
     *		NArrivalTime

          LabelCost(Nodee,ITime,FirstLabel(Nodee,ITime,M),M)=
     *	NewLabelCost
          Label(Nodee,ITime,FirstLabel(Nodee,ITime,M),M)=
     *	NewLabel
	  Else
C----|IF B.4E

          Ktemp=SecondLabel
          Know=LabelPointer(Nodee,ITime,SecondLabel,M)
          Do 7201, While ((Know .NE. NIL) .AND. (.NOT. Found))
            If (NewLabelCost .GE. 
     *	  LabelCost(Nodee,ITime,Know,M)) Then
              Found=.TRUE.
            Else
              KTemp=Know
              Know=LabelPointer(Nodee,ITime,Ktemp,M)
            EndIf
7201      Continue
         FirstLabel(Nodee,ITime,M)=SecondLabel
          LabelCost(Nodee,ITime,EmptyLabel,M)=NewLabelCost
          Label(Nodee,ITime,EmptyLabel,M)=NewLabel
          LabelPointer(Nodee,ITime,EmptyLabel,M)=Know
          LabelPointer(Nodee,ITime,Ktemp,M)=EmptyLabel
          PathPointer(Nodee,ITime,EmptyLabel,1,M)=CurrentNode
          PathPointer(Nodee,ITime,EmptyLabel,2,M)=KPrevious
          PathPointer(Nodee,ITime,EmptyLabel,3,M)=IM
          PathPointer(Nodee,ITime,EmptyLabel,4,M)=NArrivalTime
        EndIf
C----<IF B.4F
! Moved by MTI team Feb 10 2004 0.930.9
	ENDIF
C----<IF B.3.1F

	 I=1
	 Found=.False.
	 Do 7202, While ((.NOT.(Found)).AND.
     *     (I.LE.DequeLabelCounter(Nodee,ITime,M))) 
	   If (DequeLabel2(Nodee,ITime,I,M).EQ.EmptyLabel) Then
             DequeLabel1(Nodee,iTime,I,M)=NewLabel
	       DequeLabel1Cost(Nodee,iTime,I,M)=NewLabelCost
	     Found=.True.
	   Else
	     I=I+1
	   EndIf
7202	 Continue
	If (.NOT.(Found)) Then
	  DequeLabelCounter(Nodee,ITime,M)=
     *		DequeLabelCounter(Nodee,ITime,M)+1

          DequeLabel1(Nodee,ITime,DequeLabelCounter
     *		(Nodee,iTime,M),M)=NewLabel
	    DequeLabel1Cost(Nodee,ITime,DequeLabelCounter
     *		(Nodee,iTime,M),M)=NewLabelCost
          DequeLabel2(Nodee,ITime,DequeLabelCounter
     *		(Nodee,ITime,M),M)=EmptyLabel

	EndIf

! Moved by MTI team Feb 10 2004 0.930.9
!	ENDIF
!C----<IF B.3.1F


      Else
C---|IF B.3E
		Update(M,ITime,I3)=.False.

	      EndIf
C---<IF B.3F

            Else
C---|IF B.2E
 
       Update(M,ITime,I3)=.TRUE.
       Found=.FALSE.

       FirstGoodLabel(Nodee,ITime,M)=FirstGoodLabel(Nodee,ITime,M)+1
       Label(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),M)=NewLabel
        LabelCost(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),M)=
     *  NewLabelCost
C------>IF B.5S
       If (NewLabelCost.Ge. LabelCost(Nodee,ITime,
     *		FirstLabel(Nodee,ITime,M),M)) Then
        LabelPointer(Nodee,ITime,FirstGoodLabel(Nodee,ITIme,M),M)=
     *		FirstLabel(Nodee,ITime,M)
        FirstLabel(Nodee,ITime,M)=FirstGoodLabel(Nodee,ITime,M)
        PathPointer(Nodee,ITime,FirstGoodLabel(Nodee,iTime,M),1,M)=
     *		CurrentNode
        PathPointer(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),2,M)=
     *		KPrevious
        PathPointer(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),3,M)=
     *		IM
        PathPointer(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),4,M)=
     *  	NArrivalTime		
       Else
C------|IF B.5S
        Know=LabelPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),M)
        Ktemp=FirstLabel(Nodee,ITime,M)
        Do 8201, While ((Know .NE. NIL) .AND. (.NOT. Found))
          If (NewLabelCost .Ge. 
     *	LabelCost(Nodee,ITime,Know,M)) Then
            Found=.TRUE.
          Else
            Ktemp=Know
            Know=LabelPointer(Nodee,ITime,Ktemp,M)
          EndIf
8201     Continue
         LabelPointer(Nodee,ITime,FirstGoodLabel
     *		(Nodee,ITime,M),M)=Know
         LabelPointer(Nodee,ITime,Ktemp,M)=FirstGoodLabel(Nodee,ITime,M)
         PathPointer(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),1,M)=
     *		CurrentNode
         PathPointer(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),2,M)=
     *		KPrevious
         PathPointer(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),3,M)=
     *		IM
         PathPointer(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),4,M)=
     *		NArrivalTime
        Endif
C-----<IF B.5F

       DequeLabelCounter(Nodee,ITime,M)=
     *		DequeLabelCounter(Nodee,ITime,M)+1
       DequeLabel1(Nodee,ITime,DequeLabelCounter(Nodee,ITIme,M),M)=
     *		NewLabel
	   DequeLabel1Cost(Nodee,ITime,
     *   DequeLabelCounter(Nodee,ITIme,M),M)=NewLabelCost
         DequeLabel2(Nodee,iTime,DequeLabelCounter(Nodee,ITime,M),M)=
     *   FirstGoodLabel(Nodee,ITime,M)	
         EndIf
C---<IF B.2F

203      Continue

2031	  Continue

C  This part updates the Label of The Next Node without
c  the Intermovements.- Real Label

	  M=Movements+1
	  
!     Modified by MTI team Feb 07 2004 0.930.9
! We do not load entry queue on connectors, but on generation links
!	  NextDistance=TTpenalty(arc,ITime,M)

        NextDistance=0
	  NArrivalNoPen=NextDistance/TimeInterval+ITime+1
	  NextCost=0 ! Jan 12 2007


!     Modified by MTI team Feb 07 2004 0.930.9
! We do not load entry queue on connectors, but on generation links
!	  NextCost=ttmarginal(iTime,arc,M)

	  If(NArrivalNoPen.gt. Iti_nu) NArrivalNoPen=Iti_nu
	  
	  							  
	  IDCounter=DequeLabelCounter(CurrentNode,NArrivalTime,IM)
	  Do 2132 I3=1,IDCounter
	  KPrevious=DequeLabel2(CurrentNode,NarrivalTime,I3,IM)
          NewLabel=Label(CurrentNode,NArrivalNoPen,KPrevious,IM)+
     *	       NextDistance 
      ! Modified for weather VMS, June 8 2009
	    if(i_risk.eq.0) then
	      NewLabelCost=LabelCost
     *        (CurrentNode,NArrivalNoPen,KPrevious,IM)
     *	      + NextCost
!    *          + cost(Arc,ltype,ioccup)
     +          + cost(Arc,ltype,ioccup,NArrivalNoPen)
          else
	      NewLabelCost=LabelCost
     *        (CurrentNode,NArrivalNoPen,KPrevious,IM)
     *	      + NextCost*(1+risk(Arc,ltype,ioccup,ArrIndex))
     +          + cost(Arc,ltype,ioccup,NArrivalNoPen)
          endif
! May 23 2005 TD link toll

C--->IF B.2rS
          If (FirstGoodLabel(Nodee,iTime,M).GE.KPaths) Then

             MaxLabel=Label(Nodee,ITime,FirstLabel(Nodee,ITime,M),M)
	       MaxLabelCost=
     *       LabelCost(Nodee,ITime,FirstLabel(Nodee,ITime,M),M)
C--->IF B.3rS
             If (NewLabelCost.LT. MaxLabelCost) Then

        Found=.FALSE.
C--->IF B.3.1rS	
		If(Kay .EQ. 1) then
          PathPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),1,M)=
     *		CurrentNode
          PathPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),2,M)=
     *		KPrevious
          PathPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),3,M)=
     *		IM
          PathPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),4,M)=
     *		NArrivalNoPen

          Label(Nodee,ITime,FirstLabel(Nodee,ITime,M),M)=NewLabel
         LabelCost(Nodee,ITime,FirstLabel(Nodee,ITime,M),M)=
     *   NewLabelCost  
	
C---->IF B.3.1rE       
	else

        SecondLabel=LabelPointer(Nodee,ITime,FirstLabel
     *	   (Nodee,iTime,M),M)
C---->IF B.4rS       

        If (NewLabelCost.Ge.
     *    LabelCost(Nodee,ITime,SecondLabel,M)) Then
          PathPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),1,M)=
     *		CurrentNode
          PathPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),2,M)=
     *		KPrevious
          PathPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),3,M)=
     *		IM
          PathPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),4,M)=
     *		NArrivalNoPen

          Label(Nodee,ITime,FirstLabel(Nodee,ITime,M),M)=NewLabel
         LabelCost(Nodee,ITime,FirstLabel(Nodee,ITime,M),M)=
     *   NewLabelCost  
        Else
C----|IF B.4rE

        EmptyLabel=FirstLabel(Nodee,ITime,M)
        Ktemp=SecondLabel
        Know=LabelPointer(Nodee,ITime,SecondLabel,M)
        Do 7211, While ((Know .NE. NIL) .AND. (.NOT. Found))
          If (NewLabelCost.GE. 
     *    	LabelCost(Nodee,iTime,Know,M)) Then
            Found=.TRUE.
          Else
            KTemp=Know
            Know=LabelPointer(Nodee,iTime,Ktemp,M)
          EndIf
7211     Continue
         FirstLabel(Nodee,iTime,M)=SecondLabel

         Label(Nodee,iTime,EmptyLabel,M)=NewLabel
         LabelCost(Nodee,iTime,EmptyLabel,M)=NewLabelCost  
         LabelPointer(Nodee,iTime,EmptyLabel,M)=Know


         LabelPointer(Nodee,iTime,Ktemp,M)=EmptyLabel


         PathPointer(Nodee,iTime,EmptyLabel,1,M)=CurrentNode
	 PathPointer(Nodee,iTime,EmptyLabel,2,M)=KPrevious
 	 PathPointer(Nodee,iTime,EmptyLabel,3,M)=IM
 	 PathPointer(Nodee,iTime,EmptyLabel,4,M)=NArrivalNoPen

         EndIf
C----<IF B.4rF
	   Endif	
C----<IF B.3.1rF
         EndIf
C----<IF B.3rF
            Else
 
        Found=.FALSE.
        FirstGoodLabel(Nodee,Itime,M)=FirstGoodLabel(Nodee,ITime,M)+1

        Label(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),M)=NewLabel
	  LabelCost(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),M)=
     *  NewLabelCost
        PathPointer(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),1,M)=
     *		CurrentNode
        PathPointer(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),2,M)=
     *		KPrevious
        PathPointer(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),3,M)=
     *		IM
        PathPointer(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),4,M)=
     *		NArrivalNoPen

C------>IF B.5rS
       If (NewLabelCost .Ge. LabelCost(Nodee,itime,FirstLabel
     * 		(Nodee,ITime,M),M)) Then

        LabelPointer(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),M)=
     *		FirstLabel(Nodee,ITime,M)

        FirstLabel(Nodee,ITime,M)=FirstGoodLabel(Nodee,ITime,M)
       Else
C------|IF B.5rE
        Know=LabelPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),M)
        Ktemp=FirstLabel(Nodee,ITime,M)
        Do 8211, While ((Know .NE. NIL) .AND. (.NOT. Found))
          If (NewLabelCost.Ge.
     *	 Labelcost(Nodee,ITime,Know,M)) Then
            Found=.TRUE.
          Else
            Ktemp=Know
            Know=LabelPointer(Nodee,ITime,Ktemp,M)
          EndIf
8211     Continue
         LabelPointer(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),M)=
     *		Know

         LabelPointer(Nodee,ITime,Ktemp,M)=FirstGoodLabel
     *		(Nodee,ITime,M)
        Endif
C-----<IF B.5rF

            EndIf
C---<IF B.2F
2132	continue


c  To the next time interval at Node.
333	continue
c  I have moved this out of the loop because we may need these counters
c  multiple times as we discover the NArrival Times
        
        Do 2222 ITime=1,Iti_nu
	    DequeLabelCounter(CurrentNode,ITime,IM)=0
2222	continue

c---------------------------------------------------------------------
c  If Node has not been scanned previously:
       Else
C---<IF B.1E
	Do 334 NTime=1,Iti_nu


	 DequeLabelCounter(CurrentNode,NTime,IM)=0



         Do 9031 M=1,Movements

	  NextPenalty=TTPenalty(Arc,NTime,M)
          NPenaltyArrivalIndex=NextPenalty/TimeInterval
	  ArrIndex=Ntime+NPenaltyArrivalIndex
	  If(ArrIndex .gt. Iti_nu)ArrIndex=Iti_nu
	  
          NextDistance=TTime(Arc,ArrIndex)
	  NArrivalTime=((NextPenalty+NextDistance)/
     *		TimeInterval)+NTime+1
	  NextCost=ttmarginal(NTime,arc,M)
	  If(NArrivalTime .gt. Iti_nu) NArrivalTime=Iti_nu
          FirstGoodLabel(Nodee,NTime,M)=FirstGoodLabel
     *		(CurrentNode,NTime,IM)


         FirstLabel(Nodee,NTime,M)=FirstLabel(CurrentNode,NTime,IM)
         DequeLabelCounter(Nodee,NTime,M)=FirstGoodLabel(Nodee,NTime,M)


	 Do 9199 IK=1,FirstGoodLabel(CurrentNode,NTime,IM)
	   Label(Nodee,NTime,IK,M)=NextDistance+NextPenalty+
     *		Label(CurrentNode,NArrivalTime,IK,IM)
         ! Modified for weather VMS, June 8 2009
	   if(i_risk.eq.0) then
	     LabelCost(Nodee,NTime,IK,M)=
     *		LabelCost(CurrentNode,NArrivalTime,IK,IM)
     *      +NextCost
!    *        + cost(Arc,ltype,ioccup)
     +        + cost(Arc,ltype,ioccup,ArrIndex)
         else
	     LabelCost(Nodee,NTime,IK,M)=
     *		LabelCost(CurrentNode,NArrivalTime,IK,IM)
     *      +NextCost*(1+risk(Arc,ltype,ioccup,ArrIndex))
     +        + cost(Arc,ltype,ioccup,ArrIndex)
         endif 
!May 23 2005 TD link toll


           LabelPointer(Nodee,NTime,IK,M)=LabelPointer
     *		(CurrentNode,NTime,IK,IM)
     
           PathPointer(Nodee,NTime,IK,1,M)=CurrentNode
           PathPointer(Nodee,NTime,IK,2,M)=IK
           PathPointer(Nodee,NTime,IK,3,M)=IM
           PathPointer(Nodee,NTime,IK,4,M)=NArrivalTime

           DequeLabel1(Nodee,NTime,IK,M)=Label(Nodee,NTime,IK,M)
	   DequeLabel1Cost(Nodee,NTime,IK,M)=
     *   LabelCost(Nodee,NTime,IK,M)
           DequeLabel2(Nodee,NTime,IK,M)=IK
9199     Continue
         Update(M,NTime,1)=.True.
9031    Continue

	 M=Movements+1
	 
!     Modified by MTI team Feb 07 2004 0.930.9
! We do not load entry queue on connectors, but on generation links
!	 NextDistance=TTpenalty(arc,NTime,M)
	 NextDistance=0

	 NArrivalNoPen=NextDistance/TimeInterval+NTime+1

!     Modified by MTI team Feb 07 2004 0.930.9
! We do not load entry queue on connectors, but on generation links
!	 NextCost=ttmarginal(Ntime,arc,M)
	 NextCost=0

	 If(NArrivalNoPen .gt.Iti_nu ) NArrivalNoPen=Iti_nu
         FirstGoodLabel(Nodee,NTime,M)=FirstGoodLabel
     *		(CurrentNode,NTime,IM)

         FirstLabel(Nodee,NTime,M)=FirstLabel(CurrentNode,NTime,IM)

	 Do 9299 IO=1,FirstGoodLabel(CurrentNode,NTime,IM)
	   Label(Nodee,NTime,IO,M)=Label(CurrentNode,
     *		NArrivalNoPen,IO,IM)+NextDistance
          ! Modified for weather VMS, June 8 2009
	    if(i_risk.eq.0) then
      	    LabelCost(Nodee,NTime,IO,M)=LabelCost(CurrentNode,
     *		NArrivalNoPen,IO,IM)
     *         +NextCost
!    *         + cost(Arc,ltype,ioccup)
     +         + cost(Arc,ltype,ioccup,NArrivalNoPen)
          else
      	    LabelCost(Nodee,NTime,IO,M)=LabelCost(CurrentNode,
     *		NArrivalNoPen,IO,IM)
     *         +NextCost*(1+risk(Arc,ltype,ioccup,ArrIndex))
     +         + cost(Arc,ltype,ioccup,NArrivalNoPen)
          endif
! May 23 2005 TD link toll

      
	
          LabelPointer(Nodee,NTime,IO,M)=
     *		LabelPointer(CurrentNode,NTime,IO,IM)

           PathPointer(Nodee,NTime,IO,1,M)=CurrentNode
           PathPointer(Nodee,NTime,IO,2,M)=IO
           PathPointer(Nodee,NTime,IO,3,M)=IM
           PathPointer(Nodee,NTime,IO,4,M)=NArrivalNoPen
9299     Continue
         Update(M,NTime,1)=.True. ! Jan 12 2007
334	continue
       Endif

C--->IF B.1F

Comment B.2: Check the Update Status and Insert the Node in the Deque
	 UpdateCombined=.FALSE.
	Do 2033 ITime=1,Iti_nu
	 IDCounter=DequeLabelCounter(CurrentNode,ITime,IM)
	 Do 2032, I3=1,UpCounter(ITime)
	 Do 2032, M=1,Movements+1 ! Jan 12 2007
	    If (Update(M,ITime,I3))  Then
	      UpdateCombined=.TRUE.
	      Update(M,ITime,I3)=.False.
	    EndIf
2032	  Continue
2033    continue

          If (UpdateCombined)  Then 
        If (StatusInDeque(Nodee) .EQ. 0) Then
           If (FirstDeque .NE. INFINITY) Then
              StatusInDeque(LastDeque)=Nodee
              StatusInDeque(Nodee)=INFINITY
              LastDeque=Nodee
           Else
              StatusInDeque(Nodee)=FirstDeque
              FirstDeque=Nodee
              LastDeque=Nodee
           EndIf
        Else
           If (StatusInDeque(Nodee) .EQ. -1) Then
              If (FirstDeque.EQ.INFINITY) LastDeque=Nodee
              StatusInDeque(Nodee)=FirstDeque
              FirstDeque=Nodee
           EndIf
        EndIf

       EndIf

202     Continue
C--<DO B.2

201   Continue
C-<DO B.1
901	Format (I10)
      return
      end




! Added by zihan 20160616 for AMS
! The change here is iti_nu is updated to iti_nu+current minute
! Since there is a memory issue, iti_nu cannot be set to stagelength
! So what we are doing is to use the the current minute from the next prediction horizon for TDSP

      subroutine kspcost_calculate_AMS
c         include 'common.inc'
      use muc_mod
c -- modified by Xuesong Zhou 10 /26 /2002 for the k=1 case
c --
c -- This subroutie is for the main calculations of the k shortest paths.
c --
c -- This subroutine is called from ksp_main.
c -- This subroutine does not call any other subroutines.
c --
c -- INPUT:
c -- no specific input.
c -- OUTPUT:
c -- k shortest paths.
c -- 
c --
c --    for improper destinations
	NCountEquality=0

       if (BackPointr(Destin+1)-BackPointr(Destin).eq.0) then
         write (911,*) 'Unacceptable isolated destination', destin
         stop
       endif

       NGenericCounter=0
	
      iyy=iti_nu

C->DO B.1
	Do 201 While (FirstDeque .NE. INFINITY)

 
C        Take off the first NODE from the head of Deque
         CurrentNode=FirstDeque
         FirstDeque=StatusInDeque(CurrentNode)
         BackPointrCurrent=BackPointr(CurrentNode)
         NoOfArcsLeaving=BackPointr(CurrentNode+1)-BackPointrCurrent-1
         StatusInDeque(CurrentNode)=-1
 	   NgenericCounter=NgenericCounter+1


C--->DO B.2
         Do 202 I2=0,NoOfArcsLeaving
	   NTransient=BackPointr(CurrentNode)+I2
         Nodee=UNodeOfBackLink(NTransient)
	   Arc=BackPointrCurrent+i2
	   Movements=BackPointr(Nodee+1)-BackPointr(Nodee)
 	   IM=I2+1



!   Added by Xuesong and Jason June 11 2003
	    if( IM .gt.MaxMove) then
		IM = 1 
		! Reason 1: Only centriod will satisfy this condition:IM .gt.MaxMove  
		! Reason 2: Labels on the centriod are zero (the same) for all the movements
		! Reason 3: we do not have chances to rescan the centriod.
	    endif
! End of modification
		if(Movements .gt.MaxMove_current) then
		MaxMove_current = Movements;
		endif
      
      Iti_nu_AMS=Iti_nu+nint(time_now/60)

	do itime=nint(time_now/60),Iti_nu_AMS
		UpCounter(ITime)=DequeLabelCounter(CurrentNode,iTime,IM)
        enddo



	  IF (StatusInDeque(Nodee).NE.0) Then
	   do 333 ITime=nint(time_now/60),Iti_nu_AMS
	     Do 2031 M=1,Movements
	      NextPenalty=TTPenalty(arc,ITime,M)

              NPenaltyArrivalIndex=NextPenalty/TimeInterval
	      ArrIndex=ITime+NPenaltyArrivalIndex
	      NextCost=ttmarginal(iTime,arc,M) 
              If(ArrIndex .gt. Iti_nu_AMS) ArrIndex=Iti_nu_AMS
              NextDistance=TTime(Arc,ArrIndex)
	      NArrivalTime=((NextDistance+NextPenalty)/
     *		TimeInterval)+ITime+1
	      If(NArrivalTime .gt. Iti_nu_AMS) NArrivalTime=Iti_nu_AMS
	    IDCounter=DequeLabelCounter(CurrentNode,NArrivalTime,IM)



             Do 203 I3=1,IDCounter
	      KPrevious=DequeLabel2(CurrentNode,NArrivalTime,I3,IM) 
              NewLabel=NextDistance+NextPenalty+
     *	       Label(CurrentNode,NArrivalTime,KPrevious,IM)
   
          ! Modified for weather VMS, June 8 2009
	    if(i_risk.eq.0) then
             NewLabelCost=NextCost+
     *       LabelCost(CurrentNode,NArrivalTime,KPrevious,IM) 
!    *          + cost(Arc,ltype,ioccup)
     +          + cost(Arc,ltype,ioccup,ArrIndex)
          else
             NewLabelCost=NextCost*(1+risk(Arc,ltype,ioccup,ArrIndex))+
     *       LabelCost(CurrentNode,NArrivalTime,KPrevious,IM) 
     +          + cost(Arc,ltype,ioccup,ArrIndex)
          endif     
! May 23 2005 TD link toll





C--->IF B.2S
              if (FirstGoodLabel(Nodee,ITime,M).GE.KPaths) Then
                MaxLabelCost=LabelCost(Nodee,ITime,FirstLabel
     *			(Nodee,ITime,M),M)
	        MaxLabel=Label(Nodee,ITime,FirstLabel
     *			(Nodee,ITime,M),M)
C--->IF B.3S
       If (NewLabelCost.LT. MaxLabelCost) Then

        Found=.FALSE.
        Update(M,ITime,I3)=.TRUE.
C---->IF B.3.1S
	 If(kay .EQ.1) then
          PathPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),1,M)=
     *		CurrentNode
          PathPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),2,M)=
     *		KPrevious
          PathPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),3,M)=
     *		IM
      	  PathPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),4,M)=
     *		NArrivalTime

          LabelCost(Nodee,ITime,FirstLabel(Nodee,ITime,M),M)=
     *	NewLabelCost
          Label(Nodee,ITime,FirstLabel(Nodee,ITime,M),M)=
     *	NewLabel

! Added by MTI team Feb 10 2004 0.930.9
        EmptyLabel=1


	else
			 	
        SecondLabel=LabelPointer(Nodee,iTime,
     *       FirstLabel(Nodee,ITime,M),M)
        EmptyLabel=FirstLabel(Nodee,Itime,M)

C---->IF B.4S       
        If (NewLabelCost.Ge.
     *    LabelCost(Nodee,itime,SecondLabel,M)) Then
          PathPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),1,M)=
     *		CurrentNode
          PathPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),2,M)=
     *		KPrevious
          PathPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),3,M)=
     *		IM
      	  PathPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),4,M)=
     *		NArrivalTime

          LabelCost(Nodee,ITime,FirstLabel(Nodee,ITime,M),M)=
     *	NewLabelCost
          Label(Nodee,ITime,FirstLabel(Nodee,ITime,M),M)=
     *	NewLabel
	  Else
C----|IF B.4E

          Ktemp=SecondLabel
          Know=LabelPointer(Nodee,ITime,SecondLabel,M)
          Do 7201, While ((Know .NE. NIL) .AND. (.NOT. Found))
            If (NewLabelCost .GE. 
     *	  LabelCost(Nodee,ITime,Know,M)) Then
              Found=.TRUE.
            Else
              KTemp=Know
              Know=LabelPointer(Nodee,ITime,Ktemp,M)
            EndIf
7201      Continue
         FirstLabel(Nodee,ITime,M)=SecondLabel
          LabelCost(Nodee,ITime,EmptyLabel,M)=NewLabelCost
          Label(Nodee,ITime,EmptyLabel,M)=NewLabel
          LabelPointer(Nodee,ITime,EmptyLabel,M)=Know
          LabelPointer(Nodee,ITime,Ktemp,M)=EmptyLabel
          PathPointer(Nodee,ITime,EmptyLabel,1,M)=CurrentNode
          PathPointer(Nodee,ITime,EmptyLabel,2,M)=KPrevious
          PathPointer(Nodee,ITime,EmptyLabel,3,M)=IM
          PathPointer(Nodee,ITime,EmptyLabel,4,M)=NArrivalTime
        EndIf
C----<IF B.4F
! Moved by MTI team Feb 10 2004 0.930.9
	ENDIF
C----<IF B.3.1F

	 I=1
	 Found=.False.
	 Do 7202, While ((.NOT.(Found)).AND.
     *     (I.LE.DequeLabelCounter(Nodee,ITime,M))) 
	   If (DequeLabel2(Nodee,ITime,I,M).EQ.EmptyLabel) Then
             DequeLabel1(Nodee,iTime,I,M)=NewLabel
	       DequeLabel1Cost(Nodee,iTime,I,M)=NewLabelCost
	     Found=.True.
	   Else
	     I=I+1
	   EndIf
7202	 Continue
	If (.NOT.(Found)) Then
	  DequeLabelCounter(Nodee,ITime,M)=
     *		DequeLabelCounter(Nodee,ITime,M)+1

          DequeLabel1(Nodee,ITime,DequeLabelCounter
     *		(Nodee,iTime,M),M)=NewLabel
	    DequeLabel1Cost(Nodee,ITime,DequeLabelCounter
     *		(Nodee,iTime,M),M)=NewLabelCost
          DequeLabel2(Nodee,ITime,DequeLabelCounter
     *		(Nodee,ITime,M),M)=EmptyLabel

	EndIf

! Moved by MTI team Feb 10 2004 0.930.9
!	ENDIF
!C----<IF B.3.1F


      Else
C---|IF B.3E
		Update(M,ITime,I3)=.False.

	      EndIf
C---<IF B.3F

            Else
C---|IF B.2E
 
       Update(M,ITime,I3)=.TRUE.
       Found=.FALSE.

       FirstGoodLabel(Nodee,ITime,M)=FirstGoodLabel(Nodee,ITime,M)+1
       Label(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),M)=NewLabel
        LabelCost(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),M)=
     *  NewLabelCost
C------>IF B.5S
       If (NewLabelCost.Ge. LabelCost(Nodee,ITime,
     *		FirstLabel(Nodee,ITime,M),M)) Then
        LabelPointer(Nodee,ITime,FirstGoodLabel(Nodee,ITIme,M),M)=
     *		FirstLabel(Nodee,ITime,M)
        FirstLabel(Nodee,ITime,M)=FirstGoodLabel(Nodee,ITime,M)
        PathPointer(Nodee,ITime,FirstGoodLabel(Nodee,iTime,M),1,M)=
     *		CurrentNode
        PathPointer(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),2,M)=
     *		KPrevious
        PathPointer(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),3,M)=
     *		IM
        PathPointer(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),4,M)=
     *  	NArrivalTime		
       Else
C------|IF B.5S
        Know=LabelPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),M)
        Ktemp=FirstLabel(Nodee,ITime,M)
        Do 8201, While ((Know .NE. NIL) .AND. (.NOT. Found))
          If (NewLabelCost .Ge. 
     *	LabelCost(Nodee,ITime,Know,M)) Then
            Found=.TRUE.
          Else
            Ktemp=Know
            Know=LabelPointer(Nodee,ITime,Ktemp,M)
          EndIf
8201     Continue
         LabelPointer(Nodee,ITime,FirstGoodLabel
     *		(Nodee,ITime,M),M)=Know
         LabelPointer(Nodee,ITime,Ktemp,M)=FirstGoodLabel(Nodee,ITime,M)
         PathPointer(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),1,M)=
     *		CurrentNode
         PathPointer(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),2,M)=
     *		KPrevious
         PathPointer(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),3,M)=
     *		IM
         PathPointer(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),4,M)=
     *		NArrivalTime
        Endif
C-----<IF B.5F

       DequeLabelCounter(Nodee,ITime,M)=
     *		DequeLabelCounter(Nodee,ITime,M)+1
       DequeLabel1(Nodee,ITime,DequeLabelCounter(Nodee,ITIme,M),M)=
     *		NewLabel
	   DequeLabel1Cost(Nodee,ITime,
     *   DequeLabelCounter(Nodee,ITIme,M),M)=NewLabelCost
         DequeLabel2(Nodee,iTime,DequeLabelCounter(Nodee,ITime,M),M)=
     *   FirstGoodLabel(Nodee,ITime,M)	
         EndIf
C---<IF B.2F

203      Continue

2031	  Continue

C  This part updates the Label of The Next Node without
c  the Intermovements.- Real Label

	  M=Movements+1
	  
!     Modified by MTI team Feb 07 2004 0.930.9
! We do not load entry queue on connectors, but on generation links
!	  NextDistance=TTpenalty(arc,ITime,M)

        NextDistance=0
	  NArrivalNoPen=NextDistance/TimeInterval+ITime+1
	  NextCost=0 ! Jan 12 2007


!     Modified by MTI team Feb 07 2004 0.930.9
! We do not load entry queue on connectors, but on generation links
!	  NextCost=ttmarginal(iTime,arc,M)

	  If(NArrivalNoPen.gt. Iti_nu_AMS) NArrivalNoPen=Iti_nu_AMS
	  
	  							  
	  IDCounter=DequeLabelCounter(CurrentNode,NArrivalTime,IM)
	  Do 2132 I3=1,IDCounter
	  KPrevious=DequeLabel2(CurrentNode,NarrivalTime,I3,IM)
          NewLabel=Label(CurrentNode,NArrivalNoPen,KPrevious,IM)+
     *	       NextDistance 
      ! Modified for weather VMS, June 8 2009
	    if(i_risk.eq.0) then
	      NewLabelCost=LabelCost
     *        (CurrentNode,NArrivalNoPen,KPrevious,IM)
     *	      + NextCost
!    *          + cost(Arc,ltype,ioccup)
     +          + cost(Arc,ltype,ioccup,NArrivalNoPen)
          else
	      NewLabelCost=LabelCost
     *        (CurrentNode,NArrivalNoPen,KPrevious,IM)
     *	      + NextCost*(1+risk(Arc,ltype,ioccup,ArrIndex))
     +          + cost(Arc,ltype,ioccup,NArrivalNoPen)
          endif
! May 23 2005 TD link toll

C--->IF B.2rS
          If (FirstGoodLabel(Nodee,iTime,M).GE.KPaths) Then

             MaxLabel=Label(Nodee,ITime,FirstLabel(Nodee,ITime,M),M)
	       MaxLabelCost=
     *       LabelCost(Nodee,ITime,FirstLabel(Nodee,ITime,M),M)
C--->IF B.3rS
             If (NewLabelCost.LT. MaxLabelCost) Then

        Found=.FALSE.
C--->IF B.3.1rS	
		If(Kay .EQ. 1) then
          PathPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),1,M)=
     *		CurrentNode
          PathPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),2,M)=
     *		KPrevious
          PathPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),3,M)=
     *		IM
          PathPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),4,M)=
     *		NArrivalNoPen

          Label(Nodee,ITime,FirstLabel(Nodee,ITime,M),M)=NewLabel
         LabelCost(Nodee,ITime,FirstLabel(Nodee,ITime,M),M)=
     *   NewLabelCost  
	
C---->IF B.3.1rE       
	else

        SecondLabel=LabelPointer(Nodee,ITime,FirstLabel
     *	   (Nodee,iTime,M),M)
C---->IF B.4rS       

        If (NewLabelCost.Ge.
     *    LabelCost(Nodee,ITime,SecondLabel,M)) Then
          PathPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),1,M)=
     *		CurrentNode
          PathPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),2,M)=
     *		KPrevious
          PathPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),3,M)=
     *		IM
          PathPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),4,M)=
     *		NArrivalNoPen

          Label(Nodee,ITime,FirstLabel(Nodee,ITime,M),M)=NewLabel
         LabelCost(Nodee,ITime,FirstLabel(Nodee,ITime,M),M)=
     *   NewLabelCost  
        Else
C----|IF B.4rE

        EmptyLabel=FirstLabel(Nodee,ITime,M)
        Ktemp=SecondLabel
        Know=LabelPointer(Nodee,ITime,SecondLabel,M)
        Do 7211, While ((Know .NE. NIL) .AND. (.NOT. Found))
          If (NewLabelCost.GE. 
     *    	LabelCost(Nodee,iTime,Know,M)) Then
            Found=.TRUE.
          Else
            KTemp=Know
            Know=LabelPointer(Nodee,iTime,Ktemp,M)
          EndIf
7211     Continue
         FirstLabel(Nodee,iTime,M)=SecondLabel

         Label(Nodee,iTime,EmptyLabel,M)=NewLabel
         LabelCost(Nodee,iTime,EmptyLabel,M)=NewLabelCost  
         LabelPointer(Nodee,iTime,EmptyLabel,M)=Know


         LabelPointer(Nodee,iTime,Ktemp,M)=EmptyLabel


         PathPointer(Nodee,iTime,EmptyLabel,1,M)=CurrentNode
	 PathPointer(Nodee,iTime,EmptyLabel,2,M)=KPrevious
 	 PathPointer(Nodee,iTime,EmptyLabel,3,M)=IM
 	 PathPointer(Nodee,iTime,EmptyLabel,4,M)=NArrivalNoPen

         EndIf
C----<IF B.4rF
	   Endif	
C----<IF B.3.1rF
         EndIf
C----<IF B.3rF
            Else
 
        Found=.FALSE.
        FirstGoodLabel(Nodee,Itime,M)=FirstGoodLabel(Nodee,ITime,M)+1

        Label(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),M)=NewLabel
	  LabelCost(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),M)=
     *  NewLabelCost
        PathPointer(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),1,M)=
     *		CurrentNode
        PathPointer(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),2,M)=
     *		KPrevious
        PathPointer(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),3,M)=
     *		IM
        PathPointer(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),4,M)=
     *		NArrivalNoPen

C------>IF B.5rS
       If (NewLabelCost .Ge. LabelCost(Nodee,itime,FirstLabel
     * 		(Nodee,ITime,M),M)) Then

        LabelPointer(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),M)=
     *		FirstLabel(Nodee,ITime,M)

        FirstLabel(Nodee,ITime,M)=FirstGoodLabel(Nodee,ITime,M)
       Else
C------|IF B.5rE
        Know=LabelPointer(Nodee,ITime,FirstLabel(Nodee,ITime,M),M)
        Ktemp=FirstLabel(Nodee,ITime,M)
        Do 8211, While ((Know .NE. NIL) .AND. (.NOT. Found))
          If (NewLabelCost.Ge.
     *	 Labelcost(Nodee,ITime,Know,M)) Then
            Found=.TRUE.
          Else
            Ktemp=Know
            Know=LabelPointer(Nodee,ITime,Ktemp,M)
          EndIf
8211     Continue
         LabelPointer(Nodee,ITime,FirstGoodLabel(Nodee,ITime,M),M)=
     *		Know

         LabelPointer(Nodee,ITime,Ktemp,M)=FirstGoodLabel
     *		(Nodee,ITime,M)
        Endif
C-----<IF B.5rF

            EndIf
C---<IF B.2F
2132	continue


c  To the next time interval at Node.
333	continue
c  I have moved this out of the loop because we may need these counters
c  multiple times as we discover the NArrival Times
        
        Do 2222 ITime=nint(time_now/60),Iti_nu_AMS
	    DequeLabelCounter(CurrentNode,ITime,IM)=0
2222	continue

c---------------------------------------------------------------------
c  If Node has not been scanned previously:
       Else
C---<IF B.1E
	Do 334 NTime=nint(time_now/60),Iti_nu_AMS

	 DequeLabelCounter(CurrentNode,NTime,IM)=0



         Do 9031 M=1,Movements

	  NextPenalty=TTPenalty(Arc,NTime,M)
          NPenaltyArrivalIndex=NextPenalty/TimeInterval
	  ArrIndex=Ntime+NPenaltyArrivalIndex
	  If(ArrIndex .gt. Iti_nu_AMS)ArrIndex=Iti_nu_AMS
	  
          NextDistance=TTime(Arc,ArrIndex)
	  NArrivalTime=((NextPenalty+NextDistance)/
     *		TimeInterval)+NTime+1
	  NextCost=ttmarginal(NTime,arc,M)
	  If(NArrivalTime .gt. Iti_nu_AMS) NArrivalTime=Iti_nu_AMS
          FirstGoodLabel(Nodee,NTime,M)=FirstGoodLabel
     *		(CurrentNode,NTime,IM)


         FirstLabel(Nodee,NTime,M)=FirstLabel(CurrentNode,NTime,IM)
         DequeLabelCounter(Nodee,NTime,M)=FirstGoodLabel(Nodee,NTime,M)


	 Do 9199 IK=1,FirstGoodLabel(CurrentNode,NTime,IM)
	   Label(Nodee,NTime,IK,M)=NextDistance+NextPenalty+
     *		Label(CurrentNode,NArrivalTime,IK,IM)
         ! Modified for weather VMS, June 8 2009
	   if(i_risk.eq.0) then
	     LabelCost(Nodee,NTime,IK,M)=
     *		LabelCost(CurrentNode,NArrivalTime,IK,IM)
     *      +NextCost
!    *        + cost(Arc,ltype,ioccup)
     +        + cost(Arc,ltype,ioccup,ArrIndex)
         else
	     LabelCost(Nodee,NTime,IK,M)=
     *		LabelCost(CurrentNode,NArrivalTime,IK,IM)
     *      +NextCost*(1+risk(Arc,ltype,ioccup,ArrIndex))
     +        + cost(Arc,ltype,ioccup,ArrIndex)
         endif 
!May 23 2005 TD link toll


           LabelPointer(Nodee,NTime,IK,M)=LabelPointer
     *		(CurrentNode,NTime,IK,IM)
     
           PathPointer(Nodee,NTime,IK,1,M)=CurrentNode
           PathPointer(Nodee,NTime,IK,2,M)=IK
           PathPointer(Nodee,NTime,IK,3,M)=IM
           PathPointer(Nodee,NTime,IK,4,M)=NArrivalTime

           DequeLabel1(Nodee,NTime,IK,M)=Label(Nodee,NTime,IK,M)
	   DequeLabel1Cost(Nodee,NTime,IK,M)=
     *   LabelCost(Nodee,NTime,IK,M)
           DequeLabel2(Nodee,NTime,IK,M)=IK
9199     Continue
         Update(M,NTime,1)=.True.
9031    Continue

	 M=Movements+1
	 
!     Modified by MTI team Feb 07 2004 0.930.9
! We do not load entry queue on connectors, but on generation links
!	 NextDistance=TTpenalty(arc,NTime,M)
	 NextDistance=0

	 NArrivalNoPen=NextDistance/TimeInterval+NTime+1

!     Modified by MTI team Feb 07 2004 0.930.9
! We do not load entry queue on connectors, but on generation links
!	 NextCost=ttmarginal(Ntime,arc,M)
	 NextCost=0

	 If(NArrivalNoPen .gt.Iti_nu_AMS ) NArrivalNoPen=Iti_nu_AMS
         FirstGoodLabel(Nodee,NTime,M)=FirstGoodLabel
     *		(CurrentNode,NTime,IM)

         FirstLabel(Nodee,NTime,M)=FirstLabel(CurrentNode,NTime,IM)

	 Do 9299 IO=1,FirstGoodLabel(CurrentNode,NTime,IM)
	   Label(Nodee,NTime,IO,M)=Label(CurrentNode,
     *		NArrivalNoPen,IO,IM)+NextDistance
          ! Modified for weather VMS, June 8 2009
	    if(i_risk.eq.0) then
      	    LabelCost(Nodee,NTime,IO,M)=LabelCost(CurrentNode,
     *		NArrivalNoPen,IO,IM)
     *         +NextCost
!    *         + cost(Arc,ltype,ioccup)
     +         + cost(Arc,ltype,ioccup,NArrivalNoPen)
          else
      	    LabelCost(Nodee,NTime,IO,M)=LabelCost(CurrentNode,
     *		NArrivalNoPen,IO,IM)
     *         +NextCost*(1+risk(Arc,ltype,ioccup,ArrIndex))
     +         + cost(Arc,ltype,ioccup,NArrivalNoPen)
          endif
! May 23 2005 TD link toll

      
	
          LabelPointer(Nodee,NTime,IO,M)=
     *		LabelPointer(CurrentNode,NTime,IO,IM)

           PathPointer(Nodee,NTime,IO,1,M)=CurrentNode
           PathPointer(Nodee,NTime,IO,2,M)=IO
           PathPointer(Nodee,NTime,IO,3,M)=IM
           PathPointer(Nodee,NTime,IO,4,M)=NArrivalNoPen
9299     Continue
         Update(M,NTime,1)=.True. ! Jan 12 2007
334	continue
       Endif

C--->IF B.1F

Comment B.2: Check the Update Status and Insert the Node in the Deque
	 UpdateCombined=.FALSE.
	Do 2033 ITime=nint(time_now/60),Iti_nu_AMS
	 IDCounter=DequeLabelCounter(CurrentNode,ITime,IM)
	 Do 2032, I3=1,UpCounter(ITime)
	 Do 2032, M=1,Movements+1 ! Jan 12 2007
	    If (Update(M,ITime,I3))  Then
	      UpdateCombined=.TRUE.
	      Update(M,ITime,I3)=.False.
	    EndIf
2032	  Continue
2033    continue

          If (UpdateCombined)  Then 
        If (StatusInDeque(Nodee) .EQ. 0) Then
           If (FirstDeque .NE. INFINITY) Then
              StatusInDeque(LastDeque)=Nodee
              StatusInDeque(Nodee)=INFINITY
              LastDeque=Nodee
           Else
              StatusInDeque(Nodee)=FirstDeque
              FirstDeque=Nodee
              LastDeque=Nodee
           EndIf
        Else
           If (StatusInDeque(Nodee) .EQ. -1) Then
              If (FirstDeque.EQ.INFINITY) LastDeque=Nodee
              StatusInDeque(Nodee)=FirstDeque
              FirstDeque=Nodee
           EndIf
        EndIf

       EndIf

202     Continue
C--<DO B.2

201   Continue
C-<DO B.1
901	Format (I10)
      return
      end






