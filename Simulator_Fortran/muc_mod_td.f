        module muc_mod
! ==================================================================  
! --  $$$  Previous update                                        ==
c --  Dates:   Jan 1999 - July 2002 by                            ==
c --  Authors: Yi-Chang Chiu                                      ==
c --  Tasks:   Re-organize categorization of variables and arrays ==
c --                                                              ==
c --  $$$  Last update                                            ==
c --  Dates:   July 31                                            ==
c --  Authors: Yi-Chang Chiu                                      ==
c --  Tasks:   Add Modified Greenshield data structure            ==
! ==================================================================  

c  --XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c  --  This file includes all the parameters, common blocks and array 
c  --  definitions which are used in the DYNASMART-P system.
c  --XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c  -- This file consits of three parts.  
c  --     Part I : Parameter Setting.
c  --     Part II : Array dimension and definition.  
c  --        II.a: Core DYNASMART (Simulator) Variables, classifiy by functionality
c  --              Overall System
c  --              Network Topology
c  --              Vehicle Static/Run-time Attributes
c  --              Vehicle Path attributes/run-time stats
c  --              K-Shortest Path
c  --              Interfacing between KSP and/or MUC
c  --              Signals
c  --              Demand/Vehicle Loading
c  --              Control/Operational Scenarios
c  --              Run-Time Link or Network Level Stats
c  --              GUI Related
c  --        II.b: Trip Chain Related Variables
c  --        II.c: MUC-DYNASMART Interfacing Variables
c  --     Part III : Global Variable defintions - for those scalar global variable declaration


c  --XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c  -- Part I : Parameter definition and setting.
c  --XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c  -- Part I.a.: Parameter setting
c  --XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c  -- nu_ph1 : Maximum number of phases for an signalized intersection.
c  -- nu_de : Number of simulation intervals over which the link exit flow rate
c  --         will be averaged. (the default is 10 which is 1 minutes)
c  -- nu_mv : Maximum number of movements allowed at any regular node (except centroid).
c  --           processing component.
c  -- nu_types : Maximum number of vehicle types in the network
c  --            Note : vehicle types include bus,truck, pcu, etc. 
c  -- nu_classes : Maximum number of simulated vehicle classes.
c  --              Note : vehicle classes, with respect to the received 
c  --                     information, include SO,UE,BR,...
c  -- nu_control : Mmaximum number of intersection control types.
c  --              Currently, there are 7 types (no control, yield, stop (2way and 4 way)
c  --              pre-timed and actuated(single ring and dual ring)).

!Modification: 10182013
      logical::iterativeSummary=.FALSE.
      ! iterativeSummary=.FALSE. default setting ONLY output simulation results at LAST iteration
      ! iterativeSummary=.TRUE.  allows user to output simulation results at the end of each iteration
      ! SET this value in rhmuc_main.f 
      
      logical:: varyBlockSize = .FALSE.
      ! varyBlockSize=.FALSE. default setting --> original linkProportion subroutine
      ! varyBlockSize=.TRUE.  allow grouping origins during the link proportion calculation --> New subroutine modified by Omer
      ! SET this value in rhmuc_main.f 

!Modification: 10182013      

!  Added by MTI March 3 2004 0.930.9
	integer::isystemrunningmode = 0
! 0: DYNASMART-P
! 1: RT-DYNA in DYNASMART-X
! 2: P-DYNA in DYNASMART-X

! Added May 26 2005
      integer::iSequentialLoad = 0 

! Added Sep 12 2005
	integer::irunno = 0
	integer::ioutput_tdtime_flag = 0
! End

      parameter (nu_ph1=8)
      parameter (nu_mv=11)

      parameter (nu_types=7) 
      parameter (nu_classes=5)
      parameter (nu_control=7)
      parameter (nu_de=10)

c  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c  -- Part II : Array dimension and definition.
c  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

c  -- =======================================
c  -- II.a: Core DYNASMART (Simulator) Variables
c  -- =======================================

c  -- **********Overall System**********

	logical::callavg = .False.
c --  callavg: flags for memory allocation for aggregated info for    

	integer maxden
! Added August 12, 2009 for link-specific maximum density implementation
	integer::i_maxden = 0
	integer,allocatable::maxden_l(:)
	
      integer :: kspstep = 0
c --  kspstep : time interval for calculating the k shortest paths 
c --           (number of simulation intervals). 
	integer :: kupstep = 0
c --  kupstep : time interval for updating the k shortest paths.
c --           (number of simulation intervals). 
      integer soda2
c --  soda2 is an indicator for the vehicle generation. Note soda2 is used mainly in X version.  Not functional in P version
c --  soda2=0  means that the program will generate vehicles according to
c --            the time-dependent OD matrix
c --       =1  the program will read vehicle and path files
c --       =2  read initial condition and generate vehicles according to
c --            the time-dependent OD matrix
c --       =3  read initial condition and vehicle and path files
c --
c --  NOTE : soda2 =2 or 3 are used for the Rolling Horizon procedure.
      integer ftr
c --  ftr: number of simulation intervals per aggregation interval
      integer tad
c --  tad: number of simulation intervals per assignment interval
      integer soint
c --  soint: number of assignment intervals in the planning horizon 
c --  soint = stagelength*10/tad      
      real tii
c --  tii: simulation interval in terms of min, currently = 0.1 min     
	real time_now
c --  time_now : the end of the current simulation interval (in seconds)
      integer nout_tag, nout_nontag
c --  nout_tag: # of taged vehicle exit the network
c --  nout_notag: # of non-taged vehicle exit the network
      integer nout_tag_i,nout_nontag_i
c --  nout_tag_i: # of taged vehicle exit the network at the end of previous sim interval
c --  nout_notag_i: # of non-taged vehicle exit the network at the end of previous sim interval
      integer istrm
c --  istrm : first seed number for the random number generation function. // for vehicle simulation only

! Added by Xuesong and Jing March 28 2004 1.0
      integer istrm2
c --  istrm2 : second seed number for the random number generation function. // for demand generation and path assignment only

! Added by Jason and Jing April 11 2004 1.0
      integer istrm3
c --  istrm3 : third seed number for the random number for UE LOV
      integer istrm4
c --  istrm3 : fourth seed number for the random number for UE HOV
      integer istrm5
c --  istrm3 : fifth seed number for the random number for SO LOV
      integer istrm6
c --  istrm3 : sixth seed number for the random number for SO HOV



c --  *******************Network Topology**********************
      integer kno_nu
c  -- kno_nu : This paramter is used in the path processing component to 
c  --          identify the node-path-movement combination.
c  --          The minimum value for this paramter should be no_nu*nu_mv*nu_ksp.
	real longest_link
c --  longest_link length in the network (mile)
	integer MaxLinkVeh
c  -- MaxLinkVeh : Maximum number of vehicles that may exist on any link at 
c  --         the same time.
      integer :: noofnodes = 0
c --  noofnodes: total # of nodes in the network, including centroids
      integer :: noofnodes_org = 0
c --  noofnodes_org: total # of nodes in the network before adding centroids
      integer :: noofarcs = 0
c --  noofarcs: total # of arcs in the network including connecting links between centroids and destinations (connectors)
	integer :: noofarcs_org = 0
c --  noofarcs_org: total # of arcs in the network before adding connecting links
      integer noofarcsperzone
c --  noofarcsperzone: Max # of arcs in a zone. 
      integer*1::SuperZoneSwitch = 0
c --  SuperZoneSwitch: switch for activating the super zone feature
c --                0: No Super zone
c --                1: with super zone
      real entrymx
c --  entrymx: Max entry rate of entry links
      integer nzones
c  -- nzones: Maximum number of demand zones considered	
	integer noof_master_destinations
c  -- noof_master_destinations: Number of super destination that users specify in superzone.dat

!     Added by Xuesong and Jason July 1 2003 
	integer noof_master_destinations_original
c  -- noof_master_destinations_original: Number of super destination that users specify in superzone.dat
	integer real_SuperzoneIndex
c---  real_SuperzoneIndex: this will be used to get the correspoding destination node ID when implementing memory-less MUC

!     End of change

	real,allocatable::s(:)
c --  s(i): the physical length of the link.
      integer(1),allocatable :: bay(:)
c  -- bay(i) :  >= 1,  link i has  bay(i) left turning bays.
c  --           = 0,  link i does not have a left turning bay.

!added by Hayssam june 18,2003 for 0.930.7 to allow right turn bays
      integer(1),allocatable :: bayR(:)
c  -- bayR(i) :  >= 1,  link i has  bayR(i) right turn bays.
c  --           = 0,  link i does not have a right turn bay.


      integer(1),allocatable::opp_lane(:)
c --  opp_lane(i) : number of lanes for the opposing link to link i.
      INTEGER,allocatable::opp_linkP(:)
c --  opp_linkP(i) : the link ID for the opposing link to link i.
      INTEGER,allocatable::opp_linkS(:)
c --  opp_linkS(i) = 0, if there is no green time allocated to the opposing
c --                  link in the same phase.
c --              = opp_linkP(i), otherwise. 
      INTEGER,allocatable::inlink(:,:)
c --  inlink(i,j): link number of upstream movement j for number of link i,
c --               the total number of these links is stored in the last
c --               cell (nu_mv+1) of the array. The movement is determined by backpointr
      INTEGER,allocatable::llink(:,:)
c --  llink(i,nu_mv+1): downstream link number of link i,
c --                    the number of these links is stored in the last
c --                    cell (nu_mv+1) of the array.

      INTEGER,allocatable::topocont(:)

      integer(1),allocatable::movein(:,:)
c --  movein(i,j): store the movement type (left, right, straight, etc.) for link i from upstream
c --               movement j (determined by inlink), the number of these movements is stored in the last
c --               cell of the array.  
      integer(1),allocatable::move(:,:)
c --  move(i,j): an array to store the movements from link i to downstream
c --             link j the number of these movements is stored in the last
c --             cell of the array.

! Added by Hayssam and Xuesong Aug 5 0.930.7
      integer(1),allocatable::UturnFlag(:)
c --  UturnFlag(i): an array to store the U turn prevented flag
c --             0: prevented
c --             1: allowed

	real,allocatable :: left_capacity(:)
c --  left_capacity(i) : left turn capacity, maximum number of vehicles that
c --                     can pass from the left turn lane of link i during
c --                     every simulation interval.
      integer leftcapWb(5,3,7)
c --  leftcapWb(i,j,l_o) : is the left turn capacity for g/c ratio i, number
c --                     of oppoising lanes j, and level of volume on the opposing
c --                     link equal l_o and there is a left turn bay.
      integer leftcapWOb(5,3,7,7)
c --  leftcapWOb(i,j,k,l_o) : is the left turn capacity for g/c ratio i, number of
c --                        opposing lanes j, level of volume on the opposing link
c --                        equal l_o, and level of volume on current link equal k
c --                        and there is no left turn bay.

!added by Hayssam on June 18,2003 for 0.930.7
************************ Start of Addition ***********************************
	real,allocatable :: right_capacity(:)
c --  right_capacity(i) : right turn capacity, maximum number of vehicles that
c --                     can make a right turn from link i during
c --                     every simulation interval.
************************ End of Addition ***********************************




	real,allocatable::MaxFlowRateOrig(:)
c --  original max flow rate read from input files.
c --  not to be updated by heavy vehicle factor


	!added by Hayssam on April 22 2004 for DYNA 1.0
	! to keep original flow rates intact for incidents
	real,allocatable::Input_Flow_Rate_Orig(:)


	real,allocatable::MaxFlowRate(:)
c --  MaxFlowRate(i) : Maximum service flow for link i in vehicles per second.
c --           note: this variable is read in the input file as the max service
c --           flow per lane and then updated to be the total max service flow per
c --           link by multiplying by the number of lanes on the link.
c --           == sat() in the old definition
c --           this is mainly used as the capacity for moving vehciles
c --           this is updated by heavy vehicle factor

	real,allocatable::SatFlowRate(:)
c --  SatFlowRate(i) : Saturation flow for link i in vehicles per second.
c --           note: this variable is read in the input file as the saturation
c --           flow per lane and then updated to be the total saturation flow per
c --           link by multiplying by the number of lanes on the link.
c --           this is mainly used to discharge queue on the arterials and incidents on
c --           both arterials and freeways

      integer(1),allocatable:: LGrade(:)
c --  LGrade(i): grade of link i

	INTEGER,allocatable::link_iden(:)
c --  link_iden(i): identification index for link i
c --                =1, freeway link
c --                =2, freeway link with detector
c --                =3, on ramp
c --                =4, off ramp
c --                =5, arterial link
c --                =6, High Occupancy Vehicle (HOV) link
      real,allocatable::LoadWeight(:)
c --  LoadWeight(i): loading weight for link i.  Default value is 1.00
      Logical,allocatable::LoadWeightID(:)
c --  LoadWeightID(i): Index to specify if the user wants to specify loading weights for zone i, default is false
	INTEGER,allocatable::iunod(:)
c --  iunod(i): upstream node of link i.
	INTEGER,allocatable::idnod(:)
c --  idnod(i): downstram node of link i.
	integer(1),allocatable::nlanes(:)
c --  nlanes(i) : number of lanes on link i.
	real,allocatable::xl(:)
c --  xl(i): the lane length of the link = # of lanes x link length
c --         it changes if there is incident on the link.

! Added by MTI team Feb 04 2004 0.930.9
	real,allocatable::original_xl(:)
c --  original_xl(i): the original_lane length of the link = # of lanes x link length
c --         it changes if there is incident on the link.
      integer,allocatable:: LGenerationFlag(:)
c --  LGenerationFlag(i): 0: Non-generation link, otherwise: corresponding zone number



	real,allocatable::cmax(:)
c --  cmax(i): the maximum density of link i

      integer::NoOfFlowModel=0
c --  NoOfFlowModel: # of traffic models
      Type MGS 
	  INTEGER Kcut
        INTEGER Vf2
        INTEGER V02
        INTEGER Kjam2
        real alpha2
   	end type
! data structure for Modified GreenShield model, reason for dimension 2 is for two regime
      type(MGS),allocatable:: MGreenS(:)
c --  MGreenS(:): Modified Greenshield models that are to be defined
      integer(1),allocatable::FlowModelNum(:)
c --  FlowModelNum(i): Traffic flow model number for link i specifed in TrafficFlowModel.dat
      integer(1),allocatable::FlowModelType(:)
c --  Type of flow model, only two types are currenlty defined in DYNASMART
c --  Type 1: HCM freeway type with two regimes, first one is flat at free-flow speed
c --       2: with out flat part
	INTEGER,allocatable::Vfadjust(:)
c --  Vfadjust(i): difference between the observed free-flow speed from post speed-limit signs, could be + or - certain mile from the speed limit
	real,allocatable::p(:)
      INTEGER,allocatable::SpeedLimit(:)
c --  post speed limit
c --  p(i): a parameter used in the speed-density relationship
	INTEGER,allocatable::destination(:)
c --  destination(i): internal node number for the centroid (superzone) i

! Added by MTI team Jan 17 2004 0.930.7D
	INTEGER,allocatable::origin(:)
c --  origin(i): internal node number for the centroid (origin zone) i

	INTEGER,allocatable::MasterDest(:) 
c --  MasterDest(i): super zone that the original zone i belongs to
    	integer,allocatable::nodenum(:)
c --  nodenum: internal node number -> input node number
	integer, allocatable::idnum(:)  !G
c --  idnum: input node number -> internal node number
c    	integer(1),allocatable::iflag_gen(:)
c --  iflag_gen: 0: the node is not the destination
c --             1: the node is the destination
	INTEGER,allocatable::izone(:)
c --  izone(i): original (without aggregation) zone that node i belongs to
	INTEGER,allocatable::iConZone(:,:)
c --  iConZone(i,1): number of original zones that connector i connects to
c --  iConZone(i,2-3): original zone number that connector i connects to (only allow max 2 mapping for each node)
	INTEGER,allocatable::iGenZone(:,:)
c --  iGenZone(i,1): number of original zones that link i receives demand from
c --  iGenZone(i,2-3): original zone number that link i receives demand from (only allow max 2 mapping for each link)
	INTEGER,allocatable::BackToForLink(:)
c --  BackToForLink(k)=j : backward * link k -> forward* link j, originally called ifwdarc()
      INTEGER,allocatable::BackPointr(:)
c --  BackPointr(i)=k: node i -> the backward* link number for those with downstrem node i
	INTEGER,allocatable::ForToBackLink(:)
c --  ForToBackLink(j)=k : forward * link j -> back* link k, originally called backindex

c --  ====================More about "ForToBackLink(:)" and "BackToForLink(:)"=================================================
c --  The link specification in "network.dat" is "forward* represatation". 
c--   (e.g., [linkID:upstream node->downstream node] link1:1->2, link2:1->3, link3:1->4, link4:2->3, link5:3->2, and so on (based on the ascending order of the UPstream nodes))
c --  If this is converted into "backward* represatation", the order of links is changed.
c--   (e.g., [linkID:upstream node->downstream node] link1:1->2, link5:3->2, link2:1->3, link4:2->3, link3:1->4, and so on (based on the ascending order of the DOWNstream nodes))
c--   Basically ForToBackLink provides the converted link order in Backward* representation using the original link order in Forward* Representation shown in network.dat.
c--   BackToForLink provides the original link order in Forward* Representatio from the converted link order in Backward* representation.
c--   (e.g. using the above example)
c--   ForToBackLink(j)=k : j(1-2-3-4-5) ->> k(1-5-2-4-3)
c--   BackToForLink(k)=j : k(1-5-2-4-3) ->> j(1-2-3-4-5)
c--   =========================================================================================================================added by JK


	INTEGER,allocatable::UNodeOfBackLink(:)
c  -- UNodeOfBackLink(k)=iunod(j): upstream node of backward* k obtained from corresponding for* link j
	integer(1),allocatable::connectivity(:,:)
! Modified by MTI team Jan 27 2004 0.930.9
c --  connectivity(i,j): check the connectivity between any link i to super destination j
c --                0: there is no path exits between link i and j
c --                1: there is at least one path between link i and j


!c --  connectivity(i,j): check the connectivity between any node i to centroid j
!c --                0: there is no path exits between i and j
!c --                1: there is at least one path between i and j
      integer, allocatable::GradeBPnt(:)
c --  GradeBPnt(i): Breakpoint for grade category i
	real, allocatable::LengthBPnt(:,:)
c --  LengthBPnt(i): breakpoint for link length category i
	integer, allocatable::TruckBPnt(:)
c --  TruckBPnt(i): breakpint for truck percentage category i
	real, allocatable::PCE(:,:,:)      
c --  PCE(i,j,k): PCE value for grade category i, length category j, and truck % category k
      integer GradeNum, LenNum, TruckNum
c --  Number of dimensions of PCE table (HCM 98 Table 3-3)
	real, allocatable::DynPCE(:)
c --  Dynamic PCE for heavy vehicles, updated every simulation interval based on grade and link length
	INTEGER, allocatable::GRDInd(:)
c --  GRDInD(i): pointer for Grade category
	INTEGER, allocatable::LENInd(:)
c --  Pointer for Length category

	INTEGER, allocatable::OriginLinkIndex(:)
c --  OriginLinkIndex


c --  +++++ HOV/HOT +++++
      logical, allocatable:: HOVFlag(:)
c --  HOVFlag(j): flag indicating if the vehicle j traverses the HOV lane
      logical, allocatable:: HOTFlag(:)
c --  HOTFlag(j): flag indicating if the vehicle j traverses the HOT lane

      real price_regular,price_hot_lov, price_hot_hov  !V
c --  price_regular: additional cost for regular links
c --  price_hot_lov: additional cost for lov vehicles in HOT lanes
c --  price_hot_hov: additional cost for hov vehicles in HOT lanes
      real price_regular_c,price_hot_lov_c, price_hot_hov_c,time_value  !V
c --  all these_c are temporary variables from input, however need them for printout

! added by Hayssam and Jason August 18, 2005
! for freeway bias implementation
      real fwy_bias

      integer :: no_link_type =1
c --  HOV application: 
c --  no_link_type: 1: no HOV lane modeled in the network
c --                2: with HOV lane modeled in the network
	integer :: no_occupancy_level = 1
c --  HOV application: 
c --  no_occupancy_level: 1: no HOV vehicles modeled in the network
c --                2: with HOV vehicles modeled in the network
      real time_lov_hot,time_hov_hot	!V
c --  time_lov_hot: total travel time for lov vehicles in HOT lanes
c --  time_hov_hot: total travel time for hov vehicles in HOT lanes

! Added by MTI team Feb 04 2004 0.930.9
      real time_lov,time_hov	!V
c --  time_lov: total travel time for lov vehicles
c --  time_hov: total travel time for hov vehicles



      real time_lov_ohot,time_hov_ohot !V
c --  total travel time for lov vehicles not in HOT
c --  total travel time for hov vehicles not in HOT
      integer::iactual_lov_hot = 0	!V
c --  number of lov vehicles in HOT lanes
      integer::iactual_lov_ohot = 0	!V
c --  number of lov vehicles not in HOT lanes
      integer::iactual_hov_hot = 0	!V
c --  number of hov vehicles in HOT lanes
      integer::iactual_hov_ohot = 0	!V
c --  number of hov vehicles not in HOT lanes


! Added by MTI team Feb 04 2004 0.930.9
      integer::iactual_lov = 0	!V
c --  number of lov vehicles
      integer::iactual_hov = 0	!V
c --  number of hov vehicles


      integer Link_hot
c --  # of HOT links in the network.
      integer Link_hov
c --  # of HOV links in the network.

!Added May 23 2005 TD link toll
c --  *********************************************************
      integer::iAdvance_pricing
      integer num_of_tolllinks  ! num of links with toll

      Type Toll
        real    ST            ! starting time (minutes)
	  real    ET            ! ending time   (minutes)
	  integer tollType      ! link toll type: regular toll(0), HOV(1), HOT(2),...
        real    tollcost      ! toll price (USD)
	end type
		
	Type LinkToll
        integer    FNode	     ! from node
	  integer    TNode       ! to node        
        integer    num_tolls_per_link      
	  type(toll) Tolls(24)  
	end Type

	type(LinkToll),allocatable::LinkTolls(:)
c --  *********************************************************
! End
!Added Feb 10 2009 Weather Impact
c --  *********************************************************
      integer::i_weather
      integer num_of_weatherlinks  ! num of links impacted by inclement weather
!Weather June 1 2011
!      real network_st
!      real network_et
!      real network_visibility
!      real network_rain
!      real network_snow

      integer:: network_weather
      ! 0: NO network wide effect
      ! 1: ONE time interval for network wide effect. SAME definition as the old weather.dat
      ! 2+: number of time interval for newtwork wide effect. used for NEW weather.dat
      
    ! the following VARs are kept for old weather.dat format  
      real network_st_scalar
      real network_et_scalar
      real network_visibility_scalar
      real network_rain_scalar
      real network_snow_scalar
    ! end old VARs
    
    ! the following Arrays are for time-dependent network weather effect <-- NEW weather.dat format 
    !  integer:: num_network_weather_intervals
      real,allocatable:: network_st(:)
      real,allocatable:: network_et(:)
      real,allocatable:: network_visibility(:)
      real,allocatable:: network_rain(:)
      real,allocatable:: network_snow(:)
    ! end NEW    
!end Weather June 1 2011
      
      Type Weather
        real    ST            ! starting time (minutes)
	  real    ET            ! ending time   (minutes)
	  real    visibility    ! unit: mile
        real    rain          ! unit: inch per hour
        real    snow          ! unit: inch per hour
	end type
		
	Type LinkWeather
        integer    FNode	   ! from node
	  integer    TNode      ! to node        
        integer    num_weather_per_link      
	  type(Weather) Weathers(24)  
	end Type

	type(LinkWeather),allocatable::LinkWeathers(:)
	real,allocatable::WAF(:,:)!weather adjustment factor from WAF.dat(18x6)
	real,allocatable::linkWAF(:,:)!WAFs for each link (noofarcs x 18)
      !Modified by Alex on 040416 for snow accumulation
      !SaFactorNum: Number of scenarios for snow accumulation. Different scenarios are determined by the depth of snow accumulation and have different speed/link capacity reduction factor
      !PlowEffect: 1 if the snow stop accumulate after the plow, otherwise the snow will continue to accumulate
      !SaFactor(i,j):SaFctor(i,1): The max snow accumulation for scenario i. SaFctor(i,2): The speed reduction factor for scenario i.SaFctor(i,2): The capacity reduction factor for scenario i.
      !AccuStime(i): The difference between current time and the last time the link is free from snow accumulation.
      !SnowDepth(i): The snow depth on link i. It is equal
      !sp_routes(i,:) the routes that the ith snowplow will take
      !LinkTobeServed(i,j): Link will be served at time interval i
      !LinkServedTime(i,j): The service time of ith link's j's lane
      integer SaFactorNum,lexist_SnowAccu,PlowEffect
      integer noofsnowplows,longestroute
	integer noofsnowplowinterval,nooflsininterval 
      real,allocatable::SAFactor(:,:),AccuStime(:),SnowDepth(:,:)
      real,allocatable::speedreduction(:),capacityreduction(:)
      real,allocatable::sp_routes(:,:)
      integer,allocatable::LinkTobeServed(:,:)
      real,allocatable:: LinkServedTime(:,:)
      !End of snow accumulation
c --  *********************************************************
! End of Feb 10 2009

c --  ********** Vehicle Static/Run-time Attributes**********
      integer iread_veh_flag
c --  flag ind
	integer iread_veh_count
c --  iread_veh_count: counter indicating the number of vehicles are loaded from vehicle and path files
      integer ktotal_out
c --  total # of vehicles exit the network (will be removed later)
	integer :: nu_ve = 0
c --  # of vehicles to be generated
      integer jtotal
c --  jtotal: used in vehicle file loading mode, it is the total # of vehicles in
c --          vehicle.dat and path.dat to be loaded
      real com_frac, fracinf,ribfa,bound
c --  parameters from BR-class vehicles
c --  com_frac: % of compliant vehicles
c --  fracinf: % of vehicles with information
c --  ribfa : relative indifference band (percent improvement at whcih 
c --          a user will switch his/her path).
c --  bound : threshold bound for path switching (user will not switch path
c --          unless the time savings are greater than the bound)
      real starttm
c --  warmu-up time for statistics collections
      real endtm
c --  endtm: end time in which the veh stat are collected. 
	integer numcars
c --  # of vehicles exist in the network as of now

! Added by MTI team Jan 28 2004 0.930.9
!      integer jj, jj_i
      integer jj, jj_i, jj_MUC
c --  jj: number of vehicles generated as of now
c --  jj_i: number of vehicles generated at the end of the last sim interval.
c --  jj_MUC: number of vehicles generated in this demand interval 
      integer,allocatable::vehclass(:)
c -- vehclass(j) : the calss for vehicle j (SO,UE,etc)
c --            =1, prespecified path (pre-trip information)
c --            =2, SO path information
c --            =3, UE path information
c --            =4, boundadly rational vehicles
	real classpro(nu_classes)
c --  classpro(i) : cumulative proporition for vehicle classes 1 through i.
      integer,allocatable::vehclass2(:)
c --  vehclass2(j) : the type for vehicle j.
c --             =1, non-equipped pcu
c --             =2, non-equipped trucks 
c --             =3, non-equipped HOV 
c --             =4, equipped pcu
c --             =5, equipped trucks 
c --             =6, equipped HOV 


	!****************************************************************
	!Added by hayssam and Xuesong to model different MUC distributions 
	!for different vehicle types
	! DYNA 930.9 Jan22, 2004
	integer No_Veh_Types
	integer::Max_No_Veh = 3
	real :: MUC_Frac(3,5)
	real :: Dem_Frac(3)
	integer :: Veh_Type(3) 
	integer:: Dem_Mode(3)
	integer:: MUC_Mode(3)
	integer :: Numof_Veh_Type(7) ! Numof_Veh_Type(i)  number of vehicles for vehicle type i
	! 1: PC, 2: Truck, 3: HOV, 7: Bus
	integer :: Numof_Veh_Class(6)  ! Numof_Veh_Class(i)  number of vehicles for vehicle class i
	!***********************************






      real classpro2(nu_types)
c --  classpro2(i) : cumulative proporition for vehicle types 1 through i.
      real vehicle_length
c --  vehicle_length: average vehicle length, taken to be 20 feet.
	real,allocatable::ttstop(:)
c --  ttstop(j): the stop time till now for vehicle j
c      real, allocatable:: pathstop(:,:)
c --  pathstop(i,j): the stop time of vehicle i till node j.
	real,allocatable::ribf(:)
c --  ribf(j): relative indifference band of vehicle j, it determines the
c --          threshold at which the vehicle can switch its path
c --          to a better path.
	integer,allocatable::icurrnt(:)
c --  icurrnt(j): the order of current link of vehicle j in its path.
	integer(1),allocatable::notin(:)
c --  notin(j): an indix to indicate whether or not the vehicle
c --            is in the network.
c --    =1, the vehicle is out of the network
c --    =0, otherwise.
	real,allocatable::xpar(:)
c --  xpar(i) : the distance between the vehicle and the stop line of
c --            its current link.
	real,allocatable::tleft(:)
c --  tleft(j): the time remaining from the current simulation interval
c --             when vehicle j reaches the end of its current link.
	real,allocatable::tqwait(:)
c --  tqwait(i): the waiting time at the queue for vehicle j at the current node.
	real,allocatable::compliance(:)
c --  compliance(i): a variable for vehicle j by which its compliance index
c --                 is calculated.
	real,allocatable::mtnum(:)
c --  mtnum(j): passenger car equivalent for vehicle j
c --         =1 for cars
c --         =2 for buses
	INTEGER,allocatable::isec(:)
c --  isec(j): the generation link of vehicle j.
	real,allocatable::stime(:)
c --  stime(j): the start time of vehicle j.
	real,allocatable::atime(:)
c --  atime(j): the arrival time of vehicle j.
	integer(1),allocatable::info(:)
c --  info(j): whether vehicle j has capability to receive en-route real-time information or not
c           0: no information
c           1: with information	
	integer(1),allocatable::itag(:)
c --  itag(j): tag for vehicle j
c           0: vehicle j is not generated in the period of interest as defined by starttm and endtm in scenario.dat
c           1: vehicle j is generated within the period of interest and is still in the network
c           2: vehicle j is generated within the period of interest and has left the network
	INTEGER,allocatable::nexlink(:)
c --  nexlink(j): the next link of the current link that vehicle j is on
	real,allocatable::tocross(:)
c --  tocrossj): time (min) needed for vehicle j to reach the stop line in this simulation interval
	integer(1),allocatable::lt(:)
c -- determine if the vehicle will use the (high occupancy vehicle/toll lane) HOT lane.
c -- lt(j)=1, the vehicle uses the regular network only.
c -- lt(j)=2, the vehicle uses the HOT lane as part of its path.
	integer(1),allocatable::ioc(:)
c -- assign level of occupation for the vehicle.
c -- ioc(j)= 1, lower occupancy vehicle
c -- ioc(j)= 2, higher occupancy vehicle   
	integer(1),allocatable::iuserpath(:)
c --  iuserpath(j): under the HOV scenario, which path out of k path the vehicle j is taking
	integer jrestore
c --  counter for the number of vehicles currently loaded in the network



c  -- ********** Vehicle Path	attributes/run-time stats******
      INTEGER,allocatable:: nnpath(:)
c --  nnpath(j): number of nodes on vehicle j's path
      integer :: nu_switch = 0
c --  nu_switch : Maximum number of path switches for each vehicle
      INTEGER,allocatable:: pathindex(:)
c --  pathindex(i) : the number of nodes in the path of vehicle i.
	real,allocatable::ttilnow(:)
c --  ttilnow(i): the travel time till now for vehicle i including its stop
c --              time.
	INTEGER,allocatable::jdest(:)
c --  jdest(j): the destination zone (super zone number) for vehicle j.

! Added by MTI team Jan 17 2004 0.930.7D
	INTEGER,allocatable::jorigin(:)
c --  jorigin(j): the origin zone (not super zone number) for vehicle j.

! Added by MTI team Jan 17 2004 0.930.7D
	INTEGER,allocatable::jipick(:)
c --  jipick(j): the MUC path index for vehicle j.


c  -- ********** K-Shortest Path**********
      integer::MaxMove = 0
      integer::MaxMove_current = 0
c --  MaxMove: the max # of movement at centrold.  This is determined by 
c --  check the user-specified # of destinations
c --  This is used to allocate memory for KSP arrays
      real infinity,nil
c --  constant 
	integer kpaths	
c --  counter used run-time in KSP indicating which path is currently solved
	integer ltype, ioccup
c --  counter for HOV/HOT in KSP calculation
	integer ides
c --  destination that is currently been computed by KSP
      integer Ndummy, Nd2,Nd3
c --  ###  Not Defined Yet ###
      integer Kcurrent
c --  ###  Not Defined Yet ###
      integer,allocatable::UpCounter(:)
c --  ###  Not Defined Yet ###
      integer interval_avg_for_tdksp
c --  ###  Not Defined Yet ###
      real timeinterval
c --  timeinterval=interval_avg_for_tdksp*tii
      allocatable::labelforods(:,:,:,:,:)
c --  ###  Not Defined Yet ###
      real,allocatable::LabelOut(:,:,:,:,:,:,:)
c --  LabelOut(no_link_type,no_occupancy_level,noof_master_destinations,noofnodes,ksptime,kay,nu_mv):
c --  Store the time label for the optimal k-paths from each
c --  node-movement combination to each destination at each
c --  departure interval. These paths are considered for
c --  different pricing schemes to model HOV/LOV.
	real,allocatable::LabelOutCost(:,:,:,:,:,:,:)
c --  LabelOutCost(no_link_type,no_occupancy_level,noof_master_destinations,noofnodes,ksptime,kay,nu_mv):
c --  Stores the cost label for the optimal k-paths from
c --  each node-movement combination to each destination at
c --  each departure interval. These paths are considered
c --  for different pricing schemes to model HOV/LOV.
      INTEGER,allocatable::PathPointerOut1(:,:,:,:,:,:,:)
c --  PathPointerOut1(no_link_type,no_occupancy_level,noof_master_destinations,noofnodes,ksptime,kay,nu_mv)
c --  : Stores the optimal k-paths from each node-movement
c --  combination to each destination at each departure
c --  interval. These paths are considered for different
c --  pricing schemes to model HOV/LOV. Points to the node
c --  associated with the predecessor optimal label. 
	integer(1),allocatable::PathPointerOut2(:,:,:,:,:,:,:)
c --  PathPointerOut2(no_link_type,no_occupancy_level,noof_master_destinations,noofnodes,ksptime,kay,nu_mv)
c --  : Stores the optimal k-paths from each node-movement
c --  combination to each destination at each departure
c --  interval. These paths are considered for different
c --  pricing schemes to model HOV/LOV. Points to the "k"
c --  associated with the predecessor optimal label. 
	integer(1),allocatable::PathPointerOut3(:,:,:,:,:,:,:)
c --  PathPointerOut3(no_link_type,no_occupancy_level,noof_master_destinations,noofnodes,ksptime,kay,nu_mv)
c --  : Stores the optimal k-paths from each node-movement
c --  combination to each destination at each departure
c --  interval.  These paths are considered for different
c --  pricing schemes to model HOV/LOV.Points to the
c --  movement index associated with the predecessor optimal label. 
	INTEGER,allocatable::PathPointerOut4(:,:,:,:,:,:,:)
c --  PathPointerOut4(no_link_type,no_occupancy_level,noof_master_destinations,noofnodes,ksptime,kay,nu_mv)
c --  : Stores the optimal k-paths from each node-movement
c --  combination to each destination at each departure
c --  interval. These paths are considered for different
c --  pricing schemes to model HOV/LOV. Points to the time
c --  index associated with the predecessor optimal label. 
      integer(1),allocatable::LabelPointerOut(:,:,:,:,:,:,:)
c --  LabelPointerOut(no_link_type,no_occupancy_level,noof_master_destinations,noofnodes,ksptime,kay,nu_mv)
c --  : Stores the order of the k-paths associated with each
c --  node-movement combination to each destination at each
c --  departure interval. These paths are considered for
c --  different pricing schemes to model HOV/LOV. 
      integer(4),allocatable::totalpriority(:,:,:)
c --  totalpriority(no_link_type,no_occupancy_level,noofnodes):
c --  A temporary variable to store the total number of
c --  predecessor retrieving calculations from all nodes to
c --  all destinations.
 	integer(4),allocatable::priority(:,:,:,:,:,:,:)
c --  priority(no_link_type,no_occupancy_level,noof_master_destinations,noofnodes,ksptime,kay,nu_mv):
c --  A temporary array to store the optimal k-paths from
c --  all nodes to all destinations. These paths are
c --  considered for different pricing schemes to model HOV/LOV. 
!	integer(4),allocatable::pp(:,:,:,:,:)
	INTEGER,allocatable::pp(:,:,:,:,:)
! Changed by Xuesong and Jason June 12 2003
! Reason: pp copies all the information from PathPointerOut1, up to PathPointerOut4,
! which only use INTEGER

c --  pp(no_link_type,no_occupancy_level,noof_master_destinations,kno_nu,4): 
c --  A temporary array to store the optimal k-paths from
c --  all nodes to all destinations. These paths are
c --  considered for different pricing schemes to model HOV/LOV. 
	integer totalprior
c --  A temporary variable to store the total number of
c --  predecessor retrieving calculations from all nodes to
c --  all destinations
      integer(4),allocatable::track(:,:)
c --  track(kno_nu,4): A temporary array to store the
c --  optimal k-paths from all nodes to a single destination. 
	real,allocatable::TTimeOfBackLink(:)
c --  TTimeOfBackLink(noofarcs) : For each node, it stores the
c --  travel time on all incoming arcs, considering backward
c --  network representation.  Originally called backstr2
      real,allocatable:: Label(:,:,:,:)
c --  Label(noofnodes,ksptime,kay,nu_mv): Store the time
c --  label for the optimal k-paths from each node-movement
c --  combination to one destination at each departure
c --  interval. These paths are considered for different
c --  pricing schemes to model HOV/LOV.
	real,allocatable:: LabelCost(:,:,:,:)
c --  LabelCost(noofnodes,ksptime,kay,nu_mv): Stores the
c --  cost label for the optimal k-paths from each
c --  node-movement combination to each destination at each
c --  departure interval. These paths are considered for
c --  different pricing schemes to model HOV/LOV.
	integer(4),allocatable:: FirstLabel(:,:,:)
c --  FirstLabel(noofnodes,ksptime,nu_mv) : Points to the
c --  position of the best path in the k-path vector.
      integer(4),allocatable:: LabelPointer(:,:,:,:)   
c --  LabelPointer(noofnodes,ksptime,kay,nu_mv) : Stores the
c --  order of the k-paths associated with each
c --  node-movement combination to one destination at each
c --  departure interval. These paths are considered for
c --  different pricing schemes to model HOV/LOV. 
	integer(4),allocatable::FirstGoodLabel(:,:,:)
      INTEGER,allocatable::PathPointer(:,:,:,:,:)
c --  PathPointer(noofnodes,ksptime,kay,4,nu_mv) : Stores
c --  the optimal k-paths from each node-movement
c --  combination to one destination at each departure
c --  interval. It points to the node, k, movement, and time
c --  indexes associated with the predecessor optimal label.
c --  These paths are considered for different pricing schemes to model HOV/LOV. 
	real,allocatable::DequeLabel1(:,:,:,:)
c --  DequeLabel1(noofnodes,ksptime,kay,nu_mv)  : A
c --  temporary array to store the value of time label for
c --  each node-movement-path-time combination.
	real,allocatable::DequeLabel1Cost(:,:,:,:)
c --  DequeLabel1Cost(noofnodes,ksptime,kay,nu_mv) : A
c --  temporary array to store the value of cost label for
c --  each node-movement-path-time combination.
	integer(4),allocatable::DequeLabel2(:,:,:,:)
c --  DequeLabel2(noofnodes,ksptime,kay,nu_mv) : A temporary
c --  array to store the k indexes for each node-movement-path-time combination.
	integer,allocatable::StatusInDeque(:)
c --  StatusInDeque(noofnodes) : indicates the status of the
c --  nodes in terms of the scanned queue. It indicates if the nodes is 
c --  previously scanned or not
      integer FirstDeque
c --  Points to first node in the deque.
      integer LastDeque
c --  ###  Not Defined Yet ###
      integer(4), allocatable::DequeLabelCounter(:,:,:)
c --  DequeLabelCounter(noofnodes,ksptime,nu_mv) : A
c --  temporary array to store the number of paths computed
c --  at each node-time-movement combination.
      logical,allocatable::Update(:,:,:)
c --  Update(nu_mv,ksptime,kay+1): An array to indicate if
c --  the label at the node is updated or not (for each
c --  move-k comination). Then, the variable used to
c --  determine the status of the node in the deque.
      integer ArrIndex
c --  The arrival time at a node converted into integer
c --  number. This varibale is used to determiine the time
c --  index for the time-dependent link and node cost.
      Real NextDistance
c --  A temporary variable to store the travel time on the
c --  arc connecting the current scanned node and the
c --  upstream node
	real NewLabel
c --  A temporary variable to store the time label.
	real NewLabelCost
c --  A temporary variable to store the cost label.
      real NextPenalty
c --  A temporary variable to store the movement cost (in
c --  terms of travel time) at the current scanned node.
      Real MaxLabel
c --  A temporary variable to store the maximum time label in the k-vector
	real MaxLabelCost
c --  A temporary variable to store the maximum cost label in the k-vector.
	real NextCost
c --  A temporary variable to store the travel cost on the
c --  arc connecting the current scanned node and the upstream node.
      integer CurrentNode
c --  Temporary varibale represent the currently scanned node.
      integer Arc
c --  Indes for the network links.
      integer BackPointrCurrent
c --  A temporary variable to store number of inbound arcs to given node.
      integer SecondLabel
c --  A temporary variable to store the k index for the 
c --  second worst label in the k-vector. 
      integer EmptyLabel
c --  A temporary variable to store the path index.
      Logical UpdateCombined
c --  A temporary variable to indicate if the label at the
c --  node is updated or not. Then, the variable used to
c --  determine the status of the node in the deque. This
c --  variable is exactly the same ias Update but in non array format.
      logical Found
c --  A temporary variable to indicate if the label at the node is updated or not. 
      integer destin
c --  The destination node to which shortest paths are determined.
      logical finish


c  -- ********** Interfacing between KSP and/or MUC**********
      integer kay
c  -- number of shortest paths to be generated
      real,allocatable :: penalty(:,:)
c  -- penalty(i,m): Estimated delay on the link i turning movement m, backward *
c  --               structure.
      real,allocatable :: openalty(:,:) 
c  -- openalty(i,m): Estimated delay on the link i turning movement m, forward *
c  --               structure.
      real,allocatable :: link_entry_time(:)
c  -- link_entry_time(i) : Estimated delay on the entry link for generation
	real,allocatable :: entry_service(:,:)
c --  entry_service(i,t) : number of vehicles entering the network through 
c --                the link, from the dummy entry link, during averaging
c --                interval t.
      real,allocatable:: entryrate(:)
c --  entryrate(i) : average number of vehicles entering the network through 
c --                link i from its dummy entry link.
      integer(1),allocatable:: inentry(:) 
c  -- inentry(i) : number of simulation intervals over which the entryrate
c  --             has been accumulated for link i.
      INTEGER,allocatable:: link_entry_queue(:)
c -- link_entry_queue(i) : number of vehicles waiting on the dummy entry link
c --                       of generation link i.
	integer:: indelay = 0
c --  indelay(i): counter for the simulation intervals (averaging intervals)for link i
c --  indelay is kept as an array for future modifications in the code.
c --  i.e. the user might be allowed to input different averaging intervals for
c --  different links.
	real,allocatable::delaystep(:,:)
c --  delaystep(i,k): total number of vehicles leaving the intersection from link
c --                  i during simulation interval k.
! Added Aug 21 2007 
! to calculate the actual flow on major road for 2-way stop sign and yield sign
	real,allocatable::outflow2(:,:)
c --  outflow2(i,k): actual total number of vehicles leaving the intersection from link
c --                  i during simulation interval k.
! End of Aug 21 2007
	real,allocatable::delayleft(:,:)
c --  delayleft(i,k): total number of vehicles leaving the intersection through a
c --                  left turn movement on link i during simulation interval k.
	real,allocatable::aveoutflow(:)
c --  aveoutflow(i): the averaging of delaystep(i) over nu_de simulation intervals.
      real,allocatable::aveoutleft(:)
c --  aveoutflow(i): the averaging of delayleft(i) over nu_de simulation intervals.
      integer,allocatable::AccuVol(:)
c --  AccuVol(i): the accumulated volume over nu_de simulation intervals.
      integer,allocatable::LinkVolume(:)
c --  Volume(i): the volume for link i at each minute.
	real,allocatable::astmpt(:)
c --  astmpt(i): temporary arry for averaging the link travel time. Used in MUC
	real,allocatable::apen(:,:)
c --  apen(i,j): temporary array for averaging the link penalty for link i movement j. Used in MUC
	real,allocatable::alet(:)
c --  alet(i): temporary array for averaging the link entry time
	real,allocatable::diff(:,:)
c --  diff(i,1): temporary array for averaging the moveturnMG for left turn
c --  diff(i,2): temporary array for averaging the moveturnMG for others turn
      real,allocatable::apenal(:,:)
c --  apenal(i,j) temporary array for averaging the opeantly.
      real,allocatable::lint(:)
c --  lint(i): temporary array for averaging linfree
	real,allocatable::lfr(:)
c --  lfr(i): temporary array for averaging intoo


	
c --  ********** Signals**********
      Integer SignCount
	Type Sign
       integer node
	 integer NofMajor
       integer NofMinor
	end type
	type (Sign), allocatable::SignData(:)
	     
	Type SignApproach
       integer major(4)
       integer minor(4)
      end type
      type (SignApproach), allocatable::SignApprh(:)

	integer NLevel 
c --  Number of vehicle levels for stop signs
      integer NMove
c --  Number of movement of interest in stop
      integer Level2N, Move2N

	!added by hayssam april 24 2004 for DYNA 1.0
	!  Number of movement of interest in yield signs
      integer YLevel2N, YMove2N


      integer, allocatable::stopcap4w(:,:)
      integer, allocatable::stopcap2w(:,:)
      integer, allocatable::stopcap2wIND(:)
      integer, allocatable::YieldCap(:,:)
      integer, allocatable::YieldCapIND(:)

      integer isig
c --  isig: # of signal timing plans in the control.dat
	integer isigcount
c --  counter indicating which signl timing plan the current time is at
      INTEGER nodetmp(nu_control)
c -- nodetmp(i) : number of intersection controlled by each control type i.
      real,allocatable::gcratio(:)
c --  gcratio(i) : the ratio of green time, for link i, to the cycle 
c --              length at its downstream node.(g/c) 
      INTEGER,allocatable::kgpoint(:)
c -- kgpoint(i): pointer for the starting phase for node i, considering all 
c --            the phases in the whole network 
	INTEGER,allocatable::node(:,:)
c --    node(i,j) : the information for every node
c --       node(i,1) : node number
c --    if node(i,2) 1: no control
c --                 2: yield control
c --                 3: stop control
c --                 4: signal control
c --                 5: actuated signal control
c --    node(i,3): number of phases, only for control types 4 and 5.
c --    node(i,4): cycle length in seconds, only for control types 4 and 5.
      INTEGER,allocatable::nsign(:,:)
c --  nsign (i,j) : information for phase i (i is a counter on all phases
c --                in the network).
c --  nsign(i,1) : phase number.
c --  nsign(i,2) : offset (if pretimed) or maximum green (if actuated).
c --  nsign(i,3) : green time (if pretimed) or minimum green (if actuated).
c --  nsign(i,4) : amber time (yellow).
c --  nsign(i,5) : number of inbound links in this phase.
c --  nsign(i, 6:11) : upstream nodes of the inbound links (1 through nsign(i,5)).
c --                   note: the downstream node of these links is the node at which
c --                         this signal exists.
      integer(1),allocatable::total_count(:)
c --  total_count(i): total number of approaches with queue, for the same downstream node, other than link i
	integer(1),allocatable::green(:,:)
c --  green(i,j) : allocated green time for movement from link i to link j
c --               during each simulation interval (in seconds).
      integer(1),allocatable::movement(:,:,:)
c --  movement(i,j,k) : an indicator to define which movement is allowed
c --                    during each phase.
c --                    if(movement(i,j,k)= 1, then movement from link i
c --                    to link k is allowed during phase j, and 0 otherwise.



!*********************Dual Ring****************************
!added by hayssam on 6/21/2006 for dual ring
      integer,allocatable:: Dual_Entry(:)
	integer,allocatable:: Dual_Entry_Act(:)
	integer,allocatable:: Stay_In_Green(:)
	integer,allocatable:: Lead_Flag(:)
	integer,allocatable:: Call_For_Service(:)
!Hooram
	integer,allocatable:: Dual_Offset(:)
!Hooram	
!**********************************************************




      INTEGER cmalink(nu_ph1)
c --  cmalink(i): the critical link in phase i, which is the link with the
c --              maximum queue during the phase.
      real cma(nu_ph1)
c --  cma(i): the vehicle queue on the critical link of phase i.
      real cma_time(nu_ph1)
c --  cma_time(i): time required  discharge the maximum queue of phase i
c --               according to the saturation flow rate.
      integer(1),allocatable::SignalPreventBack(:,:)
c --  SignalPreventBack(i,j): index to indicate whether the movement to link i from movement j (determined by backpointr, see MoveNoBackLink function)
c --               is prevented or not, i and j are in the backward star
c --               representation.  Orignally called prevent()
c --  = 1, the movemet is prevented, =0, otherwise
	integer(1),allocatable::SignalPreventFor(:,:)
c --  SignalPreventFor(i,j): index to indicate whether the movement to link i from movement j (determined by inlink(j,.), see MoveNoForLink)
c --               is prevented or not, i and j are in the forward star
c --               representation.  Originally called prevent1()
c --  = 1, the movemet is prevented, =0, otherwise
      integer(1),allocatable::GeoPreventFor(:,:), GeoPreventBack(:,:)
c --  GeoPreventFor(i,j) = 1: the inbound movement j (determined by inlink) is prevented for forward*
c --                       link i due to the movement.dat
c --  GeoPreventBack(i,j): the inbound movement j (determined by backpointr) is prevented for backward*
c --                       link i due to movement.dat


! Feb 25 2010: HOV & HOT Toll
      real::PenForPreventMove = 99999
	real::PenForHOV = 99999
!End Feb 25 2010: HOV & HOT Toll
      integer phase
c --  ###  Not Defined Yet ###
      real length
      integer gcycle
c      integer g
      real,allocatable::strtsig(:)
c --  strtsig(k): the start time of signal plan k.
      INTEGER almov(nu_mv,nu_mv)
c --  almov(i,j):  a list of nodes to which the movements from the current approach are allowed.
c --  i here is the upstream node of the current link of the vehicle, j is a counter
c --  almov(i,j) is the downstream node of one downstream links of link i
c --



c -- ********** Demand/Vehicle Loading**********
	integer::MaxVehicles = 0
c --  Max # of vehicles to be generated.  
      real tnext,tnextT, tnextH 
c --  tnext: starting time of the next demand table
c --  tnextT: starting time of the next truck demand table

! Added by Xuesong and Hayssam Jan 22 2004 0.930.9
c --  tnextH: starting time of the next HOV demand table
	integer CntDemtime
c --  CntDemTime: count indicating which demand table the current time is at
	integer CntDemtimeT
c --  CntDemTimeT: count indicating which truck demand table the current time is at

! Added by Xuesong and Hayssam Jan 22 2004 0.930.9
	integer CntDemtimeH
c --  CntDemTimeH: count indicating which HOV demand table the current time is at


      real multi, multiT, multiH
c --  multi : multiplication factor for the demand (i.e. each value in the
c --         provided OD matrix will be multiplied by this factor to specify
c --         the number of vehicle generated from each zone to each desti
c --  multiT: for truck

! Added by Xuesong and Hayssam Jan 22 2004 0.930.9
c --  multiH: for HOV

	integer nints, nintsT, nintsH
c --  nints: number of demand tables/intervals
c --  nintsT: number of demand tables/intervals for truck
! Added by Xuesong and Hayssam Jan 22 2004 0.930.9
c --  nintsH: number of HOVedemand tables/intervals for truck

      real,allocatable::begint(:)
c --  begint(k): the start time of demand interval k.
      real,allocatable::begintT(:)
c --  begintT(k): the start time of demand interval k.

! Added by Xuesong and Hayssam Jan 22 2004 0.930.9
      real,allocatable::begintH(:)
c --  begintH(k): the start time of HOV demand interval k.


	INTEGER,allocatable::vlg(:)
c --  vlg(i): the number of vehicles generated on link i during
c --          the current simulation interval.

! Added by MTI team Jan 17 2004 0.930.7D
	integer,allocatable::vlg_vhcID(:,:)
c --  vlg_vhcID(i,j): the jth vehicle ID generated on link i during
c --          the current simulation interval.


	INTEGER,allocatable::gen(:)
c --  gen(i) : the number of vehicle to be generated on link i.
	INTEGER,allocatable::NoofGenLinksPerZone(:)
c --  NoofGenLinksPerZone(i): # of generation links in zone i
!INTEGER,allocatable::LinkNoInZone(:,:)
! Modified by MTI team Jan 24 2004, 0.930.9
	integer,allocatable::LinkNoInZone(:,:)

c --  LinkNoInZone(i,j): the ID of the jth generation link for zone i
	real,allocatable::zdem(:,:)
c --  zdem(i,j): the demand table entry for Origin i, dest j and OD interval t

	real,allocatable::zdemT(:,:)
c --  zdemT(i,j): the truck demand table entry for Origin i, dest j and OD interval t

! Added by Xuesong and Hayssam Jan 22 2004 0.930.9
	real,allocatable::zdemH(:,:)
c --  zdemH(i,j): the HOV demand table entry for Origin i, dest j and OD interval t


      integer, allocatable::NoofConsPerZone(:)
c --  NoofConsPerZone(i): Number of Connectors per zone

      integer,allocatable::ConNoInZone(:,:)
c --  ConNoInZone(i,j): Node number for connector j in zone i
	real,allocatable::TotalLinkLenPerZone(:)
c --  TotalLinkLenPerZone(i): total lane length for all generation link in zone i

	real,allocatable::ztdemGen(:)
c --  ztdemGen(i): sum of demand tabel entries zdem over all dest zones for origin i and cnrrent demand time interval
	real,allocatable::ztdemGenT(:)
c --  ztdemGenT(i): sum of demand tabel entries zdem over all dest zones for origin i and cnrrent demand time interval


! Added by Xuesong and Hayssam Jan 22 2004 0.930.9
	real,allocatable::ztdemGenH(:)
c --  ztdemGenH(i): sum of demand tabel entries zdem over all dest zones for origin i and cnrrent demand time interval

	real,allocatable::ztdemAtt(:,:)
c --  ztdemAtt(i.t): sum of demand tabel entries zdem over all origin zones for dest i and time t
	real,allocatable::zfdem(:,:)
c --  zfdem(i,:): accumulative demand from zone i to all zones at current time

! Added by Xuesong and Hayssam Jan 22 2004 0.930.9
	real,allocatable::zfdemT(:,:)
c --  zfdemT(i,:): accumulative truck demand from zone i to all zones at current time
	real,allocatable::zfdemH(:,:)
c --  zfdemH(i,:): accumulative HOV demand from zone i to all zones at current time


	real,allocatable::expgenz(:)
c --  expgenz(i): total demand generation (vehicles) for zone i at current OD time interval
	real,allocatable::expgenzT(:)
c --  expgenzT(i): total truck demand generation (vehicles) for zone i at current OD time interval

! Added by Xuesong and Hayssam Jan 22 2004 0.930.9
	real,allocatable::expgenzH(:)
c --  expgenzH(i): total HOV demand generation (vehicles) for zone i at current OD time interval

	real,allocatable::expgen(:)
c --  expgen(i): new demand generation (vehicles) for link i for current simulation interval
	real,allocatable::expgenT(:)
c --  expgenT(i): new truck demand generation (vehicles) for link i for current simulation interval

! Added by Xuesong and Hayssam Jan 22 2004 0.930.9
	real,allocatable::expgenH(:)
c --  expgenH(i): new HOV demand generation (vehicles) for link i for current simulation interval


c  -- ********** Control/Operational Scenarios**********
      Type WZ
       integer FNode
	 integer TNode
       real ST          ! starting time
	 real ET          ! ending time
	 real CapRed      ! percentage of capacity reduction
       integer SpeedLmt ! speed limit in work zone
	 integer Discharge ! discharge rate at work zone
!April 4 2008
!	 integer OrigDisChg
	 real OrigDisChg
	 
	 integer OrigSpdLmt
!Added Oct 4 2005
       integer delta_speed !the difference between org speed limit and wz speed limit
	end Type

	type (WZ), allocatable:: WorkZone(:)
      integer WorkZoneNum
      logical, allocatable:: wzstartflag(:)

! Modified by Xuesong and Hayssam Jan 22 2004 0.930.9
!      real total_hov
      integer inci_num
c --  # of incident/workzone to be modeled
	integer listtotal
c --  listtotal : number of active incidents at a given time
      integer InfoPM
c --  InfoPM: switch to indicate if the in-vehicle information preempt VMS

! comma out by hayssam and xuesong to have the code and gui consistent
! Dyna 930.9 feb 2 2004 

!
*************** old *********************************
c --          1: In-vehicle information preempts VMS
c --          0: VMS preempts in-vehicle information
**************** new *************************************
c --          0: In-vehicle information preempts VMS
c --          1: VMS preempts in-vehicle information

      integer ipinit
c --  ipinit: switch for choosing the path
c --         0: randomly choose a path from K path
c --         1: assign the best path
	integer nrate      
c --  nrate : time interval for checking the ramp metering (in minutes)
	integer dec_num
c  -- dec_num: # of ramp metering detectors in the network
	real,allocatable::ramp_par(:,:)
c --    ramp_par(i,3)
c --    ramp_par(i,1): cons1, used in RATE=RATEP+cons1(cons2-OCC)
c --    ramp_par(i,2): cons2
c --      According to simulation experiments, the default values of
c --      cons1 and cons2 are 0.32 and 0.2 respectively and these values
c --      may be calibrated using actual data.
c --    ramp_par(i,3): ramp rate (sturation flow rate on the ramp veh/sec/lane)
C --
	!Added July 24 2007	
	integer,allocatable::ramp_type(:)
c --  ramp_type(i) : 1: pretimed; 2: traffic responsive; 3: feedback control
	integer lookup_num
c --  lookup_num: # of lookup tables for traffic responsive ramp metering
	real,allocatable::ramp_lookup(:,:,:)
c --  1st dimension: lookup table number
c --  2nd dimension:
c --  ramp_lookup(i,1,j): metering rate
c --  ramp_lookup(i,2,j): occupancy
c --  ramp_lookup(i,3,j): volume
c --  ramp_lookup(i,4,j): speed
c --  3rd dimension: metering level 1 to 8
      !End 

 	real,allocatable::ramp_start(:)
c --  ramp_start(i) : the time at which the ramp meter i starts its function.
      real,allocatable::ramp_end(:)
c --  ramp_end(i) : the time at which the ramp meter i ends its function.


	INTEGER,allocatable::detector(:,:)
c --  detector(i,j) : stores information for detector i.
c --  detector(i,1): detector number
c --  detector(i,2): upstream node of the link on which the detector exists.
c --  detector(i,3): downsream node of the link on which the detector exists.
c --  detector(i,4): position of the first detector on the downstream link (in feet).
c --  detector(i,5): position of the second detector on the downstream link (in feet).
c --  both distances in 4 and 5 are measured from the downstream node (detector(i,3)).
c --  detector(i,6): upstream  node of the metered ramp
c --  detector(i,7): downstream  node of the metered ramp
c --

	INTEGER,allocatable::link_detector(:)
c --  link_detector(i): the detector exists on link i.
	real,allocatable::detector_length(:)
c --  detector_length(i): the length of the detector in feet.
	INTEGER,allocatable::detector_ramp(:)
c --  detector_ramp(i) : the link on which the metering i exists. This link will
c --                     be an on ramp.
	INTEGER,allocatable::det_link(:)
c --  det_link(i) : the link on which detector i exists.
	real,allocatable::occup(:)
c --  occup(i): the occupancy on the link on which the detector of ramp i exists.

c --  +++++ VMS +++++
      type VMSTypeTwoPathType
       integer node
	 integer link
	end type
      Type (VMSTypeTwoPathType),allocatable::vmstypetwopath(:,:)
c --  vmstypetwopath(i,1-5): subpaths for detour (vms type2) 

! Added by MTI on March 20 2004 for DYNA 1.0 VMS type 4
!      INTEGER,allocatable::Detoured_Nodes(:,:) 


!Add by Zihan 20160606 for AMS project
! to Update control. network, travel time and penalty with predictive info
      integer :: i_new_control=0
      !it is a flag varible to indicate whether the system is reading new control plans
      ! i_new_control = 0: read existing opened control file
      ! i_new_control = 1: the control has been updated from new_control.dat, read the updated file
      integer :: i_predictiveinfo=0
      ! it is a flag variable to indicate if predictiveinfo mode is activated
      
      integer :: i_new_tt=0

      integer :: i_new_tp=0
      ! it is a flag varible to indicate whether the system is reading new predictive travel time and turn penalty
      ! i_new_tt/i_new_tp = 0: default value, no predictive info provided
      ! i_new_tt/i_new_tp = 1: the info has been updated, read the updated file
      integer :: latency=0
      ! latency is also given in predict.dat if predictiveinfo mode is activated
      
!      integer numof_predinterval     ! need to be aggregated for TravelTime_pred(:,:) and TravelPenalty_pred(:,:,:)
!      integer freq_predinterval
      
!      integer predictionmode

      real,allocatable :: TravelTime_pred(:,:)
c  -- TravelTime_pred: predictive travel time for each link at each so interval from Dtax-p
      real,allocatable :: TravelPenalty_pred(:,:,:)
c  -- TravelPenalty_pred: penalty for each link at each so interval from Dtax-p
! End addition





c  -- vms_num: Maximum number of dynamic message signs in the network.
      integer vms_num
	integer :: vms_num_reversible = 0   !! Add by Zihan and Archak on 20160301 for reversible lane
      integer(1),allocatable::vmstype(:)
c --  vmstype(i): the type of the vms sign.
c --  There are three different types of VMS :
c --  1. speed reduction
c --  2. assign a specific path
c --  3. divert vehciles to other paths
      INTEGER,allocatable::vms(:,:)
c --  vms(i,j): information for vms number i.
c --  vms(i,1): the link number on which the vms is located.
c --  vms(i,2): its definition depends on the vms type.
c --    type 1: speed threshold (+ or -). if the link speed is less than
c --            this threshold, increase the link speed by a pecentage
c --            equal to vms(i,3).
c --    type 2: the path number (among the k_paths) to which the vms refers.
c --    type 3: percentage of vehicles to be diverted.
c --    type 6: value of risk

c --  vms(i,3): its definition depends on the vms type.
c --    type 1: the percentage of reduction or increase in the speed of the link
c --            on which the vms is located.
c --    type 2: it is either 0 or a destination number.
c --             when 0, the path specified in vms(i,2) applies for all destinations
c --             when a destination number, the path is applied for vehiles heading
c --             this destination only.
c --    type 3: number of paths among which vehicles are diverted.
c --    type 4: number of nodes in detour sub-path
c --    type 5: speed reduction on the VMS link
c --    type 6: travel penalty in terms of percentage of link travel time
c --    type 7: look-up table number

	integer(1),allocatable::ivms(:)
c --  ivms(j) :  an index to indicate if a vehicle subjected to a divert vms
c --             reached the end of the link and ready to switch.
      real,allocatable::vms_start(:)
c --  vms(k): the time at which the vms k starts its operation.
	real,allocatable::vms_end(:)
c --  vms(k): the time at which the vms k ends its operation.
	
	! Added for weather VMS, June 8 2009
	integer num_vsl_table
	integer,allocatable::vsl_num_line(:)
	real,allocatable::vsl_v_l(:,:)
c --  vsl_v_l(i,j): the lower bound of visibility for look-up table i, record j.
	real,allocatable::vsl_v_u(:,:)
c --  vsl_v_u(i,j): the upper bound of visibility for look-up table i, record j.
	real,allocatable::vsl_r_l(:,:)
c --  vsl_r_l(i,j): the lower bound of rain precipitation for look-up table i, record j.
	real,allocatable::vsl_r_u(:,:)
c --  vsl_r_u(i,j): the upper bound of rain precipitation for look-up table i, record j.
	real,allocatable::vsl_s_l(:,:)
c --  vsl_s_l(i,j): the lower bound of snow precipitation for look-up table i, record j.
	real,allocatable::vsl_s_u(:,:)
c --  vsl_s_u(i,j): the upper bound of snow precipitation for look-up table i, record j.
	real,allocatable::vsl_speed(:,:)
c --  vsl_speed(i,j): the speed limit reduction for look-up table i, record j.
      INTEGER,allocatable::SpeedLimit_org(:)
c --  original post speed limit

      integer i_risk
      real, allocatable::risk(:,:,:,:)
c --  risk(link,linktype,vehtype,Iti_nu) stores the extra travel penalty (risk) of the link   
      ! End of June 8 2009

! Modified by Hayssam on March 25 for DYNA  1.0
! need to keep track of all workzones/incidents

!      Type Impact
!       integer InciMode
!       integer WZMode
!	 integer InciIM
!	 integer WZIM
!	end type
!      type (IMpact),allocatable::ImpactType(:)

c --  ImpactType(:)
c --  ImpactType(j)%Inci(WZ)Mode( = 0: vehicle j is not an impacted vehicle
c --  ImpactType(j)%Mode = 1: vehicle j is an impacted vehicle that doesn't divert
c --  ImpactType(j)%Mode = 2: vehicle j is an impacted vehicle that does divert
c --  ImpactType(j)%InciIM = ip: vehicle j is an impacted vehicle for inciddent location ip
c --  ImpactType(j)%WZIM   = ip: vehicle j is an impacted vehicle for Work Zone location ip
c --  +++++ Incident/ Work zone +++++


      Type Impact
	  INTEGER, POINTER :: InciMode(:) 
        INTEGER, POINTER :: WZMode(:)
      end type Impact
      type (IMpact),allocatable::ImpactType(:)


c --  ImpactType(j)%InciMode(k) = 0: vehicle j is not an impacted vehicle by Incident k
c --  ImpactType(j)%InciMode(k) = 1: vehicle j is impacted by Incident k, but does not divert
c --  ImpactType(j)%InciMode(k) = 2: vehicle j is impacted by Incident k, which diverts

c --  ImpactType(j)%WZMode(k) = 0: vehicle j is not an impacted vehicle by WZ k
c --  ImpactType(j)%WZMode(k) = 1: vehicle j is impacted by WZ k, but does not divert
c --  ImpactType(j)%WZMode(k) = 2: vehicle j is impacted by WZ k, which diverts


c --  +++++ Incident/ Work zone +++++



	real,allocatable::inci(:,:)
c --  inci(i,j): information for incident i
c --  inci(i,1): start time of incident i (minute)
c --  inci(i,2): end time of incident i (minute)
c --  inci(i,3): severity of incident i.
c --  inci(i,4): original flow rate of incident i.

      Logical,allocatable::incistartflag(:)
c --  incistartflag(i): flag indicating if ith incient/work zone has started
!      Logical,allocatable::inciendflag(:)
c --  inciendflag(i): flag indicating if ith incient/work zone has finished
	INTEGER,allocatable::incil(:)
c --  incil(k): the link on which incident k occurs.
	INTEGER,allocatable::incilist(:)
c --  incilist(i): gives a number of one of the active incidents.
	INTEGER,allocatable::itp(:)


c --  +++++ Bus +++++
      integer nubus
c --  nubus: number of buses considered in the whole simulation.
      INTEGER,allocatable::NoBusNode(:)
c --- # of nodes on bus paths
	integer,allocatable::busid(:)
c --  busid(j): the vehicle number corresponding to bus j.
	INTEGER,allocatable::buslink(:)
c --  buslink(j): the generation link of bus j.
      INTEGER::TotalBusGen = 0
c --  total # of buses that have already been loaded into the network
	integer(1),allocatable::ngenbus(:)
c --  ngenbus(i) =0 if bus i is not generated yet, and 1 otherwise
	real,allocatable::bustime(:)
c --  bustime(i):a variable to indicate the remaining time of the dewell time
c --            at the bus stop.
c --  The negative value for bustime indicates the remaining time for the
c --  bus to stop on the current link.  When the bus raeches a stop, it
c --  is required
c --  to stop for the dwelling time.  When bustime is not negative, this
c --  means that the bus will start moving again.
	real,allocatable::busstart(:)
c --  busstart(i): the start time of bus i.
	real,allocatable::busdwell(:)
c --  busdwell(i): average dwell time (in minutes).
	real tlatest_bus
c --  the latest starting time of all the bus


c  -- ********** Run-Time Link or Network Level Stats**********
	integer itag0, itag1,itag2,itag3,information,noinformation
      real dtotal1,dtotal2,dtotal3,vtothr1,vtothr2,vtothr3,triptime1,
     * triptime2,triptime3,stoptime,stopinfo,stopnoinfo,entry_queue1,
     * entry_queue2,entry_queue3,totaldecsion,ave_trip1,
     * ave_trip2,ave_trip3,vavg1,vavg2,vavg3,ave_entry1,ave_entry2,
     * ave_entry3,avestoptime,avestopnoinfo,avestopinfo,avedtotal1,
     * avedtotal2,avedtotal3
c --  temporary variables for calculating the final stats
      real entry_queue2_1,entry_queue2_2,entry_queue2_3,
     + entry_queue3_1,entry_queue3_2,entry_queue3_3,
     + triptime2_1,triptime2_2,triptime2_3,
     + triptime3_1,triptime3_2,triptime3_3,
     + vtothr2_1,vtothr2_2,vtothr2_3,
     + vtothr3_1,vtothr3_2,vtothr3_3,
     + dtotal2_1,dtotal2_2,dtotal2_3,
     + dtotal3_1,dtotal3_2,dtotal3_3,
     + stopinfo_1,stopinfo_2,stopinfo_3,
     + stopnoinfo_1,stopnoinfo_2,stopnoinfo_3
c --  temporary variables for calculating the final stats
      integer information_1,information_2,information_3
c --  informationi_*: number of vehicles with information with * stops in their trip chain
      integer noinformation_1,noinformation_2,noinformation_3
c --  informationi_*: number of vehicles without information with * stops in their trip chain 
      real vavg2_1, vavg2_2, vavg2_3, vavg3_1,vavg3_2,vavg3_3,
     + avedtotal2_1,avedtotal2_2,avedtotal2_3,
     + avedtotal3_1, avedtotal3_2,avedtotal3_3,
     + avestopinfo_1, avestopinfo_2,avestopinfo_3,
     + avestopnoinfo_1,avestopnoinfo_2,avestopnoinfo_3,
     + ave_entry2_1, ave_entry2_2, ave_entry2_3,
     + ave_entry3_1, ave_entry3_2, ave_entry3_3,
     + ave_trip2_1, ave_trip2_2, ave_trip2_3,
     + ave_trip3_1, ave_trip3_2, ave_trip3_3
      INTEGER,allocatable::volume(:)
c --  volume(i) : number of vehicles on link i at the current simulation 
c --            interval.
      INTEGER,allocatable::vehicle_queue(:) 
c --  vehicle_queue(i) : number of vehicle in the queue on link i at the
c --                   current simulation interval.


!**********************************
! added by hayssam to correctly compute the fraction of link that is a queue
! sep 8, 2003 for 930.7B
      real,allocatable::vehicle_queue_PCE(:) 
c --  vehicle_queue_PCE(i) : number of PCs (not vehicles) in the queue on link i at the
c --                   current simulation interval.
!****************************************


      integer,allocatable::decision(:)
c --  decision(j) : number of nodes at which vehicle j  may choose to switch.
c --              Note : for boundedly rational vehicles, all nodes in the 
c --                     path are decision nodes.  
      integer,allocatable::switch(:)
c --  switch(j) : number of path switches (rerouting) for vehicle j.
c --            Note : number of switches for each vehicle is less than
c --                   or equal to the number of decisions.
      integer,allocatable::decisionnum(:)
c --  decisionnum(i) : number of vehicles making i decsions.
      integer,allocatable::switchnum(:)
c --  decisionnum(i) : number of vehicles making i switches.
      integer totalswitch
c --  totalswitch : the sum of number of switches for all boundedly rational
c --               vehicles.
      integer totaldecision
c --  totaldecision : the sum of number of decisions for all boundedly rational
c --               vehicles.
      INTEGER,allocatable::outflow(:)
c --  outflow(i) : total number of vehicle exiting link i. 
      INTEGER,allocatable::outleft(:)
c --  outleft(i) : number of vehicles exiting link i making a left turn
c --              movement.(outleft(i) <= outflow(i)). 
	integer(1),allocatable::iflag(:)
c --  iflag(i): an index to indicate if the concentration on link i
c --            c(i) is greater than the maximum concentration.
c --  iflag(i)=1, the concentration exceeds the maximum concentration
c --           0, otherwise
	integer,allocatable::nout(:)
c --  nout(i): number of vehicles exiting the network through link i.
	INTEGER,allocatable::npar(:)
c --  npar(i): number of vehicles exist on link i
	INTEGER,allocatable::nTruck(:)
c --  nTruck(i): number of heavy vehicles (trucks and buses) exist on link i
	INTEGER,allocatable::nparold(:)
c --  nparold(i): number of vehicle exist on link i in the previous simulation interval
	real,allocatable::turnveh(:,:)
c --  turnveh(i,j): 
	real,allocatable::turnvehso(:,:)
c --  turnvehso(i,j): the number of vehicles in the queue on link i which moving to link j.
      integer(1),allocatable::isel(:)
c --  isel(j): an index to indicate whether or not vehicle j has moved to the
c --           downstream link.
c --  =1, the veicle has moved to the downstram link
c --  =0, otherwise.
	real,allocatable::c(:)
c --  c(i): concentration on link i.
	real,allocatable::v(:)
c --  v(i): speed on link i.
	real,allocatable::ctmp(:)
c --  ctmp(i) : the concentraion on the queue-free length of link i.
c --            It is used for short term prediction.
	real,allocatable::vtmp(:)
c --  vtmp(i): speed on the queue-free length of link i.
	real,allocatable::capacity(:,:)
c --  capacity(i,j): the capacity for the movement from link i to
c --                 through movement j to associated downstream link
	real,allocatable::captot(:)
c --  captot(i): maximum number of vehicles per lane that can exit
c --             link i during the simulation interval according to
c --             the green time assigned to this link.
      real, allocatable::truckpct(:)
c --  truckpct(i): percentage of truck presented on link i
	integer,allocatable::nmov(:)
c --  nmov(i): the number of vehicles (cummulative) that moved from link i
c --           to a downstream link.
	real,allocatable::partotal(:)
c --  ###  Not Defined Yet ###
	real,allocatable::distans(:)
c --  distans(j): distance that vehicle j traverses
	logical,allocatable::qflag(:)
c --  qflag(j): flag indicating if the vehicle is in the queue and won't be discharged
c --  in current simulation interval
	INTEGER,allocatable::ntryq(:)
c --  ntryq(i): entry queue for link i
	real,allocatable::statmpt(:)
c --  statmpt(i): average travel time on link i
!	integer,allocatable::intooi(:,:,:)
c --  attributes of vehicles ready to move into link nl at this simulation interval
c --  intooi(nl,intoo(nl),1)=j: vehicle j is the intoo(nl)th vehicle to move into link nl
c --  intooi(nl,intoo(nl),2)=i: vehicle j is moving from link i to link nl
c --  intooi(nl,intoo(nl),3)=kj: vehicle j is the kjth vehicle to be moved from link i
	type VehLink
        integer NVehIn ! # of vehicels ready to move in 
	  integer NVehOut ! # of vehicles ready to move out
	end type
	type(VehLink),allocatable::intoo(:)
c --  c. intoo(i) : the number of vehicles ready to move into link i.
	real,allocatable::tmp30(:)
	real,allocatable::tmp31(:)
	real,allocatable::tmp32(:)
	real,allocatable::tmp33(:)
	real,allocatable::tmp34(:)
	real,allocatable::tmp35(:)
	real,allocatable::tmp36(:)
	real,allocatable::tmp37(:)
	real,allocatable::tmp38(:)
	real,allocatable::tmp39(:)
	real,allocatable::tmp40(:)
c --  itmp30 - 39 are teh temporary arrays for averaging the link stats based
c --  on use input interval
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

      integer i18,i30,i31,i32,i33,i34,i35,i36,i37,i38,i39,i40
      integer i30_t,i31_t,i32_t,i33_t,i34_t,i35_t,i36_t,
     + i37_t,i38_t,i39_t,i40_t,idemand_info
c --  i* : switch for printing out link statistics to file *
c --  i*_t: time interval of printing out such info

! Added by Jing & Xuesong Mar 16 2004 Dynasmart 1.0
! i180 is for vehicle location file	
! i180_t is for time interval of printing vehicle location file
	integer i180, i180_t

c --  **********      GUI Related       **********
      integer int_d
c --  int_d : is the dispaly interval defined by the user (from the GUI).
	real::GUITotalTime = 0.0
c --  Total time for all vehicles used in GUI

c --    Added by Archak and Zihan on 20151111 for Speed Harmonization.
      real,allocatable :: speed_limit_change_time(:)      
!      real :: total_network_length=0
      real :: SH_distance = 2.0
      integer :: noofarcs_SH
!      real,allocatable::speed(:,:)
      integer :: SH_flag
      real :: updatetime_speedHarm
c --  v(i,t): speed on link i at time t.

      integer,allocatable :: links_for_SH(:,:)
      
      integer,allocatable :: flowmodel_SH(:)
      integer,allocatable :: link_SH(:)
      real,allocatable :: time_SH(:)
      integer :: shockwave_count = 0
 
c -- end of addition



c --    Added by Archak and Zihan on 20160325 for Reversible Lane.
     
      integer :: RevLane_flag
      integer :: noofarcs_Inbound, noofarcs_Inbound_ramp 
      integer, allocatable :: links_for_Inbound (:)
!      integer, allocatable :: links_for_Inbound_ramp (:,:)
!      integer, allocatable :: links_for_Inbound_offramp (:,:)
      integer :: noofarcs_Outbound, noofarcs_Outbound_ramp 
      integer, allocatable :: links_for_Outbound (:)
!      integer, allocatable :: links_for_Outbound_onramp (:,:)           
!      integer, allocatable :: links_for_Outbound_offramp (:,:)      
      integer :: schedule_Inbound(2), schedule_Outbound(2)
      integer :: direction_flag = 1
      
c -- end of addition

c --    Added by Zihan on 20160427 for Shoulder Lane.
     
      integer :: ShlLane_flag
      integer :: noofarcs_ShlLane, noofplans_ShlLane
      integer,allocatable::SE_ShLane(:,:) !the start and end time matrix for shoulder lane
      
c -- end of addition

	
c  -- =======================================
c  -- II.b: Trip Chain Related Variables
c  -- =======================================

      integer :: noofstops = 1
c  -- max # of stops in the trip chain
	real, allocatable:: IntDestDwell(:,:)
c --  IntDestDwell(nu_ve, noofstops): IntDestDwell(j,k):
c --  required activity duration for vehicle j at the kth destination in its trip (path).
	INTEGER, allocatable:: IntDestZone(:,:)
c --  IntDestZone(nu_ve, noofstops):IntDestZone(j,k): 
c --  the zone (original zone number) of the kth destination in the trip of vehicle j.
      INTEGER, allocatable:: IntDestPath(:,:)
c --  IntDestPath(nu_ve, Noofstops): IntDestPath(j,k)
c --  Number of nodes in the path from the beginning to the kth destination for vehicle j
      integer(1),allocatable:: NoOfIntDst(:)
c --  NoOfIntDst(nu_ve): NoOfIntDst(j):
c --  number of destinations in the trip of vehicle j.
      real,allocatable:: RemainDwell(:)
c --  RemainDwell(nu_ve): RemainDwell(j):
c --  the remaining time that vehicle j will spend at the currnt intermediate destination in its trip. 
      integer(1),allocatable:: DestVisit(:)
c --  DestVisit(nu_ve): DestVisit(j):
c --  the order of the current destination that vehicle j is heading to 

! May 23 2005 TD link toll (add time dimension to this array)
!     real, allocatable::cost(:,:,:)
      real, allocatable::cost(:,:,:,:)
!	cost(link,linktype,vehtype,Iti_nu) 
! End

c --  linktype:1 = HOV, 2: HOT
c --  vehtype: 1 = LOV, 2: HOV
c --  cost(i,1,1) = cost for LOV on HOV lane i, usually infinity
c --  cost(i,1,2) = cost for HOV on HOV lane i, 0
c --  cost(i,2,2) = cost for HOV on HOT lane i, user defined in pricing.dat
c --  cost(i,2,2) = cost for HOV on HOV lane i, user defined in pricing.dat



c  -- =======================================
c  -- II.c: MUC-DYNASMART Interfacing Variables
c  -- =======================================
c     Definition for MUC   
      integer iso_ok,iue_ok
c --  flag indicating if there is so or ue vehicles generated in the network

! Added by MTI team jan 24 2004
      integer ienroute_ok,ivms_ok
c --  flag indicating if there is enroute or vms vehicles generated in the network

! Added by MTI team Feb 9 2004 0.930.9
      integer best_iteration 
	real best_MOE
	real current_MOE
c --  best_iteration : store the iteration index for the best iteration
c --  best_MOE: the best avg trip time for the vehicles exiting the network
! End

      integer itedex
c --  itedex: number of iterations for MUC
	integer realdm
c --  realdm: vehicle generation modes
c --       0: load vehicles with vehicle file but not path file.  The paths are detemined by Dynasmart
c --       1: load vehicles with trip tables.  
c --       2: load vehicles with both vehicle and path files
!added by Hayssam August 19 2005
!	     3: load vehicles from vehicle and path files for select user classes, others will get path from simulator

	integer NO_PL_UC
	integer,allocatable::PL_UC(:)
	
	integer no_via
c --  no_via: # of violations between assignment to assigment (used for stopping criteria)
	integer aggint
c --  aggint: aggregation interval: in terms of # of simulation intervals
	integer iteration
c --  iteration: counter indicates which iteration it is at.
      real stagelength
c --  stagelength: the lenght of one stage.  In P version, stagelength is the planning horizon
	!real roll            !Modified by Zihan for DYNASMART-P only
	integer:: roll=1
c --  roll: roll period.  Used only in X version
      !real horizon      !Modified by Zihan for DYNASMART-P only
      integer horizon
c --  horizon: planning horizon.  Used only in X version
      real stagest
c --  stagest: stage starting time.  Used only in X version
      real muc_diff
c --  muc_diff: the threshold for difference between two path assignment solustions
c --            to be counted as one violation

! Added by Jason Feb 3 2004 DYNASMART-P 0.930.9
      integer numof_siminterval  ! number of simulation intervals 

! Added by MTI team Nov 18 2004
! to initialize arrays for MUC travel times and penalties when all veh are discharged before simulation time is reached
      integer::curr_agg_interval=0 ! current aggregation interval, updated in avg.f

      real,allocatable :: moveturnMG_sim(:,:,:)
c --  moveturnMG_sim(i,t,1): Number of vehicles make left turn on link i at SIMULATION INTERVAL t
c --  moveturnMG_sim(i,t,2): Number of vehicles make other turn on link i at SIMULATION INTERVAL t
      real,allocatable :: TravelTime_sim(:,:)
c  -- TravelTime_sim: travel time for each link at each simulation interval
      real,allocatable :: openaltyMG_sim(:,:,:)
c --  opeanltyMG_sim(i,t,m): MarginalForward * penalty for link i SIMULATION INTERVAL t and movement m.
      real,allocatable :: DiffMG_sim(:,:)
c --  DiffMG(i,t): difference between # of veh ready to move in link i and
c --  # of free slots in link i at SIMULATION INTERVAL t
	real,allocatable::PenaltyEntry_sim(:,:)
c     PenaltyEntry is the link entry time for each simulation interval
! End




      real,allocatable :: TravelTime(:,:)
c  -- TravelTime: travel time for each link at each so interval
      real,allocatable :: TravelPenalty(:,:,:)
c  -- TravelPenalty: penalty for each link at each so interval
      real,allocatable :: TravelLET(:,:)
c  -- TravelLET: link entry time
      real,allocatable :: moveturnMG(:,:,:)
c --  moveturnMG(i,t,1): Number of vehicles make left turn on link i at time t
c --  moveturnMG(i,t,2): Number of vehicles make other turn on link i at time t
      real,allocatable :: openaltyMG(:,:,:)
c --  opeanltyMG(i,t,m): MarginalForward * penalty for link i time t and movement m.
      real,allocatable :: DiffMG(:,:)
c --  DiffMG(i,t): Avg difference between Avg # of veh ready to move in link i and
c --  Avg # of free slots in link i at time t
!      type pathstruct
!        INTEGER                    :: node
!        type(pathstruct),pointer   :: next_node
!      end type pathstruct
c  -- Define the data structure for the muc grand path
	type PathAtt 
	   INTEGER   :: node_sum
	   INTEGER   :: node_number
	end type PathAtt
c --  Define the data structure for storing the attributes of each path in the Grand Path Set
!      type(pathstruct),pointer,dimension(:,:,:):: MucPath_Lov
!	type(pathstruct),pointer,dimension(:,:,:):: MucPath_Hov
!      type(pathstruct),pointer                 :: traverse

! Added by Xuesong June 17 2003

	TYPE MUCMEMBER
	  INTEGER, POINTER :: P(:) ! size associated with the path length
!	  INTEGER, POINTER :: P(:) ! size associated with the path length
	END TYPE MUCMEMBER
   
c --  Define the data structure for storing the attributes of each path in the Grand Path Set
	TYPE(MUCMEMBER), ALLOCATABLE :: MUCPath_Lov_Array(:,:,:)
	TYPE(MUCMEMBER), ALLOCATABLE :: MUCPath_Hov_Array(:,:,:)

! End


c --  MucPath_Lov, MucPath_Hov: initial pointer address for path between origin i, destinaion j
c --  Assignment interval t and path k
c --  The paths are built upon this starting address using link-list structure node sequence for each 
c --  path in the grand path set.  In this grand path set, each path is unique.  
c --  The path attributes (node sum and number of nodes on the path is stored in MucPathAtt_lov and hov
      type(PathAtt),allocatable,dimension(:,:,:):: MucPathAtt_lov
	type(PathAtt),allocatable,dimension(:,:,:):: MucPathAtt_hov
c --  Correspond to MucPath_Lov & Hov, path attributes
c --  for each path in the grand path set (MucPath_Lov & Hov)
	INTEGER, allocatable:: NumMucPath_lov(:,:)
	INTEGER, allocatable:: NumMucPath_hov(:,:)
c  -- Number of paths in the grand path set	  
	INTEGER, allocatable:: uepath_lov(:,:,:,:)      
	INTEGER, allocatable:: uepath_hov(:,:,:,:)      
	INTEGER, allocatable:: sopath_lov(:,:,:,:)      
	INTEGER, allocatable:: sopath_hov(:,:,:,:)      
c --  Arrays that keep the id for the mucpath between, origin i, destination j,
c --  assignment interval t and iteration k
      type mucpolicy
       INTEGER   :: nodenumber
       INTEGER   :: nodesum
       real      :: prob
       INTEGER	   :: NumOfVehicle
!       real	   :: NumOfVehicle
      end type mucpolicy
c --  define the data structure for storing the MUC routing policies
      type (mucpolicy),allocatable,dimension(:,:,:,:):: sopolicy_lov
      type (mucpolicy),allocatable,dimension(:,:,:,:):: sopolicy_hov
      type (mucpolicy),allocatable,dimension(:,:,:,:):: uepolicy_lov
      type (mucpolicy),allocatable,dimension(:,:,:,:):: uepolicy_hov
c  -- MUC routing policy.  The policy is defined in terms of the percentage of
c  -- the total vehicles go between origina i, destination j, assignment interval t and iteration k 
	real, allocatable::ueaccuprob_lov(:,:,:,:)
	real, allocatable::ueaccuprob_hov(:,:,:,:)
	real, allocatable::soaccuprob_lov(:,:,:,:)
	real, allocatable::soaccuprob_hov(:,:,:,:)
c  -- accumulated prob for UE/SO LOV/HOV paths for vehicle from i to j depart 
c  -- time t, taking path k
      integer(1), allocatable :: sonxz_lov(:,:,:)
	integer(1), allocatable :: sonxz_hov(:,:,:)
      integer(1), allocatable :: uenxz_lov(:,:,:)
	integer(1), allocatable :: uenxz_hov(:,:,:)
c  -- sonxz_lov: number of LOV SO vehicle from i to j depart at time t
c  -- sonxz_hov: number of HOV SO vehicle from i to j depart at time t
c  -- uenxz_lov: number of LOV UE vehicle from i to j depart at time t
c  -- uenxz_hov: number of HOV UE vehicle from i to j depart at time t
      integer(1), allocatable :: NumUePath_lov(:,:,:)
      integer(1), allocatable :: NumUePath_hov(:,:,:)
      integer(1), allocatable :: NumSoPath_lov(:,:,:)
      integer(1), allocatable :: NumSoPath_hov(:,:,:)
c  -- NumSoPath: number of SO paths from i to j depart at time t
c  -- NumUePath: number of UE paths from i to j depart at time t
      integer::muc_veh(nu_classes) = 0
c --  muc_veh: number of vehicles in each class that are generated or loaded
      integer Iti_nu


c --  iti_nu:  number of simulation intervals (6s) in an aggregation interval
	integer muc_path_total
c --  muc_path_total is the total possible muc paths in the grand path set
c --  it depends on the number of assignment intervals and iterations
c --  Currnt approximation is muc_path_total = soint*itedex

! Added by Jason June 19 2003
      integer muc_path_total_lov ! for MUCPath_Lov_Array, MucPathAtt_lov
	integer muc_path_total_hov ! for MUCPath_Hov_Array, MucPathAtt_hov
! end of modification

	real TotalViolation
c --  TotalViolation is the threshold for muc convergence
	real,allocatable::TTime(:,:)
c --  TTime(i,t): average travel on link i at aggregation interval t
	real,allocatable::TTpenalty(:,:,:)
c --  TTpenalty(i,t,m): average travel on link i at aggregation interval t on movement m
	real,allocatable::penaltyMG(:,:,:)
c  -  penaltyMG(i,t,m): link marginal for link i at aggregation interval t on movement m
	real,allocatable::TTmarginal(:,:,:)
c --  TTmarginal(i,t,m): total marginal for link i at aggregation interval t on movement m
      real,allocatable::PenaltyEntryMG(:,:)
c     PenaltyEntryMG is the marginal entry time for each link and agg int
      real,allocatable::PenaltyEntry(:,:)
c     PenaltyEntry is the link entry time
      INTEGER,allocatable::linfree(:)
c     linfree the free slot for link i


	TYPE(MUCMEMBER), ALLOCATABLE :: tmpMUCPath_Lov_Array(:,:,:)
	TYPE(MUCMEMBER), ALLOCATABLE :: tmpMUCPath_Hov_Array(:,:,:)
	TYPE(PathAtt), ALLOCATABLE, dimension(:,:,:):: tmpMucPathAtt_lov
	TYPE(PathAtt), ALLOCATABLE, dimension(:,:,:):: tmpMucPathAtt_hov

!Added by Jason Mar 4 2005 pre-process of vehicle arrays for memory improvement MUC
      integer::igps = 0 
	!0: without grand path set and routing polocies; 1: otherwise

	integer,allocatable::ODT_num_veh_UELOV(:,:,:) 
	integer,allocatable::ODT_num_veh_UEHOV(:,:,:) 
	integer,allocatable::ODT_num_veh_SOLOV(:,:,:) 
	integer,allocatable::ODT_num_veh_SOHOV(:,:,:) 
	! num of vehicles for each ODT
	      
	TYPE VehMember
	  INTEGER, POINTER :: P(:) ! size associated with the path length
	END TYPE VehMember	
	TYPE(VehMember), ALLOCATABLE :: ODT_veh_UELOV(:,:,:)
	TYPE(VehMember), ALLOCATABLE :: ODT_veh_SOLOV(:,:,:)
	TYPE(VehMember), ALLOCATABLE :: ODT_veh_UEHOV(:,:,:)
	TYPE(VehMember), ALLOCATABLE :: ODT_veh_SOHOV(:,:,:)
	! store vehicle ids for each ODT
!End  

!Added by Jason April 4 2005 for calculating total violations      
	!UELOV
	integer ODT_max_num_path_UELOV
      integer,allocatable::ODTK_UELOV(:,:,:)
	!num of paths for each ODT
	integer,allocatable::ODTK_nodesum_UELOV(:,:,:,:)
	!nodesum for each path K in each ODT
      real,allocatable::ODTK_numveh_UELOV(:,:,:,:)
	!num of vehicles for each path K in each ODT
	!Note: it's NOT the simulated num of vehs on path K of ODT,
	!it can be viewed as the current solution to be updated in the MSA
	!and is used for calculating total violations
	
	!SOLOV
      integer ODT_max_num_path_SOLOV
      integer,allocatable::ODTK_SOLOV(:,:,:)
	integer,allocatable::ODTK_nodesum_SOLOV(:,:,:,:)
      real,allocatable::ODTK_numveh_SOLOV(:,:,:,:)

	!UEHOV
      integer ODT_max_num_path_UEHOV
      integer,allocatable::ODTK_UEHOV(:,:,:)
	integer,allocatable::ODTK_nodesum_UEHOV(:,:,:,:)
      real,allocatable::ODTK_numveh_UEHOV(:,:,:,:)

	!SOHOV
	integer ODT_max_num_path_SOHOV
      integer,allocatable::ODTK_SOHOV(:,:,:)
	integer,allocatable::ODTK_nodesum_SOHOV(:,:,:,:)
      real,allocatable::ODTK_numveh_SOHOV(:,:,:,:)
!End

!Added by Jason May 10 2005 
!To reduce memory used for storing the arrays in the total violation calculations
      integer::i_ODTK_mem = 1 
	!if i_ODTK_mem = 1 then allocate the following one dimensioanl arrays
      !if i_ODTK_mem = 0 then allocate the above multi-dimensioanl arrays
      !UELOV
      integer ODTK_UELOV_mem
	!num of paths for each ODT
	integer,allocatable::ODTK_nodesum_UELOV_mem(:)
	!nodesum for each path K in each ODT
      real,allocatable::ODTK_numveh_UELOV_mem(:)
	!num of vehicles for each path K in each ODT
	!Note: it's NOT the simulated num of vehs on path K of ODT,
	!it can be viewed as the current solution to be updated in the MSA
	!and is used for calculating total violations
	
	!SOLOV
      integer ODTK_SOLOV_mem
	integer,allocatable::ODTK_nodesum_SOLOV_mem(:)
      real,allocatable::ODTK_numveh_SOLOV_mem(:)

	!UEHOV
      integer ODTK_UEHOV_mem
	integer,allocatable::ODTK_nodesum_UEHOV_mem(:)
      real,allocatable::ODTK_numveh_UEHOV_mem(:)

	!SOHOV
      integer ODTK_SOHOV_mem
	integer,allocatable::ODTK_nodesum_SOHOV_mem(:)
      real,allocatable::ODTK_numveh_SOHOV_mem(:) 
!End

c --  ============End of MUC=================   

c --  ============Begin X-specific variables=================   

	integer,allocatable:: counter_ary(:)

	TYPE LinkVehicleMEMBER
	  integer, POINTER :: P(:) ! dynamic pointer
	END TYPE LinkVehicleMEMBER

	TYPE(LinkVehicleMEMBER), ALLOCATABLE :: LinkVehID_ary(:) 
	! one dimentional array to hold the dynamic pointers


	integer,allocatable:: counter_ary_queue(:)

	TYPE QueueVehicleMEMBER
	  integer, POINTER :: P(:) ! dynamic pointer
	END TYPE QueueVehicleMEMBER

	TYPE(QueueVehicleMEMBER), ALLOCATABLE :: QueueVehID_ary(:) 
	! one dimentional array to hold the dynamic pointers

	integer interval_observation
      integer interval_observation_ode
      integer interval_od
	
	logical get_demand_from_ODP
	real(4) ODP_demand_start_time
	real(4) ODP_demand_end_time
	
	integer(2), allocatable:: LinkIndex_obs(:)
	
	integer GUI_aggint
	      
	real GUI_stagest
	real GUI_stagelength
	integer GUI_numcars
	
	real GUI_AvgDen	
	
	real GUI_AvgStop
	real GUI_AvgTravelTime
	

	real GUI_AvgSpeed_Free  ! average speed on freeway
	real GUI_AvgSpeed_Net   ! average speed on surface streets
	real GUI_AvgSpeed

c --  ============End X-specific variables=================   


c --  ============Begin RTDYNA-specific variables=================   

      logical save_linkprop_for_CCA
	logical save_variables_for_CCB

	real(4), allocatable :: cca_scale(:,:)  
C	This is allocated in dynasmart subroutine.

c  -- TravelPenalty: penalty for each link at each so interval
c  -- TravelTime: travel time for each link at each so interval
c  -- TravelLET: link entry time
	logical ksp_allocated
	logical predinfo_allocated
	real(4) ksp_cal_time
      real,allocatable :: TravelTimePRED(:,:),TravelPenaltyPRED(:,:,:)
	real,allocatable :: PenaltyEntryPRED(:,:)
C     The three arrays are allocated in allocate_predinfo.f which is only in -X.

!    FOR GUI	 more comments required.
	real,allocatable::GUI_c(:)
	real,allocatable::GUI_q(:) ! Link flow
	real,allocatable::GUI_v(:)
	real,allocatable::GUI_QueueRatio(:)

	real GUI_timenow

c --  ============End RTDYNA-specific variables=================   


c --  ============Begin PDYNA-Specific=================   


	logical save_linkprop_for_ODE

c	for calculating link proportions
		integer, parameter :: li_nu = 800
		integer, parameter :: nu_obs = 16
		integer, parameter :: nu_tdi = 9
		integer, parameter :: nu_nod = 3700
		integer, parameter :: nu_zon = 61
!          integer, parameter :: max_lp_fraction = 100
!	 	integer, parameter :: max_lp = 
!     *				nu_nod*li_nu*nu_tdi*nu_obs/max_lp_fraction
		integer, parameter :: max_lpode = 5

          integer kkll_ode !number of nonzero elements in LP from PDYNA to ODE
 		real(4) kyod_ode(nu_obs,nu_tdi,nu_nod)
 		real(4) proportion_ode
		real(4),allocatable:: ppp2(:,:)
	! dynamic array to keep the size (kkll, max_lpode)

!		real(4), ppp(max_lp,max_lpode)
		real(4),allocatable::ppp(:,:)
		integer ppp_size

		real(4) zdem1(nu_zon,nu_zon,nu_tdi)
          integer no_depart, kkll
	    integer, save :: iobservation

!    FOR GUI	 more comments required.
	real,allocatable::GUI_PredDensity(:,:)
	real,allocatable::GUI_PredSpeed(:,:)
	real,allocatable::GUI_PredQueueRatio(:,:)
	real,allocatable::GUI_PredFlow(:,:)













c --  ============End PDYNA-Specific=================   



      CONTAINS
! --  General function calls and subroutines 

! Added by Jason June 19 2003
! modified by Xuesong June 21 2003

! This subroutine is used to take care of the reallocate of MUCPath_Lov_Array , MUCPath_HOV_Array
! MucPathAtt_lov, and MucPathAtt_hov in case of the muc_path_total (size) is updated
! if index = 1 => reallocate the arrays related to LOV: MUCPath_Lov_Array, MucPathAtt_lov
! if index = 2 => reallocate the arrays pertinent to HOV: MUCPath_HOV_Array, MucPathAtt_hov
      
	SUBROUTINE MUCArray_Reallocate(index)
!   Use noof_master_destinations_original intead of noof_master_destinations
      integer index
      integer OldSize
      integer error
	integer ix, iy, iz
	integer psize

!      print *, 'running MUCArray_Reallocate'


      IF(index .eq. 1) THEN

      ! create temp array to store contents of array

! Modified by MTI team Jan 23 2004 0.930.9
!	allocate(tmpMUCPath_Lov_Array
!     *(noofnodes,noof_master_destinations_original,muc_path_total_lov),
!     +stat=error)
	allocate(tmpMUCPath_Lov_Array
     *(nzones,noof_master_destinations_original,muc_path_total_lov),
     +stat=error)


	if(error.ne.0) then
	 write(911,*) "allocate mucpath_array error - insufficient memory"
	 stop
	endif

! Modified by MTI team Jan 23 2004 0.930.9
!      allocate(tmpMucPathAtt_lov
!     *(noofnodes,noof_master_destinations_original,muc_path_total_lov),
!     +stat=error)
      allocate(tmpMucPathAtt_lov
     *(nzones,noof_master_destinations_original,muc_path_total_lov),
     +stat=error)
	if(error.ne.0) then
	 write(911,*) "allocate mucpath_array error - insufficient memory"
	 stop
	endif

	! Copy content of old array to temp pointer
!      tmpMUCPath_Lov_Array = MUCPath_Lov_Array
!	tmpMucPathAtt_lov = MucPathAtt_lov

      do ix=1, muc_path_total_lov ! only copy up to the OldSize not MucPathAtt_lov
	 do iy=1, noof_master_destinations_original

! Modified by MTI team Jan 17 2004 0.930.7D
!	  do iz=1, noofnodes
	  do iz=1, nzones

	tmpMucPathAtt_lov(iz,iy,ix)%node_sum =
     + MucPathAtt_lov(iz,iy,ix)%node_sum
	tmpMucPathAtt_lov(iz,iy,ix)%node_number =
     + MucPathAtt_lov(iz,iy,ix)%node_number

	psize = MucPathAtt_lov(iz,iy,ix)%node_number

	if(psize .gt.0) then
      ALLOCATE(tmpMUCPath_Lov_Array(iz,iy,ix)%P(psize),stat=error)
	 if(error.ne.0) then
	   write(911,*) 'allocate tmpMUCPath_Lov_Array vectorerror'
	   stop
	 endif
	endif

	   do iu = 1, psize
		tmpMUCPath_Lov_Array(iz,iy,ix)%P(iu)
     +		 = MUCPath_Lov_Array(iz,iy,ix)%P(iu)
	   enddo

	if(psize .gt.0) then

	if (associated(MUCPath_Lov_Array(iz,iy,ix)%P)) then
	  DEALLOCATE(MUCPath_Lov_Array(iz,iy,ix)%P,stat=error)
	  if(error.ne.0) then
	    write(911,*)"deallocate MUCPath_Lov_Array%P vector error"
!	    print *,"deallocate MUCPath_Lov_Array%P vector error"
	    pause
	  endif
      endif
	endif

	  enddo
	 enddo
	enddo

      ! Delete the old array
      deallocate(MUCPath_Lov_Array)
	deallocate(MucPathAtt_lov)

      ! reallocate array
      OldSize = muc_path_total_lov
      muc_path_total_lov = muc_path_total_lov + 10 ! increase by 10

!	print *, 'old size=', OldSize, '  new size=', muc_path_total_lov 

! Modified by MTI team Jan 17 2004 0.930.7D
!      allocate(MUCPath_Lov_Array
!     *(noofnodes,noof_master_destinations_original,muc_path_total_lov),
!     +stat=error)
      allocate(MUCPath_Lov_Array
     *(nzones,noof_master_destinations_original,muc_path_total_lov),
     +stat=error)

	if(error.ne.0) then
	 write(911,*) "allocate mucpath_array error - insufficient memory"
	 stop
	endif

! Modified by MTI team Jan 17 2004 0.930.7D
!      allocate(MucPathAtt_lov
!     *(noofnodes,noof_master_destinations_original,muc_path_total_lov),
!     +stat=error)
      allocate(MucPathAtt_lov
     *(nzones,noof_master_destinations_original,muc_path_total_lov),
     +stat=error)
	if(error.ne.0) then
	 write(911,*) "allocate mucpath_array error - insufficient memory"
!	 print *, "allocate mucpath_array error - insufficient memory"
	 stop
	endif

      ! Copy contents from temp back to array
      do ix=1, OldSize ! only copy up to the OldSize not MucPathAtt_lov
	 do iy=1, noof_master_destinations_original

! Modified by MTI team Jan 17 2004 0.930.7D
!	  do iz=1, noofnodes
	  do iz=1, nzones

!	   MUCPath_Lov_Array(iz,iy,ix) = tmpMUCPath_Lov_Array(iz,iy,ix)
!	   MucPathAtt_lov(iz,iy,ix) = tmpMucPathAtt_lov(iz,iy,ix)
	MucPathAtt_lov(iz,iy,ix)%node_sum =
     + tmpMucPathAtt_lov(iz,iy,ix)%node_sum
	MucPathAtt_lov(iz,iy,ix)%node_number =
     + tmpMucPathAtt_lov(iz,iy,ix)%node_number

	psize = tmpMucPathAtt_lov(iz,iy,ix)%node_number

	if(psize .gt.0) then
      ALLOCATE(MUCPath_Lov_Array(iz,iy,ix)%P(psize),stat=error)
	 if(error.ne.0) then
	   write(911,*) 'allocate MUCPath_Lov_Array vectorerror'
	   stop
	 endif
	endif

	   do iu = 1, psize
		MUCPath_Lov_Array(iz,iy,ix)%P(iu)
     +		 = tmpMUCPath_Lov_Array(iz,iy,ix)%P(iu)
	   enddo

	if(psize .gt.0) then
	if (associated(tmpMUCPath_Lov_Array(iz,iy,ix)%P)) then
	  DEALLOCATE(tmpMUCPath_Lov_Array(iz,iy,ix)%P,stat=error)
	  if(error.ne.0) then
	    write(911,*)"deallocate tmpMUCPath_Lov_Array%P vector error"
!	    print *,"deallocate tmpMUCPath_Lov_Array%P vector error"
	    pause
	  endif
      endif

	endif
	  enddo
	 enddo
	enddo

      ! initialize array for the remaining elements
      do ix=OldSize+1, muc_path_total_lov
	 do iy=1, noof_master_destinations_original

! Modified by MTI team Jan 17 2004 0.930.7D
!	  do iz=1, noofnodes
	  do iz=1, nzones

!	   MUCPath_Lov_Array(iz,iy,ix)%P=>null()
	   MucPathAtt_lov(iz,iy,ix)%node_sum = 0
	   MucPathAtt_lov(iz,iy,ix)%node_number = 0
	  enddo
	 enddo
	enddo

      ! Delete the temp arrays
      ! if(associated(tmpMUCPath_Lov_Array)) deallocate(tmpMUCPath_Lov_Array)
      deallocate(tmpMUCPath_Lov_Array)

	deallocate(tmpMucPathAtt_lov)

	ENDIF !(index = 1)

      IF(index .eq. 2) THEN

      ! create temp array to store contents of array
! Modified by MTI team Jan 23 2004 0.930.9
!	allocate(tmpMUCPath_hov_Array
!     *(noofnodes,noof_master_destinations_original,muc_path_total_hov),
!     +stat=error)
	allocate(tmpMUCPath_hov_Array
     *(nzones,noof_master_destinations_original,muc_path_total_hov),
     +stat=error)
	if(error.ne.0) then
	 write(911,*) "allocate mucpath_array error - insufficient memory"
	 stop
	endif

! Modified by MTI team Jan 23 2004 0.930.9
!      allocate(tmpMucPathAtt_hov
!     *(noofnodes,noof_master_destinations_original,muc_path_total_hov),
!     +stat=error)
      allocate(tmpMucPathAtt_hov
     *(nzones,noof_master_destinations_original,muc_path_total_hov),
     +stat=error)
	if(error.ne.0) then
	 write(911,*) "allocate mucpath_array error - insufficient memory"
	 stop
	endif

	! Copy content of old array to temp pointer
!      tmpMUCPath_hov_Array = MUCPath_hov_Array
!	tmpMucPathAtt_hov = MucPathAtt_hov

      do ix=1, muc_path_total_hov ! only copy up to the OldSize not MucPathAtt_hov
	 do iy=1, noof_master_destinations_original
! Modified by MTI team Jan 17 2004 0.930.7D
!	  do iz=1, noofnodes
	  do iz=1, nzones

	tmpMucPathAtt_hov(iz,iy,ix)%node_sum =
     + MucPathAtt_hov(iz,iy,ix)%node_sum
	tmpMucPathAtt_hov(iz,iy,ix)%node_number =
     + MucPathAtt_hov(iz,iy,ix)%node_number

	psize = MucPathAtt_hov(iz,iy,ix)%node_number

	if(psize .gt.0) then
      ALLOCATE(tmpMUCPath_hov_Array(iz,iy,ix)%P(psize),stat=error)
	 if(error.ne.0) then
	   write(911,*) 'allocate tmpMUCPath_hov_Array vectorerror'
	   stop
	 endif
	endif

	   do iu = 1, psize
		tmpMUCPath_hov_Array(iz,iy,ix)%P(iu)
     +		 = MUCPath_hov_Array(iz,iy,ix)%P(iu)
	   enddo

	if(psize .gt.0) then

	if (associated(MUCPath_hov_Array(iz,iy,ix)%P)) then
	  DEALLOCATE(MUCPath_hov_Array(iz,iy,ix)%P,stat=error)
	  if(error.ne.0) then
	    write(911,*)"deallocate MUCPath_hov_Array%P vector error"
!	    print *,"deallocate MUCPath_hov_Array%P vector error"
	    pause
	  endif
      endif
	endif

	  enddo
	 enddo
	enddo

      ! Delete the old array
      deallocate(MUCPath_hov_Array)
	deallocate(MucPathAtt_hov)

      ! reallocate array
      OldSize = muc_path_total_hov
      muc_path_total_hov = muc_path_total_hov + 10 ! increase by 10

!	print *, 'old size=', OldSize, '  new size=', muc_path_total_hov 

! Modified by MTI team Jan 23 2004 0.930.9
!      allocate(MUCPath_hov_Array
!     *(noofnodes,noof_master_destinations_original,muc_path_total_hov),
!     +stat=error)
      allocate(MUCPath_hov_Array
     *(nzones,noof_master_destinations_original,muc_path_total_hov),
     +stat=error)
	if(error.ne.0) then
	 write(911,*) "allocate mucpath_array error - insufficient memory"
	 stop
	endif

! Modified by MTI team Jan 23 2004 0.930.9
!      allocate(MucPathAtt_hov
!     *(noofnodes,noof_master_destinations_original,muc_path_total_hov),
!     +stat=error)
      allocate(MucPathAtt_hov
     *(nzones,noof_master_destinations_original,muc_path_total_hov),
     +stat=error)
	if(error.ne.0) then
	 write(911,*) "allocate mucpath_array error - insufficient memory"
!	 print *, "allocate mucpath_array error - insufficient memory"
	 stop
	endif

      ! Copy contents from temp back to array
      do ix=1, OldSize ! only copy up to the OldSize not MucPathAtt_hov
	 do iy=1, noof_master_destinations_original

! Modified by MTI team Jan 17 2004 0.930.7D
!	  do iz=1, noofnodes
	  do iz=1, nzones
!	   MUCPath_hov_Array(iz,iy,ix) = tmpMUCPath_hov_Array(iz,iy,ix)
!	   MucPathAtt_hov(iz,iy,ix) = tmpMucPathAtt_hov(iz,iy,ix)
	MucPathAtt_hov(iz,iy,ix)%node_sum =
     + tmpMucPathAtt_hov(iz,iy,ix)%node_sum
	MucPathAtt_hov(iz,iy,ix)%node_number =
     + tmpMucPathAtt_hov(iz,iy,ix)%node_number

	psize = tmpMucPathAtt_hov(iz,iy,ix)%node_number

	if(psize .gt.0) then
      ALLOCATE(MUCPath_hov_Array(iz,iy,ix)%P(psize),stat=error)
	 if(error.ne.0) then
	   write(911,*) 'allocate MUCPath_hov_Array vectorerror'
	   stop
	 endif
	endif

	   do iu = 1, psize
		MUCPath_hov_Array(iz,iy,ix)%P(iu)
     +		 = tmpMUCPath_hov_Array(iz,iy,ix)%P(iu)
	   enddo

	if(psize .gt.0) then
	if (associated(tmpMUCPath_hov_Array(iz,iy,ix)%P)) then
	  DEALLOCATE(tmpMUCPath_hov_Array(iz,iy,ix)%P,stat=error)
	  if(error.ne.0) then
	    write(911,*)"deallocate tmpMUCPath_hov_Array%P vector error"
!	    print *,"deallocate tmpMUCPath_hov_Array%P vector error"
	    pause
	  endif
      endif

	endif
	  enddo
	 enddo
	enddo

      ! initialize array for the remaining elements
      do ix=OldSize+1, muc_path_total_hov
	 do iy=1, noof_master_destinations_original
! Modified by MTI team Jan 17 2004 0.930.7D
!	  do iz=1, noofnodes
	  do iz=1, nzones
!	   MUCPath_hov_Array(iz,iy,ix)%P=>null()
	   MucPathAtt_hov(iz,iy,ix)%node_sum = 0
	   MucPathAtt_hov(iz,iy,ix)%node_number = 0
	  enddo
	 enddo
	enddo

      ! Delete the temp arrays
      ! if(associated(tmpMUCPath_hov_Array)) deallocate(tmpMUCPath_hov_Array)
      deallocate(tmpMUCPath_hov_Array)

	deallocate(tmpMucPathAtt_hov)

	ENDIF !(index = 2)

      END SUBROUTINE
! End of modification

! --  Get Forward * link number from up and downstream nodes
      Integer Function GetFLinkFromNode(Unode,Dnode)
      Integer Unode,Dnode
      GetFLinkFromNode = 0
      do i = BackPointr(Dnode),BackPointr(Dnode+1)-1
        if(UNodeOfBackLink(i).eq.Unode) then
	     GetFLinkFromNode=BackToForLink(i)
	     exit
	  endif
      enddo
      end function

! --  Get Original Forward * link number from up and downstream nodes
      Integer Function GetOriginFLinkFromNode(Unode,Dnode)
      Integer Unode,Dnode
      GetOriginFLinkFromNode = 0
      do i = BackPointr(Dnode),BackPointr(Dnode+1)-1
        if(UNodeOfBackLink(i).eq.Unode) then
	     GetOriginFLinkFromNode=OriginLinkIndex(BackToForLink(i))
	     exit
	  endif
      enddo
      end function

! --  Get Backward * link number from up and downstream nodes
      Integer Function GetBLinkFromNode(Unode,Dnode)
      Integer Unode,Dnode
      GetBLinkFromNode = 0
      do i = BackPointr(Dnode),BackPointr(Dnode+1)-1
        if(UNodeOfBackLink(i).eq.Unode) then
	     GetBLinkFromNode = i
	     exit
	  endif
      enddo
      end function
	
      subroutine SwapLogArray(arg1,arg2)
	Logical arg1, arg2, c
        c = arg1
	  arg1 = arg2
	  arg2 = c
	end Subroutine

      subroutine SwapIntArray1B(arg1,arg2)
	Integer(1) arg1, arg2, c
        c = arg1
	  arg1 = arg2
	  arg2 = c
	end Subroutine
	
      subroutine SwapIntArray2B(arg1,arg2)
	INTEGER arg1, arg2, c
        c = arg1
	  arg1 = arg2
	  arg2 = c
	end Subroutine

	subroutine SwapRealArray(arg1,arg2)
	real arg1, arg2, c
        c = arg1
	  arg1 = arg2
	  arg2 = c
	end Subroutine			


      Integer Function MoveNoBackLink(ULink,DLink)
      INTEGER ULink,DLink
      MoveNoBackLink = 0

	do MO = backpointr(iunod(Dlink)),backpointr(iunod(DLink)+1)-1
        if(MO.eq.ForToBackLink(ULink)) then
		MoveNoBackLink = MO - backpointr(iunod(Dlink)) + 1
          exit
	  endif
	enddo
      end function



      Integer Function MoveNoForLink(ULink,DLink)
      INTEGER ULink,DLink

      MoveNoForLink = 0

	do MO = 1, inlink(DLink,nu_mv+1)
        if(ULink.eq.inlink(DLink,MO)) then
		MoveNoForLink = MO
          exit
	  endif
	enddo
      end function


      SUBROUTINE ErReadEOF(FileUnit)
      Character *20 FileUnit
	write(911,*) "Error! End-of-file while reading"
!      write(911,'(20x)') FileUnit
      write(911,*) FileUnit
	stop
!960   FORMAT (20H)
	END SUBROUTINE

! Added by Xuesong and Jason July 9, to have the same stream of random numbers in DYNASMART
      SUBROUTINE DYNA_random_number(harvest, sfrom)
      
	real harvest 
	integer sfrom

!	ind = ind + 1

	
	call random_number(harvest)


!	if(sfrom .eq. 1) then
!	  write(110,*) ind, ' demand_generation  ', harvest
!	elseif(sfrom .eq. 2) then
!	  write(110,*) ind, ' get_link_capacity  ', harvest
!	elseif(sfrom .eq. 3) then
!	  write(110,*) ind, ' get_sopath_hov     ', harvest
!	elseif(sfrom .eq. 4) then
!	  write(110,*) ind, ' get_sopath_lov     ', harvest
!	elseif(sfrom .eq. 5) then
!	  write(110,*) ind, ' get_uepath_hov     ', harvest
!	elseif(sfrom .eq. 6) then
!	  write(110,*) ind, ' get_uepath_lov     ', harvest
!	elseif(sfrom .eq. 7) then
!	  write(110,*) ind, ' get_veh_path       ', harvest
!	elseif(sfrom .eq. 8) then
!	  write(110,*) ind, ' vehicle_generation ', harvest
!	elseif(sfrom .eq. 9) then
!	  write(110,*) ind, ' vehicle_loading    ', harvest
!	elseif(sfrom .eq. 10) then
!	  write(110,*) ind, ' vehicle_moving     ', harvest
!	elseif(sfrom .eq. 11) then
!	  write(110,*) ind, ' vms_divert         ', harvest
!	endif

	END SUBROUTINE





	end module

