      subroutine title()
c --
c -- This subroutine prints the headings for all output files
c --
c -- This subroutine is called from main.
c -- This subroutine does not call any other subroutines.
c --
      use muc_mod
c --
      if(iteration.eq.0) then
      write(6,61)
61    format(15x,'****************************************************',
     +     /,15x,'*         D  Y  N  A  S  M  A  R  T  -  P          *',
     +     /,15x,'*                                                  *',
     +     /,15x,'* Intelligent Transportation Network Planning Tool *',
     +     /,15x,'*                                                  *',
     +     /,15x,'*                   Version (1.6)                  *',
     +     /,15x,'*                                                  *',
     +     /,15x,'*              Northwestern University             *',
     +     /,15x,'*                                                  *',
     +     /,15x,'*           Release Date:   June,  2016            *',     
     +     /,15x,'*           Update  Date:   June,  2016            *',     
     +     /,15x,'****************************************************')

      endif
c -- Title corrected from "University of Maryland -> Northwestern University" by JK (Oct 8,2010)
      !Modification: 10182013
!	write(666,*)
!
!      write(666,61)
      if(iterativeSummary) then
        if(iteration.eq.0) then
            write(666,*)
            write(666,61)
        endif
      else
            write(666,*)
            write(666,61)
      endif
      !Modification: 10182013
      
      ! Add by Archak 07032016      
      if (SH_flag) then
        write(6,*) 
        write(6,*) 'Speed Harmonization activated'
        write(6,*)
!      else 
!        write(6,*) 'Speed Harmonization not activated'
      endif      
      
c --
c -- Fort.18 : vehicle trajectory file
c --
      if(i18.eq.1) then
      write(18,181)
      write(18,182)
      write(18,183)
181   format(/,'****  Output file for vehicles trajectories ****')
182   format(  '=================================================')
183   format(  'This file provides all the vehicles trajectories')
      write(18,*)
      endif
c --
c -- Fort.30
c --
      if(i30.eq.1) then
      write(30,*) '******  Output file for vehicle generation ******'
      write(30,*) '================================================='
      write(30,*) 'This file provides the average number of vehicles'
      write(30,*) 'per sim. int., averaged over',i30_t,'sim. int.'
      write(30,*)
      endif
c --
c -- Fort.31
c --
      if(i31.eq.1) then
      write(31,*) '******  Output file for link volumes       ******'
      write(31,*) '================================================='
      write(31,*) 'This file provides the average number of vehicles'
      write(31,*) 'per sim. int. averaged over',i31_t, 'sim. int.'
      write(31,*)
      endif
c --
c -- Fort.32
c --
      if(i32.eq.1) then
      write(32,*) '******  Output file for vehicle queue      ******'
      write(32,*) '================================================='
      write(32,*) 'This file provides the average number of vehicles'
      write(32,*) 'in the queues on links per sim. int. averaged over'
     *            ,i32_t ,'sim. int.'
      write(32,*)
      endif
c --
c -- Fort.33
c --
      if(i33.eq.1) then
      write(33,*) '******  Output file for link speed         ******'
      write(33,*) '================================================='
      write(33,*) 'This file provides the average speed  '
      write(33,*) 'on links per sim. int. averaged over ',i33_t,
     *            'sim. int.'
      write(33,*)
      endif
c --
c -- Fort.34
c --
      if(i34.eq.1) then
      write(34,*) '******  Output file for link density      ******'
      write(34,*) '================================================='
      write(34,*) 'This file provides the average density  '
      write(34,*) 'on links per sim. int. averaged over',i34_t, 
     *            'sim. int.'
      write(34,*)
      endif
c --
c -- Fort.35
c --
      if(i35.eq.1) then
      write(35,*) 'Speed on the queue-free portion of the link'
      write(35,*) '================================================='
      write(35,*) 'This file provides the average speed on' 
      write(35,*) 'the queue-free portion on links every'
      write(35,*) 'sim. int. averaged over', i35_t, 'sim. int.'
      write(35,*)
      endif
c --
c -- Fort.36
c --
      if(i36.eq.1) then
      write(36,*) 'Density on the queue-free portion of the link'
      write(36,*) '================================================='
      write(36,*) 'This file provides the average density on'
      write(36,*) 'the queue-free portion on links per'
      write(36,*) 'sim. int. averaged over',i36_t,'sim. int.'
      write(36,*)
      endif
c --
c -- Fort.37
c --
      if(i37.eq.1) then
      write(37,*) 'Output file for left turning out flow'
      write(37,*) '================================================='
      write(37,*) 'This file provides the average number of'
      write(37,*) 'left turning vehicles on links per'
      write(37,*) 'sim. int. averaged over',i37_t, 'sim. int.'
      write(37,*)
      endif
c --
c -- Fort.38
c --
      if(i38.eq.1) then
      write(38,*) 'Output file for green time '
      write(38,*) '================================================='
      write(38,*) 'This file provides the average green time'
      write(38,*) 'for each link every per sim. int. averated over'
     *            ,i38_t, 'sim. int.'
      write(38,*)
      endif
c --
c -- Fort.39
c --
      if(i39.eq.1) then
      write(39,*) 'Output file for out flow '
      write(39,*) '================================================='
      write(39,*) 'This file provides the average number of vehicles'
      write(39,*) 'out of each link per sim. int. averaged over'
     *            ,i39_t, 'sims ints'
      write(39,*)
      endif

      if(i40.eq.1) then
      write(40,*) 'Output file for accumulated volume '
      write(40,*) '================================================='
      write(40,*) 'This file provides the accummulated number of veh.'
      write(40,*) 'on of each link ',i40_t, 'every sims ints'
      write(40,*)
      endif

      write(29,*) 'Output file for link volume '
      write(29,*) '================================================='
      write(29,*) 'This file provides the number of veh.'
      write(29,*) 'on of each link ','every 10 sims ints'
      write(29,*)


c --
      return
      end
