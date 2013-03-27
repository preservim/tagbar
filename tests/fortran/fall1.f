c<html>
c<body>
c<pre>
      module constants
         integer, parameter :: np=2000, dbl=selected_real_kind(14,100)
         real(dbl) :: g=9.807,dtmin=.001
      end module constants
c
      program fall
      use constants
      implicit none
c
c   Program to calculate the dynamics of a falling body
c
c   John Mahaffy    4/15/95
c
c   Arrays:
c     v   -   velocity at each time integration step
c     z   -   height at  each time integration step
c     t   -   time for each corresponding v and z
c     zreal - Actual height at time t(i) for comparison with computed z
c
c   In this program, I am using allocatate just to save space in the
c   executable program file (a.out).  No attempt is made to estimate a size.
c   Module "constants" communicates between subroutines.
c
c<a name="alable"><font color="FF0000">
      real(dbl), allocatable :: v(:),z(:),t(:), zreal(:)
c</font></a>
      real(dbl) dt
      integer nsteps
c<a name="all"><font color="FF0000">
      allocate (v(np),z(np),t(np),zreal(np))
c</font></a>
      call input(z,dt)
      call odesolve(v,z,t,dt,nsteps)
      call realans(t,z,nsteps,zreal)
      call output (t,z,zreal,v,nsteps)
      stop
      end
c
      subroutine input (z,dt)
      use constants
      implicit none
c
c   Obtain user input for initial height and time step
c
c   John Mahaffy    4/15/95
c
c  Output Arguments:
c     z(1)   -  initial height
c     dt     -  integration time step
c
      real(dbl) z(*),dt
c
      write(6,'(a)',advance='no') ' Initial height (meters): '
      read *, z(1)
      write(6,'(a)',advance='no') 'Time step size (seconds): '
      read *, dt
      if(dt.le.0.) dt=dtmin
      return
      end
c
      subroutine odesolve(v,z,t,dt,nsteps)
      use constants
c
c   Solve the Ordinary Differential Equation of motion for the body
c
c   John Mahaffy    4/15/95
c
c   Arguments:
c   Input
c     dt     -   timestep size
c   Output:
c     v    -   velocity
c     z    -   height
c     t    -   time
c     nsteps - last step in the integration
c
      real (dbl) v(*),z(*),t(*),dt
      integer i, nsteps
c
c   Solve the equation for a falling body
c
c     d v                    d z
c     ---    =  - g          ---   =  v
c     d t                    d t
c
c   Set remaining initial conditions:
c
      t(1)=0.
      v(1)=0.
c
c     Now loop through time steps until z goes negative or we run out of space
c
      do 100 i=2,np
         v(i)= v(i-1)-dt*g
         z(i)= z(i-1)+dt*.5*(v(i)+v(i-1))
         t(i)=t(i-1)+dt
         if(z(i).lt.0.) go to 200
c<a name="con"><font color="FF0000">
 100     continue
c</font></a>
      write(6,*) 'Ran out of space to continue integration'
      write(6,*) ' Last height was ',z(np),' meters'
      i=np
 200  nsteps=i
c     return
      end
c
      subroutine realans(t,z,nsteps,zreal)
      use constants
c
c     Get the values of the analytic solution to the differential equation
c     for each time point to check the numerical accuracy.
c
c   John Mahaffy    4/15/95
c
      real(dbl) t(*),z(*),zreal(*)
      integer i,nsteps
c
      do 10 i=1,nsteps
  10  zreal(i)=z(1)-.5*g*t(i)**2
      return
      end
c
      subroutine output(t,z,zreal,v,nsteps)
      use constants, only : dbl
      implicit none
c
c   Outputs the full results of the time integration
c
c   John Mahaffy    4/15/95
c
      real(dbl) v(*),z(*),t(*), zreal(*)
      integer nsteps,i
      print *, 'Results are in fall.output'
      open (11,file='fall.output')
      write(11,2000)
      do 300 i=1,nsteps
      write(11,2001) t(i),v(i),z(i),zreal(i)
 300  continue
 2000 format (33x,'Computed',8x,'Actual',/,
     &        6x,'Time',9x,'Velocity', 8x,'Height',8x,'Height')
 2001 format (1x,1p,4e15.7)
      return
      end
c</pre>
c</body>
c</html>
