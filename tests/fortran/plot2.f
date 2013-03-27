c<html>
c<head><title>plot2.f</title></head>
c<body>
c<pre>

      program plotit
c
c    Program to provide plots of Sin(x)
c    Ascii Character plots go to terminal and file 'pplot.out'
c
c     John Mahaffy   2/1/95
c
      implicit none
c
c    The following line tells Fortran that "func" is a function or subroutine
c    and not a variable
c
c<a name="ex"><font color="FF0000">
      external func
c</font></a>
c
c   The next line is necessary to pass the location of the intrinsic sin
c   function as an argument to a subprogram
c
c<a name="intrinsic"><font color="FF0000">
      intrinsic sin
c</font></a>
c
      call pplot(func,0.,6.)
      print *, 'Hit the Enter key to continue'
c<a name="pause"><font color="FF0000">
      pause
c</font></a>
      call gnuplot('x','y','sin(x)',func,0.0,6.0)
      call gnuplot('x','y','Intrinsic sin(x)',sin,0.0,6.0)
      stop
      end
      subroutine pplot(f,xmin,xmax)
c
c   Generate ASCII character plot to screen and file 'pplot.out'
c
      implicit none
      character line*72
      real x, f , xmin , xmax
      integer ip,i,imax
c
c    The following line tells Fortran that f is a function or subroutine
c    and not a variable
c
      external f
c
c   INPUT Arguments
c
c   f       -   function to be ploted
c   xmin    -   minimum x value
c   xmax    -   maximum x value
c
c   OTHER key variables
c
c   line    -   Character string loaded with a line of output
c   ip      -   Position in line for a function value
c
      open (11,file='pplot.out')
c
c   Label values of the y axis
c
      line=' '
      line(14:15)='-1'
      line(65:65)='1'
      write(*,*) line
      write(11,*) line
      line=' '
      write(line(10:13),'(f4.1)') xmin
c
c   Draw the y axis
c
      line(15:40)='+----+----+----+----+----+'
      line(41:65)=line(16:40)
c
c   Plot the value at x=0
c
      ip= nint(25*f(0.0))+40
      line(ip:ip)='*'
      write(*,*) line
      write(11,*) line
      line=' '
      imax=nint((xmax-xmin)*10)
c
c     Limit output
c
      imax=min(1000,imax)
c
c    Loop through and plot points for other x values
c
      do 50 i=1,imax
         x=.1*i
         ip=nint(25*f(x))+40
c 
         if(mod(i,10).eq.0) then
            write(line(10:13),'(f4.1)') x
            line(40:40)='+'
c<a name="else"><font color="FF0000">
         else
c</font></a>
            line(10:13)=' '
            line(40:40)='|'
         endif
         line(ip:ip)='*'
         write(*,*) line
         write(11,*) line
 50      line(ip:ip)=' '
c<a name="close"><font color="FF0000">
      close (11)
c</font></a>
      return
      end
      subroutine gnuplot(xlabel,ylabel,title,f,xmin,xmax)
c
c   Ship data to the public domain program "gnuplot" for plotting
c
      implicit none
      character line*72,sq*1
      real x,f,xmin,xmax,fx
      character*(*) xlabel,ylabel,title
      integer i,imax,lc
      external f
      data sq/''''/
c
c   INPUT Arguments
c
c   f       -   function to be ploted
c   xmin    -   minimum x value
c   xmax    -   maximum x value
c   xlabel  -   Contains a label for the x-axis
c   ylabel  -   Contains a label for the y-axis
c   title   -   Contains a title for the plot
c
c   OTHER key variables
c
c   line   -   Contains a line of character data
c
c   Drive a separate true graphics program (gnuplot)
c
c   First set up the command file for gnuplot
c   Run gnuplot interactively and use the "help" command to learn more
c   about what I am doing.
c
      open (12,file='gnuxy')
c
c  UnComment the next line if you are on a NCSA/BYU Telnet Session
c
c     write(12,*) 'set terminal tek40xx'
c
      write(12,*) 'set data style lines'
c     <a name=1><font color=FF0000>
      lc=len(xlabel)
c     </font>
      line='set xlabel '''//xlabel(1:lc)//sq
      write(12,*)line
c
c   You don't really need to calculate the character variable length
c   here.  The following works just fine because of the character*(*)
c
      line='set ylabel '''//ylabel//sq
      write(12,*)line
      line='set title '''//title//sq
      write(12,*)line
      write(12,*)'set nokey'
      write(12,2000) xmin,xmax
 2000 format('set xrange [',f3.0,':',f3.0,']')
      write(12,*) 'plot ''dataxy'' using 1:2'
      write(12,*) 'pause 10'
      close(12)
c
c   Generate x-y pairs for the graph
c
      open (12,file='dataxy')
      imax=nint((xmax-xmin)*10)
c
c     Limit output
c
      imax=min(1000,imax)
c
      do 100 i=0,imax
         x=.1*i
         fx=f(x)
         write(12,*) x,fx
  100 continue
      close(12)
c
c   Tell the system to run the program gnuplot
c   This call works on either IBM RS6000 or Sun, but is not part of
c   the Fortran standard.
c   Comment out the line if you aren't at a terminal with graphics
c
      call system('gnuplot gnuxy')
c<a name="10"><font color="FF0000">
      return
c</a></font>
      end
c<a name="fun"><font color="FF0000">
      real function func(x)
c</font></a>
      func=sin(x)
      return
      end
c</pre>
c</body>
c</html>
