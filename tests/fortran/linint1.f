       subroutine linint(x,y)
c
c     Given a value of x return a value of y based on interpolation
c     within a table of y values (ytab) evaluated at evenly spaced
c     x points between xmin and xmax.
c
c    John Mahaffy 2/12/95
c
       real ytab(11)
       parameter (xmin=300.,dx=100.,xmax=xmin+10.*dx,rdx=1./dx)
       data ytab/1.0,2.1,3.2,4.4,5.7,7.1,8.6,10.2,11.9,13.7,15.8/
       if(x.ge.xmin.and.x.le.xmax) then
               i1= int((x-xmin)*rdx)+1            
               x1=xmin+(i1-1)*dx
               wx=(x-x1)*rdx
               y=(1-wx)*ytab(i1)+wx*ytab(i1+1)
       else
               write(6,*) 'x = ', x, '  is out of table range' 
               stop
       endif
       return
       end
