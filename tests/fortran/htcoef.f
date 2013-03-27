      program htcoef
c
c       John Mahaffy,  Penn State University, CmpSc 201 Example
c       1/26/96
c
      implicit none
      real k,D,Pr,h,Nulam,Nuturb
      real Re1,Re2,Re3,Re4
c
c    Calculate an approximation for heat transfer coefficients
c    in a 1 inch pipe for several different Reynolds numbers
c
c    An example of why you should learn to use subprograms
c
c   h    -  heat transfer coefficient ( w/m**2/K)'
c   Nulam - laminar Nusselt number
c   Nuturb - Turbulent Nusselt number (Dittus-Boelter correlation)
c   k   -  conductivity ( w/m/K)'
c   D   -  hydraulic diameter (m)
c   Re  -  Reynolds number
c   Pr  -  Prandl number
c
      data k,D,Pr/0.617,0.0254,1.0/, Nulam/4.0/
c
c    Each of the following blocks assigns a Reynolds number, calculates
c    an associated Turbulent Nusselt number and calculates the heat
c    transfer coefficient based on the maximum of Turbulent and Laminar
c    Nusselt Numbers
c
      Re1=10.
      Nuturb=0.023*Re1**0.8*Pr**0.4
      h=k/D*max(Nulam,Nuturb)
      print *, 'For Reynolds Number = ',Re1
      print *, 'Heat Transfer Coefficient is ',h,' w/m**2/K'
c
      Re2=100.
      Nuturb=0.023*Re2**0.8*Pr**0.4
      h=k/D*max(Nulam,Nuturb)
      print *, 'For Reynolds Number = ',Re2
      print *, 'Heat Transfer Coefficient is ',h,' w/m**2/K'
c
      Re3=1000.
      Nuturb=0.023*Re3**0.8*Pr**0.4
      h=k/D*max(Nulam,Nuturb)
      print *, 'For Reynolds Number = ',Re3
      print *, 'Heat Transfer Coefficient is ',h,' w/m**2/K'
c
      Re4=10000.
      Nuturb=0.023*Re4**0.8*Pr**0.4
      h=k/D*max(Nulam,Nuturb)
      print *, 'For Reynolds Number = ',Re4
      print *, 'Heat Transfer Coefficient is ',h,' w/m**2/K'
c
      stop
      end
