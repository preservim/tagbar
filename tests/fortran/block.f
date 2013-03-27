      block data
         integer nmax
         parameter (nmax=20)
         real v(nmax), alpha, beta
         common /vector/v,alpha,beta
         data v/20*100.0/, alpha/3.14/, beta/2.71/
      end
