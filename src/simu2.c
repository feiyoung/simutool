/*some useful header file from R*/
  #include <R.h>
  #include <Rinternals.h>
  #include <Rmath.h>
  #include <stdio.h>
  void rmixnorm(int *n, double *P,int *d, double *mu, double *sigma, double *result)
{
  double Ps[100], U[5000];
  int i, j;
  double tmp;
  Ps[0] = 0;
  for(i=0; i<*d; i++){
    Ps[i+1] = Ps[i]+P[i];
  }
  for(i=0; i<*n;i++){
    U[i] = runif(0,1);
    for(j=0; j<*d; j++){
      if(U[i] >= Ps[j] && U[i]<=Ps[j+1]){
        result[i] = rnorm(mu[j], sigma[j]);
      }
    }
  }
  }
