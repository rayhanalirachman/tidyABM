#include <stdio.h>
#include <math.h>
#include <stdint.h>
extern double Rf_qnorm5(double, double, double, int, int);
#define F fma
// explicit-fma version of the qnorm5 central and r<=5 branches (what Rust f64::mul_add would do)
static double q_fma(double p){
  double q=p-0.5,r,num,den;
  if(fabs(q)<=.425){
    r=F(-q,q,.180625);
    num=F(F(F(F(F(F(F(r,2509.0809287301226727,33430.575583588128105),r,67265.770927008700853),r,45921.953931549871457),r,13731.693765509461125),r,1971.5909503065514427),r,133.14166789178437745),r,3.387132872796366608);
    den=F(F(F(F(F(F(F(r,5226.495278852854561,28729.085735721942674),r,39307.89580009271061),r,21213.794301586595867),r,5394.1960214247511077),r,687.1870074920579083),r,42.313330701600911252),r,1.);
    return q*num/den;
  }
  r=sqrt(-log(q>0?1-p:p)); if(r>5.) return NAN; r+=-1.6;
  num=F(F(F(F(F(F(F(r,7.7454501427834140764e-4,.0227238449892691845833),r,.24178072517745061177),r,1.27045825245236838258),r,3.64784832476320460504),r,5.7694972214606914055),r,4.6303378461565452959),r,1.42343711074968357734);
  den=F(F(F(F(F(F(F(r,1.05075007164441684324e-9,5.475938084995344946e-4),r,.0151986665636164571966),r,.14810397642748007459),r,.68976733498510000455),r,1.6763848301838038494),r,2.05319162663775882187),r,1.);
  return q<0?-num/den:num/den;
}
int main(){ uint64_t s=88172645463325252ULL; long d=0,n=10000000,maxulp=0; 
  for(long i=0;i<n;i++){ s^=s<<13; s^=s>>7; s^=s<<17; double p=(s>>11)*0x1.0p-53; if(p<=0) continue;
    double a=q_fma(p), b=Rf_qnorm5(p,0,1,1,0); if(isnan(a)) continue; if(a!=b) d++; }
  printf("explicit fma() vs libR: %ld of %ld differ\n",d,n); return 0; }
