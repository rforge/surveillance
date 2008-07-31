//Assorted stuff to overwrite misbehaviour in GSL functions.

#include <math.h>
#include <gsl/gsl_sf_gamma.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>



//hoehle: The original function assumes mu>0, which needs not be the case!
//This version handles that part. This is the log version.

double
gsl_ran_poisson_log_pdf (const unsigned int k, const double mu)
{
  double p;
  if (mu==0) {
    return(log(k == 0));
  } else {
    double lf = gsl_sf_lnfact (k); 

    p = k*log(mu) - lf - mu;
    return p;
  }
}

/**********************************************************************
 * Log version of the Gamma pdf with mean a*b and variance a*b^2.
 *
 **********************************************************************/

double
gsl_ran_gamma_log_pdf (const double x, const double a, const double b)
{
  if (x < 0)
    {
      //This is problematic!
      return log(0) ;
    }
  else if (x == 0)
    {
      if (a == 1)
        return log(1/b) ;
      else
        return log(0) ;
    }
  else if (a == 1)
    {
      return -x/b - log(b) ;
    }
  else 
    {
      double p;
      double lngamma = gsl_sf_lngamma (a);
      p = (a-1)*log(x) - x/b - lngamma - a*log(b);
      return p;
    }
}

