/*******************************************************************************
 * C-implementation of "intrfr" functions for polyCub_iso from polyCubAPI.h
 *
 * Copyright (C) 2017 Sebastian Meyer
 *
 * This file is part of the R package "surveillance",
 * free software under the terms of the GNU General Public License, version 2,
 * a copy of which is available at http://www.R-project.org/Licenses/.
 ******************************************************************************/

#include <math.h>
#include <polyCubAPI.h>

// power-law kernel
static double intrfr_powerlaw(double R, double *logpars)
{
    double sigma = exp(logpars[0]);
    double d = exp(logpars[1]);
    if (d == 1.0) {
        return R - sigma * log(R/sigma + 1);
    } else if (d == 2.0) {
        return log(R/sigma + 1) - R/(R+sigma);
    } else {
        return (R*pow(R+sigma,1-d) - (pow(R+sigma,2-d) - pow(sigma,2-d))/(2-d)) / (1-d);
    }
}


// function to be called from R
void twinstim_siaf_polyCub_iso(
    double *x, double *y,  // vertex coordinates (open)
    int *L,                // number of vertices
    int *intrfr_code,      // F(R) identifier
    double *pars,          // parameters for F(R)
    int *subdivisions, double *epsabs, double *epsrel, // Rdqags options
    int *stop_on_error,
    double *value, double *abserr, int *neval) // results
{
    intrfr_fn intrfr;
    switch(*intrfr_code) {
    case 10: intrfr = intrfr_powerlaw;
        //case 11: intrfr = 
    }
    double center_x = 0.0;
    double center_y = 0.0;
    polyCub_iso(x, y, L, intrfr, pars, &center_x, &center_y,
                subdivisions, epsabs, epsrel, stop_on_error,
                value, abserr, neval);
    return;
}
