/*******************************************************************************
 * C-implementation of "intrfr" functions used in C_siaf_polyCub1_iso
 *
 * Copyright (C) 2017 Sebastian Meyer
 *
 * This file is part of the R package "surveillance",
 * free software under the terms of the GNU General Public License, version 2,
 * a copy of which is available at http://www.R-project.org/Licenses/.
 ******************************************************************************/

#include "twinstim_siaf_intrfr.h"  // function prototypes
#include <math.h>

// power-law kernel
double intrfr_powerlaw(double R, double *logpars)
{
    double sigma = exp(logpars[0]);
    double d = exp(logpars[1]);
    if (d == 1.0) {
        return R - sigma * log1p(R/sigma);
    } else if (d == 2.0) {
        return log1p(R/sigma) - R/(R+sigma);
    } else {
        return (R*pow(R+sigma,1.0-d) - (pow(R+sigma,2.0-d) - pow(sigma,2.0-d))/(2.0-d)) / (1.0-d);
    }
}

double intrfr_powerlaw_dlogsigma(double R, double *logpars)
{
    double newlogpars[2] = {logpars[0], log1p(exp(logpars[1]))};
    // sigma*d = exp(logsigma+logd)
    return -exp(logpars[0]+logpars[1]) * intrfr_powerlaw(R, newlogpars);
}

double intrfr_powerlaw_dlogd(double R, double *logpars)
{
    double sigma = exp(logpars[0]);
    double d = exp(logpars[1]);
    if (d == 1.0) {
        return sigma * logpars[0] * (1.0-logpars[0]/2.0) - log(R+sigma) * (R+sigma) +
            sigma/2.0 * pow(log(R+sigma),2.0) + R;
    } else if (d == 2.0) {
        return (-log(R+sigma) * ((R+sigma)*log(R+sigma) + 2.0*sigma) +
                (R+sigma)*logpars[0]*(logpars[0]+2.0) + 2.0*R) / (R+sigma);
    } else {
        return (pow(sigma,2.0-d) * (logpars[0]*(-d*d + 3.0*d - 2.0) - 2.0*d + 3.0) +
                pow(R+sigma,1.0-d) * (log(R+sigma)*(d-1.0)*(d-2.0) * (R*(d-1.0) + sigma) +
                                      R*(d*d+1.0) + 2.0*d*(sigma-R) - 3.0*sigma)
                ) * d / pow(d-1.0,2.0) / pow(d-2.0,2.0);
    }
}
