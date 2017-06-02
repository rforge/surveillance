/*******************************************************************************
 * Call polyCub_iso from polyCubAPI.h for a specific intrfr function
 *
 * Copyright (C) 2017 Sebastian Meyer
 *
 * This file is part of the R package "surveillance",
 * free software under the terms of the GNU General Public License, version 2,
 * a copy of which is available at http://www.R-project.org/Licenses/.
 ******************************************************************************/

#include "twinstim_siaf_intrfr.h"  // intrfr prototypes
#include <polyCubAPI.h>

// function to be called from R
void C_siaf_polyCub1_iso(
    double *x, double *y,  // vertex coordinates (open)
    int *L,                // number of vertices
    int *intrfr_code,      // F(R) identifier
    double *pars,          // parameters for F(R)
    int *subdivisions, double *epsabs, double *epsrel, // Rdqags options
    int *stop_on_error,
    double *value, double *abserr, int *neval) // results
{
    intrfr_fn intrfr;
    switch(*intrfr_code) { // = INTRFR_CODE in ../R/twinstim_siaf_polyCub_iso.R
    case 10: intrfr = intrfr_powerlaw; break;
    case 11: intrfr = intrfr_powerlaw_dlogsigma; break;
    case 12: intrfr = intrfr_powerlaw_dlogd; break;
    }
    double center_x = 0.0;
    double center_y = 0.0;
    polyCub_iso(x, y, L, intrfr, pars, &center_x, &center_y,
                subdivisions, epsabs, epsrel, stop_on_error,
                value, abserr, neval);
    return;
}
