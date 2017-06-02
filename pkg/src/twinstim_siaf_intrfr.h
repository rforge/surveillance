/*******************************************************************************
 * Header file of twinstim_siaf_intrfr.c with prototypes of "intrfr" functions
 *
 * Copyright (C) 2017 Sebastian Meyer
 *
 * This file is part of the R package "surveillance",
 * free software under the terms of the GNU General Public License, version 2,
 * a copy of which is available at http://www.R-project.org/Licenses/.
 ******************************************************************************/

// power-law kernel
double intrfr_powerlaw(double R, double *logpars);
double intrfr_powerlaw_dlogsigma(double R, double *logpars);
double intrfr_powerlaw_dlogd(double R, double *logpars);
