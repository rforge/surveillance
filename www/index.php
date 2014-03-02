
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
	<style type="text/css">
	  dt { margin-top:1em; }
	</style>
  </head>

<body>

<! --- R-Forge Logo --- >
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="/"><img src="<?php echo $themeroot; ?>/imagesrf/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->

<hr />
<h3>Description</h3>

<dl>

  <dt>Intention:</dt>
  <dd>To provide open source software for the temporal and spatio-temporal
    visualization, modelling and monitoring of epidemic phenomena. This includes
    count, binary and categorical data time series as well as continuous-time
    processes having discrete or continuous spatial resolution.
  </dd>

  <dt>Potential users:</dt>
  <dd>Biostatisticians, epidemiologists and others working in, e.g., applied
    infectious disease epidemiology. However, applications could just as well
    originate from environmetrics, reliability engineering, econometrics or
    social sciences.
  </dd>

  <dt>Main applications:</dt>
    <ul>
      <li>Prospective detection of aberrations in routinely collected public
	health data seen as univariate and multivariate time series of counts.</li>
      <li>General regression modelling of spatio-temporal epidemic phenomena (retrospectively).</li>
    </ul>

  <dt>License:</dt>
  <dd>This program is free software, and you are welcome to redistribute it
    under the terms of
    the <a href="http://www.gnu.org/licenses/gpl-2.0.html">GNU General Public
    License, version 2</a>.<br/>
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY.
  </dd>

</dl>


<hr />
<h3>Prospective Outbreak Detection</h3>

<ul>

  <li><tt>surveillance</tt> provides the <tt>S4</tt> class data
    structure <tt>"sts"</tt> and a framework for methodological developments
    of change-point algorithms for time series of counts.</li>

  <li>Prospective outbreak detection procedures for count data time series:
    <ul>
      <li><code>cdc</code> - Stroup et al. (1989)</li>
      <li><code>farrington</code> - Farrington et al. (1996)</li>
      <li><code>farringtonFlexible</code> - Improved Farrington algorithm of Noufaily et al. (2012)</li>
      <li><code>rki</code> - The system previously used at the Robert Koch
	Institute, Germany </li>
      <li><code>bayes</code> - A Bayesian predictive posterior
	approach, see Höhle (2007)</li>
      <li><code>boda</code> - Bayesian outbreak detection algorithm based on a Generalized Additive Model fitted with INLA, see Manitz and Höhle (2013)</li>
      <li><code>hmm</code> - A predictive version of the Hidden Markov
	Model approach by Le Strat and Carrat (1999)</li>
      <li><code>rogerson</code> - Surveillance for time varying
	Poisson means as documented in Rogerson and Yamada (2004).<br/>
	This approach has been extended to
	cover time varying proportions in a binomial setting.</li>
      <li><code>cusum</code> - An approximate CUSUM method for time
	varying Poisson means
	as documented in Rossi et al (1999)</li>
      <li><code>glrnb</code> - Likelihood and generalized likelihood
	ratio detectors for time varying
	Poisson and negative binomial distributed series documented in Höhle
	and Paul (2008).</li>
      <li><code>outbreakP</code> - Semiparametric surveillance of
	outbreaks by Frisén and Andersson (2009)</li>
    </ul>
  </li>
    
  <li>Online change-point detection in categorical time series:
    <ul>
      <li><code>categoricalCUSUM</code> - includes change-point
	detection based on regression models for binomial and beta-binomial
	distributed response. Furthermore, multi-categorical models includes
	the multinomial logistic model, proportional odds model and the
	Bradley-Terry models, see Höhle (2010)</a>.</li>
      <li><code>pairedbinCUSUM</code> - paired-binary approach taken
	in Steiner et al. (1999)</li>
    </ul>
  </li>

  <li>Prospective space-time cluster detection:
    <ul>
      <li><code>stcd</code> - (experimental) Point process based
	approach by Assuncao &amp; Correa (2009)</li>
    </ul>
  </li>

  <li>For evaluation purposes, the package contains example
    datasets drawn
    from the SurvStat@RKI database maintained by the RKI, Germany. More
    comprehensive comparisons using simulation studies are possible by
    methods for simulating point source outbreak data using a hidden Markov
    model. To compare the algorithms, benchmark numbers like sensitivity,
    specificity and detection delay can be computed for entire sets of
    surveillance time series. Furthermore, a Markov Chain approximation for
    computing the run-length distribution of the proposed likelihood ratio
    CUSUMs is available as function <code>LRCUSUM.runlength.</code></li>

</ul>



<hr />
<h3>Spatio-Temporal Regression Frameworks for the Retrospective Modelling of Epidemic Phenomena</h3>


<ul>

  <li>For univariate and multivariate count data time series:
    <ul>
      <li><code>algo.hhh</code> - Held et al. (2005) and Paul et al. (2008)</li>
      <li><code>hhh4</code> - Paul and Held (2011)</li>
      <li><code>algo.twins</code> - Held et al. (2006)</li>
    </ul>
  </li>

  <li>Continuous-time point process modelling:
    <ul>
      <li><code>twinSIR</code> - continuous-time/discrete-space modelling as
	described in H&ouml;hle (2009). The <code>"epidata"</code> class
	provides the appropriate data structure for such data.</li>
      <li><code>twinstim</code> - continuous-time/continuous-space modelling as described in
	Meyer et al. (2012).
	The <code>"epidataCS"</code> class describes the data, which mainly consist
	of the observed events and exogenous covariates on a space-time lattice.</li>
    </ul>
  </li>

</ul>

<hr/>
<h3>Modelling Structural Delays and Reporting Delays in Surveillance Data</h3>
<ul>
  <li>Backprojection methods
    <ul>
      <li><code>backprojNP</code> - Non-parametric back-projection method of Becker et al. (1991) used in, e.g., Werber et al. (2013).</li>
    </ul>
  </li>
</ul>


<hr />
<h3>Download</h3>


<blockquote>
The <tt>surveillance</tt> package is available for download from <a
 href="http://CRAN.R-project.org/package=surveillance">CRAN</a>.
</blockquote>

<blockquote>
Current package development, help-forum and bugtracking is hosted
through R-Forge:<br/><br/>
<div style="text-align: center;">
  <a href="https://r-forge.r-project.org/projects/surveillance/">https://r-forge.r-project.org/projects/surveillance/</a><br/>
</div><br/>
From this page, <a href="https://r-forge.r-project.org/R/?group_id=45">snapshots
  of the current development version</a> are available for download as a source
tarball and a Windows binary.<br/>
You can easily install the current snapshot in <tt>R</tt> via<br><br>

<div style="text-align: center;">
<tt>install.packages("surveillance",repos="http://r-forge.r-project.org")</tt>.
</div><br>
Currently, R-Forge does not offer binaries for MacOS X, but installation might
succeed with the additional argument <tt>type="source"</tt> in the above call.
</blockquote>


<hr />
<h3>News</h3>

<ul>
  <li>See
    the <a href="http://cran.r-project.org/web/packages/surveillance/news.html">NEWS</a>
    of the latest version released on CRAN or 
    the <a href="https://r-forge.r-project.org/scm/viewvc.php/pkg/inst/NEWS.Rd?view=markup&root=surveillance">NEWS file</a> 
    of the current development version.
  </li>
  <li>2013/04/23 <a href="StockholmR-Hoehle_4.pdf">Talk</a> at the  Stockholm R useR group (StockholmR) on <a href="http://www.meetup.com/StockholmR/events/105738342/">Making R packages (and) Shiny</a>.</li>
</ul>

  
<hr/>
<h3>Documentation</h3>


<blockquote>
  <blockquote>
    <ul>
      <li>A good (but slightly outdated) introduction to the outbreak detection part of the package is
provided in the paper <a
 href="http://dx.doi.org/10.1007/s00180-007-0074-8"><span
 style="font-family: monospace;">surveillance</span>: An R package for
the surveillance of infectious diseases</a>, Computational Statistics
(2007), 22(4), pp. 571-582. <a
 href="http://www.stat.uni-muenchen.de/%7Ehoehle/pubs/hoehle-CoSt2008-preprint.pdf">
[preprint]</a></li>
      <li>A more recent description can be found in the book chapter <i>Aberration
detection in R illustrated by Danish mortality monitoring</i> (2010),
M. Höhle and A. Mazick, To appear in T. Kass-Hout and X. Zhang (Eds.)
Biosurveillance: A Health Protection Priority, CRC Press. [<a
							   href="http://www.stat.uni-muenchen.de/%7Ehoehle/pubs/hoehle_mazick2009-preprint.pdf">preprint</a>]. Note: As ISO 8601 handling is not fully implemented in R on Windows the demo("biosurvbook") will only run with package version >= 1.2, where a workaround was implemented.</li>
      <li>An overview of statistical methods and implementational usage
is given the course notes of several courses on the package, e.g.
the course notes
of the lecture
<a href="http://www.statistik.lmu.de/~hoehle/teaching/moid2011/moid.pdf">
Temporal and spatio-temporal modelling of infectious
diseases
</a>
at the Department of Statistics, University of Munich, Oct 10-13, 2011 or
the shortcourse  <a
 href="http://www.stat.uni-muenchen.de/%7Ehoehle/surv-short/index.html">Statistical
surveillance of infectious diseases</a> held at the Department of
Statistics, Universidade Federal de Minas Gerais (UFMG), Belo
Horizonte, Brazil, Nov 27-28, 2008. 
</li>
      <li><a href="hoehle-surveillance.pdf">Invited talk</a> held at
the 2008 ESCAIDE satellite workshop on <ii>Computer supported outbreak
detection and signal management</ii> (<a href="hoehle-surveillance.R">R-File</a>,
        <a href="ha.csv">Data</a> from SurvStat@RKI)
      </li>
      <li>Application of the package in veterinary public health
surveillance is described in <a
 href="http://dx.doi.org/10.1016/j.prevetmed.2009.05.017">Statistical
approaches to the surveillance of infectious diseases for veterinary
public health</a> [<a href="http://epub.ub.uni-muenchen.de/2093/">preprint</a>].
      </li>
      <li>Read the package vignettes or look <a
 href="http://www.stat.uni-muenchen.de/%7Ehoehle/pubs/">here</a> for
further preprints.<br>
      </li>
      <li>Sometimes pictures says more than 1000 words:</li>
      <p>
      <code>algo.farrington</code> + <code>algo.glrnb</code> + <code>nowcast</code><p>
      <img src="detectandnowcast.png" align="middle"><p>
      <code>backprojNP</code><p>
      <img src="backproj.png" align="middle"><p>
      <code>twinSIR</code><p>
<!--  convert -density 100x100 mpbb-intensity.pdf mpbb-intensity.png -->
      <img src="mpbb-intensity.png" align="middle"><p>
    </ul>
  </blockquote>
</blockquote>



<hr/>
<h3>Developers</h3>

<blockquote>
    <ul>
      <li><a href="http://www.math.su.se/%7Ehoehle">Michael
Höhle</a>, Department of Mathematics, Stockholm University, Sweden
(Project Admin)</li>
      <li><a href="http://www.biostat.uzh.ch/aboutus/people/smeyer.html">Sebastian
Meyer</a>, Institute of Social and Preventive Medicine, University of 
Zurich, Switzerland</li> 
      <li>Michaela Paul (previously at the Institute of Social and Preventive
	Medicine, University of Zurich, Switzerland)</li>
      <li><a href="http://www.normalesup.org/~masalmon/">Ma&eumllle Salmon</a>,
	Department for Infectious Disease Epidemiology, Robert Koch Institute,
	Germany</li>

      <li>Former student programmers: C. Lang, A. Riebler, V. Wimmer</li>

      <li>Contributions by: L. Held, T. Correa, M. Hofmann, J. Manitz, 
	D. Sabanés Bové, D. Schumacher, S. Steiner, M. Virtanen</li>
    </ul>
  </blockquote>


<hr/>
<h3>Financial Support</h3>

<blockquote>
    <ul>
      <li>German Science Foundation (DFG, 2003-2006)</li>
      <li><a href="http://www.en.mc-health.uni-muenchen.de/index.html">Munich
Center of Health Sciences</a> (MC-Health, 2007-2010)</li>
      <li>Swiss National Science Foundation (SNSF, 2007-2010, projects <a href="http://p3.snf.ch/Project-116776">#116776</a> and <a href="http://p3.snf.ch/Project-124429">#124429</a>)</li>
      <li>Swiss National Science Foundation (SNSF, 2012-2015, project <a href="http://p3.snf.ch/Project-137919">#137919</a>)</li>
    </ul>
</blockquote>


<hr/>
<h3>References</h3>

<dl>
<dt></dt>
<dd><a href="http://dx.doi.org/10.1191/1471082X05st098oa">Held, L., Höhle, M. and Hofmann, M. (<b>2005</b>)</a></dd>
<dd><i>A statistical framework for the analysis of multivariate infectious disease surveillance counts</i></dd>
<dd>Statistical Modelling, Vol. 5(3), pp. 187-199</dd>

<dt></dt>
<dd><a href = "http://dx.doi.org/10.1093/biostatistics/kxj016">Held, L., Hofmann, M., Höhle, M. and Schmid, V. (<b>2006</b>)</a></dd>
<dd><i>A two-component model for counts of infectious diseases</i></dd>
<dd>Biostatistics, Vol. 7(3), pp. 422-437</dd>

<dt></dt>
<dd><a href="http://www.stat.uni-muenchen.de/%7Ehoehle/pubs/hoehle2010-preprint.pdf">Höhle, M. (<b>2010</b>)</a></dd>
<dd><i>Online Change-Point Detection in Categorical Time Series</i></dd>
<dd>In: Statistical Modelling and Regression Structures (Kneib, T. &amp; Tutz, G., <i>eds.</i>)</dd>
<dd>Physica-Verlag HD, pp. 377-397</dd>

<dt></dt>
<dd><a href="http://dx.doi.org/10.1002/bimj.200900050">Höhle, M. (<b>2009</b>)</a></dd>
<dd><i>Additive-multiplicative regression models for spatio-temporal epidemics</i></dd>
<dd>Biometrical Journal, Vol. 51(6), pp. 961-978</dd>

<dt></dt>
<dd><a href="http://dx.doi.org/10.1016/j.csda.2008.02.015">Höhle, M. and Paul, M. (<b>2008</b>)</a></dd>
<dd><i>Count data regression charts for the monitoring of surveillance time series</i></dd>
<dd>Computational Statistics and Data Analysis, Vol. 52(9), pp. 4357-4368</dd>

<dt></dt>
<dd><a href="http://dx.doi.org/10.1007/s00180-007-0074-8">Höhle, M. (<b>2007</b>)</a></dd>
<dd><i><tt>surveillance</tt>: An R package for the monitoring of infectious diseases</i></dd>
<dd>Computational Statistics, Vol. 22(4), pp. 571-582</dd>

<dt></dt>
<dd><a href="http://dx.doi.org/10.1002/bimj.201200141">Manitz, J. and Höhle, M. (<b>2013</b>)</a></dd>
<dd><i>Bayesian outbreak detection algorithm for monitoring reported cases of campylobacteriosis in Germany</i></dd>
<dd>Biometrical Journal, Vol. 55(4), pp. 509-526</dd>

<dt></dt>
<dd><a href="http://dx.doi.org/10.1111/j.1541-0420.2011.01684.x">Meyer, S., Elias, J. and Höhle, M. (<b>2012</b>)</a></dd>
<dd><i>A space-time conditional intensity model for invasive meningococcal disease occurrence</i></dd>
<dd>Biometrics, Vol. 68(2), pp. 607-616</dd>

<dt></dt>
<dd><a href="http://arxiv.org/abs/1308.5115">Meyer, S. and Held, L. (<b>2014</b>)</a></dd>
<dd><i>Power-law models for infectious disease spread</i></dd>
<dd>Revised for the Annals of Applied Statistics, available as arXiv:1308.5115</dd>

<dt></dt>
<dd><a href="http://dx.doi.org/10.1002/sim.4177">Paul, M. and Held, L. (<b>2011</b>)</a></dd>
<dd><i>Predictive assessment of a non-linear random effects model for multivariate time series of infectious disease counts</i></dd>
<dd>Statistics in Medicine, Vol. 30(10), pp. 1118-1136</dd>

<dt></dt>
<dd><a href="http://dx.doi.org/10.1002/sim.3440">Paul, M., Held, L. and Toschke, A. M. (<b>2008</b>)</a></dd>
<dd><i>Multivariate modelling of infectious disease surveillance data</i></dd>
<dd>Statistics in Medicine, Vol. 27(29), pp. 6250-6267</dd>

<dt></dt>
<dd><a href="http://dx.doi.org/10.1093/aje/kwt069">Werber, D., King, L.A., Müller, L., Follin, P., Buchholz, U., Bernard, H., Rosner, B.M., Ethelberg, S., de Valk, H., Höhle, M. (<b>2013</b>)</a></dd>
<dd><i>Associations of Age and Sex on Clinical Outcome and Incubation Period of Shiga toxin-producing Escherichia coli O104:H4 Infections, 2011</i></dd>
<dd>American Journal of Epidemiology, Vol. 178(6):984-992</dd>

</dl>


</body>
</html>
