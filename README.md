# plantecowrap

[![Build Status](https://travis-ci.com/jstinzi/plantecowrap.svg?branch=master)](https://travis-ci.com/jstinzi/plantecowrap)

{plantecowrap} provides a series of functions to enhance the utility of
{plantecophys} by Duursma (2015). Specifically, it includes easy support
for using customized temperature response functions of mesophyll
conductance (gm), photorespiratory CO2 compensation points (GammaStar),
and Michaelis-Menten kinetics of rubisco (Km in 21% O2, Kcair, apparent
Km in 21% O2). As well, the package contains a series of functions to fit 
temperature responses of Vcmax and Jmax using the modified Arrhenius model 
containing Topt as found in Medlyn et al. (2002).

For information on how to use the code, please see the vignettes folder.
To download this package, run the following code in R:

devtools::install_github("jstinzi/plantecowrap")

Please direct any questions or comments to me at:
josephstinziano@gmail.com

Or you can log an issue on the package webpage.

References

Duursma R. 2015. Plantecophys - an R package for analysing and
modelling leaf gas exchange data. PLoS ONE 10:e0143346

Medlyn BE, Dreyer E, Ellsworth D, Forstreuter M, Harley PC,
Kirschbaum MUF, Le Roux X, Montpied P, Strassemeyer J, Walcroft A,
Wang K, Loutstau D. 2002. Temperature response of parameters of a
biochemically based model of photosynthesis. II. A review of
experimental data. Plant Cell Environ 25:1167-1179
