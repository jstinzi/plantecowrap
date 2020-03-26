# plantecowrap

[![Build Status](https://travis-ci.com/jstinzi/plantecowrap.svg?branch=master)](https://travis-ci.com/jstinzi/plantecowrap)

{plantecowrap} Provides wrapping functions to add to capabilities to 'plantecophys' 
(Duursma, 2015, <doi:10.1371/journal.pone.0143346>). Key added capabilities 
include temperature responses of mesophyll conductance (gm, gmeso), apparent 
Michaelis-Menten constant for rubisco carboxylation in air (Km, Kcair),and
photorespiratory CO2 compensation point (GammaStar) for fitting A-Ci or A-Cc
curves for C3 plants (for temperature responses of gm, Km, & GammaStar,  see 
Bernacchi et al., 2002, <doi:10.1104/pp.008250>; for theory on fitting A-Ci 
or A-Cc curves, see Farquhar et al., 1980; <doi:10.1007/BF00386231>, von 
Caemmerer, 2000, ISBN:064306379X; Ethier & Livingston, 2004 
<doi:10.1111/j.1365-3040.2004.01140.x>; and Gu et al., 2010, 
<doi:10.1111/j.1365-3040.2010.02192.x>). Includes the ability to fit the 
Arrhenius and modified Arrhenius temperature response functions (see Medlyn 
et al., 2002, <doi:10.1046/j.1365-3040.2002.00891.x>) for maximum rubisco 
carboxylation rates (Vcmax) and maximum electron transport rates (Jmax) (see
Farquhar et al., 1980; <doi:10.1007/BF00386231>).

For information on how to use the code, please see the vignettes folder.
To download this package, run the following code in R:

devtools::install_github("jstinzi/plantecowrap")

Please direct any questions or comments to me at:
<josephstinziano@gmail.com>

Or you can log an issue on the package webpage at:
<https://github.com/jstinzi/plantecowrap/issues>.
