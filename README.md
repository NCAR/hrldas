# hrldas

HRLDAS (High Resolution Land Data Assimilation System) contains the Noah-MP Land Surface Model and can
be executed in offline uncoupled mode for a 2-D gridded domain or a single point. 

As of May 6, 2020, this repository does not explicitly contain the Noah-MP code and parameters, but
connects to the Noah-MP repository (https://github.com/NCAR/noahmp) through git submodules. To clone
this repository and populate with the Noah-MP code/parameters, use the following command:

git clone --recurse-submodules https://github.com/NCAR/hrldas


This is an official HRLDAS/NoahMP code version consistent with the WRF-v4.3 release. This new version includes the following major updates:

(1) Snow-related update:

Add wet-bulb temperature snow-rain partitioning scheme (OPT_SNF=5) based on Wang et al. 2019 (NWM)

Add snow retention process at the snowpack bottom to improve streamflow modeling (NWM)

Modify wind-canopy absorption coefficient (CWPVT) parameter values in MPTABLE to be vegetation dependent based on Goudriaan1977

Bring hard-coded snow emissivity and parameter (2.5*z0) in snow cover formulation to tunable MPTABLE parameters

Update MFSNO in snow cover formulation with optimized vegetation-dependent values

Limit the bulk leaf boundary layer resistance (RB) to a more realistic range (5~50)

(2) New irrigation scheme:

multiple irrigation methods: sprinkler, micro, and surface flooding

(3) Crop scheme update:

separate the original generic crop physiology parameters in the modis vegetation section into C3/C4 specific parameters in the crop section

(4) New urban physics working with Noah-MP:

Local climate zone (LCZ), solar panel, green roof, new building drag parameterization


Updated on April 28, 2021
