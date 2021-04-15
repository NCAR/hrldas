# hrldas

HRLDAS (High Resolution Land Data Assimilation System) contains the Noah-MP Land Surface Model and can
be executed in offline uncoupled mode for a 2-D gridded domain or a single point. 

As of May 6, 2020, this repository does not explicitly contain the Noah-MP code and parameters, but
connects to the Noah-MP repository (https://github.com/NCAR/noahmp) through git submodules. To clone
this repository and populate with the Noah-MP code/parameters, use the following command:

git clone --recurse-submodules https://github.com/NCAR/hrldas

Repository branch structure

  master: this is the base branch and contains all finalized development before the WRF release
