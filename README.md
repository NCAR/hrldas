[![DOI](https://zenodo.org/badge/261594454.svg)](https://zenodo.org/badge/latestdoi/261594454)


# HRLDAS (High Resolution Land Data Assimilation System) Community Model Repository

The High-Resolution Land Data Assimilation System (HRLDAS) is a widely-used open-source offline community framework/driver of land surface models (LSMs). HRLDAS uses a combination of observed and analyzed meterological forcing (precipitation, shortwave and longwave radiation, surface wind, specific humidity, temperature, surface pressure) to drive a LSM to simulate the evolution of land surface states. The system has been developed to leverage the WRF pre-processed input data (e.g., WPS geo_em* file) and conduct computationally-efficient model run to generate more accurate initial land state conditions and/or produce the offline LSM simulations alone for scientific studies.

This is the offical HRLDAS/Noah-MP unified Github repository for code downloading and contribution. Note that the HRLDAS/Noah-MP model is a community model developed with contributions from the entire scientific community. For maintenance and release of this GitHub, please contact: Cenlin He (cenlinhe@ucar.edu).

HRLDAS model website: https://ral.ucar.edu/solutions/products/high-resolution-land-data-assimilation-system-hrldas

Noah-MP model GitHub repository: https://github.com/NCAR/noahmp

To download the HRLDAS/Noah-MP code, use the following command:

git clone --recurse-submodules https://github.com/NCAR/hrldas

If the "--recurse-submodules" is not specified, the Noah-MP source code will not be downloaded.


## Release of HRLDAS/Noah-MP version 5.0 (Refactored/Modernized version)

This is a new HRLDAS version 5.0 that is coupled with the modernized/refactored Noah-MP model version 5.0. To work with the refactored Noah-MP code structure, some substantial changes have also been made to the HRLDAS code and data structures since this release. Particularly, a new "NoahmpIO" data type has been created, which has been defined in the HRLDAS/Noah-MP interface (driver) part in Noah-MP model repository. All future HRLDAS/Noah-MP developments and updates will be made only to this modernized/refactored version. More detailed information about the model refactoring/modernization is provided in the Noah-MP model repository (https://github.com/NCAR/noahmp) and the new technical documentation (http://dx.doi.org/10.5065/ew8g-yr95).


## HRLDAS/Noah-MP technical documentation and model description papers

Technical documentation freely available at http://dx.doi.org/10.5065/ew8g-yr95

**To cite the technical documentation**:  He, C., P. Valayamkunnath, M. Barlage, F. Chen, D. Gochis, R. Cabell, T. Schneider, R. Rasmussen, G.-Y. Niu, Z.-L. Yang, D. Niyogi, and M. Ek (2023): The Community Noah-MP Land Surface Modeling System Technical Description Version 5.0, (No. NCAR/TN-575+STR). doi:10.5065/ew8g-yr95

**HRLDAS/Noah-MP version 5.0 model description paper**:  He, C., Valayamkunnath, P., Barlage, M., Chen, F., Gochis, D., Cabell, R., Schneider, T., Rasmussen, R., Niu, G.-Y., Yang, Z.-L., Niyogi, D., and Ek, M.: Modernizing the open-source community Noah with multi-parameterization options (Noah-MP) land surface model (version 5.0) with enhanced modularity, interoperability, and applicability, Geosci. Model Dev., 16, 5131–5151, https://doi.org/10.5194/gmd-16-5131-2023, 2023.

**Original HRLDAS model description paper**: Fei Chen, Kevin W. Manning, Margaret A. LeMone, Stanley B. Trier, Joseph G. Alfieri, Rita Roberts, Mukul Tewari, Dev Niyogi, Thomas W. Horst, Steven P. Oncley, Jeffrey B. Basara, and Peter D. Blanken, 2007: Description and Evaluation of the Characteristics of the NCAR High-Resolution Land Data Assimilation System. J. Appl. Meteor. Climatol., 46, 694–713.
doi: http://dx.doi.org/10.1175/JAM2463.1

**Noah-MP Workshop Summary & Future Priorities**: He, C., Chen, F., Barlage, M., Yang, Z.-L., Wegiel, J. W., Niu, G.-Y., Gochis, D., Mocko, D. M., Abolafia-Rosenzweig, R., Zhang, Z., Lin, T.-S., Valayamkunnath, P., Ek, M., and Niyogi, D. (2023): Enhancing the community Noah-MP land model capabilities for Earth sciences and applications, Bull. Amer. Meteor. Soc., E2023–E2029, https://doi.org/10.1175/BAMS-D-23-0249.1


## Noah-MP Strategic Plan Committee

Cenlin He, Prasanth Valayamkunnath, Michael Barlage, David Mocko, Guo-Yue Niu, Myung-Seo Koo, Fei Chen, Jerry Wiegel, Zong-Liang Yang, Dev Niyogi, Xuemei Wang, Gonzalo Miguez-Macho


## Noah-MP Code Review Committee

The current code review committee list is here: https://docs.google.com/spreadsheets/d/13kGfVbf5TmFJm-wjis1ZQjmt7eCXFLfbZaqXkvPSi14/edit?gid=0#gid=0


## HRLDAS GitHub structure

**The folders**:

1. hrldas/: The main hlrdas forcing, driver, and run code folders;

2. noahmp/: Noah-MP LSM source code that is direclty connected to Noah-MP GitHub repository via submodule;

3. urban/: Urban model code (currently include WRF-urban models: SLUCM, BEP, BEP_BEM)

**The branches**:

1. "master" branch: (currently version 5.0), most stable & latest version, updated whenever there are bug fixes or major model update/release (by merging from the "develop" branch);

2. "develop" branch: (currently version 5.0), used for continuous HRLDAS/Noah-MP development, keep updated by including bug fixes and code updates (e.g., new physics options, processes, etc.); 

3. other version release branches: archive different released code versions.


## Important notes

For users who are interested in previous HRLDAS/Noah-MP code versions (prior to version 5.0), please refer to the different GitHub branches in this repository. Particularly, the "release-v4.5-WRF" branch has the same model physics as version 5.0, but with an old model code structure, which is consistent with the Noah-MP code released along with WRF version 4.5.


## Code contribution via GitHub

Users are welcome to make code development and contributions through GitHub pull requests. The pull request will be reviewed by the HRLDAS/Noah-MP model physics and code release team, and if everything looks good, the pull request of new code development or bug fixes will be merged into the develop branch. During each year's major version release period, the updated develop branch will be further merged into the master branch for official release of a new HRLDAS/Noah-MP model version.

Some suggestions for model developers to contribute to HRLDAS code through the GitHub repository (typical procedures):

1. Step (1) Create a fork of this official HRLDAS repository to your own GitHub account; 

2. Step (2) Create a new branch based on the latest "develop" branch and make code updates/changes in the forked repository under your own account; 

3. Step (3) Finalize and test the code updates you make; 

4. Step (4) Submit a pull request for your code updates from your own forked Github repository to the "develop" branch of this official HRLDAS repository;

5. Step (5) The HRLDAS/Noah-MP physics and code review committee reviews and tests the model updates in the submitted pull request and discusses with the developer if there is any problem; 

6. Step (6) The HRLDAS/Noah-MP physics and code review committee confirms the pull request and merges the updated code to the "develop" branch in this official HRLDAS repository;

7. Step (7) The HRLDAS/Noah-MP physics and code review committee merges the updated "develop" branch to the master branch during the annual release of new model versions.


## License

The license and terms of use for this software can be found [here](https://github.com/NCAR/hrldas/blob/master/LICENSE.txt).
