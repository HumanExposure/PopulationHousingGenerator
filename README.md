# Residential Population Generator.

This repository contains scripts, input files, and some example 
output files for the Residential Population Generator, an R-based tool to generate synthetic human residental populations to use in making estimates of near-field chemical exposures. This tool is most readily adapted for using in the workflow for CHEM, the Combined Human Exposure Model, avaialable in two other GitHub repositories in the HumanExposure project, including ProductUseScheduler and source2dose. CHEM is currently best suited to estimating exposure to product use. Outputs from RPGen are translated into ProductUseScheduler, which with subsequent outputs used in source2dose. Please see documentation for those modules in the appropriate repositories.  

Detailed information about this module is being included in the following manuscript, currently in preparation:
East et al. 2020. The Residential Population Generator(RPGen): A tool to parameterize residential, demographic, and physiological data to model intraindividual exposure, dose, and risk.


RPGen requires information from three data sources, including the American Housing Survey(AHS), the Residential Energy Consumption Survey(RECS) and the Person Public Use Microdata Survey (PUMS). This information must currently be manually downloaded from the following sources:

RECS: https://www.eia.gov/consumption/residential/data/2015
AHS: https://census.gov/programs-surveys/ahs/data.html
PUMS: http://www.census.gov/programs-surveys/acs/data/pums.html

This version of RPGen includes a .SAS file to process manually downloaded AHS, RECS, and PUMS data into properly formatted inputs. In addition, processed versions of data for RECS and AHS are included as default input values. PUMS data is quite large, exceeding the 100 MB file size limit for GitHub. Therefore, it must be downloaded and processed by the user prior to use by RPGEn.  

RPGen utilizes tools from the R package httk to link physiological parameters to sampled individuals. However, it no longer requires httkpop.tar.gz datafile, as the httk package is freely avaialble from CRAN. 

RPG was originally written by GG at ICF. This version includes information submitted to EPA in August 2019. 
Additional updates to RPGen are forthcoming, including an automatic download function and process function.  
