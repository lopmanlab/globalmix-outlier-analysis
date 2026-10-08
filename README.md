# Outlier Analysis

We analyzed contact diary data from the GlobalMix study collected in Mozambique, 
India, Pakistan, and Guatemala (n = 5,085). We defined three pathogen-specific 
contact types corresponding to direct deposition (RSV-like), inhalation (Mtb-like), 
and fecal-oral (rotavirus-like) transmission routes. Using principal component 
analysis and logistic regression, we identified socio-demographic predictors 
(age, sex, urban/rural residence, and occupation) of having a high number of
community contacts (non-household contacts above the 75th percentile) 
for each transmission route and country.

## Data
Clone this GlobalMix directory: https://github.com/lopmanlab/GlobalMix

Data must be in folder "../GlobalMix" relative to this directory.

## Run analysis
Entire analysis is run from   00_run_outlier_analysis_simple.R  , which reads functions 
from scripts starting 01-05.

 - `01_simpleoutcome_dataprep.R`        data cleaning and preparation
 - `02_simpleoutcome_hist.R`            plots histograms (descriptive)
 - `03_simple_outlier_analysis.R`       performs high-contacts regression
 - `04_plot_simple_analysis_results.R`  plots regression results (Figure 1-3)
 - `04_pca_simple_analysis_results.R`   performs pca analysis (Table 2)



