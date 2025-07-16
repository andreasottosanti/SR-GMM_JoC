# SR-GMM
This repository contains all the scripts to reproduce the analyses that appear in the article ``Spatially Regularized Gaussian Mixtures for
Clustering Spatial Transcriptomic Data".

### Simulation experiments

The scripts are configured to automatically detect the working directory when run in RStudio.

To reproduce a specific simulation experiment, navigate to the corresponding directory within the `Simulations` folder:

- Run `1_generate_data.R` to generate the data.  
  Before running the script, set the number of observations per cluster. For example:  
  `n <- c(100, 100)` will generate two equally sized clusters. Then, run the entire script.
  The simulated datasets used in the article are already available in the `/Data` folder.
- Run `2_estimate.R` to estimate SR-GMM.
- Run `3_get_results.R` to retrieve the results.


### Analysis of the prostate cancer tissue sample

The directory `Real_Data_Application/01_OUTPUT/APPLICATION/RDS` already contains the file `best3.RDS`, which stores the results from the run that achieved the highest posterior value using three clusters.

- To generate **Figure 1** of the article, run `R_00_Figure1.R`.
- To estimate the models, run `R_01_Running_models.R`.
- To reproduce **Figure 5** from the article, run `R_02_Analysis.R`.  
  The file `best3.RDS` already includes the `rho_statistic` object, so it is only necessary to load the file and generate the plot—no need to rerun the analyses.
- To perform the **gene set enrichment analysis**, run `R_03_GSEA.R`.
- To reproduce **Supplementary Figure 3**, run `R_04_comp_time_plots.R`.  
  Note that the files `K3_10runs.RDS`, `K4_10runs.RDS`, and `K5_10runs.RDS` are not included in the `Real_Data_Application/01_OUTPUT/APPLICATION/RDS` directory due to their large size.
- To reproduce **Supplementary Figure 2**, run `R_05_boxplots.R`.
