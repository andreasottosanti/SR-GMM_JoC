# SR-GMM
Package for fitting Spatially Regularized Gaussian Mixture Models 

### Simulation experiments

The scripts are configured to automatically detect the working directory when run in RStudio.

To reproduce a specific simulation experiment, open the corresponding directory within the `Simulations` folder.

- to generate the data, `run 1_generate_data.R`, making sure to first set the number of observations per cluster. For example:
`n <- c(100, 100)` will produce two equally sized clusters. Simulated data shown in the article are already available in `/Data`
- to perform the estimation using SR-GMM, run `2_estimate.R`
- to obtain the results, run `3_get_results.R`.

### Analysis of the prostate cancer tissue sample

- to estimate the models, run `R_01_Running_models.R`
- to reproduce Figure 1 and Figure 5 shown in the article, use `R_02_Results.R`
- to reproduce Supplementary Figures 2 and 3, use `R_03_Supplementary_Figures.R`