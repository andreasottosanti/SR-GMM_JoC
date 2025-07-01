rm(list = ls())
library(srgmm)
library(NPVecchia)
library(Matrix)
library(fields)
set.seed(123)

Setups <- c("150_50", "100_100")
directory_path <- dirname(rstudioapi::getSourceEditorContext()$path)

for(setup in Setups){
  working_directory <- paste(directory_path, setup, sep="/")

  # create results directory
  if(!("Results" %in% dir(working_directory))) dir.create(paste(working_directory,"Results",sep="/"))

  N_scenarios <- length(dir(paste(working_directory, "Data",sep="/")))

  Run.per.simulation <- 10
  for(i in 1:N_scenarios){
    combination_type <- dir(paste(working_directory, "Data",sep="/"))[i]
    print(combination_type)
    for(j in 1:length(dir(paste(working_directory, "Data", combination_type, sep = "/")))){
      results <- list()
      load(paste(working_directory, "/Data/", combination_type, "/Data", j, ".Rdata", sep=""))
      for(k in 1:Run.per.simulation){
        print(paste("---------------------RUN",k))
        results[[k]] <- srgmm(datum = y, locs = C, K = 2, maxit = 200, iters.before.convergence = 2, seed = 123*k)
      }
      if(!(combination_type %in% dir(paste(working_directory,"Results",sep="/")))) dir.create(paste(working_directory,"Results",combination_type,sep="/"))
      saving_results_directory <- paste(working_directory,"Results",combination_type,sep="/")
      save(results, file = paste(saving_results_directory,"/Results",j,".Rdata",sep=""))
    }
  }

}
