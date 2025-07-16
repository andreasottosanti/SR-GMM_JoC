rm(list = ls())

#------re-running the model with data augmentation strategy
library(fields)
library(srgmm)
library(Matrix)

data2 <- readRDS("Real_Data_Application/00_DATA/APPLICATION/data.RDS")
coordinates <- data2$locs.ord
data2 <- data2$datum.ord

re3  = list()
re4  = list()
re5  = list()

for(i in 1:10){
  cat(paste("------------------- RUN",i,"-------------------"))
  re3[[i]] <- srgmm::srgmm(datum = (data2), 
                             locs = coordinates, 
                             K = 3, 
                             optim.lim = c(-6,4.1),
                             conv_thres  = 1e-4,
                             penalization_theta  = 100, 
                             penalization_mu = 100, 
                             nu = 1, 
                             maxit = 50, 
                           seed = 123*i)
}
saveRDS(re3,file = "Real_Data_Application/01_OUTPUT/APPLICATION/RDS/K3_10runs.RDS")

for(i in 1:10){
  cat(paste("------------------- RUN",i,"-------------------"))
  re4[[i]] <- srgmm::srgmm(datum = (data2), 
                           locs = coordinates, 
                           K = 4, 
                           optim.lim = c(-6,4.1),
                           conv_thres  = 1e-4,
                           penalization_theta  = 100, 
                           penalization_mu = 100, 
                           nu = 1, 
                           maxit = 50, 
                           seed = 123*i)
}
saveRDS(re4,file = "Real_Data_Application/01_OUTPUT/APPLICATION/RDS/K4_10runs.RDS")

for(i in 1:10){
  cat(paste("------------------- RUN",i,"-------------------"))
  re5[[i]] <- srgmm::srgmm(datum = (data2), 
                           locs = coordinates, 
                           K = 5, 
                           optim.lim = c(-6,4.1),
                           conv_thres  = 1e-4,
                           penalization_theta  = 100, 
                           penalization_mu = 100, 
                           nu = 1, 
                           maxit = 50, 
                           seed = 123*i)
}
saveRDS(re5,file = "Real_Data_Application/01_OUTPUT/APPLICATION/RDS/K5_10runs.RDS")



# select the best run -----------------------------------------------------


re3 = readRDS("Real_Data_Application/01_OUTPUT/APPLICATION/RDS/K3_10runs.RDS")
re4 = readRDS("Real_Data_Application/01_OUTPUT/APPLICATION/RDS/K4_10runs.RDS")
re5 = readRDS("Real_Data_Application/01_OUTPUT/APPLICATION/RDS/K5_10runs.RDS")

plot(unlist(lapply(re3, function(x) x$max.logL)))
plot(unlist(lapply(re4, function(x) x$max.logL)))
plot(unlist(lapply(re5, function(x) x$max.logL)))

ind3=which.max(unlist(lapply(re3, function(x) x$max.logL)))
ind4=which.max(unlist(lapply(re4, function(x) x$max.logL)))
ind5=which.max(unlist(lapply(re5, function(x) x$max.logL)))


saveRDS(re3[[ind3]],"Real_Data_Application/01_OUTPUT/APPLICATION/RDS/best3.RDS")
saveRDS(re4[[ind4]],"Real_Data_Application/01_OUTPUT/APPLICATION/RDS/best4.RDS")
saveRDS(re5[[ind5]],"Real_Data_Application/01_OUTPUT/APPLICATION/RDS/best5.RDS")

