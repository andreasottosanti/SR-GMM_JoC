
hist(c(dat),breaks = 100)
data2 <- scale(dat,center = T)

sum(is.na(data2))
sum(data2==1)
data2 <- data2 
sum(data2<0)
sum(data2>1)

hist(c(data2),breaks = 100)


plot(rowMeans(data2))

#------re-running the model with data augmentation strategy
library(fields)
re3  = list()
re4  = list()
re5  = list()

for(i in 1:10){
set.seed(123*i)
re3[[i]] <- srgmm::srgmm(datum = (data2), 
                           locs = results$data$locs.ord, 
                           K = 3, 
                           optim.lim = c(-6,4.1),
                           conv_thres  = 1e-4,
                           penalization_theta  = 100, 
                           penalization_mu = 100, 
                           nu = 1, 
<<<<<<< HEAD
                           maxit = 50)
}
saveRDS(re3,file = "Real_Data_Application/01_OUTPUT/APPLICATION/RDS/K3_10runs.RDS")

for(i in 1:10){
set.seed(1234*i)
re4[[i]] <- srgmm::srgmm(datum = (data2), 
=======
                           maxit = 50,
                           stochastic = T)
}
# saveRDS(re3,file = "K3_10runs.RDS")

for(i in 1:10){
set.seed(1234*i)
re4[[i]] <- srgmm::srgmm(datum =(data2), 
>>>>>>> b86f12cc99353d831acd9b756ac6f651287ccabe
                           locs = results$data$locs.ord, 
                           K = 4, 
                           optim.lim = c(-6,4.1),
                           conv_thres  = 1e-4,
                           penalization_theta  = 100, 
                           penalization_mu = 100, 
                           nu = 1, 
<<<<<<< HEAD
                           maxit = 50)
}
saveRDS(re4,file = "Real_Data_Application/01_OUTPUT/APPLICATION/RDS/K4_10runs.RDS")
=======
                           maxit = 50,
                           stochastic = T)
}
# saveRDS(re4,file = "K4_10runs.RDS")
>>>>>>> b86f12cc99353d831acd9b756ac6f651287ccabe
for(i in 1:10){
set.seed(12345*i)
re5[[i]] <- srgmm::srgmm(datum = (data2), 
                           locs = results$data$locs.ord, 
                           K = 5, 
                           optim.lim = c(-6,4.1),
                           conv_thres  = 1e-4,
                           penalization_theta  = 100, 
                           penalization_mu = 100, 
                           nu = 1, 
<<<<<<< HEAD
                           maxit = 50)

}
saveRDS(re5,file = "Real_Data_Application/01_OUTPUT/APPLICATION/RDS/K5_10runs.RDS")
=======
                           maxit = 50,
                           stochastic = T)

}
# saveRDS(re5,file = "K5_10runs.RDS")
>>>>>>> b86f12cc99353d831acd9b756ac6f651287ccabe


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

