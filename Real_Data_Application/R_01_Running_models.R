
hist(c(dat),breaks = 100)
# plot(c(dat),pch=".")
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
                           maxit = 50,
                           stochastic = T)
}
# saveRDS(re3,file = "K3_10runs.RDS")

for(i in 1:10){
set.seed(1234*i)
re4[[i]] <- srgmm::srgmm(datum =(data2), 
                           locs = results$data$locs.ord, 
                           K = 4, 
                           optim.lim = c(-6,4.1),
                           conv_thres  = 1e-4,
                           penalization_theta  = 100, 
                           penalization_mu = 100, 
                           nu = 1, 
                           maxit = 50,
                           stochastic = T)
}
# saveRDS(re4,file = "K4_10runs.RDS")
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
                           maxit = 50,
                           stochastic = T)

}
# saveRDS(re5,file = "K5_10runs.RDS")


re3 = readRDS("K3_10runs.RDS")
re4 = readRDS("K4_10runs.RDS")
re5 = readRDS("K5_10runs.RDS")

plot(unlist(lapply(re3, function(x) x$max.logL)))
plot(unlist(lapply(re4, function(x) x$max.logL)))
plot(unlist(lapply(re5, function(x) x$max.logL)))

ind3=which.max(unlist(lapply(re3, function(x) x$max.logL)))
ind4=which.max(unlist(lapply(re4, function(x) x$max.logL)))
ind5=which.max(unlist(lapply(re5, function(x) x$max.logL)))


saveRDS(re3[[ind3]],"best3.RDS")
saveRDS(re4[[ind4]],"best4.RDS")
saveRDS(re5[[ind5]],"best5.RDS")

