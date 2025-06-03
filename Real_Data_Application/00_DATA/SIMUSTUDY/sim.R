library(tidyverse)
library(NPVecchia)
library(srgmm)
library(invgamma)
library(MASS)
library(fields)


load("../../../Downloads/Sim2_Data1.Rdata")
data <- sim

hist(c(data),breaks = 100)
plot(c(data),pch=".")


#data2 <- scale(data)
data2 <- data
sum(is.na(data2))
sum(data2==1)
data2 <- data2 
sum(data2<0)
sum(data2>1)

hist(c(data2),breaks = 100)

plot(coordinates)

#------re-running the model with data augmentation strategy
set.seed(21454)
res_sim <- srgmm::srgmm(datum = (data2), 
                            locs = coordinates, 
                            K = 2, 
                            optim.lim = c(-6,4.1),
                            conv_thres  = 1e-5,
                            penalization_theta =  10, 
                            penalization_mu = 100, 
                            nu = 1, 
                            maxit =100,
                            stochastic = F)

saveRDS(res_sim,"simuandrea.RDS")
table(Z,res_small$Z)



plot(ts(res_small$logL))
plot(ts(diff(res_small$logL)))

data3  =(data2)

table(res_small$Z)

exp(res_small$thetas)
exp(res_small$mu)

U_1 <- get_map(res_small$thetas[1,], 
               datum = data3[res_small$Z == 1,], 
               NNarray = res_small$data$NNarray)
diag(U_1) # pay attention that the diagonal contains sqrt(solve(D)), thus 1/sqrt(d_i)
invS1 <- U_1 %*% t(U_1)  # precision matrix of cluster 1
U_2 <- get_map(res_small$thetas[2,], 
               datum = data3[res_small$Z == 2,], 
               NNarray = res_small$data$NNarray)
diag(U_2) # pay attention that the diagonal contains sqrt(solve(D)), thus 1/sqrt(d_i)
invS2 <- U_2 %*% t(U_2)  # precision matrix of cluster 1

get_precision = function(ind){
  U_3 <- get_map(res_small$thetas[ind,], 
                 datum = data3[res_small$Z == ind,], 
                 NNarray = res_small$data$NNarray)
  diag(U_3) # pay attention that the diagonal contains sqrt(solve(D)), thus 1/sqrt(d_i)
  invS3 <- U_3 %*% t(U_3)  # precision matrix of cluster 1
  
}

image(get_precision(1))
image(get_precision(2))
res_small$thetas
res_small$mu


pheatmap::pheatmap(S1_0)
pheatmap::pheatmap(S2_0)


S1_0 <- cov2cor(as.matrix(solve(get_precision(1))))
S2_0 <- cov2cor(as.matrix(solve(get_precision(2)))) 
S3_0 <- cov2cor(as.matrix(solve(get_precision(3)))) 
S4_0 <- cov2cor(as.matrix(solve(get_precision(4)))) 
S5_0 <- cov2cor(as.matrix(solve(get_precision(5)))) 

diag(S1_0) <- 0
diag(S2_0) <- 0
diag(S3_0) <- 0
diag(S4_0) <- 0
diag(S5_0) <- 0



D <- rbind(
  data.frame(x= coordinates[,1],
             y= coordinates[,2],
             c= (rowMeans(abs(S1_0))),
             l = "S1"),
  data.frame(x= coordinates[,1],
             y= coordinates[,2],
             c= (rowMeans(abs(S2_0))),
             l = "S2"),
data.frame(x= coordinates[,1],
           y= coordinates[,2],
           c= (rowMeans(abs(S3_0))),
           l = "S3"))

D %>% filter(l=="S1") %>% 
  ggplot()+
  geom_point(aes(x = x,
                 y = y,
                 col = log(c)),
             size=2)+
  theme_bw() + 
  scale_color_distiller(palette = "Spectral")+
  #scale_color_viridis_c(option = "A")+
  facet_wrap(~l)
D %>% filter(l=="S2") %>% 
  ggplot()+
  geom_point(aes(x = x,
                 y = y,
                 col = (c)),
             size=2)+
  theme_bw() + 
  scale_color_distiller(palette = "Spectral")+
  #scale_color_viridis_c(option = "A")+
  facet_wrap(~l)
D %>% filter(l=="S3") %>% 
  ggplot()+
  geom_point(aes(x = x,
                 y = y,
                 col = (c)),
             size=2)+
  theme_bw() + 
  scale_color_distiller(palette = "Spectral")+
  #scale_color_viridis_c(option = "A")+
  facet_wrap(~l)
D %>% filter(l=="S4") %>% 
  ggplot()+
  geom_point(aes(x = x,
                 y = y,
                 col = log(c)),
             size=2)+
  theme_bw() + 
  scale_color_distiller(palette = "Spectral")+
  #scale_color_viridis_c(option = "A")+
  facet_wrap(~l)
D %>% filter(l=="S5") %>% 
  ggplot()+
  geom_point(aes(x = x,
                 y = y,
                 col = log(c)),
             size=2)+
  theme_bw() + 
  scale_color_distiller(palette = "Spectral")+
  #scale_color_viridis_c(option = "A")+
  facet_wrap(~l)
D %>%  
  ggplot()+
  geom_point(aes(x = x,
                 y = y,
                 col = (c)),
             size=2)+
  theme_bw() + 
  scale_color_distiller(palette = "Spectral")+
  #scale_color_viridis_c(option = "A")+
  facet_wrap(~l)





