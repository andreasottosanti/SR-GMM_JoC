library(tidyverse)
library(patchwork)
library(latex2exp)

exponential <- function(x, sigma2, phi, tau) sigma2*exp(-x/phi)+tau*I(x == 0)
gaussian <- function(x, sigma2, phi, tau) sigma2*exp(-x^2/phi^2)+tau*I(x == 0)
exponential <- Vectorize(exponential, "x")
gaussian <- Vectorize(gaussian, "x")

gg_graphs <- list()


# simulation 1 ------------------------------------------------------------

x_seq <- seq(0, 7, by = 0.1)
sigma2 <- c(1,2,3)
phi <- c(1,1,1)
tau <- c(0,0,0)
df <- labels <- c()
for(i in 1:3){
  df_exp <- data.frame(x = x_seq, 
                       y = exponential(x_seq, i, phi[1], tau[1]), 
                       Model = "Exponential", 
                       #lab = labels, 
                       lab = paste("$\\sigma^2 =",i,";\\, \\phi =", phi[1],";\\, \\tau^2 =",tau[1],"$"))
  df_gaus <- data.frame(x = x_seq, y = gaussian(x_seq, i, phi[2], tau[1]),  
                        Model = "Gaussian",
                        lab = paste("$\\sigma^2 =",i,";\\, \\phi =", phi[2],";\\, \\tau^2 =",tau[1],"$"))
  df <- bind_rows(df, df_exp, df_gaus)
}

table(df$lab)

#gg_graphs[[1]] <- 
  ggplot(df )+
  geom_line(aes(x = x, y = y, col = (lab),lty =  Model ), lwd = .8)+
  scale_color_brewer(labels = (latex2exp::TeX(unique(df$lab))),
                     palette = "Dark2")+
  theme_bw()+theme(legend.position = "bottom", text=element_text(size=20))+
  labs(x = "Distance", y = "Covariance", col="", lty="")+
  facet_wrap(~"Simulation 1")
  
# simulation 2 ------------------------------------------------------------

x_seq <- seq(0, 7, by = 0.1)
sigma2 <- c(2,2,2)
tau <- c(0,0,0)
df2 <- labels <- c()
for(i in c(.1, 1, 3)){
  labs <- paste("$\\sigma^2 =",sigma2[i],";\\, \\phi =", i,";\\, \\tau^2 =",tau[i],"$")
  df_exp <- data.frame(x = x_seq, y = exponential(x_seq, sigma2[1], i, tau[1]), 
                       Model = "Exponential", lab = labs)
  df_gaus <- data.frame(x = x_seq, y = gaussian(x_seq, sigma2[2], i, tau[2]),  
                        Model = "Gaussian",  lab = labs)
  df2 <- rbind(df2, df_exp, df_gaus)
}
table(df$lab)
table(df2$lab)


# simulation 3 ------------------------------------------------------------

x_seq <- seq(0, 7, by = 0.1)
sigma2 <- c(2,2,2)
phi = c(1,1,1)
tau <- c(0,1,3)
df3 <- labels <- c()
for(i in 1:3){
  labs <- paste("$\\sigma^2 =",sigma2[i],";\\, \\phi =", i,";\\, \\tau^2 =",tau[i],"$")
  df_exp <- data.frame(x = x_seq, y = exponential(x_seq, sigma2[1], phi[1], tau[i]), 
                       Model = "Exponential", lab = labs)
  df_gaus <- data.frame(x = x_seq, y = gaussian(x_seq, sigma2[1], phi[2], tau[i]),  
                        Model = "Gaussian",  lab = labs)
  df3 <- rbind(df3, df_exp, df_gaus)
}
table(df$lab)
table(df2$lab)
table(df3$lab)



D1 <- df %>% mutate(type="Simulation 1")
D2 <- df2 %>% mutate(type="Simulation 2")
#D3 <- df3 %>% mutate(type="Simulation 3")

D3 <- rbind(D1,D2,D3)
table(D3$lab,D3$Model)

levels(factor(D3$lab))

QQ <- 
  ggplot(D3)+
  geom_line(aes(x = x, y = y, col = (lab), lty =  Model), lwd = .8)+
  scale_color_brewer(labels = latex2exp::TeX(unique(D3$lab)),
                      palette = "Dark2")+
  theme_bw()+
  theme(legend.position = "bottom", text=element_text(size=20))+
  labs(x = "Distance", y = "Covariance", col="", lty="")+
  facet_wrap(~type)+
  guides(col = guide_legend(nrow = 2), lty = guide_legend(nrow = 2))

QQ
QQ+ggview::canvas(h=5,w=14)

ggsave("01_OUTPUT/APPLICATION/PLOT/kernels.pdf",width = 14,height = 5)
ggsave("01_OUTPUT/APPLICATION/PLOT/kernels.png",width = 14,height = 5)
ggsave("01_OUTPUT/APPLICATION/PLOT/kernels.eps",width = 14,height = 5)




# simulation 1 ------------------------------------------------------------

x_seq <- seq(0, 7, by = 0.1)
sigma2 <- c(2,2)
phi <- c(1,1)
tau <- c(0,1,3)
df <- labels <- c()
for(i in 1:3){
  df_exp <- data.frame(x = x_seq, 
                       y = exponential(x_seq, sigma2[i], phi[1], tau[i]), 
                       Model = "Exponential", 
                       #lab = labels, 
                       lab = paste("$\\sigma^2 =",i,";\\, \\phi =", phi[1],"$"))#; $\\tau$ =",tau[1]))
  df_gaus <- data.frame(x = x_seq, y = gaussian(x_seq, sigma2[i], phi[i], tau[i]),  
                        Model = "Gaussian",
                        lab = paste("$\\sigma^2 =",i,";\\, \\phi =", phi[2],"$"))#,"; $\\tau$ =",tau[2]))
  df <- bind_rows(df, df_exp, df_gaus)
}

table(df$lab)

#gg_graphs[[1]] <- 
ggplot(df )+
  geom_line(aes(x = x, y = y, col = (lab),lty =  Model ), lwd = .8)+
  scale_color_brewer(labels = unname(latex2exp::TeX(unique(df$lab))),
                     palette = "Dark2")+
  theme_bw()+theme(legend.position = "bottom", text=element_text(size=20))+
  labs(x = "Distance", y = "Covariance", col="", lty="")+
  facet_wrap(~"Simulation 1")

# simulation 2 ------------------------------------------------------------

x_seq <- seq(0, 7, by = 0.1)
sigma2 <- c(2,2)
tau <- c(0,0)
df2 <- labels <- c()
for(i in c(.1, 1, 3)){
  labs <- paste("$\\sigma^2 =",sigma2[1],";\\, \\phi =", i,"$")
  df_exp <- data.frame(x = x_seq, y = exponential(x_seq, sigma2[1], i, tau[1]), 
                       Model = "Exponential", lab = labs)
  df_gaus <- data.frame(x = x_seq, y = gaussian(x_seq, sigma2[2], i, tau[2]),  
                        Model = "Gaussian",  lab = labs)
  df2 <- rbind(df2, df_exp, df_gaus)
}
table(df$lab)
table(df2$lab)

plot(df$y,df2$y)

D1 <- df %>% mutate(type="Simulation 1")
D2 <- df2 %>% mutate(type="Simulation 2")

D3 <- rbind(D1,D2)
table(D3$lab,D3$Model)

levels(factor(D3$lab))

QQ <- 
  ggplot(D3)+
  geom_line(aes(x = x, y = y, col = (lab), lty =  Model), lwd = .8)+
  scale_color_brewer(labels = unname(latex2exp::TeX(unique(D3$lab))),
                     palette = "Set1")+
  theme_bw()+
  theme(legend.position = "bottom", text=element_text(size=20))+
  labs(x = "Distance", y = "Covariance", col="", lty="")+
  facet_wrap(~type)

QQ+ggview::canvas(h=5,w=14)

ggsave("01_OUTPUT/APPLICATION/PLOT/kernels_nugg.pdf",width = 14,height = 5)
ggsave("01_OUTPUT/APPLICATION/PLOT/kernels_nugg.png",width = 14,height = 5)
ggsave("01_OUTPUT/APPLICATION/PLOT/kernels_nugg.eps",width = 14,height = 5)



# -------------------------------------------------------------------------

library(tidyverse)
library(patchwork)
library(latex2exp)

exponential <- function(x, sigma2, phi, tau) sigma2 * exp(-x/phi) + tau*I(x == 0)
gaussian    <- function(x, sigma2, phi, tau) sigma2 * exp(-x^2/phi^2) + tau*I(x == 0)
exponential <- Vectorize(exponential, "x")
gaussian <- Vectorize(gaussian, "x")

gg_graphs <- list()


# simulation 1 ------------------------------------------------------------

x_seq <- seq(0, 7, by = 0.1)
sigma2 <- c(1,2,3)
phi <- c(1,1,1)
tau <- c(0,0,0)
df <- labels <- c()
for(i in 1:3){
  df_exp <- data.frame(x = x_seq, 
                       y = exponential(x_seq, i, phi[1], tau[1]), 
                       Model = "Exponential", 
                       #lab = labels, 
                       lab = paste("$\\sigma^2 =",i,";\\, \\phi =", phi[1]))#,";\\, \\tau^2 =",tau[1],"$"))
  df_gaus <- data.frame(x = x_seq, y = gaussian(x_seq, i, phi[2], tau[1]),  
                        Model = "Gaussian",
                        lab = paste("$\\sigma^2 =",i,";\\, \\phi =", phi[2]))#,";\\, \\tau^2 =",tau[1],"$"))
  df <- bind_rows(df, df_exp, df_gaus)
}

table(df$lab)

#gg_graphs[[1]] <- 
ggplot(df )+
  geom_line(aes(x = x, y = y, col = (lab),lty =  Model ), lwd = .8)+
  scale_color_brewer(labels = (latex2exp::TeX(unique(df$lab))),
                     palette = "Dark2")+
  theme_bw()+theme(legend.position = "bottom", text=element_text(size=20))+
  labs(x = "Distance", y = "Covariance", col="", lty="")+
  facet_wrap(~"Simulation 1")

# simulation 2 ------------------------------------------------------------

x_seq <- seq(0, 7, by = 0.1)
sigma2 <- c(2,2,2)
tau <- c(0,0,0)
df2 <- labels <- c()
for(i in c(.1, 1, 3)){
  labs <- paste("$\\sigma^2 =",2,";\\, \\phi =", i)#,";\\, \\tau^2 =",tau[i],"$")
  df_exp <- data.frame(x = x_seq, y = exponential(x_seq, sigma2[1], i, tau[1]), 
                       Model = "Exponential", lab = labs)
  df_gaus <- data.frame(x = x_seq, y = gaussian(x_seq, sigma2[2], i, tau[2]),  
                        Model = "Gaussian",  lab = labs)
  df2 <- rbind(df2, df_exp, df_gaus)
}
table(df$lab)
table(df2$lab)


# simulation 3 ------------------------------------------------------------

x_seq <- seq(0, 7, by = 0.1)
sigma2 <- c(2,2,2)
phi = c(1,1,1)
tau <- c(0,0,0)
df3 <- labels <- c()
for(i in 1){
  labs <- paste("$\\sigma^2 =",sigma2[i],";\\, \\phi =", i)#,";\\, \\tau^2 =",tau[i],"$")
  df_exp <- data.frame(x = x_seq, y = exponential(x_seq, sigma2[1], i, tau[1]), 
                       Model = "Exponential", lab = labs)
  df_gaus <- data.frame(x = x_seq, y = gaussian(x_seq, sigma2[2], i, tau[2]),  
                        Model = "Gaussian",  lab = labs)
  df3 <- rbind(df3, df_exp, df_gaus)
}
table(df$lab)
table(df2$lab)
table(df3$lab)



D1 <- df %>% mutate(type="Simulation 1")
D2 <- df2 %>% mutate(type="Simulation 2")
D3 <- df3 %>% mutate(type="Simulation 3")

D3 <- rbind(D1,D2,D3)
table(D3$lab,D3$Model)

levels(factor(D3$lab))

D3aux = data.frame(x = c(0,0,0), y = c(2,3,5), type = "Simulation 3", shap = c("$\\tau^2 = 0$","$\\tau^2 = 1$","$\\tau^2 = 3$"))


QQ <- 
  ggplot(D3)+
  geom_line(aes(x = x, y = y, col = (lab), lty =  Model), lwd = .8)+
  scale_color_brewer(labels = latex2exp::TeX(unique(D3$lab)),
                     palette = "Dark2")+
  geom_point(data = D3aux, aes(x =x,y=y,shape = shap), size=3)+
  scale_shape_manual("",values = c(16,4,21), labels = latex2exp::TeX(unique(D3aux$shap)))+
  theme_bw()+
  theme(legend.position = "bottom", text=element_text(size=20))+
  labs(x = "Distance", y = "Covariance function", col="", lty="")+
  facet_wrap(~type)+
  guides(col = guide_legend(nrow = 2), lty = guide_legend(nrow = 2),shape = guide_legend(nrow = 1))

QQ
QQ+ggview::canvas(h=5,w=14)

ggsave("01_OUTPUT/APPLICATION/PLOT/kernelsScen3.pdf",width = 14,height = 5)
ggsave("01_OUTPUT/APPLICATION/PLOT/kernelsScen3.png",width = 14,height = 5)
ggsave("01_OUTPUT/APPLICATION/PLOT/kernelsScen3.eps",width = 14,height = 5)




# simulation 1 ------------------------------------------------------------

x_seq <- seq(0, 7, by = 0.1)
sigma2 <- c(2,2)
phi <- c(1,1)
tau <- c(0,1,3)
df <- labels <- c()
for(i in 1:3){
  df_exp <- data.frame(x = x_seq, 
                       y = exponential(x_seq, sigma2[i], phi[1], tau[i]), 
                       Model = "Exponential", 
                       #lab = labels, 
                       lab = paste("$\\sigma^2 =",i,";\\, \\phi =", phi[1],"$"))#; $\\tau$ =",tau[1]))
  df_gaus <- data.frame(x = x_seq, y = gaussian(x_seq, sigma2[i], phi[i], tau[i]),  
                        Model = "Gaussian",
                        lab = paste("$\\sigma^2 =",i,";\\, \\phi =", phi[2],"$"))#,"; $\\tau$ =",tau[2]))
  df <- bind_rows(df, df_exp, df_gaus)
}

table(df$lab)

#gg_graphs[[1]] <- 
ggplot(df )+
  geom_line(aes(x = x, y = y, col = (lab),lty =  Model ), lwd = .8)+
  scale_color_brewer(labels = unname(latex2exp::TeX(unique(df$lab))),
                     palette = "Dark2")+
  theme_bw()+theme(legend.position = "bottom", text=element_text(size=20))+
  labs(x = "Distance", y = "Covariance", col="", lty="")+
  facet_wrap(~"Simulation 1")

# simulation 2 ------------------------------------------------------------

x_seq <- seq(0, 7, by = 0.1)
sigma2 <- c(2,2)
tau <- c(0,0)
df2 <- labels <- c()
for(i in c(.1, 1, 3)){
  labs <- paste("$\\sigma^2 =",sigma2[1],";\\, \\phi =", i,"$")
  df_exp <- data.frame(x = x_seq, y = exponential(x_seq, sigma2[1], i, tau[1]), 
                       Model = "Exponential", lab = labs)
  df_gaus <- data.frame(x = x_seq, y = gaussian(x_seq, sigma2[2], i, tau[2]),  
                        Model = "Gaussian",  lab = labs)
  df2 <- rbind(df2, df_exp, df_gaus)
}
table(df$lab)
table(df2$lab)

plot(df$y,df2$y)

D1 <- df %>% mutate(type="Simulation 1")
D2 <- df2 %>% mutate(type="Simulation 2")

D3 <- rbind(D1,D2)
table(D3$lab,D3$Model)

levels(factor(D3$lab))

QQ <- 
  ggplot(D3)+
  geom_line(aes(x = x, y = y, col = (lab), lty =  Model), lwd = .8)+
  scale_color_brewer(labels = unname(latex2exp::TeX(unique(D3$lab))),
                     palette = "Set1")+
  theme_bw()+
  theme(legend.position = "bottom", text=element_text(size=20))+
  labs(x = "Distance", y = "Covariance", col="", lty="")+
  facet_wrap(~type)

QQ+ggview::canvas(h=5,w=14)

ggsave("01_OUTPUT/APPLICATION/PLOT/kernels_Scen3.pdf",width = 14,height = 5)
ggsave("01_OUTPUT/APPLICATION/PLOT/kernels_Scen3.png",width = 14,height = 5)
ggsave("01_OUTPUT/APPLICATION/PLOT/kernels_Scen3.eps",width = 14,height = 5)
