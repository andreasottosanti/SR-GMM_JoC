rm(list = ls())
library(patchwork)
library(ggplot2)
library(srgmm)
library(fields)
library(imager)



# Figure 1 ----------------------------------------------------------------

plot_tissue_cont = function(D, 
                            palette = "davos", 
                            facet = NULL){
  
  G <- ggplot(D)+
    ggstar::geom_star(aes(x = x,
                          y = y,
                          fill = value),
                      color = 1 ,
                      starstroke = 0.01,
                      starshape = "hexagon", 
                      size = 2.7) +
    ggview::canvas(h=7,w=7)+
    scico::scale_fill_scico(palette = palette)+
    #  scale_fill_viridis_c(option = "A")+
    theme(legend.position = "none",text= element_text(size=20))+
    xlab("x coord.")+ylab("y coord.")+ facet_wrap(~facet) 
  G
}


plot_tissue_discr = function(D, filling, palette = "davos"){
  
  G <- ggplot(D)+
    ggstar::geom_star(aes(x=x,y=y,
                          fill=value),
                      color =   1 ,starstroke=.1,
                      #                    position = position_jitter(width = 0.25),
                      starshape = "hexagon", 
                      size = 2.7) +
    ggview::canvas(h=7,w=7)+
    scico::scale_fill_scico_d(palette = palette)+
    #  scale_fill_viridis_c(option = "A")+
    theme(legend.position = "bottom",text= element_text(size=20))+
    xlab("x coord.")+ylab("y coord.")+ facet_wrap(~facet) 
  G
  
  
}


theme_set(theme_bw())

data <- readRDS("Real_Data_Application/00_DATA/APPLICATION/data.RDS")
dat <- data$datum.ord
pix <- data$locs.ord

ppix <- rbind(pix,pix,pix)


Cor <-  cor(dat)
dis <- as.matrix(dist(pix))
CM <- rowSums(Cor*dis)/rowSums(dis)

col1 <- colMeans(dat)
col2 <- apply(dat,2,function(x) (sd(x)))


normalizer <- function(x) (x-min(x))/(max(x)-min(x))

D_mean <- data.frame(x = pix[,1],
                y=1-pix[,2],
                value = normalizer(col1),
                facet = "Avg. gene expression")
D_sd <- data.frame(x = pix[,1],
                     y=1-pix[,2],
                     value = normalizer(col2),
                     facet = "Std. dev. of the gene expressions")
D_cm <- data.frame(x = pix[,1],
                     y=1-pix[,2],
                     value = normalizer(CM),
                     facet = "Avg. correlation weighted by distance")

b1 =  plot_tissue_cont(D = D_mean,  palette = "oslo")
b2 =  plot_tissue_cont(D = D_cm,    palette = "oslo")
b3 =  plot_tissue_cont(D = D_sd,    palette = "oslo")

orig = load.image("Real_Data_Application/vis10x.png")

down <- resize(orig,round( width(orig)/2),
               round(height(orig)/2))

Dr <- as.data.frame(down) %>% filter(cc==1) %>% dplyr::select(-cc)
Dg <- as.data.frame(down) %>% filter(cc==2) %>% dplyr::select(-cc)
Db <- as.data.frame(down) %>% filter(cc==3) %>% dplyr::select(-cc)

D = Dr %>% rename(r = value) %>% mutate(g = Dg$value, b = Db$value)
kme <- kmeans(D[,3:5],10)
ind <- which.min(kme$centers[,1])
ind = 0
D_only <- D %>% mutate(cl = kme$cluster) %>% filter(cl != ind) %>% 
  mutate(col = rgb(r,g,b))
Fig = ggplot(D_only)+
  #  ylim(10,210)+
  theme_bw()+
  geom_tile(aes(x=x,y=y),fill=D_only$col)+ 
  scale_y_reverse()+
  facet_wrap(~"Annotated Visium Sample")+
  theme(#axis.title.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank())+
  theme(#axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())+
  theme(legend.position = "bottom", text=element_text(size = 20))+
  theme(legend.position = "none")+# +
  xlab("x coord.")+  ylab("y coord.")

M <- (b1+b3+b2+Fig+
        plot_layout(axis_titles = 'collect')) + 
  ggview::canvas(h=16,w=16)+
  scale_fill_brewer("")
M
ggsave("01_OUTPUT/APPLICATION/PLOT/Descr_v4.pdf",h=16,w=16)
ggsave("01_OUTPUT/APPLICATION/PLOT/Descr_v4.png",h=16,w=16)
ggsave("01_OUTPUT/APPLICATION/PLOT/Descr_v4.eps",h=16,w=16)







# Results clustering ------------------------------------------------------

re3 <- readRDS("~/SR-GMM/Real_Data_Application/01_OUTPUT/APPLICATION/RDS/best3.RDS")
re4 <- readRDS("~/SR-GMM/Real_Data_Application/01_OUTPUT/APPLICATION/RDS/best4.RDS")
re5 <- readRDS("~/SR-GMM/Real_Data_Application/01_OUTPUT/APPLICATION/RDS/best5.RDS")


#######################################################
# change accordingly
res <- re3
KK <- nrow(res$thetas)
#######################################################
plot(ts(res$logL))
plot(ts(diff(res$logL)))
######################################################
res$thetas
exp(res$thetas)
res$mu
exp(res$mu)
######################################################
library(SPAGHY)
S0 <- list()
for(k in 1:KK){
  S0[[k]] <- get_precision(res = res,ind = k,inverse = T)
  cat(k)
}


image(S0[[1]]$Prec[1:500,1:500])
image(S0[[2]]$Prec[1:500,1:500])
image(S0[[3]]$Prec[1:500,1:500])



Corrs <- lapply(S0, function(x) x$Corr)
mean_cor <- c()
for(kk in 1:KK){
  diag(Corrs[[kk]]) <- 0
  mean_cor <- 
    ########### CHANGE INDEX HERE
    rbind(mean_cor,
          cbind(apply(abs(as.matrix(Corrs[[kk]])),1,
                      function(x) median(x)),kk) )
    ###############################
}
mean_cor <- mean_cor %>% as.data.frame() %>% mutate(kk = paste("Cluster ",kk))

matplot(mean_cor[,1],type='l')
 
# scico::scico_palette_show(categorical = F)
ppix <- rbind(pix,pix,pix)

D <- data.frame(x = ppix[,1],
                y=1-ppix[,2],
                value = (mean_cor[,1]),
                facet = factor(mean_cor[,2]))

a1=  plot_tissue_cont(D = D, palette = "oslo")
a1+ggview::canvas(h=7,w=21)
ggsave("01_OUTPUT/APPLICATION/PLOT/Results1.pdf",h=7,w=21)
ggsave("01_OUTPUT/APPLICATION/PLOT/Results1.png",h=7,w=21)
ggsave("01_OUTPUT/APPLICATION/PLOT/Results1.eps",h=7,w=21)


plot_tissue_cont2 = function(D, 
                            palette = "davos", 
                            facet = NULL){
  
  G <- ggplot(D)+
    ggstar::geom_star(aes(x = x,
                          y = y,
                          fill = value),
                      color = 1 ,
                      starstroke = 0.01,
                      starshape = "hexagon", 
                      size = 2.7) +
    ggview::canvas(h=7,w=7)+
    scico::scale_fill_scico(TeX("$\\rho^{(k)}_i$"),palette = palette)+
    #  scale_fill_viridis_c(option = "A")+
    theme(legend.position = "bottom",text= element_text(size=20))+
    xlab("x coord.")+ylab("y coord.")+ facet_wrap(~facet) +
    theme(legend.key.width = unit(2, "cm"))
  G
}


a1 <- plot_tissue_cont2(D = D, palette = "oslo")
a1+ggview::canvas(h=8,w=21)

ggsave("01_OUTPUT/APPLICATION/PLOT/Results1v2.pdf",h=8,w=21)
ggsave("01_OUTPUT/APPLICATION/PLOT/Results1v2.png",h=8,w=21)
ggsave("01_OUTPUT/APPLICATION/PLOT/Results1v2.eps",h=8,w=21)



# stopped here ------------------------------------------------------------








Prec <- lapply(S0, function(x) x$Prec)
mean_pre <- c()
for(kk in 1:KK){
  diag(Prec[[kk]]) <- 0
  mean_pre <- 
    ########### CHANGE INDEX HERE
    rbind(mean_pre,
          cbind(apply(abs(as.matrix(Prec[[kk]])),1,
                      function(x) sum(x)),kk) )
  ###############################
}

matplot(mean_pre[,1],type='l')

# scico::scico_palette_show(categorical = F)


D <- data.frame(x = ppix[,1],
                y=1-ppix[,2],
                value = (mean_pre[,1]),
                facet = factor(mean_cor[,2]))

a1=  plot_tissue_cont(D = D, palette = "oslo")
a1
