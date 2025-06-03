
library(tidyverse)
library(scico)
library(ggview)
library(NPVecchia)
library(srgmm)
library(invgamma)
library(MASS)
library(fields)

data <- readRDS("00_DATA/APPLICATION/data.RDS")
load("00_DATA/APPLICATION/prostate_cancer.Rdata")
load("00_DATA/APPLICATION/results_prostate_500genes_4K.Rdata")
clust <- Ds[data$order]
dat = data$datum.ord
pix = data$locs.ord

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

#plot_tissue_discr(pix = pix,clust,palette = "imola")
#plot_tissue_discr(pix,colMeans(dat),palette = "imola")

# scico_palette_show(categorical = T)



get_precision = function(res, ind, inverse = FALSE){
  U_3 <- get_map(res$thetas[ind,], 
                 datum = res$data$datum.ord[res$Z == ind,], 
                 NNarray = res$data$NNarray)
  # pay attention that the diagonal contains sqrt(solve(D)), thus 1/sqrt(d_i)
  invS3 <- U_3 %*% t(U_3)  # precision matrix of cluster 1
  
  if(inverse){
    S3 <- solve(t(U_3))%*%solve(U_3)
    C3 <- cov2cor(as.matrix(S3))
  }else{
    S3 <- C3 <- NULL
  }
  
  return(list(U_chol = U_3, 
              Prec = invS3, 
              Var = S3, 
              Corr = C3))
}

