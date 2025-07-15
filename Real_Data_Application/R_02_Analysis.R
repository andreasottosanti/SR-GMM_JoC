rm(list = ls())
library(srgmm)
library(NPVecchia)
library(Matrix)
library(fields)
library(ggplot2)

res <- readRDS("~/SR-GMM/Real_Data_Application/01_OUTPUT/APPLICATION/RDS/best3.RDS")


# Comparison with k-means -------------------------------------------------
set.seed(123)
res_kmeans <- kmeans(res$data$datum.ord, centers = 3)
xtable::xtable(table(srgmm = res$Z, kmeans = res_kmeans$cluster))


# Computing the connectivity ----------------------------------------------
res <- rho_k(x = res, k = 1:3, compute_covariance = T)
saveRDS(res, file = "~/SR-GMM/Real_Data_Application/01_OUTPUT/APPLICATION/RDS/best3.RDS")


dd <- data.frame(x = rep(res$data$locs.ord[,1],3),
                 y = 1-rep(res$data$locs.ord[,2],3),
                 rho = as.vector(t(res$data$rho_statistic)), 
                 cluster = as.vector(sapply(1:3, function(x) rep(paste("Cluster",x), ncol(res$data$rho_statistic)))))
dd$cluster <- as.factor(dd$cluster)
ggplot(dd, aes(x = x, y = y, col = rho))+ggstar::geom_star(aes(x = x,
                                                               y = y,
                                                               fill = rho),
                                                           color = 1 ,
                                                           starstroke = 0.01,
                                                           starshape = "hexagon", 
                                                           size = 2.7)+theme_bw()+
  facet_wrap(~cluster)+
  scico::scale_fill_scico(expression(rho[i]^(k)),palette = "oslo")+
  labs(x = "x coord.", y = "y coord.", col = expression(rho[i]^(k)))+
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 18, hjust = .5),
        strip.text = element_text(size = 18), 
        legend.key.width = unit(2, "cm"),
        legend.position = "bottom",
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18))


ggsave(filename = "~/SR-GMM/Real_Data_Application/01_OUTPUT/APPLICATION/PLOT/spatial_connection.pdf", 
       width = 21, height = 8)
