library(ggplot2)
library(patchwork)
library(dplyr)

res <- readRDS("Real_Data_Application/01_OUTPUT/APPLICATION/RDS/best3.RDS")
d <- t(res$data$datum.ord)
val <- data.frame(values = as.vector(d), 
                  cluster = as.vector(sapply(res$Z[,1], function(s) rep(s, nrow(d)))),
                  gene = as.vector(unlist(sapply(1:nrow(res$data$datum.ord), function(s) rep(s, nrow(d))))))
val$cluster <- factor(val$cluster, levels = c("1","2","3"))
val$gene_group <- cut(val$gene, c(0,100,200,300,400,500))
levels(val$gene_group) <- c("Genes 1-100","Genes 101-200","Genes 201-300","Genes 301-400","Genes 401-500")
val$gene <- factor(val$gene, levels = 1:nrow(res$data$datum.ord))


# divided in groups -------------------------------------------------------
group_id <- 1:500 
ggplot(val[val$gene %in% group_id,], aes(x = as.factor(rep(rep(1:100, each = nrow(res$data$locs.ord)),5)), y = values))+geom_boxplot()+theme_bw()+
  labs(x = NULL,y = "Deviance residuals")+facet_wrap(~gene_group)+
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 18, hjust = .5),
        strip.text = element_text(size = 18))
#ggsave(Real_Data_Application/01_OUTPUT/APPLICATION/PLOT/genes_boxplot.pdf", width = 12, height = 9)


# all combined ------------------------------------------------------------
aggregate_means <- val %>%
  group_by(gene) %>%
  summarise(mean_value = mean(values, na.rm = TRUE))
ggplot(val, aes(x = gene, y = values))+geom_boxplot()+theme_bw()+
  labs(x = "Genes",y = "Deviance residuals")+
  geom_hline(yintercept = 0, col = 2, lty = 2)+
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 18, hjust = .5),
        strip.text = element_text(size = 18))+
  geom_line(mapping = aes(x = gene, y = mean_value, group = 1), data = aggregate_means, col = "green")
ggsave("Real_Data_Application/01_OUTPUT/APPLICATION/PLOT/genes_boxplot.pdf", width = 15, height = 9)

