library(ggplot2)
library(patchwork)

re3 <- readRDS("Real_Data_Application/01_OUTPUT/APPLICATION/RDS/K3_10runs.RDS")
re4 <- readRDS("Real_Data_Application/01_OUTPUT/APPLICATION/RDS/K4_10runs.RDS")
re5 <- readRDS("Real_Data_Application/01_OUTPUT/APPLICATION/RDS/K5_10runs.RDS")

times_r3 <- matrix(unlist(lapply(re3, function(x) return(c(x$time, length(x$logL))))), ncol = 2, byrow = T)
times_r4 <- matrix(unlist(lapply(re4, function(x) return(c(x$time, length(x$logL))))), ncol = 2, byrow = T)
times_r5 <- matrix(unlist(lapply(re5, function(x) return(c(x$time, length(x$logL))))), ncol = 2, byrow = T)

gg <- data.frame(rbind(times_r3, times_r4, times_r5), model = as.vector(sapply(3:5, function(s) rep(paste("K = ",s,sep=""), length(re3)))))
names(gg)[1:2] <- c("time","tot.iter")
plots <- list()
plots[[1]] <- ggplot(gg, aes(x = model, y = time/(tot.iter+1)))+geom_boxplot()+theme_bw()+
  labs(x = NULL, y = "Average runtime per iteration (in sec.)")+
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 18))+ylim(c(200,300))

plots[[2]] <- ggplot(gg, aes(x = model, y = (tot.iter+1)))+geom_boxplot()+theme_bw()+
  labs(x = NULL, y = "N. of iterations")+
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 18))

plots[[1]]+plots[[2]]

ggsave("Real_Data_Application/01_OUTPUT/APPLICATION/PLOT/Comp_time_application.pdf", width = 12, height = 9)
ggsave("Real_Data_Application/01_OUTPUT/APPLICATION/PLOT/Comp_time_application.png", width = 12, height = 9)
ggsave("Real_Data_Application/01_OUTPUT/APPLICATION/PLOT/Comp_time_application.eps", width = 12, height = 9)
