rm(list = ls())
library(fossil)
library(ggplot2)
library(mclust)

directories <- list.dirs(path = dirname(rstudioapi::getSourceEditorContext()$path), full.names = FALSE, recursive = FALSE)
final_plots <- list()
Comp_time <- data.frame(comp_time = numeric(), simulation_name = character(), setup = character())

for(k in 1:length(directories)){
  setup <- directories[k]
  gg_graphs <- list()
  N_experiments <- length(dir(paste(dirname(rstudioapi::getSourceEditorContext()$path), setup, "Results", sep="/")))
  names_experiments <- dir(paste(dirname(rstudioapi::getSourceEditorContext()$path), setup, "Results", sep="/"))
  for(i in 1:N_experiments){
    print(i)
    simulation_name <- dir(paste(dirname(rstudioapi::getSourceEditorContext()$path), setup, "Results", sep="/"))[i]
    run_per_simulation <- length(dir(paste(dirname(rstudioapi::getSourceEditorContext()$path), setup, "Results", simulation_name, sep="/")))
    dir_data <- paste(dirname(rstudioapi::getSourceEditorContext()$path), setup, "Data", simulation_name, sep="/")
    dir_results <- paste(dirname(rstudioapi::getSourceEditorContext()$path), setup, "Results", simulation_name, sep="/")
    Rand_values <- Rand_mixture <- Rand_kmeans <- numeric(run_per_simulation)
    for(j in 1:run_per_simulation){
      # load data
      load(paste(dir_data, "/Data", j, ".Rdata", sep = ""))
      # load results
      load(paste(dir_results, "/Results", j, ".Rdata", sep = ""))
      Comp_time <- rbind(Comp_time,
                         data.frame(comp_time = unlist(lapply(results, function(l) l$time)),
                                    tot_iter = unlist(lapply(results, function(l) length(l$logL)+1)),
                                    simulation_name = rep(simulation_name, length(results)),
                                    setup = rep(setup, length(results))))
      Rands <- unlist(lapply(results, function(l) rand.index(l$Z, z)))
      ll_val <- unlist(lapply(results, function(l) l$max.logL))
      Rand_values[j] <- Rands[which.max(ll_val)]
      Rand_mixture[j] <- rand.index(z, Mclust(y, G = 3)$classification)
      Rand_kmeans[j] <- rand.index(z, kmeans(y, centers = 3, nstart = 20)$cluster)
    }
    if(i == 1) Rand_simulations <- Rand_mixture_simulations <- Rand_kmeans_simulations <- matrix(0, run_per_simulation, N_experiments)
    Rand_simulations[,i] <- Rand_values
    Rand_mixture_simulations[,i] <- Rand_mixture
    Rand_kmeans_simulations[,i] <- Rand_kmeans
  }
  Rand_simulations <- data.frame(Rand_simulations)
  Rand_mixture_simulations <- data.frame(Rand_mixture_simulations)
  Rand_kmeans_simulations <- data.frame(Rand_kmeans_simulations)
  names(Rand_simulations) <- names(Rand_mixture_simulations) <- names(Rand_kmeans_simulations) <- names_experiments


  # ggplot ------------------------------------------------------------------

  cRands <- rbind(Rand_simulations, Rand_mixture_simulations, Rand_kmeans_simulations)
  values <- data.frame(Rands = as.vector(as.matrix(cRands)),
                       model = rep(rep(c("SR-GMM","GMM","k-means"), each = nrow(Rand_simulations)), ncol(Rand_simulations)),
                       setup = rep(names(Rand_simulations), each = nrow(cRands)))
  values$model <- as.factor(values$model)

  transform_labels <- function(label, signal = 2, noise_to_signal = F){
    # get the two noise levels and convert the split strings to numeric
    numbers <- strsplit(label, "_")[[1]]
    numbers_vector <- as.numeric(numbers)

    new_label <- paste("Variances: (", paste(numbers_vector[1], numbers_vector[2], sep = ", "),")",sep="")
    new_label
  }
  values$setup_relabelled <- factor(sapply(values$setup, function(x) transform_labels(x, noise_to_signal = F)))

  final_plots[[k]] <- ggplot(values, aes(model, Rands))+geom_boxplot()+
    theme_bw()+facet_wrap(~setup_relabelled)+labs(x = "", y = "Rand index")+
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 18),
          strip.text = element_text(size=18), plot.title = element_text(hjust = 0.5), title = element_text(size = 18))+
    ggtitle(paste("Cluster sizes: (", paste(strsplit(setup, "_")[[1]], collapse = ", "), ")", sep=""))

  # save graphs
  if(!("Graphs" %in% dir(paste((dirname(rstudioapi::getSourceEditorContext()$path)),setup,sep="/")))){
    dir.create(paste(dirname(rstudioapi::getSourceEditorContext()$path),setup,"Graphs",sep="/"))}

  ggsave(filename = paste(dirname(rstudioapi::getSourceEditorContext()$path), "/", setup, "/Graphs/Rand_",setup,".pdf",sep=""),
         plot = final_plots[[k]], width = 12, height = 9)

}


# ggplot computational times ------------------------------------------------------------------
Comp_time$simulation_name <- factor(Comp_time$simulation_name)
Comp_time$setup <- factor(Comp_time$setup)
levels(Comp_time$simulation_name) <- sapply(levels(Comp_time$simulation_name), function(x) transform_labels(x, noise_to_signal = F))
levels(Comp_time$setup) <- sapply(levels(Comp_time$setup), function(x) paste("(", paste(strsplit(x, "_")[[1]], collapse = ", "), ")", sep=""))

runtime_plot <- ggplot(Comp_time, aes(x = setup, y = comp_time))+geom_boxplot()+
  theme_bw()+facet_wrap(~simulation_name)+
  labs(x = "Cluster sizes", y = "Runtime (in sec.)")+
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        strip.text = element_text(size=18), plot.title = element_text(hjust = 0.5), title = element_text(size = 18))
runtime_plot_std <- ggplot(Comp_time, aes(x = setup, y = comp_time/(tot_iter+1)))+geom_boxplot()+
  theme_bw()+facet_wrap(~simulation_name)+
  labs(x = "Cluster sizes", y = "Average runtime per iteration (in sec.)")+
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        strip.text = element_text(size=18), plot.title = element_text(hjust = 0.5), title = element_text(size = 18))
n_iter_plot <- ggplot(Comp_time, aes(x = setup, y = tot_iter+1))+geom_boxplot()+
  theme_bw()+facet_wrap(~simulation_name)+
  labs(x = "Cluster sizes", y = "N. of iterations")+
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        strip.text = element_text(size=18), plot.title = element_text(hjust = 0.5), title = element_text(size = 18))



#ggsave(filename = paste(dirname(rstudioapi::getSourceEditorContext()$path), "/Runtime_variance",".pdf",sep=""),
#       plot = runtime_plot, width = 12, height = 9)
ggsave(filename = paste(dirname(rstudioapi::getSourceEditorContext()$path), "/Runtime_per_iter_variance",".pdf",sep=""),
       plot = runtime_plot_std, width = 12, height = 9)
ggsave(filename = paste(dirname(rstudioapi::getSourceEditorContext()$path), "/N_iter_variance",".pdf",sep=""),
       plot = n_iter_plot, width = 12, height = 9)
