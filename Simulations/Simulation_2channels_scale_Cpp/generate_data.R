rm(list = ls())
set.seed(123)
working_directory <- dirname(rstudioapi::getSourceEditorContext()$path)

n <- c(100, 100)   # number of observations per cluster
p <- 500

simulation_type <- paste(n[1], n[2], sep="_") # options can be "100_100" or "150_50"
scale_values <- c(0.1, 1, 3)  # the scales considered
scale_values_grid <- expand.grid(scale_values, scale_values)
tau <- 2      # the spatial variance of each cluster
xi <- 0       # the nugget effect of each cluster
N_data_per_simulation <- 10


# if the directory does not exist, create it
if(!(simulation_type %in% dir(working_directory))){
  dir.create(path = paste(working_directory, simulation_type, sep = "/"))
  dir.create(path = paste(working_directory, simulation_type, "Data", sep = "/"))
}
working_directory <- paste(working_directory, simulation_type, "Data", sep = "/")


for(i in 1:nrow(scale_values_grid)){
  combination_type <- paste(scale_values_grid[i,1], scale_values_grid[i,2], sep="_")

  # if the directory does not exist, create it
  if(!(combination_type %in% dir(working_directory))) dir.create(path = paste(working_directory, combination_type, sep = "/"))

  # generate the data
  for(j in 1:N_data_per_simulation){
    C <- matrix(runif(2*p, 0, 10), p, 2)   # generate the coordinates

    # Z = 1
    label1 <- paste("expo, tau=",tau,", phi=",scale_values_grid[i,1],", xi=",xi,sep = "")
    K1 <- tau * exp(-as.matrix(dist(C))/scale_values_grid[i,1]) + diag(xi, p)
    row.names(K1) <- colnames(K1) <- NULL
    y1 <- mvtnorm::rmvnorm(n = n[1], sigma = K1)

    # Z = 2
    label2 <- paste("gaus, tau=",tau,", phi=", scale_values_grid[i,2],", xi=",xi,sep = "")
    K2 <- tau * exp(-as.matrix(dist(C))^2/scale_values_grid[i,2]^2) + diag(xi, p)
    row.names(K2) <- colnames(K2) <- NULL
    y2 <- mvtnorm::rmvnorm(n = n[2], sigma = K2)

    y <- rbind(y1, y2)
    z <- c(rep(1,n[1]), rep(2,n[2]))

    save(y, C, z, label1, label2, file = paste(working_directory, "/", combination_type, "/", "Data", j, ".Rdata", sep = ""))

  }

}
