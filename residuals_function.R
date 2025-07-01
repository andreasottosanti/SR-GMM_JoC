#' Residuals from a SR-GMM 
#' 
#' This function computes the residuals of the data based on the estimated SR-GMM.
#'
#' @param x an object of class \code{srgmm}
#'
#' @return An object of class \code{srgmm}, identical to \code{x} except that the \code{$data} element now also includes a matrix named \code{resid}, with the same dimensions as \code{datum.ord}, containing the estimated residuals.
#' @details
#' For every observation \eqn{\mathbf{y}_{\ell,.}}, if \eqn{\mathrm{Z}_{\ell,k}=1} (the observation has been assigned to the k-th cluster), then the vector of residuals is computed as
#' \deqn{\mathbf{r}_{\ell,.}=\mathbf{U}^{(k)}{\mathbf{D}^{(k)}}^{-1/2}(\mathbf{y}_{\ell,.}-\mu_k\mathbf{1})}
#' 
#' @export
#' 
residuals.srgmm <- function(x){
  x$data$resid <- x$data$datum.ord
  K <- nrow(x$thetas)
  for(k in 1:K){
    x$data$resid[x$Z == k,] <- as.matrix((x$data$resid[x$Z == k,] - x$mu[k]) %*%
                                           get_map(thetas = x$thetas[k,], datum = x$data$datum.ord[x$Z == k,], NNarray = x$data$NNarray, threshh = x$data$threshh))
  }
  return(x)
}


