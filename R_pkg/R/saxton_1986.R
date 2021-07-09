#' @export
#' @rdname saxton_1986_K
saxton_1986_theta_s <- function(sand, clay) {
  h <- 0.332
  j <- -7.251e-4
  k <- 0.1276
  
  h + j * sand + k * log10(clay)
}

#' Estimate Hydraulic Conductivity using Saxton et al. (1986)
#'
#' @param sand _numeric_; sand percentage
#' @param silt _numeric_; silt percentage
#' @param clay _numeric_; clay percentage
#' @param theta _numeric_; water content percentage
#'
#' @return _numeric_ estimates of hydraulic conductivity at specified water content given sand, silt and clay percentages
#' @export
#' @aliases saxton_1986_theta_s saxton_1986_K saxton_1986_Ksat
#' @references Saxton, K.E., W.J. Rawls, J.S. Romberger, R.I. Papendick. Estimating generalized soil-water characteristics from texture. Soil Sci. Soc. Am. J. 50: 1031-1036.
#' @examples
#' # estimate saturated water content 80% sand, 10% clay
#' theta_s <- saxton_1986_theta_s(80, 10)
#' 
#' # estimate hydraulic conductivity at 50% porosity filled with water
#' saxton_1986_K(80, 10, 10, theta = theta_s / 2)
#' 
#' # estimate hydraulic conductivity at saturation
#' saxton_1986_Ksat(80, 10, 10)
#' 
saxton_1986_K <- function(sand, silt, clay, theta) {
 
  if (theta > 1) 
    theta <- theta / 100
  
  m <- -0.108
  n <- 0.341
  
  p <- 12.012
  q <- -7.55e-2
  r <- -3.8950
  tt <- 3.671e-2
  u <- -0.1103
  v <- 8.7546e-4
  
  A <- exp(-4.396 - 0.0715 * clay - 4.880e-4 * (sand^2) - 4.285e-5 * (sand^2) * clay) * 100.0
  B <- -3.140 - 0.00222 * (clay^2) - 3.484e-5 * (sand^2) * clay
  
  psi_1500to10 <- A * theta ^ B
  theta_10 <- exp((2.302 - log(A)) / B)
  theta_s <- saxton_1986_thetas(sand, clay)
  psie <- 100 * (m + n * theta_s)
  psi_10topsie <- 10 - (theta - theta_10) * (10 - psie) / (theta_s - theta_10)
  
  2.778e-6 * exp(p + q*sand + (r + tt*sand + u*clay + v*(clay^2)) / theta)
}

#' @export
#' @rdname saxton_1986_K
saxton_1986_Ksat <- function(sand, silt, clay) {
  saxton_1986_K(sand, silt, clay, theta = saxton_1986_thetas(sand, clay))
}

