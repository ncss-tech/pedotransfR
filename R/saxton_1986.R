#' Estimate Hydraulic Conductivity using Saxton et al. (1986)
#'
#' @param sand numeric; sand percentage
#' @param clay numeric; clay percentage
#' @param theta numeric; water content proportion
#'
#' @return numeric estimates of hydraulic conductivity (m/s) at specified water content (`theta`) given `sand` and `clay` percentages
#' 
#' @export
#' 
#' @aliases saxton_1986_theta_s saxton_1986_K saxton_1986_Ksat
#' @references Saxton, K.E., W.J. Rawls, J.S. Romberger, R.I. Papendick. Estimating generalized soil-water characteristics from texture. Soil Sci. Soc. Am. J. 50: 1031-1036.
#' @examples
#' 
#' # estimate saturated water content 80% sand, 10% clay
#' theta_s <- saxton_1986_theta_s(80, 10)
#' 
#' # estimate hydraulic conductivity at 50% porosity filled with water
#' saxton_1986_K(80, 10, theta = theta_s / 2)
#' 
#' # estimate hydraulic conductivity at saturation
#' saxton_1986_Ksat(80, 10)
#' 
saxton_1986_K <- function(sand, clay, theta) {
 
  if (any(theta > 1)) {
    theta[theta > 1] <- theta / 100
    message("some theta have values greater than one; scaling by factor of 0.01")
  }
  
  p <- 12.012
  q <- -7.55e-2
  r <- -3.8950
  tt <- 3.671e-2
  u <- -0.1103
  v <- 8.7546e-4
  
  2.778e-6 * exp(p + q*sand + (r + tt*sand + u*clay + v*(clay^2)) / theta)
}

#' @export
#' @rdname saxton_1986_K
saxton_1986_theta_s <- function(sand, clay) {
  h <- 0.332
  j <- -7.251e-4
  k <- 0.1276
  
  h + j * sand + k * log10(clay)
}

#' @export
#' @rdname saxton_1986_K
saxton_1986_Ksat <- function(sand, clay) {
  saxton_1986_K(sand, clay, theta = saxton_1986_theta_s(sand, clay))
}

#' Estimate Matric Water Potential using Saxton et al. (1986)
#'
#' @param sand numeric; sand percentage
#' @param clay numeric; clay percentage
#' @param theta numeric; water content proportion
#'
#' @return numeric estimates of matric water potential (kPa) at specified water content (`theta`) given `sand` and `clay` percentages
#' 
#' @export
#' 
#' @references Saxton, K.E., W.J. Rawls, J.S. Romberger, R.I. Papendick. Estimating generalized soil-water characteristics from texture. Soil Sci. Soc. Am. J. 50: 1031-1036.
#' @examples
#' 
#' theta <- seq(0.00, 1, 0.01)
#' plot(log10(saxton_1986_psi(80, 10, theta)) ~ theta,
#'      xlab = "Water Content (\u03b8)", ylab="Log10 Matric Potential (\u03a8, kPa)", 
#'      type="l", lty=2, xlim=c(0,1))
#' lines(log10(saxton_1986_psi(35, 40, theta)) ~ theta, lty=3)
#' legend("topright", legend=c("80% sand / 10% clay", "35% sand / 40% clay"), lty=2:3)
#' 
saxton_1986_psi <- function(sand, clay, theta) {
  m <- -0.108
  n <- 0.341
  
  # coefficients for water potential curve
  A <- .saxton_1986_A(sand, clay)
  B <- .saxton_1986_B(sand, clay)
  
  # water content at saturation
  theta_s <- saxton_1986_theta_s(sand, clay)
  
  # water content at 10kPa tension
  theta_10 <- exp((2.302 - log(A)) / B)
  
  # water potential at air entry
  psie <- 100 * (m + n * theta_s)
  res <- numeric(length(theta))

  # water potential over interval [1500, 10] kPa tension
  idx_1500to10 <- theta < theta_10
  res[idx_1500to10] <- A * theta[idx_1500to10] ^ B 
  
  # water potential over interval [10, air entry] kPa tension
  idx_10topsie <- theta >= theta_10
  res[idx_10topsie] <- 10 - (theta[idx_10topsie] - theta_10) * (10 - psie) / (theta_s - theta_10) 
  
  # water potential values for specified theta
  res
}

.saxton_1986_A <- function(sand, clay) {
  exp(-4.396 - 0.0715 * clay - 4.880e-4 * (sand^2) - 4.285e-5 * (sand^2) * clay) * 100.0
}

.saxton_1986_B <- function(sand, clay) {
  -3.140 - 0.00222 * (clay^2) - 3.484e-5 * (sand^2) * clay
}
