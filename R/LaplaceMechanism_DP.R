#' @title Random laplace generator
#' 
#' @description Extracted from https://rpubs.com/mengxu/gaussian-and-laplace-noise
#'
#' @param n \code{integer} (default \code{1}) Number of samples to generate
#' @param m \code{integer} (default \code{0}) Mean of Laplace distribution
#' @param s \code{integer} (default \code{1}) Standard deviation of Laplace distribution
#'
#' @return Vector of numbers
#' @export

rlaplace <- function (n = 1, m = 0, s = 1){
  if (any(s <= 0)) 
    stop("s must be positive")
  q <- stats::runif(n)
  ifelse(q < 0.5, s * log(2 * q) + m, -s * log(2 * (1 - q)) + m)
}

#' @title Random laplace distribution generator
#' 
#' @description Extracted from https://rpubs.com/mengxu/gaussian-and-laplace-noise
#' 
#' @param y \code{numeric} Samples
#' @param m \code{integer} (default \code{0}) Mean of Laplace distribution
#' @param s \code{integer} (default \code{1}) Standard deviation of Laplace distribution
#' @param log \code{bool} (default \code{FALSE}) Apply \code{exp} transformation to the distribution
#'
#' @return Vector of numbers
#' @export

dlaplace <- function (y, m = 0, s = 1, log = FALSE) {
  if (any(s <= 0)) 
    stop("s must be positive")
  density <- -abs(y - m)/s - log(2 * s)
  if (!log) 
    density <- exp(density)
  density
}

#' @title Random laplace generator
#' 
#' @description Extracted from https://rpubs.com/mengxu/gaussian-and-laplace-noise
#' 
#' @param mu \code{integer} Mean of Laplace distribution
#' @param b \code{integer} Standard deviation of Laplace distribution
#' @param n.noise \code{integer} Number of samples to generate
#'
#' @return Vector of numbers
#' @export

Laplace_noise_generator <- function(mu, b, n.noise){
  # generate n noise points
  noise.points <- rlaplace(n = n.noise, m = mu, s = b)
  noise.density <- dlaplace(noise.points, m = mu, s = b)
  
  return(noise = noise.points)
}

#' Differential Privacy Implementation
#'
#' Applies differential privacy to a given dataset by adding Laplace noise.
#' The function supports both numeric vectors and matrices.
#'
#' @param res A list containing the original value (`og_value`) and 
#'   the L1 sensitivity (`l1_sens`). For numeric vectors, `l1_sens` should be a single value. 
#'   For matrices, `l1_sens` should be a vector with L1 sensitivities for each cell 
#'   of the matrix. The `og_value` can be either a numeric vector or a matrix.
#' @param epsilon The privacy budget parameter. A non-zero value 
#'   specifies the degree of noise to be added for privacy. 
#'   Setting the value to NULL means no noise is added.
#'
#' @details 
#' This function implements differential privacy by adding Laplace noise to 
#' the original data (`og_value`). The amount of noise depends on the L1 
#' sensitivity (`l1_sens`) of the query and the privacy budget (`epsilon`).
#' For numeric vectors, a single noise value is generated and added. 
#' For matrices, individual noise values are generated for each cell based on 
#' the provided `l1_sens` vector and added to the corresponding cells. 
#' The function uses `Laplace_noise_generator` to generate the required noise.
#'
#' @return 
#' The function returns the data with Laplace noise added for differential privacy. 
#' The structure of the returned data (matrix or numeric vector) matches that of the 
#' input `og_value`.
#'
#' @examples
#' # Example usage for a numeric vector
#' res <- list(og_value = c(1, 2, 3), l1_sens = 1)
#' differential_privacy(res, 0.5)
#'
#' # Example usage for a matrix
#' matrix_sensitivities <- rep(2, 4) # Replace with actual sensitivities
#' res <- list(og_value = matrix(c(1, 2, 3, 4), nrow = 2), l1_sens = matrix_sensitivities)
#' differential_privacy(res, 0.5)
#' @export
differential_privacy <- function(res, epsilon) {
  freq_sens <- res$l1_sens
  data <- res$og_value
  if (is.null(epsilon)==FALSE) {
    if (inherits(data, "matrix")) {
      laplace_noise <- lapply(freq_sens, function(x){
        if(x != 0) {
          Laplace_noise_generator(
            m = 0,
            b = x/epsilon,
            n.noise = 1
          )
        } else {0}
      }) |> unlist()
      data <- data + laplace_noise
    } else if (inherits(data, "numeric")) {
      if(freq_sens != 0) {
        data <- data + Laplace_noise_generator(
          m = 0,
          b = freq_sens/epsilon,
          n.noise = 1
        )
      }
    }
  }
  return(data)
}






