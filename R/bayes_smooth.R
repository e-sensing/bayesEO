#' @title Smooth probability images
#'
#' @name  bayes_smooth
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a classified image with probabilities, and reduces outliers
#' and smoothens probability according to Bayesian statistics
#'
#' @param  x                 SpatRaster object with probabilities images
#' @param  window_size       Size of the neighborhood.
#' @param  neigh_fraction    Fraction of neighbors with high probabilities
#'                           to be used in Bayesian inference.
#' @param  smoothness        Estimated variance of logit of class probabilities
#'                           (Bayesian smoothing parameter). It can be either
#'                           a vector or a scalar.
#'
#' @return A SpatRaster object
#'
#' @examples
#' if (bayes_run_examples()) {
#'     # select a file with probability values
#'     data_dir <- system.file("/extdata/probs/", package = "bayesEO")
#'     file <- list.files(data_dir)
#'     # create a full path for the file
#'     probs_file <- paste0(data_dir, "/", file)
#'     # provide the labels
#'     labels <- c("Water", "ClearCut_Burn", "ClearCut_Soil",
#'              "ClearCut_Veg", "Forest", "Wetland")
#'     # read the probs file
#'     probs <- bayes_read(probs_file, labels)
#'     # smooth the probability image
#'     probs_smooth <- bayes_smooth(probs,
#'             window_size = 7,
#'             smoothness = 20
#'     )
#'     # plot the probability image
#'     bayes_plot(probs_smooth, scale = 0.0001)
#'}
#'
#' @export
bayes_smooth <- function(x,
                         window_size = 7,
                         neigh_fraction = 0.5,
                         smoothness = 10){
    # check input image
    .check_that(
        "SpatRaster" %in% class(x),
        msg = "input is not SpatRaster type"
    )
    labels <- names(x)
    # Check window size
    .check_window_size(window_size, min = 5)
    # Check neigh_fraction
    .check_num_parameter(neigh_fraction, exclusive_min = 0, max = 1)
    # Check smoothness
    .check_smoothness(smoothness, length(labels))
    # Prepare smoothness parameter
    if (length(smoothness == 1)) {
        smoothness <- rep(smoothness, length(labels))
    }
    # read values
    values <- terra::values(x)
    # deduce scale
    scale <- 1/max(values)
    # scale values
    values <- values * scale
    # avoid zero
    values[values < 0.0001] <- 0.0001
    # avoid one
    values[values > 0.9999] <- 0.9999
    # Compute logit
    values <- log(values / (rowSums(values) - values))
    # Process Bayesian
    values <- bayes_smoother_fraction(
        logits = values,
        nrows  = terra::nrow(x),
        ncols  = terra::ncol(x),
        window_size = window_size,
        smoothness = smoothness,
        neigh_fraction = neigh_fraction
    )
    # Compute inverse logit
    values <- exp(values) / (exp(values) + 1)
    # rescale the data
    values <- values / scale
    # create SpatRaster
    y <- terra::rast(x)
    terra::values(y) <- values
    names(y) <- names(x)
    return(y)
}
#' @title Smooth probability images with Gaussian filter
#'
#' @name  gaussian_smooth
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a classified image with probabilities, and reduces outliers
#' and smoothens probability according to a Gaussian filter
#'
#' @param  x                 SpatRaster object with probabilities images
#' @param  window_size       Size of the neighborhood.
#' @param  neigh_fraction    Fraction of neighbors with high probabilities
#'                           to be used in Bayesian inference.
#' @param  sigma             Standard deviation of the spatial Gaussian kernel
#'
#' @return A SpatRaster object
#'
#' @examples
#' if (bayes_run_examples()) {
#'     # select a file with probability values
#'     data_dir <- system.file("/extdata/probs/", package = "bayesEO")
#'     file <- list.files(data_dir)
#'     # create a full path for the file
#'     probs_file <- paste0(data_dir, "/", file)
#'     # provide the labels
#'     labels <- c("Water", "ClearCut_Burn", "ClearCut_Soil",
#'              "ClearCut_Veg", "Forest", "Wetland")
#'     # read the probs file
#'     probs <- bayes_read(probs_file, labels)
#'     # smooth the probability image
#'     gauss <- gaussian_smooth(probs,
#'             window_size = 5,
#'             sigma = 1
#'     )
#'     # plot the probability image
#'     bayes_plot(gauss, scale = 0.0001)
#'}
#'
#' @export
gaussian_smooth <- function(x,
                         window_size = 5,
                         sigma = 1){
    # check input image
    .check_that(
        "SpatRaster" %in% class(x),
        msg = "input is not SpatRaster type"
    )
    labels <- names(x)
    # Check window size
    .check_window_size(window_size, min = 5)
    # Check neigh_fraction
    .check_num_parameter(sigma, min = 1)
    # read values
    values <- terra::values(x)
    # deduce scale
    scale <- 1/max(values)
    # scale values
    values <- values * scale
    # avoid zero
    values[values < 0.0001] <- 0.0001
    # avoid one
    values[values > 0.9999] <- 0.9999
    # create output window
    gauss_kernel <- .smooth_gauss_kernel(window_size = window_size,
                                         sigma = sigma)
    # process Gaussian smoother
    values <- kernel_smoother(m = values,
                            m_nrow = nrow(x),
                            m_ncol = ncol(x),
                            w = gauss_kernel,
                            normalised = TRUE)
    # rescale the data
    values <- values / scale
    # create SpatRaster
    y <- terra::rast(x)
    terra::values(y) <- values
    names(y) <- names(x)
    return(y)
}
#' @title Smooth probability images with Gaussian filter
#'
#' @name  bilateral_smooth
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a classified image with probabilities, and reduces outliers
#' and smoothens probability according to a Gaussian filter
#'
#' @param  x                 SpatRaster object with probabilities images
#' @param  window_size       Size of the neighborhood.
#' @param  neigh_fraction    Fraction of neighbors with high probabilities
#'                           to be used in Bayesian inference.
#' @param  sigma             Standard deviation of the spatial Gaussian kernel
#' @param  tau               Standard deviation of the class probs value
#'
#'
#' @return A SpatRaster object
#'
#' @examples
#' if (bayes_run_examples()) {
#'     # select a file with probability values
#'     data_dir <- system.file("/extdata/probs/", package = "bayesEO")
#'     file <- list.files(data_dir)
#'     # create a full path for the file
#'     probs_file <- paste0(data_dir, "/", file)
#'     # provide the labels
#'     labels <- c("Water", "ClearCut_Burn", "ClearCut_Soil",
#'              "ClearCut_Veg", "Forest", "Wetland")
#'     # read the probs file
#'     probs <- bayes_read(probs_file, labels)
#'     # smooth the probability image
#'     bilat <- bilateral_smooth(probs,
#'                               window_size = 5,
#'                               sigma = 8,
#'                               tau = 0.1
#'     )
#'     # plot the probability image
#'     bayes_plot(bilat, scale = 0.0001)
#'}
#'
#' @export
bilateral_smooth <- function(x,
                            window_size = 5,
                            sigma = 8,
                            tau = 0.1){
    # check input image
    .check_that(
        "SpatRaster" %in% class(x),
        msg = "input is not SpatRaster type"
    )
    labels <- names(x)
    # Check window size
    .check_window_size(window_size, min = 5)
    # Check neigh_fraction
    .check_num_parameter(sigma, min = 1)
    # read values
    values <- terra::values(x)
    # deduce scale
    scale <- 1/max(values)
    # scale values
    values <- values * scale
    # avoid zero
    values[values < 0.0001] <- 0.0001
    # avoid one
    values[values > 0.9999] <- 0.9999
    # create output window
    gauss_kernel <- .smooth_gauss_kernel(window_size = window_size,
                                         sigma = sigma)
    # process Gaussian smoother
    values <- bilateral_smoother(m = values,
                                 m_nrow = nrow(x),
                                 m_ncol = ncol(x),
                                 w = gauss_kernel,
                                 tau = tau)
    # rescale the data
    values <- values / scale
    # create SpatRaster
    y <- terra::rast(x)
    terra::values(y) <- values
    names(y) <- names(x)
    return(y)
}
#' @title Compute the 2-D Gaussian kernel
#' @name .smooth_gauss_kernel
#' @keywords internal
#'
#' @param window_size   Size of the neighbourhood.
#' @param sigma         Standard deviation of the spatial Gaussian kernel
#'
#' @return  returns a squared matrix filled with Gaussian function
#'
.smooth_gauss_kernel <- function(window_size, sigma) {

    stopifnot(window_size %% 2 != 0)

    w_center <- ceiling(window_size / 2)
    w_seq <- seq_len(window_size)
    x <- stats::dnorm(
        (abs(rep(w_seq, each = window_size) - w_center) ^ 2 +
             abs(rep(w_seq, window_size) - w_center) ^ 2) ^ (1 / 2),
        sd = sigma) / stats::dnorm(0)
    matrix(x / sum(x), nrow = window_size, byrow = T)
}
