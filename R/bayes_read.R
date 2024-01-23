#' @title  Read probability maps
#' @name   bayes_read_image
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param  file    Full path to raster multi-band file
#'                       containing probability matrices
#' @param  labels        Labels to be assigned to the bands
#' @return A SpatRaster object
#' @export
bayes_read_image <- function(red_file, green_file, blue_file){
    # read the probability file into a SpatRaster
    x <- terra::rast(c(red_file, green_file, blue_file))
    # associate the labels to the names of the SpatRaster
    return(x)
}
#' @title  Read probability maps
#' @name   bayes_read_probs
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param  probs_file    Full path to raster multi-band file
#'                       containing probability matrices
#' @param  labels        Labels to be assigned to the bands
#' @return A SpatRaster object
#' @examples
#' {
#' # Define location of a probability file
#' data_dir <- system.file("/extdata/Rondonia-20LLQ/probs", package = "bayesEO")
#' # list the file
#' file <- list.files(data_dir)
#' # build the full path
#' probs_file <- paste0(data_dir, "/", file)
#' # define labels
#' labels <- c("Water", "ClearCut_Burn", "ClearCut_Soil",
#'              "ClearCut_Veg", "Forest", "Wetland")
#'
#' probs_image <- bayes_read_probs(probs_file, labels)
#' }
#' @export
bayes_read_probs <- function(probs_file, labels){
    # read the probability file into a SpatRaster
    x <- terra::rast(probs_file)
    # associate the labels to the names of the SpatRaster
    names(x) <- labels
    return(x)
}
