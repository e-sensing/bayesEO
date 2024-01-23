#' @title  Read probability maps
#' @name   bayes_read_image
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param  files    Full path to raster files
#' @return A SpatRaster object
#' @examples
#' if (bayes_run_examples()) {
#' # Define location of a probability file
#' data_dir <- system.file("/extdata/rgb", package = "bayesEO")
#' # list the file
#' files <- list.files(data_dir)
#' # build the full path
#' image_files <- paste0(data_dir, "/", files)
#' rgb_image <- bayes_read_image(image_files)
#' }
#' @export
bayes_read_image <- function(files){
    # read the probability file into a SpatRaster
    x <- terra::rast(files)
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
#' if (bayes_run_examples()) {
#' # Define location of a probability file
#' data_dir <- system.file("/extdata/probs", package = "bayesEO")
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
