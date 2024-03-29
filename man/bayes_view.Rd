% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bayes_view.R
\name{bayes_view}
\alias{bayes_view}
\title{Include leaflet to view images (BW or RGB)}
\usage{
bayes_view(
  image,
  map,
  red,
  green,
  blue,
  legend = NULL,
  palette = "Spectral",
  opacity = 0.75
)
}
\arguments{
\item{image}{Image to be plotted}

\item{map}{Classified map to be overlayed on top on image}

\item{red}{Band for red color.}

\item{green}{Band for green color.}

\item{blue}{Band for blue color.}

\item{legend}{Named vector that associates labels to colors.}

\item{palette}{Palette provided in the configuration file.}

\item{opacity}{Opacity of overlayed map}
}
\value{
A leaflet object.
}
\description{
Include leaflet to view images (BW or RGB)
}
\examples{
if (bayes_run_examples()) {
    # Define location of a probability file
    data_dir <- system.file("/extdata/probs",
                package = "bayesEO")
    # list the file
    file <- list.files(data_dir)
    # build the full path
    probs_file <- paste0(data_dir, "/", file)
    # define labels
    labels <- c("Water", "ClearCut_Burn", "ClearCut_Soil",
                "ClearCut_Veg", "Forest", "Wetland")

    probs_image <- bayes_read_probs(probs_file, labels)
    # Label the probs image
    map <- bayes_label(probs_image)

    # Define location of a RGB files
    rgb_dir <- system.file("/extdata/rgb", package = "bayesEO")
    # list the files
    files <- list.files(rgb_dir)
    # build the full path
    image_files <- paste0(rgb_dir, "/", files)
    rgb_image <- bayes_read_image(image_files)
    # view the images and map
    bayes_view(
        image = rgb_image,
        map = map,
        red = "B11",
        green = "B8A",
        blue = "B02"
    )
}


}
\author{
Gilberto Camara, \email{gilberto.camara@inpe.br}
}
