#' @title Function to retrieve bayesEO color table
#' @name bayes_colors
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Returns a color table
#' @return              A tibble with color names and values
#'
#'
#' @export
#'
bayes_colors <- function() {
    # load the color configuration file
    color_yml_file <- system.file("./extdata/config_colors.yml", package = "bayesEO")
    config_colors <- yaml::yaml.load_file(
        input = color_yml_file,
        merge.precedence = "override"
    )
    config_colors <- config_colors$colors
    base_names <- names(config_colors)
    color_table <- purrr::map2_dfr(config_colors, base_names, function(cl, bn) {
        cc_tb <- tibble::tibble(name = names(cl),
                                color = unlist(cl),
                                group = bn)
        return(cc_tb)
    })
    # set the color table
    bayesEO_env$color_table <- color_table
    return(invisible(NULL))
}
#' @title Function to show colors in SITS
#' @name bayes_colors_show
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description         Shows the default SITS colors
#'
#' @return  no return, called for side effects
#'
#' @export
#'
bayes_colors_show <- function() {
    color_tb <- bayesEO_env$color_table
    n_colors <- nrow(color_tb)
    n_rows_show <- n_colors %/% 3

    color_tb <- tibble::add_column(color_tb,
                                   y = seq(0, n_colors - 1) %% n_rows_show,
                                   x = seq(0, n_colors - 1) %/% n_rows_show)
    y_size <- 1.2
    g <- ggplot2::ggplot() +
        ggplot2::scale_x_continuous(name = "",
                                    breaks = NULL,
                                    expand = c(0, 0)) +
        ggplot2::scale_y_continuous(name = "",
                                    breaks = NULL,
                                    expand = c(0, 0)) +
        ggplot2::geom_rect(data = color_tb,
                           mapping = ggplot2::aes(
                               xmin = .data[["x"]] + 0.05,
                               xmax = .data[["x"]] + 0.95,
                               ymin = .data[["y"]] + 0.05,
                               ymax = .data[["y"]] + y_size
                           ),
                           fill = color_tb$color
        ) +
        ggplot2::geom_text(data = color_tb,
                           mapping = ggplot2::aes(
                               x = .data[["x"]] + 0.5,
                               y = .data[["y"]] + 0.8,
                               label = .data[["name"]]),
                           colour = "grey15",
                           hjust = 0.5,
                           vjust = 1,
                           size = 9 / ggplot2::.pt)

    g + ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "#FFFFFF"))

    return(g)
}


#' @title Get colors associated to the labels
#' @name .color_get_labels
#' @param  labels  labels associated to the training classes
#' @param  palette  palette from `grDevices::hcl.pals()`
#'                  replaces default colors
#'                  when labels are not included in the config palette
#' @param  rev      revert the order of colors?
#' @keywords internal
#' @noRd
#' @return colors required to display the labels
.color_get_labels <- function(labels,
                              palette = "Spectral",
                              legend = NULL,
                              rev = TRUE) {

    # Get the  Color table
    color_tb <- bayesEO_env$color_table
    # Try to find colors in thecolor palette
    names_tb <- dplyr::filter(color_tb, .data[["name"]] %in% labels)$name
    # find the labels that exist in the color table
    labels_exist <- labels[labels %in% names_tb]
    # get the colors for the names that exist
    colors <- purrr::map_chr(labels_exist, function(l) {
        col <- color_tb |>
            dplyr::filter(.data[["name"]] == l) |>
            dplyr::pull(.data[["color"]])
        return(col)
    })
    # get the names of the colors that exist in the SITS color table
    names(colors) <- labels_exist

    # if there is a legend?
    if (!purrr::is_null(legend)) {
        # what are the names in the legend that are in the labels?
        labels_leg <- labels[labels %in% names(legend)]
        # what are the color labels that are included in the legend?
        colors_leg <- legend[labels_leg]
        # join color names in the legend to those in default colors
        colors <- c(
            colors_leg,
            colors[!names(colors) %in% names(colors_leg)]
        )
    }
    # are there any colors missing?
    if (!all(labels %in% names(colors))) {
        missing <- labels[!labels %in% names(colors)]
        warning("missing colors for labels ",
                paste(missing, collapse = ", ")
        )
        warning("using palette ", palette, " for missing colors")
        # grDevices does not work with one color missing
        colors_pal <- grDevices::hcl.colors(
            n = max(2, length(missing)),
            palette = palette,
            alpha = 1,
            rev = rev
        )
        # if there is only one color, get it
        colors_pal <- colors_pal[seq_len(length(missing))]
        names(colors_pal) <- missing
        # put all colors together
        colors <- c(colors, colors_pal)
    }
    # rename colors to fit the label order
    # and deal with duplicate labels
    colors <- colors[labels]
    # post-condition
    .check_chr(colors,
               len_min = length(labels),
               len_max = length(labels),
               is_named = TRUE,
               has_unique_names = FALSE,
               msg = "invalid color values"
    )

    return(colors)
}

