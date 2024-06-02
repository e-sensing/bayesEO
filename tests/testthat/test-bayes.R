test_that("BayesEO", {

    data_dir <- system.file("/extdata/probs/", package = "bayesEO")
    file <- list.files(data_dir)
    probs_file <- paste0(data_dir, "/", file)
    labels <- c("Water", "ClearCut_Burn", "ClearCut_Soil",
                "ClearCut_Veg", "Forest", "Wetland")

    x <- bayes_read_probs(probs_file, labels)
    expect_equal(names(x), labels)
    expect_true("SpatRaster" %in% class(x))
    expect_equal(terra::xmax(x), 350000)

    rgb_dir <- system.file("/extdata/rgb/", package = "bayesEO")
    rgb_files <- paste0(rgb_dir, "/", list.files(rgb_dir))

    rgb_image <- bayes_read_image(rgb_files)
    expect_equal(terra::nlyr(rgb_image), 3)
    expect_true("SpatRaster" %in% class(rgb_image))
    expect_equal(terra::xmax(rgb_image), 350000)

    p <- bayes_plot_probs(x, labels = c("Forest", "ClearCut_Soil"))
    expect_true(p$tm_layout$legend.bg.color == "white")
    expect_true(p$tm_shape$line.center == "midpoint")

    y <- bayes_smooth(
        x,
        window_size = 7,
        neigh_fraction = 0.5,
        smoothness = 10
    )
    expect_equal(names(y), labels)
    expect_equal(terra::xmax(y), 350000)
    expect_true(max(y[,1]) < 10000)
    expect_true(max(y[,1]) > 9500)

    z <- bayes_label(x)
    expect_equal(terra::levels(z)[[1]]$class, labels)
    expect_equal(terra::xmax(z), 350000)
    expect_equal(terra::nlyr(z), 1)

    p2 <- bayes_plot_map(z)
    expect_true(p2$tm_layout$legend.bg.color == "white")
    expect_true(p2$tm_shape$line.center == "midpoint")
    expect_equal(p2$tm_raster$labels, labels)

    v <- bayes_variance(y)
    expect_equal(names(v), labels)
    expect_equal(terra::xmax(v), 350000)
    expect_true(max(v[,1]) < 50)
    expect_true(max(v[,1]) > 0)

    p3 <- bayes_plot_probs(v, labels = c("Forest", "ClearCut_Soil"))
    expect_true(p3$tm_layout$legend.bg.color == "white")
    expect_true(p3$tm_shape$line.center == "midpoint")
    expect_equal(p3$tm_raster$palette, "YlGnBu")

    p4 <- bayes_plot_hist(v, quantile = 0.75)
    expect_true(p4$labels$x == "variance")
    expect_true(p4$labels$y == "count")

    yg <- gaussian_smooth(
        x,
        window_size = 5
    )
    expect_equal(names(yg), labels)
    expect_equal(terra::xmax(yg), 350000)
    expect_true(max(yg[,1]) < 10000)
    expect_true(max(yg[,1]) > 9500)

    yb <- bilateral_smooth(
        x,
        window_size = 5
    )
    expect_equal(names(yb), labels)
    expect_equal(terra::xmax(yb), 350000)
    expect_true(max(yb[,1]) < 10000)
    expect_true(max(yb[,1]) > 9500)




})
