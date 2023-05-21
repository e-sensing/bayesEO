test_that("BayesEO", {

    data_dir <- system.file("/extdata/probs/", package = "bayesEO")
    file <- list.files(data_dir)

    x <- terra::rast(paste0(data_dir, "/", file))
    expect_true("SpatRaster" %in% class(x))
    expect_equal(terra::xmax(x), 350000)
    expect_true(grepl("SENTINEL-2", terra::sources(x)))

    labels <- c("Water", "ClearCut_Burn", "ClearCut_Soil",
                "ClearCut_Veg", "Forest", "Wetland")

    names(x) <- labels
    expect_equal(names(x), labels)

    p <- bayes_plot(x, scale = 0.0001, labels = c("Forest", "ClearCut_Soil"))
    expect_true(p$tm_layout$legend.bg.color == "white")
    expect_true(p$tm_layout$legend.text.size  == 1)
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

    p1 <- bayes_plot(y, scale = 0.0001, labels = c("Forest", "ClearCut_Soil"))
    expect_true(p1$tm_layout$legend.bg.color == "white")
    expect_true(p1$tm_layout$legend.text.size  == 1)
    expect_true(p1$tm_shape$line.center == "midpoint")

    z <- bayes_label(x)
    expect_equal(terra::levels(z)[[1]]$class, labels)
    expect_equal(terra::xmax(z), 350000)
    expect_equal(terra::nlyr(z), 1)

    p2 <- bayes_map(z)
    expect_true(p2$tm_layout$legend.bg.color == "white")
    expect_true(p2$tm_layout$legend.text.size  == 1)
    expect_true(p2$tm_shape$line.center == "midpoint")
    expect_equal(p2$tm_raster$labels, labels)

    v <- bayes_variance(y)
    expect_equal(names(v), labels)
    expect_equal(terra::xmax(v), 350000)
    expect_true(max(v[,1]) < 50)
    expect_true(max(v[,1]) > 0)

    p3 <- bayes_plot(v, quantile = 0.75, labels = c("Forest", "ClearCut_Soil"))
    expect_true(p3$tm_layout$legend.bg.color == "white")
    expect_true(p3$tm_layout$legend.text.size  == 1)
    expect_true(p3$tm_shape$line.center == "midpoint")
    expect_equal(p3$tm_raster$palette, "YlGnBu")

    p4 <- bayes_hist(v, quantile = 0.75)
    expect_true(p4$labels$x == "variance")
    expect_true(p4$labels$y == "count")

})
