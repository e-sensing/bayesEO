data_dir <- system.file("/extdata/Rondonia-20LLQ/", package = "sitsdata")
probs_file <- paste0(data_dir,
                     "SENTINEL-2_MSI_20LLQ_2020-06-04_2021-08-26_probs_v1.tif")
labels <- c("Water", "ClearCut_Burn", "ClearCut_Soil",
            "ClearCut_Veg", "Forest", "Wetland")

probs_image <- bayes_read_probs(probs_file, labels)
bayes_plot_probs(probs_image)

label_map_no_smooth <- bayes_label(probs_image)
bayes_plot_map(label_map_no_smooth)

green_file <- paste0(data_dir, "SENTINEL-2_MSI_20LLQ_B8A_2021-09-06.tif")
red_file <- paste0(data_dir, "SENTINEL-2_MSI_20LLQ_B11_2021-09-06.tif")
blue_file <- paste0(data_dir, "SENTINEL-2_MSI_20LLQ_B02_2021-09-06.tif")

rgb_files <- c(green_file, red_file, blue_file)

rgb_image <- bayes_read_image(rgb_files)

bayes_plot_rgb(rgb_image, red = "B11", green = "B8A", blue = "B02")

bayes_view(rgb_image, label_map_no_smooth,
           red = "B11", green = "B8A", blue = "B02")
