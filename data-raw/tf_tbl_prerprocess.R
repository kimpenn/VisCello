



# Table obtain from Lambert, Samuel A., et al. "The human transcription factors." Cell 172.4 (2018): 650-665.
tf_human <- read.csv("data-raw/cisbp_human_dataCSV956.csv")
tf_human <- tf_human %>%dplyr::filter(tf_human$Evidence != "N")
tf_human <- data.frame(name = tf_human$Name)
tf_human$name <- as.character(tf_human$name)

tf_mouse <- read.csv("data-raw/cisbp_mouse_CSV5929.csv")
tf_mouse <- tf_mouse %>%dplyr::filter(tf_mouse$Evidence != "N")
tf_mouse <- data.frame(name = tf_mouse$Name)
tf_mouse$name <- as.character(tf_mouse$name)


usethis::use_data(tf_human, overwrite = T)
usethis::use_data(tf_mouse, overwrite = T)
