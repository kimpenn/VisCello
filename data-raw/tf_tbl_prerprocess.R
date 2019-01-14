



# Table obtain from Lambert, Samuel A., et al. "The human transcription factors." Cell 172.4 (2018): 650-665.
tf_human <- read.table("data-raw/TFs_Ensembl_v_1.01_human.txt", quote="\"", comment.char="")
tf_human <- as.data.frame(tf_human)[-c(1:2),,drop=F]
colnames(tf_human) <- "id"
tf_human$id <- as.character(tf_human$id)

tf_mouse <- read_excel("data-raw/1-s2.0-S0092867410000796-mmc1.xls", sheet = "Sheet1")
tf_mouse <- data.frame(name = tf_mouse$`Symbol (Mouse)`)
tf_mouse <- tf_mouse[tf_mouse$name != "---",, drop=F]
colnames(tf_mouse) <- "name"
tf_mouse$name <- as.character(tf_mouse$name)

usethis::use_data(tf_human, overwrite = T)
usethis::use_data(tf_mouse, overwrite = T)
