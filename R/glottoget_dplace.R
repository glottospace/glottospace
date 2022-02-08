# EApath <- "D:/Linguistic/D-PLACE/dplace-data-v2.2.0/D-PLACE-dplace-data-541f9f8/datasets/EA/"
# https://zenodo.org/record/5554412
#
# # variables <- read.csv(paste0(EApath, "variables.csv"))
# eadata <- read.csv(paste0(EApath, "data.csv"))
# eadata <- eadata %>% select(soc_id, var_id, code)
# eadata <-  tidyr::spread(data = eadata, key = "var_id", value = "code")
# eatb <- as_tibble(eadata)
#
# socdf <- read.csv(paste0(EApath, "societies.csv"))
# soctb <- as_tibble(socdf)
#
# dplace <- left_join(soctb, eatb, by = c("id" = "soc_id"))
# # gcea <- c("glottocode", "id", grep(colnames(dplace), pattern = "EA", value = TRUE) )
# #
# # dplace <- dplace[, gcea] %>% rename(soc_id = id)
#
# gcea <- c("glottocode", grep(colnames(dplace), pattern = "EA", value = TRUE) )
#
# dplace <- dplace[, gcea]
#
# write.xlsx(dplace, 'data/dplace_ea.xlsx')
