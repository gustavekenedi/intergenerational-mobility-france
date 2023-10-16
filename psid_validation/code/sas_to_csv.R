packages <- c("data.table", "SAScii", "furrr")
librarian::shelf(packages, quiet = TRUE)
rm(packages)

# path to data
data_path <- "~/Dropbox/3resources/data/psid/"

# IND2019ER
data <- read.SAScii(paste0(data_path, "ind2019er/IND2019ER.txt"),
                    paste0(data_path, "ind2019er/IND2019ER.sas"))
fwrite(data, paste0(data_path, "ind2019er/IND2019ER.csv"))
rm(data)
gc()

# family files
plan(multisession, workers = 6)
system.time(
  c(1968:1997, seq(1999, 2019, 2)) %>% future_map(function(i) {
    
    if (i %in% 1968:1993) {
      file_path <- paste0(data_path, "fam", i, "/FAM", i)
    } else {
      file_path <- paste0(data_path, "fam", i, "er", "/FAM", i, "ER")
    }
    
    if (!file.exists(paste0(file_path, ".csv"))) {
      data <- read.SAScii(paste0(file_path, ".txt"),
                          paste0(file_path, ".sas"))
      fwrite(data, paste0(file_path, ".csv"))
    }
  }, .progress = T))
rm(file_path, data)
gc()

# parent identification file
data <- read.SAScii(paste0(data_path, "pid19/PID19.txt"),
                    paste0(data_path, "pid19/PID19.sas"))
fwrite(data, paste0(data_path, "pid19/PID19.csv"))
rm(data)
gc()

# family identification mapping system (FIMS) file - long
data <- read.SAScii(paste0(data_path, "family_identification_mapping_system/fim13036_gid_BA_2_UBL.txt"),
                    paste0(data_path, "family_identification_mapping_system/fim13036_gid_BA_2_UBL.sas"))
fwrite(data, paste0(data_path, "family_identification_mapping_system/fim13036_gid_BA_2_UBL.csv"))
rm(data)
gc()

# family identification mapping system (FIMS) file - wide
data <- read.SAScii(paste0(data_path, "family_identification_mapping_system/fim13037_gid_BA_2_UBL_wide.txt"),
                    paste0(data_path, "family_identification_mapping_system/fim13037_gid_BA_2_UBL_wide.sas"))
fwrite(data, paste0(data_path, "family_identification_mapping_system/fim13037_gid_BA_2_UBL_wide.csv"))
rm(data)
gc()
