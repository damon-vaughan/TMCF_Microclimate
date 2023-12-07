# Step 1a: Import ZC ------------------------------

# remotes::install_git(url = "https://gitlab.com/meter-group-inc/pubpackages/zentracloud")
# remove.packages("zentracloud")

library(needs)
needs(tidyverse, zentracloud, readxl)

source("Functions_Microclimate.R")

options(readr.show_col_types = FALSE)

setZentracloudOptions(
  cache_dir = "C:/Users/vaug8/AppData/Local/R/cache/R/zentracloud",
  token = "2f197f0980ac6da7c6bc2922f48f6ac291ca1086"
  , domain = "default"
)
# getZentracloudOptions()
# clearCache(file_age = 0L)

zl6.db <- read_csv(file.path("Microclimate_data_supporting",
                             "zl6_database.csv")) %>% 
  filter(Zentracloud == "Yes")

name.change <- read_excel(file.path("Microclimate_data_supporting",
                                    "Variable_names.xlsx"))

import.log <- read_csv(file.path("Microclimate_data_supporting",
                                 "zl6_import_log.csv"), show_col_types = F)

# config.changes <- read_excel(file.path("Microclimate_data_supporting",
#                                      "zl6_config_changes.xlsx")) %>% 
#   mutate(Timestamp = ymd_hms(Timestamp, tz = "UTC"))

## Vectors --------------------------------------------------------------

ET.vec <- c("ET2_MC1", "ET2_MC2", "ET2_MC3",
           "ET3_MC1", "ET3_MC2", "ET3_MC3",
           "ET4_MC1", "ET4_MC2", "ET4_MC3",
           "ET5_MC1", "ET5_MC2", "ET5_MC3",
           "ET7_MC1", "ET7_MC2", "ET7_MC3",
           "ET8_MC1", "ET8_MC2", "ET8_MC3") 

FB.vec <- c("FB1_MC1", "FB1_MC2", "FB1_MC3",
           "FB2_MC1", "FB2_MC2", "FB2_MC3",
           "FB3_MC1", "FB3_MC2", "FB3_MC3",
           "FB4_MC1", "FB4_MC2", "FB4_MC3",
           "FB5_MC1", "FB5_MC2", "FB5_MC3",
           "FB6_MC1", "FB6_MC2", "FB6_MC3",
           "FB7_MC1", "FB7_MC2", "FB7_MC3",
           "FB8_MC1", "FB8_MC2", "FB8_MC3")

TV.vec <- c("TV1_MC1", "TV1_MC2", "TV1_MC3",
           "TV2_MC1", "TV2_MC2", "TV2_MC3",
           "TV3_MC1", "TV3_MC2", "TV3_MC3",
           "TV4_MC1", "TV4_MC2", "TV4_MC3")

## Loop --------------------------------------------------------------------
# The warning is because sometimes the same timestamp has 2 different sensor readings. Makes no sense, but it happens and generates a long warning that has to do with the error columns.

endDL <- "2023-11-30 23:23:59"
Log <- data.frame(MC = NA, Action = NA, Reason = NA)
# Last.import <- as_datetime("2023-09-01 00:00:00")

MC.vec <- TV.vec
# i <- "FB3_MC3"
for(i in MC.vec){
  
  # Have to read and write this for each element 
  import.log <- read_csv(file.path("Microclimate_data_supporting",
                                   "zl6_import_log.csv"), show_col_types = F)
  
  #  Check timestamp of last import
  Last.import <- import.log %>% 
    filter(MC.ID == i) %>% 
    pull(Last.import) 
  
  # config.changes2 <- config.changes %>%
  #   filter(ymd_hms(Timestamp, tz = "UTC") > Last.import &
  #            ymd_hms(Timestamp, tz = "UTC") < endDL)

  # if(i %in% config.changes2$MC.ID){
  #   cat("Config change in ", i, "\n")
  #   next
  #   }
  
  # Verify > 3 day difference between the import log and endDL timestamp. If satisfied, download from last import to endDL. If not, skip to next and note in the log
  if(Last.import <= as.POSIXct(endDL) - days(3)) {
    repeat {
      tryCatch({
        # subtract 5 hours to fix strange gap before data start
        MC <- read_ZC(i, as.character(Last.import - hours(5)), endDL)
        # MC <- read_ZC(i, Start = "2023-06-01 00:00:00", 
        #               End = "2023-06-08 00:00:00")
        break
      }
      , error = function(e) {
        # An error occurred, so print a message and continue with the same i
        cat(paste("Error on iteration", i, ":", conditionMessage(e), "\n"))
      }
      )
    } 
  } else {
    cat("Skipped ", i, ", no import needed", "\n")
    Log1 <- data.frame(MC = i, Action = "Skipped",
                       Reason = "Import log")
    Log <- bind_rows(Log, Log1)
    next
  }
  
  # Format each port's data and combine them together. reduce function is a way to join list elements by a key column
  d <- lapply(seq(1:length(MC)), format_element) %>% 
    lapply(change_column_names, "ZC_R") %>% 
    reduce(left_join, by = "Timestamp")
  
  # Extract metadata from i
  MC.ID <- str_c(str_sub(i, start = 5, end = 7))
  Tree.ID <- str_sub(i, start = 1, end = 3)
  
  # Add metadata to the df and sort columns
  d2 <- d %>% 
    # rename_with(.cols = !starts_with("Time"), .fn = ~str_c(MC.ID, .)) %>% 
    mutate(Tree = Tree.ID,
           MC = MC.ID) %>% 
    select(Tree, MC, Timestamp, everything())
  
  filename.in.raw <- file.path("Microclimate_data_raw", str_c(i, ".csv"))
  
  if(file.exists(filename.in.raw)){
    raw.data <- read_csv(filename.in.raw, show_col_types = F)
    
    d.out <- bind_rows(raw.data, d2) %>% 
      distinct() %>% 
      arrange(Timestamp)
    
    write_csv(d.out, filename.in.raw)
    
  } else{
    write_csv(d2, filename.in.raw)
  }
  cat("Finished ", i, "\n")
  
  import.log <- import.log %>%
    mutate(Last.import = case_when(
      MC.ID == i ~ max(d2$Timestamp, na.rm = T),
      MC.ID != i ~ Last.import))
  
  Log1 <- data.frame(MC = i, Action = "Saved raw data", 
                     Problems = "None")
  Log <- bind_rows(Log, Log1)
  
  write_csv(import.log, file.path("Microclimate_data_supporting",
                                  "zl6_import_log.csv"))

# Step 1b -------------------------------------------------------------
zl6.noZC <- read_csv(file.path("Microclimate_data_supporting",
                               "zl6_database.csv")) %>% 
  filter(Zentracloud == "No")

## Loop ----------------------------------------------------------------

no.ZC.vec <- no.ZC.vec.full

i <- no.ZC.vec[9]
for(i in no.ZC.vec){
  filenames <- list.files(file.path("Microclimate_data_raw", "MC_noZC",
                                    i),
                          pattern = "csv", full.names = T)
  
  MC.ID <- str_split(i, "-")[[1]][1]
  
  # Read true raw data and change column names
  d <- lapply(filenames, read_MC_noZC) %>% 
    bind_rows() %>% 
    change_column_names(source = "NoZC") %>% 
    arrange(Timestamp)
  
  # get rid of any ghost sensors
  if(length(which(str_detect(names(d), "Unknown"))) > 0){
    d2 <- d %>% 
      select(-names(d)[which(str_detect(names(d), "Unknown"))])
  } else {
    d2 <- d
  }
  
  d3 <- d2 %>% 
    mutate(Tree = str_split(MC.ID, "_")[[1]][1],
           MC = str_split(MC.ID, "_")[[1]][2]) %>%
    select(Tree, MC, Timestamp, everything()) 
  
  write_csv(d3, file.path("Microclimate_data_raw", str_c(MC.ID, ".csv")))
  cat("Imported from: ", MC.ID, "\n")
}

test <- read_csv(filename.in.raw, 
                 show_col_types = F)
str(test)


# Step 2: Switch to Station-based organization ----------------------------
# and make it look like the toJune data
# ZC/noZC are both treated the same here

library(needs)
needs(tidyverse)

zl6.db.long <- read_csv(file.path("Microclimate_data_supporting",
                             "zl6_database_long.csv"))

zl6.db <- read_csv(file.path("Microclimate_data_supporting",
                             "zl6_database.csv"))

## Loop, trees ------------------------------------------------------------

tree.vec <- full.tree.vec
tree.vec <- c("FB5", "FB6", "FB7", "FB8", "TV1", "TV2", "TV3", "TV4")

duplicate.log <- data.frame(
  Tree = NA, Station = NA, Timestamp = NA, Solar = NA, Atmos_pressure = NA, 
  RH = NA, VPD = NA, Temp = NA, LWS_Count = NA, LW_minutes_H = NA, 
  LW_minutes = NA, Wind_speed = NA, Wind_direction = NA, Gust_speed = NA,
  Timestamp2 = NA, EpiMoisture = NA, EpiTemp = NA) 

i <- MCvec[4]
for(i in tree.vec){
  
  d.nst <- MC_to_tree(i)
  
  station.list <- unique(d.nst$Station)
  
  d.list <- lapply(station.list, MC_to_station, d.nested = d.nst) 
  
  d.list2 <- lapply(d.list, fix_names)
  
  d <- d.list2 %>% 
    bind_rows() %>% 
    arrange(Tree, Station, Timestamp) %>% 
    select(-ATM22_temp, -ATM22_Xaxis, 
           -ATM22_Yaxis)
  
  # Fixes duplicated timestamp issue. Slow but it works
  # d2 <- d  %>%
  #   group_by(Tree, Station, Timestamp) %>% 
  #   summarise(across(where(is.numeric), ~mean(., na.rm = T))) %>% 
  #   mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .))) %>% 
  #   ungroup()

  # A quicker fix that just takes the last one, and logs which are dropped
  duplicates <- d  %>%
    group_by(Tree, Station) %>% 
    mutate(Timestamp2 = lag(Timestamp)) %>% 
    ungroup() %>% 
    filter(Timestamp == Timestamp2)
  duplicate.log <- duplicate.log %>% 
    bind_rows(duplicates)
  
  d2 <- d %>% 
    anti_join(duplicates)
  
  d3 <- d2 %>% 
    mutate(Wetness = 1.54 * exp(0.0058 * LWS_Count)) %>% 
    select(-LWS_Count) %>% 
    correct_common_errors()
  
  # Calculate VPD where necessary (noZC)
  if(!("VPD" %in% colnames(d3))){
    d4 <- d3 %>% 
      mutate(VPD = calc_VPD(0.611, 17.502, 240.97, d2)) 
  } else {
    d4 <- d3
  }

  write_csv(d4, file.path("Microclimate_data_L2",
                              str_c(i, "_MC", "_L2.csv")))
  cat("saved data for: ", i, "\n")
}

## Loop, pasture stations --------------------------------------


# pasture.vec <- "TVP"
for(i in pasture.vec){
  
  d <- read_csv(file.path("Microclimate_data_raw", str_c(i, "_ATM41.csv")),
                show_col_types = F) %>% 
    mutate(Station = "S0") %>% 
    select(-MC) %>% 
    select(Tree, Station, Timestamp, everything()) %>% 
    fix_names()
  
  d2 <- d %>% 
    mutate(Wetness = 1.54 * exp(0.0058 * LWS_Count)) %>%
    mutate(VPD = calc_VPD(0.611, 17.502, 240.97, d)) %>% 
    select(-Lightning_count, -Lightning_distance, -ATM22_Yaxis, 
           -ATM22_Xaxis, -ATM41_SensorTemp, -LW_minutes_H, 
           -Drop1, -Drop2, -Drop3, -Drop4,
           -LWS_Count)
  
  write_csv(d2, file.path("Microclimate_data_L2",
                              str_c(i, "_MC", "_L2.csv")))
  cat("saved data for: ", i, "\n")
}

# Step 3: combine with pre-June ------------------------------
#per tree, combined w pre june. Also clean up the data

library(needs)
needs(tidyverse, here, lubridate)

tree.vec <- full.tree.vec

MC.vec <- c(tree.vec, pasture.vec)
# MC.vec <- JulyProbs

# i <- "FB2"
for(i in MC.vec){
  old.dat <- read_csv(file.path("Microclimate_data_L2",
                                "Microclimate_toJune2023_L2",
                                str_c(i, "_LVL2.csv")),
                      show_col_types = F) 
  
  if(i %in% pasture.vec){
    old.dat2 <- old.dat %>% 
      rename(Tree = PastureID) %>% 
      mutate(Station = "S0") %>% 
      select(Tree, Station, Timestamp, everything())
  } else{
    old.dat2 <- old.dat
  }
  
  filename.in.L2 <- file.path("Microclimate_data_L2",
                              str_c(i, "_MC", "_L2.csv"))
  if(file.exists(filename.in.L2)){
    new.dat <- read_csv(file.path("Microclimate_data_L2",
                                  str_c(i, "_MC_L2.csv")),
                        show_col_types = F)
    d <- bind_rows(old.dat2, new.dat)
  } else {
    d <- old.dat
  }
  
  d2 <- d %>% 
    distinct() %>% 
    arrange(Tree, Station, Timestamp)
  
  # Now fix problems with VPD and RH not filling out their range. No need for an if statement because if criteria already met then they are only adjusted by 0
  # Gives warnings for CanSoil stations, but I think that's ok
  d3 <- d2 %>% 
    group_by(Station) %>% 
    mutate(RH = RH + (1 - max(RH, na.rm = T))) %>% 
    mutate(VPD = VPD - min(VPD, na.rm = T))
    
  write_csv(d3, file.path("Microclimate_data_L3",
                          str_c(i, "_MC", "_L3.csv")))
  cat("saved data for: ", i, "\n")
}

# Troubleshooting and extra ------------------------------------

## Adding Cansoil to the pre-June data -------------------------------------
# First, manually download from ZC. Then save Excel files as UTF csv, and clean up a little bit. Then edit the name to not have MC number.
# Save over the original. To find the original original, look in Archive for the preJune MC original folder

to.June.dir <- file.path("Microclimate_data_L2", "Microclimate_toJune2023_L2")

FB2 <- read_csv(file.path(to.June.dir, "FB2_LVL2.csv"))
FB2.Cansoil <- read_csv(file.path(to.June.dir, "Epi_sensors", "FB2_MC1.csv"),
                        na = "#N/A") %>% 
  mutate(Timestamp = mdy_hm(Timestamp))

x <- "FB2"
add_preJune_cansoil <- function(x){
  d <- read_csv(file.path(to.June.dir, str_c(x, "_LVL2.csv")))
  Cansoil <- read_csv(file.path(to.June.dir, "Epi_sensors", 
                                str_c(x, ".csv")),
                      na = "#N/A") %>% 
    mutate(Timestamp = mdy_hm(Timestamp)) %>% 
    mutate(Tree = x, Station = "Cansoil")
  out <- bind_rows(d, Cansoil)
  return(out)
  }  

FB2 <- add_preJune_cansoil("FB2")
write_csv(FB2, file.path(to.June.dir, "FB2_LVL2.csv"))

FB4 <- add_preJune_cansoil("FB4")
write_csv(FB4, file.path(to.June.dir, "FB4_LVL2.csv"))

FB6 <- add_preJune_cansoil("FB6")
write_csv(FB6, file.path(to.June.dir, "FB6_LVL2.csv"))

ET8 <- add_preJune_cansoil("ET8")
write_csv(ET8, file.path(to.June.dir, "ET8_LVL2.csv"))

TV4 <- add_preJune_cansoil("TV4")
write_csv(TV4, file.path(to.June.dir, "TV4_LVL2.csv"))


## Troubleshoot ZC read in function ----------------------------------------


# didnt work before update
test <- getReadings(
  device_sn = "z6-16010", start_time = "2023-06-01 00:00:00", end_time = "2023-06-30 23:59:00"
)
d <- test[[1]]
which(duplicated(d$datetime) == T)

#always worked
test2 <- getReadings(
  device_sn = "z6-15848", start_time = "2023-06-01 00:00:00", end_time = "2023-06-30 23:59:00"
)
d <- test2[[1]]
which(duplicated(d$datetime) == T)

# config change
test <- getReadings(
  device_sn = "z6-16009", start_time = "2023-06-01 00:00:00", end_time = "2023-06-30 23:59:00"
)
d <- test[[5]]

test2 <- getReadings(
  device_sn = "z6-16009", start_time = "2023-06-07 00:00:00", end_time = "2023-06-30 23:59:00"
)
d <- test2[[5]]




