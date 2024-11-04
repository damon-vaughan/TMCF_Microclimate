# test <- d2 |>
#   dplyr::summarise(n = dplyr::n(), .by = c(Tree, Station, Timestamp)) |>
#   dplyr::filter(n > 1L)

# Step 1a: Import ZC ------------------------------

# remotes::install_git(url = "https://gitlab.com/meter-group-inc/pubpackages/zentracloud")
# remove.packages("zentracloud")

library(needs)
needs(tidyverse, zentracloud, readxl)

source("1_Functions_Microclimate.R")

options(readr.show_col_types = FALSE)

# Desktop:
# setZentracloudOptions(
#   cache_dir = NULL,
#   # cache_dir = "C:/Users/User/AppData/Local/R/zentracloud",
#   token = "2f197f0980ac6da7c6bc2922f48f6ac291ca1086"
#   , domain = "default"
# )

# Laptop
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

removed.vec <- c("ET5_MC2", "FB3_MC2", "FB1_MC2", "ET2_MC3", "ET6_MC3",
                 "FB4_MC1", "FB4_MC2", "FB4_MC3", "TV3_MC3")

ET.vec <- c("ET2_MC1", "ET2_MC2", 
            "ET3_MC1", "ET3_MC2", "ET3_MC3",
            "ET4_MC1", "ET4_MC2", "ET4_MC3",
            "ET5_MC1", "ET5_MC3",
            "ET7_MC1", "ET7_MC2", "ET7_MC3",
            "ET8_MC1", "ET8_MC2", "ET8_MC3") 

FB.vec <- c("FB1_MC1", "FB1_MC3",
            "FB2_MC1", "FB2_MC2", "FB2_MC3",
            "FB3_MC1", "FB3_MC3",
            "FB5_MC1", "FB5_MC2", "FB5_MC3",
            "FB6_MC1", "FB6_MC2", "FB6_MC3",
            "FB7_MC1", "FB7_MC2", "FB7_MC3",
            "FB8_MC1", "FB8_MC2", "FB8_MC3")

TV.vec <- c("TV1_MC1", "TV1_MC2", "TV1_MC3",
            "TV2_MC1", "TV2_MC2", "TV2_MC3",
            "TV3_MC1", "TV3_MC2", 
            "TV4_MC1", "TV4_MC2", "TV4_MC3")

## Loop --------------------------------------------------------------------
# The warning is because sometimes the same timestamp has 2 different sensor readings. Makes no sense, but it happens and generates a long warning that has to do with the error columns.

endDL <- "2024-08-28 23:23:59"
Log <- data.frame(MC = NA, Action = NA, Reason = NA)
# Last.import <- as_datetime("2023-09-01 00:00:00")

MC.vec <- TV.vec
# MC.vec <- c("TV1_MC1")
# i <- "TV1_MC1"
for(i in MC.vec){
  
  # Have to read and write this for each element 
  import.log <- read_csv(file.path("Microclimate_data_supporting",
                                   "zl6_import_log.csv"), show_col_types = F)
  
  #  Check timestamp of last import
  Last.import <- import.log %>% 
    filter(MC.ID == i) %>% 
    pull(Last.import) 
  
  # Verify > 3 day difference between the import log and endDL timestamp. If satisfied, download from last import to endDL. If not, skip to next and note in the log
  if(Last.import <= as.POSIXct(endDL) - days(3)) {
    repeat {
      tryCatch({
        # subtract 5 hours to fix strange gap before data start
        MC <- read_ZC(i, as.character(Last.import - hours(5)), endDL)
        # MC <- read_ZC(i, Start = "2024-06-21 00:00:00",
        #               End = endDL)
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
    reduce(full_join, by = "Timestamp")
  
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
}

# Step 1b -------------------------------------------------------------
# Removed from this process: "ET6_MC2-15957"

zl6.noZC <- read_csv(file.path("Microclimate_data_supporting",
                               "zl6_database.csv")) %>% 
  filter(Zentracloud == "No")

## Loop ----------------------------------------------------------------

no.ZC.vec <- no.ZC.vec.full
# no.ZC.vec <- no.ZC.vec.full[8]

# i <- no.ZC.vec[8]
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

# test <- read_csv(filename.in.raw, 
#                  show_col_types = F)
# str(test)


# Step 2a: Switch to Station-based organization ----------------------------
# and make it look like the toJune data
# ZC/noZC are both treated the same here

library(needs)
needs(tidyverse, readxl)

source("1_Functions_Microclimate.R")
options(readr.show_col_types = FALSE)

zl6.db.long <- read_csv(file.path("Microclimate_data_supporting",
                                  "zl6_database_long.csv"))

zl6.db <- read_csv(file.path("Microclimate_data_supporting",
                             "zl6_database.csv"))

moved.instruments <- read_excel(file.path("Microclimate_data_supporting",
                                          "zl6_moved_instruments.xlsx")) %>%
  unite(MC.port.old, MC.old, Port.old, sep = "_") %>%
  unite(MC.port.new, MC.new, Port.new, sep = "_") %>%
  select(Tree, Station, Instrument, MC.port.old, MC.port.new, Timestamp) %>% 
  pivot_longer(4:5, names_to = "Tracking", values_to = "MC.port") %>% 
  separate(MC.port, into = c("MC", "Port"), remove = F) %>% 
  mutate(Port = as.numeric(Port))

## Loop, trees ------------------------------------------------------------

tree.vec <- full.tree.vec
# tree.vec <- c("ET5")

# tree.vec <- "ET7"
# i <- "TV1"
for(i in tree.vec){
  
  d.nst <- MCdata_to_tree(i) %>% 
    ungroup()
  
  d.nst2 <- d.nst %>% 
    mutate(data = map(data, fix_ZC_timestamp_duplication))
  
  # Moving instrument to new datalogger causes a duplicated record with filler NAs !Only add row to moved_instruments if it was moved to new datalogger! Otherwise, just update the zl6_database_long sheet!
  fix.needed <- moved.instruments %>%
    filter(Tree == i) 

  if(nrow(fix.needed != 0)){
    d.nst3 <- remove_NAs_from_moved_sensors(d.nst2)
  } else {
    d.nst3 <- d.nst2
  }
  
  stations <- unique(d.nst3$Station)
  
  # Switching to lapply because I am applying the function to a vector (stations) and expecting a list output. The nested df is used as support so purrr approach not appropriate
  d.list <- lapply(stations, MC_to_station, d.nested = d.nst3) 
  
  d.list2 <- lapply(d.list, remove_station_from_header)
  
  d <- d.list2 %>% 
    bind_rows() %>% 
    select(-ATM22_temp, -ATM22_Xaxis, 
           -ATM22_Yaxis) %>% 
    arrange(Tree, Station, Timestamp) 
  
  # Drop VPD because it doesn't come in every tree
  if("VPD" %in% colnames(d)){
    d2 <- d %>%
      select(-VPD)
  } else {
    d2 <- d
  }
  
  write_csv(d2, file.path("Microclimate_data_L2",
                          str_c(i, "_MC", "_L2.csv")))
  cat("saved data for: ", i, "\n")
}

## Loop, ground stations --------------------------------------

ground.vec <- full.ground.vec
# i <- "ETP2"
for(i in ground.vec){
  
  d <- read_csv(file.path("Microclimate_data_raw", str_c(i, "_ATM41.csv"))) %>% 
    mutate(Station = "S0") %>% 
    select(-MC) %>% 
    select(Tree, Station, Timestamp, everything()) %>% 
    remove_port_from_header() 
  
  d2 <- d %>% 
    select(-Lightning_count, -Lightning_distance, -ATM22_Yaxis, 
           -ATM22_Xaxis, -ATM41_SensorTemp,
           -Drop1, -Drop2, -Drop3, -Drop4)
  
  d3 <- fix_ZC_timestamp_duplication(d2)
  
  write_csv(d3, file.path("Microclimate_data_L2",
                          str_c(i, "_MC", "_L2.csv")))
  cat("saved data for: ", i, "\n")
}


# Step 2b: Bring pre-June data up to L2 -----------------------------------

name.change <- read_excel(file.path("Microclimate_data_supporting",
                                    "Variable_names.xlsx"))
source("1_Functions_Microclimate.R")
prejune.L1.dir <- file.path("Microclimate_data_L2", "Microclimate_toJune2023_L1")
all.files <- list.files(prejune.L1.dir, 
                        pattern = ".csv")

## Most trees --------------------------------------------------------------

tree.vec.prejune <- full.tree.vec[-1]

# i <- "ET2"
for(i in tree.vec.prejune){
  pointer <- which(str_detect(all.files, i) == T)
  filenames <- str_c(prejune.L1.dir, all.files[pointer], sep = "/")
  S1 <- read_csv(filenames[which(str_detect(filenames, "S1") == T)]) %>% 
    change_column_names_prejune()
  S2 <- read_csv(filenames[which(str_detect(filenames, "S2") == T)]) %>% 
    change_column_names_prejune()
  S3 <- read_csv(filenames[which(str_detect(filenames, "S3") == T)]) %>% 
    change_column_names_prejune()
  S4 <- read_csv(filenames[which(str_detect(filenames, "S4") == T)]) %>% 
    change_column_names_prejune()
  if(length(which(str_detect(filenames, "S5") == T)) > 0){
    S5 <- read_csv(filenames[which(str_detect(filenames, "S5") == T)]) %>% 
      change_column_names_prejune()
  }
  
  if(length(which(str_detect(filenames, "S5") == T)) > 0){
    allStations <- bind_rows(S1, S2, S3, S4, S5)
  } else {
    allStations <- bind_rows(S1, S2, S3, S4)
  }
  
  allStations2 <- allStations %>%
    select(Tree, Station, Timestamp, Solar, Temp, RH, Atmos_pressure,
           LW_minutes, LW_minutes_H, LWS_Count, Wind_direction, Wind_speed, Gust_speed) 
  
  out <- str_c(i, "_L2_prejune.csv")
  # write_csv(treeAvg, here("Microclimate_data_LVL2", out))
  write_csv(allStations2, file.path("Microclimate_data_L2", "Microclimate_toJune2023_L2_new", out)) 
  print(i)
}

## ET1 -------------------------------------------------------------------

ET1.vec <- full.tree.vec[1]

# i <- "ET1"
for(i in ET1.vec){
  pointer <- which(str_detect(all.files, i) == T)
  filenames <- str_c(prejune.L1.dir, all.files[pointer], sep = "/")
  
  # Not using S5 in this case
  filenames <- str_c(prejune.L1.dir, all.files[pointer], sep = "/")
  S1 <- read_csv(filenames[which(str_detect(filenames, "S1") == T)]) %>% 
    change_column_names_prejune()
  S2 <- read_csv(filenames[which(str_detect(filenames, "S2") == T)]) %>% 
    change_column_names_prejune()
  S4 <- read_csv(filenames[which(str_detect(filenames, "S4") == T)]) %>% 
    change_column_names_prejune()
  
  all.stations <- bind_rows(S1, S2, S4)
  
  all.stations2 <- all.stations %>%
    select(Tree, Station, Timestamp, Solar, Temp, RH, Atmos_pressure,
           LW_minutes, LW_minutes_H, LWS_Count, Wind_direction, Wind_speed, Gust_speed) 
  
  out <- str_c(i, "_L2_prejune.csv")
  # write_csv(treeAvg, here("Microclimate_data_LVL2", out))
  write_csv(all.stations2, file.path("Microclimate_data_L2", "Microclimate_toJune2023_L2_new", out)) 
  print(i)
}

## Ground --------------------------------------------------------------

ground.vec <- full.ground.vec

# i <- ground.vec[5]
for(i in ground.vec){
  
  d <- read_csv(file.path("Microclimate_data_L2", "Microclimate_toJune2023_L1", 
                          str_c(i, "_LVL1.csv"))) %>%
    mutate(Station = "S0") %>% 
    select(-BatteryPercent, -mVBatteryVoltage, -kPaReferencePressure, -degreeCLoggerTemperature) %>% 
    select(Tree = PastureID, Station, Timestamp, everything()) %>% 
    change_column_names_prejune(location = "Ground")
  
  d2 <- d %>%
    select(Tree, Station, Timestamp, Solar, Temp, RH, Atmos_pressure,  
           LW_minutes, LW_minutes_H, LWS_Count, Wind_direction, Wind_speed, Gust_speed, 
           Precip_max, Precipitation) 
  
  out <- str_c(i, "_L2_prejune.csv")
  write_csv(d2, file.path("Microclimate_data_L2", "Microclimate_toJune2023_L2_new", out))
}

# Step 3: combine with pre-June ------------------------------
#per tree, combined w pre june. Also clean up the data

library(needs)
needs(tidyverse, lubridate)

source("1_Functions_Microclimate.R")

tree.vec <- full.tree.vec
ground.vec <- full.ground.vec

MC.vec <- c(tree.vec, ground.vec)
# MC.vec <- JulyProbs

# i <- "TV1"
for(i in MC.vec){
  old.dat <- read_csv(file.path("Microclimate_data_L2",
                                "Microclimate_toJune2023_L2_new",
                                str_c(i, "_L2_prejune.csv")))
  
  old.dat2 <- old.dat %>% 
    group_by(Station) %>% 
    nest() %>% 
    mutate(data = map(data, fix_ZC_timestamp_duplication)) %>% 
    unnest(data) %>% 
    ungroup()
  
  filename.in.L2 <- file.path("Microclimate_data_L2",
                              str_c(i, "_MC", "_L2.csv"))
  
  if(file.exists(filename.in.L2)){
    new.dat <- read_csv(file.path("Microclimate_data_L2",
                                  str_c(i, "_MC_L2.csv")))
    old.dat4 <- old.dat2 %>% 
      filter(Timestamp < min(new.dat$Timestamp, na.rm = T))
    d <- bind_rows(old.dat4, new.dat)
  } else {
    d <- old.dat2
    cat("no new data for: ", i, "\n")
  }
  
  d2 <- d %>% 
    distinct() %>% 
    arrange(Tree, Station, Timestamp)
  
  write_csv(d2, file.path("Microclimate_data_L3",
                          str_c(i, "_MC", "_L3.csv")))
  cat("saved data for: ", i, "\n")
}



# >>>Troubleshooting and extra ------------------------------------


## Basic graphs ------------------------------------------------------------

d <- read_csv(file.path("Microclimate_data_L3", "ET8_MC_L3.csv"))
str(d)

d2 <- d %>% 
  filter(Station == "Cansoil")

ggplot(d2) +
  geom_line(aes(x = Timestamp, y = soil_EpiTemp)) +
  theme_bw()

## Adding Cansoil to the pre-June data -------------------------------------
# First, manually download from ZC. Then save Excel files as UTF csv, and clean up a little bit. Then edit the name to not have MC number.
# Save over the original. To find the original original, look in Archive for the preJune MC original folder

to.June.dir <- file.path("Microclimate_data_L2", "Microclimate_toJune2023_L2")

FB2 <- read_csv(file.path(to.June.dir, "FB2_LVL2.csv"))
FB2.Cansoil <- read_csv(file.path(to.June.dir, "Epi_sensors", "FB2.csv"),
                        na = "#N/A") %>% 
  mutate(Timestamp = mdy_hm(Timestamp))

x <- "FB2"
add_preJune_cansoil <- function(x){
  # Drop the Cansoil stations to erase previous faulty additions
  d <- read_csv(file.path(to.June.dir, str_c(x, "_LVL2.csv"))) %>% 
    filter(Station != "Cansoil") %>% 
    select(-EpiMoisture, -EpiTemp)
  CS <- read_csv(file.path(to.June.dir, "Epi_sensors", 
                           str_c(x, ".csv")),
                 na = "#N/A") %>% 
    mutate(Timestamp = mdy_hm(Timestamp)) %>% 
    mutate(Tree = x, Station = "CS")
  out <- bind_rows(d, CS)
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



# Test read in ------------------------------------------------------------

i <- ET.vec[3]
start.DL <- "2024-07-01 00:00:00"
end.DL <- "2024-07-31 00:00:00"

# Exactly the same import process as in for loop, just simplified here, and run on a datalogger I know had the duplicated timestamp problem
repeat {
  tryCatch({
    MC <- read_ZC(i, start.DL, end.DL)
    break
  }
  , error = function(e) {
    cat(paste("Error on iteration", i, ":", conditionMessage(e), "\n"))
  }
  )
} 

d <- lapply(seq(1:length(MC)), format_element) %>% 
  lapply(change_column_names, "ZC_R") %>% 
  reduce(left_join, by = "Timestamp")

MC.ID <- str_c(str_sub(i, start = 5, end = 7))
Tree.ID <- str_sub(i, start = 1, end = 3)

d2 <- d %>% 
  mutate(Tree = Tree.ID,
         MC = MC.ID) %>% 
  select(Tree, MC, Timestamp, everything())

# Are there duplicated timestamps in final product? Yes
which(duplicated(d2$Timestamp) == T)
# Are there duplicated timestamps a step before? Yes
which(duplicated(d$Timestamp) == T)
# Are there duplicated timestamps in the initial download from ZC? Where are they
which(duplicated(MC[[1]]$datetime) == T)
# We just learned there was a duplicate at index 2115. Look at that in this df:
test <- MC[[1]] 

test[2112:2117,]
