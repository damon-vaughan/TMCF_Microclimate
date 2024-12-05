

# Constants and vectors ---------------------------------------------------


full.tree.vec <- c("ET1", "ET2", "ET3", "ET4", "ET5", "ET6", "ET7", "ET8",
                   "FB1", "FB2", "FB3", "FB4", "FB5", "FB6", "FB7", "FB8", 
                   "TV1", "TV2", "TV3", "TV4")

no.ZC.vec.full <- c("ET1_MC1-16008", "ET1_MC2-15954", 
                    "ET6_MC1-15982", "ET6_MC3-15949",
                    "ETP1_ATM41-15899", "ETP2_ATM41-16041", 
                    "FBP1_ATM41-15996", "FBP2_ATM41-15979-15935-16001", 
                    "TVP_ATM41-15917")

full.ground.vec <- c("ETP1", "ETP2", "FBP1", "FBP2", "TVP")


# Step 1: Import ZC data ---------------------------------------------


# ZC read function
# test <- getReadings(
#   device_sn = "z6-15987", start_time = "2023-07-01 00:00:00", end_time = "2023-07-31 23:59:00")
# test2 <- test[[1]]

# read_ZC is the basic ZC read function. Read data from zentracloud based on our naming convention
# Parameters. MC: MC ID; Start: Date start in y-m-d h:m:s; End: Date end
read_ZC <- function(MC, Start, End){
  ZL <- zl6.db %>% 
    filter(MC.ID == MC) %>% 
    pull(ZL.ID)
  d <- getReadings(
    device_sn = ZL, start_time = Start, end_time = End
  )
  return(d)
}



# Format the columns correctly and add port # to colname
# Parameters. Element: information from a single port; MC: MC ID, supplied in the for loop
# Notes: Port numbers need to be added at this stage otherwise there are name duplication problems when two of the same variables are measured from different ports
format_element <- function(element){
  # Add the tz offset so it shows the correct time, but call it "UTC" to match with all the other data
  d <- MC[[element]] %>% 
    select(!contains("error")) %>% 
    rename_with(~str_remove(., ".value")) %>%
    mutate(Timestamp = timestamp_utc + tz_offset,
           Timestamp = as_datetime(Timestamp)) %>% 
    select(Timestamp, everything(), -timestamp_utc, -tz_offset, -datetime)
  
  port.num <- rep(str_c("Port", str_sub(names(MC), -1)[element], "_"),
                  ncol(d) - 1)
  
  d2 <- d %>%
    rename_with(.cols = !starts_with("Time"), .fn = ~str_c(port.num, .))
  return(d2)
}



# Change column names so they aren't crazy
# Parameters. Element: An output from format_element(); source: The way the data was obtained (ZC_Old (downloaded from online), ZC_R, or NoZC)
# Notes: The variable names are different depending on how you downloaded the data. This function relies on a supporting excel sheet that tracks this.
change_column_names <- function(element, source){
  name.change2 <- name.change %>% 
    select(Final, Original = all_of(source))
  
  VarNames = names(element[2:ncol(element)])
  VarNamesReduced = str_sub(VarNames, start = 7)
  newVarNames = name.change$Final[match(VarNamesReduced, name.change2$Original)]
  newColNames = c(names(element)[1], 
                  str_c(str_sub(VarNames, end = 6), newVarNames))
  names(element) <- newColNames
  return(element)
}
# test <- change_column_names(element, "ZC_R")


# Functions to import non-ZC data -----------------------------------------


# Imports data from direct datalogger downloads that has not been formatted in ZentraCloud
# Parameters. x: a file path where the data is stored
# x <- filename.import
read_MC_noZC <- function(x){
  
  data.view = suppressMessages(read_csv(x, col_names = FALSE, n_max = 3,
                                        col_types = cols(.default = "c")))
  data.types = c("c", rep("n", ncol(data.view) - 1))
  port.names = str_remove(as.character(data.view[1,][-1]), " ")
  
  variable.names = as.character(data.view[3,][-1])
  variable.names2 = gsub("[^[:alnum:]]", "", variable.names)
  
  #fix duplicated column names (ECRN-100 and ATM41 both have Precip)
  instrument.names = as.character(data.view[2,][-1])
  clarify.instrument = which(instrument.names == "ECRN-100")
  variable.names3 = replace(variable.names2, 
                            clarify.instrument, 
                            str_c(instrument.names[clarify.instrument],
                                  "_", variable.names2[clarify.instrument]))
  
  new.col.names = c("Timestamp", 
                    str_c(port.names, "_",
                          variable.names3))
  
  d <- read_csv(x, col_names = new.col.names, 
                na = "#N/A",
                col_types = data.types, skip = 4) %>% 
    mutate(Timestamp = mdy_hm(Timestamp, tz = "UTC"))
  
  return(d)
}
# read_MC_noZC(filenames[3])


# Step 2: Organize by station -------------------------------------------
# The script runs MCdata_to_tree, which relies on the sub-function MCdata_to_datalogger, which in turn relies on MCdata_to_port


# MCdata_to_tree applies MCdata_to_datalogger to all dataloggers in a tree and binds the results together
# Parameters. TreeID: Name of a tree
# TreeID <- "TV1"
MCdata_to_tree <- function(TreeID){
  ZLs <- zl6.db %>% 
    filter(Location == TreeID) %>% 
    pull(MC.ID)
  d <- lapply(ZLs, MCdata_to_datalogger) %>% 
    bind_rows()
  return(d)
}
# test <- MCdata_to_tree("TV1")
# str(test$Port[[1]])


# MCdata_to_datalogger reads the raw data from a datalogger, and applies MCdata_to_port to all ports and creates a nested dataframe that carries the port metadata
# Parameters. MCnum: MCID of an individual datalogger (eg "FB1_MC1), typically passed in from MCdata_to_tree
# MCnum <- "ET5_MC3"
MCdata_to_datalogger <- function(MCnum){
  rawdat <- read_csv(file.path("Microclimate_data_raw", str_c(MCnum, ".csv")),
                     show_col_types = F)
  
  # make vector of active port numbers
  port.vec <- zl6.db.long %>%
    filter(MC.ID == MCnum) %>%
    select(MC.ID, Port, Station) %>%
    na.omit() 
  
  lapply(port.vec$Port, MCdata_to_port, df = rawdat) %>% 
    bind_rows()  
}
# MCdata_to_datalogger("ET5_MC3")



# MCdata_to_port extracts data from an individual port and saves as 1 row in a nested dataframe that also holds the metadata about station number and instrument type. ***This is the function that really does the work here; the other two are just organizing the outputs from this one.
# Parameters. Portnum: Port number, typically passed in from the MCdata_to_datalogger function; df: raw data, also typically passed in from that function
# Portnum <- 6
# df <- read_csv(file.path("Microclimate_data_raw", str_c("FB1_MC1", ".csv")),
#                show_col_types = F)
MCdata_to_port <- function(Portnum, df){
  
  MCID <- unique(df$MC)
  TreeID <- unique(df$Tree)
  
  port.metadata = zl6.db.long %>%
    filter(MC.ID == str_c(TreeID, "_", MCID)) %>% 
    filter(Port == Portnum)
  
  # Swap Port # for Station #. Need to keep some identifier to avoid column name duplication, but this accounts for sensors being moved to different ports 
  swap_port_for_station <- function(x){
    str_replace(x, "Port\\d+", pull(port.metadata, Station))
  }
  
  df2 <- df %>% 
    select(Tree, MC, Timestamp, 
           which(str_detect(colnames(df), 
                            str_c("Port", as.character(Portnum))) == T)) %>% 
    mutate(Station = pull(port.metadata, Station)) %>% 
    mutate(Port = as.numeric(Portnum)) %>% 
    mutate(Instrument = pull(port.metadata, Instrument)) %>% 
    select(Tree, MC, Station, Instrument, Timestamp, everything()) %>% 
    rename_with(swap_port_for_station)
  
  df3 <- df2  %>%  
    group_by(Tree, MC, Station, Port, Instrument) %>%
    nest() 
  # %>% 
  #   mutate(Start.timestamp = min(df2$Timestamp, na.rm = T),
  #          End.timestamp = max(df2$Timestamp, na.rm = T))
  return(df3)
}
# test <- MCdata_to_port(5, df)



# fix_ZC_timestamp_duplication fixes the duplicate timestamps that come from ZentraCloud. Sometimes multiple records are given of the same timestamp, which can cause major problems with joins. This is a quick and dirty method that just grabs the last entry. Meant to work on a single time series at a time, so must nest first if working at tree or higher levels.
# Parameters. df: a dataframe, typically one unnested row from MC_to_tree
# df <- MC_to_tree("ET3") %>%
#   filter(Station == "S5") %>% 
#   filter(Instrument == "A14") %>% 
#   unnest(data)
# which(duplicated(df$Timestamp) == T)
fix_ZC_timestamp_duplication <- function(df){
  x2 <- df %>% 
    distinct() %>% 
    arrange(Timestamp) 
  
  # Create df with all but the first entry. In first entry, Timestamp does not equal the previous timestamp, but in all others it does. This df will be removed from x2
  dupe.time <- x2  %>%
    mutate(Timestamp2 = lag(Timestamp)) %>% 
    filter(Timestamp == Timestamp2) 
  
  # anti join means that all entries in dupe.time will be removed, leaving only the first
  suppressMessages(x3 <- x2 %>% 
                     anti_join(dupe.time))
  
  return(x3)
}
# test <- fix_ZC_timestamp_duplication(df)
# which(duplicated(test$Timestamp) == T)



# remove_NAs_from_moved_sensors deals with the leading and trailing NAs that result from sensors being moved to new ports (leading NAs are added to top of new port, trailing NAs are added below old port). These NAs then create duplicated timestamps when bound together if the problem is not dealt with
# Parameters. d.nst: output from MC_to_tree, created in the first line of the for loop in Step 2
# d.nst <- MCdata_to_tree("TV1") %>%
#   ungroup() %>% 
#   mutate(data = map(data, fix_ZC_timestamp_duplication))
remove_NAs_from_moved_sensors <- function(d.nst){
  remove_leading_NAs <- function(x, y){
    x %>% 
      filter(floor_date(Timestamp, "days") > y) 
  }
  
  remove_trailing_NAs <- function(x, y){
    x %>% 
      filter(floor_date(Timestamp, "days") < y)
  }
  
  # Join on the metadata about moved instruments and filter so only rows with moved instruments appear. Remove the trailing NAs from the old position, and remove the leading NAs from the new position
  d <- d.nst %>% 
    left_join(moved.instruments, by = c("Tree", "Station", "Instrument", "Port", "MC")) %>% 
    filter(!is.na(Timestamp)) %>% 
    mutate(data = ifelse(Tracking == "MC.port.new", 
                         map2(data, Timestamp, remove_leading_NAs), data)) %>%
    mutate(data = ifelse(Tracking == "MC.port.old",
                         map2(data, Timestamp, remove_trailing_NAs), data)) %>% 
    select(-Timestamp, -Tracking, -MC.port)
  
  # Bind the data from multiple port locations into one dataframe with the correct Station # identified. MCID simply gets defined as "Changed", but this does not matter 
  suppressMessages(d2 <- d %>% 
                     group_by(Tree, Station, Instrument) %>% 
                     summarise(data = list(bind_rows(data))) %>%
                     mutate(data = map(data, arrange, Timestamp)) %>% 
                     mutate(MC = "Changed"))
  
  # Remove all old rows associated with the moved instrument, and bind on the one that was just created
  d3 <- d.nst %>% 
    anti_join(d2, by = c("Tree", "Station", "Instrument")) %>% 
    bind_rows(d2) %>% 
    select(-Port)
  
  return(d3)
}
# test <- remove_NAs_from_moved_sensors(d.nst)



# MC_to_station filters the output of "MC_to_tree" to an individual station and outputs the df with all variables
# Parameters. StationNum: A station ID, such as "S1"; d.nested: A nested df output from "MC_to_tree"
# StationNum <- "S4"
# d.nested <- d.nst3 #Grab this from the import script
MC_to_station <- function(StationNum, d.nested){
  nst <- d.nested %>%
    filter(Station == StationNum) %>% 
    ungroup() 
  
  df <- reduce(nst$data, full_join, by = "Timestamp") %>% 
    mutate(Tree = unique(nst$Tree),
           Station = unique(nst$Station)) %>% 
    select(Tree, Station, Timestamp, everything())
  return(df)
}
# test <- MC_to_station(StationNum = "S2", d.nested = d.nst3)



# remove_station_from_header strips the headers of the Station identifier. Station identifier is no longer needed in the dataframes, but is maintained in the nested dataframes. If this is not done, causes problems with binding rows together
# Parameters. x:
remove_station_from_header <- function(x){
  v <- names(x)
  no.station <- str_sub(v[4: length(v)], start = 4)
  names(x) <- c(v[1:3], no.station)
  return(x)
}



# x <- d.nst$data[[1]]
remove_port_from_header <- function(x){
  v <- names(x)
  no.port <- str_sub(v[4: length(v)], start = 7)
  names(x) <- c(v[1:3], no.port)
  return(x)
}



# change_column_names_prejune adapts the earlier function to work on prejune data. Simpler than the other function
# Parameters. df: a station-level df 
# df <- d
# location <- "Ground"
change_column_names_prejune <- function(df, location = "Tree"){
  name.change2 <- name.change %>% 
    select(Final, Original = all_of("ZC_Old"))
  
  var.names = names(df[4:ncol(df)])
  
  # Tree sensors have datalogger and port in front of variable names; ground sensors dont
  if(location == "Tree"){
    var.names.reduced = str_sub(var.names, start = 11)
  } else if(location == "Ground") {
    var.names.reduced = var.names
  }
  
  new.var.names = name.change2$Final[match(var.names.reduced, 
                                       name.change2$Original)]
  new.col.names = c(names(df)[1:3], new.var.names)
  names(df) <- new.col.names
  return(df)
}
# test <- change_column_names(element, "ZC_R")

# Step 3: Clean data ------------------------------------------------------

# Check for duplicates in dataframes with a full key of Tree Station Timestamp (TST)
check_dupes_TST <- function(x){
  x |>
    dplyr::summarise(n = dplyr::n(), .by = c(Tree, Station, Timestamp)) |>
    dplyr::filter(n > 1L)
}

# Generalized function for creating an expanded df of timestamps, meant to be joined to another df to flag certain entries for things like bad data, leaf drop, etc. Meant to be applied to nested dataframe. X needs to be formatted as a df with a Start and End column. The for loop is necessary because even if this is used with a nested df sometimes there are multiple entries within that
# Expanded Timestamp column includes the full day of "Start" and the full day of "End"
# x <- bad.data.nst$data[[1]]
# y <- "15 min"
expand_data_window <- function(x, y){
  results <- NULL
  for(j in 1:nrow(x)){
    toBind <- data.frame(
      Timestamp = seq(from = x$Start[j],
                      # add a day here so the End day is included
                      to = x$End[j] + days(1),
                      by = y)) %>%
      mutate(Data.flag = 1)
    results <- bind_rows(results, toBind)
  }
  return(results)
}

# Function for time series where the RH has a constant upper flat value that is close to but not equal to 1. Makes a simple adjustment using the offset between the series max and 1.
# x <- d3.nst$data[[1]]
adjust_RH_maxes_global <- function(x){
  if(unique(x$Key) %in% RH.low.maxes.global){
    x2 <- x %>% 
      mutate(RH = RH + (1 - max(RH, na.rm = T)))
  } else {
    x2 <- x
  }
  return(x2)
}
# str(x)

# More complicated adjustment function for when the flat max RH value drifts throughout the life of the series. Similar strategy but uses weekly maxes to determine the offset value, and assumes the RH reaches 1 on a weekly basis.
adjust_RH_maxes_local <- function(x){
  if(unique(x$Key) %in% RH.low.maxes.local){
    weekly.max <- x %>% 
      group_by(Week = floor_date(Timestamp, "weeks")) %>%
      summarise(Week.max = max(RH, na.rm = T)) %>% 
      mutate(Week.max = ifelse(Week.max == -Inf, 1, Week.max))
    x2 <- x %>% 
      mutate(Week = floor_date(Timestamp, "weeks")) %>% 
      left_join(weekly.max, by = "Week") %>% 
      mutate(RH = RH + (1 - Week.max)) %>% 
      select(-Week, -Week.max)
  } else {
    x2 <- x
  }
  return(x2)
}

# This one needs to be verified. newer i think
calculate_VPD <- function(Temp, RH){
  a <- 0.611  #kPa
  b <- 17.502
  c <- 240.97 #degrees C
  
  eSat <- a * exp((b*Temp)/(Temp + c))
  eActual <- eSat * RH
  VPD <- (eSat) - eActual
  VPD_rnd <- round(VPD, 2)
  return(VPD_rnd)
}