
# Constants and vectors ---------------------------------------------------

full.tree.vec <- c("ET1", "ET2", "ET3", "ET4", "ET5", "ET6", "ET7", "ET8",
                   "FB1", "FB2", "FB3", "FB4", "FB5", "FB6", "FB7", "FB8", 
                   "TV1", "TV2", "TV3", "TV4")

no.ZC.vec.full <- c("ET1_MC1-16008", "ET1_MC2-15954", 
                    "ET6_MC1-15982", "ET6_MC2-15957", "ET6_MC3-15949",
                    "ETP1_ATM41-15899", "ETP2_ATM41-16041", 
                    "FBP1_ATM41-15996", "FBP2_ATM41-15979-15935", 
                    "TVP_ATM41-15917")

pasture.vec <- c("ETP1", "ETP2", "FBP1", "FBP2", "TVP")

# Step 1: Import ZC data ---------------------------------------------

# ZC read function
# test <- getReadings(
#   device_sn = "z6-15987", start_time = "2023-07-01 00:00:00", end_time = "2023-07-31 23:59:00")
# test2 <- test[[1]]

# Basic ZC read function. Read data from zentracloud based on our naming convention
# Parameters: MC: MC ID; Start: Date start in y-m-d h:m:s; End: Date end
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
# Parameters: Element: information from a single port; MC: MC ID, supplied in the for loop
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
# Parameters: Element: An output from format_element(); source: The way the data was obtained (ZC_Old (downloaded from online), ZC_R, or NoZC)
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
# Parameters: x: a file path where the data is stored
read_MC_noZC <- function(x){
  
  data.view = suppressMessages(read_csv(x, col_names = FALSE, n_max = 3,
                                        col_types = cols(.default = "c")))
  data.types = c("c", rep("n", ncol(data.view) - 1))
  port.names = str_remove(as.character(data.view[1,][-1]), " ")
  
  variable.names = as.character(data.view[3,][-1])
  variable.names2 = gsub("[^[:alnum:]]", "", variable.names)
  
  #fix duplicated column names
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

# Step 2: Organize by station -------------------------------------------

# Extract data from an individual port and save as nested dataframe that also holds the metadata about station number and instrument type
# Portnum: Port number, typically passed in from the "MC_to_datalogger" function; df: raw data, also typically passed in from that function
MC_to_port <- function(Portnum, df){
  MCID <- unique(df$MC)
  TreeID <- unique(df$Tree)
  
  port.metadata = zl6.db.long %>%
    filter(MC.ID == str_c(TreeID, "_", MCID)) %>% 
    filter(Port == Portnum)
  
  df %>% 
    select(Tree, MC, Timestamp, 
           which(str_detect(colnames(df), 
                            str_c("Port", as.character(Portnum))) == T)) %>% 
    mutate(Station = pull(port.metadata, Station)) %>% 
    mutate(Instrument = pull(port.metadata, Instrument)) %>%  
    group_by(Tree, MC, Station, Instrument) %>%
    nest()
}

# Reads the raw data from a datalogger, and applies "MC_to_port" to all ports and creates a nested dataframe that carries the port metadata
# MCnum: MCID of an individual datalogger (eg "FB1_MC1), typically passed in from "MC_to_tree"
# MCnum <- "ET2_MC3"
MC_to_datalogger <- function(MCnum){
  rawdat <- read_csv(file.path("Microclimate_data_raw", str_c(MCnum, ".csv")),
                     show_col_types = F)
  
  # make vector of active port numbers
  port.vec <- zl6.db.long %>%
    filter(MC.ID == MCnum) %>%
    select(MC.ID, Port, Station) %>%
    na.omit() 
  
  lapply(port.vec$Port, MC_to_port, df = rawdat) %>% 
    bind_rows()  
}

# Applies "MC_to_datalogger" to all dataloggers in a tree and binds the results together
# TreeID: Name of a tree
TreeID <- "TV2"
MC_to_tree <- function(TreeID){
  ZLs <- zl6.db %>% 
    filter(Location == TreeID) %>% 
    pull(MC.ID)
  d <- lapply(ZLs, MC_to_datalogger) %>% 
    bind_rows()
  return(d)
}

# Filters the output of "MC_to_tree" to an individual station and outputs the df with all variables
# StationNum: A station ID, such as "S1"; d.nested: A nested df output from "MC_to_tree"
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

fix_names <- function(x){
  v <- names(x)
  no.port <- str_sub(v[4: length(v)], start = 7)
  names(x) <- c(v[1:3], no.port)
  return(x)
}

# Function to correct common problems with the variables, such as RH of >1 or NA in VPD where RH >1
correct_common_errors <- function(x){
  var.names = colnames(x)
  if("VPD" %in% var.names & "RH" %in% var.names){
    x2 <- x %>% 
      mutate(VPD = ifelse(RH >= 1, 0, VPD),
             RH = ifelse(RH > 1, 1, RH))
  } else {
    x2 <- x
  }
  return(x2)
}

calc_VPD <- function(a, b, c, Dat){
  e_s <- a * exp((b*Dat$Temp)/(Dat$Temp + c))
  vap <- e_s * Dat$RH
  VPD <- (e_s) - vap
  VPD_rnd <- round(VPD, 2)
  return(VPD_rnd)
}