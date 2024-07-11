library(needs)
needs(tidyverse, readxl)

source("3_Functions_Microclimate.R")

options(readr.show_col_types = FALSE)
theme_set(theme_bw())

RH.low.maxes.global <- c("ET2_S4", "ET7_S4", "FB4_S1", "FB4_S4", "TV3_S2")
RH.low.maxes.local <- c("FB8_S5")

# Load general data ------------------------------------------------------------

bad.data <- read_excel(file.path("Microclimate_data_supporting", "MC_bad_data_windows_Damon.xlsx")) %>%
  select(-Note) 

bad.data.nst <- bad.data %>% 
  group_by(Tree, Station, Variable) %>% 
  nest() %>% 
  mutate(Data.expanded = map(data, expand_data_window, "15 min")) %>% 
  select(-data)

bad.data.expanded <- bad.data.nst  %>% 
  unnest(Data.expanded) %>% 
  ungroup() %>%
  rename(Bad.data = Data.flag)

# i <- "FB8"
tree.vec <- full.tree.vec
ground.vec <- full.ground.vec

MC.vec <- c(tree.vec, ground.vec)

# MC.vec <- c("ET2", "ET8", "FB4", "FB6", "FB8", "TV3")
# MC.vec <- c("TV3")

# MC.vec <- "FB4"
# i <- "FB8"
# !!!! SLOW !!!!!
for(i in MC.vec){
  d <- read_csv(file.path("Microclimate_data_L3", str_c(i, "_MC", "_L3.csv")))

  ## Remove bad data ---------------------------------------------------------
    
  BDE <- bad.data.expanded %>% 
    filter(Tree == i)
  
  d2 <- d %>% 
    pivot_longer(4:12, names_to = "Variable", values_to = "Value") %>%
    left_join(BDE, by = c("Tree", "Station", "Variable", "Timestamp")) %>%
    mutate(Value = ifelse(!is.na(Bad.data), NA, Value)) %>%
    select(-Bad.data) %>%
    pivot_wider(names_from = Variable, values_from = Value)
  
  # These thresholds are determined in the lower section of the script
  d3 <- d2 %>% 
    mutate(Temp = ifelse(Temp == 0, NA, Temp),
           RH = ifelse(RH == 0, NA, RH),
           Atmos_pressure = ifelse(Atmos_pressure == 0, NA, Atmos_pressure),
           Atmos_pressure = ifelse(Atmos_pressure > 100, NA, Atmos_pressure),
           Wetness = ifelse(Wetness > 2500, NA, Wetness))
  
  # RH fixes ------------------------------------------------------------------

  # Change all RH > 1 to 1, then nest the df for the adjustment functions
  d3.nst <- d3 %>% 
    mutate(RH = ifelse(RH > 1, 1, RH)) %>% 
    mutate(Key = str_c(i, "_", Station)) %>% 
    group_by(Station) %>%
    nest() %>% 
    ungroup()
  # str(d3.nst, max.level = 1)
  
  d4 <- d3.nst %>% 
    mutate(data = map(data, adjust_RH_maxes_global)) %>% 
    mutate(data = map(data, adjust_RH_maxes_local)) %>% 
    unnest(data) %>%
    ungroup() %>% 
    select(-Key)

  # Calculate variables -----------------------------------------------------

  d5 <- d4 %>% 
    mutate(VPD = calculate_VPD(Temp, RH))
  
  # Save --------------------------------------------------------------------
  
  write_csv(d5, file.path("Microclimate_data_L4",
                          str_c(i, "_MC", "_L4.csv")))
  cat("saved data for: ", i, "\n")
}



# Check data ----------------------------------------------------------

all.files <- list.files("Microclimate_data_L4", pattern = ".csv", full.names = TRUE)

files.tree <- all.files[which(str_detect(all.files, "P") == F)]
files.ground <- all.files[which(str_detect(all.files, "P") == T)]

d.tree <- lapply(files.tree, read_csv) %>% 
  bind_rows()
d.ground <- lapply(files.ground, read_csv) %>% 
  bind_rows()

## Duplicates --------------------------------------------------------------

# !!!SLOW!!! for tree
check_dupes_TST(d.tree)
check_dupes_TST(d.ground)

## Solar -------------------------------------------------------------------
# Completed for all data up to early June 2023

ggplot(d.tree %>% filter(Solar != 0)) +
  geom_density(aes(x = Solar)) +
  theme_bw()
summary(d.tree$Solar)

# Low outliers
which(d.tree$Solar < 0)
which(d.ground$Solar < 0)

# High outliers
length(which(LVL2_trees$Solar > 1200))
d <- d.tree %>% 
  filter(Solar > 1200)
d <- d.ground %>% 
  filter(Solar > 1200)

# Solar at night
d <- d.tree %>% 
  filter(hour(Timestamp) >= 19 | hour(Timestamp) < 5) %>% 
  filter(Solar != 0)
d <- d.ground %>% 
  filter(hour(Timestamp) >= 19 | hour(Timestamp) < 5) %>% 
  filter(Solar != 0)

# Do any series never reach zero?
check <- d.tree %>% 
  filter(Station != "S5" & Station != "CS") %>% 
  group_by(Tree, Station) %>% 
  summarise(minSol = min(Solar, na.rm = T))
check <- d.ground %>% 
  group_by(Tree) %>% 
  summarise(minSol = min(Solar, na.rm = T))

## Temp --------------------------------------------------------------------

ggplot(d.tree) +
  geom_density(aes(x = Temp)) +
  theme_bw()
summary(d.tree$Temp)

# Low outliers
d <- d.tree %>% 
  filter(Temp < 12)
d <- d.ground %>% 
  filter(Temp < 12)

# High outliers
d <- d.tree %>% 
  filter(Temp > 28)
d <- d.ground %>% 
  filter(Temp > 28)


## RH ----------------------------------------------------------------------

ggplot(d.tree) +
  geom_density(aes(x = RH)) +
  theme_bw()
summary(d.tree$RH)

# RH of zero
d <- d.tree %>% 
  filter(RH == 0)
d <- d.ground %>% 
  filter(RH == 0)

# Low outliers
d <- d.tree %>% 
  filter(RH <= 0.4)
d <- d.ground %>% 
  filter(RH <= 0.4)

# Series that never reach 1. These need to be corrected
check <- d.tree %>% 
  filter(Station != "CS") %>% 
  group_by(Tree, Station) %>% 
  summarise(maxRH = max(RH, na.rm = T))
check <- d.ground %>% 
  group_by(Tree) %>% 
  summarise(maxRH = max(RH, na.rm = T))

# Special cases
FB8_S5 <- d.tree %>% 
  filter(Tree == "FB8" & Station == "S5") %>% 
  select(Timestamp, RH) %>% 
  group_by(Timestamp = floor_date(Timestamp, "days")) %>%
  summarise(RH.max = max(RH, na.rm = T))
# Use 0.988

## Atmos_pressure ----------------------------------------------------------
# This is generally in a very narrow range and there are only 2 weird spikes to deal with

ggplot(d.tree) +
  geom_density(aes(x = Atmos_pressure)) +
  theme_bw()
summary(d.tree$Atmos_pressure)

# Low outliers
d <- d.tree %>% 
  filter(Atmos_pressure <= 84)
d <- d.ground %>% 
  filter(Atmos_pressure <= 84)

# High outliers
d <- d.tree %>% 
  filter(Atmos_pressure >= 90)
d <- d.ground %>% 
  filter(Atmos_pressure >= 90)

## VPD ---------------------------------------------------------------------
ggplot(d.tree) +
  geom_density(aes(x = VPD)) +
  theme_bw()
summary(d.tree$VPD)

# low outliers
d <- d.tree %>%
  filter(VPD < 0)
d <- d.ground %>%
  filter(VPD < 0)

# High outliers
d <- d.tree %>%
  filter(VPD > 1.7)

# Series that never reach 0. These need to be corrected
check <- d.tree %>%
  filter(Station != "CS") %>% 
  group_by(Tree, Station) %>%
  summarise(minVPD = min(VPD, na.rm = T))

## LWS ---------------------------------------------------------------------
ggplot(d.tree) +
  geom_density(aes(x = Wetness)) +
  theme_bw()
summary(d.tree$Wetness)

# Low outliers
d <- d.tree %>% 
  filter(Wetness < 20)
d <- d.ground %>% 
  filter(Wetness < 20)

# High outliers
d <- d.tree %>% 
  filter(Wetness > 1500)
d <- d.ground %>% 
  filter(Wetness > 1500)

## Wind speed --------------------------------------------------------------
ggplot(d.tree) +
  geom_density(aes(x = Wind_speed)) +
  theme_bw()
summary(d.tree$Wind_speed)

# High outliers
d <- d.tree %>% 
  filter(Wind_speed > 10)
d <- d.ground %>% 
  filter(Wind_speed > 10)

## Precip ------------------------------------------------------------------
ggplot(d.ground) +
  geom_density(aes(x = Precipitation)) +
  theme_bw()

d <- d.ground %>% 
  filter(Precipitation > 10)

## Flagging the data -------------------------------------------------------

bad.data <- read_excel(file.path("Microclimate_data_supporting",
                                 "Microclimate_bad_data_windows.xlsx")) %>% 
  select(Tree, MC, Station, Port, Sensor, Start, End)

zl.db <- read_csv(file.path("Microclimate_data_supporting",
                            "zl6_database_long.csv"))

# Look at specific series -------------------------------------------------

Tree.ID <- "FBP1"
d <- read_csv(file.path("Microclimate_data_L4", str_c(Tree.ID, "_MC_L4.csv")))

x <- "Solar"
plot_series <- function(x){
  ggplot(d) +
    geom_line(aes(x = Timestamp, y = .data[[x]]))
}

# Export ----------------------------------------------------------


# For Phillipe ------------------------------------------------------------

FB5 <- read_csv(file.path("Microclimate_data_L4", "FB5_MC_L4.csv"))

S1 <- FB5 %>% 
  filter(Station == "S1")
write_csv(S1, file.path("Data_export", "Philipp", "FB5_S1.csv"))

S2 <- FB5 %>% 
  filter(Station == "S2")
write_csv(S2, file.path("Data_export", "Philipp", "FB5_S2.csv"))

S3 <- FB5 %>% 
  filter(Station == "S3")
write_csv(S3, file.path("Data_export", "Philipp", "FB5_S3.csv"))

S4 <- FB5 %>% 
  filter(Station == "S4")
write_csv(S4, file.path("Data_export", "Philipp", "FB5_S4.csv"))

## For Microclimate paper --------------------------------------------------

all.files <- list.files("Microclimate_data_L4", pattern = ".csv", full.names = TRUE)

# Trees
files.tree <- all.files[which(str_detect(all.files, "P") == F)]
files.ground <- all.files[which(str_detect(all.files, "P") == T)]

d.tree <- lapply(files.tree, read_csv) %>% 
  bind_rows() %>% 
  filter(Timestamp >= ymd("2022-12-01") &
           Timestamp < ymd("2023-12-01")) %>% 
  select(Tree, Station, Timestamp, Solar, Temp, RH, VPD, Wetness, Wind_speed)

d.ground <- lapply(files.ground, read_csv) %>% 
  bind_rows() %>% 
  filter(Timestamp >= ymd("2022-12-01") &
           Timestamp < ymd("2023-12-01")) %>% 
  select(Tree, Station, Timestamp, Solar, Temp, RH, 
         VPD, Wetness, Wind_speed, Precipitation,
         "ECRN-100_Precipitation")

out.dir <- "C:/Users/User/OneDrive - University of Kentucky/TMCF/TMCF_Analysis/TMCF_data_processed/MC_paper"

write_csv(d.tree, file.path(out.dir, "Base_tree_data.csv"))
write_csv(d.ground, file.path(out.dir, "Base_ground_data.csv"))

# Check data processing ----------------------------------------
# Make sure nothing got scrambled in the various processing steps

filenames.init <- list.files(here("Microclimate_data_LVL1"), 
                             full.names = T, pattern = ".csv")
filenames.full <- filenames.init[!str_detect(filenames.init, "Data_check") &
                                   !str_detect(filenames.init, "P")]

filenames.ET <- filenames.full[which(str_detect(filenames.full, "ET") == T & 
                                       str_detect(filenames.full, "ET1") == F)]
filenames.TV <- filenames.full[which(str_detect(filenames.full, "TV") == T)]
filenames.FB <- filenames.full[which(str_detect(filenames.full, "FB") == T)]
filenames.sub <- filenames.full[which(str_detect(filenames.full, "ET1") == T | 
                                        str_detect(filenames.full, "FB5") == T |
                                        str_detect(filenames.full, "FB7") == T)]

# y is how many randoms per dataset that you want
make_randoms_LVL1 <- function(x, y){
  d <- read_csv(x)
  d2 <- d %>% 
    pivot_longer(4:ncol(d), names_to = "variable", values_to = "value") %>% 
    sample_n(y)
  return(d2)
}

output <- lapply(filenames.sub, make_randoms_LVL1, 1) %>% 
  bind_rows()

write_csv(output, "Microclimate_data_LVL1/data_check2.csv")


# Comparing ground stations -------------------------------------
filenames <- list.files(here("Microclimate_data_LVL2"))

f1 <- filenames[which(str_detect(filenames, "FBP1") == T)]
f2 <- filenames[which(str_detect(filenames, "FBP2") == T)]

TVP <- read_csv(here("Microclimate_data_LVL2", "Pasture_stations", f1)) %>% 
  mutate(Plot = "TVP")
FBP1 <- read_csv(here("Microclimate_data_LVL2", f1)) %>% 
  mutate(Plot = "FBP1")
FBP2 <- read_csv(here("Microclimate_data_LVL2", f2)) %>% 
  mutate(Plot = "FBP2")

together <- bind_rows(FBP1, FBP2) 
together2 <- together %>% 
  filter(Timestamp >= max(together$Timestamp) - months(3))

quick_comparison_plot <- function(x, Yaxis){
  ggplot(x, aes(x = Timestamp)) +
    geom_line(aes(y = .data[[Yaxis]], color = factor(PastureID))) +
    theme_bw() +
    ggtitle(Yaxis) 
}

quick_comparison_plot(together2, "Solar")
quick_comparison_plot(together2, "Temp")
quick_comparison_plot(together2, "RH")
quick_comparison_plot(together2, "Atmos_pressure")
quick_comparison_plot(together2, "VPD")
quick_comparison_plot(together2, "LW_minutes_H")
quick_comparison_plot(together2, "Wetness")
quick_comparison_plot(together2, "Wind_direction")
quick_comparison_plot(together2, "Wind_speed")
quick_comparison_plot(together2, "Gust_speed")
quick_comparison_plot(together2, "Precip_max")
quick_comparison_plot(together2, "Precipitation")


# Cool plots! ---------------------------------------------------
# Quick comparisons where each tree has a line, show minimal differences between control/exp

library(needs)
needs(tidyverse, readxl, here, lubridate)

FB1 <- read_csv(here("Microclimate_data_LVL2", "FB1_LVL2.csv")) %>% 
  mutate(Plot = "FB_Pasture_Close", Design = "Experimental")
FB2 <- read_csv(here("Microclimate_data_LVL2", "FB2_LVL2.csv")) %>% 
  mutate(Plot = "FB_Pasture_Close", Design = "Control")
FB3 <- read_csv(here("Microclimate_data_LVL2", "FB3_LVL2.csv")) %>% 
  mutate(Plot = "FB_Pasture_Far", Design = "Experimental")
FB4 <- read_csv(here("Microclimate_data_LVL2", "FB4_LVL2.csv")) %>% 
  mutate(Plot = "FB_Pasture_Far", Design = "Control")

FB <- bind_rows(FB1, FB2, FB3, FB4)

TV1 <- read_csv(here("Microclimate_data_LVL2", "TV1_LVL2.csv")) %>% 
  mutate(Plot = "TV_Forest", Design = "Experimental")
TV2 <- read_csv(here("Microclimate_data_LVL2", "TV2_LVL2.csv")) %>% 
  mutate(Plot = "TV_Forest", Design = "Control")
TV3 <- read_csv(here("Microclimate_data_LVL2", "TV3_LVL2.csv")) %>% 
  mutate(Plot = "TV_Pasture", Design = "Experimental")
TV4 <- read_csv(here("Microclimate_data_LVL2", "TV4_LVL2.csv")) %>% 
  mutate(Plot = "TV_Pasture", Design = "Control")

TV <- bind_rows(TV1, TV2, TV3, TV4)

plot_ctrl_exp <- function(x, z){
  ggplot(x, 
         aes(x = Timestamp_Hourly, color = factor(Design))) +
    geom_smooth(aes_string(y = z), method = "gam") +
    labs(color = "Design") +
    theme_bw() +
    facet_wrap(~Plot, ncol = 1) +
    ggtitle(str_c(z))
}
plot_ctrl_exp(TV, "Solar")
plot_ctrl_exp(TV, "Temp")
plot_ctrl_exp(TV, "RH")
plot_ctrl_exp(TV, "Atmos_pressure")
plot_ctrl_exp(TV, "VPD")
plot_ctrl_exp(TV, "Wetness")
plot_ctrl_exp(TV, "Wind_speed")
plot_ctrl_exp(TV, "Gust_speed")

plot_ctrl_exp(FB, "Solar")
plot_ctrl_exp(FB, "Temp")
plot_ctrl_exp(FB, "RH")
plot_ctrl_exp(FB, "Atmos_pressure")
plot_ctrl_exp(FB, "VPD")
plot_ctrl_exp(FB, "Wetness")
plot_ctrl_exp(FB, "Wind_speed")
plot_ctrl_exp(FB, "Gust_speed")





