library(needs)
needs(tidyverse, readxl, here, lubridate)

zl6.db.long <- read_csv(file.path("Microclimate_data_supporting",
                                  "zl6_database_long.csv"))

zl6.db <- read_csv(file.path("Microclimate_data_supporting",
                             "zl6_database.csv"))

filenames.toJune <- list.files(
  file.path("Microclimate_data_L2", "Microclimate_toJune2023_L2"), 
  pattern = ".csv", full.names = T)
filenames.toJune.pasture <- filenames.toJune[which(str_detect(filenames.toJune, "P") == T)]
filenames.toJune.tree <- filenames.toJune[which(str_detect(filenames.toJune, "P") == F)]

toJune.trees <- lapply(filenames.toJune.tree, read_csv, show_col_types = F) %>%
  bind_rows() %>% 
  filter(Timestamp >= ymd("2022-09-01", tz = "UTC"))
toJune.pasture <- lapply(filenames.toJune.pasture, read_csv, show_col_types = F) %>% 
  bind_rows()

filenames.L2 <- list.files(file.path("Microclimate_data_L2"), 
                           pattern = ".csv", full.names = T)
filenames.tree <- filenames.L2[which(str_detect(filenames.L2, "P") == F)]
filenames.pasture <- filenames.L2[which(str_detect(filenames.L2, "P") == T)]

L2.trees <- lapply(filenames.tree, read_csv, show_col_types = F) %>%
  bind_rows()
L2.pasture <- lapply(filenames.pasture, read_csv, show_col_types = F) %>%
  bind_rows()

# Data integrity ----------------------------------------------------------

## Duplicate timestamp problem ---------------------------------------------

L2.trees %>% 
  group_by(Tree, Station, Timestamp) %>% 
  summarise(length = n()) %>% 
  filter(length > 1)

test <- L2.trees %>% 
  filter(Tree == "ET2") %>% 
  filter(Station == "S3") %>% 
  filter(Timestamp >= ymd_hms("2023-06-07 00:15:00", tz = "UTC"))

toJune.trees %>% 
  group_by(Tree, Station, Timestamp) %>% 
  summarise(length = n()) %>% 
  filter(length > 1)

check_for_dupes <- function(x){
  x %>% 
    group_by(datetime) %>% 
    summarise(length = n()) %>% 
    filter(length > 1)
}
check_for_dupes(MC[[1]])
check_for_dupes(MC[[2]])
check_for_dupes(MC[[3]])
check_for_dupes(MC[[4]])
check_for_dupes(MC[[5]])

# Joining like this makes 2 dupes in one port and 2 dupes at the same time in another port become a total of 4
test <- MC[[1]] %>% 
  left_join(MC[[3]], by = "datetime")


# Bad data checks ---------------------------------------------------------

## Solar -------------------------------------------------------------------
# Completed for all data up to early June 2023

ggplot(LVL2_trees %>% filter(Solar != 0)) +
  geom_density(aes(x = Solar)) +
  theme_bw()
summary(LVL2_trees$Solar)

# Low outliers
which(LVL2_trees$Solar < 0)
which(LVL2_pasture$Solar < 0)

# High outliers
length(which(LVL2_trees$Solar > 1200))
d <- LVL2_trees %>% 
  filter(Solar > 1200)
d <- LVL2_pasture %>% 
  filter(Solar > 1200)

# Solar at night
d <- LVL2_trees %>% 
  filter(hour(Timestamp) >= 19 | hour(Timestamp) < 5) %>% 
  filter(Solar != 0)
d <- LVL2_pasture %>% 
  filter(hour(Timestamp) >= 19 | hour(Timestamp) < 5) %>% 
  filter(Solar != 0)

# Do any series never reach zero?
check <- LVL2_trees %>% 
  group_by(Tree, Station) %>% 
  summarise(minSol = min(Solar, na.rm = T))
check <- LVL2_pasture %>% 
  group_by(PastureID) %>% 
  summarise(minSol = min(Solar, na.rm = T))

## Temp --------------------------------------------------------------------

ggplot(LVL2_trees) +
  geom_density(aes(x = Temp)) +
  theme_bw()
summary(LVL2_trees$Temp)

# Low outliers
d <- LVL2_trees %>% 
  filter(Temp < 12)
d <- LVL2_pasture %>% 
  filter(Temp < 12)

# High outliers
d <- LVL2_trees %>% 
  filter(Temp > 28)
d <- LVL2_pasture %>% 
  filter(Temp > 28)


## RH ----------------------------------------------------------------------

ggplot(LVL2_trees) +
  geom_density(aes(x = RH)) +
  theme_bw()
summary(LVL2_trees$RH)

# RH of zero
d <- LVL2_trees %>% 
  filter(RH == 0)
d <- LVL2_pasture %>% 
  filter(RH == 0)

# Low outliers
d <- LVL2_trees %>% 
  filter(RH != 0) %>% 
  filter(RH <= 0.4)
d <- LVL2_pasture %>% 
  filter(RH != 0) %>% 
  filter(RH <= 0.4)

# Series that never reach 1. These need to be corrected
check <- LVL2_trees %>% 
  group_by(Tree, Station) %>% 
  summarise(maxRH = max(RH, na.rm = T))
check <- LVL2_pasture %>% 
  group_by(PastureID) %>% 
  summarise(maxRH = max(RH, na.rm = T))


## Atmos_pressure ----------------------------------------------------------
# This is generally in a very narrow range and there are only 2 weird spikes to deal with

ggplot(LVL2_trees) +
  geom_density(aes(x = Atmos_pressure)) +
  theme_bw()
summary(LVL2_trees$Atmos_pressure)

# Low outliers
d <- LVL2_trees %>% 
  filter(Atmos_pressure <= 84)
d <- LVL2_pasture %>% 
  filter(Atmos_pressure <= 84)

# High outliers
d <- LVL2_trees %>% 
  filter(Atmos_pressure >= 90)
d <- LVL2_pasture %>% 
  filter(Atmos_pressure >= 90)

## VPD ---------------------------------------------------------------------
ggplot(LVL2_trees) +
  geom_density(aes(x = VPD)) +
  theme_bw()
summary(LVL2_trees$VPD)

# High outliers
d <- LVL2_trees %>% 
  filter(VPD > 1.7)

# Series that never reach 0. These need to be corrected
check <- LVL2_trees %>% 
  group_by(Tree, Station) %>% 
  summarise(minVPD = min(VPD, na.rm = T))

## LWS ---------------------------------------------------------------------
ggplot(LVL2_trees) +
  geom_density(aes(x = Wetness)) +
  theme_bw()
summary(LVL2_trees$Wetness)

# Low outliers
d <- LVL2_trees %>% 
  filter(Wetness < 10)

# High outliers
d <- LVL2_trees %>% 
  filter(Wetness > 1500)
d <- LVL2_pasture %>% 
  filter(Wetness > 1500)

## Wind speed --------------------------------------------------------------
ggplot(LVL2_trees) +
  geom_density(aes(x = Wind_speed)) +
  theme_bw()
summary(LVL2_trees$Wind_speed)

# High outliers
d <- LVL2_trees %>% 
  filter(Wind_speed > 10)
d <- LVL2_pasture %>% 
  filter(Wind_speed > 10)

## Precip ------------------------------------------------------------------
ggplot(LVL2_pasture) +
  geom_density(aes(x = Wind_speed)) +
  theme_bw()

## Flagging the data -------------------------------------------------------

bad.data <- read_excel(file.path("Microclimate_data_supporting",
                                 "Microclimate_bad_data_windows.xlsx")) %>% 
  select(Tree, MC, Station, Port, Sensor, Start, End)

zl.db <- read_csv(file.path("Microclimate_data_supporting",
                            "zl6_database_long.csv"))

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


# Comparing pasture stations -------------------------------------
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


  
  
