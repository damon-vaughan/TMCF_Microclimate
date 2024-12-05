summarise_MC <- function(x){
  create_summary_table <- list(
    Xmin = ~min(.x, na.rm = TRUE),
    Xmean = ~mean(.x, na.rm = TRUE),
    Xmax = ~max(.x, na.rm = TRUE),
    Xtotal = ~sum(.x, na.rm = TRUE)
  )
  
  x2 <- x %>%
    group_by(Station) %>% 
    summarise(across(where(is.numeric), create_summary_table))
  
  x3 <- x2 %>%
    pivot_longer(2:ncol(x2), names_to = "var", values_to = "value") %>%
    separate(var, into = c("measure", "sumstat"), sep = "X") %>%
    pivot_wider(names_from = sumstat, values_from = value) %>%
    mutate(Change = max - min) %>% 
    mutate(measure = str_sub(measure, end = -2))
  
  return(x3)
}

full.tree.vec <- c("ET1", "ET2", "ET3", "ET4", "ET5", "ET6", "ET7", "ET8",
                   "FB1", "FB2", "FB3", "FB4", "FB5", "FB6", "FB7", "FB8", 
                   "TV1", "TV2", "TV3", "TV4")
