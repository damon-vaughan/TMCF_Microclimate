# Removes all but the last duplicated timestamp record.
# x <- d.nst$data[[6]]
fix_duplicate_timestamps_old <- function(x){
  x2 <- x %>% 
    distinct()
  
  dupe.time <- x2  %>%
    mutate(Timestamp2 = lag(Timestamp)) %>% 
    ungroup() %>% 
    filter(Timestamp == Timestamp2) %>% 
    slice(-1)
  
  suppressMessages(x3 <- x2 %>% 
                     anti_join(dupe.time))
  
  return(x3)
}
