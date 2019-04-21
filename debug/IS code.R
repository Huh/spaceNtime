library(tidyverse)

# What we want... 
# Dataset
cam_occ_EH <- tibble(cam = rep(c(1,2), each = 3), 
                  step = rep(1:3, 2),
                  nanimals = c(0, 2, 0, 0, 0, 1),
                  a = c(901, 891.2, 910.9, 700.6, 651.5, 851.1))

# Create IS encounter history and run model
is_estN_fn <- function(cam_occ_EH, A){
  
  is_est <- cam_occ_EH %>%
    mutate(dens_ij = nanimals/a) %>%
    summarise(D = mean(dens_ij, na.rm = T) ) %>%
    mutate(N = D * A)
  
  return(is_est)
}

is_estN <- is_estN_fn(
    cam_occ_EH,
    A = 10000
    )
is_estN


# What we actually have...

df <- data.frame(
  cam = c(1,1,2,2,2),
  datetime = as.POSIXct(c("2016-01-02 12:00:00",
                          "2016-01-03 13:12:00",
                          "2016-01-02 12:00:00",
                          "2016-01-02 14:00:00",
                          "2016-01-03 16:53:42"),
                        tz = "GMT"),
  count = c(1, 0, 2, 1, 2)
)
cam_areas <- data.frame(
  cam = c(1, 2, 2, 2),
  start = as.POSIXct(c("2015-12-01 15:00:00",
                       "2015-12-08 00:00:00", 
                       "2016-01-01 00:00:00", 
                       "2016-01-02 00:00:00"),
                     tz = "GMT"),
  end = as.POSIXct(c("2016-01-05 00:00:00", 
                     "2015-12-19 03:30:00", 
                     "2016-01-01 05:00:00",
                     "2016-01-05 00:00:00"), 
                   tz = "GMT"),
  area = c(300, 200, 200, 450)
)