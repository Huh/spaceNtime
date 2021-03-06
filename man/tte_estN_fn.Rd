% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tte_estN_fn.R
\name{tte_estN_fn}
\alias{tte_estN_fn}
\title{Estimate abundance with time-to-event (TTE)}
\usage{
tte_estN_fn(eh, study_area)
}
\arguments{
\item{eh}{TTE encounter history. A data.frame formulated by tte_build_eh}

\item{study_area}{Size of the study area (same units as camera viewshed)}
}
\value{
A data.frame with the estimated abundance with its standard error and confidence intervals
}
\description{
Estimate abundance with time-to-event (TTE)
}
\examples{
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
deploy <- data.frame(
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
study_dates <- as.POSIXct(c("2016-01-01 00:00:00", "2016-01-04 23:59:59"), tz = "GMT")
occ <- build_occ(samp_freq = 3600 * 10,
                 samp_length = 3600 * 10, 
                 study_start = study_dates[1],
                 study_end = study_dates[2]) 
tte_eh <- tte_build_eh(df, deploy, occ)
tte_estN_fn(tte_eh, 1e6)
}
