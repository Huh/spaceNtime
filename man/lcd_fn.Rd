% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lcd_fn.R
\name{lcd_fn}
\alias{lcd_fn}
\title{Find the lowest common denominator between occasions}
\usage{
lcd_fn(occ)
}
\arguments{
\item{occ}{occasions, a dataframe}
}
\value{
Returns a value: either the lowest common denominator between occasions 
if they are evenly spaced (equal to samp_freq if occ is built by build_occ) 
or 0 if occasions are not evenly spaced
}
\description{
Find the lowest common denominator between occasions
}
\examples{
  occ <- build_occ(samp_freq = 3600, 
            samp_length = 10,
            study_start = study_dates[1],
            study_end = study_dates[2])
 findfrq <- lcd_fn(occ)
}
