% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_and_clean_NOAA_data.R
\name{load_NOAA_db}
\alias{load_NOAA_db}
\title{Read the NOAA significant Earth quake data file 'inst/extdata/signif.txt}
\usage{
load_NOAA_db(data_file)
}
\arguments{
\item{data_file}{character string}
}
\value{
a tbl_df dataframe
}
\description{
Check the existance of the input file.
Read the file as a tsvm tab seperated file
}
\examples{
\dontrun{
source_noaa <- system.file("extdata","signif.txt",package="NOAAsignifEarthQuakes",mustWork=TRUE)
all_data <- load_NOAA_db(source_noaa)
}
}
