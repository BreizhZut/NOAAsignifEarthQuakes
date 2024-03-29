% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_NOAA.R
\name{eq_map}
\alias{eq_map}
\title{Interactive map vizualization of earthquakes}
\usage{
eq_map(data,annot_col='')
}
\arguments{
\item{data}{Processed NOAA data}

\item{annot_col}{optional argument defining popup content,
annot_col = "DATE" print the date of eartquke in the popup,
annot_col = "popup_text" print location, mangitude and total deaths}
}
\value{
leaflet map
}
\description{
From the NOAA data prostreated through eq_clean()
displays an interactive map of Earthquakes locations. Each earthquake is represented by a circle
with a radius representing the Richter scale Magnitude. Passing "DATE" to option annot_col enable to display
the earthquake date in a popup window. Passing "popup_text" instead unable to display Earthquake location,
magnitude and total number of victim if available.
}
\examples{
\dontrun{
file_noaa <- system.file("extdata","signif.txt",package="NOAAsignifEarthQuakes",mustWork=TRUE)
filt_noaa <- load_NOAA_db(file_noaa)\%>\% eq_clean_data() \%>\%
   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000)

   dplyr::mutate(popup_text = eq_create_label(.)) \%>\%
   eq_map(annot_col = "popup_text")

}
}
