#' Create html labels for Earthquke data
#'
#' @description
#' Create popup content as a html encoded character.
#' Indicates location if value is not missing (NA),
#' richter scale magnitude if value is not missing(NA),
#' total number of deaths if value is not missing(NA).
#'
#' @param data processed NOAA data
#'
#' @importFrom htmltools htmlEscape
#'
#' @usage eq_create_label(data)
#'
#' @return character of the same lenght as the input data
#' @export
#'
#' @examples
#' \dontrun{
#' noaa <- load_NOAA_db(file_noaa)%>% eq_clean_data()
#' noaa %>%eq_create_label()
#' noaa_with_label -> noaa %>%
#'     dplyr::mutate("popup_text"=eq_create_label(.))
#' }
#'
eq_create_label <- function(data){

    paste(
        # if LOCATION_NAME is null set label to empty string
        ifelse(is.na(data$LOCATION_NAME),'',
               # else prepare location label
               paste("<b>Location:</b>",
                     htmltools::htmlEscape(data$LOCATION_NAME),
                     "</br>")
        ),
        # if MAG is null set label to empty string
        ifelse(is.na(data$MAG),'',
               # else prepare magnitude label
               paste("<b>Magnitude:</b> ",
                     htmltools::htmlEscape(data$MAG),
                     "</br>")
        ),
        # if DEATHS is null set label to empty string
        ifelse(is.na(data$DEATHS),'',
               # else prepare Total deaths label
               paste("<b>Total deaths:</b>",
                     htmltools::htmlEscape(data$DEATHS),
                     "</br>")
        )
    )
}



#' Interactive map vizualization of earthquakes
#'
#' @description From the NOAA data prostreated through eq_clean()
#' displays an interactive map of Earthquakes locations. Each earthquake is represented by a circle
#' with a radius representing the Richter scale Magnitude. Passing "DATE" to option annot_col enable to display
#' the earthquake date in a popup window. Passing "popup_text" instead unable to display Earthquake location,
#' magnitude and total number of victim if available.
#'
#' @param data Processed NOAA data
#' @param annot_col optional argument defining popup content,
#' annot_col = "DATE" print the date of eartquke in the popup,
#' annot_col = "popup_text" print location, mangitude and total deaths
#'
#' @importFrom dplyr mutate
#' @importFrom htmltools htmlEscape
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#'
#' @usage eq_map(data,annot_col='')
#'
#' @return leaflet map
#' @export
#'
#' @examples
#' \dontrun{
#' file_noaa <- system.file("extdata","signif.txt",package="NOAAsignifEarthQuakes",mustWork=TRUE)
#' filt_noaa <- load_NOAA_db(file_noaa)%>% eq_clean_data() %>%
#'    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000)
#'
#'    dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'    eq_map(annot_col = "popup_text")
#'
#' }
eq_map <- function(data,annot_col=''){

    DATE <- NULL
    if(annot_col=="DATE"){
        # set popup annotation to Date
        data <- data %>% dplyr::mutate(popup_text=htmltools::htmlEscape(DATE))
    } else if(annot_col=="popup_text"){
        if(! "popup_text" %in% names(data)){
            data <- data %>% dplyr::mutate(popup_text=eq_create_label(data))
        }
    } else (
        data <- data %>% dplyr::mutate(popup_text=NULL)
    )
    # Create leaflet map
    # add circle marker for each eartquake
    lfmap <- leaflet::leaflet(data) %>%
        leaflet::addTiles()

    if("popup_text" %in% names(data)){
        lfmap %>% leaflet::addCircleMarkers(
            lng=~LONGITUDE, lat=~LATITUDE,
            radius=~MAG,weight=1,
            popup=~popup_text
        )
    } else {
        lfmap %>% leaflet::addCircleMarkers(
            lng=~LONGITUDE, lat=~LATITUDE,
            radius=~MAG,weight=1)
    }

}
