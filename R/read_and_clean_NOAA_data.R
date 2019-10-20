#' Read the NOAA significant Earth quake data file 'inst/extdata/signif.txt
#'
#' @description
#' Check the existance of the input file.
#' Read the file as a tsvm tab seperated file
#'
#' @param data_file character string
#'
#' @usage load_NOAA_db(data_file)
#'
#' @importFrom readr read_tsv col_double col_character col_integer
#'
#' @return a tbl_df dataframe
#'
#' @examples
#' \dontrun{
#' source_noaa <- system.file("extdata","signif.txt",package="NOAAsignifEarthQuakes",mustWork=TRUE)
#' all_data <- load_NOAA_db(source_noaa)
#' }
#' @export
load_NOAA_db <- function(data_file){

    # Schema specification
    col_types <- list(
        readr::col_character(), # I_D
        readr::col_character(), # FLAG_TSUNAMI
        readr::col_integer(),   # YEAR
        readr::col_integer(),   # MONTH
        readr::col_integer(),   # DAY
        readr::col_integer(),   # HOUR
        readr::col_integer(),   # MINUTE
        readr::col_double(),    # SECOND
        readr::col_integer(),   # FOCAL_DEPTH
        readr::col_double(),    # EQ_PRIMARY
        readr::col_double(),    # EQ_MAG_MW
        readr::col_double(),    # EQ_MAG_MS
        readr::col_double(),    # EQ_MAG_MB
        readr::col_double(),    # EQ_MAG_ML
        readr::col_double(),    # EQ_MAG_MFA
        readr::col_double(),    # EQ_MAG_UNK
        readr::col_integer(),   # INTENSITY
        readr::col_character(), # COUTRY
        readr::col_character(), # STATE
        readr::col_character(), # LOCATION_NAME
        readr::col_double(),    # LATITUDE
        readr::col_double(),    # LONGITUDE
        readr::col_character(), # REGION_CODE
        readr::col_integer(),   # DEATHS
        readr::col_character(), # DEATHS_DESCRIPTION
        readr::col_integer(),   # MISSING
        readr::col_character(), # MISSING_DESCRIPTION
        readr::col_integer(),   # INJURIES
        readr::col_character(), # INJURIES_DESCRIPTION
        readr::col_double(),    # DAMAGE_MILLIONS_DOLLARS
        readr::col_character(), # DAMAGE_DESCRIPTION
        readr::col_integer(),   # HOUSES_DESTROYED
        readr::col_character(), # HOUSES_DESTROYED_DESCRIPTION
        readr::col_integer(),   # HOUSES_DAMAGED
        readr::col_character(), # HOUSES_DAMAGED_DESCRIPTION
        readr::col_integer(),   # TOTAL_DEATHS
        readr::col_character(), # TOTAL_DEATHS_DESCRIPTION
        readr::col_integer(),   # TOTAL_MISSING
        readr::col_character(), # TOTAL_MISSING_DESCRIPTION
        readr::col_integer(),   # TOTAL_INJURIES
        readr::col_character(), # TOTAL_INJURIES_DESCRIPTION
        readr::col_double(),    # TOTAL_DAMAGE_MILLIONS_DOLLARS
        readr::col_character(), # TOTAL_DAMAGE_DESCRIPTION
        readr::col_integer(),   # TOTAL_HOUSES_DESTROYED
        readr::col_character(), # TOTAL_HOUSES_DESTROYED_DESCRIPTION
        readr::col_integer(),   # TOTAL_HOUSES_DAMAGED
        readr::col_character()  # TOTAL_HOUSES_DAMAGED_DESCRIPTION
    )

    # Check file existence
    if(! file.exists(data_file)){
        stop(paste("Missing input file",data_file))
    }
    # read file
    readr::read_tsv(data_file,col_types=col_types)
}

#' Combine year, month and day numeric features into a date.
#'
#' @description
#' Take 3 numeric as input: year, mounth and day (of the month).
#' Year must be a valid 4 digit year. If month is NA, month is set to 01 (January),
#' if day is NA, day of the month is set to 1.
#'
#' @param y_raw year numeric
#' @param m_raw month numeric
#' @param d_raw date numeric
#'
#' @usage build_date(y_raw,m_raw,d_raw)
#'
#' @details
#' If year is null return NA
#' If month is null set moth to january
#' If day is null set day to 1
#'
#' @importFrom lubridate ymd year month day
#'
#' @return Date
#'
build_date <- function(y_raw,m_raw,d_raw){
    if(is.na(y_raw)) {
        NA
    } else{
        dt <- lubridate::ymd("0000-01-01")
        lubridate::year(dt) <- y_raw
        if(!is.na(m_raw)) {lubridate::month(dt) <- m_raw}
        if(!is.na(d_raw)) {lubridate::day(dt) <- d_raw}
        dt
    }
}

#' Building dates form NOAA raw data
#'
#'@description
#' Take 3 numeric as input: year, mounth and day (of the month).
#' Year must be a valid 4 digit year. If month is NA, month is set to 01 (January),
#' if day is NA, day of the month is set to 1.
#'
#' @param data raw NOAA data
#'
#' @usage eq_build_date(data)
#'
#' @return Date
#' @export
#'
#' @examples
#' \dontrun{
#' source_noaa <- system.file("extdata","signif.txt",package="NOAAsignifEarthQuakes",mustWork=TRUE)
#' noaa_raw <- load_NOAA_db(source_noaa)
#' noaa_raw %>%dplyr::select('DAY','MONTH','YEAR') %>% eq_build_date()
#' }
eq_build_date <- function(data){
    exp_date <- paste0(
        "0000",
        ifelse(is.na(data$MONTH),'01',stringr::str_pad(data$MONTH,2,pad = "0")),
        ifelse(is.na(data$DAY),'01',stringr::str_pad(data$DAY,2,pad = "0"))
    )
    exp_date <- exp_date %>% lubridate::ymd()
    lubridate::year(exp_date) <- data$YEAR
    exp_date
}

#' Capitalize each word
#'
#' @description
#' Take a character string as input, split into words
#' the first letter of each word set to upper case next to lower case
#'
#' @param x input character string
#'
#' @usage simpleCap(x)
#'
#' @return character string
#'
simpleCap <- function(x) {
    s <- strsplit(x, "[ -]")[[1]]
    x <- paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)),sep = "", collapse = " ")
    s <- strsplit(x, "-")[[1]]
    paste(toupper(substring(s, 1, 1)),substring(s, 2),sep = "", collapse = "-")
}

#' Format the LOCATION_NAME colomn.
#'
#' @description
#' Feature LOCATION_NAME is encoded as COUNTRY: Location. All set to upper case.
#' Remove the country and capitalize the rest.
#'
#' @param location character string
#'
#' @usage clean_location(location)
#'
#' @return chahracter string
#'
clean_location <-function(location){
    # detect and remove Country
    loc_without_country <- sub("^[A-Z :]*:[ ]*","",location)
    # add space after any commar
    loc_without_country <- sub(",$",", ",loc_without_country)
    # remaove aditionnal spaces
    loc_without_country <- sub("[ ]{2,}"," ",loc_without_country)
    # remove everything between parethesis
    loc_without_country <- sub("\\(.*\\)","",loc_without_country)
    #simpleCap(loc_without_country)
    loc_without_country
}

#' Clean Location Names in NOAA data
#'
#' @description
#' Remove countries and unnecessary test,
#' clean spaces and switch to title cap.
#'
#' @usage eq_build_location(data)
#'
#' @param data raw NOAA data
#'
#' @importFrom stringr str_trim str_to_title
#'
#' @return charater
#' @export
#'
#' @examples
#' \dontrun{
#' source_noaa <- system.file("extdata","signif.txt",package="NOAAsignifEarthQuakes",mustWork=TRUE)
#' noaa_raw <- load_NOAA_db(source_noaa)
#' noaa_raw %>%dplyr::select('LOCATION_NAME') %>% eq_build_date()
#' }
eq_build_location <- function(data){
    # remove country and anything between parenthesis
    x <- gsub("(^[A-Z :]*:[ ]*|\\(.*\\)|;.*$)","",data$LOCATION_NAME)
    # add space after comma
    x <- gsub(",",", ",x)
    # remove extra spaces
    x <- stringr::str_trim(gsub(" {2,}"," ",x))
    # switch cases
    stringr::str_to_title(x)
}

#' Perform the cleaning of the NOAA data
#'
#' @description
#' Process the data to obtain the followning features:
#'
#' * DATE: date of the earthquake
#'
#' * LONGITUDE, LATITUDE: gps coordinates of the earthquake
#'
#' * LOCATION_NAME: Location of the earthquake
#'
#' * COUNTRY: Country where the earthquke occured
#'
#' * DEATHS: Number of fatalities due to the earthquakes and related events
#'
#' * MAG: Richter (equivalent) of the maginitude of the earthquake
#'
#' @details
#' Features YEAR, MONTH and DAY are selected to define the feature DATE
#' buld through function build_date.
#'
#' Features LONGITUDE, LATITUDE are simply converted into numeric
#'
#' Feature COUNTRY is left unchanged by the LOCATION_NAME is processed through package function clean_location.
#'
#' Select features: LONGITUDE, LATITUDE, COUNTRY, LOCATION_NAME to define location.
#' LOCATION_NAME
#'
#' Selected features TOTAL_DEATHS and EQ_RPIMARY are left unchanged but renames DEATHS and MAG
#'
#'
#' @param raw_data dbl_df dataframe input data frame
#'
#' @usage eq_clean_data(raw_data)
#'
#' @importFrom dplyr select mutate
#' @importFrom magrittr %>%
#'
#' @return dbl_df dataframe
#'
#' @seealso build_date clean_location
#'
#' @examples
#' \dontrun{
#' source_noaa <- system.file("extdata","signif.txt",package="NOAAsignifEarthQuakes",mustWork=TRUE)
#' noaa_raw <- load_NOAA_db(source_noaa)
#' noaa_raw %>% eq_clean_data()
#' }
#' @export
eq_clean_data <- function(raw_data){

    # save columns selection into variables
    col_dt <- c("YEAR","MONTH","DAY")
    col_gps <- c("LONGITUDE","LATITUDE")
    col_loc <- c("COUNTRY","LOCATION_NAME")
    col_death <- "TOTAL_DEATHS"
    col_mag <- "EQ_PRIMARY"
    col_sel <- c(col_dt,col_gps,col_loc,col_death,col_mag)
    # set retruned features
    col_out <- c("DATE","COUNTRY","LOCATION_NAME","LONGITUDE","LATITUDE","DEATHS","MAG")
    # declare feature as variable to avoid error message
    TOTAL_DEATHS <- NULL
    EQ_PRIMARY <- NULL
    . <- NULL
    # use dplyr to process the data
    raw_data %>%
        dplyr::select(col_sel) %>%
        dplyr::mutate(
            DATE = eq_build_date(.),
            LOCATION_NAME = eq_build_location(.),
            DEATHS = TOTAL_DEATHS,
            MAG=EQ_PRIMARY
            ) %>%
        dplyr::select(col_out)
}
