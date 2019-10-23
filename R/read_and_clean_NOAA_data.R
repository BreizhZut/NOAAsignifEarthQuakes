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
#' @seealso eq_clean_data
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
#' @seealso eq_clean_data
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
    x <- gsub("(^[A-Z :]*:[ ]*|\\(.*\\)||\\[.*\\]|[:;].*$)","",data$LOCATION_NAME)
    # add space after comma
    x <- gsub(" *,",", ",x)
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
#' Features YEAR, MONTH and DAY are selected to define the feature DATE through function eq_build_date.
#' Feature LOCATION_NAME is processed through package function eq_build_location.
#'
#' Select features: LONGITUDE, LATITUDE, COUNTRY, LOCATION_NAME to define location.
#' LOCATION_NAME
#'
#' Selected features TOTAL_DEATHS and EQ_RPIMARY are left unchanged but renamed DEATHS and MAG
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
#' @seealso eq_build_date eq_build_location
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
