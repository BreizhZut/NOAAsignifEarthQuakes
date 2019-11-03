context("load_NOAA_db: Reading input")
file_noaa <- system.file("extdata","signif.txt",package="NOAAsignifEarthQuakes",mustWork=TRUE)
test_that("Source file exists",{
    expect(file.exists(file_noaa),failure_message = paste("File missing for test",file_noaa))
})

test_that("Data can be red",{
    expect_failure(
        expect_warning(all_data <- load_NOAA_db(file_noaa)),
        failure_message="load_NOAA_db should not produce warnings")
    expect(any(class(all_data) == "tbl_df"),failure_message = "Result should be a tbl_df data.frame")
    expect(nrow(all_data)>0,failure_message = "Table should not be empty")
    input_cols <- c("YEAR","MONTH","DAY","LOCATION_NAME","LONGITUDE","LATITUDE")
    expect(all(input_cols %in% names(all_data)),failure_message = "Missing input columns")
})

all_data <- load_NOAA_db(file_noaa)
col_dt <- c("YEAR","MONTH","DAY")
col_gps <- c("LONGITUDE","LATITUDE")
col_loc <- c("COUNTRY","LOCATION_NAME")
col_death <- "TOTAL_DEATHS"
col_mag <- "EQ_PRIMARY"
test_that("Feature are not missing",{
    expect(all(col_dt %in% names(all_data)),failure_message = "Date features are missing")
    expect(all(col_gps %in% names(all_data)),failure_message = "GPS features are missing")
    expect(all(col_loc %in% names(all_data)),failure_message = "Location features are missing")
    expect(col_death %in% names(all_data),failure_message = "Total deaths feature is missing")
    expect(col_mag %in% names(all_data),failure_message = "Richeter scale magnitude feature is missing")
})

test_that("Feature have the right type",{
    expect_is(all_data$DAY,'integer')
    expect_is(all_data$MONTH,'integer')
    expect_is(all_data$YEAR,'integer')
    expect_is(all_data$LONGITUDE,'numeric')
    expect_is(all_data$LATITUDE,'numeric')
    expect_is(all_data$COUNTRY,'character')
    expect_is(all_data$LOCATION_NAME,'character')
    expect_is(all_data$TOTAL_DEATHS,'integer')
    expect_is(all_data$EQ_PRIMARY,'numeric')
})

context("eq_build_date: Creating date feature")
test_that("eq_build_date produce expected results",{
    date_data <- dplyr::tbl_df(
        data.frame(
            DAY=c(NA,12),
            MONTH=c(NA,05),
            YEAR=c(-1023,2013))
        )
    date_output <- eq_build_date(date_data)
    expect(class(date_output)=="Date",failure_message="Function eq_build_date has not produced dates")
    expect(length(date_output)==2,failure_message="Function eq_build_date has not produced the right number of dates")
    expect(format(date_output[2]) == "2013-05-12",
           failure_message="Function eq_build_date has not produced the expected date with non problematic DAT MONTH YEAR")
    expect(lubridate::year(date_output[1]) == -1023,
           failure_message="Function eq_build_date could not resolve year BC")
    expect(lubridate::month(date_output[1]) == 1,
           failure_message="Function eq_build_date did not set NA month to January")
    expect(lubridate::day(date_output[1]) == 1,
           failure_message="Function eq_build_date did not set NA day to the 1st")
})

context("eq_build_location: Cleaning location feature")
test_that("eq_build_date produce expected results",{
    location_data <- dplyr::tbl_df(
        data.frame(LOCATION_NAME = c(
            NA,
            "BREST",
            "PORT-AUX-PRINCES",
            "FRANCE:BREST(EUROPE)[FRANCE]:EARTH;VIA LACTEA",
            "FRANCE: BREST,BRETAGNE (CUB)",
            "FRANCE:   BREST ,  BRETAGNE (BZH)  "
        )
        )
    )
    expected_location <-c(
        NA,"Brest","Port-Aux-Princes","Brest",
        "Brest, Bretagne","Brest, Bretagne"
    )
    cleaned_location <- eq_build_location(location_data)
    expect(class(cleaned_location)=="character",failure_message="Function eq_build_location has not produced character")
    expect(length(cleaned_location)==6,failure_message="Function eq_build_location has not produced the right number of characters")
    expect(is.na(cleaned_location[1]),failure_message="Function eq_build_location did not produce NA")
    expect(cleaned_location[2]==expected_location[2],
           failure_message="Function eq_build_location has not switched to title case")
    expect(cleaned_location[3]==expected_location[3],
           failure_message="Function eq_build_location has not switched to title case with -")
    expect(cleaned_location[4]==expected_location[4],
           failure_message="Function eq_build_location has not removed countries and ()")
    expect(cleaned_location[5]==expected_location[5],
           failure_message="Function eq_build_location has not inserted space after commar or trimmed")
    expect(cleaned_location[6]==expected_location[6],
           failure_message="Function eq_build_location has not removed extra spaces")
})

context("eq_clean_data: Cleaning input raw input data")
output_cols <- c("DATE","LOCATION_NAME","LONGITUDE","LATITUDE","DEATHS","MAG")
test_that("Head can be cleaned with the right schema",{
    ntest <- 20
    expect_failure(
        expect_warning(
            clean_head <- all_data %>% head(ntest) %>%eq_clean_data()
        ),failure_message = "eq_clean_data should not produce warnings")
    expect_is(clean_head, "tbl_df")
    expect(all(output_cols %in% names(clean_head)),failure_message = "Missing output columns")
    expect(nrow(clean_head)==ntest,failure_message = "All rows should be present")
    expect_is(clean_head$DATE,"Date")
    expect_is(clean_head$LONGITUDE,"numeric")
    expect_is(clean_head$LATITUDE,"numeric")
})

test_that("Tail can be cleaned with the right schema",{
    ntest <- 20
    expect_failure(
        expect_warning(
            clean_tail <- all_data %>% tail(ntest) %>% eq_clean_data()
        ),failure_message = "eq_clean_data should not produce warnings")
    expect_is(clean_tail, "tbl_df")
    expect(all(output_cols %in% names(clean_tail)),failure_message = "Missing output columns")
    expect(nrow(clean_tail)==ntest,failure_message = "All rows should be present")
    expect_is(clean_tail$DATE,"Date")
    expect_is(clean_tail$LONGITUDE,"numeric")
    expect_is(clean_tail$LATITUDE,"numeric")
})
