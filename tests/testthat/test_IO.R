context("Reading input with load_NOAA_db")
file_noaa <- system.file("extdata","signif.txt",package="NOAAsignifEarthQuakes",mustWork=TRUE)
test_that("Source file exists",{
    expect(file.exists(file_noaa),failure_message = paste("File missing for test",file_noaa))
})

all_data <- load_NOAA_db(file_noaa)
test_that("Data can be red",{
    expect(nrow(all_data)>0,failure_message = "Table should not be empty")
    input_cols <- c("YEAR","MONTH","DAY","LOCATION_NAME","LONGITUDE","LATITUDE")
    expect(all(input_cols %in% names(all_data)),failure_message = "Missing input columns")
})

context("Helper function buid_date and singleCap")
test_that("build_date produce expected result",{
    expect(is.na(build_date(NA,"01","01")),failure_message ="With missing year result shoud be NA")
    expect(build_date(2018,NA,NA)==lubridate::ymd("2018-01-01"),failure_message ="With missing month and day sould be set 01")
    expect(build_date(2018,NA,15)==lubridate::ymd("2018-01-15"),failure_message ="With missing month should be 01 and day returned as is")
    expect(build_date(2018,04,NA)==lubridate::ymd("2018-04-01"),failure_message ="With missing day should be 01 and month returned as is")
    expect(build_date(2018,04,15)==lubridate::ymd("2018-04-15"),failure_message ="Should return correct date when no missing value")
})

context("Cleaning input with eq_clean_data")
output_cols <- c("DATE","LOCATION_NAME","LONGITUDE","LATITUDE","DEATHS","MAG")
#file_noaa <- system.file("extdata","signif.txt",package="NOAAsignifEarthQuakes",mustWork=TRUE)
#all_data <- load_NOAA_db(file_noaa)
test_that("Head can be cleaned",{
    ntest <- 20
    clean_head <- all_data %>% head(ntest) %>%
        eq_clean_data()
    expect(all(output_cols %in% names(clean_head)),failure_message = "Missing output columns")
    expect(nrow(clean_head)==ntest,failure_message = "All rows should be present")
    expect(class(clean_head$DATE)=="Date",failure_message = "DATE feature should be date")
    expect(is.numeric(clean_head$LONGITUDE),failure_message = "LONGITUDE should be numeric")
    expect(is.numeric(clean_head$LATITUDE),failure_message = "LATITUDE should be numeric")
})

test_that("Tail can be cleaned",{
    ntest <- 20
    clean_tail <- all_data %>% tail(ntest) %>%
        eq_clean_data()
    expect(all(output_cols %in% names(clean_tail)),failure_message = "Missing output columns")
    expect(nrow(clean_tail)==ntest,failure_message = "All rows should be present")
    expect(class(clean_tail$DATE)=="Date",failure_message = "DATE feature should be date")
    expect(is.numeric(clean_tail$LONGITUDE),failure_message = "LONGITUDE should be numeric")
    expect(is.numeric(clean_tail$LATITUDE),failure_message = "LATITUDE should be numeric")
})
