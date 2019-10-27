context("timeline_data: Filtering data for geom_timeline")
file_noaa <- system.file("extdata","signif.txt",package="NOAAsignifEarthQuakes",mustWork=TRUE)
noaa <- load_NOAA_db(file_noaa) %>% eq_clean_data()
timeline_cols <- c('DATE','COUNTRY','LOCATION_NAME','DEATHS','MAG','MAG_RANK')
test_that("Data can filtered",{
    filt_data <- noaa %>% timeline_data()
    expect(any(class(filt_data)=="tbl_df"),failure_message='Result of timeline_data should be a tbl_df data.frame')
    expect(nrow(filt_data)==nrow(noaa),failure_message='By default timeline_data should not filter results')
    filt_data <- noaa %>% timeline_data(countries='Neverland')
    expect(nrow(filt_data)==0,failure_message='timeline_data should produce no result for non existing country')
    filt_data <- noaa %>% timeline_data(dmin="2010-01-01",dmax="2015-01-01")
    expect(min(filt_data$DATE)>=lubridate::ymd("2010-01-01"),failure_message='Dates are not filtered correctly through dmin')
    expect(max(filt_data$DATE)<=lubridate::ymd("2015-01-01"),failure_message='Dates are not filtered correctly through dmax')
    filt_data <- noaa %>% timeline_data(dmin="2010-01-01",dmax="2015-01-01",countries=c('USA','China'))
    expect(all(filt_data$COUNTRY %in% c('USA','CHINA')),failure_message='Countries are not filtered correctly')
})
filt_data <- noaa %>% timeline_data(dmin="2010-01-01",dmax="2015-01-01",countries=c('USA','China'))
test_that("Filtered data has the right schema",{
    expect(all(timeline_cols %in% names(filt_data)),failure_message='Missing output columns')
    expect(class(filt_data$DATE)=='Date',failure_message='Feature DATE should be a Date')
    expect(class(filt_data$COUNTRY)=='character',failure_message='Feature COUNTRY should be a Character')
    expect(class(filt_data$LOCATION_NAME)=='character',failure_message='Feature LOCATION_NAME should be a Character')
    expect(class(filt_data$DEATHS)=='integer',failure_message='Feature DEATHS should be a integer')
    expect(class(filt_data$MAG)=='numeric',failure_message='Feature MAG should be a numeric')
    expect(class(filt_data$MAG_RANK)=='integer',failure_message='Feature MAG_RANK should be a integer')
})

context("eq_legend_timeline: building legends for geom_timeline")
# if labels are changed in geom_timeline they should also be changed here
test_that("eq_legend_timeline give the right results",{
    mapping <- ggplot2::aes(y=COUNTRY,mag=MAG,deaths=DEATHS,mag2=mag)
    expect(is.null(eq_legend_timeline(mapping$x)),
           failure_message="If mapping does ot exist should return null")
    expect(eq_legend_timeline(mapping$y)=="Country",
           failure_message="Should return unknown value as titlecase")
    expect(eq_legend_timeline(mapping$deaths)=="Total nb of deaths",
           failure_message="Return wrong value for expr DEATHS")
    expect(eq_legend_timeline(mapping$mag)=="Richter scale magnitude",
           failure_message="Return wrong value for expr MAG")
    expect(eq_legend_timeline(mapping$mag)=="Richter scale magnitude",
           failure_message="Return wrong value for expr mag (case sensitive)")
})

