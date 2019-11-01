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

test_that("Filtered data has the right schema",{
    filt_data <- noaa %>% timeline_data(dmin="2010-01-01",dmax="2015-01-01",countries=c('USA','China'))
    expect(all(timeline_cols %in% names(filt_data)),failure_message='Missing output columns')
    expect(class(filt_data$DATE)=='Date',failure_message='Feature DATE should be a Date')
    expect(class(filt_data$COUNTRY)=='character',failure_message='Feature COUNTRY should be a Character')
    expect(class(filt_data$LOCATION_NAME)=='character',failure_message='Feature LOCATION_NAME should be a Character')
    expect(class(filt_data$DEATHS)=='integer',failure_message='Feature DEATHS should be a integer')
    expect(class(filt_data$MAG)=='numeric',failure_message='Feature MAG should be a numeric')
    expect(class(filt_data$MAG_RANK)=='integer',failure_message='Feature MAG_RANK should be a integer')
})

context("eq_legend_timeline: Building legends for geom_timeline")
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

context("geom_timeline")
g <- geom_timeline(noaa)
test_that("geom_timeline expects xmin, xmax,countries,and mapping arguments",{
    expect_failure(expect_warning(
        geom_timeline(noaa,
                      xmin="2017-01-01",
                      xmax='2018-01-01',
                      countries=c('pim','pam','poum'),
                      mapping=ggplot2::aes(y=COUNTRY),
                      alpha=0.1)
    ),failure_message=
        'geom_timeline should expect xmin, xmax, countries,mapping and alpha as argument')
})
test_that("geom_timeline produce a geom with expected aestetics",{
    expect("ggplot" %in% class(g),failure_message="geom_timeline fills to produce a ggplot object")
    expect(! rlang::quo_is_null(g$mapping$x),failure_message="geom_timeline should contians x aesthetic")
    expect(rlang::quo_get_expr(g$mapping$x)=='DATE',failure_message="x aesthetics sould be DATE")
    expect(is.null(g$mapping$y),failure_message="y should not be an aesthetic by default")
    expect(is.null(g$mapping$fill),failure_message="fill should not be an aesthetic by default")
    expect(is.null(g$mapping$size),failure_message="size should not be an aesthetic by default")
})
test_that("geom_timeline produce a GeomTimeline",{
    expect(length(g$layers)==1,failure_message = "geom_timeline should contain 1 layer")
    expect(! is.null(g$layers[[1]]$geom),failure_message="layer should contain a geom")
    ggeom <- g$layers[[1]]$geom
    expect(all(c("ggproto","GeomTimeLine","Geom") %in% class(ggeom)),failure_message="geom should be a ggproto object of class GeomTimeLine inherited of class Geom")
    expect(ggeom$required_aes == "x",failure_message = "x should be a required aesthetic")
})
test_that("geom_timeline produce the expected figure",{
    g <- geom_timeline(noaa,
                       mapping=aes(fill=DEATHS,size=MAG),
                       countries='USA',
                       xmin='2000-01-01',
                       xmax='2017-01-01')
    vdiffr::expect_doppelganger("timeline_usa", g)
    g <- geom_timeline(noaa,
                       mapping=aes(fill=DEATHS,size=MAG,y=COUNTRY),
                       countries=c('USA','CHINA'),
                       xmin='2000-01-01',
                       xmax='2017-01-01')
    vdiffr::expect_doppelganger("timeline_usa_china", g)
})

context("geom_timeline_label")
g <- geom_timeline(noaa,
                   mapping=aes(fill=DEATHS,size=MAG,y=COUNTRY),
                   countries=c('USA','CHINA'),
                   xmin='2000-01-01',
                   xmax='2017-01-01')
test_that("geom_timeline_label produce the expected figure",{
    expect_failure(expect_warning(
        g + geom_timeline_label(n_max=2)),
        failure_massage = "geom_timeline_label should expect n_max as argument")
    g <- g + geom_timeline_label()
    expect(length(g$layers)==2,failure_message = "ggplot should contain 2 layers")
    ggeom <- g$layers[[2]]$geom
    expect(all(c("ggproto","GeomTimeLineLabel","Geom") %in% class(ggeom)),failure_message="geom should be a ggproto object of class GeomTimeLineLabel inherited of class Geom")
    expect(all(c('x','label') %in% ggeom$required_aes),failure_message = "x and label should be a required aesthetic")
    vdiffr::expect_doppelganger("timeline_usa_china_label", g)
})
