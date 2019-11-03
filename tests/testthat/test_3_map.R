context("eq_create_label: Creating popup labels for maps")
test_that("Labels contains right information",{
    df_labels <- dplyr::tbl_df(
        data.frame(
            LOCATION_NAME = c(NA,NA,"Brest","Brest","Brest"),
            MAG = c(NA,3.1,NA,3.1,3.1),
            DEATHS = c(NA,0,0,NA,0)
        )
    )
    expected <- c(
        "",
        "<b>Magnitude:</b>  3.1 <br/><b>Total deaths:</b> 0 <br/>",
        "<b>Location:</b> Brest <br/><b>Total deaths:</b> 0 <br/>",
        "<b>Location:</b> Brest <br/><b>Magnitude:</b>  3.1 <br/>",
        "<b>Location:</b> Brest <br/><b>Magnitude:</b>  3.1 <br/><b>Total deaths:</b> 0 <br/>"
        )
    results <- eq_create_label(df_labels)
    expect(results[1]==expected[1],failure_message="If all is NA shoull produce empty character")
    expect(results[2]==expected[2],failure_message="Location should be discarded if NA")
    expect(results[3]==expected[3],failure_message="Magnitude should be discarded if NA")
    expect(results[4]==expected[4],failure_message="Total deaths should be discarded if NA")
    expect(results[5]==expected[5],failure_message="If no filed NA all should be present")
})

test_that("Labels imprevious to html injection",{
    df_labels <- dplyr::tbl_df(
        data.frame(
            LOCATION_NAME = c("<button type=\"button\" onclick=\"alert('You have been hacked!')\">Click
Me!</button>",NA,NA),
            MAG = c(NA,"<a href=\"https://donate.wikimedia.org/w/index.php\">Follow Me!<a/>",NA),
            DEATHS =c(NA,NA,"<h2><a href=www.attacker.site>special offer</a></h2>")
        )
    )
    expected <- c(
        "<b>Location:</b> &lt;button type=\"button\" onclick=\"alert('You have been hacked!')\"&gt;Click\nMe!&lt;/button&gt; <br/>",
        "<b>Magnitude:</b>  &lt;a href=\"https://donate.wikimedia.org/w/index.php\"&gt;Follow Me!&lt;a/&gt; <br/>",
        "<b>Total deaths:</b> &lt;h2&gt;&lt;a href=www.attacker.site&gt;special offer&lt;/a&gt;&lt;/h2&gt; <br/>"
        )
    results <- eq_create_label(df_labels)
    expect(results[1]==expected[1],failure_message="Location field is not properly escaped")
    expect(results[2]==expected[2],failure_message="Magntude field is not properly escaped")
    expect(results[3]==expected[3],failure_message="Total deaths field is not properly escaped")
})

context("eq_map: Create Leaflet map of Earthquakes location")
file_noaa <- system.file("extdata","signif.txt",package="NOAAsignifEarthQuakes",mustWork=TRUE)
filt_noaa <- load_NOAA_db(file_noaa) %>%
    eq_clean_data() %>%
    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000)
nevents <- length(filt_noaa)
test_that("eq_map produce a map with annot_col Date",{
    expect_failure(expect_warning(
        m1 <- eq_map(filt_noaa,annot_col = "DATE")
    ), failure_message ="eq_map produce warning with annot_col DATE")
    expect_is(m1,"leaflet")
    expect_is(m1,"htmlwidget")
    expect(length(m1$x$calls)==2,failure_message="eq_map should produce 2 layers")
    expect(m1$x$calls[[1]]$method == "addTiles",failure_message="eq_map should contains Tiles")
    expect(m1$x$calls[[2]]$method == "addCircleMarkers",failure_message="eq_map should contains CircleMarkers")
    expect(m1$x$calls[[1]]$method == "addTiles",failure_message="eq_map should contains Tiles")
    expect(m1$x$calls[[2]]$method == "addCircleMarkers",failure_message="eq_map should contains CircleMarkers")
    expect(
        all(c(
            m1$x$alls[[2]]$args[[1]]==nevents,
            m1$x$alls[[2]]$args[[2]]==nevents,
            m1$x$alls[[2]]$args[[3]]==nevents,
            m1$x$alls[[2]]$args[[9]]==nevents
        ))
        ,failure_message="eq_map should contains the righ number of markers")
    expect(all(
        stringr::str_detect(
            m1$x$alls[[2]]$args[[9]],
            "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")
    ),failure_message="labels should be dates")
})

test_that("eq_map produce a map with annot_col text",{
    expect_failure(expect_warning(
        m2 <- eq_map(filt_noaa,annot_col = "popup_text")
    ), failure_message ="eq_map produce warning with annot_col popup_text")
    expect_is(m2,"leaflet")
    expect_is(m2,"htmlwidget")
    expect(length(m2$x$calls)==2,failure_message="eq_map should produce 2 layers")
    expect(m2$x$calls[[1]]$method == "addTiles",failure_message="eq_map should contains Tiles")
    expect(m2$x$calls[[2]]$method == "addCircleMarkers",failure_message="eq_map should contains CircleMarkers")
    expect(
        all(c(
            m2$x$alls[[2]]$args[[1]]==nevents,
            m2$x$alls[[2]]$args[[2]]==nevents,
            m2$x$alls[[2]]$args[[3]]==nevents,
            m2$x$alls[[2]]$args[[9]]==nevents
        ))
        ,failure_message="eq_map should contains the righ number of markers")
    expect(all(
        stringr::str_detect(
            m2$x$alls[[2]]$args[[9]],
            "^(<b>Location:</b>[^<>]*</br>)?(<b>Magnitude:</b>[^<>]*</br>)?(<b>Total deaths:</b>[^<>]*</br>)?$")
    ),failure_message="labels should be popup test")
})

