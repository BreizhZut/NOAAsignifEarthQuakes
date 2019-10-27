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
        "<b>Magnitude:</b>  3.1 </br><b>Total deaths:</b> 0 </br>",
        "<b>Location:</b> Brest </br><b>Total deaths:</b> 0 </br>",
        "<b>Location:</b> Brest </br><b>Magnitude:</b>  3.1 </br>",
        "<b>Location:</b> Brest </br><b>Magnitude:</b>  3.1 </br><b>Total deaths:</b> 0 </br>"
        )
    results <- eq_create_label(df_labels)
    expect(results[1]==expected[1],failure_message="If all is NA shoull produce empty character")
    expect(results[2]==expected[2],failure_message="Location should be discarde if NA")
    expect(results[3]==expected[3],failure_message="Magnitude should be discarde if NA")
    expect(results[4]==expected[4],failure_message="Total deaths should be discarde if NA")
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
        "<b>Location:</b> &lt;button type=\"button\" onclick=\"alert('You have been hacked!')\"&gt;Click\nMe!&lt;/button&gt; </br>",
        "<b>Magnitude:</b>  &lt;a href=\"https://donate.wikimedia.org/w/index.php\"&gt;Follow Me!&lt;a/&gt; </br>",
        "<b>Total deaths:</b> &lt;h2&gt;&lt;a href=www.attacker.site&gt;special offer&lt;/a&gt;&lt;/h2&gt; </br>"
        )
    results <- eq_create_label(df_labels)
    expect(results[1]==expected[1],failure_message="Location field is not properly escaped")
    expect(results[2]==expected[2],failure_message="Magntude field is not properly escaped")
    expect(results[3]==expected[3],failure_message="Total deaths field is not properly escaped")
})
