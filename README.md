<!-- README.md is generated from README.Rmd. Please edit that file -->

# NOAAsignifEarthQuakes

The goal of NOAAsignifEarthQuakes is to:

1.  Read and clean data from the NOAA Significant Earthquake
    Database\[1\]
2.  Provide exploratory visualization tools:
    1.  Timelines of earthquakes by countries
    2.  Mapping of earthquakes in a specific region

## Example

### Loading the package

``` r
library(NOAAsignifEarthQuakes)
library(magrittr)
library(ggplot2)
```

### Reading and cleaning the data

``` r
## set the file input
file_noaa <- system.file("extdata","signif.txt",
                         package="NOAAsignifEarthQuakes",
                         mustWork=TRUE)
## Read and clean data
noaa <- load_NOAA_db(file_noaa) %>% eq_clean_data()
tail(noaa)
#> # A tibble: 6 x 7
#>   DATE       COUNTRY   LOCATION_NAME        LONGITUDE LATITUDE DEATHS   MAG
#>   <date>     <chr>     <chr>                    <dbl>    <dbl>  <int> <dbl>
#> 1 2018-09-08 CHINA     Mojiang Hani             102.    23.3       NA   5.6
#> 2 2018-09-12 INDIA     West Bengal               90.2   26.4        1   5.3
#> 3 2018-09-28 INDONESIA Sulawesi                 120.    -0.178   2256   7.5
#> 4 2018-10-07 HAITI     Port-Dex-Paix            -73.0   20.0       18   5.9
#> 5 2018-10-10 INDONESIA Madura Island, Java      114.    -7.46       4   6  
#> 6 2018-10-25 GREECE    Zakynthos, Strofades      20.6   37.5       NA   6.8
```

### Display a timeline

``` r
geom_timeline(noaa,mapping=aes(size=MAG,fill=DEATHS),
              countries='USA',
              xmin='2000-01-01',
              xmax='2017-01-01')
```

![](README-timeline_usa-1.png)<!-- -->

### Display a timeline with labels

``` r
g <- geom_timeline(noaa,mapping=aes(size=MAG,fill=DEATHS,y=COUNTRY),
              countries=c('USA','China'),
              xmin='2000-01-01',
              xmax='2017-01-01') 
g + geom_timeline_label()
```

![](README-timeline_usa_china_labels-1.png)<!-- -->

1.  Source: National Geophysical Data Center / World Data Service
    (NGDC/WDS); Significant Earthquake Database; National Geophysical
    Data Center, NOAA. <doi:10.7289/V5TD9V7K>.
