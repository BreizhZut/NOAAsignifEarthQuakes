## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning= FALSE,
  message=FALSE
)

## ----dependencies, echo=FALSE--------------------------------------------
library(NOAAsignifEarthQuakes)
# additionally for piping
library(magrittr)

## ----ref.label='dependencies'--------------------------------------------
library(NOAAsignifEarthQuakes)
# additionally for piping
library(magrittr)

## ----functions, echo=FALSE-----------------------------------------------
desc_func <- dplyr::tbl_df(
  data.frame(
    Function = ls(getNamespace("NOAAsignifEarthQuakes"))
  )
) %>% 
  dplyr::mutate(
    Processing = Function %in% c("load_NOAA_db","eq_build_date","eq_build_location","eq_clean_data"),
    Timeline= Function %in% c("eq_legend_timeline","geom_timeline","geom_timeline_label","timeline_data"),
    Map=Function %in% c("eq_create_label","eq_map")
  ) %>% 
  dplyr::arrange(desc(Processing),desc(Timeline),desc(Map)) %>% 
  dplyr::mutate(
    Processing = ifelse(Processing,"&#10004;",""),
    Timeline= ifelse(Timeline,"&#10004;",""),
    Map=ifelse(Map,"&#10004;","")
  ) 
knitr::kable(
  desc_func,
  align=c("l","c","c","c"),
  col.names=c("Function","Data Processing","Timeline visualization","Map visualization")
)

## ----read_data, echo=FALSE-----------------------------------------------
file_noaa <- system.file("extdata","signif.txt",package="NOAAsignifEarthQuakes",mustWork=TRUE)
noaa_raw <- load_NOAA_db(file_noaa)

## ----row_cols, echo=FALSE------------------------------------------------
noaa_name <- colnames(noaa_raw)

## ----clean_data, echo=FALSE----------------------------------------------
noaa_clean <- eq_clean_data(noaa_raw)

## ----clean_cols, echo=FALSE----------------------------------------------
clean_cols <- names(noaa_clean)

## ----date_head-----------------------------------------------------------
knitr::kable(
head(noaa_raw,5) %>% 
  dplyr::select("DAY","MONTH","YEAR") %>%
  dplyr::mutate(clean_date=eq_build_date(.))
)

## ----date_tail-----------------------------------------------------------
knitr::kable(
tail(noaa_raw,5) %>% 
  dplyr::select("DAY","MONTH","YEAR") %>%
  dplyr::mutate(clean_date=eq_build_date(.))
)

## ------------------------------------------------------------------------
knitr::kable(
head(noaa_raw,10) %>% 
  dplyr::select("LOCATION_NAME") %>%
  dplyr::mutate(clean_location=eq_build_location(.))
)

## ----ref.label='clean_data'----------------------------------------------
noaa_clean <- eq_clean_data(noaa_raw)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(tail(noaa_clean,10))

## ----timeline_data-------------------------------------------------------
filt_noaa <- noaa_clean %>%
  timeline_data(dmin='2010-01-01',dmax='2011-01-01',countries=c("USA","China"))

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(filt_noaa)

## ----basic_timeline, results='asis',fig.width=7,fig.height=3-------------
g_usa <- geom_timeline(noaa_clean,
                       countries='USA',
                       xmin='2000-01-01',
                       xmax='2017-01-01')

## ----complex_timeline, results='asis',fig.width=7,fig.height=4-----------
g_usachina <- geom_timeline(noaa_clean,
              mapping = ggplot2::aes(
                y=COUNTRY,
                fill=DEATHS,
                size= MAG
              ),
              countries=c('USA',"China"),
              xmin='2000-01-01',
              xmax='2017-01-01')
g_usachina

## ----eq_legend_timeline--------------------------------------------------
# define fake aesthetics
feat <- ggplot2::aes(aes_1=DATE,aes_2=MAG,aes_3=DEATHS,aes_4=COUNTRY,aes_5=LOCATION_NAME)
# test eq_legend_timeline in a table
legend_aes <- dplyr::tbl_df(data.frame(
  aesthetic=c('aes_1','aes_2','aes_3','aes_4','aes_5')
)) %>% dplyr::rowwise() %>%
  dplyr::mutate(
    feature= rlang::quo_name(feat[[aesthetic]]),
    label= eq_legend_timeline(feat[[aesthetic]]))
knitr::kable(legend_aes)

## ----labeled_usa_timeline, results='asis',fig.width=7,fig.height=4-------
g_usa + geom_timeline_label(n_max=10)

## ----labeled_usachina_timeline, results='asis',fig.width=7,fig.height=6----
g_usachina + geom_timeline_label()

## ----filt_noaa-----------------------------------------------------------
filt_noaa <- noaa_clean %>% dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000)

## ----map_date_label, fig.width=7, fig.height=4---------------------------
eq_map(filt_noaa,annot_col = "DATE")

## ----annot_noaa----------------------------------------------------------
annot_noaa <- filt_noaa %>% head(10) %>%
  dplyr::mutate(annot_text=eq_create_label(.)) %>%
  dplyr::select("DATE","LOCATION_NAME","DEATHS","MAG","annot_text")
knitr::kable(annot_noaa)

## ----map_popup_label, fig.width=7, fig.height=4--------------------------
eq_map(filt_noaa,annot_col = "popup_text")

