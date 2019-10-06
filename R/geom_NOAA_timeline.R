#'
#' Filter NOAA processed data by date and countries
#'
#' @description
#' Prepare data for visualisation, tking the clean data as input,
#' apply some filters in term of date range and countries in order
#' to limit the amount of earthquakes to display.
#'
#' @usage
#' timeline_data(data=NULL,dmin=NULL,dmax=NULL,countries=NULL)
#'
#' @param dmin start date for data vizualisation [Character] encoded as YYYY-MM-DD or [Date]
#' @param dmax end date for data vizualisation [Character] encoded as YYYY-MM-DD or [Date]
#' @param countries [Character] list of Countries to filter
#' @param data input tbl_df dataframe
#'
#' @importFrom lubridate ymd
#' @importFrom stats quantile
#' @importFrom dplyr filter group_by mutate ungroup desc
#' @return tbl_df dataframe
#'
#' @seealso timeline_draw geom_timeline
#'
#' @examples
#' \dontrun{
#' filtered_data <- timeline_data(clean_noaa,
#'                                dmin='2010-01-01',
#'                                dmax='2018-01-01',
#'                                countries=c('USA','China'))
#'}
timeline_data <- function(data=NULL,dmin=NULL,dmax=NULL,countries=NULL){

    # declare feature as variable to avoid error message
    DATE <- NULL
    COUNTRY <- NULL
    MAG <- NULL
    DEATHS <- NULL
    # if start date set remove data prior start date
    if(!is.null(dmin)){
        data <- data %>% dplyr::filter(DATE>=lubridate::ymd(dmin))
    }
    # if end date set remove data after end date
    if(!is.null(dmax)){
        data <- data %>% dplyr::filter(DATE<=lubridate::ymd(dmax))
    }
    # country set take only data for these countries
    # define rank of earthquakes from the highest to the lowest magnitude
    # for each county seperatly
    if(!is.null(countries)){
        # warning Grouping rowwise data frame strips rowwise nature comes from
        data <- data %>%
                dplyr::filter(COUNTRY %in% countries) %>%
                dplyr::group_by(COUNTRY) %>%
                dplyr::mutate(mrank = dplyr::row_number(dplyr::desc(MAG))) %>%
                dplyr::ungroup()
    } else {
        data <- data %>% dplyr:: mutate(mrank = dplyr::row_number(dplyr::desc(MAG)))
    }
    # cap the number of deuths to the ).75 percentile
    death_sc <- ceiling(stats::quantile(data$DEATHS,p=0.75,na.rm=TRUE))
    sat_death <- function(x){
        x[x>death_sc] <- death_sc
        x
    }
    data <- data %>% dplyr::mutate(DEATHS=sat_death(DEATHS))
    data
}


#' Build a grob describing a timeline of earthquake events
#'
#' @description
#' Each eathquakes is represented by a circle grob along a time axis.
#' Optional feature enable to add a vertical axis usualy country aesthetic \code{y}.
#' Additional optional aesthetics \code{fill} an \code{r} enable the user to represent
#' additional continuous variables such as nb of deaths or magnitude.
#'
#' @param data tbl_df input untransformed dataframe
#' @param panel_scales panel parameters
#' @param coord Coord object required to transform data coordinates to figure coordinates
#'
#' @usage timeline_draw(data, panel_scales,coord)
#'
#' @importFrom dplyr filter group_by mutate ungroup
#' @importFrom grid circleGrob gpar segmentsGrob gTree
#' @importFrom scales rescale alpha
#'
#' @return gTree instance
#'
#' @seealso timeline_data geom_timeline
#'
#' @examples
#' \dontrun{
#' GeomTimeLine <- ggplot2:: ggproto(
#'      "GeomTimeLine", ggplot2::Geom,
#'      required_aes = c('x'),
#'      default_aes = ggplot2::aes(y=0.5,shape=21,size=1,fill=1,r=10),
#'      draw_key = c("r","fill"),
#'      draw_panel = timeline_draw
#'   )
#' ggplot2:: ggplot()+
#' ggplot2:: layer(
#'     geom = GeomTimeLine, mapping = ggplot2:: aes(x=DATE),
#'     data = data, stat = "identity", position = "identity",
#'     show.legend = TRUE, inherit.aes = TRUE,
#'     params = list(na.rm = TRUE)
#'   )
#' }
timeline_draw <- function(data, panel_scales,coord) {

    # declare feature as variable to avoid error message
    y <- NULL
    size <- NULL

    # transform coordinates
    coords <- data  %>%
        coord$transform(panel_scales) %>%
        dplyr::mutate(size=replace(size,is.na(size),0))
    yshift <- min(coords$y)-0.1
    coords <- coords%>% dplyr::mutate(y=y-yshift)
    # str(coords)
    # draw circles for each earthquake
    earthquakes <- grid::circleGrob(
        x = coords$x,
        y = coords$y,
        r = scales::rescale(coords$r,from=c(2,10),to=c(0.01,0.02)),
        # default.units = "native",
        gp= grid::gpar(fill = scales::alpha(data$fill),alpha = 0.5)
    )
    # draw the timeline line
    timeline <- grid::segmentsGrob(
        y0=coords$y,
        y1=coords$y,
        gp= grid::gpar(col='gray')
    )

    # return the gTree instance
    grid::gTree(children=grid::gList(
        timeline,
        earthquakes
    ))
}

#' Displays a timeline of Earthquakes
#'
#' @description
#' For a given (optional) selection of date range and countries.
#' Teh resulting selection of earthquakes is represented by a circle along a time axis.
#' Optional feature enable to add a vertical axis usualy country aesthetic \code{y}.
#' Additional optional aesthetics \code{fill} an \code{r} enable the user to represent
#' additional continuous variables such as nb of deaths or magnitude.
#'
#' @inheritParams ggplot2:: geom_point
#' @param xmin start date for data vizualisation [Character] encoded as YYYY-MM-DD or [Date]
#' @param xmax end date for data vizualisation [Character] encoded as YYYY-MM-DD or [Date]
#' @param countries [Character] list of Countries to filter
#' @param ... inherited parameters
#'
#' @usage geom_timeline(data = NULL, xmin=NULL,xmax=NULL, countries= NULL,...)
#'
#' @importFrom ggplot2 ggproto aes
#' @importFrom lubridate ymd
#' @importFrom dplyr filter mutate
#'
#' @return ggplot plot object
#'
#' @examples
#' \dontrun{
#' ggplot(data=clean_data,aes(x=DATE,y=COUNTRY)) + geom_timeline()
#' }
#'
#' @seealso timeline_data timeline_draw geom_timeline_label
#'
#' @export
geom_timeline <- function(data = NULL, xmin=NULL,xmax=NULL, countries= NULL,...) {

    # declare feature as variable to avoid error message
    y <- NULL
    DATE <- NULL

    # define GeomTimeLine instance
    GeomTimeLine <- ggplot2:: ggproto(
        "GeomTimeLine", ggplot2::Geom,
        required_aes = c('x'),
        default_aes = ggplot2::aes(y=0.5,shape=21,size=1,fill=1,r=10),#label=LOCATION_NAME,mag=MAG),
        draw_key = c("r","fill"),
        draw_panel = timeline_draw
    )

    # set default mapping to enforce x to be the DATE
    mapping <- ggplot2:: aes(x=DATE,...)

    # apply timeline_data for date and country selection
    data <- data %>% timeline_data(dmin=xmin,dmax=xmax,countries=countries)
    # create the ggplot
    ggplot2:: ggplot()+
    # modify theme
    ggplot2::theme_minimal() +
    ggplot2::theme(
        axis.title.y = ggplot2::element_blank(),
        axis.line.x = ggplot2::element_line(colour='black',size=0.5),
        panel.grid = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_line(colour='black',size=0.5),ggplot2::element_line(colour=NULL,size=NULL),
        legend.position = "bottom"
    ) +
    # apply geom layer
    ggplot2:: layer(
        geom = GeomTimeLine, mapping = mapping,
        data = data, stat = "identity", position = "identity",
        show.legend = TRUE, inherit.aes = TRUE,
        params = list(na.rm = TRUE)
    )

}


#' timeline_label
#'
#' @param data input data transformed for vizualisation
#' @param panel_scales panel parameters
#' @param coord Coord object required to transform data coordinates to figure coordinates
#'
#' @usage timeline_label(data, panel_scales,coord)
#'
#' @return gTree instance
#' @seealso geom_timeline_label
timeline_label <- function(data, panel_scales,coord) {

}

#' geom_timeline_label
#'
#' @param data input data transformed for vizualisation
#' @param n_max maximun number of earthquake to label by countries default value 5
#' @param ... inherited parameters
#'
#' @usage geom_timeline_label(data=NULL, n_max =5, ...)
#'
#' @return ggplot2 layer
#' @export
#'
#' @examples
#' \dontrun{
#' g <- ggplot(data=clean_data,aes(x=DATE,y=LOCATION_NAME)) +
#'      geom_timeline() +
#'      geom_timeline_label()
#' }
#' @seealso timeline_label geom_timeline
geom_timeline_label <- function(data=NULL, n_max =5, ...) {

}
