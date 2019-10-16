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
                dplyr::filter(COUNTRY %in% countries) %>% as.data.frame() %>%
                dplyr::group_by(COUNTRY) %>%
                dplyr::mutate('MAG_RANK' = dplyr::row_number(dplyr::desc(MAG))) %>%
                dplyr::ungroup()
    } else {
        data <- data %>% dplyr:: mutate('MAG_RANK' = dplyr::row_number(dplyr::desc(MAG)))
    }
    data
}


#' Build a grob describing a timeline of earthquake events
#'
#' @description
#' Each eathquakes is represented by a circle grob along a time axis.
#' Optional feature enable to add a vertical axis usualy country aesthetic \code{y}.
#' Additional optional aesthetics \code{fill} an \code{size} enable the user to represent
#' additional continuous variables such as nb of deaths or magnitude.
#'
#' @param data tbl_df input untransformed dataframe
#' @param panel_scales panel parameters
#' @param coord Coord object required to transform data coordinates to figure coordinates
#' @param alpha alpha parameter for transparency
#'
#' @usage timeline_draw(data, panel_scales,coord,alpha=NULL)
#'
#' @importFrom dplyr filter group_by mutate ungroup
#' @importFrom grid pointsGrob gpar segmentsGrob gTree unit
#' @importFrom scales alpha
#'
#' @return gTree instance
#'
#' @seealso timeline_data geom_timeline
#'
#' @examples
#' \dontrun{
#' ggplot2:: ggplot()+
#' ggplot2:: layer(
#'     geom = GeomTimeLine, mapping = ggplot2:: aes(x=DATE),
#'     data = data, stat = "identity", position = "identity",
#'     show.legend = TRUE, inherit.aes = TRUE,
#'     params = list(na.rm = TRUE)
#'   )
#' }
timeline_draw <- function(data, panel_scales,coord,alpha=NULL) {

    # declare feature as variable to avoid error message
    y <- NULL
    size <- NULL

    # transform coordinates
    coords <- data  %>% coord$transform(panel_scales)
    print(summary(coords$size))
    # draw circles for each earthquake
    earthquakes <- grid::pointsGrob(
        x = coords$x,
        y = coords$y,
        default.units = "native",
        size= grid::unit(coords$size,'char'),
        pch= 20,
        gp= grid::gpar(col = scales::alpha(coords$fill),alpha = alpha,fontsize=5)
    )
    # draw the timeline line
    timeline <- grid::segmentsGrob(
        y0=coords$y,
        y1=coords$y,
        gp= grid::gpar(col='gray',lwd=1.5)
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
#' @param alpha alpha parameter for transparency
#'
#' @usage geom_timeline(data = NULL,mapping=NULL,xmin=NULL,xmax=NULL, countries= NULL,alpha=0.5)
#'
#' @importFrom ggplot2 ggproto aes
#' @importFrom utils modifyList
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
#' @seealso timeline_data timeline_draw geom_timeline_label scale_y_discrete expand_scale
#'
#' @export
geom_timeline <- function(data = NULL,mapping=NULL,xmin=NULL,xmax=NULL, countries= NULL,alpha=0.5) {

    # declare feature as variable to avoid error message
    y <- NULL
    DATE <- NULL
    mrank <- NULL
    LOCATION_NAME <- NULL
    COUNTRY <- NULL
    DEATHS <- NULL
    MAG <- NULL
    MAG_RANK <- NULL

    # apply timeline_data for date and country selection
    data <- data %>% timeline_data(dmin=xmin,dmax=xmax,countries=countries)

    # define minimal aesthetics
    min_mapping <- ggplot2:: aes(x=DATE,rank=MAG_RANK,label=LOCATION_NAME)

    # set default mapping to enforce x to be the DATE
    if (is.null(mapping)) {
        mapping <- min_mapping
    } else {
        mapping <- utils::modifyList(min_mapping,mapping)
    }

    # define GeomTimeLine instance
    GeomTimeLine <- ggplot2:: ggproto(
        "GeomTimeLine", ggplot2::Geom,
        required_aes = c('x'),
        default_aes = ggplot2::aes(y=0.2,size=5,fill=1),
        draw_key = ggplot2::draw_key_pointrange,
        draw_panel = timeline_draw
    )

    # create the ggplot
    ggplot2:: ggplot(data=data,mapping = mapping)+
    # modify theme
    ggplot2::theme_minimal() +
    ggplot2::theme(
        axis.title.y = ggplot2::element_blank(),
        axis.line.x = ggplot2::element_line(colour='black',size=0.5),
        panel.grid = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_line(colour='black',size=0.5),ggplot2::element_line(colour=NULL,size=NULL),
        panel.grid.major.y  = ggplot2::element_line(colour = "gray", size = 1),
        legend.position = "bottom"
    ) +
    ggplot2::scale_y_discrete(expand = ggplot2::expand_scale(mult = c(0.2, 0.8))) +
    ggplot2::scale_radius(range=c(0.5,5)) +
    # apply geom layer
    ggplot2:: layer(
        geom = GeomTimeLine,
        stat = "identity", position = "identity",
        show.legend = TRUE, inherit.aes = TRUE,
        params = list(na.rm = TRUE,alpha=alpha),
        key_glyph =ggplot2::draw_key_point
    )

}


#' timeline_label
#'
#' @param data input data transformed for vizualisation
#' @param panel_scales panel parameters
#' @param coord Coord object required to transform data coordinates to figure coordinates
#' @param n_max parameter number of earthquake location to display by country
#'
#' @usage timeline_label(data, panel_scales,coord,n_max=NULL)
#'
#' @importFrom dplyr filter
#' @importFrom grid segmentsGrob textGrob gTree
#'
#' @return gTree instance
#' @seealso geom_timeline_label
timeline_label <- function(data, panel_scales,coord,n_max=NULL) {

    # filter data to keep only row with rank bellow n_max
    data <-  data %>% dplyr::filter(rank <= n_max) %>% dplyr::mutate(group=dplyr::row_number())
    # transfrom the data into the coordnate system
    coords <- data  %>% coord$transform(panel_scales)
    # buid vertical ticks mark using segment gorb
    timeline_ticks <- grid::segmentsGrob(
        x0=coords$x,
        x1=coords$x,
        y0=coords$y,
        y1=coords$y+0.18,
        gp= grid::gpar(alpha=0.7)
    )
    timeline_text <- grid::textGrob(
        coords$label,
        x=coords$x,
        y=coords$y+0.2,
        hjust=0,
        vjust=0.5,
        rot=45
    )
    # return the gTree instance
    grid::gTree(children=grid::gList(
        timeline_ticks,
        timeline_text
    ))
}

#' geom_timeline_label
#'
#' @param data input data transformed for vizualisation
#' @param n_max maximun number of earthquake to label by countries default value 5
#'
#' @usage geom_timeline_label(data=NULL, n_max =5)
#'
#' @importFrom ggplot2 ggproto layer
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
geom_timeline_label <- function(data=NULL, n_max =5) {

    # define GeomTimeLine instance
    GeomTimeLineLabel <- ggplot2:: ggproto(
        "GeomTimeLineLabel", ggplot2::Geom,
        required_aes = c('x'),
        default_aes = ggplot2::aes(y=0.2),
        draw_panel = timeline_label
    )

    ggplot2:: layer(
        geom = GeomTimeLineLabel,
        data = data, stat = "identity", position = "identity",
        show.legend = FALSE, inherit.aes = TRUE,
        params = list(na.rm = TRUE,n_max=n_max)
    )
}
