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
#' @importFrom dplyr filter group_by mutate ungroup desc select
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
#' @export
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
                dplyr::filter(COUNTRY %in% toupper(countries)) %>% as.data.frame() %>%
                dplyr::group_by(COUNTRY) %>%
                dplyr::mutate('MAG_RANK' = dplyr::row_number(dplyr::desc(MAG))) %>%
                dplyr::ungroup()
    } else {
        data <- data %>% dplyr:: mutate('MAG_RANK' = dplyr::row_number(dplyr::desc(MAG)))
    }
    data %>% dplyr::select("DATE","COUNTRY","LOCATION_NAME","DEATHS","MAG","MAG_RANK")
}

#' Transform NOAA feature into human readable character
#'
#' @description
#' For DEATHS feature returns "Total nb of deaths",
#' for MAG feature returns "Richter scale magnitude"
#' and for any other return the feature name in title case.
#'
#' @param quo quosure corresponding to mapping of a variable
#'
#' @importFrom rlang quo_is_null quo_name
#' @importFrom stringr str_to_title
#'
#' @return Character
#'
#' @examples
#' \dontrun{
#'  mapping <- ggplot2::aes(x=DATE,fill=MAG,size=DEATHS)
#'  eq_legend_timeline(mapping$x)
#'  eq_legend_timeline(mapping$fill)
#'  "Total nb of deaths"
#'  eq_legend_timeline(mapping$size)
#'  "Richter scale magnitude"
#' }
#' @export
eq_legend_timeline <- function(quo){

    if(is.null(quo)){
        return(NULL)
    } else if(rlang:: quo_is_null(quo)){
        return(NULL)
    } else if(rlang:: quo_name(quo) == "DEATHS"){
        return("Total nb of deaths")
    } else if(rlang:: quo_name(quo) == "MAG"){
        return("Richter scale magnitude")
    } else {
        stringr::str_to_title(rlang::quo_name(quo))
    }
}

#' Displays a timeline of Earthquakes
#'
#' @description
#' For a given (optional) selection of date range and countries.
#' The resulting selection of earthquakes is represented by a circle along a time axis.
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
#' @importFrom dplyr filter mutate group_by ungroup
#'
#' @importFrom dplyr filter  mutate ungroup
#' @importFrom grid pointsGrob gpar segmentsGrob gTree unit
#' @importFrom scales alpha
#'
#' @return ggplot plot object
#'
#' @examples
#' \dontrun{
#' ggplot(data=clean_data,aes(x=DATE,y=COUNTRY)) + geom_timeline()
#' }
#'
#' @seealso timeline_data geom_timeline_label relabel_legend
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

    # This method was inserted within the main function as it could not be used
    # and tested outside a geom
    timeline_draw <- function(data, panel_scales,coord,alpha=NULL) {

        # declare feature as variable to avoid error message
        y <- NULL
        size <- NULL

        # transform coordinates
        coords <- data  %>% coord$transform(panel_scales)
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

    # Define GeomTimeLine instance
    GeomTimeLine <- ggplot2:: ggproto(
        "GeomTimeLine", ggplot2::Geom,
        required_aes = c('x'),
        optional_aes = c('fill','size','y'),
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
    ggplot2::labs(
        fill=eq_legend_timeline(mapping$fill),
        size=eq_legend_timeline(mapping$size),
        x=eq_legend_timeline(mapping$x))+
    # apply geom layer
    ggplot2:: layer(
        geom = GeomTimeLine,
        stat = "identity", position = "identity",
        show.legend = TRUE, inherit.aes = TRUE,
        params = list(na.rm = TRUE,alpha=alpha),
        key_glyph =ggplot2::draw_key_point
    )
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

    # This method was inserted within the main function as it could not be used
    # and tested outside a geom
    timeline_label <- function(data, panel_scales,coord,n_max=NULL) {

        # filter data to keep only row with rank bellow n_max
        data <-  data %>% dplyr::filter(rank <= n_max)# %>% dplyr::mutate(group=dplyr::row_number())
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

    # define GeomTimeLine instance
    GeomTimeLineLabel <- ggplot2:: ggproto(
        "GeomTimeLineLabel", ggplot2::Geom,
        required_aes = c('x','label'),
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
