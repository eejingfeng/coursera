library(dplyr)

## This is necessary to get past the "no visible binding for global
## variable" NOTE
year = MONTH = STATE = NULL

##' Read CSV files into a tibble.  Using \code{\link[dplyr]{tbl_df}}
##' read a file supported by that function.
##' @title FARS Read Funcion
##' @param filename String contianing the name and relative path to a
##'     file.
##' @return A "tbl_df" tibble object.
##' @import dplyr
##' @import readr
##' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}


##' FARS make_filename
##' 
##' From the year, creates a valid file name to work with FARS
##' dataset.
##' @param year A string or integer representation of a year.
##' @return A string matching a valid file name for the dataset, e.g.,
##'     for 2015, \code{make_filename(2015)} will return
##'     "./data/accident_2015.csv.bz2"
##' @export
##' @examples
##' make_filename(2014)
##' make_filename("2015")
make_filename <- function(year) {
        year <- as.integer(year)
        file <- sprintf("accident_%d.csv.bz2", year)
        system.file("extdata", file, package="fars")
}


##' Create a list of year tibbles.
##'
##' For each year passed into the function, create an tibble in a list
##' that contains just the years and months from the dataset for those
##' years.
##' @title FARS Read Years
##' @param years Vector of either intigers or strings correspondig to
##'     years for which the data set is available.
##' @import dplyr
##' @return A list of FARS tibbles.
##' @examples
##' fars_read_years(c("2014", 2015))
##' @export
fars_read_years <- function(years) {
    lapply(years, function(year) {
        file <- make_filename(year)
        tryCatch({
            dat <- fars_read(file)
            dplyr::mutate(dat, year = year) %>% 
                dplyr::select(MONTH, year)
        }, error = function(e) {
            warning("invalid year: ", year)
            return(NULL)
        })
    })
}


##' Produce a summary of the number of accidents each month in the given years.
##'
##'  For each year passed into the function, create a tibble listing
##'     the number of accidents per month for that year.
##'
##' Errors: An invalid year will produce an "invalid year" warning,
##'     and result in a empty column for that year.
##' @title FARS Summarize Years
##' @param years Vector of either intigers or strings correspondig to
#'     years for which the data set is available.
##' @return A tibble of 12 rows (one per month) a column for month
##'     number, and as many year columns as were passed to the
##'     function.
##' @import dplyr
##' @import tidyr
##' @export
fars_summarize_years <- function(years) {
    dat_list <- fars_read_years(years)
    dplyr::bind_rows(dat_list) %>% 
        dplyr::group_by(year, MONTH) %>% 
            dplyr::summarize(n = n()) %>%
            tidyr::spread(year, n)
}


##' Map of a state with plotting all accidents for the given year.
##'
##' For each year and state number passed into the function, create a
##'     map of the state showing the location accidents in that year.
##' @title FARS State Maps
##' @param state.num Integer corresponding to the number of the state
##'     in the data set.
##' @param year Either string or integer representation of the year
##'     for which the map is to be drawn.
##' @return NULL; side effect: plots the map of the state.
##' @import dplyr
##' @import maps
##' @import graphics
##' @export
fars_map_state <- function(state.num, year) {
    filename <- make_filename(year)
    data <- fars_read(filename)
    state.num <- as.integer(state.num)

    if(!(state.num %in% unique(data$STATE)))
        stop("invalid STATE number: ", state.num)
    data.sub <- dplyr::filter(data, STATE == state.num)
    if(nrow(data.sub) == 0L) {
        message("no accidents to plot")
        return(invisible(NULL))
    }
    is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
    is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
    with(data.sub, {
        maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                  xlim = range(LONGITUD, na.rm = TRUE))
        graphics::points(LONGITUD, LATITUDE, pch = 46)
    })
}
