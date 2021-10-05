#' fars_read
#' 
#' @param filename File Directory
#' @return Reads the content of a .bz2 file and returns a dataframe object
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @section Warning: If file does not exist, error will show.
#' @examples 
#' fars_read("accident_2013.csv.bz2")

fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' make_filename
#'
#' @param year A numeric vector representing the years in a time period
#' @return A character vector corresponding the data filenames specified in the time period
#' @examples 
#' make_filename(2013)

make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' fars_read_years
#'
#' @param years A numeric vector representing the years in a time period
#' @return A dataframe object with car fatalities by month for each specified year
#' @importFrom dplyr mutate select %>%
#' @section Warning: If the file for the year does not exist, warning will show.
#' @examples 
#' fars_read_years(c(2013,2014))

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

#' fars_summarize_years
#'
#' @param years A numeric vector representing the years in a time period
#' @return A dataframe object with the tallies of car fatalities by month for each specified year
#' @importFrom bind_rows group_by summarize %>%
#' @importFrom tidyr spread
#' @section Warning: If the file for the year does not exist, warning will show.
#' @examples 
#' fars_summarize_years(c(2013,2014))

fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' fars_map_state 
#'
#' @param state.num An integer corresponding to a US state
#' @inheritParams make_filename
#' @return A map plot of the car fatalities occured in a given US state and in the specific year
#' @importFrom maps map
#' @importFrom dplyr filter
#' @importFrom graphics points
#' @section Warning: If the state.num is not a valid state number or the year does not exist, warning will show.
#' @examples 
#' fars_map_state(13,2013)


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