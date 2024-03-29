#' guess location
#'
#' code adapted from the scrubr package - source at github.com/ropensci/scrubr/
#' original from leaflet package - source at github.com/rstudio/leaflet
#'
#' @param x a data.frame with species records and geographical coordinates.
#' @param lat column with latitude information
#' @param lon column with longitude information
#'
#' @return A data frame with geographical coordinates
#' @export
#'
#' @examples
#' guess_latlon(tidyniche::rawSpOccPnts)
guess_latlon <- function(x, lat = NULL, lon = NULL) {
  lat_options <- c("lat", "latitude", "decimallatitude", "y")
  lon_options <- c("lon", "lng", "long", "longitude", "decimallongitude", "x")
  nms <- names(x)

  if (!is.null(attr(x, "lat_var_orig"))) lat <- attr(x, "lat_var_orig")
  if (!is.null(attr(x, "lon_var_orig"))) lon <- attr(x, "lon_var_orig")

  if (is.null(lat)) {
    lats <- nms[grep(sprintf("^(%s)$", paste0(lat_options, collapse = "|")), nms, ignore.case = TRUE)]

    if (length(lats) == 1) {
      if (length(nms) > 2) {
        message("Assuming '", lats, "' is latitude")
      }
      names(x)[names(x) %in% lats] <- lat_var <-  "latitude"
    } else {
      stop("Couldn't infer latitude column, please specify with the 'lat' parameter",
           call. = FALSE)
    }
  } else {
    lats <- lat
    if (!any(names(x) %in% lat)) stop("'", lat, "' not found in your data", call. = FALSE)
    names(x)[names(x) %in% lat] <- lat_var <- "latitude"
  }

  if (is.null(lon)) {
    lngs <- nms[grep(sprintf("^(%s)$", paste0(lon_options, collapse = "|")), nms, ignore.case = TRUE)]

    if (length(lngs) == 1) {
      if (length(nms) > 2) {
        message("Assuming '", lngs, "' is longitude")
      }
      names(x)[names(x) %in% lngs] <- lon_var <- "longitude"
    } else {
      stop("Couldn't infer longitude column, please specify with 'lon' parameter",
           call. = FALSE)
    }
  } else {
    lngs <- lon
    if (!any(names(x) %in% lon)) stop("'", lon, "' not found in your data", call. = FALSE)
    names(x)[names(x) %in% lon] <- lon_var <- "longitude"
  }

  structure(x, lat_var = lat_var, lon_var = lon_var,
            lat_var_orig = lats, lon_var_orig = lngs)
}

