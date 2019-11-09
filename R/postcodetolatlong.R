#' @export
#' @title postcodetolatlong
#'
#' @description
#' \code{postcodetolatlong} is a macro (rather than a function) which appends a coarse latitude and longitude
#' of the patient's home address (using the outer portion of the postcode only)
#'
#' @details
#' This macro takes as input a tibble (an optimised data frame; see
#' the tibble package).
#' 
#' The tibble should contain a column labelled S1PostcodeOut
#' representing the time of clock start for the patient at the
#' admitting hospital, and S2BrainImagingDateTime representing the
#' time of first imaging.
#' 
#' The tibble is updated with a new column containing the times in
#' minutes between these values, expressed as numerics.
#' 
#' The best place for this to be calculated is at record creation,
#' rather than in routine analysis. The function exists where we do
#' not have the facility to calculate the value on record creation.
#'
#' This function uses a postcode CSV file,  postcode-outcodes.csv,
#' obtained from 
#' https://www.freemaptools.com/download-uk-postcode-lat-lng.htm .
#' 
#' Using outer postcodes only should be sufficent for most forms of
#' aggregated data and mapping. The likely exceptions to this rule
#' are modelling of transport times or more detailed mapping: however
#' checking coordinates using a full postcode could be extremely
#' processor-intensive as the postcode map is several orders of
#' magnitude larger.
#' 
#' Postcode data is provided under license from the National Archives
#' and contains public sector information licensed under the Open
#' Government License version 3.0
#' http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/
#' 
#' @param data_table A tibble (optimised data frame) containing
#' S1PostcodeOut.
#' @return Longitudes and latitudes where known based upon outer
#' portion of postcode, in the new columns S1PostcodeOutLatitude and
#' S1PostcodeOutLongitude.
#' @author Andrew Hill, \email{andrew.hill@@doctors.org.uk}

postcodetolatlong <- function(data_table) {
  postcode_csv_path <- system.file("extdata", "postcode-outcodes.csv",
    package = "ssnapstats")
  long_lat <- readr::read_csv(postcode_csv_path,
    col_names = TRUE,
    readr::cols(id = readr::col_integer(),
                postcode = readr::col_character(),
                latitude = readr::col_double(),
                longitude = readr::col_double()
    ))
  long_lat <- dplyr::rename(long_lat,
    S1PostcodeOut = .data[["postcode"]])
  data_table <- dplyr::left_join(data_table, long_lat,
    by = "S1PostcodeOut")
  data_table <- dplyr::rename (data_table,
    S1PostcodeOutLatitude = .data[["latitude"]],
    S1PostcodeOutLongitude = .data[["longitude"]])
}
