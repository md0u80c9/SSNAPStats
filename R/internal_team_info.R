internal_team_info <- function() {
  team_csv_path <- system.file("extdata", "teamdetails.csv",
                             package = "ssnapstats")
  team_data <- readr::read_csv(team_csv_path,
                  col_names = TRUE,
                  readr::cols(
                    TeamCode = readr::col_character(),
                    TeamName = readr::col_character(),
                    Country = readr::col_character(),
                    TeamType = readr::col_skip(),
                    Active = readr::col_skip(),
                    ParticipationFrom = readr::col_skip(),
                    ExpectedQuarterlyCases = readr::col_skip(),
                    Address1 = readr::col_skip(),
                    Address2 = readr::col_skip(),
                    Town = readr::col_skip(),
                    County = readr::col_skip(),
                    Postcode = readr::col_skip(),
                    NhsOrganisation = readr::col_skip(),
                    NhsHospitalCode = readr::col_skip(),
                    AMBT_Groups = readr::col_skip(),
                    TRUST_Groups = readr::col_skip(),
                    TRUST_Names = readr::col_character(),
                    SCN_Groups = readr::col_skip(),
                    SCN_Names = readr::col_character(),
                    SHAC_Groups = readr::col_skip()
                  ))
  dplyr::mutate(team_data,
    TeamCode = ssnapinterface::teamcode_to_number(
      .data[["TeamCode"]]))
}
