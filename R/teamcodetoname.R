#' @export
#' @title append_team_data_from_teamcode
#'
#' @description
#' \code{teamcodetoname} is a macro (rather than a function) which converts team codes in the raw SSNAP
#' data to team names.
#'
#' @details
#' This macro takes as input a tibble (an optimised data frame; see the tibble package).
#' 
#' The function API needs nailing down more firmly - at present it knows of three hard-coded column
#' names (TeamName, TransferFrom and TransferTo) but it should be able to work more generically in
#' future if we provide it with a column name to translate and an output name.
#' 
#' It also requires Unit tests which currently we don't have. Take this function as a 'work in progress'
#' It will not win an R beauty contest any time soon.
#' 
#' @param data_table A tibble (optimised data frame) containing S1PostcodeOut.
#' @return Data table updated with Team names in TeamName, TransferToTeamName, TransferFromTeamName,
#' and S7TransferHospitalName
#' @author Andrew Hill, \email{andrew.hill@@doctors.org.uk}

append_team_data_from_teamcode <- function(data_table) {
  if ("TeamCode" %in% names(data_table)) {
    teamcsvpath <- system.file("extdata", "teamdetails.csv",
      package = "ssnapstats")
    team_data <- readr::read_csv(teamcsvpath,
      col_names = TRUE,
      readr::cols_only(TeamCode  = readr::col_character(),
                       TeamName  = readr::col_character(),
                       TeamType  = readr::col_character(),
                       SCN_Names = readr::col_character(),
                       TRUST_Names = readr::col_character()#,
#                      Country   = readr::col_character()
    ))

  team_data <- dplyr::mutate(team_data, "TeamCode" =
                               ssnapinterface::teamcode_to_number(.data[["TeamCode"]]))

  # Use the data as-is to change the team code into a team name
  data_table <- dplyr::inner_join(data_table,
                                  team_data,
                                  by = "TeamCode")
  }
  return(data_table)
}


old_teamcodetoname <- function(data_table) {
  teamcsvpath <- system.file("extdata", "teamdetails.csv",
    package = "ssnapstats")
  team_data <- readr::read_csv(teamcsvpath,
    col_names = TRUE,
    readr::cols(TeamCode = readr::col_character(),
      TeamName = readr::col_character(),
      Country = readr::col_character()
  ))
# Use the data as-is to change the team code into a team name
  data_table <- dplyr::left_join(data_table, team_data, by = "TeamCode")

# We don't need country codes for transfers so drop the country column
  team_data$Country <- NULL

# Now rename the team code columns to do the same translation to do
# TransferTo and TransferFrom
  team_data <- dplyr::rename (team_data,
    TransferToTeamCode = .data[["TeamCode"]],
    TransferToTeamName = .data[["TeamName"]])
  data_table <- dplyr::left_join(data_table, team_data,
    by = "TransferToTeamCode")

  team_data <- dplyr::rename (team_data,
    TransferFromTeamCode = .data[["TransferToTeamCode"]],
    TransferFromTeamName = .data[["TransferToTeamName"]])
  data_table <- dplyr::left_join(data_table, team_data,
    by = "TransferFromTeamCode")

  team_data <- dplyr::rename (team_data,
    S7TransferTeamCode = .data[["TransferFromTeamCode"]],
    S7TransferTeamName = .data[["TransferFromTeamName"]])
  data_table <- dplyr::left_join(data_table, team_data,
    by = "S7TransferTeamCode")
}
