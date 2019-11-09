# This is (disabled) code to connect to SQL Server
# It will be needed for later development.
#con <- dbConnect(odbc::odbc(),
#                 Driver = "ODBC Driver 13 for SQL Server",
#                 timeout = 10, server = "Test-PC,50789",
#                 database = "odbc_test", uid="ssnap_user",
#                 pwd = "ssnap")

# test_tbl <- dplyr::tbl(con, "odbc_test")