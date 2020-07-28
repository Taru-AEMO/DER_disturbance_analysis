setwd(dirname(parent.frame(2)$ofile))
source("R/interface.R")

load_test_file <- function(path_name){
  data <- read.csv(file = path_name, header = TRUE, stringsAsFactors = FALSE)
  return(data)
}

timeseries_path_name <- "tests/testthat/data/simple_timeseries.csv"
site_details_path_name <- "tests/testthat/data/simple_site_details.csv"
circuit_details_path_name <- "tests/testthat/data/simple_circuit_details.csv"

# Create the DBInterface and test creating the database.
if (file.exists("test.db")) {file.remove("test.db")}
dp <- DBInterface$new()
dp$connect_to_database("test.db")
dp$build_database(timeseries = timeseries_path_name,
                  circuit_details = circuit_details_path_name,
                  site_details = site_details_path_name)

output_timeseries <- dp$get_time_series_data()
output_site_details <- dp$get_site_details_raw()
output_circuit_details <- dp$get_circuit_details_raw()