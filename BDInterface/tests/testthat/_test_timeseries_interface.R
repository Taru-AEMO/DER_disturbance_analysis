testthat::context("Testing the creation of the timeseries file in the database.")


load_test_file <- function(text){
  df <- read.table(text = gsub(" ", "", text), sep = ",", header = TRUE, stringsAsFactors = FALSE)
  return(df)
}

save_test_file <- function(df, name){
  write.csv(df, name, row.names = FALSE, quote = FALSE)
}

testthat::test_that("Building works when no column aliases are required.",{
  
  x <- "                 ts, c_id, d,      e,   v,  f
        2018-01-01 00:00:00,   10, 5,  1.100, 240, 50
        2018-01-01 00:05:00,   10, 5,  1.100, 240, 50
        2018-01-01 00:10:00,   10, 5,  1.100, 240, 50"
  
  csv_name <- 'test.csv'
  
  expected_timeseries <- load_test_file(x)
  save_test_file(expected_timeseries, csv_name)
  
  if (file.exists("test.db")) {file.remove("test.db")}
  dp <- TimeSeriesTable$new()
  dp$set_database("test.db")
  dp$build_table(csv_name)
  
  output_timeseries <- dp$get_time_series_data()
  testthat::expect_equal(output_timeseries, expected_timeseries)
})