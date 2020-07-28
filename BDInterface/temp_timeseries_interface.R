TimeSeriesTable <- R6::R6Class("TimeSeriesTable",
  public = list(
    db_path_name = NULL,
    default_timeseries_column_aliases = list(ts = '_ts', time_stamp = '_ts', c_id = '_c_id', v = '_v', 
                                             f = '_f', e = '_e', d = '_d', p = '_p'),
    set_database = function(db_path_name){
      self$db_path_name = db_path_name
    },
    build_table = function(timeseries_csv){
      con <- RSQLite::dbConnect(RSQLite::SQLite(), self$db_path_name)
      tryCatch({
        self$clear_existing_table(con)
        self$create_new_table(con)
        self$insert_csv_data_into_table(timeseries_csv)
        self$drop_repeated_headers(con)
      },
      finally = {
        RSQLite::dbDisconnect(con)
      })
    },
    clear_existing_table = function(con){
      RSQLite::dbExecute(con, "DROP TABLE IF EXISTS timeseries")
    },
    create_new_table = function(con){
      RSQLite::dbExecute(con, "CREATE TABLE timeseries(
                                ts TEXT,
                                c_id INT,
                                d INT,
                                e REAL,
                                v REAL,
                                f REAL,
                                PRIMARY KEY (ts, c_id)
                               )"
                         )
    },
    insert_csv_data_into_table = function(timeseries_csv){
      time_series_build_query <- self$create_time_series_build_query(timeseries_csv)
      sqldf::read.csv.sql(timeseries_csv, sql = time_series_build_query, dbname = self$db_path_name, eol = '\n')
    },
    create_time_series_build_query = function(timeseries_csv){
      column_names <- names(read.csv(timeseries_csv, nrows = 3, header = TRUE))
      
      query <- "REPLACE INTO timeseries 
      SELECT _ts as ts, _c_id as c_id, _d as d, _e as e, _v as v,
      _f as f from file"
      
      for (name in column_names){ 
        if (name %in% names(self$default_timeseries_column_aliases)){ 
          query <- gsub(self$default_timeseries_column_aliases[[name]], name, query)
        } else {
          error_message <- "The provided time series file should have the columns ts, c_id, d, e, v and f, 
          or known aliases of these columns. 
          
          The columns _cols_ where found instead.
          
          Please overide the column alaises in the database interface and try again."
          
          error_message <- gsub('_cols_', paste(column_names, collapse = ', '), error_message)
          
          stop(error_message)
        }
      }
      return(query)
      
    },
    drop_repeated_headers = function(con){
      RSQLite::dbExecute(con, "DELETE FROM timeseries where ts=='ts'")
    },
    get_time_series_data_by_c_id = function(c_ids){
      time_series <- sqldf::read.csv.sql(
        sql = "select * from timeseries where c_id in (select c_id from c_ids)", dbname = self$db_path_name)
      return(time_series)
    },
    get_time_series_data = function(){
      time_series <- sqldf::read.csv.sql(sql = "select * from timeseries", dbname = self$db_path_name)
      time_series <- time_series[with(time_series, order(c_id, ts)), ]
      rownames(time_series) <- NULL
      return(time_series)
    },
    get_filtered_time_series_data = function(state, duration, start_time, end_time){
      circuit_details_reader <- CircuitDetailsTable()
      circuit_details_reader$set_database(db_path_name)
      site_details_reader <- SiteDetailsTable()
      site_details_reader$set_database(db_path_name)
      circuit_details = circuit_details_reader$get_circuit_details_raw()
      site_details = site_details_reader$get_site_details_raw()
      site_in_state = filter(site_details, s_state == state)
      circuit_in_state = filter(circuit_details, site_id %in% site_in_state$site_id)
      query <- "select * from timeseries 
                         where c_id in (select c_id from circuit_in_state)
                         and d = duration
                         and ts >= 'start_time'
                         and ts <= 'end_time'"
      query <- gsub('duration', duration, query)
      query <- gsub('start_time', start_time, query)
      query <- gsub('end_time', end_time, query)
      time_series <- sqldf::read.csv.sql(sql = query , dbname = self$db_path_name)
      return(time_series)
    },
    update_timeseries_table_in_database = function(time_series){
      sqldf::read.csv.sql(
        sql = "REPLACE INTO timeseries SELECT ts, c_id, d, e, v, f FROM time_series", 
        dbname = self$db_path_name)
    },
    get_min_timestamp = function(){
      con <- RSQLite::dbConnect(RSQLite::SQLite(), self$db_path_name)
      min_timestamp <- RSQLite::dbGetQuery(con, "SELECT MIN(ts) as ts FROM timeseries")
      RSQLite::dbDisconnect(con)
      return(fastPOSIXct(min_timestamp$ts[1], tz = "Australia/Brisbane"))
    },
    get_max_timestamp = function(){
      con <- RSQLite::dbConnect(RSQLite::SQLite(), self$db_path_name)
      max_timestamp <- RSQLite::dbGetQuery(con, "SELECT MAX(ts) as ts FROM timeseries")
      RSQLite::dbDisconnect(con)
      return(fastPOSIXct(max_timestamp$ts[1], tz = "Australia/Brisbane"))
    }
    ))


CircuitDetailsTable <- R6::R6Class("CircuitDetailsTable",
  public = list(
    build_circuit_details_table <- function(db_path_name, circuit_details_csv){
      check_circuit_details_header(circuit_details_csv)
      con <- RSQLite::dbConnect(RSQLite::SQLite(), self$db_path_name)
      tryCatch({
        self$clear_existing_circuit_details(con)
        self$create_new_circuit_details_table(con)
        self$insert_csv_data_into_circuit_details_table(circuit_details_csv)
        self$set_default_table_values()
      },
      finally = {
        RSQLite::dbDisconnect(con)
      })
    },
    clear_existing_circuit_details = function(con){
      RSQLite::dbExecute(con, "DROP TABLE IF EXISTS circuit_details_raw")
    },
    create_new_circuit_details_table = function(con){
      RSQLite::dbExecute(con, "CREATE TABLE circuit_details_raw(
                           c_id INT PRIMARY KEY,
                         site_id INT,
                         con_type TEXT,
                         polarity REAL)")
    },
    insert_csv_data_into_circuit_details_table = function(circuit_details_csv){
      query <- "REPLACE INTO circuit_details_raw  
                             SELECT c_id, site_id,  con_type, polarity 
                             FROM file"
      sqldf::read.csv.sql(circuit_details_csv, sql = query, dbname = self$db_path_name, eol = '\n')
    },
    check_circuit_details_header = function(circuit_details_csv){
      column_names <- names(read.csv(circuit_details_csv, nrows = 3, header = TRUE))
      
      expected_columns <- list('c_id', 'site_id', 'con_type', 'polarity')
      
      for (name in column_names){
        if (!(name %in% expected_columns)){
          stop("The provided circuit details file should have the columns c_id, site_id, con_type
               and polarity. Please check this file and try again.")
        }
      }
    },
    set_default_table_values = function(){
      RSQLite::dbExecute(con, "ALTER TABLE circuit_details_raw ADD manual_compliance TEXT DEFAULT 'Not set'")
    },
    get_circuit_details_raw = function(){
      circuit_details_raw <- sqldf::read.csv.sql(sql = "select * from circuit_details_raw", dbname = self$db_path_name)
      return(circuit_details_raw)
    },
    get_circuit_details_cleaned = function(){
      circuit_details_cleaned <- sqldf::read.csv.sql(sql = "select c_id, site_id, con_type, polarity, manual_compliance
                                                              from circuit_details_cleaned", 
                                                     dbname = self$db_path_name)
      return(circuit_details_cleaned)
    },
    get_circuit_details_cleaning_report = function(){
      circuit_details_cleaned <- sqldf::read.csv.sql(sql = "select * from circuit_details_cleaned", 
                                                     dbname = self$db_path_name)
      return(circuit_details_cleaned)
    },
    create_circuit_details_cleaned_table = function(){
      con <- RSQLite::dbConnect(RSQLite::SQLite(), self$db_path_name)
      RSQLite::dbExecute(con, "DROP TABLE IF EXISTS circuit_details_cleaned")
      RSQLite::dbExecute(con, "CREATE TABLE circuit_details_cleaned(
                         c_id INT PRIMARY KEY, site_id INT, con_type TEXT, polarity INT, sunrise TEXT, sunset TEXT, 
                         min_power REAL, max_power REAL, frac_day REAL, old_con_type TEXT,
                         con_type_changed INT, polarity_changed INT, manual_compliance TEXT)")
      RSQLite::dbDisconnect(con)
    },
    insert_circuit_details_cleaned = function(circuit_details){
      query <- "INSERT INTO circuit_details_cleaned
      SELECT c_id, site_id, con_type, polarity, sunrise, sunset, min_power, max_power, frac_day, 
      old_con_type, con_type_changed, polarity_changed, manual_compliance
      FROM circuit_details"
      sqldf::read.csv.sql(sql = query, dbname = self$db_path_name)
    },
    update_circuit_details_cleaned = function(circuit_details){
      query <- "REPLACE INTO circuit_details_cleaned
      SELECT c_id, site_id, con_type, polarity, sunrise, sunset, min_power, max_power, frac_day, 
      old_con_type, con_type_changed, polarity_changed, manual_compliance
      FROM circuit_details"
      sqldf::read.csv.sql(sql = query, dbname = self$db_path_name)
    },
    update_circuit_details_raw = function(circuit_details){
      query <- "REPLACE INTO circuit_details_raw
      SELECT c_id,  site_id,  con_type, polarity, manual_compliance
      FROM circuit_details"
      sqldf::read.csv.sql(sql = query, dbname = self$db_path_name)
    }
    ))

SiteDetailsTable <- R6::R6Class("SiteDetailsTable",
  public = list(
   build_site_details_table <- function(db_path_name, site_details_csv){
     check_site_details_header(site_details_csv)
     con <- RSQLite::dbConnect(RSQLite::SQLite(), self$db_path_name)
     tryCatch({
       self$clear_existing_site_details(con)
       self$create_new_site_details_table(con)
       self$insert_csv_data_into_site_details_table(site_details_csv)
     },
     finally = {
       RSQLite::dbDisconnect(con)
     })
   },
   clear_existing_site_details = function(con){
     RSQLite::dbExecute(con, "DROP TABLE IF EXISTS site_details_raw")
   },
   create_new_site_details_table = function(con){
     RSQLite::dbExecute(con, "CREATE TABLE site_details_raw(
                                site_id INT, 
                                s_postcode INT,
                                s_state TEXT, 
                                ac REAL, 
                                dc REAL, 
                                manufacturer TEXT, model TEXT,
                                pv_installation_year_month TEXT)")
   },
   insert_csv_data_into_site_details_table = function(site_details_csv){
     query <- "REPLACE INTO site_details_raw 
                            SELECT site_id,  s_postcode,  s_state, ac, dc,  manufacturer, model,  
                                   pv_installation_year_month 
                              FROM site_details"
     site_details <- read.csv(file = site_details_csv, header = TRUE, stringsAsFactors = FALSE)
     sqldf::read.csv.sql(sql = site_details_build_query, dbname = self$db_path_name)
   },
   check_site_details_header = function(site_details_csv){
     column_names <- names(read.csv(site_details_csv, nrows = 3, header = TRUE))
     
     expected_columns <- list('site_id', 's_state', 's_postcode', 'ac', 'dc', 'manufacturer', 'mode', 
                              'pv_installation_year_month')
     
     for (name in column_names){
       if (!(name %in% expected_columns)){
         stop("The provided site details file should have the columns site_id, s_postcode, s_state,
               ac, dc, manufacturer, model and pv_installation_year_month. The ac column should be in
               kW and the the dc in W. Please check this file and try again.")
       }
     }
   },
   get_site_details_raw = function(){
     site_details_raw <- sqldf::read.csv.sql(sql = "select * from site_details_raw", dbname = self$db_path_name)
     return(site_details_raw)
   },
   get_site_details_cleaned = function(){
     site_details_cleaned <- sqldf::read.csv.sql(sql = "select site_id, s_state, s_postcode, ac,
                                                 manufacturer, model, pv_installation_year_month
                                                 from site_details_cleaned", 
                                                 dbname = self$db_path_name)
     return(site_details_cleaned)
   },
   get_site_details_cleaning_report = function(){
     site_details_cleaned <- sqldf::read.csv.sql(sql = "select * from site_details_cleaned", 
                                                 dbname = self$db_path_name)
     return(site_details_cleaned)
   },
   create_site_details_cleaned_table = function(){
     con <- RSQLite::dbConnect(RSQLite::SQLite(), self$db_path_name)
     RSQLite::dbExecute(con, "DROP TABLE IF EXISTS site_details_cleaned")
     RSQLite::dbExecute(con, "CREATE TABLE site_details_cleaned(
                                 site_id INT PRIMARY KEY, s_postcode INT, s_state TEXT, pv_installation_year_month TEXT, 
                                 ac REAL, dc REAL, ac_old REAL, dc_old REAL, ac_dc_ratio REAL, manufacturer TEXT, 
                                 model TEXT, rows_grouped REAL, max_power_kW REAL, change_ac INT, change_dc INT)")
     RSQLite::dbDisconnect(con)
   },
   insert_site_details_cleaned = function(site_details){
     query <- "INSERT INTO site_details_cleaned
     SELECT site_id, s_postcode, s_state, pv_installation_year_month, ac, dc, ac_old, dc_old, 
     ac_dc_ratio, manufacturer, model, rows_grouped, max_power_kW, change_ac, change_dc
     FROM site_details"
     sqldf::read.csv.sql(sql = query, dbname = self$db_path_name)
   },
   update_site_details_cleaned = function(site_details){
     query <- "REPLACE INTO site_details_cleaned
     SELECT site_id, s_postcode, s_state, pv_installation_year_month, ac, dc, ac_old, dc_old, 
     ac_dc_ratio, manufacturer, model, rows_grouped, max_power_kW, change_ac, change_dc 
     FROM site_details"
     sqldf::read.csv.sql(sql = query, dbname = self$db_path_name)
   }
  ))

DataCleaningInterface <- R6::R6Class("DataCleaningInterface",
  public = list(
    set_database = function(db_path_name){
      con <- RSQLite::dbConnect(RSQLite::SQLite(), db_path_name)
      RSQLite::dbDisconnect(con)
      self$db_path_name = db_path_name
      self$timeseries_interface <- TimeSeriesInterface$new()
      self$timeseries_interface$set_database(db_path_name)
      self$circuit_details_interface <- CircuitDetailsInterface$new()
      self$circuit_details_interface$set_database(db_path_name)
      self$site_details_interface <- SiteDetailsInterface$new()
      self$site_details_interface$set_database(db_path_name)
      self$postcode_interface <- PostcodeInterface$new()
      self$postcode_interface$set_database(db_path_name)
    },
    run_data_cleaning_loop = function(max_chunk_size=500){
      con <- RSQLite::dbConnect(RSQLite::SQLite(), self$db_path_name)
      
      circuit_details <- self$circuit_details_interface$get_circuit_details_raw()
      site_details <- self$site_details_interface$get_site_details_raw()
      postcode_data <- self$get_postcode_lon_lat()
      
      site_details <- site_details_data_cleaning_one(site_details)
      
      # Setup for first iteration.
      length_ids <- length(site_details$site_id)
      iteration_number <- 1
      start_chunk_index <- self$calc_start_chunk_index(iteration_number, max_chunk_size)
      end_chunk_index <- self$calc_end_chunk_index(length_ids, max_chunk_size, start_chunk_index)
      sites_in_chunk <- self$fiter_dataframe_by_start_and_end_index(site_details, start_chunk_index, end_chunk_index)
      circuits <- filter(circuit_details, site_id %in% sites_in_chunk$site_id)
      
      site_details_cleaned <- dplyr::data_frame()
      circuit_details_cleaned <- dplyr::data_frame()
      
      while (length(circuits$c_id)){
        
        time_series <- self$get_time_series_data_by_c_id(circuits)
        time_series <- mutate(time_series, time = fastPOSIXct(ts, tz="Australia/Brisbane"))
        time_series <- self$clean_duration_values(time_series)
        updated_records <- self$filter_out_unchanged_records(time_series)
        self$update_timeseries_table_in_database(updated_records)
        time_series <- mutate(time_series, ts=time)
        
        time_series <- self$add_meta_data_to_time_series(time_series, circuit_details)
        time_series <- self$perform_power_calculations(time_series)
        pv_time_series <- filter(time_series, con_type %in% c("pv_site_net", "pv_site", "pv_inverter_net", "pv_inverter"))
        site_details_cleaned_chunk <- site_details_data_cleaning_two(pv_time_series, sites_in_chunk)
        site_details_cleaned <- bind_rows(site_details_cleaned, site_details_cleaned_chunk)
        
        details_to_add <- select(site_details_cleaned_chunk,  site_id, s_postcode, ac)
        time_series <- inner_join(time_series, details_to_add, by='site_id')
        
        circuit_details_cleaned_chunk <- clean_connection_types(time_series, circuits, postcode_data)
        circuit_details_cleaned <- bind_rows(circuit_details_cleaned, circuit_details_cleaned_chunk)
        
        print(paste0("Done cleaning batch ", iteration_number))
        
        # Setup for next iteration.
        iteration_number <- iteration_number + 1
        start_chunk_index <- self$calc_start_chunk_index(iteration_number, max_chunk_size)
        end_chunk_index <- self$calc_end_chunk_index(length_ids, max_chunk_size, start_chunk_index)
        sites_in_chunk <- self$fiter_dataframe_by_start_and_end_index(site_details, start_chunk_index, end_chunk_index)
        circuits <- filter(circuit_details, site_id %in% sites_in_chunk$site_id)
        #if (start_chunk_index > length_ids){circuits <- ids[0:0,,drop=FALSE]}
      }
      
      self$site_details_interface$create_site_details_cleaned_table()
      self$site_details_interface$insert_site_details_cleaned(site_details_cleaned)
      
      self$circuit_details_interface$create_circuit_details_cleaned_table()
      self$circuit_details_interface$insert_circuit_details_cleaned(circuit_details_cleaned)
      
      RSQLite::dbDisconnect(con)
    },
    calc_start_chunk_index = function(iteration_number, max_chunk_size){
      start_chunk_index <- (iteration_number - 1) * max_chunk_size + 1
      return(start_chunk_index)
    },
    calc_end_chunk_index = function(number_of_ids, max_chunk_size, start_chunk_index){
      end_chunk_index <- start_chunk_index + max_chunk_size - 1
      if (end_chunk_index > number_of_ids){end_chunk_index <- number_of_ids}
      return(end_chunk_index)
    },
    fiter_dataframe_by_start_and_end_index = function(df, start_index, end_index){
      df <- df[start_index:end_index,,drop=F]
      return(df)
    },
    clean_duration_values = function(time_series){
      time_series <- self$calc_interval_between_measurements(time_series)
      time_series <- self$flag_duration_for_updating_if_value_non_standard_and_calced_interval_is_5s(time_series)
      time_series <- self$replace_duration_value_with_calced_interval(time_series)
      return(time_series)
    },
    calc_interval_between_measurements = function(time_series){
      time_series <- time_series %>% dplyr::group_by(c_id) %>% 
        dplyr::mutate(interval = time - lag(time, order_by = time))
      return(time_series)
    },
    filter_out_unchanged_records = function(time_series){
      time_series <- dplyr::filter(time_series, d_change)
      time_series <- select(time_series, ts, c_id, d, e, v, f)
      return(time_series)
    },
    replace_duration_value_with_calced_interval = function(time_series){
      time_series <- dplyr::mutate(time_series, d=ifelse(d_change, 5, d))
      return(time_series)
    },
    flag_duration_for_updating_if_value_non_standard_and_calced_interval_is_5s = function(time_series){
      time_series <- dplyr::mutate(time_series, d_change=ifelse((interval %in% 5) & (!d %in% c(5, 30, 60)), TRUE, FALSE))
      return(time_series)
    },
    update_timeseries_table_in_database = function(time_series){
      self$timeseries_interface$update_timeseries_table_in_database(time_series)
    },
    perform_power_calculations = function(time_series){
      time_series <- mutate(time_series, d = as.numeric(d))
      time_series <- mutate(time_series, e_polarity=e*polarity)
      time_series <- mutate(time_series, power_kW = e_polarity/(d * 1000))
      return(time_series)
    },
    add_meta_data_to_time_series = function(time_series, circuit_details){
      details_to_add <- select(circuit_details, c_id, site_id, polarity, con_type)
      time_series <- inner_join(time_series, details_to_add, by = 'c_id')
      return(time_series)
    }
  ))

