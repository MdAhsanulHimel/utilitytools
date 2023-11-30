#' Seconds to Time String Conversion
#'
#' Converts a numeric value to time string
#' @param seconds numeric value containing seconds
#' @return seconds in time string
#' @examples
#' seconds_to_time_string(67)
#' seconds_to_time_string(5*60*60+9*60+58)
#' seconds_to_time_string(45*24*60*60+13*60*60+15*60+25)
#' @export
seconds_to_time_string <- function(seconds) {

  days <- floor(seconds %/% (60*60*24))
  hours <- floor((seconds / 3600) %% 24)
  minutes <- floor((seconds %% 3600)/60)
  remaining_seconds <- seconds %% 60
  time_string <- character(0L)
  if(days != 0){
    time_string <- paste0(days, " Day",ifelse(days != 1, "s", ""),
                          " ", hours, " Hour", ifelse(hours != 1, "s", ""),
                          " ", minutes, " Minute", ifelse(minutes != 1, "s", ""),
                          " ", remaining_seconds, " Second", ifelse(remaining_seconds != 1, "s", ""))

  }else if(days == 0 & hours != 0){
    time_string <- paste0(hours, " Hour", ifelse(hours != 1, "s", ""),
                          " ", minutes, " Minute", ifelse(minutes != 1, "s", ""),
                          " ", remaining_seconds, " Second", ifelse(remaining_seconds != 1, "s", ""))

  }else if(days == 0 & hours == 0 & minutes != 0){
    time_string <- paste0(minutes, " Minute", ifelse(minutes != 1, "s", ""),
                          " ", remaining_seconds, " Second", ifelse(remaining_seconds != 1, "s", ""))

  }else if(days == 0 & hours == 0 & minutes == 0 & remaining_seconds != 0){
    time_string <- paste0(remaining_seconds, " Second", ifelse(remaining_seconds != 1, "s", ""))
  }

  return(time_string)
}


#' Record Starting Time of a Program
#'
#' `begin()` starts running the stopwatch and `end()` stops the stopwatch to record run time.
#' @return Several objects loaded to the global environment
#' \itemize{
#'   \item begin_time - Starting time of the program
#'   \item end_time - Ending time of the program
#'   \item runtime - Numeric value containing elapsed seconds
#' }
#' @examples
#' begin()
#' Sys.sleep(10)
#' end()
#' @export
begin <- function() {
  begin_time <<- Sys.time();
  message("Program started: ", begin_time)
}

#' Record Ending Time of a Program
#'
#' `begin()` starts running the stopwatch and `end()` stops the stopwatch to record run time.
#' @return Several objects loaded to the global environment
#' \itemize{
#'   \item begin_time - Starting time of the program
#'   \item end_time - Ending time of the program
#'   \item runtime - Numeric value containing elapsed seconds
#' }
#' @examples
#' begin()
#' Sys.sleep(10)
#' end()
#' @export
end <- function(){
  end_time <<- Sys.time()
  runtime <<- as.numeric(format(end_time, "%s")) - as.numeric(format(begin_time, "%s"))
  message("Program end: ", end_time)
  message("Runtime: ", seconds_to_time_string(runtime))
  message("Runtime stored in \"runtime\"")
}


#' Record Run-time to an Excel File
#'
#' Records duration of a program to an Excel file. Plays a sound when task is successful.
#' @param dataname character value containing the name of the program
#' @param runtime numeric value containing the seconds took to run the program. Default to `runtime` returned by `end_time()`
#' @param runtime_file character value containing the file path to the Excel file. Default to "RUNTIME.xlsx"
#' @examples
#' begin()
#' Sys.sleep(10)
#' end()
#' write_runtime("Task 1", runtime, "Duration.xlsx")
#' @export
write_runtime <- function(dataname, runtime = runtime, runtime_file = "RUNTIME.xlsx"){

  load_packages(packages = c("readxl","openxlsx", "beepr"))

  wb <- tryCatch(loadWorkbook(runtime_file),
                 error=function(e){
                   message(paste0("Creating ", runtime_file))
                   openxlsx::createWorkbook()
                 })
  tryCatch(getSheetNames(runtime_file),
           error = function(e) {
             message("Adding Sheet 1")
             openxlsx::addWorksheet(wb, "Sheet 1")
           })
  saveWorkbook(wb, runtime_file, overwrite = TRUE)

  runtime_data <- rbind(readxl::read_excel(runtime_file),
                        data.frame(Data = dataname, Runtime = runtime))

  openxlsx::write.xlsx(runtime_data, file = runtime_file, overwrite = TRUE)

  beepr::beep(2)
}

#' Calculate Total Run-time of a Program
#'
#' This function calculates total run-time of a program from the run-time excel file created using the function `write_runtime()`.
#' @param runtime_file character value containing the file path to the Run-time Excel file. Default to "RUNTIME.xlsx"
#' @examples
#' begin()
#' Sys.sleep(10)
#' end()
#' write_runtime("Task 1", runtime, "Duration.xlsx")
#' begin()
#' Sys.sleep(15)
#' end()
#' write_runtime("Task 2", runtime, "Duration.xlsx")
#' total_runtime("Duration.xlsx")
#' # Data	  Runtime
#' # Task 1	10
#' # Task 2	15
#' # Total	25 Seconds
#' @export
total_runtime <- function(runtime_file = "RUNTIME.xlsx"){

  # Total run time calculation
  runtime_data <- readxl::read_excel(runtime_file)
  total_time <- seconds_to_time_string(sum(as.numeric(runtime_data$Runtime), na.rm = TRUE))

  runtime_data <- rbind(runtime_data,
                        data.frame(Data = "Total",
                                   Runtime = total_time))
  openxlsx::write.xlsx(runtime_data, file = runtime_file, overwrite = TRUE)
}
