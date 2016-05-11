# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' The best travel function - deprecated
#'
#' This function calculates the travel index with the minimum length:
#'  0 if the current travel has the minimum length;
#' -1 if the previous travel has the minimum length;
#'  1 if the next travel has the minimum length;
#'
#' @param prev_travel length of the previous travel in minutes
#' @param curr_travel length of the main travel in minutes
#' @param later_travel length of the next travel in minutes
#'
#' @return -1 or 0 or 1
#'
#' @export
#'
#' @examples
#' best_travel_deprecated(20,21,30)
#' best_travel_deprecated(NA,21,30)
#' best_travel_deprecated(20,21,NA)
best_travel_deprecated <- function(prev_travel, curr_travel, later_travel){
  if (!is.na(prev_travel) && !is.na(curr_travel) && prev_travel == curr_travel && !is.na(later_travel) && curr_travel <= later_travel) {
    return(0)
  }
  lista <- c(prev_travel, curr_travel, later_travel)
  return (which.min(lista) - 2)
}

#' Time difference between 3 consecutive travels - deprecated
#'
#' This function calculates the difference between the current travel and the best travel.
#'
#' @param prev_travel length of the previous travel in minutes
#' @param curr_travel length of the current travel in minutes
#' @param later_travel length of the next travel in minutes
#'
#' @return This function returns the difference between the current travel and the minimum of the specifieds travels.
#' @export
#'
#' @examples
#' best_length_diff_deprecated(35,100,40)
#' best_length_diff_deprecated(NA,100,40)
#' best_length_diff_deprecated(35,100,NA)
best_length_diff_deprecated <- function(prev_travel, curr_travel, later_travel) {
  return(curr_travel - min(c(prev_travel, curr_travel, later_travel), na.rm = TRUE))
}

#' Time difference
#'
#' This function calculates the difference in minutes between 2 schedules
#'
#' @param schedule1 time in format "hh:mm:ss"
#' @param schedule2 time in format "hh:mm:ss"
#'
#' @return the difference in minutes between 2 schedules
#' @export
#'
#' @examples
#' time_difference("12:30:45", "12:50:10")
#' time_difference("13:30:00", "12:50:00")
time_difference <- function(schedule1, schedule2){
  diff <- as.numeric(
    difftime(
      strptime(paste("2015-03-03", schedule1), "%Y-%m-%d %H:%M:%S"),
      strptime(paste("2015-03-03", schedule2), "%Y-%m-%d %H:%M:%S"),
      units = "mins"
    )
  )
  return(abs(diff))
}

#' The best travel function
#'
#' This function calculates the index of the minimum travel as long as
#' the difference between the best travel and the current don't extrapolate
#' the time limit and the interval between the current travel schedule and
#' the best travel schedule
#'
#' @param prev_travel length of the previous travel in minutes
#' @param curr_travel length of the main travel in minutes
#' @param next_travel length of the next travel in minutes
#' @param diff_prev_schedule difference in minutes between the current travel and the previous
#' @param diff_next_schedule difference in minutes between the current travel and the next
#' @param duration_limit time limit applied on the difference between the best travel duration and the current travel duration
#' @param time_limit headway maximum time in order that a travel can be considered the best
#'
#' @return -1 or 0 or 1
#' @export
#'
#' @examples
#' best_travel(50,32,20,25,35,5,45)
#' best_travel(50,32,20,25,35,5,30)
#' best_travel(26,32,20,25,35,5,30)
best_travel <- function(prev_travel, curr_travel, next_travel, diff_prev_schedule, diff_next_schedule, duration_limit, time_limit) {
  if (!is.na(prev_travel) && !is.na(curr_travel) &&
      prev_travel == curr_travel && !is.na(next_travel) &&
      curr_travel <= next_travel) {
    return(0)
  }

  lista <- c(prev_travel, curr_travel, next_travel)
  best.index <- which.min(lista)

  # Diferença de duração é menor que "duration_limit" minutos
  if (abs(lista[best.index] - lista[2]) <= duration_limit) {
    return(0)
  }

    # Horário da viagem alternativa para a viagem atual é maior que "time_limit" minutos
  if (best.index == 1) {
    if (diff_prev_schedule >= time_limit) {
      if (!is.na(next_travel) && next_travel < curr_travel && diff_next_schedule < time_limit) {
        return(1)
      } else {
        return(0)
      }
    }
  } else if (best.index == 3) {
    if (diff_next_schedule >= time_limit) {
      if (!is.na(prev_travel) && prev_travel < curr_travel && diff_prev_schedule < time_limit) {
        return(-1)
      } else {
        return(0)
      }
    }
  }
  return (best.index - 2)
}

#' Time difference between 3 different travels
#'
#' This function calculates the difference between the current travel and the best travel.
#' It considers that the function best_travel has already been used to calculate the best_travel.
#'
#' @param prev_travel length of the previous travel in minutes
#' @param curr_travel length of the current travel in minutes
#' @param next_travel length of the next travel in minutes
#' @param best_travel best travel -1 or 0 or 1
#'
#' @return This function returns the difference between the current travel and the minimum of the 3 different travels.
#' @export
#'
#' @examples
#' best_length_diff(20,30,45,-1)
#' best_length_diff(35,30,25,1)
#' best_length_diff(35,30,31,0)
best_length_diff <- function(prev_travel, curr_travel, next_travel, best_travel) {
  list <- c(prev_travel, curr_travel, next_travel)
  return(abs(curr_travel - list[best_travel + 2]))
}

#' Group minutes
#'
#' This function is similar to the floor function, but instead of an integer, it is passed a schedule.
#' The value will be decreased acording to the passed delimiter
#'
#' @param schedule schedule to be decreased in the format "hh:mm:ss"
#' @param delimiter amount of minutes to delimit the schedule
#'
#' @return the schedule descreased
#' @export
#'
#' @examples
#' group_minutes("02:17:05", 15)
#' group_minutes("02:17:05", 20)
group_minutes <- function(schedule, delimiter) {
  timesplit <- strsplit(toString(schedule), ":")[[1]]
  seconds <- as.integer(timesplit[1]) * 3600 + as.integer(timesplit[2]) * 60 + as.integer(timesplit[2])
  group <- as.integer(seconds / (delimiter * 60)) * (delimiter * 60)
  hours <- as.integer(group / 3600)
  minutes <- as.integer(group %% 3600 / 60)
  seconds <- as.integer(group %% 3600 %% 60)
  return(paste(sprintf("%02d", hours), sprintf("%02d", minutes), sprintf("%02d", seconds), sep = ":"))
}

