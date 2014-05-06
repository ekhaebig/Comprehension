# This file collects functions used by both tasks. 

library(plyr)
library(reshape2)
library(lubridate, warn.conflicts = FALSE)
library(tools)
library(stringr)
library(ggplot2)
library(grid)
library(httr)

# Load the set-in-stone versions of functions from github
source("lookr/github.r")

# Load the not-quite-ready-for-primetime directly
source("lookr/strings.R")
source("lookr/firstclass.R")
source("lookr/shortcuts.R")
source("lookr/aoi_log_odds.r")
source("lookr/interpolation.r")
source("lookr/task.r")
source("lookr/latency.r")















#' Calculate the amount of missing data in a list of Trials
#' 
#' `start` and `end` can use the following strings to pull time attributes such
#' as `ImageOnset`, `CarrierOnset`, `CarrierEnd`, `TargetOnset`, `TargetEnd`,
#' `AttentionOnset`, and `AttentionEnd`.
#' 
#' @param trials A list of Trial objects.
#' @param start A time value or the name of a time attribute of a Trial. Default
#'   is to use the whole trial.
#' @param end A time value or the name of a time attribute of a Trial. Default 
#'   is to use the whole trial.
#' @param column A character string that names one of the columns of Gazedata in
#'   the trial object. Default is `"GazeByImageAOI"`.
#' @return A list of Trial objects that has been updated to include attributes containing percentage of mistracked data.
CalculateMistrackings <- function(trials, start = NULL, end = NULL, column = "GazeByImageAOI") {
  # Preserve classes of the list of trials
  classes <- class(trials)
  # A curried version of the function that operates on individual Trials.
  LambdaTrial <- function(trial) {
    # If no start/end time is given, use the start/end frame. If start/end are
    # the names of time attributes, use those attributes.
    if (is.null(start)) {
      start <- trial$Time[1]
    } else if (is.character(start)) {
      start <- trial %@% start
    }
    
    if (is.null(end)) {
      end <- trial$Time[nrow(trial)]
    } else if (is.character(end)) {
      end <- trial %@% end
    }
    
    # Get the indices of the time frames closest to the start and end times
    start_frame <- length(which(trial$Time <= start))
    end_frame <- length(which(trial$Time <= end))
    frames <- seq(start_frame, end_frame)
    num_frames <- length(frames)
    
    # Compute the number of NA frames
    mistrackings <- sum(is.na(trial[frames, column]))
    trial %@% "MistrackedFrames" <- mistrackings
    trial %@% "NumberOfFrames" <- num_frames
    trial %@% "PercentNA" <- mistrackings / num_frames  
    trial
  } 
  
  # Calculate mistrackings for each individual trial.
  trials <- lapply(trials, LambdaTrial)
  class(trials) <- classes
  trials
}




