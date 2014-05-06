
#### Latency -----------------------------------------------------------------

#' Calculate the latency
#' 
#' A latency is a basically an arrow of time with an origin and endpoint. A 
#' latency is valid if it has a valid origin and a valid endpoint. The endpoint 
#' is when the subject first looks to the target image. Without a look to the 
#' target, the endpoint is invalid.
#' 
#' It's not guaranteed that the subject's gaze will be tracked at a particular
#' frame of time, so we define the origin of the latency over an onset window (a
#' span of time a few frames in length). For the origin to be valid, the subject
#' must be looking onscreen during some part of the onset window and they cannot
#' be looking at the target image during this window.
#' 
#' @param trial a Trial object with a GazeByImageAOI column
#' @param trials a list of Trial objects, each with a GazeByImageAOI columns
#' @param onset_window an OnsetWindow object
#' @param target_name the name of the target image, as its stored in the 
#'   GazeByImageAOI column of the Trial
#' @return the Trial object or list of Trial objects with an added Latency 
#'   attribute
CalculateLatency <- function(...) UseMethod("CalculateLatency")

CalculateLatency.list <- function(trials, onset_window, target_name) {
  LambdaTrial <- function(trial) CalculateLatency(trial, onset_window, target_name)
  lapply(trials, LambdaTrial)
}

CalculateLatency.Trial <- function(trial, onset_window, target_name) {
  onset_time <- CheckLatencyOnset(trial, onset_window, target_name)
  first_look <- FindFirstLook(trial, onset_window, target_name)
  onset_gaze <- FindGazeAtOnset(trial, onset_window, target_name)
  
  # The response latency is the time between the onset and the first look to
  # target. A latency calculation therefore requires a valid onset and a valid
  # first look to target.
  if (is.na(onset_time) | is.na(first_look)) {
    latency <- NA
  } else {
    latency <- first_look - onset_time
  }
  
  trial %@% "Latency" <- Latency(latency, onset_time, first_look, onset_gaze, 
                                 target_name)
  trial
}


#' Make a latency calculator
#' 
#' Higher-order function that can set the onset_window and target_name 
#' parameters for the CalculateLatency function. Useful for making a separate 
#' functions to handle looks to target and look to distractor.
#' 
#' @inheritParams CalculateLatency
#' @return a partially applied version of the CalculateLatency function with the
#'   onset_window and target_name arguments already defined
LatencyCalculator <- function(onset_window, target_name) {
  function(trial) {
    CalculateLatency(trial, onset_window, target_name)
  }
}


#' Make a Latency object
#' 
#' A Latency object is list of all the relevant details about a latency 
#' calculation. This function merely binds those details (see the parameters) 
#' together into a Latency object. 
#' 
#' @param latency how long it took to look to the target image
#' @param onset_time the time of the first tracked look in the onset window
#' @param first_look the time of the first look to target
#' @param onset_gaze where the subject was looking at onset_time
#' @param target the name of the image designated as the target for the latency
#'   calculation
#' @return a Latency object (a list with the class Latency)
Latency <- function(latency, onset_time, first_look, onset_gaze, target) {
    structure(
      list(Latency = latency, 
           OnsetTime = onset_time, 
           FirstLook = first_look, 
           OnsetGaze = onset_gaze,
           Target = target), 
      class = c("Latency", "list")
    )
}


#' Extract a data-frame of latency information
#' 
#' @param trials a list of Trial objects each with a "Latency" attribute 
#'   containing information from a latency calculation.
#' @return a data-frame with the Latency infromation plus Subject and TrialNo
#'   fields
SummarizeLatencies <- function(trials) {
  # Reduce the list of (Latency) lists into a data-frame
  latencies_list <- lapply(trials, attr, "Latency")
  latencies_frame <- do.call(rbind.data.frame, latencies_list)
  latencies <- cbind(Subject = trials %@% "Subject", 
                     TrialNo = trials %@% "TrialNo", 
                     latencies_frame)
  
  # Convert factors to strings
  latencies$Subject <- as.character(latencies$Subject)
  latencies$OnsetGaze <- as.character(latencies$OnsetGaze)
  latencies$Target <- as.character(latencies$Target)
  latencies
}


#' Is the onset window valid?
#' 
#' @inheritParams CalculateLatency
#' @return Either the time of the first tracked gaze in the onset window or NA
#'   (if the subject looks at nothing or to the target image during the onset
#'   window)
CheckLatencyOnset <- function(trial, onset_window, target_name) {
  # An invalid onset contains a look to the target or contains all NA values. 
  window <- GetLooksWithinWindow(trial, onset_window)
  if (window$GazeByImageAOI %contains% target_name) {
    onset_time <- NA
  } else if (is_all_na(window$GazeByImageAOI)) {
    onset_time <- NA
  } else {
    valid_looks <- window[!is.na(window$GazeByImageAOI), ]
    onset_time <- valid_looks[1, "Time"]
  }
  onset_time  
}


#' When did the subject first look to the target?
#' 
#' @inheritParams CalculateLatency
#' @return Either the time of the first tracked look to the target image after
#'   the onset window or NA (if the subject never looks to the target)
FindFirstLook <- function(trial, onset_window, target_name) {
  # If there are no looks to target, there can be no first look to target.
  window <- GetLooksAfterWindow(trial, onset_window)
  if (window$GazeByImageAOI %lacks% target_name) {
    first_look <- NA
  } else {
    valid_looks <- window[window$GazeByImageAOI == target_name, ]
    first_look <- valid_looks[1, "Time"]
  }
  first_look  
}


#' What is the subject looking at during the onset window?
#' 
#' @inheritParams CalculateLatency
#' @return Either where the subject is looking during the first tracked gaze in
#'   the onset window or NA (if the subject looks at nothing during the onset
#'   window)
FindGazeAtOnset <- function(trial, onset_window, target_name) {
  # An invalid onset contains a look to the target or contains all NA values. 
  window <- GetLooksWithinWindow(trial, onset_window)
  if (is_all_na(window$GazeByImageAOI)) {
    gaze_location <- NA
  } else {
    valid_looks <- window[!is.na(window$GazeByImageAOI), ]
    gaze_location <- valid_looks[1, "GazeByImageAOI"]
  }
  gaze_location  
}




#### Accuracy -----------------------------------------------------------------

#' Compute the accuracy of looking to target over a window
#' 
#' @inheritParams CalculateLatency
#' @return the Trial object or list of Trial objects with an added Accuracy
#'   attribute
ComputeAccuracy <- function(...) UseMethod("ComputeAccuracy")

ComputeAccuracy.list <- function(trials, window, target_name) {
  LambdaTrial <- function(trial) ComputeAccuracy(trial, window, target_name)
  lapply(trials, LambdaTrial)
}

ComputeAccuracy.Trial <- function(trial, window, target_name) {
  # Get the looks in the window
  times_to_get <- start(window) <= trial$Time & trial$Time <= offset(window)
  looks <- trial[times_to_get, "GazeByImageAOI"]
  tracked_looks <- Filter(Negate(is.na), looks)
  
  # Count looks to target, transitional looks and total tracked frames
  to_target <- sum(tracked_looks == target_name)
  tracked <- sum(tracked_looks == "tracked")
  total_looks <- length(tracked_looks)
  trial %@% "Accuracy" <- Accuracy(to_target, total_looks, target_name, tracked)
  trial
}


#' Make an Accuracy object
#' 
#' An Accuracy object is list of all the relevant details about an accuracy 
#' calculation. This function merely binds those details (see the parameters) 
#' together into a single object.
#' 
#' @param target_looks number of frames looking to target over the window
#' @param total_looks number of frames with tracked looks over the window
#' @param target_name the name of the image designated as the target
#' @param tracked_looks number of tracked looks that do not fall inside one of 
#'   the defined areas of interest. These looks are coded as "tracked" in the 
#'   GazeByImageAOI column in a trial. These looks are called "Transitions" in
#'   the Accuracy object.
#' @return an Accuracy object (a list with the class Accuracy)
Accuracy <- function(target_looks, total_looks, target_name, tracked_looks) {
  structure(
    list(TargetLooks = target_looks,
         TotalLooks = total_looks,
         Target = target_name,
         Transitions = tracked_looks), 
    class = c("Accuracy", "list")
  )
} 


#' Extract a data-frame of accuracy information
#' 
#' @param trials a list of Trial objects each with an "Accuracy" attribute 
#'   containing information from a accuracy calculation.
#' @return a data-frame with the Accuracy infromation plus Subject and TrialNo
#'   fields
SummarizeAccuracies <- function(trials) {
  # Reduce the list of (Latency) lists into a data-frame
  accuracies_list <- lapply(trials, attr, "Accuracy")
  accuracies_frame <- do.call(rbind.data.frame, accuracies_list)
  accuracies <- cbind(Subject = trials %@% "Subject", 
                      TrialNo = trials %@% "TrialNo", 
                      accuracies_frame)
  
  # Convert factors to strings
  accuracies$Subject <- as.character(accuracies$Subject)
  accuracies$Target <- as.character(accuracies$Target)
  accuracies
}




#### Helper functions ---------------------------------------------------------

#' Define the Onset Window for a Latency calculation
#' 
#' In a latency calculation, we need a starting point. Since a single frame of
#' eyetracking data may be mistracked, we allow ourselves a window of time for
#' the starting point. This is the onset window. It typically (by default) is
#' four frames or 50 ms.
#' 
#' @param starting_time start time of the window
#' @param window_duration length of the window
#' @return an OnsetWindow (list) object containing the parameters
OnsetWindow <- function(starting_time = 0, window_duration = 50) {
  structure(
    list(Start = starting_time, 
         Duration = window_duration), 
    class = c("OnsetWindow", "list")
  )
}

#' Accessors for OnsetWindow objects
start <- function(...) UseMethod("start")
start.OnsetWindow <- function(window) window$Start

offset <- function(...) UseMethod("offset")
offset.OnsetWindow <- function(window) window$Start + window$Duration


#' Extract the looks inside an onset window
#' 
#' @param trial A Trial object
#' @param an OnsetWindow object
#' @return a Trial object with just the data from the onset window
GetLooksWithinWindow <- function(trial, window) {
  # start <- window$Start
  # end <- window$Start + window$Duration
  trial[start(window) <= trial$Time & trial$Time <= offset(window), ]
}

#' Extract the looks after an onset window
#' 
#' @inheritParams GetLooksWithinWindow
#' @return a Trial object with just the data from after the onset window
GetLooksAfterWindow <- function(trial, window) {
  # start <- window$Start + window$Duration
  # end <- max(trial$Time)
  trial[offset(window) < trial$Time & trial$Time <= max(trial$Time), ]
}

`%contains%` <- function(x, y) any(y %in% x)
`%lacks%` <- function(x, y) !any(y %in% x)
is_all_na <- function(x) all(is.na(x))









