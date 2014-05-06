# Data for LWL experiments are organized with the following folder hierarchy:
#   task_dir / gaze_dir / data_dir / [.gazedata files]

# + `task_dir` experimental task, working directory
# + `gaze_dir` folder containing all the participant subdirectories
# + `data_dir` the participant's gazedata folder

InitializeLocations <- function(task, gaze, subj) {
  # Specify the task
  task_dir <<- task
  setwd(task_dir)
  
  # Specify the name of the directory that contains the subdirectories of
  # gazedata, one subdirectory per child.
  gaze_dir <<- gaze
  
  # Assign to a variable the participant to be analyzed.
  child <<- subj
  
  # Build the path to the data.
  data_dir <<- sprintf('%s/%s/', gaze_dir, child)
  
  # Figures will go in the same directory as the data.
  figs_dir <<- data_dir
  
}




JustReduceDataMinimal <- function(trials) {
  # Align timing events
  message("Aligning trials so Target Onset occurs at 0 ms")
  trials <- AlignTrials(trials, "TargetOnset")
  
  # Extract only a subset of each trial
  from_time <- -250
  to_time <- 2200
  
  message(paste0("Excluding data before ", from_time, " ms and data after ", to_time, " ms" ))
  trials <- TimeSlice(trials, from = from_time, to = to_time)
  
  trials <- AddAOIData(trials)
  trials <- InterpolateMissingFrames(trials)
  trials <- AddAOIData(trials)
  return(trials)
}


LoadAndReduceData <- function(data_dir, ...) {
  message(paste0("Loading data in ", getwd(), "/", data_dir))
  trials <- Session(data_dir)  
  JustReduceData(trials, ...)
}




JustReduceData <- function(trials, drop = FALSE) {
  # Align timing events
  message("Aligning trials so Target Onset occurs at 0 ms")
  trials <- AlignTrials(trials, "TargetOnset")
  
  # Extract only a subset of each trial
  from_time <- -1000
  to_time <- 2500
  message(paste0("Excluding data before ", from_time, " ms and data after ", to_time, " ms" ))
  trials <- TimeSlice(trials, from = from_time, to = to_time)
  
  # If necessary, normalize the lengths of the trials so they are the same
  # number of frames long
  trials <- AlignZeroFrames(trials)
  
  trials <- AddAOIData(trials)
  
  # Calculate the percentage of missing data
  trials <- CalculateMistrackings(trials, start = from_time, end = to_time, column="GazeByImageAOI")
  avg_mistrackings <- mean(trials %@% "PercentNA")
  
  trials <- InterpolateMissingFrames(trials)
  
  # Optionally, drop trials with over 50% missing data
  if(drop) {
    trials <- CalculateMistrackings(trials, start = -1000, end = 2500)
    post_mistrackings <- trials %@% "PercentNA"
    drop <- which(.50 <= post_mistrackings)
    trials[drop] %@% "PercentNA"
    trials <- DropTrials(trials, drop)  
  }
  
  # Compute how much data was gained via interpolation and removing bad trials. 
  post_mistrackings <- trials %@% "PercentNA"
  post_avg_mistrackings <- mean(trials %@% "PercentNA")
  gain <- avg_mistrackings - post_avg_mistrackings
  gain
  
  # Update the AOI data in the session.
  trials <- AddAOIData(trials)
  return(trials)
}










RunSingleRWLAnalysis <- function(data_dir) {
  trials <- Session(data_dir)
  trials <- AlignTrials(trials, "TargetOnset")
  trials %@% "CarrierOnset"
  trials <- AddAOIData(trials)
  
  trials %@% "Protocol"
  
  # Extract the portion of the trials between CarrierOnset and AttentionEnd.
  trials <- TimeSlice(trials, "CarrierOnset", "AttentionEnd")
  
  
  # Align the sliced intervals so time 0 occurs in the same frame number.
  trials <- AlignZeroFrames(trials)
  
  
  
  
  
  # Calculate percentage of mistrackings, interpolate missing data, and calculate 
  # how much data was recovered from interpolation.
  trials <- CalculateMistrackings(trials)
  mistrackings <- trials %@% "PercentNA"
  avg_mistrackings <- mean(trials %@% "PercentNA")
  trials <- InterpolateMissingFrames(trials)
  trials <- CalculateMistrackings(trials)
  post_mistrackings <- trials %@% "PercentNA"
  post_avg_mistrackings <- mean(trials %@% "PercentNA")
  gain <- avg_mistrackings - post_avg_mistrackings
  
  # Plot the log-odds data for the Session. The first plot ignores the
  # fixation-movie area of interest and the second includes information about this
  # AOI.
  DrawLogOdds(trials, figs_dir, fixation = FALSE)
  DrawLogOdds(trials, figs_dir, fixation = TRUE)
  
  # Open up the explorer Window in the figures directory
  shell(sprintf("start %s/%s", task_dir, figs_dir))
  
  
  
}



