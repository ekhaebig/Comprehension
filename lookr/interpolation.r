## Functions to interpolate missing data in a Trial or list of Trials

InterpolateMissingFrames <- function(...) {
  UseMethod('InterpolateMissingFrames')
}

# Version of the function for lists of Trials
InterpolateMissingFrames.list <- function(listOfTrials) {
  # Preserve classes of the inputted list
  classes <- class(listOfTrials)
  
  # Call the Trial version of the function on each of the trials.
  listOfTrials <- Map(InterpolateMissingFrames, listOfTrials)
  
  # Reinstate classes on the list
  class(listOfTrials) <- classes
  listOfTrials
}



InterpolateMissingFrames.Trial <- function(trial, window = lwl_opts$get("interpolation_window")) {
  # Convert the window duration (ms) into the number of frames in that window,
  # rounded down.
  frames_in_window <- floor(window / lwl_constants$ms_per_frame)
  
  # Extract the gazes from the trial. Record how many missing frames there are.
  gazes <- trial$GazeByImageAOI
  missing <- sum(is.na(gazes))
  
  # Grab all the non-NA gaze frames. 
  tracked <- which(!is.na(gazes))
  
  # For each non-NA gaze frame, subtract the index of the previous frame from it. 
  # The difference allows us to find out how many NA frames were skipped when we 
  # extracted all the non-NA frames.
  differences <- tracked
  for (frame in seq(from = length(differences), to = 1)){
    if (1 < frame) {
      differences[frame] <- (differences[frame] - differences[frame - 1])
    }
  }
  
  ## Find the precise starts and ends of each gap
  
  # Starts are not accurate because they don't take into account other missing 
  # frames. Use the cumulative sum of missing frames to correct the start 
  # locations.
  gap_start <- which(1 < differences)
  gap_size <- differences[gap_start] - 1
  gap_sizes <- cumsum(gap_size)
  
  # Correction of gap locations
  for (gap in seq_along(gap_start)){
    if (gap == 1){
      next
    }  
    gap_start[gap] <- gap_start[gap] + gap_sizes[gap - 1]
  }
  
  # Final adjustments so gap_start and gap_end are indices are non-NA values.
  gap_start <- gap_start - 1
  gap_end <- gap_start + gap_size + 1
  
  # Exclude gaps larger than frames_in_window frames from consideration
  correctable <- which(gap_size <= frames_in_window)
  
  if (length(correctable) == 0) {
    trial %@% "InterpolatedPoints" <- 0
    trial %@% "CorrectedFrames" <- NA
    return(trial)
  }
  
  gap_start <- gap_start[correctable]
  gap_end <- gap_end[correctable]
  gap_size <- gap_size[correctable]
  
  # Exclude the very first gap if it includes the very first frame
  if (gap_start[1] == 0){
    gap_start <- gap_start[-1]
    gap_end <- gap_end[-1]
    gap_size <- gap_size[-1]  
  }
  
  # Get the values before and after each NA gap
  start_values <- gazes[gap_start]
  end_values <- gazes[gap_end]
  
  corrected_frames <- NULL
  
  # If the values before and after the gaps match, interpolate
  for (point in seq_along(gap_start)){
    if (start_values[point] == end_values[point]){
      value <- start_values[point]
      gazes[gap_start[point]:gap_end[point]] <- value
      
      columns <- c("XLeft", "XRight", "XMean", "YLeft", "YRight", "YMean", 
                   "XLeftToTarget", "XRightToTarget", "XMeanToTarget", 
                   "YLeftToTarget", "YRightToTarget", "YMeanToTarget")
      
      means <- sapply(trial[gap_start[point]:gap_end[point], columns], 
                      mean, na.rm = TRUE)
      
      na_start <- gap_start[point] + 1
      na_end <- gap_end[point] - 1
      
      corrected_frames <- c(corrected_frames, na_start:na_end)
      
      for (col in columns) {
        trial[na_start:na_end, col] <- means[col]
      }
    }
  }
  
  # Update the trial
  trial %@% "CorrectedFrames" <- corrected_frames
  trial %@% "InterpolatedPoints" <- missing - sum(is.na(gazes))
  
  # Update the AOI Data
  AddAOIData(trial)
}
