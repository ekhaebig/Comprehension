# source("LWL_Visualize.R")
source("LWL_Shared.R")


# Get subclass from Task attribute then dispatch using `plot_lwl`
plot.Trial <- function(trial) {
  class(trial) <- c(class(trial), trial %@% "Task")
  plot_lwl(trial)
}


#' Plot a Looking-While-Listening Trial
#' 
#' @param trial a Trial object with columns for Time and a GazeByImageAOI
#' @return a ggplot object
plot_lwl <- function(trial, ...) UseMethod("plot_lwl")


# By default, draw looks to AOIs over time
plot_lwl.default <- function(trial) {
  qplot(data = trial, x = Time, y = GazeByImageAOI)
}

# For the MP task, draw how the mean x-position
# of the gaze varies over the course of the trial
plot_lwl.MP <- function(trial) {
  # Make a title
  trial_title <- paste(trial %@% "Subject", trial %@% "TrialNo", 
                       trial %@% "TargetImage", trial %@% "TargetWord", 
                       trial %@% "StimType", sep=", ")
  # Prepare events
  trial_events <- data.frame(Events = c("Target Onset", "300ms", "Target End"), 
                             Times = c(trial %@% "TargetOnset", 
                                       trial %@% "TargetOnset" + 300, 
                                       trial %@% "TargetEnd"))
  event_lines <- function(events) {
    geom_vline(x = events$Times, alpha = .75, linetype = "dashed")
  }
  events <- function(events) {
    annotate("text", x = events$Times, y = 0, label = events$Events, 
             hjust = 0, vjust = -.5, alpha = .75, angle = 90)
  }
  
  ggplot(trial, aes(x = Time, y = XMean, color = GazeByImageAOI, 
                    group = 1, ymin = 0, ymax = 1)) + 
    labs(title = trial_title) + 
    events(trial_events) + event_lines(trial_events) + 
    geom_point() + geom_path()
}






















#### List methods general to Block, Session, and Task objects -----------------





# A function for making a histogram of the gazedata points from all trials in a
# list in order to determine likely AOIs.
ExploreAOI <- function(trial_list, toward.target = FALSE) {

  # Utility function for getting gazedata from a trial. Curried so that it can
  # be applied to a list of trials.
  getGazedata <- function(gaze) {
    lambda.trial <- function(trial) {return(trial[[gaze]])}
  }

  # Set the gazedata for which histograms should be created.
  if (toward.target) {
    x.gazedata <- 'XMeanToTarget'
    y.gazedata <- 'YMeanToTarget'
    x.label <- 'Mean X position from distractor-edge to target-edge'
  } else {
    x.gazedata <- 'XMean'
    y.gazedata <- 'YMean'
    x.label <- 'Mean X position from left to right'
  }

  # Collect all the relevant x-gazedata from trial_list.
  x.gazes <- Map(getGazedata(x.gazedata), trial_list)
  x.gazes <- Reduce(c, x.gazes)

  # Collect all the relevant y-gazedata from trial_list.
  y.gazes <- Map(getGazedata(y.gazedata), trial_list)
  y.gazes <- Reduce(c, y.gazes)

  # Create a two-panel canvas.
  par(mfcol = c(1, 2))

  # Make a histogram of x.gazes.
  hist(x.gazes, breaks = seq(0, 1, by = 0.01), 
       col = 'dodgerblue', main = '', xlab = x.label)
  
  abline(v = seq(0, 1, by = 0.1), lty = 3, lwd = 1.5, col = 'black')
  abline(v = seq(0.05, 0.95, by = 0.1), lty = 3, col = 'gray50')
  cuts <- c(c(100, 700) / 1920, c(1220, 1820) / 1920)
  abline(v = cuts, col = 'red')

  # Make a histogram of y.gazes.
  hist(y.gazes, breaks = seq(0, 1, by = 0.01), 
       col = 'dodgerblue', main = '',
       xlab = 'Mean Y position from top to bottom')
  
  abline(v = seq(0, 1, by = 0.1), lty = 3, lwd = 1.5, col = 'black')
  abline(v = seq(0.05, 0.95, by = 0.1), lty = 3, col = 'gray50')
  cuts <- c(300, 900) / 1200
  abline(v = cuts, col = 'red')
}







#### AOI.LogOddsRatio initialization and methods ------------------------------


AOI.LogOddsRatio <- function(listOfTrials, 
                             aoiDataColNames = c('GazeByImageAOI'), 
                             targetAOICodes = c("Target"), 
                             relativeToAOICodes = c("Distractor"), 
                             binWidth = 3) {
# AOILogOddsRatio is a function for computing the log odds ratio of the 
# discretized gazedata from a list of Trial objects.
# Arguments:
#         listOfTrials: A list of Trial objects that have been aligned and had
#                       discretized AOI data added.
#      aoiDataColNames: A character vector whose elements each name a column of
#                       discretized AOI data in all the Trial objects in
#                       listOfTrials.  Default is c('GazeByImageAOI').
#       targetAOICodes: A numeric vector that specifies which AOI codes should
#                       be counted as looks to target---i.e., which AOI codes
#                       should be summed as the numerator of the log odds ratio.
#                       Default is c(1), which counts only the looks to the
#                       target image in the 'AOI.Target' discretization.
#   relativeToAOICodes: A numeric vector that specifies which AOI codes should
#                       be counted as looks to "distractor," or perhaps more
#                       correctly, looks away from target---i.e., which AOI
#                       codes should be summed as the denominator of the log
#                       odds ratio.  Default is c(0), which counts
#                       all looks to the non-target AOI.
#             binWidth: An integer specifying the number of adjacent frames that
#                       should be binned and used to compute the log odds ratio
#                       of the discretized data.
# Returns:
#   The log odds ratio function of each type of discretized AOI data specified
#   by aoiDataColNames.  The value of the log odds ratio function for each
#   time point is found by (1) binning the adjacent time points in each trial
#   in a way such that the bins do not overlap; (2) summing all the looks to
#   a "target" AOI in each bin across all trials in listOfTrials, call this
#   target.sum; (3) summing all the looks to a "distractor" AOI in each bin 
#   across all trials, call this distract.sum; (4) correcting the zero values of
#   target.sum and distract.sum; (5) computing 
#   log(target.sum / distract.sum).
#   The times at which each log odds ratio function is defined are found by
#   taking the mean time value of each bin.
#   The times and values of the log odds ratio function(s) are collected into
#   a data.frame.  The first column of the data.frame is the times, each other
#   column is a log odds ratio function.

  
  # Make a function that makes partitioning functions
  MakePartitioner <- function(trials, field_name) {
    trial_classes <- class(trials)
    function(value) {
      structure(trials[which(trials %@% field_name == value)], 
                class = trial_classes)
    }
  }
  
  # Partition the set of trials by stimulus type
  stim_types <- unique(listOfTrials %@% "StimType")
  PartitionByStimType <- MakePartitioner(listOfTrials, "StimType")
  trials_by_stim_type <- lapply(stim_types, PartitionByStimType)
  names(trials_by_stim_type) <- stim_types
  

  # Make a function that returns a column of logodds values
  ComputeLogOdds <- function(trials) {
    # Extract the columns in aoiDataColNames from each Trial
    aoi_data <- Map(.ExtractDataFrameCols(aoiDataColNames), trials)  
    aoi_codes <- targetAOICodes
    
    GetTotalLooksAndBin <- function(aoi_codes) {
      # Transform the values of each element in aoi_data from the discrete
      # AOI values to either 1 or 0 according to whether the value at that frame
      # counts as a look to one of the AOI codes in aoi_codes or not, 
      # respectively.
      list.of.mapped.aoi <- Map(.FindLooksToAOICodes(aoi_codes), aoi_data)
      
      # Reduce the list of each trial's AOI data mapped to looks to target, to the
      # the total number of looks to target for each time point, by summing the
      # elements of list.of.mapped. aoi.
      total.looks <- Reduce(`+`, list.of.mapped.aoi)
      
      return(.BinAndSumValues(total.looks, binWidth=binWidth))
    }
    
    binned.looks.to.target <- GetTotalLooksAndBin(targetAOICodes)
    binned.looks.to.foils <- GetTotalLooksAndBin(relativeToAOICodes)
    
    # Continuity correction: Add .5 so there are no zeroes.
    binned.looks.to.target <- binned.looks.to.target + .5
    binned.looks.to.foils <- binned.looks.to.foils + .5
    
    # Compute the log odds ratio functions.
    log.odds <- log(binned.looks.to.target / binned.looks.to.foils)
    names(log.odds) <- "LogOdds"
    
    return(log.odds)
  }
  
  # Compute logodds on each partition and assemble dataframe
  logodds <- lapply(trials_by_stim_type, ComputeLogOdds)
  logodds_frame <- as.data.frame(logodds)
  names(logodds_frame) <- names(logodds)
  
  # Add the overall logodds
  logodds_frame <- cbind(ComputeLogOdds(listOfTrials), logodds_frame)
  

  #### Add a timing column
  # Find the average time value for each point in the log odds ratio functions:
  # First, extract the 'Time' column from the first Trial object in
  # listOfTrials.  Each element of listOfTrials should have the same values in
  # their respective 'Time' column, so taking the first element of listOfTrials
  # is fine.
  time.values <- .ExtractDataFrameCols('Time')(listOfTrials[[1]])
  
  # Second, bin and sum the time.values.
  binned.time.values <- .BinAndSumValues(time.values, binWidth=binWidth)
  # Lastly, divide each element of binned.time.values by the binWidth to
  # get the average time value for that bin.
  time <- binned.time.values / binWidth
  
  log_odds_ratios <- cbind(time, logodds_frame)
  
  
  
  #### Add sensible attributes to the data-frame of log-odds ratio. 
  # These shortcut functions take attributes from `listOfTrials` and attach them
  # onto the log-odds data-frame. They are used just to make the code more
  # readable.
  AddUniqueAttribute <- function(t_attr) {
    log_odds_ratios %@% t_attr <- unique(listOfTrials %@% t_attr)
    return(log_odds_ratios)
  }
  AddMeanAttribute <- function(t_attr) {
    log_odds_ratios %@% t_attr <- mean(listOfTrials %@% t_attr)
    return(log_odds_ratios)
  }
  
  log_odds_ratios <- AddUniqueAttribute("Task")
  log_odds_ratios <- AddUniqueAttribute("Subject")
  log_odds_ratios <- AddUniqueAttribute("StimType")
  log_odds_ratios <- AddUniqueAttribute("TargetWord")
  log_odds_ratios <- AddUniqueAttribute("AlignedBy")
  
  log_odds_ratios <- AddMeanAttribute("ImageOnset")
  log_odds_ratios <- AddMeanAttribute("CarrierOnset")
  log_odds_ratios <- AddMeanAttribute("CarrierEnd")
  log_odds_ratios <- AddMeanAttribute("TargetOnset")
  log_odds_ratios <- AddMeanAttribute("TargetEnd")
  log_odds_ratios <- AddMeanAttribute("AttentionOnset")
  log_odds_ratios <- AddMeanAttribute("AttentionEnd")
  log_odds_ratios <- AddMeanAttribute("FixationOnset")
    
  class(log_odds_ratios) <- c('AOI.LogOdds', 'data.frame')
  
  # Return the data.frame of log odds ratio functions.
  return(log_odds_ratios)
}










## Functions for plotting AOI.LogOdds data

plot_closure.AOI.LogOdds <- function(f) {
  # plot_closure.AOI.LogOddsis a utility function to generate functions for 
  # plotting AOI.LogOdds data. The lines, points, and plot functions take the
  # same arguments and handle those arguments similarly, so the closure function
  # generates the .AOI.LogOdds versions of those functions and keeps us from
  # having to have create or modify 3 different functions.
  function(log.odds, aoi.data = 'AOI.Target', from = NULL, to = NULL, ...) {
    
    if (is.null(from)) {
      from <- log.odds$Time[1]
    } else if (class(from) == 'function') {
      from <- from(log.odds)
    } else if (class(from) == 'character') {
      from <- log.odds %@% from
    }
    
    if (is.null(to)) {
      to <- log.odds$Time[dim(log.odds)[1]]
    } else if (class(to) == 'function') {
      to <- to(log.odds)
    } else if (class(to) == 'character') {
      to <- log.odds %@% to
    }
    
    start.ind <- length(which(log.odds$Time <= from))
    end.ind <- length(which(log.odds$Time <= to))
    
    f(log.odds$Time[start.ind:end.ind], 
      log.odds[[aoi.data]][start.ind:end.ind], ...)
  }
}


plot.AOI.LogOdds <- plot_closure.AOI.LogOdds(plot)
lines.AOI.LogOdds <- plot_closure.AOI.LogOdds(lines)
points.AOI.LogOdds <- plot_closure.AOI.LogOdds(points)






#### Utility functions --------------------------------------------------------

is.substring <- function(pattern, string) length(grep(pattern, string)) > 0 
