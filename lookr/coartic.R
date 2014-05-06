
# Utility function for getting gazedata from a trial. Curried so that it can
# be applied to a list of trials.
.MakeGazeGrabber <- function(column) {
  function(trial) trial[[column]]
}


# A function for making a histogram of the gazedata points from all trials in a
# list in order to determine likely AOIs.
ExploreAOIAndSave <- function(trials, figs_dir, toward_target = FALSE) {
  # Set the gazedata for which histograms should be created.
  if (toward_target) {
    x_gazedata <- 'XMean.Target'
    y_gazedata <- 'YMean.Target'
    x_label <- 'Mean X position from distractor-edge to target-edge'
  } else {
    x_gazedata <- 'XMean'
    y_gazedata <- 'YMean'
    x_label <- 'Mean X position from left to right'
  }
  
  # Collect all the relevant x-gazedata
  x_gazes <- Map(.MakeGazeGrabber(x_gazedata), trials)
  x_gazes <- Reduce(c, x_gazes)
  
  # Collect all the relevant y-gazedata
  y_gazes <- Map(.MakeGazeGrabber(y_gazedata), trials)
  y_gazes <- Reduce(c, y_gazes)
  
  # Combine the gazedata into a long dataframe
  gazes <- data.frame(X = x_gazes, Y = y_gazes)
  gazes <- melt(gazes, value.name = "Value", variable.name = "Gaze")

  # Determine where to draw reference lines on the histogram according to the
  # AOI boundaries
  x_cuts <- c(c(100, 700) / 1920, c(1220, 1820) / 1920)
  y_cuts <- c(300, 900) / 1200
  cuts <- melt(list(X = x_cuts, Y = y_cuts), level = 1)
  cuts <- rev(cuts)
  names(cuts) <- c("Gaze", "Cut")
  
  # Make a function to label the facets of the histogram
  labeller <- function(variable, value) {
    from_to <- ifelse(value == "X", "from left to right", "from top to bottom")
    paste0("Mean ", value, " position ", from_to)
  }
  
  # Plot the gazedata distributed over screen proportions
  p <- ggplot(gazes, aes(x = Value)) + geom_histogram(binwidth = .01) + 
    facet_grid(. ~ Gaze, labeller = labeller)
  
  # Draw reference lines
  p <- p + geom_vline(aes(xintercept = Cut, color = "red"), data = cuts) 
  ggplot2::`%+%`
  # Clean up the plot
  p <- p + theme_bw() + labs(x = "Gaze location (proportion of screen", 
                             y = "Frequency")
  
  # Save plot
  plot_file <- sprintf('%scounts_%s.png', figs_dir, "xy_histograms")
  ggsave(plot = p, file = plot_file, height = 8.5, width = 11, units = "in")
}





ComputeLogOddsByStimType <- function(trials) {
  ## Partition the trials by StimType, determine the log-odds of looking to target
  ## within each trial type, and combine the log-odds information into a long
  ## dataframe.
  
  # SPLIT
  # Make an apply-able function that grabs trials based on StimType
  GetTrialsWithStimType <- MakeTrialGrabber("StimType", reversed = TRUE)
  
  # Partition the list of trials by StimType
  stimtypes <- unique(trials %@% 'StimType')
  stimtype_trials <- llply(stimtypes, GetTrialsWithStimType, trials)
  
  # APPLY
  # Compute the log-odds of looking to target in each StimType condition
  stimtype_odds <- llply(stimtype_trials, AOI.LogOddsRatio, binWidth = 3)
  names(stimtype_odds) <- stimtypes
  
  # COMBINE
  # Combine the separate sets of log-odds into a single "long" dataframe of odds
  stim_odds <- melt(stimtype_odds, id.vars = c("Time", "GazeByImageAOI"), level = 1)
  names(stim_odds) <- c("Time", "GazeByImageAOI", "StimType")
  
  # Adjust timing properties
  stim_odds %@% "CarrierOnset" <- mean(trials %@% "CarrierOnset")
  stim_odds %@% "TargetOnset" <- mean(trials %@% "TargetOnset")
  stim_odds %@% "TheStart" <- mean(trials %@% "TheStart")
  stim_odds %@% "TargetStart" <- mean(trials %@% "TargetStart") 
  stim_odds %@% "TargetEnd" <- mean(trials %@% "TargetOnset") + mean(trials %@% "TargetDur")
  
  stim_odds %@% "Subject" <- child
  stim_odds %@% "NumTrials"  <- length(trials)
  stim_odds %@% "PercentNA"  <- round(mean(trials %@% "PercentNA") * 100, 2)
  
  return(stim_odds)
}






PlotLongLogOdds <- function(dframe, figs_dir = "N:/CoArticulation/CollectedData/plots/") {
  # Map data
  p <- ggplot(data = dframe, aes(Time, GazeByImageAOI, color = StimType))
  
  # Draw vertical time reference lines
  x_intercepts <- c(dframe %@% "CarrierOnset", 
                    dframe %@% "TheStart", 
                    dframe %@% "TargetStart", 
                    dframe %@% "TargetEnd")
  p <- p + geom_vline(xintercept = x_intercepts, colour = "black", 
                      linetype = "longdash")
  p <- p + geom_hline(yintercept = 0, colour = "gray")
  
  # Label timing lines
  y_rng <- range(dframe$GazeByImageAOI)
  line_labels <- c("Carrier Onset", 
                   "the", 
                   "Target Onset", 
                   "Target End")
  p <- p + annotate("text", x = x_intercepts + 25, y = y_rng[1], hjust = 0, 
                    angle = 90, label = line_labels)
  
  # Build plot title
  plot_title <- paste0(dframe %@% "Subject", 
                       " in ", dframe %@% "NumTrials", " trials with ",
                       dframe %@% "PercentNA", "% data missing")
  
  # Create basic scatterplot with lines
  p <- p + geom_path() + geom_point() + theme_bw() + 
    labs(x = "Time (3-frame smoothing)", 
         y = "Log-Odds of Looking to Target Image", title = plot_title)
  
  # Finish theming, plot and save.
  p <- p + theme(panel.grid.major = element_line(colour = "white"))
  print(p)
  plot_file <- sprintf('%sLogOdds_%s_fixed.png', figs_dir, child)
  ggsave(plot = p, file = plot_file, height = 8.5, width = 11, units = "in")
}









WidenTrial <- function(trial, column) {
  trial$Condition <- trial %@% "StimType"
  trial$TrialNo <- trial %@% "TrialNo"
  trial$WordGroup <- trial %@% "WordGroup"
  trial$TargetWord <- trial %@% "TargetWord"
  trial$TargetSound <- str_extract(trial$TargetWord, "^[db]{1}")
  melt(trial, id.vars = c("TrialNo", "Time", "Condition", "WordGroup", 
                          "TargetWord", "TargetSound", column), 
       measure.vars = NULL)
}


MakeLooks <- function(trials, child) {
  # Make a big dataframe of each trial's looks to each AOI
  long_session <-ldply(trials, WidenTrial, "GazeByImageAOI")
  # Don't worry about looks to the fixation-movie AOI
  long_session$GazeByImageAOI <- ifelse(long_session$GazeByImageAOI == -1, NA, 
                                    long_session$GazeByImageAOI)
  # Count the number of looks to each AOI
  looks <- dcast(long_session, formula = Condition + Time ~ GazeByImageAOI, 
                 fun.aggregate = length, value.var = "GazeByImageAOI")
  names(looks) <- c("Condition", "Time", "Zeroes", "Ones", "NAs") 
  
  # Count the number of trials. Ignore NA looks. Compute proportions.
  looks$Trials <- looks$Zeroes + looks$Ones + looks$NAs
  looks <- transform(looks, Usable = Trials - NAs)
  looks <- transform(looks, Props = Ones / Usable)
  
  # Compute event times
  looks %@% "CarrierOnset" <- mean(trials %@% "CarrierOnset")
  looks %@% "TargetOnset" <- mean(trials %@% "TargetOnset")
  looks %@% "TargetStart" <- mean(trials %@% "TargetStart")
  looks %@% "TheStart" <- mean(trials %@% "TheStart")
  looks %@% "TargetEnd" <- mean(trials %@% "TargetOnset") + 
    mean(trials %@% "TargetDur")
  # Attach other useful attributes
  looks %@% "Subject" <- child
  looks %@% "NumTrials"  <- length(trials)
  looks %@% "PercentNA"  <- round(mean(trials %@% "PercentNA") * 100, 2)
  return(looks)
}



















CountLooks <- function(trials, child, target_code = "TargetImage") {
  # Make a big dataframe of each trial's looks to each AOI
  long_session <-ldply(trials, WidenTrial, "GazeByImageAOI")
  
  # Count the number of looks to each AOI
  looks <- dcast(long_session, formula = Condition + Time ~ GazeByImageAOI)
  
  # Count the number of trials
  looks$Trials <- rowSums(looks[-c(1, 2)])
  
  # Compute the proportion of trials that look to the target at that time
  looks$Props <- looks[[target_code]] / looks$Trials
  
  # Compute event times
  looks %@% "CarrierOnset" <- mean(trials %@% "CarrierOnset")
  looks %@% "TargetOnset" <- mean(trials %@% "TargetOnset")
  looks %@% "TargetStart" <- mean(trials %@% "TargetOnset") + 470
  looks %@% "TargetEnd" <- mean(trials %@% "TargetOnset") + 1270
  
  # Attach other useful attributes
  looks %@% "Subject" <- child
  looks %@% "NumTrials"  <- length(trials)
  looks %@% "PercentNA"  <- round(mean(trials %@% "PercentNA") * 100, 2)
  return(looks)
}

















PlotCoarticProportions <- 
  function(looks, figs_dir = "N:/CoArticulation/CollectedData/plots/") {
  
    # Generate the basic plot template.
  p <- qplot(data = looks, x = Time, y = Props, color = Condition, 
             geom = "blank", ylab = "Proportion of target looking") +
  
    # Mark y = .50 with a reference line. Use black and white theme.
    geom_hline(yintercept = .5, linetype = "solid") + theme_bw() + 
  
    # Update the x-axis
    scale_x_continuous("Time from target word onset (ms)", 
                       breaks = c(-594, 0, 1000, 2000),
                       labels = expression(italic(the), "0", "1000", "2000"),
                       limits = c(-1000, 2500)) +
    # Remove the grid lines. 
    theme(panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank()) +
    opts(legend.position = 'bottom')
    
  
  #geom_vline(xintercept = -594, linetype = "longdash")
  

  
#   # Add timing lines.
#   x_intercepts <- c(looks %@% "CarrierOnset", looks %@% "TheStart", 
#                     looks %@% "TargetStart",  looks %@% "TargetEnd")
#   p <- last_plot() + geom_vline(xintercept = x_intercepts, colour = "black", 
#                                 linetype = "longdash")
#   
#   # Label timing lines.
#   y_rng <- range(looks$Props, na.rm = TRUE)
#   line_labels <- c("Carrier Onset", "the", "Target Onset", "Target End")
#   
#   p <- last_plot() + annotate("text", x = x_intercepts + 25, y = y_rng[1], 
#                               hjust = 0, angle = 90, label = line_labels)
#   
  
  # If there is a 3 digit identifier in the subject id field, extract it.
  # Otherwise, use the field's raw value.
  id <- looks %@% "Subject"
  digits <- str_extract(id, "^[0-9]{3}")
  id <- ifelse(is.na(digits), id, paste0("ID", digits))
  
  
#   # Add a title to complete the template.
#   plot_title <- paste0(id, " (", looks %@% "NumTrials", " trials, ", 
#                        looks %@% "PercentNA", "% data missing)")
#   p <- last_plot() + labs(x = "Time", y = "Proportion of looks to target",
#                           title = plot_title)

  
  
  # Plots for all the conditions
  raw_p <- p + geom_point() + geom_line() + facet_grid(Condition ~ .)
  
  
  # Update y breaks
  p <- p + scale_y_continuous(breaks=seq(from=0, to=1.0, by = .1))
  
  smooth_p <- p + geom_smooth(aes(color = Condition))
  
  # Remove the two weirdo conditions
  looks_simple <- subset(looks, Condition != "filler" & Condition != "neutral")
  
  raw_p_simple <- (p + aes(color = Condition) + geom_point() + geom_line()) %+% looks_simple
  # raw_p_shape <- (p + aes(shape = Condition, color = Condition) + geom_point()) %+% looks_simple
  smooth_p_simple <- smooth_p %+% looks_simple
  
  
  
  # Finish theming, plot and save.
  
  raw_4 <- sprintf('%s%s_4_raw_props_f.png', figs_dir, id)
  raw_2 <- sprintf('%s%s_2_raw_props_f.png', figs_dir, id)
  smooth_4 <- sprintf('%s%s_4_smooth_props_f.png', figs_dir, id)
  smooth_2 <- sprintf('%s%s_2_smooth_props_f.png', figs_dir, id)
  
  ggsave(plot = raw_p, file = raw_4, height = 5, width = 6, units = "in")
  ggsave(plot = raw_p_simple, file = raw_2, height = 5, width = 6, units = "in")
  ggsave(plot = smooth_p, file = smooth_4, height = 5, width = 6, units = "in")
  ggsave(plot = smooth_p_simple, file = smooth_2, height = 5, width = 6, units = "in")
}





PlotProportions <- function(looks, figs_dir = "N:/CoArticulation/CollectedData/plots/") {
  # Generate the basic plot template.
  p <- qplot(data = looks, x = Time, y = Props, geom = "blank", 
             color = Condition) + theme_bw()
  
  # Add timing lines.
  x_intercepts <- c(looks %@% "CarrierOnset", looks %@% "TheStart", 
                    looks %@% "TargetStart",  looks %@% "TargetEnd")
  p <- last_plot() + geom_vline(xintercept = x_intercepts, colour = "black", 
                                linetype = "longdash")
  
  # Label timing lines.
  y_rng <- range(looks$Props, na.rm = TRUE)
  line_labels <- c("Carrier Onset", "the", "Target Onset", "Target End")
  
  p <- last_plot() + annotate("text", x = x_intercepts + 25, y = y_rng[1], 
                              hjust = 0, angle = 90, label = line_labels)
  
  
  # If there is a 3 digit identifier in the subject id field, extract it.
  # Otherwise, use the field's raw value.
  id <- looks %@% "Subject"
  digits <- str_extract(id, "^[0-9]{3}")
  id <- ifelse(is.na(digits), id, paste0("ID", digits))
  
  
  # Add a title to complete the template.
  plot_title <- paste0(id, " (", looks %@% "NumTrials", " trials, ", 
                       looks %@% "PercentNA", "% data missing)")
  p <- last_plot() + labs(x = "Time", y = "Proportion of looks to target",
                          title = plot_title)
  
  # Plots for all the conditions
  raw_p <- p + geom_point() + geom_line() + facet_grid(Condition ~ .)
  smooth_p <- p + geom_smooth()
  
  # Remove the two weirdo conditions
  looks_simple <- subset(looks, Condition != "filler" & Condition != "neutral")
  raw_p_simple <- (p + geom_point() + geom_line()) %+% looks_simple
  smooth_p_simple <- smooth_p %+% looks_simple  
  
  # Finish theming, plot and save.
  
  raw_4 <- sprintf('%s%s_4_raw_props_f.png', figs_dir, id)
  raw_2 <- sprintf('%s%s_2_raw_props_f.png', figs_dir, id)
  smooth_4 <- sprintf('%s%s_4_smooth_props_f.png', figs_dir, id)
  smooth_2 <- sprintf('%s%s_2_smooth_props_f.png', figs_dir, id)
  
  ggsave(plot = raw_p, file = raw_4, height = 8.5, width = 11, units = "in")
  ggsave(plot = raw_p_simple, file = raw_2, height = 8.5, width = 11, units = "in")
  ggsave(plot = smooth_p, file = smooth_4, height = 8.5, width = 11, units = "in")
  ggsave(plot = smooth_p_simple, file = smooth_2, height = 8.5, width = 11, units = "in")
}










ComputeMovingAverage <- function(x, n = 1, centered = TRUE) {
  
  if (centered) {
    before <- floor  ((n-1)/2)
    after  <- ceiling((n-1)/2)
  } else {
    before <- n-1
    after  <- 0
  }
  
  # Track the sum and count of number of non-NA items
  s     <- rep(0, length(x))
  count <- rep(0, length(x))
  
  # Add the centered data 
  new <- x
  # Add to count list wherever there isn't a NA 
  count <- count + !is.na(new)
  # Now replace NA_s with 0_s and add to total
  new[is.na(new)] <- 0
  s <- s + new
  
  # Add the data from before
  i <- 1
  while (i <= before) {
    # This is the vector with offset values to add
    new <- c(rep(NA, i), x[1:(length(x)-i)])
    
    count <- count + !is.na(new)
    new[is.na(new)] <- 0
    s <- s + new
    
    i <- i+1
  }
  
  # Add the data from after
  i <- 1
  while (i <= after) {
    # This is the vector with offset values to add
    new   <- c(x[(i+1):length(x)], rep(NA, i))
    
    count <- count + !is.na(new)
    new[is.na(new)] <- 0
    s <- s + new
    
    i <- i+1
  }
  
  # return sum divided by count
  s/count
}





BinLooks <- function(l) {
  IsEven <- function(x) (x %% 2) == 0
  zero_index <- which(l$Time == 0)
  even_index <- IsEven(zero_index)
  even_length <- IsEven(nrow(l))
  
  # Requre that the data have an even number of observations and zero occurs in
  # an an even-numbered row
  if(!even_index | !even_length) stop()
  
  
  rows <- 1:nrow(l)
  base_rows <- which(IsEven(rows))
  move_rows <- which(!IsEven(rows))
  # Add odd rows to even rows
  cols <- c("Zeroes", "Ones", "NAs", "Trials", "Usable")
  l[base_rows, cols] <- l[base_rows, cols] + l[move_rows, cols]
  # Drop odd rows
  l <- l[base_rows, ]
  row.names(l) <- base_rows / 2
  # Recompute proportions
  l$Props <- l$Ones / l$Usable
  return(l)
}





#' Add a `TargetStart` event attribute to each trial
AddTargetStartTimes <- function(trials) {
  
  if (length(trials %@% "Pitch") != 0) {
    .AddTime <- function(trial) {
      trial %@% "TargetStart" <- trial %@% "TargetOnset"
      trial %@% "TheStart" <- trial %@% "CarrierOnset" + 750
      return(trial)
    }
  } else {
    .AddTime <- function(trial) {
      trial %@% "TargetStart" <- trial %@% "TargetOnset" + 470
      trial %@% "TheStart"  <- trial %@% "TargetOnset"
      return(trial)
    }
    
  }
  
  # Preserve classes
  trials_classes <- class(trials)
  # Single trial version of the function

  # Update all the trials
  trials <- Map(.AddTime, trials)
  # Restore classes
  class(trials) <- trials_classes
  return(trials)
}






# Manually align trials if necessary
#event <- "TargetOnset"
AdjustTrial <- function(trial, event) {
  event_time <- trial %@% event
  trial$Time <- trial$Time - event_time
  trial %@% "CarrierOnset" <- trial %@% "CarrierOnset" - event_time
  trial %@% "TargetOnset" <- trial %@% "TargetOnset" - event_time
  trial %@% "TargetStart" <- trial %@% "TargetOnset" + 470
  trial %@% "TargetEnd" <- trial %@% "TargetOnset" + 1270
  return(trial)
}