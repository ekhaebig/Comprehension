



#### Update tasks -------------------------------------------------------------


#' Update a Task object
#' 
#' `UpdateTask(...)` synchronizes the trials in a Task with the blocks that can 
#' be found from a task directory. Any new Blocks are merged into the Task, and 
#' Blocks that could not be found from the task directory are removed from the 
#' Task. In other words, the blocks in the Task are synchronized with those in 
#' the task's directory.
#' 
#' @param task_trials a Task object (list of Trials sharing the same Task 
#'   attribute)
#' @param task_path the location of the subject-level directories from which new
#'   Sessions should be loaded. If no subject directories are found from
#'   `task_path`, the function is aborted.
#' @param refresh if `TRUE` the entire Task is reloaded (and possibly rereduced)
#'   from scratch.
#' @param reducer an optional function to apply to the new trials before 
#'   merging with the other trials. By default, no reduction is done.
UpdateTask <- function(task_trials, task_path, refresh = FALSE, reducer = function(x) x, handler = warning) {
  # Check if there is new data to be added
  subjects <- ListSubjectsInTaskDir(task_path)
  if (is.empty(subjects)) {
    warning(paste0("No subject directories found in: ", task_path, 
                   "\n Update Aborted"))
    return(task_trials)
  }
  
  # Just call Task() if refreshing
  if (refresh) return(reducer(Task(task_path)))
  
  # Make a list of the basenames of all .gazedata files organized by subject dir
  FindGazedata <- function(path) file_path_sans_ext(dir(path, pattern = ".gazedata"))
  candidates <- structure(lapply(subjects, FindGazedata), names = subjects)
  
  # There are four cases that the merge needs to resolve:
  #   1. Folder has BlockX,         and Task has BlockX + BlockY
  #   2. Folder has BlockX + BlockY and Task has BlockX + BlockY
  #   3. Folder has BlockX + BlockY and Task has BlockX.
  #   4. Folder has BlockX + BlockY and Task has neither.
  
  # Case 1: Remove blocks that have disappeared from the dataset
  OldBlocks <- function() unique(task_trials %@% "Basename")
  lost_blocks <- OldBlocks()[!is.element(OldBlocks(), unlist(candidates))]
  task_trials <- FilterBasename(task_trials, lost_blocks)
  
  # Determine which Folder blocks are already in the Task
  has_new_data <- lapply(candidates, function(x) !is.element(x, OldBlocks()))
  
  # Case 2: Remove candidates with no new data
  no_new_data <- names(Filter(Negate(any), has_new_data))
  candidates[no_new_data] <- NULL
  
  # Case 3: Remove partial sessions from the Task, so they can be recompiled
  some_new_data <- names(Filter(function(x) any(x) & !all(x), has_new_data))
  blocks_to_exclude <- unlist(candidates[some_new_data])
  task_trials <- FilterBasename(task_trials, blocks_to_exclude)  
  
  # Case 4: Load remaining blocks
  paths <- ListSubjectsInTaskDir(task_path)
  new_paths <- which(is.element(paths, names(candidates)))
  
  message(paste0(length(new_paths), " blocks of data will be merged into the task"))
  
  # Working on too many blocks at once grinds my machine to a halt, so we merge 
  # them into the task 5 at a time. I think it's AlignTrials and TimeSlice
  # functions on thousands of trials that the gobble all the memory.
  if (!is.empty(new_paths)) {
    for (chunk in slice(new_paths) ) {
      new_blocks <- Task(task_path, partial = chunk, handler=handler)
      if (is.null(new_blocks)) next
      task_trials <- c(task_trials, reducer(new_blocks))
    }  
  }
  
  task_trials
}


# Filter out trials with certain "Basename" attribute values. 
FilterBasename <- MakeAttributeFilter("Basename")
# See `attributes.r` for `MakeAttributeFilter(...)`




#' Divide a vector into equal-sized chunks, except for maybe the last one
#' 
#' @param xs a vector
#' @param step the length of a chunk
#' @return a list with the elements of `xs` partitioned into `step`-sized chunks
slice <- function(xs, step = 5) {
  last_tail <- length(xs)
  heads <- seq(1, last_tail, step)
  expand_head <- function(this_head) { 
    next_tail <- this_head + (step - 1)
    this_tail <- min(next_tail, last_tail) 
    xs[seq(this_head, this_tail)]
  }
  lapply(heads, expand_head)
}








#### Cache tasks --------------------------------------------------------------

#' Create a TaskCache object
#' 
#' A `TaskCache` object is a list that bundles up useful information about where
#' to load session data (`$TaskDir`), where to load and save a Task's cache 
#' (`$CachePath`), how to reduce (pre-process) sessions that loaded 
#' (`$reducer`), and which blocks could not be loaded when the task was being
#' compiled (`$error_blocks()`).
#' 
#' The actual work of creating or updating a cache is done by `UpdateCache`.
#' 
#' @param task_dir directory containing the subject-directories of eye-tracking 
#'   data that will be loaded and cached. Presently, `task_dir` needs to contain
#'   a valid task name (`Mispronunciation` or `RealWordListening`), and a 
#'   timepoint name (`TimePoint[0-9]`).
#' @param cache_dir directory where the task's cache should be saved
#' @param reducer a data-reduction function to be run on Sessions that are 
#'   loaded into the task. Default is `ReduceTrials(...)`
#' @return a TaskCache object which is a list with the following names 
#'   `TaskName`, `TimePoint`, `TaskDir`, `CacheDir`, `CachePath`, `reducer`, 
#'   `errors`, `update_errors`, `error_blocks`, and `exists`.
TaskCache <- function(task_dir, cache_dir, reducer = ReduceTrials) {  
  # Check for timepoint and task names in the path
  time <- str_extract(task_dir, pattern = "TimePoint[0-9]")
  task <- str_extract(task_dir, pattern = "RealWordListening|Mispronunciation|CoArticulation")
  stopifnot(!is.na(task), !is.na(time))
  
  cache_file <- paste0(task, "_", time, "_trials.Rdata")
  cache_path <- paste0(cache_dir, cache_file)
  errors <- character()
  
  structure(list(
    # "nouns"
    TaskName = task, 
    TimePoint = time, 
    TaskDir = task_dir, 
    CacheDir = cache_dir,
    CachePath = cache_path, 
    reducer = reducer,
    # "verbs"
    errors = function() errors,
    update_errors = function(e) errors <<- c(e, errors),
    error_blocks = function() file_path_sans_ext(basename(errors)),
    exists = function() file.exists(cache_path)), class = "TaskCache")
}

#' @method print TaskCache
print.TaskCache <- function(...) str(...)


# Shortcut for reduction
ReduceTrials <- function(trials) {
  trials <- TimeSlice(AlignTrials(trials))  
  trials <- InterpolateMissingFrames(AddAOIData(trials))
  CalculateMistrackings(trials)
}

# 
# .GetDate <- function(stimdata_path) {
#   stimdata_path <- paste0(stimdata_path, ".txt")
#   stimlog <- suppressMessages(try(LoadStimdataFile(stimdata_path)))
#   if (is.error(stimlog)) return(NA)
#   date <- unique(.GetValuesOfStimdataType(stimlog)("SessionDate"))
#   date <- mdy(date)
#   time <- unique(.GetValuesOfStimdataType(stimlog)("SessionTime"))
#   time <- hms(time)
#   date + time[1]
# }




#' Update a Task's cache
#' 
#' If a cache could not be found, it is created. Otherwise the existing cache is
#' loaded and updated. Note that the functionality of loading and updating a 
#' cache is separated into two different functions: `UpdateCache` and 
#' `LoadCache`.
#' 
#' @param task a `TaskCache` object
#' @return the `TaskCache` object with the `$errors` and `$error_blocks` slots 
#'   updated. As a side-effect, the `.Rdata` file described in `$CachePath` is
#'   created or updated and saved.
UpdateCache <- function(...) UseMethod("UpdateCache")

#' @S3method UpdateCache default
UpdateCache.default <- function(...) invisible(NULL)

#' @S3method UpdateCache TaskCache
UpdateCache.TaskCache <- function(task) {
  # Check for the cache. If it doesn't exist, load the first session of it.
  trials <- if (task$exists()) {
    LoadCache(task)
  } else {
    task$reducer(Task(task$TaskDir, partial = 1))
  }    
  # Update the Task object `trials` that was loaded or initialized above.
  trials <- UpdateTask(trials, task$TaskDir, reducer = task$reducer, 
                       handler = task$update_errors)
  save(trials, file = task$CachePath)
  task
}




#' Load a Task's cache
#' 
#' @param task a `TaskCache` object
#' @return the trials in the cache located in `task$CachePath`.
LoadCache <- function(...) UseMethod("LoadCache")

#' @S3method LoadCache default
LoadCache.default <- function(...) invisible(NULL)

#' @S3method LoadCache default
LoadCache.TaskCache <- function(task) {
  stopifnot(task$exists())
  load(task$CachePath)
  trials
}
