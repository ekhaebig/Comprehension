# Functions for string manipulation.

# Might functions in tools or stringr packages makes these easier?







#' Tokenize a single string
#'
#' .SplitString() is a simple wrapper for the built-in R function strsplit, that
#' makes it more straightforward to split a single string (i.e., a character
#' vector of length 1).
#' 
#' @param string A character vector of length 1 that needs to be split into
#'   substrings.
#' @param splitBy A valid regular expression used to determine where string
#'   should be split.
#' @return A character vector whose entries are the substrings of string that
#'   are separated by splitBy.
#' @examples 
#' .SplitString(string='keep_calm_and_carry_on', splitBy='_')
#' # [1] "keep"  "calm"  "and"   "carry" "on"  
#' 
.SplitString <- function(string, splitBy = '/') {
  
  # Split the string using strsplit. strsplit returns a *list* of length 1.
  split_string <- strsplit(x = string, split = splitBy)
  
  # Return the first element of split_string, which is a character vector whose 
  # elements are the substrings of string separated by splitBy.
  return(split_string[[1]])
}





#' Strip a filepath of its directory path. 
#' 
#' .StripDirectoryPath() is a simple wrapper to the built-in R function strsplit,
#' that strips a full filename of its directory path.  This function is
#' vectorized.
#' 
#' @param filePaths A character vector each entry of which is the full path of a
#'   file.
#' @param delimiter The character that delimits directory names in each path
#'   name.
#' @return A string containing just the file basename and extension, separated
#'   by a dot.
#' @examples 
#' .StripDirectoryPath('/home/pdlg/LearningToTalk/example.txt')
#' # [1] "example.txt"
#' .StripDirectoryPath('C:/pdlg/LearningToTalk/example.txt')
#' # [1] "example.txt"
#' 
.StripDirectoryPath <- function(filePaths, delimiter = '/') {
  
  # A curried function for stripping the directory path off a single file path.
  .LambdaPath <- function(path) {
    
    # Split filePath into its directory names and filename.
    split_path <- .SplitString(string = path, splitBy = delimiter)
    
    # The filename is the last element of split.path.
    filename <- split_path[length(split_path)]
    return(filename)
  }
  # Apply .LambdaPath to each element of filePath, and then reduce the resulting
  # list to a character vector.
  split_paths <- Map(.LambdaPath, filePaths)
  filenames <- Reduce(c, split_paths)
  return(filenames)
} 





#' Remove the extension from a filename
#'
#' .StripExtension() is a simple wrapper to the built-in R function strsplit,
#' that removes the extension from a filename, leaving just the file basename.
#' This function is vectorized.
#'
#' @param filenames A character vector of filenames (i.e.,
#'   [basename].[extension]).
#' @return A string containing just the file basename.
#' @examples
#' .StripExtension('example.txt')  
#' # [1] "example"
#' .StripExtension('example.with.dots.in.the.name.txt') 
#' # [1] "example.with.dots.in.the.name"
#' 
.StripExtension <- function(filenames) {
  
  # A curried function for stripping the extension off a single filename.
  .LambdaFilename <- function(filename) {
    
    # Split filename at dot locations. 
    split_filename <- .SplitString(string = filename, splitBy = '\\.')
    
    # Just in case the filename had dots in the basename...
    basename_substrings <- split_filename[1:(length(split_filename) - 1)]
    file_basename <- paste(basename_substrings, collapse = '.')
    return(file_basename)
  } 
  
  # Apply .LambdaFilename to each element of filenames, and then reduce the
  # resulting list to a character vector.
  split_filenames <- Map(.LambdaFilename, filenames)
  basenames <- Reduce(c, split_filenames)
  return(basenames)
} 
