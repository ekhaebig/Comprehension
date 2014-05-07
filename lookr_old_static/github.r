

# Modeled after devtools::source_url
source_url <- function(url, ...) {
  stopifnot(is.character(url), length(url) == 1)
  
  temp_file <- tempfile()
  on.exit(unlink(temp_file))
  
  request <- GET(url)
  stop_for_status(request)
  writeBin(content(request, type = "raw"), temp_file)
  source(temp_file, ...)
}



# Load some functions from github
source_url("https://raw.github.com/tjmahr/lookr/master/R/options.r")
source_url("https://raw.github.com/tjmahr/lookr/master/R/constants.r")

source_url("https://raw.github.com/tjmahr/lookr/master/R/stimdata.r")
source_url("https://raw.github.com/tjmahr/lookr/master/R/stimdata-config.r")
source_url("https://raw.github.com/tjmahr/lookr/master/R/stimdata-config-tasks.r")
source_url("https://raw.github.com/tjmahr/lookr/master/R/stimdata-finalize.r")

source_url("https://raw.github.com/tjmahr/lookr/master/R/gazedata.r")

source_url("https://raw.github.com/tjmahr/lookr/master/R/attributes.r")
source_url("https://raw.github.com/tjmahr/lookr/master/R/oop.r")

source_url("https://raw.github.com/tjmahr/lookr/master/R/session.r")
source_url("https://raw.github.com/tjmahr/lookr/master/R/task.r")

source_url("https://raw.github.com/tjmahr/lookr/master/R/time.r")
source_url("https://raw.github.com/tjmahr/lookr/master/R/aoi.r")

source_url("https://raw.github.com/tjmahr/lookr/master/R/missing.r")
source_url("https://raw.github.com/tjmahr/lookr/master/R/melting.r")
