setwd("//cifs/COMP/ExperimentalTasks/SemanticTask/DataAnalysis/TristanDataAnalysis/")
library(stringr)
library(assertthat)
library(reshape2)
library(plyr)
library(xlsx)

options(stringsAsFactors = FALSE)

# Read in the raw looking data text file
lines <- readLines(file("Semantic_merged_5.3.14.txt"))
closeAllConnections()

# Get rid of blank lines
empty_lines <- str_detect(lines, "^\t+$")
lines <- lines[!empty_lines]

# Get rid of all lines with column headings, except for the first
header_line <- lines[[1]]
headers <- str_detect(lines, header_line)
lines <- lines[!headers]
lines <- c(header_line, lines)

# Treat the lines of text as a data-source
df <- read.table(textConnection(lines), sep = "\t", header = TRUE)

# Assume that all columns from X to the right are frames of looking data
first_frame <- which(names(df) == "X")
last_frame <- length(names(df))
frames <- seq(first_frame, last_frame)

# Make column names include timing in ms from **trial** onset
times <- round((frames - first_frame) * 33.3333, 0)
frame_names <- str_c("Time_", times)
onset <- which(names(df) == "F0")
names(df)[frames] <- frame_names

# Sanity check: F0 is now Time_2000
assert_that(names(df)[onset] == "Time_2000")

# Rename columns of non-measurement info
renaming <- list(
  Subject = "Sub.Num", 
  Sex = "Sex",
  Order = "Order",
  TrialNo = "Tr.Num", 
  ImageL = "L.image", 
  ImageR = "R.image", 
  TargetImage = "Target.Side",
  Target = "Target.Image", 
  Condition = "Condition",
  Response = "Response",
  FirstShiftGap = "First.Shift.Gap",
  RT = "RT",
  TargetOnset = "CritOnSet",
  TargetEnd = "CritOffSet")

for (name_num in seq_along(renaming)) {
  old_name <- renaming[[name_num]]
  new_name <- names(renaming)[name_num]
  names(df)[which(names(df) == old_name)] <- new_name
}

# Drop uninformative columns
df <- subset(df, select = -c(Prescreen.Notes, C.Image, Months)) 

# Convert "" values to NA
make_blank_na <- function(xs) ifelse(xs == "", NA, xs)
df <- colwise(make_blank_na)(df)

# Convert to long format
long_table <- melt(df, id.vars = names(renaming), variable.name = "Time", 
                   value.name = "Look", na.rm = TRUE)

# Turn the Time_ column names into numbers. Update timing so 0 is TargetOnset
long_table$Time <- as.numeric(str_replace(long_table$Time, "Time_", ""))
long_table$Time <- long_table$Time - long_table$TargetOnset
long_table

# Make additional condition columns
long_table$Perceptual <- str_detect(long_table$Condition, "S$") #Perceptual means the perceptually similar condition 
long_table$Semantic <- str_detect(long_table$Condition, "^S")  #Semantic means the semantically similar condition 



########## MERGE IN PARTICPANT INFO 

# define filename 
database_dir <- "//cifs/COMP/Database/"
database_filename <- list.files(database_dir, pattern = "Pilot_Database_[0-9]{6}")
database_filename <- paste0(database_dir, "/", database_filename)

# Load the excel data 
participantinfo <- read.xlsx(database_filename, 1)

# merge the participant data into the main dataset 
long_table <- merge(long_table, participantinfo, by.x="Subject", by.y="ID")


########## SAVE DATASET

save(long_table, file = "looking_data.Rdata")
write.csv(long_table, "looking_data.csv", row.names = FALSE)


