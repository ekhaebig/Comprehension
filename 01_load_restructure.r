library(reshape2)
library(xlsx)

# +----------------------------------------+ 
# | Read in the raw looking data text file | ####
# +----------------------------------------+
print("Reading in the raw data from text file...")
# Because this text file is messy (it has headings for each
# subject, and many empty lines) we need to read in this file a
# little more carefully than we would otherwise:

# setwd("F:/Comprehension")
# setwd("G:/Comprehension")

# Open a connection to the file, and specify that it is read-only
f <- file("./Semantic_merged_5.5.14.txt",open="r")

# Read from the file:

# 1. We want the first copy of the column names, but we will omit
# lines matching this later.
first_line <- readLines(f,n=1)

# Note: We have now read the first line of the file. That line is
# now ``behind us''. If we were to read all lines at this point,
# we will run through to the end of the file from our current
# position (which is at the beginning on the second line).
lines <- readLines(f) # This may throw a warning---nothing to worry about.

# We are done reading the file, so close the connection.
close(f)

# We now have a vector, with as many elements as there are lines
# in the text file (minus 1, since we already read the first line
# above to get the column names). We can see which lines we want
# to discard by checking the first character: if the line begins
# with a \t, it means the first field is definitely empty, and
# indicates that the line is blank. If the line begins with S,
# then it is a line containing heading information, and should
# also be removed.
linesToRemove <- substr(lines,1,1) %in% c('\t','S')
lines <- lines[!linesToRemove]

# And now let's stick the header line on the front:
lines <- c(first_line,lines)

# To take this vector of unuseful text and create a useful data
# structure, we can can tell R to read this text variable as if it
# were a file, and then we can pass it through read.table() like
# we would normally.

f <- textConnection(lines, open='r')

# This file delimits columns with tabs. Thus, we will split lines
# into columns wherever there is a \t in the string (the special
# character for ``tab'').
df <- read.table(f, sep = "\t", header = TRUE)

# +--------------------------------+
# | Rename the measurement columns | ####
# +--------------------------------+
print("Renaming the measurement columns...")
# Assume that all columns from X to the right are frames of
# looking data
firstFrame <- which(names(df) == "X")
lastFrame <- length(names(df))
frameColumns <- seq(firstFrame, lastFrame)
frameIndex <- seq(0,lastFrame - firstFrame)

# Make column names include timing in ms from **trial** onset
Hz = 30 # 30 samples per second
samplingRate = 1000/Hz # That's one sample every 33.333 ms

times <- round(frameIndex * samplingRate, 0)
frameNames <- paste("Time_", times, sep="")
onset <- which(names(df) == "F0")
names(df)[frameColumns] <- frameNames

# Sanity check: F0 is now Time_2000
stopifnot(names(df)[onset] == "Time_2000")

# +------------------------------------+
# | Rename the non-measurement columns | ####
# +------------------------------------+
print("Renaming the non-measurement columns...")
# Rename columns of non-measurement info
renaming <- list(
  # OLD  ---> NEW				 
  Sub.Num = "Subject",
  Sex = "Sex",
  Order = "Order",
  Tr.Num = "TrialNo",
  L.image = "ImageL",
  R.image = "ImageR",
  Target.Side = "TargetImage",
  Target.Image = "Target",
  Condition = "Condition",
  Response = "Response",
  First.Shift.Gap = "FirstShiftGap",
  RT = "RT",
  CritOnSet = "TargetOnset",
  CritOffSet = "TargetEnd"
)

i <- 1
# NB: All columns with indexes lower than firstFrame contain
# ``non-measurement info, and these are the ones that will be
# renamed (if there is a proper entry in ``renaming''.
for (n in names(df)[1:(firstFrame-1)]) {
	# If the current column name has an entry in renaming, and if
	# the new name does not match the old name, then update the
	# name using the corresponding entry in renaming.
	if (!is.null(renaming[[n]])) {
		if (n != renaming[[n]]) {
			msg <- sprintf("+ Changing fieldname %s to %s.", n, renaming[n])
			print(msg)
			names(df)[i] <- renaming[n]
		}
	}
	i <- i + 1
}

# +-----------------------------+
# | Drop uninformative columns  | ####
# +-----------------------------+
print("Dropping uninformative columns...")
uninformativeColumns = c(
	"Prescreen.Notes",
	"C.Image", 
	"Months"
)
fieldsToDrop <- names(df) %in% uninformativeColumns
df <- df[,!fieldsToDrop]

# +--------------------------+
# | Convert "" values to NA  | ####
# +--------------------------+
print("Converting empty strings to NAs...")
df[ df=="" ] <- NA

# +-------------------------+
# | Reshape to long format  | ####
# +-------------------------+
print("Reshaping to long format...")
# Convert to long format
meta <- do.call(c,renaming)
names(meta) <- NULL
longTable <- melt(df, id.vars = meta, variable.name = "Time", 
				   value.name = "Look", na.rm = TRUE)

# The levels of the factor are indexed in alphanumeric
# order---that is, if we simply transform the factor to numeric,
# we will get numbers ranging from 1:nFrames that are ordered
# appropriately. If we subtract one, we have base-zero indexes.
# Finally, we can multiply by the sampling rate in milliseconds to
# get back to what we want.
TimeNumeric <- as.numeric(longTable$Time)-1 # zero base.
longTable$Time <- round(TimeNumeric*samplingRate,0)

# Make it so Time=0 corresponds with target onset.
longTable$Time <- longTable$Time - longTable$TargetOnset

# Make additional condition columns
longTable$Perceptual <- longTable$Condition=="DS"
longTable$Semantic <- longTable$Condition=="SD"

########## SAVE CSV FILE 
print('Writing looking_data.csv...')
save("longTable",file="looking_data.RDAT")
write.csv(longTable, "looking_data.csv", row.names = FALSE)
print("DONE!")
########## MERGE IN PARTICPANT INFO 

# define filename 
#database_dir <- "//cifs/COMP/Database/"
database_filename <- list.files('./', pattern = "Pilot_Database_[0-9]{6}")
database_filename <- paste0('.', "/", database_filename)

# Load the excel data 
print(paste('Loading participant info from',database_filename ))
participantinfo <- read.xlsx(database_filename, 1)
print("Merging into the looking dataset...")
# merge the participant data into the main dataset 
longTable <- merge(longTable, participantinfo, by.x="Subject", by.y="ID")
print("DONE!")

######### SAVE DATASET
longTable[ longTable==-99 ] <- NA
save(longTable, file = "looking_data.Rdata")
# 
# # checking things
# longTable2 <- longTable
# rm(longTable)
# load(file="looking_data.Rdata")
# str(longTable)



