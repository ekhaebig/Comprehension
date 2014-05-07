library(testthat)
# run me: 
setwd("L:/scripts/lookr/")
source("AOI.R")
setwd("L:/scripts/lookr/tests/")
# test_file("testthat.R")


context(".DefineAOI converts AOI pixel coordinates into screen proportions")
test_that(desc = "DefineAOI is robust to ordering of pixels", {
  xy_ordered <- .DefineAOI(c(300, 600), c(100, 700))
  x_ordered <- .DefineAOI(c(300, 600), c(700, 100))
  y_ordered <- .DefineAOI(c(600, 300), c(100, 700))
  no_ordered <- .DefineAOI(c(600, 300), c(700, 100))
  expect_identical(xy_ordered, x_ordered)
  expect_identical(xy_ordered, y_ordered)
  expect_identical(xy_ordered, no_ordered)
  expect_true(xy_ordered$x[1] < xy_ordered$x[2])
  expect_true(xy_ordered$y[1] < xy_ordered$y[2])
})

context(".GetImageAOI maps AOI names onto AOI coordinates")
test_that(desc = "GetImageAOI handles inputs correctly", {
  .UL_AOI <- .DefineAOI(x_pix = c(410, 860), y_pix = c(500, 50))
  .UR_AOI <- .DefineAOI(x_pix = c(1060, 1510), y_pix = c(500, 50))
  .LR_AOI <- .DefineAOI(x_pix = c(1060, 1510), y_pix = c(1150, 700))
  .LL_AOI <- .DefineAOI(x_pix = c(410, 860), y_pix = c(1150, 700))
  .LEFT_AOI  <- .DefineAOI(x_pix = c(100, 700), y_pix = c(300, 900))
  .RIGHT_AOI <- .DefineAOI(x_pix = c(1220, 1820), y_pix = c(300, 900))
  .FX_AOI <- .DefineAOI(x_pix = c(885, 1035), y_pix = c(525, 675))
  
  expect_identical(.GetImageAOI("UpperLeftImage"), .UL_AOI)
  expect_identical(.GetImageAOI("UpperRightImage"), .UR_AOI)
  expect_identical(.GetImageAOI("LowerRightImage"), .LR_AOI)
  expect_identical(.GetImageAOI("LowerLeftImage"), .LL_AOI)
  expect_identical(.GetImageAOI("FixationImage"), .FX_AOI)
  expect_identical(.GetImageAOI("ImageL"), .LEFT_AOI)
  expect_identical(.GetImageAOI("ImageR"), .RIGHT_AOI)
  expect_error(.GetImageAOI(""))
  expect_error(.GetImageAOI("imageL"))
  expect_error(.GetImageAOI(NA))
})

context(".GetFramesWithGazeInAOI checks whether points in a trial fall into an AOI")
test_that(desc = "test dummy Trials with no/all looks in an AOI", {
  img_AOI <- .LEFT_AOI
  # Try points that are in the AOI in only one dimension, on the corners of the 
  # screen, and close to the edges of the AOI.
  XMean <- c(0, 1, .2, .2, 0, 1, 1, 0, .37, .20, .05, NA, .20, .20)
  YMean <- c(.5, .5, 0, 1, 0, 1, 0, 1, .50, .76, .50, .50, NA, .24999)
  false_trial <- data.frame(XMean, YMean)
  # Expect no false positives
  expect_false(TRUE %in% .GetFramesWithGazeInAOI(false_trial, img_AOI))
  
  # Try a point within the AOI and try its four corners
  XMean2 <- c(.2, img_AOI$x[1], img_AOI$x[2], img_AOI$x[1], img_AOI$x[2])
  YMean2 <- c(.5, img_AOI$y[1], img_AOI$y[1], img_AOI$y[2], img_AOI$y[2])
  true_trial <- data.frame(XMean = XMean2, YMean = YMean2)
  # Expect no false negatives
  expect_false(FALSE %in% .GetFramesWithGazeInAOI(true_trial, img_AOI))
})



context("AddAOIData.Trial correctly maps gaze coordinates onto AOIs")
TestAOIs <- function(trial) {
  # Map AOI data onto trial. 
  trial <- AddAOIData.Trial(trial)    
  
  # Store some useful information from trial attributes.
  q_file <- paste0(trial %@% "TestName", "_stim.png")
  r_file <- paste0(trial %@% "TestName", "_aoi.png")
  title <- paste0(trial %@% "TestName", ", ", trial %@% "Protocol")
  
  # Build a plot template, then specify different colorings for the location
  # and stimuli image plots.
  p <- qplot(data = trial, x = XMean, y = YMean) + 
    labs(title = title) + 
    coord_fixed(ratio = 1200 / 1920)
  q <- p + geom_point(aes(color = GazeByImageAOI)) 
  # r <- p + geom_point(aes(color = GazeByAOI))
  ggsave(plot = q, file = q_file, width=16, height=10)
  # ggsave(plot = r, file = r_file, width=16, height=10)
}

test_that(desc = "Exhaustively plot possible gaze points, colored by AOIs", {
  # Make a trial template to test 241 * 241 points.
  XMean <- c(seq(from = 0, to = 1920, by = 16), NA) / 1920
  YMean <- c(seq(from = 0, to = 1200, by = 12), NA) / 1200
  test_trial <- expand.grid(XMean, YMean)
  names(test_trial) <- c("XMean", "YMean")
  
  # Make trials to represent each test possible MP arrangement
  TestMPTrial <- function(protocol, target, distractor, test_name) {
    trial <- test_trial
    trial %@% "Task" <- "MP"  
    trial %@% "Protocol" <- protocol
    trial %@% "TargetImage" <- target
    trial %@% "DistractorImage" <- distractor
    trial %@% "TestName" <- test_name
    suppressWarnings(TestAOIs(trial))
  }
  
  TestMPTrial("WFF_Movie", "ImageL", "ImageR", "mp_lrf")
  TestMPTrial("WFF_Movie", "ImageR", "ImageL", "mp_rlf")
  TestMPTrial("WFF_Area", "ImageL", "ImageR", "mp_lra")
  TestMPTrial("WFF_Area", "ImageR", "ImageL", "mp_rla")

  # Test all 48 possible RWL trials  
  orders <- structure(list(UpperLeftImage = structure(c(1L, 1L, 1L, 1L, 1L, 
  1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 4L,
  1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 4L,
  4L, 4L, 4L, 4L, 4L), .Label = c("PhonologicalFoil", "SemanticFoil",
  "TargetImage", "Unrelated"), class = "factor"), UpperRightImage =
  structure(c(2L, 2L, 3L, 3L, 4L, 4L, 1L, 1L, 3L, 3L, 4L, 4L, 1L, 1L, 2L, 2L,
  4L, 4L, 1L, 1L, 2L, 2L, 3L, 3L, 2L, 2L, 3L, 3L, 4L, 4L, 1L, 1L, 3L, 3L, 4L,
  4L, 1L, 1L, 2L, 2L, 4L, 4L, 1L, 1L, 2L, 2L, 3L, 3L), .Label =
  c("PhonologicalFoil", "SemanticFoil", "TargetImage", "Unrelated"), class =
  "factor"), LowerRightImage = structure(c(3L, 4L, 2L, 4L, 2L, 3L, 3L, 4L, 1L,
  4L, 1L, 3L, 2L, 4L, 1L, 4L, 1L, 2L, 2L, 3L, 1L, 3L, 1L, 2L, 3L, 4L, 2L, 4L,
  2L, 3L, 3L, 4L, 1L, 4L, 1L, 3L, 2L, 4L, 1L, 4L, 1L, 2L, 2L, 3L, 1L, 3L, 1L,
  2L), .Label = c("PhonologicalFoil", "SemanticFoil", "TargetImage",
  "Unrelated"), class = "factor"), LowerLeftImage = structure(c(4L, 3L, 4L,
  2L, 3L, 2L, 4L, 3L, 4L, 1L, 3L, 1L, 4L, 2L, 4L, 1L, 2L, 1L, 3L, 2L, 3L, 1L, 
  2L, 1L, 4L, 3L, 4L, 2L, 3L, 2L, 4L, 3L, 4L, 1L, 3L, 1L, 4L, 2L, 4L, 1L, 2L,
  1L, 3L, 2L, 3L, 1L, 2L, 1L), .Label = c("PhonologicalFoil", "SemanticFoil",
  "TargetImage", "Unrelated"), class = "factor"), Protocol = c("WFF_Movie",
  "WFF_Movie", "WFF_Movie", "WFF_Movie", "WFF_Movie", "WFF_Movie",
  "WFF_Movie", "WFF_Movie", "WFF_Movie", "WFF_Movie", "WFF_Movie",
  "WFF_Movie", "WFF_Movie", "WFF_Movie", "WFF_Movie", "WFF_Movie",
  "WFF_Movie", "WFF_Movie", "WFF_Movie", "WFF_Movie", "WFF_Movie",
  "WFF_Movie", "WFF_Movie", "WFF_Movie", "WFF_Area", "WFF_Area", "WFF_Area",
  "WFF_Area", "WFF_Area", "WFF_Area", "WFF_Area", "WFF_Area", "WFF_Area",
  "WFF_Area", "WFF_Area", "WFF_Area", "WFF_Area", "WFF_Area", "WFF_Area", 
  "WFF_Area", "WFF_Area", "WFF_Area", "WFF_Area", "WFF_Area", "WFF_Area",
  "WFF_Area", "WFF_Area", "WFF_Area")), .Names = c("UpperLeftImage", 
  "UpperRightImage", "LowerRightImage", "LowerLeftImage", "Protocol" ),
  row.names = c(NA, 48L), class = "data.frame")
  

  TestRWLTrial <- function(row_num) {
    aois <- orders[row_num, ]
    trial <- test_trial
    
    trial %@% "Task" <- "RWL"  
    trial %@% "Protocol" <- aois$Protocol
    trial %@% "UpperLeftImage" <- as.character(aois$UpperLeftImage)
    trial %@% "UpperRightImage" <- as.character(aois$UpperRightImage)
    trial %@% "LowerRightImage" <- as.character(aois$LowerRightImage)
    trial %@% "LowerLeftImage" <- as.character(aois$LowerLeftImage)
    
    # The value of the above attributes are also attribute names, so we make the
    # location and stimulus attributes point name each other. Yes it looks dumb.
    trial %@% (trial %@% "UpperLeftImage") <- "UpperLeftImage"
    trial %@% (trial %@% "UpperRightImage") <- "UpperRightImage"
    trial %@% (trial %@% "LowerRightImage") <- "LowerRightImage"
    trial %@% (trial %@% "LowerLeftImage") <- "LowerLeftImage"
    
    # Come up with a unique test name.
    cs[1] <- str_sub(aois$UpperLeftImage, 0, 1)
    cs[2] <- str_sub(aois$UpperRightImage, 0, 1)
    cs[3] <- str_sub(aois$LowerRightImage, 0, 1)
    cs[4] <- str_sub(aois$LowerLeftImage, 0, 1)
    cs[5] <- ifelse(aois$Protocol == "WFF_Area", "A", "F")
    trial %@% "TestName" <- paste0(c("RWL_", cs), collapse="")
    
    suppressWarnings(TestAOIs(trial))
  }
  
  Map(TestRWLTrial, 1:nrow(orders))
})



