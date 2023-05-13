#=======================================================================

# Rattle is Copyright (c) 2006-2020 Togaware Pty Ltd.
# It is free (as in libre) open source software.
# It is licensed under the GNU General Public License,
# Version 2. Rattle comes with ABSOLUTELY NO WARRANTY.
# Rattle was written by Graham Williams with contributions
# from others as acknowledged in 'library(help=rattle)'.
# Visit https://rattle.togaware.com/ for details.

#=======================================================================
# Rattle timestamp: 2021-10-26 13:17:56 x86_64-w64-mingw32 

# Rattle version 5.4.0 user 'Teeners'

# This log captures interactions with Rattle as an R script. 

# For repeatability, export this activity log to a 
# file, like 'model.R' using the Export button or 
# through the Tools menu. Th script can then serve as a 
# starting point for developing your own scripts. 
# After xporting to a file called 'model.R', for exmample, 
# you can type into a new R Console the command 
# "source('model.R')" and so repeat all actions. Generally, 
# you will want to edit the file to suit your own needs. 
# You can also edit this log in place to record additional 
# information before exporting the script. 
 
# Note that saving/loading projects retains this log.

# We begin most scripts by loading the required packages.
# Here are some initial packages to load and others will be
# identified as we proceed through the script. When writing
# our own scripts we often collect together the library
# commands at the beginning of the script here.

library(rattle)   # Access the weather dataset and utilities.
library(magrittr) # Utilise %>% and %<>% pipeline operators.

# This log generally records the process of building a model. 
# However, with very little effort the log can also be used 
# to score a new dataset. The logical variable 'building' 
# is used to toggle between generating transformations, 
# when building a model and using the transformations, 
# when scoring a dataset.

building <- TRUE
scoring  <- ! building

# A pre-defined value is used to reset the random seed 
# so that results are repeatable.

crv$seed <- 42 

#=======================================================================
# Rattle timestamp: 2021-10-26 13:18:49 x86_64-w64-mingw32 

# Load a dataset from file.

fname         <- "file:///C:/Users/Teeners/Documents/2017Data.csv" 
crs$dataset <- read.csv(fname,
			na.strings=c(".", "NA", "", "?"),
			strip.white=TRUE, encoding="UTF-8")

#=======================================================================
# Rattle timestamp: 2021-10-26 13:18:54 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=58221 train=40755 validate=8733 test=8733

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("X.U.FEFF.Ind_id", "job_title", "company_name",
                   "post_code", "vertical", "revenue.type",
                   "relative_revenue_value", "crs_num", "title_name",
                   "event_crs_grade", "ind_crs_grd",
                   "event_inst_grade", "ind_inst_grd", "inst_cd",
                   "content_provider", "primary_curriculum", "level",
                   "cert_aligned", "start_date", "minus.365",
                   "plus.365", "Month", "Year", "train_unit",
                   "length_eve", "attn_hours", "eve_type",
                   "attn_delivery_type", "remote_att", "total",
                   "practicearea", "subpractice", "Courses.Taken",
                   "Course.taken.in.order",
                   "Return.s..within.after.one.year.of.course",
                   "Return.within.1yr..Binary.",
                   "Previous.Purchase.ID",
                   "Most.Recent.Purchase.Date",
                   "Days.between.repurchase")

crs$numeric   <- c("relative_revenue_value", "event_crs_grade",
                   "ind_crs_grd", "event_inst_grade", "ind_inst_grd",
                   "Month", "Year", "length_eve", "attn_hours",
                   "remote_att", "total", "Courses.Taken",
                   "Course.taken.in.order",
                   "Return.s..within.after.one.year.of.course",
                   "Days.between.repurchase")

crs$categoric <- c("X.U.FEFF.Ind_id", "job_title", "company_name",
                   "post_code", "vertical", "revenue.type",
                   "crs_num", "title_name", "inst_cd",
                   "content_provider", "primary_curriculum", "level",
                   "cert_aligned", "start_date", "minus.365",
                   "plus.365", "train_unit", "eve_type",
                   "attn_delivery_type", "practicearea",
                   "subpractice", "Return.within.1yr..Binary.",
                   "Previous.Purchase.ID",
                   "Most.Recent.Purchase.Date")

crs$target    <- "industry"
crs$risk      <- NULL
crs$ident     <- c("unique_Ind_id", "order_id")
crs$ignore    <- c("country", "product_type", "attendees", "Count.of.null", "X..of.if.a.then.b")
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2021-10-26 13:21:58 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=58221 train=29110 validate=14555 test=14556

set.seed(123)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.5*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.25*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("vertical", "revenue.type",
                   "relative_revenue_value", "event_crs_grade",
                   "event_inst_grade", "content_provider",
                   "primary_curriculum", "level", "Month", "Year",
                   "train_unit", "length_eve", "eve_type",
                   "attn_delivery_type", "total", "practicearea",
                   "subpractice", "Courses.Taken",
                   "Course.taken.in.order",
                   "Return.s..within.after.one.year.of.course")

crs$numeric   <- c("relative_revenue_value", "event_crs_grade",
                   "event_inst_grade", "Month", "Year", "length_eve",
                   "total", "Courses.Taken", "Course.taken.in.order",
                   "Return.s..within.after.one.year.of.course")

crs$categoric <- c("vertical", "revenue.type", "content_provider",
                   "primary_curriculum", "level", "train_unit",
                   "eve_type", "attn_delivery_type", "practicearea",
                   "subpractice")

crs$target    <- "Return.within.1yr..Binary."
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("X.U.FEFF.Ind_id", "unique_Ind_id", "job_title", "country", "company_name", "post_code", "order_id", "industry", "product_type", "attendees", "crs_num", "title_name", "ind_crs_grd", "ind_inst_grd", "inst_cd", "cert_aligned", "start_date", "minus.365", "plus.365", "attn_hours", "remote_att", "Count.of.null", "Previous.Purchase.ID", "Most.Recent.Purchase.Date", "Days.between.repurchase", "X..of.if.a.then.b")
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2021-10-26 13:22:06 x86_64-w64-mingw32 

# CLEANUP the Dataset 

# Remove specific variables from the dataset.

crs$dataset$X.U.FEFF.Ind_id <- NULL
crs$dataset$unique_Ind_id <- NULL
crs$dataset$job_title <- NULL
crs$dataset$country <- NULL
crs$dataset$company_name <- NULL
crs$dataset$post_code <- NULL
crs$dataset$order_id <- NULL
crs$dataset$industry <- NULL
crs$dataset$product_type <- NULL
crs$dataset$attendees <- NULL
crs$dataset$crs_num <- NULL
crs$dataset$title_name <- NULL
crs$dataset$ind_crs_grd <- NULL
crs$dataset$ind_inst_grd <- NULL
crs$dataset$inst_cd <- NULL
crs$dataset$cert_aligned <- NULL
crs$dataset$start_date <- NULL
crs$dataset$minus.365 <- NULL
crs$dataset$plus.365 <- NULL
crs$dataset$attn_hours <- NULL
crs$dataset$remote_att <- NULL
crs$dataset$Count.of.null <- NULL
crs$dataset$Previous.Purchase.ID <- NULL
crs$dataset$Most.Recent.Purchase.Date <- NULL
crs$dataset$Days.between.repurchase <- NULL
crs$dataset$X..of.if.a.then.b <- NULL

#=======================================================================
# Rattle timestamp: 2021-10-26 13:22:07 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# The following variable selections have been noted.

crs$input     <- c("vertical", "revenue.type",
                   "relative_revenue_value", "event_crs_grade",
                   "event_inst_grade", "content_provider",
                   "primary_curriculum", "level", "Month", "Year",
                   "train_unit", "length_eve", "eve_type",
                   "attn_delivery_type", "total", "practicearea",
                   "subpractice", "Courses.Taken",
                   "Course.taken.in.order",
                   "Return.s..within.after.one.year.of.course")

crs$numeric   <- c("relative_revenue_value", "event_crs_grade",
                   "event_inst_grade", "Month", "Year", "length_eve",
                   "total", "Courses.Taken", "Course.taken.in.order",
                   "Return.s..within.after.one.year.of.course")

crs$categoric <- c("vertical", "revenue.type", "content_provider",
                   "primary_curriculum", "level", "train_unit",
                   "eve_type", "attn_delivery_type", "practicearea",
                   "subpractice")

crs$target    <- "Return.within.1yr..Binary."
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2021-10-26 13:24:14 x86_64-w64-mingw32 

# CLEANUP the Dataset 

# Remove rows with missing values.

crs$dataset <- crs$dataset[complete.cases(crs$dataset),]

#=======================================================================
# Rattle timestamp: 2021-10-26 13:24:16 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# The following variable selections have been noted.

crs$input     <- c("vertical", "revenue.type",
                   "relative_revenue_value", "event_crs_grade",
                   "event_inst_grade", "content_provider",
                   "primary_curriculum", "level", "Month", "Year",
                   "train_unit", "length_eve", "eve_type",
                   "attn_delivery_type", "total", "practicearea",
                   "subpractice", "Courses.Taken",
                   "Course.taken.in.order",
                   "Return.s..within.after.one.year.of.course")

crs$numeric   <- c("relative_revenue_value", "event_crs_grade",
                   "event_inst_grade", "Month", "Year", "length_eve",
                   "total", "Courses.Taken", "Course.taken.in.order",
                   "Return.s..within.after.one.year.of.course")

crs$categoric <- c("vertical", "revenue.type", "content_provider",
                   "primary_curriculum", "level", "train_unit",
                   "eve_type", "attn_delivery_type", "practicearea",
                   "subpractice")

crs$target    <- "Return.within.1yr..Binary."
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2021-10-26 13:24:35 x86_64-w64-mingw32 

# Transform variables by rescaling. 

# The 'reshape' package provides the 'rescaler' function.

library(reshape, quietly=TRUE)

# Rescale relative_revenue_value.

crs$dataset[["R01_relative_revenue_value"]] <- crs$dataset[["relative_revenue_value"]]

# Rescale to [0,1].

if (building)
{
  crs$dataset[["R01_relative_revenue_value"]] <-  rescaler(crs$dataset[["relative_revenue_value"]], "range")
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R01_relative_revenue_value"]] <- (crs$dataset[["relative_revenue_value"]] - 5.000000)/abs(40.000000 - 5.000000)
}

# Rescale event_crs_grade.

crs$dataset[["R01_event_crs_grade"]] <- crs$dataset[["event_crs_grade"]]

# Rescale to [0,1].

if (building)
{
  crs$dataset[["R01_event_crs_grade"]] <-  rescaler(crs$dataset[["event_crs_grade"]], "range")
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R01_event_crs_grade"]] <- (crs$dataset[["event_crs_grade"]] - 0.000000)/abs(4.000000 - 0.000000)
}

# Rescale event_inst_grade.

crs$dataset[["R01_event_inst_grade"]] <- crs$dataset[["event_inst_grade"]]

# Rescale to [0,1].

if (building)
{
  crs$dataset[["R01_event_inst_grade"]] <-  rescaler(crs$dataset[["event_inst_grade"]], "range")
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R01_event_inst_grade"]] <- (crs$dataset[["event_inst_grade"]] - 0.000000)/abs(4.000000 - 0.000000)
}

# Rescale Month.

crs$dataset[["R01_Month"]] <- crs$dataset[["Month"]]

# Rescale to [0,1].

if (building)
{
  crs$dataset[["R01_Month"]] <-  rescaler(crs$dataset[["Month"]], "range")
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R01_Month"]] <- (crs$dataset[["Month"]] - 1.000000)/abs(12.000000 - 1.000000)
}

# Rescale Year.

crs$dataset[["R01_Year"]] <- crs$dataset[["Year"]]

# Rescale to [0,1].

if (building)
{
  crs$dataset[["R01_Year"]] <-  rescaler(crs$dataset[["Year"]], "range")
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R01_Year"]] <- (crs$dataset[["Year"]] - 2017.000000)/abs(2021.000000 - 2017.000000)
}

# Rescale length_eve.

crs$dataset[["R01_length_eve"]] <- crs$dataset[["length_eve"]]

# Rescale to [0,1].

if (building)
{
  crs$dataset[["R01_length_eve"]] <-  rescaler(crs$dataset[["length_eve"]], "range")
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R01_length_eve"]] <- (crs$dataset[["length_eve"]] - 1.000000)/abs(40.000000 - 1.000000)
}

# Rescale total.

crs$dataset[["R01_total"]] <- crs$dataset[["total"]]

# Rescale to [0,1].

if (building)
{
  crs$dataset[["R01_total"]] <-  rescaler(crs$dataset[["total"]], "range")
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R01_total"]] <- (crs$dataset[["total"]] - 1.000000)/abs(38.000000 - 1.000000)
}

# Rescale Courses.Taken.

crs$dataset[["R01_Courses.Taken"]] <- crs$dataset[["Courses.Taken"]]

# Rescale to [0,1].

if (building)
{
  crs$dataset[["R01_Courses.Taken"]] <-  rescaler(crs$dataset[["Courses.Taken"]], "range")
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R01_Courses.Taken"]] <- (crs$dataset[["Courses.Taken"]] - 1.000000)/abs(37.000000 - 1.000000)
}

# Rescale Course.taken.in.order.

crs$dataset[["R01_Course.taken.in.order"]] <- crs$dataset[["Course.taken.in.order"]]

# Rescale to [0,1].

if (building)
{
  crs$dataset[["R01_Course.taken.in.order"]] <-  rescaler(crs$dataset[["Course.taken.in.order"]], "range")
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R01_Course.taken.in.order"]] <- (crs$dataset[["Course.taken.in.order"]] - 1.000000)/abs(43.000000 - 1.000000)
}

# Rescale Return.s..within.after.one.year.of.course.

crs$dataset[["R01_Return.s..within.after.one.year.of.course"]] <- crs$dataset[["Return.s..within.after.one.year.of.course"]]

# Rescale to [0,1].

if (building)
{
  crs$dataset[["R01_Return.s..within.after.one.year.of.course"]] <-  rescaler(crs$dataset[["Return.s..within.after.one.year.of.course"]], "range")
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R01_Return.s..within.after.one.year.of.course"]] <- (crs$dataset[["Return.s..within.after.one.year.of.course"]] - 0.000000)/abs(31.000000 - 0.000000)
}

#=======================================================================
# Rattle timestamp: 2021-10-26 13:24:36 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# The following variable selections have been noted.

crs$input     <- c("vertical", "revenue.type", "content_provider",
                   "primary_curriculum", "level", "train_unit",
                   "eve_type", "attn_delivery_type", "practicearea",
                   "subpractice", "R01_relative_revenue_value",
                   "R01_event_crs_grade", "R01_event_inst_grade",
                   "R01_Month", "R01_Year", "R01_length_eve",
                   "R01_total", "R01_Courses.Taken",
                   "R01_Course.taken.in.order",
                   "R01_Return.s..within.after.one.year.of.course")

crs$numeric   <- c("R01_relative_revenue_value",
                   "R01_event_crs_grade", "R01_event_inst_grade",
                   "R01_Month", "R01_Year", "R01_length_eve",
                   "R01_total", "R01_Courses.Taken",
                   "R01_Course.taken.in.order",
                   "R01_Return.s..within.after.one.year.of.course")

crs$categoric <- c("vertical", "revenue.type", "content_provider",
                   "primary_curriculum", "level", "train_unit",
                   "eve_type", "attn_delivery_type", "practicearea",
                   "subpractice")

crs$target    <- "Return.within.1yr..Binary."
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("relative_revenue_value", "event_crs_grade", "event_inst_grade", "Month", "Year", "length_eve", "total", "Courses.Taken", "Course.taken.in.order", "Return.s..within.after.one.year.of.course")
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2021-10-26 13:26:20 x86_64-w64-mingw32 

# CLEANUP the Dataset 

# Remove specific variables from the dataset.

crs$dataset$relative_revenue_value <- NULL
crs$dataset$event_crs_grade <- NULL
crs$dataset$event_inst_grade <- NULL
crs$dataset$Month <- NULL
crs$dataset$Year <- NULL
crs$dataset$length_eve <- NULL
crs$dataset$total <- NULL
crs$dataset$Courses.Taken <- NULL
crs$dataset$Course.taken.in.order <- NULL
crs$dataset$Return.s..within.after.one.year.of.course <- NULL

#=======================================================================
# Rattle timestamp: 2021-10-26 13:26:22 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# The following variable selections have been noted.

crs$input     <- c("vertical", "revenue.type", "content_provider",
                   "primary_curriculum", "level", "train_unit",
                   "eve_type", "attn_delivery_type", "practicearea",
                   "subpractice", "R01_relative_revenue_value",
                   "R01_event_crs_grade", "R01_event_inst_grade",
                   "R01_Month", "R01_Year", "R01_length_eve",
                   "R01_total", "R01_Courses.Taken",
                   "R01_Course.taken.in.order",
                   "R01_Return.s..within.after.one.year.of.course")

crs$numeric   <- c("R01_relative_revenue_value",
                   "R01_event_crs_grade", "R01_event_inst_grade",
                   "R01_Month", "R01_Year", "R01_length_eve",
                   "R01_total", "R01_Courses.Taken",
                   "R01_Course.taken.in.order",
                   "R01_Return.s..within.after.one.year.of.course")

crs$categoric <- c("vertical", "revenue.type", "content_provider",
                   "primary_curriculum", "level", "train_unit",
                   "eve_type", "attn_delivery_type", "practicearea",
                   "subpractice")

crs$target    <- "Return.within.1yr..Binary."
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2021-10-26 13:26:34 x86_64-w64-mingw32 

# Remap variables. 

# Turn a factor into indicator variables.

  crs$dataset[, make.names(paste("TIN_vertical_", levels(crs$dataset[["vertical"]]), sep=""))] <- diag(nlevels(crs$dataset[["vertical"]]))[crs$dataset[["vertical"]],]

#=======================================================================
# Rattle timestamp: 2021-10-26 13:26:35 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# The following variable selections have been noted.

crs$input     <- c("revenue.type", "content_provider",
                   "primary_curriculum", "level", "train_unit",
                   "eve_type", "attn_delivery_type", "practicearea",
                   "subpractice", "R01_relative_revenue_value",
                   "R01_event_crs_grade", "R01_event_inst_grade",
                   "R01_Month", "R01_Year", "R01_length_eve",
                   "R01_total", "R01_Courses.Taken",
                   "R01_Course.taken.in.order",
                   "R01_Return.s..within.after.one.year.of.course",
                   "TIN_vertical_Federal.Government",
                   "TIN_vertical_State.and.Local")

crs$numeric   <- c("R01_relative_revenue_value",
                   "R01_event_crs_grade", "R01_event_inst_grade",
                   "R01_Month", "R01_Year", "R01_length_eve",
                   "R01_total", "R01_Courses.Taken",
                   "R01_Course.taken.in.order",
                   "R01_Return.s..within.after.one.year.of.course",
                   "TIN_vertical_Federal.Government",
                   "TIN_vertical_State.and.Local")

crs$categoric <- c("revenue.type", "content_provider",
                   "primary_curriculum", "level", "train_unit",
                   "eve_type", "attn_delivery_type", "practicearea",
                   "subpractice")

crs$target    <- "Return.within.1yr..Binary."
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("vertical", "TIN_vertical_Commercial")
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2021-10-26 13:26:37 x86_64-w64-mingw32 

# Remap variables. 

# Turn a factor into indicator variables.

  crs$dataset[, make.names(paste("TIN_revenue.type_", levels(crs$dataset[["revenue.type"]]), sep=""))] <- diag(nlevels(crs$dataset[["revenue.type"]]))[crs$dataset[["revenue.type"]],]

#=======================================================================
# Rattle timestamp: 2021-10-26 13:26:37 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# The following variable selections have been noted.

crs$input     <- c("content_provider", "primary_curriculum",
                   "level", "train_unit", "eve_type",
                   "attn_delivery_type", "practicearea",
                   "subpractice", "R01_relative_revenue_value",
                   "R01_event_crs_grade", "R01_event_inst_grade",
                   "R01_Month", "R01_Year", "R01_length_eve",
                   "R01_total", "R01_Courses.Taken",
                   "R01_Course.taken.in.order",
                   "R01_Return.s..within.after.one.year.of.course",
                   "TIN_vertical_Federal.Government",
                   "TIN_vertical_State.and.Local",
                   "TIN_revenue.type_Regular",
                   "TIN_revenue.type_Voucher")

crs$numeric   <- c("R01_relative_revenue_value",
                   "R01_event_crs_grade", "R01_event_inst_grade",
                   "R01_Month", "R01_Year", "R01_length_eve",
                   "R01_total", "R01_Courses.Taken",
                   "R01_Course.taken.in.order",
                   "R01_Return.s..within.after.one.year.of.course",
                   "TIN_vertical_Federal.Government",
                   "TIN_vertical_State.and.Local",
                   "TIN_revenue.type_Regular",
                   "TIN_revenue.type_Voucher")

crs$categoric <- c("content_provider", "primary_curriculum",
                   "level", "train_unit", "eve_type",
                   "attn_delivery_type", "practicearea",
                   "subpractice")

crs$target    <- "Return.within.1yr..Binary."
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("vertical", "revenue.type", "TIN_vertical_Commercial", "TIN_revenue.type_Passport")
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2021-10-26 13:26:39 x86_64-w64-mingw32 

# Remap variables. 

# Turn a factor into indicator variables.

  crs$dataset[, make.names(paste("TIN_content_provider_", levels(crs$dataset[["content_provider"]]), sep=""))] <- diag(nlevels(crs$dataset[["content_provider"]]))[crs$dataset[["content_provider"]],]

#=======================================================================
# Rattle timestamp: 2021-10-26 13:26:39 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# The following variable selections have been noted.

crs$input     <- c("primary_curriculum", "level", "train_unit",
                   "eve_type", "attn_delivery_type", "practicearea",
                   "subpractice", "R01_relative_revenue_value",
                   "R01_event_crs_grade", "R01_event_inst_grade",
                   "R01_Month", "R01_Year", "R01_length_eve",
                   "R01_total", "R01_Courses.Taken",
                   "R01_Course.taken.in.order",
                   "R01_Return.s..within.after.one.year.of.course",
                   "TIN_vertical_Federal.Government",
                   "TIN_vertical_State.and.Local",
                   "TIN_revenue.type_Regular",
                   "TIN_revenue.type_Voucher",
                   "TIN_content_provider_Adobe.Partner",
                   "TIN_content_provider_Agile.Partner",
                   "TIN_content_provider_AWS.Partner",
                   "TIN_content_provider_Blockchain.Partner",
                   "TIN_content_provider_Cisco.Partner",
                   "TIN_content_provider_Citrix.Partner",
                   "TIN_content_provider_CompTIA.Partner",
                   "TIN_content_provider_Cyber.Management.Partner",
                   "TIN_content_provider_EC.Council.Partner",
                   "TIN_content_provider_F5.Partner",
                   "TIN_content_provider_IAPP.Partner",
                   "TIN_content_provider_IBM.Partner",
                   "TIN_content_provider_ISACA.Partner",
                   "TIN_content_provider_LTRE.Standard",
                   "TIN_content_provider_Microsoft.Partner",
                   "TIN_content_provider_Nutanix.Partner",
                   "TIN_content_provider_Oracle.Partner",
                   "TIN_content_provider_Palo.Alto.Partner",
                   "TIN_content_provider_Red.Hat.Partner",
                   "TIN_content_provider_Sales.Force.Partner",
                   "TIN_content_provider_SAP.Partner",
                   "TIN_content_provider_VMware.Partner")

crs$numeric   <- c("R01_relative_revenue_value",
                   "R01_event_crs_grade", "R01_event_inst_grade",
                   "R01_Month", "R01_Year", "R01_length_eve",
                   "R01_total", "R01_Courses.Taken",
                   "R01_Course.taken.in.order",
                   "R01_Return.s..within.after.one.year.of.course",
                   "TIN_vertical_Federal.Government",
                   "TIN_vertical_State.and.Local",
                   "TIN_revenue.type_Regular",
                   "TIN_revenue.type_Voucher",
                   "TIN_content_provider_Adobe.Partner",
                   "TIN_content_provider_Agile.Partner",
                   "TIN_content_provider_AWS.Partner",
                   "TIN_content_provider_Blockchain.Partner",
                   "TIN_content_provider_Cisco.Partner",
                   "TIN_content_provider_Citrix.Partner",
                   "TIN_content_provider_CompTIA.Partner",
                   "TIN_content_provider_Cyber.Management.Partner",
                   "TIN_content_provider_EC.Council.Partner",
                   "TIN_content_provider_F5.Partner",
                   "TIN_content_provider_IAPP.Partner",
                   "TIN_content_provider_IBM.Partner",
                   "TIN_content_provider_ISACA.Partner",
                   "TIN_content_provider_LTRE.Standard",
                   "TIN_content_provider_Microsoft.Partner",
                   "TIN_content_provider_Nutanix.Partner",
                   "TIN_content_provider_Oracle.Partner",
                   "TIN_content_provider_Palo.Alto.Partner",
                   "TIN_content_provider_Red.Hat.Partner",
                   "TIN_content_provider_Sales.Force.Partner",
                   "TIN_content_provider_SAP.Partner",
                   "TIN_content_provider_VMware.Partner")

crs$categoric <- c("primary_curriculum", "level", "train_unit",
                   "eve_type", "attn_delivery_type", "practicearea",
                   "subpractice")

crs$target    <- "Return.within.1yr..Binary."
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("vertical", "revenue.type", "content_provider", "TIN_vertical_Commercial", "TIN_revenue.type_Passport")
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2021-10-26 13:26:41 x86_64-w64-mingw32 

# Remap variables. 

# Turn a factor into indicator variables.

  crs$dataset[, make.names(paste("TIN_primary_curriculum_", levels(crs$dataset[["primary_curriculum"]]), sep=""))] <- diag(nlevels(crs$dataset[["primary_curriculum"]]))[crs$dataset[["primary_curriculum"]],]

#=======================================================================
# Rattle timestamp: 2021-10-26 13:26:42 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# The following variable selections have been noted.

crs$input     <- c("level", "train_unit", "eve_type",
                   "attn_delivery_type", "practicearea",
                   "subpractice", "R01_relative_revenue_value",
                   "R01_event_crs_grade", "R01_event_inst_grade",
                   "R01_Month", "R01_Year", "R01_length_eve",
                   "R01_total", "R01_Courses.Taken",
                   "R01_Course.taken.in.order",
                   "R01_Return.s..within.after.one.year.of.course",
                   "TIN_vertical_Federal.Government",
                   "TIN_vertical_State.and.Local",
                   "TIN_revenue.type_Regular",
                   "TIN_revenue.type_Voucher",
                   "TIN_content_provider_Adobe.Partner",
                   "TIN_content_provider_Agile.Partner",
                   "TIN_content_provider_AWS.Partner",
                   "TIN_content_provider_Blockchain.Partner",
                   "TIN_content_provider_Cisco.Partner",
                   "TIN_content_provider_Citrix.Partner",
                   "TIN_content_provider_CompTIA.Partner",
                   "TIN_content_provider_Cyber.Management.Partner",
                   "TIN_content_provider_EC.Council.Partner",
                   "TIN_content_provider_F5.Partner",
                   "TIN_content_provider_IAPP.Partner",
                   "TIN_content_provider_IBM.Partner",
                   "TIN_content_provider_ISACA.Partner",
                   "TIN_content_provider_LTRE.Standard",
                   "TIN_content_provider_Microsoft.Partner",
                   "TIN_content_provider_Nutanix.Partner",
                   "TIN_content_provider_Oracle.Partner",
                   "TIN_content_provider_Palo.Alto.Partner",
                   "TIN_content_provider_Red.Hat.Partner",
                   "TIN_content_provider_Sales.Force.Partner",
                   "TIN_content_provider_SAP.Partner",
                   "TIN_content_provider_VMware.Partner",
                   "TIN_primary_curriculum_.NET.Development",
                   "TIN_primary_curriculum_Adobe.Partner",
                   "TIN_primary_curriculum_Agile",
                   "TIN_primary_curriculum_Agile.Partner",
                   "TIN_primary_curriculum_Apple.IBM.Enterprise",
                   "TIN_primary_curriculum_AWS.Partner",
                   "TIN_primary_curriculum_Azure",
                   "TIN_primary_curriculum_Big.Data",
                   "TIN_primary_curriculum_Blockchain.Partner",
                   "TIN_primary_curriculum_Business.Analysis",
                   "TIN_primary_curriculum_Cisco.Partner",
                   "TIN_primary_curriculum_Citrix.Partner",
                   "TIN_primary_curriculum_Cloud.Computing",
                   "TIN_primary_curriculum_CompTIA.Partner",
                   "TIN_primary_curriculum_Cyber.Management.Partner",
                   "TIN_primary_curriculum_Cyber.Security",
                   "TIN_primary_curriculum_Data.Science",
                   "TIN_primary_curriculum_EC.Council.Partner",
                   "TIN_primary_curriculum_F5.Partner",
                   "TIN_primary_curriculum_IAPP.Partner",
                   "TIN_primary_curriculum_IBM.Partner",
                   "TIN_primary_curriculum_ISACA.Partner",
                   "TIN_primary_curriculum_ITIL..COBIT.and.SFIA",
                   "TIN_primary_curriculum_Java",
                   "TIN_primary_curriculum_Leadership.and.Professional.Development",
                   "TIN_primary_curriculum_Learning.Tree.Coaching",
                   "TIN_primary_curriculum_Microsoft.Office",
                   "TIN_primary_curriculum_Microsoft.Partner",
                   "TIN_primary_curriculum_Microsoft.Project",
                   "TIN_primary_curriculum_Mobile.App.Development",
                   "TIN_primary_curriculum_Networking",
                   "TIN_primary_curriculum_Not.Applicable",
                   "TIN_primary_curriculum_Nutanix.Partner",
                   "TIN_primary_curriculum_Oracle",
                   "TIN_primary_curriculum_Oracle.Partner",
                   "TIN_primary_curriculum_Palo.Alto.Partner",
                   "TIN_primary_curriculum_Perl",
                   "TIN_primary_curriculum_Programming.Fundamentals",
                   "TIN_primary_curriculum_Project.Management",
                   "TIN_primary_curriculum_Python",
                   "TIN_primary_curriculum_Red.Hat.Partner",
                   "TIN_primary_curriculum_Sales.Force.Partner",
                   "TIN_primary_curriculum_SAP.Partner",
                   "TIN_primary_curriculum_SharePoint",
                   "TIN_primary_curriculum_Software.Engineering",
                   "TIN_primary_curriculum_SQL.Server",
                   "TIN_primary_curriculum_TechData",
                   "TIN_primary_curriculum_UNIX.Linux",
                   "TIN_primary_curriculum_VMware.Partner",
                   "TIN_primary_curriculum_Web.Development",
                   "TIN_primary_curriculum_Windows")

crs$numeric   <- c("R01_relative_revenue_value",
                   "R01_event_crs_grade", "R01_event_inst_grade",
                   "R01_Month", "R01_Year", "R01_length_eve",
                   "R01_total", "R01_Courses.Taken",
                   "R01_Course.taken.in.order",
                   "R01_Return.s..within.after.one.year.of.course",
                   "TIN_vertical_Federal.Government",
                   "TIN_vertical_State.and.Local",
                   "TIN_revenue.type_Regular",
                   "TIN_revenue.type_Voucher",
                   "TIN_content_provider_Adobe.Partner",
                   "TIN_content_provider_Agile.Partner",
                   "TIN_content_provider_AWS.Partner",
                   "TIN_content_provider_Blockchain.Partner",
                   "TIN_content_provider_Cisco.Partner",
                   "TIN_content_provider_Citrix.Partner",
                   "TIN_content_provider_CompTIA.Partner",
                   "TIN_content_provider_Cyber.Management.Partner",
                   "TIN_content_provider_EC.Council.Partner",
                   "TIN_content_provider_F5.Partner",
                   "TIN_content_provider_IAPP.Partner",
                   "TIN_content_provider_IBM.Partner",
                   "TIN_content_provider_ISACA.Partner",
                   "TIN_content_provider_LTRE.Standard",
                   "TIN_content_provider_Microsoft.Partner",
                   "TIN_content_provider_Nutanix.Partner",
                   "TIN_content_provider_Oracle.Partner",
                   "TIN_content_provider_Palo.Alto.Partner",
                   "TIN_content_provider_Red.Hat.Partner",
                   "TIN_content_provider_Sales.Force.Partner",
                   "TIN_content_provider_SAP.Partner",
                   "TIN_content_provider_VMware.Partner",
                   "TIN_primary_curriculum_.NET.Development",
                   "TIN_primary_curriculum_Adobe.Partner",
                   "TIN_primary_curriculum_Agile",
                   "TIN_primary_curriculum_Agile.Partner",
                   "TIN_primary_curriculum_Apple.IBM.Enterprise",
                   "TIN_primary_curriculum_AWS.Partner",
                   "TIN_primary_curriculum_Azure",
                   "TIN_primary_curriculum_Big.Data",
                   "TIN_primary_curriculum_Blockchain.Partner",
                   "TIN_primary_curriculum_Business.Analysis",
                   "TIN_primary_curriculum_Cisco.Partner",
                   "TIN_primary_curriculum_Citrix.Partner",
                   "TIN_primary_curriculum_Cloud.Computing",
                   "TIN_primary_curriculum_CompTIA.Partner",
                   "TIN_primary_curriculum_Cyber.Management.Partner",
                   "TIN_primary_curriculum_Cyber.Security",
                   "TIN_primary_curriculum_Data.Science",
                   "TIN_primary_curriculum_EC.Council.Partner",
                   "TIN_primary_curriculum_F5.Partner",
                   "TIN_primary_curriculum_IAPP.Partner",
                   "TIN_primary_curriculum_IBM.Partner",
                   "TIN_primary_curriculum_ISACA.Partner",
                   "TIN_primary_curriculum_ITIL..COBIT.and.SFIA",
                   "TIN_primary_curriculum_Java",
                   "TIN_primary_curriculum_Leadership.and.Professional.Development",
                   "TIN_primary_curriculum_Learning.Tree.Coaching",
                   "TIN_primary_curriculum_Microsoft.Office",
                   "TIN_primary_curriculum_Microsoft.Partner",
                   "TIN_primary_curriculum_Microsoft.Project",
                   "TIN_primary_curriculum_Mobile.App.Development",
                   "TIN_primary_curriculum_Networking",
                   "TIN_primary_curriculum_Not.Applicable",
                   "TIN_primary_curriculum_Nutanix.Partner",
                   "TIN_primary_curriculum_Oracle",
                   "TIN_primary_curriculum_Oracle.Partner",
                   "TIN_primary_curriculum_Palo.Alto.Partner",
                   "TIN_primary_curriculum_Perl",
                   "TIN_primary_curriculum_Programming.Fundamentals",
                   "TIN_primary_curriculum_Project.Management",
                   "TIN_primary_curriculum_Python",
                   "TIN_primary_curriculum_Red.Hat.Partner",
                   "TIN_primary_curriculum_Sales.Force.Partner",
                   "TIN_primary_curriculum_SAP.Partner",
                   "TIN_primary_curriculum_SharePoint",
                   "TIN_primary_curriculum_Software.Engineering",
                   "TIN_primary_curriculum_SQL.Server",
                   "TIN_primary_curriculum_TechData",
                   "TIN_primary_curriculum_UNIX.Linux",
                   "TIN_primary_curriculum_VMware.Partner",
                   "TIN_primary_curriculum_Web.Development",
                   "TIN_primary_curriculum_Windows")

crs$categoric <- c("level", "train_unit", "eve_type",
                   "attn_delivery_type", "practicearea",
                   "subpractice")

crs$target    <- "Return.within.1yr..Binary."
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("vertical", "revenue.type", "content_provider", "primary_curriculum", "TIN_vertical_Commercial", "TIN_revenue.type_Passport")
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2021-10-26 13:26:46 x86_64-w64-mingw32 

# Remap variables. 

# Turn a factor into indicator variables.

  crs$dataset[, make.names(paste("TIN_level_", levels(crs$dataset[["level"]]), sep=""))] <- diag(nlevels(crs$dataset[["level"]]))[crs$dataset[["level"]],]

#=======================================================================
# Rattle timestamp: 2021-10-26 13:26:47 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# The following variable selections have been noted.

crs$input     <- c("train_unit", "eve_type", "attn_delivery_type",
                   "practicearea", "subpractice",
                   "R01_relative_revenue_value",
                   "R01_event_crs_grade", "R01_event_inst_grade",
                   "R01_Month", "R01_Year", "R01_length_eve",
                   "R01_total", "R01_Courses.Taken",
                   "R01_Course.taken.in.order",
                   "R01_Return.s..within.after.one.year.of.course",
                   "TIN_vertical_Federal.Government",
                   "TIN_vertical_State.and.Local",
                   "TIN_revenue.type_Regular",
                   "TIN_revenue.type_Voucher",
                   "TIN_content_provider_Adobe.Partner",
                   "TIN_content_provider_Agile.Partner",
                   "TIN_content_provider_AWS.Partner",
                   "TIN_content_provider_Blockchain.Partner",
                   "TIN_content_provider_Cisco.Partner",
                   "TIN_content_provider_Citrix.Partner",
                   "TIN_content_provider_CompTIA.Partner",
                   "TIN_content_provider_Cyber.Management.Partner",
                   "TIN_content_provider_EC.Council.Partner",
                   "TIN_content_provider_F5.Partner",
                   "TIN_content_provider_IAPP.Partner",
                   "TIN_content_provider_IBM.Partner",
                   "TIN_content_provider_ISACA.Partner",
                   "TIN_content_provider_LTRE.Standard",
                   "TIN_content_provider_Microsoft.Partner",
                   "TIN_content_provider_Nutanix.Partner",
                   "TIN_content_provider_Oracle.Partner",
                   "TIN_content_provider_Palo.Alto.Partner",
                   "TIN_content_provider_Red.Hat.Partner",
                   "TIN_content_provider_Sales.Force.Partner",
                   "TIN_content_provider_SAP.Partner",
                   "TIN_content_provider_VMware.Partner",
                   "TIN_primary_curriculum_.NET.Development",
                   "TIN_primary_curriculum_Adobe.Partner",
                   "TIN_primary_curriculum_Agile",
                   "TIN_primary_curriculum_Agile.Partner",
                   "TIN_primary_curriculum_Apple.IBM.Enterprise",
                   "TIN_primary_curriculum_AWS.Partner",
                   "TIN_primary_curriculum_Azure",
                   "TIN_primary_curriculum_Big.Data",
                   "TIN_primary_curriculum_Blockchain.Partner",
                   "TIN_primary_curriculum_Business.Analysis",
                   "TIN_primary_curriculum_Cisco.Partner",
                   "TIN_primary_curriculum_Citrix.Partner",
                   "TIN_primary_curriculum_Cloud.Computing",
                   "TIN_primary_curriculum_CompTIA.Partner",
                   "TIN_primary_curriculum_Cyber.Management.Partner",
                   "TIN_primary_curriculum_Cyber.Security",
                   "TIN_primary_curriculum_Data.Science",
                   "TIN_primary_curriculum_EC.Council.Partner",
                   "TIN_primary_curriculum_F5.Partner",
                   "TIN_primary_curriculum_IAPP.Partner",
                   "TIN_primary_curriculum_IBM.Partner",
                   "TIN_primary_curriculum_ISACA.Partner",
                   "TIN_primary_curriculum_ITIL..COBIT.and.SFIA",
                   "TIN_primary_curriculum_Java",
                   "TIN_primary_curriculum_Leadership.and.Professional.Development",
                   "TIN_primary_curriculum_Learning.Tree.Coaching",
                   "TIN_primary_curriculum_Microsoft.Office",
                   "TIN_primary_curriculum_Microsoft.Partner",
                   "TIN_primary_curriculum_Microsoft.Project",
                   "TIN_primary_curriculum_Mobile.App.Development",
                   "TIN_primary_curriculum_Networking",
                   "TIN_primary_curriculum_Not.Applicable",
                   "TIN_primary_curriculum_Nutanix.Partner",
                   "TIN_primary_curriculum_Oracle",
                   "TIN_primary_curriculum_Oracle.Partner",
                   "TIN_primary_curriculum_Palo.Alto.Partner",
                   "TIN_primary_curriculum_Perl",
                   "TIN_primary_curriculum_Programming.Fundamentals",
                   "TIN_primary_curriculum_Project.Management",
                   "TIN_primary_curriculum_Python",
                   "TIN_primary_curriculum_Red.Hat.Partner",
                   "TIN_primary_curriculum_Sales.Force.Partner",
                   "TIN_primary_curriculum_SAP.Partner",
                   "TIN_primary_curriculum_SharePoint",
                   "TIN_primary_curriculum_Software.Engineering",
                   "TIN_primary_curriculum_SQL.Server",
                   "TIN_primary_curriculum_TechData",
                   "TIN_primary_curriculum_UNIX.Linux",
                   "TIN_primary_curriculum_VMware.Partner",
                   "TIN_primary_curriculum_Web.Development",
                   "TIN_primary_curriculum_Windows",
                   "TIN_level_Foundation", "TIN_level_Intermediate")

crs$numeric   <- c("R01_relative_revenue_value",
                   "R01_event_crs_grade", "R01_event_inst_grade",
                   "R01_Month", "R01_Year", "R01_length_eve",
                   "R01_total", "R01_Courses.Taken",
                   "R01_Course.taken.in.order",
                   "R01_Return.s..within.after.one.year.of.course",
                   "TIN_vertical_Federal.Government",
                   "TIN_vertical_State.and.Local",
                   "TIN_revenue.type_Regular",
                   "TIN_revenue.type_Voucher",
                   "TIN_content_provider_Adobe.Partner",
                   "TIN_content_provider_Agile.Partner",
                   "TIN_content_provider_AWS.Partner",
                   "TIN_content_provider_Blockchain.Partner",
                   "TIN_content_provider_Cisco.Partner",
                   "TIN_content_provider_Citrix.Partner",
                   "TIN_content_provider_CompTIA.Partner",
                   "TIN_content_provider_Cyber.Management.Partner",
                   "TIN_content_provider_EC.Council.Partner",
                   "TIN_content_provider_F5.Partner",
                   "TIN_content_provider_IAPP.Partner",
                   "TIN_content_provider_IBM.Partner",
                   "TIN_content_provider_ISACA.Partner",
                   "TIN_content_provider_LTRE.Standard",
                   "TIN_content_provider_Microsoft.Partner",
                   "TIN_content_provider_Nutanix.Partner",
                   "TIN_content_provider_Oracle.Partner",
                   "TIN_content_provider_Palo.Alto.Partner",
                   "TIN_content_provider_Red.Hat.Partner",
                   "TIN_content_provider_Sales.Force.Partner",
                   "TIN_content_provider_SAP.Partner",
                   "TIN_content_provider_VMware.Partner",
                   "TIN_primary_curriculum_.NET.Development",
                   "TIN_primary_curriculum_Adobe.Partner",
                   "TIN_primary_curriculum_Agile",
                   "TIN_primary_curriculum_Agile.Partner",
                   "TIN_primary_curriculum_Apple.IBM.Enterprise",
                   "TIN_primary_curriculum_AWS.Partner",
                   "TIN_primary_curriculum_Azure",
                   "TIN_primary_curriculum_Big.Data",
                   "TIN_primary_curriculum_Blockchain.Partner",
                   "TIN_primary_curriculum_Business.Analysis",
                   "TIN_primary_curriculum_Cisco.Partner",
                   "TIN_primary_curriculum_Citrix.Partner",
                   "TIN_primary_curriculum_Cloud.Computing",
                   "TIN_primary_curriculum_CompTIA.Partner",
                   "TIN_primary_curriculum_Cyber.Management.Partner",
                   "TIN_primary_curriculum_Cyber.Security",
                   "TIN_primary_curriculum_Data.Science",
                   "TIN_primary_curriculum_EC.Council.Partner",
                   "TIN_primary_curriculum_F5.Partner",
                   "TIN_primary_curriculum_IAPP.Partner",
                   "TIN_primary_curriculum_IBM.Partner",
                   "TIN_primary_curriculum_ISACA.Partner",
                   "TIN_primary_curriculum_ITIL..COBIT.and.SFIA",
                   "TIN_primary_curriculum_Java",
                   "TIN_primary_curriculum_Leadership.and.Professional.Development",
                   "TIN_primary_curriculum_Learning.Tree.Coaching",
                   "TIN_primary_curriculum_Microsoft.Office",
                   "TIN_primary_curriculum_Microsoft.Partner",
                   "TIN_primary_curriculum_Microsoft.Project",
                   "TIN_primary_curriculum_Mobile.App.Development",
                   "TIN_primary_curriculum_Networking",
                   "TIN_primary_curriculum_Not.Applicable",
                   "TIN_primary_curriculum_Nutanix.Partner",
                   "TIN_primary_curriculum_Oracle",
                   "TIN_primary_curriculum_Oracle.Partner",
                   "TIN_primary_curriculum_Palo.Alto.Partner",
                   "TIN_primary_curriculum_Perl",
                   "TIN_primary_curriculum_Programming.Fundamentals",
                   "TIN_primary_curriculum_Project.Management",
                   "TIN_primary_curriculum_Python",
                   "TIN_primary_curriculum_Red.Hat.Partner",
                   "TIN_primary_curriculum_Sales.Force.Partner",
                   "TIN_primary_curriculum_SAP.Partner",
                   "TIN_primary_curriculum_SharePoint",
                   "TIN_primary_curriculum_Software.Engineering",
                   "TIN_primary_curriculum_SQL.Server",
                   "TIN_primary_curriculum_TechData",
                   "TIN_primary_curriculum_UNIX.Linux",
                   "TIN_primary_curriculum_VMware.Partner",
                   "TIN_primary_curriculum_Web.Development",
                   "TIN_primary_curriculum_Windows",
                   "TIN_level_Foundation", "TIN_level_Intermediate")

crs$categoric <- c("train_unit", "eve_type", "attn_delivery_type",
                   "practicearea", "subpractice")

crs$target    <- "Return.within.1yr..Binary."
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("vertical", "revenue.type", "content_provider", "primary_curriculum", "level", "TIN_vertical_Commercial", "TIN_revenue.type_Passport", "TIN_level_Advanced")
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2021-10-26 13:26:50 x86_64-w64-mingw32 

# Remap variables. 

# Turn a factor into indicator variables.

  crs$dataset[, make.names(paste("TIN_train_unit_", levels(crs$dataset[["train_unit"]]), sep=""))] <- diag(nlevels(crs$dataset[["train_unit"]]))[crs$dataset[["train_unit"]],]

#=======================================================================
# Rattle timestamp: 2021-10-26 13:26:51 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# The following variable selections have been noted.

crs$input     <- c("eve_type", "attn_delivery_type",
                   "practicearea", "subpractice",
                   "R01_relative_revenue_value",
                   "R01_event_crs_grade", "R01_event_inst_grade",
                   "R01_Month", "R01_Year", "R01_length_eve",
                   "R01_total", "R01_Courses.Taken",
                   "R01_Course.taken.in.order",
                   "R01_Return.s..within.after.one.year.of.course",
                   "TIN_vertical_Federal.Government",
                   "TIN_vertical_State.and.Local",
                   "TIN_revenue.type_Regular",
                   "TIN_revenue.type_Voucher",
                   "TIN_content_provider_Adobe.Partner",
                   "TIN_content_provider_Agile.Partner",
                   "TIN_content_provider_AWS.Partner",
                   "TIN_content_provider_Blockchain.Partner",
                   "TIN_content_provider_Cisco.Partner",
                   "TIN_content_provider_Citrix.Partner",
                   "TIN_content_provider_CompTIA.Partner",
                   "TIN_content_provider_Cyber.Management.Partner",
                   "TIN_content_provider_EC.Council.Partner",
                   "TIN_content_provider_F5.Partner",
                   "TIN_content_provider_IAPP.Partner",
                   "TIN_content_provider_IBM.Partner",
                   "TIN_content_provider_ISACA.Partner",
                   "TIN_content_provider_LTRE.Standard",
                   "TIN_content_provider_Microsoft.Partner",
                   "TIN_content_provider_Nutanix.Partner",
                   "TIN_content_provider_Oracle.Partner",
                   "TIN_content_provider_Palo.Alto.Partner",
                   "TIN_content_provider_Red.Hat.Partner",
                   "TIN_content_provider_Sales.Force.Partner",
                   "TIN_content_provider_SAP.Partner",
                   "TIN_content_provider_VMware.Partner",
                   "TIN_primary_curriculum_.NET.Development",
                   "TIN_primary_curriculum_Adobe.Partner",
                   "TIN_primary_curriculum_Agile",
                   "TIN_primary_curriculum_Agile.Partner",
                   "TIN_primary_curriculum_Apple.IBM.Enterprise",
                   "TIN_primary_curriculum_AWS.Partner",
                   "TIN_primary_curriculum_Azure",
                   "TIN_primary_curriculum_Big.Data",
                   "TIN_primary_curriculum_Blockchain.Partner",
                   "TIN_primary_curriculum_Business.Analysis",
                   "TIN_primary_curriculum_Cisco.Partner",
                   "TIN_primary_curriculum_Citrix.Partner",
                   "TIN_primary_curriculum_Cloud.Computing",
                   "TIN_primary_curriculum_CompTIA.Partner",
                   "TIN_primary_curriculum_Cyber.Management.Partner",
                   "TIN_primary_curriculum_Cyber.Security",
                   "TIN_primary_curriculum_Data.Science",
                   "TIN_primary_curriculum_EC.Council.Partner",
                   "TIN_primary_curriculum_F5.Partner",
                   "TIN_primary_curriculum_IAPP.Partner",
                   "TIN_primary_curriculum_IBM.Partner",
                   "TIN_primary_curriculum_ISACA.Partner",
                   "TIN_primary_curriculum_ITIL..COBIT.and.SFIA",
                   "TIN_primary_curriculum_Java",
                   "TIN_primary_curriculum_Leadership.and.Professional.Development",
                   "TIN_primary_curriculum_Learning.Tree.Coaching",
                   "TIN_primary_curriculum_Microsoft.Office",
                   "TIN_primary_curriculum_Microsoft.Partner",
                   "TIN_primary_curriculum_Microsoft.Project",
                   "TIN_primary_curriculum_Mobile.App.Development",
                   "TIN_primary_curriculum_Networking",
                   "TIN_primary_curriculum_Not.Applicable",
                   "TIN_primary_curriculum_Nutanix.Partner",
                   "TIN_primary_curriculum_Oracle",
                   "TIN_primary_curriculum_Oracle.Partner",
                   "TIN_primary_curriculum_Palo.Alto.Partner",
                   "TIN_primary_curriculum_Perl",
                   "TIN_primary_curriculum_Programming.Fundamentals",
                   "TIN_primary_curriculum_Project.Management",
                   "TIN_primary_curriculum_Python",
                   "TIN_primary_curriculum_Red.Hat.Partner",
                   "TIN_primary_curriculum_Sales.Force.Partner",
                   "TIN_primary_curriculum_SAP.Partner",
                   "TIN_primary_curriculum_SharePoint",
                   "TIN_primary_curriculum_Software.Engineering",
                   "TIN_primary_curriculum_SQL.Server",
                   "TIN_primary_curriculum_TechData",
                   "TIN_primary_curriculum_UNIX.Linux",
                   "TIN_primary_curriculum_VMware.Partner",
                   "TIN_primary_curriculum_Web.Development",
                   "TIN_primary_curriculum_Windows",
                   "TIN_level_Foundation", "TIN_level_Intermediate",
                   "TIN_train_unit_EN", "TIN_train_unit_US")

crs$numeric   <- c("R01_relative_revenue_value",
                   "R01_event_crs_grade", "R01_event_inst_grade",
                   "R01_Month", "R01_Year", "R01_length_eve",
                   "R01_total", "R01_Courses.Taken",
                   "R01_Course.taken.in.order",
                   "R01_Return.s..within.after.one.year.of.course",
                   "TIN_vertical_Federal.Government",
                   "TIN_vertical_State.and.Local",
                   "TIN_revenue.type_Regular",
                   "TIN_revenue.type_Voucher",
                   "TIN_content_provider_Adobe.Partner",
                   "TIN_content_provider_Agile.Partner",
                   "TIN_content_provider_AWS.Partner",
                   "TIN_content_provider_Blockchain.Partner",
                   "TIN_content_provider_Cisco.Partner",
                   "TIN_content_provider_Citrix.Partner",
                   "TIN_content_provider_CompTIA.Partner",
                   "TIN_content_provider_Cyber.Management.Partner",
                   "TIN_content_provider_EC.Council.Partner",
                   "TIN_content_provider_F5.Partner",
                   "TIN_content_provider_IAPP.Partner",
                   "TIN_content_provider_IBM.Partner",
                   "TIN_content_provider_ISACA.Partner",
                   "TIN_content_provider_LTRE.Standard",
                   "TIN_content_provider_Microsoft.Partner",
                   "TIN_content_provider_Nutanix.Partner",
                   "TIN_content_provider_Oracle.Partner",
                   "TIN_content_provider_Palo.Alto.Partner",
                   "TIN_content_provider_Red.Hat.Partner",
                   "TIN_content_provider_Sales.Force.Partner",
                   "TIN_content_provider_SAP.Partner",
                   "TIN_content_provider_VMware.Partner",
                   "TIN_primary_curriculum_.NET.Development",
                   "TIN_primary_curriculum_Adobe.Partner",
                   "TIN_primary_curriculum_Agile",
                   "TIN_primary_curriculum_Agile.Partner",
                   "TIN_primary_curriculum_Apple.IBM.Enterprise",
                   "TIN_primary_curriculum_AWS.Partner",
                   "TIN_primary_curriculum_Azure",
                   "TIN_primary_curriculum_Big.Data",
                   "TIN_primary_curriculum_Blockchain.Partner",
                   "TIN_primary_curriculum_Business.Analysis",
                   "TIN_primary_curriculum_Cisco.Partner",
                   "TIN_primary_curriculum_Citrix.Partner",
                   "TIN_primary_curriculum_Cloud.Computing",
                   "TIN_primary_curriculum_CompTIA.Partner",
                   "TIN_primary_curriculum_Cyber.Management.Partner",
                   "TIN_primary_curriculum_Cyber.Security",
                   "TIN_primary_curriculum_Data.Science",
                   "TIN_primary_curriculum_EC.Council.Partner",
                   "TIN_primary_curriculum_F5.Partner",
                   "TIN_primary_curriculum_IAPP.Partner",
                   "TIN_primary_curriculum_IBM.Partner",
                   "TIN_primary_curriculum_ISACA.Partner",
                   "TIN_primary_curriculum_ITIL..COBIT.and.SFIA",
                   "TIN_primary_curriculum_Java",
                   "TIN_primary_curriculum_Leadership.and.Professional.Development",
                   "TIN_primary_curriculum_Learning.Tree.Coaching",
                   "TIN_primary_curriculum_Microsoft.Office",
                   "TIN_primary_curriculum_Microsoft.Partner",
                   "TIN_primary_curriculum_Microsoft.Project",
                   "TIN_primary_curriculum_Mobile.App.Development",
                   "TIN_primary_curriculum_Networking",
                   "TIN_primary_curriculum_Not.Applicable",
                   "TIN_primary_curriculum_Nutanix.Partner",
                   "TIN_primary_curriculum_Oracle",
                   "TIN_primary_curriculum_Oracle.Partner",
                   "TIN_primary_curriculum_Palo.Alto.Partner",
                   "TIN_primary_curriculum_Perl",
                   "TIN_primary_curriculum_Programming.Fundamentals",
                   "TIN_primary_curriculum_Project.Management",
                   "TIN_primary_curriculum_Python",
                   "TIN_primary_curriculum_Red.Hat.Partner",
                   "TIN_primary_curriculum_Sales.Force.Partner",
                   "TIN_primary_curriculum_SAP.Partner",
                   "TIN_primary_curriculum_SharePoint",
                   "TIN_primary_curriculum_Software.Engineering",
                   "TIN_primary_curriculum_SQL.Server",
                   "TIN_primary_curriculum_TechData",
                   "TIN_primary_curriculum_UNIX.Linux",
                   "TIN_primary_curriculum_VMware.Partner",
                   "TIN_primary_curriculum_Web.Development",
                   "TIN_primary_curriculum_Windows",
                   "TIN_level_Foundation", "TIN_level_Intermediate",
                   "TIN_train_unit_EN", "TIN_train_unit_US")

crs$categoric <- c("eve_type", "attn_delivery_type",
                   "practicearea", "subpractice")

crs$target    <- "Return.within.1yr..Binary."
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("vertical", "revenue.type", "content_provider", "primary_curriculum", "level", "train_unit", "TIN_vertical_Commercial", "TIN_revenue.type_Passport", "TIN_level_Advanced", "TIN_train_unit_CN")
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2021-10-26 13:26:53 x86_64-w64-mingw32 

# Remap variables. 

# Turn a factor into indicator variables.

  crs$dataset[, make.names(paste("TIN_eve_type_", levels(crs$dataset[["eve_type"]]), sep=""))] <- diag(nlevels(crs$dataset[["eve_type"]]))[crs$dataset[["eve_type"]],]

#=======================================================================
# Rattle timestamp: 2021-10-26 13:26:54 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# The following variable selections have been noted.

crs$input     <- c("attn_delivery_type", "practicearea",
                   "subpractice", "R01_relative_revenue_value",
                   "R01_event_crs_grade", "R01_event_inst_grade",
                   "R01_Month", "R01_Year", "R01_length_eve",
                   "R01_total", "R01_Courses.Taken",
                   "R01_Course.taken.in.order",
                   "R01_Return.s..within.after.one.year.of.course",
                   "TIN_vertical_Federal.Government",
                   "TIN_vertical_State.and.Local",
                   "TIN_revenue.type_Regular",
                   "TIN_revenue.type_Voucher",
                   "TIN_content_provider_Adobe.Partner",
                   "TIN_content_provider_Agile.Partner",
                   "TIN_content_provider_AWS.Partner",
                   "TIN_content_provider_Blockchain.Partner",
                   "TIN_content_provider_Cisco.Partner",
                   "TIN_content_provider_Citrix.Partner",
                   "TIN_content_provider_CompTIA.Partner",
                   "TIN_content_provider_Cyber.Management.Partner",
                   "TIN_content_provider_EC.Council.Partner",
                   "TIN_content_provider_F5.Partner",
                   "TIN_content_provider_IAPP.Partner",
                   "TIN_content_provider_IBM.Partner",
                   "TIN_content_provider_ISACA.Partner",
                   "TIN_content_provider_LTRE.Standard",
                   "TIN_content_provider_Microsoft.Partner",
                   "TIN_content_provider_Nutanix.Partner",
                   "TIN_content_provider_Oracle.Partner",
                   "TIN_content_provider_Palo.Alto.Partner",
                   "TIN_content_provider_Red.Hat.Partner",
                   "TIN_content_provider_Sales.Force.Partner",
                   "TIN_content_provider_SAP.Partner",
                   "TIN_content_provider_VMware.Partner",
                   "TIN_primary_curriculum_.NET.Development",
                   "TIN_primary_curriculum_Adobe.Partner",
                   "TIN_primary_curriculum_Agile",
                   "TIN_primary_curriculum_Agile.Partner",
                   "TIN_primary_curriculum_Apple.IBM.Enterprise",
                   "TIN_primary_curriculum_AWS.Partner",
                   "TIN_primary_curriculum_Azure",
                   "TIN_primary_curriculum_Big.Data",
                   "TIN_primary_curriculum_Blockchain.Partner",
                   "TIN_primary_curriculum_Business.Analysis",
                   "TIN_primary_curriculum_Cisco.Partner",
                   "TIN_primary_curriculum_Citrix.Partner",
                   "TIN_primary_curriculum_Cloud.Computing",
                   "TIN_primary_curriculum_CompTIA.Partner",
                   "TIN_primary_curriculum_Cyber.Management.Partner",
                   "TIN_primary_curriculum_Cyber.Security",
                   "TIN_primary_curriculum_Data.Science",
                   "TIN_primary_curriculum_EC.Council.Partner",
                   "TIN_primary_curriculum_F5.Partner",
                   "TIN_primary_curriculum_IAPP.Partner",
                   "TIN_primary_curriculum_IBM.Partner",
                   "TIN_primary_curriculum_ISACA.Partner",
                   "TIN_primary_curriculum_ITIL..COBIT.and.SFIA",
                   "TIN_primary_curriculum_Java",
                   "TIN_primary_curriculum_Leadership.and.Professional.Development",
                   "TIN_primary_curriculum_Learning.Tree.Coaching",
                   "TIN_primary_curriculum_Microsoft.Office",
                   "TIN_primary_curriculum_Microsoft.Partner",
                   "TIN_primary_curriculum_Microsoft.Project",
                   "TIN_primary_curriculum_Mobile.App.Development",
                   "TIN_primary_curriculum_Networking",
                   "TIN_primary_curriculum_Not.Applicable",
                   "TIN_primary_curriculum_Nutanix.Partner",
                   "TIN_primary_curriculum_Oracle",
                   "TIN_primary_curriculum_Oracle.Partner",
                   "TIN_primary_curriculum_Palo.Alto.Partner",
                   "TIN_primary_curriculum_Perl",
                   "TIN_primary_curriculum_Programming.Fundamentals",
                   "TIN_primary_curriculum_Project.Management",
                   "TIN_primary_curriculum_Python",
                   "TIN_primary_curriculum_Red.Hat.Partner",
                   "TIN_primary_curriculum_Sales.Force.Partner",
                   "TIN_primary_curriculum_SAP.Partner",
                   "TIN_primary_curriculum_SharePoint",
                   "TIN_primary_curriculum_Software.Engineering",
                   "TIN_primary_curriculum_SQL.Server",
                   "TIN_primary_curriculum_TechData",
                   "TIN_primary_curriculum_UNIX.Linux",
                   "TIN_primary_curriculum_VMware.Partner",
                   "TIN_primary_curriculum_Web.Development",
                   "TIN_primary_curriculum_Windows",
                   "TIN_level_Foundation", "TIN_level_Intermediate",
                   "TIN_train_unit_EN", "TIN_train_unit_US",
                   "TIN_eve_type_VIRTUAL")

crs$numeric   <- c("R01_relative_revenue_value",
                   "R01_event_crs_grade", "R01_event_inst_grade",
                   "R01_Month", "R01_Year", "R01_length_eve",
                   "R01_total", "R01_Courses.Taken",
                   "R01_Course.taken.in.order",
                   "R01_Return.s..within.after.one.year.of.course",
                   "TIN_vertical_Federal.Government",
                   "TIN_vertical_State.and.Local",
                   "TIN_revenue.type_Regular",
                   "TIN_revenue.type_Voucher",
                   "TIN_content_provider_Adobe.Partner",
                   "TIN_content_provider_Agile.Partner",
                   "TIN_content_provider_AWS.Partner",
                   "TIN_content_provider_Blockchain.Partner",
                   "TIN_content_provider_Cisco.Partner",
                   "TIN_content_provider_Citrix.Partner",
                   "TIN_content_provider_CompTIA.Partner",
                   "TIN_content_provider_Cyber.Management.Partner",
                   "TIN_content_provider_EC.Council.Partner",
                   "TIN_content_provider_F5.Partner",
                   "TIN_content_provider_IAPP.Partner",
                   "TIN_content_provider_IBM.Partner",
                   "TIN_content_provider_ISACA.Partner",
                   "TIN_content_provider_LTRE.Standard",
                   "TIN_content_provider_Microsoft.Partner",
                   "TIN_content_provider_Nutanix.Partner",
                   "TIN_content_provider_Oracle.Partner",
                   "TIN_content_provider_Palo.Alto.Partner",
                   "TIN_content_provider_Red.Hat.Partner",
                   "TIN_content_provider_Sales.Force.Partner",
                   "TIN_content_provider_SAP.Partner",
                   "TIN_content_provider_VMware.Partner",
                   "TIN_primary_curriculum_.NET.Development",
                   "TIN_primary_curriculum_Adobe.Partner",
                   "TIN_primary_curriculum_Agile",
                   "TIN_primary_curriculum_Agile.Partner",
                   "TIN_primary_curriculum_Apple.IBM.Enterprise",
                   "TIN_primary_curriculum_AWS.Partner",
                   "TIN_primary_curriculum_Azure",
                   "TIN_primary_curriculum_Big.Data",
                   "TIN_primary_curriculum_Blockchain.Partner",
                   "TIN_primary_curriculum_Business.Analysis",
                   "TIN_primary_curriculum_Cisco.Partner",
                   "TIN_primary_curriculum_Citrix.Partner",
                   "TIN_primary_curriculum_Cloud.Computing",
                   "TIN_primary_curriculum_CompTIA.Partner",
                   "TIN_primary_curriculum_Cyber.Management.Partner",
                   "TIN_primary_curriculum_Cyber.Security",
                   "TIN_primary_curriculum_Data.Science",
                   "TIN_primary_curriculum_EC.Council.Partner",
                   "TIN_primary_curriculum_F5.Partner",
                   "TIN_primary_curriculum_IAPP.Partner",
                   "TIN_primary_curriculum_IBM.Partner",
                   "TIN_primary_curriculum_ISACA.Partner",
                   "TIN_primary_curriculum_ITIL..COBIT.and.SFIA",
                   "TIN_primary_curriculum_Java",
                   "TIN_primary_curriculum_Leadership.and.Professional.Development",
                   "TIN_primary_curriculum_Learning.Tree.Coaching",
                   "TIN_primary_curriculum_Microsoft.Office",
                   "TIN_primary_curriculum_Microsoft.Partner",
                   "TIN_primary_curriculum_Microsoft.Project",
                   "TIN_primary_curriculum_Mobile.App.Development",
                   "TIN_primary_curriculum_Networking",
                   "TIN_primary_curriculum_Not.Applicable",
                   "TIN_primary_curriculum_Nutanix.Partner",
                   "TIN_primary_curriculum_Oracle",
                   "TIN_primary_curriculum_Oracle.Partner",
                   "TIN_primary_curriculum_Palo.Alto.Partner",
                   "TIN_primary_curriculum_Perl",
                   "TIN_primary_curriculum_Programming.Fundamentals",
                   "TIN_primary_curriculum_Project.Management",
                   "TIN_primary_curriculum_Python",
                   "TIN_primary_curriculum_Red.Hat.Partner",
                   "TIN_primary_curriculum_Sales.Force.Partner",
                   "TIN_primary_curriculum_SAP.Partner",
                   "TIN_primary_curriculum_SharePoint",
                   "TIN_primary_curriculum_Software.Engineering",
                   "TIN_primary_curriculum_SQL.Server",
                   "TIN_primary_curriculum_TechData",
                   "TIN_primary_curriculum_UNIX.Linux",
                   "TIN_primary_curriculum_VMware.Partner",
                   "TIN_primary_curriculum_Web.Development",
                   "TIN_primary_curriculum_Windows",
                   "TIN_level_Foundation", "TIN_level_Intermediate",
                   "TIN_train_unit_EN", "TIN_train_unit_US",
                   "TIN_eve_type_VIRTUAL")

crs$categoric <- c("attn_delivery_type", "practicearea",
                   "subpractice")

crs$target    <- "Return.within.1yr..Binary."
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("vertical", "revenue.type", "content_provider", "primary_curriculum", "level", "train_unit", "eve_type", "TIN_vertical_Commercial", "TIN_revenue.type_Passport", "TIN_level_Advanced", "TIN_train_unit_CN", "TIN_eve_type_HYBRID")
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2021-10-26 13:26:55 x86_64-w64-mingw32 

# Remap variables. 

# Turn a factor into indicator variables.

  crs$dataset[, make.names(paste("TIN_attn_delivery_type_", levels(crs$dataset[["attn_delivery_type"]]), sep=""))] <- diag(nlevels(crs$dataset[["attn_delivery_type"]]))[crs$dataset[["attn_delivery_type"]],]

#=======================================================================
# Rattle timestamp: 2021-10-26 13:26:56 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# The following variable selections have been noted.

crs$input     <- c("practicearea", "subpractice",
                   "R01_relative_revenue_value",
                   "R01_event_crs_grade", "R01_event_inst_grade",
                   "R01_Month", "R01_Year", "R01_length_eve",
                   "R01_total", "R01_Courses.Taken",
                   "R01_Course.taken.in.order",
                   "R01_Return.s..within.after.one.year.of.course",
                   "TIN_vertical_Federal.Government",
                   "TIN_vertical_State.and.Local",
                   "TIN_revenue.type_Regular",
                   "TIN_revenue.type_Voucher",
                   "TIN_content_provider_Adobe.Partner",
                   "TIN_content_provider_Agile.Partner",
                   "TIN_content_provider_AWS.Partner",
                   "TIN_content_provider_Blockchain.Partner",
                   "TIN_content_provider_Cisco.Partner",
                   "TIN_content_provider_Citrix.Partner",
                   "TIN_content_provider_CompTIA.Partner",
                   "TIN_content_provider_Cyber.Management.Partner",
                   "TIN_content_provider_EC.Council.Partner",
                   "TIN_content_provider_F5.Partner",
                   "TIN_content_provider_IAPP.Partner",
                   "TIN_content_provider_IBM.Partner",
                   "TIN_content_provider_ISACA.Partner",
                   "TIN_content_provider_LTRE.Standard",
                   "TIN_content_provider_Microsoft.Partner",
                   "TIN_content_provider_Nutanix.Partner",
                   "TIN_content_provider_Oracle.Partner",
                   "TIN_content_provider_Palo.Alto.Partner",
                   "TIN_content_provider_Red.Hat.Partner",
                   "TIN_content_provider_Sales.Force.Partner",
                   "TIN_content_provider_SAP.Partner",
                   "TIN_content_provider_VMware.Partner",
                   "TIN_primary_curriculum_.NET.Development",
                   "TIN_primary_curriculum_Adobe.Partner",
                   "TIN_primary_curriculum_Agile",
                   "TIN_primary_curriculum_Agile.Partner",
                   "TIN_primary_curriculum_Apple.IBM.Enterprise",
                   "TIN_primary_curriculum_AWS.Partner",
                   "TIN_primary_curriculum_Azure",
                   "TIN_primary_curriculum_Big.Data",
                   "TIN_primary_curriculum_Blockchain.Partner",
                   "TIN_primary_curriculum_Business.Analysis",
                   "TIN_primary_curriculum_Cisco.Partner",
                   "TIN_primary_curriculum_Citrix.Partner",
                   "TIN_primary_curriculum_Cloud.Computing",
                   "TIN_primary_curriculum_CompTIA.Partner",
                   "TIN_primary_curriculum_Cyber.Management.Partner",
                   "TIN_primary_curriculum_Cyber.Security",
                   "TIN_primary_curriculum_Data.Science",
                   "TIN_primary_curriculum_EC.Council.Partner",
                   "TIN_primary_curriculum_F5.Partner",
                   "TIN_primary_curriculum_IAPP.Partner",
                   "TIN_primary_curriculum_IBM.Partner",
                   "TIN_primary_curriculum_ISACA.Partner",
                   "TIN_primary_curriculum_ITIL..COBIT.and.SFIA",
                   "TIN_primary_curriculum_Java",
                   "TIN_primary_curriculum_Leadership.and.Professional.Development",
                   "TIN_primary_curriculum_Learning.Tree.Coaching",
                   "TIN_primary_curriculum_Microsoft.Office",
                   "TIN_primary_curriculum_Microsoft.Partner",
                   "TIN_primary_curriculum_Microsoft.Project",
                   "TIN_primary_curriculum_Mobile.App.Development",
                   "TIN_primary_curriculum_Networking",
                   "TIN_primary_curriculum_Not.Applicable",
                   "TIN_primary_curriculum_Nutanix.Partner",
                   "TIN_primary_curriculum_Oracle",
                   "TIN_primary_curriculum_Oracle.Partner",
                   "TIN_primary_curriculum_Palo.Alto.Partner",
                   "TIN_primary_curriculum_Perl",
                   "TIN_primary_curriculum_Programming.Fundamentals",
                   "TIN_primary_curriculum_Project.Management",
                   "TIN_primary_curriculum_Python",
                   "TIN_primary_curriculum_Red.Hat.Partner",
                   "TIN_primary_curriculum_Sales.Force.Partner",
                   "TIN_primary_curriculum_SAP.Partner",
                   "TIN_primary_curriculum_SharePoint",
                   "TIN_primary_curriculum_Software.Engineering",
                   "TIN_primary_curriculum_SQL.Server",
                   "TIN_primary_curriculum_TechData",
                   "TIN_primary_curriculum_UNIX.Linux",
                   "TIN_primary_curriculum_VMware.Partner",
                   "TIN_primary_curriculum_Web.Development",
                   "TIN_primary_curriculum_Windows",
                   "TIN_level_Foundation", "TIN_level_Intermediate",
                   "TIN_train_unit_EN", "TIN_train_unit_US",
                   "TIN_eve_type_VIRTUAL",
                   "TIN_attn_delivery_type_Virtual")

crs$numeric   <- c("R01_relative_revenue_value",
                   "R01_event_crs_grade", "R01_event_inst_grade",
                   "R01_Month", "R01_Year", "R01_length_eve",
                   "R01_total", "R01_Courses.Taken",
                   "R01_Course.taken.in.order",
                   "R01_Return.s..within.after.one.year.of.course",
                   "TIN_vertical_Federal.Government",
                   "TIN_vertical_State.and.Local",
                   "TIN_revenue.type_Regular",
                   "TIN_revenue.type_Voucher",
                   "TIN_content_provider_Adobe.Partner",
                   "TIN_content_provider_Agile.Partner",
                   "TIN_content_provider_AWS.Partner",
                   "TIN_content_provider_Blockchain.Partner",
                   "TIN_content_provider_Cisco.Partner",
                   "TIN_content_provider_Citrix.Partner",
                   "TIN_content_provider_CompTIA.Partner",
                   "TIN_content_provider_Cyber.Management.Partner",
                   "TIN_content_provider_EC.Council.Partner",
                   "TIN_content_provider_F5.Partner",
                   "TIN_content_provider_IAPP.Partner",
                   "TIN_content_provider_IBM.Partner",
                   "TIN_content_provider_ISACA.Partner",
                   "TIN_content_provider_LTRE.Standard",
                   "TIN_content_provider_Microsoft.Partner",
                   "TIN_content_provider_Nutanix.Partner",
                   "TIN_content_provider_Oracle.Partner",
                   "TIN_content_provider_Palo.Alto.Partner",
                   "TIN_content_provider_Red.Hat.Partner",
                   "TIN_content_provider_Sales.Force.Partner",
                   "TIN_content_provider_SAP.Partner",
                   "TIN_content_provider_VMware.Partner",
                   "TIN_primary_curriculum_.NET.Development",
                   "TIN_primary_curriculum_Adobe.Partner",
                   "TIN_primary_curriculum_Agile",
                   "TIN_primary_curriculum_Agile.Partner",
                   "TIN_primary_curriculum_Apple.IBM.Enterprise",
                   "TIN_primary_curriculum_AWS.Partner",
                   "TIN_primary_curriculum_Azure",
                   "TIN_primary_curriculum_Big.Data",
                   "TIN_primary_curriculum_Blockchain.Partner",
                   "TIN_primary_curriculum_Business.Analysis",
                   "TIN_primary_curriculum_Cisco.Partner",
                   "TIN_primary_curriculum_Citrix.Partner",
                   "TIN_primary_curriculum_Cloud.Computing",
                   "TIN_primary_curriculum_CompTIA.Partner",
                   "TIN_primary_curriculum_Cyber.Management.Partner",
                   "TIN_primary_curriculum_Cyber.Security",
                   "TIN_primary_curriculum_Data.Science",
                   "TIN_primary_curriculum_EC.Council.Partner",
                   "TIN_primary_curriculum_F5.Partner",
                   "TIN_primary_curriculum_IAPP.Partner",
                   "TIN_primary_curriculum_IBM.Partner",
                   "TIN_primary_curriculum_ISACA.Partner",
                   "TIN_primary_curriculum_ITIL..COBIT.and.SFIA",
                   "TIN_primary_curriculum_Java",
                   "TIN_primary_curriculum_Leadership.and.Professional.Development",
                   "TIN_primary_curriculum_Learning.Tree.Coaching",
                   "TIN_primary_curriculum_Microsoft.Office",
                   "TIN_primary_curriculum_Microsoft.Partner",
                   "TIN_primary_curriculum_Microsoft.Project",
                   "TIN_primary_curriculum_Mobile.App.Development",
                   "TIN_primary_curriculum_Networking",
                   "TIN_primary_curriculum_Not.Applicable",
                   "TIN_primary_curriculum_Nutanix.Partner",
                   "TIN_primary_curriculum_Oracle",
                   "TIN_primary_curriculum_Oracle.Partner",
                   "TIN_primary_curriculum_Palo.Alto.Partner",
                   "TIN_primary_curriculum_Perl",
                   "TIN_primary_curriculum_Programming.Fundamentals",
                   "TIN_primary_curriculum_Project.Management",
                   "TIN_primary_curriculum_Python",
                   "TIN_primary_curriculum_Red.Hat.Partner",
                   "TIN_primary_curriculum_Sales.Force.Partner",
                   "TIN_primary_curriculum_SAP.Partner",
                   "TIN_primary_curriculum_SharePoint",
                   "TIN_primary_curriculum_Software.Engineering",
                   "TIN_primary_curriculum_SQL.Server",
                   "TIN_primary_curriculum_TechData",
                   "TIN_primary_curriculum_UNIX.Linux",
                   "TIN_primary_curriculum_VMware.Partner",
                   "TIN_primary_curriculum_Web.Development",
                   "TIN_primary_curriculum_Windows",
                   "TIN_level_Foundation", "TIN_level_Intermediate",
                   "TIN_train_unit_EN", "TIN_train_unit_US",
                   "TIN_eve_type_VIRTUAL",
                   "TIN_attn_delivery_type_Virtual")

crs$categoric <- c("practicearea", "subpractice")

crs$target    <- "Return.within.1yr..Binary."
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("vertical", "revenue.type", "content_provider", "primary_curriculum", "level", "train_unit", "eve_type", "attn_delivery_type", "TIN_vertical_Commercial", "TIN_revenue.type_Passport", "TIN_level_Advanced", "TIN_train_unit_CN", "TIN_eve_type_HYBRID", "TIN_attn_delivery_type_In.Class")
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2021-10-26 13:26:58 x86_64-w64-mingw32 

# Remap variables. 

# Turn a factor into indicator variables.

  crs$dataset[, make.names(paste("TIN_practicearea_", levels(crs$dataset[["practicearea"]]), sep=""))] <- diag(nlevels(crs$dataset[["practicearea"]]))[crs$dataset[["practicearea"]],]

#=======================================================================
# Rattle timestamp: 2021-10-26 13:26:59 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# The following variable selections have been noted.

crs$input     <- c("subpractice", "R01_relative_revenue_value",
                   "R01_event_crs_grade", "R01_event_inst_grade",
                   "R01_Month", "R01_Year", "R01_length_eve",
                   "R01_total", "R01_Courses.Taken",
                   "R01_Course.taken.in.order",
                   "R01_Return.s..within.after.one.year.of.course",
                   "TIN_vertical_Federal.Government",
                   "TIN_vertical_State.and.Local",
                   "TIN_revenue.type_Regular",
                   "TIN_revenue.type_Voucher",
                   "TIN_content_provider_Adobe.Partner",
                   "TIN_content_provider_Agile.Partner",
                   "TIN_content_provider_AWS.Partner",
                   "TIN_content_provider_Blockchain.Partner",
                   "TIN_content_provider_Cisco.Partner",
                   "TIN_content_provider_Citrix.Partner",
                   "TIN_content_provider_CompTIA.Partner",
                   "TIN_content_provider_Cyber.Management.Partner",
                   "TIN_content_provider_EC.Council.Partner",
                   "TIN_content_provider_F5.Partner",
                   "TIN_content_provider_IAPP.Partner",
                   "TIN_content_provider_IBM.Partner",
                   "TIN_content_provider_ISACA.Partner",
                   "TIN_content_provider_LTRE.Standard",
                   "TIN_content_provider_Microsoft.Partner",
                   "TIN_content_provider_Nutanix.Partner",
                   "TIN_content_provider_Oracle.Partner",
                   "TIN_content_provider_Palo.Alto.Partner",
                   "TIN_content_provider_Red.Hat.Partner",
                   "TIN_content_provider_Sales.Force.Partner",
                   "TIN_content_provider_SAP.Partner",
                   "TIN_content_provider_VMware.Partner",
                   "TIN_primary_curriculum_.NET.Development",
                   "TIN_primary_curriculum_Adobe.Partner",
                   "TIN_primary_curriculum_Agile",
                   "TIN_primary_curriculum_Agile.Partner",
                   "TIN_primary_curriculum_Apple.IBM.Enterprise",
                   "TIN_primary_curriculum_AWS.Partner",
                   "TIN_primary_curriculum_Azure",
                   "TIN_primary_curriculum_Big.Data",
                   "TIN_primary_curriculum_Blockchain.Partner",
                   "TIN_primary_curriculum_Business.Analysis",
                   "TIN_primary_curriculum_Cisco.Partner",
                   "TIN_primary_curriculum_Citrix.Partner",
                   "TIN_primary_curriculum_Cloud.Computing",
                   "TIN_primary_curriculum_CompTIA.Partner",
                   "TIN_primary_curriculum_Cyber.Management.Partner",
                   "TIN_primary_curriculum_Cyber.Security",
                   "TIN_primary_curriculum_Data.Science",
                   "TIN_primary_curriculum_EC.Council.Partner",
                   "TIN_primary_curriculum_F5.Partner",
                   "TIN_primary_curriculum_IAPP.Partner",
                   "TIN_primary_curriculum_IBM.Partner",
                   "TIN_primary_curriculum_ISACA.Partner",
                   "TIN_primary_curriculum_ITIL..COBIT.and.SFIA",
                   "TIN_primary_curriculum_Java",
                   "TIN_primary_curriculum_Leadership.and.Professional.Development",
                   "TIN_primary_curriculum_Learning.Tree.Coaching",
                   "TIN_primary_curriculum_Microsoft.Office",
                   "TIN_primary_curriculum_Microsoft.Partner",
                   "TIN_primary_curriculum_Microsoft.Project",
                   "TIN_primary_curriculum_Mobile.App.Development",
                   "TIN_primary_curriculum_Networking",
                   "TIN_primary_curriculum_Not.Applicable",
                   "TIN_primary_curriculum_Nutanix.Partner",
                   "TIN_primary_curriculum_Oracle",
                   "TIN_primary_curriculum_Oracle.Partner",
                   "TIN_primary_curriculum_Palo.Alto.Partner",
                   "TIN_primary_curriculum_Perl",
                   "TIN_primary_curriculum_Programming.Fundamentals",
                   "TIN_primary_curriculum_Project.Management",
                   "TIN_primary_curriculum_Python",
                   "TIN_primary_curriculum_Red.Hat.Partner",
                   "TIN_primary_curriculum_Sales.Force.Partner",
                   "TIN_primary_curriculum_SAP.Partner",
                   "TIN_primary_curriculum_SharePoint",
                   "TIN_primary_curriculum_Software.Engineering",
                   "TIN_primary_curriculum_SQL.Server",
                   "TIN_primary_curriculum_TechData",
                   "TIN_primary_curriculum_UNIX.Linux",
                   "TIN_primary_curriculum_VMware.Partner",
                   "TIN_primary_curriculum_Web.Development",
                   "TIN_primary_curriculum_Windows",
                   "TIN_level_Foundation", "TIN_level_Intermediate",
                   "TIN_train_unit_EN", "TIN_train_unit_US",
                   "TIN_eve_type_VIRTUAL",
                   "TIN_attn_delivery_type_Virtual",
                   "TIN_practicearea_Leadership.and.Professional.Development",
                   "TIN_practicearea_Learning.Tree.Coaching",
                   "TIN_practicearea_Process.and.methodology",
                   "TIN_practicearea_Risk.and.Compliance")

crs$numeric   <- c("R01_relative_revenue_value",
                   "R01_event_crs_grade", "R01_event_inst_grade",
                   "R01_Month", "R01_Year", "R01_length_eve",
                   "R01_total", "R01_Courses.Taken",
                   "R01_Course.taken.in.order",
                   "R01_Return.s..within.after.one.year.of.course",
                   "TIN_vertical_Federal.Government",
                   "TIN_vertical_State.and.Local",
                   "TIN_revenue.type_Regular",
                   "TIN_revenue.type_Voucher",
                   "TIN_content_provider_Adobe.Partner",
                   "TIN_content_provider_Agile.Partner",
                   "TIN_content_provider_AWS.Partner",
                   "TIN_content_provider_Blockchain.Partner",
                   "TIN_content_provider_Cisco.Partner",
                   "TIN_content_provider_Citrix.Partner",
                   "TIN_content_provider_CompTIA.Partner",
                   "TIN_content_provider_Cyber.Management.Partner",
                   "TIN_content_provider_EC.Council.Partner",
                   "TIN_content_provider_F5.Partner",
                   "TIN_content_provider_IAPP.Partner",
                   "TIN_content_provider_IBM.Partner",
                   "TIN_content_provider_ISACA.Partner",
                   "TIN_content_provider_LTRE.Standard",
                   "TIN_content_provider_Microsoft.Partner",
                   "TIN_content_provider_Nutanix.Partner",
                   "TIN_content_provider_Oracle.Partner",
                   "TIN_content_provider_Palo.Alto.Partner",
                   "TIN_content_provider_Red.Hat.Partner",
                   "TIN_content_provider_Sales.Force.Partner",
                   "TIN_content_provider_SAP.Partner",
                   "TIN_content_provider_VMware.Partner",
                   "TIN_primary_curriculum_.NET.Development",
                   "TIN_primary_curriculum_Adobe.Partner",
                   "TIN_primary_curriculum_Agile",
                   "TIN_primary_curriculum_Agile.Partner",
                   "TIN_primary_curriculum_Apple.IBM.Enterprise",
                   "TIN_primary_curriculum_AWS.Partner",
                   "TIN_primary_curriculum_Azure",
                   "TIN_primary_curriculum_Big.Data",
                   "TIN_primary_curriculum_Blockchain.Partner",
                   "TIN_primary_curriculum_Business.Analysis",
                   "TIN_primary_curriculum_Cisco.Partner",
                   "TIN_primary_curriculum_Citrix.Partner",
                   "TIN_primary_curriculum_Cloud.Computing",
                   "TIN_primary_curriculum_CompTIA.Partner",
                   "TIN_primary_curriculum_Cyber.Management.Partner",
                   "TIN_primary_curriculum_Cyber.Security",
                   "TIN_primary_curriculum_Data.Science",
                   "TIN_primary_curriculum_EC.Council.Partner",
                   "TIN_primary_curriculum_F5.Partner",
                   "TIN_primary_curriculum_IAPP.Partner",
                   "TIN_primary_curriculum_IBM.Partner",
                   "TIN_primary_curriculum_ISACA.Partner",
                   "TIN_primary_curriculum_ITIL..COBIT.and.SFIA",
                   "TIN_primary_curriculum_Java",
                   "TIN_primary_curriculum_Leadership.and.Professional.Development",
                   "TIN_primary_curriculum_Learning.Tree.Coaching",
                   "TIN_primary_curriculum_Microsoft.Office",
                   "TIN_primary_curriculum_Microsoft.Partner",
                   "TIN_primary_curriculum_Microsoft.Project",
                   "TIN_primary_curriculum_Mobile.App.Development",
                   "TIN_primary_curriculum_Networking",
                   "TIN_primary_curriculum_Not.Applicable",
                   "TIN_primary_curriculum_Nutanix.Partner",
                   "TIN_primary_curriculum_Oracle",
                   "TIN_primary_curriculum_Oracle.Partner",
                   "TIN_primary_curriculum_Palo.Alto.Partner",
                   "TIN_primary_curriculum_Perl",
                   "TIN_primary_curriculum_Programming.Fundamentals",
                   "TIN_primary_curriculum_Project.Management",
                   "TIN_primary_curriculum_Python",
                   "TIN_primary_curriculum_Red.Hat.Partner",
                   "TIN_primary_curriculum_Sales.Force.Partner",
                   "TIN_primary_curriculum_SAP.Partner",
                   "TIN_primary_curriculum_SharePoint",
                   "TIN_primary_curriculum_Software.Engineering",
                   "TIN_primary_curriculum_SQL.Server",
                   "TIN_primary_curriculum_TechData",
                   "TIN_primary_curriculum_UNIX.Linux",
                   "TIN_primary_curriculum_VMware.Partner",
                   "TIN_primary_curriculum_Web.Development",
                   "TIN_primary_curriculum_Windows",
                   "TIN_level_Foundation", "TIN_level_Intermediate",
                   "TIN_train_unit_EN", "TIN_train_unit_US",
                   "TIN_eve_type_VIRTUAL",
                   "TIN_attn_delivery_type_Virtual",
                   "TIN_practicearea_Leadership.and.Professional.Development",
                   "TIN_practicearea_Learning.Tree.Coaching",
                   "TIN_practicearea_Process.and.methodology",
                   "TIN_practicearea_Risk.and.Compliance")

crs$categoric <- "subpractice"

crs$target    <- "Return.within.1yr..Binary."
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("vertical", "revenue.type", "content_provider", "primary_curriculum", "level", "train_unit", "eve_type", "attn_delivery_type", "practicearea", "TIN_vertical_Commercial", "TIN_revenue.type_Passport", "TIN_level_Advanced", "TIN_train_unit_CN", "TIN_eve_type_HYBRID", "TIN_attn_delivery_type_In.Class", "TIN_practicearea_Hard.skills")
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2021-10-26 13:27:00 x86_64-w64-mingw32 

# Remap variables. 

# Turn a factor into indicator variables.

  crs$dataset[, make.names(paste("TIN_subpractice_", levels(crs$dataset[["subpractice"]]), sep=""))] <- diag(nlevels(crs$dataset[["subpractice"]]))[crs$dataset[["subpractice"]],]

#=======================================================================
# Rattle timestamp: 2021-10-26 13:27:01 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# The following variable selections have been noted.

crs$input     <- c("R01_relative_revenue_value",
                   "R01_event_crs_grade", "R01_event_inst_grade",
                   "R01_Month", "R01_Year", "R01_length_eve",
                   "R01_total", "R01_Courses.Taken",
                   "R01_Course.taken.in.order",
                   "R01_Return.s..within.after.one.year.of.course",
                   "TIN_vertical_Federal.Government",
                   "TIN_vertical_State.and.Local",
                   "TIN_revenue.type_Regular",
                   "TIN_revenue.type_Voucher",
                   "TIN_content_provider_Adobe.Partner",
                   "TIN_content_provider_Agile.Partner",
                   "TIN_content_provider_AWS.Partner",
                   "TIN_content_provider_Blockchain.Partner",
                   "TIN_content_provider_Cisco.Partner",
                   "TIN_content_provider_Citrix.Partner",
                   "TIN_content_provider_CompTIA.Partner",
                   "TIN_content_provider_Cyber.Management.Partner",
                   "TIN_content_provider_EC.Council.Partner",
                   "TIN_content_provider_F5.Partner",
                   "TIN_content_provider_IAPP.Partner",
                   "TIN_content_provider_IBM.Partner",
                   "TIN_content_provider_ISACA.Partner",
                   "TIN_content_provider_LTRE.Standard",
                   "TIN_content_provider_Microsoft.Partner",
                   "TIN_content_provider_Nutanix.Partner",
                   "TIN_content_provider_Oracle.Partner",
                   "TIN_content_provider_Palo.Alto.Partner",
                   "TIN_content_provider_Red.Hat.Partner",
                   "TIN_content_provider_Sales.Force.Partner",
                   "TIN_content_provider_SAP.Partner",
                   "TIN_content_provider_VMware.Partner",
                   "TIN_primary_curriculum_.NET.Development",
                   "TIN_primary_curriculum_Adobe.Partner",
                   "TIN_primary_curriculum_Agile",
                   "TIN_primary_curriculum_Agile.Partner",
                   "TIN_primary_curriculum_Apple.IBM.Enterprise",
                   "TIN_primary_curriculum_AWS.Partner",
                   "TIN_primary_curriculum_Azure",
                   "TIN_primary_curriculum_Big.Data",
                   "TIN_primary_curriculum_Blockchain.Partner",
                   "TIN_primary_curriculum_Business.Analysis",
                   "TIN_primary_curriculum_Cisco.Partner",
                   "TIN_primary_curriculum_Citrix.Partner",
                   "TIN_primary_curriculum_Cloud.Computing",
                   "TIN_primary_curriculum_CompTIA.Partner",
                   "TIN_primary_curriculum_Cyber.Management.Partner",
                   "TIN_primary_curriculum_Cyber.Security",
                   "TIN_primary_curriculum_Data.Science",
                   "TIN_primary_curriculum_EC.Council.Partner",
                   "TIN_primary_curriculum_F5.Partner",
                   "TIN_primary_curriculum_IAPP.Partner",
                   "TIN_primary_curriculum_IBM.Partner",
                   "TIN_primary_curriculum_ISACA.Partner",
                   "TIN_primary_curriculum_ITIL..COBIT.and.SFIA",
                   "TIN_primary_curriculum_Java",
                   "TIN_primary_curriculum_Leadership.and.Professional.Development",
                   "TIN_primary_curriculum_Learning.Tree.Coaching",
                   "TIN_primary_curriculum_Microsoft.Office",
                   "TIN_primary_curriculum_Microsoft.Partner",
                   "TIN_primary_curriculum_Microsoft.Project",
                   "TIN_primary_curriculum_Mobile.App.Development",
                   "TIN_primary_curriculum_Networking",
                   "TIN_primary_curriculum_Not.Applicable",
                   "TIN_primary_curriculum_Nutanix.Partner",
                   "TIN_primary_curriculum_Oracle",
                   "TIN_primary_curriculum_Oracle.Partner",
                   "TIN_primary_curriculum_Palo.Alto.Partner",
                   "TIN_primary_curriculum_Perl",
                   "TIN_primary_curriculum_Programming.Fundamentals",
                   "TIN_primary_curriculum_Project.Management",
                   "TIN_primary_curriculum_Python",
                   "TIN_primary_curriculum_Red.Hat.Partner",
                   "TIN_primary_curriculum_Sales.Force.Partner",
                   "TIN_primary_curriculum_SAP.Partner",
                   "TIN_primary_curriculum_SharePoint",
                   "TIN_primary_curriculum_Software.Engineering",
                   "TIN_primary_curriculum_SQL.Server",
                   "TIN_primary_curriculum_TechData",
                   "TIN_primary_curriculum_UNIX.Linux",
                   "TIN_primary_curriculum_VMware.Partner",
                   "TIN_primary_curriculum_Web.Development",
                   "TIN_primary_curriculum_Windows",
                   "TIN_level_Foundation", "TIN_level_Intermediate",
                   "TIN_train_unit_EN", "TIN_train_unit_US",
                   "TIN_eve_type_VIRTUAL",
                   "TIN_attn_delivery_type_Virtual",
                   "TIN_practicearea_Leadership.and.Professional.Development",
                   "TIN_practicearea_Learning.Tree.Coaching",
                   "TIN_practicearea_Process.and.methodology",
                   "TIN_practicearea_Risk.and.Compliance",
                   "TIN_subpractice_Infrastructure",
                   "TIN_subpractice_Software.Development",
                   "TIN_subpractice_User.Tools",
                   "TIN_subpractice_Vendor")

crs$numeric   <- c("R01_relative_revenue_value",
                   "R01_event_crs_grade", "R01_event_inst_grade",
                   "R01_Month", "R01_Year", "R01_length_eve",
                   "R01_total", "R01_Courses.Taken",
                   "R01_Course.taken.in.order",
                   "R01_Return.s..within.after.one.year.of.course",
                   "TIN_vertical_Federal.Government",
                   "TIN_vertical_State.and.Local",
                   "TIN_revenue.type_Regular",
                   "TIN_revenue.type_Voucher",
                   "TIN_content_provider_Adobe.Partner",
                   "TIN_content_provider_Agile.Partner",
                   "TIN_content_provider_AWS.Partner",
                   "TIN_content_provider_Blockchain.Partner",
                   "TIN_content_provider_Cisco.Partner",
                   "TIN_content_provider_Citrix.Partner",
                   "TIN_content_provider_CompTIA.Partner",
                   "TIN_content_provider_Cyber.Management.Partner",
                   "TIN_content_provider_EC.Council.Partner",
                   "TIN_content_provider_F5.Partner",
                   "TIN_content_provider_IAPP.Partner",
                   "TIN_content_provider_IBM.Partner",
                   "TIN_content_provider_ISACA.Partner",
                   "TIN_content_provider_LTRE.Standard",
                   "TIN_content_provider_Microsoft.Partner",
                   "TIN_content_provider_Nutanix.Partner",
                   "TIN_content_provider_Oracle.Partner",
                   "TIN_content_provider_Palo.Alto.Partner",
                   "TIN_content_provider_Red.Hat.Partner",
                   "TIN_content_provider_Sales.Force.Partner",
                   "TIN_content_provider_SAP.Partner",
                   "TIN_content_provider_VMware.Partner",
                   "TIN_primary_curriculum_.NET.Development",
                   "TIN_primary_curriculum_Adobe.Partner",
                   "TIN_primary_curriculum_Agile",
                   "TIN_primary_curriculum_Agile.Partner",
                   "TIN_primary_curriculum_Apple.IBM.Enterprise",
                   "TIN_primary_curriculum_AWS.Partner",
                   "TIN_primary_curriculum_Azure",
                   "TIN_primary_curriculum_Big.Data",
                   "TIN_primary_curriculum_Blockchain.Partner",
                   "TIN_primary_curriculum_Business.Analysis",
                   "TIN_primary_curriculum_Cisco.Partner",
                   "TIN_primary_curriculum_Citrix.Partner",
                   "TIN_primary_curriculum_Cloud.Computing",
                   "TIN_primary_curriculum_CompTIA.Partner",
                   "TIN_primary_curriculum_Cyber.Management.Partner",
                   "TIN_primary_curriculum_Cyber.Security",
                   "TIN_primary_curriculum_Data.Science",
                   "TIN_primary_curriculum_EC.Council.Partner",
                   "TIN_primary_curriculum_F5.Partner",
                   "TIN_primary_curriculum_IAPP.Partner",
                   "TIN_primary_curriculum_IBM.Partner",
                   "TIN_primary_curriculum_ISACA.Partner",
                   "TIN_primary_curriculum_ITIL..COBIT.and.SFIA",
                   "TIN_primary_curriculum_Java",
                   "TIN_primary_curriculum_Leadership.and.Professional.Development",
                   "TIN_primary_curriculum_Learning.Tree.Coaching",
                   "TIN_primary_curriculum_Microsoft.Office",
                   "TIN_primary_curriculum_Microsoft.Partner",
                   "TIN_primary_curriculum_Microsoft.Project",
                   "TIN_primary_curriculum_Mobile.App.Development",
                   "TIN_primary_curriculum_Networking",
                   "TIN_primary_curriculum_Not.Applicable",
                   "TIN_primary_curriculum_Nutanix.Partner",
                   "TIN_primary_curriculum_Oracle",
                   "TIN_primary_curriculum_Oracle.Partner",
                   "TIN_primary_curriculum_Palo.Alto.Partner",
                   "TIN_primary_curriculum_Perl",
                   "TIN_primary_curriculum_Programming.Fundamentals",
                   "TIN_primary_curriculum_Project.Management",
                   "TIN_primary_curriculum_Python",
                   "TIN_primary_curriculum_Red.Hat.Partner",
                   "TIN_primary_curriculum_Sales.Force.Partner",
                   "TIN_primary_curriculum_SAP.Partner",
                   "TIN_primary_curriculum_SharePoint",
                   "TIN_primary_curriculum_Software.Engineering",
                   "TIN_primary_curriculum_SQL.Server",
                   "TIN_primary_curriculum_TechData",
                   "TIN_primary_curriculum_UNIX.Linux",
                   "TIN_primary_curriculum_VMware.Partner",
                   "TIN_primary_curriculum_Web.Development",
                   "TIN_primary_curriculum_Windows",
                   "TIN_level_Foundation", "TIN_level_Intermediate",
                   "TIN_train_unit_EN", "TIN_train_unit_US",
                   "TIN_eve_type_VIRTUAL",
                   "TIN_attn_delivery_type_Virtual",
                   "TIN_practicearea_Leadership.and.Professional.Development",
                   "TIN_practicearea_Learning.Tree.Coaching",
                   "TIN_practicearea_Process.and.methodology",
                   "TIN_practicearea_Risk.and.Compliance",
                   "TIN_subpractice_Infrastructure",
                   "TIN_subpractice_Software.Development",
                   "TIN_subpractice_User.Tools",
                   "TIN_subpractice_Vendor")

crs$categoric <- NULL

crs$target    <- "Return.within.1yr..Binary."
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("vertical", "revenue.type", "content_provider", "primary_curriculum", "level", "train_unit", "eve_type", "attn_delivery_type", "practicearea", "subpractice", "TIN_vertical_Commercial", "TIN_revenue.type_Passport", "TIN_level_Advanced", "TIN_train_unit_CN", "TIN_eve_type_HYBRID", "TIN_attn_delivery_type_In.Class", "TIN_practicearea_Hard.skills", "TIN_subpractice_Data")
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2021-10-26 13:27:05 x86_64-w64-mingw32 

# CLEANUP the Dataset 

# Remove specific variables from the dataset.

crs$dataset$vertical <- NULL
crs$dataset$revenue.type <- NULL
crs$dataset$content_provider <- NULL
crs$dataset$primary_curriculum <- NULL
crs$dataset$level <- NULL
crs$dataset$train_unit <- NULL
crs$dataset$eve_type <- NULL
crs$dataset$attn_delivery_type <- NULL
crs$dataset$practicearea <- NULL
crs$dataset$subpractice <- NULL
crs$dataset$TIN_vertical_Commercial <- NULL
crs$dataset$TIN_revenue.type_Passport <- NULL
crs$dataset$TIN_level_Advanced <- NULL
crs$dataset$TIN_train_unit_CN <- NULL
crs$dataset$TIN_eve_type_HYBRID <- NULL
crs$dataset$TIN_attn_delivery_type_In.Class <- NULL
crs$dataset$TIN_practicearea_Hard.skills <- NULL
crs$dataset$TIN_subpractice_Data <- NULL

#=======================================================================
# Rattle timestamp: 2021-10-26 13:27:07 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# The following variable selections have been noted.

crs$input     <- c("R01_relative_revenue_value",
                   "R01_event_crs_grade", "R01_event_inst_grade",
                   "R01_Month", "R01_Year", "R01_length_eve",
                   "R01_total", "R01_Courses.Taken",
                   "R01_Course.taken.in.order",
                   "R01_Return.s..within.after.one.year.of.course",
                   "TIN_vertical_Federal.Government",
                   "TIN_vertical_State.and.Local",
                   "TIN_revenue.type_Regular",
                   "TIN_revenue.type_Voucher",
                   "TIN_content_provider_Adobe.Partner",
                   "TIN_content_provider_Agile.Partner",
                   "TIN_content_provider_AWS.Partner",
                   "TIN_content_provider_Blockchain.Partner",
                   "TIN_content_provider_Cisco.Partner",
                   "TIN_content_provider_Citrix.Partner",
                   "TIN_content_provider_CompTIA.Partner",
                   "TIN_content_provider_Cyber.Management.Partner",
                   "TIN_content_provider_EC.Council.Partner",
                   "TIN_content_provider_F5.Partner",
                   "TIN_content_provider_IAPP.Partner",
                   "TIN_content_provider_IBM.Partner",
                   "TIN_content_provider_ISACA.Partner",
                   "TIN_content_provider_LTRE.Standard",
                   "TIN_content_provider_Microsoft.Partner",
                   "TIN_content_provider_Nutanix.Partner",
                   "TIN_content_provider_Oracle.Partner",
                   "TIN_content_provider_Palo.Alto.Partner",
                   "TIN_content_provider_Red.Hat.Partner",
                   "TIN_content_provider_Sales.Force.Partner",
                   "TIN_content_provider_SAP.Partner",
                   "TIN_content_provider_VMware.Partner",
                   "TIN_primary_curriculum_.NET.Development",
                   "TIN_primary_curriculum_Adobe.Partner",
                   "TIN_primary_curriculum_Agile",
                   "TIN_primary_curriculum_Agile.Partner",
                   "TIN_primary_curriculum_Apple.IBM.Enterprise",
                   "TIN_primary_curriculum_AWS.Partner",
                   "TIN_primary_curriculum_Azure",
                   "TIN_primary_curriculum_Big.Data",
                   "TIN_primary_curriculum_Blockchain.Partner",
                   "TIN_primary_curriculum_Business.Analysis",
                   "TIN_primary_curriculum_Cisco.Partner",
                   "TIN_primary_curriculum_Citrix.Partner",
                   "TIN_primary_curriculum_Cloud.Computing",
                   "TIN_primary_curriculum_CompTIA.Partner",
                   "TIN_primary_curriculum_Cyber.Management.Partner",
                   "TIN_primary_curriculum_Cyber.Security",
                   "TIN_primary_curriculum_Data.Science",
                   "TIN_primary_curriculum_EC.Council.Partner",
                   "TIN_primary_curriculum_F5.Partner",
                   "TIN_primary_curriculum_IAPP.Partner",
                   "TIN_primary_curriculum_IBM.Partner",
                   "TIN_primary_curriculum_ISACA.Partner",
                   "TIN_primary_curriculum_ITIL..COBIT.and.SFIA",
                   "TIN_primary_curriculum_Java",
                   "TIN_primary_curriculum_Leadership.and.Professional.Development",
                   "TIN_primary_curriculum_Learning.Tree.Coaching",
                   "TIN_primary_curriculum_Microsoft.Office",
                   "TIN_primary_curriculum_Microsoft.Partner",
                   "TIN_primary_curriculum_Microsoft.Project",
                   "TIN_primary_curriculum_Mobile.App.Development",
                   "TIN_primary_curriculum_Networking",
                   "TIN_primary_curriculum_Not.Applicable",
                   "TIN_primary_curriculum_Nutanix.Partner",
                   "TIN_primary_curriculum_Oracle",
                   "TIN_primary_curriculum_Oracle.Partner",
                   "TIN_primary_curriculum_Palo.Alto.Partner",
                   "TIN_primary_curriculum_Perl",
                   "TIN_primary_curriculum_Programming.Fundamentals",
                   "TIN_primary_curriculum_Project.Management",
                   "TIN_primary_curriculum_Python",
                   "TIN_primary_curriculum_Red.Hat.Partner",
                   "TIN_primary_curriculum_Sales.Force.Partner",
                   "TIN_primary_curriculum_SAP.Partner",
                   "TIN_primary_curriculum_SharePoint",
                   "TIN_primary_curriculum_Software.Engineering",
                   "TIN_primary_curriculum_SQL.Server",
                   "TIN_primary_curriculum_TechData",
                   "TIN_primary_curriculum_UNIX.Linux",
                   "TIN_primary_curriculum_VMware.Partner",
                   "TIN_primary_curriculum_Web.Development",
                   "TIN_primary_curriculum_Windows",
                   "TIN_level_Foundation", "TIN_level_Intermediate",
                   "TIN_train_unit_EN", "TIN_train_unit_US",
                   "TIN_eve_type_VIRTUAL",
                   "TIN_attn_delivery_type_Virtual",
                   "TIN_practicearea_Leadership.and.Professional.Development",
                   "TIN_practicearea_Learning.Tree.Coaching",
                   "TIN_practicearea_Process.and.methodology",
                   "TIN_practicearea_Risk.and.Compliance",
                   "TIN_subpractice_Infrastructure",
                   "TIN_subpractice_Software.Development",
                   "TIN_subpractice_User.Tools",
                   "TIN_subpractice_Vendor")

crs$numeric   <- c("R01_relative_revenue_value",
                   "R01_event_crs_grade", "R01_event_inst_grade",
                   "R01_Month", "R01_Year", "R01_length_eve",
                   "R01_total", "R01_Courses.Taken",
                   "R01_Course.taken.in.order",
                   "R01_Return.s..within.after.one.year.of.course",
                   "TIN_vertical_Federal.Government",
                   "TIN_vertical_State.and.Local",
                   "TIN_revenue.type_Regular",
                   "TIN_revenue.type_Voucher",
                   "TIN_content_provider_Adobe.Partner",
                   "TIN_content_provider_Agile.Partner",
                   "TIN_content_provider_AWS.Partner",
                   "TIN_content_provider_Blockchain.Partner",
                   "TIN_content_provider_Cisco.Partner",
                   "TIN_content_provider_Citrix.Partner",
                   "TIN_content_provider_CompTIA.Partner",
                   "TIN_content_provider_Cyber.Management.Partner",
                   "TIN_content_provider_EC.Council.Partner",
                   "TIN_content_provider_F5.Partner",
                   "TIN_content_provider_IAPP.Partner",
                   "TIN_content_provider_IBM.Partner",
                   "TIN_content_provider_ISACA.Partner",
                   "TIN_content_provider_LTRE.Standard",
                   "TIN_content_provider_Microsoft.Partner",
                   "TIN_content_provider_Nutanix.Partner",
                   "TIN_content_provider_Oracle.Partner",
                   "TIN_content_provider_Palo.Alto.Partner",
                   "TIN_content_provider_Red.Hat.Partner",
                   "TIN_content_provider_Sales.Force.Partner",
                   "TIN_content_provider_SAP.Partner",
                   "TIN_content_provider_VMware.Partner",
                   "TIN_primary_curriculum_.NET.Development",
                   "TIN_primary_curriculum_Adobe.Partner",
                   "TIN_primary_curriculum_Agile",
                   "TIN_primary_curriculum_Agile.Partner",
                   "TIN_primary_curriculum_Apple.IBM.Enterprise",
                   "TIN_primary_curriculum_AWS.Partner",
                   "TIN_primary_curriculum_Azure",
                   "TIN_primary_curriculum_Big.Data",
                   "TIN_primary_curriculum_Blockchain.Partner",
                   "TIN_primary_curriculum_Business.Analysis",
                   "TIN_primary_curriculum_Cisco.Partner",
                   "TIN_primary_curriculum_Citrix.Partner",
                   "TIN_primary_curriculum_Cloud.Computing",
                   "TIN_primary_curriculum_CompTIA.Partner",
                   "TIN_primary_curriculum_Cyber.Management.Partner",
                   "TIN_primary_curriculum_Cyber.Security",
                   "TIN_primary_curriculum_Data.Science",
                   "TIN_primary_curriculum_EC.Council.Partner",
                   "TIN_primary_curriculum_F5.Partner",
                   "TIN_primary_curriculum_IAPP.Partner",
                   "TIN_primary_curriculum_IBM.Partner",
                   "TIN_primary_curriculum_ISACA.Partner",
                   "TIN_primary_curriculum_ITIL..COBIT.and.SFIA",
                   "TIN_primary_curriculum_Java",
                   "TIN_primary_curriculum_Leadership.and.Professional.Development",
                   "TIN_primary_curriculum_Learning.Tree.Coaching",
                   "TIN_primary_curriculum_Microsoft.Office",
                   "TIN_primary_curriculum_Microsoft.Partner",
                   "TIN_primary_curriculum_Microsoft.Project",
                   "TIN_primary_curriculum_Mobile.App.Development",
                   "TIN_primary_curriculum_Networking",
                   "TIN_primary_curriculum_Not.Applicable",
                   "TIN_primary_curriculum_Nutanix.Partner",
                   "TIN_primary_curriculum_Oracle",
                   "TIN_primary_curriculum_Oracle.Partner",
                   "TIN_primary_curriculum_Palo.Alto.Partner",
                   "TIN_primary_curriculum_Perl",
                   "TIN_primary_curriculum_Programming.Fundamentals",
                   "TIN_primary_curriculum_Project.Management",
                   "TIN_primary_curriculum_Python",
                   "TIN_primary_curriculum_Red.Hat.Partner",
                   "TIN_primary_curriculum_Sales.Force.Partner",
                   "TIN_primary_curriculum_SAP.Partner",
                   "TIN_primary_curriculum_SharePoint",
                   "TIN_primary_curriculum_Software.Engineering",
                   "TIN_primary_curriculum_SQL.Server",
                   "TIN_primary_curriculum_TechData",
                   "TIN_primary_curriculum_UNIX.Linux",
                   "TIN_primary_curriculum_VMware.Partner",
                   "TIN_primary_curriculum_Web.Development",
                   "TIN_primary_curriculum_Windows",
                   "TIN_level_Foundation", "TIN_level_Intermediate",
                   "TIN_train_unit_EN", "TIN_train_unit_US",
                   "TIN_eve_type_VIRTUAL",
                   "TIN_attn_delivery_type_Virtual",
                   "TIN_practicearea_Leadership.and.Professional.Development",
                   "TIN_practicearea_Learning.Tree.Coaching",
                   "TIN_practicearea_Process.and.methodology",
                   "TIN_practicearea_Risk.and.Compliance",
                   "TIN_subpractice_Infrastructure",
                   "TIN_subpractice_Software.Development",
                   "TIN_subpractice_User.Tools",
                   "TIN_subpractice_Vendor")

crs$categoric <- NULL

crs$target    <- "Return.within.1yr..Binary."
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2021-10-26 13:27:25 x86_64-w64-mingw32 

# Build a Random Forest model using the traditional approach.

set.seed(crv$seed)

crs$rf <- randomForest::randomForest(Return.within.1yr..Binary. ~ .,
  data=crs$dataset[crs$train, c(crs$input, crs$target)], 
  ntree=500,
  mtry=10,
  importance=TRUE,
  na.action=na.omit,
  replace=FALSE)

# Generate textual output of the 'Random Forest' model.

crs$rf

# The `pROC' package implements various AUC functions.

# Calculate the Area Under the Curve (AUC).

pROC::roc(crs$rf$y, as.numeric(crs$rf$predicted))

# Calculate the AUC Confidence Interval.

pROC::ci.auc(crs$rf$y, as.numeric(crs$rf$predicted))FALSE

# List the importance of the variables.

rn <- round(randomForest::importance(crs$rf), 2)
rn[order(rn[,3], decreasing=TRUE),]

# Time taken: 1.26 mins

# Plot the error rate against the number of trees.

plot(crs$rf, main="")
legend("topright", c("OOB", "No", "Yes"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest 2017Data.csv",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#=======================================================================
# Rattle timestamp: 2021-10-26 13:29:34 x86_64-w64-mingw32 

# Build a Random Forest model using the traditional approach.

set.seed(crv$seed)

crs$rf <- randomForest::randomForest(Return.within.1yr..Binary. ~ .,
  data=crs$dataset[crs$train, c(crs$input, crs$target)], 
  ntree=200,
  mtry=10,
  importance=TRUE,
  na.action=na.omit,
  replace=FALSE)

# Generate textual output of the 'Random Forest' model.

crs$rf

# The `pROC' package implements various AUC functions.

# Calculate the Area Under the Curve (AUC).

pROC::roc(crs$rf$y, as.numeric(crs$rf$predicted))

# Calculate the AUC Confidence Interval.

pROC::ci.auc(crs$rf$y, as.numeric(crs$rf$predicted))FALSE

# List the importance of the variables.

rn <- round(randomForest::importance(crs$rf), 2)
rn[order(rn[,3], decreasing=TRUE),]

# Time taken: 30.61 secs
