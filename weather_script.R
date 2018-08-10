#============================================================

# Rattle is Copyright (c) 2006-2017 Togaware Pty Ltd.
# It is open source software and is freely available.
# It is licensed under the GNU General Public License,
# Version 2. Rattle comes with ABSOLUTELY NO WARRANTY.
# Rattle was written by Graham Williams with contributions
# from others as acknowledged in 'library(help=rattle)'.
# Visit https://rattle.togaware.com/ for details.

#============================================================
# Rattle timestamp: 2017-11-18 19:38:46 x86_64-w64-mingw32 

# Rattle version 5.1.0 user 'PAPPAL'

# This log captures Rattle interactions as an R script. 

# For repeatability export this log of all activity to a 
# file using the Export button or the Tools menu. This 
# script can serve as a starting point for developing your 
# own scripts. Exporting to a file called 'model.R' will 
# allow you to type into a new R Console the command 
#"source('model.R')" and so repeat all actions. Generally, 
# you will want to edit the file to suit your own needs. 
# You can also edit this log in place to record additional 
# information before exporting the script. 
 
# Note that saving/loading projects retains this log.

# We begin most scripts by loading the required packages.
# Here are some initial packages to load and others will be
# identified as we proceed through the script. When writing
# our own scripts we often collect together the library
# commands at the beginning of the script here.

library(rattle)   # Access weather dataset and utilities.
library(magrittr) # For the %>% and %<>% pipeline operators.

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

#============================================================
# Rattle timestamp: 2017-11-18 22:30:24 x86_64-w64-mingw32 

# Load the dataset from file.

fname <- system.file("csv", "weather.csv", package="rattle") 
crs$dataset <- read.csv(fname, encoding="UTF-8")

#============================================================
# Rattle timestamp: 2017-11-18 22:30:28 x86_64-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=366 train=256 validate=54 test=56

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("MinTemp", "MaxTemp", "Rainfall", "Evaporation",
                   "Sunshine", "WindGustDir", "WindGustSpeed", "WindDir9am",
                   "WindDir3pm", "WindSpeed9am", "WindSpeed3pm", "Humidity9am",
                   "Humidity3pm", "Pressure9am", "Pressure3pm", "Cloud9am",
                   "Cloud3pm", "Temp9am", "Temp3pm", "RainToday")

crs$numeric   <- c("MinTemp", "MaxTemp", "Rainfall", "Evaporation",
                   "Sunshine", "WindGustSpeed", "WindSpeed9am", "WindSpeed3pm",
                   "Humidity9am", "Humidity3pm", "Pressure9am", "Pressure3pm",
                   "Cloud9am", "Cloud3pm", "Temp9am", "Temp3pm")

crs$categoric <- c("WindGustDir", "WindDir9am", "WindDir3pm", "RainToday")

crs$target    <- "RainTomorrow"
crs$risk      <- "RISK_MM"
crs$ident     <- "Date"
crs$ignore    <- "Location"
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2017-11-18 22:30:35 x86_64-w64-mingw32 

# Decision Tree 

# The 'rpart' package provides the 'rpart' function.

library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

crs$rpart <- rpart(RainTomorrow ~ .,
    data=crs$dataset[crs$train, c(crs$input, crs$target)],
    method="class",
    parms=list(split="information"),
    control=rpart.control(usesurrogate=0, 
        maxsurrogate=0))

# Generate a textual view of the Decision Tree model.

print(crs$rpart)
printcp(crs$rpart)
cat("\n")

# Time taken: 0.36 secs

#============================================================
# Rattle timestamp: 2017-11-18 22:32:35 x86_64-w64-mingw32 

# Plot the resulting Decision Tree. 

# We use the maptools support functions.

drawTreeNodes(crs$rpart)
title(main="Decision Tree weather.csv $ RainTomorrow",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#============================================================
# Rattle timestamp: 2017-11-18 22:32:54 x86_64-w64-mingw32 

# Plot the resulting Decision Tree. 

# We use the maptools support functions.

drawTreeNodes(crs$rpart)
title(main="Decision Tree weather.csv $ RainTomorrow",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#============================================================
# Rattle timestamp: 2017-11-18 22:33:35 x86_64-w64-mingw32 

# Plot the resulting Decision Tree. 

# We use the maptools support functions.

drawTreeNodes(crs$rpart)
title(main="Decision Tree weather.csv $ RainTomorrow",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#============================================================
# Rattle timestamp: 2017-11-18 22:33:57 x86_64-w64-mingw32 

# Plot the resulting Decision Tree. 

# We use the maptools support functions.

drawTreeNodes(crs$rpart)
title(main="Decision Tree weather.csv $ RainTomorrow",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#============================================================
# Rattle timestamp: 2017-11-18 22:34:15 x86_64-w64-mingw32 

# Decision Tree 

# The 'rpart' package provides the 'rpart' function.

library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

crs$rpart <- rpart(RainTomorrow ~ .,
    data=crs$dataset[crs$train, c(crs$input, crs$target)],
    method="class",
    parms=list(split="information"))

# Generate a textual view of the Decision Tree model.

print(crs$rpart)
printcp(crs$rpart)
cat("\n")

# Time taken: 0.09 secs

#============================================================
# Rattle timestamp: 2017-11-18 22:47:09 x86_64-w64-mingw32 

# Plot the resulting Decision Tree. 

# We use the maptools support functions.

drawTreeNodes(crs$rpart)
title(main="Decision Tree weather.csv $ RainTomorrow",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#============================================================
# Rattle timestamp: 2017-11-18 22:48:53 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# Generate an Error Matrix for the Decision Tree model.

# Obtain the response from the Decision Tree model.

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)],
    type="class")

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$validate, c(crs$input, crs$target)]$RainTomorrow, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$validate, c(crs$input, crs$target)]$RainTomorrow, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================
# Rattle timestamp: 2017-11-18 22:54:27 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# Generate a risk chart.

# Rattle provides evaluateRisk() and plotRisk().

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)])[,2]

crs$eval <- evaluateRisk(crs$pr, 
    crs$dataset[crs$validate, c(crs$input, crs$target)]$RainTomorrow, 
    crs$dataset[crs$validate, c(crs$input, crs$target, crs$risk)]$RISK_MM)
plotRisk(crs$eval$Caseload, crs$eval$Precision, crs$eval$Recall, crs$eval$Risk,
    risk.name="RISK_MM", recall.name="RainTomorrow", show.lift=TRUE, show.precision=TRUE)
title(main="Risk Chart Decision Tree weather.csv [validate] RISK_MM",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#============================================================
# Rattle timestamp: 2017-11-18 22:55:30 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# Generate an ROC Curve for the rpart model on weather.csv [validate].

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)])[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]$RainTomorrow)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "tpr", "fpr"), col="#CC0000FF", lty=1, add=FALSE)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]$RainTomorrow)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
performance(pred, "auc")

# Add a legend to the plot.

legend("bottomright", c("rpart"), col=rainbow(1, 1, .8), lty=1:1, title="Models", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="ROC Curve  weather.csv [validate]",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

#============================================================
# Rattle timestamp: 2017-11-18 23:00:31 x86_64-w64-mingw32 

# Score the validation dataset. 

# Obtain probability scores for the Decision Tree model on weather.csv [validate].

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$validate, c(crs$input)],
    type="class")

# Extract the relevant variables from the dataset.

sdata <- subset(crs$dataset[crs$validate,], select=c("Date", "RainTomorrow"))

# Output the combined data.

write.csv(cbind(sdata, crs$pr), file="C:\Users\PAPPAL\Desktop\weather_validate_score_idents.csv", row.names=FALSE)
