# Regression & Correlation
# Feb 12 (lecture only, no notes), 13 and 15

################################################################

# Regression & Correlation Problem Solving and Discussion

# REGRESSION/CORRELATION
# ms.lm <- lm(Exam ~ Quiz, data = ms) ms.lm, dependent variable first, then independent
    # output: coefficients - alpha (intercept) and beta (coefficient of variation) (how much is explained)
    # summary() for p values and error of coefficients 
    # can call residuals using $residuals

# class exercise: analyze summary statistics for regression set.csv
# set working directory
setwd("/Users/caseymeili/Downloads")

# import data
dset <- read.csv("regression set.csv")
dset2 <- data.frame(dset$x2, dset$y2)

# mean
mean(dset$x2) # 54.26327
mean(dset$y2) # 47.83225

# variance
var(dset$x2) # 281.07
var(dset$y2) # 725.516

# standard deviation
sd(dset$x2) # 16.76514
sd(dset$y2) # 26.9354

# calculate a and b with the y value as the dependent variable
lm <- lm(dset$y2 ~ dset$x2, data = dset) # intercept (alpha) = 53.4530, beta = -0.1036 

# plot 
plot(dset$x2, dset$y2) # all data sets have the same summary statistics but look completely different when plotted

# plot residuals
dset$yval <- 1:142
plot(x = lm$residuals, y = dset$yval)


# class exercise: plot datasauRus data
library(datasauRus)
    # datasaurus_dozen - 1846 rows
    # datasaurus_dozen_wide - 26 columns of 142 rows

plot(datasaurus_dozen_wide$bullseye_x, datasaurus_dozen_wide$bullseye_y)
plot(datasaurus_dozen_wide$dots_x, datasaurus_dozen_wide$dots_y)

# PLOTTING WITH GGPLOT2
# ggplot2(data, aesthetics) + geometry(s)
library(ggplot2)

################################################################

# Team Based Learning (TBL) - Regression & Correlation

# GAE 3B
# set working directory
setwd("/Users/caseymeili/Downloads")

# Rread the csv file in and name it Wormdata
Wormdata <- read.csv("WormTempLoc.csv")

# create a scatterplot of the data
plot(Wormdata$Temp, Wormdata$Bends)

# linear regression model
lm_model <- lm(Wormdata$Bends ~ Wormdata$Temp, data = Wormdata)

# summary of the regression model
summary(lm_model)

# plot residuals
plot(Wormdata$Temp, lm_model$residuals)


