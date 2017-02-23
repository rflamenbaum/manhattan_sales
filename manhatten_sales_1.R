# Author: Benjamin Reddy
# Taken from pages 49-50 of O'Neil and Schutt

#require(gdata)
#require(plyr) #Added by Monnie McGee
#Brooklyn references changed to Manhattan by Robert Flamenbaum
#install the gdata and plyr packages and load in to R.
library(plyr)
library(gdata)




# So, save the file as a csv and use read.csv instead
mn <- read.csv("rollingsales_manhattan.csv",skip=4,header=TRUE)

## Check the data
head(mn)
summary(mn)
str(mn) # Very handy function!

## clean/format the data with regular expressions
## More on these later. For now, know that the
## pattern "[^[:digit:]]" refers to members of the variable name that
## start with digits. We use the gsub command to replace them with a blank space.
# We create a new variable that is a "clean' version of sale.price.
# And sale.price.n is numeric, not a factor.
mn$SALE.PRICE.N <- as.numeric(gsub("[^[:digit:]]","", mn$SALE.PRICE))
count(is.na(mn$SALE.PRICE.N))

names(mn) <- tolower(names(mn)) # make all variable names lower case
## Get rid of leading digits
mn$gross.sqft <- as.numeric(gsub("[^[:digit:]]","", mn$gross.square.feet))
mn$land.sqft <- as.numeric(gsub("[^[:digit:]]","", mn$land.square.feet))
mn$year.built <- as.numeric(as.character(mn$year.built))

## do a bit of exploration to make sure there's not anything
## weird going on with sale prices
attach(mn)
hist(sale.price.n) 
detach(mn)

## keep only the actual sales

mn.sale <- mn[mn$sale.price.n!=0,]
plot(mn.sale$gross.sqft,mn.sale$sale.price.n)
plot(log10(mn.sale$gross.sqft),log10(mn.sale$sale.price.n))

## for now, let's look at 1-, 2-, and 3-family homes
mn.homes <- mn.sale[which(grepl("FAMILY",mn.sale$building.class.category)),]
dim(mn.homes)
plot(log10(mn.homes$gross.sqft),log10(mn.homes$sale.price.n))
summary(mn.homes[which(mn.homes$sale.price.n<100000),])


## remove outliers that seem like they weren't actual sales
mn.homes$outliers <- (log10(mn.homes$sale.price.n) <=5) + 0
mn.homes <- mn.homes[which(mn.homes$outliers==0),]
plot(log10(mn.homes$gross.sqft),log10(mn.homes$sale.price.n))