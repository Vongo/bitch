require(ggplot2)
require(scales)

# source("./format.R",chdir=T)
# source("./OPERATORS.R",chdir=T)
source("./CONSTANTS.R",chdir=T)
source("./display.R",chdir=T)

# Aggregates the content of a given factor, counts occurrences for each level, puts the result into a numeric vector
vectorize.factor <- function(v) {
	x <- summary(v)
	y <- as.numeric(x)
	names(y) <- names(x)
	y
}


avgLength.by.hourInDay <- function(data, lengthCol,...) {
	vectorize.factor(factor(HOURS[as.POSIXlt(strptime(data[,timeCol],format='%d/%m/%Y %H:%M'))$hour],levels=HOURS))	
}

avgLength.by.dayInMonth <- function(data, lengthCol,...) {

}

avgLength.by.dayInWeek <- function(data, lengthCol,...) {

}

avgLength.by.monthInYear <- function(data, lengthCol,...) {

}

avgLength.by.time <- function(data, lengthCol,...) {

}


# Assumes that schema(data) contains (timeCol) as a (character) column formatted like ('%d/%m/%Y %H:%M')
aggreg.by.hourInDay <- function(data, timeCol,...) {
	vectorize.factor(factor(HOURS[as.POSIXlt(strptime(data[,timeCol],format='%d/%m/%Y %H:%M'))$hour],levels=HOURS))
}

# Assumes that schema(data) contains (timeCol) as a (character) column formatted like ('%d/%m/%Y %H:%M')
aggreg.by.dayInMonth <- function(data, timeCol,...) {
	vectorize.factor(factor(MDAYS[as.POSIXlt(strptime(data[,timeCol],format='%d/%m/%Y %H:%M'))$mday],levels=MDAYS))
}

# Assumes that schema(data) contains (timeCol) as a (character) column formatted like ('%d/%m/%Y %H:%M')
aggreg.by.dayInWeek <- function(data, timeCol,...) {
	vectorize.factor(factor(WDAYS[as.POSIXlt(strptime(data[,timeCol],format='%d/%m/%Y %H:%M'))$wday + 1],levels=WDAYS))
}

# Assumes that schema(data) contains (timeCol) as a (character) column formatted like ('%d/%m/%Y %H:%M')
aggreg.by.monthInYear <- function(data, timeCol,...) {
	vectorize.factor(factor(MONTHS[as.POSIXlt(strptime(data[,timeCol],format='%d/%m/%Y %H:%M'))$mon + 1],levels=OMONTHS))
}

# Assumes that schema(data) contains (timeCol) as a (character) column formatted like ('%d/%m/%Y %H:%M')
aggreg.by.time <- function(data, timeCol,...) {
	as.POSIXct(strptime(data[,timeCol],format='%d/%m/%Y %H:%M'))
}

# Frequency of mail transactions by hour in day (range 1-24)
# Assumes that data is a vector already aggregated
img.by.hourInDay <- function(data, ttl, yLeg = "Count") {
	qplot(x=as.numeric(names(data)),y=data[1:24],stat="identity",geom="histogram") + xlab("Time") + labs(title = ttl) + ylab(yLeg)
}

# Frequency of mail transactions by day in Month (range 1-31)
# Assumes that data is a vector already aggregated
img.by.dayInMonth <- function(data, ttl, yLeg = "Count") {
	qplot(x=as.numeric(names(data)),y=data[1:31],stat="identity",geom="histogram") + xlab("Day") + labs(title = ttl) + ylab(yLeg)
}

# Frequency of mail transactions by day in week (range Sunday-Saturday)
# Assumes that data is a vector already aggregated
img.by.dayInWeek <- function(data, ttl, yLeg = "Count") {
	# We're killing ants with a tank.
	qplot(x=factor(WDAYS, levels=WDAYS),y=data[1:7],stat="identity",geom="histogram") + xlab("Day") + labs(title = ttl) + ylab(yLeg)
}

# Frequency of mail transactions by month in year (range jan - dec)
# Assumes that data is a vector already aggregated
img.by.monthInYear <- function(data, ttl, yLeg = "Count") {
	qplot(x=factor(OMONTHS,levels=OMONTHS),y=data[1:12],stat="identity",geom="histogram") + xlab("Month") + labs(title = ttl) + ylab(yLeg)
}

# Absolute frequency of mail transactions for all data (auto scale)
# Assumes that data is a vector already aggregated
# Avoid this function, still buggy
img.by.time <- function(data, ttl, yLeg = "Count") {
	qplot(data) + xlab("Time slot") + labs(title = ttl) +  + ylab(yLeg)
		scale_x_datetime(labels = date_format("%m"),breaks = date_breaks("6 month"),minor_breaks = date_breaks("1 month"))
}

# Calculates the ratio x/y
# If y is close or equal to 0, the ratio won't diverge to infinite value but be equal to 10.
ratio <- function(x,y) {
	sapply(x/y,function(x) ifelse(is.finite(x), ifelse(x<10, x, 10), 0))
}

# the variables "by.x" are pointers to the functions "img.by.x" and "aggreg.by.x"
# these couples of functions can be accessed as list elements, e.g. imgByX <- by.x[[1]]
by.hourInDay <- c(img.by.hourInDay,aggreg.by.hourInDay)
by.dayInMonth <- c(img.by.dayInMonth,aggreg.by.dayInMonth)
by.dayInWeek <- c(img.by.dayInWeek,aggreg.by.dayInWeek)
by.monthInYear <- c(img.by.monthInYear,aggreg.by.monthInYear)
by.time <- c(img.by.time,aggreg.by.time)


# Many things to say here.
stats.by.actor.detail <- function() {

	## WARNING : this function is context-dependant
	images <- function(fun,path,...) {
		# Global stats
		img <- fun[[1]]
		aggreg <- fun[[2]]
		dRcv <- aggreg(messages[messages$Target==BITCH,],"DateTime",...)
		dSnt <- aggreg(messages[messages$Source==BITCH,],"DateTime",...)
		pRcv <- img(dRcv,"Received",...)
		pSnt <- img(dSnt,"Sent",...)
		dRpt <- ratio(dRcv,dSnt)
		pRpt <- img(dRpt,"Received/Sent")

		dir.create(paste("./pic/",path,sep=""),showWarnings=F)
		png(file=paste("./pic/",path,"/Global.png",sep=""),width=756,height=756)
		multiplot(pRcv,pSnt,pRpt,cols=1)
		dev.off()

		# Actors
		for (actor in ACTORS) {
			dRcv <- aggreg(messages[messages$Source==actor,],"DateTime",...)
			dSnt <- aggreg(messages[messages$Target==actor,],"DateTime",...)
			pRcv <- img(dRcv,"Received",...)
			pSnt <- img(dSnt,"Sent",...)
			dRpt <- ratio(dRcv,dSnt)
			pRpt <- img(dRpt,"Received/Sent")

			png(file=paste("./pic/",path,"/",actor,".png",sep=""),width=756,height=756)
			multiplot(pRcv,pSnt,pRpt,cols=1)
			dev.off()
		}
	}

	messages <- read.csv("CLEAN_ANONYMOUS.csv",stringsAsFactors=F)

	lengths <- data.frame(matrix(vector(), nrow(messages), 4, dimnames=list(c(), c("Source","Target","DateTime","Length"))), stringsAsFactors=F)
	lengths[,1:3] <- messages[,2:4]
	lengths[,4] <- sapply(messages$Content,function(x) nchar(x))

	if(VERBOSE) cat(paste("Data is clean and tidy.\n"))
	# Could generate a lot of warnings, but it is ok. Shouldn't though.
	if(VERBOSE) cat(paste("Charts : time (hour in the day)...\n"))
	images(by.hourInDay,"HourInDay")
	if(VERBOSE) cat(paste("Charts : day in the week...\n"))
	images(by.dayInWeek,"DayInWeek")
	if(VERBOSE) cat(paste("Charts : day in the month...\n"))
	images(by.dayInMonth,"DayInMonth")
	if(VERBOSE) cat(paste("Charts : month...\n"))
	images(by.monthInYear,"MonthInYear")
	# Except this one : lot of exceptions and (yet) unhandled cases
	cat(paste("Charts : global...\n"))
	images(by.time,"General")
	if(VERBOSE) cat(paste("It's all done.\n"))
}