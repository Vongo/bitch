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

avgLength.by.hourInDay <- function(data, timeCol,...) {
	zzz <- HOURS[as.POSIXlt(strptime(data[,timeCol],format='%d/%m/%Y %H:%M'))$hour+1]
	datab <- cbind(data,zzz)
	avg <- vector()
	for (h in HOURS)
		avg <- c(avg,mean(datab[datab$zzz==h,"Length"]))
	# names(avg) <- sapply(HOURS,function(x) paste(x,"h",sep=""))
	names(avg) <- HOURS
	avg[is.nan(avg)] <- 0
	avg
}

avgLength.by.dayInMonth <- function(data, timeCol,...) {
	zzz <- MDAYS[as.POSIXlt(strptime(data[,timeCol],format='%d/%m/%Y %H:%M'))$mday]
	datab <- cbind(data,zzz)
	avg <- vector()
	for (d in MDAYS)
		avg <- c(avg,mean(datab[datab$zzz==d,"Length"]))
	names(avg) <- MDAYS
	avg[is.nan(avg)] <- 0
	avg
}

avgLength.by.dayInWeek <- function(data, timeCol,...) {
	zzz <- WDAYS[as.POSIXlt(strptime(data[,timeCol],format='%d/%m/%Y %H:%M'))$wday+1]
	datab <- cbind(data,zzz)
	avg <- vector()
	for (d in WDAYS)
		avg <- c(avg,mean(datab[datab$zzz==d,"Length"]))
	names(avg) <- WDAYS
	avg[is.nan(avg)] <- 0
	avg
}

avgLength.by.monthInYear <- function(data, timeCol,...) {
	zzz <- MONTHS[as.POSIXlt(strptime(data[,timeCol],format='%d/%m/%Y %H:%M'))$mon+1]
	datab <- cbind(data,zzz)
	avg <- vector()
	for (m in OMONTHS)
		avg <- c(avg,mean(datab[datab$zzz==m,"Length"]))
	names(avg) <- OMONTHS
	avg[is.nan(avg)] <- 0
	avg
}

# Assumes that schema(data) contains (timeCol) as a (character) column formatted like ('%d/%m/%Y %H:%M')
aggreg.by.hourInDay <- function(data, timeCol,...) {
	vectorize.factor(factor(HOURS[as.POSIXlt(strptime(data[,timeCol],format='%d/%m/%Y %H:%M'))$hour+1],levels=HOURS))
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

# Calculates the ratio x/y
# If y is close or equal to 0, the ratio won't diverge to infinite value but be equal to 10.
ratio <- function(x,y) {
	sapply(x/y,function(x) ifelse(is.finite(x), ifelse(x<10, x, 10), 0))
}

# the variables "by.x" are pointers to the functions "img.by.x" and "aggreg.by.x"
# these couples of functions can be accessed as list elements, e.g. imgByX <- by.x[[1]]
by.hourInDay <- c(img.by.hourInDay,aggreg.by.hourInDay,avgLength.by.hourInDay)
by.dayInMonth <- c(img.by.dayInMonth,aggreg.by.dayInMonth,avgLength.by.dayInMonth)
by.dayInWeek <- c(img.by.dayInWeek,aggreg.by.dayInWeek,avgLength.by.dayInWeek)
by.monthInYear <- c(img.by.monthInYear,aggreg.by.monthInYear,avgLength.by.monthInYear)

# Many things to say here.
stats.by.actor.detail <- function() {

	## WARNING : this function is context-dependant
	images <- function(fun,path,...) {
		dir.create("./pic/",showWarnings=F)
		dir.create("./pic/count/",showWarnings=F)
		dir.create("./pic/length/",showWarnings=F)
		dir.create(paste("./pic/count/",path,sep=""),showWarnings=F)
		dir.create(paste("./pic/length/",path,sep=""),showWarnings=F)

		# Global stats
		img <- fun[[1]]
		aggreg <- fun[[2]]
		avgLength <- fun[[3]]

		for (actor in c(ACTORS,BITCH)) {
			dRcv <- aggreg(messages[messages$Target==actor,],"DateTime",...)
			dSnt <- aggreg(messages[messages$Source==actor,],"DateTime",...)
			pRcv <- img(dRcv,"Received",...)
			pSnt <- img(dSnt,"Sent",...)
			dRpt <- ratio(dRcv,dSnt)
			pRpt <- img(dRpt,"Received/Sent")

			png(file=paste("./pic/count/",path,"/",actor,".png",sep=""),width=756,height=756)
			multiplot(pRcv,pSnt,pRpt,cols=1)
			dev.off()

			dRcv <- avgLength(lengths[lengths$Target==actor,],"DateTime",...)
			dSnt <- avgLength(lengths[lengths$Source==actor,],"DateTime",...)
			pRcv <- img(dRcv,"Received",yLeg="avg Length",...)
			pSnt <- img(dSnt,"Sent",yLeg="avg Length",...)
			dRpt <- ratio(dRcv,dSnt)
			pRpt <- img(dRpt,"Received/Sent",yLeg="avg Length")

			png(file=paste("./pic/length/",path,"/",actor,".png",sep=""),width=756,height=756)
			multiplot(pRcv,pSnt,pRpt,cols=1)
			dev.off()
		}
	}

	messages <- read.csv("CLEAN_ANONYMOUS.csv",stringsAsFactors=F)

	lengths <- data.frame(matrix(vector(), nrow(messages), 4, dimnames=list(c(), c("Source","Target","DateTime","Length"))), stringsAsFactors=F)
	lengths[,1:3] <- messages[,1:3]
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
	if(VERBOSE) cat(paste("It's all done.\n"))
}