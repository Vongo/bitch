


messages <- read.csv("CLEAN_ANONYMOUS.csv",stringsAsFactors=F)

lengths <- data.frame(matrix(vector(), nrow(messages), 4, dimnames=list(c(), c("Source","Target","DateTime","Length"))), stringsAsFactors=F)
lengths[,1:3] <- messages[,2:4]
lengths[,4] <- sapply(messages$Content,function(x) nchar(x))

for (a in ACTORS) {
	inc <- lengths[lengths$Target == a,]
	out <- lengths[lengths$Source == a,]

}


zzz <- HOURS[as.POSIXlt(strptime(data[,timeCol],format='%d/%m/%Y %H:%M'))$hour+1]
datab <- cbind(lengths,zzz)

lab <- paste(h,"h",sep="")

mns <- vector()
for (h in HOURS) {
	mns <- c(mns,mean(datab[datab$zzz==h,"Length"]))
}
names(mns) <- sapply(HOURS,function(x) paste(x,"h",sep=""))
