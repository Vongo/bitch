expeditors <- data.frame(matrix(vector(), 0, 5, dimnames=list(c(), c("Object","Source","Target","DateTime","Content"))), stringsAsFactors=F)
write.table(expeditors,"data.csv",
	append = FALSE, quote = TRUE, sep = ",",
	eol = "\n", na = "NA", dec = ".", row.names = FALSE,
	col.names = TRUE, qmethod = c("escape", "double"),
	fileEncoding = "")

justName <- function(x) {
	tmp <- strsplit(x,"<")[[1]][1]
	ifelse(substr(tmp,nchar(tmp),nchar(tmp))==" ", substr(tmp,1,nchar(tmp)-1), tmp)
}

sink("data.csv",append=T)
con  <- file("sms.csv", open = "r")
endedLine <- T
buffer <- ""
while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
	if(endedLine){
		cat(paste(buffer,"\n",sep=""))
		buffer <- oneLine
	} else {
		buffer <- paste(buffer,oneLine)
	}
	end <- nchar(oneLine)
	endedLine <- substr(oneLine,end,end) == "\""
}
close(con)
sink()

data <- read.csv("data.csv", sep = ",", header = T, stringsAsFactors = F)

data$Source <- sapply(data$Source,justName)
data$Target <- sapply(data$Target,justName)

write.table(data,"final.csv",
	append = FALSE, quote = TRUE, sep = ",",
	eol = "\n", na = "NA", dec = ".", row.names = FALSE,
	col.names = TRUE, qmethod = c("escape", "double"),
	fileEncoding = "")
