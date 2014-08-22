
source("CONSTANTS.R")

remove.symbols <- function(x) {

}

root <- function(x) {

}

transform <- function(x,s=NA,t=NA) {
	df <- data.frame(matrix(vector(), 99, 3, dimnames=list(c(), c("Source","Target","Strength"))), stringsAsFactors=F)
	su <- summary(as.factor(x))
	if(is.na(t)){
		df$Source <- rep(s,99)
		df$Target <- names(su)[1:99]
	} else {
		df$Source <- names(su)[1:99]
		df$Target <- rep(t,99)
	}
	df$Strength <- as.numeric(su)[1:99]
	df
}

graph <- data.frame(matrix(vector(), 0, 3, dimnames=list(c(), c("Source","Target","Strength"))), stringsAsFactors=F)

data <- read.csv("CLEAN_ANONYMOUS.csv", sep = ",", header = T, stringsAsFactors = F)
data$Content <- tolower(data$Content)

# Naive implementation
for (a in ACTORS) {
	incoming.data <- data[data$Target==a,]
	outgoing.data <- data[data$Source==a,]
	incoming.vocabulary <- unlist(lapply(incoming.data$Content,function(x) unlist(strsplit(x," "))))
	outgoing.vocabulary <- unlist(lapply(outgoing.data$Content,function(x) unlist(strsplit(x," "))))
	incoming.strong.vocabulary <- incoming.vocabulary[nchar(incoming.vocabulary)>3]
	outgoing.strong.vocabulary <- outgoing.vocabulary[nchar(outgoing.vocabulary)>3]
	a.inc.voc <- transform(incoming.strong.vocabulary,t=a)
	a.out.voc <- transform(outgoing.strong.vocabulary,s=a)
	tryCatch({
		graph <- rbind(graph,a.out.voc)
		graph <- rbind(graph,a.inc.voc)
	}, warning = function(w) {
		print(w)
	}, error = function(e) {
	    print(paste("Failed for user",a,"with error",e))
	    print(dim(a.inc.voc))
	    print(dim(a.out.voc))
	}, finally = function(e){
	})
}

graph <- graph[complete.cases(graph) & graph$Strength >= 4,]

write.table(graph,"vocable.csv",
	append = FALSE, quote = TRUE, sep = ",",
	eol = "\n", na = "NA", dec = ".", row.names = FALSE,
	col.names = TRUE, qmethod = c("escape", "double"),
	fileEncoding = "")
