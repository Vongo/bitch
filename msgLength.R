


messages <- read.csv("CLEAN_ANONYMOUS.csv",stringsAsFactors=F)

lengths <- data.frame(matrix(vector(), nrow(messages), 4, dimnames=list(c(), c("Source","Target","DateTime","Length"))), stringsAsFactors=F)
lengths[,1:3] <- messages[,2:4]
lengths[,4] <- sapply(messages$Content,function(x) nchar(x))



