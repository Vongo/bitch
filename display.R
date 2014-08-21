# Libs
require(grid)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
	plots <- c(list(...), plotlist)
	numPlots = length(plots)
	if (is.null(layout)) {
		layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
						ncol = cols, nrow = ceiling(numPlots/cols))
	}
	if (numPlots==1)
		print(plots[[1]])
	else {
		grid.newpage()
		pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
		for (i in 1:numPlots) {
			matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
			tryCatch({
				print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,layout.pos.col = matchidx$col))
			}, warning = function(w) {
				# print(w)
			}, error = function(e) {
			    # print(e)
			}, finally = {
			})
		}
	}
}