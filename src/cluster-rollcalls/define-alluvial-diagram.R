
library(alluvial)


## Add an alpha value to a colour
add.alpha <- function(col, alpha=1){
	apply(sapply(col, col2rgb)/255, 2, 
			function(x) 
				rgb(x[1], x[2], x[3], alpha=alpha))  
}




###############################################################
#
# n: The number of vertical layer (i.e partition) desired. 
# 	  Not that it migh be lower than 'n' at the end if the best k value is low (i.e. 2 or 3, etc.)
# themes: Themes covered in a given instance (i.e. France, Agri, 12-13).
# 		   Note that themes will differ from other instances (i.e. for another period)
# partitions.df: A data frame object in which each column corresponds to a partition
# x.labels: Labels in x axis, for each vertical layer
#
################################################################
plot.alluvial.diagram = function(out.folder, n, curr.themes.ids.per.doc, partitions.df, x.labels, domain,
		measure, order.docs.by.theme=FALSE, show.stripes.border=FALSE){

	# ===========================================================================
	adj.labels = c()
	adj.part.df = c()
	
	# iterate over theme.id list and identify the docs with multiple labels.
	# if it exists, turn string data into list (as if there wre 2 different docs with different single label)
	# then, duplicate partition info for corresponding docs
	for(i in 1:length(curr.themes.ids.per.doc)){
		adj.label = unlist(strsplit(curr.themes.ids.per.doc[i], split=","))
		adj.row = partitions.df[rep(i, each=length(adj.label)),]
		
		adj.labels = c(adj.labels, adj.label)
		adj.part.df = rbind(adj.part.df, adj.row)
	}
	
	curr.themes.ids.per.doc = adj.labels
	partitions.df = adj.part.df
	# ===========================================================================
	

	
	docs.order.indx = 1:length(curr.themes.ids.per.doc) # normal, i.e. without ordering
	if(order.docs.by.theme)
		docs.order.indx = order(curr.themes.ids.per.doc)
		
	for(i in 1:ncol(partitions.df)){
		p = partitions.df[docs.order.indx,i]
		partitions.df[,i] = p
	}
	
	
	
	
	theme.id.list = unique(curr.themes.ids.per.doc)
	nb.theme = length(theme.id.list)
	palette <- as.list(rainbow(nb.theme))
	# ==============
	# The 4th color is green. The 5rd is too. So change the 4th to distinguish theme clearly
	palette[[1]] = "lightseagreen" 
	palette[[2]] = "gray87" # light gray
	palette[[3]] = "#6E8B3D" # dark olive green
	palette[[4]] = "#E3CF57" # banana yellow
	palette[[5]] = "seagreen4" #
	palette[[6]] = "green" # 
	palette[[7]] = "darkorange" #
	palette[[8]] = "maroon1" # 
	palette[[9]] = "blue2" # 
	palette[[10]] = "darkviolet" # 
	palette[[11]] = "plum1" #
	palette[[12]] = "yellow" # 
	palette[[13]] = "burlywood4" # 
	palette[[14]] = "black" #
	
	#palette = lapply(palette, function(col) add.alpha(col, alpha=0.5))
	# ==============
	names(palette) = theme.id.list
	cols <- unlist(palette[ curr.themes.ids.per.doc[docs.order.indx] ])
		
	
	curr.theme.names.per.doc = sapply(theme.id.list,
			function(labels){				
				doc.theme.ids = as.integer(unlist(strsplit(as.character(labels),split=",")))
				doc.themes = retreive.theme.names.by.id(domain, doc.theme.ids)
				
				# TODO handle the long label name problem later ==> this is the case when a doc has multi labels
				handled.themes = ""
				NB.LIM = 3 # nb item limit per line
				if(length(doc.themes)>NB.LIM){
					handled.themes = paste0(paste(doc.themes[1:NB.LIM],collapse=";"), "\n",
							paste(doc.themes[(NB.LIM+1):length(doc.themes)],collapse=";") )
				} else
					handled.themes = paste(doc.themes,collapse=";")
					
				return(handled.themes)
			})
	
	
	
	filename=paste0(measure,"-alluvial-diagram")
	border=NA
	if(show.stripes.border){
		border="black"
		filename=paste0(filename,"-bordered")
	}
	if(order.docs.by.theme)
		filename=paste0(filename,"-ordered")
	
	pdf(file=paste0(out.folder,"/",filename,".pdf"),bg="white",compress=COMPRESS, width=4, height=7)
	# 'alpha' parameter changes transparency level of colors
	alluvial(x=partitions.df, freq=1, col=cols, border=border, axis_labels=x.labels, cex.axis=0.70, alpha=0.5)
	
	
	palette = lapply(palette, function(col) add.alpha(col, alpha=0.65))
	
	plot(1, type = "n", axes=FALSE, xlab="", ylab="") # put the legend on the 2nd page as alluvial diagram needs more space
	legend("top",legend=curr.theme.names.per.doc,col=unlist(palette),pch=rep(19, length(nb.theme)), bty="n",cex=0.37,pt.cex=0.8)
#	legend("top",legend=curr.theme.names.per.doc,col=unlist(palette),pch=rep(19, length(nb.theme)), bty="n",cex=0.7,pt.cex=1.5)
	dev.off()
}