#############################################################################################
# Functions used to plot signed networks, including their node partition if any.
# 
# 11/2015 Vincent Labatut
#############################################################################################
library("igraph")



########################################################################
# Adds legend which describes the shapes used in the plot.
# src: https://stackoverflow.com/questions/10389967/common-legend-for-multiple-plots-in-r
#
# target.type: "Group" or "State"
# shape.info: contains "shape label ids" and "shape membership"
# 
# returns: -
########################################################################
add.shape.legend.into.plot = function(shape.label.ids){
	
	# ======================================================
	# we can think that these 2 variables are constant.
	# we'll choose the corresponding shape type and info with "shape.label.ids"
	SHAPE.SIZES = c(0.8, 0.8, 1.4, 1, 1, 1, 1, 1.4)
	SHAPE.TYPES = c(0, 1, 0, 2, 6, 5, 8, 1) #pch = c(15, 17, 25, 23, 8, 20)) 
	
	shape.list = GROUP.VALUES
	# =======================================================
	
	plot(1, type = "n", axes=FALSE, xlab="", ylab="")
	
	# 0: square, 0: square, 2:triangleUp, 
	# 6:triangleDown, 5:diamond, 8:star, 1:circle, 1:circle
	legend(x = "bottom",inset = 0,
			legend = shape.list[shape.label.ids], 
			cex=0.4, 
#			xpd = TRUE, 
			horiz = TRUE, 
			pt.cex=SHAPE.SIZES[shape.label.ids], 
			
			# pt.cex allows to change shape size
			pch = SHAPE.TYPES[shape.label.ids] 
	) 
}

















#############################################################################################
# Plot the specified signed graph, generating files of the specified formats.
#
# g: signed graph to plot.
# plot.file: base name (and path) of the generated files.
# format: format(s) to handle (among "PDF", "PNG", and NA for screen).
#
# returns: the same graph, with the spatial positions stored as nodal attributes x and y.
#############################################################################################
plot.network <- function(g, membership=NA, plot.file, format=c("PDF","PNG",NA))
{	
#	# ----------------------------------------------------------------------
#	#partition = get.membership.from.file(CORCLU.ALGO.PILS, plot.inputs$algo.output.file)
#	
#	edge.mat <- get.edgelist(g)
#	clus.mat <- cbind(membership[edge.mat[,1]],membership[edge.mat[,2]])
#	same.clus <- clus.mat[,1]==clus.mat[,2]
#	
#	# compare link signs and positions 
#	neg.links <- E(g)$weight<0
#	pos.links <- E(g)$weight>0
#	neg.misplaced <- same.clus & neg.links
#	pos.misplaced <- !same.clus & pos.links
#	all.misplaced <- neg.misplaced | pos.misplaced
#	
#	imb.value.from.vincent = sum(abs(E(g)$weight[all.misplaced]))
#	print("imb.value from vincent:")
#	print(imb.value.from.vincent)
#	
#	# ----------------------------------------------------------------------
	
	
	############################################################################
	#### MISC ###### CUSTOM IGRAPH SHAPES
	############################################################################
	
	miniSquare <- function(coords, v=NULL, params) {
		vertex.color <- params("vertex", "color")
		if (length(vertex.color) != 1 && !is.null(v)) {
			vertex.color <- vertex.color[v]
		}
		vertex.size <- 1/130 * params("vertex", "size")
		#if (length(vertex.size) != 1 && !is.null(v)) {
		#  vertex.size <- vertex.size[v]
		#}
		
		symbols(x=coords[,1], y=coords[,2], bg=vertex.color,
				squares = 0.05,
				#stars=cbind(NA, NA, NA, vertex.size, vertex.size, vertex.size),
				add=TRUE, inches=FALSE)
	}
	# clips as a circle
	add.vertex.shape("miniSquare", clip=igraph.shape.noclip,
			plot=miniSquare)
	
	
	
	############################################################################
	
	miniCircle <- function(coords, v=NULL, params) {
		vertex.color <- params("vertex", "color")
		if (length(vertex.color) != 1 && !is.null(v)) {
			vertex.color <- vertex.color[v]
		}
		vertex.size <- 1/130 * params("vertex", "size")
		#if (length(vertex.size) != 1 && !is.null(v)) {
		#  vertex.size <- vertex.size[v]
		#}
		
		symbols(x=coords[,1], y=coords[,2], bg=vertex.color,
				circles = 0.025,
				#stars=cbind(NA, NA, NA, vertex.size, vertex.size, vertex.size),
				add=TRUE, inches=FALSE)
	}
	# clips as a circle
	add.vertex.shape("miniCircle", clip=igraph.shape.noclip,
			plot=miniCircle)
	
	
	############################################################################
	
	mydiamond <- function(coords, v=NULL, params) {
		vertex.color <- params("vertex", "color")
		if (length(vertex.color) != 1 && !is.null(v)) {
			vertex.color <- vertex.color[v]
		}
		vertex.size <- 1/130 * params("vertex", "size")
		#if (length(vertex.size) != 1 && !is.null(v)) {
		#  vertex.size <- vertex.size[v]
		#}
		
		#symbols(x=coords[,1], y=coords[,2], bg=vertex.color,
		symbols(x=coords[,1]+0.03, y=coords[,2]+0.03, bg=vertex.color,
				stars=cbind(NA, NA, NA, vertex.size, vertex.size, vertex.size),
				add=TRUE, inches=FALSE)
	}
	# clips as a circle
	#add.vertex.shape("diamond", clip=vertex.shapes("rectangle")$clip, plot=mydiamond)
	add.vertex.shape("diamond", clip=igraph.shape.noclip, plot=mydiamond)
	
	
	############################################################################
	
	mytrianglePointDown <- function(coords, v=NULL, params) {
		vertex.color <- params("vertex", "color")
		if (length(vertex.color) != 1 && !is.null(v)) {
			vertex.color <- vertex.color[v]
		}
		vertex.size <- 1/100 * params("vertex", "size")
		#if (length(vertex.size) != 1 && !is.null(v)) {
		#  vertex.size <- vertex.size[v]
		#}
		
		symbols(x=( coords[,1]-0.05 ), y=coords[,2]+0.02, bg=vertex.color,
				stars=cbind(vertex.size, NA,  NA, NA, NA, vertex.size),
				#stars=cbind(vertex.size, vertex.size, vertex.size),
				add=TRUE, inches=FALSE)
	}
	# clips as a circle
	add.vertex.shape("triangleDown", clip=igraph.shape.noclip,
			plot=mytrianglePointDown)
	
	##########################################################”
	
	mytrianglePointUp <- function(coords, v=NULL, params) {
		vertex.color <- params("vertex", "color")
		if (length(vertex.color) != 1 && !is.null(v)) {
			vertex.color <- vertex.color[v]
		}
		vertex.size <- 1/100 * params("vertex", "size")
		#if (length(vertex.size) != 1 && !is.null(v)) {
		#  vertex.size <- vertex.size[v]
		#}
		
		symbols(x=( coords[,1]+0.05 ), y=coords[,2]-0.02, bg=vertex.color,
				stars=cbind(NA, NA, vertex.size, vertex.size, NA, NA),
				#stars=cbind(vertex.size, vertex.size, vertex.size),
				add=TRUE, inches=FALSE)
	}
	# clips as a circle
	add.vertex.shape("triangleUp", clip=igraph.shape.noclip,
			plot=mytrianglePointUp)
	
	
	
	
	# generic square vertex shape, with a parameter for number of rays
	mysquare <- function(coords, v=NULL, params) {
		vertex.color <- params("vertex", "color")
		if (length(vertex.color) != 1 && !is.null(v)) {
			vertex.color <- vertex.color[v]
		}
		vertex.size  <- 1/200 * params("vertex", "size")
		if (length(vertex.size) != 1 && !is.null(v)) {
			vertex.size <- vertex.size[v]
		}
		norays <- params("vertex", "norays")
		if (length(norays) != 1 && !is.null(v)) {
			norays <- norays[v]
		}
		
		mapply(coords[,1], coords[,2], vertex.color, vertex.size, norays,
				FUN=function(x, y, bg, size, nor) {
					symbols(x=x, y=y, bg=bg,
							stars=matrix(c(size,size), nrow=1, ncol=nor*2),
							add=TRUE, inches=FALSE)
				})
	}
	# no clipping, edges will be below the vertices anyway
	add.vertex.shape("mysquare", clip=igraph.shape.noclip,
			plot=mysquare, parameters=list(vertex.norays=10))
	
	
	
	# generic star vertex shape, with a parameter for number of rays
	mystar <- function(coords, v=NULL, params) {
		vertex.color <- params("vertex", "color")
		if (length(vertex.color) != 1 && !is.null(v)) {
			vertex.color <- vertex.color[v]
		}
		vertex.size  <- 1/200 * params("vertex", "size")
		if (length(vertex.size) != 1 && !is.null(v)) {
			vertex.size <- vertex.size[v]
		}
		norays <- params("vertex", "norays")
		if (length(norays) != 1 && !is.null(v)) {
			norays <- norays[v]
		}
		
		mapply(coords[,1], coords[,2], vertex.color, vertex.size, norays,
				FUN=function(x, y, bg, size, nor) {
					symbols(x=x, y=y, bg=bg,
							stars=matrix(c(size,size/2), nrow=1, ncol=nor*2),
							add=TRUE, inches=FALSE)
				})
	}
	# no clipping, edges will be below the vertices anyway
	add.vertex.shape("star", clip=igraph.shape.noclip,
			plot=mystar, parameters=list(vertex.norays=5))
	
	
	
	vshapes=
			c(
					"miniSquare",
					"miniCircle", 
					"square",
					"triangleUp",
					"triangleDown", 
					"diamond",
					"star",
					"circle",
					"sphere", 
					"rectangle"
			)
	V(g)$shapes=vertex.shapes() 
	
	# pie.portions <- lapply(1:length(shape.membership), 
	#					function(x) rep(1,4)) #equal pie: %25,%25,%25,%25 
	
	############################################################################
	### Labatut's code
	############################################################################
	COMPRESS <- FALSE # PDF icin
	
	
		
	# setup node parameters
	vertex.sizes <- 10
#	vertex.labels = V(g)$MEPid
	vertex.labels = paste(V(g)$Lastname,"(",V(g)$MepGlobalId,")",sep="")
	vertex.label.colors = "black"
	vertex.label.cexs = 0.4
	

	
	
	# setup link parameters
	if(ecount(g)>0)
	{	edge.colors <- rep(NA,ecount(g))
		edge.colors[E(g)$weight<0] <- adjustcolor("RED", alpha.f=0.5)
		edge.colors[E(g)$weight>0] <- adjustcolor("GREEN", alpha.f=0.5)
		edge.widths <- abs(E(g)$weight)*10
	}
	
	# set up node colors
	if(all(is.na(membership)))
		vertex.colors <- "SkyBlue2" # default igraph color
	else
	{	
		# ===================== PREC PALETTE
		iso.indx = get.isolated.nodes.indx(g)
		removed.node.indx.by.kmbs = which(membership == -1) # if kmbs has been applied
		membership[removed.node.indx.by.kmbs] = length(unique(membership))+1 # convert -1 values to a positive value before using palette
		# =====================
		
		palette <- rainbow(length(unique(membership)))
		vertex.colors <- palette[membership]
		
		# ===================== POST PALETTE
		vertex.colors[iso.indx] = "White"
		vertex.colors[removed.node.indx.by.kmbs] = "Black"
		# =====================
	}
	
	
	
	# setup layout (only if the graph doesn't already have one)
	lyt = NA
	v.att <- list.vertex.attributes(graph=g)
	if(!("x" %in% v.att) | !("y" %in% v.att)) 
	{	gpos <- delete.edges(graph=g,edges=which(E(g)$weight<0))
		
#		lyt <- layout.kamada.kawai(graph=gpos)
		lyt <- layout.fruchterman.reingold(graph=gpos)
#		lyt <- layout.circle(graph=gpos)
		
		# store spatial positions as nodal attributes
		V(g)$x <- lyt[,1]
		V(g)$y <- lyt[,2]
	}
	else
		lyt <- cbind(V(g)$x,V(g)$y)
	
	
	# ========================================================================================
	# vertex.shapes = vshapes[rep(8, vcount(g))] # vshapes[8] = "circle" # by default
	is.country.network = TRUE
	if(length(unique(V(g)$Group)) == 1)
		is.country.network = FALSE
	
	if(is.country.network)
		shape.membership = sapply(V(g)$Group, function(name) POLITICAL.GROUP.SHAPE.ASS[[name]])
	else
		shape.membership = as.factor( sapply(V(g)$Country, function(name) EU.REGION.FOR.STATE[[name]]) )

	shape.label.ids = sort(unique(as.integer(shape.membership))) # later, shape ids will give us shape names
	vertex.shapes = vshapes[shape.membership]
	# ========================================================================================
	
	
	# process each specified format
	for(frmt in format)
	{	# set plot file name
		plot.filename <- plot.file
		if(toupper(substr(plot.filename, nchar(plot.filename)-2, nchar(plot.filename)))!=toupper(frmt))
			plot.filename <- paste0(plot.filename ,".",frmt)
		# create the file
		if(!is.na(frmt))
		{	if(frmt=="PNG")
			{	png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
			}
			else if(frmt=="PDF")
			{	pdf(file=plot.filename,bg="white",compress=COMPRESS)
			}
		}
		
		
		nb.row=1
		nb.col=1
		m = matrix(c(1,2),nrow = nb.row+1,ncol = nb.col,byrow = TRUE)
		heights = c(0.7,0.3)
		layout(mat = m,heights = heights)
		
		
		# create the plot
		if(ecount(g)>0)
		{	
					plot(
							g,
							vertex.shape=vertex.shapes,
							layout=lyt,
							vertex.size=vertex.sizes,
							vertex.label=vertex.labels, 
							vertex.color=vertex.colors, 
							vertex.label.color=vertex.label.colors, 
							vertex.label.cex=vertex.label.cexs,
							edge.color=edge.colors, 
							edge.width=edge.widths
#				main=plot.title # BUNU KULLANMA, font size orantisiz cikiyo
					)
		}
		else
		{	
					plot(
							g, 
							layout=lyt,
							vertex.size=vertex.sizes, 
							vertex.label=vertex.label,
							vertex.color=vertex.colors
					)
		}
		title(g$name, cex.main=0.5)
		
		# finalize plot file
		if(!is.na(frmt)){
			add.shape.legend.into.plot(shape.label.ids)
			dev.off()
		}
			
	}
	
	return(g)
	
	
}

