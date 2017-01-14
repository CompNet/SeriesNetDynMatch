# Plot the evolution of the strength of the specified nodes and links, for
# three version of the dynamic network: narrative smoothing, windowed
# with a 10-scene window, and with a 40-scene window.
# 
# Usage:
# 1) possibly edit the below parameters
# 2) open an R console
# 3) to install the igraph library, type:
#		install.packages("igraph")
# 3) to set up the current R workspace, type (replace by your own path to this project):
#		setwd("D:/Eclipse/workspaces/Networks/Series")
# 4) then, to launch the script, type: 
#		source("src/rscripts/strength-evol.R")
# 
# Author: Vincent Labatut 01/2017
###############################################################################
library("tools")		# basic R functions
library("igraph")		# to handle graphs




# parameters
###############################################################################
series.name <- "BB"										# TODO name of the series to process
#series.name <- "GoT"
#series.name <- "HoC"
if(series.name=="BB")
{	# TODO last scene limit (or NA for all scenes), as numbered in the graph file names
#	last.scene <- NA	# Everything
	last.scene <- 400	# S1-S2
	# TODO names of the characters to plot
	char.names <- list(											
		c("Walter White","Tuco Salamanca"),
		c("Walter White","Jesse Pinkman"),
		c("Walter White","Saul Goodman","Mike Ehrmantraut"),
		c("Walter White","Skyler White","Walter White Jr.")
	)
	# TODO periods displayed in the character plot (possibly none)
	char.periods <- list(
		list(),
		list(),
		list(),
		list()
	)
	# TODO vertical marks displayed in the character plot (possibly none)
	char.marks <- list(
		list("Tuco's death "=172),	# verified
		list(),
		list(),
		list()
	)
	# TODO names of the relationships to plot
	link.names <- list(
		c("Walter White","Jesse Pinkman")
	)
	# TODO periods displayed in the link plot (possibly none)
	link.periods <- list(
		list()
	)
	# TODO vertical marks displayed in the link plot (possibly none)
	link.marks <- list(
		list()
	)
}else if(series.name=="GoT")
{	# TODO last scene limit (or NA for all scenes), as numbered in the graph file names
#	last.scene <- NA
#	last.scene <- 457	# S1-S2
#	last.scene <- 670	# S1-S3
#	last.scene <- 857	# S1-S4
	last.scene <- 1039	# S1-S5 (not all S5, just the scenes present in all 3 types of graphs)
	# TODO names of the characters to plot
	char.names <- list(
		c("Daenerys Targaryen","Tyrion Lannister"),
		c("Rickon Stark","Catelyn Stark","Brandon Stark","Eddard Stark","Sansa Stark","Arya Stark","Robb Stark","Jon Snow"),
		c("Tywin Lannister","Jaime Lannister","Cersei Lannister","Tyrion Lannister")
	)
	# TODO periods displayed in the character plot (possibly none)
	char.periods <- list(
		list(),#list("Test situation"=c(100,140)),
		list(),
		list()
	)
	# TODO vertical marks displayed in the character plot (possibly none)
	char.marks <- list(
		list("Tyrion becomes the King's Hand "=252),	# verified
		list(),
		list()
	)
	# TODO names of the relationships to plot
	link.names <- list(
		c("Jaime Lannister","Cersei Lannister","Jaime Lannister","Tyrion Lannister","Cersei Lannister","Tyrion Lannister"),
		c("Jon Snow","Ygritte","Jon Snow","Samwell Tarly")
	)
	# TODO periods displayed in the link plot (possibly none)
	link.periods <- list(
		list(),
#		list("Jon and Ygritte's relationship"=c(358,641),"Battle of Castle Black"=c(823,839))
		list(" "=c(358,641)," "=c(847,869)) # estimated from summaries
#		list()
	)
	# TODO vertical marks displayed in the link plot (possibly none)
	link.marks <- list(
		list("Tyrion becomes the King's Hand "=252),	# verified
#		list("Jon captures Ygritte "=358,"Jon and Ygritte have sex "=542,"Jon flees the Wildlings "=631,"Ygritte shoots Jon "=667,"Ygritte dies "=834)	# these are approximations based on summaries
		list("Jon captures Ygritte "=358,"Jon and Ygritte have sex "=554,"Jon flees the Wildlings "=641,"Ygritte shoots Jon "=679,"Ygritte dies "=855)	# these are approximations based on summaries
#		list()
	)
}else if(series.name=="HoC")
{	# TODO last scene limit (or NA for all scenes), as numbered in the graph file names
#	last.scene <- NA
	last.scene <- 879	# S1-S2 (not all S2, just the scenes present in all 3 types of graphs)
	# TODO names of the characters to plot
	char.names <- list(
		c("Francis Underwood","Claire Underwood"),
		c("Francis Underwood","Garrett Walker"),
		c("Zoe Barnes","Lucas Goodwin")
	)
	# TODO periods displayed in the character plot (possibly none)
	char.periods <- list(
		list(),
		list(),
		list()
	)
	# TODO vertical marks displayed in the character plot (possibly none)
	char.marks <- list(
		list(),
		list(),
		list()
	)
	# TODO names of the relationships to plot
	link.names <- list(
		c("Francis Underwood","Claire Underwood","Francis Underwood","Martin Spinella","Lucas Goodwin","Gavin Orsay")
	)
	# TODO periods displayed in the link plot (possibly none)
	link.periods <- list(
		list()
	)
	# TODO vertical marks displayed in the link plot (possibly none)
	link.marks <- list(
		list("F.Underwood/M.Spinella fight "=129)	# verified
	)
}




# folders
###############################################################################
data.base <- "data"
res.folder <- file.path(data.base,paste0(series.name,"_dyn_strength"))
dir.create(res.folder, showWarnings=FALSE, recursive=TRUE)




# color palette
###############################################################################
palette <- c(
	"#e41a1c",
	"#377eb8",
	"#4daf4a",
	"#984ea3",
	"#ff7f00",
	"#ffff33",
	"#a65628",
	"#f781bf"
)




# get the character strengths
###############################################################################
# process each type of network
#for(type in c("ts10"))
for(type in c("ns","ts10","ts40"))
{	# setup the folders
	input.folder <- file.path(data.base,paste0(series.name,"_dyn_",type))
	
	# get the list of network files
	###############################################################################
	graph.files <- list.files(path=input.folder,pattern="*.graphml", all.files=FALSE, full.names=FALSE, recursive=FALSE, ignore.case=FALSE, include.dirs=FALSE, no..=TRUE)
	scenes <- sapply(strsplit(graph.files,"[_.]",fixed=FALSE),function(s) as.integer(s[3]))
	graph.files <- c(sort(graph.files[scenes<1000]),sort(graph.files[scenes>=1000]))
	scenes <- sapply(strsplit(graph.files,"[_.]",fixed=FALSE),function(s) as.integer(s[3]))
	if(!is.na(last.scene))
	{	idx <- which(scenes==last.scene)	
		scenes <- scenes[1:idx]
		graph.files <- graph.files[1:idx]
	}
	
	# load the graphs, including the link weights
	###############################################################################
	cat("\n\n[",format(Sys.time(),"%a %d %b %Y %X"),"] Loading the graphs\n",sep="")
	char.str <- list()
	link.str <- list()
	for(f in 1:length(graph.files))
	{	graph.file <- graph.files[[f]]
		cat("[",format(Sys.time(),"%a %d %b %Y %X"),"]   Processing file '",graph.file,"'\n",sep="")
		
		# read the graphml file
		graphml.file <- file.path(input.folder,graph.file)
		g <- read_graph(file=graphml.file,format="graphml")
		cat("[",format(Sys.time(),"%a %d %b %Y %X"),"]    Number of links: ",gsize(g),"\n",sep="")
		node.names <- V(g)$label
		
		# get the strength for each group of characters
		for(grp in 1:length(char.names))
		{	if(f==1)
			{	char.str[[grp]] <- matrix(NA,nrow=length(char.names[[grp]]),ncol=length(graph.files))
				rownames(char.str[[grp]]) <- char.names[[grp]]
			}
			idx <- match(char.names[[grp]],V(g)$label)
			char.str[[grp]][,f] <- strength(graph=g,vids=idx)
		}
		
		# get the strength for each group of links
		for(grp in 1:length(link.names))
		{	if(f==1)
			{	link.str[[grp]] <- matrix(NA,nrow=length(link.names[[grp]])/2,ncol=length(graph.files))
				rownames(link.str[[grp]]) <- apply(matrix(link.names[[grp]],ncol=2,byrow=TRUE),1,function(x) paste(x,collapse="-"))
			}
			V(g)$name <- V(g)$label
			idx <- get.edge.ids(graph=g, vp=link.names[[grp]])
			vals <- rep(0,length(idx))
			wg <- E(g)[idx]$weight
			vals[which(idx!=0)] <- wg
			link.str[[grp]][,f] <- vals
		}
	}
	
	# generate the resulting plots
	###############################################################################
	for(grp in 1:length(char.names))
	{	pdf.file <- file.path(res.folder,paste0("strengh_grp",grp,"_",type,".pdf"))
		pdf(file=pdf.file)
			# create the empty plot 
#			plot(x=1:length(graph.files),y=rep(0,length(graph.files)),
			plot(x=scenes,y=rep(0,length(graph.files)),
					ylim=c(min(char.str[[grp]]),max(char.str[[grp]])),
					type="n",
					xlab="Scene rank",
#					xaxt="n",
					ylab="Speaker strength")
			# add period parks
			if(length(char.periods[[grp]])>0)
			{	for(i in 1:length(char.periods[[grp]]))
#				{	start.scene <- char.periods[[grp]][[i]][1]
#					end.scene <- char.periods[[grp]][[i]][2]
				{	start.scene <- which(scenes==char.periods[[grp]][[i]][1])
					end.scene <- which(scenes==char.periods[[grp]][[i]][2])
					rect(xleft=start.scene, xright=end.scene, 
							ybottom=grconvertY(0,from='npc'), ytop=grconvertY(1,from='npc'),
							col="GREY80",border=NA)
					text(x=(start.scene+end.scene)/2, y=max(char.str[[grp]]),
							labels=names(char.periods[[grp]])[i],
							#adj=NULL, pos=NULL, offset=0.5,
							pos=3, #offset=0.25,
							cex=0.5, col="GREY30")
				}
			}
			# add punctual marks
			if(length(char.marks[[grp]])>0)
			{	for(i in 1:length(char.marks[[grp]]))
#				{	scene <- char.marks[[grp]][i]
				{	scene <- which(scenes==char.marks[[grp]][[i]])
					abline(v=scene,
							col="GREY30", lwd=2) #, lty=3
					text(x=scene, y=grconvertY(1,from='npc'), 
							labels=names(char.marks[[grp]])[i],
							#adj=NULL, pos=NULL, offset=0.5,
							pos=2, offset=0.25,
							srt=90, cex=0.5, col="GREY30")
				}
			}
			# add the data series
			for(i in 1:nrow(char.str[[grp]]))
#			{	lines(x=1:length(graph.files),y=char.str[[grp]][i,],
			{	lines(x=scenes,y=char.str[[grp]][i,],
					type="l",
					lwd=1,
					col=palette[i]
				)
			}
			# add the legend
			legend(x="topright",
					legend=char.names[[grp]],
					fill=palette[1:nrow(char.str[[grp]])]
			)
			
			# re-draw the bounding box border (on top of the possibly interfering graphics)
			rect(xleft=grconvertX(0,from='npc'), xright=grconvertX(1,from='npc'), 
					ybottom=grconvertY(0,from='npc'), ytop=grconvertY(1,from='npc'),
					col=NA,border="BLACK",xpd=TRUE)
		dev.off()
	}

	for(grp in 1:length(link.names))
	{	pdf.file <- file.path(res.folder,paste0("weight_grp",grp,"_",type,".pdf"))
		pdf(file=pdf.file)	
			# create the empty plot 
#			plot(x=1:length(graph.files),y=rep(0,length(graph.files)),
			plot(x=scenes,y=rep(0,length(graph.files)),
					ylim=c(min(link.str[[grp]]),max(link.str[[grp]])),
					type="n",
					xlab="Scene rank",
					ylab="Relation weight")
			# add period parks
			if(length(link.periods[[grp]])>0)
			{	for(i in 1:length(link.periods[[grp]]))
#				{	start.scene <- link.periods[[grp]][[i]][1]
#					end.scene <- link.periods[[grp]][[i]][2]
				{	start.scene <- which(scenes==link.periods[[grp]][[i]][1])
					end.scene <- which(scenes==link.periods[[grp]][[i]][2])
					rect(xleft=start.scene, xright=end.scene, 
							ybottom=grconvertY(0,from='npc'), ytop=grconvertY(1,from='npc'),
							col="GREY80",border=NA)
					text(x=(start.scene+end.scene)/2, y=max(link.str[[grp]]),
							labels=names(link.periods[[grp]])[i],
							#adj=NULL, pos=NULL, offset=0.5,
							pos=3, #offset=0.25,
							cex=0.5, col="GREY30")
				}
			}
			# add punctual marks
			if(length(link.marks[[grp]])>0)
			{	for(i in 1:length(link.marks[[grp]]))
#				{	scene <- link.marks[[grp]][[i]]
				{	scene <- which(scenes==link.marks[[grp]][[i]])
					abline(v=scene,
							col="GREY30", lwd=2) #, lty=3
					text(x=scene, y=grconvertY(1,from='npc'), 
							labels=names(link.marks[[grp]])[i],
							#adj=NULL, pos=NULL, offset=0.5,
							pos=2, offset=0.25,
							srt=90, cex=0.5, col="GREY30")
				}
			}
			# add the data series
			for(i in 1:nrow(link.str[[grp]]))
#			{	lines(x=1:length(graph.files),y=link.str[[grp]][i,],
			{	lines(x=scenes,y=link.str[[grp]][i,],
						type="l",
						lwd=1,
						col=palette[i]
				)
			}
			# add the legend
			legend(x="topright",
					legend=rownames(link.str[[grp]]),
					fill=palette[1:nrow(link.str[[grp]])]
			)
			
			# re-draw the bounding box border (on top of the possibly interfering graphics)
			rect(xleft=grconvertX(0,from='npc'), xright=grconvertX(1,from='npc'), 
					ybottom=grconvertY(0,from='npc'), ytop=grconvertY(1,from='npc'),
					col=NA,border="BLACK",xpd=TRUE)
		dev.off()
	}
}

#TODO afficher saisons & épisodes en x ?
#TODO afficher période au lieu de points d'intérêt ?
