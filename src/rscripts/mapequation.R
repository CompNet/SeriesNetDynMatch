# Detection of communities in dynamic TV Series network using MapEquation
# 
# Usage:
# 1) possibly edit the above parameters
# 2) open an R console
# 3) to install the igraph library, type:
#		install.packages("igraph")
# 3) to set up the current R workspace, type:
#		setwd("xxxxxx")
#    where xxxxxx is the path of this "Series" project.
# 4) then, to launch the script, type: 
#		source("src/rscripts/mapequation.R")
# 
# Author: Vincent Labatut 12/2016
###############################################################################
library("tools")
library("igraph")




# parameters
###############################################################################
#data.folder <- "data/test"							# TODO folder containing the data (relatively to the current R workspace)
#data.folder <- "data/BB_dyn_ns"
data.folder <- "data/GoT_dyn_ns"
#data.folder <- "data/HoC_dyn_ns"
data.folder2 <- paste0(data.folder,"_updt")
dyncom.folder <- "conf_infomap_undir"						# TODO folder containing the MapEquation executable files
dyncom.exec <- file.path(dyncom.folder,"conf-infomap")		# TODO executable file of the community detection program 





# get the list of original network files
###############################################################################
graph.files <- list.files(path=data.folder,pattern="*.graphml", all.files=FALSE, full.names=FALSE, recursive=FALSE, ignore.case=FALSE, include.dirs=FALSE, no..=TRUE)




# convert each network to the Pajek format
###############################################################################
cat("\n\n[",format(Sys.time(),"%a %d %b %Y %X"),"] Converting the graphs\n",sep="")
for(graph.file in graph.files)
{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"]  Processing file '",graph.file,"'\n",sep="")
	
	# read the graphml file
	graphml.file <- file.path(data.folder,graph.file)
	g <- read_graph(file=graphml.file,format="graphml")
	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"]  Number of links: ",gsize(g),"\n",sep="")

	# write as a Pajek file
	V(g)$id <- V(g)$label
	pajek.file <- paste0(file_path_sans_ext(graphml.file),".net")
	write_graph(graph=g,file=pajek.file,format="pajek")
}




# apply MapEquation to each iteration
###############################################################################
cat("\n\n[",format(Sys.time(),"%a %d %b %Y %X"),"] Detecting the communities\n",sep="")
prev.coms <- NA
for(i in 1:length(graph.files))
{	graph.file <- graph.files[i]
	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"]  Processing file '",graph.file,"'\n",sep="")
	
	# setup file names
	graphml.file <- file.path(data.folder,graph.file)
	pajek.file <- file.path(getwd(),paste0(file_path_sans_ext(graphml.file),".net"))
	trace.file <- file.path(getwd(),paste0(file_path_sans_ext(graphml.file),"_trace.txt"))
	
	# execute MapEquation
	seed <- 1 
	cmd <- paste0(dyncom.exec," ",seed," ",pajek.file," > ",trace.file)
	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"]  Executing command '",cmd,"'\n",sep="")
	system(cmd)
}






