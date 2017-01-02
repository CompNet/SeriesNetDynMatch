# Detection of communities in dynamic TV Series network using Louvain
# 
# Usage:
# 1) possibly edit the above parameters
# 2) open an R console
#		install.packages("igraph")
# 3) to set up the current R workspace, type:
#		setwd("xxxxxx")
#    where xxxxxx is the path of this "Series" project.
# 4) then, to launch the script, type: 
#		source("incrlouvain.R")
# 
# Author: Vincent Labatut
###############################################################################
library("tools")
library("igraph")




# parameters
###############################################################################
data.folder <- "test"									# TODO folder containing the data (relatively to the current R workspace)
#data.folder <- "BB_dyn_ns"
#data.folder <- "GoT_dyn_ns"
data.folder <- "HoC_dyn_ns"
data.folder2 <- paste0(data.folder,"_updt")
louvain.folder <- "louvain"								# TODO folder containing the Louvain executable files
convert.exec <- file.path(louvain.folder,"convert")		# TODO executable file of the conversion program 
louvain.exec <- file.path(louvain.folder,"louvain")		# TODO executable file of the community detection program 
hierarchy.exec <- file.path(louvain.folder,"hierarchy")	# TODO executable file of the community structure conversion program 




# get the list of original network files
###############################################################################
graph.files <- list.files(path=data.folder,pattern="*.graphml", all.files=FALSE, full.names=FALSE, recursive=FALSE, ignore.case=FALSE, include.dirs=FALSE, no..=TRUE)




# compile each network to the Louvain binary format
###############################################################################
cat("\n\n[",format(Sys.time(),"%a %d %b %Y %X"),"] Converting the graphs\n",sep="")
for(graph.file in graph.files)
{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"]  Processing file '",graph.file,"'\n",sep="")
	
	# read the graphml file
	graphml.file <- file.path(data.folder,graph.file)
	g <- read_graph(file=graphml.file,format="graphml")
	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"]    Number of links: ",gsize(g),"\n",sep="")

	# write as an edgelist
	edgelist <- as_edgelist(graph=g,names=FALSE)	# get the edges
	edgelist <- cbind(edgelist, E(g)$weight)			# add the edge weights
	edgelist <- rbind(edgelist, c(1,gorder(g),0))		# add a zero weight link between first and last nodes: this should force Louvain to handle the correct number of nodes
	edgelist <- edgelist - 1			# re-number the nodes starting from zero
	edgelist.file <- paste0(file_path_sans_ext(graphml.file),".edgelist")
	write.table(x=edgelist,file=edgelist.file,row.names=FALSE,col.names=FALSE)
	
	# compile to the Louvain binary format
	txt.file <- file.path(getwd(),edgelist.file)
	net.bin <- file.path(getwd(),paste0(file_path_sans_ext(graphml.file),".bin"))
	weight.bin <- file.path(getwd(),paste0(file_path_sans_ext(graphml.file),".weights"))
	cmd <- paste0(convert.exec," -i ",txt.file," -o ",net.bin," -w ",weight.bin)
	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"]    Executing command '",cmd,"'\n",sep="")
	system(cmd)
}




# apply Louvain to each iteration
###############################################################################
cat("\n\n[",format(Sys.time(),"%a %d %b %Y %X"),"] Detecting the communities\n",sep="")
prev.coms <- NA
for(i in 1:length(graph.files))
{	graph.file <- graph.files[i]
	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"]  Processing file '",graph.file,"'\n",sep="")
	
	# setup file names
	graphml.file <- file.path(data.folder,graph.file)
	net.bin <- file.path(getwd(),paste0(file_path_sans_ext(graphml.file),".bin"))
	weight.bin <- file.path(getwd(),paste0(file_path_sans_ext(graphml.file),".weights"))
	tree.file <- file.path(getwd(),paste0(file_path_sans_ext(graphml.file),".tree"))
	coms.file <- file.path(getwd(),paste0(file_path_sans_ext(graphml.file),".coms"))
	temp.file <- file.path(getwd(),paste0(file_path_sans_ext(graphml.file),".temp"))
	
	# setup command
	if(is.na(prev.coms))
	{	#part.file <- file.path(getwd(),paste0(file_path_sans_ext(file.path(data.folder,graph.files[length(graph.files)])),".coms"))
		#cmd <- paste0(louvain.exec," ",net.bin," -l -1 -q 0 -w ",weight.bin," -p ",part.file," > ",tree.file)
		cmd <- paste0(louvain.exec," ",net.bin," -l -1 -q 0 -w ",weight.bin," > ",tree.file)
	}		
	else
		cmd <- paste0(louvain.exec," ",net.bin," -l -1 -q 0 -w ",weight.bin," -p ",prev.coms," > ",tree.file)
	
	# execute Louvain
	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"]    Executing command '",cmd,"'\n",sep="")
	system(cmd)
	
#	if(file.exists(tree.file))	
	{	# retrieve the community structure
		cmd <- paste0(hierarchy.exec," ",tree.file," -n > ",temp.file)	# very weird, but couldn't find another way to get the number of communities
		cat("[",format(Sys.time(),"%a %d %b %Y %X"),"]    Executing command '",cmd,"'\n",sep="")
		system(cmd)
		lines <- readLines(temp.file)
		coms <- length(lines) - 2
		cat("[",format(Sys.time(),"%a %d %b %Y %X"),"]    Number of levels in the dendrogram: ",coms,"\n",sep="")
		cmd <- paste0(hierarchy.exec," ",tree.file," -l ",coms," > ",coms.file)
		cat("[",format(Sys.time(),"%a %d %b %Y %X"),"]    Executing command '",cmd,"'\n",sep="")
		system(cmd)
		file.remove(temp.file)
		prev.coms <- coms.file
	}
}





# get the resulting communities, add them to original the graphml file as nodal attributes
###############################################################################
cat("\n\n[",format(Sys.time(),"%a %d %b %Y %X"),"] Updating the graphs\n",sep="")
dir.create(data.folder2, showWarnings=FALSE, recursive=TRUE)
for(i in 1:length(graph.files))
{	graph.file <- graph.files[i]
	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"]  Processing file '",graph.file,"'\n",sep="")

	# read the graphml file
	graphml.file <- file.path(data.folder,graph.file)
	g <- read_graph(file=graphml.file,format="graphml")

	# get the communities
	coms.file <- paste0(file_path_sans_ext(graphml.file),".coms")
	coms <- as.matrix(read.table(coms.file))[,2]
	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"]  Number of distinct communities: ",length(unique(coms)),"\n",sep="")
	
	# add to the graph and record
	V(g)$com <- coms
	updt.file <- file.path(data.folder2,graph.file)
	write.graph(graph=g,file=updt.file,format="graphml")
}

