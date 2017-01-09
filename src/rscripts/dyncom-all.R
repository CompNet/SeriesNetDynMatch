# Detection of communities in dynamic TV Series network using static
# approaches then matching communities in consecutive time slices through
# Derek Greene's dynamic-communty method.
# https://github.com/derekgreene/dynamic-community
# 
# In this version of the script, we process all scenes at once... which
# does not work (much too long).
# 
# The post-processing (i.e. extracting the meta-graph once the communities
# have been matched) is much too slow when performed in R, so it was
# re-implemented in Java.
# 
# Usage:
# 1) possibly edit the above parameters
# 2) open an R console
# 3) to install the igraph library, type:
#		install.packages("igraph")
# 3) to set up the current R workspace, type:
#		setwd("D:/Eclipse/workspaces/Networks/Series")
# 4) then, to launch the script, type: 
#		source("src/rscripts/dyncom-all.R")
# 
# Author: Vincent Labatut 12/2016
###############################################################################
library("tools")
library("igraph")
#library("compiler")




# parameters
###############################################################################
#data.folder <- "data/test"							# TODO folder containing the data (relatively to the current R workspace)
#data.folder <- "data/BB_dyn_ns"
data.folder <- "data/GoT_dyn_ns"
#data.folder <- "data/HoC_dyn_ns"
data.folder2 <- paste0(data.folder,"_updt")
dyncom.folder <- "dyncom"							# TODO folder containing the dynamic-community executable files
dyncom.exec <- file.path(dyncom.folder,"tracker")	# TODO executable file of the dynamic-community tracking program 
dyncompostj.folder <- "dyncompostj"					# TODO folder containing the Java class for the postprocessing of dynamic-community results
dyncompostj.exec <- "Main"							# TODO executable file of the dynamic-community tracking program 
static.method <- "Louvain"							# TODO static community detection method
reload <- TRUE										# TODO only perform the last part (detection and tracking already done) 
use.java <- TRUE									# TODO use the Java version of the postprocessing (much faster) 




# get the list of original network files
###############################################################################
graph.files <- list.files(path=data.folder,pattern="*.graphml", all.files=FALSE, full.names=FALSE, recursive=FALSE, ignore.case=FALSE, include.dirs=FALSE, no..=TRUE)
scenes <- sapply(strsplit(graph.files,"[_.]",fixed=FALSE),function(s) as.integer(s[3]))
graph.files <- c(sort(graph.files[scenes<1000]),sort(graph.files[scenes>=1000]))




# apply the static approach to each iteration
###############################################################################
cat("\n\n[",format(Sys.time(),"%a %d %b %Y %X"),"] Detecting the communities\n",sep="")
node.names <- NA
all.coms <- list()
integ.mat <- NA
for(graph.file in graph.files)
{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"]   Processing file '",graph.file,"'\n",sep="")
	
	# read the graphml file
	graphml.file <- file.path(data.folder,graph.file)
	g <- read_graph(file=graphml.file,format="graphml")
	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"]    Number of links: ",gsize(g),"\n",sep="")
	node.names <- V(g)$label
	if(graph.file==graph.files[1])
		integ.mat <- as_adjacency_matrix(graph=g, attr="weight")
	else
		integ.mat <- integ.mat + as_adjacency_matrix(graph=g, attr="weight")
	
	# execute the static approach
	if(reload)
	{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"]    Just loading the file\n",sep="")
		comlist.file <- file.path(getwd(),paste0(file_path_sans_ext(graphml.file),"_cl.txt"))
		comlist.txt <- readLines(comlist.file)
		comlist <- strsplit(comlist.txt,split=" ",fixed=TRUE)
		comlist <- lapply(comlist,as.integer)
		all.coms[[length(all.coms)+1]] <- comlist
		membersp <- rep(NA,gorder(g))
		for(i in 1:length(comlist))
			membersp[comlist[[i]]] <- i 
		V(g)$com <- membersp
	}
	else
	{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"]    Applying community detection method '",static.method,"'\n",sep="")
		if(static.method=="Infomap")
			coms <- cluster_infomap(graph=g, e.weights=E(g)$weight, v.weights=NULL, nb.trials=10, modularity=FALSE)
		else if(static.method=="EdgeBetweenness")
			coms <- cluster_edge_betweenness(graph=g, weights=E(g)$weight, directed=FALSE, membership=TRUE)
		else if(static.method=="FastGreedy")
			coms <- cluster_fast_greedy(graph=g, membership=TRUE) 		# no weights...
		else if(static.method=="LabelPropagation")
			coms <- cluster_label_prop(graph=g, weights=E(g)$weight)
		else if(static.method=="SpinGlass")
			coms <- cluster_spinglass(graph=g,weights=E(g)$weight)		# needs a connected graph
		else if(static.method=="leadingEigenvector")
			coms <- cluster_leading_eigen(graph=g,weights=E(g)$weight)
		else if(static.method=="Louvain")
			coms <- cluster_louvain(graph=g,weights=E(g)$weight)
		else if(static.method=="WalkTrap")
			coms <- cluster_walktrap(graph=g,weights=E(g)$weight,membership=TRUE)
		
		# record as a list of communities
		comlist.file <- file.path(getwd(),paste0(file_path_sans_ext(graphml.file),"_cl.txt"))
		comlist <- communities(coms)
		all.coms[[length(all.coms)+1]] <- comlist
		comstr <- sapply(comlist,function(v) paste(v,collapse=" "))
		writeLines(comstr,con=comlist.file)
		
		# add as an attribute to the graph and record a copy
		dir.create(data.folder2, showWarnings=FALSE, recursive=TRUE)
		V(g)$com <- membership(coms)
		updt.file <- file.path(data.folder2,graph.file)
		cat("[",format(Sys.time(),"%a %d %b %Y %X"),"]    Recording file '",updt.file,"'\n",sep="")
		write.graph(graph=g,file=updt.file,format="graphml")
	}
}
# record the names
char.file <- file.path(data.folder,"characters.txt")
write.table(x=node.names,file=char.file,row.names=FALSE,col.names=FALSE)




# track the communities
###############################################################################
cat("\n\n[",format(Sys.time(),"%a %d %b %Y %X"),"] Tracking the communities\n",sep="")
out.file <- file.path(data.folder,"all")
if(!reload)
{	com.files <- file.path(data.folder,"*_cl.txt")
	treshod <- 0.3
	cmd <- paste0(dyncom.exec," -t ",treshod," -o ",out.file," ",com.files)
	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"]  Executing command '",cmd,"'\n",sep="")
	system(cmd)
}




# process the overall centrality (to later identify the main nodes) 
###############################################################################
cat("\n\n[",format(Sys.time(),"%a %d %b %Y %X"),"] Process centrality in aggregated network\n",sep="")
integ.g <- graph_from_adjacency_matrix(adjmatrix=integ.mat, mode="undirected", weighted=TRUE)
cent <- eigen_centrality(graph=integ.g, scale=TRUE, weights=E(integ.g)$weight)$vector
cent.file <- file.path(data.folder,paste0("all_centr.txt"))
write.table(x=cent,file=cent.file,row.names=FALSE,col.names=FALSE)




# convert to a meta-graph format
###############################################################################
cat("\n\n[",format(Sys.time(),"%a %d %b %Y %X"),"] Convert to a meta-graph\n",sep="")
if(use.java)
{	cmd <- paste0("java -Xmx8G -classpath ",dyncompostj.folder," ",dyncompostj.exec," \"",data.folder,"\" 0")
	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Using the Java version of the post-processing '",cmd,"'\n",sep="")
	system(cmd)
	
	result.file <- file.path(data.folder2,"all.graphml")
	g <- read.graph(file=result.file,format="graphml");
	V(g)$x <- V(g)$x * 100 
	V(g)$y <- V(g)$y * 100
	V(g)$size <- 20 
	E(g)$arrow.size <- 0.1
	V(g)$label.cex <- 0.1
	plot(g,edge.label=NA)
	second.file <- file.path(data.folder2,paste0("all_bis.graphml"))
	write.graph(g,file=second.file,format="graphml")
	second.file <- file.path(data.folder2,paste0("all_bis.net"))
	write.graph(g,file=second.file,format="pajek")
#	stop()
}else
{	# load the file
	str <- readLines(con=paste0(out.file,".timeline"))
	# build an equivalent matrix
	cat("\n\n[",format(Sys.time(),"%a %d %b %Y %X"),"]   Build the matrix (",length(str),"x",length(graph.files),")\n",sep="")
	m <- matrix(0,nrow=length(str),ncol=length(graph.files))
	for(i in 1:length(str))
	{	tmp <- strsplit(str[i],split=":",fixed=TRUE)[[1]][2]
		tmp <- strsplit(tmp,split=",",fixed=TRUE)[[1]]
		for(cpl in tmp)
		{	tmp2 <- as.integer(strsplit(cpl,split="=",fixed=TRUE)[[1]])
			m[i,tmp2[1]] <- tmp2[2]
		}
	}
	# define the corresponding meta-graph: one meta-node represents one community at a given time slice
	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"]    Define meta graph\n",sep="")
	g <- make_empty_graph(n=0, directed=TRUE)
	for(i in 1:nrow(m))
	{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"]    Process row ",i,"/",nrow(m),"\n",sep="")
		prev.name <- NA
		prev.com <- NA
		prev.id <- 0
		for(j in 1:ncol(m))
		{	if(m[i,j]==0)
			{	prev.name <- NA
				prev.com <- NA
				prev.id <- 0
			}
			else
			{	name <- paste0(j,"-",m[i,j])
				com.nodes <- all.coms[[j]][[m[i,j]]]
				if((name %in% V(g)$name))
					cur.id <- which(V(g)$name==name)
				else
				{	content <- paste0("(",paste(node.names[com.nodes],collapse="), ("),")")
					main.node <- which.max(cent[com.nodes])[1]
					label <- node.names[com.nodes[main.node]]
					avg.centr <- mean(cent[com.nodes])
					max.centr <- max(cent[com.nodes])
					g <- add_vertices(graph=g,nv=1,attr=list(
							name=vertices(name),
							x=j,#*10,
							y=i,#*10,
							content=content,
							label=label,
							weight=length(com.nodes),
							avgcentr=avg.centr,
							maxcentr=max.centr
						))
					cur.id <- gorder(g)
				}
				if(!is.na(prev.name))
				{	inter.com <- intersect(prev.com,com.nodes)
					content <- paste0("(",paste(node.names[inter.com],collapse="), ("),")")
					main.node <- which.max(cent[inter.com])[1]
					label <- node.names[inter.com[main.node]]
					g <- add_edges(graph=g,edges=c(prev.id,cur.id),attr=list(
								content=content,
								label=label,
								weight=length(inter.com)
							))
				}
				prev.name <- name
				prev.com <- com.nodes
				prev.id <- cur.id
			}
		}
	}
	g <- simplify(graph=g,remove.multiple=TRUE,edge.attr.comb="first")
	result.file <- file.path(data.folder2,"all.graphml")
	write.graph(graph=g,file=result.file,format="graphml")
	#plot(g)
}
