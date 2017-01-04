# Detection of communities in dynamic TV Series network using static
# approaches then matching communities in consecutive time slice through
# a naive approach.
# 
# In this version of the script, we process each season separately.
# 
# Usage:
# 1) possibly edit the above parameters
# 2) open an R console
# 3) to install the igraph library, type:
#		install.packages("igraph")
# 3) to set up the current R workspace, type (replace by your own path to this project):
#		setwd("D:/Eclipse/workspaces/Networks/Series")
# 4) then, to launch the script, type: 
#		source("custom-match.R")
# 
# Author: Vincent Labatut 12/2016
###############################################################################
library("tools")		# basic R functions
library("igraph")		# to handle graphs
#library("compiler")	# to speed up the code
library("alluvial")		# to generate the alluvial diagrams 




# parameters
###############################################################################
#data.folder <- "test"								# TODO folder containing the data (relatively to the current R workspace)
data.folder <- "BB_dyn_ns"
#data.folder <- "GoT_dyn_ns"
#data.folder <- "HoC_dyn_ns"
data.folder2 <- paste0(data.folder,"_updt")
static.method <- "Louvain"							# TODO static community detection method
use.cache <- TRUE									# TODO use the cached file instead of community detection (only use if community detection was performed once before) 
min.jacc <- 0.3 									# TODO value of Jaccard's coefficient above which two communities are considered similar
min.size <- 4										# TODO below this size, we don't try to match the communities
min.cent <- 0.2										# TODO below this centrality, the nodes are omitted from the simplified labels (so this option is purely graphical)




# function processing Jaccard's coefficient (a weighted version)
###############################################################################
jaccard <- function(com1, com2, weights)
{	inter.com <- intersect(com1,com2)
	union.com <- union(com1,com2)
	
#	result <- length(intcom)/length(uncom)						# regular (unweighted) Jaccard
	result <- sum(weights[inter.com])/sum(weights[union.com]) 	# weighted Jaccard
	return(result)	
}




# get the list of original network files
###############################################################################
all.graph.files <- list.files(path=data.folder,pattern="*.graphml", all.files=FALSE, full.names=FALSE, recursive=FALSE, ignore.case=FALSE, include.dirs=FALSE, no..=TRUE)
tmp <- strsplit(all.graph.files,split="_",fixed=TRUE)
tmp2 <- sapply(tmp,function(v)v[2])
seasons <- as.integer(substr(x=tmp2,start=2,stop=3))




# process each season separately
###############################################################################
#for(season in seasons)
for(season in c(1))
{	graph.files <- all.graph.files[seasons==season]
	
	# apply the static approach to each iteration
	###############################################################################
	cat("\n\n[",format(Sys.time(),"%a %d %b %Y %X"),"] Detecting the communities for season #",season,"\n",sep="")
	node.names <- NA
	all.coms <- list()
	all.membersp <- NA
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
		if(use.cache)
		{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"]    Just loading the file\n",sep="")
			comlist.file <- file.path(getwd(),paste0(file_path_sans_ext(graphml.file),"_cl.txt"))
			comlist.txt <- readLines(comlist.file)
			comlist <- strsplit(comlist.txt,split=" ",fixed=TRUE)
			comlist <- lapply(comlist,as.integer)
			comlist2 <- list()
			for(c in 1:length(comlist))
				comlist2[[paste(length(all.coms)+1,"-",c,sep="")]] <- comlist[[c]]
			all.coms[[length(all.coms)+1]] <- comlist2
			membersp <- rep(NA,gorder(g))
			for(i in 1:length(comlist))
				membersp[comlist[[i]]] <- i 
			V(g)$com <- membersp
			if(all(is.na(all.membersp)))
				all.membersp <- membersp
			else
				all.membersp <- cbind(all.membersp,membersp)
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
			comlist2 <- list()
			for(c in 1:length(comlist))
				comlist2[[paste(length(all.coms)+1,"-",c,sep="")]] <- comlist[[c]]
			all.coms[[length(all.coms)+1]] <- comlist2
			comstr <- sapply(comlist2,function(v) paste(v,collapse=" "))
			writeLines(comstr,con=comlist.file)
			
			# add as an attribute to the graph and record a copy
			dir.create(data.folder2, showWarnings=FALSE, recursive=TRUE)
			V(g)$com <- membership(coms)
			updt.file <- file.path(data.folder2,graph.file)
			cat("[",format(Sys.time(),"%a %d %b %Y %X"),"]    Recording file '",updt.file,"'\n",sep="")
			write.graph(graph=g,file=updt.file,format="graphml")
		}
	}
	colnames(all.membersp) <- NULL
	# record the names
	char.file <- file.path(data.folder,"characters.txt")
	write.table(x=node.names,file=char.file,row.names=FALSE,col.names=FALSE)
	
	
	
	
	# process the overall centrality (to later identify the main nodes) 
	###############################################################################
	cat("\n\n[",format(Sys.time(),"%a %d %b %Y %X"),"] Process centrality in aggregated network (season #",season,")\n",sep="")
	integ.g <- graph_from_adjacency_matrix(adjmatrix=integ.mat, mode="undirected", weighted=TRUE)
	cent <- eigen_centrality(graph=integ.g, scale=TRUE, weights=E(integ.g)$weight)$vector
	cent.file <- file.path(data.folder,paste0("season",season,"_centr.txt"))
	write.table(x=cent,file=cent.file,row.names=FALSE,col.names=FALSE)
	
	
	
	
	# track the communities using a custom method
	###############################################################################
	cat("\n\n[",format(Sys.time(),"%a %d %b %Y %X"),"] Tracking the communities for season #",season,"\n",sep="")
	
#	# only keep communities larger than a certain size
#	filtered.coms <- lapply(all.coms,function(time.coms) time.coms[which(sapply(time.coms,length)>3)])
	
	# links between communities in the final graph 
	link.mat <- NA
	el <- NA
	
	# process each pair of consecutive time slices
	for(t in 2:length(all.coms))
	{	# get the previous and current communities
		coms1 <- all.coms[[t-1]]
		coms2 <- all.coms[[t]]
		# process each community in the current time step
		for(c2 in 1:length(coms2))
		{	com2 <- coms2[[c2]]
			# only deal with communities which are large enough
			if(length(com2)>=min.size)
			{	name2 <- paste(t,"-",c2,sep="")
				# process each community in the previous time step
				for(c1 in 1:length(coms1))
				{	com1 <- coms1[[c1]]
					# only deal with communities which are large enough
					if(length(com1)>=min.size)
					{	name1 <- paste(t-1,"-",c1,sep="")
						
						# process the potential connection between the current and previous communities 
						jacc <- jaccard(com1,com2,cent)
						if(jacc>=min.jacc)
						{	# only the first time
							if(all(is.na(link.mat)))
							{	link.mat <- matrix(c(t-1,c1,t,c2,jacc),ncol=5)
								el <- matrix(c(name1,name2),ncol=2)
							}
							# general case (matrices already exist)
							else
							{	link.mat <- rbind(link.mat,c(t-1,c1,t,c2,jacc))
								el <- rbind(el,c(name1,name2))
							}
						}
					}
				}
			}
		}
	}
	
	# build the graph
	g <- graph_from_edgelist(el=el,directed=TRUE)
	
	# complete the graph with node attributes
	for(i in 1:gorder(g))
	{	cpl <- as.integer(strsplit(V(g)[i]$name,"-",fixed=TRUE)[[1]])
		com.nodes <- all.coms[[cpl[1]]][[cpl[2]]]
		content <- paste0("(",paste(node.names[com.nodes],collapse="), ("),")")
		V(g)[i]$content <- content
		com.nodes.simpl <- com.nodes[which(cent[com.nodes]>min.cent)]
		content.smpl <- paste0("(",paste(node.names[com.nodes.simpl],collapse="), ("),")")
		V(g)[i]$contentsmpl <- content.smpl
		main.node <- which.max(cent[com.nodes])[1]
		label <- node.names[com.nodes[main.node]]
		V(g)[i]$label <- label
		avg.centr <- mean(cent[com.nodes])
		V(g)[i]$avgcentr <- avg.centr
		max.centr <- max(cent[com.nodes])
		V(g)[i]$maxcentr <- max.centr
		V(g)[i]$weight <- length(com.nodes)
	}
	
	# complete the graph with link attributes
	for(i in 1:nrow(link.mat))
	{	com1 <- all.coms[[link.mat[i,1]]][[link.mat[i,2]]]
		com2 <- all.coms[[link.mat[i,3]]][[link.mat[i,4]]]
		inter.com <- intersect(com1,com2)
		content <- paste0("(",paste(node.names[inter.com],collapse="), ("),")")
		E(g)[i]$content <- content
		inter.com.simpl <- inter.com[which(cent[inter.com]>min.cent)]
		content.smpl <- paste0("(",paste(node.names[inter.com.simpl],collapse="), ("),")")
		E(g)[i]$contentsmpl <- content.smpl
		main.node <- which.max(cent[inter.com])[1]
		label <- node.names[inter.com[main.node]]
		E(g)[i]$label <- label
		E(g)[i]$weight=length(inter.com)
		E(g)[i]$jaccard=link.mat[i,5]		
	}
	
#	# process positions
#	V(g)$x <- as.numeric(sapply(V(g)$name, function(nm) strsplit(nm,"-",fixed=TRUE)[[1]][1]))
#	done <- c()
#	roots <- which(degree(g,mode="in")==0)
#	V(g)[roots]$y <- 1:length(roots)*100
#	V(g)[roots]$ysup <- V(g)[roots]$y + 100
#	fringe <- roots
#	while(length(fringe)>0)
#	{	node <- fringe[1]
#		fringe <- fringe[-1]
#		if(!(node %in% done))
#		{	done <- c(done,node)
#			# get neighbors		
#cat("node=",node,"\n",sep="")	
#			neighs <- ego(graph=g,order=1,nodes=node,mode="out")[[1]][-1]
#cat("  neighs=",paste(neighs,collapse=" ")," (",length(neighs)>1,")\n",sep="")	
#			# update positions
#cat("  y=",V(g)[node]$y," ysup=",V(g)[node]$ysup,"\n",sep="")
#			if(length(neighs)==1)
#			{	V(g)[neighs]$y <- V(g)[node]$y
#				V(g)[neighs]$ysup <- V(g)[node]$ysup
#				fringe <- c(fringe,neighs)
#			}
#			else if(length(neighs)>1)
#			{	# identify the most similar child
#				same <- 1
#				same.jacc <- edge_attr(g,"jaccard",E(g)[node %->% neighs[1]])
#				for(n in 2:length(neighs))
#				{	jacc <- edge_attr(g,"jaccard",E(g)[node %->% neighs[n]])
#					if(jacc>same.jacc)
#					{	same <- n
#						same.jacc <- jacc
#					}
#				}
#cat("  same=",same," same.jacc=",same.jacc,"\n",sep="")
#				neighs <- c(neighs[same],neighs[-same])
##				V(g)[neighs[same]]$y <- V(g)[node]$y
##				V(g)[neighs[same]]$ysup <- V(g)[node]$ysup
##				fringe <- c(fringe,neighs[same])
##				neighs <- neighs[-same]
#				# process the rest of the children
#				pos <- seq(from=V(g)[node]$y,to=V(g)[node]$ysup,by=(V(g)[node]$ysup-V(g)[node]$y)/length(neighs))
#				V(g)[neighs]$y <- pos[1:(length(pos)-1)]
#				V(g)[neighs]$ysup <- pos[2:length(pos)]
#				fringe <- c(fringe,neighs)
#cat("  neighs=",paste(neighs,collapse=" "),"\n",sep="")
#cat("  y=",paste(pos[2:(length(pos)-1)],collapse=" "),"\n",sep="")	
#cat("  ysup=",paste(pos[3:length(pos)],collapse=" "),"\n",sep="")	
#			}
#		}
#	}
#	g <- delete_vertex_attr(g,"ysup")
#	# normalize y positions
#	vals <- sort(unique(V(g)$y))
#	idx <- match(V(g)$y,vals)
#	V(g)$y <- idx
	
	# process positions
	V(g)$x <- as.numeric(sapply(V(g)$name, function(nm) strsplit(nm,"-",fixed=TRUE)[[1]][1]))
	# init position matrix
	t <- 1
	while(length(which(V(g)$x==t))==0)
		t <- t + 1
	roots <- which(V(g)$x==t)
	pos.mat <- matrix(NA,nrow=length(roots),ncol=length(all.coms))
	pos.mat[,t] <- V(g)[roots]$name
	# update position matrix
	for(t in (t+1):length(all.coms))
	{	nodes <- which(V(g)$x==t)
		# get previous nodes
		prev <- rep(NA,nrow(pos.mat))
		t0 <- t
		while(t0>0 && any(is.na(prev)))
		{	idx <- which(is.na(prev))
			prev[idx] <- pos.mat[idx,t0]
			t0 <- t0 - 1
		}
		if(any(is.na(prev)))
			prev <- prev[-which(is.na(prev))]
		prev <- as.numeric(V(g)[prev])
#		prev <- which(V(g)$x==(t-1))
		# first, one-to-one association should be favored for stability matters
		singles <- which(degree(g,V(g)[nodes],mode="in")==1)
		for(tgt in nodes[singles])
		{	src <- ego(g,1,tgt,mode="in")[[1]][-1]
			if(degree(g,V(g)[src],mode="out")==1)
			{	pos <- which(pos.mat[,t-1]==V(g)[src]$name)
				pos.mat[pos,t] <- V(g)[tgt]$name
				prev <- prev[-which(prev==src)]
				nodes <- nodes[-which(nodes==tgt)]
			}
		}
		# second, identify the best associations for remaining non-root nodes
		links <- E(g)[prev %->% nodes]
		if(length(links)>0)
		{	jacc <- edge_attr(g,"jaccard",links)
			idx <- order(jacc,decreasing=TRUE)
			for(i in idx)
			{	link <- links[i]
				src <- head_of(g, link)
				tgt <- tail_of(g, link)
				if(src %in% prev & tgt %in% nodes)
				{	pos <- which(pos.mat[,t-1]==V(g)[src]$name)
					pos.mat[pos,t] <- V(g)[tgt]$name
					prev <- prev[-which(prev==src)]
					nodes <- nodes[-which(nodes==tgt)]
				}
			}
		}
		# third, process the remaining non-root nodes
		nonroots <- which(degree(g,V(g)[nodes],mode="in")>0)
		for(tgt in nodes[nonroots])
		{	links <- E(g)[prev %->% tgt]
			if(length(links)>0)
			{	jacc <- edge_attr(g,"jaccard",links)
				idx <- which.max(jacc)
				link <- links[idx]
				src <- head_of(g, link)
				pos <- which(pos.mat[,t-1]==V(g)[src]$name)
				pos.mat[pos,t] <- V(g)[tgt]$name
				prev <- prev[-which(prev==src)]
				nodes <- nodes[-which(nodes==tgt)]
			}
		}
		# fourth, find the best position for the remaining nodes (roots or orphans)
		if(length(prev)>0 && length(nodes)>0)
		{	pairs <- as.matrix(expand.grid(prev,nodes))
			jacc <- apply(pairs,1,function(pair) 
					{	t0 <- as.numeric(strsplit(V(g)[pair[1]]$name,"-",fixed=TRUE)[[1]][1])
						jaccard(all.coms[[t0]][[V(g)[pair[1]]$name]], all.coms[[t]][[V(g)[pair[2]]$name]],cent)
					})
			if(any(jacc>=min.jacc))
			{	jacc <- jacc[-which(jacc<min.jacc)]
				idx <- order(jacc,decreasing=TRUE)
				for(i in idx)
				{	src <- pairs[i,1]
					tgt <- pairs[i,2]
					if(src %in% prev & tgt %in% nodes)
					{	t0 <- as.numeric(strsplit(V(g)[src]$name,"-",fixed=TRUE)[[1]][1])
						pos <- which(pos.mat[,t0]==V(g)[src]$name)
						pos.mat[pos,t] <- V(g)[tgt]$name
						prev <- prev[-which(prev==src)]
						nodes <- nodes[-which(nodes==tgt)]
					}
				}
			}
		}
		# fifth, create new positions for the rest
		if(length(nodes)>0)
		{	tmp <- matrix(NA,nrow=length(nodes),ncol=length(all.coms))
			tmp[,t] <- V(g)[nodes]$name
			pos.mat <- rbind(tmp,pos.mat)
		}
	}
	# use position matrix to setup nodes y position
	for(i in 1:nrow(pos.mat))
	{	for(j in 1:ncol(pos.mat))
		{	idx <- pos.mat[i,j]
			if(!is.na(idx))
				V(g)[idx]$y <- i
		}
	}
	
	# format
	V(g)$x <- V(g)$x * 100 
	V(g)$y <- V(g)$y * 100
	V(g)$size <- 20 
	E(g)$arrow.size <- 0.1
	V(g)$label.cex <- 0.1
	plot(g,edge.label=NA)
	#tkplot(g,edge.label=NA,vertex.label=V(g)$name,vertex.size=5)
	#plot(g,edge.label=NA,rescale=FALSE,axes=FALSE,xlim=c(min(V(g)$x),max(V(g)$x)),ylim=c(min(V(g)$y),max(V(g)$y)),asp=NA)
	
	# record
	result.file <- file.path(data.folder2,paste0("season",season,".graphml"))
	write.graph(graph=g,file=result.file,format="graphml")
	
	
	
	
	# generate an alluvial diagram
	###############################################################################
	cat("\n\n[",format(Sys.time(),"%a %d %b %Y %X"),"] Generate the alluvial diagram for season #",season,"\n",sep="")
	
	# replace the com id in all.membersp depending on the previously identified vertical positions
	all.membersp.chg <- matrix(NA,nrow=nrow(all.membersp),ncol=ncol(all.membersp))
	for(t in 1:ncol(pos.mat))
	{	i <- 1
		old.coms <- order(unique(all.membersp[,t]))
		for(c in 1:nrow(pos.mat))
		{	com <- pos.mat[c,t]
			if(!is.na(com))
			{	old.com <- as.integer(strsplit(com,"-",fixed=TRUE)[[1]])[2]
				old.coms <- old.coms[-which(old.coms==old.com)]
				idx <- which(all.membersp[,t]==old.com)
				all.membersp.chg[idx,t] <- sprintf("%04d",i)
				i <- i + 1
			}
		}
		for(old.com in old.coms)
		{	idx <- which(all.membersp[,t]==old.com)
			all.membersp.chg[idx,t] <- sprintf("%04d",i)
			i <- i + 1
		}
	}

	# index of characters always appearing in singletons
	single.mat <- matrix(FALSE,nrow=nrow(all.membersp),ncol=ncol(all.membersp))
	for(t in 1:ncol(all.membersp.chg))
	{	tb <- table(all.membersp.chg[,t])
		sgl.coms <- names(tb)[which(tb==1)]
		idx <- which(!is.na(match(all.membersp.chg[,t],sgl.coms)))
		single.mat[idx,t] <- TRUE
	}
	
	# convert the resulting matrix to dataframe, table and all the things the alluvial package needs.
	t.limit <- 40
	sel.t <- 1:t.limit
	singletons <- apply(single.mat[,sel.t],1,all)
	sel.chars <- (1:nrow(all.membersp.chg))[!singletons]
	data <- data.frame(apply(all.membersp.chg[sel.chars,sel.t],2,as.character))
	colnames(data) <- sapply(strsplit(graph.files[sel.t],"[_.]",fixed=FALSE),function(s) as.integer(s[3]))
	colors <- rep("BLUE",length(sel.chars))
	colors[which(node.names[sel.chars]=="Jesse Pinkman")] <- "RED"
	colors[which(node.names[sel.chars]=="Walter White")] <- "PURPLE"
	data[["freq"]] <- 1
	alluvial(data[,1:(ncol(data)-1)],
			freq=data[,ncol(data)],
			col=colors
	)
	
#	data.frame(apply(matrix(sample(1:4,30,replace=T),nrow=6),2,as.character))
#	x <- data.frame(factor(1:10),factor(c(rep(1,5),rep(2,5))),factor(sample(1:3,10,replace=TRUE)))
#	colnames(x) <- c("a","b","c")
#	df <- as.data.frame(table(x))
#	alluvial(df[,1:3],freq=df[,4])
	
	
	
}

# TODO
# - sort the coms numbers so that the graphical order matches the... matches. smaller ids are at the bottom
# - gather all the singletons in the same fake com ?