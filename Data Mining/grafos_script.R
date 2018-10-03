# Tutorial and data based on the network visualization workshop published over https://rpubs.com/kateto/netviz
#packages:
#http://ars-uns.blogspot.pe/2015/06/ars-101-empezando-ars-con-r.html

library(igraph)
library(RCurl)

# get the data
nodes <- read.csv("https://raw.githubusercontent.com/danielmarcelino/Datasets/master/Media-NODES.csv", header=T, as.is=T)
ties <- read.csv("https://raw.githubusercontent.com/danielmarcelino/Datasets/master/Media-EDGES.csv", header=T, as.is=T)

# Explore data:
head(nodes);head(ties);nrow(nodes);length(unique(nodes$id));nrow(ties);nrow(unique(ties[,c("from", "to")]))

# Because the data is detailed and not as matrix, we need to simplify it by collapsing multiple edges of the same type between the same two nodes. This can be achieved by summing their "weights", using aggregate() by "from", "to",  and "type":
ties <- aggregate(ties[,3], ties[,-3], sum)
ties <- ties[order(ties$from, ties$to),]
colnames(ties)[4] <- "weight"
rownames(ties) <- NULL

# After the hard work is done, we can convert the data to an igraph object:
net <- graph.data.frame(ties, nodes, directed=T)

# One final touch is to removing loops from the graph, so the edges won't appear that bushy:
net <- simplify(net, remove.multiple = F, remove.loops = T)

# A clean plot: reduced arrow size and remove the labels:
plot(net, edge.arrow.size=.4,vertex.label=NA)

# There are plenty of parameters that can be set, but the most importants are the node & edge options

# 1)  Plot with curved edges (edge.curved=.1) and reduce arrow size:
plot(net, edge.arrow.size=.4, vertex.label=NA, edge.curved=.1)

# 2) nodes' colors. Here we set color to orange and the border color to hex #555555
plot(net, edge.arrow.size=.4, edge.curved=0,
     vertex.color="orange", vertex.frame.color="#555555")


# 3) Replace the vertex label with the node names stored in "media"
plot(net, edge.arrow.size=.4, edge.curved=0,
     vertex.color="orange", vertex.frame.color="#555555",
     vertex.label=V(net)$media, vertex.label.color="black",
     vertex.label.cex=.7)

# Another way to set attributes is to add them to the igraph object.
# 1) Generate colors base on media type:
colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$media.type]

# 2) Compute node degree (#ties) and use it to set node size:
deg <- degree(net, mode="all")
V(net)$size <- deg*3
V(net)$size <- V(net)$audience.size*0.6

# 3) The labels are currently node IDs, setting them to NA will render no labels:
V(net)$label.color <- "black"
V(net)$label <- NA

# 4) We can set edge width based on weight:
E(net)$width <- E(net)$weight/6

#5) changing arrow size and edge color:
E(net)$arrow.size <- .2
E(net)$edge.color <- "gray80"

# We can also override the attributes explicitly inline:
plot(net, edge.color="orange", vertex.color="gray50")

# Don't you think a legend explaining the meaning of the colors is a good idea?:
plot(net)
legend(x=-1.1, y=-1.1, c("Newspaper","Television", "Online News"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2.5, bty="n", ncol=1)

# For now, a final touch would is to highlight areas in the network:
plot(net, mark.groups=list(c(1,4,5,8), c(15:17)), 
     mark.col=c("#C5E5E7","#ECD89A"), mark.border=NA)
















#############################################################################
#############################################################################
####GRAFO DINÁMICO

#############################################################################
#############################################################################
library("networkD3")


library("igraph")





# Download prepared igraph file from github


url <- "https://github.com/andrie/cran-network-structure/blob/master/pdb/depGraph-CRAN.rds?raw=true"


datafile <- tempfile(fileext = ".rds")


download.file(url, destfile = datafile, mode = "wb")


gs <- readRDS(datafile)





# Remove all nodes with fewer than 50 edges


deg <- degree(gs, mode = "out")


idx <- names(which(deg > 50))


gn <- induced.subgraph(gs, idx)





# Extract into data frame and plot


gd <- get.data.frame(gn, what = "edges")


simpleNetwork(gd, fontSize = 12)

