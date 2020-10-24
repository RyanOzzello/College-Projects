library(igraph)
library(igraphdata)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Read crosslisted data
cl_raw <- as.matrix(read.csv (file = "Cross_Listed_Courses.csv", as.is=TRUE))
cl_pairs <- cl_raw[ ,c(1,2)]

# Plotting margins
par(mar= c(0,0,0,0))

# Crosslisted courses of classes considering prefixes and class number
cl_G <- graph.edgelist(cl_pairs, directed = FALSE)
cl <- plot.igraph(cl_G, vertex.size = 2, edge.width = 5, edge.length = 10)


# Crosslisted courses only considering prefixes, better showing how departments are connected.
cl_pairs_copy <- cl_pairs
cl_pairs_copy[,1] <- substr(cl_pairs_copy[,1], start = 1, stop = 4)
cl_pairs_copy[,2] <- substr(cl_pairs_copy[,2], start = 1, stop = 4)

cl_pref_G <- graph.edgelist(cl_pairs_copy, directed = FALSE)
cl_pref <- plot.igraph(cl_pref_G, vertex.size = 5, edge.width = 3, edge.length = 1)

# Remoce duplicate counts and make adjacency table
Adjtab_cl_pref <- get.adjacency(cl_pref_G)
# As vector
Adjvec_cl_pref <- as.vector(Adjtab_cl_pref)
# As vector without 0s
shortAdjvec_cl_pref <- Adjvec_cl_pref[Adjvec_cl_pref!=0]
shortAdjvec_cl_pref
#graph adjacency 

Adj_cl_pref <- graph.adjacency(Adjtab_cl_pref, mode = "undirected", weighted = TRUE)
plot(Adj_cl_pref)

# Label edges and plot
plot(Adj_cl_pref, edge.label = shortAdjvec_cl_pref)
mar=(c(5,5,5,5))
plot.igraph(Adj_cl_pref, edge.label = c(4,4,2,2,2,14,6,2,2,2,2,2,12)/2, 
            vertex.size = 8, vertex.label.cex = .6, vertex.color="orange" , 
            vertex.label.color="black", vertex.label.dist=1.5 ,
            edge.width = 3, edge.color="grey", edge.label.cex=1.2)




