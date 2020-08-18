#!/usr/bin/env Rscript

# libraries required:
library(graph)  # this is located on the bioconductor repo and not CRAN
library(PairViz)
library(igraph)
library(shiny) # needed for dashboarding / the gui

# igraph helper function
requireNamespace("igraph")
igplot <- function(g,weights=FALSE,layout=igraph::layout_in_circle,
                   vertex.size=60, vertex.color="lightblue",...){
    g <- igraph::graph_from_graphnel(as(g, "graphNEL"))
    op <- par(mar=c(1,1,1,1))
    if (weights){
      ew <- round(igraph::get.edge.attribute(g,"weight"),3)
      igraph::plot.igraph(g,layout=layout,edge.label=ew,vertex.size=vertex.size,vertex.color=vertex.color,...)
    }
    else
    igraph::plot.igraph(g,layout=layout,vertex.size=vertex.size,vertex.color=vertex.color,...)
    par(op)
}

# creates a ref class as a represenation of graph nodes
#node <- setRefClass("node", fields=list(id="numeric", connectedTo="list"))



#k4 <- mk_complete_graph(4)
#k5 <- mk_complete_graph(5)
#igplot(k4)
#igplot(k5)
