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

# helper function to check for a path between two nodes
node_check <- function(graph, cur_node, next_node) {
  edges_cur  <- graph@edgeL[LETTERS[cur_node]]
  edges_next <- graph@edgeL[LETTERS[next_node]]

  print(edges_cur)
  print(edges_next)

  return(1)
}

# recursive function to compute the hamiltonian cycle
recurse_cycle <- function(graph, cur_node) {
  node_check(graph, cur_node, cur_node+1)
}

# compute the hameltonian path
compute_hamiltonian <- function(graph) {
  # $nodes -> node names
  # $edgeL$<node>$edges -> appears to give edges connected to a node
  # Both edges and nodes have distinct identifiers
  cur_node <- 1 # possible cycle through starting nodes?

  path <- recurse_cycle(graph, cur_node)

  return(path) # switch to a custom algorithm
}

# creates a ref class as a represenation of graph nodes
#node <- setRefClass("node", fields=list(id="numeric", connectedTo="list"))



#k4 <- mk_complete_graph(4)
#k5 <- mk_complete_graph(5)
#igplot(k4)
#igplot(k5)
