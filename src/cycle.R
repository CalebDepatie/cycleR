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
  edges_cur  <- graph@edgeL[[LETTERS[cur_node]]]$edges
  edges_next <- graph@edgeL[[LETTERS[next_node]]]$edges

  print(edges_cur)
  print(edges_next)

  for (edge_cur in edges_cur) {
    for (edge_next in edges_next) {
      if (edge_cur == edge_next) {
        print(edge_cur)
        print(edge_next)
        return(edge_cur) # returns the index of the edge
      }
    }
  }


  return(0)
}

# recursive function to compute the hamiltonian cycle
# this is a fairly naive algorithm
recurse_cycle <- function(graph, cur_node, path=c()) {
  line <- node_check(graph, cur_node, cur_node+1)
  if (line == 0) {
    return(c()) # a returned zero means the nodes are NOT adj
  } else {
    if (length(graph@nodes) == cur_node+1) {
      return(append(path, line))
    } else {
      return(recurse_cycle(graph, cur_node+1, path=append(path, line)))
    }
  }
}

# compute the hameltonian path
compute_hamiltonian <- function(graph) {
  # $nodes -> node names
  # $edgeL$<node>$edges -> appears to give edges connected to a node
  # Both edges and nodes have distinct identifiers
  cur_node <- 1 # possible cycle through starting nodes?

  path <- recurse_cycle(graph, cur_node)
  print("")
  print(path)

  return(path) # switch to a custom algorithm
}

# creates a ref class as a represenation of graph nodes
#node <- setRefClass("node", fields=list(id="numeric", connectedTo="list"))



#k4 <- mk_complete_graph(4)
#k5 <- mk_complete_graph(5)
#igplot(k4)
#igplot(k5)
