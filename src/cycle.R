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
    g  <- igraph::graph_from_graphnel(as(g, "graphNEL"))
    op <- par(mar=c(1,1,1,1))
    if (weights){
      ew <- round(igraph::get.edge.attribute(g,"weight"),3)
      igraph::plot.igraph(g,layout=layout,edge.label=ew,vertex.size=vertex.size,vertex.color=vertex.color,...)
    }
    else
    igraph::plot.igraph(g,layout=layout,vertex.size=vertex.size,vertex.color=vertex.color,...)
    par(op)
}

# helper function for sorting
sort_pass <- function(vec) {
  for (i in 1:(length(vec)-1)) {
    if (vec[[i+1]][1] < vec[[i]][1]) {
      tmp        <- vec[[i]]
      vec[[i]]   <- vec[[i+1]]
      vec[[i+1]] <- tmp
    }
  }
  return(vec)
}

# helper function to manually sort my list of vectors and not lose placement ordering
# uses bubble sort, could change to a more efficient algo later, but unnessecary with such constrained data
vec_sort <- function(vec) {
  new_vec <- sort_pass(vec)
  if (isTRUE(all.equal(vec, new_vec))) {
    return(new_vec)
  } else {
    return(vec_sort(new_vec))
  }
}

# helper function to check for a path between two nodes
node_check <- function(graph, cur_node, next_node, edges) {
  cur <- LETTERS[cur_node]
  nxt <- LETTERS[next_node]

  for (i in 1:length(edges)) {
    cur_edge <- edges[[i]]
    if ((cur %in% cur_edge) & (nxt %in% cur_edge)) {
      return(i)
    }
  }

  return(0)
}

# recursive function to compute the hamiltonian cycle
# this is a fairly naive algorithm
recurse_cycle <- function(graph, cur_node, nodes_left, edges, path=c()) {
  print(nodes_left)
  next_node <- 0
  line      <- 0
  for (next_node in nodes_left) {
    line <- node_check(graph, cur_node, next_node, edges) # possibly change to not requiring linearly moving through nodes
    if (line != 0) {
      if (length(nodes_left) == 1) {
        return(append(path, line))
      }
      tmp <- recurse_cycle(graph, next_node, nodes_left[!nodes_left %in% next_node], edges, path=append(path, line))
      if (is.null(tmp)) {
        # if the cycle on this path ended up not finding a proper path, try with the next valid node
        next
      } else {
        return(tmp)
      }
    } else {
      # if node_check did not find a valid path try again
      next
    }
  }
  # if something goes wrong it wasnt able to find a valid path
  return(c())
}

# compute the hameltonian path
compute_hamiltonian <- function(graph, to, from) {
  # $nodes -> node names
  # $edgeL$<node>$edges -> appears to give edges connected to a node
  # Both edges and nodes have distinct identifiers
  #  *----*
  # The way the edges are indexed is inconsistent with any direct method of getting them :
  # It's indexed by Node, if A has 4 edges the first 4 indexes will be those edges, irrespective of what
  # the S4 attribute gives how or (mostly) how they are structured in the from and to
  # After taking into account the node priority, the indexing does appear to be based on where they were initially structured
  #  *----*

  # create the edges object for use in the cycle
  edges <- list()
  # can make the assumption that len of to == len of from
  for (i in 1:length(to)) {
    # create and sort the inner vec before appending, so its already alphabetically and closer to the format of the edge indexing
    inner_vec  <- c(to[i], from[i])
    inner_vec  <- sort(inner_vec)
    edges[[i]] <- inner_vec
  }

  # drop all non unique values because those dont get indexed
  edges    <- unique(edges)
  # manually sort the vec by the first value in each of the inner vec, without losing placement order
  edges    <- vec_sort(edges)
  #print(edges)

  nodes_left <- 1:length(graph@nodes)
  for (cur_node in nodes_left) { # cycles through starting node
    path <- recurse_cycle(graph, cur_node, nodes_left[!nodes_left %in% cur_node], edges)
    if (!is.null(path)) {
      break
    }
  }

  print("")
  print(path)

  return(path) # switch to a custom algorithm
}
