library(shiny)

source("cycle.R")

# shiny UI component
ui <- fluidPage(

  # App title ----
  titlePanel("cycleR"),


  # Sidebar layout
  sidebarLayout(
    position = "right", # set the sidebar to the right side of the page

    # Sidebar panel
    sidebarPanel(
      p("cycleR is a visual demonstration of a hamiltonian cycle, and it allows you to customize the nodes. Tweak these settings and explore the output! (Graphs without a blue 'path' are not Hamiltonian graphs)"),

      # options inputs
      sliderInput(inputId = "nodes",
                  label = "Number of nodes:",
                  min = 2,
                  max = 10,
                  value = 4),

      sliderInput(inputId = "cons",
                  label = "Outbound connections per node:",
                  min = 1,
                  max = 5,
                  value = 2),


      actionButton("refresh", "Refresh") # procs the graph to refresh, which should rerandomize
    ),

    # Main panel
    mainPanel(

      plotOutput(outputId = "nodePlot")

    )
  )
)

# shiny server component
server <- function(input, output) {

  observeEvent(input$refresh, {

    output$nodePlot <- renderPlot({
      # generates the output plot semi randomly
      node_names <- LETTERS[1:input$nodes]
      graph      <- new("graphNEL",nodes=node_names)
      # pre allocates vectors to be numeric vecs of the same size of the total cons
      n_to       <- character(input$nodes*input$cons)
      n_from     <- character(input$nodes*input$cons)
      n_weights  <- numeric(input$nodes*input$cons) # weights are currently all equal
      index      <- 1
      for (node_i in 1:input$nodes) {
        for (con_i in 1:input$cons) {
          out <- sample(1:input$nodes, 1)
          # dont have connections that go to themselves
          while (node_i == out) {
            out <- sample(1:input$nodes, 1)
          }

          n_from[index]    <- node_names[node_i]
          n_to[index]      <- node_names[out]
          n_weights[index] <- 1

          index = index + 1
        }
      }

      graph <- addEdge(from=n_from, to=n_to, graph=graph, weights=n_weights)

      # hameltonian stuff
      edge_weights       <- rep(1, length(edgeNames(graph)))
      path               <- compute_hamiltonian(graph, n_from, n_to)
      edge_weights[path] <- 5
      edge_colour        <- rep("grey40", length(edge_weights))
      edge_colour[path]  <- "cyan"

      igplot(graph, edge.width=edge_weights, edge.color=edge_colour)
      })

  })

}

# runs the app with the given objects
shinyApp(ui=ui, server=server)
