A Hamiltonian Path visualizer written in R, using Shiny for the GUI peice.

![cycleR image](/snips/cycleR.png)

The current implementation has many limitations (such as largely relying on iteration and using a naive hamiltonian algorithm), but ultimately it works and does so without a large performance penalty. It should not return false negatives.

Eventually this will be written to be a Hamiltonian *Cycle* visualizer.

The following libraries are required:

- "shiny"
- "graph" - This is on the bioconductor repo and not CRAN
- "PairViz"
- "igraph"

This app must be ran using via the commands in an R session:
```R
library(shiny)
runApp("src")
```
