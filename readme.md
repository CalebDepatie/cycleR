A Hamiltonian Cycle visualizer written in R, using Shiny for the GUI peice.

![cycleR image](/snips/cycleR.png)

The current implementation has many limitations (such as largely relying on iteration and using a naive hamiltonian algorithm), but ultimately it works and does so without a large performance penalty. It should not return false negatives.

This app must be ran using via the commands in an R session:
```R
library(shiny)
runApp("src")
```
