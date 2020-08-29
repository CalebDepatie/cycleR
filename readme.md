A Hamiltonian Cycle visualizer written in R, using Shiny for the GUI peice.

![cycleR image](/snips/cycleR.png)

The current implementation has many limitations (such as largely relying on iteration), but ultimately it works and does so without a large performance penalty. It currently has a possibilty to give a false negative, although I have not found it to be common.

This app must be ran using via the commands in an R session:
```R
library(shiny)
runApp("src")
```
