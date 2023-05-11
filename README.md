[![codecov](https://codecov.io/gh/BershteynLab/EMODAnalyzeR/branch/main/graph/badge.svg?token=G6NWONEW83)](https://codecov.io/gh/BershteynLab/EMODAnalyzeR)

# EMODAnalyzeR

This package intends to help members of the Bershteyn Lab analyze results of emod. This package was developed by members of the Bershteyn lab and Edinah Mudimu.

[View Documentation Here](https://bershteynlab.github.io/EMODAnalyzeR/)

## Installation

1. Make sure you are running R 4.0 or greater
2. Make sure you have devtools installed (if not, run `install.packages("devtools")` )
3. Run `library(devtools)`
4. Run `install_github("BershteynLab/EMODAnalyzeR")`

## Usage

    library(EMODAnalyzeR)
    data = read.simulation.results(<path to runscenarios output folder>, 'Baseline', c("Infected", "Population"), min_age_inclusive = 15, max_age_inclusive = 49)
    plot.prevalence(data, 2000, 2040)
    
