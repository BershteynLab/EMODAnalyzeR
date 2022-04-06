.. documentation master file, created by sphinx-quickstart
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

EMODAnalyzeR
================================

.. raw:: html

    <style> .red {color:red} </style>

.. role:: red


This package intends to help members of the Bershteyn Lab analyze results of emod. This package was developed by members of the Bershteyn lab and Edinah Mudimu.

Installation
================================

1. Make sure you are running R 4.0 or greater
2. Make sure you have devtools installed (if not, run `install.packages("devtools")` )
3. Run `library(devtools)`
4. Run `install_github("BershteynLab/EMODAnalyzeR")`

As of 2022-03-10, installation problems might come up as it has mostly been tested on David Kaftan's machine.

Usage
================================

    library(EMODAnalyzeR)
    data = read.simulation.results(<path to runscenarios output folder>, 'Baseline', c("Infected", "Population"), min_age_inclusive = 15, max_age_inclusive = 49)
    plot.prevalence(data, 2000, 2040)



.. toctree::
   :maxdepth: 1
   :caption: Vignettes



.. toctree::
   :maxdepth: 1
   :caption: Functions

   functions/calculate.bounds.effective_count.md
   functions/calculate.bounds.two_sigma.md
   functions/calculate.DALY.md
   functions/calculate.incidence.md
   functions/calculate.pop_scaling_factor.md
   functions/read.simulation.results.bigpurple.md
   functions/read.simulation.results.md
   functions/report.calibration.results.md
