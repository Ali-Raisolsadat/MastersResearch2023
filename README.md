# MastersResearch2023

This is my Masters project R code.
The purpose of this project is to construct loss layers over the North America region aggregated estimated loss values from cliamte and
weather-related disasters.

In this project:

1. We classify losses using the second-order difference over the loss and cumulative probability space.
2. We then use the classified losses to construct attachment and exhaustion thresholds, which will give us low-medium
medium-high and high loss layer. 

The files contained in this project are:

1. "DataCleaner.R": This script This R script cleans the EM-DAT data file, given the name of the file in directory of the project.
2. "LossClassfier.R": This is the main project script. This script uses the main methodology to evaluate the results and constructs the first 6 plots of the masters essay.
3. "DiscussionPlotMaker.R": This R script constructs additional plots for the discussion section of the masters essay.
It also outputs the summary of the regressions used to construct the attachment and exhaustion thresholds.

To run this porject, run the "LossClassifier.R" script. Then run the "DiscussionPlotMaker.R" script.
Pay attention ot the console, as the output for the discussion tables are shown when "DiscussionPlotMaker.R" script runs.