###
## Script name: Assignment 5.R
## Author: Gibson Wirth
## Date: December 2015
####
## Data set: RawSpoonData.xlsx
## Pre-processing: Separated data and transformed to include ratings for appropriate variables to run ANOVA
##
####

####
## Experiment 1: Spoon weight vs. density
##
###
library("ez")
library("schoRsch")

DensitySpoons <- read.csv("Analysis 1.csv")

## Running ANOVA to find F score and P-value (reported results: 4.28, 0.046)

anova1 <- ezANOVA(data=DensitySpoons, dv=.(rating), wid=.(subject), within=.(spoonsize, spoonweight), detailed=TRUE, type=3)
print(anova1)

## Using anova_out to find eta-squared partial (reported result: 0.11)

anova_out(anova1, print=TRUE, etasq="partial")

####
## Experiment 2: Spoon color vs. saltiness
## 
###

SaltySpoon <- read.csv("Analysis2.csv")

## Running ANOVA to find F score and P-value (reported results: 3.645; 0.007)

anova2 <- ezANOVA(data=SaltySpoon, dv=.(rating), wid=.(subject), within=.(yogurtcolor, spooncolor), detailed=TRUE, type=3)
print(anova2)

## Using anova_out to find eta-squared partial (reported result: 0.084)

anova_out(anova2, print=TRUE, etasq="partial")


###
## Experiment 3: Experience and cheese vs. perceived expensiveness
###

ExpensiveCheese <- read.csv("Analysis 3.csv")
## Running ANOVA to find F score and P-value (reported results: 5.77; 0.023 )

anova3 <- ezANOVA(data=ExpensiveCheese, dv=.(rating), wid=.(subject), within=.(cheese, cutlery), between=.(experience), detailed=TRUE, type=3)
print(anova3)

## Using anova_out to find eta-squared partial (reported result: 0.17)

anova_out(anova3, print=TRUE, etasq="partial")