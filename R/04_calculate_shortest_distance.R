# Load necessary packages -------------------------------------------------------------------
# If you haven't installed these packages, uncomment the following lines and run the install commands:
# install.packages("sfnetworks")
# install.packages("sf")
# install.packages("dplyr")
# install.packages("tidygraph")
# install.packages("ggplot2")
# install.packages("readxl")
# install.packages("readr")
# install.packages("here")
# install.packages("purrr")
# install.packages("units")
# install.packages("future") # Added: underlying framework for parallel computing
# install.packages("furrr")  # Added: furrr package provides parallel versions of purrr functions

library(sfnetworks)
library(sf)
library(dplyr)
library(tidygraph)
library(ggplot2)
library(readxl)
library(readr)
library(here)
library(purrr)
library(units) # Used for handling and displaying distance units
library(future) # Introduced future package for setting parallel strategy
library(furrr)  # Introduced furrr package, provides parallel versions of purrr functions
library(janitor)
library(skimr)
# funs -------------------------------------------------------------------

source(here("R/funs/calculate_network_distance.R"))



# Print final result data frame
print("Calculation complete, results are as follows:")
print(result_df_vectorized)



