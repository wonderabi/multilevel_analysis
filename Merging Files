# ==============================================================================
# Merging Files ----
# ==============================================================================


# ==============================================================================
# NOTE: ----
#' This code provides the basis on how to merge multiple files for multilevel
#' analysis.
#' You should clean your datasets prior to merging. A cleaning code is NOT 
#' provided as your cleaning is unique to your original datasets. 
# ==============================================================================


# ==============================================================================
# Load Packages ----
# ==============================================================================
library(dplyr)
library(readr)


# ==============================================================================
# Set Directory ----
# ==============================================================================
setwd("yourDir")
dir <- "yourDir"


# ==============================================================================
# Read Files ----
# ==============================================================================
score_file <- read.csv(paste0(dir, "yourScoreFile.csv"))
enrollment_file <- read.csv(paste0(dir, "yourEnrollmentFile.csv"))
school_file <- read.csv(paste0(dir, "yourSchoolFile.csv"))
census_file <- read.csv(paste0(dir, "yourCensusFile.csv"))

# ==============================================================================
# Convert Census Variables to z-scores ----
# ==============================================================================
## Step 1: Choose the variables from census_file that you want to use for ----
##         your analysis
cols_to_scale <- c("variableName1", "variableName3")

## Step 2: Save the z-scores into its own dataset ----
census_z_file <- census_file %>% 
  dplyr::select(CDP, all_of(cols_to_scale)) %>%
  mutate(across(all_of(cols_to_scale), scale))


# ==============================================================================
# Merge Files ----
#' NOTE: You must clean your files based on your needs.
#' A cleaning code is NOT provided as your cleaning is unique to your original 
#' datasets. 
#' You may need to merge files multiple times to achieve your desired outcome.
#' This is how my team merged our datasets, but it may vary based on your
#' desired outcome.
# ==============================================================================
merged_file <- enrollment_file %>% left_join(score_file, 
                                             by = "yourMergingVariable")

merged_file <- merged_file %>% left_join(school_file,
                                         by = "yourMergingVariable")

merged_file <- merged_file %>% left_join(census_z_file,
                                         by = "yourMergingVariable")

# ==============================================================================
# Save Merged File ----
# ==============================================================================
write_csv(merged_file, "yourMergedFileName.csv")
