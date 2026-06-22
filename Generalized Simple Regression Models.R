# ==============================================================================
# Generalized Simple Regression Models ----
# ==============================================================================


# ==============================================================================
# NOTE: ----
#' This code provides the basis for creating simple linear regression models.
#' This can be used for the first step of your analysis to identify patterns
#' and trends.
#' You can create as many regression models as your analysis needs.
# ==============================================================================


# ==============================================================================
# Set Directory ----
# ==============================================================================
setwd("yourDir")
dir <- "yourDir"


# ==============================================================================
# Read Files ----
# ==============================================================================
merged_file <- read.csv(paste0(dir, "yourMergedFile.csv"))


# ==============================================================================
# Create Simple Linear Regression Models ----
# ==============================================================================
## Step 1: Decide what variables you want to use to predict your outcome ----
gen_linear_model <- lm(predictedVariable ~ predictorVariable1 
                       + predictorVariable2,
                       weights = weightedVarible, # Optional
                       data = merged_file,
                       )

## Step 2: Save mean residuals ----
residual_df <- aggregate(resid(gen_linear_model),
                         
                         #' Decide how you want to group your residuals.
                         #' Ex: CDP, City, School
                         by = list(CDP = merged_file$CDP),
                         
                         FUN = mean)

## Step 3: Ensure that the dataframe has the correct column names ----
names(residual_df)[1] <- "CDP"  # Grouping variable
names(residual_df)[2] <- "Residual"


# ==============================================================================
# Save Residual Dataframe ----
# ==============================================================================
write_csv(residual_df, "yourResidualDF.csv")
