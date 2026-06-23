# ==============================================================================
# LASSO Regression Analysis and Mixed Regression Models ----
# ==============================================================================

# ==============================================================================
# NOTE: ----
#' This code requires residuals from simple regession models.
#' Please refer to General Simple Regression Models.R on 
#' github.com/wonderabi/multilevel_analysis
# ==============================================================================


# ==============================================================================
# Functions ----
#' You can use this function to shorten the syntax of your code or you can type 
#' the codes out manually if you choose not to use these functions 
# ==============================================================================
# (*) Function: Get Specific Variables ----
#'
#' This functions takes the coefficients from a LASSO regression and returns
#' the variables that did not shrink to 0.
#' 
#' @param coef_object The coefficients from the LASSO regression
#' 
#' @returns A list of all the coefficients
#' @export
#' 
#' @examples
#' get_lasso_vars(coef_min_hs_ma)
#' 
get_lasso_vars <- function(coef_object) {
  coef_df <- as.matrix(coef_object) %>%
    as.data.frame()
  
  colnames(coef_df) <- "coef"
  
  coef_df %>%
    rownames_to_column("term") %>%
    filter(coef != 0 & term != "(Intercept)") %>%
    pull(term)
}

# ==============================================================================
# (*) Function: Build Mixed Regression Models ----
#'
#' This function shortens the syntax needed to create a mixed regression model
#' 
#' @param vars A list of variables to include in the mixed regression model
#' @param other_vars An optional variable to include other level predictors
#' 
#' @returns A mixed regression model with the chosen variables
#' @export
#' 
#' @examples
#' build_mixed_formula(vars_hs_math_min, other_vars)
#' 
build_mixed_formula <- function(vars, other_vars = NULL) {
  
  all_vars <- c(vars, other_vars)
  all_vars <- gsub("`", "", all_vars)
  all_vars <- unique(all_vars)
  
  all_vars <- paste0("`", all_vars, "`")
  
  as.formula(
    paste(
      "`Test.Scale.Score` ~",
      paste(all_vars, collapse = " + "),
      "+ (1 | randomIntercept1) + (1 | randomIntercept2)"
    )
  )
}


# ==============================================================================
# Load Packages ----
# ==============================================================================
library(dplyr)
library(tibble)
library(caret)
library(glmnet)
library(lme4)


# ==============================================================================
# Set Directory ----
# ==============================================================================
setwd("yourDir")
dir <- "yourDir"


# ==============================================================================
# Read Files ----
# ==============================================================================
merged_file <- read.csv(paste0(dir, "yourMergedFile.csv"))
residual_df <- read.csv(paste0(dir, "yourResidualDF.csv"))


# ==============================================================================
# Merge Residuals to Main Merged File ----
# ==============================================================================
## Step 1: Perform the merge ----
merged_file <- merged_file %>%
  left_join(residual_df %>% dplyr::select(CDP, Residual),
            by = "CDP")

## Step 2: Remove NAs that might've been added ----
merged_file <- merged_file %>%
  dplyr::filter(!is.na(`Residual`))


# ==============================================================================
# Create Your Training and Testing Subsets ----
# ==============================================================================
## Step 1: Set seed for replicability ----
set.seed(2444)

## Step 2: Create index ----
train_index <- createDataPartition(merged_file$Residual, 
                                   p = 0.7, 
                                   list = FALSE)

## Step 3: Create train dataset ----
train_data <- merged_file[train_index, ]

## Step 4: Create test dataset ----
test_data <- merged_file[-train_index, ]

# ==============================================================================
# Create Model Matrices with Desired Predictor Variables ----
# ==============================================================================
## Step 1: Generate model matrix with train dataset ----
x_train <- model.matrix(predictedVariable ~ predictorVariable1 
                        + predcitorVarible2 + predictorVariable3 
                        + predictorVariable4 + predictorVariable5,
                        data = train_data)[, -1]
y_train <- train_data$Residual

## Step 2: Generate model matrix with test dataset ----
x_test <- model.matrix(predictedVariable ~ predictorVariable1 
                       + predcitorVarible2 + predictorVariable3 
                       + predictorVariable4 + predictorVariable5,
                       data = test_data)[, -1]
y_test <- test_data$Residual


# ==============================================================================
# Train LASSO Model with Cross-Validation ----
# ==============================================================================
## Step 1: Create Cross-Validation to find optimal lambda
#' Note: alpha = 1 specifies that this is LASSO
lasso_cv <- cv.glmnet(x_train, y_train, alpha = 1)

## Optional: Plot for lambda selction ----
plot(lasso_cv)

## Step 2: Display best lambda values ----
lasso_cv$lambda.min  # Keep note of the lambda value here:
lasso_cv$lambda.1se  # Keep note of the lambda value here:

## Step 3: Fit final LASSO model ----
lasso_final <- glmnet(x_train, y_train, alpha = 1,
                      lambda = lasso_cv$yourChosenLambda)

## Step 4: Display coefficients and selected variables ----
coef(lasso_final) 
# Keep note of either the shrunken variables or selected variables here:


# ==============================================================================
# Create Mixed Regression Model ----
# ==============================================================================
## Step 1: Extract coefficients from LASSO regression ----
lasso_coef <- coef(lasso_cv, s = "yourChosenLambda")

## Step 2: Extract variable names from LASSO regression ----
lasso_vars <- get_lasso_vars(lasso_coef)  # This uses a provided function

## Step 3: Build the models ----
### Optional: Create list of other predictors ----
other_vars <- c("otherVariable1", "otherVariable2")

### Create model (this uses a provided function) ----
mixed_model <- lmer(build_mixed_formula(lasso_vars, other_vars),
                    data = train_data)
summary(mixed_model)


























