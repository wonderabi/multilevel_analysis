library(lme4)
# If the file is in your current working directory
dataset <- read.csv("filename.csv")

# Using a specific absolute file path
dataset <- read.csv("C:/Users/YourName/Documents/filename.csv")

#Creating Simple Linear Regression Models for student-level predictors
#We use the general function lm() and set our different variables
#The first variable is our outcome variable and the rest will be our predictor variables

gen_linear_model <- lm(Scaled_Score ~ Ethnicity_Simple + Gender + 
                      as.factor(GRADE_LEVEL) + SPED_SERVICES + 
                      ESL_RECEIVE, data = dataset)


#Once we have the variables in our function, we use the   summary() function to summarize  the coefficients of each variable
summary(gen_linear_model)
#This step can be replicated for each grade level and subject type


#####################Calculating Mean Residuals
#We would calculate the general mean residuals of each model to determine school performance if the school performed better or worse
# Calculate school-level mean residuals
# Positive values = performing better than expected
# Negative values = performing worse than expected

school_mean_residuals <- aggregate(
  resid(student_model),
  by = list(School = dataset$SCHOOL_NAME),
  FUN = mean
)

colnames(school_mean_residuals)[2] <- "Mean_Residual"


#Weighted Multiple Regression Model with added school-level variables
general_school_model <- lm(school_mean_residual ~ `Type` + `Filipino Percent` + 
                    `Chuukese Percent` + `Other FAS, Asian, and Other Percent` 
                  + `ELL Percent` + `SPED Percent` #These are individual level predictors and we then add our school-level predictors
                  + `Attendance Rate` + 
                    `Teacher-Student` + `InstructionalAide-Student` + 
                    `Expenditure-Student`,
                  weights =  `Total Students Math`,
                  data = school_data)
summary(general_school_model)

