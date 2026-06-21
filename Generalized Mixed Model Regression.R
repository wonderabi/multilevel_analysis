library(lme4)
#We will then create our mixed model regression starting with the null model
# The null model consists of the outcome variables and representing 
#the scenario when our predictor variables are zero or have no effect
null_model <- lmer(
  Scaled_Score ~ 1 +
    (1 | School) +
    (1 | CDP),
  data = dataset
)
summary(null_model)


# Student-Level Model
#The student model consists of our equation where the student-variables are added to our base model 
#accounting our students that are nested by school and CDP
student_model <- lmer(
  Scaled_Score ~
    AGE + ETHNICITY + SPED + ESL +
    (1 | School) +
    (1 | CDP),
  data = dataset
)
summary(student_model)


# School-Level Model
#We then add our school-level predictors on top of the student-level predictors
#alongside random intercepts that are nested by school and CDPs
school_model <- lmer(
  Scaled_Score ~
    AGE + ETHNICITY + SPED + ESL +
    `Attendance Rate` +
    `Teacher-Student` +
    `InstructionalAide-Student` +
    `Expenditure-Student` +
    (1 | School) +
    (1 | CDP),
  data = dataset
)

summary(school_model)


# Full Community-Level Model
#Finally we added selected community-level predictors ontop of both student  and school-level predictors
#alongside random intercepts that are nested by school and CDPs
community_model <- lmer(
  Scaled_Score ~
    AGE + ETHNICITY + SPED + ESL +
    `Attendance Rate` +
    `Teacher-Student` +
    `InstructionalAide-Student` +
    `Expenditure-Student` +
    Income +
    Housing +
    Education +
    (1 | School) +
    (1 | CDP),
  data = dataset
)
summary(community_model)