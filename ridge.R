#Gender based Models: 3 : ridge regression (glmnet with alpha = 0)

#Loading of packages
#--------------------
library(readr)
library(here)
library(rio)
library(glmnet)
library(dplyr)
#Loading of data

#---------------
df_data_g<- readr::read_rds(file.path(here::here(),"Tinnitus", "data", "adsl_adsl_sum-dur-classif_df.rds"))


#Delete response_org
#-------------------
df_data_g$response_org<-NULL
df_data_g$response_admission<-NULL

#Dividing the dataset based on gender
#------------------------------------
#export(df_data_g, "df_data_g.csv")

str(df_data_g$sozk_soz01_male)

df_data_g$sozk_soz01_male<-as.factor(df_data_g$sozk_soz01_male)

df_male<-filter(df_data_g,sozk_soz01_male == "1")
df_female<-filter(df_data_g,sozk_soz01_male == "0")

# Dumy code categorical predictor variables
x <- model.matrix(response~., df_male)

# Convert the outcome (class) to a numerical variable
y <- ifelse(df_male$response == "s", 1, 0)

# Setting the range of lambda values
lambda_seq <- 10^seq(2, -2, by = -.1)

# Using glmnet function to build the ridge regression model
fit.male.ridge <- glmnet(x, 
              y, 
              alpha = 0, 
              lambda  = lambda_seq)

# Checking the model
summary(fit.male.ridge)

#choosing the optimal lamda value
#--------------------------------
# Using cross validation glmnet
ridge_cv.male <- cv.glmnet(x, y, alpha = 0, lambda = lambda_seq)
# Best lambda value
best_lambda <- ridge_cv.male$lambda.min
best_lambda

#Extracting the best model using K-cross validation
#--------------------------------------------------
best_fit.ridge.male <- ridge_cv.male$glmnet.fit
head(best_fit.ridge.male)

# Rebuilding the model with optimal lambda value
#-----------------------------------------------
best_ridge.male <- glmnet(x,
                     y,
                     alpha = 0, 
                     lambda = best_lambda)

#Checking the coefficients
#-------------------------
co.male.ridge<-coef(best_ridge.male)
View(co.male.ridge)

#----------------------------------------------------------------------------------------------------

#FEMALE

# Dumy code categorical predictor variables
x <- model.matrix(response~., df_female)

# Convert the outcome (class) to a numerical variable
y <- ifelse(df_female$response == "s", 1, 0)

# Setting the range of lambda values
lambda_seq <- 10^seq(2, -2, by = -.1)

# Using glmnet function to build the ridge regression model
fit <- glmnet(x, 
              y, 
              alpha = 0, 
              lambda  = lambda_seq)

# Checking the model
summary(fit)

#choosing the optimal lamda value
#--------------------------------
# Using cross validation glmnet
ridge_cv <- cv.glmnet(x, y, alpha = 0, lambda = lambda_seq)
# Best lambda value
best_lambda <- ridge_cv$lambda.min
best_lambda

#Extracting the best model using K-cross validation
#--------------------------------------------------
best_fit <- ridge_cv$glmnet.fit
head(best_fit)

# Rebuilding the model with optimal lambda value
#-----------------------------------------------
best_ridge <- glmnet(x,
                     y,
                     alpha = 0, 
                     lambda = best_lambda)

#Checking the coefficients
#-------------------------
co.female.ridge<-coef(best_ridge)
co.female.ridge


