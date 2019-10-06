#Gender based Models: 2 : lasso regression(glmnet with alpha = 1)

#Loading of packages
#--------------------
library(readr)
library(here)
library(rio)
library(mlr)
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

# Create training (70%) and test (30%) sets 
# Use set.seed for reproducibility
#-------------------------------------------
set.seed(123)
split_df<-sample(nrow(df_male), 2/3 * nrow(df_male))

df_male_train <- df_male[split_df, ]
df_male_test  <- df_male[-split_df, ]

#Task for training data
task_male<-makeClassifTask(id = "Tinnitus_male", 
                           data = data.frame(df_male), 
                           target = "response")
#Repeated cross validation
#-------------------------
cv.folds <- makeResampleDesc("CV", iters = 3)

# Define model tuning algorithm ~ Random tune algorithm
#------------------------------------------------------
random.tune <- makeTuneControlRandom(maxit = 5)

#Constructing a learner with ranger classifier
#---------------------------------------------
lrn_male.svm<-makeLearner("classif.glmnet")

#Get parameter set of the learner
#--------------------------------
getParamSet(lrn_male.svm)

# Define parameters of model and search grid ~ !!!! MODEL SPECIFIC !!!!
#----------------------------------------------------------------------
model.params.lasso <- makeParamSet(makeNumericParam("alpha", lower = 1, upper = 1),
                                   makeIntegerParam("nlambda", lower = 50, upper = 150),
                                   makeNumericParam("lambda.min.ratio", lower = 0, upper = 1))

#Tuning parameters
#-----------------
tune_male.lasso<-tuneParams(learner    = "classif.glmnet",
                            task       = task_male,
                            resampling = cv.folds,
                            control    = random.tune,
                            par.set    = model.params.lasso )
#Hyperparameters
#---------------
tune_male.lasso$x

# Train the learner
#------------------

# Dumy code categorical predictor variables
x.male.lasso <- model.matrix(response~., df_male)

# Convert the outcome (class) to a numerical variable
y.male.lasso <- ifelse(df_male$response == "s", 1, 0)

Lasso.mod.male <- glmnet(x.male.lasso, 
                    y.male.lasso, 
                    alpha=1, 
                    nlambda= tune_male.lasso$x$nlambda, 
                    lambda.min.ratio= tune_male.lasso$x$lambda.min.ratio,
                    family="binomial")

cv.out.male.lasso <- cv.glmnet(x,
                    y,
                    alpha=1,
                    nlambda= tune_male.lasso$x$nlambda,
                    type.measure = "mse" )
plot(cv.out.male.lasso)

#min value of lambda
lambda_min.male.lasso <- cv.out.male.lasso$lambda.min

#best value of lambda
lambda_1se.male.lasso <- cv.out.male.lasso$lambda.1se

#regression coefficients
co.male.lasso<-coef(cv.out.male.lasso, s = "lambda.min")
View(coef(cv.out.male.lasso, s = "lambda.min"))
#Selection of the significant features(predictors)
inds<-which(co.male.lasso!=0)
risk.factors.male.lasso<-row.names(co.male.lasso)[inds]
risk.factors.male.lasso<-risk.factors.male.lasso[!(risk.factors.male.lasso %in% '(Intercept)')];
print(risk.factors.male.lasso)
df.male.lasso<- data.frame(risk.factors.male.lasso)
#------------------------------------------------------------------------------------------------------------------

#FEMALE

#Task for training data
task_female<-makeClassifTask(id = "Tinnitus_male", 
                             data = data.frame(df_female), 
                             target = "response")
#Repeated cross validation
#-------------------------
cv.folds <- makeResampleDesc("CV", iters = 3)

# Define model tuning algorithm ~ Random tune algorithm
#------------------------------------------------------
random.tune <- makeTuneControlRandom(maxit = 5)

#Constructing a learner with ranger classifier
#---------------------------------------------
lrn_female.svm<-makeLearner("classif.glmnet")

#Get parameter set of the learner
#--------------------------------
getParamSet(lrn_female.svm)

# Define parameters of model and search grid ~ !!!! MODEL SPECIFIC !!!!
#----------------------------------------------------------------------
model.params.lasso.f <- makeParamSet(makeNumericParam("alpha", lower = 1, upper = 1),
                                     makeIntegerParam("nlambda", lower = 50, upper = 150),
                                     makeNumericParam("lambda.min.ratio", lower = 0, upper = 1))

#Tuning parameters
#-----------------
tune_female.lasso<-tuneParams(learner    = "classif.glmnet",
                              task       = task_female,
                              resampling = cv.folds,
                              control    = random.tune,
                              par.set    = model.params.lasso.f )
#Hyperparameters
#---------------
tune_female.lasso$x

# Train the learner
#------------------

# Dumy code categorical predictor variables
x <- model.matrix(response~., df_female)

# Convert the outcome (class) to a numerical variable
y <- ifelse(df_female$response == "s", 1, 0)

Lasso.mod.f <- glmnet(x, 
                      y, 
                      alpha=1, 
                      nlambda= tune_female.lasso$x$nlambda, 
                      lambda.min.ratio= tune_female.lasso$x$lambda.min.ratio,
                      family="binomial")

cv.out.f <- cv.glmnet(x,
                    y,
                    alpha=1,
                    nlambda= tune_male.lasso$x$nlambda,
                    type.measure = "mse" )
plot(cv.out.f)

#min value of lambda
lambda_min <- cv.out.f$lambda.min

#best value of lambda
lambda_1se <- cv.out.f$lambda.1se

#regression coefficients
co.f<-coef(cv.out.f, s = "lambda.min")
View(coef(cv.out.f, s = "lambda.min"))
#Selection of the significant features(predictors)
inds<-which(co.f!=0)
risk.factors.female.lasso<-row.names(co.f)[inds]
risk.factors.female.lasso<-risk.factors.female.lasso[!(risk.factors.female.lasso %in% '(Intercept)')];
print(risk.factors.female.lasso)
df.female.lasso <- data.frame(risk.factors.female.lasso)

#-----------------------------targer gender----------------------------------------
