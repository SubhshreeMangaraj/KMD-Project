#Gender based Models: 2 : lasso regression(glmnet with alpha = 1)

#Loading of packages
#--------------------
library(readr)
library(here)
library(rio)
library(mlr)
library(glmnet)
library(dplyr)
library(ggplot2)  # plotting
library(broom)
library(data.table)
library(iml)

#Loading of data


#Loading of data

#---------------
df_data_g<- df_tune_w4c1 <- readr::read_csv(file.path(here::here(), "df_filter_gen.csv"))
df_data_g <- df_data_g[, -c(1)]
df_data_g$sozk_soz01_male<-as.character(df_data_g$sozk_soz01_male)

# Create training (70%) and test (30%) sets 
# Use set.seed for reproducibility
#-------------------------------------------
set.seed(123)
split_df<-sample(nrow(df_data_g), 2/3 * nrow(df_data_g))

df_male_train <- df_data_g[split_df, ]
df_male_test  <- df_data_g[-split_df, ]

#Task for training data
task_lasso<-makeClassifTask(id = "Tinnitus_svm", 
                          data = data.frame(df_male_train), 
                          target = "sozk_soz01_male")
#Repeated cross validation
#-------------------------
cv.folds <- makeResampleDesc("CV", iters = 3)

# Define model tuning algorithm ~ Random tune algorithm
#------------------------------------------------------
random.tune <- makeTuneControlRandom(maxit = 5)

#Constructing a learner with ranger classifier
#---------------------------------------------
lrn.lasso<-makeFilterWrapper("classif.cvglmnet")

#Get parameter set of the learner
#--------------------------------
getParamSet(lrn.lasso)

# Define parameters of model and search grid ~ !!!! MODEL SPECIFIC !!!!
#----------------------------------------------------------------------
model.params.lasso <- makeParamSet(makeIntegerParam("fw.perc", lower = 0, upper = 1),
                                 makeIntegerParam("nlambda", lower = 1, upper = 500),
                                 makeNumericParam("lambda.min.ratio", lower = 0, upper = 1))

tune.lasso<-tuneParams( learner    = lrn.lasso,
                      task       = task_lasso,
                      resampling = cv.folds,
                      par.set    = model.params.lasso,
                      control    = random.tune )

lrn.lasso <- makeFilterWrapper(learner   = "classif.cvglmnet", 
                             fw.perc   = tune.lasso$x$fw.perc, 
                             par.set   = tune.lasso,
                             predict.type = "response")

mod.lasso<-mlr::train(lrn.lasso,task_lasso)
getFeatureImportance(mod.lasso)

mod = Predictor$new(mod.lasso, data = df_male_train, type = "response")
mod$predict(df_male_test)
imp = FeatureImp$new(mod, loss = "ce")
plot(imp)
