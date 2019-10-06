#Loading of packages
#--------------------
library(readr)
library(here)
library(rio)
library(tidyverse)
library(mlr)
library(ranger)
library(caret)
library(vip)
library(forestFloor)
library(randomForest)
library(iml)
library(R6)
library(devtools)
#Loading of data

#---------------
df_data_g<- df_tune_w4c1 <- readr::read_csv(file.path(here::here(),"df_filter_gen.csv"))
df_data_g <- df_data_g[, -c(1)]
df_data_g$sozk_soz01_male <- ifelse(df_data_g$sozk_soz01_male == "1", 1, 2)
df_data_g$sozk_soz01_male<-as.character(df_data_g$sozk_soz01_male)


# Use set.seed for reproducibility
#-------------------------------------------
set.seed(123)
split_df<-sample(nrow(df_data_g), 2/3 * nrow(df_data_g))

df.rf.train <- df_data_g[split_df, ]
df.rf.test  <- df_data_g[-split_df, ]

#Task for training data
#----------------------
task_rf.train<-  makeClassifTask(id = "Tinnitus_gender", 
                           data = data.frame(df.rf.train), 
                           target = "sozk_soz01_male")

#Task for test data
#----------------------
task_rf.test<-  makeClassifTask(id = "Tinnitus_gender1", 
                                 data = data.frame(df.rf.test), 
                                 target = "sozk_soz01_male")

#Repeated cross validation
#-------------------------
cv.folds <- makeResampleDesc("CV", iters = 3)

# Define model tuning algorithm ~ Random tune algorithm
#------------------------------------------------------
random.tune <- makeTuneControlRandom(maxit = 5)

#Constructing a learner with ranger classifier
#---------------------------------------------
lrn.rf.g<-makeLearner("classif.ranger")

#Get parameter set of the learner
#--------------------------------
getParamSet(lrn.rf.g)

# Define parameters of model and search grid ~ !!!! MODEL SPECIFIC !!!!
#----------------------------------------------------------------------
model.params.rf.g <- makeParamSet(makeIntegerParam("num.trees", lower = 300, upper = 900),
                                  makeIntegerParam("mtry",lower= 1, upper = 10),
                                  makeIntegerParam("min.node.size", lower = 1, upper = 10),
                                  makeNumericParam("sample.fraction", lower = 0, upper = 1),
                                  makeDiscreteParam("importance", values = c("impurity", "permutation")),
                                  makeDiscreteParam("splitrule", values = c("gini","extratrees")))
#Tuning parameters
#-----------------
tune.rf.g<-tuneParams(learner    = "classif.ranger",
                      task       = task_rf.train,
                      resampling = cv.folds,
                      control    = random.tune,
                      par.set    = model.params.rf.g )

#Constructing a learner with ranger classifier with tuned hyperparameters
#------------------------------------------------------------------------
lrn_rf.g<-setHyperPars(makeLearner(("classif.ranger"), par.vals = list(num.trees       = tune.rf.g$x$num.trees,
                                                                       mtry            = tune.rf.g$x$mtry,
                                                                       min.node.size   = tune.rf.g$x$min.node.size,
                                                                       sample.fraction = tune.rf.g$x$sample.fraction,
                                                                       importance      = tune.rf.g$x$importance,
                                                                       splitrule       = tune.rf.g$x$splitrule),
                                   predict.type = "response"))



mod.rf.g = mlr::train(lrn_rf.g, task_rf.train)

mod = Predictor$new(mod.rf.g, data = df_data_g, type = "response")
mod$predict(df.rf.test)
imp = FeatureImp$new(mod, loss = "ce")
plot(imp)



task.pred = predict(mod, task = bh.task, subset = test.set)
task.pred