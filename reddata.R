library(ParamHelpers)
library(mlbench)
library(mlr)
library(FSelector)
library(party)
library(xgboost)
library(kknn)
library(adabag)
library(rpart)
library(party)
library(mboost)
library(gbm)
library(e1071)
library(nnet)

#--------------Loading of data----------------------------

df_data_reg <- readr::read_rds(file.path(here::here(),"FS_scrirep_charite", "data", "adsl_adsl_sum-dur-regr_df.rds"))
df_data_clasf<- readr::read_rds(file.path(here::here(),"FS_scrirep_charite", "data", "adsl_adsl_sum-dur-classif_df.rds"))

#to store results
df_result<-data.frame(type=character(),method=character(),learner=character(),MSE=numeric(),Accuracy=numeric())
#---------------------------------------------------------

#-------------Random sampling of data with a subset of features------------
#Regression data
df_samp_reg<-df_data_reg[sample(nrow(df_data_reg),50),]
temp_tar<-df_samp_reg$response
df_samp_reg<-df_samp_reg[,sample(ncol(df_samp_reg),8)]
df_samp_reg$response<-temp_tar

#classification data
df_samp_cls<-df_data_clasf[sample(nrow(df_data_clasf),50),]
temp_tarc<-df_samp_cls$response
df_samp_cls<-df_samp_cls[,sample(ncol(df_samp_cls),8)]
df_samp_cls$response<-temp_tarc
#--------------------------------------------------------------------------

#---------------Model training and testing--------------

modeltest<- function(df, typeM){
  
  if(typeM == 'R'){#Regression
    
    regr.task_mod<-makeRegrTask( data = data.frame(df), target = "response")
    n1 = getTaskSize(regr.task_mod)
    
    # Splitting the observations for training
    train.mod = seq(1, n1, by = 2)
    test.mod = seq(2, n1, by = 2)
    
    # Train the learner
    mod = mlr::train("regr.lm", regr.task_mod, subset = train.mod)
    mod
    
    task.pred = predict(mod, task = regr.task_mod, subset = test.mod)
    task.pred
    res<-performance(task.pred)
    
    return(res)
    
  }
  
  if(typeM == 'C'){#Classification
    
    clsf.task_mod<-makeClassifTask( data = data.frame(df), target = "response")
    n2 = getTaskSize(clsf.task_mod)
    
    # Splitting the observations for training
    train.mod = seq(1, n2, by = 2)
    test.mod = seq(2, n2, by = 2)
    
    # Train the learner
    mod = mlr::train("classif.cforest", clsf.task_mod, subset = train.mod)
    mod
    task.pred = predict(mod, task = clsf.task_mod, subset = test.mod)
    task.pred
    res<-performance(task.pred)
    
    return(res)
    
  }
}

#-------------FILTER:Regression----------------------------------

#Task Creation
regr.task<-makeRegrTask(id = "Tin_regr", data = data.frame(df_samp_reg), target = "response")
getTaskFeatureNames(regr.task)

#Repeated cross validation
cv.folds <- makeResampleDesc("CV", iters = 3)

# Define model tuning algorithm ~ Random tune algorithm
random.tune <- makeTuneControlRandom(maxit = 5)


#-------------1.Learner~Conditional Inference Trees------------
#package:party
#learner:regr.ctree 

# Define learner model
lrn <- makeFilterWrapper("regr.ctree", fw.method = "party_cforest.importance")

#Get parameter set of the learner
getParamSet(lrn)

# Define parameters of model and search grid ~ !!!! MODEL SPECIFIC !!!!
model.params <- makeParamSet(
  makeNumericParam("fw.perc", lower = 0, upper = 1),
  makeDiscreteParam("teststat", values = c("quad","max")),
  makeNumericParam("mincriterion", lower = 0, upper = 1),
  #makeIntegerParam("minsplit", lower = 1, upper = 50),
  makeIntegerParam("minbucket", lower = 1, upper = 10),
  makeIntegerParam("maxsurrogate",lower = 0, upper = 10),
  makeIntegerParam("mtry", lower = 0, upper = 5),
  makeIntegerParam("maxdepth",lower = 0, upper = 10)
)


# Tune model to find best performing parameter settings using random search algorithm
tuned.model <- tuneParams(learner = lrn,
                          task = regr.task,
                          resampling = cv.folds,
                          par.set = model.params,
                          control = random.tune,
                          show.info = FALSE)
tuned.model$x$fw.perc
lrn <- makeFilterWrapper("regr.ctree", fw.method = "party_cforest.importance", fw.perc = tuned.model$x$fw.perc)
mod123<-mlr::train(lrn,regr.task)

#Extract the features
getFilteredFeatures(mod123)

cols<-(getFilteredFeatures(mod123))
cols<-c(cols,"response")
View(cols)

#dataframe with selected features
df_data_tune<-df_samp_reg
df_data_tune<-df_data_tune[, names(df_data_tune) %in% cols] 
ncol(df_data_tune)

res_mod<-modeltest(df_data_tune, 'R')

#Append to result

#----------------------------------------------------------------------------------


#-------------2.Learner~eXtreme Gradient Boosting---------------------------------
#package:xgboost
#learner:regr.xgboost  

# Define learner model
lrn1 <- makeFilterWrapper("regr.xgboost")

#Get parameter set of the learner
getParamSet(lrn1)

# Define parameters of model and search grid ~ !!!! MODEL SPECIFIC !!!!
model.params1 <- makeParamSet(
  makeNumericParam("fw.perc", lower = 0, upper = 1),
  makeDiscreteParam("booster", values = c("gbtree","gblinear","dart")),
  makeNumericParam("eta", lower = 0, upper = 1),
  makeNumericParam("gamma", lower = 0, upper = 50),
  makeIntegerParam("max_depth", lower = 1, upper = 10),
  makeNumericParam("min_child_weight",lower = 0, upper = 10),
  makeNumericParam("subsample", lower = 0, upper = 1),
  makeNumericParam("colsample_bytree",lower = 0, upper = 1),
  makeNumericParam("colsample_bylevel",lower = 0, upper = 1),
  makeIntegerParam("num_parallel_tree", lower = 1, upper = 10),
  makeNumericParam("lambda",lower = 0, upper = 10),
  makeNumericParam("lambda_bias",lower = 0, upper = 10),
  makeNumericParam("alpha",lower = 0, upper = 10),
  makeNumericParam("base_score",lower = 0, upper = 10),
  makeNumericParam("max_delta_step",lower = 0, upper = 10),
  makeNumericParam("tweedie_variance_power",lower = 1, upper = 10),
  makeIntegerParam("verbose", lower = 0, upper = 2),
  makeIntegerParam("print_every_n", lower = 1, upper = 10),
  makeDiscreteParam("normalize_type", values = c("tree","forest")),
  makeNumericParam("rate_drop",lower = 0, upper = 1)
)

# Tune model to find best performing parameter settings using random search algorithm
tuned.model1 <- tuneParams(learner = lrn1,
                          task = regr.task,
                          resampling = cv.folds,
                          par.set = model.params1,
                          control = random.tune,
                          show.info = FALSE)

tuned.model1$x$fw.perc
lrn1 <- makeFilterWrapper("regr.xgboost", fw.perc = tuned.model1$x$fw.perc)
mod_xg<-mlr::train(lrn1,regr.task)

#Extract the features
getFilteredFeatures(mod_xg)

cols<-(getFilteredFeatures(mod_xg))
cols<-c(cols,"response")
View(cols)

#dataframe with selected features
df_data_tune_xg<-df_samp_reg
df_data_tune_xg<-df_data_tune_xg[, names(df_data_tune_xg) %in% cols] 
ncol(df_data_tune_xg)

res_mod<-modeltest(df_data_tune_xg, 'R')
print(res_mod)
#Append to result


#-------------3.Learner~K-Nearest-Neighbor regressiong---------------------------------

#package:kknn
#learner:regr.kknn   

# Define learner model
lrn2 <- makeFilterWrapper("regr.kknn")

#Get parameter set of the learner
getParamSet(lrn2)

# Define parameters of model and search grid ~ !!!! MODEL SPECIFIC !!!!
model.params2 <- makeParamSet(
  makeNumericParam("fw.perc", lower = 0, upper = 1),
  makeIntegerParam("k", lower = 1, upper = 100),
  makeNumericParam("distance", lower = 1, upper = 100)
)

# Tune model to find best performing parameter settings using random search algorithm
tuned.model2 <- tuneParams(learner = lrn2,
                           task = regr.task,
                           resampling = cv.folds,
                           par.set = model.params2,
                           control = random.tune,
                           show.info = FALSE)
tuned.model2$x$fw.perc
lrn2 <- makeFilterWrapper("regr.kknn", fw.perc = tuned.model2$x$fw.perc)
mod_knn<-mlr::train(lrn2,regr.task)

#Extract the features
getFilteredFeatures(mod_knn)

cols<-(getFilteredFeatures(mod_knn))
cols<-c(cols,"response")
View(cols)

#dataframe with selected features
df_data_tune_knn<-df_samp_reg
df_data_tune_knn<-df_data_tune_knn[, names(df_data_tune_knn) %in% cols] 
ncol(df_data_tune_knn)

res_mod<-modeltest(df_data_tune_knn, 'R')
print(res_mod)
#Append to result


#-------------FILTER:Classification----------------------------------
#Task Creation
clsf.task<-makeClassifTask(id = "Tin_clsf", data = data.frame(df_samp_cls), target = "response")
getTaskFeatureNames(clsf.task)

#Repeated cross validation
cv.folds <- makeResampleDesc("CV", iters = 3)

# Define model tuning algorithm ~ Random tune algorithm
random.tune <- makeTuneControlRandom(maxit = 5)


#-------------1.Learner~Adabag Boosting---------------------------------
#package: adabag,rpart
#learner: classif.boosting

# Define learner model
lrn3 <- makeFilterWrapper("classif.boosting")

#Get parameter set of the learner
getParamSet(lrn3)

# Define parameters of model and search grid ~ !!!! MODEL SPECIFIC !!!!
model.params3 <- makeParamSet(
  makeNumericParam("fw.perc", lower = 0, upper = 1),
  makeIntegerParam("mfinal", lower = 1, upper = 500),
  makeDiscreteParam("coeflearn", values = c("Breiman","Freund","Zhu")),
  makeIntegerParam("minsplit", lower = 1, upper = 100),
  makeIntegerParam("minbucket", lower = 1, upper = 100),
  makeNumericParam("cp", lower = 0, upper = 1),
  makeIntegerParam("maxcompete", lower = 0, upper = 10),
  makeIntegerParam("maxsurrogate", lower = 0, upper = 10),
  makeDiscreteParam("usesurrogate", values = c("0","1","2")),
  makeDiscreteParam("surrogatestyle", values = c("0","1")),
  makeIntegerParam("maxdepth", lower = 1, upper = 30),
  makeIntegerParam("xval", lower = 0, upper = 50)
)

# Tune model to find best performing parameter settings using random search algorithm
tuned.model3 <- tuneParams(learner = lrn3,
                           task = clsf.task,
                           resampling = cv.folds,
                           par.set = model.params3,
                           control = random.tune,
                           show.info = FALSE)

tuned.model3$x$fw.perc
lrn3 <- makeFilterWrapper("classif.boosting", fw.perc = tuned.model3$x$fw.perc)
mod_c1<-mlr::train(lrn3,clsf.task)

#Extract the features
getFilteredFeatures(mod_c1)

cols<-(getFilteredFeatures(mod_c1))
cols<-c(cols,"response")
View(cols)

#dataframe with selected features
df_data_tune_c1<-df_samp_cls
df_data_tune_c1<-df_data_tune_c1[, names(df_data_tune_c1) %in% cols] 
ncol(df_data_tune_c1)

res_mod<-modeltest(df_data_tune_c1, 'C')
print(res_mod)
#Append to result

#-------------2.Learner~ada Boosting---------------------------------
#package: ada,rpart
#learner: classif.ada

# Define learner model
lrn4 <- makeFilterWrapper("classif.ada")

#Get parameter set of the learner
getParamSet(lrn4)

# Define parameters of model and search grid ~ !!!! MODEL SPECIFIC !!!!
model.params4 <- makeParamSet(
  makeNumericParam("fw.perc", lower = 0, upper = 1),
  makeDiscreteParam("loss", values = c("exponential","adaboost","logistic")),
  makeDiscreteParam("type", values = c("discrete","adaboost","real","gentle")),
  makeIntegerParam("iter", lower = 1, upper = 100),
  makeNumericParam("nu",lower = 0, upper = 1),
  makeNumericParam("bag.frac",lower = 0,upper = 1),
  makeIntegerParam("max.iter", lower = 0, upper = 30),
  makeIntegerParam("maxcompete",lower = 0, upper = 10),
  makeIntegerLearnerParam("maxsurrogate",lower = 0,upper = 10),
  makeDiscreteParam("usesurrogate", values = c("0","1","2")),
  makeDiscreteParam("surrogatestyle",values = c("0","1"))
)

# Tune model to find best performing parameter settings using random search algorithm
tuned.model4 <- tuneParams(learner = lrn4,
                           task = clsf.task,
                           resampling = cv.folds,
                           par.set = model.params4,
                           control = random.tune,
                           show.info = FALSE)

tuned.model4$x$fw.perc
lrn4 <- makeFilterWrapper("classif.ada", fw.perc = tuned.model4$x$fw.perc)
mod_c2<-mlr::train(lrn4,clsf.task)

#Extract the features
getFilteredFeatures(mod_c2)

cols<-(getFilteredFeatures(mod_c2))
cols<-c(cols,"response")
View(cols)

#dataframe with selected features
df_data_tune_c2<-df_samp_cls
df_data_tune_c2<-df_data_tune_c2[, names(df_data_tune_c2) %in% cols] 
ncol(df_data_tune_c2)

res_mod<-modeltest(df_data_tune_c2, 'C')
print(res_mod)
#Append to result

#------------------------------------------------------------------------------

#------------------WRAPPER:Regression------------------------------------------

#Task Creation
regr.task<-makeRegrTask(id = "Tin_regr", data = data.frame(df_samp_reg), target = "response")
getTaskFeatureNames(regr.task)

#Repeated cross validation
cv.folds <- makeResampleDesc("CV", iters = 2)

# Define model tuning algorithm ~ Random tune algorithm
random.tune <- makeTuneControlRandom(maxit = 1)

#-----------------1.Learner~Support Vector Machines (libsvm)----------------
#package:e1071
#learner:regr.svm 

lrn.control<-makeFeatSelControlExhaustive(same.resampling.instance = TRUE,
                                          maxit = 1)

lrn_w1<-makeFeatSelWrapper("regr.lm", resampling = cv.folds,
                           control = lrn.control, show.info = FALSE)

#Get parameter set of the learner
getParamSet(lrn_w1)

mod_w1 = mlr::train(lrn_w1, task = regr.task)

mod_w1
sfeats = getFeatSelResult(mod_w1)
sfeats

#The selected features are:
sfeats$x

cols<-sfeats$x
cols<-c(cols,"response")
View(cols)

#dataframe with selected features
df_data_tune_w1<-df_samp_reg
df_data_tune_w1<-df_data_tune_w1[, names(df_data_tune_w1) %in% cols] 
ncol(df_data_tune_w1)

res_mod<-modeltest(df_data_tune_w1, 'R')
print(res_mod)


#The 5-fold cross-validated performance of the learner specified above can be computed as follows:
#out.rdesc = makeResampleDesc("CV", iters = 5)

#r = resample(learner = lrn_w1, task = regr.task, resampling = out.rdesc, models = TRUE,
             #show.info = FALSE)


#-----------------2.Learner~Featureless regression----------------
#package:mlr
#learner:regr.featureless 

lrn.control2<-makeFeatSelControlRandom(same.resampling.instance = TRUE,
                                          maxit = 1)

lrn_w2<-makeFeatSelWrapper("regr.featureless", resampling = cv.folds,
                           control = lrn.control, show.info = FALSE)
#Get parameter set of the learner
getParamSet(lrn_w2)

mod_w2 = mlr::train(lrn_w2, task = regr.task)

mod_w2
sfeats2 = getFeatSelResult(mod_w2)
sfeats2

#The selected features are:
sfeats2$x

cols<-sfeats2$x
cols<-c(cols,"response")
View(cols)

#dataframe with selected features
df_data_tune_w2<-df_samp_reg
df_data_tune_w2<-df_data_tune_w2[, names(df_data_tune_w2) %in% cols] 
ncol(df_data_tune_w2)

res_mod<-modeltest(df_data_tune_w2, 'R')
print(res_mod)


#------------------WRAPPER:Classification------------------------------------------

#Task Creation
clsf.task<-makeClassifTask(id = "Tin_clsf", data = data.frame(df_samp_cls), target = "response")
getTaskFeatureNames(clsf.task)

#Repeated cross validation
cv.folds <- makeResampleDesc("CV", iters = 2)

# Define model tuning algorithm ~ Random tune algorithm
random.tune <- makeTuneControlRandom(maxit = 1)

#-----------------1.Learner~Naive Bayes----------------

#package:e1071
#learner:classif.naiveBayes 

lrn.control3<-makeFeatSelControlRandom(same.resampling.instance = TRUE,
                                       maxit = 1)

lrn_w3<-makeFeatSelWrapper("classif.naiveBayes", resampling = cv.folds,
                           control = lrn.control, show.info = FALSE)

mod_w3 = mlr::train(lrn_w3, task = clsf.task)

mod_w2
sfeats3 = getFeatSelResult(mod_w3)
sfeats3

#The selected features are:
sfeats3$x

cols<-sfeats3$x
cols<-c(cols,"response")
View(cols)

#dataframe with selected features
df_data_tune_w3<-df_samp_cls
df_data_tune_w3<-df_data_tune_w3[, names(df_data_tune_w3) %in% cols] 
ncol(df_data_tune_w3)

res_mod<-modeltest(df_data_tune_w3, 'C')
print(res_mod)

#-----------------2.Learner~classif.nnet ----------------

#package:nnet
#learner:classif.nnet  

lrn.control4<-makeFeatSelControlRandom(same.resampling.instance = TRUE,
                                       maxit = 1)

lrn_w4<-makeFeatSelWrapper("classif.nnet", resampling = cv.folds,
                           control = lrn.control, show.info = FALSE)

mod_w4 = mlr::train(lrn_w4, task = clsf.task)

mod_w4
sfeats4 = getFeatSelResult(mod_w4)
sfeats4

#The selected features are:
sfeats4$x

cols<-sfeats4$x
cols<-c(cols,"response")
View(cols)

#dataframe with selected features
df_data_tune_w4<-df_samp_cls
df_data_tune_w4<-df_data_tune_w4[, names(df_data_tune_w4) %in% cols] 
ncol(df_data_tune_w4)

res_mod<-modeltest(df_data_tune_w4, 'C')
print(res_mod)

