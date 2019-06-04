#----------------------Description-------------------------

#----------------------------------------------------------

#---------------------Loading of packages------------------
library(ParamHelpers)
library(mlbench)
library(mlr)
library(FSelector)
library(party)
library(kknn)
library(randomForest)
library(tgp)
library(stats)
library(fnn)
library(MASS)
#---------------------------------------------------------

#--------------------Loading of data----------------------

df_data_reg <- readr::read_rds(file.path(here::here(),"FS_scrirep_charite", "data", "adsl_adsl_sum-dur-regr_df.rds"))
df_data_clasf<- readr::read_rds(file.path(here::here(),"FS_scrirep_charite", "data", "adsl_adsl_sum-dur-classif_df.rds"))

#---------------------------------------------------------

#------------Data Sampling--------------------------------

#Regression data
df_samp_reg<-df_data_reg[sample(nrow(df_data_reg),100),]
temp_tar<-df_samp_reg$response
df_samp_reg<-df_samp_reg[,sample(ncol(df_samp_reg),10)]
df_samp_reg$response<-temp_tar

#classification data
df_samp_cls<-df_data_clasf[sample(nrow(df_data_clasf),100),]
temp_tarc<-df_samp_cls$response
df_samp_cls<-df_samp_cls[,sample(ncol(df_samp_cls),10)]
df_samp_cls$response<-temp_tarc

#--------------------------------------------------------

#-----------Model Training and Testing-------------------
modeltest<- function(df_tune, typeM, wrap){
  
  if(typeM == 'R'){#Regression
    
    regr.task_mod<-makeRegrTask( data = data.frame(df_tune), target = "response")
    n1 = getTaskSize(regr.task_mod)
    
    # Splitting the observations for training
    train.mod = seq(1, n1, by = 2)
    test.mod = seq(2, n1, by = 2)
    
    # Train the learner
    mod = mlr::train("regr.lm", regr.task_mod, subset = train.mod)
    mod
    
    task.pred = predict(mod, task = regr.task_mod, subset = test.mod)
    task.pred
    res<-performance(task.pred, measures = mse)
    
    
    return(round(res,digits = 3))
    
  }
  
  if(typeM == 'C'){#Classification
    
    clsf.task_mod<-makeClassifTask( data = data.frame(df_tune), target = "response")
    n2 = getTaskSize(clsf.task_mod)
    
    # Splitting the observations for training
    train.mod = seq(1, n2, by = 2)
    test.mod = seq(2, n2, by = 2)
    
    # Train the learner
    mod = mlr::train("classif.cforest", clsf.task_mod, subset = train.mod)
    mod
    task.pred = predict(mod, task = clsf.task_mod, subset = test.mod)
    task.pred
    res<-performance(task.pred, measures = acc)
    
    return(round(res,digits = 3))
    
  }
}
#--------------------------------------------------------

#----------Common data: learner--------------------------

#Regression:Task Creation
regr.task<-makeRegrTask(id     = "Tin_regr", 
                        data   = data.frame(df_samp_reg), 
                        target = "response")
getTaskFeatureNames(regr.task)

#Repeated cross validation
cv.folds <- makeResampleDesc("CV", iters = 3)

# Define model tuning algorithm ~ Random tune algorithm
random.tune <- makeTuneControlRandom(maxit = 5)

  #Classification:Task Creation
clsf.task<-makeClassifTask(id     = "Tin_clsf", 
                           data   = data.frame(df_samp_cls), 
                           target = "response")
getTaskFeatureNames(clsf.task)

ctrl = makeTuneControlGrid()
inner = makeResampleDesc("Holdout")
outer = makeResampleDesc("CV", iters = 3)
#--------------------------------------------------------

#----------W:1.makeFeatSelControlRandom-----------------

control1<-makeFeatSelControlRandom(same.resampling.instance = TRUE,
                                   maxit                = 20L)

##REGRESSION
#-----------

#---------1.1 R:K-Nearest-Neighbor regressiong-----------

#Tuning hyperparameters
lrnr<-makeLearner("regr.kknn")
#Get parameter set of the learner
getParamSet(lrnr)
ps <- makeParamSet(makeIntegerParam("k",       lower = 1, upper = 10),
                   makeNumericParam("distance",lower = 1, upper = 10)
)
lrnr = makeTuneWrapper(lrnr, resampling = inner, par.set = ps, control = ctrl)
modw = train(lrnr, regr.task)
print(getTuneResult(modw))
r = resample(lrnr, regr.task, outer, extract = getTuneResult)
print(r$extract)

#using the yuned parameters to make the learner
lrnr<-makeLearner("regr.kknn",
                  par.vals = list(k=10, distance=1))

#Defining learner model in the wrapper method
lrn_wr1<- makeFeatSelWrapper(learner    = lrnr, 
                             resampling = cv.folds,
                             control    = control1, 
                             show.info  = FALSE)

#Training of model to extract features
mod_wr1 = mlr::train(lrn_wr1, task = regr.task)

#Extract the features
sfeats = getFeatSelResult(mod_wr1)

cols<-sfeats$x
cols<-c(cols,"response")
#print(cols)

#dataframe with selected features
df_tune_w1r1<-df_samp_reg
df_tune_w1r1<-df_tune_w1r1[, names(df_tune_w1r1) %in% cols] 
ncol(df_tune_w1r1)

#Getting Accuracy ~ Model training & Testing with subset of features
res_w1r1<-modeltest(df_tune_w1r1, 'R')
print(res_w1r1)

#Capturing best values of hyperparameter after tuning
df_val_w1r1<-data.frame(Observation=character(), Value=character())

nwln<- list(Observation= "No. of Features", Value = (ncol(df_tune_w1r1)-1))
df_val_w1r1 = rbind(df_val_w1r1,nwln, stringsAsFactors=FALSE)

nwln<- list(Observation= "k", Value = 10)
df_val_w1r1 = rbind(df_val_w1r1,nwln, stringsAsFactors=FALSE)

nwln<- list(Observation= "distance", Value = 1)
df_val_w1r1 = rbind(df_val_w1r1,nwln, stringsAsFactors=FALSE)

nwln<- list(Observation= "mse", Value = res_w1r1)
df_val_w1r1 = rbind(df_val_w1r1,nwln, stringsAsFactors=FALSE)

print(df_val_w1r1)

#---------End of Nearest-Neighbor regressiong------------

#---------1.2 R:Conditional Inference Trees--------------

#Tuning hyperparameters
lrnr<-makeLearner("regr.ctree")
#Get parameter set of the learner
getParamSet(lrnr)
ps <- makeParamSet(makeDiscreteParam("teststat",    values = c("quad","max")),
                   makeNumericParam("mincriterion", lower = 0, upper = 1),
                   makeIntegerParam("minsplit",     lower = 1, upper = 50), #remove cmt for org data
                   makeIntegerParam("minbucket",    lower = 1, upper = 10)
                   #makeIntegerParam("maxsurrogate", lower = 0, upper = 10)
                   #makeIntegerParam("mtry",         lower = 0, upper = 5)
                   #makeIntegerParam("maxdepth",     lower = 0, upper = 10)
)
lrnr = makeTuneWrapper(lrnr, resampling = inner, par.set = ps, control = ctrl)
modw = train(lrnr, regr.task)
print(getTuneResult(modw))
r = resample(lrnr, regr.task, outer, extract = getTuneResult)
print(r$extract)

#using the yuned parameters to make the learner
lrnr<-makeLearner("regr.ctree",
                  par.vals = list(teststat=10, 
                                  mincriterion=1,
                                  minsplit=2,
                                  minbucket=1))

#Defining learner model
lrn_w1r2<- makeFeatSelWrapper(learner    = lrnr, 
                             resampling = cv.folds,
                             control    = control1, 
                             show.info  = FALSE)


#Training of model to extract features
mod_w1r2 = mlr::train(lrn_w1r2, task = regr.task)

#Extract the features
sfeats = getFeatSelResult(mod_w1r2)

cols<-sfeats$x
cols<-c(cols,"response")
#print(cols)

#dataframe with selected features
df_tune_w1r2<-df_samp_reg
df_tune_w1r2<-df_tune_w1r2[, names(df_tune_w1r2) %in% cols] 
ncol(df_tune_w1r2)

#Getting Accuracy ~ Model training & Testing with subset of features
res_w1r2<-modeltest(df_tune_w1r2, 'R')
print(res_w1r2)

#Capturing best values of hyperparameter after tuning
df_val_w1r2<-data.frame(Observation=character(), Value=character())

nwln<- list(Observation= "No. of Features", Value = (ncol(df_tune_w1r2)-1))
df_val_w1r2 = rbind(df_val_w1r2,nwln, stringsAsFactors=FALSE)

nwln<- list(Observation= "teststat", Value = tuned.model_r2$x$teststat)
df_val_w1r2 = rbind(df_val_w1r2,nwln, stringsAsFactors=FALSE)

nwln<- list(Observation= "mincriterion", Value = tuned.model_r2$x$mincriterion)
df_val_w1r2 = rbind(df_val_w1r2,nwln, stringsAsFactors=FALSE)

nwln<- list(Observation= "minsplit", Value = tuned.model_r2$x$minsplit)
df_val_w1r2 = rbind(df_val_w1r2,nwln, stringsAsFactors=FALSE)

nwln<- list(Observation= "minbucket", Value = tuned.model_r2$x$minbucket)
df_val_w1r2 = rbind(df_val_w1r2,nwln, stringsAsFactors=FALSE)

#nwln<- list(Observation= "maxsurrogate", Value = tuned.model_r2$x$maxsurrogate)
#df_val_w1r2 = rbind(df_val_w1r2,nwln, stringsAsFactors=FALSE)

#nwln<- list(Observation= "mtry", Value = tuned.model_r2$x$mtry)
#df_val_w1r2 = rbind(df_val_w1r2,nwln, stringsAsFactors=FALSE)

#nwln<- list(Observation= "maxdepth", Value = tuned.model_r2$x$maxdepth)
#df_val_w1r2 = rbind(df_val_w1r2,nwln, stringsAsFactors=FALSE)

nwln<- list(Observation= "mse", Value = res_w1r2)
df_val_w1r2 = rbind(df_val_w1r2,nwln, stringsAsFactors=FALSE)

print(df_val_w1r2)

#---------End of Conditional Inference Trees------------

#---------1.3 R:Bayesian CART---------------------------

#Tuning hyperparameters
lrnr<-makeLearner("regr.bcart")
#Get parameter set of the learner
getParamSet(lrnr)
ps <- makeParamSet(makeDiscreteParam("bprior",values = c("b0","b0not","bflat","bmle","bmznot","bmzt")),
                   makeIntegerParam("R",      lower = 1, upper = 10),
                   makeIntegerParam("verb",   lower = 0, upper = 4)
)
lrnr = makeTuneWrapper(lrnr, resampling = inner, par.set = ps, control = ctrl)
modw = train(lrnr, regr.task)
print(getTuneResult(modw))
r = resample(lrnr, regr.task, outer, extract = getTuneResult)
print(r$extract)

#using the yuned parameters to make the learner
lrnr<-makeLearner("regr.bcart",
                  par.vals = list(bprior=10, 
                                  R=1,
                                  verb=2))


#Defining learner model
lrn_w1r3<- makeFeatSelWrapper(learner    = "regr.bcart", 
                             resampling = cv.folds,
                             control    = control1, 
                             show.info  = FALSE)


#Training of model to extract features
mod_w1r3 = mlr::train(lrn_w1r3, task = regr.task)

#Extract the features
sfeats = getFeatSelResult(mod_w1r3)

cols<-sfeats$x
cols<-c(cols,"response")
#print(cols)

#dataframe with selected features
df_tune_w1r3<-df_samp_reg
df_tune_w1r3<-df_tune_w1r3[, names(df_tune_w1r3) %in% cols] 
ncol(df_tune_w1r3)

#Getting Accuracy ~ Model training & Testing with subset of features
res_w1r3<-modeltest(df_tune_w1r3, 'R')
print(res_w1r3)

#Capturing best values of hyperparameter after tuning
df_val_w1r3<-data.frame(Observation=character(), Value=character())

nwln<- list(Hyperparameter= "No. of Features", Value = (ncol(df_tune_w1r3)-1))
df_val_w1r3 = rbind(df_val_w1r3,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "bprior", Value = tuned.model_r3$x$bprior)
df_val_w1r3 = rbind(df_val_w1r3,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "R", Value = tuned.model_r3$x$R)
df_val_w1r3 = rbind(df_val_w1r3,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "verb", Value = tuned.model_r3$x$verb)
df_val_w1r3 = rbind(df_val_w1r3,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "mse", Value = res_r3)
df_val_w1r3 = rbind(df_val_w1r3,nwln, stringsAsFactors=FALSE)


print(df_val_w1r3)

#---------End of Bayesian CART---------------------------

##CLASSIFICATION
#---------------

#---------2.1 C:Binomial Regression----------------------
#Tuning hyperparameters
lrnr<-makeLearner("classif.binomial")
#Get parameter set of the learner
getParamSet(lrnr)
ps <- makeParamSet(makeDiscreteParam("link",values = c("logit","probit","cloglog","cauchit","log"))
)
lrnr = makeTuneWrapper(lrnr, resampling = inner, par.set = ps, control = ctrl)
modw = train(lrnr, clsf.task)
print(getTuneResult(modw))
r = resample(lrnr, clsf.task, outer, extract = getTuneResult)
print(r$extract)

#using the yuned parameters to make the learner
lrnr<-makeLearner("classif.binomial",
                  par.vals = list(link=10))


#Defining learner model
lrn_w1c1<- makeFeatSelWrapper(learner    = lrnr, 
                             resampling = cv.folds,
                             control    = control1, 
                             show.info  = FALSE)


#Training of model to extract features
mod_w1c1 = mlr::train(lrn_w1c1, task = clsf.task)

#Extract the features
sfeats = getFeatSelResult(mod_w1c1)

cols<-sfeats$x
cols<-c(cols,"response")
#print(cols)

#dataframe with selected features
df_tune_w1c1<-df_samp_cls
df_tune_w1c1<-df_tune_w1c1[, names(df_tune_w1c1) %in% cols] 
ncol(df_tune_w1c1)

#Getting Accuracy ~ Model training & Testing with subset of features
res_w1c1<-modeltest(df_tune_w1c1, 'C')
print(res_w1c1)

#Capturing best values of hyperparameter after tuning
df_val_w1c1<-data.frame(Observation=character(), Value=character())

nwln<- list(Observation= "No. of Features", Value = (ncol(df_tune_w1c1)-1))
df_val_w1c1 = rbind(df_val_w1c1,nwln, stringsAsFactors=FALSE)

nwln<- list(Observation= "link", Value = tuned.model_c1$x$link)
df_val_w1c1 = rbind(df_val_w1c1,nwln, stringsAsFactors=FALSE)

nwln<- list(Observation= "Accuracy", Value = res_w1c1)
df_val_w1c1 = rbind(df_val_w1c1,nwln, stringsAsFactors=FALSE)

print(df_val_w1c1)

#---------End of Binomial Regression---------------------

#---------2.2 C:Fast k-Nearest Neighbour-----------------

#Tuning hyperparameters
lrnr<-makeLearner("classif.fnn")
#Get parameter set of the learner
getParamSet(lrnr)
ps <- makeParamSet(makeIntegerParam( "k",     lower = 1, upper = 10),
                   makeDiscreteParam("algorithm",values = c("cover_tree","kd_tree","brute"))
)
lrnr = makeTuneWrapper(lrnr, resampling = inner, par.set = ps, control = ctrl)
modw = train(lrnr, clsf.task)
print(getTuneResult(modw))
r = resample(lrnr, clsf.task, outer, extract = getTuneResult)
print(r$extract)

#using the yuned parameters to make the learner
lrnr<-makeLearner("classif.fnn",
                  par.vals = list(k=10,
                                  algorithm="kd_tree"))

#Defining learner model
lrn_w1c2<- makeFeatSelWrapper(learner    = lrnr, 
                             resampling = cv.folds,
                             control    = control1, 
                             show.info  = FALSE)


#Training of model to extract features
mod_w1c2 = mlr::train(lrn_w1c2, task = clsf.task)

#Extract the features
sfeats = getFeatSelResult(mod_w1c2)

cols<-sfeats$x
cols<-c(cols,"response")
#print(cols)

#dataframe with selected features
df_tune_w1c2<-df_samp_cls
df_tune_w1c2<-df_tune_w1c2[, names(df_tune_w1c2) %in% cols] 
ncol(df_tune_w1c2)

#Getting Accuracy ~ Model training & Testing with subset of features
res_w1c2<-modeltest(df_tune_w1c2, 'C')
print(res_w1c2)

#Capturing best values of hyperparameter after tuning
df_val_w1c2<-data.frame(Hyperparameter=character(), Value=character())

nwln<- list(Hyperparameter= "No. of Features", Value = (ncol(df_tune_w1c2)-1))
df_val_w1c2 = rbind(df_val_w1c2,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "k", Value = tuned.model_c2$x$k)
df_val_w1c2 = rbind(df_val_w1c2,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "algorithm", Value = tuned.model_c2$x$algorithm)
df_val_w1c2 = rbind(df_val_w1c2,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "Accuracy", Value = res_w1c2)
df_val_w1c2 = rbind(df_val_w1c2,nwln, stringsAsFactors=FALSE)


print(df_val_w1c2)

#---------End of Fast k-Nearest Neighbour----------------

#---------2.3 C:Linear Discriminant Analysis-------------

#Tuning hyperparameters
lrnr<-makeLearner("classif.lda")
#Get parameter set of the learner
getParamSet(lrnr)
ps <- makeParamSet(makeNumericParam( "nu",     lower = 2, upper = 10),
                   makeNumericParam( "tol",    lower = 0, upper = 0.0005),
                   makeDiscreteParam("predict.method",values = c("plug-in","predictive","debiased"))
)
lrnr = makeTuneWrapper(lrnr, resampling = inner, par.set = ps, control = ctrl)
modw = train(lrnr, clsf.task)
print(getTuneResult(modw))
r = resample(lrnr, clsf.task, outer, extract = getTuneResult)
print(r$extract)

#using the yuned parameters to make the learner
lrnr<-makeLearner("classif.lda",
                  par.vals = list(nu=10,
                                  tot=0,
                                  predict.method="predictive"))

#Defining learner model
lrn_w1c3<- makeFeatSelWrapper(learner    = lrnr, 
                             resampling = cv.folds,
                             control    = control1, 
                             show.info  = FALSE)


#Training of model to extract features
mod_w1c3 = mlr::train(lrn_w1c3, task = clsf.task)

#Extract the features
sfeats = getFeatSelResult(mod_w1c3)

cols<-sfeats$x
cols<-c(cols,"response")
#print(cols)

#dataframe with selected features
df_tune_w1c3<-df_samp_cls
df_tune_w1c3<-df_tune_w1c3[, names(df_tune_w1c3) %in% cols] 
ncol(df_tune_w1c3)

#Getting Accuracy ~ Model training & Testing with subset of features
res_w1c3<-modeltest(df_tune_w1c3, 'C')
print(res_w1c3)

#Capturing best values of hyperparameter after tuning
df_val_w1c3<-data.frame(Hyperparameter=character(), Value=character())

nwln<- list(Hyperparameter= "No. of Features", Value = (ncol(df_tune_w1c3)-1))
df_val_w1c3 = rbind(df_val_w1c3,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "nu", Value = tuned.model_c3$x$nu)
df_val_w1c3 = rbind(df_val_w1c3,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "tol", Value = tuned.model_c3$x$tol)
df_val_w1c3 = rbind(df_val_w1c3,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "predict.method", Value = tuned.model_c3$x$predict.method)
df_val_w1c3 = rbind(df_val_w1c3,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "Accuracy", Value = res_w1c3)
df_val_w1c3 = rbind(df_val_w1c3,nwln, stringsAsFactors=FALSE)

print(df_val_w1c3)

#---------End of Linear Discriminant Analysis------------
#--------------------------------------------------------

#----------W:2.makeFeatSelControlGA----------------------

control1<-makeFeatSelControlGA(same.resampling.instance = TRUE,
                                   maxit                = 20L)

##REGRESSION
#-----------

#---------2.1 R:K-Nearest-Neighbor regressiong-----------

#Defining learner model
lrn_w2r1<- makeFeatSelWrapper(learner   = "regr.kknn", 
                             resampling = cv.folds,
                             control    = control1, 
                             show.info  = FALSE)


#Training of model to extract features
mod_wr1 = mlr::train(lrn_w2r1, task = regr.task)

#Getting Accuracy ~ Model training & Testing with subset of features
res_w2r1<-modeltest(mod_wr1, 'R')
print(res_w2r1)

#---------End of Nearest-Neighbor regressiong------------

#---------2.2 R:Conditional Inference Trees--------------

#Defining learner model
lrn_w2r2<- makeFeatSelWrapper(learner   = "regr.ctree", 
                             resampling = cv.folds,
                             control    = control1, 
                             show.info  = FALSE)


#Training of model to extract features
mod_wr2 = mlr::train(lrn_w2r2, task = regr.task)

#Getting Accuracy ~ Model training & Testing with subset of features
res_w2r2<-modeltest(mod_wr2, 'R')
print(res_w2r2)
#---------End of Conditional Inference Trees------------

#---------2.3 R:Bayesian CART---------------------------

#Defining learner model
lrn_w2r3<- makeFeatSelWrapper(learner   = "regr.bcart", 
                             resampling = cv.folds,
                             control    = control1, 
                             show.info  = FALSE)


#Training of model to extract features
mod_wr3 = mlr::train(lrn_w2r3, task = regr.task)

#Getting Accuracy ~ Model training & Testing with subset of features
res_w2r3<-modeltest(mod_wr3, 'R')
print(res_w2r3)

#---------End of Bayesian CART---------------------------

##CLASSIFICATION
#---------------

#---------2.1 C:Binomial Regression----------------------

#Defining learner model
lrn_w2c1<- makeFeatSelWrapper(learner   = "classif.binomial", 
                             resampling = cv.folds,
                             control    = control1, 
                             show.info  = FALSE)


#Training of model to extract features
mod_wc1 = mlr::train(lrn_w2c1, task = clsf.task)

#Getting Accuracy ~ Model training & Testing with subset of features
res_w2c1<-modeltest(mod_wc1, 'C')
print(res_w2c1)

#---------End of Binomial Regression---------------------

#---------2.2 C:Fast k-Nearest Neighbour-----------------

#Defining learner model
lrn_w2c2<- makeFeatSelWrapper(learner    = "classif.fnn", 
                             resampling = cv.folds,
                             control    = control1, 
                             show.info  = FALSE)


#Training of model to extract features
mod_wc2 = mlr::train(lrn_w2c2, task = clsf.task)

#Getting Accuracy ~ Model training & Testing with subset of features
res_w2c2<-modeltest(mod_wc2, 'C')
print(res_w2c2)

#---------End of Fast k-Nearest Neighbour----------------

#---------2.3 C:Linear Discriminant Analysis-------------

#Defining learner model
lrn_w2c3<- makeFeatSelWrapper(learner    = "classif.lda", 
                             resampling = cv.folds,
                             control    = control1, 
                             show.info  = FALSE)


#Training of model to extract features
mod_wc3 = mlr::train(lrn_w2c3, task = clsf.task)

#Getting Accuracy ~ Model training & Testing with subset of features
res_w2c3<-modeltest(mod_wc3, 'C')
print(res_w2c3)

#---------End of Linear Discriminant Analysis------------
#--------------------------------------------------------

#----------W:3.makeFeatSelControlSequential--------------

control1<-makeFeatSelControlSequential(same.resampling.instance = TRUE,
                                       impute.val = NULL, method = "sfs", alpha = 0.01, beta = -0.001,
                                       maxit = NA_integer_, max.features = NA_integer_,
                                       tune.threshold = FALSE, tune.threshold.args = list(),
                                       log.fun = "default")

##REGRESSION
#-----------

#---------3.1 R:K-Nearest-Neighbor regressiong-----------

#Defining learner model
lrn_w3r1<- makeFeatSelWrapper(learner   = "regr.kknn", 
                              resampling = cv.folds,
                              control    = control1, 
                              show.info  = FALSE)


#Training of model to extract features
mod_wr1 = mlr::train(lrn_w3r1, task = regr.task)

#Getting Accuracy ~ Model training & Testing with subset of features
res_w3r1<-modeltest(mod_wr1, 'R')
print(res_w3r1)

#---------End of Nearest-Neighbor regressiong------------

#---------3.2 R:Conditional Inference Trees--------------

#Defining learner model
lrn_w3r2<- makeFeatSelWrapper(learner   = "regr.ctree", 
                              resampling = cv.folds,
                              control    = control1, 
                              show.info  = FALSE)


#Training of model to extract features
mod_wr2 = mlr::train(lrn_w3r2, task = regr.task)

#Getting Accuracy ~ Model training & Testing with subset of features
res_w3r2<-modeltest(mod_wr2, 'R')
print(res_w3r2)

#---------End of Conditional Inference Trees------------

#---------3.3 R:Bayesian CART---------------------------

#Defining learner model
lrn_w3r3<- makeFeatSelWrapper(learner    = "regr.bcart", 
                              resampling = cv.folds,
                              control    = control1, 
                              show.info  = FALSE)


#Training of model to extract features
mod_wr3 = mlr::train(lrn_w3r3, task = regr.task)

#Getting Accuracy ~ Model training & Testing with subset of features
res_w3r3<-modeltest(mod_wr3, 'R')
print(res_w3r3)

#---------End of Bayesian CART---------------------------

##CLASSIFICATION
#---------------

#---------3.1 C:Binomial Regression----------------------

#Defining learner model
lrn_w3c1<- makeFeatSelWrapper(learner   = "classif.binomial", 
                              resampling = cv.folds,
                              control    = control1, 
                              show.info  = FALSE)


#Training of model to extract features
mod_wc1 = mlr::train(lrn_w3c1, task = clsf.task)

#Getting Accuracy ~ Model training & Testing with subset of features
res_w3c1<-modeltest(mod_wc1, 'C')
print(res_w3c1)

#---------End of Binomial Regression---------------------

#---------2.2 C:Fast k-Nearest Neighbour-----------------

#Defining learner model
lrn_w3c2<- makeFeatSelWrapper(learner    = "classif.fnn", 
                              resampling = cv.folds,
                              control    = control1, 
                              show.info  = FALSE)


#Training of model to extract features
mod_wc2 = mlr::train(lrn_w3c2, task = clsf.task)

#Getting Accuracy ~ Model training & Testing with subset of features
res_w3c2<-modeltest(mod_wc2, 'C')
print(res_w3c2)

#---------End of Fast k-Nearest Neighbour----------------

#---------2.3 C:Linear Discriminant Analysis-------------

#Defining learner model
lrn_w3c3<- makeFeatSelWrapper(learner    = "classif.lda", 
                              resampling = cv.folds,
                              control    = control1, 
                              show.info  = FALSE)


#Training of model to extract features
mod_wc3 = mlr::train(lrn_w3c3, task = clsf.task)

#Getting Accuracy ~ Model training & Testing with subset of features
res_w3c3<-modeltest(mod_wc3, 'C')
print(res_w3c3)

#---------End of Linear Discriminant Analysis------------
#--------------------------------------------------------

#----------W:4.makeFeatSelControlExhaustive--------------

control1<-makeFeatSelControlExhaustive(same.resampling.instance = TRUE,
                                       maxit                = 20L)

##REGRESSION
#-----------

#---------1.1 R:K-Nearest-Neighbor regressiong-----------

#Defining learner model
lrn_w4r1<- makeFeatSelWrapper(learner   = "regr.kknn", 
                              resampling = cv.folds,
                              control    = control1, 
                              show.info  = FALSE)


#Training of model to extract features
mod_wr1 = mlr::train(lrn_w4r1, task = regr.task)

#Getting Accuracy ~ Model training & Testing with subset of features
res_w4r1<-modeltest(mod_wr1, 'R')
print(res_w4r1)

#---------End of Nearest-Neighbor regressiong------------

#---------1.2 R:Conditional Inference Trees--------------

#Defining learner model
lrn_w4r2<- makeFeatSelWrapper(learner   = "regr.ctree", 
                              resampling = cv.folds,
                              control    = control1, 
                              show.info  = FALSE)


#Training of model to extract features
mod_wr2 = mlr::train(lrn_w3r2, task = regr.task)

#Getting Accuracy ~ Model training & Testing with subset of features
res_w4r2<-modeltest(mod_wr2, 'R')
print(res_w4r2)

#---------End of Conditional Inference Trees------------

#---------1.3 R:Bayesian CART---------------------------

#Defining learner model
lrn_w4r3<- makeFeatSelWrapper(learner    = "regr.bcart", 
                              resampling = cv.folds,
                              control    = control1, 
                              show.info  = FALSE)


#Training of model to extract features
mod_wr3 = mlr::train(lrn_w3r3, task = regr.task)

#Getting Accuracy ~ Model training & Testing with subset of features
res_w4r3<-modeltest(mod_wr3, 'R')
print(res_w4r3)

#---------End of Bayesian CART---------------------------

##CLASSIFICATION
#---------------

#---------2.1 C:Binomial Regression----------------------

#Defining learner model
lrn_w4c1<- makeFeatSelWrapper(learner   = "classif.binomial", 
                              resampling = cv.folds,
                              control    = control1, 
                              show.info  = FALSE)


#Training of model to extract features
mod_wc1 = mlr::train(lrn_w3c1, task = clsf.task)

#Getting Accuracy ~ Model training & Testing with subset of features
res_w4c1<-modeltest(mod_wc1, 'C')
print(res_w4c1)

#---------End of Binomial Regression---------------------

#---------2.2 C:Fast k-Nearest Neighbour-----------------

#Defining learner model
lrn_w4c2<- makeFeatSelWrapper(learner    = "classif.fnn", 
                              resampling = cv.folds,
                              control    = control1, 
                              show.info  = FALSE)


#Training of model to extract features
mod_wc2 = mlr::train(lrn_w3c2, task = clsf.task)

#Getting Accuracy ~ Model training & Testing with subset of features
res_w4c2<-modeltest(mod_wc2, 'C')
print(res_w4c2)


#---------End of Fast k-Nearest Neighbour----------------

#---------2.3 C:Linear Discriminant Analysis-------------

#Defining learner model
lrn_w4c3<- makeFeatSelWrapper(learner    = "classif.lda", 
                              resampling = cv.folds,
                              control    = control1, 
                              show.info  = FALSE)


#Training of model to extract features
mod_wc3 = mlr::train(lrn_w3c3, task = clsf.task)

#Getting Accuracy ~ Model training & Testing with subset of features
res_w4c3<-modeltest(mod_wc3, 'C')
print(res_w4c3)
#---------End of Linear Discriminant Analysis------------
#--------------------------------------------------------