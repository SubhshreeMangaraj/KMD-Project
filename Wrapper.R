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
modeltest<- function(mod, typeM){
  
  if(typeM == 'R'){#Regression
    
    #Extract the features
    sfeats = getFeatSelResult(mod)
    
    cols<-sfeats$x
    cols<-c(cols,"response")
    #print(cols)
    
    #dataframe with selected features
    df_tune<-df_samp_reg
    df_tune<-df_tune[, names(df_tune) %in% cols] 
    #ncol(df_tune)
    
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
    
    #Capturing best values of hyperparameter after tuning
    df_val<-data.frame(Name=character(), Value=character())
    
    nwln<- list(Name= "No. of Features", Value = ncol(df_tune))
    df_val = rbind(df_val,nwln, stringsAsFactors=FALSE)
    
    nwln<- list(Name= "mse", Value = res)
    df_val = rbind(df_val,nwln, stringsAsFactors=FALSE)
    
    return(df_val)
    
  }
  
  if(typeM == 'C'){#Classification
    
    #Extract the features
    sfeats = getFeatSelResult(mod)
    
    cols<-sfeats$x
    cols<-c(cols,"response")
    #print(cols)
    
    #dataframe with selected features
    df_tune<-df_samp_cls
    df_tune<-df_tune[, names(df_tune) %in% cols] 
    #ncol(df_tune)
    
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
    
    #r = calculateROCMeasures(task.pred)
    #res<- list(res, r)
    
    #Capturing best values of hyperparameter after tuning
    df_val<-data.frame(Name=character(), Value=character())
    
    nwln<- list(Name= "No. of Features", Value = ncol(df_tune))
    df_val = rbind(df_val,nwln, stringsAsFactors=FALSE)
    
    nwln<- list(Name= "Accuracy", Value = res)
    df_val = rbind(df_val,nwln, stringsAsFactors=FALSE)
    
    return(df_val)
    
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
#--------------------------------------------------------

#----------W:1.makeFeatSelControlRandom-----------------

control1<-makeFeatSelControlRandom(same.resampling.instance = TRUE,
                                   maxit                = 20L)

##REGRESSION
#-----------

#---------1.1 R:K-Nearest-Neighbor regressiong-----------

#Defining learner model
lrn_wr1<- makeFeatSelWrapper(learner    = "regr.kknn", 
                             resampling = cv.folds,
                             control    = control1, 
                             show.info  = FALSE)


#Training of model to extract features
mod_wr1 = mlr::train(lrn_wr1, task = regr.task)

#Getting Accuracy ~ Model training & Testing with subset of features
res_w1r1<-modeltest(mod_wr1, 'R')
print(res_w1r1)

#---------End of Nearest-Neighbor regressiong------------

#---------1.2 R:Conditional Inference Trees--------------

#Defining learner model
lrn_wr2<- makeFeatSelWrapper(learner    = "regr.ctree", 
                             resampling = cv.folds,
                             control    = control1, 
                             show.info  = FALSE)


#Training of model to extract features
mod_wr2 = mlr::train(lrn_wr2, task = regr.task)

#Getting Accuracy ~ Model training & Testing with subset of features
res_w1r2<-modeltest(mod_wr2, 'R')
print(res_w1r2)

#---------End of Conditional Inference Trees------------

#---------1.3 R:Bayesian CART---------------------------

#Defining learner model
lrn_wr3<- makeFeatSelWrapper(learner    = "regr.bcart", 
                             resampling = cv.folds,
                             control    = control1, 
                             show.info  = FALSE)


#Training of model to extract features
mod_wr3 = mlr::train(lrn_wr3, task = regr.task)

#Getting Accuracy ~ Model training & Testing with subset of features
res_w1r3<-modeltest(mod_wr3, 'R')
print(res_w1r3)

#---------End of Bayesian CART---------------------------

##CLASSIFICATION
#---------------

#---------2.1 C:Binomial Regression----------------------

#Defining learner model
lrn_wc1<- makeFeatSelWrapper(learner    = "classif.binomial", 
                             resampling = cv.folds,
                             control    = control1, 
                             show.info  = FALSE)


#Training of model to extract features
mod_wc1 = mlr::train(lrn_wc1, task = clsf.task)

#Getting Accuracy ~ Model training & Testing with subset of features
res_w1c1<-modeltest(mod_wc1, 'C')
print(res_w1c1)

#---------End of Binomial Regression---------------------

#---------2.2 C:Fast k-Nearest Neighbour-----------------

#Defining learner model
lrn_wc2<- makeFeatSelWrapper(learner    = "classif.fnn", 
                             resampling = cv.folds,
                             control    = control1, 
                             show.info  = FALSE)


#Training of model to extract features
mod_wc2 = mlr::train(lrn_wc2, task = clsf.task)

#Getting Accuracy ~ Model training & Testing with subset of features
res_w1c2<-modeltest(mod_wc2, 'C')
print(res_w1c2)

#---------End of Fast k-Nearest Neighbour----------------

#---------2.3 C:Linear Discriminant Analysis-------------

#Defining learner model
lrn_wc3<- makeFeatSelWrapper(learner    = "classif.lda", 
                             resampling = cv.folds,
                             control    = control1, 
                             show.info  = FALSE)


#Training of model to extract features
mod_wc3 = mlr::train(lrn_wc3, task = clsf.task)

#Getting Accuracy ~ Model training & Testing with subset of features
res_w1c3<-modeltest(mod_wc3, 'C')
print(res_w1c3)

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