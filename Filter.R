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
library(FSelectorRcpp)
#---------------------------------------------------------

#--------------------Loading of data----------------------

df_data_reg <- readr::read_rds(file.path(here::here(),"FS_scrirep_charite", "data", "adsl_adsl_sum-dur-regr_df.rds"))
df_data_clasf<- readr::read_rds(file.path(here::here(),"FS_scrirep_charite", "data", "adsl_adsl_sum-dur-classif_df.rds"))

#---------------------------------------------------------

#------------Data Sampling--------------------------------

#Regression data
df_samp_reg<-df_data_reg[sample(nrow(df_data_reg),500),]
temp_tar<-df_samp_reg$response
df_samp_reg<-df_samp_reg[,sample(ncol(df_samp_reg),15)]
df_samp_reg$response<-temp_tar

#classification data
df_samp_cls<-df_data_clasf[sample(nrow(df_data_clasf),500),]
temp_tarc<-df_samp_cls$response
df_samp_cls<-df_samp_cls[,sample(ncol(df_samp_cls),15)]
df_samp_cls$response<-temp_tarc

#--------------------------------------------------------

#-----------Model Training and Testing-------------------
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
    res<-performance(task.pred, measures = mse)
    
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
    res<-performance(task.pred, measures = acc)
    
    #r = calculateROCMeasures(task.pred)
    #res<- list(res, r)
    
    return(res)
    
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

#----------FS:1.FSelector_chi.squared--------------------

##REGRESSION
#-----------

#---------1.1 R:K-Nearest-Neighbor regressiong-----------

#Defining learner model
lrn_r1 <- makeFilterWrapper("regr.kknn")

#Get parameter set of the learner
getParamSet(lrn_r1)

# Define parameters of model and search grid ~ !!!! MODEL SPECIFIC !!!!
model.params_r1 <- makeParamSet(
                   makeNumericParam("fw.perc", lower = 0, upper = 1),
                   makeDiscreteParam("fw.method", values = c("FSelector_chi.squared",
                                                              "party_cforest.importance",
                                                               "variance")),
                   makeIntegerParam("k",       lower = 1, upper = 10),
                   makeNumericParam("distance",lower = 1, upper = 10)
)

# Tune model to find best performing parameter settings using random search algorithm
tuned.model_r1 <- tuneParams(learner   = lrn_r1,
                            task       = regr.task,
                            resampling = cv.folds,
                            par.set    = model.params_r1,
                            control    = random.tune,
                            show.info  = FALSE)

#Redefining Learner with tuned parameters
lrn_r1 <- makeFilterWrapper(learner   = "regr.kknn",
                            fw.method = tuned.model_r1$x$fw.method, 
                            fw.perc   = tuned.model_r1$x$fw.perc, 
                            par.set   = tuned.model_r1 )

#Training of model to extract features
mod_r1<-mlr::train(lrn_r1,regr.task)

#Extract the features
getFilteredFeatures(mod_r1)

cols<-(getFilteredFeatures(mod_r1))
cols<-c(cols,"response")
View(cols)

#Dataframe with selected features
df_tune_r1<-df_samp_reg
df_tune_r1<-df_tune_r1[, names(df_tune_r1) %in% cols] 
ncol(df_tune_r1)

#Getting Accuracy ~ Model training & Testing with subset of features

res_r1<-modeltest(df_tune_r1, 'R')
print(res_r1)

#Capturing best values of hyperparameter after tuning
df_val_r1<-data.frame(Hyperparameter=character(), Value=character())

nwln<- list(Hyperparameter= "No. of Features", Value = tuned.model_r1$x$fw.perc)
df_val_r1 = rbind(df_val_r1,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "Filter Method", Value = tuned.model_r1$x$fw.method)
df_val_r1 = rbind(df_val_r1,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "k", Value = tuned.model_r1$x$k)
df_val_r1 = rbind(df_val_r1,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "distance", Value = tuned.model_r1$x$distance)
df_val_r1 = rbind(df_val_r1,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "mse", Value = res_r1)
df_val_r1 = rbind(df_val_r1,nwln, stringsAsFactors=FALSE)


print(df_val_r1)
#---------End:K-Nearest-Neighbor regressiong-------------

#---------1.2 R:Conditional Inference Trees--------------

#Defining learner model
lrn_r2 <- makeFilterWrapper("regr.ctree", fw.method = "FSelector_chi.squared")

#Get parameter set of the learner
getParamSet(lrn_r2)

# Define parameters of model and search grid ~ !!!! MODEL SPECIFIC !!!!
model.params_r2 <- makeParamSet(makeNumericParam("fw.perc",      lower = 0, upper = 1),
                                makeDiscreteParam("teststat",    values = c("quad","max")),
                                makeNumericParam("mincriterion", lower = 0, upper = 1),
                                makeIntegerParam("minsplit",     lower = 1, upper = 50), #remove cmt for org data
                                makeIntegerParam("minbucket",    lower = 1, upper = 10)
                                #makeIntegerParam("maxsurrogate", lower = 0, upper = 10)
                                #makeIntegerParam("mtry",         lower = 0, upper = 5)
                                #makeIntegerParam("maxdepth",     lower = 0, upper = 10)
)

# Tune model to find best performing parameter settings using random search algorithm
tuned.model_r2 <- tuneParams(learner    = lrn_r2,
                             task       = regr.task,
                             resampling = cv.folds,
                             par.set    = model.params_r2,
                             control    = random.tune,
                             show.info  = FALSE)

#Redefining Learner with tuned parameters
lrn_r2 <- makeFilterWrapper(learner   = "regr.ctree",
                            fw.method = "FSelector_chi.squared", 
                            fw.perc   = tuned.model_r2$x$fw.perc, 
                            par.set   = tuned.model_r2 )

#Training of model to extract features
mod_r2<-mlr::train(lrn_r2,regr.task)

#Extract the features
getFilteredFeatures(mod_r2)

cols<-(getFilteredFeatures(mod_r2))
cols<-c(cols,"response")
View(cols)

#Dataframe with selected features
df_tune_r2<-df_samp_reg
df_tune_r2<-df_tune_r2[, names(df_tune_r2) %in% cols] 
ncol(df_tune_r2)

#Getting Accuracy ~ Model training & Testing with subset of features

res_r2<-modeltest(df_tune_r2, 'R')
print(res_r2)

#Capturing best values of hyperparameter after tuning
df_val_r2<-data.frame(Hyperparameter=character(), Value=character())

nwln<- list(Hyperparameter= "No. of Features", Value = tuned.model_r2$x$fw.perc)
df_val_r2 = rbind(df_val_r2,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "teststat", Value = tuned.model_r2$x$teststat)
df_val_r2 = rbind(df_val_r2,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "mincriterion", Value = tuned.model_r2$x$mincriterion)
df_val_r2 = rbind(df_val_r2,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "minsplit", Value = tuned.model_r2$x$minsplit)
df_val_r2 = rbind(df_val_r2,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "minbucket", Value = tuned.model_r2$x$minbucket)
df_val_r2 = rbind(df_val_r2,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "mse", Value = res_r2)
df_val_r2 = rbind(df_val_r2,nwln, stringsAsFactors=FALSE)

#nwln<- list(Hyperparameter= "maxsurrogate", Value = tuned.model_r2$x$maxsurrogate)
#df_val_r2 = rbind(df_val_r2,nwln, stringsAsFactors=FALSE)

#nwln<- list(Hyperparameter= "mtry", Value = tuned.model_r2$x$mtry)
#df_val_r2 = rbind(df_val_r2,nwln, stringsAsFactors=FALSE)

#nwln<- list(Hyperparameter= "maxdepth", Value = tuned.model_r2$x$maxdepth)
#df_val_r2 = rbind(df_val_r2,nwln, stringsAsFactors=FALSE)

#---------End:Conditional Inference Trees----------------

#---------1.3 R:Bayesian CART----------------------------

#Defining learner model
lrn_r3 <- makeFilterWrapper("regr.bcart", fw.method = "FSelector_chi.squared")

#Get parameter set of the learner
getParamSet(lrn_r3)

# Define parameters of model and search grid ~ !!!! MODEL SPECIFIC !!!!
model.params_r3 <- makeParamSet(makeNumericParam("fw.perc",lower = 0, upper = 1),
                                makeDiscreteParam("bprior",values = c("b0","b0not","bflat","bmle","bmznot","bmzt")),
                                makeIntegerParam("R",      lower = 1, upper = 10),
                                makeIntegerParam("verb",   lower = 0, upper = 4)
)

# Tune model to find best performing parameter settings using random search algorithm
tuned.model_r3 <- tuneParams(learner    = lrn_r3,
                             task       = regr.task,
                             resampling = cv.folds,
                             par.set    = model.params_r3,
                             control    = random.tune,
                             show.info  = FALSE)

#Redefining Learner with tuned parameters
lrn_r3 <- makeFilterWrapper(learner   = "regr.bcart",
                            fw.method = "FSelector_chi.squared", 
                            fw.perc   = tuned.model_r3$x$fw.perc, 
                            par.set   = tuned.model_r3 )

#Training of model to extract features
mod_r3<-mlr::train(lrn_r3,regr.task)

#Extract the features
getFilteredFeatures(mod_r3)

cols<-(getFilteredFeatures(mod_r3))
cols<-c(cols,"response")
View(cols)

#Dataframe with selected features
df_tune_r3<-df_samp_reg
df_tune_r3<-df_tune_r3[, names(df_tune_r3) %in% cols] 
ncol(df_tune_r3)

#Getting Accuracy ~ Model training & Testing with subset of features
res_r3<-modeltest(df_tune_r3, 'R')
print(res_r3)

#Capturing best values of hyperparameter after tuning
df_val_r3<-data.frame(Hyperparameter=character(), Value=character())

nwln<- list(Hyperparameter= "No. of Features", Value = tuned.model_r3$x$fw.perc)
df_val_r3 = rbind(df_val_r3,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "bprior", Value = tuned.model_r3$x$bprior)
df_val_r3 = rbind(df_val_r3,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "R", Value = tuned.model_r3$x$R)
df_val_r3 = rbind(df_val_r3,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "verb", Value = tuned.model_r3$x$verb)
df_val_r3 = rbind(df_val_r3,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "mse", Value = res_r3)
df_val_r3 = rbind(df_val_r3,nwln, stringsAsFactors=FALSE)


print(df_val_r3)
#---------End:Bayesian CART------------------------------

##CLASSIFICATION
#---------------

#---------2.1 C:Binomial Regression----------------------

#Defining learner model
lrn_c1 <- makeFilterWrapper("classif.binomial", fw.method = "FSelector_chi.squared")

#Get parameter set of the learner
getParamSet(lrn_c1)

# Define parameters of model and search grid ~ !!!! MODEL SPECIFIC !!!!
model.params_c1 <- makeParamSet(makeNumericParam("fw.perc",lower = 0, upper = 1),
                                makeDiscreteParam("link",values = c("logit","probit","cloglog","cauchit","log"))
                                
)

# Tune model to find best performing parameter settings using random search algorithm
tuned.model_c1 <- tuneParams(learner    = lrn_c1,
                             task       = clsf.task,
                             resampling = cv.folds,
                             par.set    = model.params_c1,
                             control    = random.tune,
                             show.info  = FALSE)

#Redefining Learner with tuned parameters
lrn_c1 <- makeFilterWrapper(learner   = "classif.binomial",
                            fw.method = "FSelector_chi.squared", 
                            fw.perc   = tuned.model_c1$x$fw.perc, 
                            par.set   = tuned.model_c1 )

#Training of model to extract features
mod_c1<-mlr::train(lrn_c1,clsf.task)

#Extract the features
getFilteredFeatures(mod_c1)

cols<-(getFilteredFeatures(mod_c1))
cols<-c(cols,"response")
View(cols)

#Dataframe with selected features
df_tune_c1<-df_samp_cls
df_tune_c1<-df_tune_c1[, names(df_tune_c1) %in% cols] 
ncol(df_tune_c1)

#Getting Accuracy ~ Model training & Testing with subset of features
res_c1<-modeltest(df_tune_c1, 'C')
print(res_c1)

#Capturing best values of hyperparameter after tuning
df_val_c1<-data.frame(Hyperparameter=character(), Value=character())

nwln<- list(Hyperparameter= "No. of Features", Value = tuned.model_c1$x$fw.perc)
df_val_c1 = rbind(df_val_c1,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "link", Value = tuned.model_c1$x$link)
df_val_c1 = rbind(df_val_c1,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "Accuracy", Value = res_c1)
df_val_c1 = rbind(df_val_c1,nwln, stringsAsFactors=FALSE)

print(df_val_c1)

#------------End of Binomial Regression------------------

#---------2.2 C:Fast k-Nearest Neighbour-----------------

#Defining learner model
lrn_c2 <- makeFilterWrapper("classif.fnn", fw.method = "FSelector_chi.squared")

#Get parameter set of the learner
getParamSet(lrn_c2)

# Define parameters of model and search grid ~ !!!! MODEL SPECIFIC !!!!
model.params_c2 <- makeParamSet(makeNumericParam("fw.perc",lower = 0, upper = 1),
                                makeIntegerParam( "k",     lower = 1, upper = 10),
                                makeDiscreteParam("algorithm",values = c("cover_tree","kd_tree","brute"))
                                
)

# Tune model to find best performing parameter settings using random search algorithm
tuned.model_c2 <- tuneParams(learner    = lrn_c2,
                             task       = clsf.task,
                             resampling = cv.folds,
                             par.set    = model.params_c2,
                             control    = random.tune,
                             show.info  = FALSE)

#Redefining Learner with tuned parameters
lrn_c2 <- makeFilterWrapper(learner   = "classif.fnn",
                            fw.method = "FSelector_chi.squared", 
                            fw.perc   = tuned.model_c2$x$fw.perc, 
                            par.set   = tuned.model_c2 )

#Training of model to extract features
mod_c2<-mlr::train(lrn_c2,clsf.task)

#Extract the features
getFilteredFeatures(mod_c2)

cols<-(getFilteredFeatures(mod_c2))
cols<-c(cols,"response")
View(cols)

#Dataframe with selected features
df_tune_c2<-df_samp_cls
df_tune_c2<-df_tune_c2[, names(df_tune_c2) %in% cols] 
ncol(df_tune_c2)

#Getting Accuracy ~ Model training & Testing with subset of features
res_c2<-modeltest(df_tune_c2, 'C')
print(res_c2)

#Capturing best values of hyperparameter after tuning
df_val_c2<-data.frame(Hyperparameter=character(), Value=character())

nwln<- list(Hyperparameter= "No. of Features", Value = tuned.model_c2$x$fw.perc)
df_val_c2 = rbind(df_val_c2,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "k", Value = tuned.model_c2$x$k)
df_val_c2 = rbind(df_val_c2,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "algorithm", Value = tuned.model_c2$x$algorithm)
df_val_c2 = rbind(df_val_c2,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "Accuracy", Value = res_c2)
df_val_c2 = rbind(df_val_c2,nwln, stringsAsFactors=FALSE)


print(df_val_c2)

#------------End of Fast k-Nearest Neighbour-------------

#---------2.3 C:Linear Discriminant Analysis-------------

#Defining learner model
lrn_c3 <- makeFilterWrapper("classif.lda", fw.method = "FSelector_chi.squared")

#Get parameter set of the learner
getParamSet(lrn_c3)

# Define parameters of model and search grid ~ !!!! MODEL SPECIFIC !!!!
model.params_c3 <- makeParamSet(makeNumericParam("fw.perc", lower = 0, upper = 1),
                                makeNumericParam( "nu",     lower = 2, upper = 10),
                                makeNumericParam( "tol",    lower = 0, upper = 0.0005),
                                makeDiscreteParam("predict.method",values = c("plug-in","predictive","debiased"))
                                
)

# Tune model to find best performing parameter settings using random search algorithm
tuned.model_c3 <- tuneParams(learner    = lrn_c3,
                             task       = clsf.task,
                             resampling = cv.folds,
                             par.set    = model.params_c3,
                             control    = random.tune,
                             show.info  = FALSE)

#Redefining Learner with tuned parameters
lrn_c3 <- makeFilterWrapper(learner   = "classif.lda",
                            fw.method = "FSelector_chi.squared", 
                            fw.perc   = tuned.model_c3$x$fw.perc, 
                            par.set   = tuned.model_c3 )

#Training of model to extract features
mod_c3<-mlr::train(lrn_c3,clsf.task)

#Extract the features
getFilteredFeatures(mod_c3)

cols<-(getFilteredFeatures(mod_c3))
cols<-c(cols,"response")
View(cols)

#Dataframe with selected features
df_tune_c3<-df_samp_cls
df_tune_c3<-df_tune_c3[, names(df_tune_c3) %in% cols] 
ncol(df_tune_c3)

#Getting Accuracy ~ Model training & Testing with subset of features
res_c3<-modeltest(df_tune_c3, 'C')
print(res_c3)

#Capturing best values of hyperparameter after tuning
df_val_c3<-data.frame(Hyperparameter=character(), Value=character())

nwln<- list(Hyperparameter= "No. of Features", Value = tuned.model_c3$x$fw.perc)
df_val_c3 = rbind(df_val_c3,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "nu", Value = tuned.model_c3$x$nu)
df_val_c3 = rbind(df_val_c3,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "tol", Value = tuned.model_c3$x$tol)
df_val_c3 = rbind(df_val_c3,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "predict.method", Value = tuned.model_c3$x$predict.method)
df_val_c3 = rbind(df_val_c3,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "Accuracy", Value = res_c3)
df_val_c3 = rbind(df_val_c3,nwln, stringsAsFactors=FALSE)

print(df_val_c3)

#------------End of Fast k-Nearest Neighbour-------------


#------------End of FSelector_chi.squared----------------



#----------FS:2.party_cforest.importance-------------------

##REGRESSION
#-----------

#---------1.1 R:K-Nearest-Neighbor regressiong------------

#Defining learner model
lrn_f2r1 <- makeFilterWrapper("regr.kknn")

#Get parameter set of the learner
getParamSet(lrn_f2r1)

# Define parameters of model and search grid ~ !!!! MODEL SPECIFIC !!!!
model.params_f2r1 <- makeParamSet(
                                  makeNumericParam("fw.perc", lower = 0, upper = 1),
                                  makeDiscreteParam("fw.method", values = c("party_cforest.importance")),
                                  makeIntegerParam("k",       lower = 1, upper = 10),
                                  makeNumericParam("distance",lower = 1, upper = 10)
)

# Tune model to find best performing parameter settings using random search algorithm
tuned.model_f2r1 <- tuneParams(learner   = lrn_f2r1,
                              task       = regr.task,
                              resampling = cv.folds,
                              par.set    = model.params_f2r1,
                              control    = random.tune,
                              show.info  = FALSE)

#Redefining Learner with tuned parameters
lrn_f2r1 <- makeFilterWrapper(learner   = "regr.kknn",
                              fw.method = tuned.model_f2r1$x$fw.method, 
                              fw.perc   = tuned.model_f2r1$x$fw.perc, 
                              par.set   = tuned.model_f2r1 )

#Training of model to extract features
mod_r1<-mlr::train(lrn_f2r1,regr.task)

#Extract the features
getFilteredFeatures(mod_r1)

cols<-(getFilteredFeatures(mod_r1))
cols<-c(cols,"response")
View(cols)

#Dataframe with selected features
df_tune_r1<-df_samp_reg
df_tune_r1<-df_tune_r1[, names(df_tune_r1) %in% cols] 
ncol(df_tune_r1)

#Getting Accuracy ~ Model training & Testing with subset of features

res_f2r1<-modeltest(df_tune_r1, 'R')
print(res_f2r1)

#Capturing best values of hyperparameter after tuning
df_val_f2r1<-data.frame(Hyperparameter=character(), Value=character())

nwln<- list(Hyperparameter= "No. of Features", Value = tuned.model_f2r1$x$fw.perc)
df_val_f2r1 = rbind(df_val_f2r1,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "Filter Method", Value = tuned.model_f2r1$x$fw.method)
df_val_f2r1 = rbind(df_val_f2r1,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "k", Value = tuned.model_f2r1$x$k)
df_val_f2r1 = rbind(df_val_f2r1,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "distance", Value = tuned.model_f2r1$x$distance)
df_val_f2r1 = rbind(df_val_f2r1,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "mse", Value = res_f2r1)
df_val_f2r1 = rbind(df_val_f2r1,nwln, stringsAsFactors=FALSE)

print(df_val_f2r1)
#---------End:K-Nearest-Neighbor regressiong-------------

#---------1.2 R:Conditional Inference Trees--------------

#Defining learner model
lrn_f2r2 <- makeFilterWrapper("regr.ctree", fw.method = "party_cforest.importance")

#Get parameter set of the learner
getParamSet(lrn_f2r2)

# Define parameters of model and search grid ~ !!!! MODEL SPECIFIC !!!!
model.params_f2r2 <- makeParamSet(makeNumericParam("fw.perc",      lower = 0, upper = 1),
                                 makeDiscreteParam("teststat",    values = c("quad","max")),
                                  makeNumericParam("mincriterion", lower = 0, upper = 1),
                                  makeIntegerParam("minsplit",     lower = 1, upper = 50), #remove cmt for org data
                                  makeIntegerParam("minbucket",    lower = 1, upper = 10)
                                 #makeIntegerParam("maxsurrogate", lower = 0, upper = 10)
                                #makeIntegerParam("mtry",         lower = 0, upper = 5)
                                #makeIntegerParam("maxdepth",     lower = 0, upper = 10)
)

# Tune model to find best performing parameter settings using random search algorithm
tuned.model_f2r2 <- tuneParams(learner  = lrn_f2r2,
                             task       = regr.task,
                             resampling = cv.folds,
                             par.set    = model.params_f2r2,
                             control    = random.tune,
                             show.info  = FALSE)

#Redefining Learner with tuned parameters
lrn_f2r2 <- makeFilterWrapper(learner   = "regr.ctree",
                              fw.method = "party_cforest.importance", 
                              fw.perc   = tuned.model_f2r2$x$fw.perc, 
                              par.set   = tuned.model_f2r2 )

#Training of model to extract features
mod_r2<-mlr::train(lrn_f2r2,regr.task)

#Extract the features
getFilteredFeatures(mod_r2)

cols<-(getFilteredFeatures(mod_r2))
cols<-c(cols,"response")
View(cols)

#Dataframe with selected features
df_tune_r2<-df_samp_reg
df_tune_r2<-df_tune_r2[, names(df_tune_r2) %in% cols] 
ncol(df_tune_r2)

#Getting Accuracy ~ Model training & Testing with subset of features

res_f2r2<-modeltest(df_tune_r2, 'R')
print(res_f2r2)

#Capturing best values of hyperparameter after tuning
df_val_f2r2<-data.frame(Hyperparameter=character(), Value=character())

nwln<- list(Hyperparameter= "No. of Features", Value = tuned.model_f2r2$x$fw.perc)
df_val_f2r2 = rbind(df_val_f2r2,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "teststat", Value = tuned.model_f2r2$x$teststat)
df_val_f2r2 = rbind(df_val_f2r2,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "mincriterion", Value = tuned.model_f2r2$x$mincriterion)
df_val_f2r2 = rbind(df_val_f2r2,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "minsplit", Value = tuned.model_f2r2$x$minsplit)
df_val_f2r2 = rbind(df_val_f2r2,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "minbucket", Value = tuned.model_f2r2$x$minbucket)
df_val_f2r2 = rbind(df_val_f2r2,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "mse", Value = res_f2r2)
df_val_f2r2 = rbind(df_val_f2r2,nwln, stringsAsFactors=FALSE)

#nwln<- list(Hyperparameter= "maxsurrogate", Value = tuned.model_f2r2$x$maxsurrogate)
#df_val_f2r2 = rbind(df_val_f2r2,nwln, stringsAsFactors=FALSE)

#nwln<- list(Hyperparameter= "mtry", Value = tuned.model_f2r2$x$mtry)
#df_val_f2r2 = rbind(df_val_f2r2,nwln, stringsAsFactors=FALSE)

#nwln<- list(Hyperparameter= "maxdepth", Value = tuned.model_f2r2$x$maxdepth)
#df_val_f2r2 = rbind(df_val_f2r2,nwln, stringsAsFactors=FALSE)

#---------End:Conditional Inference Trees----------------

#---------1.3 R:Bayesian CART----------------------------

#Defining learner model
lrn_f2r3 <- makeFilterWrapper("regr.bcart", fw.method = "party_cforest.importance")

#Get parameter set of the learner
getParamSet(lrn_f2r3)

# Define parameters of model and search grid ~ !!!! MODEL SPECIFIC !!!!
model.params_f2r3 <- makeParamSet(makeNumericParam("fw.perc",lower = 0, upper = 1),
                                 makeDiscreteParam("bprior",values = c("b0","b0not","bflat","bmle","bmznot","bmzt")),
                                  makeIntegerParam("R",      lower = 1, upper = 10),
                                  makeIntegerParam("verb",   lower = 0, upper = 4)
)

# Tune model to find best performing parameter settings using random search algorithm
tuned.model_f2r3 <- tuneParams(learner  = lrn_f2r3,
                             task       = regr.task,
                             resampling = cv.folds,
                             par.set    = model.params_f2r3,
                             control    = random.tune,
                             show.info  = FALSE)

#Redefining Learner with tuned parameters
lrn_f2r3 <- makeFilterWrapper(learner = "regr.bcart",
                            fw.method = "party_cforest.importance", 
                            fw.perc   = tuned.model_f2r3$x$fw.perc, 
                            par.set   = tuned.model_f2r3 )

#Training of model to extract features
mod_r3<-mlr::train(lrn_f2r3,regr.task)

#Extract the features
getFilteredFeatures(mod_r3)

cols<-(getFilteredFeatures(mod_r3))
cols<-c(cols,"response")
View(cols)

#Dataframe with selected features
df_tune_r3<-df_samp_reg
df_tune_r3<-df_tune_r3[, names(df_tune_r3) %in% cols] 
ncol(df_tune_r3)

#Getting Accuracy ~ Model training & Testing with subset of features
res_f2r3<-modeltest(df_tune_r3, 'R')
print(res_f2r3)

#Capturing best values of hyperparameter after tuning
df_val_f2r3<-data.frame(Hyperparameter=character(), Value=character())

nwln<- list(Hyperparameter= "No. of Features", Value = tuned.model_f2r3$x$fw.perc)
df_val_f2r3 = rbind(df_val_f2r3,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "bprior", Value = tuned.model_f2r3$x$bprior)
df_val_f2r3 = rbind(df_val_f2r3,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "R", Value = tuned.model_f2r3$x$R)
df_val_f2r3 = rbind(df_val_f2r3,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "verb", Value = tuned.model_f2r3$x$verb)
df_val_f2r3 = rbind(df_val_f2r3,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "mse", Value = res_f2r3)
df_val_f2r3 = rbind(df_val_f2r3,nwln, stringsAsFactors=FALSE)

print(df_val_f2r3)
#---------End:Bayesian CART------------------------------

##CLASSIFICATION
#---------------

#---------2.1 C:Binomial Regression----------------------

#Defining learner model
lrn_f2c1 <- makeFilterWrapper("classif.binomial", fw.method = "party_cforest.importance")

#Get parameter set of the learner
getParamSet(lrn_f2c1)

# Define parameters of model and search grid ~ !!!! MODEL SPECIFIC !!!!
model.params_f2c1 <- makeParamSet(makeNumericParam("fw.perc",lower = 0, upper = 1),
                                   makeDiscreteParam("link",values = c("logit","probit","cloglog","cauchit","log"))
                                
)

# Tune model to find best performing parameter settings using random search algorithm
tuned.model_f2c1 <- tuneParams(learner  = lrn_f2c1,
                             task       = clsf.task,
                             resampling = cv.folds,
                             par.set    = model.params_f2c1,
                             control    = random.tune,
                             show.info  = FALSE)

#Redefining Learner with tuned parameters
lrn_f2c1 <- makeFilterWrapper(learner   = "classif.binomial",
                            fw.method = "party_cforest.importance", 
                            fw.perc   = tuned.model_f2c1$x$fw.perc, 
                            par.set   = tuned.model_f2c1 )

#Training of model to extract features
mod_c1<-mlr::train(lrn_f2c1,clsf.task)

#Extract the features
getFilteredFeatures(mod_c1)

cols<-(getFilteredFeatures(mod_c1))
cols<-c(cols,"response")
View(cols)

#Dataframe with selected features
df_tune_c1<-df_samp_cls
df_tune_c1<-df_tune_c1[, names(df_tune_c1) %in% cols] 
ncol(df_tune_c1)

#Getting Accuracy ~ Model training & Testing with subset of features
res_f2c1<-modeltest(df_tune_c1, 'C')
print(res_f2c1)

#Capturing best values of hyperparameter after tuning
df_val_f2c1<-data.frame(Hyperparameter=character(), Value=character())

nwln<- list(Hyperparameter= "No. of Features", Value = tuned.model_f2c1$x$fw.perc)
df_val_f2c1 = rbind(df_val_f2c1,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "link", Value = tuned.model_f2c1$x$link)
df_val_f2c1 = rbind(df_val_f2c1,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "Accuracy", Value = res_f2c1)
df_val_f2c1 = rbind(df_val_f2c1,nwln, stringsAsFactors=FALSE)

print(df_val_f2c1)

#------------End of Binomial Regression------------------

#---------2.2 C:Fast k-Nearest Neighbour-----------------

#Defining learner model
lrn_f2c2 <- makeFilterWrapper("classif.fnn", fw.method = "party_cforest.importance")

#Get parameter set of the learner
getParamSet(lrn_f2c2)

# Define parameters of model and search grid ~ !!!! MODEL SPECIFIC !!!!
model.params_f2c2 <- makeParamSet(makeNumericParam("fw.perc",lower = 0, upper = 1),
                                  makeIntegerParam( "k",     lower = 1, upper = 10),
                              makeDiscreteParam("algorithm",values = c("cover_tree","kd_tree","brute"))
                                
)

# Tune model to find best performing parameter settings using random search algorithm
tuned.model_f2c2 <- tuneParams(learner  = lrn_f2c2,
                             task       = clsf.task,
                             resampling = cv.folds,
                             par.set    = model.params_f2c2,
                             control    = random.tune,
                             show.info  = FALSE)

#Redefining Learner with tuned parameters
lrn_f2c2 <- makeFilterWrapper(learner   = "classif.fnn",
                            fw.method = "party_cforest.importance", 
                            fw.perc   = tuned.model_f2c2$x$fw.perc, 
                            par.set   = tuned.model_f2c2 )

#Training of model to extract features
mod_c2<-mlr::train(lrn_f2c2,clsf.task)

#Extract the features
getFilteredFeatures(mod_c2)

cols<-(getFilteredFeatures(mod_c2))
cols<-c(cols,"response")
View(cols)

#Dataframe with selected features
df_tune_c2<-df_samp_cls
df_tune_c2<-df_tune_c2[, names(df_tune_c2) %in% cols] 
ncol(df_tune_c2)

#Getting Accuracy ~ Model training & Testing with subset of features
res_f2c2<-modeltest(df_tune_c2, 'C')
print(res_f2c2)

#Capturing best values of hyperparameter after tuning
df_val_f2c2<-data.frame(Hyperparameter=character(), Value=character())

nwln<- list(Hyperparameter= "No. of Features", Value = tuned.model_f2c2$x$fw.perc)
df_val_f2c2 = rbind(df_val_f2c2,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "k", Value = tuned.model_f2c2$x$k)
df_val_f2c2 = rbind(df_val_f2c2,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "algorithm", Value = tuned.model_f2c2$x$algorithm)
df_val_f2c2 = rbind(df_val_f2c2,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "Accuracy", Value = res_f2c2)
df_val_f2c2 = rbind(df_val_f2c2,nwln, stringsAsFactors=FALSE)

print(df_val_f2c2)

#------------End of Fast k-Nearest Neighbour-------------

#---------2.3 C:Linear Discriminant Analysis-------------

#Defining learner model
lrn_f2c3 <- makeFilterWrapper("classif.lda", fw.method = "party_cforest.importance")

#Get parameter set of the learner
getParamSet(lrn_f2c3)

# Define parameters of model and search grid ~ !!!! MODEL SPECIFIC !!!!
model.params_f2c3 <- makeParamSet(makeNumericParam("fw.perc", lower = 0, upper = 1),
                                  makeNumericParam( "nu",     lower = 2, upper = 10),
                                  makeNumericParam( "tol",    lower = 0, upper = 0.0005),
                                makeDiscreteParam("predict.method",values = c("plug-in","predictive","debiased"))
                                
)

# Tune model to find best performing parameter settings using random search algorithm
tuned.model_f2c3 <- tuneParams(learner  = lrn_f2c3,
                             task       = clsf.task,
                             resampling = cv.folds,
                             par.set    = model.params_f2c3,
                             control    = random.tune,
                             show.info  = FALSE)

#Redefining Learner with tuned parameters
lrn_f2c3 <- makeFilterWrapper(learner = "classif.lda",
                            fw.method = "party_cforest.importance", 
                            fw.perc   = tuned.model_f2c3$x$fw.perc, 
                            par.set   = tuned.model_f2c3 )

#Training of model to extract features
mod_c3<-mlr::train(lrn_f2c3,clsf.task)

#Extract the features
getFilteredFeatures(mod_c3)

cols<-(getFilteredFeatures(mod_c3))
cols<-c(cols,"response")
View(cols)

#Dataframe with selected features
df_tune_c3<-df_samp_cls
df_tune_c3<-df_tune_c3[, names(df_tune_c3) %in% cols] 
ncol(df_tune_c3)

#Getting Accuracy ~ Model training & Testing with subset of features
res_f2c3<-modeltest(df_tune_c3, 'C')
print(res_f2c3)

#Capturing best values of hyperparameter after tuning
df_val_f2c3<-data.frame(Hyperparameter=character(), Value=character())

nwln<- list(Hyperparameter= "No. of Features", Value = tuned.model_f2c3$x$fw.perc)
df_val_f2c3 = rbind(df_val_f2c3,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "nu", Value = tuned.model_f2c3$x$nu)
df_val_f2c3 = rbind(df_val_f2c3,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "tol", Value = tuned.model_f2c3$x$tol)
df_val_f2c3 = rbind(df_val_f2c3,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "predict.method", Value = tuned.model_f2c3$x$predict.method)
df_val_f2c3 = rbind(df_val_f2c3,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "Accuracy", Value = res_f2c3)
df_val_f2c3 = rbind(df_val_f2c3,nwln, stringsAsFactors=FALSE)

print(df_val_f2c3)

#------------End of Fast k-Nearest Neighbour-------------
#------------End of party_cforest.importance-------------


#----------FS:3.FSelectorRcpp_information.gain-----------

##REGRESSION
#-----------

#---------1.1 R:K-Nearest-Neighbor regressiong------------

#Defining learner model
lrn_f3r1 <- makeFilterWrapper("regr.kknn")

#Get parameter set of the learner
getParamSet(lrn_f3r1)

# Define parameters of model and search grid ~ !!!! MODEL SPECIFIC !!!!
model.params_f3r1 <- makeParamSet(
  makeNumericParam("fw.perc", lower = 0, upper = 1),
  makeDiscreteParam("fw.method", values = c("FSelectorRcpp_information.gain")),
  makeIntegerParam("k",       lower = 1, upper = 10),
  makeNumericParam("distance",lower = 1, upper = 10)
)

# Tune model to find best performing parameter settings using random search algorithm
tuned.model_f3r1 <- tuneParams(learner   = lrn_f3r1,
                               task       = regr.task,
                               resampling = cv.folds,
                               par.set    = model.params_f3r1,
                               control    = random.tune,
                               show.info  = FALSE)

#Redefining Learner with tuned parameters
lrn_f3r1 <- makeFilterWrapper(learner   = "regr.kknn",
                              fw.method = tuned.model_f3r1$x$fw.method, 
                              fw.perc   = tuned.model_f3r1$x$fw.perc, 
                              par.set   = tuned.model_f3r1 )

#Training of model to extract features
mod_r1<-mlr::train(lrn_f3r1,regr.task)

#Extract the features
getFilteredFeatures(mod_r1)

cols<-(getFilteredFeatures(mod_r1))
cols<-c(cols,"response")
View(cols)

#Dataframe with selected features
df_tune_r1<-df_samp_reg
df_tune_r1<-df_tune_r1[, names(df_tune_r1) %in% cols] 
ncol(df_tune_r1)

#Getting Accuracy ~ Model training & Testing with subset of features

res_f3r1<-modeltest(df_tune_r1, 'R')
print(res_f3r1)

#Capturing best values of hyperparameter after tuning
df_val_f3r1<-data.frame(Hyperparameter=character(), Value=character())

nwln<- list(Hyperparameter= "No. of Features", Value = tuned.model_f3r1$x$fw.perc)
df_val_f3r1 = rbind(df_val_f3r1,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "Filter Method", Value = tuned.model_f3r1$x$fw.method)
df_val_f3r1 = rbind(df_val_f3r1,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "k", Value = tuned.model_f3r1$x$k)
df_val_f3r1 = rbind(df_val_f3r1,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "distance", Value = tuned.model_f3r1$x$distance)
df_val_f3r1 = rbind(df_val_f3r1,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "mse", Value = res_f3r1)
df_val_f3r1 = rbind(df_val_f3r1,nwln, stringsAsFactors=FALSE)

print(df_val_f3r1)
#---------End:K-Nearest-Neighbor regressiong-------------

#---------1.2 R:Conditional Inference Trees--------------

#Defining learner model
lrn_f3r2 <- makeFilterWrapper("regr.ctree", fw.method = "FSelectorRcpp_information.gain")

#Get parameter set of the learner
getParamSet(lrn_f3r2)

# Define parameters of model and search grid ~ !!!! MODEL SPECIFIC !!!!
model.params_f3r2 <- makeParamSet(makeNumericParam("fw.perc",      lower = 0, upper = 1),
                                  makeDiscreteParam("teststat",    values = c("quad","max")),
                                  makeNumericParam("mincriterion", lower = 0, upper = 1),
                                  makeIntegerParam("minsplit",     lower = 1, upper = 50), #remove cmt for org data
                                  makeIntegerParam("minbucket",    lower = 1, upper = 10)
                                  #makeIntegerParam("maxsurrogate", lower = 0, upper = 10)
                                  #makeIntegerParam("mtry",         lower = 0, upper = 5)
                                  #makeIntegerParam("maxdepth",     lower = 0, upper = 10)
)

# Tune model to find best performing parameter settings using random search algorithm
tuned.model_f3r2 <- tuneParams(learner  = lrn_f3r2,
                               task       = regr.task,
                               resampling = cv.folds,
                               par.set    = model.params_f3r2,
                               control    = random.tune,
                               show.info  = FALSE)

#Redefining Learner with tuned parameters
lrn_f3r2 <- makeFilterWrapper(learner   = "regr.ctree",
                              fw.method = "FSelectorRcpp_information.gain", 
                              fw.perc   = tuned.model_f3r2$x$fw.perc, 
                              par.set   = tuned.model_f3r2 )

#Training of model to extract features
mod_r2<-mlr::train(lrn_f3r2,regr.task)

#Extract the features
getFilteredFeatures(mod_r2)

cols<-(getFilteredFeatures(mod_r2))
cols<-c(cols,"response")
View(cols)

#Dataframe with selected features
df_tune_r2<-df_samp_reg
df_tune_r2<-df_tune_r2[, names(df_tune_r2) %in% cols] 
ncol(df_tune_r2)

#Getting Accuracy ~ Model training & Testing with subset of features

res_f3r2<-modeltest(df_tune_r2, 'R')
print(res_f3r2)

#Capturing best values of hyperparameter after tuning
df_val_f3r2<-data.frame(Hyperparameter=character(), Value=character())

nwln<- list(Hyperparameter= "No. of Features", Value = tuned.model_f3r2$x$fw.perc)
df_val_f3r2 = rbind(df_val_f3r2,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "teststat", Value = tuned.model_f3r2$x$teststat)
df_val_f3r2 = rbind(df_val_f3r2,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "mincriterion", Value = tuned.model_f3r2$x$mincriterion)
df_val_f3r2 = rbind(df_val_f3r2,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "minsplit", Value = tuned.model_f3r2$x$minsplit)
df_val_f3r2 = rbind(df_val_f3r2,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "minbucket", Value = tuned.model_f3r2$x$minbucket)
df_val_f3r2 = rbind(df_val_f3r2,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "mse", Value = res_f3r2)
df_val_f3r2 = rbind(df_val_f3r2,nwln, stringsAsFactors=FALSE)

#nwln<- list(Hyperparameter= "maxsurrogate", Value = tuned.model_f3r2$x$maxsurrogate)
#df_val_f3r2 = rbind(df_val_f3r2,nwln, stringsAsFactors=FALSE)

#nwln<- list(Hyperparameter= "mtry", Value = tuned.model_f3r2$x$mtry)
#df_val_f3r2 = rbind(df_val_f3r2,nwln, stringsAsFactors=FALSE)

#nwln<- list(Hyperparameter= "maxdepth", Value = tuned.model_f3r2$x$maxdepth)
#df_val_f3r2 = rbind(df_val_f3r2,nwln, stringsAsFactors=FALSE)

#---------End:Conditional Inference Trees----------------

#---------1.3 R:Bayesian CART----------------------------

#Defining learner model
lrn_f3r3 <- makeFilterWrapper("regr.bcart", fw.method = "FSelectorRcpp_information.gain")

#Get parameter set of the learner
getParamSet(lrn_f3r3)

# Define parameters of model and search grid ~ !!!! MODEL SPECIFIC !!!!
model.params_f3r3 <- makeParamSet(makeNumericParam("fw.perc",lower = 0, upper = 1),
                                  makeDiscreteParam("bprior",values = c("b0","b0not","bflat","bmle","bmznot","bmzt")),
                                  makeIntegerParam("R",      lower = 1, upper = 10),
                                  makeIntegerParam("verb",   lower = 0, upper = 4)
)

# Tune model to find best performing parameter settings using random search algorithm
tuned.model_f3r3 <- tuneParams(learner  = lrn_f3r3,
                               task       = regr.task,
                               resampling = cv.folds,
                               par.set    = model.params_f3r3,
                               control    = random.tune,
                               show.info  = FALSE)

#Redefining Learner with tuned parameters
lrn_f3r3 <- makeFilterWrapper(learner = "regr.bcart",
                              fw.method = "FSelectorRcpp_information.gain", 
                              fw.perc   = tuned.model_f3r3$x$fw.perc, 
                              par.set   = tuned.model_f3r3 )

#Training of model to extract features
mod_r3<-mlr::train(lrn_f3r3,regr.task)

#Extract the features
getFilteredFeatures(mod_r3)

cols<-(getFilteredFeatures(mod_r3))
cols<-c(cols,"response")
View(cols)

#Dataframe with selected features
df_tune_r3<-df_samp_reg
df_tune_r3<-df_tune_r3[, names(df_tune_r3) %in% cols] 
ncol(df_tune_r3)

#Getting Accuracy ~ Model training & Testing with subset of features
res_f3r3<-modeltest(df_tune_r3, 'R')
print(res_f3r3)

#Capturing best values of hyperparameter after tuning
df_val_f3r3<-data.frame(Hyperparameter=character(), Value=character())

nwln<- list(Hyperparameter= "No. of Features", Value = tuned.model_f3r3$x$fw.perc)
df_val_f3r3 = rbind(df_val_f3r3,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "bprior", Value = tuned.model_f3r3$x$bprior)
df_val_f3r3 = rbind(df_val_f3r3,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "R", Value = tuned.model_f3r3$x$R)
df_val_f3r3 = rbind(df_val_f3r3,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "verb", Value = tuned.model_f3r3$x$verb)
df_val_f3r3 = rbind(df_val_f3r3,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "mse", Value = res_f3r3)
df_val_f3r3 = rbind(df_val_f3r3,nwln, stringsAsFactors=FALSE)

print(df_val_f3r3)
#---------End:Bayesian CART------------------------------

##CLASSIFICATION
#---------------

#---------2.1 C:Binomial Regression----------------------

#Defining learner model
lrn_f3c1 <- makeFilterWrapper("classif.binomial", fw.method = "FSelectorRcpp_information.gain")

#Get parameter set of the learner
getParamSet(lrn_f3c1)

# Define parameters of model and search grid ~ !!!! MODEL SPECIFIC !!!!
model.params_f3c1 <- makeParamSet(makeNumericParam("fw.perc",lower = 0, upper = 1),
                                  makeDiscreteParam("link",values = c("logit","probit","cloglog","cauchit","log"))
                                  
)

# Tune model to find best performing parameter settings using random search algorithm
tuned.model_f3c1 <- tuneParams(learner    = lrn_f3c1,
                               task       = clsf.task,
                               resampling = cv.folds,
                               par.set    = model.params_f3c1,
                               control    = random.tune,
                               show.info  = FALSE)

#Redefining Learner with tuned parameters
lrn_f3c1 <- makeFilterWrapper(learner   = "classif.binomial",
                              fw.method = "FSelectorRcpp_information.gain", 
                              fw.perc   = tuned.model_f3c1$x$fw.perc, 
                              par.set   = tuned.model_f3c1 )

#Training of model to extract features
mod_c1<-mlr::train(lrn_f3c1,clsf.task)

#Extract the features
getFilteredFeatures(mod_c1)

cols<-(getFilteredFeatures(mod_c1))
cols<-c(cols,"response")
print(cols)

#Dataframe with selected features
df_tune_c1<-df_samp_cls
df_tune_c1<-df_tune_c1[, names(df_tune_c1) %in% cols] 
print(ncol(df_tune_c1))

#Getting Accuracy ~ Model training & Testing with subset of features
res_f3c1<-modeltest(df_tune_c1, 'C')
print(res_f3c1)

#Capturing best values of hyperparameter after tuning
df_val_f3c1<-data.frame(Hyperparameter=character(), Value=character())

nwln<- list(Hyperparameter= "No. of Features", Value = tuned.model_f3c1$x$fw.perc)
df_val_f3c1 = rbind(df_val_f3c1,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "link", Value = tuned.model_f3c1$x$link)
df_val_f3c1 = rbind(df_val_f3c1,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "Accuracy", Value = res_f3c1)
df_val_f3c1 = rbind(df_val_f3c1,nwln, stringsAsFactors=FALSE)

print(df_val_f3c1)

#------------End of Binomial Regression------------------

#---------2.2 C:Fast k-Nearest Neighbour-----------------

#Defining learner model
lrn_f3c2 <- makeFilterWrapper("classif.fnn", fw.method = "FSelectorRcpp_information.gain")

#Get parameter set of the learner
getParamSet(lrn_f3c2)

# Define parameters of model and search grid ~ !!!! MODEL SPECIFIC !!!!
model.params_f3c2 <- makeParamSet(makeNumericParam("fw.perc",lower = 0, upper = 1),
                                  makeIntegerParam( "k",     lower = 1, upper = 10),
                                  makeDiscreteParam("algorithm",values = c("cover_tree","kd_tree","brute"))
                                  
)

# Tune model to find best performing parameter settings using random search algorithm
tuned.model_f3c2 <- tuneParams(learner    = lrn_f3c2,
                               task       = clsf.task,
                               resampling = cv.folds,
                               par.set    = model.params_f3c2,
                               control    = random.tune,
                               show.info  = FALSE)

#Redefining Learner with tuned parameters
lrn_f3c2 <- makeFilterWrapper(learner   = "classif.fnn",
                              fw.method = "FSelectorRcpp_information.gain", 
                              fw.perc   = tuned.model_f3c2$x$fw.perc, 
                              par.set   = tuned.model_f3c2 )

#Training of model to extract features
mod_c2<-mlr::train(lrn_f3c2,clsf.task)

#Extract the features
getFilteredFeatures(mod_c2)

cols<-(getFilteredFeatures(mod_c2))
cols<-c(cols,"response")
View(cols)

#Dataframe with selected features
df_tune_c2<-df_samp_cls
df_tune_c2<-df_tune_c2[, names(df_tune_c2) %in% cols] 
ncol(df_tune_c2)

#Getting Accuracy ~ Model training & Testing with subset of features
res_f3c2<-modeltest(df_tune_c2, 'C')
print(res_f3c2)

#Capturing best values of hyperparameter after tuning
df_val_f3c2<-data.frame(Hyperparameter=character(), Value=character())

nwln<- list(Hyperparameter= "No. of Features", Value = tuned.model_f3c2$x$fw.perc)
df_val_f3c2 = rbind(df_val_f3c2,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "k", Value = tuned.model_f3c2$x$k)
df_val_f3c2 = rbind(df_val_f3c2,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "algorithm", Value = tuned.model_f3c2$x$algorithm)
df_val_f3c2 = rbind(df_val_f3c2,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "Accuracy", Value = res_f3c2)
df_val_f3c2 = rbind(df_val_f3c2,nwln, stringsAsFactors=FALSE)

print(df_val_f3c2)

#------------End of Fast k-Nearest Neighbour-------------

#---------2.3 C:Linear Discriminant Analysis-------------

#Defining learner model
lrn_f3c3 <- makeFilterWrapper("classif.lda", fw.method = "FSelectorRcpp_information.gain")

#Get parameter set of the learner
getParamSet(lrn_f3c3)

# Define parameters of model and search grid ~ !!!! MODEL SPECIFIC !!!!
model.params_f3c3 <- makeParamSet(makeNumericParam("fw.perc", lower = 0, upper = 1),
                                  makeNumericParam( "nu",     lower = 2, upper = 10),
                                  makeNumericParam( "tol",    lower = 0, upper = 0.0005),
                                  makeDiscreteParam("predict.method",values = c("plug-in","predictive","debiased"))
                                  
)

# Tune model to find best performing parameter settings using random search algorithm
tuned.model_f3c3 <- tuneParams(learner    = lrn_f3c3,
                               task       = clsf.task,
                               resampling = cv.folds,
                               par.set    = model.params_f3c3,
                               control    = random.tune,
                               show.info  = FALSE)

#Redefining Learner with tuned parameters
lrn_f3c3 <- makeFilterWrapper(learner = "classif.lda",
                              fw.method = "FSelectorRcpp_information.gain", 
                              fw.perc   = tuned.model_f3c3$x$fw.perc, 
                              par.set   = tuned.model_f3c3 )

#Training of model to extract features
mod_c3<-mlr::train(lrn_f3c3,clsf.task)

#Extract the features
getFilteredFeatures(mod_c3)

cols<-(getFilteredFeatures(mod_c3))
cols<-c(cols,"response")
View(cols)

#Dataframe with selected features
df_tune_c3<-df_samp_cls
df_tune_c3<-df_tune_c3[, names(df_tune_c3) %in% cols] 
ncol(df_tune_c3)

#Getting Accuracy ~ Model training & Testing with subset of features
res_f3c3<-modeltest(df_tune_c3, 'C')
print(res_f3c3)

#Capturing best values of hyperparameter after tuning
df_val_f3c3<-data.frame(Hyperparameter=character(), Value=character())

nwln<- list(Hyperparameter= "No. of Features", Value = tuned.model_f3c3$x$fw.perc)
df_val_f3c3 = rbind(df_val_f3c3,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "nu", Value = tuned.model_f3c3$x$nu)
df_val_f3c3 = rbind(df_val_f3c3,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "tol", Value = tuned.model_f3c3$x$tol)
df_val_f3c3 = rbind(df_val_f3c3,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "predict.method", Value = tuned.model_f3c3$x$predict.method)
df_val_f3c3 = rbind(df_val_f3c3,nwln, stringsAsFactors=FALSE)

nwln<- list(Hyperparameter= "Accuracy", Value = res_f3c3)
df_val_f3c3 = rbind(df_val_f3c3,nwln, stringsAsFactors=FALSE)

print(df_val_f3c3)

#------------End of Fast k-Nearest Neighbour-------------
#--------------------------------------------------------
