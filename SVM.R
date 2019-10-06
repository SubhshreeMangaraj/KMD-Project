#Gender based Models: 1 : SVM(e1071)

#Loading of packages
#--------------------
library(readr)
library(here)
library(rio)
library(mlr)
library(glmnet)
library(e1071)
library(dplyr)
library(randomForestSRC)
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

View(df_data_g$sozk_soz01_male)

df_data_g$sozk_soz01_male = as.factor(df_data_g$sozk_soz01_male)

df_male<-filter(df_data_g,sozk_soz01_male == "1")
df_female<-filter(df_data_g,sozk_soz01_male == "0")

df_male$sozk_soz01_male<-NULL
df_female$sozk_soz01_male<-NULL

# Create training (70%) and test (30%) sets 
# Use set.seed for reproducibility
#-------------------------------------------
set.seed(123)
split_df<-sample(nrow(df_male), 2/3 * nrow(df_male))

df_male_train <- df_male[split_df, ]
df_male_test  <- df_male[-split_df, ]

df_male$response = as.factor(df_male$response)
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
lrn_male.svm<-makeLearner("classif.svm")

#Get parameter set of the learner
#--------------------------------
getParamSet(lrn_male.svm)

# Define parameters of model and search grid ~ !!!! MODEL SPECIFIC !!!!
#----------------------------------------------------------------------
model.params.svm <- makeParamSet(makeDiscreteParam("type", values = "C-classification"),
                                 makeNumericParam("cost", lower = 0, upper = 5),
                                 makeDiscreteParam("kernel", values = c("linear","polynomial","radial","sigmoid")),
                                 makeNumericParam("gamma", lower = 0, upper = 5))
#makeIntegerParam("degree", lower = 1, upper = 6))
tune_male<-tuneParams(learner    = "classif.svm",
                      task       = task_male,
                      resampling = cv.folds,
                      control    = random.tune,
                      par.set    = model.params.svm )

lrn_male.svm <- makeFilterWrapper(learner   = "classif.svm", 
                                  par.set   = tune_male,
                                  fw.abs =   20)

mod_male.svm<-mlr::train(lrn_male.svm,task_male)
View(getFilteredFeatures(mod_male.svm))
pred_male.svm = predict(mod_male.svm, task = task_male)

head(as.data.frame(pred_male.svm))
calculateConfusionMatrix(pred_male.svm)
performance(pred_male.svm)

#threshold parameter tuning for prediction
#-------------------------------

## FEMALE
#Task for training data
task_female<-makeClassifTask(id = "Tinnitus_female", 
                             data = data.frame(df_female), 
                             target = "response")

#Constructing a learner with ranger classifier
#---------------------------------------------
lrn_female.svm<-makeLearner("classif.svm")

#Get parameter set of the learner
#--------------------------------
getParamSet(lrn_female.svm)

# Define parameters of model and search grid ~ !!!! MODEL SPECIFIC !!!!
#----------------------------------------------------------------------
model.params.svm.f <- makeParamSet(makeDiscreteParam("type", values = "C-classification"),
                                   makeNumericParam("cost", lower = 0, upper = 5),
                                   makeDiscreteParam("kernel", values = c("linear","polynomial","radial","sigmoid")),
                                   makeNumericParam("gamma", lower = 0, upper = 5))
#makeIntegerParam("degree", lower = 1, upper = 6))
tune_female<-tuneParams(learner    = "classif.svm",
                        task       = task_female,
                        resampling = cv.folds,
                        control    = random.tune,
                        par.set    = model.params.svm.f )

lrn_female.svm<-makeLearner("classif.svm")

lrn_female.svm <- makeFilterWrapper(learner   = "classif.svm", 
                                    par.set   = tune_female,
                                    fw.abs =   20)

mod_female.svm<-mlr::train(lrn_female.svm,task_female)
getFilteredFeatures(mod_female.svm)

pred_female.svm = predict(mod_female.svm, task = task_female)

head(as.data.frame(pred_female.svm))
calculateConfusionMatrix(pred_female.svm)
performance(pred_female.svm)

