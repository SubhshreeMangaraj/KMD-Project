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
library(iml)
library(R6)
library(devtools)
library(parallelMap)
#Loading of data

#---------------
df_data_g<- df_tune_w4c1 <- readr::read_csv(file.path(here::here(),"df_filter_gen.csv"))
df_data_g <- df_data_g[, -c(1)]
df_data_g$sozk_soz01_male<-as.character(df_data_g$sozk_soz01_male)

# Create training (70%) and test (30%) sets 
# Use set.seed for reproducibility
#-------------------------------------------
set.seed(123)
split_df<-sample(nrow(df_data_g), 2/3 * nrow(df_data_g))

df_male_train <- df_data_g[split_df, ]
df_male_test  <- df_data_g[-split_df, ]

df_male_train$sozk_soz01_male

#Task for training data
task_svm<-makeClassifTask(id = "Tinnitus_svm", 
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
lrn.svm<-makeFilterWrapper("classif.svm")

#Get parameter set of the learner
#--------------------------------
getParamSet(lrn.svm)

# Define parameters of model and search grid ~ !!!! MODEL SPECIFIC !!!!
#----------------------------------------------------------------------
model.params.svm <- makeParamSet(makeIntegerParam("fw.perc", lower = 0, upper = 1),
                                 makeDiscreteParam("type", values = "C-classification"),
                                 makeNumericParam("cost", lower = 0, upper = 5),
                                 makeDiscreteParam("kernel", values = c("linear","polynomial","radial","sigmoid")),
                                 makeNumericParam("gamma", lower = 0, upper = 5))

#makeIntegerParam("degree", lower = 1, upper = 6))
tune.svm<-tuneParams( learner    = lrn.svm,
                      task       = task_svm,
                      resampling = cv.folds,
                      par.set    = model.params.svm,
                      control    = random.tune )

#-------------------------------------------------------------------







#--------------------------------------------------------------------
lrn.svm <- makeFilterWrapper(learner   = "classif.svm", 
                             fw.perc   = tune.svm$x$fw.perc, 
                             par.set   = tune.svm,
                             predict.type = "response")

mod.svm<-mlr::train(lrn.svm,task_svm)

X = df_male_train[which(names(df_male_train) != "sozk_soz01_male")]

predictor = Predictor$new(mod.svm, data = X, y = df_male_train$sozk_soz01_male, class = "1")
mod$predict(df_male_test)

imp = FeatureImp$new(mod, loss = "ce")
plot(imp)
imp$results

