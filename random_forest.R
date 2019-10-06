#Gender based Models: 1 : Random Forest(ranger)

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

#Loading of data

#---------------
df_data_g<- df_tune_w4c1 <- readr::read_csv(file.path(here::here(),"Tinnitus","data", "df_filter_gen.csv"))
df_data_g <- df_data_g[, -c(1)]
df_data_g$sozk_soz01_male<-as.factor(df_data_g$sozk_soz01_male)

#Task for training data
#----------------------
task_male<-makeClassifTask(id = "Tinnitus_gender", 
                           data = data.frame(df_data_g), 
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
getParamSet(lrn_male)

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
                      task       = task_male,
                      resampling = cv.folds,
                      control    = random.tune,
                      par.set    = model.params.rf.g )
#Values of tuned Hyperparameters
#---------------
df_hyper<-tune.rf.g$x
View(df_hyper)

#Constructing a learner with ranger classifier with tuned hyperparameters
#------------------------------------------------------------------------
lrn_rf.g<-setHyperPars(makeLearner(("classif.ranger"), par.vals = list(num.trees       = tune_male.rf$x$num.trees,
                                                                       mtry            = tune_male.rf$x$mtry,
                                                                       min.node.size   = tune_male.rf$x$min.node.size,
                                                                       sample.fraction = tune_male.rf$x$sample.fraction,
                                                                       importance      = tune_male.rf$x$importance,
                                                                      splitrule       = tune_male.rf$x$splitrule)))
# Use set.seed for reproducibility
#-------------------------------------------
set.seed(123)
split_df<-sample(nrow(df_data_g), 2/3 * nrow(df_data_g))

df.rf.train <- df_data_g[split_df, ]
df.rf.test  <- df_data_g[-split_df, ]
# Train the learner
#------------------
mod.rf.g<-ranger( sozk_soz01_male~.,
                       data            = df.rf.train,
                       num.trees       = tune.rf.g$x$num.trees,
                       mtry            = tune.rf.g$x$mtry,
                       min.node.size   = tune.rf.g$x$min.node.size,
                       sample.fraction = tune.rf.g$x$sample.fraction,
                       importance      = "impurity",
                       splitrule       = tune.rf.g$x$splitrule,
                       probability     = TRUE )

View(mod.rf.g$variable.importance)
importance(mod.rf.g)

#Plotting the variable importance
vi_plot <- mod.rf.g$variable.importance
barplot(vi_plot, horiz = TRUE, las = 1)
vip(mod_train_male, width = 0.3, fill = "green3",num_features = 20) 

predict.rf.g<-predict(mod.rf.g,df.rf.test)
confusionMatrix(predict.rf.g, df.rf.test)
mod.rf.g$prediction.error
#---------------------------------------------------------------------------------------------------------------------------------------



y = df.rf.train$sozk_soz01_male
X = df.rf.train
X = X[,!names(X)=="sozk_soz01_male"]

rfo=randomForest(X,
                 y,
                 ntree           = tune.rf.g$x$num.trees,
                 mtry            = tune.rf.g$x$mtry,
                 importance      = TRUE,
                 nodesize        = tune.rf.g$x$min.node.size,
                 keep.inbag      = TRUE)

ff=forestFloor(rfo,X)
#print forestFloor
print(ff) 

#plot partial functions of most important variables first
plot(ff) 

#Non interacting functions are well displayed, whereas X3 and X4 are not
#by applying different colourgradient, interactions reveal themself 
Col = fcol(ff,3,orderByImportance=FALSE)
plot(ff,col=Col,compute_GOF=TRUE) 


#by applying different colourgradient, interactions reveal themself 
Col = fcol(ff,1,orderByImportance=TRUE)
plot(ff,col=Col,compute_GOF=TRUE) 



#in 3D the interaction between X3 and X reveals itself completely
show3d(ff,1:5,col=Col,plot.rgl=list(size=5),sortByImportance=FALSE) 
#-----------------------------------------
library(ggplot2)
library(ggridges)
ggplot(df.rf.train, aes(x = df.rf.train$response, y = df.rf.train$sozk_soz01_male)) +
  geom_density_ridges(aes(fill = df.rf.train$sozk_soz01_male)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))
#----------------------------------------------
library(ICE)
library(MASS)
library(ICEbox)
library(iml)
library(randomForestSRC)
df.rf.train$tq_tin01
predictor <- Predictor$new(model = rfo, data = X, y = y)
effect_pdp <- FeatureEffect$new(predictor = predictor, feature = "tq_tin01", method = "pdp+ice",
                                # creates marginal prediction for...
                                # ...20 equi-distant values along the range of lstat
                                # grid.size = 20 -> default value
                                grid.size = 20,
                                center.at = 0)
effect_pdp
effect_pdp$plot()
rfo
#-----var imp-------------
library(ggplot2)  # for theme_light() function
#> Warning: package 'ggplot2' was built under R version 3.5.2
vip(rfo, num_features = 20, bar = FALSE, color, horizontal = FALSE, 
    color = "red", shape = 17, size = 4) +
  theme_light()
#------------feature contributions
library(rfFC)
li<-getLocalIncrements(rfo, X)


library(mlbench)
library(AUC)
library(randomForestExplainer)
load("min_depth_frame.rda")
head(min_depth_frame, n = 10)
#----------------------------------------------------
#verify similar performance
plot(roc(rfo$votes[,2],y),main="ROC: default black, robust is red")
plot(roc(rfo$votes[,2],y),col=2,add = T)
auc(roc(rfo$votes[,2],y))
auc(roc(rfo$votes[,2],y))

#compute feature contributions
ff = forestFloor(rfo,X,binary_reg = T,calc_np=T)
ff = convolute_ff(ff)
#the convolutions correlation to the feature contribution



Col = fcol(ff,cols=5,outlier.lim = 2.5)

#the plot in this blog
plot(ff,col=Col,plot_GOF = T)
#some 3d plots
show3d(ff,c(1,5),5,col=Col,plot_GOF = T)
