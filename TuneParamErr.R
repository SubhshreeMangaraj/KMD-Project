
library(mlbench)
library(randomForest)
library(FSelector)
library(mlr)
library(RSQLite)
library(ParamHelpers)

df_data<-readRDS(file.path(here::here(), "Train.rds"))

ncol(df_data)
task<-makeClassifTask(data = df_data, target = "response")

ps = makeParamSet(makeNumericParam("fw.perc", lower = 0, upper = 1),
                  makeNumericParam("C", lower = -10, upper = 10, 
                                   trafo = function(x) 2^x),
                  makeNumericParam("sigma", lower = -10, upper = 10,
                                   trafo = function(x) 2^x)
)

ctrl = makeTuneControlGrid(resolution = 2L)
rdesc = makeResampleDesc("CV", iters = 2L)
res = tuneParams("classif.lda", task, rdesc, par.set = ps, control = ctrl)

lrn3 = makeFilterWrapper(learner = "classif.lda", fw.perc = 0.25,
                         fw.method = "randomForest.importance", C = res2$x$C, sigma = res2$x$sigma)

mod1 = train(lrn3, task)
getFilteredFeatures(mod1)