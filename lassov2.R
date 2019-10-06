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

df_male_train$sozk_soz01_male
# Dumy code categorical predictor variables
x.male.lasso <- model.matrix(sozk_soz01_male~., df_male_train)

# Convert the outcome (class) to a numerical variable
y.male.lasso <- ifelse(df_male_train$sozk_soz01_male == "1", 1, 0)
y.male.lassopred<- ifelse(df_male_test$sozk_soz01_male == "1", 1, 0)

data<-cbind(x.male.lasso,y.male.lasso)
model<-model.matrix(y.male.lasso~., data=data.frame(data))

lambda_seq <- 10^seq(2, -2, by = -.1)

## Apply lasso regression 
tinn_lasso <- glmnet(
  x = x.male.lasso,
  y = y.male.lasso,
  alpha = 1
)

plot(tinn_lasso, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(X), cex = .7)

#tuning of lamba
# Apply CV Ridge regression to ames data
tinn_lasso.cv <- cv.glmnet(
  x = x.male.lasso,
  y = y.male.lasso,
  alpha = 1,
  lambda = lambda_seq
)
# plot results
plot(tinn_lasso.cv)

min(tinn_lasso.cv$cvm)       # minimum MSE

tinn_lasso.cv$lambda.min     # lambda for this min MSE


tinn_lasso.cv$cvm[tinn_lasso.cv$lambda == tinn_lasso.cv$lambda.1se]  # 1 st.error of min MSE

tinn_lasso.cv$lambda.1se  # lambda for this MSE

# identifying best lamda
best_lambda <- tinn_lasso.cv$lambda.min
print(best_lambda)

# Rebuilding the model with best lamda value identified
tinn_lasso_min <- glmnet(  x = x.male.lasso,
                           y = y.male.lasso,
                           alpha = 1, 
                           lambda = best_lambda)


tinn_lasso_pred <- predict(tinn_lasso_min, 
                           s = best_lambda, 
                           newx = as.matrix(df_male_test), 
                           type = "coefficients")
plot(tinn_lasso_min, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(X), cex = .7)

plot(tinn_lasso.cv, xvar = "lambda")
abline(v = log(tinn_lasso.cv$lambda.min), col = "red", lty = "dashed")
abline(v = log(tinn_lasso.cv$lambda.1se), col = "red", lty = "dashed")

coef(tinn_lasso.cv, s = "best_lambda") %>%
  tidy() %>%
  filter(row != "(Intercept)") %>%
  ggplot(aes(value, reorder(row, value), color = value > 0)) +
  geom_point(show.legend = FALSE) +
  ggtitle("Influential variables") +
  xlab("Coefficient") +
  ylab(NULL)
#-----------------------------------------
best_lambda_lasso <- tinn_lasso.cv$lambda.min  # largest lambda in 1 SE
lasso_coef <- tinn_lasso.cv$glmnet.fit$beta[,  # retrieve coefficients
                                            tinn_lasso.cv$glmnet.fit$lambda  # at lambda.1se
                                          == best_lambda_lasso]
coef_l = data.table(lasso = lasso_coef)      # build table

coef_l[, feature := names(lasso_coef)]       # add feature names

coef_l<-filter(coef_l,lasso > 0.0000000000)

to_plot_l = melt(coef_l                      # label table
                 , id.vars='feature'
                 , variable.name = 'model'
                 , value.name = 'coefficient')
ggplot(data=to_plot_l,                       # plot coefficients
       aes(x=feature, y=coefficient, fill=model)) +
  coord_flip() +         
  geom_bar(stat='identity', fill='brown4', color='blue') +
  facet_wrap(~ model) + guides(fill=FALSE) 
#==========Model relience====================
library(iml)

X = df_male_train[which(names(df_male_train) != "sozk_soz01_male")]
predictor = Predictor$new(tinn_lasso_min, data = data.frame(x.male.lasso), y = y.male.lasso)                    
imp = FeatureImp$new(predictor, loss = "ce")
plot(imp)
