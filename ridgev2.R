#Gender based Models: 2 : ridge regression(glmnet with alpha = 1)

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
#Loading of data


#Loading of data

#---------------
df_data_g<- df_tune_w4c1 <- readr::read_csv(file.path(here::here(),"Tinnitus","data", "df_filter_gen.csv"))
df_data_g <- df_data_g[, -c(1)]
df_data_g$sozk_soz01_male<-as.factor(df_data_g$sozk_soz01_male)

# Create training (70%) and test (30%) sets 
# Use set.seed for reproducibility
#-------------------------------------------
set.seed(123)
split_df<-sample(nrow(df_data_g), 2/3 * nrow(df_data_g))

df_male_train <- df_data_g[split_df, ]
df_male_test  <- df_data_g[-split_df, ]

df_male_train$sozk_soz01_male
# Dumy code categorical predictor variables
x.male.ridge <- model.matrix(sozk_soz01_male~., df_male_train)

# Convert the outcome (class) to a numerical variable
y.male.ridge  <- ifelse(df_male_train$sozk_soz01_male == "1", 1, 0)
y.male.ridgepred<- ifelse(df_male_test$sozk_soz01_male == "1", 1, 0)

lambda_seq <- 10^seq(2, -2, by = -.1)

## Apply ridge regression 
tinn_ridge <- glmnet(
  x = x.male.ridge,
  y = y.male.ridge,
  alpha = 1
)

plot(tinn_ridge, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(X), cex = .7)

#tuning of lamba
# Apply CV Ridge regression to ames data
tinn_ridge.cv <- cv.glmnet(
  x = x.male.ridge,
  y = y.male.ridge,
  alpha = 1,
  lambda = lambda_seq
)
# plot results
plot(tinn_ridge.cv)

min(tinn_ridge.cv$cvm)       # minimum MSE

tinn_ridge.cv$lambda.min     # lambda for this min MSE


tinn_ridge.cv$cvm[tinn_ridge.cv$lambda == tinn_ridge.cv$lambda.1se]  # 1 st.error of min MSE

tinn_ridge.cv$lambda.1se  # lambda for this MSE

# identifying best lamda
best_lambda <- tinn_ridge.cv$lambda.min
print(best_lambda)

# Rebuilding the model with best lamda value identified
tinn_ridge_min <- glmnet(  x = x.male.ridge,
                           y = y.male.ridge,
                           alpha = 1, 
                           lambda = best_lambda)

tinn_ridge_pred <- predict(tinn_ridge_min, 
                           s = best_lambda, 
                           newx = as.matrix(df_male_test), 
                           type = "coefficients")
plot(tinn_ridge_min, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(X), cex = .7)

plot(tinn_ridge.cv, xvar = "lambda")
abline(v = log(tinn_ridge.cv$lambda.min), col = "red", lty = "dashed")
abline(v = log(tinn_ridge.cv$lambda.1se), col = "red", lty = "dashed")

coef(tinn_ridge.cv, s = "best_lambda") %>%
  tidy() %>%
  filter(row != "(Intercept)") %>%
  ggplot(aes(value, reorder(row, value), color = value > 0)) +
  geom_point(show.legend = FALSE) +
  ggtitle("Influential variables") +
  xlab("Coefficient") +
  ylab(NULL)
#-----------------------------------------
best_lambda_ridge <- tinn_ridge.cv$lambda.min  # largest lambda in 1 SE
ridge_coef <- tinn_ridge.cv$glmnet.fit$beta[,  # retrieve coefficients
                                            tinn_ridge.cv$glmnet.fit$lambda  # at lambda.1se
                                            == best_lambda_ridge]
coef_l = data.table(ridge = ridge_coef)      # build table

coef_l[, feature := names(ridge_coef)]       # add feature names

coef_l<-filter(coef_l,ridge > 0.0000000000)

to_plot_l = melt(coef_l                      # label table
                 , id.vars='feature'
                 , variable.name = 'model'
                 , value.name = 'coefficient')
ggplot(data=to_plot_l,                       # plot coefficients
       aes(x=feature, y=coefficient, fill=model)) +
  coord_flip() +         
  geom_bar(stat='identity', fill='brown4', color='blue') +
  facet_wrap(~ model) + guides(fill=FALSE) 
