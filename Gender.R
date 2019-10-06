#--------GENDER BASED MODELS----------------------------

library(tidyverse)
library(dplyr)
#--------PREPROCESSING----------------------------------

df_data_reg_g <- readr::read_rds(file.path(here::here(),"Tinnitus","data", "adsl_adsl_sum-dur-regr_df.rds"))

colnames(df_data_reg_g)

df_data_reg_g <- df_data_reg_g[, -c(167:189)]

df_data_reg_g <- df_data_reg_g[, -c(1:7)]
df_data_reg_g <- df_data_reg_g[, -c(2)]

df_filter_gen<-filter(df_data_reg_g, response > 16)

write.csv(df_filter_gen, file = "C:/Users/Krunal/Documents/Tinnitus/data/df_filter_gen.csv")

