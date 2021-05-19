mod <-
    logistic_reg(mixture = 1/3) %>%
    set_engine("glmnet", nlambda = 10)
