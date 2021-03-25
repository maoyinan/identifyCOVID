
logit_transpose <- function(y){
  exp(y)/(1+exp(y))
}

# prediction output for shiny  -----------------------------------------------------

##### for shiny app

model_fits <- list(
  coef_mean=coef_mean)

# function
predict_covid <- function(testx, model_fits){
  logit_transpose <- function(y){
    exp(y)/(1+exp(y))
  }
  coef_m <- model_fits$coef_mean
  
  ind2 <- grep('[.]',names(coef_m))
  iterms <- names(coef_m)[ind2]
  x_vars_interact1 <- sub("\\..*", "", iterms)
  x_vars_interact2 <- sub(".*\\.", "", iterms)
  x2 <- testx[,x_vars_interact1] * testx[, x_vars_interact2]
  
  ind1 <- setdiff(2:length(coef_m), ind2)
  x1 <- testx[,names(coef_m)[ind1]]
  
  c(logit_transpose(
    cbind(1,x1,x2)%*%coef_m
  ))
}

# test prediction function agree with GAM fit
testx <- dat0 %>% select_(.dots=x_vars) %>% as.matrix %>% .[1:2,]
testx
predict_covid(testx,model_fits)
predict(fit, newdata=as.data.frame(X)[1:2,], type='response')

# dump('model_fits', file='model_fits.R')
# dump('predict_covid', file='predict_covid.R')
# dump(c('testx'), file='testx.R')


# predict everything
y_hat_mat <- matrix(NA, nrow(dat0), 3, dimnames=list(c(),c('Predict','Actual','Predict1')))
y_hat_mat[,1] <- predict(fit, newdata=as.data.frame(X), type= 'response')
y_hat_mat[,2] <- dat0$case_new

y_hat_mat <- 
  y_hat_mat %>% 
  as_tibble() %>% 
  bind_cols(dat0 %>% select_(.dots=c('illday_group','severe', x_vars)))

dim(y_hat_mat)
y_hat_mat$Weight= dat0$Weight
