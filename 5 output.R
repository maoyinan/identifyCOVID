# run '4 optimise specificity.R' --------------------------------------------------

# predict function with cutoff
predict_covid <- function(testx, model_fits, cutoff){
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
  
  score <- c(logit_transpose(
    cbind(1,x1,x2)%*%coef_m
  ))
  illdays <- sort(unique(x_vars_interact2))
  if(length(cutoff)==1) cutoff <- rep(cutoff, length(illdays))
  
  data.frame(predict= score, cutoff= testx[,illdays]%*%cutoff) %>% 
    mutate(greater= ifelse(predict>= cutoff,1,0))
}
predict_covid(testx, model_fits, cutoff)
dump(c('cutoff_O95','cutoff_M95','cutoff_O90','cutoff_M90'), file='cutoff.R')
dump('predict_covid', file='predict_covid.R')

y_hat_mat$O95 <- predict_covid(as.matrix(dat0[,x_vars]),model_fits, cutoff=cutoff_O95)$greater
y_hat_mat$O90 <- predict_covid(as.matrix(dat0[,x_vars]),model_fits, cutoff=cutoff_O90)$greater
y_hat_mat$M95 <- predict_covid(as.matrix(dat0[,x_vars]),model_fits, cutoff=cutoff_M95)$greater
y_hat_mat$M90 <- predict_covid(as.matrix(dat0[,x_vars]),model_fits, cutoff=cutoff_M90)$greater
write.csv(y_hat_mat[,-3],'dataPredicted.csv')

write.csv(cbind(y_hat_mat[,-3], Predict_loocv= y1), file= 'dataPredictedLOO.csv')
