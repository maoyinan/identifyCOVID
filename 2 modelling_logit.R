source('cvLOO.R')
options(digits=6)

# loop through illness days -----------------------------------------------
# control flags
do_coefficients <- T
do_roc <- T
illdays <- sort(as.numeric(na.omit(unique(dat0$illday_group))))

roc_total <- 10001

x <- as.data.frame(X)
y <- dat0 %>% select_(.dots=y_var) %>% unlist
w <- dat0$Weight

dim(x)

vars_select <- x_vars_select
# vars_select <- c(x_vars_select, paste('sob10',x_vars_interact, sep='.'))
# gam model
if(length(vars_select)){
  model_string = 
    sprintf('glm(y ~ %s,
          family = binomial(link = logit),
            weights= .$w, 
            data = .)', 
            paste(vars_select, collapse=' + '))
} 

f_wrap <- paste0('function(.) {',model_string,'}') %>% parse(text=.) %>% eval
f_wrap

summary(data.frame(y=y, x, w) %>% f_wrap())
# x_var rearranged
if(do_coefficients){
  fit <- data.frame(y=y, x, w) %>%
    f_wrap()
  summary(fit)
  coef_mean <- coef(fit)
  coef_se <- sqrt(diag(vcov(fit)))
  coef_cov <- vcov(fit)
}
if(do_roc==T){
  sensitivityW= specificityW = sensitivity0 = specificity0 = sensitivity = specificity <- numeric(roc_total)  
  
  temp <- cvLOO(x,y,f_wrap,illdays)
  # prepare ROC
  y1 <- unlist(temp$Predicted)
  y0 <- unlist(temp$Actual)
  yd <- unlist(temp$Day)
  y1k <- y1
  y0k <- y0
  y1l <- predict(fit,type='response')
  y0l <- dat0$case_new
  
  for(i in 1:roc_total){ # i/roc_total as cutoff to determine predict case or control
    # for(k in seq_along(illdays)){
    #   ind <- which(yday==illdays[k])
    #   y1k <- y1[ind]
    #   y0k <- y0[ind]
    #   yes_yes <- y0k==1 & y1k>=((i-1)/(roc_total-1))
    #   no_no <- y0k==0 & y1k<=(i-1)/(roc_total-1)
    #   sensitivity[i,k] <- sum(yes_yes)/sum(y0k==1) # true predict out of cases
    #   specificity[i,k] <- sum(no_no)/sum(y0k==0) # true predict out of controls
    #   
    # }
    cutoff <- ((i-1)/(roc_total-1))
    # LOOCV
    yes_yes <- y0k==1 & y1k>=cutoff
    no_no <- y0k==0 & y1k<=cutoff
    sensitivityW[i] <- sum(yes_yes)/sum(y0k==1) # true predict out of cases
    specificityW[i] <- sum(no_no)/sum(y0k==0) # true predict out of controls
    sensitivity[i] <- weighted.mean(yes_yes[y0k==1],w = w[y0k==1])
    specificity[i] <- weighted.mean(no_no[y0k==0], w= w[y0k==0])
    
    # original
    yes_yes <- y0l==1 & y1l>=cutoff
    no_no <- y0l==0 & y1l<=cutoff
    # sensitivity0[i] <- sum(yes_yes)/sum(y0l==1) # true predict out of cases
    # specificity0[i] <- sum(no_no)/sum(y0l==0) # true predict out of controls
    sensitivity0[i] <- weighted.mean(yes_yes[y0l==1],w = w[y0l==1])
    specificity0[i] <- weighted.mean(no_no[y0l==0], w= w[y0l==0])
  }
  data.frame(cutoff =((1:roc_total-1)/(roc_total-1)),sensitivity,specificity, sensitivity0,specificity0) %>% View
  
  # one cutoff for all
  ind1 <- which(specificity0>=.95)[1]
  cutoff_O95 <- (ind1-1)/(roc_total-1)
  sens_O95 <- sensitivity0[ind1]
  cutoff_O95;sens_O95;round(sensitivity[ind1], 2)
  spec_O95 <- specificity0[ind1]
  spec_O95;round(specificity[ind1], 2)
  sensitivityW[ind1];specificityW[ind1]
  sensitivity[ind1];specificity[ind1]
  
  ind2 <- which(specificity0>=.90)[1]
  cutoff_O90 <- (ind2-1)/(roc_total-1)
  sens_O90 <- round(sensitivity0[ind2], 2)
  cutoff_O90;sens_O90;round(sensitivity[ind2], 2)
  spec_O90 <- round(specificity0[ind2], 2)
  spec_O90;round(specificity[ind2], 2)
  sensitivityW[ind2];specificityW[ind2]
  sensitivity[ind2];specificity[ind2]
  
  # # bootstrap for sensitivity CI 
  # sens_boot95=spec_boot95=sens_boot90=spec_boot90 <- numeric(10000)
  # for(j in seq(10000)){
  #   idx <- sample(length(y1),1000, replace = T)
  #   
  #   y1k <- y1[idx]
  #   y0k <- y0[idx]
  #   yes_yes <- y0k==1 & y1k>=cutoff_O95
  #   no_no <- y0k==0 & y1k<=cutoff_O95
  #   sens_boot95[j] <- sum(yes_yes)/sum(y0k==1)
  #   spec_boot95[j] <- sum(no_no)/sum(y0k==0) 
  #   yes_yes <- y0k==1 & y1k>=cutoff_O90
  #   no_no <- y0k==0 & y1k<=cutoff_O90
  #   sens_boot90[j] <- sum(yes_yes)/sum(y0k==1)
  #   spec_boot90[j] <- sum(no_no)/sum(y0k==0) 
  # }
  # hist(sens_boot95)
  # hist(spec_boot95)
  # hist(sens_boot90)
  # hist(spec_boot90)
  # quantile(sens_boot95,probs = c(0.05,0.95))
  # quantile(spec_boot95,probs = c(0.05,0.95))
  # quantile(sens_boot90,probs = c(0.05,0.95))
  # quantile(spec_boot90,probs = c(0.05,0.95))
}
