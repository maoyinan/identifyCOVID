# find day specific cutoff that
# optimise sensitivity with overall minimum .95 specificity
# Set sensitivity*I(specificity>0.95) to be the objective function


# sensitivity by day
tb_sensitivity_day <- NULL
for(k in seq_along(illdays)){
  tb_sensitivity <- 
    data.frame(Cutoff=seq(0,1000)/1000, Sensitivity=NA, Specificity=NA) 
  ind <- which(y_hat_mat[,'illday_group']==illdays[k])
  x <- y_hat_mat$Predict
  y0 <- y_hat_mat$Actual
  w <- y_hat_mat$Weight
  
  xk <- x[ind]
  y0k <- y0[ind]
  
  for(i in seq_len(nrow(tb_sensitivity))){
    g <- tb_sensitivity[i,'Cutoff']
    yes_yes <- y0k==1 & xk>=g
    no_no <- y0k==0 & xk<=g
    
    # tb_sensitivity[i,'Sensitivity'] <- sum(yes_yes)/sum(y0k==1)
    # tb_sensitivity[i,'Specificity'] <- sum(no_no)/sum(y0k==0)
    tb_sensitivity[i,'Sensitivity'] <- weighted.mean(yes_yes[y0k==1],w = w[y0k==1])
    tb_sensitivity[i,'Specificity'] <- weighted.mean(no_no[y0k==0], w= w[y0k==0])
    
  } 
  tb_sensitivity_day <- rbind(tb_sensitivity_day, cbind(tb_sensitivity, Day=illdays[k]))
}

# maximise overall sensitivity with minimum specificity
sensitivity.calculate0 <- function(y_hat_mat, threshold_specificity){
  illdays <- sort(unique(y_hat_mat$illday_group))
  if(length(threshold_specificity)==1) threshold_specificity=rep(threshold_specificity,length(illdays))
  
  tb_threshold <- 
    bind_cols(Day=illdays, Threshold= threshold_specificity)
  
  y_hat_mat %>%
    as_tibble() %>% 
    select(Predict, Actual, Day=illday_group) %>% 
    left_join(tb_threshold, by='Day') %>% 
    mutate(Greater= ifelse(Predict>= Threshold,1,0)) %>% 
    summarise(
      Sensitivity= sum(Greater * Actual)/sum(Actual),
      Specificity= sum((1-Greater)*(1-Actual))/sum(1-Actual),
      PPV= sum(Greater * Actual)/sum(Greater),
      Fscore= 2*(PPV*Sensitivity)/(PPV+Sensitivity))
}

sensitivity.calculate <- function(y_hat_mat, threshold_specificity){
  illdays <- sort(unique(y_hat_mat$illday_group))
  if(length(threshold_specificity)==1) threshold_specificity=rep(threshold_specificity,length(illdays))
  
  tb_threshold <- 
    bind_cols(Day=illdays, Threshold= threshold_specificity)
  
  y_hat_mat %>%
    as_tibble() %>% 
    select(Predict, Actual, Day=illday_group, w=Weight) %>% 
    left_join(tb_threshold, by='Day') %>% 
    mutate(Greater= ifelse(Predict>= Threshold,1,0)) %>% 
    summarise(
      Sensitivity= weighted.mean((Greater * Actual)[Actual==1], w=w[Actual==1]),
      Specificity= weighted.mean(((1-Greater)*(1-Actual))[Actual==0], w=w[Actual==0]),
      PPV= weighted.mean((Greater * Actual)[Greater==1], w=w[Greater==1]),
      Fscore= 2*(PPV*Sensitivity)/(PPV+Sensitivity))
}


# # on each day set specificity>.95
# tb_sensitivity_day %>% 
#   group_by(Day) %>% 
#   filter(Specificity>=.95) %>% 
#   slice(1) %>% 
#   ungroup() %>% 
#   select(Cutoff) %>% unlist -> threshold_specificity
# 
# sensitivity.calculate(y_hat_mat, threshold_specificity)
# 

# bootstrap
objective.function <- function(sensitivity, specificity, th_specificity=.95){
  sensitivity*ifelse(specificity>=th_specificity, 1, 0)
}
optimise.sensitivity <- function(trainX, n_boot, cutoff_all, rg_cutoff, objective.function, th_specificity=.95){
  # # two groups
  # trainX <- trainX %>% 
  #   mutate(illday_group= ifelse(illday_group<5, '1-4','5+'))
 
  illdays <- sort(unique(trainX$illday_group))
  if(!is.matrix(rg_cutoff)){
    rg_cutoff <- matrix(rg_cutoff, nrow= length(illdays), ncol= 2, byrow=T)
  }
  
  cutoff_candidates <- 
    lapply(seq_len(nrow(rg_cutoff)), function(i){
      cutoff_all[between(cutoff_all, rg_cutoff[i,1], rg_cutoff[i,2])]
    })
  
  cutoff_mat <- 
    sapply(seq_along(illdays), function(i){
      sample(cutoff_candidates[[i]], 
             size = n_boot, replace = T)
    })
  
  objective <- apply(cutoff_mat, 1, function(th){
    temp <- sensitivity.calculate(trainX, th)
    objective.function(
      sensitivity= temp$Sensitivity, 
      specificity= temp$Specificity, 
      th_specificity= th_specificity)
  })
  ind <- which(objective==max(objective))[1]
  cutoff_mat[ind,]
}

cutoff_all <- seq(.001,.999, .001)
n_boot <- 1000

# find cutoffs combination for specificity threshold .95
optimise.sensitivity(y_hat_mat, n_boot, cutoff_all, c(0.5,1), objective.function, th_specificity=.95)
optimise.sensitivity(y_hat_mat, n_boot, cutoff_all, c(0.7,1), objective.function, th_specificity=.95)
optimise.sensitivity(y_hat_mat, n_boot, cutoff_all, c(0.85,1), objective.function, th_specificity=.95)
optimise.sensitivity(y_hat_mat, n_boot, cutoff_all, c(0.9,1), objective.function, th_specificity=.95)

rg_cutoff <- 
  matrix(c(
    .8,.84,
    .91,.95,
    .91,.95,
    .93,.97
  ), nrow= 4, ncol=2, byrow=T)

rg_cutoff <- 
  matrix(c(
    .8,.84,
    .91,.95,
    .91,.95,
    .93,.97
  ), nrow= 4, ncol=2, byrow=T)
optimise.sensitivity(y_hat_mat, n_boot, cutoff_all, rg_cutoff, objective.function, th_specificity=.95)
cutoff_M95 <- c(0.82, 0.93, 0.93, 0.95)
te <- sensitivity.calculate(y_hat_mat, th=cutoff_M95)
sens_M95 <- te$Sensitivity
spec_M95 <- te$Specificity

# loocv 
source('cvCutoff.R')
rg_cutoff <-  
  matrix(c(
    .77,.87,
    .88,.98,
    .88,.98,
    .9,.999
  ), nrow= 4, ncol=2, byrow=T)
temp1 <- cvCutoff(y_hat_mat, n_boot, cutoff_all, rg_cutoff, objective.function, th_specificity=.95, CViter=200)
apply(temp1$Cutoff,2, mean)
table(temp1$Actual, temp1$Predicted)
sum(temp1$Predicted * temp1$Actual)/sum(temp1$Actual)
sum((1-temp1$Predicted)*(1-temp1$Actual))/sum(1-temp1$Actual)

# find cutoffs combination for specificity threshold .9 -----------------------------------

optimise.sensitivity(y_hat_mat, n_boot, cutoff_all, c(0.5,1), objective.function, th_specificity=.9)

rg_cutoff <- 
  matrix(c(
    .72,.76,
    .74,.88,
    .68,.72,
    .92,.96
  ), nrow= 4, ncol=2, byrow=T)

optimise.sensitivity(y_hat_mat, n_boot, cutoff_all, rg_cutoff, objective.function, th_specificity=.9)
cutoff_M90 <- c(0.75, 0.76, 0.72, 0.95)

te <- sensitivity.calculate(y_hat_mat, th=cutoff_M90)
sens_M90 <- te$Sensitivity
spec_M90 <- te$Specificity

# loocv
rg_cutoff <- 
  matrix(c(
    .7,.8,
    .71,.81,
    .67,.77,
    .9,.99
  ), nrow= 4, ncol=2, byrow=T)
temp2 <- cvCutoff(y_hat_mat, n_boot, cutoff_all, rg_cutoff, objective.function, th_specificity=.9, CViter=200)
apply(temp2$Cutoff,2, mean)
table(temp2$Actual, temp2$Predicted)
sum(temp2$Predicted * temp2$Actual)/sum(temp2$Actual)
# .86, .78, .81
sum((1-temp2$Predicted)*(1-temp2$Actual))/sum(1-temp2$Actual)


# in sample
tb1 <- matrix(NA, nrow=4, ncol=4, dimnames=list(c('O95','M95','O90','M90'),c('sensitivity','specificity','PPV','Fscore')))
tb1[1,] <- sensitivity.calculate(y_hat_mat, th=cutoff_O95) %>% unlist
tb1[2,] <- sensitivity.calculate(y_hat_mat, th=cutoff_M95) %>% unlist
tb1[3,] <- sensitivity.calculate(y_hat_mat, th=cutoff_O90) %>% unlist
tb1[4,] <- sensitivity.calculate(y_hat_mat, th=cutoff_M90) %>% unlist

# loocv
y_hat_mat1 <- y_hat_mat
y_hat_mat1[,1] <- y1
tb2 <- matrix(NA, nrow=4, ncol=4, dimnames=list(c('O95','M95','O90','M90'),c('sensitivity','specificity','PPV','Fscore')))
tb2[1,] <- sensitivity.calculate(y_hat_mat1, th=cutoff_O95) %>% unlist
tb2[2,] <- sensitivity.calculate(y_hat_mat1, th=cutoff_M95) %>% unlist
tb2[3,] <- sensitivity.calculate(y_hat_mat1, th=cutoff_O90) %>% unlist
tb2[4,] <- sensitivity.calculate(y_hat_mat1, th=cutoff_M90) %>% unlist

# unweighted
# in sample
tb3 <- matrix(NA, nrow=4, ncol=4, dimnames=list(c('O95','M95','O90','M90'),c('sensitivity','specificity','PPV','Fscore')))
tb3[1,] <- sensitivity.calculate0(y_hat_mat, th=cutoff_O95) %>% unlist
tb3[2,] <- sensitivity.calculate0(y_hat_mat, th=cutoff_M95) %>% unlist
tb3[3,] <- sensitivity.calculate0(y_hat_mat, th=cutoff_O90) %>% unlist
tb3[4,] <- sensitivity.calculate0(y_hat_mat, th=cutoff_M90) %>% unlist

# loocv
y_hat_mat1 <- y_hat_mat
y_hat_mat1[,1] <- y1
tb4 <- matrix(NA, nrow=4, ncol=4, dimnames=list(c('O95','M95','O90','M90'),c('sensitivity','specificity','PPV','Fscore')))
tb4[1,] <- sensitivity.calculate0(y_hat_mat1, th=cutoff_O95) %>% unlist
tb4[2,] <- sensitivity.calculate0(y_hat_mat1, th=cutoff_M95) %>% unlist
tb4[3,] <- sensitivity.calculate0(y_hat_mat1, th=cutoff_O90) %>% unlist
tb4[4,] <- sensitivity.calculate0(y_hat_mat1, th=cutoff_M90) %>% unlist

tb1;tb2
