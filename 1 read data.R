library(tidyverse)
library(zoo)
library(wesanderson)
library(grid)
library(MASS)
library(mgcv)
select <- dplyr::select
complete <- tidyr::complete
library(xtable)
library(broom)

weight_method <- c('Equal','Decreasing','No')[2]

dat <- read.csv('../data for alex protected v9/data-Table 1.csv') 

dat %>% 
  mutate(
    fevered= as.Date(fevered, '%m/%d/%y'),
    obsdate= as.Date(obsdate, '%m/%d/%y'),
    after_fever= obsdate-fevered>=0) %>% 
  group_by(record_id) %>% 
  mutate(
    temp_merge= ifelse(after_fever & is.na(temp_merge), 36.9, temp_merge)
  ) %>%
  ungroup %>% 
  mutate(
    temp_merge= ifelse(temp_merge>42, NA, temp_merge),
    temperature= scale(temp_merge),
    sex = ifelse(is.na(sex), 0.5, sex),
    sex = ifelse(sex==2, 0, sex),
    fever10= ifelse(is.na(fever10), 0, fever10),
    cough10= ifelse(is.na(cough10), 0, cough10),
    # sputum10= ifelse(is.na(sputum10), 0, sputum10),
    # drycough= ifelse(sputum10==0,1,0),
    sob10= ifelse(is.na(sob10), 0, sob10),
    runny10= ifelse(is.na(runny10), 0, runny10),
    sore10= ifelse(is.na(sore10), 0, sore10),
    diarr10= ifelse(is.na(diarr10), 0, diarr10),
    abdo10= ifelse(is.na(abdo10), 0, abdo10),
    muscle10= ifelse(is.na(muscle10), 0, muscle10),
    head10= ifelse(is.na(head10), 0, head10),
    vomit10= ifelse(is.na(vomit10), 0, vomit10),
    age= ifelse(is.na(age), 44, age),
    age= ifelse(age>75, 75, age),
    illnessday= ifelse(source==1, duration, illnessday),
    severe= ifelse(is.na(severe), 0, severe)
  )  %>% 
  mutate(
    age_group= cut(age, breaks = c(16,29,39,49,59,99), include.lowest = T),
    # temp_group= as.numeric(cut(temp_merge, breaks = c(seq(35.5,38.5, by=1.5),40.5), include.lowest = T)),
    temp_group= cut(temp_merge, breaks = c(35.5,37.4,37.9,40.5), include.lowest = T),
    age16to29= as.numeric(age_group)==1,
    age30to39= as.numeric(age_group)==2,
    age40to49= as.numeric(age_group)==3,
    age50to59= as.numeric(age_group)==4,
    age60to99= as.numeric(age_group)==5,
    # < 37.5, 37.5-37.7, 37.8 - 37.9, >=38
    temp355to374= as.numeric(temp_group)==1,
    temp375to379= as.numeric(temp_group)==2,
    temp380to405= as.numeric(temp_group)==3,
    # temp355to378= temp_group==1,
    # temp379to405= temp_group==2,
    illday2= ifelse(illnessday<3,1,0),
    illday3= ifelse(between(illnessday,3,4),1,0),
    illday5= ifelse(between(illnessday, 5,7),1,0),
    illday8= ifelse(illnessday>=8,1,0),
    illday_group= illday2*2+illday3*3+illday5*5+illday8*8,
    consult_date_merge= ifelse(case_new==0, 
                               as.character(as.Date(as.character(consult_date), format='%m/%d/%y')),
                               as.character(obsdate)),
    consult_date_group= cut(as.Date(consult_date_merge), 'months', labels= c('Dec 2019', '23~31 Jan 2020', 'Feb 2020', 'Mar 2020', '1~7 Apr 2020'))
  ) %>% 
  mutate(flag_include= 
           age>=16&
           !is.na(illnessday)&
           # !(source==1&suspected_infectious==2), 
           !is.na(temp_merge)&
           !(source>1 & presentday>15)
           # (source==3&(presentday==illnessday))|source==1,
           ) -> dat

# write.csv(dat, file='data_v9_cleaned.csv')
dat0 <- dat %>% 
  filter(flag_include,
         (source==3&(presentday==illnessday))|source==1)

vars_symptoms <- c("fever10", "cough10", "sob10", "runny10", "sore10", "diarr10", 
                   "muscle10", "head10", "abdo10", "vomit10")
dat$nSymptom <- apply(dat[,vars_symptoms,],1,sum)

dat0 <- dat %>% filter(flag_include, nSymptom>0)
dat0 %>% group_by(case_new, consult_date_group) %>% summarise(min(consult_date_merge), max(consult_date_merge))
dat0 %>% count(case_new)
#
# weighting ---------------------------------------------------------------


if(weight_method=='Equal'){
  # calculate weights: equal weights for days
  dat0 %>% 
    filter(!is.na(illday_group)) %>% 
    count(illday_group, case_new) %>% 
    bind_cols(n_days= rep(c(3,2,2,8), each=2)) %>% 
    filter(case_new==1)  %>% 
    mutate(
      Weight= sum(n)/n*n_days/sum(n_days))-> tb_weight
  dat0 <- 
    dat0 %>% 
    left_join(
      tb_weight %>% select(illday_group, case_new, Weight), 
      by=c('illday_group','case_new')) %>% 
    mutate(Weight= ifelse(is.na(Weight), 1, Weight))
  
} else if(weight_method=='Decreasing'){
  # weight p= 1-t/12
  dat0 %>% 
    filter(!is.na(illnessday)) %>% 
    count(illnessday, case_new) %>% 
    filter(case_new==1)  %>% 
    mutate(p= 1-illnessday/15) %>% 
    mutate(
      Weight= sum(n)/n*p/sum(p))-> tb_weight
  dat0 <- 
    dat0 %>% 
    left_join(
      tb_weight %>% select(illnessday, case_new, Weight), 
      by=c('illnessday','case_new')) %>% 
    mutate(Weight= ifelse(is.na(Weight), 1, Weight))
  
} else if (weight_method=='No'){
  dat0 <- 
    dat0 %>% 
    mutate(Weight=1)
}

# write.csv(dat0, file='data_cleaned.csv')


# leave out demographics -----------------------------------------------------------

y_var <- 'case_new'
x_vars_linear0 <- c("fever10", "cough10", "sob10", "runny10", "sore10", "diarr10", 
                    "muscle10", "head10", "abdo10", "vomit10", 
                    "temp375to379", "temp380to405")
x_vars_interact <- c( "illday2", "illday3", "illday5", "illday8")
x_vars_interact_with <- setdiff(x_vars_linear0,x_vars_interact)
x_vars <- c(x_vars_linear0, x_vars_interact)

expand.grid(x_vars_interact_with,x_vars_interact,stringsAsFactors = F) %>% 
  as_tibble() %>% 
  rowwise() %>% 
  mutate(term= sprintf('%s*%s',Var1, Var2)) -> x_vars_interact_terms_tb
x_vars_interact_terms0 <- x_vars_interact_terms_tb$term

x <- dat0 %>% select_(.dots=x_vars)
y <- dat0 %>% select_(.dots=y_var) %>% unlist
w <- dat0$Weight

dim(x)

X <- model.matrix(as.formula(
  sprintf(' ~ %s + %s',
          paste(x_vars,collapse='+'),
          paste(x_vars_interact_terms0,collapse='+')
  )), data=dat0)[,-1]
colnames(X) <- gsub('TRUE','',colnames(X))
x_vars_interact_terms0 <- gsub('[*]','.',x_vars_interact_terms0)
colnames(X)[(ncol(X)-length(x_vars_interact_terms0)+1):ncol(X)] <- 
  x_vars_interact_terms0


fit2 <- glmnet(x=X[,c(x_vars_interact_terms0)], y=y, weights=w,family= 'binomial')
summary(fit2)
set.seed(2)
fit3 <- cv.glmnet(x=X[,c(x_vars_interact_terms0)], y=y, weights=w,family= 'binomial')
plot(fit3)
te <- coef(fit3, s= 'lambda.1se')
x_vars_select <- setdiff(rownames(te)[as.numeric(te)!=0], '(Intercept)')
x_vars_interact_terms <- setdiff(x_vars_select, x_vars_interact)
x_vars_linear <- intersect(x_vars_linear0, gsub('[.].*$','', x_vars_interact_terms))
x_vars <- c(x_vars_linear, x_vars_interact)

