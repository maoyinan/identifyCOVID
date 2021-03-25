
# coefficient plot --------------------------------------------------------

# source(sprintf('model_fits_%s.R',model_type))
names(coef_mean)
vars_select <- names(coef_mean)[-1]
x_vars_interact_with
x_vars_interact_terms_tb
x_labs_linear <- c(
  'feverishness', 
  'cough',
  'shortness of breath',
  'runny nose', 
  'sore throat', 
  'diarrhea',
  'muscle ache',
  'headache',
  'abdominal pain',
  'nausea and/or vomit',
  '37.5-37.9\u00B0C',
  '\u226538.0\u00B0C')
d_labs <- c("day 1-2", "3-4", "5-7", "8+")
names(d_labs) <- c(2,3,5,8)

x_vars_interact_terms_tb %>% 
  bind_cols(Label=rep(x_labs_linear, 4)) %>% 
  mutate(
    Variable= gsub('[*]','.',term), 
    Group=d_labs[gsub('illday','',Var2)]) %>% 
  group_by(Label) %>% 
  mutate(row= order(Var2)) %>% 
  ungroup %>% 
  select(-(1:3)) -> tb

# data.frame(
#   Label= 'illness day',
#   Variable= x_vars_interact[-1], 
#   Group= c("day 3-4", "5-7", "8+"),
#   row= 1:3) %>% 
# bind_rows(
#   x_vars_interact_terms_tb %>% 
#     bind_cols(Label=rep(x_labs_linear, 4)) %>% 
#     mutate(
#       Variable= gsub('[*]','.',term), 
#       Group=d_labs[gsub('illday','',Var2)]) %>% 
#     group_by(Label) %>% 
#     mutate(row= order(Var2)) %>% 
#     ungroup %>% 
#     select(-(1:3))) -> tb

# linear coefficients CI
tb_coef <- 
  tb %>% 
  left_join(data.frame(Variable=names(coef_mean),
                       Mean=coef_mean,
                       SE=coef_se)) %>% 
  mutate(CIl= Mean -1.96*SE, 
         CIh= Mean +1.96*SE,
         Mean=ifelse(is.na(Mean),0,Mean),
         Variable= paste(Label, Group)
  ) 
tb_coef %>% 
  mutate_at(.vars=c('Mean','CIl','CIh'), .funs = exp) #%>% View()

tb_coef %>% 
  mutate(Significant= CIh<0|CIl>0) -> tb0
  
tb0 %>% 
  group_by(Label) %>% 
  slice(1) %>% 
  ungroup %>% 
  mutate(Variable=Label,
         row=0) -> tb0_label
tb0 %>% 
  bind_rows(tb0_label) %>% 
  arrange(Label, row) -> tb0

tb_coef <- 
  # tb0 %>% filter(Label=='illness day') %>% 
  tb0 %>% filter(Label=='37.5-37.9?C') %>% 
  bind_rows(tb0 %>% filter(!Label%in%c('37.5-37.9?C','illness day'))) %>% 
  group_by(Label) %>% 
  filter(sum(!is.na(Significant))>0)

scale.exp <- function(rg){
  upper <- exp(rg[2])
  lower <- exp(rg[1])
  n1 <- round(log(upper,base = 2),0)
  n2 <- round(log(lower,base = 2),0)
  
  ax <- sort(unique(c(
    2^seq(n2,n1, by=round((n1-n2)/6,0)),
    1)))
  ax <- fractions(ax) 
  
  ax[between(ax,lower,upper)]
}

n_rows1 <- 30
n_rows2 <- nrow(tb_coef)-n_rows1
x_rg <- c(-2.78, 2.78)
tb <- tb_coef %>% 
  mutate(CIl=ifelse(CIl<x_rg[1],x_rg[1],CIl),
         CIh=ifelse(CIh>x_rg[2],x_rg[2],CIh))
tb_left <- tb[1:n_rows1,]
tb_right <- tb[(1+n_rows1):nrow(tb),]
pretty(exp(x_rg))
x_axis <- scale.exp(x_rg)


# plot 
tiff('fig2.tif',
     height=18,width=18,units='cm',res=450,pointsize=9)
{
  pushViewport(plotViewport(c(0,0,0,0)))
  pushViewport(viewport(layout=grid.layout(nrow=1,ncol=2)))
  
  for(j in 1:2){
    if(j==1) tb <- tb_left
    else tb <- tb_right
    
    pushViewport(viewport(layout.pos.row=1, layout.pos.col=j))
    pushViewport(plotViewport(c(.2,3.5,4,.5), xscale= x_rg))
    # axis
    grid.xaxis(log(x_axis), x_axis, main=FALSE)
    grid.text('Adjusted odds ratio',  0.5, unit(1,'npc')+unit(3,'lines'))
    
    pushViewport(viewport(layout=grid.layout(nrow=max(n_rows1, n_rows2),ncol=1)))
    
    
    for(i in seq_len(nrow(tb))){
      vari <- tb[i,'Variable']
      
      tbi <- tb[i,]
      
      pushViewport(viewport(layout.pos.row=i, layout.pos.col=1))
      if(tbi$row==0)
        grid.rect(gp=gpar(fill='black', col=NA))
      else if(tbi$row%%2==1)
        grid.rect(gp=gpar(fill='grey',alpha=0.2, col=NA))
      
      pushViewport(plotViewport(c(0,0,0,0), xscale= x_rg, yscale= c(0, 1)))
      
      # labels on y axis
      if(tbi$row==0) 
        grid.text(tbi$Label, unit(.5,'lines'), unit(.5,'npc'), just='left', gp=gpar(col='white'))
      else {
        grid.text(tbi$Group,  unit(-.5,'lines'), unit(.5,'npc'), just='right')
        # confidence interval
        coli <- ifelse(tbi$Significant%in%c(T,NA), 'black','grey')
        grid.lines(x= unlist(tbi[, c('CIl','CIh')]), 
                   y= c(.5,.5),
                   default.units = 'native',
                   gp = gpar(col=coli))
        
        # mean dots
        grid.points(tbi$Mean, .5, pch=20, gp= gpar(col= coli, cex=1))#, col=roc_colors[seq_len(nrow(tb))]))
        
      }
      # middle line
      grid.lines(unit(0, 'native') , c(0,1),
                 gp=gpar(col='black'))
      
      # outer frame
      grid.lines(0, c(0,1))
      grid.lines(1, c(0,1))
      if(i==nrow(tb)) grid.lines(c(0,1), 0)
      if(i==1) grid.lines(c(0,1), 1)
      
      
      popViewport()
      popViewport()
    }
    popViewport()
    
    popViewport()
    popViewport()
  }
  popViewport()
  popViewport()
}
dev.off()

