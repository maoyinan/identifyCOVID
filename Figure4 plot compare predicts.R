
# plot nicer for actual vs logit: jitter with kernel density
jitter.kernel <- function(X, w=1){
  X_density <- density(X)
  
  Y <- numeric(length(X))
  for(i in seq_along(X)){
    Y[i] <- sum(dnorm(X[i],X,X_density$bw))
  }
  Y <- Y/max(Y)
  
  y <- runif(length(X),-Y*w, Y*w)
  y  
}

x <- y_hat_mat$Predict
y0 <- y_hat_mat$Actual
w <- .5
all_cases <- c(0,1)
ind <- which(y0==0)
y_case0 <- jitter.kernel(x[ind],w)
y_case1 <- jitter.kernel(x[-ind],w)+1


# x <-  unlist(temp$Predicted)
# y0 <- unlist(temp$Actual)
# w <- .5
# all_cases <- c(0,1)
# ind <- which(y0==0)
# y_case0 <- jitter.kernel(x[ind],w)
# y_case1 <- jitter.kernel(x[-ind],w)+1

x_group_breaks <-  c(0,.2,.4,.6,.8,.9,.95,1)
x_group <- cut(x, breaks = x_group_breaks, include.lowest = T)
x_group_mat <- data.frame(left=x_group_breaks[-length(x_group_breaks)], 
                          right=x_group_breaks[-1], 
                          predictGroup= levels(x_group), stringsAsFactors = F)

tb_perc <- 
  y_hat_mat %>% 
  cbind(data.frame(predictGroup=x_group, stringsAsFactors = F))  %>% 
  group_by(predictGroup) %>% 
  summarise(Perc_case= mean(Actual)) %>% 
  right_join(x_group_mat) %>% 
  mutate(Perc_case= Perc_case*100)

# sensitivity overall
tb_sensitivity <- 
  data.frame(Cutoff=seq(0,1000)/1000, Sensitivity=NA, Specificity=NA, PPV=NA, NPV=NA) 
ind <- which(y0==0)
for(i in seq_len(nrow(tb_sensitivity))){
  g <- tb_sensitivity[i,'Cutoff']
  tb_sensitivity[i,'Sensitivity'] <- mean(x[-ind]>=g)
  tb_sensitivity[i,'Specificity'] <- mean(x[ind]<=g)
  tb_sensitivity[i,'PPV'] <- mean(y0[x>=g])
  tb_sensitivity[i,'NPV'] <- mean((1-y0)[x<=g]) 
} 
# tb_sensitivity %>% 
#   filter(Cutoff%in%seq(.1,.95,.05)) %>% 
#   round(.,2)
tb_sensitivity[which(between(tb_sensitivity$Specificity,.949,.957)),] %>% round(3)

# plot
tiff('fig4.tif',
     height=15,width=10,units='cm',res=450,pointsize=9)
{
  pushViewport(plotViewport(c(4,8,1,1)))
  
  # labels on y axis
  # grid.text('Figure 2: comparison of predicted risk and response', unit(-5,'lines'), unit(-5,'lines'), just='left')
  
  
  pushViewport(viewport(layout=grid.layout(nrow=3,ncol=1)))
  
  # jitter plot
  {
    x_rg <- c(0,1)
    y_rg <- c(-w,1+w)
    
    pushViewport(viewport(layout.pos.row=2, layout.pos.col=1))
    pushViewport(plotViewport(c(0,0,0,0), xscale= x_rg, yscale= y_rg))
    
    grid.rect()
    grid.points(x[ind], y_case0, pch = '.')
    # gp=gpar(col= roc_colors[sapply(y_hat_mat$Day[ind], function(d)which(illdays==d))]))
    grid.points(x[-ind], y_case1, pch = '.')
    # gp=gpar(col= roc_colors[sapply(y_hat_mat$Day[-ind], function(d)which(illdays==d))]))
    grid.yaxis(all_cases, label = c('Control','COVID-19'))
    grid.text('(b)', unit(1,'lines'), unit(1,'npc')-unit(1,'lines'))
    
    popViewport()
    popViewport()
  }
  
  
  # bar plot
  {
    x_rg <- c(0,1)
    x_axis <- x_group_breaks
    y_rg <- c(0,100)
    y_axis <- pretty(y_rg)
    
    pushViewport(viewport(layout.pos.row=1, layout.pos.col=1))
    pushViewport(plotViewport(c(0,0,0,0), xscale= x_rg, yscale= y_rg))
    # labels on y axis
    grid.text('Percent of cases (%)', 
              gp=gpar(col=wes_palette(n=4, name="GrandBudapest2")[4]),
              unit(-3,'lines'), .5, rot = 90)
    # axis
    grid.yaxis(y_axis, y_axis)
    
    
    # plot bars
    # grid.rect(gp=gpar(fill=wes_palette(n=4, name="GrandBudapest2")[2]))
    grid.rect(x= tb_perc %>% mutate(x=(left+right)/2) %>% .$x, 
              y= numeric(nrow(tb_perc)),
              width=  tb_perc %>% mutate(x=right-left) %>% .$x, 
              height=tb_perc$Perc_case, 
              just='bottom', 
              default.units='native',
              gp=gpar(fill=wes_palette(n=4, name="GrandBudapest2")[4], alpha=.5, col=NA))  
    
    grid.text('(a)', unit(1,'lines'), unit(1,'npc')-unit(1,'lines'))
    
    popViewport()
    popViewport()
  } 
  # sensitivity plot
  {
    x_rg <- c(0,1)
    x_axis <- x_group_breaks
    y_rg <- c(0,1)
    y_axis <- pretty(y_rg)
    # y_axis <- y_axis[-length(y_axis)]
    
    pushViewport(viewport(layout.pos.row=3, layout.pos.col=1))
    pushViewport(plotViewport(c(0,0,0,0), xscale= x_rg, yscale= y_rg))
    
    grid.rect()
    # labels on y axis
    grid.text('Sensitivity', unit(-6,'lines'), .5, rot = 90, gp=gpar(col=roc_colors[1]))
    grid.text('Specificity', unit(-5,'lines'), .5, rot = 90, gp=gpar(col=roc_colors[2]))
    grid.text('PPV', unit(-4,'lines'), .5, rot = 90, gp=gpar(col=roc_colors[3]))
    grid.text('NPV', unit(-3,'lines'), .5, rot = 90, gp=gpar(col=roc_colors[4]))
    grid.yaxis(y_axis, y_axis)
    
    grid.lines(tb_sensitivity$Cutoff, tb_sensitivity$Sensitivity, gp=gpar(col=roc_colors[1]))
    grid.lines(tb_sensitivity$Cutoff, tb_sensitivity$Specificity, gp=gpar(col=roc_colors[2]))
    grid.lines(tb_sensitivity$Cutoff, tb_sensitivity$PPV, gp=gpar(col=roc_colors[3]))
    grid.lines(tb_sensitivity$Cutoff, tb_sensitivity$NPV, gp=gpar(col=roc_colors[4]))
    
    grid.text('(c)', unit(1,'lines'), unit(1,'npc')-unit(1,'lines'))
    grid.xaxis(x_axis, x_axis)
    # label x axis
    grid.text('Predicted score', 
              .5, unit(-3,'lines'))
    
    popViewport()
    popViewport()
    
  }
  # grid lines vertical
  for(i in seq_along(x_group_breaks)){
    grid.lines(unit(x_group_breaks[i], 'native') , c(0,1),
               gp=gpar(col='grey', lty=2))
  }
  
  # outer frame
  grid.lines(0, c(0,1))
  grid.lines(1, c(0,1))
  grid.lines(c(0,1), 0)
  grid.lines(c(0,1), 1)
  
  popViewport()
  popViewport()
  
}
dev.off()
