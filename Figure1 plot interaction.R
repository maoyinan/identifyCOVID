
vars_select <- setdiff(x_vars_linear0, c('sex','temp355to374'))
x_labels <- c(
  '1-2', 
  '3-4', 
  '5-7', 
  '8+')
y_labels <-  paste('%', c(
  # 'male',
  'feverishness', 
  'cough',
  'shortness of breath',
  'runny nose', 
  'sore throat', 
  'diarrhoea', 
  'muscle ache',
  'headache',
  'abdominal pain',
  'nausea and/or vomit',
  '\u2265 37.5\u00B0C', # group with higher temperatures
  '\u2265 38.0\u00B0C'
))

i=5
dat0 %>% 
  group_by(case_new,illday_group) %>% 
  count_(vars= vars_select[i]) %>% 
  ungroup %>% 
  rename(var= vars_select[i]) %>% 
  right_join(data.frame(expand.grid(case_new=c(0,1), illday_group= sort(unique(dat0$illday_group)), var=c(FALSE,TRUE)))) %>% 
  group_by(case_new, illday_group) %>%
  mutate(freq= round(n/sum(n)*100,0),
         freq= ifelse(is.na(freq), 0, freq)) %>% 
  ungroup %>% 
  select(-n) %>% 
  filter(var==1) %>% 
  arrange(illday_group, case_new) %>% 
  group_by(case_new) %>% 
  mutate(row= (1:n())*3) %>% 
  ungroup %>% 
  mutate(row= row -3.5 + 1:2) -> tb

p_rows <- 3
p_cols <- 4
# bar plot
tiff('fig1.tif',
     height=16,width=20,units='cm',res=450,pointsize=9)
{
  x_axis <- seq(1, max(tb$row)-.5, by=3)
  x_rg <- range(tb$row)+c(-0.5,0.5)
  
  pushViewport(plotViewport(c(3,0,1,1)))
  pushViewport(viewport(layout=grid.layout(nrow=p_rows,ncol=p_cols)))
  
  for(i in seq_along(vars_select)){
    
    if(vars_select[i]=="temp375to379")
      dat0 %>% 
      group_by(case_new,illday_group) %>% 
      mutate(temp375to379= temp380to405 | temp375to379) %>% 
      count_(vars= vars_select[i]) -> tb
    else
      dat0 %>% 
      group_by(case_new,illday_group) %>% 
      count_(vars= vars_select[i]) -> tb
    tb %>% 
      ungroup %>% 
      rename(var= vars_select[i]) %>% 
      right_join(data.frame(expand.grid(case_new=c(0,1), illday_group= sort(unique(dat0$illday_group)), var=c(FALSE,TRUE)))) %>% 
      group_by(case_new, illday_group) %>%
      mutate(freq= round(n/sum(n)*100,0),
             freq= ifelse(is.na(freq), 0, freq)) %>% 
      ungroup %>% 
      select(-n) %>% 
      filter(var==1) %>% 
      arrange(illday_group, case_new) %>% 
      group_by(case_new) %>% 
      mutate(row= (1:n())*3) %>% 
      ungroup %>% 
      mutate(row= row -3.5 + 1:2) -> tb
    
    y_rg <- c(0, max(tb$freq))
    y_axis <- pretty(y_rg)[between(pretty(y_rg), y_rg[1],y_rg[2])]
    
    if(vars_select[i]%in%c('fever10','cough10', "sob10", "runny10", "sore10",'muscle10','diarr10','vomit10','abdo10', "head10", "temp375to379", "temp380to405")){
      y_rg[2] <- 1.2*y_rg[2]
    }
    else{
      y_rg[2] <- 1.02*y_rg[2]
    }
    row <- (i+p_cols-1)%/%p_cols
    col <- ifelse(i%%p_cols==0,p_cols,i%%p_cols)
    pushViewport(viewport(layout.pos.row=row, layout.pos.col=col))
    pushViewport(plotViewport(c(2.5,2.5,0,0), xscale= x_rg, yscale= y_rg))
    
    grid.rect()
    # labels on y axis
    grid.text(y_labels[i],
              .5, unit(1,'npc') + unit(-1,'lines'))
    
    # axis
    grid.xaxis(x_axis, x_labels, main = T)
    grid.yaxis(y_axis, y_axis)
    
    
    # plot bars
    grid.rect(x= tb$row, 
              y= 0,
              width=  1, 
              height= tb$freq, 
              just='bottom', 
              default.units='native',
              gp=gpar(fill=wes_palette(n=4, name="GrandBudapest2")[4-tb$case_new], alpha=.5, col=NA))  
 
    # outer frame
    grid.lines(0, unit(c(0,1), 'npc'))
    grid.lines(1, unit(c(0,1), 'npc'))
    grid.lines(c(0,1), 0)
    grid.lines(c(0,1), unit(1,'npc'))
    
    if(row==p_rows|col==p_cols){
      grid.text(label = 'Days of illness',
                x= unit(0.5,'npc'),
                y= unit(-3,'lines'))  
      
    }
    
    # if(i == length(vars_symptoms)){
    #   
    # }
    popViewport()
    popViewport()
  }
  popViewport()
  popViewport()
  
  pushViewport(viewport(y=unit(1,'lines')))
  # legend
  grid.rect(x= unit(0.5,'npc')+unit(-5,'lines'),
            # y= unit(1,'npc') + unit(-2.5,'lines'),
            width= unit(.5,'lines'),
            height= unit(.5,'lines'),
            gp=gpar(fill=wes_palette(n=4, name="GrandBudapest2")[4], alpha=.5, col=NA))  
  grid.text(label = 'Control',
            x= unit(0.5,'npc')+unit(-4,'lines'),
            # y= unit(1,'npc') + unit(-2.5,'lines'),
            just='left')  
  grid.rect(width= unit(.5,'lines'),
            height= unit(.5,'lines'),
            gp=gpar(fill=wes_palette(n=4, name="GrandBudapest2")[3], alpha=.5, col=NA))  
  grid.text(label = 'COVID-19',
            x= unit(0.5,'npc')+unit(1,'lines'),
            just='left')  
  popViewport()
  
} 
dev.off()

