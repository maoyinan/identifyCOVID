
#
# plot ROC ----------------------------------------------------------------
names(wes_palettes)
# [1] "BottleRocket1"  "BottleRocket2"  "Rushmore1"     
# [4] "Rushmore"       "Royal1"         "Royal2"        
# [7] "Zissou1"        "Darjeeling1"    "Darjeeling2"   
# [10] "Chevalier1"     "FantasticFox1"  "Moonrise1"     
# [13] "Moonrise2"      "Moonrise3"      "Cavalcanti1"   
# [16] "GrandBudapest1" "GrandBudapest2" "IsleofDogs1"   
# [19] "IsleofDogs2"   
wes_palette('Moonrise3')

roc_colors <- 
  c(wes_palette(name="Darjeeling1")[c(4,2)],
    wes_palette(name="Moonrise3")[c(1,2,3)])


id <- order(specificity)
id0 <- order(specificity0)
auc <- sum(diff(specificity[id])*rollmean(sensitivity[id],2))
auc0 <- sum(diff(specificity0[id0])*rollmean(sensitivity0[id0],2))

circle_colors <- c('orange','red')
circle_pos  <-  c(.95,.9)
circle_cutoff <- c(cutoff_O95*(roc_total-1)+1,cutoff_O90*(roc_total-1)+1)
plot.new()
tiff('fig3.tif',
     height=7,width=16,units='cm',res=450,pointsize=9)
{
  pushViewport(plotViewport(c(0,0,0,0)))
  pushViewport(viewport(layout=grid.layout(nrow=1,ncol=2)))
  
  x_rg <- c(1,0)
  x_axis <- sort(c(.9,.95,pretty(x_rg)))
  y_rg <- c(0,1)
  y_axis <- pretty(y_rg)
  
  {# leftside
    pushViewport(viewport(layout.pos.row=1, layout.pos.col=1))
    pushViewport(plotViewport(c(4,4,1,1), xscale= x_rg, yscale= y_rg))
    grid.rect()
    grid.text('Specificity', y= unit(-3,'lines'))
    grid.text('Sensitivity', x= unit(-3,'lines'), rot=90)
    grid.xaxis(x_axis, x_axis)
    grid.yaxis(y_axis, y_axis)
    grid.text('(a)', x=unit(1,'lines'), y=unit(1,'npc')+unit(-.5,'lines'))
    # grid.text(sprintf('Full data AUC = %.2f', auc0), 
    #           x= unit(1,'npc')-unit(1,'lines'), 
    #           y= unit(2,'lines'), just='right',
    #           gp=gpar(col=roc_colors[2]))
    # grid.text(sprintf('LOOCV AUC = %.2f', auc), 
    #           x= unit(1,'npc')-unit(1,'lines'), 
    #           y= unit(1,'lines'), just='right')
    grid.legend(
      labels=c(sprintf('Full data AUC = %.2f', auc0), sprintf('LOOCV AUC = %.2f', auc)),
      nrow=2,ncol=1,
      pch=1,
      gp=gpar(col=c(roc_colors[2],'black')),
      vp= viewport(x=.6, y=unit(2,'lines'))
    )
    grid.points(
      pch=1, size=unit(.5, "char"),
      specificity[id],
      sensitivity[id],
      default.units = 'native')
    grid.points(
      pch=1, size=unit(.5, "char"),
      specificity0[id0],
      sensitivity0[id0],
      default.units = 'native',
      gp=gpar(col=roc_colors[2]))
    grid.lines(
      specificity[id], 
      sensitivity[id],
      default.units = 'native')  
    grid.lines(
      specificity0[id0], 
      sensitivity0[id0],
      default.units = 'native',
      gp=gpar(col=roc_colors[2]))  
    for(j in seq_along(circle_pos)){
      pos <- circle_pos[j]
      grid.lines(
        pos,
        c(0, sensitivity0[circle_cutoff[j]]),
        default.units = 'native',
        gp = gpar(lty=2, col=circle_colors[j])
      )
      grid.points(
        pch='*', #size=unit(2, "char"),
        specificity0[circle_cutoff[j]], 
        sensitivity0[circle_cutoff[j]],
        default.units = 'native',
        gp = gpar(cex=2, col=circle_colors[j])
      )
    }
    popViewport()
    popViewport()
  }
  {# rightside
    pushViewport(viewport(layout.pos.row=1, layout.pos.col=2))
    pushViewport(plotViewport(c(4,4,1,1), xscale= x_rg, yscale= y_rg))
    grid.rect()
    grid.text('Specificity', y= unit(-3,'lines'))
    grid.text('Sensitivity', x= unit(-3,'lines'), rot=90)
    grid.xaxis(x_axis, x_axis)
    grid.yaxis(y_axis, y_axis)
    grid.text('(b)', x=unit(1,'lines'), y=unit(1,'npc')+unit(-.5,'lines'))
    
    v_day <- unique(tb_sensitivity_day$Day)
    for(k in seq_along(v_day)){
      tbk <- tb_sensitivity_day %>% filter(Day==v_day[k])
      grid.points(
        pch=1, size=unit(.5, "char"),
        tbk$Specificity,
        tbk$Sensitivity,
        default.units = 'native',
        gp=gpar(col=roc_colors[k]))
      grid.lines(
        tbk$Specificity, 
        tbk$Sensitivity,
        default.units = 'native',
        gp=gpar(col=roc_colors[k]))  
    }
    grid.legend(
      labels=paste('Day', x_labels),
      nrow=length(v_day),ncol=1,
      pch=1, gp=gpar(col=roc_colors),
      vp= viewport(x=.8, y=unit(4,'lines'))
    )
    popViewport()
    popViewport()
  }
  popViewport()
  popViewport()
}  
dev.off()

