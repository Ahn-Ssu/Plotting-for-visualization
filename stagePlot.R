# install.packages("wesanderson")
library(wesanderson)
library(scales)

pltColors = c(wes_palette(name="GrandBudapest1") ,rev(wes_palette(name="GrandBudapest2")))

setwd("/Users/ahnssu/plot")
getwd()
src_dirPath = "~/plot/3_output/2021-08-18"
src_dirPath = paste0(src_dirPath,"/")
src_dirPath

filenameList = dir(src_dirPath)
filenameList

setwd("~/plot/plot")
print(paste0("now working directory is ",getwd()))
print(paste0("the directory of target(result) csv is ",src_dirPath))

for( iteration in 1 : length(filenameList)){
  
  absolPath = paste0(src_dirPath,filenameList[iteration])
  exp_result = read.csv(file = absolPath)
  
  
  
  if ( min(exp_result$MAE) < 2 )   { 
    y_range=c(0.5,2.5)
    y_line = 1.5} # CCEL
  else if (min(exp_result$MAE) < 3.5){ 
    y_range=c(2.5,5)
    y_line = 3}
  else { 
    y_range=c(3,7)
    y_line = 4.5}
  
  
  colList = names(exp_result) # for removin meaningless labels
  folds = split(exp_result,exp_result$stage)
  
  png(paste0(filenameList[iteration],"_.png"), width=5000, height = 4000, res=800, unit='px' )
  label = c() # for making legend
  
  for( stageNum in 1 : length(folds)){
    label = c(label, paste0('fold ',toString(stageNum)))
    fold = data.frame(folds[stageNum],check.names = F)
    colnames(fold) <- colList 
    plot(seq(25, max(fold$epoch),25), fold$MAE, type="l", col=alpha(pltColors[stageNum], 0.85),
         xlim=c(0, max(exp_result$epoch)), ylim=y_range,
         xlab='# of epochs',ylab='MAE')
    
    par(new=TRUE)
    
  }
  
  legend("topright",label,col = pltColors,lwd=3,lty=1, cex=0.9)
  lines(x=c(0,max(exp_result$epoch)),
        y=c(min(exp_result$MAE),min(exp_result$MAE)),
        col=pltColors[length(pltColors)],
        lty=2)
  
  lines(x=c(0,max(exp_result$epoch)),
        y=c(y_line,y_line),
        col='red',
        lty=5)
  dev.off()
}

