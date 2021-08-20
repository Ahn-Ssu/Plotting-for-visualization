# install.packages("wesanderson")
library(wesanderson)
pltColors = c('#1f77b4', '#ff7f0e', '#2ca02c','#d62728', '#9467bd','#8c564b','#e377c2','#7f7f7f','#bcbd22','#17becf')
setwd("~/plot/meanPlot-result")

dataPath = "~/plot/meanPlot-src/"
file_list=paste0(dataPath,dir(dataPath))
file_list
name_list=c('GIN','SAGE')
K=1
for(i in 1:length(name_list)){
  assign(name_list[i],read.csv(file_list[i]))
}

coln = colnames(eval(parse(text=name_list[1])))[-1]

tmp=eval(parse(text=name_list[1]))
rn=nrow(tmp)
for(j in 1:length(name_list)){
  tmp = NULL
  for(i in 1:(rn/K)){
    tmp=rbind(tmp,as.numeric(lapply(eval(parse(text=name_list[j]))[seq(i,rn,rn/K),2:5],mean)))
  }
  tmp=as.data.frame(tmp)  
  colnames(tmp)=coln
  
  assign(name_list[j],tmp)
}


png(filename = paste0("./CCEL GIN vs SAGE in d5 K=1.png"), width=5000,height=4000, res = 800)
xEnd = max(eval(parse(text=name_list[1]))$epoch)
plot(0, xlab='# of epochs',ylab='MAE',xlim=c(0,max(eval(parse(text=name_list[1]))$epoch))
     ,ylim=c(1,2),cex=0,main="")


lines(x=c(0,xEnd),
      y=c(1.5,1.5),
      col="red")
# lines(x=c(0,10000), y=c(4,4))

for(i in 1:length(name_list)){
  dt= eval(parse(text=name_list[i]))
  lines(dt$epoch,dt$MAE,col=pltColors[i],lwd=1,lty=1)
}

legend("topright",name_list,col = pltColors,lwd=3,lty=1)
dev.off()

rm(list=name_list)

