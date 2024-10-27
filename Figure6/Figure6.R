
setwd("/Users/orlacamus/Desktop/projects/ua-rep-only/6.FC/")

# function summarySE is in package Rmisc, but Rmisc contains plyr which conflicts with dplyr
# https://stackoverflow.com/questions/26923862/why-are-my-dplyr-group-by-summarize-not-working-properly-name-collision-with
# difference between summarySEwithin & summarise:  idvar is used to normalize rather than groupby 
# if want to use plyr, import before dplyr


# Install required packages if necessary:
want = c("data.table", "lme4", "Matrix","ez","ggplot2","dplyr",'ggpubr','patchwork','tidyr',' reticulate','tidyverse','readxl')
have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }
# Now load them all
lapply(want, require, character.only = TRUE)
library(R.matlab)
library(circlize)
library(svglite)
# Read data
subList=c(101,102,103,105,106,107,108,109,110,111,112,115,116,117,118,119,120,121,123,125,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143)
FC_ROIs=c('BA24-rACC','BA34-hip','NAcc','amygdala','lDLPFC','lTPJ','rDLPFC','rTPJ','ventral_striatum','precuneus','BA10-FPC','caudate')


# betweenness 
i=3
k=12

for (i in c(1:12)){
  
  for (runno in c(1,2,3,4,5,6,7,8)){
    d=read.csv(paste0('RRC/elie',runno,'.csv'))
    f=paste0('X.',k)
    if (runno==1){
      ge=data.frame(as.numeric(d[[f]][2:38]))
      ge$run=1
    }else{
      temp=data.frame(as.numeric(d[[f]][2:38]))
      temp$run=runno
      ge=rbind(ge,temp)
    }
  }
  colnames(ge)=c('measure','run')
  agg <- ge %>% group_by(run) %>% dplyr::summarise(mean = mean(measure),sd=sd(measure),n = n(),se=sd(measure)/sqrt(n))
  
  f=ggplot(data = agg, aes(x = factor(run), y=mean)) + 
    
    geom_point(data = agg, aes(x = factor(run), y=mean),size=5, shape=16,alpha=1,color='#e7b13e') +
    geom_errorbar(width=0.2, alpha=0.8,size=1,data = agg,aes(ymin=mean-se, ymax=mean+se,),position = position_nudge(x = 0),color='#e7b13e') +
    geom_smooth(data = ge, aes(x = as.numeric(run), y = measure), col='#e7b13e', method = 'lm',se=F)+
    
    
    #geom_point(data = agg, aes(x = factor(run), y=mean),size=5, shape=16,alpha=1,color='#00b4d8') +
    #geom_errorbar(width=0.2, alpha=0.8,size=1,data = agg,aes(ymin=mean-se, ymax=mean+se,),position = position_nudge(x = 0),color='#00b4d8') +
    #geom_smooth(data = data, aes(x = as.numeric(run), y = beta), col="#0077b6", method = 'lm',se=F)+
    
    scale_x_discrete(name='run') + # have tick marks for each session
    scale_y_continuous(name='betweenness') + 
    #ggtitle(ROIs[i]) +
    theme(axis.text.x = element_text(size=25),
          axis.text.y = element_text(size=20),
          axis.title=element_text(size=25),
          plot.title = element_text(size=25,hjust = 0.5),
          panel.background = element_rect(fill = "white"),
          axis.line = element_line(colour = "black"))
  
  ggsave(file=paste0('/Users/orlacamus/Downloads/ua',i,".svg"), plot=f, width=6, height=4)
  
  test=lm(ge$measure~as.numeric(ge$run))
  s=data.frame(summary(test)$coefficients)
  if (s$Pr...t..[2]<0.05){
    print(i)
    print(FC_ROIs[i])
    print(summary(test))
  }
  
  #test=t.test(ge[ge$run==8,]$measure,ge[ge$run==1,]$measure,paired = TRUE)
  #if (test$p.value<0.05){
  #  print(i)
  #  print(FC_ROIs[i])
  #  print(test)
  #}
  
 
  k=k+9

  
}



elie_AUC=read.csv('elie_AUC.csv')
colnames(elie_AUC)=c('subList','actual','predict')
elie_AUC$subList=subList

cor.test(elie_AUC$actual,elie_AUC$predict)

pred <- predict(lm(predict ~ actual, elie_AUC), 
                se.fit = TRUE, interval = "confidence")
limits <- as.data.frame(pred$fit)


f=ggplot(data = elie_AUC, aes(x = actual, y=predict)) + 
  
  #geom_point(data = agg, aes(x = factor(run), y=mean),size=5, shape=16,alpha=1,color='#7209b7') +
  #geom_errorbar(width=0.2, alpha=0.8,size=1,data = agg,aes(ymin=mean-se, ymax=mean+se,),position = position_nudge(x = 0),color='#7209b7') +
  #geom_smooth(data = data, aes(x = as.numeric(run), y = similarity), col="#3a0ca3", method = 'lm',se=F)+
  
  
  geom_point(size=5, shape=16,alpha=0.50,color='#e7b13e') +
  geom_smooth(col='#e7b13e', method = 'lm',se=F,size=2)+
  
  scale_x_continuous(name='actual AUC change') + # have tick marks for each session
  scale_y_continuous(name='predicted AUC change') + 
  geom_line(aes(x = actual, y = limits$lwr), 
            linetype = 2) +
  geom_line(aes(x = actual, y = limits$upr), 
            linetype = 2) +
  theme(axis.text.x = element_text(size=25),
        axis.text.y = element_text(size=25),
        axis.title=element_text(size=25),
        plot.title = element_text(size=25,hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"))
f
ggsave(file=paste0('/Users/orlacamus/Downloads/elie_AUC.svg'), plot=f, width=6, height=4)




elie_AUC=read.csv('rand_AUC.csv')
colnames(elie_AUC)=c('subList','actual','predict')
elie_AUC$subList=subList

cor.test(elie_AUC$actual,elie_AUC$predict)

pred <- predict(lm(predict ~ actual, elie_AUC), 
                se.fit = TRUE, interval = "confidence")
limits <- as.data.frame(pred$fit)


f=ggplot(data = elie_AUC, aes(x = actual, y=predict)) + 
  
  #geom_point(data = agg, aes(x = factor(run), y=mean),size=5, shape=16,alpha=1,color='#7209b7') +
  #geom_errorbar(width=0.2, alpha=0.8,size=1,data = agg,aes(ymin=mean-se, ymax=mean+se,),position = position_nudge(x = 0),color='#7209b7') +
  #geom_smooth(data = data, aes(x = as.numeric(run), y = similarity), col="#3a0ca3", method = 'lm',se=F)+
  
  
  geom_point(size=5, shape=16,alpha=0.50,color='#2a74a3') +
  geom_smooth(col='#2a74a3', method = 'lm',se=F,size=2)+
  
  scale_x_continuous(name='actual AUC change') + # have tick marks for each session
  scale_y_continuous(name='predicted AUC change') + 
  geom_line(aes(x = actual, y = limits$lwr), 
            linetype = 2) +
  geom_line(aes(x = actual, y = limits$upr), 
            linetype = 2) +
  theme(axis.text.x = element_text(size=25),
        axis.text.y = element_text(size=25),
        axis.title=element_text(size=25),
        plot.title = element_text(size=25,hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"))
f
ggsave(file=paste0('/Users/orlacamus/Downloads/elie_AUC.svg'), plot=f, width=6, height=4)
