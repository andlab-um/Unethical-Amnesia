
setwd("/Users/orlacamus/Desktop/projects/ua-rep-only/3.ROIextraction")

# Install required packages if necessary:
packages = c("data.table", "lme4", "Matrix", "stargazer","coxme","survival",
             "bdsmatrix","ez","ggplot2","lmerTest","dplyr",'yarrr','gridExtra',
             'ggsignif','ggpubr','patchwork','readxl','reticulate','Hmisc','ggnewscale',
             'ggcorrplot2','RColorBrewer', 'igraph', 'car', 'beeswarm', 'plotrix','rockchalk',
             'plotly','hrbrthemes')

newpackages = packages[!(packages %in% installed.packages()[,"Package"])]
if(length(newpackages)) install.packages(newpackages)
loaded = lapply(packages, require, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
cat(paste('## ', sum(unlist(loaded)), ' out of ', length(unlist(loaded)) ,' packages loaded\n', sep = ''))



# subject list: 37
subList = c(101,102,103,105,106,107,108,109,110,111,112,115,116,117,118,119,120,121,123,125,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143)

dir='/Users/orlacamus/Downloads/f-beta-uarep/'

ROIs=c('BA34-hip','caudate')



# GLM1: correlation with memory
# only meta
for (i in c(1:length(ROIs))){
  t0=read_excel("ROIbeta_GLM1_SPM.xlsx",sheet=ROIs[i])
  
  
  colnames(t0)=c('subjno','enhance_lie_1','enhance_lie_2','enhance_lie_3','enhance_lie_4','enhance_lie_5','enhance_lie_6','enhance_lie_7','enhance_lie_8',
                 'enhance_honesty_1','enhance_honesty_2','enhance_honesty_3','enhance_honesty_4','enhance_honesty_5','enhance_honesty_6','enhance_honesty_7','enhance_honesty_8',
                 'enhance_random_1','enhance_random_2','enhance_random_3','enhance_random_4','enhance_random_5','enhance_random_6','enhance_random_7','enhance_random_8')
  
  #colnames(t0)[1]='subjno'
  t=t0[t0$subjno %in% subList,]
  
  #temp_lie=data.frame(t$subjno,t$modxlie_1,t$modxlie_8)########
  temp_lie=data.frame(t$subjno,t$enhance_lie_1,t$enhance_lie_8)########
  colnames(temp_lie)=c('subjno','1','8')
  
  #temp_hon=data.frame(t$subjno,t$modxhon_1,t$modxhon_8)########
  temp_hon=data.frame(t$subjno,t$enhance_honesty_1,t$enhance_honesty_8)########
  colnames(temp_hon)=c('subjno','1','8')
  
  #temp_rand=data.frame(t$subjno,t$modxrand_1,t$modxrand_8)########
  temp_rand=data.frame(t$subjno,t$enhance_random_1,t$enhance_random_8)########
  colnames(temp_rand)=c('subjno','1','8')
  
  
  file_lie = temp_lie[temp_lie$subjno %in% subList,]
  file_lie=temp_lie
  data_lie=file_lie %>% tidyr::pivot_longer(cols = -"subjno")
  data_lie$condition='enhance dishonesty'
  
  file_hon = temp_hon[temp_hon$subjno %in% subList,]
  file_hon=temp_hon
  data_hon=file_hon %>% tidyr::pivot_longer(cols = -"subjno")
  data_hon$condition='enhance honesty'
  
  file_rand = temp_rand[temp_rand$subjno %in% subList,]
  file_rand=temp_rand
  data_rand=file_rand %>% tidyr::pivot_longer(cols = -"subjno")
  data_rand$condition='random'
  
  if (cor.test(temp_rand$`1`-temp_rand$`8`,change1[change1$condition=='enhance_random',]$RT)$p.value<0.05){
  #if (cor.test(temp_rand$`1`-temp_rand$`8`,agg_change1$RT)$p.value<0.05){
    print(i)
    print(ROIs[i])
    print('random')
    
  }
  
  if (cor.test(temp_lie$`1`-temp_lie$`8`,change1[change1$condition=='enhance_lie',]$meta)$p.value<0.05){
  #if (cor.test(temp_lie$`1`-temp_lie$`8`,agg_change1$RT)$p.value<0.05){
    print(i)
    print(ROIs[i])
    print('elie')
    print(cor.test(temp_lie$`1`-temp_lie$`8`,change1[change1$condition=='enhance_lie',]$meta))
    temp=data.frame(temp_lie$`1`-temp_lie$`8`)
    temp$memchange=change1[change1$condition=='enhance_lie',]$meta
    colnames(temp)[1]='betachange'
    f=ggplot(data = temp, aes(x = betachange, y=memchange)) + 
      
      #geom_point(data = agg, aes(x = factor(run), y=mean),size=5, shape=16,alpha=1,color='#7209b7') +
      #geom_errorbar(width=0.2, alpha=0.8,size=1,data = agg,aes(ymin=mean-se, ymax=mean+se,),position = position_nudge(x = 0),color='#7209b7') +
      #geom_smooth(data = data, aes(x = as.numeric(run), y = similarity), col="#3a0ca3", method = 'lm',se=F)+
      
      
      geom_point(size=5, shape=16,color='#e7b13e',alpha=0.7) +
      geom_smooth(col='#e7b13e', method = 'lm',se=T,fill='#e7b13e',linewidth=2)+
      
      scale_x_continuous(name='beta change\n(run1 minus run8)') + # have tick marks for each session
      scale_y_continuous(name='metacognition change\n(pre minus post)') + 
      theme(axis.text.x = element_text(size=25),
            axis.text.y = element_text(size=25),
            axis.title=element_text(size=25),
            plot.title = element_text(size=25,hjust = 0.5),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(colour = "black"))
    
    f
    ggsave(file=paste0('/Users/orlacamus/Downloads/',ROIs[i],'.svg'), plot=f, width=8, height=6)
    
  }
  
} 

setwd("/Users/orlacamus/Desktop/projects/ua-rep-only/4.RSA")
midcl='#FFAD65FF'
darkcl='#CC5800FF'
ROIs=c('amygL','ACC','mPFC')
#for (i in c(1:length(ROIs))){
for (i in c('amygL','ACC','mPFC')){
  #for (i in sigua){
  #for (i in sig){
  t=read_excel("ROIdistinctiveness_elie_rep.xlsx",sheet=i)
  
  colnames(t)=c('subjno','1','2','3','4','5','6','7','8','9')
  
  
  
  data=t %>% tidyr::pivot_longer(cols = -"subjno")
  colnames(data)=c('subjno','run','beta')
  data$session=as.factor(data$run)
  test=lm(data$beta~as.numeric(data$run))
  summary(test)
  #out=lm(beta~session,data=data_lie)
  #s=summary(out)
  #print(s)
  agg <- data %>% group_by(run) %>% dplyr::summarise(mean = mean(beta),sd=sd(beta),n = n(),se=sd(beta)/sqrt(n))
  test=lm(data$beta~as.numeric(data$run))
  s=data.frame(summary(test)$coefficients)
  if (s$Pr...t..[2]<0.05){
    print(i)
    print(ROIs[i])
    print(summary(test))
    f=ggplot(data = agg, aes(x = factor(run), y=mean)) + 
      
      geom_point(data = agg, aes(x = factor(run), y=mean),size=5, shape=16,alpha=1,color=midcl) +
      geom_errorbar(width=0.2, alpha=0.8,size=1,data = agg,aes(ymin=mean-se, ymax=mean+se,),position = position_nudge(x = 0),color=midcl) +
      geom_smooth(data = data, aes(x = as.numeric(run), y = beta), col=darkcl, method = 'lm',se=F)+
      
      scale_x_discrete(name='run') + # have tick marks for each session
      scale_y_continuous(name='Neural distinctiveness') + 
      ggtitle(ROIs[i]) +
      theme(axis.text.x = element_text(size=25),
            axis.text.y = element_text(size=25),
            axis.title=element_text(size=25),
            plot.title = element_text(size=25,hjust = 0.5),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(colour = "black"))
    
    f
    
    ggsave(file=paste0('/Users/orlacamus/Downloads/',ROIs[i],".svg"), plot=f, width=6, height=4.5)
    
  }
  
}



#for (i in c(1:length(ROIs))){
for (i in c('amygL','ACC','mPFC')){
  #for (i in sigua){
  #for (i in sig){
  t=read_excel("ROIdistinctiveness_elie_rep.xlsx",sheet=i)
  
  colnames(t)=c('subjno','1','2','3','4','5','6','7','8','9')
  
  
  
  data=t %>% tidyr::pivot_longer(cols = -"subjno")
  colnames(data)=c('subjno','run','beta')
  data$session=as.factor(data$run)
  test=lm(data$beta~as.numeric(data$run))
  summary(test)
  #out=lm(beta~session,data=data_lie)
  #s=summary(out)
  #print(s)
  temp=data.frame(data[data$session==8,]$beta)
  colnames(temp)='beta'
  temp$AUC=abs(change1[change1$condition=='enhance_lie',]$AUC)
  temp=temp[-34,]
  a=cor.test(temp$beta,temp$AUC)
  #a=cor.test(data[data$session==8,]$beta,allchange1$meta)
  #a=cor.test(agg$mean,allchange1$meta)
  #a=cor.test(agg$mean,change1[change1$condition=='enhance_lie',]$meta)
  
  if (a$p.value<0.05){
    print(i)
    print(ROIs[i])
    print(a)
    
    pred <- predict(lm(AUC ~ beta, temp), 
                    se.fit = TRUE, interval = "confidence")
    limits <- as.data.frame(pred$fit)
    
    
    
    f=ggplot(data = temp, aes(x = beta, y=AUC)) + 
      
      geom_point(data = temp, aes(x = beta, y=AUC),size=5, shape=16,alpha=1,color=midcl) +
      geom_smooth(data = temp, aes(x = beta, y=AUC),col=darkcl, method = 'lm',se=F)+
      scale_x_continuous(name='distinctiveness') + # have tick marks for each session
      scale_y_continuous(name='abs(AUC change)') + 
      geom_line(aes(x = beta, y = limits$lwr), 
                linetype = 2) +
      geom_line(aes(x = beta, y = limits$upr), 
                linetype = 2) +
      ggtitle('') +
      theme(axis.text.x = element_text(size=25),
            axis.text.y = element_text(size=25),
            axis.title=element_text(size=25),
            plot.title = element_text(size=25,hjust = 0.5),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(colour = "black"))
    ggsave(file=paste0('/Users/orlacamus/Downloads/corr-',i,".svg"), plot=f, width=6, height=4.5)
  }
  

}
