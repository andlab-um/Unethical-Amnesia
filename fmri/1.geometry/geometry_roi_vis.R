rm(list=ls())
# Install required packages if necessary:
want = c("data.table", "lme4", "Matrix","ez","ggplot2","dplyr",'ggpubr','patchwork','tidyr',' reticulate','tidyverse','readxl','graphics')
have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }
# Now load them all
lapply(want, require, character.only = TRUE)
library(reticulate)
library(svglite)
library(raveio)
library(rstatix)

setwd("/Users/orlacamus/Desktop/projects/unethical_amnesia/analysisCode/6.geometry")
subL=c(102,103,105,107,108,110,111,112,113,114,115,116,117,118,119,120,121,122,123,125,127,128,129,130,131,132)




ROIs=c('BA6-SMA','BA9-dlPFC','BA13-insula','BA24-dACC',
'BA28-hip1','BA34-hip','lTPJ','BA23-PCC','OFC')

## separation score
for (i in c(1:length(ROIs))){
  temp=read_excel("separation0925.xlsx",sheet=ROIs[i])
  temp=temp[,2:4]
  colnames(temp)=c('islie','con','mon')
  temp['subjno']=subL
  long=temp %>% pivot_longer(cols=-'subjno')
  colnames(long)=c('subjno','var','score')
  agg <- long %>% group_by(var) %>% dplyr::summarise(mean = mean(score),sd=sd(score),n = n(),se=sd(score)/sqrt(n))
  stat.test <- long %>%
    group_by(var) %>%
    t_test(score ~ 1, mu = 0,alternative='greater') %>%
    mutate(y.position = 0.15)
  print(stat.test)
  v=factor(agg$var,c('islie','con','mon'))
  vs=factor(long$var,c('islie','con','mon'))
  f=ggplot(data = agg, aes(x = factor(v), y=mean)) + 
    #geom_bar(data = agg, aes(x = factor(model), y=mean),stat = "identity",width=0.8,fill='#CDC1FF',color='#CDC1FF') +
    geom_bar(data = agg, aes(x = factor(v), y=mean),stat = "identity",width=0.8,fill='white',color='black') +
    geom_jitter(data = long, aes(x = factor(vs), y=score,fill=vs,color=vs),size=2, width=0.15,shape=21, alpha=0.3) +
    #stat_pvalue_manual(stat.test, label = "p", xmin = "var", xmax = NULL)+
    scale_fill_manual(values=c("dark red",'#578737','#70309f')) +
    scale_color_manual(values=c("dark red",'#578737','#70309f')) +
    geom_errorbar(width=0.2, alpha=0.8,size=1,data = agg,aes(x= factor(v),ymin=mean-se, ymax=mean+se),position = position_nudge(x = 0),color='black') +
    scale_x_discrete(name='var') + # have tick marks for each session
    scale_y_continuous(name='separation score') + 
    ggtitle(ROIs[i]) +
    theme(axis.text.x = element_text(size=18),
          axis.text.y = element_text(size=25),
          axis.title=element_text(size=25),
          plot.title = element_text(size=25,hjust = 0.5),
          panel.background = element_rect(fill = "white"),
          axis.line = element_line(colour = "black"),
          legend.position = "none")
  
  ggsave(file=paste0('/Users/orlacamus/Downloads/',ROIs[i],".svg"), plot=f, width=5, height=5)
}
  


###### model fitting

modno=12
if (modno==10){
  models=c('grid','orth1','orth2','par1','par2','rotgrid1','rotgrid2','con','mon','diag')
}

ROIs_name=c("BA9-dlPFC","BA6-SMA","BA35-HIP2","lTPJ","BA23-PCC")

ROIs=c("BA9-dlPFC","OFC","lTPJ","BA24-dACC","BA28-hip1")
ROIs=c("BA6-SMA",'OFC',"lTPJ","BA23-PCC","BA13-insula")

ROIs_name=ROIs
# bar plot
for (i in c(1:length(ROIs))){
  temp=data.frame(matrix(nrow = length(subL), ncol = length(models)))
  a=1
  for (subno in subL){
    t=read_excel("RDMregression-12models0925.xlsx",sheet=paste0('sub-',subno))
    temp[a,]=t(t[,ROIs[i]])
    a=a+1
  }
  colnames(temp)=models
  temp['subjno']=subL
  long=temp %>% pivot_longer(cols=-'subjno')
  colnames(long)=c('subjno','model','beta')

  agg <- long %>% group_by(model) %>% dplyr::summarise(mean = mean(beta),sd=sd(beta),n = n(),se=sd(beta)/sqrt(n))
  
  stat.test <- long %>%
    group_by(model) %>%
    t_test(beta ~ 1, mu = 0, alternative='greater')%>%
    adjust_pvalue(method = "fdr")%>%
    mutate(y.position = 0.5)
  
  write_csv(stat.test,paste0('12models_stat/',ROIs[i],'.csv'))
  
  f=ggplot(data = agg, aes(x = factor(model,levels = models), y=mean)) + 
    #geom_bar(data = agg, aes(x = factor(model), y=mean),stat = "identity",width=0.8,fill='#CDC1FF',color='#CDC1FF') +
    geom_bar(stat = "identity",width=0.8,fill='white',color='black') +
    geom_jitter(data = long, aes(x = factor(model,levels = models), y=beta),size=1.5,width = 0, shape=21, fill="gray70",colour="gray70") +
    #stat_pvalue_manual(stat.test, label = "p", xmin = "model", xmax = NULL)+
    #scale_fill_manual(values=c("dark blue","#c9342c",'green')) +
    #scale_color_manual(values=c("dark blue","#c9342c",'green')) +
    geom_errorbar(width=0.2, alpha=0.8,size=1,data = agg,aes(x=factor(model,levels = models),ymin=mean-se, ymax=mean+se),position = position_nudge(x = 0),color='black') +
    scale_x_discrete(name='model RDMs') + # have tick marks for each session
    scale_y_continuous(name='Regression Coefficients \n(beta value)') + 
    ggtitle(ROIs_name[i]) +
    theme(axis.text.x = element_text(size=18,angle=45, vjust=1, hjust=1),
          axis.text.y = element_text(size=25),
          axis.title=element_text(size=25),
          plot.title = element_text(size=25,hjust = 0.5),
          panel.background = element_rect(fill = "white"),
          axis.line = element_line(colour = "black"))
  f
  ggsave(file=paste0('/Users/orlacamus/Downloads/RDMregression/formal-',modno,'models-',ROIs[i],".svg"), plot=f, width=6, height=6)
    
} 



# parameterized model parameters
setwd("/Users/orlacamus/Desktop/projects/unethical_amnesia/analysisCode/6.geometry/results_seuc")

all=matrix(ncol = 8)
colnames(all)=c('subj','lie','hon','con','mon','rot','offset','ROI')
for (roi in ROIs){
  compr=matrix(data=NA, nrow =length(subL),ncol = 7)
  colnames(compr)=c('subj','lie','hon','con','mon','rot','offset')
  i=1
  for (subno in subL){
    subdata <- read_mat(paste0("results_fmincon_1000iter_parametrised_rdms_",roi,"_sub-",subno,".mat"))
    #comp_lie=log(subdata[["results/betas_hat"]][1]/subdata[["results/betas_hat"]][2])
    #comp_hon=log(subdata[["results/betas_hat"]][3]/subdata[["results/betas_hat"]][4])
    #comp_con=log(subdata[["results/betas_hat"]][1]/subdata[["results/betas_hat"]][3])
    #comp_mon=log(subdata[["results/betas_hat"]][2]/subdata[["results/betas_hat"]][4])
    
    comp_lie=subdata[["results/betas_hat"]][1]
    comp_hon=subdata[["results/betas_hat"]][2]
    comp_con=subdata[["results/betas_hat"]][3]
    comp_mon=subdata[["results/betas_hat"]][4]
    
    rot=subdata[["results/betas_hat"]][5]
    
    offset=subdata[["results/betas_hat"]][6]
    
    compr[i,'subj']=subno
    compr[i,'lie']=comp_lie
    compr[i,'hon']=comp_hon
    compr[i,'con']=comp_con
    compr[i,'mon']=comp_mon
    compr[i,'rot']=rot
    compr[i,'offset']=offset
    
    i=i+1
  }


  roi_comp=data.frame(compr)
  roi_comp$ROI=1
  roi_comp[,'ROI']=roi
  all=rbind(all,roi_comp)



  long=data.frame(compr) %>% pivot_longer(cols=-'subj')
  
  compr_long=long[(long['name']!='rot' & long['name']!='offset'),]
  
  agg <- compr_long %>% group_by(name) %>% dplyr::summarise(mean = mean(value),sd=sd(value),n = n(),se=sd(value)/sqrt(n))
  stat.test <- compr_long %>%
    group_by(name) %>%
    t_test(value ~ 1, mu = 0) %>%
    mutate(y.position = 0.2)
  
  f=ggplot(data = agg, aes(x = factor(name), y=mean)) + 
    #geom_bar(data = agg, aes(x = factor(model), y=mean),stat = "identity",width=0.8,fill='#CDC1FF',color='#CDC1FF') +
    geom_bar(data = agg, aes(x = factor(name), y=mean),stat = "identity",width=0.8,fill='white',color='black') +
    geom_jitter(data = compr_long, aes(x = factor(name), y=value),size=2,width = 0.15, shape=21, fill="#A594F9",colour="#A594F9",alpha=0.3) +
    stat_pvalue_manual(stat.test, label = "p", xmin = "name", xmax = NULL)+
    #scale_fill_manual(values=c("dark blue","#c9342c",'green')) +
    #scale_color_manual(values=c("dark blue","#c9342c",'green')) +
    geom_errorbar(width=0.2, alpha=0.8,size=1,data = agg,aes(ymin=mean-se, ymax=mean+se),position = position_nudge(x = 0),color='black') +
    scale_x_discrete(name='response condition') + # have tick marks for each session
    scale_y_continuous(name='compression') + 
    ggtitle(roi) +
    theme(axis.text.x = element_text(size=18),
          axis.text.y = element_text(size=25),
          axis.title=element_text(size=25),
          plot.title = element_text(size=25,hjust = 0.5),
          panel.background = element_rect(fill = "white"),
          axis.line = element_line(colour = "black"))
  
  ggsave(file=paste0('/Users/orlacamus/Downloads/RDMregression-figs/compr-',roi,".svg"), plot=f, width=5, height=5)
}




# parameterized model parameters: selected ROIs
results_type='results_seuc'

setwd(paste0("/Users/orlacamus/Desktop/projects/unethical_amnesia/analysisCode/6.geometry/",results_type))

all=matrix(ncol = 6)
colnames(all)=c('subj','lie','hon','rot','offset','ROI')
new_ROIs=c("BA9-dlPFC","BA6-SMA","BA24-dACC","OFC","BA35-hip2")
ROIs=c('amyg','amygL','amygR')

#for (roi in ROIs){
for (roi in c('precuneus')){
  compr=matrix(data=NA, nrow =length(subList),ncol = 5)
  colnames(compr)=c('subj','lie','hon','rot','offset')
  i=1
  for (subno in subList){
    subdata <- read_mat(paste0("results_fmincon_1000iter_parametrised_rdms_",roi,"_sub-",subno,".mat"))
    comp_lie=log(subdata[["results/betas_hat"]][1]/subdata[["results/betas_hat"]][2])
    comp_hon=log(subdata[["results/betas_hat"]][3]/subdata[["results/betas_hat"]][4])
    
     
    rot=subdata[["results/betas_hat"]][5]
    
    offset=subdata[["results/betas_hat"]][6]
    
    compr[i,'subj']=subno
    compr[i,'lie']=comp_lie
    compr[i,'hon']=comp_hon
    compr[i,'rot']=rot
    compr[i,'offset']=offset
    
    i=i+1
  }

  roi_comp=data.frame(compr)
  roi_comp$ROI=1
  roi_comp[,'ROI']=roi
  all=rbind(all,roi_comp)
  
}


setwd("/Users/orlacamus/Desktop/projects/unethical_amnesia/analysisCode/6.geometry")
write.csv(all,paste0("parameterized-",results_type,".csv"), row.names=FALSE)


all=read_csv(paste0("parameterized-",results_type,".csv"))

all_temp=all
all = all[-1,]
all_temp = all_temp[-1,]
#all_temp$lie=all_temp$conlie/all_temp$monlie
#all_temp$hon=all_temp$conhon/all_temp$monhon

long=data.frame(all_temp) %>% pivot_longer(cols=-c('subj','ROI'))

compr_long=long[(long['name']=='lie' | long['name']=='hon'),]


agg <- compr_long %>% group_by(name,ROI) %>% dplyr::summarise(mean = mean(value),sd=sd(value),n = n(),se=sd(value)/sqrt(n))
stat.test <- compr_long %>%
  group_by(name,ROI) %>%
  t_test(value ~ 1, mu = 0) %>%
  mutate(y.position = 0.2)
write.csv(stat.test,'geometry_results.csv')
new_ROIs=c("BA9-dlPFC","BA6-SMA","BA35-hip2",'lTPJ','BA23-PCC')
new_ROIs_name=c("BA9-dlPFC","BA6-SMA","BA35-HIP2",'lTPJ','BA23-PCC')
ROIs=c("BA9-dlPFC","OFC","lTPJ","BA24-dACC","BA28-hip1")
ROIs=c("BA6-SMA","OFC","lTPJ","BA23-PCC","BA13-insula")
ROIs=c("BA9-dlPFC",'BA34-hip','BA24-dACC')
new_ROIs=ROIs
new_ROIs_name=ROIs
agg_temp=agg[agg$ROI %in% new_ROIs,]
ROI_order=factor(agg_temp$ROI,level=new_ROIs)

f=ggplot(data = agg_temp, aes(x = ROI_order, y=mean,group=name)) + 
  geom_hline(yintercept = 0,color='black',size=1,linetype='dashed')+
  geom_line(data = agg_temp, aes(x=interaction(name,ROI_order),y=mean,group=ROI_order),color='gray70')+
  geom_point(data = agg_temp, aes(x = interaction(name,ROI_order), y=mean,color=name),size=5,position = position_dodge(width = 0.8)) +
  #stat_pvalue_manual(stat.test, label = "p", xmin = "name", xmax = NULL)+

  scale_color_manual(values=c("gray70",'black')) +
  geom_errorbar(alpha=0.8,width=0.4,size=1,data = agg_temp,aes(x = interaction(name,ROI_order),ymin=mean-se, ymax=mean+se,color=name),position = position_dodge(width = 0.8)) +
  scale_x_discrete(name='moral response') + # have tick marks for each session
  scale_y_continuous(name='') + 
  ggtitle('') +
  theme(axis.text.x = element_text(size=15,angle = 45, vjust=1, hjust=1),
        axis.text.y = element_text(size=25),
        axis.title=element_text(size=25),
        plot.title = element_text(size=25,hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        legend.position='none')
f
ggsave(file=paste0('/Users/orlacamus/Downloads/compr.svg'), plot=f, width=7, height=5)



rot_plot=all[,c('subj','rot','ROI')]
stat.test <- rot_plot %>%
  group_by(ROI) %>%
  t_test(rot ~ 1, mu = 0) %>%
  mutate(y.position = 1.5)

stat.test <- rot_plot %>%
  group_by(ROI) %>%
  t_test(rot ~ 1, mu = 0) %>%
  mutate(y.position = 1.5)



agg <- rot_plot %>% group_by(ROI) %>% dplyr::summarise(mean = mean(rot),sd=sd(rot),n = n(),se=sd(rot)/sqrt(n))
agg=agg[agg$ROI %in% new_ROIs,]
rot_plot=rot_plot[rot_plot$ROI %in% new_ROIs,]
ROI_order=factor(agg$ROI,level=new_ROIs)
f=ggplot(data = agg, aes(x = ROI_order, y=mean)) + 
  geom_bar(stat = "identity",width=0.8,fill='white',color='gray70') +
  geom_jitter(data = rot_plot, aes(x = factor(ROI,level=new_ROIs), y=rot),size=2, width=0,shape=21,color='gray70') +
  #stat_pvalue_manual(stat.test, label = "p", xmin = "ROI", xmax = NULL)+
  #scale_fill_manual(values=c("dark blue","#c9342c",'green')) +
  #scale_color_manual(values=c("dark blue","#c9342c",'green')) +
  geom_errorbar(width=0.2, alpha=0.8,size=1,data = agg,aes(x=ROI_order,ymin=mean-se, ymax=mean+se),position = position_nudge(x = 0),color='gray70') +
  geom_line(color='black',aes(group=1),size=1)+
  scale_x_discrete(name='',labels=new_ROIs_name) + # have tick marks for each session
  scale_y_continuous(name='Rotation') +
  geom_hline(yintercept = 0,color='black',size=1,linetype='dashed')+
  ggtitle('') +
  theme(axis.text.x = element_text(size=20,angle = 45, vjust=1, hjust=1),
        axis.text.y = element_text(size=25),
        axis.title=element_text(size=25),
        plot.title = element_text(size=25,hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"))
f
ggsave(file=paste0('/Users/orlacamus/Downloads/rot.svg'), plot=f, width=4, height=6)#




# offset
offset_plot=all[,c('subj','offset','ROI')]

stat.test <- offset_plot %>%
  group_by(ROI) %>%
  t_test(offset ~ 1, mu = 1) %>%
  mutate(y.position = 1)
agg <- offset_plot %>% group_by(ROI) %>% dplyr::summarise(mean = mean(offset),sd=sd(offset),n = n(),se=sd(offset)/sqrt(n))
agg=agg[agg$ROI %in% new_ROIs,]
offset_plot=offset_plot[offset_plot$ROI %in% new_ROIs,]
ROI_order=factor(agg$ROI,level=new_ROIs)


f=ggplot(data = agg, aes(x = factor(ROI_order), y=mean)) + 
  #geom_bar(data = agg, aes(x = factor(model), y=mean),stat = "identity",width=0.8,fill='#CDC1FF',color='#CDC1FF') +
  geom_bar(data = agg, aes(x = factor(ROI_order), y=mean),stat = "identity",width=0.8,fill='white',color='black') +
  geom_jitter(data = offset_plot, aes(factor(ROI,level=new_ROIs), y=offset),size=2,width = 0.15, shape=21, fill="gray70",colour="gray70",alpha=0.3) +
  #stat_pvalue_manual(stat.test, label = "p", xmin = "ROI", xmax = NULL)+
  #scale_fill_manual(values=c("dark blue","#c9342c",'green')) +
  #scale_color_manual(values=c("dark blue","#c9342c",'green')) +
  geom_errorbar(width=0.2, alpha=0.8,size=1,data = agg,aes(x=ROI_order,ymin=mean-se, ymax=mean+se),position = position_nudge(x = 0),color='black') +
  scale_x_discrete(name='',labels=new_ROIs_name) + # have tick marks for each session
  scale_y_continuous(name='offset') + 
  #coord_cartesian(ylim = c(0.9,1.05))+

  ggtitle('') +
  theme(axis.text.x = element_text(size=20,angle = 45, vjust=1, hjust=1),
        axis.text.y = element_text(size=25),
        axis.title=element_text(size=25),
        plot.title = element_text(size=25,hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"))
f

ggsave(file=paste0('/Users/orlacamus/Downloads/offset.svg'), plot=f, width=4, height=6)#










