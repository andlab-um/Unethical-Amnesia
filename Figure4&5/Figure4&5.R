
# Install required packages if necessary:
packages = c("data.table", "lme4", "Matrix", "stargazer","coxme","survival",
             "bdsmatrix","ez","ggplot2","lmerTest","dplyr",'yarrr','gridExtra',
             'ggsignif','ggpubr','patchwork','readxl','reticulate','Hmisc','ggnewscale',
             'ggcorrplot2','RColorBrewer', 'igraph', 'car', 'beeswarm', 'plotrix','rockchalk',
             'plotly','hrbrthemes','svglite','raveio','rstatix')
newpackages = packages[!(packages %in% installed.packages()[,"Package"])]
if(length(newpackages)) install.packages(newpackages)
loaded = lapply(packages, require, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
cat(paste('## ', sum(unlist(loaded)), ' out of ', length(unlist(loaded)) ,' packages loaded\n', sep = ''))


setwd("/Users/orlacamus/Desktop/projects/ua-rep-only/5.geometry")
subList=c(101,102,103,105,106,107,108,109,110,111,112,115,116,
       117,118,119,120,121,123,125,127,128,129,130,131,132,
       133,134,135,136,137,138,139,140,141,142,143)


ROIs=c('BA34-hip','caudate','NAcc','BA33-rACC')
## separation score
# hippocampus: i=8; 22,27,caudate: i=33
for (i in c(1:length(ROIs))){
  temp=read_excel("separation.xlsx",sheet=ROIs[i])
  temp=temp[,2:4]
  colnames(temp)=c('islie','con','mon')
  temp['subjno']=subList
  long=temp %>% tidyr::pivot_longer(cols=-'subjno')
  colnames(long)=c('subjno','var','score')
  agg <- long %>% group_by(var) %>% dplyr::summarise(mean = mean(score),sd=sd(score),n = n(),se=sd(score)/sqrt(n))
  stat.test <- long %>%
    group_by(var) %>%
    t_test(score ~ 1, mu = 0,alternative='greater') %>%
    mutate(y.position = 0.15)
  print(ROIs[i])
  print(stat.test)
  v=factor(agg$var,c('islie','con','mon'))
  vs=factor(long$var,c('islie','con','mon'))
  f=ggplot(data = agg, aes(x = factor(v), y=mean)) + 
    #geom_bar(data = agg, aes(x = factor(model), y=mean),stat = "identity",width=0.8,fill='#CDC1FF',color='#CDC1FF') +
    geom_bar(data = agg, aes(x = factor(v), y=mean),stat = "identity",width=0.8,fill='white',color='black') +
    geom_jitter(data = long, aes(x = factor(vs), y=score,fill=vs,color=vs),size=2, width=0.15,shape=21,alpha=0.3) +
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
  
  ggsave(file=paste0('/Users/orlacamus/Downloads/separation-',ROIs[i],".svg"), plot=f, width=5, height=5)
}


###### separation score correlation with memory 

##separation
# with meta
for (i in c(1:length(ROIs))){
  temp=read_excel("separation.xlsx",sheet=ROIs[i])
  temp=temp[,2:4]
  colnames(temp)=c('islie','con','mon')
  temp['subjno']=subList
  long=temp %>% tidyr::pivot_longer(cols=-'subjno')
  colnames(long)=c('subjno','var','score')
  
  for (var in c('islie','con','mon')){
    corr=cor.test(temp[[var]],change1[change1$condition=='enhance_lie',]$meta)#
    df <- data.frame(temp[[var]],change1[change1$condition=='enhance_lie',]$meta)#
    #corr=cor.test(temp[[var]],agg_all$entropy)#
    #df <- data.frame(temp[[var]],agg_all$entropy)#
    colnames(df)=c(var,"change")
    if (corr$p.value<0.05){
      print(i)
      print(ROIs[i])
      print(var)
      print(corr)
      if (var=='islie'){
        clr='dark red'
      }
      if (var=='con'){
        clr='#578737'
      }
      if (var=='mon'){
        clr='#70309f'
      }
      tempdata=data.frame(temp[[var]])
      tempdata$change=change1[change1$condition=='enhance_lie',]$meta
      colnames(tempdata)[1]=var
      pred <- predict(lm(paste0("change~",var), tempdata), 
                      se.fit = TRUE, interval = "confidence")
      limits <- as.data.frame(pred$fit)
  #
      f=ggplot(data=df,aes_string(x=var,y='change'))+
        geom_point(size=3,colour=clr,fill=clr,alpha=0.6)+
        geom_smooth(method='lm',color=clr,se=F,linewidth=2)+
        geom_line(aes(x=.data[[var]],y = limits$lwr), 
                  linetype = 2,color=clr) +
        geom_line(aes(x=.data[[var]],y = limits$upr), 
                  linetype = 2,color=clr) +
        scale_y_continuous(name='metacognition change')+
        scale_x_continuous(name=paste0(ROIs[i],'-',var,'score'))+
        #stat_cor(method = "pearson",label.x.npc = 0.2,label.y.npc=0.2,size=5,color='black')+
        theme(axis.text.x = element_text(size=18),
              axis.text.y = element_text(size=25),
              axis.title=element_text(size=25),
              plot.title = element_text(size=1,hjust = 0.5),
              panel.background = element_rect(fill = "white"),
              axis.line = element_line(colour = "black"))
      ggsave(file=paste0('/Users/orlacamus/Downloads/',ROIs[i],'-',var,".svg"), plot=f, width=5, height=5)#
    }
  }
  
  
}





# parameterized model parameters: selected ROIs

all=read.csv("parameterized-results.csv")

all_temp=all
long=data.frame(all) %>% tidyr::pivot_longer(cols=-c('subj','ROI'))

compr_long=long[(long['name']!='rot' & long['name']!='offset'),]

agg <- compr_long %>% group_by(name,ROI) %>% dplyr::summarise(mean = mean(value),sd=sd(value),n = n(),se=sd(value)/sqrt(n))
stat.test <- compr_long %>%
  group_by(name,ROI) %>%
  t_test(value ~ 1, mu = 0) %>%
  mutate(y.position = 0.2)
write.csv(stat.test,'geometry_results.csv')


new_ROIs=c('amyg','rTPJ','ventral_striatum','putamen','rDLPFC','lDLPFC')
agg=agg[agg$ROI %in% new_ROIs,]
ROI_order=factor(agg$ROI,level=new_ROIs)
f=ggplot(data = agg, aes(x = ROI_order, y=mean,group=ROI)) + 
  geom_hline(yintercept = 0,color='black',size=1,linetype='dashed')+
  geom_line(data = agg, aes(x=interaction(name,ROI_order),y=mean,group=ROI_order),color='gray70')+
  geom_point(data = agg, aes(x = interaction(name,ROI_order), y=mean,color=name),size=5,position = position_dodge(width = 0.8)) +
  #stat_pvalue_manual(stat.test, label = "p", xmin = "name", xmax = NULL)+
  
  scale_color_manual(values=c("#80b918",'#007f5f')) +
  geom_errorbar(alpha=0.8,width=0.4,size=1,data = agg,aes(x = interaction(name,ROI_order),ymin=mean-se, ymax=mean+se,color=name),position = position_dodge(width = 0.8)) +
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
ggsave(file=paste0('/Users/orlacamus/Downloads/compr_rep.svg'), plot=f, width=7, height=5)





# parameter correlation: prefchange/memchange1/memchang2

allchange1=item_change1 %>% group_by(subject_nr) %>% 
  summarise(acc = mean(acc),
            conf = mean(conf),
            meta = mean(meta),
            AUC = mean(AUC),
            RT=mean(RT),
            #MAD = mean(MAD),
            #AD=mean(AD)
  )
ROIs=new_ROIs
for (i in c(1:length(ROIs))){
  temp=all[all[,'ROI']==ROIs[i],]
  
  #for (var in c('conlie','monlie','conhon','monhon','rot','offset')){
  for (var in c('lie')){
    corr=cor.test(temp[[var]],change1[change1$condition=='enhance_lie',]$meta)#
    df <- data.frame(temp[[var]], change1[change1$condition=='enhance_lie',]$meta)#
    #corr=cor.test(temp[[var]],allchange1$meta)#
    #df <- data.frame(temp[[var]], allchange1$meta)#
    colnames(df)=c(var,'change')
    if (corr$p.value<0.05){
      print(i)
      print(ROIs[i])
      print(var)
      print(corr)
      f=ggplot(data=df,aes_string(x=var,y='change'))+
        geom_point(color='#007f5f75',size=3)+
        geom_smooth(method='lm',color='#007f5f',se=T,linewidth=2,fill='#007f5f50')+
        scale_y_continuous(name='metacognition change')+
        scale_x_continuous(name=paste0(ROIs[i],'_compression param-',var))+
        #stat_cor(method = "pearson",label.x.npc = 0.2,label.y.npc=0.2,size=5,color='black')+
        theme(axis.text.x = element_text(size=26),
              axis.text.y = element_text(size=26),
              axis.title=element_text(size=25),
              plot.title = element_text(size=1,hjust = 0.5),
              panel.background = element_rect(fill = "white"),
              axis.line = element_line(colour = "black"))
        
      ggsave(file=paste0('/Users/orlacamus/Downloads/compcorr-memchange1-',ROIs[i],'-',var,".svg"), plot=f, width=6, height=6)#
    }
  }
  
  
}
