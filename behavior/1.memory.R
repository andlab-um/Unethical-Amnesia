
rm(list=ls())
setwd("/Users/orlacamus/Desktop/projects/unethical_amnesia/analysisCode/data")

want = c("data.table", "lme4", "Matrix", "stargazer","coxme","survival","bdsmatrix","ez","ggplot2","lmerTest","dplyr",'yarrr','gridExtra','ggsignif','ggpubr','patchwork')
have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }
# Now load them all
lapply(want, require, character.only = TRUE)


packages = c('RColorBrewer', 'igraph', 'lme4', 'car', 'beeswarm', 'plotrix','rockchalk')
newpackages = packages[!(packages %in% installed.packages()[,"Package"])]
if(length(newpackages)) install.packages(newpackages)
loaded = lapply(packages, require, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
cat(paste('## ', sum(unlist(loaded)), ' out of ', length(unlist(loaded)) ,' packages loaded\n', sep = ''))






#read the data
memory=fread("memory_summary.csv")


# 
subList = c(102, 103, 105, 107, 108, 110, 111, 112, 113, 114,
            115, 116, 117, 118, 119, 120, 121, 122, 123, 125, 127, 128,
            129, 130, 131, 132)
#subList = c(102, 103, 105, 107, 108, 110, 111, 112, 113, 114,
#            115, 116, 117, 119, 120, 121, 122, 123, 125, 127, 128,
#            130, 131)

memory = memory[memory$subject_nr %in% subList,]

memory = memory[memory$rt!='None',]



memory$correct=memory$accuracy
memory[memory$correct==0,]$correct=-1
memory$meta=1-(memory$accuracy-memory$confidence/100)^2
memory$AUC=max(memory$AUC)-(max(memory$AUC)-max(memory$AUC)*memory$accuracy-memory$AUC)^2/max(memory$AUC)
memory$RT=4-(4-memory$accuracy*4-memory$RT/1000)^2/4
memory$MAD=max(memory$MAD)-(max(memory$MAD)-max(memory$MAD)*memory$accuracy-memory$MAD)^2/max(memory$MAD)
memory$AD=max(memory$AD)-(max(memory$AD)-max(memory$AD)*memory$accuracy-memory$AD)^2/max(memory$AD)

#####


item_agg = memory %>% group_by(subject_nr,menu, stage,condition) %>% 
  summarise(acc = mean(accuracy),
            conf = mean(confidence),
            meta = mean(meta),
            AUC = mean(AUC),
            RT=mean(RT),
            MAD=mean(MAD),
            AD=mean(AD)
  )

item_agg_pre=item_agg[item_agg$stage=='pre',]
item_agg_post=item_agg[item_agg$stage=='post',]
item_agg_post2=item_agg[item_agg$stage=='post2',]

item_change1=item_agg_pre
item_change1$meta=item_agg_pre$meta-item_agg_post$meta
item_change1$conf=item_agg_pre$conf-item_agg_post$conf
item_change1$acc=item_agg_pre$acc-item_agg_post$acc
item_change1$RT=item_agg_pre$RT-item_agg_post$RT
item_change1$AUC=item_agg_pre$AUC-item_agg_post$AUC
item_change1$MAD=item_agg_pre$MAD-item_agg_post$MAD
item_change1$AD=item_agg_pre$AD-item_agg_post$AD

item_change2=item_agg_pre
item_change2$meta=item_agg_pre$meta-item_agg_post2$meta
item_change2$conf=item_agg_pre$conf-item_agg_post2$conf
item_change2$acc=item_agg_pre$acc-item_agg_post2$acc
item_change2$RT=item_agg_pre$RT-item_agg_post2$RT
item_change2$AUC=item_agg_pre$AUC-item_agg_post2$AUC
item_change2$MAD=item_agg_pre$MAD-item_agg_post2$MAD
item_change2$AD=item_agg_pre$AD-item_agg_post2$AD
####

change1 <- item_change1 %>% group_by(subject_nr,condition) %>% 
  summarise(acc = mean(acc),
            conf = mean(conf),
            meta = mean(meta),
            AUC = mean(AUC),
            RT=mean(RT),
            MAD = mean(MAD),
            AD=mean(AD)
  )

change1['session']='pre-post'

change2 <- item_change2  %>% group_by(subject_nr,condition) %>% 
  summarise(acc = mean(acc),
            conf = mean(conf),
            meta = mean(meta),
            AUC = mean(AUC),
            RT=mean(RT),
            MAD = mean(MAD),
            AD=mean(AD)
  )

change2['session']='pre-post2'



change=rbind(change1,change2)




##
agg_pre=item_agg_pre %>% group_by(condition,subject_nr) %>% 
  summarise(acc = mean(acc),
            conf = mean(conf),
            meta = mean(meta),
            AUC = mean(AUC),
            RT=mean(RT),
            MAD = mean(MAD),
            AD=mean(AD)
  )

agg_pre$stage='pre'
agg_post=item_agg_post%>% group_by(condition,subject_nr) %>% 
  summarise(acc = mean(acc),
            conf = mean(conf),
            meta = mean(meta),
            AUC = mean(AUC),
            RT=mean(RT),
            MAD = mean(MAD),
            AD=mean(AD)
  )

agg_post$stage='post'
agg_post2=item_agg_post2 %>% group_by(condition,subject_nr) %>% 
summarise(acc = mean(acc),
          conf = mean(conf),
          meta = mean(meta),
          AUC = mean(AUC),
          RT=mean(RT),
          MAD = mean(MAD),
          AD=mean(AD)
)

agg_post2$stage='post2'
agg_merge=rbind(agg_pre,agg_post,agg_post2)



#
sum_pre <- agg_pre %>% 
  group_by(condition) %>% 
  summarise(acc_mean = mean(acc),
            auc_mean = mean(AUC),
            mad_mean = mean(MAD),
            ad_mean = mean(AD),
            rt_mean = mean(RT),
            conf_mean = mean(conf),
            meta_mean = mean(meta),
            
            acc_sd = sd(acc),
            auc_sd = sd(AUC),
            mad_sd = sd(MAD),
            ad_sd = sd(AD),
            rt_sd = sd(RT),
            conf_sd = sd(conf),
            meta_sd = sd(meta),
            
            n = n(),
            acc_se = acc_sd / sqrt(n),
            auc_se = auc_sd / sqrt(n),
            mad_se = mad_sd / sqrt(n),
            ad_se = ad_sd / sqrt(n),
            rt_se = rt_sd / sqrt(n),
            conf_se = conf_sd / sqrt(n),
            meta_se = meta_sd / sqrt(n),) 
  
sum_pre['session']='pre'


sum_post <- agg_post %>% 
  group_by(condition) %>% 
  summarise(acc_mean = mean(acc),
            auc_mean = mean(AUC),
            mad_mean = mean(MAD),
            ad_mean = mean(AD),
            rt_mean = mean(RT),
            conf_mean = mean(conf),
            meta_mean = mean(meta),
            
            acc_sd = sd(acc),
            auc_sd = sd(AUC),
            mad_sd = sd(MAD),
            ad_sd = sd(AD),
            rt_sd = sd(RT),
            conf_sd = sd(conf),
            meta_sd = sd(meta),
            
            n = n(),
            acc_se = acc_sd / sqrt(n),
            auc_se = auc_sd / sqrt(n),
            mad_se = mad_sd / sqrt(n),
            ad_se = ad_sd / sqrt(n),
            rt_se = rt_sd / sqrt(n),
            conf_se = conf_sd / sqrt(n),
            meta_se = meta_sd / sqrt(n),) 


sum_post['session']='post'

sum_post2 <- agg_post2 %>% 
  group_by(condition) %>% 
  summarise(acc_mean = mean(acc),
            auc_mean = mean(AUC),
            mad_mean = mean(MAD),
            ad_mean = mean(AD),
            rt_mean = mean(RT),
            conf_mean = mean(conf),
            meta_mean = mean(meta),
            
            acc_sd = sd(acc),
            auc_sd = sd(AUC),
            mad_sd = sd(MAD),
            ad_sd = sd(AD),
            rt_sd = sd(RT),
            conf_sd = sd(conf),
            meta_sd = sd(meta),
            
            n = n(),
            acc_se = acc_sd / sqrt(n),
            auc_se = auc_sd / sqrt(n),
            mad_se = mad_sd / sqrt(n),
            ad_se = ad_sd / sqrt(n),
            rt_se = rt_sd / sqrt(n),
            conf_se = conf_sd / sqrt(n),
            meta_se = meta_sd / sqrt(n),) 



sum_post2['session']='post2'

sum_merge=rbind(sum_pre,sum_post,sum_post2)

##################################################

library("ggnewscale")


comp_colors = c('#e7b13e','#c66236','#2a74a3','#e7b13e80','#c6623680','#2a74a380','#e7b13e50','#c6623650','#2a74a350')#!!!!

c=factor(sum_merge$condition,c("enhance_lie","enhance_honesty",'enhance_random'))
s=factor(sum_merge$session,c("pre","post",'post2'))
c_dot=factor(agg_merge$condition,c("enhance_lie","enhance_honesty",'enhance_random'))



### AUC
f = ggplot(data = sum_merge, aes(x = c, y=auc_mean,fill=interaction(c,s))) + 
  geom_bar(position = position_dodge(0.8),stat = "identity",width=0.8)  + 
  scale_fill_manual(values=comp_colors)+
  geom_errorbar(width=0.2, size=1,
                aes(ymin=auc_mean-auc_se, ymax=auc_mean+auc_se),
                position=position_dodge(0.8)) +
  scale_x_discrete(name='condition',labels=c('elie','ehon','random')) + # have tick marks for each session
  scale_y_continuous(expand=c(0,0),name='AUC') +
  coord_cartesian(ylim = c(1.5,1.75))+
  geom_hline(yintercept=1.5, linetype="dashed",color = "gray", size=2)+
  #ggtitle("") +
  theme(axis.text.x = element_text(size=25),
        axis.text.y = element_text(size=20),
        axis.title=element_text(size=25),
        text = element_text(size=25,family ='Helvetica'),
        axis.ticks.length=unit(0.25, "cm"),
        axis.line.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(size=25,hjust = 0.5),
        legend.position="bottom",
        legend.text = element_text(family ='Helvetica'),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
f
ggsave(file=paste0('/Users/orlacamus/Downloads/AUC.svg'), plot=f, width=7, height=7)

### MAD
f = ggplot(data = sum_merge, aes(x = c, y=mad_mean,fill=interaction(c,s))) + 
  geom_bar(position = position_dodge(0.8),stat = "identity",width=0.8)  + 
  scale_fill_manual(values=comp_colors)+
  geom_errorbar(width=0.2, size=1,
                aes(ymin=mad_mean-mad_se, ymax=mad_mean+mad_se),
                position=position_dodge(0.8)) +
  scale_x_discrete(name='condition',labels=c('elie','ehon','random')) + # have tick marks for each session
  scale_y_continuous(expand=c(0,0),name='MAD') +
  coord_cartesian(ylim = c(1,1.4))+
  geom_hline(yintercept=1, linetype="dashed",color = "gray", size=2)+
  #ggtitle("") +
  theme(axis.text.x = element_text(size=25),
        axis.text.y = element_text(size=20),
        axis.title=element_text(size=25),
        text = element_text(size=25,family ='Helvetica'),
        axis.ticks.length=unit(0.25, "cm"),
        axis.line.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(size=25,hjust = 0.5),
        legend.position="bottom",
        legend.text = element_text(family ='Helvetica'),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
f
ggsave(file=paste0('/Users/orlacamus/Downloads/MAD.svg'), plot=f, width=7, height=7)

### AD
f = ggplot(data = sum_merge, aes(x = c, y=ad_mean,fill=interaction(c,s))) + 
  geom_bar(position = position_dodge(0.8),stat = "identity",width=0.8)  + 
  scale_fill_manual(values=comp_colors)+
  geom_errorbar(width=0.2, size=1,
                aes(ymin=ad_mean-ad_se, ymax=ad_mean+ad_se),
                position=position_dodge(0.8)) +
  scale_x_discrete(name='condition',labels=c('elie','ehon','random')) + # have tick marks for each session
  scale_y_continuous(expand=c(0,0),name='AD') +
  coord_cartesian(ylim = c(0.6,1))+
  geom_hline(yintercept=0.6, linetype="dashed",color = "gray", size=2)+
  #ggtitle("") +
  theme(axis.text.x = element_text(size=25),
        axis.text.y = element_text(size=20),
        axis.title=element_text(size=25),
        text = element_text(size=25,family ='Helvetica'),
        axis.ticks.length=unit(0.25, "cm"),
        axis.line.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(size=25,hjust = 0.5),
        legend.position="bottom",
        legend.text = element_text(family ='Helvetica'),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
f
ggsave(file=paste0('/Users/orlacamus/Downloads/MD.svg'), plot=f, width=7, height=7)


### rt
f = ggplot(data = sum_merge, aes(x = c, y=rt_mean,fill=interaction(c,s))) + 
  geom_bar(position = position_dodge(0.8),stat = "identity",width=0.8)  + 
  scale_fill_manual(values=comp_colors)+
  geom_errorbar(width=0.2, size=1,
                aes(ymin=rt_mean-rt_se, ymax=rt_mean+rt_se),
                position=position_dodge(0.8)) +
  scale_x_discrete(name='condition',labels=c('elie','ehon','random')) + # have tick marks for each session
  scale_y_continuous(expand=c(0,0),name='RT') +
  coord_cartesian(ylim = c(3,4))+
  geom_hline(yintercept=3, linetype="dashed",color = "gray", size=2)+
  #ggtitle("") +
  theme(axis.text.x = element_text(size=25),
        axis.text.y = element_text(size=20),
        axis.title=element_text(size=25),
        text = element_text(size=25,family ='Helvetica'),
        axis.ticks.length=unit(0.25, "cm"),
        axis.line.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(size=25,hjust = 0.5),
        legend.position="bottom",
        legend.text = element_text(family ='Helvetica'),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
f
ggsave(file=paste0('/Users/orlacamus/Downloads/RT.svg'), plot=f, width=7, height=7)

### confidence
comp_colors = c('gray10','gray10','gray10','gray40','gray40','gray40','gray70','gray70','gray70')
f = ggplot(data = sum_merge, aes(x = c, y=conf_mean,fill=interaction(c,s))) + 
  geom_bar(position = position_dodge(0.8),stat = "identity",width=0.8)  + 
  scale_fill_manual(values=comp_colors)+
  geom_errorbar(width=0.2, size=1,
                aes(ymin=conf_mean-conf_se, ymax=conf_mean+conf_se),
                position=position_dodge(0.8)) +
  scale_x_discrete(name='condition',labels=c('elie','ehon','random')) + # have tick marks for each session
  scale_y_continuous(expand=c(0,0),name='self-reported confidence') +
  coord_cartesian(ylim = c(75,100))+
  geom_hline(yintercept=75, linetype="dashed",color = "gray", size=2)+
  #ggtitle("") +
  theme(axis.text.x = element_text(size=25),
        axis.text.y = element_text(size=20),
        axis.title=element_text(size=25),
        text = element_text(size=25,family ='Helvetica'),
        axis.ticks.length=unit(0.25, "cm"),
        axis.line.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(size=25,hjust = 0.5),
        legend.position="bottom",
        legend.text = element_text(family ='Helvetica'),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
f
ggsave(file=paste0('/Users/orlacamus/Downloads/confidence.svg'), plot=f, width=7, height=7)

### meta
f = ggplot(data = sum_merge, aes(x = c, y=meta_mean,fill=interaction(c,s))) + 
  geom_bar(position = position_dodge(0.8),stat = "identity",width=0.8)  + 
  scale_fill_manual(values=comp_colors)+
  geom_errorbar(width=0.2, size=1,
                aes(ymin=meta_mean-meta_se, ymax=meta_mean+meta_se),
                position=position_dodge(0.8)) +
  scale_x_discrete(name='condition',labels=c('elie','ehon','random')) + # have tick marks for each session
  scale_y_continuous(expand=c(0,0),name='metacognition') +
  coord_cartesian(ylim = c(0.75,1))+
  geom_hline(yintercept=0.75, linetype="dashed",color = "gray", size=2)+
  #ggtitle("") +
  theme(axis.text.x = element_text(size=25),
        axis.text.y = element_text(size=20),
        axis.title=element_text(size=25),
        text = element_text(size=25,family ='Helvetica'),
        axis.ticks.length=unit(0.25, "cm"),
        axis.line.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(size=25,hjust = 0.5),
        legend.position="bottom",
        legend.text = element_text(family ='Helvetica'),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
f
ggsave(file=paste0('/Users/orlacamus/Downloads/meta.svg'), plot=f, width=7, height=7)

### acc
comp_colors = c('gray10','gray10','gray10','gray40','gray40','gray40','gray70','gray70','gray70')
f = ggplot(data = sum_merge, aes(x = c, y=acc_mean,fill=interaction(c,s))) + 
  geom_bar(position = position_dodge(0.8),stat = "identity",width=0.8)  + 
  scale_fill_manual(values=comp_colors)+
  geom_errorbar(width=0.2, size=1,
                aes(ymin=acc_mean-acc_se, ymax=acc_mean+acc_se),
                position=position_dodge(0.8)) +
  scale_x_discrete(name='condition',labels=c('elie','ehon','random')) + # have tick marks for each session
  scale_y_continuous(expand=c(0,0),name='accuracy') +
  coord_cartesian(ylim = c(0.75,1))+
  geom_hline(yintercept=0.75, linetype="dashed",color = "gray", size=2)+
  #ggtitle("") +
  theme(axis.text.x = element_text(size=25),
        axis.text.y = element_text(size=20),
        axis.title=element_text(size=25),
        text = element_text(size=25,family ='Helvetica'),
        axis.ticks.length=unit(0.25, "cm"),
        axis.line.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(size=25,hjust = 0.5),
        legend.position="bottom",
        legend.text = element_text(family ='Helvetica'),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
f
ggsave(file=paste0('/Users/orlacamus/Downloads/acc.svg'), plot=f, width=7, height=7)

