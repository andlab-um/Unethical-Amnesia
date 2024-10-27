
setwd("/Users/orlacamus/Desktop/projects/ua-rep-only/data")

# load packages
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

# read the data
memory=fread("memory_summary_rep.csv") # including time-out trials
fmridata=fread('fmribehav_summary.csv') # including time-out trials

memory = memory[memory$subject_nr %in% subList,]
memory = memory[memory$rt!='None',]
memory$correct=memory$accuracy
memory[memory$correct==0,]$correct=-1
memory$meta=1-(memory$accuracy-memory$confidence/100)^2



fmridata=fmridata[fmridata$subj %in% subList,]
fmridata=fmridata[fmridata$rt<=4,]




## entropy ~ condition

agg_menu <- fmridata %>% group_by(subj,menu,condition) %>% 
  summarise(lierate = mean(islie),
            scan_rt = mean(as.numeric(rt)),
            reward_sign = mean(reward_sign),
            reward_gain_sum=sum(reward_gain)
  )

agg_menu$entropy=-agg_menu$lierate*log2(agg_menu$lierate)-(1-agg_menu$lierate)*log2(1-agg_menu$lierate)
agg_menu[agg_menu$lierate==1,]$entropy=0
agg_menu[agg_menu$lierate==0,]$entropy=0


agg_entropy=agg_menu %>% group_by(subj,condition) %>% 
  summarise(lie_entropy = mean(entropy),
            lierate=mean(lierate)) 
a=aov(lie_entropy ~ condition, data = agg_entropy)
summary(a)
etaSquared(a)
a=aov(lierate ~ condition, data = agg_entropy)
summary(a)
etaSquared(a)

t.test(agg_entropy[agg_entropy$condition=='enhance_lie',]$lie_entropy,agg_entropy[agg_entropy$condition=='enhance_honesty',]$lie_entropy,paired = TRUE)
t.test(agg_entropy[agg_entropy$condition=='enhance_random',]$lie_entropy,agg_entropy[agg_entropy$condition=='enhance_honesty',]$lie_entropy,paired = TRUE)
t.test(agg_entropy[agg_entropy$condition=='enhance_lie',]$lie_entropy,agg_entropy[agg_entropy$condition=='enhance_random',]$lie_entropy,paired = TRUE)

t.test(agg_entropy[agg_entropy$condition=='enhance_lie',]$lierate,agg_entropy[agg_entropy$condition=='enhance_honesty',]$lierate,paired = TRUE)
t.test(agg_entropy[agg_entropy$condition=='enhance_random',]$lierate,agg_entropy[agg_entropy$condition=='enhance_honesty',]$lierate,paired = TRUE)
t.test(agg_entropy[agg_entropy$condition=='enhance_lie',]$lierate,agg_entropy[agg_entropy$condition=='enhance_random',]$lierate,paired = TRUE)


sum_entropy=agg_entropy %>% group_by(subj,condition) %>% 
  summarise(lie_entropy = mean(lie_entropy),lierate=mean(lierate)) %>% 
  group_by(condition) %>% 
  summarise(entropy = mean(lie_entropy),
            sd = sd(lie_entropy),
            n = n(),
            se = sd / sqrt(n),
            lierate = mean(lierate),
            sd2 = sd(lierate),
            n = n(),
            se2 = sd2 / sqrt(n))

c=factor(sum_entropy$condition,c("enhance_lie","enhance_honesty",'enhance_random'))
conditions=c("enhance_lie","enhance_honesty",'enhance_random')
comp_colors = c('#e7b13e','#c66236','#2a74a3')#!!!!
f = ggplot(data = sum_entropy, aes(x = c, y=entropy,fill=c)) + 
  geom_bar(position = position_dodge(0.8),stat = "identity",width=0.8)  + 
  scale_fill_manual(values=comp_colors)+
  geom_jitter(data=agg_entropy,aes(x=factor(condition,levels = conditions),y = lie_entropy,fill=factor(condition,levels = conditions)),
              size=5,width = 0, shape=21) +
  geom_errorbar(width=0.2, size=1,
                aes(ymin=entropy-se, ymax=entropy+se),
                position=position_dodge(0.8)) +
  
  scale_x_discrete(name='condition',labels=c('elie','ehon','random')) + # have tick marks for each session
  scale_y_continuous(expand=c(0,0),name='response entropy') +
  coord_cartesian(ylim = c(-0.1,1))+
  geom_hline(yintercept=-0.1, linetype="dashed",color = "gray", size=2)+
  #ggtitle("") +
  theme(axis.text.x = element_text(size=25),
        axis.text.y = element_text(size=20),
        axis.title=element_text(size=25),
        text = element_text(size=25,family ='Helvetica'),
        axis.ticks.length=unit(0.25, "cm"),
        axis.line.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(size=25,hjust = 0.5),
        legend.position="none",
        legend.text = element_text(family ='Helvetica'),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
f

ggsave(file=paste0('/Users/orlacamus/Downloads/ent_rep.svg'), plot=f, width=5, height=5)


f = ggplot(data = sum_entropy, aes(x = c, y=lierate,fill=c)) + 
  geom_bar(position = position_dodge(0.8),stat = "identity",width=0.8)  + 
  scale_fill_manual(values=comp_colors)+
  geom_jitter(data=agg_entropy,aes(x=factor(condition,levels = conditions),y = lierate,fill=factor(condition,levels = conditions)),
              size=5,width = 0, shape=21) +
  geom_errorbar(width=0.2, size=1,
                aes(ymin=lierate-se2, ymax=lierate+se),
                position=position_dodge(0.8)) +
  
  scale_x_discrete(name='condition',labels=c('elie','ehon','random')) + # have tick marks for each session
  scale_y_continuous(expand=c(0,0),name='lie rate') +
  coord_cartesian(ylim = c(-0.1,1))+
  geom_hline(yintercept=-0.1, linetype="dashed",color = "gray", size=2)+
  #ggtitle("") +
  theme(axis.text.x = element_text(size=25),
        axis.text.y = element_text(size=20),
        axis.title=element_text(size=25),
        text = element_text(size=25,family ='Helvetica'),
        axis.ticks.length=unit(0.25, "cm"),
        axis.line.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(size=25,hjust = 0.5),
        legend.position="none",
        legend.text = element_text(family ='Helvetica'),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
f

ggsave(file=paste0('/Users/orlacamus/Downloads/lierate_rep.svg'), plot=f, width=5, height=5)


# IST data aggregation
agg_all <- agg_menu %>% group_by(subj) %>% 
  summarise(lierate = mean(lierate),
            sacn_rt = mean(as.numeric(scan_rt)),
            entropy = mean(entropy)
  )
agg_all_sum <- fmridata %>% group_by(subj) %>% 
  summarise(reward_gain_sum = sum(reward_gain)
  )
agg_all$reward_gain_sum=agg_all_sum$reward_gain_sum


agg_cond <- agg_menu %>% group_by(subj,condition) %>% 
  summarise(lierate = mean(lierate),
            scan_rt = mean(as.numeric(scan_rt)),
            entropy = mean(entropy)
  )
agg_cond_sum <- fmridata %>% group_by(subj,condition) %>% 
  summarise(reward_gain_sum = sum(reward_gain)
  )
agg_cond$reward_gain_sum=agg_cond_sum$reward_gain_sum


# memory data aggregation

item_agg = memory %>% group_by(subject_nr,menu, stage,condition) %>% 
  summarise(acc = mean(accuracy),
            conf = mean(confidence),
            meta = mean(meta),
            AUC = mean(AUC),
            RT=mean(RT))



item_agg_pre=item_agg[item_agg$stage=='pre',]
item_agg_post=item_agg[item_agg$stage=='post',]
item_agg_post2=item_agg[item_agg$stage=='post2',]

item_change1=item_agg_pre
item_change1$meta=item_agg_pre$meta-item_agg_post$meta
item_change1$conf=item_agg_pre$conf-item_agg_post$conf
item_change1$acc=item_agg_pre$acc-item_agg_post$acc
item_change1$RT=item_agg_pre$RT-item_agg_post$RT
item_change1$AUC=item_agg_pre$AUC-item_agg_post$AUC
#item_change1$MAD=item_agg_pre$MAD-item_agg_post$MAD
#item_change1$AD=item_agg_pre$AD-item_agg_post$AD


item_change2=item_agg_pre
item_change2$meta=item_agg_pre$meta-item_agg_post2$meta
item_change2$conf=item_agg_pre$conf-item_agg_post2$conf
item_change2$acc=item_agg_pre$acc-item_agg_post2$acc
item_change2$RT=item_agg_pre$RT-item_agg_post2$RT
item_change2$AUC=item_agg_pre$AUC-item_agg_post2$AUC
#item_change2$MAD=item_agg_pre$MAD-item_agg_post2$MAD
#item_change2$AD=item_agg_pre$AD-item_agg_post2$AD

item_change3=item_agg_post
item_change3$meta=item_agg_post$meta-item_agg_post2$meta
item_change3$conf=item_agg_post$conf-item_agg_post2$conf
item_change3$acc=item_agg_post$acc-item_agg_post2$acc
item_change3$RT=item_agg_post$RT-item_agg_post2$RT
item_change3$AUC=item_agg_post$AUC-item_agg_post2$AUC





change1 <- item_change1 %>% group_by(subject_nr,condition) %>% 
  summarise(acc = mean(acc),
            conf = mean(conf),
            meta = mean(meta),
            AUC = mean(AUC),
            RT=mean(RT),
            #MAD = mean(MAD),
            #AD=mean(AD)
  )

change1['session']='pre-post'


change2 <- item_change2  %>% group_by(subject_nr,condition) %>% 
  summarise(acc = mean(acc),
            conf = mean(conf),
            meta = mean(meta),
            AUC = mean(AUC),
            RT=mean(RT),
            #MAD = mean(MAD),
            #AD=mean(AD)
  )

change2['session']='pre-post2'

change3 <- item_change3  %>% group_by(subject_nr,condition) %>% 
  summarise(acc = mean(acc),
            conf = mean(conf),
            meta = mean(meta),
            AUC = mean(AUC),
            RT=mean(RT),
            #MAD = mean(MAD),
            #AD=mean(AD)
  )

change3['session']='post-post2'
change=rbind(change1,change2)


# plot basic features: pre & post memory tests

agg_pre=item_agg_pre %>% group_by(condition,subject_nr) %>% 
  summarise(acc = mean(acc),
            conf = mean(conf),
            meta = mean(meta),
            AUC = mean(AUC),
            RT=mean(RT),
  )
agg_pre$stage='pre'

agg_post=item_agg_post%>% group_by(condition,subject_nr) %>% 
  summarise(acc = mean(acc),
            conf = mean(conf),
            meta = mean(meta),
            AUC = mean(AUC),
            RT=mean(RT),
  )
agg_post$stage='post'

agg_post2=item_agg_post2 %>% group_by(condition,subject_nr) %>% 
  summarise(acc = mean(acc),
            conf = mean(conf),
            meta = mean(meta),
            AUC = mean(AUC),
            RT=mean(RT),
  )
agg_post2$stage='post2'
agg_merge=rbind(agg_pre,agg_post,agg_post2)


# all data
sum_pre <- agg_pre %>% 
  group_by(condition) %>% 
  summarise(acc_mean = mean(acc),
            auc_mean = mean(AUC),
            rt_mean = mean(RT),
            conf_mean = mean(conf),
            meta_mean = mean(meta),
            
            acc_sd = sd(acc),
            auc_sd = sd(AUC),
            rt_sd = sd(RT),
            conf_sd = sd(conf),
            meta_sd = sd(meta),
            
            n = n(),
            acc_se = acc_sd / sqrt(n),
            auc_se = auc_sd / sqrt(n),
            rt_se = rt_sd / sqrt(n),
            conf_se = conf_sd / sqrt(n),
            meta_se = meta_sd / sqrt(n),) 

sum_pre['session']='pre'


sum_post <- agg_post %>% 
  group_by(condition) %>% 
  summarise(acc_mean = mean(acc),
            auc_mean = mean(AUC),
            #mad_mean = mean(MAD),
            #ad_mean = mean(AD),
            rt_mean = mean(RT),
            conf_mean = mean(conf),
            meta_mean = mean(meta),
            
            acc_sd = sd(acc),
            auc_sd = sd(AUC),
            #mad_sd = sd(MAD),
            #ad_sd = sd(AD),
            rt_sd = sd(RT),
            conf_sd = sd(conf),
            meta_sd = sd(meta),
            
            n = n(),
            acc_se = acc_sd / sqrt(n),
            auc_se = auc_sd / sqrt(n),
            #mad_se = mad_sd / sqrt(n),
            #ad_se = ad_sd / sqrt(n),
            rt_se = rt_sd / sqrt(n),
            conf_se = conf_sd / sqrt(n),
            meta_se = meta_sd / sqrt(n),) 


sum_post['session']='post'

sum_post2 <- agg_post2 %>% 
  group_by(condition) %>% 
  summarise(acc_mean = mean(acc),
            auc_mean = mean(AUC),
            #mad_mean = mean(MAD),
            #ad_mean = mean(AD),
            rt_mean = mean(RT),
            conf_mean = mean(conf),
            meta_mean = mean(meta),
            
            acc_sd = sd(acc),
            auc_sd = sd(AUC),
            #mad_sd = sd(MAD),
            #ad_sd = sd(AD),
            rt_sd = sd(RT),
            conf_sd = sd(conf),
            meta_sd = sd(meta),
            
            n = n(),
            acc_se = acc_sd / sqrt(n),
            auc_se = auc_sd / sqrt(n),
           # mad_se = mad_sd / sqrt(n),
            #ad_se = ad_sd / sqrt(n),
            rt_se = rt_sd / sqrt(n),
            conf_se = conf_sd / sqrt(n),
            meta_se = meta_sd / sqrt(n),) 



sum_post2['session']='post2'

sum_merge=rbind(sum_pre,sum_post,sum_post2)

## plot

comp_colors = c('#e7b13e','#c66236','#2a74a3','#e7b13e80','#c6623680','#2a74a380','#e7b13e50','#c6623650','#2a74a350')#!!!!


c=factor(sum_merge$condition,c("enhance_lie","enhance_honesty",'enhance_random'))
s=factor(sum_merge$session,c("pre","post",'post2'))
c_dot=factor(agg_merge$condition,c("enhance_lie","enhance_honesty",'enhance_random'))



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
ggsave(file=paste0('/Users/orlacamus/Downloads/meta.svg'), plot=f, width=5, height=6)

### acc
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
ggsave(file=paste0('/Users/orlacamus/Downloads/acc.svg'), plot=f, width=5, height=6)



