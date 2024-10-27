# Figure 2A & 2B
z=data.frame(c(0,0,7/8,0,6/7,0,6/8,5/6,0,5/7,4/5,0,5/8,4/6,3/4,0,4/7,3/5,2/3,0,1/2,1,1/3,2/5,3/7,1,1/4,2/6,3/8,1,1/5,2/7,1,1/6,2/8,1,1/7,1,1/8,1,1))
colnames(z)=c('lierate')
z$consis=c(8,7,6,6,5,5,4,4,4,3,3,3,2,2,2,2,1,1,1,1,0,-1,-1,-1,-1,-2,-2,-2,-2,-3,-3,-3,-4,-4,-4,-5,-5,-6,-6,-7,-8)
z$ent=-z$lierate*log2(z$lierate)-(1-z$lierate)*log2(1-z$lierate)
#z[z$lierate==1,]$ent=0
#z[z$lierate==0,]$ent=0
plot(z$consis,z$ent)

f=ggplot(z, aes(x=consis, y=ent)) +
  geom_point(size=5, shape=16,alpha=0.5,color='#3d6e85')+
  geom_smooth(col='#3d6e85', se=F,size=2)+
  scale_x_continuous(name='relative cumulative responses',
                     breaks=c(8,7,6,6,5,5,4,4,4,3,3,3,2,2,2,2,1,1,1,1,0,-1,-1,-1,-1,-2,-2,-2,-2,-3,-3,-3,-4,-4,-4,-5,-5,-6,-6,-7,-8)) + # have tick marks for each session
  scale_y_continuous(name='entropy') +
  geom_vline(xintercept=0,linetype = "dotted", size = 0.5)+
  #scale_fill_distiller('',palette = "Purples",direction = 1) +
  theme(axis.text.x = element_text(size=25),
        axis.text.y = element_text(size=25),
        axis.title=element_text(size=40),
        plot.title = element_text(size=40,hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"))

f
ggsave(file=paste0("/Users/orlacamus/Downloads/CR-ent.png"), plot=f, width=9, height=6)


z=data.frame(c(0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1))
colnames(z)=c('lierate')
z$ent=-z$lierate*log2(z$lierate)-(1-z$lierate)*log2(1-z$lierate)
#z[z$lierate==1,]$ent=0
#z[z$lierate==0,]$ent=0
plot(z$lierate,z$ent)

f=ggplot(z, aes(x=lierate, y=ent)) +
  
  geom_smooth(col='#3d6e85', se=F,size=2)+
  geom_point(size=6, shape=16,alpha=0.5,color='#3d6e85')+
  scale_x_continuous(name='lie rate',
                     breaks=c(0,0.5,1)) + # have tick marks for each session
  scale_y_continuous(name='entropy') +
  #scale_fill_distiller('',palette = "Purples",direction = 1) +
  theme(axis.text.x = element_text(size=25),
        axis.text.y = element_text(size=25),
        axis.title=element_text(size=40),
        plot.title = element_text(size=40,hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"))

f
ggsave(file=paste0("/Users/orlacamus/Downloads/lierate-ent.png"), plot=f, width=6, height=6)


# Figure 2C
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

## lierate ~ 2d matrix of reward & consistency

agg <- fmridata %>% group_by(former_diff,diff,subj) %>% 
  summarise(lierate = mean(islie))

temp=agg %>% group_by(former_diff,diff) %>% 
  summarise(lierate = mean(lierate))

plot_ly(x=temp$former_diff, y=temp$diff, z=temp$lierate, type="scatter3d", mode="markers",color = temp$lierate)

temp=temp[temp$former_diff!=0,]
temp$former_diff=as.factor(temp$former_diff)
temp$diff=as.factor(temp$diff)
f=ggplot(temp, aes(x=former_diff, y=diff, fill=lierate)) +
  geom_tile() +
  geom_vline(xintercept = 7.5,color='red',linetype="dashed") +
  geom_hline(yintercept = 4.5,color='red',linetype="dashed") +
  scale_x_discrete(name='relative cumulative responses') + # have tick marks for each session
  scale_y_discrete(name='relative reward') +
  scale_fill_distiller('',palette = "Purples",direction = 1) +
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        axis.title=element_text(size=25),
        text = element_text(size=25,family ='Helvetica'),
        axis.ticks.length=unit(0.25, "cm"),
        axis.line.x=element_blank(),
        #axis.ticks.x=element_blank(),
        axis.line.y=element_blank(),
        #axis.ticks.y=element_blank(),
        plot.title = element_text(size=25,hjust = 0.5),
        legend.position="bottom",
        legend.text =element_blank(),
        panel.background = element_rect(fill = "white"),
        #panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
f
ggsave(file=paste0("/Users/orlacamus/Downloads/liematrix.png"), plot=f, width=6, height=6)


# DDM
ddm_results <- fread("UAhddm/DDM_result/Group/GroupReg5/stats_all_group_n37_reg5_stats.csv")
v_former_diff=ddm_results[ddm_results$V1 %like% "v_former_diff_subj",c('mean','V1')]
v_ses=ddm_results[ddm_results$V1 %like% "v_ses_subj",c('mean','V1')]
v_former_diff_ses=ddm_results[ddm_results$V1 %like% "v_former_diff:ses_subj",c('mean','V1')]
v_diff=ddm_results[ddm_results$V1 %like% "v_diff_subj",c('mean','V1')]
v_diff_ses=ddm_results[ddm_results$V1 %like% "v_diff:ses_subj",c('mean','V1')]
a_ses=ddm_results[ddm_results$V1 %like% "a_ses_subj",c('mean','V1')]
z_ses=ddm_results[ddm_results$V1 %like% "z_ses_subj",c('mean','V1')]
ddm_params=cbind(v_former_diff,v_ses$mean,v_former_diff_ses$mean,v_diff$mean,v_diff_ses$mean,a_ses$mean,z_ses$mean)
colnames(ddm_params)=c('v_former_diff','subjno','v_ses','v_former_diff_ses','v_diff','v_diff_ses','a_ses','z_ses')
ddm_params$subjno=subList

temp=ddm_params
temp=cbind(temp,change1[change1$condition==conditions[i],])

cor.test(temp$meta,temp$v_diff_ses)

pred <- predict(lm(meta ~ v_diff, temp), 
                se.fit = TRUE, interval = "confidence")
limits <- as.data.frame(pred$fit)


f=ggplot(data = temp, aes(x = v_diff, y=meta)) + 
  
  #geom_point(data = agg, aes(x = factor(run), y=mean),size=5, shape=16,alpha=1,color='#7209b7') +
  #geom_errorbar(width=0.2, alpha=0.8,size=1,data = agg,aes(ymin=mean-se, ymax=mean+se,),position = position_nudge(x = 0),color='#7209b7') +
  #geom_smooth(data = data, aes(x = as.numeric(run), y = similarity), col="#3a0ca3", method = 'lm',se=F)+
  
  
  geom_point(data = temp, aes(x = v_diff, y=meta),size=5, shape=16,alpha=1,color='#ecc46c') +
  geom_smooth(data = temp, aes(x = v_diff, y=meta), col='#d48d38', method = 'lm',se=F,size=2)+
  
  scale_x_continuous(name='weight on relative reward') + # have tick marks for each session
  scale_y_continuous(name='metacognition change') + 
  geom_line(aes(x = v_diff, y = limits$lwr), 
            linetype = 2) +
  geom_line(aes(x = v_diff, y = limits$upr), 
            linetype = 2) +
  theme(axis.text.x = element_text(size=25),
        axis.text.y = element_text(size=25),
        axis.title=element_text(size=25),
        plot.title = element_text(size=25,hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"))
f
ggsave(file=paste0('/Users/orlacamus/Downloads/ddm-corr.svg'), plot=f, width=5, height=4)





# correlation: entropy~v_consistency

cor.test(agg_all$entropy,ddm_params$v_former_diff)
temp=agg_all
temp$v_former_diff=ddm_params$v_former_diff

pred <- predict(lm(v_former_diff ~ entropy, temp), 
                se.fit = TRUE, interval = "confidence")
limits <- as.data.frame(pred$fit)


f=ggplot(data = temp, aes(x = entropy, y=v_former_diff)) + 
  
  #geom_point(data = agg, aes(x = factor(run), y=mean),size=5, shape=16,alpha=1,color='#7209b7') +
  #geom_errorbar(width=0.2, alpha=0.8,size=1,data = agg,aes(ymin=mean-se, ymax=mean+se,),position = position_nudge(x = 0),color='#7209b7') +
  #geom_smooth(data = data, aes(x = as.numeric(run), y = similarity), col="#3a0ca3", method = 'lm',se=F)+
  
  
  geom_point(data = temp, aes(x = entropy, y=v_former_diff),size=5, shape=16,alpha=1,color='#e2cfea') +
  geom_smooth(data = temp, aes(x = entropy, y=v_former_diff), col='#a06cd5', method = 'lm',se=F,size=2)+
  
  scale_x_continuous(name='overall response entropy') + # have tick marks for each session
  scale_y_continuous(name='weight on CR') + 
  geom_line(aes(x = entropy, y = limits$lwr), 
            linetype = 2) +
  geom_line(aes(x = entropy, y = limits$upr), 
            linetype = 2) +
  theme(axis.text.x = element_text(size=25),
        axis.text.y = element_text(size=25),
        axis.title=element_text(size=25),
        plot.title = element_text(size=25,hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"))
f
ggsave(file=paste0('/Users/orlacamus/Downloads/ddm-corr.svg'), plot=f, width=5, height=4)



