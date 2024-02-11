rm(list=ls())
setwd("/Users/orlacamus/Desktop/projects/unethical_amnesia/analysisCode/data")

want = c("data.table", "lme4", "Matrix", "stargazer","coxme","survival","bdsmatrix","ez","ggplot2","lmerTest","dplyr",'yarrr','gridExtra','ggsignif','ggpubr','patchwork')
have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }
# Now load them all
lapply(want, require, character.only = TRUE)
library(rearrr)
library(dplyr)

packages = c('factoextra','RColorBrewer', 'igraph', 'lme4', 'car', 'beeswarm', 'plotrix','rockchalk')
newpackages = packages[!(packages %in% installed.packages()[,"Package"])]
if(length(newpackages)) install.packages(newpackages)
loaded = lapply(packages, require, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
cat(paste('## ', sum(unlist(loaded)), ' out of ', length(unlist(loaded)) ,' packages loaded\n', sep = ''))


# to confirm that AUC and metacognition are independent
subList = c(102, 103, 105, 107, 108, 110, 111, 112, 113, 114,
            115, 116, 117, 119, 120, 121, 122, 123, 125, 127, 128,
            130, 131)
subList = c(102, 103, 105, 107, 108, 110, 111, 112, 113, 114,
            115, 116, 117, 118, 119, 120, 121, 122, 123, 125, 127, 128,
            129, 130, 131, 132)

#read the data
#read the data
memory=fread("memory_summary.csv")
memory_rep=memory

memory=memory[memory$rt!="None"]
memory$correct=memory$accuracy
memory[memory$correct==0,]$correct=-1
#memory_alt=memory
#
#memory_alt$meta=1-(memory$accuracy-memory$confidence/100)
#memory_alt$AUC=(max(memory$AUC)-max(memory$AUC)*memory$accuracy-memory$AUC)
#memory_alt$RT=(4-memory$accuracy*4-memory$RT/1000)
#memory_alt$MAD=(max(memory$MAD)-max(memory$MAD)*memory$accuracy-memory$MAD)
#memory_alt$AD=(max(memory$AD)-max(memory$AD)*memory$accuracy-memory$AD)





memory$meta=1-(memory$accuracy-memory$confidence/100)^2
memory$AUC=max(memory$AUC)-(max(memory$AUC)-max(memory$AUC)*memory$accuracy-memory$AUC)^2/max(memory$AUC)
memory$RT=4-(4-memory$accuracy*4-memory$RT/1000)^2/4
memory$MAD=max(memory$MAD)-(max(memory$MAD)-max(memory$MAD)*memory$accuracy-memory$MAD)^2/max(memory$MAD)
memory$AD=max(memory$AD)-(max(memory$AD)-max(memory$AD)*memory$accuracy-memory$AD)^2/max(memory$AD)


item_agg = memory %>% group_by(subject_nr,menu, stage,condition) %>% 
  dplyr::summarise(acc = mean(accuracy),
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
item_change1$AUC=item_agg_pre$AUC-item_agg_post$AUC
item_change1$RT=item_agg_pre$RT-item_agg_post$RT
item_change1$MAD=item_agg_pre$MAD-item_agg_post$MAD
item_change1$AD=item_agg_pre$AD-item_agg_post$AD


item_change2=item_agg_pre
item_change2$meta=item_agg_pre$meta-item_agg_post2$meta
item_change2$conf=item_agg_pre$conf-item_agg_post2$conf
item_change2$acc=item_agg_pre$acc-item_agg_post2$acc
item_change2$AUC=item_agg_pre$AUC-item_agg_post2$AUC
item_change2$RT=item_agg_pre$RT-item_agg_post2$RT
item_change2$MAD=item_agg_pre$MAD-item_agg_post2$MAD
item_change2$AD=item_agg_pre$AD-item_agg_post2$AD


pre=memory[memory$stage=='pre',]
post=memory[memory$stage=='post',]
post2=memory[memory$stage=='post2',]



# PCA inter-subject

detach(package:plyr)
temp=item_change1
pt='/Users/orlacamus/Desktop/projects/unethical_amnesia/analysisCode/7.PCA/'

temp = temp %>% group_by(subject_nr) %>% 
  summarise(
            meta = mean(meta),
            AUC = mean(AUC),
            RT=mean(RT),
            MAD=mean(MAD),
            AD=mean(AD)
  )


temp=temp[,c('RT','AUC','meta','MAD','AD')]


pca=prcomp(temp,scale=TRUE)



var <- pca$sdev^2 /sum(pca$sdev^2 )
cumsum <- data.frame(x=1:length(var),
                     y=cumsum(pca$sdev^2 )*100/length(pca$sdev))
cumsum$subjno=subj


f=fviz_eig(pca,
           geom = c("bar"), barfill = "gray70",
           barcolor = "gray70", linecolor = "black", 
           addlabels = FALSE)

f=f+
  geom_point(data=cumsum, aes(x, y), size=2, color="gray50") +
  geom_line(data=cumsum, aes(x, y), color="gray50") +
  geom_hline(yintercept=90,colour = 'gray70',linetype='dashed')+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title=element_text(size=20),
        text = element_text(size=20,family ='Helvetica'),
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
ggsave(file=paste0('/Users/orlacamus/Downloads/pca.svg'), plot=f, width=7, height=5)
ind <- get_pca_ind(pca)
coord=data.frame(ind$coord) 


colnames(coord)=c(1:ncol(coord))
coord$fullconf=distance(coord,origin=rep(0,ncol(coord)),cols = as.character(c(1:ncol(coord))))$.distance
coord$conf=distance(coord,origin=rep(0,cumsum[cumsum$y>90,1][1]),cols = as.character(c(1:cumsum[cumsum$y>90,1][1])))$.distance
coord$subjno=subList
fviz_pca_var(pca, col.var = "black")
f=fviz_pca_biplot(pca, label="var", col.ind="cos2",col.var = "gray70")+
  theme(legend.position = "none")
f=fviz_pca_biplot(pca, label="none", col.ind="cos2",col.var = "gray70",title = 'pre-post')+
  
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title=element_text(size=20),
        text = element_text(size=20,family ='Helvetica'),
        axis.ticks.length=unit(0.25, "cm"),
        axis.line.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(size=25,hjust = 0.5),
        legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
f
ggsave(file=paste0('/Users/orlacamus/Downloads/dim.svg'), plot=f, width=5, height=5)

write.csv(coord,paste0(pt,'subject-level_coords_new.csv'), row.names=FALSE)



# each subject

temp=item_change2

temp = temp %>% group_by(subject_nr) %>% 
  summarise(
    meta = mean(meta),
    AUC = mean(AUC),
    RT=mean(RT),
    MAD=mean(MAD),
    AD=mean(AD)
  )


temp=temp[,c('RT','AUC','meta','MAD','AD')]





pca=prcomp(temp,scale=TRUE)


var <- pca$sdev^2 /sum(pca$sdev^2 )
cumsum <- data.frame(x=1:length(var),
                     y=cumsum(pca$sdev^2 )*100/length(pca$sdev))
cumsum$subjno=subj


f=fviz_eig(pca,
           geom = c("bar"), barfill = "gray70",
           barcolor = "gray70", linecolor = "black", 
           addlabels = FALSE)

f=f+
  geom_point(data=cumsum, aes(x, y), size=2, color="gray50") +
  geom_line(data=cumsum, aes(x, y), color="gray50") +
  geom_hline(yintercept=90,colour = 'gray70',linetype='dashed')+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title=element_text(size=20),
        text = element_text(size=20,family ='Helvetica'),
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
ggsave(file=paste0('/Users/orlacamus/Downloads/pca2.svg'), plot=f, width=7, height=5)
ind <- get_pca_ind(pca)
coord=data.frame(ind$coord)
colnames(coord)=c(1:ncol(coord))

coord$fullconf=distance(coord,origin=rep(0,ncol(coord)),cols = as.character(c(1:ncol(coord))))$.distance
coord$conf=distance(coord,origin=rep(0,cumsum[cumsum$y>90,1][1]),cols = as.character(c(1:cumsum[cumsum$y>90,1][1])))$.distance

coord$subjno=subList
fviz_pca_var(pca, col.var = "black")
f=fviz_pca_biplot(pca, label="none", col.ind="cos2",col.var = "gray70",title = 'pre-post2')+
  
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title=element_text(size=20),
        text = element_text(size=20,family ='Helvetica'),
        axis.ticks.length=unit(0.25, "cm"),
        axis.line.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(size=25,hjust = 0.5),
        legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
ggsave(file=paste0('/Users/orlacamus/Downloads/dim2.svg'), plot=f, width=5, height=5)

write.csv(coord,paste0(pt,'subject-level_coords2_new.csv'), row.names=FALSE)

