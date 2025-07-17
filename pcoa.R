data <-read.table("winter.txt",header=TRUE,sep="\t",row.names=1)

data <-data/apply(data,2,sum)
dat <-t(data)
group <- read.delim('group.txt',sep='\t',stringsAsFactors=FALSE)

bray<- vegdist(data,method='bray')
#euclidean <- vegdist(data,method='euclidean')
#manhattan <- vegdist(data,method='manhattan')
#jaccard <- vegdist(data,method='jaccard')

#dune_bray<-vegdist(data,method="bray",binary=F)
bray<-as.matrix(bray)
write.table(bray,"bray-crutis.txt",sep="\t")

#k=(nrow(data)-1
pcoa<-cmdscale(bray,k=3,eig=T
pcoa_data<-data.frame({pcoa$point})


pcoa_data$Sample_ID<-rownames(pcoa_data)
names(pcoa_data)[1:3]<-paste0("PCoA",1:3)

eig=pcoa$eig

eig_sum <- sum(eig)  
 
eig_percent <- round(eig / eig_sum * 100, 1)  
 
print(eig_percent)

poi=pcoa$points
poi=as.data.frame(poi)

pcoa_result<-cbind(pcoa_data,group)
head(pcoa_result)

dune.div<-adonis2(data~group,data=group,permutations=999,method="bray")
dune.div
dune_adonis<-paste0("adonisR2:",round(dune.div$R2,2),";P-value:",dune.div$`Pr(>F)`)
dune_adonis
print(dune_adonis)


p <- ggplot(pcoa_result, aes(x = PCoA1, y = PCoA2, color = group)) +
  geom_point(aes(color = group, shape = group), size = 5) +
  labs(x = paste("PCoA1 (", eig_percent[1], "%)", sep = ""),
       y = paste("PCoA2 (", eig_percent[2], "%)", sep = ""),
       caption = dune_adonis) +  # 也可用title
  scale_colour_manual(values = c("#E1C855", "#E07B54", "#51B1B7")) +
  theme(legend.position = c(0.9, 0.19),
        legend.title = element_blank(),
        panel.grid = element_blank(),  
        plot.title = element_text(hjust = 0.5), 
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(color = 'black', fill = 'transparent'),  
        axis.text = element_text(color = "black", size = 10)) + 
  geom_hline(aes(yintercept = 0), colour = "#BEBEBE", linetype = "dashed") +  
  geom_vline(aes(xintercept = 0), colour = "#BEBEBE", linetype = "dashed")   


print(p)


p=p+stat_ellipse(data=pcoa_result,
                 geom="polygon",
                 level=0.9,
                 linetype=2,
                 linewidth=0.5,
                 aes(fill=group),
                 alpha=0.3,
                 show.legend=T)+
  scale_fill_manual(values=c("#f2ccac","#a1d5b9","#e1abbc"))



png(file="bray_pcoa.png",width=5,height=5,res=600,units="in")

png(file = "bray_pcoa_with_marginals.png", width = 5, height = 5, res = 600, units = "in")  
ggMarginal(p, type = c('density'), margins = 'both', size = 3.5, groupColour = FALSE, groupFill = TRUE)  
dev.off()  





