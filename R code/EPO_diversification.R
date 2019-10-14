#######################################################################
#######################################################################
##
##                         Keungoui Kim
##       goal : 1. Construct dataset for measuring diversification indices 
##              2. Measure diversification indicesMeausring technology diversification indices
##       Data set: EPO
##       Time Span: 
##       Variables 
##   Notice :
date()
#######################
getwd()
setwd("E:/[Analysis]/Ph.D.research/EPO")

index.list<- c("variety","HHI","gini.simpson","1/HHI","entropy","rao.stirling")

##### country-yearly level technology diversification meaure #########
# load data 
EPO10_IPC_EP<-as.data.frame(read.csv("./EPO10_IPC_EP.csv",header=TRUE))
save(EPO10_IPC_EP,file="./EPO10_IPC_EP.rdt")
load(file="./EPO10_IPC_EP.rdt")
head(EPO10_IPC_EP)
# settting time period: 1995 ~ 2015
EPO10_IPC_EP.01 %>% filter (appln_filing_year >=1995 & $appln_filing_year<=2015)

# checking data set
nrow(EPO10_IPC_EP) 
nrow(EPO10_IPC_EP) 

# create year-ipc summary data
EPO10_IPC_EP.all.ipc.yr.sort<-ddply(EPO10_IPC_EP.01, .(appln_filing_year, ipc4), summarize, IPC_sum=length(ipc4))
EPO10_IPC_EP.all.ipc.yr.sort$ipc4<-as.character(EPO10_IPC_EP.all.ipc.yr.sort$ipc4)

# construct accumulated data set
EPO10_IPC_EP.3TDI.yr<-0
temp<-EPO10_IPC_EP.all.ipc.yr.sort[EPO10_IPC_EP.all.ipc.yr.sort$appln_filing_year==unique(EPO10_IPC_EP.all.ipc.yr.sort$appln_filing_year)[[1]],]  
for (i in 2:length(unique(EPO10_IPC_EP.all.ipc.yr.sort$appln_filing_year))){
  temp.1<-EPO10_IPC_EP.all.ipc.yr.sort[EPO10_IPC_EP.all.ipc.yr.sort$appln_filing_year==unique(EPO10_IPC_EP.all.ipc.yr.sort$appln_filing_year)[[i]],]  
  for (j in 1:length(unique(temp.1$ipc4))){
    if (sum(unique(temp$ipc4) %in% unique(temp.1$ipc4)[[j]])>0){
      # when existing IPC increases 
      temp.1.1<-temp.1[temp.1$ipc4==unique(temp.1$ipc4)[[j]],]
      temp.1.1.1<-temp[temp$ipc4==unique(temp.1$ipc4)[[j]],]
      temp.1.1.2<-rbind(temp.1.1.1[temp.1.1.1$IPC_sum==max(temp.1.1.1$IPC_sum),][1,],temp.1.1)
      temp.1.2<-rbind(temp[temp$ipc4==unique(temp.1$ipc4)[[j]],],temp.1.1)
      temp.1.2$IPC_sum[[nrow(temp.1.2)]]<-sum(temp.1.2$IPC_sum[[(nrow(temp.1.2)-1)]],temp.1.2$IPC_sum[[nrow(temp.1.2)]])
      temp.1.2.1<-temp.1.2[nrow(temp.1.2),]
      temp<-rbind(temp,temp.1.2.1)
    } else {
      # when new IPC increases
      temp.1.1<-temp.1[temp.1$ipc4==unique(temp.1$ipc4)[[j]],]
      temp<-rbind(temp,temp.1.1)
    }
  }
  EPO10_IPC_EP.3TDI.yr<-rbind(EPO10_IPC_EP.3TDI.yr,temp)
  print(i)
}
EPO10_IPC_EP.3TDI.yr<-EPO10_IPC_EP.3TDI.yr[2:nrow(EPO10_IPC_EP.3TDI.yr),]
write.table(EPO10_IPC_EP.3TDI.yr, file = "./EPO10_IPC_EP.3TDI.yr.csv", sep = ",",
            row.names=FALSE, qmethod = "double")

# Calculate technology diversification
EPO10_IPC_EP.3TDI.yr.1<-read_data(path="./EPO10_IPC_EP.3TDI.yr.csv")
EPO10_IPC_EP.3TDI.yr.1.div<-diversity(EPO10_IPC_EP.3TDI.yr.1, type = "all", category_row = FALSE, dis = NULL,
                                      method = "euclidean", q = 0, alpha = 1, beta = 1, base = exp(1))
save(EPO10_IPC_EP.3TDI.yr.1.div,file="./EPO10_IPC_EP.3TDI.yr.1.div.rdt")
load(file="./EPO10_IPC_EP.3TDI.yr.1.div.rdt")
head(EPO10_IPC_EP.3TDI.yr.1.div)

EPO10_IPC_EP.3TDI.yr.1.div.1<-EPO10_IPC_EP.3TDI.yr.1.div[,index.list]

# Conduct PCA
EPO10_IPC_EP.3TDI.yr.1.div.1.pca<-prcomp(EPO10_IPC_EP.3TDI.yr.1.div.1,
                                         center=TRUE, #average=0
                                         scale.=TRUE) #noarmalize
attributes(EPO10_IPC_EP.3TDI.yr.1.div.1.pca)

pairs.panels(EPO10_IPC_EP.3TDI.yr.1.div.1,gap=0,pch=21)
dev.copy(png,filename=cc)
dev.off()

pairs.panels(EPO10_IPC_EP.3TDI.yr.1.div.1.pca$x,gap=0,pch=21)
dev.copy(png,filename=dd)
dev.off()

capture.output(print(EPO10_IPC_EP.3TDI.yr.1.div.1.pca),
               file=ff,append=TRUE)

capture.output(summary(EPO10_IPC_EP.3TDI.yr.1.div.1.pca),
               file=gg,append=TRUE)
biplot(EPO10_IPC_EP.3TDI.yr.1.div.1.pca,main='_EP_')
ee<-paste('./Figure/_EP/_EP_YR_ALL_PCA','_biplot.png',sep='')
dev.copy(png,filename=ee)
dev.off()

ggbiplot(EPO10_IPC_EP.3TDI.yr.1.div.1.pca,
         obs.scale=1,var.scale=1, # setting scale
         ellipse=TRUE, circle=TRUE, labels=rownames(EPO10_IPC_EP.3TDI.yr.1.div.1.pca$x), 
         var.axes=TRUE, main='_EP_',alpha=TRUE
) + scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')+
  labs(title='_EP_')
ee<-paste('./Figure/_EP/_EP_YR_ALL_PCA','_biplot2.png',sep='')
dev.copy(png,filename=ee)
dev.off()
