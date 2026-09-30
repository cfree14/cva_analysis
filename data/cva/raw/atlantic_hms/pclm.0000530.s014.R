#The R code and datasets in this study uses the Gulf of Mexico to refer to the area now known as the Gulf of America consistent with Executive Order (E.O.) 14172 (Restoring Names That Honor American Greatness).
#read in S1 Dataset: Biological Sensitivity Attribute Scores wherever it is stored locally
dat<-read.csv("~/Climate projects/CVA/SensAttr_Scoring/export_stocks_csv__7.13.23.csv")
datshort<-subset(dat,select=c(Stock.Name,Functional.Group))

#read in S2 Table: S2 Table: Species Name Match List wherever it is stored locally
species.functional.sorted<-read.csv("~/Climate projects/CVA/species_name_match.csv")
species.functional.sorted<-species.functional.sorted[,c(2,7)]
###calculate mean and sd of data quality scores for each species

dq<-data.frame(tapply(dat$Data.Quality,dat$Stock.Name,FUN = mean))
colnames(dq)<-"DQ_mean"
#reassign row.names as put in column
dq$Species<-row.names(dq)
#add SD to dataframe
dq$DQ_sd<-data.frame(tapply(dat$Data.Quality,dat$Stock.Name,FUN = sd))$tapply.dat.Data.Quality..dat.Stock.Name..FUN...sd.
#rearrange columns
dq<-data.frame(Stock.Name=dq$Species,DQ_mean=dq$DQ_mean,DQ_sd=dq$DQ_sd)
#add functional group to dataframe
dq<-merge(dq,datshort,by="Stock.Name",all.x=T)
#remove duplicates because merge created a bunch of duplicated rows
dq<-dq[!duplicated(dq),]
#add shortened stock name
dq$Code.Name<-c("angel","blacknose (ATL)","blacknose (GOM)","blacktip (ATL)","bonnethead (ATL)","Atlantic sharpnose (ATL)",
                 "Atlantic sharpnose (GOM)","basking","bigeye sand tiger","bigeye sixgill","bigeye thresher","bigeye tuna",
                 "bignose","blue marlin","bluntnose sixgill","bull","Caribbean reef","Caribbean sharpnose","dusky","finetooth",
                 "Florida smoothhound","Galapagos","great hammerhead","blacktip (GOM)","bonnethead (GOM)","gulf smoothhound",
                 "lemon","longbill spearfish","longfin mako","narrowtooth","night","albacore tuna","blue","shortfin mako",
                 "swordfish","porbeagle","nurse","oceanic whitetip","roundscale spearfish","sand tiger","sandbar","scalloped hammerhead",
                 "sharpnose sevengill","silky","smalltail","smooth dogfish (ATL)","smooth dogfish (GOM)","smooth hammerhead",
                 "spinner","common thresher","tiger","bluefin tuna","Atlantic sailfish","skipjack tuna","whale","white marlin","white","yellowfin tuna")

dq<-merge(dq,species.functional.sorted,by="Code.Name")

write.csv(dq,file="C:/Users/dan.crear/Documents/Climate projects/CVA/Data_quality_scores/sens_att_data_quality20230716.csv",row.names = F)

#isolate by functional group
dqLCS<-dq[which(dq$Functional.Group=="Large Coastal Sharks"),]
dqSCS<-dq[which(dq$Functional.Group=="Small Coastal Sharks & Smoothhound"),]
dqPel<-dq[which(dq$Functional.Group=="Pelagic Sharks"),]
dqTuna<-dq[which(dq$Functional.Group=="Tunas"),]
dqBill<-dq[which(dq$Functional.Group=="Billfish/Swordfish"),]

#reorder from smallest to largest mean
dqLCS<-dqLCS[order(dqLCS$DQ_mean),]
dqSCS<-dqSCS[order(dqSCS$DQ_mean),]
dqPel<-dqPel[order(dqPel$DQ_mean),]
dqTuna<-dqTuna[order(dqTuna$DQ_mean),]
dqBill<-dqBill[order(dqBill$DQ_mean),]


###plot mean and sd data quality scores for each species by species group

png("C:/Users/dan.crear/Documents/Climate projects/CVA/Data_quality_scores/dqSCS1.png",height=6,width=7,res=150,units="in")
par(mar=c(15,6,4,0))
barplot<-barplot(height=dqSCS$DQ_mean,names.arg=dqSCS$Narrative.Name,xaxt="n",ylab="Mean Data Quality Score",ylim=c(0,3.5),main="Small Coastal Sharks & Smoothhound Sharks",col="green")
arrows(y0=dqSCS$DQ_mean-dqSCS$DQ_sd,y1=dqSCS$DQ_mean+dqSCS$DQ_sd,x0=barplot,x1=barplot,code=3,angle=90,length=0.1)
text(barplot,par("usr")[3]-0.25,adj=1,xpd=T,cex=0.75,dqSCS$Narrative.Name,srt=60)
text(barplot,dqSCS$DQ_mean+dqSCS$DQ_sd+0.1,labels=as.character(round(dqSCS$DQ_mean,digits=1)),cex=0.75)
dev.off()

png("C:/Users/dan.crear/Documents/Climate projects/CVA/Data_quality_scores/dqLCS1.png",height=6,width=7,res=150,units="in")
par(mar=c(11,6,4,0))
barplot<-barplot(height=dqLCS$DQ_mean,names.arg=dqLCS$Narrative.Name,xaxt="n",ylab="Mean Data Quality Score",ylim=c(0,3.5),main="Large Coastal Sharks",col="red")
arrows(y0=dqLCS$DQ_mean-dqLCS$DQ_sd,y1=dqLCS$DQ_mean+dqLCS$DQ_sd,x0=barplot,x1=barplot,code=3,angle=90,length=0.1)
text(barplot,par("usr")[3]-0.25,adj=1,xpd=T,cex=0.75,dqLCS$Narrative.Name,srt=60)
text(barplot,dqLCS$DQ_mean+dqLCS$DQ_sd+0.1,labels=as.character(round(dqLCS$DQ_mean,digits=1)),cex=0.75)
dev.off()

png("C:/Users/dan.crear/Documents/Climate projects/CVA/Data_quality_scores/dqPel1.png",height=6,width=7,res=150,units="in")
par(mar=c(10,6,4,0))
barplot<-barplot(height=dqPel$DQ_mean,names.arg=dqPel$Narrative.Name,xaxt="n",ylab="Mean Data Quality Score",ylim=c(0,3.5),main="Pelagic Sharks",col="blue")
arrows(y0=dqPel$DQ_mean-dqPel$DQ_sd,y1=dqPel$DQ_mean+dqPel$DQ_sd,x0=barplot,x1=barplot,code=3,angle=90,length=0.1)
text(barplot,par("usr")[3]-0.25,adj=1,xpd=T,cex=0.75,dqPel$Narrative.Name,srt=60)
text(barplot,dqPel$DQ_mean+dqPel$DQ_sd+0.1,labels=as.character(round(dqPel$DQ_mean,digits=1)))
dev.off()

png("C:/Users/dan.crear/Documents/Climate projects/CVA/Data_quality_scores/dqTuna1.png",height=6,width=7,res=150,units="in")
par(mar=c(9,6,4,0))
barplot<-barplot(height=dqTuna$DQ_mean,names.arg=dqTuna$Narrative.Name,xaxt="n",ylab="Mean Data Quality Score",ylim=c(0,3.5),main="Tuna",col="yellow")
arrows(y0=dqTuna$DQ_mean-dqTuna$DQ_sd,y1=dqTuna$DQ_mean+dqTuna$DQ_sd,x0=barplot,x1=barplot,code=3,angle=90,length=0.1)
text(barplot,par("usr")[3]-0.25,adj=1,xpd=T,cex=0.75,dqTuna$Narrative.Name,srt=60)
text(barplot,dqTuna$DQ_mean+dqTuna$DQ_sd+0.1,labels=as.character(round(dqTuna$DQ_mean,digits=1)))
dev.off()

png("C:/Users/dan.crear/Documents/Climate projects/CVA/Data_quality_scores/dqBillSwo1.png",height=6,width=7,res=150,units="in")
par(mar=c(9,6,4,0))
barplot<-barplot(height=dqBill$DQ_mean,names.arg=dqBill$Narrative.Name,xaxt="n",ylab="Mean Data Quality Score",ylim=c(0,3.5),main="Billfish & Swordfish",col="purple")
arrows(y0=dqBill$DQ_mean-dqBill$DQ_sd,y1=dqBill$DQ_mean+dqBill$DQ_sd,x0=barplot,x1=barplot,code=3,angle=90,length=0.1)
text(barplot,par("usr")[3]-0.25,adj=1,xpd=T,cex=0.75,dqBill$Narrative.Name,srt=60)
text(barplot,dqBill$DQ_mean+dqBill$DQ_sd+0.1,labels=as.character(round(dqBill$DQ_mean,digits=1)))
dev.off()


###calculate mean and sd of data quality scores for each attribute by species group
alldatLCS<-dat[which(dat$Functional.Group=="Large Coastal Sharks"),]
alldatSCS<-dat[which(dat$Functional.Group=="Small Coastal Sharks & Smoothhound"),]
alldatPel<-dat[which(dat$Functional.Group=="Pelagic Sharks"),]
alldatTuna<-dat[which(dat$Functional.Group=="Tunas"),]
alldatBill<-dat[which(dat$Functional.Group=="Billfish/Swordfish"),]

dqLCSatt<-data.frame(tapply(alldatLCS$Data.Quality,alldatLCS$Attribute.Name,FUN = mean))
dqSCSatt<-data.frame(tapply(alldatSCS$Data.Quality,alldatSCS$Attribute.Name,FUN = mean))
dqPelatt<-data.frame(tapply(alldatPel$Data.Quality,alldatPel$Attribute.Name,FUN = mean))
dqTunatt<-data.frame(tapply(alldatTuna$Data.Quality,alldatTuna$Attribute.Name,FUN = mean))
dqBillatt<-data.frame(tapply(alldatBill$Data.Quality,alldatBill$Attribute.Name,FUN = mean))

dqLCSatt <- cbind(rownames(dqLCSatt), data.frame(dqLCSatt, row.names=NULL))
dqSCSatt <- cbind(rownames(dqSCSatt), data.frame(dqSCSatt, row.names=NULL))
dqPelatt <- cbind(rownames(dqPelatt), data.frame(dqPelatt, row.names=NULL))
dqTunatt <- cbind(rownames(dqTunatt), data.frame(dqTunatt, row.names=NULL))
dqBillatt <- cbind(rownames(dqBillatt), data.frame(dqBillatt, row.names=NULL))

colnames(dqLCSatt)<-c("att","DQ_mean")
colnames(dqSCSatt)<-c("att","DQ_mean")
colnames(dqPelatt)<-c("att","DQ_mean")
colnames(dqTunatt)<-c("att","DQ_mean")
colnames(dqBillatt)<-c("att","DQ_mean")

dqLCSatt$DQ_sd<-data.frame(tapply(alldatLCS$Data.Quality,alldatLCS$Attribute.Name,FUN = sd))$tapply.alldatLCS.Data.Quality..alldatLCS.Attribute.Name..FUN...sd.
dqSCSatt$DQ_sd<-data.frame(tapply(alldatSCS$Data.Quality,alldatSCS$Attribute.Name,FUN = sd))$tapply.alldatSCS.Data.Quality..alldatSCS.Attribute.Name..FUN...sd.
dqPelatt$DQ_sd<-data.frame(tapply(alldatPel$Data.Quality,alldatPel$Attribute.Name,FUN = sd))$tapply.alldatPel.Data.Quality..alldatPel.Attribute.Name..FUN...sd.
dqTunatt$DQ_sd<-data.frame(tapply(alldatTuna$Data.Quality,alldatTuna$Attribute.Name,FUN = sd))$tapply.alldatTuna.Data.Quality..alldatTuna.Attribute.Name..FUN...sd.
dqBillatt$DQ_sd<-data.frame(tapply(alldatBill$Data.Quality,alldatBill$Attribute.Name,FUN = sd))$tapply.alldatBill.Data.Quality..alldatBill.Attribute.Name..FUN...sd.


#reorder from smallest to largest mean
dqLCSatt<-dqLCSatt[order(dqLCSatt$DQ_mean,decreasing=F),]
dqSCSatt<-dqSCSatt[order(dqSCSatt$DQ_mean,decreasing=F),]
dqPelatt<-dqPelatt[order(dqPelatt$DQ_mean,decreasing=F),]
dqTunatt<-dqTunatt[order(dqTunatt$DQ_mean,decreasing=F),]
dqBillatt<-dqBillatt[order(dqBillatt$DQ_mean,decreasing=F),]

###plot mean and sd data quality scores for each attribute by species group

png("C:/Users/dan.crear/Documents/Climate projects/CVA/Data_quality_scores/dqSCSatt1.png",height=6,width=7,res=150,units="in")
par(mar=c(16,5,4,1))
barplot<-barplot(height=dqSCSatt$DQ_mean,names.arg=dqSCSatt$att,xaxt="n",ylab="Mean Data Quality Score",ylim=c(0,3.5),main="Small Coastal Sharks & Smoothhound Sharks",col="green",cex.names=0.85)
arrows(y0=dqSCSatt$DQ_mean-dqSCSatt$DQ_sd,y1=dqSCSatt$DQ_mean+dqSCSatt$DQ_sd,x0=barplot,x1=barplot,code=3,angle=90,length=0.1)
text(barplot,par("usr")[3]-0.25,adj=1,xpd=T,cex=1,dqSCSatt$att,srt=60)
text(barplot,dqSCSatt$DQ_mean+dqSCSatt$DQ_sd+0.2,labels=as.character(round(dqSCSatt$DQ_mean,digits=1)),cex=0.75)
dev.off()

png("C:/Users/dan.crear/Documents/Climate projects/CVA/Data_quality_scores/dqLCSatt1.png",height=6,width=7,res=150,units="in")
par(mar=c(16,5,4,1))
barplot<-barplot(height=dqLCSatt$DQ_mean,names.arg=dqLCSatt$att,xaxt="n",ylab="Mean Data Quality Score",ylim=c(0,3.5),main="Large Coastal Sharks",col="red",cex.names=0.85)
arrows(y0=dqLCSatt$DQ_mean-dqLCSatt$DQ_sd,y1=dqLCSatt$DQ_mean+dqLCSatt$DQ_sd,x0=barplot,x1=barplot,code=3,angle=90,length=0.1)
text(barplot,par("usr")[3]-0.25,adj=1,xpd=T,cex=1,dqLCSatt$att,srt=60)
text(barplot,dqLCSatt$DQ_mean+dqLCSatt$DQ_sd+0.2,labels=as.character(round(dqLCSatt$DQ_mean,digits=1)),cex=0.75)
dev.off()

png("C:/Users/dan.crear/Documents/Climate projects/CVA/Data_quality_scores/dqPelatt1.png",height=6,width=7,res=150,units="in")
par(mar=c(16,5,4,1))
barplot<-barplot(height=dqPelatt$DQ_mean,names.arg=dqPelatt$att,xaxt="n",ylab="Mean Data Quality Score",ylim=c(0,3.5),main="Pelagic Sharks",col="blue",cex.names=0.85)
arrows(y0=dqPelatt$DQ_mean-dqPelatt$DQ_sd,y1=dqPelatt$DQ_mean+dqPelatt$DQ_sd,x0=barplot,x1=barplot,code=3,angle=90,length=0.1)
text(barplot,par("usr")[3]-0.25,adj=1,xpd=T,cex=1,dqPelatt$att,srt=60)
text(barplot,dqPelatt$DQ_mean+dqPelatt$DQ_sd+0.2,labels=as.character(round(dqPelatt$DQ_mean,digits=1)),cex=0.75)
dev.off()

png("C:/Users/dan.crear/Documents/Climate projects/CVA/Data_quality_scores/dqTunatt1.png",height=6,width=7,res=150,units="in")
par(mar=c(16,5,4,1))
barplot<-barplot(height=dqTunatt$DQ_mean,names.arg=dqTunatt$att,xaxt="n",ylab="Mean Data Quality Score",ylim=c(0,3.5),main="Tuna",col="yellow",cex.names=0.85)
arrows(y0=dqTunatt$DQ_mean-dqTunatt$DQ_sd,y1=dqTunatt$DQ_mean+dqTunatt$DQ_sd,x0=barplot,x1=barplot,code=3,angle=90,length=0.1)
text(barplot,par("usr")[3]-0.25,adj=1,xpd=T,cex=1,dqTunatt$att,srt=60)
text(barplot,dqTunatt$DQ_mean+dqTunatt$DQ_sd+0.2,labels=as.character(round(dqTunatt$DQ_mean,digits=1)),cex=0.75)
dev.off()

png("C:/Users/dan.crear/Documents/Climate projects/CVA/Data_quality_scores/dqBillatt1.png",height=6,width=7,res=150,units="in")
par(mar=c(16,7,4,0))
barplot<-barplot(height=dqBillatt$DQ_mean,names.arg=dqBillatt$att,xaxt="n",ylab="Mean Data Quality Score",ylim=c(0,3.5),main="Billfish & Swordfish",col="purple",cex.names=0.85)
arrows(y0=dqBillatt$DQ_mean-dqBillatt$DQ_sd,y1=dqBillatt$DQ_mean+dqBillatt$DQ_sd,x0=barplot,x1=barplot,code=3,angle=90,length=0.1)
text(barplot,par("usr")[3]-0.25,adj=1,xpd=T,cex=1,dqBillatt$att,srt=60)
text(barplot,dqBillatt$DQ_mean+dqBillatt$DQ_sd+0.2,labels=as.character(round(dqBillatt$DQ_mean,digits=1)),cex=0.75)
dev.off()

