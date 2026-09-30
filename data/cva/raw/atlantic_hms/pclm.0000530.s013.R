#The R code and datasets in this study uses the Gulf of Mexico to refer to the area now known as the Gulf of America consistent with Executive Order (E.O.) 14172 (Restoring Names That Honor American Greatness).
#read in S1 Dataset: Biological Sensitivity Attribute Scores wherever it is stored locally
ss<-read.csv("~/Climate projects/CVA/SensAttr_Scoring/export_stocks_csv__7.13.23.csv")


#change stock size/status to stock size_status
ss$Attribute.Name[which(ss$Attribute.Name=="Stock Size/Status")]<-"Stock Size_Status"

#change albacore name 
ss$Stock.Name[which(ss$Stock.Name=="North Atlantic / Mediterranean albacore tuna")]<-"North Atlantic_Mediterranean albacore tuna"

#make shorter attribute names
ss$Short.Attribute<-ss$Attribute.Name
ss$Short.Attribute[which(ss$Short.Attribute=="Mobility and Dispersal of Early Life Stages")]<-"Mobility_Dispersal of ELS"
ss$Short.Attribute[which(ss$Short.Attribute=="Reproductive Strategy Sensitivity")]<-"Repro Strat Sensitivity"
ss$Short.Attribute[which(ss$Short.Attribute=="Sensitivity to Ocean Acidification")]<-"Sensitivity to OA"
ss$Short.Attribute[which(ss$Short.Attribute=="Sensitivity to Temperature")]<-"Sensitivity to Temp"
ss$Short.Attribute[which(ss$Short.Attribute=="Specificity in Early Life History Requirements")]<-"Specificity EL Hist REQs"
ss$Short.Attribute[which(ss$Short.Attribute=="Population Growth Rate")]<-"Pop Growth Rate"

#make some group names shorter
ss$Func.Grp.Short<-ss$Functional.Group
ss$Func.Grp.Short[which(ss$Func.Grp.Short=="Large Coastal Sharks")]<-"LCS"
ss$Func.Grp.Short[which(ss$Func.Grp.Short=="Small Coastal Sharks & Smoothhound")]<-"SCS_smoothhound"
ss$Func.Grp.Short[which(ss$Func.Grp.Short=="Pelagic Sharks")]<-"Pelagic_sharks"
ss$Func.Grp.Short[which(ss$Func.Grp.Short=="Billfish/Swordfish")]<-"Billfish_Swordfish"


stocks<-unique(ss$Stock.Name)
atts<-unique(ss$Attribute.Name)


colrs<- setNames(c("brown","green","forestgreen","grey","cyan","red","blue","orange","darkorchid1",
                   "khaki2","magenta4","coral","yellow","hotpink","darkseagreen1"),unique(ss$Scorer))

logic_model<-function(dat){
  
  if(length(which(dat$sen_att_mean>=3.5))>=3){
    Score<-"very high"
  }else if(length(which(dat$sen_att_mean>=3))>=2){
    Score<-"high"
  }else if(length(which(dat$sen_att_mean>=2.5))>=2){
    Score<-"moderate"
  }else{
    Score<-"low"
  }
  
  sen_att_score<-Score
  
  #calculate mean sd of score
  meansd<-round(mean(dat$sen_att_sd),digits=2)
  
  return(list(score_breakdown=dat,sen_att_score=sen_att_score,meansd=meansd))
}


########## by group and then by species ################          
sen_att_analysis<-function(stock,group){
  
  s<-which(ss$Stock.Name==stock)
  s1<-ss[s,]
  
  w.att.meanRec<-NULL
  att.nameRec<-NULL
  att.sdRec<-NULL
  data.quality.meanRec<-NULL
  LpRec<-NULL
  MpRec<-NULL
  HpRec<-NULL
  VpRec<-NULL
  pdf(paste("~/Climate projects/CVA/SensAttr_Scoring/Final_Run/score_vis/",group,"/",stock,".pdf",sep=""))
  par(mfrow=c(4,4))
  for(j in atts){
    a<-which(s1$Attribute.Name==j)
    a1<-s1[a,]
    
    att.name<-unique(a1$Attribute.Name)
    short.att.name<-unique(a1$Short.Attribute)
    num.experts<-length(unique(a1$Scorer))
    
    #calculate number of tallies into L, M, H, V
    L<-sum(a1$Scoring.Rank1,na.rm=T)
    M<-sum(a1$Scoring.Rank2,na.rm=T)
    H<-sum(a1$Scoring.Rank3,na.rm=T)
    V<-sum(a1$Scoring.Rank4,na.rm=T)
    
    #calculate percent of tallies in each category
    #calculate counts and percent of counts into L, M, H, V
    Lp<-L/sum(L,M,H,V)
    Mp<-M/sum(L,M,H,V)
    Hp<-H/sum(L,M,H,V)
    Vp<-V/sum(L,M,H,V)
    
    #calculate weighted average
    w.att.mean<-((L*1) + (M*2) + (H*3) + (V*4)) / (num.experts * 5)
    
    #calculate SD
    att.sd<-round(sd(c(rep(1,L),rep(2,M),rep(3,H),rep(4,V))),digits=2)
    
    #calculate average of datat quality score
    data.quality.mean<-mean(a1$Data.Quality,na.rm=T)
    
    #put weighted averages and other values in vector
    w.att.meanRec<-c(w.att.meanRec,w.att.mean)
    att.sdRec<-c(att.sdRec,att.sd)
    att.nameRec<-c(att.nameRec,att.name)
    data.quality.meanRec<-c(data.quality.meanRec,data.quality.mean)
    #need to proportions for species narrative barplot
    LpRec<-c(LpRec,Lp)
    MpRec<-c(MpRec,Mp)
    HpRec<-c(HpRec,Hp)
    VpRec<-c(VpRec,Vp)
    
    #generate plot for distribution of scoring
    plot.a1<-as.matrix(subset(a1,select=c(Scorer,Scoring.Rank1,Scoring.Rank2,Scoring.Rank3,Scoring.Rank4)))
    
    i_colrs<-match(a1$Scorer,names(colrs))
    
    round.w.att.mean<-round(w.att.mean,digits=2)#Tyler requested 2 decimal places
    barplot(plot.a1[,2:5],names.arg=c("L","M","H","V"),col = colrs[i_colrs],main=paste(short.att.name,"\nm=",round.w.att.mean,"sd=",att.sd),sub=stock)

  }
  dev.off()
  
  #put weighted averages in df
  sen_att_means<-data.frame(sen_att_names=att.nameRec,sen_att_mean=w.att.meanRec,sen_att_sd=att.sdRec,sen_att_dq_mean=data.quality.meanRec,LpRec,MpRec,HpRec,VpRec)
  
  sen_att_score<-logic_model(dat=sen_att_means)
  
  #sen_att_means is same as dat and same as score_breakdown
  #save score breakdown
  write.csv(sen_att_score$score_breakdown, file=paste("~/Climate projects/CVA/SensAttr_Scoring/Final_Run/",stock,"_senattscore.csv",sep=""),row.names = F)
  
  
  return(sen_att_score)
  
}

#### LCS stock run
SBK<-sen_att_analysis(stock="Atlantic blacktip shark",group="LCS")
BSG<-sen_att_analysis(stock="Basking shark",group="LCS")
BST<-sen_att_analysis(stock="Bigeye sand tiger shark",group="LCS")
SBG<-sen_att_analysis(stock="Bignose shark",group="LCS")
SBU<-sen_att_analysis(stock="Bull shark",group="LCS")
SRF<-sen_att_analysis(stock="Caribbean reef shark",group="LCS")
DUS<-sen_att_analysis(stock="Dusky shark",group="LCS")
GAL<-sen_att_analysis(stock="Galapagos shark",group="LCS")
GHH<-sen_att_analysis(stock="Great hammerhead shark",group="LCS")
SBKgom<-sen_att_analysis(stock="Gulf of Mexico blacktip shark",group="LCS")
LEM<-sen_att_analysis(stock="Lemon shark",group="LCS")
SNT<-sen_att_analysis(stock="Narrowtooth shark",group="LCS")
SNI<-sen_att_analysis(stock="Night shark",group="LCS")
NUR<-sen_att_analysis(stock="Nurse shark",group="LCS")
SST<-sen_att_analysis(stock="Sand tiger shark",group="LCS")
SSB<-sen_att_analysis(stock="Sandbar shark",group="LCS")
SPL<-sen_att_analysis(stock="Scalloped hammerhead shark",group="LCS")
FAL<-sen_att_analysis(stock="Silky shark",group="LCS")
SHH<-sen_att_analysis(stock="Smooth hammerhead shark",group="LCS")
SSP<-sen_att_analysis(stock="Spinner shark",group="LCS")
TIG<-sen_att_analysis(stock="Tiger shark",group="LCS")
WHA<-sen_att_analysis(stock="Whale shark",group="LCS")
WHI<-sen_att_analysis(stock="White Shark",group="LCS")

setwd("~/Climate projects/CVA/SensAttr_Scoring/Final_Run/score_vis/LCS")
file.remove("LCS_final_plots.pdf")
file.list=list.files()
pdftools::pdf_combine(file.list, output  = "~/Climate projects/CVA/SensAttr_Scoring/Final_Run/score_vis/LCS/LCS_final_plots.pdf")

#### SCS/smoothhound stock run
ANG<-sen_att_analysis(stock="Atlantic angel shark",group="SCS_smoothhound")
SBN<-sen_att_analysis(stock="Atlantic blacknose shark - Atlantic",group="SCS_smoothhound")
SBNgom<-sen_att_analysis(stock="Atlantic blacknose shark - Gulf of Mexico",group="SCS_smoothhound")
BON<-sen_att_analysis(stock="Atlantic bonnethead",group="SCS_smoothhound")
SAS<-sen_att_analysis(stock="Atlantic sharpnose shark - Atlantic",group="SCS_smoothhound")
SASgom<-sen_att_analysis(stock="Atlantic sharpnose shark - GOM",group="SCS_smoothhound")
SCS<-sen_att_analysis(stock="Caribbean sharpnose shark",group="SCS_smoothhound")
SFT<-sen_att_analysis(stock="Finetooth shark",group="SCS_smoothhound")
FSH<-sen_att_analysis(stock="Florida smoothhound shark",group="SCS_smoothhound")
GSH<-sen_att_analysis(stock="Gulf smoothhound shark",group="SCS_smoothhound")
BONgom<-sen_att_analysis(stock="Gulf of Mexico bonnethead shark",group="SCS_smoothhound")
STS<-sen_att_analysis(stock="Smalltail shark",group="SCS_smoothhound")
DGS<-sen_att_analysis(stock="Smooth dogfish shark - Atlantic",group="SCS_smoothhound")
DGSgom<-sen_att_analysis(stock="Smooth dogfish shark - Gulf of Mexico",group="SCS_smoothhound")

setwd("~/Climate projects/CVA/SensAttr_Scoring/Final_Run/score_vis/SCS_smoothhound")
file.remove("SCS_final_plots.pdf")
file.list=list.files()
pdftools::pdf_combine(file.list, output  = "~/Climate projects/CVA/SensAttr_Scoring/Final_Run/score_vis/SCS_smoothhound/SCS_final_plots.pdf")


#### Pelagic sharks stock run
BES<-sen_att_analysis(stock="Bigeye sixgill shark",group="Pelagic_sharks")
BTH<-sen_att_analysis(stock="Bigeye thresher shark",group="Pelagic_sharks")
LMA<-sen_att_analysis(stock="Longfin mako shark",group="Pelagic_sharks")
BSH<-sen_att_analysis(stock="North Atlantic blue shark",group="Pelagic_sharks")
SMA<-sen_att_analysis(stock="North Atlantic shortfin mako shark",group="Pelagic_sharks")
POR<-sen_att_analysis(stock="Northwest Atlantic porbeagle shark",group="Pelagic_sharks")
OCS<-sen_att_analysis(stock="Oceanic whitetip shark",group="Pelagic_sharks")
SVG<-sen_att_analysis(stock="Sharpnose Sevengill shark",group="Pelagic_sharks")
SXG<-sen_att_analysis(stock="Bluntnose Sixgill shark",group="Pelagic_sharks")
PTH<-sen_att_analysis(stock="Thresher shark",group="Pelagic_sharks")

setwd("~/Climate projects/CVA/SensAttr_Scoring/Final_Run/score_vis/Pelagic_sharks")
file.remove("pelagic_sharks_final_plots.pdf")
file.list=list.files()
pdftools::pdf_combine(file.list, output  = "~/Climate projects/CVA/SensAttr_Scoring/Final_Run/score_vis/Pelagic_sharks/pelagic_sharks_final_plots.pdf")


#### Billfish/Swordfish stock run
BUM<-sen_att_analysis(stock="Blue marlin",group="Billfish_Swordfish")
SPF<-sen_att_analysis(stock="Longbill spearfish",group="Billfish_Swordfish")
SWO<-sen_att_analysis(stock="North Atlantic swordfish",group="Billfish_Swordfish")
SPG<-sen_att_analysis(stock="Roundscale spearfish",group="Billfish_Swordfish")
SAI<-sen_att_analysis(stock="West Atlantic sailfish",group="Billfish_Swordfish")
WHM<-sen_att_analysis(stock="White marlin",group="Billfish_Swordfish")

setwd("~/Climate projects/CVA/SensAttr_Scoring/Final_Run/score_vis/Billfish_Swordfish")
file.remove("bill_sword_final_plots.pdf")
file.list=list.files()
pdftools::pdf_combine(file.list, output  = "~/Climate projects/CVA/SensAttr_Scoring/Final_Run/score_vis/Billfish_Swordfish/bill_sword_final_plots.pdf")


#### Tuna stock run
BET<-sen_att_analysis(stock="Bigeye tuna",group="Tunas")
ALB<-sen_att_analysis(stock="North Atlantic_Mediterranean albacore tuna",group="Tunas")
BFT<-sen_att_analysis(stock="West Atlantic bluefin tuna",group="Tunas")
SKJ<-sen_att_analysis(stock="West Atlantic skipjack tuna",group="Tunas")
YFT<-sen_att_analysis(stock="Yellowfin tuna",group="Tunas")

setwd("~/Climate projects/CVA/SensAttr_Scoring/Final_Run/score_vis/Tunas")
file.remove("tuna_final_plots.pdf")
file.list=list.files()
pdftools::pdf_combine(file.list, output  = "~/Climate projects/CVA/SensAttr_Scoring/Final_Run/score_vis/Tunas/tuna_final_plots.pdf")


#combine sensitivity attribute scpre breakdown for all species
sen_att_scores<-data.frame(Group=c(rep("LCS",23),rep("SCS",14),rep("pelagic shark",10),rep("billfish/swordfish",6),rep("tuna",5)),
                           Species=c("blacktip (ATL)","basking","bigeye sand tiger","bignose","bull","Caribbean reef","dusky","Galapagos",
                                     "great hammerhead","blacktip (GOM)","lemon","narrowtooth","night","nurse","sand tiger","sandbar",
                                     "scalloped hammerhead","silky","smooth hammerhead","spinner","tiger","whale","white",
                                     "angel","blacknose (ATL)","blacknose (GOM)","bonnethead (ATL)","Atlantic sharpnose (ATL)","Atlantic sharpnose (GOM)",
                                     "Caribbean sharpnose","finetooth","Florida smoothhound","gulf smoothhound","bonnethead (GOM)","smalltail",
                                     "smooth dogfish (ATL)","smooth dogfish (GOM)","bigeye sixgill","bigeye thresher","longfin mako","blue",
                                     "shortfin mako","porbeagle","oceanic whitetip","sharpnose sevengill","bluntnose sixgill","common thresher","blue marlin",
                                     "longbill spearfish","swordfish","roundscale spearfish","Atlantic sailfish","white marlin","bigeye tuna",
                                     "albacore tuna","bluefin tuna","skipjack tuna","yellowfin tuna"),
                           Score=rbind(SBK$sen_att_score,BSG$sen_att_score,BST$sen_att_score,SBG$sen_att_score,SBU$sen_att_score,SRF$sen_att_score,DUS$sen_att_score,GAL$sen_att_score,GHH$sen_att_score,SBKgom$sen_att_score,
                                       LEM$sen_att_score,SNT$sen_att_score,SNI$sen_att_score,NUR$sen_att_score,SST$sen_att_score,SSB$sen_att_score,SPL$sen_att_score,FAL$sen_att_score,
                                       SHH$sen_att_score, SSP$sen_att_score,TIG$sen_att_score,WHA$sen_att_score,WHI$sen_att_score,ANG$sen_att_score,SBN$sen_att_score,SBNgom$sen_att_score,
                                       BON$sen_att_score,SAS$sen_att_score,SASgom$sen_att_score,SCS$sen_att_score,SFT$sen_att_score,FSH$sen_att_score,GSH$sen_att_score,BONgom$sen_att_score,STS$sen_att_score,DGS$sen_att_score,DGSgom$sen_att_score,
                                       BES$sen_att_score,BTH$sen_att_score,LMA$sen_att_score,BSH$sen_att_score,SMA$sen_att_score,POR$sen_att_score,OCS$sen_att_score,SVG$sen_att_score,
                                       SXG$sen_att_score,PTH$sen_att_score,BUM$sen_att_score,SPF$sen_att_score,SWO$sen_att_score,SPG$sen_att_score,SAI$sen_att_score,WHM$sen_att_score,BET$sen_att_score,
                                       ALB$sen_att_score,BFT$sen_att_score,SKJ$sen_att_score,YFT$sen_att_score),
                           MeanSD_Score=rbind(SBK$meansd,BSG$meansd,BST$meansd,SBG$meansd,SBU$meansd,SRF$meansd,DUS$meansd,GAL$meansd,GHH$meansd,SBKgom$meansd,
                                              LEM$meansd,SNT$meansd,SNI$meansd,NUR$meansd,SST$meansd,SSB$meansd,SPL$meansd,FAL$meansd,
                                              SHH$meansd, SSP$meansd,TIG$meansd,WHA$meansd,WHI$meansd,ANG$meansd,SBN$meansd,SBNgom$meansd,
                                              BON$meansd,SAS$meansd,SASgom$meansd,SCS$meansd,SFT$meansd,FSH$meansd,GSH$meansd,BONgom$meansd,STS$meansd,DGS$meansd,DGSgom$meansd,
                                              BES$meansd,BTH$meansd,LMA$meansd,BSH$meansd,SMA$meansd,POR$meansd,OCS$meansd,SVG$meansd,
                                              SXG$meansd,PTH$meansd,BUM$meansd,SPF$meansd,SWO$meansd,SPG$meansd,SAI$meansd,WHM$meansd,BET$meansd,
                                              ALB$meansd,BFT$meansd,SKJ$meansd,YFT$meansd))

sen_att_scores$Num_Score<-NA
sen_att_scores$Num_Score<-ifelse(sen_att_scores$Score=="very high",4,
                                 ifelse(sen_att_scores$Score=="high",3,
                                        ifelse(sen_att_scores$Score=="moderate",2,
                                               ifelse(sen_att_scores$Score=="low",1,NA))))


write.csv(sen_att_scores, file="~/Climate projects/CVA/SensAttr_Scoring/Final_Run/sen_att_scores_final20230716.csv",row.names = F)


### Manuscript figure: Distribution of mean scores of sensitivity attributes box & whisker plot

sen_att_deets<-data.frame(Group=c(rep("LCS",299),rep("SCS",182),rep("pelagic shark",130),rep("billfish/swordfish",78),rep("tuna",65)),
                          Species=c(rep("blacktip (ATL)",13), rep("basking",13), rep("bigeye sand tiger",13), rep("bignose",13), rep("bull",13), rep("Caribbean reef",13), rep("dusky",13), rep("Galapagos",13), 
                                    rep("great hammerhead",13), rep("blacktip (GOM)",13), rep("lemon",13), rep("narrowtooth",13), rep("night",13), rep("nurse",13), rep("sand tiger",13), rep("sandbar",13), 
                                    rep("scalloped hammerhead",13), rep("silky",13), rep("smooth hammerhead",13), rep("spinner",13), rep("tiger",13), rep("whale",13), rep("white",13), 
                                    rep("angel",13), rep("blacknose (ATL)",13), rep("blacknose (GOM)",13), rep("bonnethead (ATL)",13), rep("Atlantic sharpnose (ATL)",13), rep("Atlantic sharpnose (GOM)",13), 
                                    rep("Caribbean sharpnose",13), rep("finetooth",13), rep("Florida smoothhound",13), rep("gulf smoothhound",13), rep("bonnethead (GOM)",13), rep("smalltail",13), 
                                    rep("smooth dogfish (ATL)",13), rep("smooth dogfish (GOM)",13), rep("bigeye sixgill",13), rep("bigeye thresher",13), rep("longfin mako",13), rep("blue",13), 
                                    rep("shortfin mako",13), rep("porbeagle",13), rep("oceanic whitetip",13), rep("sharpnose sevengill",13), rep("bluntnose sixgill",13), rep("common thresher",13), rep("blue marlin",13), 
                                    rep("longbill spearfish",13), rep("swordfish",13), rep("roundscale spearfish",13), rep("Atlantic sailfish",13), rep("white marlin",13), rep("bigeye tuna",13), 
                                    rep("albacore tuna",13), rep("bluefin tuna",13), rep("skipjack tuna",13), rep("yellowfin tuna",13)), 
                          rbind(SBK$score_breakdown,BSG$score_breakdown,BST$score_breakdown,SBG$score_breakdown,SBU$score_breakdown,SRF$score_breakdown,DUS$score_breakdown,GAL$score_breakdown,GHH$score_breakdown,SBKgom$score_breakdown,
                                LEM$score_breakdown,SNT$score_breakdown,SNI$score_breakdown,NUR$score_breakdown,SST$score_breakdown,SSB$score_breakdown,SPL$score_breakdown,FAL$score_breakdown,
                                SHH$score_breakdown,SSP$score_breakdown,TIG$score_breakdown,WHA$score_breakdown,WHI$score_breakdown,ANG$score_breakdown,SBN$score_breakdown,SBNgom$score_breakdown,
                                BON$score_breakdown,SAS$score_breakdown,SASgom$score_breakdown,SCS$score_breakdown,SFT$score_breakdown,FSH$score_breakdown,GSH$score_breakdown,BONgom$score_breakdown,STS$score_breakdown,DGS$score_breakdown,DGSgom$score_breakdown,
                                BES$score_breakdown,BTH$score_breakdown,LMA$score_breakdown,BSH$score_breakdown,SMA$score_breakdown,POR$score_breakdown,OCS$score_breakdown,SVG$score_breakdown,
                                SXG$score_breakdown,PTH$score_breakdown,BUM$score_breakdown,SPF$score_breakdown,SWO$score_breakdown,SPG$score_breakdown,SAI$score_breakdown,WHM$score_breakdown,BET$score_breakdown,
                                ALB$score_breakdown,BFT$score_breakdown,SKJ$score_breakdown,YFT$score_breakdown))

sen_att_deets$sen_att_names[which(sen_att_deets$sen_att_names=="Stock Size_Status")]<-"Stock Size Status"

write.csv(sen_att_deets,file="~/Climate projects/CVA/SensAttr_Scoring/Final_Run/sen_att_means_allspecies_final20230716.csv",row.names = F)

#order by median
group_ordered <- with(sen_att_deets,                       
                      reorder(sen_att_names,
                              sen_att_mean,
                              median))

png("~/Climate projects/CVA/Manuscript_Figures/sensitivity_attribute_mean_scores.png", width=10, height=8,units="in",res=300)
par(mar=c(5,19,4,2))
boxplot(sen_att_mean~group_ordered,data=sen_att_deets,horizontal = T,las=1,xlab="Mean Score",ylab="",ylim=c(0.85,4),
        cex.lab=1.5)
mtext("Sensitivity Attribute",side=2,line=17.5,cex=1.5)
dev.off()


########## distribution shift potential #####
#based on four attributes: adult mobility, early life stage dispersal, habitat specificity, sensitivity to temperature
stockRec<-NULL
dist_ScoreRec<-NULL
adu_mobRec<-NULL
hab_speRec<-NULL
mob_disp_elsRec<-NULL
sen_tempRec<-NULL
for(i in stocks){
  #read in sensitivity attribute scores for species i
  sen_att_means<-read.csv(paste("~/Climate projects/CVA/SensAttr_Scoring/Final_Run/",i,"_senattscore.csv",sep=""))
  
  #only keep 4 sen atts we care about for dist shift potential
  dist_sen_att_means<-sen_att_means[which(sen_att_means$sen_att_names=="Adult Mobility" | sen_att_means$sen_att_names=="Habitat Specificity" | 
                                            sen_att_means$sen_att_names=="Mobility and Dispersal of Early Life Stages" | sen_att_means$sen_att_names=="Sensitivity to Temperature"),]
  
  #multiply LpRec, MpRec, HpRec, VpRec by 25 (total number of possible tallies) to get number of tallies for each group
  dist_sen_att_means$L<-dist_sen_att_means$LpRec*25
  dist_sen_att_means$M<-dist_sen_att_means$MpRec*25
  dist_sen_att_means$H<-dist_sen_att_means$HpRec*25
  dist_sen_att_means$V<-dist_sen_att_means$VpRec*25
  
  #reverse tallies for adult mobility, early life stage dispersal, and habitat specificity (ex. if 10 for V and 3 for L, switch them)
  dist_sen_att_means$Ldist<-dist_sen_att_means$L
  dist_sen_att_means$Mdist<-dist_sen_att_means$M
  dist_sen_att_means$Hdist<-dist_sen_att_means$H
  dist_sen_att_means$Vdist<-dist_sen_att_means$V
  
  dist_sen_att_means$Ldist[1:3]<-dist_sen_att_means$V[1:3]
  dist_sen_att_means$Mdist[1:3]<-dist_sen_att_means$H[1:3]
  dist_sen_att_means$Hdist[1:3]<-dist_sen_att_means$M[1:3]
  dist_sen_att_means$Vdist[1:3]<-dist_sen_att_means$L[1:3]
  
  #calculate weighted average
  w.dist.att.mean<-data.frame(sen_att_mean=(dist_sen_att_means$Ldist*1) + (dist_sen_att_means$Mdist*2) + (dist_sen_att_means$Hdist*3) + (dist_sen_att_means$Vdist*4)) / (25)
  
  dist_sen_att_score<-logic_model(w.dist.att.mean) #ignore warning...there's no sd calculation needed here
  dist_Score<-dist_sen_att_score$sen_att_score
  
  stockRec<-c(stockRec,i)
  dist_ScoreRec<-c(dist_ScoreRec,dist_Score)
  adu_mobRec<-c(adu_mobRec,w.dist.att.mean$sen_att_mean[1])
  hab_speRec<-c(hab_speRec,w.dist.att.mean$sen_att_mean[2])
  mob_disp_elsRec<-c(mob_disp_elsRec,w.dist.att.mean$sen_att_mean[3])
  sen_tempRec<-c(sen_tempRec,w.dist.att.mean$sen_att_mean[4])
  
}#ignore warning...there's no sd calculation needed here


dist_shift_potential<-data.frame(Stock.Name=stockRec,Distribution.Score=dist_ScoreRec,Adult.Mobility.Mean=adu_mobRec,
                                 Habitat.Specificity.Mean=hab_speRec,Mobility.Dispersal.ELS.Mean=mob_disp_elsRec,
                                 Sensitivity.Temp.Mean=sen_tempRec)

write.csv(dist_shift_potential,file="~/Climate projects/CVA/SensAttr_Scoring/Final_Run/Distribution_shift_potential_scores20230719.csv",row.names = F)

