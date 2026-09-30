################################################
# Purpose:
# Calculate vulnerability uncertainty by bootstrapping expert tallies for attributes
# Calculate distribution potential uncertainty by using same bootstrapped values
#The R code and datasets in this study uses the Gulf of Mexico to refer to the area now known as the Gulf of America consistent with Executive Order (E.O.) 14172 (Restoring Names That Honor American Greatness).
#read in S1 Dataset: Biological Sensitivity Attribute Scores wherever it is stored locally
ss<-read.csv("~/Climate projects/CVA/SensAttr_Scoring/export_stocks_csv__7.13.23.csv")
#read in exposure scores generated from S1_script
exp_scoresP1<-read.csv("~/Climate projects/CVA/Exposure_score_breakdown/exp_scores20230628.csv")
#read in sensitivity attribute scores generated from S2_script
sen_scoresP2<-read.csv("~/Climate projects/CVA/SensAttr_Scoring/Final_Run/sen_att_scores_final20230716.csv")
#read in S2 Table: S2 Table: Species Name Match List wherever it is stored locally
species_name_match<-read.csv("~/Climate projects/CVA/species_name_match.csv")
#read in vulnerability scores generated from S4_script
vul_final_scores<-read.csv("~/Climate projects/CVA/Vulnerability_Scores/vul_final_scores20230716.csv")
#read in S3 Table: Exposure Factor Abbreviations wherever it is stored locally
exp_names<-read.csv("~/Climate projects/CVA/exp_factor_name_match.csv")
#read in distribution shift potential scores generated from S2_script
dist_shift_potential<-read.csv("~/Climate projects/CVA/SensAttr_Scoring/Final_Run/Distribution_shift_potential_scores20230719.csv")
#read in S2 Dataset: Directional Effect Scores
direct_effects<-read.csv("~/Climate projects/CVA/Directional_Effects/Master Directional Effects - Worksheet20230801.csv")#tallies per expert by species

#remove chl and ohc700 because didn't use either for any species
exp_names<-exp_names[-which(exp_names$env_names=="ohc700"|exp_names$env_names=="chl"),]

#change stock size/status to stock size_status
ss$Attribute.Name[which(ss$Attribute.Name=="Stock Size/Status")]<-"Stock Size_Status"

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

#adjust stock names in ss df to match species_name_match
ss$Stock.Name[which(ss$Stock.Name=="North Atlantic / Mediterranean albacore tuna")]<-"North Atlantic albacore tuna"
ss$Stock.Name[which(ss$Stock.Name=="Bigeye tuna")]<-"Atlantic bigeye tuna"
ss$Stock.Name[which(ss$Stock.Name=="Atlantic sharpnose shark - GOM")]<-"Atlantic sharpnose shark - Gulf of Mexico"
ss$Stock.Name[which(ss$Stock.Name=="Smooth dogfish shark - Atlantic")]<-"Atlantic smooth dogfish - Atlantic"
ss$Stock.Name[which(ss$Stock.Name=="Smooth dogfish shark - Gulf of Mexico")]<-"Atlantic smooth dogfish - Gulf of Mexico"
ss$Stock.Name[which(ss$Stock.Name=="Bluntnose Sixgill shark")]<-"Bluntnose sixgill shark"
ss$Stock.Name[which(ss$Stock.Name=="Sharpnose Sevengill shark")]<-"Sharpnose sevengill shark"
ss$Stock.Name[which(ss$Stock.Name=="White Shark")]<-"White shark"

#adjust stock names in dist_shift_potential df to match species_name_match
dist_shift_potential$Stock.Name[which(dist_shift_potential$Stock.Name=="Bigeye tuna")]<-"Atlantic bigeye tuna"
dist_shift_potential$Stock.Name[which(dist_shift_potential$Stock.Name=="Yellowfin tuna")]<-"Atlantic yellowfin tuna"
dist_shift_potential$Stock.Name[which(dist_shift_potential$Stock.Name=="North Atlantic_Mediterranean albacore tuna")]<-"North Atlantic albacore tuna"
dist_shift_potential$Stock.Name[which(dist_shift_potential$Stock.Name=="Atlantic sharpnose shark - GOM")]<-"Atlantic sharpnose shark - Gulf of Mexico"
dist_shift_potential$Stock.Name[which(dist_shift_potential$Stock.Name=="Sharpnose Sevengill shark")]<-"Sharpnose sevengill shark"
dist_shift_potential$Stock.Name[which(dist_shift_potential$Stock.Name=="Bluntnose Sixgill shark")]<-"Bluntnose sixgill shark"
dist_shift_potential$Stock.Name[which(dist_shift_potential$Stock.Name=="Smooth dogfish shark - Atlantic")]<-"Atlantic smooth dogfish - Atlantic"
dist_shift_potential$Stock.Name[which(dist_shift_potential$Stock.Name=="Smooth dogfish shark - Gulf of Mexico")]<-"Atlantic smooth dogfish - Gulf of Mexico"
dist_shift_potential$Stock.Name[which(dist_shift_potential$Stock.Name=="White Shark")]<-"White shark"

#adjust names in direct effects df to match species_name_match
direct_effects$Species[which(direct_effects$Species=="North Atlantic / Mediterranean albacore tuna")]<-"North Atlantic_Mediterranean albacore tuna"
direct_effects$Species[which(direct_effects$Species=="White shark")]<-"White Shark"
direct_effects$Species[which(direct_effects$Species=="Atlantic sharpnose shark - Gulf of Mexico")]<-"Atlantic sharpnose shark - GOM"

#use code names for species names
ss<-merge(ss,species_name_match)

stocks<-unique(ss$Code.Name)
ef_species_names<-unique(ss$EF.Stock.Name)
atts<-unique(ss$Attribute.Name)



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
  
  return(list(score_breakdown=dat,sen_att_score=sen_att_score))
}

#same logic model just modified names for exposure factor
logic_modelEF<-function(dat){
  
  if(length(which(dat$exp_fact_mean>=3.5))>=3){
    Score<-"very high"
  }else if(length(which(dat$exp_fact_mean>=3))>=2){
    Score<-"high"
  }else if(length(which(dat$exp_fact_mean>=2.5))>=2){
    Score<-"moderate"
  }else{
    Score<-"low"
  }
  
  exp_scores<-Score
  
  return(list(score_breakdown=dat,exp_scores=exp_scores))
}


###### Calculate Vulnerability Uncertainty, Distribution Potential Uncertainty using bootstrapping #####

vul_uncert<-function(stock){
  
  s<-which(ss$Stock.Name==stock)
  s1<-ss[s,]
  
  #isolate exposure factor score
  code.name<-unique(ss$Code.Name[which(ss$Stock.Name==stock)])
  species_exp_score<-exp_scoresP1$Num_Score[which(exp_scoresP1$Species==code.name)]
  
  #what is the final vulnerability rank for the species (need this for later after function)
  final_vul_rank<-vul_final_scores$Final_Rank[which(vul_final_scores$Species==code.name)]
  
  #what is the final distribution potential score for the species (need this for later after function)
  final_dist_score<-dist_shift_potential$Distribution.Score[which(dist_shift_potential$Stock.Name==stock)]
  
  vul_rankRec<-NULL
  dist_ScoreRec<-NULL
  
  set.seed(99)
  for(i in 1:10000){
    
    w.att.meanRec<-NULL
    w.dist.att.meanRec<-NULL
    att.nameRec<-NULL
    
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
      
      #create draw pile based on number of tallies in each category
      draw_pile<-c(rep(1,L),rep(2,M),rep(3,H),rep(4,V))
      
      #randomly sample with replacement 25 (5 tallies x 5 experts) from draw pile
      samp<-sample(x=draw_pile,size=25,replace=TRUE)
      
      #table of 1,2,3,4 in samp
      samp.tab<-data.frame(table(samp))
      
      sampL<-ifelse(any(unique(samp)==1),samp.tab$Freq[which(samp.tab$samp==1)],0)
      sampM<-ifelse(any(unique(samp)==2),samp.tab$Freq[which(samp.tab$samp==2)],0)
      sampH<-ifelse(any(unique(samp)==3),samp.tab$Freq[which(samp.tab$samp==3)],0)
      sampV<-ifelse(any(unique(samp)==4),samp.tab$Freq[which(samp.tab$samp==4)],0)
      
      #calculate weighted average
      w.att.mean<-((sampL*1) + (sampM*2) + (sampH*3) + (sampV*4)) / (num.experts * 5)
      
      #recalculate weighted average assuming distribution potential (technically only need for 3 attributes, but will calculate for all here and isolate 3 later)
      w.dist.att.mean<-((sampL*4) + (sampM*3) + (sampH*2) + (sampV*1)) / (num.experts * 5)
      
      #store weighted means for each attribute
      w.att.meanRec<-c(w.att.meanRec,w.att.mean)
      
      #store weighted distribution potential mean for each attribute (again only need for 3 put will isolate those 3 after loop)
      w.dist.att.meanRec<-c(w.dist.att.meanRec,w.dist.att.mean)
      
      #store attribute name
      att.nameRec<-c(att.nameRec,att.name)
    }
    
    #put weighted averages in df
    sen_att_means<-data.frame(sen_att_names=att.nameRec,sen_att_mean=w.att.meanRec, sen_att_dist_mean=w.dist.att.meanRec)
    
    #put means for iteration i through logic model
    sen_att_score<-logic_model(dat=sen_att_means)
    
    #assign number score to sensitivty score
    sen_att_num_score<-ifelse(sen_att_score$sen_att_score=="very high",4,
                              ifelse(sen_att_score$sen_att_score=="high",3,
                                     ifelse(sen_att_score$sen_att_score=="moderate",2,
                                            ifelse(sen_att_score$sen_att_score=="low",1,NA))))
    
    #calculate final vulnerability score
    vul_num_score<-species_exp_score * sen_att_num_score
    
    #assign final vulnerability ranking
    vul_rank<-ifelse(vul_num_score<=3,"low",
                     ifelse(vul_num_score>=4 & vul_num_score<=6, "moderate",
                            ifelse(vul_num_score>=8 & vul_num_score<=9, "high",
                                   ifelse(vul_num_score>=12,"very high",NA))))
    
    #record vul_rank
    vul_rankRec<-c(vul_rankRec,vul_rank)
    
    #calculate distribution potential using adult mobility, early life stage dispersal, habitat specificity, sensitivity to temperature
    #use reverse means for adult mobility, early life stage dispersal, habitat specificity
    #use normal mean for sensitivity to temperature
    dist_sen_att_means<-sen_att_means[which(sen_att_means$sen_att_names=="Adult Mobility" | sen_att_means$sen_att_names=="Habitat Specificity" | 
                                              sen_att_means$sen_att_names=="Mobility and Dispersal of Early Life Stages" | sen_att_means$sen_att_names=="Sensitivity to Temperature"),]
    dist_sen_att_means$sen_att_dist_mean_use<-dist_sen_att_means$sen_att_dist_mean
    dist_sen_att_means$sen_att_dist_mean_use[which(dist_sen_att_means$sen_att_names=="Sensitivity to Temperature")]<-dist_sen_att_means$sen_att_mean[which(dist_sen_att_means$sen_att_names=="Sensitivity to Temperature")]#use actual mean for sens to temp
    
    #rename column headers so that it can go through logic model
    dist_sen_att_means<-data.frame(sen_att_names=dist_sen_att_means$sen_att_names,sen_att_mean=dist_sen_att_means$sen_att_dist_mean_use)
    
    #put distribution potential means for iteration i through logic model
    dist_sen_att_score<-logic_model(dat=dist_sen_att_means) #ignore warning...there's no sd calculation needed here
    dist_Score<-dist_sen_att_score$sen_att_score
    
    #record distribution potential score
    dist_ScoreRec<-c(dist_ScoreRec,dist_Score)
    
    print(i)
  }
  
  #put vul ranks in table
  vul_rank_tab<-data.frame(table(vul_rankRec))
  
  #calculate percent for each category
  perL<-round(ifelse(any(vul_rank_tab$vul_rankRec=="low"),(vul_rank_tab$Freq[which(vul_rank_tab$vul_rankRec=="low")]/10000)*100,0),digits=0)
  perM<-round(ifelse(any(vul_rank_tab$vul_rankRec=="moderate"),(vul_rank_tab$Freq[which(vul_rank_tab$vul_rankRec=="moderate")]/10000)*100,0),digits=0)
  perH<-round(ifelse(any(vul_rank_tab$vul_rankRec=="high"),(vul_rank_tab$Freq[which(vul_rank_tab$vul_rankRec=="high")]/10000)*100,0),digits=0)
  perV<-round(ifelse(any(vul_rank_tab$vul_rankRec=="very high"),(vul_rank_tab$Freq[which(vul_rank_tab$vul_rankRec=="very high")]/10000)*100,0),digits=0)
  
  #put distribution potential scores in table
  dist_Score_tab<-data.frame(table(dist_ScoreRec))
  
  #calculate percent for each category
  perL_dist<-round(ifelse(any(dist_Score_tab$dist_ScoreRec=="low"),(dist_Score_tab$Freq[which(dist_Score_tab$dist_ScoreRec=="low")]/10000)*100,0),digits=0)
  perM_dist<-round(ifelse(any(dist_Score_tab$dist_ScoreRec=="moderate"),(dist_Score_tab$Freq[which(dist_Score_tab$dist_ScoreRec=="moderate")]/10000)*100,0),digits=0)
  perH_dist<-round(ifelse(any(dist_Score_tab$dist_ScoreRec=="high"),(dist_Score_tab$Freq[which(dist_Score_tab$dist_ScoreRec=="high")]/10000)*100,0),digits=0)
  perV_dist<-round(ifelse(any(dist_Score_tab$dist_ScoreRec=="very high"),(dist_Score_tab$Freq[which(dist_Score_tab$dist_ScoreRec=="very high")]/10000)*100,0),digits=0)
  
  #put into final table
  uncertainty_tab<-data.frame(Species=c(code.name,code.name,code.name,code.name),Category=c("low","moderate","high","very high"),Uncertainty=c(perL,perM,perH,perV),Dist_Uncertainty=c(perL_dist,perM_dist,perH_dist,perV_dist))
  
  write.csv(uncertainty_tab, file=paste("~/Climate projects/CVA/Vulnerability_Scores/Uncertainty_Scores/",code.name,"_uncertscore.csv",sep=""),row.names = F)
  
  #include final_vul_rank so that I can identify what uncertainty score to give each species after function
  return(list(uncertainty_tab=uncertainty_tab,final_vul_rank=final_vul_rank,final_dist_score=final_dist_score))
  
}

vul_final_scores$Final_Rank_Uncertainty<-NA

#### Tuna stock run
BET_uncert<-vul_uncert(stock="Atlantic bigeye tuna")
ALB_uncert<-vul_uncert(stock="North Atlantic albacore tuna")
BFT_uncert<-vul_uncert(stock="West Atlantic bluefin tuna")
SKJ_uncert<-vul_uncert(stock="West Atlantic skipjack tuna")
YFT_uncert<-vul_uncert(stock="Atlantic yellowfin tuna")

#### Billfish/Swordfish stock run
BUM_uncert<-vul_uncert(stock="Blue marlin")
SPF_uncert<-vul_uncert(stock="Longbill spearfish")
SWO_uncert<-vul_uncert(stock="North Atlantic swordfish")
SPG_uncert<-vul_uncert(stock="Roundscale spearfish")
SAI_uncert<-vul_uncert(stock="West Atlantic sailfish")
WHM_uncert<-vul_uncert(stock="White marlin")

#### Pelagic Shark stock run
BES_uncert<-vul_uncert(stock="Bigeye sixgill shark")
BTH_uncert<-vul_uncert(stock="Bigeye thresher shark")
LMA_uncert<-vul_uncert(stock="Longfin mako shark")
BSH_uncert<-vul_uncert(stock="North Atlantic blue shark")
SMA_uncert<-vul_uncert(stock="North Atlantic shortfin mako shark")
POR_uncert<-vul_uncert(stock="Northwest Atlantic porbeagle shark")
OCS_uncert<-vul_uncert(stock="Oceanic whitetip shark")
SVG_uncert<-vul_uncert(stock="Sharpnose sevengill shark")
SXG_uncert<-vul_uncert(stock="Bluntnose sixgill shark")
PTH_uncert<-vul_uncert(stock="Thresher shark")

#### SCS/smoothhound stock run
ANG_uncert<-vul_uncert(stock="Atlantic angel shark")
SBN_uncert<-vul_uncert(stock="Atlantic blacknose shark - Atlantic")
SBNgom_uncert<-vul_uncert(stock="Atlantic blacknose shark - Gulf of Mexico")
BON_uncert<-vul_uncert(stock="Atlantic bonnethead")
SAS_uncert<-vul_uncert(stock="Atlantic sharpnose shark - Atlantic")
SASgom_uncert<-vul_uncert(stock="Atlantic sharpnose shark - Gulf of Mexico")
SCS_uncert<-vul_uncert(stock="Caribbean sharpnose shark")
SFT_uncert<-vul_uncert(stock="Finetooth shark")
FSH_uncert<-vul_uncert(stock="Florida smoothhound shark")
GSH_uncert<-vul_uncert(stock="Gulf smoothhound shark")
BONgom_uncert<-vul_uncert(stock="Gulf of Mexico bonnethead shark")
STS_uncert<-vul_uncert(stock="Smalltail shark")
DGS_uncert<-vul_uncert(stock="Atlantic smooth dogfish - Atlantic")
DGSgom_uncert<-vul_uncert(stock="Atlantic smooth dogfish - Gulf of Mexico")


#### LCS stock run
SBK_uncert<-vul_uncert(stock="Atlantic blacktip shark")
BSG_uncert<-vul_uncert(stock="Basking shark")
BST_uncert<-vul_uncert(stock="Bigeye sand tiger shark")
SBG_uncert<-vul_uncert(stock="Bignose shark")
SBU_uncert<-vul_uncert(stock="Bull shark")
SRF_uncert<-vul_uncert(stock="Caribbean reef shark")
DUS_uncert<-vul_uncert(stock="Dusky shark")
GAL_uncert<-vul_uncert(stock="Galapagos shark")
GHH_uncert<-vul_uncert(stock="Great hammerhead shark")
SBKgom_uncert<-vul_uncert(stock="Gulf of Mexico blacktip shark")
LEM_uncert<-vul_uncert(stock="Lemon shark")
SNT_uncert<-vul_uncert(stock="Narrowtooth shark")
SNI_uncert<-vul_uncert(stock="Night shark")
NUR_uncert<-vul_uncert(stock="Nurse shark")
SST_uncert<-vul_uncert(stock="Sand tiger shark")
SSB_uncert<-vul_uncert(stock="Sandbar shark")
SPL_uncert<-vul_uncert(stock="Scalloped hammerhead shark")
FAL_uncert<-vul_uncert(stock="Silky shark")
SHH_uncert<-vul_uncert(stock="Smooth hammerhead shark")
SSP_uncert<-vul_uncert(stock="Spinner shark")
TIG_uncert<-vul_uncert(stock="Tiger shark")
WHA_uncert<-vul_uncert(stock="Whale shark")
WHI_uncert<-vul_uncert(stock="White shark")


#add vulernability uncertainty score, distribution potential score, and distribution potential uncertainty score to vul_final_scores
vul_final_scores$Final_Rank_Uncertainty<-NA
vul_final_scores$Distribution_Score<-NA
vul_final_scores$Distribution_Score_Uncertainty<-NA
for(i in stocks){
  vul_loc<-which(vul_final_scores$Species==i)
  vul_sp<-vul_final_scores[vul_loc,]
  abr<-species_name_match$Abbreviation[which(species_name_match$Code.Name==i)]
  #searches name of object 
  uncert_tab <- get(ls(pattern = abr))
  #for species like bigeye sand tiger, NA is under official vul rank so need an if statement
  if(sum(uncert_tab$uncertainty_tab$Uncertainty)>0){
    vul_final_scores$Final_Rank_Uncertainty[vul_loc]<-uncert_tab$uncertainty_tab$Uncertainty[which(uncert_tab$uncertainty_tab$Category==uncert_tab$final_vul_rank)]
  }else{
    vul_final_scores$Final_Rank_Uncertainty[vul_loc]<-NA
  }
  vul_final_scores$Distribution_Score[vul_loc]<-uncert_tab$final_dist_score
  vul_final_scores$Distribution_Score_Uncertainty[vul_loc]<-uncert_tab$uncertainty_tab$Dist_Uncertainty[which(uncert_tab$uncertainty_tab$Category==uncert_tab$final_dist_score)]
}

### Manuscript figure: Potential for distribution change bar plot

dist_pot<-subset(vul_final_scores,select=c(Species,Group,Distribution_Score,Distribution_Score_Uncertainty))
dist_pot$Distribution_Score1<-ifelse(dist_pot$Distribution_Score=="very high",1,
                                     ifelse(dist_pot$Distribution_Score=="high",2,
                                            ifelse(dist_pot$Distribution_Score=="moderate",3,
                                                   ifelse(dist_pot$Distribution_Score=="low",4,NA))))

dist_pot$Distribution_Score_Uncertainty1<-ifelse(dist_pot$Distribution_Score_Uncertainty>95,1,
                                                 ifelse(dist_pot$Distribution_Score_Uncertainty<=95 & dist_pot$Distribution_Score_Uncertainty>90,2,
                                                        ifelse(dist_pot$Distribution_Score_Uncertainty<=90 & dist_pot$Distribution_Score_Uncertainty>=66,3,4)))

#reorder dist pot to go distribution score:very high-low, then dist score uncert: very high-low, then species name
dist_pot<-dist_pot[with(dist_pot,(order(Distribution_Score1,Distribution_Score_Uncertainty1,Species))),]
dist_pot_plot<-c(0,rev(table(dist_pot$Distribution_Score1)))
cols<-c("green","yellow","orange","red")
png("~/Climate projects/CVA/Manuscript_Figures/Distribution_Potential.png", width=11, height=10,units="in",res=300)
bp<-barplot(height=dist_pot_plot,names.arg=c("Low","Moderate","High","Very High"),col=cols,cex.names=1.5,ylab="Number of Species",cex.axis=1.5,cex.lab=1.5)
dev.off()



###### Directional Effects and Uncertainty using bootstrapping ####
stock.names<-unique(species_name_match$Stock.Name)

direct_effects[is.na(direct_effects)]<-0

vul_final_scores$Direction_Effects_Mean<-NA
vul_final_scores$Direction_Effects_Rank<-NA
vul_final_scores$Directional_Effects_Uncertainty<-NA

for(i in stock.names){
  code.name<-species_name_match$Code.Name[which(species_name_match$Stock.Name==i)]
  
  vul_loc<-which(vul_final_scores$Species==code.name)
  vul_sp<-vul_final_scores[vul_loc,]
  
  d<-which(direct_effects$Species==i)
  d1<-direct_effects[d,]
  
  num.experts<-5
  
  num_negs<-sum(d1$Negative)
  num_neu<-sum(d1$Neutral)
  num_pos<-sum(d1$Positive)
  
  #calculate weighted average
  w.mean<-((num_negs*-1) + (num_neu*0) + (num_pos*1)) / (num.experts * 4)
  
  #ranking
  rank<-ifelse(w.mean<= -0.33,"negative",
               ifelse(w.mean>=0.33,"positive","neutral"))
  
  vul_final_scores$Direction_Effects_Mean[vul_loc]<-w.mean
  vul_final_scores$Direction_Effects_Rank[vul_loc]<-rank
  
  #create draw pile based on number of tallies in each category
  draw_pile<-c(rep("neg",num_negs),rep("neu",num_neu),rep("pos",num_pos))
  
  set.seed(15)
  rankRec<-NULL
  for(j in 1:10000){
    #randomly sample with replacement 20 (4 tallies x 5 experts) from draw pile
    samp<-sample(x=draw_pile,size=20,replace=TRUE)
    
    #table of neg, neu, pos in samp
    samp.tab<-data.frame(table(samp))
    
    sampNEG<-ifelse(any(unique(samp)=="neg"),samp.tab$Freq[which(samp.tab$samp=="neg")],0)
    sampNEU<-ifelse(any(unique(samp)=="neu"),samp.tab$Freq[which(samp.tab$samp=="neu")],0)
    sampPOS<-ifelse(any(unique(samp)=="pos"),samp.tab$Freq[which(samp.tab$samp=="pos")],0)
    
    #calculate weighted average
    w.mean<-((sampNEG*-1) + (sampNEU*0) + (sampPOS*1)) / (num.experts * 4)
    
    #ranking
    rank<-ifelse(w.mean<= -0.33,"negative",
                 ifelse(w.mean>=0.33,"positive","neutral"))
    
    rankRec<-c(rankRec,rank)
  }
  direct_effects_tab<-data.frame(table(rankRec))
  
  #calculate percent for each category
  perNEG<-round(ifelse(any(direct_effects_tab$rankRec=="negative"),(direct_effects_tab$Freq[which(direct_effects_tab$rankRec=="negative")]/10000)*100,0),digits=0)
  perNEU<-round(ifelse(any(direct_effects_tab$rankRec=="neutral"),(direct_effects_tab$Freq[which(direct_effects_tab$rankRec=="neutral")]/10000)*100,0),digits=0)
  perPOS<-round(ifelse(any(direct_effects_tab$rankRec=="positive"),(direct_effects_tab$Freq[which(direct_effects_tab$rankRec=="positive")]/10000)*100,0),digits=0)
  
  #put into final table
  direct_effects_uncertainty_tab<-data.frame(Species=c(code.name,code.name,code.name),Category=c("negative","neutral","positive"),Uncertainty=c(perNEG,perNEU,perPOS))
  
  #save directional effects uncertainty table
  write.csv(direct_effects_uncertainty_tab, file=paste("~/Climate projects/CVA/Directional_Effects/",code.name,"_direct_effects_uncert.csv",sep=""),row.names = F)
  
  #add uncertainty value for selected directional effect in vul_final_scores
  vul_final_scores$Directional_Effects_Uncertainty[vul_loc]<-direct_effects_uncertainty_tab$Uncertainty[which(direct_effects_uncertainty_tab$Category==rank)]
  
  print(i)
}


### Manuscript figure: Directional effects bar plot

dir_eff_plot<-c(0,rev(table(vul_final_scores$Direction_Effects_Rank)))
cols<-c("green","beige","red")
png("~/Climate projects/CVA/Manuscript_Figures/Directional_Effects.png", width=11, height=10,units="in",res=300)
bp<-barplot(height=dir_eff_plot,names.arg=c("Positive","Neutral","Negative"),col=cols,cex.names=1.5,ylab="Number of Species",cex.axis=1.5,cex.lab=1.5)
dev.off()



#### LAST OUTPUT FOR vul_final_scores! ###
#write vul_final_scores with uncertainty values
write.csv(vul_final_scores,file="~/Climate projects/CVA/Vulnerability_Scores/vul_dist_direct_uncert_final_scores20230802.csv",row.names = F)


###### Sensitivity of Vulnerability Ranking to each Sensitivity Attributes and Exposure Factors (Leave-One-Out) #######


#create matrix to put results in
loo_saRec<-matrix(data=NA,nrow=58,ncol=13)
loo_efRec<-matrix(data=NA,nrow=58,ncol=12)

#goal is to go through each species one at a time in loop
#within stock loop, leave one senitivity attribute out and recalculate vulnerability score, then leave one exposure factor out and recalculate vul score
for(i in 1:length(stocks)){
  s<-which(ss$Code.Name==stocks[i])
  s1<-ss[s,]
  
  full_stock_name<-unique(s1$Stock.Name)
  
  species<-stocks[i]
  ef_species_name<-ef_species_names[i]
  
  attributes<-unique(ss$Short.Attribute)
  exposures<-unique(exp_names$env_names)
  
  #read in individual sen attr score for species i
  iso_fileSA<-list.files(path="C:/Users/dan.crear/Documents/Climate projects/CVA/SensAttr_Scoring/Final_Run",pattern=full_stock_name)
  species_sen_scores<-read.csv(paste("C:/Users/dan.crear/Documents/Climate projects/CVA/SensAttr_Scoring/Final_Run/",iso_fileSA,sep=""))
  
  #read in individual exp fact score for species i
  iso_fileEF<-list.files(path="C:/Users/dan.crear/Documents/Climate projects/CVA/Exposure_score_breakdown/official_run",pattern=paste(ef_species_name,"_",sep=""))
  #for species that we have no exposure factor score for
  #if isofileEF is a character (0) just put NAs in matrix
  if(identical(iso_fileEF,character(0))){
    loo_saRec[i,]<-NA
    loo_efRec[i,]<-NA
  }else{ #if not character (0) then run everything else as normal
    species_exp_scores<-read.csv(paste("C:/Users/dan.crear/Documents/Climate projects/CVA/Exposure_score_breakdown/official_run/",iso_fileEF,sep=""))
    
    
    ### Part 1: leave one attribute out at a time, calculate sensitivity score (P1)
    for(y in 1:length(attributes)){
      #remove row for sensitivity attribute we are leaving out
      sen_att_loo<-species_sen_scores[-y,]
      #run logic model on sensitivity attribute scores
      sen_att_scoreP1<-logic_model(dat=sen_att_loo)
      #number for sensitivity attribute
      sen_att_scoreNumP1<-ifelse(sen_att_scoreP1$sen_att_score=="low",1,
                                 ifelse(sen_att_scoreP1$sen_att_score=="moderate",2,
                                        ifelse(sen_att_scoreP1$sen_att_score=="high",3,
                                               4)))
      #number for exposure factor from exp_scoresP1 dataset
      exp_fact_scoreNumP1<-exp_scoresP1$Num_Score[which(exp_scoresP1$Species==species)]
      #vulnerability score number
      vulscoreNumP1<-sen_att_scoreNumP1*exp_fact_scoreNumP1
      #get recalculated vulnerability score
      vul_scoreP1<-ifelse(vulscoreNumP1<=3,"low",
                          ifelse(vulscoreNumP1>=4 & vulscoreNumP1<=6, "moderate",
                                 ifelse(vulscoreNumP1>=8 & vulscoreNumP1<=9, "high",
                                        ifelse(vulscoreNumP1>=12,"very high",NA))))
      
      #put recalculated vulnerability score in loo_saRec
      loo_saRec[i,y]<-vul_scoreP1
      
    }
    
    ### Part 2: leave one exposure factor out at a time, calculate sensitivity score (P2)
    for(z in 1:length(exposures)){
      #df of all exposure factors
      exposures_df<-data.frame(env_names=exposures)
      #add full exposure factor name to df
      exposures_df<-merge(exposures_df,exp_names,by="env_names")
      #merge to have all exposure factors in one df because only used certain EF for certain species
      full_exp_fact<-merge(exposures_df,species_exp_scores,by="env_names",all=T)
      #remove row for exposure factor we are leaving out
      exp_fact_loo<-full_exp_fact[-z,]
      #run logic model on exposure factor scores
      exp_fact_scoreP2<-logic_modelEF(dat=exp_fact_loo)
      #number for exposure factor
      exp_fact_scoreNumP2<-ifelse(exp_fact_scoreP2$exp_scores=="low",1,
                                  ifelse(exp_fact_scoreP2$exp_scores=="moderate",2,
                                         ifelse(exp_fact_scoreP2$exp_scores=="high",3,
                                                4)))
      #number for sensitivity attribute from sen_scoresP2 dataset
      sen_att_scoreNumP2<-sen_scoresP2$Num_Score[which(sen_scoresP2$Species==species)]
      #vulnerability score number
      vulscoreNumP2<-exp_fact_scoreNumP2*sen_att_scoreNumP2
      #get recalculated vulnerability score
      vul_scoreP2<-ifelse(vulscoreNumP2<=3,"low",
                          ifelse(vulscoreNumP2>=4 & vulscoreNumP2<=6, "moderate",
                                 ifelse(vulscoreNumP2>=8 & vulscoreNumP2<=9, "high",
                                        ifelse(vulscoreNumP2>=12,"very high",NA))))
      
      #put recalculated vulnerability score in loo_saRec
      loo_efRec[i,z]<-vul_scoreP2
    }
  }
}

#turn each matrix to df
loo_sa_df<-data.frame(loo_saRec)
colnames(loo_sa_df)<-species_sen_scores$sen_att_names #make sure sens att order is correct (same order of y loop)
loo_sa_df<-cbind(Species=stocks,loo_sa_df) #make sure stocks order is correct (same order as i loop)

loo_ef_df<-data.frame(loo_efRec)
colnames(loo_ef_df)<-full_exp_fact$Exposure.Factor #make sure exp fact order is correct (same order of z loop)
loo_ef_df<-cbind(Species=stocks,loo_ef_df) #make sure stocks order is correct (same order as i loop)

#add actual vul score in final column
loo_sa_df<-merge(loo_sa_df,vul_final_scores[ , c("Species", "Final_Rank")],by="Species")
loo_ef_df<-merge(loo_ef_df,vul_final_scores[ , c("Species", "Final_Rank")],by="Species")


#### compare each leave-one-out with Final Rank for each species
## Sensitivity attributes
loo_sa_df_compare<-loo_sa_df
loo_sa_df_compare[2:14]<-loo_sa_df[2:14]==loo_sa_df$Final_Rank
loo_sa_df_compare[2:14]<-ifelse(loo_sa_df_compare[2:14]=="FALSE",1,0) #make all false values equal to 1, then sum 1s to get number of falses
#sum number of FALSES per column
loo_sa_sums<-colSums(loo_sa_df_compare[2:14],na.rm=T)

library(dplyr)
sen_att_names<-species_sen_scores$sen_att_names
sen_att_names<-recode(sen_att_names,`Stock Size_Status`="Stock Size Status")#need to remove "_" in name


### Manuscript figure: Leave-one-out analysis bar plot

png("~/Climate projects/CVA/Manuscript_Figures/change_vul_sens_att_loo.png", width=8, height=9,units="in",res=300)
par(mar=c(20,4,1,1))
barplot(height=loo_sa_sums,names.arg = sen_att_names,las=2,col="black",
        ylab="Number of Changes in Climate Vulnerability",ylim=c(0,12))
dev.off()

## Exposure factors
loo_ef_df_compare<-loo_ef_df
loo_ef_df_compare[2:13]<-loo_ef_df[2:13]==loo_ef_df$Final_Rank
loo_ef_df_compare[2:13]<-ifelse(loo_ef_df_compare[2:13]=="FALSE",1,0) #make all false values equal to 1, then sum 1s to get number of falses
#sum number of FALSES per column
loo_ef_sums<-colSums(loo_ef_df_compare[2:13],na.rm=T)

png("~/Climate projects/CVA/Manuscript_Figures/change_vul_exp_fact_loo.png", width=8, height=9,units="in",res=300)
par(mar=c(21,4,1,1))
barplot(height=loo_ef_sums,names.arg = full_exp_fact$Exposure.Factor,las=2,col="black",
        ylab="Number of Changes in Climate Vulnerability",ylim=c(0,35))
dev.off()


###### By Species Group figure #####

main_scores<-subset(vul_final_scores,select=c(Species,Group,Final_Rank,Distribution_Score,Direction_Effects_Rank))
main_scores$Final_Rank<-ifelse(main_scores$Final_Rank=="very high",4,
                               ifelse(main_scores$Final_Rank=="high",3,
                                      ifelse(main_scores$Final_Rank=="moderate",2,
                                             ifelse(main_scores$Final_Rank=="low",1,NA))))
main_scores$Distribution_Score<-ifelse(main_scores$Distribution_Score=="very high",4,
                                       ifelse(main_scores$Distribution_Score=="high",3,
                                              ifelse(main_scores$Distribution_Score=="moderate",2,
                                                     ifelse(main_scores$Distribution_Score=="low",1,NA))))
main_scores$Direction_Effects_Rank<-ifelse(main_scores$Direction_Effects_Rank=="positive",1,
                                           ifelse(main_scores$Direction_Effects_Rank=="neutral",2,
                                                  ifelse(main_scores$Direction_Effects_Rank=="negative",3,NA)))


final_rank_org<-aggregate(main_scores$Final_Rank,list(main_scores$Group,main_scores$Final_Rank),length,drop=FALSE)
final_rank_org<-final_rank_org[with(final_rank_org,order(Group.1)),]
final_rank_org$x[is.na(final_rank_org$x)]<-0

dist_rank_org<-aggregate(main_scores$Distribution_Score,list(main_scores$Group,main_scores$Distribution_Score),length,drop=FALSE)
missingscores<-data.frame(Group.1=c("billfish/swordfish","LCS","SCS","tuna","pelagic shark"),Group.2=rep(1,5),x=rep(0,5))
dist_rank_org<-rbind(dist_rank_org,missingscores)
dist_rank_org<-dist_rank_org[with(dist_rank_org,order(Group.1,Group.2)),]
dist_rank_org$x[is.na(dist_rank_org$x)]<-0

dir_eff_rank_org<-aggregate(main_scores$Direction_Effects_Rank,list(main_scores$Group,main_scores$Direction_Effects_Rank),length,drop=FALSE)
dir_eff_rank_org<-rbind(dir_eff_rank_org,missingscores)
dir_eff_rank_org<-dir_eff_rank_org[with(dir_eff_rank_org,order(Group.1,Group.2)),]
dir_eff_rank_org$x[is.na(dir_eff_rank_org$x)]<-0


cols4<-c("green","yellow","orange","red")
cols3<-c("green","beige","red")
png("~/Climate projects/CVA/Manuscript_Figures/num_species_func_group_vul_dist_dir.png", width=10, height=10,units="in",res=300)
par(mfrow=c(5,3))
par(oma=c(3,1,1,1))
par(mar=c(3,3,1,1))
barplot(height=final_rank_org$x[which(final_rank_org$Group.1=="billfish/swordfish")],col=cols4,ylim=c(0,4),cex.axis=1.5)
text(x=0.2,y=3.5,"Billfish & Swordfish",cex=1.4,adj=c(0,0))
barplot(height=dist_rank_org$x[which(dist_rank_org$Group.1=="billfish/swordfish")],col=cols4,ylim=c(0,6),cex.axis=1.5)
text(x=0.2,y=4.5,"Billfish &\nSwordfish",cex=1.4,adj=c(0,0))
barplot(height=dir_eff_rank_org$x[which(dir_eff_rank_org$Group.1=="billfish/swordfish")],col=cols3,ylim=c(0,6),cex.axis=1.5)
text(x=0.2,y=4.5,"Billfish &\nSwordfish",cex=1.4,adj=c(0,0))
barplot(height=final_rank_org$x[which(final_rank_org$Group.1=="LCS")],col=cols4,ylim=c(0,10),cex.axis=1.5)
text(x=0.2,y=7,"Large Coastal\nSharks",cex=1.4,adj=c(0,0))
barplot(height=dist_rank_org$x[which(dist_rank_org$Group.1=="LCS")],col=cols4,cex.axis=1.5)
text(x=0.2,y=13.5,"Large Coastal\nSharks",cex=1.4,adj=c(0,0))
barplot(height=dir_eff_rank_org$x[which(dir_eff_rank_org$Group.1=="LCS")],col=cols3,ylim=c(0,22),cex.axis=1.5)
text(x=0.2,y=15.5,"Large Coastal\nSharks",cex=1.4,adj=c(0,0))
barplot(height=final_rank_org$x[which(final_rank_org$Group.1=="pelagic shark")],col=cols4,ylim=c(0,9.5),cex.axis=1.5)
text(x=0.2,y=7.5,"Pelagic Sharks",cex=1.4,adj=c(0,0))
barplot(height=dist_rank_org$x[which(dist_rank_org$Group.1=="pelagic shark")],col=cols4,cex.axis=1.5)
text(x=0.2,y=5.5,"Pelagic Sharks",cex=1.4,adj=c(0,0))
barplot(height=dir_eff_rank_org$x[which(dir_eff_rank_org$Group.1=="pelagic shark")],col=cols3,cex.axis=1.5)
text(x=0.2,y=5.5,"Pelagic\nSharks",cex=1.4,adj=c(0,0))
barplot(height=final_rank_org$x[which(final_rank_org$Group.1=="SCS")],col=cols4,ylim=c(0,8),cex.axis=1.5)
text(x=0.2,y=6.2,"Small Coastal Sharks &\nSmoothhound Sharks",cex=1.4,adj=c(0,0))
barplot(height=dist_rank_org$x[which(dist_rank_org$Group.1=="SCS")],col=cols4,ylim=c(0,15),cex.axis=1.5)
text(x=0.2,y=11.5,"Small Coastal Sharks &\nSmoothhound Sharks",cex=1.4,adj=c(0,0))
barplot(height=dir_eff_rank_org$x[which(dir_eff_rank_org$Group.1=="SCS")],col=cols3,ylim=c(0,17.5),cex.axis=1.5)
text(x=0.2,y=13.5,"Small Coastal Sharks &\nSmoothhound Sharks",cex=1.4,adj=c(0,0))
barplot(height=final_rank_org$x[which(final_rank_org$Group.1=="tuna")],col=cols4,names.arg=c("Low","Moderate","High","Very High"),cex.axis=1.5,cex.name=1.25,ylim=c(0,4))
text(x=0.2,y=3.5,"Tuna",cex=1.4,adj=c(0,0))
mtext("Overall Climate Vulnerability",side=1,line=3.3)
barplot(height=dist_rank_org$x[which(dist_rank_org$Group.1=="tuna")],col=cols4,names.arg=c("Low","Moderate","High","Very High"),cex.axis=1.5,cex.name=1.25)
text(x=0.2,y=4.5,"Tuna",cex=1.4,adj=c(0,0))
mtext("Potential for Distribution Change",side=1,line=3.3)
barplot(height=dir_eff_rank_org$x[which(dir_eff_rank_org$Group.1=="tuna")],col=cols3,names.arg=c("Postive","Neutral","Negative"),cex.axis=1.5,cex.name=1.25)
text(x=0.2,y=4.5,"Tuna",cex=1.4,adj=c(0,0))
mtext("Directional Effects",side=1,line=3.3)
dev.off()

