library(ncdf4)
library(sp)
library(raster)
library(sf)
library(terra)

#The R code and datasets in this study uses the Gulf of Mexico to refer to the area now known as the Gulf of America consistent with Executive Order (E.O.) 14172 (Restoring Names That Honor American Greatness).
#CMIP6: https://psl.noaa.gov/ipcc/cmip6/ccwp6.html
#Model: Average of All Models
#Experiment: SSP5-8.5
#Season: Entire Year
#Historical period: 1985-2014
#Future period: 2020-2049
#Statistic: Standard Anom (avg historical)
#exposure factor pull domain 75N, 60S, 43E, 99W (encompasses all species ranges)

#function to isolate correct exp factors and calculate exposure factor score using logic model
selected_ef<-function(dat,temp,sal,o2precip,msstg_swsm){
  #remove chl first
  dat<-dat[which(dat$env!="chl"),]
  #remove ohc700
  dat<-dat[which(dat$env!="ohc700"),]
  
  #sst or bt
  if(temp=="sst"){
    dat<-dat[which(dat$env!="bt"),]
  }else{
    dat<-dat[which(dat$env!="sst"),]
  }
  #sss or bs
  if(sal=="sss"){
    dat<-dat[which(dat$env!="bs"),]
  }else{
    dat<-dat[which(dat$env!="sss"),]
  }
  #o200 or precip
  if(o2precip=="o200"){
    dat<-dat[which(dat$env!="precip"),]
  }else{
    dat<-dat[which(dat$env!="o200"),]
  }
  #msstg or swsm
  if(msstg_swsm=="msstg"){
    dat<-dat[which(dat$env!="swsm"),]
  }else{
    dat<-dat[which(dat$env!="msstg"),]
  }
  
  #logic model
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

#Function developed to run full exposure analysis (run function for each species)
exp_analysis<-function(species,modelV,futureyear,temp,sal,o2precip,msstg_swsm){
  
  ######## READ IN/MANIPULATE EXPOSURE FACTOR ###########
  setwd(paste("~/Climate projects/CVA/Exposure_factors",modelV,futureyear,sep="/"))
  file.list<-list.files()
  exp_fact_meanRec<-NULL #empty vector for exp_fact_mean, will get filled in at end of loop
  env_namesRec<-NULL #empty vector for env names, will get filled in at end of loop
  LpRec<-NULL
  MpRec<-NULL
  HpRec<-NULL
  VpRec<-NULL
  for(i in 1:length(file.list)){
    setwd(paste("~/Climate projects/CVA/Exposure_factors",modelV,futureyear,sep="/"))
    env.nc<-nc_open(file.list[i])
    env.name<-strsplit(file.list[i],split="_")[[1]][1]
    
    env.nc
    
    lon<-ncvar_get(env.nc, "lon")
    #360 lons, in units east...for some reason data extraction is not limiting longitude to north atlantic so do it manually
    lon<-ifelse(lon>=180,lon-360,lon) #need to substract all values that are >=180 by 360 to get them on correct range
    #goes from 0.5 (just east of prime meridian) going east and ends -0.05 (just west of prime meridian)
    #reduce lons to be between -100.5 to 40.5
    lon<-lon[c(1:44,261:360)] #144 lons
    lat<-ncvar_get(env.nc, "lat")
    #137 lats, south (-60.5) to north (75.5) 
    
    #want to pull in anomaly data from nc
    env_an<-ncvar_get(env.nc,"anomaly")
    dim(env_an) #360 x 137
    #reduce lons to match lon dim
    env_an<-env_an[c(1:44,261:360),]
    dim(env_an) #144 x 137
    
    #right now longitude goes from 0.5 to 43.5 to -99.5 to -0.5
    #want to shift longitudes to that it starts with -100.5 and goes to 40.5
    lon<-lon[c(45:144,1:44)]
    
    #shift matrix around so that it starts with -100.5 and goes to 40.5
    env_an<-env_an[c(45:144,1:44),]
    
    nc_close(env.nc)
    
    #for some variables (bottom salinity) there are unknown values for outputs which is valued at 1e20
    #so r wont display integers as large as 1e20 so just pick a lower value like 1000 (which no standardized anomalies will be greater than)
    #and turn all grid cells with greater than 1000 to NAs
    env_an<-ifelse(env_an>1000,NA,env_an) 
    
    #turn exposure factor matrix into raster
    lon_lat<-expand.grid(lon=lon, lat=lat)
    coordinates(lon_lat)<- ~lon + lat 
    projection(lon_lat)<-crs("+init=epsg:4326")#need to know coordinate system used in other CVAs
    e<-extent(c(min(lon),max(lon),min(lat),max(lat)))
    r<-raster(e,nrow=137,ncol=144, crs="+proj=longlat +datum=WGS84 +no_defs")
    
    envraster<-rasterize(lon_lat, r,env_an, fun=mean)
    
    
    ######## READ IN SPECIES SHP ###########
    setwd("~/Climate projects/CVA/In_progress_iucn_tweaks")
    sp_shp<-shapefile(paste(species,".shp",sep=""))
    
    
    ######## MASK EXPOSURE FACTOR W/SPECIES SHP ###########
    
    #could use polygon (sp_shp) as mask but will leave some cells not covered that don't overlap with centroid (see comment below)
    #or could rasterize polygon first and include all partially covered cells (getCover=TRUE)
    #go with rasterize polygon first and include all partially covered cells (sp_shp1)
    sp_shp1<- rasterize(sp_shp, r, getCover=TRUE)
    sp_shp1[sp_shp1==0] <- NA
    sp_shp1[sp_shp1>0]<- 1 #for some reasons cells around boundary is a different value, so just make all non zero values equal to 1
    
    env_an_masked<-mask(envraster,sp_shp1)
    
    #find max and min lat and lon where there are non NA values to zoom in map
    maxlon<-max(raster::xFromCell(env_an_masked,which(is.na(env_an_masked[])==FALSE)))
    minlon<-min(raster::xFromCell(env_an_masked,which(is.na(env_an_masked[])==FALSE)))
    maxlat<-max(raster::yFromCell(env_an_masked,which(is.na(env_an_masked[])==FALSE)))
    minlat<-min(raster::yFromCell(env_an_masked,which(is.na(env_an_masked[])==FALSE)))
    
    setwd("~/Climate projects/CVA/Masked_exposure_factors/official_run")#pngs of regular mask from sp_shp saved in regular_mask_pilot_species folder 
    png(paste(species,"masked",env.name,modelV,futureyear,".png",sep="_"),height=6,width=6,res=150,units="in")
    plot(env_an_masked,ylim=c(minlat,maxlat),xlim=c(minlon,maxlon))
    plot(sp_shp,add=T)
    dev.off()
    
    ######## GENERATE GRID CELL COUNT HISTOGRAM AND BARPLOT ###########
    
    #just make 0.25 breaks based on range of anomalies of exposure factors
    breaks<-seq(floor(min(values(env_an_masked),na.rm=T)),ceiling(max(values(env_an_masked),na.rm=T)),by=0.25)
    my_colors <- rep("red", length(breaks))       # Specify colors corresponding to breaks
    my_colors[breaks >= -0.5 & breaks <= 0.5] <- "green"
    my_colors[breaks < -0.5 & breaks >= -1.5] <- "yellow"
    my_colors[breaks > 0.5 & breaks <= 1.5] <- "yellow"
    my_colors[breaks < -1.5 & breaks >= -2] <- "orange"
    my_colors[breaks > 1.5 & breaks <= 2] <- "orange"
    h<-hist(values(env_an_masked),breaks=breaks,freq=FALSE,col=my_colors,xlab=paste(env.name,"anomalies",sep=" "),
            ylab="Percent",main=paste(species,modelV,sep=" "))
    
    #calculate counts and percent of counts into L, M, H, V
    L<-sum(h$counts[h$breaks>= -0.5 & h$breaks<= 0.5],na.rm=T)
    Lp<-L/sum(h$counts)#calc percent of counts in L
    M<-sum(h$counts[(h$breaks < -0.5 & h$breaks>= -1.5) | (h$breaks > 0.5 & h$breaks <= 1.5)],na.rm=T)
    Mp<-M/sum(h$counts)
    H<-sum(h$counts[(h$breaks < -1.5 & h$breaks>= -2) | (h$breaks > 1.5 & h$breaks <= 2)],na.rm=T)
    Hp<-H/sum(h$counts)
    V<-sum(h$counts[h$breaks< -2 | h$breaks> 2],na.rm=T)#NA is generated because there are more breaks than counts so remove NAs
    Vp<-V/sum(h$counts)
    
    
    ######## CALCULATE WEIGHTED MEAN OF SCORE ###########
    
    #reassign grid cell values of L with 1, M with 2, H with 3, and V with 4
    #multiply number of grid cells for each category by 1 for L, 2 for M, 3 for H, and 4 for V
    #sum those values and divide by total number of grid cells to get exposure factor weighted mean
    exp_fact_mean<-((L*1)+(M*2)+(H*3)+(V*4))/sum(L,M,H,V)
    
    ######## PLOT ANOMALY DISTRIBUTIONS ###########
    
    setwd("~/Climate projects/CVA/Anomaly_dist_plots/official_run") #pngs of regular mask from sp_shp saved in regular_mask_pilot_species folder 
    png(paste(species,env.name,modelV,futureyear,".png",sep="_"),height=8,width=4,res=300,units="in")
    par(mfrow=c(2,1))
    hist(values(env_an_masked),breaks=breaks,freq=FALSE,col=my_colors,xlab=paste(env.name,"anomalies",sep=" "),
         ylab="Percent",main=paste(species,modelV,sep=" "))
    barplot(height=c(Lp,Mp,Hp,Vp),names.arg=c("L","M","H","V"),col=c("green","yellow","orange","red"),
            ylab="Percent",ylim=c(0,1))
    abline(h=0)
    text(x=1,y=0.8,round(exp_fact_mean,digits=1))
    dev.off()
    
    ######## PUT EXPOSURE FACTOR MEAN IN VECTOR #########
    
    exp_fact_meanRec<-c(exp_fact_meanRec,exp_fact_mean)
    env_namesRec<-c(env_namesRec,env.name)
    LpRec<-c(LpRec,Lp)
    MpRec<-c(MpRec,Mp)
    HpRec<-c(HpRec,Hp)
    VpRec<-c(VpRec,Vp)
    #after exp_fact_mean is calculated for each exp fact, then use logic table to determine if species is L,M,H,or V
    
    
  }
  exp_fact_means<-data.frame(env_names=env_namesRec,exp_fact_mean=exp_fact_meanRec,Lp=LpRec,Mp=MpRec,Hp=HpRec,Vp=VpRec)
  
  expscore<-selected_ef(dat=exp_fact_means,temp=temp,sal=sal,o2precip=o2precip,msstg_swsm=msstg_swsm)
  
  #exp_fact_means is same as dat and same as score_breakdown
  #save score breakdown
  write.csv(expscore$score_breakdown, file=paste("~/Climate projects/CVA/Exposure_score_breakdown/official_run/",species,"_expscore.csv",sep=""),row.names = F)
  
  
  return(expscore)
}

#RUN EXPOSURE ANALYSIS FUNCTION FOR EACH SPECIES (some species not run because not confident in species distribution)

#### LCS species run
SBK<-exp_analysis(species="c_limbatus_v6",modelV="CMIP6",futureyear="future2020_2049",temp="bt",sal="bs",o2precip="precip",msstg_swsm="swsm")
BSG<-exp_analysis(species="c_maximus_v5",modelV="CMIP6",futureyear="future2020_2049",temp="sst",sal="sss",o2precip="o200",msstg_swsm="msstg")
#bigeye sand tiger not doing
#bignose shark not doing
SBU<-exp_analysis(species="c_leucas_v8",modelV="CMIP6",futureyear="future2020_2049",temp="bt",sal="bs",o2precip="precip",msstg_swsm="swsm")
SRF<-exp_analysis(species="c_perezi_v4",modelV="CMIP6",futureyear="future2020_2049",temp="bt",sal="bs",o2precip="precip",msstg_swsm="swsm")
DUS<-exp_analysis(species="c_obscurus_v9",modelV="CMIP6",futureyear="future2020_2049",temp="bt",sal="bs",o2precip="precip",msstg_swsm="swsm")
#galapagos shark not doing
GHH<-exp_analysis(species="s_mokarran_v10",modelV="CMIP6",futureyear="future2020_2049",temp="bt",sal="bs",o2precip="precip",msstg_swsm="swsm")
SBKgom<-exp_analysis(species="c_limbatusGOM_v5",modelV="CMIP6",futureyear="future2020_2049",temp="bt",sal="bs",o2precip="precip",msstg_swsm="swsm")
LEM<-exp_analysis(species="n_brevirostris_v5",modelV="CMIP6",futureyear="future2020_2049",temp="bt",sal="bs",o2precip="precip",msstg_swsm="swsm")
#narrowtooth shark not doing
SNI<-exp_analysis(species="c_signatus_v5",modelV="CMIP6",futureyear="future2020_2049",temp="bt",sal="bs",o2precip="precip",msstg_swsm="swsm")
NUR<-exp_analysis(species="g_cirratum_v9",modelV="CMIP6",futureyear="future2020_2049",temp="bt",sal="bs",o2precip="precip",msstg_swsm="swsm")
SST<-exp_analysis(species="c_taurus_v3",modelV="CMIP6",futureyear="future2020_2049",temp="bt",sal="bs",o2precip="precip",msstg_swsm="swsm")
SSB<-exp_analysis(species="c_plumbeus_v4",modelV="CMIP6",futureyear="future2020_2049",temp="bt",sal="bs",o2precip="precip",msstg_swsm="swsm")
SPL<-exp_analysis(species="s_lewini_v12",modelV="CMIP6",futureyear="future2020_2049",temp="bt",sal="bs",o2precip="precip",msstg_swsm="swsm")
FAL<-exp_analysis(species="c_falciformis_v2",modelV="CMIP6",futureyear="future2020_2049",temp="sst",sal="sss",o2precip="o200",msstg_swsm="msstg")
SHH<-exp_analysis(species="s_zygaena_v1",modelV="CMIP6",futureyear="future2020_2049",temp="bt",sal="bs",o2precip="precip",msstg_swsm="swsm")
SSP<-exp_analysis(species="c_brevipinna_v6",modelV="CMIP6",futureyear="future2020_2049",temp="bt",sal="bs",o2precip="precip",msstg_swsm="swsm")
TIG<-exp_analysis(species="g_cuvier_v6",modelV="CMIP6",futureyear="future2020_2049",temp="bt",sal="bs",o2precip="precip",msstg_swsm="swsm")
WHA<-exp_analysis(species="r_typus_v2",modelV="CMIP6",futureyear="future2020_2049",temp="sst",sal="sss",o2precip="o200",msstg_swsm="msstg")
WHI<-exp_analysis(species="c_carcharias_v3",modelV="CMIP6",futureyear="future2020_2049",temp="bt",sal="bs",o2precip="precip",msstg_swsm="swsm")

#### SCS/smoothhound species run
ANG<-exp_analysis(species="s_dumeril_v4",modelV="CMIP6",futureyear="future2020_2049",temp="bt",sal="bs",o2precip="precip",msstg_swsm="swsm")
SBN<-exp_analysis(species="c_acronotus_v3",modelV="CMIP6",futureyear="future2020_2049",temp="bt",sal="bs",o2precip="precip",msstg_swsm="swsm")
SBNgom<-exp_analysis(species="c_acronotusGOM_v3",modelV="CMIP6",futureyear="future2020_2049",temp="bt",sal="bs",o2precip="precip",msstg_swsm="swsm")
BON<-exp_analysis(species="s_tiburo_v5",modelV="CMIP6",futureyear="future2020_2049",temp="bt",sal="bs",o2precip="precip",msstg_swsm="swsm")
SAS<-exp_analysis(species="r_terraenovae_v5",modelV="CMIP6",futureyear="future2020_2049",temp="bt",sal="bs",o2precip="precip",msstg_swsm="swsm")
SASgom<-exp_analysis(species="r_terraenovaeGOM_v5",modelV="CMIP6",futureyear="future2020_2049",temp="bt",sal="bs",o2precip="precip",msstg_swsm="swsm")
#caribbean sharpnose shark not doing
SFT<-exp_analysis(species="c_isodon_v18",modelV="CMIP6",futureyear="future2020_2049",temp="bt",sal="bs",o2precip="precip",msstg_swsm="swsm")
#florida smoothhound shark not doing
#gulf smoothhound shark not doing
BONgom<-exp_analysis(species="s_tiburoGOM_v6",modelV="CMIP6",futureyear="future2020_2049",temp="bt",sal="bs",o2precip="precip",msstg_swsm="swsm")
#smalltail shark not doing
DGS<-exp_analysis(species="m_canisATL_v6",modelV="CMIP6",futureyear="future2020_2049",temp="bt",sal="bs",o2precip="precip",msstg_swsm="swsm")
#GOM smooth dogfish not doing

#### Pelagic sharks species run
#bigeye sixgill shark not doing
BTH<-exp_analysis(species="a_superciliosus_v7",modelV="CMIP6",futureyear="future2020_2049",temp="sst",sal="sss",o2precip="o200",msstg_swsm="msstg")
LMA<-exp_analysis(species="i_paucus_v2",modelV="CMIP6",futureyear="future2020_2049",temp="sst",sal="sss",o2precip="o200",msstg_swsm="msstg")
BSH<-exp_analysis(species="p_glauca_v3",modelV="CMIP6",futureyear="future2020_2049",temp="sst",sal="sss",o2precip="o200",msstg_swsm="msstg")
SMA<-exp_analysis(species="i_oxyrinchus_v5",modelV="CMIP6",futureyear="future2020_2049",temp="sst",sal="sss",o2precip="o200",msstg_swsm="msstg")
POR<-exp_analysis(species="l_nasus_v4",modelV="CMIP6",futureyear="future2020_2049",temp="sst",sal="sss",o2precip="o200",msstg_swsm="msstg")
OCS<-exp_analysis(species="c_longimanus_v2",modelV="CMIP6",futureyear="future2020_2049",temp="sst",sal="sss",o2precip="o200",msstg_swsm="msstg")
SVG<-exp_analysis(species="h_perlo_v4",modelV="CMIP6",futureyear="future2020_2049",temp="bt",sal="bs",o2precip="o200",msstg_swsm="swsm")
SXG<-exp_analysis(species="h_griseus_v4",modelV="CMIP6",futureyear="future2020_2049",temp="bt",sal="bs",o2precip="o200",msstg_swsm="swsm")
PTH<-exp_analysis(species="a_vulpinus_v4",modelV="CMIP6",futureyear="future2020_2049",temp="sst",sal="sss",o2precip="o200",msstg_swsm="msstg")

#### Billfish/Swordfish species run
BUM<-exp_analysis(species="m_nigricans_v9",modelV="CMIP6",futureyear="future2020_2049",temp="sst",sal="sss",o2precip="o200",msstg_swsm="msstg")
#longbill spearfish not doing
SWO<-exp_analysis(species="x_gladius_v4",modelV="CMIP6",futureyear="future2020_2049",temp="sst",sal="sss",o2precip="o200",msstg_swsm="msstg")
#roundscale spearfish not doing
SAI<-exp_analysis(species="i_platypterus_v4",modelV="CMIP6",futureyear="future2020_2049",temp="sst",sal="sss",o2precip="o200",msstg_swsm="msstg")
WHM<-exp_analysis(species="k_albidus_v5",modelV="CMIP6",futureyear="future2020_2049",temp="sst",sal="sss",o2precip="o200",msstg_swsm="msstg")

#### Tuna species run
BET<-exp_analysis(species="t_obesus_v4",modelV="CMIP6",futureyear="future2020_2049",temp="sst",sal="sss",o2precip="o200",msstg_swsm="msstg")
ALB<-exp_analysis(species="t_alalunga_v2",modelV="CMIP6",futureyear="future2020_2049",temp="sst",sal="sss",o2precip="o200",msstg_swsm="msstg")
BFT<-exp_analysis(species="t_thynnus_v5",modelV="CMIP6",futureyear="future2020_2049",temp="sst",sal="sss",o2precip="o200",msstg_swsm="msstg")
SKJ<-exp_analysis(species="k_pelamis_v6",modelV="CMIP6",futureyear="future2020_2049",temp="sst",sal="sss",o2precip="o200",msstg_swsm="msstg")
YFT<-exp_analysis(species="t_albacares_v6",modelV="CMIP6",futureyear="future2020_2049",temp="sst",sal="sss",o2precip="o200",msstg_swsm="msstg")


#combine exposure scores for all species
exp_scores<-data.frame(Group=c(rep("LCS",23),rep("SCS",14),rep("pelagic shark",10),rep("billfish/swordfish",6),rep("tuna",5)),
                       Species=c("blacktip (ATL)","basking","bigeye sand tiger","bignose","bull","Caribbean reef","dusky","Galapagos",
                                 "great hammerhead","blacktip (GOM)","lemon","narrowtooth","night","nurse","sand tiger","sandbar",
                                 "scalloped hammerhead","silky","smooth hammerhead","spinner","tiger","whale","white",
                                 "angel","blacknose (ATL)","blacknose (GOM)","bonnethead (ATL)","Atlantic sharpnose (ATL)","Atlantic sharpnose (GOM)",
                                 "Caribbean sharpnose","finetooth","Florida smoothhound","gulf smoothhound","bonnethead (GOM)","smalltail",
                                 "smooth dogfish (ATL)","smooth dogfish (GOM)","bigeye sixgill","bigeye thresher","longfin mako","blue",
                                 "shortfin mako","porbeagle","oceanic whitetip","sharpnose sevengill","bluntnose sixgill","common thresher","blue marlin",
                                 "longbill spearfish","swordfish","roundscale spearfish","Atlantic sailfish","white marlin","bigeye tuna",
                                 "albacore tuna","bluefin tuna","skipjack tuna","yellowfin tuna"),
                       Score=rbind(SBK$exp_scores,BSG$exp_scores,NA,NA,SBU$exp_scores,SRF$exp_scores,DUS$exp_scores,NA,GHH$exp_scores,SBKgom$exp_scores,
                                   LEM$exp_scores,NA,SNI$exp_scores,NUR$exp_scores,SST$exp_scores,SSB$exp_scores,SPL$exp_scores,FAL$exp_scores,
                                   SHH$exp_scores, SSP$exp_scores,TIG$exp_scores,WHA$exp_scores,WHI$exp_scores,ANG$exp_scores,SBN$exp_scores,SBNgom$exp_scores,
                                   BON$exp_scores,SAS$exp_scores,SASgom$exp_scores,NA,SFT$exp_scores,NA,NA,BONgom$exp_scores,NA,DGS$exp_scores,NA,
                                   NA,BTH$exp_scores,LMA$exp_scores,BSH$exp_scores,SMA$exp_scores,POR$exp_scores,OCS$exp_scores,SVG$exp_scores,
                                   SXG$exp_scores,PTH$exp_scores,BUM$exp_scores,NA,SWO$exp_scores,NA,SAI$exp_scores,WHM$exp_scores,BET$exp_scores,
                                   ALB$exp_scores,BFT$exp_scores,SKJ$exp_scores,YFT$exp_scores))

exp_scores$Num_Score<-NA
exp_scores$Num_Score<-ifelse(exp_scores$Score=="very high",4,
                             ifelse(exp_scores$Score=="high",3,
                                    ifelse(exp_scores$Score=="moderate",2,
                                           ifelse(exp_scores$Score=="low",1,NA))))

#save final exposure factor scores
write.csv(exp_scores, file="~/Climate projects/CVA/Exposure_score_breakdown/exp_scores20230628.csv",row.names = F)




### Manuscript figure: Distribution of mean scores of exposure scores box & whisker plot

#combine exposure score breakdown for all species
exp_fact_deets<-data.frame(rbind(SBK$score_breakdown,BSG$score_breakdown,NA,NA,SBU$score_breakdown,SRF$score_breakdown,DUS$score_breakdown,NA,GHH$score_breakdown,SBKgom$score_breakdown,
                                 LEM$score_breakdown,NA,SNI$score_breakdown,NUR$score_breakdown,SST$score_breakdown,SSB$score_breakdown,SPL$score_breakdown,FAL$score_breakdown,
                                 SHH$score_breakdown, SSP$score_breakdown,TIG$score_breakdown,WHA$score_breakdown,WHI$score_breakdown,ANG$score_breakdown,SBN$score_breakdown,SBNgom$score_breakdown,
                                 BON$score_breakdown,SAS$score_breakdown,SASgom$score_breakdown,NA,SFT$score_breakdown,NA,NA,BONgom$score_breakdown,NA,DGS$score_breakdown,NA,
                                 NA,BTH$score_breakdown,LMA$score_breakdown,BSH$score_breakdown,SMA$score_breakdown,POR$score_breakdown,OCS$score_breakdown,SVG$score_breakdown,
                                 SXG$score_breakdown,PTH$score_breakdown,BUM$score_breakdown,NA,SWO$score_breakdown,NA,SAI$score_breakdown,WHM$score_breakdown,BET$score_breakdown,
                                 ALB$score_breakdown,BFT$score_breakdown,SKJ$score_breakdown,YFT$score_breakdown))
#order by median
group_ordered <- with(exp_fact_deets,                       
                      reorder(env_names,
                              exp_fact_mean,
                              median))

exp_fact_order<-c("Precipitation ","Surface Wind Stress Magnitude ","Primary Production ","Mixed Layer Depth ","Magnitude of SST Gradient ","Oxygen at 200m ",
                  "Sea Surface Salinity ","Bottom Salinity ","Sea Surface Temperature ","Sea Surface Oxygen ","Bottom Temperature ","pH ")

png("~/Climate projects/CVA/Manuscript_Figures/exposure_factor_mean_scores.png", width=10, height=8,units="in",res=300)
par(mar=c(5,14.5,4,2))
boxplot(exp_fact_mean~group_ordered,data=exp_fact_deets,horizontal = T,las=1,xlab="Mean Score",ylab="",yaxt="n",ylim=c(0.85,4),
        cex.lab=1.5)
mtext(exp_fact_order,side=2,at=1:12,las=1)
mtext("Exposure Factor",side=2,line=13,cex=1.5)
text(x=0.85,y=12,"(46)")
text(x=0.85,y=11,"(27)")
text(x=0.85,y=10,"(46)")
text(x=0.85,y=9,"(19)")
text(x=0.85,y=8,"(27)")
text(x=0.85,y=7,"(19)")
text(x=0.85,y=6,"(21)")
text(x=0.85,y=5,"(19)")
text(x=0.85,y=4,"(46)")
text(x=0.85,y=3,"(46)")
text(x=0.85,y=2,"(27)")
text(x=0.85,y=1,"(25)")
dev.off()

