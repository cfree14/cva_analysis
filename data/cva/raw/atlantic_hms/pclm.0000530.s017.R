#####Non-Metric Multidimensional Scaling
#The R code and datasets in this study uses the Gulf of Mexico to refer to the area now known as the Gulf of America consistent with Executive Order (E.O.) 14172 (Restoring Names That Honor American Greatness).
#read in detailed sensitivity attribute scores generated in S2_script
sen_att_deets<-read.csv("~/Climate projects/CVA/SensAttr_Scoring/Final_Run/sen_att_means_allspecies_final20230716.csv")

### Are sensitivity attributes means shared by species group?
#non-metric multidimensional scaling
library(vegan)
library(dplyr)
library(ggplot2)
sen_att_deets<-subset(sen_att_deets,select=-c(sen_att_sd,sen_att_dq_mean,LpRec,MpRec,HpRec,VpRec))
sen_att_deets_wide<-reshape(data=sen_att_deets,idvar="Species",timevar="sen_att_names",direction="wide")
sen_att_deets_wide<-sen_att_deets_wide[,c(1:2,3,5,7,9,11,13,15,17,19,21,23,25,27)]
names(sen_att_deets_wide)[2]<-"Group"

sen_att_mds <- metaMDS(sen_att_deets_wide[3:15], distance = "bray", autotransform = TRUE)

sen_att_deets_wide <- sen_att_deets_wide %>% 
  dplyr::mutate(NMDS1 = sen_att_mds$points[,1],
                NMDS2 = sen_att_mds$points[,2])

ggplot(sen_att_deets_wide) +
  geom_point(aes(x = NMDS1, y = NMDS2, color = Group), alpha = 0.5,size=5) +
  scale_color_manual(values=c("billfish/swordfish"="blue","SCS"="green","LCS"="red","pelagic shark"="purple","tuna"="brown"),
                     labels=c("billfish/swordfish"="Billfish/Swordfish","SCS"="SCS","LCS"="LCS","pelagic shark"="Pelagic Shark","tuna"="Tuna")) +
  theme_bw() +
  theme(axis.title = element_text(size=20),axis.text = element_text(size=20),legend.text=element_text(size=20),
        legend.title = element_text(size=20),panel.grid = element_blank())

###add eclipse to plot
# function
veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100)
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

#calculate eclipse
df_ell <- data.frame()
for(g in unique(sen_att_deets_wide$Group)){
  df_ell <- rbind(df_ell,
                  cbind(as.data.frame(with(sen_att_deets_wide[sen_att_deets_wide$Group==g,],
                                           veganCovEllipse(cov.wt(cbind(NMDS1,NMDS2), wt=rep(1/length(NMDS1), length(NMDS1)))$cov, center=c(mean(NMDS1),mean(NMDS2))))),
                        Group=g))
}

png("~/Climate projects/CVA/Manuscript_Figures/sensitivity_attribute_nMDS2.0.png", width=8, height=8,units="in",res=300)
ggplot(sen_att_deets_wide) +
  geom_point(aes(x = NMDS1, y = NMDS2, color = Group), alpha = 0.5,size=5) +
  scale_color_manual(values=c("billfish/swordfish"="blue","SCS"="green","LCS"="red","pelagic shark"="purple","tuna"="brown"),
                     labels=c("billfish/swordfish"="Billfish/\nSwordfish","SCS"="Small Coastal \nSharks/\nSmoothhound","LCS"="Large Coastal \nSharks","pelagic shark"="Pelagic Shark","tuna"="Tuna")) +
  geom_path(data = df_ell, aes(x = NMDS1, y = NMDS2, color = Group),size=1) +
  #annotate("text",x=0.11,y=0.25,label="Stress = 0.162",size=4) +
  theme_bw() +
  guides(size=guide_legend(order=1),col=guide_legend(order=2)) +
  theme(axis.title = element_text(size=20),axis.text = element_text(size=20),legend.text=element_text(size=20),
        legend.title = element_text(size=25),panel.grid = element_blank())
dev.off()

#for using shapes and colors
png("~/Climate projects/CVA/Manuscript_Figures/sensitivity_attribute_nMDS_shape.png", width=8, height=8,units="in",res=300)
ggplot(sen_att_deets_wide) +
  geom_point(aes(x = NMDS1, y = NMDS2, shape = Group, color=Group), alpha = 0.85,size=5) +
  scale_color_manual(values=c("billfish/swordfish"="blue","SCS"="green","LCS"="red","pelagic shark"="purple","tuna"="brown"),
                     labels=c("billfish/swordfish"="Billfish &\nSwordfish","SCS"="Small Coastal \nSharks &\nSmoothhound \nSharks","LCS"="Large Coastal \nSharks","pelagic shark"="Pelagic Sharks","tuna"="Tuna")) +
  scale_shape_manual(values=c("billfish/swordfish"=4,"SCS"=3,"LCS"=2,"pelagic shark"=1,"tuna"=0),
                     labels=c("billfish/swordfish"="Billfish &\nSwordfish","SCS"="Small Coastal \nSharks &\nSmoothhound \nSharks","LCS"="Large Coastal \nSharks","pelagic shark"="Pelagic Sharks","tuna"="Tuna")) +
  geom_path(data = df_ell, aes(x = NMDS1, y = NMDS2, color = Group),size=1) +
  #annotate("text",x=0.11,y=0.25,label="Stress = 0.162",size=4) +
  theme_bw() +
  theme(axis.title = element_text(size=20),axis.text = element_text(size=20),legend.text=element_text(size=20),
        legend.title = element_text(size=25),panel.grid = element_blank())
dev.off()


#ANOSIM
sen_att_deets_dist <- vegdist(sen_att_deets_wide[3:15], method = "bray")
sen_att_deets_anosim <- anosim(sen_att_deets_dist, grouping = sen_att_deets_wide$Group)
#ANOSIM statistic R: 0.5047 (species are grouped more by functional group)
#Significance: 0.001
#The closer this anosim stat R is to 1, the more the communities within a group are similar to each other and dissimilar to communities in other groups.
#Significance of the R statistic is determined by permuting group membership a large number of times to obtain the null distribution of the R statistic. 
#Comparing the position of the observed R value to the null distribution allows an assessment of statistical significance.



### Are sensitivity attributes means shared by vulnerability rank?
#non-metric multidimensional scaling

#read in final vulnerability rankings with all other scores generated in S5_script
vul_final_scores<-read.csv("~/Climate projects/CVA/Vulnerability_Scores/vul_dist_direct_uncert_final_scores20230802.csv")
sen_att_deets_wide<-merge(sen_att_deets_wide,vul_final_scores[,c(1,9)],by="Species",)
names(sen_att_deets_wide)[18]<-"Final Rank"

#remove species where we have no vul rank
sen_att_deets_wide_no_na<-sen_att_deets_wide[!is.na(sen_att_deets_wide$`Final Rank`),]

ggplot(sen_att_deets_wide_no_na) +
  geom_point(aes(x = NMDS1, y = NMDS2, color = `Final Rank`), alpha = 0.5,size=5) +
  scale_color_manual(values=c("low"="green","moderate"="yellow","high"="orange","very high"="red"),
                     labels=c("low"="Low","moderate"="Moderate","high"="High","very high"="Very High")) +
  theme_bw() +
  theme(axis.title = element_text(size=20),axis.text = element_text(size=20),legend.text=element_text(size=20),
        legend.title = element_text(size=20),panel.grid = element_blank())


#calculate eclipse
df_ell_vul <- data.frame()
for(g in unique(sen_att_deets_wide_no_na$`Final Rank`)){
  df_ell_vul <- rbind(df_ell_vul,
                      cbind(as.data.frame(with(sen_att_deets_wide_no_na[sen_att_deets_wide_no_na$`Final Rank`==g,],
                                               veganCovEllipse(cov.wt(cbind(NMDS1,NMDS2), wt=rep(1/length(NMDS1), length(NMDS1)))$cov, center=c(mean(NMDS1),mean(NMDS2))))),
                            `Final Rank`=g))
}

png("~/Climate projects/CVA/Manuscript_Figures/sensitivity_attribute_nMDS_by_vul_rank2.0.png", width=8, height=8,units="in",res=300)
ggplot(sen_att_deets_wide_no_na) +
  geom_point(aes(x = NMDS1, y = NMDS2, color=`Final Rank`), alpha = 0.85,size=5) +
  scale_color_manual(values=c("low"="green","moderate"="yellow","high"="orange","very high"="red"),
                     labels=c("low"="Low","moderate"="Moderate     ","high"="High","very high"="Very High")) +
  geom_path(data = df_ell_vul, aes(x = NMDS1, y = NMDS2, color = `Final Rank`),size=1) +
  #annotate("text",x=0.11,y=0.25,label="Stress = 0.162",size=4) +
  theme_bw() +
  guides(size=guide_legend(order=1),col=guide_legend(order=2)) +
  theme(axis.title = element_text(size=23),axis.text = element_text(size=23),legend.text=element_text(size=23),
        legend.title = element_text(size=25),panel.grid = element_blank())
dev.off()


#for shapes and colors
png("~/Climate projects/CVA/Manuscript_Figures/sensitivity_attribute_nMDS_by_vul_rank_shape.png", width=8, height=8,units="in",res=300)
ggplot(sen_att_deets_wide_no_na) +
  geom_point(aes(x = NMDS1, y = NMDS2, shape = `Final Rank`, color=`Final Rank`), alpha = 0.85,size=5) +
  scale_color_manual(values=c("low"="green","moderate"="yellow","high"="orange","very high"="red"),
                     labels=c("low"="Low","moderate"="Moderate     ","high"="High","very high"="Very High")) +
  scale_shape_manual(values=c("low"=4,"moderate"=3,"high"=2,"very high"=1),
                     labels=c("low"="Low","moderate"="Moderate     ","high"="High","very high"="Very High")) +
  geom_path(data = df_ell_vul, aes(x = NMDS1, y = NMDS2, color = `Final Rank`),size=1) +
  #annotate("text",x=0.11,y=0.25,label="Stress = 0.162",size=4) +
  theme_bw() +
  theme(axis.title = element_text(size=23),axis.text = element_text(size=23),legend.text=element_text(size=23),
        legend.title = element_text(size=25),panel.grid = element_blank())
dev.off()

#ANOSIM
sen_att_deets_by_vul_dist <- vegdist(sen_att_deets_wide_no_na[3:15], method = "bray")
sen_att_deets_by_vul_anosim <- anosim(sen_att_deets_by_vul_dist, grouping = sen_att_deets_wide_no_na$`Final Rank`)
#ANOSIM statistic R: 0.3401 (species are not grouped much by vulnerability rank)
#Significance: 0.001 

