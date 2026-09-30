#The R code and datasets in this study uses the Gulf of Mexico to refer to the area now known as the Gulf of America consistent with Executive Order (E.O.) 14172 (Restoring Names That Honor American Greatness).
#read in exposure scores generated from S1_script
exp_scores<-read.csv("~/Climate projects/CVA/Exposure_score_breakdown/exp_scores20230628.csv")
#read in sensitivity attribute scores generated from S2_script
sen_att_scores<-read.csv("~/Climate projects/CVA/SensAttr_Scoring/Final_Run/sen_att_scores_final20230716.csv")

#adjust names
names(exp_scores)<-c("Group","Species","EF_Score","EF_Num_Score")
names(sen_att_scores)<-c("Group","Species","SA_Score","SA_sdScore","SA_Num_Score")

#remove group from sen_att_scores
sen_att_scores<-subset(sen_att_scores,select=-Group)

vul_scores<-merge(exp_scores,sen_att_scores,by="Species",no.dups=F)

#multiply exposure score and sensitivity score together to get final vulnerability rank
vul_scores$Vul_Num_Score<-vul_scores$EF_Num_Score * vul_scores$SA_Num_Score

vul_scores$Final_Rank<-ifelse(vul_scores$Vul_Num_Score<=3,"low",
                              ifelse(vul_scores$Vul_Num_Score>=4 & vul_scores$Vul_Num_Score<=6, "moderate",
                                     ifelse(vul_scores$Vul_Num_Score>=8 & vul_scores$Vul_Num_Score<=9, "high",
                                            ifelse(vul_scores$Vul_Num_Score>=12,"very high",NA))))

write.csv(vul_scores,file="~/Climate projects/CVA/Vulnerability_Scores/vul_final_scores20230716.csv",row.names = F)
