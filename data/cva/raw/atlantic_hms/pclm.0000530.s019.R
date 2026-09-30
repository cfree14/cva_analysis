#The R code and datasets in this study uses the Gulf of Mexico to refer to the area now known as the Gulf of America consistent with Executive Order (E.O.) 14172 (Restoring Names That Honor American Greatness).
# Read in functions for summarizing the data and plotting the species narratives
# species_narratives_func.R

library(plyr)

source("C:/Users/dan.crear/Documents/Climate projects/CVA/Manuscript Scripts/species_narratives_funcMS.R")

#read in S2 Table: S2 Table: Species Name Match List wherever it is stored locally
species.functional.sorted<-read.csv("~/Climate projects/CVA/species_name_match.csv")
#read in S3 Table: Exposure Factor Abbreviations wherever it is stored locally
exp.factor.list<-read.csv("~/Climate projects/CVA/exp_factor_name_match.csv")
file.format<-"png"
#read in final vulnerability rankings with all other scores generated in S5_script
overall.scores<-read.csv("~/Climate projects/CVA/Vulnerability_Scores/vul_dist_direct_uncert_final_scores20230802.csv")
#read in sensitivity attribute data quality scores generated in S3_script
dq<-read.csv("~/Climate projects/CVA/Data_quality_scores/sens_att_data_quality20230716.csv")
#place where you want to store species narratives
files.folder.name<-"C:/Users/dan.crear/Documents/Climate projects/CVA/Species_Narratives/"


#### LCS species run
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="c_limbatus_v6",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="c_maximus_v5",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="o_noronhai",file.format="png",ef_analysis="no")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="c_altimus",file.format="png",ef_analysis="no")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="c_leucas_v8",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="c_perezi_v4",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="c_obscurus_v9",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="c_galapagensis",file.format="png",ef_analysis="no")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="s_mokarran_v10",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="c_limbatusGOM_v5",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="n_brevirostris_v5",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="c_brachyurus",file.format="png",ef_analysis="no")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="c_signatus_v5",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="g_cirratum_v9",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="c_taurus_v3",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="c_plumbeus_v4",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="s_lewini_v12",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="c_falciformis_v2",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="s_zygaena_v1",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="c_brevipinna_v6",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="g_cuvier_v6",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="r_typus_v2",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="c_carcharias_v3",file.format="png",ef_analysis="yes")


#### SCS/smoothhound species run
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="s_dumeril_v4",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="c_acronotus_v3",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="c_acronotusGOM_v3",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="s_tiburo_v5",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="r_terraenovae_v5",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="r_terraenovaeGOM_v5",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="r_porosus",file.format="png",ef_analysis="no")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="c_isodon_v18",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="m_norrisi",file.format="png",ef_analysis="no")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="m_sinusmexicanus",file.format="png",ef_analysis="no")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="s_tiburoGOM_v6",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="c_porosus",file.format="png",ef_analysis="no")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="m_canisATL_v6",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="m_canisGOM",file.format="png",ef_analysis="no")


#### Pelagic sharks species run
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="h_nakamurai",file.format="png",ef_analysis="no")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="a_superciliosus_v7",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="i_paucus_v2",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="p_glauca_v3",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="i_oxyrinchus_v5",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="l_nasus_v4",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="c_longimanus_v2",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="h_perlo_v4",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="h_griseus_v4",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="a.vulpinus_v4",file.format="png",ef_analysis="yes")


#### Billfish/Swordfish species run
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="m_nigricans_v9",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="t_pfluegeri",file.format="png",ef_analysis="no")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="x_gladius_v4",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="t_georgii",file.format="png",ef_analysis="no")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="i_platypterus_v4",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="k_albidus_v5",file.format="png",ef_analysis="yes")


#### Tuna species run
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="t_obesus_v4",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="t_alalunga_v2",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="t_thynnus_v5",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="k_pelamis_v6",file.format="png",ef_analysis="yes")
species_narrative(overall.scores=overall.scores, dq = dq, species.functional.sorted=species.functional.sorted, exp.factor.list=exp.factor.list,
                  files.folder.name=files.folder.name, ef_species_name="t_albacares_v6",file.format="png",ef_analysis="yes")

