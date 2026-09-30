# species_narratives_func.R
#
# Dan Crear
#
# Created June 30, 2023
#The R code and datasets in this study uses the Gulf of Mexico to refer to the area now known as the Gulf of America consistent with Executive Order (E.O.) 14172 (Restoring Names That Honor American Greatness).
# Functions for summarizing the data and plotting the species narratives
# Used in species_narratives.R

spacing.rank <- function(x){
  
  # Gives spacing to leave after text depening on the score rank and it's length
  #
  # Args:
  #   x             vector of length 1 containing the number 1, 2, 3, or 4
  #
  # Returns:
  #   space         space coresponding to the specified score
  
  if(is.na(x)){
    space <- .07 
  }else if(x=="low"){
    space <- .07
  }else if(x=='moderate'){
    space <- .15
  }else if(x=='high'){
    space <- 0.08
  }else space <- 0.15
  return(space)
}

###############################################################################
###############################################################################

assign.colors.words <- function(x){
  
  # Assigns colors based on a score, specified as a word (low=green, moderate=yellow, high=orage, very high=red)
  #
  # Args:
  #   x             vector of length 1 containing the number 1, 2, 3, or 4
  #
  # Returns:
  #   color         color coresponding to the specified number x
  
  if(is.na(x)){
    color <- NULL
  }else if(x=="low"){
    color <- 'green'
  }else if(x=='moderate'){
    color <- 'yellow'
  }else if(x=='high'){
    color <- 'orange'
  }else color <- 'red'
  return(color)
}

###############################################################################

species_narrative <- function(overall.scores,dq,species.functional.sorted,files.folder.name,file.format,
                              exp.factor.list,ef_species_name,ef_analysis){
  
  # Function for plotting the species information sheets
  #
  # Args:
  #   overall.scores                  final results from each component
  #   species.functional.sorted       all different names used throughout cva process
  #   files.folder.name               folder to save the csv files and bubble plot in
  #   file.format                     format to save the species information sheets as "png"
  #   exp.factor.list                 all exposure factors listed by name
  #   ef_species_name                 name used for species exp factor table file
  
  # Returns:
  #   No returns, saves files with the species information sheets in the specified format
  
  #######################################################################
  ### read in exp factor and sens att means and do some name manipulations
  #read in exp factor means (if exp analysis was done on species)
  if(ef_analysis=="yes"){
    exposure_factor_means<-read.csv(paste("C:/Users/dan.crear/Documents/Climate projects/CVA/Exposure_score_breakdown/official_run/",ef_species_name,"_expscore.csv",sep=""))
  }else{ #read in atlantic blacktip and make all exposure scores NA
    exposure_factor_means<-read.csv("C:/Users/dan.crear/Documents/Climate projects/CVA/Exposure_score_breakdown/official_run/c_limbatus_v6_expscore.csv")
    exposure_factor_means[,2:6]<-NA
  }
  
  #remove version number from name for later use
  ef_species_name<-strsplit(ef_species_name,split='_v', fixed=TRUE)[[1]][1]
  #get code name for later
  code.name <- species.functional.sorted$Code.Name[which(species.functional.sorted$EF.Stock.Name==ef_species_name)]
  #get stock name for later
  stock.name<-species.functional.sorted$Stock.Name[which(species.functional.sorted$EF.Stock.Name==ef_species_name)]
  #get narrative name for later
  narrative.name<-species.functional.sorted$Narrative.Name[which(species.functional.sorted$EF.Stock.Name==ef_species_name)]
  #read in sens att means
  sens_att_means<-read.csv(paste("C:/Users/dan.crear/Documents/Climate projects/CVA/SensAttr_Scoring/Final_Run/",stock.name,"_senattscore.csv",sep=""))
  #fix stock Size_Status (remove _)
  sens_att_means$sen_att_names[which(sens_att_means$sen_att_names=="Stock Size_Status")]<-"Stock Size Status"
  
  #######################################################################
  ### Add data quality rank to overall.scores data frame
  colnames(overall.scores)[colnames(overall.scores) == "Species"] <- "Code.Name"
  colnames(dq)[colnames(dq) == "Short_Name"] <- "Code.Name"
  
  overall.scores<-merge(overall.scores,dq,by="Code.Name")
  
  #######################################################################
  ### Get the sensitivity and exposure attributes
  sensitivity.attributes <- unique(sens_att_means$sen_att_names)
  
  #need to give full exposure factor names into exposure_factor_means
  exposure_factor_means<-merge(exposure_factor_means,exp.factor.list,by="env_names",all.x=T)
  exposure.factors <- unique(exposure_factor_means$Exposure.Factor)
  
  #######################################################################
  #### Get the needed data to make the plot for this species
  
  # Get the scientific name
  scientific.name <- species.functional.sorted$Scientific.Name[which(species.functional.sorted$EF.Stock.Name==ef_species_name)]
  
  # get the scores
  overall.scores.row <- which(overall.scores$Code.Name==code.name)  
  exposure.rank <- overall.scores$EF_Score[overall.scores.row] # exposure score
  sensitivity.rank <- overall.scores$SA_Score[overall.scores.row] # sensitivity score
  vulnerability.rank <- overall.scores$Final_Rank[overall.scores.row] # vulnerability score
  data.quality.rank <- round(length(which(sens_att_means$sen_att_dq_mean>=2))/13,digits=2) # proportion of DQ scores >= 2
  distribution.potential.rank<-overall.scores$Distribution_Score[overall.scores.row]
  direction.effects.rank<-overall.scores$Direction_Effects_Rank[overall.scores.row]
  
  # uncertainty % (technically its the % certainty, but it's labeled as uncertainty in the overall.scores table, just know the value refers to the percent certainty)
  vul.cert<-overall.scores$Final_Rank_Uncertainty[overall.scores.row]
  dist.cert<-overall.scores$Distribution_Score_Uncertainty[overall.scores.row]
  dir.eff.cert<-overall.scores$Directional_Effects_Uncertainty[overall.scores.row]
  
  # change name to match rest of code
  data.sensitivity <- sens_att_means
  # change name to match rest of code
  data.exposure <- exposure_factor_means
  
  #######################################################################
  #### Plot species name and vulnerability rank at the top of the page
  
  dev.new(w=8.5, h=11)
  png(paste(files.folder.name, narrative.name, ' species narrative image.png', sep=''), pointsize=12, width=8.5, height=11,units="in",res=300)
  frame()
  par(xpd=TRUE)
  
  
  y.start <- 1.08
  y.diff <- 0.032
  box.diff <- 0.1  #to make the vuln color boxes line up better with the text
  
  # species common and scientific name
  text(x=-0.13, y=y.start, labels=bquote(.(narrative.name)~"("*italic(.(scientific.name))*")"), xpd=TRUE, pos=4, cex=1.2) # species name
  
  # overall vulnerability score
  # Original spacing code replaced with improved spacing from Habitat CVA code
  text(x=-0.13, y=y.start-2*y.diff, labels=paste('Overall Vulnerability Rank =', vulnerability.rank), 
       xpd=TRUE, pos=4, cex=1.2)
  # Correct spacing for png output
  points(x=-0.13+.39+spacing.rank(vulnerability.rank), y=y.start+box.diff*y.diff-2*y.diff, pch=22,  xpd=TRUE, cex=2.5, 
         bg=assign.colors.words(vulnerability.rank), col='black')
  #add certainty %
  text(x=-0.13+.39+spacing.rank(vulnerability.rank)+.02,y=y.start-2*y.diff,labels=paste("(",vul.cert,"% certainty)",sep=""), 
       xpd=TRUE, pos=4, cex=1.2)
  
  # sensitivity score
  text(x=-0.13, y=y.start-3.5*y.diff, labels=paste('Biological Sensitivity =', sensitivity.rank), 
       xpd=TRUE, pos=4, cex=1.2)
  # Correct spacing for png output
  points(x=-0.13+.32+spacing.rank(sensitivity.rank), y=y.start+box.diff*y.diff-3.5*y.diff, pch=22,  xpd=TRUE, cex=2.5, 
         bg=assign.colors.words(sensitivity.rank), col='black')
  
  # exposure score
  text(x=-0.13, y=y.start-4.5*y.diff, labels=paste('Climate Exposure =', exposure.rank), xpd=TRUE, pos=4, cex=1.2)
  # Correct spacing for png output
  points(x=-0.13+.28+spacing.rank(exposure.rank), y=y.start+box.diff*y.diff-4.5*y.diff, pch=22,  xpd=TRUE, cex=2.5, 
         bg=assign.colors.words(exposure.rank), col='black')
  
  # distribution shift potential
  text(x=-0.13, y=y.start-6*y.diff, labels=paste('Distributional Vulnerability Rank =', distribution.potential.rank), xpd=TRUE, pos=4, cex=1.2)
  # Correct spacing for png output
  points(x=-0.13+.47+spacing.rank(distribution.potential.rank), y=y.start+box.diff*y.diff-6*y.diff, pch=22,  xpd=TRUE, cex=2.5, 
         bg=assign.colors.words(distribution.potential.rank), col='black')
  #add certainty %
  text(x=-0.13+.47+spacing.rank(distribution.potential.rank)+.02,y=y.start-6*y.diff,labels=paste("(",dist.cert,"% certainty)",sep=""), 
       xpd=TRUE, pos=4, cex=1.2)
  
  # directional effect
  text(x=-0.13, y=y.start-7*y.diff, labels=paste('Directional Effect =', direction.effects.rank), xpd=TRUE, pos=4, cex=1.2)
  # no color box required here
  #add certainty %
  text(x=-0.13+.38,y=y.start-7*y.diff,labels=paste("(",dir.eff.cert,"% certainty)",sep=""), 
       xpd=TRUE, pos=4, cex=1.2)
  
  # data quality score
  text(x=-0.13, y=y.start-8.5*y.diff, 
       labels=paste('Data Quality = ', data.quality.rank*100, '% of scores', sep=''),
       xpd=TRUE, pos=4, cex=1.2)
  text(x=-0.13+.39, y=y.start-8.5*y.diff, labels=expression("\u2265", "   2"), xpd=TRUE, pos=4, cex=1.2)
  
  #######################################################################
  #### Plot the table of results
  
  #font fize
  size <- .79
  
  # where lines of the table are drawn
  x.table.min <- -0.13
  #   x.table.max <- 0.66
  x.table.max <- 0.895
  
  # x coordinates of the 3 columns
  x.column1 <- 0.183 
  x.column2 <- .49
  x.column3 <- .60
  x.column4 <- .77
  
  # y coordintes of the top row and space between rows
  y.row1 <- 0.75
  y.diff <- 0.03
  
  # plot species name and colun labels
  text(x.column1 - 0.06, y.row1-0.3*y.diff, bquote(italic(.(scientific.name))), cex=size*1.5)
  text(x.column2, y.row1, 'Attribute\nMean', cex=size)
  text(x.column3, y.row1, 'Data\nQuality', cex=size)
  text(x.column4, y.row1, 'Expert Scores Plots\n(tallies by bin)', cex=size)
  
  y.current <- y.row1-2*y.diff
  vertical4 <- y.current+0.5*y.diff
  polygon(c(x.table.min,x.table.max), c(y.current+0.5*y.diff, y.current+0.5*y.diff), lwd=2)
  polygon(c(x.table.min,x.table.max), c(y.row1+0.8*y.diff, y.row1+0.8*y.diff), lwd=2)
  
  
  ########################################################################
  # Plot sensitivity attribute names and scores
  for(j in 1:length(sensitivity.attributes)){
    rows <- which(data.sensitivity$sen_att_names==sensitivity.attributes[j])
    text(x.column1, y.current, sensitivity.attributes[j], cex=size)
    text(x.column2, y.current, round(data.sensitivity$sen_att_mean[rows], digits=1), cex=size)
    text(x.column3, y.current, round(data.sensitivity$sen_att_dq_mean[rows], digits=1), cex=size)
    
    ########################################################################
    # Plot barplots for this sensitivity attribute
    y.base <- y.current-0.5*y.diff
    
    # low
    y.new <- y.base+data.sensitivity$LpRec[rows]*.025
    polygon(c(x.column3+0.075,x.column3+0.075,x.column3+0.125,x.column3+0.125), c(y.base, y.new, y.new,y.base), border='black',col='green')
    # moderate
    y.new <- y.base+data.sensitivity$MpRec[rows]*.025
    polygon(c(x.column3+0.125,x.column3+0.125,x.column3+0.175,x.column3+0.175), c(y.base, y.new, y.new,y.base), border='black',col='yellow')
    # high
    y.new <- y.base+data.sensitivity$HpRec[rows]*.025
    polygon(c(x.column3+0.175,x.column3+0.175,x.column3+0.225,x.column3+0.225), c(y.base, y.new, y.new,y.base), border='black',col='orange')
    # very high
    y.new <- y.base+data.sensitivity$VpRec[rows]*.025
    polygon(c(x.column3+0.225,x.column3+0.225,x.column3+0.275,x.column3+0.275), c(y.base, y.new, y.new,y.base), border='black',col='red')
    
    
    ########################################################################
    # Add line below this row to form table
    polygon(c(x.table.min + 0.06,x.table.max), c(y.current-0.5*y.diff, y.current-0.5*y.diff))
    y.current <- y.current-y.diff
    
  }
  
  vertical1 <- y.current+0.5*y.diff
  
  # overall sensitivity score
  text(x.column1, y.current, "Sensitivity Score", cex=size*1.4)
  text(mean(c(x.column2,x.column3)), y.current, sensitivity.rank, cex=size*1.4)
  polygon(c(x.table.min,x.table.max), c(y.current-0.5*y.diff, y.current-0.5*y.diff), lwd=2)
  y.sensitivity.section.lower <- (y.current-0.5*y.diff)
  
  y.current <- y.current-y.diff
  vertical2 <- y.current+0.5*y.diff
  
  ########################################################################
  # Plot exposure attribute names and scores
  for(j in 1:length(exposure.factors)){
    rows <- which(data.exposure$Exposure.Factor==exposure.factors[j])
    text(x.column1, y.current, labels=exposure.factors[j], cex=size)
    if(ef_analysis=="yes"){
      text(x.column2, y.current, labels=round(data.exposure$exp_fact_mean[rows], digits=1), cex=size)
    }else{
      text(x.column2, y.current, labels="-", cex=size)
    }
    text(x.column3, y.current, labels="-", cex=size)
    
    ########################################################################
    # Add barplots for this exposure attribute
    y.base <- y.current-0.5*y.diff
    
    # low
    y.new <- y.base+(data.exposure$Lp[rows])*.025
    polygon(c(x.column3+0.075,x.column3+0.075,x.column3+0.125,x.column3+0.125), c(y.base, y.new, y.new,y.base), 
            border='black',col='green')
    # moderate
    y.new <- y.base+(data.exposure$Mp[rows])*.025
    polygon(c(x.column3+0.125,x.column3+0.125,x.column3+0.175,x.column3+0.175), c(y.base, y.new, y.new,y.base), 
            border='black',col='yellow')
    # high
    y.new <- y.base+(data.exposure$Hp[rows])*.025
    polygon(c(x.column3+0.175,x.column3+0.175,x.column3+0.225,x.column3+0.225), c(y.base, y.new, y.new,y.base), 
            border='black',col='orange')
    # very high
    y.new <- y.base+(data.exposure$Vp[rows])*.025
    polygon(c(x.column3+0.225,x.column3+0.225,x.column3+0.275,x.column3+0.275), c(y.base, y.new, y.new,y.base), 
            border='black',col='red')
    
    ########################################################################
    # Add line below this row to form table
    polygon(c(x.table.min + 0.06,x.table.max), c(y.current-0.5*y.diff, y.current-0.5*y.diff))
    y.current <- y.current-y.diff
    
  }
  
  vertical3 <- y.current+0.5*y.diff
  
  # overall exposure score
  text(x.column1, y.current, labels="Exposure Score", cex=size*1.4)
  if(ef_analysis=="yes"){
    text(mean(c(x.column2,x.column3)), y.current, labels=exposure.rank, cex=size*1.4)
  }else{
    text(mean(c(x.column2,x.column3)), y.current, labels="NA", cex=size*1.4)
  }
  polygon(c(x.table.min,x.table.max), c(y.current-0.5*y.diff, y.current-0.5*y.diff), lwd=2)
  y.exposure.section.lower <- y.current-0.5*y.diff
  
  y.current <- y.current-y.diff
  
  # overall vulnerability score
  text(x.column1, y.current, labels="Overall Vulnerability Rank", cex=size*1.4)
  if(ef_analysis=="yes"){
    text(mean(c(x.column2,x.column3)), y.current, labels=vulnerability.rank, cex=size*1.4)
  }else{
    text(mean(c(x.column2,x.column3)), y.current, labels="NA", cex=size*1.4)
  }
  polygon(c(x.table.min,x.table.max), c(y.current-0.5*y.diff, y.current-0.5*y.diff), lwd=2)
  
  y.min <- y.current-0.5*y.diff
  
  # plot the vertical lines of the table
  polygon(c(x.table.min,x.table.min), c(y.row1+0.8*y.diff, y.min), lwd=2)
  polygon(c(x.table.max,x.table.max), c(y.row1+0.8*y.diff, y.min), lwd=2)
  polygon(c(2*x.column2-mean(c(x.column2,x.column3)),2*x.column2-mean(c(x.column2,x.column3))), c(y.row1+0.8*y.diff, y.min))
  polygon(c(mean(c(x.column2,x.column3)),mean(c(x.column2,x.column3))), c(y.row1+0.8*y.diff, vertical1))
  polygon(c(mean(c(x.column2,x.column3)),mean(c(x.column2,x.column3))), c(vertical2, vertical3)) 
  polygon(c(mean(c(x.column3,x.column4-.08)),mean(c(x.column3,x.column4-.08))), c(y.row1+0.8*y.diff, vertical1))
  polygon(c(mean(c(x.column3,x.column4-.08)),mean(c(x.column3,x.column4-.08))), c(vertical2, vertical3))
  polygon(c(x.table.min + 0.06, x.table.min + 0.06), c(vertical4, vertical3 - y.diff))
  
  # Label the Sensitivty and exposure attribute sections
  text(x.table.min + 0.03, (vertical4+y.sensitivity.section.lower)/2, "Sensitivity Attributes", cex=size, srt=90)
  text(x.table.min + 0.03, (y.sensitivity.section.lower+y.exposure.section.lower)/2, "Exposure Factors", cex=size, srt=90)
  
  # Plot the legend for the barplots
  legend(.88, y.row1, legend=c('Low','Moderate','High','Very High'), fill=c('green','yellow','orange','red'), 
         bty='n',x.intersp=0.25, cex=1)
  
  # close the plotting window
  dev.off()
  graphics.off()
  
}

