RAW_PATH = "D:\\core\\Research\\projects\\Embodied\\EXPDATA\\ActionObject\\2016_Simulation\\Data\\EXP02\\MT\\1027_100subjects_pilot\\"

# import analytical data for 3 IV
subj_table_2IV <- read.csv(paste0(RAW_PATH,"subj_table1027_2IV.csv"))[,2:7]
subj_table_3IV <- read.csv(paste0(RAW_PATH,"subj_table1027_3IV.csv"))[,2:7]
#subj_table$ID = as.character(subj_table$ID)
#subj_table$Size = as.character(subj_table$Size)
#subj_table$Orientation = as.character(subj_table$Orientation)
#subj_table$Match = as.character(subj_table$Match)
### Merge orientation

# Take the means and medians for 2 IV
RT_Summary_A <- matrix(unlist( with(data = subj_table_2IV,
                tapply(RT, paste0(Size, Match), summary)
        ) ), nc = 6, byrow = TRUE)
dimnames(RT_Summary_A) <- list(c("Large Size, Mismatch", "Large Size, Match", "Small Size, Mismatch", "Small Size, Match"), c("Min.", "1st Qu.",  "Median", "Mean", "3rd Qu.", "Max"))

# ANOVA for 2 IV
RT_aov_A <- with(data = subj_table_2IV,
     summary( aov( RT ~ Size*Match + Error(ID/(Size*Match))))     )

# Take the means and medians for 3 IV
RT_Summary_B <- matrix(unlist( with(data = subj_table_3IV,
                                    tapply(RT, paste0(Size, Orientation, Match), summary)
) ), nc = 6, byrow = TRUE)
dimnames(RT_Summary_B) <- list(c("Large Size, Horizontal, Mismatch", "Large Size, Horizontal, Match", "Large Size, Vertical, Mismatch", "Large Size, Vertical, Match", "Small Size, Horizontal, Mismatch", "Small Size, Horizontal, Match", "Small Size, Vertical, Mismatch", "Small Size, Vertical, Match"), c("Min.", "1st Qu.",  "Median", "Mean", "3rd Qu.", "Max"))

# ANOVA for 3 IV
RT_aov_B <- with(data = subj_table_3IV,
                 summary(aov(RT ~ List*Size*Orientation*Match + Error(ID/(Size*Orientation*Match) ), na.rm=TRUE))   )

