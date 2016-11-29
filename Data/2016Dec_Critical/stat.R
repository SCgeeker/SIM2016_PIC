DATA_PATH = "D:\\core\\Research\\projects\\Embodied\\EXPDATA\\ActionObject\\2016_Simulation\\Data\\EXP_Cri\\"

# import analytical data
DATA_2IV <- read.csv(paste0(DATA_PATH,"2IV_analytic_data.csv"))
DATA_3IV <- read.csv(paste0(DATA_PATH,"3IV_analytic_data.csv"))

# Take the means and medians for 2 IV
RT_Summary_A <- matrix(unlist( with(data = subj_table_2IV,
                tapply(RT, paste0(Lang, Size, Match), summary)
        ) ), nc = 6, byrow = TRUE)
dimnames(RT_Summary_A) <- list(paste(rep(c("C,","D,","E,"),each=4),rep(c("Large Size, Mismatch", "Large Size, Match", "Small Size, Mismatch", "Small Size, Match"), 3) ), c("Min.", "1st Qu.",  "Median", "Mean", "3rd Qu.", "Max"))

# ANOVA for 2 IV
RT_aov_A <- with(data = subj_table_2IV,
     summary( aov( RT ~ Lang*Size*Match + Error(ID/(Size*Match))))     )

# Take the means and medians for 3 IV
RT_Summary_B <- matrix(unlist( with(data = subj_table_3IV,
                                    tapply(RT, paste0(Size, Orientation, Match), summary)
) ), nc = 6, byrow = TRUE)
dimnames(RT_Summary_B) <- list(paste(rep(c("C,","D,","E,"),each=8),rep(c("Large Size, Horizontal, Mismatch", "Large Size, Horizontal, Match", "Large Size, Vertical, Mismatch", "Large Size, Vertical, Match", "Small Size, Horizontal, Mismatch", "Small Size, Horizontal, Match", "Small Size, Vertical, Mismatch", "Small Size, Vertical, Match"),3)), c("Min.", "1st Qu.",  "Median", "Mean", "3rd Qu.", "Max"))

# ANOVA for 3 IV
RT_aov_B <- with(data = subj_table_3IV,
                 summary(aov(RT ~ Lang*List*Size*Orientation*Match + Error(ID/(Size*Orientation*Match) ) ))   )

