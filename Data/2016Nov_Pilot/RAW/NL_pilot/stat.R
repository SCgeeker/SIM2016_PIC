library(dplyr)
library(stringi)

# Read analytical data
RAW_PATH = "D:\\core\\Research\\projects\\Embodied\\EXPDATA\\ActionObject\\2016_Simulation\\Data\\EXP02\\RAW\\"
NL_3IV <- read.csv(paste0(RAW_PATH,"NL_pilot_3IV_data.csv"))[,-1]
NL_2IV <- read.csv(paste0(RAW_PATH,"NL_pilot_2IV_data.csv"))[,-1]

# Make data set for jasp
jasp_NL_3IV <- data.frame(
        List = as.vector(NL_3IV$List[seq(1,320,by=8)]),
        ID = as.vector(NL_3IV$ID[seq(1,320,by=8)]),
        matrix(NL_3IV$RT,nc=8,byrow = TRUE,dimnames = list(1:40,unique(with(data = NL_3IV, paste0(Size, Orientation, Match,"_RT"))))),
        matrix(NL_3IV$ACC,nc=8,byrow = TRUE,dimnames = list(1:40,unique(with(data = NL_3IV, paste0(Size, Orientation, Match,"_ACC")))))
)

write.csv(arrange(jasp_NL_3IV,ID), file = paste0(RAW_PATH,"NL_pilot_3IV_jasp.csv"))

jasp_NL_2IV <- data.frame(
        List = as.vector(NL_2IV$List[seq(1,160,by=4)]),
        ID = as.vector(NL_2IV$ID[seq(1,160,by=4)]),
        matrix(NL_2IV$RT,nc=4,byrow = TRUE,dimnames = list(1:40,unique(with(data = NL_3IV, paste0(Size, Match,"_RT"))))),
        matrix(NL_2IV$ACC,nc=4,byrow = TRUE,dimnames = list(1:40,unique(with(data = NL_3IV, paste0(Size, Match,"_ACC")))))
)

write.csv(arrange(jasp_NL_2IV, ID), file = paste0(RAW_PATH,"NL_pilot_2IV_jasp.csv"),row.names = FALSE)

# Descriptive statistics of 2IV data
RT_Summary_2IV <- matrix(unlist( with(data = NL_2IV,
                                    tapply(RT, paste0(Size, Match), summary)
) ), nc = 6, byrow = TRUE)
dimnames(RT_Summary_2IV) <- list(c("Large Size, Mismatch", "Large Size, Match", "Small Size, Mismatch", "Small Size, Match"), c("Min.", "1st Qu.",  "Median", "Mean", "3rd Qu.", "Max"))

# ANOVA for 2 IV
NL_2IV$ID = as.factor(NL_2IV$ID)
RT_aov_2IV <- with(data = NL_2IV,
                 summary( aov( RT ~ Size*Match + Error(ID/(Size*Match))))     )

# Descriptive statistics of 3IV data
RT_Summary_3IV <- matrix(unlist( with(data = NL_3IV,
tapply(RT, paste0(Size, Orientation, Match), summary)
) ), nc = 6, byrow = TRUE)
dimnames(RT_Summary_3IV) <- list(c("Large Size, Horizontal, Mismatch", "Large Size, Horizontal, Match", "Large Size, Vertical, Mismatch", "Large Size, Vertical, Match", "Small Size, Horizontal, Mismatch", "Small Size, Horizontal, Match", "Small Size, Vertical, Mismatch", "Small Size, Vertical, Match"), c("Min.", "1st Qu.",  "Median", "Mean", "3rd Qu.", "Max"))

# ANOVA for 3 IV
NL_3IV$ID = as.factor(NL_3IV$ID)
RT_aov_3IV <- with(data = NL_3IV,
                 summary(aov(RT ~ Size*Orientation*Match + Error(ID/(Size*Orientation*Match) ))   ) )
