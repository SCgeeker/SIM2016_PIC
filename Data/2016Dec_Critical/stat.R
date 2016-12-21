library(ggplot2)

DATA_PATH = "D:\\core\\Research\\projects\\Embodied\\EXPDATA\\ActionObject\\2016_Simulation\\Data\\2016Dec_Critical\\"

# import analytical data
DATA_2IV <- read.csv(paste0(DATA_PATH,"2IV_analytic_data.csv"))
DATA_3IV <- read.csv(paste0(DATA_PATH,"3IV_analytic_data.csv"))

# Make ID as factor
DATA_2IV$ID <- as.factor(DATA_2IV$ID)
DATA_3IV$ID <- as.factor(DATA_3IV$ID)
# Rename labels in List
DATA_2IV$List = gsub("SP_|D_|C_|0|.csv","",DATA_2IV$List)
DATA_3IV$List = gsub("SP_|D_|C_|0|.csv","",DATA_3IV$List)
DATA_2IV$List <- as.factor(DATA_2IV$List)
DATA_3IV$List <- as.factor(DATA_3IV$List)
# Rename labels in Lang 
DATA_2IV$Lang = gsub("E","English",DATA_2IV$Lang)
DATA_2IV$Lang = gsub("C","Chinese",DATA_2IV$Lang)
DATA_2IV$Lang = gsub("D","Dutch",DATA_2IV$Lang)
DATA_3IV$Lang = gsub("E","English",DATA_3IV$Lang)
DATA_3IV$Lang = gsub("C","Chinese",DATA_3IV$Lang)
DATA_3IV$Lang = gsub("D","Dutch",DATA_3IV$Lang)
DATA_2IV$Lang <- as.factor(DATA_2IV$Lang)
DATA_3IV$Lang <- as.factor(DATA_3IV$Lang)
# Rename labels in Size
DATA_2IV$Size = gsub("L","Large",DATA_2IV$Size)
DATA_2IV$Size = gsub("S","Small",DATA_2IV$Size)
DATA_3IV$Size = gsub("L","Large",DATA_3IV$Size)
DATA_3IV$Size = gsub("S","Small",DATA_3IV$Size)
DATA_2IV$Size <- as.factor(DATA_2IV$Size)
DATA_3IV$Size <- as.factor(DATA_3IV$Size)

# Check the assignments of lists
with(data = DATA_3IV, tapply(List, Lang, table))

# Take the means and medians for 3 IV
RT_Summary_3IV <- matrix(unlist( with(data = DATA_3IV,
                                    tapply(RT, paste0(Lang, Size, Orientation, Match), summary)
) ), nc = 6, byrow = TRUE)
dimnames(RT_Summary_3IV) <- list(paste(rep(c("C,","D,","E,"),each=8),rep(c("Large Size, Horizontal, Mismatch", "Large Size, Horizontal, Match", "Large Size, Vertical, Mismatch", "Large Size, Vertical, Match", "Small Size, Horizontal, Mismatch", "Small Size, Horizontal, Match", "Small Size, Vertical, Mismatch", "Small Size, Vertical, Match"),3)), c("Min.", "1st Qu.",  "Median", "Mean", "3rd Qu.", "Max"))

# Plot match effects across langauges, object size,  and object orientation
plot_3IV <- ggplot(DATA_3IV,aes(x=paste(Size,Orientation),y=RT,fill=Match)) + stat_summary(fun.y=median,position=position_dodge(),geom="bar") + coord_cartesian( ylim = c(550, 750) ) + scale_fill_grey(start = 0, end = 0.9) + theme(panel.background = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(), panel.ontop = TRUE)+facet_wrap(~ Lang )

# ANOVA for 3 IV
RT_aov_3IV_list <- with(data = DATA_3IV,
                 summary(aov(RT ~ Lang*List*Size*Orientation*Match + Error(ID/(Size*Orientation*Match) ) ))   )

# Compute ANOVA by languages
RT_aov_3IV <- by(data = DATA_3IV, DATA_3IV$Lang,
               function(x) summary( aov( RT ~ Size*Orientation*Match + Error(ID/(Size*Orientation*Match)), data = x))     )


# Take the means and medians for 2 IV
RT_Summary_2IV <- matrix(unlist( with(data = DATA_2IV,
                                      tapply(RT, paste0(Lang, Size, Match), summary)
) ), nc = 6, byrow = TRUE)
dimnames(RT_Summary_2IV) <- list(paste(rep(c("C,","D,","E,"),each=4),rep(c("Large Size, Mismatch", "Large Size, Match", "Small Size, Mismatch", "Small Size, Match"), 3) ), c("Min.", "1st Qu.",  "Median", "Mean", "3rd Qu.", "Max"))

# Plot match effects across langauges and object size
plot_2IV <- ggplot(DATA_2IV,aes(x= Size,y=RT,fill=Match)) + stat_summary(fun.y=median,position=position_dodge(),geom="bar") + coord_cartesian( ylim = c(550, 750) ) + scale_fill_grey(start = 0, end = 0.9) + theme(panel.background = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(), panel.ontop = TRUE)+facet_wrap(~ Lang)


# ANOVA for 2 IV
# Check main effect of lists
RT_aov_2IV_list <- with(data = DATA_2IV,
                        summary( aov( RT ~ Lang*List*Size*Match + Error(ID/(Size*Match))))     )

# Compute ANOVA by languages
RT_aov_2IV <- by(data = DATA_2IV, DATA_2IV$Lang,
                 function(x) summary( aov( RT ~ Size*Match + Error(ID/(Size*Match)), data = x))     )


## Compute Bayes Factor
library(BayesFactor)
## Compute Bayes Factor for English, Large Objects
for(i in with(data = DATA_2IV, unique(paste(Lang,Size))) ){
        print(i)
        print( ttestBF( unlist( select( filter(DATA_2IV, Lang == unlist(strsplit(i, " "))[1]
, Size == unlist(strsplit(i, " "))[2]
, Match == "N"), RT) - select( filter(DATA_2IV, Lang == unlist(strsplit(i, " "))[1]
, Size == unlist(strsplit(i, " "))[2]
, Match == "Y"), RT) )  ) )  
}
