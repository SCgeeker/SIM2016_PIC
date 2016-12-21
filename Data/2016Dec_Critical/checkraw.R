library(dplyr)
library(stringi)

## Import all raw data from three groups

From_folder1 <- "D:\\core\\Research\\projects\\Embodied\\EXPDATA\\ActionObject\\2016_Simulation\\Data\\2016Dec_Critical\\Dutch_Chinese_LAB\\"
From_folder2 <- "D:\\core\\Research\\projects\\Embodied\\EXPDATA\\ActionObject\\2016_Simulation\\Data\\2016Dec_Critical\\English_MTurk\\"
To_folder <- "D:\\core\\Research\\projects\\Embodied\\EXPDATA\\ActionObject\\2016_Simulation\\Data\\2016Dec_Critical\\"

rawdata_files1 <- list.files(path = From_folder1, pattern = "_rawdata", recursive = TRUE, include.dirs = TRUE)

rawdata_files2 <- list.files(path = From_folder2, pattern = "_rawdata", recursive = TRUE, include.dirs = TRUE)

# Import cleaned raw data of verification task
data_TW <- read.csv(file = paste0(From_folder1,rawdata_files1[1]), na.strings = NA)
data_NL <- read.csv(file = paste0(From_folder1,rawdata_files1[2]), na.strings = NA)
data_US <- read.csv(file = paste0(From_folder2,rawdata_files2), na.strings = NA)
data_US$Lang = "E"

# check accuracies for Chinese by participant
with(data = data_TW, tapply(correct,ID,mean) )
with(data = data_TW, tapply(Comprehension_correct,ID,mean, na.rm = TRUE) )
sum(with(data = data_TW, table(List)/128 ) )
# check accuracies for Dutch by participant
with(data = data_NL, tapply(correct,ID,mean) )
with(data = data_NL, tapply(Comprehension_correct,ID,mean, na.rm = TRUE) )
sum(with(data = data_NL, table(List)/128 ) )
# check accuracies for English by participant
with(data = data_US, tapply(correct,ID,mean) )
with(data = data_US, tapply(Comprehension_correct,ID,mean, na.rm = TRUE) )
sum(with(data = data_US, table(List)/128 ) )

data_all <- rbind( data_US[,names(data_US)!="Item_index"],rbind(data_NL[,!names(data_NL)%in%c("Probe","Target")], data_TW[,!names(data_NL)%in%c("Probe","Target")]))

# compare the probe sentence reading time among three groups
Reading_Time = with(data = data_all, tapply(sentence_read_time, paste(Lang,ID),median))

GROUP = matrix( unlist(stri_split_regex(names(Reading_Time)," ") ), byrow = TRUE, nc=2 )[,1]

Summary_Reading_Time <- tapply(Reading_Time, GROUP, summary)

Comp_Reading_Time <- summary(aov(Reading_Time~GROUP))
        
# Summarize participants' comprehension/memory accuracies
rec_all_ACC <- 100*with(data = data_all, tapply(as.numeric(Comprehension_correct), paste0(Lang,"-", ID), mean, na.rm = TRUE))
# Check participants' comprehension/memory accuracies < .50
names(rec_all_ACC[rec_all_ACC < 50])

# Test memory/comprehesion accuracy based on the pre-registered criterion
t.test(rec_all_ACC[grep("C",names(rec_all_ACC))], mu=50, alternative = "greater")
t.test(rec_all_ACC[grep("D",names(rec_all_ACC))], mu=50, alternative = "greater")
t.test(rec_all_ACC[grep("E",names(rec_all_ACC))], mu=50, alternative = "greater")

# Retrieve the correct responses of critical items in the raw data
CRITIC_all <- subset(data_all, correct == 1 & Size != "F")

# Summarize participant data for 3 IV analysis
RT = with(data = CRITIC_all, tapply(Target_response_time, paste(Lang, List, ID, Size, Orien, Match, sep = "-"), median))
ACC = 100*with(data = subset(data_all, Size!="F"), tapply(correct, paste(Lang, List, ID, Size, Orien, Match, sep = "-"), mean))

# Save the particiants' median RTs and accuracies in the data frame
RT_DF = data.frame(
          matrix(unlist(stri_split_fixed(names(RT), "-")), ncol = 6, byrow = TRUE),
          RT,
          ACC,
        row.names = NULL
)

colnames(RT_DF) = c("Lang", "List", "ID","Size","Orientation","Match","RT","ACC")

# Export 3 IV analytic data
write.csv(RT_DF, file = paste0(To_folder,"3IV_analytic_data.csv"),row.names = FALSE)

# Export 3 IV analytic data for jasp
jasp_3IV <- data.frame(
        Lang = as.vector(RT_DF$Lang[seq(1,dim(RT_DF)[1],by=8)]),
        List = as.vector(RT_DF$List[seq(1,dim(RT_DF)[1],by=8)]),
        ID = as.vector(RT_DF$ID[seq(1,dim(RT_DF)[1],by=8)]),
        matrix(RT_DF$RT,nc=8,byrow = TRUE,dimnames = list(1:(length(paste0(RT_DF$Lang,RT_DF$ID))/8),unique(with(data = RT_DF, paste0(Size, Orientation, Match,"_RT"))))),
        matrix(RT_DF$ACC,nc=8,byrow = TRUE,dimnames = list(1:(length(paste0(RT_DF$Lang,RT_DF$ID))/8),unique(with(data = RT_DF, paste0(Size, Orientation, Match,"_ACC")))))
)

write.csv(arrange(jasp_3IV,ID), file = paste0(To_folder,"3IV_analytic_jasp.csv"),row.names = FALSE)

# Size X Orientation X Match
# Codition medians
round(with(data = RT_DF, tapply(RT, paste(Lang, Size, Orientation, Match), median)),digits = 2)
# Codition means
round(with(data = RT_DF, tapply(RT, paste(Lang, Size, Orientation, Match), mean)),digits = 2)
# Condition accuracies
round(with(data = RT_DF, tapply(ACC, paste(Lang, Size, Orientation, Match), mean)),digits = 2)

rm(RT_DF)

# Summarize participant data for 2 IV analysis
RT = with(data = CRITIC_all, tapply(Target_response_time, paste(Lang, List, ID, Size, Match, sep = "-"), median))
ACC = 100*with(data = subset(data_all, Size!="F"), tapply(correct, paste(Lang, List, ID, Size, Match, sep = "-"), mean))

# Save the particiants' median RTs and accuracies in the data frame
RT_DF = data.frame(
        matrix(unlist(stri_split_fixed(names(RT), "-")), ncol = 5, byrow = TRUE),
        RT,
        ACC,
        row.names = NULL
)

# Export 2IV analytic data
colnames(RT_DF) = c("Lang","List", "ID","Size","Match","RT","ACC")
write.csv(RT_DF, file = paste0(To_folder,"2IV_analytic_data.csv"),row.names = FALSE)

# Export 2 IV analytic data for jasp
jasp_2IV <- data.frame(
        Lang = as.vector(RT_DF$Lang[seq(1,dim(RT_DF)[1],by=4)]),
        List = as.vector(RT_DF$List[seq(1,dim(RT_DF)[1],by=4)]),
        ID = as.vector(RT_DF$ID[seq(1,dim(RT_DF)[1],by=4)]),
        matrix(RT_DF$RT,nc=4,byrow = TRUE,dimnames = list(1:(length(paste0(RT_DF$Lang,RT_DF$ID))/4),unique(with(data = RT_DF, paste0(Size, Match,"_RT"))))),
        matrix(RT_DF$ACC,nc=4,byrow = TRUE,dimnames = list(1:(length(paste0(RT_DF$Lang,RT_DF$ID))/4),unique(with(data = RT_DF, paste0(Size, Match,"_ACC")))))
)
write.csv(arrange(jasp_2IV, ID), file = paste0(To_folder,"2IV_analytic_jasp.csv"),row.names = FALSE)


# Size X Match
# Codition medians
round(with(data = RT_DF, tapply(RT, paste(Lang, Size, Match), median)),digits = 2)
# Codition means
round(with(data = RT_DF, tapply(RT, paste(Lang, Size, Match), mean)),digits = 2)
# Condition accuracies
round(with(data = RT_DF, tapply(ACC, paste(Lang, Size, Match), mean)),digits = 2)
rm(RT_DF)

rm(list = ls())

