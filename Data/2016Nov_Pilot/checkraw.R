library(dplyr)
library(stringi)

# check accuracies for Rotterda by participant
with(data = data_NL, tapply(correct,paste(ID,correct_response),mean) )
sum(with(data = data_NL, table(List)/128 ) )
# check accuracies for Taiwan by participant
with(data = data_TW, tapply(correct,paste(ID,correct_response),mean) ) sum(with(data = data_TW, table(List)/128 ) )

# Retrieve the correct responses of critical items in the raw data
CRITIC_NL <- subset(data_NL, correct == 1 & Size != "F")

CRITIC_TW <- subset(data_TW, correct == 1 & Size != "F")

# Summarize participant data for 3 IV analysis
RT = with(data = CRITIC_NL, tapply(response_time_Target_response, paste(List, ID, Size, Orien, Match, sep = "-"), median))
ACC = 100*with(data = subset(data_NL, Size!="F"), tapply(correct, paste(List, ID, Size, Orien, Match, sep = "-"), sum))/8

# Save the particiants' median RTs and accuracies in the data frame
RT_DF = data.frame(
          matrix(unlist(stri_split_fixed(names(RT), "-")), ncol = 8, byrow = TRUE),
          RT,
          ACC,
        row.names = NULL
)

colnames(RT_DF) = c("List","Lang", "Gender", "Age","ID","Size","Orientation","Match","RT","ACC")

rm(RT, ACC)

# Save 3 IV data frame in the CSV file
write.csv(RT_DF, file = paste0("D:\\core\\Research\\projects\\Embodied\\EXPDATA\\ActionObject\\2016_Simulation\\Data\\EXP02\\RAW\\","NL_pilot_3IV_data.csv"))
rm(RT_DF)

# Summarize participant data for 2 IV analysis
RT = with(data = CRITIC_NL, tapply(response_time_Target_response, paste(List, ID, Size, Match, sep = "-"), median))
ACC = 100*with(data = subset(data_NL, Size!="F"), tapply(correct, paste(List, ID, Size, Match, sep = "-"), sum))/16

# Save the particiants' median RTs and accuracies in the data frame
RT_DF = data.frame(
        matrix(unlist(stri_split_fixed(names(RT), "-")), ncol = 7, byrow = TRUE),
        RT,
        ACC,
        row.names = NULL
)

colnames(RT_DF) = c("List","Lang", "Gender", "Age","ID","Size","Match","RT","ACC")
rm(RT, ACC)

# Save 2IV data frame in to the CSV file
write.csv(RT_DF, file = paste0("D:\\core\\Research\\projects\\Embodied\\EXPDATA\\ActionObject\\2016_Simulation\\Data\\EXP02\\RAW\\","NL_pilot_2IV_data.csv"))
rm(RT_DF)



# Size X Match
# Codition medians
round(with(data = RT_DF, tapply(RT, paste(Size, Match), median)),digits = 2)
# Codition means
round(with(data = RT_DF, tapply(RT, paste(Size, Match), mean)),digits = 2)
# Condition accuracies
round(with(data = RT_DF, tapply(ACC, paste(Size, Match), mean)),digits = 2)
# Size X Orientation X Match
# Codition medians
round(with(data = RT_DF, tapply(RT, paste(Size, Orientation, Match), median)),digits = 2)
# Codition means
round(with(data = RT_DF, tapply(RT, paste(Size, Orientation, Match), mean)),digits = 2)
# Condition accuracies
round(with(data = RT_DF, tapply(ACC, paste(Size, Orientation, Match), mean)),digits = 2)

# Build the data frame for plot 
Conditions_Stat <- data.frame(
        RT_MEAN = round(with(data = RT_DF, tapply(RT, paste(Lang, Size, Pic, Match), mean)),digits = 2),
        RT_SD = round(with(data = RT_DF, tapply(RT, paste(Lang, Size, Pic, Match), sd)),digits = 2),
        ACC_MEAN = round(with(data = RT_DF, tapply(PE, paste(Lang, Size, Pic, Match), mean)),digits = 2),
        ACC_SD = round(with(data = RT_DF, tapply(PE, paste(Lang, Size, Pic, Match), sd)),digits = 2)
)

Conditions_Stat = data.frame(matrix(unlist(stri_split(row.names(Conditions_Stat), regex = " ")), ncol = 4, byrow = TRUE, dimnames = list(1:16,c("Lang", "Size", "Pic", "Match"))), Conditions_Stat)
# 
levels(Conditions_Stat$Size) <- c("Large", "Small")
levels(Conditions_Stat$Pic) <- c("Default", "Rotated")
levels(Conditions_Stat$Match) <- c("No", "Yes")
Conditions_Stat$Match <- factor(Conditions_Stat$Match, levels = c("Yes", "No"))

with(data = RT_DF, table(Lang, ID) )
Sample_N <- with(data = RT_DF, tapply(ID, Lang, unique) )
## After correct the arrangement of ID and condtion lables, the results of Taiwan participants perfectly fits the prediction, but the results of EUR participants had a reversed pattern on small objects. In addition, the accuracy of Taiwan participants is higher than EUR participants. 
