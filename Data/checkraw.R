library(dplyr)
library(stringi)

with(data = data_EUR, tapply(correct,paste(ID,correct_response),mean) )  # check accuracy by participant

with(data = data_TCU, tapply(correct,paste(ID,correct_response),mean) )  # check accuracy by participant

CRITIC_EUR <- subset(data_EUR, (correct_response == "/" & correct == 1))
with(data = CRITIC_EUR, tapply(correct,ID,table)/80 )
#CORRECT_EUR <- subset(CRITIC_EUR, correct == 1)
CRITIC_TCU <- subset(data_TCU, (correct_response == "/" & correct == 1))
with(data = CRITIC_TCU, tapply(correct,ID,table)/80 )

## Functions to check outliers
up_out <- function(x){
        mean(x) + 2*sd(x)
}
        
down_out <- function(x){
        mean(x) - 2*sd(x)
}

#CRITIC_EUR = arrange(CRITIC_EUR, desc(size))

## Filter EUR response data
UP <- with(data = CRITIC_EUR, tapply(response_time, paste(size, Pic, Match, sep = "-"), up_out))

DOWN <- with(data = CRITIC_EUR, tapply(response_time, paste(size, Pic, Match, sep = "-"), down_out))

FILTER_EUR <- NULL

for(i in 1:length(UP)){
        FILTER_EUR = rbind(
                FILTER_EUR,
                filter(CRITIC_EUR, paste(size, Pic, Match, sep = "-") == names(UP)[i], response_time > DOWN[i], response_time < UP[i])
        )
}

#FILTER_EUR = data.frame(LANG = rep("ENG",dim(FILTER_EUR)[1]),FILTER_EUR)

## Filter TCU response data
UP <- with(data = CRITIC_TCU, tapply(response_time, paste(size, Pic, Match, sep = "-"), up_out))

DOWN <- with(data = CRITIC_TCU, tapply(response_time, paste(size, Pic, Match, sep = "-"), down_out))

FILTER_TCU <- NULL

for(i in 1:length(UP)){
        FILTER_TCU = rbind(
                FILTER_TCU,
                filter(CRITIC_TCU, paste(size, Pic, Match, sep = "-") == names(UP)[i], response_time > DOWN[i], response_time < UP[i])
        )
}

rm(i, UP, DOWN, down_out, up_out)

#RT <- c(
#        with(data = FILTER_EUR, tapply(response_time, paste(ID, size, Pic, Match, sep = "-"), mean)),
#        with(data = FILTER_TCU, tapply(response_time, paste(ID, size, Pic, Match, sep = "-"), mean)) 
#)

RT = with(data = rbind(FILTER_EUR, FILTER_TCU), tapply(response_time, paste(ID, size, Pic, Match, sep = "-"), mean))
PE = 100*with(data = subset(rbind(data_EUR, data_TCU), size!="D"), tapply(correct, paste(ID, size, Pic, Match, sep = "-"), sum))/10

RT_DF = data.frame(
          matrix(unlist(stri_split_fixed(names(RT), "-")), ncol = 7, byrow = TRUE),
          RT,
          PE,
        row.names = NULL
)

colnames(RT_DF) = c("Lang", "Cond", "List","ID","Size","Pic","Match","RT","PE")

rm(RT, PE)

# Build the data frame for plot 
Conditions_Stat <- data.frame(
        RT_MEAN = round(with(data = RT_DF, tapply(RT, paste(Lang, Size, Pic, Match), mean)),digits = 2),
        RT_SD = round(with(data = RT_DF, tapply(RT, paste(Lang, Size, Pic, Match), sd)),digits = 2),
        PE_MEAN = round(with(data = RT_DF, tapply(PE, paste(Lang, Size, Pic, Match), mean)),digits = 2),
        PE_SD = round(with(data = RT_DF, tapply(PE, paste(Lang, Size, Pic, Match), sd)),digits = 2)
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

## KICK_EUR = filter(RT_DF, ID != "001",ID != "008",ID != "011",ID != "002")
## with(data = KICK_EUR, tapply(RT, paste(Lang, Size, Pic, Match), mean))
## with(data = KICK_EUR, tapply(PE, paste(Lang, Size, Pic, Match), mean))
