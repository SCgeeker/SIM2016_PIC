library(dplyr)
library(stringi)

RAW_PATH = "D:\\core\\Research\\projects\\Embodied\\EXPDATA\\ActionObject\\2016_Simulation\\Data\\EXP02\\MT\\1027_100subjects_pilot\\"

subj_path <- dir(RAW_PATH,pattern = "list[1-4]_[0-9]{1,2}-[0-9]{1,2}.csv")
        #dir(RAW_PATH, pattern = "[0-5]{1,3}.csv")

#tmp <- read.csv(paste0(RAW_PATH,subj_path[1]), allowEscapes = TRUE, na.strings = " ", quote = "", comment.char = "", check.names = TRUE, skipNul = TRUE)

subj_raw <- data.frame(
        Size <- factor(),
        Orien <- factor(),
        Match <- factor(),
        BlockID <- as.character(),
        TrialNr <- as.numeric(),
        Sent_RT <- as.numeric(),
        RT <- as.numeric(),
        ACC <- as.numeric(),
        EndTime <- as.numeric()
)

subj_raw01 <- NULL
subj_raw02 <- NULL
subj_raw03 <- NULL
subj_raw04 <- NULL

for(i in 1:5){
        subj_raw01 <-  rbind( subj_raw01, read.csv(paste0(RAW_PATH,subj_path[i]), allowEscapes = TRUE, na.strings = " ", quote = "", comment.char = "", check.names = FALSE, skipNul = TRUE, header = TRUE, encoding = "utf-16LE")[,c("Size","Orien","Match","BlockId","SP_List1[TrialNr]","SentenceItem[RT]","PictureItem[RT]","PictureItem[ACC]")] )
}
colnames(subj_raw01) <- c("Size","Orien","Match","BlockID","TrialNr","Sent_RT","RT","ACC")

for(i in 6:10){
        subj_raw02 <-  rbind( subj_raw02, read.csv(paste0(RAW_PATH,subj_path[(i)]), allowEscapes = TRUE, na.strings = " ", quote = "", comment.char = "", check.names = FALSE, skipNul = TRUE, header = TRUE, encoding = "utf-16LE")[,c("Size","Orien","Match","BlockId","SP_List2[TrialNr]","SentenceItem[RT]","PictureItem[RT]","PictureItem[ACC]")] )
}
colnames(subj_raw02) <- c("Size","Orien","Match","BlockID","TrialNr","Sent_RT","RT","ACC")

for(i in 11:15){
        subj_raw03 <-  rbind( subj_raw03, read.csv(paste0(RAW_PATH,subj_path[(i)]), allowEscapes = TRUE, na.strings = " ", quote = "", comment.char = "", check.names = FALSE, skipNul = TRUE, header = TRUE, encoding = "utf-16LE")[,c("Size","Orien","Match","BlockId","SP_List3[TrialNr]","SentenceItem[RT]","PictureItem[RT]","PictureItem[ACC]")] )
}
colnames(subj_raw03) <- c("Size","Orien","Match","BlockID","TrialNr","Sent_RT","RT","ACC")
## List3, #19 has 133 trials

for(i in 16:20){
        subj_raw04 <- rbind( subj_raw04, read.csv(paste0(RAW_PATH,subj_path[(i)]), allowEscapes = TRUE, na.strings = " ", quote = "", comment.char = "", check.names = FALSE, skipNul = TRUE, header = TRUE, encoding = "utf-16LE")[,c("Size","Orien","Match","BlockId","SP_List4[TrialNr]","SentenceItem[RT]","PictureItem[RT]","PictureItem[ACC]")] )
}
colnames(subj_raw04) <- c("Size","Orien","Match","BlockID","TrialNr","Sent_RT","RT","ACC")

subj_raw = rbind(rbind( rbind(subj_raw01,subj_raw02), subj_raw03), subj_raw04)

subj_raw = cbind(ID = paste0("pilot1027",sprintf("%03d",c(rep(1:68,134),rep(69,133),rep(70:100,134) )) ),subj_raw)
rm(subj_raw01, subj_raw02, subj_raw03, subj_raw04)

subj_critical = subj_raw[subj_raw$Size != "F" & subj_raw$BlockID != "SP_Prac",]

subj_ACC_check <- with(data=subj_critical,tapply(ACC, ID, mean)*100)

List_count <- with(data=subj_critical[subj_critical$ACC == 1,],table(BlockID))

# Calcuate the median RTs across 3 independent variables
subj_RT_median <- with(data=subj_critical[subj_critical$ACC == 1,],tapply(RT, paste(ID,Size, Orien, Match), median, na.rm=TRUE))

# Calcuate the median RTs across 2 independent variables
subj_RT_median2 <- with(data=subj_critical[subj_critical$ACC == 1,],tapply(RT, paste(ID,Size, Match), median, na.rm=TRUE))

# Calcuate the accuracies across 3 independent variables
subj_ACC_means <- with(data=subj_critical,tapply(ACC, paste(ID,Size, Orien, Match), mean)*100)

# Calcuate the accuracies across 2 independent variables
subj_ACC_means2 <- with(data=subj_critical,tapply(ACC, paste(ID,Size, Match), mean)*100)

subj_ReadTime_means <- with(data=subj_critical[subj_critical$ACC == 1,],tapply(Sent_RT, paste(ID,Size, Orien, Match), mean, na.rm=TRUE))

# Create the analytic data for 3 IV
subj_table = data.frame( matrix(unlist(stri_split_fixed(names(subj_RT_median), " ")),nc=4, byrow = TRUE ), RT = subj_RT_median, ACC = subj_ACC_means)
row.names(subj_table) <- NULL
colnames(subj_table) = c("ID","Size","Orientation","Match","RT","ACC")
subj_table = cbind(List = c( rep("List1", each=c( sum(table(subj_table$ID)[1:25]))), rep("List2", each=c( sum(table(subj_table$ID)[26:50]))),  rep("List3", each=c( sum(table(subj_table$ID)[51:75])) ), rep("List4", each=c( sum(table(subj_table$ID)[76:100])) )), subj_table )

# Create the analytic data for 2 IV
subj_table2 = data.frame( matrix(unlist(stri_split_fixed(names(subj_RT_median2), " ")),nc=3, byrow = TRUE ), RT = subj_RT_median2, ACC = subj_ACC_means2)
row.names(subj_table2) <- NULL
colnames(subj_table2) = c("ID","Size","Match","RT","ACC")
subj_table2 = cbind(List = c( rep("List1", each=c( sum(table(subj_table2$ID)[1:25]))), rep("List2", each=c( sum(table(subj_table2$ID)[26:50]))),  rep("List3", each=c( sum(table(subj_table2$ID)[51:75])) ), rep("List4", each=c( sum(table(subj_table2$ID)[76:100])) )), subj_table2 )


# Arrange the condition means (3 IV) in JASP format
LHY = subj_table[subj_table$Size == "L" & subj_table$Orientation == "H" & subj_table$Match == "Y",];colnames(LHY) <- c("List","ID","Size","Orientation","Match","LHY_RT","LHY_ACC")
LHN = subj_table[subj_table$Size == "L" & subj_table$Orientation == "H" & subj_table$Match == "N",];colnames(LHN) <- c("List","ID","Size","Orientation","Match","LHN_RT","LHN_ACC")
LVY = subj_table[subj_table$Size == "L" & subj_table$Orientation == "V" & subj_table$Match == "Y",];colnames(LVY) <- c("List","ID","Size","Orientation","Match","LVY_RT","LVY_ACC")
LVN = subj_table[subj_table$Size == "L" & subj_table$Orientation == "V" & subj_table$Match == "N",];colnames(LVN) <- c("List","ID","Size","Orientation","Match","LVN_RT","LVN_ACC")
SHY = subj_table[subj_table$Size == "S" & subj_table$Orientation == "H" & subj_table$Match == "Y",];colnames(SHY) <- c("List","ID","Size","Orientation","Match","SHY_RT","SHY_ACC")
SHN = subj_table[subj_table$Size == "S" & subj_table$Orientation == "H" & subj_table$Match == "N",];colnames(SHN) <- c("List","ID","Size","Orientation","Match","SHN_RT","SHN_ACC")
SVY = subj_table[subj_table$Size == "S" & subj_table$Orientation == "V" & subj_table$Match == "Y",];colnames(SVY) <- c("List","ID","Size","Orientation","Match","SVY_RT","SVY_ACC")
SVN = subj_table[subj_table$Size == "S" & subj_table$Orientation == "V" & subj_table$Match == "N",];colnames(SVN) <- c("List","ID","Size","Orientation","Match","SVN_RT","SVN_ACC")

jasp_data_3IV <- full_join(LHY[,c(1,2,6,7)], full_join(LHN[,c(1,2,6,7)], full_join(LVY[,c(1,2,6,7)], full_join(LVN[,c(1,2,6,7)], full_join(SHY[,c(1,2,6,7)], full_join(SHN[,c(1,2,6,7)],full_join(SVY[,c(1,2,6,7)],SVN[,c(1,2,6,7)], by = "ID"), by = "ID"), by = "ID"), by = "ID"), by = "ID"), by = "ID"), by = "ID")[,c(1,2,3,4,6,7,9,10,12,13,15,16,18,19,21,22,24,25)]
colnames(jasp_data_3IV)[1] = "List"
rm(LHY,LHN,LVY,LVN,SHY,SHN,SVY,SVN)
## Erase the participants with low accuracies
jasp_data_3IV = filter(jasp_data_3IV, ID != jasp_data_3IV$ID[is.na(jasp_data_3IV$SHN_RT)])

# Arrange the condition means (2 IV) in JASP format
jasp_data_2IV <- cbind(matrix(subj_RT_median2,nc=4,byrow = TRUE, dimnames = list(ID = levels(subj_table$ID),with(subj_table, paste(Size, Match))[c(1:2,5:6)])),matrix(subj_ACC_means2,nc=4,byrow = TRUE) )
jasp_data_2IV = data.frame(List = rep(c("List1","List2","List3","List4"),each=25), ID = row.names(jasp_data_2IV), jasp_data_2IV)
row.names(jasp_data_2IV) <- NULL
colnames(jasp_data_2IV) <- c("List", "ID", "LN_RT", "LY_RT", "SN_RT", "SY_RT", "LN_ACC", "LY_ACC", "SN_ACC", "SY_ACC")
jasp_data_2IV = filter(jasp_data_2IV, ID %in% jasp_data_3IV$ID)

write.csv(jasp_data_3IV, file = paste0(RAW_PATH,"pilot1027_3IV.csv"), na = "")
write.csv(jasp_data_2IV, file = paste0(RAW_PATH,"pilot1027_2IV.csv"))
write.csv(subj_table, file = paste0(RAW_PATH,"subj_table1027_3IV.csv"))
write.csv(subj_table2, file = paste0(RAW_PATH,"subj_table1027_2IV.csv"))


#subj001$SentenceItem.CalculatedDuration.
#names(subj001)
#col_summary <- c(17:19, 56, 54, 33, 43,45)


#assign( paste0("subj",sprintf("%03d",i)),read.csv(paste0(RAW_PATH,subj_path[i]), allowEscapes = TRUE, na.strings = " ", quote = "", comment.char = "", check.names = FALSE, skipNul = TRUE, encoding = "utf-16LE")[c(17:19, 56, 54, 33, 43,45)] )
