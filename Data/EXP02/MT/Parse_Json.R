library(dplyr)
## Define the source folder
RAW_PATH = "D:\\core\\Research\\projects\\Embodied\\EXPDATA\\ActionObject\\2016_Simulation\\Data\\EXP02\\MT\\1027_100subjects_pilot\\"
## Define the parsed data 
FILE <- "9b81d288-e3b5-4261-9fd3-9b3ff28d736d.csv"
## Define the available Qualtrics ID
ID <- "Orientation_Size_ENG_R2_ID.csv"

## Import the parsed raw data
Resp_RAW = read.csv(paste0(RAW_PATH,FILE), encoding = "utf-8") # X_recordId
## Import the Qualtrics research ID
Research_ID = read.csv(paste0(RAW_PATH,ID),encoding="utf-8")[-1,] # X_recordId
## Turn Reserach_ID to vectors
Research_ID[,2] = as.vector(Research_ID[,2])
## Make Lists
List_labels <- c(rep("SP_List1",length(grep("SP_List1",Research_ID[,2]))), rep("SP_List2",length(grep("SP_List2",Research_ID[,2]))), rep("SP_List3",length(grep("SP_List3",Research_ID[,2]))), rep("SP_List4",length(grep("SP_List4",Research_ID[,2]))))
List_seq <- c(grep("SP_List1",Research_ID[,2]),grep("SP_List2",Research_ID[,2]),grep("SP_List3",Research_ID[,2]),grep("SP_List4",Research_ID[,2])) 
Research_ID = cbind(Research_ID, List = arrange(data.frame(cbind(List_labels ,as.integer(List_seq)) ), List_seq )[,1])
rm(List_labels, List_seq)
## Merge by research ID
Resp_RAW = merge(Resp_RAW, Research_ID, by="X_recordId")
rm(Research_ID)
## Sort raw data bay endDate
Resp_RAW = arrange(Resp_RAW, endDate)

## Remove the participants who do this experiment twice
Repeated_ID <- c("1977932", "570714")
Resp_RAW = Resp_RAW[order(Resp_RAW$mTurkCode %in% Repeated_ID,decreasing = TRUE)[3:115],]

## Count the participants in each list
table(Resp_RAW$List)

## Check column names
write.csv(file = paste0(RAW_PATH,"names.txt"), names(Resp_RAW),  row.names = FALSE, fileEncoding = "utf-8")

## Split raw data by List and take the previous 25
Resp_RAW01 = Resp_RAW[Resp_RAW$List == "SP_List1",grep("X[0-9]{1,3}_QID[0-9]{1,3}_1",names(Resp_RAW))]
Resp_RAW02 = Resp_RAW[Resp_RAW$List == "SP_List2",grep("X[0-9]{1,3}_QID[0-9]{1,3}_1",names(Resp_RAW))][1:25,]
Resp_RAW03 = Resp_RAW[Resp_RAW$List == "SP_List3",grep("X[0-9]{1,3}_QID[0-9]{1,3}_1",names(Resp_RAW))][1:25,]
Resp_RAW04 = Resp_RAW[Resp_RAW$List == "SP_List4",grep("X[0-9]{1,3}_QID[0-9]{1,3}_1",names(Resp_RAW))][1:25,]

## Save each 5 participants in one json
for(i in seq(1,25, by = 5)){
        write.table(Resp_RAW01[i:(i+4),], file = paste0(RAW_PATH,paste0("RAW_JSON01",sprintf("%02d",i),".txt") ), quote = FALSE, eol = "", row.names = FALSE, col.names = FALSE, fileEncoding = "utf-8")
}

for(i in seq(1,25, by = 5)){
        write.table(Resp_RAW02[i:(i+4),], file = paste0(RAW_PATH,"RAW_JSON02",sprintf("%02d",i),".txt") , quote = FALSE, eol = "", row.names = FALSE, col.names = FALSE, fileEncoding = "utf-8")
}


for(i in seq(1,25, by = 5)){
        write.table(Resp_RAW03[i:(i+4),], file = paste0(RAW_PATH,"RAW_JSON03",sprintf("%02d",i),".txt") , quote = FALSE, eol = "", row.names = FALSE, col.names = FALSE, fileEncoding = "utf-8")
}

for(i in seq(1,25, by = 5)){
        write.table(Resp_RAW04[i:(i+4),], file = paste0(RAW_PATH,"RAW_JSON04",sprintf("%02d",i),".txt") , quote = FALSE, eol = "", row.names = FALSE, col.names = FALSE, fileEncoding = "utf-8")
}


