library(dplyr)
library(stringi)


# Take the columns for pre-processing
# Comments describe the collected raw data of verifcation task
V <- c(
        "logfile", # filename = Lang-Gender-Age-ID
        "List",    # Stimuli lists
        "count_Trial", # Trial sequence
        "Size",  # within-participant variable  
        "Orien", # within-participant variable  
        "Match", # within-participant variable  
        "Probe", # Probe sentence in each trial
        "Target", # Target picture in each trial
        "response_time_Probe_response", # Reaction time of reading sentence
        "response_time_Target_response", # Reaction time of verifying target
        "correct",       # Correctness in each tiral
        "Comprehension_correct", # Correctness of recognition trials
        "datetime"  # Date and time collected data
)

# Comments describe the collected raw data of recognition task
M <- c(
        "alt_task", # Probe sentence in each trial
        "correct"       # Correctness in each tiral
)

# Retrieve TW raw data
From_folder <- "D:\\core\\Research\\projects\\Embodied\\EXPDATA\\ActionObject\\2016_Simulation\\Data\\EXP_Cri\\Dutch_Chinese_LAB\\Chinese\\"
To_folder <- "D:\\core\\Research\\projects\\Embodied\\EXPDATA\\ActionObject\\2016_Simulation\\Data\\EXP_Cri\\Dutch_Chinese_LAB\\"

# Import multiple-bytes string in English system
Sys.setlocale("LC_ALL","English") 

# Import Taiwan raw data files
# This loop is modified from Ananda Mahto's reply @ http://stackoverflow.com/questions/11433432/importing-multiple-csv-files-into-r
rawfiles_TW <- list.files(From_folder, recursive = TRUE)

# create the raw data frame for pre-processing
data_TW <- NULL
for(i in seq(rawfiles_TW)){
        tmp <- read.csv(paste0(From_folder,rawfiles_TW[i]), encoding = "utf-8")
        # Fetch the sequency of trial number in this experiment
        rec_tmp <- tmp[tmp$Task == "M",M]
        Comprehension_vector <- rep(NA, dim(tmp)[1])
        S <- gsub("\\[","",levels(tmp$alt_task))
        S <- gsub("\\]","",S)
        S <- as.numeric( unlist(stri_split_fixed(S, ", ", omit_empty = TRUE)) ) + 1
        Comprehension_correct = replace(Comprehension_vector, S,rec_tmp$correct)
        tmp = cbind(tmp, Comprehension_correct)
                                        
        data_TW <- rbind(
                data_TW, tmp[tmp$Task == "V",V]
        )
}

meta <- matrix(unlist(stri_split_fixed(data_TW$logfile, "-")), ncol = 4, byrow = TRUE,dimnames = list(seq(1,dim(data_TW)[1]),c("Lang","Gender","Age","ID")))  

data_TW = data.frame(meta, data_TW[,-1])

rm(meta, tmp, rec_tmp, Comprehension_correct, Comprehension_vector, i, S, rawfiles_TW) 
# Make the consistent column names
names(data_TW) <- c("Lang","Gender","Age","ID","List","Trial_Seq","Size","Orien","Match","Probe","Target","sentence_read_time","Target_response_time","correct","Comprehension_correct","datetime")
# save filtered raw file to RAW folder
write.csv(data_TW, file=paste0(To_folder,"\\","Chinese_Verication_rawdata.csv"), row.names = FALSE)

## ------------------------------------------------
# Combine NL raw data
From_folder <- "D:\\core\\Research\\projects\\Embodied\\EXPDATA\\ActionObject\\2016_Simulation\\Data\\EXP_Cri\\Dutch_Chinese_LAB\\Dutch\\"
To_folder <- "D:\\core\\Research\\projects\\Embodied\\EXPDATA\\ActionObject\\2016_Simulation\\Data\\EXP_Cri\\Dutch_Chinese_LAB\\"

# Export cleaned raw file to RAW folder
Sys.setlocale("LC_ALL","English") 

# Import Rotterdam raw data to data frame
# This loop is modified from Ananda Mahto's reply @ http://stackoverflow.com/questions/11433432/importing-multiple-csv-files-into-r
rawfiles_NL <- list.files(From_folder, recursive = TRUE)

# create the raw data frame for pre-processing
data_NL <- NULL
for(i in seq(rawfiles_NL)){
        tmp <- read.csv(paste0(From_folder,rawfiles_NL[i]), encoding = "utf-8")
        # Fetch the sequency of trial number in this experiment
        rec_tmp <- tmp[tmp$Task == "M",M]
        Comprehension_vector <- rep(NA, dim(tmp)[1])
        S <- gsub("\\[","",levels(tmp$alt_task))
        S <- gsub("\\]","",S)
        S <- as.numeric( unlist(stri_split_fixed(S, ", ", omit_empty = TRUE)) ) + 1
        Comprehension_correct = replace(Comprehension_vector, S,rec_tmp$correct)
        tmp = cbind(tmp, Comprehension_correct)
        
        data_NL <- rbind(
                data_NL, tmp[tmp$Task == "V",V]
        )
}

meta <- matrix(unlist(stri_split_fixed(data_NL$logfile, "-")), ncol = 4, byrow = TRUE,dimnames = list(seq(1,dim(data_NL)[1]),c("Lang","Gender","Age","ID")))  
data_NL = data.frame(meta, data_NL[,-1])

# Make the consistent column names
names(data_NL) <- c("Lang","Gender","Age","ID","List","Trial_Seq","Size","Orien","Match","Probe","Target","sentence_read_time","Target_response_time","correct","Comprehension_correct","datetime")

# save filtered raw file to RAW folder
write.csv(data_NL, file=paste0(To_folder,"\\","Dutch_Verication_rawdata.csv"), row.names = FALSE)

# compare the probe sentence reading time
NL_Reading_Time = summary(data_NL$sentence_read_time)
TW_Reading_Time = summary(data_TW$sentence_read_time)
Comp_Reading_Time <- t.test(
        with(data = data_NL, tapply(sentence_read_time,ID,mean)), with(data = data_TW, tapply(sentence_read_time,ID,mean)), var.equal = TRUE)

rm(list = ls())
