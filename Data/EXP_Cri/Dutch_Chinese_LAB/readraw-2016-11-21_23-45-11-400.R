# Retrieve TW raw data
From_folder <- "D:\\core\\Research\\projects\\Embodied\\EXPDATA\\ActionObject\\2016_Simulation\\Data\\EXP_Cri\\Dutch_Chinese_LAB\\Chinese\\"
To_folder <- "D:\\core\\Research\\projects\\Embodied\\EXPDATA\\ActionObject\\2016_Simulation\\Data\\EXP_Cri\\Dutch_Chinese_LAB\\"

# Import multiple-bytes string in English system
Sys.setlocale("LC_ALL","English") 

# Import Taiwan raw data files
# This loop is modified from Ananda Mahto's reply @ http://stackoverflow.com/questions/11433432/importing-multiple-csv-files-into-r
rawfiles_TW <- list.files(From_folder, pattern = "*.csv", recursive = TRUE)

# Take the columns for pre-processing
# Comments comprise the code book of raw data
V <- c(
        "logfile", # ID = filename
        "List",    # Stimuli lists
        "Task",  # V = Verification task; M = recognition task
        "count_Trial", # Trial sequence
        "Size",  # within-participant variable  
        "Orien", # within-participant variable  
        "Match", # within-participant variable  
        "Probe", # Probe sentence in each trial
        "Target", # Target picture in each trial
        "correct_response", # Correct key in each trial
        "response_time_Probe_response", # Reaction time of reading sentence
        "response_time_Target_response", # Reaction time of verifying target
        "correct"       # Correctness in each tiral
)

# create the raw data frame for pre-processing
data_TW <- NULL
for(i in seq(rawfiles_TW)){
        data_TW <- rbind(
                data_TW,
                cbind(
                        read.csv(paste0(From_folder,rawfiles_TW[i]), encoding = "utf-8")[,V]
                )
        )
}

# save filtered raw file to RAW folder
write.csv(data_TW, file=paste0(To_folder,"\\","Chinese_raw.csv"))

## ------------------------------------------------
# Combine NL raw data
From_folder <- "D:\\core\\Research\\projects\\Embodied\\EXPDATA\\ActionObject\\2016_Simulation\\Data\\EXP_Cri\\Dutch_Chinese_LAB\\Dutch\\"
To_folder <- "D:\\core\\Research\\projects\\Embodied\\EXPDATA\\ActionObject\\2016_Simulation\\Data\\EXP_Cri\\Dutch_Chinese_LAB\\"

# Import multiple-bytes string in English system
Sys.setlocale("LC_ALL","English") 

# Import Rotterdam raw data to data frame
# This loop is modified from Ananda Mahto's reply @ http://stackoverflow.com/questions/11433432/importing-multiple-csv-files-into-r
rawfiles_NL <- list.files(From_folder, pattern = "*.csv", recursive = TRUE)

# Take the columns for pre-processing
V <- c(
        "logfile", # ID = filename
        "List",    # Stimuli lists
        "Task",  # V = Verification task; M = recognition task
        "count_Trial", # Trial sequence
        "Size",  # within-participant variable  
        "Orien", # within-participant variable  
        "Match", # within-participant variable  
        "Probe", # Probe sentence in each trial
        "Target", # Target picture in each trial
        "correct_response", # Correct key in each trial
        "response_time_Probe_response", # Reaction time of reading sentence
        "response_time_Target_response", # Reaction time of verifying target
        "correct"       # Correctness in each tiral
)

# create the raw data frame for pre-processing
data_NL <- NULL
for(i in seq(raw_NL)){
        data_NL <- rbind(
                data_NL,
                cbind(
                        ID = rep(gsub("*.csv$", "",raw_NL[i]), 128), 
                        read.csv(paste0(To_folder,"\\NL_pilot\\",raw_NL[i]), encoding = "utf-8")[,V]
                        
                )
        )
}

# save filtered raw file to RAW folder
write.csv(data_NL, file=paste0(To_folder,"\\","NL_pilot_raw.csv"))
# remove the temporary raw files
file.remove(paste0(To_folder,"\\NL_pilot\\",raw_NL))
rm(i,V,From_folder, To_folder,raw_TW,raw_NL)

# compare the probe sentence reading time
NL_Reading_Time = summary(data_NL$response_time_Probe_response)
TW_Reading_Time = summary(data_TW$response_time_Probe_response)
Comp_Reading_Time <- t.test(
        with(data = data_NL, tapply(response_time_Probe_response,ID,mean)), with(data = data_TW, tapply(response_time_Probe_response,ID,mean)), var.equal = TRUE)

