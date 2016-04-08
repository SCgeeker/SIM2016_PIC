# Read single csv files
setwd("D:/core/Research/projects/Embodied/EXPDATA/ActionObject/2016_Simulation/Data")

From_folder <- "D:/Clouds/Dropbox/Dropbox/DESKS/ActObj/2015NOV/DATA"
To_folder <- "D:/core/Research/projects/Embodied/EXPDATA/ActionObject/2016_Simulation/"

file.copy(From_folder, To_folder, recursive = TRUE)

#rawfile_names <- list.files(pattern = "*.csv")

# The following loop is modified from Ananda Mahto's reply @ http://stackoverflow.com/questions/11433432/importing-multiple-csv-files-into-r

raw_EUR <- list.files(list.dirs()[2], pattern = "*.csv", recursive = TRUE)


V <- c(
        "List1",
        "List2",
        "List3",
        "List4",
        "Target",
        "correct_response",
        "response_time", # measurement
        "correct",       # measurement
        "size",  # within
        "Match", # within
        "Pic"    # within
)

data_EUR <- NULL

for(i in 1:length(raw_EUR)){
        data_EUR <- rbind(
                data_EUR,
                cbind(
                        ID = rep(gsub("*.csv$", "", raw_EUR[i]), 160), 
                        cbind(
                        Trial = 1:160,
                        read.csv(paste0("EUR/",raw_EUR[i]), fileEncoding = "UTF-8")[5:164,V]
                        )
                )
        )
}

# assign(make.names(gsub("*.csv$", "", raw_EUR[i])), )

## The unicode in raw files have to be transfered to unicode numbers.
raw_TCU <- list.files(list.dirs()[3], pattern = "*.csv", recursive = TRUE)


data_TCU <- NULL

for(i in 1:length(raw_TCU)){
        data_TCU <- rbind(
                data_TCU,
                cbind(
                        ID = rep(gsub("*.csv$", "", raw_TCU[i]), 160), 
                        cbind(
                                Trial = 1:160,
                                read.csv(paste0("TCU/",raw_TCU[i]))[5:164,V]
                        )
                )
        )
}

rm(From_folder, To_folder, i, raw_EUR, raw_TCU, V)
# Included columns:
# form_response, response_rtime, response_time_Probe_C, response_time_Probe_C, correct, correct_response
