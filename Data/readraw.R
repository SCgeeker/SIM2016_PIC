# Read single csv files
setwd("D:/core/Research/projects/Embodied/EXPDATA/ActionObject/2016_Simulation/Data")
rawfile_names <- list.files(pattern = "*.csv")

# The following loop is modified from Ananda Mahto's reply @ http://stackoverflow.com/questions/11433432/importing-multiple-csv-files-into-r
for(i in 1:length(rawfile_names))
        assign(make.names(gsub("*.csv$", "", rawfile_names[i])), read.csv(rawfile_names[i], fileEncoding = "UTF-8"))

# Included columns:
# form_response, response_time, response_time_Probe_C, response_time_Probe_C, correct, correct_response
