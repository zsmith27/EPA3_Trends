#==============================================================================
# Author: Ross Mandel
# Date: ~ October, 2015
# Maintained: Zachary M. Smith
# Purpose: The script was written to aggregate and organize Legacy STORET
#          Sampling Event data (files containing "res").
# Output: A single table of Sampling Event data for the specified State.
#==============================================================================
get_res <- function(){
  # Assume in top of state tree directory  
  home.dir <- getwd() #get home directory
  dirs<-list.dirs(full.names = TRUE) #get list of directories in vector form
  for (i in seq_along(dirs)){
    setwd(dirs[i]) # go to sub-directory
    print(dirs[i])
    dfiles <- list.files(pattern = glob2rx("*res*.txt")) # get list of files with data
    for (j in seq_along(dfiles)){
      if (grepl("inv", dfiles[j])) next 
      if (grepl("sta", dfiles[j])) next 
      print(dfiles[j])
      #read file (tab-delimited) as dataframe, skip header
      temp.df<-read.delim(dfiles[j], header = FALSE, skip = 2, 
                          stringsAsFactors = FALSE)
      if ("master.df" %in% ls()){
        master.df <- rbind(master.df, temp.df)
      } else {
        master.df <- temp.df
      }
      
      
    }# end dfiles
    setwd(home.dir) # go back to home
  }#end dirs
  colnames(master.df) <- c("Agency","Station","Station.Name","Agency.Name",
                         "State.Name","County.Name","Latitude","Longitude",
                         "Result.Value","R","HUC","Param","Start.Date",
                         "Start.Time","End.Date","End.Time","Sample.Depth",
                         "UMK","Replicate.Number","CS","COMPOSITE_GRAB_NUMBER",
                         "CM","Primary.Activity.Category","Secondary.Activity.Category")
  return (master.df)
}
#==============================================================================
setwd("C:/Users/zsmith/Desktop/WQ Trends/Legacy STORET/Raw Data/District_Of_Columbia")
DC <- get_res()

setwd("C:/Users/zsmith/Desktop/WQ Trends/Legacy STORET/Raw Data/Delaware")
DE <- get_res()

setwd("C:/Users/zsmith/Desktop/WQ Trends/Legacy STORET/Raw Data/Maryland")
MD <- get_res()

setwd("C:/Users/zsmith/Desktop/WQ Trends/Legacy STORET/Raw Data/Pennsylvania")
PA <- get_res()

setwd("C:/Users/zsmith/Desktop/WQ Trends/Legacy STORET/Raw Data/Virginia")
VA <- get_res()

setwd("C:/Users/zsmith/Desktop/WQ Trends/Legacy STORET/Raw Data/West_Virginia")
WV <- get_res()
