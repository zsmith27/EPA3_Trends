#==============================================================================
# Author: Ross Mandel
# Date: ~ October, 2015
# Maintained: Zachary M. Smith
# Purpose: The script was written to aggregate and organize Legacy STORET
#          Station data (files containing "sta").
# Output: A single table of Station data for the specified State.
#==============================================================================

#setwd("C:\\Users\\Zsmith\\Desktop\\WQ_Trends\\R\\District_Of_Columbia")
getWQ<-function(){
  # Assume in top of state tree directory
  home.dir<-getwd() #get home directory
  dirs<-list.dirs(full.names =TRUE) #get list of directories in vector form
  for (i in seq_along(dirs)){
    setwd(dirs[i]) # go to sub-directory
    print(dirs[i])
    dfiles<-list.files(pattern = glob2rx("*sta*.txt")) # get list of files with data
    for (j in seq_along(dfiles)){
      if (grepl("_inv",dfiles[j])) next
      if (grepl("_res",dfiles[j])) next
      if (grepl("'S",dfiles[j])) next
      print(dfiles[j])
      #read file (tab-delimited) as dataframe, skip header
      temp.df<-read.delim(dfiles[j], header = FALSE, skip = 2, quote = "")
      #subtemp.df <- temp.df[ grep('(E-|E+)', temp.df$Result.Value), ]
      
      if ("master.df" %in% ls()){
        master.df<-rbind(master.df,temp.df)
      } else {
        master.df <- temp.df
      }
      
      
    }# end dfiles
    setwd(home.dir) # go back to home
  }#end dirs
  colnames(master.df)<-c("Agency","Station","Station.Name","Agency.Name",
                         "State.Name","County.Name","Latitude","Longitude",
                         "HUC","Rchmile.Segment", "Miles.Up.Reach", "Rchonoff",
                         "Rchname", "Station.Alias", "Station.Type", 
                         "Station.Depth", "Depth.Units", "S", "G",
                         "Description")
  return (master.df)
}

#==============================================================================
setwd("C:/Users/zsmith/Desktop/WQ Trends/Legacy STORET/Raw Data/District_Of_Columbia")
DC <- getWQ()

setwd("C:/Users/zsmith/Desktop/WQ Trends/Legacy STORET/Raw Data/Delaware")
DE <- getWQ()

setwd("C:/Users/zsmith/Desktop/WQ Trends/Legacy STORET/Raw Data/Maryland")
MD <- getWQ()

setwd("C:/Users/zsmith/Desktop/WQ Trends/Legacy STORET/Raw Data/Pennsylvania")
PA <- getWQ()

setwd("C:/Users/zsmith/Desktop/WQ Trends/Legacy STORET/Raw Data/Virginia")
VA <- getWQ()

setwd("C:/Users/zsmith/Desktop/WQ Trends/Legacy STORET/Raw Data/West_Virginia")
WV <- getWQ()

#write.table(DC, file="C:\\Users\\Zsmith\\Desktop\\WQ_Trends\\R\\DC_Station.txt")
