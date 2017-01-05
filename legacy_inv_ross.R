#==============================================================================
# Author: Ross Mandel
# Date: ~ October, 2015
# Maintained: Zachary M. Smith
# Purpose: The script was written to aggregate and organize Legacy STORET
#          Inventory data (files containing "inv").
# Output: A single table of Inventory data for the specified State.
#==============================================================================
#setwd("C:\\Users\\Zsmith\\Desktop\\WQ_Trends\\R\\West_Virginia")
getWQ<-function(){
  # Assume in top of state tree directory
  home.dir<-getwd() #get home directory
  dirs<-list.dirs(full.names =TRUE) #get list of directories in vector form
  for (i in seq_along(dirs)){
    setwd(dirs[i]) # go to sub-directory
    print(dirs[i])
    dfiles<-list.files(pattern =glob2rx("*inv*.txt")) # get list of files with data
    for (j in seq_along(dfiles)){
      if (grepl("_sta",dfiles[j])) next
      if (grepl("_res",dfiles[j])) next
      if (grepl("'S",dfiles[j])) next
      print(dfiles[j])
      #read file (tab-delimited) as dataframe, skip header
      temp.df<-read.delim(dfiles[j], header = FALSE, skip = 7, skipNul = TRUE,
                          colClasses = c("factor", "character", "character",
                                         "numeric", "numeric", "character", "character",
                                         "character", "character", "numeric"))
      
      #subtemp.df <- temp.df[ grep('(E-|E+)', temp.df$Result.Value), ]
      temp.df <- na.omit(temp.df)
      
      if ("master.df" %in% ls()){
        master.df<-rbind(master.df, temp.df)
      } else {
        master.df <- temp.df
      }
      
      
    }# end dfiles
    setwd(home.dir) # go back to home
  }#end dirs
  colnames(master.df)<-c("Code", "Short.Name", "Long.Name", "Num.Stns",
                         "Num.Obs", "First.Date", "Last.Date", "Min.Value",
                         "Max.Value", "Average")
  master.df$Min.Value <- as.numeric(master.df$Min.Value)
  master.df$Max.Value <- as.numeric(master.df$Max.Value)
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



#write.table(WV, file="C:\\Users\\Zsmith\\Desktop\\WQ_Trends\\R\\WV_Params.txt")
