    

##########################################################################
########################      Data preparation     #######################
##########################################################################

library(dplyr)
########    merge data    ########
    data_TB_total<-list()
    data_DS_TB<-list()
    data_MDR_TB<-list()
    data_XDR_TB<-list()
    
    for (i in 1:length(year)) {
      data_TB_total[[i]]<-merge(TB_total[[i]][,-3],urban_final[[i]],
                              by.x="name",by.y ="iso_code",all.y = TRUE)%>%merge(weather_urban_final2[[i]],
                              by.x="iso_code_2",by.y ="ISO2C",all.x = TRUE)
      data_DS_TB[[i]]<-merge(DS_TB[[i]][,-3],urban_final[[i]],
                                by.x="name",by.y ="iso_code",all.y = TRUE)%>%merge(weather_urban_final2[[i]],
                                by.x="iso_code_2",by.y ="ISO2C",all.x = TRUE)
      data_MDR_TB[[i]]<-merge(MDR_TB[[i]][,-3],urban_final[[i]],
                                by.x="name",by.y ="iso_code",all.y = TRUE)%>%merge(weather_urban_final2[[i]],
                                by.x="iso_code_2",by.y ="ISO2C",all.x = TRUE)
      data_XDR_TB[[i]]<-merge(XDR_TB[[i]][,-3],urban_final[[i]],
                                by.x="name",by.y ="iso_code",all.y = TRUE)%>%merge(weather_urban_final2[[i]],
                                by.x="iso_code_2",by.y ="ISO2C",all.x = TRUE)
     i<-i+1 
    }

#########  convert array to data frame ###########
    data_TB_totala<-data_TB_totaln[[1]]
    data_DS_TBa<-data_DS_TBn[[1]]
    data_MDR_TBa<-data_MDR_TBn[[1]]
    data_XDR_TBa<-data_XDR_TBn[[1]]
    
    for (i in 2:length(year)) {
      data_TB_totala<-rbind(data_TB_totala,data_TB_totaln[[i]])
      data_DS_TBa<-rbind(data_DS_TBa,data_DS_TBn[[i]])
      data_MDR_TBa<-rbind(data_MDR_TBa,data_MDR_TBn[[i]])
      data_XDR_TBa<-rbind(data_XDR_TBa,data_XDR_TBn[[i]])
      
      i<-i+1
    }