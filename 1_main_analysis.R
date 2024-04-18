
##########################################################################
########################        Main analysis      #######################
##########################################################################
    
########  load packages  ########
    library(plm)
    library(xts)
    library(tseries)

########  panel data  ########
    panel_TB<-pdata.frame(data_TB_totala,index=c("iso_code_2","year"))
    panel_DSTB<-pdata.frame(data_DS_totala,index=c("iso_code_2","year"))
    panel_MDR<-pdata.frame(data_MDR_totala,index=c("iso_code_2","year"))
    panel_XDR<-pdata.frame(data_XDR_totala,index=c("iso_code_2","year"))

########  unit root test for stationary time series   ########
    tlist1<-xts(panel_TB$urban_score,as.Date(panel_TB$year))
    adf.test(tlist1) 
    
    tlist2<-xts(panel_DSTB$urban_score,as.Date(panel_DSTB$year))
    adf.test(tlist2) 
    
    tlist3<-xts(panel_MDR$urban_score,as.Date(panel_MDR$year))
    adf.test(tlist3) 
    
    tlist4<-xts(panel_XDR$urban_score,as.Date(panel_XDR$year))
    adf.test(tlist4) 

####  model formula  ####    
    TB_in<-log(incidence)~urban_score+log10(gni)+log10(density)+old_people+UHC_index+
      PM2.5+Temp_med+humidity_med+Wind_med
    TB_pre<-log(prevalence)~urban_score+log10(gni)+log10(density)+old_people+UHC_index+
      PM2.5+Temp_med+humidity_med+Wind_med
    TB_dea<-log(mortality)~urban_score+log10(gni)+log10(density)+old_people+UHC_index+
      PM2.5+Temp_med+humidity_med+Wind_med
    
    
###################################################
###################  total TB  ####################
###################################################
    
####incidence
    fit_in1<-plm(TB_in,effect="twoways",data=panel_TB,model = "within")
   
    #test for fixed-effects or random effects                        
    fit_in1t<-plm(TB_in,data=panel_TB,model = "random")
    phtest(fit_in1t,fit_in1)  ##Hausman test
    
####prevalence
    fit_pre1<-plm(TB_pre,effect="twoways",data=panel_TB,model = "within")##两种

    #test for fixed-effects or random effects                        
    fit_pre1t<-plm(TB_pre,data=panel_TB,model = "random")
    phtest(fit_pre1t,fit_pre1)
    
####death
    fit_dea1<-plm(TB_dea,effect="twoways",data=panel_TB,model = "within")##两种

    #test for fixed-effects or random effects                        
    fit_dea1t<-plm(TB_dea,data=panel_TB,model = "random")
    phtest(fit_dea1t,fit_dea1)
    

###################################################
###################    DS TB   ####################
###################################################
    
####incidence
    fit_in2<-plm(TB_in,effect="twoways",data=panel_DSTB,model = "within")
   
    #test for fixed-effects or random effects                        
    fit_in2t<-plm(TB_in,data=panel_DSTB,model = "random")
    phtest(fit_in2t,fit_in2)  ##Hausman test
    
####prevalence
    fit_pre2<-plm(TB_pre,effect="twoways",data=panel_DSTB,model = "within")##两种

    #test for fixed-effects or random effects                        
    fit_pre2t<-plm(TB_pre,data=panel_DSTB,model = "random")
    phtest(fit_pre2t,fit_pre2)
    
####death
    fit_dea2<-plm(TB_dea,effect="twoways",data=panel_DSTB,model = "within")##两种

    #test for fixed-effects or random effects                        
    fit_dea2t<-plm(TB_dea,data=panel_DSTB,model = "random")
    phtest(fit_dea2t,fit_dea2)
    
    
###################################################
###################   MDR TB   ####################
###################################################
    
####incidence
    fit_in3<-plm(TB_in,effect="twoways",data=panel_MDR,model = "within")
   
    #test for fixed-effects or random effects                        
    fit_in3t<-plm(TB_in,data=panel_MDR,model = "random")
    phtest(fit_in3t,fit_in3)  ##Hausman test
    
####prevalence
    fit_pre3<-plm(TB_pre,effect="twoways",data=panel_MDR,model = "within")##两种

    #test for fixed-effects or random effects                        
    fit_pre3t<-plm(TB_pre,data=panel_MDR,model = "random")
    phtest(fit_pre3t,fit_pre3)
    
####death
    fit_dea3<-plm(TB_dea,effect="twoways",data=panel_MDR,model = "within")##两种

    #test for fixed-effects or random effects                        
    fit_dea3t<-plm(TB_dea,data=panel_MDR,model = "random")
    phtest(fit_dea3t,fit_dea3)
    

###################################################
###################   XDR TB   ####################
###################################################
    
####incidence
    fit_in4<-plm(TB_in,effect="twoways",data=panel_XDR,model = "within")
   
    #test for fixed-effects or random effects                        
    fit_in4t<-plm(TB_in,data=panel_XDR,model = "random")
    phtest(fit_in4t,fit_in4)  ##Hausman test
    
####prevalence
    fit_pre4<-plm(TB_pre,effect="twoways",data=panel_XDR,model = "within")##两种

    #test for fixed-effects or random effects                        
    fit_pre4t<-plm(TB_pre,data=panel_XDR,model = "random")
    phtest(fit_pre4t,fit_pre4)
    
####death
    fit_dea4<-plm(TB_dea,effect="twoways",data=panel_XDR,model = "within")##两种

    #test for fixed-effects or random effects                        
    fit_dea4t<-plm(TB_dea,data=panel_XDR,model = "random")
    phtest(fit_dea4t,fit_dea4)
    
    
    
    