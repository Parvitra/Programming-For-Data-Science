install.packages("magrittr")
install.packages("dplyr")
install.packages("devtools")
install.packages("missForest")
install.packages("tidyverse")
install.packages("ggpubr")


library(dplyr)
library(magrittr)
library(devtools)
library(missForest)
library(tidyverse)
library(tidyr)
library(ggpubr)
theme_set(theme_pubr())


library(ggplot2)
library(tidyr)
library(reshape)
library(ggthemes)
library(RJSONIO)

ESGData <- read.csv(file="C:/Users/user/Desktop/ESGData.csv", header=TRUE, sep=",")
ESGData <-  ESGData %>% filter(SI.POV.NAHC, AG.PRD.FOOD.XD, NV.AGR.TOTL.ZS,SH.DYN.MORT,
                               SE.ENR.PRSC.FM.ZS,SE.XPD.TOTL.GD.ZS,SL.TLF.CACT.FM.ZS,
                               SH.STA.SMSS.ZS,EG.ELC.COAL.ZS,EG.USE.COMM.FO.ZS,
                               EG.FEC.RNEW.ZS,EN.ATM.CO2E.PC,NY.GDP.MKTP.KD.ZG,
                               SL.TLF.ACTI.ZS,SL.UEM.TOTL.ZS,GB.XPD.RSDV.GD.ZS,
                               IP.PAT.RESD,NY.ADJ.DRES.GN.ZS,NY.ADJ.DFOR.GN.ZS,
                               PV.EST %in% c("EG.CFT.ACCS.ZS","EG.ELC.ACCS.ZS","NY.ADJ.DRES.GN.ZS", "NY.ADJ.DFOR.GN.ZS",
                                             "AG.LND.AGRI.ZS","NV.AGR.TOTL.ZS","ER.H2O.FWTL.ZS", "SI.SPR.PCAP.ZG",
                                             "SH.DTH.COMM.ZS","SL.TLF.0714.ZS","EN.ATM.CO2E.PC","CC.EST",
                                             "EN.CLC.CDDY.XD","EN.CLC.MDAT.ZS","IC.BUS.EASE.XQ","EG.ELC.COAL.ZS",
                                             "EG.IMP.CONS.ZS","EG.EGY.PRIM.PP.KD","EG.USE.PCAP.KG.OE","SP.DYN.TFRT.IN",
                                             "AG.PRD.FOOD.XD","AG.LND.FRST.ZS","EG.USE.COMM.FO.ZS", "NY.GDP.MKTP.KD.ZG",
                                             "EN.CLC.GHGR.MT.CE",  "SI.POV.GINI", "GE.EST", "SE.XPD.TOTL.GB.ZS",     
                                             "EN.CLC.HEAT.XD","SH.MED.BEDS.ZS","SI.DST.FRST.20", "IT.NET.USER.ZS",
                                             "SL.TLF.ACTI.ZS", "SP.DYN.LE00.IN","SE.ADT.LITR.ZS", "EN.MAM.THRD.NO",
                                             "EN.CLC.PRCP.XD","EN.CLC.SPEI.XD","EN.ATM.METH.PC","SH.DYN.MORT",
                                             "SM.POP.NETM","EN.ATM.NOXE.PC","IP.PAT.RESD","SH.H2O.SMDW.ZS",
                                             "SH.STA.SMSS.ZS","EN.ATM.PM25.MC.M3","PV.EST", "SP.POP.65UP.TO.ZS",
                                             "EN.POP.DNST","SI.POV.NAHC","SH.STA.OWAD.ZS","SN.ITK.DEFC.ZS",
                                             "SG.GEN.PARL.ZS","SL.TLF.CACT.FM.ZS","RQ.EST","EG.ELC.RNEW.ZS",
                                             "EG.FEC.RNEW.ZS","GB.XPD.RSDV.GD.ZS", "RL.EST", "SE.PRM.ENRR",
                                             "SE.ENR.PRSC.FM.ZS","IP.JRN.ARTC.SC","IC.LGL.CRED.XQ", "ER.PTD.TOTL.ZS",
                                             "SL.UEM.TOTL.ZS", "SP.UWT.TFRT","VA.EST"))
#Filter CountryName
ESGData_SDG_ASIA <- ESGData %>% filter(country %in% c("China","India","Indonesia","Pakistan",
                                                              "Bangladesh","Japan","Philippines","Vietnam",
                                                              "Turkey","Iran, Islamic Rep.","Thailand","Myanmar",
                                                              "Korea, Rep.","Iraq","Afghanistan","Saudi Arabia",
                                                              "Uzbekistan","Malaysia","Yemen, Rep.","Nepal",
                                                              "Korea, Dem. People's Rep.","Sri Lanka","Kazakhstan","Syrian Arab Republic",
                                                              "Cambodia","Jordan","Azerbaijan","United Arab Emirates",
                                                              "Tajikistan","Israel","Lao PDR","Lebanon",
                                                              "Kyrgyz Republic","Turkmenistan","Singapore","West Bank and Gaza",
                                                              "Oman","Kuwait","Georgia","Mongolia",
                                                              "Armenia","Qatar","Bahrain","Timor-Leste",
                                                              "Cyprus","Bhutan","Maldives","Brunei Darussalam",
                                                              "Russian Federation","Taiwan"))

# Select features to remove
drops <- c("iso2c","iso3c", "capital", "longitude", "latitude", "income", "lending","region")
ESGData_SDG_ASIA <- ESGData_SDG_ASIA[ , !(names(ESGData_SDG_ASIA) %in% drops)]

# Name features
colnames(ESGData_SDG_ASIA) <- c("Country","Year", "Poverty_Population", "Food_Production", "Agriculture_GDP","Mortality_Rate","Gross_SchoolEnrollment","Education_GDP",
                  "LaborForce_GenderRate","Sanitationservices_Pop","Electricity_Production","Total_FossilConsumption","Total_RenewableConsumption",
                  "CO2emissions_capita","GDP_growth","Total_Labor_Age15_64","Total_Unemployment","R_D_GDP",
                  "Patent_Residents","Naturalresources_depletion","Netforest_depletion","Estimation_Political")


#remove categorical variables and Year
ESGData_SDG_ASIA.mis <- subset(ESGData_SDG_ASIA, select = -c(Country,Year))
summary(ESGData_SDG_ASIA.mis)

#impute missing value
imputed_Data <- missForest(ESGData_SDG_ASIA.mis,verbose = TRUE)

#OOB impututation error
imputed_Data$OOBerror

#before using ntree = total time for computation is 19.3 seconds
imputed_Data <- missForest(ESGData_SDG_ASIA.mis,verbose = TRUE,ntree=20)
#after using ntreee=20 , computation time is 5.42 seconds
ESGDATA_SDG_ASIA_New <- cbind(ESGData_SDG_ASIA$Country,ESGData_SDG_ASIA$Year,imputed_Data$ximp)

#Rename Column of  ESGDATA_NEW
colnames(ESGDATA_SDG_ASIA_New)[1:2] <- c("Country","Year")

#Statistical Analysis SingleRegression
#vizualization Q1 

ggplot(ESGDATA_SDG_ASIA_New, aes(x =Agriculture_GDP,y =Poverty_Population)) +
  geom_point() +
  stat_smooth()
#correlation coefficient
cor(ESGDATA_SDG_ASIA_New$Poverty_Population, ESGDATA_SDG_ASIA_New$Agriculture_GDP)
#result : 0.6720196

#intercept and the beta coefficient for the agriculture (GDP)variable
model <- lm(Poverty_Population~Agriculture_GDP, data = ESGDATA_SDG_ASIA_New)
model


#fit line - regression line
ggplot(ESGDATA_SDG_ASIA_New, aes(x =Agriculture_GDP,y = Poverty_Population)) +
  geom_point() +
  stat_smooth(method = lm)

#t-test&p-value
summary(model)
#============================================================================
#vizualization Q2-Environmental

ggplot(ESGDATA_SDG_ASIA_New, aes(x =Naturalresources_depletion,y =CO2emissions_capita)) +
  geom_point() +
  stat_smooth()
#correlation coefficient
cor(ESGDATA_SDG_ASIA_New$CO2emissions_capita, ESGDATA_SDG_ASIA_New$Naturalresources_depletion)
#result : 0.2440563

#intercept and the beta coefficient for the agriculture (GDP)variable
model <- lm(CO2emissions_capita~Naturalresources_depletion, data = ESGDATA_SDG_ASIA_New)
model


#fit line - regression line
ggplot(ESGDATA_SDG_ASIA_New, aes(x =Naturalresources_depletion,y = CO2emissions_capita)) +
  geom_point() +
  stat_smooth(method = lm)

#t-test&p-value
summary(model)

#============================================================================
#vizualization Q3-Governance

ggplot(ESGDATA_SDG_ASIA_New, aes(x =Gross_SchoolEnrollment,y =Sanitationservices_Pop)) +
  geom_point() +
  stat_smooth()
#correlation coefficient
cor(ESGDATA_SDG_ASIA_New$Sanitationservices_Pop, ESGDATA_SDG_ASIA_New$Gross_SchoolEnrollment)
#result : 0.3558884

#intercept and the beta coefficient for the agriculture (GDP)variable
model <- lm(Sanitationservices_Pop~Gross_SchoolEnrollment, data = ESGDATA_SDG_ASIA_New)
model


#fit line - regression line
ggplot(ESGDATA_SDG_ASIA_New, aes(x =Gross_SchoolEnrollment,y = Sanitationservices_Pop)) +
  geom_point() +
  stat_smooth(method = lm)

#t-test&p-value
summary(model)
