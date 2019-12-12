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
drops <- c("iso2c","iso3c", "capital", "longitude", "latitude", "income", "lending","region","EG.CFT.ACCS.ZS","EG.ELC.ACCS.ZS","AG.LND.AGRI.ZS","ER.H2O.FWTL.ZS", "SI.SPR.PCAP.ZG",
           "SH.DTH.COMM.ZS","SL.TLF.0714.ZS","CC.EST",
           "EN.CLC.CDDY.XD","EN.CLC.MDAT.ZS","IC.BUS.EASE.XQ",
           "EG.IMP.CONS.ZS","EG.EGY.PRIM.PP.KD","EG.USE.PCAP.KG.OE","SP.DYN.TFRT.IN",
           "AG.LND.FRST.ZS", "EN.CLC.GHGR.MT.CE",  "SI.POV.GINI", "GE.EST", "SE.XPD.TOTL.GB.ZS",     
           "EN.CLC.HEAT.XD","SH.MED.BEDS.ZS","SI.DST.FRST.20", "IT.NET.USER.ZS",
           "SP.DYN.LE00.IN","SE.ADT.LITR.ZS", "EN.MAM.THRD.NO","EN.CLC.PRCP.XD","EN.CLC.SPEI.XD","EN.ATM.METH.PC",
           "SM.POP.NETM","EN.ATM.NOXE.PC","SH.H2O.SMDW.ZS","EN.ATM.PM25.MC.M3","SP.POP.65UP.TO.ZS",
           "EN.POP.DNST","SH.STA.OWAD.ZS","SN.ITK.DEFC.ZS", "SG.GEN.PARL.ZS","RQ.EST","EG.ELC.RNEW.ZS",
           "RL.EST", "SE.PRM.ENRR","IP.JRN.ARTC.SC","IC.LGL.CRED.XQ", "ER.PTD.TOTL.ZS",
           "SP.UWT.TFRT","VA.EST")
ESGData_SDG_ASIA <- ESGData_SDG_ASIA[ , !(names(ESGData_SDG_ASIA) %in% drops)]

# Name features
colnames(ESGData_SDG_ASIA) <- c("X","Country","Year", "Poverty_Population", "Food_Production", "Agriculture_GDP","Mortality_Rate","Gross_SchoolEnrollment","Education_GDP",
                                "LaborForce_GenderRate","Sanitationservices_Pop","Electricity_Production","Total_FossilConsumption","Total_RenewableConsumption",
                                "CO2emissions_capita","GDP_growth","Total_Labor_Age15_64","Total_Unemployment","R_D_GDP",
                                "Patent_Residents","Naturalresources_depletion","Netforest_depletion","Estimation_Political")

drops <- c("X")
ESGData_SDG_ASIA <- ESGData_SDG_ASIA[ , !(names(ESGData_SDG_ASIA) %in% drops)]

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

#======================================PLOT VISUALIZATION===================


#Question No. 1 Goals: Poverty
view(ESGDATA_SDG_ASIA_New)
a <- ggplot(subset(ESGDATA_SDG_ASIA_New,  Year > "2005" & Year <= "2017"), aes(x = Agriculture_GDP, y = Poverty_Population)) + geom_point(aes(col=Country),size = 3) +
  coord_cartesian(xlim = c(0,40) , ylim = c(0,75)) +  facet_wrap(~ Year) + geom_smooth(method = "lm")+
  labs(title = "Agriculture_GDP Vs Poverty_Population", subtitle = "From ESGData_SDG_ASIA dataset", y="Poverty_Population",x="Agriculture_GDP", caption = "ESG Demographics")

ggplotly(a)


#Question No. 2 Environmental


b <- ggplot(subset(ESGDATA_SDG_ASIA_New,  Year > "2005" & Year <= "2017"), aes(x = Naturalresources_depletion, y = CO2emissions_capita)) + geom_point(aes(col=Country),size = 3) +
  coord_cartesian(xlim = c(0,60) , ylim = c(0,70)) +  facet_wrap(~ Year) + 
  labs(title = "Naturalresources_depletion Vs CO2emissions_capita", subtitle = "From ESGData_SDG_ASIA dataset", y="CO2emissions_capita",x="Naturalresources_depletion", caption = "ESG Demographics")

ggplotly(b)


#Question 3 Governace

c <- ggplot(subset(ESGDATA_SDG_ASIA_New,  Year > "2005" & Year <= "2017"), aes(x = Gross_SchoolEnrollment, y = Sanitationservices_Pop)) + geom_point(aes(col=Country),size = 3) +
  coord_cartesian(xlim = c(0,1.5) , ylim = c(0,100)) +  facet_wrap(~ Year) +
  labs(title = "Awareness of Sanitation related to education", subtitle = "From ESGData_SDG_ASIA dataset", y="Sanitationservices_Pop",x="Gross_SchoolEnrollment", caption = "ESG Demographics")

ggplotly(c)



#=========================================PLOT ALL YEAR SCATTER PLOT Q1=============================================================



install_github('plotly/dashR')
packageVersion("plotly")

library(devtools)
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)


app <- Dash$new()


countries <- unique(ESGDATA_SDG_ASIA_New$Country)

Agriculture_Poverty_2017 <- with(ESGDATA_SDG_ASIA_New,
                                 lapply(countries,
                                        function(cont) {
                                          list(
                                            x = Agriculture_GDP[Country == cont],
                                            y = Poverty_Population[Country == cont],
                                            opacity=0.7,
                                            text = Country[Country == cont],
                                            mode = 'markers',
                                            name = cont,
                                            marker = list(size = 10,
                                                          line = list(width = 0.5, color = 'white'))
                                          )
                                        }
                                 )
)
years <- ESGDATA_SDG_ASIA_New$Year

app$layout(htmlDiv(list(
  
  dccGraph( id = 'graph-with-slider',
            figure = list(data =  Agriculture_Poverty_2017,layout = list(xaxis = list('type' = 'linear', 'title' = 'Agriculture GDP'),
                                                                         yaxis = list('title' = 'Poverty Population'), margin = list('l' = 40, 'b' = 40, 't' = 10, 'r' = 10),legend = list('x' = 1, 'y' = 0),hovermode = 'closest'),
                          
                          dccSlider(
                            id = 'year-slider',
                            min = 0,
                            max = length(years) + 1,
                            marks = years,
                            value = 0
                          ))))))


app$run_server()
#============================================ PLOT ALL YEAR SCATTER Q2==============================================
NRD_CO2_2017 <- with(ESGDATA_SDG_ASIA_New,
                     lapply(countries,
                            function(cont) {
                              list(
                                x = Naturalresources_depletion[Country == cont],
                                y = CO2emissions_capita[Country == cont],
                                opacity=0.7,
                                text = Country[Country == cont],
                                mode = 'markers',
                                name = cont,
                                marker = list(size = 10,
                                              line = list(width = 0.5, color = 'white'))
                              )
                            }
                     )
)
years <- ESGDATA_SDG_ASIA_New$Year

app$layout(htmlDiv(list(
  
  dccGraph( id = 'graph-with-slider',
            figure = list(data =  NRD_CO2_2017,layout = list(xaxis = list('type' = 'linear', 'title' = 'Natural Resources Depletion (GNI)'),
                                                             yaxis = list('title' = 'CO2 emission metric ton per capita '), margin = list('l' = 40, 'b' = 40, 't' = 10, 'r' = 10),legend = list('x' = 1, 'y' = 0),hovermode = 'closest'),
                          
                          dccSlider(
                            id = 'year-slider',
                            min = 0,
                            max = length(years) + 1,
                            marks = years,
                            value = 0
                          ))))))


app$run_server()
#================================================== PLOT ALL YEAR SCATTER Q3==========================
sanitation_GSE_2017 <- with(ESGDATA_SDG_ASIA_New,
                            lapply(countries,
                                   function(cont) {
                                     list(
                                       x = Gross_SchoolEnrollment[Country == cont],
                                       y = Sanitationservices_Pop[Country == cont],
                                       opacity=0.7,
                                       text = Country[Country == cont],
                                       mode = 'markers',
                                       name = cont,
                                       marker = list(size = 10,
                                                     line = list(width = 0.5, color = 'white'))
                                     )
                                   }
                            )
)
years <- ESGDATA_SDG_ASIA_New$Year()

app$layout(htmlDiv(list(
  
  dccGraph( id = 'graph-with-slider',
            figure = list(data =  sanitation_GSE_2017,layout = list(xaxis = list('type' = 'linear', 'title' = 'Gross School Enrollment Ratio'),
                                                                    yaxis = list('title' = 'Sanitation Services (Population) '), margin = list('l' = 40, 'b' = 40, 't' = 10, 'r' = 10),legend = list('x' = 1, 'y' = 0),hovermode = 'closest'),
                          
                          dccSlider(
                            id = 'year-slider',
                            min = 0,
                            max = length(years) + 1,
                            marks = years,
                            value = 0
                          ))))))


app$run_server()
