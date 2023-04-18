library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)


urbana_part01 <- read_csv("C:/Users/Ivan/Desktop/Stat448Prezi/data_urbana_policing-main/urbana-police-incidents-data-part01.csv")
urbana_part02 <- read_csv("C:/Users/Ivan/Desktop/Stat448Prezi/data_urbana_policing-main/urbana-police-incidents-data-part02.csv")
urbana_part03 <- read_csv("C:/Users/Ivan/Desktop/Stat448Prezi/data_urbana_policing-main/urbana-police-incidents-data-part03.csv")
urbana_part04 <- read_csv("C:/Users/Ivan/Desktop/Stat448Prezi/data_urbana_policing-main/urbana-police-incidents-data-part04.csv")
urbana_part05 <- read_csv("C:/Users/Ivan/Desktop/Stat448Prezi/data_urbana_policing-main/urbana-police-incidents-data-part05.csv")
urbana_part06 <- read_csv("C:/Users/Ivan/Desktop/Stat448Prezi/data_urbana_policing-main/urbana-police-incidents-data-part06.csv")

urbana = rbind(urbana_part01,urbana_part02,urbana_part03,urbana_part04,urbana_part05,urbana_part06) %>%
  filter_at(vars(`MAPPING ADDRESS`),all_vars(!is.na(.))) %>%
  filter(`YEAR OCCURRED` > 1988)%>%
  distinct(INCIDENT, .keep_all = TRUE)%>%
  select(-c(`DATE REPORTED`,`DATE ARRIVED`,
            STREET,`CROSS STREET`,`CRIME CODE`,`CRIME CATEGORY`,`BIAS CODE`,
            `PLACE CODE`,`STATUS DESCRIPTION`,`BIAS DESCRIPTION`, `CRIME DESCRIPTION`,
            `STATUS CODE`,`WEAPONS CODE 2`,`WEAPON 2 DESCRIPTION`,
            `WEAPONS CODE 3`,`WEAPON 3 DESCRIPTION`,`CSA DESCRIPTION`,
            `GEO CODE`,`HOUSE NUMBER BLOCK`,COUNTER)) %>%
  mutate(`AREA` = case_when(
    `PLACE CODE DESCRIPTION` %in% c("APARTMENT","RESIDENCE-PRIVATE","RESIDENCE-PORCH","DRIVEWAY-RESIDENTIAL",
                                    "RESIDENCE-YARD") ~ 'Private',
    !(`PLACE CODE DESCRIPTION` %in% c("APARTMENT","RESIDENCE-PRIVATE","RESIDENCE-PORCH","DRIVEWAY-RESIDENTIAL",
                                      "RESIDENCE-YARD")) ~ 'Public'
  )) 
View(urbana)

#Campus coordinate boundaries
# 40.115542, -88.242878 (upper left corner)
# 40.100755, -88.219140 (lower right corner)

longitude = as.numeric(str_extract_all(urbana$`MAPPING ADDRESS`, "\\d+\\.\\d+(?=\\,)")) 
longitude = longitude[!is.na(longitude)]

latitude = as.numeric(str_extract_all(urbana$`MAPPING ADDRESS`, "\\s\\-\\d+\\.\\d+"))
latitude = latitude[!is.na(latitude)]

#make dataset containing crimes during major holidays
library('lubridate')

#Biggest Source: https://alcohol.org/statistics-information/holiday-binge-drinking/

#New Year's Eve 12/31 and 1/1
#St. Patrick's Day 3/17
#Fourth of July 7/4 and 7/5
#Halloween 10/31 and 11/1
#Cinco De Mayo 5/5
#Christmas 12/25 and 12/24

#Top 3 crimes according to 

holiday = urbana %>%
  mutate(`DATE OCCURRED` = format(ymd(as.Date(`DATE OCCURRED`,"%m/%d/%Y")),"%m/%d"), EVENT = case_when(
    `DATE OCCURRED`  %in% c("12/24","12/25","12/29","12/30","12/31","01/01") ~ "Christmas/NewYears",
    `DATE OCCURRED` == "03/17" ~ "St.Patricks",
    `DATE OCCURRED` == "07/04"|`DATE OCCURRED` == "07/05" ~ "July4th",
    `DATE OCCURRED` == "10/31"|`DATE OCCURRED` == "11/01" ~ "Halloween",
    `DATE OCCURRED` == "05/05" ~ "CincoDeMayo",
    `DATE OCCURRED` >= "11/22" & `DATE OCCURRED` <= "11/28" ~ "ThanksgivingWeek"
  ))%>%
  filter(!is.na(EVENT))

holiday %>%
  group_by(EVENT)%>%
  count(EVENT)%>%
  mutate(Popularity = case_when(
    EVENT %in% c("Halloween", "St.Patricks","CincoDeMayo","July4th") ~ "'Minor' Holiday",
    EVENT %in% c("ThanksgivingWeek", "Christmas/NewYears") ~ "'Major' Holiday"
  ))%>%
  ggplot(aes(x= reorder(EVENT,-n), y = n, fill = Popularity)) + 
  geom_bar(stat = "identity")+
  ggtitle("Number of Crimes during Holiday Periods in Urbana From 1988-2022") +
  labs(x = NULL, y = "Frequency of Crime") +
  scale_fill_manual(values = c('darkblue','goldenrod2'))+
  theme_minimal()+
  theme(legend.title = element_text(size = 15),legend.text = element_text(size = 15),axis.title = element_text(size = 20),axis.text = element_text(size = 16), plot.title = element_text(size = 30))

holiday %>%
  group_by(EVENT)%>%
  count(EVENT)%>%
  mutate(n = case_when(
    EVENT == "Christmas/NewYears" ~ n/6,
    EVENT == "ThanksgivingWeek" ~ n/7,
    EVENT == "Halloween" ~ n/2,
    EVENT == "July4th" ~ n/2, 
    EVENT == "CincoDeMayo" ~ n/1,
    EVENT == "St.Patricks" ~ n/1
  ))%>%
  mutate(Popularity = case_when(
    EVENT %in% c("Halloween", "St.Patricks","CincoDeMayo","July4th") ~ "'Minor' Holiday",
    EVENT %in% c("ThanksgivingWeek", "Christmas/NewYears") ~ "'Major' Holiday"
  ))%>%
  ggplot(aes(x= reorder(EVENT,-n), y = n, fill = Popularity)) + 
  geom_bar(stat = "identity")+
  ggtitle("Daily Number of Crimes during Holiday Periods in Urbana From 1988-2022") +
  labs(x = NULL, y = "Frequency of Crime") +
  scale_fill_manual(values = c('darkblue','goldenrod2'))+
  theme_minimal()+
  theme(legend.title = element_text(size = 15),legend.text = element_text(size = 15),axis.title = element_text(size = 20),axis.text = element_text(size = 16), plot.title = element_text(size = 30))

holiday %>%
  group_by(EVENT)%>%
  filter(`CRIME CATEGORY DESCRIPTION` %in% c("Driving Under the Influence","Liquor Offenses","Controlled Substance Offenses","Methamphetamine Offenses","Cannabis Offenses"))%>%
  summarise(count = n()/length(unique(`DATE OCCURRED`)))%>%
  ggplot(aes(x= reorder(`EVENT`, -count), y= count)) + 
  geom_bar(stat = "identity", color = 'gold', fill = 'goldenrod2')+
  ggtitle("Number of Substance-related Crimes during Holidays in Urbana from 1988-2022") +
  labs(x = NULL, y = "Frequency of Crime") +
  theme_minimal()+
  theme(axis.title = element_text(size = 20),axis.text = element_text(size = 20), plot.title = element_text(size = 30))

influenceholiday = holiday %>%
  count(`CRIME CATEGORY DESCRIPTION`) %>%
  filter(`CRIME CATEGORY DESCRIPTION` %in% c("Driving Under the Influence","Liquor Offenses","Controlled Substance Offenses","Methamphetamine Offenses","Cannabis Offenses"))
  summarise(`CRIME CATEGORY DESCRIPTION`,perc = n/nrow(holiday))

influenceurbana = urbana %>%
  filter(!(urbana$INCIDENT %in% holiday$INCIDENT))%>%
  count(`CRIME CATEGORY DESCRIPTION`)%>%
  filter(`CRIME CATEGORY DESCRIPTION` %in% c("Driving Under the Influence","Liquor Offenses","Controlled Substance Offenses","Methamphetamine Offenses","Cannabis Offenses"))
  summarise(`CRIME CATEGORY DESCRIPTION`,perc = n/nrow(urbana))
  

#testing if there is significant difference between driving under the influence crimes during holidays vs not
prop.test(x = c(sum(influenceholiday$n),sum(influenceurbana$n)), n = c(nrow(holiday), nrow(urbana)), alternative = 'greater')

#create best model for predicting response time
holiday = holiday %>%
  mutate(`RESPONSE TIME`= as.numeric(difftime(`TIME ARRIVED`,`TIME REPORTED`,units = 'mins')))


summary(glm(`RESPONSE TIME`~ as.factor(EVENT) + as.factor(AREA) + `MONTH OCCURRED` ,data = holiday))
#month occurred is insignificant
summary(glm(`RESPONSE TIME`~ as.factor(EVENT) + as.factor(AREA) ,data = holiday))

holiday %>%
  group_by(EVENT)%>%
  count(AREA) %>%
  arrange(desc(n))%>%
  summarise(AREA = AREA, percentage = n/sum(n))%>%
  ggplot(aes(fill = AREA, x = EVENT, y = percentage))+
  geom_bar(position = 'fill', stat = 'identity')

urbanatheft = urbana %>%
  filter(!(urbana$INCIDENT %in% holiday$INCIDENT))%>%
  filter(`CRIME CATEGORY DESCRIPTION` %in% c('Burglary from Motor Vehicle', 
                                             "Theft","Robbery","Burglary",
                                             "Motor Vehice Theft"))%>%
  group_by(`YEAR OCCURRED`)%>%
  mutate(count = n())

holiday %>%
  filter(`CRIME CATEGORY DESCRIPTION` %in% c('Burglary from Motor Vehicle', 
                                             "Theft","Robbery","Burglary",
                                             "Motor Vehice Theft"))%>%
  group_by(`YEAR OCCURRED`)%>%
  mutate(count = n())%>%
  ggplot() +
  geom_line(aes( x = `YEAR OCCURRED`, y = count),stat = 'identity', size = 2.5, color = 'goldenrod2')+
  geom_line(data = urbanatheft, stat = 'identity', aes(x = `YEAR OCCURRED`, y = count/18), size = 2, color = '#171438ff')+
  theme_minimal() +
  labs(x = "Year Occurred", y = "Theft Frequency",title = "Frequency of Theft in Urbana During Drinking Holidays from 1988-2022")+
  theme(axis.title = element_text(size = 20),axis.text = element_text(size = 20), plot.title = element_text(size = 30))

theftholiday = holiday %>%
  count(`CRIME CATEGORY DESCRIPTION`) %>%
  filter(`CRIME CATEGORY DESCRIPTION` %in% c('Burglary from Motor Vehicle', 
                                             "Theft","Robbery","Burglary",
                                             "Motor Vehice Theft")) 
holiday %>%
  group_by(EVENT)%>%
  count(`CRIME CATEGORY DESCRIPTION`)%>%
  arrange(desc(n)) %>%
  filter((`CRIME CATEGORY DESCRIPTION` %in% c('Theft', 'Battery','Domestic Dispute','Criminal Damage','Burglary')))%>%
  ggplot(aes(fill=`CRIME CATEGORY DESCRIPTION`, y=n, x= reorder(`CRIME CATEGORY DESCRIPTION`,-n))) + 
  geom_bar(position="dodge", stat="identity", color = 'gold', fill = 'goldenrod2') +
  ggtitle("Crime Category Rankings for each Holiday Period in Urbana") +
  facet_wrap(~EVENT) +
  theme_minimal()+
  theme(legend.position="none",axis.title = element_text(size = 10), strip.text = element_text(size = 15),axis.text = element_text(size = 7), plot.title = element_text(size = 20, hjust = .5)) +
  xlab("")+
  ylab("Frequency of Crime")

holiday %>%
  filter(`CRIME CATEGORY DESCRIPTION` %in% c('Assault','Domestic Dispute'))%>%
  filter(`RESPONSE TIME` > 0 & `RESPONSE TIME` < 100)%>%
  group_by(`RESPONSE TIME`)%>%
  mutate(count = n())%>%
  ggplot(aes(x = `RESPONSE TIME`, y = count))+
  geom_line(stat = 'identity', size = 1.5, color = 'goldenrod2')+
  geom_line(data = holidayresponse, aes(x = `RESPONSE TIME`, y = count), size = 1, color = '#171438ff')+
  labs(x = "Minutes until Police Action", y = 'Frequency', title = 'Minutes Until Response for Crimes in Urbana during Holidays from 1988-2022')+
  theme(axis.title = element_text(size = 20),axis.text = element_text(size = 20), plot.title = element_text(size = 30)) +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )


holidayresponse = holiday %>%
  filter(`CRIME CATEGORY DESCRIPTION` %in% c('Battery','Domestic - Other Offenses'))%>%
  filter(`RESPONSE TIME` > 0)%>%
  group_by(`RESPONSE TIME`)%>%
  mutate(count = n())

thefturbana = urbana %>%
  filter(!(urbana$INCIDENT %in% holiday$INCIDENT))%>%
  count(`CRIME CATEGORY DESCRIPTION`) %>%
  filter(`CRIME CATEGORY DESCRIPTION` %in% c('Burglary from Motor Vehicle', 
                                             "Theft","Robbery","Burglary",
                                             "Motor Vehice Theft"))

prop.test(x = c(sum(theftholiday$n),sum(thefturbana$n)), n = c(nrow(holiday), nrow(urbana)), alternative = 'greater')


assaultholiday = holiday %>%
  count(`CRIME CATEGORY DESCRIPTION`) %>%
  filter(`CRIME CATEGORY DESCRIPTION` %in% c('Assault','Domestic Dispute'))
assaulturbana = urbana %>%
  filter(!(urbana$INCIDENT %in% holiday$INCIDENT))%>%
  count(`CRIME CATEGORY DESCRIPTION`) %>%
  filter(`CRIME CATEGORY DESCRIPTION` %in% c('Assault','Domestic Dispute'))
prop.test(x = c(sum(assaultholiday$n),sum(assaulturbana$n)), n = c(nrow(holiday), nrow(urbana)), alternative = 'greater')


batteryholiday = holiday %>%
  count(`CRIME CATEGORY DESCRIPTION`) %>%
  filter(`CRIME CATEGORY DESCRIPTION` %in% c('Battery','Domestic - Other Offenses'))
batteryurbana = urbana %>%
  filter(!(urbana$INCIDENT %in% holiday$INCIDENT))%>%
  count(`CRIME CATEGORY DESCRIPTION`) %>%
  filter(`CRIME CATEGORY DESCRIPTION` %in% c('Battery','Domestic - Other Offenses'))
prop.test(x = c(sum(batteryholiday$n),sum(batteryurbana$n)), n = c(nrow(holiday), nrow(urbana)), alternative = 'greater')


holiday %>%
  group_by(EVENT)%>%
  count(`CRIME CATEGORY DESCRIPTION`)%>%
  arrange(desc(n)) %>%
  filter(!(`CRIME CATEGORY DESCRIPTION` %in% c('Accident', 'Traffic Offenses')))%>%
  slice(1:6) 



