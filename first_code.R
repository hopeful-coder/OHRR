#This is a second upload into Github from R

#Load data in
library(haven)
library(kableExtra)
library(plyr)
library(readr)
library(dplyr)
OHRR_All_Flattened <- read_sav("C:/Users/Mitchell Schepps/Desktop/nena/OHRR_All_Flattened.sav")
# OHRR_All_Flattened2 <- read_sav("C:/Users/Mitchell Schepps/Desktop/nena/OHRR_All_Flattened052919.sav")
group              <- read.csv("C:/Users/Mitchell Schepps/Desktop/nena/group.csv")
labels             <- read.csv("C:/Users/Mitchell Schepps/Desktop/nena/labels.csv", header = F)

list1              <- read.csv("C:/Users/Mitchell Schepps/Desktop/nena/list1.csv")
list2              <- read.csv("C:/Users/Mitchell Schepps/Desktop/nena/list2.csv")
list3              <- read.csv("C:/Users/Mitchell Schepps/Desktop/nena/list3.csv")
list4              <- read.csv("C:/Users/Mitchell Schepps/Desktop/nena/list4.csv")
list5              <- read.csv("C:/Users/Mitchell Schepps/Desktop/nena/list5.csv")
recid <- read_sav("C:/Users/Mitchell Schepps/Desktop/nena/recid_data.sav")
recid %>%
  mutate_all(as.character)
recid <- data.frame(recid)
names(recid)[8:ncol(recid)] <- recid[1, c(8:ncol(recid))]
recid <- recid[6:nrow(recid),]
recid$Gender <- tolower(recid$Gender)


full_id = read.csv("C:/Users/Mitchell Schepps/Desktop/nena/full_id.csv")
names(full_id)[5] = 'UCLACLIENTID'
names(full_id)[1] = 'BookingNumber'
full_id = full_id[6:nrow(full_id), ]
recid = recid[order(recid$BookingNumber), ]
full_id = full_id[order(full_id$BookingNumber), ]

recid$UCLACLIENTID = full_id$UCLACLIENTID

table(recid$Gender, recid$GROUPASSIGNED)
withdrawal <- c(2024, 2029, 2034, 2040, 2045, 1058)
withd <- which(recid$UCLACLIENTID %in% withdrawal)
recid <- recid[-withd,]

names(labels)[1] <- 'Variable'
names(labels)[5] <- 'Name'

names(group)[1] <- 'ID1'
data <- merge(OHRR_All_Flattened, group, by = 'ID1')

withdrawal <- c(2024, 2029, 2034, 2040, 2045, 1058)

data <- data[!data$ID1 %in% withdrawal,]
data$age <- 2019 - data$QDS_BL_DEM1Y

recid = recid[order(recid$UCLACLIENTID), ]
data = data[order(data$ID1), ]

recid$GROUPASSIGNED == data$GROUP


#Follow-up Indicator
data$follow_up <- ifelse(is.na(data$QDS_FU_DEM3), 0, 1)
recid = recid[order(recid$UCLACLIENTID), ]
data = data[order(data$ID1), ]
recid$follow_up = data$follow_up

#
#Addendum to Nena's research
#Mental Health
#MHT1.	receive a diagnosis/assessment of mental illness?
#MHT2.	receive inpatient mental health treatment?
#MHT3.	receive individual counseling?
#MHT4.	receive group counseling?
#MHT5.	obtain prescription meds for psychological problems?
#MHT6.	receive education and management on how to use your meds?
#MHT7.	receive help with managing your symptoms?
#MD1.	receive medical/dental insurance?
#MD2.	receive a medical exam by a doctor, nurse, or physician assistant?
#MD3.	receive prescription medications for health problems?
#MD4.	receive dental care (i.e., exam, cleaning, filling, crown)?
#MD5.	receive HIV/AIDS prevention and education?
#MD6.	receive Hepatitis C testing/education/treatment?
#MD7.	receive an exam exam for eyeglasses?

mht = data.frame(variable = c(paste0('MHT', 1:7)),
                 name     = c('receive a diagnosis/assessment of mental illness?',
                              'receive inpatient mental health treatment?',
                              'receive individual counseling?',
                              'receive group counseling?',
                              'obtain prescription meds for psychological problems?',
                              'receive education and management on how to use your meds?',
                              'receive help with managing your symptoms?'
                              ))
attachment_large = data.frame('Control' = c(),
                              'HealthN' = c(),
                              'Overall' = c())
for(i in 1:7){
  #Variable Name
  var1 = paste0('QDS_FU_MHT', i)
  var2 = paste0(var1, 'A')
  
  #Extract variables
  a = table(data[[var1]], data$GROUP, useNA ='always')[2]
  b = table(data[[var1]], data$GROUP, useNA = 'always')[4]
  c = table(data[[var2]], data$GROUP, useNA = 'always')[2]
  d = table(data[[var2]], data$GROUP, useNA = 'always')[4]
  
  attachment = data.frame('Control' = c(paste0(a, '/', a+b, ' (', round(a/(a+b) * 100, 0), '%)')),
                          'HealthN' = c(paste0(c, '/', c+d, ' (', round(c/(c+d) * 100, 0), '%)')),
                          'Overall' = c(paste0(a + c, '/', a + b + c + d, ' (', round((a +c)/(a+b+c+d) * 100, 0), '%)')))
  rownames(attachment) = mht$name[i]
  attachment_large = rbind(attachment_large, attachment)
}

