rm(list=ls(all=TRUE))
start_time <- proc.time()

setwd("~/insofe/CuTE2/CSE7302c_CUTe01_Exam-Files/data")
#cust-owner data
system.time(cust_owner <- read.delim("stg_bgdt_cust_ownr.txt",header=T))#tab delim file
str(cust_owner)
library(lubridate)#column X_NOMIN_DT contains dates in mixed formats

cust_owner$X_NOMIN_DT <- as.Date(mdy_hm(cust_owner$X_NOMIN_DT))
#filter 1
cust_owner <-subset(cust_owner,X_EDW_INTEGRATION_ID == "kutti" | X_EDW_INTEGRATION_ID == "PAD2")
cust_owner <-subset(cust_owner,X_NOMIN_DT > "2012-12-24" & X_NOMIN_DT < "2013-01-27")
sum(is.na(cust_owner))
#--------------------------------------
game_actv <- read.csv("stg_bgdt_cust_gam_actv.csv",header = T)
nrow(game_actv) #1048575
str(game_actv)
game_actv$TITLE_NOMIN_DT <- dmy(game_actv$TITLE_NOMIN_DT)

#filter
game_actv <-subset(game_actv,TITLE_NOMIN_DT > "2011-12-25")
#min(game_actv$TITLE_NOMIN_DT) #sanity check


#--------------------------------------

system.time(game_cart <- read.delim("stg_bgdt_cust_gam_play_cart.txt",header=T))#tab delimited
dim(game_cart)
str(game_cart)
game_cart$TITLE_NOMIN_DT <- as.Date(dmy_hm(game_cart$TITLE_NOMIN_DT))
#filter
game_cart <- subset(game_cart,TITLE_NOMIN_DT > "2011-12-25" & TITLE_NOMIN_DT < "2013-04-11")

#--------------------------------------


system.time(purc_app <- read.delim("stg_bgdt_cust_purc_app.txt",header=T))#tab delimited
dim(purc_app)
str(purc_app)
purc_app$TRANSACTION_DATE <- as.Date(mdy_hm(purc_app$TRANSACTION_DATE))
#filter
purc_app <- subset(purc_app,TRANSACTION_DATE > "2011-12-25")

#--------------------------------

system.time(purc_lf <- read.delim("STG_BGDT_CUST_PURC_LF.txt",header=T))#tab delimited
dim(purc_lf)
str(purc_lf)
purc_lf$TRANSACTION_DT <- as.Date(dmy_hm(purc_lf$TRANSACTION_DT))
#filter
purc_lf <- subset(purc_lf,TRANSACTION_DT>"2011-12-25")
#-------------------------------
system.time(cust_child <- read.delim("stg_bgdt_cust_chld.txt",header=T))#tab delimited
dim(cust_child)
str(cust_child)
cust_child$CHILD_BIRTH_DATE <- as.Date(mdy_hm(cust_child$CHILD_BIRTH_DATE))
cust_child$CHILD_CREATION_DATE<- as.Date(mdy_hm(cust_child$CHILD_CREATION_DATE))

#--------------------------------
system.time(app_dwnld <- read.delim("stg_bgdt_cust_app_dwnld.txt",header=T))#tab delimited
dim(app_dwnld)
str(app_dwnld)
app_dwnld$NOMIN_DT <- as.Date(mdy_hm(app_dwnld$NOMIN_DT))
app_dwnld <- subset(app_dwnld,NOMIN_DT > "2011-12-25")
min(app_dwnld$NOMIN_DT)
#-------------

#loaded data from all the tables
#create the final data frame
#1. CONTACT_WID

#unique ids 56660
hopmonk_data <- data.frame(CONTACT_WID=unique(cust_owner$CONTACT_WID))
dim(hopmonk_data)
#2. NominationDate = 
library(dplyr)
nomin_dt <- cust_owner%>%group_by(CONTACT_WID)%>%summarise(NOMIN_DT=min(X_NOMIN_DT))%>%select(CONTACT_WID,NOMIN_DT)

hopmonk_data <- merge(hopmonk_data,nomin_dt,by="CONTACT_WID",all.x = TRUE)
dim(hopmonk_data)

#3.Country
country <- cust_owner%>%group_by(CONTACT_WID)%>%distinct(X_CNTRY)
hopmonk_data <- merge(hopmonk_data,country,by="CONTACT_WID",all.x = TRUE)
dim(hopmonk_data)

#4.MaxChildAge ; 5.MinChildAge ; 6. ChildAgeRange
  #compute child age
library(lubridate)
cust_child$CHILD_AGE <- as.duration(cust_child$CHILD_BIRTH_DATE %--% as.Date("2013-04-11"))/dyears(1)

cust_child$CHILD_AGE <- round(cust_child$CHILD_AGE,1)

length(cust_child$CONTACT_WID)
length(unique(cust_child$CONTACT_WID))
max_chld_age <- cust_child%>%group_by(CONTACT_WID)%>%summarise(MAX_CHLD_AGE=max(CHILD_AGE))
dim(max_chld_age)
min_chld_age <- cust_child%>%group_by(CONTACT_WID)%>%summarise(MIN_CHLD_AGE=min(CHILD_AGE))
dim(min_chld_age)
hopmonk_data<-merge(hopmonk_data,max_chld_age,by="CONTACT_WID",all.x = TRUE)
dim(hopmonk_data)
hopmonk_data<-merge(hopmonk_data,min_chld_age,by="CONTACT_WID",all.x = TRUE)
dim(hopmonk_data)


hopmonk_data$ChildAgeRange <-hopmonk_data$MAX_CHLD_AGE - hopmonk_data$MIN_CHLD_AGE
sum(is.na(hopmonk_data))

#-------------------------
#7. Units
length(purc_app$CONTACT_WID);length(unique(purc_app$CONTACT_WID))
length(purc_lf$CONTACT_WID);length(unique(purc_lf$CONTACT_WID))

purc_app_sum_units <- purc_app%>%group_by(CONTACT_WID)%>%summarise(purc_app_sum_units=sum(UNITS))

#hopmonk_data <- merge(hopmonk_data,purc_app_sum_units,by="CONTACT_WID",all.x = TRUE)

purc_lf_sum_units <- purc_lf%>%group_by(CONTACT_WID)%>%summarise(purc_lf_sum_units=sum(UNITS))


tot_units <- merge(purc_app_sum_units,purc_lf_sum_units,by="CONTACT_WID",all.x = TRUE)

#NAs to zero before sum #update helper
tot_units$purc_app_sum_units[is.na(tot_units$purc_app_sum_units)] <- 0
tot_units$purc_lf_sum_units[is.na(tot_units$purc_lf_sum_units)] <- 0

tot_units$UNITS <- tot_units$purc_app_sum_units + tot_units$purc_lf_sum_units

tot_units <- subset(tot_units,select=-c(purc_app_sum_units,purc_lf_sum_units))
#hopmonk_data <- merge(hopmonk_data,purc_lf_sum_units,by="CONTACT_WID",all.x = TRUE)
hopmonk_data <- merge(hopmonk_data,tot_units,by="CONTACT_WID",all.x = TRUE)
colnames(hopmonk_data)
#ok <- complete.cases(hopmonk_data) ;sum(ok) ;head(hopmonk_data[ok,])
#????sum(ok)/(sum(ok)+sum(!ok)) ~ 3.2%


#8.OveralllastTransaction ; TENURE_DAYS
ovr_lst_trans <-purc_lf%>%group_by(CONTACT_WID)%>%summarise(OVR_LST_TRANS=max(TRANSACTION_DT))
dim(ovr_lst_trans)
hopmonk_data<-merge(hopmonk_data,ovr_lst_trans,by="CONTACT_WID",all.x = TRUE)
#sum(ok)/(sum(ok)+sum(!ok)) ~ 3.2%
hopmonk_data$TENURE_DAYS <- as.numeric(difftime(hopmonk_data$OVR_LST_TRANS,hopmonk_data$NOMIN_DT,units = "days"))
#????inconsistent data NOMDT>LASTTRANS

#9. FrequencyLF ; FrequencyApp
#dim(purc_lf[!(complete.cases(purc_lf[,"TRANSACTION_DT"])),])
freq_lf <- purc_lf%>%group_by(CONTACT_WID)%>%summarise(FrequencyLF=n())
#zero for missing values
hopmonk_data <- merge(hopmonk_data,freq_lf,by="CONTACT_WID",all.x = TRUE)
hopmonk_data["FrequencyLF"][is.na(hopmonk_data["FrequencyLF"])] <- 0#update time_2

#head(hopmonk_data)

freq_app <- purc_app%>%group_by(CONTACT_WID)%>%summarise(FrequencyApp=n())
hopmonk_data <- merge(hopmonk_data,freq_app,by="CONTACT_WID",all.x = TRUE)
#zero for missing values
hopmonk_data["FrequencyApp"][is.na(hopmonk_data["FrequencyApp"])] <- 0#update time_2
dim(freq_app)

#11. freq_game_play
str(game_actv)
freq_game_play <- game_actv%>%group_by(CONTACT_WID)%>%summarise(freq_game_play=sum(ATMP_CNT))
hopmonk_data <- merge(hopmonk_data,freq_game_play,by="CONTACT_WID",all.x = TRUE)


#12. Recency app
recency_app <- purc_app%>%group_by(CONTACT_WID)%>%summarise(recency_app=max(TRANSACTION_DATE))
recency_app$recency_app <- as.numeric(difftime(as.Date("2013-04-11"),recency_app$recency_app,units = "days"))
hopmonk_data <- merge(hopmonk_data,recency_app,by="CONTACT_WID",all.x = TRUE)


#12. Recency LF
recency_lf <- purc_lf%>%group_by(CONTACT_WID)%>%summarise(recency_lf=max(TRANSACTION_DT))
recency_lf$recency_lf <- as.numeric(difftime(as.Date("2013-04-11"),recency_lf$recency_lf,units = "days"))
hopmonk_data <- merge(hopmonk_data,recency_lf,by="CONTACT_WID",all.x = TRUE)
dim(recency_lf)

#13.Recency Down

str(app_dwnld)
recency_dwnld <- app_dwnld%>%group_by(CONTACT_WID)%>%summarise(recency_dwnld=max(NOMIN_DT))
recency_dwnld$recency_dwnld <- as.numeric(difftime(as.Date("2013-04-11"),recency_dwnld$recency_dwnld,units = "days"))

hopmonk_data <- merge(hopmonk_data,recency_dwnld,by="CONTACT_WID",all.x = TRUE)


#14.maxRecencyCum,minRecencyCum
hopmonk_data$maxRecencyCum <- max(hopmonk_data$recency_app,hopmonk_data$recency_lf,hopmonk_data$recency_dwnld)

hopmonk_data$minRecencyCum <- min(hopmonk_data$recency_app,hopmonk_data$recency_lf,hopmonk_data$recency_dwnld)

#15. NumHouseChildren
num_housechld <- cust_child[complete.cases(cust_child$CRM_CHLD_KEY.),]%>%group_by(CONTACT_WID)%>%summarise(num_house_chld=n())

hopmonk_data <- merge(hopmonk_data,num_housechld,by="CONTACT_WID",all.x = TRUE)


#16. TotalTimeGamePlay
tot_game_play <- game_actv%>%group_by(CONTACT_WID)%>%summarise(tot_game_play=sum(ACT_TME_SPN_QTY))

hopmonk_data <- merge(hopmonk_data,tot_game_play,by="CONTACT_WID",all.x = TRUE)

dim(hopmonk_data)
#17. NumFemaleChildrenHousehold, 18. NumMaleChildrenHousehold
str(cust_child)
num_female_child <- cust_child[complete.cases(cust_child$SEX_MF_CODE),]%>%filter(SEX_MF_CODE=="F")%>%group_by(CONTACT_WID)%>%summarise(num_female_child=n())

hopmonk_data <- merge(hopmonk_data,num_female_child,by="CONTACT_WID",all.x = TRUE)


num_male_child <- cust_child[complete.cases(cust_child$SEX_MF_CODE),]%>%filter(SEX_MF_CODE=="M")%>%group_by(CONTACT_WID)%>%summarise(num_male_child=n())

hopmonk_data <- merge(hopmonk_data,num_male_child,by="CONTACT_WID",all.x = TRUE)



dim(hopmonk_data)
#19. NumGamesPlayed  20. NumGamesBought

num_game_plyd <- game_actv[complete.cases(game_actv$X_GAME_NM),]%>%group_by(CONTACT_WID)%>%summarise(num_game_plyd=n())
hopmonk_data <- merge(hopmonk_data,num_game_plyd,by="CONTACT_WID",all.x = TRUE)
dim(hopmonk_data)


num_game_bght <- purc_app[complete.cases(purc_app$ITEM_NUMBER),]%>%group_by(CONTACT_WID)%>%summarise(num_game_bght=n())
hopmonk_data <- merge(hopmonk_data,num_game_bght,by="CONTACT_WID",all.x = TRUE)


dim(hopmonk_data)
sum(complete.cases(hopmonk_data))

hopmonk_data_bkup<-hopmonk_data
dim(hopmonk_data_bkup)

#20. Fav game bin
tot_game_time <- game_actv%>% group_by(CONTACT_WID,X_GAME_NM) %>% summarise(tot_time=sum(ACT_TME_SPN_QTY
))%>%group_by(CONTACT_WID)%>%mutate(fav=ifelse(tot_time==max(tot_time),"fav","not"))
fav_gam<-subset(tot_game_time,fav=="fav",select = c(CONTACT_WID,X_GAME_NM))
hopmonk_data <- merge(hopmonk_data,fav_gam,by="CONTACT_WID",all.x = TRUE)
dim(hopmonk_data)

#21. Favorite Source
sum(complete.cases(hopmonk_data$FrequencyLF))
a<-hopmonk_data[,c("CONTACT_WID","FrequencyLF","FrequencyApp")]
a$fav_source <- ifelse(a$FrequencyApp>a$FrequencyLF,"App","LF")
a[is.na(a)] <- 0 #update time2

#a[complete.cases(a),]
a<-subset(a,select = c(CONTACT_WID,fav_source))
hopmonk_data<-merge(hopmonk_data,a,by="CONTACT_WID",all.x = TRUE)


dim(hopmonk_data)

#22.FavoriteChannel 
summary <- purc_app %>% group_by(CONTACT_WID,CHANNEL_DESCRIPTION) %>% summarise(cnt=n())%>%mutate(freq=cnt/sum(cnt))

a <- summary%>%group_by(CONTACT_WID)%>%mutate(fav=ifelse(freq==max(freq),"fav","not"))

a <-subset(a,fav=="fav",select = c(CONTACT_WID,CHANNEL_DESCRIPTION))
#rows with equal frfeq cause duplicates
a<-a%>%distinct(CONTACT_WID,.keep_all = TRUE)
hopmonk_data <- merge(hopmonk_data,a,by="CONTACT_WID",all.x = TRUE)
dim(hopmonk_data)

#TotalRevenueGenerated
sum_app <- purc_app%>%group_by(CONTACT_WID)%>%summarise(sum_app=sum(AMOUNT_USD))
sum_lf <- purc_lf%>%group_by(CONTACT_WID)%>%summarise(sum_lf=sum(AMOUNT))
sum_app_lf = merge(sum_app,sum_lf,by="CONTACT_WID")
#set NAs to 0 #update helper
sum_app_lf[is.na(sum_app_lf)]<-0

sum_app_lf$TOT_REV_GEN <- sum_app_lf$sum_app+sum_app_lf$sum_lf
sum_app_lf = subset(sum_app_lf,select = c(CONTACT_WID,TOT_REV_GEN))
hopmonk_data <- merge(hopmonk_data,sum_app_lf,by="CONTACT_WID",all.x = TRUE)

dim(hopmonk_data)#[1] 56660    26

end_time <- proc.time()
end_time-start_time

#----------------------
#% of NAs in each column
perc_na <- data.frame(perc = colMeans(is.na(hopmonk_data)))
perc_na$perc <- perc_na$perc*100
perc_na <-  data.frame(cbind(row.names(perc_na),perc_na$perc))
perc_na <- perc_na[order(perc_na$X2),]

library(ggplot2)
ggplot(perc_na)+geom_col(mapping = aes(x=X1,y=X2)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#set NAs in numeric columns to zero
str(hopmonk_data)
factor_colnames<-c("CONTACT_WID","NOMIN_DT","OVR_LST_TRANS","X_GAME_NM","fav_source","CHANNEL_DESCRIPTION")
hopmonk_data$CONTACT_WID <- as.factor(hopmonk_data$CONTACT_WID)
hopmonk_data$NOMIN_DT <- as.factor(hopmonk_data$NOMIN_DT)
hopmonk_data$OVR_LST_TRANS <- as.factor(hopmonk_data$OVR_LST_TRANS)
hopmonk_data$X_GAME_NM <- as.factor(hopmonk_data$X_GAME_NM)
hopmonk_data$fav_source <- as.factor(hopmonk_data$fav_source)
str(hopmonk_data)

#extract numeric
library(purrr)
numeric_colnames<-colnames(hopmonk_data%>%keep(is.numeric))
time_columns<-c("MAX_CHLD_AGE","MIN_CHLD_AGE","ChildAgeRange","TENURE_DAYS","recency_app","recency_lf","recency_dwnld","maxRecencyCum","minRecencyCum")

#semantically a zero for numeric col like count would mean there are no such transactions
#but fo durations like recency it could maean the event of interest occured on the start point

cols_replace_na <- setdiff(numeric_colnames,time_columns)

sum(is.na(hopmonk_data[,cols_replace_na])) #470470
hopmonk_data[cols_replace_na][is.na(hopmonk_data[cols_replace_na])] <- 0
#tenure days contqins -ve values as nomin_dt is recent than last trans date set them to NA
hopmonk_data$TENURE_DAYS <- ifelse(hopmonk_data$TENURE_DAYS<0,NA,hopmonk_data$TENURE_DAYS)
summary(hopmonk_data)

#--------------------------
#extract numeric
library(purrr)
numeric_colnames<-colnames(hopmonk_data_2%>%keep(is.numeric))
time_columns<-c("MAX_CHLD_AGE","MIN_CHLD_AGE","ChildAgeRange","TENURE_DAYS","recency_app","recency_lf","recency_dwnld","maxRecencyCum","minRecencyCum")

#semantically a zero for numeric col like count would mean there are no such transactions
#but fo durations like recency it could maean the event of interest occured on the start point

cols_replace_na <- setdiff(numeric_colnames,time_columns)

sum(is.na(hopmonk_data[,cols_replace_na])) #470470
hopmonk_data[cols_replace_na][is.na(hopmonk_data[cols_replace_na])] <- 0
#tenure days contqins -ve values as nomin_dt is recent than last trans date set them to NA
hopmonk_data$TENURE_DAYS <- ifelse(hopmonk_data$TENURE_DAYS<0,NA,hopmonk_data$TENURE_DAYS)
summary(hopmonk_data)
#----------------------
  #% of NAs in each column
perc_na <- data.frame(perc = colMeans(is.na(hopmonk_data)))
perc_na$perc <- perc_na$perc*100
perc_na <-  data.frame(cbind(row.names(perc_na),perc_na$perc))
perc_na <- perc_na[order(perc_na$X2),]
colnames(perc_na)<-c("variable","perc_of_NAs")

library(ggplot2)
ggplot(perc_na)+geom_col(mapping = aes(x=variable,y=perc_of_NAs)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#---------correlation matrix
library(caret)
library(corrplot)

#remove high NA cols using perc_na
high_NAs <- c("maxRecencyCum", "minRecencyCum", "TENURE_DAYS","recency_lf","OVR_LST_TRANS") 
cor_cols <- setdiff(numeric_colnames,high_NAs)
for_cor <- subset(hopmonk_data,select = cor_cols)
correlationMatrix <- cor(for_cor,use ="complete.obs" )
print(correlationMatrix)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)#recheck
print(highlyCorrelated)

corrplot(correlationMatrix, method="circle")

colnames(hopmonk_data[highlyCorrelated])

