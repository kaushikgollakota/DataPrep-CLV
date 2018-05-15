########################################################################################
############################## CUTe01- 20170129- CSE7302c #######################################
###################### Statistics and Probability in Decision Modelling ################
#########################################################################################
############################################### Team : Abhilash Reddy, Kaushik, Krishna, Ambrash#####

# Set the working directory
setwd("E:\\Cute2\\CSE7302c_CUTe01_Exam-Files\\data")

# Read all the files into the Global environemnet

# This gives the complete info of the customer who got registered on to the platform.
cust_info=read.csv("stg_bgdt_cust_ownr.csv",na.strings = NA)
cust_info$CONTACT_WID=as.factor(cust_info$CONTACT_WID)
summary(cust_info)

# This gives the info of thr customers who had downloaded the app.
app_down=read.csv("stg_bgdt_cust_app_dwnld.csv",na.strings = NA)
app_down$CONTACT_WID=as.factor(app_down$CONTACT_WID)
summary(app_down)

# Info regarding the games they played.
games_played=read.csv("stg_bgdt_cust_gam_actv.csv",na.strings = NA)
games_played$CONTACT_WID=as.factor(games_played$CONTACT_WID)
summary(games_played)

# Info regarding the games added to the cart.
games_added=read.csv("stg_bgdt_cust_gam_play_cart.csv",na.strings = NA)
games_added$CONTACT_WID=as.factor(games_added$CONTACT_WID)
summary(games_added)

# Info regarding the purchase through app
purc_app=read.csv("stg_bgdt_cust_purc_app.csv",na.strings = NA)
purc_app$CONTACT_WID=as.factor(purc_app$CONTACT_WID)
summary(purc_app)

# Info regarding the purchase through app lf
purc_lf=read.csv("STG_BGDT_CUST_PURC_LF.csv",na.strings = NA)
purc_lf$CONTACT_WID=as.factor(purc_lf$CONTACT_WID)
summary(purc_lf)

# Ino of the childrens
child_info=read.csv("stg_bgdt_cust_chld.csv",na.strings = NA)
child_info$CONTACT_WID=as.factor(child_info$CONTACT_WID)
########################## Customer Info work is been done #############################
library(lubridate)
cust_info$X_NOMIN_DT=as.Date(parse_date_time(x = cust_info$X_NOMIN_DT,orders = c("mdY HM"),locale = "eng"))

# Filter one based on the date for 30 days slab
a=cust_info[cust_info$X_NOMIN_DT > "2012-12-24" & cust_info$X_NOMIN_DT < "2013-01-27",];
# Filter based on the integration id to extract only kutti and pad2.
a=a[a$X_EDW_INTEGRATION_ID == "kutti" | a$X_EDW_INTEGRATION_ID == "PAD2",]

######################################################################################
######################### App Downloadas #######################################
str(app_down)
# Parse the date variable into same format and apply the filter
app_down$NOMIN_DT=as.Date(parse_date_time(app_down$NOMIN_DT,orders =c("mdY HM"),locale = "eng"))
app_down=app_down[app_down$NOMIN_DT>"2011-12-25",]
app_down=subset(app_down,select = -c(X))
##########################################################################################
############################ Games Played #############################################
str(games_played)
#Parse the date and apply the filter and store it into another variable
games_played$TITLE_NOMIN_DT=as.Date(parse_date_time(games_played$TITLE_NOMIN_DT,orders =c("d m y", "d B Y", "m/d/y"),locale = "eng"))
b=games_played[games_played$TITLE_NOMIN_DT>"2011-12-25",]
##########################################################################################

################################Games added to the cart ################################
str(games_added)
library(lubridate)
#Parse the date and apply the filter and store it into another variable
games_added$TITLE_NOMIN_DT=as.Date(parse_date_time(x = games_added$TITLE_NOMIN_DT,orders = c("dmY HM"),locale = "eng"))
c=games_added[games_added$TITLE_NOMIN_DT > "2011-12-25" & games_added$TITLE_NOMIN_DT < "2013-04-11",]
##############################################################################################
###################### Purchase from the app ###################################
str(purc_app)
#Parse the date and apply the filter and store it into another variable
purc_app$TRANSACTION_DATE=as.Date(parse_date_time(x = purc_app$TRANSACTION_DATE,orders = c("mdY HM"),locale = "eng"))
d=purc_app[purc_app$TRANSACTION_DATE>"2011-12-25",]
######################################################################################
########################## Purchase from app lf #########################################
str(purc_lf)
summary(purc_lf)
#Parse the date and apply the filter and store it into another variable
purc_lf$TRANSACTION_DT=as.Date(parse_date_time(x = purc_lf$TRANSACTION_DT,orders = c("dmY HM"),locale = "eng"))
e=purc_lf[purc_lf$TRANSACTION_DT>"2011-12-25",]
###########################################################################################
########################### Child info ###############################################
##Parse the date and check for NA 
child_info$CHILD_BIRTH_DATE=as.Date(parse_date_time(x = child_info$CHILD_BIRTH_DATE,orders = c("mdY HM"),locale = "eng"))
child_info$CHILD_CREATION_DATE=as.Date(parse_date_time(x = child_info$CHILD_CREATION_DATE,orders = c("mdY HM"),locale = "eng"))
sum(is.na(child_info))

########################## Unique Value of Customer ##############################
#Extract main three columns from the datarame and change the colnames
unique1=subset(a,select = c(CONTACT_WID,X_NOMIN_DT,X_CNTRY))
unique1=unique1[!duplicated(unique1$CONTACT_WID), ]
colnames(unique1)=c('CONTACT_WID','NominationDate','Country')
########################################################################################################
############################# Max, Min and Range of Child ##################################3
# Mereg the two data frames and aggragte based on the child age column.
merge1=merge(x = unique1,y=child_info,by = 'CONTACT_WID',all = T)
x <- aggregate(CHILD_AGE ~ CONTACT_WID+PR_HOUSEHOLD_WID, merge1, FUN = max)
y <- aggregate(CHILD_AGE ~ CONTACT_WID+PR_HOUSEHOLD_WID, merge1, FUN = min)
#merge the two frames
merge2=merge(x,y,by="CONTACT_WID")
# subset some columns and apply the function of maxage-minage to find therange
merge2=subset(merge2,select = -c(PR_HOUSEHOLD_WID.x,PR_HOUSEHOLD_WID.y))
col=c("CONTACT_WID","MAX_AGE","MIN_AGE")
colnames(merge2)=col
merge2$ChildAgeRange=merge2$MAX_AGE-merge2$MIN_AGE
merge3=merge(unique1,merge2,by="CONTACT_WID",all.x = T)
colnames(merge3)=c('CONTACT_WID','NominationDate','Country','MaxChildAge',
                   'MinChildAge','ChildAgeRange')
####################################################################################################
######################## Units ####################################3
app1=d #d copy
applf=e #e copy
# Merge the two data frames
merge4=merge(merge3,app1,by="CONTACT_WID",all=T)
merge5=merge(merge3,applf,by="CONTACT_WID",all=T)

# Sum of units for every user bought through app
units_app <- aggregate(UNITS ~ CONTACT_WID, merge4, FUN = sum)
units_lf <- aggregate(UNITS~CONTACT_WID,merge5,FUN=sum)

unitsjoin=merge(units_app,units_lf,all=T)
# Extrac the sum of units bought per user
unitsjjoin <- aggregate(UNITS~CONTACT_WID,unitsjoin,FUN=sum)

######################## Units 7 ##################################
units_app7=d[d$TRANSACTION_DATE<"2012-01-02",]
units_lf7=e[e$TRANSACTION_DT<"2012-01-02",]
# Sum of units for every user bought through app
sum_app7 <- aggregate(UNITS ~ CONTACT_WID, units_app7, FUN = sum)
sum_lf7 <- aggregate(UNITS~CONTACT_WID,units_lf7,FUN=sum)

sum_app_lf_7=merge(sum_app7,sum_lf7,all=T)
sum_app_lf_7=aggregate(UNITS~CONTACT_WID,sum_app_lf_7,FUN=sum)
colnames(sum_app_lf_7)[2]="UNITS7"
######################## Units 30 ##################################
units_app30=d[d$TRANSACTION_DATE<"2012-01-26",]
units_lf30=e[e$TRANSACTION_DT<"2012-01-26",]
# Sum of units for every user bought through app
sum_app30 <- aggregate(UNITS ~ CONTACT_WID, units_app30, FUN = sum)
sum_lf30 <- aggregate(UNITS~CONTACT_WID,units_lf30,FUN=sum)

sum_app_lf_30=merge(sum_app30,sum_lf30,all=T)
sum_app_lf_30=aggregate(UNITS~CONTACT_WID,sum_app_lf_30,FUN=sum)
colnames(sum_app_lf_30)[2]="UNITS30"
######################## Units 90 ##################################
units_app90=d[d$TRANSACTION_DATE<"2012-03-26",]
units_lf90=e[e$TRANSACTION_DT<"2012-03-26",]
# Sum of units for every user bought through app
sum_app90 <- aggregate(UNITS ~ CONTACT_WID, units_app90, FUN = sum)
sum_lf90 <- aggregate(UNITS~CONTACT_WID,units_lf90,FUN=sum)

sum_app_lf_90=merge(sum_app90,sum_lf90,all=T)
sum_app_lf_90=aggregate(UNITS~CONTACT_WID,sum_app_lf_90,FUN=sum)
colnames(sum_app_lf_90)[2]="UNITS90"
######################## Units 180 ##################################
units_app180=d[d$TRANSACTION_DATE<"2012-06-23",]
units_lf180=e[e$TRANSACTION_DT<"2012-06-23",]
# Sum of units for every user bought through app
sum_app180 <- aggregate(UNITS ~ CONTACT_WID, units_app180, FUN = sum)
sum_lf180 <- aggregate(UNITS~CONTACT_WID,units_lf180,FUN=sum)

sum_app_lf_180=merge(sum_app180,sum_lf180,all=T)
sum_app_lf_180=aggregate(UNITS~CONTACT_WID,sum_app_lf_180,FUN=sum)
colnames(sum_app_lf_180)[2]="UNITS180"
######################## Units 360 ##################################
units_app360=d[d$TRANSACTION_DATE<"2012-12-26",]
units_lf360=e[e$TRANSACTION_DT<"2012-12-26",]
# Sum of units for every user bought through app
sum_app360 <- aggregate(UNITS ~ CONTACT_WID, units_app360, FUN = sum)
sum_lf360 <- aggregate(UNITS~CONTACT_WID,units_lf360,FUN=sum)

sum_app_lf_360=merge(sum_app360,sum_lf360,all=T)
sum_app_lf_360=aggregate(UNITS~CONTACT_WID,sum_app_lf_360,FUN=sum)
colnames(sum_app_lf_360)[2]="UNITS360"
#######################################################################
############## Overall Last Transaction ###############################
trans_lf=e # e copy
# Finding the max Date of the transaction per user
trans_max_lf <- aggregate(TRANSACTION_DT~CONTACT_WID,trans_lf,FUN=max)
colnames(trans_max_lf)[2]="OveralllastTransaction"
trans_app=d
trans_ma_app<-aggregate(TRANSACTION_DATE~CONTACT_WID,trans_app,FUN=max)

#########################################################################
######################## Tenure Days #############################
# Merge two data frame
tenure_lf=merge(a,e,by="CONTACT_WID",all=T)
tenure_lf=subset(tenure_lf,select = c(CONTACT_WID,X_NOMIN_DT))
tenure=merge(tenure_lf,trans_max_lf,all=T,by="CONTACT_WID")
#convert the date to numeric
tenure$X_NOMIN_DT=as.numeric(tenure$X_NOMIN_DT)
tenure$OveralllastTransaction=as.numeric(tenure$OveralllastTransaction)
tenure[is.na(tenure)]=0 # make all na 0
tenure$TenureDays=ifelse(tenure$X_NOMIN_DT==0|tenure$OveralllastTransaction==0,0,ifelse(tenure$OveralllastTransaction>tenure$X_NOMIN_DT,tenure$OveralllastTransaction-tenure$X_NOMIN_DT,0))
# Subset the data
tenure=subset(tenure,select = -c(X_NOMIN_DT,OveralllastTransaction))
#########################################################################
#################### Frequency of Transaction ##################################
fre_app=d #d copy
fre_lf=e # e copy
fre_game=b # b copy
#Apply the function to get the frequency of transaction same for all below
freq_app=aggregate(TRANSACTION_DATE~CONTACT_WID,fre_app[!duplicated(fre_app), ],FUN=length)
colnames(freq_app)[2]='FrequencyApp'
freq_lf=aggregate(TRANSACTION_DT~CONTACT_WID,fre_lf[!duplicated(fre_lf), ],FUN=length)
colnames(freq_lf)[2]='FrequencyLF'
freq_game=aggregate(ATMP_CNT~CONTACT_WID,fre_game[!duplicated(fre_game), ],FUN=sum)
colnames(freq_game)[2]='FreqGamePlay'
###############################################################################3
############################ Frequency 7 #######################################
fre_app7=d[d$TRANSACTION_DATE<"2012-01-02",]
fre_lf7=e[e$TRANSACTION_DT<"2012-01-02",]
fre_game7=b[b$TITLE_NOMIN_DT<"2012-01-02",]


freq_app7=aggregate(TRANSACTION_DATE~CONTACT_WID,fre_app7[!duplicated(fre_app7), ],FUN=length)
colnames(freq_app7)[2]='FrequencyApp7'
freq_lf7=aggregate(TRANSACTION_DT~CONTACT_WID,fre_lf7[!duplicated(fre_lf7), ],FUN=length)
colnames(freq_lf7)[2]='Frequencylf7'
freq_game7=aggregate(ATMP_CNT~CONTACT_WID,fre_game7[!duplicated(fre_game7), ],FUN=sum)
colnames(freq_game7)[2]='FreqGamePlay7'

############################ Frequency 30 #######################################
fre_app30=d[d$TRANSACTION_DATE<"2012-01-026",]
fre_lf30=e[e$TRANSACTION_DT<"2012-01-026",]
fre_game30=b[b$TITLE_NOMIN_DT<"2012-01-026",]


freq_app30=aggregate(TRANSACTION_DATE~CONTACT_WID,fre_app30[!duplicated(fre_app30), ],FUN=length)
colnames(freq_app30)[2]='FrequencyApp30'
freq_lf30=aggregate(TRANSACTION_DT~CONTACT_WID,fre_lf30[!duplicated(fre_lf30), ],FUN=length)
colnames(freq_lf30)[2]='Frequencylf30'
freq_game30=aggregate(ATMP_CNT~CONTACT_WID,fre_game30[!duplicated(fre_game30), ],FUN=sum)
colnames(freq_game30)[2]='FreqGamePlay30'

############################ Frequency 90 #######################################
fre_app90=d[d$TRANSACTION_DATE<"2012-03-26",]
fre_lf90=e[e$TRANSACTION_DT<"2012-03-26",]
fre_game90=b[b$TITLE_NOMIN_DT<"2012-03-26",]


freq_app90=aggregate(TRANSACTION_DATE~CONTACT_WID,fre_app90[!duplicated(fre_app90), ],FUN=length)
colnames(freq_app90)[2]='FrequencyApp90'
freq_lf90=aggregate(TRANSACTION_DT~CONTACT_WID,fre_lf90[!duplicated(fre_lf90), ],FUN=length)
colnames(freq_lf90)[2]='Frequencylf90'
freq_game90=aggregate(ATMP_CNT~CONTACT_WID,fre_game90[!duplicated(fre_game90), ],FUN=sum)
colnames(freq_game90)[2]='FreqGamePlay90'

############################ Frequency 180 #######################################
fre_app180=d[d$TRANSACTION_DATE<"2012-06-23",]
fre_lf180=e[e$TRANSACTION_DT<"2012-06-23",]
fre_game180=b[b$TITLE_NOMIN_DT<"2012-06-23",]


freq_app180=aggregate(TRANSACTION_DATE~CONTACT_WID,fre_app180[!duplicated(fre_app180), ],FUN=length)
colnames(freq_app7)[2]='FrequencyApp180'
freq_lf180=aggregate(TRANSACTION_DT~CONTACT_WID,fre_lf180[!duplicated(fre_lf180), ],FUN=length)
colnames(freq_lf180)[2]='Frequencylf180'
freq_game180=aggregate(ATMP_CNT~CONTACT_WID,fre_game180[!duplicated(fre_game180), ],FUN=sum)
colnames(freq_game180)[2]='FreqGamePlay180'

############################ Frequency 360 #######################################
fre_app360=d[d$TRANSACTION_DATE<"2012-12-26",]
fre_lf360=e[e$TRANSACTION_DT<"2012-12-26",]
fre_game360=b[b$TITLE_NOMIN_DT<"2012-12-26",]


freq_app360=aggregate(TRANSACTION_DATE~CONTACT_WID,fre_app360[!duplicated(fre_app360), ],FUN=length)
colnames(freq_app360)[2]='FrequencyApp360'
freq_lf360=aggregate(TRANSACTION_DT~CONTACT_WID,fre_lf360[!duplicated(fre_lf360), ],FUN=length)
colnames(freq_lf360)[2]='Frequencylf360'
freq_game360=aggregate(ATMP_CNT~CONTACT_WID,fre_game360[!duplicated(fre_game360), ],FUN=sum)
colnames(freq_game360)[2]='FreqGamePlay360'

##########################################################################################
################### Total Revenue Generated ########################################
revenue_app=d #copy d
revenue_lf=e # copy e

# Use aggregate function inorder to find the total revenue for each id.
rev_app=aggregate(AMOUNT_USD~CONTACT_WID,revenue_app,FUN=sum)
rev_lf=aggregate(AMOUNT~CONTACT_WID,revenue_lf,FUN=sum)


total_revenue=merge(rev_app,rev_lf,by="CONTACT_WID",all=T)
total_revenue[is.na(total_revenue)]=0
total_revenue$Revenue=total_revenue$AMOUNT_USD+total_revenue$AMOUNT
total_revenue=subset(total_revenue,select = c(CONTACT_WID,Revenue))
colnames(total_revenue)[2]="TotalRevenueGenerated"
########################################################################################
######################## Revenue 7 #######################################
revenue_app7=d[d$TRANSACTION_DATE<"2012-01-02",]
revenue_lf7=e[e$TRANSACTION_DT<"2012-01-02",]


# Use aggregate function inorder to find the total revenue for 7days each id.
rev_app7=aggregate(AMOUNT_USD~CONTACT_WID,revenue_app7,FUN=sum)
rev_lf7=aggregate(AMOUNT~CONTACT_WID,revenue_lf7,FUN=sum)


total_revenue7=merge(rev_app7,rev_lf7,by="CONTACT_WID",all=T)
total_revenue7[is.na(total_revenue7)]=0
total_revenue7$Revenue7=total_revenue7$AMOUNT_USD+total_revenue7$AMOUNT
total_revenue7=subset(total_revenue7,select = c(CONTACT_WID,Revenue7))

########################################################################################
######################## Revenue 30 #######################################
revenue_app30=d[d$TRANSACTION_DATE<"2012-01-26",]
revenue_lf30=e[e$TRANSACTION_DT<"2012-01-26",]

# Use aggregate function inorder to find the total revenue for 30 days each id.
rev_app30=aggregate(AMOUNT_USD~CONTACT_WID,revenue_app30,FUN=sum)
rev_lf30=aggregate(AMOUNT~CONTACT_WID,revenue_lf30,FUN=sum)
#rev_carts=

total_revenue30=merge(rev_app30,rev_lf30,by="CONTACT_WID",all=T)
total_revenue30[is.na(total_revenue30)]=0
total_revenue30$Revenue30=total_revenue30$AMOUNT_USD+total_revenue30$AMOUNT
total_revenue30=subset(total_revenue30,select = c(CONTACT_WID,Revenue30))

########################################################################################
######################## Revenue 90 #######################################
revenue_app90=d[d$TRANSACTION_DATE<"2012-03-26",]
revenue_lf90=e[e$TRANSACTION_DT<"2012-03-26",]


rev_app90=aggregate(AMOUNT_USD~CONTACT_WID,revenue_app90,FUN=sum)
rev_lf90=aggregate(AMOUNT~CONTACT_WID,revenue_lf90,FUN=sum)


total_revenue90=merge(rev_app90,rev_lf90,by="CONTACT_WID",all=T)
total_revenue90[is.na(total_revenue90)]=0
total_revenue90$Revenue90=total_revenue90$AMOUNT_USD+total_revenue90$AMOUNT
total_revenue90=subset(total_revenue90,select = c(CONTACT_WID,Revenue90))

########################################################################################
######################## Revenue 180 #######################################
revenue_app180=d[d$TRANSACTION_DATE<"2012-06-23",]
revenue_lf180=e[e$TRANSACTION_DT<"2012-06-23",]


rev_app180=aggregate(AMOUNT_USD~CONTACT_WID,revenue_app180,FUN=sum)
rev_lf180=aggregate(AMOUNT~CONTACT_WID,revenue_lf180,FUN=sum)


total_revenue180=merge(rev_app180,rev_lf180,by="CONTACT_WID",all=T)
total_revenue180[is.na(total_revenue180)]=0
total_revenue180$Revenue180=total_revenue180$AMOUNT_USD+total_revenue180$AMOUNT
total_revenue180=subset(total_revenue180,select = c(CONTACT_WID,Revenue180))
########################################################################################
######################## Revenue 360 #######################################
revenue_app360=d[d$TRANSACTION_DATE<"2012-12-26",]
revenue_lf360=e[e$TRANSACTION_DT<"2012-12-26",]


rev_app360=aggregate(AMOUNT_USD~CONTACT_WID,revenue_app360,FUN=sum)
rev_lf360=aggregate(AMOUNT~CONTACT_WID,revenue_lf360,FUN=sum)


total_revenue360=merge(rev_app360,rev_lf360,by="CONTACT_WID",all=T)
total_revenue360[is.na(total_revenue360)]=0
total_revenue360$Revenue360=total_revenue360$AMOUNT_USD+total_revenue360$AMOUNT
total_revenue360=subset(total_revenue360,select = c(CONTACT_WID,Revenue360))
##################################################################################
######################### Recency #########################################
rec_app=d
rec_lf=e
rec_down=app_down
# Agrregate the data based on the transaction date 
rec_app=aggregate(TRANSACTION_DATE~CONTACT_WID,rec_app,FUN=max)
colnames(rec_app)[2]='Recency'
rec_lf=aggregate(TRANSACTION_DT~CONTACT_WID,rec_lf,FUN=max)
colnames(rec_lf)[2]='Recency'
rec_down=aggregate(NOMIN_DT~CONTACT_WID,rec_down,FUN=max)
colnames(rec_down)[2]='Recency'
#Find the recency by substracting the two dates in numeric form
rec_app$RecencyAPP=as.numeric(as.Date('2013-04-11'))-as.numeric(rec_app$Recency)
rec_lf$Recencylf=as.numeric(as.Date('2013-04-11'))-as.numeric(rec_lf$Recency)
rec_down$Recencydown=as.numeric(as.Date('2013-04-11'))-as.numeric(rec_down$Recency)

rec_app=subset(rec_app,select = -c(Recency))
rec_lf=subset(rec_lf,select = -c(Recency))
rec_down=subset(rec_down,select = -c(Recency))
###############################################################################3
######################### Recency7 #########################################
rec_app7=d[d$TRANSACTION_DATE<"2012-01-02",]
rec_lf7=e[e$TRANSACTION_DT<"2012-01-02",]
rec_down7=app_down[app_down$NOMIN_DT<"2012-01-02",]

rec_app7=aggregate(TRANSACTION_DATE~CONTACT_WID,rec_app7,FUN=max)
colnames(rec_app7)[2]='Recency7'
rec_lf7=aggregate(TRANSACTION_DT~CONTACT_WID,rec_lf7,FUN=max)
colnames(rec_lf7)[2]='Recency7'
rec_down7=aggregate(NOMIN_DT~CONTACT_WID,rec_down7,FUN=max)
colnames(rec_down7)[2]='Recency7'

rec_app7$RecencyAPP7=as.numeric(as.Date('2012-01-02'))-as.numeric(rec_app7$Recency7)
rec_lf7$Recencylf7=as.numeric(as.Date('2012-01-02'))-as.numeric(rec_lf7$Recency7)
rec_down7$Recencydown7=as.numeric(as.Date('2012-01-02'))-as.numeric(rec_down7$Recency7)

rec_app7=subset(rec_app7,select = -c(Recency7))
rec_lf7=subset(rec_lf7,select = -c(Recency7))
rec_down7=subset(rec_down7,select = -c(Recency7))
###############################################################################3
######################### Recency 30 #########################################
rec_app30=d[d$TRANSACTION_DATE<"2012-01-26",]
rec_lf30=e[e$TRANSACTION_DT<"2012-01-26",]
rec_down30=app_down[app_down$NOMIN_DT<"2012-01-26",]

rec_app30=aggregate(TRANSACTION_DATE~CONTACT_WID,rec_app30,FUN=max)
colnames(rec_app30)[2]='Recency30'
rec_lf30=aggregate(TRANSACTION_DT~CONTACT_WID,rec_lf30,FUN=max)
colnames(rec_lf30)[2]='Recency30'
rec_down30=aggregate(NOMIN_DT~CONTACT_WID,rec_down30,FUN=max)
colnames(rec_down30)[2]='Recency30'

rec_app30$RecencyAPP30=as.numeric(as.Date('2012-01-26'))-as.numeric(rec_app30$Recency30)
rec_lf30$Recencylf30=as.numeric(as.Date('2012-01-26'))-as.numeric(rec_lf30$Recency30)
rec_down30$Recencydown30=as.numeric(as.Date('2012-01-26'))-as.numeric(rec_down30$Recency30)

rec_app30=subset(rec_app30,select = -c(Recency30))
rec_lf30=subset(rec_lf30,select = -c(Recency30))
rec_down30=subset(rec_down30,select = -c(Recency30))
######################### Recency 90 #########################################
rec_app90=d[d$TRANSACTION_DATE<"2012-03-26",]
rec_lf90=e[e$TRANSACTION_DT<"2012-03-26",]
rec_down90=app_down[app_down$NOMIN_DT<"2012-03-26",]

rec_app90=aggregate(TRANSACTION_DATE~CONTACT_WID,rec_app90,FUN=max)
colnames(rec_app90)[2]='Recency90'
rec_lf90=aggregate(TRANSACTION_DT~CONTACT_WID,rec_lf90,FUN=max)
colnames(rec_lf90)[2]='Recency90'
rec_down90=aggregate(NOMIN_DT~CONTACT_WID,rec_down90,FUN=max)
colnames(rec_down90)[2]='Recency90'

rec_app90$RecencyAPP90=as.numeric(as.Date('2012-03-26'))-as.numeric(rec_app90$Recency90)
rec_lf90$Recencylf90=as.numeric(as.Date('2012-03-26'))-as.numeric(rec_lf90$Recency90)
rec_down90$Recencydown90=as.numeric(as.Date('2012-03-26'))-as.numeric(rec_down90$Recency90)

rec_app90=subset(rec_app90,select = -c(Recency90))
rec_lf90=subset(rec_lf90,select = -c(Recency90))
rec_down90=subset(rec_down90,select = -c(Recency90))
######################### Recency 180 #########################################
rec_app180=d[d$TRANSACTION_DATE<"2012-06-23",]
rec_lf180=e[e$TRANSACTION_DT<"2012-06-23",]
rec_down180=app_down[app_down$NOMIN_DT<"2012-06-23",]

rec_app180=aggregate(TRANSACTION_DATE~CONTACT_WID,rec_app180,FUN=max)
colnames(rec_app180)[2]='Recency180'
rec_lf180=aggregate(TRANSACTION_DT~CONTACT_WID,rec_lf180,FUN=max)
colnames(rec_lf180)[2]='Recency180'
rec_down180=aggregate(NOMIN_DT~CONTACT_WID,rec_down180,FUN=max)
colnames(rec_down180)[2]='Recency180'

rec_app180$RecencyAPP180=as.numeric(as.Date('2012-06-23'))-as.numeric(rec_app180$Recency180)
rec_lf180$Recencylf180=as.numeric(as.Date('2012-06-23'))-as.numeric(rec_lf180$Recency180)
rec_down180$Recencydown180=as.numeric(as.Date('2012-06-23'))-as.numeric(rec_down180$Recency180)

rec_app180=subset(rec_app180,select = -c(Recency180))
rec_lf180=subset(rec_lf180,select = -c(Recency180))
rec_down180=subset(rec_down180,select = -c(Recency180))
######################### Recency 360 #########################################
rec_app360=d[d$TRANSACTION_DATE<"2012-12-26",]
rec_lf360=e[e$TRANSACTION_DT<"2012-12-26",]
rec_down360=app_down[app_down$NOMIN_DT<"2012-12-26",]

rec_app360=aggregate(TRANSACTION_DATE~CONTACT_WID,rec_app360,FUN=max)
colnames(rec_app360)[2]='Recency360'
rec_lf360=aggregate(TRANSACTION_DT~CONTACT_WID,rec_lf360,FUN=max)
colnames(rec_lf360)[2]='Recency360'
rec_down360=aggregate(NOMIN_DT~CONTACT_WID,rec_down360,FUN=max)
colnames(rec_down360)[2]='Recency360'

rec_app360$RecencyAPP360=as.numeric(as.Date('2012-12-26'))-as.numeric(rec_app360$Recency360)
rec_lf360$Recencylf360=as.numeric(as.Date('2012-12-26'))-as.numeric(rec_lf360$Recency360)
rec_down360$Recencydown360=as.numeric(as.Date('2012-12-26'))-as.numeric(rec_down360$Recency360)

rec_app360=subset(rec_app360,select = -c(Recency360))
rec_lf360=subset(rec_lf360,select = -c(Recency360))
rec_down360=subset(rec_down360,select = -c(Recency360))
#########################################################################################

############################### Max Recency Cumulative ###########################
maxrec1=merge(rec_app,rec_lf,by="CONTACT_WID",all=T)
maxrec2=merge(maxrec1,rec_down,by="CONTACT_WID",all=T)

# use the apply function to find the ma and min values.
maxrec2[, "maxRecencyCum"] <- apply(maxrec2[, 2:4], 1, max,na.rm=T)
maxrec2[, "minRecencyCum"] <- apply(maxrec2[, 2:4], 1, min,na.rm=T)
maxrec2=subset(maxrec2,select = c(CONTACT_WID,maxRecencyCum,minRecencyCum))
############################### Max Recency Cumulative 7 days ###########################
maxrec17=merge(rec_app7,rec_lf7,by="CONTACT_WID",all=T)
maxrec27=merge(maxrec17,rec_down7,by="CONTACT_WID",all=T)

maxrec27[, "maxRecencyCum7"] <- apply(maxrec27[, 2:4], 1, max,na.rm=T)
maxrec27[, "minRecencyCum7"] <- apply(maxrec27[, 2:4], 1, min,na.rm=T)
maxrec27=subset(maxrec27,select = c(CONTACT_WID,maxRecencyCum7,minRecencyCum7))
############################### Max Recency Cumulative 30days ###########################
maxrec130=merge(rec_app30,rec_lf30,by="CONTACT_WID",all=T)
maxrec230=merge(maxrec130,rec_down30,by="CONTACT_WID",all=T)

maxrec230[, "maxRecencyCum30"] <- apply(maxrec230[, 2:4], 1, max,na.rm=T)
maxrec230[, "minRecencyCum30"] <- apply(maxrec230[, 2:4], 1, min,na.rm=T)
maxrec230=subset(maxrec230,select = c(CONTACT_WID,maxRecencyCum30,minRecencyCum30))

############################### Max Recency Cumulative 90 days ###########################
maxrec190=merge(rec_app90,rec_lf90,by="CONTACT_WID",all=T)
maxrec290=merge(maxrec190,rec_down90,by="CONTACT_WID",all=T)

maxrec290[, "maxRecencyCum90"] <- apply(maxrec290[, 2:4], 1, max,na.rm=T)
maxrec290[, "minRecencyCum90"] <- apply(maxrec290[, 2:4], 1, min,na.rm=T)
maxrec290=subset(maxrec290,select = c(CONTACT_WID,maxRecencyCum90,minRecencyCum90))

############################### Max Recency Cumulative 180days ###########################
maxrec1180=merge(rec_app180,rec_lf180,by="CONTACT_WID",all=T)
maxrec2180=merge(maxrec1180,rec_down180,by="CONTACT_WID",all=T)

maxrec2180[, "maxRecencyCum180"] <- apply(maxrec2180[, 2:4], 1, max,na.rm=T)
maxrec2180[, "minRecencyCum180"] <- apply(maxrec2180[, 2:4], 1, min,na.rm=T)
maxrec2180=subset(maxrec2180,select = c(CONTACT_WID,maxRecencyCum180,minRecencyCum180))

############################### Max Recency Cumulative 360 day ###########################
maxrec1360=merge(rec_app360,rec_lf360,by="CONTACT_WID",all=T)
maxrec2360=merge(maxrec1360,rec_down360,by="CONTACT_WID",all=T)

maxrec2360[, "maxRecencyCum360"] <- apply(maxrec2360[, 2:4], 1, max,na.rm=T)
maxrec2360[, "minRecencyCum360"] <- apply(maxrec2360[, 2:4], 1, min,na.rm=T)
maxrec2360=subset(maxrec2360,select = c(CONTACT_WID,maxRecencyCum360,minRecencyCum360))

##################################################################################################
##################### No of Childrens in the House #####################################
# Find the number of childrens in each house
NumHouseChildren=aggregate(CRM_CHLD_KEY~CONTACT_WID,child_info,FUN=length)
colnames(NumHouseChildren)[2]='No_of_Childrens'

######################################################################################
########################## TOTAL TIME GAME PLaY ###############################
# aggregate to find the sum of time they play the game
TotalTimeGamePlay=aggregate(ACT_TME_SPN_QTY~CONTACT_WID,b,FUN=sum)
colnames(TotalTimeGamePlay)[2]="Time"

game7=b[b$TITLE_NOMIN_DT<"2012-01-02",]
game30=b[b$TITLE_NOMIN_DT<"2012-01-26",]
game90=b[b$TITLE_NOMIN_DT<"2012-03-26",]
game180=b[b$TITLE_NOMIN_DT<"2012-06-23",]
game360=b[b$TITLE_NOMIN_DT<"2012-12-26",]
# Apply the samefunction above and get the time span for every days
TotalTimeGamePlay7=aggregate(ACT_TME_SPN_QTY~CONTACT_WID,game7,FUN=sum)
colnames(TotalTimeGamePlay7)[2]="Time7"
TotalTimeGamePlay30=aggregate(ACT_TME_SPN_QTY~CONTACT_WID,game30,FUN=sum)
colnames(TotalTimeGamePlay30)[2]="Time30"
TotalTimeGamePlay90=aggregate(ACT_TME_SPN_QTY~CONTACT_WID,game90,FUN=sum)
colnames(TotalTimeGamePlay90)[2]="Time90"
TotalTimeGamePlay180=aggregate(ACT_TME_SPN_QTY~CONTACT_WID,game180,FUN=sum)
colnames(TotalTimeGamePlay180)[2]="Time180"
TotalTimeGamePlay360=aggregate(ACT_TME_SPN_QTY~CONTACT_WID,game360,FUN=sum)
colnames(TotalTimeGamePlay360)[2]="Time360"
###############################################################################

############### Number of female and Male Childrens in the household################
# Aggregate the function to find the female count in the house per userid
NumFemaleChildren=aggregate((SEX_MF_CODE=="F")~CONTACT_WID+PR_HOUSEHOLD_WID,child_info,FUN=sum)
NumFemaleChildren=subset(NumFemaleChildren,select = -c(PR_HOUSEHOLD_WID))
colnames(NumFemaleChildren)[2]="NumFemaleChildrenHousehold"
# Aggregate the function to find the male count in the house hold per user id
NumMaleChildren=aggregate((SEX_MF_CODE=="M")~CONTACT_WID+PR_HOUSEHOLD_WID,child_info,FUN=sum)
NumMaleChildren=subset(NumMaleChildren,select = -c(PR_HOUSEHOLD_WID))
colnames(NumMaleChildren)[2]="NumMaleChildrenHousehold"
##########################################################################################
############################ Games Played and Games Bought #########################
# Aggregate the function to find the no of time a particular game is been played and bought
NumGamesPlayed=aggregate(X_GAME_NM~CONTACT_WID,b,FUN=length)
colnames(NumGamesPlayed)[2]='Number_Games_Played'
NumGamesBought=aggregate(ITEM_NUMBER~CONTACT_WID,d,FUN=length)
colnames(NumGamesBought)[2]="Number_Games_Bought"
gameplayed7=b[b$TITLE_NOMIN_DT<"2012-01-02",]
gameplayed30=b[b$TITLE_NOMIN_DT<"2012-01-26",]
gameplayed90=b[b$TITLE_NOMIN_DT<"2012-03-26",]
gameplayed180=b[b$TITLE_NOMIN_DT<"2012-06-23",]
gameplayed360=b[b$TITLE_NOMIN_DT<"2012-12-26",]

NumGamesPlayed7=aggregate(X_GAME_NM~CONTACT_WID,gameplayed7,FUN=length)
colnames(NumGamesPlayed7)[2]='No_of_games_played7'
NumGamesPlayed30=aggregate(X_GAME_NM~CONTACT_WID,gameplayed30,FUN=length)
colnames(NumGamesPlayed30)[2]='No_of_games_played30'
NumGamesPlayed90=aggregate(X_GAME_NM~CONTACT_WID,gameplayed90,FUN=length)
colnames(NumGamesPlayed90)[2]='No_of_games_played90'
NumGamesPlayed180=aggregate(X_GAME_NM~CONTACT_WID,gameplayed180,FUN=length)
colnames(NumGamesPlayed180)[2]='No_of_games_played180'
NumGamesPlayed360=aggregate(X_GAME_NM~CONTACT_WID,gameplayed360,FUN=length)
colnames(NumGamesPlayed360)[2]='No_of_games_played360'

#####################################################################################
########################### Favorite Source, Channel and Game ############################
channel_source=subset(d,select = c(CONTACT_WID,CHANNEL_DESCRIPTION))
channel_source$CONTACT_WID = as.factor(channel_source$CONTACT_WID)

library(magrittr)
library(dplyr)
user_sum <- channel_source %>% group_by(CONTACT_WID) %>% summarise(user_sum =n())
summary <- channel_source %>% group_by(CONTACT_WID,CHANNEL_DESCRIPTION) %>% summarise(freq=n())
final <- merge(summary,user_sum)
final$freq1 <- (final$freq/final$user_sum)*100
Favourite_Channel <- final %>% group_by(CONTACT_WID) %>% summarise(freq1=max(freq1))
colnames(Favourite_Channel)[2]="FavoriteChannel"
Favourite_Channel$FavoriteChannel=ifelse(Favourite_Channel$FavoriteChannel>50,'Fav','Uni')


########################## Source ######################################
source=subset(d,select = c(CONTACT_WID,SOURCE_OF_PURCHASE))
source$CONTACT_WID=as.factor(source$CONTACT_WID)
source_sum <- source %>% group_by(CONTACT_WID) %>% summarise(sum =n())
summary_source <- source %>% group_by(CONTACT_WID,SOURCE_OF_PURCHASE) %>% summarise(freq=n())
final_source <- merge(summary_source,source_sum)

final_source$freq1 <- (final_source$freq/final_source$sum)*100
Favourite_Source <- final_source %>% group_by(CONTACT_WID) %>% summarise(freq1=max(freq1))
colnames(Favourite_Source)[2]="FavoriteSource"
Favourite_Source$FavoriteSource=ifelse(Favourite_Source$FavoriteSource>50,'Fav','Uni')

####################### Game ##############################################
ga=subset(b,select = c(CONTACT_WID,X_GAME_NM,ACT_TME_SPN_QTY))

game_sum <- ga %>% group_by(CONTACT_WID) %>% summarise(sum =sum(ACT_TME_SPN_QTY))

final_game=merge(ga,game_sum)
final_game$percent=(final_game$ACT_TME_SPN_QTY/final_game$sum)*100
final_game=subset(final_game,select = -c(ACT_TME_SPN_QTY,sum))

Favourite_Game=final_game %>% group_by(CONTACT_WID) %>% summarise(percent = max(percent))
colnames(Favourite_Game)[2]="FavoriteGame"
Favourite_Game$FavoriteGame[is.nan(Favourite_Game$FavoriteGame)]=0
Favourite_Game$FavoriteGame=as.factor(ifelse(Favourite_Game$FavoriteGame>50,'Fav','Uni'))

##############################################################################

######################### Favourite Source by various days #################
source7=d[d$TRANSACTION_DATE<"2012-01-02",]
source30=d[d$TRANSACTION_DATE<"2012-01-26",]
source90=d[d$TRANSACTION_DATE<"2012-03-26",]
source180=d[d$TRANSACTION_DATE<"2012-06-23",]
source360=d[d$TRANSACTION_DATE<"2012-12-26",]


source7=subset(source7,select = c(CONTACT_WID,SOURCE_OF_PURCHASE))
source7$CONTACT_WID = as.factor(source7$CONTACT_WID)
source30=subset(source30,select = c(CONTACT_WID,SOURCE_OF_PURCHASE))
source30$CONTACT_WID = as.factor(source30$CONTACT_WID)
source90=subset(source90,select = c(CONTACT_WID,SOURCE_OF_PURCHASE))
source90$CONTACT_WID = as.factor(source90$CONTACT_WID)
source180=subset(source180,select = c(CONTACT_WID,SOURCE_OF_PURCHASE))
source180$CONTACT_WID = as.factor(source180$CONTACT_WID)
source360=subset(source360,select = c(CONTACT_WID,SOURCE_OF_PURCHASE))
source360$CONTACT_WID = as.factor(source360$CONTACT_WID)

source_sum7 <- source7 %>% group_by(CONTACT_WID) %>% summarise(sum =n())
summary_source7 <- source7 %>% group_by(CONTACT_WID,SOURCE_OF_PURCHASE) %>% summarise(freq=n())
final7 <- merge(summary_source7,source_sum7)
final7$freq7 <- (final7$freq/final7$sum)*100
Favourite_Source7 <- final7 %>% group_by(CONTACT_WID) %>% summarise(freq7=max(freq7))
colnames(Favourite_Source7)[2]="FavoriteSource7"
Favourite_Source7$FavoriteSource7=ifelse(Favourite_Source7$FavoriteSource7>50,'Fav','Uni')


source_sum30 <- source30 %>% group_by(CONTACT_WID) %>% summarise(sum =n())
summary_source30 <- source30 %>% group_by(CONTACT_WID,SOURCE_OF_PURCHASE) %>% summarise(freq=n())
final30 <- merge(summary_source30,source_sum30)
final30$freq30 <- (final30$freq/final30$sum)*100
Favourite_Source30 <- final30 %>% group_by(CONTACT_WID) %>% summarise(freq30=max(freq30))
colnames(Favourite_Source30)[2]="FavoriteSource30"
Favourite_Source30$FavoriteSource30=ifelse(Favourite_Source30$FavoriteSource30>50,'Fav','Uni')


source_sum90 <- source90 %>% group_by(CONTACT_WID) %>% summarise(sum =n())
summary_source90 <- source90 %>% group_by(CONTACT_WID,SOURCE_OF_PURCHASE) %>% summarise(freq=n())
final90 <- merge(summary_source90,source_sum90)
final90$freq90 <- (final90$freq/final90$sum)*100
Favourite_Source90 <- final90 %>% group_by(CONTACT_WID) %>% summarise(freq90=max(freq90))
colnames(Favourite_Source90)[2]="FavoriteSource90"
Favourite_Source90$FavoriteSource90=ifelse(Favourite_Source90$FavoriteSource90>50,'Fav','Uni')


source_sum180 <- source180 %>% group_by(CONTACT_WID) %>% summarise(sum =n())
summary_source180 <- source180 %>% group_by(CONTACT_WID,SOURCE_OF_PURCHASE) %>% summarise(freq=n())
final180 <- merge(summary_source180,source_sum180)
final180$freq180 <- (final180$freq/final180$sum)*100
Favourite_Source180 <- final180 %>% group_by(CONTACT_WID) %>% summarise(freq180=max(freq180))
colnames(Favourite_Source180)[2]="FavoriteSource180"
Favourite_Source180$FavoriteSource180=ifelse(Favourite_Source180$FavoriteSource180>50,'Fav','Uni')


source_sum360 <- source360 %>% group_by(CONTACT_WID) %>% summarise(sum =n())
summary_source360 <- source360 %>% group_by(CONTACT_WID,SOURCE_OF_PURCHASE) %>% summarise(freq=n())
final360 <- merge(summary_source360,source_sum360)
final360$freq360 <- (final360$freq/final360$sum)*100
Favourite_Source360 <- final360 %>% group_by(CONTACT_WID) %>% summarise(freq360=max(freq360))
colnames(Favourite_Source360)[2]="FavoriteSource360"
Favourite_Source360$FavoriteSource360=ifelse(Favourite_Source360$FavoriteSource360>50,'Fav','Uni')

#############################################################################################
################ Favourite channels by various days m######################
channel7=d[d$TRANSACTION_DATE<"2012-01-02",]
channel30=d[d$TRANSACTION_DATE<"2012-01-26",]
channel90=d[d$TRANSACTION_DATE<"2012-03-26",]
channel180=d[d$TRANSACTION_DATE<"2012-06-23",]
channel360=d[d$TRANSACTION_DATE<"2012-12-26",]


channel7=subset(channel7,select = c(CONTACT_WID,CHANNEL_DESCRIPTION))
channel7$CONTACT_WID = as.factor(channel7$CONTACT_WID)
channel30=subset(channel30,select = c(CONTACT_WID,CHANNEL_DESCRIPTION))
channel30$CONTACT_WID = as.factor(channel30$CONTACT_WID)
channel90=subset(channel90,select = c(CONTACT_WID,CHANNEL_DESCRIPTION))
channel90$CONTACT_WID = as.factor(channel90$CONTACT_WID)
channel180=subset(channel180,select = c(CONTACT_WID,CHANNEL_DESCRIPTION))
channel180$CONTACT_WID = as.factor(channel180$CONTACT_WID)
channel360=subset(channel360,select = c(CONTACT_WID,CHANNEL_DESCRIPTION))
channel360$CONTACT_WID = as.factor(channel360$CONTACT_WID)

channel_sum7 <- channel7 %>% group_by(CONTACT_WID) %>% summarise(sum =n())
summary_channel7 <- channel7 %>% group_by(CONTACT_WID,CHANNEL_DESCRIPTION) %>% summarise(freq=n())
finalchannel7 <- merge(summary_channel7,channel_sum7)
finalchannel7$freqchannel7 <- (finalchannel7$freq/finalchannel7$sum)*100
Favourite_Channel7 <- finalchannel7 %>% group_by(CONTACT_WID) %>% summarise(freqchannel7=max(freqchannel7))
colnames(Favourite_Channel7)[2]="FavoriteChannel7"
Favourite_Channel7$FavoriteChannel7=ifelse(Favourite_Channel7$FavoriteChannel7>50,'Fav','Uni')

channel_sum30 <- channel30 %>% group_by(CONTACT_WID) %>% summarise(sum =n())
summary_channel30 <- channel30 %>% group_by(CONTACT_WID,CHANNEL_DESCRIPTION) %>% summarise(freq=n())
finalchannel30 <- merge(summary_channel30,channel_sum30)
finalchannel30$freqchannel30 <- (finalchannel30$freq/finalchannel30$sum)*100
Favourite_Channel30 <- finalchannel30 %>% group_by(CONTACT_WID) %>% summarise(freqchannel30=max(freqchannel30))
colnames(Favourite_Channel30)[2]="FavoriteChannel30"
Favourite_Channel30$FavoriteChannel30=ifelse(Favourite_Channel30$FavoriteChannel30>50,'Fav','Uni')

channel_sum90 <- channel90 %>% group_by(CONTACT_WID) %>% summarise(sum =n())
summary_channel90 <- channel90 %>% group_by(CONTACT_WID,CHANNEL_DESCRIPTION) %>% summarise(freq=n())
finalchannel90 <- merge(summary_channel90,channel_sum90)
finalchannel90$freqchannel90 <- (finalchannel90$freq/finalchannel90$sum)*100
Favourite_Channel90 <- finalchannel90 %>% group_by(CONTACT_WID) %>% summarise(freqchannel90=max(freqchannel90))
colnames(Favourite_Channel90)[2]="FavoriteChannel90"
Favourite_Channel90$FavoriteChannel90=ifelse(Favourite_Channel90$FavoriteChannel90>50,'Fav','Uni')

channel_sum180 <- channel180 %>% group_by(CONTACT_WID) %>% summarise(sum =n())
summary_channel180 <- channel180 %>% group_by(CONTACT_WID,CHANNEL_DESCRIPTION) %>% summarise(freq=n())
finalchannel180 <- merge(summary_channel180,channel_sum180)
finalchannel180$freqchannel180 <- (finalchannel180$freq/finalchannel180$sum)*100
Favourite_Channel180 <- finalchannel180 %>% group_by(CONTACT_WID) %>% summarise(freqchannel180=max(freqchannel180))
colnames(Favourite_Channel180)[2]="FavoriteChannel180"
Favourite_Channel180$FavoriteChannel180=ifelse(Favourite_Channel180$FavoriteChannel180>50,'Fav','Uni')

channel_sum360 <- channel360 %>% group_by(CONTACT_WID) %>% summarise(sum =n())
summary_channel360 <- channel360 %>% group_by(CONTACT_WID,CHANNEL_DESCRIPTION) %>% summarise(freq=n())
finalchannel360 <- merge(summary_channel360,channel_sum360)
finalchannel360$freqchannel360 <- (finalchannel360$freq/finalchannel360$sum)*100
Favourite_Channel360 <- finalchannel360 %>% group_by(CONTACT_WID) %>% summarise(freqchannel360=max(freqchannel360))
colnames(Favourite_Channel360)[2]="FavoriteChannel360"
Favourite_Channel360$FavoriteChannel360=ifelse(Favourite_Channel360$FavoriteChannel360>50,'Fav','Uni')

###############################################################################################
##################### Fav game by various day###################################
favgame7=b[b$TITLE_NOMIN_DT<"2012-01-02",]
favgame30=b[b$TITLE_NOMIN_DT<"2012-01-26",]
favgame90=b[b$TITLE_NOMIN_DT<"2012-03-26",]
favgame180=b[b$TITLE_NOMIN_DT<"2012-06-23",]
favgame360=b[b$TITLE_NOMIN_DT<"2012-12-26",]

favgame7=subset(b,select = c(CONTACT_WID,X_GAME_NM,ACT_TME_SPN_QTY))
game_sum7 <- favgame7 %>% group_by(CONTACT_WID) %>% summarise(sum =sum(ACT_TME_SPN_QTY))
final_game7=merge(favgame7,game_sum7)
final_game7$percent=(final_game7$ACT_TME_SPN_QTY/final_game7$sum)*100
final_game7=subset(final_game7,select = -c(ACT_TME_SPN_QTY,sum))
Favourite_Game7=final_game7 %>% group_by(CONTACT_WID) %>% summarise(percent = max(percent))
colnames(Favourite_Game7)[2]="FavoriteGame7"
Favourite_Game7$FavoriteGame7[is.nan(Favourite_Game7$FavoriteGame7)]=0
Favourite_Game7$FavoriteGame7=ifelse(Favourite_Game7$FavoriteGame7>50,"Fav","Uni")

game_sum30 <- favgame30 %>% group_by(CONTACT_WID) %>% summarise(sum =sum(ACT_TME_SPN_QTY))
final_game30=merge(favgame30,game_sum30)
final_game30$percent=(final_game30$ACT_TME_SPN_QTY/final_game30$sum)*100
final_game30=subset(final_game30,select = -c(ACT_TME_SPN_QTY,sum))
Favourite_Game30=final_game30 %>% group_by(CONTACT_WID) %>% summarise(percent = max(percent))
colnames(Favourite_Game30)[2]="FavoriteGame30"
Favourite_Game30$FavoriteGame30[is.nan(Favourite_Game30$FavoriteGame30)]=0
Favourite_Game30$FavoriteGame30=ifelse(Favourite_Game30$FavoriteGame30>50,"Fav","Uni")


game_sum90 <- favgame90 %>% group_by(CONTACT_WID) %>% summarise(sum =sum(ACT_TME_SPN_QTY))
final_game90=merge(favgame90,game_sum90)
final_game90$percent=(final_game90$ACT_TME_SPN_QTY/final_game90$sum)*100
final_game90=subset(final_game90,select = -c(ACT_TME_SPN_QTY,sum))
Favourite_Game90=final_game90 %>% group_by(CONTACT_WID) %>% summarise(percent = max(percent))
colnames(Favourite_Game90)[2]="FavoriteGame90"
Favourite_Game90$FavoriteGame90[is.nan(Favourite_Game90$FavoriteGame90)]=0
Favourite_Game90$FavoriteGame90=ifelse(Favourite_Game90$FavoriteGame90>50,"Fav","Uni")


game_sum180 <- favgame180 %>% group_by(CONTACT_WID) %>% summarise(sum =sum(ACT_TME_SPN_QTY))
final_game180=merge(favgame180,game_sum180)
final_game180$percent=(final_game180$ACT_TME_SPN_QTY/final_game180$sum)*100
final_game180=subset(final_game180,select = -c(ACT_TME_SPN_QTY,sum))
Favourite_Game180=final_game180 %>% group_by(CONTACT_WID) %>% summarise(percent = max(percent))
colnames(Favourite_Game180)[2]="FavoriteGame180"
Favourite_Game180$FavoriteGame180[is.nan(Favourite_Game180$FavoriteGame180)]=0
Favourite_Game180$FavoriteGame180=ifelse(Favourite_Game180$FavoriteGame180>50,"Fav","Uni")

game_sum360 <- favgame360 %>% group_by(CONTACT_WID) %>% summarise(sum =sum(ACT_TME_SPN_QTY))
final_game360=merge(favgame360,game_sum360)
final_game360$percent=(final_game360$ACT_TME_SPN_QTY/final_game360$sum)*100
final_game360=subset(final_game360,select = -c(ACT_TME_SPN_QTY,sum))
Favourite_Game360=final_game360 %>% group_by(CONTACT_WID) %>% summarise(percent = max(percent))
colnames(Favourite_Game360)[2]="FavoriteGame360"
Favourite_Game360$FavoriteGame360[is.nan(Favourite_Game360$FavoriteGame360)]=0
Favourite_Game360$FavoriteGame360=ifelse(Favourite_Game360$FavoriteGame360>50,"Fav","Uni")

#########################################################################################
########################################################################################
strengthoffavouritegame=final_game
strengthoffavouritegame$percent=ifelse(strengthoffavouritegame$percent>=95,"ADDICTED",ifelse(strengthoffavouritegame$percent>=80&strengthoffavouritegame$percent<95,"Strongly Addictive",ifelse(strengthoffavouritegame$percent>=60&strengthoffavouritegame$percent<80,"Favourite",ifelse(strengthoffavouritegame$percent>=40&strengthoffavouritegame$percent<60,"Medium Fav",ifelse(strengthoffavouritegame$percent>=20&strengthoffavouritegame$percent<40,"Weekly Addictive","Nothing")))))

strengthoffavouritegame=favgame
strengthoffavouritegame$percent=ifelse(strengthoffavouritegame$percent>=95,"ADDICTED",ifelse(strengthoffavouritegame$percent>=80&strengthoffavouritegame$percent<95,"Strongly Addictive",ifelse(strengthoffavouritegame$percent>=60&strengthoffavouritegame$percent<80,"Favourite",ifelse(strengthoffavouritegame$percent>=40&strengthoffavouritegame$percent<60,"Medium Fav",ifelse(strengthoffavouritegame$percent>=20&strengthoffavouritegame$percent<40,"Weekly Addictive","Nothing")))))
colnames(strengthoffavouritegame)[2]="Strength_of_FavoriteGame"
strengthoffavouritegame=subset(strengthoffavouritegame,select = -c(X_GAME_NM))
########################################################################################################
###############################################################################################################

##################### Make up the DATA FRAME  ######################################
MyMerge <- function(x, y){
  df <- merge(x, y, by= "CONTACT_WID",all.x = T)
  return(df)
}
new.df <- Reduce(MyMerge, list(merge3,unitsjjoin,sum_app_lf_7,sum_app_lf_30,
                               sum_app_lf_90,sum_app_lf_180,sum_app_lf_360,
                               trans_max_lf,tenure,freq_lf,freq_app,freq_game,
                               freq_lf7,freq_app7,freq_lf30,freq_app30,freq_lf90,
                               freq_app90,freq_lf180,freq_app180,freq_lf360,freq_app360,
                               freq_game7,freq_game30,freq_game90,freq_game180,
                               freq_game360,total_revenue,total_revenue7,
                               total_revenue30,total_revenue90,total_revenue180,
                               total_revenue360,rec_app,rec_lf,rec_down,
                               rec_app7,rec_app30,rec_app90,rec_app180,
                               rec_app360,rec_lf7,rec_lf30,rec_lf90,
                               rec_lf180,rec_lf360
                               ,rec_down7,rec_down30,rec_down90,
                               rec_down180,rec_down360,maxrec2,
                               maxrec27,maxrec230,maxrec290,
                               maxrec2180,maxrec2360,
                               NumHouseChildren,TotalTimeGamePlay,
                               TotalTimeGamePlay7,TotalTimeGamePlay30,
                               TotalTimeGamePlay90,TotalTimeGamePlay180,
                               TotalTimeGamePlay360,NumFemaleChildren,
                               NumMaleChildren,NumGamesPlayed,NumGamesBought,
                               NumGamesPlayed7,NumGamesPlayed30,NumGamesPlayed90,
                               NumGamesPlayed180,NumGamesPlayed360,Favourite_Source,
                               Favourite_Channel,Favourite_Game,Favourite_Source7,
                               Favourite_Channel7,Favourite_Game7,Favourite_Source30,
                               Favourite_Channel30,Favourite_Game30,Favourite_Source90,
                               Favourite_Channel90,Favourite_Game90,Favourite_Source180,
                               Favourite_Channel180,Favourite_Game180,
                               Favourite_Source360,Favourite_Channel360,Favourite_Game360,
                               strengthoffavouritegame
                               
))

Final_Data_Frame=write.csv(new.df[!duplicated(new.df[c('CONTACT_WID')]),],'final.csv')

##########################################Data Frame Building over#################################################################
#############################################################################################
############################ Data Preprocessing #####################################

# Read the main final data frame
data=read.csv("final.csv",na.strings = c("",NA))

# working with the missing values
# Replace the outlier with median valuas mean and median of the data are same.
data$MaxChildAge=ifelse(data$MaxChildAge>100,median(data$MaxChildAge),data$MaxChildAge)
data$MinChildAge=ifelse(data$MinChildAge>100,median(data$MaxChildAge),data$MinChildAge)

# Replace Na with the Median for thr age as there might be 1 children in the house
data$MaxChildAge[is.na(data$MaxChildAge)]=median(data$MaxChildAge,na.rm = T)
data$MinChildAge[is.na(data$MinChildAge)]=median(data$MaxChildAge,na.rm = T)
data$ChildAgeRange=data$MaxChildAge-data$MinChildAge
data$ChildAgeRange=ifelse(data$ChildAgeRange<0,0,data$ChildAgeRange)

# Fill all the NA with 0 in the units as they might have not bought in that particular time.
data[,7:12][is.na(data[,7:12])] <- 0

# Fill the Over all Transaction with 0000-00-00 as no transaction is been made
library(zoo)
data$OveralllastTransaction1=as.character(data$OveralllastTransaction)
data$OveralllastTransaction1[is.na(data$OveralllastTransaction)]="0000-00-00"

#Fill all the column of frequency with 0 where ever there is an NA.
data[,15:34][is.na(data[,15:34])] <- 0
data$Number_Games_Played[is.na(data$Number_Games_Played)]<-0
# Fill all the Column with 0 were ever there is an NA in Revenue,Recency,macrec columns.
data[,35:68][is.na(data[,35:68])] <- 0

# Fill Number of childrens females and males in household of NA value with median value
data$No_of_Childrens[is.na(data$No_of_Childrens)]=median(data$No_of_Childrens,na.rm=T)
data$NumFemaleChildrenHousehold[is.na(data$NumFemaleChildrenHousehold)]=median(data$NumFemaleChildrenHousehold,na.rm=T)
data$NumMaleChildrenHousehold[is.na(data$NumMaleChildrenHousehold)]=median(data$NumMaleChildrenHousehold,na.rm=T)

# Fill the NA in Time value with 0
data[,70:76][is.na(data[,70:76])] <- 0

# Fill the NA in Number of game played and bought value with 0
data[,78:84][is.na(data[,78:84])] <- 0

# Get levels and add "None"
# refactor FavoriteSource to include "None" as a factor level
# and replace NA with "None"
levels = levels(data$FavoriteSource)
levels[length(levels) + 1] = "None"
data$FavoriteSource <- factor(data$FavoriteSource, levels = levels)
data$FavoriteSource[is.na(data$FavoriteSource)] <- "None"


data$FavoriteSource7 <- factor(data$FavoriteSource7, levels = levels)
data$FavoriteSource7[is.na(data$FavoriteSource7)] <- "None"

data$FavoriteSource30 <- factor(data$FavoriteSource30, levels = levels)
data$FavoriteSource30[is.na(data$FavoriteSource30)] <- "None"


data$FavoriteSource90 <- factor(data$FavoriteSource90, levels = levels)
data$FavoriteSource90[is.na(data$FavoriteSource90)] <- "None"


data$FavoriteSource180 <- factor(data$FavoriteSource180, levels = levels)
data$FavoriteSource180[is.na(data$FavoriteSource180)] <- "None"


data$FavoriteSource360 <- factor(data$FavoriteSource360, levels = levels)
data$FavoriteSource360[is.na(data$FavoriteSource360)] <- "None"

# refactor FavoriteChannel to include "None" as a factor level
# and replace NA with "None"
levels2 = levels(data$FavoriteChannel)
levels2[length(levels2) + 1] = "None"
data$FavoriteChannel <- factor(data$FavoriteChannel, levels = levels2)
data$FavoriteChannel[is.na(data$FavoriteChannel)] <- "None"


data$FavoriteChannel7 <- factor(data$FavoriteChannel7, levels = levels2)
data$FavoriteChannel7[is.na(data$FavoriteChannel7)] <- "None"

data$FavoriteChannel30 <- factor(data$FavoriteChannel30, levels = levels2)
data$FavoriteChannel30[is.na(data$FavoriteChannel30)] <- "None"

data$FavoriteChannel90 <- factor(data$FavoriteChannel90, levels = levels2)
data$FavoriteChannel90[is.na(data$FavoriteChannel90)] <- "None"

data$FavoriteChannel180 <- factor(data$FavoriteChannel180, levels = levels2)
data$FavoriteChannel180[is.na(data$FavoriteChannel180)] <- "None"

data$FavoriteChannel360 <- factor(data$FavoriteChannel360, levels = levels2)
data$FavoriteChannel360[is.na(data$FavoriteChannel360)] <- "None"

# refactor FavoriteGame to include "None" as a factor level
# and replace NA with "None"
levels3 = levels(data$FavoriteGame)
levels3[length(levels3) + 1] = "None"
data$FavoriteGame <- factor(data$FavoriteGame, levels = levels3)
data$FavoriteGame[is.na(data$FavoriteGame)] <- "None"


data$FavoriteGame7 <- factor(data$FavoriteGame7, levels = levels3)
data$FavoriteGame7[is.na(data$FavoriteGame7)] <- "None"


data$FavoriteGame30 <- factor(data$FavoriteGame30, levels = levels3)
data$FavoriteGame30[is.na(data$FavoriteGame30)] <- "None"


data$FavoriteGame90 <- factor(data$FavoriteGame90, levels = levels3)
data$FavoriteGame90[is.na(data$FavoriteGame90)] <- "None"


data$FavoriteGame180 <- factor(data$FavoriteGame180, levels = levels3)
data$FavoriteGame180[is.na(data$FavoriteGame180)] <- "None"


data$FavoriteGame360 <- factor(data$FavoriteGame360, levels = levels3)
data$FavoriteGame360[is.na(data$FavoriteGame360)] <- "None"

levels4 = levels(data$Strength_of_FavoriteGame)
levels4[length(levels4) + 1] = "None"
data$Strength_of_FavoriteGame <- factor(data$Strength_of_FavoriteGame, levels = levels4)
data$Strength_of_FavoriteGame[is.na(data$Strength_of_FavoriteGame)] <- "None"

data=subset(data,select = -c(OveralllastTransaction))
##################################################################################################
####################### Univariate Analysis and Plots ####################################
library(ggplot2)
ggplot(data,aes(x=Country))+geom_bar()+ggtitle("Number of Unique Users in Different Countries")
ggplot(data,aes(x=Country,fill=FavoriteChannel))+geom_bar()+ggtitle("Favourite Channel Of Transaction")
ggplot(data,aes(x=Country,fill=FavoriteSource))+geom_bar()+ggtitle("Favourite Source")

ggplot(data,aes(x=NumMaleChildrenHousehold,y=NumFemaleChildrenHousehold))+geom_bin2d(col="red")+ggtitle("Number of Male childrens to Female childrens in the Household")+
facet_wrap(~Country)

ggplot(data,aes(x=No_of_Childrens,fill=FavoriteSource))+geom_bar()+ggtitle("Number of Childrens in the House Hold in Different Country")+
  facet_wrap(~Country)
  
ggplot(data,aes(x=No_of_Childrens,fill=FavoriteSource))+geom_bar()+ggtitle("Number of Childrens in the House Hold in Different Country")+
  facet_wrap(~Country)

ggplot(data,aes(x=UNITS))+geom_bar()+ggtitle("Revenue Plot")+
  facet_wrap(~Country)


# Data by country and UNITS
data_country_units=subset(data,select = c(Country,UNITS))
data_country_units=aggregate(UNITS~Country,data_country_units,FUN=sum)
plot.table(data_country_units)
country_units.plot <- barplot(data_country_units$UNITS, names.arg = data_country_units$Country,
                      ylab="Units",col=rainbow(20), xlab = "Country",las=1,main ="No of Units Sold in each Country",ylim = c(0,100000))
# Most Frequently Played Games 
# calculate frequencies
tab <- table(data$FavoriteGame)
# sort
tab_s <- data.frame(sort(tab))
# extract 10 most frequent nationalities
top5 <- data.frame(tail(tab_s, 5))
top5=top5[1:4,]
# plot for the top played games ######################### 
ggplot(top5,aes(Var1,Freq))+geom_count()
barplot(top5$Freq,names.arg = top5$Var1,ylab = "No of Times the Game Played by Different Users",cex.names=0.8,xlab = "Name of the Game",main = "Top 5 Games",col = 'Red')
#################################################################################################################################
###################### Selecting the Top Variables from the data frame ################################################################################################################ 
##################### Final data variables ######################################3


library(corrplot)
numerichopmonk=data[sapply(data,is.numeric)]
corelation=cor(numerichopmonk)
sum(is.na(numerichopmonk))

zdf <- as.data.frame(as.table(corelation))
write.csv(zdf,'cor2.csv')


# Range of child is corelated with the maxchild age with 80 percent corelation
# As we can see that co relations keeps on increasing when the time span gets increased so we will
# be taking only the time frame of 7 and 90 as they are not highly corelated with previous time span

#The Final variale took based on the corrlation between the variables and the variable
'''
CONTACT_WID,Country,Revenue360,UNITS,FrequencyLF,Revenue180,
Number_Games_Played,FreqGamePlay,Frequencylf180,
maxRecencyCum360,RecencyAPP,Revenue7,No_of_Childrens,
TenureDays,Recencydown180,MaxChildAge,Time,FreqGamePlay7,
FreqGamePlay30,Recencydown,Time180,Time90,MinChildAge,
minRecencyCum,FavoriteSource,FavoriteChannel,FavoriteGameBin,
FavoriteSource30,FavoriteChannel30,FavoriteGameBin30,
FavoriteSource90,FavoriteChannel90,FavoriteGameBin90,
Strength-of- FavoriteGame,TotalRevenueGenerated
'''

dput(names(data)) #Extracts all the variables with names in the predeined formats.

'''hopmonkdata=subset(data,select = c(CONTACT_WID,Country,Revenue360,UNITS,FrequencyLF,Revenue180,
                                   Number_Games_Played,FreqGamePlay,Frequencylf180,
                                   maxRecencyCum360,RecencyAPP,Revenue7,No_of_Childrens,
                                   TenureDays,Recencydown180,MaxChildAge,Time,FreqGamePlay7,
                                   Recencydown,Time180,Time90,MinChildAge,
                                   minRecencyCum,FavoriteSource,FavoriteChannel,FavoriteGame,
                                   FavoriteSource30,FavoriteChannel30,FavoriteSource180,FavoriteChannel180,
                                   FavoriteGame30,FavoriteGame180,TotalRevenueGenerated))
'''
hopmonkdata=subset(data,select=c(CONTACT_WID,Country, MaxChildAge, MinChildAge, UNITS,UNITS360,TenureDays,
                                 FrequencyLF, 
                                   FrequencyApp,FreqGamePlay,Frequencylf30,FrequencyApp30,
                                   RecencyAPP, Recencylf, 
                                   Recencylf30, 
                                   Recencylf360, 
                                   maxRecencyCum,minRecencyCum, 
                                   No_of_Childrens, 
                                 FrequencyApp360, 
                                 FreqGamePlay360,TotalRevenueGenerated, 
                                 Revenue180, Revenue360,
                                   FavoriteSource, FavoriteChannel, 
                                   FavoriteGame, 
                                   FavoriteSource30, FavoriteChannel30, FavoriteGame30,
                                   FavoriteSource180, FavoriteChannel180, 
                                   FavoriteGame180, FavoriteSource360, FavoriteChannel360, 
                                   FavoriteGame360, Strength_of_FavoriteGame
))
write.csv(hopmonkdata,"hopmonk.csv")

