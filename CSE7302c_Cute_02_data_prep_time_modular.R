source('helper_funcs.R')
start_time <- proc.time()
hopmonk_data_2 <- data.frame(CONTACT_WID=unique(cust_owner$CONTACT_WID))
dim(hopmonk_data_2)

list_dates = c("2012-01-02", "2012-01-26","2012-03-26","2012-06-23", "2012-12-26")
df_names = c("Units7","Units30","Units90","Units180","Units360")
list_dfs <- list()

#___________UNITS-------------
for(i in seq(1,length(list_dates))){
  #filter by date
  purc_app_filter <- subset(purc_app,TRANSACTION_DATE<list_dates[i])
  purc_lf_filter <- subset(purc_lf,TRANSACTION_DT<list_dates[i])
  #calc feature
  tot_units<-calc_units(purc_app_filter,purc_lf_filter)
  #subsetting and renaming cols
  tot_units <- subset(tot_units,select = c(CONTACT_WID,UNITS))
  colnames(tot_units)[colnames(tot_units)=="UNITS"] = df_names[i]
  #merge with hopmonk_data_2
  hopmonk_data_2 <- merge(hopmonk_data_2,tot_units,by="CONTACT_WID",all.x = TRUE)
  if(dim(hopmonk_data_2)[1] != 56660){print("\n---!*Duplicates!*---\n")}
}
#head(hopmonk_data_2)

#--------------FrequencyLF-------------------
freq_lf_names = c("FrequencyLF7","FrequencyLF30","FrequencyLF90","FrequencyLF180","FrequencyLF360")

freq_app_names = c("FrequencyApp7","FrequencyApp30","FrequencyApp90","FrequencyApp180","FrequencyApp360")

for(i in seq(1,length(list_dates))){
  #--------------FrequencyLF-------------------
  
  #filter
  purc_lf_filter <- subset(purc_lf,TRANSACTION_DT<list_dates[i])
  #calc feature
  freq_lf <- calc_freq_lf(purc_lf_filter)
  #subsetting and renaming cols
  colnames(freq_lf)[colnames(freq_lf)=="FrequencyLF"] = freq_lf_names[i]
      
  #merge with hopmonk_data_2
  hopmonk_data_2 <- merge(hopmonk_data_2,freq_lf,by="CONTACT_WID",all.x = TRUE)
  
  #replace NA
  hopmonk_data_2[,freq_lf_names[i]][is.na(hopmonk_data_2[,freq_lf_names[i]])] <- 0
  
  
  #--------------FrequencyApp-------------------
  #filter
  purc_app_filter <- subset(purc_app,TRANSACTION_DATE<list_dates[i])
  #calc feature
  freq_app <- calc_freq_app(purc_app_filter)
  #subsetting and renaming cols
  colnames(freq_app)[colnames(freq_app)=="FrequencyApp"] = freq_app_names[i]
  #merge with hopmonk_data_2
  hopmonk_data_2 <- merge(hopmonk_data_2,freq_app,by="CONTACT_WID",all.x = TRUE)
  #replace NA
  hopmonk_data_2[,freq_app_names[i]][is.na(hopmonk_data_2[,freq_app_names[i]])] <- 0
  
  if(dim(hopmonk_data_2)[1] != 56660){print("\n---!*Duplicates!*---\n")}
}

#head(hopmonk_data_2)

#---------------#FreqGamePlay
freq_game_play_names <- c("FreqGamePlay7","FreqGamePlay30","FreqGamePlay90","FreqGamePlay180","FreqGamePlay360")
for(i in seq(1,length(list_dates))){
  #filter
  game_actv_filter <- subset(game_actv,TITLE_NOMIN_DT<list_dates[i])
  #calc feature
  freq_game_play <- calc_freq_game_play(game_actv_filter)
  #subsetting and renaming cols
  colnames(freq_game_play)[colnames(freq_game_play)=="freq_game_play"] = freq_game_play_names[i]
  #merge with hopmonk_data_2
  hopmonk_data_2 <- merge(hopmonk_data_2,freq_game_play,by="CONTACT_WID",all.x = TRUE)
  
  if(dim(hopmonk_data_2)[1] != 56660){print("\n---!*Duplicates!*---\n")}
  
}
#head(hopmonk_data_2)
#------------------TotalRevenueGenerated------
revenue_names <- c("Revenue7","Revenue30","Revenue90","Revenue180","Revenue360")
for(i in seq(1,length(list_dates))){
  #filter
  purc_app_filter <- subset(purc_app,TRANSACTION_DATE<list_dates[i])
  purc_lf_filter <- subset(purc_lf,TRANSACTION_DT<list_dates[i])
  #calc feature
  sum_app_lf <- calc_tot_rev_gen(purc_app_filter,purc_lf_filter)
  #subsetting and renaming cols
  sum_app_lf = subset(sum_app_lf,select = c(CONTACT_WID,TOT_REV_GEN))
  colnames(sum_app_lf)[colnames(sum_app_lf)=="TOT_REV_GEN"] = revenue_names[i]
  #merge with hopmonk_data_2
  hopmonk_data_2 <- merge(hopmonk_data_2,sum_app_lf,by="CONTACT_WID",all.x = TRUE)
  
  if(dim(hopmonk_data_2)[1] != 56660){print("\n---!*Duplicates!*---\n")}
}
#head(hopmonk_data_2,2)

#--------------------RecencyApp
rec_app_names <- c("RecencyApp7","RecencyApp30","RecencyApp90","RecencyApp180","RecencyApp360")
for(i in seq(1,length(list_dates))){
  #filter
  purc_app_filter <- subset(purc_app,TRANSACTION_DATE<list_dates[i])
  #calc feature
  recency_app <- calc_rec_app(purc_app_filter,list_dates[i])
  #subsetting and renaming cols
  colnames(recency_app)[colnames(recency_app)=="recency_app"] = rec_app_names[i]
  #merge with hopmonk_data_2
  hopmonk_data_2 <- merge(hopmonk_data_2,recency_app,by="CONTACT_WID",all.x = TRUE)
  
  if(dim(hopmonk_data_2)[1] != 56660){print("\n---!*Duplicates!*---\n")}
}
#head(hopmonk_data_2,2)

#--------------------RecencyLF
rec_lf_names <- c("RecencyLF7","RecencyLF30","RecencyLF90","RecencyLF180","RecencyLF360")
for(i in seq(1,length(list_dates))){
  #filter
  purc_lf_filter <- subset(purc_lf,TRANSACTION_DT<list_dates[i])
  #calc feature
  recency_lf <- calc_rec_lf(purc_lf_filter,list_dates[i])
  #subsetting and renaming cols
  colnames(recency_lf)[colnames(recency_lf)=="recency_lf"] = rec_lf_names[i]
  #merge with hopmonk_data_2
  hopmonk_data_2 <- merge(hopmonk_data_2,recency_lf,by="CONTACT_WID",all.x = TRUE)
  
  
  
  if(dim(hopmonk_data_2)[1] != 56660){print("\n---!*Duplicates!*---\n")}
}
#head(hopmonk_data_2,2)
#-------------------maxRecencyCum,minRecencyCum
hopmonk_data_2$maxRecencyCum7 <- max(hopmonk_data_2$RecencyApp7,hopmonk_data_2$RecencyLF7,hopmonk_data_2$RecencyDown7)
hopmonk_data_2$minRecencyCum7 <- min(hopmonk_data_2$RecencyApp7,hopmonk_data_2$RecencyLF7,hopmonk_data_2$RecencyDown7)

hopmonk_data_2$maxRecencyCum30 <- max(hopmonk_data_2$RecencyApp30,hopmonk_data_2$RecencyLF30,hopmonk_data_2$RecencyDown30)
hopmonk_data_2$minRecencyCum30 <- min(hopmonk_data_2$RecencyApp30,hopmonk_data_2$RecencyLF30,hopmonk_data_2$RecencyDown30)

hopmonk_data_2$maxRecencyCum90 <- max(hopmonk_data_2$RecencyApp90,hopmonk_data_2$RecencyLF90,hopmonk_data_2$RecencyDown90)
hopmonk_data_2$minRecencyCum90 <- min(hopmonk_data_2$RecencyApp90,hopmonk_data_2$RecencyLF90,hopmonk_data_2$RecencyDown90)

hopmonk_data_2$maxRecencyCum180 <- max(hopmonk_data_2$RecencyApp180,hopmonk_data_2$RecencyLF180,hopmonk_data_2$RecencyDown180)
hopmonk_data_2$minRecencyCum180 <- min(hopmonk_data_2$RecencyApp180,hopmonk_data_2$RecencyLF180,hopmonk_data_2$RecencyDown180)

hopmonk_data_2$maxRecencyCum360 <- max(hopmonk_data_2$RecencyApp360,hopmonk_data_2$RecencyLF360,hopmonk_data_2$RecencyDown360)
hopmonk_data_2$minRecencyCum360 <- min(hopmonk_data_2$RecencyApp360,hopmonk_data_2$RecencyLF360,hopmonk_data_2$RecencyDown360)

#head(hopmonk_data_2,2)

#--------------------Recency Down
rec_down_names <- c("RecencyDown7","RecencyDown30","RecencyDown90","RecencyDown180","RecencyDown360")
for(i in seq(1,length(list_dates))){
  #filter
  app_dwnld_filter <- subset(app_dwnld,NOMIN_DT<list_dates[i])
  #calc feature
  recency_dwnld <- calc_rec_down(app_dwnld_filter,list_dates[i])
  #subsetting and renaming cols
  colnames(recency_dwnld)[colnames(recency_dwnld)=="recency_dwnld"] = rec_down_names[i]
  #merge with hopmonk_data_2
  hopmonk_data_2 <- merge(hopmonk_data_2,recency_dwnld,by="CONTACT_WID",all.x = TRUE)
  if(dim(hopmonk_data_2)[1] != 56660){print("\n---!*Duplicates!*---\n")}
}
#head(hopmonk_data_2,2)


#TotalTimeGamePlay----------------------------
time_game_names <- c("TotalTimeGame7","TotalTimeGame30","TotalTimeGame90","TotalTimeGame180","TotalTimeGame360")
for(i in seq(1,length(list_dates))){
  #filter
  game_actv_filter <- subset(game_actv,TITLE_NOMIN_DT<list_dates[i])
  #calc feature
  tot_game_play <- calc_time_game(game_actv_filter)
  #subsetting and renaming cols
  colnames(tot_game_play)[colnames(tot_game_play)=="tot_game_play"] = time_game_names[i]
  #merge with hopmonk_data_2
  hopmonk_data_2 <- merge(hopmonk_data_2,tot_game_play,by="CONTACT_WID",all.x = TRUE)
  
  
  if(dim(hopmonk_data_2)[1] != 56660){print("\n---!*Duplicates!*---\n")}
}
#head(hopmonk_data_2,2)

#FavoriteSource
#FavoriteSource7
a<-hopmonk_data_2[,c("CONTACT_WID","FrequencyLF7","FrequencyApp7")]
a$fav_source7 <- ifelse(a$FrequencyApp7>a$FrequencyLF7,"App7","LF7")
a[is.na(a)] <- 0

a<-subset(a,select = c(CONTACT_WID,fav_source7))
hopmonk_data_2<-merge(hopmonk_data_2,a,by="CONTACT_WID",all.x = TRUE)

#FavoriteSource30
a<-hopmonk_data_2[,c("CONTACT_WID","FrequencyLF30","FrequencyApp30")]
a$fav_source30 <- ifelse(a$FrequencyApp30>a$FrequencyLF30,"App30","LF30")
a[is.na(a)] <- 0

a<-subset(a,select = c(CONTACT_WID,fav_source30))
hopmonk_data_2<-merge(hopmonk_data_2,a,by="CONTACT_WID",all.x = TRUE)

#FavoriteSource90
a<-hopmonk_data_2[,c("CONTACT_WID","FrequencyLF90","FrequencyApp90")]
a$fav_source90 <- ifelse(a$FrequencyApp90>a$FrequencyLF90,"App90","LF90")
a[is.na(a)] <- 0

a<-subset(a,select = c(CONTACT_WID,fav_source90))
hopmonk_data_2<-merge(hopmonk_data_2,a,by="CONTACT_WID",all.x = TRUE)
#FavoriteSource180
a<-hopmonk_data_2[,c("CONTACT_WID","FrequencyLF180","FrequencyApp180")]
a$fav_source180 <- ifelse(a$FrequencyApp180>a$FrequencyLF180,"App180","LF180")
a[is.na(a)] <- 0

a<-subset(a,select = c(CONTACT_WID,fav_source180))
hopmonk_data_2<-merge(hopmonk_data_2,a,by="CONTACT_WID",all.x = TRUE)
#FavoriteSource360
a<-hopmonk_data_2[,c("CONTACT_WID","FrequencyLF360","FrequencyApp360")]
a$fav_source360 <- ifelse(a$FrequencyApp360>a$FrequencyLF360,"App360","LF360")
a[is.na(a)] <- 0

a<-subset(a,select = c(CONTACT_WID,fav_source360))
hopmonk_data_2<-merge(hopmonk_data_2,a,by="CONTACT_WID",all.x = TRUE)

if(dim(hopmonk_data_2)[1] != 56660){print("\n---!*Duplicates!*---\n")}

#head(hopmonk_data_2,2)
#------------------------------------

#NumGamesPlayed
num_game_names <- c("NumGames7","NumGames30","NumGames90","NumGames180","NumGames360")
for(i in seq(1,length(list_dates))){
  #filter
  game_actv_filter <- subset(game_actv,TITLE_NOMIN_DT<list_dates[i])
  #calc feature
  num_game_plyd <- calc_num_game(game_actv_filter)
  #subsetting and renaming cols
  colnames(num_game_plyd)[colnames(num_game_plyd)=="num_game_plyd"] = num_game_names[i]
  #merge with hopmonk_data_2
  hopmonk_data_2 <- merge(hopmonk_data_2,num_game_plyd,by="CONTACT_WID",all.x = TRUE)
  if(dim(hopmonk_data_2)[1] != 56660){print("\n---!*Duplicates!*---\n")}
}
#head(hopmonk_data_2,2)

#------------------------------------

#FavoriteChannel 
fav_channel_names <- c("FavChannel7","FavChannel30","FavChannel90","FavChannel180","FavChannel360")
for(i in seq(1,length(list_dates))){
  #filter
  purc_app_filter <- subset(purc_app,TRANSACTION_DATE<list_dates[i])
  #calc feature
  fav_channel <- calc_fav_channel(purc_app_filter)
  #subsetting and renaming cols
  colnames(fav_channel)[colnames(fav_channel)=="CHANNEL_DESCRIPTION"] = fav_channel_names[i]
  #merge with hopmonk_data_2
  hopmonk_data_2 <- merge(hopmonk_data_2,fav_channel,by="CONTACT_WID",all.x = TRUE)
  
  if(dim(hopmonk_data_2)[1] != 56660){print("\n---!*Duplicates!*---\n")}
}
#head(hopmonk_data_2,2)
hopmonk_data_bkup_2 <- hopmonk_data_2

#Fav game bin
fav_game_names <- c("FavGameBin7","FavGameBin30","FavGameBin90","FavGameBin180","FavGameBin360")
for(i in seq(1,length(list_dates))){
  #filter
  game_actv_filter <- subset(game_actv,TITLE_NOMIN_DT<list_dates[i])
  #calc feature
  fav_gam<-calc_fav_game(game_actv_filter)
  #subsetting and renaming cols
  colnames(fav_gam)[colnames(fav_gam)=="X_GAME_NM"] = fav_game_names[i]
  #print(paste0("dups_count:",sum(duplicated(fav_gam$CONTACT_WID))))
  print(fav_game_names[i])
  #merge with hopmonk_data_2
  hopmonk_data_2 <- merge(hopmonk_data_2,fav_gam,by="CONTACT_WID",all.x = TRUE)
  
  if(dim(hopmonk_data_2)[1] != 56660){print("\n---!*Duplicates!*---\n")}
}
dim(hopmonk_data_2)
#head(hopmonk_data_2,2)

end_time=proc.time()
print(end_time-start_time)

"> dim(hopmonk_data_2)
[1] 56660    71"






