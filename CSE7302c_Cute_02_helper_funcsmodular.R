calc_units <- function(purc_app,purc_lf){
  purc_app_sum_units <- purc_app%>%group_by(CONTACT_WID)%>%summarise(purc_app_sum_units=sum(UNITS))
  
  purc_lf_sum_units <- purc_lf%>%group_by(CONTACT_WID)%>%summarise(purc_lf_sum_units=sum(UNITS))
  tot_units <- merge(purc_app_sum_units,purc_lf_sum_units,by="CONTACT_WID",all.x = TRUE)
  #NAs to zero before sum
  tot_units$purc_app_sum_units[is.na(tot_units$purc_app_sum_units)] <- 0
  tot_units$purc_lf_sum_units[is.na(tot_units$purc_lf_sum_units)] <- 0
  
  tot_units$UNITS <- tot_units$purc_app_sum_units + tot_units$purc_lf_sum_units
  return(tot_units)
}

#FrequencyLF
calc_freq_lf<-function(purc_lf){
  freq_lf <- purc_lf%>%group_by(CONTACT_WID)%>%summarise(FrequencyLF=n())
  return(freq_lf)
}

#FrequencyApp
calc_freq_app<-function(purc_app){
  freq_app <- purc_app%>%group_by(CONTACT_WID)%>%summarise(FrequencyApp=n())
  return(freq_app)
}

#FreqGamePlay
calc_freq_game_play<-function(game_actv){
  freq_game_play <- game_actv%>%group_by(CONTACT_WID)%>%summarise(freq_game_play=sum(ATMP_CNT))
  return(freq_game_play)
}

#TotalRevenueGenerated
calc_tot_rev_gen<-function(purc_app,purc_lf){
  sum_app <- purc_app%>%group_by(CONTACT_WID)%>%summarise(sum_app=sum(AMOUNT_USD))
  sum_lf <- purc_lf%>%group_by(CONTACT_WID)%>%summarise(sum_lf=sum(AMOUNT))
  sum_app_lf = merge(sum_app,sum_lf,by="CONTACT_WID")
  #set NAs to 0
  sum_app_lf[is.na(sum_app_lf)]<-0
  sum_app_lf$TOT_REV_GEN <- sum_app_lf$sum_app+sum_app_lf$sum_lf
  return(sum_app_lf)
}

#RecencyApp
calc_rec_app<-function(purc_app,from_Date){
  recency_app <- purc_app%>%group_by(CONTACT_WID)%>%summarise(recency_app=max(TRANSACTION_DATE))
  recency_app$recency_app <- as.numeric(difftime(as.Date(from_Date),recency_app$recency_app,units = "days"))
  return(recency_app)
}

#RecencyLF
calc_rec_lf <- function(purc_lf,from_Date){
  recency_lf <- purc_lf%>%group_by(CONTACT_WID)%>%summarise(recency_lf=max(TRANSACTION_DT))
  recency_lf$recency_lf <- as.numeric(difftime(as.Date(from_Date),recency_lf$recency_lf,units = "days"))
  return(recency_lf)
}

#Recency Down
calc_rec_down <- function(app_dwnld,from_Date){
  recency_dwnld <- app_dwnld%>%group_by(CONTACT_WID)%>%summarise(recency_dwnld=max(NOMIN_DT))
  recency_dwnld$recency_dwnld <- as.numeric(difftime(as.Date(from_Date),recency_dwnld$recency_dwnld,units = "days"))
  return(recency_dwnld)
  
}

#TotalTimeGamePlay
calc_time_game <-function(game_actv){
  tot_game_play <- game_actv%>%group_by(CONTACT_WID)%>%summarise(tot_game_play=sum(ACT_TME_SPN_QTY))
  return(tot_game_play)
}

#NumGamesPlayed

calc_num_game <- function(game_actv){
  num_game_plyd <- game_actv[complete.cases(game_actv$X_GAME_NM),]%>%group_by(CONTACT_WID)%>%summarise(num_game_plyd=n())
  return(num_game_plyd)
}

#FavoriteChannel
calc_fav_channel <- function(purc_app){
  summary <- purc_app %>% group_by(CONTACT_WID,CHANNEL_DESCRIPTION) %>% summarise(cnt=n())%>%mutate(freq=cnt/sum(cnt))
  head(summary)
  fav_channel <- summary%>%group_by(CONTACT_WID)%>%mutate(fav=ifelse(freq==max(freq),"fav","not"))
  fav_channel <-subset(fav_channel,fav=="fav",select = c(CONTACT_WID,CHANNEL_DESCRIPTION))
  
  #rows with equal frfeq cause duplicates
  fav_channel<-fav_channel%>%distinct(CONTACT_WID,.keep_all = TRUE)
  return(fav_channel)
}
#FavGameBin
calc_fav_game <- function(game_actv){
  tot_game_time <- game_actv%>% group_by(CONTACT_WID,X_GAME_NM) %>% summarise(tot_time=sum(ACT_TME_SPN_QTY
  ))%>%group_by(CONTACT_WID)%>%mutate(fav=ifelse(tot_time==max(tot_time),"fav","not"))
  fav_gam<-subset(tot_game_time,fav=="fav",select = c(CONTACT_WID,X_GAME_NM))
  #rows with equal frfeq cause duplicates
  fav_gam<-fav_gam%>%distinct(CONTACT_WID,.keep_all = TRUE)

  return(fav_gam)
}