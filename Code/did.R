# Import Data, and Create Dataframes
path = here('Data','urd2.csv')
df_ur <- read.csv(path)

St <- state.name

path2 = here('Data','ud.csv')
df <- read.csv(path2) 
#Union Rates
df_ud <- as_tibble_col(matrix(t(df[-c(1,2)]), ncol = 1),column_name = 'UD') %>%
  filter_at(vars(UD),all_vars(!is.na(.)))
#97 employment data
emp_97 <- read.csv(here('Data','emp_97_perc.csv')) %>%
  rename(State = 1)



#df_ur <- tail (df_ur,-55)
l <- c('A',state.name,'District of Columbia')
l_dup <- sort(rep(l,times = 42))
df_ud$State <- l_dup
y <- sort(c(1977:2018), decreasing = TRUE)
y <- rep(y,times = 52)
df_ud$year = y

## Right to Work
rtw <- read.csv(here('Data','rtw.csv')) %>%
  rename(State = 1)
#Real Per Capita Income
rpci <- read.csv(here('Data','realpci.csv'))
df_ur$rpci = 0
for(i in 1:nrow(df_ur)){
  st <- df_ur$State[i]
  yr <- df_ur$year[i]
  col <- which(colnames(rpci) == st)
  if (df_ur$State[i] %in% colnames(rpci)) {
    df_ur$rpci[i] = rpci[yr-1963,col]
  }
}
df_ur$rtw = 0
for (i in 1:nrow(df_ur)) {
  st <- df_ur$State[i]
  yr <- df_ur$year[i]
  rtw_year <- rtw$Year[rtw$State == st]
  if (st %in% rtw$State) {
    if (yr > rtw$Year[rtw$State == st]) {
      df_ur$rtw[i] = 1
    }
  }
}
#Import Legislative Data
leg <- read.csv(here('Data','legis_data.csv'))%>%
  rename(State = 1, '1978' = 2, '1980' = 3, '1982' = 4, '1984' = 5, '1986' = 6, '1988' = 7,
         '1990' = 8, '1992' = 9, '1994' = 10, '1996' = 11, '1998' = 12, '2000' = 13, '2002' = 14,
         '2004' = 15, '2006' = 16, '2008' = 17, '2010' = 18, '2012' = 19, '2014' = 20 )
leg_yr <- colnames(leg)[2:20]
df_ur$dem_leg = 0
df_ur$rep_leg = 0
df_ur$bi_leg = 0
for (i in 1:nrow(df_ur)){
  lyr = df_ur$year[i]
  if (lyr >=1978){
    n = tail(which(leg_yr <= df_ur$year[i]), n=1)
    m = n+1
    st = df_ur$State[i]
    x = which(leg$State == st)
    leg_st = leg[leg$State == st,]
    if ((st %in% leg$State) & (st != 'Nebraska')){
      if (pull(leg_st[m]) == 'D'){
        df_ur$dem_leg[i] = 1
      }
      else if (pull(leg_st[m]) == 'R'){
        df_ur$rep_leg[i] = 1
      }
      else if (pull(leg_st[m]) == 'S'){
        df_ur$bi_leg[i] = 1
      }
    }
  }
}
df_urf <- df_ur[df_ur$year > 1978,]
df_urf <- df_urf[df_urf$year < 2015,]
df_udf <- df_ud[df_ud$year > 1978,]
df_udf <- df_udf[df_udf$year < 2015,]
did_df <- merge(df_urf,df_udf, by  = c('year','State'))
did_df <- merge(did_df,emp_97, by = c('State','year'), all.x = TRUE)
did_df$fr <- (did_df$UD-did_df$UR)/did_df$UD
did_df$stid <- 0
for (i in 1:nrow(did_df)){
  if (did_df$State[i] != 'District of Columbia'){
    rst <- did_df$State[i]
    id <- which(St == did_df$State[i])
    did_df$stid[i] <- id}
  else if (did_df$State[i] == 'District of Columbia'){
    did_df$stid[i] == 51
  }
}
did_df$id <- as.numeric(did_df$stid)

#Create State ID for CS

#format 1997 employment data

#TWFE of Union Rate on right to work laws
tfe_ur_dummy <- plm( fr ~ rtw, data = did_df, index = c('State','year'),model = 'within',effect =  'twoways' )
tfe_ur_dummy
tfe_ur_full <- plm( fr ~ rtw + rpci, data = did_df, index = c('State','year'),model = 'within',effect =  'twoways' )
tfe_ur_full
summary(tfe_ur_full)
#TWFE With State representation
tfe_urf_full <- plm( fr~ rtw + rpci + dem_leg + rep_leg + bi_leg, data = did_df, index = c('State','year'),model = 'within',effect =  'twoways' )
summary(tfe_urf_full)
#Bacon Decomp of TWFE
bacon_ur <- bacon(UR~rtw, data = did_df, id_var = 'State',time_var = 'year')
bacon_ur$weighted_est <- bacon_ur$estimate * bacon_ur$weight
bacon_ur_sum = bacon_ur %>%
  group_by(type) %>%
  summarize(weight = sum(weight),estimate = sum(weighted_est))
bacon_ur_sum$estimate = bacon_ur_sum$estimate/bacon_ur_sum$weight
bacon_ur_sum
feb_ur <- sum(bacon_ur_sum$weight * bacon_ur_sum$estimate)
#Bacon Decomp matches TWFE
feb_ur
df_ur$Group = 0
#Create Groups for Callaway Sant'anna (CS)
rollout <- did_df %>%
  subset(rtw == 1)%>%
  group_by(State)%>%
  summarize(Group = min(year))

did_df <- merge(did_df,rollout, by = c('State'),all.x = TRUE)%>%
did_df$Group[is.na(did_df$Group)]<- 0 
did_df <- subset(did_df,State != 'A')



#CS Regression W/ Group aggregation
cs_ur <- att_gt(yname = 'fr', tname ='year', idname = 'stid', gname = 'Group', data = did_df, panel = FALSE)
cs_ur_group <- aggte(cs_ur,type = 'group')
cs_ur_group

#TWFE With State representation
tfe_urf_full <- plm( UR ~ rtw + rpci + dem_leg + rep_leg data = did_df, index = c('State','year'),model = 'within',effect =  'twoways' )
summary(tfe_urf_full)



