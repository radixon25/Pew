
# Import Data, and Create Dataframes
path = here('Data','union.csv')
df <- read.csv(path)
#Union Rates
df_ur <- as_tibble_col(matrix(t(df[-c(1,2)]), ncol = 1),column_name = 'UR') %>%
  filter_at(vars(UR),all_vars(!is.na(.)))
df_ur <- tail(df_ur,-55)
l <- c(state.name,'District of Columbia')
l_dup <- sort(rep(l,times = 55))
df_ur$State <- l_dup
y <- sort(c(1964:2018), decreasing = TRUE)
y <- rep(y,times = 51)
df_ur$year = y
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
#TWFE of Union Rate on right to work laws
tfe_ur_dummy <- plm( UR ~ rtw, data = df_ur, index = c('State','year'),model = 'within',effect =  'twoways' )
tfe_ur_dummy
tfe_ur_full <- plm( UR ~ rtw + rpci, data = df_ur, index = c('State','year'),model = 'within',effect =  'twoways' )
tfe_ur_full
summary(tfe_ur_full)
#TWFE With State representation
tfe_urf_full <- plm( UR ~ rtw + rpci + dem_leg + rep_leg + bi_leg, data = df_urf, index = c('State','year'),model = 'within',effect =  'twoways' )
summary(tfe_urf_full)
#Bacon Decomp of TWFE
bacon_ur <- bacon(UR~rtw, data = df_ur, id_var = 'State',time_var = 'year')
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
for (i in 1:nrow(df_ur)) {
  if (df_ur$State[i] %in% rtw$State){
    df_ur$Group[i] = rtw$Year[rtw$State == df_ur$State[i]]
  }
}
#Create State ID for CS
df_ur$stid = 0
for (i in 1:nrow(df_ur)) {
  df_ur$stid[i] = which(l == df_ur$State[i])[1]
}
#CS Regression W/ Group aggregation
cs_ur <- att_gt(yname = 'UR', tname ='year', idname = 'stid', gname = 'Group', data = df_ur)
cs_ur_group <- aggte(cs_ur,type = 'group')
cs_ur_group
cs_urf <- att_gt(yname = 'UR', tname ='year', idname = 'stid', gname = 'Group', data = df_urf,xformla = ~ rpci + dem_leg + rep_leg)
cs_urf_group <- aggte(cs_urf, type = 'group')
cs_urf_group
#TWFE With State representation
tfe_urf_full <- plm( UR ~ rtw + rpci + dem_leg + rep_leg data = df_urf, index = c('State','year'),model = 'within',effect =  'twoways' )
summary(tfe_urf_full)
