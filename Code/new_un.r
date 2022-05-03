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
ur_synth <- subset(df_urf,!(df_urf$State %in% c('A','Alabama','Arizona','Arkansas','Florida','Georgia','Idaho','Indiana','Iowa','Kentucky','Louisiana','Michigan','Mississippi','Nebraska','Nevada','North Carolina','North Dakota','South Carolina','South Dakota','Tennessee','Texas','Utah','Virginia','West Virginia')))
ud_synth <- subset(df_udf,!(df_udf$State %in% c('A','Alabama','Arizona','Arkansas','Florida','Georgia','Idaho','Indiana','Iowa','Kentucky','Louisiana','Michigan','Mississippi','Nebraska','Nevada','North Carolina','North Dakota','South Carolina','South Dakota','Tennessee','Texas','Utah','Virginia','West Virginia')))
df_synth <- merge(ur_synth,ud_synth, by  = c('year','State'))
df_synth <- merge(df_synth,emp_97, by = c('State','year'), all.x = TRUE)
df_synth$fr <- (df_synth$UD-df_synth$UR)/df_synth$UD
df_synth$stid <- 0
for (i in 1:nrow(df_synth)){
  if (df_synth$State[i] != 'District of Columbia'){
  rst <- df_synth$State[i]
  id <- which(St == df_synth$State[i])
  df_synth$stid[i] <- id}
  else if (df_synth$State[i] == 'District of Columbia'){
    df_synth$stid[i] == 51
  }
  
}
df_synth$Ag <- as.numeric(df_synth$Ag)

d_synth <- as.data.frame(df_synth)
dataprep_out <- dataprep(
  foo = df_synth,
  predictors = c('rpci','rep_leg'),
  predictors.op = c("mean"),
  dependent = "fr",
  unit.variable = "stid",
  time.variable = "year",
  time.predictors.prior = 1985:2003,
  special.predictors = list(
    list("Ag", c( 1997,1998), "mean"),
    list("Cl", c(1997,1998), "mean"),
    list("Man", c(1997,1998), "mean"),
    list("Pro", c(1997,1998), "mean"),
    list("Prof", c(1997,1998), "mean"),
    list("Sale", c(1997,1998), "mean"),
    list("Serv", c(1997,1998), "mean")),
  unit.names.variable = "State",
  treatment.identifier = 36,
  controls.identifier = c(2,5,6,7,8,11,13,16,19,20,21,23,25,26,29,30,31,32,35,37,38,39,45,47,49),
  time.optimize.ssr = c(1985:2003),
  time.plot =1985:2014)

synth_out <- synth(data.prep.obj = dataprep_out)

path.plot(synth_out, dataprep_out)                 

gaps.plot(synth_out, dataprep_out)                   

placebos <- generate.placebos(dataprep_out, synth_out, Sigf.ipop = 3)

plot_placebos(placebos)

mspe.plot(placebos, discard.extreme = TRUE, mspe.limit = 1, plot.hist = TRUE)

          