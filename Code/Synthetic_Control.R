for (i in 1983:2009){
  nam <- paste('df_',i,sep = '')
  frame_list <- append(frame_list,as.name(nam))
  t <-  paste('State U_', i ,'.csv',sep = '')
  x <- read.csv(here::here('Data','Sector',paste('State U_', i ,'.csv',sep = ''))) %>%
    slice(-c(1,2))%>%
    row_to_names(1)%>%
    discard(~all(is.na(.) | . ==""))
  x$year <- i
  empty_columns <- sapply(x, function(x) all(is.na(x) | x == ""))
  x[, !empty_columns]
  assign(nam,x)
}
df <- do.call(rbind,frame_list)
emp_97 <- read.csv(here::here('Data','emp_97_perc.csv')) %>%
  dplyr::rename(State = 1)
## Right to Work
rtw <- read.csv(here::here('Data','rtw.csv')) %>%
  dplyr::rename(State = 1)
#Real Per Capita Income
rpci <- read.csv(here::here('Data','realpci.csv'))
df$rpci = 0
for(i in 1:nrow(df)){
  st <- df$State[i]
  yr <- df$year[i]
  col <- which(colnames(rpci) == st)
  if (df$State[i] %in% colnames(rpci)) {
    df$rpci[i] = rpci[yr-1963,col]
  }
}
df$rtw = 0
for (i in 1:nrow(df)) {
  st <- df$State[i]
  yr <- df$year[i]
  rtw_year <- rtw$Year[rtw$State == st]
  if (st %in% rtw$State) {
    if (yr > rtw$Year[rtw$State == st]) {
      df$rtw[i] = 1
    }
  }
}
leg <- read.csv(here::here('Data','legis_data.csv'))%>%
  dplyr::rename(State = 1, '1978' = 2, '1980' = 3, '1982' = 4, '1984' = 5, '1986' = 6, '1988' = 7,
                '1990' = 8, '1992' = 9, '1994' = 10, '1996' = 11, '1998' = 12, '2000' = 13, '2002' = 14,
                '2004' = 15, '2006' = 16, '2008' = 17, '2010' = 18, '2012' = 19, '2014' = 20 )
leg_yr <- colnames(leg)[2:20]
df$dem_leg = 0
df$rep_leg = 0
df$bi_leg = 0
for (i in 1:nrow(df)){
  lyr = df$year[i]
  if (lyr >=1978){
    n = tail(which(leg_yr <= df$year[i]), n=1)
    m = n+1
    st = df$State[i]
    x = which(leg$State == st)
    leg_st = leg[leg$State == st,]
    if ((st %in% leg$State) & (st != 'Nebraska')){
      if (pull(leg_st[m]) == 'D'){
        df$dem_leg[i] = 1
      }
      else if (pull(leg_st[m]) == 'R'){
        df$rep_leg[i] = 1
      }
      else if (pull(leg_st[m]) == 'S'){
        df$bi_leg[i] = 1
      }
    }
  }
}

df1 <- dplyr::filter(df,year > 1982 & year < 2010)
df_synth <- subset(df1,!(df$State %in% c('D.C.','A','Alabama','Arizona','Arkansas','Florida','Georgia','Idaho','Indiana','Iowa','Kentucky','Louisiana','Michigan','Mississippi','Nebraska','Nevada','North Carolina','North Dakota','South Carolina','South Dakota','Tennessee','Texas','Utah','Virginia','West Virginia','Wyoming')))
df_synth <- df_synth[-which(df_synth$State == ""), ]
df_synth <- merge(df_synth,emp_97, by = c('State','year'), all.x = TRUE)
df_synth$fr <-  (as.numeric(gsub(",","",df_synth$Covered))- as.numeric(gsub(",","",df_synth$Members)))/( as.numeric(gsub(",","",df_synth$Covered)))
df_synth$stid <- 0
for (i in 1:nrow(df_synth)){
  id <- which(state.name == df_synth$State[i])
  df_synth$stid[i] <- id}
df_synth$Ag <- as.numeric(df_synth$Ag)
synth <- filter(df_synth, Sector == 'Total')
pri_synth <- filter(df_synth, Sector == 'Private')
pub_synth <- filter(df_synth, Sector == 'Public')
cons_synth <- filter(df_synth, Sector == 'Priv. Construction')
man_synth <- filter(df_synth, Sector == 'Priv. Manufacturing')


df_list <- list(synth,pri_synth,pub_synth,cons_synth,man_synth)
for (i in 1:length(df_list)){
  dataprep_out <- dataprep(
    foo = data.frame(df_list[i]),
    predictors = c('rpci','rep_leg'),
    predictors.op = c("mean"),
    dependent = "fr",
    unit.variable = "stid",
    time.variable = "year",
    time.predictors.prior = 1983:2001,
    special.predictors = list(
      list("Ag", c( 1997,1998), "mean"),
      list("Man", c(1997,1998), "mean"),
      list("Pro", c(1997,1998), "mean"),
      list("Prof", c(1997,1998), "mean"),
      list("Sale", c(1997,1998), "mean"),
      list("Serv", c(1997,1998), "mean")),
    unit.names.variable = "State",
    treatment.identifier = 36,
    controls.identifier = c(2,5,6,7,8,11,13,16,19,20,21,23,25,26,29,30,31,32,35,37,38,39,45,47,49),
    time.optimize.ssr = c(1985:2001),
    time.plot =1985:2009)
  synth_out <- synth(data.prep.obj = dataprep_out)
  synth.tables<- synth.tab(dataprep.res = dataprep_out,synth.res = synth_out )
  x <- xtable(as.data.frame(synth.tables$tab.w))
  nam <- paste(i,'_table',sep = '')
  assign(nam,x)
  path.plot(synth_out, dataprep_out)
  gaps.plot(synth_out, dataprep_out)
  placebos <- generate.placebos(dataprep_out, synth_out, Sigf.ipop = 3)
  plot_placebos(placebos)
  m <- mspe.plot(placebos, discard.extreme = TRUE, mspe.limit = 1, plot.hist = TRUE)
  jpeg(file=here::here('Figures',paste(i,'_plot.jpg', sep='')))
  plot(m)
  dev.off()
}

tot <- plm( fr ~ rtw + rpci + dem_leg, data = synth, index = c('State','year'), model = 'within',effect =  'twoways' )
pri <- plm( fr ~ rtw + rpci + dem_leg, data = pri_synth, index = c('State','year'), model = 'within',effect =  'twoways' )
pub <- plm( fr ~ rtw + rpci + dem_leg, data = pub_synth, index = c('State','year'), model = 'within',effect =  'twoways' )
cons <- plm( fr ~ rtw + rpci + dem_leg, data = cons_synth, index = c('State','year'), model = 'within',effect =  'twoways' )
man <- plm( fr ~ rtw + rpci + dem_leg, data = man_synth, index = c('State','year'), model = 'within',effect =  'twoways' )
stargazer(tot,pri,pub,cons,man)
