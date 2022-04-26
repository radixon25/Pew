library(dplyr)
library(tibble)
path = here('Data','union.csv')
df <- read.csv(path)
df_ur <- as_tibble_col(matrix(t(df[-c(1,2)]), ncol = 1),column_name = 'UR') %>% 
  filter_at(vars(UR),all_vars(!is.na(.))

            