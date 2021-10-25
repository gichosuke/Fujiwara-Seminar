setwd("/Users/KosukeA/Documents/R/")
library(dplyr)
library(readr)

nikkei <- read_csv("NIKKEI225.csv",col_types = cols(DATE=col_date(),NIKKEI225=col_double())) %>% 
  filter(!is.na(NIKKEI225)) %>% mutate(lnNIKKEI=log(NIKKEI225))

# needs::prioritize(dplyr)
res1 <- lm(NIKKEI225~lag(NIKKEI225),data=nikkei)
res2 <- lm(lnNIKKEI~lag(lnNIKKEI),data=nikkei)

summary(res1)
summary(res2)
