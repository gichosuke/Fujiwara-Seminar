# Panel analysis ----------------------------------------------------------

library(readxl)
dat <- read_excel("BAD_ASSETSNEW.xls", 
                  sheet = "Sheet1")
View(dat)
library(plm)
# needs::prioritize(plm)
# pdata.frame(dat,index = c(colname for individuals,time))
pdat <- pdata.frame(dat,index = c("BANK","YEAR"))

# pooled OLS
res1 <- plm(log(BADLOAN)~log(lag(YOKIN,1))+lag(CAPGAP)+lag(PUBLIC)+
              KYUJIN+LISTED,
            data = pdat,model = "pooling")

summary(res1)

# fixed effect
res2 <- plm(log(BADLOAN)~log(lag(YOKIN))+lag(CAPGAP)+lag(PUBLIC)+
              KYUJIN+LISTED,
            data = pdat,model = "within")

summary(res2)

# random effect
res3 <- plm(log(BADLOAN)~log(lag(YOKIN))+lag(CAPGAP)+lag(PUBLIC)+
              KYUJIN+LISTED,
            data = pdat,model = "random")

summary(res3)


# Earthquake DID ----------------------------------------------------------

tohoku <- read_xlsx("Tohoku.xlsx",sheet = 1)

res4 <- lm(Y~DT+D2+DT*D2,data=tohoku)
summary(res4)