## main.R
## 
## 

library(dplyr)
library(ggplot2)
library(mgsub)
library(tidyr)
library(lubridate)
library(lmtest)
library(tibble)
library(PerformanceAnalytics)

## Conditions to be met
params <- list(nobs   = 13000,
               df     = 40,
               adj_R2 = 0.6,
               RMSE   = 450000 # in $
               )
 
read_raw_data <- function(loc){
  
  lf <- list.files(loc)
  dir <- paste0(loc,'/',lf)
  
  colnm <- c('borough','neighborhood','bldclasscat',
             'taxclasscurr','block','lot','easement',
             'bldclasscurr','address','aptnum','zip',
             'resunits','comunits','totunits',
             'landsqft','grosssqft','yrbuilt',
             'taxclasssale','bldclasssale',
             'price','date')
  
  xlist <- lapply(dir[1:4], function(x){read.csv(x, 
                                                 skip=4,
                                                 na.strings = c(" -   ","",
                                                                "            ")) %>% 
      setNames(.,colnm)})
  
  xdat_2020 <- read.csv(dir[5], 
                        skip=6,
                        na.strings = c(" -   ","","            ")) %>% 
    setNames(.,colnm)
  
  xdat0 <- do.call("rbind",xlist)
  xdat1 <- rbind(xdat0,xdat_2020)
  
  
  ## Format col type
  xdat2 <- xdat1 %>% 
    mutate(date = as.Date(date,"%m/%d/%Y"),
           date = as.Date((ifelse(year(date) < 1000, 
                                  date %m+% years(2000), 
                                  date)),origin='1970-01-01'),
           bldclasssale = gsub(" ","",bldclasssale)) %>% 
    mutate_at(c('grosssqft','landsqft','price'),
              .funs=function(x){as.numeric(mgsub(pattern=c(","," ","\\$"),
                                                 replacement = c("","",""),
                                                 x))}) %>% 
    suppressWarnings()
  
  
  
  return(xdat2)
}

loc <- './data'
dat0 <- read_raw_data(loc)

## Filter data
xdat1 <- dat0 %>% 
  filter((grepl("^*\\s*A",.$bldclasssale) | 
            grepl("^*\\s*R",.$bldclasssale)) &
           price >= 10000 &
           !is.na(price) &
           resunits == "1" & 
           totunits == "1" &
           grosssqft > 0 
  )

# [1] "borough"      "neighborhood" "bldclasscat"  "taxclasscurr" "block"       
# [6] "lot"          "easement"     "bldclasscurr" "address"      "aptnum"      
# [11] "zip"          "resunits"     "comunits"     "totunits"     "landsqft"    
# [16] "grosssqft"    "yrbuilt"      "taxclasssale" "bldclasssale" "price"                
# [21] "date"

## EDA

chart.Correlation(xdat1[,c(15:16,20)], histogram = TRUE, method = "pearson")

ggplot(xdat1, aes(x=yrbuilt,y=price)) + geom_bar(stat='identity')

xtmp <- xdat1 %>% group_by(zip) %>% summarise(mnp = mean(price)) %>% arrange(mnp)
ggplot(xtmp, aes(x=reorder(zip,mnp),y=mnp)) + geom_bar(stat='identity') + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(xdat1, aes(x=bldclasssale,y=price)) + geom_bar(stat='identity')



## Fix yrbuilt == 0, take the average yrbuilt of that neighbourhood and bldclasscat
## and fix those zeros
xdat2 <- xdat1 %>% 
  mutate(yrbuilt = ifelse(yrbuilt == 0, NA, yrbuilt)) %>% 
  group_by(neighborhood,bldclasscat) %>% 
  mutate(yrbuilt1=ifelse(is.na(yrbuilt), round(mean(yrbuilt,na.rm=T),0), yrbuilt)) %>% 
  data.frame() %>% 
  filter(!is.na(yrbuilt1)) # now remove the yrbuilt == 0 rows


## Feature engineering

xdat3 <- xdat2 %>% mutate(total_sqft = grosssqft + landsqft,
                           bldclasscat = ifelse(grepl("ONE",bldclasscat),
                                                "01 ONE FAMILY DWELLINGS",
                                                bldclasscat),
                           region = ifelse(zip %in% c(11249,11238,11217,11215,11231,11201),'high_income',
                                           ifelse(zip %in% c(11239,11236,11224,11208,11207), '10000low_income',as.character(zip))),
                           era_built = ifelse(yrbuilt1 %in% 1799:1898, 'cent_19',
                                              ifelse(yrbuilt1 %in% 1899:1949, 'firsthlfcent_20',
                                                     ifelse(yrbuilt1 %in% 1950:1999,'sechlfcent_20',
                                                            ifelse(yrbuilt1 > 1999,'cent_21','Other')))),
                           yr_sale = as.character(year(date)),
                           qr_sale = as.character(quarter(date)),
                           sqprice = sqrt(price),
                           sq_grosssqft = sqrt(grosssqft),
                           sq_landsqft = sqrt(landsqft),
                           cat = ifelse(grepl("A",bldclasssale),'A','R'),
                           age = year(date) - yrbuilt1,
                           region1 = ifelse(zip %in% c(11249,11238,11217,11215,11231,11201),'high_income',
                                            ifelse(zip %in% c(11239,11236,11224,11208,11207), '1low_income','med_income')),
                           time_of_sale = ifelse(yr_sale == '2020' & qr_sale == '3', '2020Q3',
                                                 ifelse(yr_sale == '2020' & qr_sale == '4', '2020Q4','Rest')))


## chopping off extremely high prices
xdat4 <- xdat3 %>% filter(price < 5900000) 

## Model training
xlm <- lm(sqprice ~ factor(zip) + sq_grosssqft + time_of_sale, xdat4)
summary(xlm)


xdat5 <- cbind(xdat4,xlm$fitted.values,xlm$residuals) %>% 
  setNames(.,c(colnames(xdat4),'fit_val','residuals'))

chart.Correlation(xdat5[,c(28:29,35:36)], histogram = TRUE, method = "pearson")


## Plot 1a
ggplot(xdat5 %>% filter(date >= '2020-06-01', grosssqft > 1500)) + 
  geom_point(aes(x=date,y=fit_val^2), color = 'red') + 
  geom_smooth(aes(x=date,y=fit_val^2),color='red',method='lm')+
  geom_point(aes(x=date,y=price), color = 'black')+
  geom_smooth(aes(x=date,y=price),color='black',method='lm')+
  facet_wrap(~region1, scales='free_y') +
  ggtitle('Spacious neighbourhoods with sqft > 1500 (Red is fitted data from the model \n and black is the actual data)')

## Plot 1b
ggplot(xdat5 %>% filter(date >= '2020-06-01', grosssqft < 1500)) + 
  geom_point(aes(x=date,y=fit_val^2), color = 'red') + 
  geom_smooth(aes(x=date,y=fit_val^2),color='red',method='lm')+
  geom_point(aes(x=date,y=price), color = 'black')+
  geom_smooth(aes(x=date,y=price),color='black',method='lm')+
  facet_wrap(~region1, scales='free_y') +
  ggtitle('Non-spacious neighbourhoods with sqft < 1500')


## RMSE
xdat5 %>% summarise(rmse = sqrt(mean((price-fit_val^2)^2)))

anova(xlm)

dwtest(xlm)

## Normality test
hist(xlm$residuals)
ks.test(xlm$residuals,pnorm)


plot(xlm$fitted.values,xlm$residuals)
bptest(xlm)

TukeyHSD(aov(sqprice ~ time_of_sale, xdat4))
plot(TukeyHSD(aov(sqprice ~ time_of_sale, xdat4), conf.level=.95), las = 2)

saveRDS(list(model=xlm, data=xdat4), file='./housing_mod.RDS') 
