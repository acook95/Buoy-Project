library(tidyverse)
library(stringr)
library(rstanarm)

### make URLs
url1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=mlrf1h"
url2 <- ".txt.gz&dir=data/historical/stdmet/"

years <- c(2000:2018)
#note: we've ducked the issue of two digit years by starting with 2000

urls <- str_c(url1, years, url2, sep = "")
filenames <- str_c("mr", years, sep = "")

###  Read the data from the website

N <- length(urls)

#
for (i in 1:N){
  suppressMessages(  ###  This stops the annoying messages on your screen.
    assign(filenames[i], read_table(urls[i], col_names = TRUE, na = ""))
  )
  
 file <- get(filenames[i])
  
 #This is needed because some years (eg 2007) label their year column "#YY"
 colnames(file)[1] <-"YYYY"
  
  #For each year, only grab the YYYY, MM, and temp columns and only keep the
  #data for the month of August.  Also remove large NA values in ATMP
  file %<>%
    select(YYYY, MM, ATMP) %>% 
    filter(MM=="02", ATMP<70)
  
  if(i == 1){
    MR <- file
  }
  else{
    MR <- rbind.data.frame(MR, file)
  }
}

MR2 <- transform(MR, YYYY=as.numeric(YYYY), ATMP= as.numeric(ATMP))
#ugh - why didn't the following piping work:
#MR %<>% transform(ATMP=as.numeric(ATMP))

MR3 <- MR2 %>%
  group_by(YYYY) %>% 
  summarize(mean(ATMP))

colnames(MR3)[2]<-"AvgTMP"

ggplot(MR3, aes(YYYY, AvgTMP)) + geom_point()


plot(MR3)
yhat<-lm(MR3$AvgTMP~MR3$YYYY)
abline(yhat, col="red")

# 
# MR2 <- filter(MR, DD==11, hh==12, ATMP<500)
# 
# MR3 <- select(MR2, YYYY, MM, ATMP)
# 
# MR4 <- transform(MR3, YYYY= as.numeric(YYYY), MM= as.numeric(MM), ATMP= as.numeric(ATMP))
# 
# #MR5 <- unite(MR4, DATE, YYYY, MM, sep = "-")
# MR5 <- mutate(MR4, DATE = 12 * (YYYY-2000) + MM)
# 
# ggplot(data=MR5, aes(DATE, ATMP)) + geom_point() + geom_line()
# 
# fit <- stan_glm(ATMP ~ DATE, data = MR5, refresh=0)
# fit
# 
# MR7 <- filter(MR, MM==11, DD==11, hh==12, ATMP<500)
# MR8 <- transform(MR7, YYYY= as.numeric(YYYY), ATMP= as.numeric(ATMP))
# 
# ggplot(data=MR8, aes(YYYY,ATMP)) + geom_point()
# fit2 <- stan_glm(ATMP ~ YYYY, data=MR8, refresh=0)
# fit2
# 
# ##group_by() and summarize() mean of each group together in dplyr package
# 
# #Good idea!
