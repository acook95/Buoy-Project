library(tidyverse)
library(stringr)
library(rstanarm)

### make URLs

url1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=mlrf1h"
url2 <- ".txt.gz&dir=data/historical/stdmet/"

years <- c(2000:2018)
months <- c(01:12)
#note: we've ducked the issue of two digit years by starting with 2000

urls <- str_c(url1, years, url2, sep = "")
filenames <- str_c("mr", years, sep = "")
month_files <- str_c("Month", months, sep = "")

###  Read the data from the website

N <- length(urls)
M <- 12
for (j in 1:M){
  

  for (i in 1:N){
    
    suppressMessages(  ###  This stops the annoying messages on your screen.  Do this last.
      
      assign(filenames[i], read_table(urls[i], col_names = TRUE, na = ""))
      
    )
    
    file <- get(filenames[i])
   
    
    #This is necessary because some because some years (eg 2007) label their year 
    #column as "YY"
    colnames(file)[1] <-"YYYY"
    
    #For each year, grab only the YYYY, MM, and temp columns, and only keep the 
    #data for the month of August. Also remove large NA values in ATMP.
    file %<>%
      select(YYYY, MM, ATMP) %>% 
      transform(YYYY=as.numeric(YYYY), MM=as.numeric(MM), ATMP=as.numeric(ATMP)) %>% 
      filter(MM==j, ATMP<70)
      
    
    
    
    if(i == 1){
      MR <- file
    }
    else{
      
      MR <- rbind.data.frame(MR, file)
    }
  }

month <- MR %>%
  group_by(YYYY) %>% 
  summarize(mean(ATMP))

colnames(month)[2]<-"AvgTMP"

assign(month_files[j], month)

}

ggplot(MR3, aes(YYYY, AvgTMP)) + geom_point()

yhat<-lm(MR3$AvgTMP~MR3$YYYY)
yhat
##any particular reason for using lm instead of stan_glm here?

plot(MR3)
abline(yhat, col="red")


##in April and August 2018, there was no recorded data due to lack of funding.

