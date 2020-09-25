#old
month <- MR %>%
  group_by(YYYY) %>% 
  summarize(mean(ATMP), median(MM))
colnames(month)[2]<-"AvgTMP"
colnames(month)[3] <- "MM"
assign(month_files[j], month)

#new
month <- MR %>%
  group_by(YYYY) %>% 
  summarize(mean(ATMP), median(MM))
colnames(month)[2]<-"AvgTMP"
colnames(month)[3] <- "MM"
month <- month %>% mutate(Day=c(1)) %>% 
  mutate(Date=make_date(year=YYYY, month=MM, day=Day))

#and the plot of the sinusoidal
fit <- stan_glm(AvgTMP ~ Date, data = frame, refresh=0)
cptn <- c(c("slope of regression line = ", round(coef(fit)[2],digits=3)))
ggplot(frame, aes(Date, AvgTMP)) + 
  geom_line() +
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], color = "blue") +
  geom_hline(aes(yintercept=mean(AvgTMP)), linetype="dotted")

#Trying to calculate the rise in average monthly temperature from beginning
#to end of our time frame based on the model.  BUT "Date" is in lubridate and we
#weren't able to figure out how to do operations with the date in this format.
coef(frame)[2]*max(frame$Date) - coef(frame[2]*min(frame$Date))


