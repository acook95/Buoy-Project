
months<-str_pad(months, 2, pad="0")
month_files <- str_c("M", months, sep = "")
plot_files <- str_c("P", months, sep = "")

grid.arrange(P01, P02, P03, P04, P05, P06, P07, P08, P09, P10, P11, P12, nrow=4, ncol=3, newpage = TRUE)
#We tried - but failed in our attempt to condense the P01:P12 within grid.arrange :-(