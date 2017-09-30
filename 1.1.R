# Year wise total no of suicides

SuicideYearly <- function(Dataset)
{
  year <- unique(Dataset$Year)
  no_of_suicides <- c(0,0,0,0,0,0,0,0,0,0,0,0)
  year <- sort(year)
  
  frame <- data.frame(year,no_of_suicides)
  
  for(i in 1:236583) {
    for(j in 1:12) {
      if(Dataset$Year[i] == frame[j,"year"]) {
        frame[j,"no_of_suicides"] = frame[j,"no_of_suicides"]+Dataset$Total[i]
      }
    }
  }
  write.table(frame,"output/1.1.csv", row.names = TRUE, sep = ",")
  
  # bar graph
  
  png(file = "plots/yearly_suicides.png")
  barplot(frame$no_of_suicides/100000,names.arg = year,xlab = "Years",ylab = "Suicides(in lakhs)",col = "blue",
          main = "Suicides over the Years",border = "red")
  dev.off()
}