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
  print(frame)
}