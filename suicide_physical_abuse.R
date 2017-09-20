# year wise suicide due to physical abuse

physical_abuse <- function(Dataset) {
  years <- c(unique(Dataset$Year))
  suicides <- c(0,0,0,0,0,0,0,0,0,0,0,0)
  table <- data.frame(years,suicides)
  
  for(i in 1:236583) {
    if(Dataset$Type[i] == "Physical Abuse (Rape/Incest Etc.)") {
      for (j in 1:12) {
        if( Dataset$Year[i] == table[j,"years"]) {
          table[j,"suicides"] = table[j,"suicides"] + Dataset$Total[i];
        }
      }
    }
  }
  print(table)
}