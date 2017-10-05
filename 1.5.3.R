# number of suicides every year by means adopted

suicideProfiles <- function(Dataset)
{
  # generating the list of unique causes
  
  profile <- vector() # Null Vector
  Dataset$Type_code <- as.character(Dataset$Type_code)
  Dataset$Type <- as.character(Dataset$Type)
  for(i in 1:236583) {
    if(Dataset$Type_code[i] == 'Professional_Profile') {
      profile<-c(profile,Dataset$Type[i])
    }
  }
  profile <- unique(profile)
  profile <- sort(profile)
  suicides <- vector("integer",length(profile))
  profile_wise <- data.frame(profile,suicides)
  
  # performs the calculation
  
  for(i in 1:236583) {
    if(Dataset$Type_code[i] == 'Professional_Profile') {
      for(j in 1:length(profile)) {
        if(Dataset$Type[i] == profile_wise$profile[j]) {
          profile_wise$suicides[j] = profile_wise$suicides[j] + Dataset$Total[i]
        }
      }
    }
  }
  write.table(profile_wise,"output/1.5.3.csv", row.names = FALSE, sep = ",")
  
  # bar graphs
  library(ggplot2)
  png("plots/Suicides_profile.png")
  options(scipen=999)
  g <- ggplot(data.frame(profile_wise$suicides), 
              aes(x=profile_wise$profile, y=profile_wise$suicides, 
                  fill=profile_wise$profile)) + geom_bar(stat = "identity") + 
    theme(legend.position="bottom", axis.text.x=element_text(angle=90)) +
    xlab("Professional Profile") + ylab("Suicide Count") +
    ggtitle("Professials who committed Suicides in India from 2001 to 2012")
  print(g)
  dev.off()
}
