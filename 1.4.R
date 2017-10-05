# Age_group wise total no of suicides

SuicideAgeGroup <- function(Dataset)
{
  age_group_levels <- unique(Dataset$Age_group)
  age_group_names <- c("Unknown","Children","Teenagers","Middle-Aged Adult","Older Adults","Senior Citizens")
  no_of_suicides <- c(0,0,0,0,0,0)
  age_group_levels <- sort(age_group_levels)
  
  age_group <- data.frame(age_group_levels,age_group_names,no_of_suicides)
  
  for(i in 1:236583) {
    if(Dataset$Type_code[i] == 'Causes' && Dataset$Age_group[i] != '0-100+') {
    for(j in 1:6) {
      if(Dataset$Age_group[i] == age_group[j,"age_group_levels"]) {
        age_group[j,"no_of_suicides"] = age_group[j,"no_of_suicides"]+Dataset$Total[i]
      }
    }
    }
  }
  write.table(age_group,"output/1.4.csv", row.names = FALSE, sep = ",")
  
  # bar graph
  png("plots/Suicides_Age.png")
  options(scipen=999)
  g <- ggplot(age_group, aes(x=age_group_levels, y=age_group$no_of_suicides, fill=age_group_levels)) + geom_bar(stat = "identity") + 
    theme(legend.position="bottom",axis.text.x=element_text(angle=90)) +
    xlab("Age Group") + ylab("Suicide Count") +
    ggtitle("Suicides in India from 2001 to 2012 in age groups")
  print(g)
  dev.off()
  
}