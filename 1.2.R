# State wise total no of suicides

SuicideState <- function(Dataset)
{
  library(ggplot2)
  state <- unique(as.character(Dataset$State))
  no_of_suicides <- vector(mode="integer", length=length(state))
  state <- sort(state)
  
  mat <- matrix(0, nrow = length(state), ncol = 1)
  dimnames(mat) <- list(state)
  
  # problem solving
  
  for(i in 1:236583) {
    if(Dataset$Type_code[i] == 'Causes' && Dataset$Total[i] != 0) {
      mat[Dataset$State[i]] = mat[Dataset$State[i]] + Dataset$Total[i] 
    }
  }
  write.table(mat,"output/1.2.csv", row.names = TRUE, sep = ",")
  
  # bar graph
  
  png("plots/Suicides_State.png")
  plott <- data.frame(mat)
  options(scipen=999)
  g <- ggplot(plott, aes(x=state, y=plott, fill=state)) + geom_bar(stat = "identity") + 
    theme(legend.position="bottom", legend.text=element_text(size=5, face ="bold"), axis.text.x=element_text(angle=90)) +
    xlab("States") + ylab("Suicide Count") +
    ggtitle("Suicides in India from 2001 to 2012 by State")
  print(g)
  dev.off()
}