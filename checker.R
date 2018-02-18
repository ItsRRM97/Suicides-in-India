checker <- function (Dataset) {
  counter <- 0
  for(i in 1:236583) {
    if(Dataset$Year[i] == '2012') {
      if(Dataset$State[i]=="WEST BENGAL") {
        counter<-counter+1
        print(Dataset$Year[i])
        print(Dataset$State[i])
      }
    }
  }
  print(counter)
}