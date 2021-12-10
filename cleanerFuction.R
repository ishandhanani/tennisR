#PREPROCESSING CSV FILES
#1. Create player1 and player2 columns and transfer all statistics into player columns so model isnt training with bias
#2. Create the target variable based on which player won the match
#3. Drop the old columns and keep the final data set 

clean = function(x)
{
  #test = atp_matches_2000
  #x = test
  x = x %>% mutate(player1 = ifelse(x$winner_name < x$loser_name, winner_name, loser_name))
  x = x %>% mutate(player2 = ifelse(x$winner_name > x$loser_name, winner_name, loser_name))
  
  extensions = c('age', 'entry', 'hand', 'ht', 'id', 'ioc', 'name', 'rank', 'rank_points', 'seed')
  
  rows = nrow(x)
  
  for(e in seq(1,10,by=1)){
    x[paste0("p1_",extensions[e],collapse = "")] = NA                  
    x[paste0("p2_",extensions[e],collapse = "")] = NA         
  }
  
  for(e in seq(1,10,by=1)){
    for(i in 1:rows)
    {
      if(x[i,"player1"] == x[i,"winner_name"])
      {
        x[i,paste0("p1_",extensions[e],collapse = "")] = x[i,paste0("winner_",extensions[e],collapse = "")]
      }
      if(x[i,"player1"] == x[i,"loser_name"])
      {
        x[i,paste0("p1_",extensions[e],collapse = "")] = x[i,paste0("loser_",extensions[e],collapse = "")]
      }
      if(x[i,"player2"] == x[i,"winner_name"])
      {
        x[i,paste0("p2_",extensions[e],collapse = "")] = x[i,paste0("winner_",extensions[e],collapse = "")]
      }
      if(x[i,"player2"] == x[i,"loser_name"])
      {
        x[i,paste0("p2_",extensions[e],collapse = "")] = x[i,paste0("loser_",extensions[e],collapse = "")]
      }
    }
  }
  
  x$target = x$winner_name
  
  x = x%>%select(-contains(c("winner","loser")))
  
  return(x)
}


#Working with the score column to get the games and remove rows that have ret, inj, etc

scoreAdj  = function(xf)
{
  #xf = xf %>% select(score)
  xf = xf %>% filter(!grepl("Apr-00", score))
  xf = xf %>% filter(!grepl("Default", score))
  xf = xf %>% filter(!grepl("Played and abandoned", score))
  xf = xf %>% filter(!grepl("Played and unfinished", score))
  xf = xf %>% filter(!grepl("Walkover", score))
  xf = xf %>% filter(!grepl("DEF", score))
  xf = xf %>% filter(!grepl("Unfinished", score))
  xf = xf %>% filter(!grepl("Def.", score))
  xf = xf %>% filter(!grepl("In progress", score))
  xf = xf %>% filter(!grepl("W/O", score))
  xf = xf %>% filter(!grepl("RET", score))
  xf = xf %>% filter(!grepl("In Progress", score))
  
  return(xf)
}




