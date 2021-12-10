library(deuce)
library(tidyverse)
library(ggplot2)
#source("cleanerFunction.R",local = TRUE)
#==============================================================================================================================================================================
#Read in all csv files from 2000 to 2019 and create master file
filenames = list.files()
filenames = filenames[c(-1,-22,-23)]
csv = lapply(filenames, read.csv)
full = do.call(rbind, csv)

#Initial clean step through the clean function - open the file for specific steps
#data contains the set with winner/loser titles stripped
data=clean(full)
stBackup = data
#==============================================================================================================================================================================
#The following section will continue cleaning but will do it locally to shorten function runtime
#Step 1. Remove columns that are not needed
removedCols = data %>% select(tourney_id,
                              tourney_name,
                              surface,
                              tourney_level,
                              tourney_date,
                              score,
                              best_of,
                              player1,
                              round,
                              player2,
                              contains(c("p1_","p2_")),
                              match_num,
                              target)
removedCols = removedCols %>% select(-p1_name,
                                     -p2_name,
                                     -p1_entry,
                                     -p2_entry)

#Step 2. Format the date into a %Y%m%d format for future sorting
dateFormat = removedCols %>% mutate(tourney_date = as.Date(as.character(tourney_date), format= "%Y%m%d"))

#Step 3. Remove scorelines that do not show a completed match. Function in cleaner
cleanedScore = scoreAdj(dateFormat)

#Step 4. Arrange matches in order --> year, tournament, match number
arranged = cleanedScore %>% arrange(tourney_date)
arranged = arranged %>% arrange(match_num)

#Step 5. Create the H2H between players
#The H2H rivalry will start in 2000 and will not reflect matches before 2000
#THIS CAN POTENTIALLY BE ADDRESED..........
h2h = arranged %>% select(player1, player2, target)
h2h$p1_h2h = 0
h2h$p2_h2h = 0
h2h = h2h %>% group_by(player1, player2)
#player1 h2h perspective 
#h2h = h2h %>% group_by(player1,player2) %>% add_count(p1h2h = (player1 == target))
row = nrow(h2h)
for(i in 1:row)
{
  if(h2h[i,"player1"] == h2h[i,"target"])
  {
    h2h[i,"p1_h2h"] = h2h[i-1,"p1_h2h"] + 1
    h2h[]
    
  }
}