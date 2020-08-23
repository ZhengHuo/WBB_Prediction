#
# Arthur Ye, Athena Huo, Helen Huang, Jonathan Xiao - 08/23/2020
# NCAA Women's Basketball 2014-15 through 2018-19
# Prediction of Women's College Basketball Teams' Strengths and the "Home 
# Court Advantage" by Linear Model
#

### For 351 teams, we're interested in studying game-level results.

# Read all HTML files into "all"
files <- dir("WBB_multiple", full.names=TRUE, pattern="RData")
temp <- sapply(files, load, envir=.GlobalEnv)
all <- lapply(temp, get)
names(all) <- temp

### First, We will clean the HTML codes to get the information we want and 
# save them into a large called "entire":

# creat a list of length 351 to save the information about each team
entire <- list(NULL)

# Season Name
season <- c("2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019")

# All Seasons for All Teams
for (n in 1:length(all)) {
  
  # Create a dataframe to save the information about all seasons for one team:
  team <- NULL
  
  # All Seasons for One Team:
  for (i in 1:5) {
    
    x <- all[[n]][[i]]
    
    # Create a html folder containing data of each team:
    filename <- paste0("htmlfiles/", names(all)[n], "_", 
                       names(all[[n]])[i], ".html")
    writeLines(x, filename)
    
    ### We will get a series of data we need in one season and create variables 
    # containing the data: 
    
    ## Game dates in one season:
    datelines <- grep("<td>[0-9]+/[0-9]+/\\d\\d\\d\\d", x)
    x[datelines]
    dates <- gsub("<[^<>]*>| ", "", x[datelines])
    
    ## Primary teams' names in one season:
    if (i == 1) {
      teamline <- grep("<legend><img alt", x, fixed = TRUE)
      teamname <- gsub(".*img alt=\"|\" height.*", "", x[teamline])
      teamname <- gsub("amp;", "", teamname)
      teamname <- gsub("&#x27;", "'", teamname)
    }
    
    ## Opponent teams' name in one season:
    opplines <- datelines + 2
    oppneutral <- x[opplines] # use for searching neutral site games later

    # The varibale contains the opponent teams' names with "@" sign 
    # (in order to determine the home or away games later)
    opplines <- gsub("        |<a href=\"/.*img alt=\"|\" height.*", "", 
                     x[opplines])
    opplines <- trimws(gsub("\" height.*", "", opplines))
    
    # The Varible contains the opponent teams' names without signs and letters 
    # other than the real team name 
    # (the names that would be shown in the final dataframe)
    oppnames <- opplines
    these <- substring(oppnames, 1, 1) == "@"
    oppnames[these] <- gsub("@", "", oppnames[these])
    oppnames <- gsub("amp;", "", oppnames)
    oppnames <- gsub("&#x27;", "'", oppnames)

    ## Game results (win or lose):
    scorelines <- datelines + 5
    x[scorelines]
    temp1 <- trimws(gsub("<[^<>]*>", "", x[scorelines]))
    gameresult <- substring(temp1, 1, 1)
    
    ## Scores received by oneteam and its opponent teams:
    temp2 <- gsub("[WL] (\\d*-\\d*).*", "\\1", temp1)  
    scores <- matrix(temp2)
    splitscores <- strsplit(scores[,1], "-")
    scores <- matrix(as.numeric(unlist(splitscores)), ncol=2, byrow=TRUE)
    
    # Create a data frame containing the variables we made above:
    y <- data.frame(dates, stringsAsFactors = TRUE)
    
    y$teamname <- teamname 
    y$season <- season[i]
    y$orgid <- names(all)[n] 
    y$opponents <- oppnames
    y$result <- gameresult
    y$myscores <- scores[, 1] 
    y$oppscores <- scores[, 2]
    y$scorediff <- y$myscores - y$oppscores
    
    ## Game location:
    # If there is no @ at all, it is a home game
    # If there is a @ in the first character, it is a away
    # If there is a @ some place else, it is neutral
    temp <- regexpr("@", opplines) # find the location of @
    y$place <- "home"
    y$place[temp == 1] <- "away"
    temp1 <- regexpr("<br/>@", oppneutral)
    y$place[temp1 != -1] <- "neutral"
    
    y$opponents[y$place == "home"] <- gsub("<br/>", "", 
                                           y$opponents[y$place == "home"])
    
    y$opponents[y$place == "away"] <- gsub("<br/>", "", 
                                           y$opponents[y$place == "away"])
    
    # Save all "y" data frames into "team"
    team <- rbind(team, y)
  }
  
  # Remove all "@" in each opponent name: 
  team$opponents <- gsub("@", "", team$opponents)
  
  # Put all "team" data in the "entire" list, and name each entry with the 
  # name of that team
  entire[[n]] <- team
  names(entire)[n] <- teamname
}

### Next, we will look at the new data frame more carefully to find if there are
# some problems hiding there or not:

# Save each data frame in list "entire" to a large data frame:
x <- do.call("rbind", entire) 
x <- x[!(x$myscores == 0), ]
row.names(x) <- NULL

# Create a unique game identifier that uses the alphabetical ordering of team 
# names:
x$gameid <- paste(x$dates, x$teamname, x$opponents)
these <- x$teamname < x$opponents
x$gameid[these] <- paste(x$dates, x$opponents, x$teamname)[these]

### We will use the detective work below to find out the missing games:

table(table(x$gameid))
which(table(x$gameid) == 1)
temp <- table(x$gameid)
bad <- x[x$gameid %in% names(temp)[temp == 1], ]
unique(x$orgid[x$teamname %in% bad$opponents])

### End of Detective Work.
### There are different types of issues about our data that need to be fixed:

# Delete games with two "@" signs in the webpage.
x <- x[!(x$teamname == "Siena" & x$opponents == "Iona" 
         & x$dates == "03/07/2019"), ]
x <- x[!(x$teamname == "Iona" & x$opponents == "Siena" 
         & x$dates == "03/07/2019"), ]
x <- x[!(x$teamname == "UC Irvine" & x$opponents == "UC Santa Barbara" 
         & x$dates == "03/12/2019"), ]
x <- x[!(x$teamname == "UC Santa Barbara" & x$opponents == "UC Irvine" 
         & x$dates == "03/12/2019"), ]

# Replace the abbreviate name of some opponent teams:
x$opponents <- gsub("UMKC", "Kansas City", x$opponents)
x$opponents <- gsub("Incarnate Word", "UIW", x$opponents)
x$opponents <- gsub("Loyola Marymount", "LMU (CA)", x$opponents)

# Fix the problem of names of teams outside divison 1
x$opponents <- gsub("@.*", "", x$opponents)
x$opponents <- gsub("Z_Do Not Use_","", x$opponents)

# Dangerous unless opponents has been cleaned up properly
x <- x[x$opponents %in% x$teamname, ] # only keep d1 vs d1

# Once you fix the problem above, solving the duplicate problem is easy.
x <- x[!duplicated(x$gameid), ]

### After dropping the duplicated games, there are some teams only appeared in 
# either "teamname" or "opponents" but not both (and the total number of teams 
# is) still correcr (351) 

# Save the cleaned data in a new csv file ready for future analysis:
write.csv(x, file = "WBB_Cleaned.csv", row.names = FALSE)