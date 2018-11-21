library(RSelenium)
library(rvest)
library(XML)
library(magrittr)
library(stringr) 
library(dplyr)
library(getPass)
library(stringr)

#shell('docker pull selenium/standalone-chrome')

#pass word for ESPN
pass <- getPass()

rd <- rsDriver()

remDr <- rd[["client"]]
  

#Nagivate to page to collect links to teams
remDr$navigate(url = "http://games.espn.com/ffl/leaguesetup/ownerinfo?leagueId=309844")

Sys.sleep(1.0)

#Add info the username/password fields
#The username/password inputs are on an iframe document, so I need to switch to the iframe before I search for elements
remDr$switchToFrame(remDr$findElement("tag name", "iframe"))

#Username/Password input
remDr$findElements(using = "xpath", '//*[@id="did-ui-view"]/div/section/section/form/section/div[1]/div/label/span[2]/input')
remDr$sendKeysToActiveElement(sendKeys = list("syork2816@gmail.com", key = "tab", pass, key = "enter")
)

Sys.sleep(1.0)


source <- remDr$getPageSource()[[1]]

#Links of teams
teamLinks <- read_html(source) %>%
  html_node(xpath = '//*[@id="frmOwnerInfo"]/table[1]') %>%
  html_nodes("a")

# Only keep links to the team
keepers <- grep(teamLinks, pattern = "clubhouse")
teamLinks <- teamLinks[keepers]
teamName <- teamLinks %>% html_text()
teamURL <- teamLinks %>% html_attr("href")
teamURL <- paste0("http://games.espn.com", teamURL)
teamID <- str_extract_all(teamURL, "(?<=teamId=).+(?=&)", simplify = TRUE) # Some code to extract between "teamID =" and "&"

teamMapping <- data.frame(teamName = teamName, teamURL = teamURL, teamID = teamID)

teamMapping$teamName <- as.character(teamName)
teamMapping$teamURL <- as.character(teamURL)
teamMapping$teamID <- as.character(teamID)

#Add results to list
scoreList <- list()

for (n in c(1:nrow(teamMapping))){
  
  id <- teamMapping$teamID[n]
  
remDr$navigate(url = sprintf("http://games.espn.com/ffl/schedule?leagueId=309844&teamId=%s", id) )

print(id)

#Sleep, otherwise page will load source to early and will return blanks.
Sys.sleep(2.0)

source <- remDr$getPageSource()[[1]]

#Collect links to each game
scheduleLinks <- read_html(source) %>%
  html_nodes("tbody") %>% .[[2]] %>%
  html_nodes("a") %>%
  html_attr("href")


# Isolate links to only those for the game
scoreIndex <- grep(x = scheduleLinks, pattern = "boxscorequick")
scheduleLinks <- scheduleLinks[scoreIndex]

#Only take the first 11 so that we don't have to worry about playoff stuff
scheduleLinks <- scheduleLinks[1:11]

# Create final url
scheduleLinks <- paste0("http://games.espn.com", scheduleLinks)



  # Navigate to each game
  for(i in c(1:length(scheduleLinks))){
  #i <- 1
    remDr$navigate(scheduleLinks[i])
    
    Sys.sleep(2.0)
    
    # Find the link to show the bench players and click on it
    showBench <- remDr$findElement(using = "link text", "Show Bench")
    showBench$clickElement()
    

    
    Sys.sleep(2.0)
    
    source <- remDr$getPageSource()[[1]]
    
    ### The person's player table can either appear on the right or the left,
    ### so a test is needed to determine which table to extract so that
    ### the correct player data is extracted.
    
    #Team name whose data we want to extract
    team <- teamMapping$teamName[which(teamMapping$teamID == id)]
    
    #The name of the team as displayed in the LEFT table header
    teamTable <- read_html(source) %>%
      html_node(xpath = '//*[@id="playertable_0"]/tbody/tr[1]/td') %>% html_text()
    
    #Test if team name is in the table name
    nameTest <- grepl(pattern = team, teamTable)
    
    #If it is true, extract the LEFT table, if false, extract the RIGHT table.
    if(nameTest == TRUE){
      ### Create table showing roster of players played
      playingRoster <- read_html(source) %>%
        html_node(xpath = '//*[@id="playertable_0"]') %>%
        html_table(fill = TRUE)
      
      # Create table of players that are benched
      benchRoster <- read_html(source) %>%
        html_node(xpath = '//*[@id="playertable_1"]') %>%
        html_table(fill = TRUE)
      
    }else{
      playingRoster <- read_html(source) %>%
        html_node(xpath = '//*[@id="playertable_2"]') %>%
        html_table(fill = TRUE)
      
      # Create table of players that are benched
      benchRoster <- read_html(source) %>%
        html_node(xpath = '//*[@id="playertable_3"]') %>%
        html_table(fill = TRUE)
      
    }
    
    #Only keep the first few columns and rename them
    playingRoster <- playingRoster[-c(1:3),c(1:5)]
    colnames(playingRoster) <- c("position", "player", "opponent", "status", "pts")
    
    
    ### Create table showing roster of players played
    #Only keep the first few columns and rename them
    benchRoster <- benchRoster[-1,]
    colnames(benchRoster) <- c("position", "player", "opponent", "status", "pts")
    
    ### Insert position into position column for the benched players 
    # Remove IR designations
    benchRoster$player <-  sub(pattern = "IR", replacement = "", x = benchRoster$player)
    #And whatever player is in the IR spot since they can't play
    benchRoster <- benchRoster[which(benchRoster$position != "IR"),]
    
    #Add position
    for(a in c(1:nrow(benchRoster))){
      player <- benchRoster$player[a]
      player <- gsub("[[:space:]]", "", player) # Remove white space  
      player <- gsub(intToUtf8(160),'',benchRoster$player[a])
      
      #Remove Q, O, or D from last position
      for(b in c(1:length(player))){
        
        lastChar <- substr(x = player[b], start = nchar(player[b]), stop = nchar(player[b]))
        
        if(lastChar %in% c("Q", "D", "O")){
          player <- substr(x = player[b], start = 1, stop = (nchar(player[b])-1))
        }
        
      }
      
        pos <- substr(x = player, start = (nchar(player)-1), stop = nchar(player))
        benchRoster$position[a] <- pos
    }
    
    #Lastly, fix D/ST
    for (e in c(1:length(benchRoster$position))){
      if(benchRoster$position[e] == "ST"){
        benchRoster$position[e] <- "D/ST"
      }
    }
    
    #combine playing roster and bench roster
    data <- rbind(playingRoster, benchRoster)
    data$pts <- gsub(pattern = "--", replacement = NA, x = data$pts)
    data$pts <- as.numeric(data$pts)
    
    #Convert NA points to 0
    for(a in c(1:nrow(data))){
      if(is.na(data$pts[a])){
        data$pts[a] <- 0
      }
    }
    
   if ( sum(is.na(data$pts)) > 1 ){
     next
   }
    
    # Helper function to calculate which player to play.
    bestPos <- function(df = data, pos){
      player <- filter(df, position == pos)
      player <- na.omit(player)
      player <- player[which(player$pts == max(player$pts)),]
      return(player)
    }
    
    optimizeRoster <- function(data){
      tempData <- data
      
      #QB
      QB <- bestPos(tempData, "QB")
      if(nrow(QB) > 1){
        QB <- QB[1,]
      }
      tempData <- filter(tempData, player != QB$player)
    
      #RB1 
      RB1 <- bestPos(tempData, "RB")
      if(nrow(RB1) > 1){
        RB1 <- RB1[1,]
      }
      tempData <- filter(tempData, player != RB1$player)
      
      #RB2
      RB2 <- bestPos(tempData, "RB")
      if(nrow(RB2) > 1){
        RB2 <- RB2[1,]
      }
      tempData <- filter(tempData, player != RB2$player)
      
      
      
      #WR1
      WR1 <- bestPos(tempData, "WR")
      if(nrow(WR1) > 1){
        WR1 <- WR1[1,]
      }
      tempData <- filter(tempData, player != WR1$player)
      
      
      #WR2 
      WR2 <- bestPos(tempData, "WR")
      if(nrow(WR2) > 1){
        WR2 <- WR2[1,]
      }
      tempData <- filter(tempData, player != WR2$player)

      
      #TE
      TE <-bestPos(tempData, "TE")
      tempData <- filter(tempData, player != TE$player)
      
      #FLEX
      FLEX <- filter(tempData, position %in% c("WR", "TE", "RB"))
      FLEX <- na.omit(FLEX)
      FLEX <- FLEX[which(FLEX$pts == max(FLEX$pts)),]
      #In the scenario where there is a tie at flex, take the first one.
      if(nrow(FLEX) > 1){
        FLEX <- FLEX[1,]
      }
      tempData <- filter(tempData, player != FLEX$player)
      
      #DEF/ST
      DEF <- bestPos(tempData, "D/ST")
      if(nrow(DEF) > 1){
        DEF <- DEF[1,]
      }
      tempData <- filter(tempData, player != DEF$player)
      
      #K
      K <- bestPos(tempData, "K")
      if(nrow(K) > 1){
        K <- K[1,]
      }
      tempData <- filter(tempData, player != K$player)
      
      # Combine into optimized roster
      optimalData <- rbind(QB, RB1, RB2, WR1, WR2, FLEX, DEF, K)
      
      return(optimalData)
    }
  
    optimizedData <- optimizeRoster(data)
    print(optimizedData)
    
    week <- paste0("week", i)
    teamName <- teamMapping$teamName[n]
    scoreList[[teamName]][[week]] <- optimizedData
    
  }
}
  
### Find total points for each team per week

scoreList[[1]]
  
