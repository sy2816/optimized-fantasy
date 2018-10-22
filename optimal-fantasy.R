library(RSelenium)
library(rvest)
library(XML)
library(magrittr)
library(stringr) 
library(dplyr)
library(getPass)

#shell('docker pull selenium/standalone-chrome')

#pass word for ESPN
pass <- getPass()

rd <- rsDriver()

remDr <- rd[["client"]]
  
remDr$navigate(url = "http://games.espn.com/ffl/schedule?leagueId=309844&teamId=4")

#Add info the username/password fields
#The username/password inputs are on an iframe document, so I need to switch to the iframe before I search for elements
remDr$switchToFrame(remDr$findElement("tag name", "iframe"))

#Username/Password input
remDr$findElements(using = "xpath", '//*[@id="did-ui-view"]/div/section/section/form/section/div[1]/div/label/span[2]/input')
remDr$sendKeysToActiveElement(sendKeys = list("syork2816@gmail.com", key = "tab", pass, key = "enter")
)

#Sleep, otherwise page will load source to early and will return blanks.
Sys.sleep(1.0)

source <- remDr$getPageSource()[[1]]

#Collect links to each game
scheduleLinks <- read_html(source) %>%
  html_node(xpath = '//*[@id="content"]/div/div[4]/div/div/div[2]/table/tbody') %>%
  html_nodes("a") %>%
  html_attr("href")

# Isolate links to only those for the game
scoreIndex <- grep(x = scheduleLinks, pattern = "boxscorequick")
scheduleLinks <- scheduleLinks[scoreIndex]

# Create final url
scheduleLinks <- paste0("http://games.espn.com", scheduleLinks)

#Add results to list
scoreList <- list()

# Navigate to each game
for(i in c(1:length(scheduleLinks))){
#i <- 1
  remDr$navigate(scheduleLinks[i])
  
  Sys.sleep(1.5)
  
  # Find the link to show the bench players and click on it
  showBench <- remDr$findElement(using = "xpath", value = '//*[@id="content"]/div/div[4]/div/div/div/div[5]/div[3]/a')
  showBench$clickElement()
  
  Sys.sleep(1.0)
  
  source <- remDr$getPageSource()[[1]]
  
  ### Create table showing roster of players played
  playingRoster <- read_html(source) %>%
    html_node(xpath = '//*[@id="playertable_0"]') %>%
    html_table(fill = TRUE)
  
  #Only keep the first few columns and rename them
  playingRoster <- playingRoster[-c(1:3),c(1:5)]
  colnames(playingRoster) <- c("position", "player", "opponent", "status", "pts")
  
  # Create table of players that are benched
  ### Create table showing roster of players played
  benchRoster <- read_html(source) %>%
    html_node(xpath = '//*[@id="playertable_1"]') %>%
    html_table(fill = TRUE)
  
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
      pos <- substr(x = player, start = (nchar(player)-1), stop = nchar(player))
      benchRoster$position[a] <- pos
  }
  
  #combine playing roster and bench roster
  data <- rbind(playingRoster, benchRoster)
  data$pts <- as.numeric(data$pts)
  
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
    tempData <- filter(tempData, player != QB$player)
  
    #RB1 
    RB1 <- bestPos(tempData, "RB")
    tempData <- filter(tempData, player != RB1$player)
    
    #RB2
    RB2 <- bestPos(tempData, "RB")
    tempData <- filter(tempData, player != RB2$player)
    
    #WR1
    WR1 <- bestPos(tempData, "WR")
    tempData <- filter(tempData, player != WR1$player)
    
    #WR2 
    WR2 <- bestPos(tempData, "WR")
    tempData <- filter(tempData, player != WR2$player)
    
    #TE
    TE <-bestPos(tempData, "TE")
    tempData <- filter(tempData, player != TE$player)
    
    #FLEX
    FLEX <- filter(tempData, position %in% c("WR", "TE", "RB"))
    FLEX <- na.omit(FLEX)
    FLEX <- FLEX[which(FLEX$pts == max(FLEX$pts)),]
    tempData <- filter(tempData, player != FLEX$player)
    
    #DEF/ST
    DEF <- bestPos(tempData, "D/ST")
    tempData <- filter(tempData, player != DEF$player)
    
    #K
    K <- bestPos(tempData, "K")
    tempData <- filter(tempData, player != K$player)
    
    # Combine into optimized roster
    optimalData <- rbind(QB, RB1, RB2, WR1, WR2, FLEX, DEF, K)
    
    return(optimalData)
  }

  optimizedData <- optimizeRoster(data)
  print(optimizedData)
  
  week <- paste0("week", i)
  scoreList[[week]] <- optimizedData
  
}
  
  
  
  
  
  
