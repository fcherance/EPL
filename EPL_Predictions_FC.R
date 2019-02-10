library(rvest)
library(data.table)

# English Premier League (EPL) --------------------------------------------

WebsiteString <-"https://www.bbc.co.uk/sport/football/premier-league/"

EPL_table <- read_html(paste0(WebsiteString,
                              "table")
                       )

EPL_fixtures <- read_html(paste0(WebsiteString,
                                "/scores-fixtures/",
                                 format(Sys.Date(),"%Y-%m"),
                                 "?filter=fixtures")
                          )

# EPL functions -----------------------------------------------------------

# Fetch fields from EPL html
EPL_fields <- 
  EPL_table %>% 
  html_nodes(".gs-o-table__cell--right") %>%
  html_text() %>%
  head(,n=8L)

EPL_TeamsStandings <-
  EPL_table %>%
  html_nodes("td.gs-o-table__cell--left") %>%
  html_text()

EPL_UpdateTime <- EPL_TeamsStandings[41]

EPL_TeamsStandings <- EPL_TeamsStandings[1:40] %>%
  matrix(ncol=2,byrow=T)

EPL_GameStandings<-
  EPL_table %>% 
  html_nodes("td.gs-o-table__cell--right") %>%
  html_text() %>%
  matrix(ncol=8,byrow=T) %>%
  as.data.frame

colnames(EPL_GameStandings) <- EPL_fields  
rownames(EPL_GameStandings) <- EPL_TeamsStandings[,1]

EPL_tonum <- function(x) if (length(grep("[1-9]",unique(x)))>0) {as.numeric(as.character(x))} else {as.character(x)}
EPL_stats <- as.data.frame(sapply(EPL_GameStandings,EPL_tonum))
EPL_stats$Team <- rownames(EPL_GameStandings)

# EPL Table creation ------------------------------------------------------

EPL_stats$FOR_AVG <- EPL_stats$F / EPL_stats$P
EPL_stats$AGAINST_AVG <- EPL_stats$A / EPL_stats$P
EPL_stats$DIFF_AVG <- EPL_stats$GD / EPL_stats$P

EPL_stats <- data.table(EPL_stats)

# EPL Fixtures Predictions ------------------------------------------------

EPL_fixtures_t <- 
EPL_fixtures %>%
  html_nodes("article") %>%
  html_text()

EPL_fixtures_s <-
EPL_fixtures_t %>%
  tstrsplit(":",fixed=TRUE)

fixtures <-data.table(Home=gsub("[0-9]","",unlist(EPL_fixtures_s[[1]])),
              Away=gsub("[0-9]","",unlist(EPL_fixtures_s[[2]])))

figures <- setdiff(names(EPL_stats),"Team")

fixtures$Home_Goals <- 0
fixtures$Away_Goals <- 0
fixtures$Home_Expect <- 0
fixtures$Away_Expect <- 0


LU_ClubNames <- sapply(EPL_stats$Team,
function(x)grep(x,unique(c(fixtures$Home,fixtures$Away)),value=T)
)

fixtures$Home<-names(LU_ClubNames[match(fixtures$Home,LU_ClubNames)])
fixtures$Away<-names(LU_ClubNames[match(fixtures$Away,LU_ClubNames)])


for (i in 1:nrow(fixtures)){

  figdiffs <- EPL_stats[Team==fixtures$Home[i],figures,with=F] 
              - 
              EPL_stats[Team==fixtures$Away[i],figures,with=F]


  fixtures$Home_Expect[i] <- 
    max(
      # Expected FOR deteriorated by an opposer's good defence 
      # (Away team against goals low)
      EPL_stats[Team==fixtures$Home[i],FOR_AVG]*
                  (EPL_stats[Team==fixtures$Away[i],AGAINST_AVG]/5)+
        # The differenciation factor is the gap in goals difference 
        # (half applied on Home, half on away)
        EPL_stats[Team==fixtures$Away[i],AGAINST_AVG]+figdiffs$DIFF_AVG/2,
      0)
  
  fixtures$Away_Expect[i] <- 
    max(
      # Formulas are the symmetrical to the ones applied for Home team
      EPL_stats[Team==fixtures$Away[i],FOR_AVG]*
        (EPL_stats[Team==fixtures$Home[i],
                                   AGAINST_AVG]/5)+
        EPL_stats[Team==fixtures$Home[i],
                                  AGAINST_AVG]-
                        figdiffs$DIFF_AVG/2,
        0)
  
  
  fixtures$Home_Goals[i] <- floor(fixtures$Home_Expect[i])
  fixtures$Away_Goals[i] <- floor(fixtures$Away_Expect[i])
}

View(fixtures)

write.csv(fixtures[,1:4,with=F],paste("C:\\Users\\Fabien\\Desktop\\EPL_Predictions_FC",
                                      Sys.Date(),".csv"),row.names = F)
