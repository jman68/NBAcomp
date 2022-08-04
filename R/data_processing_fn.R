#' Obtain list of active NBA players for a specified year
#'
#' @param year integer denoting year between [1947] and [2022]
#' @return list of active NBA players
#'
#' @export
getPlayers <- function(year) {
  pergURL <- paste("http://www.basketball-reference.com/leagues/NBA_", year, "_per_game.html", sep="")
  perg <- pergURL %>% read_html() %>% html_table()
  perg <- as.data.frame(perg, stringsAsFactors=FALSE, header=TRUE)
  players <- unique(perg$Player[perg$Player != "Player"])
  return(players)
}

#' Obtain web URL for given NBA player name
#'
#' @param player string denoting NBA player
#' @return string denoting [www.basketball-reference.com] web URL
#'
#' @export
getURL <- function(player) {
  # clean player names
  player <- tolower(unlist(strsplit(player, " ")))
  player <- iconv(player, to='ASCII//TRANSLIT')
  player <- gsub("[[:punct:]]", "", player)

  # coding for special URL cases
  if(player[2] == "barea") {
    player[1] <- "jo"
  }
  if(player[2] == "capela") {
    player[1] <- "caint"
  }

  # get player URL
  playerURL <- paste("https://www.basketball-reference.com/players/",
                     substring(player[2],1,1), "/", substring(player[2],1,5),
                     substring(player[1],1,2), "01.html", sep="")

  # special cases where player code [first 5 letters lastname][first 2 letters firstname] not unique
  # or special cases where player code does not follow aforementioned pattern
  if(player[1] == "dairis"){
    playerURL <- "https://www.basketball-reference.com/players/b/bertada02.html"
  }
  if(grepl("bert",player[2]) && grepl('ns',player[2]) && grepl("vis", player[1])){
    playerURL <- "https://www.basketball-reference.com/players/b/bertada01.html"
  }
  if(grepl("ntilikina", player[2])) {
    playerURL <-"https://www.basketball-reference.com/players/n/ntilila01.html"
  }
  if(grepl("wallerprince", player[2])) {
    playerURL <-"https://www.basketball-reference.com/players/p/princta02.html"
  }
  if(grepl("anthony", player[1]) && grepl("davis", player[2])) {
    playerURL <-"https://www.basketball-reference.com/players/d/davisan02.html"
  }
  return(playerURL)
}

#' Obtain stats table for a specific player for a specific year
#'
#' @param player string denoting NBA player
#' @param flag integer denoting which table to retrieve (2 = per game stats, 4 = per 36 stats). defaults to 2
#' @return string denoting [www.basketball-reference.com] web URL
#'
#' @export
getStatsTable <- function(player, flag=2) {
  player_url <- getURL(player)
  player_df <- player_url %>% read_html() %>% html_table()
  stats_df <- as.data.frame(player_df[[flag]]) %>%
    filter(Lg == "NBA") %>%
    filter(!is.na(Age)) %>%
    group_by(Season) %>% mutate(n_teams = n()) %>%
    mutate(n_teams = ifelse(Tm == "TOT", 1, n_teams)) %>% # collapses years where player played for multiple teams
    filter(n_teams == 1)
  return(as.data.frame(stats_df))
}

#' Obtain game logs for a specific player for a specific year
#'
#' @param player string denoting NBA player
#' @param year integer denoting year to get game logs
#' @return string denoting [www.basketball-reference.com] web URL
#'
#' @export
getGameLog <- function(player, year) {
  player_url <- getURL(player)
  base_url <- substr(player_url,1,nchar(player_url)-5)
  game_log_url <- glue::glue("{base_url}/gamelog/{year}")

  tryCatch({
    game_logs <- game_log_url %>% read_html() %>% html_table(fill = TRUE)
  }, warning = function(w) {
  }, error = function(e) {
    return("No available data for selected year!")
  })

  game_log_df <- as.data.frame(game_logs[length(game_logs)])
  out_table <- game_log_df %>%
    filter(!GS %in% c("Inactive","Did Not Dress","Did Not Play","GS")) %>%
    rename("+/-" = "X...",
           "H/A" = "Var.6",
           "SPRD" = "Var.8") %>%
    select(-Rk, -Age)
  return(out_table)
}

#' Obtain game logs for a specific player for a specific year
#'
#' @param player string denoting NBA player
#' @param year integer denoting year to get game logs
#' @return string denoting [www.basketball-reference.com] web URL
#'
#' @export
getPlayerImage <- function(player) {
  player_url <- getURL(player)
  imgsrc <- read_html(player_url) %>%
    html_nodes("img") %>%
    html_attr('src')
  return(imgsrc[2])
}
