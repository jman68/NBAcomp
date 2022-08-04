# pull in latest batch of current NBA players
players <- getPlayers(as.integer(format(Sys.Date(), "%Y")))

ui <- dashboardPage(
  dashboardHeader(title = "Basic Comparison of Current NBA Players", titleWidth = 450),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarid",
      menuItem("Career Averages (Player 1)", tabName = "Career_Averages", icon = icon("dashboard")),
      menuItem("Game Logs (Player 1)", tabName = "Game_Logs", icon = icon("th")),
      menuItem("Compare (Player 1 vs. Player 2)", tabName = "Compare", icon = icon("th"))
    ),
    column(align='center',
           width=12,
           htmlOutput("picture")),
    column(align='center',
           width=12,
           selectInput(label = "Player 1:",
                inputId = "player",
                selected = "LeBron James",
                choices = players)),
    conditionalPanel(
      'input.sidebarid == "Game_Logs"',
      column(align='center',
             width=12,
             selectInput(label = "Year",
                         inputId = "year",
                         selected = 2022,
                         choices = 2000:2022))
    ),
    conditionalPanel(
      'input.sidebarid == "Career_Averages"',
      column(align='center',
             width=12,
             selectInput(label = "Regular Season/Playoffs",
                         inputId = "stats_type",
                         selected = "Regular Season",
                         choices = c("Regular Season", "Playoffs")))
    ),
    conditionalPanel(
      'input.sidebarid == "Compare"',
      column(align='center',
             width=12,
             htmlOutput("picture2")),
      column(align='center',
             width=12,
             selectInput(label = "Player 2:",
                         inputId = "player2",
                         selected = "Anthony Davis",
                         choices = players))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Career_Averages",
        fluidRow(
          box(
            width = 24,
            solidHeader = TRUE,
            infoBoxOutput("infobox_team"),
            infoBoxOutput("infobox_age"),
            infoBoxOutput("infobox_pts"),
            infoBoxOutput("infobox_reb"),
            infoBoxOutput("infobox_ast"),
            infoBoxOutput("infobox_tov")
          ),
          box(
            width = 12,
            solidHeader = TRUE,
            DT::dataTableOutput("table_career_stats")
          )
        )
      ),
      tabItem(tabName = "Game_Logs",
        fluidRow(
          box(
            width = 12,
            solidHeader = TRUE,
            DT::dataTableOutput("table_game_logs")
          )
        )
      ),
      tabItem(tabName = "Compare",
        fluidRow(
          box(
            width = 4,
            solidHeader = TRUE,
            title = "Comparison Statistic",
            selectInput('stats', 'Stat to compare:', "PTS")
          ),
          box(
            width = 8,
            solidHeader = TRUE,
            plotOutput("plot1")
          )
        ),
        fluidRow(
          box(
            width = 4,
            solidHeader = TRUE,
            title = "Comparison Statistic",
            selectInput('stats2', 'Stat to compare:', "AST")
          ),
          box(
            width = 8,
            solidHeader = TRUE,
            plotOutput("plot2")
          )
        )
      )
    )
  )
)




