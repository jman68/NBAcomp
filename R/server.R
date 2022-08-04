server <- function(input,output,session) {
  ### Reactive values section ------------------------------------------------
  # player images
  playerImage <- reactive({
    getPlayerImage(input$player)
  })
  playerImage2 <- reactive({
    getPlayerImage(input$player2)
  })

  # player URLs
  playerURL <- reactive({
      getURL(input$player)
  })
  player2URL <- reactive({
    getURL(input$player2)
  })

  # type of stats
  statsType <- reactive({
    if (input$stats_type == "Regular Season") {2} else {3}
  })

  # player stats tables
  playerTable <- reactive({
    req(statsType())
    getStatsTable(input$player, statsType())
  })
  playerTable2 <- reactive({
    req(statsType())
    getStatsTable(input$player2, statsType())
  })

  gamelogTable <- reactive({
    getGameLog(input$player, input$year)
  })

  ### Output section ------------------------------------------------------------

  # player infoboxes
  output$infobox_team <- renderInfoBox({
    infoBox("Team", playerTable() %>% slice_tail(n = 1) %>% select(Tm) %>% pull())
  })
  output$infobox_age <- renderInfoBox({
    infoBox("Age", playerTable() %>% slice_tail(n = 1) %>% select(Age) %>% pull())
  })
  output$infobox_pts <- renderInfoBox({
    infoBox("PPG", playerTable() %>% slice_tail(n = 1) %>% select(PTS) %>% pull())
  })
  output$infobox_reb <- renderInfoBox({
    infoBox("RPG", playerTable() %>% slice_tail(n = 1) %>% select(TRB) %>% pull())
  })
  output$infobox_ast <- renderInfoBox({
    infoBox("APG", playerTable() %>% slice_tail(n = 1) %>% select(AST) %>% pull())
  })
  output$infobox_tov <- renderInfoBox({
    infoBox("TOVPG", playerTable() %>% slice_tail(n = 1) %>% select(TOV) %>% pull())
  })

  # player images
  output$picture <- renderText({
    c('<img src="', playerImage(), '">')
  })
  output$picture2 <- renderText({
    c('<img src="', playerImage2(), '">')
  })

  # player stats tables
  output$table <- DT::renderDataTable(DT::datatable({
    playerTable()
  }))
  output$table2 <- DT::renderDataTable(DT::datatable({
    gamelogTable()
  }))

  # comparison plots
  output$plot1 <- renderPlot(
    ggplot() +
      geom_line(playerTable(), mapping = aes_string('Age',input$stats, colour = '"red"')) +
      geom_line(playerTable2(), mapping = aes_string('Age',input$stats, colour = '"blue"')) +
      scale_color_discrete(name = "Legend", labels = c(input$player2, input$player)) +
      scale_x_continuous(breaks = seq(18, 40, by = 1)) +
      ggtitle(paste("Age vs.",input$stats)) +
      theme(text = element_text(size=16)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      theme_bw()
  )

  observe({
    updateSelectInput(session, "stats",
                      choices = playerTable() %>% select(-Season, -Age, -Tm, -Lg, -G, -GS, -Pos) %>% names(),
                      selected = 'PTS'
    )})
}
