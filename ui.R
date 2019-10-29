#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define UI for application that draws a histogram
ui<-dashboardPage(
  dashboardHeader(title="Forbes Ranking Stats"),
  dashboardSidebar(
    sidebarUserPanel("Yan Mu", image = 'Forbes.jpg'),
    sidebarMenu(
      menuItem("Introduction",tabName="intro",icon=icon("list-alt")),
      menuItem("Ranking Data Exploer",tabName="ranking",icon=icon("table")),
      menuItem("Company", tabName="top_profits_companies",icon=icon("alicorn")),
      menuItem("Countries", tabName="top_ranking_countries",icon=icon("alicorn")),
      menuItem("Industries", tabName="top_ranking_industries",icon =icon("flag-checkered")),
      menuItem("Analysis",tabName="profits_volitilities", icon=icon("analytics")),
      menuItem("References",tabName = "references",icon =icon("asterisk"))
    ),
    selectizeInput(
      "year",
      label = 'Select Year:',
      choices = list("2019","2018","2017","2018_USA")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",
              fluidRow(
                box(
                  h3("Global 2000: The World's Largest Public Compnies"),
                  tags$p(
                    "It’s been a rollercoaster of a year for global markets: Stocks saw both major gains and losses as volatility ticked upwards, trade tensions between the United States and China heated up, and many companies got a boost from new American corporate tax legislation." ),
                  tags$p(
                    "Forbes’ annual ranking of the world’s largest public companies is a reflection of the state of the global economy today: who’s on top, who’s growing and who’s seen better days."),
                  tags$p("More details can be found"),
                  tags$a(href = "https://www.forbes.com/global2000/#3b41e172335d","HERE!"),
                  width = 12
                )
              ),
              fluidRow(
                box(
                  h3("What is our research goal?"),
                  tags$p(
                    "Our agenda is to figure out the largest companies ranking breakdown by countries and industries,the profits gain and international competitiveness." ),
                  tags$p(
                    "This app could specifically benefit investment decision making, forecasting profits volitilites by ranking and industries."),
                  tags$br(),
                  tags$br(),
                  width = 12
                )
              ),
              
              plotlyOutput("total_stats")
      ),
      tabItem(tabName = "ranking",
              fluidRow(
                infoBoxOutput("TotalAveBox"),
                infoBoxOutput("TotalProfitsBox")
              ),
              fluidRow(
                infoBoxOutput("TotalAssetBox"),
                infoBoxOutput("TotalMktVBox")
              ),
              fluidRow(box(DT::dataTableOutput("dynamic_data_table"), width = 12))
      ),
              
      #####################################################
      tabItem(tabName ="top_profits_companies",
              fluidRow(
                box(
                  title = "Select year",
                  solidHeader = T,
                  width = NULL,
                  status = "info",
                  plotlyOutput("top_profits")
                )
              )
      ),
      # #####################################################
      tabItem(tabName ="top_ranking_countries",
              fluidRow(
                column(
                  width = 9,
                  box(
                    title = "Where The Worlds Largest Companies Are",
                    solidHeader = T,
                    status = "info",
                    plotOutput("breakdown_countries"),
                    width = NULL,
                    height = "auto"
                  )
                )
              ),
              fluidRow(
                column(
                  width = 9,
                  box(
                    title = "US vs China",
                    solidHeader = T,
                    status = "info",
                    tags$img(src = 'us_vs_china.png', width=600, height=400),
                    width = NULL,
                    height = "auto"
                  )
                )
              )
      ),
      # ##################################################################################
      tabItem(tabName = "top_ranking_industries",
              fluidRow(
                column(
                  width = 9,
                  box(
                    title = " Top industries of the Global 2000",
                    solidHeader = T,
                    status = "info",
                    plotlyOutput("breakdown_sector2017"),
                    plotlyOutput('breakdown_sector2018USA'),
                    width = NULL,
                    height = "auto"
                  )
                )
              )
      ),
      # #################################
      tabItem(
        tabName = "profits_volitilities",
        fluidRow(
          column(
            width = 9,
            box(
              title = " 2018 USA breakdown by profits",
              solidHeader = T,
              status = "info",
              plotlyOutput("breakdown_profit2018USA"),
              plotlyOutput("breakdown_rankvol2018USA"),
              plotlyOutput("breakdown_sectorvol2018USA"),
              width = NULL,
              height = "auto"
            )
          )
        )
      ),
      
      ##################################
      tabItem(tabName = "references",
              fluidRow(
                box(
                  strong("My Shiny Project code:"),
                  tags$br(),
                  tags$a(
                    href = "","-> Link to GitHub"),
                  tags$br(),
                  strong("Dataset Links:"),
                  tags$br(),
                  tags$a(href = "https://www.kaggle.com/ash316/forbes-top-2000-companies#Forbes%20Top2000%202017.csv", " 1)	Forbes 2000 2019 ranking"),
                  tags$br(),
                  tags$a(href = "https://www.forbes.com/sites/forbespr/2018/06/06/forbes-releases-16th-annual-global-2000-ranking-of-the-worlds-largest-public-companies/#171f859b12a0", " 2)	forbes 2000 2018 ranking"),
                  tags$br(),
                  tags$a(href = "https://www.forbes.com/sites/antoinegara/2017/05/24/2017-global-2000-the-worlds-largest-transportation-companies/#1bb06597406d", " 3) forbes 2000 2017 ranking"),
                  tags$br(),
                  tags$a(href = "https://www.kaggle.com/Eruditepanda/fortune-1000-2018", " 4)	Fortune 1000 2018 USA ranking"),
                  tags$br(),
                  tags$br(),
                  strong("Future Work:"),
                  tags$br(),
                  tags$p("1)	Research main economic factors volitilities linear relationship"),
                  tags$p("2)	Does this volitilities effect stock market price?"),
                  tags$p("3)	Notable newcomers from M&A and IPO hit the ranking"),
                  tags$p("4)   How USA ranking effect Global 2000?"),
                  tags$p("5)   International competitiveness between countries")
                )
              )
      )
    )
  )
)