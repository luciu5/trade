require(shiny)
require(rhandsontable)
require(trade)


shinyUI(navbarPage("",id = "menu",
           tabPanel("Tariffs",

fluidPage(


  titlePanel("Simulate a Tariff") ,

    sidebarLayout(fluid=TRUE,
      sidebarPanel(

      htmlOutput("urlText"),hr(),

       h5(tags$b("Directions:")),
        helpText(tags$ul(
                 tags$li("Copy and paste (or enter) information into Inputs table (right) to simulate an", tags$em("ad valorem"),"tariff."),
                 tags$li("Default example simulates an increase in the ", tags$em("ad valorem"),"tariff (expressed as a proportion of consumer price) from 5% to 25% on products produced by 'Firm1' and 'Firm2'."),
                 tags$li("Products without current or new tariffs are assumed to be produced domestically. Otherwise, products are assumed to be produced abroad.")
                 #,tags$li(helpText("See the",tags$a(href=system.file('trade_shiny', package='trade'), "trade"),"R package vignette for more details about the models used here." ))
                 #tags$li("Shares must be between 0 and 1."),
                 #tags$li("Margins should exclude fixed costs.")
                 )
                 ),hr(),
        sliderInput("addRows", "Add rows to Inputs table:", value=10,min=5,max=50,step=5),
       radioButtons("calcElast", "Calibrate model parameters using:",
                     choices = c("market elasticity and 1 or more margins",
                                 "2 or more margins"), selected="market elasticity and 1 or more margins"
                    ),
        conditionalPanel(
          condition = "input.calcElast.includes('elasticity') == true ",
          numericInput("enterElast", "Enter Market Elasticity:", value=-1,min=-Inf,max=0,step=.1#, width='75%'
                       )
        ),hr(),

        radioButtons("supply", "Competitive Interaction:",
                    choices = c("Bertrand",
                               # "2nd Score Auction",
                                "Cournot"
                                )),

          selectInput("demand", "Demand Specification:",
                      choices = c("logit", "ces",
                                  #"linear",
                                  "aids")),
       hr(),
        conditionalPanel(
          condition = "input.supply == 'Cournot'",
          helpText(tags$b("Note:"), "only the first non-missing inputted price and product name is used for Cournot.")
          ),
       conditionalPanel(
         condition = "input.supply == '2nd Score Auction' && input.calcElast.includes('elasticity') == true",
         helpText(tags$b("Note:"), "2nd score Auction only requires a single price.")
       ),
       conditionalPanel(
         condition = "input.supply == '2nd Score Auction' && input.calcElast.includes('elasticity') == false",
         helpText(tags$b("Note:"), "2nd Score Auction does not require prices.")
       ),
       conditionalPanel(
         condition = "input.supply == 'Bertrand' && input.demand == 'aids'",
         helpText(tags$b("Note:"), "aids does not require pricing information.")
       )
      ),
      mainPanel(
        h2("Enter Inputs"),
        rHandsontableOutput("hot"), br(),
        #tags$head(
        #  tags$style(HTML('#run{color:white;background-color:black}'))
        #),
        actionButton(inputId ="simulate" , label = tags$b("Simulate"),style='padding:4px; font-size:125%')
      #)
        ,
        br(), br(),br(),
        tabsetPanel(id = "inTabset",
          tabPanel("Summary", value = "respanel", br(),br(),tableOutput("results"), br(),
                   helpText(tags$b("Note:"), "all price changes are (new tariff) share-weighted averages.
                            Negative Consumer Harm or Net Harm numbers denotes benefit.")
          ),
          tabPanel("Details", value = "detpanel", br(),br(), tableOutput("results_shareOut"),br(), tableOutput("results_detailed")

                   #,conditionalPanel("input.demand == 'aids' || input.demand == 'ces' || input.demand == 'ces (unknown elasticity)'",
                   #                  helpText(tags$b("Note:"), "shares are revenue-based.")
                   #)
                   ),
          tabPanel("Elasticities", value = "elastpanel",  br(),br(),
                   radioButtons("pre_elast", "",
                                                choices = c("Current Tariff",
                                                            "New Tariff"
                                                ), inline = TRUE),
                    br(),
                   tableOutput("results_mktelast"),br(),
                   tableOutput("results_elast"),
                   conditionalPanel("input.supply !=='Cournot'",
                                    checkboxInput("diversions", "Report diversion ratios", value =FALSE),
                   helpText(tags$b("Note:"), "diagonal elements are own-price elasticities.","Off-diagonal elements are the cross-price elasticities of row with respect to column.")
                   ),
                   conditionalPanel("input.supply == 'Cournot'",
                                    helpText(tags$b("Note:"), "above are own-price elasticities")
                   )
                   ),
          tabPanel("Diagnostics", value = "diagpanel", br(),br(), h4("Inputted vs. Fitted Values"),
                   tableOutput("results_diag_elast"),
                   tableOutput("results_diagnostics"),
                   htmlOutput("overIDText"),br(),
                   #helpText(tags$b("Note:"), "Negative numbers mean that observed values are larger than predicted values."),br(),
                   h4("Parameters"),verbatimTextOutput("parameters"),
                   helpText("See the",tags$a(href="https://CRAN.R-project.org/package=antitrust", "antitrust"),"R package vignette for more details about the parameters displayed here." )
          ),
          tabPanel("R Code", value = "codepanel", br(),verbatimTextOutput("results_code")),
          tabPanel("Messages", value = "msgpanel", br(),h4("Warnings"),  verbatimTextOutput("warnings"), br(),h4("Errors"),  verbatimTextOutput("errors"))

        )

      )

    )
)

  )

 ,tabPanel("Quotas",

          fluidPage(


            titlePanel("Simulate a Quota") ,

           sidebarLayout(
             sidebarPanel(

               htmlOutput("urlTextQuota"),hr(),

               h5(tags$b("Directions:")),
               helpText(tags$ul(
                 tags$li("Copy and paste (or enter) information into Inputs table (right) to simulate a quota."),
                 tags$li("Default example simulates an increase in the quota from 100% of current output to 75% of current output on products produced by 'Firm1' and 'Firm2'."),
                 tags$li("Products without current or new quotas are assumed to be produced domestically. Otherwise, products are assumed to be produced abroad.")
                 #,tags$li(helpText("See the",tags$a(href=system.file('trade_shiny', package='trade'), "trade"),"R package vignette for more details about the models used here." ))
                 #tags$li("Shares must be between 0 and 1."),
                 #tags$li("Margins should exclude fixed costs.")
               )
               ),hr(),
               sliderInput("addRowsQuota", "Add rows to Inputs table:", value=10,min=5,max=50,step=5),
               radioButtons("calcElastQuota", "Calibrate model parameters using:",
                            choices = c("market elasticity and 1 or more margins",
                                        "2 or more margins"), selected="market elasticity and 1 or more margins"
               ),
               conditionalPanel(
                 condition = "input.calcElastQuota.includes('elasticity') == true ",
                 numericInput("enterElastQuota", "Enter Market Elasticity:", value=-1,min=-Inf,max=0,step=.1#, width='75%'
                 )
               ),hr(),

               radioButtons("supplyQuota", "Competitive Interaction:",
                            choices = c("Bertrand"
                                        #,"2nd Score Auction"
                                        #,"Cournot"
                            )),

               selectInput("demandQuota", "Demand Specification:",
                           choices = c("logit"#, "ces",
                                       #"linear",
                                       #"aids"
                                       )),
               hr(),
               conditionalPanel(
                 condition = "input.supplyQuota == 'Cournot'",
                 helpText(tags$b("Note:"), "only the first non-missing inputted price and product name is used for Cournot.")
               ),
               conditionalPanel(
                 condition = "input.supplyQuota == '2nd Score Auction' && input.calcElastQuota.includes('elasticity') == true",
                 helpText(tags$b("Note:"), "2nd score Auction only requires a single price.")
               ),
               conditionalPanel(
                 condition = "input.supplyQuota == '2nd Score Auction' && input.calcElastQuota.includes('elasticity') == false",
                 helpText(tags$b("Note:"), "2nd Score Auction does not require prices.")
               ),
               conditionalPanel(
                 condition = "input.supplyQuota == 'Bertrand' && input.demandQuota == 'aids'",
                 helpText(tags$b("Note:"), "aids does not require pricing information.")
               )
             ),
              mainPanel(
               h2("Enter Inputs"),
               rHandsontableOutput("hotQuota"), br(),
               #tags$head(
               #  tags$style(HTML('#run{color:white;background-color:black}'))
               #),
               actionButton(inputId ="simulateQuota" , label = tags$b("Simulate"),style='padding:4px; font-size:125%')
               #)
               ,
               br(), br(),br(),
               tabsetPanel(id = "inTabsetQuota",
                           tabPanel("Summary", value = "respanelQuota", br(),br(),tableOutput("resultsQuota"), br(),
                                    helpText(tags$b("Note:"), "all price changes are (new quota) share-weighted averages.
                                             Negative Consumer Harm or Net Harm numbers denotes benefit.")
                                    ),
                           tabPanel("Details", value = "detpanelQuota", br(),br(), tableOutput("results_shareOutQuota"),br(), tableOutput("results_detailedQuota")

                                    #,conditionalPanel("input.demand == 'aids' || input.demand == 'ces' || input.demand == 'ces (unknown elasticity)'",
                                    #                  helpText(tags$b("Note:"), "shares are revenue-based.")
                                    #)
                           ),
                           tabPanel("Elasticities", value = "elastpanelQuota",  br(),br(),
                                    radioButtons("pre_elastQuota", "",
                                                 choices = c("Current Quota",
                                                             "New Quota"
                                                 ), inline = TRUE),
                                    br(),
                                    tableOutput("results_mktelastQuota"),br(),
                                    tableOutput("results_elastQuota"),
                                    conditionalPanel("input.supplyQuota !=='Cournot'",
                                                     checkboxInput("diversionsQuota", "Report diversion ratios", value =FALSE),
                                                     helpText(tags$b("Note:"), "diagonal elements are own-price elasticities.","Off-diagonal elements are the cross-price elasticities of row with respect to column.")
                                    ),
                                    conditionalPanel("input.supplyQuota == 'Cournot'",
                                                     helpText(tags$b("Note:"), "above are own-price elasticities")
                                    )
                           ),
                           tabPanel("Diagnostics", value = "diagpanelQuota", br(),br(), h4("Inputted vs. Fitted Values"),
                                    tableOutput("results_diag_elastQuota"),
                                    tableOutput("results_diagnosticsQuota"),
                                    htmlOutput("overIDTextQuota"),br(),
                                    #helpText(tags$b("Note:"), "Negative numbers mean that observed values are larger than predicted values."),br(),
                                    h4("Parameters"),verbatimTextOutput("parametersQuota"),
                                    helpText("See the",tags$a(href="https://CRAN.R-project.org/package=antitrust", "antitrust"),"R package vignette for more details about the parameters displayed here." )
                           ),
                           tabPanel("R Code", value = "codepanelQuota", br(),verbatimTextOutput("results_codeQuota")),
                           tabPanel("Messages", value = "msgpanelQuota", br(),h4("Warnings"),  verbatimTextOutput("warningsQuota"), br(),h4("Errors"),  verbatimTextOutput("errorsQuota"))

                           )

            )

            )
          )

 )

,tabPanel("Documentation",
         fluidPage(htmlOutput("reference")
                   )
)
)

)
