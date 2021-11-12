source("global.R")

dashboardPage(
  dashboardHeader(title = "Final Project Submission",
                  dropdownMenu( type = "messages", 
                                messageItem("my github","github", 
                                            href = "https://github.com/x0cristian0x/Data_Science_Capstone_Final" ),
                                messageItem("my app shiny", "shiny", 
                                            href = "https://x0cristian0x.shinyapps.io/Predict_Text/", 
                                            icon = icon("life-ring")))),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Enter Text",
               startExpanded = TRUE,
               menuSubItem( icon = NULL,
                 textInput("inputString", label = "Here Enter Text", value = "")  ) ,
               tabName = "Enter text", icon = icon("keyboard")),
      
      menuItem(text = "Logo", tabName = "Logo", icon = icon("r-project")
            ),
      
      menuItem(text = "info", tabName = "info", icon = icon("info-circle")),
      actionButton("click","Click To Predict Text", icon = icon("refresh"))
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Logo", 
              h2("Next word is:"), verbatimTextOutput("prediction"),
              plotlyOutput("plot")), 
        
      tabItem(tabName = "info", h2("Information"), includeMarkdown("about.md"))
      
      
      
    ))
)



