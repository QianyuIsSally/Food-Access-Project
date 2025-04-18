# Load the data 
dat <- read.csv(file = "https://raw.githubusercontent.com/sarahlotspeich/ACSnavigatR/refs/heads/main/ACSnavigatR_data.csv")

# Define levels for factor variables 
dat$AGE = factor(x = dat$AGE, 
                 levels = c("ALL AGES", "UNDER 18 YEARS", "18 YEARS AND OVER", 
                            "UNDER 5 YEARS", "5 TO 9 YEARS", "10 TO 14 YEARS", 
                            "15 TO 17 YEARS", "18 AND 19 YEARS", "20 TO 24 YEARS", 
                            "25 TO 29 YEARS", "30 TO 34 YEARS", "35 TO 44 YEARS", 
                            "45 TO 54 YEARS", "55 TO 64 YEARS", "65 TO 74 YEARS",
                            "75 TO 84 YEARS", "85 YEARS AND OVER" ))

server <- function(input, output) {
  # Initialized filtered_dat at original dat
  filtered_dat <- dat 
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    if (input$sex != "ALL SEXES") {
      filtered_dat <- filtered_dat[filtered_dat$SEX == input$sex, ]
    }
    if (input$age != "ALL AGES") {
      filtered_dat <- filtered_dat[filtered_dat$AGE == input$age,]
    }
    if (input$nativity != "ALL NATIVITIES") {
      filtered_dat <- filtered_dat[filtered_dat$NATIVITY == input$nativity,]
    }
    if (input$citizen != "ALL CITIZENSHIPS") {
      filtered_dat <- filtered_dat[filtered_dat$CITIZENSHIP == input$citizen,]
    }
    filtered_dat
  }))
  
  output$download <- downloadHandler(
    filename = function(){"thename.csv"}, 
    content = function(fname){
      write.csv(thedata(), fname)
    }
  )
}