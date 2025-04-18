
# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)
library(bslib)

# defines how the application looks and what inputs the user can provide.
ui <- fluidPage(
  titlePanel("ACSNavigatR"),
  selectInput( 
    "select", 
    "Select options below:", 
    list("Sex" = "Sex", "Age" = "Age", "Race" = "Race", "Nativity"= "Nativity", "Citizenship" = "Citizenship"), 
    multiple = TRUE 
  ), 
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(2,
           selectInput("sex",
                       "Sex:",
                       unique(dat$SEX))
    ),
    # AGE
    # unique(as.character(dat$age))?
    column(2,
           selectInput("age",
                       "Age:",
                       unique(dat$AGE))
    ),
    # RACE
    column(2,
           selectInput("race",
                       "RACE:",
                       unique(dat$RACE))
    ), 
    # NATIVITY 
    column(2,
           selectInput("nativity",
                       "NATIVITY:",
                       unique(dat$NATIVITY))
    ), 
    # CITIZENSHIP
    column(3,
           selectInput("citizen",
                       "CITIZENSHIP:",
                       unique(dat$CITIZENSHIP))
    ), 
  ),
  # download the result of a table
  downloadButton('download',"Download the data"),
  #display the filtered data as a data table
  DT::dataTableOutput("table")
)
