library(shiny)

# Load probability tables
new_bi <- read.csv("Kneser-Ney Bigram Probabilities.csv", header=T)
new_tri <- read.csv("Kneser-Ney Trigram Probabilities.csv", header=T)

# Define functions
# Use trigram prob tables to predict the next word following a two-word phrase
next_word_tri <- function(x){
    if (x %in% new_tri$bigrams_w1w2==F) {
        Rank <- "Please try again" 
        Prediction <- "No match"
        no_match <- data.frame(Rank, Prediction)
        no_match
    }
    else {
        subset <- new_tri[new_tri$bigrams_w1w2==x,]
        subset <- subset[,c(4,2)] 
        subset <- subset[complete.cases(subset),]
        names(subset) <- c("Rank", "Prediction")
        subset 
    }
}
x <- "it's not"

# Use bigram prob tables to predict the next word following a two-word phrase
next_word_bi <- function(x){
    if (x %in% new_bi$term1==F) {
        Rank <- "Please try again" 
        Prediction <- "No match"
        no_match <- data.frame(Rank, Prediction)
        no_match 
    }
    else {
        subset <- new_bi[new_bi$term1==x,]
        subset <- subset[,c(4,2)]
        subset <- subset[complete.cases(subset),]
        names(subset) <- c("Rank", "Prediction")
        subset
    }
}

# Combine the two functions
next_word <- function (x) {
    x <- tolower(x)
    if(sapply(gregexpr("\\S+", x), length) >= 2) {
        y <- word(x,-2, -1)
        trigram_pred <- next_word_tri(y)
        if (trigram_pred[1,2] == "No match") {
            z <- word(x,-1)
            next_word_bi(z)
        } else { 
            trigram_pred
        }
    } else {
        next_word_bi(x)
    }
}

# Create the UI object
ui <- shinyUI(fluidPage(
    
    # Main title
    navbarPage("Predict Next Word",
               
               # Tab1 title
               tabPanel("App",
                        
                        # Tab1 layout type 
                        sidebarLayout(
                            
                            # Tab1 layout container 1 (for input text)
                            sidebarPanel(

                                # Tab1 input content (input phrase)
                                textInput("phrase", "Enter a word or phrase here", "It’s not rocket"),
                                
                                # Tab1 inputs content (submit button)
                                submitButton("Predict Next Word")
                                
                                ),
                            
                            # Tab1 layout container 2 (for text prediction)
                            mainPanel(
                                
                                # Tab1 explanatory text for prediction content
                                h4(textOutput("caption")),
                                
                                br(),
                                
                                # Tab1 prediction content
                                tableOutput("prediction")
                            )
                        )
               ),
               
               # Tab 2 title
               tabPanel("User Guide",
                        
                            # Tab2 layout type
                            verticalLayout(
                        
                                # Tab2 text content 1
                                p(strong("Instructions on how to use the app:")),
                                
                                # Tab2 text content 2
                                p("       1. Enter a word or phrase into the text box in the top right corner."),
                                
                                # Tab2 text content 3
                                p("       2. Click the 'Predict Next Word' button."),
                                
                                # Tab2 text content 4
                                p("       3. See the top prediction results in the main display window."),
                                
                                # Tab2 text content 5
                                p("The algorithm for the app was built using the Modified Kneser-Ney Smoothing algorithm developed by", a(href="http://joneschen.org/spigdog/papers/h015a-techreport.pdf", "Chen and Goodman"), ".", "The code for this app is available on", a(href="https://github.com/", "Github"), ".", "More detailed documentation on building the KN algorithm in R is available", a(href="https://rpubs.com/", "here"), ".")

                            )
               )
    )
))

# Define server logic required to render the text prediction output
server <- shinyServer(function(input, output) {
    
    output$caption <- renderText({"These are the most likely words to follow your phrase."})
    
    output$prediction <- renderTable({next_word(as.character(input$phrase))})
    
    
})

# Run the application 
shinyApp(ui = ui, server = server)
