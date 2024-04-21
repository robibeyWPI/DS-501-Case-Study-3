library(shiny)
library(ggplot2)
library(corrplot)
library(caret)
library(rsconnect)

# Read in csv
lol_frame = read.csv("high_diamond_ranked_10min.csv")
blue = lol_frame[, grep("blue", colnames(lol_frame))]
blue = subset(blue, select = -c(blueTotalGold, blueTotalMinionsKilled, blueHeralds))

#cdplot(factor(blueWins) ~ blueKills, data = blue)
#corrplot(cor(blue), method = "number")


ui = fluidPage(
  titlePanel("Predictors of Winrate in League of Legends"),
  h4("First 10 minutes of diamond ranked matches"),
  
  sidebarLayout(
    sidebarPanel(selectInput("variables", "Choose Variables", choices = colnames(subset(blue, select = -c(blueWins))),
                             multiple = TRUE, selected = c("blueKills", "blueDeaths")),
                 selectInput("displayProb", "Choose Variable", choices = c("blueKills", "blueDeaths")),
                 p("In this case study I am using a logistic regression algorithm to analyze what factors
                 can be used to predict a win in a League of Legends ranked match. The dataset I used provides
                 a depdendent variable (blueWins) as well as a bunch of other variables for the first 10 minutes
                 of ~10,000 diamond 1 - master ranked matches The motivation behind this
                 topic is that I am a gamer and I am interested to see what variables are the most imporant in
                 predicting a win within the first 10 minutes of the game. Since games always last more than 10 minutes,
                 with some lasting 45 minutes or more, I do not expect this algorithm to be perfect. However,
                 since diamond is a high rank, players should be able to more consistently capitalize on advantages
                 that occur within the first 10 minutes of the match to close out games before the lead is lost."
                   ),
                 p("The dataset provides binary values on whether blue side won (1) or lost (0) the match. The dataset
                   also provides values for blue and red side but I chose to focus only on blue side variables since I
                   would expect them to be mirrored for red side. For example, if the blueGoldDiff is 1000, the redGoldDiff
                   would be -1000. However, it would be interesting to see if including red side variables lead to a more
                   accurate algorithm in the future."
                 ),
                 p("This case study is done using the glm function with a binomial family since the dependent variable is binary
                   (either a win or a loss). The seed is set to an arbitrary value to produce consistent results and the data is
                   split into 70% training data, 30% testing data. I created 2 glm models: one is using all of the variables selected,
                   while the other uses only the individual variable selected to see how each variable alone impacts the results.
                   Since this is a logistic regression, it is important to round predicted values up or down to result in a binary
                   classification. I pondered for a while on what visualizations would make the most sense to represent the results
                   of the regression and I eventually decide on an accuracy score and confusion matrices. The accuracy score
                   represents a simple way of assessing how accurate the model classified the test data while the confusion
                   matrices provide a more visually appealing and in depth way of looking at true positives, true negatives, false
                   positives, and false negatives."),
                 p("Everything is reactive, so feel free to change things around and the models will automatically rerun."
                   ),
                 tags$a(href="https://www.kaggle.com/datasets/bobbyscience/league-of-legends-diamond-ranked-games-10-min", "Link to dataset")),
    mainPanel(
      fluidRow(
        column(
          width = 6,
          wellPanel(
            tableOutput("txt")
          )
        )
      ),
      fluidRow(
        column(
          width = 6,
          wellPanel(
            plotOutput("all_plot")
          )
        )
      ),
      fluidRow(
        column(
          width = 6,
          wellPanel(
            plotOutput("ind_plot")
          )
        )
      )
    )
  
))

server = function(input, output, session) {
  split_data = reactive({
    set.seed(100)
    splitBlueTeam = caret::createDataPartition(blue$blueWins, p = 0.7, list = F, times = 1)
    trainBlueTeam = blue[splitBlueTeam,]
    testBlueTeam = blue[-splitBlueTeam,]
    return(list(train = trainBlueTeam, test = testBlueTeam))
  })
  
  pred_vars = reactive({
    test_data = split_data()$test
    pred_df = data.frame(Actual = test_data$blueWins, Predicted = predict(model(), newdata = test_data, type = "response"),
                         IndVar = predict(ind_model(), newdata = test_data, type = "response"))
    pred_classes = ifelse(pred_df$Predicted > 0.5, 1, 0)
    pred_class_ind = ifelse(pred_df$IndVar > 0.5, 1, 0)
    pred_df$IndVar = pred_class_ind
    return(list(pred_df = pred_df, pred_classes = pred_classes, pred_class_ind = pred_class_ind))
  })

  model = reactive({
    glm(blueWins ~ ., data = split_data()$train[, c("blueWins", input$variables)], family = binomial(link = "logit"))
    })

  ind_model = reactive({
    glm(blueWins ~ ., data = split_data()$train[, c("blueWins", input$displayProb)], family = binomial(link = "logit"))
  })
  
  all_confusion_matrix = reactive({
    cm = confusionMatrix(as.factor(pred_vars()$pred_df$Actual), as.factor(pred_vars()$pred_classes))
    return(cm)
  })
  
  single_confusion_matrix = reactive({
    cm = confusionMatrix(as.factor(pred_vars()$pred_df$Actual), as.factor(pred_vars()$pred_class_ind))
    return(cm)
  })


  output$all_plot = renderPlot({
    confusion_df = as.data.frame(all_confusion_matrix()$table)
    
    ggplot(data = confusion_df, aes(x = Reference, y = Prediction, fill = Freq)) +
      geom_tile() +
      geom_text(aes(label = Freq), vjust = 1, size = 5) +
      scale_fill_gradient(low = "snow", high = "darkgreen") +
      labs(x = "Actual", y = "Predicted", title = "All Parameters Confusion Matrix", subtitle = "0 = loss, 1 = win") +
      theme_minimal()
      
  })
  
  output$ind_plot = renderPlot({
    confusion_df = as.data.frame(single_confusion_matrix()$table)
    
    ggplot(data = confusion_df, aes(x = Reference, y = Prediction, fill = Freq)) +
      geom_tile() +
      geom_text(aes(label = Freq), vjust = 1, size = 5) +
      scale_fill_gradient(low = "snow", high = "darkgreen") +
      labs(x = "Actual", y = "Predicted", title = paste(input$displayProb, "Confusion Matrix"), subtitle = "0 = loss, 1 = win") +
      theme_minimal()
  })
  
  acc_scores = reactive({
    accuracy = data.frame(Overall.Accuracy = all_confusion_matrix()$overall['Accuracy'], Individual.Accuracy = single_confusion_matrix()$overall['Accuracy'])
  })
  output$txt = renderTable({
    accs = acc_scores()
    colnames(accs) = c("Overall Accuracy", paste(input$displayProb, "Accuracy"))
    accs
  })
  
  observe({
    updateSelectInput(session, "displayProb", choices = input$variables, selected = input$displayProb)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

