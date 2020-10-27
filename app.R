#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(plotly)
library(scales)
load("PL_Web.RData") #data 

# functions to find the nearest weight value, in kg, of listed weight attempts compared to input
test_squat <- function(x) {
    squat_sampsize$diff <- squat_sampsize$Squat3Kg - x
    y <- which.min(abs(squat_sampsize$diff))
    return(squat_sampsize$Squat3Kg[y])
}

test_bench <- function(x) {
    bench_sampsize$diff <- bench_sampsize$Bench3Kg - x
    y <- which.min(abs(bench_sampsize$diff))
    return(bench_sampsize$Bench3Kg[y])
}

test_deadlift <- function(x) {
    deadlift_sampsize$diff <- deadlift_sampsize$Deadlift3Kg - x
    y <- which.min(abs(deadlift_sampsize$diff))
    return(deadlift_sampsize$Deadlift3Kg[y])
}

ui <- fluidPage(
    titlePanel("Powerlifting Lift Attempt Calculator"),

    # sidebar with inputs for age, sex, equipment, weight, lift weights, display/enter in lbs/kg 
    sidebarLayout(
        sidebarPanel(
            fluidRow(
                column(6,radioButtons("lbskg",
                         h4("Entering data in kg or lbs?"),
                         choices = list("kg" = 1, "lbs" = 0)
                         )),
                column(6,radioButtons("lbskg2",
                                      h4("View data in kg or lbs?"),
                                      choices = list("kg" = 1, "lbs" = 0))
                )),
            numericInput("age",
                         h4("Enter your age:"),
                         value = sample(20:40,1)),
            radioButtons("sex",
                         h4("Sex:"),
                         choices = list("Male" = "M", "Female" ="F")
            ),
            numericInput("weight",
                         h4("Enter your weight:"),
                         value = sample(50:110,1)),
            selectInput("equipment",
                         h4("What equipment will you use?"),
                         choices = list("Single-ply" = "Single-ply", 
                                        "Multi-ply" = "Multi-ply", 
                                        "Wraps" = "Wraps", 
                                        "Raw" = "Raw"), selected = "Raw"),
            numericInput("squat",
                         h4("Enter your squat weight:"),
                         value = 140),
            numericInput("bench",
                         h4("Enter your bench weight:"),
                         value = 100),
            numericInput("deadlift",
                         h4("Enter your deadlift weight:"),
                         value = 185)
        ),

        
        mainPanel(
            # tabs for predictive model output, about, squat/bench/deadlift graphs
            tabsetPanel(
                tabPanel("Prediction", 
                         uiOutput("squat_predictions"),
                         hr(),
                         uiOutput("bench_predictions"),
                         hr(),
                         uiOutput("deadlift_predictions"),
                         br(),
                         br(),
                         h6(em(textOutput("example_lifter")))
                         ),
                tabPanel("About",
                         h3("About this calculator"),
                         br(),
                         br(),
                         h4("This calculator was originally made with the intent to create the most accurate lift attempt calculator for powerlifters who do not have experience with determining their openers or weight jumps. This calculator takes into account your age, body weight, sex, equipment, and expected max lift attempts. Statistically, all of these are important in determining your ideal attempted weight for each lift. Realistically, these data in general do not impact your overall lift attempts. The data was fit using a simple linear regression since powerlifting data is incredibly linear in nature."),
                         br(),
                         br(),
                         h4("A few tabs are included that show historical attempts in powerlifting meets for your target max weight. The y-axis shows the percentage of people that had a successful third lift at specific weights for attempt 1 or 2. You can think of this as supplemental information to see the most common attempts historically or to help you strategize your starting weights or second attempt weights. These graphs are iteratively generated, so some may not be that pretty. In the future, I plan to further add graphs to help choose an opener and see the highest success rates per lift."),
                         br(),
                         br(),
                         h4("The last piece of information currently present in this calculator is an homage to some of the greatest lifters of all time. I've taken a list of the top 1% of lifters among different weights, sexes, and equipment categories. When you enter your body weight on the main page, a small blurb will appear about one of these lifters with a similar weight to you. I have also included if their meet was drug-tested or not; surprisingly, 71% of these performances were at drug-tested meets."),
                         br(),
                         br(),
                         h5("This page uses data from the OpenPowerlifting project, https://www.openpowerlifting.org.
                            You may download a copy of the data at https://gitlab.com/openpowerlifting/opl-data.")
                         
                         ),
                tabPanel("Squat graph",
                         uiOutput("squatone"),
                         br(),
                         plotlyOutput("squat1Plot"),
                         hr(),
                         uiOutput("squattwo"),
                         br(),
                         plotlyOutput("squat2Plot")),
                tabPanel("Bench graph",
                         uiOutput("benchone"),
                         br(),
                         plotlyOutput("bench1Plot"),
                         hr(),
                         uiOutput("benchtwo"),
                         br(),
                         plotlyOutput("bench2Plot")),
                tabPanel("Deadlift graph", 
                         uiOutput("deadliftone"),
                         br(),
                         plotlyOutput("deadlift1Plot"),
                         hr(),
                         uiOutput("deadlifttwo"),
                         br(),
                         plotlyOutput("deadlift2Plot"))
            )
        )
    )
)

server <- function(input, output) {
    # generate file for outputting squat lift predictions
    output$squat_predictions <- renderUI({
        # input validation
        if(is.na(input$age)) {
            HTML(paste0(h3("Missing age! Please enter age.")))
        } else if (is.na(input$weight)) {
            HTML(paste0(h3("Missing body weight! Please enter weight.")))
        } else if (input$age < 0) {
            HTML(paste0(h3("Please enter a non-negative age.")))
        } else if (input$weight < 0) {
            HTML(paste0(h3("Please enter a non-negative weight.")))
        } else if (is.na(input$squat)) {
            HTML(paste0(h3("Missing squat! Please enter a squat weight.")))
        } else if (is.na(input$bench)) {
            HTML(paste0(h3("Missing bench press! Please enter a bench press weight.")))
        } else if (is.na(input$deadlift)) {
            HTML(paste0(h3("Missing deadlift! Please enter a deadlift weight.")))
        } else if (input$squat < 0) {
            HTML(paste0(h3("Please enter a non-negative squat weight.")))
        } else if (input$bench < 0) {
            HTML(paste0(h3("Please enter a non-negative bench press weight.")))
        } else if (input$deadlift < 0) {
            HTML(paste0(h3("Please enter a non-negative deadlift weight.")))
        }
        else {
            #create a dataframe called user_info with all the users' input
        user_info <- setNames(data.frame(input$age, factor(input$sex), ifelse(input$lbskg == 1, input$weight, input$weight*0.453592), 
                                         factor(input$equipment), ifelse(input$lbskg == 1, input$squat, input$squat*0.453592), 
                                         ifelse(input$lbskg == 1, input$bench, input$bench*0.453592), 
                                         ifelse(input$lbskg == 1, input$deadlift, input$deadlift*0.453592)),
                              c("Age", "Sex", "BodyweightKg", "Equipment", "Squat3Kg", "Bench3Kg", "Deadlift3Kg"))
        #variables for squat predictions, lift 1 and 2, in kg
        squat_1kg_pred <- round(predict(Squat1Kg_model, user_info)*2)/2
        squat_2kg_pred <- round(predict(Squat2Kg_model, user_info)*2)/2
        if (input$lbskg2 == 0){
            #in lbs
            squat_1lb_pred <- round(squat_1kg_pred/0.453592)
            squat_2lb_pred <- round(squat_2kg_pred/0.453592)
            #paste result, lbs
            HTML(paste0(h3("Your Squat lift attempts are:"), br(), h4("Lift 1: ", squat_1lb_pred, "lbs"), br(), h4("Lift 2: ", squat_2lb_pred, "lbs")))
        } else {
            #paste result, kg
        HTML(paste0(h3("Your Squat lift attempts are:"), br(), h4("Lift 1: ", squat_1kg_pred, "kg"), br(), h4("Lift 2: ", squat_2kg_pred, "kg"))
        )
        }
        }
    })
    # repeat above; only have squat 1 prediction included
    output$squatone <- renderUI({
        if(is.na(input$age)) {
            paste0("")
        } else if (is.na(input$weight)) {
            paste0("")
        } else if (input$age < 0) {
            paste0("")
        } else if (input$weight < 0) {
            paste0("")
        } else if (is.na(input$squat)) {
            paste0("")
        } else if (is.na(input$bench)) {
            paste0("")
        } else if (is.na(input$deadlift)) {
            paste0("")
        } else if (input$squat < 0) {
            paste0("")
        } else if (input$bench < 0) {
            paste0("")
        } else if (input$deadlift < 0) {
            paste0("")
        } else {
            user_info <- setNames(data.frame(input$age, factor(input$sex), ifelse(input$lbskg == 1, input$weight, input$weight*0.453592), 
                                             factor(input$equipment), ifelse(input$lbskg == 1, input$squat, input$squat*0.453592), 
                                             ifelse(input$lbskg == 1, input$bench, input$bench*0.453592), 
                                             ifelse(input$lbskg == 1, input$deadlift, input$deadlift*0.453592)),
                                  c("Age", "Sex", "BodyweightKg", "Equipment", "Squat3Kg", "Bench3Kg", "Deadlift3Kg"))
            squat_1kg_pred <- round(predict(Squat1Kg_model, user_info)*2)/2
            if (input$lbskg2 == 0){
                squat_1lb_pred <- round(squat_1kg_pred/0.453592)
                HTML(paste0(h3("Your Squat 1 attempt:", squat_1lb_pred, "lbs")))
            } else {
            HTML(paste0(h3("Your Squat 1 attempt:", squat_1kg_pred, "kg")))
            }
        }
    })
    
    #repeat above, only squat 2 prediction
    output$squattwo <- renderUI({
        if(is.na(input$age)) {
            paste0("")
        } else if (is.na(input$weight)) {
            paste0("")
        } else if (input$age < 0) {
            paste0("")
        } else if (input$weight < 0) {
            paste0("")
        } else if (is.na(input$squat)) {
            paste0("")
        } else if (is.na(input$bench)) {
            paste0("")
        } else if (is.na(input$deadlift)) {
            paste0("")
        } else if (input$squat < 0) {
            paste0("")
        } else if (input$bench < 0) {
            paste0("")
        } else if (input$deadlift < 0) {
            paste0("")
        } else {
            user_info <- setNames(data.frame(input$age, factor(input$sex), ifelse(input$lbskg == 1, input$weight, input$weight*0.453592), 
                                             factor(input$equipment), ifelse(input$lbskg == 1, input$squat, input$squat*0.453592), 
                                             ifelse(input$lbskg == 1, input$bench, input$bench*0.453592), 
                                             ifelse(input$lbskg == 1, input$deadlift, input$deadlift*0.453592)),
                                  c("Age", "Sex", "BodyweightKg", "Equipment", "Squat3Kg", "Bench3Kg", "Deadlift3Kg"))
            squat_2kg_pred <- round(predict(Squat2Kg_model, user_info)*2)/2
            if (input$lbskg2 == 0){
                squat_2lb_pred <- round(squat_2kg_pred/0.453592)
                HTML(paste0(h3("Your Squat 2 attempt:", squat_2lb_pred, "lbs")))
            } else {
            HTML(paste0(h3("Your Squat 2 attempt:", squat_2kg_pred, "kg")))
            }
        }
    })
    
    #bench 1 prediction
    output$benchone <- renderUI({
        if(is.na(input$age)) {
            paste0("")
        } else if (is.na(input$weight)) {
            paste0("")
        } else if (input$age < 0) {
            paste0("")
        } else if (input$weight < 0) {
            paste0("")
        } else if (is.na(input$squat)) {
            paste0("")
        } else if (is.na(input$bench)) {
            paste0("")
        } else if (is.na(input$deadlift)) {
            paste0("")
        } else if (input$squat < 0) {
            paste0("")
        } else if (input$bench < 0) {
            paste0("")
        } else if (input$deadlift < 0) {
            paste0("")
        } else {
            user_info <- setNames(data.frame(input$age, factor(input$sex), ifelse(input$lbskg == 1, input$weight, input$weight*0.453592), 
                                             factor(input$equipment), ifelse(input$lbskg == 1, input$squat, input$squat*0.453592), 
                                             ifelse(input$lbskg == 1, input$bench, input$bench*0.453592), 
                                             ifelse(input$lbskg == 1, input$deadlift, input$deadlift*0.453592)),
                                  c("Age", "Sex", "BodyweightKg", "Equipment", "Squat3Kg", "Bench3Kg", "Deadlift3Kg"))
            bench_1kg_pred <- round(predict(Bench1Kg_model, user_info)*2)/2
            if (input$lbskg2 == 0){
                bench_1lb_pred <- round(bench_1kg_pred/0.453592)
                HTML(paste0(h3("Your Bench 1 attempt:", bench_1lb_pred, "lbs")))
            } else {
            HTML(paste0(h3("Your Bench 1 attempt:", bench_1kg_pred, "kg")))
            }
        }
    })
    
    #bench 2 prediction
    output$benchtwo <- renderUI({
        if(is.na(input$age)) {
            paste0("")
        } else if (is.na(input$weight)) {
            paste0("")
        } else if (input$age < 0) {
            paste0("")
        } else if (input$weight < 0) {
            paste0("")
        } else if (is.na(input$squat)) {
            paste0("")
        } else if (is.na(input$bench)) {
            paste0("")
        } else if (is.na(input$deadlift)) {
            paste0("")
        } else if (input$squat < 0) {
            paste0("")
        } else if (input$bench < 0) {
            paste0("")
        } else if (input$deadlift < 0) {
            paste0("")
        } else {
            user_info <- setNames(data.frame(input$age, factor(input$sex), ifelse(input$lbskg == 1, input$weight, input$weight*0.453592), 
                                             factor(input$equipment), ifelse(input$lbskg == 1, input$squat, input$squat*0.453592), 
                                             ifelse(input$lbskg == 1, input$bench, input$bench*0.453592), 
                                             ifelse(input$lbskg == 1, input$deadlift, input$deadlift*0.453592)),
                                  c("Age", "Sex", "BodyweightKg", "Equipment", "Squat3Kg", "Bench3Kg", "Deadlift3Kg"))
            bench_2kg_pred <- round(predict(Bench2Kg_model, user_info)*2)/2
            if (input$lbskg2 == 0){
                bench_2lb_pred <- round(bench_2kg_pred/0.453592)
                HTML(paste0(h3("Your Bench 2 attempt:", bench_2lb_pred, "lbs")))
            } else {
            HTML(paste0(h3("Your Bench 2 attempt:", bench_2kg_pred, "kg")))
            }
        }
    })
    
    #deadlift 1 prediction
    output$deadliftone <- renderUI({
        if(is.na(input$age)) {
            paste0("")
        } else if (is.na(input$weight)) {
            paste0("")
        } else if (input$age < 0) {
            paste0("")
        } else if (input$weight < 0) {
            paste0("")
        } else if (is.na(input$squat)) {
            paste0("")
        } else if (is.na(input$bench)) {
            paste0("")
        } else if (is.na(input$deadlift)) {
            paste0("")
        } else if (input$squat < 0) {
            paste0("")
        } else if (input$bench < 0) {
            paste0("")
        } else if (input$deadlift < 0) {
            paste0("")
        } else {
            user_info <- setNames(data.frame(input$age, factor(input$sex), ifelse(input$lbskg == 1, input$weight, input$weight*0.453592), 
                                             factor(input$equipment), ifelse(input$lbskg == 1, input$squat, input$squat*0.453592), 
                                             ifelse(input$lbskg == 1, input$bench, input$bench*0.453592), 
                                             ifelse(input$lbskg == 1, input$deadlift, input$deadlift*0.453592)),
                                  c("Age", "Sex", "BodyweightKg", "Equipment", "Squat3Kg", "Bench3Kg", "Deadlift3Kg"))
            deadlift_1kg_pred <- round(predict(Deadlift1Kg_model, user_info)*2)/2
            if (input$lbskg2 == 0){
                deadlift_1lb_pred <- round(deadlift_1kg_pred/0.453592)
                HTML(paste0(h3("Your Deadlift 1 attempt:", deadlift_1lb_pred, "lbs")))
            } else {
            HTML(paste0(h3("Your Deadlift 1 attempt:", deadlift_1kg_pred, "kg")))
            }
        }
    })
    
    #deadlift 2 prediction
    output$deadlifttwo <- renderUI({
        if(is.na(input$age)) {
            paste0("")
        } else if (is.na(input$weight)) {
            paste0("")
        } else if (input$age < 0) {
            paste0("")
        } else if (input$weight < 0) {
            paste0("")
        } else if (is.na(input$squat)) {
            paste0("")
        } else if (is.na(input$bench)) {
            paste0("")
        } else if (is.na(input$deadlift)) {
            paste0("")
        } else if (input$squat < 0) {
            paste0("")
        } else if (input$bench < 0) {
            paste0("")
        } else if (input$deadlift < 0) {
            paste0("")
        } else {
            user_info <- setNames(data.frame(input$age, factor(input$sex), ifelse(input$lbskg == 1, input$weight, input$weight*0.453592), 
                                             factor(input$equipment), ifelse(input$lbskg == 1, input$squat, input$squat*0.453592), 
                                             ifelse(input$lbskg == 1, input$bench, input$bench*0.453592), 
                                             ifelse(input$lbskg == 1, input$deadlift, input$deadlift*0.453592)),
                                  c("Age", "Sex", "BodyweightKg", "Equipment", "Squat3Kg", "Bench3Kg", "Deadlift3Kg"))
            deadlift_2kg_pred <- round(predict(Deadlift2Kg_model, user_info)*2)/2
            if (input$lbskg2 == 0){
                deadlift_2lb_pred <- round(deadlift_2kg_pred/0.453592)
                HTML(paste0(h3("Your Deadlift 2 attempt:", deadlift_2lb_pred, "lbs")))
            } else {
            HTML(paste0(h3("Your Deadlift 2 attempt:", deadlift_2kg_pred, "kg")))
            }
        }
    })
    
    #list of bench predictions for page 1
    output$bench_predictions <- renderUI({
        if(is.na(input$age)) {
            paste0("")
        } else if (is.na(input$weight)) {
            paste0("")
        } else if (input$age < 0) {
            paste0("")
        } else if (input$weight < 0) {
            paste0("")
        } else if (is.na(input$squat)) {
            paste0("")
        } else if (is.na(input$bench)) {
            paste0("")
        } else if (is.na(input$deadlift)) {
            paste0("")
        } else if (input$squat < 0) {
            paste0("")
        } else if (input$bench < 0) {
            paste0("")
        } else if (input$deadlift < 0) {
            paste0("")
        } else {
            user_info <- setNames(data.frame(input$age, factor(input$sex), ifelse(input$lbskg == 1, input$weight, input$weight*0.453592), 
                                             factor(input$equipment), ifelse(input$lbskg == 1, input$squat, input$squat*0.453592), 
                                             ifelse(input$lbskg == 1, input$bench, input$bench*0.453592), 
                                             ifelse(input$lbskg == 1, input$deadlift, input$deadlift*0.453592)),
                                  c("Age", "Sex", "BodyweightKg", "Equipment", "Squat3Kg", "Bench3Kg", "Deadlift3Kg"))
            bench_1kg_pred <- round(predict(Bench1Kg_model, user_info)*2)/2
            bench_2kg_pred <- round(predict(Bench2Kg_model, user_info)*2)/2
            if (input$lbskg2 == 0){
                bench_1lb_pred <- round(bench_1kg_pred/0.453592)
                bench_2lb_pred <- round(bench_2kg_pred/0.453592)
                HTML(paste0(h3("Your Bench Press lift attempts are:"), br(), h4("Lift 1: ", bench_1lb_pred, "lbs"), br(), h4("Lift 2: ", bench_2lb_pred, "lbs")))
            } else {
                HTML(paste0(h3("Your Bench Press lift attempts are:"), br(), h4("Lift 1: ", bench_1kg_pred, "kg"), br(), h4("Lift 2: ", bench_2kg_pred, "kg")))
            }
        }
    })
    
    #list of deadlift predictions for page 1
    output$deadlift_predictions <- renderUI({
        if(is.na(input$age)) {
            paste0("")
        } else if (is.na(input$weight)) {
            paste0("")
        } else if (input$age < 0) {
            paste0("")
        } else if (input$weight < 0) {
            paste0("")
        } else if (is.na(input$squat)) {
            paste0("")
        } else if (is.na(input$bench)) {
            paste0("")
        } else if (is.na(input$deadlift)) {
            paste0("")
        } else if (input$squat < 0) {
            paste0("")
        } else if (input$bench < 0) {
            paste0("")
        } else if (input$deadlift < 0) {
            paste0("")
        } else {
            user_info <- setNames(data.frame(input$age, factor(input$sex), ifelse(input$lbskg == 1, input$weight, input$weight*0.453592), 
                                             factor(input$equipment), ifelse(input$lbskg == 1, input$squat, input$squat*0.453592), 
                                             ifelse(input$lbskg == 1, input$bench, input$bench*0.453592), 
                                             ifelse(input$lbskg == 1, input$deadlift, input$deadlift*0.453592)),
                                  c("Age", "Sex", "BodyweightKg", "Equipment", "Squat3Kg", "Bench3Kg", "Deadlift3Kg"))
            deadlift_1kg_pred <- round(predict(Deadlift1Kg_model, user_info)*2)/2
            deadlift_2kg_pred <- round(predict(Deadlift2Kg_model, user_info)*2)/2
            if (input$lbskg2 == 0){
                deadlift_1lb_pred <- round(deadlift_1kg_pred/0.453592)
                deadlift_2lb_pred <- round(deadlift_2kg_pred/0.453592)
                HTML(paste0(h3("Your Deadlift lift attempts are:"), br(), h4("Lift 1: ", deadlift_1lb_pred, "lbs"), br(), h4("Lift 2: ", deadlift_2lb_pred, "lbs")))
            } else {
                HTML(paste0(h3("Your Deadlift lift attempts are:"), br(), h4("Lift 1: ", deadlift_1kg_pred, "kg"), br(), h4("Lift 2: ", deadlift_2kg_pred, "kg")))
            }
        }
    })
    
    #rendering an interactive plot
    output$squat1Plot <- renderPlotly({
        #input validation
        if (is.na(input$squat)|input$squat<0) {} else {
            #test for units for input and viewing
            if (input$lbskg == 0 & input$lbskg2 == 0) { 
                #use function to find nearest squat value
                squat_val <- test_squat(input$squat*0.453592) #multiply due to input in lbs
                #filter for squats that are of the same squat value, equipment, sex; group and find ratio of success compared to squat 3 (exclude 100% success)
                squat_one <- pl_web_clean %>% filter(Squat3Kg == squat_val, Equipment == input$equipment, Sex == input$sex) %>% group_by(Squat1Kg) %>% 
                    summarise(ratio = sum(Squat3SF/n()), Sample_Size = n()) %>% filter(ratio != 1) %>% mutate(Squat1Lb = Squat1Kg/0.453592)
                #create column plot showing weight vs ratio of success on lift 3, includes sample size per weight
                g <- ggplot(squat_one, aes(Squat1Lb, ratio, fill = Sample_Size)) + geom_col(aes(text = paste0(round(ratio*100), "% success on lift 3"))) + 
                    scale_fill_viridis_c() + xlim(round(squat_val*0.8/0.453592),squat_val/0.453592-1) + theme_pubr(legend="right") + 
                    ylab("Ratio of success on Squat 3") + xlab("Squat 1 Weight (in lbs)") + labs(fill = "# of lifts") + scale_y_continuous(labels = percent)
                ggplotly(g, tooltip = c("Sample_Size", "text", "Squat1Lb")) #convert it to plotly for interactivity
            } else if (input$lbskg == 0 & input$lbskg2 == 1) { 
                #test units for input, viewing; same as above but with different units
                squat_val <- test_squat(input$squat*0.453592)
                squat_one <- pl_web_clean %>% filter(Squat3Kg == squat_val, Equipment == input$equipment, Sex == input$sex) %>% group_by(Squat1Kg) %>% 
                    summarise(ratio = sum(Squat3SF/n()), Sample_Size = n()) %>% filter(ratio != 1) 
                g <- ggplot(squat_one, aes(Squat1Kg, ratio, fill = Sample_Size)) + geom_col(aes(text = paste0(round(ratio*100), "% success on lift 3"))) +
                    scale_fill_viridis_c() + xlim(round(squat_val*0.8),squat_val-1) + theme_pubr(legend="right") + ylab("Ratio of success on Squat 3") + 
                    xlab("Squat 1 Weight (in kg)") + labs(fill = "# of lifts") + scale_y_continuous(labels = percent)
                ggplotly(g, tooltip = c("Sample_Size", "text", "Squat1Kg"))
            } else if (input$lbskg == 1 & input$lbskg2 == 0) { #same as above
                squat_val <- test_squat(input$squat)
                squat_one <- pl_web_clean %>% filter(Squat3Kg == squat_val, Equipment == input$equipment, Sex == input$sex) %>% group_by(Squat1Kg) %>% 
                    summarise(ratio = sum(Squat3SF/n()), Sample_Size = n()) %>% filter(ratio != 1) %>% mutate(Squat1Lb = Squat1Kg/0.453592)
                g <- ggplot(squat_one, aes(Squat1Lb, ratio, fill = Sample_Size)) + geom_col(aes(text = paste0(round(ratio*100), "% success on lift 3"))) + 
                    scale_fill_viridis_c() + xlim(round(squat_val*0.8/0.453592),squat_val/0.453592-1) + theme_pubr(legend="right") + 
                    ylab("Ratio of success on Squat 3") + xlab("Squat 1 Weight (in lbs)") + labs(fill = "# of lifts") + scale_y_continuous(labels = percent)
                ggplotly(g, tooltip = c("Sample_Size", "text", "Squat1Lb"))
            } else if (input$lbskg == 1 & input$lbskg2 == 1) { #same as above
                squat_val <- test_squat(input$squat)
                squat_one <- pl_web_clean %>% filter(Squat3Kg == squat_val, Equipment == input$equipment, Sex == input$sex) %>% group_by(Squat1Kg) %>% 
                    summarise(ratio = sum(Squat3SF/n()), Sample_Size = n()) %>% filter(ratio != 1) 
                g <- ggplot(squat_one, aes(Squat1Kg, ratio, fill = Sample_Size)) + geom_col(aes(text = paste0(round(ratio*100), "% success on lift 3"))) + 
                    scale_fill_viridis_c() + xlim(round(squat_val*0.8),squat_val-1) + theme_pubr(legend="right") + ylab("Ratio of success on Squat 3") + 
                    xlab("Squat 1 Weight (in kg)") + labs(fill = "# of lifts") + scale_y_continuous(labels = percent)
                ggplotly(g, tooltip = c("Sample_Size", "text", "Squat1Kg"))
            }
        }
        })
    
    #same as squat1Plot
    output$squat2Plot <- renderPlotly({
        if (is.na(input$squat)|input$squat<0) {} else {
            if (input$lbskg == 0 & input$lbskg2 == 0) {
                squat_val <- test_squat(input$squat*0.453592)
                squat_two <- pl_web_clean %>% filter(Squat3Kg == squat_val, Equipment == input$equipment, Sex == input$sex) %>% group_by(Squat2Kg) %>% 
                    summarise(ratio = sum(Squat3SF/n()), Sample_Size = n()) %>% filter(ratio != 1) %>% mutate(Squat2Lb = Squat2Kg/0.453592)
                g <- ggplot(squat_two, aes(Squat2Lb, ratio, fill = Sample_Size)) + geom_col(aes(text = paste0(round(ratio*100), "% success on lift 3"))) + 
                    scale_fill_viridis_c() + xlim(round(squat_val*0.8/0.453592),squat_val/0.453592-1) + theme_pubr(legend="right") + 
                    ylab("Ratio of success on Squat 3") + xlab("Squat 2 Weight (in lbs)") + labs(fill = "# of lifts") + scale_y_continuous(labels = percent)
                ggplotly(g, tooltip = c("Sample_Size", "text", "Squat2Lb"))
            } else if (input$lbskg == 0 & input$lbskg2 == 1) {
                squat_val <- test_squat(input$squat*0.453592)
                squat_two <- pl_web_clean %>% filter(Squat3Kg == squat_val, Equipment == input$equipment, Sex == input$sex) %>% group_by(Squat2Kg) %>% 
                    summarise(ratio = sum(Squat3SF/n()), Sample_Size = n()) %>% filter(ratio != 1) 
                g <- ggplot(squat_two, aes(Squat2Kg, ratio, fill = Sample_Size)) + geom_col(aes(text = paste0(round(ratio*100), "% success on lift 3"))) + 
                    scale_fill_viridis_c() + xlim(round(squat_val*0.8),squat_val-1) + theme_pubr(legend="right") + ylab("Ratio of success on Squat 3") + 
                    xlab("Squat 2 Weight (in kg)") + labs(fill = "# of lifts") + scale_y_continuous(labels = percent)
                ggplotly(g, tooltip = c("Sample_Size", "text", "Squat2Kg"))
            } else if (input$lbskg == 1 & input$lbskg2 == 0) {
                squat_val <- test_squat(input$squat)
                squat_two <- pl_web_clean %>% filter(Squat3Kg == squat_val, Equipment == input$equipment, Sex == input$sex) %>% group_by(Squat2Kg) %>% 
                    summarise(ratio = sum(Squat3SF/n()), Sample_Size = n()) %>% filter(ratio != 1) %>% mutate(Squat2Lb = Squat2Kg/0.453592)
                g <- ggplot(squat_two, aes(Squat2Lb, ratio, fill = Sample_Size)) + geom_col(aes(text = paste0(round(ratio*100), "% success on lift 3"))) + 
                    scale_fill_viridis_c() + xlim(round(squat_val*0.8/0.453592),squat_val/0.453592-1) + theme_pubr(legend="right") + 
                    ylab("Ratio of success on Squat 3") + xlab("Squat 2 Weight (in lbs)") + labs(fill = "# of lifts") + scale_y_continuous(labels = percent)
                ggplotly(g, tooltip = c("Sample_Size", "text", "Squat2Lb"))
            } else if (input$lbskg == 1 & input$lbskg2 == 1) {
                squat_val <- test_squat(input$squat)
                squat_two <- pl_web_clean %>% filter(Squat3Kg == squat_val, Equipment == input$equipment, Sex == input$sex) %>% group_by(Squat2Kg) %>% 
                    summarise(ratio = sum(Squat3SF/n()), Sample_Size = n()) %>% filter(ratio != 1) 
                g <- ggplot(squat_two, aes(Squat2Kg, ratio, fill = Sample_Size)) + geom_col(aes(text = paste0(round(ratio*100), "% success on lift 3"))) + 
                    scale_fill_viridis_c() + xlim(round(squat_val*0.8),squat_val-1) + theme_pubr(legend="right") + ylab("Ratio of success on Squat 3") + 
                    xlab("Squat 2 Weight (in kg)") + labs(fill = "# of lifts") + scale_y_continuous(labels = percent)
                ggplotly(g, tooltip = c("Sample_Size", "text", "Squat2Kg"))
            }
        }
    })
    
    #same as above
    output$bench1Plot <- renderPlotly({
        if (is.na(input$bench)|input$bench<0) {} else {
            if (input$lbskg == 0 & input$lbskg2 == 0) {
                bench_val <- test_bench(input$bench*0.453592)
                bench_one <- pl_web_clean %>% filter(Bench3Kg == bench_val, Equipment == input$equipment, Sex == input$sex) %>% group_by(Bench1Kg) %>% 
                    summarise(ratio = sum(Bench3SF/n()), Sample_Size = n()) %>% filter(ratio != 1) %>% mutate(Bench1Lb = Bench1Kg/0.453592)
                g <- ggplot(bench_one, aes(Bench1Lb, ratio, fill = Sample_Size)) + geom_col(aes(text = paste0(round(ratio*100), "% success on lift 3"))) + 
                    scale_fill_viridis_c() + xlim(round(bench_val*0.8/0.453592),bench_val/0.453592-1) + theme_pubr(legend="right") + 
                    ylab("Ratio of success on Bench 3") + xlab("Bench 1 Weight (in lbs)") + labs(fill = "# of lifts") + scale_y_continuous(labels = percent)
                ggplotly(g, tooltip = c("Sample_Size", "text", "Bench1Lb"))
            } else if (input$lbskg == 0 & input$lbskg2 == 1) {
                bench_val <- test_bench(input$bench*0.453592)
                bench_one <- pl_web_clean %>% filter(Bench3Kg == bench_val, Equipment == input$equipment, Sex == input$sex) %>% group_by(Bench1Kg) %>% 
                    summarise(ratio = sum(Bench3SF/n()), Sample_Size = n()) %>% filter(ratio != 1) 
                g <- ggplot(bench_one, aes(Bench1Kg, ratio, fill = Sample_Size)) + geom_col(aes(text = paste0(round(ratio*100), "% success on lift 3"))) + 
                    scale_fill_viridis_c() + xlim(round(bench_val*0.8),bench_val-1) + theme_pubr(legend="right") + ylab("Ratio of success on Bench 3") + 
                    xlab("Bench 1 Weight (in kg)") + labs(fill = "# of lifts") + scale_y_continuous(labels = percent)
                ggplotly(g, tooltip = c("Sample_Size", "text", "Bench1Kg"))
            } else if (input$lbskg == 1 & input$lbskg2 == 0) {
                bench_val <- test_bench(input$bench)
                bench_one <- pl_web_clean %>% filter(Bench3Kg == bench_val, Equipment == input$equipment, Sex == input$sex) %>% group_by(Bench1Kg) %>% 
                    summarise(ratio = sum(Bench3SF/n()), Sample_Size = n()) %>% filter(ratio != 1) %>% mutate(Bench1Lb = Bench1Kg/0.453592)
                g <- ggplot(bench_one, aes(Bench1Lb, ratio, fill = Sample_Size)) + geom_col(aes(text = paste0(round(ratio*100), "% success on lift 3"))) + 
                    scale_fill_viridis_c() + xlim(round(bench_val*0.8/0.453592),bench_val/0.453592-1) + theme_pubr(legend="right") + 
                    ylab("Ratio of success on Bench 3") + xlab("Bench 1 Weight (in lbs)") + labs(fill = "# of lifts") + scale_y_continuous(labels = percent)
                ggplotly(g, tooltip = c("Sample_Size", "text", "Bench1Lb"))
            } else if (input$lbskg == 1 & input$lbskg2 == 1) {
                bench_val <- test_bench(input$bench)
                bench_one <- pl_web_clean %>% filter(Bench3Kg == bench_val, Equipment == input$equipment, Sex == input$sex) %>% group_by(Bench1Kg) %>% 
                    summarise(ratio = sum(Bench3SF/n()), Sample_Size = n()) %>% filter(ratio != 1) 
                g <- ggplot(bench_one, aes(Bench1Kg, ratio, fill = Sample_Size)) + geom_col(aes(text = paste0(round(ratio*100), "% success on lift 3"))) + 
                    scale_fill_viridis_c() + xlim(round(bench_val*0.8),bench_val-1) + theme_pubr(legend="right") + ylab("Ratio of success on Bench 3") + 
                    xlab("Bench 1 Weight (in kg)") + labs(fill = "# of lifts") + scale_y_continuous(labels = percent)
                ggplotly(g, tooltip = c("Sample_Size", "text", "Bench1Kg"))
            }
        }
    })
    
    #same as above
    output$bench2Plot <- renderPlotly({
        if (is.na(input$bench)|input$bench<0) {} else {
            if (input$lbskg == 0 & input$lbskg2 == 0) {
                bench_val <- test_bench(input$bench*0.453592)
                bench_two <- pl_web_clean %>% filter(Bench3Kg == bench_val, Equipment == input$equipment, Sex == input$sex) %>% group_by(Bench2Kg) %>% 
                    summarise(ratio = sum(Bench3SF/n()), Sample_Size = n()) %>% filter(ratio != 1) %>% mutate(Bench2Lb = Bench2Kg/0.453592)
                g <- ggplot(bench_two, aes(Bench2Lb, ratio, fill = Sample_Size)) + geom_col(aes(text = paste0(round(ratio*100), "% success on lift 3"))) + 
                    scale_fill_viridis_c() + xlim(round(bench_val*0.8/0.453592),bench_val/0.453592-1) + theme_pubr(legend="right") + 
                    ylab("Ratio of success on Bench 3") + xlab("Bench 2 Weight (in lbs)") + labs(fill = "# of lifts") + scale_y_continuous(labels = percent)
                ggplotly(g, tooltip = c("Sample_Size", "text", "Bench2Lb"))
            } else if (input$lbskg == 0 & input$lbskg2 == 1) {
                bench_val <- test_bench(input$bench*0.453592)
                bench_two <- pl_web_clean %>% filter(Bench3Kg == bench_val, Equipment == input$equipment, Sex == input$sex) %>% group_by(Bench2Kg) %>% 
                    summarise(ratio = sum(Bench3SF/n()), Sample_Size = n()) %>% filter(ratio != 1) 
                g <- ggplot(bench_two, aes(Bench2Kg, ratio, fill = Sample_Size)) + geom_col(aes(text = paste0(round(ratio*100), "% success on lift 3"))) + 
                    scale_fill_viridis_c() + xlim(round(bench_val*0.8),bench_val-1) + theme_pubr(legend="right") + ylab("Ratio of success on Bench 3") + 
                    xlab("Bench 2 Weight (in kg)") + labs(fill = "# of lifts") + scale_y_continuous(labels = percent)
                ggplotly(g, tooltip = c("Sample_Size", "text", "Bench2Kg"))
            } else if (input$lbskg == 1 & input$lbskg2 == 0) {
                bench_val <- test_bench(input$bench)
                bench_two <- pl_web_clean %>% filter(Bench3Kg == bench_val, Equipment == input$equipment, Sex == input$sex) %>% group_by(Bench2Kg) %>% 
                    summarise(ratio = sum(Bench3SF/n()), Sample_Size = n()) %>% filter(ratio != 1) %>% mutate(Bench2Lb = Bench2Kg/0.453592)
                g <- ggplot(bench_two, aes(Bench2Lb, ratio, fill = Sample_Size)) + geom_col(aes(text = paste0(round(ratio*100), "% success on lift 3"))) + 
                    scale_fill_viridis_c() + xlim(round(bench_val*0.8/0.453592),bench_val/0.453592-1) + theme_pubr(legend="right") + 
                    ylab("Ratio of success on Bench 3") + xlab("Bench 2 Weight (in lbs)") + labs(fill = "# of lifts") + scale_y_continuous(labels = percent)
                ggplotly(g, tooltip = c("Sample_Size", "text", "Bench2Lb"))
            } else if (input$lbskg == 1 & input$lbskg2 == 1) {
                bench_val <- test_bench(input$bench)
                bench_two <- pl_web_clean %>% filter(Bench3Kg == bench_val, Equipment == input$equipment, Sex == input$sex) %>% group_by(Bench2Kg) %>% 
                    summarise(ratio = sum(Bench3SF/n()), Sample_Size = n()) %>% filter(ratio != 1) 
                g <- ggplot(bench_two, aes(Bench2Kg, ratio, fill = Sample_Size)) + geom_col(aes(text = paste0(round(ratio*100), "% success on lift 3"))) + 
                    scale_fill_viridis_c() + xlim(round(bench_val*0.8),bench_val-1) + theme_pubr(legend="right") + ylab("Ratio of success on Bench 3") + 
                    xlab("Bench 2 Weight (in kg)") + labs(fill = "# of lifts") + scale_y_continuous(labels = percent)
                ggplotly(g, tooltip = c("Sample_Size", "text", "Bench2Kg"))
            }
        }
    })
    
    #same as above
    output$deadlift1Plot <- renderPlotly({
        if (is.na(input$deadlift)|input$deadlift<0) {} else {
            if (input$lbskg == 0 & input$lbskg2 == 0) {
                deadlift_val <- test_deadlift(input$deadlift*0.453592)
                deadlift_one <- pl_web_clean %>% filter(Deadlift3Kg == deadlift_val, Equipment == input$equipment, Sex == input$sex) %>% 
                    group_by(Deadlift1Kg) %>% summarise(ratio = sum(Deadlift3SF/n()), Sample_Size = n()) %>% filter(ratio != 1) %>% 
                    mutate(Deadlift1Lb = Deadlift1Kg/0.453592)
                g <- ggplot(deadlift_one, aes(Deadlift1Lb, ratio, fill = Sample_Size)) + geom_col(aes(text = paste0(round(ratio*100), "% success on lift 3"))) + 
                    scale_fill_viridis_c() + xlim(round(deadlift_val*0.8/0.453592),deadlift_val/0.453592-1) + theme_pubr(legend="right") + 
                    ylab("Ratio of success on Deadlift 3") + xlab("Deadlift 1 Weight (in lbs)") + labs(fill = "# of lifts") + 
                    scale_y_continuous(labels = percent)
                ggplotly(g, tooltip = c("Sample_Size", "text", "Deadlift1Lb"))
            } else if (input$lbskg == 0 & input$lbskg2 == 1) {
                deadlift_val <- test_deadlift(input$deadlift*0.453592)
                deadlift_one <- pl_web_clean %>% filter(Deadlift3Kg == deadlift_val, Equipment == input$equipment, Sex == input$sex) %>% 
                    group_by(Deadlift1Kg) %>% summarise(ratio = sum(Deadlift3SF/n()), Sample_Size = n()) %>% filter(ratio != 1) 
                g <- ggplot(deadlift_one, aes(Deadlift1Kg, ratio, fill = Sample_Size)) + geom_col(aes(text = paste0(round(ratio*100), "% success on lift 3"))) + 
                    scale_fill_viridis_c() + xlim(round(deadlift_val*0.8),deadlift_val-1) + theme_pubr(legend="right") + ylab("Ratio of success on Deadlift 3") 
                + xlab("Deadlift 1 Weight (in kg)") + labs(fill = "# of lifts") + scale_y_continuous(labels = percent)
                ggplotly(g, tooltip = c("Sample_Size", "text", "Deadlift1Kg"))
            } else if (input$lbskg == 1 & input$lbskg2 == 0) {
                deadlift_val <- test_deadlift(input$deadlift)
                deadlift_one <- pl_web_clean %>% filter(Deadlift3Kg == deadlift_val, Equipment == input$equipment, Sex == input$sex) %>% 
                    group_by(Deadlift1Kg) %>% summarise(ratio = sum(Deadlift3SF/n()), Sample_Size = n()) %>% filter(ratio != 1) %>% 
                    mutate(Deadlift1Lb = Deadlift1Kg/0.453592)
                g <- ggplot(deadlift_one, aes(Deadlift1Lb, ratio, fill = Sample_Size)) + geom_col(aes(text = paste0(round(ratio*100), "% success on lift 3"))) + 
                    scale_fill_viridis_c() + xlim(round(deadlift_val*0.8/0.453592),deadlift_val/0.453592-1) + theme_pubr(legend="right") + 
                    ylab("Ratio of success on Deadlift 3") + xlab("Deadlift 1 Weight (in lbs)") + labs(fill = "# of lifts") + 
                    scale_y_continuous(labels = percent)
                ggplotly(g, tooltip = c("Sample_Size", "text", "Deadlift1Lb"))
            } else if (input$lbskg == 1 & input$lbskg2 == 1) {
                deadlift_val <- test_deadlift(input$deadlift)
                deadlift_one <- pl_web_clean %>% filter(Deadlift3Kg == deadlift_val, Equipment == input$equipment, Sex == input$sex) %>% 
                    group_by(Deadlift1Kg) %>% summarise(ratio = sum(Deadlift3SF/n()), Sample_Size = n()) %>% filter(ratio != 1) 
                g <- ggplot(deadlift_one, aes(Deadlift1Kg, ratio, fill = Sample_Size)) + geom_col(aes(text = paste0(round(ratio*100), "% success on lift 3"))) + 
                    scale_fill_viridis_c() + xlim(round(deadlift_val*0.8),deadlift_val-1) + theme_pubr(legend="right") + ylab("Ratio of success on Deadlift 3") +
                    xlab("Deadlift 1 Weight (in kg)") + labs(fill = "# of lifts") + scale_y_continuous(labels = percent)
                ggplotly(g, tooltip = c("Sample_Size", "text", "Deadlift1Kg"))
            }
        }
        
    })
    
    #same as above
    output$deadlift2Plot <- renderPlotly({
        if (is.na(input$deadlift)|input$deadlift<0) {} else {
            if (input$lbskg == 0 & input$lbskg2 == 0) {
                deadlift_val <- test_deadlift(input$deadlift*0.453592)
                deadlift_two <- pl_web_clean %>% filter(Deadlift3Kg == deadlift_val, Equipment == input$equipment, Sex == input$sex) %>% 
                    group_by(Deadlift2Kg) %>% summarise(ratio = sum(Deadlift3SF/n()), Sample_Size = n()) %>% filter(ratio != 1) %>% 
                    mutate(Deadlift2Lb = Deadlift2Kg/0.453592)
                g <- ggplot(deadlift_two, aes(Deadlift2Lb, ratio, fill = Sample_Size)) + geom_col(aes(text = paste0(round(ratio*100), "% success on lift 3"))) + 
                    scale_fill_viridis_c() + xlim(round(deadlift_val*0.8/0.453592),deadlift_val/0.453592-1) + theme_pubr(legend="right") + 
                    ylab("Ratio of success on Deadlift 3") + xlab("Deadlift 2 Weight (in lbs)") + labs(fill = "# of lifts") + 
                    scale_y_continuous(labels = percent)
                ggplotly(g, tooltip = c("Sample_Size", "text", "Deadlift2Lb"))
            } else if (input$lbskg == 0 & input$lbskg2 == 1) {
                deadlift_val <- test_deadlift(input$deadlift*0.453592)
                deadlift_two <- pl_web_clean %>% filter(Deadlift3Kg == deadlift_val, Equipment == input$equipment, Sex == input$sex) %>% 
                    group_by(Deadlift2Kg) %>% summarise(ratio = sum(Deadlift3SF/n()), Sample_Size = n()) %>% filter(ratio != 1) 
                g <- ggplot(deadlift_two, aes(Deadlift2Kg, ratio, fill = Sample_Size)) + geom_col(aes(text = paste0(round(ratio*100), "% success on lift 3"))) + 
                    scale_fill_viridis_c() + xlim(round(deadlift_val*0.8),deadlift_val-1) + theme_pubr(legend="right") + ylab("Ratio of success on Deadlift 3") +
                    xlab("Deadlift 2 Weight (in kg)") + labs(fill = "# of lifts") + scale_y_continuous(labels = percent)
                ggplotly(g, tooltip = c("Sample_Size", "text", "Deadlift2Kg"))
            } else if (input$lbskg == 1 & input$lbskg2 == 0) {
                deadlift_val <- test_deadlift(input$deadlift)
                deadlift_two <- pl_web_clean %>% filter(Deadlift3Kg == deadlift_val, Equipment == input$equipment, Sex == input$sex) %>% 
                    group_by(Deadlift2Kg) %>% summarise(ratio = sum(Deadlift3SF/n()), Sample_Size = n()) %>% filter(ratio != 1) %>% 
                    mutate(Deadlift2Lb = Deadlift2Kg/0.453592)
                g <- ggplot(deadlift_two, aes(Deadlift2Lb, ratio, fill = Sample_Size)) + geom_col(aes(text = paste0(round(ratio*100), "% success on lift 3"))) + 
                    scale_fill_viridis_c() + xlim(round(deadlift_val*0.8/0.453592),deadlift_val/0.453592-1) + theme_pubr(legend="right") + 
                    ylab("Ratio of success on Deadlift 3") + xlab("Deadlift 2 Weight (in lbs)") + labs(fill = "# of lifts") + 
                    scale_y_continuous(labels = percent)
                ggplotly(g, tooltip = c("Sample_Size", "text", "Deadlift2Lb"))
            } else if (input$lbskg == 1 & input$lbskg2 == 1) {
                deadlift_val <- test_deadlift(input$deadlift)
                deadlift_two <- pl_web_clean %>% filter(Deadlift3Kg == deadlift_val, Equipment == input$equipment, Sex == input$sex) %>% 
                    group_by(Deadlift2Kg) %>% summarise(ratio = sum(Deadlift3SF/n()), Sample_Size = n()) %>% filter(ratio != 1) 
                g <- ggplot(deadlift_two, aes(Deadlift2Kg, ratio, fill = Sample_Size)) + geom_col(aes(text = paste0(round(ratio*100), "% success on lift 3"))) + 
                    scale_fill_viridis_c() + xlim(round(deadlift_val*0.8),deadlift_val-1) + theme_pubr(legend="right") + ylab("Ratio of success on Deadlift 3") +
                    xlab("Deadlift 2 Weight (in kg)") + labs(fill = "# of lifts") + scale_y_continuous(labels = percent)
                ggplotly(g, tooltip = c("Sample_Size", "text", "Deadlift2Kg"))
            }
        }
        
    })
    
    #have text available for displaying a remarkable lifter (top 1%)
    output$example_lifter <- renderText({
        null_lifter <- F
        if(is.na(input$weight)) { #if no weight listed, print nothing
            paste0("")
        } else {
            #filter for lifter near input weight 
        example_lifter <- pl_top01 %>% filter(Sex == input$sex, abs(BodyweightKg-ifelse(input$lbskg==1,input$weight,input$weight*0.453592)) < 2, 
                                              Equipment == input$equipment)
        if (nrow(example_lifter) == 0) { #change from lbs to kg if entering in lbs
            example_lifter <- pl_top01 %>% filter(Sex == input$sex, abs(BodyweightKg-ifelse(input$lbskg==1,input$weight,input$weight*0.453592)) < 5, 
                                                  Equipment == input$equipment)
            if (nrow(example_lifter) == 0) { #if no lifters found, set value to true
                null_lifter <- T
            }
        }
        random_nums <- runif(nrow(example_lifter))
        index <- which.max(random_nums)
        explain_lifter <- example_lifter[index,] #use random uniform variable to sample from list of lifters near your weight 


        if (null_lifter == T) { #do nothing if null lifter
            paste0("")
        } else {
            if (input$lbskg2 == 0) { #print using if-else statements for gender to print out statement about lifter
                paste0("Did you know? On ", explain_lifter$Date, ", ", explain_lifter$Name, " lifted an incredible ", round(explain_lifter$TotalKg/0.453592), 
                       "lbs while weighing ", round(explain_lifter$BodyweightKg/0.453592), " lbs, meaning", ifelse(input$sex =="M", " he", " she"), " lifted ",
                       round(explain_lifter$BwRRaw,2), "x", ifelse(input$sex =="M", " his", " her"), " body weight.", ifelse(input$sex == "M", " His", " Her"), 
                       " squat was ", ifelse(is.na(explain_lifter$Squat4Kg), round(explain_lifter$Best3SquatKg/0.453592), ifelse(explain_lifter$Squat4Kg > 0, 
                                                                                                  round(explain_lifter$Squat4Kg/0.453592), 
                                                                                                  round(explain_lifter$Best3SquatKg/0.453592))), "lbs,",
                       ifelse(input$sex =="M", " his", " her"), " bench press was ", ifelse(is.na(explain_lifter$Bench4Kg), 
                                                                                            round(explain_lifter$Best3BenchKg/0.453592), 
                                                                                            ifelse(explain_lifter$Bench4Kg > 0, 
                                                                                                   round(explain_lifter$Bench4Kg/0.453592), 
                                                                                                   round(explain_lifter$Best3BenchKg/0.453592))), " lbs, and",
                       ifelse(input$sex =="M", " his", " her"), " deadlift was ", ifelse(is.na(explain_lifter$Deadlift4Kg), 
                                                                                         round(explain_lifter$Best3DeadliftKg/0.453592), 
                                                                                         ifelse(explain_lifter$Deadlift4Kg > 0, 
                                                                                                round(explain_lifter$Deadlift4Kg/0.453592), 
                                                                                                round(explain_lifter$Best3DeadliftKg/0.453592))), " lbs. This", 
                       ifelse(explain_lifter$Tested=="Yes", " was", " was not"), " a drug-tested meet.")
            } else {
            paste0("Did you know? On ", explain_lifter$Date, ", ", explain_lifter$Name, " lifted an incredible ", explain_lifter$TotalKg, "kg while weighing ",
                   explain_lifter$BodyweightKg, " kg, meaning", ifelse(input$sex =="M", " he", " she"), " lifted ", round(explain_lifter$BwRRaw,2), "x",
                   ifelse(input$sex =="M", " his", " her"), " body weight.", ifelse(input$sex == "M", " His", " Her"), " squat was ",
                   ifelse(is.na(explain_lifter$Squat4Kg), explain_lifter$Best3SquatKg, ifelse(explain_lifter$Squat4Kg > 0, 
                                                                                              explain_lifter$Squat4Kg, explain_lifter$Best3SquatKg)), "kg,",
                   ifelse(input$sex =="M", " his", " her"), " bench press was ", ifelse(is.na(explain_lifter$Bench4Kg), explain_lifter$Best3BenchKg, 
                                                                                        ifelse(explain_lifter$Bench4Kg > 0, explain_lifter$Bench4Kg, 
                                                                                               explain_lifter$Best3BenchKg)), " kg, and",
                   ifelse(input$sex =="M", " his", " her"), " deadlift was ", ifelse(is.na(explain_lifter$Deadlift4Kg), explain_lifter$Best3DeadliftKg, 
                                                                                     ifelse(explain_lifter$Deadlift4Kg > 0, explain_lifter$Deadlift4Kg, 
                                                                                            explain_lifter$Best3DeadliftKg)), " kg. This", 
                   ifelse(explain_lifter$Tested=="Yes", " was", " was not"), " a drug-tested meet.")
            }
        }
        }
    })
    
    #not used now, shows Dots score for usage instead of Wilks score
    output$ref <- renderUI({
        tagList("", a("Dots score reference", href="https://drive.google.com/drive/folders/1-0rE_GbYWVum7U1UfpR0XWiFR9ZNbXWJ"))
    })
    }

# Run the application 
shinyApp(ui = ui, server = server)
