library(shiny)

ui <- fluidPage(
    titlePanel("reproval"),
    inputPanel(
        sliderInput(".p.previous", label = ".p.previous:",
                    min = 0.01, max = .999, value = .7, step = 0.01),
        numericInput(".par.iota", label = ".par.iota:",
                    min = 0, max = 1, value = 0.1, step = 0.001),
        numericInput(".par.rho", label = ".par.rho:",
                    min = 0, max = 7, value = 1, step = 0.001)),
        mainPanel(
           plotOutput("plocisz")))

server <- function(input, output) {
    output$plocisz <- renderPlot({
        plot(exp((log(input$.p.previous / (1 - input$.p.previous))) + 
                     input$.par.iota * seq(0, 60, 1) - input$.par.rho) / 
                 (1 + exp((log(input$.p.previous / (1 - input$.p.previous))) + 
                              input$.par.iota * seq(0, 60, 1) - input$.par.rho)),
            ylab="p\'", xlab = "interval", xlim=c(0,70), ylim=c(0,1))})}

shinyApp(ui = ui, server = server)
