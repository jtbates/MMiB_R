library(shiny)
library(RColorBrewer)

default_next_p_str <- 'p + 1.8*p*(1-p/10)'

next_p_fun <- function(next_p_str=default_next_p_str) {
  next_p <- parse(text=next_p_str)
  function(p) {
    eval(next_p)
  }
}

# Define UI for application
ui <- basicPage(
  pageWithSidebar(
    headerPanel("Cobweb plot for one-pop difference equation model"),
    sidebarPanel(
      h4("Population model"),
      helpText("Enter a formula defining the population model, using \"p\"
                to denote the size of the population"),
      textInput('next_p_str', "next_p = ", default_next_p_str),
      actionButton('updateButton', "Update model"),
      h4("Graph parameters"),
      sliderInput('plimit', label = "Population range to show on graph",
                  min = 0,  max = 100, value = c(0, 20))
    ),
    mainPanel(
      plotOutput('cobweb_plot', click = 'plot_click')
    )
  )
)

# Define server logic
server <- function(input, output) {
  data <- reactiveValues(
    next_p = next_p_fun(),
    init_p = NULL
  )
  
  observeEvent(input$updateButton, {
    data$next_p <- next_p_fun(input$next_p_str)
  })
  
  observeEvent(input$plimit, {
  })
  
  observeEvent(input$plot_click, {
    data$init_p <- input$plot_click$x
  })
  
  output$cobweb_plot <- renderPlot({
    plot.new()
    plot.window(xlim = input$plimit,  ylim = input$plimit,
                xaxs = 'i', yaxs = 'i')
    box()
    axis(1, col.axis = 'grey30',
         at=seq(input$plimit[1], input$plimit[2], length.out=11))
    axis(2, col.axis = 'grey30',
         at=seq(input$plimit[1], input$plimit[2], length.out=11))
    grid(10, 10)
    title(col.main = 'green4', col.sub = 'green4',
          xlab = expression('P'['t']),
          ylab = expression('P'['t'+1]),
          col.lab = 'blue', font.lab = 3)
    
    # plot the identity line
    abline(a = 0, b = 1, col = 'red')
    
    # plot the difference equation
    x <- seq(input$plimit[1], input$plimit[2], length.out=50)
    y <- sapply(x, data$next_p)
    lines(x, y, col = 'blue')
    
    # plot the cobweb
    if(!is.null(data$init_p)) {
      p2 <- data$init_p
      for(i in 1:50) {
        p1 <- p2
        p2 <- data$next_p(p1)
        lines(c(p1, p1), c(p1, p2)) # vertical cobweb step
        if(p2 < 0) { break }
        lines(c(p1, p2), c(p2, p2)) # horizontal cobweb step
      }
    }
    
  },
  height=600)
}

# Run the application 
shinyApp(ui = ui, server = server)