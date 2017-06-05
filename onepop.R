library(shiny)
library(RColorBrewer)

default_next_p <- 'p + .2*p*(1-p/10)'

calc_trajectory <- function(next_p_str, p0, nsteps) {
  next_p <- parse(text=next_p_str)
  pops <- p <- p0
  for (i in 1:nsteps) {
    p <- eval(next_p)
    pops <- c(pops, p)
  }
  pops
}

# Define UI for application
ui <- basicPage(
  pageWithSidebar(
    headerPanel("One-population model"),
    sidebarPanel(
      h4("Population model"),
      helpText("Enter a formula defining the population model, using \"p\"
                to denote the size of the population"),
      textInput('next_p', "next_p = ", default_next_p),
      actionButton('updateButton', "Update model"),
      h4("Graph parameters"),
      sliderInput('ylimit', label = "Population range to show on graph",
                  min = 0,  max = 100, value = c(0, 20)),
      numericInput('nsteps', "Number of time steps", 20,
                   min = 10, max = 100)
    ),
    mainPanel(
      plotOutput("pop_plot", click = "plot_click")
    )
  )
)

# Define server logic
server <- function(input, output) {
  data <- reactiveValues(
    next_p = default_next_p,
    trajectories = list()
  )
  
  observeEvent(input$updateButton, {
    data$next_p <- input$next_p
    data$trajectories <- list()
  })
  
  observeEvent(input$nsteps, {
    data$trajectories <- list()
    data$times <- 0:(input$nsteps)
  })
  
  observeEvent(input$ylimit, {
    data$trajectories <- list()
  })
  
  observeEvent(input$plot_click, {
    pops <- calc_trajectory(next_p_str=input$next_p,
                            p0=input$plot_click$y,
                            nsteps=input$nsteps)
    data$trajectories <- c(data$trajectories, list(pops))
  })
  
  output$pop_plot <- renderPlot({
    num_traj <- length(data$trajectories)
    plot.new()
    plot.window(xlim = c(0, input$nsteps),  ylim=input$ylimit,
                yaxs="i", xaxs="i")
    box()
    axis(1, col.axis = "grey30",
         at=seq(0, input$nsteps, length.out=11))
    axis(2, col.axis = "grey30",
         at=seq(input$ylimit[1], input$ylimit[2], length.out=11))
    grid(10, 10)
    title(col.main = "green4", col.sub = "green4",
          xlab = "Time", ylab = "Population p",
          col.lab = "blue", font.lab = 3)
    if(num_traj > 0) {
      for(i in 1:length(data$trajectories)) {
        trajectory <- data$trajectories[[i]] 
        line_col <- brewer.pal(7, "Dark2")[(i %% 7) + 1]
        lines(data$times, trajectory, col=line_col)
      }
    }
  },
  height=600)
}

# Run the application 
shinyApp(ui = ui, server = server)