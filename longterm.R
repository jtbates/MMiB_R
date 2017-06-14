library(shiny)
library(RColorBrewer)

default_next_p <- 'p + r*p*(1-p)'

calc_trajectory <- function(next_p_str, p0, r, nsteps) {
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
    headerPanel("Bifurcation diagram for one-population model"),
    sidebarPanel(
      h4("Population model"),
      helpText("Enter a formula defining the population model, using \"p\"
                to denote the size of the population and \"r\" to denote 
                the parameter to be varied."),
      textInput('next_p', "next_p = ", default_next_p),
      numericInput('p0', "Population at time 0", 0.1,
                   min = 0.0001, max = 5),
      h4("Graph parameters"),
      sliderInput('rlimit',
                  label = "Range of \"r\" to show on graph",
                  min = 0.1, max = 4, value = c(1.5, 2.9), step=0.1),
      numericInput('num_rinc', "Number of \"r\" increments", 400,
                   min = 10, max = 10000),
      sliderInput('plimit', label = "Population range to show on graph",
                  min = 0,  max = 3, value = c(0, 1.6), step=0.1),
      numericInput('num_steps', "Number of iterations", 32*3,
                   min = 10, max = 10000),
      numericInput('num_plot_steps', "Number of population values to plot", 32,
                   min = 10, max = 10000)
    ),
    mainPanel(
      plotOutput("pop_plot", click = "plot_click")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$pop_plot <- renderPlot({
    plot.new()
    plot.window(xlim = input$rlimit,  ylim=input$plimit,
                yaxs="i", xaxs="i")
    box()
    axis(1, col.axis = "grey30",
         at=seq(input$rlimit[1], input$rlimit[2], length.out=11))
    axis(2, col.axis = "grey30",
         at=seq(input$plimit[1], input$plimit[2], length.out=11))
    grid(10, 10)
    title(col.main = "green4", col.sub = "green4",
          xlab = "r value", ylab = "Attracting Population Cycle",
          col.lab = "blue", font.lab = 3)
    for (r in seq(from=input$rlimit[1], to=input$rlimit[2],
                  length.out=input$num_rinc)) {
      traj <- calc_trajectory(input$next_p, input$p0, r, input$num_steps)
      pvals <- traj[(input$num_steps-input$num_plot_steps+1):input$num_steps]
      points(rep(r, input$num_plot_steps), pvals, pch=".")
    }
  },
  height=600)
}

# Run the application 
shinyApp(ui = ui, server = server)