library(shiny)
library(RColorBrewer)

default_next_p <- 'p+.4*p*(1-p/10)-.05*p*q'
default_next_q <- 'q-.2*q+.08*p*q'

calc_trajectory <- function(next_p_str, next_q_str, p0, q0, nsteps) {
  next_p <- parse(text=next_p_str)
  next_q <- parse(text=next_q_str)
  p_pops <- p <- p0
  q_pops <- q <- q0
  for (i in 1:nsteps) {
    new_p <- eval(next_p)
    new_q <- eval(next_q)
    p <- new_p
    q <- new_q
    p_pops <- c(p_pops, p)
    q_pops <- c(q_pops, q)
  }
  data.frame(p=p_pops, q=q_pops)
}

# Define UI for application
ui <- fluidPage(
  title = "Two-population model",
  fluidRow(
    column(6,
      plotOutput('phase_plot', click = 'plot_click')
    ),
    column(6,
      plotOutput('pop_plot')
    )
  ),
  fluidRow(
    column(4,
      h4("Population model"),
      helpText("Enter formulas defining the population model, using \"p\"
               and \"q\" to denote the size of the populations"),
      textInput('next_p', "next_p = ", default_next_p),
      textInput('next_q', "next_q = ", default_next_q),
      actionButton('updateButton', "Update model")
    ),
    column(6,
      h4("Graph parameters"),
      sliderInput('p_limit', label = "Range of p values to display",
                  min = 0,  max = 50, value = c(0, 10)),
      sliderInput('q_limit', label = "Range of q values to display",
                  min = 0,  max = 50, value = c(0, 10)),
      numericInput('nsteps', "Number of time steps", 100,
                   min = 10, max = 10000)
    )
  )
)

# Define server logic
server <- function(input, output) {
  data <- reactiveValues(
    next_p = default_next_p,
    next_q = default_next_q,
    trajectories = list()
  )
  
  observeEvent(input$updateButton, {
    data$next_p <- input$next_p
    data$next_q <- input$next_q
    data$trajectorie <- list()
  })
  
  observeEvent(input$nsteps, {
    data$trajectories <- list()
    data$times <- 0:(input$nsteps)
  })
  
  observeEvent(input$p_limit, {
    data$trajectories <- list()
  })
  
  observeEvent(input$q_limit, {
    data$trajectories <- list()
  })
  
  observeEvent(input$plot_click, {
    pops <- calc_trajectory(next_p_str=input$next_p,
                            next_q_str=input$next_q,
                            p0=input$plot_click$x,
                            q0=input$plot_click$y,
                            nsteps=input$nsteps)
    print(pops)
    data$trajectories <- c(data$trajectories, list(pops))
  })
  
  output$phase_plot <- renderPlot({
    num_traj <- length(data$trajectories)
    plot.new()
    plot.window(xlim = input$p_limit,  ylim=input$q_limit,
                yaxs='i', xaxs='i')
    box()
    axis(1, col.axis = 'grey30',
         at=seq(input$p_limit[1], input$p_limit[2], length.out=11))
    axis(2, col.axis = 'grey30',
         at=seq(input$q_limit[1], input$q_limit[2], length.out=11))
    grid(10, 10)
    title(col.main = 'green4', col.sub = 'green4',
          xlab = "Population p", ylab = "Population q",
          col.lab = 'blue', font.lab = 3)
    if(num_traj > 0) {
      for(i in 1:length(data$trajectories)) {
        trajectory <- data$trajectories[[i]] 
        line_col <- brewer.pal(7, 'Dark2')[(i %% 7) + 1]
        lines(trajectory$p, trajectory$q, col=line_col)
      }
    }
  },
  height=400)
  
  output$pop_plot <- renderPlot({
    num_traj <- length(data$trajectories)
    plot.new()
    pq_limit <- c(min(input$p_limit[1], input$q_limit[1]),
                  max(input$p_limit[2], input$q_limit[2]))
    plot.window(xlim = c(0, input$nsteps),  ylim=pq_limit,
                yaxs='i', xaxs='i')
    box()
    axis(1, col.axis = 'grey30',
         at=seq(0, input$nsteps, length.out=11))
    axis(2, col.axis = 'grey30',
         at=seq(pq_limit[1], pq_limit[2], length.out=11))
    grid(10, 10)
    title(col.main = 'green4', col.sub = 'green4',
          xlab = "Time", ylab = "Population",
          col.lab = 'blue', font.lab = 3)
    if(num_traj > 0) {
      trajectory <- data$trajectories[[num_traj]] 
      print(length(data$times))
      print(length(trajectory$p))
      lines(data$times, trajectory$p, col='red')
      lines(data$times, trajectory$q, col='blue')
    }
  },
  height=400)
}

# Run the application 
shinyApp(ui = ui, server = server)