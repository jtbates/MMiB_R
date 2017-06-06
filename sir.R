library(shiny)
library(RColorBrewer)

default_next_s <- 's-.1*s*i'
default_next_i <- 'i+.1*s*i-.05*i'
default_next_r <- 'r+.05*i'
default_totpop <- 1

calc_trajectory <- function(next_state, state0, nsteps) {
  pops <- data.frame(s=state0['s'], i=state0['i'], r=state0['r'])
  for (j in 1:nsteps) {
    state <- next_state(pops[j,])
    pops <- rbind(pops, state)
  }
  pops
}

next_fun <- function(next_s_str=default_next_s,
                     next_i_str=default_next_i,
                     next_r_str=default_next_r) {
  function(input) {
    s <- input['s']; i <- input['i']; r <- input['r']
    next_s <- eval(parse(text=next_s_str))
    next_i <- eval(parse(text=next_i_str))
    next_r <- eval(parse(text=next_r_str))
    c(next_s, next_i, next_r)
  }
}

# Define UI for application
ui <- fluidPage(
  title = "SIR infectious disease model",
  fluidRow(
    column(6,
      plotOutput('phase_plot', click = 'plot_click')
    ),
    column(6,
      plotOutput('pop_plot')
    )
  ),
  fluidRow(
    column(7,
      h4("Population model"),
      fluidRow(
        column(5,
          helpText('Enter formulas for difference equations defining the model,
                    using "s" to denote the susceptible class, "i" to denote
                    the infected class, and "r" to denote the removed class')
        ),
        column(5,
          textInput('next_s_str', "next_s = ", default_next_s),
          textInput('next_i_str', "next_i = ", default_next_i),
          textInput('next_r_str', "next_r = ", default_next_r),
          actionButton('updateButton', "Update model")
        )
      )
    ),
    column(5,
      h4("Graph parameters"),
      numericInput('totpop', "Total population size", default_totpop,
                   min = 1, max = 1e+10),
      numericInput('nsteps', "Number of time steps", 100,
                   min = 10, max = 1e+4)
    )
  )
)

# Define server logic
server <- function(input, output) {
  data <- reactiveValues(
    next_state = next_fun(),
    xlim_pop = c(0, 100),
    ylim_pop = c(0, default_totpop),
    xlim_phase = c(0, default_totpop),
    ylim_phase = c(0, default_totpop),
    trajectories = list()
  )
  
  observeEvent(input$updateButton, {
    data$next_state <- next_fun(input$next_s_str,
                                input$next_i_str,
                                input$next_r_str)
    data$trajectories <- list()
  })
  
  observeEvent(input$nsteps, {
    data$xlim_pop <- c(0, input$nsteps)
    data$times <- 0:(input$nsteps)
    data$trajectories <- list()
  })
  
  observeEvent(input$totpop, {
    data$xlim_phase <- c(0, input$totpop)
    data$ylim_phase <- c(0, input$totpop)
    data$ylim_pop <- c(0, input$totpop)
    data$trajectories <- list()
  })
  
  observeEvent(input$plot_click, {
    s <- input$plot_click$x
    i <- input$plot_click$y
    r <- input$totpop - s - i
    pops <- calc_trajectory(next_state=data$next_state,
                            state0=c(s=s, i=i, r=r),
                            nsteps=input$nsteps)
    data$trajectories <- c(data$trajectories, list(pops))
  })
  
  output$phase_plot <- renderPlot({
    plot.new()
    plot.window(xlim = data$xlim_phase,  ylim=data$ylim_phase,
                xaxs='i', yaxs='i')
    box()
    ticks <- seq(data$xlim_phase[1], data$xlim_phase[2], length.out=11)
    axis(1, col.axis = 'grey30', at=ticks)
    axis(2, col.axis = 'grey30', at=ticks)
    grid(10, 10)
    title(col.main = 'green4', col.sub = 'green4',
          xlab = "Susceptibles", ylab = "Infectives",
          col.lab = 'blue', font.lab = 3)
    lines(c(0, input$totpop), c(input$totpop, 0))
    num_traj <- length(data$trajectories)
    if(num_traj > 0) {
      for(i in 1:num_traj) {
        trajectory <- data$trajectories[[i]] 
        line_col <- brewer.pal(7, 'Dark2')[(i %% 7) + 1]
        lines(trajectory$s, trajectory$i, col=line_col)
      }
    }
  },
  height=400)
  
  output$pop_plot <- renderPlot({
    plot.new()
    plot.window(xlim = data$xlim_pop,  ylim=data$ylim_pop, xaxs='i', yaxs='i')
    box()
    axis(1, col.axis = 'grey30',
         at=seq(data$xlim_pop[1], data$xlim_pop[2], length.out=11))
    axis(2, col.axis = 'grey30',
         at=seq(data$ylim_pop[1], data$ylim_pop[2], length.out=11))
    grid(10, 10)
    title(col.main = 'green4', col.sub = 'green4',
          xlab = "Time", ylab = "Populations",
          col.lab = 'blue', font.lab = 3)
    num_traj <- length(data$trajectories)
    if(num_traj > 0) {
      trajectory <- data$trajectories[[num_traj]] 
      lines(data$times, trajectory$s, col='blue')
      lines(data$times, trajectory$i, col='red')
      lines(data$times, trajectory$r, col='orange')
    }
  },
  height=400)
}

# Run the application 
shinyApp(ui = ui, server = server)