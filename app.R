library(shiny)
library(ggplot2)
library(ggpubr)
source("helpers.R")
p_range <- seq(0, 10, 0.1)
t_input <- seq(0, 40)

# Define UI for application that draws a histogram
ui <- fluidPage(
  withMathJax(),
  titlePanel("Das Cobweb Modell"),
  fluidRow(
    column(3,
           h3("Grundeinstellungen"),
           numericInput("n_plots", label = h3("Anzahl Plots"), 
                        value = 1, min = 1, max = 3, step = 1),
           sliderInput("p_0", 
                       label='Ausgangspreis \\( p_0 \\)',
                       min = 0.5, max = 10, step=0.5, value = 5.0)
           ),
    column(3, 
           h3("Diagram 1"),
           sliderInput("intercept_supply_1", 
                       label='Achsenabschnitt Angebotskurve 1: \\( \\alpha \\)',
                       min = -5.0, max = 5.0, step=0.5, value = -0.5),
           sliderInput("slope_supply_1", 
                       label='Steigung Angebotskurve 1: \\( \\beta \\)',
                       min = 0.1, max = 1.9, step=0.05, value = 0.5),
           sliderInput("intercept_demand_1", 
                       label='Achsenabschnitt Nachfragekurve 1: \\( \\gamma \\)',
                       min = 0.5, max = 10.0, step=0.5, value = 5.0),
           sliderInput("slope_demand_1", 
                       label='Steigung Nachfragekurve 1: \\( \\delta \\)',
                       min = -1.9, max = -0.1, step=0.05, value = -0.5)
           ),
    column(3,
           h3("Diagram 2"),
           conditionalPanel(
             condition = "input.n_plots>1",
             sliderInput("intercept_supply_2", 
                         label='Achsenabschnitt Angebotskurve 2: \\( \\zeta \\)',
                         min = -5.0, max = 5.0, step=0.5, value = -0.5),
             sliderInput("slope_supply_2", 
                         label='Steigung Angebotskurve 2: \\( \\eta \\)',
                         min = 0.1, max = 1.9, step=0.05, value = 0.5),
             sliderInput("intercept_demand_2", 
                         label='Achsenabschnitt Nachfragekurve 2: \\( \\theta \\)',
                         min = 0.5, max = 10.0, step=0.5, value = 5.0),
             sliderInput("slope_demand_2", 
                         label='Steigung Nachfragekurve 2: \\( \\iota \\)',
                         min = -1.9, max = -0.1, step=0.05, value = -0.5)
           )
           ),
    column(3,
           h3("Diagram 3"),
           conditionalPanel(
             condition = "input.n_plots>2",
             sliderInput("intercept_supply_3", 
                         label='Achsenabschnitt Angebotskurve 3: \\( \\kappa \\)',
                         min = -5.0, max = 5.0, step=0.5, value = -0.5),
             sliderInput("slope_supply_3", 
                         label='Steigung Angebotskurve 3: \\( \\lambda \\)',
                         min = 0.1, max = 1.9, step=0.05, value = 0.5),
             sliderInput("intercept_demand_3", 
                         label='Achsenabschnitt Nachfragekurve 3: \\( \\mu \\)',
                         min = 0.5, max = 10.0, step=0.5, value = 5.0),
             sliderInput("slope_demand_3", 
                         label='Steigung Nachfragekurve 3: \\( \\nu \\)',
                         min = -1.9, max = -0.1, step=0.05, value = -0.5)
           )
           )
    ),
  fluidRow(
    column(3),
    column(3,
           h3("Abbildung zum erstem Fall"),
           plotOutput("s_d_1"),
           plotOutput("dyn_1")
           ),
    column(3,
           conditionalPanel(
             condition = "input.n_plots>1",
             h3("Abbildung zweiter Fall"),
             plotOutput("s_d_2"),
             plotOutput("dyn_2")
             )
           ),
    column(3,
           conditionalPanel(
             condition = "input.n_plots>2",
             h3("Dritter Fall"),
             plotOutput("s_d_3"),
             plotOutput("dyn_3")
             )
           )),
  fluidRow(
    column(3),
    column(9,
           downloadButton("downloadPlot", "Download der Abbildungen im PDF Format"),
           h3("Beschreibung des Modells"),
           p("Eine genaue Beschreibung des Modelles und der Implementierung in R finden Sie im Begleitdokument (Moodle oder auf Github im Ordner 'beschreibung')."),
           h3("Benutzung der App"),
           p("Parameterwerte können auf der linken Seite geändert werden. Der Download Button unter der Abbildung erlaubt Ihnen die aktuelle Version der Abbildungen als PDF herunterzuladen."),
           h3("Leitfragen:"),
           p("...")
           )
    )
  )


server <- function(input, output) {
  # The first figure
  
  eq_price_1 <- reactive({
    get_eq_price(
      intercept_supply=input$intercept_supply_1, 
      intercept_demand=input$intercept_demand_1,
      slope_supply=input$slope_supply_1, 
      slope_demand=input$slope_demand_1)
  })
  
   s_d_1 <- reactive({
     make_supply_demand_plot(
       price_range=p_range, 
       intercept_nachfrage=input$intercept_demand_1, 
       intercept_angebot=input$intercept_supply_1, 
       slope_nachfrage=input$slope_demand_1, 
       slope_angebot=input$slope_supply_1,
       plot_n=1
     )
   })
   output$s_d_1 <- renderPlot({
     validate(need(input$intercept_supply_1<input$intercept_demand_1, 
                   message = "Angebotskurve 1 beginnt ueber Nachfragekurve 1"))
     validate(need((input$intercept_supply_1 + input$slope_supply_1*10) > 
                     (input$intercept_demand_1 + input$slope_demand_1*10), 
                   message = "Kein Gleichgewicht für p<10"))
     s_d_1()
   })
   
   dyn_diag_1 <- reactive({
     p_dynamics <- price_func(
       p_0=input$p_0, 
       intercept_demand=input$intercept_demand_1, 
       slope_demand=input$slope_demand_1, 
       intercept_supply=input$intercept_supply_1, 
       slope_supply=input$slope_supply_1, 
       timesteps=t_input
       )
     
     demand_supply_dynamics <- tibble(
       Zeit=t_input,
       Preisdynamik=p_dynamics)
     
     get_dyn_ggplot(demand_supply_dynamics, "Preisdynamik", 
                    eq_price=eq_price_1()) + 
       ggtitle(TeX("Preisdynamik Fall 1"))
   })
   output$dyn_1 <- renderPlot({
     validate(need(input$intercept_supply_1<input$intercept_demand_1, 
                   message = "Angebotskurve 1 beginnt ueber Nachfragekurve 1"))
     validate(need((input$intercept_supply_1 + input$slope_supply_1*10) > 
                     (input$intercept_demand_1 + input$slope_demand_1*10), 
                   message = "Kein Gleichgewicht für p<10"))
     dyn_diag_1()
   })
   
   # The second figures
   eq_price_2 <- reactive({
     get_eq_price(
       intercept_supply=input$intercept_supply_2, 
       intercept_demand=input$intercept_demand_2,
       slope_supply=input$slope_supply_2, 
       slope_demand=input$slope_demand_2)
   })
   
   s_d_2 <- reactive({
     make_supply_demand_plot(
       price_range=p_range, 
       intercept_nachfrage=input$intercept_demand_2, 
       intercept_angebot=input$intercept_supply_2, 
       slope_nachfrage=input$slope_demand_2, 
       slope_angebot=input$slope_supply_2,
       plot_n=2
     )
   })
   output$s_d_2 <- renderPlot({
     validate(need(input$intercept_supply_2<input$intercept_demand_2, 
                   message = "Angebotskurve 2 beginnt ueber Nachfragekurve 2"))
     validate(need((input$intercept_supply_2 + input$slope_supply_2*10) > 
                     (input$intercept_demand_2 + input$slope_demand_2*10), 
                   message = "Kein Gleichgewicht für p<10"))
     s_d_2()
   })
   
   dyn_diag_2 <- reactive({
     p_dynamics <- price_func(
       p_0=input$p_0, 
       intercept_demand=input$intercept_demand_2, 
       slope_demand=input$slope_demand_2, 
       intercept_supply=input$intercept_supply_2, 
       slope_supply=input$slope_supply_2, 
       timesteps=t_input
     )
     
     demand_supply_dynamics <- tibble(
       Zeit=t_input,
       Preisdynamik=p_dynamics)
     
     get_dyn_ggplot(demand_supply_dynamics, "Preisdynamik", 
                    eq_price=eq_price_2()) + 
       ggtitle(TeX("Preisdynamik Fall 2"))
   })
   output$dyn_2 <- renderPlot({
     validate(need(input$intercept_supply_2<input$intercept_demand_2, 
                   message = "Angebotskurve 2 beginnt ueber Nachfragekurve 2"))
     validate(need((input$intercept_supply_2 + input$slope_supply_2*10) > 
                     (input$intercept_demand_2 + input$slope_demand_2*10), 
                   message = "Kein Gleichgewicht für p<10"))
     dyn_diag_2()
   })
   
   # The third figures
   eq_price_3 <- reactive({
     get_eq_price(
       intercept_supply=input$intercept_supply_3, 
       intercept_demand=input$intercept_demand_3,
       slope_supply=input$slope_supply_3, 
       slope_demand=input$slope_demand_3)
   })
   
   s_d_3 <- reactive({
     make_supply_demand_plot(
       price_range=p_range, 
       intercept_nachfrage=input$intercept_demand_3, 
       intercept_angebot=input$intercept_supply_3, 
       slope_nachfrage=input$slope_demand_3, 
       slope_angebot=input$slope_supply_3,
       plot_n=3
     )
   })
   output$s_d_3 <- renderPlot({
     validate(need(input$intercept_supply_3<input$intercept_demand_3, 
                   message = "Angebotskurve 3 beginnt ueber Nachfragekurve 3"))
     validate(need((input$intercept_supply_3 + input$slope_supply_3*10) > 
                     (input$intercept_demand_3 + input$slope_demand_3*10), 
                   message = "Kein Gleichgewicht für p<10"))
     s_d_3()
   })
   
   dyn_diag_3 <- reactive({
     p_dynamics <- price_func(
       p_0=input$p_0, 
       intercept_demand=input$intercept_demand_3, 
       slope_demand=input$slope_demand_3, 
       intercept_supply=input$intercept_supply_3, 
       slope_supply=input$slope_supply_3, 
       timesteps=t_input
     )
     
     demand_supply_dynamics <- tibble(
       Zeit=t_input,
       Preisdynamik=p_dynamics)
     
     get_dyn_ggplot(demand_supply_dynamics, "Preisdynamik", 
                    eq_price=eq_price_3()) + 
       ggtitle(TeX("Preisdynamik Fall 3"))
   })
   output$dyn_3 <- renderPlot({
     validate(need(input$intercept_supply_3<input$intercept_demand_3, 
                   message = "Angebotskurve 3 beginnt ueber Nachfragekurve 3"))
     validate(need((input$intercept_supply_3 + input$slope_supply_3*10) > 
                     (input$intercept_demand_3 + input$slope_demand_3*10), 
                   message = "Kein Gleichgewicht für p<10"))
     dyn_diag_3()
   })
   
   output$downloadPlot <- downloadHandler(
     filename = function() {
       paste("cobweb.pdf", sep = "")
     },
     content = function(file) {
       if (input$n_plots==1){
         full_plot <- ggarrange(s_d_1(), 
                                dyn_diag_1(), 
                                ncol = 1, nrow = 2,
                                font.label = list(face="bold"))
       } else if (input$n_plots==2){
         full_plot <- ggarrange(s_d_1(), s_d_2(),
                                dyn_diag_1(), dyn_diag_2(),
                                ncol = 2, nrow = 2,
                                labels = paste0(letters[1:4], ")"), 
                                font.label = list(face="bold"))
       } else {
         full_plot <- ggarrange(s_d_1(), s_d_2(), s_d_3(),
                                dyn_diag_1(), dyn_diag_2(), dyn_diag_3(),  
                                ncol = 3, nrow = 2,
                                labels = paste0(letters[1:6], ")"),  
                                font.label = list(face="bold"))
       }
       plot_width <- input$n_plots * 4
       ggsave(file, plot = full_plot, width = plot_width, height = 8)
     }
   )
}


# Run the application 
shinyApp(ui = ui, server = server)
