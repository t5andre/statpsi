
library(shiny)
library(ggplot2)
library(dplyr)

pop <- rnorm(1000000,0,1)
ic <- data.frame(0,-0.84,0.84,0)
names(ic) <- c("X1","X2","X3","X4")

app_theme <-  theme_minimal() + 
              theme(axis.title.y = element_blank(),
                   panel.background = element_blank(),
                   panel.grid = element_blank(),
                   axis.text.y = element_blank())

calculo_ic95 <- function(x,n){
  c(mean(x) - 1.96 * (sd(x)/sqrt(n)),
    mean(x) + 1.96 * (sd(x)/sqrt(n)))
  }

gerar_ic <- function(n){
    amostra <- sample(x = pop,
                      size = n, 
                      replace = FALSE)
    c(mean(amostra),
      calculo_ic95(amostra,n),
      if_else(calculo_ic95(amostra,n)[1] > mean(pop),
             1,
             if_else(calculo_ic95(amostra,n)[2] < mean(pop),
                    1,
                    0)))
}

multiplas_coletas <- function(k,n){
  data.frame(t(replicate(k,gerar_ic(n))))
}

ui <- fluidPage(

    
    titlePanel("Intervalo de confiança"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                        "Tamanho amostral",
                        min = 5,
                        max = 50,
                        value = 10),
            actionButton("criar",
                         "Coletar uma amostra"),
            actionButton("criar10",
                         "Coletar 10 amostras"),
            actionButton("criar25",
                         "Coletar 25 amostras")
        ),

        
        mainPanel(
            plotOutput("plot_ics"),
            verbatimTextOutput("print")
        )
    )
)


server <- function(input, output) {
    df <- reactiveValues(data = ic)
  
    observeEvent(input$criar,
                 {df$data <- rbind(df$data,gerar_ic(input$n))})
    
    observeEvent(input$criar10,
                 {df$data <- rbind(df$data,multiplas_coletas(10,input$n))})
    
    observeEvent(input$criar25,
                 {df$data <- rbind(df$data,multiplas_coletas(25,input$n))})
    
    output$plot_ics <- renderPlot({
        ggplot(df$data) +
        geom_vline(aes(xintercept = mean(pop)), linetype = "dashed") +
        geom_point(aes(x = X1, y = c(1:length(row.names(df$data))), color = factor(X4))) +
        scale_color_manual(values = c("#bd3022","#006495")) +
        geom_segment(aes(x = X2, xend = X3, y = c(1:length(row.names(df$data))),
                         yend = c(1:length(row.names(df$data))), color = factor(X4))) +
        lims(x = c(-3,3),
             y = c(if_else(length(row.names(df$data)) < 50, 0, length(row.names(df$data)) - 50),
                   length(row.names(df$data)))) +
        app_theme
    })
      output$print <- renderPrint({
        str(df$data)
        df$data$X4 %>% table() %>% prop.table*100})
}


shinyApp(ui = ui, server = server)
