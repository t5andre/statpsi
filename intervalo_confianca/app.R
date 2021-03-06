library(shiny)
library(ggplot2)
library(dplyr)
pop <- rnorm(10000, 0, 1)
ic <- data.frame(0, -.84, .84, 0)
names(ic) <- c("X1", "X2", "X3", "X4")

app_theme <-  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none"
  )

#cálculo do intervalo de confiança
#x o vetor da amostra e n o tamanho
calculo_ic95 <- function(x, n) {
  c(mean(x) - 1.96 * (sd(x) / sqrt(n)),
    mean(x) + 1.96 * (sd(x) / sqrt(n)))
}
# processo que coleta uma amostra da população com tamanho n
# e calcula a média e limites superior e inferior do IC95
# gera uma var binária para definir se a média pop está contida
# no intervalo
gerar_ic <- function(n) {
  amostra <- sample(x = pop,
                    size = n,
                    replace = FALSE)
  c(
    mean(amostra),
    calculo_ic95(amostra, n),
    if_else(
      calculo_ic95(amostra, n)[1] > mean(pop),
      1,
      if_else(calculo_ic95(amostra, n)[2] < mean(pop),
              1,
              0)
    )
  )
}

#função para múltiplas ocorrências da função anterior
multiplas_coletas <- function(k, n) {
  data.frame(t(replicate(k, gerar_ic(n))))
}

ui <- fluidPage(titlePanel("Intervalo de confiança"),
                
                # Sidebar with a slider input for number of bins
                sidebarLayout(
                  sidebarPanel(
                    sliderInput(
                      "n",
                      "Tamanho amostral",
                      min = 5,
                      max = 50,
                      value = 10
                    ),
                    "Cada vez que você clicar em um dos botões abaixos,
                       uma ou mais amostras serão coletadas e o intervalo de confiança
                       de 95% para a estimativa da média populacional será adicionado
                       ao gráfico à direita.",
                    br(),
                    "Os pontos indicam a média amostral e as
                       linhas indicam a extensão do intervalo de confiança.",
                    hr(),
                    actionButton("criar",
                                 "Coletar uma amostra"),
                    br(),
                    actionButton("criar50",
                                 "Coletar 50 amostras"),
                    br(),
                    actionButton("criar100",
                                 "Coletar 100 amostras"),
                    hr(),
                    "Em azul destacamos os intervalos que contém a média populacional (linha tracejada)",
                    br(),
                    "Em vermelho destacamos aqueles que não contém a média populacional.",
                    br(),
                    "Como o processo é aleatório, em dados momentos menos de 95% dos.
                       intervalos conterão a média populacional, e em alguns momentos o
                       percentual será maior.",
                    hr(),
                    actionButton("limpar",
                                 "Excluir todas as coletas")
                    
                    
                  ),
                  mainPanel(
                    textOutput("amostras"),
                    br(),
                    textOutput("percentual"),
                    hr(),
                    plotOutput("plot_ics")
                    )
                            
                ))


server <- function(input, output) {
  df <- reactiveValues(data = ic)
  
  
  observeEvent(input$criar,
               {
                 df$data <- rbind(df$data, gerar_ic(input$n))
               })
  
  observeEvent(input$criar50,
               {
                 df$data <- rbind(df$data, multiplas_coletas(50, input$n))
               })
  
  observeEvent(input$criar100,
               {
                 df$data <- rbind(df$data, multiplas_coletas(100, input$n))
               })
  
  observeEvent(input$limpar,
               {
                 df$data <- ic
               })
  
  output$plot_ics <- renderPlot({
    ggplot(df$data) +
      geom_vline(aes(xintercept = mean(pop)), linetype = "dashed") +
      geom_point(aes(
        x = X1,
        y = c(1:length(row.names(df$data))),
        color = factor(X4) # se media pop está dentro ou fora do intervalo
      )) +
      scale_color_manual(values = c("#006495","#bd3022")) +
      geom_segment(aes(
        x = X2,
        xend = X3,
        y = c(1:length(row.names(df$data))),
        yend = c(1:length(row.names(df$data))),
        color = factor(X4)
      )) +
      labs(x = "Média") +
      lims(x = c(-3, 3),
           y = c(if_else(
             length(row.names(df$data)) < 50, 0, length(row.names(df$data)) - 75
           ),
           length(row.names(df$data)))) +
      app_theme
  })
  
  output$percentual <- renderText({
    paste0(round(mean(abs(df$data$X4-1))*100), "% dos intervalos de confiança abrangem a média populacional.")
    })
  
  output$amostras <- renderText({
    paste0(length(df$data$X1), " amostras foram coletadas.")
    })
}


shinyApp(ui = ui, server = server)
