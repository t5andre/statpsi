library(shiny)
library(ggplot2)
ui <- fluidPage(
    
    # Título
    titlePanel(h1("Teorema do limite central"), windowTitle = "Teorema do limite central"),
    
    # Painel
    fluidRow(
        column(width = 2,
            radioButtons("dist",
                         "Formato da distribuição populacional",
                         choices = c("Normal","Assimétrica"))
            ),
        column(width = 3,
            sliderInput("n",
                        "Tamanho amostral",
                        min = 1,
                        max = 20,
                        value = 5,
                        step = 1,
                        ticks = FALSE)
            ),
        column(width = 3,
            sliderInput("qtd",
                        "Número de amostras",
                        min = 3,
                        max = 1000,
                        value = 5,
                        sep = "",
                        ticks = FALSE)
        )
        ),
        mainPanel(
            plotOutput("popDist"), #distribuição populacional
            plotOutput("amostDist") #distribuição das médias amostrais
        )
    )


server <- function(input, output) {
    set.seed(1999)
    output$popDist <- renderPlot({
        ifelse(input$dist == "Assimétrica", #adicionar outras condições
               pop <- sample(sort(rnorm(mean = 150, sd = 30, n = 5000)
                                  )[2000:5000],5000, replace = TRUE), # faz uma distribuição assimétrica
               pop <- rnorm(mean = 200, sd = 30, n = 5000))
        ggplot(data.frame(pop)) + geom_histogram(aes(x = data.frame(pop)[,1]),
                                                 bins = 70,
                                                 colour = "#006495",
                                                 fill = "#eb968e") +
            theme_minimal() + labs(title = "Distribuição populacional",
                                   y = "", x = "Tempo de reação em ms") +
            theme(axis.text.y = element_blank(), panel.grid = element_blank(),
                  text = element_text(size = 18),
                  axis.title.x = element_text(size = 14),
                  axis.text.x = element_text(size = 10)) +
            scale_x_continuous(breaks = c(100,150,200,250,300),
                               limits = c(80,310))+
            scale_y_continuous(limits = c(0,ifelse(input$dist == "Assimétrica", # talvez possa remover, pra ficar mais rápido
                                                   500,
                                                   400)))
    })
    output$amostDist <- renderPlot({
        set.seed(1999)
        ifelse(input$dist == "Assimétrica",
               pop <- sample(sort(rnorm(mean = 150, sd = 30, n = 5000)
                                  )[2000:5000],5000, replace = TRUE),
               pop <- rnorm(mean = 200, sd = 30, n = 5000))
        amost_m <- data.frame(replicate(input$qtd,mean(sample(pop, size = input$n, replace = FALSE))))
        ggplot(amost_m) + geom_histogram(aes(x = amost_m[,1]),
                                         bins = 70,
                                         colour = "#006495",
                                         fill = "#eb968e") +
            geom_vline(aes(xintercept = mean(pop)), linetype = "dashed") +
            geom_label(aes(label = "Média populacional",
                          x= mean(pop), y = ifelse(input$qtd < 16, 10*0.8,
                                                   (input$qtd*0.6)*0.8))) +
            theme_minimal() + labs(title = "Distribuição das médias amostrais",
                                   y = "", x = "Tempo de reação em ms") +
            theme(axis.text.y = element_blank(), panel.grid = element_blank(),
                  text = element_text(size = 18),
                  axis.title.x = element_text(size = 14),
                  axis.text.x = element_text(size = 10)) +
            scale_x_continuous(breaks = c(100,150,200,250,300),
                               limits = c(80,310)) +
            scale_y_continuous(limits = c(0,ifelse(input$qtd < 16, 10, input$qtd*0.6))) # talvez possa remover, pra ficar mais rápido, a ideia era evitar barras muito altas
        
    })
}


shinyApp(ui = ui, server = server)
