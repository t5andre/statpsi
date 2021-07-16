library(shiny)
library(ggplot2)
ui <- fluidPage(
    
    # Título
    titlePanel(h1("Teorema do limite central"),
               windowTitle = "Teorema do limite central"),
    
    # Painel
    fluidRow(
        column(width = 2,
            radioButtons("dist",
                         "Formato da distribuição populacional",
                         choices = c("Normal","Assimétrica positiva",
                                     "Assimétrica negativa"))
            ),
        column(width = 3,
            sliderInput("n",
                        "Tamanho amostral",
                        min = 1,
                        max = 20,
                        value = 5,
                        step = 1,
                        ticks = FALSE),
            "Quantidade de observações coletadas em cada amostra."
        ),
        column(width = 3,
               sliderInput("qtd",
                           "Número de amostras",
                           min = 3,
                           max = 1000,
                           value = 5,
                           sep = "",
                           ticks = FALSE),
               "Quantidade total de amostras coletadas.")
        ),
        mainPanel(
            plotOutput("popDist"), #distribuição populacional
            plotOutput("amostDist"), #distribuição das médias amostrais
            h4(textOutput("resumo"),style="color:black")
        )
    )


server <- function(input, output) {
    set.seed(19)
    output$popDist <- renderPlot({
        {if(input$dist == "Assimétrica positiva") #adicionar outras condições
               {pop <- sample(sort(rnorm(mean = 150, sd = 30, n = 5000)
                                  )[2000:5000],5000, replace = TRUE)}  
            else if(input$dist == "Assimétrica negativa") # faz uma distribuição assimétrica positiva
            {pop <- sample(sort(rnorm(mean = 250, sd = 30, n = 5000),decreasing = TRUE
            )[2000:5000],5000, replace = TRUE)} # faz uma distribuição assimétrica negativa
            else if(input$dist == "Normal")
            {pop <- rnorm(mean = 200, sd = 30, n = 5000)}
        }
        
    ggplot(data.frame(pop)) +
    geom_histogram(aes(x = data.frame(pop)[,1]),
                     bins = 70,
                     colour = "#006495",
                     fill = "#eb968e") +
        theme_minimal() +
        labs(title = "Distribuição populacional (N = 5000)",
                                   y = "", x = "Tempo de reação em ms") +
            theme(axis.text.y = element_blank(),
                  panel.grid = element_blank(),
                  text = element_text(size = 18),
                  axis.title.x = element_text(size = 14),
                  axis.text.x = element_text(size = 10)) +
            scale_x_continuous(breaks = c(100,150,200,250,300),
                               limits = c(80,310))+
            scale_y_continuous(limits = c(0,500)) #simplificado
    })
    output$amostDist <- renderPlot({
        set.seed(19)
        
        {if(input$dist == "Assimétrica positiva") #adicionar outras condições
             {pop <- sample(sort(rnorm(mean = 150, sd = 30, n = 5000)
                 )[2000:5000],5000, replace = TRUE)}  
         else if(input$dist == "Assimétrica negativa") # distribuição assimétrica positiva
             {pop <- sample(sort(rnorm(mean = 250, sd = 30, n = 5000),decreasing = TRUE
                 )[2000:5000],5000, replace = TRUE)} # faz uma distribuição assimétrica negativa
         else if(input$dist == "Normal")
             {pop <- rnorm(mean = 200, sd = 30, n = 5000)}
        }
        
        amost_m <- data.frame(replicate(input$qtd,
                                        mean(sample(pop,
                                                    size = input$n,
                                                    replace = FALSE))))
        
        ggplot(amost_m) + geom_histogram(aes(x = amost_m[,1]),
                                         bins = 70,
                                         colour = "#006495",
                                         fill = "#eb968e") +
            geom_vline(aes(xintercept = mean(pop)), linetype = "dashed") +
            geom_label(aes(label = "Média populacional",
                          x= mean(pop), y = ifelse(input$qtd < 16, 10*0.8,
                                                   (input$qtd*0.6)*0.8))) +
            theme_minimal() + labs(
                title = paste0("Distribuição das médias amostrais (n = ",
                               input$qtd,")"),
                                   y = "", x = "Tempo de reação em ms") +
            theme(axis.text.y = element_blank(),
                  panel.grid = element_blank(),
                  text = element_text(size = 18),
                  axis.title.x = element_text(size = 14),
                  axis.text.x = element_text(size = 10)) +
            scale_x_continuous(breaks = c(100,150,200,250,300),
                               limits = c(80,310)) +
            scale_y_continuous(limits = c(0,ifelse(input$qtd < 16, 10,
                                                   input$qtd*0.6)))
    })
    output$resumo <- renderText({
        paste("Distribuição das médias de",input$qtd,"amostras com",
              input$n,"observações cada.")})
}


shinyApp(ui = ui, server = server)
