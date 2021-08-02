library(shiny)
library(ggplot2)

app_theme <- theme(axis.text.y = element_blank(),
                   panel.grid = element_blank(),
                   text = element_text(size = 18),
                   axis.title.x = element_text(size = 14),
                   axis.text.x = element_text(size = 10))
                   
ui <- fluidPage(
    
    # Título
    titlePanel(h1("Teorema do limite central",align = "center"),
               windowTitle = "Teorema do limite central"),
    
    # Painel
    sidebarLayout(
        sidebarPanel(
            radioButtons("dist",
                         "Formato da distribuição populacional",
                         choices = c("Normal","Assimétrica positiva",
                                     "Assimétrica negativa")),
            
            sliderInput("n",
                        "Tamanho amostral",
                        min = 1,
                        max = 20,
                        value = 5,
                        step = 1,
                        ticks = FALSE),
            "Quantidade de observações coletadas em cada amostra.
            Quanto maior o tamanho de cada amostra, menor é a dispersão
            da distribuição de médias amostrais.",
        
               sliderInput("qtd",
                           "Número de amostras",
                           min = 3,
                           max = 1000,
                           value = 5,
                           sep = "",
                           ticks = FALSE),
            "Quantidade total de amostras coletadas.
            Quanto mais amostras são coletadas, mais a distribuição dessas
            médias se aproxima de uma distribuição normal.",
            
            hr(),
            strong(a(href = "https://forms.gle/6suyDNpBvCsRSYKT7",
                         "Nos conte o que você achou dessa visualização!")),
            hr(),       
            "Elaborado para a disciplina de Estatística Aplicada
            à Psicologia da Universidade Federal do Rio Grande do Sul.",
            br(),
            a(href = "https://github.com/t5andre/statpsi/tree/main/teorema_limite_central",
              "Código")
                   
            ),
    
        mainPanel(align = "center",
            plotOutput("popDist"), #distribuição populacional
            plotOutput("amostDist"), #distribuição das médias amostrais
            h4(textOutput("resumo"),style="color:black")
            )
    ),
)



server <- function(input, output) {
    pop <- reactive({
        # pra manter a população sempre igual
                    set.seed(19) 
        #adicionar outras condições
                    {if(input$dist == "Assimétrica positiva") 
                # faz uma distribuição assimétrica positiva
                        {pop <- sample(sort(
                            rnorm(mean = 150,
                                  sd = 30,
                                  n = 5000)
                            )[2000:5000],
                            5000,
                            replace = TRUE)}  
                    else if(input$dist == "Assimétrica negativa")
                # faz uma distribuição assimétrica negativa
                        {pop <- sample(sort(
                            rnorm(mean = 250,
                                  sd = 30,
                                  n = 5000),
                            decreasing = TRUE
                            )[2000:5000],
                            5000,
                            replace = TRUE)} 
                    else if(input$dist == "Normal")
                # faz uma distribuição normal
                    {pop <- rnorm(mean = 200,
                                  sd = 30,
                                  n = 5000)}
        }
    })
    
    output$popDist <- renderPlot({
        
    ggplot(data.frame(pop())) +
    geom_histogram(aes(x = data.frame(pop())[,1]),
                     bins = 70,
                     colour = "#006495",
                     fill = "#eb968e") +
        theme_minimal() +
            app_theme +
        labs(title = "Distribuição populacional (N = 5000)",
             y = "", x = "Tempo de reação em ms") +
            scale_x_continuous(breaks = c(100,150,200,250,300),
                               limits = c(80,310))+
            scale_y_continuous(limits = c(0,500)) #simplificado
    })
    output$amostDist <- renderPlot({
        amost_m <- data.frame(replicate(input$qtd,
                                        mean(sample(pop(),
                                                    size = input$n,
                                                    replace = FALSE))))
        
        ggplot(amost_m) + geom_histogram(aes(x = amost_m[,1]),
                                         bins = 70,
                                         colour = "#006495",
                                         fill = "#eb968e") +
            # as linhas vem antes pra ficar atrás das labels
            geom_vline(aes(xintercept = mean(pop())), linetype = "dashed",
                       color = "#006495") + 
            
            geom_vline(aes(xintercept = mean(amost_m[,1])), linetype = "dashed",
                       color = "#eb968e") +
            
            geom_label(aes(label = paste0("Média populacional: ",
                                          round(mean(pop()),2)),
                          x= mean(pop()), y = ifelse(input$qtd < 16, 10*0.8,
                                                   (input$qtd*0.6)*0.8)),
                      nudge_x = 3, hjust = "left", fill = "#0d3a50",
                      color = "white", fontface = "bold", size = 4) +
            geom_label(aes(label = paste0("Média das amostras: ",
                                          round(mean(amost_m[,1]),2)),
                           x= mean(amost_m[,1]), y = ifelse(input$qtd < 16, 10*0.6,
                                                    (input$qtd*0.6)*0.6)),
                      nudge_x = 3, hjust = "left", fill = "#6d261f",
                      color = "white", fontface = "bold", size = 4) +
            theme_minimal() + 
            app_theme +
            labs(title = paste0("Distribuição das médias amostrais (n = ",
                               input$qtd,")"),
                 y = "", x = "Tempo de reação em ms") +
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
