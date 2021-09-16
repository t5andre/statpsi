library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(car)


app_theme <-  theme_minimal() +
    theme(
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none"
    )




ui <- fluidPage(
    titlePanel(h1("Teste T Dependente",
                  align = "center"),
               windowTitle = "Teste T Dependente"),
    sidebarLayout(
        sidebarPanel(
            numericInput(
                "n",
                "Tamanho amostral (número de pares)",
                min = 2,
                max = 500,
                value = 12
            ),
            numericInput(
                "media1",
                "Média na medida 1",
                min = -Inf,
                max = Inf,
                value = 5
            ),
            numericInput(
                "dp1",
                "Desvio-padrão da medida 1",
                min = 2,
                max = 500,
                value = 1
            ),
            numericInput(
                "media2",
                "Média na medida 2",
                min = -Inf,
                max = Inf,
                value = 6
            ),
            numericInput(
                "dp2",
                "Desvio-padrão da medida 2",
                min = -Inf,
                max = Inf,
                value = 1.5
            ),
            hr(),
            strong(a(href = "https://forms.gle/ic35aJ1HuEQdaYCT8",
                     "Nos conte o que você achou dessa visualização!")),
            hr(),       
            "Elaborado para a disciplina de Estatística Aplicada
            à Psicologia da Universidade Federal do Rio Grande do Sul.",
            br(),
            a(href = "https://github.com/t5andre/statpsi/tree/main/teste_t_dep",
              "Código")
        ),
        mainPanel(
            h4(
                "Na tabela abaixo temos os valores de um Teste T dependente
                comparando essas duas medidas em uma amostra aleatória."
            ),
            tableOutput("resultados"),
        h4(
            "No gráfico abaixo, cada ponto representa uma observação,
            e os pontos na medida estão conectados aos pontos na medida pois
            eles foram gerados pela mesma pessoa, equipe ou organização."
        ),
        plotOutput("plot_dists"),
        
        h5(
            "Note como algumas trajetórias são opostas ao observado no conjunto.
            Por exemplo, mesmo que a média na medida 2 seja maior que a média
            na medida 1, algumas observações diminuem, e vice-versa."
        ),
        h5("O sombreado colorido representa a distribuição dessas observações,
           de forma similar ao boxplot. Lembrando que o boxplot tem um traço no
           meio representando a mediana, e a caixa é limitada pelo primeiro e
           terceiro quartis.")
    )
    )
)

server <- function(input, output) {
    df <- reactive(data.frame(
        medias = c(input$media1+input$dp1*scale(
            rnorm(
            n = input$n
        )
        ),
        input$media2+input$dp2*scale(
            rnorm(
            n = input$n
        )
        )
        ),
        grupos = factor(
            c(
                rep(
                    1,
                    input$n
                ),
                rep(
                    2,
                    input$n)
            )
        )
        )
    )

    
    
    output$plot_normal <- renderPlot({
        ggplot() + 
            stat_function(geom = "area",
                          fun = dnorm,
                          args = list(mean = input$media1,
                                      sd = input$dp1),
                          fill = "#006495",
                          colour = "white",
                          alpha = 0.3,
                          xlim = c(input$media1-5*input$dp1,
                                   input$media1+5*input$dp1)
            ) +
            stat_function(geom = "area",
                          fun = dnorm,
                          args = list(mean = input$media2,
                                      sd = input$dp2),
                          fill = "#bd3022",
                          colour = "white",
                          alpha = 0.3,
                          xlim = c(input$media2-5*input$dp2,
                                   input$media2+5*input$dp2)
            ) +
            geom_vline(aes(xintercept = input$media1),
                       colour = "#006495",
                       alpha = 0.7, 
                       linetype = "dashed") +
            geom_vline(aes(xintercept = input$media2),
                       colour = "#bd3022",
                       alpha = 0.7, 
                       linetype = "dashed") +
            app_theme
        
    })
    
    output$plot_dists <- renderPlot({
            ggplot() +
            geom_boxplot(aes(x = df()$medias,
                             y = df()$grupos,
                             fill = df()$grupos),
                         width = 0.2,
                         alpha = 0.1) +
            geom_violin(aes(x = df()$medias,
                            y = df()$grupos,
                            fill = df()$grupos),
                        alpha = 0.3,
                        trim = FALSE,
                        colour = "white") +
            geom_point(aes(x = df()$medias,
                           y = df()$grupos,
                           colour = df()$grupos),
                       alpha = 0.6) +
            annotate(geom = "text", y = 1.5,
                     x = Inf, label = paste0("d = ",
                                             round(abs(
                                                 mean(
                                                     df()[df()$grupos == 1,1]) -
                                                     mean(
                                                         df()[df()$grupos == 2,1])
                                             )/sd(df()[df()$grupos == 1,1] -
                                                  df()[df()$grupos == 2,1]),
                                             2)
                     ),vjust = 3, size = 5)+
            coord_flip() +
            geom_segment(aes(x = df()[df()$grupos == 1,1], xend = df()[df()$grupos == 2,1],
                             y = df()[df()$grupos == 1,2], yend = df()[df()$grupos == 2,2]),
                         linetype = "dashed",
                         alpha = 0.4) +
            scale_fill_manual(values = c("#006495","#bd3022")) +
            scale_colour_manual(values = c("#006495","#bd3022")) +
            labs(x = "Escores", y = "Medidas") +
            app_theme
    })
    
    output$resultados <- renderTable({
        
        teste_t_dep <- t.test(medias ~ grupos, df(), paired = TRUE)
        
        data.frame("Dif_Médias" = teste_t_dep$estimate[[1]],
                   "Estatística_teste" = teste_t_dep$statistic,
                   "Graus_de_Liberdade" = teste_t_dep$parameter,
                   "p" = ifelse(
                       teste_t_dep$p.value < .001,
                       "< 0.001",
                       round(teste_t_dep$p.value,3)))
    }, digits = 3)
    
}


shinyApp(ui = ui, server = server)

