library(shiny)
library(ggplot2)
library(dplyr)
library(car)


app_theme <-  theme_minimal() +
    theme(
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none"
    )




ui <- fluidPage(
    titlePanel(h1(
        "Teste T independente",
               align = "center"),
        windowTitle = "Teste T Independente"
        ),
    fluidRow(
        column(
            3,
            numericInput(
                "n1",
                "Tamanho amostral do grupo 1",
                min = 2,
                max = 500,
                value = 8
            ),
            numericInput(
                "media1",
                "Média do grupo 1",
                min = -Inf,
                max = Inf,
                value = 4
            ),
            numericInput(
                "dp1",
                "Desvio-padrão do grupo 1",
                min = 2,
                max = 500,
                value = 0.8
            )
        ),
        column(
            3,
            numericInput(
                "n2",
                "Tamanho amostral do grupo 2",
                min = 2,
                max = 500,
                value = 10
            ),
            numericInput(
                "media2",
                "Média do grupo 2",
                min = -Inf,
                max = Inf,
                value = 3
            ),
            numericInput(
                "dp2",
                "Desvio-padrão do grupo 2",
                min = -Inf,
                max = Inf,
                value = 0.5
            ),
        )
    ),
    fluidRow(
        column(
        8,
            
            
            h4(
                "Na tabela abaixo temos os valores de um Teste T independente comparando essas amostras geradas aleatoriamente."
            ),
            tableOutput("resultados_ind"),
            h4(
                "Perceba que quando o teste de Levene tem p < 0,05, o resultado do teste T muda em função das variâncias não serem homogêneas."
            ),
            h4(
                "Podemos perceber isso pelos graus de liberdade, que deixam de ser um número inteiro."
            ),
            h4(
                "Para criar diferença entre as variâncias, experimente aumentar o desvio-padrão de um dos grupos."
            )
        )
    ),
    fluidRow(
        column(6,
               h5(
                   "No gráfico abaixo temos duas distribuições normais
                           padronizadas a partir da média e desvio-padrão estabelecidos."
               ),
                plotOutput("plot_normal")),
        column(6,
               h5(
                   "No gráfico abaixo as distribuições são geradas aleatoriamente cada
                           vez que um dos valores muda, e seus formatos dependem também do tamanho amostral."
               ),
               plotOutput("plot_dists"),
               
               br()),
    fluidRow(
        column(
            3,     
            "Elaborado para a disciplina de Estatística Aplicada
            à Psicologia da Universidade Federal do Rio Grande do Sul."
        ),
        column(3,
               strong(a(href = "https://forms.gle/3jgD3er4JCQiup3B9",
                        "Nos conte o que você achou dessa visualização!"))
               ),
        column(3,
            a(href = "https://github.com/t5andre/statpsi/tree/main/teste_t_ind",
              "Código"))
    )
    )
    
)

server <- function(input, output) {
    df <- reactive(data.frame(
        medias = c(rnorm(
            n = input$n1,
            mean = input$media1,
            sd = input$dp1
            ),
          rnorm(
              n = input$n2,
              mean = input$media2,
              sd = input$dp2
              )
          ),
        grupos = factor(
              c(
              rep(
                  1,
                  input$n1
                  ),
              rep(
                  2,
                  input$n2)
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
            labs(x = "Escores", y = "") +
            app_theme
            
        })
    
    output$plot_dists <- renderPlot({
        ggplot(df()) +
            geom_density(aes(x = df()[,1],
                             fill = df()[,2]),
                         alpha = 0.3,
                         colour = "white") +
            geom_vline(aes(xintercept = mean(df()[df()[,2] == 1,1])),
                       colour = "#006495", alpha = 0.7, linetype = "dashed") +
            geom_vline(aes(xintercept = mean(df()[df()[,2] == 2,1])),
                       colour = "#bd3022", alpha = 0.7, linetype = "dashed") +
            
            geom_segment(aes(x = ifelse(mean(df()[df()[,2] == 1,1]) < mean(df()[df()[,2] == 2,1]),
                                        mean(df()[df()[,2] == 1,1]),
                                        mean(df()[df()[,2] == 2,1])),
                             
                             xend = ifelse(mean(df()[df()[,2] == 1,1]) > mean(df()[df()[,2] == 2,1]),
                                        mean(df()[df()[,2] == 1,1]),
                                        mean(df()[df()[,2] == 2,1])),
                             y = Inf,
                             yend = Inf),
                         linetype = 5,
                         size = 1.1) +
            geom_text(aes(x = (mean(df()[df()[,2] == 1,1]) + mean(df()[df()[,2] == 2,1]))/2,
                          y = Inf,
                          label = paste0("d = ",
                                             round(abs(
                                                 mean(
                                                     df()[df()[,2] == 1,1]) -
                                                     mean(
                                                         df()[df()[,2] == 2,1])
                                                 )/sqrt((sd(
                                                     df()[df()[,2] == 1,1])^2 +
                                                        sd(
                                                            df()[df()[,2] == 2,1])^2)/2),
                                             2)
                                            )
                                         ),
                      vjust = 2) +
            scale_fill_manual(values = c("#006495","#bd3022")) +
            labs(x = "Escores", y = "") +
            xlim(c(ifelse(mean(df()[df()[,2] == 1,1]) <= mean(df()[df()[,2] == 2,1]),
                        mean(df()[df()[,2] == 1,1])-(4*sd(df()[df()[,2] == 1,1])),
                        mean(df()[df()[,2] == 2,1])-(4*sd(df()[df()[,2] == 2,1]))),
                   ifelse(mean(df()[df()[,2] == 1,1]) >= mean(df()[df()[,2] == 2,1]),
                          mean(df()[df()[,2] == 1,1])+(4*sd(df()[df()[,2] == 1,1])),
                          mean(df()[df()[,2] == 2,1])+(4*sd(df()[df()[,2] == 2,1]))))) +
            app_theme
    })
    
    output$resultados_ind <- renderTable({
        
        levtest <- leveneTest(medias ~ grupos, df())
        teste_t_ind <- t.test(medias ~ grupos, df(), var.equal = levtest[[3]][1] > 0.05)
        
        data.frame("Teste" = c("Levene","T independente"),
                   "Dif_Médias" = 
                       c(NA, teste_t_ind$estimate[[1]]-teste_t_ind$estimate[[2]]),
                   "Tamanho_efeito_d_cohen" = 
                       c(NA, (teste_t_ind$estimate[[1]]-teste_t_ind$estimate[[2]])/
                             sqrt((sd(df()[df()[,2] == 1,1])^2 +
                               sd(df()[df()[,2] == 2,1])^2)/2)),
                   "Estatística_teste" = 
                       c(levtest$`F value`[[1]],teste_t_ind$statistic),
                   "Graus_de_Liberdade" = 
                       c(levtest$Df[[1]],teste_t_ind$parameter),
                   "p" = 
                       c(levtest$`Pr(>F)`[[1]],teste_t_ind$p.value))
    }, digits = 4, na = "")
    
}


shinyApp(ui = ui, server = server)

