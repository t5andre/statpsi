library(shiny)
library(ggplot2)


ui <- fluidPage(
    
    # Título
    titlePanel(h1("Curva normal"), windowTitle = "Curva Normal"),
    
    # Painel
    fluidRow(
        column(width = 2,
               radioButtons("calc",
                            "Cálculo",
                            choices = c("Probabilidade abaixo de um escore",
                                        "Probabilidade acima de um escore",
                                        "Probabilidade entre dois valores",
                                        "Limites para um intervalo de confiança"))
        ),
        column(width = 3,
               numericInput("escore",
                            "Escore",
                            0,
                            min = -Inf,
                            max = Inf)      
        ),
        column(width = 3,
               numericInput("media",
                            "Média",
                            0,
                            min = -Inf,
                            max = Inf),
               numericInput("dp",
                            "Desvio-padrão",
                            1,
                            min = -Inf,
                            max = Inf)
        ),
        column(width = 3,
               numericInput("lim_inf",
                            "Limite inferior do intervalo",
                            -1,
                            min = -Inf,
                            max = Inf),
               numericInput("lim_sup",
                            "Limite superior do intervalo",
                            1,
                            min = -Inf,
                            max = Inf)
               
        ),
        column(width = 3,
               numericInput("ic_perc",
                            "Percentual do Intervalo de confiança",
                            95,
                            min = 1,
                            max = 99)
    )
    ),
    mainPanel(
        plotOutput("probDist"),
        textOutput("resultado")
        )
)

server <- function(input, output, session) {
    output$probDist <- renderPlot({

         observeEvent((0 == input$media),{
             updateNumericInput(session, "dp", value = round(abs(input$media)/6,0)) #abs() pra evitar DPs negativos
             updateNumericInput(session, "escore", value = input$media)
             updateNumericInput(session, "lim_inf", value = round(input$media-input$media/6,0))
             updateNumericInput(session, "lim_sup", value = round(input$media+input$media/6,0))
         }, ignoreInit = TRUE)
         
         menor_q <- stat_function(fun = dnorm,
                                  args = list(mean = input$media, sd = input$dp),
                                  xlim = c(-4*input$dp+input$media,input$escore),
                                  geom = "area", fill = "#006495ff", alpha = .3, color = "#006495")
         
         maior_q <- stat_function(fun = dnorm,
                                  args = list(mean = input$media, sd = input$dp),
                                  xlim = c(input$escore,4*input$dp+input$media),
                                  geom = "area", fill = "#006495ff", alpha = .3, color = "#006495")
         
         entre <- stat_function(fun = dnorm,
                                args = list(mean = input$media, sd = input$dp),
                                xlim = c(input$lim_inf,input$lim_sup),
                                geom = "area", fill = "#006495ff", alpha = .3, color = "#006495")
         
         lim_ic <- stat_function(fun = dnorm,
                                 args = list(mean = input$media, sd = input$dp),
                                 xlim = c(input$media-input$dp*qnorm((100-input$ic_perc)/200),input$media-input$dp*qnorm((100+input$ic_perc)/200)),
                                 geom = "area", fill = "#006495ff", alpha = .3, color = "#006495")
        
        ggplot() +
            stat_function(geom = "area", fun = dnorm, args = list(mean = input$media, sd = input$dp),
                          color = "#eb968e", alpha = 0.2, xlim = c(input$media-input$dp*4,input$media+input$dp*4))  +
            {if (input$calc == "Probabilidade abaixo de um escore") {menor_q}
                else if(input$calc == "Probabilidade acima de um escore") {maior_q}
                else if(input$calc == "Probabilidade entre dois valores") {entre}
                else if(input$calc == "Limites para um intervalo de confiança") {lim_ic}
            }+ {if (input$calc %in% c("Probabilidade abaixo de um escore",
                                      "Probabilidade acima de um escore"))
                    {geom_vline(aes(xintercept = input$escore), linetype = "dashed")}} +
            theme_classic() +
            theme(axis.line.y = element_blank(), axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(), axis.title = element_blank()) +
            scale_x_continuous(breaks = seq(input$media-input$dp*3,input$media+input$dp*3,input$dp))
            

    })
    
    output$resultado <- renderText({if (input$calc == "Probabilidade abaixo de um escore") {
        paste0("A probabilidade de um valor abaixo de ",input$escore," é de ",
               round(diff(pnorm(c(-Inf, input$escore),input$media,input$dp)),3)*100,"%")}
        else if(input$calc == "Probabilidade acima de um escore") {
            paste0("A probabilidade de um valor acima de ",input$escore," é de ",
                   round(diff(pnorm(c(input$escore,Inf),input$media,input$dp)),3)*100,"%")}
        else if(input$calc == "Probabilidade entre dois valores") {
            paste0("A probabilidade de um valor entre ",input$lim_inf," e ",input$lim_sup," é ",
                   round(diff(pnorm(c(input$lim_inf, input$lim_sup),input$media,input$dp)),3)*100,"%")}
        else if(input$calc == "Limites para um intervalo de confiança") {
            paste0("Um intervalo de confiança de ",input$ic_perc,"%, para esta média, vai de ",
                   round(qnorm(((100-input$ic_perc)/200), input$media, input$dp),2)," até ",
                   round(qnorm(((100+input$ic_perc)/200), input$media, input$dp),2))}
    })
}

shinyApp(ui = ui, server = server)
