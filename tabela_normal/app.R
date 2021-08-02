library(shiny)
library(ggplot2)
options(OutDec = ",")

ui <- fluidPage(
    
    # Título
    titlePanel(h1("Curva normal", align = "center"),
               windowTitle = "Curva Normal"),
    
    # Painel
    sidebarLayout(
        sidebarPanel(
               radioButtons("calc",
                            "Cálculo",
                            choices = c("Probabilidade abaixo de um escore",
                                        "Probabilidade acima de um escore",
                                        "Probabilidade entre dois valores",
                                        "Limites para um intervalo de confiança")),
               numericInput("media",
                            "Média",
                            0,
                            min = -Inf,
                            max = Inf),
               numericInput("dp",
                            "Desvio-padrão",
                            1,
                            min = 0,
                            max = Inf,
                            step = 0.01),
               conditionalPanel("input.calc == 'Probabilidade abaixo de um escore' || input.calc == 'Probabilidade acima de um escore' ",
                                numericInput("escore",
                                             "Escore",
                                             0,
                                             min = -Inf,
                                             max = Inf)),
               conditionalPanel("input.calc == 'Probabilidade entre dois valores'",
               numericInput("lim_inf",
                            "Limite inferior do intervalo",
                            -1,
                            min = -Inf,
                            max = Inf),
               numericInput("lim_sup",
                            "Limite superior do intervalo",
                            1,
                            min = -Inf,
                            max = Inf)),
               conditionalPanel("input.calc == 'Limites para um intervalo de confiança'",
               numericInput("ic_perc",
                            "Percentual do Intervalo de confiança",
                            95,
                            min = 1,
                            max = 99)),
               hr(),
               strong(a(href = "https://forms.gle/1CbWRrAeHbUkQ5bY6",
                        "Nos conte o que você achou dessa visualização!")),
               hr(),       
               "Elaborado para a disciplina de Estatística Aplicada
            à Psicologia da Universidade Federal do Rio Grande do Sul.",
               br(),
               a(href = "https://github.com/t5andre/statpsi/tree/main/tabela_normal",
                 "Código")
               
               ),
    mainPanel(
        h4(textOutput("resultado")),
        plotOutput("probDist")
        )
    )
)

server <- function(input, output, session) {
    #não estou satisfeito com essa solução pra atualizar valores com base
    #em alterações da média
    observeEvent((input$media),{
        #abs() pra evitar DPs negativos
        if (input$media < 4){
        updateNumericInput(session, "dp", value = 1)
        updateNumericInput(session, "escore", value = integer(input$media))
        updateNumericInput(session, "lim_inf", value = input$media-2)
        updateNumericInput(session, "lim_sup", value = input$media+2)}
       
         else{
            updateNumericInput(session, "dp", value = round(abs(input$media)/6,0),min = 1)
            updateNumericInput(session, "escore", value = integer(input$media) )
            updateNumericInput(session, "lim_inf", value = round(input$media-(input$media/6),0))
            updateNumericInput(session, "lim_sup", value = round(input$media+(input$media/6),0))
            }
            
    }, ignoreInit = TRUE)
    
    output$probDist <- renderPlot({

         
        
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
                                 xlim = c(input$media-input$dp*qnorm((100-input$ic_perc)/200),
                                          input$media-input$dp*qnorm((100+input$ic_perc)/200)),
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
                                     {geom_vline(aes(xintercept = input$escore), linetype = "dashed")}
                
                else if(input$calc == "Probabilidade entre dois valores")
                {geom_vline(aes(xintercept = c(input$lim_inf,input$lim_sup)),
                                linetype = "dashed")}
                
                else if(input$calc == "Limites para um intervalo de confiança")
                {geom_vline(aes(xintercept = c(input$media-input$dp*qnorm((100-input$ic_perc)/200),
                                               input$media-input$dp*qnorm((100+input$ic_perc)/200))),
                            linetype = "dashed")}
                } +
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
            paste0("A probabilidade de um valor entre ",input$lim_inf," e ",input$lim_sup," é de ",
                   round(diff(pnorm(c(input$lim_inf, input$lim_sup),input$media,input$dp)),3)*100,"%")}
        
        else if(input$calc == "Limites para um intervalo de confiança") {
            paste0("Um intervalo de confiança de ",input$ic_perc,"%, para esta média, vai de ",
                   round(qnorm(((100-input$ic_perc)/200), input$media, input$dp),2)," até ",
                   round(qnorm(((100+input$ic_perc)/200), input$media, input$dp),2))}
    })
}

shinyApp(ui = ui, server = server)
