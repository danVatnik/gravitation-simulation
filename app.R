source('vect.r')
source('body.r')
library(shiny)

earthSunPreset <- list(
            body1 = list(mass = "5.972E24", position = c("0","-1.5192E11"), velocity = c("3E4","0")),
            body2 = list(mass = "1.989E30", position = c("0","0"), velocity = c("0","0"))
            )
binaryStarPreset <- list(
    body1 = list(mass = "1.989E30", position = c("0","-1.5192E11"), velocity = c("1E4","0")),
    body2 = list(mass = "1.989E30", position = c("0","1.5192E11"), velocity = c("-1E4","0"))
    )
gravityAssistPreset <- list(
    body1 = list(mass = "5.972E22", position = c("-1.5192E11","-1.5192E11"), velocity = c("4E4","5E4")),
    body2 = list(mass = "1.989E30", position = c("0","0"), velocity = c("2E4","0"))
    )

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Gravitation Simulation"),
    hr(),
    fluidRow(
        column(4,
            wellPanel(
                h4(paste("Body 1")),
                numericInput("mass1", "Mass", "5.972E24", min = 0),
                h4("Position"),
                fluidRow(
                    column(6,
                           numericInput("posX1", "X", 0)
                    ),
                    column(6,
                           numericInput("posY1", "Y", "-1.5192E11")
                    )
                ),
                h4("Velocity"),
                fluidRow(
                    column(6,
                           numericInput("velX1", "X", "3E4")
                    ),
                    column(6,
                           numericInput("velY1", "Y", 0)
                    )
                ),
                hr(),
                h4(paste("Body 2")),
                numericInput("mass2", "Mass", "1.989E30", min = 0),
                h4("Position"),
                fluidRow(
                    column(6,
                           numericInput("posX2", "X", 0)
                    ),
                    column(6,
                           numericInput("posY2", "Y", 0)
                    )
                ),
                h4("Velocity"),
                fluidRow(
                    column(6,
                           numericInput("velX2", "X", 0)
                    ),
                    column(6,
                           numericInput("velY2", "Y", 0)
                    )
                )
            )
        ),
        column(8,
                fluidRow(
                    column(1),
                    column(10,
                        plotOutput('trajectoryPlot'),
                    ),
                    column(1)
                ),
               column(7,
                wellPanel(
                    h4("Presets"),
                    radioButtons('preset', "", choices = c("Earth-Sun" = "earthSun",
                                                            "Binary Star" = "binaryStar",
                                                            "Gravity Assist" = "gravityAssist"),
                                 selected = "earthSun",
                                 inline = FALSE, width = NULL, choiceNames = NULL,
                                 choiceValues = NULL)
                )
                      ),
               column(5,
                wellPanel(
                    fluidRow(
                        column(3,),
                        column(3,actionButton("play","Play")),
                        column(3,actionButton("stop","Stop")),
                        column(3,),
                        column(12,hr()),
                        column(12,numericInput("timeStep","TimeStep (s)", "1.64E5")),
                        column(12,numericInput("gravConst","Grav Const", "6.674E-11"))
                    )
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    reactiveState <- reactiveValues()
    reactiveState$body1 <- NULL
    reactiveState$body2 <- NULL
    reactiveState$b1Pos <- NULL
    reactiveState$b2Pos <- NULL
    reactiveState$b1Trail <- data.frame()
    reactiveState$b2Trail <- data.frame()
    reactiveState$counter <- 0
    
    checkReqs <- function(){
        req(input$mass1)
        req(input$mass2)
        req(input$posX1)
        req(input$posY1)
        req(input$posX2)
        req(input$posY2)
        req(input$velX1)
        req(input$velY1)
        req(input$velX2)
        req(input$velY2)
        req(input$timeStep)
    }
    
    gravAcc <- function(massOtherObject, distance){

        
        acc = input$gravConst * massOtherObject / distance^2
        
        return(acc)
    }
    
    setAcceleration <- function(){
        attractionVect = vect(reactiveState$body1$position, reactiveState$body2$position)
        
        gravAccNormB1 <- gravAcc(reactiveState$body2$mass, attractionVect$len)
        gravAccB1 <- attractionVect$unit * gravAccNormB1
        
        reactiveState$body1$acceleration <<- vect(gravAccB1)
        
        attractionVect = vect(reactiveState$body2$position, reactiveState$body1$position)
        
        gravAccNormB2 <- gravAcc(reactiveState$body1$mass, attractionVect$len)
        gravAccB2 <- attractionVect$unit * gravAccNormB2
        
        reactiveState$body2$acceleration <<- vect(gravAccB2)
    }
    
    setVelocity <- function(){
        reactiveState$body1$velocity <<- reactiveState$body1$velocity %v+% (reactiveState$body1$acceleration %v*% input$timeStep)
        reactiveState$body2$velocity <<- reactiveState$body2$velocity %v+% (reactiveState$body2$acceleration %v*% input$timeStep)
    }
    
    setPosition <- function(){
        reactiveState$body1$position <<- reactiveState$body1$position + (reactiveState$body1$velocity %v*% input$timeStep)$comp
        reactiveState$body2$position <<- reactiveState$body2$position + (reactiveState$body2$velocity %v*% input$timeStep)$comp
    }
    
    setTrails <- function(){
        reactiveState$b1Trail <- rbind(tail(reactiveState$b1Trail, 10), reactiveState$b1Pos)
        reactiveState$b2Trail <- rbind(tail(reactiveState$b2Trail, 10), reactiveState$b2Pos)

    }
    
    initializeBodies <- function(){
        checkReqs()
        reactiveState$body1 <- body(input$mass1, c(input$posX1, input$posY1), c(input$velX1, input$velY1), c(0,0))
        reactiveState$body2 <- body(input$mass2, c(input$posX2, input$posY2), c(input$velX2, input$velY2), c(0,0))
        reactiveState$b1Pos <- c(input$posX1, input$posY1)
        reactiveState$b2Pos <- c(input$posX2, input$posY2)
        
        setTrails()
    }

    forward <- function(){
        reactiveState$counter <- reactiveState$counter + 1
        
        checkReqs()
        
        setAcceleration()
        
        setVelocity()
        
        setPosition()
        
        reactiveState$b1Pos <- reactiveState$body1$position
        reactiveState$b2Pos <- reactiveState$body2$position

        if(reactiveState$counter %% 5 == 0){
            setTrails()
        }
    }
    
    getPlotLims <- function(b1pos = NULL, b2pos = NULL){
        if(!is.null(b1pos) & !is.null(b2pos)){
            dist = sqrt((b1pos[1] - b2pos[1])^2 + (b1pos[2] - b2pos[2])^2)
        }
        else{
            dist = sqrt((input$posX1 - input$posX2)^2 + (input$posY1 - input$posY2)^2)
        }
        
        limsDir <- c(-dist, dist) * 1.15
        lims = list(xLim = limsDir, yLim = limsDir)
        
        return(lims)
    }
    
    renderTrajPlot <- function(b1Pos = NULL, b2Pos = NULL){
        plotLims <- getPlotLims(b1Pos, b2Pos)
        plot(reactiveState$b2Pos[1], reactiveState$b2Pos[2], xlim = plotLims$xLim, ylim = plotLims$yLim, pch = 19, col = 'red', cex=1.5, xlab = "", ylab = "")
        points(reactiveState$b1Pos[1], reactiveState$b1Pos[2], pch = 19, col = 'blue', cex=1.5)
        if(dim(reactiveState$b1Trail)[1] > 0){
            points(reactiveState$b1Trail, pch = 21, col = 'blue')
            points(reactiveState$b2Trail, pch = 21, col = 'red')
        }
    }
    
    output$trajectoryPlot<-renderPlot({
        renderTrajPlot()
    },width = 400, height = 400)
    
    setPreset <- function(preset){
        updateNumericInput(session, "mass1", value = preset$body1$mass)
        updateNumericInput(session, "posX1", value = preset$body1$position[1])
        updateNumericInput(session, "posY1", value = preset$body1$position[2])
        updateNumericInput(session, "velX1", value = preset$body1$velocity[1])
        updateNumericInput(session, "velY1", value = preset$body1$velocity[2])
        updateNumericInput(session, "mass2", value = preset$body2$mass)
        updateNumericInput(session, "posX2", value = preset$body2$position[1])
        updateNumericInput(session, "posY2", value = preset$body2$position[2])
        updateNumericInput(session, "velX2", value = preset$body2$velocity[1])
        updateNumericInput(session, "velY2", value = preset$body2$velocity[2])
    }
     
    currSession<-reactiveValues()
    currSession$timer<-reactiveTimer(Inf)
    
    observeEvent(input$play,{
        initializeBodies()
        currSession$timer<-reactiveTimer(50)
        observeEvent(currSession$timer(),{
            forward()
        })
    })
    
    observeEvent(input$stop,{
        currSession$timer<-reactiveTimer(Inf)
        reactiveState$counter <- 0
        reactiveState$b1Trail <- data.frame()
        reactiveState$b2Trail <- data.frame()
    })
    
    observeEvent(input$preset, {
        preset <- switch(input$preset,
                       earthSun = earthSunPreset,
                       binaryStar = binaryStarPreset,
                       gravityAssist = gravityAssistPreset,
                       earthSunPreset)
        setPreset(preset)
        reactiveState$b1Pos <- c(preset$body1$position[1], preset$body1$position[2])
        reactiveState$b2Pos <- c(preset$body2$position[1], preset$body2$position[2])
        renderTrajPlot(as.numeric(preset$body1$position), as.numeric(preset$body2$position))
    })
    
    ####################################################
    # Adding bodies on the fly left for futur iterations
    ####################################################
    
    #inserted <- c()
    
    # observeEvent(input$insertBtn, addBodyInputForm())
    # 
    # observeEvent(input$removeBtn, {
    #     removeUI(
    #         ## pass in appropriate div id
    #         selector = paste0('#', inserted[length(inserted)])
    #     )
    #     inserted <<- inserted[-length(inserted)]
    # })
    
    # addBodyInputForm = function(){
    #     pos <- length(inserted) + 1
    #     id <- paste0('body', pos)
    #     insertUI(
    #         selector = '#placeholder',
    #         ## wrap element in a div with id for ease of removal
    #         ui = tags$div(
    #             bodyInputForm(pos), 
    #             id = id
    #         )
    #     )
    #     inserted <<- c(id, inserted)
    # }
    
    # bodyInputForm <- function(id){
    #     return(
    #         fluidRow(
    #             h4(paste("Body", id)),
    #             numericInput(paste0("mass", id), "Mass", 1, min = 0),
    #             h4("Position"),
    #             column(6,
    #                 numericInput(paste0("posX", id), "X", 0)
    #             ),
    #             column(6,
    #                 numericInput(paste0("posY", id), "Y", 0)
    #             ),
    #             h4("Velocity"),
    #             column(6,
    #                numericInput(paste0("velX", id), "X", 0)
    #             ),
    #             column(6,
    #                numericInput(paste0("valY", id), "Y", 0)
    #             )
    #         )
    #     )
    # }
    
    # addBodyInputForm()
    # addBodyInputForm()
}

# Run the application 
shinyApp(ui = ui, server = server)
