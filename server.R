library(shiny)
library(ggplot2)
library(pbapply)
library(reshape2)
library(dplyr)
library(DT)
library(gridExtra)
library(rhandsontable)
library(Cairo)
library(broom)
library(shinyjs)
library(formattable)
library(markdown)
library(rmarkdown)
library(XML)
library(corrplot)
library(scales)
library(caret)
library(randomForest)
library(DescTools)
pdf(NULL)


options(shiny.maxRequestSize=9000000*1024^2)

options(warn=-1)
assign("last.warning", NULL, envir = baseenv())

shinyServer(function(input, output, session) {
 
 observeEvent(!is.null(calibration), {
     



dataHold <- reactive({
    data <- calibration$Spectra

    
    data <- data[order(as.character(data$Spectrum)),]
    
    data$Spectrum <- gsub(".pdz", "", data$Spectrum)
    data$Spectrum <- gsub(".csv", "", data$Spectrum)
    data$Spectrum <- gsub(".CSV", "", data$Spectrum)
    data$Spectrum <- gsub(".spt", "", data$Spectrum)
    data$Spectrum <- gsub(".mca", "", data$Spectrum)
    data$Spectrum <- gsub(".spx", "", data$Spectrum)

    data
    
})

calFileIsTRUE <- TRUE


dataCount <- reactive({
    
    length(calibration$Intensities)

})






standardElements <- reactive({
    

        choosen.elements <- ls(calibration$Intensities)
        
        element.frame <- data.frame(elements=choosen.elements, order=atomic_order_vector(choosen.elements))
        as.vector(element.frame[order(element.frame$order),]$elements)
    
    
})

standardLines <- reactive({
    
    spectra.line.table <- dataHold()
    
    n <- length(names(spectra.line.table))
    
    
        spectralLines
 
    
})






selectedElementsCal <- reactive({
    
    choosen.elements <- ls(calibration$Intensities)
    
    element.frame <- data.frame(elements=choosen.elements, order=atomic_order_vector(choosen.elements))
    as.vector(element.frame[order(element.frame$order),]$elements)
    
})

elementallinestouse <- reactive({
    
    #c(selectedElements()_k_alpha, selectedElements()_k_beta, selectedElements()_l_alpha, selectedElements()_l_beta, selectedElements()_m)
    
    selectedElementsCal()
    
    
})



 
 spectraData <- reactive({

    elementFrame(data=dataHold(), elements=elementallinestouse())
     
 })
 
 
 calDataType <- reactive({
     
         spectraData()
   
 })
 
 
 
 tableInput <- reactive({
     
     elements <- elementallinestouse()


         spectraData()
   
     
     rounded <- round(select.line.table[,elements], digits=0)
     full <- data.frame(select.line.table$Spectrum, rounded)
     colnames(full) <- c("Spectrum", elements)
     
     full
 })





  
  filetype <- "CSV"
  
  



values <- reactiveValues()
values[["DF"]] <- calibration$Values



output$covarianceplotvalues <- renderPlot({
    
    data.table <- values[["DF"]]
    correlations <- cor(data.table[,3:length(data.table)], use="pairwise.complete.obs")
    corrplot(correlations, method="circle")
    
})

      # randomInterList <- reactive({
      #   if (is.null(input$intercept_vars))
      #   paste(,2)
      #   else
      #   input$intercept_vars
      #})
      
      
      #randomSlopeList <- reactive({
      #   if (is.null(input$intercept_vars))
      #   paste(,2)
      #   else
      #   input$slope_vars
      #})
      
      #output$nullintercept <- randomInterList()
      
      #output$nullslope <- randomSlopeList()


outVar <- reactive({
    !is.null(calibration)

    myelements <- elementallinestouse()

    result <- if(is.null(myelements)){
        "Ca.K.alpha"
    }else{
        myelements
    }
    
    result


    })

outVaralt <- reactive({
    !is.null(calibration)
    
    
    myelements <- c(elementallinestouse())

    
    if(is.null(myelements)){
        paste("Ca.K.alpha")
    }else{
        myelements
    }
    
})

outVaralt2 <- reactive({
    !is.null(calibration)
    
    
    myelements <- c(elementallinestouse())
    
    
    if(is.null(myelements)){
        paste("Ca.K.alpha")
    }else{
        myelements[! myelements %in% c(input$calcurveelement)]
    }
    
})

output$inVar2 <- renderUI({
    selectInput(inputId = "calcurveelement", label = h4("Element"), choices =  outVar())
})

inVar3Selectedpre <- reactive({
    
    hold <- values[["DF"]]
    
    optionhold <- if(is.null(input$calcurveelement)){
        ls(hold)[2]
    }else{
        input$calcurveelement
    }
    
    calibration$calList[[optionhold]][[1]]$Intercept

    
})




inVar4Selectedpre <- reactive({
    
    hold <- values[["DF"]]
    
    optionhold <- if(is.null(input$calcurveelement)){
        ls(hold)[2]
    }else{
        input$calcurveelement
    }
    
    
    calibration$calList[[optionhold]][[1]]$Slope

})




########Machine Learning: Normalization

normMinPre <- reactive({
    
    hold <- values[["DF"]]
    
    optionhold <- if(is.null(input$calcurveelement)){
        ls(hold)[2]
    }else{
        input$calcurveelement
    }
    
    calibration$calList[[optionhold]][[1]]$CalTable$Min

    
})

normMaxPre <- reactive({
    
    hold <- values[["DF"]]
    
    optionhold <- if(is.null(input$calcurveelement)){
        ls(hold)[2]
    }else{
        input$calcurveelement
    }
    
    calibration$calList[[optionhold]][[1]]$CalTable$Max

})



planktonVector <- reactive({
    
    #0.7, 0.9
    #2.5, 2.8
    #11.0, 11.2
    #18.4, 19.4
    #19.5, 22
    #21, 22
    #30, 35
    #35, 40
    
    mins <- c(0.7, 2.5, 11.0, 18.4, 19.5, 21, 30, 35)
    maxs <- c(0.9, 2.8, 11.2, 19.4, 22, 22, 35, 40)
    
    norm.list <- list(mins, maxs)
    names(norm.list) <- c("Min", "Max")
    norm.list
    
    
})

bestNormVars <- reactive({
    
    norm.list <- planktonVector()
    
    element <- input$calcurveelement
    
    choices <- elementallinestouse()
    spectra.line.table <- spectraLineTable()
    data <- dataNorm()
    
    index <- seq(1, length(norm.list[[1]]), 1)
    
    
    spectra.line.table <- spectraLineTable()[spectraLineTable()$Spectrum %in% holdFrame()$Spectrum, ]
    
    concentration.table <- concentrationTable()[concentrationTable()$Spectrum %in% holdFrame()$Spectrum, ]
    
    
    time.bic <- if(dataType()=="Spectra"){
        extractAIC(lm(concentration.table[, input$calcurveelement]~general_prep_xrf(spectra.line.table, input$calcurveelement)$Intensity, k=log(length(1))))[2]
    } else if(dataType()=="Net"){
        extractAIC(lm(concentration.table[, input$calcurveelement]~general_prep_xrf_net(spectra.line.table, input$calcurveelement)$Intensity, k=log(length(1))))[2]
    }
    
    tc.bic <- if(dataType()=="Spectra"){
        extractAIC(lm(concentration.table[, input$calcurveelement]~simple_tc_prep_xrf(data, spectra.line.table, input$calcurveelement)$Intensity, k=log(length(1))))[2]
    } else if(dataType()=="Net"){
        extractAIC(lm(concentration.table[, input$calcurveelement]~simple_tc_prep_xrf_net(data, spectra.line.table, input$calcurveelement)$Intensity, k=log(length(1))))[2]
    }
    
    comp.bic <- if(dataType()=="Spectra"){
        optimal_norm_chain_xrf(data=data, element=element, spectra.line.table=spectra.line.table, values=concentration.table, possible.mins=norm.list[["Min"]], possible.maxs=norm.list[["Max"]])
    } else if(dataType()=="Net"){
        time.bic
    }
    
    norm.chain <- c(time.bic, tc.bic, comp.bic)
    type.chain <- c(1, 2, 3)
    
    best <- index[[which.min(unlist(norm.chain))]]
    best.comp <- c(planktonVector()[["Min"]][best], planktonVector()[["Max"]][best])
    best.type <- type.chain[which.min(unlist(norm.chain))]
    result.list <- list(best.type, best.comp)
    names(result.list) <- c("Type", "Compton")
    result.list
})


normhold <- reactiveValues()

observeEvent(!is.null(calibration), {
    normhold$norms <- c(normMinPre(), normMaxPre())
    normhold$normtype <- calNormSelectionpre()
})


observeEvent(input$trainslopes, {
    
    isolate(normhold$norms[1] <- bestNormVars()[["Compton"]][1])
    isolate(normhold$norms[2] <- bestNormVars()[["Compton"]][2])
    isolate(normhold$normtype <- bestNormVars()[["Type"]])
    
})

calNormSelection <- reactive({
    normhold$normtype
})

normMinSelection <- reactive({
    normhold$norms[1]
})

normMaxSelection <- reactive({
    normhold$norms[2]
})




calNormSelectionpre <- reactive({
    
    hold <- values[["DF"]]
    
    optionhold <- if(is.null(input$calcurveelement)){
        ls(hold)[2]
    }else{
        input$calcurveelement
    }
    
    if(calFileIsTRUE==FALSE && is.null(calList[[optionhold]])==TRUE){
        calConditons[[1]][[2]]
    }else if(calFileIsTRUE==TRUE && is.null(calList[[optionhold]])==TRUE && is.null(calibration$calList[[optionhold]])==FALSE){
        calibration$calList[[optionhold]][[1]]$CalTable$NormType
    } else if(calFileIsTRUE==FALSE && is.null(calList[[optionhold]])==FALSE){
        calList[[optionhold]][[1]]$CalTable$NormType
    } else if(calFileIsTRUE==TRUE && is.null(calList[[optionhold]])==FALSE){
        calList[[optionhold]][[1]]$CalTable$NormType
    } else if(calFileIsTRUE==TRUE && is.null(calList[[optionhold]])==TRUE && is.null(calibration$calList[[optionhold]])==TRUE){
        calConditons[[1]][[2]]
    }
    
})


output$normTypeInput <- renderUI({
    
    selectInput("normcal", label = "Normalization",
    choices = list("Time" = 1, "Total Counts" = 2, "Compton" = 3),
    selected = calNormSelection())
    
    
})


output$comptonMinInput <- renderUI({
    
    numericInput('comptonmin', label=h6("Min"), step=0.001, value=normMinSelection(), min=0, max=50, width='30%')
    
})

output$comptonMaxInput <- renderUI({
    
    numericInput('comptonmax', label=h6("Max"), step=0.001, value=normMaxSelection(), min=0, max=50, width='30%')
    
})


#####Machine Learning: Intercepts



cephlopodVector <- reactive({
    
    combos_mod <- function(a.vector){
        
        so <- seq(from=1, to=length(a.vector), by=1)
        
        long <- pblapply(so, function(x) gRbase::combnPrim(x=a.vector, m=x), cl=6L)
        and <- pblapply(long, function(x) plyr::alply(x, 2), cl=6L)
        thanks.for.all.the.fish <- do.call(list, unlist(and, recursive=FALSE))
        
        thanks.for.all.the.fish
        
    }
    
    if(!is.null(likely_intercepts_xrf(input$calcurveelement))){
        combos_mod(likely_intercepts_xrf(input$calcurveelement))
    } else if(is.null(likely_intercepts_xrf(input$calcurveelement))){
        c("Rh.K.alpha", "Rh.L.alpha")
    }
    
})


bestInterceptVars <- reactive({
    
    element <- input$calcurveelement
    
    choices <- elementallinestouse()
    
    spectra.line.table <- if(all(cephlopodVector() %in% colnames(spectraLineTable()))==TRUE){
        spectraLineTable()
    } else if(all(cephlopodVector() %in% colnames(spectraLineTable()))==FALSE){
        merge(spectraLineTable(), elementFrame(data=dataHold(), elements=cephlopodVector()[cephlopodVector() %in% colnames(spectraLineTable())]))
    }
    
    data <- dataNorm()
    concentration.table <- concentrationTable()
    
    
    spectra.line.table <- spectra.line.table[spectra.line.table$Spectrum %in% holdFrame()$Spectrum, ]
    
    
    predict.intensity.list <- if(input$normcal==1){
        pblapply(cephlopodVector(), function(x) lucas_simp_prep_xrf(spectra.line.table=spectra.line.table, element.line=element, slope.element.lines=element, intercept.element.lines=c(element, x)))
    } else if(input$normcal==2){
        pblapply(cephlopodVector(), function(x) lucas_tc_prep_xrf(data=data, spectra.line.table=spectra.line.table, element.line=element, slope.element.lines=element, intercept.element.lines=c(element, x)))
    } else if(input$normcal==3){
        pblapply(cephlopodVector(), function(x) lucas_comp_prep_xrf(data=data, spectra.line.table=spectra.line.table, element.line=element, slope.element.lines=element, intercept.element.lines=c(element, x), norm.min=input$comptonmin, norm.max=input$comptonmax))
    }
    
    optimal_intercept_chain_xrf(element=element, intensities=predict.intensity.list, values=concentration.table, keep=vals$keeprows)
    
    
})


intercepthold <- reactiveValues()
intercepthold$intercepts <- NULL

observeEvent(input$calcurveelement, {
    
    isolate(intercepthold$intercepts <- inVar3Selectedpre())
    
})


#observeEvent(input$trainslopes, {
    
    #    isolate(intercepthold$intercepts <- bestInterceptVars())
    
    #})


inVar3Selected <- reactive({
    
    intercepthold$intercepts
    
    
})

output$inVar3 <- renderUI({
    
    selectInput(inputId = "intercept_vars", label = h4("Intercept"), choices =  outVaralt2(), selected=inVar3Selected(), multiple=TRUE)
})



####Machine Learning: Slopes

caretSlope <- reactive({
    
    element <- input$calcurveelement
    
    # prepare simple test suite
    control <- trainControl(method="cv", number=5)
    seed <- 7
    metric <- "RMSE"
    set.seed(seed)
    
    data <- dataNorm()
    concentration.table <- concentrationTable()

    concentration.table <- concentration.table[complete.cases(concentration.table[,input$calcurveelement]),]

    
    
    spectra.line.table <- spectraLineTable()[spectraLineTable()$Spectrum %in% holdFrame()$Spectrum, ]
    
    #spectra.line.table <- spectra.line.table[spectra.line.table$Spectrum %in% concentration.table$Spectrum, ]
    
    spectra.line.table <- spectra.line.table[complete.cases(concentration.table[, element]),]
    
    data <- data[data$Spectrum %in% concentration.table$Spectrum, ]
    

    cal.table <- if(dataType()=="Spectra"){
        if(input$normcal==1){
            lucas_simp_prep_xrf(spectra.line.table=spectra.line.table, element.line=input$calcurveelemenet,slope.element.lines=colnames(spectra.line.table[,-1]), intercept.element.lines=input$intercept_vars)
        } else if(input$normcal==2){
            lucas_tc_prep_xrf(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=colnames(spectra.line.table[,-1]), intercept.element.lines=input$intercept_vars)
        } else if(input$normcal==3){
            lucas_comp_prep_xrf(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=colnames(spectra.line.table[,-1]), intercept.element.lines=input$intercept_vars, norm.min=input$comptonmin, norm.max=input$comptonmax)
        }
    } else if(dataType()=="Net"){
        if(input$normcal==1){
            lucas_simp_prep_xrf_net(spectra.line.table=spectra.line.table, element.line=input$calcurveelemenet,slope.element.lines=colnames(spectra.line.table[,-1]), intercept.element.lines=input$intercept_vars)
        } else if(input$normcal==2){
            lucas_tc_prep_xrf_net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=colnames(spectra.line.table[,-1]), intercept.element.lines=input$intercept_vars)
        } else if(input$normcal==3){
            lucas_comp_prep_xrf_net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=colnames(spectra.line.table[,-1]), intercept.element.lines=input$intercept_vars, norm.min=input$comptonmin, norm.max=input$comptonmax)
        }
    }
    
    #cal.table <- cal.table[,!colnames(cal.table) %in% "Intensity"]
    cal.table$Concentration <- concentration.table[,input$calcurveelement]
        
    
    train(Concentration~., data=cal.table[,-1], method="lm", metric=metric, preProc=c("center", "scale"), trControl=control)
    

})



slopeImportance <- reactive({
    
    varImp(caretSlope(), scale=FALSE)
    
})


rainForestImportance <- reactive({
    
    
    as.data.frame(importance(elementModel()))
    
})

importanceFrame <- reactive({
    
    importance.frame <- rainForestImportance()
    colnames(importance.frame) <- c("NodePurity")
    importance.frame$Energy <- as.numeric(gsub("X", "", rownames(importance.frame)))
    importance.frame
    
})

rainForestImportancePlot <- reactive({
    
    importance.frame <- importanceFrame()
    
    
    ggplot(importance.frame) +
    geom_line(aes(Energy, NodePurity)) +
    theme_light() +
    scale_x_continuous("Energy (keV)")
    
    
})


variablesPlot <- reactive({
    
    if(calType()!=5){
        plot(slopeImportance())
    } else if(calType()==5){
        rainForestImportancePlot()
    }
    
})


output$importanceplot <- renderPlot({
    
    variablesPlot()
    
})


output$hover_info_variable <- renderUI({
    
    point.table <- importanceFrame()
    
    hover <- input$plot_hover_variable
    point <- nearPoints(point.table,  coordinfo=hover,   threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
    style = style,
    p(HTML(paste0("Energy:", " ", round(point$Energy, 0)))),
    p(HTML(paste0("NodePurity:", " ", round(point$NodePurity, 1))))
    )
})

output$variablePlot <- downloadHandler(
filename = function() { paste0(input$projectname, '_Variables', '.jpg', sep='') },
content = function(file) {
    ggsave(file,variablesPlot(), width=14, height=8, device="jpeg")
}
)


fishVector <- reactive({
    
    combos_mod <- function(a.vector){
        
        so <- seq(from=2, to=input$nvariables, by=1)
        
        long <- pblapply(so, function(x) gRbase::combnPrim(x=a.vector, m=x), cl=6L)
        and <- pblapply(long, function(x) plyr::alply(x, 2), cl=6L)
        thanks.for.all.the.fish <- do.call(list, unlist(and, recursive=FALSE))
        thanks.for.all.the.fish <- pblapply(thanks.for.all.the.fish, function(x) c(input$calcurveelement, x))
        
        thanks.for.all.the.fish
        
    }
    
    fit.lm <- caretSlope()
    
    first.combos <- c(elementallinestouse()[!elementallinestouse() %in% input$calcurveelement])
    
    coef.frame <- as.data.frame(summary(fit.lm)$coefficients)
    sig.frame <- subset(coef.frame, coef.frame[,4] < 0.05)
    
    second.combos <- first.combos[c(first.combos %in% rownames(sig.frame)[c(!rownames(sig.frame) %in% "(Intercept)")])]

    
    
    combos_mod(second.combos)
    

})


bestSlopeVars <- reactive({
    
    element <- input$calcurveelement
    
    choices <- elementallinestouse()
    spectra.line.table <- spectraLineTable()
    data <- dataNorm()
    concentration.table <- concentrationTable()
    
    #concentration.table[complete.cases(concentration.table[,input$calcurveelement]),]
    
    #index <- complete.cases(concentration.table[,input$calcurveelement])
    
    
    spectra.line.table <- spectraLineTable()[spectraLineTable()$Spectrum %in% holdFrame()$Spectrum, ]
    
    #spectra.line.table <- spectra.line.table[spectra.line.table$Spectrum %in% concentration.table$Spectrum, ]
    
    spectra.line.table <- spectra.line.table[complete.cases(concentration.table[, element]),]
    
    data <- data[data$Spectrum %in% concentration.table$Spectrum, ]


    
    
    predict.intensity <- if(input$normcal==1){
       if(dataType()=="Spectra"){
            lucas_simp_prep_xrf(spectra.line.table=spectra.line.table, element.line=element, slope.element.lines=choices, intercept.element.lines=input$intercept_vars)
        } else if(dataType()=="Net"){
            lucas_simp_prep_xrf_net(spectra.line.table=spectra.line.table, element.line=element, slope.element.lines=choices, intercept.element.lines=input$intercept_vars)
        }
    } else if(input$normcal==2){
        predict.intensity <- if(dataType()=="Spectra"){
            lucas_tc_prep_xrf(data=data, spectra.line.table=spectra.line.table, element.line=element, slope.element.lines=choices, intercept.element.lines=input$intercept_vars)
        } else if(dataType()=="Net"){
            lucas_tc_prep_xrf_net(data=data, spectra.line.table=spectra.line.table, element.line=element, slope.element.lines=choices, intercept.element.lines=input$intercept_vars)
        }
    } else if(input$normcal==3){
        predict.intensity <- if(dataType()=="Spectra"){
            lucas_comp_prep_xrf(data=data, spectra.line.table=spectra.line.table, element.line=element, slope.element.lines=choices, intercept.element.lines=input$intercept_vars, norm.min=input$comptonmin, norm.max=input$comptonmax)
        } else if(dataType()=="Net"){
            lucas_comp_prep_xrf_net(data=data, spectra.line.table=spectra.line.table, element.line=element, slope.element.lines=choices, intercept.element.lines=input$intercept_vars, norm.min=input$comptonmin, norm.max=input$comptonmax)
        }
    }

    
    
    #optimal_r_chain.xrf(element=element, intensities=predict.intensity, values= concentration.table, possible.slopes=fishVector(), keep=vals$keeprows)
    
    results <- variable_select_short(slopeImportance())
    
    c(input$calcurveelement, results[!results %in% input$calcurveelement])

    
})

slopehold <- reactiveValues()
slopehold$slopes <- NULL

observeEvent(input$calcurveelement, {
    
    isolate(slopehold$slopes <- inVar4Selectedpre())

})


observeEvent(input$trainslopes, {
    
    isolate(slopehold$slopes <- bestSlopeVars())
    
})


inVar4Selected <- reactive({
    
    slopehold$slopes
    
    
})



output$inVar4 <- renderUI({
    
    selectInput(inputId = "slope_vars", label = h4("Slope"), choices =  outVaralt(), selected=inVar4Selected(), multiple=TRUE)
})




#####Machine Learning: Cal Type



calTypeSelectionPre <- reactive({
    
    hold <- values[["DF"]]
    
    optionhold <- if(is.null(input$calcurveelement)){
        ls(hold)[2]
    }else{
        input$calcurveelement
    }
    
    calibration$calList[[optionhold]][[1]]$CalTable$CalType

    
})




bestCalType <- reactive({
    
    concentration.table <- concentrationTable()
    data <- dataNorm()
    spectra.line.table <- spectraLineTable()
    
    
    #concentration.table <- concentration.table[complete.cases(concentration.table[, input$calcurveelement]),]
    
    #spectra.line.table <- spectra.line.table[complete.cases(concentration.table[, input$calcurveelement]),]
    #data2 <- data[data$Spectrum %in% concentration.table$Spectrum, ]
    
    predict.intensity <- if(input$normcal==1){
        if(dataType()=="Spectra"){
            lucas_simp_prep_xrf(spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=elementallinestouse(), intercept.element.lines=input$intercept_vars)
        } else if(dataType()=="Net"){
            lucas_simp_prep_xrf_net(spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=elementallinestouse(), intercept.element.lines=input$intercept_vars)
        }
    } else if(input$normcal==2){
        predict.intensity <- if(dataType()=="Spectra"){
            lucas_tc_prep_xrf(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=elementallinestouse(), intercept.element.lines=input$intercept_vars)
        } else if(dataType()=="Net"){
            lucas_tc_prep_xrf_net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=elementallinestouse(), intercept.element.lines=input$intercept_vars)
        }
    } else if(input$normcal==3){
        predict.intensity <- if(dataType()=="Spectra"){
            lucas_comp_prep_xrf(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=elementallinestouse(), intercept.element.lines=input$intercept_vars, norm.min=input$comptonmin, norm.max=input$comptonmax)
        } else if(dataType()=="Net"){
            lucas_comp_prep_xrf_net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=elementallinestouse(), intercept.element.lines=input$intercept_vars, norm.min=input$comptonmin, norm.max=input$comptonmax)
        }
    }
    
    if(input$normcal==1){
        spectra.data <- if(dataType()=="Spectra"){
            spectra_simp_prep_xrf(spectra=data)[,-1]
        } else if(dataType()=="Net"){
            NULL
        }
    } else if(input$normcal==2){
        spectra.data <- if(dataType()=="Spectra"){
            spectra_tc_prep_xrf(spectra=data)[,-1]
        } else if(dataType()=="Net"){
            NULL
        }
    } else if(input$normcal==3){
        spectra.data <- if(dataType()=="Spectra"){
            spectra_comp_prep_xrf(spectra=data, norm.min=input$comptonmin, norm.max=input$comptonmax)[,-1]
        } else if(dataType()=="Net"){
            NULL
        }
    }
    
    
    
    predict.frame <- data.frame(predict.intensity, concentration.table[,input$calcurveelement])
    predict.frame <- predict.frame[complete.cases(predict.frame),]
    predict.frame <- predict.frame[vals$keeprows,]
    colnames(predict.frame) <- c(names(predict.intensity), "Concentration")
    predict.frame <- predict.frame[complete.cases(predict.frame$Concentration),]
    
    spectra.data$Concentration <- concentration.table[complete.cases(concentration.table[,input$calcurveelement]),input$calcurveelement]
    spectra.data <- spectra.data[complete.cases(spectra.data$Concentration),]
    spectra.data <- spectra.data[vals$keeprows,]
    
    
    predict.frame.simp <- predict.frame[,c("Concentration", "Intensity")]
    predict.frame.luc <- predict.frame[, c("Concentration", "Intensity", input$slope_vars)]
    predict.frame.forest <- predict.frame
    predict.frame.rainforest <- spectra.data
    
    cal.lm.simp <- lm(Concentration~Intensity, data=predict.frame.simp, na.action=na.exclude)
    lm.predict <- predict(cal.lm.simp, new.data=predict.frame.simp, proximity=FALSE)
    lm.sum <- summary(lm(predict.frame$Concentration~lm.predict, na.action=na.exclude))
    
    cal.lm.two <- lm(Concentration~Intensity + I(Intensity^2), data=predict.frame.simp, na.action=na.exclude)
    lm2.predict <- predict(cal.lm.two, new.data=predict.frame.simp, proximity=FALSE)
    lm2.sum <- summary(lm(predict.frame$Concentration~lm2.predict, na.action=na.exclude))
    
    cal.lm.luc <- lm(Concentration~., data=predict.frame.luc, na.action=na.exclude)
    lucas.predict <- predict(cal.lm.luc, new.data=predict.frame.luc, proximity=FALSE)
    lucas.sum <- summary(lm(predict.frame$Concentration~lucas.predict, na.action=na.exclude))
    
    cal.lm.forest <- randomForest(Concentration~., data=predict.frame.forest, na.action=na.omit)
    forest.predict <- predict(cal.lm.forest, new.data=predict.frame.forest, proximity=FALSE)
    forest.sum <- summary(lm(predict.frame$Concentration~forest.predict, na.action=na.exclude))
    
    cal.lm.rainforest <- randomForest(Concentration~., data=spectra.data, na.action=na.omit)
    rainforest.predict <- predict(cal.lm.rainforest, new.data=predict.frame.rainforest, proximity=FALSE)
    rainforest.sum <- summary(lm(predict.frame$Concentration~rainforest.predict, na.action=na.exclude))
    
    
    r2.slope.vector <- c(lm.sum$r.squared*lm.sum$coef[2], lm2.sum$r.squared*lm2.sum$coef[2], lucas.sum$r.squared*lucas.sum$coef[2], forest.sum$r.squared*forest.sum$coef[2], rainforest.sum$r.squared*rainforest.sum$coef[2])
    
    Closest(r2.slope.vector, 1, which=TRUE)
    
})


calhold <- reactiveValues()

    calhold$caltype <- calTypeSelectionPre()


observeEvent(input$trainslopes, {
    
     isolate(calhold$caltype <- bestCalType())
    
})



calTypeSelection <- reactive({
    calhold$caltype
})


output$calTypeInput <- renderUI({
    
    selectInput("radiocal", label = "Calibration Curve",
    choices = list("Linear" = 1, "Non-Linear" = 2, "Lucas-Tooth" = 3, "Forest" = 4, "Rainforest"=5),
    selected = calTypeSelection())
    
    
})




#####Set Defaults


calConditons <- reactiveValues()
calList <- reactiveValues()
calList <- NULL

observeEvent(!is.null(calibration), {
    
    cal.condition <- calhold$caltype
    norm.condition <- normhold$normtype
    
    norm.min <- normhold$norms[1]
    norm.max <- normhold$norms[2]
    
    cal.table <- data.frame(cal.condition, norm.condition, norm.min, norm.max)
    colnames(cal.table) <- c("CalType", "NormType", "Min", "Max")
    
    slope.corrections <- slopehold$slopes
    intercept.corrections <- intercepthold$intercepts
    
    standards.used <- vals$keeprows
    
    cal.mode.list <- list(cal.table, slope.corrections, intercept.corrections, standards.used)
    names(cal.mode.list) <- c("CalTable", "Slope", "Intercept", "StandardsUsed")
    
    calConditons <<- cal.mode.list
    
})







elementHold <- reactive({
    
    if(is.null(input$calcurveelement)==TRUE){
        ls(dataHold())[1]
    } else{
        input$calcurveelement
    }
    
})


  
  calFileStandards <- reactive({

          
          rep(TRUE, dataCount())

      
      
      
      
  })
  
  
  
  
  vals <- reactiveValues()
  

vals$keeprows <- if(calFileIsTRUE==TRUE){

    rep(TRUE, 34)
}

keepRowsFrame <- reactive({
    
    spectra.stuff <- values[["DF"]]
    rows <- vals$keeprows
    
    the.frame <- data.frame(Spectrum=spectra.stuff$Spectrum, Standards=rows)
    the.frame

})


output$whichrowstokeep <- renderRHandsontable({
    
    DF <- keepRowsFrame()
    
    DF <- DF[order(as.character(DF$Spectrum)),]
    
    
    
    if (!is.null(DF))
    rhandsontable(DF) %>% hot_col(2:length(DF), renderer=htmlwidgets::JS("safeHtmlRenderer"))
    
    
})




  
  #if(!is.null(calibration)){vals$keeprows <- vals$keeprows[dropStandard()]}


output$temp <- renderTable({
    
    as.data.frame(vals$keeprows)
    
})


dataType <- reactive({
    if(filetype=="CSV"){
        "Spectra"
    } else if(filetype=="TXT"){
        "Spectra"
    } else if(filetype=="Elio"){
        "Spectra"
    }  else if(filetype=="MCA"){
        "Spectra"
    }  else if(filetype=="SPX"){
        "Spectra"
    }  else if(filetype=="PDZ"){
        "Spectra"
    } else if (filetype=="Net"){
        "Net"
    }
    
})

  
  
  concentrationTable <- reactive({
      
      concentration.table <- data.frame(calibration$Values)

      concentration.table[concentration.table==""] <- NA
      concentration.table[values[["DF"]]$Include,]
      
  })
  
  spectraLineTable <- reactive({
      
      spectra.line.table <- data.frame(Spectrum=concentrationTable()$Spectrum, calibration$Intensities)
      
      spectra.line.table <- spectra.line.table[order(as.character(spectra.line.table$Spectrum)),]
      spectra.line.table

      
      
  })
  
  
  holdFrame <- reactive({
      
      spectra.line.table <- spectraLineTable()
      
      concentration <- as.vector(as.numeric(unlist(concentrationTable()[,input$calcurveelement])))
      
      
      
      intensity <- as.vector(as.numeric(unlist(spectraLineTable()[,input$calcurveelement])))
      
      spectra.names <- spectra.line.table$Spectrum
      
      hold.frame <- data.frame(spectra.names, concentration, intensity)
      colnames(hold.frame) <- c("Spectrum", "Concentration", "Intensity")
      hold.frame <- na.omit(hold.frame)
      
      hold.frame <- hold.frame[order(as.character(hold.frame$Spectrum)),]

      
      hold.frame[complete.cases(hold.frame),]

      
  })
  
  dataNorm <- reactive({
      
      data <- dataHold()
      data[data$Spectrum %in% holdFrame()$Spectrum, ]
      
      
  })
  
  
  predictFramePre <- reactive({
      
      intensity <- holdFrame()$Intensity

      concentration <- holdFrame()$Concentration
      
      predict.frame <- data.frame(concentration, intensity)
      colnames(predict.frame) <- c("Concentration", "Intensity")
      
      
      predict.frame
      
      
  })
  
  
  
  predictIntensity <- reactive({
      
      spectra.line.table <- spectraLineTable()
      data <- dataNorm()
      
      
      spectra.line.table <- spectraLineTable()[spectraLineTable()$Spectrum %in% holdFrame()$Spectrum, ]

      
      if (input$radiocal==1){
          
          if(input$normcal==1){
              predict.intensity <- if(dataType()=="Spectra"){
                  general_prep_xrf(spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
              } else if(dataType()=="Net"){
                  general_prep_xrf_net(spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
              }
          }
          
          if(input$normcal==2){
              predict.intensity <- if(dataType()=="Spectra"){
                  simple_tc_prep_xrf(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
              } else if(dataType()=="Net"){
                  simple_tc_prep_xrf_net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
              }
          }
          
          if(input$normcal==3){
              predict.intensity <- if(dataType()=="Spectra"){
                  simple_comp_prep_xrf(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, norm.min=input$comptonmin, norm.max=input$comptonmax)
              } else if(dataType()=="Net"){
                  simple_comp_prep_xrf_net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, norm.min=input$comptonmin, norm.max=input$comptonmax)
              }
          }
          
      }
          
          if (input$radiocal==2){
              
              if(input$normcal==1){
                  predict.intensity <- if(dataType()=="Spectra"){
                      general_prep_xrf(spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
                  } else if(dataType()=="Net"){
                      general_prep_xrf_net(spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
                  }
              }
              
              if(input$normcal==2){
                  predict.intensity <- if(dataType()=="Spectra"){
                      simple_tc_prep_xrf(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
                  } else if(dataType()=="Net"){
                      simple_tc_prep_xrf_net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
                  }
              }
          
          if(input$normcal==3){
              predict.intensity <- if(dataType()=="Spectra"){
                  simple_comp_prep_xrf(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, norm.min=input$comptonmin, norm.max=input$comptonmax)
              } else if(dataType()=="Net"){
                  simple_comp_prep_xrf_net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, norm.min=input$comptonmin, norm.max=input$comptonmax)
              }
          }
          
      }
      
      
      if (input$radiocal==3){
          
          if(input$normcal==1){
              predict.intensity <- if(dataType()=="Spectra"){
                  lucas_simp_prep_xrf(spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
              } else if(dataType()=="Net"){
                  lucas_simp_prep_xrf_net(spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
              }
          }
          
          if(input$normcal==2){
              predict.intensity <- if(dataType()=="Spectra"){
                  lucas_tc_prep_xrf(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
              } else if(dataType()=="Net"){
                  lucas_tc_prep_xrf_net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
              }
          }
          
          if(input$normcal==3){
              predict.intensity <- if(dataType()=="Spectra"){
                  lucas_comp_prep_xrf(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars, norm.min=input$comptonmin, norm.max=input$comptonmax)
              } else if(dataType()=="Net"){
                  lucas_comp_prep_xrf_net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars, norm.min=input$comptonmin, norm.max=input$comptonmax)
              }
          }
          
      }
      
      if (input$radiocal==4){
          concentration.table <- concentrationTable()
          spectra.line.table <- spectra.line.table[complete.cases(concentration.table[, input$calcurveelement]),]
          data <- data[data$Spectrum %in% concentration.table$Spectrum, ]

          predict.intensity <- if(input$normcal==1){
              if(dataType()=="Spectra"){
                  lucas_simp_prep_xrf(spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=elementallinestouse(), intercept.element.lines=input$intercept_vars)
              } else if(dataType()=="Net"){
                  lucas_simp_prep_xrf_net(spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=elementallinestouse(), intercept.element.lines=input$intercept_vars)
              }
          } else if(input$normcal==2){
              predict.intensity <- if(dataType()=="Spectra"){
                  lucas_tc_prep_xrf(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=elementallinestouse(), intercept.element.lines=input$intercept_vars)
              } else if(dataType()=="Net"){
                  lucas_tc_prep_xrf_net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=elementallinestouse(), intercept.element.lines=input$intercept_vars)
              }
          } else if(input$normcal==3){
              predict.intensity <- if(dataType()=="Spectra"){
                  lucas_comp_prep_xrf(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=elementallinestouse(), intercept.element.lines=input$intercept_vars, norm.min=input$comptonmin, norm.max=input$comptonmax)
              } else if(dataType()=="Net"){
                  lucas_comp_prep_xrf_net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=elementallinestouse(), intercept.element.lines=input$intercept_vars, norm.min=input$comptonmin, norm.max=input$comptonmax)
              }
          }
          
      }
      
      
      if (input$radiocal==5){
          if(input$normcal==1){
              predict.intensity <- if(dataType()=="Spectra"){
                  spectra_simp_prep_xrf(spectra=data)[,-1]
              } else if(dataType()=="Net"){
                  NULL
              }
          } else if(input$normcal==2){
              predict.intensity <- if(dataType()=="Spectra"){
                  spectra_tc_prep_xrf(spectra=data)[,-1]
              } else if(dataType()=="Net"){
                  NULL
              }
          } else if(input$normcal==3){
              predict.intensity <- if(dataType()=="Spectra"){
                  spectra_comp_prep_xrf(spectra=data, norm.min=input$comptonmin, norm.max=input$comptonmax)[,-1]
              } else if(dataType()=="Net"){
                  NULL
              }
          }
      }
      
      
      predict.intensity
      
      
  })
  
  
  predictFrame <- reactive({
      
      predict.frame <- predictFramePre()
      predict.intensity <- predictIntensity()
      

      
      predict.frame <- data.frame(predict.intensity, predict.frame$Concentration)
      colnames(predict.frame) <- c(names(predict.intensity), "Concentration")
      
      
      
      predict.frame
      
      
  })
  
  
  predictFrameName <- reactive({
      
      predict.frame <- predictFrame()[ vals$keeprows, , drop = FALSE]
      spectra.line.table <- spectraLineTable()[ vals$keeprows, , drop = FALSE]
      
      predict.frame.name <- data.frame(spectra.line.table$Spectrum, predict.frame)
      colnames(predict.frame.name) <- c("Spectrum", names(predict.frame))
      predict.frame.name

  })
  
  
  
  calCurveFrame <- reactive({
      
      predictFrame()
      
  })
  
  
  elementModel <- reactive({
      
      predict.frame <- predictFrame()
      
      
      if (input$radiocal==1){
          cal.lm <- lm(Concentration~Intensity, data=predict.frame[ vals$keeprows, , drop = FALSE])
      }
      
      
      if (input$radiocal==2){
          cal.lm <- lm(Concentration~Intensity + I(Intensity^2), data=predict.frame[ vals$keeprows, , drop = FALSE])
      }
      
      if (input$radiocal==3){
          cal.lm <- lm(Concentration~., data=predict.frame[ vals$keeprows, , drop = FALSE])
      }
      
      if (input$radiocal==4){
          cal.lm <- randomForest(Concentration~., data=predict.frame[ vals$keeprows, , drop = FALSE], na.action=na.omit)
      }
      
      if (input$radiocal==5){
          cal.lm <- randomForest(Concentration~., data=predict.frame[ vals$keeprows, , drop = FALSE], na.action=na.omit)
      }
      
      cal.lm
      
  })
  
  
  valFrame <- reactive({
      
      predict.intensity <- predictIntensity()
      predict.frame <- predictFrame()
      element.model <- elementModel()
      
      
      if (input$radiocal==1){
          cal.est.conc.pred <- predict(object=element.model, newdata=predict.intensity, interval='confidence')
          cal.est.conc.tab <- data.frame(cal.est.conc.pred)
          cal.est.conc <- cal.est.conc.tab$fit
          
          val.frame <- data.frame(predict.frame$Concentration, cal.est.conc)
          colnames(val.frame) <- c("Concentration", "Prediction")
      }
      
      if (input$radiocal==2){
          cal.est.conc.pred <- predict(object=element.model, newdata=predict.intensity, interval='confidence')
          cal.est.conc.tab <- data.frame(cal.est.conc.pred)
          cal.est.conc <- cal.est.conc.tab$fit
          
          val.frame <- data.frame(predict.frame$Concentration, cal.est.conc)
          colnames(val.frame) <- c("Concentration", "Prediction")
      }
      
      if (input$radiocal==3){
          
          xmin = 0; xmax=10
          N = length(predict.frame$Concentration)
          means = colMeans(predict.frame)
          dummyDF = t(as.data.frame(means))
          for(i in 2:N){dummyDF=rbind(dummyDF,means)}
          xv=seq(xmin,xmax, length.out=N)
          dummyDF$Concentration = xv
          yv=predict(element.model, newdata=predict.intensity)
          
          
          lucas.x <- yv
          
          cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity, interval='confidence')
          cal.est.conc.tab <- data.frame(cal.est.conc.pred.luc)
          cal.est.conc.luc <- cal.est.conc.tab$fit
          cal.est.conc.luc.up <- cal.est.conc.tab$upr
          cal.est.conc.luc.low <- cal.est.conc.tab$lwr
          
          
          val.frame <- data.frame(predict.frame$Concentration, predict.intensity$Intensity, lucas.x, cal.est.conc.luc, cal.est.conc.luc.up, cal.est.conc.luc.low)
          colnames(val.frame) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
      }
      
      if (input$radiocal==4){
          
          xmin = 0; xmax=10
          N = length(predict.frame$Concentration)
          means = colMeans(predict.frame)
          dummyDF = t(as.data.frame(means))
          for(i in 2:N){dummyDF=rbind(dummyDF,means)}
          xv=seq(xmin,xmax, length.out=N)
          dummyDF$Concentration = xv
          yv=predict(element.model, newdata=predict.intensity)
          
          
          lucas.x <- yv
          
          cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity, interval='confidence')
          #cal.est.conc.tab <- data.frame(cal.est.conc.pred.luc)
          #cal.est.conc.luc <- cal.est.conc.tab$fit
          #cal.est.conc.luc.up <- cal.est.conc.tab$upr
          #cal.est.conc.luc.low <- cal.est.conc.tab$lwr
          
          
          val.frame <- data.frame(predict.frame$Concentration, predict.intensity$Intensity, lucas.x, as.vector(cal.est.conc.pred.luc))
          colnames(val.frame) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction")
      }
      
      
      if (input$radiocal==5){
          
          xmin = 0; xmax=10
          N = length(predict.frame$Concentration)
          means = colMeans(predict.frame)
          dummyDF = t(as.data.frame(means))
          for(i in 2:N){dummyDF=rbind(dummyDF,means)}
          xv=seq(xmin,xmax, length.out=N)
          dummyDF$Concentration = xv
          yv=predict(element.model, newdata=predict.intensity)
          
          
          lucas.x <- yv
          
          cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity, interval='confidence')
          #cal.est.conc.tab <- data.frame(cal.est.conc.pred.luc)
          #cal.est.conc.luc <- cal.est.conc.tab$fit
          #cal.est.conc.luc.up <- cal.est.conc.tab$upr
          #cal.est.conc.luc.low <- cal.est.conc.tab$lwr
          
          
          val.frame <- data.frame(predict.frame$Concentration, lucas.x, as.vector(cal.est.conc.pred.luc))
          colnames(val.frame) <- c("Concentration",  "IntensityNorm", "Prediction")
      }
      
      
      
      
      val.frame
      
  })
  
  calValFrame <- reactive({
      
      valFrame()
      
  })
  
  
  calType <- reactive({
      
      if(input$radiocal==1){
          1
      } else if(input$radiocal==2){
          2
      } else if(input$radiocal==3){
          3
      } else if(input$radiocal==4){
          3
      } else if(input$radiocal==5){
          5
      }
      
  })
  
  rangescalcurve <- reactiveValues(x = NULL, y = NULL)
  
  
  
  calCurvePlot <- reactive({
      
      predict.frame <- predictFrame()
      element.model <- elementModel()
      val.frame <- valFrame()
      
      element.name <- gsub("[.]", "", substr(input$calcurveelement, 1, 2))
      intens <- " Counts per Second"
      norma <- " Normalized"
      norma.comp <- " Compton Normalized"
      norma.tc <- " Valid Counts Normalized"
      conen <- " (%)"
      predi <- " Estimate (%)"
      log <- "Log "
      
      
      intensity.name <- c(element.name, intens)
      concentration.name <- c(element.name, conen)
      prediction.name <- c(element.name, predi)
      
      
      if(input$radiocal==1){
          calcurve.plot <- ggplot(data=predict.frame[ vals$keeprows, , drop = FALSE], aes(Intensity, Concentration)) +
          theme_light() +
          annotate("text", label=lm_eqn(lm(Concentration~Intensity, predict.frame[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
          geom_point() +
          geom_point(data = predict.frame[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
          stat_smooth(method="lm", fullrange = TRUE) +
          scale_x_continuous(paste(element.name, intens)) +
          scale_y_continuous(paste(element.name, conen)) +
          coord_cartesian(xlim = rangescalcurve$x, ylim = rangescalcurve$y, expand = TRUE)
          
      }
      
      if(input$radiocal==2){
          calcurve.plot <- ggplot(data=predict.frame[ vals$keeprows, , drop = FALSE], aes(Intensity, Concentration)) +
          theme_light() +
          annotate("text", label=lm_eqn_poly(lm(Concentration~Intensity + I(Intensity^2), predict.frame[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
          geom_point() +
          geom_point(data = predict.frame[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
          stat_smooth(method="lm", formula=y~poly(x,2)) +
          scale_x_continuous(paste(element.name, intens)) +
          scale_y_continuous(paste(element.name, conen)) +
          coord_cartesian(xlim = rangescalcurve$x, ylim = rangescalcurve$y, expand = TRUE)
          
      }
      
      if(input$radiocal==3){
          calcurve.plot <- ggplot(data=val.frame[ vals$keeprows, , drop = FALSE], aes(IntensityNorm, Concentration)) +
          theme_light() +
          annotate("text", label=lm_eqn(lm(Concentration~., val.frame[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
          geom_point() +
          geom_point(aes(IntensityNorm, Concentration), data = val.frame[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
          geom_smooth(aes(x=IntensityNorm, y=Concentration, ymin = Lower, ymax = Upper)) +
          scale_x_continuous(paste(element.name, norma)) +
          scale_y_continuous(paste(element.name, conen)) +
          coord_cartesian(xlim = rangescalcurve$x, ylim = rangescalcurve$y, expand = TRUE)
          
      }
      
      if(input$radiocal==4){
          calcurve.plot <- ggplot(data=val.frame[ vals$keeprows, , drop = FALSE], aes(IntensityNorm, Concentration)) +
          theme_light() +
          annotate("text", label=lm_eqn(lm(Concentration~., val.frame[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
          geom_point() +
          geom_point(aes(IntensityNorm, Concentration), data = val.frame[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
          geom_smooth() +
          scale_x_continuous(paste(element.name, norma)) +
          scale_y_continuous(paste(element.name, conen)) +
          coord_cartesian(xlim = rangescalcurve$x, ylim = rangescalcurve$y, expand = TRUE)
          
      }
      
      if(input$radiocal==5){
          calcurve.plot <- ggplot(data=val.frame[ vals$keeprows, , drop = FALSE], aes(IntensityNorm, Concentration)) +
          theme_light() +
          annotate("text", label=lm_eqn(lm(Concentration~., val.frame[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
          geom_point() +
          geom_point(aes(IntensityNorm, Concentration), data = val.frame[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
          geom_smooth() +
          scale_x_continuous(paste(element.name, norma)) +
          scale_y_continuous(paste(element.name, conen)) +
          coord_cartesian(xlim = rangescalcurve$x, ylim = rangescalcurve$y, expand = TRUE)
          
      }
      
      
      
      calcurve.plot
      
      
  })
  
  observeEvent(input$cropcal, {
      brush <- input$plot_cal_brush
      if (!is.null(brush)) {
          rangescalcurve$x <- c(brush$xmin, brush$xmax)
          rangescalcurve$y <- c(brush$ymin, brush$ymax)
          
      } else {
          rangescalcurve$x <- NULL
          rangescalcurve$y <- NULL
      }
  })
  
  output$calcurveplots <- renderPlot({
      calCurvePlot()
  })
  
  
  rangesvalcurve <- reactiveValues(x = NULL, y = NULL)
  
  
  valCurvePlot <- reactive({
      
      predict.frame <- predictFrame()
      element.model <- elementModel()
      
      
      
      element.name <- gsub("[.]", "", substr(input$calcurveelement, 1, 2))
      intens <- " Counts per Second"
      norma <- " Normalized"
      norma.comp <- " Compton Normalized"
      norma.tc <- " Valid Counts Normalized"
      conen <- " (%)"
      predi <- " Estimate (%)"
      log <- "Log "
      
      intensity.name <- c(element.name, intens)
      concentration.name <- c(element.name, conen)
      prediction.name <- c(element.name, predi)
      val.frame <- valFrame()
      
      
      valcurve.plot <- ggplot(data=val.frame[ vals$keeprows, , drop = FALSE], aes(Prediction, Concentration)) +
      theme_bw() +
      annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
      geom_abline(intercept=0, slope=1, lty=2) +
      stat_smooth(method="lm") +
      geom_point() +
      geom_point(aes(Prediction, Concentration),  data = val.frame[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
      scale_x_continuous(paste(element.name, predi)) +
      scale_y_continuous(paste(element.name, conen)) +
      coord_cartesian(xlim = rangesvalcurve$x, ylim = rangesvalcurve$y, expand = TRUE)
      
      
      
      
      
      valcurve.plot
      
  })
  
  observeEvent(input$cropval, {
      brush <- input$plot_val_brush
      if (!is.null(brush)) {
          rangesvalcurve$x <- c(brush$xmin, brush$xmax)
          rangesvalcurve$y <- c(brush$ymin, brush$ymax)
          
      } else {
          rangesvalcurve$x <- NULL
          rangesvalcurve$y <- NULL
      }
  })
  
  
  output$valcurveplots <- renderPlot({
      valCurvePlot()
  })
  
  
  
  calPlotDownload <- reactive({
      
      grid.arrange(calCurvePlot(), valCurvePlot(), ncol=2)
      
  })
  
  
  output$downloadcloudplot <- downloadHandler(
  filename = function() { paste(paste(c(input$calname, "_", input$calcurveelement), collapse=''), '.tiff',  sep='') },
  content = function(file) {
      ggsave(file,calPlotDownload(), device="tiff", compression="lzw", type="cairo", dpi=300, width=12, height=7)
  }
  )
  
  
  calValTable <- reactive({
      
      standard.table <- valFrame()
      hold.frame <- holdFrame()
      
      standard.table.summary <- data.frame(hold.frame$Spectrum, standard.table$Concentration, standard.table$Prediction, standard.table$Concentration-standard.table$Prediction, ((standard.table$Concentration-standard.table$Prediction)/standard.table$Concentration))
      colnames(standard.table.summary) <- c("Standard", "Concentration", "Prediction", "Difference", "Relative")
      
      standard.table.summary[,-1] <-round(standard.table.summary[,-1],4)
      standard.table.summary[,5] <- as.character(percent(standard.table.summary[,5]))
      
      this.table <- standard.table.summary
      this.table
      
  })
  
  
  output$standardsperformance <- DT::renderDataTable({
      
      
      standard.table <- calValTable()
      standard.table
      
  }, options =list(aoColumnDefs = list(list(sClass="alignRight",aTargets=c(list(2), list(3),list(4),list(5))))  ))
  
  
  randomizeData <- reactive({
      
      cal.frame <- concentrationTable()
      cal.frame <- cal.frame[ vals$keeprows, , drop = FALSE]
      total.number <- length(cal.frame[,1])
      sample.number <- total.number-round(input$percentrandom*total.number, 0)
      
      hold <- cal.frame[sample(nrow(cal.frame), sample.number),]
      cal.frame$Spectrum %in% hold$Spectrum
      
  })
  
  
  
  calCurveFrameRandomized <- reactive({
      
      predict.frame <- predictFrame()
      predict.frame <- predict.frame[ vals$keeprows, , drop = FALSE]
      
      predict.frame[randomizeData(),]
      
  })
  
  
  elementModelRandom <- reactive({
      
      predict.frame <- calCurveFrameRandomized()
      
      
      if (input$radiocal==1){
          cal.lm <- lm(Concentration~Intensity, data=predict.frame)
      }
      
      
      if (input$radiocal==2){
          cal.lm <- lm(Concentration~Intensity + I(Intensity^2), data=predict.frame)
      }
      
      if (input$radiocal==3){
          cal.lm <- lm(Concentration~., data=predict.frame)
      }
      
      if (input$radiocal==4){
          cal.lm <- randomForest(Concentration~., data=predict.frame, na.action=na.omit)
      }
      
      if (input$radiocal==5){
          cal.lm <- randomForest(Concentration~., data=predict.frame, na.action=na.omit)
      }
      
      cal.lm
      
  })
  
  
  valFrameRandomized <- reactive({
      
      predict.intensity <- predictIntensity()[ vals$keeprows, , drop = FALSE]
      predict.frame <- predictFrame()[ vals$keeprows, , drop = FALSE]
      
      predict.intensity <- predict.intensity[!(randomizeData()), , drop = FALSE]
      predict.frame <- predict.frame[!(randomizeData()), , drop = FALSE]
      element.model <- elementModelRandom()
      
      
      
      if (input$radiocal==1){
          cal.est.conc.pred <- predict(object=element.model, newdata=predict.intensity, interval='confidence')
          cal.est.conc.tab <- data.frame(cal.est.conc.pred)
          cal.est.conc <- cal.est.conc.tab$fit
          
          val.frame <- data.frame(predict.frame$Concentration, cal.est.conc)
          colnames(val.frame) <- c("Concentration", "Prediction")
      }
      
      if (input$radiocal==2){
          cal.est.conc.pred <- predict(object=element.model, newdata=predict.intensity, interval='confidence')
          cal.est.conc.tab <- data.frame(cal.est.conc.pred)
          cal.est.conc <- cal.est.conc.tab$fit
          
          val.frame <- data.frame(predict.frame$Concentration, cal.est.conc)
          colnames(val.frame) <- c("Concentration", "Prediction")
      }
      
      if (input$radiocal==3){
          
          xmin = 0; xmax=10
          N = length(predict.frame$Concentration)
          means = colMeans(predict.frame)
          dummyDF = t(as.data.frame(means))
          for(i in 2:N){dummyDF=rbind(dummyDF,means)}
          xv=seq(xmin,xmax, length.out=N)
          dummyDF$Concentration = xv
          yv=predict(element.model, newdata=predict.intensity)
          
          
          lucas.x <- yv
          
          cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity, interval='confidence')
          cal.est.conc.tab <- data.frame(cal.est.conc.pred.luc)
          cal.est.conc.luc <- cal.est.conc.tab$fit
          cal.est.conc.luc.up <- cal.est.conc.tab$upr
          cal.est.conc.luc.low <- cal.est.conc.tab$lwr
          
          
          val.frame <- data.frame(predict.frame$Concentration, predict.intensity$Intensity, lucas.x, cal.est.conc.luc, cal.est.conc.luc.up, cal.est.conc.luc.low)
          colnames(val.frame) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
      }
      
      if (input$radiocal==4){
          
          xmin = 0; xmax=10
          N = length(predict.frame$Concentration)
          means = colMeans(predict.frame)
          dummyDF = t(as.data.frame(means))
          for(i in 2:N){dummyDF=rbind(dummyDF,means)}
          xv=seq(xmin,xmax, length.out=N)
          dummyDF$Concentration = xv
          yv=predict(element.model, newdata=predict.intensity)
          
          
          lucas.x <- yv
          
          cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity, interval='confidence')
          #cal.est.conc.tab <- data.frame(cal.est.conc.pred.luc)
          #cal.est.conc.luc <- cal.est.conc.tab$fit
          #cal.est.conc.luc.up <- cal.est.conc.tab$upr
          #cal.est.conc.luc.low <- cal.est.conc.tab$lwr
          
          
          val.frame <- data.frame(predict.frame$Concentration, predict.intensity$Intensity, lucas.x, as.vector(cal.est.conc.pred.luc))
          colnames(val.frame) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction")
      }
      
      if (input$radiocal==5){
          
          xmin = 0; xmax=10
          N = length(predict.frame$Concentration)
          means = colMeans(predict.frame)
          dummyDF = t(as.data.frame(means))
          for(i in 2:N){dummyDF=rbind(dummyDF,means)}
          xv=seq(xmin,xmax, length.out=N)
          dummyDF$Concentration = xv
          yv=predict(element.model, newdata=predict.intensity)
          
          
          lucas.x <- yv
          
          cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity, interval='confidence')
          #cal.est.conc.tab <- data.frame(cal.est.conc.pred.luc)
          #cal.est.conc.luc <- cal.est.conc.tab$fit
          #cal.est.conc.luc.up <- cal.est.conc.tab$upr
          #cal.est.conc.luc.low <- cal.est.conc.tab$lwr
          
          
          val.frame <- data.frame(predict.frame$Concentration, lucas.x, as.vector(cal.est.conc.pred.luc))
          colnames(val.frame) <- c("Concentration", "IntensityNorm", "Prediction")
      }
      
      
      
      
      val.frame
      
  })
  
  
  valFrameRandomizedRev <- reactive({
      
      predict.intensity <- predictIntensity()[ vals$keeprows, , drop = FALSE]
      predict.frame <- predictFrame()[ vals$keeprows, , drop = FALSE]
      
      predict.intensity <- predict.intensity[(randomizeData()), ]
      predict.frame <- predict.frame[(randomizeData()), ]
      element.model <- elementModelRandom()
      
      
      
      if (input$radiocal==1){
          cal.est.conc.pred <- predict(object=element.model, newdata=predict.intensity, interval='confidence')
          cal.est.conc.tab <- data.frame(cal.est.conc.pred)
          cal.est.conc <- cal.est.conc.tab$fit
          
          val.frame <- data.frame(predict.frame$Concentration, cal.est.conc)
          colnames(val.frame) <- c("Concentration", "Prediction")
      }
      
      if (input$radiocal==2){
          cal.est.conc.pred <- predict(object=element.model, newdata=predict.intensity, interval='confidence')
          cal.est.conc.tab <- data.frame(cal.est.conc.pred)
          cal.est.conc <- cal.est.conc.tab$fit
          
          val.frame <- data.frame(predict.frame$Concentration, cal.est.conc)
          colnames(val.frame) <- c("Concentration", "Prediction")
      }
      
      if (input$radiocal==3){
          
          xmin = 0; xmax=10
          N = length(predict.frame$Concentration)
          means = colMeans(predict.frame)
          dummyDF = t(as.data.frame(means))
          for(i in 2:N){dummyDF=rbind(dummyDF,means)}
          xv=seq(xmin,xmax, length.out=N)
          dummyDF$Concentration = xv
          yv=predict(element.model, newdata=predict.intensity)
          
          
          lucas.x <- yv
          
          cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity, interval='confidence')
          cal.est.conc.tab <- data.frame(cal.est.conc.pred.luc)
          cal.est.conc.luc <- cal.est.conc.tab$fit
          cal.est.conc.luc.up <- cal.est.conc.tab$upr
          cal.est.conc.luc.low <- cal.est.conc.tab$lwr
          
          
          val.frame <- data.frame(predict.frame$Concentration, predict.intensity$Intensity, lucas.x, cal.est.conc.luc, cal.est.conc.luc.up, cal.est.conc.luc.low)
          colnames(val.frame) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
      }
      
      if (input$radiocal==4){
          
          xmin = 0; xmax=10
          N = length(predict.frame$Concentration)
          means = colMeans(predict.frame)
          dummyDF = t(as.data.frame(means))
          for(i in 2:N){dummyDF=rbind(dummyDF,means)}
          xv=seq(xmin,xmax, length.out=N)
          dummyDF$Concentration = xv
          yv=predict(element.model, newdata=predict.intensity)
          
          
          lucas.x <- yv
          
          cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity, interval='confidence')
          
          
          
          val.frame <- data.frame(predict.frame$Concentration, predict.intensity$Intensity, lucas.x, as.vector(cal.est.conc.pred.luc))
          colnames(val.frame) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction")
      }
      
      if (input$radiocal==5){
          
          xmin = 0; xmax=10
          N = length(predict.frame$Concentration)
          means = colMeans(predict.frame)
          dummyDF = t(as.data.frame(means))
          for(i in 2:N){dummyDF=rbind(dummyDF,means)}
          xv=seq(xmin,xmax, length.out=N)
          dummyDF$Concentration = xv
          yv=predict(element.model, newdata=predict.intensity)
          
          
          lucas.x <- yv
          
          cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity, interval='confidence')
          
          
          
          val.frame <- data.frame(predict.frame$Concentration, lucas.x, as.vector(cal.est.conc.pred.luc))
          colnames(val.frame) <- c("Concentration", "IntensityNorm", "Prediction")
      }
      
      
      
      
      val.frame
      
  })
  
  rangescalcurverandom <- reactiveValues(x = NULL, y = NULL)
  
  
  calCurvePlotRandom <- reactive({
      
      predict.frame <- calCurveFrameRandomized()
      element.model <- elementModelRandom()
      
      
      element.name <- gsub("[.]", "", substr(input$calcurveelement, 1, 2))
      intens <- " Counts per Second"
      norma <- " Normalized"
      norma.comp <- " Compton Normalized"
      norma.tc <- " Valid Counts Normalized"
      conen <- " (%)"
      predi <- " Estimate (%)"
      
      intensity.name <- c(element.name, intens)
      concentration.name <- c(element.name, conen)
      prediction.name <- c(element.name, predi)
      
      
      if(input$radiocal==1){
          calcurve.plot <- ggplot(data=predict.frame, aes(Intensity, Concentration)) +
          theme_light() +
          annotate("text", label=lm_eqn(element.model), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
          geom_point() +
          geom_point(data = predict.frame, shape = 21, fill = "red", color = "black", alpha = 0.25) +
          stat_smooth(method="lm", fullrange = TRUE) +
          scale_x_continuous(paste(element.name, intens)) +
          scale_y_continuous(paste(element.name, conen)) +
          coord_cartesian(xlim = rangescalcurverandom$x, ylim = rangescalcurverandom$y, expand = TRUE)
          
      }
      
      if(input$radiocal==2){
          
          calcurve.plot <- ggplot(data=predict.frame, aes(Intensity, Concentration)) +
          theme_light() +
          annotate("text", label=lm_eqn_poly(element.model), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
          geom_point() +
          geom_point(data = predict.frame, shape = 21, fill = "red", color = "black", alpha = 0.25) +
          stat_smooth(method="lm", formula=y~poly(x,2)) +
          scale_x_continuous(paste(element.name, intens)) +
          scale_y_continuous(paste(element.name, conen)) +
          coord_cartesian(xlim = rangescalcurverandom$x, ylim = rangescalcurverandom$y, expand = TRUE)
      }
      
      if(input$radiocal==3){
          val.frame <- valFrameRandomizedRev()
          
          calcurve.plot <- ggplot(data=val.frame, aes(IntensityNorm, Concentration)) +
          theme_light() +
          annotate("text", label=lm_eqn(element.model), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
          geom_point() +
          geom_point(aes(IntensityNorm, Concentration), data = val.frame, shape = 21, fill = "red", color = "black", alpha = 0.25) +
          geom_smooth(aes(x=IntensityNorm, y=Concentration, ymin = Lower, ymax = Upper)) +
          scale_x_continuous(paste(element.name, norma)) +
          scale_y_continuous(paste(element.name, conen)) +
          coord_cartesian(xlim = rangescalcurverandom$x, ylim = rangescalcurverandom$y, expand = TRUE)
      }
      
      if(input$radiocal==4){
          val.frame <- valFrameRandomizedRev()
          
          calcurve.plot <- ggplot(data=val.frame, aes(IntensityNorm, Concentration)) +
          theme_light() +
          annotate("text", label=lm_eqn(lm(Concentration~., val.frame)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
          geom_point() +
          geom_point(aes(IntensityNorm, Concentration), data = val.frame, shape = 21, fill = "red", color = "black", alpha = 0.25) +
          geom_smooth() +
          scale_x_continuous(paste(element.name, norma)) +
          scale_y_continuous(paste(element.name, conen)) +
          coord_cartesian(xlim = rangescalcurverandom$x, ylim = rangescalcurverandom$y, expand = TRUE)
      }
      
      if(input$radiocal==5){
          val.frame <- valFrameRandomizedRev()
          
          calcurve.plot <- ggplot(data=val.frame, aes(IntensityNorm, Concentration)) +
          theme_light() +
          annotate("text", label=lm_eqn(lm(Concentration~., val.frame)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
          geom_point() +
          geom_point(aes(IntensityNorm, Concentration), data = val.frame, shape = 21, fill = "red", color = "black", alpha = 0.25) +
          geom_smooth() +
          scale_x_continuous(paste(element.name, norma)) +
          scale_y_continuous(paste(element.name, conen)) +
          coord_cartesian(xlim = rangescalcurverandom$x, ylim = rangescalcurverandom$y, expand = TRUE)
      }
      
      calcurve.plot
      
      
  })
  
  observeEvent(input$cropcalrandom, {
      brush <- input$plot_cal_brush_random
      if (!is.null(brush)) {
          rangescalcurverandom$x <- c(brush$xmin, brush$xmax)
          rangescalcurverandom$y <- c(brush$ymin, brush$ymax)
          
      } else {
          rangescalcurverandom$x <- NULL
          rangescalcurverandom$y <- NULL
      }
  })
  
  
  
  output$calcurveplotsrandom <- renderPlot({
      calCurvePlotRandom()
  })
  
  
  rangesvalcurverandom <- reactiveValues(x = NULL, y = NULL)
  
  valCurvePlotRandom <- reactive({
      
      
      
      element.name <- gsub("[.]", "", substr(input$calcurveelement, 1, 2))
      intens <- " Counts per Second"
      norma <- " Normalized"
      norma.comp <- " Compton Normalized"
      norma.tc <- " Valid Counts Normalized"
      conen <- " (%)"
      predi <- " Estimate (%)"
      
      intensity.name <- c(element.name, intens)
      concentration.name <- c(element.name, conen)
      prediction.name <- c(element.name, predi)
      
      val.frame <- valFrameRandomized()
      
      valcurve.plot <- ggplot(data=val.frame, aes(Prediction, Concentration)) +
      theme_bw() +
      annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
      geom_abline(intercept=0, slope=1, lty=2) +
      stat_smooth(method="lm") +
      geom_point() +
      geom_point(aes(Prediction, Concentration),  data = val.frame, shape = 21, fill = "red", color = "black", alpha = 0.25) +
      scale_x_continuous(paste(element.name, predi)) +
      scale_y_continuous(paste(element.name, conen)) +
      coord_cartesian(xlim = rangesvalcurverandom$x, ylim = rangesvalcurverandom$y, expand = TRUE)
      
      valcurve.plot
      
  })
  
  observeEvent(input$cropvalrandom, {
      brush <- input$plot_val_brush_random
      if (!is.null(brush)) {
          rangesvalcurverandom$x <- c(brush$xmin, brush$xmax)
          rangesvalcurverandom$y <- c(brush$ymin, brush$ymax)
          
      } else {
          rangesvalcurverandom$x <- NULL
          rangesvalcurverandom$y <- NULL
      }
  })
  
  output$valcurveplotsrandom <- renderPlot({
      valCurvePlotRandom()
  })
  
  
  ####CalCurves
  
  # Float over info
  output$hover_infocal <- renderUI({
      
      point.table <- if(calType()==1){
          calCurveFrame()
      } else if(calType()==2){
          calCurveFrame()
      } else if(calType()==3) {
          calValFrame()
      } else if(calType()==5) {
          calValFrame()
      }
      
      concentration.table <- concentrationTable()
      hold.table <- concentration.table[,c("Spectrum", input$calcurveelement)]
      colnames(hold.table) <- c("Spectrum", "Selection")
      hold.table$Selection[hold.table$Selection==""] <- NA
      hold.table <- hold.table[complete.cases(hold.table), ]
      
      point.table$Spectrum <- hold.table["Spectrum"]
      
      hover <- input$plot_hovercal
      point <- nearPoints(point.table,  coordinfo=hover,   threshold = 5, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) return(NULL)
      
      
      
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
      style = style,
      p(HTML(paste0(point$Spectrum
      
      )))
      )
  })
  
  
  # Float over info
  output$hover_infocal_random <- renderUI({
      
      point.table <- if(calType()==1){
          calCurveFrame()
      } else if(calType()==2){
          calCurveFrame()
      } else if(calType()==3) {
          calValFrame()
      } else if(calType()==5) {
          calValFrame()
      }
      
      randomized <- randomizeData()
      
      
      point.table <- point.table[ vals$keeprows, , drop = FALSE]
      point.table <- point.table[randomized,]
      
      
      concentration.table <- concentrationTable()
      
      concentration.table <- concentration.table[ vals$keeprows, , drop = FALSE]
      concentration.table <- concentration.table[randomized,]
      
      hold.table <- concentration.table[,c("Spectrum", input$calcurveelement)]
      colnames(hold.table) <- c("Spectrum", "Selection")
      
      
      point.table$Spectrum <- hold.table["Spectrum"]
      
      hover <- input$plot_hovercal_random
      point <- nearPoints(point.table,  coordinfo=hover,   threshold = 5, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) return(NULL)
      
      
      
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
      style = style,
      p(HTML(paste0(point$Spectrum
      
      )))
      )
  })
  
  
  # Toggle points that are clicked
  observeEvent(input$plot_cal_click, {
      
      predict.frame <- if(calType()==1){
          calCurveFrame()
      } else if(calType()==2){
          calCurveFrame()
      } else if(calType()==3) {
          calValFrame()
      } else if(calType()==5) {
          calValFrame()
      }
      
      res <- nearPoints(predict.frame, input$plot_cal_click, allRows = TRUE)
      
      vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  
  # Toggle points that are brushed, when button is clicked
  observeEvent(input$exclude_toggle, {
      
      predict.frame <- if(calType()==3){
          calCurveFrame()
      } else if(calType()==2){
          calCurveFrame()
      } else if(calType()==3) {
          calValFrame()
      } else if(calType()==5) {
          calValFrame()
      }
      res <- brushedPoints(predict.frame, input$plot_cal_brush, allRows = TRUE)
      
      vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Reset all points
  observeEvent(input$exclude_reset, {
      
      predict.frame <- if(calType()==1){
          calCurveFrame()
      } else if(calType()==2){
          calCurveFrame()
      } else if(calType()==3) {
          calValFrame()
      } else if(calType()==5) {
          calValFrame()
      }
      vals$keeprows <- rep(TRUE, nrow(predict.frame))
  })
  
  # Reset all points on element change
  observeEvent(input$calcurveelement, {
      
      
      
      vals$keeprows <- calFileStandards()
      
      
      
  })
  
  
  
  
  
  ####ValCurves
  
  
  
  
  # Float over info
  output$hover_infoval <- renderUI({
      
      point.table <- calValFrame()
      concentration.table <- concentrationTable()
      hold.table <- concentration.table[,c("Spectrum", input$calcurveelement)]
      colnames(hold.table) <- c("Spectrum", "Selection")
      hold.table$Selection[hold.table$Selection==""] <- NA
      hold.table <- hold.table[complete.cases(hold.table), ]
      
      point.table$Spectrum <- hold.table["Spectrum"]
      
      
      hover <- input$plot_hoverval
      point <- nearPoints(point.table,  coordinfo=hover,   threshold = 5, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) return(NULL)
      
      
      
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
      style = style,
      p(HTML(paste0(point$Spectrum
      
      )))
      )
  })
  
  
  output$hover_infoval_random <- renderUI({
      
      point.table <- calValFrame()
      
      randomized <- randomizeData()
      
      
      point.table <- point.table[ vals$keeprows, , drop = FALSE]
      point.table <- point.table[!(randomized),]
      
      concentration.table <- concentrationTable()
      
      concentration.table <- concentration.table[ vals$keeprows, , drop = FALSE]
      concentration.table <- concentration.table[!(randomized),]
      concentration.table.rev <- concentration.table[(randomized),]
      
      hold.table <- concentration.table[,c("Spectrum", input$calcurveelement)]
      colnames(hold.table) <- c("Spectrum", "Selection")
      
      
      point.table$Spectrum <- hold.table["Spectrum"]
      
      
      point.table <- point.table[point.table$Concentration > min(concentration.table.rev[,input$calcurveelement], na.rm = TRUE) & point.table$Concentration < max(concentration.table.rev[,input$calcurveelement], na.rm = TRUE), ]
      
      
      
      hover <- input$plot_hoverval_random
      point <- nearPoints(point.table,  coordinfo=hover,   threshold = 5, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) return(NULL)
      
      
      
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
      style = style,
      p(HTML(paste0(point$Spectrum
      
      )))
      )
  })
  
  # Toggle points that are clicked
  observeEvent(input$plot_val_click, {
      
      predict.frame <- calValFrame()
      
      res <- nearPoints(predict.frame, input$plot_val_click, allRows = TRUE)
      
      vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  
  # Toggle points that are brushed, when button is clicked
  observeEvent(input$exclude_toggle, {
      predict.frame <- calValFrame()
      
      res <- brushedPoints(predict.frame, input$plot_val_brush, allRows = TRUE)
      
      vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Reset all points
  observeEvent(input$exclude_reset, {
      predict.frame <- calValFrame()
      
      vals$keeprows <- rep(TRUE, nrow(predict.frame))
  })
  
  
  normalLM <- reactive({
      
      
      model <- elementModel()
      
      model.frame <- as.data.frame(augment(model))
      
      model.frame$qq <- qqnorm(model.frame$.std.resid)[[1]]
      
      model.frame$sqrt.std.resid <- sqrt(abs(model.frame$.std.resid))
      
      model.frame$seq.cooksd <- seq_along(model.frame$.cooksd)
      
      #model.frame$Spectrum <- predictFrameName()$Spectrum
      
      
      
      model.frame
      
  })
  
  
  forestLM <- reactive({
      
      
      model <- lm(Concentration~Prediction, data=as.data.frame(calValTable()))
      
      model.frame <- as.data.frame(augment(model))
      
      model.frame$qq <- qqnorm(model.frame$.std.resid)[[1]]
      
      model.frame$sqrt.std.resid <- sqrt(abs(model.frame$.std.resid))
      
      model.frame$seq.cooksd <- seq_along(model.frame$.cooksd)
      
      #model.frame$Spectrum <- predictFrameName()$Spectrum
      
      
      
      model.frame
      
      
  })
  
  
  modelFrame <- reactive({
      
      if(input$radiocal==1){
          normalLM()
      } else if(input$radiocal==2){
          normalLM()
      } else if(input$radiocal==3){
          normalLM()
      } else if(input$radiocal==4){
          forestLM()
      } else if(input$radiocal==5){
          forestLM()
      }
      
      
  })
  
  
  
  
  
  diagResidualsFitted <- reactive({
      
      model <- modelFrame()
      
      p1 <- ggplot(model[ vals$keeprows, , drop = FALSE], aes(.fitted, .resid)) +
      stat_smooth(method="loess") +
      geom_hline(yintercept=0, col="red", linetype="dashed") +
      xlab("Fitted values") +
      ylab("Residuals") +
      ggtitle("Residual vs Fitted Plot") +
      theme_light() +
      geom_point() +
      geom_point(data=model[ !vals$keeprows, , drop = FALSE], aes(.fitted, .resid), shape = 21, fill = "red", color = "black", alpha = 0.25)
      
      p1
      
  })
  
  output$residualsfitted <- renderPlot({
      
      diagResidualsFitted()
      
  })
  
  
  diagQQ <- reactive({
      
      model <- modelFrame()
      
      p2 <- ggplot(model[ vals$keeprows, , drop = FALSE], aes(qq, .std.resid))+geom_point(na.rm = TRUE) +
      geom_abline() +
      xlab("Theoretical Quantiles") +
      ylab("Standardized Residuals") +
      ggtitle("Normal Q-Q") +
      theme_light() +
      geom_point(data=model[ !vals$keeprows, , drop = FALSE], aes(qq, .std.resid), shape = 21, fill = "red", color = "black", alpha = 0.25)
      
      
      p2
      
  })
  
  
  output$qq <- renderPlot({
      
      diagQQ()
      
  })
  
  diagScaleLocation <- reactive({
      
      model <- modelFrame()
      
      
      p3 <- ggplot(model[ vals$keeprows, , drop = FALSE], aes(.fitted, sqrt.std.resid)) +
      stat_smooth(method="loess", na.rm = TRUE) +
      xlab("Fitted Value") +
      ylab(expression(sqrt("|Standardized residuals|"))) +
      ggtitle("Scale-Location") +
      theme_light() +
      geom_point(na.rm=TRUE) +
      geom_point(data=model[ !vals$keeprows, , drop = FALSE], aes(.fitted, sqrt.std.resid), shape = 21, fill = "red", color = "black", alpha = 0.25)
      
      
      p3
      
      
  })
  
  
  output$scalelocation <- renderPlot({
      
      diagScaleLocation()
      
  })
  
  
  diagCooksDistance <- reactive({
      
      model <- modelFrame()
      
      p4 <- ggplot(model, aes(seq.cooksd, .cooksd)) +
      geom_bar(stat="identity", position="identity") +
      xlab("Obs. Number") +
      ylab("Cook's distance") +
      ggtitle("Cook's distance") +
      theme_light()
      
      p4
      
  })
  
  output$cooksdistance <- renderPlot({
      
      diagCooksDistance()
      
  })
  
  
  diagResidualLeverage <- reactive({
      
      model <- modelFrame()
      
      
      p5<-ggplot(model[ vals$keeprows, , drop = FALSE], aes(.hat, .std.resid))+
      geom_point(aes(size=.cooksd), na.rm=TRUE) +
      geom_point(data=model[ !vals$keeprows, , drop = FALSE], aes(.hat, .std.resid), shape = 21, fill = "red", color = "black", alpha = 0.25) +
      stat_smooth(method="loess", na.rm=TRUE) +
      xlab("Leverage") +
      ylab("Standardized Residuals") +
      ggtitle("Residual vs Leverage Plot") +
      scale_size_continuous("Cook's Distance", range=c(1,5)) +
      theme_light() +
      theme(legend.position="bottom")
      
      p5
      
  })
  
  output$residualleverage <- renderPlot({
      
      diagResidualLeverage()
      
  })
  
  
  diagCooksLeverage <- reactive({
      
      model <- modelFrame()
      
      
      p6 <- ggplot(model[ vals$keeprows, , drop = FALSE], aes(.hat, .cooksd)) +
      stat_smooth(method="loess", na.rm=TRUE) +
      xlab("Leverage hii") +
      ylab("Cook's Distance") +
      ggtitle("Cook's dist vs Leverage hii/(1-hii)") +
      geom_abline(slope=seq(0,3,0.5), color="gray", linetype="dashed") +
      theme_light() +
      geom_point(na.rm=TRUE) +
      geom_point(data=model[ vals$keeprows, , drop = FALSE], aes(.hat, .cooksd), shape = 21, fill = "red", color = "black", alpha = 0.25)
      
      p6
      
  })
  
  
  output$cooksleverage <- renderPlot({
      
      diagCooksLeverage()
      
  })
  
  
  diagPlotDownload <- reactive({
      
      grid.arrange(diagResidualsFitted(), diagQQ(),
      diagScaleLocation(), diagCooksDistance(),
      diagResidualLeverage(), diagCooksLeverage(),
      ncol=2, nrow=3)
      
  })
  
  
  output$diagplots <- downloadHandler(
  filename = function() { paste(input$calname, "_", input$calcurveelement, "_diag", ".tiff", sep='') },
  content = function(file) {
      ggsave(file,diagPlotDownload(), width=10, height=10, device="tiff", compression="lzw", type="cairo", dpi=300, )
  }
  )
  
  #########Diagnostic Plot Controls#######
  ####Residuals Fitted
  # Float over info
  output$hover_inforesidualsfitted <- renderUI({
      
      point.table <- modelFrame()
      
      
      hover <- input$plot_hoverresidualsfitted
      point <- nearPoints(point.table,  coordinfo=hover,   threshold = 5, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) return(NULL)
      
      
      
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
      style = style,
      p(HTML(paste0(point$Spectrum
      
      )))
      )
  })
  
  # Toggle points that are clicked
  observeEvent(input$plot_residualsfitted_click, {
      
      predict.frame <- modelFrame()
      
      res <- nearPoints(predict.frame, input$plot_residualsfitted_click, allRows = TRUE)
      
      vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  
  # Toggle points that are brushed, when button is clicked
  observeEvent(input$exclude_toggle_diag, {
      
      predict.frame <- modelFrame()
      
      res <- brushedPoints(predict.frame, input$plot_residualsfitted_brush, allRows = TRUE)
      
      vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  
  ####QQ Norm
  # Float over info
  output$hover_infoqq <- renderUI({
      
      point.table <- modelFrame()
      
      
      hover <- input$plot_hoverqq
      point <- nearPoints(point.table,  coordinfo=hover,   threshold = 5, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) return(NULL)
      
      
      
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
      style = style,
      p(HTML(paste0(point$Spectrum
      
      )))
      )
  })
  
  # Toggle points that are clicked
  observeEvent(input$plot_qq_click, {
      
      predict.frame <- modelFrame()
      
      res <- nearPoints(predict.frame, input$plot_qq_click, allRows = TRUE)
      
      vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  
  # Toggle points that are brushed, when button is clicked
  observeEvent(input$exclude_toggle_diag, {
      
      predict.frame <- modelFrame()
      
      res <- brushedPoints(predict.frame, input$plot_qq_brush, allRows = TRUE)
      
      vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  
  ####Scaled Residuals
  # Float over info
  output$hover_infoscalelocation <- renderUI({
      
      point.table <- modelFrame()
      
      
      hover <- input$plot_hoverscalelocation
      point <- nearPoints(point.table,  coordinfo=hover,   threshold = 5, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) return(NULL)
      
      
      
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
      style = style,
      p(HTML(paste0(point$Spectrum
      
      )))
      )
  })
  
  # Toggle points that are clicked
  observeEvent(input$plot_scalelocation_click, {
      
      predict.frame <- modelFrame()
      
      res <- nearPoints(predict.frame, input$plot_scalelocation_click, allRows = TRUE)
      
      vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  
  # Toggle points that are brushed, when button is clicked
  observeEvent(input$exclude_toggle_diag, {
      
      predict.frame <- modelFrame()
      
      res <- brushedPoints(predict.frame, input$plot_scalelocation_brush, allRows = TRUE)
      
      vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  
  ####Residuals Leverage
  # Float over info
  output$hover_inforesidualleverage <- renderUI({
      
      point.table <- modelFrame()
      
      
      hover <- input$plot_hoverresidualleverage
      point <- nearPoints(point.table,  coordinfo=hover,   threshold = 5, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) return(NULL)
      
      
      
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
      style = style,
      p(HTML(paste0(point$Spectrum
      
      )))
      )
  })
  
  # Toggle points that are clicked
  observeEvent(input$plot_residualleverage_click, {
      
      predict.frame <- modelFrame()
      
      res <- nearPoints(predict.frame, input$plot_residualleverage_click, allRows = TRUE)
      
      vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  
  # Toggle points that are brushed, when button is clicked
  observeEvent(input$exclude_toggle_diag, {
      
      predict.frame <- modelFrame()
      
      res <- brushedPoints(predict.frame, input$plot_residualleverage_brush, allRows = TRUE)
      
      vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  
  ####Cooks Leverage
  # Float over info
  output$hover_infocooksleverage <- renderUI({
      
      point.table <- modelFrame()
      
      
      hover <- input$plot_hovercooksleverage
      point <- nearPoints(point.table,  coordinfo=hover,   threshold = 5, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) return(NULL)
      
      
      
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
      style = style,
      p(HTML(paste0(point$Spectrum
      
      )))
      )
  })
  
  # Toggle points that are clicked
  observeEvent(input$plot_cooksleverage_click, {
      
      predict.frame <- modelFrame()
      
      res <- nearPoints(predict.frame, input$plot_cooksleverage_click, allRows = TRUE)
      
      vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  
  # Toggle points that are brushed, when button is clicked
  observeEvent(input$exclude_toggle_diag, {
      
      predict.frame <- modelFrame()
      
      res <- brushedPoints(predict.frame, input$plot_cooksleverage_brush, allRows = TRUE)
      
      vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  
  
  # Reset all points
  observeEvent(input$exclude_reset_diag, {
      
      predict.frame <- modelFrame()
      
      vals$keeprows <- rep(TRUE, nrow(predict.frame))
  })
  
  
  
  
  #output$downloadcal <- downloadHandler(
  #filename = function() { paste(input$dataset, '.csv', sep=',') },
  #content = function(file
  #) {
  #  write.csv(metadataForm(), file)
  # }
  #)
  
  
  
  ####alternative take
  
  nullList <- reactive({
      
      spectra.line.table <- spectraData()
      
      cal.vector <- elementallinestouse()
      cal.vector2 <- cal.vector[2:length(cal.vector)]
      cal.list <- as.list(cal.vector2)
      setNames(cal.list, cal.vector2)
      cal.list <- pblapply(cal.list, function(x) return(NULL))
      nullList <- cal.list
      
      
  })
  
  
  
  
  #rf2 <- reactiveValues()
  #observe({
  #    if(input$createcalelement > 0){
  #    calList[[input$calcurveelement]] <- elementModel()
  #    }
  #    rf2 <<- calList
  #})
  
  emptyList <- reactive({
      a.list <- list()
      a.list
  })
  
  
  
  
  
  calList <- reactiveValues()
  
  observeEvent(input$actionprocess, {
      isolate(calList <- emptyList())
      calList <<- calList
  })
  
  
  
  
  observeEvent(input$createcalelement, {
      
      cal.condition <- input$radiocal
      norm.condition <- input$normcal
      
      norm.min <- print(input$comptonmin)
      norm.max <- print(input$comptonmax)
      
      cal.table <- data.frame(cal.condition, norm.condition, norm.min, norm.max)
      colnames(cal.table) <- c("CalType", "NormType", "Min", "Max")
      
      slope.corrections <- input$slope_vars
      intercept.corrections <- input$intercept_vars
      
      standards.used <- vals$keeprows
      
      cal.mode.list <- list(cal.table, slope.corrections, intercept.corrections, standards.used)
      names(cal.mode.list) <- c("CalTable", "Slope", "Intercept", "StandardsUsed")
      
      calConditons <<- cal.mode.list
      
  })
  
  
  calList <- reactiveValues()
  observeEvent(input$createcalelement, {
      
      
      calList[[input$calcurveelement]] <- list(isolate(calConditons), isolate(strip_glm(elementModel())))
      
      calList <<- calList
      
  })
  
  calPlotList <- reactiveValues()
  calPlotList <- emptyList()
  observeEvent(input$createcalelement, {
      
      
      calPlotList[[input$calcurveelement]] <- isolate(calPlotDownload())
      
      calPlotList <<- calPlotList
      
  })
  
  diagPlotList <- reactiveValues()
  diagPlotList <- emptyList()
  #observeEvent(input$createcalelement, {
  
  
  #diagPlotList[[input$calcurveelement]] <- isolate(diagPlotDownload())
  
  #diagPlotList <<- diagPlotList
  
  #})
  
  Calibration <- reactiveValues()
  observeEvent(input$createcal, {
      
      
      spectra.line.table <- if(dataType()=="Spectra"){
          spectraData()
      } else if(dataType()=="Net"){
          dataHold()
      }
      cal.intensities <- spectra.line.table[elementallinestouse()]
      cal.values <- values[["DF"]]
      cal.data <- if(dataType()=="Spectra"){
          dataHold()
      } else if(dataType()=="Net"){
          myData()
      }
      
      
      dataHold()
      
      calibrationList <- NULL
      calibrationList <- list(input$filetype, input$calunits, cal.data, cal.intensities, cal.values, calList)
      names(calibrationList) <- c("FileType", "Units", "Spectra", "Intensities", "Values", "calList")
      
      Calibration <<- calibrationList
      
      
  })
  
  CalibrationPlots <- reactiveValues()
  observeEvent(input$createcal, {
      
      CalibrationPlots$calCurves <<- calPlotList
      
      
  })
  
  #observeEvent(input$createcal, {
  
  #CalibrationPlots$diagPlots <<- diagPlotList
  
  
  #})
  
  
  
  output$downloadModel <- downloadHandler(
  filename <- function(){
      paste(input$calname, "quant", sep=".")
  },
  
  content = function(file) {
      saveRDS(Calibration, file = file, compress="xz")
  }
  )
  
  
  output$downloadReport <- downloadHandler(
  function() { paste(paste(c(input$calname), collapse=''), '.pdf',  sep='') },
  content = function(file){
      ml = marrangeGrob(grobs=CalibrationPlots$calCurves, nrow=1, ncol=1)
      ggsave(file, ml, device="pdf", dpi=300, width=12, height=7)
      
      dev.off()
  })





    
 })



 })
