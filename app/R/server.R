server <- function(input, output, session) {
  
  ##############################################################################
  ###################################### data ##################################
  ##############################################################################
  # load data
  response <- reactive({
    req(input$responsefile)
    responseFile(input$responsefile)})
  features <- reactive({
    req(input$featuresfile)
    featuresFile(input$featuresfile)})
  codata <- reactive({
    req(input$codatafile)
    codataFile(input$codatafile)})
  
  # response file flag for reponse type button
  output$responsefileflag <- reactive({isTruthy(input$responsefile)})
  outputOptions(output, "responsefileflag", suspendWhenHidden=FALSE)
  
  # show head of file for visual check
  output$checkresponse <- renderTable({
    tab <- rbind(round(as.matrix(head(response()$y)), 3), 
                 c("...", rep(" ", ncol(as.matrix(head(response()$y))) - 1)),
                 round(as.matrix(tail(response()$y, n=1L)), 3))
    if(!is.null(response()$unpen)) {
      if(ncol(response()$unpen) < 6) {
        tab <- cbind(tab, 
                     rbind(round(as.matrix(head(response()$unpen)), 3), " ",
                           round(as.matrix(tail(response()$unpen, n=1L)), 3)))
      } else {
        tab <- cbind(tab, 
                     rbind(round(as.matrix(head(response()$unpen[, 1:5])), 3), 
                           " ",
                           round(as.matrix(tail(response()$unpen[, 1:5], n=1L)), 
                                 3)),
                     " "=c("...", rep("", 7)),
                     rbind(round(as.matrix(head(response()$unpen[, ncol(
                       response()$unpen), drop=FALSE])), 3), " ",
                           round(as.matrix(tail(response()$unpen[, ncol(
                             response()$unpen), drop=FALSE], n=1L)), 3)))
      }
    }
    return(tab)
    }, rownames=reactive({!is.null(rownames(response()$y))}))
  output$checkfeatures <- renderTable({
    tab <- rbind(round(head(features()[, 1:min(6, ncol(features()))]), 3),
                 " "=c("...", rep("", min(6, ncol(features())) - 1)), 
                 round(tail(features()[, 1:min(6, ncol(features()))], n=1L), 3))
    tab <- cbind(tab, " "=c("...", rep("", 7)),
                 rbind(round(head(features()[, ncol(features()), drop=FALSE]),
                             3), " ",
                       round(tail(features()[, ncol(features()), drop=FALSE], 
                                  n=1L), 3)))
    return(tab)}, 
    rownames=reactive({!is.null(rownames(features()))}))
  output$checkcodata <- renderTable({
    tab <- apply(codata(), 2, function(x) {
      if(!suppressWarnings(any(is.na(as.numeric(x))))) {
        round(as.numeric(x), 3)
      } else {
        x
      }})
    rownames(tab) <- rownames(codata())
    tab <- rbind(head(tab), " "=c("...", rep("", ncol(tab) - 1)),
                 tail(tab, n=1L))
    return(tab)}, rownames=reactive({!is.null(rownames(codata()))}))
  
  # show codata type action button if codata uploaded
  output$codatatypes <- renderUI({
    lapply(1:ncol(codata()), function(i) {
      tagList(
        titlePanel(colnames(codata())[i]),
        fluidRow(
          column(6, radioButtons(
            paste0("codatatype", colnames(codata())[i]), "type",
            choices=c("continuous"="continuous", "categorical"="categorical"))),
          conditionalPanel(
            condition=paste0("input.codatatype", colnames(codata())[i], 
                             " == 'continuous'"), {
            column(6, radioButtons(
              paste0("split", colnames(codata())[i]), "split",
              choices=c("both"="both", "lower"="lower")))})))})})
  
  # import settings
  settings <- reactive({
    setNames(lapply(which(substr(names(input), 1, 8)=="settings"), function(i) {
      input[[names(input)[i]]]}), 
      names(input)[substr(names(input), 1, 8)=="settings"])})
  
  # update choices based on loaded data
  choices1 <- list("deviance"="deviance", "mse"="mse", "class"="class",
                   "auc"="auc", "mae"="mae", "C"="C")
  observe({updateCheckboxGroupInput(
    session, "cvmeasures", label="calculated CV measures", 
    choices=switch(input$responsetype,
                   gaussian={choices1[c(1, 2, 5)]},
                   binomial={choices1[c(1, 2, 3, 4, 5)]},
                   cox={choices1[c(1, 6)]}),
    selected=switch(input$responsetype,
                    gaussian={choices1[c(1, 2, 5)]},
                    binomial={choices1[c(1, 2, 3, 4, 5)]},
                    cox={choices1[c(1, 6)]}))})
  choices2 <- list("enet"="enet", "gren"="gren", "ecpc"="ecpc")
  observe({updateCheckboxGroupInput(
    session, "methods", label="methods", 
    choices=switch(input$responsetype,
                   gaussian={choices2[c(1, 3)]},
                   binomial={choices2[c(1, 2, 3)]},
                   cox={choices2[c(1, 3)]}),
    selected=switch(input$responsetype,
                    gaussian={choices2[c(1, 3)]},
                    binomial={choices2[c(1, 2, 3)]},
                    cox={choices2[c(1, 3)]}))})
  
  ##############################################################################
  ##################################### models #################################
  ##############################################################################
  # fit models
  fit <- eventReactive(input$fit, {
    withProgress(message = 'Fitting models', value=0.3, {
    fitFunction(response=response(), features=features(), codata=codata(),
                methods=input$methods, settings=settings(),
                codatatype=reactiveValuesToList(input)[
                  substr(names(input), 1, 10)=="codatatype"],
                split=reactiveValuesToList(input)[
                  substr(names(input), 1, 5)=="split"],
                responsetype=input$responsetype)})})
  
  # cross-validate models
  cv <- eventReactive(input$cv, {
    withProgress(message = 'Cross-validating models', value = 0, {
    cvFunction(response=response(), features=features(), codata=codata(), 
               methods=input$methods, settings=settings(), 
               codatatype=reactiveValuesToList(input)[
                 substr(names(input), 1, 10)=="codatatype"], 
               split=reactiveValuesToList(input)[
                 substr(names(input), 1, 5)=="split"],
               responsetype=input$responsetype,
               nfolds=input$nfolds, cvmeasures=input$cvmeasures)})})
  
  # download fitted models
  observeEvent(input$fit, {
    output$modelsfile <- downloadHandler(
      filename=function() {
        paste0("fit-", gsub(" ", "-", Sys.time()), ".Rdata")},
      content=function(file) {
        fit <- fit()
        save(fit, file=file)})
    output$modelsdownload <- renderUI({
      req(fit())
      downloadButton("modelsfile", label="Save models")})})
  ##############################################################################
  #################################### output ##################################
  ##############################################################################
  # codata weights plot
  observeEvent(input$fit, {
    output$multplotsflag <- reactive(("ecpc" %in% names(fit())) |
                                       ("gren" %in% names(fit())))
    outputOptions(output, "multplotsflag", suspendWhenHidden=FALSE)
    output$multplots <- renderPlot({
      multplots(fit(), codatatype=isolate({reactiveValuesToList(input)[
        substr(names(input), 1, 10)=="codatatype"]}),
        codata=isolate(codata()))})
    output$multplotsfile <- downloadHandler(
      filename=function() {
        paste0("weights-", gsub(" ", "-", Sys.time()), ".png")},
      content=function(file) {
        png(file)
        multplots(fit(), codatatype=isolate({reactiveValuesToList(input)[
          substr(names(input), 1, 10)=="codatatype"]}),
          codata=isolate({codata()}))
        dev.off()})
    output$multplotsdownload <- renderUI({
      if(("ecpc" %in% names(fit())) | ("gren" %in% names(fit()))) {
        downloadButton("multplotsfile", label="Save")}})})

  # selected variables table
  observeEvent(input$fit, {
    output$selvar <- renderPrint({selvar(fit(), isolate({features()}),
                                         isolate({response()}))})
    output$selvarfile <- downloadHandler(
      filename=function() {
        paste0("selvar-", gsub(" ", "-", Sys.time()), ".Rdata")},
      content=function(file) {
        sv <- selvar(fit(), isolate({features()}), isolate({response()}))
        save(sv, file=file)})
    output$selvardownload <- renderUI({
      req(fit(), features())
      downloadButton("selvarfile", label="Save")})})

  # cv measures tables
  observeEvent(input$cv, {
    output$cvtable <- renderTable({cv()$measures}, rownames=TRUE)
    output$cvtablefile <- downloadHandler(
      filename=function() {
        paste0("cvtable-", gsub(" ", "-", Sys.time()), ".csv")},
      content=function(file) {
        write.csv(cv()$measures, file=file)})
    output$cvtabledownload <- renderUI({
      req(cv())
      downloadButton("cvtablefile", label="Save")})})

  # ROC plot
  observeEvent(input$cv, {
    output$rocplots <- renderPlot({
      if(req(input$responsetype)=="binomial") {
        rocplots(cv(), response())
      }})
    output$rocplotsfile <- downloadHandler(
      filename=function() {
        paste0("roc-", gsub(" ", "-", Sys.time()), ".png")},
      content=function(file) {
        png(file)
        rocplots(cv(), response())
        dev.off()})
    output$rocplotsdownload <- renderUI({
      req(cv())
      downloadButton("rocplotsfile", label="Save")})
  })
  
  ############################# test checks
  # output$multsecpccheck <- renderPrint({fit()$ecpc$gamma})
  # output$multsgrencheck <- renderPrint({fit()$gren$lambdag})
  # output$hypershrinkagecheck <- renderPrint({input$settingsecpchypershrinkage})
  # output$datacheck <- renderPrint({response()$family})
  # output$fitcheck <- renderPrint({lapply(fit(), function(l) {l})})
  # output$codatatypescheck <- renderPrint({reactiveValuesToList(input)[
  #   substr(names(input), 1, 10)=="codatatype"]})
  # output$methodscheck <- renderPrint({input$methods})
  # output$settingscheck <- renderPrint({settings()})
  # output$splitcheck <- renderPrint({
  #   reactiveValuesToList(input)[substr(names(input), 1, 5)=="split"]})
  # output$responsetypecheck <- renderPrint({input$responsetype})
  # output$cvcheck <- renderPrint({cv()})
  # output$cvmeasures <- renderPrint({input$cvmeasures})
  # output$nfolds <- renderPrint({input$nfolds})
  #############################

}