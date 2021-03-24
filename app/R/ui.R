# load libraries
library(shiny)
library(shinydashboard)
library(shinyBS)
library(glmnet)
library(gren)
library(ecpc)
library(openxlsx)
library(survival)
library(shinyWidgets)
library(pROC)

# UI function
ui <- navbarPage(
  "co-data learning",
  
################################################################################
############################## data uploading tab ##############################
################################################################################
  tabPanel(
    "data",
    sidebarLayout(
      sidebarPanel(
        img(src="data.png", width="100%"),
        titlePanel("upload data"),
        "Note: Decimal separator is '.' (decimal point), ",
        "and no header for rownames column",
        # radioButtons("decimal", "decimal separator", 
        #              choices=c("."="point", ","="comma"), selected="point"),
        fileInput("responsefile", "response (+ unpenalized)", multiple=FALSE),
        bsTooltip(id="responsefile", title=tooltips$responsefile,
                  placement="right", trigger="hover"),
        fileInput("featuresfile", "features", multiple=FALSE),
        bsTooltip(id="featuresfile", title=tooltips$featuresfile,
                  placement="right", trigger="hover"),
        fileInput("codatafile", "codata", multiple=FALSE),
        bsTooltip(id="codatafile", title=tooltips$codatafile,
                  placement="right", trigger="hover"),
        conditionalPanel(
          "output.responsefileflag == true",
          titlePanel("response"),
          radioButtons(
            "responsetype", label="type",
            choices=c("gaussian"="gaussian", "binomial"="binomial", 
                      "cox"="cox"), selected="gaussian"),
          bsTooltip(id="responsetype", title=tooltips$responsetype,
                    placement="right", trigger="hover")),
        uiOutput("codatatypes"),
        bsTooltip(id="codatatypes", title=tooltips$codatatypes,
                  placement="right", trigger="hover")),
      mainPanel(
        tableOutput("checkresponse"),
        tableOutput("checkfeatures"),
        tableOutput("checkcodata")))),
################################################################################
################################# settings tab #################################
################################################################################
  tabPanel(
    "settings",
    sidebarLayout(
      sidebarPanel(
        titlePanel("general settings"),
        checkboxGroupInput(
          "methods", label="methods",
          choices=list("enet"="enet", "gren"="gren", "ecpc"="ecpc"),
          selected=c("enet")),
        bsTooltip(id="methods", title=tooltips$methods,
                  placement="right", trigger="hover"),
        numericInput("nfolds", "number of CV folds", 5),
        bsTooltip(id="nfolds", title=tooltips$nfolds,
                  placement="right", trigger="hover"),
        checkboxGroupInput(
          "cvmeasures", label="calculated CV measures",
          choices=list("deviance"="deviance", "mse"="mse", "class"="class",
                       "auc"="auc", "mae"="mae", "C"="C"),
          selected="deviance"),
        bsTooltip(id="cvmeasures", title=tooltips$cvmeasures,
                  placement="right", trigger="hover")),
      mainPanel(
        settingsUI(
          "enet", 
          settings=list(alpha=0.5, standardize=TRUE, intercept=TRUE)),
        settingsUI(
          "gren", 
          settings=list(alpha=0.5, intercept=TRUE)),
        settingsUI(
          "ecpc", 
          settings=list(intrcpt=TRUE, maxsel=10))))),
################################################################################
################################## output tab ##################################
################################################################################
  tabPanel(
    "output",
    sidebarLayout(
      sidebarPanel(
        titlePanel("controls"),
        actionButton("fit", label="fit"),
        actionButton("cv", label="CV"),
        uiOutput("modelsdownload")),
      mainPanel(      
        ### some checks
        # uiOutput("multsecpccheck"),
        # uiOutput("multsgrencheck"),
        # uiOutput("hypershrinkagecheck"),
        # uiOutput("datacheck"),
        # uiOutput("fitcheck"),
        # uiOutput("codatatypescheck"),
        # uiOutput("cvcheck"),
        # uiOutput("settingscheck"),
        # uiOutput("cvmeasures"),
        # uiOutput("nfolds"),
        # uiOutput("methods"),
        # uiOutput("splitcheck"),
        # uiOutput("responsetypecheck"),
        ###
        conditionalPanel(
          "input.fit && output.multplotsflag == true",
          plotOutput("multplots"),
          uiOutput("multplotsdownload")),
        verbatimTextOutput("selvar"),
        uiOutput("selvardownload"),
        tableOutput("cvtable"),
        uiOutput("cvtabledownload"),
        conditionalPanel(
          # "input.cv && input.reponsetype == 'binomial'",
          "input.responsetype == 'binomial'",
          plotOutput("rocplots"),
          uiOutput("rocplotsdownload"))
      )
    )
  )
)