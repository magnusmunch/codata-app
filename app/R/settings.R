### creates setting panels
settingsUI <- function(id, settings) {
  
  conditionalPanel(
    condition=paste0("input.methods.includes('", id, "')"),
    {
      column(
        4,
        titlePanel(id),
        radioButtons(paste0("advanced", id), label="advanced",
                     choices=list("off"="off", "on"="on")),
        bsTooltip(paste0("advanced", id), title=tooltips$advanced,
                  placement="right", trigger="hover"),
        lapply(1:length(settings), function(i) {
          if(class(settings[[i]])=="logical") {
            checkboxInput(paste0("settings", id, names(settings)[i]), 
                          label=names(settings)[i], value=settings[[i]])
          } else if(class(settings[[i]])=="numeric") {
            numericInput(paste0("settings", id, names(settings)[i]), 
                         label=names(settings)[i], value=settings[[i]],
                         min=ifelse(names(settings)[i]=="alpha", 0, NA),
                         max=ifelse(names(settings)[i]=="alpha", 1, NA))
          } else if(class(settings[[i]])=="character") {
            selectInput(paste0("settings", id, names(settings)[i]), 
                        label=names(settings)[i], choices=settings[[i]])
          }
          # if(names(settings)[i] %in% toolnames) {
          #   bsTooltip(paste0("settings", id, names(settings)[i]),
          #             title=tooltips[[names(settings)[i]]],
          #             placement="right", trigger="hover")  
          # }
        })
        ,
        # }
        # ),
        conditionalPanel(
          condition=paste0("input.advanced", id, "=='on'"), 
          {
            fn <- ifelse(id=="enet", "glmnet", id)
            # fms <- eval(parse(text=paste0("formals(", fn, ")")))
            fms <- formals(fn)
            fms <- fms[!(names(fms) %in% names(settings))]
            fms <- fms[!(names(fms) %in% 
                           c("x", "X", "y", "Y", "unpen", "unpenalized", 
                             "family", "model", "partititions", "groupings",
                             "groupings.grouplvl", "..."))]
            lapply(1:length(fms), function(i) {
              textInput(paste0("settings", fn, names(fms)[i]), 
                        label=names(fms)[i], value=fms[i])})
          }
        )
      )
    }
  )
}
