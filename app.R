library(shiny)
library(openxlsx)
library(readxl)
library(tidyverse)
library(glue)
source("Budget Functions.R")
# Define the UI
defaultprojects <- activeprojects()
projectnames <- lapply(defaultprojects[[1]],function(i) {gsub("_"," ",i) })



ui <- navbarPage("",
                 tabPanel("Settings",fluidRow(column(5,textInput("budgetpath","Budget Path"),
                          textInput("payrollpath","Payroll Path"),
                          textInput("costspath","Other Costs Path"),
                          textInput("monthyear","Date (As on Budget Sheet)")),
                          column(5,checkboxGroupInput("projects","Select Projects",choiceNames=projectnames,choiceValues = defaultprojects[[1]]),checkboxInput("selectall","Select All"))),
                          fluidRow(column(5,textInput("additionalprojects","Additional Projects (As On Budget Sheet)"),actionButton("testbutton","Run!"),textOutput("errormessage")))),
                 tabPanel("Output", uiOutput("newstuff"))
                 
   
)


server<- function(input,output,session){
  #gather project input
  checkboxprojects <- reactive(input$projects)
  extraprojects <- reactive(input$additionalprojects)
  rawbudgetpath <- reactive(input$budgetpath)
  rawpayrollpath <- reactive(input$payrollpath)
  rawcostpath <- reactive(input$costspath)
  
  observeEvent(input$selectall,{if(input$selectall == TRUE){updateCheckboxGroupInput(session,"projects",selected=defaultprojects[[1]],choiceNames = projectnames,choiceValues = defaultprojects[[1]])}
    else{updateCheckboxGroupInput(session,"projects",choiceNames = projectnames,choiceValues = defaultprojects[[1]])}})
  observeEvent(input$testbutton,{
    newbudgetpath <- gsub('"','',rawbudgetpath())
    newpayrollpath <- gsub('"','',rawpayrollpath())
    newcostspath <- gsub('"','',rawcostpath())
    tryCatch({
    if(!rawbudgetpath() == ""){
      
    #modify paths to be usable in function

    
    #collect wds numbers using selected projects
    if(extraprojects() == ""){selectedprojects <- checkboxprojects()}
    if(extraprojects()!= ""){
      splitprojects <- strsplit(extraprojects(),",")
      selectedprojects <- c(checkboxprojects(),splitprojects[[1]])
      print(selectedprojects)}
    WDS <- c()
    for(i in 1:length(defaultprojects[[1]])){
      if(defaultprojects[[1]][i] %in% selectedprojects){
        WDS <- c(WDS,defaultprojects[[2]][i])}
      
    }
    #call function to gather outputs
    
    outputs <- masterfunction(newbudgetpath,newpayrollpath,newcostspath,WDS,selectedprojects,input$monthyear)
    #render tables for each project
    lapply(seq_len(length(selectedprojects)),function(i){
      print(i)
      output[[paste0("budget",i)]] <- renderDataTable({DT::datatable(data.frame(outputs[[1]][i]),options=list(pageLength=100))})
      output[[paste0("payroll",i)]] <- renderDataTable({DT::datatable(data.frame(outputs[[2]][i]),options = list(pageLength=100))})
      output[[paste0("costs",i)]] <- renderDataTable({DT::datatable(data.frame(outputs[[3]][i]),options= list(pageLength=100))})
      output[[paste0("download",i)]] <- downloadHandler(filename = function(){paste(selectedprojects[i],Sys.Date(),".xlsx",sep="")},
                                                        content = function(file){
                                                          wb <- createWorkbook()
                                                          addWorksheet(wb,sheetName = "Budget")
                                                          addWorksheet(wb, "Payroll")
                                                          addWorksheet(wb,"Othercosts")
                                                          writeData(wb,outputs[[1]][i],sheet = "Budget")
                                                          writeData(wb,outputs[[2]][i],sheet = "Payroll")
                                                          writeData(wb,outputs[[3]][i],sheet = "Othercosts")
                                                          saveWorkbook(wb,file=file)},contentType = "file/xlsx")
                                                          
                                                        })
    

    #render new tabs containing tables dynamically
    output$newstuff <- renderUI({
      nTabs <- length(outputs[[1]])
      tabPanels <- length(output)
      navpanels <- c()

      
      for(i in 1:length(outputs[[1]])){
        
        newpanel <- navlistPanel(tabPanel("Budget",downloadButton(paste0("download",i),"Download"),dataTableOutput(paste0("budget",i))),tabPanel("Payroll",dataTableOutput(paste0("payroll",i))),tabPanel("Costs",dataTableOutput(paste0("costs",i))),widths = c(1,10))
        navpanels <- append(navpanels,list(newpanel))
      }
      #print(navpanels)
      myTabs <- mapply(tabPanel,as.list(selectedprojects),navpanels,SIMPLIFY = FALSE)
      do.call(tabsetPanel,myTabs)
      
    })
}
    
    #output everything minus the budget
    if(rawbudgetpath() == "" ){
      if(newcostspath != "" & newpayrollpath != ""){ 
        payrolloutput <- payrollonly(newpayrollpath)
        costsoutput <- othercostsonly(newcostspath)
        
        wds <- c()
        ntabs <- length(payrolloutput)
        for(i in 1:ntabs){
          print(payrolloutput[i])
          df <- data.frame(payrolloutput[i])

          wds <-c(wds,df$WDS[1])
        }
        print(wds)
        lapply(seq_len(ntabs),function(i){
          output[[paste0("payroll",i)]] <- renderDataTable({DT::datatable(data.frame(payrolloutput[i]),options=list(pageLength=100))})
          output[[paste0("othercosts",i)]] <- renderDataTable({DT::datatable(data.frame(costsoutput[i]),options=list(pageLength=100))})
          })
        output$newstuff <- renderUI({
          nTabs <- length(payrolloutput)
          tabPanels <- length(output)
          navpanels <- c()
          
          
          for(i in 1:ntabs){
            
            newpanel <- navlistPanel(tabPanel("Payroll",dataTableOutput(paste0("payroll",i))),tabPanel("Other Costs",dataTableOutput(paste0("othercosts",i))),widths = c(1,10))
            navpanels <- append(navpanels,list(newpanel))
          }
          print(length(navpanels))
          
          myTabs <- mapply(tabPanel,as.list(wds),navpanels,SIMPLIFY = FALSE)
          do.call(tabsetPanel,myTabs)
          
        })        
        
        
        
      }
      #output costs only
      if(newcostspath != "" & newpayrollpath ==""){
        costsoutput <- othercostsonly(newcostspath)
        wds <- c()
        ntabs <- length(costsoutput)
        for(i in 1:ntabs){
          
          df <- data.frame(costsoutput[i])
          
          wds <-c(wds,df$WBS.Element[1])
        }
        print(wds)
        lapply(seq_len(ntabs),function(i){
          output[[paste0("othercosts",i)]] <- renderDataTable({DT::datatable(data.frame(costsoutput[i]),options=list(pageLength=100))})
        })
        output$newstuff <- renderUI({
          nTabs <- length(costsoutput)
          tabPanels <- length(output)
          navpanels <- c()
          
          
          for(i in 1:ntabs){
            
            newpanel <- navlistPanel(tabPanel("Other Costs",dataTableOutput(paste0("othercosts",i))),widths = c(1,10))
            navpanels <- append(navpanels,list(newpanel))
          }
          print(length(navpanels))
          
          myTabs <- mapply(tabPanel,as.list(wds),navpanels,SIMPLIFY = FALSE)
          do.call(tabsetPanel,myTabs)
          
        })

        
      }
      #output payroll only
      if(payrollpath != "" & newcostspath ==""){
        message("Hi")
      payrolloutput <- payrollonly(newpayrollpath)
      wds <- c()
      ntabs <- length(payrolloutput)
      for(i in 1:ntabs){
        
        df <- data.frame(payrolloutput[i])
        
        wds <-c(wds,df$WDS[1])
      }
      print(wds)
      lapply(seq_len(ntabs),function(i){
        output[[paste0("payroll",i)]] <- renderDataTable({DT::datatable(data.frame(payrolloutput[i]),options=list(pageLength=100))})
      })
      output$newstuff <- renderUI({
        nTabs <- length(payrolloutput)
        tabPanels <- length(output)
        navpanels <- c()
        
        
        for(i in 1:ntabs){
          
          newpanel <- navlistPanel(tabPanel("Payroll",dataTableOutput(paste0("payroll",i))),widths = c(1,10))
          navpanels <- append(navpanels,list(newpanel))
        }
        print(length(navpanels))
        
        myTabs <- mapply(tabPanel,as.list(wds),navpanels,SIMPLIFY = FALSE)
        do.call(tabsetPanel,myTabs)
        
      })

    }}
    
    output$errormessage <- renderText({""})},
  error = function(cond){
    output$errormessage <- renderText({paste0("Error occured. Make sure paths are correct and at least Payroll or Costs are present. Error",conditionMessage(cond))})
  })})
#Dynamic Run Button
#observe(if((!input$budgetpath=="" & !input$payrollpath=="" & !input$costspath=="" & (!length(input$projects)==0 | !input$additionalprojects==""))){
 # output$runbutton <- renderUI(actionButton("testbutton","Run!"))
#})

observeEvent({input$budgetpath 
  input$payrollpath 
  input$additionalprojects 
  input$costspath 
  input$projects
  input$monthyear},{print(input$monthyear)
    if(TRUE){
  output$runbutton <- renderUI(actionButton("testbutton","Run!"))}
    else{output$runbutton <-renderUI(fluidRow())}})

}

shinyApp(ui,server)

#eventReactive()
#reactive()
