library(shiny)
library(openxlsx)
library(readxl)
library(tidyverse)
library(glue)
source("Budget Functions BOA.R")
# Define the UI
projectdata <- read_excel("Active Projects.xlsx")
projectbnames <- projectdata$Project
projectwbs <- projectdata$WBS

projectnames <- lapply(projectbnames,function(i) {gsub("_"," ",i) })



ui <- navbarPage("",
                 tabPanel("Settings",fluidRow(column(5,fileInput("budgetfile","Upload Budget"),
                          fileInput("payrollfile","Upload Payroll"),
                          fileInput("costfile","Upload Costs"),
                          textInput("monthyear","Month")),
                          column(5,checkboxGroupInput("projects","Select Projects",choiceNames=projectnames,choiceValues = projectbnames),checkboxInput("selectall","Select All"))),
                          fluidRow(column(5,actionButton("testbutton","Run!"),textOutput("errormessage")))),
                 tabPanel("Output", uiOutput("newstuff"))
                 
   
)


server<- function(input,output,session){

  #for files
  costfile <- reactive(input$costfile$datapath)
  payrollfile <- reactive(input$payrollfile$datapath)
  budgetfile <- reactive(input$budgetfile$datapath)
  checkboxprojects <- reactive(input$projects)
  
  observeEvent(input$selectall,{if(input$selectall == TRUE){updateCheckboxGroupInput(session,"projects",selected=projectbnames,choiceNames = projectnames,choiceValues = projectbnames)}
    else{updateCheckboxGroupInput(session,"projects",choiceNames = projectnames,choiceValues = projectbnames)}})
  observeEvent(input$testbutton,{
    
    tryCatch({
    if(!is.null(budgetfile())){
    print("hi")
    #modify paths to be usable in function

    
    #collect wds numbers using selected projects
    selectedprojects <- checkboxprojects()
    WDS <- c()
    for(i in 1:length(projectbnames)){
      if(projectbnames[i] %in% selectedprojects){
        WDS <- c(WDS,projectwbs[i])}
      
    }
    #call function to gather outputs
    
    #outputs <- masterfunction(newbudgetpath,newpayrollpath,newcostspath,WDS,selectedprojects,input$monthyear)
    outputs <- masterfunction(budgetfile(),payrollfile(),costfile(),WDS,selectedprojects,input$monthyear)
    print("Hi again")
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
        
        newpanel <- navlistPanel(fluidRow(downloadButton(paste0("download",i),"Download")),tabPanel("Budget",dataTableOutput(paste0("budget",i))),tabPanel("Payroll",dataTableOutput(paste0("payroll",i))),tabPanel("Costs",dataTableOutput(paste0("costs",i))),widths = c(1,10))
        navpanels <- append(navpanels,list(newpanel))
      }
      #print(navpanels)
      myTabs <- mapply(tabPanel,as.list(selectedprojects),navpanels,SIMPLIFY = FALSE)
      do.call(tabsetPanel,myTabs)
      
    })
}
    
    #output everything minus the budget
    if(is.null(budgetfile())){
      if(!is.null(costfile()) & !is.null(payrollfile())){ 
        payrolloutput <- payrollonly(payrollfile())
        costsoutput <- othercostsonly(costfile())
        
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
      
      if(!is.null(costfile()) & is.null(payrollfile())){
        costsoutput <- othercostsonly(costfile())
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
      
      if(!is.null(payrollfile()) & is.null(costfile())){
        message("Hi")
      payrolloutput <- payrollonly(payrollfile())
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
