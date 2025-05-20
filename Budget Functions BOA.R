
#This is the list of functions used to drive the budget helping tool.
#most of these functions end up being wrapped up in the end with the master function
#to allow for quick budget adjustments across all projects

#The first series of functions are clean functions. Their primary goal is to get the
#sheets formatted in a way that will allow ease of use in R and allow for more variable
#coding so that the functions are usable for all projects.


#clean up non payroll cost sheet for a given WBS
cleancosts <- function(costs,WBS){
  costs <- drop_na(costs,`Document type`)
  costs <- filter(costs,`WBS Element`== WBS)
  grabcomp<- costs[grepl("^5198",costs$`Cost Element`),]
  costs <- costs[!grepl("^51",costs$`Cost Element`),]
  costs <- rbind(costs,grabcomp)
  return(costs)
}


#clean up payroll sheet
payrollcleaner <- function(payrolldata) {
  payrolldata <- payrolldata[,c(-1,-6,-7)]
  names(payrolldata) <- payrolldata[11,]
  payrolldata <- payrolldata[-1:-11,]
  payrolldata$Amount <- as.numeric(payrolldata$Amount)
  return(payrolldata)
}

#formatting budget for use
#Supplemental projects have the start line placed differently
#will either need to change how setting the column names works or standardize them
budgetcleaner <- function(budget) {
  colnames(budget)[1]<-"UNO"
  #budget <- budget %>% drop_na(`UNO`)
  #budget <- distinct(budget,budget$`UNO`,.keep_all = TRUE)
  iindex <- 0
  usedindexes <- c()
  for(i in budget$UNO){
    iindex <- iindex + 1
    print(i)
    if(grepl("Start/End",i)){
      usedindexes <- c(usedindexes, iindex)
    }
  }
  if(length(usedindexes)>1){
    budget <- budget[1:usedindexes[2]-1,]
  }

  daterow <- budget[with(budget,grepl("Start/End",budget$`UNO`)),]
  colnames(budget)<- daterow
 
  #budget$`Feb 2025`<- as.numeric(budget$`Feb 2025`)
  colnames(budget)[1]<- "Start"
  budget$Account <- replace_na(budget$Account,"")
  return(budget)
}

#The following series of functions are used to map the data from the SAP data sheets
#to the correct budget sheet lines. There is an abundance of indexes being made such as iindex <- 0
#and then paired with iindex <- iindex +1 within the for loop. These operations keep track of which line
#the for loops are on so that data can be mapped to the correct line of the budget from the correct line of the data sheet.


#function to map cost sheet data to correct budget sheet lines
#currently only places exact matches
placecosts <- function(budget,cleanedcosts,month){
  subawardwbs <- strsplit(as.character(budget$Account[2]),",")
  costindex <- 0
  for(j in cleanedcosts$`Cost Element`){
    costindex <- costindex+1
    count<- 0
    #handle individual budget account numbers
    for(i in budget$Account){
      
      elements <- strsplit(i,",")
      count <- count+1
      if(i != ""){
      #the following if is the logic driving the placements of the costs.
      #if this function fails to place costs correctly or at all then it can most likely be resolved
      #with additions to the logic as well as any needed changes to the accounts on the budget sheet
      if((j == i)|((j== "519800") & grepl("Faculty",budget$Start[count]))|
         (((substr(j,1,2)=="54")|(j=="526001")|(j=="521951"))& grepl("Domestic",budget$Start[count]))|
         ((j=="519310")&(grepl("0.32%",budget$Start[count])))|
         (j %in% elements[[1]])){budget[count,month]<- budget[count,month] + cleanedcosts[costindex,"Val/COArea Crcy"]}
        
      #handle ranges 
      message("check new stuff")
      for(k in 1:length(elements[[1]])){
        print(grepl("-",elements[[1]][k]))
        if(grepl("-",elements[[1]][k])){
          elementrange <- strsplit(elements[[1]][k],"-")
          lowerbound <- as.numeric(elementrange[[1]][1])
          upperbound <- as.numeric(elementrange[[1]][2])
          print(c(lowerbound, cleanedcosts$`Cost Element`[costindex] ,upperbound))
          message("check here boss")
          print((as.numeric(cleanedcosts$`Cost Element`[costindex]) > lowerbound) & (as.numeric(cleanedcosts$`Cost Element`[costindex]) < upperbound))
          if((as.numeric(cleanedcosts$`Cost Element`[costindex]) > lowerbound) & (as.numeric(cleanedcosts$`Cost Element`[costindex]) < upperbound)){
            print(budget[count,month])
           budget[count,month] <- budget[count,month] + cleanedcosts[costindex,"Val/COArea Crcy"]
           print(budget[count,month])
          }
          
        }
      } 
      }
     }
    }
    
    
    
     #handle BOA Subawards
    #handle case of 1 subaward
    message("check here actually")
    print(subawardwbs)
    if(length(subawardwbs[[1]]) ==1 ){
      for(i in 1:length(subawardwbs)){
        message(subawardwbs[i] %in% cleanedcosts$`WBS Element`)
        if(subawardwbs[i] %in% cleanedcosts$`WBS Element`){
          for(j in 1:length(budget$Start)){
            if(grepl(subawardwbs[i],budget$Start[j])){
              budget[j,month] <- budget[j,month] + sum(cleanedcosts$`Val/COArea Crcy`)
            }
            
            }
          }
        }
    }
    #handle case of more than 1 subaward number
    message("Look over here")
    message(length(subawardwbs[[1]]))
    if( length(subawardwbs[[1]])>1){
      for(i in 1:length(subawardwbs[[1]])){
        message(subawardwbs[[1]][i] %in% cleanedcosts$`WBS Element`)
        if(subawardwbs[[1]][i] %in% cleanedcosts$`WBS Element`){
          for(j in 1:length(budget$Start)){
            if(grepl(subawardwbs[[1]][i],budget$Start[j])){
              budget[j,month] <- budget[j,month] + sum(cleanedcosts$`Val/COArea Crcy`)
            }
            
          }
        }
      }      
    }
  return(budget)
}


#gather named payroll information for insertion into the budget sheet
#this function essentially does additional formatting to the payroll sheet
#for easier placement
gatherpayroll <- function(payroll){
  Names = c()
  Pay = c()
  Account = c()
  WDS = c()
  Descript = c()
  index = 0
  payroll[is.na(payroll[,"Account"]),"Account"] <- ""
  for(i in payroll$Name){
    index = index+1
    if( !is.na(i) | (payroll$Account[index] == "519310")){
      
      Names <- c(Names,payroll$Name[index])
      Account <- c(Account,payroll$Account[index])
      Pay <- c(Pay,payroll$Amount[index])
      WDS <- c(WDS,payroll$`Cost Object`[index])
      Descript <- c(Descript,payroll$`Acct. Description`[index])
    }
  }
  data <- data.frame(Names,Account,Pay,WDS,Descript)
  data[is.na(data[,"Names"]),"Names"] <- "Grad Students"
  
  message("Check here")
  return(data)
}


#function to place payroll data into the budget sheet irregardless if there are other WDS attached
#currently needs the name to be present in the corresponding column and account number to be present
#benefits are handled separately

placepayroll <- function(namedproll,budget,month){
  jindex <- 0
  remaining <- namedproll
  usedpayrollindex <- c()
  usedbudgetindex <- c()
  for(j in namedproll$WDS){
    jindex <- jindex+1
    iindex <-0
    if(j ==budget$Account[1]){
      
      for(i in budget$Account){
        iindex <- iindex+1
        currentname<- strsplit(strsplit(namedproll$Names[jindex],";")[[1]][2]," ")[[1]][2]
        
        if((i==namedproll$Account[jindex])&(grepl(glue("{currentname}"), budget$Start[iindex]))){
          
          budget[iindex,month]<- budget[iindex,month]+namedproll$Pay[jindex]
          remaining <- remaining[!remaining$Names==namedproll$Names[jindex],]
          usedbudgetindex <- c(usedbudgetindex,iindex)
          usedpayrollindex <- c(usedpayrollindex,jindex)
          
        }
      }
    }
    
  }
  
  kindex <- 0
  usedbudgetindex2 <- c()
  usedpayrollindex2 <- c()
  for(k in remaining$Account){
    lindex <-0
    kindex <- kindex+1
    if(remaining$WDS[kindex]==budget$Account[1]){
      for(l in budget$Account){
        lindex <- lindex +1
        if(l == k){
          budget[lindex,month] <- budget[lindex,month] + remaining$Pay[kindex]
          usedbudgetindex2 <- c(usedbudgetindex2,lindex)
          usedpayrollindex2 <- c(usedpayrollindex2, kindex)
        }
      }
    }
  }
  overcountedindex <- c()
  for(o in usedbudgetindex2){
    for(p in usedbudgetindex){
      if(p==o){
        overcountedindex <- c(overcountedindex,p)
      }
    }
  }
  
  for(m in overcountedindex){
    subtractproll <- namedproll[-usedpayrollindex,]
    nindex <- 0
    for(n in subtractproll$Account){
      nindex <- nindex + 1
      if(subtractproll$WDS[nindex] == budget$Account[1]){
        if(budget$Account[m]== n){
          budget[m,month] <- budget[m,month] - subtractproll$Pay[nindex]
        }
      }
    }
  }
  return(budget)
}

#function to place benefits. The first paret of the function creates another
#dataframe that places the category name of the individual on the payroll sheet
#to an additional column based on name so that each benefits line is associated
#with either faculty,administration, GA, or managerial positions.
placebenefits <- function(budget,namedproll,month){
  namedproll$FacType <- ""
  index <-0
  for(i in namedproll$Account){
    index <- index + 1
    if(grepl("^511",i)|grepl("^512",i)|grepl("^513",i)){
      namedproll$FacType[index] <- namedproll$Descript[index]
    }
  }
  #create frame with just faculty and type to iterate through
  filterframe <- filter(namedproll,FacType != "")
  #iterate through filterframe to allocate faculty type to benefits
  jindex<-0
  for(j in filterframe$Names){
    jindex <-jindex+1
    index <-0
    for(i in namedproll$Names){
      index <- index + 1
      if(i==j){
        namedproll$FacType[index]<- filterframe$FacType[jindex]
      }
    }
  }
  
  #place benefits on budget sheet.
  benefits <- filter(namedproll,grepl("^519",namedproll$Account))
  iindex <- 0
  for(i in benefits$WDS){
    iindex <- iindex +1
    jindex <-0
    if(i == budget$Account[1]){
      for(j in budget$Account){
        
        jindex <- jindex + 1
        if(grepl("^519",j)){
          if((grepl("Faculty",budget$Start[jindex]) & grepl("Faculty",benefits$FacType[iindex])) |
             (grepl("Admin",budget$Start[jindex]) & grepl("Administrative",benefits$FacType[iindex] )) |
             (grepl("Staff",budget$Start[jindex]) & grepl("Manag",benefits$FacType[iindex]))) {
            budget[jindex,month] <- budget[jindex,month] + benefits[iindex,"Pay"]
          }
        }
      }
    }
  }
  
  output <- list(budget,namedproll)
  return(output)
}


#this is the "master" function that puts all of the cleaning and placement functions into one place
#for quick use of the functions.
masterfunction <- function(budgetpath,payrollpath,costspath,WDSlist,projectnamelist,month){
  finalbudgets <- list()
  costlist <- list()
  projectpayrolls <- list()
  costss <- read_excel(costspath)
  payroll <- read_excel(payrollpath)
  cleanedpayroll <- payrollcleaner(payroll)
  cleanedpayroll <- gatherpayroll(cleanedpayroll)
  for(i in projectnamelist){
    budget <- read_excel(budgetpath,sheet = i)
    budget <- budgetcleaner(budget)
    budget[,month] <- 0
    for(WDS in WDSlist){
      if(WDS == budget$Account[1]){
        activeWDS <- WDS
      }
    }
    budget <- budget
    costs <- costss
    subawardwbs <- strsplit(as.character(budget$Account[2]),",")
    totalcost <- filter(costs,`WBS Element`==activeWDS)
    totalcost <- sum(totalcost$`Val/COArea Crcy`)
    costs<- cleancosts(costs,activeWDS)

    numberofrows <- dim(costs)[1]

    
    #filter payroll by current project and add faculty benefit sums to payroll
    
    projectpayroll <- filter(cleanedpayroll,WDS == activeWDS)
    output <- placebenefits(budget,cleanedpayroll,month)
    namedproll <- output[[2]]
    namedproll <- filter(namedproll,WDS == activeWDS)
    namedproll <- filter(namedproll,substr(Account,1,3)=="519")
    
    #handle grad student benefits
    message("Check Here")
    if("Grad Students" %in% projectpayroll$Names){
      gradbentest <- rev(projectpayroll$Names == "Grad Students")
      nindex <- 0
      
      for(i in gradbentest){
        message(i)
        nindex <- nindex +1
        if(i){
          gradbentest <- rep(TRUE,length(gradbentest))
          gradbentest[nindex] <- !i
          gradbentest[nindex + 1] <- !i
          projectpayroll <- projectpayroll[rev(gradbentest),]
          break
        }
      }}
    
    
    payrollrows <- dim(projectpayroll)[1]
    costadmin <-  sum(subset(namedproll,grepl("Admin",FacType))$Pay)
    costfac <- sum(subset(namedproll,grepl("Faculty",FacType))$Pay)
    costmanag <- sum(subset(namedproll,grepl("Manag",FacType))$Pay)
    projectpayroll[payrollrows +1,1]<-"Faculty Benefits"
    projectpayroll[payrollrows +1,"Pay"] <- costfac
    projectpayroll[payrollrows +2,1]<-"Admin Benefits"
    projectpayroll[payrollrows +2,"Pay"] <- costadmin
    projectpayroll[payrollrows +3,1]<-"Managerial Benefits"
    projectpayroll[payrollrows +3,"Pay"] <- costmanag   
    
    projectpayrolls <- append(projectpayrolls,list(projectpayroll))
    
    

    budget <- output[[1]]
    budget <- placepayroll(cleanedpayroll,budget,month)
    budget <- placecosts(budget,costs,month)
    
    #handle BOA subaward
    print(budget$Account[2])
    print(!is.null(budget$Account[2]))
    if(budget$Account[2]!=""){
      for(i in subawardwbs){
        print(i)
        subcost <- cleancosts(costss,i)
        budget <- placecosts(budget,subcost,month)
        costs <- rbind(costs,subcost)
        numberofrows <- numberofrows + 1
        totalcost <- sum(subcost$`Val/COArea Crcy`) + totalcost
      }
    }    
    
    budget <- select(budget,"Start",month)
    #place total costs on cost sheet
    costs[numberofrows +1,1] <- "Total (All Costs)"
    costs[numberofrows +1,"Val/COArea Crcy"] <- totalcost
    

   
    costlist <- append(costlist,list(costs))
    
    
    
    finalbudgets <- append(finalbudgets,list(budget))
    
  }
  finaloutput <- list(finalbudgets,projectpayrolls,costlist)
  return(finaloutput)
}

#current active projects and their WDS numbers. This function exists to simply
#create lists for those two elements to be fed to the master function. This function
#is static and will need to be updated as active projects change.
activeprojects <- function(){
  print("Last updated 3/1/2025")
  return(list(list("UNO Bass_316","UNO Hunter_317","UNO Nguyen_318","UNO Hughes_319","UNO Hughes (Chapman) _320","UNO Rottweiler (UCL)_325","UNO House (Cyber)_327","UNO Drones- (SUPP)_315",
                   "Electon Security (SUPP)_324","IAPSS (SUPP) NCE_323","NCE-DVE Landscape (SUPP)_322","NCE- Training Asses. (SUPP)_321"),
              list("44-0108-1001-316","44-0108-1001-317","44-0108-1001-318","44-0108-1001-319","44-0108-1001-320", "44-0108-1001-325","44-0108-1001-327","44-0108-1001-315","44-0108-1001-324","44-0108-1001-323","44-0108-1001-322","44-0108-1001-321")))
}

#external libraries used to develop this package. Running this function before use of this
#library is required. If this function fails, it is most likely because these libraries are not
#already installed
requiredlibraries <- function(){
  library(readxl)
  library(tidyverse)
  library(glue)
}

#separate function to clean payroll and bin benefits
payrollonly <- function(payrollpath){
  payroll <- read_excel(payrollpath)
  cleanpayroll <- payrollcleaner(payroll)
  basenamedproll <- gatherpayroll(cleanpayroll)
  print(basenamedproll)
  wds <- unique(basenamedproll$WDS)
  outputs = list()
  for(k in wds){
    print(k)
    namedproll <- basenamedproll
    namedproll <- namedproll[namedproll[,"WDS"]==k,]
    
    namess <- namedproll[!substr(namedproll[,'Account'],1,3)=="519",]
    namess
    namedproll$FacType <- ""
    print(namess)
    print(namedproll)
    for(i in 1:length(namess$Names)){
      for(j in 1:length(namedproll$Names)){
        if(namess$Names[i]==namedproll$Names[j]){
          namedproll$FacType[j] <- namess$Descript[i] 
        }
      }
    }
    message("check here instead")
    print(namedproll)
    if("Grad Students" %in% namedproll$Names){
      gradbentest <- rev(namedproll$Names == "Grad Students")
      nindex <- 0
      
      for(i in gradbentest){
        message(i)
        nindex <- nindex +1
        if(i){
          gradbentest <- rep(TRUE,length(gradbentest))
          gradbentest[nindex] <- !i
          gradbentest[nindex + 1] <- !i
          print(gradbentest)
          namedproll <- namedproll[rev(gradbentest),]
          break
        }
      }}
    
    
    
    
    
    
    
    payrollrows <- dim(namedproll)[1]
    projectbenefits <- filter(namedproll,substr(Account,1,3)=="519")
    costadmin <-  sum(subset(projectbenefits,grepl("Admin",FacType))$Pay)
    costfac <- sum(subset(projectbenefits,grepl("Faculty",FacType))$Pay)
    costmanag <- sum(subset(projectbenefits,grepl("Manag",FacType))$Pay)
    namedproll[payrollrows +1,1]<-"Faculty Benefits"
    namedproll[payrollrows +1,"Pay"] <- costfac
    namedproll[payrollrows +2,1]<-"Admin Benefits"
    namedproll[payrollrows +2,"Pay"] <- costadmin
    namedproll[payrollrows +3,1]<-"Managerial Benefits"
    namedproll[payrollrows +3,"Pay"] <- costmanag
    outputs <- append(outputs,list(namedproll[1:5]))
  }  
  return(outputs)
}


#separate function to filter costs and show total cost for month. Similar to payrollonly.
othercostsonly <- function(othercostspath){
  othercosts <- read_excel(othercostspath)
  wbs <- unique(othercosts$`WBS Element`)
  
  totals <- filter(othercosts,str_length(`Document type`) > 4)
  wbs <- wbs[!is.na(wbs)]
  wbstotalmap <- data.frame(wbs,totals$`Val/COArea Crcy`)
  outputs <- list()
  for(i in 1:length(wbs)){
    currentcosts <- cleancosts(othercosts,wbs[i])
    currentlength <- length(currentcosts$`WBS Element`)
    currentcosts[currentlength+1,"Val/COArea Crcy"]<-wbstotalmap$totals..Val.COArea.Crcy.[i]
    currentcosts[currentlength+1,"Name"] <- "Total Cost For Month"
    outputs <- append(outputs,list(currentcosts))
    
  }
  return(outputs)
} 





