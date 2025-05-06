
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
  costindex <- 0
  for(j in cleanedcosts$`Cost Element`){
    costindex <- costindex+1
    count<- 0
    for(i in budget$Account){
      elements <- strsplit(i,",")
      count <- count+1
      #the following if is the logic driving the placements of the costs.
      #if this function fails to place costs correctly or at all then it can most likely be resolved
      #with additions to the logic as well as any needed changes to the accounts on the budget sheet
      if((j == i)|((j== "519800") & grepl("Faculty",budget$Start[count]))|
         (((substr(j,1,2)=="54")|(j=="526001")|(j=="521951"))& grepl("Domestic",budget$Start[count]))|
         ((j=="519310")&(grepl("0.32%",budget$Start[count])))|
         (j %in% elements[[1]]))#can implement absurd logic here to make the placement as accurate as possible{
        budget[count,month]<- budget[count,month]+cleanedcosts[costindex,"Val/COArea Crcy"]
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
  for(i in payroll$Name){
    index = index+1
    if( !is.na(i)){
      
      Names <- c(Names,payroll$Name[index])
      Account <- c(Account,payroll$Account[index])
      Pay <- c(Pay,payroll$Amount[index])
      WDS <- c(WDS,payroll$`Cost Object`[index])
      Descript <- c(Descript,payroll$`Acct. Description`[index])
    }
  }
  data <- data.frame(Names,Account,Pay,WDS,Descript)
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
          print("made it here")
          
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
  
  #benefitsManagerial <- sum(subset(namedproll,grepl("Manag",FacType)))
  #benefitsAdmin <- sum(subset(namedproll,grepl("Administrative",FacType)))
  #benefitsFaculty <- sum(subset(namedproll,grepl("Faculty",FacType)))
  #print(benefitsManagerial)
  #print(benefitsAdmin)
  #print(benefitsFaculty)
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
  print("hi again")
  for(i in projectnamelist){
    print(i)
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
#arlready installed
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



#test Bass March 2025

#budgetpath <- "C:/Users/ccraft/Documents/Budget Updates/March 2025/Ligon Year 5 NCC + Supp. (Tracking) (2).xlsx"
#payrollpath <- "C:/Users/ccraft/Documents/Budget Updates/March 2025/detailpayrollmarch2025.xlsx"
#costspath <- "C:/Users/ccraft/Documents/Budget Updates/March 2025/costsmarch2025.xlsx"

#budget <- read_excel(budgetpath,"UNO Bass_316")
#cleanbudget <- budgetcleaner(budget)
#payroll <- read_excel(payrollpath)
#cleanpayroll <- payrollcleaner(payroll)
#namedpayroll <- gatherpayroll(cleanpayroll)
#cleanbudget$`Mar 2025` <- 0
#labeled <- placebenefits(cleanbudget,namedpayroll,"Mar 2025")
#labeled <- filter(labeled,WDS == "44-0108-1001-318")
#labeled <- filter(labeled,substr(Account,1,3)=="519" )
#totaladmin <- sum(subset(labeled,grepl("Admin",FacType))$Pay)
#totaladmin


