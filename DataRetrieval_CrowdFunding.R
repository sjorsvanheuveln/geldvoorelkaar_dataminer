#################################
#### IR of Geldvoorelkaar.nl ####
#### and Analysis 29-05-2015 ####
#################################

# Extra Info #
# Classificatie is een maat van afloscapaciteit,
# dus deel van afbetaling op het totaal inkomen.
# een s betekent daarbij start-up dus daar zijn de inkomsten onbekend.
# PD is de kans op insolventie binnen 1 jaar (niet over looptijd)

setwd("~/Desktop/Awesome_Scripts")
library(RCurl)
library(XML)
library(ggplot2)
library(calibrate)

##### Functions #####
read_parse_store <- function(page_range){
  for (i in 1:page_range){ #range of pages to retrieve data
    cat("Currently parsing page",i,"\n")
    webpage <- getURL(paste("https://www.geldvoorelkaar.nl/geld-investeren/projecten.aspx?page=",i,sep=""))
    webpage <- readLines(tc <- textConnection(webpage)); close(tc)
    
    name_tag <- 'ProjectNaamLabel\">'
    pd_tag <- 'jaar. Zie toelichting in hoofdstuk Risico.\" style=\"font-weight:bold;\">'
    class_tag <- 'ClassificatieLabel'
    graydon_tag <- 'GraydonRatingLabel\" title=\"Volgens Graydon'
    loan_tag <- 'BedragLabel\"'
    creditsafe_tag <- 'CreditSafeLabel\"'

    name_lines <- webpage[gregexpr(name_tag,webpage, fixed=T)!=-1]
    pd_lines <- webpage[gregexpr(pd_tag,webpage, fixed=T)!=-1]
    class_lines <- webpage[gregexpr(class_tag,webpage, fixed=T)!=-1]
    graydon_lines <- webpage[gregexpr(graydon_tag,webpage, fixed=T)!=-1]
    loan_lines <- webpage[gregexpr(loan_tag,webpage, fixed=T)!=-1]
    creditsafe_lines <- webpage[gregexpr(creditsafe_tag,webpage, fixed=T)!=-1]
    
    for (z in 1:length(name_lines)){
      ### here the project name is parsed
      if (z==1){n <- 1}
      if (z==1){m <- 1}
      if (z==1){l <- 1}
      if (z==1){k <- 1}
      if (z==1){p <- 1}
      start_index <- gregexpr('\">',name_lines[z], fixed=T)[[1]][1]
      project_name <- c(project_name,substring(name_lines[z],start_index+2,nchar(name_lines[z])-7))
      
      ### here the pd rating is parsed
      if (grepl(paste("ctl0",z,sep=""),pd_lines[n])){
        start_index <- gregexpr('\">',pd_lines[n], fixed=T)[[1]][1]
        entry <- substring(pd_lines[n],start_index+2,nchar(pd_lines[n])-7)
        project_pd <- c(project_pd,as.numeric(sub(",",".",entry)))
        n <- n+1
      }
      else{project_pd <- c(project_pd,NA)}
      
      ### here the classification is parsed
      if (grepl(paste("ctl0",z,sep=""),class_lines[m])){
        start_index <- gregexpr('\">',class_lines[m], fixed=T)[[1]][1]
        project_class <- c(project_class,substring(class_lines[m],start_index+2,nchar(class_lines[m])-7))
        m <- m+1
      }
      else{project_class <- c(project_class,NA)}
      
      ### here graydon rating is parsed
      if (grepl(paste("ctl0",z,sep=""),graydon_lines[l])){
        start_index <- gregexpr('\">',graydon_lines[l], fixed=T)[[1]][1]
        project_graydon <- c(project_graydon,substring(graydon_lines[l],start_index+2,nchar(graydon_lines[l])-7))
        l <- l+1
      }
      else{project_graydon <- c(project_graydon,NA)}
      
      ### here loan is parsed
      if (grepl(paste("ctl0",z,sep=""),loan_lines[k])){
        start_index <- gregexpr('\">',loan_lines[k], fixed=T)[[1]][1]
        entry <- substring(loan_lines[k],start_index+3,nchar(loan_lines[k])-7)
        project_loan <- c(project_loan,gsub(",","",entry))
        k <- k+1
      }
      else{project_loan <- c(project_loan,NA)}
      
      ### here creditsafe is parsed
      if (grepl(paste("ctl0",z,sep=""),creditsafe_lines[p])){
        start_index <- gregexpr('\">',creditsafe_lines[p], fixed=T)[[1]][1]
        entry <- substring(creditsafe_lines[p],start_index+2,nchar(creditsafe_lines[p])-7)
        project_creditsafe <- c(project_creditsafe,gsub(",","",entry))
        p <- p+1
      }
      else{project_creditsafe <- c(project_creditsafe,NA)}
    }
    if (i==1){
      cat("Current List\n")
      print(project_name)}
  }
  data <- data.frame(cbind(project_name,project_pd,project_class,project_graydon,project_loan,project_creditsafe))
  return(data)
}
numerize_gray <- function(df){
  if (is.na(df["Graydon"])){df$Graydon <- NA}#as.integer(0)}
  else if (df["Graydon"] == "AAA"){df$Graydon <- as.integer(1)}
  else if (df["Graydon"] == "AA"){df$Graydon <- as.integer(2)}
  else if (df["Graydon"] == "A"){df$Graydon <- as.integer(3)}
  else if (df["Graydon"] == "BBB"){df$Graydon <- as.integer(4)}
  else if (df["Graydon"] == "BB"){df$Graydon <- as.integer(5)}
  else if (df["Graydon"] == "B"){df$Graydon <- as.integer(6)}
  else if (df["Graydon"] == "CCC"){df$Graydon <- as.integer(7)}
}
numerize_class <- function(df){
  if (is.na(df["Class"])){df$Graydon <- NA}#as.integer(0)}
  else if (df["Class"] == "1"){df$Class <- as.integer(1)}
  else if (df["Class"] == "2"){df$Class <- as.integer(2)}
  else if (df["Class"] == "3"){df$Class <- as.integer(3)}
  else if (df["Class"] == "4"){df$Class <- as.integer(4)}
  else if (df["Class"] == "5"){df$Class <- as.integer(5)}
  else if (df["Class"] == "5s"){df$Class <- as.integer(6)}
}


##### Constants and Initialization #####
project_name <- c()
project_pd <- c()
project_class <- c()
project_graydon <- c()
project_loan <- c()
project_creditsafe <- c()
chartime <- as.character(Sys.time())
page_range <- 25 #number of latest pages to be retrieved

##### Retrieve HTML and Parse #####
data <- read_parse_store(page_range)
colnames(data) <- c("Project","PD","Class","Graydon","Loan","Creditsafe")

##### Transformations #####
#make PD to numeric
data$PD <- as.numeric(as.character(data$PD))
#make Loan to numeric
data$Loan <- as.numeric(sub(".","",as.character(data$Loan), fixed=T))
#numerize Graydon Rating
data$Graydon <- apply(data,1,numerize_gray)
#numerize Class
data$Class <- apply(data,1,numerize_class)
#fill empty cells in Creditsafe with NA
data[(data[,"Creditsafe"] == "") & !is.na((data[,"Creditsafe"])),"Creditsafe"] <- NA
#numerize Creditsafe
data$Creditsafe <- as.numeric(as.character(data$Creditsafe))
View(data)

##### Plots and Correlations#####
plot(data$Class,data$Loan)
hist(data$Class)
plot(data$Class,data$Graydon, ylab="Graydon Integer", xlab="Aflos-Capaciteit Classification",col=rgb(0,0,1,1/2), main="Class Rating vs Graydon Rating",type="p")
cor(data$Class,data$Graydon,use="complete.obs")
plot(data$Class,data$PD)
cor(data$Class,data$PD,use="complete.obs") #what the hell?
plot(data$Creditsafe,data$PD,col="blue", main=paste("Average Creditsafe Rating = ",mean(data$Creditsafe,na.rm=T)))
cor(data$Creditsafe,data$PD,use="complete.obs") # however no correlation here
plot(data$Class,data$Creditsafe,main=paste("Correlation Class vs Creditsafe",round(cor(data$Class,data$Creditsafe, use="complete.obs"),3)))
abline(lm(Creditsafe~Class,data=data),col="red")


data2 <- subset(data,!is.na(data$Creditsafe) & !is.na(data$Class))[1:50,]
plot(data2$Creditsafe,data2$Class,xlim=c(0,100),col="blue")
textxy(data2$Creditsafe,data2$Class,data2$Project)
cor(data2$Creditsafe,data2$Class, use="complete.obs") # strong correlation here


##### Annuïteit #####
N = 84/12 #number of payments
r = 0.06 #interest rate
loan = 1000
m = 12

periodic_payment_year = loan*r/(1-1/(1+r)^(N))
periodic_payment_month = loan*(r/m)/(1-(1+(r/m))^(-N*m)) #monthly payment/annual compounding
