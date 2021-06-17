#################################################
#### St. Galler Kantonalbank Case Study      ####
#### Author: Felix Jost, fel.jost@gmail.com  ####
#################################################
################################################
#### 1. Setup & Data Loading               #####
################################################

# Load Libraries
library(dplyr)
library(plyr)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(data.table)
library(likert)

# Data Loading
setwd("~/GitHub/CaseStudy-StGallerKantonalbank")

file = "mobile_banking_casestudy.csv"
df = read.csv(file, header = TRUE, sep = ";", na.strings=c("","NA","-","0"))


################################################
##### 2. Data Cleaning                     #####
################################################

# Get rid of whitespaces
df = data.frame(lapply(df, trimws), stringsAsFactors = FALSE)

# Factorize the data
df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], 
                                       as.factor)

# Rename Columns
colnames(df) <- c('ID',
                  'Gender',
                  'Age',
                  'Marital.Status',
                  'Job',
                  'Employment.Status',
                  'Phone.Model',
                  'Bank.Name',
                  'Years.Using.Internet.Banking',
                  'Years.Using.Mobile.Banking',
                  "Find.Mobile.Banking.userfriendly",                          
                  "Internet.banking.usage.frequency",                                      
                  "Mobile.banking.usage.frequency",                                        
                  "Credit.card.debt.inquiry",                                              
                  "Credit.card.debt.payment",                                              
                  "Credit.card.limit.transactions",                                        
                  "Create.delete.virtual.card",                                          
                  "Virtual.card.limit.transactions",                                       
                  "An.additional.new.card.application",                                    
                  "Opening.a.new.account",                                                 
                  "Balance.check",                                                         
                  "Loan.transaction",                                                      
                  "Transfer.money.to.previously.transferred.account.",                     
                  "Transfer.money.to.new.account.",                                        
                  "Invoice.payment_Automatic.Once.A.Day",
                  "Government.payments",                                                   
                  "GSM.transactions",                                                      
                  "Rent.payment",                                                          
                  "Lottery.transactions",                                                  
                  "HGS.OGS.transactions",                                                
                  "Tax.penalty.transactions",                                            
                  "SGK.transactions",                                                      
                  "Bill.transactions",                                                     
                  "Salary.payment",                                                        
                  "Investment.account.transactions",                                       
                  "Foreign.money.exchange.transactions",                                   
                  "Gold.transactions",                                                     
                  "Investment.fund.transactions",                                          
                  "Stock.transactions",                                                    
                  "Public.offering.operations",                                            
                  "Repo.transactions",                                                     
                  "Bond.transactions",                                                     
                  "Instant.stock.exchange.transactions",                                   
                  "Consumer.credit.usage",                                                 
                  "My.credit.accounts",                                                    
                  "Applying.for.mortgage",                                                 
                  "Applying.for.car.loan",                                                 
                  "Findeks.risk.report",                                                   
                  "New.card.application",                                                  
                  "New.account.opening",                                                   
                  "Request.status",                                                        
                  "Foreign.exchange.calculation",                                          
                  "Loan.calculation",                                                      
                  "Make.an.appointment.at.a.bank.branch",                                 
                  "Donate.transactions",                                                   
                  "View.payment.schedule",                                                 
                  "View.bank.campaigns",                                                   
                  "Mobile.signature.use",                                                  
                  "Communicating.with.the.bank")


# Recode Factors to manageable bins
for (i in (12:59)) {  # Loop through all Columns with feature awnsers
  df[,i]<- revalue(df[,i], c("Once in a day"="Daily",
                            "Several times in a day" = "Daily",
                            "Several times in a week" = "Weekly",
                            "Once in a week" = "Weekly",
                            "Once in a month" = "Monthly",
                            "Several times in a month" = "Monthly",
                            "Once or several times a year" = "Yearly",
                            "I Prefer Internet Banking" = "Never",
                            "Never & I prefer to do this in the bank." = "Never",
                            "I prefer to do this from the internet banking via computer." = "Never"))

  df[,i] <- factor(df[,i], levels = c("Daily","Weekly",
                                      "Monthly","Yearly",
                                      "Never"))
}

# Clean "No" and "NO"
df$Find.Mobile.Banking.userfriendly <- revalue(df$Find.Mobile.Banking.userfriendly, c("NO"="No"))

# Set Mobile Operating System
df$OS = ifelse(grepl("Iphone", df$Phone.Model),"iPhone","Other")
df$OS = ifelse(is.na(df$Phone.Model), NA, df$OS)
df$OS = as.factor(df$OS)


################################################
#### 3. Data Exploration & Graphs          #####
################################################

                       
# Define St Galler Kantonalbank Color Scheme
colors=c('springgreen4','springgreen3','springgreen2','springgreen1','grey')

#### Graph 1: Feature to Usage ####

p <- likert(df[,14:59]) # using the likert library
plot(p,
     plot.percents = FALSE,
     plot.percent.low = FALSE,
     plot.percent.high = FALSE,
     centered = FALSE,
     center=4.5,
     include.center = FALSE,
     wrap=30,
     colors=colors) +
    ggtitle("Usage of Mobile Apps")


#### Graph 2: Top Features based on Usage #### 

# Get the Relative counts for the 
usage_sums = data.frame()
for (i in (14:59)) {
  title = names(df)[i]
  sum_daily = table(df[,i])
  row = c(title, sum_daily)
  usage_sums = rbind(usage_sums, row)
}
colnames(usage_sums) = c('Col','Daily','Weekly','Monthly','Yearly','Never') #set column names

# Make sure these are numeric values and not Characters
for (i in 2:6){
  usage_sums[,i] <- as.numeric(usage_sums[,i])
}

usage_sums = usage_sums[order(-usage_sums$Never),] #order them by daily use
usage_sums$order_n <- seq_len(nrow(usage_sums))

# Create Melted DF for Stacked Barcharts
melted_df = melt(usage_sums, id.vars = c("Col",'order_n'),
                 measure.vars = c("Daily", "Weekly", "Monthly","Yearly","Never"))


# Actual Plotting of Top 15 
melted_df %>% filter(order_n>31) %>% 
  ggplot(aes(x=value,y=reorder(Col,order_n),fill=variable))+
  geom_bar(stat = "identity",position = position_fill(reverse = TRUE,vjust = 1))+
  theme_classic()+scale_x_continuous(labels=scales::percent)+
  labs(y="",x="Percentage" , fill="Frequency") + 
  scale_fill_manual(values=colors)+
  theme(axis.ticks.y = element_blank(),)


#### Graph 3: Usage Sums by Various Factors ####

# Revalue a lot of bins to manageable bins
df[,9]<- revalue(df[,9], c("10+"="10-15",
                           "5+" = "5-10",
                           "4-6" = "0-5",
                           "Never"="None"))
df[,9] <- factor(df[,9], levels = c("None","0-5","5-10","10-15","15+"))  
df[,10]<- revalue(df[,10], c("0-3"="0-3",
                           "3" = "0-3",
                           "3+" = "3-5",
                           "4+"= "3-5",
                           "0-5"="3-5",
                           "5"="3-5",
                           "6"="5+",
                           "7"="5+",
                           "7+"="5+",
                           "Never"="None"))
df[,10] <- factor(df[,10], levels = c("None","0-3","3-5","5+"))

# Create a Dataframe showing the Amount of Features used Daily, Weekly, ...
df_usage = df

df_usage$Amount.Daily = rowSums(df_usage[,12:59] == "Daily", na.rm=TRUE)
df_usage$Amount.Weekly = rowSums(df_usage[,12:59] == "Weekly", na.rm=TRUE) + df_usage$Amount.Daily
df_usage$Amount.Monthly = rowSums(df_usage[,12:59] == "Monthly", na.rm=TRUE) + df_usage$Amount.Weekly
df_usage$Amount.Yearly = rowSums(df_usage[,12:59] == "Yearly", na.rm=TRUE) + df_usage$Amount.Monthly # Same as N-"Never" 


#### Graph 3.1: Years using Mobile Banking vs. Features used Monthly ####
df_usage %>% 
  drop_na(Years.Using.Mobile.Banking) %>%
  ggplot(aes(x=Years.Using.Mobile.Banking, y=Amount.Monthly)) +
  geom_boxplot()

df_usage %>% 
  drop_na(Years.Using.Internet.Banking) %>%
  ggplot(aes(x=Years.Using.Internet.Banking, y=Amount.Monthly)) +
  geom_boxplot()

#### Graph 3.2: Years using Mobile Banking vs. Features used Monthly incl. Confidence Interval ####

# This Function Returns the Mean, N, SD, and SE of the Dataset based on group
data_summary <- function(data, varname, groupnames){
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE),
      n = length(x[[col]]),
      se = sd(x[[col]], na.rm=TRUE)/sqrt(length(x[[col]])))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

# Create Dataframe for Barchart
df_barchart <- df_usage %>% 
  drop_na(Years.Using.Mobile.Banking) %>% 
  data_summary(varname="Amount.Yearly", 
               groupnames=c("Years.Using.Mobile.Banking"))

# Plot Barchart
df_barchart %>% 
  ggplot(aes(x=Years.Using.Mobile.Banking, y=Amount.Yearly)) + 
  geom_bar(stat="identity", position=position_dodge(), fill = "springgreen4") +
  theme_classic() +
  geom_errorbar(aes(ymin=Amount.Yearly-1.96*se, ymax=Amount.Yearly+1.96*se), width=.2,
                position=position_dodge(.9)) +
  ylab("Features used atleast Yearly") + xlab("Years Using Mobile Banking")

#### Graph 3.3: User-friendlyness vs. Features used Monthly incl. Confidence Interval ####

# Create Dataframe for Barchart
df_barchart <- df_usage %>% 
  drop_na(Find.Mobile.Banking.userfriendly) %>% 
  data_summary(varname="Amount.Monthly", 
               groupnames=c("Find.Mobile.Banking.userfriendly"))

# Plot Barchart
df_barchart %>% 
  ggplot(aes(x=Find.Mobile.Banking.userfriendly, y=Amount.Monthly)) + 
  geom_bar(stat="identity", position=position_dodge(), fill = "springgreen4") +
  theme_light() +
  geom_errorbar(aes(ymin=Amount.Monthly-1.96*se, ymax=Amount.Monthly+1.96*se), width=.2,
                position=position_dodge(.9)) 



#### Graph 3.4: Age vs. Features used Monthly incl. Confidence Interval ####

## Now by age
# Create Dataframe for Barchart
df_barchart <- df_usage %>% 
  drop_na(Age) %>% 
  data_summary(varname="Amount.Monthly", 
               groupnames=c("Age"))

# Plot Barchart
df_barchart %>% 
  ggplot(aes(x=Age, y=Amount.Monthly)) + 
  geom_bar(stat="identity", position=position_dodge(), fill = "springgreen4") +
  theme_light() +
  geom_errorbar(aes(ymin=Amount.Monthly-1.96*se, ymax=Amount.Monthly+1.96*se), width=.2,
                position=position_dodge(.9)) 

#### Graph 3.5: Age vs. Features used Monthly incl. Confidence Interval ####

# Create Dataframe for Barchart
df_barchart <- df_usage %>% 
  drop_na(Age) %>% 
  data_summary(varname="Amount.Weekly", 
               groupnames=c("Bank.Name"))

# Plot Barchart
df_barchart %>% 
  ggplot(aes(x=Bank.Name, y=Amount.Weekly)) + 
  geom_bar(stat="identity", position=position_dodge(), fill = "springgreen4") +
  theme_light() +
  geom_errorbar(aes(ymin=Amount.Weekly-1.96*se, ymax=Amount.Weekly+1.96*se), width=.2,
                position=position_dodge(.9)) 


#### Graph 3.6: Phone OS vs. Features used Yearly incl. Confidence Interval ####

# Create Dataframe for Barchart
df_barchart <- df_usage %>% 
  drop_na(OS) %>% 
  data_summary(varname="Amount.Yearly", 
               groupnames=c("OS"))

# Plot Barchart
df_barchart %>% 
  ggplot(aes(x=OS, y=Amount.Yearly)) + 
  geom_bar(stat="identity", position=position_dodge(), fill = "springgreen4") +
  theme_light() +
  geom_errorbar(aes(ymin=Amount.Yearly-1.96*se, ymax=Amount.Yearly+1.96*se), width=.2,
                position=position_dodge(.9)) 



#### Graph 3.7: Gender vs. Features used Yearly incl. Confidence Interval ####

# Create Dataframe for Barchart
df_barchart <- df_usage %>% 
  drop_na(Gender) %>% 
  data_summary(varname="Amount.Yearly", 
               groupnames=c("Gender"))

# Plot Barchart
df_barchart %>% 
  ggplot(aes(x=Gender, y=Amount.Yearly)) + 
  geom_bar(stat="identity", position=position_dodge(), fill = "springgreen4") +
  theme_light() +
  geom_errorbar(aes(ymin=Amount.Yearly-1.96*se, ymax=Amount.Yearly+1.96*se), width=.2,
                position=position_dodge(.9)) 


#### Graph 4: User Friendlyness vs. Years using Mobile Banking ####

df %>%
  drop_na(Years.Using.Mobile.Banking) %>%
  ggplot( 
    aes(x = Years.Using.Mobile.Banking, 
        fill = Find.Mobile.Banking.userfriendly)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion") +
  scale_fill_manual(values = c("lightgrey", "springgreen4")) +
  theme_classic() +
  labs(fill = str_wrap("Mobile Banking User-Friendly?", width = 18)) +
  xlab("Years using Mobile Banking") 

#### Many other graphs have been omitted from this code... ####

