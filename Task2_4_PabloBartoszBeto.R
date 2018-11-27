##################pacages###############################################################

pacman::p_load(caret, lattice, ggplot2, ModelMetrics, mlbench, RcppRoll, bindrcpp, backports, 
               ddalpha, DEoptimR, dimRed, gower, readr, rpart, cellranger, rpart.plot, plotly, 
               corrplot, arules, caTools, prabclus, trimcluster, prabclus, Matrix, trimcluster,
               grid, TSP, Matrix, prabclus, binr, OneR, arulesViz)


#if(sessionInfo()['basePkgs']=="dplyr" | sessionInfo()['otherPkgs']=="dplyr"){
#  detach(package:dplyr, unload=TRUE)
#}
#library(plyr)

#########################################################################################


setwd("C:/Users/Usuario/Desktop/Big Data/2.4")
# how remove duplcates works 
transactionsData <- read.transactions("ElectronidexTransactions2017.csv", format="basket",sep=",",rm.duplicates = TRUE)
#transactionsDataDubli <- read.transactions("ElectronidexTransactions2017.csv", format="basket",sep=",")


################# Function to predict profit of a rule ################
rulesbytransaction <- function(Item1, Item2, Item3, dataset, ruleName){
  
  ruleName$index <- c()
  ruleName$profit <- NULL
  
  for (m in 1:nrow(dataset)){
    t1=length(grep(Item1, LIST(dataset)[[m]]))
    t2=length(grep(Item2, LIST(dataset)[[m]]))
    t3=length(grep(Item3, LIST(dataset)[[m]]))
    
    
    if ( (t1>0) & (t2>0) & (t3>0) ){
      ruleName$index <- append(ruleName$index,m)
      ruleName$profit <- append(ruleName$profit,estimatedProfit[m])
      
    }
  }
  ruleName$avgProfit <- mean(ruleName$profit)
  return(ruleName)
}


inspect (transactionsData[2]) # You can view the transactions. Is there a way to see a certain # of transactions?

length (transactionsData) # Number of transactions.

size (transactionsData)# Number of items per transaction
LIST(transactionsData) # Lists the transactions by conversion (LIST must be capitalized)

itemLabels(transactionsData)# To see the item labels
summary(transactionsData)
itemFrequencyPlot(transactionsData, topN=10, type = "absolute", main = "Least sold products")




image(transactionsData) #better plot with more details !!!!!!!!!!!!!!!!!!!!!!!!!!!!
image(sample(transactionsData,10))




itemLabels(transactionsData)
#[1] "1TB Portable External Hard Drive"                      "2TB Portable External Hard Drive"                     
#[3] "3-Button Mouse"                                        "3TB Portable External Hard Drive"                     
#[5] "5TB Desktop Hard Drive"                                "AOC Monitor"                                          
#[7] "APIE Bluetooth Headphone"                              "ASUS 2 Monitor"                                       
#[9] "ASUS Chromebook"                                       "ASUS Desktop"                                         
#[11] "ASUS Monitor"                                          "Acer Aspire"                                          
#[13] "Acer Desktop"                                          "Acer Monitor"                                         
#[15] "Ailihen Stereo Headphones"                             "Alienware Laptop"                                     
#[17] "Apple Earpods"                                         "Apple MacBook Air"                                    
#[19] "Apple MacBook Pro"                                     "Apple Magic Keyboard"                                 
#[21] "Apple TV"                                              "Apple Wired Keyboard"                                 
#[23] "Apple Wireless Keyboard"                               "Audio Cable"                                          
#[25] "Backlit LED Gaming Keyboard"                           "Belkin Mouse Pad"                                     
#[27] "Bose Companion Speakers"                               "Brother Printer"                                      
#[29] "Brother Printer Toner"                                 "CYBERPOWER Gamer Desktop"                             
#[31] "Cambridge Bluetooth Speaker"                           "Canon Ink"                                            
#[33] "Canon Office Printer"                                  "Computer Game"                                        
#[35] "Cyber Acoustics"                                       "DOSS Touch Wireless Bluetooth"                        
#[37] "DYMO Label Manker"                                     "DYMO Labeling Tape"                                   
#[39] "Dell 2 Desktop"                                        "Dell Desktop"                                         
#[41] "Dell KM117 Wireless Keyboard & Mouse"                  "Dell Laptop"                                          
#[43] "Dell Monitor"                                          "Dell Wired Keyboard"                                  
#[45] "EagleTec Wireless Combo Keyboard and Mouse"            "Eluktronics Pro Gaming Laptop"                        
#[47] "Epson Black Ink"                                       "Epson Printer"                                        
#[49] "Etekcity Power Extension Cord Cable"                   "Ethernet Cable"                                       
#[51] "Fire HD Tablet"                                        "Fire TV Stick"                                        
#[53] "Full Motion Monitor Mount"                             "Gaming Mouse Professional"                            
#[55] "Generic Black 3-Button"                                "Google Home"                                          
#[57] "HDMI Adapter"                                          "HDMI Cable 6ft"                                       
#[59] "HP Black & Tri-color Ink"                              "HP Desktop"                                           
#[61] "HP Laptop"                                             "HP Monitor"                                           
#[63] "HP Notebook Touchscreen Laptop PC"                     "HP USB Keyboard"                                      
#[65] "HP Wireless Mouse"                                     "HP Wireless Printer"                                  
#[67] "Halter Acrylic Monitor Stand"                          "Halter Mesh Metal Monitor Stand"                      
#[69] "Height-Adjustable Standing Desk"                       "Intel Desktop"                                        
#[71] "JBL Splashproof Portable Bluetooth Speaker"            "Kensington Headphones"                                
#[73] "Kindle"                                                "Koss Home Headphones"                                 
#[75] "LG Monitor"                                            "LG Touchscreen Laptop"                                
#[77] "Large Mouse Pad"                                       "Lenovo Desktop Computer"                              
#[79] "Logitech 3-button Mouse"                               "Logitech ClearChat Headset"                           
#[81] "Logitech Desktop MK120 Mouse and keyboard Combo"       "Logitech Keyboard"                                    
#[83] "Logitech MK270 Wireless Keyboard and Mouse Combo"      "Logitech MK360 Wireless Keyboard and Mouse Combo"     
#[85] "Logitech MK550 Wireless Wave Keyboard and Mouse Combo" "Logitech Multimedia Speakers"                         
#[87] "Logitech Stereo Headset"                               "Logitech Wireless Keyboard"                           
#[89] "Logitech Wireless Mouse"                               "Mackie CR Speakers"                                   
#[91] "Microsoft Basic Optical Mouse"                         "Microsoft Headset"                                    
#[93] "Microsoft Office Home and Student 2016"                "Microsoft Wireless Comfort Keyboard and Mouse"        
#[95] "Microsoft Wireless Desktop Keyboard and Mouse"         "Monster Beats By Dr Dre"                              
#[97] "Multi Media Stand"                                     "Otium Wireless Sports Bluetooth Headphone"            
#[99] "PC Gaming Headset"                                     "Panasonic In-Ear Headphone"                           
#[101] "Panasonic On-Ear Stereo Headphones"                    "Philips Flexible Earhook Headphone"                   
#[103] "Redragon Gaming Mouse"                                 "Rii LED Gaming Keyboard & Mouse Combo"                
#[105] "Rii LED Keyboard"                                      "Rokono Mini Speaker"                                  
#[107] "Roku Express"                                          "Samsung Charging Cable"                               
#[109] "Samsung Galaxy Tablet"                                 "Samsung Monitor"                                      
#[111] "Sceptre Monitor"                                       "Slim 2TB Portable External Hard Drive"                
#[113] "Slim Wireless Mouse"                                   "Smart Light Bulb"                                     
#[115] "Sonos"                                                 "USB Cable"                                            
#[117] "VGA Monitor Cable"                                     "ViewSonic Monitor"                                    
#[119] "Wireless Portable Mouse"                               "XIBERIA Gaming Headset"                               
#[121] "Zombie Gaming Headset"                                 "iMac"                                                 
#[123] "iPad"                                                  "iPad Pro"                                             
#[125] "iPhone Charger Cable"   



newLabels<- c("ExternalHD-1TB Portable External Hard Drive","ExternalHD-2TB Portable External Hard Drive","Computer Mice-3Button Mouse","ExternalHD-3TB Portable External Hard Drive",
              "ExternalHD-5TB Desktop Hard Drive","Laptops-Acer Aspire","Desktops-Acer Desktop","Monitors-Acer Monitor","ComputerHeadphones-Ailihen Stereo Headphones","Laptops-Alienware Laptop","Monitors-AOC Monitor","ActiveHeadphones-APIE Bluetooth Headphone","ActiveHeadphones-Apple Earpods",
              "Laptops-Apple MacBook Air","Laptops-Apple MacBook Pro","Keyboards-Apple Magic Keyboard","SmartHomeDevices-Apple TV","Keyboards-Apple Wired Keyboard","Keyboards-Apple Wireless Keyboard","ComputerCords-Audio Cable",
              "Monitors-ASUS 2 Monitor","Laptops-ASUS Chromebook","Desktops-ASUS Desktop","Monitors-ASUS Monitor",
              "Keyboards-Backlit LED Gaming Keyboard","Accessories-Belkin Mouse Pad","Speakers-Bose Companion Speaker","Printers-Brother Printer","Printers-Brother Printer Toner","Speakers-Cambridge Bluetooth Speaker",
              "Cartridge-Canon Ink","Printers-Canon Office Printer","Software-Computer Game","Speakers-Cyber Acoustics","Desktops-CYBERPOWER Gamer Desktop","Desktops-Dell 2 Desktop","Desktops-Dell Desktop",
              "MKCombo-Dell KM117 Wireless Keyboard & Mouse","Laptops-Dell Laptop","Monitors-Dell Monitor","Keyboards-Dell Wired Keyboard","Speakers-DOSS Touch Wireless Bluetooth","Printers-DYMO Label Manker","Cartridge-DYMO Labeling Tape","MKCombo-EagleTec Wireless Combo Keyboard and Mouse","Laptops-Eluktronics Pro Gaming Laptop","Cartridge-Epson Black Ink","Printers-Epson Printer",
              "ComputerCords-Etekcity Power Extension Cord Cable","ComputerCords-Ethernet Cable","Tablets-Fire HD Tablet","SmartHomeDevices-Fire TV Stick","Stands-Full Motion Monitor Mount","Computer Mice-Gaming Mouse Professional","Computer Mice-Generic Black 3-Button","SmartHomeDevices-Google Home",
              "Stands-Halter Acrylic Monitor Stand","Stands-Halter Mesh Metal Monitor Stand",
              "ComputerCords-HDMI Adapter","ComputerCords-HDMI Cable 6ft","Stands-HeightAdjustable Standing Desk","Cartridge-HP Black & Tri-color Ink","Desktops-HP Desktop","Laptops-HP Laptop","Monitors-HP Monitor","Laptops-HP Notebook Touchscreen Laptop PC","Keyboards-HP USB Keyboard","Computer Mice-HP Wireless Mouse","Printers-HP Wireless Printer",
              "Desktops-iMac","Desktops-Intel Desktop","Tablets-iPad","Tablets-iPadPro",
              "ComputerCords-iPhone Charger Cable",
              "Speakers-JBL Splashproof Portable Bluetooth Speaker","ComputerHeadphones-Kensington Headphones","Tablets-Kindle","ComputerHeadphones-Koss Home Headphones",
              "Accessories-Large Mouse Pad","Desktops-Lenovo Desktop Computer","Monitors-LG Monitor","Laptops-LG Touchscreen Laptop","Computer Mice-Logitech 3-button Mouse","ComputerHeadphones-Logitech ClearChat Headset","MKCombo-Logitech Desktop MK120 Mouse and keyboard Combo","Keyboards-Logitech Keyboard",
              "MKCombo-Logitech MK270 Wireless Keyboard and Mouse Combo","MKCombo-Logitech MK360 Wireless Keyboard and Mouse Combo","MKCombo-Logitech MK550 Wireless Wave Keyboard and Mouse Combo","Speakers-Logitech Multimedia Speaker","ComputerHeadphones-Logitech Stereo Headset","Keyboards-Logitech Wireless Keyboard",
              "Computer Mice-Logitech Wireless Mouse","Speakers-Mackie CR Speaker","Computer Mice-Microsoft Basic Optical Mouse","ComputerHeadphones-Microsoft Headset","Software-Microsoft Office Home and Student 2016","MKCombo-Microsoft Wireless Comfort Keyboard and Mouse","MKCombo-Microsoft Wireless Desktop Keyboard and Mouse",
              "ActiveHeadphones-Monster Beats By Dr Dre","Stands-Multi Media Stand","ActiveHeadphones-Otium Wireless Sports Bluetooth Headphone","ActiveHeadphones-Panasonic In-Ear Headphone","ComputerHeadphones-Panasonic On-Ear Stereo Headphones","ComputerHeadphones-PC Gaming Headset","ActiveHeadphones-Philips Flexible Earhook Headphone",
              "Computer Mice-Redragon Gaming Mouse","MKCombo-Rii LED Gaming Keyboard & Mouse Combo","Keyboards-Rii LED Keyboard","Speakers-Rokono Mini Speaker","SmartHomeDevices-Roku Express","ComputerCords-Samsung Charging Cable","Tablets-Samsung Galaxy Tablet","Monitors-Samsung Monitor","Monitors-Sceptre Monitor","ExternalHD-Slim 2TB Portable External Hard Drive",
              "Computer Mice-Slim Wireless Mouse","SmartHomeDevices-Smart Light Bulb","Speakers-Sonos","ComputerCords-USB Cable","ComputerCords-VGA Monitor Cable","Monitors-ViewSonic Monitor","Computer Mice-Wireless Portable Mouse","ComputerHeadphones-XIBERIA Gaming Headset","ComputerHeadphones-Zombie Gaming Headset")
newLabels
itemLabels(transactionsData)<-newLabels

itemLabels(transactionsData)

?sub

## trim trailing white space
#str <- "Now is the time      "
#sub(" +$", "", str)  ## spaces only
#sub("[[:space:]]+$", "", str) ## white space, POSIX-style
## what is considered 'white space' depends on the locale.
## what PCRE considered white space changed in version 8.34: see ?regex
#sub("\\s+$", "", str, perl = TRUE) ## PCRE-style white space



categories<-as.factor(sub("\\-.*", "", newLabels)) # new labels as factors for aproiri 

str(transactionsData)
categories
str(categories)
length(categories)

transactionsData@itemInfo
transactionsData@itemInfo$cat<-categories


categories2<-(sub("\\-.*", "", newLabels))#Labels for spliting data and might be not neccesery 

repitedCategories
length(repitedCategories)
transactionsData@itemInfo
transactionsData@itemInfo$cat2<-repitedCategories
repitedCategories <- transactionsData
repitedCategories@itemInfo$labels <- categories2

CategoriesTransaction<- aggregate(transactionsData, transactionsData@itemInfo[["cat"]])
CategoriesTransaction
itemLabels(CategoriesTransaction)

itemFrequencyPlot(transactionsData, topN=10, type = "absolute", main = "Top 10 products sold")


################## Rules ##############

ByCategoriesDL<- apriori(CategoriesTransaction,parameter = list(support=0.05,conf=0.5),
                         appearance = list(rhs=c("Desktops"),lhs=c("Accessories","ActiveHeadphones","Cartridge","Computer Mice","ComputerCords","ComputerHeadphones","ExternalHD",
                                                                   "Keyboards","MKCombo","Monitors","Printers","SmartHomeDevices","Software","Speakers","Stands","Tablets", "Laptops" ),
                                           default="none"))

inspect(head(sort(ByCategoriesDL,by="support"),20))



ByCategoriesDL2<- apriori(CategoriesTransaction,parameter = list(support=0.05,conf=0.2),
                         appearance = list(lhs=c("Accessories","ActiveHeadphones","Cartridge","Computer Mice",
                                                 "ComputerCords","ComputerHeadphones","ExternalHD",
                                                 "Keyboards","MKCombo","Monitors","Printers","SmartHomeDevices",
                                                 "Software","Speakers","Stands","Tablets"),
                                           default="none"))

inspect(head(sort(ByCategoriesDL2,by="support"),20))



ByCategories <- list()
categoriesNames <- c("Accessories","ActiveHeadphones","Cartridge","Computer Mice","ComputerCords",
                     "ComputerHeadphones","ExternalHD", "Laptops","Keyboards","MKCombo","Monitors",
                     "Printers","SmartHomeDevices","Software","Speakers","Stands","Tablets", "Desktops")


############### Rules  #################
RulesName<- apriori (transactionsData, parameter = list(supp = 0.005, conf = 0.2, maxlen=2 )) # appearance = list(rhs=newLabels[i],lhs=newLabels[-i],default="none"))
is.significant(RulesName, transactionsData)

inspect(RulesName[is.significant(RulesName, transactionsData)])


############## Rules by product predicted #############
ByProducts <- list()
ProductPredicted <- c()
for(i in 1:length(newLabels)){
  #ByProducts[[i]] <- apriori(transactionsData, parameter = list(support=0.005,conf=0.2),
  #                             appearance = list(rhs=newLabels[i],lhs=newLabels[-i],default="none"))
  inspect(head(sort(ByProducts[[i]],by="lift"),20))
  #ProductPredicted <- append(ProductPredicted,inspect(head(sort(ByProducts[[i]],by="lift"),20)))
  print(i)
}
names(ByProducts) <- newLabels

plot(ByProducts[[13]], method="grouped")


############### Rules by predictor product #################

ByProducts2 <- list()
for(i in 1:length(newLabels)){
  #ByProducts2[[i]] <- apriori(transactionsData, parameter = list(support=0.005,conf=0.2, maxlen=2, minlen=1),
  #                           appearance = list(lhs=newLabels[i],rhs=newLabels[-i],default="none"))
  inspect(head(sort(ByProducts2[[i]],by="support"),20))
  print(i)
}
names(ByProducts2) <- newLabels

plot(ByProducts2[[i]], method="grouped")


############### Rules by category predicted #################

for(i in 1:length(categoriesNames)){
  ByCategories[[i]] <- apriori(CategoriesTransaction, parameter = list(support=0.05,conf=0.5),
                               appearance = list(rhs=categoriesNames[i],lhs=categoriesNames[-i],default="none"))
  inspect(head(sort(ByCategories[[i]],by="lift"),20))
}
names(ByCategories) <- categoriesNames



############### Rules by predictor category #################

for(i in 1:length(categoriesNames)){
  ByCategories[[i]] <- apriori(CategoriesTransaction, parameter = list(support=0.05,conf=0.5),
                               appearance = list(rhs=categoriesNames[i],lhs=categoriesNames[-i],default="none"))
  inspect(head(sort(ByCategories[[i]],by="lift"),20))
}
names(ByCategories) <- categoriesNames


############### Bussiness Data Set ##############

bussiness <- c()

for (m in 1:nrow(repitedCategories)){
  #trasactionsvector <- unname(unlist(CategoriesTransaction[m]))
  item = length(LIST(repitedCategories)[[m]])
  c=length(grep("Monitors", LIST(repitedCategories)[[m]]))+length(grep("Desktops", LIST(repitedCategories)[[m]]))+length(grep("Laptops", LIST(repitedCategories)[[m]]))
  p=length(grep("Printers", LIST(repitedCategories)[[m]]))
  s=length(grep("Software", LIST(repitedCategories)[[m]]))
  st=length(grep("Stands", LIST(repitedCategories)[[m]]))
  mk=length(grep("Keyboards", LIST(repitedCategories)[[m]]))+length(grep("MKCombo", LIST(repitedCategories)[[m]]))+length(grep("Computer Mice", LIST(repitedCategories)[[m]]))
  t=length(grep("Tablets", LIST(repitedCategories)[[m]]))
  hd=length(grep("ExternalHD", LIST(repitedCategories)[[m]]))
  
  
  
  if ( (item>8) | (c>2) | (p > 1) | (s>2) | (st>2) | (mk>2) | (t>2) | (hd>2) ){
    bussiness <- append(bussiness,m)
  }
}

itemFrequencyPlot(transactionsData[bussiness], topN=10, type = "absolute", main = "Top 10 products sold to businesses")

LIST(transactionsData[bussiness])

################ Gamers ######################
gamers <- c()

for (m in 1:nrow(transactionsData)){
  g=length(grep("gam", LIST(transactionsData)[[m]]))+length(grep("Gam", LIST(transactionsData)[[m]]))
  if ( (g>1) ){
    gamers <- append(gamers,m)
  }
}

itemFrequencyPlot(transactionsData[gamers], topN=10, type = "absolute", main = "Top 10 products sold")


################ Regular Customers ###########

regular <- regular[-bussiness]

################ Estimated Profit #######################

estimatedProfit <- c()
for (m in 1:nrow(transactionsData)){
  
  price=527.79*length(grep("Monitors", LIST(repitedCategories)[m]))+736.34*length(grep("Desktops", LIST(repitedCategories)[m]))+
    772.73*length(grep("Laptops", LIST(repitedCategories)[m]))+42.77*length(grep("Accesories", LIST(repitedCategories)[m]))+
    42.77*length(grep("Stands", LIST(repitedCategories)[m]))+42.77*length(grep("Computer Mice", LIST(repitedCategories)[m]))+
    42.77*length(grep("ComputerCords", LIST(repitedCategories)[m]))+42.77*length(grep("ComputerHeadphones", LIST(repitedCategories)[m]))+
    42.77*length(grep("ActiveHeadphones", LIST(repitedCategories)[m]))+203.89*length(grep("Printers", LIST(repitedCategories)[m]))+
    497.99*length(grep("Tablets", LIST(repitedCategories)[m]))+34.25*length(grep("Cartridge", LIST(repitedCategories)[m]))+
    93.98*length(grep("Software", LIST(repitedCategories)[m]))+42.77*length(grep("Keyboards", LIST(repitedCategories)[m]))+
    42.77*length(grep("MKCombo", LIST(repitedCategories)[m]))+42.77*length(grep("ExternalHD", LIST(repitedCategories)[m]))+
    200*length(grep("SmartHomeDevices", LIST(repitedCategories)[m]))+70*length(grep("Speakers", LIST(repitedCategories)[m]))
 
  estimatedProfit[m] <- price 
  }

plot(estimatedProfit,type='h')
plot_ly(x=estimatedProfit, type="histogram")


############# Bins for the Profit ############

bins <- bin(estimatedProfit, nbins=4, method="content")
plot_ly(x=bins)

profitbins <- list()

for (m in 1:nrow(transactionsData)){
  if ( (estimatedProfit<475) ){
    profitbins[[1]] <- append((profitbins)[[1]],m)
  }
   if ( (estimatedProfit>475) & (estimatedProfit< 725) ){
    profitbins[[2]] <- append((profitbins)[[2]],m)
   }
  if ( (estimatedProfit>725) & (estimatedProfit< 1225) ){
    profitbins[[3]] <- append((profitbins)[[3]],m)
  }
  if ( (estimatedProfit>1225) & (estimatedProfit< 2025) ){
    profitbins[[4]] <- append((profitbins)[[4]],m)
  }
  if ( (estimatedProfit>2025) ){
    profitbins[[5]] <- append((profitbins)[[5]],m)
  }
}



############ Transactions afected by a rule ################

RuleDeskMonKey <- c()
RuleDeskMonkey <- rulesbytransaction("Desktops", "Monitors", "Keyboards", CategoriesTransaction, RuleDeskMonKey)






summary(TestSupport)


TestRule<- apriori(transactionsData,parameter = list(support=0.001,conf=0.6))  
summary(TestRule)
inspect(head(sort(TestRule,by="lift"),20))

TestRule


#remove redudent files 
#?is.redundant
rerules <-is.redundant(TestRule)
summary(rerules)
list(rerules)
TestRule <-TestRule[!rerules]
TestRule



#####################################################################################

TestRule<- apriori(transactionsData,parameter = list(support=0.001,conf=0.6))  
suemmary(TestRule)
inspect(head(sort(TestRule,by="lift"),10))




#plots here 
plot(TestRule,method="grouped")
plot(sort(TestRule,by="lift"),10)
?plot

plot




################ Least Sold Products #################

ProductVolumes <- c()

#Modify
for (m in 1:nrow(repitedCategories)){
  
  item = length(LIST(repitedCategories)[[m]])
  c=length(grep("Monitors", LIST(repitedCategories)[[m]]))+length(grep("Desktops", LIST(repitedCategories)[[m]]))+length(grep("Laptops", LIST(repitedCategories)[[m]]))
  p=length(grep("Printers", LIST(repitedCategories)[[m]]))
  s=length(grep("Software", LIST(repitedCategories)[[m]]))
  st=length(grep("Stands", LIST(repitedCategories)[[m]]))
  mk=length(grep("Keyboards", LIST(repitedCategories)[[m]]))+length(grep("MKCombo", LIST(repitedCategories)[[m]]))+length(grep("Computer Mice", LIST(repitedCategories)[[m]]))
  t=length(grep("Tablets", LIST(repitedCategories)[[m]]))
  hd=length(grep("ExternalHD", LIST(repitedCategories)[[m]]))
  
  
  
  if ( (item>8) | (c>2) | (p > 1) | (s>2) | (st>2) | (mk>2) | (t>2) | (hd>2) ){
    bussiness <- append(bussiness,m)
  }
}

itemFrequencyPlot(transactionsData[bussiness], topN=10, type = "absolute", main = "Top 10 products sold to businesses")

LIST(transactionsData[bussiness])
