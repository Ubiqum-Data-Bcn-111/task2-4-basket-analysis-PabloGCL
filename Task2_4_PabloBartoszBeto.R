##################pacages###############################################################

pacman::p_load(caret, lattice, ggplot2, ModelMetrics, mlbench, RcppRoll, bindrcpp, backports, 
               ddalpha, DEoptimR, dimRed, gower, readr, rpart, cellranger, rpart.plot, plotly, 
               corrplot, arules, caTools, prabclus, trimcluster, prabclus, Matrix, trimcluster,
               grid, TSP, Matrix, prabclus)


#if(sessionInfo()['basePkgs']=="dplyr" | sessionInfo()['otherPkgs']=="dplyr"){
#  detach(package:dplyr, unload=TRUE)
#}
#library(plyr)

#########################################################################################


setwd("C:/Users/Usuario/Desktop/Big Data/2.4")
# how remove duplcates works 
transactionsData <- read.transactions("ElectronidexTransactions2017.csv", format="basket",sep=",",rm.duplicates = TRUE)
#transactionsDataDubli <- read.transactions("ElectronidexTransactions2017.csv", format="basket",sep=",")



inspect (transactionsData[2]) # You can view the transactions. Is there a way to see a certain # of transactions?

length (transactionsData) # Number of transactions.

size (transactionsData)# Number of items per transaction
list(transactionsData) # Lists the transactions by conversion (LIST must be capitalized)

itemLabels(transactionsData)# To see the item labels
summary(transactionsData)
itemFrequencyPlot(transactionsData, topN=10, type = "absolute", main = "Top 10 products sold")




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
              "ExternalHD-5TB Desktop Hard Drive","Monitors-AOC Monitor","ActiveHeadphones-APIE Bluetooth Headphone","Monitors-ASUS 2 Monitor","Laptops-ASUS Chromebook","Desktops-ASUS Desktop",
              "Monitors-ASUS Monitor","Laptops-Acer Aspire","Desktops-Acer Desktop","Monitors-Acer Monitor","ComputerHeadphones-Ailihen Stereo Headphones","Laptops-Alienware Laptop","ActiveHeadphones-Apple Earpods",
              "Laptops-Apple MacBook Air","Laptops-Apple MacBook Pro","Keyboards-Apple Magic Keyboard","SmartHomeDevices-Apple TV","Keyboards-Apple Wired Keyboard","Keyboards-Apple Wireless Keyboard","ComputerCords-Audio Cable",
              "Keyboards-Backlit LED Gaming Keyboard","Accessories-Belkin Mouse Pad","Speakers-Bose Companion Speaker","Printers-Brother Printer","Printers-Brother Printer Toner","Desktops-CYBERPOWER Gamer Desktop","Speakers-Cambridge Bluetooth Speaker",
              "Cartridge-Canon Ink","Printers-Canon Office Printer","Software-Computer Game","Speakers-Cyber Acoustics","Speakers-DOSS Touch Wireless Bluetooth","Printers-DYMO Label Manker","Cartridge-DYMO Labeling Tape","Desktops-Dell 2 Desktop","Desktops-Dell Desktop",
              "MKCombo-Dell KM117 Wireless Keyboard & Mouse","Laptops-Dell Laptop","Monitors-Dell Monitor","Keyboards-Dell Wired Keyboard","MKCombo-EagleTec Wireless Combo Keyboard and Mouse","Laptops-Eluktronics Pro Gaming Laptop","Cartridge-Epson Black Ink","Printers-Epson Printer",
              "ComputerCords-Etekcity Power Extension Cord Cable","ComputerCords-Ethernet Cable","Tablets-Fire HD Tablet","SmartHomeDevices-Fire TV Stick","Stands-Full Motion Monitor Mount","Computer Mice-Gaming Mouse Professional","Computer Mice-Generic Black 3-Button","SmartHomeDevices-Google Home",
              "ComputerCords-HDMI Adapter","ComputerCords-HDMI Cable 6ft","Cartridge-HP Black & Tri-color Ink","Desktops-HP Desktop","Laptops-HP Laptop","Monitors-HP Monitor","Laptops-HP Notebook Touchscreen Laptop PC","Keyboards-HP USB Keyboard","Computer Mice-HP Wireless Mouse","Printers-HP Wireless Printer",
              "Stands-Halter Acrylic Monitor Stand","Stands-Halter Mesh Metal Monitor Stand","Stands-HeightAdjustable Standing Desk","Desktops-Intel Desktop","Speakers-JBL Splashproof Portable Bluetooth Speaker","ComputerHeadphones-Kensington Headphones","Tablets-Kindle","ComputerHeadphones-Koss Home Headphones",
              "Monitors-LG Monitor","Laptops-LG Touchscreen Laptop","Accessories-Large Mouse Pad","Desktops-Lenovo Desktop Computer","Computer Mice-Logitech 3-button Mouse","ComputerHeadphones-Logitech ClearChat Headset","MKCombo-Logitech Desktop MK120 Mouse and keyboard Combo","Keyboards-Logitech Keyboard",
              "MKCombo-Logitech MK270 Wireless Keyboard and Mouse Combo","MKCombo-Logitech MK360 Wireless Keyboard and Mouse Combo","MKCombo-Logitech MK550 Wireless Wave Keyboard and Mouse Combo","Speakers-Logitech Multimedia Speaker","ComputerHeadphones-Logitech Stereo Headset","Keyboards-Logitech Wireless Keyboard",
              "Computer Mice-Logitech Wireless Mouse","Speakers-Mackie CR Speaker","Computer Mice-Microsoft Basic Optical Mouse","ComputerHeadphones-Microsoft Headset","Software-Microsoft Office Home and Student 2016","MKCombo-Microsoft Wireless Comfort Keyboard and Mouse","MKCombo-Microsoft Wireless Desktop Keyboard and Mouse",
              "ActiveHeadphones-Monster Beats By Dr Dre","Stands-Multi Media Stand","ActiveHeadphones-Otium Wireless Sports Bluetooth Headphone","ComputerHeadphones-PC Gaming Headset","ActiveHeadphones-Panasonic In-Ear Headphone","ComputerHeadphones-Panasonic On-Ear Stereo Headphones","ActiveHeadphones-Philips Flexible Earhook Headphone",
              "Computer Mice-Redragon Gaming Mouse","MKCombo-Rii LED Gaming Keyboard & Mouse Combo","Keyboards-Rii LED Keyboard","Speakers-Rokono Mini Speaker","SmartHomeDevices-Roku Express","ComputerCords-Samsung Charging Cable","Tablets-Samsung Galaxy Tablet","Monitors-Samsung Monitor","Monitors-Sceptre Monitor","ExternalHD-Slim 2TB Portable External Hard Drive",
              "Computer Mice-Slim Wireless Mouse","SmartHomeDevices-Smart Light Bulb","Speakers-Sonos","ComputerCords-USB Cable","ComputerCords-VGA Monitor Cable","Monitors-ViewSonic Monitor","Computer Mice-Wireless Portable Mouse","ComputerHeadphones-XIBERIA Gaming Headset","ComputerHeadphones-Zombie Gaming Headset","Desktops-iMac","Tablets-iPad","Tablets-iPadPro",
              "ComputerCords-iPhone Charger Cable")
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

categories<-as.factor(sub("\\-.*", "", newLabels)) #read more about this 



categories
str(categories)
  transactionsData@itemInfo$cat<-categories


CategoriesTransaction<- aggregate(transactionsData, transactionsData@itemInfo[["cat"]])
CategoriesTransaction
itemLabels(CategoriesTransaction)





ByCategoriesDL<- apriori(CategoriesTransaction,parameter = list(support=0.05,conf=0.5),
                         appearance = list(rhs=c("Desktops"),lhs=c("Accessories","ActiveHeadphones","Cartridge","Computer Mice","ComputerCords","ComputerHeadphones","ExternalHD",
                                                                   "Keyboards","MKCombo","Monitors","Printers","SmartHomeDevices","Software","Speakers","Stands","Tablets" ),
                                           default="none"))

inspect(head(sort(ByCategoriesDL,by="support"),20))


ByCategories <- list()
categoriesNames <- c("Accessories","ActiveHeadphones","Cartridge","Computer Mice","ComputerCords",
                     "ComputerHeadphones","ExternalHD", "Laptops","Keyboards","MKCombo","Monitors",
                     "Printers","SmartHomeDevices","Software","Speakers","Stands","Tablets", "Desktops")

# Rules by category predicted

for(i in 1:length(categoriesNames)){
  ByCategories[[i]] <- apriori(CategoriesTransaction,parameter = list(support=0.05,conf=0.5),
                               appearance = list(rhs=categoriesNames[i],lhs=categoriesNames[-i],default="none"))
  inspect(head(sort(ByCategories[[i]],by="lift"),20))
}
names(ByCategories) <- categoriesNames



bussiness <- c()
m=1L
for (m in 1:nrow(transactionsData)){
  #trasactionsvector <- unname(unlist(CategoriesTransaction[m]))
  item = size(CategoriesTransaction[m])
  c=length(grep("Monitors", LIST(CategoriesTransaction)[m]))+length(grep("Desktops", LIST(CategoriesTransaction)[m]))+length(grep("Laptops", LIST(CategoriesTransaction)[m]))
  p=length(grep("Printers", LIST(CategoriesTransaction)[[m]]))
  s=length(grep("Software", LIST(CategoriesTransaction)[[m]]))
  st=length(grep("Stands", LIST(CategoriesTransaction)[[m]]))
  mk=length(grep("Keyboards", LIST(CategoriesTransaction)[[m]]))+length(grep("MKCombo", LIST(CategoriesTransaction)[[m]]))+length(grep("Computer Mice", LIST(CategoriesTransaction)[[m]]))
  t=length(grep("Tablets", LIST(CategoriesTransaction)[[m]]))
  hd=length(grep("ExternalHD", LIST(CategoriesTransaction)[[m]]))
  
  
  
  if ( (item>8) | (c>2) | (p > 1) | (s>2) | (st>2) | (mk>2) | (t>2) | (hd>2) ){
    bussiness <- append(bussiness,m)
  }
}


estimatedProfit <- c()

for (n in 1:nrow(transactionsData)){
  
  price=527.79*length(grep("Monitors", LIST(CategoriesTransaction)[m]))+736.34*length(grep("Desktops", LIST(CategoriesTransaction)[m]))+
    772.73*length(grep("Laptops", LIST(CategoriesTransaction)[m]))+42.77*length(grep("Accesories", LIST(CategoriesTransaction)[m]))+
    42.77*length(grep("Stands", LIST(CategoriesTransaction)[m]))+42.77*length(grep("Computer Mice", LIST(CategoriesTransaction)[m]))+
    42.77*length(grep("ComputerCords", LIST(CategoriesTransaction)[m]))+42.77*length(grep("ComputerHeadphones", LIST(CategoriesTransaction)[m]))+
    42.77*length(grep("ActiveHeadphones", LIST(CategoriesTransaction)[m]))+203.89*length(grep("Printers", LIST(CategoriesTransaction)[m]))+
    497.99*length(grep("Tablets", LIST(CategoriesTransaction)[m]))+34.25*length(grep("Cartridge", LIST(CategoriesTransaction)[m]))+
    93.98*length(grep("Software", LIST(CategoriesTransaction)[m]))+42.77*length(grep("Keyboards", LIST(CategoriesTransaction)[m]))+
    42.77*length(grep("MKCombo", LIST(CategoriesTransaction)[m]))+42.77*length(grep("ExternalHD", LIST(CategoriesTransaction)[m]))+
    200*length(grep("SmartHomeDevices", LIST(CategoriesTransaction)[m]))+70*length(grep("Speakers", LIST(CategoriesTransaction)[m]))
 
  estimatedProfit[i] <- price 
  }





#list(rhs = "language in home=english", default = "lhs"))
#"rules", lhs=itemSetdiff(i[4:6],i[1:3]), rhs=i[1:3],
#quality = data.frame(support = runif(3)))
#inspect(rules)
#appearance = list(lhs=c("HouseOwnerFlag=0", "HouseOwnerFlag=1"), 
# rhs=paste0("Product=", unique(sales$Product)), default="none"))
#rules <- new("rules", lhs=itemSetdiff(i[4:6],i[1:3]), rhs=i[1:3],
# quality = data.frame(support = runif(3)))

#ByCategoriesDL<- apriori(CategoriesTransaction,parameter = list(support=0.001,conf=0.4),
#                         appearance = list(rhs=c("Desktops","Laptops"),))

inspect(head(sort(ByCategories,by="lift"),20))



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
plot(sort(TestRule,by="lift"),10))
?plot

plot


######bins 

#Desktop <- c('Lenovo Desktop Computer','iMac','HP Desktop','ASUS Desktop','Dell Desktop',
#             'Intel Desktop','Acer Desktop','CYBERPOWER Gamer Desktop','Dell 2 Desktop')

#Monitors <- c('Acer Monitor','LG Monitor','ASUS Monitor','ASUS 2 Monitor','Dell Monitor',
#              'Samsung Monitor','Sceptre Monitor','ViewSonic Monitor','AOC Monitor','HP Monitor')

#ComputerMice <- c('3-Button Mouse','Logitech Wireless Mouse','Microsoft Basic Optical Mouse',
#                  'Logitech 3-button Mouse','Redragon Gaming Mouse','HP Wireless Mouse',
#                  'Generic Black 3-Button','Wireless Portable Mouse','Gaming Mouse Professional',
#                  'Slim Wireless Mouse')

#Keyboard <- c('HP USB Keyboard','Logitech Wireless Keyboard','Rii LED Keyboard','Logitech Keyboard',
#              'Backlit LED Gaming Keyboard','Dell Wired Keyboard','Apple Wired Keyboard',
#              'Apple Wireless Keyboard','Apple Magic Keyboard')

#MouseKeyboardCombo <- c('Logitech MK550 Wireless Wave Keyboard and Mouse Combo',
#                        'Logitech Desktop MK120 Mouse and keyboard Combo',
#                        'Logitech MK270 Wireless Keyboard and Mouse Combo','Dell KM117 Wireless Keyboard & Mouse',
#                        'EagleTec Wireless Combo Keyboard and Mouse','Microsoft Wireless Comfort Keyboard and Mouse',
#                        'Microsoft Wireless Desktop Keyboard and Mouse','Rii LED Gaming Keyboard & Mouse Combo',
#                        'Logitech MK360 Wireless Keyboard and Mouse Combo')

#ComputerHeadphones <- c('Zombie Gaming Headset','Logitech ClearChat Headset',
#                        'Panasonic On-Ear Stereo Headphones','PC Gaming Headset',
#                        'Kensington Headphones','Logitech Stereo Headset','Koss Home Headphones',
#                        'Microsoft Headset','Ailihen Stereo Headphones','XIBERIA Gaming Headset')

#ActiveHeadphones <- c('Apple Earpods','Monster Beats By Dr Dre','Otium Wireless Sports Bluetooth Headphone',
#                      'Panasonic In-Ear Headphone','APIE Bluetooth Headphone',
#                      'Philips Flexible Earhook Headphone')

#ComputerCords <- c('HDMI Cable 6ft','Ethernet Cable','Etekcity Power Extension Cord Cable','Audio Cable',
#                   'VGA Monitor Cable','iPhone Charger Cable','HDMI Adapter','USB Cable',
#                   'Samsung Charging Cable')

#Accessories <- c('Microsoft Office Home and Student 2016','Computer Game','Belkin Mouse Pad',
#                 'Large Mouse Pad')

#Speakers <- c('Cambridge Bluetooth Speaker','JBL Splashproof Portable Bluetooth Speaker',
#              'DOSS Touch Wireless Bluetooth','Logitech Multimedia Speakers','Rokono Mini Speaker',
#              'Cyber Acoustics','Bose Companion Speakers','Mackie CR Speakers','Sonos')

#Printers <- c('Epson Printer','HP Wireless Printer','Canon Office Printer','Brother Printer',
#              'DYMO Label Manker')

#PrinterInk <- c('Epson Black Ink','HP Black & Tri-color Ink','Canon Ink','Brother Printer Toner',
#                'DYMO Labeling Tape')

#ComputerStands <-c('Halter Acrylic Monitor Stand','Height-Adjustable Standing Desk','Multi Media Stand',
#                   'Halter Mesh Metal Monitor Stand','Full Motion Monitor Mount')

#ComputerTablets <- c('iPad','iPad Pro','Fire HD Tablet','Samsung Galaxy Tablet','Kindle')

#ExternalHardrives <- c('1TB Portable External Hard Drive','2TB Portable External Hard Drive',
#                       '5TB Desktop Hard Drive','Slim 2TB Portable External Hard Drive',
#                       '3TB Portable External Hard Drive')

#SmartHomeDevices <- c('Apple TV','Google Home','Smart Light Bulb','Fire TV Stick','Roku Express')
