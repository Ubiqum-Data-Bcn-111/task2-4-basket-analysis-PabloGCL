pacman::p_load(caret, lattice, ggplot2, ModelMetrics, mlbench, RcppRoll, bindrcpp, backports, 
               ddalpha, DEoptimR, dimRed, gower, readr, rpart, cellranger, rpart.plot, plotly, 
               corrplot, arules, caTools, prabclus, trimcluster)

install.packages("arulesViz", dependencies = TRUE)
library(arulesViz)

setwd("C:/Users/Usuario/Desktop/Big Data/2.4")

DataFrame <- read.csv("ElectronidexTransactions2017.csv", header=FALSE, sep=",")

transactionsWrepited <- read.transactions("ElectronidexTransactions2017.csv", sep=",", rm.duplicates=FALSE, 
                                          format="basket")
transactions <- read.transactions("ElectronidexTransactions2017.csv", sep=",", rm.duplicates=TRUE)
#distribution of transactions with duplicates:
 #   items
 #   1    2 
 #   191  10 

inspect (transactions) # You can view the transactions. Is there a way to see a certain # of transactions?
length (transactions) # Number of transactions.
sizetransactions <- size (transactions) # Number of items per transaction
sizetransactionsWrepited <- size (transactionsWrepited)
LIST(transactions) # Lists the transactions by conversion (LIST must be capitalized)
labels <- itemLabels(transactions)# To see the item labels


itemFrequencyPlot(transactions)
itemFrequencyPlot(transactions, topN=20, weighted=TRUE)
transactions

prodfrequency <- c()
prodfrequency$total <- itemFrequency(transactions, type="absolute")
prodfrequency$percentage <- itemFrequency(transactions)

image(transactions)
image(sample(transactions,20))


RulesNames <- apriori(transactions, parameter = list(supp = 0.005, conf = 0.5, minlen=1))#, maxlen=10, maxtime=15))
RedundantRulesNames <- is.redundant(RulesNames) 
RulesNames <- RulesNames[!RedundantRulesNames]
RulesNames

View(RulesNames)
inspect(RulesNames)



lift()
summary(RulesNames)
inspect(head(sort(RulesNames, na.last = NA, by = "lift"),20))


ItemRules

plot()
plot(RulesNames[1:28], method="graph", control=list(type="items")) 

sel = plot(RulesNames, measure=c("support","lift"), shading="confidence", interactive =TRUE);
subrules = RulesNames[quality(RulesNames)$lift > 2.6];

plot(subrules, method="matrix", measure="lift")

subrules

subrules2 = head(sort(RulesNames, by="lift"), 20);
plot(subrules2, method="graph");
plot(subrules2, method="graph", control=list(type="items"));
plot(subrules2, method="paracoord");
plot(subrules2, method="paracoord", control=list(reorder=TRUE));
oneRule = sample(RulesNames, 1);

inspect(oneRule)



itemFrequencyPlot(Adult, support = 0.1, cex.names=0.8);
fsets = eclat(trans, parameter = list(support = 0.05), control = list(verbose=FALSE));
singleItems = fsets[size(items(fsets)) == 1];
singleSupport = quality(singleItems)$support;
names(singleSupport) = unlist(LIST(items(singleItems), decode = FALSE));
head(singleSupport, n = 5);
itemsetList = LIST(items(fsets), decode = FALSE);
allConfidence = quality(fsets)$support / sapply(itemsetList, function(x)
  max(singleSupport[as.character(x)]));
quality(fsets) = cbind(quality(fsets), allConfidence);
summary(fsets)


########## Types of Products ############
Type <- c()

Type$Laptops <- c('LG Touchscreen Laptop','Acer Aspire','HP Laptop','ASUS Chromebook','Apple Macbook Pro',
             'Apple MacBook Air','Dell Laptop','Eluktronics Pro Gaming Laptop',
             'Alienware Laptop','HP Notebook Touchscreen Laptop PC')

Type$Desktop <- c('Lenovo Desktop Computer','iMac','HP Desktop','ASUS Desktop','Dell Desktop',
             'Intel Desktop','Acer Desktop','CYBERPOWER Gamer Desktop','Dell 2 Desktop')

Type$Monitors <- c('Acer Monitor','LG Monitor','ASUS Monitor','ASUS 2 Monitor','Dell Monitor',
              'Samsung Monitor','Sceptre Monitor','ViewSonic Monitor','AOC Monitor','HP Monitor')

Type$ComputerMice <- c('Logitech Wireless Mouse','Microsoft Basic Optical Mouse',
                  'Logitech 3-button Mouse','Redragon Gaming Mouse','HP Wireless Mouse',
                  'Generic Black 3-Button','Wireless Portable Mouse','Gaming Mouse Professional', '3-Button Mouse',
                  'Slim Wireless Mouse')

Type$Keyboard <- c('HP USB Keyboard','Logitech Wireless Keyboard','Rii LED Keyboard','Logitech Keyboard',
              'Backlit LED Gaming Keyboard','Dell Wired Keyboard','Apple Wired Keyboard',
              'Apple Wireless Keyboard','Apple Magic Keyboard')

Type$MouseKeyboardCombo <- c('Logitech MK550 Wireless Wave Keyboard and Mouse Combo',
                        'Logitech Desktop MK120 Mouse and keyboard Combo',
                        'Logitech MK270 Wireless Keyboard and Mouse Combo','Dell KM117 Wireless Keyboard & Mouse',
                        'EagleTec Wireless Combo Keyboard and Mouse','Microsoft Wireless Comfort Keyboard and Mouse',
                        'Microsoft Wireless Desktop Keyboard and Mouse','Rii LED Gaming Keyboard & Mouse Combo',
                        'Logitech MK360 Wireless Keyboard and Mouse Combo')

Type$ComputerHeadphones <- c('Zombie Gaming Headset','Logitech ClearChat Headset',
                        'Panasonic On-Ear Stereo Headphones','PC Gaming Headset',
                        'Kensington Headphones','Logitech Stereo Headset','Koss Home Headphones',
                        'Microsoft Headset','Ailihen Stereo Headphones','XIBERIA Gaming Headset')

Type$ActiveHeadphones <- c('Apple Earpods','Monster Beats By Dr Dre','Otium Wireless Sports Bluetooth Headphone',
                      'Panasonic In-Ear Headphone','APIE Bluetooth Headphone',
                      'Philips Flexible Earhook Headphone')

Type$ComputerCords <- c('HDMI Cable 6ft','Ethernet Cable','Etekcity Power Extension Cord Cable','Audio Cable',
                   'VGA Monitor Cable','iPhone Charger Cable','HDMI Adapter','USB Cable',
                   'Samsung Charging Cable')

Type$Accessories <- c('Microsoft Office Home and Student 2016','Computer Game','Belkin Mouse Pad',
                 'Large Mouse Pad')

Type$Speakers <- c('Cambridge Bluetooth Speaker','JBL Splashproof Portable Bluetooth Speaker',
              'DOSS Touch Wireless Bluetooth','Logitech Multimedia Speakers','Rokono Mini Speaker',
              'Cyber Acoustics','Bose Companion Speakers','Mackie CR Speakers','Sonos')

Type$Printers <- c('Epson Printer','HP Wireless Printer','Canon Office Printer','Brother Printer',
              'DYMO Label Manker')

Type$PrinterInk <- c('Epson Black Ink','HP Black & Tri-color Ink','Canon Ink','Brother Printer Toner',
                'DYMO Labeling Tape')

Type$ComputerStands <-c('Halter Acrylic Monitor Stand','Height-Adjustable Standing Desk','Multi Media Stand',
                   'Halter Mesh Metal Monitor Stand','Full Motion Monitor Mount')

Type$ComputerTablets <- c('iPad Pro','iPad','Fire HD Tablet','Samsung Galaxy Tablet','Kindle')

Type$ExternalHardrives <- c('Slim 2TB Portable External Hard Drive','1TB Portable External Hard Drive',
                            '2TB Portable External Hard Drive','5TB Desktop Hard Drive',
                            '3TB Portable External Hard Drive')

Type$SmartHomeDevices <- c('Apple TV','Google Home','Smart Light Bulb','Fire TV Stick','Roku Express')


aggregate()

transactionsType <- transactions

namesn <-  names(Type)
for(i in 1:length(Type)){
  for(j in 1:length(Type[[i]])){
  transactionsType@itemInfo$labels <- gsub(paste(Type[[i]][j]),paste(namesn[i]),
                                           transactionsType@itemInfo$labels)
  }
}

transactionsType@itemInfo$labels

inspect (transactionsType) # You can view the transactions. Is there a way to see a certain # of transactions?
length (transactionsType) # Number of transactions.
sizetransactionsType <- size (transactionsType) # Number of items per transaction
LIST(transactionsType) # Lists the transactions by conversion (LIST must be capitalized)
itemLabels(transactionsType)# To see the item labels


itemFrequencyPlot(transactionsType)
itemFrequencyPlot(transactionsType, topN=20, weighted=TRUE)
transactionsType

prodfrequency <- c()
prodfrequency$total <- itemFrequency(transactionsType, type="absolute")
prodfrequency$percentage <- itemFrequency(transactionsType)

image(transactionsType)
image(sample(transactionsType,20))


RulesNames <- apriori(transactionsType, parameter = list(supp = 0.005, conf = 0.5, minlen=1))#, maxlen=10, maxtime=15))
RedundantRulesNames <- is.redundant(RulesNames) 
RulesNames <- RulesNames[!RedundantRulesNames]
RulesNames

View(RulesNames)
inspect(RulesNames)



lift()
summary(RulesNames)
inspect(head(sort(RulesNames, na.last = NA, by = "lift"),20))

ItemRules <- subset(RulesNames, items %in% "iMac")
ItemRules


DataFrame







for(i in 1:length(Accessories)){
  transactionsType@itemInfo$labels <- gsub(paste(Accessories[i]),"Accesories",transactionsType@itemInfo$labels)
  i
  paste(Laptops[i])
}

