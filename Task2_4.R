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

ItemRules <- subset(RulesNames, items %in% "iMac")
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

Type$Laptops <- c('LG_Touchscreen_Laptop','Acer_Aspire','HP_Laptop','ASUS_Chromebook','Apple_Macbook_Pro',
             'Apple_MacBook_Air','Dell_Laptop','Eluktronics_Pro_Gaming_Laptop',
             'Alienware_Laptop','HP_Notebook_Touchscreen_Laptop_PC')

Type$Desktop <- c('Lenovo_Desktop_Computer','iMac','HP_Desktop','ASUS_Desktop','Dell Desktop',
             'Intel_Desktop','Acer_Desktop','CYBERPOWER_Gamer_Desktop','Dell_2_Desktop')

Type$Monitors <- c('Acer_Monitor','LG_Monitor','ASUS_Monitor','ASUS_2_Monitor','Dell_Monitor',
              'Samsung_Monitor','Sceptre_Monitor','ViewSonic_Monitor','AOC_Monitor','HP_Monitor')

Type$ComputerMice <- c('3-Button_Mouse','Logitech_Wireless_Mouse','Microsoft_Basic_Optical_Mouse',
                  'Logitech_3-button_Mouse','Redragon_Gaming_Mouse','HP_Wireless_Mouse',
                  'Generic_Black_3-Button','Wireless_Portable_Mouse','Gaming_Mouse_Professional',
                  'Slim_Wireless_Mouse')

Type$Keyboard <- c('HP_USB_Keyboard','Logitech_Wireless_Keyboard','Rii_LED_Keyboard','Logitech_Keyboard',
              'Backlit_LED_Gaming_Keyboard','Dell_Wired_Keyboard','Apple_Wired_Keyboard',
              'Apple_Wireless_Keyboard','Apple_Magic_Keyboard')

Type$MouseKeyboardCombo <- c('Logitech_MK550_Wireless_Wave_Keyboard_and_Mouse_Combo',
                        'Logitech_Desktop_MK120_Mouse_and_keyboard_Combo',
                        'Logitech_MK270_Wireless_Keyboard_and_Mouse_Combo','Dell_KM117_Wireless_Keyboard_&_Mouse',
                        'EagleTec_Wireless_Combo_Keyboard_and_Mouse','Microsoft_Wireless_Comfort_Keyboard_and_Mouse',
                        'Microsoft_Wireless_Desktop_Keyboard_and_Mouse','Rii_LED_Gaming_Keyboard_&_Mouse_Combo',
                        'Logitech_MK360_Wireless_Keyboard_and_Mouse_Combo')

Type$ComputerHeadphones <- c('Zombie_Gaming_Headset','Logitech_ClearChat_Headset',
                        'Panasonic_On-Ear_Stereo_Headphones','PC_Gaming_Headset',
                        'Kensington_Headphones','Logitech_Stereo_Headset','Koss_Home_Headphones',
                        'Microsoft_Headset','Ailihen_Stereo_Headphones','XIBERIA_Gaming_Headset')

Type$ActiveHeadphones <- c('Apple_Earpods','Monster_Beats_By_Dr_Dre','Otium_Wireless_Sports_Bluetooth_Headphone',
                      'Panasonic_In-Ear_Headphone','APIE_Bluetooth_Headphone',
                      'Philips_Flexible_Earhook_Headphone')

Type$ComputerCords <- c('HDMI_Cable_6ft','Ethernet_Cable','Etekcity_Power_Extension_Cord_Cable','Audio_Cable',
                   'VGA_Monitor_Cable','iPhone_Charger_Cable','HDMI_Adapter','USB_Cable',
                   'Samsung_Charging Cable')

Type$Accessories <- c('Microsoft_Office_Home_and_Student_2016','Computer_Game','Belkin_Mouse_Pad',
                 'Large_Mouse_Pad')

Type$Speakers <- c('Cambridge_Bluetooth_Speaker','JBL_Splashproof_Portable_Bluetooth_Speaker',
              'DOSS_Touch_Wireless_Bluetooth','Logitech_Multimedia_Speakers','Rokono_Mini_Speaker',
              'Cyber_Acoustics','Bose_Companion_Speakers','Mackie_CR_Speakers','Sonos')

Type$Printers <- c('Epson_Printer','HP_Wireless_Printer','Canon_Office_Printer','Brother_Printer',
              'DYMO_Label_Manker')

Type$PrinterInk <- c('Epson_Black_Ink','HP_Black_&_Tri-color_Ink','Canon_Ink','Brother_Printer_Toner',
                'DYMO_Labeling_Tape')

Type$ComputerStands <-c('Halter_Acrylic_Monitor_Stand','Height-Adjustable_Standing_Desk','Multi_Media_Stand',
                   'Halter_Mesh_Metal_Monitor_Stand','Full_Motion_Monitor_Mount')

Type$ComputerTablets <- c('iPad_Pro','iPad','Fire_HD_Tablet','Samsung_Galaxy_Tablet','Kindle')

Type$ExternalHardrives <- c('Slim_2TB_Portable_External_Hard_Drive','1TB_Portable_External_Hard_Drive',
                            '2TB_Portable_External_Hard_Drive','5TB_Desktop_Hard_Drive',
                            '3TB_Portable_External_Hard_Drive')

Type$SmartHomeDevices <- c('Apple_TV','Google_Home','Smart_Light_Bulb','Fire_TV_Stick','Roku_Express')


aggregate()

transactionsType <- transactions

namesn <-  names(Type)
for(i in 1:length(Type)){
  for(j in 1:length(Type[[i]])){
  transactionsType@itemInfo$labels <- gsub(paste(Type[[i]][j]),paste(namesn[i]),
                                           transactionsType@itemInfo$labels, )
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










for(i in 1:length(Accessories)){
  transactionsType@itemInfo$labels <- gsub(paste(Accessories[i]),"Accesories",transactionsType@itemInfo$labels)
  i
  paste(Laptops[i])
}

