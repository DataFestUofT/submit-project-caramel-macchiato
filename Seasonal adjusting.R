install.packages("seasonal")
library(seasonal)
install.packages("tidyverse")
library(tidyverse)
install.packages("forecast")
library(forecast)
install.packages("strucchange")
library(strucchange)
install.packages("tseries")
library(tseries)

library(forecast)
library(tseries)
library(strucchange)



ab_data <- read.csv("/cloud/project/alberta.csv") #Load data
bc_data <- read.csv("/cloud/project/bc.csv") 
mb_data <- read.csv("/cloud/project/manitoba.csv") 
ns_data <- read.csv("/cloud/project/nova-scotia.csv") 
on_data <- read.csv("/cloud/project/ontario.csv") 
pe_data <- read.csv("/cloud/project/pei.csv") 
qc_data <- read.csv("/cloud/project/quebec.csv") 
sk_data <- read.csv("/cloud/project/saskatchewan.csv") 
yt_data <- read.csv("/cloud/project/whitehorseyukon.csv") 
yn_data <- read.csv("/cloud/project/yellowknife.csv")



#ON
#Repeat following steps with each province
on_sg<- ts(on_data$Sugar.and.confectionery, start = c(2015,1),frequency = 12)
on_sg_saj <- seas(on_sg, x11="")
write.table( round(seasadj(on_sg_saj),1), file = "on_sg.csv", sep = ",",  # export to csv
             row.names = FALSE, col.names = FALSE)

on_bu<- ts(on_data$Butter, start = c(2015,1),frequency = 12)
on_bu_saj <- seas(on_bu, x11="")
write.table( round(seasadj(on_bu_saj),1), file = "on_bu.csv", sep = ",",  # export to csv
             row.names = FALSE, col.names = FALSE)

on_ch<- ts(on_data$Cheese, start = c(2015,1),frequency = 12)
on_sg_saj <- seas(on_sg, x11="")
write.table( round(seasadj(on_sg_saj),1), file = "on_ch.csv", sep = ",",  # export to csv
             row.names = FALSE, col.names = FALSE)

on_efo<- ts(on_data$Edible.fats.and.oils, start = c(2015,1),frequency = 12)
on_efo_saj <- seas(on_efo, x11="")
write.table( round(seasadj(on_efo_saj),1), file = "on_efo.csv", sep = ",",  # export to csv
             row.names = FALSE, col.names = FALSE)

on_egg<- ts(on_data$Eggs, start = c(2015,1),frequency = 12)
on_egg_saj <- seas(on_sg, x11="")
write.table( round(seasadj(on_egg_saj),1), file = "on_egg.csv", sep = ",",  # export to csv
             row.names = FALSE, col.names = FALSE)

on_fe6<- ts(on_data$Food.and.energy.6, start = c(2015,1),frequency = 12)
on_fe6_saj <- seas(on_fe6, x11="")
write.table( round(seasadj(on_fe6_saj),1), file = "on_fe6.csv", sep = ",",  # export to csv
             row.names = FALSE, col.names = FALSE)

on_ff<- ts(on_data$Fresh.fruit, start = c(2015,1),frequency = 12)
on_ff_saj <- seas(on_ff, x11="")
write.table( round(seasadj(on_ff_saj),1), file = "on_ff.csv", sep = ",",  # export to csv
             row.names = FALSE, col.names = FALSE)

on_ffb<- ts(on_data$Fresh.or.frozen.beef, start = c(2015,1),frequency = 12)
on_ffb_saj <- seas(on_ffb, x11="")
write.table( round(seasadj(on_ffb_saj),1), file = "on_ffb.csv", sep = ",",  # export to csv
             row.names = FALSE, col.names = FALSE)

on_ffc<- ts(on_data$Fresh.or.frozen.chicken, start = c(2015,1),frequency = 12)
on_ffc_saj <- seas(on_sg, x11="")
write.table( round(seasadj(on_ffc_saj),1), file = "on_ffc.csv", sep = ",",  # export to csv
             row.names = FALSE, col.names = FALSE)

on_ffm<- ts(on_data$Fresh.or.frozen.meat..excluding.poultry., start = c(2015,1),frequency = 12)
on_ffm_saj <- seas(on_ffm, x11="")
write.table( round(seasadj(on_ffm_saj),1), file = "on_ffm.csv", sep = ",",  # export to csv
             row.names = FALSE, col.names = FALSE)
on_ffp<- ts(on_data$Fresh.or.frozen.pork, start = c(2015,1),frequency = 12)
on_ffp_saj <- seas(on_ffp, x11="")
write.table( round(seasadj(on_ffp_saj),1), file = "on_ffp.csv", sep = ",",  # export to csv
             row.names = FALSE, col.names = FALSE)

on_ffpy<- ts(on_data$Fresh.or.frozen.poultry, start = c(2015,1),frequency = 12)
on_ffpy_saj <- seas(on_ffpy, x11="")
write.table( round(seasadj(on_ffpy_saj),1), file = "on_ffpy.csv", sep = ",",  # export to csv
             row.names = FALSE, col.names = FALSE)

on_ffv<- ts(on_data$Fresh.fruit.and.vegetables, start = c(2015,1),frequency = 12)
on_ffv_saj <- seas(on_ffv, x11="")
write.table( round(seasadj(on_ffv_saj),1), file = "on_ffv.csv", sep = ",",  # export to csv
             row.names = FALSE, col.names = FALSE)

on_fnn<- ts(on_data$Fruit..fruit.preparations.and.nuts, start = c(2015,1),frequency = 12)
on_fnn_saj <- seas(on_fnn, x11="")
write.table( round(seasadj(on_fnn_saj),1), file = "on_fnn.csv", sep = ",",  # export to csv
             row.names = FALSE, col.names = FALSE)

on_fv<- ts(on_data$Fresh.vegetables, start = c(2015,1),frequency = 12)
on_fv_saj <- seas(on_fv, x11="")
write.table( round(seasadj(on_fv_saj),1), file = "on_fv.csv", sep = ",",  # export to csv
             row.names = FALSE, col.names = FALSE)

on_meat<- ts(on_data$Meat, start = c(2015,1),frequency = 12)
on_meat_saj <- seas(on_meat, x11="")
write.table( round(seasadj(on_meat_saj),1), file = "on_meat.csv", sep = ",",  # export to csv
             row.names = FALSE, col.names = FALSE)

on_pff<- ts(on_data$Preserved.fruit.and.fruit.preparations, start = c(2015,1),frequency = 12)
on_pff_saj <- seas(on_pff, x11="")
write.table( round(seasadj(on_pff_saj),1), file = "on_pff.csv", sep = ",",  # export to csv
             row.names = FALSE, col.names = FALSE)
