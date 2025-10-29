library(tidyverse)
#import the first dataset which contains most of our x variables
library(readxl)
library(stargazer)
qog_oecd_ts_jan24 <- read_excel("C:/Users/liloy/Downloads/qog_oecd_ts_jan24.xlsx")
View(qog_oecd_ts_jan24)
df <- qog_oecd_ts_jan24

#clean df to keep the years and countries that we will use and rename countries with the same name as in the database than contains our y to later merge them easily
df <- filter(df,cname !="Australia")
df <- filter(df,cname !="Canada");
df <- filter(df,cname !="Chile")
df <- filter(df,cname !="Colombia")
df <- filter(df,cname !="Korea")
df <- filter(df,cname !="Costa Rica")
df <- filter(df,cname !="Iceland")
df <- filter(df,cname !="Japan")
df <- filter(df,cname !="Israël")
df <- filter(df,cname !="Mexico")
df <- filter(df,cname !="")
df <- filter(df,cname !="United States of America (the)")

df <- filter(df,cname !="Korea (the Republic of)")
df <- filter(df,cname !="New Zealand")            
df <- filter(df,cname !="Turkey")   
df <- filter(df,cname !="Israel")
df <- filter(df,cname !="Switzerland")
df <- filter(df,cname !="Latvia")
df <- filter(df, year <= 2017)
df <- filter(df, year >= 2004)
df$ccode <- NULL
colnames(df)[1] <-"country"
df[197:210,1] <-"Netherlands"
df[309:322,1] <-"United Kingdom"
df[29:42,1] <-"Czech Republic"

#then we create df1 on which we import only  the variables that we want to use
df1 <- df[, c("country","year","oecd_govdefct_t3","oecd_socexpnd_t1a","vdem_libdem","wdi_expedu","wdi_pop1564","oecd_sizegdp_t1")]

#import the World inequality database that contains gini and the wealth owned by the 10% richest of each country (our explained variables)
WID_Data_WID_Data <- read_csv("C:/Users/liloy/Downloads/WID-Data - WID-Data.csv")
View(WID_Data_WID_Data)

df2 <- WID_Data_WID_Data

#remove the unused years
df2 <- filter(df2, Year<= 2017)
df2 <- filter(df2, Year >= 2004)


#We forgot to add Portugal on the WID Database so we add it now
library(readr)
portugal <- read_delim("C:/Users/liloy/Downloads/portugal.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(portugal)

df2 <- union(df2, portugal)

#rename the columns before merging
colnames(df1)[1] <-"country"
colnames(df2)[1] <-"country"
colnames(df2)[2] <-"year"

#now we can merge by columns 
df4 <- left_join(df1, df2, by=c("country", "year"))

#add 2 more variables
df4$dr_eg <- df$dr_eg
df4$wdi_lfpedua <- df$wdi_lfpedua

#import a new database that contains tax progressivity (a new explanatory variable)

library(readr)
tax_67_ <- read_delim("C:/Users/liloy/Downloads/tax-67_.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(tax_67_)

tax_167_ <- read.csv2("C:/Users/liloy/Downloads/tax-167_.csv")
tax_67_ <- read.csv2("C:/Users/liloy/Downloads/tax-67_.csv")

#rename years
c <- 2004:2017
colnames(tax_167_)[2:15] <- c
colnames(tax_67_)[2:15] <- c
#modify the data to have 1 lign by year
taxx_167_ <- gather(tax_167_, year, tp167, 2:15)
taxx_67_ <- gather(tax_67_, year, tp67, 2:15)

taxx_67_$tp167 <- taxx_167_$tp167

#creation of 2 variables tp which is the difference between tp167 and tp67 but in the end we will use tpprop, the closer this variable is to 1 the less progressiveness there is in income taxation
taxx_67_$tp <- taxx_67_$tp167-taxx_67_$tp67
taxx_67_$tpprop <- taxx_67_$tp67/taxx_67_$tp167

#prepare to import our tpprop variable into df4, using df44 as an intermediate database in case we've made a mistake
colnames(taxx_67_)[1] <-"country"
taxx_67_$year <- as.numeric(taxx_67_$year)
df44 <- left_join(df4, taxx_67_, by = c("country", "year"))

df44$tp67 <- NULL
df44$tp167 <- NULL
df44$tp <- NULL

df5 <- df44

#remove Lithuania which is on OECD since 2016 only, we forgot to check that before
df4 <- filter(df4,country !="Lithuania")
df5 <- filter(df5,country !="Lithuania")
df44 <- filter(df44,country !="Lithuania")

#checking for potential missing values
na_count <- sum(is.na(df4$oecd_govdefct_t3))
print(na_count)
na_count <- sum(is.na(df4$oecd_socexpnd_t1a))
print(na_count)
na_count <- sum(is.na(df4$vdem_libdem))
print(na_count)
na_count <- sum(is.na(df4$wdi_expedu))
print(na_count)
na_count <- sum(is.na(df4$Gini))
print(na_count)
na_count <- sum(is.na(df4$dr_eg))
print(na_count)
na_count <- sum(is.na(df$wdi_lfpedua))
print(na_count)
na_count <- sum(is.na(df5$tpprop))


#export the final database, the fusion of our 3 sources, which contains all our variables
write.csv(df5,"C:/Users/liloy/Downloads/df5.csv")

#first tries of graphs
ggplot(df5, aes(x=tpprop, y=Gini)) +
  geom_point() +
stat_smooth(method = "lm")

ggplot(df5, aes(x=oecd_socexpnd_t1a, y=Gini)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(df5, aes(x=tpprop, y=`Top-10-percent`)) +
  geom_point() +
  stat_smooth(method = "lm")

#running the first fixed effects model
library(plm)
fixed <- plm(Gini ~ oecd_socexpnd_t1a, data=df5, index =c("country","year"), model="within")
summary(fixed)

summary(df5)

# Running Hausman test to check whether it is better to use random effects or fixed ones:
colnames(df5)[10] <- "top10perc"
library(plm)
#Fixed models for Hausmann Test:
fixed <- plm(Gini ~ tpprop + oecd_socexpnd_t1a + oecd_sizegdp_t1 + wdi_pop1564 + oecd_sizegdp_t1^2 + dr_eg, data=df5, index=c("country", "year"), model="within")  #fixed model
fixed2 <- plm(top10perc ~ tpprop + oecd_socexpnd_t1a + oecd_sizegdp_t1 + wdi_pop1564 + oecd_sizegdp_t1^2 + dr_eg, data=df5, index=c("country", "year"), model="within")  #fixed model


random <- plm(Gini ~  tpprop + oecd_socexpnd_t1a + oecd_sizegdp_t1 + wdi_pop1564 + oecd_sizegdp_t1^2 + dr_eg, data=df5, index=c("country", "year"), model="random")  #random model
summary(random)
random2 <- plm(top10perc ~  tpprop + oecd_socexpnd_t1a + oecd_sizegdp_t1 + wdi_pop1564 + oecd_sizegdp_t1^2 + dr_eg, data=df5, index=c("country", "year"), model="random")  #random model
summary(random2)


phtest(fixed,random) #Hausman test
phtest(fixed2,random2)


# Calculation of the average Gini per year for a graph of its evolution
gini_par_annee <- df5 %>%
  group_by(year) %>%
  summarise(Gini_moyen = mean(Gini, na.rm = TRUE))

top10_par_annee <- df5 %>%
  group_by(year) %>%
  summarise(top10_moyen = mean(top10perc, na.rm = TRUE))


evolution_gini <- ggplot(gini_par_annee, aes(x=year, y=Gini_moyen)) +
  geom_point()

ggsave("gini_par_annee.pdf", plot = evolution_gini, width = 8, height = 5)
ggsave("gini_par_annee.jpeg", plot = evolution_gini, width = 8, height = 5, dpi = 300)


#other graphs
ggplot(df5, aes(x=oecd_socexpnd_t1a, y=Gini)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(df5, aes(x=tpprop, y=`top10perc`)) +
  geom_point() +
  stat_smooth(method = "lm")


# Remove country and year from the correlation matrix
xy <- df5 %>% select(-country, -year)  

# Compute the correlation matrix
cor_matrix <- cor(xy, use = "complete.obs") 
print(cor_matrix)

#add squared gdp per capita (won't be used because too much correlation with gdp per capita of course)
df5$squared_gdp_percapita <- df5$oecd_sizegdp_t1^2

# Convert into panel data
df5_panel <- pdata.frame(df5, index = c("country", "year"))

# Running random effects models
model_re <- plm(Gini ~ oecd_socexpnd_t1a + tpprop + wdi_expedu + wdi_pop1564 + oecd_sizegdp_t1 + vdem_libdem + wdi_lfpedua + dr_eg, data = df5_panel, model = "random")
summary(model_re)

model_reTop10 <- plm(top10perc ~ oecd_socexpnd_t1a + tpprop + wdi_expedu + wdi_pop1564 + oecd_sizegdp_t1 + vdem_libdem + wdi_lfpedua + dr_eg, data = df5_panel, model = "random")
summary(model_reTop10)

rm(list=ls())



