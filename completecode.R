

#to combine all city data into one file

csv_files <- dir(pattern='*.csv$', recursive = T) # read all the csv files from selected folder

library(dplyr) # use dplyr library

library(dplyr)
require(data.table)

dataset = do.call(rbind, lapply(csv_files, fread)) #rread all csv files & combine it by doing rowbind operation
rm(csv_files)

dataset <- as.data.frame(unclass(dataset)) # convert this into data frame
View(dataset)

dim(dataset) #check dimwnsions
is.na(dataset) 
sum(is.na(dataset)) #count missing values
write.csv(dataset, file = "combinecities.csv") # save the file



__________________________________________________________________________________________________________________________
# make new column "poi_name_factype_city_freqency" which will count according to poi name.
  
ds=read.csv("combinecities.csv") #read the file
View(ds) #check its table
dim(ds)  #check dim of data frameno.of rows & cols of data frame
unique(ds$fac_type) #check for  unique fac_type code available in dataset

df1= ds[
  with(ds,order(fac_type)),  #sort column by fac_type
]


#now count the frequency of each poi name acc to its fac_type in each city & make new column "Totalfactypecount"

df=df1 %>% 
  group_by(fac_type,,is_tc,t_name,poi_name,) %>% 
  count(fac_type,name="Totalfactypecount") 

View(df)


#change the columnnames for better understanding
colnames(df)=df("fac_type","is_tc","city","poi_name","poi_name_factype_city_freqency")

View(df)

df2=df %>% 
  select(4,1,5,3) #select required columns

dim(df2)

df3=write.csv(df2,"CITIES_FACTLEVL_POI.csv",row.names =F)# save the csv file
_______________________________________________________________________________________________________________________________________
ds=read.csv("combinecities.csv") #read the file

View(ds) #check its table

dim(ds)  #check dim of data frameno.of rows & cols of data frame

unique(ds$fac_type) #check for  unique fac_type code available in dataset
______________________________________________________________________________________________________________________________
#now make new column "factypefrequency" which will count the frequency of  each factype in each city

library(dplyr)

ds1=ds %>% 
  group_by(fac_type,t_name) %>% 
  count(fac_type,name="factypefrequency")  #frequency of  each factype in each city

View(ds1)

z=write.csv(ds1,"citywiese_factypefrequency.csv",row.names =F) # save the file 

____________________________________________________________________________________________________________________________________
  
  #now  add new column "totalfacttypefreq" which will count the frequncy of each factype all over in city
  
  
ds=read.csv("combinecities.csv") # again read the file
View(ds)     #check its table

ds1=ds %>%    #use group_by with chaining method
  group_by(fac_type) %>% 
  summarise(totalfacttypefreq=table(fac_type)) #overall frequency of each facttype in all city

z1=write.csv(ds1,"totalfactype.csv",,row.names =F) #save the file

_____________________________________________________________________________________________________________________________________
#now join the tables which we have craeted on a common column name
  
  View(y)

v=read.csv("citywiese_factypefrequency.csv")
w=read.csv("totalfactype.csv")

View(v)
View(w)


df1=left_join(v,w)#left join
View(df1)

colnames(df1)=c("fac_type","city_name","factype_city_freq","total_fact_type") # rename the columnames for better understanding

##now  make new column to count the total "poi" in each city
ds=read.csv("combinecities.csv") #again read this file  

df2=ds %>% 
  group_by(t_name) %>%   
  count(nrow(poi_name))  #no.of poi in each city


#rename the column names 
c=rename(df2,"city_name"="t_name")
c=rename(df2,"citywise_total_poi"="n")


#Now join df1 & df2 on same column name using left join 

df=left_join(df1,df2)

#now load below file & join this with df dataset using left join

df3=read.csv("CITIES_FACTLEVL_POI.csv",row.names =F)

df=left_join(df,df3) --#left join

#rename column names
  
df=rename(df,"city_name"="city")
df=rename(df,"poiname_factype_city_freq"="citywise_factype_frequency")

_________________________________________________________________________________________________________________________________
# now add new column "Total_POi_in_all_city" which will count no.of poi all over city.


df3=df %>% 
  mutate(Total_POi_in_all_city=sum(poiname_factype_city_freq)) #total_Poiin_AllCity

#again join df &df3 on same column name using left join   

df4=left_join(df,df3)

z=write.csv(df4,"POI_OF_ALL_CITIES.csv",row.names = FALSE) # now save the file 


_______________________________________________________________________________________________________________________________
# PREPARED TOP 5 & 10 LIST OF EACH FACTYPE IN surat CITY 

##top5 of  surat city


x=read.csv("POI_OF_ALL_CITIES.csv",stringsAsFactors = FALSE)
head(x)

df1=x[x$city_name=="Surat",]  # subset data by city_name=="Surat"

head(df1)

#top_n(5,poiname_factype_city_freq)

#now  fetch top 5 brands  for surat city & each fact type using group_by chaining method & then use  slice_max fun
#to fech the top 5 rows for surat city & each fact type & add new columns to the existing data .


df2=df1 %>%
  group_by(fac_type) %>%
  slice_max(order_by=poiname_factype_city_freq,n=10,with_ties = FALSE) 
summarise(sum_of_top5_poiofsameFactypeinaCity=sum(poiname_factype_city_freq),#sum of top 5 poiname_factype_city_freq
          TotalPoiOfSameFactypeInCity=mean(factype_city_freq),              # average of  factype_city_freq
          TotalPoiOfSameFactypeInAllCity=mean(totalfacttypefreq),           #average of totalfacttypefreq
          TotalPoiInSameCity=mean(citywise_total_poi),                      #average of citywise_total_poi
          TotalPoiInallCity1=mean(Total_POi_in_all_city))                   # Total_POi_in_all_city






df4= df2[
  with(df2,order(-sum_of_top5_poiofsameFactypeinaCity)),  #sort column by top5_poiofsameFactypeinaCity in desc
]



View(df4)

df3=write.csv(df4,"Top5_Poi_of_surat.csv",row.names = F)



View(df4)

#now to find percentage of top 5 brand covers in surat city divide this column by other variables & add new columns for the same.

df4$Top5BrandPercentageofSameFactypeInSameCity=(df4$sum_of_top5_poiofsameFactypeinaCity/df4$TotalPoiOfSameFactypeInCity)*100

df4$Top5BrandPercentageofSameFactypeInAllCity=(df4$sum_of_top5_poiofsameFactypeinaCity/df4$TotalPoiOfSameFactypeInAllCity)*100

df4$Top5BrandPercentageInsameCity=(df4$sum_of_top5_poiofsameFactypeinaCity/df4$TotalPoiInSameCity)*100

df4$Top5BrandPercentageInsallCity=(df4$sum_of_top5_poiofsameFactypeinaCity/df4$TotalPoiInallCity1)*100

df4=write.csv(df4,"Top5_Poi_of_surat.csv",row.names = F)


##top10______________________________________________________________________________________________________________________________________
##top10 of surat city 

x=read.csv("POI_OF_ALL_CITIES.csv",stringsAsFactors = FALSE)
head(x)

df1=x[x$city_name=="Surat",]  # subset data by city_name=="Surat"


#now  fetch top 10 brands  for surat city & each fact type using group_by chaining method & then use  slice_max fun
#to fech the top 10 rows for surat city & each fact type & add new columns to the existing data 

df2=df1 %>%
  group_by(fac_type) %>%
  slice_max(order_by=poiname_factype_city_freq,n=10,with_ties = FALSE) 
summarise(sum_of_top10_poiofsameFactypeinaCity=sum(poiname_factype_city_freq),#sum of top 10 poiname_factype_city_freq
          TotalPoiOfSameFactypeInCity=mean(factype_city_freq),                #average of  factype_city_freq
          TotalPoiOfSameFactypeInAllCity=mean(totalfacttypefreq),              #average of totalfacttypefreq
          TotalPoiInSameCity=mean(citywise_total_poi),                         #average of citywise_total_poi
          TotalPoiInallCity1=mean(Total_POi_in_all_city))                       # Total_POi_in_all_city


View(df2)

# find percentage of top 10 brands covers in surat city divide this column by other variables & add new columns for the same.

df2$Top10BrandPercentageofSameFactypeInSameCity=(df2$sum_of_top10_poiofsameFactypeinaCity/df2$TotalPoiOfSameFactypeInCity)*100

df2$Top10BrandPercentageofSameFactypeInAllCity=(df2$sum_of_top10_poiofsameFactypeinaCity/df2$TotalPoiOfSameFactypeInAllCity)*100

df2$Top10BrandPercentageInsameCity=(df2$sum_of_top10_poiofsameFactypeinaCity/df2$TotalPoiInSameCity)*100

df2$Top10BrandPercentageInsallCity=(df2$sum_of_top10_poiofsameFactypeinaCity/df2$TotalPoiInallCity1)*100

df5=write.csv(df2,"Top10_Poi_of_surat.csv",row.names = F)

df3=read.csv("Top5_Poi_of_surat.csv")

View(df3)

#for better compariosn join this two tables using left join

df6=left_join(df3,df2) 

View(df6)

#rename column names

df6=rename(df6,"sum_of_top5_BrandofsameFactypeinaCity"="sum_of_top5_poiofsameFactypeinaCity")
df6=rename(df6,"sum_of_top10_BrandofsameFactypeinaCity"="sum_of_top10_poiofsameFactypeinaCity")


df6=df6 %>% 
  select(1:2,11,3:10,12:14,15) # arrange the columns order

df7=write.csv(df6,"Top5&10_Poi_of_surat.csv",row.names = F) #save the file

_________________________________________________________________________________________________________________________________
# PREPARED TOP 5 & 10 LIST OF EACH FACTYPE IN EACH CITY  
##top5 of  all cities
  
  
df=read.csv("POI_OF_ALL_CITIES.csv",stringsAsFactors = FALSE) #read csv file
head(df)#check top 6 rows

#now  fetch top 5 brands  for each city & each fact type using group_by chaining method & then use  slice_max fun
#to fech the top 5 rows for each city & each fact type & add new columns to the existing data .




df2=df %>%
  group_by(fac_type,city_name) %>%
  slice_max(order_by=poiname_factype_city_freq,n=5,with_ties = FALSE) %>%
  summarise(sum_of_top5_brandofsameFactypeinaCity=sum(poiname_factype_city_freq), #sum of top 5 poiname_factype_city_freq
            TotalPoiOfSameFactypeInCity=mean(factype_city_freq),                 #average of factype_city_freq
            TotalPoiOfSameFactypeInAllCity=mean(totalfacttypefreq),              # average of totalfacttypefreq
            TotalPoiInSameCity=mean(citywise_total_poi),                         #average of citywise_total_poi
            TotalPoiInallCity1=mean(Total_POi_in_all_city))                      # Total_POi_in_all_city

View(df2)  #check data frame table 

#now to find percentage of top 5 brand covers in each city divide this column by other variables & add new columns for the same.

df2$Top5BrandPercentageofSameFactypeInSameCity=(df2$sum_of_top5_brandofsameFactypeinaCity/df2$TotalPoiOfSameFactypeInCity)*100

df2$Top5BrandPercentageofSameFactypeInAllCity=(df2$sum_of_top5_brandofsameFactypeinaCity/df2$TotalPoiOfSameFactypeInAllCity)*100

df2$Top5BrandPercentageInsameCity=(df2$sum_of_top5_brandofsameFactypeinaCity/df2$TotalPoiInSameCity)*100

df2$Top5BrandPercentageInsallCity=(df2$sum_of_top5_brandofsameFactypeinaCity/df2$TotalPoiInallCity1)*100

df3=write.csv(df2,"Top5_Poi_of_all_cities.csv",row.names=F) # save the file

________________________________________________________________________________________________________________________________________________
  
  
  ##top10 of all cities
  
  
df=read.csv("POI_OF_ALL_CITIES.csv",stringsAsFactors = FALSE)
head(df)

#now  fetch top 10 brands  for each city & each fact type using group_by chaining method & then use  slice_max fun
#to fech the top 10 rows for each city & each fact type & add new columns to the existing data .


df4=df %>%
  group_by(fac_type,city_name) %>%
  slice_max(order_by=poiname_factype_city_freq,n=10,with_ties = FALSE) %>%
  summarise(sum_of_top10_brandofsameFactypeinaCity=sum(poiname_factype_city_freq),#sum of top 10 poiname_factype_city_freq
            TotalPoiOfSameFactypeInCity=mean(factype_city_freq),                   #average of factype_city_freq
            TotalPoiOfSameFactypeInAllCity=mean(totalfacttypefreq),                # average of totalfacttypefreq
            TotalPoiInSameCity=mean(citywise_total_poi),                           #average of citywise_total_poi
            TotalPoiInallCity1=mean(Total_POi_in_all_city))                         # Total_POi_in_all_city


View(df4)
# find percentage of top 10 brands covers in each city divide this column by other variables & add new columns for the same.

df4$Top10BrandPercentageofSameFactypeInSameCity=(df4$sum_of_top10_brandofsameFactypeinaCity/df4$TotalPoiOfSameFactypeInCity)*100

df4$Top10BrandPercentageofSameFactypeInAllCity=(df4$sum_of_top10_brandofsameFactypeinaCity/df4$TotalPoiOfSameFactypeInAllCity)*100

df4$Top10BrandPercentageInsameCity=(df4$sum_of_top10_brandofsameFactypeinaCity/df4$TotalPoiInSameCity)*100

df4$Top10BrandPercentageInsallCity=(df4$sum_of_top10_brandofsameFactypeinaCity/df4$TotalPoiInallCity1)*100


df5=write.csv(df4,"Top10_Poi_of_all_cities.csv",row.names=F) # save the file

------------------------______________________________________________________________________________________________________________________________________
  
  #for better compariosn join this two tables .
  
  df6=left_join(df2,df4)# join dataframes on same column name
View(df6)
df6=df6 %>% 
  select(1:3,12,4:11,13:16)  # rearrange the columns
df7=write.csv(df6,"Top5&10_Poi_of_allCities.csv",row.names = F)#save the file

____________________________________________________________________________________________________________________________________________________________
  #code for feching top 5  brand list for each city & each fact type.
  
df=read.csv("POI_OF_ALL_CITIES.csv",stringsAsFactors = FALSE)  #read the file

head(df) # check its top6 rows

library(dplyr) #to perform group_by use dplyr library.

#top_n(5,poiname_factype_city_freq)

##top5 brand names for each city 

df=df[,c(1,2,3,4)] #data subseting  select all rows & 4 columns  which we needed


#now  fetch top 5 brand list for each city & each fact type using group_by chaining method & then use  slice_max fun
  #to fech the top 5 rows for each city & each fact type.
  
df2=df %>%group_by(city_name ,fac_type) %>%
  slice_max(order_by=poiname_factype_city_freq,n=5,with_ties = FALSE) 

View(df2)

df2=rename(df2,"Top5Brands"="poi_name")#rename to column name for better understanding

df3=write.csv(df2,"TOP5BRAND_LIST_OF_ALL-CITIES.CSV",row.names = F) # save the file in .csv format by giving apropriate name. 

____________________________________________________________________________________________________________________________________________________







