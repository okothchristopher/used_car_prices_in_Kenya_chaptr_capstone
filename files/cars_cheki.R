library(tidyverse)
library(magrittr)

setwd("C:/Users/christopher/Desktop/used_car_price_estimation")
cars_data <- read_csv('cheki_cars_all.csv')


# data cleaning  ----------------------------------------------------------
###  dealing with price ,transmission ,fuel type ,where used , location of seller 
cars_data %>% 
  mutate(price_in_Ksh=parse_number(price)) %>% # all the rows with price in number all the other characters are ignored 
  filter(!is.na(price_in_Ksh)) %>% # we remove the rows whose price were not recorded since this will be our target var 
  mutate(year_of_manufacture=str_extract(make,pattern = ("\\d{4}"))) %>% # from the first column we take the first 4 numbers 
  mutate(transmission=case_when(str_detect(details, "Automatic")~"Automatic", # if there is a word automatic in details 
                                str_detect(details,"Manual")~"Manual",
                                T~"Unknown")) %>% 
  mutate(mileage_in_km=case_when(str_detect(details,"km")~parse_number(details)),# if there is the word km then there is mileage
         location=str_split(details," ",simplify = F)) %>% 
  mutate(fuel=case_when(str_detect(details,"Petrol")~"Petrol",
                        str_detect(details,"Diesel")~"Diesel",
                        str_detect(details,"Electrics")~"EV",
                        str_detect(details,"Hybrid")~"Hybrid",
                        str_detect(make,"hybrid")~"Hybrid",
                        T~"Other")) %>% 
  mutate(foreign_local=case_when(str_detect(details,"Foreign")~"Foreign",# this shows the usage of the vehicle local or imported
                                 str_detect(details,"Locally")~"Local",
                                 str_detect(details,"Brand")~"Brand",
                                 T~"Local")) %>% 
           separate(details,into=c('location','two',"three",'empty','color'),sep = " ") %>% 
  select(make,location,color,price_in_Ksh,                            # we take only the columns we need 
         year_of_manufacture,transmission,
         mileage_in_km,fuel,foreign_local) ->cars_data_intermediate   # we store this in a data frame with left assignment 


# second level data cleaning  ---------------------------------------------
#### extracting reviews, car manufacturer,color,car make 

cars_tidy_df <- cars_data_intermediate %>% 
  separate(make,into = c("one","two","three"),sep = "  ") %>%   # separating the first column into the pieces on double space
  mutate(review=str_extract_all(three,"\\d+",simplify = T)) %>% # after  we then get the reviews stars and number of reviews 
  mutate(stars=review[,1],                                      # simplify =True gives a character matrix 
         number_of_reviews=review[,2],                          # we then access the matrix in the following 
         number_of_reviews=as.numeric(number_of_reviews)) %>%   # convert this to numeric data type 
  select(-c(three,review))%>%                                   # we deselect the redundant columns 
  unite("year_make_model",one:two,sep = " ") %>%                # combine and separate with a white space 
  mutate(ymm=str_replace(year_make_model,pattern = "[:blank:]",replacement = "%%")) %>% ## detect the white space and replace with a %% 
                                                                                        #  for easy regex representation 
  separate(ymm,into = c("model","make"),sep = " ",extra = "merge") %>% # the extra information that will result will be combined to "make"
  mutate(model=str_remove_all(model,pattern = "\\d{4}%%"), ## we don not want the year in the manufacturer 
         make=str_remove(make,pattern = "Automatic|NA"),   ## any other descriptive information we strip off 
         make=str_remove(make,"NA")) %>% 
  mutate(color=parse_character(color)) %>%                 ## color cannot be a number 
  select(make=model,model=make,everything(),-year_make_model) %>% ## just to order the columns in a way we want 
  mutate(color=case_when(str_detect(color,"\\d+")~"Not_Specified",
                         str_detect(color,"Automatic")~"Not_Specified",
                         T~color)) %>%
  filter(!str_detect(make,"%%")) # we remove rows with meaningless car manufacture 

# correcting prices and any other issue 
cars_tidy_df <- cars_tidy_df %>% 
  mutate(make=case_when(make=="Land"~"Land Rover",
                        T~make)) %>%
  filter(price_in_Ksh>100000)                               ## vehicles below 100000 , here we get rid of irrelevant prices 


# dealing with color ------------------------------------------------------

car_colors <- c(colors(),"silver","pearl",'biege','other')         ## add other possible car colors that are not in the colors() list
cars_tidy_df <- cars_tidy_df %>% mutate(color=str_to_lower(color), ## change to lowerr case 
                        color=case_when(color%in%car_colors~color, ## if it is in the possible vehicle colors then it is left 
                                        T~"Not-specified"))        ## otherwise we leave it unspecified 


# dealing with the model of the car ---------------------------------------
cars_tidy_df <- cars_tidy_df  %>% 
  mutate(model=str_to_lower(model),
         make=str_to_lower(make)) 


## special cases where the car name is in two 
cars_tidy_df_special <-  cars_tidy_df %>% 
  filter(str_detect(model,"prado|mark|rover")) ## these are models with 2 names 

#### dealing with the lexuses 
cars_tidy_df_special_lexus <-  cars_tidy_df %>%
  mutate(new_model=case_when(make=='lexus'~str_remove(model,"awd|4x4"), ## these are just extra information that wont be of use 
                                    T~model)) %>% filter(make=='lexus') ## we only remain with the lexus 


cars_tidy_df_special_select <- cars_tidy_df_special %>% 
  mutate(new_model=case_when(str_detect(model,"mark")~"mark x",
                             str_detect(model,"prado")~'land cruiser prado',
                             str_detect(model,'discovery')~"discovery",
                             str_detect(model,"range")~"range rover",
                             str_detect(model,'freelander')~"freelander",
                             str_detect(model,'defender')~"defender",
                             T~"range rover")) 


# removing the selected cars 
cars_tidy_df_other <- cars_tidy_df %>% 
  filter(!str_detect(model,"prado|mark|rover")) %>% 
  mutate(fuel=case_when(str_detect(model,"hybrid")~'Hybrid',
                        T~fuel))

# remove the lexuses from this list 
cars_tidy_df_other <- cars_tidy_df_other %>% 
  filter(!make=="lexus")                      

#### dealing with the other cruisers
cars_tidy_df_other_cruiser <- cars_tidy_df_other %>% filter(str_detect(model,"cruiser")) %>% 
  mutate(new_model=case_when(str_detect(model,"fj")~'fj-cruiser',
                             T~'land cruiser'))

##### now dealing with all the other models 
cars_tidy_df <- cars_tidy_df_other %>% 
  mutate(new_model=str_extract(model,"([^\\s]+)")) %>% 
  # we extract everything before the fisrt space then we combine all the above exceptions 
  bind_rows(cars_tidy_df_other_cruiser) %>% 
  bind_rows(cars_tidy_df_special_lexus) %>% 
  bind_rows(cars_tidy_df_special_select) %>%
  select(-model,model=new_model)

# dealing with review -----------------------------------------------------

### we shall add one extremely positive review and another extremely negative review then calculate the mean review score 
cars_tidy_df <- cars_tidy_df %>% 
  mutate(stars=as.numeric(stars),
         review_score=((stars*number_of_reviews)+6)/(number_of_reviews+2)) %>% 
  select(make,model,everything(),-c(stars,number_of_reviews))

### then for the other cases we shall replace with the minimum review score 
min_review_score <- min(cars_tidy_df$review_score,na.rm = T)
cars_tidy_df$review_score <- cars_tidy_df$review_score %>% replace_na(min_review_score)


# any other issues in the data  -------------------------------------------
## vehicles with 1km as mileage seem to have a faulty recording , we shall correct this by assuming they are NA in mileage 
cars_tidy_df %<>% 
  mutate(mileage_in_km=na_if(mileage_in_km,1))

# saving the resultant data frame  ----------------------------------------
## save the clean data frame 
cars_tidy_df %>% write_csv("cheki_cars_cleaned_new.csv",append = FALSE)





# 
# # joining to other data frames --------------------------------------------
# library(fuzzyjoin)
# 
# data_2010_epa <- dta_2010 %>% janitor::clean_names()
# 
# tidy_car_epa <- data_2010_epa %>% select(vehicle_manufacturer_name,represented_test_veh_model,vehicle_type,
#                                          rated_horsepower,drive_system_code,equivalent_test_weight_lbs,
#                                          rnd_adj_fe) %>% distinct(represented_test_veh_model,.keep_all = T) %>%
#   mutate(model=str_to_lower(represented_test_veh_model))
# 
# ###############
# cars_tidy_df %>%left_join(tidy_car_epa) %>% view()
# 
# 
# cars_tidy_df %>% stringdist_left_join(tidy_car_epa) %>% view()
# 
# 
# ######## another joining to get extra information
# 
# cars_belarus <- read_csv('cars.csv')
# 
# cars_belarus %>% select(make=1,model=2,year_of_manufacture=year_produced,engine_capacity,body_type) %>%
#   filter(make=='Toyota') %>% view()
# 
# #### account for diffence in naming schemes
# cars_belarus <- cars_belarus %>% select(make=1,model=2,year_of_manufacture=year_produced,engine_capacity,body_type) %>%
#   mutate(model=case_when(str_detect(model,'Highlander')~'Kluger',
#                          str_detect(model,'Verso')~'Spacio',
#                          str_detect(model,'Yaris')~"Vitz",
#                          T~model)) %>%
#   mutate(model=str_to_lower(model),
#          make=str_to_lower(make),
#          year_of_manufacture=as.character(year_of_manufacture))
# 
# 
# 
# cars_tidy_df %>% left_join(cars_belarus,by=c("model","year_of_manufacture")) %>% view()
