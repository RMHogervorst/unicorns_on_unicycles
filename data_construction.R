## Construction of dataset
## 
## This is the code used to create the unicorn dataset, 
## when you present this dataset as a genuine old timey dataset, it
## does not really work that well, with modern R code that produces the set.
## 
## So maybe it was handed down through a time vortex by Antonie van Leeuwenhoek the time-traveler?
## 
## This code is also cc-by-4.0. use remix and whatever you like.

library(tidyverse)
# ISOcodes package, contains groupings per country, but this is coded in a
# string in a row.  
country_vec <- ISOcodes::UN_M.49_Regions %>% 
    filter(Name %in%  c( "Western Europe") )%>% 
    mutate(countries = str_split(Children, ", ")) %>% 
    unnest() %>% 
    select(countries) %>% 
    left_join(ISOcodes::UN_M.49_Countries, 
              by = c("countries"="Code")) %>% 
    pull(Name) %>% str_trim()
# create country names and years
baseset <- crossing( 
    countryname = country_vec,
    year = 1670:1680
    ) %>% 
    filter(!countryname %in% c("Liechtenstein","Monaco", "Luxembourg", "Belgium")) %>% 
    mutate(
        exclude = case_when(
        countryname == "Austria" & year == 1679 ~ TRUE,
        TRUE ~ FALSE
                    ),
        random = rbernoulli(nrow(.),p = .1)
        ) 
baseset %>% filter(year < 1675) %>% nrow()

baseset$exclude[baseset$year < 1674] %>% length() #20
baseset$exclude[baseset$year < 1674] <- rbernoulli(20, p = .2)

# some missing data from Spain in 76-85 , austria 1679 due to plagues
# Some more missing from early years
# random missing because I feel like it
# delete some countries because they do not exist yet. Liecht, 
# Monaco and Luxembourg were part of france.
# Belgium did not really exist, sort of part of Netherlands (under Spanish rule?)
# Austria was not yet austria-hungary
# some size considerations

new_set <- baseset %>% filter(!exclude) %>% filter(!random) %>% 
    select(countryname,year)

## onward to create some data.
## ranges of population
## populations would be correlated with previous years. 
## could use arima. 
## Could draw a starting value from rnbinom, and add gausian (Normal) noise and round t
##  nearest integer, ugly, but it would make more sense I guess. 
create_pop <- function(n_values){
    rep(rnbinom(1,20,.23), n_values) + rnorm(n_values, 0, 5) %>% 
        round(0)
}

### This took a lot of effort, but the trick is to make a function that 
### works on a dataframe and returns a dataframe, if then use group_by and
### do, it works for everything. The real trick is, as is often the case,
### make it work on a subset, than apply on the total.
### 
make_popcolumn <- . %>% 
    mutate(pop = create_pop(nrow(.)))

new_set %>% 
    group_by(countryname) %>% 
    do(make_popcolumn(.)) %>% 
    ggplot() +
    geom_line(aes(year, pop, group = countryname))+
    geom_point(aes(year, pop, color = countryname), alpha = .7)

final_set <- new_set %>% 
    group_by(countryname) %>% 
    do(make_popcolumn(.))

final_set %>%     
    ggplot() +
    geom_line(aes(year, pop, group = countryname))+
    geom_point(aes(year, pop, color = countryname), alpha = .7)


### creating unicycles set
### unicycles numbers and total price should be related. 
### perhaps use some of the population as basis ?
### .70 percent can buy cycle but with wide margins because of outside
### influences? Average price, in dutch guilders. 
### A unicycle in 2016 is approx 90 euros.  rembrandt got approx 4000 scudi, 
### 3200 gulden (guilders). .. I will use 90 guilders a piece, it being artisinal
### and all. 

unicycles_price <- function(n_values){
    round(rep(runif(1,80,90), n_values) + rt(n_values, 20, 5),1)
}

set2 <- final_set %>% 
    mutate(bikes = round(.7*pop + rt(1,df = 6),digits = 0)) %>% 
    mutate(total_turnover = round(bikes * unicycles_price(1),2)) %>% 
    mutate(name_of_country = str_to_upper(countryname) ) %>% 
    ungroup() %>% 
    select(-pop, -countryname) 

set2 %>% write_csv("sales.csv")
final_set %>% write_csv("observations.csv")

## could make seperate currency translation. ~ 1 guilder is this in marks, table.