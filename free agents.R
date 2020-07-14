library(tidyverse)
library(rvest)
library(janitor)

#past free agents from 2016-2019
get_free_agents<-function(year=2016)
{
  url=paste0("https://www.basketball-reference.com/friv/free_agents.cgi?year=",year)
  a<-url %>% read_html() %>% html_nodes("table") %>% .[[1]] %>% html_table() %>% 
    mutate(WS=as.numeric(WS))
  #get rid of separator rows and players who didn't play in NBA previous season (signed from overseas, so WS=NA)
  #change Nene's name to match stats
  a<-a %>% filter(Player != "Player",!is.na(WS)) %>% 
    mutate(Rk=year,Player=ifelse(Player=="Nenê Hilário","Nenê",Player)) %>% rename(Season=Rk) %>% 
    select(Season,Player,Type,WS,Terms) %>% clean_names()
  return (a)
}

free_agents<-get_free_agents(2016)
sapply(2017:2019,function(x){
  new_free<-get_free_agents(x)
  free_agents<<-rbind(free_agents,new_free)
})

# remove retired players
free_agents<-free_agents %>% filter(terms != "Retired") %>% mutate(contract_yrs=NA,yr_1_salary=NA)

no_contract<-free_agents %>% 
  filter(terms==""|
           #remove players going to play in different countries
           str_detect(terms,"China|Greece|Israel|Russia|Spain|Turkey|Italy|Moscow|Germany|France|Australia|Croatia")|
           #remove players with explicitly non-guaranteed first year
           str_detect(terms,"camp|two-way|Exhibit|2-way"))
free_agents<-anti_join(free_agents,no_contract)
no_contract<-no_contract %>% mutate(contract_yrs=0,yr_1_salary=0)
free_agents<-full_join(free_agents,no_contract)
#write_csv(free_agents,"2016-2019 Free Agents.csv")
#add Jr. to Derrick Jones, Kelly Oubre, Tim Hardaway and Walt Lemon
#add III to James Webb, Glenn Robinson, Frank Mason
#add II to Gary Payton
#include option years, partial guaranteed years in counting contract years

#wait until after free agent window!!!
url="https://www.spotrac.com/nba/free-agents/"
fa_2020<-url %>% read_html() %>% html_nodes("table") %>% .[[1]] %>% html_table() %>% 
  rename(Player=`Player (206)`) %>% select(Player,Type) %>% mutate(season=2020)
weird_names<-fa_2020 %>% filter(str_detect(Player,"Jr.|II|III"))
fa_2020<-anti_join(fa_2020,weird_names)
weird_names<-weird_names %>% separate(Player,c("Name1","Player"),"\\.|III|II") %>%
  mutate(Player=ifelse(str_detect(Player,"Jr"),paste0(Player,"."),Player)) %>%
  mutate(Player=ifelse(str_detect(Player,"Bagley|Robinson|Mason"),paste0(Player,"III"),Player)) %>%
  mutate(Player=ifelse(str_detect(Player,"Payton"),paste0(Player,"II"),Player)) %>%
  mutate(Player=ifelse(str_detect(Player,"Otto|Charlie"),substr(Player,0,nchar(Player)-4),Player)) %>%
  mutate(Player=ifelse(str_detect(Player,"Bowen"),substr(Player,0,nchar(Player)-1),Player)) %>%
  select(-Name1)
fa_2020<-fa_2020 %>% separate(Player,c("Player","Last")," ") %>% mutate(Player=substr(Player,nchar(Last)+1,nchar(Player))) %>% 
  unite("Player",Player:Last,sep=" ")
fa_2020<-full_join(fa_2020,weird_names) %>% arrange(Player) %>% clean_names() %>% 
  mutate(contract_yrs=NA,first_year_percent_of_cap=NA)
rm(weird_names)

write_csv(fa_2020,"Free Agents 2020.csv")
#go into excel and correct names to match bball-ref

salary_cap_hist_url<-"https://basketball.realgm.com/nba/info/salary_cap"
salary_cap_hist<-salary_cap_hist_url %>% read_html() %>% 
  html_node("table") %>% html_table(fill=TRUE)
#add correct column names (ended up as first row)
colnames(salary_cap_hist)<-salary_cap_hist[1,]
salary_cap_hist<-salary_cap_hist[-1,]
#only take year and cap number, parse cap into a number (has dollar sign and commas originally)
salary_cap_hist<-salary_cap_hist %>% select(3:4) %>%
  rename(season=`Luxury Tax`,cap=BAE) %>%
  mutate(season=as.numeric(substr(season,0,4))) %>%
  mutate(cap=parse_number(cap))
write_csv(salary_cap_hist,"Salary Cap History.csv")
