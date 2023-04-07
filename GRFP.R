library(dplyr)
library(ggplot2)
library(ggthemes)
library(stringr) 

d <- read.delim(file = "./AwardeeList.do", header=T, sep = "\t", stringsAsFactors = FALSE) #Reading in file

# merging bacc when there is no current institution
d <- d %>% mutate(Current.Institution = ifelse(Current.Institution == "", Baccalaureate.Institution, Current.Institution)) 
    
#Cleaning institution names
d <- d %>% 
  mutate(across(Current.Institution, str_replace, 'ARIZONA STATE UNIVERSITY', 'Arizona State University')) %>% 
  mutate(across(Current.Institution, str_replace, 'UNIVERSITY OF ARIZONA', 'University of Arizona')) %>% 
  mutate(across(Current.Institution, str_replace, 'Baylor College of Medicine|Baylor University', 'Baylor')) %>% 
  mutate(across(Current.Institution, str_replace, 'Board of Regents, NSHE, obo University of Nevada, Reno', 'University of Nevada Reno')) %>%
  mutate(across(Current.Institution, str_replace, 'CALIFORNIA STATE UNIVERSITY , FULLERTON', 'California State University, Fullerton')) %>%
  mutate(across(Current.Institution, str_replace, 'Carnegie Mellon University', 'Carnegie-Mellon University')) %>%
  mutate(across(Current.Institution, str_replace, 'COLORADO STATE UNIVERSITY', 'Colorado State University')) %>%
  mutate(across(Current.Institution, str_replace, 'CORNELL UNIVERSITY|Joan and Sanford I. Weill Medical College of Cornell University|Weill Cornell Graduate School of Medical Sciences', 'Cornell University')) %>%
  mutate(across(Current.Institution, str_replace, 'GEORGE WASHINGTON UNIVERSITY, THE', 'George Washington University')) %>% 
  mutate(across(Current.Institution, str_replace, 'HARVARD COLLEGE, PRESIDENT & FELLOWS OF|Harvard T. H. Chan School of Public Health|Harvard University|Harvard University Medical School|Harvard Medical School', 'Harvard')) %>% 
  mutate(across(Current.Institution, str_replace, 'IOWA STATE UNIVERSITY OF SCIENCE AND TECHNOLOGY', 'Iowa State University')) %>% 
  mutate(across(Current.Institution, str_replace, 'MICHIGAN STATE UNIVERSITY', 'Michigan State University')) %>%
  mutate(across(Current.Institution, str_replace, 'NEW YORK UNIVERSITY', 'New York University')) %>% 
  mutate(across(Current.Institution, str_replace, 'OHIO STATE UNIVERSITY|The Ohio State University/CETE', 'Ohio State University')) %>% 
  mutate(across(Current.Institution, str_replace, 'PENNSYLVANIA STATE UNIVERSITY, THE|Pennsylvania State Univ University Park|The Pennsylvania State University', 'Pennsylvania State University')) %>% 
  mutate(across(Current.Institution, str_replace, 'Regents of the University of Idaho', 'University of Idaho')) %>% 
  mutate(across(Current.Institution, str_replace, 'Regents of the University of Michigan - Ann Arbor|The University of Michigan|University of Michigan-Ann Arbor|University of Michigan - Ann Arbor', 'University of Michigan')) %>% 
  mutate(across(Current.Institution, str_replace, 'RUTGERS, THE STATE UNIVERSITY OF NEW JERSEY|Rutgers University Camden|Rutgers University New Brunswick', 'Rutgers')) %>% 
  mutate(across(Current.Institution, str_replace, 'SOUTHERN METHODIST UNIVERSITY', 'Southern Methodist University')) %>% 
  mutate(across(Current.Institution, str_replace, 'LELAND STANFORD JUNIOR UNIVERSITY, THE|STANFORD LELAND JUNIOR UNIVERSITY', 'Stanford University')) %>% 
  mutate(across(Current.Institution, str_replace, 'SUNY, UNIVERSITY AT BUFFALO', 'SUNY at Buffalo')) %>% 
  mutate(across(Current.Institution, str_replace, 'UNIVERSITY OF CENTRAL FLORIDA BOARD OF TRUSTEES, THE', 'The University of Central Florida Board of Trustees')) %>% 
  mutate(across(Current.Institution, str_replace, 'Boston University', 'Trustees of Boston University')) %>% 
  mutate(across(Current.Institution, str_replace, 'UNITED STATES MILITARY ACADEMY', 'United States Military Academy')) %>% 
  mutate(across(Current.Institution, str_replace, 'UNIVERSITY OF ALASKA FAIRBANKS|University of Alaska Fairbanks Campus', 'University of Alaska Fairbanks')) %>% 
  mutate(across(Current.Institution, str_replace, 'UNIVERSITY OF CALIFORNIA SAN DIEGO|University of California-San Diego Scripps Inst of Oceanography', 'University of California-San Diego')) %>% 
  mutate(across(Current.Institution, str_replace, 'UNIVERSITY OF CALIFORNIA, BERKELEY|University of California-Berkeley', 'University of California, Berkeley')) %>% 
  mutate(across(Current.Institution, str_replace, 'UNIVERSITY OF CALIFORNIA, DAVIS', 'University of California, Davis')) %>% 
  mutate(across(Current.Institution, str_replace, 'University of California-Irvine', 'University of California, Irvine')) %>% 
  mutate(across(Current.Institution, str_replace, 'University of California-Los Angeles', 'University of California, Los Angeles')) %>% 
  mutate(across(Current.Institution, str_replace, 'University of California-Riverside', 'University of California, Riverside')) %>% 
  mutate(across(Current.Institution, str_replace, 'UNIVERSITY OF CALIFORNIA, SAN FRANCISCO', 'University of California, San Francisco')) %>% 
  mutate(across(Current.Institution, str_replace, 'University of California-Santa Barbara', 'University of California, Santa Barbara')) %>% 
  mutate(across(Current.Institution, str_replace, 'University of California-Santa Cruz', 'University of California, Santa Cruz')) %>% 
  mutate(across(Current.Institution, str_replace, 'University of Colorado Boulder|University of Colorado, Boulder', 'University of Colorado at Boulder')) %>% 
  mutate(across(Current.Institution, str_replace, 'UNIVERSITY OF DENVER', 'University of Denver')) %>% 
  mutate(across(Current.Institution, str_replace, 'University of Illinois Urbana-Champaign', 'University of Illinois at Urbana-Champaign')) %>% 
  mutate(across(Current.Institution, str_replace, 'UNIVERSITY OF KANSAS|University of Kansas Center for Research Inc', 'University of Kansas')) %>% 
  mutate(across(Current.Institution, str_replace, 'UNIVERSITY OF MARYLAND|University of Maryland Center for Environmental Sciences', 'University of Maryland')) %>% 
  mutate(across(Current.Institution, str_replace, 'UNIVERSITY OF MIAMI', 'University of Miami')) %>% 
  mutate(across(Current.Institution, str_replace, 'University of Minnesota - Twin Cities', 'University of Minnesota-Twin Cities')) %>% 
  mutate(across(Current.Institution, str_replace, 'UNIVERSITY OF NORTH CAROLINA AT CHAPEL HILL', 'University of North Carolina at Chapel Hill')) %>% 
  mutate(across(Current.Institution, str_replace, 'UNIVERSITY OF NORTH TEXAS', 'University of North Texas')) %>% 
  mutate(across(Current.Institution, str_replace, 'University of Puerto Rico Rio Piedras', 'University of Puerto Rico-Rio Piedras')) %>% 
  mutate(across(Current.Institution, str_replace, 'UNIVERSITY OF RHODE ISLAND', 'University of Rhode Island')) %>% 
  mutate(across(Current.Institution, str_replace, 'UNIVERSITY OF WASHINGTON', 'University of Washington')) %>% 
  mutate(across(Current.Institution, str_replace, 'VIRGINIA POLYTECHNIC INSTITUTE & STATE UNIVERSITY', 'Virginia Polytechnic Institute and State University')) %>% 
  mutate(across(Current.Institution, str_replace, 'Wake Forest University School of Medicine', 'Wake Forest University')) %>% 
  mutate(across(Current.Institution, str_replace, 'Washington University', 'Washington University in St. Louis')) %>% 
  mutate(across(Current.Institution, str_replace, 'William Marshall Rice University', 'William Marsh Rice University')) %>% 
  mutate(across(Current.Institution, str_replace, 'YALE UNIVERSITY', 'Yale University'))
  
   
institution_count <- d %>% 
#  distinct(Baccalaureate.Institution, Current.Institution) %>%
  group_by(Current.Institution) %>%
  summarize("award_count" = n()) #getting count of awards per institution

sum(institution_count$award_count) # checking award count

#Count and frequency of awards per institution
institution_freq <- institution_count %>%
  group_by(Current.Institution) %>%
  summarise(n = sum(award_count)) %>%
  mutate(freq = n / sum(institution_count$award_count))
