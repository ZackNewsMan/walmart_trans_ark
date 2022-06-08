con_2018 <- con_2018_2 <- read_csv("H:/Politics/Walmart trrans bill donor/con_2018_2.csv")
con_2019 <- con_2019_2 <- read_csv("H:/Politics/Walmart trrans bill donor/con_2019_2.csv")
con_2020 <- con_2020_2 <- read_csv("H:/Politics/Walmart trrans bill donor/con_2020_2.csv")
con_2021 <- con_2021_2 <- read_csv("H:/Politics/Walmart trrans bill donor/con_2021_2.csv")
con_2022 <- con_2022_2 <- read_csv("H:/Politics/Walmart trrans bill donor/con_2022_2.csv")

library(tidyverse)


con_2018 %>% 
  filter(grepl("Walmart",last_name)) %>% 
           View()

  walmart_con_2018 <- con_2018 %>% 
    filter(grepl("Walmart",last_name))

  # This worked to pull out all donations Walmart made in 2018
    # Strategy will be to pull out each one per year, and then full join. From there, will group and summarize
      # Inspo is myself, from my street racing project. 
        # https://github.com/ZackNewsMan/street_racing_9WTK/blob/main/street%20race_4.R 

 # 2019 

    con_2019 %>% 
      filter(grepl("Walmart",last_name)) %>% 
      View()
  
    walmart_con_2019 <- con_2019 %>% 
      filter(grepl("Walmart",last_name))
    
  # 2020     
    
    con_2020 %>% 
      filter(grepl("Walmart",last_name)) %>% 
      View()
    
    walmart_con_2020 <- con_2020 %>% 
      filter(grepl("Walmart",last_name))
    
  # 2021
    
    con_2021 %>% 
      filter(grepl("Walmart",last_name)) %>% 
      View()
    
    walmart_con_2021 <- con_2021 %>% 
      filter(grepl("Walmart",last_name))
    
    
  # 2022   
    
    con_2022 %>% 
      filter(grepl("Walmart",last_name)) %>% 
      View()
    
    walmart_con_2022 <- con_2022 %>% 
      filter(grepl("Walmart",last_name))
    
    
    # let's join babyyyyyyyyyyy
    
          walmart_con_18_19 <- walmart_con_2019 %>% full_join(walmart_con_2018)
          
          walmart_con_18_20 <- walmart_con_18_19 %>% full_join(walmart_con_2020)
          
          walmart_con_18_21 <- walmart_con_18_20 %>% full_join(walmart_con_2021)
          
          walmart_con_18_22 <- walmart_con_18_21 %>% full_join(walmart_con_2022)
          
    # now let's sum and count 
         
          # committe name
          
          walmart_con_18_22 %>% 
            group_by(committee_name) %>% 
            summarize(contribution_count = n(),
                      total_contribution = sum(receipt_amount)) %>% 
            View()
          
          # by candidate name
         
          walmart_con_18_22 %>% 
            group_by(candidate_name) %>% 
            summarize(contribution_count = n(),
                    total_contribution = sum(receipt_amount)) %>% 
            View()
          
         
        candidate_walmart_con_18_22 <- walmart_con_18_22 %>% 
            group_by(candidate_name) %>% 
            summarize(contribution_count = n(),
                      total_contribution = sum(receipt_amount))
    
        # and let's see if they gave to any sponsors
        
        candidate_walmart_con_18_22 %>% 
        filter(grepl("Lundstrum",candidate_name)|("Clark", candidate_name)|("Barker", candidate_name)) %>% 
          View()
        
        candidate_walmart_con_18_22 %>% 
          filter(grepl(("Lundstrum", candidate_name)|("Clark", candidate_name)|("Barker", candidate_name)) %>% 
          View()
          
          candidate_walmart_con_18_22 %>% write_csv("candidate_walmart_con_18_22.csv", na = "")
          
          # I don't have time to fgiure out why grepl has screwed itself, but this is what I did in SQL for HB1570
          
            # https://www.arkleg.state.ar.us/Bills/Detail?id=HB1570&ddBienniumSession=2021%2F2021R
              # Hits for Hiram Richelieu Hill but idk if same guy as Ricky Hill
                  # I think it is. Both ran in Senate District 29 with a Cabot, Arkansaw address
              # There's a Mark and Blake Johnson that sponsored, but Lee didn't. So would need to delete Lee too.
          
          SELECT *
            FROM candidate_walmart_con_18_22
          WHERE candidate_name LIKE "%Lundstrum"
          OR candidate_name LIKE "%Clark"
          OR candidate_name LIKE "%Barker"
          OR candidate_name LIKE "%Bentley"
          OR candidate_name LIKE "%Brown"
          OR candidate_name LIKE "%Bryant"
          OR candidate_name LIKE "%Cavenaugh"
          OR candidate_name LIKE "%Cloud"
          OR candidate_name LIKE "%Coleman"
          OR candidate_name LIKE "%Cooper"
          OR candidate_name LIKE "%Cozart"
          OR candidate_name LIKE "%Crawford"
          OR candidate_name LIKE "%Dalby"
          OR candidate_name LIKE "%Dotson"
          OR candidate_name LIKE "%Fite"
          OR candidate_name LIKE "%Furman"
          OR candidate_name LIKE "%Gazaway"
          OR candidate_name LIKE "%Gonzales"
          OR candidate_name LIKE "%Gray"
          OR candidate_name LIKE "%Haak"
          OR candidate_name LIKE "%Hollowell"
          OR candidate_name LIKE "%Ladyman"
          OR candidate_name LIKE "%Lowery"
          OR candidate_name LIKE "%Lynch"
          OR candidate_name LIKE "%Mayberry"
          OR candidate_name LIKE "%McGrew"
          OR candidate_name LIKE "%McNair"
          OR candidate_name LIKE "%Meeks"
          OR candidate_name LIKE "%Miller"
          OR candidate_name LIKE "%Payton"
          OR candidate_name LIKE "%Penzo"
          OR candidate_name LIKE "%Pilkington"
          OR candidate_name LIKE "%Ray"
          OR candidate_name LIKE "%Richmond"
          OR candidate_name LIKE "%Slape"
          OR candidate_name LIKE "%Smith"
          OR candidate_name LIKE "%Speaks"
          OR candidate_name LIKE "%Tollett"
          OR candidate_name LIKE "%Tosh"
          OR candidate_name LIKE "%Underwood"
          OR candidate_name LIKE "%Vaught"
          OR candidate_name LIKE "%Warren"
          OR candidate_name LIKE "%Watson"
          OR candidate_name LIKE "%Wing"
          OR candidate_name LIKE "%Bragg"
          OR candidate_name LIKE "%Hillman"
          OR candidate_name LIKE "%Wooten"
          OR candidate_name LIKE "%Ballinger"
          OR candidate_name LIKE "%Beckham"
          OR candidate_name LIKE "%Bledsoe"
          OR candidate_name LIKE "%Davis"
          OR candidate_name LIKE "%English"
          OR candidate_name LIKE "%Gilmore"
          OR candidate_name LIKE "%Hammer"
          OR candidate_name LIKE "%Hill"
          OR candidate_name LIKE "%Irvin"
          OR candidate_name LIKE "%Johnson"
          OR candidate_name LIKE "%Rapert"
          OR candidate_name LIKE "%Rice"
          OR candidate_name LIKE "%Stubblefield"
          OR candidate_name LIKE "%Wallace"
          OR candidate_name LIKE "%Sullivan"
          OR candidate_name LIKE "%Hester"
          OR candidate_name LIKE "%Garner"
          OR candidate_name LIKE "%Irvin"
          OR candidate_name LIKE "%Barker"
          OR candidate_name LIKE "%Bentley"
          OR candidate_name LIKE "%Berry"
          OR candidate_name LIKE "%Brown"
          OR candidate_name LIKE "%Cavenaugh"
          OR candidate_name LIKE "%Cloud"
          OR candidate_name LIKE "%Cooper"
          OR candidate_name LIKE "%Cozart"
          OR candidate_name LIKE "%Crawford"
          OR candidate_name LIKE "%Dalby"
          OR candidate_name LIKE "%Fite"
          OR candidate_name LIKE "%Furman"
          OR candidate_name LIKE "%Gonzales"
          OR candidate_name LIKE "%Gray"
          OR candidate_name LIKE "%Haak"
          OR candidate_name LIKE "%Ladyman"
          OR candidate_name LIKE "%Lundstrum"
          OR candidate_name LIKE "%Mayberry"
          OR candidate_name LIKE "%McGrew"
          OR candidate_name LIKE "%Milligan"
          OR candidate_name LIKE "%Payton"
          OR candidate_name LIKE "%Pilkington"
          OR candidate_name LIKE "%Ray"
          OR candidate_name LIKE "%Richmond"
          OR candidate_name LIKE "%Smith"
          OR candidate_name LIKE "%Speaks"
          OR candidate_name LIKE "%Vaught"
          OR candidate_name LIKE "%Watson"
          OR candidate_name LIKE "%Womack"
          OR candidate_name LIKE "%Penzo"
          OR candidate_name LIKE "%Tosh"
          OR candidate_name LIKE "%Wing"
          OR candidate_name LIKE "%Beckham"
          OR candidate_name LIKE "%Bledsoe"
          OR candidate_name LIKE "%Clark"
          OR candidate_name LIKE "%Davis"
          OR candidate_name LIKE "%English"
          OR candidate_name LIKE "%Garner"
          OR candidate_name LIKE "%Gilmore"
          OR candidate_name LIKE "%Hammer"
          OR candidate_name LIKE "%Hester"
          OR candidate_name LIKE "%Johnson"
          OR candidate_name LIKE "%Rapert"
          OR candidate_name LIKE "%Stubblefield"
          OR candidate_name LIKE "%Wallace"
          OR candidate_name LIKE "%Dismang"
          OR candidate_name LIKE "%John"
                    
        
                                          ################# Rob Walton #############    
    
    # 2019 
    
    
    
    
                                  ################# Carrie Walton-Penner #############    
    
    
    