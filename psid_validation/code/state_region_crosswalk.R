## Manually created data

# Region 1: Northeast
# Division 1: New England (Connecticut, Maine, Massachusetts, New Hampshire, Rhode Island, and Vermont)
# Division 2: Middle Atlantic (New Jersey, New York, and Pennsylvania)
# 
# Region 2: Midwest (Prior to June 1984, the Midwest Region was designated as the North Central Region.)
# Division 3: East North Central (Illinois, Indiana, Michigan, Ohio, and Wisconsin)
# Division 4: West North Central (Iowa, Kansas, Minnesota, Missouri, Nebraska, North Dakota, and South Dakota)
# 
# Region 3: South
# Division 5: South Atlantic (Delaware; Florida; Georgia; Maryland; North Carolina; South Carolina; Virginia; Washington, D.C. and West Virginia)
# Division 6: East South Central (Alabama, Kentucky, Mississippi, and Tennessee) Division 7: West South Central (Arkansas, Louisiana, Oklahoma, and Texas)
# 
# Region 4: West
# Division 8: Mountain (Arizona, Colorado, Idaho, Montana, Nevada, New Mexico, Utah, and Wyoming)
# Division 9: Pacific (Alaska, California, Hawaii, Oregon, and Washington)

state_correspondance <- tibble(state_code = 1:51,
                               state_name = c("Alabama", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming", "Alaska", "Hawaii"))

state_correspondance <- state_correspondance %>% 
  mutate(region = case_when(state_name %in% c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont", "New Jersey", "New York", "Pennsylvania") ~ "Northeast",
                            state_name %in% c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin", "Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota") ~ "Midwest",
                            state_name %in% c("Delaware", "Florida", "Georgia", "Maryland", "North Carolina", "South Carolina", "Virginia", "District of Columbia", "West Virginia", "Alabama", "Kentucky", "Mississippi", "Tennessee", "Arkansas", "Louisiana", "Oklahoma", "Texas") ~ "South",
                            state_name %in% c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", "Wyoming", "Alaska", "California", "Hawaii", "Oregon", "Washington") ~ "West"))

state_correspondance %>% count(region, state_name, state_code) %>% print(n = 51)

fwrite(state_correspondance, here("out/data/state_correspondance.csv"))