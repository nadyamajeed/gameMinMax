########## set up ##########

# load libraries
library(gsheet)
library(dplyr)
library(tidyr)
library(clipr)

# read in type vs type data
combis = read.csv("https://raw.githubusercontent.com/nadyamajeed/gameMinMax/main/pokemongo/combis.csv")

########## score my pokemon ##########

# custom functions for convenience
findAttackers = function(x) combis$Attacker[combis %>% with(Defender==x & AttackMultiplier=="Strong")]
findTargets = function(x) combis$Defender[combis %>% with(Attacker==x & AttackMultiplier=="Strong")]

# read in and clean my partial pokemon database
myPokemon = 
  gsheet2tbl("https://docs.google.com/spreadsheets/d/1TjzPZEZbEwejC6Aq5h-PUB4tvglMcMFNYjqximbYe6c/edit#gid=0") %>%
  tidyr::separate_wider_delim(cols = Type, delim = "/", names = c("Type1", "Type2"), too_few = "align_start", cols_remove = FALSE) %>%
  dplyr::mutate(
    STAB = ifelse(`Fast Attack` == Type1 | `Fast Attack` == Type2, "Yes", "No"),
    STAB = ifelse(is.na(STAB), "No", STAB),
    `Gap in CP` = `Max CP` - `Current CP`)

# get fighting data for my pokemon
for(i in 1:nrow(myPokemon)) {
  # pokemon i should run from
  currentHide = findAttackers(myPokemon$Type1[i])
  if(!is.na(myPokemon$Type2[i])) {
    currentHide = c(currentHide, findAttackers(myPokemon$Type2[i]))
    safeFrom = combis$Attacker[combis %>% with(
      (Defender==myPokemon$Type1[i] & AttackMultiplier=="Weak") |
        (Defender==myPokemon$Type2[i] & AttackMultiplier=="Weak"))]
    currentHide = currentHide[!(currentHide %in% safeFrom)]
    }
  myPokemon[i, "HIDE FROM"] = currentHide %>% 
    table(dnn = "Attacker") %>% 
    as.data.frame() %>% 
    dplyr::arrange(-Freq, Attacker) %>% 
    dplyr::mutate(Attacker = ifelse(Freq == 1, as.character(Attacker), paste0(Attacker, "*"))) %>%
    dplyr::pull(Attacker) %>%
    paste0(collapse = ", ")
  # pokemon i can take down - fast attack
  currentFite = findTargets(myPokemon$`Fast Attack`[i])
  if(length(currentFite) == 0) currentFite = "-"
  myPokemon[i, "USE AGAINST"] = currentFite %>%
    table(dnn = "Target") %>% 
    as.data.frame() %>%  
    dplyr::arrange(-Freq, Target) %>% 
    dplyr::mutate(Target = ifelse(Freq == 1, as.character(Target), paste0(Target, "*"))) %>%
    dplyr::pull(Target) %>%
    paste0(collapse = ", ")
}; rm(i); rm(currentHide); rm(currentFite); rm(safeFrom)

# clean up and write to clipboard
myPokemon = myPokemon %>%
  dplyr::select(-Type1, -Type2) %>%
  dplyr::arrange(`Gap in CP`, -`Current CP`) %>% 
  clipr::write_clip()

########## create teams against each type ##########

types = combis$Defender %>% unique()

for(type in types) {
  cat("\nAnti-", type, " Team\n", sep = "")
  myPokemon %>%
    dplyr::arrange(-`Current CP`) %>%
    dplyr::filter(!grepl(type, `HIDE FROM`)) %>%
    dplyr::filter(grepl(type, `USE AGAINST`)) %>%
    dplyr::select(Species, `Current CP`, `Fast Attack`) %>%
    dplyr::slice(1:6) %>%
    as.data.frame() %>%
    print()
}; rm(type)

########## end of code ##########
