## generate profile data

experiment <- data.frame(
  party = sample(c("SD", "Venstre", "DPP", "SL", "SF"), 5),
  polls = sample(c(2, 5, 10, 15, 20, 25), 5),
  sn_emphasised = # randomly assign to binary
    as.integer(rbernoulli(5, p = 0.2)),
  dn_emphasised = # randomly assign to binary
    as.integer(rbernoulli(5, p = 0.2))) %>%
  mutate(change = case_when(
    polls < 2 ~ sample(
      c(-10, -7, -5, -2, 0), 5),
    polls < 7 ~ sample(
      c(-10, -7, -5, -2, 0, 2), 5),
    polls < 10 ~ sample(
      c(-10, -7, -5, -2, 0, 2, 5, 7), 5),
    TRUE ~ sample(c(-10, -7, -5, -2, 0, 2, 5, 7, 10), 5)))

for(i in 1:2000){
  experiment <- rbind(experiment,
                      data.frame(
                        party = sample(c("SD", "Venstre", "DPP", "SL", "SF"), 5),
                        polls = sample(c(2, 5, 10, 15, 20, 25), 5),
                        sn_emphasised = # randomly assign to binary
                          as.integer(rbernoulli(5, p = 0.2)),
                        dn_emphasised = # randomly assign to binary
                          as.integer(rbernoulli(5, p = 0.2))) %>%
                        mutate(change = case_when(
                          polls < 5 ~ sample(
                            c(-10, -7, -5, -2, 0, 2), 5),
                          polls < 7 ~ sample(
                            c(-10, -7, -5, -2, 0, 2, 5), 5),
                          polls < 10 ~ sample(
                            c(-10, -7, -5, -2, 0, 2, 5, 7), 5),
                          TRUE ~ sample(c(-10, -7, -5, -2, 0, 2, 5, 7, 10), 5))))
}

profiles <- experiment %>%
  mutate(
    sn_emphasis = case_when(
      sn_emphasised == 1 & polls == 25 ~ 
        "With a very strong showing in the polls, this party looks likely to be the largest party in a governing coalition.",
      sn_emphasised == 1 & polls == 20 ~ 
        "A large vote share like this means that this party will be one of the largest in parliament.",
      sn_emphasised == 1 & polls == 15 ~
        "With a decent vote share in the polls, this party looks set to win a considerable number of seats in parliament.",
      sn_emphasised == 1 & polls == 10 ~
        "With a small but non-trivial vote share in the polls, you would expect this party to win quite a few seats in parliament.",
      sn_emphasised == 1 & polls == 5 ~
        "With such a small share of the vote, this party looks set to win only a few seats in parliament.",
      sn_emphasised == 1 & polls == 2 ~
        "With such low poll numbers, it looks questionable whether or not this party will cross the treshold for representation in parliament.",
      TRUE ~ ""
    ),
    dn_emphasis = case_when(
      dn_emphasised == 1 & change <= -5 ~ 
        "This party is in a state of crisis, losing considerable ground in the polls.",
      dn_emphasised == 1 & change > -5 & change < 0 ~ 
        "This party appears to be losing ground slightly in the polls.",
      dn_emphasised == 1 & change == 0 ~ 
        "This party has neither lost nor gained ground in the polls.",
      dn_emphasised == 1 & change < 5 & change > 0 ~ 
        "This party appears to be gaining ground slightly in the polls.",
      dn_emphasised == 1 & change >= 5 ~ 
        "This party has the wind in its sails and is gaining considerable ground in the polls.",
      TRUE ~ ""
    ),
    polls = paste0("National polls: ",
                   floor(jitter(as.numeric(polls), amount = 2)), "%."),
    emphases = paste0(sn_emphasis, " ", dn_emphasis),
    change = case_when(
      change >= 0 ~ paste0("National change since last poll: +", change),
      change < 0 ~ paste0("National change since last poll: ", change))
  ) %>%
  select(
    party,
    polls, 
    change,
    emphases)

saveRDS(experiment, here::here("data", "experiment.rds"))
saveRDS(profiles, here::here("data", "profiles.rds"))
