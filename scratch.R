
source("utils.R")





rolls <- dbGetQuery(con, "select p.*, r.date, r.question, b.sponsor_party, b.primary_subject, b.cosponsors
                    from congress.positions p
                    left join congress.roll_call r
                    on p.congress = r.congress
                    and p.session = r.session
                    and p.chamber = r.chamber
                    and p.roll_call = r.roll_call
                    left join bill.bills b
                    on r.bill_id = b.bill_id
                    --where p.congress > 100 and p.congress < 106 
                    where p.congress = 114
                    and p.chamber = 'Senate'
                    --and r.aye > 5
                    --and r.nay > 5
                    and p.vote_code is not null
                    --limit 10000") %>%
  mutate(question = str_trim(question))

bills <- dbGetQuery(con, "select b.*, s.name, s.url_name from bill.bills b
                    left join bill.subjects s
                    on s.bill_id = b.bill_id
                    where b.congress = 114
                    and b.original_chamber = 'senate'")

cosp <- dbGetQuery(con, "select c.*, b.sponsor_party, b.primary_subject, b.cosponsors
                    from bill.cosponsors c
                   left join bill.bills b
                   on c.bill_id = b.bill_id
                   --where b.congress > 100 and b.congress < 106 
                   where b.congress = 114
                   and b.original_chamber = 'senate'
                   --limit 10000")

members <- dbGetQuery(con, "select * from congress.member_roster where congress = 114") %>%
  select(member_id, first_name, district, at_large)


pre_data <- rolls %>%
  # sample_n(50000) %>%
  select(member_id, name, state, party, vote_code, date, congress,
         session, roll_call, question, sponsor_party, cosponsors, primary_subject) %>%
  filter(vote_code %in% c(0,1)) %>%
  mutate(vote_id = paste0(congress, "-", session, "-", roll_call),
         party_code = case_when(
           party == "R" ~ 1,
           party == "D" ~ -1,
           1 == 1 ~ 0
         ),
         period = as.numeric((date - min(date))) %/% 80 + 1) %>%
  left_join(members, by = "member_id") %>%
  filter(!is.na(first_name))

member_matrix <- model.matrix(name ~ party_code,
                              data = pre_data %>%
                                count(name, party_code))

data <- list(
  N = nrow(pre_data),
  L = nrow(member_matrix),
  V = length(unique(pre_data$vote_id)),
  K = 2,
  legislator = as.numeric(factor(pre_data$name)),
  vote = as.numeric(factor(pre_data$vote_id)),
  X_alpha = member_matrix,
  K_L = ncol(member_matrix),
  # X_vote = model.matrix(vote_id ~ question,
  #                       data = pre_data %>%
  #                         count(vote_id, question, sponsor_party, cosponsors, primary_subject)),
  # K_V = ncol(model.matrix(vote_id ~ question,
  #                         data = pre_data %>%
  #                           count(vote_id, question, sponsor_party, cosponsors, primary_subject))),
  outcome = pre_data$vote_code
)

init_function <- function(chain_id = 1,
                          member_length,
                          party_codes) {
  list(
    delta_alpha_1 = as.array(c(0)),
    delta_alpha_2 = as.array(c(1.5)),
    alpha = rnorm(member_length, 3, 0.5) * party_codes
  )
}

n_chains <- 2
init_list <- lapply(1:n_chains, function(id) init_function(id, member_length = data$L, party_codes = data$X_alpha[,2]))

bafumi <- stan(file = "stan/bafumi.stan",
                data = data,
                iter = 1000,
                init = init_list,
                chains = n_chains)

summary <- rstan::summary(bafumi)$summary %>%
  as.data.frame() %>%
  mutate(params = rownames(.))

party_vector <- (pre_data %>%
                   count(name, party = party))$party

alpha <- summary %>%
  filter(grepl("alpha_adj", params)) %>%
  mutate(member_name = levels(factor(pre_data$name)),
         party = party_vector)

beta <- summary %>%
  filter(grepl("beta_adj", params)) %>%
  mutate(vote_id = levels(factor(pre_data$vote_id)),
         parameter = "beta") %>%
  separate(vote_id, into = c("congress", "session", "roll_call_number"),
           convert = TRUE)

gamma <- summary %>%
  filter(grepl("gamma_adj", params)) %>%
  mutate(vote_id = levels(factor(pre_data$vote_id)),
         parameter = "gamma") %>%
  separate(vote_id, into = c("congress", "session", "roll_call_number"),
           convert = TRUE)

vote_results <- rbind(beta,
                      gamma)

alpha %>%
  ggplot(aes(member_name,mean,color=party)) +
  geom_point() +
  geom_segment(aes(xend = member_name, y = `25%`, yend = `75%`)) +
  scale_color_manual(values = c("D" = "blue", "I" = "green", "R" = "red")) +
  coord_flip()


bill_data <- bills %>%
  group_by(bill_id) %>%
  sample_n(1)

bill_params <- vote_results %>%
  mutate(vote_id = paste0(congress, "-", session, "-", roll_call_number)) %>%
  # rbind(beta, gamma) %>%
  # mutate(par = str_extract(params, "beta|gamma")) %>%
  left_join(bill_data, by = c("vote_id" = "bill_id")) %>%
  mutate(bill_progress = case_when(
    !is.na(enacted) ~ "enacted",
    !is.na(house_passage) ~ "house passage",
    !is.na(senate_passage) ~ "senate passage",
    1 == 1 ~ "none"
  ))

logit_pred <- bill_params %>%
  # select(`25%`, bill_id, par) %>%
  select(`25%`, vote_id, parameter) %>%
  spread(parameter, `25%`) %>%
  mutate(id = 1) %>%
  left_join(
    data.frame(x = seq(-10, 10, by = 0.1),
               id = 1)
  ) %>%
  mutate(log_prob = gamma * (x - beta),
         prob = exp(log_prob) / (1 + exp(log_prob))) %>%
  left_join(rolls %>%
              mutate(vote_id = paste0(congress, "-", session, "-", roll_call)) %>%
              count(vote_id, question),
            by = c("vote_id"))
  # left_join(bills, by = c("bill_id" = "bill_id")) %>%
  # mutate(bill_progress = case_when(
  #   !is.na(enacted) ~ "enacted",
  #   !is.na(house_passage) ~ "house passage",
  #   !is.na(senate_passage) ~ "senate passage",
  #   1 == 1 ~ "none"
  # ))

logit_pred %>%
  mutate(question = ifelse(is.na(question), "NA", question))
ggplot() +
  geom_point(data = alpha,
             aes(mean, y=1, color = party),
             pch = 17, size = 3) +
  xlim(c(-2,2)) +
  scale_color_manual(values = c("D" = "blue","I" = "green", "R" = "red"
                                # "enacted" = "orange",
                                # "house passage" = "purple",
                                # "senate passage" = "brown",
                                # "none" = "gray"
                                )) +
  geom_line(data = logit_pred %>%
              # filter(question == "On the Nomination") %>%
              mutate(question = ifelse(is.na(question), "NA", question)),
            aes(x = x, y = prob, mapping = vote_id),
            color = "purple",
            alpha = 0.7) +
  facet_wrap(~question)




