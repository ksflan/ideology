



source("utils.R")


current_congress <- (lubridate::year(today() - 3) - 1789) / 2 + 1


rolls <- dbGetQuery(con, paste0("select p.*, r.date, r.question, b.sponsor_party, b.primary_subject, b.cosponsors
                    from congress.positions p
                    left join congress.roll_call r
                    on p.congress = r.congress
                    and p.session = r.session
                    and p.chamber = r.chamber
                    and p.roll_call = r.roll_call
                    left join bill.bills b
                    on r.bill_id = b.bill_id
                    where p.congress = ", current_congress, " 
                    and p.chamber = 'Senate'
                    and p.vote_code is not null
                    --limit 10000")) %>%
  mutate(question = str_trim(question))

bills <- dbGetQuery(con, paste0("select b.*, s.name, s.url_name from bill.bills b
                    left join bill.subjects s
                    on s.bill_id = b.bill_id
                    where b.congress = ", current_congress, "
                    and b.original_chamber = 'senate'"))

cosp <- dbGetQuery(con, paste0("select c.*, b.sponsor_party, b.primary_subject, b.cosponsors
                   from bill.cosponsors c
                   left join bill.bills b
                   on c.bill_id = b.bill_id
                   --where b.congress > 100 and b.congress < 106 
                   where b.congress = ", current_congress, "
                   and b.original_chamber = 'senate'
                   --limit 10000"))

members <- dbGetQuery(con, paste0("select * from congress.member_roster where congress = ",
                                  current_congress)) %>%
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

member_matrix <- model.matrix(member_id ~ party_code,
                              data = pre_data %>%
                                count(member_id, party_code))

data <- list(
  N = nrow(pre_data),
  L = nrow(member_matrix),
  V = length(unique(pre_data$vote_id)),
  K = 2,
  legislator = as.numeric(factor(pre_data$member_id)),
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

bafumi <- stan(file = "../stan/bafumi.stan",
               data = data,
               iter = 2000,
               init = init_list,
               chains = n_chains)

summary <- rstan::summary(bafumi)$summary %>%
  as.data.frame() %>%
  mutate(params = rownames(.))

party_vector <- (pre_data %>%
                   count(name, party = party))$party

alpha <- summary %>%
  filter(grepl("alpha_adj", params)) %>%
  mutate(member_id = levels(factor(pre_data$member_id)),
         party = party_vector,
         congress = current_congress,
         chamber = "senate")


beta <- summary %>%
  filter(grepl("beta_adj", params)) %>%
  mutate(vote_id = levels(factor(pre_data$vote_id)),
         parameter = "beta",
         chamber = "senate") %>%
  separate(vote_id, into = c("congress", "session", "roll_call_number"),
           convert = TRUE)

gamma <- summary %>%
  filter(grepl("gamma_adj", params)) %>%
  mutate(vote_id = levels(factor(pre_data$vote_id)),
         parameter = "gamma",
         chamber = "senate") %>%
  separate(vote_id, into = c("congress", "session", "roll_call_number"),
           convert = TRUE)

vote_results <- rbind(beta,
                      gamma)


dbGetQuery(con, paste0("delete from ideology.bafumi_members where congress = ", current_congress))
dbWriteTable(conn = con,
             value = alpha,
             name = c("ideology", "bafumi_members"),
             row.names = FALSE,
             append = TRUE)

dbGetQuery(con, paste0("delete from ideology.bafumi_votes where congress = ", current_congress))
dbWriteTable(conn = con,
             value = vote_results,
             name = c("ideology", "bafumi_votes"),
             row.names = FALSE,
             append = TRUE)

dbDisconnect(con)



