
// model from Bafumi, et al. 2004

data {
  int N; // num obs
  int L; // num legislators
  int V; // num votes
  int K; // num legislator predictors
  int legislator[N]; // legislator index
  int vote[N]; // vote index
  matrix[L,K] X_alpha; // matrix of legislator predictors
  int outcome[N]; // decision on each vote
}
parameters {
  // vector[K] delta_alpha;
  vector[1] delta_alpha_1;
  vector[1] delta_alpha_2;
  
  vector[K] sigma_alpha;
  
  vector[L] alpha;
  vector[V] beta;
  vector[V] gamma;
}
transformed parameters {
  vector[K] delta_alpha;
  
  vector[L] alpha_adj;
  vector[V] beta_adj;
  vector[V] gamma_adj;
  
  delta_alpha = append_row(delta_alpha_1, exp(delta_alpha_2));
  
  alpha_adj = (alpha - mean(alpha)) / sd(alpha);
  beta_adj = (beta - mean(alpha)) / sd(alpha);
  gamma_adj = gamma * sd(alpha);
}
model {
  // delta_alpha[1] ~ normal(0,1);
  // delta_alpha[2] ~ normal(1,1);
  delta_alpha_1 ~ normal(0,1);
  delta_alpha_2 ~ normal(0,1);
  
  sigma_alpha ~ normal(0,1);
  
  alpha ~ normal(X_alpha*delta_alpha, exp(X_alpha*sigma_alpha)); // heteroskedastic normal
  beta ~ normal(0,10);
  gamma ~ normal(0,10);
  
  outcome ~ bernoulli_logit(gamma_adj[vote] .* (alpha_adj[legislator] - beta_adj[vote]));
}
