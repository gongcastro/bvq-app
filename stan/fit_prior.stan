// generated with brms 2.20.4
functions {
  /* compute correlated group-level effects
   * Args:
   *   z: matrix of unscaled group-level effects
   *   SD: vector of standard deviation parameters
   *   L: cholesky factor correlation matrix
   * Returns:
   *   matrix of scaled group-level effects
   */
  matrix scale_r_cor(matrix z, vector SD, matrix L) {
    // r is stored in another dimension order than z
    return transpose(diag_pre_multiply(SD, L) * z);
  }
  /* cumulative-logit log-PDF for a single response
   * Args:
   *   y: response category
   *   mu: latent mean parameter
   *   disc: discrimination parameter
   *   thres: ordinal thresholds
   * Returns:
   *   a scalar to be added to the log posterior
   */
  real cumulative_logit_lpmf(int y, real mu, real disc, vector thres) {
    int nthres = num_elements(thres);
    if (y == 1) {
      return log_inv_logit(disc * (thres[1] - mu));
    } else if (y == nthres + 1) {
      return log1m_inv_logit(disc * (thres[nthres] - mu));
    } else {
      return log_diff_exp(log_inv_logit(disc * (thres[y] - mu)),
                          log_inv_logit(disc * (thres[y - 1] - mu)));
    }
  }
  /* cumulative-logit log-PDF for a single response and merged thresholds
   * Args:
   *   y: response category
   *   mu: latent mean parameter
   *   disc: discrimination parameter
   *   thres: vector of merged ordinal thresholds
   *   j: start and end index for the applid threshold within 'thres'
   * Returns:
   *   a scalar to be added to the log posterior
   */
  real cumulative_logit_merged_lpmf(int y, real mu, real disc, vector thres,
                                    array[] int j) {
    return cumulative_logit_lpmf(y | mu, disc, thres[j[1] : j[2]]);
  }
  /* ordered-logistic log-PDF for a single response and merged thresholds
   * Args:
   *   y: response category
   *   mu: latent mean parameter
   *   thres: vector of merged ordinal thresholds
   *   j: start and end index for the applid threshold within 'thres'
   * Returns:
   *   a scalar to be added to the log posterior
   */
  real ordered_logistic_merged_lpmf(int y, real mu, vector thres,
                                    array[] int j) {
    return ordered_logistic_lpmf(y | mu, thres[j[1] : j[2]]);
  }
}
data {
  int<lower=1> N; // total number of observations
  array[N] int Y; // response variable
  int<lower=2> nthres; // number of thresholds
  int<lower=1> K; // number of population-level effects
  matrix[N, K] X; // population-level design matrix
  int<lower=1> Kc; // number of population-level effects after centering
  // data for group-level effects of ID 1
  int<lower=1> N_1; // number of grouping levels
  int<lower=1> M_1; // number of coefficients per level
  array[N] int<lower=1> J_1; // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_1;
  vector[N] Z_1_2;
  vector[N] Z_1_3;
  int<lower=1> NC_1; // number of group-level correlations
  // data for group-level effects of ID 2
  int<lower=1> N_2; // number of grouping levels
  int<lower=1> M_2; // number of coefficients per level
  array[N] int<lower=1> J_2; // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_2_1;
  vector[N] Z_2_2;
  vector[N] Z_2_3;
  vector[N] Z_2_4;
  vector[N] Z_2_5;
  int<lower=1> NC_2; // number of group-level correlations
  int prior_only; // should the likelihood be ignored?
}
transformed data {
  matrix[N, Kc] Xc; // centered version of X
  vector[Kc] means_X; // column means of X before centering
  for (i in 1 : K) {
    means_X[i] = mean(X[ : , i]);
    Xc[ : , i] = X[ : , i] - means_X[i];
  }
}
parameters {
  vector[Kc] b; // regression coefficients
  ordered[nthres] Intercept; // temporary thresholds for centered predictors
  vector<lower=0>[M_1] sd_1; // group-level standard deviations
  matrix[M_1, N_1] z_1; // standardized group-level effects
  cholesky_factor_corr[M_1] L_1; // cholesky factor of correlation matrix
  vector<lower=0>[M_2] sd_2; // group-level standard deviations
  matrix[M_2, N_2] z_2; // standardized group-level effects
  cholesky_factor_corr[M_2] L_2; // cholesky factor of correlation matrix
}
transformed parameters {
  real disc = 1; // discrimination parameters
  matrix[N_1, M_1] r_1; // actual group-level effects
  // using vectors speeds up indexing in loops
  vector[N_1] r_1_1;
  vector[N_1] r_1_2;
  vector[N_1] r_1_3;
  matrix[N_2, M_2] r_2; // actual group-level effects
  // using vectors speeds up indexing in loops
  vector[N_2] r_2_1;
  vector[N_2] r_2_2;
  vector[N_2] r_2_3;
  vector[N_2] r_2_4;
  vector[N_2] r_2_5;
  real lprior = 0; // prior contributions to the log posterior
  // compute actual group-level effects
  r_1 = scale_r_cor(z_1, sd_1, L_1);
  r_1_1 = r_1[ : , 1];
  r_1_2 = r_1[ : , 2];
  r_1_3 = r_1[ : , 3];
  // compute actual group-level effects
  r_2 = scale_r_cor(z_2, sd_2, L_2);
  r_2_1 = r_2[ : , 1];
  r_2_2 = r_2[ : , 2];
  r_2_3 = r_2[ : , 3];
  r_2_4 = r_2[ : , 4];
  r_2_5 = r_2[ : , 5];
  lprior += normal_lpdf(b[1] | 1, 0.25);
  lprior += normal_lpdf(b[2] | 0, 0.25);
  lprior += normal_lpdf(b[3] | 0, 0.25);
  lprior += normal_lpdf(b[4] | 0, 0.25);
  lprior += normal_lpdf(Intercept | -0.25, 0.1);
  lprior += normal_lpdf(sd_1 | 1, 0.1) - 3 * normal_lccdf(0 | 1, 0.1);
  lprior += lkj_corr_cholesky_lpdf(L_1 | 2);
  lprior += normal_lpdf(sd_2 | 1, 0.1) - 5 * normal_lccdf(0 | 1, 0.1);
  lprior += lkj_corr_cholesky_lpdf(L_2 | 2);
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = rep_vector(0.0, N);
    mu += Xc * b;
    for (n in 1 : N) {
      // add more terms to the linear predictor
      mu[n] += r_1_1[J_1[n]] * Z_1_1[n] + r_1_2[J_1[n]] * Z_1_2[n]
               + r_1_3[J_1[n]] * Z_1_3[n] + r_2_1[J_2[n]] * Z_2_1[n]
               + r_2_2[J_2[n]] * Z_2_2[n] + r_2_3[J_2[n]] * Z_2_3[n]
               + r_2_4[J_2[n]] * Z_2_4[n] + r_2_5[J_2[n]] * Z_2_5[n];
    }
    for (n in 1 : N) {
      target += ordered_logistic_lpmf(Y[n] | mu[n], Intercept);
    }
  }
  // priors including constants
  target += lprior;
  target += std_normal_lpdf(to_vector(z_1));
  target += std_normal_lpdf(to_vector(z_2));
}
generated quantities {
  // compute actual thresholds
  vector[nthres] b_Intercept = Intercept + dot_product(means_X, b);
  // compute group-level correlations
  corr_matrix[M_1] Cor_1 = multiply_lower_tri_self_transpose(L_1);
  vector<lower=-1, upper=1>[NC_1] cor_1;
  // compute group-level correlations
  corr_matrix[M_2] Cor_2 = multiply_lower_tri_self_transpose(L_2);
  vector<lower=-1, upper=1>[NC_2] cor_2;
  // extract upper diagonal of correlation matrix
  for (k in 1 : M_1) {
    for (j in 1 : (k - 1)) {
      cor_1[choose(k - 1, 2) + j] = Cor_1[j, k];
    }
  }
  // extract upper diagonal of correlation matrix
  for (k in 1 : M_2) {
    for (j in 1 : (k - 1)) {
      cor_2[choose(k - 1, 2) + j] = Cor_2[j, k];
    }
  }
}

