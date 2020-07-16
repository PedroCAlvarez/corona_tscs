// Coronavirus tracking model 
// Robert Kubinec and Luiz Carvalho
// New York University Abu Dhabi & Vertulio Vargas Foundation
// July 15, 2020
data {
    int time_all;
    int num_country;
    int cc[num_country*time_all]; // country counter
    int cases[num_country*time_all];
    int tests[num_country*time_all];
    int time_outbreak[num_country,time_all];
    int S; // number of suppression measures
    int G; // google mobility data (by type of mobility)
    int L; // just lockdown data (for hierarchical predictor)
    int R; // number of seroprevalence essays
    int maxO; // maximum number of post-outbreak time points (could be greater than T due to left-censoring)
    matrix[maxO,3] ortho_time;
    matrix[num_country*time_all,S] suppress; // time-varying suppression measures
    matrix[num_country*time_all,G] mobility; // time-varying mobility measures
    matrix[num_country*time_all,L] lockdown; // hierachical lockdown predictors
    vector[time_all*num_country] count_outbreak;
    vector[time_all*num_country] cases_per_cap;
    int sero_time[R]; // counters for which state/time points have CDC sero surveys
    int sero_country[R];
    matrix[R,6] sero; // sero-prevalence datas
    int country_pop[num_country*time_all];
    real phi_scale; // prior on how much change there could be in infection rate over time
}
transformed data {
  matrix[num_country*time_all,3] time_outbreak_trans; // convert raw time numbers to ortho-normal polynomials
  vector[num_country*time_all] test_max;
  vector[num_country*time_all] cases_per_capita; // need for prior adjustment
  
  matrix[num_country*time_all, S] Q_supp;
  matrix[S, S] R_supp;
  matrix[S, S] R_supp_inverse;
  
  matrix[num_country*time_all, G] Q_mob;
  matrix[G, G] R_mob;
  matrix[G, G] R_mob_inverse;
  
  matrix[num_country*time_all, L] Q_lock;
  matrix[L, L] R_lock;
  matrix[L, L] R_lock_inverse;
  
  // thin and scale the QR decomposition
  Q_supp = qr_Q(suppress)[, 1:S] * sqrt(num_country*time_all - 1);
  R_supp = qr_R(suppress)[1:S, ] / sqrt(num_country*time_all - 1);
  R_supp_inverse = inverse(R_supp);
  
  Q_mob = qr_Q(mobility)[, 1:G] * sqrt(num_country*time_all - 1);
  R_mob = qr_R(mobility)[1:G, ] / sqrt(num_country*time_all - 1);
  R_mob_inverse = inverse(R_mob);
  
  Q_lock = qr_Q(lockdown)[, 1:L] * sqrt(num_country*time_all - 1);
  R_lock = qr_R(lockdown)[1:L, ] / sqrt(num_country*time_all - 1);
  R_lock_inverse = inverse(R_lock);
  
  // make some arrays of counts of the outbreak
  
  for(t in 1:time_all) {
    for(n in 1:num_country) {
      if(time_outbreak[n,t]>0) {
        time_outbreak_trans[(t-1)*num_country + n,1] = ortho_time[time_outbreak[n,t],1];
        time_outbreak_trans[(t-1)*num_country + n,2] = ortho_time[time_outbreak[n,t],2];
        time_outbreak_trans[(t-1)*num_country + n,3] = ortho_time[time_outbreak[n,t],3];
      } else {
        time_outbreak_trans[(t-1)*num_country + n,1] = 0;
        time_outbreak_trans[(t-1)*num_country + n,2] = 0;
        time_outbreak_trans[(t-1)*num_country + n,3] = 0;
      }
      if(t==1) {
        test_max[(t-1)*num_country + n] = tests[(t-1)*num_country + n];
      } else {
        if(test_max[(t-2)*num_country + n]>tests[(t-1)*num_country + n]) {
          test_max[(t-1)*num_country + n] = test_max[(t-2)*num_country + n];
        } else {
          test_max[(t-1)*num_country + n] = tests[(t-1)*num_country + n];
        }
      }
    }
  }
  
  // standardized time max
  
  for(n in 1:num_country) {
    test_max = test_max ./ to_vector(country_pop);
  }
  
}
parameters {
  vector[num_country] poly1; // polinomial function of time
  vector[num_country] poly2; // polinomial function of time
  vector[num_country] poly3; // polinomial function of time
  real<lower=0> finding; // difficulty of identifying infected cases 
  //vector<lower=0,upper=1>[R] survey_prop; // variable that stores survey proportions from CDC data
  real<lower=0> world_infect; // infection rate based on number of travelers
  vector[S] suppress_effect_raw; // suppression effect of govt. measures, cannot increase virus transmission rate
  vector[L] lockdown_effect_raw;
  vector[L] suppress_hier_const[G];
  vector[3] mu_poly; // hierarchical mean for poly coefficients
  vector[G] mob_effect_raw;
  real<lower=0> test_max_par;
  vector<lower=0>[3] sigma_poly; // varying sigma polys
  vector[G] mob_alpha_const; // mobility hierarchical intercepts
  vector<lower=0>[G] sigma_med;
  vector<lower=0>[num_country] country_test_raw; // unobserved rate at which countries are willing to test vs. number of infected
  // we assume that as infection rates increase, more tests will be conducted
  vector[3] alpha; // other intercepts
  vector<lower=0>[2] phi; // shape parameter for infected
  real<lower=0> sigma_test_raw; // estimate of between-state testing heterogeneity
}
transformed parameters {

  vector[num_country*time_all] prop_infected; // modeled infection rates for domestic transmission
  vector[num_country] poly_nonc1; // non-centered poly parameters
  vector[num_country] poly_nonc2; // non-centered poly parameters
  vector[num_country] poly_nonc3; // non-centered poly parameters
  real<lower=0> sigma_test; 
  
  sigma_test = .1 * sigma_test_raw;
  
  // non-centering of polynomial time trends
  
  poly_nonc1 = mu_poly[1] + sigma_poly[1]*poly1;
  poly_nonc2 = mu_poly[2] + sigma_poly[2]*poly2;
  poly_nonc3 = mu_poly[3] + sigma_poly[3]*poly3;

  // latent infection rate (unobserved), on the logit scale (untransformed)
  prop_infected = alpha[2] + time_outbreak_trans[,1] .* poly_nonc1[cc]  +
                  time_outbreak_trans[,2] .* poly_nonc2[cc] +
                  time_outbreak_trans[,3] .* poly_nonc3[cc] +
                  //world_infect*count_outbreak +
                  Q_supp*suppress_effect_raw +
                  Q_lock*lockdown_effect_raw +
                  Q_mob*mob_effect_raw;

}
model {
  

  poly1 ~ normal(0,1);
  poly2 ~ normal(0,1);
  poly3 ~ normal(0,1);

  
  sigma_poly ~ exponential(.1);
  mu_poly ~ normal(0,10);
  world_infect ~ normal(0,3);
  lockdown_effect_raw ~ normal(0,5);
  alpha ~ normal(0,10); // this can reach extremely low values
  
  phi ~ exponential(phi_scale);
  mob_effect_raw ~ normal(0,5);
  suppress_effect_raw ~ normal(0,5);
  test_max_par ~ normal(0,5);
  
  for(g in 1:G) {
    suppress_hier_const[g] ~ normal(0,5);
  }
  
   mob_alpha_const ~ normal(0,5);
    
  
  finding ~ exponential(.1);
  sigma_test_raw ~ exponential(.1);
  sigma_med ~ exponential(.1);
  country_test_raw ~ exponential(sigma_test_raw); // more likely near the middle than the ends
  
  for(g in 1:G)
    to_vector(mobility[,g]) ~ normal(mob_alpha_const[g] + lockdown*suppress_hier_const[g],sigma_med[g]);
    
  //next model the true infection rate as a function of time since outbreak

    {
    // locations for cases and tests

    vector[num_country*time_all] mu_cases = inv_logit(alpha[3] + finding*prop_infected);
    vector[num_country*time_all] mu_tests = inv_logit(alpha[1] + country_test_raw[cc].* prop_infected +
                                              test_max_par*test_max);
    vector[num_country*time_all] mix_prop = inv_logit(prop_infected);
    
    tests ~ beta_binomial(country_pop,mu_tests*phi[1],(1-mu_tests) * phi[1]);
    cases ~ beta_binomial(tests,mu_cases *phi[2],(1-mu_cases) * phi[2]);
    
    // loop over serology surveys to add informative prior information
    for(r in 1:R) {
          int n = sero_country[r];
          int t = sero_time[r];
          real cum_infect = mix_prop[((n-1)*time_all + t)];
          
          // Beta prior = uncertainty proportional to sero essay sample size at time point t
          cum_infect ~ beta(.5 + sero[r,1].*sero[r,3],.5 + sero[r,3] - (sero[r,3].*sero[r,1]));
          
          // jacobian adjustment
          
          target += log(cum_infect) + log1m(cum_infect);     
    }
  
    }
    
}

generated quantities {
  
  // convert QR estimates back to actual numbers
  
  vector[S] suppress_effect; // suppression effect of govt. measures, cannot increase virus transmission rate
  vector[L] lockdown_effect;
  vector[G] mob_effect;
  
  suppress_effect = R_supp_inverse * suppress_effect_raw;
  lockdown_effect = R_lock_inverse * lockdown_effect_raw;
  mob_effect = R_mob_inverse * mob_effect_raw;
  
}

