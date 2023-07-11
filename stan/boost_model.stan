functions {

// // Function to estimate the boosting dynamics
  real titre_boost(real t_e,
                   real grad_boost,
                   real t) {

    // mu is the expected titre at time t, given the current parameters
    real mu;

    // if time is less than the timing of the peak parameter, to determine whether the individual is
    // boosting or waning
    mu = t_e + grad_boost*t;

    // returning only positive values or zeros
    mu = fmax(mu, 0);

    return(mu);
  }
}

data {
    int N; // number of observations
    int N_ind; // number of individuals
    array[N] int ids; // ids of each individual
    vector[N] t; // timing of bleed
    vector[N] y;  // titre value at bleed
}

parameters {

  //--- parameters for the boost/wane model
  // population-level parameters
  real t_e; // initial titre value
  real<lower = 0> grad_boost; // gradient of boost parameter

  vector[N_ind] z_t_e;
  vector[N_ind] z_grad_boost_ind;

  //--- measurement error parameters
  real<lower = 0> sigma_t_e;
  real<lower = 0> sigma_grad_boost;

  #--- shared measurement parameter
  real<lower = 0> sigma;
}

transformed parameters{

  // using a non-centered parameterisation, i.e. individual-level parameters
  // given by: p_ind = p_pop + std_normal*sigma_pop
  vector[N_ind] t_e_ind;
  vector[N_ind] grad_boost_ind;

  t_e_ind = t_e + z_t_e*sigma_t_e;
  grad_boost_ind = grad_boost + z_grad_boost_ind*sigma_grad_boost;

  vector[N] titre_est;

  // Get the estimated titre landscape given the parameters
  for (i in 1:N) {

      titre_est[i] = titre_boost(t_e_ind[ids[i]],
                                 grad_boost_ind[ids[i]],
                                 t[i]);
  }
}

model {
  // likelihood
  for (i in 1:N) {
    if (y[i] == 11) {
      target += normal_lccdf(11 | titre_est[i], sigma); // if titre value is above upper censoring threshold
    } else if (y[i] == 0 ) {
      target += normal_lcdf(0 | titre_est[i], sigma); // if titre value is below lower censoring threshold
    } else {
      y[i] ~ normal(titre_est[i], sigma); // normally distributed likelihood function
    // }
    }
  }

  //--- priors
  // population-level priors
  t_e_ind ~ normal(3.5, 2.5);
  grad_boost_ind ~ normal(0, 0.2);

  // non-centered parameterisation
  z_t_e ~ std_normal();
  z_grad_boost_ind ~ std_normal();

  sigma_t_e ~ normal(0, 1);
  sigma_grad_boost ~ normal(0, 0.25);

  sigma ~ normal(0, 2) T[0, ];
}
