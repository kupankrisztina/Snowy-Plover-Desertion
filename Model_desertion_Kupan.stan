
data {                                             // data block
  int<lower=0> Nests_des;                          // nr of nests
  int<lower=0> last_des[Nests_des];            // day of last observation 
  int<lower=0> max_age_des;                        // maximum of last
  int<lower=0> y_des[Nests_des, max_age_des];     
  int<lower=0> NYears;                           // n levels
  int<lower=1, upper = NYears> Year[Nests_des];
  int<lower=0> NFemale;                           // n levels
  int<lower=1, upper = NFemale> Female_ID[Nests_des];
       real CBS_des[Nests_des, max_age_des];    // covariates
       real RHD_des[Nests_des];
       real CH_CI_max[Nests_des];
       real MTM_des[Nests_des];
       real FCI[Nests_des];
       real MCI[Nests_des];
       real BA_des[max_age_des]; 
       // real Manip[Nests_des];
       
       
}


parameters {              // parameter block for posteriori distr.
  vector[9] b;   
  // vector [10] b;
 real yeareff[NYears];
 real<lower=0> sigma_year;
 real femaleeff[NFemale];
 real<lower=0> sigma_female;
}

model {                                 // model block - likelihood
  real S[Nests_des, max_age_des-1];     // mu          
  for(i in 1:Nests_des){  
    for(t in 1:(last_des[i]-1)){ 
     S[i,t] = inv_logit(b[1] 
     + b[2]*CBS_des[i,t]
     + b[3]*RHD_des[i]
     + b[4]*CH_CI_max[i]
     + b[5]*MTM_des[i]
     + b[6]*MCI[i]
     + b[7]*FCI[i]
     + b[8]*BA_des[t]
     + b[9]*pow(BA_des[t], 2)
    // + b[10]*Manip[i]
    + sigma_year*yeareff[Year[i]]
    + sigma_female*femaleeff[Female_ID[i]]);
    }
  }

  // priors
  b[1]~normal(0,5);
  b[2]~normal(0,5);
  b[3]~normal(0,5);
  b[4]~normal(0,5);
  b[5]~normal(0,5);
  b[6]~normal(0,5);
  b[7]~normal(0,5);
  b[8]~normal(0,5);
  b[9]~normal(0,5);
 // b[10]~cauchy(0,5);
  sigma_year~cauchy(0,5);
  yeareff~normal(0,1);
  sigma_female~cauchy(0,5);  
  femaleeff ~ normal(0,1);   
  
  // likelihood
  for (i in 1:Nests_des) {   
    for(t in 2:last_des[i]){
      y_des[i,t]~bernoulli(y_des[i,t-1]*S[i,t-1]);
    }
  }
}
