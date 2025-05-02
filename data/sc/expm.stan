data{
  //times
  real t1; real t2; real t3; real t4;
  //targets
  real m1; real m2; real m3; real m4;
  //target SDs
  real s1; real s2; real s3; real s4;
}
parameters{
  real<lower=0> gamma_a; // Self-clearance Year 1
  real<lower=0> gamma_b; // Self-clearance Year 2
  real<lower=0> gamma_c; // Self-clearance Year 3-9
  real<lower=0> gamma_d; // Self-clearance Year 10+
}
transformed parameters{
    //fixed parameters
    real kappa_ab = 1.0; // Transition Y1 -> Y2
    real kappa_bc = 1.0; // Transition Y2 -> Y3-9
    real kappa_cd = 1.0/8; // Transition Y3-9 -> Y10+
    //transition matrix
    matrix[5, 5] R = [[0,gamma_a,gamma_b,gamma_c,gamma_d], //S
                      [0,-gamma_a-kappa_ab,0,0,0],         //Ia
                      [0,kappa_ab,-gamma_b-kappa_bc,0,0],  //Ib
                      [0,0,kappa_bc,-gamma_c-kappa_cd,0],  //Ic
                      [0,0,0,kappa_cd,-gamma_d]];          //Id
    //full states
    vector[5] Y0 = [0,1,0,0,0]';
    vector[5] Y1 = matrix_exp(t1*R) * Y0;
    vector[5] Y2 = matrix_exp(t2*R) * Y0;
    vector[5] Y3 = matrix_exp(t3*R) * Y0;
    vector[5] Y4 = matrix_exp(t4*R) * Y0;
    //cleared
    real c1 = Y1[1];
    real c2 = Y2[1];
    real c3 = Y3[1];
    real c4 = Y4[1];
}
model{
  //priors
  gamma_a ~ exponential(1.0/1.26); // Self-clearance Year 1
  gamma_b ~ exponential(1.0/1.26); // Self-clearance Year 2
  gamma_c ~ exponential(1.0/0.50); // Self-clearance Year 3-9
  gamma_d ~ exponential(1.0/0.10); // Self-clearance Year 10+
  //data likelihood
  m1 ~ normal(c1,s1);
  m2 ~ normal(c2,s2);
  m3 ~ normal(c3,s3);
  m4 ~ normal(c4,s4);
}
