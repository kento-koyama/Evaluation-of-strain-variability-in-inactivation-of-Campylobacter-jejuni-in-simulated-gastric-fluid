#Install Rstan
#setwd("~appendix")
library(rstan)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

cam_allpH_data <-"
data {
	int N;
	int K;
	int H;
	real logNt[N];
	real t[N]; 
	real<lower=3.0, upper=3.8> pH[N];
	int<lower=1, upper=K> strain_index[N];
	int<lower=1, upper=H> ph_index[N];
	real Nu;
}	

parameters {
	real<lower=5.5, upper=7> logN0[K];
	real<lower=5.5, upper=7> mean_logN0;  
	real<lower=0> sigma_logN0;  
	real<lower=0, upper=1> sigma_logNt;
	matrix[K, 2] lndp[H];
	vector[2] mean_lndp[H];
	cholesky_factor_corr[2] corr_chol1;
	cholesky_factor_corr[2] corr_chol2;
	cholesky_factor_corr[2] corr_chol3;
	cholesky_factor_corr[2] corr_chol4;
	cholesky_factor_corr[2] corr_chol5;
	vector<lower=0>[2] sigma_vec[H];
}
transformed parameters{
	vector[K] d[H];
	vector[K] p[H];    
	cholesky_factor_cov[2] cov_chol1;
	cholesky_factor_cov[2] cov_chol2;
	cholesky_factor_cov[2] cov_chol3;
	cholesky_factor_cov[2] cov_chol4;
	cholesky_factor_cov[2] cov_chol5;
	cov_chol1 = diag_pre_multiply(sigma_vec[1],corr_chol1);
	cov_chol2 = diag_pre_multiply(sigma_vec[2],corr_chol2);
	cov_chol3 = diag_pre_multiply(sigma_vec[3],corr_chol3);
	cov_chol4 = diag_pre_multiply(sigma_vec[4],corr_chol4);
	cov_chol5 = diag_pre_multiply(sigma_vec[5],corr_chol5);
		
	for(h in 1:H){
		for(k in 1:K){
			d[h, k] = exp(lndp[h, k, 1]);
			p[h, k] = exp(lndp[h, k, 2]);
		}
	}
	
}
model {
	corr_chol1 ~ lkj_corr_cholesky(Nu);
    corr_chol2 ~ lkj_corr_cholesky(Nu);
    corr_chol3 ~ lkj_corr_cholesky(Nu);
    corr_chol4 ~ lkj_corr_cholesky(Nu);
    corr_chol5 ~ lkj_corr_cholesky(Nu);
	for(k in 1:K) {
    	logN0[k] ~ normal(mean_logN0, sigma_logN0);
	    lndp[1, k] ~ multi_normal_cholesky(mean_lndp[1], cov_chol1);
    	lndp[2, k] ~ multi_normal_cholesky(mean_lndp[2], cov_chol2);
	    lndp[3, k] ~ multi_normal_cholesky(mean_lndp[3], cov_chol3);
    	lndp[4, k] ~ multi_normal_cholesky(mean_lndp[4], cov_chol4);
	    lndp[5, k] ~ multi_normal_cholesky(mean_lndp[5], cov_chol5);
    }
	for(n in 1:N) {
    	logNt[n] ~ normal(logN0[strain_index[n]] -(t[n]/d[ph_index[n],strain_index[n]])^p[ph_index[n],strain_index[n]], sigma_logNt);
  	}
}
generated quantities {
  matrix[2,2] corr[H];
  matrix[2,2] cov[H];
  corr[1] = multiply_lower_tri_self_transpose(corr_chol1);
  corr[2] = multiply_lower_tri_self_transpose(corr_chol2);
  corr[3] = multiply_lower_tri_self_transpose(corr_chol3);
  corr[4] = multiply_lower_tri_self_transpose(corr_chol4);
  corr[5] = multiply_lower_tri_self_transpose(corr_chol5);
  cov[1] = multiply_lower_tri_self_transpose(cov_chol1);
  cov[2] = multiply_lower_tri_self_transpose(cov_chol2);
  cov[3] = multiply_lower_tri_self_transpose(cov_chol3);
  cov[4] = multiply_lower_tri_self_transpose(cov_chol4);
  cov[5] = multiply_lower_tri_self_transpose(cov_chol5);
}
"

#Load data
d <- read.csv("data/All_dataset0.csv")
d <- subset(d, d$pH != 3.3)
d <- subset(d, d$pH != 3.5)

PH_INDEX <- matrix(0, nrow = length(d$pH), ncol=1)   
for(n in 1:length(d$pH))  
{
  PH_INDEX[n,1] <- identical(d$pH[n],3.0)*1+identical(d$pH[n],3.2)*2+identical(d$pH[n],3.4)*3+identical(d$pH[n],3.6)*4+identical(d$pH[n],3.8)*5
}


#Convert data into list
listd <- list(t = as.numeric(d$time), 
              logNt = as.numeric(d$logN),
              strain_index = as.numeric(d$strain), 
              pH = as.numeric(d$pH), N=length(d$time), 
              ph_index = as.numeric(PH_INDEX),
              N = length(d$time),
              K = 11, 
              H = 5, 
              Nu = 1
              )

#MCMC_fitting
cam_allpH_stan <- stan( model_code = cam_allpH_data,
                        data = listd,
                        iter = 5000,
                        warmup = 2500,
                        chain = 4,                  
                        thin = 1,
                        seed=1234,
                        control=list(adapt_delta = 0.8, max_treedepth = 15)
                        )
#MCMC_output
ms <- rstan::extract(cam_allpH_stan)
saveRDS(ms, file = 'result_nonparametric.obj')