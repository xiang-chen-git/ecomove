		data {
			int<lower=0> N; 			// length of data set
			vector[2*N] XY; 				// locations in X followed by locations in Y
			vector[N] T; 					// times of observations
			real v0x;    // initial velocity x
			real v0y;    // initial velocity y
      real<lower=0> tau0;
      real<lower=0> nu0;
		}
	
		parameters {
			real<lower=0> tau; 
			real<lower=0> nu;
			//real<lower=0> v0;
		}
		
		transformed parameters {
		// declare Mu, Sigma, chol_Sigma
			vector[2*N] MuXY;
			matrix[N,N] Sigma;
			//matrix[N,N] chol_Sigma;
			matrix[2*N,2*N] SigmaXY;

			
		// intermediate variables
			real eps1;
			real kappa;
			real Var_Z;
			real Cov_VZ;
					
		
		// define Sigma (upper diagonal) and Mu
		
    for (i in 1:N) 
		for (j in i:N)
		{
			MuXY[i] <- v0x*tau*(1.0-exp(-T[i]/tau));
			MuXY[i+N] <- v0y*tau*(1.0-exp(-T[i]/tau));
			
			eps1 <- exp(-T[i]/tau);
			kappa <- exp(-(T[j] - T[i])/tau);
			Var_Z <- 4.0 * square(nu) * square(tau)/pi() * (T[i]/tau - 2.0 * (1.0 - eps1) + (1.0 - square(eps1))/2.0) ;
			Cov_VZ <- 2.0 * square(nu) * (tau/pi()) * square(1.0 - eps1);
			Sigma[i,j] <- Var_Z + tau * (1.0 - kappa) * Cov_VZ;
		}		
		
		// fill in lower diagonal of Sigma
		for(i in 2:N)
		for(j in 1:(i-1))
			Sigma[i,j] <- Sigma[j,i];
	
		// fill in SigmaXY
		for(i in 1:N)
		for(j in 1:N)
		{
			SigmaXY[i,j] <- Sigma[i,j];
			SigmaXY[i+N,j+N] <- Sigma[i,j];
			SigmaXY[i+N,j] <- 0;
			SigmaXY[i,j+N] <- 0;
		}
		
		// define chol_Sigma
		//	chol_Sigma <- cholesky_decompose(Sigma);
		}

		model {
		// priors
			tau ~ lognormal(log(tau0),1);
			nu ~ lognormal(log(nu0),1);
		//	v0 ~ lognormal(1,1);
		  XY ~ multi_normal(MuXY, SigmaXY);
		} 