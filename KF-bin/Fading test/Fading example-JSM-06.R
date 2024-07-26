corrT<-function(T, Tf, ktc, Tc)
{
Tf-T*(1-ktc*(log(T/Tc)-1))
}

# Value with errors
# Samples collected 270 days before preparation & measurement 

Tf<-79756
 	# it should be in years
error<-24658	# as above
Tf_errorUB<-Tf+error
Tf_errorLB<-Tf-error


g<-3.37	# %/decade

tc<-2/365 	# tc=2 days
Tc<-270/365	# Tc=270 days
k<-g/100/log(10)
ktc<-k/(1-k*log(Tc/tc))

CorrecT<-uniroot(corrT, c(0.0001,100000),Tf, ktc, Tc)$root
UB_CorrecT<-uniroot(corrT, c(0.0001,100000),Tf_errorUB, ktc, Tc)$root
LB_CorrecT<-uniroot(corrT, c(0.0001,1000000),Tf_errorLB, ktc, Tc)$root

Error_c<-(UB_CorrecT-LB_CorrecT)/2

cat("Corrected Age = ", CorrecT, "+/-", Error_c , "\n")
