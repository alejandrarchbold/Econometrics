clear all

set obs 10000
set seed 10101 //Semilla

generate u = rnormal(0,50)

generate x1 = rpoisson(11)

scalar b0 = 540.6
scalar b1 = 17.918

generate Y = b0 + b1*x1 + u
generate Yp = b0 + b1*x1 // media condicional causal

set matsize 5000

foreach i of numlist 1 5 10 50 {
	mat b `i' = J(500,2,.)
	
	forval j=1 (1)500 {
		preserve
		
		sample `i'
		qui reg y x1
		mat b `i' [`j',1] = _b[x1]
		mat b `i' [`j',2] = _se[x1]
		
		restore
		
	}
}

preserve 
clear


