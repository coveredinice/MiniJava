class Main {

	public boolean main(){
		
		String result;
		int cond;
		boolean bool1;
		boolean bool2;
		boolean res1;
		boolean res2;
		boolean res3;
		boolean res4;
		int fint;
		float ffloat;

		// If-else
		result = "Success";
		cond= 25;
		if (cond<10) {
			result= "Failure";
		} else {
			result = "Double success";
		}
		cond = 7;
		// If
		if (cond<10) {
			result= "Thrice the success";
		} 
		// While
		cond = 1;	
		while (cond<5) {
			cond = cond + 1;
		}

		// Boolean assignments
		bool1 = true;
		bool2 = false;
		// Boolean operations	
		res1 = bool1 && bool2;
		res2 = bool1 || bool2;
		res3 = !bool1;
		res4 = !bool2;

		// Mixed operations
		fint = 1+7;
		fint = 1/7;
		ffloat = 1.1+7;
		fint = 1-7;
		ffloat = 1.1-7;
		fint = 1*7;
		ffloat = 1.1 / 7;
		ffloat = 1.1 * 7;
		// Uncomment to get an error (division by zero)
		//ffloat = 1.1 / 0;
		// Uncomment to get an error (division by a string)
		//ffloat = 1.1 / result;

		// Should return true
		return res4;
	}
}