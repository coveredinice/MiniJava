class Europe {
	int hope;

	public float firstmethod(){
		float l;
		l = 0.75;
		return l;
	}

	public float secondmethod(int u) {
		float q;
		q = 0.5;
		q = q + u;
		return q;
	}
}

class France  {
	int baguette;
	String cheese;
	boolean paris;
}

class Main {

int a;
String b;
boolean c;
float d;
	
	public int  thirdmethod() {
		int tm;
		return 0;
	}

	public int main(){

		int [] arrayint;
		int singlevalue;
		London londonvar;
		String [] arraystring;
		int [] arrayint2;
		Europe [] arrayeurope;
		London [] londoneurope;
		England englandvalue;
		Europe europevalue;
		float methodonobject;
		float methodonobject2;
		int [] arrayofarrays;
		int [] arrayinarray;
		France franceobject;
		/* Milla is a cat, Panina is a dog
		a mismatch must be in place */
		int [] milla;
		float [] panina;
		England eng;
		float a;

		England eng2;
		London lond;

		// New London object and array of London objects
		londonvar = new London();
		londoneurope = new London[5];
		europevalue = new Europe();
		englandvalue = new England();
		franceobject = new France();
		
		// New array of int
		arrayint = new int[10];

		// New array of String
		arraystring = new String[5];

		// New array of int
		arrayint2 = new int[6];

		// Assignment between arrays (same type)
		arrayint = arrayint2;

		// Assignment between arrays (type mismatch)
		//arrayint = arraystring;

		// Assignment on array
		arrayint[5] = 7;
		singlevalue = arrayint[5];

		// New array of objects Europe
		arrayeurope = new Europe[5];
		// Uncomment to get an error
		//arrayeurope = new France();
		// populate location with a new object Europe
		arrayeurope[0] = new Europe();

		// Using a method without parameters
		methodonobject = englandvalue.firstmethod();

		// Using a method with parameters
		methodonobject2 = englandvalue.secondmethod(5);

		// Array of arrays
		arrayofarrays = new int[5];
		arrayofarrays[0] = new int[7];
		arrayinarray = new int[6];
		arrayinarray[1] = 5;
		arrayofarrays[0] = arrayinarray;
		
		// Uncomment to get an error	
		//franceobject = europevalue;

		// Arrays of different types
		milla = new int[5];
		panina = new float[3];
		//Uncomment to get an error
		//milla = panina;
		//europevalue = milla;
		//milla = europevalue;	

		// Inheritance example
		eng = new Europe();
		//eng2 = new London();
		lond = new England();
		// Uncomment to get an error
		//eng = new London();
		//europevalue = new London();

		// Uncomment to get an error
		//arrayint = new France();
		//arrayint[0] = new Europe(); 

		// Populate an array of objects with an object, with inheritance
		// an array of London can have London, England and Europe objects
		// an array of Europe can only have Europe objects
		londoneurope[1] = new England();
		arrayeurope[2] = new Europe();
		// Uncomment to get an error
		//arrayeurope[2] = new England();

		// Mismatching types, uncomment to get error
		// because France is a class on its own
		//londoneurope[3] = new France();

 		// Should return 5
 		// Write something else to return an error 
		return arrayofarrays[0][1];

	}
}

class England extends Europe {
	int rain;
}

class London extends England {
	int city;
}
