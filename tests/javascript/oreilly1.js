// This example is from the book _JavaScript: The Definitive Guide_.    
// Written by David Flanagan.  Copyright (c) 1996 O'Reilly & Associates.
// This example is provided WITHOUT WARRANTY either expressed or implied.
// You may study, use, modify, and distribute it for any purpose.        

function Circle(radius) {   // the constructor defines the class itself
    // r is an instance variable; defined and initialized in the constructor
    this.r = radius;    
}

// Circle.PI is a class variable--it is a property of the constructor function
Circle.PI = 3.14159;

// Here is a function that computes a circle area.
function Circle_area() { return Circle.PI * this.r * this.r; }

// Here we make the function into an instance method by assigning it
// to the prototype object of the constructor.  Remember that we have to
// create and discard one object before the prototype object exists
new Circle(0);
Circle.prototype.area = Circle_area;

// Here's another function.  It takes two circle objects are arguments and
// returns the one that is larger (has the larger radius).  
function Circle_max(a,b) {
    if (a.r > b.r) return a;
    else return b;
}

// Since this function compares two circle objects, it doesn't make sense as 
// an instance method operating on a single circle object.  But we don't want
// it to be a stand-alone function either, so we make it into a class method
// by assigning it to the constructor function:
Circle.max = Circle_max;

// Here is some code that uses each of these fields:
c = new Circle(1.0);       // create an instance of the Circle class
c.r = 2.2;                 // set the r instance variable
a = c.area();              // invoke the area() instance method 
x = Math.exp(Circle.PI);   // use the PI class variable in our own computation.
d = new Circle(1.2);       // create another Circle instance
bigger = Circle.max(c,d);  // use the max() class method.
