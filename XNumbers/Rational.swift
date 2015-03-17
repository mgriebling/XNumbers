//
//  Rational.swift
//  XNumbers
//
//  Created by Mike Griebling on 10 Mar 2015.
//  Copyright (c) 2015 Computer Inspirations. All rights reserved.
//

import Foundation

func == (lhs: Rational, rhs: Rational) -> Bool {
	return lhs.Cmp(rhs) == 0
}

func > (lhs: Rational, rhs: Rational) -> Bool {
	return lhs.Cmp(rhs) == 1
}

func < (lhs: Rational, rhs: Rational) -> Bool {
	return lhs.Cmp(rhs) == -1
}

prefix func - (a: Rational) -> Rational {
	return a.Neg()
}

prefix func + (a: Rational) -> Rational {
	return a
}

struct Rational : Printable, Equatable, Comparable {

	/*
	
	Implements rational number operations with unlimited size.
	Copyright (c) 2010 - 2015 Michael Griebling
 
	This module is free software; you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation; either version 2 of the License, or
	(at your option) any later version.
 
	This module is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.
 
	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
	
	*/
	
	var num, den : Integer
	
	var description: String {
		return self.ToString()
	}
	
	func /* (a: Rational) */ ToString() -> String {
		return self.num.description + "/" + self.den.description
	} //Format;
	
	private func Normalize (var n: Integer, var _ d : Integer) -> (nout: Integer, dout: Integer) {
		var z: Integer
		
		/* normalize signs */
		if n.Sign() != d.Sign() {
			if d.Sign() < 0 { d = d.Abs(); n = -n }
		} else if  d.Sign() < 0 {
			d = d.Abs(); n = n.Abs()
		}
		
		/* reduce the fraction to its smallest rational denominator */
		z = n.GCD(d)
		return (n.Div(z), d.Div(z))
	} //Normalize;
	
	/*---------------------------------------------------------*/
	/* Constructors                                            */
	
	init (n: Integer, d : Integer) {
		self.num = Integer.zero; self.den = Integer.zero
		(self.num, self.den) = Normalize(n, d)
	} //Init;
	
	init (real : Real, int : Int) {
		self.init(n: ToInteger(real), d: Integer(fromInt: int))
	} //RInit;
	
	init (fromString str: String) {
		var pos: Int;
		let elements : [String] = str.componentsSeparatedByString("/")
		if elements.count <= 1 {
			let s = str.stringByTrimmingCharactersInSet(NSCharacterSet.whitespaceAndNewlineCharacterSet())
			self.init(n: Integer(fromString: s), d: Integer.one)
		} else {
			let rstr = elements[0].stringByTrimmingCharactersInSet(NSCharacterSet.whitespaceAndNewlineCharacterSet())
			let cstr = elements[1].stringByTrimmingCharactersInSet(NSCharacterSet.whitespaceAndNewlineCharacterSet())
			self.init(n: Integer(fromString: rstr), d: Integer(fromString: cstr))
		}
	} //New;
	
	init (n: Int, d : Int)  {
		self.init(n: Integer(fromInt: n), d:Integer(fromInt: d))
	} //NewRational;
	
	/*---------------------------------------------------------*/
	/* User functions                                          */
	
	func /* (a: Rational) */ Add (b: Rational) -> Rational {
		var z: Integer
		var a = self
		z = a.num.Mul(b.den)
		return Rational(n: z.Add(b.num.Mul(a.den)), d: a.den.Mul(b.den))
	} //Add;
	
	func /* (a: Rational) */ Sub(b: Rational) -> Rational {
		var z: Integer
		var a = self
		z = a.num.Mul(b.den)
		return Rational(n: z.Sub(b.num.Mul(a.den)), d: a.den.Mul(b.den))
	} //Sub;

	func /* (a: Rational) */ Mul(b: Rational) -> Rational {
		var a = self
		return Rational(n: a.num.Mul(b.num), d: a.den.Mul(b.den))
	} //Mul;
	
	func /* (a: Rational) */ Div(b: Rational) -> Rational {
		var a = self
		return Rational(n: a.num.Mul(b.den), d: a.den.Mul(b.num))
	} //Div;

////	func /* (a: Rational) */ Store(w: Storable.Writer) RAISES IO.Error;
////	
////	a.num.Store(w); a.den.Store(w);
////	a.Store^(w);    /* store parent parameters */
////	} //Store;
////	
////	func /* (a: Rational) */ Load(r: Storable.Reader) RAISES IO.Error;
////	
////	NEW(a.num); NEW(a.den);
////	a.num.Load(r); a.den.Load(r);
////	a.Load^(r);				/* read parent parameters */
////	} //Load;

	func /* (a: Rational) */ Equals (b: Rational) -> Bool {
		var a = self
		if a.num == b.num && a.den == b.den {
			return true
		} else {
			return false
		}
	} //Equals;
	
	func ToReal (var A : Integer) -> Real {
		let MAXC = 1000000000
		var iMAX, ia : Integer
		var x, y, MAX : Real
		var neg : Bool
		
		x = Real(0)
		MAX = Real(MAXC); iMAX = Integer(fromInt: MAXC)
		neg = false; y = Real(1)
		if A.Sign() == -1 { A = A.Abs(); neg = true }
		while A.Sign() == 1 {
			ia = A.Mod(iMAX)
			x = x.Add(y.Mul(Real(ia.ToInt())))
			A = A.Div(iMAX); y = y.Mul(MAX)
		}
		if neg { x = Real.zero.Sub(x) }
		return x
	} // IntegerToReal;
	
	func /* (a: Rational) */ IsZero() -> Bool {
		return self.num.IsZero()
	} //IsZero;
	
	func /* (a: Rational) */ NonZero() -> Bool {
		return !self.num.IsZero()
	} //NonZero;
	
	func /* (a: Rational) */ ToReal() -> Real {
		var ra = ToReal(self.num)
		// TBD - Compiler Bug? //
		return ra.Div(ToReal(self.den))
	} //ToReal;

	func /* (a: Rational) */ Sign() -> Int {
		if (self.num.IsZero()) {
			return 0
		} else {
			return self.num.Sign()
		}
	} //Sign;
	
	func /* (a: Rational) */ Cmp (b: Rational) -> Int {
		var ra, rb : Real
		var a = self
		ra = a.ToReal()
		rb = b.ToReal()
		return ra.Cmp(rb)
	} //Cmp;
	
	func /* (a: Rational) */ Abs() -> Rational {
		return Rational(n: self.num.Abs(), d: self.den)
	} //Abs;
	
	func /* (a: Rational) */ Neg() -> Rational {
		return Rational(n: -self.num, d: self.den)
	} //Neg;
	
	func Test () {
		var a, b : Rational
		
		a = Rational(n: 1, d: -2); b = Rational(n: -6, d: 16)
		println("\(a) + \(b) = \(a.Add(b))")
		println("\(a) - \(b) = \(a.Sub(b))")
		println("\(a) * \(b) = \(a.Mul(b))")
		println("\(a) / \(b) = \(a.Div(b))")
		println("Real(\(a)) = \(a.ToReal())")
		println("Real(\(b)) = \(b.ToReal())")
		println("Abs(\(a)) = \(a.Abs())")
		print(a)
		if a.Cmp(b) > 0 { print(" > ")
		} else if  a.Cmp(b) == 0 { print(" = ")
		} else { print(" < ")
		}
		println(b)
		
		a = Rational(fromString: "12345678901234567890 / -234567890")
		println("12345678901234567890 / -234567890 = \(a)")
	} //Test;

}
