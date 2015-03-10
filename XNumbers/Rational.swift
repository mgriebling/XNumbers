//
//  Rational.swift
//  XNumbers
//
//  Created by Mike Griebling on 10 Mar 2015.
//  Copyright (c) 2015 Computer Inspirations. All rights reserved.
//

import Foundation

struct Rational {

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
		(self.num, self.den) = Normalize(n, d)
	} //Init;
	
	init (real : Real, int : Int) {
		self.init(n: ToInteger(real), d: Integer(fromInt: int))
	} //RInit;
	
	init (str: String) {
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
	
	func /* (a: Rational) */ IsZero() -> Bool {
		return self.num.IsZero()
	} //IsZero;
	
	func /* (a: Rational) */ NonZero() -> Bool {
		return !self.num.IsZero()
	} //NonZero;
	
	func /* (a: Rational) */ ToReal() -> Real {
//		var ra = ToReal(self.num)
//		// TBD - Compiler Bug? //
//		return ra.Div(ToReal(self.den))
		var ra = ToReal(Integer(fromInteger: self.num))
		return Real.zero
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
	
//	func Test;
//	var
//	a, b : Rational;
//	
//	a = NewRational(1, -2); b = NewRational(-6, 16);
//	io.Object(a); io.String(" + "); io.Object(b); io.String(" = "); io.Object(a.Add(b)); io.Ln;
//	io.Object(a); io.String(" - "); io.Object(b); io.String(" = "); io.Object(a.Sub(b)); io.Ln;
//	io.Object(a); io.String(" * "); io.Object(b); io.String(" = "); io.Object(a.Mul(b)); io.Ln;
//	io.Object(a); io.String(" / "); io.Object(b); io.String(" = "); io.Object(a.Div(b)); io.Ln;
//	io.String("Real("); io.Object(a); io.String(") = "); io.Object(a.ToReal()); io.Ln;
//	io.String("Real("); io.Object(b); io.String(") = "); io.Object(b.ToReal()); io.Ln;
//	io.String("Abs("); io.Object(a); io.String(") = "); io.Object(a.Abs()); io.Ln;
//	io.Object(a);
//	if a.Cmp(b) > 0 { io.String(" > ");
//	} else if  a.Cmp(b) = 0 { io.String(" = ");
//	} else {io.String(" < ")
//	};
//	io.Object(b); io.Ln;
//	
//	a = New("12345678901234567890 / -234567890");
//	io.String("12345678901234567890 / -234567890 = "); io.Object(a); io.Ln;
//	} //Test;
	
}
