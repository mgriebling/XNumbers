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
		var str2 : String;
		
		pos = str.IndexOf("/", 0);
		if pos < 0 {
			return Init(XI.New(str.Trim(), 10), XI.one);
			else
			str2 = str.Substring(pos+1, str.length);
			str = str.Substring(0, pos);
			return Init(XI.New(str.Trim(), 10), XI.New(str2.Trim(), 10));
		}
	} //New;
	
	init (n: Int, d : Int)  {
		self.init(n: Integer(fromInt: n), d:Integer(fromInt: d))
	} //NewRational;
	
//	func /* (a: Rational) */ Add(b: Complex) -> Rational {
//	var
//	z: Integer;
//	
//	/* Ok, you caught me, this isn't mathematically correct because
//	Rational numbers aren't a superset of Complex numbers but it
//	lets me support Rational numbers fairly compactly with
//	minimal disruption. */
//	
//	z = a.num.Mul(b.den);
//	return Init(z.Add(b.num.Mul(a.den)), a.den.Mul(b.den));
//	else
//	return a.Add(RInit(b.real, 1))
//	};
//	} //Add;
//	
//	func /* (a: Rational) */ Sub(b: Complex) -> Rational {
//	var
//	z: Integer;
//	
//	
//	z = a.num.Mul(b.den);
//	return Init(z.Sub(b.num.Mul(a.den)), a.den.Mul(b.den));
//	else
//	return a.Sub(RInit(b.real, 1))
//	};
//	} //Sub;
//	
//	func /* (a: Rational) */ Mul(b: Complex) -> Rational {
//	
//	
//	return Init(a.num.Mul(b.num), a.den.Mul(b.den));
//	else
//	return a.Mul(RInit(b.real, 1))
//	};
//	} //Mul;
//	
//	func /* (a: Rational) */ Div(b: Complex) -> Rational {
//	
//	
//	return Init(a.num.Mul(b.den), a.den.Mul(b.num));
//	else
//	return a.Div(RInit(b.real, 1))
//	};
//	} //Div;
//	
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
//
//	func /* (a: Rational) */ Equals(b: Object.Object) -> Bool {
//	
//	
//	if a.num.Equals(b.num) & a.den.Equals(b.den) {
//	return TRUE;
//	else
//	return FALSE;
//	};
//	else
//	return FALSE;
//	};
//	} //Equals;
//	
//	func /* (a: Rational) */ IsZero() -> Bool {
//	
//	return a.num.IsZero();
//	} //IsZero;
//	
//	func /* (a: Rational) */ NonZero() -> Bool {
//	
//	return ~a.num.IsZero();
//	} //NonZero;
//	
//	func /* (a: Rational) */ ToReal() -> Real;
//	var
//	ra : Real;
//	
//	ra = C.IntegerToReal(a.num);
//	return ra.Div(C.IntegerToReal(a.den));
//	} //ToReal;
//	
//func /* (a: Rational) */ Sign() -> Int {
//	
//	if (a.num.IsZero()) { return 0;
//	} else {return a.num.Sign();
//	};
//	} //Sign;
//	
//func /* (a: Rational) */ Cmp(b: Object.Object) -> Int {
//	var
//	ra, rb : Real;
//	
//	
//	ra = a.ToReal();
//	rb = b.ToReal();
//	return ra.Cmp(rb);
//	};
//	} //Cmp;
//	
//	func /* (a: Rational) */ Abs() -> Rational {
//	
//	return Init(a.num.Abs(), a.den);
//	} //Abs;
//	
//	func /* (a: Rational) */ Neg() -> Rational {
//	
//	return Init(a.num.Neg(), a.den);
//	} //Neg;
//	
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
