//
//  Complex.swift
//  XNumbers
//
//  Created by Mike Griebling on 5 Mar 2015.
//  Copyright (c) 2015 Computer Inspirations. All rights reserved.
//

import Foundation

func == (lhs: Complex, rhs: Complex) -> Bool {
	return lhs.Cmp(rhs) == 0
}

func > (lhs: Complex, rhs: Complex) -> Bool {
	return lhs.Cmp(rhs) == 1
}

func < (lhs: Complex, rhs: Complex) -> Bool {
	return lhs.Cmp(rhs) == -1
}

prefix func - (a: Complex) -> Complex {
	return Complex.zero.Sub(a)
}

prefix func + (a: Complex) -> Complex {
	return a
}

func ToInteger (var A : Real) -> Integer {
	let MAXC = 0x40000000
	var x, iMAX : Integer
	var a, MAX : Real
	var exp : Int
	var neg : Bool
	
	A = A.Entier()
	x = Integer.zero; exp = 0
	MAX = Real(fromInt: MAXC); iMAX = Integer(fromInt: MAXC)
	neg = false;
	if A.isNegative() { A = A.Abs(); neg = true }
	while A > MAX {
		A = A.Div(MAX)
		++exp
	}
	do {
		a = A.Entier(); x = x.Mul(iMAX)
		x = x.Add(Integer(fromInt: Int(a.Short())))
		A = MAX.Mul(A.Fraction())
		--exp
	} while !(exp < 0)
	if neg { x = Integer.zero.Sub(x) }
	return x
} // RealToInteger;

func ToReal (var A : Integer) -> Real {
	let MAXC = 1000000000
	var iMAX, ia : Integer
	var x, y, MAX : Real
	var neg : Bool
	
	x = Real(fromInt: 0)
	MAX = Real(fromInt: MAXC); iMAX = Integer(fromInt: MAXC)
	neg = false; y = Real(fromInt: 1);
	if A.Sign() == -1 { A = A.Abs(); neg = true }
	while A.Sign() == 1 {
		ia = A.Mod(iMAX)
		x = x.Add(y.Mul(Real(fromInt: ia.ToInt())))
		A = A.Div(iMAX); y = y.Mul(MAX)
	}
	if neg { x = Real.zero.Sub(x) }
	return x
} // IntegerToReal;


func FromRadians (radianAngle: Real) -> Real {
	/* conversion from radians to an angular measure */
	if Complex.nState.DegRadFlag == Complex.AngularMeasure.Degrees {
		let K180 = Real(fromInt: 180)
		return radianAngle.Mul(K180.Div(Real.pi))
	} else if Complex.nState.DegRadFlag == Complex.AngularMeasure.Gradians {
		let K200 = Real(fromInt: 200)
		return radianAngle.Mul(K200.Div(Real.pi))
	} else {
		return radianAngle
	}
} // fromRadians;


func ToRadians (radianAngle : Real) -> Real {
	/* Convert an angular measure into radians */
	if Complex.nState.DegRadFlag == Complex.AngularMeasure.Degrees {
		return radianAngle.Mul(Real.pi.Div(Real(fromInt: 180)))
	} else if Complex.nState.DegRadFlag == Complex.AngularMeasure.Gradians {
		return radianAngle.Mul(Real.pi.Div(Real(fromInt: 200)))
	} else {
		return radianAngle
	}
} // toRadians;


func Long (x : Double) -> Complex {
	return Complex(re: Real(fromDouble: x), im: Real.zero)
} // Long;

struct Complex : Printable, Equatable, Comparable {
	
	/*
	Complex - Complex number math functions.
	Copyright (C) 1996-2010 Michael Griebling
 
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

	enum NotationType {
		case Normal
		case Scientific
		case Engineering
	}
	enum AngularMeasure {
		case Degrees
		case Radians
		case Gradians
	}
	
	var real, imag : Real
	
	struct NumbState {
		init () {
			LocalBase = 10
			Notation = NotationType.Normal
			DecPoint = 0
			DegRadFlag = AngularMeasure.Degrees
			DigSep = ","
			FracSep = "."
			Rational = false
		}
		
		mutating func Default() {
			/* set up default state */
			LocalBase = 10
			Notation = NotationType.Normal
			DecPoint = 0
			DegRadFlag = AngularMeasure.Degrees
			DigSep = "\0"
			FracSep = "\0"
			Rational = false
		} // Default;
	
		var LocalBase  : Int
		var Notation   : NotationType
		var DecPoint   : Int
		var DegRadFlag : AngularMeasure
		var DigSep     : Character
		var FracSep    : Character
		var Rational   : Bool
	}
	
	var description: String {
		return self.ToString()
	}
	
	static let zero = Complex()
	static let one = Complex(re: Real.one, im: Real.zero)
	static var nState = NumbState()
	
	/*---------------------------------------------------------*/
	/*                                                         */
	/* The following routines are intended for external users  */
	/*                                                         */
	/*---------------------------------------------------------*/
	/* Constructors */
	
	init() {
		self.real = Real.zero
		self.imag = Real.zero
	}
	
	init (re: Real, im : Real) {
		self.real = re; self.imag = im
	} // Init;
	
	init (fromDouble: Double) {
		self.init(fromReal:Real(fromDouble: fromDouble))
	}
	
	init (fromComplex c: Complex) {
		self.init(re:c.real, im:c.imag)
	}
	
	init (fromReal: Real) {
		self.init(re:fromReal, im:Real.zero)
	}
	
	init (r: Real, theta: Real) {
		var s, c: Real
		var t: Complex
		c = Real.zero
		s = ToRadians(theta); s.SinCos(&s, cos: &c)
		t = Complex.one.Add(Complex(re: r.Mul(c), im: r.Mul(s)))   /* force rounding */
		self = t.Sub(Complex.one)
	} // ToRectangular;
	
	/*---------------------------------------------------------*/
	/* Conversion routines                                     */
	
	func ToString () -> String {
		var Result : String
		var iZero, rZero: Bool
		var EngFormat: Bool
		var ds: Int = 3
		var ExpWidth: Int
		var cs: String
		var Str: String = ""
		var Aim: Real
		var A: Complex = self
		
		func NumToStr (A : Real, Decimal: Int, ExpWidth : Int, EngFormat: Bool) -> String {
			/* Combines radix and normal conversions */
			var ia: Integer
			
			if Complex.nState.LocalBase == 10 {
				return A.ToString(Decimal, ExpWidth: ExpWidth, EngFormat: EngFormat)
			} else {
				if Complex.nState.LocalBase != 8 { ds = 4 }
				ia = ToInteger(A)
				return ia.description(Complex.nState.LocalBase)
			}
		} // NumToStr;
		
		func SetChar (s: String, pos: Range<String.Index>, ch: Character) -> String {
			var f = s
			f.replaceRange(pos, with: [ch])
			return f
		} // SetChar;
		
		func InsertChar (s: String, pos: String.Index, ch: Character) -> String {
			var f = s
			f.insert(ch, atIndex: pos)
			return f
		} // InsertChar;
		
		func DecIndex (pos : String.Index, var by: Int) -> String.Index {
			var ipos = pos
			while (by > 0) && (pos > Str.startIndex) {
				ipos--; by--
			}
			return ipos
		}
		
		func IncIndex (pos : String.Index, var by: Int) -> String.Index {
			var ipos = pos
			while (by > 0) && (pos < Str.endIndex) {
				ipos++; by--
			}
			return ipos
		}
		
		/* round numbers */
		A.real = A.real.Add(Real.one).Sub(Real.one)
		A.imag = A.imag.Add(Real.one).Sub(Real.one)
		
		iZero = A.imag.isZero(); rZero = A.real.isZero()
		ds = 3
		if Complex.nState.Notation != NotationType.Normal { ExpWidth = 1 } else { ExpWidth = 0 }
		EngFormat = Complex.nState.Notation == NotationType.Engineering
		if iZero || !rZero {
			Str = A.real.ToString(Complex.nState.DecPoint, ExpWidth: ExpWidth, EngFormat: EngFormat) /* real part */
		} else {
			Str = ""
		}
		if !iZero {
			Aim = A.imag
			if !Aim.isNegative() {
				if !rZero { Str.append(Character("+")) }
			} else {
				Str.append(Character("-"))
			}
			Aim = Aim.Abs()
			if Aim.Cmp(Real.one) != 0 {
				Result = Aim.ToString(Complex.nState.DecPoint, ExpWidth: ExpWidth, EngFormat: EngFormat)  /* imaginary part */
				Str += Result
			}
			Str.append(Character("i"))
		}
		
		/* add digit separators */
		var dp = Str.rangeOfString(".")				/* find location of decimal point */
		if dp == nil {
			dp = Str.endIndex...Str.endIndex
		} else if Complex.nState.DigSep == "." {
			Str = SetChar(Str, dp!, ",")
		}
		if Complex.nState.DigSep != "\0" {
			var i = DecIndex(dp!.startIndex, ds)	/* insert whole separator */
			while i > Str.startIndex {
				Str = InsertChar(Str, i, Complex.nState.DigSep)
				i = DecIndex(i, ds)
			}
		}
		if Complex.nState.FracSep != "\0" {
			var i = IncIndex(dp!.startIndex, ds+1)	/* insert fraction separator */
			while i < Str.endIndex {
				Str = InsertChar(Str, i, Complex.nState.DigSep)
				i = IncIndex(i, ds+1)
			}
		}
		return Str
	} // Format;
	
	func Conj () -> Complex {
		return Complex(re: self.real, im: Real.zero.Sub(self.imag))
	} // Conj;
	
	
	func PolarMag () -> Real {
		var A: Real
		var B = self
		
		if B.imag.isZero() {
			return B.real.Abs()
		} else if B.real.isZero() {
			return B.imag.Abs()
		} else {
			A = B.imag.Mul(B.imag)
			A = A.Add(B.real.Mul(B.real))
			return A.Sqrt()
		}
	} // PolarMag;
	
	func PolarAngle () -> Real {
		return FromRadians(self.imag.Arctan2(self.real))
	} // PolarAngle;
	
	func Short () -> Double {
		return self.real.Short()
	} // Short;
	
	
	func Neg () -> Complex {
		return Complex(re: Real.zero.Sub(self.real), im: Real.zero.Sub(self.imag))
	} // Neg;
	
	
	func Add (C : Complex) -> Complex {
		/** A = B + C = (a + bi) + (c + di) = (a+c) + (b+d)i */
		return Complex(re: self.real.Add(C.real), im: self.imag.Add(C.imag))
	} // Add;
	
	
	func Sub (C : Complex) -> Complex {
		/** A = B - C = (a + bi) - (c + di) = (a-c) + (b-d)i */
		return Complex(re: self.real.Sub(C.real), im: self.imag.Sub(C.imag))
	} // Sub;
	
	func Mul (C : Complex) -> Complex {
		var r, i: Real
		/* A = B * C = (a + bi) * (c + di) = (ac-bd) + (ad+bc)i */
		r = self.real.Mul(C.real); i = self.real.Mul(C.imag);
		return Complex(re: r.Sub(self.imag.Mul(C.imag)), im: i.Add(self.imag.Mul(C.real)))
	} // Mul;
	
	func Div (C : Complex) -> Complex {
		var div, r, i : Real
		/**
		A = B / C = (a + bi) / (c + di) = (ac+bd)/e + (bc-ad)/e i
		where e = c^2+d^2
		*/
		div = C.real.Mul(C.real); div = div.Add(C.imag.Mul(C.imag));
		r = self.real.Mul(C.real); r = r.Add(self.imag.Mul(C.imag));
		i = self.imag.Mul(C.real); i = i.Sub(self.real.Mul(C.imag));
		return Complex(re: r.Div(div), im: i.Div(div))
	} // Div;
	
	func Cmp (b: Complex) -> Int {
		/**
		This routine compares the extended numbers `a' and `b' and
		returns the value -1, 0, or 1 depending on whether 'a'<'b',
		'a'='b', or 'a'>'b'.  It is faster than merely subtracting
		'a' and 'b' and looking at the sign of the result.
		*/
		var c: Real
		if self.imag.isZero() { c = self.real } else { c = self.PolarMag() }
		if b.imag.isZero() {
			return c.Cmp(b.real)
		} else {
			return c.Cmp(self.PolarMag())
		}
	} // Cmp;
	
//	func /* (a: Complex) */ Store* (w: Storable.Writer) RAISES IO.Error;
//	/** Write 'a' to the 'w' writer. */
//	
//	a.real.Store(w); a.imag.Store(w);
//	} // Store;
//	
//	func /* (a: Complex) */ Load*(r: Storable.Reader) RAISES IO.Error;
//	/** Read 'a' from the 'r' reader. */
//	
//	NEW(a.real); NEW(a.imag);
//	a.real.Load(r); a.imag.Load(r);
//	} // Load;
	
	/*---------------------------------------------------------*/
	/* Power and transcendental routines                       */
	
	func IPower (var i : Int) -> Complex {
		var Y = Complex.one
		var negative = i < 0
		var x = self
		i  =  abs(i)
		for ;; {
			if (i&1) != 0 { Y = Y.Mul(x) }
			i  =  i / 2
			if i == 0 { break }
			x = x.Mul(x)
		}
		if negative {
			return Complex.one.Div(Y)
		} else {
			return Y
		}
	} // IPower;
	
	
	func IRoot (i : Int) -> Complex {
		var r, theta: Real
		var x = self
		let izero = x.imag.isZero()
		if izero && !x.real.isZero() {
			return Complex(re: x.real.IRoot(i), im: Real.zero)
		} else if izero && ((i&1) != 0) {
			r = x.real.Abs(); r = r.IRoot(i);
			return Complex(re: Real.zero.Sub(r), im: Real.zero)
		} else {
			r = x.PolarMag(); theta = x.PolarAngle()
			return Complex(r: r.IRoot(i), theta: theta.Div(Real(fromInt: i)))
		}
	} // IRoot;

	
	func fromRadians () -> Complex {
		/* Convert a radian measure into current angle measure */
		return Complex(re: FromRadians(self.real), im: FromRadians(self.imag))
	} // fromRadians;
	
	
	func toRadians () -> Complex {
		/* Convert an angle measure into radians */
		return Complex(re: ToRadians(self.real), im: ToRadians(self.imag))
	} // toRadians;
	
	
	func Sqrt () -> Complex {
		return self.IRoot(2)
	} // Sqrt;
	
	
	func Ln () -> Complex {
		var pos : Bool
		var t, half: Real
		var x = self
		
		if x.imag.isZero() {
			pos = !x.real.isNegative()
			t = x.real.Abs()
			if pos { return Complex(re: t.Ln(), im: Real.zero)
			} else { return Complex(re: t.Ln(), im: Real.pi)
			}
		} else if x.real.isZero() {
			pos = !x.imag.isNegative()
			t = x.imag.Abs(); half = Real(fromDouble: 0.5)
			if !pos {
				return Complex(re: t.Ln(), im: Real.zero.Sub(half.Mul(Real.pi)))
			} else {
				return Complex(re: t.Ln(), im: half.Mul(Real.pi))
			}
		} else {
			t = x.PolarMag()
			return Complex(r: t.Ln(), theta: x.PolarAngle())
		}
	} // Ln;
	
	
	func Log (base: Complex) -> Complex {
		var x = self.Ln()
		return x.Div(base.Ln())
	} // Log;
	
	
	func Exp () -> Complex {
		return Complex(r: self.real.Exp(), theta: self.imag)
	} // Exp;
	
	
	func Power (y : Complex) -> Complex {
		var p : Double
		var x = self
		
		if y.imag.isZero() && !y.real.isNegative() { /* just real powers */
			if x.imag.isZero() && !x.real.isNegative() { /* real numbers */
				return Complex(re: x.real.Power(y.real), im: Real.zero)
			} else {
				p = y.Short()
				if (abs(p) < Double(Int.max)) && y.real.Fraction().isZero() {
					return x.IPower(Int(p))
				} else {
					x = y.Mul(x.Ln()); return x.Exp()
				}
			}
		} else {
			x = y.Mul(x.Ln()); return x.Exp()
		}
	} // Power;
	
	
	func Root (y : Complex) -> Complex {
		var r: Double
		var x = self
		
		if y.imag.isZero() && !y.real.isNegative() { /* just real roots */
			if x.imag.isZero() && !x.real.isNegative() { /* real numbers */
				return Complex(re: x.real.IRoot(Int(y.real.Short())), im: Real.zero)
			} else {
				r = y.Short()
				if (abs(r) < Double(Int.max)) && y.real.Fraction().isZero() {
					return x.IRoot(Int(r))
				} else {
					x = x.Ln(); x = x.Div(y); return x.Exp()
				}
			}
		} else {
			x = x.Ln(); x = x.Div(y); return x.Exp()
		}
	} // Root;
	
	
	func Sin () -> Complex {
		var s,c,sh,ch: Real
		var x = self
		s = ToRadians(x.real); c = Real.zero; ch = Real.zero; sh = Real.zero
		s.SinCos(&s, cos: &c); x.imag.SinhCosh(&sh, cosh: &ch)
		return Complex(re: s.Mul(ch), im: c.Mul(sh))
	} // Sin;
	
	
	func Cos () -> Complex {
		var s,c,sh,ch: Real
		var x = self
		s = ToRadians(x.real); c = Real.zero; ch = Real.zero; sh = Real.zero
		s.SinCos(&s, cos: &c); x.imag.SinhCosh(&sh, cosh: &ch)
		return Complex(re: c.Mul(ch), im: Real.zero.Sub(s.Mul(sh)))
	} // Cos;
	
	
	func SinCos (inout sin: Complex, inout cos: Complex) {
		var s,c,sh,ch: Real
		var x = self
		s = ToRadians(x.real); c = Real.zero; ch = Real.zero; sh = Real.zero
		s.SinCos(&s, cos: &c); x.imag.SinhCosh(&sh, cosh: &ch)
		sin = Complex(re: s.Mul(ch), im: c.Mul(sh))
		cos = Complex(re: s.Mul(c), im: Real.zero.Sub(sh.Mul(sh)))
	} // SinCosC;
	
	
	func Tan () -> Complex {
		var d, s, c, sh, ch: Real
		var x = self
		
		let TWO = Real(fromInt: 2); s = ToRadians(x.real)
		s = TWO.Mul(s); sh = TWO.Mul(x.imag)
		c = Real.zero; ch = Real.zero; sh = Real.zero
		s.SinCos(&s, cos: &c); sh.SinhCosh(&sh, cosh: &ch)
		d = ch.Add(c);
		return Complex(re: s.Div(d), im: sh.Div(d))
	} // Tan;
	
	
	private func CalcAlphaBeta (inout a: Real, inout b: Real) {
		var x, x2, y, r, t: Real
		var z = self
		let HALF = Real(fromDouble: 0.5)
		x = z.real.Add(Real.one); x = x.Mul(x); y = z.imag.Mul(z.imag)
		x2 = z.real.Sub(Real.one); x2 = x2.Mul(x2)
		t = x.Add(y); r = t.Sqrt(); t = x2.Add(y)
		t = t.Sqrt()
		a = HALF.Mul(r.Add(t))
		b = r.Sub(t); b = b.Mul(HALF)
	} // CalcAlphaBeta;
	
	
	func /* (x: Complex) */ Arcsin () -> Complex {
		var a, b, t: Real
		var x = self
		a = Real.zero; b = Real.zero
		x.CalcAlphaBeta(&a, b: &b)
		t = a.Mul(a); t = t.Sub(Real.one); t = a.Add(t.Sqrt());
		return Complex(re: FromRadians(b.Arcsin()), im: t.Ln())
	} // Arcsin;
	
	func /* (x: Complex) */ Arccos () -> Complex {
		var a, b, t: Real
		var x = self
		a = Real.zero; b = Real.zero
		x.CalcAlphaBeta(&a, b: &b)
		t = a.Mul(a); t = t.Sub(Real.one); t = a.Add(t.Sqrt())
		return Complex(re: FromRadians(b.Arccos()), im: Real.zero.Sub(t.Ln()))
	} // Arccos;
	
	
	func Arctan () -> Complex {
		var x, x2, y2, y, yp, t: Real
		var z = self
		let TWO = Real(fromInt: 2)
		let HALF = Real(fromDouble: 0.5)
		let QUARTER = Real(fromDouble: 0.25)
		x = TWO.Mul(z.real); y = z.imag.Add(Real.one); y = y.Mul(y)
		yp = z.imag.Sub(Real.one); yp = yp.Mul(yp)
		x2 = z.real.Mul(z.real); y2 = z.imag.Mul(z.imag)
		t = Real.one.Sub(x2); t = t.Sub(y2); t = x.Div(t)
		x = HALF.Mul(t.Arctan())
		t = x2.Add(y); t = t.Div(x2.Add(yp))
		y = QUARTER.Mul(t.Ln())
		return Complex(re: FromRadians(x), im: y)
	} // Arctan;
	
	
	func Sinh () -> Complex {
		var s1, c1, s2, c2: Real
		var x = self
		s1 = Real.zero; c1 = Real.zero; c2 = Real.zero; s2 = Real.zero
		x.real.SinhCosh(&s1, cosh: &c1); x.imag.SinhCosh(&s2, cosh: &c2)
		return Complex(re: s1.Mul(c2), im: c1.Mul(s2))
	} // Sinh;
	
	
	func Cosh () -> Complex {
		var s1, c1, s2, c2: Real
		var x = self
		s1 = Real.zero; c1 = Real.zero; c2 = Real.zero; s2 = Real.zero
		x.real.SinhCosh(&s1, cosh: &c1); x.imag.SinhCosh(&s2, cosh: &c2)
		return Complex(re: c1.Mul(c2), im: s1.Mul(s2))
	} // Cosh;
	
	
	func SinhCosh (inout sinh: Complex, inout cosh : Complex) {
		var s1, c1, s2, c2: Real
		var x = self
		s1 = Real.zero; c1 = Real.zero; c2 = Real.zero; s2 = Real.zero
		x.real.SinhCosh(&s1, cosh: &c1); x.imag.SinhCosh(&s2, cosh: &c2)
		sinh = Complex(re: s1.Mul(c2), im: c1.Mul(s2))
		cosh = Complex(re: c1.Mul(c2), im: s1.Mul(s2))
	} // SinhCoshC;
	
	
	func Tanh () -> Complex {
		var a, b : Complex
		a = Complex.zero; b = Complex.zero
		self.SinhCosh(&a, cosh: &b)
		return a.Div(b)
	} // Tanh;
	
	
	func Arcsinh () -> Complex {
		var Temp : Complex
		var x = self
		
		/* Result = ln(x + sqrt(x*x + 1)) */
		Temp = Complex.one.Add(x.Mul(x)); Temp = x.Add(Temp.Sqrt())
		return Temp.Ln()
	} // Arcsinh;
	
	
	func Arccosh () -> Complex {
		var Temp : Complex
		var x = self
		
		/* Result = ln(x + sqrt(x*x - 1)) */
		Temp = x.Mul(x); Temp = Temp.Sub(Complex.one); Temp = x.Add(Temp.Sqrt())
		return Temp.Ln()
	} // Arccosh;
	
	
	func Arctanh () -> Complex {
		var Temp, Temp2, Half: Complex
		var x = self
		
		/* Result = ln((1 + x) / (1 - x)) / 2 */
		Temp = Complex.one.Add(x); Temp2 = Complex.one.Sub(x); Temp = Temp.Div(Temp2)
		if Real.err == 0 {
			Half = Complex(fromDouble: 0.5)
			return Half.Mul(Temp.Ln())
		} else {
			return Complex.zero
		}
	} // Arctanh;

//	func Store* (w: Storable.Writer) RAISES IO.Error;
//	/** Write calculator state to the 'w' writer. */
//	
//	w.WriteSInt(nState.LocalBase);
//	w.WriteSInt(nState.Notation);
//	w.WriteSInt(nState.DecPoint);
//	w.WriteSInt(nState.DegRadFlag);
//	w.WriteChar(nState.DigSep);
//	w.WriteChar(nState.FracSep);
//	w.WriteNum(X.sigDigs);
//	w.WriteBool(nState.Rational);
//	} // Store;
//	
//	func Load*(r: Storable.Reader) RAISES IO.Error;
//	/** Read calculator state from the 'r' reader. */
//	var digs: Int;
//	
//	r.ReadSInt(nState.LocalBase);
//	r.ReadSInt(nState.Notation);
//	r.ReadSInt(nState.DecPoint);
//	r.ReadSInt(nState.DegRadFlag);
//	r.ReadChar(nState.DigSep);
//	r.ReadChar(nState.FracSep);
//	r.ReadNum(digs);
//	X.SetDigits(digs);
//	r.ReadBool(nState.Rational);
//	} // Load;

	func Default () {
		Complex.nState.Default()
		Real.digits = 10
	}
	
	
	func Test () {
		var x:Real
		var xi:Integer
		
		xi = ToInteger(Real.pi)
		println("Real = \(Real.pi); Int = \(xi)")
		x = Real(fromString: "1234567890.12345")
		xi = ToInteger(x)
		println("Real = \(x); Int = \(xi)")
		
		x = Real(fromString: "1234567890123456789012345678901234567890.12345")
		xi = ToInteger(x)
		println("Real = \(x); Int = \(xi)")
		
		xi = Integer(fromString: "98765432109876543210")
		x = ToReal(xi)
		println("Real = \(x); Int = \(xi)")
	} // Test;

}
