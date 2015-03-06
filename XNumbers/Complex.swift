////
////  Complex.swift
////  XNumbers
////
////  Created by Mike Griebling on 5 Mar 2015.
////  Copyright (c) 2015 Computer Inspirations. All rights reserved.
////
//
//import Foundation
//
//struct Complex {
//	
//	/*
//	Complex - Complex number math functions.
//	Copyright (C) 1996-2010 Michael Griebling
// 
//	This module is free software; you can redistribute it and/or modify
//	it under the terms of the GNU General Public License as published by
//	the Free Software Foundation; either version 2 of the License, or
//	(at your option) any later version.
// 
//	This module is distributed in the hope that it will be useful,
//	but WITHOUT ANY WARRANTY; without even the implied warranty of
//	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//	GNU General Public License for more details.
// 
//	You should have received a copy of the GNU General Public License
//	along with this program; if not, write to the Free Software
//	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
//	
//	*/
//	
//	// IMPORT X  =  Reals, XI  =  Integers, ADT:Storable, Object:Boxed, Object, Out, IO;
//	
//	enum NotationType {
//		case Normal
//		case Scientific
//		case Engineering
//	}
//	enum AngularMeasure {
//		case Degrees
//		case Radians
//		case Gradians
//	}
//	
//	var real, imag : Real
//	
//	struct NumbState {
//		init () {
//			LocalBase = 10
//			Notation = NotationType.Normal
//			DecPoint = 0
//			DegRadFlag = AngularMeasure.Degrees
//			DigSep = ","
//			FracSep = "."
//			Rational = false
//		}
//		
//		var LocalBase  : Int
//		var Notation   : NotationType
//		var DecPoint   : Int
//		var DegRadFlag : AngularMeasure
//		var DigSep     : Character
//		var FracSep    : Character
//		var Rational   : Bool
//	}
//	
//	static let zero = Complex()
//	static let one = Complex()
//	static var nState = NumbState()
//	
//	func IsZero (A : Real) -> Bool {
//		return A.Cmp(Real.zero) == 0
//	} // IsZero;
//	
//	func IsNegative (A : Real) -> Bool {
//		return A.Cmp(X.zero) == -1
//	} // IsNegative;
//	
//	/*---------------------------------------------------------*/
//	/*                                                         */
//	/* The following routines are intended for external users  */
//	/*                                                         */
//	/*---------------------------------------------------------*/
//	/* Constructors */
//	
//	init() {
//		self.real = Real(fromInteger: 0)
//		self.imag = Real(fromInteger: 0)
//	}
//	
//	func RealToInteger (A : Real) -> Integer {
//	let MAXC = 0x40000000
//	var x, iMAX : Integer;
//	var a, MAX : Real;
//	var exp : Int;
//	var neg : Bool;
//	
//	A = A.Entier();
//	x = XI.NewInt(0); exp = 0;
//	MAX = X.Long(MAXC); iMAX = XI.NewInt(MAXC);
//	neg = FALSE;
//	if IsNegative(A) { A = A.Abs(); neg = TRUE }
//	while A.Cmp(MAX) = 1 {
//	A = A.Div(MAX);
//	INC(exp)
//	}
//	do {
//	a = A.Entier(); x = x.Mul(iMAX);
//	x = x.Add(XI.NewInt(ENTIER(a.Short())));
//	A = MAX.Mul(A.Fraction());
//	DEC(exp)
//	} while !(exp<0)
//	if neg { x = XI.zero.Sub(x) }
//	return x
//	} // RealToInteger;
//	
//	func IntegerToReal (A : Integer) -> Real {
//	let 
//	MAXC = 1000000000;
//	var
//	iMAX, ia : Integer;
//	x, y, MAX : Real;
//	neg : Bool;
//	
//	x = X.Long(0);
//	MAX = X.Long(MAXC); iMAX = XI.NewInt(MAXC);
//	neg = FALSE; y = X.Long(1);
//	if A.Sign() = -1 { A = A.Abs(); neg = TRUE }
//	while A.Sign() = 1 {
//	ia = A.Mod(iMAX);
//	x = x.Add(y.Mul(X.Long(ia.ToLongInt())));
//	A = A.Div(iMAX); y = y.Mul(MAX)
//	}
//	if neg { x = X.zero.Sub(x) }
//	return x
//	} // IntegerToReal;
//	
//	/*---------------------------------------------------------*/
//	/* Conversion routines                                     */
//	
//	func (A: Complex) Format * () -> String;
//	var
//	Result : String;
//	iZero, rZero: Bool;
//	EngFormat: Bool;
//	i, ds, dp: Int;
//	ExpWidth: Int;
//	cs: ARRAY 2 OF Character;
//	Str: String;
//	Aim: Real;
//	
//	func NumToStr (A : Real; Decimal, ExpWidth : Int; EngFormat: Bool) -> String;
//	/* Combines radix and normal conversions */
//	var
//	ia: Integer;
//	
//	if nState.LocalBase=10 {
//	return A.Format(Decimal, ExpWidth, EngFormat)
//	} else {
//	if nState.LocalBase#8 { ds = 4 }
//	ia = RealToInteger(A);
//	return ia.Format(nState.LocalBase)
//	}
//	} // NumToStr;
//	
//	func SetChar (s: String; pos: Int; ch: Character) -> String;
//	var
//	f: String;
//	
//	f = s.Substring(0, pos-1); f = f.Concat(Object.NewLatin1Char(ch));
//	f = f.Concat(s.Substring(pos+1, s.length));
//	return f
//	} // SetChar;
//	
//	func InsertChar (s: String; pos: Int; ch: Character) -> String;
//	var
//	f: String;
//	
//	f = s.Substring(0, pos-1); f = f.Concat(Object.NewLatin1Char(ch));
//	f = f.Concat(s.Substring(pos, s.length));
//	return f
//	} // InsertChar;
//	
//	
//	/* round numbers */
//	A.real = X.Sub2(A.real.Add(X.one), X.one);
//	A.imag = X.Sub2(A.imag.Add(X.one), X.one);
//	
//	iZero  =  IsZero(A.imag); rZero  =  IsZero(A.real);
//	ds = 3;
//	if nState.Notation>Normal { ExpWidth = 1 } else { ExpWidth = 0 }
//	EngFormat = nState.Notation=Engineering;
//	if iZero OR ~rZero {
//	Str = NumToStr(A.real, nState.DecPoint, ExpWidth, EngFormat) /* real part */
//	} else { Str = ""
//	}
//	if ~iZero {
//	Aim = A.imag;
//	if ~IsNegative(Aim) {
//	if ~rZero { Str = Str.Concat("+") }
//	} else { Str = Str.Concat("-")
//	}
//	Aim = Aim.Abs();
//	if Aim.Cmp(X.one) # 0 {
//	Result = NumToStr(Aim, nState.DecPoint, ExpWidth, EngFormat);  /* imaginary part */
//	Str = Str.Concat(Result)
//	}
//	Str = Str.Concat("i")
//	}
//	
//	/* add digit separators */
//	cs[1] = 0X;
//	dp = Str.IndexOf(".", 0);                  /* find location of decimal point */
//	if dp<0 { dp = Str.length
//	ELSif nState.DigSep="." { Str = SetChar(Str, dp, ",")
//	}
//	if nState.DigSep # 0X {
//	i = dp-ds;                               /* insert whole separator */
//	while i>0 {
//	Str = InsertChar(Str, i, nState.DigSep);
//	DEC(i, ds); INC(dp)
//	}
//	}
//	if nState.FracSep # 0X {
//	i = dp+ds+1;                             /* insert fraction separator */
//	while i<Str.length {
//	Str = InsertChar(Str, i, nState.DigSep);
//	INC(i, ds+1)
//	}
//	}
//	return Str
//	} // Format;
//	
//	func /* (x: Complex) */ ToString*(): String;
//	
//	return x.Format()
//	} // ToString;
//	
//	func /* (x: Complex) */ Init * (re, im : Real);
//	
//	x.real  =  re; x.imag  =  im
//	} // Init;
//	
//	func Init * (re, im : Real) -> Complex {
//	var
//	A: Complex;
//	
//	NEW(A); A.Init(re, im);
//	return A
//	} // Init;
//	
//	
//	func /* (x: Complex) */ Conj * () -> Complex {
//	
//	return Init(x.real, X.zero.Sub(x.imag))
//	} // Conj;
//	
//	
//	func (B: Complex) PolarMag * () -> Real {
//	var
//	A: Real;
//	
//	if IsZero(B.imag) { return B.real.Abs()
//	ELSif IsZero(B.real) { return B.imag.Abs()
//	} else {
//	A = B.imag.Mul(B.imag);
//	A = A.Add(B.real.Mul(B.real));
//	return A.Sqrt()
//	}
//	} // PolarMag;
//	
//	PROCEDURE^ fromRadians (radianAngle: Real) -> Real {
//	
//	func (B: Complex) PolarAngle * () -> Real {
//	
//	return fromRadians(B.imag.Arctan2(B.real))
//	} // PolarAngle;
//	
//	PROCEDURE^ toRadians (radianAngle : Real) -> Real {
//	
//	func Long * (x : LONGREAL) -> Complex {
//	
//	return Init(X.Long(x), X.zero)
//	} // Long;
//	
//	
//	func (xc : Complex) Short * () -> LONGREAL;
//	
//	return xc.real.Short()
//	} // Short;
//	
//	
//	func /* (x: Complex) */ Neg * () -> Complex {
//	
//	return Init(X.zero.Sub(x.real), X.zero.Sub(x.imag))
//	} // Neg;
//	
//	
//	func /* (B : Complex) */ Add * (C : Complex) -> Complex {
//	
//	/** A = B + C = (a + bi) + (c + di) = (a+c) + (b+d)i */
//	return Init(B.real.Add(C.real), B.imag.Add(C.imag))
//	} // Add;
//	
//	
//	func /* (B : Complex) */ Sub * (C : Complex) -> Complex {
//	
//	/** A = B - C = (a + bi) - (c + di) = (a-c) + (b-d)i */
//	return Init(B.real.Sub(C.real), B.imag.Sub(C.imag))
//	} // Sub;
//	
//	
//	func ToRectangular * (r, theta : Real) -> Complex {
//	var
//	s, c: Real;
//	t: Complex;
//	
//	s = toRadians(theta); s.SinCos(s, c);
//	t = one.Add(Init(r.Mul(c), r.Mul(s)));   /* force rounding */
//	return t.Sub(one)
//	} // ToRectangular;
//	
//	
//	func /* (B : Complex) */ Mul * (C : Complex) -> Complex {
//	var
//	r, i: Real;
//	
//	/* A = B * C = (a + bi) * (c + di) = (ac-bd) + (ad+bc)i */
//	r = B.real.Mul(C.real); i = B.real.Mul(C.imag);
//	return Init(r.Sub(B.imag.Mul(C.imag)), i.Add(B.imag.Mul(C.real)))
//	} // Mul;
//	
//	
//	func /* (B : Complex) */ Div * (C : Complex) -> Complex {
//	var div, r, i : Real;
//	
//	/**
//	A = B / C = (a + bi) / (c + di) = (ac+bd)/e + (bc-ad)/e i
//	where e = c^2+d^2
//	*/
//	div = C.real.Mul(C.real); div = div.Add(C.imag.Mul(C.imag));
//	r = B.real.Mul(C.real); r = r.Add(B.imag.Mul(C.imag));
//	i = B.imag.Mul(C.real); i = i.Sub(B.real.Mul(C.imag));
//	return Init(r.Div(div), i.Div(div))
//	} // Div;
//	
//	func /* (a: Complex) */ Cmp * (b: Object.Object) -> Int;
//	/**
//	This routine compares the extended numbers `a' and `b' and
//	returns the value -1, 0, or 1 depending on whether 'a'<'b',
//	'a'='b', or 'a'>'b'.  It is faster than merely subtracting
//	'a' and 'b' and looking at the sign of the result.
// */
//	var
//	c: Real;
//	
//	if IsZero(a.imag) { c = a.real } else { c = a.PolarMag() }
//	WITH b: Complex {
//	if IsZero(b.imag) { return c.Cmp(b.real)
//	} else { return c.Cmp(a.PolarMag())
//	}
//	}
//	} // Cmp;
//	
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
//	
//	/*---------------------------------------------------------*/
//	/* Power and transcendental routines                       */
//	
//	func /* (x: Complex) */ IPower * (i : Int) -> Complex {
//	var
//	Y : Complex;
//	negative : Bool;
//	
//	Y  =  one;
//	negative  =  i < 0;
//	i  =  ABS(i);
//	LOOP
//	if ODD(i) { Y = Y.Mul(x) }
//	i  =  i DIV 2;
//	if i = 0 { EXIT }
//	x = x.Mul(x);
//	}
//	if negative {
//	return one.Div(Y);
//	} else {
//	return Y
//	}
//	} // IPower;
//	
//	
//	func /* (x: Complex) */ IRoot * (i : Int) -> Complex {
//	var r, theta: Real; izero: Bool;
//	
//	izero = IsZero(x.imag);
//	if izero & ~IsNegative(x.real) {
//	return Init(x.real.IRoot(i), X.zero)
//	ELSif izero & ODD(i) {
//	r = x.real.Abs(); r = r.IRoot(i);
//	return Init(X.zero.Sub(r), X.zero)
//	} else {
//	r = x.PolarMag(); theta = x.PolarAngle();
//	return ToRectangular(r.IRoot(i), theta.Div(X.Long(i)))
//	}
//	} // IRoot;
//	
//	
//	func fromRadians (radianAngle: Real) -> Real {
//	var
//	K180, K200: Real;
//	
//	/* conversion to/from radian angles */
//	if nState.DegRadFlag=Degrees {
//	K180 = X.Long(180);
//	return radianAngle.Mul(K180.Div(X.pi))
//	ELSif nState.DegRadFlag=Gradians {
//	K200 = X.Long(200);
//	return radianAngle.Mul(K200.Div(X.pi))
//	} else {
//	return radianAngle
//	}
//	} // fromRadians;
//	
//	
//	func toRadians (radianAngle : Real) -> Real {
//	/* Convert an angular measure into radians */
//	
//	/* conversion to/from radian angles */
//	if nState.DegRadFlag=Degrees {
//	return radianAngle.Mul(X.pi.Div(X.Long(180)))
//	ELSif nState.DegRadFlag=Gradians {
//	return radianAngle.Mul(X.pi.Div(X.Long(200)))
//	} else {
//	return radianAngle
//	}
//	} // toRadians;
//	
//	
//	func /* (x: Complex) */ fromRadians * () -> Complex {
//	/* Convert a radian measure into current angle measure */
//	
//	return Init(fromRadians(x.real), fromRadians(x.imag))
//	} // fromRadians;
//	
//	
//	func /* (x: Complex) */ toRadians*() -> Complex {
//	/* Convert an angle measure into radians */
//	
//	return Init(toRadians(x.real), toRadians(x.imag))
//	} // toRadians;
//	
//	
//	func /* (x: Complex) */ Sqrt* () -> Complex {
//	
//	return x.IRoot(2);
//	} // Sqrt;
//	
//	
//	func /* (x: Complex) */ Ln* () -> Complex {
//	var
//	pos : Bool;
//	t, half: Real;
//	
//	if IsZero(x.imag) {
//	pos  =  ~IsNegative(x.real);
//	t = x.real.Abs();
//	if pos { return Init(t.Ln(), X.zero)
//	} else { return Init(t.Ln(), X.pi)
//	}
//	ELSif IsZero(x.real) {
//	pos  =  ~IsNegative(x.imag);
//	t = x.imag.Abs(); half = X.Long(0.5);
//	if ~pos {
//	return Init(t.Ln(), X.zero.Sub(half.Mul(X.pi)))
//	} else {
//	return Init(t.Ln(), half.Mul(X.pi))
//	}
//	} else {
//	t = x.PolarMag();
//	return Init(t.Ln(), x.PolarAngle())
//	}
//	} // Ln;
//	
//	
//	func /* (x: Complex) */ Log* (base: Real) -> Complex {
//	var
//	lne : Real; /* log e */
//	
//	lne = X.one.Exp();  /* e */
//	lne = lne.Log();
//	x = x.Ln();
//	return Init(x.real.Mul(lne), x.imag.Mul(lne))
//	} // Log;
//	
//	
//	func /* (x: Complex) */ Exp* () -> Complex {
//	
//	return ToRectangular(x.real.Exp(), x.imag)
//	} // Exp;
//	
//	
//	func /* (x: Complex) */ Power* (y : Complex) -> Complex {
//	var p : LONGREAL;
//	
//	if IsZero(y.imag) & ~IsNegative(y.real) { /* just real powers */
//	if IsZero(x.imag) & ~IsNegative(x.real) { /* real numbers */
//	return Init(x.real.Power(y.real), X.zero)
//	} else {
//	p = y.Short();
//	if (ABS(p) < MAX(Int)) & IsZero(y.real.Fraction()) {
//	return x.IPower(ENTIER(p))
//	} else { x = y.Mul(x.Ln()); return x.Exp()
//	}
//	}
//	} else { x = y.Mul(x.Ln()); return x.Exp()
//	}
//	} // Power;
//	
//	
//	func /* (x: Complex) */ Root* (y : Complex) -> Complex {
//	var r:LONGREAL;
//	
//	if IsZero(y.imag) & ~IsNegative(y.real) { /* just real roots */
//	if IsZero(x.imag) & ~IsNegative(x.real) { /* real numbers */
//	return Init(x.real.IRoot(ENTIER(y.real.Short())), X.zero)
//	} else { r = y.Short();
//	if (ABS(r) < MAX(Int)) & IsZero(y.real.Fraction()) {
//	return x.IRoot(ENTIER(r))
//	} else { x = x.Ln(); x = x.Div(y); return x.Exp()
//	}
//	}
//	} else { x = x.Ln(); x = x.Div(y); return x.Exp()
//	}
//	} // Root;
//	
//	
//	func /* (x: Complex) */ Sin* () -> Complex {
//	var
//	s,c,sh,ch: Real;
//	
//	s = toRadians(x.real);
//	s.SinCos(s, c); x.imag.SinhCosh(sh, ch);
//	return Init(s.Mul(ch), c.Mul(sh))
//	} // Sin;
//	
//	
//	func /* (x: Complex) */ Cos* () -> Complex {
//	var
//	s,c,sh,ch: Real;
//	
//	s = toRadians(x.real);
//	s.SinCos(s, c); x.imag.SinhCosh(sh, ch);
//	return Init(c.Mul(ch), X.zero.Sub(s.Mul(sh)))
//	} // Cos;
//	
//	
//	func /* (x: Complex) */ SinCosC* (var sin, cos: Complex);
//	var
//	s,c,sh,ch: Real;
//	
//	s = toRadians(x.real);
//	s.SinCos(s, c); x.imag.SinhCosh(sh, ch);
//	sin = Init(s.Mul(ch), c.Mul(sh));
//	cos = Init(s.Mul(c), X.zero.Sub(sh.Mul(sh)))
//	} // SinCosC;
//	
//	
//	func /* (x: Complex) */ Tan* () -> Complex {
//	var TWO, d, s, c, sh, ch: Real;
//	
//	TWO = X.Long(2.0); s = toRadians(x.real);
//	s = TWO.Mul(s); sh = TWO.Mul(x.imag);
//	s.SinCos(s, c); sh.SinhCosh(sh, ch);
//	d = ch.Add(c);
//	return Init(s.Div(d), sh.Div(d))
//	} // Tan;
//	
//	
//	func CalcAlphaBeta (z : Complex; var a, b: Real);
//	var x, x2, y, r, t, HALF: Real;
//	
//	HALF = X.Long(0.5);
//	x = z.real.Add(X.one); x = x.Mul(x); y = z.imag.Mul(z.imag);
//	x2 = z.real.Sub(X.one); x2 = x2.Mul(x2);
//	t = x.Add(y); r = t.Sqrt(); t = x2.Add(y);
//	t = t.Sqrt();
//	a = HALF.Mul(r.Add(t));
//	b = r.Sub(t); b = b.Mul(HALF)
//	} // CalcAlphaBeta;
//	
//	
//	func /* (x: Complex) */ Arcsin* () -> Complex {
//	var a, b, t: Real;
//	
//	CalcAlphaBeta(x, a, b);
//	t = a.Mul(a); t = t.Sub(X.one); t = a.Add(t.Sqrt());
//	return Init(fromRadians(b.Arcsin()), t.Ln())
//	} // Arcsin;
//	
//	func /* (x: Complex) */ Arccos* () -> Complex {
//	var a, b, t: Real;
//	
//	CalcAlphaBeta(x, a, b);
//	t = a.Mul(a); t = t.Sub(X.one); t = a.Add(t.Sqrt());
//	return Init(fromRadians(b.Arccos()), X.zero.Sub(t.Ln()))
//	} // Arccos;
//	
//	
//	func (z : Complex) Arctan* () -> Complex {
//	var x, x2, y2, y, yp, TWO, HALF, QUARTER, t: Real;
//	
//	TWO = X.Long(2.0); HALF = X.Long(0.5); QUARTER = X.Long(0.25);
//	x = TWO.Mul(z.real); y = z.imag.Add(X.one); y = y.Mul(y);
//	yp = z.imag.Sub(X.one); yp = yp.Mul(yp);
//	x2 = z.real.Mul(z.real); y2 = z.imag.Mul(z.imag);
//	t = X.one.Sub(x2); t = t.Sub(y2); t = x.Div(t);
//	x = HALF.Mul(t.Arctan());
//	t = x2.Add(y); t = t.Div(x2.Add(yp));
//	y = QUARTER.Mul(t.Ln());
//	return Init(fromRadians(x), y)
//	} // Arctan;
//	
//	
//	func /* (x: Complex) */ Sinh* () -> Complex {
//	var
//	s1, c1, s2, c2: Real;
//	
//	x.real.SinhCosh(s1, c1); x.imag.SinhCosh(s2, c2);
//	return Init(s1.Mul(c2), c1.Mul(s2))
//	} // Sinh;
//	
//	
//	func /* (x: Complex) */ Cosh* () -> Complex {
//	var
//	s1, c1, s2, c2: Real;
//	
//	x.real.SinhCosh(s1, c1); x.imag.SinhCosh(s2, c2);
//	return Init(c1.Mul(c2), s1.Mul(s2))
//	} // Cosh;
//	
//	
//	func /* (x: Complex) */ SinhCoshC* (var sinh, cosh : Complex);
//	var
//	s1, c1, s2, c2: Real;
//	
//	x.real.SinhCosh(s1, c1); x.imag.SinhCosh(s2, c2);
//	sinh = Init(s1.Mul(c2), c1.Mul(s2));
//	cosh = Init(c1.Mul(c2), s1.Mul(s2))
//	} // SinhCoshC;
//	
//	
//	func /* (x: Complex) */ Tanh* () -> Complex {
//	var
//	a, b : Complex;
//	
//	x.SinhCoshC(a, b);
//	return a.Div(b)
//	} // Tanh;
//	
//	
//	func /* (x: Complex) */ Arcsinh* () -> Complex {
//	var
//	Temp : Complex;
//	
//	/* Result = ln(x + sqrt(x*x + 1)) */
//	Temp = one.Add(x.Mul(x)); Temp = x.Add(Temp.Sqrt());
//	return Temp.Ln()
//	} // Arcsinh;
//	
//	
//	func /* (x: Complex) */ Arccosh* () -> Complex {
//	var
//	Temp : Complex;
//	
//	/* Result = ln(x + sqrt(x*x - 1)) */
//	Temp = x.Mul(x); Temp = Temp.Sub(one); Temp = x.Add(Temp.Sqrt());
//	return Temp.Ln()
//	} // Arccosh;
//	
//	
//	func /* (x: Complex) */ Arctanh* () -> Complex {
//	var
//	Temp, Temp2, Half: Complex;
//	
//	/* Result = ln((1 + x) / (1 - x)) / 2 */
//	Temp = one.Add(x); Temp2 = one.Sub(x); Temp = Temp.Div(Temp2);
//	if X.err=0 {
//	Half = Long(0.5);
//	return Half.Mul(Temp.Ln())
//	} else { return zero
//	}
//	} // Arctanh;
//	
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
//	func Default*;
//	
//	/* set up default state */
//	nState.LocalBase = 10;
//	nState.Notation = Normal;
//	nState.DecPoint = 0;
//	nState.DegRadFlag = Degrees;
//	nState.DigSep = 0X;
//	nState.FracSep = 0X;
//	X.SetDigits(16);
//	nState.Rational = FALSE;
//	} // Default;
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
//	
//	/*
//	func Test;
//	var
//	x:Real; xi:Integer;
//	
//	xi = RealToInteger(X.pi);
//	Out.String("Real = "); Out.Object(X.pi); Out.String("; Int = ");
//	Out.Object(xi); Out.Ln;
//	
//	x = X.ToReal("1234567890.12345");
//	xi = RealToInteger(x);
//	Out.String("Real = "); Out.Object(x); Out.String("; Int = ");
//	Out.Object(xi); Out.Ln;
//	
//	x = X.ToReal("1234567890123456789012345678901234567890.12345");
//	xi = RealToInteger(x);
//	Out.String("Real = "); Out.Object(x); Out.String("; Int = ");
//	Out.Object(xi); Out.Ln;
//	
//	xi = XI.New("98765432109876543210", 10);
//	x = IntegerToReal(xi);
//	Out.String("Int = "); Out.Object(xi); Out.String("; Real = ");
//	Out.Object(x); Out.Ln;
//	} // Test;
//	*/
//	
////	
////	Default();
////	one = Init(X.one, X.zero);
////	zero = Init(X.zero, X.zero);
//}
