//
//  Equation.swift
//  XNumbers
//
//  Created by Mike Griebling on 11 Mar 2015.
//  Copyright (c) 2015 Computer Inspirations. All rights reserved.
//

import Foundation

class Equation {
	
 /*
	Equations - Equation evaluation.
	Copyright (c) 1996-2010 Michael Griebling
 
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
	
	let zero = Complex.zero
	
	var Token: Scanner.Tokens
	var CommandLine : String
	var LastAnswer: Complex
	
	init (command: String) {
		self.CommandLine = command
		self.LastAnswer = Complex.zero
		self.Token = .Empty
	}
	
	private func StoreVariable (Location: String, Value : Complex) {
		/* Store the `Value' argument in the `Location' variable. */
		Variables.Set(Location, value: Value)
	} // StoreVariable;
	
	private func Print (name: String, arg: String) {
		println("\(name) = \(arg)")
	} // Print;
	
	
	private func Printf (name: String, arg:String, value: String) {
		println("\(name)(\(arg)) = \(value)")
	} // Printf;
	
	
	private func Min (a: Complex, b: Complex) -> Complex {
		if a.Cmp(b) > 0 {
			return b
		} else {
			return a
		}
	} // Min;
	
	
	private func Max (a: Complex, b: Complex) -> Complex {
		if a.Cmp(b) > 0 {
			return a
		} else {
			return b
		}
	} // Max;


	func Permutations (n: Complex, _ r: Complex) -> Complex {
		/** Return the number of permutations of n different objects
		taken r at a time (i.e., n!/(n-r)! )
		*/
		var ni, ri: Integer
		var nI: Integer
		
		ni = ToInteger(n.real)
		ri = ToInteger(r.real)
		nI = ni.Factorial()
		nI = nI.Div(ni.Sub(ri).Factorial())
		return Complex(re: ToReal(nI), im: Real.zero)
	} // Permutations;
	
	
	func Combinations (n: Complex, r: Complex) -> Complex {
		/** Return the combinations of n different objects taken
		r at a time (i.e., n!/(r!(n-r)!))
		*/
		var ni, ri: Integer
		var nI: Integer
		var n2 : Complex
		
		ni = ToInteger(n.real)
		ri = ToInteger(r.real)
		n2 = Permutations(n, r)            /* n=n!/(n-r)! */
		nI = ri.Factorial()              /* n=r! */
		nI = ni.Div(nI);               /* Result=n!/(r!(n-r)!) */
		return Complex(re: ToReal(nI), im: Real.zero)
	} // Combinations;
	
	
	func Help () {
		println()
		println(" +, -, *, /      Addition/Subtraction/Multiplication/Division")
		println("  != , <>, =        Inequality, equality comparison")
		println(" >, >=           Greater than, Greater or equal comparison")
		println(" <, <=           Less than, Less or equal comparison")
		println(" true, false     Logical TRUE and FALSE")
		println(" if(exp; a; b)   If exp > 0 return a else return b")
		println(" ()              Brackets")
		println(" ^, **           Power")
		println(" %               x 0.01")
		println(" &, and          Logical And")
		println(" or, xor         Logical Inclusive/Exclusive Or")
		println(" !, not          Logical Complement")
		println(" mod, div        Modulo/Integer Division")
		println(" sqrt, cbrt      Square/Cube Root")
		println(" root            Any Root")
		println(" abs, |x|        Absolute value or complex magnitude")
		println(" rand            Random number between 0 and 1")
		println(" e               Euler's constant")
		println(" e^              Power of e")
		println(" i               Imaginary number")
		println(" angle           Complex angle")
		println(" re, im          Real/Imaginary part of a number")
		println(" rect            Polar to rectangle conversion")
		println(" conj            Complex conjugate")
		println(" nCr, nPr        Combinations/Permutations")
		println(" int, frac       Integer/Fractional part of a number")
		println(" sign            Sign of a number (-1 or 1)")
		println(" min(x;y;...z)   Minimum of x, y, ...z")
		println(" max(x;y;...z)   Maximum of x, y, ...z")
		println(" sum(x;y;...z)   Summation of x, y, ...z")
		println(" avg(x;y;...z)   Average of x, y, ...z")
		println(" mul(x;y;...z)   Multiply x, y, ...z")
		println(" ln, log         Natural, Base 10 Logarithm")
		println(" sin, asin       Sine, Arcsine")
		println(" cos, acos       Cosine, Arccosine")
		println(" tan, atan       Tangent, Arctangent")
		println(" sinh, asinh     Hyperbolic Sine, Arcsine")
		println(" cosh, acosh     Hyperbolic Cosine, Arccosine")
		println(" tanh, atanh     Hyperbolic Tangent, Arctangent")
		println(" sbit, cbit      Set/Clear Bit")
		println(" tbit            Toggle Bit")
		println(" shr, shl        Shift Right/Left")
		println(" <name>          Variable contents")
		println(" f(x;y;...z)     Evaluate function <f>")
		println(" let n=<exp>     Define variable 'n' with <exp>")
		println(" let f(x)=<exp>  Define function 'f' with args 'x' and expression <exp>")
		println(" del <name>      Deletes the variable, function")
		println(" list            Lists all defined variables, functions")
		println(" help            Displays this list")
		println(" pi              Constant Pi")
		println(" sci             Toggle Scientific/Engineering/Floating Point")
		println(" rat             Toggle Rational calculations on/off")
		println(" bas n           Change to Base n (n = 2..36)")
		println(" dig n           Use n Digits")
		println(" dp n            Use n Decimal Places")
		println(" drg             Toggle Degree/Radian/Grad")
	} // Help;
	
	func Function (fname: String) -> Complex {
		var arg, eqn: String
		var ok: Bool
		var nargs: Int
		var ptoken: Scanner.Tokens
		var res: Complex
		
		/* evaluate the passed function */
		Functions.Get(fname, eqn, ok);
		nargs = 0;
		res = Complex.zero;
		if Token == .LeftBrace {
			do {
				sc.Get(Token); Expression(res);
				f.GetArg(fname, nargs, arg, ok); INC(nargs);
				V.Setf(arg, res, ok)
			} while Token == .Semi
			if Token != .RightBrace { sc.Mark(.MismatchBraces) };
			if nargs != f.NumArgs(fname) { sc.Mark(.IncompatibleArgs) };
			sc.Get(Token);   /* skip right brace */
			
			/* now we finally evaluate the function */
			sc.PushState();                 /* save current expression's state */
			ptoken = Token;                  /* save the current token */
			Real.status = .Okay;             /* clear out any previous errors */
			Real.err  =  0;
			sc.Init(eqn); sc.Get(Token);    /* start things off with the first token */
			Expression(res);
			sc.PopState();                  /* restore our previous state */
			V.Deletef();                    /* clear function stack */
			Token = ptoken                   /* and active token */
		} else if f.NumArgs(fname) > 0 {
			sc.Mark(cli.ExpectingArgs);
			res = Complex.zero
		};
		return res
	} // Function;
	
	
//	func IfCondition () -> Complex {
//	/** Handle If(<expression>;<expression>;<expression>) */
//	var
//	cond, tval, fval: Complex;
//	
//	if Token = sc.LeftBrace {
//	sc.Get(Token); Expression(cond);
//	if Token  !=  sc.Semi { sc.Mark(cli.ExpectingArgs) };
//	sc.Get(Token); Expression(tval);
//	if Token  !=  sc.Semi { sc.Mark(cli.ExpectingArgs) };
//	sc.Get(Token); Expression(fval);
//	if Token  !=  sc.RightBrace { sc.Mark(X.MismatchBraces) };
//	sc.Get(Token);
//	if cond.Cmp(zero)>0 { return tval } else { return fval }
//	} else { return Complex.zero
//	}
//	} // IfCondition;
//	
//	
//	func Factor (var Result : Complex);
//	/** 'Result' = <[Factor Operator] (<Variable> | <Function> | "(" <Expression> ")" | <Number>) */
//	TYPE
//	funcType = func(x,y:Complex):Complex;
//	
//	var
//	SaveBase : SHORTINT;
//	tmp      : Complex;
//	t        : Real
//	i        : XI.Integer;
//	Digits   : Int
//	n        : Int
//	str      : String
//	ok       : Bool;
//	
//	func Add (x,y: Complex) -> Complex {
//	
//	return x.Add(y)
//	} // Add;
//	
//	func Avg (x,y: Complex) -> Complex {
//	
//	INC(n); return x.Add(y)
//	} // Avg;
//	
//	func Mul (x,y: Complex) -> Complex {
//	
//	return x.Mul(y)
//	} // Mul;
//	
//	func Next;
//	
//	sc.Get(Token);
//	if Token = sc.Minus {
//	sc.Get(Token); Factor(Result);
//	Result  =  Result.Neg()
//	} else {
//	Factor(Result)
//	}
//	} // Next;
//	
//	func Args (func: funcType) -> Complex {
//	var
//	tmp2: Complex;
//	
//	tmp = Complex.zero;
//	sc.Get(Token);
//	if Token != sc.LeftBrace { sc.Mark(X.MismatchBraces) };
//	sc.Get(Token); Expression(tmp);
//	if Token != sc.Semi { sc.Mark(cli.ExpectingArgs) };
//	do {
//	sc.Get(Token); Expression(tmp2);
//	tmp = func(tmp, tmp2)
//	} while ! Token != sc.Semi;
//	if Token != sc.RightBrace { sc.Mark(X.MismatchBraces) };
//	sc.Get(Token);
//	return tmp
//	} // Args;
//	
//	func FixR;
//	
//	Result = XM.Init(t, X.zero)
//	} // FixR;
//	
//	func FixI;
//	
//	Result = XM.Init(XM.IntegerToReal(i), X.zero)
//	} // FixI;
//	
//	
//	CASE Token OF
//	sc.LeftBrace  : sc.Get(Token); Expression(Result);
//	if Token = sc.RightBrace { sc.Get(Token);
//	} else { sc.Mark(X.MismatchBraces)
//	};
//	| sc.VertBrace  : sc.Get(Token); Expression(Result);
//	t = Result.PolarMag(); FixR;
//	if Token = sc.VertBrace { sc.Get(Token);
//	} else { sc.Mark(X.MismatchBraces)
//	};
//	| sc.Number     : if XM.nState.Rational { Result  =  R.RInit(sc.s.val.real, 1)
//	} else { Result  =  sc.s.val
//	};
//	sc.Get(Token);
//	/*
//	if Token = sc.Number {
//	Result = Result.Mul(sc.val);
//	sc.Get(Token)
//	} else if Token = sc.LeftBrace {
//	sc.Get(Token); Expression(tmp);
//	if Token = sc.RightBrace { sc.Get(Token);
//	} else { sc.Mark(X.MismatchBraces)
//	};
//	Result = Result.Mul(tmp)
//	}
//	*/
//	| sc.If         : sc.Get(Token); Result = IfCondition();
//	| sc.True       : sc.Get(Token); Result = XM.one
//	| sc.False      : sc.Get(Token); Result = Complex.zero
//	| sc.Pi         : sc.Get(Token); Result = XM.Init(X.pi, X.zero)
//	| sc.Complement : Next(); i = XM.RealToInteger(Result.real); i = i.Invert(); FixI
//	| sc.Sin        : Next(); Result = Result.Sin();
//	| sc.Cos        : Next(); Result = Result.Cos();
//	| sc.Tan        : Next(); Result = Result.Tan();
//	| sc.ArcSin     : Next(); Result = Result.Arcsin();
//	| sc.ArcCos     : Next(); Result = Result.Arccos();
//	| sc.ArcTan     : Next(); Result = Result.Arctan();
//	| sc.Sinh       : Next(); Result = Result.Sinh();
//	| sc.Cosh       : Next(); Result = Result.Cosh();
//	| sc.Tanh       : Next(); Result = Result.Tanh();
//	| sc.ArcSinh    : Next(); Result = Result.Arcsinh();
//	| sc.ArcCosh    : Next(); Result = Result.Arccosh();
//	| sc.ArcTanh    : Next(); Result = Result.Arctanh();
//	| sc.SquareRoot : Next(); Result = Result.Sqrt();
//	| sc.CubeRoot   : Next(); Result = Result.IRoot(3);
//	| sc.NaturalLog : Next(); Result = Result.Ln();
//	| sc.Log        : Next(); Result = Result.Log(X.Long(10));
//	| sc.PowerOfe   : Next(); Result = Result.Exp();
//	| sc.Name       : V.Get(sc.s.var, Result, ok);
//	if !ok {
//	/* check if this is a function */
//	f.Get(sc.s.var, str, ok);
//	if !ok {
//	sc.Mark(cli.Undefined)
//	} else {
//	/* evaluate function */
//	sc.Get(Token);
//	Result = Function(sc.s.var);
//	}
//	} else { sc.Get(Token);
//	if Token=sc.LeftBrace { /* duplicated name? */
//	f.Get(sc.s.var, str, ok);
//	if !ok {
//	sc.Mark(cli.Undefined)
//	} else {
//	/* evaluate function */
//	Result = Function(sc.s.var);
//	}
//	}
//	}
//	| sc.Base       : SaveBase  =  XM.nState.LocalBase;
//	XM.nState.LocalBase  =  10;
//	Next();
//	XM.nState.LocalBase  =  SHORT(SHORT(ENTIER(Result.real.Short())));
//	if (XM.nState.LocalBase < 2) OR (XM.nState.LocalBase > 36) {
//	XM.nState.LocalBase  =  SaveBase;
//	};
//	Result  =  LastAnswer;
//	| sc.Digits     : Next();
//	if X.status = X.Okay {
//	Digits = SHORT(ENTIER(Result.real.Short()));
//	X.SetDigits(Digits);
//	Result  =  LastAnswer;
//	};
//	| sc.Decimals   : Next();
//	if X.status = X.Okay {
//	XM.nState.DecPoint = SHORT(SHORT(ENTIER(Result.real.Short())));
//	Result  =  LastAnswer;
//	};
//	| sc.Notation   : sc.Get(Token);
//	XM.nState.Notation = (XM.nState.Notation+1) MOD 3;
//	Result  =  LastAnswer;
//	| sc.DegRadGrad : sc.Get(Token);
//	if XM.nState.DegRadFlag = XM.Gradians {
//	XM.nState.DegRadFlag  =  XM.Degrees;
//	} else { INC(XM.nState.DegRadFlag) };
//	Result  =  LastAnswer;
//	| sc.Rat		: sc.Get(Token);
//	XM.nState.Rational  =  !XM.nState.Rational;
//	Result  =  LastAnswer;
//	| sc.List       : sc.Get(Token);
//	if V.Defined()>0 {
//	println("Variables:")
//	V.Iterate(Print);
//	Out.Ln;
//	} else { println("No variables defined."); Out.Ln
//	};
//	if f.Defined()>0 {
//	println("Functions:")
//	f.Iterate(Printf);
//	} else { println("No functions defined."); Out.Ln
//	};
//	Result  =  LastAnswer
//	| sc.Help       : sc.Get(Token);
//	Help;
//	Result  =  Complex.zero
//	| sc.Delete     : sc.Get(Token);
//	if Token=sc.Name {
//	V.Delete(sc.s.var);
//	f.Delete(sc.s.var)
//	} else { sc.Mark(cli.IllegalVariable)
//	};
//	Result  =  Complex.zero; sc.Get(Token)
//	| sc.iToken     : sc.Get(Token); Result = XM.Init(X.zero, X.one)
//	| sc.rToken     : Next(); t = Result.PolarMag(); FixR
//	| sc.Theta      : Next(); t = Result.PolarAngle(); FixR
//	| sc.ImagPart   : Next(); Result = XM.Init(Result.imag, X.zero)
//	| sc.RealPart   : Next(); Result = XM.Init(Result.real, X.zero)
//	| sc.IntPart    : Next(); t = Result.real.Entier(); FixR
//	| sc.FracPart   : Next(); t = Result.real.Fraction(); FixR
//	| sc.SignOf     : Next();
//	if Result.real.Sign()>0 { Result = XM.Init(X.one, X.zero)
//	} else { Result = XM.Init(X.one.Neg(), X.zero)
//	}
//	| sc.Abs        : Next(); Result = XM.Init(Result.real.Abs(), X.zero)
//	| sc.Min        : Result = Args(Min)
//	| sc.Max        : Result = Args(Max)
//	| sc.Sum        : Result = Args(Add)
//	| sc.Average    : n = 1; tmp = Args(Avg); Result = tmp.Div(XM.Long(n))
//	| sc.Multiply   : Result = Args(Mul)
//	| sc.Conj       : Next(); Result = Result.Conj()
//	| sc.Rand       : sc.Get(Token); t = X.Random(); FixR
//	} else {              sc.Mark(X.IllegalOperator); Result  =  Complex.zero
//	};
//	if X.status != X.Okay { sc.Mark(X.status) }
//	} // Factor;
//	
//	
//	func Powers (var Result : Complex);
//	/** 'Result' = <Factor> @{<Power Operator> [Factor]@} */
//	var
//	tmp : Complex;
//	xtmp : Real
//	
//	func Next;
//	
//	sc.Get(Token);
//	if Token = sc.Minus {
//	sc.Get(Token); Factor(Result);
//	Result  =  Result.Neg()
//	} else {
//	Factor(Result)
//	};
//	} // Next;
//	
//	
//	tmp = Complex.zero;
//	Factor(tmp);
//	WHILE (Token>=sc.Power) & (Token<=sc.PolarToRect) DO
//	CASE Token OF
//	sc.Power     : Next(); tmp = tmp.Power(Result);
//	| sc.Root      : Next(); tmp = Result.Root(tmp);
//	| sc.Squared   : sc.Get(Token); tmp = tmp.Mul(tmp);
//	| sc.Cubed     : sc.Get(Token); tmp = tmp.IPower(3);
//	| sc.Inverse   : sc.Get(Token); tmp = XM.one.Div(tmp);
//	| sc.Factorial : sc.Get(Token);
//	xtmp = X.Factorial(ENTIER(tmp.real.Short()));
//	tmp = XM.Init(xtmp, X.zero);
//	| sc.PercentOf : sc.Get(Token);
//	Result = tmp.Div(XM.Init(X.Long(100), X.zero));
//	Factor(tmp);
//	tmp = tmp.Mul(Result);
//	| sc.PolarToRect:Next(); tmp = XM.ToRectangular(tmp.real, Result.real)
//	} else { /* skip */  sc.Mark(X.IllegalOperator); sc.Get(Token);
//	};
//	};
//	Result  =  tmp;
//	if X.status != X.Okay { sc.Mark(X.status) };
//	} // Powers;
//	
//	
//	func Term (var Result : Complex);
//	/** 'Result' = <Powers> @{<Term Operator> <Powers>@} */
//	var
//	tmp: Complex;
//	itmp, ti : XI.Integer;
//	
//	func Next;
//	
//	sc.Get(Token);
//	if Token = sc.Minus {
//	sc.Get(Token); Powers(Result);
//	Result  =  Result.Neg()
//	} else {
//	Powers(Result)
//	};
//	WITH Result: R.Rational DO
//	ti = XM.RealToInteger(Result.ToReal())
//	} else {
//	ti = XM.RealToInteger(Result.real)
//	}
//	} // Next;
//	
//	func ToCard(Ex : Complex) -> Int
//	
//	return SHORT(ENTIER(Ex.real.Short()));
//	} // ToCard;
//	
//	func Fix;
//	
//	tmp = XM.Init(XM.IntegerToReal(ti), X.zero)
//	} // Fix;
//	
//	
//	tmp = Complex.zero;
//	Powers(tmp);
//	WHILE (Token>=sc.Times) & (Token<=sc.nPr) OR (Token=sc.Number) DO
//	CASE Token OF
//	sc.Times       : Next(); tmp = tmp.Mul(Result)
//	| sc.Number      : tmp = tmp.Mul(Result); Next()
//	| sc.Divide      : Next(); tmp = tmp.Div(Result)
//	| sc.Div         : Next(); itmp = XM.RealToInteger(tmp.real); ti = itmp.Div(ti); Fix
//	| sc.Mod         : Next(); itmp = XM.RealToInteger(tmp.real); ti = itmp.Mod(ti); Fix
//	| sc.And         : Next(); itmp = XM.RealToInteger(tmp.real); ti = itmp.And(ti); Fix
//	| sc.ShiftRight  : Next(); itmp = XM.RealToInteger(tmp.real); ti = itmp.RShift(ToCard(Result)); Fix
//	| sc.AShiftRight : Next(); itmp = XM.RealToInteger(tmp.real); ti = itmp.RShift(ToCard(Result)); Fix
//	| sc.RotateRight : Next(); itmp = XM.RealToInteger(tmp.real); ti = itmp.RShift(ToCard(Result)); Fix
//	| sc.ShiftLeft   : Next(); itmp = XM.RealToInteger(tmp.real); ti = itmp.LShift(ToCard(Result)); Fix
//	| sc.RotateLeft  : Next(); itmp = XM.RealToInteger(tmp.real); ti = itmp.LShift(ToCard(Result)); Fix
//	| sc.ClearBit    : Next(); itmp = XM.RealToInteger(tmp.real); ti = itmp.ClearBit(ToCard(Result)); Fix
//	| sc.SetBit      : Next(); itmp = XM.RealToInteger(tmp.real); ti = itmp.SetBit(ToCard(Result)); Fix
//	| sc.ToggleBit   : Next(); itmp = XM.RealToInteger(tmp.real); ti = itmp.ToggleBit(ToCard(Result)); Fix
//	| sc.nCr         : Next(); tmp = Combinations(tmp, Result)
//	| sc.nPr         : Next(); tmp = Permutations(tmp, Result)
//	} else {
//	sc.Mark(X.IllegalOperator); sc.Get(Token)
//	}
//	};
//	Result  =  tmp;
//	if X.status != X.Okay { sc.Mark(X.status) };
//	} // Term;
//	
//	
//	func SimpleExpression (var Result : Complex);
//	/** 'Result' = [+|-] <Term> @{+|- <Term>@} */
//	var
//	tmp : Complex;
//	ti  : XI.Integer;
//	ires : XI.Integer;
//	
//	func Next(var Result : Complex);
//	
//	sc.Get(Token);
//	if Token = sc.Minus {
//	sc.Get(Token); Term(Result);
//	Result  =  Result.Neg()
//	} else {
//	Term(Result)
//	};
//	WITH tmp: R.Rational DO
//	ires = XM.RealToInteger(tmp.ToReal())
//	} else {
//	ires = XM.RealToInteger(tmp.real)
//	}
//	} // Next;
//	
//	func Fix;
//	
//	tmp = XM.Init(XM.IntegerToReal(ti), X.zero)
//	} // Fix;
//	
//	
//	tmp = Complex.zero;
//	CASE Token OF
//	sc.Plus  : Next(tmp);
//	| sc.Minus : Next(tmp); tmp = tmp.Neg()
//	} else {       Term(tmp)
//	};
//	WHILE (Token >= sc.Plus) & (Token <= sc.Xor) DO
//	CASE Token OF
//	sc.Plus  : Next(Result); tmp = tmp.Add(Result);
//	| sc.Minus : Next(Result); tmp = tmp.Sub(Result);
//	| sc.Or    : Next(Result); ti = ires.Or(XM.RealToInteger(Result.real)); Fix
//	| sc.Xor   : Next(Result); ti = ires.Xor(XM.RealToInteger(Result.real)); Fix
//	} else {         Term(tmp);
//	};
//	};
//	Result  =  tmp;
//	if X.status != X.Okay { sc.Mark(X.status) }
//	} // SimpleExpression;
//	
//	
//	func Expression (var Result : Complex);
//	/** 'Result' = <Expression> @{+|- <Term>@} */
//	var
//	tmp : Complex;
//	
//	func Next(var Result : Complex);
//	
//	sc.Get(Token);
//	SimpleExpression(Result)
//	} // Next;
//	
//	
//	SimpleExpression(tmp);
//	if (Token >= sc.Greater) & (Token <= sc.Assign) {
//	CASE Token OF
//	sc.Greater      : Next(Result);
//	if tmp.Cmp(Result)=1 { tmp = XM.one } else { tmp = zero }
//	| sc.GreaterEqual : Next(Result);
//	if tmp.Cmp(Result)>=0 { tmp = XM.one } else { tmp = zero }
//	| sc.Less         : Next(Result);
//	if tmp.Cmp(Result)<0 { tmp = XM.one } else { tmp = zero }
//	| sc.LessEqual    : Next(Result);
//	if tmp.Cmp(Result)<=0 { tmp = XM.one } else { tmp = zero }
//	| sc.NotEqual     : Next(Result);
//	if tmp.Cmp(Result) != 0 { tmp = XM.one } else { tmp = zero }
//	| sc.Assign       : Next(Result);
//	if tmp.Cmp(Result)=0 { tmp = XM.one } else { tmp = zero }
//	} else {                sc.Mark(X.IllegalOperator); sc.Get(Token)
//	};
//	};
//	Result = tmp;
//	if X.status != X.Okay { sc.Mark(X.status) }
//	} // Expression;
//	
//	
//	func Evaluate* (arg: String var Result: Complex): Bool;
//	/** Evaluate 'arg' input and return the result in 'Result'.  The
//	function returns true if there were no evaluation errors. */
//	var
//	name: String
//	ok: Bool;
//	r: Complex;
//	
//	CommandLine = arg;              /* remember this string for later        */
//	X.status  =  X.Okay;            /* clear out any previous errors         */
//	X.err  =  0;
//	sc.Init(arg); sc.Get(Token);   /* start things off with the first token */
//	
//	if Token = sc.Let {         /* define a variable or a function */
//	sc.Get(Token);
//	if Token  !=  sc.Name { sc.Mark(cli.ExpectingName); name = "Error"
//	} else { name = sc.s.var
//	};
//	sc.Get(Token);
//	if Token = sc.LeftBrace {
//	/* defining a function */
//	sc.Get(Token);
//	f.Set(name);                /* define a new function */
//	WHILE Token = sc.Name DO    /* define the argument list */
//	f.AddArg(name, sc.s.var, ok);
//	if !ok { sc.Mark(cli.IllegalArg) };
//	sc.Get(Token);
//	if Token = sc.Semi { sc.Get(Token) }
//	};
//	if Token  !=  sc.RightBrace { sc.Mark(cli.ExpectingRBrace) };
//	sc.Get(Token);
//	if Token  !=  sc.Assign { sc.Mark(cli.ExpectingAssign) };
//	f.SetEquation(name, sc.GetString());
//	r = Complex.zero
//	} else {
//	/* defining a variable */
//	if Token  !=  sc.Assign { sc.Mark(cli.ExpectingAssign) };
//	sc.Get(Token);
//	Expression(r);
//	StoreVariable(name, r)
//	};
//	Token = sc.Empty;
//	} else { Expression(r)
//	};
//	if Token != sc.Empty {
//	sc.Mark(cli.IllegalExpression)
//	};
//	Result = r;
//	LastAnswer = r;
//	return X.status=X.Okay
//	} // Evaluate;
//	
//	
//	func Store* (w: Storable.Writer) RAISES IO.Error;
//	/** Store the defined functions to writer 'w' */
//	
//	f.Store(w);
//	if LastAnswer IS R.Rational { w.WriteLInt(1) } else { w.WriteLInt(0) };
//	LastAnswer.Store(w)
//	} // Store;
//	
//	
//	func Load * (r: Storable.Reader) RAISES IO.Error;
//	/** Load functions from the reader 'r' */
//	var
//	type: Int
//	n: R.Rational;
//	
//	f.Load(r);
//	r.ReadLInt(type);
//	if type = 1 { NEW(n); n.Load(r); LastAnswer  =  n
//	} else { NEW(LastAnswer); LastAnswer.Load(r)
//	}
//	} // Load;
//	
//	
//	
//	Token = sc.Empty;
//	zero = XM.Init(X.zero, X.zero);
//	LastAnswer = XM.Init(X.zero, X.zero);
//	} // Equations.
	
	
}