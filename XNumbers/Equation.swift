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
	
	
	private func Printf (name: String, arg: String, value: Functions.FuncType) {
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


	private func Permutations (n: Complex, _ r: Complex) -> Complex {
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
	
	
	private func Combinations (n: Complex, _ r: Complex) -> Complex {
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
	
	private func Function (fname: String) -> Complex {
		var arg: String
		var ok: Bool
		var nargs: Int
		var ptoken: Scanner.Tokens
		var res: Complex
		
		/* evaluate the passed function */
		let eqn = Functions.Get(fname)
		nargs = 0
		res = Complex.zero
		if Token == .LeftBrace {
			do {
				Token = Scanner.Get(); res = Expression()
				if let arg = Functions.GetArg(fname, argNumber: nargs) {
					// nothing to do
				} else {
					arg = ""
				}
				++nargs
				Variables.Setf(arg, value: res)
			} while Token == .Semi
			if Token != .RightBrace { Scanner.Mark(.MismatchBraces) }
			if nargs != Functions.NumArgs(fname) { Scanner.Mark(.IncompatibleArgs) }
			Token = Scanner.Get()   /* skip right brace */
			
			/* now we finally evaluate the function */
			Scanner.PushState()                 /* save current expression's state */
			ptoken = Token                  /* save the current token */
			Real.status = .Okay             /* clear out any previous errors */
			Real.err = 0
			Scanner.Initialize(eqn!); Token = Scanner.Get()    /* start things off with the first token */
			res = Expression()
			Scanner.PopState()                  /* restore our previous state */
			Variables.Deletef()                    /* clear function stack */
			Token = ptoken                   /* and active token */
		} else if Functions.NumArgs(fname) > 0 {
			Scanner.Mark(.ExpectingArgs)
			res = Complex.zero
		}
		return res
	} // Function;
	
	
	private func IfCondition () -> Complex {
		/** Handle If(<expression>;<expression>;<expression>) */
		var cond, tval, fval: Complex
		
		if Token == .LeftBrace {
			Token = Scanner.Get(); cond = Expression()
			if Token != .Semi { Scanner.Mark(cli.ExpectingArgs) }
			Token = Scanner.Get(); tval = Expression()
			if Token != .Semi { Scanner.Mark(cli.ExpectingArgs) }
			Token = Scanner.Get(); fval = Expression()
			if Token != .RightBrace { Scanner.Mark(.MismatchBraces) }
			Token = Scanner.Get();
			if cond.Cmp(zero) > 0 { return tval } else { return fval }
		} else {
			return Complex.zero
		}
	} // IfCondition;
	
	
	private func Factor () -> Complex {
		/** 'Result' = <[Factor Operator] (<Variable> | <Function> | "(" <Expression> ")" | <Number>) */
		//		typeAlias funcType = (x:Complex, y:Complex) -> Complex
		
		var SaveBase : Int
		var tmp      : Complex
		var t        : Real
		var i        : Integer
		var Digits   : Int
		var n        : Int
		var str      : String
		var ok       : Bool
		var Result	 : Complex
		
		func Add (x: Complex, y: Complex) -> Complex {
			return x.Add(y)
		} // Add;
		
		func Avg (x: Complex, y: Complex) -> Complex {
			++n; return x.Add(y)
		} // Avg;
		
		func Mul (x: Complex, y: Complex) -> Complex {
			return x.Mul(y)
		} // Mul;
		
		func Next() {
			Token = Scanner.Get()
			if Token == .Minus {
				Token = Scanner.Get(); Result = -Factor()
			} else {
				Result = Factor()
			}
		} // Next;
		
		func Args (function: (x:Complex, y:Complex) -> Complex) -> Complex {
			var tmp2: Complex
			tmp = Complex.zero
			Token = Scanner.Get()
			if Token != .LeftBrace { Scanner.Mark(.MismatchBraces) }
			Token = Scanner.Get(); tmp = Expression()
			if Token != .Semi { Scanner.Mark(.ExpectingArgs) }
			do {
				Token = Scanner.Get(); tmp2 = Expression()
				tmp = function(x: tmp, y: tmp2)
			} while Token == .Semi
			if Token != .RightBrace { Scanner.Mark(.MismatchBraces) }
			Token = Scanner.Get()
			return tmp
		} // Args;
		
		func FixR () {
			Result = Complex(re: t, im: Real.zero)
		} // FixR;
		
		func FixI () {
			Result = Complex(re: ToReal(i), im: Real.zero)
		} // FixI;
		
		
		switch Token {
		case .LeftBrace :
			Token = Scanner.Get()
			Result = Expression()
			if Token == .RightBrace {
				Token = Scanner.Get()
			} else {
				Scanner.Mark(.MismatchBraces)
			}
		case .VertBrace :
			Token = Scanner.Get()
			Result = Expression()
			t = Result.PolarMag(); FixR()
			if Token == .VertBrace {
				Token = Scanner.Get()
			} else {
				Scanner.Mark(.MismatchBraces)
			}
		case .Number :
			if Complex.nState.Rational {
				Result = Complex(re: Scanner.s.val.real, im: Real.one)
			} else {
				Result = Scanner.s.val
			}
			Token = Scanner.Get()
			/*
			if Token = .Number {
			Result = Result.Mul(Scanner.val);
			Token = Scanner.Get()
			} else if Token = .LeftBrace {
			Token = Scanner.Get(); Expression(tmp);
			if Token = .RightBrace { Token = Scanner.Get();
			} else { Scanner.Mark(X.MismatchBraces)
			};
			Result = Result.Mul(tmp)
			}
			*/
		case .If         : Token = Scanner.Get(); Result = IfCondition();
		case .True       : Token = Scanner.Get(); Result = Complex.one
		case .False      : Token = Scanner.Get(); Result = Complex.zero
		case .Pi         : Token = Scanner.Get(); Result = Complex(re: Real.pi, im: Real.zero)
		case .Complement : Next(); i = ToInteger(Result.real); i = i.Invert(); FixI()
		case .Sin        : Next(); Result = Result.Sin()
		case .Cos        : Next(); Result = Result.Cos()
		case .Tan        : Next(); Result = Result.Tan()
		case .ArcSin     : Next(); Result = Result.Arcsin()
		case .ArcCos     : Next(); Result = Result.Arccos()
		case .ArcTan     : Next(); Result = Result.Arctan()
		case .Sinh       : Next(); Result = Result.Sinh()
		case .Cosh       : Next(); Result = Result.Cosh()
		case .Tanh       : Next(); Result = Result.Tanh()
		case .ArcSinh    : Next(); Result = Result.Arcsinh()
		case .ArcCosh    : Next(); Result = Result.Arccosh()
		case .ArcTanh    : Next(); Result = Result.Arctanh()
		case .SquareRoot : Next(); Result = Result.Sqrt()
		case .CubeRoot   : Next(); Result = Result.IRoot(3)
		case .NaturalLog : Next(); Result = Result.Ln()
		case .Log        : Next(); Result = Result.Log(Complex(fromDouble: 10))
		case .PowerOfe   : Next(); Result = Result.Exp()
		case .Name       :
			if var temp = Variables.Get(Scanner.s.varn) {
				Token = Scanner.Get()
				if Token == .LeftBrace { /* duplicated name? */
					if let str = Functions.Get(Scanner.s.varn) {
						/* evaluate function */
						temp = Function(Scanner.s.varn)
					} else {
						Scanner.Mark(.Undefined)
					}
				}
				Result = temp
			} else {
				/* check if this is a function */
				if let str = Functions.Get(Scanner.s.varn) {
					/* evaluate function */
					Token = Scanner.Get();
					Result = Function(Scanner.s.varn)
				} else {
					Scanner.Mark(.Undefined)
				}
			}
		case .Base :
			SaveBase = Complex.nState.LocalBase
			Complex.nState.LocalBase = 10
			Next();
			Complex.nState.LocalBase = Int(Result.real.Short())
			if (Complex.nState.LocalBase < 2) || (Complex.nState.LocalBase > 36) {
				Complex.nState.LocalBase = SaveBase
			}
			Result = LastAnswer
		case .Digits :
			Next()
			if Real.status == .Okay {
				Digits = Int(Result.real.Short())
				Real.digits = Digits
				Result = LastAnswer
			}
		case .Decimals :
			Next()
			if Real.status == .Okay {
				Complex.nState.DecPoint = Int(Result.real.Short())
				Result = LastAnswer
			}
		case .Notation :
			Token = Scanner.Get();
			switch Complex.nState.Notation {
				case .Normal: Complex.nState.Notation  = .Scientific
				case .Scientific: Complex.nState.Notation  = .Engineering
				case .Engineering: Complex.nState.Notation  = .Normal
			}
			Result = LastAnswer
		case .DegRadGrad :
			Token = Scanner.Get()
			switch Complex.nState.DegRadFlag {
				case .Degrees: Complex.nState.DegRadFlag = .Radians
				case .Radians: Complex.nState.DegRadFlag = .Gradians
				case .Gradians: Complex.nState.DegRadFlag = .Degrees
			}
			Result = LastAnswer
		case .Rat :
			Token = Scanner.Get()
			Complex.nState.Rational = !Complex.nState.Rational
			Result = LastAnswer
		case .List :
			Token = Scanner.Get()
			if Variables.Defined() > 0 {
				println("Variables:")
				Variables.Iterate(Print)
				println()
			} else {
				println("No variables defined.")
			}
			if Functions.Defined() > 0 {
				println("Functions:")
				Functions.Iterate(Printf)
			} else {
				println("No functions defined.")
			}
			Result = LastAnswer
		case .Help :
			Token = Scanner.Get();
			Help()
			Result = Complex.zero
		case .Delete :
			Token = Scanner.Get();
			if Token == .Name {
				Variables.Delete(Scanner.s.varn)
				Functions.Delete(Scanner.s.varn)
			} else {
				Scanner.Mark(cli.IllegalVariable)
			}
			Result = Complex.zero; Token = Scanner.Get()
		case .iToken     : Token = Scanner.Get(); Result = Complex(re: Real.zero, im:Real.one)
		case .rToken     : Next(); t = Result.PolarMag(); FixR()
		case .Theta      : Next(); t = Result.PolarAngle(); FixR()
		case .ImagPart   : Next(); Result = Complex(re: Result.imag, im:Real.zero)
		case .RealPart   : Next(); Result = Complex(re: Result.real, im:Real.zero)
		case .IntPart    : Next(); t = Result.real.Entier(); FixR()
		case .FracPart   : Next(); t = Result.real.Fraction(); FixR()
		case .SignOf     :
			Next();
			if Result.real.Sign() > 0 {
				Result = Complex(re: Real.one, im: Real.zero)
			} else {
				Result = Complex(re: -Real.one, im: Real.zero)
			}
		case .Abs        : Next(); Result = Complex(re: Result.real.Abs(), im: Real.zero)
		case .Min        : Result = Args(Min)
		case .Max        : Result = Args(Max)
		case .Sum        : Result = Args(Add)
		case .Average    : n = 1; tmp = Args(Avg); Result = tmp.Div(Complex(fromDouble: Double(n)))
		case .Multiply   : Result = Args(Mul)
		case .Conj       : Next(); Result = Result.Conj()
		case .Rand       : Token = Scanner.Get(); t = t.Random(); FixR()
		default: Scanner.Mark(.IllegalOperator); Result = Complex.zero
		}
		if Real.status != .Okay { Scanner.Mark(Real.status) }
		return Result
	} // Factor;
	
	
	private func Powers () -> Complex {
		/** 'Result' = <Factor> @{<Power Operator> [Factor]@} */
		var tmp : Complex
		var xtmp : Real
		var Result : Complex
		
		func Next () {
			Token = Scanner.Get()
			if Token == .Minus {
				Token = Scanner.Get(); Result = Factor()
				Result = -Result
			} else {
				Result = Factor()
			}
		} // Next;
		
		tmp = Factor()
		while (Token >= .Power && Token <= .PolarToRect) {
			switch Token {
			case .Power     : Next(); tmp = tmp.Power(Result)
			case .Root      : Next(); tmp = Result.Root(tmp)
			case .Squared   : Token = Scanner.Get(); tmp = tmp.Mul(tmp)
			case .Cubed     : Token = Scanner.Get(); tmp = tmp.IPower(3)
			case .Inverse   : Token = Scanner.Get(); tmp = Complex.one.Div(tmp)
			case .Factorial : Token = Scanner.Get();
				xtmp = tmp.real.Factorial()
				tmp = Complex(re: xtmp, im: Real.zero)
			case .PercentOf : Token = Scanner.Get()
				Result = tmp.Div(Complex(fromDouble: 100))
				tmp = Factor()
				tmp = tmp.Mul(Result);
			case .PolarToRect:Next(); tmp = Complex(r: tmp.real, theta: Result.real)
			default:  /* skip */  Scanner.Mark(.IllegalOperator); Token = Scanner.Get()
			}
		}
		if Real.status != .Okay { Scanner.Mark(Real.status) }
		return tmp
	} // Powers;
	
	
	private func Term () -> Complex {
		/** 'Result' = <Powers> @{<Term Operator> <Powers>@} */
		var tmp: Complex
		var itmp, ti : Integer
		var Result : Complex
		
		func Next () {
			Token = Scanner.Get();
			if Token == .Minus {
				Token = Scanner.Get(); Result = Powers()
				Result = -Result
			} else {
				Result = Powers()
			}
			ti = ToInteger(Result.real)
		} // Next;
		
		func ToCard(Ex : Complex) -> Int {
			return Int(Ex.real.Short())
		} // ToCard;
		
		func Fix () {
			tmp = Complex(re: ToReal(ti), im: Real.zero)
		} // Fix;
		
		tmp = Powers()
		while (Token >= .Times && Token <= .nPr) || (Token == .Number) {
			switch Token {
			case .Times       : Next(); tmp = tmp.Mul(Result)
			case .Number      : tmp = tmp.Mul(Result); Next()
			case .Divide      : Next(); tmp = tmp.Div(Result)
			case .Div         : Next(); itmp = ToInteger(tmp.real); ti = itmp.Div(ti); Fix()
			case .Mod         : Next(); itmp = ToInteger(tmp.real); ti = itmp.Mod(ti); Fix()
			case .And         : Next(); itmp = ToInteger(tmp.real); ti = itmp.And(ti); Fix()
			case .ShiftRight  : Next(); itmp = ToInteger(tmp.real); ti = itmp.RShift(ToCard(Result)); Fix()
			case .AShiftRight : Next(); itmp = ToInteger(tmp.real); ti = itmp.RShift(ToCard(Result)); Fix()
			case .RotateRight : Next(); itmp = ToInteger(tmp.real); ti = itmp.RShift(ToCard(Result)); Fix()
			case .ShiftLeft   : Next(); itmp = ToInteger(tmp.real); ti = itmp.LShift(ToCard(Result)); Fix()
			case .RotateLeft  : Next(); itmp = ToInteger(tmp.real); ti = itmp.LShift(ToCard(Result)); Fix()
			case .ClearBit    : Next(); itmp = ToInteger(tmp.real); ti = itmp.ClearBit(ToCard(Result)); Fix()
			case .SetBit      : Next(); itmp = ToInteger(tmp.real); ti = itmp.SetBit(ToCard(Result)); Fix()
			case .ToggleBit   : Next(); itmp = ToInteger(tmp.real); ti = itmp.ToggleBit(ToCard(Result)); Fix()
			case .nCr         : Next(); tmp = Combinations(tmp, Result)
			case .nPr         : Next(); tmp = Permutations(tmp, Result)
			default: 			Scanner.Mark(.IllegalOperator); Token = Scanner.Get()
			}
		}
		if Real.status != .Okay { Scanner.Mark(Real.status) }
		return tmp
	} // Term;
	
	
	private func SimpleExpression () -> Complex {
		/** 'Result' = [+|-] <Term> @{+|- <Term>@} */
		var tmp : Complex
		var ti  : Integer
		var ires : Integer
		var Result : Complex
		
		func Next() -> Complex {
			var Result : Complex
			Token = Scanner.Get()
			if Token == .Minus {
				Token = Scanner.Get(); Result = -Term()
			} else {
				Result = Term()
			}
			ires = ToInteger(tmp.real)
			return Result
		} // Next;
		
		func Fix() {
			tmp = Complex(re: ToReal(ti), im: Real.zero)
		} // Fix;
		
		tmp = Complex.zero
		switch Token {
			case .Plus  : tmp = Next()
			case .Minus : tmp = -Next()
			default:      tmp = Term()
		}
		while (Token >= .Plus) && (Token <= .Xor) {
			switch Token {
				case .Plus  : Result = Next(); tmp = tmp.Add(Result)
				case .Minus : Result = Next(); tmp = tmp.Sub(Result)
				case .Or    : Result = Next(); ti = ires.Or(ToInteger(Result.real)); Fix()
				case .Xor   : Result = Next(); ti = ires.Xor(ToInteger(Result.real)); Fix()
				default		: tmp = Term()
			}
		}
		if Real.status != .Okay { Scanner.Mark(Real.status) }
		return tmp
	} // SimpleExpression;
	
	
	private func Expression () -> Complex {
		/** 'Result' = <Expression> @{+|- <Term>@} */
		var tmp : Complex
		var Result : Complex
		
		func Next() -> Complex {
			Token = Scanner.Get()
			return SimpleExpression()
		} // Next;
		
		tmp = SimpleExpression()
		if (Token >= .Greater) && (Token <= .Assign) {
			switch Token {
			case .Greater : Result = Next()
				tmp = tmp > Result ? Complex.one : zero
			case .GreaterEqual : Result = Next()
				tmp = tmp >= Result ? Complex.one : zero
			case .Less : Result = Next()
				tmp = tmp < Result ? Complex.one : zero
			case .LessEqual : Result = Next()
				tmp = tmp <= Result ? Complex.one : zero
			case .NotEqual : Result = Next()
				tmp = tmp != Result ? Complex.one : zero
			case .Assign : Result = Next()
				tmp = tmp == Result ? Complex.one : zero
			default: Scanner.Mark(.IllegalOperator); Token = Scanner.Get()
			}
		}
		if Real.status != .Okay { Scanner.Mark(Real.status) }
		return tmp
	} // Expression;
	
	
	func Evaluate (arg: String ) -> Complex? {
		/** Evaluate 'arg' input and return the result in 'Result'.  The
		function returns true if there were no evaluation errors. */
		var name: String
		var ok: Bool;
		var r: Complex;
		var Result: Complex
		
		CommandLine = arg              /* remember this string for later        */
		Real.status  = .Okay           /* clear out any previous errors         */
		Real.err = 0;
		Scanner.Initialize(arg); Token = Scanner.Get()   /* start things off with the first token */
		
		if Token == .Let {         /* define a variable or a function */
			Token = Scanner.Get();
			if Token != .Name {
				Scanner.Mark(cli.ExpectingName); name = "Error"
			} else {
				name = Scanner.s.varn
			}
			Token = Scanner.Get()
			if Token == .LeftBrace {
				/* defining a function */
				Token = Scanner.Get();
				Functions.Set(name)       /* define a new function */
				while Token == .Name {    /* define the argument list */
					Functions.AddArg(name, name: Scanner.s.varn)
					Token = Scanner.Get()
					if Token == .Semi { Token = Scanner.Get() }
				}
				if Token != .RightBrace { Scanner.Mark(cli.ExpectingRBrace) }
				Token = Scanner.Get()
				if Token != .Assign { Scanner.Mark(cli.ExpectingAssign) }
				Functions.SetEquation(name, value: Scanner.GetString())
				r = Complex.zero
			} else {
				/* defining a variable */
				if Token != .Assign { Scanner.Mark(cli.ExpectingAssign) }
				Token = Scanner.Get()
				r = Expression()
				StoreVariable(name, Value: r)
			}
			Token = .Empty
		} else {
			r = Expression()
		}
		if Token != .Empty {
			Scanner.Mark(cli.IllegalExpression)
		}
		LastAnswer = r
		if Real.status == .Okay {
			return r
		}
		return nil
	} // Evaluate;
	
	
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
//	if type = 1 { NEW(n); n.Load(r); LastAnswer = n
//	} else { NEW(LastAnswer); LastAnswer.Load(r)
//	}
//	} // Load;
	
}