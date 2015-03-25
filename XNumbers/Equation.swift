//
//  Equation.swift
//  XNumbers
//
//  Created by Mike Griebling on 11 Mar 2015.
//  Copyright (c) 2015 Computer Inspirations. All rights reserved.
//

import Foundation

class Equation : NSCoding {
	
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
	
	let zero = xNumber.zero
	let one = xNumber.one
	
	var Token: Scanner.Tokens
	var CommandLine : String
	var LastAnswer: xNumber
	
	init (command: String) {
		self.CommandLine = command
		self.LastAnswer = zero
		self.Token = .Empty
	}
	
	required init (coder decoder: NSCoder) {
		self.Token = .Empty
		self.LastAnswer = decoder.decodeObject() as xNumber
		self.CommandLine = decoder.decodeObject() as String
		nState = NumbState(decoder: decoder)
		Functions.Load(decoder)
		Variables.Load(decoder)
	}
	
	func encodeWithCoder (encoder: NSCoder) {
		encoder.encodeObject(self.LastAnswer)
		encoder.encodeObject(self.CommandLine)
		nState.Save(encoder)
		Functions.Save(encoder)
		Variables.Save(encoder)
	}
	
	private func StoreVariable (Location: String, Value : xNumber) {
		/* Store the `Value' argument in the `Location' variable. */
		Variables.Set(Location, value: Value)
	} // StoreVariable;
	
	private func Print (name: String, arg: String) {
		println("\(name) = \(arg)")
	} // Print;
	
	
	private func Printf (name: String, arg: String, value: Functions.FuncType) {
		println("\(name)(\(arg)) = \(value)")
	} // Printf;
	
	
	private func Min (a: xNumber, b: xNumber) -> xNumber {
		return a > b ? b : a
	} // Min;
	
	
	private func Max (a: xNumber, b: xNumber) -> xNumber {
		return a > b ? a : b
	} // Max;


	private func Permutations (n: xNumber, _ r: xNumber) -> xNumber {
		/** Return the number of permutations of n different objects
		taken r at a time (i.e., n!/(n-r)! )
		*/
		return n.permutations(r)
	} // Permutations;
	
	
	private func Combinations (n: xNumber, _ r: xNumber) -> xNumber {
		/** Return the combinations of n different objects taken
		r at a time (i.e., n!/(r!(n-r)!))
		*/
		return n.combinations(r)
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
		println(" mod, div        Modulo/xNumber Division")
		println(" sqrt, cbrt      Square/Cube Root")
		println(" root            Any Root")
		println(" abs, |x|        Absolute value or complex magnitude")
		println(" rand            Random number between 0 and 1")
		println(" e               Euler's constant")
		println(" e^              Power of e")
		println(" i               Imaginary number")
		println(" angle           xNumber angle")
		println(" re, im          xNumber/Imaginary part of a number")
		println(" rect            Polar to rectangle conversion")
		println(" conj            xNumber conjugate")
		println(" nCr, nPr        Combinations/Permutations")
		println(" int, frac       xNumber/Fractional part of a number")
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
	
	private func Function (fname: String) -> xNumber {
		var arg: String = ""
		var ok: Bool
		var nargs: Int
		var ptoken: Scanner.Tokens
		var res: xNumber
		
		/* evaluate the passed function */
		let eqn = Functions.Get(fname)
		nargs = 0
		res = zero
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
			status = .Okay             /* clear out any previous errors */
			xNumber.setErr(0)
			Scanner.Initialize(eqn!); Token = Scanner.Get()    /* start things off with the first token */
			res = Expression()
			Scanner.PopState()                  /* restore our previous state */
			Variables.Deletef()                    /* clear function stack */
			Token = ptoken                   /* and active token */
		} else if Functions.NumArgs(fname) > 0 {
			Scanner.Mark(.ExpectingArgs)
			res = zero
		}
		return res
	} // Function;
	
	
	private func IfCondition () -> xNumber {
		/** Handle If(<expression>;<expression>;<expression>) */
		var cond, tval, fval: xNumber
		
		if Token == .LeftBrace {
			Token = Scanner.Get(); cond = Expression()
			if Token != .Semi { Scanner.Mark(.ExpectingArgs) }
			Token = Scanner.Get(); tval = Expression()
			if Token != .Semi { Scanner.Mark(.ExpectingArgs) }
			Token = Scanner.Get(); fval = Expression()
			if Token != .RightBrace { Scanner.Mark(.MismatchBraces) }
			Token = Scanner.Get();
			if cond.cmp(zero) > 0 { return tval } else { return fval }
		} else {
			return zero
		}
	} // IfCondition;
	
	
	private func Factor () -> xNumber {
		/** 'Result' = <[Factor Operator] (<Variable> | <Function> | "(" <Expression> ")" | <Number>) */
		//		typeAlias funcType = (x:xNumber, y:xNumber) -> xNumber
		
		var SaveBase : Int
		var tmp      : xNumber = zero
		var t        : xNumber = zero
		var i        : xNumber = zero
		var Digits   : Int
		var n        : Int = 0
		var str      : String
		var ok       : Bool
		var Result	 : xNumber = zero
		
		func Add (x: xNumber, y: xNumber) -> xNumber {
			return x.add(y)
		} // Add;
		
		func Avg (x: xNumber, y: xNumber) -> xNumber {
			++n; return x.add(y)
		} // Avg;
		
		func Mul (x: xNumber, y: xNumber) -> xNumber {
			return x.mul(y)
		} // Mul;
		
		func Next() {
			Token = Scanner.Get()
			if Token == .Minus {
				Token = Scanner.Get(); Result = Factor().negate()
			} else {
				Result = Factor()
			}
		} // Next;
		
		func Args (function: (x:xNumber, y:xNumber) -> xNumber) -> xNumber {
			var tmp2: xNumber
			tmp = zero
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
			Result = Result.polarMag()
			if Token == .VertBrace {
				Token = Scanner.Get()
			} else {
				Scanner.Mark(.MismatchBraces)
			}
		case .Empty : break  // do nothing
		case .Number :
			Result = Scanner.s.val
			Token = Scanner.Get()
		case .If         : Token = Scanner.Get(); Result = IfCondition();
		case .True       : Token = Scanner.Get(); Result = one
		case .False      : Token = Scanner.Get(); Result = zero
		case .Pi         : Token = Scanner.Get(); Result = xNumber.pi
		case .Complement : Next(); Result = Result.onesComp()
		case .Sin        : Next(); Result = Result.sin()
		case .Cos        : Next(); Result = Result.cos()
		case .Tan        : Next(); Result = Result.tan()
		case .ArcSin     : Next(); Result = Result.arcsin()
		case .ArcCos     : Next(); Result = Result.arccos()
		case .ArcTan     : Next(); Result = Result.arctan()
		case .Sinh       : Next(); Result = Result.sinh()
		case .Cosh       : Next(); Result = Result.cosh()
		case .Tanh       : Next(); Result = Result.tanh()
		case .ArcSinh    : Next(); Result = Result.arcsinh()
		case .ArcCosh    : Next(); Result = Result.arccosh()
		case .ArcTanh    : Next(); Result = Result.arctanh()
		case .SquareRoot : Next(); Result = Result.sqrt()
		case .CubeRoot   : Next(); Result = Result.iroot(3)
		case .NaturalLog : Next(); Result = Result.ln()
		case .Log        : Next(); Result = Result.log10()
		case .PowerOfe   : Next(); Result = Result.exp()
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
			SaveBase = nState.LocalBase
			nState.LocalBase = 10
			Next();
			nState.LocalBase = Int(Result.Short())
			if (nState.LocalBase < 2) || (nState.LocalBase > 36) {
				nState.LocalBase = SaveBase
			}
			Result = LastAnswer
		case .Digits :
			Next()
			if status == .Okay {
				Digits = Int(Result.Short())
				xNumber.setDigits(Digits)
				Result = LastAnswer
			}
		case .Decimals :
			Next()
			if status == .Okay {
				nState.DecPoint = Int(Result.Short())
				Result = LastAnswer
			}
		case .Notation :
			Token = Scanner.Get();
			switch nState.Notation {
				case .Normal: nState.Notation  = .Scientific
				case .Scientific: nState.Notation  = .Engineering
				case .Engineering: nState.Notation  = .Normal
			}
			Result = LastAnswer
		case .DegRadGrad :
			Token = Scanner.Get()
			switch nState.DegRadFlag {
				case .Degrees: nState.DegRadFlag = .Radians
				case .Radians: nState.DegRadFlag = .Gradians
				case .Gradians: nState.DegRadFlag = .Degrees
			}
			Result = LastAnswer
		case .Rat :
			Token = Scanner.Get()
			nState.Rational = !nState.Rational
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
			Token = Scanner.Get()
			Help()
			Result = zero
		case .Delete :
			Token = Scanner.Get();
			if Token == .Name {
				Variables.Delete(Scanner.s.varn)
				Functions.Delete(Scanner.s.varn)
			} else {
				Scanner.Mark(.IllegalVariable)
			}
			Result = zero; Token = Scanner.Get()
		case .iToken     : Token = Scanner.Get(); Result = xNumber(real: zero, andImaginary: one)
		case .rToken     : Next(); Result = Result.polarMag()
		case .Theta      : Next(); Result = Result.polarAngle()
		case .ImagPart   : Next(); Result = Result.imaginary()
		case .RealPart   : Next(); Result = Result.real()
		case .IntPart    : Next(); Result = Result.entier()
		case .FracPart   : Next(); Result = Result.fraction()
		case .SignOf     : Next(); Result = xNumber(int: Result.sign())
		case .Abs        : Next(); Result = Result.abs()
		case .Min        : Result = Args(Min)
		case .Max        : Result = Args(Max)
		case .Sum        : Result = Args(Add)
		case .Average    : n = 1; tmp = Args(Avg); Result = tmp.div(xNumber(int: n))
		case .Multiply   : Result = Args(Mul)
		case .Conj       : Next(); Result = Result.conj()
		case .Rand       : Token = Scanner.Get(); Result = xNumber.random()
		default: Scanner.Mark(.IllegalOperator); Result = zero
		}
		if status != .Okay { Scanner.Mark(status) }
		return Result
	} // Factor;
	
	
	private func Powers () -> xNumber {
		/** 'Result' = <Factor> @{<Power Operator> [Factor]@} */
		var tmp : xNumber
		var xtmp : xNumber
		var Result = zero
		
		func Next () {
			Token = Scanner.Get()
			if Token == .Minus {
				Token = Scanner.Get(); Result = Factor()
				Result = Result.negate()
			} else {
				Result = Factor()
			}
		} // Next;
		
		tmp = Factor()
		while (Token >= .Power && Token <= .PolarToRect) {
			switch Token {
			case .Power     : Next(); tmp = tmp.power(Result)
			case .Root      : Next(); tmp = Result.root(tmp)
			case .Squared   : Token = Scanner.Get(); tmp = tmp.mul(tmp)
			case .Cubed     : Token = Scanner.Get(); tmp = tmp.ipower(3)
			case .Inverse   : Token = Scanner.Get(); tmp = one.div(tmp)
			case .Factorial : Token = Scanner.Get();
				xtmp = tmp.factorial()
				tmp = xNumber(real: xtmp, andImaginary: zero)
			case .PercentOf : Token = Scanner.Get()
				Result = tmp.div(xNumber(int: 100))
				tmp = Factor()
				tmp = tmp.mul(Result);
			case .PolarToRect:Next(); tmp = xNumber(magnitude: tmp, andAngle: Result)
			default:  /* skip */  Scanner.Mark(.IllegalOperator); Token = Scanner.Get()
			}
		}
		if status != .Okay { Scanner.Mark(status) }
		return tmp
	} // Powers;
	
	
	private func Term () -> xNumber {
		/** 'Result' = <Powers> @{<Term Operator> <Powers>@} */
		var tmp: xNumber = zero
		var Result: xNumber = zero
		
		func Next () {
			Token = Scanner.Get();
			if Token == .Minus {
				Token = Scanner.Get(); Result = Powers()
				Result = Result.negate()
			} else {
				Result = Powers()
			}
		} // Next;
		
		func ToCard(Ex : xNumber) -> Int {
			return Int(Ex.Short())
		} // ToCard;
		
		tmp = Powers()
		while (Token >= .Times && Token <= .nPr) || (Token == .Number) {
			switch Token {
			case .Times       : Next(); tmp = tmp.mul(Result)
			case .Number      : tmp = tmp.mul(Scanner.s.val); Next()
			case .Divide      : Next(); tmp = tmp.div(Result)
			case .Div         : Next(); tmp = tmp.div(Result)
			case .Mod         : Next(); tmp = tmp.mod(Result)
			case .And         : Next(); tmp = tmp.and(Result)
			case .ShiftRight  : Next(); tmp = tmp.shr(ToCard(Result))
			case .AShiftRight : Next(); tmp = tmp.shr(ToCard(Result))
			case .RotateRight : Next(); tmp = tmp.shr(ToCard(Result))
			case .ShiftLeft   : Next(); tmp = tmp.shl(ToCard(Result))
			case .RotateLeft  : Next(); tmp = tmp.shl(ToCard(Result))
			case .ClearBit    : Next(); tmp = tmp.clearBit(ToCard(Result))
			case .SetBit      : Next(); tmp = tmp.setBit(ToCard(Result))
			case .ToggleBit   : Next(); tmp = tmp.toggleBit(ToCard(Result))
			case .nCr         : Next(); tmp = Combinations(tmp, Result)
			case .nPr         : Next(); tmp = Permutations(tmp, Result)
			default: 			Scanner.Mark(.IllegalOperator); Token = Scanner.Get()
			}
		}
		if status != .Okay { Scanner.Mark(status) }
		return tmp
	} // Term;
	
	
	private func SimpleExpression () -> xNumber {
		/** 'Result' = [+|-] <Term> @{+|- <Term>@} */
		var tmp = zero
		var Result = zero
		
		func Next() {
			Token = Scanner.Get()
			if Token == .Minus {
				Token = Scanner.Get(); Result = Term().negate()
			} else {
				Result = Term()
			}
		} // Next;
		
		switch Token {
		case .Plus  : Next(); tmp = Result
		case .Minus : Next(); tmp = Result.negate()
		default:      tmp = Term()
		}
		while (Token >= .Plus) && (Token <= .Xor) {
			switch Token {
				case .Plus  : Next(); tmp = tmp.add(Result)
				case .Minus : Next(); tmp = tmp.sub(Result)
				case .Or    : Next(); tmp = tmp.or(Result)
				case .Xor   : Next(); tmp = tmp.xor(Result)
				default		: tmp = Term()
			}
		}
		if status != .Okay { Scanner.Mark(status) }
		return tmp
	} // SimpleExpression;
	
	
	private func Expression () -> xNumber {
		/** 'Result' = <Expression> @{+|- <Term>@} */
		var tmp : xNumber
		var Result : xNumber
		
		func Next() -> xNumber {
			Token = Scanner.Get()
			return SimpleExpression()
		} // Next;
		
		tmp = SimpleExpression()
		if (Token >= .Greater) && (Token <= .Assign) {
			switch Token {
			case .Greater : Result = Next()
				tmp = tmp > Result ? one : zero
			case .GreaterEqual : Result = Next()
				tmp = tmp >= Result ? one : zero
			case .Less : Result = Next()
				tmp = tmp < Result ? one : zero
			case .LessEqual : Result = Next()
				tmp = tmp <= Result ? one : zero
			case .NotEqual : Result = Next()
				tmp = tmp != Result ? one : zero
			case .Assign : Result = Next()
				tmp = tmp == Result ? one : zero
			default: Scanner.Mark(.IllegalOperator); Token = Scanner.Get()
			}
		}
		if status != .Okay { Scanner.Mark(status) }
		return tmp
	} // Expression;
	
	
	func Evaluate (arg: String ) -> xNumber? {
		/** Evaluate 'arg' input and return the result in 'Result'.  The
		function returns true if there were no evaluation errors. */
		var name: String
		var ok: Bool;
		var r = one
		var Result: xNumber
		
		CommandLine = arg              /* remember this string for later        */
		status  = .Okay           /* clear out any previous errors         */
		xNumber.setErr(0);
		Scanner.Initialize(arg); Token = Scanner.Get()   /* start things off with the first token */
		
		if Token == .Let {         /* define a variable or a function */
			Token = Scanner.Get()
			if Token != .Name {
				Scanner.Mark(.ExpectingName); name = "Error"
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
				if Token != .RightBrace { Scanner.Mark(.ExpectingRBrace) }
				Token = Scanner.Get()
				if Token != .Assign { Scanner.Mark(.ExpectingAssign) }
				Functions.SetEquation(name, value: Scanner.GetString())
				r = zero
			} else {
				/* defining a variable */
				if Token != .Assign { Scanner.Mark(.ExpectingAssign) }
				Token = Scanner.Get()
				r = Expression()
				StoreVariable(name, Value: r)
			}
			Token = .Empty
		} else {
			do {
				r = r.mul(Expression())  // handle implicit multiplication
			} while (Token != .Empty && status == .Okay)
		}
		if Token != .Empty {
			Scanner.Mark(.IllegalExpression)
		}
		LastAnswer = r
		if status == .Okay {
			return r
		}
		return nil
	} // Evaluate;
	
}