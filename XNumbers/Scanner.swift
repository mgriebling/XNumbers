//
//  Scanner.swift
//  XNumbers
//
//  Created by Mike Griebling on 11 Mar 2015.
//  Copyright (c) 2015 Computer Inspirations. All rights reserved.
//

import Foundation

struct Scanner {

/*
	Scanner - Expression tokenizer.
		Copyright (C) 1996-2003 Michael Griebling
 
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

	enum Tokens {
		case Empty
		
		/* expression tokens */
		case Plus; case Minus; case Or;	case Xor
		
		/* term tokens */
		case Times; case Divide; case Div; case Mod
		case And; case ShiftRight; case AShiftRight
		case RotateRight; case ShiftLeft; case RotateLeft
		case ClearBit; case SetBit; case ToggleBit
		case nCr; case nPr
		
		/* power tokens */
		case Power; case Root; case Squared; case Cubed
		case Inverse; case Factorial; case PercentOf
		case PolarToRect
		
		/* factor tokens */
		case LeftBrace; case RightBrace; case Number
		case Complement; case Sin; case Cos; case Tan
		case ArcSin; case ArcCos; case ArcTan; case Sinh
		case Cosh; case Tanh; case ArcSinh; case ArcCosh
		case ArcTanh; case SquareRoot; case CubeRoot
		case NaturalLog; case Log; case PowerOfe; case Name
		case Base; case Digits; case Decimals; case Notation
		case DegRadGrad; case Plot; case iToken; case rToken
		case Theta; case ImagPart; case RealPart; case Convert
		case IntPart; case FracPart; case SignOf; case Abs
		case Min; case Max; case Conj; case Rand; case Semi
		case Pi; case Delete; case List; case Help; case Rat
		case Sum; case Average; case VertBrace; case Multiply
		
		/* comparison tokens */
		case Greater; case Less; case GreaterEqual
		case LessEqual; case NotEqual; case Assign
		case If; case True; case False
		
		/* miscellaneous tokens */
		case Let
	}

	typealias VarStr = String
	
	let ExpChar : Character = "E"
	
	struct StateType {
		var val     : Complex  /* number value */
		var varn    : VarStr
		var ch      : Character
		var pos     : Int
		var errpos  : Int
		var error   : Bool
		var pline   : String
		
		init () {
			self.val = Complex(fromDouble: 0)
			self.varn = ""
			self.ch = "\0"
			self.pos = 0
			self.errpos = 0
			self.error = false
			self.pline = ""
		}
	}
	
	var s = StateType()
	var stack: [StateType] = []
	var nkw: Int = 0

	let keyTab: [Tokens: String] = [
		.nCr:          "nCr",
		.nPr:          "nPr",
		.Let:          "let",
		.FracPart:     "frac",
		.Sum:          "sum",
		.Average:      "avg",
		.Multiply:     "mul",
		.Mod:          "mod",
		.Max:          "max",
		.rToken:       "mag",
		.Min:          "min",
		.ImagPart:     "im",
		.If:           "if",
		.True:         "true",
		.False:        "false",
		.IntPart:      "int",
		.Base:         "bas",
		.Or:           "or",
		.Xor:          "xor",
		.Complement:   "not",
		.Pi:           "pi",
		.And:          "and",
		.Theta:        "angle",
		.Abs:          "abs",
		.Log:          "log",
		.NaturalLog:   "ln",
		.List:         "list",
		.Help:         "help",
		.PolarToRect:  "rect",
		.RealPart:     "re",
		.Root:         "root",
		.Rand:         "rand",
		.Rat:			"rat",
		.Div:          "div",
		.Decimals:     "dp",
		.DegRadGrad:   "drg",
		.Digits:       "dig",
		.SetBit:       "sbit",
		.ShiftRight:   "shr",
		.ShiftLeft:    "shl",
		.SquareRoot:   "sqrt",
		.Delete:       "del",
		.Notation:     "sci",
		.SignOf:       "sign",
		.ClearBit:     "cbit",
		.CubeRoot:     "cbrt",
		.ToggleBit:    "tbit",
		.Conj:         "conj",
		.ArcSinh:      "sinhØπ",
		.ArcSinh:      "sinh-π",
		.ArcSinh:      "asinh",
		.ArcCosh:      "coshØπ",
		.ArcCosh:      "cosh-π",
		.ArcCosh:      "acosh",
		.ArcTanh:      "tanhØπ",
		.ArcTanh:      "tanh-π",
		.ArcTanh:      "atanh",
		.ArcSin:       "sinØπ",
		.ArcSin:       "sin-π",
		.ArcSin:       "asin",
		.ArcCos:       "cosØπ",
		.ArcCos:       "cos-π",
		.ArcCos:       "acos",
		.ArcTan:       "tanØπ",
		.ArcTan:       "tan-π",
		.ArcTan:       "atan",
		.Sinh:         "sinh",
		.Cosh:         "cosh",
		.Tanh:         "tanh",
		.Sin:          "sin",
		.Cos:          "cos",
		.Tan:          "tan"
	]

	mutating func Mark (errid: Real.Status) {
		if s.pos > s.errpos {
//			err.DisplayError(errid, s.pos, s.pline);
			Real.status = errid
		}
		s.errpos = s.pos; s.error = true
	} // Mark;
	
	mutating func PushState () {
		stack.insert(s, atIndex: 0) /* push on the stack */
	} // PushState;
	
	mutating func PopState () {
		if var s = stack.last {
			removeLast(&stack)
		}
	} // PopState;
	
	mutating func Read () {
		if !self.s.pline.isEmpty {
			s.ch = self.s.pline[s.pline.startIndex]
			removeAtIndex(&self.s.pline, s.pline.startIndex)
		}
		++s.pos
	} // Read();

	init (str: String) {
		self.s = StateType()
		self.s.pline = str
		Read() /* prime the tokenizer */
	} // Init;
	
	private func LocateChar(Str : String, ch : Character, start : Int) -> String.Index? {
		return find(Str, ch)
	} // LocateChar;
	
	func GetString () -> String {
		return s.pline   // .Substring(s.pos-1, s.pline.length)
	} // GetString;

	mutating func Get () -> Tokens {
		let sqrt : Character = "√"
		let pi  : Character = "π"
		var sym : Tokens = .Empty
		
		func number() {
			var Constant    : String = ""
			var NumChars    : String = ""
			var NumberChars : String
			var PunctuationChars : String = ""
			var ConIndex    : Int
			var pos         : Int
			
			func GetNumber() {
				for ;; {
					/* gather number characters */
					if LocateChar(NumChars, ch: s.ch, start: 0) != nil {
						/* valid numerical character */
						Constant.append(s.ch)
						if ((s.ch == "E") || (s.ch == ExpChar)) && (Complex.nState.LocalBase == 10) {
							NumChars = "+-0123456789"  /* just exponent digits */
						} else if (s.ch == "+") || (s.ch == "-") {
							NumChars = "0123456789"
						}
						Read()
					} else if LocateChar(PunctuationChars, ch: s.ch, start: 0) != nil {
						Read() /* ignore the punctuation */
					} else {
						break
					}
					if s.ch == "\0" { break }
				}
			} // GetNumber;
			
			func UnsignInt() {
				var num: Real
				var chars: String
				
				/* perform the actual conversion from string to number */
				Real.status = .Okay
				if Complex.nState.LocalBase == 10 {
					chars = Constant
					num = Real(fromString: chars)
				} else {
					num = ToReal(Integer(fromString: Constant, withBase: Complex.nState.LocalBase))
				}
				if Real.status == .Okay {  /* all went OK */
					s.val = Complex(re: num, im: Real.zero)
				} else {
					s.val = Complex.zero
					Mark(.IllegalNumber)
				}
			} // UnsignInt;
			
			
			sym = .Number; Constant = ""; ConIndex = 0
			NumberChars = ".EE0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
			PunctuationChars = ",'_"
			
			/* check if decimal point is a comma */
			if (Complex.nState.DigSep == ".") || (Complex.nState.FracSep == ".") {
				PunctuationChars = "." + PunctuationChars.substringFromIndex(find(PunctuationChars, ",")!)  // Substring(1, 2)
				NumberChars = "," + NumberChars.substringFromIndex(find(NumberChars, "E")!)  // Substring(1, 18)
			}
			
			/* valid number characters */
			if Complex.nState.LocalBase == 10 {
				NumChars = NumberChars.substringToIndex(find(NumberChars, "9")!) // .Substring(0, 13)
			} else {
				let start = find(NumberChars, "0")!
				var end = start
				var i = Complex.nState.LocalBase
				while i > 0 { end.successor(); i-- }
				NumChars = NumberChars.substringWithRange(start...end)  // Substring(3, xm.nState.LocalBase+3)
			}
			
			/* get a number string from the input */
			GetNumber()
			
			/* exchange the decimal point -- if needed */
			if NumberChars.hasPrefix(",") {
				Constant = Constant.stringByReplacingOccurrencesOfString(",", withString: ".")
			}
			
			/* convert to a Real */
			if !Constant.isEmpty {
				UnsignInt()
				if (s.ch == "°") || (s.ch == "i") {
					s.val = Complex(re: Real.zero, im: s.val.real); Read()
				}
			} else {
				s.val = Complex.zero
				Mark(.IllegalNumber) /* illegal number or constant */
			}
		} // number;
		
		func Illegal () {
			/* Illegal token if we reach here */
			Mark(.IllegalOperator); Read()
		} // Illegal;
		
		func IsAlphaNumeric(ch : Character) -> Bool {
			let str : String = ("" + [ch]).uppercaseString
			return str.rangeOfCharacterFromSet(NSCharacterSet.alphanumericCharacterSet()) != nil
		} // IsAlphaNumeric;
		
		func Variable () {
			s.varn = ""
			do {
				s.varn.append(s.ch); Read()
			} while IsAlphaNumeric(s.ch)
			sym = .Name
			for (key, name) in keyTab {
				if s.varn.compare(name) == NSComparisonResult.OrderedSame {
					sym = key; break
				}
			}
		} // Variable;
		
		while (s.ch != "\0") && (s.ch <= " ") { Read() }
		if s.ch == "\0" {
			sym = .Empty
		} else {
			switch s.ch {
			case "0"..."9", "." : number()
			case "+"     : Read(); sym = .Plus
			case "-"     :
				Read(); sym = .Minus
				if s.ch == "π" { Read(); sym = .Inverse }
			case sqrt    : Read(); sym = .SquareRoot
			case "²"     : Read(); sym = .Squared
			case "³"     : Read(); sym = .Cubed;
			if s.ch == sqrt { Read(); sym = .CubeRoot }
			case "°"     : Read(); sym = .iToken
			case "i"     : Read();
			if IsAlphaNumeric(s.ch) {
				--s.pos; s.ch = "i"; Variable()
			} else {
				sym = .iToken
				}
			case "~"     : Read(); sym = .Complement
			case "×"     : Read(); sym = .Times
			case ";"     : Read(); sym = .Semi
			case "/","÷" : Read(); sym = .Divide
			case "("     : Read(); sym = .LeftBrace
			case ")"     : Read(); sym = .RightBrace
			case "^"     : Read(); sym = .Power
			case "%"     : Read(); sym = .PercentOf
			case "!"     : Read(); sym = .Factorial
			case "&"     : Read(); sym = .And
			case "|"     : Read(); sym = .VertBrace
			case pi      : Read(); sym = .Pi
			case "e"     :
				Read(); sym = .Number
				if s.ch == "^" {
					Read(); sym = .PowerOfe
				} else if IsAlphaNumeric(s.ch) {
					--s.pos; s.ch = "e"; Variable()
				} else {
					s.val = Complex(re: Real.one.Exp(), im: Real.zero)
				}
			case "*"     :
				Read(); sym = .Times;
				if s.ch == "*" {
					Read(); sym = .Power
				} else if s.ch == sqrt {
					Read(); sym = .Root
				}
			case "ª"     : Read(); if s.ch == "°" { Read(); sym = .PolarToRect } else { Illegal() }
			case "="     : Read(); sym = .Assign
			case "#"     : Read(); sym = .NotEqual
			case ">"     : Read();
			if s.ch == "=" {
				Read(); sym = .GreaterEqual
			} else {
				sym = .Greater
				}
			case "<"     : Read();
			if s.ch == "=" {
				Read(); sym = .LessEqual
			} else if s.ch == ">" {
				Read(); sym = .NotEqual
			} else {
				sym = .Less
			}
			case "Ø"     : Read(); if s.ch == "π" { Read(); sym = .Inverse } else { Illegal() }
			case "A"..."Z", "a"..."d", "f"..."h", "j"..."z" : Variable()
			default: Read(); sym = .Empty
			}
		}
		return sym
	} // Get;

}
