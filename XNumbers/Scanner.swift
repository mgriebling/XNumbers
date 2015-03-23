//
//  Scanner.swift
//  XNumbers
//
//  Created by Mike Griebling on 11 Mar 2015.
//  Copyright (c) 2015 Computer Inspirations. All rights reserved.
//

import Foundation


func > (lhs: Scanner.Tokens, rhs: Scanner.Tokens) -> Bool {
	return lhs.rawValue > rhs.rawValue
}

func < (lhs: Scanner.Tokens, rhs: Scanner.Tokens) -> Bool {
	return lhs.rawValue < rhs.rawValue
}

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

	enum Tokens : Int, Comparable {
		case Empty = 0
		
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
	
	static let ExpChar : Character = "E"
	
	struct StateType {
		var val     : xNumber  /* number value */
		var varn    : VarStr
		var ch      : Character
		var pos     : Int
		var errpos  : Int
		var error   : Bool
		var pline   : String
		
		init () {
			self.val = xNumber(int: 0)
			self.varn = ""
			self.ch = "\0"
			self.pos = 0
			self.errpos = 0
			self.error = false
			self.pline = ""
		}
	}
	
	static var s = StateType()
	static var stack: [StateType] = []
	
	struct Symbol {
		let sym : Tokens
		let id : String
	}
	
	static let keyTab: [Symbol] = [
		Symbol(sym: .nCr,			id: "nCr"),
		Symbol(sym: .nPr,			id: "nPr"),
		Symbol(sym: .Let,			id: "let"),
		Symbol(sym: .FracPart,		id: "frac"),
		Symbol(sym: .Sum,			id: "sum"),
		Symbol(sym: .Average,		id: "avg"),
		Symbol(sym: .Multiply,		id: "mul"),
		Symbol(sym: .Mod,			id: "mod"),
		Symbol(sym: .Max,			id: "max"),
		Symbol(sym: .rToken,		id: "mag"),
		Symbol(sym: .Min,			id: "min"),
		Symbol(sym: .ImagPart,		id: "im"),
		Symbol(sym: .If,			id: "if"),
		Symbol(sym: .True,			id: "true"),
		Symbol(sym: .False,			id: "false"),
		Symbol(sym: .IntPart,		id: "int"),
		Symbol(sym: .Base,			id: "bas"),
		Symbol(sym: .Or,			id: "or"),
		Symbol(sym: .Xor,			id: "xor"),
		Symbol(sym: .Complement,	id: "not"),
		Symbol(sym: .Pi,			id: "pi"),
		Symbol(sym: .And,			id: "and"),
		Symbol(sym: .Theta,			id: "angle"),
		Symbol(sym: .Abs,			id: "abs"),
		Symbol(sym: .Log,			id: "log"),
		Symbol(sym: .NaturalLog,	id: "ln"),
		Symbol(sym: .List,			id: "list"),
		Symbol(sym: .Help,			id: "help"),
		Symbol(sym: .PolarToRect,	id: "rect"),
		Symbol(sym: .RealPart,		id: "re"),
		Symbol(sym: .Root,			id: "root"),
		Symbol(sym: .Rand,			id: "rand"),
		Symbol(sym: .Rat,			id: "rat"),
		Symbol(sym: .Div,			id: "div"),
		Symbol(sym: .Decimals,		id: "dp"),
		Symbol(sym: .DegRadGrad,	id: "drg"),
		Symbol(sym: .Digits,		id: "dig"),
		Symbol(sym: .SetBit,		id: "sbit"),
		Symbol(sym: .ShiftRight,	id: "shr"),
		Symbol(sym: .ShiftLeft,		id: "shl"),
		Symbol(sym: .SquareRoot,	id: "sqrt"),
		Symbol(sym: .Delete,		id: "del"),
		Symbol(sym: .Notation,		id: "sci"),
		Symbol(sym: .SignOf,		id: "sign"),
		Symbol(sym: .ClearBit,		id: "cbit"),
		Symbol(sym: .CubeRoot,		id: "cbrt"),
		Symbol(sym: .ToggleBit,		id: "tbit"),
		Symbol(sym: .Conj,			id: "conj"),
		Symbol(sym: .ArcSinh,		id: "sinh⁻¹"),
		Symbol(sym: .ArcSinh,		id: "sinh-¹"),
		Symbol(sym: .ArcSinh,		id: "asinh"),
		Symbol(sym: .ArcCosh,		id: "cosh⁻¹"),
		Symbol(sym: .ArcCosh,		id: "cosh-¹"),
		Symbol(sym: .ArcCosh,		id: "acosh"),
		Symbol(sym: .ArcTanh,		id: "tanh⁻¹"),
		Symbol(sym: .ArcTanh,		id: "tanh-¹"),
		Symbol(sym: .ArcTanh,		id: "atanh"),
		Symbol(sym: .ArcSin,		id: "sin⁻¹"),
		Symbol(sym: .ArcSin,		id: "sin-¹"),
		Symbol(sym: .ArcSin,		id: "asin"),
		Symbol(sym: .ArcCos,		id: "cos⁻¹"),
		Symbol(sym: .ArcCos,		id: "cos-¹"),
		Symbol(sym: .ArcCos,		id: "acos"),
		Symbol(sym: .ArcTan,		id: "tan⁻¹"),
		Symbol(sym: .ArcTan,		id: "tan-¹"),
		Symbol(sym: .ArcTan,		id: "atan"),
		Symbol(sym: .Sinh,			id: "sinh"),
		Symbol(sym: .Cosh,			id: "cosh"),
		Symbol(sym: .Tanh,			id: "tanh"),
		Symbol(sym: .Sin,			id: "sin"),
		Symbol(sym: .Cos,			id: "cos"),
		Symbol(sym: .Tan,			id: "tan")
	]

	static func Mark (errid: Status) {
		if s.pos > s.errpos {
//			err.DisplayError(errid, s.pos, s.pline);
			status = errid
		}
		s.errpos = s.pos; s.error = true
	} // Mark;
	
	static func PushState () {
		stack.insert(s, atIndex: 0) /* push on the stack */
	} // PushState;
	
	static func PopState () {
		if var s = stack.last {
			removeLast(&stack)
		}
	} // PopState;
	
	static func Read () {
		if !self.s.pline.isEmpty {
			s.ch = self.s.pline[s.pline.startIndex]
			removeAtIndex(&self.s.pline, s.pline.startIndex)
		} else {
			s.ch = "\0"
		}
		++s.pos
	} // Read();

	static func Initialize (str: String) {
		self.s = StateType()
		self.s.pline = str
		Read() /* prime the tokenizer */
	} // Init;
	
	static private func LocateChar(Str : String, ch : Character, start : Int) -> String.Index? {
		return find(Str, ch)
	} // LocateChar;
	
	static func GetString () -> String {
		return s.pline   // .Substring(s.pos-1, s.pline.length)
	} // GetString;

	static func Get () -> Tokens {
		let sqrt : Character = "√"
		let pi  : Character = "π"
		var sym : Tokens = .Empty
		
		func number(base : Int) {
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
						if ((s.ch == "E") || (s.ch == ExpChar)) && (base == 10) {
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
				var num: xNumber
				var chars: String
				
				/* perform the actual conversion from string to number */
				status = .Okay
				if base == 10 {
					chars = Constant
					num = xNumber(string: chars)
				} else {
					num = xNumber(string: Constant, andBase: base)
				}
				if status == .Okay {  /* all went OK */
					s.val = num
				} else {
					s.val = xNumber.zero
					Mark(.IllegalNumber)
				}
			} // UnsignInt;
			
			
			sym = .Number; Constant = ""; ConIndex = 0
			NumberChars = ".EE0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
			PunctuationChars = ",'_"
			
			/* check if decimal point is a comma */
			if (nState.DigSep == ".") || (nState.FracSep == ".") {
				println("nState = \(nState.DigSep) & \(nState.FracSep)");
				PunctuationChars = "." + PunctuationChars.substringFromIndex(find(PunctuationChars, ",")!)  // Substring(1, 2)
				NumberChars = "," + NumberChars.substringFromIndex(find(NumberChars, "E")!)  // Substring(1, 18)
			}
			
			/* valid number characters */
			if base == 10 {
				NumChars = NumberChars.substringToIndex(find(NumberChars, "A")!) // .Substring(0, 13)
			} else {
				let start = find(NumberChars, "0")!
				var end = start
				var i = base-1
				while i > 0 { end = end.successor(); i-- }
				NumChars = NumberChars.substringWithRange(start...end)  // Substring(3, xm.nState.LocalBase+3)
			}
			
			/* get a number string from the input */
			GetNumber()
			
			/* exchange the decimal point -- if needed */
			if NumberChars.hasPrefix(",") {
				Constant = Constant.stringByReplacingOccurrencesOfString(",", withString: ".")
			}
			
			/* convert to a xNumber */
			if !Constant.isEmpty {
				UnsignInt()
				if (s.ch == "ⅈ") || (s.ch == "i") {
					s.val = xNumber(real: xNumber.zero, andImaginary: s.val.real()); Read()
				}
			} else {
				s.val = xNumber.zero
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
			for rec in keyTab {
				if s.varn == rec.id {
					sym = rec.sym; break
				}
			}
		} // Variable;
		
		while (s.ch != "\0") && (s.ch <= " ") { Read() }
		if s.ch == "\0" {
			sym = .Empty
		} else {
			switch s.ch {
				case "0": Read(); sym = .Number
					switch s.ch {
						case "x", "X": Read(); number(16)
						case "o", "O": Read(); number(8)
						case "b", "B": Read(); number(2)
						default: number(nState.LocalBase)
					}
				case "1"..."9", "." : number(nState.LocalBase)
				case "+"     : Read(); sym = .Plus
				case "-"     :
					Read(); sym = .Minus
					if s.ch == "¹" { Read(); sym = .Inverse }
				case sqrt    : Read(); sym = .SquareRoot
				case "²"     : Read(); sym = .Squared
				case "³"     : Read(); sym = .Cubed;
					if s.ch == sqrt { Read(); sym = .CubeRoot }
				case "ⅈ"      : Read(); sym = .iToken
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
						s.val = xNumber.one.exp()
					}
				case "*"     :
					Read(); sym = .Times;
					if s.ch == "*" {
						Read(); sym = .Power
					} else if s.ch == sqrt {
						Read(); sym = .Root
					}
				case "→"     : Read(); if s.ch == "ℝ" { Read(); sym = .PolarToRect } else { Illegal() }
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
				case "⁻"     : Read(); if s.ch == "¹" { Read(); sym = .Inverse } else { Illegal() }
				case "A"..."Z", "a"..."d", "f"..."h", "j"..."z" : Variable()
				default: Read(); sym = .Empty
			}
		}
		return sym
	} // Get;

}
