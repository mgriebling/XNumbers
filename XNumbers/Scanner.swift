////
////  Scanner.swift
////  XNumbers
////
////  Created by Mike Griebling on 11 Mar 2015.
////  Copyright (c) 2015 Computer Inspirations. All rights reserved.
////
//
//import Foundation
//
//MODULE Scanner;
//
//(*
//	Scanner - Expression tokenizer.
//		Copyright (C) 1996-2003 Michael Griebling
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
//	*)
//
//IMPORT el   :=  ADT:LinkedList,
//Object,
//
//err  :=  CLIErrors,
//xm   :=  Complex,
//xi   :=  Integers,
//x    :=  Reals;
//
//CONST
//(* symbols *)
//Empty * = 0;
//
//(* expression tokens *)
//Plus * = 1; Minus * = 2; Or * = 3; Xor * = 4;
//
//(* term tokens *)
//Times * = 10; Divide * = 11; Div * = 12; Mod * = 13;
//And * = 14; ShiftRight * = 15; AShiftRight * = 16;
//RotateRight * = 17; ShiftLeft * = 18; RotateLeft * = 19;
//ClearBit * = 20; SetBit * = 21; ToggleBit * = 22;
//nCr * = 23; nPr * = 24;
//
//(* power tokens *)
//Power * = 30; Root * = 31; Squared * = 32; Cubed * = 33;
//Inverse * = 34; Factorial * = 35; PercentOf * = 36;
//PolarToRect * = 37;
//
//(* factor tokens *)
//LeftBrace * = 40; RightBrace * = 41; Number * = 42;
//Complement * = 43; Sin * = 44; Cos * = 45; Tan * = 46;
//ArcSin * = 47; ArcCos * = 48; ArcTan * = 49; Sinh * = 50;
//Cosh * = 51; Tanh * = 52; ArcSinh * = 53; ArcCosh * = 54;
//ArcTanh * = 55; SquareRoot * = 56; CubeRoot * = 57;
//NaturalLog * = 58; Log * = 59; PowerOfe * = 60; Name * = 61;
//Base * = 62; Digits * = 63; Decimals * = 64; Notation * = 65;
//DegRadGrad * = 66; Plot * = 67; iToken * = 68; rToken * = 69;
//Theta * = 70; ImagPart * = 71; RealPart * = 72; Convert * = 73;
//IntPart * = 74; FracPart * = 75; SignOf * = 76; Abs * = 77;
//Min * = 78; Max * = 79; Conj * = 80; Rand * = 81; Semi * = 82;
//Pi * = 83; Delete * = 84; List * = 85; Help * = 86; Rat * = 87;
//Sum * = 88; Average * = 89; VertBrace * = 90; Multiply * = 91;
//
//(* comparison tokens *)
//Greater * = 100; Less * = 101; GreaterEqual * = 102;
//LessEqual * = 103; NotEqual * = 104; Assign * = 105;
//If * = 106; True * = 107; False * = 108;
//
//(* miscellaneous tokens *)
//Let * = 110;
//
//TYPE
//VarStr = STRING;
//String * = STRING;
//
//CONST
//KW = 100;
//ExpChar * = "E";
//
//TYPE
//StateType = POINTER TO StateTypeDesc;
//StateTypeDesc = RECORD (Object.ObjectDesc)
//val-    : xm.Complex;  (* number value *)
//var-    : VarStr;
//ch      : UCS4CHAR;
//pos     : INTEGER;
//errpos  : INTEGER;
//error-  : BOOLEAN;
//pline   : String
//END;
//
//VAR
//s-: StateType;
//stack: el.LinkedList;
//nkw: INTEGER;
//
//keyTab: ARRAY KW OF
//RECORD sym: INTEGER; id: STRING END;
//
//PROCEDURE Mark * (errid: LONGINT);
//BEGIN
//IF s.pos>s.errpos THEN
//err.DisplayError(errid, s.pos, s.pline);
//x.status:=errid
//END;
//s.errpos:=s.pos; s.error:=TRUE
//END Mark;
//
//PROCEDURE PushState * ();
//BEGIN
//stack.Prepend(s);         (* push on the stack *)
//END PushState;
//
//PROCEDURE PopState * ();
//VAR
//o: Object.Object;
//BEGIN
//o:=stack.RemoveLast();    (* pop off the stack *)
//s:=o(StateType)
//END PopState;
//
//PROCEDURE Read;
//BEGIN
//s.ch:=s.pline.CharAt(s.pos); INC(s.pos)
//END Read;
//
//PROCEDURE Init * (str: STRING);
//BEGIN
//NEW(s);
//s.pline:=str; s.errpos:=0; s.pos:=0; s.error:=FALSE;
//Read (* prime the tokenizer *)
//END Init;
//
//PROCEDURE LocateChar(Str : STRING; ch : UCS4CHAR;
//start : LONGINT) : LONGINT;
//BEGIN
//RETURN Str.IndexOf(ch, start)
//END LocateChar;
//
//PROCEDURE GetString * () : STRING;
//BEGIN
//RETURN s.pline.Substring(s.pos-1, s.pline.length)
//END GetString;
//
//PROCEDURE Get * (VAR sym: INTEGER);
//CONST
//(* These values require a special font CalcFont *)
//sqrt = 0C7X; (* « *)
//pi   = 0BFX; (* ø *)
//
//PROCEDURE number;
//VAR
//Constant    : STRING;
//NumChars    : STRING;
//NumberChars : STRING;
//PunctuationChars : STRING;
//ConIndex    : INTEGER;
//pos         : LONGINT;
//
//PROCEDURE GetNumber();
//BEGIN
//LOOP
//(* gather number characters *)
//IF LocateChar(NumChars, s.ch, 0) >= 0 THEN
//(* valid numerical character *)
//Constant:=Constant.Concat(Object.NewUCS4Char(s.ch));
//IF ((s.ch = 'E') OR (s.ch = ExpChar)) & (xm.nState.LocalBase = 10) THEN
//NumChars:="+-0123456789";  (* just exponent digits *)
//ELSIF (s.ch = '+') OR (s.ch = '-') THEN NumChars:="0123456789"
//END;
//Read
//ELSIF LocateChar(PunctuationChars, s.ch, 0) >= 0 THEN Read (* ignore the punctuation *)
//ELSE EXIT
//END;
//IF s.ch = 0X THEN EXIT END;
//END
//END GetNumber;
//
//PROCEDURE UnsignInt;
//VAR num: x.Real;
//str8: Object.String8;
//chars: Object.CharsLatin1;
//BEGIN
//(* perform the actual conversion from string to number *)
//x.status:=x.Okay;
//IF xm.nState.LocalBase = 10 THEN
//str8:=Constant.ToString8("?");
//chars:=str8.CharsLatin1();
//num:=x.ToReal(chars^)
//ELSE
//num:=xm.IntegerToReal(xi.New(Constant, xm.nState.LocalBase))
//END;
//IF x.status=x.Okay THEN  (* all went OK *)
//s.val:=xm.Init(num, x.zero)
//ELSE
//s.val := xm.zero;
//Mark(x.IllegalNumber)
//END
//END UnsignInt;
//
//BEGIN
//sym:=Number; Constant:=""; ConIndex:=0;
//NumberChars := ".EE0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
//PunctuationChars := ",'_";
//
//(* check if decimal point is a comma *)
//IF (xm.nState.DigSep=".") OR (xm.nState.FracSep=".") THEN
//PunctuationChars:="." + PunctuationChars.Substring(1, 2);
//NumberChars:="," + NumberChars.Substring(1, 18)
//END;
//
//(* valid number characters *)
//IF xm.nState.LocalBase = 10 THEN
//NumChars:=NumberChars.Substring(0, 13);
//ELSE
//NumChars:=NumberChars.Substring(3, xm.nState.LocalBase+3);
//END;
//
//(* get a number string from the input *)
//GetNumber();
//
//(* exchange the decimal point -- if needed *)
//IF NumberChars.CharAt(0)="," THEN
//pos:=LocateChar(Constant, ",", 0);
//IF pos>=0 THEN
//Constant:=Constant.Substring(0, pos-1) + "." + Constant.Substring(pos+1, Constant.length-pos)
//END
//END;
//
//(* convert to a Real *)
//IF Constant.length > 0 THEN
//UnsignInt;
//IF (s.ch = "°") OR (s.ch = "i") THEN
//s.val:=xm.Init(x.zero, s.val.real); Read
//END
//ELSE
//s.val := xm.zero;
//Mark(x.IllegalNumber) (* illegal number or constant *)
//END
//END number;
//
//PROCEDURE Illegal;
//BEGIN
//(* Illegal token if we reach here *)
//Mark(x.IllegalOperator); Read
//END Illegal;
//
//PROCEDURE IsAlphaNumeric(): BOOLEAN;
//BEGIN
//RETURN (s.ch>="0") & (s.ch<="9") OR (CAP(s.ch)>="A") & (CAP(s.ch)<="Z")
//END IsAlphaNumeric;
//
//PROCEDURE Variable;
//VAR cnt, k: INTEGER;
//BEGIN
//cnt:=0; s.var:="";
//REPEAT
//s.var:=s.var+Object.NewUCS4Char(s.ch); Read
//UNTIL ~IsAlphaNumeric();
//k:=0;
//WHILE (k<nkw) & (~s.var.Equals(keyTab[k].id)) DO INC(k) END;
//IF k<nkw THEN sym:=keyTab[k].sym ELSE sym:=Name END
//END Variable;
//
//BEGIN
//WHILE (s.ch#0X) & (s.ch<=" ") DO Read END;
//IF s.ch=0X THEN sym:=Empty
//ELSE
//CASE s.ch OF
//| "0".."9","." : number
//| "+"     : Read; sym:=Plus
//| "-"     : Read; sym:=Minus;
//IF s.ch = "π" THEN Read; sym:=Inverse END
//| sqrt    : Read; sym:=SquareRoot
//| "≤"     : Read; sym:=Squared
//| "≥"     : Read; sym:=Cubed;
//IF s.ch = sqrt THEN Read; sym:=CubeRoot END
//| "°"     : Read; sym:=iToken
//| "i"     : Read;
//IF IsAlphaNumeric() THEN
//DEC(s.pos); s.ch:="i"; Variable
//ELSE sym:=iToken
//END
//| "~"     : Read; sym:=Complement
//| "◊"     : Read; sym:=Times
//| ";"     : Read; sym:=Semi
//| "/","˜" : Read; sym:=Divide
//| "("     : Read; sym:=LeftBrace
//| ")"     : Read; sym:=RightBrace
//| "^"     : Read; sym:=Power
//| "%"     : Read; sym:=PercentOf
//| "!"     : Read; sym:=Factorial
//| "&","∑" : Read; sym:=And
//| "|"     : Read; sym:=VertBrace
//| pi      : Read; sym:=Pi
//| "e"     : Read; sym:=Number;
//IF s.ch = "^" THEN Read; sym:=PowerOfe
//ELSIF IsAlphaNumeric() THEN
//DEC(s.pos); s.ch:="e"; Variable
//ELSE s.val:=xm.Init(x.one.Exp(), x.zero)
//END
//| "*"     : Read; sym:=Times;
//IF s.ch = "*" THEN Read; sym:=Power
//ELSIF s.ch = sqrt THEN Read; sym:=Root
//END
//| "ª"     : Read; IF s.ch="°" THEN Read; sym:=PolarToRect ELSE Illegal END
//| "="     : Read; sym:=Assign
//| "#"     : Read; sym:=NotEqual
//| ">"     : Read; IF s.ch="=" THEN Read; sym:=GreaterEqual
//ELSE sym:=Greater
//END
//| "<"     : Read; IF s.ch="=" THEN Read; sym:=LessEqual
//ELSIF s.ch=">" THEN Read; sym:=NotEqual
//ELSE sym:=Less
//END
//| "Ø"     : Read; IF s.ch="π" THEN Read; sym:=Inverse ELSE Illegal END
//| "A".."Z", "a".."d", "f".."h", "j".."z" : Variable
//| ELSE Read; sym:=Empty
//END
//END;
//END Get;
//
//PROCEDURE EnterKW (sym: INTEGER; name: STRING);
//BEGIN
//ASSERT(nkw<KW);
//keyTab[nkw].sym:=sym;
//keyTab[nkw].id:=name;
//INC(nkw)
//END EnterKW;
//
//BEGIN
//nkw:=0;
//EnterKW(nCr,          "nCr");
//EnterKW(nPr,          "nPr");
//EnterKW(Let,          "let");
//EnterKW(FracPart,     "frac");
//EnterKW(Sum,          "sum");
//EnterKW(Average,      "avg");
//EnterKW(Multiply,     "mul");
//EnterKW(Mod,          "mod");
//EnterKW(Max,          "max");
//EnterKW(rToken,       "mag");
//EnterKW(Min,          "min");
//EnterKW(ImagPart,     "im");
//EnterKW(If,           "if");
//EnterKW(True,         "true");
//EnterKW(False,        "false");
//EnterKW(IntPart,      "int");
//EnterKW(Base,         "bas");
//EnterKW(Or,           "or");
//EnterKW(Xor,          "xor");
//EnterKW(Complement,   "not");
//EnterKW(Pi,           "pi");
//EnterKW(And,          "and");
//EnterKW(Theta,        "angle");
//EnterKW(Abs,          "abs");
//EnterKW(Log,          "log");
//EnterKW(NaturalLog,   "ln");
//EnterKW(List,         "list");
//EnterKW(Help,         "help");
//EnterKW(PolarToRect,  "rect");
//EnterKW(RealPart,     "re");
//EnterKW(Root,         "root");
//EnterKW(Rand,         "rand");
//EnterKW(Rat,			"rat");
//EnterKW(Div,          "div");
//EnterKW(Decimals,     "dp");
//EnterKW(DegRadGrad,   "drg");
//EnterKW(Digits,       "dig");
//EnterKW(SetBit,       "sbit");
//EnterKW(ShiftRight,   "shr");
//EnterKW(ShiftLeft,    "shl");
//EnterKW(SquareRoot,   "sqrt");
//EnterKW(Delete,       "del");
//EnterKW(Notation,     "sci");
//EnterKW(SignOf,       "sign");
//EnterKW(ClearBit,     "cbit");
//EnterKW(CubeRoot,     "cbrt");
//EnterKW(ToggleBit,    "tbit");
//EnterKW(Conj,         "conj");
//EnterKW(ArcSinh,      "sinhØπ");
//EnterKW(ArcSinh,      "sinh-π");
//EnterKW(ArcSinh,      "asinh");
//EnterKW(ArcCosh,      "coshØπ");
//EnterKW(ArcCosh,      "cosh-π");
//EnterKW(ArcCosh,      "acosh");
//EnterKW(ArcTanh,      "tanhØπ");
//EnterKW(ArcTanh,      "tanh-π");
//EnterKW(ArcTanh,      "atanh");
//EnterKW(ArcSin,       "sinØπ");
//EnterKW(ArcSin,       "sin-π");
//EnterKW(ArcSin,       "asin");
//EnterKW(ArcCos,       "cosØπ");
//EnterKW(ArcCos,       "cos-π");
//EnterKW(ArcCos,       "acos");
//EnterKW(ArcTan,       "tanØπ");
//EnterKW(ArcTan,       "tan-π");
//EnterKW(ArcTan,       "atan");
//EnterKW(Sinh,         "sinh");
//EnterKW(Cosh,         "cosh");
//EnterKW(Tanh,         "tanh");
//EnterKW(Sin,          "sin");
//EnterKW(Cos,          "cos");
//EnterKW(Tan,          "tan");
//
//stack:=el.New()
//END Scanner.
