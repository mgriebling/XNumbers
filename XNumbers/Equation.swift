//
//  Equation.swift
//  XNumbers
//
//  Created by Mike Griebling on 11 Mar 2015.
//  Copyright (c) 2015 Computer Inspirations. All rights reserved.
//

import Foundation

class Equation {
//	
//	MODULE Equations;
//	
// (*
//	Equations - Equation evaluation.
//	Copyright (c) 1996-2010 Michael Griebling
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
//	IMPORT        ADT:Storable,
//	IO,
//	Out,
//	Object,
//	
//	cli := CLIErrors,
//	XM  := Complex,
//	f   := Functions,
//	XI  := Integers,
//	X   := Reals,
//	R   := Rationals,
//	sc  := Scanner,
//	V   := Variables;
//	
//	VAR
//	Token: INTEGER;
//	zero: XM.Complex;
//	CommandLine : STRING;
//	LastAnswer: XM.Complex;
//	
//	
//	PROCEDURE StoreVariable(Location: STRING; Value : XM.Complex);
//	(* Store the `Value' argument in the `Location' variable. *)
//	VAR ok: BOOLEAN;
//	BEGIN
//	V.Set(Location, Value, ok);
//	IF ~ok THEN sc.Mark(cli.TooManyVariables) END
//	END StoreVariable;
//	
//	PROCEDURE^ Expression (VAR Result : XM.Complex);
//	
//	PROCEDURE^ Evaluate (arg: STRING; VAR Result: XM.Complex): BOOLEAN;
//	
//	
//	PROCEDURE Print (name, arg: STRING);
//	BEGIN
//	Out.Object(name); Out.String(" = "); Out.Object(arg); Out.Ln
//	END Print;
//	
//	
//	PROCEDURE Printf (name, arg, value: STRING);
//	BEGIN
//	Out.Object(name);
//	Out.String("("); Out.Object(arg); Out.String(") = ");
//	Out.Object(value); Out.Ln
//	END Printf;
//	
//	
//	PROCEDURE Min (a, b: XM.Complex) : XM.Complex;
//	BEGIN
//	IF a.Cmp(b) > 0 THEN RETURN b
//	ELSE RETURN a
//	END
//	END Min;
//	
//	
//	PROCEDURE Max (a, b: XM.Complex) : XM.Complex;
//	BEGIN
//	IF a.Cmp(b) > 0 THEN RETURN a
//	ELSE RETURN b
//	END
//	END Max;
//	
//	
//	PROCEDURE Permutations * (n, r: XM.Complex) : XM.Complex;
//	(** Return the number of permutations of n different objects
//	taken r at a time (i.e., n!/(n-r)! )
// *)
//	VAR ni, ri: LONGINT;
//	nI: X.Real;
//	BEGIN
//	ni:=ENTIER(n.real.Short());
//	ri:=ENTIER(r.real.Short());
//	nI:=X.Factorial(ni);
//	nI:=nI.Div(X.Factorial(ni-ri));
//	RETURN XM.Init(nI.Entier(), X.zero)
//	END Permutations;
//	
//	
//	PROCEDURE Combinations * (n, r: XM.Complex) : XM.Complex;
//	(** Return the combinations of n different objects taken
//	r at a time (i.e., n!/(r!(n-r)!))
// *)
//	VAR ni, ri: LONGINT;
//	nI: X.Real;
//	BEGIN
//	ni:=ENTIER(n.real.Short());
//	ri:=ENTIER(r.real.Short());
//	n:=Permutations(n, r);            (* n=n!/(n-r)! *)
//	nI:=X.Factorial(ri);              (* n=r! *)
//	nI:=n.real.Div(nI);               (* Result=n!/(r!(n-r)!) *)
//	RETURN XM.Init(nI.Entier(), X.zero)
//	END Combinations;
//	
//	
//	
//	PROCEDURE Help;
//	BEGIN
//	Out.Ln;
//	Out.String(" +, -, *, /      Addition/Subtraction/Multiplication/Division"); Out.Ln;
//	Out.String(" #, <>, =        Inequality, equality comparison"); Out.Ln;
//	Out.String(" >, >=           Greater than, Greater or equal comparison"); Out.Ln;
//	Out.String(" <, <=           Less than, Less or equal comparison"); Out.Ln;
//	Out.String(" true, false     Logical TRUE and FALSE"); Out.Ln;
//	Out.String(" if(exp; a; b)   If exp > 0 return a else return b"); Out.Ln;
//	Out.String(" ()              Brackets"); Out.Ln;
//	Out.String(" ^, **           Power"); Out.Ln;
//	Out.String(" %               x 0.01"); Out.Ln;
//	Out.String(" &, and          Logical And"); Out.Ln;
//	Out.String(" or, xor         Logical Inclusive/Exclusive Or"); Out.Ln;
//	Out.String(" ~, not          Logical Complement"); Out.Ln;
//	Out.String(" mod, div        Modulo/Integer Division"); Out.Ln;
//	Out.String(" sqrt, cbrt      Square/Cube Root"); Out.Ln;
//	Out.String(" root            Any Root"); Out.Ln;
//	Out.String(" abs, |x|        Absolute value or complex magnitude"); Out.Ln;
//	Out.String(" rand            Random number between 0 and 1"); Out.Ln;
//	Out.String(" e               Euler's constant"); Out.Ln;
//	Out.String(" e^              Power of e"); Out.Ln;
//	Out.String(" i               Imaginary number"); Out.Ln;
//	Out.String(" angle           Complex angle"); Out.Ln;
//	Out.String(" re, im          Real/Imaginary part of a number"); Out.Ln;
//	Out.String(" rect            Polar to rectangle conversion"); Out.Ln;
//	Out.String(" conj            Complex conjugate"); Out.Ln;
//	Out.String(" nCr, nPr        Combinations/Permutations"); Out.Ln;
//	Out.String(" int, frac       Integer/Fractional part of a number"); Out.Ln;
//	Out.String(" sign            Sign of a number (-1 or 1)"); Out.Ln;
//	Out.String(" min(x;y;...z)   Minimum of x, y, ...z"); Out.Ln;
//	Out.String(" max(x;y;...z)   Maximum of x, y, ...z"); Out.Ln;
//	Out.String(" sum(x;y;...z)   Summation of x, y, ...z"); Out.Ln;
//	Out.String(" avg(x;y;...z)   Average of x, y, ...z"); Out.Ln;
//	Out.String(" mul(x;y;...z)   Multiply x, y, ...z"); Out.Ln;
//	Out.String(" ln, log         Natural, Base 10 Logarithm"); Out.Ln;
//	Out.String(" sin, asin       Sine, Arcsine"); Out.Ln;
//	Out.String(" cos, acos       Cosine, Arccosine"); Out.Ln;
//	Out.String(" tan, atan       Tangent, Arctangent"); Out.Ln;
//	Out.String(" sinh, asinh     Hyperbolic Sine, Arcsine"); Out.Ln;
//	Out.String(" cosh, acosh     Hyperbolic Cosine, Arccosine"); Out.Ln;
//	Out.String(" tanh, atanh     Hyperbolic Tangent, Arctangent"); Out.Ln;
//	Out.String(" sbit, cbit      Set/Clear Bit"); Out.Ln;
//	Out.String(" tbit            Toggle Bit"); Out.Ln;
//	Out.String(" shr, shl        Shift Right/Left"); Out.Ln;
//	Out.String(" <name>          Variable contents"); Out.Ln;
//	Out.String(" f(x;y;...z)     Evaluate function <f>"); Out.Ln;
//	Out.String(" let n=<exp>     Define variable 'n' with <exp>"); Out.Ln;
//	Out.String(" let f(x)=<exp>  Define function 'f' with args 'x' and expression <exp>"); Out.Ln;
//	Out.String(" del <name>      Deletes the variable, function"); Out.Ln;
//	Out.String(" list            Lists all defined variables, functions"); Out.Ln;
//	Out.String(" help            Displays this list"); Out.Ln;
//	Out.String(" pi              Constant Pi"); Out.Ln;
//	Out.String(" sci             Toggle Scientific/Engineering/Floating Point"); Out.Ln;
//	Out.String(" rat             Toggle Rational calculations on/off"); Out.Ln;
//	Out.String(" bas n           Change to Base n (n = 2..36)"); Out.Ln;
//	Out.String(" dig n           Use n Digits"); Out.Ln;
//	Out.String(" dp n            Use n Decimal Places"); Out.Ln;
//	Out.String(" drg             Toggle Degree/Radian/Grad"); Out.Ln;
//	END Help;
//	
//	PROCEDURE Function (fname: STRING) : XM.Complex;
//	VAR
//	arg, eqn: STRING;
//	ok: BOOLEAN;
//	nargs: LONGINT;
//	ptoken: INTEGER;
//	res: XM.Complex;
//	BEGIN
//	(* evaluate the passed function *)
//	f.Get(fname, eqn, ok);
//	nargs:=0;
//	res:=XM.zero;
//	IF Token = sc.LeftBrace THEN
//	REPEAT
//	sc.Get(Token); Expression(res);
//	f.GetArg(fname, nargs, arg, ok); INC(nargs);
//	V.Setf(arg, res, ok)
//	UNTIL Token # sc.Semi;
//	IF Token # sc.RightBrace THEN sc.Mark(X.MismatchBraces) END;
//	IF nargs # f.NumArgs(fname) THEN sc.Mark(cli.IncompatibleArgs) END;
//	sc.Get(Token);   (* skip right brace *)
//	
//	(* now we finally evaluate the function *)
//	sc.PushState();                 (* save current expression's state *)
//	ptoken:=Token;                  (* save the current token *)
//	X.status := X.Okay;             (* clear out any previous errors *)
//	X.err := 0;
//	sc.Init(eqn); sc.Get(Token);    (* start things off with the first token *)
//	Expression(res);
//	sc.PopState();                  (* restore our previous state *)
//	V.Deletef();                    (* clear function stack *)
//	Token:=ptoken                   (* and active token *)
//	ELSIF f.NumArgs(fname) > 0 THEN
//	sc.Mark(cli.ExpectingArgs);
//	res:=XM.zero
//	END;
//	RETURN res
//	END Function;
//	
//	
//	PROCEDURE IfCondition () : XM.Complex;
//	(** Handle If(<expression>;<expression>;<expression>) *)
//	VAR
//	cond, tval, fval: XM.Complex;
//	BEGIN
//	IF Token = sc.LeftBrace THEN
//	sc.Get(Token); Expression(cond);
//	IF Token # sc.Semi THEN sc.Mark(cli.ExpectingArgs) END;
//	sc.Get(Token); Expression(tval);
//	IF Token # sc.Semi THEN sc.Mark(cli.ExpectingArgs) END;
//	sc.Get(Token); Expression(fval);
//	IF Token # sc.RightBrace THEN sc.Mark(X.MismatchBraces) END;
//	sc.Get(Token);
//	IF cond.Cmp(zero)>0 THEN RETURN tval ELSE RETURN fval END
//	ELSE RETURN XM.zero
//	END
//	END IfCondition;
//	
//	
//	PROCEDURE Factor (VAR Result : XM.Complex);
//	(** 'Result' = <[Factor Operator] (<Variable> | <Function> | "(" <Expression> ")" | <Number>) *)
//	TYPE
//	funcType = PROCEDURE(x,y:XM.Complex):XM.Complex;
//	
//	VAR
//	SaveBase : SHORTINT;
//	tmp      : XM.Complex;
//	t        : X.Real;
//	i        : XI.Integer;
//	Digits   : INTEGER;
//	n        : LONGINT;
//	str      : STRING;
//	ok       : BOOLEAN;
//	
//	PROCEDURE Add (x,y: XM.Complex) : XM.Complex;
//	BEGIN
//	RETURN x.Add(y)
//	END Add;
//	
//	PROCEDURE Avg (x,y: XM.Complex) : XM.Complex;
//	BEGIN
//	INC(n); RETURN x.Add(y)
//	END Avg;
//	
//	PROCEDURE Mul (x,y: XM.Complex) : XM.Complex;
//	BEGIN
//	RETURN x.Mul(y)
//	END Mul;
//	
//	PROCEDURE Next;
//	BEGIN
//	sc.Get(Token);
//	IF Token = sc.Minus THEN
//	sc.Get(Token); Factor(Result);
//	Result := Result.Neg()
//	ELSE
//	Factor(Result)
//	END
//	END Next;
//	
//	PROCEDURE Args (func: funcType) : XM.Complex;
//	VAR
//	tmp2: XM.Complex;
//	BEGIN
//	tmp:=XM.zero;
//	sc.Get(Token);
//	IF Token#sc.LeftBrace THEN sc.Mark(X.MismatchBraces) END;
//	sc.Get(Token); Expression(tmp);
//	IF Token#sc.Semi THEN sc.Mark(cli.ExpectingArgs) END;
//	REPEAT
//	sc.Get(Token); Expression(tmp2);
//	tmp:=func(tmp, tmp2)
//	UNTIL Token#sc.Semi;
//	IF Token#sc.RightBrace THEN sc.Mark(X.MismatchBraces) END;
//	sc.Get(Token);
//	RETURN tmp
//	END Args;
//	
//	PROCEDURE FixR;
//	BEGIN
//	Result:=XM.Init(t, X.zero)
//	END FixR;
//	
//	PROCEDURE FixI;
//	BEGIN
//	Result:=XM.Init(XM.IntegerToReal(i), X.zero)
//	END FixI;
//	
//	BEGIN
//	CASE Token OF
//	sc.LeftBrace  : sc.Get(Token); Expression(Result);
//	IF Token = sc.RightBrace THEN sc.Get(Token);
//	ELSE sc.Mark(X.MismatchBraces)
//	END;
//	| sc.VertBrace  : sc.Get(Token); Expression(Result);
//	t:=Result.PolarMag(); FixR;
//	IF Token = sc.VertBrace THEN sc.Get(Token);
//	ELSE sc.Mark(X.MismatchBraces)
//	END;
//	| sc.Number     : IF XM.nState.Rational THEN Result := R.RInit(sc.s.val.real, 1)
//	ELSE Result := sc.s.val
//	END;
//	sc.Get(Token);
//	(*
//	IF Token = sc.Number THEN
//	Result:=Result.Mul(sc.val);
//	sc.Get(Token)
//	ELSIF Token = sc.LeftBrace THEN
//	sc.Get(Token); Expression(tmp);
//	IF Token = sc.RightBrace THEN sc.Get(Token);
//	ELSE sc.Mark(X.MismatchBraces)
//	END;
//	Result:=Result.Mul(tmp)
//	END
//	*)
//	| sc.If         : sc.Get(Token); Result:=IfCondition();
//	| sc.True       : sc.Get(Token); Result:=XM.one
//	| sc.False      : sc.Get(Token); Result:=XM.zero
//	| sc.Pi         : sc.Get(Token); Result:=XM.Init(X.pi, X.zero)
//	| sc.Complement : Next(); i:=XM.RealToInteger(Result.real); i:=i.Invert(); FixI
//	| sc.Sin        : Next(); Result:=Result.Sin();
//	| sc.Cos        : Next(); Result:=Result.Cos();
//	| sc.Tan        : Next(); Result:=Result.Tan();
//	| sc.ArcSin     : Next(); Result:=Result.Arcsin();
//	| sc.ArcCos     : Next(); Result:=Result.Arccos();
//	| sc.ArcTan     : Next(); Result:=Result.Arctan();
//	| sc.Sinh       : Next(); Result:=Result.Sinh();
//	| sc.Cosh       : Next(); Result:=Result.Cosh();
//	| sc.Tanh       : Next(); Result:=Result.Tanh();
//	| sc.ArcSinh    : Next(); Result:=Result.Arcsinh();
//	| sc.ArcCosh    : Next(); Result:=Result.Arccosh();
//	| sc.ArcTanh    : Next(); Result:=Result.Arctanh();
//	| sc.SquareRoot : Next(); Result:=Result.Sqrt();
//	| sc.CubeRoot   : Next(); Result:=Result.IRoot(3);
//	| sc.NaturalLog : Next(); Result:=Result.Ln();
//	| sc.Log        : Next(); Result:=Result.Log(X.Long(10));
//	| sc.PowerOfe   : Next(); Result:=Result.Exp();
//	| sc.Name       : V.Get(sc.s.var, Result, ok);
//	IF ~ok THEN
//	(* check if this is a function *)
//	f.Get(sc.s.var, str, ok);
//	IF ~ok THEN
//	sc.Mark(cli.Undefined)
//	ELSE
//	(* evaluate function *)
//	sc.Get(Token);
//	Result:=Function(sc.s.var);
//	END
//	ELSE sc.Get(Token);
//	IF Token=sc.LeftBrace THEN (* duplicated name? *)
//	f.Get(sc.s.var, str, ok);
//	IF ~ok THEN
//	sc.Mark(cli.Undefined)
//	ELSE
//	(* evaluate function *)
//	Result:=Function(sc.s.var);
//	END
//	END
//	END
//	| sc.Base       : SaveBase := XM.nState.LocalBase;
//	XM.nState.LocalBase := 10;
//	Next();
//	XM.nState.LocalBase := SHORT(SHORT(ENTIER(Result.real.Short())));
//	IF (XM.nState.LocalBase < 2) OR (XM.nState.LocalBase > 36) THEN
//	XM.nState.LocalBase := SaveBase;
//	END;
//	Result := LastAnswer;
//	| sc.Digits     : Next();
//	IF X.status = X.Okay THEN
//	Digits:=SHORT(ENTIER(Result.real.Short()));
//	X.SetDigits(Digits);
//	Result := LastAnswer;
//	END;
//	| sc.Decimals   : Next();
//	IF X.status = X.Okay THEN
//	XM.nState.DecPoint:=SHORT(SHORT(ENTIER(Result.real.Short())));
//	Result := LastAnswer;
//	END;
//	| sc.Notation   : sc.Get(Token);
//	XM.nState.Notation:=(XM.nState.Notation+1) MOD 3;
//	Result := LastAnswer;
//	| sc.DegRadGrad : sc.Get(Token);
//	IF XM.nState.DegRadFlag = XM.Gradians THEN
//	XM.nState.DegRadFlag := XM.Degrees;
//	ELSE INC(XM.nState.DegRadFlag) END;
//	Result := LastAnswer;
//	| sc.Rat		: sc.Get(Token);
//	XM.nState.Rational := ~XM.nState.Rational;
//	Result := LastAnswer;
//	| sc.List       : sc.Get(Token);
//	IF V.Defined()>0 THEN
//	Out.String("Variables:"); Out.Ln;
//	V.Iterate(Print);
//	Out.Ln;
//	ELSE Out.String("No variables defined."); Out.Ln
//	END;
//	IF f.Defined()>0 THEN
//	Out.String("Functions:"); Out.Ln;
//	f.Iterate(Printf);
//	ELSE Out.String("No functions defined."); Out.Ln
//	END;
//	Result := LastAnswer
//	| sc.Help       : sc.Get(Token);
//	Help;
//	Result := XM.zero
//	| sc.Delete     : sc.Get(Token);
//	IF Token=sc.Name THEN
//	V.Delete(sc.s.var);
//	f.Delete(sc.s.var)
//	ELSE sc.Mark(cli.IllegalVariable)
//	END;
//	Result := XM.zero; sc.Get(Token)
//	| sc.iToken     : sc.Get(Token); Result:=XM.Init(X.zero, X.one)
//	| sc.rToken     : Next(); t:=Result.PolarMag(); FixR
//	| sc.Theta      : Next(); t:=Result.PolarAngle(); FixR
//	| sc.ImagPart   : Next(); Result:=XM.Init(Result.imag, X.zero)
//	| sc.RealPart   : Next(); Result:=XM.Init(Result.real, X.zero)
//	| sc.IntPart    : Next(); t:=Result.real.Entier(); FixR
//	| sc.FracPart   : Next(); t:=Result.real.Fraction(); FixR
//	| sc.SignOf     : Next();
//	IF Result.real.Sign()>0 THEN Result:=XM.Init(X.one, X.zero)
//	ELSE Result:=XM.Init(X.one.Neg(), X.zero)
//	END
//	| sc.Abs        : Next(); Result:=XM.Init(Result.real.Abs(), X.zero)
//	| sc.Min        : Result:=Args(Min)
//	| sc.Max        : Result:=Args(Max)
//	| sc.Sum        : Result:=Args(Add)
//	| sc.Average    : n:=1; tmp:=Args(Avg); Result:=tmp.Div(XM.Long(n))
//	| sc.Multiply   : Result:=Args(Mul)
//	| sc.Conj       : Next(); Result:=Result.Conj()
//	| sc.Rand       : sc.Get(Token); t:=X.Random(); FixR
//	ELSE              sc.Mark(X.IllegalOperator); Result := XM.zero
//	END;
//	IF X.status#X.Okay THEN sc.Mark(X.status) END
//	END Factor;
//	
//	
//	PROCEDURE Powers (VAR Result : XM.Complex);
//	(** 'Result' = <Factor> @{<Power Operator> [Factor]@} *)
//	VAR
//	tmp : XM.Complex;
//	xtmp : X.Real;
//	
//	PROCEDURE Next;
//	BEGIN
//	sc.Get(Token);
//	IF Token = sc.Minus THEN
//	sc.Get(Token); Factor(Result);
//	Result := Result.Neg()
//	ELSE
//	Factor(Result)
//	END;
//	END Next;
//	
//	BEGIN
//	tmp:=XM.zero;
//	Factor(tmp);
//	WHILE (Token>=sc.Power) & (Token<=sc.PolarToRect) DO
//	CASE Token OF
//	sc.Power     : Next(); tmp:=tmp.Power(Result);
//	| sc.Root      : Next(); tmp:=Result.Root(tmp);
//	| sc.Squared   : sc.Get(Token); tmp:=tmp.Mul(tmp);
//	| sc.Cubed     : sc.Get(Token); tmp:=tmp.IPower(3);
//	| sc.Inverse   : sc.Get(Token); tmp:=XM.one.Div(tmp);
//	| sc.Factorial : sc.Get(Token);
//	xtmp:=X.Factorial(ENTIER(tmp.real.Short()));
//	tmp:=XM.Init(xtmp, X.zero);
//	| sc.PercentOf : sc.Get(Token);
//	Result:=tmp.Div(XM.Init(X.Long(100), X.zero));
//	Factor(tmp);
//	tmp:=tmp.Mul(Result);
//	| sc.PolarToRect:Next(); tmp:=XM.ToRectangular(tmp.real, Result.real)
//	ELSE (* skip *)  sc.Mark(X.IllegalOperator); sc.Get(Token);
//	END;
//	END;
//	Result := tmp;
//	IF X.status#X.Okay THEN sc.Mark(X.status) END;
//	END Powers;
//	
//	
//	PROCEDURE Term (VAR Result : XM.Complex);
//	(** 'Result' = <Powers> @{<Term Operator> <Powers>@} *)
//	VAR
//	tmp: XM.Complex;
//	itmp, ti : XI.Integer;
//	
//	PROCEDURE Next;
//	BEGIN
//	sc.Get(Token);
//	IF Token = sc.Minus THEN
//	sc.Get(Token); Powers(Result);
//	Result := Result.Neg()
//	ELSE
//	Powers(Result)
//	END;
//	WITH Result: R.Rational DO
//	ti:=XM.RealToInteger(Result.ToReal())
//	ELSE
//	ti:=XM.RealToInteger(Result.real)
//	END
//	END Next;
//	
//	PROCEDURE ToCard(Ex : XM.Complex) : INTEGER;
//	BEGIN
//	RETURN SHORT(ENTIER(Ex.real.Short()));
//	END ToCard;
//	
//	PROCEDURE Fix;
//	BEGIN
//	tmp:=XM.Init(XM.IntegerToReal(ti), X.zero)
//	END Fix;
//	
//	BEGIN
//	tmp:=XM.zero;
//	Powers(tmp);
//	WHILE (Token>=sc.Times) & (Token<=sc.nPr) OR (Token=sc.Number) DO
//	CASE Token OF
//	sc.Times       : Next(); tmp:=tmp.Mul(Result)
//	| sc.Number      : tmp:=tmp.Mul(Result); Next()
//	| sc.Divide      : Next(); tmp:=tmp.Div(Result)
//	| sc.Div         : Next(); itmp:=XM.RealToInteger(tmp.real); ti:=itmp.Div(ti); Fix
//	| sc.Mod         : Next(); itmp:=XM.RealToInteger(tmp.real); ti:=itmp.Mod(ti); Fix
//	| sc.And         : Next(); itmp:=XM.RealToInteger(tmp.real); ti:=itmp.And(ti); Fix
//	| sc.ShiftRight  : Next(); itmp:=XM.RealToInteger(tmp.real); ti:=itmp.RShift(ToCard(Result)); Fix
//	| sc.AShiftRight : Next(); itmp:=XM.RealToInteger(tmp.real); ti:=itmp.RShift(ToCard(Result)); Fix
//	| sc.RotateRight : Next(); itmp:=XM.RealToInteger(tmp.real); ti:=itmp.RShift(ToCard(Result)); Fix
//	| sc.ShiftLeft   : Next(); itmp:=XM.RealToInteger(tmp.real); ti:=itmp.LShift(ToCard(Result)); Fix
//	| sc.RotateLeft  : Next(); itmp:=XM.RealToInteger(tmp.real); ti:=itmp.LShift(ToCard(Result)); Fix
//	| sc.ClearBit    : Next(); itmp:=XM.RealToInteger(tmp.real); ti:=itmp.ClearBit(ToCard(Result)); Fix
//	| sc.SetBit      : Next(); itmp:=XM.RealToInteger(tmp.real); ti:=itmp.SetBit(ToCard(Result)); Fix
//	| sc.ToggleBit   : Next(); itmp:=XM.RealToInteger(tmp.real); ti:=itmp.ToggleBit(ToCard(Result)); Fix
//	| sc.nCr         : Next(); tmp:=Combinations(tmp, Result)
//	| sc.nPr         : Next(); tmp:=Permutations(tmp, Result)
//	ELSE
//	sc.Mark(X.IllegalOperator); sc.Get(Token)
//	END
//	END;
//	Result := tmp;
//	IF X.status#X.Okay THEN sc.Mark(X.status) END;
//	END Term;
//	
//	
//	PROCEDURE SimpleExpression (VAR Result : XM.Complex);
//	(** 'Result' = [+|-] <Term> @{+|- <Term>@} *)
//	VAR
//	tmp : XM.Complex;
//	ti  : XI.Integer;
//	ires : XI.Integer;
//	
//	PROCEDURE Next(VAR Result : XM.Complex);
//	BEGIN
//	sc.Get(Token);
//	IF Token = sc.Minus THEN
//	sc.Get(Token); Term(Result);
//	Result := Result.Neg()
//	ELSE
//	Term(Result)
//	END;
//	WITH tmp: R.Rational DO
//	ires:=XM.RealToInteger(tmp.ToReal())
//	ELSE
//	ires:=XM.RealToInteger(tmp.real)
//	END
//	END Next;
//	
//	PROCEDURE Fix;
//	BEGIN
//	tmp:=XM.Init(XM.IntegerToReal(ti), X.zero)
//	END Fix;
//	
//	BEGIN
//	tmp:=XM.zero;
//	CASE Token OF
//	sc.Plus  : Next(tmp);
//	| sc.Minus : Next(tmp); tmp:=tmp.Neg()
//	ELSE       Term(tmp)
//	END;
//	WHILE (Token >= sc.Plus) & (Token <= sc.Xor) DO
//	CASE Token OF
//	sc.Plus  : Next(Result); tmp:=tmp.Add(Result);
//	| sc.Minus : Next(Result); tmp:=tmp.Sub(Result);
//	| sc.Or    : Next(Result); ti:=ires.Or(XM.RealToInteger(Result.real)); Fix
//	| sc.Xor   : Next(Result); ti:=ires.Xor(XM.RealToInteger(Result.real)); Fix
//	ELSE         Term(tmp);
//	END;
//	END;
//	Result := tmp;
//	IF X.status#X.Okay THEN sc.Mark(X.status) END
//	END SimpleExpression;
//	
//	
//	PROCEDURE Expression (VAR Result : XM.Complex);
//	(** 'Result' = <Expression> @{+|- <Term>@} *)
//	VAR
//	tmp : XM.Complex;
//	
//	PROCEDURE Next(VAR Result : XM.Complex);
//	BEGIN
//	sc.Get(Token);
//	SimpleExpression(Result)
//	END Next;
//	
//	BEGIN
//	SimpleExpression(tmp);
//	IF (Token >= sc.Greater) & (Token <= sc.Assign) THEN
//	CASE Token OF
//	sc.Greater      : Next(Result);
//	IF tmp.Cmp(Result)=1 THEN tmp:=XM.one ELSE tmp:=zero END
//	| sc.GreaterEqual : Next(Result);
//	IF tmp.Cmp(Result)>=0 THEN tmp:=XM.one ELSE tmp:=zero END
//	| sc.Less         : Next(Result);
//	IF tmp.Cmp(Result)<0 THEN tmp:=XM.one ELSE tmp:=zero END
//	| sc.LessEqual    : Next(Result);
//	IF tmp.Cmp(Result)<=0 THEN tmp:=XM.one ELSE tmp:=zero END
//	| sc.NotEqual     : Next(Result);
//	IF tmp.Cmp(Result)#0 THEN tmp:=XM.one ELSE tmp:=zero END
//	| sc.Assign       : Next(Result);
//	IF tmp.Cmp(Result)=0 THEN tmp:=XM.one ELSE tmp:=zero END
//	ELSE                sc.Mark(X.IllegalOperator); sc.Get(Token)
//	END;
//	END;
//	Result:=tmp;
//	IF X.status#X.Okay THEN sc.Mark(X.status) END
//	END Expression;
//	
//	
//	PROCEDURE Evaluate* (arg: STRING; VAR Result: XM.Complex): BOOLEAN;
//	(** Evaluate 'arg' input and return the result in 'Result'.  The
//	function returns true if there were no evaluation errors. *)
//	VAR
//	name: STRING;
//	ok: BOOLEAN;
//	r: XM.Complex;
//	BEGIN
//	CommandLine:=arg;              (* remember this string for later        *)
//	X.status := X.Okay;            (* clear out any previous errors         *)
//	X.err := 0;
//	sc.Init(arg); sc.Get(Token);   (* start things off with the first token *)
//	
//	IF Token = sc.Let THEN         (* define a variable or a function *)
//	sc.Get(Token);
//	IF Token # sc.Name THEN sc.Mark(cli.ExpectingName); name:="Error"
//	ELSE name:=sc.s.var
//	END;
//	sc.Get(Token);
//	IF Token = sc.LeftBrace THEN
//	(* defining a function *)
//	sc.Get(Token);
//	f.Set(name);                (* define a new function *)
//	WHILE Token = sc.Name DO    (* define the argument list *)
//	f.AddArg(name, sc.s.var, ok);
//	IF ~ok THEN sc.Mark(cli.IllegalArg) END;
//	sc.Get(Token);
//	IF Token = sc.Semi THEN sc.Get(Token) END
//	END;
//	IF Token # sc.RightBrace THEN sc.Mark(cli.ExpectingRBrace) END;
//	sc.Get(Token);
//	IF Token # sc.Assign THEN sc.Mark(cli.ExpectingAssign) END;
//	f.SetEquation(name, sc.GetString());
//	r:=XM.zero
//	ELSE
//	(* defining a variable *)
//	IF Token # sc.Assign THEN sc.Mark(cli.ExpectingAssign) END;
//	sc.Get(Token);
//	Expression(r);
//	StoreVariable(name, r)
//	END;
//	Token:=sc.Empty;
//	ELSE Expression(r)
//	END;
//	IF Token#sc.Empty THEN
//	sc.Mark(cli.IllegalExpression)
//	END;
//	Result:=r;
//	LastAnswer:=r;
//	RETURN X.status=X.Okay
//	END Evaluate;
//	
//	
//	PROCEDURE Store* (w: Storable.Writer) RAISES IO.Error;
//	(** Store the defined functions to writer 'w' *)
//	BEGIN
//	f.Store(w);
//	IF LastAnswer IS R.Rational THEN w.WriteLInt(1) ELSE w.WriteLInt(0) END;
//	LastAnswer.Store(w)
//	END Store;
//	
//	
//	PROCEDURE Load * (r: Storable.Reader) RAISES IO.Error;
//	(** Load functions from the reader 'r' *)
//	VAR
//	type: LONGINT;
//	n: R.Rational;
//	BEGIN
//	f.Load(r);
//	r.ReadLInt(type);
//	IF type = 1 THEN NEW(n); n.Load(r); LastAnswer := n
//	ELSE NEW(LastAnswer); LastAnswer.Load(r)
//	END
//	END Load;
//	
//	
//	BEGIN
//	Token:=sc.Empty;
//	zero:=XM.Init(X.zero, X.zero);
//	LastAnswer:=XM.Init(X.zero, X.zero);
	END Equations.
	
	
}