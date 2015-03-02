MODULE Rationals;

(*
    Implements rational number operations with unlimited size.
    Copyright (c) 2010 Michael Griebling
 
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

*)

IMPORT XI := Integers, R := Reals, C := Complex,
       ADT:Storable, Object, IO, io := Out;

TYPE
  Rational * = POINTER TO RationalDesc;
  RationalDesc * = RECORD (C.ComplexDesc)
    num-, den- : XI.Integer
  END;

PROCEDURE (a: Rational) Format* (): STRING;
BEGIN
	RETURN a.num.Format(10) + "/" + a.den.Format(10)
END Format;

PROCEDURE (x: Rational) ToString*(): STRING;
BEGIN
  	RETURN x.Format()
END ToString;

PROCEDURE Normalize (VAR n, d : XI.Integer);
VAR
  z: XI.Integer;
BEGIN
	(* normalize signs *)
	IF n.Sign() # d.Sign() THEN
		IF d.Sign() < 0 THEN d := d.Abs(); n := n.Neg(); END
	ELSIF d.Sign() < 0 THEN
		d := d.Abs(); n := n.Abs();
	END;
	
	(* reduce the fraction to its smallest rational denominator *)
	z := n.GCD(d);
	n := n.Div(z); d := d.Div(z);
END Normalize;

PROCEDURE Init * (n, d : XI.Integer) : Rational;
VAR
  	A: Rational;
  	r: R.Real;
BEGIN
	Normalize(n, d);
  	NEW(A); A.num:=n; A.den:=d;
  	r := C.IntegerToReal(n);
  	A.Init^(r.Div(C.IntegerToReal(d)), R.zero);	(* also initialize complex components *)
  	RETURN A
END Init;

PROCEDURE RInit * (n : R.Real; d : LONGINT) : Rational;
BEGIN
  	RETURN Init(C.RealToInteger(n), XI.NewInt(d));
END RInit;

PROCEDURE New* (str: STRING): Rational;
VAR
	pos: LONGINT;
	str2 : STRING;
BEGIN
	pos := str.IndexOf("/", 0);
	IF pos < 0 THEN
    	RETURN Init(XI.New(str.Trim(), 10), XI.one);
    ELSE
    	str2 := str.Substring(pos+1, str.length);
    	str := str.Substring(0, pos);
    	RETURN Init(XI.New(str.Trim(), 10), XI.New(str2.Trim(), 10));
    END
END New;

PROCEDURE NewRational* (n, d : LONGINT): Rational;
BEGIN
	RETURN Init(XI.NewInt(n), XI.NewInt(d))
END NewRational;

PROCEDURE (a: Rational) Add* (b: C.Complex): Rational;
VAR
	z: XI.Integer;
BEGIN
	(* Ok, you caught me, this isn't mathematically correct because
	   Rational numbers aren't a superset of Complex numbers but it
	   lets me support Rational numbers fairly compactly with
	   minimal disruption. *)
	WITH b:Rational DO 
		z := a.num.Mul(b.den);
		RETURN Init(z.Add(b.num.Mul(a.den)), a.den.Mul(b.den));
	ELSE
		RETURN a.Add(RInit(b.real, 1)) 
	END;
END Add;

PROCEDURE (a: Rational) Sub* (b: C.Complex): Rational;
VAR
	z: XI.Integer;
BEGIN
	WITH b:Rational DO 
		z := a.num.Mul(b.den);
		RETURN Init(z.Sub(b.num.Mul(a.den)), a.den.Mul(b.den));
	ELSE
		RETURN a.Sub(RInit(b.real, 1)) 
	END;
END Sub;

PROCEDURE (a: Rational) Mul* (b: C.Complex): Rational;
BEGIN
	WITH b:Rational DO 
		RETURN Init(a.num.Mul(b.num), a.den.Mul(b.den));
	ELSE
		RETURN a.Mul(RInit(b.real, 1)) 
	END;
END Mul;
  
PROCEDURE (a: Rational) Div* (b: C.Complex):Rational;
BEGIN
	WITH b:Rational DO 
		RETURN Init(a.num.Mul(b.den), a.den.Mul(b.num));
	ELSE
		RETURN a.Div(RInit(b.real, 1)) 
	END;
END Div;

PROCEDURE (a: Rational) Store*(w: Storable.Writer) RAISES IO.Error;
  BEGIN
  	a.num.Store(w); a.den.Store(w);
    a.Store^(w);    (* store parent parameters *)
  END Store;

PROCEDURE (a: Rational) Load*(r: Storable.Reader) RAISES IO.Error;
  BEGIN  
  	NEW(a.num); NEW(a.den);
    a.num.Load(r); a.den.Load(r);
  	a.Load^(r);				(* read parent parameters *)
  END Load;

PROCEDURE (a: Rational) Equals* (b: Object.Object): BOOLEAN;
  BEGIN
    WITH b: Rational DO
      IF a.num.Equals(b.num) & a.den.Equals(b.den) THEN
        RETURN TRUE;
      ELSE
        RETURN FALSE;
      END;
    ELSE
      RETURN FALSE;
    END;
  END Equals;

PROCEDURE (a: Rational) IsZero*(): BOOLEAN;
  BEGIN
    RETURN a.num.IsZero();
  END IsZero;

PROCEDURE (a: Rational) NonZero*(): BOOLEAN;
  BEGIN
    RETURN ~a.num.IsZero();
  END NonZero;

PROCEDURE (a: Rational) ToReal*(): R.Real;
  VAR
  	ra : R.Real;
  BEGIN
  	ra := C.IntegerToReal(a.num);
  	RETURN ra.Div(C.IntegerToReal(a.den)); 
  END ToReal; 
  
PROCEDURE (a: Rational) Sign*(): LONGINT;
  BEGIN
    IF (a.num.IsZero()) THEN RETURN 0;
    ELSE RETURN a.num.Sign();
    END;
  END Sign;

PROCEDURE (a: Rational) Cmp* (b: Object.Object): LONGINT;
  VAR
  	ra, rb : R.Real;
  BEGIN
    WITH b: Rational DO
    	ra := a.ToReal();
    	rb := b.ToReal();
		RETURN ra.Cmp(rb);
    END;
  END Cmp;

PROCEDURE (a: Rational) Abs*(): Rational;
  BEGIN
  	RETURN Init(a.num.Abs(), a.den);
  END Abs;

PROCEDURE (a: Rational) Neg*(): Rational;
  BEGIN
    RETURN Init(a.num.Neg(), a.den);
  END Neg;
  
PROCEDURE Test;
VAR
  a, b : Rational;
BEGIN
	a := NewRational(1, -2); b := NewRational(-6, 16);
	io.Object(a); io.String(" + "); io.Object(b); io.String(" = "); io.Object(a.Add(b)); io.Ln;
	io.Object(a); io.String(" - "); io.Object(b); io.String(" = "); io.Object(a.Sub(b)); io.Ln;
	io.Object(a); io.String(" * "); io.Object(b); io.String(" = "); io.Object(a.Mul(b)); io.Ln;
	io.Object(a); io.String(" / "); io.Object(b); io.String(" = "); io.Object(a.Div(b)); io.Ln;
	io.String("Real("); io.Object(a); io.String(") = "); io.Object(a.ToReal()); io.Ln;
	io.String("Real("); io.Object(b); io.String(") = "); io.Object(b.ToReal()); io.Ln;
	io.String("Abs("); io.Object(a); io.String(") = "); io.Object(a.Abs()); io.Ln;
	io.Object(a);
	IF a.Cmp(b) > 0 THEN io.String(" > ");
	ELSIF a.Cmp(b) = 0 THEN io.String(" = ");
	ELSE io.String(" < ")
	END;
	io.Object(b); io.Ln;
	
	a := New("12345678901234567890 / -234567890");
	io.String("12345678901234567890 / -234567890 = "); io.Object(a); io.Ln;
END Test;

BEGIN
  Test
END Rationals.