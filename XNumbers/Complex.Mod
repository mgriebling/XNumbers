MODULE Complex;

 (*
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

*)

IMPORT X := Reals, XI := Integers, ADT:Storable, Object:Boxed, Object, Out, IO;

CONST
  Normal*=0; Scientific*=1; Engineering*=2;
  Degrees*=0; Radians*=1; Gradians*=2;  

TYPE
  Complex * = POINTER TO ComplexDesc;
  ComplexDesc * = RECORD (Boxed.ObjectDesc)
    real-, imag- : X.Real
  END;
  
  NumbState * = RECORD
    LocalBase*  : SHORTINT;
    Notation*   : SHORTINT;
    DecPoint*   : SHORTINT;
    DegRadFlag* : SHORTINT;
    DigSep*     : CHAR;
    FracSep*    : CHAR;
    Rational*   : BOOLEAN;
  END;

VAR
  zero-, one- : Complex;
  nState* : NumbState;
  
PROCEDURE IsZero (A : X.Real) : BOOLEAN;
BEGIN
  RETURN A.Cmp(X.zero)=0
END IsZero;

PROCEDURE IsNegative (A : X.Real) : BOOLEAN;
BEGIN
  RETURN A.Cmp(X.zero)=-1
END IsNegative;

(*---------------------------------------------------------*)
(*                                                         *)
(* The following routines are intended for external users  *)
(*                                                         *)
(*---------------------------------------------------------*)
(* Constructors                                            *)

PROCEDURE RealToInteger * (A : X.Real) : XI.Integer;
CONST
  MAXC = 40000000H;
VAR
  x, iMAX : XI.Integer;
  a, MAX : X.Real;
  exp : LONGINT;
  neg : BOOLEAN;
BEGIN
  A:=A.Entier();
  x:=XI.NewInt(0); exp:=0; 
  MAX:=X.Long(MAXC); iMAX:=XI.NewInt(MAXC);
  neg:=FALSE;
  IF IsNegative(A) THEN A:=A.Abs(); neg:=TRUE END;  
  WHILE A.Cmp(MAX) = 1 DO
    A:=A.Div(MAX);
    INC(exp)
  END;     
  REPEAT
    a:=A.Entier(); x:=x.Mul(iMAX);
    x:=x.Add(XI.NewInt(ENTIER(a.Short())));
    A:=MAX.Mul(A.Fraction());
    DEC(exp) 
  UNTIL exp<0;   
  IF neg THEN x:=XI.zero.Sub(x) END;  
  RETURN x
END RealToInteger;

PROCEDURE IntegerToReal * (A : XI.Integer) : X.Real;
CONST
  MAXC = 1000000000;
VAR
  iMAX, ia : XI.Integer;
  x, y, MAX : X.Real;
  neg : BOOLEAN;
BEGIN
  x:=X.Long(0);
  MAX:=X.Long(MAXC); iMAX:=XI.NewInt(MAXC);
  neg:=FALSE; y:=X.Long(1);
  IF A.Sign() = -1 THEN A:=A.Abs(); neg:=TRUE END;
  WHILE A.Sign() = 1 DO
    ia:=A.Mod(iMAX);
    x:=x.Add(y.Mul(X.Long(ia.ToLongInt())));
    A:=A.Div(iMAX); y:=y.Mul(MAX)
  END;
  IF neg THEN x:=X.zero.Sub(x) END; 
  RETURN x
END IntegerToReal;

(*---------------------------------------------------------*)
(* Conversion routines                                     *)

PROCEDURE (A: Complex) Format * () : STRING;
VAR
  Result : STRING;
  iZero, rZero: BOOLEAN;
  EngFormat: BOOLEAN;
  i, ds, dp: LONGINT;
  ExpWidth: LONGINT;
  cs: ARRAY 2 OF CHAR;
  Str: STRING;
  Aim: X.Real;
  
  PROCEDURE NumToStr (A : X.Real; Decimal, ExpWidth : LONGINT; EngFormat: BOOLEAN) : STRING;
  (* Combines radix and normal conversions *)
  VAR
    ia: XI.Integer;
  BEGIN
    IF nState.LocalBase=10 THEN
      RETURN A.Format(Decimal, ExpWidth, EngFormat)
    ELSE
      IF nState.LocalBase#8 THEN ds:=4 END;
      ia:=RealToInteger(A);
      RETURN ia.Format(nState.LocalBase)
    END
  END NumToStr;
  
  PROCEDURE SetChar (s: STRING; pos: LONGINT; ch: CHAR) : STRING;
  VAR
    f: STRING;
  BEGIN
    f:=s.Substring(0, pos-1); f:=f.Concat(Object.NewLatin1Char(ch));
    f:=f.Concat(s.Substring(pos+1, s.length));
    RETURN f
  END SetChar;
  
  PROCEDURE InsertChar (s: STRING; pos: LONGINT; ch: CHAR) : STRING;
  VAR
    f: STRING;
  BEGIN
    f:=s.Substring(0, pos-1); f:=f.Concat(Object.NewLatin1Char(ch));
    f:=f.Concat(s.Substring(pos, s.length));
    RETURN f
  END InsertChar;  
  
BEGIN
  (* round numbers *)
  A.real:=X.Sub2(A.real.Add(X.one), X.one);
  A.imag:=X.Sub2(A.imag.Add(X.one), X.one);

  iZero := IsZero(A.imag); rZero := IsZero(A.real);
  ds:=3; 
  IF nState.Notation>Normal THEN ExpWidth:=1 ELSE ExpWidth:=0 END;
  EngFormat:=nState.Notation=Engineering;
  IF iZero OR ~rZero THEN
    Str:=NumToStr(A.real, nState.DecPoint, ExpWidth, EngFormat) (* real part *)
  ELSE Str:=""
  END; 
  IF ~iZero THEN
    Aim:=A.imag;
    IF ~IsNegative(Aim) THEN
      IF ~rZero THEN Str:=Str.Concat("+") END
    ELSE Str:=Str.Concat("-")
    END;
    Aim:=Aim.Abs();
    IF Aim.Cmp(X.one) # 0 THEN
      Result:=NumToStr(Aim, nState.DecPoint, ExpWidth, EngFormat);  (* imaginary part *)
      Str:=Str.Concat(Result)
    END;
    Str:=Str.Concat("i")
  END;

  (* add digit separators *)
  cs[1]:=0X;
  dp:=Str.IndexOf(".", 0);                  (* find location of decimal point *)    
  IF dp<0 THEN dp:=Str.length
  ELSIF nState.DigSep="." THEN Str:=SetChar(Str, dp, ",")
  END;
  IF nState.DigSep # 0X THEN
    i:=dp-ds;                               (* insert whole separator *)
    WHILE i>0 DO
      Str:=InsertChar(Str, i, nState.DigSep);
      DEC(i, ds); INC(dp)
    END
  END;
  IF nState.FracSep # 0X THEN
    i:=dp+ds+1;                             (* insert fraction separator *)
    WHILE i<Str.length DO
      Str:=InsertChar(Str, i, nState.DigSep); 
      INC(i, ds+1)
    END
  END;
  RETURN Str
END Format; 

PROCEDURE (x: Complex) ToString*(): STRING;
BEGIN
  RETURN x.Format()
END ToString;

PROCEDURE (x: Complex) Init * (re, im : X.Real);
BEGIN
  x.real := re; x.imag := im
END Init;

PROCEDURE Init * (re, im : X.Real) : Complex;
VAR
  A: Complex;
BEGIN
  NEW(A); A.Init(re, im);
  RETURN A
END Init;


PROCEDURE (x: Complex) Conj * () : Complex;
BEGIN
  RETURN Init(x.real, X.zero.Sub(x.imag))
END Conj;


PROCEDURE (B: Complex) PolarMag * () : X.Real;
VAR
  A: X.Real;
BEGIN
  IF IsZero(B.imag) THEN RETURN B.real.Abs()
  ELSIF IsZero(B.real) THEN RETURN B.imag.Abs()
  ELSE
    A:=B.imag.Mul(B.imag);
    A:=A.Add(B.real.Mul(B.real));
    RETURN A.Sqrt()
  END
END PolarMag;

PROCEDURE^ fromRadians (radianAngle: X.Real) : X.Real;

PROCEDURE (B: Complex) PolarAngle * () : X.Real;
BEGIN
  RETURN fromRadians(B.imag.Arctan2(B.real))
END PolarAngle;

PROCEDURE^ toRadians (radianAngle : X.Real) : X.Real;

PROCEDURE Long * (x : LONGREAL) : Complex;
BEGIN
  RETURN Init(X.Long(x), X.zero)
END Long;


PROCEDURE (xc : Complex) Short * () : LONGREAL;
BEGIN
  RETURN xc.real.Short()
END Short;


PROCEDURE (x: Complex) Neg * () : Complex;
BEGIN
  RETURN Init(X.zero.Sub(x.real), X.zero.Sub(x.imag))
END Neg;


PROCEDURE (B : Complex) Add * (C : Complex) : Complex;
BEGIN
  (** A = B + C = (a + bi) + (c + di) = (a+c) + (b+d)i *)
  RETURN Init(B.real.Add(C.real), B.imag.Add(C.imag))
END Add;


PROCEDURE (B : Complex) Sub * (C : Complex) : Complex;
BEGIN
  (** A = B - C = (a + bi) - (c + di) = (a-c) + (b-d)i *)
  RETURN Init(B.real.Sub(C.real), B.imag.Sub(C.imag))
END Sub;


PROCEDURE ToRectangular * (r, theta : X.Real) : Complex;
VAR
  s, c: X.Real;
  t: Complex;
BEGIN
  s:=toRadians(theta); s.SinCos(s, c);
  t:=one.Add(Init(r.Mul(c), r.Mul(s)));   (* force rounding *)
  RETURN t.Sub(one)
END ToRectangular;


PROCEDURE (B : Complex) Mul * (C : Complex) : Complex;
VAR
  r, i: X.Real;
BEGIN
  (* A = B * C = (a + bi) * (c + di) = (ac-bd) + (ad+bc)i *)
  r:=B.real.Mul(C.real); i:=B.real.Mul(C.imag);
  RETURN Init(r.Sub(B.imag.Mul(C.imag)), i.Add(B.imag.Mul(C.real)))
END Mul;


PROCEDURE (B : Complex) Div * (C : Complex) : Complex;
VAR div, r, i : X.Real;
BEGIN
  (**
  		A = B / C = (a + bi) / (c + di) = (ac+bd)/e + (bc-ad)/e i
             where e = c^2+d^2  
   *)
  div:=C.real.Mul(C.real); div:=div.Add(C.imag.Mul(C.imag));
  r:=B.real.Mul(C.real); r:=r.Add(B.imag.Mul(C.imag)); 
  i:=B.imag.Mul(C.real); i:=i.Sub(B.real.Mul(C.imag));
  RETURN Init(r.Div(div), i.Div(div))
END Div;

PROCEDURE (a: Complex) Cmp * (b: Object.Object) : LONGINT;
(**
     This routine compares the extended numbers `a' and `b' and
     returns the value -1, 0, or 1 depending on whether 'a'<'b',
     'a'='b', or 'a'>'b'.  It is faster than merely subtracting
     'a' and 'b' and looking at the sign of the result.
 *)
VAR
  c: X.Real;
BEGIN
  IF IsZero(a.imag) THEN c:=a.real ELSE c:=a.PolarMag() END;
  WITH b: Complex DO
    IF IsZero(b.imag) THEN RETURN c.Cmp(b.real) 
    ELSE RETURN c.Cmp(a.PolarMag())
    END
  END
END Cmp;

PROCEDURE (a: Complex) Store* (w: Storable.Writer) RAISES IO.Error;
(** Write 'a' to the 'w' writer. *)
  BEGIN
    a.real.Store(w); a.imag.Store(w);  
  END Store;

PROCEDURE (a: Complex) Load*(r: Storable.Reader) RAISES IO.Error;
(** Read 'a' from the 'r' reader. *)
  BEGIN
    NEW(a.real); NEW(a.imag);
    a.real.Load(r); a.imag.Load(r);
  END Load;

(*---------------------------------------------------------*)
(* Power and transcendental routines                       *)
  
PROCEDURE (x : Complex) IPower * (i : LONGINT) : Complex;
VAR
  Y : Complex;
  negative : BOOLEAN;
BEGIN
  Y := one;
  negative := i < 0;
  i := ABS(i);
  LOOP
    IF ODD(i) THEN Y:=Y.Mul(x) END;
    i := i DIV 2;
    IF i = 0 THEN EXIT END;
    x:=x.Mul(x);
  END;
  IF negative THEN
    RETURN one.Div(Y);
  ELSE
    RETURN Y
  END;
END IPower;


PROCEDURE (x : Complex) IRoot * (i : LONGINT) : Complex;
VAR r, theta: X.Real; izero: BOOLEAN;
BEGIN
  izero:=IsZero(x.imag);
  IF izero & ~IsNegative(x.real) THEN
    RETURN Init(x.real.IRoot(i), X.zero)
  ELSIF izero & ODD(i) THEN
    r:=x.real.Abs(); r:=r.IRoot(i);
    RETURN Init(X.zero.Sub(r), X.zero)
  ELSE 
    r:=x.PolarMag(); theta:=x.PolarAngle();
    RETURN ToRectangular(r.IRoot(i), theta.Div(X.Long(i)))
  END
END IRoot;


PROCEDURE fromRadians (radianAngle: X.Real) : X.Real;
VAR
  K180, K200: X.Real;
BEGIN
  (* conversion to/from radian angles *)
  IF nState.DegRadFlag=Degrees THEN
    K180:=X.Long(180);  
    RETURN radianAngle.Mul(K180.Div(X.pi))
  ELSIF nState.DegRadFlag=Gradians THEN
    K200:=X.Long(200);
    RETURN radianAngle.Mul(K200.Div(X.pi))
  ELSE
    RETURN radianAngle
  END
END fromRadians;


PROCEDURE toRadians (radianAngle : X.Real) : X.Real;
(* Convert an angular measure into radians *)
BEGIN
  (* conversion to/from radian angles *)
  IF nState.DegRadFlag=Degrees THEN
    RETURN radianAngle.Mul(X.pi.Div(X.Long(180)))
  ELSIF nState.DegRadFlag=Gradians THEN
    RETURN radianAngle.Mul(X.pi.Div(X.Long(200)))
  ELSE
    RETURN radianAngle
  END
END toRadians;


PROCEDURE (x: Complex) fromRadians * () : Complex;
(* Convert a radian measure into current angle measure *)
BEGIN
  RETURN Init(fromRadians(x.real), fromRadians(x.imag))
END fromRadians;


PROCEDURE (x: Complex) toRadians*() : Complex;
(* Convert an angle measure into radians *)
BEGIN
  RETURN Init(toRadians(x.real), toRadians(x.imag))
END toRadians;


PROCEDURE (x : Complex) Sqrt* () : Complex;
BEGIN
  RETURN x.IRoot(2);
END Sqrt;


PROCEDURE (x : Complex) Ln* () : Complex;
VAR 
  pos : BOOLEAN;
  t, half: X.Real;
BEGIN
  IF IsZero(x.imag) THEN  
    pos := ~IsNegative(x.real);
    t:=x.real.Abs();     
    IF pos THEN RETURN Init(t.Ln(), X.zero)
    ELSE RETURN Init(t.Ln(), X.pi) 
    END
  ELSIF IsZero(x.real) THEN 
    pos := ~IsNegative(x.imag);
    t:=x.imag.Abs(); half:=X.Long(0.5);
    IF ~pos THEN
      RETURN Init(t.Ln(), X.zero.Sub(half.Mul(X.pi)))
    ELSE
      RETURN Init(t.Ln(), half.Mul(X.pi))
    END
  ELSE
    t:=x.PolarMag();
    RETURN Init(t.Ln(), x.PolarAngle())
  END
END Ln;


PROCEDURE (x : Complex) Log* (base: X.Real) : Complex;
VAR
  lne : X.Real; (* log e *)
BEGIN
  lne:=X.one.Exp();  (* e *)
  lne:=lne.Log();
  x:=x.Ln();
  RETURN Init(x.real.Mul(lne), x.imag.Mul(lne))
END Log;


PROCEDURE (x : Complex) Exp* () : Complex;
BEGIN
  RETURN ToRectangular(x.real.Exp(), x.imag)
END Exp;


PROCEDURE (x : Complex) Power* (y : Complex) : Complex;
VAR p : LONGREAL;
BEGIN
  IF IsZero(y.imag) & ~IsNegative(y.real) THEN (* just real powers *)
    IF IsZero(x.imag) & ~IsNegative(x.real) THEN (* real numbers *)
      RETURN Init(x.real.Power(y.real), X.zero)
    ELSE 
      p:=y.Short();
      IF (ABS(p) < MAX(LONGINT)) & IsZero(y.real.Fraction()) THEN 
        RETURN x.IPower(ENTIER(p))
      ELSE x:=y.Mul(x.Ln()); RETURN x.Exp()
      END
    END
  ELSE x:=y.Mul(x.Ln()); RETURN x.Exp()
  END
END Power;


PROCEDURE (x : Complex) Root* (y : Complex) : Complex;
VAR r:LONGREAL;
BEGIN
  IF IsZero(y.imag) & ~IsNegative(y.real) THEN (* just real roots *)
    IF IsZero(x.imag) & ~IsNegative(x.real) THEN (* real numbers *)
      RETURN Init(x.real.IRoot(ENTIER(y.real.Short())), X.zero)
    ELSE r:=y.Short();
      IF (ABS(r) < MAX(LONGINT)) & IsZero(y.real.Fraction()) THEN 
        RETURN x.IRoot(ENTIER(r))
      ELSE x:=x.Ln(); x:=x.Div(y); RETURN x.Exp()
      END
    END
  ELSE x:=x.Ln(); x:=x.Div(y); RETURN x.Exp()
  END
END Root;


PROCEDURE (x : Complex) Sin* () : Complex;
VAR
  s,c,sh,ch: X.Real;
BEGIN
  s:=toRadians(x.real);
  s.SinCos(s, c); x.imag.SinhCosh(sh, ch);
  RETURN Init(s.Mul(ch), c.Mul(sh))
END Sin;


PROCEDURE (x : Complex) Cos* () : Complex;
VAR
  s,c,sh,ch: X.Real;
BEGIN
  s:=toRadians(x.real);
  s.SinCos(s, c); x.imag.SinhCosh(sh, ch);
  RETURN Init(c.Mul(ch), X.zero.Sub(s.Mul(sh)))
END Cos;


PROCEDURE (x : Complex) SinCosC* (VAR sin, cos: Complex);
VAR
  s,c,sh,ch: X.Real;
BEGIN
  s:=toRadians(x.real);
  s.SinCos(s, c); x.imag.SinhCosh(sh, ch);
  sin:=Init(s.Mul(ch), c.Mul(sh));
  cos:=Init(s.Mul(c), X.zero.Sub(sh.Mul(sh)))
END SinCosC;


PROCEDURE (x : Complex) Tan* () : Complex;
VAR TWO, d, s, c, sh, ch: X.Real;
BEGIN
  TWO:=X.Long(2.0); s:=toRadians(x.real);
  s:=TWO.Mul(s); sh:=TWO.Mul(x.imag);
  s.SinCos(s, c); sh.SinhCosh(sh, ch);  
  d:=ch.Add(c);
  RETURN Init(s.Div(d), sh.Div(d))
END Tan;


PROCEDURE CalcAlphaBeta (z : Complex; VAR a, b: X.Real);
VAR x, x2, y, r, t, HALF: X.Real;
BEGIN
  HALF:=X.Long(0.5);
  x:=z.real.Add(X.one); x:=x.Mul(x); y:=z.imag.Mul(z.imag);
  x2:=z.real.Sub(X.one); x2:=x2.Mul(x2);
  t:=x.Add(y); r:=t.Sqrt(); t:=x2.Add(y);
  t:=t.Sqrt();
  a:=HALF.Mul(r.Add(t));
  b:=r.Sub(t); b:=b.Mul(HALF)
END CalcAlphaBeta;


PROCEDURE (x : Complex) Arcsin* () : Complex;
VAR a, b, t: X.Real;
BEGIN
  CalcAlphaBeta(x, a, b);
  t:=a.Mul(a); t:=t.Sub(X.one); t:=a.Add(t.Sqrt());
  RETURN Init(fromRadians(b.Arcsin()), t.Ln())
END Arcsin;

PROCEDURE (x : Complex) Arccos* () : Complex;
VAR a, b, t: X.Real;
BEGIN
  CalcAlphaBeta(x, a, b);
  t:=a.Mul(a); t:=t.Sub(X.one); t:=a.Add(t.Sqrt());  
  RETURN Init(fromRadians(b.Arccos()), X.zero.Sub(t.Ln()))
END Arccos;


PROCEDURE (z : Complex) Arctan* () : Complex;
VAR x, x2, y2, y, yp, TWO, HALF, QUARTER, t: X.Real;
BEGIN
  TWO:=X.Long(2.0); HALF:=X.Long(0.5); QUARTER:=X.Long(0.25);
  x:=TWO.Mul(z.real); y:=z.imag.Add(X.one); y:=y.Mul(y);
  yp:=z.imag.Sub(X.one); yp:=yp.Mul(yp);
  x2:=z.real.Mul(z.real); y2:=z.imag.Mul(z.imag);
  t:=X.one.Sub(x2); t:=t.Sub(y2); t:=x.Div(t);
  x:=HALF.Mul(t.Arctan());
  t:=x2.Add(y); t:=t.Div(x2.Add(yp));
  y:=QUARTER.Mul(t.Ln());
  RETURN Init(fromRadians(x), y)
END Arctan;


PROCEDURE (x : Complex) Sinh* () : Complex;
VAR
  s1, c1, s2, c2: X.Real;
BEGIN
  x.real.SinhCosh(s1, c1); x.imag.SinhCosh(s2, c2);
  RETURN Init(s1.Mul(c2), c1.Mul(s2))
END Sinh;


PROCEDURE (x : Complex) Cosh* () : Complex;
VAR
  s1, c1, s2, c2: X.Real;
BEGIN
  x.real.SinhCosh(s1, c1); x.imag.SinhCosh(s2, c2);
  RETURN Init(c1.Mul(c2), s1.Mul(s2))
END Cosh;


PROCEDURE (x : Complex) SinhCoshC* (VAR sinh, cosh : Complex);
VAR
  s1, c1, s2, c2: X.Real;
BEGIN
  x.real.SinhCosh(s1, c1); x.imag.SinhCosh(s2, c2);
  sinh:=Init(s1.Mul(c2), c1.Mul(s2));
  cosh:=Init(c1.Mul(c2), s1.Mul(s2))
END SinhCoshC;


PROCEDURE (x : Complex) Tanh* () : Complex;
VAR
  a, b : Complex;
BEGIN
  x.SinhCoshC(a, b);
  RETURN a.Div(b)
END Tanh;


PROCEDURE (x : Complex) Arcsinh* () : Complex;
VAR
  Temp : Complex;
BEGIN
  (* Result = ln(x + sqrt(x*x + 1)) *)
  Temp:=one.Add(x.Mul(x)); Temp:=x.Add(Temp.Sqrt());
  RETURN Temp.Ln()
END Arcsinh;


PROCEDURE (x : Complex) Arccosh* () : Complex;
VAR
  Temp : Complex;
BEGIN
  (* Result = ln(x + sqrt(x*x - 1)) *)
  Temp:=x.Mul(x); Temp:=Temp.Sub(one); Temp:=x.Add(Temp.Sqrt());
  RETURN Temp.Ln()  
END Arccosh;


PROCEDURE (x : Complex) Arctanh* () : Complex;
VAR
  Temp, Temp2, Half: Complex;
BEGIN
  (* Result = ln((1 + x) / (1 - x)) / 2 *)
  Temp:=one.Add(x); Temp2:=one.Sub(x); Temp:=Temp.Div(Temp2);
  IF X.err=0 THEN
    Half:=Long(0.5);
    RETURN Half.Mul(Temp.Ln())
  ELSE RETURN zero
  END
END Arctanh;

PROCEDURE Store* (w: Storable.Writer) RAISES IO.Error;
(** Write calculator state to the 'w' writer. *)
  BEGIN 
    w.WriteSInt(nState.LocalBase);
    w.WriteSInt(nState.Notation);
    w.WriteSInt(nState.DecPoint);
    w.WriteSInt(nState.DegRadFlag);
    w.WriteChar(nState.DigSep);
    w.WriteChar(nState.FracSep);
    w.WriteNum(X.sigDigs);
    w.WriteBool(nState.Rational);
  END Store;
  
PROCEDURE Default*;
  BEGIN
    (* set up default state *)
    nState.LocalBase:=10;
    nState.Notation:=Normal;
    nState.DecPoint:=0;
    nState.DegRadFlag:=Degrees;
    nState.DigSep:=0X;
    nState.FracSep:=0X;
    X.SetDigits(16);
    nState.Rational:=FALSE;
  END Default;

PROCEDURE Load*(r: Storable.Reader) RAISES IO.Error;
(** Read calculator state from the 'r' reader. *)
  VAR digs: LONGINT;
  BEGIN
    r.ReadSInt(nState.LocalBase);
    r.ReadSInt(nState.Notation);
    r.ReadSInt(nState.DecPoint);
    r.ReadSInt(nState.DegRadFlag);
    r.ReadChar(nState.DigSep);
    r.ReadChar(nState.FracSep);
    r.ReadNum(digs);
    X.SetDigits(digs);
    r.ReadBool(nState.Rational);
  END Load;

(*
PROCEDURE Test;
VAR
  x:X.Real; xi:XI.Integer;
BEGIN
  xi:=RealToInteger(X.pi);
  Out.String("Real = "); Out.Object(X.pi); Out.String("; Int = ");
  Out.Object(xi); Out.Ln;
  
  x:=X.ToReal("1234567890.12345");
  xi:=RealToInteger(x);
  Out.String("Real = "); Out.Object(x); Out.String("; Int = ");
  Out.Object(xi); Out.Ln; 
  
  x:=X.ToReal("1234567890123456789012345678901234567890.12345");
  xi:=RealToInteger(x);
  Out.String("Real = "); Out.Object(x); Out.String("; Int = ");
  Out.Object(xi); Out.Ln; 

  xi:=XI.New("98765432109876543210", 10);
  x:=IntegerToReal(xi);
  Out.String("Int = "); Out.Object(xi); Out.String("; Real = ");
  Out.Object(x); Out.Ln;   
END Test;
*)

BEGIN
  Default();
  one:=Init(X.one, X.zero);	
  zero:=Init(X.zero, X.zero);
END Complex.
