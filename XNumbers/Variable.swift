////
////  Variable.swift
////  XNumbers
////
////  Created by Mike Griebling on 11 Mar 2015.
////  Copyright (c) 2015 Computer Inspirations. All rights reserved.
////
//
//import Foundation
//
//MODULE Variables;
//
//(*
//	Variables - Variable storage/recall.
//		Copyright (C) 1996-2004 Michael Griebling
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
//IMPORT el := ADT:LinkedList,
//ADT:Storable,
//IO,
//Out,
//Object,
//Object:Boxed,
//
//C  := Complex,
//R  := Rationals;
//
//TYPE
//VarDesc = RECORD (Boxed.ObjectDesc)
//Name: STRING;
//Value: C.Complex
//END;
//VarType = POINTER TO VarDesc;
//IteratorType*=PROCEDURE(name, arg: STRING);
//
//CONST
//NotFound = -1;
//
//VAR
//fvar: el.LinkedList;            (* function variables *)
//Var: el.LinkedList;             (* all defined variables *)
//
//PROCEDURE (v: VarType) Equals * (y: Object.Object) : BOOLEAN;
//BEGIN
//WITH y: VarType DO
//RETURN v.Name.Equals(y.Name)
//END
//END Equals;
//
//PROCEDURE (a: VarType) Cmp * (b: Object.Object) : LONGINT;
//(**
//This routine compares the variables `a' and `b' and
//returns the value -1, 0, or 1 depending on whether `a'<`b',
//`a'=`b', or `a'>`b'.  It is faster than merely subtracting
//`a' and `b' and looking at the sign of the result.
//*)
//BEGIN
//WITH b: VarType DO
//RETURN a.Name.Compare(b.Name)
//END
//END Cmp;
//
//PROCEDURE (v: VarType) ToString * () : STRING;
//VAR
//s: STRING;
//BEGIN
//s:=v.Name.Concat(" = ");
//RETURN s.Concat(v.Value.ToString())
//END ToString;
//
//PROCEDURE (a: VarType) Store* (w: Storable.Writer) RAISES IO.Error;
//(** Write 'a' to the 'w' writer. *)
//BEGIN
//IF a.Value IS R.Rational THEN w.WriteLInt(1) ELSE w.WriteLInt(0) END;  (* type of variable *)
//w.WriteStr(a.Name);
//a.Value.Store(w)
//END Store;
//
//PROCEDURE (a: VarType) Load*(r: Storable.Reader) RAISES IO.Error;
//(** Read 'a' from the 'r' reader. *)
//VAR
//type: LONGINT;
//c: R.Rational;
//BEGIN
//r.ReadLInt(type);
//IF type = 1 THEN NEW(c); a.Value := c ELSE NEW(a.Value) END;
//a.Name:=Object.NewLatin1("dummy"); r.ReadStr(a.Name);
//a.Value.Load(r)
//END Load;
//
//PROCEDURE Defined * () : LONGINT;
//(** Return the number of permanent variables *)
//BEGIN
//RETURN Var.Size()
//END Defined;
//
//PROCEDURE New (var: STRING; value: C.Complex) : VarType;
//VAR
//v: VarType;
//BEGIN
//NEW(v); v.Name:=var; v.Value:=value;
//RETURN v
//END New;
//
//PROCEDURE Set * (var: STRING; value : C.Complex; VAR ok: BOOLEAN);
//(** Define a permanent variable *)
//VAR pos: LONGINT;
//v: VarType;
//BEGIN
//v:=New(var, value);
//pos := Var.IndexOf(v); ok := TRUE;
//IF pos = NotFound THEN Var.Append(v)
//ELSE Var.Set(pos, v)
//END
//END Set;
//
//PROCEDURE Setf * (var: STRING; value : C.Complex; VAR ok: BOOLEAN);
//(** Define a temporary stack variable 'var' = 'value' *)
//VAR pos: LONGINT;
//v: VarType;
//BEGIN
//v:=New(var, value);
//pos := fvar.IndexOf(v); ok := TRUE;
//IF pos = NotFound THEN fvar.Append(v)
//ELSE ok:=FALSE
//END
//END Setf;
//
//PROCEDURE Get * (var: STRING; VAR value: C.Complex; VAR ok: BOOLEAN);
//(** Return in 'value' the current definition of 'var' *)
//VAR pos: LONGINT;
//v: Object.Object;
//BEGIN
//v:=New(var, value);
//pos := fvar.IndexOf(v);
//ok:=FALSE; value:=C.zero;
//IF pos = NotFound THEN
//pos := Var.IndexOf(v);
//IF pos # NotFound THEN
//ok:=TRUE; v:=Var.Get(pos); value:=v(VarType).Value
//END
//ELSE ok:=TRUE; v:=fvar.Get(pos); value:=v(VarType).Value
//END
//END Get;
//
//PROCEDURE Delete * (var : STRING);
//(** Delete variable 'var' *)
//VAR pos: LONGINT;
//v: VarType;
//o: Object.Object;
//BEGIN
//v:=New(var, C.zero);
//pos:=Var.IndexOf(v);
//IF pos # NotFound THEN o:=Var.Remove(pos) END
//END Delete;
//
//PROCEDURE Deletef * ();
//(** Delete all temporary stack variables *)
//BEGIN
//fvar.Clear()
//END Deletef;
//
//PROCEDURE Iterate * (IterFunc: IteratorType);
//(** Apply the function 'IterFunc' to all variables *)
//VAR iter: el.Iterator;
//v: Object.Object;
//n: STRING;
//BEGIN
//iter:=Var.GetIterator(NIL);
//WHILE iter.HasNext() DO
//v:=iter.Next();
//WITH v: VarType DO
//Out.String("Variable "); Out.Object(v.Name); Out.String(" = "); Out.Object(v.Value); Out.Ln;
//n:=v.Value.ToString();
//IterFunc(v.Name, n)
//END
//END
//END Iterate;
//
//PROCEDURE Store * (w: Storable.Writer) RAISES IO.Error;
//(** Store the variables to writer 'w' *)
//BEGIN
//Var.Store(w)
//END Store;
//
//PROCEDURE Load * (r: Storable.Reader) RAISES IO.Error;
//(** Load the variables from the reader 'r' *)
//BEGIN
//Var.Load(r)
//END Load;
//
//PROCEDURE Init * ();
//(** Create blanked variable lists *)
//BEGIN
//Var:=el.New();
//fvar:=el.New()
//END Init;
//
//BEGIN
//Init()
//END Variables.
//
//
