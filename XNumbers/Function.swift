////
////  Function.swift
////  XNumbers
////
////  Created by Mike Griebling on 11 Mar 2015.
////  Copyright (c) 2015 Computer Inspirations. All rights reserved.
////
//
//import Foundation
//
//MODULE Functions;
//
//(*
//	Functions - Function list management.
//		Copyright (C) 2004-2010 Michael Griebling
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
//Object,
//Object:Boxed;
//
//TYPE
//FuncDesc = RECORD (Boxed.ObjectDesc)
//Name: STRING;
//Func: STRING;
//Args: el.LinkedList
//END;
//FuncType = POINTER TO FuncDesc;
//IteratorType*=PROCEDURE(name, arg, value: STRING);
//
//CONST
//NotFound = -1;
//
//VAR
//Funcs : el.LinkedList;             (* all defined variables *)
//
//PROCEDURE (v: FuncType) Equals * (y: Object.Object) : BOOLEAN;
//BEGIN
//WITH y: FuncType DO
//RETURN v.Name.Equals(y.Name)
//END
//END Equals;
//
//PROCEDURE (a: FuncType) Cmp * (b: Object.Object) : LONGINT;
//(**
//This routine compares the variables `a' and `b' and
//returns the value -1, 0, or 1 depending on whether `a'<`b',
//`a'=`b', or `a'>`b'.  It is faster than merely subtracting
//`a' and `b' and looking at the sign of the result.
//*)
//BEGIN
//WITH b: FuncType DO
//RETURN a.Name.Compare(b.Name)
//END
//END Cmp;
//
//PROCEDURE^ ArgsToString (fname: STRING) : STRING;
//
//PROCEDURE (v: FuncType) ToString * () : STRING;
//BEGIN
//RETURN v.Name + "(" + ArgsToString(v.Name) + ") = " + v.Func.ToString()
//END ToString;
//
//PROCEDURE (a: FuncType) Store* (w: Storable.Writer) RAISES IO.Error;
//(** Write 'a' to the 'w' writer. *)
//BEGIN
//w.WriteStr(a.Name);
//w.WriteStr(a.Func);
//a.Args.Store(w)
//END Store;
//
//PROCEDURE (a: FuncType) Load*(r: Storable.Reader) RAISES IO.Error;
//(** Read 'a' from the 'r' reader. *)
//BEGIN
//a.Name:=Object.NewLatin1("dummy");
//a.Func:=a.Name;
//r.ReadStr(a.Name);
//r.ReadStr(a.Func);
//a.Args:=el.New(); a.Args.Load(r)
//END Load;
//
//PROCEDURE Defined * () : LONGINT;
//BEGIN
//RETURN Funcs.Size()
//END Defined;
//
//PROCEDURE New (name, value: STRING) : FuncType;
//VAR
//v: FuncType;
//BEGIN
//NEW(v); v.Name:=name; v.Func:=value; v.Args:=el.New();
//RETURN v
//END New;
//
//PROCEDURE Set * (fname: STRING);
//VAR pos: LONGINT;
//v: FuncType;
//BEGIN
//v:=New(fname, "");
//pos := Funcs.IndexOf(v);
//IF pos = NotFound THEN Funcs.Append(v)
//ELSE Funcs.Set(pos, v)
//END
//END Set;
//
//
//PROCEDURE SetEquation * (fname, value: STRING);
//VAR pos: LONGINT;
//v: FuncType;
//o: Object.Object;
//BEGIN
//v:=New(fname, value);
//pos := Funcs.IndexOf(v);
//IF pos # NotFound THEN        (* this is a new function *)
//o:=Funcs.Get(pos);
//v.Args:=o(FuncType).Args;   (* keep args *)
//Funcs.Set(pos, v)
//END
//END SetEquation;
//
//
//PROCEDURE Get * (fname: STRING; VAR value: STRING; VAR ok: BOOLEAN);
//VAR pos: LONGINT;
//v: Object.Object;
//BEGIN
//v:=New(fname, "");
//pos := Funcs.IndexOf(v);
//IF pos = NotFound THEN ok:=FALSE; value:=""
//ELSE ok:=TRUE; v:=Funcs.Get(pos); value:=v(FuncType).Func
//END
//END Get;
//
//PROCEDURE GetArg * (fname: STRING; argi: LONGINT; VAR value: STRING; VAR ok: BOOLEAN);
//VAR
//pos: LONGINT;
//v: Object.Object;
//args: el.LinkedList;
//BEGIN
//v:=New(fname, "");
//pos:=Funcs.IndexOf(v);
//ok:=FALSE; value:="";
//IF pos # NotFound THEN
//v:=Funcs.Get(pos); args:=v(FuncType).Args;
//IF argi<args.Size() THEN
//v:=args.Get(argi);
//value:=v(STRING); ok:=TRUE
//END
//END
//END GetArg;
//
//
//PROCEDURE NumArgs * (fname: STRING) : LONGINT;
//VAR
//pos: LONGINT;
//v: Object.Object;
//args: el.LinkedList;
//BEGIN
//v:=New(fname, "");
//pos:=Funcs.IndexOf(v);
//IF pos # NotFound THEN
//v:=Funcs.Get(pos); args:=v(FuncType).Args;
//RETURN args.Size()
//ELSE RETURN 0
//END
//END NumArgs;
//
//
//PROCEDURE ArgsToString (fname: STRING) : STRING;
//VAR
//ok: BOOLEAN;
//s, arg: STRING;
//i: LONGINT;
//BEGIN
//s:=""; i:=0;
//LOOP
//GetArg(fname, i, arg, ok);
//IF ~ok THEN EXIT END;
//IF i>0 THEN s:=s + "; " END;
//s:=s + arg; INC(i)
//END;
//RETURN s
//END ArgsToString;
//
//
//PROCEDURE AddArg * (fname: STRING; name: STRING; VAR ok: BOOLEAN);
//VAR pos: LONGINT;
//v: Object.Object;
//args: el.LinkedList;
//BEGIN
//v:=New(fname, "");
//pos := Funcs.IndexOf(v);
//IF pos = NotFound THEN ok:=FALSE
//ELSE ok:=TRUE; v:=Funcs.Get(pos); args:=v(FuncType).Args;
//args.Append(name)
//END
//END AddArg;
//
//
//PROCEDURE Delete * (fname: STRING);
//VAR pos: LONGINT;
//v: FuncType;
//o: Object.Object;
//BEGIN
//v:=New(fname, "");
//pos:=Funcs.IndexOf(v);
//IF pos # NotFound THEN o:=Funcs.Remove(pos) END
//END Delete;
//
//PROCEDURE Iterate * (IterFunc: IteratorType);
//VAR iter: el.Iterator;
//v: Object.Object;
//BEGIN
//iter:=Funcs.GetIterator(NIL);
//WHILE iter.HasNext() DO
//v:=iter.Next();
//WITH v: FuncType DO
//IterFunc(v.Name, ArgsToString(v.Name), v.Func)
//END
//END
//END Iterate;
//
//PROCEDURE Store * (w: Storable.Writer) RAISES IO.Error;
//BEGIN
//Funcs.Store(w)
//END Store;
//
//PROCEDURE Load * (r: Storable.Reader) RAISES IO.Error;
//BEGIN
//Funcs.Load(r)
//END Load;
//
//PROCEDURE Init * ();
//BEGIN
//Funcs:=el.New()
//END Init;
//
//BEGIN
//Init()
//END Functions.
//
//
