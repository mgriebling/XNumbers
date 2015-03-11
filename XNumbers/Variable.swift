//
//  Variable.swift
//  XNumbers
//
//  Created by Mike Griebling on 11 Mar 2015.
//  Copyright (c) 2015 Computer Inspirations. All rights reserved.
//

import Foundation

struct Variables {

/*
	Variables - Variable storage/recall.
		Copyright (C) 1996-2004 Michael Griebling
 
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

//	struct VarType {
//		var Name: String
//		var Value: Complex
//		
//		init (variable: String, value: Complex) {
//			self.Name = variable
//			self.Value = value
//		} // New;
//		
//		func Equals (y: VarType) -> Bool {
//			return (self.Name.compare(y.Name) == .OrderedSame)
//		} // Equals;
//		
//		func Cmp (b: VarType) -> Int {
//			/**
//			This routine compares the variables `a' and `b' and
//			returns the value -1, 0, or 1 depending on whether `a'<`b',
//			`a'=`b', or `a'>`b'.  It is faster than merely subtracting
//			`a' and `b' and looking at the sign of the result.
//			*/
//			let cmp = self.Name.compare(b.Name)
//			switch cmp {
//			case .OrderedSame: return 0
//			case .OrderedAscending: return -1
//			case .OrderedDescending: return 1
//			}
//		} // Cmp;
//		
//		func ToString() -> String {
//			return self.Name + " = \(self.Value)"
//		} // ToString;
//	}
	
	private static var fvar = [String: Complex]()	/* function variables */
	private static var Var = [String: Complex]()	/* all defined variables */

//func (a: VarType) Store* (w: Storable.Writer) RAISES IO.Error;
///** Write 'a' to the 'w' writer. */
//
//if a.Value IS R.Rational { w.WriteLInt(1) } else { w.WriteLInt(0) };  /* type of variable */
//w.WriteStr(a.Name);
//a.Value.Store(w)
//} // Store;
//
//func (a: VarType) Load*(r: Storable.Reader) RAISES IO.Error;
///** Read 'a' from the 'r' reader. */
//var
//type: Int;
//c: R.Rational;
//
//r.ReadLInt(type);
//if type = 1 { NEW(c); a.Value  =  c } else { NEW(a.Value) };
//a.Name = Object.NewLatin1("dummy"); r.ReadStr(a.Name);
//a.Value.Load(r)
//} // Load;

	func Defined () -> Int {
		/** Return the number of permanent variables */
		return Variables.Var.count
	} // Defined;

	func Set (variable: String, value : Complex) -> Bool {
		/** Define a permanent variable */
		Variables.Var[variable] = value
		return true  // can't fail
	} // Set;

	func Setf (variable: String, value : Complex) -> Bool {
		/** Define a temporary stack variable 'variable' = 'value' */
		if let found = Variables.Var[variable] {
			return false
		} else {
			Variables.Var[variable] = value
			return true
		}
	} // Setf;

	func Get (variable: String) -> Complex? {
		/** Return the current definition of 'variable' */
		return Variables.Var[variable]
	} // Get;

	func Delete (variable : String) {
		/** Delete variable 'variable' */
		Variables.Var[variable] = nil
	} // Delete;
	
	func Deletef () {
		/** Delete all temporary stack variables */
		Variables.fvar = [String: Complex]()
	} // Deletef;

	func Iterate (IterFunc: (String, String)) {
		/** Apply the function 'IterFunc' to all variables */
		for (name, value) in Variables.Var {
			let n = value.description
			println("Variable \(name) = \(n)")
//			IterFunc(name, n)   // why is the type an issue?
		}
	} // Iterate;

//func Store * (w: Storable.Writer) RAISES IO.Error;
///** Store the variables to writer 'w' */
//
//Var.Store(w)
//} // Store;
//
//func Load * (r: Storable.Reader) RAISES IO.Error;
///** Load the variables from the reader 'r' */
//
//Var.Load(r)
//} // Load;

}
