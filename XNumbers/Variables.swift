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
	
	private static var fvar = [String: xNumber]()	/* function variables */
	private static var Var = [String: xNumber]()	/* all defined variables */
	
	static let VARKEY = "Variables.fvar"
	
	static func Load (decoder: NSCoder) {
		Var = decoder.decodeObjectForKey(VARKEY) as [String: xNumber]
	}
	
	static func Save (encoder: NSCoder) {
		encoder.encodeObject(Var, forKey: VARKEY)
	}

	static func Defined () -> Int {
		/** Return the number of permanent variables */
		return Variables.Var.count
	} // Defined;

	static func Set (variable: String, value : xNumber) -> Bool {
		/** Define a permanent variable */
		Variables.Var[variable] = value
		return true  // can't fail
	} // Set;

	static func Setf (variable: String, value : xNumber) -> Bool {
		/** Define a temporary stack variable 'variable' = 'value' */
		if let found = Variables.Var[variable] {
			return false
		} else {
			Variables.Var[variable] = value
			return true
		}
	} // Setf;

	static func Get (variable: String) -> xNumber? {
		/** Return the current definition of 'variable' */
		return Variables.Var[variable]
	} // Get;

	static func Delete (variable : String) {
		/** Delete variable 'variable' */
		Variables.Var[variable] = nil
	} // Delete;
	
	static func Deletef () {
		/** Delete all temporary stack variables */
		Variables.fvar = [String: xNumber]()
	} // Deletef;

	static func Iterate (IterFunc: (String, String) -> ()) {
		/** Apply the function 'IterFunc' to all variables */
		for (name, value) in Variables.Var {
			let n = value.description
			println("Variable \(name) = \(n)")
			IterFunc(name, n)
		}
	} // Iterate;

}
