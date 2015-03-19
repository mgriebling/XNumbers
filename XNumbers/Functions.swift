//
//  Function.swift
//  XNumbers
//
//  Created by Mike Griebling on 11 Mar 2015.
//  Copyright (c) 2015 Computer Inspirations. All rights reserved.
//

import Foundation

struct Functions {

/*
	Functions - Function list management.
		Copyright (C) 2004-2010 Michael Griebling
 
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

	struct FuncType {
		var Func: String
		var Args: [String]
		
		init (value: String) {
			self.Func = value
			self.Args = [String]()
		} // New;
		
		let FUNCKEY = "FuncType.Func"
		let ARGSKEY = "FuncType.Args"
		
		init (decoder: NSCoder) {
			Func = decoder.decodeObjectForKey(FUNCKEY) as String
			Args = decoder.decodeObjectForKey(ARGSKEY) as [String]
		}
		
		func Save (encoder: NSCoder) {
			encoder.encodeObject(Func, forKey: FUNCKEY)
			encoder.encodeObject(Args, forKey: ARGSKEY)
		}
	}
	
	static let FUNCSKEY = "Functions.Funcs"
	static let FUNCSSIZEKEY = "Functions.Funcs.count"
	
	private static var Funcs = [String: FuncType]()
	
	static func Defined () -> Int {
		return Functions.Funcs.count
	} // Defined;

	
	static func Set (fname: String) {
		var v = FuncType(value: "")   // no equation defined yet
		Functions.Funcs[fname] = v
	} // Set;

	
	static func SetEquation (fname: String, value: String) {
		var pos: Int
		var v = FuncType(value: value)
		Functions.Funcs[fname] = v
	} // SetEquation;

	
	static func Get (fname: String) -> String? {
		return Functions.Funcs[fname]?.Func
	} // Get;

	
	static func GetArg (fname: String, argNumber: Int) -> String? {
		if let args = Functions.Funcs[fname]?.Args {
			if argNumber < args.count {
				return args[argNumber]
			}
		}
		return nil
	} // GetArg;

	
	static func NumArgs (fname: String) -> Int {
		if let args = Functions.Funcs[fname]?.Args {
			return args.count
		}
		return 0
	} // NumArgs;
	
	
	static func ArgsToString (fname: String) -> String {
		var s = ""
		var i = 0
		if let args = Functions.Funcs[fname]?.Args {
			for arg in args {
				if i > 0 { s = s + "; " }
				s = s + arg; i++
			}
		}
		return s
	} // ArgsToString;


	static func AddArg (fname: String, name: String) -> Bool {
		if let args = Functions.Funcs[fname]?.Args {
			Functions.Funcs[fname]?.Args.append(name)
			return true
		}
		return false
	} // AddArg;


	static func Delete (fname: String) {
		Functions.Funcs[fname] = nil
	} // Delete;

	static func Iterate (IterFunc: (String, String, FuncType) -> ()) {
		for (fname, function) in Functions.Funcs {
			IterFunc(fname, ArgsToString(fname), function)
		}
	} // Iterate;
	
	static func Load (decoder: NSCoder) {
		let size = decoder.decodeIntegerForKey(FUNCSSIZEKEY)
		var cnt : Int
		Functions.Funcs = [String: FuncType]();
		for cnt=0; cnt<size; cnt++ {
			let name : String = decoder.decodeObject() as String
			let function = FuncType(decoder: decoder)
			Functions.Funcs[name] = function
		}
	}
	
	static func Save (encoder: NSCoder) {
		encoder.encodeInteger(Functions.Funcs.count, forKey: FUNCSSIZEKEY)
		for (name, function) in Functions.Funcs {
			encoder.encodeObject(name)
			function.Save(encoder)
		}
	}
	
}
