//
//  State.swift
//  XNumbers
//
//  Created by Mike Griebling on 17 Mar 2015.
//  Copyright (c) 2015 Computer Inspirations. All rights reserved.
//

import Foundation

enum NotationType : Int {
	case Normal = 0
	case Scientific
	case Engineering
}
enum AngularMeasure : Int {
	case Degrees = 0
	case Radians
	case Gradians
}

enum Status {
	case Okay
	case Overflow
	case Underflow
	case DivideByZero
	case TooFewDigits
	case TooManyDigits
	case IllegalNumber
	case UndefinedStorage
	case IllegalOperator
	case MismatchBraces
	case IllegalArgument
	
	// Scanner errors
	case ExpectingRBrace
	case IllegalExpression
	case IllegalVariable
	case TooManyVariables
	case ExpectingName
	case ExpectingAssign
	case Undefined
	case ExpectingArgs
	case IncompatibleArgs
}

struct NumbState {
	static private let LOCALBASE  = "State.LocalBase"
	static private let NOTATION	= "State.Notation"
	static private let DECPOINT	= "State.DecPoint"
	static private let DEGRADFLAG = "State.DegRadFlag"
	static private let DIGSEP		= "State.DigSep"
	static private let FRACSEP	= "State.FracSep"
	static private let RATIONAL	= "State.Rational"
	
	init () {
		LocalBase = 10
		Notation = .Normal
		DecPoint = 0
		DegRadFlag = .Degrees
		DigSep = "\0"
		FracSep = "\0"
		Rational = false
	}
	
	init (decoder : NSCoder) {
		LocalBase = decoder.decodeIntegerForKey(NumbState.LOCALBASE)
		Notation = NotationType(rawValue: decoder.decodeIntegerForKey(NumbState.NOTATION))!
		DecPoint =  decoder.decodeIntegerForKey(NumbState.DECPOINT)
		DegRadFlag = AngularMeasure(rawValue: decoder.decodeIntegerForKey(NumbState.DEGRADFLAG))!
		let s1 : String = decoder.decodeObjectForKey(NumbState.DIGSEP) as String
		DigSep = first(s1)!
		let s2 : String = decoder.decodeObjectForKey(NumbState.FRACSEP) as String
		FracSep = first(s2)!
		Rational = decoder.decodeBoolForKey(NumbState.RATIONAL)
	}
	
	func Save (encoder : NSCoder) {
		encoder.encodeInteger(LocalBase, forKey: NumbState.LOCALBASE)
		encoder.encodeInteger(Notation.rawValue, forKey: NumbState.NOTATION)
		encoder.encodeInteger(DecPoint, forKey: NumbState.DECPOINT)
		encoder.encodeInteger(DegRadFlag.rawValue, forKey: NumbState.DEGRADFLAG)
		let s1 = "" + [DigSep]
		encoder.encodeObject(s1, forKey: NumbState.DIGSEP)
		let s2 = "" + [FracSep]
		encoder.encodeObject(s2, forKey: NumbState.FRACSEP)
		encoder.encodeBool(Rational, forKey: NumbState.RATIONAL)
	}
	
	var LocalBase  : Int
	var Notation   : NotationType
	var DecPoint   : Int
	var DegRadFlag : AngularMeasure {
		willSet (newMeasure) {
			xNumber.setAngularMeasure(newMeasure.rawValue)
		}
	}
	var DigSep     : Character
	var FracSep    : Character
	var Rational   : Bool
}

var nState = NumbState()
var status : Status = .Okay
