//
//  State.swift
//  XNumbers
//
//  Created by Mike Griebling on 17 Mar 2015.
//  Copyright (c) 2015 Computer Inspirations. All rights reserved.
//

import Foundation

enum NotationType {
	case Normal
	case Scientific
	case Engineering
}
enum AngularMeasure {
	case Degrees
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
	init () {
		LocalBase = 10
		Notation = NotationType.Normal
		DecPoint = 0
		DegRadFlag = AngularMeasure.Degrees
		DigSep = ","
		FracSep = "."
		Rational = false
	}
	
	mutating func Default() {
		/* set up default state */
		LocalBase = 10
		Notation = NotationType.Normal
		DecPoint = 0
		DegRadFlag = AngularMeasure.Degrees
		DigSep = "\0"
		FracSep = "\0"
		Rational = false
	} // Default;
	
	var LocalBase  : Int
	var Notation   : NotationType
	var DecPoint   : Int
	var DegRadFlag : AngularMeasure
	var DigSep     : Character
	var FracSep    : Character
	var Rational   : Bool
}

var nState = NumbState()
var status : Status = .Okay
