//
//  xNumber.swift
//  XNumbers
//
//  Created by Mike Griebling on 17 Mar 2015.
//  Copyright (c) 2015 Computer Inspirations. All rights reserved.
//

import Foundation

extension xNumber : Comparable, Printable {
	class var pi : xNumber { return xNumber.pif() }
	class var eps : xNumber { return xNumber.epsf() }
	class var ln2 : xNumber { return xNumber.ln2f() }
	class var ln10 : xNumber { return xNumber.ln10f() }
	class var zero : xNumber { return xNumber.zerof() }
	class var one : xNumber { return xNumber.onef() }
}

public func > (lhs: xNumber, rhs: xNumber) -> Bool {
	return lhs.cmp(rhs) == 1
}

public func < (lhs: xNumber, rhs: xNumber) -> Bool {
	return lhs.cmp(rhs) == -1
}

public func == (lhs: xNumber, rhs: xNumber) -> Bool {
	return lhs.cmp(rhs) == 0
}

