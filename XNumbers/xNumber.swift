//
//  xNumber.swift
//  XNumbers
//
//  Created by Mike Griebling on 17 Mar 2015.
//  Copyright (c) 2015 Computer Inspirations. All rights reserved.
//

import Foundation

extension xNumber : Comparable, Printable {
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

