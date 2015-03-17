//
//  main.swift
//  XNumbers
//
//  Created by Mike Griebling on 28 Feb 2015.
//  Copyright (c) 2015 Computer Inspirations. All rights reserved.
//

import Foundation

var eq = Equation(command: "123 + 456");

println("Equation \(eq.CommandLine) = \(eq.Evaluate(eq.CommandLine))")


