//
//  main.swift
//  XNumbers
//
//  Created by Mike Griebling on 28 Feb 2015.
//  Copyright (c) 2015 Computer Inspirations. All rights reserved.
//

import Foundation

var eq = Equation(command: "let a = 10")
eq.Evaluate(eq.CommandLine)
eq.CommandLine = "10a"
println("Equation \(eq.CommandLine) = \(eq.Evaluate(eq.CommandLine))")


