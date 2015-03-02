//
//  Integers.swift
//  XNumbers
//
//  Created by Mike Griebling on 28 Feb 2015.
//  Copyright (c) 2015 Computer Inspirations. All rights reserved.
//

import Foundation

func == (lhs: Integer, rhs: Integer) -> Bool {
	var i: Int
	if lhs.digit.count != rhs.digit.count {
		return false
	} else {
		for i = 0; i<lhs.digit.count; i++ {
			if lhs.digit[i] != rhs.digit[i] {
				return false
			}
		}
		return true
	}
}

func > (lhs: Integer, rhs: Integer) -> Bool {
	return lhs.Cmp(rhs) == 1
}

func < (lhs: Integer, rhs: Integer) -> Bool {
	return lhs.Cmp(rhs) == -1
}

prefix func - (a: Integer) -> Integer {
	var z = Integer(a: a)
	z.negative = !a.negative
	return z
}

prefix func + (a: Integer) -> Integer {
	return a
}

struct Integer : Equatable, Comparable, Printable, Hashable {

/*
	Implements integer values of arbitrary magnitude.
	Copyright (c) 2002, 2003 Michael van Acken and Michael Griebling
	
	This module is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public License
	as published by the Free Software Foundation; either version 2 of
	the License, or (at your option) any later version.
		
	This module is distributed in the hope that it will be useful, but
	WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
	Lesser General Public License for more details.
	
	You should have received a copy of the GNU Lesser General Public
	License along with OOC. If not, write to the Free Software Foundation,
	59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
	*/

	/* This module is a reformulation of (parts of) Python's @file{longobject.c}
	in Oberon-2.  Optimizations like Karatsuba multiplication and string
	conversion for power of two base have been omitted.  All errors are mine,
	of course.
	
	Added algorithms are from Knuth: "The Art Of Computer Programming",
	Vol 2, section 4.3.1
	
	Adapted to latest oo2c library routines including Boxed Objects
	and Boxed Strings by Michael Griebling, 8 Nov 2003.
	
	Ported
	*/

	typealias Digit = Int32;
	typealias TwoDigits = Int64;
	private typealias LONGINT = Int32
	
	private static let shift : Digit = 15                            /* minimum is 6 */
	private static let base : Digit = 1 << shift
	private static let mask : Digit = (1 << shift) - 1

	private var digit: [Digit]
	private var negative: Bool

	/* Stores an integer number of arbitrary size.  The absolute value of a
	number is equal to @samp{SUM(for @var{i}=0 through ABS(@ofield{size})-1)
	digit[@var{i}]*2^(shift*@var{i})}.  Negative numbers are represented
	with @samp{@ofield{size}<0}, and zero by @samp{@ofield{size}=0}.  In a
	normalized number, @samp{@ofield{digit}[ABS(@ofield{size})-1]} (the most
	significant digit) is never zero.  For all valid @var{i}, @samp{0 <=
	@ofield{digit}[@var{i}] <= mask}.  */

	static let zero = Integer(size: 0)
	static let one = Integer(value: 1)
	
	private let powerOf2: [Int] = [-1, -1, 1, -1, 2, -1, -1, -1, 3, -1, -1,
		-1, -1, -1, -1, -1, 4, -1, -1, -1, -1,
		-1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
		-1, 5, -1, -1, -1, -1, -1]
	
	var description: String {
		return self.description(10)
	}
	
	var	hashValue: Int {
		var len, p: Int
		var x: Int
		
		len = self.digit.count
		if len == 0 {
			return 0
		} else {
			x = Int(self.digit[0] << 7)
			p = 0
			while p != len {
				x = (1000003 &* x) & ~Int(self.digit[p])
				p++
			}
			x = x & ~len
			return x
		}
	}
	
	init (size : Int) {
		self.digit = [Digit](count:size, repeatedValue:0)
		self.negative = false
	}
	
	init (a: Integer) {
		self.init(size: a.digit.count)
		digit = a.digit
		negative = a.negative
	}
	
	init (str: String, inputBase: Int) {
		self = self.ToInteger(str, inputBase: inputBase)
	}
	
	init (str: String) {
		self.init(str: str, inputBase: 10)
	}
	
	init (value: Int) {
		let maxDigits = (sizeof(Digit)*8+Integer.shift-2) / Integer.shift
		var lvalue : Digit = Digit(value)
		var size, i: Int
		
		if value == 0 {
			self = Integer.zero
		} else {
			negative = false
			if lvalue < 0 {
				if lvalue == Digit(Int.min) { /* handle overflow for -MIN(LONGINT) */
					self.init(value: Int(lvalue+1))
					if digit[0] == Integer.mask {
						self = AddAbs(self, b: Integer.one)
						negative = true
					} else {
						digit[0]++
					}
				} else {
					lvalue = -lvalue;
					negative = true;
				}
			}
			if Integer.base == (1 << 15) {		/* unwind loop for standard case */
				self.init(size:3)				/* number not normalized */
				self.digit[0] = lvalue % Integer.base
				self.digit[1] = (lvalue / Integer.base) % Integer.base
				self.digit[2] = lvalue / (Integer.base * Integer.base)
				if digit[2] != 0 {
					// already at the correct size
				} else if digit[1] != 0 {
					digit.removeLast()
				} else {
					digit.removeRange(1...2)
				}
			} else  {                             /* loop for non-standard base */
				self.init(size: Int(maxDigits))
				i = 0;
				while (lvalue != 0) {
					digit[i] = lvalue % Integer.base
					lvalue = lvalue / Integer.base
					i++
				}
				digit.removeRange(i..<Int(maxDigits))
			}
		}
	}

	private func Normalize(var a: Integer) {
		var i, j: Int
		j = a.digit.count
		i = j
		while (i != 0) && (a.digit[i-1] == 0) {
			i--
		}
		if i != j {
			// remove leading zeros
			a.digit.removeRange(i..<j)
		}
	}

	/*func Dump (msg: ARRAY OF CHAR; big: Integer);
	var
	i: LONGINT;
	{
	Out.String (msg);
	Out.String(": size=");
	Out.LongInt(big.size,0);
	Out.String (", digits=");
	FOR i = 0 TO ABS(big.size)-1 {
	Out.Int(big.digit[i], 0);
	Out.Char(" ");
	}
	Out.String (", base=");
	Out.LongInt(base, 0);
	Out.Ln;
	} // Dump;*/

	private func AddAbs (a: Integer, b: Integer) -> Integer {
		/** Adds the absolute values of two integers.  */
		var y = b;
		var x = a;
		var z: Integer
		var carry: Digit
		var i, sizeA, sizeB: Int
		
		if x.digit.count < y.digit.count {
			z = x; x = y; y = z
		}
		sizeA = x.digit.count
		sizeB = y.digit.count
		
		z = Integer(size: sizeA+1)
		carry = 0
		i = 0
		while i != sizeB {
			carry += a.digit[i] + b.digit[i]
			z.digit[i] = carry % Integer.base
			carry = carry / Integer.base
			i++
		}
		while i != sizeA  {
			carry += x.digit[i]
			z.digit[i] = carry % Integer.base
			carry = carry / Integer.base
			i++
		}
		z.digit[i] = carry
		Normalize(z)
		return z
	}

	func IsZero() -> Bool {
		return (digit.count == 0)
	} // IsZero;
	
	func NonZero() -> Bool {
		return (digit.count != 0)
	} // NonZero;
	
	func ToInt() -> Int {
		var int: Digit
		var i: Int
		int = 0;
		for i = self.digit.count-1; i>=0; i-- {
			int = int * Integer.base + self.digit[i]
		}
		if negative {
			return -Int(int)
		} else {
			return Int(int)
		}
	} // ToLongInt;

	func IsNegative () -> Bool {
		return negative
	}

	func Cmp (b: Integer) -> Int {
		var i, res: Int
		if self.digit.count < b.digit.count {
			return -1;
		} else if self.digit.count > b.digit.count {
			return 1;
		} else {
			i = self.digit.count
			do {
				i--
			} while (i >= 0) && (self.digit[i] == b.digit[i])
			if i < 0 {
				return 0
			} else if self.digit[i] < b.digit[i] {
				res = -1
			} else {
				res = 1
			}
			if self.negative {
				return -res
			} else {
				return res
			}
		}
	}  // Cmp;

	func Abs() -> Integer {
		var z: Integer
		if !self.negative {
			return self
		} else {
			z = Integer(a:self)
			z.negative = false
			return z
		}
	} // Abs;

	private func SubAbs (a: Integer, b: Integer) -> Integer {
		/**Subtract the absolute value of two integers.  */
		var z: Integer
		var x = a
		var y = b
		var sign = false
		var i, sizeA, sizeB: Int
		var borrow: Digit
		
		if x.digit.count < y.digit.count {
			z = a; x = b; y = z
			sign = true
		} else if x.digit.count == y.digit.count {
			/* find highest digit where a and b differ */
			i = x.digit.count-1
			while (i >= 0) && (x.digit[i] == y.digit[i]) {
				i--
			}
			if i < 0 {
				return Integer.zero
			} else if x.digit[i] < y.digit[i] {
				z = a; x = b; y = z
				sign = negative
			}
		}
		sizeA = x.digit.count; sizeB = y.digit.count
		z = Integer(size: sizeA)
		borrow = 0;
		i = 0;
		while (i != sizeB) {
			borrow = a.digit[i]-b.digit[i]-borrow;
			z.digit[i] = borrow % Integer.base
			borrow = (borrow / Integer.base) % 2 /* keep only 1 sign bit */
			i++
		}
		while i != sizeA {
			borrow = a.digit[i]-borrow;
			z.digit[i] = borrow % Integer.base
			borrow = (borrow / Integer.base) % 2
			i++
		}
		assert(borrow == 0, "SubAbs borrow != 0")
		z.negative = sign
		Normalize(z)
		return z;
	} // SubAbs;

	private func MulAbs (a: Integer, b: Integer) -> Integer {
		/* Grade school multiplication, ignoring the signs.  * returns the absolute
		value of the product.  */
		var sizeA, sizeB, i, j: Int
		var f, carry: Digit
		var z: Integer
		
		sizeA = a.digit.count; sizeB = b.digit.count
		z = Integer(size:sizeA+sizeB)
		for i = 0; i<sizeA+sizeB; i++ {
			z.digit[i] = 0
		}
		
		i = 0;
		while (i != sizeA) {
			f = a.digit[i]
			carry = 0;
			j = 0;
			while (j != sizeB) {
				carry += z.digit[i+j] + b.digit[j]*f
				assert(carry >= 0, "MulAbs carry < 0")
				z.digit[i+j] = carry % Integer.base
				carry = carry / Integer.base
				j++
			}
			while (carry != 0) {
				carry += z.digit[i+j]
				assert(carry >= 0, "MulAbs carry < 0")
				z.digit[i+j] = carry % Integer.base
				carry = carry / Integer.base
				j++
			}
			i++
		}
		Normalize(z)
		return z
	} // MulAbs;

	func Add (b: Integer) -> Integer {
		var z: Integer
		if self.negative {
			if b.negative {
				z = AddAbs(self, b:b)
				z.negative = !z.negative
				return z
			} else {
				return SubAbs(b, b:self)
			}
		} else {
			if b.negative {
				return SubAbs(self, b:b);
			} else {
				return AddAbs(self, b:b);
			}
		}
	} // Add;

	func Sub (b: Integer) -> Integer {
		var
		z: Integer
		if self.negative {
			if b.negative {
				z = SubAbs(self, b:b)
			} else {
				z = AddAbs(self, b:b)
			}
			z.negative = !z.negative
			return z
		} else {
			if b.negative {
				return AddAbs(self, b:b)
			} else {
				return SubAbs(self, b:b)
			}
		}
	} // Sub;

	func Mul (b: Integer) -> Integer {
		var z: Integer
		z = MulAbs(self, b:b)
		if self.negative != b.negative {
			z.negative = !z.negative
		}
		return z
	} // Mul;

	private func InplaceDivRem1 (var pout: [Digit], var pin: [Digit], psize: Int, n: Digit) -> Digit {
		/* Divide long @oparam{pin}, with @oparam{size} digits, by non-zero digit
		@oparam{n}, storing quotient in @oparam{pout}, and returning the remainder.
		@oparam{pin[0]} and @oparam{pout[0]} point at the LSD.  It's OK for
		@samp{pin=pout} on entry, which saves oodles of mallocs/frees in
		long_format, but that should be {ne with great care since longs are
		immutable.  */
		var rem, hi: LONGINT
		var size = psize
	
		assert((n > 0) && (n < Integer.base), "InplaceDivRem1 assertion failed")
		rem = 0;
		while (size > 0) {
			size--
			rem = rem*Integer.base + pin[size]
			hi = rem / n
			pout[size] = hi
			rem -= hi*n
		}
		return rem
	} // InplaceDivRem1;

	private func DivRem1 (a: Integer, n: Digit, var rem: Digit) -> Integer {
		/* Divide a long integer by a digit, returning both the quotient
		(as function result) and the remainder (through *prem).
		The sign of a is ignored; n should not be zero. */
		var size = a.digit.count
		var z: Integer
		
		assert((n > 0) && (n < Integer.base), "DivRem1 assertion failed")
		z = Integer(size: size)
		rem = InplaceDivRem1(z.digit, pin:a.digit, psize:size, n:n)
		Normalize(z)
		return z
	} // DivRem1;

	func MulAdd1 (a: Integer, n: Digit, add: Digit) -> Integer {
		/* Multiply by a single digit and add a single digit, ignoring the sign. */
		var sizeA, i: Int
		var carry: Digit
		var z: Integer
		sizeA = a.digit.count
		z = Integer(size:sizeA+1)
		carry = add
		i = 0
		while i != sizeA {
			carry += a.digit[i]*n
			z.digit[i] = carry % Integer.base
			carry = carry / Integer.base
			i++
		}
		z.digit[i] = carry
		Normalize(z)
		return z
	} // MulAdd1;

	private func DivRemAbs (v1: Integer, w1: Integer, var rem: Integer) -> Integer {
		/* Unsigned long division with remainder -- the algorithm.  */
		
		var sizeV, sizeW, j, k, i: Int
		var d, vj, zz: TwoDigits
		var v, w, a: Integer
		var q, z, carry: TwoDigits
		
		sizeV = v1.digit.count; sizeW = w1.digit.count
		d = TwoDigits(Integer.base / (w1.digit[sizeW-1]+1))
		v = MulAdd1(v1, n:Digit(d), add:0)
		w = MulAdd1(w1, n:Digit(d), add:0)
		
		assert((sizeV >= sizeW) && (sizeW > 1), "DivRemAbs assertion 1 failed")
		assert(sizeW == w.digit.count, "DivRemAbs assertion 2 failed")
		sizeV = v.digit.count
		a = Integer(size:sizeV-sizeW+1)
		
		j = sizeV
		k = a.digit.count-1
		while k >= 0 {
			if j >= sizeV {
				vj = 0
			} else {
				vj = TwoDigits(v.digit[j])
			}
			carry = 0
			
			if vj == TwoDigits(w.digit[sizeW-1]) {
				q = TwoDigits(Integer.mask)
			} else {
				q = TwoDigits((vj*TwoDigits(Integer.base) + TwoDigits(v.digit[j-1])) / TwoDigits(w.digit[sizeW-1]))
			}
			let cond1 = vj*TwoDigits(Integer.base) + TwoDigits(v.digit[j-1]) - q*TwoDigits(w.digit[sizeW-1])
			while TwoDigits(w.digit[sizeW-2])*q > cond1*TwoDigits(Integer.base) + TwoDigits(v.digit[j-2]) {
				q--
			}
			
			i = 0;
			while (i < sizeW) && (i+k < sizeV) {
				z = TwoDigits(w.digit[i])*q
				zz = z / TwoDigits(Integer.base)
				carry += TwoDigits(v.digit[i+k]) - z + zz*TwoDigits(Integer.base)
				v.digit[i+k] = Digit(carry % TwoDigits(Integer.base))
				carry = carry >> TwoDigits(Integer.shift)
				carry -= zz
				i++
			}
			
			if i+k < sizeV {
				carry += TwoDigits(v.digit[i+k])
				v.digit[i+k] = 0
			}
			
			if carry == 0 {
				a.digit[k] = Digit(q)
			} else {
				assert(carry == -1, "DivRemAbs carry != -1")
				a.digit[k] = Digit(q-1)
				carry = 0
				i = 0
				while (i < sizeW) && (i+k < sizeV) {
					carry += TwoDigits(v.digit[i+k] + w.digit[i])
					v.digit[i+k] = Digit(carry % TwoDigits(Integer.base))
					carry = carry >> TwoDigits(Integer.shift)
					i++
				}
			}
			j--; k--
		}
		Normalize(a)
		rem = DivRem1(v, n:Digit(d), rem:Digit(d))
		return a
	} // DivRemAbs;

	private func DivRem (a: Integer, b: Integer, var div:Integer, var rem: Integer) {
		var sizeA, sizeB: Int
		var remDigit: Digit
		var z: Integer
		
		sizeA = a.digit.count; sizeB = b.digit.count
		
		assert(sizeB != 0, "Assert failed in DivRem")                   /* division by zero? */
		if (sizeA < sizeB) || ((sizeA == sizeB) && (a.digit[sizeA-1] < b.digit[sizeB-1])) {
			/* |a| < |b| */
			div = Integer.zero
			rem = a;
		} else {
			if sizeB == 1 {
				remDigit = 0
				z = DivRem1(a, n:b.digit[0], rem:remDigit)
				rem = Integer(value:Int(remDigit))
			} else {
				z = DivRemAbs(a, w1:b, rem:rem)
			}
			
			/* Set the signs.  The quotient z has the sign of a*b; the remainder r
			has the sign of b, so a = b*z + r.  */
			if a.negative != b.negative { z.negative = !z.negative }
			if b.negative { rem.negative = !rem.negative }
			div = z
		}
	} // DivRem;

	func DivMod (w: Integer, var div: Integer, var mod: Integer) {
		DivRem(self, b:w, div:div, rem:mod)
		
		/* Makes the DIV/MOD results compliant with most Oberon-2 implementations. */
		if self.negative != w.negative {
			if self.Cmp(div.Mul(w)) != 0 { div = div.Sub(Integer.one) }
			if mod.NonZero() { mod = w.Sub(mod) }
		}
	} // DivMod;

	func Div (w: Integer) -> Integer {
		var div, mod: Integer
		self.DivMod(w, div:div, mod:mod)
		return div
	} // Div;
	
	func Mod (w: Integer) -> Integer {
		var div, mod: Integer
		self.DivMod(w, div:div, mod:mod)
		return mod
	} // Mod;

	func description (outputBase: Int) -> String {
		/* Convert a long int object to a string, using a given conversion base. return a string object.  */
		let conversion : String = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
		var sizeA, i, bits, size, power, newpow, ntostore, d, pos, baseBits, accumBits: Int
		var accum: TwoDigits
		var str: String
		var powbase, rem, nextrem: Digit
		var c: Character
		var scratch: Integer
		
		sizeA = self.digit.count
		assert((outputBase >= 2) & (outputBase <= 36), "Assert outputBase failed in describe")
		
		/* Compute a rough upper bound for the length of the string */
		i = outputBase
		bits = 0
		while i > 1 {
			bits++
			i = i / 2
		}
		i = 5 + (sizeA * Int(Integer.shift) + bits - 1) / bits
		str = ""
		pos = i
		
		if sizeA == 0 {
			return "0"
		} else if powerOf2[outputBase] > 0 {
			baseBits = powerOf2[outputBase];
			accum = 0;
			accumBits = 0;
			i = 0;
			
			while i != sizeA {
				accum += TwoDigits(self.digit[i]) << TwoDigits(accumBits)
				accumBits += Int(Integer.shift)
				assert(accumBits >= baseBits, "Assert in describe")
				do {
					d = Int(accum % TwoDigits(outputBase))  /* expensive, a mask op would do */
					let index = advance(conversion.startIndex, d)
					c = conversion[index]
					str = [c] + str
					accumBits -= baseBits
					accum = accum >> TwoDigits(baseBits)
				} while !((accumBits < baseBits) && (i < sizeA-1) || (accum == 0))
				i++
			}
			if self.negative {
				pos--
				str = "-" + str
			}
			return str
			
		} else {
			/* powbase <- largest power of outputBase that fits in a digit. */
			powbase = Digit(outputBase)  /* powbase == outputBase ** power */
			power = 1
			while true {
				newpow = Int(powbase) * outputBase
				if newpow > Int(Integer.base) {
					break
				}
				powbase = Digit(newpow)
				power++
			}
			
			/* Get a scratch area for repeated division. */
			scratch = Integer(a: self)
			size = sizeA;
			
			/* Repeatedly divide by powbase. */
			do {
				ntostore = power
				rem = InplaceDivRem1(scratch.digit, pin:scratch.digit, psize:size, n:powbase)
				if scratch.digit[size-1] == 0 {
					size--
				}
				
				/* Break rem into digits. */
				assert(ntostore > 0, "Assertion in describe ntostore")
				do {
					nextrem = rem / Digit(outputBase)
					d = rem - nextrem * Digit(outputBase)
					let index = advance(conversion.startIndex, d)
					c = conversion[index]
					str = [c] + str
					rem = nextrem
					ntostore--
					/* Termination is a bit delicate:  must not
					store leading zeroes, so must get out if
					remaining quotient and rem are both 0. */
				} while !((ntostore == 0) || ((size == 0) && (rem == 0)))
			} while (size != 0)
			if self.negative {
				str = "-" + str
			}
			return str
		}
	} // Format;


	func ToInteger (str: String, inputBase: Int) -> Integer {
		var d: Int
		var negative = false
		var z: Integer
		var c: Character
		var s = str.uppercaseString
		
		assert((2 <= inputBase) & (inputBase <= 36), "ToInteger assertion")
		
		/* skip leading whitespace */
		s = s.stringByTrimmingCharactersInSet(NSCharacterSet.whitespaceAndNewlineCharacterSet())
		
		/* handle sign */
		if !s.isEmpty {
			if s[s.startIndex] == "-" {
				negative = true
				c = s.removeAtIndex(s.startIndex)
			} else if s[s.startIndex] == "+" {
				c = s.removeAtIndex(s.startIndex)
			}
		}
		
		z = Integer.zero
		while !s.isEmpty {
			c = s.removeAtIndex(s.startIndex)
			let ls = "" + [c]
			d = strtol(ls, nil, 36)
			z = MulAdd1(z, n:Digit(inputBase), add:Digit(d))
		}
		z.negative = negative
		return z
	} // ToInteger

	func Invert() -> Integer {
		/** Bitwise complement.  The result equals @samp{-(a+1)}.  */
		var a = self.Add(Integer.one)
		a.negative = !self.negative
		return a
	} // Invert;

	func LShift (n: Int) -> Integer {
		var wordShift, remShift, oldSize, newSize, i, j: Int
		var accum: Digit
		var z: Integer
		assert(n >= 0, "LShift assertion: shift is negative")
		wordShift = n / Int(Integer.shift)
		remShift = n % Int(Integer.shift)
		
		oldSize = self.digit.count
		newSize = oldSize+wordShift
		if remShift != 0 {
			newSize++
		}
		
		z = Integer(size: newSize)
		z.negative = self.negative
		i = 0
		while (i != wordShift) {
			z.digit[i] = 0
			i++
		}
		accum = 0;
		j = 0;
		while j != oldSize {
			accum += self.digit[j] << Digit(remShift)
			z.digit[i] = accum % Integer.base
			accum = accum / Integer.base
			i++
			j++
		}
		if remShift != 0 {
			z.digit[newSize-1] = Digit(accum)
		} else {
			assert(accum == 0, "Invert assertion: remShift == 0")
		}
		Normalize(z)
		return z
	} // LShift;

	func RShift (n: Int) -> Integer {
		var wordShift, newSize, loShift, hiShift, i, j: Int
		var loMask, hiMask: Int
		var z: Integer
		
		assert(n >= 0, "RShift assertion: shift is negative");
		if self.negative {
			var a = self.Invert()
			a = a.RShift(n)
			return a.Invert()
		} else {
			wordShift = n / Int(Integer.shift)
			newSize = self.digit.count-wordShift
			if newSize <= 0 {
				return Integer.zero
			} else {
				loShift = n % Int(Integer.shift)
				hiShift = Int(Integer.shift) - loShift
				loMask = (1 << hiShift) - 1
				hiMask = Int(Integer.base)-1-((1 << hiShift) - 1)
				z = Integer(size: newSize)
				i = 0;
				j = wordShift
				while i < newSize {
					z.digit[i] = (self.digit[j] >> Digit(loShift)) & Digit(loMask)
					if i+1 < newSize {
						z.digit[i] += SHORT(S.VAL(LONGINT, S.VAL(SET, ASH(a.digit[j+1], hiShift))*hiMask))
					}
					i++
					j++
				}
				Normalize(z)
				return z
			}
		}
	} // RShift;

func (a: Integer) Store*(w: Storable.Writer) RAISES IO.Error;
var
i: LONGINT;
{
w.WriteNum(a.size);
FOR i = 0 TO ABS(a.size)-1 {
w.WriteInt(a.digit[i]);
}
} // Store;

func (a: Integer) Load*(r: Storable.Reader) RAISES IO.Error;
var
i: LONGINT;
{
r.ReadNum(a.size);
NEW(a.digit, ABS(a.size));
FOR i = 0 TO ABS(a.size)-1 {
r.ReadInt(a.digit[i]);
}
} // Load;

/* MG */
func (x: Integer) GCD* (y: Integer) : Integer;
/** Pre: x,y >= 0; Post: return gcd(x,y) */
var
swap: Integer;
{
/* Euclid's gcd algorithm  (very elegant and very ancient!) */

/* To start everything must be non-negative and x>=y */
x=x.Abs(); y=y.Abs();

IF x.Cmp(y) < 0 THEN
swap=x; x=y; y=swap
}

while y.NonZero() {
swap=x.Mod(y);     /* division algorithm */
x=y; y=swap;		/* set up next iteration */
}
return x;
} // GCD;

/* MG */
func (x: Integer) Power* (exp: LONGINT) : Integer;
/** Pre: exp>=0; Post: return x**exp */
var y: Integer;
{
IF exp<0 THEN return zero }  /* x**-exp = 0 */
y=one;
LOOP
IF ODD(exp{ y=y.Mul(x) }
exp=exp / 2;
IF exp=0 THEN EXIT }
x=x.Mul(x)
}
return y
} // Power;

/* MG */
func Factorial* (x: LONGINT) : Integer;
/** Pre: x>=0; Post: return x!=x(x-1)(x-2)...(2)(1) */
var f, xi: Integer;
{
IF x<0 THEN
return zero                           /* out of range */
}
IF x<2 THEN return one }             /* 0! & 1! */
f=one; xi=NewInt(x);
while x>1 {
f=f.Mul(xi); DEC(x);                 /* f=f*x */
xi.digit[0]=SHORT(x % base);       /* convert to Integer */
IF xi.size>1 THEN
xi.digit[1]=SHORT((x / base) % base);
IF xi.size>2 THEN
xi.digit[2]=SHORT(x / (base*base));
} //
}
}
return f
} // Factorial;

/* MG */
func Ran{m* (digits: LONGINT) : Integer;
/** Pre: x>0; Post: return digits-length ran{m number */
let a=16385; c=1;
var n: Integer; i: LONGINT; s: Time.TimeStamp;
{
assert(digits>0, 109); Time.GetTime(s);
n=NewInstance(2215*digits / 10000);     /* n=digits*log32768(10) */
n.digit[0]=SHORT((a*SHORT(s.msecs % B)+c) % B);
FOR i=1 TO LEN(n.digit^)-1 { n.digit[i]=SHORT((a*n.digit[i-1]+c) % B) }
return n
} // Ran{m;


/* ******************************************************* */
/*  Logical operations                                     */

func Set (x: INTEGER) : SET;
var
i: LONGINT;
{
i=x; return S.VAL(SET, i)
} // Set;

func Int (x: SET) : INTEGER;
var
i: LONGINT;
{
i=S.VAL(LONGINT, x);
return SHORT(i)
} // Int;

func And (x, y: INTEGER) : INTEGER;
{
return Int(Set(x) * Set(y));
} // And;

func Or (x, y: INTEGER) : INTEGER;
{
return Int(Set(x) + Set(y));
} // Or;

func Xor (x, y: INTEGER) : INTEGER;
{
return Int(Set(x) / Set(y));
} // Xor;

func Dup (x: Integer; max: LONGINT) : Integer;
var
y: Integer;
i: LONGINT;
{
y=NewInstance(max);
FOR i=0 TO x.size-1 {
y.digit[i]=x.digit[i]
}
return y
} // Dup;

/* MG */
func (x: Integer) And * (y : Integer) : Integer;
/** Post: return bitwise x and y */
var
i, max : LONGINT;
a, b, z : Integer;
{
max=x.size; i=y.size;
IF max>i THEN a=x; b=Dup(y, max); z=b
else a=Dup(x, i); b=y; z=a; max=i
}
FOR i=0 TO max-1 {
z.digit[i]=And(a.digit[i], b.digit[i])
}
Normalize(z);
return z
} // And;

	/* MG */
	func (x: Integer) Or * (y : Integer) : Integer;
	/** Post: return bitwise x or y */
	var
	i, max : LONGINT;
	a, b, z : Integer;
	{
	max=x.size; i=y.size;
	IF max>i THEN a=x; b=Dup(y, max); z=b
	else a=Dup(x, i); b=y; z=a; max=i
	}
	FOR i=0 TO max-1 {
	z.digit[i]=Or(a.digit[i], b.digit[i])
	}
	Normalize(z);
	return z
	} // Or;
	
	/* MG */
	func (x: Integer) Xor * (y : Integer) : Integer;
	/** Post: return bitwise x xor y */
	var
	i, max : LONGINT;
	a, b, z : Integer;
	{
	max=x.size; i=y.size;
	IF max>i THEN a=x; b=Dup(y, max); z=b
	else a=Dup(x, i); b=y; z=a; max=i
	}
	FOR i=0 TO max-1 {
	z.digit[i]=Xor(a.digit[i], b.digit[i])
	}
	Normalize(z);
	return z
	} // Xor;
	
	/* MG */
	func (x: Integer) SetBit * (bit: LONGINT) : Integer {
	/** Post: Set 'bit' in 'x' */
	var
	y: Integer;
	y=one.LShift(bit);
	return x.Or(y)
	} // SetBit;
	
	/* MG */
	func (x: Integer) ClearBit * (bit: LONGINT) : Integer {
	/** Post: Clear 'bit' in 'x' */
	var
	y: Integer;
	y=one.LShift(bit);
	return x.And(y.Invert())
	} // ClearBit;
	
	/* MG */
	func (x: Integer) ToggleBit * (bit: LONGINT) : Integer {
	/** Post: Toggle the 'bit' in 'x' */
	return x.Xor(one.LShift(bit))
	} // ToggleBit;

/*
func OutInt (n: Integer);
{
Out.Object(n)
} // OutInt;

func Test;
var
s, n, m: Integer;
str: STRING;
{
Out.String("ZERO="); OutInt(zero); Out.Ln;
Out.String("ONE="); OutInt(one); Out.Ln;
n=NewLatin1("123456789012345678900000000000000000000", 10);
m=NewLatin1(                   "55554444333322221111", 10);
CASE n.Cmp(m) OF
| 0: Out.String("n=m")
| 1: Out.String("n>m")
| else Out.String("n<m")
}
Out.Ln;
Out.String("n="); OutInt(n); Out.Ln;
Out.String("m="); OutInt(m); Out.Ln;
s=n.Mul(m);
Out.String("n*m="); OutInt(s); Out.Ln;
s=n.Mul(m);
Out.String("(n*m) / m="); OutInt(s.Div(m)); Out.Ln;
s=n.Add(m);
Out.String("n+m="); OutInt(s); Out.Ln;
s=n.Sub(m);
Out.String("n-m="); OutInt(s); Out.Ln;
s=n.Div(m);
Out.String("n / m ="); OutInt(s); Out.Ln;
s=n.Mod(m);
Out.String("n % m="); OutInt(s); Out.Ln;
s=n.Div(m); s=s.Mul(m); s=s.Add(n.Mod(m));
Out.String("m*(n / m)+(n % m)="); OutInt(s); Out.Ln;
n=NewInt(2);
s=n.Power(64);
Out.String("2^64="); OutInt(s); Out.Ln;
n=NewLatin1("-FFFF", 16);
Out.String("-FFFF="); OutInt(n); Out.Ln;
Out.String("-FFFF="); str=n.Format(16); Out.Object(str); Out.String("H"); Out.Ln;
n=NewLatin1("-10000000000000", 10);
Out.String("-10000000000000="); OutInt(n); Out.Ln;
n=NewLatin1("-10000000000000000", 2);
Out.String("-10000000000000B="); OutInt(n); Out.Ln;
Out.String("-10000000000000000B="); str=n.Format(2); Out.Object(str); Out.String("B"); Out.Ln;
n=NewInt(-8);
Out.String("-8^3="); OutInt(n.Power(3)); Out.Ln;
Out.String("69!="); OutInt(Factorial(69)); Out.Ln;
n=NewLatin1("123456789012345", 10);
Out.String("GCD(123456789012345, 87654321)="); OutInt(n.GCD(NewInt(87654321))); Out.Ln;
Out.String("Ran{m(50)="); OutInt(Ran{m(50)); Out.Ln;
Out.String("New(987654321)="); OutInt(NewInt(987654321)); Out.Ln;
Out.String("zero SetBit 16="); OutInt(zero.SetBit(16)); Out.Ln;
Out.String("one ClearBit 0="); OutInt(one.ClearBit(0)); Out.Ln;
Out.String("zero ToggleBit 16="); OutInt(zero.ToggleBit(16)); Out.Ln;
} // Test;
*/

//func Init * ();
//var
//i: LONGINT;
//{
//FOR i = 0 TO LEN(powerOf2)-1 {
//powerOf2[i] = -1;
//}
//powerOf2[ 2] = 1;
//powerOf2[ 4] = 2;
//powerOf2[ 8] = 3;
//powerOf2[16] = 4;
//powerOf2[32] = 5;
//
//zero = NewInstance(0);
//one = NewInstance(1); one.digit[0] = 1;
//} // Init;
//
//{
//Init;
///* Test */
	
} // Integers.

