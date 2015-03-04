//
//  Real.swift
//  XNumbers
//
//  Created by Mike Griebling on 4 Mar 2015.
//  Copyright (c) 2015 Computer Inspirations. All rights reserved.
//

import Foundation

/*
Reals - Mathematical functions for an arbitrary-precision floating
point representation named Real.

Copyright (C) 1997-2004 Michael Griebling
From an original FORTRAN library MPFUN by David H. Bailey, NASA Ames
Research Center which is available from:

http://crd.lbl.gov/~dhbailey/mpdist/index.html

Source translated with permission of the original author.

This module is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This module is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*/

/*
From the author's description:

The following information is a brief description of this program.  For
full details and instructions for usage, see the paper "A Portable High
Performance Multiprecision Package", available from the author.

This package of Fortran subroutines performs multiprecision floating point
arithmetic.  If sufficient main memory is available, the maximum precision
level is at least 16 million digits.  The maximum dynamic range is at
least 10^(+-14,000,000).  It employs advanced algorithms, including an
FFT-based multiplication routine [Not yet in Oberon-2--MG] and some recently discovered
quadratically convergent algorithms for pi, exp and log.  The package also
features extensive debug and self-checking facilities, so that it can be
used as a rigorous system integrity test.  All of the routines in this
package have been written to facilitate vector, parallel processing and/or
RISC processing.

My comments:

The current algorithms are not optimized for really large numbers.  if there
is some interest, I will be porting those algorithms as well from the original
author.  The existing algorithms will work in a reasonable time for most numbers
and are hard-limited via a constant `maxDigits' to a maximum of about 500 digits.
This constant can be easily increased.  Of course, performance will suffer.  The
actual working precision is adjustable via the `SetWords' routine.  The actual
number of words will { be reflected in the `curMant' read-only constant.
Each word gives about 7.22 digits of precision.  The default precision is set
to `maxDigits'.

There may be a couple of bugs in several routines which affect the accuracy
in the last few places of the result.  This shouldn't make much difference to
casual users as long as the precision is set to be 10-20 digits more than you
need.

Modified to use the new OO2C Boxed data type to facilitate I/O.
*/

// IMPORT rm = LRealMath, rx = LRealMathExt, Object, Object:Boxed, IO, ADT:Storable, Out;

struct Real /* : Equatable, Comparable, Printable, Hashable  */ {
	
	private static let DEBUG = false
	private static let ZERO = 0.0
	private static let ONE  = 1
	private static let HALF = 0.5
	private static let invLn2 = 1.4426950408889633
	private static let Ln2 = 0.693147180559945309
	
	/* numeric precision-setting constants */
	static let maxDigits=520                                  /* initial precision level in digits */
	static let outDigits=56                                   /* initial output precision level in digits */
	static let log10eps=10-maxDigits                          /* log10 of initial eps level */
	static let digsPerWord=7.224719896
	static let maxMant=Int(Double(maxDigits)/digsPerWord+1.5)	  /* hardcoded maximum mantissa words */
	static let maxExp=2000000                                 /* maximum exponent */
	
	/* internal scaling constants */
	private static let mpbbx=4096.0
	private static let radix=mpbbx*mpbbx
	private static let mpbx2=radix*radix
	private static let mprbx=1.0/mpbbx
	private static let invRadix=mprbx*mprbx
	private static let mprx2=invRadix*invRadix
	private static let mprxx=16*mprx2
	
	/* miscellaneous constants */
	private static let NBT=24
	private static let NPR=32
	private static let MPIRD=1
	private static let NIT=3
	
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
	}

	private typealias RealArray = [Double]
	private var real: RealArray

	private struct FixedLReal {
		var r = RealArray(count:maxMant+8, repeatedValue: 0)
	}
	private struct FixedReal {
		var r = [Float](count:maxMant+8, repeatedValue: 0)
	}
	private struct Real8 {
		var r = [Float](count:8, repeatedValue: 0)
	}

	static var err : Int = 0
	static var debug : Int = 0
	
	private var curMant: Int, numBits: Int, sigDigs: Int
	var currentMantissa : Int {
		return curMant
	}
	var significantDigits: Int {
		return sigDigs
	}
	static let eps, ln2, pi, ln10, one, zero: Real
	
	/* Speed up very large factorials */
	private static let fact100000 = Real(fromString: "2.82422940796034787429342157802453551847749492609122485057891808654297795090106301787" +
		"2551771413831163610713611737361962951474996183123918022726073409093832422005556968866" +
		"7840380377379444961268380147875111966906386044926144538111370090160766866405407170565" +
		"9522612980419583567789090475415128711408369242515352930962606722710387442460886354543" +
		"6398293174776177553262185112647485586491818038151987716121968151412990230446382406889" +
		"65083575002296499396423642566352716149352078013312029433930594819960435395E+456573")
	private static let fact200000 = Real(fromString: "1.42022534547031440496694633368230597608996535674640162269622474462922677851609968565" +
		"0082553407879081329793135215376044079156034995456792440298907698327157087066286303182" +
		"5017623219084061256114573810476379717993512721296946450311966946288603601628556916324" +
		"4648770389480378251602819955788158117868794159097393435551925337859488859955701890215" +
		"4897701489299055308898497995637308558323762472340297297985768615383843817767617482336" +
		"58088832083067784773860727948019819421544453708479108922842308732119367523E+973350")
	private static let fact300000 = Real(fromString: "1.47739153173803909429290747493561414549932051952374408795791384376505240135170347653" +
		"2418899010198829649964892384917975071774129347530818714855332590431274389350896312260" +
		"9806517049255450392732030550644905383028447932954677114843634677423190476154873121734" +
		"1025709069449617692835058182617595979172730842885422104493186754451133578337885783639" +
		"5817086347597543562761254468984063083893218681681196080370667835191599919282226318984" +
		"62208531038106191099127491142755685344624042273747482199422127053615182013E+1512851")
	
	private static var Seed = Real(fromInteger:4)
	static var status = Status.Okay
	
	/*---------------------------------------------------------*/
	/* Initializers											   */
	init (fromString: String) {
		// TBD
		
	}
	
	init (fromInteger: Int) {
		 NumbExpToReal (a: Double, n: Int, inout b: RealArray)
	}

	/*---------------------------------------------------------*/
	/* Internal basic operator definitions                     */
	
	private func Min (x: Int, y: Int) -> Int {
		if x<y { return x } else { return y }
	} //Min;
	
	private func Max (x: Int, y: Int) -> Int {
		if x>y { return x } else { return y }
	} //Max;
	
	private func Sign (x: Int, y: Double) -> Int {
		if y<Real.ZERO {
			return -Int(abs(x))
		} else {
			return Int(abs(x))
		}
	} //Sign;
	
	private func Zero (inout x: [Double]) {
		/* x to zero */
		x[0] = Real.ZERO; x[1] = Real.ZERO
	} //Zero;
	
	private func ToInt (x: Double) -> Int {
		if x<Real.ZERO {
			return -Int(-x)
		} else {
			return Int(x)
		}
	} //ToInt;
	
	private func ODD (x: Int) -> Bool {
		return (x&1) != 0;
	}

	private func ipower (x: Double, base: Int) -> Double {
		/* ipower(x, base) returns the x to the integer power base where base*Log2(x) < Log2(Max) */
		var y: Double
		var neg: Bool
		var ibase = base
		var ix = x
		
		/* compute x**base using an optimised algorithm from Knuth, slightly
		altered : p442, The Art Of Computer Programming, Vol 2 */
		y = 1; if ibase<0 { neg = true; ibase  =  -ibase } else { neg =  false }
		for ;; {
			if ODD(ibase) { y = y*ix };
			ibase = ibase / 2; if ibase==0 { break };
			ix = ix*ix
		}
		if neg { return 1.0/y } else { return y }
	} //ipower;
	
	private func Reduce (inout a: Double, inout exp: Int) {
		/* reduce `a' to be within 1 and radix and adjust
		the exponent `exp' appropriately */
		let maxIterations=100
		var k: Int
		
		if a>=Real.radix {
			for k = 1; k<=maxIterations; k++ {
				a = Real.invRadix*a;
				if a<Real.radix { exp += k; return }
			}
		} else if a<1.0 {
			for k = 1; k<=maxIterations; k++ {
				a = Real.radix*a;
				if a>=1.0 { exp -= k; return }
			}
		}
	} //Reduce;


	private func copy (a: RealArray, inout b: RealArray) {
		/* b = a */
		var ia, na, i: Int
		ia = Sign(Real.ONE, y: a[0]); na = Min(Int(abs(a[0])), y: curMant)
		if na==0 { Zero(&b); return }
		b[0] = Double(Sign(na, y:Double(ia)))
		for i = 1; i<=na+2; i++ { b[i] = a[i] }
	} //copy;
	
	private func Write (q: RealArray) {
		var i: Int
		/* output raw number */
		for i = 0; i<=Int(abs(q[0]))+1; i++ {
			println("x[\(i)]=\(Int(q[i]))")
		}
		println()
	} //Write;

	private func Round (inout a: RealArray) {
		/*
		This performs rounding and truncation of the a number.
		The maxExp value is the absolute value of the largest expReal.ONEnt
		word allowed for ext}ed numbers.
		*/
		var a2: Double
		var allZeros: Bool
		var ia, na, n4, i, k: Int
		
		/* error testing */
		if Real.err != 0 { Zero(&a); return }
		
		/* check for initial zeros */
		a2 = a[1]; a[1] = Real.ZERO; ia = Sign(Real.ONE, y: a[0])
		na = Min(Int(abs(a[0])), y: curMant)
		n4 = na+4; k = 0
		if a[2]==Real.ZERO {
			/* find the nonzero word and shift the entire number left.
			The length of the result is reduced by the length of the
			shift */
			allZeros = true; i = 4;
			while allZeros & (i<=n4) {
				if a[i-1] != Real.ZERO { allZeros = false; k = i-3 }
				i++
			}
			if allZeros { Zero(&a); return };
			for i = 2; i<=n4-k-1; i++ { a[i] = a[i+k] }
			a2 = a2-Double(k); na = na-Max(k-2, y:0)
		}
		
		/* perform rounding dep}ing on MPIRD */
		if (na==curMant) && (Real.MPIRD>=1) {
			if ((Real.MPIRD==1) && (a[na+2]>=Real.HALF*Real.radix)) || ((Real.MPIRD==2) && (a[na+2]>=1.0)) {
				a[na+1] = a[na+1]+1.0
			}
			
			/* release carries as far as necessary due to rounding */
			i = na+1
			for ;; {
				if i<2 { a[2] = a[1]; na = 1; a2 = a2+1.0; break }
				if a[i]<Real.radix { break }
				a[i] = a[i]-Real.radix; a[i-1] = a[i-1]+1.0
				i--
			}
		}
		
		/* At least the last mantissa word is zero.  Find the last
		nonzero word and adjust the length of the result accordingly */
		if a[na+1] == 0.0 {
			i = na+2
			while i >= 3 {
				if a[i-1] != 0.0 { na = i-2; i = 1 }
				i--
			};
			if i != 0 { Zero(&a); return }
		}
		
		/* check for overflow and underflow */
		if a2 <= Double(Real.maxExp) {
			println("*** Round: Exponent underflow!")
			Real.err = 68
		} else if a2 > Double(Real.maxExp) {
			println("*** Round: Exponent overflow!")
			Real.err = 69
		}
		
		/* check for zero */
		if a[2] == 0.0 {
			Zero(&a)
		} else {
			a[0] = Double(Sign(na, y:Double(ia))); a[1] = a2
			a[na+2] = 0.0; a[na+3] = 0.0
		}
	} //Round;

	private func Normalize (var d: RealArray, inout a: RealArray) {
		/*
		This converts the number in array d to the standard normalized
		form in a.  Values in d are often negative or exceed the maximum
		radix radix in result arrays, and this fixes them.
		
		Normalize assumes that two extra mantissa words are input at the
		end of d.  This reduces precision loss when it is necessary to
		shift the result to the left.  The output is placed in the array
		a.  Debug output starts with debug = 10.
		*/
		var a2: Double
		var t1, t2, t3: Double
		var ia, na, n4, i: Int
		
		if Real.err != 0 { Zero(&a); return }
		ia = Sign(Real.ONE, y:d[0]); na = Min(Int(abs(d[0])), y:curMant)
		if na == 0 { Zero(&a); return }
		n4 = na+4; a2 = d[1]; d[1] = 0.0
		for ;; {
			t1 = 0.0
			for i = n4-1; i >= 2; i-- {
				t3 = t1+d[i]; t2 = Real.invRadix*t3; t1 = Double(Int(t2))
				if (t2 < 0.0) && (t1 != t2) { t1 = t1-1.0 }
				d[i] = t3-t1*Real.radix
			}
			d[1] = d[1]+t1;
			if d[1] < Real.ZERO {
				/* negate all words and re-normalize */
				ia = -ia; d[2] = d[2]+Real.radix*d[1]; d[1] = Real.ZERO;
				for i = 1; i < n4; i++ { d[i] = -d[i] }
			} else if d[1] > Real.ZERO {
				/* nonzero number spilled into d[1].  Shift the entire number
				right one cell.  The exponent and length of the result are
				increased by one. */
				for i = n4-1; i >= 2; i-- { a[i] = d[i-1] }
				na = Min(na+1, y:curMant); a2 = a2+1.0
				break
			} else {
				for i = 2; i<=n4-1; i++ { a[i] = d[i] }
				break
			}
		}
		
		/* perform rounding and truncation */
		a[0] = Double(Sign(na, y:Double(ia))); a[1] = a2
		Round(&a)
	} //Normalize;

	private func RealToNumbExp (a: RealArray, inout b: Double, inout n: Int) {
		/*
		This routine converts the multiprecision number `a' to the number
		`d'*2**`n', accurate to between 14-17 digits, dep}ing on the
		system.  `b' will be between 1 and radix.
		*/
		var aa: Double
		var na: Int
		
		/* handle error propogation */
		if Real.err != 0 { b = Real.ZERO; n = 0; return }
		
		/* trivial cases */
		if a[0] == Real.ZERO { b = Real.ZERO; n = 0; return }
		
		/* real algorithm */
		na = Int(abs(a[0])); aa = a[2]
		if na >= 2 { aa = aa+Real.invRadix*a[3] }
		if na >= 3 { aa = aa+Real.mprx2*a[4] }
		if na >= 4 { aa = aa+Real.invRadix*Real.mprx2*a[5] }
		
		n = Real.NBT*Int(a[1])
		if a[0] < 0 { b = -aa } else { b = aa }
	} //RealToNumbExp;

	private func NumbExpToReal (a: Double, n: Int, inout b: RealArray) {
		/*
		This routine converts the number `a'*2**`n' to an extended form
		in `b'.  All bits of `a' are recovered in `b'.  However, note
		for example that if `a'=0.1D0 and `n'=0, { `b' will NOT be
		the multiprecision equivalent of 1/10.  Debug output starts
		with debug = 9.  Pre: LEN(b)>=8.
		*/
		var aa: Double
		var n1, n2, i: Int
		
		assert(b.count>=8, "Assertion NumbExptoReal")
		
		/* check for zero */
		if a == Real.ZERO { Zero(&b); return }
		n1 = Int(n/Real.NBT); n2 = n-Real.NBT*n1
		aa = abs(a)*ipower(2.0, base: n2)
		
		/* reduce aa to within 1 and radix */
		Reduce(&aa, exp: &n1)
		
		/* store successive sections of aa into b */
		b[1] = Double(n1)
		b[2] = Double(Int(aa)); aa = Real.radix*(aa-b[2])
		b[3] = Double(Int(aa)); aa = Real.radix*(aa-b[3])
		b[4] = Double(Int(aa)); aa = Real.radix*(aa-b[4])
		b[5] = Double(Int(aa))
		b[6] = Real.ZERO
		b[7] = Real.ZERO
		
		/* find length of resultant number */
		for i = 5; i>=2; i-- {
			if b[i] != Real.ZERO { b[0] = Double(Sign(i-1, y:a)); return }
		}
		b[0] = Real.ZERO
	} //NumbExpToReal;

	private func Add (inout c: RealArray, a: RealArray, b : RealArray) {
		/*
		This routine adds MP numbers a and b to yield the MP
		sum c.  It attempts to include all significance of a
		and b in the result, up to the maximum mantissa length
		curMant.  Debug output starts with debug = 9.
		*/
		var i, ia, ib, na, nb, nsh: Int
		var ixa, ixb, ixd, ish, m1, m2, m3, m4, m5, nd: Int
		var db: Double
		var d: FixedLReal
		
		if Real.err != 0 { Zero(&c); return }
		if Real.debug >= 9 {
			println("Add 1"); Write(a)
			println("Add 2"); Write(b)
		}
		ia = Sign(Real.ONE, y: a[0]); ib = Sign(Real.ONE, y: b[0])
		na = Min(Int(abs(a[0])), y: curMant)
		nb = Min(Int(abs(b[0])), y: curMant)
		
		/* check for zero inputs */
		if na == 0 { /* a is zero -- the result is b */
			c[0] = Double(Sign(nb, y:Double(ib)))
			for i = 1; i<nb; i++ { c[i] = b[i] }
		} else if nb == 0 { /* b is zero -- the result is a */
			c[0] = Double(Sign(na, y:Double(ia)))
			for i = 1; i<na; i++ { c[i] = a[i] }
		} else {
			if ia == ib { db = 1.0 } else { db = -1.0 }
			ixa = Int(a[1]); ixb = Int(b[1]); ish = ixa-ixb
			
			/* check if `a's exponent is greater than `b's */
			if ish >= 0 {
				/* `b' must be shifted to the right */
				m1 = Min(na, y: ish); m2 = Min(na, y: nb+ish); m3 = na
				m4 = Min(Max(na, y: ish), y: curMant+1)
				m5 = Min(Max(na, y: nb+ish), y: curMant+1)
				d.r[0] = 0; d.r[1] = 0
				for i = 1; i <= m1; i++ { d.r[i+1] = a[i+1] }
				for i = m1+1; i <= m2; i++ { d.r[i+1] = a[i+1]+db*b[i+1-ish] }
				for i = m2+1; i <= m3; i++ { d.r[i+1] = a[i+1] }
				for i = m3+1; i <= m4; i++ { d.r[i+1] = Real.ZERO }
				for i = m4+1; i <= m5; i++ { d.r[i+1] = db*b[i+1-ish] }
				nd = m5; ixd = ixa; d.r[nd+2] = Real.ZERO; d.r[nd+3] = Real.ZERO
			} else {
				/* `b' has greater exponent than `a', so `a' is shifted
				to the right. */
				nsh = -ish; m1 = Min(nb, y:nsh); m2 = Min(nb, y:na+nsh); m3 = nb
				m4 = Min(Max(nb, y:nsh), y:curMant+1)
				m5 = Min(Max(nb, y:na+nsh), y:curMant+1)
				d.r[0] = 0; d.r[1] = 0
				for i = 1; i <= m1; i++ { d.r[i+1] = db*b[i+1] }
				for i = m1+1; i <= m2; i++ { d.r[i+1] = a[i+1-nsh]+db*b[i+1] }
				for i = m2+1; i <= m3; i++ { d.r[i+1] = db*b[i+1] }
				for i = m3+1; i <= m4; i++ { d.r[i+1] = 0 }
				for i = m4+1; i <= m5; i++ { d.r[i+1] = a[i+1-nsh] }
				nd = m5; ixd = ixb; d.r[nd+2] = 0; d.r[nd+3] = 0
			}
			d.r[0] = Double(Sign(nd, y:Double(ia))); d.r[1] = Double(ixd)
			Normalize(d.r, a:&c)
		}
		if Real.debug >= 9 { println("Add 3"); Write(c) }
	} //Add;

	private func Sub (inout c: RealArray, a: RealArray, var b: RealArray) {
		if Real.err != 0 { Zero(&c); return }
		
		/* negate and perform addition */
		b[0] = -b[0]; Add(&c, a: a, b: b)
	} //Sub;

	private func Mul (inout c: RealArray, a: RealArray, b: RealArray) {
		/*
		This routine multiplies numbers `a' and `b' to yield the
		product `c'.  When one of the arguments has a much higher
		level of precision than the other, this routine is slightly
		more efficient if `a' has the lower level of precision.
		Debug output starts with debug = 8.
		
		This routine returns up to curMant mantissa words of the
		product.  if the complete double-long product of `a' and
		`b' is desired, { curMant must be at least as large as
		the sum of the mantissa lengths of `a' and `b'.  In other
		words, if the precision levels of `a' and `b' are both
		64 words, { curMant must be at least 128 words to
		obtain the complete double-long product in `c'.
		*/
		var ia, ib, na, nb, nc, i, j, i1, i2, n2, j3: Int
		var d2, t1, t2: Double
		var d: FixedLReal
		
		if Real.err != 0 { Zero(&c); return }
		if Real.debug >= 8 {
			println("Mul 1"); Write(a)
			println("Mul 2"); Write(b)
		}
		ia = Sign(Real.ONE, y: a[0]); ib = Sign(Real.ONE, y: b[0])
		na = Min(Int(abs(a[0])), y:curMant)
		nb = Min(Int(abs(b[0])), y:curMant)
		
		/* if one of the inputs is zero--result is zero */
		if (na == 0) || (nb == 0) { Zero(&c); return }
		
		/* check for multiplication by 1 */
		if (na == 1) && (a[2] == 1) {
			/* a is 1 or -1 -- result is b or -b */
			c[0] = Double(Sign(nb, y:Double(ia*ib))); c[1] = a[1]+b[1]
			for i = 2; i < nb; i++ { c[i] = b[i] }
			return
		} else if (nb == 1) && (b[2] == 1) {
			/* b is 1 or -1 -- result is a or -a */
			c[0] = Double(Sign(na, y:Double(ia*ib))); c[1] = a[1]+b[1]
			for i = 2; i < na; i++ { c[i] = a[i] }
			return
		}
		
		nc = Min(na+nb, y: curMant);
		d2 = a[1]+b[1];
		for i = 0; i <= nc+3; i++ { d.r[i] = 0 }
		
		/* perform ordinary long multiplication algorithm.
		Accumulate at most curMant+4 mantissa words of the
		product. */
		for j = 3; j<=na+2; j++ {
			t1 = a[j-1]; j3 = j-3;
			n2 = Min(nb+2, y:curMant+4-j3)
			for i = 2; i<n2; i++ {
				d.r[i+j3] = d.r[i+j3]+t1*b[i]
			}
			
			/* release carries periodically to avoid overflowing
			the exact integer capacity of double precision
			floating point words in d */
			if j-2 % Real.NPR == 0 {
				i1 = Max(3, y: j-Real.NPR); i2 = n2+j3
				for i = i1; i<=i2; i++ {
					t1 = d.r[i-1]; t2 = Double(Int(Real.invRadix*t1))
					d.r[i-1] = t1-Real.radix*t2
					d.r[i-2] = d.r[i-2]+t2
				}
			}
		};
		
		/* if d[1] is nonzero, shift the result one cell right */
		if d.r[1] != 0 {
			d2 = d2+1
			for i = nc+3; i >= 2; i-- { d.r[i] = d.r[i-1] }
		}
		d.r[0] = Double(Sign(nc, y:Double(ia*ib))); d.r[1] = d2
		
		/* fix up result since some words may be negative or
		exceed radix */
		Normalize(d.r, a: &c);
		if Real.debug >= 9 { println("Mul 3"); Write(c) }
	} //Mul;

	private func Muld (inout c: RealArray, a: RealArray, b: Double, n: Int) {
		/*
		This routine multiplies the multiple precision number `a'
		by the number `b'*2**`n' to produce the multiple precision
		result in `c'.
		*/
		var bb: Double;
		var d: FixedLReal;
		var ia, ib, n1, n2, i, na: Int;
		var f: RealArray;
		
		if Real.err != 0 { Zero(&c); return }
		if Real.debug >= 9 {
			println("Muld 1"); Write(a)
			println("Muld 2 \(b); n=\(n)")
		}
		
		/* check for zero inputs */
		ia = Sign(Real.ONE, y:a[0]); ib = Sign(Real.ONE, y:b)
		na = Min(Int(abs(a[0])), y:curMant)
		if (na == 0) || (b == 0) { Zero(&c); return }
		n1 = Int(n/Real.NBT); n2 = n-Real.NBT*n1; bb = abs(b)*ipower(2.0, base: n2)
		
		/* reduce bb to within 1 and radix */
		Reduce(&bb, exp: &n1)
		
		/* if `b' cannot be represented exactly in a single mantissa word use Mul */
		if bb != Double(Int(bb)) {
			if b < 0 { bb = -abs(b) } else { bb = abs(b) }
			NumbExpToReal(bb, n: n1*Int(Real.NBT), b: &f)  /* convert bb to f (Real) */
			Mul(&c, a: f, b: a)
		} else {
			/* perform short multiply */
			for i = 2; i<na; i++ { d.r[i] = bb*a[i] }
			
			/* set exponent and fix up the result */
			d.r[0] = Double(Sign(na, y: Double(ia*ib))); d.r[1] = a[1]+Double(n1)
			d.r[na+2] = 0; d.r[na+3] = 0
			Normalize(d.r, a: &c)
		}
		
		if Real.debug >= 9 {
			println("Muld 3"); Write(c)
		}
	} //Muld;

	private func Div (inout c: RealArray, a: RealArray, b: RealArray) {
		/*
		This routine divides the number `a' by the number `b' to yield
		the quotient `c'.  Debug output starts with debug = 8.
		*/
		var ia, ib, na, nb, nc, i3, i2, i, j, j3, md, is1, ij: Int
		var rb, ss, t0, t1, t2: Double
		var useOldj: Bool
		var d: FixedLReal
		
		/* handle errors */
		if Real.err != 0 { Zero(&c); return }
		if Real.debug >= 8 {
			println("Div 1"); Write(a)
			println("Div 2"); Write(b)
		}
		
		/* extract lengths and number signs */
		ia = Sign(Real.ONE, y:a[0]); ib = Sign(Real.ONE, y:b[0])
		na = Min(Int(abs(a[0])), y:curMant)
		nb = Min(Int(abs(b[0])), y:curMant)
		
		/* check if divid} //is zero */
		if na == 0 { Zero(&c); return };
		
		/* check for divisors of 1 or -1 */
		if (nb == 1) && (b[2] == 1) {
			c[0] = Double(Sign(na, y: Double(ia*ib)))
			c[1] = a[1]-b[1]
			for i = 2; i<=na+1; i++ { c[i] = a[i] }
			return
		}
		
		/* check if divisor is zero */
		if nb == 0 {
			println("*** Div: Divisor is zero!")
			Real.err = 31; return
		}
		
		/* initialize trial divisor and trial division */
		t0 = Real.radix*b[2]
		if nb>=2 { t0 = t0+b[3] }
		if nb>=3 { t0 = t0+Real.invRadix*b[4] }
		if nb>=4 { t0 = t0+Real.mprx2*b[5] }
		rb = 1.0/t0; md = Min(na+nb, y: curMant)
		d.r[0] = 0
		for i = 1; i<=na; i++ { d.r[i] = a[i+1] }
		for i = na+1; i<=md+3; i++ { d.r[i] = 0 }
		
		/* perform ordinary long division algorithm.  First
		compute only the first na words of the quotient. */
		for j = 2; j<=na+1; j++ {
			t1 = Real.mpbx2*d.r[j-2]+Real.radix*d.r[j-1]+d.r[j]+Real.invRadix*d.r[j+1]
			t0 = Double(Int(rb*t1)); j3 = j-3
			i2 = Min(nb, y:curMant+2-j3)+2
			ij = i2+j3
			for i = 3; i<=i2; i++ {
				i3 = i+j3-1; d.r[i3] = d.r[i3]-t0*b[i-1]
			}
			
			/* release carries periodically to avoid overflowing
			the exact integer capacity of double precision
			floating point words in d. */
			if j-1 % Real.NPR == 0 {
				for i = j; i<=ij-1; i++ {
					t1 = d.r[i]; t2 = Double(Int(Real.invRadix*t1))
					d.r[i] = t1-Real.radix*t2; d.r[i-1] = d.r[i-1]+t2
				}
			};
			d.r[j-1] = d.r[j-1]+Real.radix*d.r[j-2]
			d.r[j-2] = t0
		}
		
		/* compute additional words of the quotient, as long as
		the remainder is nonzero. */
		j = na+2; useOldj = false
		for ;; {
			if j>curMant+3 { break }
			t1 = Real.mpbx2*d.r[j-2] + Real.radix*d.r[j-1] + d.r[j]
			if j <= curMant+2 { t1 = t1+Real.invRadix*d.r[j+1] }
			t0 = Double(Int(rb*t1)); j3 = j-3
			i2 = Min(nb, y:curMant+2-j3)+2
			ij = i2+j3; ss = Real.ZERO
			
			for i = 3; i<=i2; i++ {
				i3 = i+j3-1; d.r[i3] = d.r[i3]-t0*b[i-1];
				ss = ss+abs(d.r[i3])
			};
			
			if j-1 % Real.NPR == 0 {
				for i = j; i<=ij-1; i++ {
					t1 = d.r[i]; t2 = Double(Int(Real.invRadix*t1))
					d.r[i] = t1-Real.radix*t2; d.r[i-1] = d.r[i-1]+t2
				}
			}
			d.r[j-1] = d.r[j-1]+Real.radix*d.r[j-2]
			d.r[j-2] = t0
			if ss == 0 { useOldj = true; break }
			if ij <= curMant { d.r[ij+2] = 0 }
			j++
		}
		
		/* set sign and exponent, and fix up result */
		if !useOldj { j = curMant+3 }
		d.r[j-1] = 0
		if d.r[0] == 0 { is1 = 1 } else { is1 = 2 }
		nc = Min(j-1, y:curMant);
		d.r[nc+2] = 0; d.r[nc+3] = 0
		for i = j; i>=2; i-- { d.r[i] = d.r[i-is1] }
		d.r[0] = Double(Sign(nc, y:Double(ia*ib)))
		d.r[1] = a[1]-b[1]+Double(is1-2)
		Normalize(d.r, a:&c)
		
		if Real.debug >= 8 {
			println("Div 3"); Write(c)
		};
	} //Div;

	private func Divd (inout c: RealArray, a: RealArray, b: Double, n: Int) {
		/*
		This routine divides the multiple precision number `a'
		by the number `b'*2**`n' to produce the multiple precision
		result in `c'.
		*/
		var t1, bb, br, dd: Double
		var d: FixedLReal
		var ia, ib, n1, n2, nc, na, j: Int
		var ok: Bool
		var f: RealArray
		
		if Real.err != 0 { Zero(&c); return }
		ia = Sign(Real.ONE, y:a[0]); ib = Sign(Real.ONE, y:b)
		na = Min(Int(abs(a[0])), y: curMant)
		
		/* check if divid} //is zero */
		if na == 0 { Zero(&c); return }
		
		/* check if divisor is zero */
		if b == 0 {
			println("*** Divd: Divisor is zero!")
			Real.err = 32; return
		}
		
		n1 = Int(n/Real.NBT); n2 = n-Real.NBT*n1; bb = abs(b)*ipower(2.0, base: n2)
		
		/* reduce bb to within 1 and radix */
		Reduce(&bb, exp:&n1)
		
		/* if `b' cannot be represented exactly in a single mantissa word { use Div */
		if bb != Double(Int(bb)) {
			if b < 0 { bb = -abs(b) } else { bb = abs(b) }
			NumbExpToReal(bb, n:n1*Real.NBT, b:&f)  /* convert bb to f (Real) */
			Div(&c, a: a, b: f)
		} else {
			/* perform short division */
			br = 1.0/bb; dd = a[2]
			j = 2; ok = true
			while ok & (j<=curMant+3) {
				t1 = Double(Int(br*dd)); d.r[j] = t1
				dd = Real.radix*(dd-t1*bb)
				if j <= na {
					dd = dd+a[j+1]
				} else if dd == 0 {
					ok = false
				}
				j++
			}
			
			/* set exponent and fix up the result */
			j--; nc = Min(j-1, y:curMant);
			d.r[0] = Double(Sign(nc, y:Double(ia*ib))); d.r[1] = a[1]-Double(n1)
			if j<=curMant+2 { d.r[j+1] = 0 }
			if j<=curMant+1 { d.r[j+2] = 0 }
			Normalize(d.r, a: &c)
		}
	} //Divd;

	private func Abs (inout z: RealArray, x: RealArray) {
		copy(x, b: &z); z[0] = abs(x[0])
	} //Abs;

	private func IntPower (inout b: RealArray, a: RealArray, n: Int) {
		/*
		This routine computes the `n'-th power of the ext}ed
		number `a' and returns the ext}ed result in `b'.
		When `n' is negative, the reciprocal of `a'**|`n'| is
		returned.  The Knuth's method is used p442, "The Art
		of Computer Programming", Vol 2.
		*/
		var na, nws, nn: Int
		var r, t: FixedLReal
		
		if Real.err != 0 { Zero(&b); return }
		na = Min(Int(abs(a[0])), y:curMant)
		
		/* check for errors */
		if na == 0 {
			if n >= 0 {
				Zero(&b)
			} else {
				println("*** ipower: Argument is zero and n is <= 0.")
				Real.err = 57
			}
			return
		}
		
		/* check for trival cases */
		Zero(&t.r)
		nws = curMant
		curMant++
		nn = abs(n)
		if nn == 0 {
			copy(xONE, b:&b)									/* x^0 = 1 */
			curMant = nws; return
		} else if nn == 1 { copy(a, b: &b)					/* x^1 = x */
		} else if nn == 2 { Mul(&t.r, a:a, b:a); copy(t.r, b: &b)    /* x^2 = x*x */
		} else {
			/* apply Knuth's algorithm */
			copy(xONE, b:&b);  /* b = 1 */
			copy(a, b:&r.r);     /* r = a */
			for ;; {
				if ODD(nn) { Mul(&t.r, a:b, b:r.r); copy(t.r, b:&b) }
				nn = nn / 2; if nn == 0 { break }
				Mul(&r.r, a:r.r, b:r.r)
			}
		}
		
		/* take reciprocal if n<0 */
		if n<0 { Div(&t.r, a:xONE, b:b); copy(t.r, b:&b) }
		
		/* restore original precision */
		curMant = nws; Round(&b)
	} //IntPower;

func Cmp (a, b: RealArray) -> Int {
/*
This routine compares the ext}ed numbers `a' and `b' and
returns the value -1, 0, or 1 dep}ing on whether `a'<`b',
`a'=`b', or `a'>`b'.  It is faster than merely subtracting
`a' and `b' and looking at the sign of the result.
*/
var
ia, ib, ma, mb, na, nb, i: Int;
 
ia = Sign(ONE, a[0]); if a[0]=ZERO { ia = 0 };
ib = Sign(ONE, b[0]); if b[0]=ZERO { ib = 0 };

/* compare signs */
if ia != ib { return Sign(ONE, ia-ib) };

/* signs are the same, compare exponents */
ma = Int(a[1]); mb = Int(b[1]);
if ma != mb { return ia*Sign(ONE, ma-mb) };

/* signs & exponents are the same, compare mantissas */
na = Min(Int(abs(a[0])), curMant); nb = Min(Int(abs(b[0])), curMant);
for i = 2 TO Min(na, nb)+1 {
if a[i] != b[i] { return ia*Sign(ONE, a[i]-b[i]) }
};

/* mantissas are the same to the common length, compare lengths */
if na != nb { return ia*Sign(ONE, na-nb) };

/* signs, exponents, mantissas, and legnths are the same so a=b */
return 0
} //Cmp;

func Sqrt (var b: RealArray; a: RealArray);
/*
Computes the square root of `a' and returns the result
in `b'.

This routine employs the following Newton-Raphson iteration, which
converges to 1 / Sqrt(a):

X(k+1) = X(k) + 0.5 * (1 - X(k)^2 * a) * X(k)

where the multiplication () * X(k) is performed with only half the
normal level of precision.  These iterations are performed with a
maximum precision level curMant that is dynamically changed,
doubling with each iteration.  The final iteration is performed
as follows (this is due to A. Karp):

Sqrt(a) = (a * X(n)) + 0.5 * (a - (a * X(n)^2) * X(n)

where the multiplications a * X(n) and ()* X(n) are performed
with only half of the final level of precision.
*/
var
t1, t2: Double;
k0, k1, k2: FixedReal;
iq: Bool;
ia, na, nws, n2, n, k, nw1, nw2, mq: Int;
 
if err != 0 { Zero(b); return };
ia = Sign(ONE, a[0]); na = Min(Int(abs(a[0])), curMant);

/* trivial values */
if na=0 { Zero(b); return };

/* error checking */
if ia<0 { Out.String("*** Sqrt: Argument is negative!");
Out.Ln; err = 70; return
};
nws = curMant;

/* determine the least integer mq such that 2^mq >= curMant */
t1 = curMant; mq = Int(invLn2*rm.ln(t1)+ONE-mprxx);

/* initial approximation of 1 / Sqrt(a) */
RealToNumbExp(a, t1, n);
n2 = -(n DIV 2); t2 = rm.sqrt(t1*ipower(2.0D0, SHORT(n+2*n2)));
t1 = ONE/t2;
NumbExpToReal(t1, n2, b);
curMant = 3; iq = false

/* perform the Newton_Raphson iteration described above with
a dynamically changing precision level curMant (one greater
than the powers of two). */
for k = 2 TO mq-1 {
nw1 = curMant; curMant = SHORT(Min(2*curMant-2, nws)+1);
nw2 = curMant;
for ;; {
Mul(k0, b, b);           /* k0 = X(k)^2 */
Mul(k1, a, k0);          /* k1 = a * X(k)^2 */
Sub(k0, xONE, k1);       /* k0 = 1 - a * X(k)^2 */
curMant = SHORT(nw1);
Mul(k1, b, k0);          /* k1 = X(k)*(1 - a * X(k)^2) */
Muld(k0, k1, HALF, 0);   /* k0 = 0.5 * (X(k)*(1 - a * X(k)^2)) */
curMant = SHORT(nw2);
Add(b, b, k0);           /* X(k+1) = X(k) + 0.5 * (X(k)*(1 - a * X(k)^2)) */
if ~iq & (k=mq-NIT) { iq = true
} else { break
}
}
};

/* last iteration using Karp's trick */
Mul(k0, a, b);               /* k0 = a * X(n) */
nw1 = curMant;
curMant = SHORT(Min(2*curMant-2, nws)+1);
nw2 = curMant;
Mul(k1, k0, k0);             /* k1 = (a * X(n))^2 */
Sub(k2, a, k1);              /* k2 = a - (a * X(n))^2 */
curMant = SHORT(nw1);
Mul(k1, k2, b);              /* k1 = X(n) * (a - (a * X(n))^2) */
Muld(k2, k1, HALF, 0);       /* k2 = 0.5 * (X(n) * (a - (a * X(n))^2)) */
curMant = SHORT(nw2);
Add(k1, k0, k2);             /* Sqrt(a) = a * X(n) + 0.5 * (X(n) * (a - (a * X(n))^2)) */
copy(k1, b);

/* restore original resolution */
curMant = SHORT(nws); Round(b)
} //Sqrt;

func Root (var b: RealArray; a: RealArray; n: Int);
/*
Computes the `n'th root of `a' and returns the result in `b'.
`n' must be at least one and must not exceed 2^30.

This routine employs the following Newton-Raphson iteration, which
converges to a ^ (-1/n):

X(k+1) = X(k) + (X(k)/n) * (1 - X(k)^n * a)

The reciprocal of the final approximation to a ^ (-1/n) is the
nth root.  These iterations are performed with a maximum precision
level curMant that is dynamically changed, approximately doubling
with each iteration.

When n is large and a is very near one, the following binomial
series is employed instead of the Newton scheme:

(1+x)^(1/n) = 1 + x/n + x^2*(1-n)/(2!*n^2) + ...
*/
let 
maxN = 40000000H;   /* 2^30 */
var
t1, t2, tn: Double;
k0, k1, k2, k3: FixedReal;
f2: Real8;
iq: Bool;
nws: Int;
ia, na, n2, k, mq, n1, n3: Int;
 
if err != 0 { Zero(b); return };
ia = Sign(ONE, a[0]); na = Min(Int(abs(a[0])), curMant);

/* trivial values */
if na=0 { Zero(b); return };

/* error checking */
if ia<0 { 
if ODD(n) { a[0] = -a[0] /* continue with abs(a) */
} else { Out.String("*** Root: Argument is negative!");
Out.Ln; err = 70; return
}
};
if (n<=0) || (n>maxN) { 
Out.String("*** Root: Improper value of n!"); Out.Ln;
err = 60; return
};
nws = curMant;

/* if n = 1 or 2 use faster local routines */
if n=1 { copy(a, b); b[0] = Sign(b[0], ia); return
ELSif n=2 { Sqrt(b, a); return
};

/* determine the least integer mq such that 2^mq >= curMant */
t1 = curMant; mq = Int(invLn2*rm.ln(t1)+ONE-mprxx);

/* check how close `a' is to 1 */
Sub(k0, a, xONE);
if k0[0]=ZERO { copy(xONE, b); return };
RealToNumbExp(k0, t1, n1);
n2 = Int(invLn2*rm.ln(abs(t1)));
t1 = t1*ipower(HALF, SHORT(n2));
INC(n1, n2);
if n1<=-30 { 
t2 = n; n2 = Int(invLn2*rm.ln(t2)+ONE+mprxx);
n3 = -NBT*curMant DIV n1;
if n3<Int(1.25*n2) { 
/* `a' is so close to 1 that it is cheaper to use the
binomial series */
INC(curMant);
Divd(k1, k0, t2, 0); Add(k2, xONE, k1);
k = 0;
for ;; {
INC(k); t1 = 1-k*n; t2 = (k+1)*n;
Muld(k2, k1, t1, 0);
Muld(k1, k3, t2, 0);
Mul(k3, k0, k1);
copy(k3, k1);
Add(k3, k1, k2);
copy(k3, k2);
if (k1[0]=ZERO) || (k1[1]<-curMant) { break }
};
copy(k2, b); Div(k0, xONE, k2);
curMant = nws; Round(b);
b[0] = Sign(b[0], ia);
return
}
};

/* compute the initial approximation of a^(-1/n) */
tn = n; RealToNumbExp(a, t1, n1);
n2 = -Int(n1/tn);
t2 = rm.exp(-ONE/tn*(rm.ln(t1)+(n1+tn*n2)*Ln2));
NumbExpToReal(t2, n2, b);
NumbExpToReal(tn, 0, f2);
curMant = 3; iq = false

/* perform the Newton_Raphson iteration described above
with a dynamically changing precision level curMant
which is one greater than the powers of two. */
for k = 2 TO mq {
curMant = SHORT(Min(2*curMant-2, nws)+1);
for ;; {
IntPower(k0, b, n);
Mul(k1, a, k0);
Sub(k0, xONE, k1);
Mul(k1, b, k0);
Divd(k0, k1, tn, 0);
Add(k1, b, k0);
copy(k1, b);
if ~iq & (k=mq-NIT) { iq = true
} else { break
}
}
};

/* take reciprocal to give final result */
Div(k1, xONE, b); copy(k1, b);

/* restore original resolution */
curMant = nws; Round(b);
b[0] = Sign(b[0], ia)
} //Root;

func Pi (var pi: RealArray);
/*
Computes Pi to available precision (curMant words).

The algorithm that is used, which is due to Salamin and
Brent, is as follows:

Set A(0) = 1, B(0) = 1/Sqrt(2), D(0) = Sqrt(2) - 1/2.

{ from k = 1 iterate the following operations:

A(k) = 0.5 * (A(k-1) + B(k-1))
B(k) = Sqrt (A(k-1) * B(k-1))
D(k) = D(k-1) - 2^k * (A(k) - B(k))^2

{ P(k) = (A(k) + B(k))^2 / D(k) converges quadratically to
Pi.  In other words, each iteration approximately doubles the
number of correct digits, providing all iterations are done
with the maximum precision.
*/
var
f: Real8;
An, Bn, Dn, t, r: FixedReal;
nws, mq: Int;
k: Int;
t1: Double;
 
if err != 0 { Zero(pi); return };

/* increase working resolution */
nws = curMant; INC(curMant);

/* determine the number of iterations required for the given
precision level.  This formula is good only for this Pi
algorithm. */
t1 = nws*rx.log(radix, 10);
mq = Int(invLn2*(rm.ln(t1)-ONE)+ONE);

/* initialize working variables */
copy(xONE, An);                       /* A(0) = 1 */
f[0] = ONE; f[1] = ZERO; f[2] = 2.0;
Sqrt(t, f);                           /* t = Sqrt(2) */
Muld(Bn, t, HALF, 0);                 /* B(0) = 1 / Sqrt(2) */
f[1] = -ONE; f[2] = HALF*radix;
Sub(Dn, t, f);                        /* D(0) = Sqrt(2) - 1/2 */

/* perform iterations as above */
for k = 1 TO SHORT(mq) {
Mul(t, An, Bn);                     /* t = A(k-1) * B(k-1) */
Add(r, An, Bn);                     /* r = A(k-1) + B(k-1) */
Sqrt(Bn, t);                        /* B(k) = Sqrt(A(k-1) * B(k-1)) */
Muld(An, r, HALF, 0);               /* A(k) = 0.5 * (A(k-1) + B(k-1)) */
Sub(t, An, Bn);                     /* t = A(k) - B(k) */
Mul(t, t, t);                       /* t = (A(k) - B(k))^2 */
t1 = ipower(2.0D0, k);               /* t1 = 2^k */
Muld(t, t, t1, 0);                  /* t = 2^k * (A(k) - B(k))^2 */
Sub(Dn, Dn, t);                     /* D(k) = D(k-1) -  2^k * (A(k) - B(k))^2 */
};

/* complete the computation */
Add(t, An, Bn);                       /* t = A(k) + B(k) */
Mul(t, t, t);                         /* t = (A(k) + B(k))^2 */
Div(pi, t, Dn);                       /* k2 = (A(k) + B(k))^2 / D(k) */

/* back to original precision */
curMant = SHORT(nws); Round(pi)
} //Pi;

func Entier (var b: RealArray; a: RealArray);
/*
Set `b' to the largest integer not greater than `a'.
For example: Int(3.6) = 3 and Int(-1.6)=-2
*/
var
ia, na, ma, nb, i: Int;
 
if err != 0 { Zero(b); return };
ia = Sign(ONE, a[0]); na = Min(Int(abs(a[0])), curMant);
ma = Int(a[1]);

/* check for zero -> result is zero */
if na=0 { Zero(b); return };

/* check if `a' can be represented exactly as an integer */
if ma>=curMant { 
Out.String("*** Entier: Argument is too large!"); Out.Ln;
};

/* place integer part of a in b */
nb = Min(Max(ma+1, 0), na);
if nb=0 { Zero(b); return
ELSE
b[0] = Sign(nb, ia); b[1] = ma;
b[nb+2] = ZERO; b[nb+3] = ZERO;
for i = 2 TO nb+1 { b[i] = a[i] }
};

/* if (a < 0) & (Frac(a) != 0) { b = b - 1 */
if ia=-1 { 
for i = nb+2 TO na+1 {
if a[i] != ZERO { Sub(b, b, xONE); return }
}
}
} //Entier;

func RoundInt (var b: RealArray; a: RealArray);
/*
Set `b' to the integer nearest to the multiple precision
number `a'.
*/
var
ia, na, ma, ic, nc, mc, nb, i: Int;
f: Real8;
k0: FixedReal;
 
if err != 0 { Zero(b); return };
ia = Sign(ONE, a[0]); na = Min(Int(abs(a[0])), curMant);
ma = Int(a[1]);

/* check for zero -> result is zero */
if na=0 { Zero(b); return };

/* check if `a' can be represented exactly as an integer */
if ma>=curMant { 
Out.String("*** RoundInt: Argument is too large!"); Out.Ln;
err = 56; return
};

/* add or subtract 1/2 from the input, dep}ing on its sign */
f[0] = ONE; f[1] = -ONE; f[2] = HALF*radix;
if ia=1 { Add(k0, a, f) } else { Sub(k0, a, f) };
ic = Sign(ONE, k0[0]); nc = Int(abs(k0[0])); mc = Int(k0[1]);

/* place integer part of k0 in b */
nb = Min(Max(mc+1, 0), nc);
if nb=0 { Zero(b)
ELSE
b[0] = Sign(nb, ic); b[1] = mc;
b[nb+2] = ZERO; b[nb+3] = ZERO;
for i = 2 TO nb+1 { b[i] = k0[i] }
}
} //RoundInt;

func Exp (var b: RealArray; a: RealArray);
/*
This computes the exponential function of the ext}ed
precision number `a' and returns the result in `b'.  The
value of `ln2' is also required.

This algorithm uses a modification of the Taylor's series
for Exp(t):

Exp(t) = (1 + r + r^2/2! + r^3/3! + r^4/4! ...) ^ q*2^n

where q = 256, r = t'/q, t' = t - n Ln(2) and where n is
chosen so that -0.5 Ln(2) < t' <= 0.5 Ln(2).  Reducing t
mod Ln(2) and dividing by 256 ensures that -0.001<r<=0.001
which accelerates convergence in the above series.
*/
let 
NQ=8;
var
t1, t2, tl: Double;
ia, na, nws, n1, nz, l1, i: Int;
k0, k1, k2, k3: FixedReal;
 
if err != 0 { Zero(b); return };
ia = Sign(ONE, a[0]); na = Min(Int(abs(a[0])), curMant);
RealToNumbExp(a, t1, n1);
t1 = t1*ipower(2, SHORT(n1));
Zero(k1);

/* unless the argument is near Ln(2), ln2 must be precomputed.
This exception is necessary because Ln calls Exp to
initialize ln2 */
if (abs(t1-Ln2)>invRadix) & (ln2=NIL) { 
Out.String("*** Exp: ln2 must be precomputed!"); Out.Ln;
err = 34; return
};

/* check for overflows and underflows */
if t1>=1.0D9 { 
if t1>ZERO { 
Out.String("*** Exp: Argument is too large "); Out.LongReal(t1, 0, 0);
Out.String(" x 10^"); Out.Int(n1, 0); Out.Ln; err = 35
} else { Zero(b)
};
return
};

/* increase resolution */
nws = curMant; INC(curMant);

/* compute the reduced argument a' = a - Ln(2) * Int(a/Ln(2)).
Save nz = Int(a/Ln(2)) for correcting the exponent of the
final result. */
if abs(t1-Ln2)>invRadix { 
Div(k0, a, ln2.real^);
RoundInt(k1, k0);
RealToNumbExp(k1, t1, n1);
nz = Int(t1*ipower(2, SHORT(n1))+Sign(mprxx, SHORT(t1)));
Mul(k2, ln2.real^, k1);
Sub(k0, a, k2)
} else { copy(a, k0); nz = 0
};
tl = k0[1]-curMant;

/* check if the reduced argument is zero */
if k0[0]=ZERO { copy(xONE, k0)
ELSE
/* divide the reduced argument by 2^nq */
Divd(k1, k0, ONE, NQ);

/* compute Exp using the usual Taylor series */
copy(xONE, k2); copy(xONE, k3); l1 = 0;
for ;; {
INC(l1);
if l1=10000 { 
Out.String("*** Exp: Iteration limit exceeded!"); Out.Ln;
err = 36; curMant = SHORT(nws); return
};
t2 = l1;
Mul(k0, k2, k1);
Divd(k2, k0, t2, 0);
Add(k0, k3, k2);
copy(k0, k3);

/* check for convergence of the series */
if (k2[0]=ZERO) || (k2[1]<tl) { break };
};

/* raise to the (2^nq)-th power */
for i = 1 TO NQ { Mul(k0, k0, k0) }
};

/* multiply by 2^nz */
Muld(k1, k0, ONE, nz);
copy(k1, b);

/* restore original precision level */
curMant = SHORT(nws); Round(b)
} //Exp;

func Ln (var b: RealArray; a: RealArray);
/*
This routine computes the natural logarithm of the ext}ed
precision number `a' and returns the ext}ed precision
result in `b'.  This routine uses a previously calculated
value `ln2'.  if `a' is close to 2 { `ln2' is not required
so this func can be used to compute `ln2'.

The Taylor series for Ln converges much more slowly than that
of Exp.  Thus this routine does not employ the Taylor series,
but instead computes logarithms by solving Exp(b) = a using the
following Newton iteration, which converges to `b':

X(k+1) = X(k) + (a - Exp(X(k)) / Exp(X(k))

These iterations are performed with a maximum precision level
curMant that is dynamically changed, approximately doubling
with each iteration.
*/
var
ia, na, n1, nws, mq, k: Int;
t1, t2: Double;
k0, k1, k2: FixedReal;
iq: Bool;
 
if err != 0 { Zero(b); return };

/* check for error inputs */
ia = Sign(ONE, a[0]); na = Min(Int(abs(a[0])), curMant);
if (ia<0) || (na=0) { Out.String("*** Ln: Argument is less than or equal to zero!");
Out.Ln; err = 50; return
};

/* unless the input is close to 2, ln2 must be known */
RealToNumbExp(a, t1, n1);
if ((abs(t1-2.0D0)>1.0D-3) || (n1 != 0)) & (ln2=NIL) { 
Out.String("*** Ln: Ln(2) must be precomputed!"); Out.Ln; err = 51; return
};

/* check if input is exactly one */
if (a[0]=ONE) & (a[1]=ZERO) & (a[2]=ONE) { Zero(b); return };

/* determine the least integer mq such that 2^mq >= curMant */
nws = curMant; t2 = nws; mq = Int(invLn2*rm.ln(t2)+ONE-mprxx);

/* compute initial approximation of Ln(a) */
t1 = rm.ln(t1)+n1*Ln2; NumbExpToReal(t1, 0, b);
curMant = 3; iq = false
for k = 2 TO mq {
curMant = SHORT(Min(2*curMant-2, nws)+1);
for ;; {
Exp(k0, b);        /* k0 = Exp(X(k)) */
Sub(k1, a, k0);    /* k1 = a - Exp(X(k)) */
Div(k2, k1, k0);   /* k2 = (a - Exp(X(k))) / Exp(X(k)) */
Add(k1, b, k2);    /* k1 = X(k) + (a - Exp(X(k))) / Exp(X(k)) */
copy(k1, b);
if (k=mq-NIT) & ~iq { iq = true
} else { break
}
}
};

/* restore original precision */
curMant = SHORT(nws); Round(b)
} //Ln;

func SinCos (var sin, cos: RealArray; a: RealArray);
/*
This routine computes the cosine and sine of the ext}ed
precision number `a' and returns the results in `cos' and
`sin', respectively.  The units of `a' are in radians.

The calculations are performed using the conventional Taylor's
series for Sin(s):

Sin(s) = s - s^3/3! + s^5/5! - s^7/7! ....

where s = t - a * pi/2 - b*pi/16 and the integers a and b are
chosen to minimize the absolute value of s.  We can { compute

Sin (t) = Sin (s + a*pi/2 + b*pi/16)
Cos (t) = Cos (s + a*pi/2 + b*pi/16)

by applying elementary trig identities for sums.  The sine and
cosine of b*pi/16 are of the form 1/2*Sqrt(2 +- Sqrt(2 +- Sqrt(2))).
Reducing t in this manner ensures that -Pi/32 < s < Pi/32 which
accelerates convergence in the above series.
*/
var
t1, t2: Double;
nws: Int;
f: Real8;
ia, na, ka, kb, n1, kc, l1: Int;
k0, k1, k2, k3, k4, k5, k6: FixedReal;
 
if err != 0 { Zero(sin); Zero(cos); return };

ia = Sign(ONE, a[0]); na = Min(Int(abs(a[0])), curMant);

/* check for trivial case when a = 0 */
if na=0 { copy(xONE, cos); Zero(sin); return };

/* check if pi has been precomputed */
if pi=NIL { Out.String("*** SinCos: pi must be precomputed!");
Out.Ln; err = 28; return
};

/* increase resolution */
nws = curMant; INC(curMant);

/* reduce input to between -pi and pi */
Muld(k0, pi.real^, 2.0D0, 0);
Div(k1, a, k0);
RoundInt(k2, k1);       /* k2 = a DIV 2pi */
Sub(k3, k1, k2);        /* k3 = a MOD 2pi */

/* determine the nearest multiple of pi/2, and within a
quadrant, the nearest multiple of pi/16.  Through most
of the rest of this procedure, ka and kb are the integers
a and b of the above algorithm. */
RealToNumbExp(k3, t1, n1);
if n1>=-NBT { 
t1 = t1*ipower(2, SHORT(n1)); t2 = 4*t1;
if t2<ZERO { ka = -Int(HALF-t2) } else { ka = Int(t2+HALF) };  /* ka = rm.round(t2) */
t1 = 8*(t2-ka);
if t1<ZERO { kb = -Int(HALF-t1) } else { kb = Int(t1+HALF) };  /* kb = rm.round(8*(t2-ka)) */
/* ka = Int(t2); kb = rm.round(8*(t2-ka)) */
} else { ka = 0; kb = 0
};
t1 = (8*ka+kb)/32;
NumbExpToReal(t1, 0, k1);
Sub(k2, k3, k1);
Mul(k1, k0, k2);

/* compute cosine and sine of the reduced arguments using
Taylor's series */
if k1[0]=ZERO { Zero(k0)
ELSE
copy(k1, k0); Mul(k2, k0, k0); l1 = 0;
for ;; {
INC(l1);
if l1=10000 { 
Out.String("*** SinCos: Iteration limit exceeded!"); Out.Ln;
err = 29; curMant = nws; return
};
t2 = -(2.0D0*l1)*(2.0D0*l1+ONE);
Mul(k3, k2, k1);
Divd(k1, k3, t2, 0);
Add(k3, k1, k0);
copy(k3, k0);

/* check for convergence of the series */
if (k1[0]=ZERO) || (k1[1]<k0[1]-curMant) { break }
}
};

/* compute Cos(s) = Sqrt(1-Sin(s)^2) */
copy(k0, k1);
Mul(k2, k0, k0); Sub(k3, xONE, k2); Sqrt(k0, k3);

/* compute cosine and sine of b*Pi/16 */
kc = abs(kb); f[0] = ONE; f[1] = ZERO; f[2] = 2.0;
if kc=0 { copy(xONE, k2); Zero(k3)
ELSE
CASE kc OF
| 1: Sqrt(k4, f); Add(k5, f, k4); Sqrt(k4, k5)
| 2: Sqrt(k4, f)
| 3: Sqrt(k4, f); Sub(k5, f, k4); Sqrt(k4, k5)
| 4: Zero(k4)
| } else { /* { nothing */
};
Add(k5, f, k4); Sqrt(k3, k5); Muld(k2, k3, HALF, 0);
Sub(k5, f, k4); Sqrt(k4, k5); Muld(k3, k4, HALF, 0)
};

/* apply the trigonometric summation identities to compute
cosine and sine of s + b*Pi/16 */
if kb<0 { k3[0] = -k3[0] };
Mul(k4, k0, k2); Mul(k5, k1, k3); Sub(k6, k4, k5);
Mul(k4, k1, k2); Mul(k5, k0, k3); Add(k1, k4, k5);
copy(k6, k0);

/* this code applies the trig summation identities for
(s + b*pi/16 + a*pi/2 */
CASE ka OF
|     0: copy(k0, cos); copy(k1, sin)
|     1: copy(k1, cos); cos[0] = -cos[0]; copy(k0, sin)
|    -1: copy(k1, cos); copy(k0, sin); sin[0] = -sin[0]
| 2, -2: copy(k0, cos); cos[0] = -cos[0]; copy(k1, sin); sin[0] = -sin[0]
| } else { /* { nothing */
};

/* restore the orginal precision level */
curMant = nws; Round(cos); Round(sin)
} //SinCos;

func SinhCosh (var sinh, cosh: RealArray; a: RealArray);
/*
This routine computes the hyperbolic cosine and sine of the
number `a' and returns the two results in `cosh' and `sinh',
respectively.  The last word of the result is not reliable.
*/
var
k0, k1, k2: FixedReal;
nws: Int;
 
nws = curMant; INC(curMant);
Exp(k0, a); Div(k1, xONE, k0);         /* k1 = exp(-a); k0 = exp(a) */
Add(k2, k0, k1);                       /* k2 = exp(a) + exp(-a) */
Muld(k2, k2, HALF, 0); copy(k2, cosh); /* cosh = (exp(a) + exp(-a))/2 */
Sub(k2, k0, k1);                       /* k2 = exp(a) - exp(-a) */
Muld(k2, k2, HALF, 0); copy(k2, sinh); /* sinh = (exp(a) - exp(-a))/2 */

/* restore orginal precision */
curMant = nws; Round(cosh); Round(sinh)
} //SinhCosh;

func ATan2 (var a: RealArray; x, y: RealArray);
/*
This routine computes the angle `a' subt}ed by the
pair (`x', `y') considered as a point in the x-y plane.
This routine places the resultant angle correctly in the
full circle where -pi < `a' <= pi.  The last word of the
result is not reliable.

The Taylor series for Arcsin converges much more slowly than
that of Sin.  Thus this routine does not employ a Taylor
series, but instead computes Arccos or Arcsin by solving
Cos(a) = x or Sin(a) = y using one of the following Newton
iterations, both of which converge to `a':

z(k+1) = z(k) - (x - Cos(z(k))) / Sin(z(k))
z(k+1) = z(k) - (y - Sin(z(k))) / Cos(z(k))

The first is selected if abs(x) <= abs(y); otherwise the
second is used.  These iterations are performed with a
maximum precision level curMant that is dynamically changed,
approximately doubling with each iteration.
*/
let 
NIT=3;
var
t1, t2, t3: Double;
iq: Bool;
nws: Int;
ix, iy, nx, ny, mq, n1, n2, kk, k: Int;
k0, k1, k2, k3, k4: FixedReal;
 
if err != 0 { Zero(a); return };
ix = Sign(ONE, x[0]); nx = Min(Int(abs(x[0])), curMant);
iy = Sign(ONE, y[0]); ny = Min(Int(abs(y[0])), curMant);

/* check if both x and y are zero */
if (nx=0) & (ny=0) { 
Out.String("*** ATan2: Both arguments are zero!"); Out.Ln;
err = 7; return
};

/* check if pi has been precomputed */
if pi=NIL { 
Out.String("*** ATan2: Pi must be precomputed!"); Out.Ln;
err = 8; return
};

/* check if one of x or y is zero */
if nx=0 { 
if iy>0 { Muld(a, pi.real^, HALF, 0)
} else { Muld(a, pi.real^, -HALF, 0)
};
return
ELSif ny=0 { 
if ix>0 { Zero(a) } else { copy(pi.real^, a) };
return
};

/* increase the resolution */
nws = curMant; INC(curMant);

/* determine the least integer mq such that 2^mq >= curMant */
mq = Int(invLn2*rm.ln(nws)+ONE-mprxx);

/* normalize x and y so that x^2 + y^2 = 1 */
Mul(k0, x, x); Mul(k1, y, y); Add(k2, k0, k1); Sqrt(k3, k2);
Div(k1, x, k3); Div(k2, y, k3);

/* compute initial approximation of the angle */
RealToNumbExp(k1, t1, n1); RealToNumbExp(k2, t2, n2);
n1 = Max(n1, -66); n2 = Max(n2, -66);
t1 = t1*ipower(2, SHORT(n1)); t2 = t2*ipower(2, SHORT(n2));
t3 = rm.arctan2(t2, t1);
NumbExpToReal(t3, 0, a);

/* the smaller of x or y will be used from now on to measure convergence.
This selects the Newton iteration (of the two listed above) that has
the largest denominator */
if abs(t1)<=abs(t2) { kk = 1; copy(k1, k0)
} else { kk = 2; copy(k2, k0)
};

/* perform the Newton-Raphson iteration described above with a dynamically
changing precision level curMant (one greater than powers of two). */
curMant = 3; iq = false
for k = 2 TO mq {
curMant = SHORT(Min(2*curMant-2, nws)+1);
for ;; {
SinCos(k2, k1, a);
if kk=1 { Sub(k3, k0, k1); Div(k4, k3, k2); Sub(k1, a, k4)
} else { Sub(k3, k0, k2); Div(k4, k3, k1); Add(k1, a, k4)
};
copy(k1, a);
if ~iq & (k=mq-NIT) { iq = true } else { break }
}
};

/* restore the original precision */
curMant = nws; Round(a)
} //ATan2;

/*---------------------------------------------------------*/
/*                                                         */
/* The following routines are intended for external users  */
/*                                                         */
/*---------------------------------------------------------*/
/* Constructors                                            */

func New (size: Int) -> Real;
var
x: Real;
 
NEW(x); NEW(x.real, size);
return x
} //New;

func Long * (x: Double) -> Real;
var
r: Real;
 
/* create a new number */
r = New(curMant+4);
NumbExpToReal(x, 0, r.real^);
return r
} //Long;

func (a: Real) Copy * () -> Real;
/**
return a copy of `a'.
*/
var
b: Real;
 
b = New(curMant+4);
copy(a.real^, b.real^);  /* b = a */
return b
} //Copy;

func ToReal (str: String) -> Real {
	/**
	Converts the number in `str' to an ext}ed Real and
	returns the result.  The number representation is
	given by:
	
	number = ["+"|"-"] @{digit@} ["." @{digit@}] [scale]
	
	where  scale = "E"|"D" ["+"|"-"] digit @{digit@}
	and    digit = "0".."9" | " "
	
	Thus the following is a valid input number:
	
	"1.23456 12938 23456 E + 200"
	
	Note: This real number definition is backwardly
	compatible with the Oberon-2 real string but has
	been ext}ed to allow splitting of very large
	numbers into more readable segments by inserting
	spaces.
	*/
	var b: Real
	var s: FixedReal;
	var nexp, es, is, cc, dig, nws, dp: Int;
	var isZero: Bool;
	var f: Real8;
	
	func SkipBlanks {
		
		while str[cc]=" " { INC(cc) }
	} //SkipBlanks;
	
	func GetDigit () -> Int {
		/* skips blanks to get a digit; returns
		-1 on an invalid digit */
		var ch: CHAR;
		
		SkipBlanks; ch = str[cc];
		if (ch>="0") & (ch<="9") {
			INC(cc);
			if ch>"0" { isZero = FALSE };
			return ORD(ch)-ORD("0")
		} else { return -1
		}
	} //GetDigit;
	
	func GetSign () -> Int {
		
		SkipBlanks; /* skip leading blanks */
		
		/* check for leading sign */
		if str[cc]="+" { INC(cc); return 1
			ELSif str[cc]="-" { INC(cc); return -1
			} else { return 1
			}
		} //GetSign;
		
		
		cc = 0; nws = curMant; INC(curMant);
		
		/* check for initial sign */
		is = GetSign();
		
		/* clear result */
		Zero(s); f[0] = ONE; f[1] = ZERO;
		
		/* scan for digits, stop on a non-digit */
		isZero = true;
		for ;; {
			dig = GetDigit();
			if dig<0 { break };
			if ~isZero { Muld(s, s, 10.0, 0) };
			if dig != 0 { f[2] = dig; Add(s, f, s) }
		};
		
		/* check for decimal point */
		dp = 0;
		if str[cc] == "." {
			INC(cc);
			for ;; {
				dig = GetDigit();
				if dig<0 { break };
				Muld(s, s, 10.0, 0);
				if dig != 0 { f[0] = ONE; f[2] = dig
				} else { f[0] = ZERO
				};
				INC(dp); Add(s, f, s)
			}
		};
		
		/* check for exponent */
		nexp = 0;
		if (str[cc] == "E") || (str[cc] == "D") {
			INC(cc);
			es = GetSign();
			for ;; {
				dig = GetDigit();
				if dig<0 { break };
				nexp = nexp*10+dig
			};
			nexp = es*nexp  /* add the sign */
		};
		
		/* scale the resultant number */
		s[0] = s[0]*is; DEC(nexp, dp);
		f[0] = ONE; f[2] = 10.0; b = New(curMant+4);
		IntPower(b.real^, f, nexp); Mul(s, b.real^, s); copy(s, b.real^);
		
		/* back to original resolution */
		curMant = SHORT(nws); Round(b.real^);
		return b
	} //ToReal;


/*---------------------------------------------------------*/
/* Conversion routines                                     */

func (a: Real) Format * (Decimal, ExpWidth : Int; EngFormat: Bool) -> STRING;
/** return the ext}ed real number as a string with 'Decimal' decimal places and
an exponent with 'ExpWidth' places.  'ExpWidth' <> 0 produces a scientific
formatted number or, if 'EngFormat' is true, an engineering formatted
number. 'ExpWidth' = 0 produces a floating-point number string. A fixed-point
number is output when 'Decimal' > 0. */
let 
log2=0.301029995663981195D0;
var
pos, ManIndex, StrCnt, InCnt, Aexp, MaxExpWidth : Int;
nws, na, ia, nx, nl, l: Int;
aa, t1: Double;
ExpStr : ARRAY 41 OF CHAR;
FixPoint: Bool;
f: Real8;
k: FixedReal;
Str: POINTER TO ARRAY OF CHAR;

func ConcatChar(ch : CHAR);
 
Str[pos] = ch; INC(pos);
} //ConcatChar;

func AddDigit (d: Int);
 
ConcatChar(CHR(d+ORD("0")))
} //AddDigit;

func AddInt (n: Int);
 
if n<10 { AddDigit(n)
} else { AddInt(n DIV 10); AddDigit(n MOD 10)
}
} //AddInt;

func GetDigit();
var
nn: Int;
 
if k[0]=ZERO { AddDigit(0); return };
if k[1]=ZERO { nn = Int(k[2]); f[0] = 1; f[2] = nn
} else { f[0] = 0; nn = 0
};
AddDigit(nn); Sub(k, k, f); Muld(k, k, 10.0, 0)
} //GetDigit;

func Round();
let 
ZC = ORD('0');
var
l, c, i: Int;
 
/* BCD-based rounding algorithm */
l = pos-1; c = 5;
while l>=0 {
if (Str[l] != '.') & (Str[l] != '-') { 
c = ORD(Str[l])-ZC+c;
Str[l] = CHR(c MOD 10 + ZC);
c = c DIV 10
};
DEC(l)
};
if c>0 { 
/* insert a character at pos 0 */
for i = l+1 TO 1 BY -1 {
Str[i] = Str[i-1]
};
Str[0] = '1';
INC(pos); INC(nx)
};
DEC(pos) /* ignore rounding digit */
} //Round;

func Trim();
 
/* check if trailing zeros should be trimmed */
l = pos-1;
if (Str[l]="0") & ~FixPoint { 
while (Str[l]="0") { DEC(l) }
};

/* remove trailing `.' */
if Str[l] = "." { DEC(l) };
pos = l+1
} //Trim;

 
/* initialize a few parameters */
pos  =  0;
StrCnt  =  3;
ManIndex  =  0;
ExpStr  =  '';
NEW(Str, sigDigs+Decimal+ExpWidth+32);
ia = Sign(ONE, a.real[0]); na = Min(Int(abs(a.real[0])), curMant);
nws = curMant; INC(curMant); Zero(k);

/* round the number */
f[0] = ONE; f[1] = ZERO; f[2] = 10; nl = 0;

/* determine the exact power of ten for exponent */
if na != 0 { 
aa = a.real[2];
if na>=2 { aa = aa+invRadix*a.real[3] };
if na>=3 { aa = aa+mprx2*a.real[4] };
if na>=4 { aa = aa+invRadix*mprx2*a.real[5] };
t1 = log2*NBT*a.real[1]+rx.log(aa, 10);
if t1>=ZERO { nx = Int(t1) } else { nx = Int(t1-ONE) };
IntPower(k, f, nx);  /* k = 10**nx */
Div(k, a.real^, k);  /* k = a*10**nx */

/* adjust k if above is not quite right */
while k[1]<0 { DEC(nx); Muld(k, k, 10.0, 0) };
while k[2]>=10.0 { INC(nx); Divd(k, k, 10.0, 0) };
k[0] = abs(k[0])
} else { nx = 0; Zero(k)
};

/* force scientific notation for numbers too small or too large */
Aexp = abs(nx);
MaxExpWidth  =  ExpWidth;
if ((ExpWidth = 0) & (Aexp > sigDigs)) || (ExpWidth > 0) { 
/* force scientific notation */
if Aexp > 999999 { ExpWidth  =  7
ELSif Aexp > 99999 { ExpWidth  =  6
ELSif Aexp > 9999 { ExpWidth  =  5
ELSif Aexp > 999 { ExpWidth  =  4
ELSif Aexp > 99 { ExpWidth  =  3
ELSif Aexp > 9 { ExpWidth  =  2
} else { ExpWidth  =  1
};
};
if MaxExpWidth < ExpWidth { MaxExpWidth  =  ExpWidth };

/* add the negative sign to the number */
if ia<0 { ConcatChar('-') };

/* ensure we don't exceed the maximum digits */
FixPoint  =  Decimal  !=  0;
if (Decimal > sigDigs) || ~FixPoint { 
Decimal = sigDigs-1;
};

/* convert the number into scientific notation */
if MaxExpWidth > 0 { 
GetDigit();                         /* leading digit */
if EngFormat { 
while Aexp MOD 3  !=  0 {
if nx<0 { INC(Aexp); INC(nx)
} else { DEC(Aexp); DEC(nx)
};
DEC(Decimal);
GetDigit()                      /* next digits */
}
};
ConcatChar('.');                    /* decimal point */
for InCnt  =  1 TO Decimal {
GetDigit()                        /* add following digits */
};
Round(); DEC(pos); Trim();          /* remove extraneous digits */

/* add the exponent */
ConcatChar('E');
if nx >= 0 { ConcatChar('+')
} else { ConcatChar('-')
};
AddInt(abs(nx));
ConcatChar(0X);
ELSE
/* format a non-scientific number */
if nx < 0 { 
ConcatChar('0');                  /* leading digit */
ConcatChar('.');                  /* decimal point */
for InCnt  =  2 TO abs(nx) {      /* pad with leading zeros */
ConcatChar('0'); INC(nl)
};
INC(Decimal, nx+1);
};
InCnt  =  0;
REPEAT
GetDigit();
if InCnt > nx { 
DEC(Decimal)
ELSif InCnt = nx { 
ConcatChar('.');
};
INC(InCnt);
UNTIL (InCnt = sigDigs) || (Decimal = 0);

/* remove any trailing zeros and unneeded digits */
Round();
Trim();
ConcatChar(0X);

/* remove trailing `.' */
if Str[InCnt] = "." { Str[InCnt]  =  0X }
};
curMant = SHORT(nws);
return Object.NewLatin1(Str^)
} //Format;


func (x: Real) Neg * (): Real;
var b: Real;
 
b = New(curMant+4);
copy(x.real^, b.real^);  /* b = x */
b.real[0] = -b.real[0];
return b
} //Neg;

func (x: Real) Sign * (): Int;
 
if x.real[0] < 0.0 { return -1
ELSif x.real[0] > 0.0 { return 1
} else { return 0
}
} //Sign;

func (x: Real) ToString*(): STRING;
 
return x.Format(0, 0, FALSE)
} //ToString;

func (q: Real) Short * () -> Double;
/** returns the closest Double equivalent.  if q is too large
MAX(Double) is returned and if q is too small, zero
is returned. */
var
x: Double; exp: Int;
 
RealToNumbExp(q.real^, x, exp);
return x*ipower(2, SHORT(exp));
} //Short;

func (q: Real) Entier * () -> Real;
/**
return the largest integer not greater than `q'.
For example: Int(3.6) = 3 and Int(-1.6)=-2
*/
var
r: Real;
 
r = New(curMant+4);
Int(r.real^, q.real^);
return r
} //Entier;

func (q: Real) Fraction * () -> Real;
/**
return the fractional part of 'q'.
*/
var
r: Real;
 
r = q.Int();
if q.real[0]<ZERO { Add(r.real^, q.real^, xONE) };
Sub(r.real^, q.real^, r.real^);
return r
} //Fraction;

/*---------------------------------------------------------*/
/* Basic math routines                                     */

func (z1: Real) Add * (z2: Real): Real;
var
r: Real;
 
r = New(curMant+4);
Add(r.real^, z1.real^, z2.real^);
return r
} //Add;

func Add2 * (z1, z2: Real): Real;
 
return z1.Add(z2)
} //Add2;

func (z1: Real) Sub * (z2: Real): Real;
var
r: Real;
 
r = New(curMant+4);
Sub(r.real^, z1.real^, z2.real^);
return r
} //Sub;

func Sub2 * (z1, z2: Real): Real;
 
return z1.Sub(z2)
} //Sub2;

func (z1: Real) Mul * (z2: Real): Real;
var
r: Real;
 
r = New(curMant+4);
Mul(r.real^, z1.real^, z2.real^);
return r
} //Mul;

func Mul2 * (z1, z2: Real): Real;
 
return z1.Mul(z2)
} //Mul2;

func (z1: Real) Div * (z2: Real): Real;
var
r: Real;
 
r = New(curMant+4);
Div(r.real^, z1.real^, z2.real^);
return r
} //Div;

func (z: Real) Abs * () -> Real;
/** returns the absolute value of z */
var
r: Real;
 
r = New(curMant+4);
abs(r.real^, z.real^);
return r
} //Abs;

func (a: Real) Cmp * (b: Object.Object) -> Int {
/**
This routine compares the ext}ed numbers `a' and `b' and
returns the value -1, 0, or 1 dep}ing on whether `a'<`b',
`a'=`b', or `a'>`b'.  It is faster than merely subtracting
`a' and `b' and looking at the sign of the result.
*/
 
WITH b: Real {
return Cmp(a.real^, b.real^)
}
} //Cmp;

func (a: Real) Store*(w: Storable.Writer) RAISES IO.Error;
/** Write 'a' to the 'w' writer. */
var
i, len: Int;
 
len = LEN(a.real^);
w.WriteNum(len);
for i  =  0 TO len-1 {
w.WriteReal(a.real[i]);
}
} //Store;

func (a: Real) Load*(r: Storable.Reader) RAISES IO.Error;
/** Read 'a' from the 'r' reader. */
var
i, len: Int;
 
r.ReadNum(len); NEW(a.real, len);
for i  =  0 TO len-1 {
r.ReadReal(a.real[i]);
}
} //Load;

/*---------------------------------------------------------*/
/* Power and transc}ental routines                       */

func (x: Real) Power * (exp: Real): Real;
/** returns the value of the number x raised to the power exp */
var
r: Real;
n: Int;
 
r = New(curMant+4);

/* check for integer powers */
Int(r.real^, exp.real^);
n = Int(r.Short());
if (r.Cmp(exp)=0) & (abs(n)<2000) { 
IntPower(r.real^, x.real^, n)
} else { /* x^exp = Exp(exp*Ln(x)) */
Ln(r.real^, x.real^); Mul(r.real^, exp.real^, r.real^); Exp(r.real^, r.real^)
};
return r
} //Power;

func (z: Real) IRoot * (n: Int): Real;
/** returns the `n'th root of `z' */
var
r: Real;
 
r = New(curMant+4);
Root(r.real^, z.real^, n);
return r
} //IRoot;

func (z: Real) Sqrt * () -> Real;
/** Pre: z>=0. returns the square root of z */
var
r: Real;
 
r = New(curMant+4);
Sqrt(r.real^, z.real^);
return r
} //Sqrt;

func (z: Real) Exp * () -> Real;
/** returns the exponential of z */
var
r: Real;
 
r = New(curMant+4);
Exp(r.real^, z.real^);
return r
} //Exp;

func (z: Real) Ln * () -> Real;
/** returns the natural logarithm of z */
var
r: Real;
 
r = New(curMant+4);
Ln(r.real^, z.real^);
return r
} //Ln;

func (z: Real) Log * (base: Real): Real;
/** returns the 'base' logarithm of z */
var
r: Real; t: FixedReal;
 
r = New(curMant+4);
Ln(r.real^, z.real^); Ln(t, base.real^); Div(r.real^, r.real^, t);
return r
} //Log;

func Factorial * (n: Int) -> Real;
/** returns the factorial of 'n'. */
let 
MAXFACT = 388006;  /* Limit of 1.50339063E+1999997 */
var
f: Real;
min: Int;
 
if (n<0) || (n>MAXFACT) { 
status = IllegalArgument; return zero  /* out of range */
};
f = New(curMant+4);
if n<2 { return one                /* 0! & 1! */
ELSif n>=300000 { 
copy(fact300000.real^, f.real^); min = 300000
ELSif n>=200000 { 
copy(fact200000.real^, f.real^); min = 200000
ELSif n>=100000 { 
copy(fact100000.real^, f.real^); min = 100000
ELSE
copy(one.real^, f.real^); min = 1;
};
while n>min {
Muld(f.real^, f.real^, n, 0);       /* f=f*x */
DEC(n)                              /* x=x-1 */
};
return f
} //Factorial;

func (z: Real) Sin * () -> Real;
/** returns the sine of z */
var
s: Real; c: FixedReal;
 
s = New(curMant+4); Zero(c);
SinCos(s.real^, c, z.real^);
return s
} //Sin;

func (z: Real) Cos * (): Real;
/** returns the cosine of z */
var
s: FixedReal; c: Real;
 
c = New(curMant+4); Zero(s);
SinCos(s, c.real^, z.real^);
return c
} //Cos;

func (z: Real) SinCos * (var sin, cos: Real);
/** returns the sine & cosine of z */
 
sin = New(curMant+4); cos = New(curMant+4);
SinCos(sin.real^, cos.real^, z.real^)
} //SinCos;

func (z: Real) Tan * (): Real;
/** returns the tangent of z */
var
s, c: FixedReal; r: Real;
 
r = New(curMant+4); Zero(s); Zero(c);
SinCos(s, c, z.real^); Div(r.real^, s, c);
return r
} //Tan;

func (z: Real) Arcsin * (): Real;
/** returns the arcsine of z */
var
t: FixedReal; r: Real;
 
r = New(curMant+4);
abs(t, z.real^);
if Cmp(t, xONE)>0 { 
Out.String("*** Illegal arcsin argument!"); Out.Ln; err = 20
ELSE
Mul(t, t, t); Sub(t, xONE, t); Sqrt(t, t);  /* t = Sqrt(1 - z^2) */
ATan2(r.real^, t, z.real^)                  /* r = ATan(z/Sqrt(1-z^2)) */
};
return r
} //Arcsin;

func (z: Real) Arccos * (): Real;
/** returns the arccosine of z */
var
t: FixedReal; r: Real;
 
r = New(curMant+4);
abs(t, z.real^);
if Cmp(t, xONE)>0 { 
Out.String("*** Illegal arccos argument!"); Out.Ln; err = 21
ELSE
Mul(t, t, t); Sub(t, xONE, t); Sqrt(t, t);  /* t = Sqrt(1 - z^2) */
ATan2(r.real^, z.real^, t)                  /* r = ATan(Sqrt(1-z^2)/z) */
};
return r
} //Arccos;

func (z: Real) Arctan * (): Real;
/** returns the arctangent of z */
var
r: Real;
 
r = New(curMant+4);
ATan2(r.real^, xONE, z.real^);
return r
} //Arctan;

func (xn: Real) Arctan2 * (xd: Real): Real;
/** returns the arctangent of xn/xd */
var
r: Real;
 
r = New(curMant+4);
ATan2(r.real^, xd.real^, xn.real^);
return r
} //Arctan2;

func (z: Real) SinhCosh * (var sinh, cosh: Real);
/** returns the hyberbolic sine & cosine of z */
 
sinh = New(curMant+4); cosh = New(curMant+4);
SinhCosh(sinh.real^, cosh.real^, z.real^)
} //SinhCosh;

func (z: Real) Sinh * (): Real;
/** returns the hyperbolic sine of z */
var
s: Real;
c: FixedReal;
 
s = New(curMant+4); SinhCosh(s.real^, c, z.real^);
return s
} //Sinh;

func (z: Real) Cosh * (): Real;
/** returns the hyperbolic cosine of z */
var
c: Real;
s: FixedReal;
 
c = New(curMant+4); SinhCosh(s, c.real^, z.real^);
return c
} //Cosh;

func (z: Real) Tanh * (): Real;
/** returns the hyperbolic tangent of z */
var
sinh, cosh: FixedReal; r: Real;
 
r = New(curMant+4);
SinhCosh(sinh, cosh, z.real^); Div(r.real^, sinh, cosh);
return r
} //Tanh;

func Random * () -> Real;
/** return a random number between 0 and 1 */
var res, t: Real;
 
res = Seed.Add(pi);
t = res.Ln(); t = t.Mul(Long(5));
t = t.Exp(); res = t; t = t.Int();
Seed = res.Sub(t);
return Seed
} //Random;

func OutReal (n: Real);
 
Out.Object(n.ToString())
} //OutReal;

func Test;
var
s, n, m: Real;
 
Out.String("zero="); OutReal(zero); Out.Ln;
Out.String("one="); OutReal(one); Out.Ln;
Out.String("pi="); OutReal(pi); Out.Ln;
Out.String("ln2="); OutReal(ln2); Out.Ln;
Out.String("ln10="); OutReal(ln10); Out.Ln;
Out.String("eps="); OutReal(eps); Out.Ln;
Out.String("log10(eps)="); OutReal(eps.Log(Long(10))); Out.Ln;
n = ToReal("123456789012345678901234567890123456789");
m = ToReal("0.123456789012345678901234567890123456790");
CASE n.Cmp(m) OF
| 0: Out.String("n=m")
| 1: Out.String("n>m")
| } else { Out.String("n<m")
};
Out.Ln;
Out.String("n="); OutReal(n); Out.Ln;
Out.String("m="); OutReal(m); Out.Ln;
s = n.Mul(m);
Out.String("n*m="); OutReal(s); Out.Ln;
s = n.Add(m);
Out.String("n+m="); OutReal(s); Out.Ln;
s = n.Sub(m);
Out.String("n-m="); OutReal(s); Out.Ln;
s = n.Div(m);
Out.String("n/m="); OutReal(s); Out.Ln;
n = Long(1);
s = n.Div(Long(3));
Out.String("1/3="); OutReal(s); Out.Ln;
Out.String("1/3+1/3="); OutReal(s.Add(s)); Out.Ln;
Out.String("1/3*1/3="); OutReal(s.Mul(s)); Out.Ln;
Out.String("1/3*3="); OutReal(s.Mul(Long(3))); Out.Ln;
n = Long(2.0);
s = n.Power(Long(64));
Out.String("2^64="); OutReal(s); Out.Ln;
n = ToReal("1.010E-10");
Out.String("1.010E-10="); OutReal(n); Out.Ln;
n = ToReal("-12.0E+10");
Out.String("-12.0E+10="); OutReal(n); Out.Ln;
n = ToReal("0.00045E-10");
Out.String("0.00045E-10="); OutReal(n); Out.Ln;
n = ToReal("-12 345 678");
Out.String("-12 345 678="); OutReal(n); Out.Ln;
n = ToReal("1E10000");
Out.String("1E10000="); OutReal(n); Out.Ln;
pi.SinCos(m, n);
Out.String("Sin(pi)="); OutReal(m); Out.Ln;
Out.String("Cos(pi)="); OutReal(n); Out.Ln;
m = pi.Div(Long(8));
m.SinCos(m, n);
Out.String("Sin(pi/8)="); OutReal(m); Out.Ln;
Out.String("Cos(pi/8)="); OutReal(n); Out.Ln;
m = Long(1);
m.SinCos(m, n);
Out.String("Sin(1)="); OutReal(m); Out.Ln;
Out.String("Cos(1)="); OutReal(n); Out.Ln;
m = Long(-8);
Out.String("-8^(-1/3)="); OutReal(m.IRoot(3)); Out.Ln;
m = Long(2); m = m.Power(Long(64));
Out.String("(2^64)^(-1/64)="); OutReal(m.IRoot(64)); Out.Ln;
m = Long(4);
Out.String("4*arctan(1)="); OutReal(m.Mul(one.Arctan())); Out.Ln;
m = one.Sin();
Out.String("arcsin(sin(1))="); OutReal(m.Arcsin()); Out.Ln;
m = one.Cos();
Out.String("arccos(cos(1))="); OutReal(m.Arccos()); Out.Ln;
m = Long(3.6);
Out.String("Int(3.6)="); OutReal(m.Int()); Out.Ln;
m = Long(-3.6);
Out.String("Int(-3.6)="); OutReal(m.Int()); Out.Ln;
} //Test;

func SetDigits * (digits: Int);
/** Sets the number of active words in all Real computations.
One word contains about 7.22 digits. */
var
words: Double;
 
words = digits/digsPerWord;
if words<8 { words = 8 };
if words<=maxMant-2 { 
curMant = Int(words)+2;
sigDigs = digits
}
} //SetDigits;


func Init;
TYPE
LongFixed = ARRAY 2*(maxMant+4) OF REAL;
var
t0, t1, t2, t3, t4: LongFixed;
 
/* internal constants */
xONE[0] = ONE; xONE[1] = ZERO; xONE[2] = ONE;  /* 1.0 */
Zero(t2); Zero(t3); Zero(t4);

/* initialize internal constants */
err = 0; curMant = maxMant+1; debug = 0; numBits = 22;

/* compute required constants to correct precision */
Zero(t1); Pi(t1);            /* t1 = pi */
NumbExpToReal(2, 0, t0);     /* t0 = 2.0 */
ln2 = NIL; Ln(t2, t0);        /* t2 = Ln(2.0) */
ln2 = New(LEN(t0)); copy(t2, ln2.real^);
NumbExpToReal(10, 0, t0);    /* t0 = 10.0 */
Ln(t3, t0);                  /* t3 = Ln(10.0) */
IntPower(t4, t0, log10eps);  /* t4 = 10^(10-maxDigits) */

/* transfer to current variables */
curMant = maxMant;
pi = New(curMant+4); copy(t1, pi.real^);
ln2 = New(curMant+4); copy(t2, ln2.real^);
ln10 = New(curMant+4); copy(t3, ln10.real^);
eps = New(curMant+4); copy(t4, eps.real^);
one = New(curMant+4); copy(xONE, one.real^);
zero = New(curMant+4); Zero(zero.real^);

/* Random number generator */
Seed = Long(4);



/* set the current output precision */
sigDigs = maxDigits
} //Init;


} //Reals.

