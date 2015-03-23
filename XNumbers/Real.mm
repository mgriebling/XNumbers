//
//  Real.m
//  TestReals
//
//  Created by Mike Griebling on 14 Mar 2015.
//  Copyright (c) 2015 Computer Inspirations. All rights reserved.
//

#import <Foundation/Foundation.h>

#import "Real.h"
#import "Reals.h"
#import "ExComplex.h"
#import "ExIntegers.h"

@implementation xNumber {
	Complex::Complex z;
	BOOL rational;
}

// NSCoder methods -- required

#define RATIONALKEY	@"xNumber.rational"
#define REALKEY	@"xNumber.z.r"
#define IMAGKEY	@"xNumber.z.i"

- (id) initWithCoder:(NSCoder *)aDecoder {
	// No nice way to convert to C++ types so I use strings
	NSString * sr = [aDecoder decodeObjectForKey:REALKEY];
	NSString * si = [aDecoder decodeObjectForKey:IMAGKEY];
	Real::Real r = Real::Real(sr.UTF8String);
	Real::Real i = Real::Real(si.UTF8String);
	z = Complex(r, i);
	rational = [aDecoder decodeBoolForKey:RATIONALKEY];
	return self;
}

- (void) encodeWithCoder:(NSCoder *)aCoder {
	// No nice way to convert from C++ types so I use strings
	char str[1024];
	Real::ToString(z.RealPart(), str, 0, 0, 0);
	[aCoder encodeObject:[NSString stringWithUTF8String:str] forKey:REALKEY];
	Real::ToString(z.ImagPart(), str, 0, 0, 0);
	[aCoder encodeObject:[NSString stringWithUTF8String:str] forKey:IMAGKEY];
	[aCoder encodeBool:rational forKey:RATIONALKEY];
}

- (Real::Real) x {
	return z.RealPart();
}

- (void) setX: (Real::Real) r {
	z = Complex(r);
}

+ (void)setDigits:(NSInteger)digits {
	Real::SetDigits(digits);
}

+ (NSInteger)digits {
	return Real::sigDigs;
}

+ (NSInteger)err {
	return Real::err;
}

+ (void)setErr:(NSInteger)error {
	Real::err = (errCodes)error;
}

+ (void)setAngularMeasure:(NSInteger)measure {
	Complex::SetAngle(msAngle(measure));
}

static BOOL isZero (Real::Real r) {
	return Real::sign(r) == 0;
}

- (id) initWithString: (NSString *)real andString: (NSString *)imag withRational: (BOOL)isRational andBase: (NSInteger)base {
	self = [super init];
	if (self) {
		Real::Real xi, yi;
		rational = isRational;
		if (rational) {
			rational = YES;
			Integer::StrToInt(real.UTF8String, base, xi);
			Integer::StrToInt(imag.UTF8String, base, yi);
		} else {
			xi = Real::Real(real.UTF8String);
			yi = Real::Real(imag.UTF8String);
		}
		z = Complex(xi, yi);
	}
	return self;
}

- (id) initWithString: (NSString *)value {
	BOOL isDouble = ([value rangeOfString:@"."].location != NSNotFound) || ([value rangeOfString:@"E"].location != NSNotFound);
	if (isDouble) {
		return [self initWithString:value andString:@"" withRational:NO andBase:10];
	}
	return [self initWithString:value andString:@"1" withRational:YES andBase:10];
}

- (id) initWithString: (NSString *)value andBase: (NSInteger)base {
	if (base == 10) {
		return [self initWithString:value];  // check if it could be rational
	}
	return [self initWithString:value andString:@"1" withRational:YES andBase:base];
}

- (id) initWithDouble: (double)value {
	return [self initWithRawReal:Real::Real(value)];
}

- (id) initWithDouble: (double)real andImaginary:(double)imaginary {
	return [self initWithRawReal:Real::Real(real) andRawImaginary:Real::Real(imaginary)];
}

- (id) initWithRawReal: (Real::Real)value {
	return [self initWithRawComplex:Complex(value, Real::zero)];
}

- (id) initWithInt: (NSInteger)value {
	return [self initWithIntNumerator:value andDenominator:1];
}

- (id) initWithRawNumerator: (Real::Real)numerator andRawDenominator:(Real::Real)denominator {
	self = [super init];
	if (self) {
		rational = YES;
		z = Complex(Real::entier(numerator), Real::entier(denominator));
	}
	return self;
}

- (id) initWithMagnitude: (xNumber *)magnitude andAngle: (xNumber *)angle {
	Complex::Complex zi;
	Complex::ToRectangular(z, magnitude.x, angle.x);
	return [self initWithRawComplex:zi];
}

- (id) initWithNumerator: (xNumber *)numerator andDenominator:(xNumber *)denominator {
	return [self initWithRawNumerator:numerator.x andRawDenominator:denominator.x];
}

- (id) initWithIntNumerator: (NSInteger)numerator andDenominator:(NSInteger)denominator {
	return [self initWithNumerator:[xNumber realWithDouble:numerator] andDenominator:[xNumber realWithDouble:denominator]];
}

- (id) initWithRawReal: (Real::Real)real andRawImaginary: (Real::Real)imaginary {
	return [self initWithRawComplex:Complex(real, imaginary)];
}

- (id) initWithRawComplex : (Complex::Complex)complex {
	self = [super init];
	if (self) {
		rational = NO;
		z = complex;
	}
	return self;
}

- (id) initWithReal: (xNumber *)real {
	return [self initWithRawComplex:real->z];
}

- (id) initWithReal: (xNumber *)real andImaginary:(xNumber *)imaginary {
	return [self initWithRawReal:real.x andRawImaginary:imaginary.x];
}

+ (xNumber *) realWithString: (NSString *)value {
	return [[xNumber alloc] initWithString:value];
}

+ (xNumber *) realWithDouble: (double)value {
	return [[xNumber alloc] initWithDouble:value];
}

+ (xNumber *) realWithDouble: (double)real andImaginary:(double)imaginary {
	return [[xNumber alloc] initWithDouble:real andImaginary:imaginary];
}

+ (xNumber *) realWithInt: (NSInteger)value {
	return [[xNumber alloc] initWithInt:value];
}

+ (xNumber *) realWithReal:(xNumber *)real {
	return [[xNumber alloc] initWithReal:real];
}

+ (xNumber *) realWithReal: (xNumber *)real andImaginary:(xNumber *)imaginary {
	return [[xNumber alloc] initWithRawReal:real.x andRawImaginary:imaginary.x];
}

+ (xNumber *) realWithString:(NSString *)value andBase:(NSInteger)base {
	return [[xNumber alloc] initWithString:value andBase:base];
}

+ (xNumber *) realWithRawReal:(Real::Real)real {
	return [[xNumber alloc] initWithRawReal:real];
}

+ (xNumber *) realWithMagnitude: (xNumber *)magnitude andAngle: (xNumber *)angle {
	return [[xNumber alloc] initWithMagnitude:magnitude andAngle:angle];
}

+ (xNumber *) realWithNumerator: (xNumber *)numerator andDenominator:(xNumber *)denominator {
	return [[xNumber alloc] initWithNumerator:numerator andDenominator:denominator];
}

+ (xNumber *) realWithRawNumerator: (Real::Real)numerator andRawDenominator:(Real::Real)denominator {
	return [[xNumber alloc] initWithRawNumerator:numerator andRawDenominator:denominator];
}

+ (xNumber *) realWithIntNumerator: (NSInteger)numerator andDenominator:(NSInteger)denominator {
	return [[xNumber alloc] initWithIntNumerator:numerator andDenominator:denominator];
}

+ (xNumber *) realWithRawComplex: (Complex::Complex)complex {
	return [[xNumber alloc] initWithRawComplex:complex];
}

+ (xNumber *) realWithString: (NSString *)real andString: (NSString *)imag withRational: (BOOL)isRational andBase: (NSInteger)base {
	return [[xNumber alloc] initWithString:real andString:imag withRational:isRational andBase:base];
}

// number attributes and conversions

static NSString * toString (Real::Real const &xi) {
	char str[1024];
	Real::ToString(xi, str, 0, 0, 0);
	return [NSString stringWithCString:str encoding:NSASCIIStringEncoding];
}

- (NSString *)description {
	return self.toString;
}

- (NSString *) toString {
	if (rational) {
		Real::Real n = self->z.RealPart();
		Real::Real d = self->z.ImagPart();
		NSString *first = toString(n);
		NSString *second = toString(d);

		if ([second isEqualToString:@"1"]) {
			return first;
		} else if (Real::cmp(n, d) > 0) {
			// format 4/3 like 1 1/3
			Real::Real integral;
			Real::Real remainder;
			Integer::IntDiv(integral, n, d);
			Integer::Mod(remainder, n, d);
			NSString *whole = toString(integral);
			first = toString(remainder);
			return [NSString stringWithFormat:@"%@ %@/%@", whole, first, second];
		}
		return [NSString stringWithFormat:@"%@/%@", first, second];
	} else {
		char str[1024];
		Complex::ComplexToStr(self->z, str);
		return [NSString stringWithCString:str encoding:NSASCIIStringEncoding];
	}
}

- (NSInteger)exponent {
	return FixRational(self).RealPart().exponent();
}

- (double) Short {
	return Real::Short(FixRational(self).RealPart());
}

- (xNumber *) entier {
	return [xNumber realWithRawReal:Real::entier(FixRational(self).RealPart())];
}

- (xNumber *) fraction {
	return [xNumber realWithRawReal:Real::fraction(FixRational(self).RealPart())];
}

- (xNumber *) real {
	return [xNumber realWithRawReal:z.RealPart()];
}

- (xNumber *) imaginary {
	return [xNumber realWithRawReal:z.ImagPart()];
}

- (xNumber *) numerator {
	if (rational) {
		return [xNumber realWithRawReal:z.RealPart()];
	}
	return xNumber.zerof;
}

- (xNumber *) denominator {
	if (rational) {
		return [xNumber realWithRawReal:z.ImagPart()];
	}
	return xNumber.onef;
}


- (BOOL) isRational {
	return rational;
}

- (BOOL) isComplex {
	return Real::sign(z.ImagPart()) != 0;
}

- (BOOL) isInteger {
	if (rational) {
		return Real::cmp(Real::one, z.ImagPart()) == 0;  // integer iff denom = 1
	}
	return !self.isComplex && Real::sign(Real::fraction(z.RealPart())) == 0;
}

static void Normalize (Real::Real &n, Real::Real &d) {
	Real::Real z;
	
	/* normalize signs */
	if (Real::sign(n) != Real::sign(d)) {
		if (Real::sign(d) < 0) {
			Real::abs(d); Real::negate(n);
		} else if (Real::sign(d) < 0) {
			Real::abs(d); Real::abs(n);
		}
	}
	
	/* reduce fraction to smallest rational denominator */
	Integer::GCD(z, n, d);
	n = Real::div(n, z); d = Real::div(d, z);
}

static xNumber* InitRational (Real::Real n, Real::Real d) {
	Normalize(n, d);
	return [xNumber realWithRawNumerator:n andRawDenominator:d];
}

static Complex FixRational (xNumber *n) {
	if (n->rational) {
		// fix up rational number to real
		return Complex::Complex(Real::div(n->z.RealPart(), n->z.ImagPart()));
	}
	return n->z;
}

static xNumber * addRational (Complex::Complex &a, Complex::Complex &b) {
	Real::Real z;
	Real::Real anum = a.RealPart();
	Real::Real aden = a.ImagPart();
	Real::Real bnum = b.RealPart();
	Real::Real bden = b.ImagPart();
	z = Real::mul(anum, bden);
	return InitRational(Real::add(z, Real::mul(bnum, aden)), Real::mul(aden, bden));
}

static xNumber * subRational (Complex::Complex &a, Complex::Complex &b) {
	Real::Real z;
	Real::Real anum = a.RealPart();
	Real::Real aden = a.ImagPart();
	Real::Real bnum = b.RealPart();
	Real::Real bden = b.ImagPart();
	z = Real::mul(anum, bden);
	return InitRational(Real::sub(z, Real::mul(bnum, aden)), Real::mul(aden, bden));
}

static xNumber * mulRational (Complex::Complex &a, Complex::Complex &b) {
	Real::Real anum = a.RealPart();
	Real::Real aden = a.ImagPart();
	Real::Real bnum = b.RealPart();
	Real::Real bden = b.ImagPart();
	return InitRational(Real::mul(anum, bnum), Real::mul(aden, bden));
}

static xNumber * divRational (Complex::Complex &a, Complex::Complex &b) {
	Real::Real anum = a.RealPart();
	Real::Real aden = a.ImagPart();
	Real::Real bnum = b.RealPart();
	Real::Real bden = b.ImagPart();
	return InitRational(Real::mul(anum, bden), Real::mul(aden, bnum));
}

static long cmpRational (Complex::Complex &a, Complex::Complex &b) {
	Real::Real ar = Real::div(a.RealPart(), a.ImagPart());
	Real::Real br = Real::div(b.RealPart(), b.ImagPart());
	return Real::cmp(ar, br);
}

static xNumber * powerRational (Complex::Complex a, long b) {
	Real::Real bc;
	Complex::Complex n;
	Complex::Complex d ;
	Complex::xtoi(n, a.RealPart(), b);
	Complex::xtoi(d, a.ImagPart(), b);
	return [xNumber realWithRawNumerator:n.RealPart() andRawDenominator:d.RealPart()];
}

- (xNumber *) add: (xNumber *) r {
	if (self->rational && r->rational) {
		return addRational(self->z, r->z);
	} else {
		Complex::Complex zi;
		Complex::Add(zi, FixRational(self), FixRational(r));
		return [xNumber realWithRawComplex:zi];
	}
}

- (xNumber *) sub: (xNumber *) r {
	if (self->rational && r->rational) {
		return subRational(self->z, r->z);
	} else {
		Complex::Complex zi;
		Complex::Sub(zi, FixRational(self), FixRational(r));
		return [xNumber realWithRawComplex:zi];
	}
}

- (xNumber *) mul: (xNumber *) r {
	if (self->rational && r->rational) {
		return mulRational(self->z, r->z);
	} else {
		Complex::Complex zi;
		Complex::Mult(zi, FixRational(self), FixRational(r));
		return [xNumber realWithRawComplex:zi];
	}
}

- (xNumber *) div: (xNumber *) r {
	if (self->rational && r->rational) {
		return divRational(self->z, r->z);
	} else {
		Complex::Complex zi;
		Complex::Div(zi, FixRational(self), FixRational(r));
		return [xNumber realWithRawComplex:zi];
	}
}

- (xNumber *) abs {
	if (self->rational) {
		return [xNumber realWithRawNumerator:Real::abs(self->z.RealPart()) andRawDenominator:self->z.ImagPart()];
	}
	return self.polarMag;
}

- (NSInteger) sign {
	if ([self isComplex]) return 1;
	if (isZero(self.x)) return 0;
	return Real::sign(self.x);
}

- (NSInteger) cmp: (xNumber *) r {
	if (self->rational && r->rational) {
		return cmpRational(self->z, r->z);
	} else {
		Complex az = FixRational(self);
		Complex bz = FixRational(r);
		Real a = [self isComplex] ? [self abs].x : self.x;
		Real b = [self isComplex] ? [r abs].x : r.x;
		return Real::cmp(a, b);
	}
}

- (xNumber *) round {
	if (self->rational) {
		return self;		// no rounding needed
	}
	Real::Real r = self->z.RealPart();
	Real::Real i = self->z.ImagPart();
	Real::round(r); Real::round(i);
	return [xNumber realWithRawComplex:Complex::Complex(r, i)];
}


/* Power and transcendental routines */

- (xNumber *) power: (xNumber *) exp {
	Complex::Complex zi;
	Complex::power(zi, FixRational(self), FixRational(exp));
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) sqrt {
	Complex::Complex zi;
	Complex::sqrt(zi, FixRational(self));
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) exp {
	Complex::Complex zi;
	Complex::exp(zi, FixRational(self));
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) ln {
	Complex::Complex zi;
	Complex::ln(zi, FixRational(self));
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) sin {
	Complex::Complex zi;
	Complex::sin(zi, FixRational(self));
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) cos {
	Complex::Complex zi;
	Complex::cos(zi, FixRational(self));
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) tan {
	Complex::Complex zi;
	Complex::tan(zi, FixRational(self));
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) arcsin {
	Complex::Complex zi;
	Complex::arcsin(zi, FixRational(self));
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) arccos {
	Complex::Complex zi;
	Complex::arccos(zi, FixRational(self));
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) arctan {
	Complex::Complex zi;
	Complex::arctan(zi, FixRational(self));
	return [xNumber realWithRawComplex:zi];;
}

- (xNumber *) sinh {
	Complex::Complex zi;
	Complex::sinh(zi, FixRational(self));
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) cosh {
	Complex::Complex zi;
	Complex::sinh(zi, FixRational(self));
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) tanh {
	Complex::Complex zi;
	Complex::tanh(zi, FixRational(self));
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) conj {
	Complex::Complex zi = FixRational(self);
	Complex::Conj(zi);
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) polarMag {
	Real::Real xi;
	Complex::PolarMag(xi, FixRational(self));
	return [xNumber realWithRawReal:xi];
}

- (xNumber *) polarAngle {
	Real::Real xi;
	Complex::PolarAngle(xi, FixRational(self));
	return [xNumber realWithRawReal:xi];
}

- (xNumber *) negate {
	Complex::Complex zi = self->z;
	if (self->rational) {
		return [xNumber realWithRawNumerator:Real::negate(zi.RealPart()) andRawDenominator:zi.ImagPart()];
	}
	zi = FixRational(self);
	Complex::ChgSign(zi);
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) ipower: (NSInteger) i {
	Complex::Complex zi;
	if (self->rational) {
		return powerRational(self->z, i);
	}
	Complex::xtoi(zi, FixRational(self), i);
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) iroot: (NSInteger) n {
	Complex::Complex zi;
	Complex::Root(zi, FixRational(self), n);
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) root: (xNumber *) x {
	Complex::Complex zi;
	Complex::root(zi, FixRational(self), x->z);
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) log10 {
	Complex::Complex zi;
	Complex::log(zi, FixRational(self));
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) log2 {
	Complex::Complex zi;
	Complex::log2(zi, FixRational(self));
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) cot {
	Complex::Complex zi;
	Complex::cot(zi, FixRational(self));
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) arccot {
	Complex::Complex zi;
	Complex::arccot(zi, FixRational(self));
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) coth {
	Complex::Complex zi;
	Complex::coth(zi, FixRational(self));
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) arcsinh {
	Complex::Complex zi;
	Complex::arcsinh(zi, FixRational(self));
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) arccosh {
	Complex::Complex zi;
	Complex::arccosh(zi, FixRational(self));
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) arctanh {
	Complex::Complex zi;
	Complex::arctanh(zi, FixRational(self));
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) arccoth {
	Complex::Complex zi;
	Complex::arccoth(zi, FixRational(self));
	return [xNumber realWithRawComplex:zi];
}



/* Integer routines */
- (xNumber *) setBit: (NSInteger) bit {
	Real::Real t;
	Integer::SetBit(t, FixRational(self).RealPart(), bit);
	return [xNumber realWithRawReal:t];
}

- (xNumber *) clearBit: (NSInteger) bit {
	Real::Real t;
	Integer::ClearBit(t, FixRational(self).RealPart(), bit);
	return [xNumber realWithRawReal:t];
}

- (xNumber *) toggleBit: (NSInteger) bit {
	Real::Real t;
	Integer::ToggleBit(t, FixRational(self).RealPart(), bit);
	return [xNumber realWithRawReal:t];
}

- (xNumber *) and: (xNumber *) i {
	Real::Real t;
	Integer::And(t, FixRational(self).RealPart(), FixRational(i).RealPart());
	return [xNumber realWithRawReal:t];
}

- (xNumber *) nand: (xNumber *) i {
	Real::Real t;
	Integer::Nand(t, FixRational(self).RealPart(), FixRational(i).RealPart());
	return [xNumber realWithRawReal:t];
}

- (xNumber *) or: (xNumber *) i {
	Real::Real t;
	Integer::Or(t, FixRational(self).RealPart(), FixRational(i).RealPart());
	return [xNumber realWithRawReal:t];
}

- (xNumber *) nor: (xNumber *) i {
	Real::Real t;
	Integer::Nor(t, FixRational(self).RealPart(), FixRational(i).RealPart());
	return [xNumber realWithRawReal:t];
}

- (xNumber *) xor: (xNumber *) i {
	Real::Real t;
	Integer::Xor(t, FixRational(self).RealPart(), FixRational(i).RealPart());
	return [xNumber realWithRawReal:t];
}

- (xNumber *) count {
	Real::Real t;
	Integer::Count(t, FixRational(self).RealPart());
	return [xNumber realWithRawReal:t];
}

- (xNumber *) intDiv: (xNumber *) i {
	Real::Real t;
	Integer::IntDiv(t, FixRational(self).RealPart(), FixRational(i).RealPart());
	return [xNumber realWithRawReal:t];
}

- (xNumber *) mod: (xNumber *) i {
	Real::Real t;
	Integer::Mod(t, FixRational(self).RealPart(), FixRational(i).RealPart());
	return [xNumber realWithRawReal:t];
}

- (xNumber *) onesComp {
	Real::Real t;
	Integer::OnesComp(t, FixRational(self).RealPart());
	return [xNumber realWithRawReal:t];
}

- (xNumber *) shl: (NSInteger) bit {
	Real::Real t;
	Integer::Shl(t, FixRational(self).RealPart(), bit);
	return [xNumber realWithRawReal:t];
}

- (xNumber *) rol: (NSInteger) bit {
	Real::Real t;
	Integer::Rol(t, FixRational(self).RealPart(), bit);
	return [xNumber realWithRawReal:t];
}

- (xNumber *) shr: (NSInteger) bit {
	Real::Real t;
	Integer::Shr(t, FixRational(self).RealPart(), bit);
	return [xNumber realWithRawReal:t];
}

- (xNumber *) ashr: (NSInteger) bit {
	Real::Real t;
	Integer::Ashr(t, FixRational(self).RealPart(), bit);
	return [xNumber realWithRawReal:t];
}

- (xNumber *) ror: (NSInteger) bit {
	Real::Real t;
	Integer::Ror(t, FixRational(self).RealPart(), bit);
	return [xNumber realWithRawReal:t];
}

- (xNumber *) fib {
	Real::Real t;
	Integer::Fib(t, FixRational(self).RealPart());
	return [xNumber realWithRawReal:t];
}

- (xNumber *) gcd: (xNumber *) i {
	Real::Real t;
	Integer::GCD(t, FixRational(self).RealPart(), FixRational(i).RealPart());
	return [xNumber realWithRawReal:t];
}

+ (xNumber *) strToInt: (NSString *)str withBase: (NSInteger)base {
	Real::Real t;
	Integer::StrToInt(str.UTF8String, base, t);
	return [xNumber realWithRawReal:t];
}

- (NSString *) intToStr: (NSInteger)base {
	char result[1024];
	Integer::IntToStr(FixRational(self).RealPart(), base, result);
	return [NSString stringWithCString:result encoding:NSASCIIStringEncoding];
}


/* Misc. routines */
- (xNumber *) factorial {
	return [xNumber realWithRawReal:Real::factorial(FixRational(self).RealPart())];
}

- (xNumber *) permutations: (xNumber *) r {
	return [xNumber realWithRawReal:Real::permutations(FixRational(self).RealPart(), FixRational(r).RealPart())];
}

- (xNumber *) combinations: (xNumber *) r {
	return [xNumber realWithRawReal:Real::combinations(FixRational(self).RealPart(), FixRational(r).RealPart())];
}

+ (xNumber *) random {
	return [xNumber realWithRawReal:Real::random()];
}


+ (void)initialize {
	Complex::InitComplex();		// initialize the Integer, Real, & Complex C++ classes
}

+ (xNumber *)zerof {
	static xNumber *zerov;
	if (zerov == NULL) {
		zerov = [xNumber realWithDouble:0];
	}
	return zerov;
}

+ (xNumber *)onef {
	static xNumber *onev;
	if (onev == NULL) {
		onev = [xNumber realWithDouble:1];
	}
	return onev;
}

+ (xNumber *)pif {
	static xNumber *piv;
	if (piv == NULL) {
		piv = [xNumber realWithRawReal:Real::pi];
	}
	return piv;
}

+ (xNumber *)epsf {
	static xNumber *epsv;
	if (epsv == NULL) {
		epsv = [xNumber realWithRawReal:Real::eps];
	}
	return epsv;
}

+ (xNumber *)ln2f {
	static xNumber *ln2v;
	if (ln2v == NULL) {
		ln2v = [xNumber realWithRawReal:Real::ln2];
	}
	return ln2v;
}

+ (xNumber *)ln10f {
	static xNumber *ln10v;
	if (ln10v == NULL) {
		ln10v = [xNumber realWithRawReal:Real::ln10];
	}
	return ln10v;
}

@end