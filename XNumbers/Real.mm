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
		if (rational || base != 10) {
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
	return [self initWithString:value andString:@"" withRational:NO andBase:10];
}

- (id) initWithString: (NSString *)value andBase: (NSInteger)base {
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
		NSString *first = toString(self->z.RealPart());
		NSString *second = toString(self->z.ImagPart());
		if ([second isEqualToString:@"1"]) {
			return first;
		}
		return [NSString stringWithFormat:@"%@/%@", first, second];
	} else {
		char str[1024];
		Complex::ComplexToStr(self->z, str);
		return [NSString stringWithCString:str encoding:NSASCIIStringEncoding];
	}
}

- (NSInteger)exponent {
	return self.x.exponent();
}

- (double) Short {
	return Real::Short(self.x);
}

- (xNumber *) entier {
	return [xNumber realWithRawReal:Real::entier(self.x)];
}

- (xNumber *) fraction {
	return [xNumber realWithRawReal:Real::fraction(self.x)];
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
	return xNumber.zero;
}

- (xNumber *) denominator {
	if (rational) {
		return [xNumber realWithRawReal:z.ImagPart()];
	}
	return xNumber.one;
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

static void FixRational (xNumber *n) {
	if (n->rational) {
		// fix up rational number to real
		n.x = Real::div(n->z.RealPart(), n->z.ImagPart());
		n->rational = NO;
	}
}

static xNumber * addRational (Complex::Complex a, Complex::Complex b) {
	Real::Real z;
	Real::Real anum = a.RealPart();
	Real::Real aden = a.ImagPart();
	Real::Real bnum = b.RealPart();
	Real::Real bden = b.ImagPart();
	z = Real::mul(anum, bden);
	return InitRational(Real::add(z, Real::mul(bnum, aden)), Real::mul(aden, bden));
}

static xNumber * subRational (Complex::Complex a, Complex::Complex b) {
	Real::Real z;
	Real::Real anum = a.RealPart();
	Real::Real aden = a.ImagPart();
	Real::Real bnum = b.RealPart();
	Real::Real bden = b.ImagPart();
	z = Real::mul(anum, bden);
	return InitRational(Real::sub(z, Real::mul(bnum, aden)), Real::mul(aden, bden));
}

static xNumber * mulRational (Complex::Complex a, Complex::Complex b) {
	Real::Real anum = a.RealPart();
	Real::Real aden = a.ImagPart();
	Real::Real bnum = b.RealPart();
	Real::Real bden = b.ImagPart();
	return InitRational(Real::mul(anum, bnum), Real::mul(aden, bden));
}

static xNumber * divRational (Complex::Complex a, Complex::Complex b) {
	Real::Real anum = a.RealPart();
	Real::Real aden = a.ImagPart();
	Real::Real bnum = b.RealPart();
	Real::Real bden = b.ImagPart();
	return InitRational(Real::mul(anum, bden), Real::mul(aden, bnum));
}

- (xNumber *) add: (xNumber *) r {
	if (self->rational && r->rational) {
		return addRational(self->z, r->z);
	} else {
		FixRational(self); FixRational(r);
		Complex::Complex zi;
		Complex::Add(zi, self->z, r->z);
		return [xNumber realWithRawComplex:zi];
	}
}

- (xNumber *) sub: (xNumber *) r {
	if (self->rational && r->rational) {
		return subRational(self->z, r->z);
	} else {
		FixRational(self); FixRational(r);
		Complex::Complex zi;
		Complex::Sub(zi, self->z, r->z);
		return [xNumber realWithRawComplex:zi];
	}
}

- (xNumber *) mul: (xNumber *) r {
	if (self->rational && r->rational) {
		return mulRational(self->z, r->z);
	} else {
		FixRational(self); FixRational(r);
		Complex::Complex zi;
		Complex::Mult(zi, self->z, r->z);
		return [xNumber realWithRawComplex:zi];
	}
}

- (xNumber *) div: (xNumber *) r {
	if (self->rational && r->rational) {
		return divRational(self->z, r->z);
	} else {
		FixRational(self); FixRational(r);
		Complex::Complex zi;
		Complex::Div(zi, self->z, r->z);
		return [xNumber realWithRawComplex:zi];
	}
}

- (xNumber *) abs {
	return [xNumber realWithRawReal:Real::abs(self.x)];
}

- (NSInteger) sign {
	if (isZero(self.x)) return 0;
	return Real::sign(self.x);
}

- (NSInteger) cmp: (xNumber *) r {
	return Real::cmp(self.x, r.x);
}

- (xNumber *) round {
	Real::Real xi = self.x;
	Real::round(xi);
	return [xNumber realWithRawReal:xi];
}


/* Power and transcendental routines */

- (xNumber *) power: (xNumber *) exp {
	Complex::Complex zi;
	Complex::power(zi, self->z, exp->z);
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) sqrt {
	Complex::Complex zi;
	Complex::sqrt(zi, self->z);
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) exp {
	Complex::Complex zi;
	Complex::exp(zi, self->z);
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) ln {
	Complex::Complex zi;
	Complex::ln(zi, self->z);
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) sin {
	Complex::Complex zi;
	Complex::sin(zi, self->z);
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) cos {
	Complex::Complex zi;
	Complex::cos(zi, self->z);
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) tan {
	Complex::Complex zi;
	Complex::tan(zi, self->z);
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) arcsin {
	Complex::Complex zi;
	Complex::arcsin(zi, self->z);
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) arccos {
	Complex::Complex zi;
	Complex::arccos(zi, self->z);
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) arctan {
	Complex::Complex zi;
	Complex::arctan(zi, self->z);
	return [xNumber realWithRawComplex:zi];;
}

- (xNumber *) sinh {
	Complex::Complex zi;
	Complex::sinh(zi, self->z);
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) cosh {
	Complex::Complex zi;
	Complex::sinh(zi, self->z);
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) tanh {
	Complex::Complex zi;
	Complex::tanh(zi, self->z);
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) conj {
	Complex::Complex zi = self->z;
	Complex::Conj(zi);
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) polarMag {
	Real::Real xi;
	Complex::PolarMag(xi, self->z);
	return [xNumber realWithRawReal:xi];
}

- (xNumber *) polarAngle {
	Real::Real xi;
	Complex::PolarAngle(xi, self->z);
	return [xNumber realWithRawReal:xi];
}

- (xNumber *) negate {
	Complex::Complex zi = self->z;
	Complex::ChgSign(zi);
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) ipower: (NSInteger) i {
	Complex::Complex zi;
	Complex::xtoi(zi, self->z, i);
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) iroot: (NSInteger) n {
	Complex::Complex zi;
	Complex::Root(zi, self->z, n);
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) root: (xNumber *) x {
	Complex::Complex zi;
	Complex::root(zi, self->z, x->z);
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) log10 {
	Complex::Complex zi;
	Complex::log(zi, self->z);
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) log2 {
	Complex::Complex zi;
	Complex::log2(zi, self->z);
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) cot {
	Complex::Complex zi;
	Complex::cot(zi, self->z);
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) arccot {
	Complex::Complex zi;
	Complex::arccot(zi, self->z);
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) coth {
	Complex::Complex zi;
	Complex::coth(zi, self->z);
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) arcsinh {
	Complex::Complex zi;
	Complex::arcsinh(zi, self->z);
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) arccosh {
	Complex::Complex zi;
	Complex::arccosh(zi, self->z);
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) arctanh {
	Complex::Complex zi;
	Complex::arctanh(zi, self->z);
	return [xNumber realWithRawComplex:zi];
}

- (xNumber *) arccoth {
	Complex::Complex zi;
	Complex::arccoth(zi, self->z);
	return [xNumber realWithRawComplex:zi];
}



/* Integer routines */
- (xNumber *) setBit: (NSInteger) bit {
	Real::Real t;
	Integer::SetBit(t, self.x, bit);
	return [xNumber realWithRawReal:t];
}

- (xNumber *) clearBit: (NSInteger) bit {
	Real::Real t;
	Integer::ClearBit(t, self.x, bit);
	return [xNumber realWithRawReal:t];
}

- (xNumber *) toggleBit: (NSInteger) bit {
	Real::Real t;
	Integer::ToggleBit(t, self.x, bit);
	return [xNumber realWithRawReal:t];
}

- (xNumber *) and: (xNumber *) i {
	Real::Real t;
	Integer::And(t, self.x, i.x);
	return [xNumber realWithRawReal:t];
}

- (xNumber *) nand: (xNumber *) i {
	Real::Real t;
	Integer::Nand(t, self.x, i.x);
	return [xNumber realWithRawReal:t];
}

- (xNumber *) or: (xNumber *) i {
	Real::Real t;
	Integer::Or(t, self.x, i.x);
	return [xNumber realWithRawReal:t];
}

- (xNumber *) nor: (xNumber *) i {
	Real::Real t;
	Integer::Nor(t, self.x, i.x);
	return [xNumber realWithRawReal:t];
}

- (xNumber *) xor: (xNumber *) i {
	Real::Real t;
	Integer::Xor(t, self.x, i.x);
	return [xNumber realWithRawReal:t];
}

- (xNumber *) count {
	Real::Real t;
	Integer::Count(t, self.x);
	return [xNumber realWithRawReal:t];
}

- (xNumber *) intDiv: (xNumber *) i {
	Real::Real t;
	Integer::IntDiv(t, self.x, i.x);
	return [xNumber realWithRawReal:t];
}

- (xNumber *) mod: (xNumber *) i {
	Real::Real t;
	Integer::Mod(t, self.x, i.x);
	return [xNumber realWithRawReal:t];
}

- (xNumber *) onesComp {
	Real::Real t;
	Integer::OnesComp(t, self.x);
	return [xNumber realWithRawReal:t];
}

- (xNumber *) shl: (NSInteger) bit {
	Real::Real t;
	Integer::Shl(t, self.x, bit);
	return [xNumber realWithRawReal:t];
}

- (xNumber *) rol: (NSInteger) bit {
	Real::Real t;
	Integer::Rol(t, self.x, bit);
	return [xNumber realWithRawReal:t];
}

- (xNumber *) shr: (NSInteger) bit {
	Real::Real t;
	Integer::Shr(t, self.x, bit);
	return [xNumber realWithRawReal:t];
}

- (xNumber *) ashr: (NSInteger) bit {
	Real::Real t;
	Integer::Ashr(t, self.x, bit);
	return [xNumber realWithRawReal:t];
}

- (xNumber *) ror: (NSInteger) bit {
	Real::Real t;
	Integer::Ror(t, self.x, bit);
	return [xNumber realWithRawReal:t];
}

- (xNumber *) fib {
	Real::Real t;
	Integer::Fib(t, self.x);
	return [xNumber realWithRawReal:t];
}

- (xNumber *) gcd: (xNumber *) i {
	Real::Real t;
	Integer::GCD(t, self.x, i.x);
	return [xNumber realWithRawReal:t];
}

+ (xNumber *) strToInt: (NSString *)str withBase: (NSInteger)base {
	Real::Real t;
	Integer::StrToInt(str.UTF8String, base, t);
	return [xNumber realWithRawReal:t];
}

- (NSString *) intToStr: (NSInteger)base {
	char result[1024];
	Integer::IntToStr(self.x, base, result);
	return [NSString stringWithCString:result encoding:NSASCIIStringEncoding];
}


/* Misc. routines */
- (xNumber *) factorial {
	return [xNumber realWithRawReal:Real::factorial(self.x)];
}

- (xNumber *) permutations: (xNumber *) r {
	return [xNumber realWithRawReal:Real::permutations(self.x, r.x)];
}

- (xNumber *) combinations: (xNumber *) r {
	return [xNumber realWithRawReal:Real::combinations(self.x, r.x)];
}

+ (xNumber *) random {
	return [xNumber realWithRawReal:Real::random()];
}


+ (void)initialize {
	Complex::InitComplex();		// initialize the Integer, Real, & Complex C++ classes
}

+ (xNumber *)zero {
	static xNumber *zerov;
	if (zerov == NULL) {
		zerov = [xNumber realWithDouble:0];
	}
	return zerov;
}

+ (xNumber *)one {
	static xNumber *onev;
	if (onev == NULL) {
		onev = [xNumber realWithDouble:1];
	}
	return onev;
}

+ (xNumber *)pi {
	static xNumber *piv;
	if (piv == NULL) {
		piv = [xNumber realWithRawReal:Real::pi];
	}
	return piv;
}

+ (xNumber *)eps {
	static xNumber *epsv;
	if (epsv == NULL) {
		epsv = [xNumber realWithRawReal:Real::eps];
	}
	return epsv;
}

+ (xNumber *)ln2 {
	static xNumber *ln2v;
	if (ln2v == NULL) {
		ln2v = [xNumber realWithRawReal:Real::ln2];
	}
	return ln2v;
}

+ (xNumber *)ln10 {
	static xNumber *ln10v;
	if (ln10v == NULL) {
		ln10v = [xNumber realWithRawReal:Real::ln10];
	}
	return ln10v;
}

@end