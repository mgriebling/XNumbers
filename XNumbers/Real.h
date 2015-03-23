//
//  Real.h
//  TestReals
//
//  Created by Mike Griebling on 14 Mar 2015.
//  Copyright (c) 2015 Computer Inspirations. All rights reserved.
//

#import <Foundation/Foundation.h>

enum NumberType {
	RealNumber, ComplexNumber, RationalNumber
};

@interface xNumber : NSObject <NSCoding>

// class methods
+ (void)setDigits:(NSInteger)digits;
+ (NSInteger)digits;
+ (void)setErr:(NSInteger)error;
+ (NSInteger)err;
+ (void)setAngularMeasure:(NSInteger)measure;

// constructors
+ (xNumber *) realWithString: (NSString *)value;
+ (xNumber *) realWithString: (NSString *)value andBase: (NSInteger)base;
+ (xNumber *) realWithDouble: (double)value;
+ (xNumber *) realWithReal: (xNumber *)real;
+ (xNumber *) realWithInt: (NSInteger)value;
+ (xNumber *) realWithReal: (xNumber *)real andImaginary:(xNumber *)imaginary;
+ (xNumber *) realWithDouble: (double)real andImaginary:(double)imaginary;
+ (xNumber *) realWithNumerator: (xNumber *)numerator andDenominator:(xNumber *)denominator;
+ (xNumber *) realWithIntNumerator: (NSInteger)numerator andDenominator:(NSInteger)denominator;
+ (xNumber *) realWithString: (NSString *)real andString: (NSString *)imag withRational: (BOOL)isRational andBase: (NSInteger)base;
+ (xNumber *) realWithMagnitude: (xNumber *)magnitude andAngle: (xNumber *)angle;

- (id) initWithString: (NSString *)value;
- (id) initWithString: (NSString *)value andBase: (NSInteger)base;
- (id) initWithDouble: (double)value;
- (id) initWithInt: (NSInteger)value;
- (id) initWithReal: (xNumber *)real;
- (id) initWithReal: (xNumber *)real andImaginary:(xNumber *)imaginary;
- (id) initWithDouble: (double)real andImaginary:(double)imaginary;
- (id) initWithNumerator: (xNumber *)numerator andDenominator:(xNumber *)denominator;
- (id) initWithIntNumerator: (NSInteger)numerator andDenominator:(NSInteger)denominator;
- (id) initWithString: (NSString *)real andString: (NSString *)imag withRational: (BOOL)isRational andBase: (NSInteger)base;
- (id) initWithMagnitude: (xNumber *)magnitude andAngle: (xNumber *)angle;

// number attributes and conversions
- (NSInteger)exponent;
- (NSString *) toString;
- (double) Short;
- (xNumber *) entier;
- (xNumber *) fraction;
- (xNumber *) real;
- (xNumber *) conj;
- (xNumber *) imaginary;
- (xNumber *) numerator;
- (xNumber *) denominator;
- (xNumber *) polarMag;
- (xNumber *) polarAngle;

- (BOOL) isRational;
- (BOOL) isComplex;
- (BOOL) isInteger;

// basic math routines
- (xNumber *) add: (xNumber *) r;
- (xNumber *) sub: (xNumber *) r;
- (xNumber *) mul: (xNumber *) r;
- (xNumber *) div: (xNumber *) r;
- (xNumber *) abs;
- (xNumber *) negate;
- (NSInteger) sign;
- (NSInteger) cmp: (xNumber *) r;
- (xNumber *) round;

/* Power and transcendental routines */
- (xNumber *) power: (xNumber *) exp;
- (xNumber *) ipower: (NSInteger) i;
- (xNumber *) iroot: (long) n;
- (xNumber *) root: (xNumber *) base;
- (xNumber *) sqrt;
- (xNumber *) exp;
- (xNumber *) ln;
- (xNumber *) log10;
- (xNumber *) log2;
- (xNumber *) sin;
- (xNumber *) cos;
- (xNumber *) tan;
- (xNumber *) cot;
- (xNumber *) arccot;
- (xNumber *) arcsin;
- (xNumber *) arccos;
- (xNumber *) arctan;
- (xNumber *) sinh;
- (xNumber *) cosh;
- (xNumber *) tanh;
- (xNumber *) coth;
- (xNumber *) arcsinh;
- (xNumber *) arccosh;
- (xNumber *) arctanh;
- (xNumber *) arccoth;

/* Misc. routines */
- (xNumber *) factorial;
- (xNumber *) permutations: (xNumber *) r;
- (xNumber *) combinations: (xNumber *) r;
+ (xNumber *) random;

/* Integer routines */
- (xNumber *) setBit: (NSInteger) bit;
- (xNumber *) clearBit: (NSInteger) bit;
- (xNumber *) toggleBit: (NSInteger) bit;
- (xNumber *) and: (xNumber *) i;
- (xNumber *) nand: (xNumber *) i;
- (xNumber *) or: (xNumber *) i;
- (xNumber *) nor: (xNumber *) i;
- (xNumber *) xor: (xNumber *) i;
- (xNumber *) count;
- (xNumber *) intDiv: (xNumber *) i;
- (xNumber *) mod: (xNumber *) i;
- (xNumber *) onesComp;
- (xNumber *) shl: (NSInteger) bit;
- (xNumber *) rol: (NSInteger) bit;
- (xNumber *) shr: (NSInteger) bit;
- (xNumber *) ashr: (NSInteger) bit;
- (xNumber *) ror: (NSInteger) bit;
- (xNumber *) fib;
- (xNumber *) gcd: (xNumber *) i;
+ (xNumber *) strToInt: (NSString *)str withBase: (NSInteger)base;
- (NSString *) intToStr: (NSInteger)base;

// initialization and constants
+ (void)initialize;
+ (xNumber *)pif;
+ (xNumber *)epsf;
+ (xNumber *)ln2f;
+ (xNumber *)ln10f;
+ (xNumber *)zerof;
+ (xNumber *)onef;

@end

