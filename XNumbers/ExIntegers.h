#ifndef EXINTEGERS_H
#define EXINTEGERS_H

#include "Reals.h"

typedef char BaseType;

class Integer
{
public:
	static void SetBit(Real& Result, const Real& number, short bitnum);
	static void ClearBit(Real& Result, const Real& number, short bitnum);
	static void ToggleBit(Real& Result, const Real& number, short bitnum);
	static void And(Real& Result, const Real& op1, const Real& op2);
	static void Nand(Real& Result, const Real& op1, const Real& op2);
	static void Or(Real& Result, const Real& op1, const Real& op2);
	static void Nor(Real& Result, const Real& op1, const Real& op2);
	static void Xor(Real& Result, const Real& op1, const Real& op2);
	static void Count(Real& Result, const Real& op1);
	static void IntDiv(Real& Result, const Real& op1, const Real& op2);
	static void Mod(Real& Result, const Real& op1, const Real& op2);
	static void OnesComp(Real& Result, const Real& number);
	static void Shl(Real& Result, const Real& number, short numbits);
	static void Rol(Real& Result, const Real& number, short numbits);
	static void Shr(Real& Result, const Real& number, short numbits);
	static void Ashr(Real& Result, const Real& number, short numbits);
	static void Ror(Real& Result, const Real& number, short numbits);
	static void Fib(Real& Result, const Real& number);
	static void GCD(Real& Result, const Real& op1, const Real& op2);
	static void StrToInt(const char *S, BaseType Base, Real& A);
	static void IntToStr(const Real& A, BaseType Base, char *S);
	
	static void Init(void);
	
private:
	static const long MaxBase2Bits = 671;
	static const long LogicalSize = MaxBase2Bits / 16;
	static const bool Left = false;
	static const bool Right = true;
	
	typedef unsigned Logical[LogicalSize+1];
	typedef unsigned (* LogicalProc) (unsigned a, unsigned b);
	typedef Real (* ExNumbProc) (const Real& a, const Real& b);

	static unsigned LogZero[LogicalSize+1];
	static Real One, Two;
	static Real Zero;
	static short Cnt;
	
	static bool initialized;
	
	static unsigned AndSet (unsigned op1, unsigned op2);
	static unsigned AndNotSet (unsigned op1, unsigned op2);
	static unsigned OrSet (unsigned op1, unsigned op2);
	static unsigned XorSet (unsigned op1, unsigned op2);
	static unsigned NorSet (unsigned op1, unsigned op2);
	static unsigned NandSet (unsigned op1, unsigned op2);
	static unsigned NotSet (unsigned op1, unsigned op2 = 0);		
	static bool IsZero (const Real& x);
	
	/* Misc. local functions */
	static Real Max();
	static int MaxBits();
	static void ConstrainNum(Real& Number);
	static void NumToLogical(const Real& Numb, Logical& logical);
	static void LogicalToNum(const Logical& logical, Real& Numb);
	static void LOp(Real& Result, const Real& op1, LogicalProc Oper, const Real& op2);
	static void LOp1(Real& Result, LogicalProc Oper, const Real& op);
	static void LBit(Real& Result, const Real& number, LogicalProc Oper, short bitnum);
	static bool Bit(Real& number, short bitnum);
	static void LShift(Real& Result, const Real& number, ExNumbProc ExOper, short bits);
	static void LRotate(Real& Result, const Real& number, bool Shiftright, short bits);
};

#endif
