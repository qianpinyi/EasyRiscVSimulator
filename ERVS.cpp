/*
	EasyRiscVSimulator V1.0
	by:qianpinyi
	24.5.21
*/
#define PAL_AUTOINIT_VTERM
#define PAL_AUTOINIT_U8TERM
#include <PAL/IO/Xout>
#include <PAL/Common/Pack>
#include <PAL/Single>
#include <PAL/Legacy/PAL_Library/PAL_BasicFunctions/PAL_File.cpp>
#include <PAL/Legacy/PAL_Library/PAL_BasicFunctions/PAL_System.cpp>
#include <PAL/Legacy/PAL_Library/PAL_DataStructure/PAL_Tuple.cpp>
#include <bits/stdc++.h>
using namespace PAL;
using namespace PAL::IO;
using namespace PAL::Legacy::PAL_DS;
using namespace std;

constexpr bool IsRV64=true;

using RegisterData=Uint64;
constexpr RegisterData RegisterRank=6;
using SignedRegisterData=Sint64;

XoutType RVDD=XoutType::RegisterNewType("ERVS",LightYellow);

struct GeneralPurposeRegisters
{
	union
	{
		RegisterData x[32];
		struct
		{
			RegisterData zero,ra,sp,gp,tp,
						 t0,t1,t2,
						 s0,s1,
						 a0,a1,a2,a3,a4,a5,a6,a7,
						 s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,
						 t3,t4,t5,t6;
		};
	};
	
	inline RegisterData& operator [] (int index)
	{return x[index];}
	
	GeneralPurposeRegisters()
	{
		MemsetT<RegisterData>(x,0,32);
	}
};

template <int N> struct EasyBitSeq
{
	RegisterData x=0;
	
	RegisterData zext()
	{return x;}
	
	RegisterData sext()
	{
		if (x&1ull<<N-1)
			return x|~0ull<<N;
		else return x;
	}
	
	bool operator == (RegisterData y)
	{return x==y;}
	
	template <int M> Get()
	{return x>>M&1;}
	
	template <int M> EasyBitSeq <N+M> operator + (EasyBitSeq <M> y) requires(N+M<=64)
	{
		return EasyBitSeq<N+M>(x<<M|y.x);
	}
	
	EasyBitSeq(RegisterData y):x(y&(1ull<<N)-1) {}
	
	EasyBitSeq(RegisterData y,const int l):x(y>>l&(1ull<<N)-1) {}//[low,low+n)
};
//using BitSeq=EasyBitSeq;
#define BitSeq EasyBitSeq

template <int N> RegisterData SignedExtends(RegisterData x) requires (1<=N&&N<=64)
{
	if (x&1ull<<N-1)
		return x|~0ull<<N;
	else return x;
}

template <int N> RegisterData ZeroExtends(RegisterData x) requires (1<=N&&N<=64)
{
	return x&(1ull<<N)-1;
}

template <int N> RegisterData Truncate(RegisterData x) requires (1<=N&&N<=64)
{
	return x&(1ull<<N)-1;
}

class PhysicalMemory
{
	protected:
		map <Uint64,Uint8> Memory;//0 inited
		bool BigEndian=false;
		
		Uint64 L(Uint64 p)
		{
			if (auto mp=Memory.find(p);mp!=Memory.end())
				return mp->second;
			else return 0;
		}
		
		void S(Uint64 p,Uint64 x)
		{
			Memory.erase(p);
			if (x!=0)
				Memory[p]=x;
		}
		
		struct MemAccess
		{
			PhysicalMemory *This;
			Uint64 p;
			
			operator Uint8 ()
			{return This->L(p);}
			
			void operator = (Uint8 x)
			{This->S(p,x);}
			
			MemAccess(const MemAccess&&)=delete;
			MemAccess(const MemAccess&)=delete;
			MemAccess& operator = (const MemAccess&&)=delete;
			MemAccess& operator = (const MemAccess&)=delete;
			MemAccess(PhysicalMemory *_this,Uint64 _p):This(_this),p(_p) {}
		};
		
		template <Uint64 i> Uint64 _LoadI(Uint64 p) requires(i<8)
		{
			if constexpr (i==0)
				return L(p);
			else return L(p+i)<<i*8|_LoadI<i-1>(p); 
		}
		
		template <Uint64 i> void _StoreI(Uint64 p,Uint64 x) requires(i<8)
		{
			S(p,x&0xFF);
			if constexpr (i>0)
				_StoreI<i-1>(p+1,x>>8);
		}
		
	public:
		MemAccess operator [] (Uint64 p)
		{return MemAccess{this,p};}
		
		template <int N,bool sext> int Load(Uint64 p,RegisterData &x) requires (N==1||N==2||N==4||N==8)
		{
			x=_LoadI<N-1> (p);
			if (BigEndian)
				x=EndianSwitch<RegisterData,N>(x);
			if constexpr (sext)
				x=SignedExtends<N*8>(x);
			return 0;
		}
		
		template <int N> int Store(Uint64 p,RegisterData x) requires (N==1||N==2||N==4||N==8)
		{
			if (BigEndian)
				x=EndianSwitch<RegisterData,N>(x);
			_StoreI<N-1>(p,x);
			return 0;
		}
};

struct Instruction//RV32I/RV64I
{
	enum class InstType
	{
		IT_Unknown=0,
		R,
		I,
		S,
		B,
		U,
		J
	};
	
	enum class OpCode
	{
		OP_Unknown=0,
		R_ALU	=0b0110011,
		R_ALU2	=0b0111011,
		I_ALU	=0b0010011,
		I_ALU2	=0b0011011,
		I_Load	=0b0000011,
		S_Store	=0b0100011,
		B_Branch=0b1100011,
		J_JAL	=0b1101111,
		J_JALR	=0b1100111,
		U_LUI	=0b0110111,
		U_AUIPC	=0b0010111,
		I_ECALL	=0b1110011,
	};
	
	enum INST
	{
		Unknown=0,
		Begin_R_ALU,
			ADD,SUB,XOR,OR,AND,SLL,SRL,SRA,SLT,SLTU,ADDW,SUBW,SLLW,SRLW,SRAW,
		End_R_ALU,
		Begin_I_ALU,
			ADDI,XORI,ORI,ANDI,SLLI,SRLI,SRAI,SLTI,SLTIU,ADDIW,SLLIW,SRLIW,SRAIW,
		End_I_ALU,
		Begin_I_Load,
			LB,LH,LW,LBU,LHU,LWU,LD,
		End_I_Load,
		Begin_S_Store,
			SB,SH,SW,SD,
		End_S_Store,
		Begin_B_Branch,
			BEQ,BNE,BLT,BGE,BLTU,BGEU,
		End_B_Branch,
		JAL,JALR,
		LUI,AUIPC,
		ECALL,EBREAK,
		Begin_R_MUL,
			MUL,MULH,MULSU,MULU,DIV,DIVU,REM,REMU,
		End_R_MUL,
	};
	
	Uint64 inst=0;
	
	operator bool ()
	{return inst!=0;}
	
	OpCode opcode()
	{return (OpCode)(inst&0b1111111);}
	
	int rd()
	{return inst>>7&0b11111;}
	
	int funct3()
	{return inst>>12&0b111;}
	
	int rs1()
	{return inst>>15&0b11111;}
	
	int rs2()
	{return inst>>20&0b11111;}
	
	int funct7()
	{return inst>>25&0b1111111;}
	
	InstType type()
	{
		using enum OpCode;
		using enum InstType;
		switch (opcode())
		{
			case R_ALU:	case R_ALU2:
				return R;
			case I_ALU:	case I_Load: case I_ECALL: case J_JALR:
				return I;
			case S_Store:
				return S;
			case B_Branch:
				return B;
			case J_JAL:
				return J;
			case U_LUI:	case U_AUIPC:
				return U;
			default:
				return IT_Unknown;
		}
	}
	
	RegisterData uimm()
	{
		using enum InstType;
		switch (type())
		{
			case I:	return inst>>20;
			case S:	return inst>>20&0b111111100000|inst>>7&0b11111;
			case B:	return inst>>19&0x1000|inst<<4&0x800|inst>>20&0b11111100000|inst>>7&0b11110;//??
			case U:	return inst&0xFFFFF000;
			case J:	return inst>>11&0x100000|inst&0xFF000|inst>>7&0x800|inst>>20&0b11111111110;
			default:return 0;
		}
	}
	
	SignedRegisterData simm()
	{
		using enum InstType;
		switch (type())
		{
			case I:	return SignedExtends<12>(uimm());
			case S:	return SignedExtends<12>(uimm());
			case B:	return SignedExtends<13>(uimm());
			case U:	return SignedExtends<32>(uimm());
			case J:	return SignedExtends<21>(uimm());
			default:return 0;
		}
	}
	
	INST which()
	{
		using enum OpCode;
		using enum INST;
		switch (opcode())
		{
			case R_ALU:
				switch (funct3())
				{
					case 0x0:
						switch (funct7())
						{
							case 0x00:	return ADD;
							case 0x01:	return MUL;
							case 0x20:	return SUB;
							default:	return Unknown;
						}
					case 0x1:
						switch (funct7())
						{
							case 0x00:	return SLL;
							case 0x01:	return MULH;
							default:	return Unknown;
						}
					case 0x2:
						switch (funct7())
						{
							case 0x00:	return SLT;
							case 0x01:	return MULSU;
							default:	return Unknown;
						}
					case 0x3:
						switch (funct7())
						{
							case 0x00:	return SLTU;
							case 0x01:	return MULU;
							default:	return Unknown;
						}
					case 0x4:
						switch (funct7())
						{
							case 0x00:	return XOR;
							case 0x01:	return DIV;
							default:	return Unknown;
						}
					case 0x5:
						switch (funct7())
						{
							case 0x00:	return SRL;
							case 0x01:	return DIVU;
							case 0x20:	return SRA;
							default:	return Unknown;
						}
					case 0x6:
						switch (funct7())
						{
							case 0x00:	return OR;
							case 0x01:	return REM;
							default:	return Unknown;
						}
					case 0x7:
						switch (funct7())
						{
							case 0x00:	return AND;
							case 0x01:	return REMU;
							default:	return Unknown;
						}
					default:
						return Unknown;
				}
			case R_ALU2:
				switch (funct3())
				{
					case 0x0:
						switch (funct7())
						{
							case 0x00:	return ADDW;
							case 0x20:	return SUBW;
							default:	return Unknown;
						}
					case 0x1:
						switch (funct7())
						{
							case 0x00:	return SLLW;
							default:	return Unknown;
						}
					case 0x5:
						switch (funct7())
						{
							case 0x00:	return SRLW;
							case 0x20:	return SRAW;
							default:	return Unknown;
						}
					default:	return Unknown;
				}
			case I_ALU:
				switch (funct3())
				{
					case 0x0:	return ADDI;
					case 0x1:
						switch (uimm()>>6&0b111111)
						{
							case 0x00:	return SLLI;
							default:	return Unknown;
						}
					case 0x2:	return SLTI;
					case 0x3:	return SLTIU;
					case 0x4:	return XORI;
					case 0x5:
						switch (uimm()>>6&0b111111)
						{
							case 0x00:	return SRLI;
							case 0x10:	return SRAI;
							default:	return Unknown;
						}
					case 0x6:	return ORI;
					case 0x7:	return ANDI;
					default:	return Unknown;
				}
			case I_ALU2:
				switch (funct3())
				{
					case 0x0:	return ADDIW;
					case 0x1:
						switch (uimm()>>6&0b111111)
						{
							case 0x00:	return SLLIW;
							default:	return Unknown;
						}
					case 0x5:
						switch (uimm()>>6&0b111111)
						{
							case 0x00:	return SRLIW;
							case 0x10:	return SRAIW;
							default:	return Unknown;
						}
					default:	return Unknown;
				}
			case I_Load:
				switch (funct3())
				{
					case 0x0:	return LB;
					case 0x1:	return LH;
					case 0x2:	return LW;
					case 0x3:	return LD;
					case 0x4:	return LBU;
					case 0x5:	return LHU;
					case 0x6:	return LWU;
					default:	return Unknown;
				}
			case S_Store:
				switch (funct3())
				{
					case 0x0:	return SB;
					case 0x1:	return SH;
					case 0x2:	return SW;
					case 0x3:	return SD;
					default:	return Unknown;
				}
			case B_Branch:
				switch (funct3())
				{
					case 0x0:	return BEQ;
					case 0x1:	return BNE;
					case 0x4:	return BLT;
					case 0x5:	return BGE;
					case 0x6:	return BLTU;
					case 0x7:	return BGEU;
					default:	return Unknown;
				}
			case J_JAL:		return JAL;
			case J_JALR:
				if (funct3()==0x0)
					return JALR;
				else return Unknown;
			case U_LUI:		return LUI;
			case U_AUIPC:	return AUIPC;
			case I_ECALL:
				if (funct3()==0x0)
					if (uimm()==0x0)
						return ECALL;
					else if (uimm()==0x1)
						return EBREAK;
				return Unknown;
			default:
				return Unknown;
		}
	}
	
	static const char * InstName(int which)
	{
		switch (which)
		{
			case ADD:	return "ADD";
			case SUB:	return "SUB";
			case XOR:	return "XOR";
			case OR:	return "OR";
			case AND:	return "AND";
			case SLL:	return "SLL";
			case SRL:	return "SRL";
			case SRA:	return "SRA";
			case SLT:	return "SLT";
			case SLTU:	return "SLTU";
			case ADDW:	return "ADDW";
			case SUBW:	return "SUBW";
			case SLLW:	return "SLLW";
			case SRLW:	return "SRLW";
			case SRAW:	return "SRAW";
			case ADDI:	return "ADDI";
			case XORI:	return "XORI";
			case ORI:	return "ORI";
			case ANDI:	return "ANDI";
			case SLLI:	return "SLLI";
			case SRLI:	return "SRLI";
			case SRAI:	return "SRAI";
			case SLTI:	return "SLTI";
			case SLTIU:	return "SLTIU";
			case ADDIW:	return "ADDIW";
			case SLLIW:	return "SLLIW";
			case SRLIW:	return "SRLIW";
			case SRAIW:	return "SRAIW";
			case LB:	return "LB";
			case LH:	return "LH";
			case LW:	return "LW";
			case LBU:	return "LBU";
			case LHU:	return "LHU";
			case LWU:	return "LWU";
			case LD:	return "LD";
			case SB:	return "SB";
			case SH:	return "SH";
			case SW:	return "SW";
			case SD:	return "SD";
			case BEQ:	return "BEQ";
			case BNE:	return "BNE";
			case BLT:	return "BLT";
			case BGE:	return "BGE";
			case BLTU:	return "BLTU";
			case BGEU:	return "BGEU";
			case JAL:	return "JAL";
			case JALR:	return "JALR";
			case LUI:	return "LUI";
			case AUIPC:	return "AUIPC";
			case ECALL:	return "ECALL";
			case EBREAK:return "EBREAK";
			case MUL:	return "MUL";
			case MULH:	return "MULH";
			case MULSU:	return "MULSU";
			case MULU:	return "MULU";
			case DIV:	return "DIV";
			case DIVU:	return "DIVU";
			case REM:	return "REM";
			case REMU:	return "REMU";
			default:	return "Unknown";
		}
	}

	void DebugPrint()
	{
		xout[RVDD]<<"Inst: "<<(void*)inst<<endline
				  <<Tab4<<"name: "<<InstName(which())<<endline
				  <<Tab4<<"rd:   "<<rd()<<endline
				  <<Tab4<<"rs1:  "<<rs1()<<endline
				  <<Tab4<<"rs2:  "<<rs2()<<endline
				  <<Tab4<<"uimm: "<<(void*)uimm()<<endline
				  <<Tab4<<"simm: "<<(void*)simm()<<endline
				  <<endl;
	}
	
	template <INST I> static Instruction Create(int rd,int rs1,int rs2) requires(InRange(I,Begin_R_ALU,End_R_ALU)||InRange(I,Begin_R_MUL,End_R_MUL))
	{
		using enum OpCode;
		RegisterData f7=0,f3=0;
		OpCode op=OpCode::OP_Unknown;
		switch (I)//Why no constexpr switch...
		{
			case ADD: f7=0x00; f3=0x0; op=R_ALU; break;
			case SUB: f7=0x20; f3=0x0; op=R_ALU; break;
			case XOR: f7=0x00; f3=0x4; op=R_ALU; break;
			case OR:  f7=0x00; f3=0x6; op=R_ALU; break;
			case AND: f7=0x00; f3=0x7; op=R_ALU; break;
			case SLL: f7=0x00; f3=0x1; op=R_ALU; break;
			case SRL: f7=0x00; f3=0x5; op=R_ALU; break;
			case SRA: f7=0x20; f3=0x5; op=R_ALU; break;
			case SLT: f7=0x00; f3=0x2; op=R_ALU; break;
			case SLTU:f7=0x00; f3=0x3; op=R_ALU; break;
			case ADDW: f7=0x00; f3=0x0; op=R_ALU2; break;
			case SUBW: f7=0x20; f3=0x0; op=R_ALU2; break;
			case SLLW: f7=0x00; f3=0x1; op=R_ALU2; break;
			case SRLW: f7=0x00; f3=0x5; op=R_ALU2; break;
			case SRAW: f7=0x20; f3=0x5; op=R_ALU2; break;
			case MUL:   f7=0x01; f3=0x0; op=R_ALU; break;
			case MULH:  f7=0x01; f3=0x1; op=R_ALU; break;
			case MULSU: f7=0x01; f3=0x2; op=R_ALU; break;
			case MULU:  f7=0x01; f3=0x3; op=R_ALU; break;
			case DIV:   f7=0x01; f3=0x4; op=R_ALU; break;
			case DIVU:  f7=0x01; f3=0x5; op=R_ALU; break;
			case REM:   f7=0x01; f3=0x6; op=R_ALU; break;
			case REMU:  f7=0x01; f3=0x7; op=R_ALU; break;
			default:
				xout[Fault]<<"Failed to init such R-Type INST "<<(void*)I<<endl;
				return Instruction(0);
		}
		return Instruction((BitSeq<7>(f7)+BitSeq<5>(rs2)+BitSeq<5>(rs1)+BitSeq<3>(f3)+BitSeq<5>(rd)+BitSeq<7>((Uint64)op)).zext());
	}
	
	template <INST I> static Instruction Create(int rd,int rs1,RegisterData uimm) requires(InRange(I,Begin_I_ALU,End_I_ALU))
	{
		using enum OpCode;
		RegisterData f3=0;
		OpCode op=OpCode::OP_Unknown;
		switch (I)
		{
			case ADDI: f3=0x0; op=I_ALU; break;
			case XORI: f3=0x4; op=I_ALU; break;
			case ORI:  f3=0x6; op=I_ALU; break;
			case ANDI: f3=0x7; op=I_ALU; break;
			case SLLI: f3=0x1; op=I_ALU; uimm=(BitSeq<6>(0)+BitSeq<6>(uimm)).zext(); break;
			case SRLI: f3=0x5; op=I_ALU; uimm=(BitSeq<6>(0)+BitSeq<6>(uimm)).zext(); break;
			case SRAI: f3=0x5; op=I_ALU; uimm=(BitSeq<6>(0x10)+BitSeq<6>(uimm)).zext(); break;
			case SLTI: f3=0x2; op=I_ALU; break;
			case SLTIU:f3=0x3; op=I_ALU; break;
			case ADDIW:f3=0x0; op=I_ALU2; break;
			case SLLIW:f3=0x1; op=I_ALU2; uimm=(BitSeq<6>(0)+BitSeq<6>(uimm)).zext(); break;
			case SRLIW:f3=0x5; op=I_ALU2; uimm=(BitSeq<6>(0)+BitSeq<6>(uimm)).zext(); break;
			case SRAIW:f3=0x5; op=I_ALU2; uimm=(BitSeq<6>(0x10)+BitSeq<6>(uimm)).zext(); break;
			default:
				xout[Fault]<<"Failed to init such I-Type INST "<<(void*)I<<endl;
				return Instruction(0);
		}
		return Instruction((BitSeq<12>(uimm)+BitSeq<5>(rs1)+BitSeq<3>(f3)+BitSeq<5>(rd)+BitSeq<7>((Uint64)op)).zext());
	}
	
	template <INST I> static Instruction Create(int rd,int rs1,RegisterData uimm) requires(InRange(I,Begin_I_Load,End_I_Load))
	{
		using enum OpCode;
		RegisterData f3=0;
		OpCode op=OpCode::OP_Unknown;
		switch (I)
		{
			case LB: f3=0x0; op=I_Load;	break;
			case LH: f3=0x1; op=I_Load;	break;
			case LW: f3=0x2; op=I_Load;	break;
			case LBU:f3=0x4; op=I_Load;	break;
			case LHU:f3=0x5; op=I_Load;	break;
			case LWU:f3=0x6; op=I_Load;	break;
			case LD: f3=0x3; op=I_Load;	break;
			default:
				xout[Fault]<<"Failed to init such I-Type Load INST "<<(void*)I<<endl;
				return Instruction(0);
		}
		return Instruction((BitSeq<12>(uimm)+BitSeq<5>(rs1)+BitSeq<3>(f3)+BitSeq<5>(rd)+BitSeq<7>((Uint64)op)).zext());
	}
	
	template <INST I> static Instruction Create(int rs1,int rs2,RegisterData uimm) requires(InRange(I,Begin_S_Store,End_S_Store))
	{
		using enum OpCode;
		RegisterData f3=0;
		OpCode op=OpCode::OP_Unknown;
		switch (I)
		{
			case SB: f3=0x0; op=S_Store; break;
			case SH: f3=0x1; op=S_Store; break;
			case SW: f3=0x2; op=S_Store; break;
			case SD: f3=0x3; op=S_Store; break;
			default:
				xout[Fault]<<"Failed to init such S-Type Store INST "<<(void*)I<<endl;
				return Instruction(0);
		}
		return Instruction((BitSeq<7>(uimm,5)+BitSeq<5>(rs2)+BitSeq<5>(rs1)+BitSeq<3>(f3)+BitSeq<5>(uimm,0)+BitSeq<7>((Uint64)op)).zext());
	}
	
	template <INST I> static Instruction Create(int rs1,int rs2,RegisterData uimm) requires(InRange(I,Begin_B_Branch,End_B_Branch))
	{
		using enum OpCode;
		RegisterData f3=0;
		OpCode op=OpCode::OP_Unknown;
		switch (I)
		{
			case BEQ: f3=0x0; op=B_Branch; break;
			case BNE: f3=0x1; op=B_Branch; break;
			case BLT: f3=0x4; op=B_Branch; break;
			case BGE: f3=0x5; op=B_Branch; break;
			case BLTU:f3=0x6; op=B_Branch; break;
			case BGEU:f3=0x7; op=B_Branch; break;
			default:
				xout[Fault]<<"Failed to init such B-Type INST "<<(void*)I<<endl;
				return Instruction(0);
		}
		return Instruction((BitSeq<1>(uimm,12)+BitSeq<6>(uimm,5)+BitSeq<5>(rs2)+BitSeq<5>(rs1)+BitSeq<3>(f3)+BitSeq<4>(uimm,1)+BitSeq<1>(uimm,11)+BitSeq<7>((Uint64)op)).zext());
	}
	
	template <INST I> static Instruction Create(int rd,int rs1,RegisterData uimm) requires(InThisSet(I,JALR))
	{
		using enum OpCode;
		RegisterData f3=0x0;
		OpCode op=J_JALR;
		return Instruction((BitSeq<12>(uimm)+BitSeq<5>(rs1)+BitSeq<3>(f3)+BitSeq<5>(rd)+BitSeq<7>((Uint64)op)).zext());
	}
	
	template <INST I> static Instruction Create() requires(InThisSet(I,ECALL,EBREAK))
	{
		using enum OpCode;
		if constexpr (I==ECALL)
			return Instruction((BitSeq<12>(0)+BitSeq<5>(0)+BitSeq<3>(0)+BitSeq<5>(0)+BitSeq<7>((Uint64)I_ECALL)).zext());
		else if constexpr (I==EBREAK)
			return Instruction((BitSeq<12>(1)+BitSeq<5>(0)+BitSeq<3>(0)+BitSeq<5>(0)+BitSeq<7>((Uint64)I_ECALL)).zext());
		else static_assert(InThisSet(I,ECALL,EBREAK));
	}
	
	template <INST I> static Instruction Create(int rd,RegisterData uimm) requires(InThisSet(I,JAL))
	{
		if constexpr (I==JAL)
			return Instruction((BitSeq<1>(uimm,20)+BitSeq<10>(uimm,1)+BitSeq<1>(uimm,11)+BitSeq<8>(uimm,12)+BitSeq<5>(rd)+BitSeq<7>((Uint64)OpCode::J_JAL)).zext());
		else static_assert(InThisSet(I,JAL));
	}
	
	template <INST I> static Instruction Create(int rd,RegisterData uimm) requires(InThisSet(I,LUI))
	{
		if constexpr (I==LUI)
			return Instruction((BitSeq<20>(uimm,12)+BitSeq<5>(rd)+BitSeq<7>((Uint64)OpCode::U_LUI)).zext());
		else static_assert(InThisSet(I,LUI));
	}
	
	template <INST I> static Instruction Create(int rd,RegisterData uimm) requires(InThisSet(I,AUIPC))
	{
		if constexpr (I==AUIPC)
			return Instruction((BitSeq<20>(uimm,12)+BitSeq<5>(rd)+BitSeq<7>((Uint64)OpCode::U_AUIPC)).zext());
		else static_assert(InThisSet(I,AUIPC));
	}
	
	Instruction(RegisterData i):inst(i) {}
	Instruction() {}
};

struct CompressedInstruction
{
	enum class InstType
	{
		Unknown=0,
		CR,
		CI,
		CSS,
		CIW,
		CL,
		CS,
		CB,
		CJ
	};
	
	enum INST
	{
		Unknown,
		LWSP,SWSP,LW,SW,LDSP,SDSP,LD,SD,
		J,JAL,JR,JALR,
		BEQZ,BNEZ,
		LI,LUI,
		ADDI,ADDI16SP,ADDI4SPN,ADDIW,
		SLLI,SRLI,SRAI,
		ANDI,
		MV,
		ADD,AND,OR,XOR,SUB,
		NOP,
		EBREAK
	};
	
	Uint64 inst=0;
	
	operator bool ()
	{return inst!=0;}
	
	int opcode()
	{return inst&0b11;}
	
	int funct3()
	{return inst>>13&0b111;}
	
	int funct4()
	{return inst>>12&0b1111;}
	
	int bit12()
	{return inst>>12&0b1;}
	
	int rs2()
	{return inst>>2&0b11111;}
	
	int rdrs1()
	{return inst>>7&0b11111;}
	
	int rs2_()
	{return (inst>>2&0b111)+8;}
	
	int rs1_()
	{return (inst>>7&0b111)+8;}
	
	int rd_inrs2_()
	{return (inst>>2&0b111)+8;}
	
	int rdrs1_()
	{return (inst>>7&0b111)+8;}
	
	Doublet <INST,SignedRegisterData> which()
	{
		using enum INST;
		switch (opcode())
		{
			case 0b00:
				switch (funct3())
				{
					case 0b000:
					{
						auto uimm=BitSeq<4>(inst,7)+BitSeq<2>(inst,11)+BitSeq<1>(inst,5)+BitSeq<1>(inst,6)+BitSeq<2>(0);
						if (uimm==0)
							return {Unknown,0};
						else return {ADDI4SPN,uimm.sext()};
					}
					case 0b010:
					{
						auto uimm=BitSeq<1>(inst,5)+BitSeq<3>(inst,10)+BitSeq<1>(inst,6)+BitSeq<2>(0);
						return {LW,uimm.zext()};
					}
					case 0b011:
					{
						auto uimm=BitSeq<2>(inst,5)+BitSeq<3>(inst,10)+BitSeq<3>(0);
						return {LD,uimm.zext()};
					}
					case 0b110:
					{
						auto uimm=BitSeq<1>(inst,5)+BitSeq<3>(inst,10)+BitSeq<1>(inst,6)+BitSeq<2>(0);
						return {SW,uimm.zext()};
					}
					case 0b111:
					{
						auto uimm=BitSeq<2>(inst,5)+BitSeq<3>(inst,10)+BitSeq<3>(0);
						return {SD,uimm.zext()};
					}
					default:	return {Unknown,0};
				}
			case 0b01:
				switch (funct3())
				{
					case 0b000:
					{
						auto uimm=BitSeq<1>(inst,12)+BitSeq<5>(inst,2);
						if (rdrs1()==0&&uimm==0)
							return {NOP,0};
						else return {ADDI,uimm.sext()};
					}
					case 0b010:	return {LI,(BitSeq<1>(inst,12)+BitSeq<5>(inst,2)).sext()};
					case 0b011:
						if (rdrs1()==2)//ADDI16SP
						{
							auto uimm=BitSeq<1>(inst,12)+BitSeq<2>(inst,3)+BitSeq<1>(inst,5)+BitSeq<1>(inst,2)+BitSeq<1>(inst,6)+BitSeq<4>(0);
							if (uimm==0)
								return {Unknown,0};
							else return {ADDI16SP,uimm.sext()};
						}
						else//LUI
						{
							auto uimm=BitSeq<1>(inst,12)+BitSeq<5>(inst,2)+BitSeq<12>(0);
							if (uimm==0)
								return {Unknown,0};
							else return {LUI,uimm.sext()};
						}
					case 0b100:
					{
						auto flag1=BitSeq<2>(inst,10);
						auto uimm1=BitSeq<1>(inst,12)+BitSeq<5>(inst,2);
						auto flag2=BitSeq<3>(inst,10)+BitSeq<2>(inst,5);
						if (flag1==0b00)
							return {SRLI,uimm1.zext()};
						else if (flag1==0b01)
							return {SRAI,uimm1.zext()};
						else if (flag1==0b10)
							return {ANDI,uimm1.sext()};
						else if (flag2==0b01111)
							return {AND,0};
						else if (flag2==0b01110)
							return {OR,0};
						else if (flag2==0b01101)
							return {XOR,0};
						else if (flag2==0b01100)
							return {SUB,0};
						else return {Unknown,0};
					}
					case 0b001:
						if constexpr (IsRV64)
						{
							auto uimm=BitSeq<1>(inst,12)+BitSeq<5>(inst,2);
							return {ADDIW,uimm.sext()};
						}
						else
						{
							auto uimm=BitSeq<1>(inst,12)+BitSeq<1>(inst,8)+BitSeq<2>(inst,9)+BitSeq<1>(inst,6)+BitSeq<1>(inst,7)+BitSeq<1>(inst,2)+BitSeq<1>(inst,11)+BitSeq<3>(inst,3)+BitSeq<1>(0);
							return {JAL,uimm.sext()};
						}
					case 0b101:
					{
						auto uimm=BitSeq<1>(inst,12)+BitSeq<1>(inst,8)+BitSeq<2>(inst,9)+BitSeq<1>(inst,6)+BitSeq<1>(inst,7)+BitSeq<1>(inst,2)+BitSeq<1>(inst,11)+BitSeq<3>(inst,3)+BitSeq<1>(0);
						return {J,uimm.sext()};
					}
					case 0b110:
					case 0b111:
					{
						auto uimm=BitSeq<1>(inst,12)+BitSeq<2>(inst,5)+BitSeq<1>(inst,2)+BitSeq<2>(inst,10)+BitSeq<2>(inst,3)+BitSeq<1>(0);
						return {funct3()==0b110?BEQZ:BNEZ,uimm.sext()};
					}
					default:	return {Unknown,0};
				}
			case 0b10:
				switch (funct3())
				{
					case 0b000:
					{
						auto uimm=BitSeq<1>(inst,12)+BitSeq<5>(inst,2);
						return {SLLI,uimm.zext()};
					}
					case 0b010:
					{
						auto uimm=BitSeq<2>(inst,2)+BitSeq<1>(inst,12)+BitSeq<3>(inst,4)+BitSeq<2>(0);
						if (rdrs1()==0)
							return {Unknown,0};
						else return {LWSP,uimm.zext()};
					}
					case 0b011:
					{
						auto uimm=BitSeq<3>(inst,2)+BitSeq<1>(inst,12)+BitSeq<2>(inst,5)+BitSeq<3>(0);
						if (rdrs1()==0)
							return {Unknown,0};
						else return {LDSP,uimm.zext()};
					}
					case 0b100:
						if (rs2()==0)
							if (rdrs1()==0)
								if (bit12()==1)
									return {EBREAK,0};
								else return {Unknown,0};
							else
								if (bit12()==1)
									return {JALR,0};
								else return {JR,0};
						else
							if (bit12()==1)
									if (rdrs1()==0)
										return {Unknown,0};
									else return {ADD,0};
							else return {MV,0};
					case 0b110:
					{
						auto uimm=BitSeq<2>(inst,7)+BitSeq<4>(inst,9)+BitSeq<2>(0);
						return {SWSP,uimm.zext()};
					}
					case 0b111:
					{
						auto uimm=BitSeq<3>(inst,7)+BitSeq<3>(inst,10)+BitSeq<3>(0);
						return {SDSP,uimm.zext()};
					}
					default:	return {Unknown,0};
				}
			default:	return {Unknown,0};
		}
	}
	
	Instruction Uncompress()
	{
		using enum INST;
		using I=Instruction;
		auto [wh,simm]=which();
		xout[Debug]<<"Simm "<<(void*)simm<<endl;
		switch (wh)
		{
			case LWSP:		return I::Create<I::LW>(rdrs1(),2,simm);
			case SWSP:		return I::Create<I::SW>(2,rs2(),simm);
			case LW:		return I::Create<I::LW>(rd_inrs2_(),rs1_(),simm);
			case SW:		return I::Create<I::SW>(rs1_(),rs2_(),simm);
			case LDSP:		return I::Create<I::LD>(rdrs1(),2,simm);
			case SDSP:		return I::Create<I::SD>(2,rs2(),simm);
			case LD:		return I::Create<I::LD>(rd_inrs2_(),rs1_(),simm);
			case SD:		return I::Create<I::SD>(rs1_(),rs2_(),simm);
			case J:			return I::Create<I::JAL>(0,simm);
			case JAL:		return I::Create<I::JAL>(1,simm);
			case JR:		return I::Create<I::JALR>(0,rdrs1(),0);
			case JALR:		return I::Create<I::JALR>(1,rdrs1(),0);
			case BEQZ:		return I::Create<I::BEQ>(rs1_(),0,simm);
			case BNEZ:		return I::Create<I::BNE>(rs1_(),0,simm);
			case LI:		return I::Create<I::ADDI>(rdrs1(),0,simm);
			case LUI:		return I::Create<I::LUI>(rdrs1(),simm);
			case NOP:		return I::Create<I::ADDI>(0,0,0);
			case ADDI:		return I::Create<I::ADDI>(rdrs1(),rdrs1(),simm);
			case ADDI16SP:	return I::Create<I::ADDI>(2,2,simm);
			case ADDI4SPN:	return I::Create<I::ADDI>(rd_inrs2_(),2,simm);
			case ADDIW:		return I::Create<I::ADDIW>(rdrs1(),rdrs1(),simm);
			case SLLI:		return I::Create<I::SLLI>(rdrs1(),rdrs1(),simm);
			case SRLI:		return I::Create<I::SRLI>(rdrs1_(),rdrs1_(),simm);
			case SRAI:		return I::Create<I::SRAI>(rdrs1_(),rdrs1_(),simm);
			case ANDI:		return I::Create<I::ANDI>(rdrs1_(),rdrs1_(),simm);
			case MV:		return I::Create<I::ADD>(rdrs1(),0,rs2());
			case ADD:		return I::Create<I::ADD>(rdrs1(),rdrs1(),rs2());
			case AND:		return I::Create<I::AND>(rdrs1_(),rdrs1_(),rs2_());
			case OR:		return I::Create<I::OR>(rdrs1_(),rdrs1_(),rs2_());
			case XOR:		return I::Create<I::XOR>(rdrs1_(),rdrs1_(),rs2_());
			case SUB:		return I::Create<I::SUB>(rdrs1_(),rdrs1_(),rs2_());
			case EBREAK:	return I::Create<I::EBREAK>();
			default:		return I(0);
		}
	}
};

class ProgramContext
{
	public:
		struct ExceptionContext
		{
			ProgramContext *This=nullptr;
			
		};
	
	protected:
		GeneralPurposeRegisters GPRs;
		RegisterData PC;//Program counter
		PhysicalMemory Mem;
		
		string CurrentLine;
		void Putchar(char ch)
		{
			xout[Test]<<"Syscall_Putchar "<<(void*)ch<<endline;
			if (InRange(ch,32,126))
			{
				CurrentLine+=ch;
				xout<<Tab4<<"char: "<<ch<<endline;
			}
			else if (ch=='\n')
				xout<<Tab4<<"endl"<<endline;
			xout<<Tab4<<"Current line ["<<CurrentLine<<"]"<<endl;
			if (ch=='\n')
				CurrentLine="";
			system("pause");
		}
		
		void PrintRegs()
		{
			xout[Debug]<<"Regs:"<<endline
					   <<" sp:"<<(void*)GPRs.sp<<" ra:"<<(void*)GPRs.ra<<endline
					   <<" s0:"<<(void*)GPRs.s0<<" s1:"<<(void*)GPRs.s1<<" s2:"<<GPRs.s2<<endline
					   <<" a0:"<<(void*)GPRs.a0<<" a1:"<<(void*)GPRs.a1<<" a2:"<<(void*)GPRs.a2<<" a3:"<<(void*)GPRs.a3<<endline
					   <<" a4:"<<(void*)GPRs.a4<<" a5:"<<(void*)GPRs.a5<<" a6:"<<(void*)GPRs.a6<<" a7:"<<(void*)GPRs.a7<<endline
					   <<endl;
		}
		
		Doublet <Instruction,int> Fetch()
		{
			Instruction I;
			Mem.Load<4,false>(PC,I.inst);
			xout[RVDD]<<"-----------------------------------"<<endl;
			xout[RVDD]<<"Fetch instruction at "<<(void*)PC<<endline;
			if ((I.inst&0b11)!=0b11)
			{
				CompressedInstruction CI;
				CI.inst=I.inst&0xFFFF;
				I=CI.Uncompress();
				xout[RVDD]<<"Uncompress instruction: "<<(void*)CI.inst<<" "<<(void*)I.inst<<endl;
				I.DebugPrint();
				return {I,2};
			}
			else
			{
				xout[RVDD]<<"Instruction: "<<(void*)I.inst<<endl;
				I.DebugPrint();
				return {I,4};
			}
		}
		
	public:
		
		void FillMemoryRegion(Uint64 p,DataWithSize data)
		{
			for (auto x:data)
				Mem[p++]=x;
		}
		
		void Run(const RegisterData &InitPC,const RegisterData &InitSP)
		{
			PC=InitPC;
			GPRs.sp=InitSP;
			while (true)
			{
				auto [I,deltaPC]=Fetch();
				if (!I)
					break;
				using enum Instruction::INST;
				RegisterData rs1=GPRs[I.rs1()],
							 rs2=GPRs[I.rs2()],
							 rd=GPRs[I.rd()],
							 uimm=I.uimm();
				SignedRegisterData simm=I.simm();
				switch (I.which())
				{
					case ADD:	rd=rs1+rs2;	break;
					case SUB:	rd=rs1-rs2;	break;
					case XOR:	rd=rs1^rs2;	break;
					case OR:	rd=rs1|rs2;	break;
					case AND:	rd=rs1&rs2;	break;
					case SLL:	rd=rs1<<(rs2&(1<<RegisterRank)-1);break;
					case SRL:	rd=rs1>>(rs2&(1<<RegisterRank)-1);break;
					case SRA:
					{
						int x=rs2&(1<<RegisterRank)-1;
						if (rs1&(1<<sizeof(RegisterData)*8-1))
							rd=rs1>>x|~0ull<<sizeof(RegisterData)*8-x;
						else rd=rs1>>x;
						break;
					}
					case SLT:	rd=(SignedRegisterData)rs1<(SignedRegisterData)rs2?1:0;	break; 
					case SLTU:	rd=rs1<rs2?1:0;	break;
					case ADDW:	rd=SignedExtends<32>(Truncate<32>(rs1+rs2));break;//??
					case SUBW:	rd=SignedExtends<32>(Truncate<32>(rs1-rs2));break;
					case SLLW:	rd=SignedExtends<32>(Truncate<32>(rs1<<(rs2&0b11111)));	break;
					case SRLW:	rd=SignedExtends<32>(Truncate<32>(rs1)>>(rs2&0b11111));	break;
					case SRAW://??
					{
						int x=rs2&0b11111;
						RegisterData y=Truncate<32>(rs1);
						if (y&1<<31)
							rd=y>>x|~0ull<<64-x;
						else rd=SignedExtends<32>(y>>x);
						break;
					}
//					case MUL:	rd=rs1*rs2
//					case MULH:
//					case MULSU:
//					case MULU:
//					case DIV:
//					case DIVU:
//					case REM:
//					case REMU:
					//	ADD,SUB,XOR,OR,AND,SLL,SRL,SRA,SLT,SLTU,ADDW,SUBW,SLLW,SRLW,SRAW,
					//	MUL,MULH,MULSU,MULU,DIV,DIVU,REM,REMU
					case ADDI:	rd=rs1+simm;break;
					case XORI:	rd=rs1^simm;break;
					case ORI:	rd=rs1|simm;break;
					case ANDI:	rd=rs1&simm;break;
					case SLLI:
						if (!(uimm&1<<RegisterRank))
							rd=rs1<<(uimm&(1<<RegisterRank)-1);
						break;
					case SRLI:
						if (!(uimm&1<<RegisterRank))
							rd=rs1>>(uimm&(1<<RegisterRank)-1);
						break;
					case SRAI:
					{
						int x=uimm&(1<<RegisterRank)-1;
						if (rs1&(1<<sizeof(RegisterData)*8-1))
							rd=rs1>>x|~0ull<<sizeof(RegisterData)*8-x;
						else rd=rs1>>x;
						break;
					}
					case SLTI:	rd=(SignedRegisterData)rs1<simm?1:0;	break;
					case SLTIU:	rd=rs1<(RegisterData)simm?1:0;	break;
					case ADDIW:	rd=SignedExtends<32>(Truncate<32>(rs1+simm));	break;
					case SLLIW:	rd=SignedExtends<32>(Truncate<32>(rs1<<(uimm&0b11111)));	break;
					case SRLIW:	rd=SignedExtends<32>(Truncate<32>(rs1>>(uimm&0b11111)));	break;
					case SRAIW:
					{
						int x=uimm&0b11111;
						RegisterData y=Truncate<32>(rs1);
						if (y&1<<31)
							rd=y>>x|~0ull<<64-x;
						else rd=SignedExtends<32>(y>>x);
						break;
					}
					//	ADDI,XORI,ORI,ANDI,SLLI,SRLI,SRAI,SLTI,SLTIU,ADDIW,SLLIW,SRLIW,SRAIW,
					case LB: case LH: case LW: case LD: case LBU: case LHU: case LWU: case SB: case SH: case SW: case SD:
					{
						Uint64 p=(SignedRegisterData)rs1+simm;
						switch (I.which())
						{
							case LB:	Mem.Load<1,true> (p,rd);break;
							case LH:	Mem.Load<2,true> (p,rd);break;
							case LW:	Mem.Load<4,true> (p,rd);break;
							case LD:	Mem.Load<8,false>(p,rd);break;
							case LBU:	Mem.Load<1,false>(p,rd);break;
							case LHU:	Mem.Load<2,false>(p,rd);break;
							case LWU:	Mem.Load<4,false>(p,rd);break;
							case SB:	Mem.Store<1>(p,rs2);	break;
							case SH:	Mem.Store<2>(p,rs2);	break;
							case SW:	Mem.Store<4>(p,rs2);	break;
							case SD:	Mem.Store<8>(p,rs2);	break;
						}
						break;
					}
					case BEQ:	if (rs1==rs2) deltaPC=simm;	break;
					case BNE:	if (rs1!=rs2) deltaPC=simm;	break;
					case BLT:	if ((SignedRegisterData)rs1<(SignedRegisterData)rs2)  deltaPC=simm;	break;
					case BGE:	if ((SignedRegisterData)rs1>=(SignedRegisterData)rs2) deltaPC=simm;	break;
					case BLTU:	if (rs1<rs2)  deltaPC=simm;	break;
					case BGEU:	if (rs1>=rs2) deltaPC=simm;	break;
					//	BEQ,BNE,BLT,BGE,BLTU,BGEU
					case JAL:
						rd=PC+4;
						PC+=simm;
						break;
					case JALR:
					{
						RegisterData t=PC+4;
						PC=(rs1+simm)&~1ull;
						rd=t;
						break;
					}
					case LUI:
						rd=simm;
						break;
					case AUIPC:
						rd=(SignedRegisterData)PC+(SignedRegisterData)simm;
						break;
					case ECALL:
					case EBREAK:
//						xout[Fault]<<"ecall/ebreak"<<endl;
						xout[Test]<<"ecall/ebreak"<<endline
								  <<Tab4<<"a0:"<<(void*)GPRs.a0<<endline
								  <<Tab4<<"a1:"<<(void*)GPRs.a1<<endline
								  <<Tab4<<"a2:"<<(void*)GPRs.a2<<endline
								  <<Tab4<<"a3:"<<(void*)GPRs.a3<<endline
								  <<Tab4<<"a4:"<<(void*)GPRs.a4<<endline
								  <<Tab4<<"a5:"<<(void*)GPRs.a5<<endline
								  <<Tab4<<"a7:"<<(void*)GPRs.a7<<endline;
						if (GPRs.a7==(RegisterData)-1ll)//Putchar
							Putchar(GPRs.a0);
						else if (GPRs.a7==(RegisterData)-4ll)
						{
							xout[Test]<<"Syscall_Exit"<<endl;
							return;
						}
						xout<<endl;
						break;
					//	JAL,JALR,
					//	LUI,AUIPC,
					//	ECALL,EBREAK,
					case Unknown:
					default:
						RVDD.Enable();
						I.DebugPrint();
						xout[Fault]<<"Found unknown instruction "<<(void*)I.inst<<endl;
						break;
				}
				if (I.rd()!=0)
					GPRs[I.rd()]=rd;
				PrintRegs();
				if (InThisSet(I.which(),JAL,JALR))
					DoNothing;
				else PC+=deltaPC;
			}
		}
		
};

int main(int argc,char **argv)
{
	XoutType::SetFaultSolver([]()
	{
		system("pause");
		exit(-1);
	});
	
	RVDD.Disable();
	Debug.Disable();
//	if (argc!=2)
//		return 1;
//	const string file=argv[1];
	const string file="Test/Hello.img";
//	const string file="Test/Count1-100.img";
	
	ProgramContext ctx;
	Legacy::CFileIO cio(file,Legacy::CFileIO::OpenMode_ReadOnly);
	SizedBuffer buffer(cio.Size());
	assert(buffer.data);
	cio.Read(buffer.data,buffer.size);
	ctx.FillMemoryRegion(0x800020,buffer);
	ctx.Run(0x800020,0x80000000);
	return 0;
}
