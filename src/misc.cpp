/*
  Nanoha, a USI shogi(japanese-chess) playing engine derived from Stockfish, a UCI chess playing engine.
  Copyright (C) 2004-2008 Tord Romstad (Glaurung author)
  Copyright (C) 2008-2015 Marco Costalba, Joona Kiiski, Tord Romstad
  Copyright (C) 2015-2017 Marco Costalba, Joona Kiiski, Gary Linscott, Tord Romstad
  Copyright (C) 2014-2018 Kazuyuki Kawabata

  Nanoha is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Nanoha is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifdef _WIN32
#if _WIN32_WINNT < 0x0601
#undef  _WIN32_WINNT
#define _WIN32_WINNT 0x0601 // Force to include needed API prototypes
#endif
#include <windows.h>
// The needed Windows API for processor groups could be missed from old Windows
// versions, so instead of calling them directly (forcing the linker to resolve
// the calls at compile time), try to load them at runtime. To do this we need
// first to define the corresponding function pointers.
extern "C" {
typedef bool(*fun1_t)(LOGICAL_PROCESSOR_RELATIONSHIP,
                      PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX, PDWORD);
typedef bool(*fun2_t)(USHORT, PGROUP_AFFINITY);
typedef bool(*fun3_t)(HANDLE, CONST GROUP_AFFINITY*, PGROUP_AFFINITY);
}
#endif

#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <vector>

#include "misc.h"
#include "thread.h"

using namespace std;

namespace {

#if defined(NANOHA)
#if defined(EVAL_NANO)
const string DefaultAppName = "NanohaNano";
#elif defined(EVAL_MINI)
const string DefaultAppName = "NanohaMini";
#elif defined(EVAL_APERY)
const string DefaultAppName = "NanohaWCSC28A";
#elif defined(EVAL_TWIG)
const string DefaultAppName = "NanohaWCSC28U";
#else
#error EVAL Type
#endif
const string DefaultEngineVersion = "0.8.2";

string AppName = DefaultAppName;
string EngineVersion = DefaultEngineVersion;
#endif

/// Version number. If Version is left empty, then compile date in the format
/// DD-MM-YY and show in engine_info.
const string Version = "";

/// Our fancy logging facility. The trick here is to replace cin.rdbuf() and
/// cout.rdbuf() with two Tie objects that tie cin and cout to a file stream. We
/// can toggle the logging of std::cout and std:cin at runtime whilst preserving
/// usual I/O functionality, all without changing a single line of code!
/// Idea from http://groups.google.com/group/comp.lang.c++/msg/1d941c0f26ea0d81

struct Tie: public streambuf { // MSVC requires split streambuf for cin and cout

  Tie(streambuf* b, streambuf* l) : buf(b), logBuf(l) {}

  int sync() { return logBuf->pubsync(), buf->pubsync(); }
  int overflow(int c) { return log(buf->sputc((char)c), "<< "); }
  int underflow() { return buf->sgetc(); }
  int uflow() { return log(buf->sbumpc(), ">> "); }

  streambuf *buf, *logBuf;

  int log(int c, const char* prefix) {

    static int last = '\n'; // Single log file

    if (last == '\n')
        logBuf->sputn(prefix, 3);

    return last = logBuf->sputc((char)c);
  }
};

class Logger {

  Logger() : in(cin.rdbuf(), file.rdbuf()), out(cout.rdbuf(), file.rdbuf()) {}
 ~Logger() { start(""); }

  ofstream file;
  Tie in, out;

public:
  static void start(const std::string& fname) {

    static Logger l;

    if (!fname.empty() && !l.file.is_open())
    {
        l.file.open(fname, ifstream::out);
        cin.rdbuf(&l.in);
        cout.rdbuf(&l.out);
    }
    else if (fname.empty() && l.file.is_open())
    {
        cout.rdbuf(l.out.buf);
        cin.rdbuf(l.in.buf);
        l.file.close();
    }
  }
};

} // namespace

#if defined(NANOHA)
// 同じバイナリを使って、違うエンジン名で使いたい時があるため、
// prog名.nameを読み込んで、AppNameとEngineVersionにセットする.
// 読み込めない時はデフォルト(コンパイル時のセット内容)になる.
void load_name(const char *prog)
{
	std::string s = prog;
	std::string::size_type l;

	// プログラム名を取り出す.
	l = s.find_last_of("/\\");
	if (l != std::string::npos) {
		s.erase(0, l+1);
	}
	// .exeを消去する.
	l = s.rfind(".exe");
	if (l != std::string::npos) {
		s = s.substr(0, l);
	} else {
		l = s.rfind(".EXE");
		if (l != std::string::npos) {
			s = s.substr(0, l);
		}
	}
	s += ".name";

	std::ifstream file(s);

	if (file.is_open()) {
		std::string str;
		// AppName設定
		if (getline(file, str)) {
			if (!str.empty()) {
				// Cygwin使った時に、\rが入ってしまう場合があるため、その対応.
				l = str.find_last_of("\r\n");
				if (l != std::string::npos) {
					str.erase(l, l+1);
				}
				AppName = str;
			}
		}
		// EngineVersion設定
		if (getline(file, str)) {
			if (!str.empty()) {
				// Cygwin使った時に、\rが入ってしまう場合があるため、その対応.
				l = str.find_last_of("\r\n");
				if (l != std::string::npos) {
					str.erase(l, l+1);
				}
				EngineVersion = str;
			}
		}
	}
#if 0
	std::cout << "prog:[" << s << ']' << std::endl;
	std::cout << "name:[" << AppName << ']' << std::endl;
	std::cout << "ver.:[" << EngineVersion << ']' << std::endl;
#endif
}
#endif


/// engine_info() returns the full name of the current Stockfish version. This
/// will be either "Stockfish <Tag> DD-MM-YY" (where DD-MM-YY is the date when
/// the program was compiled) or "Stockfish <Version>", depending on whether
/// Version is empty.

const string engine_info(bool to_uci) {

  const string months("Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec");
  string month, day, year;
  stringstream ss, date(__DATE__); // From compiler, format is "Sep 21 2008"

#if defined(NANOHA)
	const string cpu64(Is64Bit ? " 64bit" : "");
#if defined(NDEBUG)
	const string release("(Release)");
#else
	const string release("(debug)");
#endif
	if (!EngineVersion.empty()) {
		return AppName + " " + EngineVersion + cpu64 + "\nid author Kawabata, Kazuyuki";
	}

	ss << AppName << setfill('0');

	date >> month >> day >> year;
	ss << setw(2) << day << setw(2) << (1 + months.find(month) / 4) << year.substr(2);

	ss << (Is64Bit ? " 64" : "")
	   << (HasPext ? " BMI2" : (HasPopCnt ? " POPCNT" : ""))
	   << (to_uci  ? "\nid author ": " by ")
	   << "Kawabata, Kazuyuki";
#else
  ss << "Stockfish " << Version << setfill('0');

  if (Version.empty())
  {
      date >> month >> day >> year;
      ss << setw(2) << day << setw(2) << (1 + months.find(month) / 4) << year.substr(2);
  }

  ss << (Is64Bit ? " 64" : "")
     << (HasPext ? " BMI2" : (HasPopCnt ? " POPCNT" : ""))
     << (to_uci  ? "\nid author ": " by ")
     << "T. Romstad, M. Costalba, J. Kiiski, G. Linscott";
#endif

  return ss.str();
}


/// Debug functions used mainly to collect run-time statistics
static int64_t hits[2], means[2];

void dbg_hit_on(bool b) { ++hits[0]; if (b) ++hits[1]; }
void dbg_hit_on(bool c, bool b) { if (c) dbg_hit_on(b); }
void dbg_mean_of(int v) { ++means[0]; means[1] += v; }

void dbg_print() {

  if (hits[0])
      cerr << "Total " << hits[0] << " Hits " << hits[1]
           << " hit rate (%) " << 100 * hits[1] / hits[0] << endl;

  if (means[0])
      cerr << "Total " << means[0] << " Mean "
           << (double)means[1] / means[0] << endl;
}


/// Used to serialize access to std::cout to avoid multiple threads writing at
/// the same time.

std::ostream& operator<<(std::ostream& os, SyncCout sc) {

  static Mutex m;

  if (sc == IO_LOCK)
      m.lock();

  if (sc == IO_UNLOCK)
      m.unlock();

  return os;
}


/// Trampoline helper to avoid moving Logger to misc.h
void start_logger(const std::string& fname) { Logger::start(fname); }


/// prefetch() preloads the given address in L1/L2 cache. This is a non-blocking
/// function that doesn't stall the CPU waiting for data to be loaded from memory,
/// which can be quite slow.
#ifdef NO_PREFETCH

void prefetch(void*) {}

#else

void prefetch(void* addr) {

#  if defined(__INTEL_COMPILER)
   // This hack prevents prefetches from being optimized away by
   // Intel compiler. Both MSVC and gcc seem not be affected by this.
   __asm__ ("");
#  endif

#  if defined(__INTEL_COMPILER) || defined(_MSC_VER)
  _mm_prefetch((char*)addr, _MM_HINT_T0);
#  else
  __builtin_prefetch(addr);
#  endif
}

#endif

void prefetch2(void* addr) {

    prefetch(addr);
    prefetch((uint8_t*)addr + 64);
}

namespace WinProcGroup {

#ifndef _WIN32

void bindThisThread(size_t) {}

#else

/// get_group() retrieves logical processor information using Windows specific
/// API and returns the best group id for the thread with index idx. Original
/// code from Texel by Peter Osterlund.

int get_group(size_t idx) {

  int threads = 0;
  int nodes = 0;
  int cores = 0;
  DWORD returnLength = 0;
  DWORD byteOffset = 0;

  // Early exit if the needed API is not available at runtime
  HMODULE k32 = GetModuleHandle("Kernel32.dll");
  auto fun1 = (fun1_t)GetProcAddress(k32, "GetLogicalProcessorInformationEx");
  if (!fun1)
      return -1;

  // First call to get returnLength. We expect it to fail due to null buffer
  if (fun1(RelationAll, nullptr, &returnLength))
      return -1;

  // Once we know returnLength, allocate the buffer
  SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX *buffer, *ptr;
  ptr = buffer = (SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX*)malloc(returnLength);

  // Second call, now we expect to succeed
  if (!fun1(RelationAll, buffer, &returnLength))
  {
      free(buffer);
      return -1;
  }

  while (ptr->Size > 0 && byteOffset + ptr->Size <= returnLength)
  {
      if (ptr->Relationship == RelationNumaNode)
          nodes++;

      else if (ptr->Relationship == RelationProcessorCore)
      {
          cores++;
          threads += (ptr->Processor.Flags == LTP_PC_SMT) ? 2 : 1;
      }

      byteOffset += ptr->Size;
      ptr = (SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX*)(((char*)ptr) + ptr->Size);
  }

  free(buffer);

  std::vector<int> groups;

  // Run as many threads as possible on the same node until core limit is
  // reached, then move on filling the next node.
  for (int n = 0; n < nodes; n++)
      for (int i = 0; i < cores / nodes; i++)
          groups.push_back(n);

  // In case a core has more than one logical processor (we assume 2) and we
  // have still threads to allocate, then spread them evenly across available
  // nodes.
  for (int t = 0; t < threads - cores; t++)
      groups.push_back(t % nodes);

  // If we still have more threads than the total number of logical processors
  // then return -1 and let the OS to decide what to do.
  return idx < groups.size() ? groups[idx] : -1;
}


/// bindThisThread() set the group affinity of the current thread

void bindThisThread(size_t idx) {

  // If OS already scheduled us on a different group than 0 then don't overwrite
  // the choice, eventually we are one of many one-threaded processes running on
  // some Windows NUMA hardware, for instance in fishtest. To make it simple,
  // just check if running threads are below a threshold, in this case all this
  // NUMA machinery is not needed.
  if (Threads.size() < 8)
      return;

  // Use only local variables to be thread-safe
  int group = get_group(idx);

  if (group == -1)
      return;

  // Early exit if the needed API are not available at runtime
  HMODULE k32 = GetModuleHandle("Kernel32.dll");
  auto fun2 = (fun2_t)GetProcAddress(k32, "GetNumaNodeProcessorMaskEx");
  auto fun3 = (fun3_t)GetProcAddress(k32, "SetThreadGroupAffinity");

  if (!fun2 || !fun3)
      return;

  GROUP_AFFINITY affinity;
  if (fun2(group, &affinity))
      fun3(GetCurrentThread(), &affinity, nullptr);
}

#endif

} // namespace WinProcGroup

#if defined(NANOHA)
namespace {
#if defined(__GNUC__)
	// 8C5C はCodePage932(シフトJIS)での文字コードなので、文字コードを変更する場合は要変更.
	static const char *piece_ja_str[] = {
		"　", "歩", "香", "桂",       "銀", "金", "角", "飛", 
		"玉", "と", "杏", "\x8C\x5C", "全", "〓", "馬", "龍",
	};
#else
	static const char *piece_ja_str[] = {
		"　", "歩", "香", "桂", "銀", "金", "角", "飛", 
		"玉", "と", "杏", "圭", "全", "〓", "馬", "龍", 
	};
#endif
}

const std::string move_to_csa(Move m)
{
	if (is_none(m)) {
		return std::string("(xxxx)");
	}
	if (equal_move(m, MOVE_NULL)) {
		return std::string("(NULL)");
	}
	if (is_win(m)) {
		return std::string("(win)");
	}

	const Piece p = piece_of(m);
	const int from = from_sq(m);
	const int to = to_sq(m);
	const Color c = color_of(p);
	char buf[128];
	static const char *pieceStr[] = {
		"..", "FU", "KY", "KE", "GI", "KI", "KA", "HI",
		"OU", "TO", "NY", "NK", "NG", "--", "UM", "RY",
	};

	string str = (c == BLACK) ? "+" : "-";
	if (is_drop(m)) {
		snprintf(buf, sizeof(buf), "00%02X%s", to, pieceStr[type_of(p)]);
		str += buf;
	} else {
		if (is_promotion(m)) {
			snprintf(buf, sizeof(buf), "%02X%02X%s", from, to, pieceStr[type_of(p) | PROMOTED]);
		} else {
			snprintf(buf, sizeof(buf), "%02X%02X%s", from, to, pieceStr[type_of(p)]);
		}
		str += buf;
	}
	return str;
}

const std::string move_to_kif(Move m)
{
	const PieceType p = ptype_of(m);
	const int from = from_sq(m);
	const int to = to_sq(m);
	char buf[16];

	// "76歩(77)　"	；移動
	// "22角(88)成"	；移動＋成
	// "24歩打　　"	；駒打ち
	// "(null)　　"	；MOVE_NULL
	if (is_none(m)) {
		// 駒打ち
		strcpy(buf, "(null)　　");
	}else if (is_drop(m)) {
		// 駒打ち
		sprintf(buf, "%02x%s打　　", to, piece_ja_str[p]);
	} else {
		// 移動
		if (is_promotion(m)) {
			sprintf(buf, "%02x%s成(%02x)", to, piece_ja_str[p], from);
		} else {
			sprintf(buf, "%02x%s(%02x)　", to, piece_ja_str[p], from);
		}
	}
	string str = buf;
	return str;
}

void move_fprint(FILE *fp, Move m, int rotate)
{
	if (fp == NULL) return;
	if (is_none(m)) {
		foutput_log(fp, "パス　　　");
	}
	int to = to_sq(m);
	int from = from_sq(m);
	int rto = to;
	int rfrom = from;
	int x, y;
	switch (rotate) {
	case 0:	// 入換えなし
		break;
	case 2:	// 左右入換え
		x = to & 0xF0;
		y = to & 0x0F;
		rto = (0xA0 - x) + y;
		x = from & 0xF0;
		y = from & 0x0F;
		rfrom = (0xA0 - x) + y;
		break;
	case 1:	// 左右入換え＋上下入換え
		x = to & 0xF0;
		y = to & 0x0F;
		rto = (0xA0 - x) + (10 - y);
		x = from & 0xF0;
		y = from & 0x0F;
		rfrom = (0xA0 - x) + (10 - y);
		break;
	case 3:	// 上下入換え
		x = to & 0xF0;
		y = to & 0x0F;
		rto = x + (10 - y);
		x = from & 0xF0;
		y = from & 0x0F;
		rfrom = x + (10 - y);
		break;
	default:
		break;
	}
	foutput_log(fp, "%02x", rto);
	foutput_log(fp, "%2s", piece_ja_str[ptype_of(m)]);
	if (is_promotion(m)) {
		foutput_log(fp, "成");
	}
	if (from<OU) {
		foutput_log(fp, "打　");
	} else {
		foutput_log(fp, "(%02x)", rfrom);
	}
	if (!is_promotion(m)) {
		foutput_log(fp, "　");
	}
}

void move_print(Move m, int rotate)
{
	move_fprint(stdout, m, rotate);
}

#endif
