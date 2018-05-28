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

#include <fstream>
#include <iostream>
#include <istream>
#include <vector>
#if defined(NANOHA)
#include <iomanip>
#endif

#include "misc.h"
#include "position.h"
#include "search.h"
#include "thread.h"
#include "uci.h"
#if defined(NANOHA)
#include "evaluate.h"
#if defined(USE_DFPN)
#include "SearchMateDFPN.h"
#endif
#endif

using namespace std;

namespace {

#if defined(NANOHA)
// 指し手生成；指し手生成祭り局面と初期局面
const vector<string> GenMoves = {
	"l6nl/5+P1gk/2np1S3/p1p4Pp/3P2Sp1/1PPb2P1P/P5GS1/R8/LN4bKL w RGgsn5p 1",
	"lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1"
};
const vector<string> EvalPos = {
	// 初期局面.
	"lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1",
	// 歩を突いた局面の左右反転と先後反転させたもので対称性を確認する.
	"lnsgkgsnl/1r5b1/ppppppppp/9/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL b - 1",
	"lnsgkgsnl/1b5r1/ppppppppp/9/9/6P2/PPPPPP1PP/1R5B1/LNSGKGSNL b - 1",
	"lnsgkgsnl/1r5b1/ppppppppp/9/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL w - 1",
	"lnsgkgsnl/1b5r1/ppppppppp/9/9/6P2/PPPPPP1PP/1R5B1/LNSGKGSNL w - 1",
	// 対称の確認.
	//   点対称
	"+l+n+sgkgsnl/1+r5b1/+pp+pp+ppppp/9/9/9/PPPP+PP+PP+P/1B5+R1/LNSGKG+S+N+L b - 1",
	//   左右対称
	"lnsgkg+s+n+l/1+b5r1/+pp+pp+pp+ppp/9/9/9/+PP+PP+PP+PPP/1+B5R1/LNSGKG+S+N+L b - 1",
	//   上下反転と左右反転
	"l6nl/5+P1gk/2np1S3/p1p4Pp/3P2Sp1/1PPb2P1P/P5GS1/R8/LN4bKL w RGgsn5p 1",
	"ln4Bkl/r8/p5gs1/1ppB2p1p/3p2sP1/P1P4pP/2NP1s3/5+p1GK/L6NL w GSN5Prg 1",
	"lkB4nl/8r/1sg5p/p1p2Bpp1/1Ps2p3/Pp4P1P/3s1PN2/KG1+p5/LN6L w GSN5Prg 1"
};
// 1手詰め；詰む局面10＋詰まない局面10
const vector<string> PositionForMate1 = {
	"lr5nk/3+P2gg1/1p2+Np1p1/p1p3K2/5P1P1/4P4/1P2S1+b+l1/2GR5/4LS3 w 7P2NSBplsg 1",
	"+R8/2g1+P4/1g1g4p/p1pp2p2/4ps2P/P2S1nP2/1k1PPK3/8L/LNS1G1r2 w PNS2B5p2ln 1",
	"ln2b3+R/1sk1g4/ppppL4/8p/2PN2PK1/7PP/P3PP3/6+r2/5g1NL w 6PNSGl2sgb 1",
	"l6n1/6k2/4p2p1/s1K1nPs2/p1P1sn2l/3r4p/PP5P1/8P/L3G2NL w 3P2G2BR5psg 1",
	"l2+R1g3/5s1+B1/ppp4p1/1k1B1N3/3p2GP1/PN7/N3SP1g1/1NS2KP+l1/L1r2G3 b 2PL7ps 1",
	"ln3R2l/1skgg2g1/1pppp3b/p4K2p/3N1SP2/PP1P+sP1PP/B1R1+n4/9/L6NL w 4PSpg 1",
	"l1S2k1nl/4g1g2/g1+S4p1/p1S1p1P1p/1KP4R1/P2+bP4/8P/2+r6/1+b5NL w 6PSG3pl2n 1",
	"3k3g1/1+B1b1s3/2LG1pLpp/pKS1p1p2/1PP4N1/P+r4P2/2+n2P2P/9/L6NL b 5PNGRp2sg 1",
	"l2g2p2/5s3/2sSp3+b/pp4+B1p/3k3P1/P2pP1P1P/1PL6/K2G5/LN6L b 4P2NG2pnsg2r 1",
	"ln7/2s+B1Bl2/pppp3p1/4n1s2/4G2n1/1PPPP1k2/P3KP1PP/4G3r/LN3+s3 b 4PSpl2gr 1",

	"l4g1nl/2+R2p3/5+S1Lk/p1p3psp/1p2+bP1K1/P5P1P/4SG1P1/3P3s1/1+r5NL b P2G5p2nb 1",
	"l1S5l/2s1g1+P2/4p3p/pp2b1+R2/1npP1p3/1N1+B2P2/PP2kP2P/1KSNs+r3/L2G3N1 w 4PLGpg 1",
	"+R6nl/6kb1/4g4/p1p1Ppp2/5PgKp/P1P6/1P3SNP1/6SN1/5Sb2 b 5PS2GR2p3ln 1",
	"l7+R/6s2/5gskp/2+B1ppp2/p7P/1P1nP4/P1P3NLL/1KG1+b4/LN7 w S8pns2gr 1",
	"+R1nk3nl/1p7/p+b1SG2p1/3S1p3/1b4p1p/5G3/P5PPP/2+r2GSK1/L4G1NL w PL7pns 1",
	"l6S1/3+R4G/p4p1pp/2pP2p2/5B3/2kb1Pn2/P+p4NPP/3+rPgK1L/2N2G3 b P2SG4p2lns 1",
	"l5k2/6sn1/2+P3n1K/p2pgpS1g/1pB1p1p2/P4P1P1/1PN3P2/6G1N/L6+rS b 5PLRplsgb 1",
	"l1S5l/4gs3/3ppp3/ppRk3rp/3N5/PN4P1P/1P2PPSP1/4GGK2/+b7L w 2PG3pl2nsb 1",
	"1lB2p3/k4+R3/2S1G1nK1/1Pp1pSpG1/p8/2bpPP2n/P5P+n1/L3LG1g1/1+r3S1N1 w 4PL3ps 1",
	"l3g1p2/4k1g2/p1+N1pp3/2Bp1s1pK/5N3/1B3PP+r1/PP2P3P/3G2ss1/8L w 5P2LNSGpnr 1"
};
const vector<string> SEEPos = {
	// 「YSS 7.0」の「2.2 仮評価」の局面.
	//  先手番だと玉が取れる状態にあるので、玉を32に移動
	"1n5nl/1r1sg1k2/1l5pp/p1gppBp2/1pp6/S2P5/P1NBP1PPP/1R7/L2GKGSNL b 2PS2p - 1",
};
#endif

const vector<string> Defaults = {
#if defined(NANOHA)
	// 局面数を 16 にする
	// 進歩本2の棋力判定問題のNo.1 - No.16
	"lR1B3nl/2gp5/ngk1+BspPp/1s2p2p1/p4S3/1Pp6/P5P1P/LGG6/KN5NL b Prs5p 1",
	"5S2l/1rP2s1k1/p2+B1gnp1/5np2/3G3n1/5S2p/P1+p1PpPP1/1P1PG2KP/L2+rLPGNL b Bs3p 1",
	"lR6l/1s1g5/1k1s1+P2p/1+bpp1+Bs2/1n1n2Pp1/2P6/S2R4P/K1GG5/9 b 2NPg2l9p 1",
	"l4g1nl/4g1k2/2n1sp1p1/p5pPp/5Ps2/1P1p2s2/P1G1+p1N1P/6K2/LN5RL b RBG3Pbs3p 1",
	"1n4g1k/6r2/1+P1psg1p+L/2p1pp3/3P5/p1P1PPPP1/3SGS3/1+p1K1G2r/9 b 2BNLPs2n2l3p 1",
	"+B2+R3n1/3+L2gk1/5gss1/p1p1p1ppl/5P2p/PPPnP1PP1/3+p2N2/6K2/L4S1RL b BGS3Pgnp 1",
	"3R4l/1kg6/2ns5/spppp2+Bb/p7p/1PPPP1g2/nSNSNP2P/KG1G5/5r2L b L4Pl2p 1",
	"ln5nl/2r2gk2/1p2sgbpp/pRspppp2/L1p4PP/3PP1G2/N4PP2/3BS1SK1/5G1NL b 3P 1",
	"ln7/1r2k1+P2/p3gs3/1b1g1p+B2/1p5R1/2pPP4/PP1S1P3/2G2G3/LN1K5 b SNL3Psnl5p 1",
	"3+P3+Rl/2+P2kg2/+B2psp1p1/4p1p1p/9/2+p1P+bnKP/P6P1/4G1S2/L4G2L b G2S2NLrn5p 1",
	"ln1gb2nl/1ks4r1/1p1g4p/p1pppspB1/5p3/PPPPP1P2/1KNG1PS1P/2S4R1/L2G3NL b Pp 1",
	"lr6l/4g1k1p/1s1p1pgp1/p3P1N1P/2Pl5/PPbBSP3/6PP1/4S1SK1/1+r3G1NL b N3Pgn2p 1",
	"l1ks3+Bl/2g2+P3/p1npp4/1sNn2B2/5p2p/2PP5/PPN1P1+p1P/1KSSg2P1/L1G5+r b GL4Pr 1",
	"ln3k1nl/2P1g4/p1lpsg1pp/4p4/1p1P1p3/2SBP4/PP1G1P1PP/1K1G3+r1/LN1s2PNR b BSPp 1",
	"+N6nl/1+R2pGgk1/5Pgp1/p2p1sp2/3B1p2p/P1pP4P/6PP1/L3G1K2/7NL b RNL2Pb3s2p 1",
	"ln1g5/1r4k2/p2pppn2/2ps2p2/1p7/2P6/PPSPPPPLP/2G2K1pr/LN4G1b b BG2SLPnp 1"
#else
  "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
  "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 10",
  "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 11",
  "4rrk1/pp1n3p/3q2pQ/2p1pb2/2PP4/2P3N1/P2B2PP/4RRK1 b - - 7 19",
  "rq3rk1/ppp2ppp/1bnpb3/3N2B1/3NP3/7P/PPPQ1PP1/2KR3R w - - 7 14",
  "r1bq1r1k/1pp1n1pp/1p1p4/4p2Q/4Pp2/1BNP4/PPP2PPP/3R1RK1 w - - 2 14",
  "r3r1k1/2p2ppp/p1p1bn2/8/1q2P3/2NPQN2/PPP3PP/R4RK1 b - - 2 15",
  "r1bbk1nr/pp3p1p/2n5/1N4p1/2Np1B2/8/PPP2PPP/2KR1B1R w kq - 0 13",
  "r1bq1rk1/ppp1nppp/4n3/3p3Q/3P4/1BP1B3/PP1N2PP/R4RK1 w - - 1 16",
  "4r1k1/r1q2ppp/ppp2n2/4P3/5Rb1/1N1BQ3/PPP3PP/R5K1 w - - 1 17",
  "2rqkb1r/ppp2p2/2npb1p1/1N1Nn2p/2P1PP2/8/PP2B1PP/R1BQK2R b KQ - 0 11",
  "r1bq1r1k/b1p1npp1/p2p3p/1p6/3PP3/1B2NN2/PP3PPP/R2Q1RK1 w - - 1 16",
  "3r1rk1/p5pp/bpp1pp2/8/q1PP1P2/b3P3/P2NQRPP/1R2B1K1 b - - 6 22",
  "r1q2rk1/2p1bppp/2Pp4/p6b/Q1PNp3/4B3/PP1R1PPP/2K4R w - - 2 18",
  "4k2r/1pb2ppp/1p2p3/1R1p4/3P4/2r1PN2/P4PPP/1R4K1 b - - 3 22",
  "3q2k1/pb3p1p/4pbp1/2r5/PpN2N2/1P2P2P/5PP1/Q2R2K1 b - - 4 26",
  "6k1/6p1/6Pp/ppp5/3pn2P/1P3K2/1PP2P2/3N4 b - - 0 1",
  "3b4/5kp1/1p1p1p1p/pP1PpP1P/P1P1P3/3KN3/8/8 w - - 0 1",
  "2K5/p7/7P/5pR1/8/5k2/r7/8 w - - 0 1",
  "8/6pk/1p6/8/PP3p1p/5P2/4KP1q/3Q4 w - - 0 1",
  "7k/3p2pp/4q3/8/4Q3/5Kp1/P6b/8 w - - 0 1",
  "8/2p5/8/2kPKp1p/2p4P/2P5/3P4/8 w - - 0 1",
  "8/1p3pp1/7p/5P1P/2k3P1/8/2K2P2/8 w - - 0 1",
  "8/pp2r1k1/2p1p3/3pP2p/1P1P1P1P/P5KR/8/8 w - - 0 1",
  "8/3p4/p1bk3p/Pp6/1Kp1PpPp/2P2P1P/2P5/5B2 b - - 0 1",
  "5k2/7R/4P2p/5K2/p1r2P1p/8/8/8 b - - 0 1",
  "6k1/6p1/P6p/r1N5/5p2/7P/1b3PP1/4R1K1 w - - 0 1",
  "1r3k2/4q3/2Pp3b/3Bp3/2Q2p2/1p1P2P1/1P2KP2/3N4 w - - 0 1",
  "6k1/4pp1p/3p2p1/P1pPb3/R7/1r2P1PP/3B1P2/6K1 w - - 0 1",
  "8/3p3B/5p2/5P2/p7/PP5b/k7/6K1 w - - 0 1",

  // 5-man positions
  "8/8/8/8/5kp1/P7/8/1K1N4 w - - 0 1",     // Kc2 - mate
  "8/8/8/5N2/8/p7/8/2NK3k w - - 0 1",      // Na2 - mate
  "8/3k4/8/8/8/4B3/4KB2/2B5 w - - 0 1",    // draw

  // 6-man positions
  "8/8/1P6/5pr1/8/4R3/7k/2K5 w - - 0 1",   // Re5 - mate
  "8/2p4P/8/kr6/6R1/8/8/1K6 w - - 0 1",    // Ka2 - mate
  "8/8/3P3k/8/1p6/8/1P6/1K3n2 b - - 0 1",  // Nd2 - draw

  // 7-man positions
  "8/R7/2q5/8/6k1/8/1P5p/K6R w - - 0 124", // Draw

  // Mate and stalemate positions
  "8/8/8/8/8/6k1/6p1/6K1 w - -",
  "5k2/5P2/5K2/8/8/8/8/8 b - -",
  "8/8/8/8/8/4k3/4p3/4K3 w - -",
  "8/8/8/8/8/5K2/8/3Q1k2 b - -",
  "7k/7P/6K1/8/3B4/8/8/8 b - -"
#endif
};

} // namespace

/// benchmark() runs a simple benchmark by letting Stockfish analyze a set
/// of positions for a given limit each. There are five parameters: the
/// transposition table size, the number of search threads that should
/// be used, the limit value spent for each position (optional, default is
/// depth 13), an optional file name where to look for positions in FEN
/// format (defaults are the positions defined above) and the type of the
/// limit value: depth (default), time in millisecs or number of nodes.

void benchmark(const Position& current, istream& is) {

  string token;
  vector<string> fens;
  Search::LimitsType limits;

  // Assign default values to missing arguments
  string ttSize    = (is >> token) ? token : "16";
  string threads   = (is >> token) ? token : "1";
  string limit     = (is >> token) ? token : "13";
  string fenFile   = (is >> token) ? token : "default";
  string limitType = (is >> token) ? token : "depth";

  Options["Hash"]    = ttSize;
  Options["Threads"] = threads;
#if defined(NANOHA)
	Options["OwnBook"] = string("false");
#endif
  Search::clear();

  if (limitType == "time")
      limits.movetime = stoi(limit); // movetime is in millisecs

  else if (limitType == "nodes")
      limits.nodes = stoll(limit);

  else if (limitType == "mate")
      limits.mate = stoi(limit);

  else
      limits.depth = stoi(limit);

  if (fenFile == "default")
      fens = Defaults;

  else if (fenFile == "current")
      fens.push_back(current.fen());

  else
  {
      string fen;
      ifstream file(fenFile);

      if (!file.is_open())
      {
          cerr << "Unable to open file " << fenFile << endl;
          return;
      }

      while (getline(file, fen))
          if (!fen.empty())
              fens.push_back(fen);

      file.close();
  }
#if defined(NANOHA) && defined(USE_DFPN)
///			MateTT.resize(Options["MateHashMB"]);	未実.
#endif

  uint64_t nodes = 0;
  TimePoint elapsed = now();
  Position pos;

  for (size_t i = 0; i < fens.size(); ++i)
  {
      StateListPtr states(new std::deque<StateInfo>(1));
#if defined(NANOHA)
      pos.set(fens[i], &states->back(), Threads.main());
#else
      pos.set(fens[i], Options["UCI_Chess960"], &states->back(), Threads.main());
#endif
      cerr << "\nPosition: " << i + 1 << '/' << fens.size() << endl;

      if (limitType == "perft")
          nodes += Search::perft(pos, limits.depth * ONE_PLY);

      else
      {
          limits.startTime = now();
          Threads.start_thinking(pos, states, limits);
          Threads.main()->wait_for_search_finished();
          nodes += Threads.nodes_searched();
      }
  }

  elapsed = now() - elapsed + 1; // Ensure positivity to avoid a 'divide by zero'

  dbg_print(); // Just before exiting

  cerr << "\n==========================="
       << "\nTotal time (ms) : " << elapsed
       << "\nNodes searched  : " << nodes
       << "\nNodes/second    : " << 1000 * nodes / elapsed << endl;
}

#if defined(NANOHA)
namespace {
	struct ResultMate1 {
		TimePoint msec;
		int result;
		Move m;
	};
	void disp_moves(ExtMove mstack[], size_t n)
	{
		cerr << "     ";
		for (size_t i = 0; i < n; i++) {
			cerr << " " << UCI::move(mstack[i].move);
			if (i % 6 == 5) cerr << endl << "     ";
		}
		cerr << endl;
	}
	string conv_per_s(const double loops, TimePoint t)
	{
		if (t == 0) t++;
		double nps = loops * 1000 / t;
		char buf[64];
		if (nps > 1000*1000) {snprintf(buf, sizeof(buf), "%.3f M", nps / 1000000.0); }
		else if (nps > 1000) {snprintf(buf, sizeof(buf), "%.3f k", nps / 1000.0); }
		else  {snprintf(buf, sizeof(buf), "%d ", int(nps)); }
		return string(buf);
	}
}

// 1手詰め or 3手詰め
void bench_mate(istream& is, const int mateplys)
{
	string token;
	vector<string> fens;
	vector<ResultMate1> result;
	int type = (mateplys == 3) ? 1 : 0;
	const char *typestr[] = { "Mate1ply", "Mate3play" };

	// デフォルト値を設定
	string fenFile = (is >> token) ? token : "default";
	bool bLoop     = (is >> token) ? (token == "yes" ? true : false) : true;
	bool bDisplay  = (is >> token) ? (token == "no" ? false : true) : false;
	bool bDispNoMate = (is >> token) ? (token == "no" ? false : true) : false;

	cerr << "Benchmark type: " << typestr[type] << " routine." << endl;

	if (fenFile == "default") {
		fens = (mateplys == 1) ? PositionForMate1 : Defaults;
	} else {
		string fen;
		ifstream file(fenFile);

		if (!file.is_open())
		{
			cerr << "Unable to open file " << fenFile << endl;
			return;
		}
		while (getline(file, fen)) {
			if (!fen.empty()) {
				if (fen.compare(0, 5, "sfen ") == 0) {
					fen.erase(0, 5);
				}
				fens.push_back(fen);
			}
		}
		file.close();
	}

	// ベンチ開始
	int loops = (bLoop ? 1000*1000 : 1000); // 1M回
	if (type != 0) loops /= 10;	// mate3はmate1より時間がかかるので、1/10にする

	ResultMate1 record;

	TimePoint elapsed = now();
	TimePoint total = 0;
	size_t i;
	Position pos;
	for (i = 0; i < fens.size(); i++)
	{
		Move move = MOVE_NONE;
		StateListPtr states(new std::deque<StateInfo>(1));
		pos.set(fens[i], &states->back(), Threads.main());
#if defined(_DEBUG) || !defined(NDEBUG)
		int failState;
		assert(pos.pos_is_ok(&failState));
#endif

		if (bLoop)
				cerr << "\nBench position: " << i + 1 << '/' << fens.size() << "  c=" << pos.side_to_move() << endl;

		volatile int v = 0;
		uint32_t info;
		TimePoint rap_time = now();
		if (type == 0) {
			// 1手詰め
			if (pos.side_to_move() == BLACK) {
				for (int j = 0; j < loops; j++) {
					v = pos.Mate1ply<BLACK>(move, info);
				}
			} else {
				for (int j = 0; j < loops; j++) {
					v = pos.Mate1ply<WHITE>(move, info);
				}
			}
		} else {
			// 3手詰め
			for (int j = 0; j < loops; j++) {
				v = pos.Mate3(pos.side_to_move(), move);
			}
		}
		rap_time = now() - rap_time + 1;
		total += rap_time;
		if (bLoop && bDisplay) pos.print_csa(move);

		record.msec = rap_time;
		record.result = v;
		record.m = move;
		result.push_back(record);

		if (bLoop) cerr << v << "\t" << UCI::move(move) << "  " << rap_time << "(ms)  "
		                << conv_per_s(loops, rap_time) << " times/s" << endl;
	}

	elapsed = now() - elapsed + 1;

	cerr << "\n==============================="
		 << "\nTotal time (ms) : " << elapsed << "(" << total << ")";
	cerr << "\n  Average : " << conv_per_s(static_cast<const double>(loops*fens.size()), elapsed) << " times/s" << endl;

	if (bDispNoMate) cerr << "\n";
	int solved = 0;
	int unknown = 0;
	for (i = 0; i < result.size(); i++) {
		if (result[i].result == VALUE_MATE) {
			solved++;
		} else {
			unknown++;
			if (bDispNoMate) {
				StateListPtr states(new std::deque<StateInfo>(1));
				pos.set(fens[i], &states->back(), Threads.main());
				cerr << "\nBench position: " << i + 1 << '/' << fens.size() << "  c=" << (pos.side_to_move() == BLACK ? "black" : "white") << endl;
				pos.print_csa();
			}
		}
	}
	cerr << "Mate    =  " << solved  << endl;
	cerr << "Unknown =  " << unknown << endl;
	cerr << "Ave.time=  " << double(elapsed) / result.size() << "(ms)" << endl;
	cerr << "Lopps   =  " << loops << endl;
	cerr << "Average =  " << conv_per_s(static_cast<const double>(loops*result.size()), elapsed) << " times/s" << endl;
}

void bench_genmove(istream& is) {

	string token;
	vector<string> fens;

	// デフォルト値を設定
	string fenFile = (is >> token) ? token : "default";
	bool bDisplay  = (is >> token) ? (token == "yes" ? true : false) : false;

	cerr << "Benchmark type: generate moves." << endl;

	if (fenFile == "default") {
		fens = GenMoves;
		cerr << "SFENs is default." << endl;
	} else {
		string fen;
		ifstream file(fenFile.c_str());

		if (!file.is_open())
		{
			cerr << "Unable to open file " << fenFile << endl;
			return;
		}

		while (getline(file, fen)) {
			if (!fen.empty()) {
				if (fen.compare(0, 5, "sfen ") == 0) {
					fen.erase(0, 5);
				}
				fens.push_back(fen);
			}
		}

		file.close();
		cerr << "SFEN file is" << fenFile << "." << endl;
	}

	TimePoint elapsed = now();

	ExtMove ss[MAX_MOVES];
	volatile ExtMove *mlist = NULL;
#if defined(NDEBUG)
	int loops = 1000*1000; // 1M回
#else
	int loops = 500*1000; // 500k回
#endif
	int j;
	Position pos;
	for (size_t i = 0; i < fens.size(); i++)
	{
		StateListPtr states(new std::deque<StateInfo>(1));
		pos.set(fens[i], &states->back(), Threads.main());
#if defined(_DEBUG)
		int failState;
		assert(pos.pos_is_ok(&failState));
#endif

		cerr << "\nBench position: " << i + 1 << '/' << fens.size() << endl;
		TimePoint rap_time = now();
		for (j = 0; j < loops; j++) {
			mlist = generate<LEGAL>(pos, ss);
		}
		rap_time = now() - rap_time;
		if (bDisplay) pos.print_csa();
		cerr << "  Genmove(" << mlist - ss << "): " << rap_time << "(ms), " << conv_per_s(loops, rap_time) << "times/s" << endl;
		if (bDisplay) disp_moves(ss, mlist - ss);

		rap_time = now();
		for (j = 0; j < loops; j++) {
			mlist = generate<CAPTURES>(pos, ss);
		}
		rap_time = now() - rap_time;
		cerr << "  Gencapture(" << mlist - ss << "): " << rap_time << "(ms), " << conv_per_s(loops, rap_time) << "times/s" << endl;
		if (bDisplay) disp_moves(ss, mlist - ss);

		rap_time = now();
		for (j = 0; j < loops; j++) {
			mlist = generate<QUIETS>(pos, ss);
		}
		rap_time = now() - rap_time;
		cerr << "  noncapture(" << mlist - ss << "): " << rap_time << "(ms), " << conv_per_s(loops, rap_time) << "times/s" << endl;
		if (bDisplay) disp_moves(ss, mlist - ss);

		rap_time = now();
		for (j = 0; j < loops; j++) {
			mlist = generate<CHECKS>(pos, ss);
		}
		rap_time = now() - rap_time;
		cerr << "  gen_check(" << mlist - ss << "): " << rap_time << "(ms), " << conv_per_s(loops, rap_time) << "times/s" << endl;
		if (bDisplay) disp_moves(ss, mlist - ss);
	}

	elapsed = now() - elapsed;

	cerr << "\n==============================="
		 << "\nTotal time (ms) : " << elapsed << endl;
}

void bench_eval(istream& is)
{
	string token;
	vector<string> fens;

	// デフォルト値を設定
	string fenFile = (is >> token) ? token : "default";
	bool bDisplay = (is >> token) ? (token == "yes" ? true : false) : false;

	cerr << "Benchmark type: evaluate." << endl;

	if (fenFile == "default") {
		fens = EvalPos;
	} else {
		string fen;
		ifstream file(fenFile.c_str());

		if (!file.is_open())
		{
			cerr << "Unable to open file " << fenFile << endl;
			return;
		}

		while (getline(file, fen)) {
			if (!fen.empty()) {
				if (fen.compare(0, 5, "sfen ") == 0) {
					fen.erase(0, 5);
				}
				fens.push_back(fen);
			}
		}

		file.close();
		cerr << "SFEN file is" << fenFile << "." << endl;
	}

	TimePoint elapsed = 0;

#if defined(NDEBUG)
	int loops = 1000*1000;	// 1M回
#else
	int loops =   50*1000;	// 50k回
#endif
	int j;
	volatile Value v = VALUE_ZERO;
	Position pos;
	for (size_t i = 0; i < fens.size(); i++)
	{
		StateListPtr states(new std::deque<StateInfo>(1));
		pos.set(fens[i], &states->back(), Threads.main());
#if defined(_DEBUG)
		int failState;
		assert(pos.pos_is_ok(&failState));
#endif

		cerr << "\nBench position: " << i + 1 << '/' << fens.size() << endl;
		TimePoint rap_time = now();
		for (j = 0; j < loops; j++) {
			v = Eval::evaluate(pos);
		}
		rap_time = now() - rap_time;
		if (bDisplay) pos.print_csa();
		cerr << "  evaluate():m=" << pos.get_material() << ", v= " << int(v) << ", time= " << rap_time << "(ms), " << conv_per_s(loops, rap_time) << " evaluate/s" << endl;

		elapsed += rap_time;
	}

	cerr << "\n==============================="
		 << "\nTotal time (ms) : " << elapsed 
		 << "\n        Average : " << conv_per_s(loops * fens.size(), elapsed) << " evaluate/s" << endl;
}

static void chop(char *p)
{
	while (*p != '\0') {
		if (*p == '\r' || *p == '\n') {
			*p = '\0';
			break;
		}
		p++;
	}
}

static int load_csa(const char *fn, Move kifu[3000])
{
	int teNum = 0;
	if (fn == NULL) {
		// 第17回世界コンピュータ将棋選手権のＹＳＳ－Bonanza 戦.
		teNum=136;
		static const Move movesyss_kifu[] = {
			Move(0x00027776), Move(0x00223334), Move(0x00026766), Move(0x002C2233),
			Move(0x00087968), Move(0x00283142), Move(0x00086867), Move(0x00228384),
			Move(0x000E2868), Move(0x00287162), Move(0x00105948), Move(0x002A4132),
			Move(0x00104838), Move(0x00305141), Move(0x00041918), Move(0x002A6152),
			Move(0x00103828), Move(0x00304131), Move(0x000C8877), Move(0x00224344),
			Move(0x00102819), Move(0x00284243), Move(0x00083928), Move(0x00303122),
			Move(0x000A4939), Move(0x00221314), Move(0x000A6958), Move(0x00221415),
			Move(0x00086756), Move(0x00228485), Move(0x000A5848), Move(0x00225354),
			Move(0x00024746), Move(0x00286253), Move(0x000A4838), Move(0x00227374),
			Move(0x00026665), Move(0x00228586), Move(0x04428786), Move(0x00224445),
			Move(0x04424645), Move(0x01AD3377), Move(0x07868977), Move(0x006E8286),
			Move(0x00026564), Move(0x00685364), Move(0x000E6848), Move(0x002A5242),
			Move(0x00020082), Move(0x00268173), Move(0x00024544), Move(0x00284352),
			Move(0x00020065), Move(0x00286453), Move(0x000C0095), Move(0x002E8683),
			Move(0x00038281), Move(0x002C0066), Move(0x00085645), Move(0x006D6657),
			Move(0x04928191), Move(0x00229394), Move(0x04CD9573), Move(0x03AE8373),
			Move(0x00060026), Move(0x00302231), Move(0x000E4888), Move(0x00220084),
			Move(0x04462634), Move(0x002A4241), Move(0x00040059), Move(0x003C5735),
			Move(0x04484554), Move(0x01285354), Move(0x05045954), Move(0x00220053),
			Move(0x00080062), Move(0x002E7383), Move(0x04496253), Move(0x03285253),
			Move(0x05055453), Move(0x00FC3534), Move(0x000E8848), Move(0x00280057),
			Move(0x00034443), Move(0x01E95748), Move(0x00020042), Move(0x026A3243),
			Move(0x05434241), Move(0x00303122), Move(0x00080031), Move(0x00302213),
			Move(0x070A3948), Move(0x02AA4353), Move(0x00080022), Move(0x00301324),
			Move(0x04C92221), Move(0x00240043), Move(0x00020047), Move(0x00260056),
			Move(0x00060036), Move(0x00302435), Move(0x000A4857), Move(0x002E0059),
			Move(0x000A0046), Move(0x00303525), Move(0x04CA4656), Move(0x00220055),
			Move(0x00022726), Move(0x00302514), Move(0x00021716), Move(0x00621516),
			Move(0x00020015), Move(0x00701415), Move(0x04982111), Move(0x002C0049),
			Move(0x000A3839), Move(0x00280038), Move(0x050A3938), Move(0x016D4938),
			Move(0x00060027), Move(0x00FC3827), Move(0x07882827), Move(0x00260017),
			Move(0x000C0042), Move(0x00220033), Move(0x044D4233), Move(0x03BC3433),
			Move(0x04C41817), Move(0x00A31617), Move(0x00020016), Move(0x00721716),
			Move(0x06482716), Move(0x01301516), Move(0x00080028), Move(0x002C0038),
		};
		int i;
		for (i = 0; i < teNum; i++) {
			kifu[i] = movesyss_kifu[i];
		}
		return teNum;
	}
	FILE *fp;
	fp = fopen(fn, "r");
	if (fp == NULL) {
		perror(fn);
		return -1;
	}

	StateListPtr states(new std::deque<StateInfo>(1));
	Position pos;
	pos.set(string("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1"), &states->back(), Threads.main());
	StateInfo st[1000];
	char buf[512];
	int line=0;
	int from, to;
	int SorG = SENTE;
	static const char tbl[] = "FUKYKEGIKIKAHIOUTONYNKNG  UMRY";
	char km[3] = {0,0,0};
	const char *p;
	int sq;
#define ERR	{output_info("Error!:%d:%s:%d:teNum=%d, buf=[%s]\n", __LINE__, fn, line, teNum, buf); return -1;}
	while (fgets(buf, sizeof(buf), fp) != NULL) {
		line++;
		chop(buf);
		if (buf[0] == '+' && isdigit(buf[1])) {
			if (SorG != SENTE) ERR
		} else if (buf[0] == '-') {
			if (!isdigit(buf[1])) ERR
			if (SorG != GOTE) ERR
		} else {
			continue;
		}
		if (!isdigit(buf[2])) ERR
		if (!isdigit(buf[3])) ERR
		if (!isdigit(buf[4])) ERR
		km[0] = buf[5];
		km[1] = buf[6];
		p = strstr(tbl, km);
		if (p == NULL) ERR
		sq = p - tbl;
		if (sq & 1) ERR
		sq = sq / 2 + 1;
		from = (buf[1] - '0')*0x10+(buf[2] - '0');
		to   = (buf[3] - '0')*0x10+(buf[4] - '0');
		if (from < 0x11) {
			// 駒打ち
			Piece koma = Piece(sq|SorG);
			Piece cap = pos.piece_on(Square(to));
			if (cap != EMP) ERR
			kifu[teNum] = cons_move(from, to, koma, cap);
		} else {
			// 駒移動
			Piece koma = pos.piece_on(Square(from));
			int pro = 0;
			if (koma != (sq|SorG)) {
				if (koma+PROMOTED == (sq|SorG)) {
					pro = 1;
				} else {
					ERR
				}
			}
			Piece cap = pos.piece_on(Square(to));
			kifu[teNum] = cons_move(from, to, koma, cap, pro);
		}
		if (pos.legal(kifu[teNum]) == false) ERR
		pos.do_move(kifu[teNum], st[teNum]);

		SorG ^= GOTE;
		teNum++;
	}

	if (fp) fclose(fp);
	cout << "teNum=" << teNum << endl;
	cout << "Move movesyss_kifu[] = {";
	for (int i = 0; i < teNum; i++) {
		if (i % 8 == 0) cout << "\n\t";
		cout << "0x" << std::hex << int(kifu[i]) << ", ";
	}
	cout << "\n};" << std::dec << endl;
	return teNum;
}

// 参考： http://chocobo.yasuda-u.ac.jp/~nisimura/mymove/index.cgi?no=967
// ノードの移動（手を進めたり、戻したり）の速度だけを測定するために、ある棋譜
// を初手から一手ずつ、評価関数を呼ぶとか余計なことは一切せずに一気に進みます。
// 投了図まで進んだら、今度は一手ずつ一気に初手まで戻ります。これを例えば 10 秒
// 間くらい繰り返し、１秒間に平均で何手進める（戻す）ことができたかを計算します。
// 進めて１手、戻して１手と数えます。
void bench_movesyss(istream& is)
{
	Move kifu[3000];
	StateInfo st[3000];
	string token;
	int num = (is >> token) ? load_csa(token.c_str(), kifu) : load_csa(NULL, kifu);
	if (num < 0) return;

	Position pos;
	StateListPtr states(new std::deque<StateInfo>(1));
	pos.set("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1", &states->back(), Threads.main());

	cerr << "Benchmark type: moves/sec." << endl;

	// ベンチ
	int i, loops = 0;
	const int LOOP = 1000*1000/200;	//  往復200手の場合、1M回手を進める/戻すのに千必要なループ数
	TimePoint begin = now();
	TimePoint end;
	while (1) {
		int l;
		for (l = 0; l < LOOP; l++) {
			for (i = 0; i < num; i++) {
				pos.do_move(kifu[i], st[i]);
			}
			for (i = num - 1; i >= 0; i--) {
				pos.undo_move(kifu[i]);
			}
		}
		loops += l;
		end = now();
		// 10秒以上回す.
		if (end - begin >= 10*1000) break;
	}
	cerr << "moves=" << num << ", " << end-begin << " ms, " << loops << " loops" << endl;
	cerr << 2000.0*num*loops/(end-begin) << " moves/sec" << endl;
	int kind[4] = {0,0,0,0};
	int piece[16] = {0,};
	int cap = 0;
	for (i = 0; i < num; i++) {
		Piece koma = piece_of(kifu[i]);
		int p = ptype_of(kifu[i]);
		if (koma == SOU) {
			kind[0]++;
		} else if (koma == GOU) {
			kind[1]++;
		} else if (is_drop(kifu[i])) {
			kind[2]++;
		} else {
			kind[3]++;
		}
		if (capture_of(kifu[i])) cap++;
		piece[p]++;
	}
	output_info("  move kinds:SOU=%d, GOU=%d, Drops=%d, else=%d, cap=%d\n", kind[0], kind[1], kind[2], kind[3], cap);
	const char *k_str[] = {
		NULL, "FU", "KY", "KE", "GI", "KI", "KA", "HI",
		"OU", "TO", "NY", "NK", "NG", NULL, "UM", "RY"
	};
	for (i = 0; i < 16; i++) {
		if (k_str[i]) {
			output_info("  %s=%d", k_str[i], piece[i]);
		} else if (piece[i] > 0) {
			output_info("\n  Error!:%d:%d\n", i, piece[i]);
			exit(1);
		}
	}
	output_info("\n");
}

// SEE
void bench_see(istream& is)
{
	string token;
	vector<string> fens;

	// デフォルト値を設定
	string fenFile = (is >> token) ? token : "default";
	Value value = (is >> token) ? Value(stoi(token)) : VALUE_ZERO;

	cerr << "Benchmark SEE" << endl;

	if (fenFile == "default") {
		fens = SEEPos;
	} else {
		string fen;
		ifstream file(fenFile);

		if (!file.is_open())
		{
			cerr << "Unable to open file " << fenFile << endl;
			return;
		}
		while (getline(file, fen)) {
			if (!fen.empty()) {
				if (fen.compare(0, 5, "sfen ") == 0) {
					fen.erase(0, 5);
				}
				fens.push_back(fen);
			}
		}
		file.close();
	}

	// ベンチ開始
	// まずはテストなので1回.
	int loops = 1; // 1回

	TimePoint elapsed = now();
	TimePoint total = 0;
	size_t i;
	Position pos;
	ExtMove movelist[MAX_MOVES];
	ExtMove *cur = movelist, *end;
	for (i = 0; i < fens.size(); i++)
	{
		StateListPtr states(new std::deque<StateInfo>(1));
		pos.set(fens[i], &states->back(), Threads.main());
#if defined(_DEBUG) || !defined(NDEBUG)
		int failState;
		assert(pos.pos_is_ok(&failState));
#endif

		cerr << "\nBench position: " << i + 1 << '/' << fens.size() << "  c=" << pos.side_to_move() << endl;

		end = generate<CAPTURES>(pos, movelist);
		cur = movelist - 1;

		pos.print_csa();
		cerr << "Moves:" << end - movelist << endl;
		cerr << "Value:" << value << endl;
		while (++cur < end) {
			TimePoint rap_time = now();
			bool b;
			for (int j = 0; j < loops; j++) {
				b = pos.see_ge(*cur, value);
			}
			rap_time = now() - rap_time + 1;
			total += rap_time;

			cerr << "  " << setw(5) << left << UCI::move(*cur) << "  "
			     << (b ? "true " : "false") << " " << rap_time << "(ms)  "
			          << conv_per_s(loops, rap_time) << " times/s" << endl;
		}
	}

	elapsed = now() - elapsed + 1;

	cerr << "\n==============================="
		 << "\nTotal time (ms) : " << elapsed << "(" << total << ")";
	cerr << "\n  Average : " << conv_per_s(static_cast<const double>(loops*fens.size()), elapsed) << " times/s" << endl;
	cerr << "Lopps   =  " << loops << endl;
}
#endif
