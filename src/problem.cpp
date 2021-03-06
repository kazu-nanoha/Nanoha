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
#include <vector>
#include <cstring>

#include "position.h"
#include "search.h"
#include "thread.h"
#include "uci.h"
#if defined(NANOHA)
#include "movegen.h"
#if defined(USE_DFPN)
// 詰めルーチン用ヘッダ
#include "SearchMateDFPN.h"
#endif
#endif

using namespace std;

namespace {
// SEEの標準
const vector<string> tc_see = {
#if 0
	// 進歩本2の棋力判定問題のNo.1 - No.5×手番(先手、後手)
	"lR1B3nl/2gp5/ngk1+BspPp/1s2p2p1/p4S3/1Pp6/P5P1P/LGG6/KN5NL b Prs5p 1",
	"lR1B3nl/2gp5/ngk1+BspPp/1s2p2p1/p4S3/1Pp6/P5P1P/LGG6/KN5NL w Prs5p 1",
	"5S2l/1rP2s1k1/p2+B1gnp1/5np2/3G3n1/5S2p/P1+p1PpPP1/1P1PG2KP/L2+rLPGNL b Bs3p 1",
	"5S2l/1rP2s1k1/p2+B1gnp1/5np2/3G3n1/5S2p/P1+p1PpPP1/1P1PG2KP/L2+rLPGNL w Bs3p 1",
	"lR6l/1s1g5/1k1s1+P2p/1+bpp1+Bs2/1n1n2Pp1/2P6/S2R4P/K1GG5/9 b 2NPg2l9p 1",
	"lR6l/1s1g5/1k1s1+P2p/1+bpp1+Bs2/1n1n2Pp1/2P6/S2R4P/K1GG5/9 w 2NPg2l9p 1",
	"l4g1nl/4g1k2/2n1sp1p1/p5pPp/5Ps2/1P1p2s2/P1G1+p1N1P/6K2/LN5RL b RBG3Pbs3p 1",
	"l4g1nl/4g1k2/2n1sp1p1/p5pPp/5Ps2/1P1p2s2/P1G1+p1N1P/6K2/LN5RL w RBG3Pbs3p 1",
	"1n4g1k/6r2/1+P1psg1p+L/2p1pp3/3P5/p1P1PPPP1/3SGS3/1+p1K1G2r/9 b 2BNLPs2n2l3p 1",
	"1n4g1k/6r2/1+P1psg1p+L/2p1pp3/3P5/p1P1PPPP1/3SGS3/1+p1K1G2r/9 w 2BNLPs2n2l3p 1",
#else
	// 5五の地点を焦点にして.
	"1k7/9/9/4p4/9/4P4/9/9/1K7 b RB2G2S2N2L8Prb2g2s2n2l8p 1", 
	"1k7/9/9/4p4/9/4P4/9/9/1K7 w RB2G2S2N2L8Prb2g2s2n2l8p 1", 
	"1k2l4/9/9/4p4/9/4P4/9/9/1K2L4 b RB2G2S2NL8Prb2g2s2nl8p 1", 
#endif
};
// 進歩本2の棋力判定問題
const vector<string> ComShogi = {
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
	"ln1g5/1r4k2/p2pppn2/2ps2p2/1p7/2P6/PPSPPPPLP/2G2K1pr/LN4G1b b BG2SLPnp 1",
	"ln3k2l/1r7/p1bp1g1pp/2p1p4/1pBP1ns2/4Pp3/PPSG3PP/1KG2R3/LN5NL b G2P2s2p 1",
	"ln6l/1r4gk1/p2psg1pp/2pb1pp2/1p2p1Ss1/2PP4P/PPSG1P3/2GB2R2/LNK5L b NPn2p 1",
	"ln1g5/1ks4+Rl/1pbg2+P2/pn2r1s1p/2Pp1P3/P3PS2P/1PGP1G3/1K1s5/LN6L b BN2P4p 1",
	"ln1g5/1ks5l/1pb6/1n2r1+P+Rp/2PpPP3/pGn2S2P/1P1P1G3/K2s5/LN6L b BGSP6p 1",
	"ln3g1nl/4g1s2/p1+Pp1p1k1/6ppp/3P5/2p1pPPPP/PP5R1/4G1SK1/LN1+r1G1NL b BSPbsp 1",
	"5g1+Ll/4g1s2/p4p2k/3p2pPp/3P1N1p1/2pbpPS1P/P+r5S1/4G3K/L3PG1NL b S2Nrb4p 1",
	"l+B6l/3+P2gk1/2p2g1s1/p2Gppp1p/1p5n1/P1+b2PPSP/2N2SNK1/6GS1/7rL b RN4Pl2p 1",
	"l+B6l/3+PRn1k1/2p2g1g1/p2Gpppp1/1p5Lp/P4PP1P/2+r4P1/6GSK/4P3L b BS2N3P2sn 1",
	"3+Rl2kl/S1S1+PG3/2+b1p1B2/7rp/1Kp2pPN1/6GPP/1P2P3L/9/L5+p2 b 2N5P2g2sn2p 1",
	"4pg1nl/2p2gkb1/p1n+RP2p1/r4pp1p/Bp7/3P1P2P/PP4PP1/2+p3SK1/LN3G1NL b 2Sgslp 1",
	"ln1g2+Rn1/1ks1g3l/1p2s2p1/p1pp1B2p/7P1/P1PSp3P/1P1P1G+p2/1K3P3/LN5+rL b BGS2Pnp 1",
	"lr6l/3+P+P1gS1/5k3/2pGs1spp/p3p4/1PPR1p2P/PGG3+b2/1K4+b2/LNN5L b S3P2n3p 1",
	"l2g4+B/2s6/2kn3+Rp/ppg1p1P2/1ns5b/2ppP4/PP1P1P1PP/LSGS5/KNG4+rL b NLP3p 1",
	"l3l1pn1/4k4/3ppg1+Rl/p2s5/9/P2+R2P2/1P1PPP3/2K1+p4/L6N1 b G3S2N3P2b2g4p 1",
	"ln5nl/2+R2sk2/pp5pp/4pb3/4Ppp2/6PP1/Psp2PB1P/3PS2R1/L1K4NL b 4GNPs2p 1",
	"l1l1B3l/6g2/4p1nk1/4spgpp/3p2B2/rpG1S1p1P/N3P1N2/PKG6/7R1 b SNLs8p 1",
	"lngg2+R2/1k4P1l/1ps2+P1pp/1nppp4/p6P1/P1P1P1S2/BP1P1S2P/2KG1+b1+r1/LN1N4L b GS2P 1",
	"l2r1g1nl/6sk1/5g3/3Ps1ppp/pRB2p3/2P1p1PPP/P2G1PSS1/6GK1/+b6NL b N4Pnlp 1",
	"l2r4l/1+R1pg1sk1/5gnl1/3P+b1ppp/p1B1pp3/2P3PPP/P3GPSS1/6GK1/7NL b S4P2n 1",
	"l+b1r1n2l/1p1p2sk1/2+R2gnl1/3G2p1p/p1B1pp1P1/2P3P1P/P3GPSS1/6GK1/7NL b 5Psn 1",
	"l2r1n2l/1p1p3k1/5g1l1/2+R3p1p/p3pp3/6PPP/P3GPNS1/6GK1/8L b BSN3Pbg2sn3p 1",
	"ln1g3nl/1k3rg2/4s1bpp/1pPsp4/2Bp2SP1/p3P4/1PNP1P2P/2KGG1R2/L6NL b S2P3p 1",
	"l2g3nl/6g2/1k1+N3pp/1pp1p4/2+rB2SP1/p3P4/1P1P1P2P/3G2R2/Ls2K2NL b 2SNbg6p 1",
	"ln1g4l/1ksg1+P3/ppppp2pp/1l3N3/9/1BPPPS2B/PP1G4P/1KS2+n3/LN1G2+r1+r b SP3p 1",
	"lnS+N4l/9/ksppp2pp/pp4p2/9/1PPPPS2B/P2G4P/1KS2+n3/LN1G1P+r1+r b GLbg3p 1",
	"lR4Bnl/5gs2/5nkpp/p1P1psp2/3P5/4P1PPP/P+p3G3/3+b2SK1/L4G1NL b G4Prsnp 1",
	"l1+L6/1k1rn2+R1/p1p1ppg2/1G1P1bp1p/2P6/2Ns1N2P/PPNpPP3/1SG1K1S2/L4G2L b 2Pbs2p 1",
	"l5k1l/4+Psg2/p1r2p1p1/4s1p1p/1p3n3/P3P4/BP3PP1P/2GS3R1/+bNK4NL b GNPgsl4p 1",
	"ln1g4l/1kss2g2/2pp1pn2/6rp1/PpPPP1pPp/p3SP3/BPS2GP1L/1KG3R2/LN5N1 b BPp 1",
	"l2g2ks1/4P4/2p1+S2pn/p2p2+r1p/5+B3/P3S2PP/1PPP+b1P2/1rG2P3/LN2KG1NL b GN2Psl2p 1",
	"l7l/4+N1+N1k/2+B1p1ng1/p5ppp/2P1S4/PpSp2P1P/2S1P4/1KG4R1/LN6+r b B4P2gsl2p 1",
	"7nl/5bgk1/2+Pp1gspp/P4p3/4P1PP1/1l1P5/2NG1+s2P/1g1S3R1/L2KB2NL b RSN6Pp 1"
};

#if defined(USE_DFPN)
// 将棋図巧 第1〜第20
const vector<string> zuko = {
	"1pG1B4/Gs+P6/pP7/n1ls5/3k5/nL4+r1b/1+p1p+R4/1S7/2N6 b PS11p2ln2g 1",	// zuko001
	"9/9/3B3G1/6+p1n/7+p1/7k1/5BL1p/7G1/7+R+r b 2PN13p3l2n4s2g 1",
	"2p1g+P3/1P1p1pp2/l1P1g1S+r1/P1B1P1+p2/1p3l3/1kN6/9/2+B6/1r7 b 2P2NSG5p2ln2sg 1",
	"6S+Pl/5s3/4L2l1/5gkBr/4pn3/5Gp2/9/6N2/7+R+n b NB15pl2s2g 1",
	"n+B1sS4/1R1g5/1Ls6/2k6/2n6/3L5/R8/9/9 b 2PB16p2l2ns3g 1",
	"+Rg2+Bpk2/2p1g2pl/1n7/2P1GP1+rp/1N2p1+p2/N5N2/9/9/9 b 4PSB6p3l3sg 1",
	"1nkg5/2s6/3rp4/1B7/r8/1PN6/9/9/9 b N3SGB16p4ln2g 1",
	"4+bn1g1/5kpPl/2l1b1s2/3+l1N1GP/4G1+P1N/5L3/9/1+r7/R8 b PG13pn3s 1",
	"1gn2+r1n1/1s1knPg2/+R1L1s1+BS1/1s1+B1N3/4P+pP2/9/9/9/9 b P13p3l2g 1",
	"6n2/+PS1+Ps1k2/3s+B1l2/1Lp1G1p1S/3p1n1+R+p/3+p1+nG2/4Pp1G1/6G+r1/5L1N+B b 2P7pl 1",
	"r+b2n4/1Pk1p4/pp1sG4/N1+s1+p4/2P1+R4/+pB1LN4/9/9/9 b NS11p3ls3g 1",
	"5+rl1s/7k1/8n/6p1+B/9/9/9/9/9 b P2NR16p3ln3s4gb 1",
	"2lS5/1kgp5/pB3+r3/3+b+P4/PlpgS4/9/9/9/9 b P2SR12p2l4n2g 1",
	"5+B1R1/5pP1k/6+b1p/9/4+p1p2/7N1/7+R1/9/9 b 3NS13p4l3s4g 1",
	"2g+Rp4/k+r1BlB3/NN1g5/n1pP+p4/9/1L7/9/9/9 b PN13p2l4s2g 1",
	"5gGl1/6R1n/5n1Lk/6+p2/5B1p1/9/9/9/9 b NBR16p2ln4s2g 1",
	"l2B2+r2/s1s3+L2/k2s1g3/5B3/l+p1s+l4/1N+R2N3/2p6/9/9 b PN15pn3g 1",
	"7n1/5+B+P1r/7k1/7+p1/6P2/7N1/7N1/8B/9 b 2PN13p4l4s4gr 1",
	"2+b6/P1l+R5/pk7/N1P6/1S7/2p6/1R7/9/9 b 3P11p3l3n3s4gb 1",
	"4ng2+r/5+r1+L1/4P1k1G/4l2P1/3spS1p+p/9/3N1L+p2/3+b5/1+B7 b 12pl2n2s2g 1"
};
#endif
}

void solve_problem(istream& is)
{
	string token;
	string arg;
	vector<string> sfens;
	Search::LimitsType limits;

	// Assign default values to missing arguments
	string ttSize   = "128";
	string threads  = "1";
	string valStr   = "2";
	string sfenFile = "default";
	string valType  = "time";
	string outFile  = "";
	string suffix   = "mal";
	bool bOut = false;

	// -hash N	ハッシュサイズ(MB)
	// -threads N	スレッド数
	// -sec N	秒数
	// -nodes N	ノード数
	// -depth N	深さ
	// -o output file	結果を書き出すファイル
	// -suffix suffix	ファイル中に書き出す拡張子を指定する
	while (is >> token) {
		cerr << "token:" << token << endl;
		if (token[0] == '-') {
			if (is >> arg) {
				if (token == "-hash") {
					ttSize = arg;
				} else if (token == "-threads") {
					threads = arg;
				} else if (token == "-sec") {
					valType = "time";
					valStr = arg;
				} else if (token == "-nodes") {
					valType = "nodes";
					valStr = arg;
				} else if (token == "-depth") {
					valType = "depth";
					valStr = arg;
				} else if (token == "-o") {
					outFile = arg;
					bOut = true;
				} else if (token == "-suffix") {
					suffix = arg;
				} else {
					cerr << "Error!:token = " << token << endl;
					exit(EXIT_FAILURE);
				}
			} else {
				cerr << "Error!:token = " << token << endl;
				exit(EXIT_FAILURE);
			}
		} else {
			sfenFile = token;
			break;
		}
	}
	cerr << "sfenFile = " << sfenFile << endl;
	cerr << "ttSize   = " << ttSize << endl;
	cerr << "threads  = " << threads << endl;
	cerr << "valType  = " << valType << endl;
	cerr << "valStr   = " << valStr << endl;
	cerr << "outFile  = " << outFile << endl;
	cerr << "output   = " << (bOut ? "true" : "false") << endl;
	cerr << "suffix   = " << suffix << endl;

	Options["Hash"] = ttSize;
	Options["Threads"] = threads;
	Options["OwnBook"] = true;
	Options["Output_AllDepth"] = true;

	// Search should be limited by nodes, time or depth ?
	if (valType == "nodes")
		limits.nodes = atoi(valStr.c_str());
	else if (valType == "time")
		limits.movetime = 1000 * atoi(valStr.c_str()); // movetime is in ms
	else
		limits.depth = atoi(valStr.c_str());

	// Do we need to load positions from a given SFEN file ?
	if (sfenFile != "default")
	{
		// ファイルを指定された
		string sfen;
		ifstream file(sfenFile);

		if (!file.is_open())
		{
			cerr << "Unable to open file " << sfenFile << endl;
			exit(EXIT_FAILURE);
		}

		while (getline(file, sfen)) {
			if (!sfen.empty()) {
				if (sfen.compare(0, 5, "sfen ") == 0) {
					sfen.erase(0, 5);
				}
				sfens.push_back(sfen);
			}
		}
		file.close();
	} else {
		sfens = ComShogi;
	}
#if defined(NANOHA) && defined(USE_DFPN)
//			MateTT.resize(Options["MateHashMB"]);	未実装
#endif

	// ファイル出力する？
	FILE *fp = NULL;	// 出力用
	string prefix;
	int width = 1;
	if (bOut) {
		fp = fopen(outFile.c_str(), "w");
		if (fp == NULL) {
			perror(outFile.c_str());
			exit(EXIT_FAILURE);
		}
		if (sfenFile != "default") {
			prefix = sfenFile;
			size_t npos = prefix.rfind(".sfen");
			prefix.replace(npos, 5, "");
		} else {
			prefix = "ComShogi";
		}
		if (sfens.size() >= 10000) {
			width = 5;
		} else if (sfens.size() >= 1000) {
			width = 4;
		} else if (sfens.size() >= 100) {
			width = 3;
		} else if (sfens.size() >= 10) {
			width = 2;
		}
	}
	cerr << "prefix  = " << prefix << ", width = " << width << endl;

	// Ok, let's start the benchmark !
	int64_t totalNodes = 0;
	int64_t totalTNodes = 0;
	int64_t nodes, tnodes;
	TimePoint elapsed = now();
	Position pos;

	for (size_t i = 0; i < sfens.size(); i++)
	{
		StateListPtr states(new std::deque<StateInfo>(1));
		pos.set(sfens[i], &states->back(), Threads.main());

		TimePoint rap_time = now();
		cerr << "\nBench position: " << i + 1 << '/' << sfens.size() << endl;

		{
			limits.startTime = now();
			Threads.start_thinking(pos, states, limits);
			Threads.main()->wait_for_search_finished();
			totalNodes  += (nodes  = Threads.nodes_searched());
			totalTNodes += (tnodes = Threads.tnodes_searched());
		}
		// [ToDo] 本当は複数のスレッドから最善手を見つけるべし。
		Move move = Threads.main()->rootMoves[0].pv[0];

		rap_time = now() - rap_time;
		if (bOut) {
			char buf[16];
			switch (width) {
			case 5:
				sprintf(buf, "%05zu", i+1);
				break;
			case 4:
				sprintf(buf, "%04zu", i+1);
				break;
			case 3:
				sprintf(buf, "%03zu", i+1);
				break;
			case 2:
				sprintf(buf, "%02zu", i+1);
				break;
			case 1:
			default:
				sprintf(buf, "%zu", i+1);
				break;
			}
			double nps = (rap_time > 0) ? 1000.0*(nodes + tnodes) / rap_time : 0;
			fprintf(fp, "%s%s.%s\t%s\t%6.3f\t" PRI64 "\t" PRI64 "\t%6.3f\t0\n",
			            prefix.c_str(), buf, suffix.c_str(),
			            move_to_kif(move).c_str(),
			            rap_time / 1000.0,
			            nodes, tnodes, nps);
		}
	}

	elapsed = now() - elapsed;

	cerr << "\n==============================="
	     << "\nTotal time (ms) : " << elapsed
	     << "\nNodes searched  : " << totalNodes
	     << "\nTNodes searched : " << totalTNodes
	     << "\nNodes/sec(all)  : " << static_cast<int>((totalNodes+totalTNodes) / (elapsed / 1000.0)) << endl;

	if (bOut) {
		fclose(fp);
	}
}

#if defined(USE_DFPN)
void solve_mate(istream& is)
{
	string token;
	string arg;
	vector<string> sfens;
	Search::LimitsType limits;

	// Assign default values to missing arguments
	string ttSize   = "1";
	string threads  = "1";
	string valStr   = "10";
	string sfenFile = "default";
	string valType  = "time";
	string outFile  = "";
	string suffix   = "mal";
	bool bOut = false;

	// -hash N	ハッシュサイズ(MB)
	// -threads N	スレッド数
	// -sec N	秒数
	// -nodes N	ノード数
	// -depth N	深さ
	// -o output file	結果を書き出すファイル
	// -suffix suffix	ファイル中に書き出す拡張子を指定する
	while (is >> token) {
		cerr << "token:" << token << endl;
		if (token[0] == '-') {
			if (is >> arg) {
				if (token == "-hash") {
					ttSize = arg;
				} else if (token == "-threads") {
					threads = arg;
				} else if (token == "-sec") {
					valType = "time";
					valStr = arg;
				} else if (token == "-nodes") {
					valType = "nodes";
					valStr = arg;
				} else if (token == "-o") {
					outFile = arg;
					bOut = true;
				} else if (token == "-suffix") {
					suffix = arg;
				} else {
					cerr << "Error!:token = " << token << endl;
					exit(EXIT_FAILURE);
				}
			} else {
				cerr << "Error!:token = " << token << endl;
				exit(EXIT_FAILURE);
			}
		} else {
			sfenFile = token;
			break;
		}
	}
	if (sfenFile == "default") suffix = "";
	cerr << "sfenFile = " << sfenFile << endl;
	cerr << "ttSize   = " << ttSize << endl;
	cerr << "threads  = " << threads << endl;
	cerr << "valType  = " << valType << endl;
	cerr << "valStr   = " << valStr << endl;
	cerr << "outFile  = " << outFile << endl;
	cerr << "output   = " << (bOut ? "true" : "false") << endl;
	cerr << "suffix   = " << suffix << endl;

	Options["MateHashMB"] = ttSize;
	Options["Threads"] = threads;

	// Search should be limited by nodes, time or depth ?
	if (valType == "nodes")
		limits.nodes = atoi(valStr.c_str());
	else if (valType == "time")
		limits.movetime = 1000 * atoi(valStr.c_str()); // movetime is in ms
	else
		limits.depth = atoi(valStr.c_str());

#if defined(USE_DFPN)
///	MateTT.resize(atoi(ttSize.c_str()));	未実装.
#endif
	// Do we need to load positions from a given SFEN file ?
	if (sfenFile != "default")
	{
		// ファイルを指定された
		string sfen;
		ifstream file(sfenFile);

		if (!file.is_open()) {
			cerr << "Unable to open file " << sfenFile << endl;
			exit(EXIT_FAILURE);
		}

		while (getline(file, sfen)) {
			if (!sfen.empty()) {
				if (sfen.compare(0, 5, "sfen ") == 0) {
					sfen.erase(0, 5);
				}
				sfens.push_back(sfen);
			}
		}
		file.close();
	} else {
		sfens = zuko;
	}

	// ファイル出力する？
	FILE *fp = NULL;	// 出力用
	string prefix;
	int width = 1;
	if (bOut) {
		fp = fopen(outFile.c_str(), "w");
		if (fp == NULL) {
			perror(outFile.c_str());
			exit(EXIT_FAILURE);
		}
		if (sfenFile != "default") {
			prefix = sfenFile;
			size_t npos = prefix.rfind(".sfen");
			prefix.replace(npos, 5, "");
		} else {
			prefix = "zuko";
		}
	}
	if (sfens.size() >= 10000) {
		width = 5;
	} else if (sfens.size() >= 1000) {
		width = 4;
	} else if (sfens.size() >= 100) {
		width = 3;
	} else if (sfens.size() >= 10) {
		width = 2;
	}
	cerr << "prefix  = " << prefix << ", width = " << width << endl;

	SearchMateDFPN SearchMate;

	// Ok, let's start the benchmark !
	int64_t totalNodes = 0;
	int64_t totalTNodes = 0;
	TimePoint elapsed = now();

	int limit_nodes = 10000;
	if (limits.nodes) {
		limit_nodes = limits.nodes + 1000;
	} else if (limits.movetime) {
		limit_nodes = 150 * limits.movetime + 1000;
	} else {
		limit_nodes = 150 * 10000;
	}

	int mated = 0;
	int unsolved = 0;
	int nomate = 0;
	Move m;
	Position pos;
	for (size_t i = 0; i < sfens.size(); i++)
	{
		StateListPtr states(new std::deque<StateInfo>(1));
		pos.set(sfens[i], &states->back(), Threads.main());

		TimePoint rap_time = now();
		cerr << "\nBench position: " << i + 1 << '/' << sfens.size() << endl;

		int nodes = limit_nodes;
///		MateTT.new_search();
		MateHashDFPN::Clear();
		const int value = SearchMate.Mate(pos.side_to_move(), pos, 0, MAX_MATE_PLY, m, nodes);

		totalNodes  += pos.nodes_searched();
		totalTNodes += pos.tnodes_searched();
		rap_time = now() - rap_time;
		if (rap_time == 0) rap_time = 1;
		if (value == VALUE_MATE) {
			mated++;
			cout << "MATE    : " << move_to_csa(m) << ",    used nodes="
			     << limit_nodes - nodes << ",  " << (limit_nodes - nodes) / rap_time << endl;
		} else if (value == -VALUE_MATE) {
			nomate++;
			cout << "NOMATE,         used nodes=" 
			     << limit_nodes - nodes << ",  " << (limit_nodes - nodes) / rap_time << endl;
		} else {
			unsolved++;
			cout << "UNSOLVED,       used nodes=" 
			     << limit_nodes - nodes << ",  " << (limit_nodes - nodes) / rap_time << endl;
		}
		{
			char buf[16];
			switch (width) {
			case 5:
				sprintf(buf, "%05zu", i+1);
				break;
			case 4:
				sprintf(buf, "%04zu", i+1);
				break;
			case 3:
				sprintf(buf, "%03zu", i+1);
				break;
			case 2:
				sprintf(buf, "%02zu", i+1);
				break;
			case 1:
			default:
				sprintf(buf, "%zu", i+1);
				break;
			}
			double nps = (rap_time > 0) ? 1000.0*(pos.nodes_searched() + pos.tnodes_searched()) / rap_time : 0;
			printf("%s%s.%s\t%s\t%6.3f\t" PRI64 "\t" PRI64 "\t%6.3f\t0\n",
			            prefix.c_str(), buf, suffix.c_str(),
			            move_to_csa(m).c_str(),
			            rap_time / 1000.0,
			            pos.nodes_searched(), pos.tnodes_searched(), nps);
			if (bOut) {
				fprintf(fp, "%s%s.%s\t%s\t%6.3f\t" PRI64 "\t" PRI64 "\t%6.3f\t0\n",
			            prefix.c_str(), buf, suffix.c_str(),
			            move_to_kif(m).c_str(),
			            rap_time / 1000.0,
			            pos.nodes_searched(), pos.tnodes_searched(), nps);
			}
		}
	}

	elapsed = now() - elapsed;

	cerr << "\n==============================="
	     << "\nTotal time (ms) : " << elapsed
	     << "\nNodes searched  : " << totalNodes
	     << "\nTNodes searched : " << totalTNodes
	     << "\nNodes/sec(all)  : " << static_cast<int>((totalNodes+totalTNodes) / (elapsed / 1000.0))
	     << "\n  mated   : " << mated
	     << "\n  nomate  : " << nomate
	     << "\n  unsolved: " << unsolved << endl;

	cerr << "\nsizeof(MateEntryDFPN)   = " << sizeof(MateEntryDFPN)
#if defined(USE_TWIN)
	     << "\nsizeof(TwinEntryDFPN)   = " << sizeof(TwinEntryDFPN)
#endif
	     << "\nsizeof(HistoryMateDFPN) = " << sizeof(HistoryMateDFPN)
	     << endl;

	if (bOut) {
		fclose(fp);
	}
}
#endif

// 静止探索のテスト.
void test_see(istream& is)
{
	string token;
	vector<string> sfenList;
	string sfenFile = (is >> token) ? token : "default";
	Value value = (is >> token) ? Value(stoi(token)) : VALUE_ZERO;

	// Do we need to load positions from a given SFEN file ?
	if (sfenFile != "default") {
		string fen;
		ifstream f(sfenFile);

		if (!f.is_open())
		{
			cerr << "Unable to open file " << sfenFile << endl;
			exit(EXIT_FAILURE);
		}

		while (getline(f, fen)) {
			if (!fen.empty()) {
				sfenList.push_back(fen);
			}
		}
		f.close();
	} else {
		sfenList = tc_see;
	}

	static const char *piece_str[] = {
		"**", "FU", "KY", "KE", "GI", "KI", "KA", "HI",
		"OU", "TO", "NY", "NK", "NG", "++", "UM", "RY",
	};
	ExtMove ss1[MAX_MOVES];
	ExtMove ss2[MAX_MOVES];
	Position pos;
	for (size_t i = 0; i < sfenList.size(); i++)
	{
		StateListPtr states(new std::deque<StateInfo>(1));
		pos.set(sfenList[i], &states->back(), Threads.main());

		// 指し手生成のテスト
		//  合法手の数＝captureの数＋non-captureの数になるか？
		ExtMove *legal = generate<LEGAL>(pos, ss1);
		ExtMove *mcap  = generate<CAPTURES>(pos, ss2);
		ExtMove *mall  = generate<QUIETS>(pos, mcap);
		if (legal - ss1 != mall - ss2) {
			cerr << "Error!:legal=" << legal-ss1 << ", capture=" << mcap - ss2 << ", noncapture=" << mall - mcap << endl;
			cerr << "Legal moves:\n";
			int j;
			for (j = 0; &ss1[j] != legal; j++) {
				cerr << move_to_csa(ss1[j].move) << " ";
			}
			cerr << "\nCapture moves:\n";
			for (j = 0; &ss2[j] != mcap; j++) {
				cerr << move_to_csa(ss2[j].move) << " ";
			}
			cerr << "\nCapture moves:\n";
			for (; &ss2[j] != mall; j++) {
				cerr << move_to_csa(ss2[j].move) << " ";
			}
			cerr << endl;
		}

		cerr << "\nBench position: " << i + 1 << '/' << sfenList.size() << "  cap moves:" << mcap - ss2  << "  non-cap moves:" << mall - mcap << endl;

		pos.print_csa();
		for (int j = 0; mall != &ss2[j]; j++) {
			Move m = ss2[j].move;
			int to = to_sq(m);
			if (pos.exist_effect<BLACK>(to) == 0 || pos.exist_effect<WHITE>(to) == 0) continue;
			int n = pos.see_ge(m, value);
			if (capture_of(m) != EMP) {
				// 駒を取る
				if (is_promotion(m)) {
					cerr << "     " << move_to_csa(ss2[j].move) << "*: cap=";
				} else {
					cerr << "     " << move_to_csa(ss2[j].move) << " : cap=";
				}
				cerr << piece_str[type_of(capture_of(m))] << ",  see=" << n << endl;
			} else {
				// 駒を取らない
				if (is_promotion(m)) {
					cerr << "     " << move_to_csa(ss2[j].move) << "*: see=" << n << endl;
				} else if (n != 0) {
					cerr << "     " << move_to_csa(ss2[j].move) << " : see=" << n << endl;
				}
			}
		}
	}
}
