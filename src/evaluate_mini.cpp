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

/* なのはminiの評価ベクトルを使用する */

#include <iostream>
#include <cassert>
#include <cstdio>

#include "position.h"
#include "evaluate.h"

// 評価関数関連定義
#if defined(EVAL_NANO)
#define FV_BIN "fv_nano.bin"
#elif defined(EVAL_MINI)
#define FV_BIN "fv_mini.bin"
#endif
#define NLIST	38

#define FV_SCALE                32

#define MATERIAL            (this->st->material)

#define SQ_BKING            NanohaTbl::z2sq[kingS]
#define SQ_WKING            NanohaTbl::z2sq[kingG]
#define HAND_B              (this->hand[BLACK].h)
#define HAND_W              (this->hand[WHITE].h)

#define Inv(sq)             (nsquare-1-sq)
#define PcOnSq(k,i)         fv_kp[k][i]
#if !defined(EVAL_NANO)
#define PcPcOn(i,j)         fv_pp[i][j]
#endif

#define I2HandPawn(hand)    (((hand) & HAND_FU_MASK) >> HAND_FU_SHIFT)
#define I2HandLance(hand)   (((hand) & HAND_KY_MASK) >> HAND_KY_SHIFT)
#define I2HandKnight(hand)  (((hand) & HAND_KE_MASK) >> HAND_KE_SHIFT)
#define I2HandSilver(hand)  (((hand) & HAND_GI_MASK) >> HAND_GI_SHIFT)
#define I2HandGold(hand)    (((hand) & HAND_KI_MASK) >> HAND_KI_SHIFT)
#define I2HandBishop(hand)  (((hand) & HAND_KA_MASK) >> HAND_KA_SHIFT)
#define I2HandRook(hand)    (((hand) & HAND_HI_MASK) >> HAND_HI_SHIFT)

enum {
	promote = 8, EMPTY = 0,	/* VC++でemptyがぶつかるので変更 */
	pawn, lance, knight, silver, gold, bishop, rook, king, pro_pawn,
	pro_lance, pro_knight, pro_silver, piece_null, horse, dragon
};

enum { nhand = 7, nfile = 9,  nrank = 9,  nsquare = 81 };

enum {
	// PP用の定義
	pp_bpawn      =  -9,            		//   -9: 先手Pの位置は 9-80(72箇所)、これを  0- 71にマップ
	pp_blance     = pp_bpawn   + 72,		//   63: 先手Lの位置は 9-80(72箇所)、これを 72-143にマップ
	pp_bknight    = pp_blance  + 63,		//  126: 先手Nの位置は18-80(63箇所)、これを144-206にマップ
	pp_bsilver    = pp_bknight + 81,		//  207: 先手Sの位置は 0-80(81箇所)、これを207-287にマップ
	pp_bgold      = pp_bsilver + 81,		//  288: 先手Gの位置は 0-80(81箇所)、これを288-368にマップ
	pp_bbishop    = pp_bgold   + 81,		//  369: 先手Bの位置は 0-80(81箇所)、これを369-449にマップ
	pp_bhorse     = pp_bbishop + 81,		//  450: 先手Hの位置は 0-80(81箇所)、これを450-530にマップ
	pp_brook      = pp_bhorse  + 81,		//  531: 先手Rの位置は 0-80(81箇所)、これを531-611にマップ
	pp_bdragon    = pp_brook   + 81,		//  612: 先手Dの位置は 0-80(81箇所)、これを612-692にマップ
	pp_bend       = pp_bdragon + 81,		//  693: 先手の最終位置

	pp_wpawn      = pp_bdragon + 81,		//  693: 後手Pの位置は 0-71(72箇所)、これを 693- 764にマップ
	pp_wlance     = pp_wpawn   + 72,		//  765: 後手Lの位置は 0-71(72箇所)、これを 765- 836にマップ
	pp_wknight    = pp_wlance  + 72,		//  837: 後手Nの位置は 0-62(63箇所)、これを 837- 899にマップ
	pp_wsilver    = pp_wknight + 63,		//  900: 後手Sの位置は 0-80(81箇所)、これを 900- 980にマップ
	pp_wgold      = pp_wsilver + 81,		//  981: 後手Gの位置は 0-80(81箇所)、これを 981-1061にマップ
	pp_wbishop    = pp_wgold   + 81,		// 1062: 後手Bの位置は 0-80(81箇所)、これを1062-1142にマップ
	pp_whorse     = pp_wbishop + 81,		// 1143: 後手Hの位置は 0-80(81箇所)、これを1143-1223にマップ
	pp_wrook      = pp_whorse  + 81,		// 1224: 後手Rの位置は 0-80(81箇所)、これを1224-1304にマップ
	pp_wdragon    = pp_wrook   + 81,		// 1305: 後手Dの位置は 0-80(81箇所)、これを1305-1385にマップ
	pp_end        = pp_wdragon + 81,		// 1386: 後手の最終位置

	// K(P+H)用の定義
	kp_hand_bpawn   =    0,
	kp_hand_wpawn   =   19,
	kp_hand_blance  =   38,
	kp_hand_wlance  =   43,
	kp_hand_bknight =   48,
	kp_hand_wknight =   53,
	kp_hand_bsilver =   58,
	kp_hand_wsilver =   63,
	kp_hand_bgold   =   68,
	kp_hand_wgold   =   73,
	kp_hand_bbishop =   78,
	kp_hand_wbishop =   81,
	kp_hand_brook   =   84,
	kp_hand_wrook   =   87,
	kp_hand_end     =   90,
	kp_bpawn        =   81,
	kp_wpawn        =  162,
	kp_blance       =  225,
	kp_wlance       =  306,
	kp_bknight      =  360,
	kp_wknight      =  441,
	kp_bsilver      =  504,
	kp_wsilver      =  585,
	kp_bgold        =  666,
	kp_wgold        =  747,
	kp_bbishop      =  828,
	kp_wbishop      =  909,
	kp_bhorse       =  990,
	kp_whorse       = 1071,
	kp_brook        = 1152,
	kp_wrook        = 1233,
	kp_bdragon      = 1314,
	kp_wdragon      = 1395,
	kp_end          = 1476
};

namespace {
	short p_value[31];

#if !defined(EVAL_NANO)
	short fv_pp[pp_end][pp_end];
#endif
	short fv_kp[nsquare][kp_end];
}

namespace NanohaTbl {
	const short z2sq[] = {
		-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
		-1,  0,  9, 18, 27, 36, 45, 54, 63, 72, -1, -1, -1, -1, -1, -1,
		-1,  1, 10, 19, 28, 37, 46, 55, 64, 73, -1, -1, -1, -1, -1, -1,
		-1,  2, 11, 20, 29, 38, 47, 56, 65, 74, -1, -1, -1, -1, -1, -1,
		-1,  3, 12, 21, 30, 39, 48, 57, 66, 75, -1, -1, -1, -1, -1, -1,
		-1,  4, 13, 22, 31, 40, 49, 58, 67, 76, -1, -1, -1, -1, -1, -1,
		-1,  5, 14, 23, 32, 41, 50, 59, 68, 77, -1, -1, -1, -1, -1, -1,
		-1,  6, 15, 24, 33, 42, 51, 60, 69, 78, -1, -1, -1, -1, -1, -1,
		-1,  7, 16, 25, 34, 43, 52, 61, 70, 79, -1, -1, -1, -1, -1, -1,
		-1,  8, 17, 26, 35, 44, 53, 62, 71, 80, -1, -1, -1, -1, -1, -1,
	};
}

namespace {
	void conv_pp(short pp[pp_end][pp_end], short pp_ori[pp_bend][pp_end])
	{
		static const struct {
			int base, inv_base, start, end;
		} tbl[18] = {
			{pp_bpawn  , pp_wpawn  ,  9, 81},
			{pp_blance , pp_wlance ,  9, 81},
			{pp_bknight, pp_wknight, 18, 81},
			{pp_bsilver, pp_wsilver,  0, 81},
			{pp_bgold  , pp_wgold  ,  0, 81},
			{pp_bbishop, pp_wbishop,  0, 81},
			{pp_bhorse , pp_whorse ,  0, 81},
			{pp_brook  , pp_wrook  ,  0, 81},
			{pp_bdragon, pp_wdragon,  0, 81},
			{pp_wpawn  , pp_bpawn  ,  0, 72},
			{pp_wlance , pp_blance ,  0, 72},
			{pp_wknight, pp_bknight,  0, 63},
			{pp_wsilver, pp_bsilver,  0, 81},
			{pp_wgold  , pp_bgold  ,  0, 81},
			{pp_wbishop, pp_bbishop,  0, 81},
			{pp_whorse , pp_bhorse ,  0, 81},
			{pp_wrook  , pp_brook  ,  0, 81},
			{pp_wdragon, pp_bdragon,  0, 81},
		};
		for (size_t i = 0; i < sizeof(tbl)/sizeof(tbl[0])/ 2; i++) {
			for (int sq1 = tbl[i].start; sq1 < tbl[i].end; sq1++) {
				int p1 = tbl[i].base + sq1;
				int p1inv = tbl[i].inv_base + Inv(sq1);
				if (p1 < 0 || p1 >= pp_bend) {
					printf("error1\n");
					*(int*)0 = 0;
				}
				if (p1inv < pp_bend || p1inv >= pp_end) {
					printf("error1inv\n");
					*(int*)0 = 0;
				}
				for (size_t j = 0; j < sizeof(tbl)/sizeof(tbl[0]); j++) {
					for (int sq2 = tbl[j].start; sq2 < tbl[j].end; sq2++) {
						int p2 = tbl[j].base + sq2;
						int p2inv = tbl[j].inv_base + Inv(sq2);
						if (p2 < 0 || p2 >= pp_end) {
							printf("error2\n");
							*(int*)0 = 0;
						}
						if (p2inv < 0 || p2inv >= pp_end) {
							printf("error2inv\n");
							*(int*)0 = 0;
						}
						pp[p1][p2]       =  pp_ori[p1][p2];
						if (p2inv < pp_bend) {
							pp[p1inv][p2inv] = pp_ori[p2inv][p1inv];
						} else {
							pp[p1inv][p2inv] = -pp_ori[p1][p2];
						}
					}
				}
			}
		}

#if !defined(NDEBUG)
		// 検算
		for (int sq1 = 0; sq1 < pp_bend; sq1++) {
			for (int sq2 = 0; sq2 < pp_end; sq2++) {
				if (pp[sq1][sq2] != pp_ori[sq1][sq2]) {
					printf("sq1 = %d, sq2 = %d, pp=%d, pp_ori=%d\n", sq1, sq2, pp[sq1][sq2], pp_ori[sq1][sq2]);
				}
			}
		}
		for (int sq1 = 0; sq1 < pp_end; sq1++) {
			for (int sq2 = 0; sq2 < pp_end; sq2++) {
				if (pp[sq1][sq2] != pp[sq2][sq1]) {
					printf("sq1 = %d, sq2 = %d, pp=%d, pp=%d\n", sq1, sq2, pp[sq1][sq2], pp[sq2][sq1]);
				}
			}
		}
#endif
	}
}

void Eval::init()
{
	int iret=0;
	FILE *fp;
	const char *fname ="評価ベクトル";

	do {
		size_t size;

		fname = FV_BIN;
		fp = fopen(fname, "rb");
		if ( fp == NULL ) { iret = -2; break;}

#if !defined(EVAL_NANO)
		size = pp_bend * pp_end;
		short (*p)[pp_end] = (short (*)[pp_end])malloc(sizeof(short)*size);
		if (p == NULL) { iret = -2; break;}
		if ( fread( p, sizeof(short), size, fp ) != size )
		{
			iret = -2;
			break;
		}
		conv_pp(fv_pp, p);
		free(p);
#endif

		size = nsquare * kp_end;
		if ( fread( fv_kp, sizeof(short), size, fp ) != size ) {
			iret = -2;
			break;
		}

		if (fgetc(fp) != EOF) {
			iret = -2;
			break;
		}
	} while (0);
	if (fp) fclose( fp );

	if (iret < 0) {
//#if !defined(NDEBUG)
		std::cerr << "Can't load " FV_BIN "." << std::endl;
//#endif
#if defined(CSADLL) || defined(CSA_DIRECT)
		::MessageBox(NULL, "評価ベクトルがロードできません\n終了します", "Error!", MB_OK);
		exit(1);
#endif	// defined(CSA_DLL) || defined(CSA_DIRECT)
	}

	for (int i = 0; i < 31; i++) { p_value[i]       = 0; }

	p_value[15+pawn]       = DPawn;
	p_value[15+lance]      = DLance;
	p_value[15+knight]     = DKnight;
	p_value[15+silver]     = DSilver;
	p_value[15+gold]       = DGold;
	p_value[15+bishop]     = DBishop;
	p_value[15+rook]       = DRook;
	p_value[15+king]       = DKing;
	p_value[15+pro_pawn]   = DProPawn;
	p_value[15+pro_lance]  = DProLance;
	p_value[15+pro_knight] = DProKnight;
	p_value[15+pro_silver] = DProSilver;
	p_value[15+horse]      = DHorse;
	p_value[15+dragon]     = DDragon;

	p_value[15-pawn]          = p_value[15+pawn];
	p_value[15-lance]         = p_value[15+lance];
	p_value[15-knight]        = p_value[15+knight];
	p_value[15-silver]        = p_value[15+silver];
	p_value[15-gold]          = p_value[15+gold];
	p_value[15-bishop]        = p_value[15+bishop];
	p_value[15-rook]          = p_value[15+rook];
	p_value[15-king]          = p_value[15+king];
	p_value[15-pro_pawn]      = p_value[15+pro_pawn];
	p_value[15-pro_lance]     = p_value[15+pro_lance];
	p_value[15-pro_knight]    = p_value[15+pro_knight];
	p_value[15-pro_silver]    = p_value[15+pro_silver];
	p_value[15-horse]         = p_value[15+horse];
	p_value[15-dragon]        = p_value[15+dragon];
}

int Position::compute_material() const
{
	int v, item, itemp;
	int i;

	item  = 0;
	itemp = 0;
	for (i = KNS_FU; i <= KNE_FU; i++) {
		if (knkind[i] == SFU) item++;
		if (knkind[i] == GFU) item--;
		if (knkind[i] == STO) itemp++;
		if (knkind[i] == GTO) itemp--;
	}
	v  = item  * p_value[15+pawn];
	v += itemp * p_value[15+pro_pawn];

	item  = 0;
	itemp = 0;
	for (i = KNS_KY; i <= KNE_KY; i++) {
		if (knkind[i] == SKY) item++;
		if (knkind[i] == GKY) item--;
		if (knkind[i] == SNY) itemp++;
		if (knkind[i] == GNY) itemp--;
	}
	v += item  * p_value[15+lance];
	v += itemp * p_value[15+pro_lance];

	item  = 0;
	itemp = 0;
	for (i = KNS_KE; i <= KNE_KE; i++) {
		if (knkind[i] == SKE) item++;
		if (knkind[i] == GKE) item--;
		if (knkind[i] == SNK) itemp++;
		if (knkind[i] == GNK) itemp--;
	}
	v += item  * p_value[15+knight];
	v += itemp * p_value[15+pro_knight];

	item  = 0;
	itemp = 0;
	for (i = KNS_GI; i <= KNE_GI; i++) {
		if (knkind[i] == SGI) item++;
		if (knkind[i] == GGI) item--;
		if (knkind[i] == SNG) itemp++;
		if (knkind[i] == GNG) itemp--;
	}
	v += item  * p_value[15+silver];
	v += itemp * p_value[15+pro_silver];

	item  = 0;
	for (i = KNS_KI; i <= KNE_KI; i++) {
		if (knkind[i] == SKI) item++;
		if (knkind[i] == GKI) item--;
	}
	v += item  * p_value[15+gold];

	item  = 0;
	itemp = 0;
	for (i = KNS_KA; i <= KNE_KA; i++) {
		if (knkind[i] == SKA) item++;
		if (knkind[i] == GKA) item--;
		if (knkind[i] == SUM) itemp++;
		if (knkind[i] == GUM) itemp--;
	}
	v += item  * p_value[15+bishop];
	v += itemp * p_value[15+horse];

	item  = 0;
	itemp = 0;
	for (i = KNS_HI; i <= KNE_HI; i++) {
		if (knkind[i] == SHI) item++;
		if (knkind[i] == GHI) item--;
		if (knkind[i] == SRY) itemp++;
		if (knkind[i] == GRY) itemp--;
	}
	v += item  * p_value[15+rook];
	v += itemp * p_value[15+dragon];

	return v;
}

int Position::make_list(int * pscore, int list0[NLIST]) const
{
	int sq, i, score, sq_bk0, sq_bk1;

	score  = 0;
	sq_bk0 = SQ_BKING;
	sq_bk1 = Inv(SQ_WKING);

	static const int pp_tbl[32] = {
		-1, pp_bpawn, pp_blance, pp_bknight, pp_bsilver, pp_bgold, pp_bbishop, pp_brook,
		-1, pp_bgold, pp_bgold,  pp_bgold,   pp_bgold,   -1,       pp_bhorse,  pp_bdragon,
		-1, pp_wpawn, pp_wlance, pp_wknight, pp_wsilver, pp_wgold, pp_wbishop, pp_wrook,
		-1, pp_wgold, pp_wgold,  pp_wgold,   pp_wgold,   -1,       pp_whorse,  pp_wdragon,
	};
	static const int kp_tbl[32] = {
		-1, kp_bpawn, kp_blance, kp_bknight, kp_bsilver, kp_bgold, kp_bbishop, kp_brook,
		-1, kp_bgold, kp_bgold,  kp_bgold,   kp_bgold,   -1,       kp_bhorse,  kp_bdragon,
		-1, kp_wpawn, kp_wlance, kp_wknight, kp_wsilver, kp_wgold, kp_wbishop, kp_wrook,
		-1, kp_wgold, kp_wgold,  kp_wgold,   kp_wgold,   -1,       kp_whorse,  kp_wdragon,
	};
	// 駒番号：1～2が玉、3～40が玉以外
	int nlist  = 0;
	for (int kn = 3; kn <= 40; kn++) {
		const int z = knpos[kn];
		if (!OnBoard(z)) continue;			// 持ち駒除く
		int index = pp_tbl[knkind[kn]];
		sq = NanohaTbl::z2sq[z];
		list0[nlist++] = index + sq;

		score += PcOnSq(sq_bk0, kp_tbl[knkind[kn]       ] + sq);
		score -= PcOnSq(sq_bk1, kp_tbl[knkind[kn] ^ GOTE] + Inv(sq));
	}

	assert( nlist <= NLIST );

{
	static int disp = 1;
	if (disp) {
		disp = 0;
		for (i = 0; i < nlist; i++) {
		  printf("%d ", list0[i]);
		}
		printf("\n");
	}
}

	*pscore += score;
	return nlist;
}

int Position::evaluate(const Color us) const
{
	int list0[NLIST];
	int nlist, score, sq_bk, sq_wk, sum;
	static int count=0;
	count++;

	sum = 0;
	sq_bk = SQ_BKING;
	sq_wk = Inv( SQ_WKING );

	sum += fv_kp[sq_bk][kp_hand_bpawn   + I2HandPawn(HAND_B)];
	sum += fv_kp[sq_bk][kp_hand_wpawn   + I2HandPawn(HAND_W)];
	sum -= fv_kp[sq_wk][kp_hand_bpawn   + I2HandPawn(HAND_W)];
	sum -= fv_kp[sq_wk][kp_hand_wpawn   + I2HandPawn(HAND_B)];

	sum += fv_kp[sq_bk][kp_hand_blance  + I2HandLance(HAND_B)];
	sum += fv_kp[sq_bk][kp_hand_wlance  + I2HandLance(HAND_W)];
	sum -= fv_kp[sq_wk][kp_hand_blance  + I2HandLance(HAND_W)];
	sum -= fv_kp[sq_wk][kp_hand_wlance  + I2HandLance(HAND_B)];

	sum += fv_kp[sq_bk][kp_hand_bknight + I2HandKnight(HAND_B)];
	sum += fv_kp[sq_bk][kp_hand_wknight + I2HandKnight(HAND_W)];
	sum -= fv_kp[sq_wk][kp_hand_bknight + I2HandKnight(HAND_W)];
	sum -= fv_kp[sq_wk][kp_hand_wknight + I2HandKnight(HAND_B)];

	sum += fv_kp[sq_bk][kp_hand_bsilver + I2HandSilver(HAND_B)];
	sum += fv_kp[sq_bk][kp_hand_wsilver + I2HandSilver(HAND_W)];
	sum -= fv_kp[sq_wk][kp_hand_bsilver + I2HandSilver(HAND_W)];
	sum -= fv_kp[sq_wk][kp_hand_wsilver + I2HandSilver(HAND_B)];

	sum += fv_kp[sq_bk][kp_hand_bgold   + I2HandGold(HAND_B)];
	sum += fv_kp[sq_bk][kp_hand_wgold   + I2HandGold(HAND_W)];
	sum -= fv_kp[sq_wk][kp_hand_bgold   + I2HandGold(HAND_W)];
	sum -= fv_kp[sq_wk][kp_hand_wgold   + I2HandGold(HAND_B)];

	sum += fv_kp[sq_bk][kp_hand_bbishop + I2HandBishop(HAND_B)];
	sum += fv_kp[sq_bk][kp_hand_wbishop + I2HandBishop(HAND_W)];
	sum -= fv_kp[sq_wk][kp_hand_bbishop + I2HandBishop(HAND_W)];
	sum -= fv_kp[sq_wk][kp_hand_wbishop + I2HandBishop(HAND_B)];

	sum += fv_kp[sq_bk][kp_hand_brook   + I2HandRook(HAND_B)];
	sum += fv_kp[sq_bk][kp_hand_wrook   + I2HandRook(HAND_W)];
	sum -= fv_kp[sq_wk][kp_hand_brook   + I2HandRook(HAND_W)];
	sum -= fv_kp[sq_wk][kp_hand_wrook   + I2HandRook(HAND_B)];


	score = 0;
	nlist = make_list( &score, list0 );

#if !defined(EVAL_NANO)
	for (int i = 0; i < nlist; i++ )
	{
		assert(i < nlist);
		const int k0 = list0[i];
		for (int j = i+1; j < nlist; j++ )
		{
			const int l0 = list0[j];
			sum += PcPcOn( k0, l0 );
		}
	}
#endif

	score += sum;
	score /= FV_SCALE;

	score += MATERIAL;

	score = (us == BLACK) ? score : -score;

	return score;
}

Value Eval::evaluate(const Position& pos)
{
	const Color us = pos.side_to_move();
	return Value(pos.evaluate(us));
}
