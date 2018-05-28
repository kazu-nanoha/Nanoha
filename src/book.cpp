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

#include <cassert>
#include <iostream>
#include <cstdio>
#include <algorithm>

#include "types.h"
#include "book.h"
#include "movegen.h"
#include "position.h"
#include "misc.h"
#include "thread.h"
#include "uci.h"

Book *book;

namespace {
	PRNG rng(now());
}

Book::Book() : joseki()
{
	for (int i = abs(now() % 10000); i > 0; i--)
		rng.sparse_rand<unsigned>();
}

Book::~Book() {}
void Book::open(const std::string& fileName)
{
	FILE *fp = fopen(fileName.c_str(), "rb");
	if (fp == NULL) {
		perror(fileName.c_str());
		return;
	}

	BookKey key;
	BookEntry data;

	for (;;) {
		size_t n;
		n = fread(&key, sizeof(key), 1, fp);
		if (n == 0) break;
		n = fread(&data, sizeof(data), 1, fp);
		if (n == 0) break;
		Joseki_type::iterator p;
		p = joseki.find(key);
		if (p == joseki.end()) {
			joseki.insert(Joseki_value(key, data));
		} else {
			output_info("Error!:Duplicated opening data\n");
		}
	}
///	output_info("Book:%d entries\n", joseki.size());

	fclose(fp);
}
void Book::close(){}
// 定跡データから、現在の局面kの合法手がその局面でどれくらいの頻度で指されたかを返す。
void Book::fromJoseki(Position &pos, int &mNum, ExtMove moves[], BookEntry data[])
{
	sync_cout << "info string entry fromJoseki()." << sync_endl;
	BookKey key;
	BookEntry d_null;
	ExtMove *last = generate<LEGAL>(pos, moves);
	mNum = last - moves;
	memset(&d_null, 0, sizeof(d_null));

	int i;
	StateInfo newSt;
	for (i = 0; i < mNum; i++) {
		Move m = moves[i].move;
		pos.do_move(m, newSt);
		int ret = pos.EncodeHuffman(key.data);
		pos.undo_move(m);
		if (ret < 0) {
			pos.print_csa(m);
			output_info("Error!:Huffman encode!\n");
			continue;
		}
		Joseki_type::iterator p;
		p = joseki.find(key);
		if (p == joseki.end()) {
			// データなし
			data[i] = d_null;
		} else {
			// データあり
			data[i] = p->second;
		}
	}
	sync_cout << "info string exit fromJoseki().  moves = " << mNum<< sync_endl;
}

// 現在の局面がどのくらいの頻度で指されたか定跡データを調べる
int Book::getHindo(const Position &pos)
{
	BookKey key;
	int hindo = 0;

	int ret = pos.EncodeHuffman(key.data);
	if (ret < 0) {
		pos.print_csa();
		output_info("Error!:Huffman encode!\n");
		return 0;
	}
	Joseki_type::iterator p;
	p = joseki.find(key);
	if (p != joseki.end()) {
		// データあり
		hindo = p->second.hindo;
	}

	return hindo;
}

Move Book::get_move(Position& pos_ori, bool findBestMove)
{
	StateListPtr states(new std::deque<StateInfo>(1));
	Position pos;
	pos.set(pos_ori.fen(), &states->back(), Threads.main());
	// 定跡データに手があればそれを使う.
	// 新形式を優先する
	if (size() > 0) {
		sync_cout << "info string search book" << size() << " entries." << sync_endl;
		int teNum = 0;
		ExtMove moves[MAX_MOVES];
		BookEntry hindo2[MAX_MOVES];
		BookEntry candidate[4];
		int i;
		memset(candidate, 0, sizeof(candidate));

		fromJoseki(pos, teNum, moves, hindo2);
		// 一番勝率の高い手を選ぶ。
		float swin = 0.5f;
		float win_max = 0.0f;
		int max = -1;
		int max_hindo = -1;
		// 極端に少ない手は選択しない ⇒最頻出数の10分の1以下とする
		for (i = 0; i < teNum; i++) {
			if (max_hindo < hindo2[i].hindo) {
				max_hindo = hindo2[i].hindo;
			}
		}
		for (i = 0; i < teNum; i++) {
			if (hindo2[i].hindo > 0) {
				// 多いものを候補に入れる
				if (candidate[3].hindo < hindo2[i].hindo) {
					for (int j = 0; j < 4; j++) {
						if (candidate[j].hindo < hindo2[i].hindo) {
							for (int k = 3; k > j; k--) {
								candidate[k] = candidate[k-1];
							}
							candidate[j] = hindo2[i];
							candidate[j].eval = i;
							break;
						}
					}
				}

				// 勝率算出
				if (hindo2[i].swin + hindo2[i].gwin > 0) {
					swin = hindo2[i].swin / float(hindo2[i].swin + hindo2[i].gwin);
				} else {
					swin = 0.5f;
				}
				if (pos.side_to_move() != BLACK) swin = 1.0f - swin;

				// 極端に少ない手は選択しない ⇒最頻出数の10分の1以下とする
				if (max_hindo / 10 < hindo2[i].hindo && win_max < swin) {
					max = i;
					win_max = swin;
				}
			}
		}

		if (max >= 0) {
			if (!findBestMove) {
				// 乱数で返す
				int n = std::min(teNum, 4);
				int total = 0;
				for (i = 0; i < n; i++) {
					total += candidate[i].hindo;
				}
				float p = rng.rand<unsigned short>() / 65536.0f;
				max = candidate[0].eval;
				int sum = 0;
				for (i = 0; i < n; i++) {
					sum += candidate[i].hindo;
					if (float(sum) / total >= p) {
						max = candidate[i].eval;
						break;
					}
				}
				if (hindo2[max].swin + hindo2[max].gwin > 0) {
					swin = hindo2[max].swin / float(hindo2[max].swin + hindo2[max].gwin);
					if (pos.side_to_move() != BLACK) swin = 1.0f - swin;
				} else {
					swin = 0.5f;
				}
			}
			// もっともよさそうな手を返す
			sync_cout << "info string " << swin*100.0f << ", P(" << hindo2[max].swin << ", " << hindo2[max].gwin << ") : " << UCI::move(moves[max].move) << sync_endl;
			return moves[max].move;
		} else {
			sync_cout << "info string No book data(plys=" << pos.game_ply() << ")" << sync_endl;
		}
	}

	return MOVE_NONE;
}
