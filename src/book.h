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

#if !defined(BOOK_H_INCLUDED)
#define BOOK_H_INCLUDED

#include <map>
#include <cstring>	// for memcmp()

class Position;
struct ExtMove;

// 定跡データのキー
struct BookKey {
	unsigned char data[32];

	bool operator < (const BookKey &b) const {
		return memcmp(data, b.data, sizeof(BookKey)) < 0;
	}
};

// 定跡データの中身
struct BookEntry {
	short eval;				// 評価値.
	unsigned short hindo;	// 頻度(出現数).
	unsigned short swin;	// 先手勝ち数.
	unsigned short gwin;	// 後手勝ち数.
	uint32_t progress;	// 進行度.
	Move next[4];		// 次の1手(最大4手まで).
	uint8_t unused[4];	// 未使用.
};

// 定跡のクラス
class Book {
private:
	typedef std::map<BookKey, BookEntry> Joseki_type;
	typedef Joseki_type::value_type Joseki_value;
	Joseki_type joseki;
	Book(const Book&);				// warning対策.
	Book& operator = (const Book&);	// warning対策.
public:
	Book();
	~Book();
	// 初期化
	void open(const std::string& fileName);
	void close();
	// 定跡データから、現在の局面kの合法手がその局面でどれくらいの頻度で指されたかを返す。
	void fromJoseki(Position &pos, int &mNum, ExtMove moves[], BookEntry data[]);
	// 現在の局面がどのくらいの頻度で指されたか定跡データを調べる
	int getHindo(const Position &pos);

	// 
	size_t size() const {return joseki.size();}

	Move get_move(Position& pos, bool findBestMove);

	static_assert(sizeof(BookEntry) == 32, "BookEntry size incorrect");
};

extern Book *book;

#endif // !defined(BOOK_H_INCLUDED)
