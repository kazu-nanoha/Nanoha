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

#ifndef MOVEGEN_H_INCLUDED
#define MOVEGEN_H_INCLUDED

#include "types.h"

class Position;

enum GenType {
  CAPTURES,             // ‹î‚ğæ‚éè
  QUIETS,               // ‹î‚ğæ‚ç‚È‚¢è
  QUIET_CHECKS,         // ‹î‚ğæ‚ç‚È‚¢‰¤è
#if defined(NANOHA)
  CHECKS,               // ‰¤è
#endif
  EVASIONS,             // ‰¤è‰ñ”ğè
  NON_EVASIONS,         // ‰¤è‚ª‚©‚©‚Á‚Ä‚¢‚È‚¢‚Ì‡–@è¶¬
  LEGAL                 // ‡–@è¶¬
};

struct ExtMove {
  Move move;
  Value value;

  operator Move() const { return move; }
  void operator=(Move m) { move = m; }
};

inline bool operator<(const ExtMove& f, const ExtMove& s) {
  return f.value < s.value;
}

template<GenType>
ExtMove* generate(const Position& pos, ExtMove* moveList);

/// The MoveList struct is a simple wrapper around generate(). It sometimes comes
/// in handy to use this class instead of the low level generate() function.
template<GenType T>
struct MoveList {

  explicit MoveList(const Position& pos) : last(generate<T>(pos, moveList)) {}
  const ExtMove* begin() const { return moveList; }
  const ExtMove* end() const { return last; }
  size_t size() const { return last - moveList; }
  bool contains(Move move) const {
    for (const auto& m : *this) if (m == move) return true;
    return false;
  }

private:
  ExtMove moveList[MAX_MOVES], *last;
};

#endif // #ifndef MOVEGEN_H_INCLUDED
