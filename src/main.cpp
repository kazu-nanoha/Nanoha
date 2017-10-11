/*
  Nanoha, a USI shogi(japanese-chess) playing engine derived from Stockfish, a UCI chess playing engine.
  Copyright (C) 2004-2008 Tord Romstad (Glaurung author)
  Copyright (C) 2008-2015 Marco Costalba, Joona Kiiski, Tord Romstad
  Copyright (C) 2015-2017 Marco Costalba, Joona Kiiski, Gary Linscott, Tord Romstad
  Copyright (C) 2014-2017 Kazuyuki Kawabata

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

#include <iostream>

#if defined(NANOHA)
#include "evaluate.h"
#else
#include "bitboard.h"
#endif
#include "position.h"
#include "search.h"
#include "thread.h"
#include "tt.h"
#include "uci.h"
#if !defined(NANOHA)
#include "syzygy/tbprobe.h"

namespace PSQT {
  void init();
}
#endif

int main(int argc, char* argv[]) {
#if defined(NANOHA)
	load_name(argv[0]);
#endif

#if !defined(NANOHA)
  std::cout << engine_info() << std::endl;
#endif
  UCI::init(Options);
#ifdef NANOHA
  init_application_once();
#endif
#if !defined(NANOHA)
  PSQT::init();
  Bitboards::init();
#endif
  Position::init();
#if !defined(NANOHA)
  Bitbases::init();
#endif
  Search::init();
#if defined(NANOHA)
  Eval::init();
#else
  Pawns::init();
#endif
  Threads.init();
#if !defined(NANOHA)
  Tablebases::init(Options["SyzygyPath"]);
#endif
  TT.resize(Options["Hash"]);

  UCI::loop(argc, argv);

  Threads.exit();
  return 0;
}
