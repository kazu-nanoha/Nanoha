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

#include <iostream>
#include <sstream>
#include <string>

#include "evaluate.h"
#include "movegen.h"
#include "position.h"
#include "search.h"
#include "thread.h"
#include "timeman.h"
#include "uci.h"
#if defined(NANOHA)
#if defined(USE_DFPN)
#include "SearchMateDFPN.h"	// 詰めルーチン用ヘッダ
#endif
#else
#include "syzygy/tbprobe.h"
#endif

using namespace std;

extern void benchmark(const Position& pos, istream& is);
#if defined(NANOHA)
extern void bench_mate(istream& is, const int mateplys);
extern void bench_genmove(istream& is);
extern void bench_eval(istream& is);
extern void bench_movesyss(istream& is);
extern void bench_see(istream& is);
extern void solve_problem(istream& is);
#if defined(USE_DFPN)
extern void solve_mate(istream& is);
#endif
#endif

namespace {
#if defined(NANOHA)
	// SFEN string of the initial position, normal shogi
	const char* StartFEN = "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1";
#else
  // FEN string of the initial position, normal chess
  const char* StartFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
#endif

  // A list to keep track of the position states along the setup moves (from the
  // start position to the position just before the search starts). Needed by
  // 'draw by repetition' detection.
  StateListPtr States(new std::deque<StateInfo>(1));


  // position() is called when engine receives the "position" UCI command.
  // The function sets up the position described in the given FEN string ("fen")
  // or the starting position ("startpos") and then makes the moves given in the
  // following move list ("moves").

  void position(Position& pos, istringstream& is) {

    Move m;
    string token, fen;

    is >> token;

    if (token == "startpos")
    {
        fen = StartFEN;
        is >> token; // Consume "moves" token if any
    }
#if defined(NANOHA)
    else if (token == "sfen")
#else
    else if (token == "fen")
#endif
        while (is >> token && token != "moves")
            fen += token + " ";
    else
        return;

    States = StateListPtr(new std::deque<StateInfo>(1));
#if defined(NANOHA)
    pos.set(fen, &States->back(), Threads.main());
#else
    pos.set(fen, Options["UCI_Chess960"], &States->back(), Threads.main());
#endif

    // Parse move list (if any)
    while (is >> token && (m = UCI::to_move(pos, token)) != MOVE_NONE)
    {
        States->push_back(StateInfo());
        pos.do_move(m, States->back());
    }
  }


  // setoption() is called when engine receives the "setoption" UCI command. The
  // function updates the UCI option ("name") to the given value ("value").

  void setoption(istringstream& is) {

    string token, name, value;

    is >> token; // Consume "name" token

    // Read option name (can contain spaces)
    while (is >> token && token != "value")
        name += string(" ", name.empty() ? 0 : 1) + token;

    // Read option value (can contain spaces)
    while (is >> token)
        value += string(" ", value.empty() ? 0 : 1) + token;

    if (Options.count(name))
        Options[name] = value;
    else
        sync_cout << "No such option: " << name << sync_endl;
  }


  // go() is called when engine receives the "go" UCI command. The function sets
  // the thinking time and other parameters from the input string, then starts
  // the search.

  void go(Position& pos, istringstream& is) {

    Search::LimitsType limits;
    string token;

    limits.startTime = now(); // As early as possible!

    while (is >> token)
        if (token == "searchmoves")
            while (is >> token)
                limits.searchmoves.push_back(UCI::to_move(pos, token));

        else if (token == "wtime")     is >> limits.time[WHITE];
        else if (token == "btime")     is >> limits.time[BLACK];
        else if (token == "winc")      is >> limits.inc[WHITE];
        else if (token == "binc")      is >> limits.inc[BLACK];
        else if (token == "movestogo") is >> limits.movestogo;
        else if (token == "depth")     is >> limits.depth;
        else if (token == "nodes")     is >> limits.nodes;
#if defined(NANOHA)
        else if (token == "movetime" || token=="byoyomi") {
            is >> limits.movetime;
            int mg = Options["ByoyomiMargin"];
            if (limits.movetime - 100 >= mg) {
                limits.movetime -= mg;
            }
        } else if (token == "mate") {
#if defined(USE_DFPN)
				is >> token;
				if (token == "infinite") {
					// "go mate infinite"が来たら実行しない.
					std::cout << "checkmate notimplemented" << std::endl;
					return;
				}
				TimePoint elapsed = now();
				SearchMateDFPN SearchMate;
//				MateTT.new_search();
				MateHashDFPN::Clear();
				int nodes = 150 * atoi(token.c_str());
				if (nodes <= 0) nodes = 300000;
				Move m;
				const int start_nodes = nodes;
				StateListPtr states(new std::deque<StateInfo>(1));
				Position position;
				position.set(pos.fen(), &states->back(), Threads.main());
				const int value = SearchMate.Mate(position.side_to_move(), position, 0, MAX_MATE_PLY, m, nodes);
				TimePoint rap_time = now();
				TimePoint solve_time = rap_time -  elapsed;
				if (solve_time == 0) solve_time = 1;
				std::cout << "info string thinking in " << rap_time -  elapsed << " ms" << std::endl;
				std::cout << "info nodes " << start_nodes - nodes
				          << " nps " << (start_nodes - nodes)*1000/(rap_time -  elapsed)
				          << " hashfull " << MateHashDFPN::hashfull() << std::endl;
				if (value == VALUE_MATE) {
					// 詰んだ
					rap_time = now();
					Move moves[MAX_MATE_PLY];
					int num = SearchMate.QueryResolt(position.side_to_move(), position, 0, moves);
					TimePoint build_time = now() - rap_time;
					std::cout << "info string building in " << build_time << " ms" << std::endl;
					std::cout << "info string total " << solve_time + build_time << " ms" << std::endl;
					std::cout << "checkmate";
					for (int i = 0; i < num; i++) {
						std::cout << " " << UCI::move(moves[i]);
					}
					std::cout << std::endl;
				} else if (value == -VALUE_MATE) {
					// 詰まない
					std::cout << "checkmate nomate" << std::endl;
				} else {
					// 不明
					std::cout << "checkmate timeout" << std::endl;
				}
#else
            std::cout << "checkmate notimplemented" << std::endl;
#endif
            return;
        }
#else
        else if (token == "movetime")  is >> limits.movetime;
#endif
        else if (token == "mate")      is >> limits.mate;
        else if (token == "infinite")  limits.infinite = 1;
        else if (token == "ponder")    limits.ponder = 1;

#if defined(NANOHA)
    Color us = pos.side_to_move();
    int mg = Options["ByoyomiMargin"];
    if (limits.inc[us] > 0) {
        limits.time[us] = (limits.time[us] < mg) ? 0 : limits.time[us] - mg;
    }
#endif

    Threads.start_thinking(pos, States, limits);
  }

} // namespace


/// UCI::loop() waits for a command from stdin, parses it and calls the appropriate
/// function. Also intercepts EOF from stdin to ensure gracefully exiting if the
/// GUI dies unexpectedly. When called with some command line arguments, e.g. to
/// run 'bench', once the command is executed the function returns immediately.
/// In addition to the UCI ones, also some additional debug commands are supported.

void UCI::loop(int argc, char* argv[]) {

  Position pos;
  string token, cmd;

#if defined(NANOHA)
  pos.set(StartFEN, &States->back(), Threads.main());
#else
  pos.set(StartFEN, false, &States->back(), Threads.main());
#endif

  for (int i = 1; i < argc; ++i)
      cmd += std::string(argv[i]) + " ";

  do {
      if (argc == 1 && !getline(cin, cmd)) // Block here waiting for input or EOF
          cmd = "quit";

      istringstream is(cmd);

      token.clear(); // getline() could return empty or blank line
      is >> skipws >> token;

      // The GUI sends 'ponderhit' to tell us to ponder on the same move the
      // opponent has played. In case Signals.stopOnPonderhit is set we are
      // waiting for 'ponderhit' to stop the search (for instance because we
      // already ran out of time), otherwise we should continue searching but
      // switching from pondering to normal search.
      if (    token == "quit"
          ||  token == "stop"
          || (token == "ponderhit" && Search::Signals.stopOnPonderhit))
      {
          Search::Signals.stop = true;
          Threads.main()->start_searching(true); // Could be sleeping
      }
      else if (token == "ponderhit")
          Search::Limits.ponder = 0; // Switch to normal search

#if defined(NANOHA)
      else if (token == "gameover") {
          Search::Signals.stop = true;
          Threads.main()->start_searching(true); // Could be sleeping
          // 将棋所の解説
          // gameover [ win | lose | draw ] ＜略＞
          // gameoverのあと、エンジンの結果に応じてwin, lose, drawのいずれかのパラメータも一緒に送られます。
#if 0
          string param;
          is >> param;
          // [ToDo] win, lose, draw に応じた処理を追加する.
#endif
      } else if (token == "usi") {
          sync_cout << "id name " << engine_info(true)
                    << "\n"       << Options
                    << "\nusiok"  << sync_endl;
      }
#else
      else if (token == "uci")
          sync_cout << "id name " << engine_info(true)
                    << "\n"       << Options
                    << "\nuciok"  << sync_endl;
#endif
#if defined(NANOHA)
      else if (token == "usinewgame")
#else
      else if (token == "ucinewgame")
#endif
      {
          Search::clear();
#if !defined(NANOHA)
          Tablebases::init(Options["SyzygyPath"]);
#endif
          Time.availableNodes = 0;
#if defined(NANOHA) && defined(USE_DFPN)
//          MateTT.resize(Options["MateHashMB"]);	未実装.
#endif
      }
      else if (token == "isready")    sync_cout << "readyok" << sync_endl;
      else if (token == "go")         go(pos, is);
      else if (token == "position")   position(pos, is);
      else if (token == "setoption")  setoption(is);

      // Additional custom non-UCI commands, useful for debugging
#if !defined(NANOHA)
      else if (token == "flip")       pos.flip();
#endif
      else if (token == "bench")      {
          benchmark(pos, is);
      }
#if defined(NANOHA)
      else if (token == "mate1")      {
          bench_mate(is, 1);
      } else if (token == "mate3")      bench_mate(is, 3);
      else if (token == "genmove")    bench_genmove(is);
      else if (token == "eval")       bench_eval(is);
      else if (token == "moves")      bench_movesyss(is);
      else if (token == "see")        bench_see(is);
      else if (token == "problem")    solve_problem(is);
#if defined(USE_DFPN)
      else if (token == "mate")       solve_mate(is);
#endif
#endif
      else if (token == "d")          sync_cout << pos << sync_endl;
#if !defined(NANOHA)
      else if (token == "eval")       sync_cout << Eval::trace(pos) << sync_endl;
#endif
      else if (token == "perft")
      {
          int depth;
          stringstream ss;

          is >> depth;
          ss << Options["Hash"]    << " "
             << Options["Threads"] << " " << depth << " current perft";

          benchmark(pos, ss);
      }
      else
          sync_cout << "Unknown command: " << cmd << sync_endl;

  } while (token != "quit" && argc == 1); // Passed args have one-shot behaviour

  Threads.main()->wait_for_search_finished();
}


/// UCI::value() converts a Value to a string suitable for use with the UCI
/// protocol specification:
///
/// cp <x>    The score from the engine's point of view in centipawns.
/// mate <y>  Mate in y moves, not plies. If the engine is getting mated
///           use negative values for y.

string UCI::value(Value v) {

  stringstream ss;

  if (abs(v) < VALUE_MATE - MAX_PLY)
#if defined(NANOHA)
      ss << "cp " << v;
#else
      ss << "cp " << v * 100 / PawnValueEg;
#endif
  else
      ss << "mate " << (v > 0 ? VALUE_MATE - v + 1 : -VALUE_MATE - v) / 2;

  return ss.str();
}


/// UCI::square() converts a Square to a string in algebraic notation (g1, a7, etc.)

std::string UCI::square(Square s) {
#if defined(NANOHA)
  return std::string{ char('0' + file_of(s)), char('a' - 1 + rank_of(s)) };
#else
  return std::string{ char('a' + file_of(s)), char('1' + rank_of(s)) };
#endif
}


/// UCI::move() converts a Move to a string in coordinate notation (g1f3, a7a8q).
/// The only special case is castling, where we print in the e1g1 notation in
/// normal chess mode, and in e1h1 notation in chess960 mode. Internally all
/// castling moves are always encoded as 'king captures rook'.

#if defined(NANOHA)
string UCI::move(Move m) {

  Square from = from_sq(m);
  Square to = to_sq(m);
  static const char *piece = ".PLNSGBR";

  if (m == MOVE_NONE)
      return "(none)";

  if (is_win(m))
      return "(win)";

  if (m == MOVE_NULL)
      return "0000";

  string move;
  if (is_drop(m)) {
      move  = piece[piece_of(m) & 7];
      move += "*" + UCI::square(to);
  } else {
      move = UCI::square(from) + UCI::square(to);
      if (is_promotion(m)) move += "+";
  }

  return move;
}
#else
string UCI::move(Move m, bool chess960) {

  Square from = from_sq(m);
  Square to = to_sq(m);

  if (m == MOVE_NONE)
      return "(none)";

  if (m == MOVE_NULL)
      return "0000";

  if (type_of(m) == CASTLING && !chess960)
      to = make_square(to > from ? FILE_G : FILE_C, rank_of(from));

  string move = UCI::square(from) + UCI::square(to);

  if (type_of(m) == PROMOTION)
      move += " pnbrqk"[promotion_type(m)];

  return move;
}
#endif

/// UCI::to_move() converts a string representing a move in coordinate notation
/// (g1f3, a7a8q) to the corresponding legal Move, if any.

Move UCI::to_move(const Position& pos, string& str) {

  if (str.length() == 5) // Junior could send promotion piece in uppercase
      str[4] = char(tolower(str[4]));

  for (const auto& m : MoveList<LEGAL>(pos))
#if defined(NANOHA)
      if (str == UCI::move(m))
#else
      if (str == UCI::move(m, pos.is_chess960()))
#endif
          return m;

  return MOVE_NONE;
}
