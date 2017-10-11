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
#ifndef THREAD_H_INCLUDED
#define THREAD_H_INCLUDED

#include <atomic>
#include <bitset>
#include <condition_variable>
#include <mutex>
#include <thread>
#include <vector>

#if !defined(NANOHA)
#include "material.h"
#endif
#include "movepick.h"
#if !defined(NANOHA)
#include "pawns.h"
#endif
#include "position.h"
#include "search.h"
#include "thread_win32.h"


/// Thread struct keeps together all the thread-related stuff. We also use
/// per-thread pawn and material hash tables so that once we get a pointer to an
/// entry its life time is unlimited and we don't have to care about someone
/// changing the entry under our feet.

class Thread {

  std::thread nativeThread;
  Mutex mutex;
  ConditionVariable sleepCondition;
  bool exit, searching;

public:
  Thread();
  virtual ~Thread();
  virtual void search();
  void idle_loop();
  void start_searching(bool resume = false);
  void wait_for_search_finished();
  void wait(std::atomic_bool& condition);

#if !defined(NANOHA)
  Pawns::Table pawnsTable;
  Material::Table materialTable;
  Endgames endgames;
#endif
  size_t idx, PVIdx;
  int maxPly, callsCnt;
#if !defined(NANOHA)
  uint64_t tbHits;
#endif

  Position rootPos;
  Search::RootMoves rootMoves;
  Depth rootDepth;
  Depth completedDepth;
  std::atomic_bool resetCalls;
  MoveStats counterMoves;
  HistoryStats history;
  CounterMoveHistoryStats counterMoveHistory;
};


/// MainThread is a derived class with a specific overload for the main thread

struct MainThread : public Thread {
  virtual void search();

  bool easyMovePlayed, failedLow;
  double bestMoveChanges;
  Value previousScore;
};


/// ThreadPool struct handles all the threads-related stuff like init, starting,
/// parking and, most importantly, launching a thread. All the access to threads
/// data is done through this class.

struct ThreadPool : public std::vector<Thread*> {

  void init(); // No constructor and destructor, threads rely on globals that should
  void exit(); // be initialized and valid during the whole thread lifetime.

  MainThread* main() { return static_cast<MainThread*>(at(0)); }
  void start_thinking(Position&, StateListPtr&, const Search::LimitsType&);
  void read_uci_options();
  uint64_t nodes_searched() const;
#if defined(NANOHA)
  uint64_t tnodes_searched() const;
#endif
#if !defined(NANOHA)
  uint64_t tb_hits() const;
#endif

private:
  StateListPtr setupStates;
};

extern ThreadPool Threads;

#endif // #ifndef THREAD_H_INCLUDED
