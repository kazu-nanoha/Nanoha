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

#include <algorithm>
#include <cassert>
#include <cstddef> // For offsetof()
#include <cstring> // For std::memset, std::memcmp
#include <iomanip>
#include <sstream>
#if defined(NANOHA)
#include <iostream>
#else
#include "bitboard.h"
#endif
#include "misc.h"
#include "movegen.h"
#include "position.h"
#include "thread.h"
#include "tt.h"
#include "uci.h"
#if !defined(NANOHA)
#include "syzygy/tbprobe.h"
#endif

using std::string;

#if defined(NANOHA)
#define MG(x)    Value(x)
#define EG(x)    Value((x)*3/4)	// [ToDo] 終盤は駒得より効率重視.
Value PieceValue[PHASE_NB][PIECE_NB] = {
	{ VALUE_ZERO, MG(DPawn),    MG(DLance),    MG(DKnight),    MG(DSilver),    MG(DGold),  MG(DBishop), MG(DRook),
	  MG(DKing),  MG(DProPawn), MG(DProLance), MG(DProKnight), MG(DProSilver), VALUE_ZERO, MG(DHorse),  MG(DDragon),
	  VALUE_ZERO, MG(DPawn),    MG(DLance),    MG(DKnight),    MG(DSilver),    MG(DGold),  MG(DBishop), MG(DRook),
	  MG(DKing),  MG(DProPawn), MG(DProLance), MG(DProKnight), MG(DProSilver), VALUE_ZERO, MG(DHorse),  MG(DDragon) },
	{ VALUE_ZERO, EG(DPawn),    EG(DLance),    EG(DKnight),    EG(DSilver),    EG(DGold),  EG(DBishop), EG(DRook),
	  EG(DKing),  EG(DProPawn), EG(DProLance), EG(DProKnight), EG(DProSilver), VALUE_ZERO, EG(DHorse),  EG(DDragon),
	  VALUE_ZERO, EG(DPawn),    EG(DLance),    EG(DKnight),    EG(DSilver),    EG(DGold),  EG(DBishop), EG(DRook),
	  EG(DKing),  EG(DProPawn), EG(DProLance), EG(DProKnight), EG(DProSilver), VALUE_ZERO, EG(DHorse),  EG(DDragon) },
};
#undef MG
#undef EG

unsigned char Position::DirTbl[0xA0][0x100];	// 方向用[from][to]

#else
namespace PSQT {
  extern Score psq[PIECE_NB][SQUARE_NB];
}
#endif

namespace Zobrist {
#if defined(NANOHA)
  Key psq[PIECE_NB][0x100];
  Key hand[PIECE_NB][32];
  Key side;
#else
  Key psq[PIECE_NB][SQUARE_NB];
  Key enpassant[FILE_NB];
  Key castling[CASTLING_RIGHT_NB];
  Key side, noPawns;
#endif
}

namespace {

#if defined(NANOHA)
const string PieceToChar(" PLNSGBR"
                         "KPLNS BR"
                         " plnsgbr"
                         "kplns br");
const char *Piece2CSA[] = {
	" * ", "+FU", "+KY", "+KE", "+GI", "+KI", "+KA", "+HI",
	"+OU", "+TO", "+NY", "+NK", "+NG", "+--", "+UM", "+RY",
	" * ", "-FU", "-KY", "-KE", "-GI", "-KI", "-KA", "-HI",
	"-OU", "-TO", "-NY", "-NK", "-NG", "---", "-UM", "-RY",
};
#else
const string PieceToChar(" PNBRQK  pnbrqk");

// min_attacker() is a helper function used by see_ge() to locate the least
// valuable attacker for the side to move, remove the attacker we just found
// from the bitboards and scan for new X-ray attacks behind it.

template<int Pt>
PieceType min_attacker(const Bitboard* bb, Square to, Bitboard stmAttackers,
                       Bitboard& occupied, Bitboard& attackers) {

  Bitboard b = stmAttackers & bb[Pt];
  if (!b)
      return min_attacker<Pt+1>(bb, to, stmAttackers, occupied, attackers);

  occupied ^= b & ~(b - 1);

  if (Pt == PAWN || Pt == BISHOP || Pt == QUEEN)
      attackers |= attacks_bb<BISHOP>(to, occupied) & (bb[BISHOP] | bb[QUEEN]);

  if (Pt == ROOK || Pt == QUEEN)
      attackers |= attacks_bb<ROOK>(to, occupied) & (bb[ROOK] | bb[QUEEN]);

  attackers &= occupied; // After X-ray that may add already processed pieces
  return (PieceType)Pt;
}

template<>
PieceType min_attacker<KING>(const Bitboard*, Square, Bitboard, Bitboard&, Bitboard&) {
  return KING; // No need to update bitboards: it is the last cycle
}
#endif
} // namespace


/// operator<<(Position) returns an ASCII representation of the position

std::ostream& operator<<(std::ostream& os, const Position& pos) {

#if defined(NANOHA)
	// [ToDo]
	const char* pieceLetters[] = {
		"  ", " P", " L", " N", " S", " G", " B", " R", " K", "+P", "+L", "+N", "+S", "--", "+B", "+R",
		"  ", " p", " l", " n", " s", " g", " b", " r", " k", "+p", "+l", "+n", "+s", "--", "+b", "+r",
	};
	const char* dottedLine = "\n+---+---+---+---+---+---+---+---+---+\n";
	for (Rank rank = RANK_1; rank <= RANK_9; ++rank) {
		os << dottedLine << '|';
		for (File file = FILE_9; file >= FILE_1; --file) {
			Square sq = make_square(file, rank);
			Piece piece = pos.piece_on(sq);

			os << pieceLetters[piece] << ' ' << '|';
		}
	}
	os << dottedLine << "Fen is: " << pos.fen() << "\nKey is: 0x" << std::hex << pos.key() << std::dec << std::endl;

	return os;
#else
  os << "\n +---+---+---+---+---+---+---+---+\n";

  for (Rank r = RANK_8; r >= RANK_1; --r)
  {
      for (File f = FILE_A; f <= FILE_H; ++f)
          os << " | " << PieceToChar[pos.piece_on(make_square(f, r))];

      os << " |\n +---+---+---+---+---+---+---+---+\n";
  }

  os << "\nFen: " << pos.fen() << "\nKey: " << std::hex << std::uppercase
     << std::setfill('0') << std::setw(16) << pos.key()
     << std::setfill(' ') << std::dec << "\nCheckers: ";

  for (Bitboard b = pos.checkers(); b; )
      os << UCI::square(pop_lsb(&b)) << " ";

  if (    int(Tablebases::MaxCardinality) >= popcount(pos.pieces())
      && !pos.can_castle(ANY_CASTLING))
  {
      StateInfo st;
      Position p;
      p.set(pos.fen(), pos.is_chess960(), &st, pos.this_thread());
      Tablebases::ProbeState s1, s2;
      Tablebases::WDLScore wdl = Tablebases::probe_wdl(p, &s1);
      int dtz = Tablebases::probe_dtz(p, &s2);
      os << "\nTablebases WDL: " << std::setw(4) << wdl << " (" << s1 << ")"
         << "\nTablebases DTZ: " << std::setw(4) << dtz << " (" << s2 << ")";
  }

  return os;
#endif
}


/// Position::init() initializes at startup the various arrays used to compute
/// hash keys.

void Position::init() {

  PRNG rng(1070372);

#if defined(NANOHA)
	int j, k;
	for (j = 0; j < GRY+1; j++) for (k = 0; k < 0x100; k++)
		Zobrist::psq[j][k] = rng.rand<Key>() << 2;

	for (j = 0; j < PIECE_NB; j++) for (k = 0; k < 32; k++)
		Zobrist::hand[j][k] = rng.rand<Key>() << 2;

	Zobrist::side = (rng.rand<Key>() << 2) | 1;

	Position::initMate1ply();

	int from;
	int to;
	int i;
	memset(Position::DirTbl, 0, sizeof(Position::DirTbl));
	for (from = 0x11; from <= 0x99; from++) {
		if ((from & 0x0F) == 0 || (from & 0x0F) > 9) continue;
		for (i = 0; i < 8; i++) {
			int dir = NanohaTbl::Direction[i];
			to = from;
			while (1) {
				to += dir;
				if ((to & 0x0F) == 0 || (to & 0x0F) >    9) break;
				if ((to & 0xF0) == 0 || (to & 0xF0) > 0x90) break;
				Position::DirTbl[from][to] = (1 << i);
			}
		}
	}
#else
  for (Piece pc : Pieces)
      for (Square s = SQ_A1; s <= SQ_H8; ++s)
          Zobrist::psq[pc][s] = rng.rand<Key>();

  for (File f = FILE_A; f <= FILE_H; ++f)
      Zobrist::enpassant[f] = rng.rand<Key>();

  for (int cr = NO_CASTLING; cr <= ANY_CASTLING; ++cr)
  {
      Zobrist::castling[cr] = 0;
      Bitboard b = cr;
      while (b)
      {
          Key k = Zobrist::castling[1ULL << pop_lsb(&b)];
          Zobrist::castling[cr] ^= k ? k : rng.rand<Key>();
      }
  }

  Zobrist::side = rng.rand<Key>();
  Zobrist::noPawns = rng.rand<Key>();
#endif
}


/// Position::set() initializes the position object with the given FEN string.
/// This function is not very robust - make sure that input FENs are correct,
/// this is assumed to be the responsibility of the GUI.

#if defined(NANOHA)
Position& Position::set(const string& fenStr, StateInfo* si, Thread* th) {
#else
Position& Position::set(const string& fenStr, bool isChess960, StateInfo* si, Thread* th) {
#endif
/*
   A FEN string defines a particular position using only the ASCII character set.

   A FEN string contains six fields separated by a space. The fields are:

   1) Piece placement (from white's perspective). Each rank is described, starting
      with rank 8 and ending with rank 1. Within each rank, the contents of each
      square are described from file A through file H. Following the Standard
      Algebraic Notation (SAN), each piece is identified by a single letter taken
      from the standard English names. White pieces are designated using upper-case
      letters ("PNBRQK") whilst Black uses lowercase ("pnbrqk"). Blank squares are
      noted using digits 1 through 8 (the number of blank squares), and "/"
      separates ranks.

   2) Active color. "w" means white moves next, "b" means black.

   3) Castling availability. If neither side can castle, this is "-". Otherwise,
      this has one or more letters: "K" (White can castle kingside), "Q" (White
      can castle queenside), "k" (Black can castle kingside), and/or "q" (Black
      can castle queenside).

   4) En passant target square (in algebraic notation). If there's no en passant
      target square, this is "-". If a pawn has just made a 2-square move, this
      is the position "behind" the pawn. This is recorded only if there is a pawn
      in position to make an en passant capture, and if there really is a pawn
      that might have advanced two squares.

   5) Halfmove clock. This is the number of halfmoves since the last pawn advance
      or capture. This is used to determine if a draw can be claimed under the
      fifty-move rule.

   6) Fullmove number. The number of the full move. It starts at 1, and is
      incremented after Black's move.
*/

#if defined(NANOHA)
	char token;
	size_t idx;
	std::istringstream ss(fenStr);
	unsigned char tmp_board[9][9] = {{'\0'}};
	int tmp_hand[GRY+1] = {0};
	int step = 0;

	std::memset(this, 0, sizeof(Position));
	std::memset(si, 0, sizeof(StateInfo));
	st = si;

	ss >> std::noskipws;

	// 1. Piece placement
	int dan = 0;
	int suji = 0; // この「筋」というのは、普通とは反転したものになる。
	while ((ss >> token) && !isspace(token))
	{
		if (token == '+') {
			// 成駒.
			ss >> token;
			if ((idx = PieceToChar.find(token)) == string::npos) goto incorrect_fen;
			tmp_board[dan][suji++] = Piece(idx | PROMOTED);
		} else if (isdigit(token)) {
			suji += (token - '0'); // 数字の分空白.
		} else if (token == '/') {
			if (suji != 9) goto incorrect_fen;
			suji = 0;
			dan++;
		}
		else if ((idx = PieceToChar.find(token)) != string::npos)
		{
			tmp_board[dan][suji++] = Piece(idx);
		} else {
			goto incorrect_fen;
		}
		if (dan > 9 || suji > 9) goto incorrect_fen;
	}
	step = 1;
	if (dan != 9 && suji != 9) goto incorrect_fen;

	// 手番取得
	step = 2;
	if (!ss.get(token) || (token != 'w' && token != 'b')) goto incorrect_fen;
	sideToMove = (token == 'b') ? BLACK : WHITE;
	// スペース飛ばす
	step = 3;
	if (!ss.get(token) || token != ' ') goto incorrect_fen;

	// 持ち駒
	step = 4;
	while ((ss >> token) && token != ' ') {
		int num = 1;
		if (token == '-') {
			break;
		} else if (isdigit(token)) {
			num = token - '0';
			ss >> token;
			if (isdigit(token)) {
				num = 10*num + token - '0';
				ss >> token;
			}
		}
		if ((idx = PieceToChar.find(token)) == string::npos) goto incorrect_fen;
		tmp_hand[idx] = num;
	}
	init_position(tmp_board, tmp_hand);
#else
  unsigned char col, row, token;
  size_t idx;
  Square sq = SQ_A8;
  std::istringstream ss(fenStr);

  std::memset(this, 0, sizeof(Position));
  std::memset(si, 0, sizeof(StateInfo));
  std::fill_n(&pieceList[0][0], sizeof(pieceList) / sizeof(Square), SQ_NONE);
  st = si;

  ss >> std::noskipws;

  // 1. Piece placement
  while ((ss >> token) && !isspace(token))
  {
      if (isdigit(token))
          sq += Square(token - '0'); // Advance the given number of files

      else if (token == '/')
          sq -= Square(16);

      else if ((idx = PieceToChar.find(token)) != string::npos)
      {
          put_piece(Piece(idx), sq);
          ++sq;
      }
  }

  // 2. Active color
  ss >> token;
  sideToMove = (token == 'w' ? WHITE : BLACK);
  ss >> token;

  // 3. Castling availability. Compatible with 3 standards: Normal FEN standard,
  // Shredder-FEN that uses the letters of the columns on which the rooks began
  // the game instead of KQkq and also X-FEN standard that, in case of Chess960,
  // if an inner rook is associated with the castling right, the castling tag is
  // replaced by the file letter of the involved rook, as for the Shredder-FEN.
  while ((ss >> token) && !isspace(token))
  {
      Square rsq;
      Color c = islower(token) ? BLACK : WHITE;
      Piece rook = make_piece(c, ROOK);

      token = char(toupper(token));

      if (token == 'K')
          for (rsq = relative_square(c, SQ_H1); piece_on(rsq) != rook; --rsq) {}

      else if (token == 'Q')
          for (rsq = relative_square(c, SQ_A1); piece_on(rsq) != rook; ++rsq) {}

      else if (token >= 'A' && token <= 'H')
          rsq = make_square(File(token - 'A'), relative_rank(c, RANK_1));

      else
          continue;

      set_castling_right(c, rsq);
  }

  // 4. En passant square. Ignore if no pawn capture is possible
  if (   ((ss >> col) && (col >= 'a' && col <= 'h'))
      && ((ss >> row) && (row == '3' || row == '6')))
  {
      st->epSquare = make_square(File(col - 'a'), Rank(row - '1'));

      if (   !(attackers_to(st->epSquare) & pieces(sideToMove, PAWN))
          || !(pieces(~sideToMove, PAWN) & (st->epSquare + pawn_push(~sideToMove))))
          st->epSquare = SQ_NONE;
  }
  else
      st->epSquare = SQ_NONE;
#endif

  // 5-6. Halfmove clock and fullmove number
#if defined(NANOHA)
	ss >> std::skipws >> gamePly;
#else
  ss >> std::skipws >> st->rule50 >> gamePly;
#endif

  // Convert from fullmove starting from 1 to ply starting from 0,
  // handle also common incorrect FEN with fullmove = 0.
#if defined(NANOHA)
// チェスと将棋で先手のBLACK/WHITEが違うが、そのままでいいか？
// ⇒将棋は駒落ちで手番が変わるので、補正しない。
#else
  gamePly = std::max(2 * (gamePly - 1), 0) + (sideToMove == BLACK);

  chess960 = isChess960;
#endif
  thisThread = th;
  set_state(st);

#if defined(NANOHA)
#if !defined(NDEBUG)
	if (pos_is_ok(&step) == false) {
		print_csa();
		std::cout << "step : " << step << std::endl;
	}
#endif
#endif
  assert(pos_is_ok());

  return *this;

#if defined(NANOHA)
incorrect_fen:
	std::cerr << "Error in SFEN string: " << fenStr << std::endl;
	std::cerr << "   token: '" << token << "', step=" << step << std::endl;
  return *this;
#endif
}


/// Position::set_castling_right() is a helper function used to set castling
/// rights given the corresponding color and the rook starting square.

#if !defined(NANOHA)
void Position::set_castling_right(Color c, Square rfrom) {

  Square kfrom = square<KING>(c);
  CastlingSide cs = kfrom < rfrom ? KING_SIDE : QUEEN_SIDE;
  CastlingRight cr = (c | cs);

  st->castlingRights |= cr;
  castlingRightsMask[kfrom] |= cr;
  castlingRightsMask[rfrom] |= cr;
  castlingRookSquare[cr] = rfrom;

  Square kto = relative_square(c, cs == KING_SIDE ? SQ_G1 : SQ_C1);
  Square rto = relative_square(c, cs == KING_SIDE ? SQ_F1 : SQ_D1);

  for (Square s = std::min(rfrom, rto); s <= std::max(rfrom, rto); ++s)
      if (s != kfrom && s != rfrom)
          castlingPath[cr] |= s;

  for (Square s = std::min(kfrom, kto); s <= std::max(kfrom, kto); ++s)
      if (s != kfrom && s != rfrom)
          castlingPath[cr] |= s;
}


/// Position::set_check_info() sets king attacks to detect if a move gives check

void Position::set_check_info(StateInfo* si) const {

  si->blockersForKing[WHITE] = slider_blockers(pieces(BLACK), square<KING>(WHITE), si->pinnersForKing[WHITE]);
  si->blockersForKing[BLACK] = slider_blockers(pieces(WHITE), square<KING>(BLACK), si->pinnersForKing[BLACK]);

  Square ksq = square<KING>(~sideToMove);

  si->checkSquares[PAWN]   = attacks_from<PAWN>(ksq, ~sideToMove);
  si->checkSquares[KNIGHT] = attacks_from<KNIGHT>(ksq);
  si->checkSquares[BISHOP] = attacks_from<BISHOP>(ksq);
  si->checkSquares[ROOK]   = attacks_from<ROOK>(ksq);
  si->checkSquares[QUEEN]  = si->checkSquares[BISHOP] | si->checkSquares[ROOK];
  si->checkSquares[KING]   = 0;
}
#endif

/// Position::set_state() computes the hash keys of the position, and other
/// data that once computed is updated incrementally as moves are made.
/// The function is only used when a new position is set up, and to verify
/// the correctness of the StateInfo data when running in debug mode.

#if defined(NANOHA)
void Position::compute_key(StateInfo* si) const {
	Key k = 0;
	int z;
	for (int dan = 1; dan <= 9; dan++) {
		for (int suji = 0x10; suji <= 0x90; suji += 0x10) {
			z = suji + dan;
			if (!empty(Square(z))) {
				k ^= Zobrist::psq[piece_on(Square(z))][z];
			}
		}
	}
	if (side_to_move() == BLACK)
		k ^= Zobrist::side;

	si->board_key = k;

	uint32_t i;
	k = 0;
	enum { S=BLACK, G=WHITE };
#define HandKEY(side, pc)  for (i = 1; i <= hand[side].get ## pc(); i++) k ^= Zobrist::hand[side ## pc][i]

	HandKEY(S, FU);
	HandKEY(S, KY);
	HandKEY(S, KE);
	HandKEY(S, GI);
	HandKEY(S, KI);
	HandKEY(S, KA);
	HandKEY(S, HI);

	HandKEY(G, FU);
	HandKEY(G, KY);
	HandKEY(G, KE);
	HandKEY(G, GI);
	HandKEY(G, KI);
	HandKEY(G, KA);
	HandKEY(G, HI);
#undef HandKEY

	si->hand_key = k;
}
#endif

void Position::set_state(StateInfo* si) const {

#if defined(NANOHA)
	compute_key(si);
	si->hand = hand[sideToMove].h;
	si->effect = (sideToMove == BLACK) ? effectW[kingS] : effectB[kingG];
	si->material = compute_material();

//	if (sideToMove == BLACK)
//		si->board_key ^= Zobrist::side;
#else
  si->key = si->materialKey = 0;
  si->pawnKey = Zobrist::noPawns;
  si->nonPawnMaterial[WHITE] = si->nonPawnMaterial[BLACK] = VALUE_ZERO;
  si->psq = SCORE_ZERO;
  si->checkersBB = attackers_to(square<KING>(sideToMove)) & pieces(~sideToMove);

  set_check_info(si);

  for (Bitboard b = pieces(); b; )
  {
      Square s = pop_lsb(&b);
      Piece pc = piece_on(s);
      si->key ^= Zobrist::psq[pc][s];
      si->psq += PSQT::psq[pc][s];
  }

  if (si->epSquare != SQ_NONE)
      si->key ^= Zobrist::enpassant[file_of(si->epSquare)];

  if (sideToMove == BLACK)
      si->key ^= Zobrist::side;

  si->key ^= Zobrist::castling[si->castlingRights];

  for (Bitboard b = pieces(PAWN); b; )
  {
      Square s = pop_lsb(&b);
      si->pawnKey ^= Zobrist::psq[piece_on(s)][s];
  }

  for (Piece pc : Pieces)
  {
      if (type_of(pc) != PAWN && type_of(pc) != KING)
          si->nonPawnMaterial[color_of(pc)] += pieceCount[pc] * PieceValue[MG][pc];

      for (int cnt = 0; cnt < pieceCount[pc]; ++cnt)
          si->materialKey ^= Zobrist::psq[pc][cnt];
  }
#endif
}


/// Position::set() is an overload to initialize the position object with
/// the given endgame code string like "KBPKN". It is mainly a helper to
/// get the material key out of an endgame code. Position is not playable,
/// indeed is even not guaranteed to be legal.
#if !defined(NANOHA)
Position& Position::set(const string& code, Color c, StateInfo* si) {

  assert(code.length() > 0 && code.length() < 8);
  assert(code[0] == 'K');

  string sides[] = { code.substr(code.find('K', 1)),      // Weak
                     code.substr(0, code.find('K', 1)) }; // Strong

  std::transform(sides[c].begin(), sides[c].end(), sides[c].begin(), tolower);

  string fenStr =  sides[0] + char(8 - sides[0].length() + '0') + "/8/8/8/8/8/8/"
                 + sides[1] + char(8 - sides[1].length() + '0') + " w - - 0 10";

  return set(fenStr, false, si, nullptr);
}
#endif

/// Position::fen() returns a FEN representation of the position. In case of
/// Chess960 the Shredder-FEN notation is used. This is mainly a debugging function.

const string Position::fen() const {

  int emptyCnt;
  std::ostringstream ss;

#if defined(NANOHA)
	for (Rank r = RANK_1; r <= RANK_9; ++r)
#else
  for (Rank r = RANK_8; r >= RANK_1; --r)
#endif
  {
#if defined(NANOHA)
		for (File f = FILE_9; f >= FILE_1; --f)
#else
      for (File f = FILE_A; f <= FILE_H; ++f)
#endif
      {
#if defined(NANOHA)
          for (emptyCnt = 0; f >= FILE_1 && empty(make_square(f, r)); --f)
#else
          for (emptyCnt = 0; f <= FILE_H && empty(make_square(f, r)); ++f)
#endif
              ++emptyCnt;

          if (emptyCnt)
              ss << emptyCnt;

#if defined(NANOHA)
          if (f >= FILE_1) {
              Square sq = make_square(f, r);
              if ((piece_on(sq)-1) & PROMOTED) {
                  ss << "+";
              }
              ss << PieceToChar[piece_on(sq)];
          }
#else
          if (f <= FILE_H)
              ss << PieceToChar[piece_on(make_square(f, r))];
#endif
      }

#if defined(NANOHA)
      if (r < RANK_9)
#else
      if (r > RANK_1)
#endif
          ss << '/';
  }

  ss << (sideToMove == WHITE ? " w " : " b ");

#if defined(NANOHA)
	// 持ち駒
	if (handS.h == 0 && handG.h == 0) {
		ss << "-";
	} else {
		unsigned int n;
		static const char *tbl[] = {
			 "",   "",   "2",  "3",  "4",  "5",  "6",  "7",
			 "8",  "9", "10", "11", "12", "13", "14", "15",
			"16", "17", "18"
		};
#define ADD_HAND(piece,c)	n = h.get ## piece(); if (n) {ss << tbl[n]; ss << #c; }

		Hand h = handS;
		ADD_HAND(HI,R)
		ADD_HAND(KA,B)
		ADD_HAND(KI,G)
		ADD_HAND(GI,S)
		ADD_HAND(KE,N)
		ADD_HAND(KY,L)
		ADD_HAND(FU,P)

		h = handG;
		ADD_HAND(HI,r)
		ADD_HAND(KA,b)
		ADD_HAND(KI,g)
		ADD_HAND(GI,s)
		ADD_HAND(KE,n)
		ADD_HAND(KY,l)
		ADD_HAND(FU,p)

#undef ADD_HAND
	}
#else
  if (can_castle(WHITE_OO))
      ss << (chess960 ? char('A' + file_of(castling_rook_square(WHITE |  KING_SIDE))) : 'K');

  if (can_castle(WHITE_OOO))
      ss << (chess960 ? char('A' + file_of(castling_rook_square(WHITE | QUEEN_SIDE))) : 'Q');

  if (can_castle(BLACK_OO))
      ss << (chess960 ? char('a' + file_of(castling_rook_square(BLACK |  KING_SIDE))) : 'k');

  if (can_castle(BLACK_OOO))
      ss << (chess960 ? char('a' + file_of(castling_rook_square(BLACK | QUEEN_SIDE))) : 'q');

  if (!can_castle(WHITE) && !can_castle(BLACK))
      ss << '-';

  ss << (ep_square() == SQ_NONE ? " - " : " " + UCI::square(ep_square()) + " ")
     << st->rule50 << " " << 1 + (gamePly - (sideToMove == BLACK)) / 2;
#endif

  return ss.str();
}


#if defined(NANOHA)
void Position::print_csa(Move move) const {

	if (move != MOVE_NONE)
	{
		sync_cout << "\nMove is: " << move_to_csa(move) << std::endl;
	} else {
		sync_cout << "\nMove is: NONE" << std::endl;
	}
	// 盤面
	for (Rank rank = RANK_1; rank <= RANK_9; ++rank)
	{
		std::cout << "P" << int(rank);
		for (File file = FILE_9; file >= FILE_1; --file)
		{
			Square sq = make_square(file, rank);
			Piece piece = piece_on(sq);
			std::cout << Piece2CSA[piece];
		}
		std::cout << std::endl;
	}
	// 持ち駒
	unsigned int n;
	std::cout << "P+";
	{
	const Hand &h = hand[BLACK];
	if ((n = h.getFU()) > 0) while (n--){ std::cout << "00FU"; }
	if ((n = h.getKY()) > 0) while (n--){ std::cout << "00KY"; }
	if ((n = h.getKE()) > 0) while (n--){ std::cout << "00KE"; }
	if ((n = h.getGI()) > 0) while (n--){ std::cout << "00GI"; }
	if ((n = h.getKI()) > 0) while (n--){ std::cout << "00KI"; }
	if ((n = h.getKA()) > 0) while (n--){ std::cout << "00KA"; }
	if ((n = h.getHI()) > 0) while (n--){ std::cout << "00HI"; }
	}
	std::cout << std::endl << "P-";
	{
	const Hand &h = hand[WHITE];
	if ((n = h.getFU()) > 0) while (n--){ std::cout << "00FU"; }
	if ((n = h.getKY()) > 0) while (n--){ std::cout << "00KY"; }
	if ((n = h.getKE()) > 0) while (n--){ std::cout << "00KE"; }
	if ((n = h.getGI()) > 0) while (n--){ std::cout << "00GI"; }
	if ((n = h.getKI()) > 0) while (n--){ std::cout << "00KI"; }
	if ((n = h.getKA()) > 0) while (n--){ std::cout << "00KA"; }
	if ((n = h.getHI()) > 0) while (n--){ std::cout << "00HI"; }
	}
	std::cout << std::endl << (sideToMove == BLACK ? '+' : '-') << std::endl;
	std::cout << "SFEN is: " << fen() << "\nKey is: 0x" << std::hex << st->board_key 
	          << ", Hand key: 0x" << st->hand_key << std::dec << sync_endl;
}

// Compare
bool Position::eq_board(const Position &a) const
{
	return memcmp(board, a.board, sizeof(board)) == 0;
}

bool Position::operator == (const Position &a) const
{
	if (eq_board(a) == false) return false;
	return true;
}

#endif

#if !defined(NANOHA)
/// Position::game_phase() calculates the game phase interpolating total non-pawn
/// material between endgame and midgame limits.

Phase Position::game_phase() const {

  Value npm = st->nonPawnMaterial[WHITE] + st->nonPawnMaterial[BLACK];

  npm = std::max(EndgameLimit, std::min(npm, MidgameLimit));

  return Phase(((npm - EndgameLimit) * PHASE_MIDGAME) / (MidgameLimit - EndgameLimit));
}


/// Position::slider_blockers() returns a bitboard of all the pieces (both colors)
/// that are blocking attacks on the square 's' from 'sliders'. A piece blocks a
/// slider if removing that piece from the board would result in a position where
/// square 's' is attacked. For example, a king-attack blocking piece can be either
/// a pinned or a discovered check piece, according if its color is the opposite
/// or the same of the color of the slider.

Bitboard Position::slider_blockers(Bitboard sliders, Square s, Bitboard& pinners) const {

  Bitboard result = 0;
  pinners = 0;

  // Snipers are sliders that attack 's' when a piece is removed
  Bitboard snipers = (  (PseudoAttacks[ROOK  ][s] & pieces(QUEEN, ROOK))
                      | (PseudoAttacks[BISHOP][s] & pieces(QUEEN, BISHOP))) & sliders;

  while (snipers)
  {
    Square sniperSq = pop_lsb(&snipers);
    Bitboard b = between_bb(s, sniperSq) & pieces();

    if (!more_than_one(b))
    {
        result |= b;
        if (b & pieces(color_of(piece_on(s))))
            pinners |= sniperSq;
    }
  }
  return result;
}


/// Position::attackers_to() computes a bitboard of all pieces which attack a
/// given square. Slider attacks use the occupied bitboard to indicate occupancy.

Bitboard Position::attackers_to(Square s, Bitboard occupied) const {

  return  (attacks_from<PAWN>(s, BLACK)    & pieces(WHITE, PAWN))
        | (attacks_from<PAWN>(s, WHITE)    & pieces(BLACK, PAWN))
        | (attacks_from<KNIGHT>(s)         & pieces(KNIGHT))
        | (attacks_bb<ROOK  >(s, occupied) & pieces(ROOK,   QUEEN))
        | (attacks_bb<BISHOP>(s, occupied) & pieces(BISHOP, QUEEN))
        | (attacks_from<KING>(s)           & pieces(KING));
}
#endif


/// Position::legal() tests whether a pseudo-legal move is legal

#if !defined(NANOHA)
bool Position::legal(Move m) const {

  assert(is_ok(m));

  Color us = sideToMove;
  Square from = from_sq(m);

  assert(color_of(moved_piece(m)) == us);
  assert(piece_on(square<KING>(us)) == make_piece(us, KING));

  // En passant captures are a tricky special case. Because they are rather
  // uncommon, we do it simply by testing whether the king is attacked after
  // the move is made.
  if (type_of(m) == ENPASSANT)
  {
      Square ksq = square<KING>(us);
      Square to = to_sq(m);
      Square capsq = to - pawn_push(us);
      Bitboard occupied = (pieces() ^ from ^ capsq) | to;

      assert(to == ep_square());
      assert(moved_piece(m) == make_piece(us, PAWN));
      assert(piece_on(capsq) == make_piece(~us, PAWN));
      assert(piece_on(to) == NO_PIECE);

      return   !(attacks_bb<  ROOK>(ksq, occupied) & pieces(~us, QUEEN, ROOK))
            && !(attacks_bb<BISHOP>(ksq, occupied) & pieces(~us, QUEEN, BISHOP));
  }

  // If the moving piece is a king, check whether the destination
  // square is attacked by the opponent. Castling moves are checked
  // for legality during move generation.
  if (type_of(piece_on(from)) == KING)
      return type_of(m) == CASTLING || !(attackers_to(to_sq(m)) & pieces(~us));

  // A non-king move is legal if and only if it is not pinned or it
  // is moving along the ray towards or away from the king.
  return   !(pinned_pieces(us) & from)
        ||  aligned(from, to_sq(m), square<KING>(us));
}


/// Position::pseudo_legal() takes a random move and tests whether the move is
/// pseudo legal. It is used to validate moves from TT that can be corrupted
/// due to SMP concurrent access or hash position key aliasing.

bool Position::pseudo_legal(const Move m) const {

  Color us = sideToMove;
  Square from = from_sq(m);
  Square to = to_sq(m);
  Piece pc = moved_piece(m);

  // Use a slower but simpler function for uncommon cases
  if (type_of(m) != NORMAL)
      return MoveList<LEGAL>(*this).contains(m);

  // Is not a promotion, so promotion piece must be empty
  if (promotion_type(m) - KNIGHT != NO_PIECE_TYPE)
      return false;

  // If the 'from' square is not occupied by a piece belonging to the side to
  // move, the move is obviously not legal.
  if (pc == NO_PIECE || color_of(pc) != us)
      return false;

  // The destination square cannot be occupied by a friendly piece
  if (pieces(us) & to)
      return false;

  // Handle the special case of a pawn move
  if (type_of(pc) == PAWN)
  {
      // We have already handled promotion moves, so destination
      // cannot be on the 8th/1st rank.
      if (rank_of(to) == relative_rank(us, RANK_8))
          return false;

      if (   !(attacks_from<PAWN>(from, us) & pieces(~us) & to) // Not a capture
          && !((from + pawn_push(us) == to) && empty(to))       // Not a single push
          && !(   (from + 2 * pawn_push(us) == to)              // Not a double push
               && (rank_of(from) == relative_rank(us, RANK_2))
               && empty(to)
               && empty(to - pawn_push(us))))
          return false;
  }
  else if (!(attacks_from(pc, from) & to))
      return false;

  // Evasions generator already takes care to avoid some kind of illegal moves
  // and legal() relies on this. We therefore have to take care that the same
  // kind of moves are filtered out here.
  if (checkers())
  {
      if (type_of(pc) != KING)
      {
          // Double check? In this case a king move is required
          if (more_than_one(checkers()))
              return false;

          // Our move must be a blocking evasion or a capture of the checking piece
          if (!((between_bb(lsb(checkers()), square<KING>(us)) | checkers()) & to))
              return false;
      }
      // In case of king moves under check we have to remove king so as to catch
      // invalid moves like b1a1 when opposite queen is on c1.
      else if (attackers_to(to, pieces() ^ from) & pieces(~us))
          return false;
  }

  return true;
}


/// Position::gives_check() tests whether a pseudo-legal move gives a check

bool Position::gives_check(Move m) const {

  assert(is_ok(m));
  assert(color_of(moved_piece(m)) == sideToMove);

  Square from = from_sq(m);
  Square to = to_sq(m);

  // Is there a direct check?
  if (st->checkSquares[type_of(piece_on(from))] & to)
      return true;

  // Is there a discovered check?
  if (   (discovered_check_candidates() & from)
      && !aligned(from, to, square<KING>(~sideToMove)))
      return true;

  switch (type_of(m))
  {
  case NORMAL:
      return false;

  case PROMOTION:
      return attacks_bb(Piece(promotion_type(m)), to, pieces() ^ from) & square<KING>(~sideToMove);

  // En passant capture with check? We have already handled the case
  // of direct checks and ordinary discovered check, so the only case we
  // need to handle is the unusual case of a discovered check through
  // the captured pawn.
  case ENPASSANT:
  {
      Square capsq = make_square(file_of(to), rank_of(from));
      Bitboard b = (pieces() ^ from ^ capsq) | to;

      return  (attacks_bb<  ROOK>(square<KING>(~sideToMove), b) & pieces(sideToMove, QUEEN, ROOK))
            | (attacks_bb<BISHOP>(square<KING>(~sideToMove), b) & pieces(sideToMove, QUEEN, BISHOP));
  }
  case CASTLING:
  {
      Square kfrom = from;
      Square rfrom = to; // Castling is encoded as 'King captures the rook'
      Square kto = relative_square(sideToMove, rfrom > kfrom ? SQ_G1 : SQ_C1);
      Square rto = relative_square(sideToMove, rfrom > kfrom ? SQ_F1 : SQ_D1);

      return   (PseudoAttacks[ROOK][rto] & square<KING>(~sideToMove))
            && (attacks_bb<ROOK>(rto, (pieces() ^ kfrom ^ rfrom) | rto | kto) & square<KING>(~sideToMove));
  }
  default:
      assert(false);
      return false;
  }
}
#endif


/// Position::do_move() makes a move, and saves all information necessary
/// to a StateInfo object. The move is assumed to be legal. Pseudo-legal
/// moves should be filtered out before this function is called.

#if !defined(NANOHA)
/// ※ shogi.cppのdo_move()、do_drop()に反映する.
void Position::do_move(Move m, StateInfo& newSt, bool givesCheck) {

  assert(is_ok(m));
  assert(&newSt != st);

  ++nodes;
  Key k = st->key ^ Zobrist::side;

  // Copy some fields of the old state to our new StateInfo object except the
  // ones which are going to be recalculated from scratch anyway and then switch
  // our state pointer to point to the new (ready to be updated) state.
  std::memcpy(&newSt, st, offsetof(StateInfo, key));
  newSt.previous = st;
  st = &newSt;

  // Increment ply counters. In particular, rule50 will be reset to zero later on
  // in case of a capture or a pawn move.
  ++gamePly;
  ++st->rule50;
  ++st->pliesFromNull;

  Color us = sideToMove;
  Color them = ~us;
  Square from = from_sq(m);
  Square to = to_sq(m);
  Piece pc = piece_on(from);
  Piece captured = type_of(m) == ENPASSANT ? make_piece(them, PAWN) : piece_on(to);

  assert(color_of(pc) == us);
  assert(captured == NO_PIECE || color_of(captured) == (type_of(m) != CASTLING ? them : us));
  assert(type_of(captured) != KING);

  if (type_of(m) == CASTLING)
  {
      assert(pc == make_piece(us, KING));
      assert(captured == make_piece(us, ROOK));

      Square rfrom, rto;
      do_castling<true>(us, from, to, rfrom, rto);

      st->psq += PSQT::psq[captured][rto] - PSQT::psq[captured][rfrom];
      k ^= Zobrist::psq[captured][rfrom] ^ Zobrist::psq[captured][rto];
      captured = NO_PIECE;
  }

  if (captured)
  {
      Square capsq = to;

      // If the captured piece is a pawn, update pawn hash key, otherwise
      // update non-pawn material.
      if (type_of(captured) == PAWN)
      {
          if (type_of(m) == ENPASSANT)
          {
              capsq -= pawn_push(us);

              assert(pc == make_piece(us, PAWN));
              assert(to == st->epSquare);
              assert(relative_rank(us, to) == RANK_6);
              assert(piece_on(to) == NO_PIECE);
              assert(piece_on(capsq) == make_piece(them, PAWN));

              board[capsq] = NO_PIECE; // Not done by remove_piece()
          }

          st->pawnKey ^= Zobrist::psq[captured][capsq];
      }
      else
          st->nonPawnMaterial[them] -= PieceValue[MG][captured];

      // Update board and piece lists
      remove_piece(captured, capsq);

      // Update material hash key and prefetch access to materialTable
      k ^= Zobrist::psq[captured][capsq];
      st->materialKey ^= Zobrist::psq[captured][pieceCount[captured]];
      prefetch(thisThread->materialTable[st->materialKey]);

      // Update incremental scores
      st->psq -= PSQT::psq[captured][capsq];

      // Reset rule 50 counter
      st->rule50 = 0;
  }

  // Update hash key
  k ^= Zobrist::psq[pc][from] ^ Zobrist::psq[pc][to];

  // Reset en passant square
  if (st->epSquare != SQ_NONE)
  {
      k ^= Zobrist::enpassant[file_of(st->epSquare)];
      st->epSquare = SQ_NONE;
  }

  // Update castling rights if needed
  if (st->castlingRights && (castlingRightsMask[from] | castlingRightsMask[to]))
  {
      int cr = castlingRightsMask[from] | castlingRightsMask[to];
      k ^= Zobrist::castling[st->castlingRights & cr];
      st->castlingRights &= ~cr;
  }

  // Move the piece. The tricky Chess960 castling is handled earlier
  if (type_of(m) != CASTLING)
      move_piece(pc, from, to);

  // If the moving piece is a pawn do some special extra work
  if (type_of(pc) == PAWN)
  {
      // Set en-passant square if the moved pawn can be captured
      if (   (int(to) ^ int(from)) == 16
          && (attacks_from<PAWN>(to - pawn_push(us), us) & pieces(them, PAWN)))
      {
          st->epSquare = (from + to) / 2;
          k ^= Zobrist::enpassant[file_of(st->epSquare)];
      }

      else if (type_of(m) == PROMOTION)
      {
          Piece promotion = make_piece(us, promotion_type(m));

          assert(relative_rank(us, to) == RANK_8);
          assert(type_of(promotion) >= KNIGHT && type_of(promotion) <= QUEEN);

          remove_piece(pc, to);
          put_piece(promotion, to);

          // Update hash keys
          k ^= Zobrist::psq[pc][to] ^ Zobrist::psq[promotion][to];
          st->pawnKey ^= Zobrist::psq[pc][to];
          st->materialKey ^=  Zobrist::psq[promotion][pieceCount[promotion]-1]
                            ^ Zobrist::psq[pc][pieceCount[pc]];

          // Update incremental score
          st->psq += PSQT::psq[promotion][to] - PSQT::psq[pc][to];

          // Update material
          st->nonPawnMaterial[us] += PieceValue[MG][promotion];
      }

      // Update pawn hash key and prefetch access to pawnsTable
      st->pawnKey ^= Zobrist::psq[pc][from] ^ Zobrist::psq[pc][to];
      prefetch2(thisThread->pawnsTable[st->pawnKey]);

      // Reset rule 50 draw counter
      st->rule50 = 0;
  }

  // Update incremental scores
  st->psq += PSQT::psq[pc][to] - PSQT::psq[pc][from];

  // Set capture piece
  st->capturedPiece = captured;

  // Update the key with the final value
  st->key = k;

  // Calculate checkers bitboard (if move gives check)
  st->checkersBB = givesCheck ? attackers_to(square<KING>(them)) & pieces(us) : 0;

  sideToMove = ~sideToMove;

  // Update king attacks used for fast check detection
  set_check_info(st);

  assert(pos_is_ok());
}


/// Position::undo_move() unmakes a move. When it returns, the position should
/// be restored to exactly the same state as before the move was made.

void Position::undo_move(Move m) {

  assert(is_ok(m));

  sideToMove = ~sideToMove;

  Color us = sideToMove;
  Square from = from_sq(m);
  Square to = to_sq(m);
  Piece pc = piece_on(to);

  assert(empty(from) || type_of(m) == CASTLING);
  assert(type_of(st->capturedPiece) != KING);

  if (type_of(m) == PROMOTION)
  {
      assert(relative_rank(us, to) == RANK_8);
      assert(type_of(pc) == promotion_type(m));
      assert(type_of(pc) >= KNIGHT && type_of(pc) <= QUEEN);

      remove_piece(pc, to);
      pc = make_piece(us, PAWN);
      put_piece(pc, to);
  }

  if (type_of(m) == CASTLING)
  {
      Square rfrom, rto;
      do_castling<false>(us, from, to, rfrom, rto);
  }
  else
  {
      move_piece(pc, to, from); // Put the piece back at the source square

      if (st->capturedPiece)
      {
          Square capsq = to;

          if (type_of(m) == ENPASSANT)
          {
              capsq -= pawn_push(us);

              assert(type_of(pc) == PAWN);
              assert(to == st->previous->epSquare);
              assert(relative_rank(us, to) == RANK_6);
              assert(piece_on(capsq) == NO_PIECE);
              assert(st->capturedPiece == make_piece(~us, PAWN));
          }

          put_piece(st->capturedPiece, capsq); // Restore the captured piece
      }
  }

  // Finally point our state pointer back to the previous state
  st = st->previous;
  --gamePly;

  assert(pos_is_ok());
}


/// Position::do_castling() is a helper used to do/undo a castling move. This
/// is a bit tricky in Chess960 where from/to squares can overlap.
template<bool Do>
void Position::do_castling(Color us, Square from, Square& to, Square& rfrom, Square& rto) {

  bool kingSide = to > from;
  rfrom = to; // Castling is encoded as "king captures friendly rook"
  rto = relative_square(us, kingSide ? SQ_F1 : SQ_D1);
  to = relative_square(us, kingSide ? SQ_G1 : SQ_C1);

  // Remove both pieces first since squares could overlap in Chess960
  remove_piece(make_piece(us, KING), Do ? from : to);
  remove_piece(make_piece(us, ROOK), Do ? rfrom : rto);
  board[Do ? from : to] = board[Do ? rfrom : rto] = NO_PIECE; // Since remove_piece doesn't do it for us
  put_piece(make_piece(us, KING), Do ? to : from);
  put_piece(make_piece(us, ROOK), Do ? rto : rfrom);
}
#endif


/// Position::do(undo)_null_move() is used to do(undo) a "null move": It flips
/// the side to move without executing any move on the board.

void Position::do_null_move(StateInfo& newSt) {

#if defined(NANOHA)
  assert(!at_checking());
#else
  assert(!checkers());
#endif
  assert(&newSt != st);

  std::memcpy(&newSt, st, sizeof(StateInfo));
  newSt.previous = st;
  st = &newSt;

#if !defined(NANOHA)
  if (st->epSquare != SQ_NONE)
  {
      st->key ^= Zobrist::enpassant[file_of(st->epSquare)];
      st->epSquare = SQ_NONE;
  }
#endif

#if defined(NANOHA)
  st->board_key ^= Zobrist::side;
  prefetch(TT.first_entry(st->board_key ^ st->hand_key));
#else
  st->key ^= Zobrist::side;
  prefetch(TT.first_entry(st->key));

  ++st->rule50;
#endif
  st->pliesFromNull = 0;

  sideToMove = ~sideToMove;

#if defined(NANOHA)
	st->effect = (sideToMove != BLACK) ? effectB[kingG] : effectW[kingS];
	st->hand = hand[sideToMove].h;
#else
  set_check_info(st);
#endif

  assert(pos_is_ok());
}

void Position::undo_null_move() {

#if defined(NANOHA)
  assert(!at_checking());
#else
  assert(!checkers());
#endif

  st = st->previous;
  sideToMove = ~sideToMove;
}


/// Position::key_after() computes the new hash key after the given move. Needed
/// for speculative prefetch. It doesn't recognize special moves like castling,
/// en-passant and promotions.

#if defined(NANOHA)
Key Position::key_after(Move m) const
{
	Key k = key();

	k ^= Zobrist::side;	// 手番反転

	// from の計算
	Square from = from_sq(m);
	Square to = to_sq(m);
	Piece pc = piece_of(m);
	const Color us = color_of(pc);
	if (is_drop(m)) {
		// 駒打ちによるハッシュ
		k ^= Zobrist::hand[pc][hand[us].get(PieceType(pc & ~GOTE))];
	} else {
		// 空白になったことで変わるハッシュ値
		k ^= Zobrist::psq[pc][from];
	}

	// to の処理
	// board[to]にあったものをＨａｓｈから消す
	Piece cap = capture_of(m);
	assert(cap == board[to]);
	if (cap) {
		k ^= Zobrist::psq[cap][to];
		Piece pc2 = Piece((cap & ~PROMOTED) ^ GOTE);
		k ^= Zobrist::hand[pc2][hand[~us].get(PieceType(pc2 & ~GOTE)) + 1];
	}

	// 新しい駒をＨａｓｈに加える
	if (is_promotion(m)) pc = Piece(pc | PROMOTED);
	k ^= Zobrist::psq[pc][to];

	return k;
}
#else
Key Position::key_after(Move m) const {

  Square from = from_sq(m);
  Square to = to_sq(m);
  Piece pc = piece_on(from);
  Piece captured = piece_on(to);
  Key k = st->key ^ Zobrist::side;

  if (captured)
      k ^= Zobrist::psq[captured][to];

  return k ^ Zobrist::psq[pc][to] ^ Zobrist::psq[pc][from];
}
#endif


/// Position::see_ge (Static Exchange Evaluation Greater or Equal) tests if the
/// SEE value of move is greater or equal to the given value. We'll use an
/// algorithm similar to alpha-beta pruning with a null window.
#if defined(NANOHA)
static bool SEERec2(const bool relativeStm, int *attacker, int *defender, Value balance, const Value v)
{
	if (*attacker == 0) return relativeStm;

	int kind = *defender;

	balance += relativeStm
	            ? -PieceValue[MG][kind]
	            : +PieceValue[MG][kind];

	if (relativeStm == (balance >= v)) return relativeStm;

	return SEERec2(!relativeStm, defender+1, attacker, balance, v);
}

bool Position::see_ge(Move m, Value v) const {

  assert(is_ok(m));

  Square from = is_drop(m) ? SQ_NULL : from_sq(m);
  Square to = to_sq(m);
  PieceType nextVictim = ptype_of(m);
  Color stm = ~turn_of(m); // First consider opponent's move
  Value balance; // Values of the pieces taken by us minus opponent's ones

  balance = is_promotion(m)
            ? PieceValue[MG][piece_on(to)] + NanohaTbl::KomaValuePro[nextVictim]
            : PieceValue[MG][piece_on(to)];

  if (balance < v)
      return false;

  // ここで(balance >= v)

  // mが合法手であれば、王で捕る手に対して捕り返しなし.
  if (nextVictim == OU)
      return true;

  balance -= is_promotion(m)
             ? (PieceValue[MG][nextVictim] + NanohaTbl::KomaValuePro[nextVictim])
             : PieceValue[MG][nextVictim];

  // 捕り返されてもv以上確定.
  if (balance >= v)
      return true;

	Color us = side_to_move();

	const effect_t *dKiki = (us == BLACK) ? effectW : effectB;
	int defender[32];	// to に利いている守りの駒
	int ndef = 0;
	effect_t k = EXIST_EFFECT(dKiki[to]);
	while (k) {
		unsigned long id;
		_BitScanForward(&id, k);
		k &= k-1;
		if (id < 16) {
			int z = to - NanohaTbl::Direction[id];
			defender[ndef++] = board[z] & ~GOTE;
			if (dKiki[z] & (0x10000u << id)) {
				z = SkipOverEMP(z, -NanohaTbl::Direction[id]);
				defender[ndef++] = board[z] & ~GOTE;
			}
		} else {
			int z = SkipOverEMP(to, -NanohaTbl::Direction[id]);
			defender[ndef++] = board[z] & ~GOTE;
			if (dKiki[z] & (0x1u << id)) {
				z = SkipOverEMP(z, -NanohaTbl::Direction[id]);
				defender[ndef++] = board[z] & ~GOTE;
			}
		}
	}
	defender[ndef] = 0;
	assert(ndef < 32-1);

	// 捕り返す手がなければ v 以上が確定.
	if (ndef == 0) return true;

	const effect_t *aKiki = (us == BLACK) ? effectB : effectW;
	int attacker[32];	// to に利いている攻めの駒
	int natk = 0;
	k = EXIST_EFFECT(aKiki[to]);
	while (k) {
		unsigned long id;
		_BitScanForward(&id, k);
		k &= k-1;
		if (id < 16) {
			int z = to - NanohaTbl::Direction[id];
			if (from != z) attacker[natk++] = board[z] & ~GOTE;
			if (aKiki[z] & (0x10000u << id)) {
				z = SkipOverEMP(z, -NanohaTbl::Direction[id]);
				attacker[natk++] = board[z] & ~GOTE;
			}
		} else {
			int z = SkipOverEMP(to, -NanohaTbl::Direction[id]);
			if (from != z) attacker[natk++] = board[z] & ~GOTE;
			if (aKiki[z] & (0x1u << id)) {
				z = SkipOverEMP(z, -NanohaTbl::Direction[id]);
				attacker[natk++] = board[z] & ~GOTE;
			}
		}
	}
	attacker[natk] = 0;
	assert(natk < 32-1);

	// 価値によるソート.
	int i, j;
	int *p;
	int n;
	p = defender;
	n = ndef;
	for (i = 0; i < n - 1; i++) {
		for (j = i + 1; j < n; j++) {
			if (NanohaTbl::KomaValueEx[p[i]] > NanohaTbl::KomaValueEx[p[j]]) {
				int tmp = p[i];
				p[i] = p[j];
				p[j] = tmp;
			}
		}
	}
	p = attacker;
	n = natk;
	for (i = 0; i < n - 1; i++) {
		for (j = i + 1; j < n; j++) {
			if (NanohaTbl::KomaValueEx[p[i]] > NanohaTbl::KomaValueEx[p[j]]) {
				int tmp = p[i];
				p[i] = p[j];
				p[j] = tmp;
			}
		}
	}
#if 0
	std::cout << std::dec << "  " << std::setw(5) << std::left << UCI::move(m) << "\n";
	std::cout << "    atk(" << natk << "):";
	for (i = 0; i < natk; i++) {
		std::cout << attacker[i] << " ";
	}
	std::cout << std::endl;
	std::cout << "    def(" << ndef << "):";
	for (i = 0; i < ndef; i++) {
		std::cout << defender[i] << " ";
	}
	std::cout << std::endl;
#endif
	// balanceは捕り返した値になっている.
	return SEERec2(false, attacker, defender, balance, v); // counter
}
#else
bool Position::see_ge(Move m, Value v) const {

  assert(is_ok(m));

  // Castling moves are implemented as king capturing the rook so cannot be
  // handled correctly. Simply assume the SEE value is VALUE_ZERO that is always
  // correct unless in the rare case the rook ends up under attack.
  if (type_of(m) == CASTLING)
      return VALUE_ZERO >= v;

  Square from = from_sq(m), to = to_sq(m);
  PieceType nextVictim = type_of(piece_on(from));
  Color stm = ~color_of(piece_on(from)); // First consider opponent's move
  Value balance; // Values of the pieces taken by us minus opponent's ones
  Bitboard occupied, stmAttackers;

  if (type_of(m) == ENPASSANT)
  {
      occupied = SquareBB[to - pawn_push(~stm)]; // Remove the captured pawn
      balance = PieceValue[MG][PAWN];
  }
  else
  {
      balance = PieceValue[MG][piece_on(to)];
      occupied = 0;
  }

  if (balance < v)
      return false;

  if (nextVictim == KING)
      return true;

  balance -= PieceValue[MG][nextVictim];

  if (balance >= v)
      return true;

  bool relativeStm = true; // True if the opponent is to move
  occupied ^= pieces() ^ from ^ to;

  // Find all attackers to the destination square, with the moving piece removed,
  // but possibly an X-ray attacker added behind it.
  Bitboard attackers = attackers_to(to, occupied) & occupied;

  while (true)
  {
      stmAttackers = attackers & pieces(stm);

      // Don't allow pinned pieces to attack pieces except the king as long all
      // pinners are on their original square.
      if (!(st->pinnersForKing[stm] & ~occupied))
          stmAttackers &= ~st->blockersForKing[stm];

      if (!stmAttackers)
          return relativeStm;

      // Locate and remove the next least valuable attacker
      nextVictim = min_attacker<PAWN>(byTypeBB, to, stmAttackers, occupied, attackers);

      if (nextVictim == KING)
          return relativeStm == bool(attackers & pieces(~stm));

      balance += relativeStm ?  PieceValue[MG][nextVictim]
                             : -PieceValue[MG][nextVictim];

      relativeStm = !relativeStm;

      if (relativeStm == (balance >= v))
          return relativeStm;

      stm = ~stm;
  }
}
#endif

/// Position::is_draw() tests whether the position is drawn by 50-move rule
/// or by repetition. It does not detect stalemates.

#if defined(NANOHA)
bool Position::is_draw(int ply, int& ret) const {
	ret=0;
	int i = 5, e = st->pliesFromNull;

	if (i <= e)
	{
		StateInfo* stp = st->previous->previous;
		int rept = 0;
		// 王手をかけている
		bool cont_checking = (st->effect && stp->effect) ? true : false;
		// 王手をかけられている
		bool cont_checked  = (st->previous->effect && stp->previous->effect) ? true : false;

		do {
			stp = stp->previous->previous;
			if (stp->effect           == 0) cont_checking = false;
			if (stp->previous->effect == 0) cont_checked  = false;

			// [ToDo] 持ち駒は優越関係を見て、劣位になっていたら大幅マイナスする.
			if (stp->board_key == st->board_key && stp->hand == st->hand) {
				rept++;
				// 過去に3回(現局面含めて4回)出現していたら千日手.
				if (rept >= 3) {
					if (cont_checking) {ret =  1; return false; }
					if (cont_checked)  {ret = -1; return false; }
					return true;
				}
			}

			i +=2;

		} while (i < e);
	}
	return false;
}
#else
bool Position::is_draw(int ply) const {

  if (st->rule50 > 99 && (!checkers() || MoveList<LEGAL>(*this).size()))
      return true;

  int end = std::min(st->rule50, st->pliesFromNull);

  if (end < 4)
    return false;

  StateInfo* stp = st->previous->previous;
  int cnt = 0;

  for (int i = 4; i <= end; i += 2)
  {
      stp = stp->previous->previous;

      // At root position ply is 1, so return a draw score if a position
      // repeats once earlier but after or at the root, or repeats twice
      // strictly before the root.
      if (   stp->key == st->key
          && ++cnt + (ply - i > 0) == 2)
          return true;
  }

  return false;
}
#endif


/// Position::flip() flips position with the white and black sides reversed. This
/// is only useful for debugging e.g. for finding evaluation symmetry bugs.
#if !defined(NANOHA)
void Position::flip() {

  string f, token;
  std::stringstream ss(fen());

  for (Rank r = RANK_8; r >= RANK_1; --r) // Piece placement
  {
      std::getline(ss, token, r > RANK_1 ? '/' : ' ');
      f.insert(0, token + (f.empty() ? " " : "/"));
  }

  ss >> token; // Active color
  f += (token == "w" ? "B " : "W "); // Will be lowercased later

  ss >> token; // Castling availability
  f += token + " ";

  std::transform(f.begin(), f.end(), f.begin(),
                 [](char c) { return char(islower(c) ? toupper(c) : tolower(c)); });

  ss >> token; // En passant square
  f += (token == "-" ? token : token.replace(1, 1, token[1] == '3' ? "6" : "3"));

  std::getline(ss, token); // Half and full moves
  f += token;

  set(f, is_chess960(), st, this_thread());

  assert(pos_is_ok());
}
#endif


/// Position::pos_is_ok() performs some consistency checks for the position object.
/// This is meant to be helpful when debugging.

bool Position::pos_is_ok(int* failedStep) const {

#if defined(NANOHA)
	const bool Fast = false; // Quick (default) or full check?

	enum { Default, King, Board, State, PieceCount };

	for (int step = Default; step <= (Fast ? Default : PieceCount); step++) {
		if (failedStep) *failedStep = step;

		if (step == Default) {
			if (sideToMove != WHITE && sideToMove != BLACK) return false;
			if (king_square(WHITE) != 0 && piece_on(king_square(WHITE)) != GOU) return false;
			if (king_square(BLACK) != 0 && piece_on(king_square(BLACK)) != SOU) return false;
		}

		if (step == King) {
			int b_cnt = 0, w_cnt = 0;
			for (Square sq = SQ_11; sq <= SQ_99; ++sq) {
				if (board[sq] == SOU) b_cnt++;
				if (board[sq] == GOU) w_cnt++;
			}
			if (b_cnt > 1 || w_cnt > 1) return false;
			if (b_cnt == 0 && king_square(BLACK) != 0) return false;
			if (w_cnt == 0 && king_square(WHITE) != 0) return false;
			if (b_cnt == 1 && king_square(BLACK) == 0) return false;
			if (w_cnt == 1 && king_square(WHITE) == 0) return false;

			// 相手玉を取れる状態にある.
			if (at_checking()) return false;

			// 自玉に3つ以上の利きがついている.
			Square ksq = king_square(sideToMove);
			if (ksq != 0) {
				effect_t efft = effect[~sideToMove][ksq];
				efft = (efft & (efft - 1));
				efft = (efft & (efft - 1));
				if (efft > 0) return false;
			}
		}

		if (step == Board) {
		}

		if (step == State) {
			StateInfo si;
			set_state(&si);

			if (   st->board_key != si.board_key
			    || st->hand_key != si.hand_key
			    || st->material != si.material
			    || st->hand != si.hand
			    || si.hand != handValue_of_side()
			    || st->effect != si.effect) {
				sync_cout <<   "side     : " << (sideToMove == BLACK ? "black" : "white")
				          << "\nKey      : 0x" << std::hex << st->board_key  << ", 0x" << si.board_key
				          << "\nHand Key : 0x" << std::hex << st->hand_key   << ", 0x" << si.hand_key
				          << "\nMaterial : "   << std::dec << st->material << ", "   << si.material
				          << "\nHand     : 0x" << std::hex << st->hand     << ", 0x" << si.hand
				          << "\neffect   : 0x" << std::hex << st->effect   << ", 0x" << si.effect
				          << std::dec << sync_endl;
				return false;
			}
			if (sideToMove == BLACK && (si.board_key & 1) != 1) return false;
			if (sideToMove == WHITE && (si.board_key & 1) != 0) return false;
		}

		if (step == PieceCount) {
			// 駒の枚数のチェック
			// 駒の枚数が一致すること！(歩なら18枚)
			// ※駒落ちや一部の詰将棋では枚数の判断は != でなく > で判断する必要あり.
			int sum, pc_cnt[PIECE_NB] = {0};
			for (Square sq = SQ_11; sq <= SQ_99; ++sq) {
				Piece pc = board[sq];
				if (pc == WALL) continue;
				if (pc > GRY) return false;
				pc_cnt[pc]++;
			}
			// 歩
			sum = pc_cnt[SFU] + pc_cnt[GFU] + pc_cnt[STO] + pc_cnt[GTO]
			    + hand[BLACK].getFU() + hand[WHITE].getFU();
			if (sum != (KNE_FU - KNS_FU + 1)) return false;
			// 香
			sum = pc_cnt[SKY] + pc_cnt[GKY] + pc_cnt[SNY] + pc_cnt[GNY]
			    + hand[BLACK].getKY() + hand[WHITE].getKY();
			if (sum != (KNE_KY - KNS_KY + 1)) return false;
			// 桂
			sum = pc_cnt[SKE] + pc_cnt[GKE] + pc_cnt[SNK] + pc_cnt[GNK]
			    + hand[BLACK].getKE() + hand[WHITE].getKE();
			if (sum != (KNE_KE - KNS_KE + 1)) return false;
			// 銀
			sum = pc_cnt[SGI] + pc_cnt[GGI] + pc_cnt[SNG] + pc_cnt[GNG]
			    + hand[BLACK].getGI() + hand[WHITE].getGI();
			if (sum != (KNE_GI - KNS_GI + 1)) return false;
			// 金
			sum = pc_cnt[SKI] + pc_cnt[GKI]
			    + hand[BLACK].getKI() + hand[WHITE].getKI();
			if (sum != (KNE_KI - KNS_KI + 1)) return false;
			// 角
			sum = pc_cnt[SKA] + pc_cnt[GKA] + pc_cnt[SUM] + pc_cnt[GUM]
			    + hand[BLACK].getKA() + hand[WHITE].getKA();
			if (sum != (KNE_KA - KNS_KA + 1)) return false;
			// 飛
			sum = pc_cnt[SHI] + pc_cnt[GHI] + pc_cnt[SRY] + pc_cnt[GRY]
			    + hand[BLACK].getHI() + hand[WHITE].getHI();
			if (sum != (KNE_HI - KNS_HI + 1)) return false;
		}
	}
#else
  const bool Fast = true; // Quick (default) or full check?

  enum { Default, King, Bitboards, State, Lists, Castling };

  for (int step = Default; step <= (Fast ? Default : Castling); step++)
  {
      if (failedStep)
          *failedStep = step;

      if (step == Default)
          if (   (sideToMove != WHITE && sideToMove != BLACK)
              || piece_on(square<KING>(WHITE)) != W_KING
              || piece_on(square<KING>(BLACK)) != B_KING
              || (   ep_square() != SQ_NONE
                  && relative_rank(sideToMove, ep_square()) != RANK_6))
              return false;

      if (step == King)
          if (   std::count(board, board + SQUARE_NB, W_KING) != 1
              || std::count(board, board + SQUARE_NB, B_KING) != 1
              || attackers_to(square<KING>(~sideToMove)) & pieces(sideToMove))
              return false;

      if (step == Bitboards)
      {
          if (  (pieces(WHITE) & pieces(BLACK))
              ||(pieces(WHITE) | pieces(BLACK)) != pieces())
              return false;

          for (PieceType p1 = PAWN; p1 <= KING; ++p1)
              for (PieceType p2 = PAWN; p2 <= KING; ++p2)
                  if (p1 != p2 && (pieces(p1) & pieces(p2)))
                      return false;
      }

      if (step == State)
      {
          StateInfo si = *st;
          set_state(&si);
          if (std::memcmp(&si, st, sizeof(StateInfo)))
              return false;
      }

      if (step == Lists)
      {
          for (Piece pc : Pieces)
          {
              if (pieceCount[pc] != popcount(pieces(color_of(pc), type_of(pc))))
                  return false;

              for (int i = 0; i < pieceCount[pc]; ++i)
                  if (board[pieceList[pc][i]] != pc || index[pieceList[pc][i]] != i)
                      return false;
          }
          if (pieceCount[PAWN] > FILE_NB)
              return false;
      }

      if (step == Castling)
          for (Color c = WHITE; c <= BLACK; ++c)
              for (CastlingSide s = KING_SIDE; s <= QUEEN_SIDE; s = CastlingSide(s + 1))
              {
                  if (!can_castle(c | s))
                      continue;

                  if (   piece_on(castlingRookSquare[c | s]) != make_piece(c, ROOK)
                      || castlingRightsMask[castlingRookSquare[c | s]] != (c | s)
                      ||(castlingRightsMask[square<KING>(c)] & (c | s)) != (c | s))
                      return false;
              }
  }
#endif

  return true;
}
