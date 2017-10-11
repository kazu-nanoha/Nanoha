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

#ifndef POSITION_H_INCLUDED
#define POSITION_H_INCLUDED

#include <cassert>
#include <deque>
#include <memory> // For std::unique_ptr
#include <string>

#if !defined(NANOHA)
#include "bitboard.h"
#endif
#include "types.h"

#if defined(NANOHA)
struct ExtMove;
#endif

#if defined(NANOHA)
namespace Zobrist {
extern Key psq[PIECE_NB][0x100];
extern Key hand[PIECE_NB][32];
extern Key side;		// 手番を区別する
}
#endif

/// StateInfo struct stores information needed to restore a Position object to
/// its previous state when we retract a move. Whenever a move is made on the
/// board (by calling Position::do_move), a StateInfo object must be passed.

struct StateInfo {
#if defined(NANOHA)
  // Copied when making a move
  int    pliesFromNull;
  int material;

  uint32_t hand;
  uint32_t effect;
  Key hand_key;

  // Not copied when making a move (will be recomputed anyhow)
  Key board_key;
  Piece      capturedPiece;
  StateInfo* previous;
#else
  // Copied when making a move
  Key    pawnKey;
  Key    materialKey;
  Value  nonPawnMaterial[COLOR_NB];
  int    castlingRights;
  int    rule50;
  int    pliesFromNull;
  Score  psq;
  Square epSquare;

  // Not copied when making a move (will be recomputed anyhow)
  Key        key;
  Bitboard   checkersBB;
  Piece      capturedPiece;
  StateInfo* previous;
  Bitboard   blockersForKing[COLOR_NB];
  Bitboard   pinnersForKing[COLOR_NB];
  Bitboard   checkSquares[PIECE_TYPE_NB];
#endif
};

#if defined(NANOHA)
// 初期化関数
extern void init_application_once();	// 実行ファイル起動時に行う初期化.
#endif

// In a std::deque references to elements are unaffected upon resizing
typedef std::unique_ptr<std::deque<StateInfo>> StateListPtr;


/// Position class stores information regarding the board representation as
/// pieces, side to move, hash keys, castling info, etc. Important methods are
/// do_move() and undo_move(), used by the search to update node info when
/// traversing the search tree.
class Thread;

class Position {
public:
  static void init();

  Position() = default;
  Position(const Position&) = delete;
  Position& operator=(const Position&) = delete;

  // FEN string input/output
#if defined(NANOHA)
  Position& set(const std::string& fenStr, StateInfo* si, Thread* th);
#else
  Position& set(const std::string& fenStr, bool isChess960, StateInfo* si, Thread* th);
  Position& set(const std::string& code, Color c, StateInfo* si);
#endif
  const std::string fen() const;

#if defined(NANOHA)
	// Text input/output
	void print_csa(Move m = MOVE_NONE) const;
	void print(Move m = MOVE_NONE) const;
	// Compare
	bool eq_board(const Position &a) const;
	bool operator == (const Position &a) const;
	bool operator != (const Position &a) const;
#endif

  // Position representation
#if !defined(NANOHA)
  Bitboard pieces() const;
  Bitboard pieces(PieceType pt) const;
  Bitboard pieces(PieceType pt1, PieceType pt2) const;
  Bitboard pieces(Color c) const;
  Bitboard pieces(Color c, PieceType pt) const;
  Bitboard pieces(Color c, PieceType pt1, PieceType pt2) const;
#endif
  Piece piece_on(Square s) const;
#if !defined(NANOHA)
  Square ep_square() const;
#endif
  bool empty(Square s) const;
#if defined(NANOHA)
	int pin_on(Square s) const;
#else
  template<PieceType Pt> int count(Color c) const;
  template<PieceType Pt> int count() const;
  template<PieceType Pt> const Square* squares(Color c) const;
  template<PieceType Pt> Square square(Color c) const;

  // Castling
  int can_castle(Color c) const;
  int can_castle(CastlingRight cr) const;
  bool castling_impeded(CastlingRight cr) const;
  Square castling_rook_square(CastlingRight cr) const;

  // Checking
  Bitboard checkers() const;
  Bitboard discovered_check_candidates() const;
  Bitboard pinned_pieces(Color c) const;
  Bitboard check_squares(PieceType pt) const;

  // Attacks to/from a given square
  Bitboard attackers_to(Square s) const;
  Bitboard attackers_to(Square s, Bitboard occupied) const;
  Bitboard attacks_from(Piece pc, Square s) const;
  template<PieceType> Bitboard attacks_from(Square s) const;
  template<PieceType> Bitboard attacks_from(Square s, Color c) const;
  Bitboard slider_blockers(Bitboard sliders, Square s, Bitboard& pinners) const;
#endif

#if defined(NANOHA)
	// Checking
	bool in_check() const;
	bool at_checking() const;	// 王手をかけている状態か？

	// Current king position for each color
	//  Stockfish2で使っていたが、今はなくなっている関数.
	Square king_square(Color c) const;
#endif

  // Properties of moves
  bool legal(Move m) const;
  bool pseudo_legal(const Move m) const;
  bool capture(Move m) const;
  bool capture_or_promotion(Move m) const;
  bool gives_check(Move m) const;
#if defined(NANOHA)
	bool is_evasion(Move m) const;
	bool is_double_pawn(const Color us, int to) const;		// 二歩か？
	bool is_pawn_drop_mate(const Color us, int to) const;	// 打ち歩詰めか？
	bool move_attacks_square(Move m, Square s) const;
#else
  bool advanced_pawn_push(Move m) const;
  Piece moved_piece(Move m) const;
#endif
  Piece captured_piece() const;

#if !defined(NANOHA)
  // Piece specific
  bool pawn_passed(Color c, Square s) const;
  bool opposite_bishops() const;
#endif

  // Doing and undoing moves
  void do_move(Move m, StateInfo& newSt);
#if defined(NANOHA)
	void do_drop(Move m);
	void undo_drop(Move m);
#else
  void do_move(Move m, StateInfo& newSt, bool givesCheck);
#endif
  void undo_move(Move m);
  void do_null_move(StateInfo& newSt);
  void undo_null_move();

#if defined(NANOHA)
	// 手生成で使うための関数
	// 指定位置から指定方向に何かある位置まで探す(WALL or 先手駒 or 後手駒の位置になる)
	int SkipOverEMP(int pos, const int dir) const;
	// 利きの更新
	void add_effect(const int z);					// 位置zの駒の利きを反映する
	void del_effect(const int z, const Piece k);	
	// 利きの演算
	template<Color>
	void add_effect_straight(const int z, const int dir, const uint32_t bit);
	template<Color>
	void del_effect_straight(const int z, const int dir, const uint32_t bit);
#define AddKikiDirS	add_effect_straight<BLACK>
#define AddKikiDirG	add_effect_straight<WHITE>
#define DelKikiDirS	del_effect_straight<BLACK>
#define DelKikiDirG	del_effect_straight<WHITE>
	// ピン情報更新
	template <Color> void add_pin_info(const int dir);
	template <Color> void del_pin_info(const int dir);
#define AddPinInfS	add_pin_info<BLACK>
#define AddPinInfG	add_pin_info<WHITE>
#define DelPinInfS	del_pin_info<BLACK>
#define DelPinInfG	del_pin_info<WHITE>

public:
	// 手生成
	template<Color> ExtMove* add_straight(ExtMove* mlist, const int from, const int dir) const;
	template<Color> ExtMove* add_move(ExtMove* mlist, const int from, const int dir) const;
#define add_straightB	add_straight<BLACK>
#define add_straightW	add_straight<WHITE>
#define add_moveB	add_move<BLACK>
#define add_moveW	add_move<WHITE>
	ExtMove* gen_move_to(const Color us, ExtMove* mlist, int to) const;		// toに動く手の生成
	ExtMove* gen_drop_to(const Color us, ExtMove* mlist, int to) const;		// toに駒を打つ手の生成
	template <Color> ExtMove* gen_drop(ExtMove* mlist) const;			// 駒を打つ手の生成
	ExtMove* gen_move_king(const Color us, ExtMove* mlist, int pindir = 0) const;			//玉の動く手の生成
	ExtMove* gen_king_noncapture(const Color us, ExtMove* mlist, int pindir = 0) const;			//玉の動く手の生成
	ExtMove* gen_move_from(const Color us, ExtMove* mlist, int from, int pindir = 0) const;		//fromから動く手の生成

	template <Color> ExtMove* generate_capture(ExtMove* mlist) const;
	template <Color> ExtMove* generate_non_capture(ExtMove* mlist) const;
	template <Color> ExtMove* generate_evasion(ExtMove* mlist) const;
	template <Color> ExtMove* generate_non_evasion(ExtMove* mlist) const;
	template <Color> ExtMove* generate_legal(ExtMove* mlist) const;

	// 王手関連
	template <Color> ExtMove* gen_check_long(ExtMove* mlist) const;
	template <Color> ExtMove* gen_check_short(ExtMove* mlist) const;
	template <Color> ExtMove* gen_check_drop(ExtMove* mlist, bool &bUchifudume) const;
	template <Color> ExtMove* generate_check(ExtMove* mlist, bool &bUchifudume) const;

	// 3手詰め用の手生成
	template <Color> ExtMove* gen_check_drop3(ExtMove* mlist, bool &bUchifudume) const;
	template <Color> ExtMove* generate_check3(ExtMove* mlist, bool &bUchifudume) const;			// 王手生成
	// 王手回避手の生成(3手詰め残り2手用)
	ExtMove *generate_evasion_rest2(const Color us, ExtMove *mBuf, effect_t effect, int &Ai);
	ExtMove *generate_evasion_rest2_MoveAi(const Color us, ExtMove *mBuf, effect_t effect);
	ExtMove *generate_evasion_rest2_DropAi(const Color us, ExtMove *mBuf, effect_t effect, int &check_pos);

	// 局面の評価
	static void init_evaluate();
#if defined(EVAL_APERY) || defined(EVAL_TWIG)
	int make_list_apery(int list0[], int list1[], int nlist) const;
#elif defined(EVAL_NANO) || defined(EVAL_MINI)
	int make_list(int * pscore, int list0[]) const;
#endif
	int evaluate(const Color us) const;

	// 稲庭判定(bInaniwa にセットするため const でない)
	bool IsInaniwa(const Color us);

	// 機能：勝ち宣言できるかどうか判定する
	bool IsKachi(const Color us) const;

	// 王手かどうか？
	bool is_check_move(const Color us, Move move) const;

	// 高速1手詰め
	static void initMate1ply();
	template <Color us> uint32_t infoRound8King() const;
	// 駒打ちにより利きを遮るか？
	template <Color us> uint32_t chkInterceptDrop(const uint32_t info, const int kpos, const int i, const int kind) const;
	// 駒移動により利きを遮るか？
	template <Color us> uint32_t chkInterceptMove(const uint32_t info, const int kpos, const int i, const int from, const int kind) const;
	// 駒打による一手詰の判定
	template <Color us> uint32_t CheckMate1plyDrop(const uint32_t info, Move &m) const;
	int CanAttack(const int kind, const int from, const int to) const;
	// 移動による一手詰の判定
	template <Color us> uint32_t CheckMate1plyMove(const uint32_t info, Move &m, const int check = 0) const;
	template <Color us> int Mate1ply(Move &m, uint32_t &info);

	// 3手詰め
	int Mate3(const Color us, Move &m);
//	int EvasionRest2(const Color us, ExtMove *antichecks, unsigned int &PP, unsigned int &DP, int &dn);
	int EvasionRest2(const Color us, ExtMove *antichecks);

	template<Color>
	effect_t exist_effect(int pos) const;				// 利き
	template<Color us>
	Square sq_king() const {return Square((us == BLACK) ? knpos[1] : knpos[2]);}

	// 局面をHuffman符号化する
	int EncodeHuffman(unsigned char buf[32]) const;
	int DecodeHuffman(unsigned char buf[32]);
#endif

  // Static Exchange Evaluation
  bool see_ge(Move m, Value value) const;

  // Accessing hash keys
  Key key() const;
  Key key_after(Move m) const;
#if defined(NANOHA)
  Key board_key() const;
  Key hand_key() const;
  Key calc_hash_no_move(Move) const;
#else
  Key material_key() const;
  Key pawn_key() const;
#endif

  // Other properties of the position
  Color side_to_move() const;
#if !defined(NANOHA)
  Phase game_phase() const;
#endif
  int game_ply() const;
#if !defined(NANOHA)
  bool is_chess960() const;
#endif
  Thread* this_thread() const;
  uint64_t nodes_searched() const;
#if defined(NANOHA)
  uint64_t tnodes_searched() const;
	// 戻り値trueは千日手
	// 戻り値falseでret==0はなにもなし
	// 戻り値falseでret==-1は連続王手の千日手
	bool is_draw(int ply, int& ret) const;
#else
  bool is_draw(int ply) const;
  int rule50_count() const;
  Score psq_score() const;
  Value non_pawn_material(Color c) const;
  Value non_pawn_material() const;
#endif
#if defined(NANOHA)
	uint32_t handValue_of_side() const {return hand[sideToMove].h; }
	template<Color us> uint32_t handValue() const {return hand[us].h; }
	int get_material() const { return st->material; }
	static unsigned char relate_pos(int z1, int z2) {return DirTbl[z1][z2];}	// z1とz2の位置関係.
#endif

  // Position consistency check, for debugging
  bool pos_is_ok(int* failedStep = nullptr) const;
  void flip();

private:
  // Initialization helpers (used while setting up a position)
#if !defined(NANOHA)
  void set_castling_right(Color c, Square rfrom);
#endif
  void set_state(StateInfo* si) const;
#if !defined(NANOHA)
  void set_check_info(StateInfo* si) const;
#endif

  // Other helpers
#if defined(NANOHA)
	void init_position(const unsigned char board_ori[9][9], const int Mochigoma_ori[]);
	void make_pin_info();
	void init_effect();
	void compute_key(StateInfo *si) const;
	int compute_material() const;
#else
  void put_piece(Piece pc, Square s);
  void remove_piece(Piece pc, Square s);
  void move_piece(Piece pc, Square from, Square to);
  template<bool Do>
  void do_castling(Color us, Square from, Square& to, Square& rfrom, Square& rto);
#endif

  // Data members
#if !defined(NANOHA)
  Piece board[SQUARE_NB];
  Bitboard byTypeBB[PIECE_TYPE_NB];
  Bitboard byColorBB[COLOR_NB];
  int pieceCount[PIECE_NB];
  Square pieceList[PIECE_NB][16];
  int index[SQUARE_NB];
  int castlingRightsMask[SQUARE_NB];
  Square castlingRookSquare[CASTLING_RIGHT_NB];
  Bitboard castlingPath[CASTLING_RIGHT_NB];
#else
	Piece banpadding[16*2];		// Padding
	Piece board[16*12];			// 盤情報 (駒種類)
	PieceNumber_t komano[16*12];		// 盤情報 (駒番号)
	effect_t effect[2][16*12];				// 利き
#define effectB	effect[BLACK]
#define effectW	effect[WHITE]

#define IsCheckS()	EXIST_EFFECT(effectW[kingS])	/* 先手玉に王手がかかっているか? */
#define IsCheckG()	EXIST_EFFECT(effectB[kingG])	/* 後手玉に王手がかかっているか? */
	int pin[16*10];					// ピン(先手と後手両用)
	Hand hand[2];					// 持駒
#define handS	hand[BLACK]
#define handG	hand[WHITE]
	Piece knkind[MAX_PIECENUMBER+1];	// knkind[num] : 駒番号numの駒種類(EMP(0x00) ～ GRY(0x1F))
	uint8_t knpos[MAX_PIECENUMBER+1];		// knpos[num]  : 駒番号numの盤上の座標(0:未使用、1:先手持駒、2:後手持駒、0x11-0x99:盤上)
									//    駒番号
#define KNS_SOU	1
#define KNE_SOU	1
								//        1    : 先手玉
#define KNS_GOU	2
#define KNE_GOU	2
								//        2    : 後手玉
#define KNS_HI	3
#define KNE_HI	4
								//        3- 4 : 飛
#define KNS_KA	5
#define KNE_KA	6
								//        5- 6 : 角
#define KNS_KI	7
#define KNE_KI	10
								//        7-10 : 金
#define KNS_GI	11
#define KNE_GI	14
								//       11-14 : 銀
#define KNS_KE	15
#define KNE_KE	18
								//       15-18 : 桂
#define KNS_KY	19
#define KNE_KY	22
								//       19-22 : 香
#define KNS_FU	23
#define KNE_FU	40
								//       23-40 : 歩
#define kingS	sq_king<BLACK>()
#define kingG	sq_king<WHITE>()
#define hiPos	(&knpos[ 3])
#define kaPos	(&knpos[ 5])
#define kyPos	(&knpos[19])
#define IsHand(x)	((x) <  0x11)
#define OnBoard(x)	((x) >= 0x11)
	bool bInaniwa;
#endif
  uint64_t nodes;
#if defined(NANOHA)
	uint64_t tnodes;
	unsigned long count_Mate1plyDrop;		// 駒打ちで詰んだ回数
	unsigned long count_Mate1plyMove;		// 駒移動で詰んだ回数
	unsigned long count_Mate3ply;			// Mate3()で詰んだ回数
#endif
  int gamePly;
  Color sideToMove;
  Thread* thisThread;
  StateInfo* st;
#if !defined(NANOHA)
  bool chess960;
#endif
	// Static variables
#if defined(NANOHA)
	static unsigned char DirTbl[0xA0][0x100];	// 方向用[from][to]

	// 王手生成用テーブル
	static const struct ST_OuteMove2 {
		int from;
		struct {
			int to;
			int narazu;
			int nari;
		} to[6];
	} OuteMove2[32];
	static uint32_t TblMate1plydrop[0x10000];	// 駒打ちで詰む判断をするテーブル.

	friend void init_application_once();	// 実行ファイル起動時に行う初期化.
#if defined(USE_DFPN)
	friend class SearchMateDFPN;
#endif
#endif
};

extern std::ostream& operator<<(std::ostream& os, const Position& pos);

inline Color Position::side_to_move() const {
  return sideToMove;
}

inline bool Position::empty(Square s) const {
  return board[s] == NO_PIECE;
}

inline Piece Position::piece_on(Square s) const {
  return board[s];
}

#if defined(NANOHA)
inline int Position::pin_on(Square s) const {
	return pin[s];
}
#endif

#if !defined(NANOHA)
inline Piece Position::moved_piece(Move m) const {
  return board[from_sq(m)];
}

inline Bitboard Position::pieces() const {
  return byTypeBB[ALL_PIECES];
}

inline Bitboard Position::pieces(PieceType pt) const {
  return byTypeBB[pt];
}

inline Bitboard Position::pieces(PieceType pt1, PieceType pt2) const {
  return byTypeBB[pt1] | byTypeBB[pt2];
}

inline Bitboard Position::pieces(Color c) const {
  return byColorBB[c];
}

inline Bitboard Position::pieces(Color c, PieceType pt) const {
  return byColorBB[c] & byTypeBB[pt];
}

inline Bitboard Position::pieces(Color c, PieceType pt1, PieceType pt2) const {
  return byColorBB[c] & (byTypeBB[pt1] | byTypeBB[pt2]);
}

template<PieceType Pt> inline int Position::count(Color c) const {
  return pieceCount[make_piece(c, Pt)];
}

template<PieceType Pt> inline int Position::count() const {
  return pieceCount[make_piece(WHITE, Pt)] + pieceCount[make_piece(BLACK, Pt)];
}

template<PieceType Pt> inline const Square* Position::squares(Color c) const {
  return pieceList[make_piece(c, Pt)];
}

template<PieceType Pt> inline Square Position::square(Color c) const {
  assert(pieceCount[make_piece(c, Pt)] == 1);
  return pieceList[make_piece(c, Pt)][0];
}

inline Square Position::ep_square() const {
  return st->epSquare;
}

inline int Position::can_castle(CastlingRight cr) const {
  return st->castlingRights & cr;
}

inline int Position::can_castle(Color c) const {
  return st->castlingRights & ((WHITE_OO | WHITE_OOO) << (2 * c));
}

inline bool Position::castling_impeded(CastlingRight cr) const {
  return byTypeBB[ALL_PIECES] & castlingPath[cr];
}

inline Square Position::castling_rook_square(CastlingRight cr) const {
  return castlingRookSquare[cr];
}

template<PieceType Pt>
inline Bitboard Position::attacks_from(Square s) const {
  return  Pt == BISHOP || Pt == ROOK ? attacks_bb<Pt>(s, byTypeBB[ALL_PIECES])
        : Pt == QUEEN  ? attacks_from<ROOK>(s) | attacks_from<BISHOP>(s)
        : StepAttacksBB[Pt][s];
}

template<>
inline Bitboard Position::attacks_from<PAWN>(Square s, Color c) const {
  return StepAttacksBB[make_piece(c, PAWN)][s];
}

inline Bitboard Position::attacks_from(Piece pc, Square s) const {
  return attacks_bb(pc, s, byTypeBB[ALL_PIECES]);
}

inline Bitboard Position::attackers_to(Square s) const {
  return attackers_to(s, byTypeBB[ALL_PIECES]);
}

inline Bitboard Position::checkers() const {
  return st->checkersBB;
}

inline Bitboard Position::discovered_check_candidates() const {
  return st->blockersForKing[~sideToMove] & pieces(sideToMove);
}

inline Bitboard Position::pinned_pieces(Color c) const {
  return st->blockersForKing[c] & pieces(c);
}

inline Bitboard Position::check_squares(PieceType pt) const {
  return st->checkSquares[pt];
}

inline bool Position::pawn_passed(Color c, Square s) const {
  return !(pieces(~c, PAWN) & passed_pawn_mask(c, s));
}

inline bool Position::advanced_pawn_push(Move m) const {
  return   type_of(moved_piece(m)) == PAWN
        && relative_rank(sideToMove, from_sq(m)) > RANK_4;
}
#endif

#if defined(NANOHA)
inline bool Position::in_check() const {
	Square sq = king_square(sideToMove);
	if (sq == SQ_NULL) return false;
	Color them = ~sideToMove;
	return EXIST_EFFECT(effect[them][sq]);
}

inline bool Position::at_checking() const {
	Square sq = (sideToMove == BLACK) ? sq_king<WHITE>() : sq_king<BLACK>();
	if (sq == SQ_NULL) return false;
	return EXIST_EFFECT(effect[sideToMove][sq]);
}

inline Square Position::king_square(Color c) const {
	return (c == BLACK) ? Square(sq_king<BLACK>()) : Square(sq_king<WHITE>());
}
#endif

inline Key Position::key() const {
#if defined(NANOHA)
#if !defined(NDEBUG)
	// BLACK: bit0 == 1, WHITE: bit0 == 0
	if (sideToMove == BLACK) {
		if ((st->board_key & 1) == 0) {
			MYABORT();
		}
	} else {
		if ((st->board_key & 1) != 0) {
			MYABORT();
		}
	}
#endif
	return st->board_key ^ st->hand_key;
#else
  return st->key;
#endif
}

#if defined(NANOHA)
inline Key Position::board_key() const {
	Key k = st->board_key;
#if !defined(NDEBUG)
	// BLACK: bit0 == 1, WHITE: bit0 == 0
	if (sideToMove == BLACK) {
		if ((st->board_key & 1) == 0) {
			MYABORT();
		}
	} else {
		if ((st->board_key & 1) != 0) {
			MYABORT();
		}
	}
#endif
	return k;
}

inline Key Position::hand_key() const {
	Key k = st->hand_key;
#if !defined(NDEBUG)
	// BLACK: bit0 == 1, WHITE: bit0 == 0
	if (sideToMove == BLACK) {
		if ((st->board_key & 1) == 0) {
			MYABORT();
		}
	} else {
		if ((st->board_key & 1) != 0) {
			MYABORT();
		}
	}
#endif
	return k;
}
#endif

#if !defined(NANOHA)
inline Key Position::pawn_key() const {
  return st->pawnKey;
}

inline Key Position::material_key() const {
  return st->materialKey;
}

inline Score Position::psq_score() const {
  return st->psq;
}

inline Value Position::non_pawn_material(Color c) const {
  return st->nonPawnMaterial[c];
}

inline Value Position::non_pawn_material() const {
  return st->nonPawnMaterial[WHITE] + st->nonPawnMaterial[BLACK];
}
#endif

inline int Position::game_ply() const {
  return gamePly;
}

#if !defined(NANOHA)
inline int Position::rule50_count() const {
  return st->rule50;
}
#endif

inline uint64_t Position::nodes_searched() const {
  return nodes;
}

#if defined(NANOHA)
inline uint64_t Position::tnodes_searched() const {
	return tnodes;
}
#endif

#if !defined(NANOHA)
inline bool Position::opposite_bishops() const {
  return   pieceCount[W_BISHOP] == 1
        && pieceCount[B_BISHOP] == 1
        && opposite_colors(square<BISHOP>(WHITE), square<BISHOP>(BLACK));
}

inline bool Position::is_chess960() const {
  return chess960;
}
#endif

inline bool Position::capture_or_promotion(Move m) const {
  assert(is_ok(m));
#if defined(NANOHA)
	return capture_of(m) != EMP || is_promotion(m);
#else
  return type_of(m) != NORMAL ? type_of(m) != CASTLING : !empty(to_sq(m));
#endif
}

inline bool Position::capture(Move m) const {
  assert(is_ok(m));
  // Castling is encoded as "king captures rook"
#if defined(NANOHA)
  return !empty(to_sq(m));
#else
  return (!empty(to_sq(m)) && type_of(m) != CASTLING) || type_of(m) == ENPASSANT;
#endif
}

inline Piece Position::captured_piece() const {
  return st->capturedPiece;
}

inline Thread* Position::this_thread() const {
  return thisThread;
}

#if !defined(NANOHA)
inline void Position::put_piece(Piece pc, Square s) {

  board[s] = pc;
  byTypeBB[ALL_PIECES] |= s;
  byTypeBB[type_of(pc)] |= s;
  byColorBB[color_of(pc)] |= s;
  index[s] = pieceCount[pc]++;
  pieceList[pc][index[s]] = s;
  pieceCount[make_piece(color_of(pc), ALL_PIECES)]++;
}

inline void Position::remove_piece(Piece pc, Square s) {

  // WARNING: This is not a reversible operation. If we remove a piece in
  // do_move() and then replace it in undo_move() we will put it at the end of
  // the list and not in its original place, it means index[] and pieceList[]
  // are not invariant to a do_move() + undo_move() sequence.
  byTypeBB[ALL_PIECES] ^= s;
  byTypeBB[type_of(pc)] ^= s;
  byColorBB[color_of(pc)] ^= s;
  /* board[s] = NO_PIECE;  Not needed, overwritten by the capturing one */
  Square lastSquare = pieceList[pc][--pieceCount[pc]];
  index[lastSquare] = index[s];
  pieceList[pc][index[lastSquare]] = lastSquare;
  pieceList[pc][pieceCount[pc]] = SQ_NONE;
  pieceCount[make_piece(color_of(pc), ALL_PIECES)]--;
}

inline void Position::move_piece(Piece pc, Square from, Square to) {

  // index[from] is not updated and becomes stale. This works as long as index[]
  // is accessed just by known occupied squares.
  Bitboard from_to_bb = SquareBB[from] ^ SquareBB[to];
  byTypeBB[ALL_PIECES] ^= from_to_bb;
  byTypeBB[type_of(pc)] ^= from_to_bb;
  byColorBB[color_of(pc)] ^= from_to_bb;
  board[from] = NO_PIECE;
  board[to] = pc;
  index[to] = index[from];
  pieceList[pc][index[to]] = to;
}

inline void Position::do_move(Move m, StateInfo& newSt) {
  do_move(m, newSt, gives_check(m));
}
#endif

#if defined(NANOHA)
// 以下、なのは固有制御
template<Color us>
effect_t Position::exist_effect(int pos) const {
	return effect[us][pos];
}

// 指定位置から指定方向に何かある位置まで探す(WALL or 先手駒 or 後手駒の位置になる)
inline int Position::SkipOverEMP(int pos, const int dir) const {
	if (board[pos += dir] != EMP) return pos;
	if (board[pos += dir] != EMP) return pos;
	if (board[pos += dir] != EMP) return pos;
	if (board[pos += dir] != EMP) return pos;
	if (board[pos += dir] != EMP) return pos;
	if (board[pos += dir] != EMP) return pos;
	if (board[pos += dir] != EMP) return pos;
	if (board[pos += dir] != EMP) return pos;
	return pos + dir;
}

// 二歩チェック(true:posの筋に歩がある＝二歩になる、false:posの筋に歩がない)
inline bool Position::is_double_pawn(const Color us, const int pos) const
{
	const Piece *p = &(board[(pos & ~0x0F)+1]);
	Piece pawn = (us == BLACK) ? SFU : GFU;
	for (int i = 0; i < 9; i++) {
		if (*p++ == pawn) {
			return true;
		}
	}
	return false;
}

// 利き関連
template<Color turn>
inline void Position::add_effect_straight(const int z, const int dir, const uint32_t bit)
{
	int zz = z;
	do {
		zz += dir;
		effect[turn][zz] |= bit;
	} while(board[zz] == EMP);

	// 利きは相手玉を一つだけ貫く
	const int enemyKing = (turn == BLACK) ? GOU : SOU;
	if (board[zz] == enemyKing) {
		zz += dir;
		if (board[zz] != WALL) {
			effect[turn][zz] |= bit;
		}
	}
}
template<Color turn>
inline void Position::del_effect_straight(const int z, const int dir, const uint32_t bit)
{
	int zz = z;
	do {
		zz += dir; effect[turn][zz] &= bit;
	} while(board[zz] == EMP);

	// 利きは相手玉を一つだけ貫く
	const int enemyKing = (turn == BLACK) ? GOU : SOU;
	if (board[zz] == enemyKing) {
		zz += dir;
		if (board[zz] != WALL) {
			effect[turn][zz] &= bit;
		}
	}
}

// ピン情報更新
template<Color turn>
inline void Position::add_pin_info(const int dir) {
	int z;
	const Color rturn = (turn == BLACK) ? WHITE : BLACK;
	z = (turn == BLACK) ? SkipOverEMP(kingS, -dir) : SkipOverEMP(kingG, -dir);
	if (board[z] != WALL) {
		if ((turn == BLACK && (board[z] & GOTE) == 0)
		 || (turn == WHITE && (board[z] & GOTE) != 0)) {
			effect_t eft = (turn == BLACK) ? EFFECT_KING_S(z) : EFFECT_KING_G(z);
			if (eft & (effect[rturn][z] >> EFFECT_LONG_SHIFT)) pin[z] = dir;
		}
	}
}
template<Color turn>
void Position::del_pin_info(const int dir) {
	int z;
	z = (turn == BLACK) ? SkipOverEMP(kingS, -dir) : SkipOverEMP(kingG, -dir);
	if (board[z] != WALL) {
		if ((turn == BLACK && (board[z] & GOTE) == 0)
		 || (turn == WHITE && (board[z] & GOTE) != 0)) {
			pin[z] = 0;
		}
	}
}

inline bool Position::operator != (const Position &a) const {
	return !(*this == a);
}

inline bool is_ok(const Position& pos, const Move m)
{
	if (is_ok(m) == false) return false;
	if (pos.piece_on(to_sq(m)) != capture_of(m)) return false;
	if (pos.side_to_move() != color_of(piece_of(m))) return false;
	if (pos.piece_on(to_sq(m)) == WALL) return false;

	if (is_drop(m) == false) {
		if (pos.piece_on(from_sq(m)) != piece_of(m)) return false;
	}

	return true;
}
#endif
#endif // #ifndef POSITION_H_INCLUDED
