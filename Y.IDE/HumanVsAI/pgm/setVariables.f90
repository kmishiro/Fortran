module setVariables ! グローバル変数の設定

  use setParameters
  
  implicit none
  
  integer &
       Turns, &     ! ターン（手番）数
       CurrentColor ! 現在手番の色。主に、BLACKかWHITEが入る。
  
  integer,dimension(MAX_TURNS, BOARD_SIZE+2, BOARD_SIZE+2):: &
       MovableDir, &  ! 各手番における盤面の「置ける位置」を記憶する配列
       MemoryRawBoard ! 各手番における盤面を記憶する配列

  integer,dimension(MAX_TURNS):: &
       MemoryPutPlace, &  ! 置いた石の位置を記憶する配列
       MemoryCurrentColor ! そのターンが黒と白、どっちの手番だったかを記憶する配列

  integer,dimension(BOARD_SIZE+2,BOARD_SIZE+2):: &
       RawBoard ! 現在の盤面が格納される

  integer &
       DiscsBLACK, & ! 黒石の数
       DiscsWHITE, & ! 白石の数
       DiscsEMPTY    ! 空きマスの数
  
contains

  subroutine init ! グローバル変数の初期化サブルーチン

    use setParameters, only : &
         BOARD_SIZE, &
         BOARD_BGN, BOARD_END, &
         MAX_TURNS, &
         EMPTY, WHITE, BLACK, WALL

    implicit none

    integer &
         x, y

    ! ボードの周りに壁を作る
    do x=1, BOARD_SIZE+2
       RawBoard(x, 1)=WALL
       RawBoard(x, BOARD_SIZE+2)=WALL
    end do
    do y=1, BOARD_SIZE+2
       RawBoard(1, y)=WALL
       RawBoard(BOARD_SIZE+2, y)=WALL
    end do

    ! 壁内を空きマスにする
    do x=BOARD_BGN, BOARD_END
       do y=BOARD_BGN, BOARD_END
          RawBoard(x, y)=EMPTY
       end do
    end do

    ! 石の初期配置を設定
    RawBoard(5, 5)=WHITE
    RawBoard(6, 6)=WHITE
    RawBoard(5, 6)=BLACK
    RawBoard(6, 5)=BLACK

    call countDiscs ! 各色の石の数を数える

    ! その他の変数の初期化
    Turns=1 ! ターンを1にする
    CurrentColor=BLACK ! オセロのルールに則り、初手は黒
    MemoryRawBoard(1:MAX_TURNS, 1:BOARD_SIZE+2, 1:BOARD_SIZE+2)=0 ! 盤面初期化
    MemoryRawBoard(Turns, 1:BOARD_SIZE+2, 1:BOARD_SIZE+2)=RawBoard(1:BOARD_SIZE+2, 1:BOARD_SIZE+2)

    call initMovable(CurrentColor) ! 石を置ける場所があるかをチェックして、MovableDirを更新する

    return
  end subroutine init

end module setVariables

!----------------------------------------------------------------

module setTheBookVarias ! 定石変数の設定

  use setParameters, only : &
       MAX_TURNS
  use setTheBookParams, only : &
       THEBOOKWAY

  implicit none

  integer, dimension(THEBOOKWAY, MAX_TURNS-1):: &
       WayBook ! 定石の棋譜が各ターン毎に格納される
  real, dimension(THEBOOKWAY):: &
       EvalBook ! 定石の評価関数を格納

contains

  subroutine initTheBook ! 定石の配列代入
  
    implicit none

    integer &
         n

    open(03, file='dat/theBookBasedWzebra.dat') ! 定石の棋譜が保存されている

    do n=1, THEBOOKWAY
       read(03, '(60i3, f9.3)') WayBook(n, 1:MAX_TURNS-1), EvalBook(n) ! 棋譜と評価値を配列に保存
    end do

    close(03)

    return
  end subroutine initTheBook

end module setTheBookVarias

!----------------------------------------------------------------

module setIndexVarias ! インデックス変数の設定

  use setParameters, only : &
       BOARD_SIZE
  use setIndexParams, only : &
       INDEX_NUM

  implicit none

  integer, dimension(INDEX_NUM):: &
       Indx, &            ! インデックス
       DiffStableStone, & ! 確定石の黒と白の差（黒 - 白）
       DiffMovableDisc    ! 着手可能手数の黒と白の差（黒 - 白）
  integer, dimension(INDEX_NUM, 2, BOARD_SIZE):: &
       Movable, &          ! 着手可能かどうかの情報を黒と白、それぞれについて格納
       FlipLeft, FlipRight ! 着手時に右と左に何個石を返せるかの情報
  integer, dimension(INDEX_NUM, 2):: &
       StableStone, & ! 確定石の数を格納
       MovableDisc    ! 着手可能手数を格納

contains
  
  subroutine initIndex ! インデックス及びその他の情報を配列に格納

    integer &
         n, x

    open(04, file='dat/indexTable.dat') ! インデックスファイル

    read(04, *)
    read(04, *)
    read(04, *)
    do n=1, INDEX_NUM ! 配列番号=index+3281
       read(04, '(i4, 2(1x, 8i1, 8(2x, i1, 1x, i1, 1x)), 2(2i2, i3))') & ! インデックスの読み込み
            Indx(n), & ! インデックス
            (Movable(n, 1, x), x=1, BOARD_SIZE), &   ! 黒石について BEGIN -->
            FlipLeft(n, 1, 1), FlipRight(n, 1, 1), & ! ---
            FlipLeft(n, 1, 2), FlipRight(n, 1, 2), & ! ---
            FlipLeft(n, 1, 3), FlipRight(n, 1, 3), & ! ---
            FlipLeft(n, 1, 4), FlipRight(n, 1, 4), & ! ---
            FlipLeft(n, 1, 5), FlipRight(n, 1, 5), & ! ---
            FlipLeft(n, 1, 6), FlipRight(n, 1, 6), & ! ---
            FlipLeft(n, 1, 7), FlipRight(n, 1, 7), & ! ---
            FlipLeft(n, 1, 8), FlipRight(n, 1, 8), & ! <-- END
            (Movable(n, 2, x), x=1, BOARD_SIZE), &   ! 白石について BEGIN -->
            FlipLeft(n, 2, 1), FlipRight(n, 2, 1), & ! ---
            FlipLeft(n, 2, 2), FlipRight(n, 2, 2), & ! ---
            FlipLeft(n, 2, 3), FlipRight(n, 2, 3), & ! ---
            FlipLeft(n, 2, 4), FlipRight(n, 2, 4), & ! ---
            FlipLeft(n, 2, 5), FlipRight(n, 2, 5), & ! ---
            FlipLeft(n, 2, 6), FlipRight(n, 2, 6), & ! ---
            FlipLeft(n, 2, 7), FlipRight(n, 2, 7), & ! ---
            FlipLeft(n, 2, 8), FlipRight(n, 2, 8), & ! <-- END
            StableStone(n, 1), StableStone(n, 2), DiffStableStone(n), & ! 確定石（黒、白、差）
            MovableDisc(n, 1), MovableDisc(n, 2), DiffMovableDisc(n)    ! 着手可能手数（黒、白、差）
    end do

    close(04)
  end subroutine initIndex

end module setIndexVarias

!----------------------------------------------------------------

module setPatternEvalVarias ! 各パターンの評価値を設定

  use setParameters, only : &
       BOARD_SIZE
  use setIndexParams, only : &
       INDEX_NUM, &
       STAGE_NUM
  use userSet, only : &
       EVALUATION_FILE

  implicit none

  integer, dimension(INDEX_NUM):: &
       Indx ! インデックス
  real, dimension(INDEX_NUM, STAGE_NUM):: & ! 各パターン、各フェーズ毎に評価値を格納する配列を準備
       Horiz1Eval, Horiz2Eval, Horiz3Eval, Horiz4Eval, & ! 水平（鉛直）方向
       Diago1Eval, Diago2Eval, Diago3Eval, Diago4Eval, Diago5Eval, & ! 斜め方向
       CornerEval ! 隅付近
  real, dimension(INDEX_NUM, STAGE_NUM, 4):: &
       HorizEval ! 水平（鉛直）方向のまとめ
  real, dimension(INDEX_NUM, STAGE_NUM, 5):: &
       DiagoEval ! 斜め方向のまとめ
  real, dimension(INDEX_NUM, STAGE_NUM):: &
       CorneEval ! 隅付近のまとめ

contains
  
  subroutine initPatternEval ! パターンの評価値を配列に格納

    integer &
         n, i

    ! 各パターンの評価値が記録されているファイルを開く
    open(21, file='dat/patternEval/horiz1Eval_'//trim(EVALUATION_FILE)//'.dat')
    open(22, file='dat/patternEval/horiz2Eval_'//trim(EVALUATION_FILE)//'.dat')
    open(23, file='dat/patternEval/horiz3Eval_'//trim(EVALUATION_FILE)//'.dat')
    open(24, file='dat/patternEval/horiz4Eval_'//trim(EVALUATION_FILE)//'.dat')
    open(31, file='dat/patternEval/diago1Eval_'//trim(EVALUATION_FILE)//'.dat')
    open(32, file='dat/patternEval/diago2Eval_'//trim(EVALUATION_FILE)//'.dat')
    open(33, file='dat/patternEval/diago3Eval_'//trim(EVALUATION_FILE)//'.dat')
    open(34, file='dat/patternEval/diago4Eval_'//trim(EVALUATION_FILE)//'.dat')
    open(35, file='dat/patternEval/diago5Eval_'//trim(EVALUATION_FILE)//'.dat')
    open(41, file='dat/patternEval/cornerEval_'//trim(EVALUATION_FILE)//'.dat')

    ! 配列に代入
    do n=1, 3
       read(21, *)
       read(22, *)
       read(23, *)
       read(24, *)
       read(31, *)
       read(32, *)
       read(33, *)
       read(34, *)
       read(35, *)
       read(41, *)
    end do
    do n=1, INDEX_NUM ! 配列番号=index+3281
       read(21, '(i4, 12f7.3)') Indx(n), (Horiz1Eval(n, i), i=1, STAGE_NUM)
       read(22, '(i4, 12f7.3)') Indx(n), (Horiz2Eval(n, i), i=1, STAGE_NUM)
       read(23, '(i4, 12f7.3)') Indx(n), (Horiz3Eval(n, i), i=1, STAGE_NUM)
       read(24, '(i4, 12f7.3)') Indx(n), (Horiz4Eval(n, i), i=1, STAGE_NUM)
       read(31, '(i4, 12f7.3)') Indx(n), (Diago1Eval(n, i), i=1, STAGE_NUM)
       read(32, '(i4, 12f7.3)') Indx(n), (Diago2Eval(n, i), i=1, STAGE_NUM)
       read(33, '(i4, 12f7.3)') Indx(n), (Diago3Eval(n, i), i=1, STAGE_NUM)
       read(34, '(i4, 12f7.3)') Indx(n), (Diago4Eval(n, i), i=1, STAGE_NUM)
       read(35, '(i4, 12f7.3)') Indx(n), (Diago5Eval(n, i), i=1, STAGE_NUM)
       read(41, '(i4, 12f7.3)') Indx(n), (CornerEval(n, i), i=1, STAGE_NUM)
    end do

    close(21)
    close(22)
    close(23)
    close(24)
    close(31)
    close(32)
    close(33)
    close(34)
    close(35)
    close(41)

    ! 水平（鉛直）方向・斜め方向・隅付近の3つにまとめる
    HorizEval(1:INDEX_NUM, 1:STAGE_NUM, 1)=Horiz1Eval(1:INDEX_NUM, 1:STAGE_NUM)
    HorizEval(1:INDEX_NUM, 1:STAGE_NUM, 2)=Horiz2Eval(1:INDEX_NUM, 1:STAGE_NUM)
    HorizEval(1:INDEX_NUM, 1:STAGE_NUM, 3)=Horiz3Eval(1:INDEX_NUM, 1:STAGE_NUM)
    HorizEval(1:INDEX_NUM, 1:STAGE_NUM, 4)=Horiz4Eval(1:INDEX_NUM, 1:STAGE_NUM)
    DiagoEval(1:INDEX_NUM, 1:STAGE_NUM, 1)=Diago1Eval(1:INDEX_NUM, 1:STAGE_NUM)
    DiagoEval(1:INDEX_NUM, 1:STAGE_NUM, 2)=Diago2Eval(1:INDEX_NUM, 1:STAGE_NUM)
    DiagoEval(1:INDEX_NUM, 1:STAGE_NUM, 3)=Diago3Eval(1:INDEX_NUM, 1:STAGE_NUM)
    DiagoEval(1:INDEX_NUM, 1:STAGE_NUM, 4)=Diago4Eval(1:INDEX_NUM, 1:STAGE_NUM)
    DiagoEval(1:INDEX_NUM, 1:STAGE_NUM, 5)=Diago5Eval(1:INDEX_NUM, 1:STAGE_NUM)
    CorneEval(1:INDEX_NUM, 1:STAGE_NUM)   =CornerEval(1:INDEX_NUM, 1:STAGE_NUM)

    return
  end subroutine initPatternEval

end module setPatternEvalVarias
