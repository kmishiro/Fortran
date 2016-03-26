subroutine eval_WDLSearch(eval) ! 必勝読み（ゲーム終了盤面に入ってくる）

  use setVariables, only : &
       DiscsBLACK, DiscsWHITE, &
       CurrentColor

  implicit none

  real &
       eval

  call countDiscs ! 石の数を数える

  eval=(DiscsBLACK-DiscsWHITE)*CurrentColor ! 自石の数を求める

  ! 評価値を勝ち負け引き分けの3パターンにする（枝狩りが多く起こるようにする）
  if(eval > 0) then ! 勝ち
     eval= 1.
  else if(eval < 0) then ! 負け
     eval=-1.
  else ! 引き分け
     eval= 0.
  end if

  return
end subroutine eval_WDLSearch

!----------------------------------------------------------------

subroutine eval_perfectSearch(eval) ! 完全読み（ゲーム終了盤面に入ってくる）

  use setVariables, only : &
       DiscsBLACK, DiscsWHITE, &
       CurrentColor

  implicit none

  real &
       eval

  call countDiscs ! 石の数を数える

  eval=(DiscsBLACK-DiscsWHITE)*CurrentColor ! 単純に自石の数を評価値とする

  return
end subroutine eval_perfectSearch

!----------------------------------------------------------------

subroutine eval_middleSearch(eval) ! 中盤の探索

  use setParameters, only : &
       BOARD_SIZE, &
       BOARD_BGN, BOARD_END, &
       BLACK, WHITE, EMPTY, &
       INFINITY
  use setVariables, only : &
       Turns, &
       CurrentColor, &
       DiscsBlack, DiscsWhite
  use setPatternEvalVarias, only : &
       HorizEval, &
       DiagoEval, &
       CorneEval
  use setIndexVarias, only : &
       DiffStableStone, &
       DiffMovableDisc
  use userSet, only : &
       WEIGHT_MOVABLEDISC, &
       WEIGHT_STABLESTONE

  implicit none

  integer, parameter:: &
       numHoriz=4, &  ! 水平パターンの回転（数）
       numDiago1=2, & ! 斜めパターン1の回転（数）
       numDiago=4, &  ! 斜めパターンの回転（数）
       numCorne=4     ! コーナーパターンの回転（数）
  real &
       eval, & ! 最終的な評価値
       allHorizEval, & ! 水平パターンの評価値合計
       allDiagoEval, & ! 斜めパターンの評価値合計
       allCorneEval, & ! コーナーパターンの評価値合計
       allDiffStableStone, & ! 確定石の差（黒 - 白）
       allDiffMovableDisc    ! 着手可能手数の差（黒 - 白）
  integer &
       n, &
       stage
  integer, dimension(numHoriz):: &
       horiz1PLine, & ! 水平パターン1のインデックス
       horiz2PLine, & ! 水平パターン2のインデックス
       horiz3PLine, & ! 水平パターン3のインデックス
       horiz4PLine    ! 水平パターン4のインデックス
  integer, dimension(numDiago1):: &
       diago1PLine ! 斜めパターン1のインデックス
  integer, dimension(numDiago):: &
       diago2PLine, & ! 斜めパターン1のインデックス
       diago3PLine, & ! 斜めパターン2のインデックス
       diago4PLine, & ! 斜めパターン3のインデックス
       diago5PLine    ! 斜めパターン4のインデックス
  integer, dimension(numCorne):: &
       cornerPLine ! コーナーパターンのインデックス

  eval=0. ! 評価値の初期化

  ! 自分の全滅を最低評価にし、相手の全滅を最高評価にする
  call countDiscs ! 石（各色）を数える

  if(DiscsBlack.eq.0) then
     if(CurrentColor.eq.BLACK) eval=-INFINITY+1
     if(CurrentColor.eq.WHITE) eval= INFINITY-1
     return
  end if
  if(DiscsWhite.eq.0) then
     if(CurrentColor.eq.BLACK) eval= INFINITY-1
     if(CurrentColor.eq.WHITE) eval=-INFINITY+1
     return
  end if

  ! パターン評価のためのインデックスを取得
  call getHorizontal1Index(horiz1PLine) ! 水平パターン1
  call getHorizontal2Index(horiz2PLine) ! 水平パターン2
  call getHorizontal3Index(horiz3PLine) ! 水平パターン3
  call getHorizontal4Index(horiz4PLine) ! 水平パターン4
  call getDiagonal1Index(diago1PLine)   ! 斜めパターン1
  call getDiagonal2Index(diago2PLine)   ! 斜めパターン2
  call getDiagonal3Index(diago3PLine)   ! 斜めパターン3
  call getDiagonal4Index(diago4PLine)   ! 斜めパターン4
  call getDiagonal5Index(diago5PLine)   ! 斜めパターン5
  call getCornerIndex(cornerPLine)      ! コーナーパターン

  stage=int((Turns+3)*0.2) ! 現在どのフェーズにいるかを計算
  
  allHorizEval=0. ! 水平パターンの評価値を計算
  do n=1, numHoriz ! 水平パターンの評価値を全て足し合わせる
     allHorizEval=allHorizEval &
          +HorizEval(horiz1PLine(n), stage, 1) &
          +HorizEval(horiz2PLine(n), stage, 2) &
          +HorizEval(horiz3PLine(n), stage, 3) &
          +HorizEval(horiz4PLine(n), stage, 4)
  end do
  allHorizEval=allHorizEval*0.0625 ! 最大値を10とするよう規格化 (1/16)

  allDiagoEval=0. ! 斜めパターンの評価値を計算
  do n=1, numDiago1 ! 斜めパターン1の評価値を全て足し合わせる
     allDiagoEval=allDiagoEval &
          +DiagoEval(diago1PLine(n), stage, 1)
  end do
  do n=1, numDiago ! 斜めパターン2-5の評価値を全て足し合わせる
     allDiagoEval=allDiagoEval &
          +DiagoEval(diago2PLine(n), stage, 2) &
          +DiagoEval(diago3PLine(n), stage, 3) &
          +DiagoEval(diago4PLine(n), stage, 4) &
          +DiagoEval(diago5PLine(n), stage, 5)
  end do
  allDiagoEval=allDiagoEval*0.0555556 ! 最大値を10とするよう規格化 (1/18)

  allCorneEval=0. ! コーナーパターンの評価値を計算
  do n=1, numCorne ! コーナーパターンの評価値を全て足し合わせる
     allCorneEval=allCorneEval &
          +CorneEval(cornerPLine(n), stage)
  end do
  allCorneEval=allCorneEval*0.250 ! 最大値を10とするよう規格化 (1/4)

  ! 足し合わせ、最大値が10.0になるように規格化
  eval=(allHorizEval+allDiagoEval+allCorneEval)*0.666667 ! (2/3)


  ! 確定石の差を評価に加える
  allDiffStableStone=0.
  do n=1, numHoriz ! 4辺の確定石の差を計算
     allDiffStableStone=allDiffStableStone+DiffStableStone(horiz1PLine(n))
  end do
  eval=eval+WEIGHT_STABLESTONE*allDiffStableStone ! 確定石の重み係数をかけて評価値に足し合わせる


  ! 着手可能手数を評価に加える
  allDiffMovableDisc=0.
  do n=1, numHoriz ! 水平方向の着手可能手数の差を計算
     allDiffMovableDisc=allDiffMovableDisc &
          +DiffMovableDisc(horiz1PLine(n)) &
          +DiffMovableDisc(horiz2PLine(n)) &
          +DiffMovableDisc(horiz3PLine(n)) &
          +DiffMovableDisc(horiz4PLine(n))
  end do
  do n=1, numDiago1 ! 斜め方向の着手可能手数の差を計算（中央ライン）
     allDiffMovableDisc=allDiffMovableDisc &
          +DiffMovableDisc(diago1PLine(n))
  end do
  do n=1, numDiago ! 斜め方向の着手可能手数の差を計算
     allDiffMovableDisc=allDiffMovableDisc &
          +DiffMovableDisc(diago2PLine(n)+2187) & ! 最後1マスのEにBを入れて着手可能手数を修正している
          +DiffMovableDisc(diago3PLine(n)+2916) & ! 最後2マスのEにBを入れて着手可能手数を修正している
          +DiffMovableDisc(diago4PLine(n)+3159) & ! 最後3マスのEにBを入れて着手可能手数を修正している
          +DiffMovableDisc(diago5PLine(n)+3240)   ! 最後4マスのEにBを入れて着手可能手数を修正している
  end do
  eval=eval+WEIGHT_MOVABLEDISC*allDiffMovableDisc ! 着手可能手数の重み係数をかけて評価値に足し合わせる


  ! 現在の手番視点からの評価値に変換
  eval=eval*CurrentColor ! BLACK -> ×1 .or. WHITE -> ×-1

  return
end subroutine eval_middleSearch
