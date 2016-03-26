subroutine ai(switchPhase, in, str_eval, phase) ! 人工知能(AI)の思考を制御する

  use setParameters, only : &
       MAX_TURNS, &
       OPENING_MODE, &
       MIDDLE_MODE, &
       WDL_MODE, &
       PERFECT_MODE
  use userSet, only : & ! ユーザ設定
       NORMAL_DEPTH, &  ! 中盤読みの探索深度
       WDL_DEPTH, &     ! 必勝読みの探索深度
       PERFECT_DEPTH, & ! 完全読みの探索深度
       SORT_DEPTH       ! ソートの探索深度
  use setVariables, only : &
       Turns

  implicit none

  real &
       eval
  character(len=2) &
       in
  character(len=7) &
       phase
  character(len=8) &
       str_eval
  integer &
       switchPhase, &
       limit, sortLimit

  if(switchPhase == OPENING_MODE) then ! 序盤(Opening)
     call theBook(in, switchPhase, eval) ! 定石を選ぶサブルーチンを呼び出す
     phase='Opening'
     write(str_eval, '(f8.3)') eval
  else ! フェーズの移行
     if(MAX_TURNS-Turns <= PERFECT_DEPTH) then ! 完全読みに移行
        switchPhase=PERFECT_MODE
     else if(MAX_TURNS-Turns <= WDL_DEPTH) then ! 必勝読みに移行
        switchPhase=WDL_MODE
     else ! 中盤読みに移行
        switchPhase=MIDDLE_MODE
     end if
  end if

  if(switchPhase == MIDDLE_MODE) then ! 中盤(Middle)
     limit=NORMAL_DEPTH   ! 中盤の探索深度を代入
     sortLimit=SORT_DEPTH ! ソート探索深度の代入
     call treeSearch(in, switchPhase, eval, limit, sortLimit) ! ゲーム木探索を開始
     phase='Middle'
     write(str_eval, '(f8.3)') eval
  end if

  if(switchPhase == WDL_MODE) then ! 必勝読み(WDL)
     limit=MAX_TURNS ! 探索深度はゲーム終了時まで
     sortLimit=0     ! ソートはしない
     call treeSearch(in, switchPhase, eval, limit, sortLimit) ! ゲーム木探索を開始
     if(eval == 1.) then ! 必ず勝つ手が見つかった
        write(str_eval, '(a)') '     WIN'
     else if(eval == 0.) then ! 共に最善手で進行したら引き分ける
        write(str_eval, '(a)') '    DRAW'
     else ! 共に最善手で進行したら負けることが分かったので、中盤探索をやり直す
        switchPhase=MIDDLE_MODE ! フェーズを中盤に戻す
        limit=NORMAL_DEPTH      ! 探索深度も中盤に戻す
        sortLimit=SORT_DEPTH    ! ソート探索深度も中盤に戻す
        call treeSearch(in, switchPhase, eval, limit, sortLimit) ! ゲーム木探索を開始
        write(str_eval, '(a)') '    LOSE'
     end if
     phase='WDL'
  end if

  if(switchPhase == PERFECT_MODE) then ! 完全読み(Perfect)
     limit=MAX_TURNS ! 探索深度はゲーム終了時まで
     sortLimit=0     ! ソートはしない
     call treeSearch(in, switchPhase, eval, limit, sortLimit) ! ゲーム木探索を開始
     phase='Perfect'
     write(str_eval, '(f8.3)') eval
  end if

  return
end subroutine ai

!----------------------------------------------------------------

subroutine treeSearch(in, switchPhase, evalMax, limit, sortLimit) ! ゲーム木探索

  use setParameters, only : &
       NON, &
       INFINITY, &
       BOARD_SIZE, &
       BOARD_BGN, BOARD_END, &
       MAX_TURNS
  use setVariables, only : &
       MovableDir, &
       Turns
  
  implicit none

  integer &
       switchPhase
  character(len=2) &
       in
  integer &
       x, y
  integer, dimension(MAX_TURNS-1):: &
       xp, yp ! 打てる手の座標が入る
  logical, allocatable:: &
       mask(:) ! ソート用のマスク配列
  integer, allocatable:: &
       xpTmp(:), ypTmp(:) ! ソート用の仮座標(x, y)
  real, allocatable:: &
       sortEval(:) ! ソート用の仮評価値
  integer &
       n, nm, &
       limit, sortLimit, &
       nSort(1)
  real &
       eval, evalMax, &
       alpha, beta ! アルファ・ベータ探索用のαとβ

  ! 初期化
  xp(1:MAX_TURNS-1)=0
  yp(1:MAX_TURNS-1)=0

  ! すべての置ける位置を生成
  n=0
  do y=BOARD_BGN, BOARD_END
     do x=BOARD_BGN, BOARD_END
        if(MovableDir(Turns, x, y) /= NON) then ! その(x, y)に打てるとき中に入る
           n=n+1
           xp(n)=x
           yp(n)=y
        end if
     end do
  end do
  nm=n
    
  ! 置ける場所が無いときはエラー(置けない状態でこのサブルーチンに入って来ない)
  if(nm == 0) then
     write(06, '(a)') 'program bug:: s:AI nm=0'
     stop
  end if

  ! 本格的に探索を開始する前に浅い探索を行い評価値が高そうな順に並べ換える（ソート）
  if(sortLimit /= 0) then ! ソート探索深度が0のときはソート無し
     if(switchPhase /= 2) then ! AIフェーズが中盤のときのみソート機能を使用する
        write(06, '(a)') 'program bug:: s:AI sort'
        write(06, *) switchPhase
        stop
     end if

     ! ソート用の配列の大きさを決定、打てる手の数になる
     allocate(mask(nm), sortEval(nm), xpTmp(nm), ypTmp(nm))
     mask(1:nm)=.true. ! マスクには始めすべて「真」を入れておく

     ! 仮探索で評価値を取得
     evalMax=-INFINITY ! 評価値の最大値を初期化
     do n=1, nm

        ! 石を打つ（AI用、mainの一般用から余分な操作を取り除き、インデックスを利用し石を裏返している）
        call AImove(xp(n), yp(n)) 

        ! αとβを初期化
        alpha=-INFINITY
        beta=INFINITY
        call maxLevel(eval, switchPhase, sortLimit-1, alpha, beta) ! ネガマックス+アルファ・ベータ法を組み合わせた

        eval=-eval ! 符号の反転（ネガマックス法）

        ! 一手戻す（AI用）、mainの一般用では前回の自分の番に戻していたが、ここは一手戻す操作であることに注意
        call AIundo

        sortEval(n)=eval ! 評価値を保存しておく
     end do

     do n=1, nm ! 評価値の高い順に並び替え（ソート）

        nSort=maxloc(sortEval(1:nm), mask(1:nm)) ! maskが「真」の位置だけを対象に最大値を取得

        xpTmp(n)=xp(nSort(1)) ! x座標の一時保存
        ypTmp(n)=yp(nSort(1)) ! y座標の一時保存

        mask(nSort(1))=.false. ! maskを「偽」にして、今選んだ座標は次の最大値検索から除外する

     end do
     xp(1:nm)=xpTmp(1:nm) ! 一時保存から正式なx座標配列に受け渡し
     yp(1:nm)=ypTmp(1:nm) ! 一時保存から正式なy座標配列に受け渡し

     deallocate(mask, sortEval, xpTmp, ypTmp) ! メモリの解放
  end if


  ! すべての手を打っていく(AIの1手目←この手を最終的に決定する)
  evalMax=-INFINITY ! 評価値の最大値を初期化
  do n=1, nm

     ! 石を打つ（AI用、mainの一般用から余分な操作を取り除き、インデックスを利用し石を裏返している）
     call AImove(xp(n), yp(n))

     ! αとβを初期化
     alpha=-INFINITY
     beta=INFINITY
     call maxLevel(eval, switchPhase, limit-1, alpha, beta) ! ネガマックス+アルファ・ベータ法を組み合わせた

     eval=-eval ! 符号の反転(ネガマックス法)

     ! 一手戻す（AI用）、mainの一般用では前回の自分の番に戻していたが、ここは一手戻す操作であることに注意
     call AIundo

     if(eval > evalMax) then ! 最終的に一番評価値が高い手が選択される
        evalMax=eval
        x=xp(n)
        y=yp(n)
     end if

  end do

  call writeIn(x, y, in) ! 打つ手の座標を数値から文字に変換

  return
end subroutine treeSearch

!----------------------------------------------------------------

recursive subroutine maxLevel(evalMax, switchPhase, limit, alpha, beta) ! ネガマックス法+アルファ・ベータ法

  use setParameters, only : &
       ON, OFF, &
       NON, &
       EMPTY, &
       INFINITY, &
       BOARD_SIZE, &
       BOARD_BGN, BOARD_END, &
       OPENING_MODE, &
       MIDDLE_MODE, &
       WDL_MODE, &
       PERFECT_MODE, &
       MAX_TURNS
  use setVariables, only : &
       Turns, &
       MovableDir, &
       MemoryRawBoard

  implicit none
  
  integer &
       gameContinue, &
       switchPhase
  integer &
       x, y
  integer, dimension(MAX_TURNS-1):: &
       xp, yp
  integer &
       n, nm, &
       limit
  real &
       eval, evalMax, &
       alpha, beta
  
  evalMax=-INFINITY ! 評価値の最大値を初期化

  gameContinue=ON ! ゲームが続行可能か終了かを判別のために導入

  call AIisGameOver(gameContinue) ! ゲーム終了かを判定
  if(gameContinue == OFF .or. limit == 0) then ! ゲームが終了、もしくは深さ上限に達したら評価する

     ! AIフェーズによって使用する評価関数の切り替え
     if(switchPhase == MIDDLE_MODE) then

        call eval_middleSearch(evalMax) ! 中盤の評価関数

     else if(switchPhase == WDL_MODE) then

        call eval_WDLSearch(evalMax) ! 終盤必勝読みの評価関数

     else if(switchPhase == PERFECT_MODE) then

        call eval_perfectSearch(evalMax) ! 終盤完全読みの評価関数

     else
        write(06, '(a)') 'Bad switchPhase.'
        stop
     end if

     return
  end if
  
  ! すべての置ける位置を生成
  n=0
  do y=BOARD_BGN, BOARD_END
     do x=BOARD_BGN, BOARD_END
        if(MovableDir(Turns, x, y) /= NON) then
           n=n+1
           xp(n)=x
           yp(n)=y
        end if
     end do
  end do
  nm=n

  ! パス
  if(nm == 0) then

     call AIpass ! パス（AI用、mainの一般用から余分な操作を除去した）
     
     call maxLevel(evalMax, switchPhase, limit, -beta, -alpha) ! 再帰で自身のサブルーチンに戻る

     evalMax=-evalMax ! 符号の反転（ネガマックス法）

     MemoryRawBoard(Turns, :, :)=EMPTY ! その手の記憶盤面を初期化（入らないかも...）

     return
  end if

  ! すべての手を打っていく
  do n=1, nm

     ! 石を打つ（AI用、mainの一般用から余分な操作を取り除き、インデックスを利用し石を裏返している）
     call AImove(xp(n), yp(n))

     call maxLevel(eval, switchPhase, limit-1, -beta, -alpha) ! 再帰で自身のサブルーチンに戻る

     ! 一手戻す（AI用）、mainの一般用では前回の自分の番に戻していたが、ここは一手戻す操作であることに注意
     call AIundo

     eval=-eval! 符号の反転（ネガマックス法）

     if(eval >= beta) then ! β値を上回ったら探索終了（アルファ・ベータ法）
        evalMax=eval
        return
     end if

     if(eval > evalMax) then ! より良い手が見つかった
        evalMax=eval
        alpha=max(alpha, evalMax) ! α値の更新（アルファ・ベータ法）
     end if

  end do
  
  return
end subroutine maxLevel

!----------------------------------------------------------------

subroutine AImove(x, y) ! 1手打つ（AI用）

  use setParameters, only : &
       NON, &
       ON, OFF, &
       BOARD_BGN, BOARD_END
  use setVariables, only : &
       Turns, &
       MemoryCurrentColor, &
       MemoryRawBoard, &
       CurrentColor, &
       RawBoard

  implicit none

  integer &
       x, y

  MemoryCurrentColor(Turns)=CurrentColor ! 現在の手番を保存
  ! 現在のボード情報を保存
  MemoryRawBoard(Turns, BOARD_BGN:BOARD_END, BOARD_BGN:BOARD_END)=RawBoard(BOARD_BGN:BOARD_END, BOARD_BGN:BOARD_END)

  call AIflipDiscs(x, y) ! 実際に石を裏返す（AI用）

  Turns=Turns+1 ! 石を置き裏返したら、ターンを進める

  CurrentColor=-CurrentColor ! 手番交代

  call initMovable(CurrentColor) ! MovableDir（置けるマスの情報）を更新

  return
end subroutine AImove

!----------------------------------------------------------------

subroutine AIundo ! 取り消し（AI用）
  use setParameters, only : &
       EMPTY, &
       BOARD_BGN, BOARD_END
  use setVariables, only : &
       Turns, &
       CurrentColor, &
       MemoryCurrentColor, &
       RawBoard, &
       MemoryRawBoard

  implicit none

  Turns=Turns-1 ! ターンを戻す
  
  CurrentColor=MemoryCurrentColor(Turns) ! 1手前のターンの色に戻す
  ! 1手前の盤面に戻す
  RawBoard(BOARD_BGN:BOARD_END, BOARD_BGN:BOARD_END)=MemoryRawBoard(Turns, BOARD_BGN:BOARD_END, BOARD_BGN:BOARD_END)
  
  MemoryCurrentColor(Turns+1)=EMPTY ! 手番の色を初期化
  MemoryRawBoard(Turns+1, BOARD_BGN:BOARD_END, BOARD_BGN:BOARD_END)=EMPTY ! 盤面を初期化
  
  return
end subroutine AIundo

!----------------------------------------------------------------

subroutine AIisGameOver(gameContinue) ! ゲーム終了か続行かを判断（AI用）

  use setParameters, only : &
       ON, OFF, &
       MAX_TURNS, &
       BOARD_BGN, BOARD_END
  use setVariables, only : &
       Turns, &
       MovableDir, &
       CurrentColor

  implicit none

  integer &
       gameContinue
  integer &
       x, y

  if(Turns == MAX_TURNS) then ! 最後のターンはゲーム終了
     gameContinue=OFF
     return
  end if

  do y=BOARD_BGN, BOARD_END
     do x=BOARD_BGN, BOARD_END
        if(MovableDir(Turns, x, y) /= 0) then ! 自石の置ける場所があれば、ゲーム続行
           gameContinue=ON
           return
        end if
     end do
  end do

  call initMovable(-CurrentColor) ! 相手の置ける場所の情報にする

  do y=BOARD_BGN, BOARD_END
     do x=BOARD_BGN, BOARD_END
        if(MovableDir(Turns, x, y) /= 0) then ! 自石が置けなくとも相手の石が置ければゲーム続行
           gameContinue=ON
           call initMovable(CurrentColor) ! 置けるマス情報を自分に戻しておく
           return
        end if
     end do
  end do

  gameContinue=OFF ! ここまでくればどちらもおける場所がないということ、ゲーム終了

  return
end subroutine AIisGameOver

!----------------------------------------------------------------

subroutine AIpass ! パスする（AI用）

  use setParameters, only : &
       ON, OFF, &
       BOARD_BGN, BOARD_END, &
       NON
  use setVariables, only : &
       CurrentColor

  implicit none

  call initMovable(CurrentColor) ! MovableDir（置けるマスの情報）を更新

  CurrentColor=-CurrentColor ! パスによって手番交代

  call initMovable(CurrentColor) ! MovableDir（置けるマスの情報）を更新

  return
end subroutine AIpass

!----------------------------------------------------------------

subroutine AIflipDiscs(x, y) ! 石を置き裏返す（AI用）

  use setParameters, only : &
       BOARD_BGN, BOARD_END, &
       BOARD_SIZE, &
       BLACK
  use setVariables, only : &
       RawBoard, &
       CurrentColor
  use setIndexVarias, only : &
       FlipLeft, FlipRight ! 石を置いたとき左右（上下、両斜め）に何個返せるかが入る
       
  
  implicit none

  integer &
       x, y, xv, &
       xBlack, xWhite, & ! 置いたマスは黒（白）ライン上の何番目に位置するかを格納
       nColor, & ! BLACK or WHITE（黒か白かで返せる石数が違うため）
       localHorizLine, localVertiLine, & ! 水平（鉛直）ラインのインデックス
       localBlackLine, localWhiteLine, & ! 斜め（黒もしくは白）ラインのインデックス
       blackLineNumber, whiteLineNumber ! 両斜めのパターンがP?にあたるかを格納する変数

  call getLocalIndex(x, y, & ! 石を置いたマスの縦横斜めラインのインデックスを取得する
       localHorizLine, localVertiLine, localBlackLine, localWhiteLine, &
       blackLineNumber, whiteLineNumber)

  if(CurrentColor == BLACK) then ! 黒だったらnColor=1、白だったらnColor=2
     nColor=1
  else
     nColor=2
  end if

  ! そのマスに石を置く
  RawBoard(x, y)=CurrentColor

  ! 水平ラインについて、左右の石を裏返す
  RawBoard(x-FlipLeft(localHorizLine, nColor, x-1):x+FlipRight(localHorizLine, nColor, x-1), y)=CurrentColor

  ! 鉛直ラインについて、上下の石を裏返す
  RawBoard(x, y-FlipLeft(localVertiLine, nColor, y-1):y+FlipRight(localVertiLine, nColor, y-1))=CurrentColor

  ! 黒ラインについて、斜めの石を裏返す
  if(localBlackLine /= 3281) then ! インデックス=3281-1=3280はEEEEEEEEであり、返せる石はない（コーナー2マスのため）
     if(blackLineNumber <= 6) then
        xBlack=y-1 ! 対象ラインがP6以下だった場合は、置いたマスのライン上での位置はy-1となる
     else
        xBlack=10-x ! 対象ラインがP7以上だった場合は、置いたマスのライン上での位置は10-xとなる
     end if
     if(FlipLeft(localBlackLine, nColor, xBlack) /= 0) then ! 右上方向の石を裏返す
        do xv=1, FlipLeft(localBlackLine, nColor, xBlack)
           RawBoard(x+xv, y-xv)=CurrentColor
        end do
     end if
     if(FlipRight(localBlackLine, nColor, xBlack) /= 0) then ! 左下方向の石を裏返す
        do xv=1, FlipRight(localBlackLine, nColor, xBlack)
           RawBoard(x-xv, y+xv)=CurrentColor
        end do
     end if
  end if

  ! 白ラインについて、斜めの石を裏返す
  if(localWhiteLine /= 3281) then ! インデックス=3281-1=3280はEEEEEEEEであり、返せる石はない（コーナー2マスのため）
     if(whiteLineNumber <= 6) then
        xWhite=10-x ! 対象ラインがP6以下だった場合は、置いたマスのライン上での位置は10-xとなる
     else
        xWhite=10-y  ! 対象ラインがP7以上だった場合は、置いたマスのライン上での位置は10-yとなる
     end if
     if(FlipLeft(localWhiteLine, nColor, xWhite) /= 0) then ! 右下方向の石を裏返す
        do xv=1, FlipLeft(localWhiteLine, nColor, xWhite)
           RawBoard(x+xv, y+xv)=CurrentColor
        end do
     end if
     if(FlipRight(localWhiteLine, nColor, xWhite) /= 0) then ! 左上方向の石を裏返す
        do xv=1, FlipRight(localWhiteLine, nColor, xWhite)
           RawBoard(x-xv, y-xv)=CurrentColor
        end do
     end if
  end if

  return
end subroutine AIflipDiscs

!----------------------------------------------------------------

subroutine getLocalIndex(x, y, & ! 石を置いたマスの縦横斜めラインについてのインデックスを取得
     localHorizLine, localVertiLine, localBlackLine, localWhiteLine, &
     blackLineNumber, whiteLineNumber)

  use setParameters, only : &
       BOARD_BGN, BOARD_END, &
       BOARD_SIZE
  use setVariables, only : &
       RawBoard

  implicit none

  integer &
       x, y, & ! 座標
       index, & ! インデックス
       blackLineNumber, whiteLineNumber ! 両斜めのパターンがP?にあたるかを格納する変数
  integer &
       localHorizLine, & ! 水平ラインのインデックス
       localVertiLine, & ! 鉛直ラインのインデックス
       localBlackLine, & ! 黒ラインのインデックス
       localWhiteLine    ! 白ラインのインデックス

  ! 水平ラインのインデックスを取得
  index= &
       3*(3*(3*(3*(3*(3*(3*RawBoard(BOARD_END, y) &
       +RawBoard(BOARD_END-1, y)) &
       +RawBoard(BOARD_END-2, y)) &
       +RawBoard(BOARD_END-3, y)) &
       +RawBoard(BOARD_BGN+3, y)) &
       +RawBoard(BOARD_BGN+2, y)) &
       +RawBoard(BOARD_BGN+1, y)) &
       +RawBoard(BOARD_BGN, y)
  localHorizLine=index+3281 ! 配列要素番号に変換している
                            ! インデックスは0000からなのに対して、配列は1からであることに留意

  ! 鉛直ラインのインデックスを取得
  index= &
       3*(3*(3*(3*(3*(3*(3*RawBoard(x, BOARD_END) &
       +RawBoard(x, BOARD_END-1)) &
       +RawBoard(x, BOARD_END-2)) &
       +RawBoard(x, BOARD_END-3)) &
       +RawBoard(x, BOARD_BGN+3)) &
       +RawBoard(x, BOARD_BGN+2)) &
       +RawBoard(x, BOARD_BGN+1)) &
       +RawBoard(x, BOARD_BGN)
  localVertiLine=index+3281 ! 配列要素番号に変換している

  ! 黒ラインのインデックスを取得
  blackLineNumber=x+y-5 ! ライン番号(P?)を取得
  select case (blackLineNumber) ! ライン番号に応じてマス数が変わるため場合分け
  case(1) ! P1ライン
     index= &
          3*(3*RawBoard(BOARD_BGN, BOARD_BGN+2) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+1)) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN)
     localBlackLine=index+3281
  case(2) ! P2ライン
     index= &
          3*(3*(3*RawBoard(BOARD_BGN, BOARD_BGN+3) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+2)) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+1)) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN)
     localBlackLine=index+3281
  case(3) ! P3ライン
     index= &
          3*(3*(3*(3*RawBoard(BOARD_BGN, BOARD_BGN+4) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+3)) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+2)) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+1)) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN)
     localBlackLine=index+3281
  case(4) ! P4ライン
     index= &
          3*(3*(3*(3*(3*RawBoard(BOARD_BGN, BOARD_BGN+5) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+4)) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+3)) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+2)) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+1)) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN)
     localBlackLine=index+3281
  case(5) ! P5ライン
     index= &
          3*(3*(3*(3*(3*(3*RawBoard(BOARD_BGN, BOARD_BGN+6) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+5)) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+4)) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+3)) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+2)) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+1)) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN)
     localBlackLine=index+3281
  case(6) ! P6ライン
     index= &
          3*(3*(3*(3*(3*(3*(3*RawBoard(BOARD_BGN, BOARD_END) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+6)) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+5)) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+4)) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+3)) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+2)) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+1)) &
          +RawBoard(BOARD_END, BOARD_BGN)
     localBlackLine=index+3281
  case(7) ! P7ライン
     index= &
          3*(3*(3*(3*(3*(3*RawBoard(BOARD_BGN+1, BOARD_END) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+6)) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+5)) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+4)) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+3)) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+2)) &
          +RawBoard(BOARD_END, BOARD_BGN+1)
     localBlackLine=index+3281
  case(8) ! P8ライン
     index= &
          3*(3*(3*(3*(3*RawBoard(BOARD_BGN+2, BOARD_END) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+6)) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+5)) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+4)) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+3)) &
          +RawBoard(BOARD_END, BOARD_BGN+2)
     localBlackLine=index+3281
  case(9) ! P9ライン
     index= &
          3*(3*(3*(3*RawBoard(BOARD_BGN+3, BOARD_END) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+6)) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+5)) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+4)) &
          +RawBoard(BOARD_END, BOARD_BGN+3)
     localBlackLine=index+3281
  case(10) ! P10ライン
     index= &
          3*(3*(3*RawBoard(BOARD_BGN+4, BOARD_END) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+6)) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+5)) &
          +RawBoard(BOARD_END, BOARD_BGN+4)
     localBlackLine=index+3281
  case(11) ! P11ライン
     index= &
          3*(3*RawBoard(BOARD_BGN+5, BOARD_END) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+6)) &
          +RawBoard(BOARD_END, BOARD_BGN+5)
     localBlackLine=index+3281
  case default ! ここはコーナー2マスのとき入ってくる
     localBlackLine=3281 ! EEEEEEEEであることにしておく
  end select

  ! 白ラインのインデックスを取得
  whiteLineNumber=6-x+y ! ライン番号(P?)を取得
  select case (whiteLineNumber) ! ライン番号に応じてマス数が変わるため場合分け
  case(1) ! P1ライン
     index= &
          3*(3*RawBoard(BOARD_BGN+5, BOARD_BGN) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+1)) &
          +RawBoard(BOARD_END, BOARD_BGN+2)
     localWhiteLine=index+3281
  case(2) ! P2ライン
     index= &
          3*(3*(3*RawBoard(BOARD_BGN+4, BOARD_BGN) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+1)) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+2)) &
          +RawBoard(BOARD_END, BOARD_BGN+3)
     localWhiteLine=index+3281
  case(3) ! P3ライン
     index= &
          3*(3*(3*(3*RawBoard(BOARD_BGN+3, BOARD_BGN) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+1)) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+2)) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+3)) &
          +RawBoard(BOARD_END, BOARD_BGN+4)
     localWhiteLine=index+3281
  case(4) ! P4ライン
     index= &
          3*(3*(3*(3*(3*RawBoard(BOARD_BGN+2, BOARD_BGN) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+1)) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+2)) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+3)) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+4)) &
          +RawBoard(BOARD_END, BOARD_BGN+5)
     localWhiteLine=index+3281
  case(5) ! P5ライン
     index= &
          3*(3*(3*(3*(3*(3*RawBoard(BOARD_BGN+1, BOARD_BGN) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+1)) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+2)) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+3)) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+4)) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+5)) &
          +RawBoard(BOARD_END, BOARD_BGN+6)
     localWhiteLine=index+3281
  case(6) ! P6ライン
     index= &
          3*(3*(3*(3*(3*(3*(3*RawBoard(BOARD_BGN, BOARD_BGN) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+1)) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+2)) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+3)) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+4)) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+5)) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+6)) &
          +RawBoard(BOARD_END, BOARD_END)
     localWhiteLine=index+3281
  case(7) ! P7ライン
     index= &
          3*(3*(3*(3*(3*(3*RawBoard(BOARD_BGN, BOARD_BGN+1) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+2)) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+3)) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+4)) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+5)) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+6)) &
          +RawBoard(BOARD_BGN+6, BOARD_END)
     localWhiteLine=index+3281
  case(8) ! P8ライン
     index= &
          3*(3*(3*(3*(3*RawBoard(BOARD_BGN, BOARD_BGN+2) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+3)) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+4)) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+5)) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+6)) &
          +RawBoard(BOARD_BGN+5, BOARD_END)
     localWhiteLine=index+3281
  case(9) ! P9ライン
     index= &
          3*(3*(3*(3*RawBoard(BOARD_BGN, BOARD_BGN+3) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+4)) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+5)) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+6)) &
          +RawBoard(BOARD_BGN+4, BOARD_END)
     localWhiteLine=index+3281
  case(10) ! P10ライン
     index= &
          3*(3*(3*RawBoard(BOARD_BGN, BOARD_BGN+4) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+5)) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+6)) &
          +RawBoard(BOARD_BGN+3, BOARD_END)
     localWhiteLine=index+3281
  case(11) ! P11ライン
     index= &
          3*(3*RawBoard(BOARD_BGN, BOARD_BGN+5) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+6)) &
          +RawBoard(BOARD_BGN+2, BOARD_END)
     localWhiteLine=index+3281
  case default ! ここはコーナー2マスのとき入ってくる
     localWhiteLine=3281 ! EEEEEEEEであることにしておく
  end select

  return
end subroutine getLocalIndex

!----------------------------------------------------------------

subroutine theBook(in, switchPhase, evalMax) ! 定石を探す

  use setParameters, only : &
       ON, OFF, &
       OPENING_MODE, &
       MIDDLE_MODE, &
       BOARD_SIZE, &
       INFINITY
  use userSet, only : &
       AI_COLOR
  use setTheBookParams, only : &
       THEBOOKWAY ! 使用する定石パターンの数
  use setVariables, only : &
       MemoryPutPlace, &
       Turns
  use setTheBookVarias, only : &
       WayBook, & ! 定石の棋譜が格納される
       EvalBook   ! 定石の評価値

  implicit none

  integer &
       n, m
  integer &
       x, y, &    ! 変換前の座標
       xNew, yNew ! 変換後の座標
  integer &
       consistBook, & ! 定石ファイルに書かれた棋譜との一致を判断する変数
       switchPhase
  real &
       eval, evalMax
  character(len=2) &
       in
  integer, allocatable:: &
       rotatedPutPlace(:)

  ! 初期化
  x=0
  y=0

  ! Turns=1の場合はf5を返す
  if(Turns == 1) then
     in='f5'
     return
  end if


  ! 過去の履歴(MemoryPutPlace)の回転操作(1手目をf5となるようにする)
  allocate(rotatedPutPlace(1:Turns-1))

  select case (MemoryPutPlace(1))
  case(54) ! d3、時計周りに90度回転 + x軸に対して反転

     do m=1, Turns-1
        x=int(MemoryPutPlace(m)*0.1) ! 回転前のx座標
        y=mod(MemoryPutPlace(m), 10) ! 回転前のy座標
        xNew=(BOARD_SIZE+2)-y+1      ! 回転後のx座標
        yNew=(BOARD_SIZE+2)-x+1      ! 回転後のy座標
        rotatedPutPlace(m)=xNew*10+yNew
     end do

  case(45) ! c4、180度回転

     do m=1, Turns-1
        x=int(MemoryPutPlace(m)*0.1) ! 回転前のx座標
        y=mod(MemoryPutPlace(m), 10) ! 回転前のy座標
        xNew=(BOARD_SIZE+2)-x+1      ! 回転後のx座標
        yNew=(BOARD_SIZE+2)-y+1      ! 回転後のy座標
        rotatedPutPlace(m)=xNew*10+yNew
     end do

  case(67) ! e6、反時計周りに90度回転 + x軸に対して反転

     do m=1, Turns-1
        x=int(MemoryPutPlace(m)*0.1) ! 回転前のx座標
        y=mod(MemoryPutPlace(m), 10) ! 回転前のy座標
        xNew=y                       ! 回転後のx座標
        yNew=x                       ! 回転後のy座標
        rotatedPutPlace(m)=xNew*10+yNew
     end do
     
  case default ! 回転させる必要無し

     rotatedPutPlace(1:Turns-1)=MemoryPutPlace(1:Turns-1) ! そのまま代入

  end select


  ! 定石を探す
  switchPhase=MIDDLE_MODE ! ゲームフェーズの初期化、ここに入ってくるときはOPENING_MODE
  evalMax=-INFINITY ! 評価値の初期化
  do n=1, THEBOOKWAY

     consistBook=ON ! 過去の履歴が定石通り
     do m=1, Turns-1
        if(rotatedPutPlace(m) /= WayBook(n, m)) then
           consistBook=OFF ! 過去の履歴が定石に従っていない
           exit
        end if
     end do
     if(WayBook(n, Turns) == 0) consistBook=OFF ! 過去の履歴は定石通りであるが、次打てる定石が無い

     eval=EvalBook(n)*AI_COLOR ! AI視点の評価値に変換
     if(consistBook == ON .and. eval >= 0) then ! 定石があり、その評価値が自分に有利である

        if(eval > evalMax) then ! 評価値が最大のものを選ぶ
           x=int(WayBook(n, Turns)*0.1)
           y=mod(WayBook(n, Turns), 10)
           evalMax=eval
           switchPhase=OPENING_MODE ! 定石ありなので、次のターンも定石を探しに来る
        end if

     end if

  end do
  if(switchPhase == MIDDLE_MODE) return ! 定石なしなので、次のターンからは定石探しは行わない


  ! 元の座標に戻す回転操作(打つ手のみ)
  select case (MemoryPutPlace(1))
  case(54) ! d3、時計周りに90度回転 + x軸に対して反転
     xNew=(BOARD_SIZE+2)-y+1
     yNew=(BOARD_SIZE+2)-x+1
  case(45) ! c4、180度回転
     xNew=(BOARD_SIZE+2)-x+1
     yNew=(BOARD_SIZE+2)-y+1
  case(67) ! e6、反時計周りに90度回転 + x軸に対して反転
     xNew=y
     yNew=x
  case default ! 変換の必要無し
     xNew=x
     yNew=y
  end select

  deallocate(rotatedPutPlace) ! 配列の開放

  call writeIn(xNew, yNew, in) ! 打つ手の数値表示を文字表示に変換

  return
end subroutine theBook

!----------------------------------------------------------------

subroutine getHorizontal1Index(horizPLine) ! 水平パターン1のインデックスを取得する

  use setParameters, only : &
       BOARD_SIZE, &
       BOARD_BGN, BOARD_END
  use setVariables, only : &
       RawBoard

  implicit none

  integer &
       n, l, &
       xNew, yNew, &
       index
  integer, parameter:: &
       nm=8, & ! パターンを構成するマスの数
       lm=4 ! 回転操作数
  integer, dimension(lm):: &
       horizPLine ! インデックスを格納
  integer, dimension(nm):: &
       x= (/ 2, 3, 4, 5, 6, 7, 8, 9 /), & ! パターンを構成するマスの配列
       y= (/ 2, 2, 2, 2, 2, 2, 2, 2 /)

  !   a b c d e f g h 
  !   ---------------
  ! 1|@ @ @ @ @ @ @ @|
  ! 2|               |
  ! 3|               |
  ! 4|      O X      |
  ! 5|      X O      |
  ! 6|               |
  ! 7|               |
  ! 8|               |
  !   ---------------
  ! type: x=BOARD_SIZE/2 ! 対称軸のタイプ

  do l=1, lm ! 回転ループ
     index=0

     do n=1, nm ! マスループ
        index=index+RawBoard(x(n), y(n))*3**(n-1)
     end do
     horizPLine(l)=index+3281

     do n=1, nm ! 時計周りに90度回転
        xNew=(BOARD_SIZE+2)-y(n)+1
        yNew=x(n)
        x(n)=xNew
        y(n)=yNew
     end do

  end do

  return
end subroutine getHorizontal1Index

!----------------------------------------------------------------

subroutine getHorizontal2Index(horizPLine) ! 水平パターン2のインデックスを取得する

  use setParameters, only : &
       BOARD_SIZE, &
       BOARD_BGN, BOARD_END
  use setVariables, only : &
       RawBoard

  implicit none

  integer &
       n, l, &
       xNew, yNew, &
       index
  integer, parameter:: &
       nm=8, & ! パターンを構成するマスの数
       lm=4 ! 回転操作数
  integer, dimension(lm):: &
       horizPLine ! インデックスを格納
  integer, dimension(nm):: &
       x= (/ 2, 3, 4, 5, 6, 7, 8, 9 /), & ! パターンを構成するマスの配列
       y= (/ 3, 3, 3, 3, 3, 3, 3, 3 /)

  !   a b c d e f g h 
  !   ---------------
  ! 1|               |
  ! 2|@ @ @ @ @ @ @ @|
  ! 3|               |
  ! 4|      O X      |
  ! 5|      X O      |
  ! 6|               |
  ! 7|               |
  ! 8|               |
  !   ---------------
  ! type: x=BOARD_SIZE/2 ! 対称軸のタイプ

  do l=1, lm ! 回転ループ
     index=0

     do n=1, nm ! マスループ
        index=index+RawBoard(x(n), y(n))*3**(n-1)
     end do
     horizPLine(l)=index+3281

     do n=1, nm ! 時計周りに90度回転
        xNew=(BOARD_SIZE+2)-y(n)+1
        yNew=x(n)
        x(n)=xNew
        y(n)=yNew
     end do

  end do

  return
end subroutine getHorizontal2Index

!----------------------------------------------------------------

subroutine getHorizontal3Index(horizPLine) ! 水平パターン3のインデックスを取得する

  use setParameters, only : &
       BOARD_SIZE, &
       BOARD_BGN, BOARD_END
  use setVariables, only : &
       RawBoard

  implicit none

  integer &
       n, l, &
       xNew, yNew, &
       index
  integer, parameter:: &
       nm=8, & ! パターンを構成するマスの数
       lm=4 ! 回転操作数
  integer, dimension(lm):: &
       horizPLine ! インデックスを格納
  integer, dimension(nm):: &
       x= (/ 2, 3, 4, 5, 6, 7, 8, 9 /), & ! パターンを構成するマスの配列
       y= (/ 4, 4, 4, 4, 4, 4, 4, 4 /)

  !   a b c d e f g h 
  !   ---------------
  ! 1|               |
  ! 2|               |
  ! 3|@ @ @ @ @ @ @ @|
  ! 4|      O X      |
  ! 5|      X O      |
  ! 6|               |
  ! 7|               |
  ! 8|               |
  !   ---------------
  ! type: x=BOARD_SIZE/2 ! 対称軸のタイプ

  do l=1, lm ! 回転ループ
     index=0

     do n=1, nm ! マスループ
        index=index+RawBoard(x(n), y(n))*3**(n-1)
     end do
     horizPLine(l)=index+3281

     do n=1, nm ! 時計周りに90度回転
        xNew=(BOARD_SIZE+2)-y(n)+1
        yNew=x(n)
        x(n)=xNew
        y(n)=yNew
     end do

  end do

  return
end subroutine getHorizontal3Index

!----------------------------------------------------------------

subroutine getHorizontal4Index(horizPLine) ! 水平パターン4のインデックスを取得する

  use setParameters, only : &
       BOARD_SIZE, &
       BOARD_BGN, BOARD_END
  use setVariables, only : &
       RawBoard

  implicit none

  integer &
       n, l, &
       xNew, yNew, &
       index
  integer, parameter:: &
       nm=8, & ! パターンを構成するマスの数
       lm=4 ! 回転操作数
  integer, dimension(lm):: &
       horizPLine ! インデックスを格納
  integer, dimension(nm):: &
       x= (/ 2, 3, 4, 5, 6, 7, 8, 9 /), & ! パターンを構成するマスの配列
       y= (/ 5, 5, 5, 5, 5, 5, 5, 5 /)

  !   a b c d e f g h 
  !   ---------------
  ! 1|               |
  ! 2|               |
  ! 3|               |
  ! 4|@ @ @ @ @ @ @ @|
  ! 5|      X O      |
  ! 6|               |
  ! 7|               |
  ! 8|               |
  !   ---------------
  ! type: x=BOARD_SIZE/2 ! 対称軸のタイプ

  do l=1, lm ! 回転ループ
     index=0

     do n=1, nm ! マスループ
        index=index+RawBoard(x(n), y(n))*3**(n-1)
     end do
     horizPLine(l)=index+3281

     do n=1, nm ! 時計周りに90度回転
        xNew=(BOARD_SIZE+2)-y(n)+1
        yNew=x(n)
        x(n)=xNew
        y(n)=yNew
     end do

  end do

  return
end subroutine getHorizontal4Index

!----------------------------------------------------------------

subroutine getDiagonal1Index(diagoPLine) ! 斜めパターン1のインデックスを取得する

  use setParameters, only : &
       BOARD_SIZE, &
       BOARD_BGN, BOARD_END
  use setVariables, only : &
       RawBoard

  implicit none

  integer &
       n, l, &
       xNew, yNew, &
       index
  integer, parameter:: &
       nm=8, & ! パターンを構成するマスの数
       lm=2 ! 回転操作数
  integer, dimension(lm):: &
       diagoPLine ! インデックスを格納
  integer, dimension(nm):: &
       x= (/ 2, 3, 4, 5, 6, 7, 8, 9 /), & ! パターンを構成するマスの配列
       y= (/ 2, 3, 4, 5, 6, 7, 8, 9 /)

  !   a b c d e f g h 
  !   ---------------
  ! 1|@              |
  ! 2|  @            |
  ! 3|    @          |
  ! 4|      @ X      |
  ! 5|      X @      |
  ! 6|          @    |
  ! 7|            @  |
  ! 8|              @|
  !   ---------------
  ! type: x=y ! 対称軸タイプ

  do l=1, lm ! 回転ループ
     index=0

     do n=1, nm ! マスループ
        index=index+RawBoard(x(n), y(n))*3**(n-1)
     end do
     diagoPLine(l)=index+3281

     do n=1, nm ! 時計周りに90度回転
        xNew=(BOARD_SIZE+2)-y(n)+1
        yNew=x(n)
        x(n)=xNew
        y(n)=yNew
     end do

  end do

  return
end subroutine getDiagonal1Index

!----------------------------------------------------------------

subroutine getDiagonal2Index(diagoPLine) ! 斜めパターン2のインデックスを取得する

  use setParameters, only : &
       BOARD_SIZE, &
       BOARD_BGN, BOARD_END
  use setVariables, only : &
       RawBoard

  implicit none

  integer &
       n, l, &
       xNew, yNew, &
       index
  integer, parameter:: &
       nm=7, & ! パターンを構成するマスの数
       lm=4 ! 回転操作数
  integer, dimension(lm):: &
       diagoPLine ! インデックスを格納
  integer, dimension(nm):: &
       x= (/ 3, 4, 5, 6, 7, 8, 9 /), & ! パターンを構成するマスの配列
       y= (/ 2, 3, 4, 5, 6, 7, 8 /)

  !   a b c d e f g h 
  !   ---------------
  ! 1|  @            |
  ! 2|    @          |
  ! 3|      @        |
  ! 4|      O @      |
  ! 5|      X O @    |
  ! 6|            @  |
  ! 7|              @|
  ! 8|               |
  !   ---------------
  ! type: x=y ! 対称軸タイプ

  do l=1, lm ! 回転ループ
     index=0

     do n=1, nm ! マスループ
        index=index+RawBoard(x(n), y(n))*3**(n-1)
     end do
     diagoPLine(l)=index+3281

     do n=1, nm ! 時計周りに90度回転
        xNew=(BOARD_SIZE+2)-y(n)+1
        yNew=x(n)
        x(n)=xNew
        y(n)=yNew
     end do

  end do

  return
end subroutine getDiagonal2Index

!----------------------------------------------------------------

subroutine getDiagonal3Index(diagoPLine) ! 斜めパターン3のインデックスを取得する

  use setParameters, only : &
       BOARD_SIZE, &
       BOARD_BGN, BOARD_END
  use setVariables, only : &
       RawBoard

  implicit none

  integer &
       n, l, &
       xNew, yNew, &
       index
  integer, parameter:: &
       nm=6, & ! パターンを構成するマスの数
       lm=4 ! 回転操作数
  integer, dimension(lm):: &
       diagoPLine ! インデックスを格納
  integer, dimension(nm):: &
       x= (/ 4, 5, 6, 7, 8, 9 /), & ! パターンを構成するマスの配列
       y= (/ 2, 3, 4, 5, 6, 7 /)

  !   a b c d e f g h 
  !   ---------------
  ! 1|    @          |
  ! 2|      @        |
  ! 3|        @      |
  ! 4|      O X @    |
  ! 5|      X O   @  |
  ! 6|              @|
  ! 7|               |
  ! 8|               |
  !   ---------------
  ! type: x=y

  do l=1, lm ! 回転ループ
     index=0

     do n=1, nm ! マスループ
        index=index+RawBoard(x(n), y(n))*3**(n-1)
     end do
     diagoPLine(l)=index+3281

     do n=1, nm ! 時計周りに90度回転
        xNew=(BOARD_SIZE+2)-y(n)+1
        yNew=x(n)
        x(n)=xNew
        y(n)=yNew
     end do

  end do

  return
end subroutine getDiagonal3Index

!----------------------------------------------------------------

subroutine getDiagonal4Index(diagoPLine) ! 斜めパターン4のインデックスを取得する

  use setParameters, only : &
       BOARD_SIZE, &
       BOARD_BGN, BOARD_END
  use setVariables, only : &
       RawBoard

  implicit none

  integer &
       n, l, &
       xNew, yNew, &
       index
  integer, parameter:: &
       nm=5, & ! パターンを構成するマスの数
       lm=4 ! 回転操作数
  integer, dimension(lm):: &
       diagoPLine ! インデックスを格納
  integer, dimension(nm):: &
       x= (/ 5, 6, 7, 8, 9 /), & ! パターンを構成するマスの配列
       y= (/ 2, 3, 4, 5, 6 /)

  !   a b c d e f g h 
  !   ---------------
  ! 1|      @        |
  ! 2|        @      |
  ! 3|          @    |
  ! 4|      O X   @  |
  ! 5|      X O     @|
  ! 6|               |
  ! 7|               |
  ! 8|               |
  !   ---------------
  ! type: x=y ! 対称軸タイプ

  do l=1, lm ! 回転ループ
     index=0

     do n=1, nm ! マスループ
        index=index+RawBoard(x(n), y(n))*3**(n-1)
     end do
     diagoPLine(l)=index+3281

     do n=1, nm ! 時計周りに90度回転
        xNew=(BOARD_SIZE+2)-y(n)+1
        yNew=x(n)
        x(n)=xNew
        y(n)=yNew
     end do

  end do

  return
end subroutine getDiagonal4Index

!----------------------------------------------------------------

subroutine getDiagonal5Index(diagoPLine) ! 斜めパターン5のインデックスを取得する

  use setParameters, only : &
       BOARD_SIZE, &
       BOARD_BGN, BOARD_END
  use setVariables, only : &
       RawBoard

  implicit none

  integer &
       n, l, &
       xNew, yNew, &
       index
  integer, parameter:: &
       nm=4, & ! パターンを構成するマスの数
       lm=4 ! 回転操作数
  integer, dimension(lm):: &
       diagoPLine ! インデックスを格納
  integer, dimension(nm):: & ! パターンを構成するマスの配列
       x= (/ 6, 7, 8, 9 /), &
       y= (/ 2, 3, 4, 5 /)

  !   a b c d e f g h 
  !   ---------------
  ! 1|        @      |
  ! 2|          @    |
  ! 3|            @  |
  ! 4|      O X     @|
  ! 5|      X O      |
  ! 6|               |
  ! 7|               |
  ! 8|               |
  !   ---------------
  ! type: x=y ! 対称軸タイプ

  do l=1, lm ! 回転ループ
     index=0

     do n=1, nm ! マスループ
        index=index+RawBoard(x(n), y(n))*3**(n-1)
     end do
     diagoPLine(l)=index+3281

     do n=1, nm ! 時計周りに90度回転
        xNew=(BOARD_SIZE+2)-y(n)+1
        yNew=x(n)
        x(n)=xNew
        y(n)=yNew
     end do

  end do

  return
end subroutine getDiagonal5Index

!----------------------------------------------------------------

subroutine getCornerIndex(cornerPLine) ! コーナーパターンのインデックスを取得する

  use setParameters, only : &
       BOARD_SIZE, &
       BOARD_BGN, BOARD_END
  use setVariables, only : &
       RawBoard

  implicit none

  integer &
       n, l, &
       xNew, yNew, &
       index
  integer, parameter:: &
       nm=8, & ! パターンを構成するマスの数
       lm=4 ! 回転操作数
  integer, dimension(lm):: &
       cornerPLine ! インデックスを格納
  integer, dimension(nm):: &
       x= (/ 2, 3, 4, 2, 3, 4, 2, 3 /), & ! パターンを構成するマスの配列
       y= (/ 2, 2, 2, 3, 3, 3, 4, 4 /)

  !   a b c d e f g h 
  !   ---------------
  ! 1|@ @ @          |
  ! 2|@ @ @          |
  ! 3|@ @            |
  ! 4|      O X      |
  ! 5|      X O      |
  ! 6|               |
  ! 7|               |
  ! 8|               |
  !   ---------------
  ! type: x=y ! 対称軸タイプ

  do l=1, lm ! 回転ループ
     index=0

     do n=1, nm ! マスループ
        index=index+RawBoard(x(n), y(n))*3**(n-1)
     end do
     cornerPLine(l)=index+3281

     do n=1, nm ! 時計周りに90度回転
        xNew=(BOARD_SIZE+2)-y(n)+1
        yNew=x(n)
        x(n)=xNew
        y(n)=yNew
     end do

  end do

  return
end subroutine getCornerIndex
