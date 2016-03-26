subroutine move(x, y, correctIn) ! 石を置き、裏返す

  use setParameters, only : &
       NON, &
       ON, OFF, &
       BOARD_BGN, BOARD_END
  use setVariables, only : &
       Turns, &
       MovableDir, &
       MemoryCurrentColor, &
       MemoryRawBoard, &
       MemoryPutPlace, &
       CurrentColor, &
       RawBoard

  implicit none

  integer &
       correctIn
  integer &
       x, y

  if(x < BOARD_BGN .or. x > BOARD_END) then ! x座標がボード外だったら、correctInにOFFを代入し返る
     correctIn=OFF
     return
  end if
  if(y < BOARD_BGN .or. y > BOARD_END) then ! y座標がボード外だったら、correctInにOFFを代入し返る
     correctIn=OFF
     return
  end if
  if(MovableDir(Turns, x, y) == NON) then ! 指定したマスには置けないとき、correctInにOFFを代入し返る
     correctIn=OFF
     return
  end if

  MemoryCurrentColor(Turns)=CurrentColor ! 現在の手番（石の色）を保存
  MemoryRawBoard(Turns, BOARD_BGN:BOARD_END, BOARD_BGN:BOARD_END) & ! 現在の盤面を保存
  =RawBoard(BOARD_BGN:BOARD_END, BOARD_BGN:BOARD_END)

  call flipDiscs(x, y) ! 実際に石を返すサブルーチン

  MemoryPutPlace(Turns)=10*x+y ! 石を置いた場所を2桁の数値で保存（例：a1=22）

  Turns=Turns+1 ! 石を置き裏返したら、ターンを進める

  CurrentColor=-CurrentColor ! 手番交代

  call initMovable(CurrentColor) ! MovableDir（置けるマスの情報）を更新

  return
end subroutine move

!----------------------------------------------------------------

subroutine pass(correctIn) ! パスが可能か調べ、可能な場合はcorrectIn=ONにして返す

  use setParameters, only : &
       ON, OFF, &
       BOARD_BGN, BOARD_END, &
       NON
  use setVariables, only : &
       CurrentColor, &
       Turns, &
       MovableDir

  implicit none

  integer &
       correctIn
  integer &
       x, y

  call initMovable(CurrentColor) ! MovableDir（置けるマスの情報）を更新
  do x=BOARD_BGN, BOARD_END
     do y=BOARD_BGN, BOARD_END
        if(MovableDir(Turns, x, y) /= NON) return ! 置ける場所が一つでもあれば、パスではない
     end do
  end do

  correctIn=ON ! パス可能と同時に入力の正誤を正にする

  CurrentColor=-CurrentColor ! パスによって手番交代

  call initMovable(CurrentColor) ! MovableDir（置けるマスの情報）を更新（黒（白）視点から白（黒）視点にする）

  return
end subroutine pass

!----------------------------------------------------------------

subroutine unundo(correctIn) ! 取り消し

  use setParameters, only : &
       ON, OFF, &
       BOARD_BGN, BOARD_END
  use userSet, only : &
       AI_COLOR
  use setVariables, only : &
       CurrentColor, &
       Turns, &
       MemoryCurrentColor, &
       MemoryRawBoard, &
       RawBoard, &
       MemoryPutPlace

  implicit none

  integer &
       correctIn

  do ! 前回の自分の番まで戻す（1手前が自分の番とは限らないことに注意）

     if(Turns <= 1) then ! 1手目までは手を戻せない
        correctIn=OFF
        call output('can not undo.', ON) ! 警告のターミナル出力
        return
     end if

     MemoryCurrentColor(Turns)=0 ! 現在の色情報を初期化
     MemoryRawBoard(Turns, BOARD_BGN:BOARD_END, BOARD_BGN:BOARD_END)=0 ! 現在の盤面を初期化
     MemoryPutPlace(Turns)=0 ! 現在打ったマス情報を初期化

     Turns=Turns-1 ! 1手戻してみる
  
     CurrentColor=MemoryCurrentColor(Turns) ! 1手前の手番の色を呼び出す
     RawBoard(BOARD_BGN:BOARD_END, BOARD_BGN:BOARD_END) & ! 1手前の盤面を呼び出す
     =MemoryRawBoard(Turns, BOARD_BGN:BOARD_END, BOARD_BGN:BOARD_END)
  
     if(CurrentColor /= AI_COLOR) exit ! 自分の番であれば、ループを抜ける

  end do

  correctIn=ON ! 入力は正しい

  call countDiscs ! 石を数え直す

  return
end subroutine unundo

!----------------------------------------------------------------

subroutine flipDiscs(x, y) ! 石を置き裏返す

  use setParameters, only : &
       UPPER, &
       LOWER, &
       LEFT, &
       RIGHT, &
       UPPER_RIGHT, &
       UPPER_LEFT, &
       LOWER_LEFT, &
       LOWER_RIGHT
  use setVariables, only : &
       Turns, &
       RawBoard, &
       CurrentColor, &
       MovableDir
  
  implicit none

  integer &
       x, y, &   ! 石を置いた座標
       xv, yv, & ! x, y座標の仮変数
       dir       ! x, yに石を置いた場合どの方向に石が返るかを記録する

  RawBoard(x, y)=CurrentColor ! そこに石を置く

  dir=MovableDir(Turns, x, y) ! どの方向に石が返るかを代入

  xv=x
  yv=y
  if(btest(dir, UPPER-1)) then ! 上方向（dirのUPPER番目のビットが１かどうかを判定）
     ! 置いた場所の上が自分の石ではない場合に、そこを自分の石にしてもう1つ上の判定に移る
     do while(RawBoard(xv, yv-1) /= CurrentColor)
        RawBoard(xv, yv-1)=CurrentColor
        yv=yv-1
     end do
  end if

  xv=x
  yv=y
  if(btest(dir, LOWER-1)) then ! 下方向（dirのLOWER番目のビットが１かどうかを判定）
     ! 置いた場所の下が自分の石ではない場合に、そこを自分の石にしてもう1つ下の判定に移る
     do while(RawBoard(xv, yv+1) /= CurrentColor)
        RawBoard(xv, yv+1)=CurrentColor
        yv=yv+1
     end do
  end if

  xv=x
  yv=y
  if(btest(dir, LEFT-1)) then ! 左方向（dirのLEFT番目のビットが１かどうかを判定）
     ! 置いた場所の左が自分の石ではない場合に、そこを自分の石にしてもう1つ左の判定に移る
     do while(RawBoard(xv-1, yv) /= CurrentColor)
        RawBoard(xv-1, yv)=CurrentColor
        xv=xv-1
     end do
  end if

  xv=x
  yv=y
  if(btest(dir, RIGHT-1)) then ! 右方向（dirのRIGHT番目のビットが１かどうかを判定）
     ! 置いた場所の右が自分の石ではない場合に、そこを自分の石にしてもう1つ右の判定に移る
     do while(RawBoard(xv+1, yv) /= CurrentColor)
        RawBoard(xv+1, yv)=CurrentColor
        xv=xv+1
     end do
  end if

  xv=x
  yv=y
  if(btest(dir, UPPER_RIGHT-1)) then ! 右上方向（dirのUPPER_RIGHT番目のビットが１かどうかを判定）
     ! 置いた場所の右上が自分の石ではない場合に、そこを自分の石にしてもう1つ右上の判定に移る
     do while(RawBoard(xv+1, yv-1) /= CurrentColor)
        RawBoard(xv+1, yv-1)=CurrentColor
        xv=xv+1
        yv=yv-1
     end do
  end if

  xv=x
  yv=y
  if(btest(dir, UPPER_LEFT-1)) then ! 左上方向（dirのUPPER_LEFT番目のビットが１かどうかを判定）
     ! 置いた場所の左上が自分の石ではない場合に、そこを自分の石にしてもう1つ左上の判定に移る
     do while(RawBoard(xv-1, yv-1) /= CurrentColor)
        RawBoard(xv-1, yv-1)=CurrentColor
        xv=xv-1
        yv=yv-1
     end do
  end if

  xv=x
  yv=y
  if(btest(dir, LOWER_LEFT-1)) then ! 左下方向（dirのLOWER_LEFT番目のビットが１かどうかを判定）
     ! 置いた場所の左下が自分の石ではない場合に、そこを自分の石にしてもう1つ左下の判定に移る
     do while(RawBoard(xv-1, yv+1) /= CurrentColor)
        RawBoard(xv-1, yv+1)=CurrentColor
        xv=xv-1
        yv=yv+1
     end do
  end if

  xv=x
  yv=y
  if(btest(dir, LOWER_RIGHT-1)) then ! 右下方向（dirのLOWER_RIGHT番目のビットが１かどうかを判定）
     ! 置いた場所の右下が自分の石ではない場合に、そこを自分の石にしてもう1つ右下の判定に移る
     do while(RawBoard(xv+1, yv+1) /= CurrentColor)
        RawBoard(xv+1, yv+1)=CurrentColor
        xv=xv+1
        yv=yv+1
     end do
  end if

  call countDiscs ! 石（色）の数を数える

  return
end subroutine flipDiscs

!----------------------------------------------------------------

subroutine isGameOver(gameRetire, gameContinue) ! ゲーム終了か続行かを判断

  use setParameters, only : &
       ON, OFF, &
       EMPTY, &
       MAX_TURNS, &
       BOARD_BGN, BOARD_END
  use setVariables, only : &
       Turns, &
       MovableDir, &
       CurrentColor

  implicit none

  integer &
       gameRetire, &
       gameContinue ! このサブルーチンで決定する変数
  integer &
       x, y

  ! 最終ターンもしくはゲーム中止コマンドが入力されていれば、gameContinueをOFFにして返す
  if(Turns == MAX_TURNS .or. gameRetire == ON) then 
     gameContinue=OFF
     return
  end if

  ! 自分が置ける場所があるかを調べる
  do y=BOARD_BGN, BOARD_END
     do x=BOARD_BGN, BOARD_END
        if(MovableDir(Turns, x, y) /= EMPTY) then
           gameContinue=ON ! 置ける場所があればゲーム続行
           return
        end if
     end do
  end do

  call initMovable(-CurrentColor) ! 置けるマス情報を相手にする

  ! 相手が置ける場所があるかを調べる
  do y=BOARD_BGN, BOARD_END
     do x=BOARD_BGN, BOARD_END
        if(MovableDir(Turns, x, y) /= EMPTY) then
           gameContinue=ON ! 置ける場所があればゲーム続行
           call initMovable(CurrentColor) ! 置けるマス情報を自分に戻しておく
           return
        end if
     end do
  end do

  gameContinue=OFF ! ここまでくればどちらもおける場所がないということ、ゲーム終了

  return
end subroutine isGameOver

!----------------------------------------------------------------

subroutine retireGame(correctIn, gameRetire) ! ゲーム止める

  use setParameters, only : &
       ON

  implicit none

  integer &
       correctIn, &
       gameRetire

  correctIn=ON  ! 入力を正に
  gameRetire=ON ! ゲーム中止をONに

  return
end subroutine retireGame

!----------------------------------------------------------------

subroutine initMovable(turgetColor) ! checkMobilityを呼び出し、置ける場所を探す

  use setParameters, only : &
       BOARD_BGN, BOARD_END

  implicit none
  
  integer &
       turgetColor ! BLACKかWHITEが入っており、その色を対象とした操作となる
  
  integer &
       x, y
  
  do y=BOARD_BGN, BOARD_END
     do x=BOARD_BGN, BOARD_END

        ! turgetColorの色について、座標(x, y)に石を置いた場合どの方向に石を返せるかの情報を取得する
        call checkMobility(turgetColor, x, y) 

     end do
  end do

  return
end subroutine initMovable

!----------------------------------------------------------------

subroutine checkMobility(turgetColor, x, y) 
! turgetColorの色について、座標(x, y)に石を置いた場合どの方向に石を返せるかの情報を取得する

  use setParameters, only : &
       NON, &
       EMPTY, &
       UPPER, &
       LOWER, &
       LEFT, &
       RIGHT, &
       UPPER_RIGHT, &
       UPPER_LEFT, &
       LOWER_LEFT, &
       LOWER_RIGHT
  use setVariables, only : &
       RawBoard, &
       MovableDir, &
       Turns

  implicit none

  integer &
       turgetColor
  integer &
       x, y, &
       xv, yv
  integer &
       dir
  
  if(RawBoard(x, y) /= EMPTY) then ! その場所に既に石があれば、置けない
     MovableDir(Turns, x, y)=NON
     return
  end if
  
  dir=NON ! dirの初期化
  
  xv=x
  yv=y
  ! 上方向について、石を返せるかを調べる
  if(RawBoard(xv, yv-1) == -turgetColor) then ! 1つ上が相手の石
     xv=xv
     yv=yv-2
     do while(RawBoard(xv, yv) == -turgetColor) ! 2つ上が相手の石のときは、もう1つ上へ
        yv=yv-1
     end do
     ! 最終的に自分の石があれば、その方向には石を返せる
     if(RawBoard(xv, yv) == turgetColor) dir=ibset(dir, UPPER-1) ! dirのUPPER番目のビットを1に設定
  end if

  xv=x
  yv=y
  ! 下方向について、石を返せるかを調べる
  if(RawBoard(xv, yv+1) == -turgetColor) then ! 1つ下が相手の石
     xv=xv
     yv=yv+2
     do while(RawBoard(xv, yv) == -turgetColor) ! 2つ下が相手の石のときは、もう1つ下へ
        yv=yv+1
     end do
     ! 最終的に自分の石があれば、その方向には石を返せる
     if(RawBoard(xv, yv) == turgetColor) dir=ibset(dir, LOWER-1) ! dirのLOWER番目のビットを1に設定
  end if
  
  xv=x
  yv=y
  ! 左方向について、石を返せるかを調べる
  if(RawBoard(xv-1, yv) == -turgetColor) then ! 1つ左が相手の石
     xv=xv-2
     yv=yv
     do while(RawBoard(xv, yv) == -turgetColor) ! 2つ左が相手の石のときは、もう1つ左へ
        xv=xv-1
     end do
     ! 最終的に自分の石があれば、その方向には石を返せる
     if(RawBoard(xv, yv) == turgetColor) dir=ibset(dir, LEFT-1) ! dirのLEFT番目のビットを1に設定
  end if
  
  xv=x
  yv=y
  ! 右方向について、石を返せるかを調べる
  if(RawBoard(xv+1, yv) == -turgetColor) then ! 1つ右が相手の石
     xv=xv+2
     yv=yv
     do while(RawBoard(xv, yv) == -turgetColor) ! 2つ右が相手の石のときは、もう1つ右へ
        xv=xv+1
     end do
     ! 最終的に自分の石があれば、その方向には石を返せる
     if(RawBoard(xv, yv) == turgetColor) dir=ibset(dir, RIGHT-1) ! dirのRIGHT番目のビットを1に設定
  end if

  xv=x
  yv=y
  ! 右上方向について、石を返せるかを調べる
  if(RawBoard(xv+1, yv-1) == -turgetColor) then ! 1つ右上が相手の石
     xv=xv+2
     yv=yv-2
     do while(RawBoard(xv, yv) == -turgetColor) ! 2つ右上が相手の石のときは、もう1つ右上へ
        xv=xv+1
        yv=yv-1
     end do
     ! 最終的に自分の石があれば、その方向には石を返せる
     if(RawBoard(xv, yv) == turgetColor) dir=ibset(dir, UPPER_RIGHT-1) ! dirのUPPER_RIGHT番目のビットを1に設定
  end if
  
  xv=x
  yv=y
  ! 左上方向について、石を返せるかを調べる
  if(RawBoard(xv-1, yv-1) == -turgetColor) then ! 1つ左上が相手の石
     xv=xv-2
     yv=yv-2
     do while(RawBoard(xv, yv) == -turgetColor) ! 2つ左上が相手の石のときは、もう1つ左上へ
        xv=xv-1
        yv=yv-1
     end do
     ! 最終的に自分の石があれば、その方向には石を返せる
     if(RawBoard(xv, yv) == turgetColor) dir=ibset(dir, UPPER_LEFT-1) ! dirのUPPER_LEFT番目のビットを1に設定
  end if

  xv=x
  yv=y
  ! 左下方向について、石を返せるかを調べる
  if(RawBoard(xv-1, yv+1) == -turgetColor) then ! 1つ左下が相手の石
     xv=xv-2
     yv=yv+2
     do while(RawBoard(xv, yv) == -turgetColor) ! 2つ左下が相手の石のときは、もう1つ左下へ
        xv=xv-1
        yv=yv+1
     end do
     ! 最終的に自分の石があれば、その方向には石を返せる
     if(RawBoard(xv, yv) == turgetColor) dir=ibset(dir, LOWER_LEFT-1) ! dirのLOWER_LEFT番目のビットを1に設定
  end if
  
  xv=x
  yv=y
  ! 右下方向について、石を返せるかを調べる
  if(RawBoard(xv+1, yv+1) == -turgetColor) then ! 1つ右下が相手の石
     xv=xv+2
     yv=yv+2
     do while(RawBoard(xv, yv) == -turgetColor) ! 2つ右下が相手の石のときは、もう1つ右下へ
        xv=xv+1
        yv=yv+1
     end do
     ! 最終的に自分の石があれば、その方向には石を返せる
     if(RawBoard(xv, yv) == turgetColor) dir=ibset(dir, LOWER_RIGHT-1) ! dirのLOWER_RIGHT番目のビットを1に設定
  end if

  MovableDir(Turns, x, y)=dir ! 最終的にdirにすべての返せる方向についての情報が入っている
  
  return
end subroutine checkMobility

!----------------------------------------------------------------

subroutine countDiscs ! 盤上の石(各色)の数を数える

  use setParameters, only : &
       BOARD_BGN, BOARD_END, &
       BLACK, WHITE
  use setVariables, only : &
       DiscsBLACK, DiscsWHITE, DiscsEMPTY, &
       RawBoard

  implicit none

  integer &
       x, y

  ! 石数の初期化
  DiscsBLACK=0
  DiscsWHITE=0
  DiscsEMPTY=0

  ! シンプルにすべてのマスについて、調べている
  do y=BOARD_BGN, BOARD_END
     do x=BOARD_BGN, BOARD_END
        if(RawBoard(x, y) == BLACK) then
           DiscsBLACK=DiscsBLACK+1
        else if(RawBoard(x, y) == WHITE) then
           DiscsWHITE=DiscsWHITE+1
        else
           DiscsEMPTY=DiscsEMPTY+1
        end if
     end do
  end do

  return
end subroutine countDiscs

!----------------------------------------------------------------

subroutine random(in) ! 人間の手をランダムに任せる

  use setParameters, only : &
       BOARD_BGN, &
       BOARD_END, &
       NON
  use setVariables, only : &
       Turns, &
       MovableDir

  implicit none

  character(len=2) in
  integer x, y

  integer, allocatable, dimension(:) :: seed ! ランダム関数のシード値
  integer nrand ! シード値配列の大きさ
  integer clock ! 時間を格納、シード値は時間の関数とする
  real, dimension(2):: randomNum ! 2次元のランダム数

  call random_seed(size=nrand)   ! シード値配列のサイズを取得
  allocate(seed(nrand))          ! シード値配列の動的割付
  call system_clock(count=clock) ! 時間を取得

  seed=clock ! 時間をシード値とする
  call random_seed(put=seed)

  do
     call random_number(randomNum) ! 2次元ランダム数を取得
     x=int(randomNum(1)*10)        ! 0-10の値に変換 (x)
     y=int(randomNum(2)*10)        ! 0-10の値に変換 (y)

     ! 取得した座標(x, y)が盤上であり、そこに石を置くことが可能であればループを抜け手を決定
     if(x.ge.2.and.x.le.9.and.y.ge.2.and.y.le.9) then
        if(MovableDir(Turns, x, y).ne.NON) exit
     end if
  end do

  call writeIn(x, y, in) ! x, yをinに変換（例：(2, 2) -> a2）

  return
end subroutine random

!----------------------------------------------------------------

subroutine writeIn(x, y, in) ! 入力を文字から数値に変換（⇔ subroutine readIn）

  use setParameters, only : &
       BOARD_BGN, BOARD_END

  implicit none

  integer, intent(in) :: &
       x, y
  character(len=2), intent(out) :: &
       in
  character(len=1) &
       inx, iny !x, yそれぞれの文字

  ! x座標について
  select case (x)
  case(2); inx='a'
  case(3); inx='b'
  case(4); inx='c'
  case(5); inx='d'
  case(6); inx='e'
  case(7); inx='f'
  case(8); inx='g'
  case(9); inx='h'
  case default
     write(06, '(a)') 'program bug:: s:rewriteIn(X)'
     stop
  end select

  ! y座標について
  if(y < BOARD_BGN .and. y > BOARD_END) then
     write(06, '(a)') 'program bug:: s:rewriteIn(Y)'
     stop
  end if
  write(iny, '(i1)') y-1

  ! 結合
  in=inx//iny

  return
end subroutine writeIn

!----------------------------------------------------------------

subroutine readIn(x, y, correctIn, in) ! 入力を数値から文字に変換（⇔ subroutine writeIn）

  use setParameters, only : &
       ON, OFF, &
       BOARD_BGN, BOARD_END

  implicit none

  integer, intent(out) :: &
       x, y
  integer, intent(out) :: &
       correctIn
  character(len=2), intent(in) :: &
       in
  character(len=1) &
       inx, iny

  ! x座標について
  inx=in(1:1)
  select case (inx)
  case('a'); x=2
  case('b'); x=3
  case('c'); x=4
  case('d'); x=5
  case('e'); x=6
  case('f'); x=7
  case('g'); x=8
  case('h'); x=9
  case default
     correctIn=OFF
     return
  end select

  ! y座標について
  iny=in(2:2)
  read(iny, '(i1)', err=100) y
  y=y+1
  if(y < BOARD_BGN .and. y > BOARD_END) then
     correctIn=OFF
     return
  end if

  correctIn=ON
  return

100 correctIn=OFF
  return
end subroutine readIn
