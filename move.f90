subroutine move(x, y, correctIn)

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

  if(x < BOARD_BGN .or. x > BOARD_END) then
     correctIn=OFF
     return
  end if
  if(y < BOARD_BGN .or. y > BOARD_END) then
     correctIn=OFF
     return
  end if
  if(MovableDir(Turns, x, y) == NON) then
     correctIn=OFF
     return
  end if

  MemoryCurrentColor(Turns)=CurrentColor ! 現在の手番を保存
  MemoryRawBoard(Turns, BOARD_BGN:BOARD_END, BOARD_BGN:BOARD_END)=RawBoard(BOARD_BGN:BOARD_END, BOARD_BGN:BOARD_END) ! 現在のボード情報を保存

  call flipDiscs(x, y) ! 実際に石を返す

  MemoryPutPlace(Turns)=10*x+y ! 石を置いた場所を保存

  Turns=Turns+1

  CurrentColor=-CurrentColor

  call initMovable(CurrentColor)

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
       correctIn, &
       gameContinue
  integer &
       x, y

  call initMovable(CurrentColor)
  do y=BOARD_BGN, BOARD_END
     do x=BOARD_BGN, BOARD_END
        if(MovableDir(Turns, x, y) /= NON) return
     end do
  end do

  call isGameOver(0, gameContinue)

  correctIn=ON

  CurrentColor=-CurrentColor

  call initMovable(CurrentColor)

  return
end subroutine pass

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
       x, y, &
       xv, yv, &
       dir

  RawBoard(x, y)=CurrentColor

  dir=MovableDir(Turns, x, y)

  xv=x
  yv=y
  if(btest(dir, UPPER-1)) then ! 上方向
     do while(RawBoard(xv, yv-1) /= CurrentColor)
        RawBoard(xv, yv-1)=CurrentColor
        yv=yv-1
     end do
  end if

  xv=x
  yv=y
  if(btest(dir, LOWER-1)) then ! 下方向
     do while(RawBoard(xv, yv+1) /= CurrentColor)
        RawBoard(xv, yv+1)=CurrentColor
        yv=yv+1
     end do
  end if

  xv=x
  yv=y
  if(btest(dir, LEFT-1)) then ! 左方向
     do while(RawBoard(xv-1, yv) /= CurrentColor)
        RawBoard(xv-1, yv)=CurrentColor
        xv=xv-1
     end do
  end if

  xv=x
  yv=y
  if(btest(dir, RIGHT-1)) then ! 右方向
     do while(RawBoard(xv+1, yv) /= CurrentColor)
        RawBoard(xv+1, yv)=CurrentColor
        xv=xv+1
     end do
  end if

  xv=x
  yv=y
  if(btest(dir, UPPER_RIGHT-1)) then ! 右上方向
     do while(RawBoard(xv+1, yv-1) /= CurrentColor)
        RawBoard(xv+1, yv-1)=CurrentColor
        xv=xv+1
        yv=yv-1
     end do
  end if

  xv=x
  yv=y
  if(btest(dir, UPPER_LEFT-1)) then ! 左上方向
     do while(RawBoard(xv-1, yv-1) /= CurrentColor)
        RawBoard(xv-1, yv-1)=CurrentColor
        xv=xv-1
        yv=yv-1
     end do
  end if

  xv=x
  yv=y
  if(btest(dir, LOWER_LEFT-1)) then ! 左下方向
     do while(RawBoard(xv-1, yv+1) /= CurrentColor)
        RawBoard(xv-1, yv+1)=CurrentColor
        xv=xv-1
        yv=yv+1
     end do
  end if

  xv=x
  yv=y
  if(btest(dir, LOWER_RIGHT-1)) then ! 右下方向
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
       gameContinue
  integer &
       x, y

  if(Turns == MAX_TURNS .or. gameRetire == ON) then
     gameContinue=OFF
     return
  end if

  do y=BOARD_BGN, BOARD_END
     do x=BOARD_BGN, BOARD_END
        if(MovableDir(Turns, x, y) /= EMPTY) then

           gameContinue=ON

           return
        end if
     end do
  end do

  call initMovable(-CurrentColor)

  do y=BOARD_BGN, BOARD_END
     do x=BOARD_BGN, BOARD_END
        if(MovableDir(Turns, x, y) /= EMPTY) then
           call initMovable(CurrentColor)

           gameContinue=ON

           return
        end if
     end do
  end do

  gameContinue=OFF

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

  correctIn=ON
  gameRetire=ON

  return
end subroutine retireGame

!----------------------------------------------------------------

subroutine initMovable(turgetColor) ! checkMobilityを呼び出し、置ける場所を探す

  use setParameters, only : &
       BOARD_BGN, BOARD_END

  implicit none
  
  integer &
       turgetColor
  
  integer &
       x, y
  
  do y=BOARD_BGN, BOARD_END
     do x=BOARD_BGN, BOARD_END
        
        call checkMobility(turgetColor, x, y)

     end do
  end do

  return
end subroutine initMovable

!----------------------------------------------------------------

subroutine checkMobility(turgetColor, x, y)

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
  
  if(RawBoard(x, y) /= EMPTY) then
     MovableDir(Turns, x, y)=NON
     return
  end if
  
  dir=NON
  
  xv=x
  yv=y
  if(RawBoard(xv, yv-1) == -turgetColor) then ! 上方向
     xv=xv
     yv=yv-2
     do while(RawBoard(xv, yv) == -turgetColor)
        yv=yv-1
     end do
     if(RawBoard(xv, yv) == turgetColor) dir=ibset(dir, UPPER-1)
  end if

  xv=x
  yv=y
  if(RawBoard(xv, yv+1) == -turgetColor) then ! 下方向
     xv=xv
     yv=yv+2
     do while(RawBoard(xv, yv) == -turgetColor)
        yv=yv+1
     end do
     if(RawBoard(xv, yv) == turgetColor) dir=ibset(dir, LOWER-1)
  end if
  
  xv=x
  yv=y
  if(RawBoard(xv-1, yv) == -turgetColor) then ! 左方向
     xv=xv-2
     yv=yv
     do while(RawBoard(xv, yv) == -turgetColor)
        xv=xv-1
     end do
     if(RawBoard(xv, yv) == turgetColor) dir=ibset(dir, LEFT-1)
  end if
  
  xv=x
  yv=y
  if(RawBoard(xv+1, yv) == -turgetColor) then ! 右方向
     xv=xv+2
     yv=yv
     do while(RawBoard(xv, yv) == -turgetColor)
        xv=xv+1
     end do
     if(RawBoard(xv, yv) == turgetColor) dir=ibset(dir, RIGHT-1)
  end if

  xv=x
  yv=y
  if(RawBoard(xv+1, yv-1) == -turgetColor) then ! 右上方向
     xv=xv+2
     yv=yv-2
     do while(RawBoard(xv, yv) == -turgetColor)
        xv=xv+1
        yv=yv-1
     end do
     if(RawBoard(xv, yv) == turgetColor) dir=ibset(dir, UPPER_RIGHT-1)
  end if
  
  xv=x
  yv=y
  if(RawBoard(xv-1, yv-1) == -turgetColor) then ! 左上方向
     xv=xv-2
     yv=yv-2
     do while(RawBoard(xv, yv) == -turgetColor)
        xv=xv-1
        yv=yv-1
     end do
     if(RawBoard(xv, yv) == turgetColor) dir=ibset(dir, UPPER_LEFT-1)
  end if

  xv=x ; yv=y
  if(RawBoard(xv-1, yv+1) == -turgetColor) then ! 左下方向
     xv=xv-2
     yv=yv+2
     do while(RawBoard(xv, yv) == -turgetColor)
        xv=xv-1
        yv=yv+1
     end do
     if(RawBoard(xv, yv) == turgetColor) dir=ibset(dir, LOWER_LEFT-1)
  end if
  
  xv=x
  yv=y
  if(RawBoard(xv+1, yv+1) == -turgetColor) then ! 左下方向
     xv=xv+2
     yv=yv+2
     do while(RawBoard(xv, yv) == -turgetColor)
        xv=xv+1
        yv=yv+1
     end do
     if(RawBoard(xv, yv) == turgetColor) dir=ibset(dir, LOWER_RIGHT-1)
  end if

  MovableDir(Turns, x, y)=dir
  
  return
end subroutine checkMobility

!----------------------------------------------------------------

subroutine countDiscs ! ボード上の石(色)の数を数える

  use setParameters, only : &
       BOARD_BGN, BOARD_END, &
       BLACK, WHITE
  use setVariables, only : &
       DiscsBLACK, DiscsWHITE, DiscsEMPTY, &
       RawBoard

  implicit none

  integer &
       x, y

  DiscsBLACK=0
  DiscsWHITE=0
  DiscsEMPTY=0

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

subroutine getLocalIndex(x, y, &
     localHorizLine, localVertiLine, localBlackLine, localWhiteLine, &
     blackLineNumber, whiteLineNumber)

  use setParameters, only : &
       BOARD_BGN, BOARD_END, &
       BOARD_SIZE
  use setVariables, only : &
       RawBoard

  implicit none

  integer &
       x, y, &
       index, &
       blackLineNumber, whiteLineNumber
  integer &
       localHorizLine, &
       localVertiLine, &
       localBlackLine, &
       localWhiteLine

  ! horizontal line
  index= &
       3*(3*(3*(3*(3*(3*(3*RawBoard(BOARD_END, y) &
       +RawBoard(BOARD_END-1, y)) &
       +RawBoard(BOARD_END-2, y)) &
       +RawBoard(BOARD_END-3, y)) &
       +RawBoard(BOARD_BGN+3, y)) &
       +RawBoard(BOARD_BGN+2, y)) &
       +RawBoard(BOARD_BGN+1, y)) &
       +RawBoard(BOARD_BGN, y)
  localHorizLine=index+3281

  ! vertical line
  index= &
       3*(3*(3*(3*(3*(3*(3*RawBoard(x, BOARD_END) &
       +RawBoard(x, BOARD_END-1)) &
       +RawBoard(x, BOARD_END-2)) &
       +RawBoard(x, BOARD_END-3)) &
       +RawBoard(x, BOARD_BGN+3)) &
       +RawBoard(x, BOARD_BGN+2)) &
       +RawBoard(x, BOARD_BGN+1)) &
       +RawBoard(x, BOARD_BGN)
  localVertiLine=index+3281

  ! black line
  blackLineNumber=x+y-5
  select case (blackLineNumber)
  case(1)
     index= &
          3*(3*RawBoard(BOARD_BGN, BOARD_BGN+2) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+1)) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN)
     localBlackLine=index+3281
  case(2)
     index= &
          3*(3*(3*RawBoard(BOARD_BGN, BOARD_BGN+3) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+2)) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+1)) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN)
     localBlackLine=index+3281
  case(3)
     index= &
          3*(3*(3*(3*RawBoard(BOARD_BGN, BOARD_BGN+4) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+3)) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+2)) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+1)) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN)
     localBlackLine=index+3281
  case(4)
     index= &
          3*(3*(3*(3*(3*RawBoard(BOARD_BGN, BOARD_BGN+5) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+4)) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+3)) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+2)) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+1)) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN)
     localBlackLine=index+3281
  case(5)
     index= &
          3*(3*(3*(3*(3*(3*RawBoard(BOARD_BGN, BOARD_BGN+6) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+5)) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+4)) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+3)) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+2)) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+1)) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN)
     localBlackLine=index+3281
  case(6)
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
  case(7)
     index= &
          3*(3*(3*(3*(3*(3*RawBoard(BOARD_BGN+1, BOARD_END) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+6)) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+5)) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+4)) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+3)) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+2)) &
          +RawBoard(BOARD_END, BOARD_BGN+1)
     localBlackLine=index+3281
  case(8)
     index= &
          3*(3*(3*(3*(3*RawBoard(BOARD_BGN+2, BOARD_END) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+6)) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+5)) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+4)) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+3)) &
          +RawBoard(BOARD_END, BOARD_BGN+2)
     localBlackLine=index+3281
  case(9)
     index= &
          3*(3*(3*(3*RawBoard(BOARD_BGN+3, BOARD_END) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+6)) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+5)) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+4)) &
          +RawBoard(BOARD_END, BOARD_BGN+3)
     localBlackLine=index+3281
  case(10)
     index= &
          3*(3*(3*RawBoard(BOARD_BGN+4, BOARD_END) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+6)) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+5)) &
          +RawBoard(BOARD_END, BOARD_BGN+4)
     localBlackLine=index+3281
  case(11)
     index= &
          3*(3*RawBoard(BOARD_BGN+5, BOARD_END) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+6)) &
          +RawBoard(BOARD_END, BOARD_BGN+5)
     localBlackLine=index+3281
  case default
     localBlackLine=3281
  end select

  ! white line
  whiteLineNumber=6-x+y
  select case (whiteLineNumber)
  case(1)
     index= &
          3*(3*RawBoard(BOARD_BGN+5, BOARD_BGN) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+1)) &
          +RawBoard(BOARD_END, BOARD_BGN+2)
     localWhiteLine=index+3281
  case(2)
     index= &
          3*(3*(3*RawBoard(BOARD_BGN+4, BOARD_BGN) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+1)) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+2)) &
          +RawBoard(BOARD_END, BOARD_BGN+3)
     localWhiteLine=index+3281
  case(3)
     index= &
          3*(3*(3*(3*RawBoard(BOARD_BGN+3, BOARD_BGN) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+1)) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+2)) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+3)) &
          +RawBoard(BOARD_END, BOARD_BGN+4)
     localWhiteLine=index+3281
  case(4)
     index= &
          3*(3*(3*(3*(3*RawBoard(BOARD_BGN+2, BOARD_BGN) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+1)) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+2)) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+3)) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+4)) &
          +RawBoard(BOARD_END, BOARD_BGN+5)
     localWhiteLine=index+3281
  case(5)
     index= &
          3*(3*(3*(3*(3*(3*RawBoard(BOARD_BGN+1, BOARD_BGN) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+1)) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+2)) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+3)) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+4)) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+5)) &
          +RawBoard(BOARD_END, BOARD_BGN+6)
     localWhiteLine=index+3281
  case(6)
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
  case(7)
     index= &
          3*(3*(3*(3*(3*(3*RawBoard(BOARD_BGN, BOARD_BGN+1) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+2)) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+3)) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+4)) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+5)) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+6)) &
          +RawBoard(BOARD_BGN+6, BOARD_END)
     localWhiteLine=index+3281
  case(8)
     index= &
          3*(3*(3*(3*(3*RawBoard(BOARD_BGN, BOARD_BGN+2) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+3)) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+4)) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+5)) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+6)) &
          +RawBoard(BOARD_BGN+5, BOARD_END)
     localWhiteLine=index+3281
  case(9)
     index= &
          3*(3*(3*(3*RawBoard(BOARD_BGN, BOARD_BGN+3) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+4)) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+5)) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+6)) &
          +RawBoard(BOARD_BGN+4, BOARD_END)
     localWhiteLine=index+3281
  case(10)
     index= &
          3*(3*(3*RawBoard(BOARD_BGN, BOARD_BGN+4) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+5)) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+6)) &
          +RawBoard(BOARD_BGN+3, BOARD_END)
     localWhiteLine=index+3281
  case(11)
     index= &
          3*(3*RawBoard(BOARD_BGN, BOARD_BGN+5) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+6)) &
          +RawBoard(BOARD_BGN+2, BOARD_END)
     localWhiteLine=index+3281
  case default
     localWhiteLine=3281
  end select

  return
end subroutine getLocalIndex

!----------------------------------------------------------------

subroutine random(in)

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

  integer, allocatable, dimension(:) :: seed
  integer nrand
  integer clock
  real, dimension(2):: randomNum

  call random_seed(size=nrand)
  allocate(seed(nrand))
  call system_clock(count=clock)

  seed=clock
  call random_seed(put=seed)

  do
     call random_number(randomNum)
     x=int(randomNum(1)*10)
     y=int(randomNum(2)*10)
     if(x.ge.2.and.x.le.9.and.y.ge.2.and.y.le.9) then
        if(MovableDir(Turns, x, y).ne.NON) exit
     end if
  end do

  call writeIn(x, y, in)

  return
end subroutine random

!----------------------------------------------------------------

subroutine writeIn(x, y, in) ! 入力を数値に変換(⇔readIn)

  use setParameters, only : &
       BOARD_BGN, BOARD_END

  implicit none

  integer, intent(in) :: &
       x, y
  character(len=2), intent(out) :: &
       in
  character(len=1) &
       inx, iny

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

  if(y < BOARD_BGN .and. y > BOARD_END) then
     write(06, '(a)') 'program bug:: s:rewriteIn(Y)'
     stop
  end if
  write(iny, '(i1)') y-1

  in=inx//iny

  return
end subroutine writeIn

!----------------------------------------------------------------

subroutine readIn(x, y, correctIn, in) ! 入力を数値に変換(⇔writeIn)

  use setParameters, only : &
       OFF, &
       BOARD_BGN, BOARD_END
  use setVariables

  implicit none

  integer, intent(out) :: &
       x, y
  integer, intent(out) :: &
       correctIn
  character(len=2), intent(in) :: &
       in
  character(len=1) &
       inx, iny

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
