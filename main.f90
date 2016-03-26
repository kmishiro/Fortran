program main ! メインプログラム

  use setParameters, only : &
       ON, OFF, DIS, &
       MAX_TURNS, &
       BLACK
  use userSet, only : &
       GAMES_NUMBER, &
       RANDOM_PROCESS, &
       INITIAL_PHASE
  use setVariables, only : &
       init, & ! サブルーチン
       Turns, &
       CurrentColor, &
       DiscsBlack, DiscsWhite, &
       Player1, Player2
!V  use setTheBookVarias, only : &
!V       initTheBook ! サブルーチン
  use setIndexVarias, only : &
       initIndex ! サブルーチン
  use setPatternEvalVarias, only : &
       initPatternEval_1, & ! サブルーチン
       initPatternEval_2    ! サブルーチン

  implicit none

  integer &
       correctIn, &
       gameRetire, &
       gameContinue, &
       switchPhase1, &
       switchPhase2, &
       x, y
  integer, dimension(8):: &
       values
  character(len=2) &
       in
  character(len=8) &
       date
  character(len=10) &
       time
  character(len=5) &
       zone
  integer &
       player1Win, &
       draw, &
       player2Win, &
       numOfGames
  character(len=6) &
       str_numOfGames

  ! ファイル名のために時間を取得
  call date_and_time(date, time, zone, values)
  open(01, file='out/'//date//time)
  write(06, '(a)') 'file name = '//date//time

  ! 初期設定を出力
  call initialSetting

  ! 初期化
  write(06, '(a)') 'initializing...'
  call initIndex ! インデックス表を配列に代入
  call initPatternEval_1 ! パターン評価値表を配列に代入(player1)
  call initPatternEval_2 ! パターン評価値表を配列に代入(player2)
  player1Win=0
  draw=0
  player2Win=0

  do numOfGames=1, GAMES_NUMBER ! 試合数ループ
     
     call init
     gameContinue=ON
     gameRetire  =OFF
     switchPhase1=INITIAL_PHASE
     switchPhase2=INITIAL_PHASE
     Player1=(-1)**numOfGames ! 先攻後攻を1ゲーム毎に入れ替える
     Player2=-Player1

     write(str_numOfGames, '(i6)') numOfGames
     call output('Game Number :'//str_numOfGames, ON)

     ! 対局の開始
     do ! 対局ループ：対局が終われば、ループ外に出る
        
        correctIn=OFF
        
        call pass(correctIn)
        
        do ! 手番ループ：正しい手が打たれれば、ループ外に出る
           
           if(Turns <= RANDOM_PROCESS) then ! ランダム進行手数
              
              call random(in)

           else

              if(Player1 == CurrentColor) then ! player1の番
              
                 call ai(switchPhase1, in, Player1)
                 
              else ! player2の番
              
                 call ai(switchPhase2, in, Player2)
              
              end if
              
           end if

           call readIn(x, y, correctIn, in)
           
           call move(x, y, correctIn)
           
           if(correctIn == OFF) then
              call output('incorrect input.', ON)
           else
              exit
           end if
              
        end do
        
        call isGameOver(gameRetire, gameContinue)
        
        if(gameContinue == OFF) exit
     end do

     ! 勝率の計算
     if(Player1 == BLACK) then
        if(DiscsBlack > DiscsWhite) then
           player1Win=player1Win+1
        else if(DiscsBlack < DiscsWhite) then
           player2Win=player2Win+1
        else
           draw=draw+1
        end if
     else
        if(DiscsWhite > DiscsBlack) then
           player1Win=player1Win+1
        else if(DiscsWhite < DiscsBlack) then
           player2Win=player2Win+1
        else
           draw=draw+1
        end if
     end if
     
     call writePutPlace

  end do

  call rateOfWin(player1Win, draw, player2Win) ! 勝率の出力
  
  close(01)

end program main
