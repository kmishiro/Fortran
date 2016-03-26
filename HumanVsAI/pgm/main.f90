program main ! メインプログラム

  use setParameters, only : &
       ON, OFF, DIS, &
       MAX_TURNS, &
       OPENING_MODE, &
       MIDDLE_MODE, &
       WDL_MODE, &
       PERFECT_MODE
  use userSet, only : &
       AUTO_IN, &
       AI_COLOR, &
       NORMAL_DEPTH, &
       WDL_DEPTH, &
       PERFECT_DEPTH, &
       SORT_DEPTH, &
       USE_RANDOM
  use setVariables, only : &
       init, & ! サブルーチン
       Turns, &
       CurrentColor
  use setTheBookVarias, only : &
       initTheBook ! サブルーチン
  use setIndexVarias, only : &
       initIndex ! サブルーチン
  use setPatternEvalVarias, only : &
       initPatternEval ! サブルーチン

  implicit none

  integer &
       correctIn, &    ! 打った手が正しいか（打てるか）を判別 (ON or OFF)
       gameRetire, &   ! ゲーム中止かどうかを判別 (ON or OFF)
       gameContinue, & ! ゲーム終了かどうかを判別 (ON or OFF)
       switchPhase, &  ! AI用にゲームフェーズを判別
                       ! (OPENING_MODE or MIDDLE_MODE or WDL_MODE or PERFECT_MODE)
       x, y            ! 盤面の座標軸
  integer*8 &
       time0, time1, dtime ! AI思考時間の取得用
  real*8 &
       computingTime, &   ! AI思考時間（1手）
       totalComputingTime ! AI思考時間（ゲームトータル）
  character(len=8) &
       str_eval ! 評価値のアウトプット用の文字列
  character(len=2) &
       in ! 打つ手の入力用（例：f5）
  character(len=7) &
       phase ! ゲームフェーズのアウトプット用の文字列
  character(len=120) &
       fileIn ! 初期盤面設定用の棋譜ファイルから読み込む文字列
  character(len=8) &
       date ! ファイル名のための年月日
  character(len=10) &
       time ! ファイル名のための時分秒.コンマ秒
  character(len=6) &
       str_computingTime ! AI思考時間のアウトプット用の文字列（1手）
  character(len=8) &
       str_totalcomputingTime ! AI思考時間のアウトプット用の文字列（ゲームトータル）

  ! ファイル名のために時間を取得
  call date_and_time(date, time)
  open(01, file='out/boardHistory.dat') ! ゲーム記録（ビジュアル的）
  open(02, file='out/'//date//time)     ! ゲーム記録（シンプル棋譜）
  call output('file name = '//date//time, ON) ! ファイル名の表示

  ! 初期化
  call output('initializing...', ON)
  call init            ! ゲーム全般に関する初期化
  call initTheBook     ! 定石に関する初期化
  call initIndex       ! インデックスに関する初期化
  call initPatternEval ! パターン評価値に関する初期化
  gameContinue=ON           ! ゲーム続行をON
  gameRetire  =OFF          ! ゲーム中止をOFF
  switchPhase =OPENING_MODE ! ゲームフェーズをOPENING_MODE
  totalComputingTime=0.     ! AI思考ゲームトータル時間の初期化
  if(AUTO_IN == ON) then    ! 初期盤面の入力用のfileInに棋譜を格納
     open(10, file='dat/fileIn.dat')
     read(10, '(a)') fileIn
     close(10)
  end if

  ! 対局の開始
  call output('------- begin -------', ON)

  do ! 対局ループ：対局が終われば、ループ外に出る

     correctIn=OFF ! 打った手の正誤を誤にする

     call pass(correctIn) ! パス判定、パスの場合はターンをチェンジ（黒⇔白）

     call output(' ', DIS) ! 盤面出力

     do ! 手番ループ：正しい手が打たれれば、ループ外に出る

        call output('coord:', OFF) ! 打った手を出力

        if(Turns <= len_trim(fileIn)/2 .and. AUTO_IN == ON) then ! 初期盤面入力がONの場合

           in=fileIn(2*Turns-1:2*Turns) ! fileInから一手づつ抽出する

        else

           if(AI_COLOR == CurrentColor) then ! AIの番

              call system_clock(time0) ! 計算時間測定（開始）
              call output('computing...', ON)

              call ai(switchPhase, in, str_eval, phase) ! AIサブルーチン、ここでコンピュータの手（in）を決定する
              call output(in, OFF) ! 手（in）の出力
              call output(' ('//trim(phase)//','//str_eval//')', ON) ! ゲームフェーズと評価値の出力
              call system_clock(time1, dtime) ! 計算時間測定（終了）
              computingTime=1d0*(time1-time0)/dtime ! 計算時間（1手）
              totalComputingTime=totalComputingTime+computingTime! 計算時間（1手）を積算してトータル時間にしている
              write(str_computingTime, '(f6.3)') computingTime 
              call output('compute time ='//str_computingTime//'s', ON) ! 計算時間の出力

           else ! 人間の番

              if(USE_RANDOM == ON) then ! USE_RANDOMをONにしていたら、自分の手をランダム関数に任せる
                 call random(in)
              else
                 read(05, '(a)') in ! 自分の手をターミナルから入力
              end if

              call output(in, ON) ! 手（in）の出力

           end if

        end if

        if(in == 'u') then ! 'u'入力を受けて、手を戻す
           call unundo(correctIn)
           x=0
           y=0
        else if(in == 'r') then ! 'r'入力を受けて、ゲームを中止する
           call retireGame(correctIn, gameRetire)
           x=0
           y=0
        else
           call readIn(x, y, correctIn, in) ! inを座標に変換する
           call move(x, y, correctIn) ! 実際に盤面に石を置いて、石を返す
           if(correctIn == OFF) call output('incorrect input.', ON) ! 手が正しくない（石が置けない）ときに警告
        end if

        if(correctIn == ON) exit ! 正しい手が打たれれば、ループを抜けて次のターンに移る
     end do

     call output(' ', ON) ! 改行

     call isGameOver(gameRetire, gameContinue) ! ゲーム終了もしく中止を判断

     if(gameContinue == OFF) exit ! ゲーム終了・中止（gameContinue=OFF）のときループを抜けゲームを終わる
  end do

  call output(' ', DIS) ! 最後の盤面を出力

  write(str_totalComputingTime, '(f8.3)') totalComputingTime
  call output('total compute time ='//str_totalcomputingTime//'s', ON) ! AIの思考時間（ゲームトータル）を出力
  call output('-------- end --------', ON)

  call writePutPlace ! file.02にゲーム記録、シンプル棋譜

  close(01)
  close(02)

end program main
