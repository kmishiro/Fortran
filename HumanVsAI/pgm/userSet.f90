module userSet ! ユーザー設定
  
  use setParameters, only : &
       ON, OFF, &
       BLACK, WHITE
  
  implicit none
  
  ! 読みの深さに関する設定
  integer, parameter:: &
       NORMAL_DEPTH = 7, & ! 中盤の読みの深さ (default: 7)
       WDL_DEPTH    =16, & ! 最後から何手目を必勝読みにするか (default: 16)
       PERFECT_DEPTH=12, & ! 最後から何手目を完全読みにするか (default: 12)
       SORT_DEPTH   = 4    ! 中盤でのソート時の読みの深さ ( default: 5 < NORNAL_DEPTH)
  
  ! 評価関数ファイルの指定
  character(len=20), parameter:: &
       EVALUATION_FILE='correct003' ! 約10万棋譜から生成した評価関数表

  ! 確定石と着手可能手数の重みに関する設定
  real, parameter:: &
       WEIGHT_STABLESTONE= 0.300, & ! 確定石の重み係数 (default: 0.300)
       WEIGHT_MOVABLEDISC= 0.014    ! 着手可能手数の重み係数 (default: 0.014)
                                    ! 共に0.000にしておく方が無難かもしれない。
                                    ! デフォルトの値を入れることで、勝率が約7.5パーセントの向上する。
  ! 初期盤面、AIの石の色に関する設定
  integer, parameter:: &
       AUTO_IN=OFF, & ! 初期盤面の有無 (ON or OFF)
                      ! ONにするとfileIn.datに書かれた棋譜で自動進行する。
       AI_COLOR=WHITE ! AIの石の色 (BLACK（先攻） or WHITE（後攻）)
  
  ! その他の設定
  integer, parameter:: &
       USE_RANDOM=OFF ! 人間の手番をランダムAIに任せる (ON or OFF)

end module userSet
