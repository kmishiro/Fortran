module setParameters ! 定数の設定
  
  implicit none
  
  integer, parameter:: & ! スイッチの定義
       ON =1, & ! オン
       OFF=0, & ! オフ
       DIS=-1   ! 盤面出力を指定する

  integer, parameter:: & ! ボードサイズと手数の定義
       BOARD_SIZE=8, & ! ボードサイズ(8x8)
       ! ボードの始まり（ボードは周りに1マスづつ仮想的なマスを持っている(1, 9)） 
       BOARD_BGN=2, & 
       BOARD_END=BOARD_SIZE+1, & ! ボードの終わり
       ! 最大手数(61)、本来は60(=64-4)であるが、便宜的に+1をしている
       MAX_TURNS=BOARD_SIZE**2-4+1

  integer, parameter:: & ! マスの状態に関する定義
       EMPTY= 0, & ! 空き
       WHITE=-1, & ! 白石
       BLACK= 1, & ! 黒石
       WALL = 2    ! 壁（ボードの周りに配置される）

  integer, parameter:: & ! 手番によるAIフェーズに関する定義
       OPENING_MODE=1, & ! 定石モード
       MIDDLE_MODE =2, & ! 中盤モード
       WDL_MODE    =3, & ! 必勝読みモード、勝ち負けを判定
       PERFECT_MODE=4    ! 完全読みモード、最終盤面の自石が最大となる
  
  integer, parameter:: & ! 石が置けるかマスを探索するために使用
       NON        =0, & ! 無
       UPPER      =1, & ! 上
       UPPER_LEFT =2, & ! 左上
       LEFT       =3, & ! 左
       LOWER_LEFT =4, & ! 左下
       LOWER      =5, & ! 下
       LOWER_RIGHT=6, & ! 右下
       RIGHT      =7, & ! 右
       UPPER_RIGHT=8    ! 右上

  real, parameter:: & ! 最大値の定義
       INFINITY=1.e+4

end module setParameters

!----------------------------------------------------------------

module setTheBookParams ! 定石の設定

  implicit none

  integer,parameter:: &
!       THEBOOKWAY=3890062 ! 定石集から定石パターンを読み込む数
       THEBOOKWAY=454 ! 定石集から定石パターンを読み込む数

end module setTheBookParams

!----------------------------------------------------------------

module setIndexParams ! インデックスの設定

  use setParameters, only : &
       BOARD_SIZE

  implicit none

  integer,parameter:: &
       INDEX_NUM=3**BOARD_SIZE, & ! インデックス数(3^8=6561)
       STAGE_NUM=12               ! パターン評価のステージ数

end module setIndexParams
