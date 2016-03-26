module setVariables ! �O���[�o���ϐ��̐ݒ�

  use setParameters
  
  implicit none
  
  integer &
       Turns, &     ! �^�[���i��ԁj��
       CurrentColor ! ���ݎ�Ԃ̐F�B��ɁABLACK��WHITE������B
  
  integer,dimension(MAX_TURNS, BOARD_SIZE+2, BOARD_SIZE+2):: &
       MovableDir, &  ! �e��Ԃɂ�����Ֆʂ́u�u����ʒu�v���L������z��
       MemoryRawBoard ! �e��Ԃɂ�����Ֆʂ��L������z��

  integer,dimension(MAX_TURNS):: &
       MemoryPutPlace, &  ! �u�����΂̈ʒu���L������z��
       MemoryCurrentColor ! ���̃^�[�������Ɣ��A�ǂ����̎�Ԃ����������L������z��

  integer,dimension(BOARD_SIZE+2,BOARD_SIZE+2):: &
       RawBoard ! ���݂̔Ֆʂ��i�[�����

  integer &
       DiscsBLACK, & ! ���΂̐�
       DiscsWHITE, & ! ���΂̐�
       DiscsEMPTY    ! �󂫃}�X�̐�
  
contains

  subroutine init ! �O���[�o���ϐ��̏������T�u���[�`��

    use setParameters, only : &
         BOARD_SIZE, &
         BOARD_BGN, BOARD_END, &
         MAX_TURNS, &
         EMPTY, WHITE, BLACK, WALL

    implicit none

    integer &
         x, y

    ! �{�[�h�̎���ɕǂ����
    do x=1, BOARD_SIZE+2
       RawBoard(x, 1)=WALL
       RawBoard(x, BOARD_SIZE+2)=WALL
    end do
    do y=1, BOARD_SIZE+2
       RawBoard(1, y)=WALL
       RawBoard(BOARD_SIZE+2, y)=WALL
    end do

    ! �Ǔ����󂫃}�X�ɂ���
    do x=BOARD_BGN, BOARD_END
       do y=BOARD_BGN, BOARD_END
          RawBoard(x, y)=EMPTY
       end do
    end do

    ! �΂̏����z�u��ݒ�
    RawBoard(5, 5)=WHITE
    RawBoard(6, 6)=WHITE
    RawBoard(5, 6)=BLACK
    RawBoard(6, 5)=BLACK

    call countDiscs ! �e�F�̐΂̐��𐔂���

    ! ���̑��̕ϐ��̏�����
    Turns=1 ! �^�[����1�ɂ���
    CurrentColor=BLACK ! �I�Z���̃��[���ɑ���A����͍�
    MemoryRawBoard(1:MAX_TURNS, 1:BOARD_SIZE+2, 1:BOARD_SIZE+2)=0 ! �Ֆʏ�����
    MemoryRawBoard(Turns, 1:BOARD_SIZE+2, 1:BOARD_SIZE+2)=RawBoard(1:BOARD_SIZE+2, 1:BOARD_SIZE+2)

    call initMovable(CurrentColor) ! �΂�u����ꏊ�����邩���`�F�b�N���āAMovableDir���X�V����

    return
  end subroutine init

end module setVariables

!----------------------------------------------------------------

module setTheBookVarias ! ��Εϐ��̐ݒ�

  use setParameters, only : &
       MAX_TURNS
  use setTheBookParams, only : &
       THEBOOKWAY

  implicit none

  integer, dimension(THEBOOKWAY, MAX_TURNS-1):: &
       WayBook ! ��΂̊������e�^�[�����Ɋi�[�����
  real, dimension(THEBOOKWAY):: &
       EvalBook ! ��΂̕]���֐����i�[

contains

  subroutine initTheBook ! ��΂̔z����
  
    implicit none

    integer &
         n

    open(03, file='dat/theBookBasedWzebra.dat') ! ��΂̊������ۑ�����Ă���

    do n=1, THEBOOKWAY
       read(03, '(60i3, f9.3)') WayBook(n, 1:MAX_TURNS-1), EvalBook(n) ! �����ƕ]���l��z��ɕۑ�
    end do

    close(03)

    return
  end subroutine initTheBook

end module setTheBookVarias

!----------------------------------------------------------------

module setIndexVarias ! �C���f�b�N�X�ϐ��̐ݒ�

  use setParameters, only : &
       BOARD_SIZE
  use setIndexParams, only : &
       INDEX_NUM

  implicit none

  integer, dimension(INDEX_NUM):: &
       Indx, &            ! �C���f�b�N�X
       DiffStableStone, & ! �m��΂̍��Ɣ��̍��i�� - ���j
       DiffMovableDisc    ! ����\�萔�̍��Ɣ��̍��i�� - ���j
  integer, dimension(INDEX_NUM, 2, BOARD_SIZE):: &
       Movable, &          ! ����\���ǂ����̏������Ɣ��A���ꂼ��ɂ��Ċi�[
       FlipLeft, FlipRight ! ���莞�ɉE�ƍ��ɉ��΂�Ԃ��邩�̏��
  integer, dimension(INDEX_NUM, 2):: &
       StableStone, & ! �m��΂̐����i�[
       MovableDisc    ! ����\�萔���i�[

contains
  
  subroutine initIndex ! �C���f�b�N�X�y�т��̑��̏���z��Ɋi�[

    integer &
         n, x

    open(04, file='dat/indexTable.dat') ! �C���f�b�N�X�t�@�C��

    read(04, *)
    read(04, *)
    read(04, *)
    do n=1, INDEX_NUM ! �z��ԍ�=index+3281
       read(04, '(i4, 2(1x, 8i1, 8(2x, i1, 1x, i1, 1x)), 2(2i2, i3))') & ! �C���f�b�N�X�̓ǂݍ���
            Indx(n), & ! �C���f�b�N�X
            (Movable(n, 1, x), x=1, BOARD_SIZE), &   ! ���΂ɂ��� BEGIN -->
            FlipLeft(n, 1, 1), FlipRight(n, 1, 1), & ! ---
            FlipLeft(n, 1, 2), FlipRight(n, 1, 2), & ! ---
            FlipLeft(n, 1, 3), FlipRight(n, 1, 3), & ! ---
            FlipLeft(n, 1, 4), FlipRight(n, 1, 4), & ! ---
            FlipLeft(n, 1, 5), FlipRight(n, 1, 5), & ! ---
            FlipLeft(n, 1, 6), FlipRight(n, 1, 6), & ! ---
            FlipLeft(n, 1, 7), FlipRight(n, 1, 7), & ! ---
            FlipLeft(n, 1, 8), FlipRight(n, 1, 8), & ! <-- END
            (Movable(n, 2, x), x=1, BOARD_SIZE), &   ! ���΂ɂ��� BEGIN -->
            FlipLeft(n, 2, 1), FlipRight(n, 2, 1), & ! ---
            FlipLeft(n, 2, 2), FlipRight(n, 2, 2), & ! ---
            FlipLeft(n, 2, 3), FlipRight(n, 2, 3), & ! ---
            FlipLeft(n, 2, 4), FlipRight(n, 2, 4), & ! ---
            FlipLeft(n, 2, 5), FlipRight(n, 2, 5), & ! ---
            FlipLeft(n, 2, 6), FlipRight(n, 2, 6), & ! ---
            FlipLeft(n, 2, 7), FlipRight(n, 2, 7), & ! ---
            FlipLeft(n, 2, 8), FlipRight(n, 2, 8), & ! <-- END
            StableStone(n, 1), StableStone(n, 2), DiffStableStone(n), & ! �m��΁i���A���A���j
            MovableDisc(n, 1), MovableDisc(n, 2), DiffMovableDisc(n)    ! ����\�萔�i���A���A���j
    end do

    close(04)
  end subroutine initIndex

end module setIndexVarias

!----------------------------------------------------------------

module setPatternEvalVarias ! �e�p�^�[���̕]���l��ݒ�

  use setParameters, only : &
       BOARD_SIZE
  use setIndexParams, only : &
       INDEX_NUM, &
       STAGE_NUM
  use userSet, only : &
       EVALUATION_FILE

  implicit none

  integer, dimension(INDEX_NUM):: &
       Indx ! �C���f�b�N�X
  real, dimension(INDEX_NUM, STAGE_NUM):: & ! �e�p�^�[���A�e�t�F�[�Y���ɕ]���l���i�[����z�������
       Horiz1Eval, Horiz2Eval, Horiz3Eval, Horiz4Eval, & ! �����i�����j����
       Diago1Eval, Diago2Eval, Diago3Eval, Diago4Eval, Diago5Eval, & ! �΂ߕ���
       CornerEval ! ���t��
  real, dimension(INDEX_NUM, STAGE_NUM, 4):: &
       HorizEval ! �����i�����j�����̂܂Ƃ�
  real, dimension(INDEX_NUM, STAGE_NUM, 5):: &
       DiagoEval ! �΂ߕ����̂܂Ƃ�
  real, dimension(INDEX_NUM, STAGE_NUM):: &
       CorneEval ! ���t�߂̂܂Ƃ�

contains
  
  subroutine initPatternEval ! �p�^�[���̕]���l��z��Ɋi�[

    integer &
         n, i

    ! �e�p�^�[���̕]���l���L�^����Ă���t�@�C�����J��
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

    ! �z��ɑ��
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
    do n=1, INDEX_NUM ! �z��ԍ�=index+3281
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

    ! �����i�����j�����E�΂ߕ����E���t�߂�3�ɂ܂Ƃ߂�
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
